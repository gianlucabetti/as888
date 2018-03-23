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
009700131125     d $Msg            s             78    dim(31) ctdata perrcd(1)
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
012800131122
012900110407      // - Sk x importanza clienti richiesti nella selezione
013000110322     d $cic            s              1    dim(5)
013100110407
013200110407      // - Sk x categoria potenziali richiesti nella selezione
013300110407     d $fls            s              1    dim(5)
013400110308
013500080206
013600080206      //---------------------------------------------------------------
013700080206      //?Definizione aree dati.
013800080206      //---------------------------------------------------------------
013900080206
014000080206      // - Dati utente
014100080206     d §AzUte        e ds                  extname(AZUTE00F)
014200080206     d                                     dtaara
014300080206     d §DatiUte      e ds                  extname(dDatiUte)
014400080206     d                                     dtaara
014500080206
014600080206      //---------------------------------------------------------------
014700080206      //?Definizione strutture dati.
014800080206      //---------------------------------------------------------------
014900080206
015000080206      // - Status
015100080206     d Psds           sds
015200080206     d   SDSpgm          *proc
015300080206
015400080206      // - InfDS
015500080206     d InfDspF         ds
015600080207     d  dsp_aid              369    369a                                        AID byte
015700080206
015800080206      // - Indicatori su DspF
015900080208     d IndDspF         ds
016000080206        // - Abilitazione tasti funzionali
016100080206     d  F10Attivo                     1n   overlay(IndDspF : 10)
016200131125        // - Reverse immage  per comando altre parzializzazioni
016300131125     d  altreparz                     1n   overlay(IndDspF : 11)
016400080206        // - Condizionamento visualizzazioni
016500080206     d  GESclipot                     1n   overlay(IndDspF : 25)
016600081020     d  pgmrichia                     1n   overlay(IndDspF : 26)
016700080206        // - Indicatori di gestione del subfile
016800080206     d  SflDsp_N                      1n   overlay(IndDspF : 30)
016900080208     d  SflDspCtl_N                   1n   overlay(IndDspF : 31)
017000080206     d  SflNxtChg                     1n   overlay(IndDspF : 32)
017100080206     d  SflEnd                        1n   overlay(IndDspF : 33)
017200080206        // - Indicatori di errore
017300080206     d  errMessage                    1n   overlay(IndDspF : 28)
017400080206     d  PosCurOpz                     1n   overlay(IndDspF : 50)
017500080206     d  PosCurCP1                     1n   overlay(IndDspF : 51)
017600080215     d  PosCurOSF                     1n   overlay(IndDspF : 52)
017700110308     d  PosCurCic1                    1n   overlay(IndDspF : 53)
017800110407     d  PosCurFls1                    1n   overlay(IndDspF : 54)
017900110308     d  PosCurCic2                    1n   overlay(IndDspF : 55)
018000080215     d  PosCurPRV                     1n   overlay(IndDspF : 56)
018100080215     d  PosCurCAP                     1n   overlay(IndDspF : 57)
018200110308     d  PosCurCic3                    1n   overlay(IndDspF : 58)
018300110308     d  PosCurCic4                    1n   overlay(IndDspF : 59)
018400110308     d  PosCurCic5                    1n   overlay(IndDspF : 60)
018500110308     d  PosCurFlt                     1n   overlay(IndDspF : 61)
018600110308     d  PosCurSCT1                    1n   overlay(IndDspF : 62)
018700080215     d  PosCurCAR                     1n   overlay(IndDspF : 63)
018800080215     d  PosCurPIV                     1n   overlay(IndDspF : 64)
018900110308     d  PosCurSct2                    1n   overlay(IndDspF : 65)
019000081017     d  PosCurCUN                     1n   overlay(IndDspF : 66)
019100110310     d  PosCurSct3                    1n   overlay(IndDspF : 67)
019200110310     d  PosCurTreda                   1n   overlay(IndDspF : 68)
019300110310     d  PosCurTrea                    1n   overlay(IndDspF : 69)
019400110310     d  PosCurPkgda                   1n   overlay(IndDspF : 70)
019500110310     d  PosCurPkga                    1n   overlay(IndDspF : 71)
019600110310     d  PosCurConc1                   1n   overlay(IndDspF : 72)
019700110310     d  PosCurConc2                   1n   overlay(IndDspF : 73)
019800110310     d  PosCurConc3                   1n   overlay(IndDspF : 74)
019900110407     d  PosCurFls2                    1n   overlay(IndDspF : 75)
020000110407     d  PosCurFls3                    1n   overlay(IndDspF : 76)
020100110407     d  PosCurFls4                    1n   overlay(IndDspF : 77)
020200110407     d  PosCurFls5                    1n   overlay(IndDspF : 78)
020300131122     d  PosCurInt                     1n   overlay(IndDspF : 79)
020400131122     d  PosCurDil1                    1n   overlay(IndDspF : 80)
020500131122     d  PosCurDil2                    1n   overlay(IndDspF : 81)
020600131122     d  PosCurlos                     1n   overlay(IndDspF : 82)
020700131122     d  PosCurConcl1                  1n   overlay(IndDspF : 83)
020800131122     d  PosCurConcl2                  1n   overlay(IndDspF : 84)
020900131122     d  PosCurConcl3                  1n   overlay(IndDspF : 85)
021000080206     d  errGenerico                   1n   overlay(IndDspF : 99)
021100080206
021200080206     d WindDspF        ds                  inz
021300080207     d   WindDspFa             1     49    inz(*zero)
021400080207     d   WindDspFb            50     99    inz(*zero)
021500080206
021600131122     d wlbdat          ds                  inz
021700131122     d  g02dat                 1      8  0
021800131122     d  g02inv                 9     16  0
021900131122     d  g02err                17     17
022000131122     d  g02tgi                18     22  0
022100131122
022200080206      // "Unificazione" codice cliente potenziale a video
022300110308     d V01cpo          ds                  inz
022400110308     d   V01cp1                            inz
022500110308     d   V01cp2                            inz
022600110308
022700110308      // Campo unico per categoria merceologica
022800110308     d V01sct          ds                  inz
022900110308     d   V01sct1                           inz
023000110308     d   V01sct2                           inz
023100110308     d   V01sct3                           inz
023200110308
023300110308      // Campo unico per Importanza clienti
023400110308     d V01cic          ds                  inz
023500110308     d   V01cic1                           inz
023600110308     d   V01cic2                           inz
023700110308     d   V01cic3                           inz
023800110308     d   V01cic4                           inz
023900110308     d   V01cic5                           inz
024000110308
024100110407      // Campo unico per Categoria potenziale
024200110407     d V01fls          ds                  inz
024300110407     d   V01fls1                           inz
024400110407     d   V01fls2                           inz
024500110407     d   V01fls3                           inz
024600110407     d   V01fls4                           inz
024700110407     d   V01fls5                           inz
024800110407
024900110308      // Campo unico per Concorrenti
025000110308     d V01conc         ds                  inz
025100110308     d   V01conc1                          inz
025200110308     d   V01conc2                          inz
025300110308     d   V01conc3                          inz
025400131122
025500131122      // Campo unico per Concorrenti Logistica
025600131125     d V02conl         ds                  inz
025700131125     d   V02concl1                         inz
025800131125     d   V02concl2                         inz
025900131125     d   V02concl3                         inz
026000080206
026100080206      // - Parametri ricevuti
026200080206     d KPJBA         e ds
026300091026     d TRMK01ds      e ds                  inz
026400080206
026500080206      // - Reperimento dati utente
026600080206     d TIBS34ds      e ds
026700081020     d TRUL31DS      e ds
026800081020     d POG                    10    759    DIM(250)
026900080206
027000080206      // - Ricerca/Controllo tabelle
027100080206     d TIBS02ds      e ds                  inz
027200120914
027300120914      // - File attività
027400120914     dTIATCds        e ds                  extname(TIATC00F)
027500110309
027600110309      // - File potenziali
027700110309     d TNCPOds       e ds                  extname(TNCPO00F)
027800110309
027900110309      // - File trattative
028000110309     d TIVIS00F      e ds                  extname(TIVIS00F)
028100110309
028200090701      // - controllo abilitazioni
028300090701     d TNTAA1DS      e ds                  inz
028400110509
028500110509      // - Ricerca tabella CPO
028600110509     d TNTB87ds      e ds                  inz
028700080206
028800080206      // - Tabella 1L = Categorie Merceologiche
028900080207     d ds1L          e ds                  inz
029000110308
029100080208      // - Tabella 05 = Codici Area
029200080208     d ds05          e ds                  inz
029300081020     d
029400110407
029500110407      // - Tabella CPO = Categoria potenziale
029600110407     d dCPO          e ds                  inz
029700110407
029800110308      // - Dati in CPORST
029900081020     d dcpo01        e ds                  inz
030000080206
030100080206      // - Controllo congruenza CAP e provincia
030200080206     d FNLV13ds      e ds
030300080206     d   I13af0      e                     inz('S')
030400080206     d   I13cng      e                     inz('S')
030500080206     d TISI95ds      e ds                  inz
030600080206     d   I95tcn      e                     inz('3')
030700080206
030800081020     d TRMK50ds      e ds                  inz
030900100408
031000100408      // - apertura attività
031100100408     d Trmk19ds      e ds
031200080206
031300080206      //---------------------------------------------------------------
031400080206      //?Definizione variabili globali.
031500080206      //---------------------------------------------------------------
031600080206
031700080206      // - Flags booleani
031800110310     d $Conc           s               n   inz(*off)
031900131122     d $Conclog        s               n   inz(*off)
032000110309     d $EndAtt         s               n   inz(*off)
032100110309     d $EndTtr         s               n   inz(*off)
032200080208     d $Fine           s               n   inz(*off)
032300080208     d $InzD01         s               n   inz(*on)
032400080208     d $InzS01         s               n   inz(*on)
032500131122     d $InzD02         s               n   inz(*on)
032600080206     d $ErrGrave       s               n   inz(*off)
032700080207     d $EoF            s               n   inz(*off)
032800080208     d $RecOK          s               n   inz(*off)
032900080206
033000080206      // - Indici di schiera
033100080208     d xx              s              3  0 inz
033200081017     d yy              s              3  0 inz
033300080206
033400080207      // - Campi associati al video
033500080207     d $Video          s              2    inz('D1')
033600080208     d S01nrr          s              4  0 inz
033700080207     d W_SflPag        s              3  0 inz
033800080207     d wPag            s              4  0 inz
033900080207     d wRig            s              3  0 inz
034000110309
034100110309       // - Stringa SQL da eseguire
034200110309     d wSQL            s           2048    Varying        inz
034300080206
034400080206      // - Campi di comodo
034500080207     d w030a           s             30    inz
034600131122     d wdil1           s              8  0 inz
034700131122     d wdil2           s              8  0 inz
034800080207     d $Ord            s              3  0 inz
034900080626     d savkpjbu        s                   like(kpjbu)
035000131122     d w_dalogistica   s              1
035100100202     d w_damenu        s              1
035200100408     d w_daatti        s              1
035300100423     d DataSYS         s               d   inz(*sys)
035400100423     d Dataiso         s               d   datfmt(*iso)
035500131125     d data_eur        s               d   Datfmt(*eur)
035600100423     d Alfaoggi        s              8
035700131125     d $logistica      s              3
035800100423     d W0080           s              8  0
035900110308     d C_tab           s                   like(TBEcod)
036000110309     d C_ke1           s                   like(TBEke1)
036100110310     d wval            s                   like(CPIval)
036200110310     d wpft            s                   like(CPIpft)
036300110308
036400110308     d idTabella       s              2
036500110308     d Ordinamento     s              1
036600110308     d idElemento      s              8
036700110308     d TastoUscita     s              1
036800110601
036900110601     d wLib            s             21    inz
037000110601     d wTtr            s              4  0 inz
037100080206
037200080206      //---------------------------------------------------------------
037300080206      //?Definizione procedure usate.
037400080206      //---------------------------------------------------------------
037500080208
037600080208      // - Controllo cap/località
037700080208     d fnlv13r         pr                  extpgm('FNLV13R')
037800080208     d  kpjba                              likeds(kpjba)    const
037900080208     d  fnlv13ds                           likeds(FNLV13ds)
038000080208     d  tisi95ds                           likeds(TISI95ds)
038100080206
038200110509      // - Gestione/Interrogazione potenziale
038300100408     d TRMK02R         pr                  extpgm('TRMK02R')
038400100408     d  kpjba                              likeds(kpjba)
038500100408     d  trmk01ds                           likeds(trmk01ds)
038600100408     d  trmk19ds                           likeds(trmk19ds) options(*nopass)
038700110509
038800110509
038900110509      // - interrogazione tabella CPO
039000110509     d TNTB87R         pr                  extpgm('TNTB87R')
039100110509     d  tntb87ds                           likeds(TNTB87ds)
039200100408
039300080626      //---------------------------------------------------------------
039400080626      //?prototipi
039500080626      //---------------------------------------------------------------
039600080626      /copy gaitrasrc/srcprotopr,tibs02r
039700080626      /copy gaitrasrc/srcprotopr,tibs34r
039800080626      /copy gaitrasrc/srcprotopr,trmk07r
039900081020      /copy gaitrasrc/srcprotopr,trmk50r
040000081022      /copy gaitrasrc/srcprotopr,trmk51r
040100110308      /copy gaitrasrc/srcprotopr,trul19r
040200081020      /copy gaitrasrc/srcprotopr,trul31r
040300080626      /copy gaitrasrc/srcprotopr,x§taber
040400081017      /copy gaitrasrc/srcprotopr,xalfa3br
040500131125      /copy gaitrasrc/srcprotopr,xsrda8
040600090703      /copy gaitrasrc/srcprotopr,TNTAA1C
040700080206      //---------------------------------------------------------------
040800080206      //?Definizione key-list.
040900080206      //---------------------------------------------------------------
041000080206
041100080206      //---------------------------------------------------------------
041200080206      //?M A I N - L I N E
041300080206      //---------------------------------------------------------------
041400080206
041500080206     c     *Entry        plist
041600080206     c                   parm                    KPJBA
041700100408     c                   parm                    trmk19ds
041800131125
041900131125      * Verifico se sono stato richiamato da Menu e da una LOGISTICA
042000131125      * i primi 3 caratteri della KPJBU sono uguali a LOG
042100131125     c                   Movel     KPJBU         $LOGISTICA
042200131125     c                   If        $logistica = 'LOG'
042300131125     c                   Clear                   KPJBU
042400131125     c                   Endif
042500080206
042600080206      /free
042700080206
042800080206       // Operazioni iniziali
042900080206       exsr RoutInz;
043000110309
043100110309       exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
043200080206
043300080206       // Gestione video
043400080206       DOW $Fine = *off;
043500080206         select;
043600080206           when $Video = 'D1';
043700080206             exsr GesD01;
043800131122           when $Video = 'D2';
043900131122             exsr GesD02;
044000080206           when $Video = 'S1';
044100080206             exsr GesS01;
044200080206           when $Video = 'WW';
044300080206             exsr sr_1_cpo;
044400080206           other;
044500080206             $Fine = *on;
044600080206         endsl;
044700080206       ENDDO;
044800080206
044900080206       // Operazioni finali
045000080206       exsr RoutEnd;
045100080206
045200080206       //--------------------------------------------------------------
045300080206       //?Operazioni iniziali.
045400080206       //--------------------------------------------------------------
045500080206       BEGSR RoutInz;
045600080206
045700080208         // Ricezione parametri
045800091027         TRMK01ds = kpjbu;
045900091028         clear  Mk01APP;
046000091028         Mk01f03 = *off;
046100091028         Mk01f06 = *off;
046200091028         Mk01f12 = *off;
046300091028         Mk01ent = *off;
046400100202
046500100202       // Se la kpjbu è vuota vuol dire che sono da menù
046600100202        IF  kpjbu = *blanks;
046700100202          eval w_damenu = 'S';
046800100202        ENDIF;
046900100408       // Se ho più di un parametro devo ricordarmi di passare anche il trmk19ds
047000100408        IF  %parms > 1   ;
047100100408          eval w_daAtti='S';
047200100408        ENDIF;
047300080206
047400080206       // Se il pgm. NON è richiamato: azzera i campi numerici
047500091028         if  Mk01k11 = *blank;
047600091028           clear  Mk01cpo;
047700091028           clear  Mk01nrv;
047800091028           clear  Mk01ksc;
047900080206         endif;
048000080206
048100080206         // Impostazione campi "fissi"
048200080208         T01pgm = SDSpgm;
048300080208         TBLkut = 1;
048400080206
048500080206         // Reperimento dati job
048600080206         exsr DatiJob;
048700080206
048800080206         // Controllo se ok l'abilitazione dell'utente
048900090701         reset tntaa1ds;
049000090701         itaa1caut='§utepot' ;
049100090701         clear kpjbu   ;
049200090701         kpjbu=tntaa1ds ;
049300090703         TNTAA1C     (kpjba);
049400090702         TNTAA1DS=KPJBU   ;
049500080206
049600131127         if  otaa1fabi='N'and $logistica = *blank ;
049700080206           $ErrGrave   = *on;
049800090702         else ;
049900131125         // in caso di richiami dalla logistica imposto la filiale a 046 e autorizzazione AZ
050000131125         If  $logistica = 'LOG' ;
050100131125             eval dutpou = 046 ;
050200131125             eval otaa1cabi = 'AZ' ;
050300131125         Endif ;
050400090701
050500081020         clear trul31ds  ;
050600090701         i31abi=otaa1cabi;
050700081020         I31cdi = DUTdis   ;
050800081020         I31car = DUTare   ;
050900081020         I31cpo = DUTpou   ;
051000081020         callp TRUL31R (kpjba:trul31ds)   ;
051100081020         if o31pog<=*zeros  ;
051200081020           $ErrGrave   = *on;
051300081020         endif;
051400090702         endif;
051500081020
051600080207
051700080207         // Intabellamento filiali e relative aree
051800080207         setll *loval AZORG;
051900080207         read  AZORG;
052000080208         dow  NOT  %eof(AZORG01L);
052100080208           if  ORGfva = *blank;
052200080207             xx += 1;
052300080208             $Fil(xx) = ORGfil;
052400080208             $Car(xx) = ORGcar;
052500080207           endif;
052600080207           read  AZORG;
052700080207         enddo;
052800110308
052900110308         //?Carico sk Categoria merceologica
053000110308         clear xx;
053100110308         TBLcod = '1L';
053200110308         setll (TBLkut:TBLcod) TABEL00F;
053300110308         reade (TBLkut:TBLcod) TABEL00F;
053400110308
053500110308         DOW not %eof(TABEL00F);
053600110308           IF  TBLflg = *blanks;
053700110308             xx += 1;
053800110309             $1l(xx) = %subst(TBLkey:1:5);
053900110308           ENDIF;
054000110308           reade (TBLkut:TBLcod) TABEL00F;
054100110308         ENDDO;
054200110308
054300110308         //?Carico sk Importanza Clienti
054400110308         clear xx;
054500110309         TBLcod = 'IC';
054600110308         setll (TBLkut:TBLcod) TABEL00F;
054700110308         reade (TBLkut:TBLcod) TABEL00F;
054800110308
054900110308         DOW not %eof(TABEL00F);
055000110308           IF  TBLflg = *blanks;
055100110308             xx += 1;
055200110309             $ic(xx) = %subst(TBLkey:1:1);
055300110308           ENDIF;
055400110308           reade (TBLkut:TBLcod) TABEL00F;
055500110308         ENDDO;
055600110407
055700110407         //?Carico sk Categoria potenziali
055800110407         clear xx;
055900110407         clear yy;
056000110407         C_Tab = 'CPO';
056100110407         setll (C_tab) TNTBE01L;
056200110407         reade (C_tab) TNTBE01L;
056300110407
056400110407         DOW not %eof(TNTBE01L);
056500110407           IF  TBEatb = *blanks;
056600110407             dCPO = TBEuni;
056700110407             xx += 1;
056800110407             $Cpo(xx) = %subst(TBEke1:1:1);
056900110407             IF  §CPOint = 'S';
057000110407               yy += 1;
057100110407               $CpoI(yy) = $Cpo(xx);
057200110407             ENDIF;
057300110407           ENDIF;
057400110407           reade (C_tab) TNTBE01L;
057500110407         ENDDO;
057600110308
057700110308         //?Carico sk Concorrenti
057800110308         clear xx;
057900110308         C_Tab = 'IFS';
058000110309         C_ke1 = '10';
058100110309         setll (C_tab:C_ke1) TNTBE01L;
058200110309         reade (C_tab:C_ke1) TNTBE01L;
058300110308
058400110308         DOW not %eof(TNTBE01L);
058500110308           IF  TBEatb = *blanks;
058600110308             xx += 1;
058700110308             $Ifs(xx) = %subst(TBEke2:1:4);
058800110308           ENDIF;
058900110309           reade (C_tab:C_ke1) TNTBE01L;
059000110308         ENDDO;
059100131122
059200131122         //?Carico sk Concorrenti LOGISTICA
059300131122         clear xx;
059400131122         C_Tab = 'IFS';
059500131122         C_ke1 = '40';
059600131122         setll (C_tab:C_ke1) TNTBE01L;
059700131122         reade (C_tab:C_ke1) TNTBE01L;
059800131122
059900131122         DOW not %eof(TNTBE01L);
060000131122           IF  TBEatb = *blanks;
060100131122             xx += 1;
060200131122             $IfsLOG(xx) = %subst(TBEke2:1:4);
060300131122           ENDIF;
060400131122           reade (C_tab:C_ke1) TNTBE01L;
060500131122         ENDDO;
060600100423
060700100423          dataiso=datasys         ;
060800100423          w0080=%dec(dataiso)     ;
060900100423          Alfaoggi=%editc(w0080:'X')  ;
061000080206
061100080206       ENDSR;
061200080206
061300080206       //--------------------------------------------------------------
061400080206       //?Reperimento Dati del job (Utente/Operativi).
061500080206       //--------------------------------------------------------------
061600080206       BEGSR DatiJob;
061700080206
061800080206         in(E) §AzUte;
061900080206         if NOT %error;
062000080206           in(E) §DatiUte;
062100080206         endif;
062200080206         if %error or RSut = *blanks;
062300080206           clear TIBS34ds;
062400080206           tibs34r(tibs34ds);
062500080206           in §AzUte;
062600080206           in §DatiUte;
062700080206         endif;
062800080206
062900080206       ENDSR;
063000080206
063100080206       //--------------------------------------------------------------
063200080206       //?Gestione videata D01
063300080206       //--------------------------------------------------------------
063400080206       BEGSR GesD01;
063500080206
063600080206         // Inizializzazione videata
063700080206         if $InzD01 = *on;
063800080206           exsr InzD01;
063900080206           $InzD01 = *off;
064000080214         else;
064100080225           if  errGenerico = *off;
064200110308             clear V01cpo;
064300080225           endif;
064400080206         endif;
064500080206
064600080206         // Emissione testata
064700091027           write MK01T01;
064800090702
064900090702         // Se errore grave, ovvero utente non abilitato, emetto msg ed esco dal pgm
065000090702         if $ErrGrave=*on    ;
065100090702           errMessage  = *on;
065200090702           errGenerico = *on;
065300090702           V1Dmsg = $Msg(01);
065400090702         endif   ;
065500080206
065600080206         // Emissione videata
065700091027         exfmt MK01D01;
065800080206         errMessage   = *off;
065900080206         errGenerico  = *off;
066000080206         clear V1Dmsg;
066100080207
066200080206         select;
066300080206         // Rilevato errore grave: qualsiasi tasto venga premuto, esce dal pgm
066400080206           when  $ErrGrave = *on;
066500091028             Mk01f03 = *on;
066600080206             $Fine  = *on;
066700110308
066800080206         // F3=Fine
066900080208           when dsp_aid = c_F03;
067000080206             exsr F03D01;
067100110308
067200131122         // F5=Altre Prazializzazioni
067300131122           when dsp_aid = c_F05;
067400131122             exsr CtrD01;
067500131122             if errGenerico;
067600131122               leavesr;
067700131122             endif;
067800131122             exsr F05D01;
067900131122
068000080206         // F10=Immissione
068100080207           when dsp_aid = c_F10;
068200080206             exsr F10D01;
068300110308
068400131122         // F11=Pulizia
068500131122           when dsp_aid = c_F11;
068600131122             exsr F11D01;
068700131122
068800081021         // F14=normalizzazione
068900081021           when dsp_aid = c_F14;
069000081021             exsr F14S01;
069100081021
069200080206         // Enter
069300080206           other;
069400080206             exsr CtrD01;
069500080206             if errGenerico;
069600080206               leavesr;
069700080206             endif;
069800110308
069900080208             if  $Ord = 1;
070000080208               $Video = 'WW';
070100080208             else;
070200080208               $Video = 'S1';
070300080208             endif;
070400110308
070500080206             $InzS01 = *on;
070600080206         endsl;
070700080206
070800080206       ENDSR;
070900080206
071000080206       //--------------------------------------------------------------
071100080206       //?Inizializzazione videata D01
071200080206       //--------------------------------------------------------------
071300080206       BEGSR InzD01;
071400080206
071500091027         clear MK01D01;
071600080206
071700080206         // Si è in "Gestione clienti potenziali" SE
071800091030         // - pgm. NON richiamato da altro pgm.  & azione MK02
071900080206         // (altrimenti si è in "Visualizzazione clienti potenziali")
072000091030         GESclipot = (Mk01k11 = *blank  and  kcdaz = 'MK02');
072100080206
072200080206         // Abilitazione tasto funzionale F10=Immissione
072300091028         F10Attivo = (Mk01imm = 'S'   or   GesCliPot = *on);
072400081020
072500091028         PGMrichia=Mk01k11<>' '    ;
072600080208
072700080206         // Se richiamato e passata, imposto la ricerca alfabetica
072800080206         select;
072900091028           when  Mk01k11 =  *blank;
073000091028             clear Mk01rag;
073100091028           when  Mk01k11 <> *blank   and
073200091028                 Mk01k11 <> '2';
073300110308             V01rag = Mk01rag;
073400080206         endsl;
073500080208
073600110520         //?Se da menù imposto ordinamento per ragione sociale
073700110520         IF  w_damenu <> *blanks;
073800110520           V01osf  = 'R';
073900110520         //?Se da F7 imposto ordinamento per importanza cliente
074000110520         ELSE;
074100110520           V01osf  = 'I';
074200110520         ENDIF;
074300131126         //?Se RICHIAMATO DALLA LOGISTICA  importanza cliente
074400131126         If $logistica = 'LOG';
074500131126           V01osf  = 'I';
074600131126         ENDIF;
074700110407
074800110407         //?Imposto le categorie potenziale da selezionare di default
074900110601         //?Mai codificato - Perso  (recupero da tab. CPO)
075000131127         //?Solo se non è da menù
075100131127         IF  w_damenu = *blanks ;
075200110518           V01fls1 = $CpoI(1);
075300110518           V01fls2 = $CpoI(2);
075400110518           V01fls3 = $CpoI(3);
075500110518           V01fls4 = $CpoI(4);
075600110518           V01fls5 = $CpoI(5);
075700110518         ENDIF;
075800080206
075900080206       ENDSR;
076000080206
076100131125       //--------------------------------------------------------------
076200131125       //?Gestione videata D02
076300131125       //--------------------------------------------------------------
076400131125       BEGSR GesD02;
076500131125
076600131125         // Inizializzazione videata
076700131125         if $InzD02 = *on;
076800131125           exsr InzD02;
076900131125           $InzD02 = *off;
077000131125         endif;
077100131125
077200131125         // Emissione testata
077300131125           write MK01T01;
077400131125
077500131125         // Emissione videata
077600131125         exfmt MK01D02;
077700131125         errMessage   = *off;
077800131125         errGenerico  = *off;
077900131125         clear V2Dmsg;
078000131125
078100131125         select;
078200131125         // Rilevato errore grave: qualsiasi tasto venga premuto, esce dal pgm
078300131125           when  $ErrGrave = *on;
078400131125             Mk01f03 = *on;
078500131125             $Fine  = *on;
078600131125
078700131127         // Enter
078800131127           other;
078900131127             exsr CtrD02;
079000131127             if errGenerico;
079100131127               leavesr;
079200131127             endif;
079300131127
079400131127             $Video = 'D1';
079500131125             $InzS01 = *on;
079600131127
079700131125         endsl;
079800131125
079900131125       ENDSR;
080000131125
080100131125       //--------------------------------------------------------------
080200131125       //?Inizializzazione videata D02
080300131125       //--------------------------------------------------------------
080400131125       BEGSR InzD02;
080500131125
080600131125         clear MK01D02;
080700131127         clear wdil1;
080800131127         clear wdil2;
080900131125
081000131125       ENDSR;
081100131125
081200080206       //--------------------------------------------------------------
081300080206       //?Gestione tasto funzionale F3 da videata D01
081400080206       //?F3=Fine
081500080206       //--------------------------------------------------------------
081600080206       BEGSR F03D01;
081700080206
081800080207         // Chiusura del programma
081900080206         $Fine = *on;
082000080206
082100080206         // Restituzione indicazione del tasto funzionale premuto
082200091028         Mk01f03 = *on;
082300080206
082400080206       ENDSR;
082500080206
082600131122       //--------------------------------------------------------------
082700131122       //?Gestione tasto funzionale F5 da videata D01
082800131122       //?F5=Altre parzializzazioni
082900131122       //--------------------------------------------------------------
083000131122       BEGSR F05D01;
083100131122
083200131122       //?Videata sucessiva
083300131125       //?Se l'infdicatore di altre parzializzazioni è spento pulisco il viedo altrimenti no
083400131125       If altreparz = *off ;
083500131122         $InzD02 = *on;
083600131125       ENDIF ;
083700131122
083800131125         $Video = 'D2';
083900131125
084000131122       ENDSR;
084100131122
084200080206       //--------------------------------------------------------------
084300080206       //?Gestione tasto funzionale F10 da videata D01
084400080206       //?F10=Immissione
084500080206       //--------------------------------------------------------------
084600080206       BEGSR F10D01;
084700080206
084800091028         Mk01k10 = 'I';
084900100202         kpjbu  = w_damenu;
085000091027         trmk02r(kpjba : trmk01ds);
085100080206
085200080206         select;
085300080206         // Se premuto F12 => si riemette la videata di selezione
085400091028           when  Mk01f03 = *on  or  Mk01f12 = *on;
085500080206             leavesr;
085600080206         // Se premuto F6 (quindi immesso il potenziale) => si emette
085700080206         //   il subfile in ordine di ragione sociale
085800091028           when  Mk01f06 = *on;
085900110309             V01rag   = Mk01rag;
086000080206         endsl;
086100080206
086200080206         // Se restituito errore => lo si visualizza
086300091028         if  Mk01err = 'E';
086400080206           errMessage  = *on;
086500080206           errGenerico = *on;
086600091028           V1Dmsg = Mk01m10;
086700110309           V01cpo = %editc(Mk01cpo : 'X');
086800080206           leavesr;
086900080206         endif;
087000080206
087100080206       ENDSR;
087200080206
087300131122       //--------------------------------------------------------------
087400131122       //?Gestione tasto funzionale F11 da videata D01
087500131122       //?F11=Pulizia
087600131122       //--------------------------------------------------------------
087700131122       BEGSR F11D01;
087800131122
087900131122       //?Pulizia delle videate 1 e 2
088000131122         $Video = 'D1';
088100131127         exsr Inzd01;
088200131127         exsr InzD02;
088300131122
088400131125         altreparz = *off;
088500131122
088600131122       ENDSR;
088700131125
088800080206       //--------------------------------------------------------------
088900080206       //?Controllo videata D01
089000080206       //--------------------------------------------------------------
089100080206       BEGSR CtrD01;
089200080206
089300080206         WindDspF = IndDspF;
089400080206         reset WindDspFb;
089500080206         IndDspF  = WindDspF;
089600110309
089700110309       //?Ordinamento Elenco
089800110309         IF  V01osf <> 'R'   and
089900110309             V01osf <> 'I';
090000110309           ErrMessage  = *on;
090100110309           ErrGenerico = *on;
090200110309           PosCurOSF   = *on;
090300110309           V1Dmsg = $Msg(07);
090400110309           leavesr;
090500110309         ENDIF;
090600080206
090700080206         SELECT;
090800110309
090900110309         //?Se inserita ragione sociale no ordinamento per importanza cliente
091000110601         //?per ora lo tolgo
091100110309           WHEN  V01rag <> *blanks and V01osf <> 'R';
091200110520             //ErrMessage  = *on;
091300110520             //ErrGenerico = *on;
091400110520             //PosCurOsf   = *on;
091500110520             //V1Dmsg      = $Msg(18);
091600110520             //leavesr;
091700110308
091800110308         //?Se inseriti dati info commerciali obbligo 1 dato delle parzializzazioni
091900110310           WHEN   (V01treda > *zeros or V01trea > *zeros or
092000110308                   V01pkgda > *zeros or V01pkga > *zeros or
092100110601                   V01she <> *blanks or V01conc <> *blanks or
092200110601                   V01sbx <> *blanks)  and
092300110308                   V01fls = *blanks and V01pst = *blanks and
092400110308                   V01rag = *blanks and V01loc = *blanks and
092500110308                   V01prv = *blanks and V01cap = *blanks and
092600110308                   V01flt = *zeros  and V01car = *blanks and
092700110308                   V01piv = *blanks and V01cdf = *blanks and
092800110308                   V01sct = *blanks and V01cic = *blanks;
092900110308             ErrMessage  = *on;
093000110308             ErrGenerico = *on;
093100110308             V1Dmsg      = $Msg(04);
093200110308             leavesr;
093300110601
093400110601         //?Se inserita selezione per trattativa obbligo 1 dato delle parzializzazioni
093500110601           WHEN   V01ttr <> *blanks and
093600110601                   V01fls = *blanks and V01pst = *blanks and
093700110601                   V01rag = *blanks and V01loc = *blanks and
093800110601                   V01prv = *blanks and V01cap = *blanks and
093900110601                   V01flt = *zeros  and V01car = *blanks and
094000110601                   V01piv = *blanks and V01cdf = *blanks and
094100110601                   V01sct = *blanks and V01cic = *blanks;
094200110601             ErrMessage  = *on;
094300110601             ErrGenerico = *on;
094400110616             V1Dmsg      = $Msg(12);
094500110601             leavesr;
094600110601         ENDSL;
094700110308
094800110308       //?Importanza Cliente
094900110308         IF  V01cic1 <> *blanks;
095000110308           IF    %scan('?' : V01cic1) > 0;
095100110308             ErrGenerico = *on;
095200110308             PosCurCic1  = *on;
095300110308             idTabella = 'IC';
095400110308             Tabel_Ricerca (idTabella : Ordinamento : idElemento :
095500110308                            tastoUscita);
095600110309             V01cic1 = idElemento;
095700110308           ENDIF;
095800110308           IF  %lookup(V01cic1 : $Ic) = 0;
095900110308             ErrMessage  = *on;
096000110308             ErrGenerico = *on;
096100110308             PosCurCic1  = *on;
096200110309             V1Dmsg = $Msg(17);
096300110308             leavesr;
096400110308           ENDIF;
096500110308         ENDIF;
096600110308         IF  V01cic2 <> *blanks;
096700110308           IF    %scan('?' : V01cic2) > 0;
096800110308             ErrGenerico = *on;
096900110308             PosCurCic2  = *on;
097000110308             idTabella = 'IC';
097100110308             Tabel_Ricerca (idTabella : Ordinamento : idElemento :
097200110308                            tastoUscita);
097300110308             V01cic2 = idElemento;
097400110308           ENDIF;
097500110308           IF  %lookup(V01cic2 : $Ic) = 0;
097600110308             ErrMessage  = *on;
097700110308             ErrGenerico = *on;
097800110308             PosCurCic2  = *on;
097900110309             V1Dmsg = $Msg(17);
098000110308             leavesr;
098100110308           ENDIF;
098200110308         ENDIF;
098300110308         IF  V01cic3 <> *blanks;
098400110308           IF    %scan('?' : V01cic3) > 0;
098500110308             ErrGenerico = *on;
098600110308             PosCurCic3  = *on;
098700110308             idTabella = 'IC';
098800110308             Tabel_Ricerca (idTabella : Ordinamento : idElemento :
098900110308                            tastoUscita);
099000110308             V01cic3 = idElemento;
099100110308           ENDIF;
099200110308           IF  %lookup(V01cic3 : $Ic) = 0;
099300110308             ErrMessage  = *on;
099400110308             ErrGenerico = *on;
099500110308             PosCurCic3  = *on;
099600110309             V1Dmsg = $Msg(17);
099700110308             leavesr;
099800110308           ENDIF;
099900110308         ENDIF;
100000110308         IF  V01cic4 <> *blanks;
100100110308           IF    %scan('?' : V01cic4) > 0;
100200110308             ErrGenerico = *on;
100300110308             PosCurCic4  = *on;
100400110308             idTabella = 'IC';
100500110308             Tabel_Ricerca (idTabella : Ordinamento : idElemento :
100600110308                            tastoUscita);
100700110308             V01cic4 = idElemento;
100800110308           ENDIF;
100900110308           IF  %lookup(V01cic4 : $Ic) = 0;
101000110308             ErrMessage  = *on;
101100110308             ErrGenerico = *on;
101200110308             PosCurCic4  = *on;
101300110309             V1Dmsg = $Msg(17);
101400110308             leavesr;
101500110308           ENDIF;
101600110308         ENDIF;
101700110308         IF  V01cic5 <> *blanks;
101800110308           IF    %scan('?' : V01cic5) > 0;
101900110308             ErrGenerico = *on;
102000110308             PosCurCic5  = *on;
102100110308             idTabella = 'IC';
102200110308             Tabel_Ricerca (idTabella : Ordinamento : idElemento :
102300110308                            tastoUscita);
102400110308             V01cic5 = idElemento;
102500110308           ENDIF;
102600110308           IF  %lookup(V01cic5 : $Ic) = 0;
102700110308             ErrMessage  = *on;
102800110308             ErrGenerico = *on;
102900110308             PosCurCic5  = *on;
103000110309             V1Dmsg = $Msg(17);
103100110308             leavesr;
103200110308           ENDIF;
103300110308         ENDIF;
103400110322         IF  V01cic <> *blanks;
103500110322           clear $cic;
103600110322           $cic(01) = V01cic1;
103700110322           $cic(02) = V01cic2;
103800110322           $cic(03) = V01cic3;
103900110322           $cic(04) = V01cic4;
104000110322           $cic(05) = V01cic5;
104100110322         ENDIF;
104200110308
104300110308         //?Categoria potenziale
104400110407         IF  V01fls1 <> *blanks;
104500110407           IF  %scan('?' : V01fls1) > 0;
104600110308             ErrGenerico = *on;
104700110407             PosCurFls1  = *on;
104800110509             clear TNTB87ds;
104900110509             clear V01fls1;
105000110509             tntb87r  (tntb87ds);
105100110509             IF  TB87fxx = *blanks and TB87err = *off;
105200110509               V01fls1 = TB87cod;
105300110308             ENDIF;
105400110308           ENDIF;
105500110407           IF  %lookup(V01fls1 : $Cpo) = 0;
105600110314             ErrMessage  = *on;
105700110314             ErrGenerico = *on;
105800110407             PosCurFls1  = *on;
105900110314             V1Dmsg = $Msg(24);
106000110314             leavesr;
106100110308           ENDIF;
106200110308         ENDIF;
106300110407         IF  V01fls2 <> *blanks;
106400110407           IF  %scan('?' : V01fls2) > 0;
106500110407             ErrGenerico = *on;
106600110407             PosCurFls2  = *on;
106700110509             clear TNTB87ds;
106800110509             clear V01fls2;
106900110509             tntb87r  (tntb87ds);
107000110509             IF  TB87fxx = *blanks and TB87err = *off;
107100110509               V01fls2 = TB87cod;
107200110509             ENDIF;
107300110407           ENDIF;
107400110407           IF  %lookup(V01fls2 : $Cpo) = 0;
107500110407             ErrMessage  = *on;
107600110407             ErrGenerico = *on;
107700110407             PosCurFls2  = *on;
107800110407             V1Dmsg = $Msg(24);
107900110407             leavesr;
108000110407           ENDIF;
108100110407         ENDIF;
108200110407         IF  V01fls3 <> *blanks;
108300110407           IF  %scan('?' : V01fls3) > 0;
108400110407             ErrGenerico = *on;
108500110407             PosCurFls3  = *on;
108600110509             clear TNTB87ds;
108700110509             clear V01fls3;
108800110509             tntb87r  (tntb87ds);
108900110509             IF  TB87fxx = *blanks and TB87err = *off;
109000110509               V01fls3 = TB87cod;
109100110509             ENDIF;
109200110407           ENDIF;
109300110407           IF  %lookup(V01fls3 : $Cpo) = 0;
109400110407             ErrMessage  = *on;
109500110407             ErrGenerico = *on;
109600110407             PosCurFls3  = *on;
109700110407             V1Dmsg = $Msg(24);
109800110407             leavesr;
109900110407           ENDIF;
110000110407         ENDIF;
110100110407         IF  V01fls4 <> *blanks;
110200110407           IF  %scan('?' : V01fls4) > 0;
110300110407             ErrGenerico = *on;
110400110407             PosCurFls4  = *on;
110500110509             clear TNTB87ds;
110600110509             clear V01fls4;
110700110509             tntb87r  (tntb87ds);
110800110509             IF  TB87fxx = *blanks and TB87err = *off;
110900110509               V01fls4 = TB87cod;
111000110509             ENDIF;
111100110407           ENDIF;
111200110407           IF  %lookup(V01fls4 : $Cpo) = 0;
111300110407             ErrMessage  = *on;
111400110407             ErrGenerico = *on;
111500110407             PosCurFls4  = *on;
111600110407             V1Dmsg = $Msg(24);
111700110407             leavesr;
111800110407           ENDIF;
111900110407         ENDIF;
112000110407         IF  V01fls5 <> *blanks;
112100110407           IF  %scan('?' : V01fls5) > 0;
112200110407             ErrGenerico = *on;
112300110407             PosCurFls5  = *on;
112400110509             clear TNTB87ds;
112500110509             clear V01fls5;
112600110509             tntb87r  (tntb87ds);
112700110509             IF  TB87fxx = *blanks and TB87err = *off;
112800110509               V01fls5 = TB87cod;
112900110509             ENDIF;
113000110407           ENDIF;
113100110407           IF  %lookup(V01fls5 : $Cpo) = 0;
113200110407             ErrMessage  = *on;
113300110407             ErrGenerico = *on;
113400110407             PosCurFls5  = *on;
113500110407             V1Dmsg = $Msg(24);
113600110407             leavesr;
113700110407           ENDIF;
113800110407         ENDIF;
113900110407         IF  V01fls <> *blanks;
114000110407           clear $fls;
114100110407           $fls(01) = V01fls1;
114200110407           $fls(02) = V01fls2;
114300110407           $fls(03) = V01fls3;
114400110407           $fls(04) = V01fls4;
114500110407           $fls(05) = V01fls5;
114600110407         ENDIF;
114700110308
114800110308         // Se inserito il cap o la località è obbligatoria la provincia
114900110308           IF    (V01cap <> *blank  or  V01loc <> *blanks)  and
115000110308                  V01prv  = *blank;
115100110308             errMessage  = *on;
115200110308             errGenerico = *on;
115300110308             PosCurPRV   = *on;
115400110308             V1Dmsg      = $Msg(03);
115500110308             leavesr;
115600110308           ENDIF;
115700080206
115800080206         // Se inserito cap e provincia controllo se sono congruenti
115900110308           if  V01cap <> *blank;
116000080206             reset  fnlv13ds;
116100080206             reset  tisi95ds;
116200110309             I95cap = V01cap;
116300110309             I95prv = V01prv;
116400080206             fnlv13r(kpjba : fnlv13ds : tisi95ds);
116500080206             if  O13err <> *blank;
116600080206               errMessage  = *on;
116700080206               errGenerico = *on;
116800080206               select;
116900080206                 when  O13err = '4';
117000080206                   PosCurPRV   = *on;
117100080206                 when  O13err = '5';
117200080206                   PosCurCAP   = *on;
117300080206               endsl;
117400080206               V1Dmsg = O13msg;
117500080206               leavesr;
117600080206             endif;
117700080206           endif;
117800110308
117900110308         //?Filiale appartenenza
118000110308         IF  V01flt > 0;
118100110308           chain V01flt AZORG01L;
118200110308           IF  not %found(AZORG01L) or ORGfva <> *blanks;
118300110308             ErrMessage  = *on;
118400110308             ErrGenerico = *on;
118500110308             PosCurFlt   = *on;
118600110308             V1Dmsg      = $Msg(16);
118700110308             leavesr;
118800110308           ENDIF;
118900110308         ENDIF;
119000110308
119100110308         // Ricerca/Controllo/Decodifica codice area
119200110308         clear  V01carD;
119300110308         TBLcod = '05';
119400110308         TBLkey = V01car;
119500110308         if  V01car = *zero;
119600110308           clear  V01car;
119700110308         endif;
119800110308         SELECT;
119900110308         // - Ricerca
120000110308           WHEN  %scan('?' : V01car) > *zero;
120100110308             errGenerico = *on;
120200110308             PosCurCAR  = *on;
120300110308             clear w030a;
120400110308             x§taber(TBLkut : TBLcod : TBLkey : w030a);
120500110308         // - - F3 invece di scelta codice
120600110308             if  TBLkey = *blank;
120700110308               clear  V01car;
120800110308         // - - effettuata scelta
120900110308             else;
121000110308               V01car   = TBLkey;
121100110309               V01carD  = %subst(w030a : 1 : %len(V01carD));
121200110308             endif;
121300110308             leavesr;
121400110308         // - Controllo validità del codice area
121500110308           WHEN  V01car <> *blank;
121600110308             clear  ds05;
121700110308             chain (TBLkut : TBLcod : TBLkey) TABEL;
121800110308             if  NOT  %found(TABEL00F)
121900110308                 or   TBLflg <> *blank;
122000110308               errMessage  = *on;
122100110308               errGenerico = *on;
122200110308               PosCurCAR   = *on;
122300110308               V1Dmsg = $Msg(06);
122400110308               leavesr;
122500110308             else;
122600110308               ds05    = TBLuni;
122700110308               V01carD = §05des;
122800110308             endif;
122900110308         ENDSL;
123000050519
123100110308       //?Categoria merceologica
123200110615         IF  V01sct1 <> *blanks;
123300110308           IF    %scan('?' : V01sct1) > 0;
123400110308             ErrGenerico = *on;
123500110308             PosCurSct1  = *on;
123600110309             idTabella = '1L';
123700110308             Tabel_Ricerca (idTabella : Ordinamento : idElemento :
123800110308                            tastoUscita);
123900110308             V01sct1 = idElemento;
124000110308           ENDIF;
124100110615           IF  V01sct1 <> *zeros;
124200110615             IF  %lookup(V01sct1 : $1l) = 0;
124300110615               ErrMessage  = *on;
124400110615               ErrGenerico = *on;
124500110615               PosCurSct1  = *on;
124600110615               V1Dmsg = $Msg(05);
124700110615               leavesr;
124800110615             ENDIF;
124900110308           ENDIF;
125000110308         ENDIF;
125100110308         IF  V01sct2 <> *blanks;
125200110308           IF    %scan('?' : V01sct2) > 0;
125300110308             ErrGenerico = *on;
125400110308             PosCurSct2  = *on;
125500110309             idTabella = '1L';
125600110308             Tabel_Ricerca (idTabella : Ordinamento : idElemento :
125700110308                            tastoUscita);
125800110308             V01sct2 = idElemento;
125900110308           ENDIF;
126000110615           IF  V01sct2 <> *zeros;
126100110615             IF  %lookup(V01sct2 : $1l) = 0;
126200110615               ErrMessage  = *on;
126300110615               ErrGenerico = *on;
126400110615               PosCurSct2  = *on;
126500110615               V1Dmsg = $Msg(05);
126600110615               leavesr;
126700110615             ENDIF;
126800110308           ENDIF;
126900110308         ENDIF;
127000110308         IF  V01sct3 <> *blanks;
127100110308           IF    %scan('?' : V01sct3) > 0;
127200110308             ErrGenerico = *on;
127300110308             PosCurSct3  = *on;
127400110309             idTabella = '1L';
127500110308             Tabel_Ricerca (idTabella : Ordinamento : idElemento :
127600110308                            tastoUscita);
127700110308             V01sct3 = idElemento;
127800110308           ENDIF;
127900110615           IF  V01sct3 <> *zeros;
128000110615             IF  %lookup(V01sct3 : $1l) = 0;
128100110615               ErrMessage  = *on;
128200110615               ErrGenerico = *on;
128300110615               PosCurSct3  = *on;
128400110615               V1Dmsg = $Msg(05);
128500110615               leavesr;
128600110615             ENDIF;
128700110308           ENDIF;
128800110308         ENDIF;
128900080213
129000080206         // Imposta il tipo selezioni da eseguire:
129100110310
129200080207         clear $Ord;
129300080207         select;
129400080207           // 1 = Singolo codice cliente potenziale
129500110308           when  V01cpo <> *zero;
129600080206             $Ord   = 1;
129700110520
129800110520           // 2 = per Ragione Sociale se richiesto ordinamento x Ragione sociale
129900110520           when  V01osf = 'R';
130000110520             $Ord   = 2;
130100110309
130200110309           // 3 = per Categoria merceologica
130300110309           when  V01sct  > *zero;
130400110309             $Ord   = 3;
130500110309
130600110309           // 4 = per Provincia
130700110309           when  V01prv <> *blank;
130800110309             $Ord   = 4;
130900110309
131000110309           // 5 = per Codice fiscale
131100110309           when  V01cdf <> *blank;
131200110309             $Ord   = 5;
131300110309
131400110309           // 6 = per Partita iva
131500110309           when  V01piv <> *blank;
131600110309             $Ord   = 6;
131700110309
131800110309           // 7 = per Importanza cliente
131900110309           when  V01cic <> *blank;
132000110309             $Ord   = 7;
132100110309
132200110309           // 8 = per Filiale o Area
132300110309           when  V01flt > *zeros or V01car <> *blanks;
132400110309             $Ord   = 8;
132500110520
132600110520           // 9 = per Categoria Potenziale
132700110520           when  V01fls <> *blank;
132800110520             $Ord   = 9;
132900110309
133000110309           // 10 = per Potenziali mai contattati
133100110310           //      imposto $ord solo per proforma
133200110309           when  V01pst <> *blank;
133300110309             $Ord   = 10;
133400110520
133500080206         endsl;
133600110518
133700110601       //?Se richiesto codice potenziale
133800110518       //?finisco qua i controlli
133900110601         IF  V01cpo > *zeros;
134000110518           leavesr;
134100110518         ENDIF;
134200110310
134300110310       //?Controllo parzializzazioni INFO
134400110310       //?Spesa trasporti ESTERO
134500110310         IF  V01treda <> 0 or V01trea <> 0;
134600110310         //?Obbligatori tutti e 2 se immesso uno dei due
134700110310           IF  V01treda <> 0  and  V01trea = 0;
134800110310             ErrMessage  = *on;
134900110310             ErrGenerico = *on;
135000110310             PosCurTrea  = *on;
135100110310             V1Dmsg      = $Msg(19);
135200110310             leavesr;
135300110310           ENDIF;
135400110310           IF  V01treda = 0  and  V01trea <> 0;
135500110310             ErrMessage  = *on;
135600110310             ErrGenerico = *on;
135700110310             PosCurTreda = *on;
135800110310             V1Dmsg      = $Msg(19);
135900110310             leavesr;
136000110310           ENDIF;
136100110310         //?Controllo congruenza tra "DA" e "A"
136200110310           IF  V01treda > V01trea;
136300110310             ErrMessage  = *on;
136400110310             ErrGenerico = *on;
136500110310             PosCurTreda = *on;
136600110310             V1Dmsg      = $Msg(20);
136700110310             leavesr;
136800110310           ENDIF;
136900110310         ENDIF;
137000110310
137100110310       //?Peso medio a Spedizione
137200110310         IF  V01pkgda <> 0 or V01pkga <> 0;
137300110310         //?Obbligatori tutti e 2 se immesso uno dei due
137400110310           IF  V01pkgda <> 0  and  V01pkga = 0;
137500110310             ErrMessage  = *on;
137600110310             ErrGenerico = *on;
137700110310             PosCurPkga  = *on;
137800110310             V1Dmsg      = $Msg(21);
137900110310             leavesr;
138000110310           ENDIF;
138100110310           IF  V01pkgda = 0  and  V01pkga <> 0;
138200110310             ErrMessage  = *on;
138300110310             ErrGenerico = *on;
138400110310             PosCurPkgda = *on;
138500110310             V1Dmsg      = $Msg(21);
138600110310             leavesr;
138700110310           ENDIF;
138800110310         //?Controllo congruenza tra "DA" e "A"
138900110310           IF  V01pkgda > V01pkga;
139000110310             ErrMessage  = *on;
139100110310             ErrGenerico = *on;
139200110310             PosCurPkgda = *on;
139300110310             V1Dmsg      = $Msg(22);
139400110310             leavesr;
139500110310           ENDIF;
139600110310         ENDIF;
139700110310
139800110310         //?Concorrente 1
139900110310         IF  V01conc1 <> *blanks;
140000110310           IF    %scan('?' : V01conc1) > 0;
140100110310             ErrGenerico = *on;
140200110310             PosCurConc1  = *on;
140300110310             clear TIBS02ds;
140400110310             T02mod = 'R';
140500110310             T02cod = 'IFS';
140600110310             T02ke1 = '10';
140700110310             T02sif = KNSIF;
140800110310             TNTBE_RicercaControllo  (kpjba : tibs02ds);
140900110310             IF  T02err = *blanks;
141000110310               V01conc1 = t02ke2;
141100110310             ENDIF;
141200110310           ENDIF;
141300110310           IF  %lookup(V01conc1 : $Ifs) = 0;
141400110310             ErrMessage  = *on;
141500110310             ErrGenerico = *on;
141600110310             PosCurConc1 = *on;
141700110310             V1Dmsg = $Msg(23);
141800110310             leavesr;
141900110310           ENDIF;
142000110310         ENDIF;
142100110310         //?Concorrente 2
142200110310         IF  V01conc2 <> *blanks;
142300110310           IF    %scan('?' : V01conc2) > 0;
142400110310             ErrGenerico = *on;
142500110310             PosCurConc2  = *on;
142600110310             clear TIBS02ds;
142700110310             T02mod = 'R';
142800110310             T02cod = 'IFS';
142900110310             T02ke1 = '10';
143000110310             T02sif = KNSIF;
143100110310             TNTBE_RicercaControllo  (kpjba : tibs02ds);
143200110310             IF  T02err = *blanks;
143300110310               V01conc2 = t02ke2;
143400110310             ENDIF;
143500110310           ENDIF;
143600110310           IF  %lookup(V01conc2 : $Ifs) = 0;
143700110310             ErrMessage  = *on;
143800110310             ErrGenerico = *on;
143900110310             PosCurConc2 = *on;
144000110310             V1Dmsg = $Msg(23);
144100110310             leavesr;
144200110310           ENDIF;
144300110310         ENDIF;
144400110310         //?Concorrente 3
144500110310         IF  V01conc3 <> *blanks;
144600110310           IF    %scan('?' : V01conc3) > 0;
144700110310             ErrGenerico = *on;
144800110310             PosCurConc3  = *on;
144900110310             clear TIBS02ds;
145000110310             T02mod = 'R';
145100110310             T02cod = 'IFS';
145200110310             T02ke1 = '10';
145300110310             T02sif = KNSIF;
145400110310             TNTBE_RicercaControllo  (kpjba : tibs02ds);
145500110310             IF  T02err = *blanks;
145600110310               V01conc3 = t02ke2;
145700110310             ENDIF;
145800110310           ENDIF;
145900110310           IF  %lookup(V01conc3 : $Ifs) = 0;
146000110310             ErrMessage  = *on;
146100110310             ErrGenerico = *on;
146200110310             PosCurConc3 = *on;
146300110310             V1Dmsg = $Msg(23);
146400110310             leavesr;
146500110310           ENDIF;
146600110310         ENDIF;
146700080206
146800080206       ENDSR;
146900131125
147000131125       //--------------------------------------------------------------
147100131125       //?Controllo videata D02
147200131125       //--------------------------------------------------------------
147300131125       BEGSR CtrD02;
147400131125
147500131125         WindDspF = IndDspF;
147600131125         reset WindDspFb;
147700131125         IndDspF  = WindDspF;
147800131125
147900131125       //?Interessati alla Logistica
148000131125       //?Se inserite le data di immissione interessamento alla logistica inserire anche S
148100131125         IF  V02int = ' ' and
148200131125             (V02dil1 > 0 or V02dil2 > 0);
148300131125           ErrMessage  = *on;
148400131125           ErrGenerico = *on;
148500131125           PosCurINT   = *on;
148600131125           V2Dmsg = $Msg(25);
148700131125           leavesr;
148800131125         ENDIF;
148900131125
149000131125       //?Se inserite "S" potenziali interessati alla LOGISTICA inserire anche le date
149100131125       //IF  V02int = 'S' and
149200131125       //    (V02dil1 = 0 and  V02dil2 = 0);
149300131125       //  ErrMessage  = *on;
149400131125       //  ErrGenerico = *on;
149500131125       //  PosCurDIL1  = *on;
149600131125       //  V2Dmsg = $Msg(26);
149700131125       //  leavesr;
149800131125       //ENDIF;
149900131125
150000131125       //?Controllo la correttezza delle date immissione interesse logistica
150100131125           IF v02dil1 > 0 ;
150200131125             clear wlbdat;
150300131125             G02dat = V02dil1;
150400131125             xsrda8(wlbdat);
150500131125             IF  G02err = '1';
150600131125               ErrMessage  = *on;
150700131125               ErrGenerico = *on;
150800131125               PosCurDil1  = *on;
150900131125               V2Dmsg      = $Msg(27);
151000131125               leavesr;
151100131125             ENDIF;
151200131125
151300131125             V02dil1  = G02dat;
151400131125             wdil1    = G02inv;
151500131125
151600131125           ENDIF;
151700131125
151800131125           IF v02dil2 > 0 ;
151900131125             clear wlbdat;
152000131125             G02dat = V02dil2;
152100131125             xsrda8(wlbdat);
152200131125             IF  G02err = '1';
152300131125               ErrMessage  = *on;
152400131125               ErrGenerico = *on;
152500131125               PosCurDil2  = *on;
152600131125               V2Dmsg      = $Msg(27);
152700131125               leavesr;
152800131125             ENDIF;
152900131125
153000131125             V02dil2  = G02dat;
153100131125             wdil2    = G02inv;
153200131125
153300131125           ENDIF;
153400131125
153500131125         //?Se impostata data "dal" e non "al"
153600131125         //?imposto "al" = data del giorno
153700131125           IF  V02dil1  > 0 and V02dil2  = 0;
153800131125             wdil2     = %dec(%date());
153900131125             data_eur  = %date(wdil2:*iso);
154000131125             V02dil2   = %dec(data_eur);
154100131125           ENDIF;
154200131125         //?"DAL" congruente con "AL"
154300131125           IF  wdil1 > 0  and  wdil2 = 0;
154400131125             ErrMessage  = *on;
154500131125             ErrGenerico = *on;
154600131125             PosCurDil1  = *on;
154700131125             V2Dmsg      = $Msg(28);
154800131125             leavesr;
154900131125           ENDIF;
155000131125           IF  wdil1  > 0  and  wdil2  > 0  and  wdil1  > wdil2 ;
155100131125             ErrMessage  = *on;
155200131125             ErrGenerico = *on;
155300131125             PosCurDil2  = *on;
155400131125             V2Dmsg      = $Msg(28);
155500131125             leavesr;
155600131125           ENDIF;
155700131125
155800131125       //?Se inserite "S" potenziali interessati alla LOGISTICA inserire anche se in outsourcing
155900131126       //IF  V02int = 'S' and
156000131126       //     V02LOS = *blank;
156100131126       //  ErrMessage  = *on;
156200131126       //  ErrGenerico = *on;
156300131126       //  PosCurLOS   = *on;
156400131126       //  V2Dmsg = $Msg(29);
156500131126       //  leavesr;
156600131126       //ENDIF;
156700131125
156800131127       //?Se inserite Concorrenti ci deve essere  anche se logistica in outsourcing = SI
156900131125         IF  V02conl <> *blanks  and
157000131127              V02LOS <> 'S' ;
157100131125           ErrMessage  = *on;
157200131125           ErrGenerico = *on;
157300131125           PosCurLOS   = *on;
157400131125           V2Dmsg = $Msg(30);
157500131125           leavesr;
157600131125         ENDIF;
157700131125
157800131125         //?Concorrente 1
157900131125         IF  V02concl1 <> *blanks;
158000131125           IF    %scan('?' : V02concl1) > 0;
158100131125             ErrGenerico = *on;
158200131125             PosCurConcl1  = *on;
158300131125             clear TIBS02ds;
158400131125             T02mod = 'R';
158500131125             T02cod = 'IFS';
158600131125             T02ke1 = '40';
158700131125             T02sif = KNSIF;
158800131125             TNTBE_RicercaControllo  (kpjba : tibs02ds);
158900131125             IF  T02err = *blanks;
159000131125               V02concl1 = t02ke2;
159100131125             ENDIF;
159200131125           ENDIF;
159300131125           IF  %lookup(V02concl1 : $IfsLOG) = 0;
159400131125             ErrMessage  = *on;
159500131125             ErrGenerico = *on;
159600131125             PosCurConcl1 = *on;
159700131125             V2Dmsg = $Msg(31);
159800131125             leavesr;
159900131125           ENDIF;
160000131125         ENDIF;
160100131125         //?Concorrente 2
160200131125         IF  V02concl2 <> *blanks;
160300131125           IF    %scan('?' : V02concl2) > 0;
160400131125             ErrGenerico = *on;
160500131125             PosCurConcl2  = *on;
160600131125             clear TIBS02ds;
160700131125             T02mod = 'R';
160800131125             T02cod = 'IFS';
160900131125             T02ke1 = '40';
161000131125             T02sif = KNSIF;
161100131125             TNTBE_RicercaControllo  (kpjba : tibs02ds);
161200131125             IF  T02err = *blanks;
161300131125               V02concl2 = t02ke2;
161400131125             ENDIF;
161500131125           ENDIF;
161600131125           IF  %lookup(V02concl2 : $IfsLOG) = 0;
161700131125             ErrMessage  = *on;
161800131125             ErrGenerico = *on;
161900131125             PosCurConcl2 = *on;
162000131125             V2Dmsg = $Msg(31);
162100131125             leavesr;
162200131125           ENDIF;
162300131125         ENDIF;
162400131125         //?Concorrente 3
162500131125         IF  V02concl3 <> *blanks;
162600131125           IF    %scan('?' : V02concl3) > 0;
162700131125             ErrGenerico = *on;
162800131125             PosCurConcl3  = *on;
162900131125             clear TIBS02ds;
163000131125             T02mod = 'R';
163100131125             T02cod = 'IFS';
163200131125             T02ke1 = '40';
163300131125             T02sif = KNSIF;
163400131125             TNTBE_RicercaControllo  (kpjba : tibs02ds);
163500131125             IF  T02err = *blanks;
163600131125               V02concl3 = t02ke2;
163700131125             ENDIF;
163800131125           ENDIF;
163900131125           IF  %lookup(V02concl3 : $IfsLOG) = 0;
164000131125             ErrMessage  = *on;
164100131125             ErrGenerico = *on;
164200131125             PosCurConcl3 = *on;
164300131125             V2Dmsg = $Msg(31);
164400131125             leavesr;
164500131125           ENDIF;
164600131125         ENDIF;
164700131125
164800131125         //?Se inserite delle selezioni metto in reverse immage F05
164900131125            altreparz = *off;
165000131125         IF V02INT <> ' 'or V02LOS <> ' ' or V02Conl <> *blanks ;
165100131125            altreparz = *on;
165200131125         ENDIF;
165300131125
165400131125       ENDSR;
165500080206
165600080206       //--------------------------------------------------------------
165700080206       //?Gestione selezione di un singolo codice cliente potenziale
165800080206       //--------------------------------------------------------------
165900080206       BEGSR sr_1_cpo;
166000080206
166100110308         CPOcpo = %int(V01cpo);
166200080208         chain  CPOcpo  TNCPO000;
166300080206
166400080206         // Codice potenziale NON trovato
166500080206         if  NOT  %found(TNCPO01L);
166600080213           $Video      = 'D1';
166700080206           errMessage  = *on;
166800080206           errGenerico = *on;
166900080206           PosCurCP1   = *on;
167000080208           V1Dmsg = $Msg(08);
167100080206           leavesr;
167200080206         endif;
167300080206
167400080206         // Programma richiamato
167500091028         if  Mk01k11 = '2'or Mk01k11='4';
167600080208           exsr Return_Dati;
167700080206           $Fine    = *on;
167800080206           leavesr;
167900080206         endif;
168000080206
168100080206         // Pgm. richiamato per interrogazione potenziali
168200080206         exsr Call_TRMK10;
168300080206
168400080206       ENDSR;
168500080206
168600080206       //--------------------------------------------------------------
168700080206       //?Gestione videata S01
168800080206       //--------------------------------------------------------------
168900080206       BEGSR GesS01;
169000080207
169100080207         // Inizializzazione videata
169200080207         if  $InzS01 = *on;
169300080207            exsr InzS01;
169400080207            $InzS01  = *off;
169500080207         endif;
169600080207
169700080207         // Posizionamento cursore
169800080207         if  C01csr  > *zero;
169900080207           C01rcd = C01csr;
170000080207         else;
170100080207         // Se non è stato caricato nulla si riemette la videata D01
170200080207           C01rcd = 1;
170300080207           $Video = 'D1';
170400080207           errMessage  = *on;
170500080207           errGenerico = *on;
170600080208           V1Dmsg = $Msg(09);
170700110309
170800110309         // Chiudo il cursore
170900110309         exec sql
171000110309           close CPO;
171100080207           leavesr;
171200080207         endif;
171300080207
171400080207         // Emissione Testata e Piede con tasti funzionali abilitati
171500091027           write  MK01T01;
171600091027           write  MK01P01;
171700080207
171800080207         // Emissione videata
171900091027         exfmt  MK01C01;
172000080207         reset errMessage;
172100080207         reset errGenerico;
172200080207         clear V1Dmsg;
172300080207
172400080207         SELECT;
172500080207
172600080207         // - F3=Fine
172700080207           WHEN  dsp_aid = c_F03;
172800080207             exsr F03D01;
172900080207
173000080207         // - F12=Ritorno
173100080207           WHEN  dsp_aid = c_F12;
173200080207             exsr F12S01;
173300080626
173400080626         // - F14=riconciliazione potenziali - solo in gestione
173500080626           WHEN  dsp_aid = c_F14;
173600080626             exsr F14S01;
173700080207
173800080207         // - Roll-Up
173900080207           WHEN  dsp_aid = c_RollUp;
174000080208           // - Verifica se raggiunto il limite di records registrabili
174100080208           //   nel subfile (9999)
174200080208             if  S01nrr = *hival;
174300080208               errMessage  = *on;
174400080208               errGenerico = *on;
174500080208               V1Dmsg = $Msg(10);
174600080208             else;
174700080208               exsr RollUpS01;
174800080208             endif;
174900080207
175000080207         // Invio
175100080207           OTHER;
175200080207             exsr OpzS01;
175300080207             if  errGenerico = *on;
175400080207               leavesr;
175500080207             endif;
175600080207
175700080207         ENDSL;
175800080207
175900080207       ENDSR;
176000080207
176100080207       //--------------------------------------------------------------
176200080207       //?Inizializzazione videata S01
176300080207       //--------------------------------------------------------------
176400080207       BEGSR InzS01;
176500080207
176600080207       // Pulizia subfile
176700080207         SflDsp_N    = *on;
176800080207         SflDspCtl_N = *on;
176900091027         write  MK01C01;
177000080207         SflDspCtl_N = *off;
177100080207         SflEnd      = *off;
177200080207
177300080207         clear W_SflPag;
177400080207         clear C01rcd;
177500080207         clear C01csr;
177600080207         clear S01nrr;
177700080207         clear V1Dmsg;
177800080207         errMessage  = *off;
177900080207         errGenerico = *off;
178000080207         WindDspF    = IndDspF;
178100080207         reset WindDspFb;
178200080207         IndDspF     = WindDspF;
178300080207
178400081017
178500080207       // Caricamento dei dati da presentare nel sfl
178600080207         $EoF = *off;
178700080213         select;
178800110309
178900080207         // - per Ragione sociale
179000110520           when  $Ord = 2 and V01osf = 'R';
179100110309             setll  V01rag  TNCPO002;
179200110309
179300110309         // - per Provincia in ordine di ragione sociale
179400110309           when  $Ord = 4 and V01osf = 'R';
179500110309             setll (V01prv : V01rag) TNCPO004;
179600110309
179700110309         // - per tutto il resto
179800110309           other;
179900110309             exsr PrepSql;
180000110309
180100080213         endsl;
180200080213
180300080213         exsr sr_ReadRec;
180400080207
180500080207         dow  $EoF   = *off     and
180600080207              S01nrr < C_SflPag;
180700080207           exsr RollUpS01;
180800080207         enddo;
180900080207
181000080207       // Impaginazione subfile
181100080207       // -> forza l'impaginazione sul 1° rec. del subfile
181200080207         if S01nrr > *zero;
181300080207           C01rcd  = 1;
181400080207           C01csr  = 1;
181500080207         else;
181600080207           clear C01rcd;
181700080207         endif;
181800080207
181900080207       ENDSR;
182000110309       //--------------------------------------------------------------
182100110309       //?Preparo Stringa SQL.
182200110309       //--------------------------------------------------------------
182300110309       BEGSR PrepSql;
182400110309
182500110309       //?Imposto la stringa per SQL
182600110601         IF  V01osf = 'R';
182700110601           wSQL = 'select * from TNCPO00F ';
182800110601         ENDIF;
182900110601         IF  V01osf = 'I';
183000110601           wSQL = 'select tncpo00f.*, -
183100110601                   case cpoftr when '' '' then ''0'' -
183200110601                   else ORD end as CPOord -
183300110601                   from tncpo00f left join -
183400110601                  (select tblkey, substr(tbluni, 31, 1) -
183500110601                   as ORD from tabel00f -
183600110601                   where tblcod = ''IC'') as TAB -
183700110601                   on cpoftr = tblkey ';
183800110601         ENDIF;
183900110601
184000110309         SELECT;
184100110309
184200110309         //?Selezione per categoria merceologica
184300110309           WHEN  $Ord = 3;
184400110309             yy = 0;
184500110309             wSQL += 'WHERE CPOsct in (';
184600110309
184700110615             IF V01sct1 <> *blanks and V01sct1 <> *zeros;
184800110309               IF yy > 0;
184900110309                 wSQL += ', ';
185000110309               ELSE;
185100110309                 yy = 1;
185200110309               ENDIF;
185300110309               wSQL += V01sct1;
185400110309             ENDIF;
185500110615             IF V01sct2 <> *blanks and V01sct2 <> *zeros;
185600110309               IF yy > 0;
185700110309                 wSQL += ', ';
185800110309               ELSE;
185900110309                 yy = 1;
186000110309               ENDIF;
186100110309               wSQL += V01sct2;
186200110309             ENDIF;
186300110615             IF V01sct3 <> *blanks and V01sct3 <> *zeros;
186400110309               IF yy > 0;
186500110309                 wSQL += ', ';
186600110309               ELSE;
186700110309                 yy = 1;
186800110309               ENDIF;
186900110309               wSQL += V01sct3;
187000110309             ENDIF;
187100110309
187200110309           wSQL += ')';
187300110309
187400110309         //?Selezione per provincia (solo in caso di ordinamento per importanza cliente)
187500110309           WHEN  $Ord = 4;
187600110309             wSQL += 'WHERE CPOprv = ';
187700110309             wSQL += '''';
187800110309             wSQL += V01prv;
187900110309             wSQL += '''';
188000110309
188100110309         //?Selezione per codice fiscale
188200110309           WHEN  $Ord = 5;
188300110309             wSQL += 'WHERE CPOcdf = ';
188400110309             wSQL += '''';
188500110309             wSQL += V01cdf;
188600110309             wSQL += '''';
188700110309
188800110309         //?Selezione per partita IVA
188900110309           WHEN  $Ord = 6;
189000110309             wSQL += 'WHERE CPOpiv = ';
189100110309             wSQL += '''';
189200110309             wSQL += V01piv;
189300110309             wSQL += '''';
189400110309
189500110309         //?Selezione per Importanza cliente
189600110309           WHEN  $Ord = 7;
189700110309             yy = 0;
189800110309             wSQL += 'WHERE CPOftr in (';
189900110309             IF V01cic1 <> *blanks;
190000110309               IF yy > 0;
190100110309                 wSQL += ', ';
190200110309               ELSE;
190300110309                 yy = 1;
190400110309               ENDIF;
190500110309               wSQL += '''';
190600110309               wSQL += V01cic1;
190700110309               wSQL += '''';
190800110309             ENDIF;
190900110309             IF V01cic2 <> *blanks;
191000110309               IF yy > 0;
191100110309                 wSQL += ', ';
191200110309               ELSE;
191300110309                 yy = 1;
191400110309               ENDIF;
191500110309               wSQL += '''';
191600110309               wSQL += V01cic2;
191700110309               wSQL += '''';
191800110309             ENDIF;
191900110309             IF V01cic3 <> *blanks;
192000110309               IF yy > 0;
192100110309                 wSQL += ', ';
192200110309               ELSE;
192300110309                 yy = 1;
192400110309               ENDIF;
192500110309               wSQL += '''';
192600110309               wSQL += V01cic3;
192700110309               wSQL += '''';
192800110309             ENDIF;
192900110309             IF V01cic4 <> *blanks;
193000110309               IF yy > 0;
193100110309                 wSQL += ', ';
193200110309               ELSE;
193300110309                 yy = 1;
193400110309               ENDIF;
193500110309               wSQL += '''';
193600110309               wSQL += V01cic4;
193700110309               wSQL += '''';
193800110309             ENDIF;
193900110309             IF V01cic5 <> *blanks;
194000110309               IF yy > 0;
194100110309                 wSQL += ', ';
194200110309               ELSE;
194300110309                 yy = 1;
194400110309               ENDIF;
194500110309               wSQL += '''';
194600110309               wSQL += V01cic5;
194700110309               wSQL += '''';
194800110309             ENDIF;
194900110309
195000110309           wSQL += ')';
195100110309
195200110309         //?Selezione per Filiale/Area
195300110309           WHEN  $Ord = 8 and V01flt > 0;
195400110309             wSQL += 'WHERE CPOflt = ';
195500110309             wSQL += %editc(V01flt:'X');
195600110309
195700110309           WHEN  $Ord = 8 and V01car <> *blanks;
195800110309             clear $Fil1;
195900110309             yy = 0;
196000110309             xx = 1;
196100110309             FOR xx by 1 to %elem($Car);
196200110309               IF %editc($Car(xx):'X') = V01car;
196300110309                 yy += 1;
196400110309                 $Fil1(yy) = $Fil(xx);
196500110309               ENDIF;
196600110309             ENDFOR;
196700110309             yy = 0;
196800110309             xx = 1;
196900110309             wSQL += 'WHERE CPOflt in(';
197000110309             FOR xx by 1 to %elem($Fil1);
197100110309               IF $Fil1(xx) <> *zeros;
197200110309                 IF yy > 0;
197300110309                   wSQL += ', ';
197400110309                 ELSE;
197500110309                   yy = 1;
197600110309                 ENDIF;
197700110309                 wSQL += %editc($Fil1(xx):'X');
197800110309               ENDIF;
197900110309             ENDFOR;
198000110309           wSQL += ')';
198100110309
198200110309         //?Selezione per Categoria potenziale
198300110309           WHEN  $Ord = 9;
198400110407             yy = 0;
198500110407             wSQL += 'WHERE CPOfls in (';
198600110407             IF V01fls1 <> *blanks;
198700110407               IF yy > 0;
198800110407                 wSQL += ', ';
198900110407               ELSE;
199000110407                 yy = 1;
199100110407               ENDIF;
199200110407               wSQL += '''';
199300110407               wSQL += V01fls1;
199400110407               wSQL += '''';
199500110407             ENDIF;
199600110407             IF V01fls2 <> *blanks;
199700110407               IF yy > 0;
199800110407                 wSQL += ', ';
199900110407               ELSE;
200000110407                 yy = 1;
200100110407               ENDIF;
200200110407               wSQL += '''';
200300110407               wSQL += V01fls2;
200400110407               wSQL += '''';
200500110407             ENDIF;
200600110407             IF V01fls3 <> *blanks;
200700110407               IF yy > 0;
200800110407                 wSQL += ', ';
200900110407               ELSE;
201000110407                 yy = 1;
201100110407               ENDIF;
201200110407               wSQL += '''';
201300110407               wSQL += V01fls3;
201400110407               wSQL += '''';
201500110407             ENDIF;
201600110407             IF V01fls4 <> *blanks;
201700110407               IF yy > 0;
201800110407                 wSQL += ', ';
201900110407               ELSE;
202000110407                 yy = 1;
202100110407               ENDIF;
202200110407               wSQL += '''';
202300110407               wSQL += V01fls4;
202400110407               wSQL += '''';
202500110407             ENDIF;
202600110407             IF V01fls5 <> *blanks;
202700110407               IF yy > 0;
202800110407                 wSQL += ', ';
202900110407               ELSE;
203000110407                 yy = 1;
203100110407               ENDIF;
203200110407               wSQL += '''';
203300110407               wSQL += V01fls5;
203400110407               wSQL += '''';
203500110407             ENDIF;
203600110407
203700110407           wSQL += ')';
203800110309
203900110309         ENDSL;
204000110309
204100110309         IF  V01osf = 'R';
204200110309           wSQL += ' order by CPOrag';
204300110309         ENDIF;
204400110309         IF  V01osf = 'I';
204500110601           //wSQL += ' order by CPOftr';
204600110601           wSQL += ' order by CPOord desc';
204700110309         ENDIF;
204800110309
204900110309         wSQL += ' for fetch only';
205000110309
205100110309         //?Dichiarazione cursore
205200110309         exec sql
205300110309           prepare S1 from :wSQL;
205400110309         exec sql
205500110309           declare CPO cursor for S1;
205600110309
205700110309         //?Apertura del cursore
205800110309         exec sql
205900110309           open CPO;
206000110309
206100110309         IF sqlcode < 0;
206200110309           $Eof = *on;
206300110309         ENDIF;
206400110309
206500110309       ENDSR;
206600080207
206700080207       //--------------------------------------------------------------
206800080207       //?Caricamento singola pagina S01
206900080207       //--------------------------------------------------------------
207000080207       BEGSR RollUpS01;
207100080207
207200080207         S01nrr    = (W_SflPag * C_SflPag);
207300080207         W_SflPag += 1;
207400080207         SflNxtChg = *off;
207500080207
207600080207         // Ciclo di caricamento dati nel sfl / lettura rec. successivo
207700080208         DOW  $EoF   = *off                   and
207800080208              S01nrr < (W_SflPag * C_SflPag)  and
207900080208              S01nrr < *hival;
208000080207
208100080207         // - Caricamento dati nel record del subfile
208200080213           exsr  RieS01;
208300080213           S01nrr += 1;
208400091027           write MK01S01;
208500080207
208600080207         // - Lettura record successivo nell'archivio
208700080213           exsr sr_ReadRec;
208800080207
208900080207         ENDDO;
209000080207
209100080207         // Visualizzazione del SFL (se ci sono dati)
209200080207         SflDsp_N = (S01nrr <= *zeros);
209300080207
209400080207         // Attivazione (eventuale) del SFLEND
209500080207         SflEnd   = ($EoF = *on);
209600080207
209700080207         // Posizionamento cursore al 1° rcd della pagina
209800080207         if  S01nrr > *zero;
209900080207           wPag   = S01nrr / C_SflPag;
210000080207           wRig   = S01nrr - (C_SflPag * wPag);
210100080207           C01rcd = wPag * C_SflPag;
210200080207           if  wRig > *zeros;
210300080207             C01rcd = C01rcd + 1;
210400080207           else;
210500080207             C01rcd = C01rcd - C_SflPag + 1;
210600080207           endif;
210700080207         else;
210800080207           clear  C01rcd;
210900080207         endif;
211000080207
211100080207         C01csr = C01rcd;
211200080207
211300080207       ENDSR;
211400080213
211500080213       //--------------------------------------------------------------
211600080213       //?Lettura record successivo in base all'ordinamento
211700080213       //--------------------------------------------------------------
211800080213       BEGSR sr_ReadRec;
211900080213
212000080213         $RecOK  = *off;
212100080213
212200080213         DOW  $EoF   = *off   and
212300080213              $RecOK = *off;
212400080213
212500080213           select;
212600080213
212700080213           // (se non rilevato stop record elaborabili in selezione)
212800080213             when  $EoF = *on;
212900080213
213000080213           // - per Ragione sociale
213100110520             when  $Ord = 2 and V01osf = 'R';
213200080213               read  TNCPO002;
213300080213               $EoF  = (%eof(TNCPO02L));
213400110309
213500110309           // - per Provincia in ordine di ragione sociale
213600110309             when  $Ord = 4 and V01osf = 'R';
213700110309               reade  V01prv  TNCPO004;
213800110309               $EoF  = (%eof(TNCPO04L));
213900110309
214000110309           // - per tutto il resto
214100110309             other;
214200110309               exec sql
214300110309               fetch next from CPO into :TNCPOds;
214400110309               IF sqlcod = 100 or sqlcod < 0;
214500110309                 $Eof = *on;
214600110309               ENDIF;
214700080213
214800080213           endsl;
214900080213
215000080213           // Selezione record in base alle parzializzazioni
215100080213           if  $EoF  = *off;
215200080213             exsr sr_SelRec;
215300080213           endif;
215400080213
215500080213         ENDDO;
215600080213
215700080213       ENDSR;
215800080207
215900080207       //--------------------------------------------------------------
216000080207       //?Selezione record in base alle parzializzazioni
216100080207       //--------------------------------------------------------------
216200080207       BEGSR sr_SelRec;
216300080207
216400080213         $RecOK  = *off;
216500120914         dcpo01=cporst   ;
216600080207
216700080207         // Parzializzazione per ragione sociale
216800110309         if  V01rag <> *blank;
216900110309           xx    = %checkr(' ' : V01rag);
217000110309           if   %subst(V01rag : 1 : xx) <> %subst(CPOrag : 1 : xx);
217100080211             if  $Ord = 2;
217200080211               $EoF = *on;
217300080211             endif;
217400080207             leavesr;
217500080207           endif;
217600080207         endif;
217700110309
217800110309         // Parzializzazione per mai contattati
217900110310         IF  V01pst <> *blanks;
218000120914           IF  (V01pst = 'N' and §CPOdtpcon <> *blanks and
218100120914                                 §CPOdtpcon <> *zeros) or
218200120914               (V01pst = 'S' and (§CPOdtpcon = *blanks or
218300120914                                  §CPOdtpcon = *zeros));
218400110314             leavesr;
218500110314           ENDIF;
218600110309         ENDIF;
218700080207
218800080207         // Altre parzializzazioni:
218900080207         select;
219000080207
219100080207           // Parzializzazione per provincia
219200110309           when  $Ord <> 4 and
219300110309                 V01prv <> *blank   and
219400110309                 V01prv <> CPOprv;
219500080207             leavesr;
219600080207
219700080207           // Parzializzazione per c.a.p.
219800110309           when  V01cap <> *blank   and
219900110309                 V01cap <> CPOcap;
220000080207             leavesr;
220100080207
220200080207           // Parzializzazione per località
220300110309           when  V01loc <> *blank   and
220400110309                 %subst( V01loc : 1 : %checkr(' ':V01loc) ) <>
220500110309                 %subst( CPOcit : 1 : %checkr(' ':V01loc) );
220600080207             leavesr;
220700080207
220800080207           // Parzializzazione per filiale di appartenenza
220900110309           when  ($Ord <> 8 or V01car > *zeros) and
221000110309                  V01flt <> *zero    and
221100110309                  V01flt <> CPOflt;
221200080207             leavesr;
221300080207
221400080207           // Parzializzazione per categoria merceologica
221500110309           when  $Ord <> 3 and
221600110615                 (V01sct1 <> *blank and V01sct1 <> *zeros and
221700110615                  %int(V01sct1) <> CPOsct) or
221800110615                 (V01sct2 <> *blank and V01sct2 <> *zeros and
221900110615                  %int(V01sct2) <> CPOsct) or
222000110615                 (V01sct3 <> *blank and V01sct3 <> *zeros and
222100110309                  %int(V01sct3) <> CPOsct);
222200080207             leavesr;
222300080207
222400080207           // Parzializzazione per area
222500110309           when  ($Ord <> 8 or V01flt > 0 ) and
222600110309                 V01car <> *blank   and
222700110309                 V01car <> *zero    and
222800110309                 V01car <> %editc( $Car( %lookup( CPOflt : $Fil ) ) : 'X');
222900080207             leavesr;
223000080207
223100080207           // Parzializzazione per partita iva
223200110309           when  $Ord <> 6 and
223300110309                 V01piv <> *blank   and
223400110309                 V01piv <> CPOpiv;
223500080207             leavesr;
223600080207
223700080207           // Parzializzazione per codice fiscale
223800110309           when  $Ord <> 5 and
223900110309                 V01cdf <> *blank   and
224000110309                 V01cdf <> CPOcdf;
224100080207             leavesr;
224200110309
224300110309           // Parzializzazione Categoria potenziale
224400110407           when  $ord <> 9 and V01fls <> *blanks and
224500110407                 CPOfls <> *blanks and %lookup(CPOfls:$fls) = 0;
224600110309             leavesr;
224700110309
224800110309           // Parzializzazione Importanza cliente
224900110322           when  $Ord <> 7 and V01cic <> *blanks and
225000110322                 CPOftr = *blanks;
225100110309             leavesr;
225200110322           when  $Ord <> 7 and V01cic <> *blanks and
225300110322                 CPOftr <> *blanks and %lookup(CPOftr:$cic) = 0;
225400110322             leavesr;
225500080207
225600080207         endsl;
225700110310
225800110310       //?Parzializzazioni INFO commerciali
225900110310       //?Spesa trasporto estero totale
226000110310         IF  V01treda > 0;
226100110310           wval = 0;
226200110310           wpft = 0;
226300110310           chain (CPOcpo:'AER') TICPI01L;
226400110310           IF  %found(TICPI01L);
226500110310             wval += CPIval;
226600110310           ENDIF;
226700110310           chain (CPOcpo:'CAE') TICPI01L;
226800110310           IF  %found(TICPI01L);
226900110310             wval += CPIval;
227000110310           ENDIF;
227100110310         //?Non trovo dati relativi all'estero escludo il potenziale
227200110310           IF  wval = 0;
227300110310             leavesr;
227400110310           ENDIF;
227500110310           chain (CPOcpo:'SPT') TICPI01L;
227600110310         //?Non trovo dati relativi alla spesa totale escludo il potenziale
227700110310           IF  not %found(TICPI01L);
227800110310             leavesr;
227900110310           ENDIF;
228000110310           wpft = (CPIpft * wval) / 100;
228100110310           IF  wpft < V01treda or wpft > V01trea;
228200110310             leavesr;
228300110310           ENDIF;
228400110310         ENDIF;
228500110310
228600110310       //?Peso medio a spedizione
228700110310         IF  V01pkgda > 0;
228800110310           chain (CPOcpo:'KMS') TICPI01L;
228900110310         //?Non trovo dati relativi al peso escludo il potenziale
229000110310           IF  not %found(TICPI01L);
229100110310             leavesr;
229200110310           ENDIF;
229300110310           IF  CPIval < V01pkgda or CPIval > V01pkga;
229400110310             leavesr;
229500110310           ENDIF;
229600110310         ENDIF;
229700110310
229800110310       //?Spedizioni 10:30-PRIORITY
229900110310         IF  V01she <> *blanks;
230000110310           chain (CPOcpo:'HPR') TICPI01L;
230100110310         //?Non trovo dati relativi al tipo spedizione escludo il potenziale
230200110310           IF  not %found(TICPI01L);
230300110310             leavesr;
230400110310           ENDIF;
230500110310           IF  CPIval = 0;
230600110310             leavesr;
230700110310           ENDIF;
230800110310         ENDIF;
230900110601
231000110601       //?Spedizioni SECURE BOX
231100110601         IF  V01sbx <> *blanks;
231200110601           chain (CPOcpo:'SBX') TICPI01L;
231300110601         //?Non trovo dati relativi al tipo spedizione escludo il potenziale
231400110601           IF  not %found(TICPI01L);
231500110601             leavesr;
231600110601           ENDIF;
231700110601           IF  CPIfsn <> 'S';
231800110601             leavesr;
231900110601           ENDIF;
232000110601         ENDIF;
232100110310
232200110310       //?Concorrenti
232300110310         IF  V01conc <> *blanks;
232400110310           $Conc = *off;
232500110310           IF  V01conc1 <> *blanks;
232600110310             chain (CPOcpo:'10':V01conc1) TICPI01L;
232700110310             IF  %found(TICPI01L) and CPIval > 0;
232800110310               $Conc = *on;
232900110310             ENDIF;
233000110310           ENDIF;
233100110310           IF  V01conc2 <> *blanks;
233200110310             chain (CPOcpo:'10':V01conc2) TICPI01L;
233300110310             IF  %found(TICPI01L) and CPIval > 0;
233400110310               $Conc = *on;
233500110310             ENDIF;
233600110310           ENDIF;
233700110310           IF  V01conc3 <> *blanks;
233800110310             chain (CPOcpo:'10':V01conc3) TICPI01L;
233900110310             IF  %found(TICPI01L) and CPIval > 0;
234000110310               $Conc = *on;
234100110310             ENDIF;
234200110310           ENDIF;
234300110310           IF  not $Conc;
234400110310             leavesr;
234500110310           ENDIF;
234600110310         ENDIF;
234700110601
234800110601       //?Controllo trattative in corso
234900110601         $EndTtr = *off;
235000110601         IF  V01ttr <> *blanks;
235100110601           wTtr = 0;
235200110601           setll (CPOcpo) TIVIS01L;
235300110601           DOW  not $EndTtr;
235400110601             reade (CPOcpo) TIVIS01L;
235500110601             IF  %eof(TIVIS01L);
235600110601               $EndTtr = *on;
235700110601               leave;
235800110601             ENDIF;
235900110606             IF  VISffz = *blanks and VISdch = 0;
236000110606               wTtr += 1;
236100110606               $EndTtr = *on;
236200110606               leave;
236300110606             ENDIF;
236400110601           ENDDO;
236500110606           IF (V01ttr = 'S' and wTtr = 0) or
236600110606              (V01ttr = 'N' and wTtr > 0);
236700110601             leavesr;
236800110601           ENDIF;
236900110601         ENDIF;
237000080207
237100131125       //?Logistica  INTERESSATI S/N
237200131125         IF  V02int <> *blank ;
237300131125
237400131125           chain (CPOcpo:'LOG') TICPI01L;
237500131125           IF  not %found(TICPI01L);
237600131125                  leavesr ;
237700131125           ENDIF ;
237800131125           IF  %found(TICPI01L);
237900131125               If CPIfsn <> V02int ;
238000131125                  leavesr ;
238100131125               ENDIF ;
238200131125       //?verifico le data di inserimento delle INFO
238300131125               IF (Wdil1 > 0 or Wdil2 > 0) and
238400131125                  (CPIdim < Wdil1 or Cpidim > Wdil2) ;
238500131125                  leavesr ;
238600131125               ENDIF ;
238700131125           ENDIF;
238800131125
238900131125         ENDIF;
239000131125
239100131125       //?Logistica  OUTSOURCING INTERESSATI S/N
239200131125         IF  V02los <> *blank ;
239300131125
239400131125           chain (CPOcpo:'OUT') TICPI01L;
239500131125           IF  not %found(TICPI01L);
239600131125                  leavesr ;
239700131125           ENDIF ;
239800131125           IF  %found(TICPI01L);
239900131125               If CPIfsn <> V02los ;
240000131125                  leavesr ;
240100131125               ENDIF ;
240200131125           ENDIF;
240300131125
240400131125         ENDIF;
240500131125
240600131125       //?Concorrenti  LOGISTICA
240700131125         IF  V02conl <> *blanks;
240800131125           $Conclog = *off;
240900131125           IF  V02concl1 <> *blanks;
241000131125             chain (CPOcpo:'40':V02concl1) TICPI01L;
241100131125             IF  %found(TICPI01L) ;
241200131125               $Conclog = *on;
241300131125             ENDIF;
241400131125           ENDIF;
241500131125           IF  V02concl2 <> *blanks;
241600131125             chain (CPOcpo:'40':V02concl2) TICPI01L;
241700131125             IF  %found(TICPI01L) ;
241800131125               $Conclog = *on;
241900131125             ENDIF;
242000131125           ENDIF;
242100131125           IF  V02concl3 <> *blanks;
242200131125             chain (CPOcpo:'40':V02concl3) TICPI01L;
242300131125             IF  %found(TICPI01L) and CPIval > 0;
242400131125               $Conclog = *on;
242500131125             ENDIF;
242600131125           ENDIF;
242700131125           IF  not $Conclog ;
242800131125             leavesr;
242900131125           ENDIF;
243000131125         ENDIF;
243100131125
243200131125                  $RecOK  = *on;
243300131125
243400080207       ENDSR;
243500110601
243600080207       //--------------------------------------------------------------
243700080207       //?Caricamento singolo record nel SubFile S01
243800080207       //--------------------------------------------------------------
243900080208       BEGSR RieS01;
244000080207
244100091027         clear  MK01S01;
244200080211
244300080211         // Decodifica categoria merceologica
244400080211         clear  ds1L;
244500080211         if  CPOsct <> *zero;
244600080211           TBLcod = '1L';
244700080211           TBLkey = %editc(CPOsct : 'X');
244800080211           chain (TBLkut : TBLcod : TBLkey) TABEL;
244900080211           if  %found(TABEL00F)   and
245000080211               TBLflg = *blank;
245100080211             ds1L    = TBLuni;
245200080211           endif;
245300080211         endif;
245400080207
245500080214         // Parametri di visualizzazione (default)
245600080214         P01da1 = C_dspatr_Norm;
245700080214         P01da2 = C_dspatr_Norm;
245800080214
245900080207         // Campi hidden
246000080208         H01sct  = CPOsct;
246100081020         dcpo01=cporst   ;
246200081020         h01ifotot=§cpoifotot   ;
246300080208
246400080207         // Campi di solo output
246500081017         if cpofsf ='S'      ;
246600081017         s01sfs  ='Sd'   ;
246700081017         else  ;
246800081017         s01sfs  ='Fi'   ;
246900081017         endif   ;
247000081017
247100080207         S01rag  = %subst(CPOrag : 1 : %len(S01rag));
247200081021         P01da1 = C_dspatr_HI  ;
247300080207         S01loc  = %subst(CPOcit : 1 : %len(S01loc));
247400080208         S01prv  = CPOprv;
247500080208         S01flt  = CPOflt;
247600080211
247700080211         S01cpo  = CPOcpo;
247800080211         S01cap  = CPOcap;
247900080211         S01sctD = %subst(§1Ldes : 1 : %len(S01sctD));
248000080211         if  CPOatb <> *blank;
248100080214           P01da1 = C_dspatr_BL;
248200080214           P01da2 = C_dspatr_RI;
248300080211           S01ann = 'A';
248400080211         endif;
248500110309
248600110309        //?memorizzo importanza cliente
248700110309         S01cic = CPOftr;
248800110209
248900110209        //?memorizzo categoria potenziale
249000110309         S01fls = CPOfls;
249100110309
249200110309        //?Cerco prox attività da fare
249300110523         $EndAtt = *off;
249400110523         exec sql
249500110523         declare ATT cursor for select ATCcad from TIATC00F
249600110523         where ATCcpo = :CPOcpo and atcdco = 0
249700110523         order by ATCdad, ATChda;
249800110523         exec sql
249900110523         open ATT;
250000110523         IF sqlcode < 0;
250100110523           $EndAtt = *on;
250200110523         ENDIF;
250300110523         DOW not $EndAtt;
250400110523           exec sql
250500110523           fetch next from ATT into :ATCcad;
250600110523           IF sqlcod = 100 or sqlcod < 0;
250700110523             $EndAtt = *on;
250800110523             leave;
250900110523           ENDIF;
251000110523           S01cad = ATCcad;
251100110523           leave;
251200110523         ENDDO;
251300110523         exec sql close ATT;
251400110309
251500110309        //?Cerco se ci sono trattative aperte
251600110309         $EndTtr = *off;
251700110601         setll (CPOcpo:0) TIVIS01L;
251800110601         DOW  not $EndTtr;
251900110601           reade (CPOcpo:0) TIVIS01L;
252000110601           IF  %eof(TIVIS01L);
252100110601             $EndTtr = *on;
252200110601             leave;
252300110601           ENDIF;
252400110601           IF  VISffz <> *blanks;
252500110601             iter;
252600110601           ENDIF;
252700110601           S01ttr = 'S';
252800110601           $EndTtr = *on;
252900110601         ENDDO;
253000080207
253100080207       ENDSR;
253200080207
253300080207       //--------------------------------------------------------------
253400080207       //?Gestione tasto funzionale F12 da videata S01
253500080207       //?F12=Ritorno
253600080207       //--------------------------------------------------------------
253700080207       BEGSR F12S01;
253800080207
253900080207         // Ritorno alla videata D01
254000080207         $Video = 'D1';
254100110309
254200110309         // Chiudo il cursore
254300110309         exec sql
254400110309           close CPO;
254500110309
254600080207       ENDSR;
254700080207
254800080626       //--------------------------------------------------------------
254900080626       //?Gestione tasto funzionale F14 da videata S01
255000080626       //?F14=riconciliazione clienti potenziali
255100080626       //--------------------------------------------------------------
255200080626       BEGSR F14S01;
255300080626       savkpjbu=kpjbu  ;
255400080626       kpjbu='N'       ;
255500080626       TRMK07R (kpjba) ;
255600080626       kpjbu=savkpjbu  ;
255700080626
255800080626       ENDSR;
255900080207       //--------------------------------------------------------------
256000080207       //?Gestione opzioni subfile
256100080207       //--------------------------------------------------------------
256200080207       BEGSR OpzS01;
256300080207
256400091027         readc MK01S01;
256500080207
256600091027         DOW  NOT  %eof(TRMK01D);
256700080207
256800080207           SflNxtChg = *off;
256900080207           WindDspF  = IndDspF;
257000080207           reset WindDspFb;
257100080207           IndDspF   = WindDspF;
257200080207           C01rcd    = S01nrr;
257300080207
257400080207           SELECT;
257500080207
257600080207             // - Nessuna opzione
257700080208             WHEN  S01opz  = *blank;
257800080208
257900080208             // - Precedente segnalazione di errore
258000080208             WHEN  S01opz  = 'E';
258100080208               clear S01opz;
258200080207
258300080207             // - 1 = Selezione
258400080208             WHEN  S01opz  = '1';
258500091028               // Se pgm richiamato con Mk01k11='2' o '4'
258600080207               // => imposta ds e poi esce
258700091028               if  Mk01k11 = '2'or Mk01k11='4';
258800080207                 exsr Return_Dati;
258900080207                 eval $Fine = *on;
259000080207                 leavesr;
259100080207               endif;
259200110209               //?Se richiamato da gestione attività
259300110209               IF w_daAtti = 'S';
259400110209               //?Controllo se potenziale codificato, in questo caso no attività
259500110601               //?per ora non lo facciamo
259600110309                 IF  S01fls = 'C';
259700110518                  // errMessage  = *on;
259800110518                  // errGenerico = *on;
259900110518                  // PosCurOpz   = *on;
260000110518                  // V1Dmsg = 'Scelta non possibile potenziale CODIFICATO';
260100110209                 ENDIF;
260200110209               ENDIF;
260300081020
260400081020               if ErrGenerico=*off   ;
260500080207               exsr Call_TRMK10;
260600081020               endif                 ;
260700080207
260800081020             // - G/S = gestione o stampa info comm
260900081020             WHEN  S01opz  = 'G' or s01opz='S';
261000081020             // Solo se non richiamato
261100081020 1           if PGMrichia=*on  ;
261200081020               errMessage  = *on;
261300081020               errGenerico = *on;
261400081020               PosCurOpz   = *on;
261500081020               V1Dmsg = $Msg(11);
261600081020 x1          else ;
261700110601               IF  %lookup(%editc(S01flt:'X'):pog) = 0;
261800110601                 errMessage  = *on;
261900110601                 errGenerico = *on;
262000110601                 PosCurOpz   = *on;
262100110601                 V1Dmsg = $Msg(15);
262200110601               ENDIF;
262300110601
262400081020 2             if ErrGenerico=*off   ;
262500081020                    clear trmk50ds    ;
262600081020                    i50cpo=s01cpo     ;
262700081020 4                  if Gesclipot=*on  ;
262800081020                      i50mod='G'        ;
262900081020                      else              ;
263000081020                      i50mod='I'        ;
263100081020 4                  endif             ;
263200081020                    i50rag=s01rag     ;
263300091027                    i50pgm='TRMK01R'  ;
263400081020                    i50ifotot=h01ifotot;
263500081022 3                if s01opz='G'     ;
263600081020                    callp TRMK50R (kpjba:trmk50ds)  ;
263700081020 3                endif     ;
263800081022
263900081022 3                if s01opz='S'     ;
264000081022                  kpjbu=trmk50ds    ;
264100081022                    callp TRMK51R (kpjba)           ;
264200081022 3                endif     ;
264300081020 2             endif     ;
264400081020 1             endif     ;
264500081020
264600080207             // - ? = Opzione NON valida
264700080207             OTHER;
264800080207               errMessage  = *on;
264900080207               errGenerico = *on;
265000080207               PosCurOpz   = *on;
265100080208               V1Dmsg = $Msg(11);
265200080207
265300080207           ENDSL;
265400080207
265500080207           // Aggiornamento sfl
265600080207           select;
265700080207             when  errMessage  = *on;
265800080207               SflNxtChg = *on;
265900080207               C01csr    = C01rcd;
266000080207             when  errGenerico = *on;
266100080208               C01csr    = C01rcd;
266200080208               clear  S01opz;
266300080207             other;
266400080207               C01csr    = C01rcd;
266500080208               clear  S01opz;
266600080207           endsl;
266700080207
266800091027           update MK01S01;
266900080207
267000080208           if  errMessage = *on  or  errGenerico = *on;
267100080207             leave;
267200080207           endif;
267300080207
267400091027           readc MK01S01;
267500080207
267600080207         ENDDO;
267700080207
267800080207       ENDSR;
267900080207
268000080207       //--------------------------------------------------------------
268100080207       //?Chiamata pgm "Gestione Clienti Potenziali" in interrogazione
268200080207       //--------------------------------------------------------------
268300080206       BEGSR Call_TRMK10;
268400080207
268500080207           if GESclipot = *off;
268600091028             Mk01k10 = 'N';
268700080207           else;
268800091028             Mk01k10 = 'R';
268900080207           endif;
269000080207
269100080207         exsr Return_Dati;
269200080207
269300100202         kpjbu  = w_damenu;
269400131126         %subst(kpjbu : 2: 3) = $Logistica;
269500100408
269600100408        // se vengo dal programma di gestione attività passo anche la ds del TRMK19
269700100408         If W_daatti <> 'S' ;
269800100408            trmk02r ( kpjba : trmk01ds );
269900100408         else ;
270000100408            trmk02r ( kpjba : trmk01ds : trmk19ds );
270100100408         endif ;
270200080207
270300080207         SELECT;
270400080207
270500100311         // Se questo pgm. risulta richiamato => esce se fatta una scelta
270600100311           WHEN  Mk01k11 = '1' and MK01f06 <> *blanks;
270700100311              Mk01ent = *on;
270800100311              $Fine    = *on;
270900100311              errGenerico = *on;
271000100311              leavesr;
271100080207
271200091028         // Se premuto F3 da TRMK02R => esce
271300091028           WHEN  Mk01f03 = *on;
271400080207             $Fine       = *on;
271500080207             errGenerico = *on;
271600080207             leavesr;
271700080207
271800091028         // Se errore restituito da TRMK02R => esce
271900091028           WHEN  Mk01err = 'E';
272000091028             V1Dmsg   = Mk01m10;
272100080207             errMessage  = *on;
272200080207             errGenerico = *on;
272300080207             if  $Video  = 'WW';
272400080207               $Video    = 'D1';
272500080207               PosCurCP1 = *on;
272600080207             else;
272700080207               PosCurOpz   = *on;
272800080207               S01opz      = 'E';
272900080207             endif;
273000080207
273100080207         // Altrimenti: se specificato un solo potenziale
273200080207         // => si torna alla 1ª videata (di selezione)
273300080207           OTHER;
273400080207             if  $Video  = 'WW';
273500080207               $Video = 'D1';
273600080207             endif;
273700080207
273800080207         ENDSL;
273900080207
274000080206       ENDSR;
274100080207
274200080207       //--------------------------------------------------------------
274300080207       //?Impostazione campi di output nella struttura dati
274400080207       //--------------------------------------------------------------
274500080207       BEGSR Return_Dati;
274600080207
274700080213         if  $Video = 'D1'   or
274800080213             $Video = 'WW';
274900091028           Mk01cpo = CPOcpo;
275000091028           Mk01rag = CPOrag;
275100091028           Mk01prv = CPOprv;
275200110308           Mk01pst = V01pst;
275300091028           Mk01flt = CPOflt;
275400091028           Mk01cap = CPOcap;
275500091028           Mk01cit = CPOcit;
275600091028           Mk01sct = %editc( CPOsct : 'X' );
275700080208         else;
275800091028           Mk01cpo = S01cpo;
275900091028           Mk01rag = S01rag;
276000091028           Mk01Prv = S01prv;
276100110308           Mk01pst = V01pst;
276200091028           Mk01flt = S01flt;
276300091028           Mk01cap = S01cap;
276400091028           Mk01cit = S01loc;
276500091028           Mk01sct = %editc( H01sct : 'X' );
276600080208         endif;
276700080207
276800080207       ENDSR;
276900080206
277000080206       //--------------------------------------------------------------
277100080206       //?Operazioni finali.
277200080206       //--------------------------------------------------------------
277300080206       BEGSR RoutEnd;
277400090702
277500090702         //
277600090702         clear tntaa1ds;
277700090702         itaa1TLA='C' ;
277800090702         clear kpjbu   ;
277900090702         kpjbu=tntaa1ds ;
278000090703         TNTAA1C     (kpjba);
278100080206
278200091027         kpjbu = TRMK01ds;
278300080206         *inLR = *on;
278400080206         return;
278500080206
278600080206       ENDSR;
278700080206
278800080206      /end-free
278900080206
279000080206       //--------------------------------------------------------------
279100080206       //?Schiere a tempo di compilazione.
279200080206       //--------------------------------------------------------------
279300080206
279400080206** - $MSG -------------------------------------------------------------------*
279500110308Utente non abilitato alla Funzione.                                           01
279600110518Inserire o Cod.Potenziale o Cliente Unificante no tutti e 2 assieme.          02
279700110308Se parzializzazione per cap o località  immettere la provincia                03
279800110308Se parzializzazione per Info Commerciale immettere almeno 1 dato anagrafico.  04
279900110308Categoria merceologica errata.                                                05
280000110308Codice area errato                                                            06
280100110308Ordinamento errato                                                            07
280200110308Potenziale inesistente                                                        08
280300110308Non è stato trovato nessun potenziale                                         09
280400110308Raggiunta l'ampiezza massima del subfile                                      10
280500110308Opzione errata                                                                11
280600110601Se richiesto 'Trattative in corso' immettere almeno 1 dato anagrafico.        12
280700110601........................Libero                                                13
280800110601........................Libero                                                14
280900110601Utente non abilitato al potenziale.                                           15
281000110308Filiale di appartenenza errata.                                               16
281100110309Importanza cliente errata.                                                    17
281200110309Ordinamento non valido se immessa ragione sociale.                            18
281300110310Immettere la Spesa in trasporti ESTERO                                        19
281400110310Spesa Trasporti Estero "DA" incongruente con Spesa Trasporti Estero "A"       20
281500110310Immettere Peso medio a Spedizione                                             21
281600110310Peso Medio Spedizione "DA" incongruente con Peso Medio Spedizione "A"         22
281700110310Concorrente errato                                                            23
281800110314Categoria Potenziale errata.                                                  24
281900131122Se non richiesti POTENZIALI interessati a LOGISTICA non immettere le date     25
282000131122Data errata. Inserire almeno un periodo                                       26
282100131122Data errata                                                                   27
282200131122Data "DAL" incongruente con data "AL"                                         28
282300131125Indicare se Logistica in Outsourcing                                          29
282400131125Immettere i Concorrenti SOLO se selezionata Logistica in outsourcing          30
282500131125Concorrente Logistica errato                                                  31
