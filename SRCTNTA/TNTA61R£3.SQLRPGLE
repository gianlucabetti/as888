000100080206      //--------------------------------------------------------------
000200110615      //?TNTA61R - INTERROGAZIONE ABILITAZIONI CLIENTI
000300080206      //--------------------------------------------------------------
000400090407     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000500090601     h dftactgrp(*no) actgrp(*caller)
000600080206
000700080206      //---------------------------------------------------------------
000800080206      //?Dichiarazione file.
000900080206      //---------------------------------------------------------------
001000110802      // - File Anagrafiche
001100110803     fCNIND00F  if   e           k disk
001200110802     fCNCLP00F  if   e           k disk
001300151116     fFNCLS01L  if   e           k disk
001400110803
001500110803      // - File Conto Economico
001600110803     fECEVD02L  if   e           k disk
001700140428      // - Contratti Comodato
001800140428     fUNCMD01l  if   e           k disk
001900160125      // - Bolle in sospensione
002000160125     fFIBSP02L  if   e           k disk
002100100624
002200110615      // - File Tabelle
002300110801     fTABEL00F  if   e           k disk    usropn
002400110808     f                                     extfile(wLibTab)
002500110801     fTNTBE01L  if   e           k disk    usropn
002600110808     f                                     extfile(wLibTbe)
002700110411
002800110615      // - Video
002900110615     fTNTA61D   cf   e             workstn
003000110615     f                                     sfile(TA61S01 : S01nrr)
003100080208     f                                     indds(IndDspF)
003200080206     f                                     infds(InfDspF)
003300111027
003400111027      // - Stampa
003500111027     fTNTA61P   o    e             printer oflind(*in90) usropn
003600080206
003700080206      //---------------------------------------------------------------
003800090406      //?Definizione costanti.
003900080206      //---------------------------------------------------------------
004000080207      // - Tasti funzionali a video
004100080207     d c_F01           c                   const(x'31')
004200080207     d c_F02           c                   const(x'32')
004300080207     d c_F03           c                   const(x'33')
004400090406     d c_F04           c                   const(x'34')
004500080207     d c_F05           c                   const(x'35')
004600080207     d c_F06           c                   const(x'36')
004700080207     d c_F07           c                   const(x'37')
004800080207     d c_F08           c                   const(x'38')
004900080207     d c_F09           c                   const(x'39')
005000080207     d c_F10           c                   const(x'3A')
005100090303     d c_F11           c                   const(x'3B')
005200080207     d c_F12           c                   const(x'3C')
005300080207     d c_F13           c                   const(x'B1')
005400080207     d c_F14           c                   const(x'B2')
005500080207     d c_F15           c                   const(x'B3')
005600080207     d c_F16           c                   const(x'B4')
005700080207     d c_F17           c                   const(x'B5')
005800080207     d c_F18           c                   const(x'B6')
005900080207     d c_F19           c                   const(x'B7')
006000080207     d c_F20           c                   const(x'B8')
006100080207     d c_F21           c                   const(x'B9')
006200080207     d c_F22           c                   const(x'BA')
006300080207     d c_F23           c                   const(x'BB')
006400080207     d c_F24           c                   const(x'BC')
006500080207     d c_Enter         c                   const(x'F1')
006600080207     d c_RollDown      c                   const(x'F4')
006700080207     d c_RollUp        c                   const(x'F5')
006800100211
006900100211     d Digits          c                   const('0123456789')
007000080206
007100080206      //---------------------------------------------------------------
007200080206      //?Definizione schiere.
007300080206      //---------------------------------------------------------------
007400110615      // - Sk per Tabella ABC
007500110615     d $Abc            s             15    dim(999)
007600111028     d $Aut            s              1    dim(999)
007700110615     d $Des            s             60    dim(999)
007800110615     d $File           s             10    dim(999)
007900111107     d $Files          s              1    dim(999)
008000111107     d $Ipers          s              1    dim(999)
008100160609     d $IntW           s              1    dim(999)
008200110615     d $Ord            s              3s 0 dim(999)
008300110615     d $Ordw           s              3s 0 dim(999)
008400110615     d $Pgm            s             10    dim(999)
008500111107     d $Rpers          s              1    dim(999)
008600111107
008700111107      // - Sk per Tabella 7R
008800111107     d $7R             s              2    dim(999)
008900111107     d $7Raut          s              1    dim(999)
009000111107
009100111107      // - Sk per Tabella 7W
009200111107     d $7W             s              2    dim(999)
009300111107     d $7Waut          s              1    dim(999)
009400120227
009500120227       // -?Schiere per gestione codici cliente di ritorno da XCLIR
009600120227     d ksa             s              4    dim(30)
009700120227     d ksc             s              7  0 dim(30)
009800110805
009900080206      // - Messaggi di errore
010000100408     d $Msg            s             78    dim(20) ctdata perrcd(1)
010100080206
010200080206      //---------------------------------------------------------------
010300080206      //?Definizione aree dati.
010400080206      //---------------------------------------------------------------
010500080206      // - Dati utente
010600080206     d §AzUte        e ds                  extname(AZUTE00F)
010700080206     d                                     dtaara
010800080206     d §DatiUte      e ds                  extname(dDatiUte)
010900080206     d                                     dtaara
011000080206
011100080206      //---------------------------------------------------------------
011200080206      //?Definizione strutture dati.
011300080206      //---------------------------------------------------------------
011400080206      // - Status
011500080206     d Psds           sds
011600080206     d   SDSpgm          *proc
011700080206
011800080206      // - InfDS
011900080206     d InfDspF         ds
012000080207     d  dsp_aid              369    369a
012100080207     d  sfl_rrn              376    377i 0
012200080207     d  min_nrr              378    379i 0
012300080207     d  num_rcds             380    381i 0
012400080206
012500080206      // - Indicatori su DspF
012600080208     d IndDspF         ds
012700110722        // - Indicatori di attivazione tasti funzione
012800110722     d  F03Attivo                     1n   overlay(IndDspF : 03)
012900110722     d  F12Attivo                     1n   overlay(IndDspF : 12)
013000100720        // - Indicatori di errore in videata
013100100720     d  ErrMessage                    1n   overlay(IndDspF : 28)
013200080206        // - Indicatori di gestione del subfile
013300080206     d  SflDsp_N                      1n   overlay(IndDspF : 30)
013400080208     d  SflDspCtl_N                   1n   overlay(IndDspF : 31)
013500080206     d  SflNxtChg                     1n   overlay(IndDspF : 32)
013600080206     d  SflEnd                        1n   overlay(IndDspF : 33)
013700110722        // - Indicatore di protezione campi
013800110722     d  Proteggi                       n   overlay(IndDspF : 40)
013900100624        // - Indicatori di errore
014000100624     d  PosCurOpz                     1n   overlay(IndDspF : 50)
014100110615     d  PosCurKsc                     1n   overlay(IndDspF : 51)
014200090422        // - Indicatori di errore generico
014300100719     d  ErrGenerico                   1n   overlay(IndDspF : 99)
014400090407
014500090407     d WindDspF        ds                  inz
014600090407     d   WindDspFa             1     49    inz(*zero)
014700090407     d   WindDspFb            50     99    inz(*zero)
014800120227
014900120227       // -?ds x ricerca clienti
015000120227     d                 ds
015100120227     d dsksn                   1      4p 0
015200120227     d dsksa                   1      4
015300120621
015400120621       // -?ds x tabella FLB
015500120621     d wFLBuni         ds
015600120621     d  skksc                  1    252    dim(36)
015700080206
015800080206      // - Parametri ricevuti
015900080206     d KPJBA         e ds
016000110615     d TNTA61DS      e ds
016100130729
016200130729       // -?ds x pgm FNLV55R
016300130729     d FNLV55DS      e ds                  inz
016400111028
016500111028      // - Ricerca/Controllo tabelle
016600111028     d TIBS02ds      e ds                  inz
016700080206
016800080206      // - Reperimento dati utente
016900080206     d TIBS34ds      e ds
017000110615
017100110615      // - Controllo abilitazioni
017200110615     d TNTAA1DS      e ds                  inz
017300120227
017400120227       // -?Ricerca clienti
017500120227     d xCLIrds       e ds
017600110616
017700110616      // - Tabella ABC
017800110616     d dABC          e ds                  inz
017900110809
018000110809      // - Tabella CLI
018100110809     d dCLI          e ds                  inz
018200110803
018300110803      // - Tabella DCC
018400110803     d dDCC          e ds                  inz
018500110913
018600120604      // - Tabella JDC
018700120604     d dJDC          e ds                  inz
018800120604
018900110913      // - Tabella LAC
019000110913     d dLAC          e ds                  inz
019100130906
019200130906      // - Tabella LCA
019300130906     d dLCA          e ds                  inz
019400110805
019500110805      // - Tabella LLT
019600110805     d dLLT          e ds                  inz
019700110809
019800110809      // - Tabella 3C
019900110809     d ds3C          e ds                  inz
020000110809
020100110809      // - Tabella 3K
020200110809     d ds3K          e ds                  inz
020300110617
020400110809      // - Tabella 3Q
020500110809     d ds3Q          e ds                  inz
020600110809
020700110809      // - Tabella 3R
020800110809     d ds3R          e ds                  inz
020900110809
021000110809      // - Tabella 4C
021100110809     d ds4C          e ds                  inz
021200110808
021300110808      // - Tabella 7x --> Particolarità P-Q-R-S
021400110808     d ds7PQRS       e ds                  inz
021500111107
021600111107      // - Tabella 7R
021700111107     d ds7R          e ds                  inz
021800110805
021900110805      // - Tabella 7U
022000110805     d ds7U          e ds                  inz
022100110808     d  $Part                  1     60    dim(30)
022200111107
022300111107      // - Tabella 7W
022400111107     d ds7W          e ds                  inz
022500110809
022600110809      // - File TIVIR00F
022700110809     d TIVIR00F      e ds
022800110809
022900110809      // - File TIVTF00F
023000110809     d TIVTF00F      e ds
023100160304
023200160304      // - File ANRCO00F
023300160304     d ANRCO00F      e ds
023400160609
023500160609      // - File Note Clienti
023600160609     dTFNTC00F       e ds
023700100428
023800080206      //---------------------------------------------------------------
023900080206      //?Definizione variabili globali.
024000080206      //---------------------------------------------------------------
024100080206      // - Flags booleani
024200120227     d wDaMenu         s               n   inz(*off)
024300120227     d wErrGrave       s               n   inz(*off)
024400130729     d $Bsp            s               n   inz(*off)
024500110927     d $Cli            s               n   inz(*off)
024600110615     d $Controlla      s               n   inz(*off)
024700110809     d $End            s               n   inz(*off)
024800100720     d $Fine           s               n   inz(*off)
024900110615     d $InzD01         s               n   inz(*on)
025000100720     d $InzS01         s               n   inz(*on)
025100160609     d $InzW01         s               n   inz(*on)
025200110808     d $Prova          s               n   inz(*off)
025300080208     d $RecOK          s               n   inz(*off)
025400110615     d $Richiamato     s               n   inz(*off)
025500110808     d $Sede           s               n   inz(*off)
025600110907     d $SedeUte        s               n   inz(*off)
025700111027     d $Stampa         s               n   inz(*off)
025800111027     d $Testa          s               n   inz(*off)
025900080206
026000080206      // - Indici di schiera
026100111107     d ww              s              4  0 inz
026200100312     d xx              s              4  0 inz
026300110615     d yy              s              4  0 inz
026400110808     d zz              s              4  0 inz
026500080206
026600080207      // - Campi associati al video
026700100719     d $Video          s              2    inz('S1')
026800080208     d S01nrr          s              4  0 inz
026900110801
027000110801      // - Campi per richiamo QCMDEXC
027100110801     d Qcmd            s            128    inz
027200110802
027300110802      // - Stringa SQL da eseguire
027400110802     d wSQL            s           2048    Varying        inz
027500110808
027600110808      // - Nome libreria file tabella
027700110808     d wLibTab         s             21a   inz
027800110808     d wLibTbe         s             21a   inz
027900110809     d wLibVir         s             21a   inz
028000110809     d wLibVtf         s             21a   inz
028100090401
028200080206      // - Campi di comodo
028300110616     d C_ke1           s                   like(TBEke1)
028400130729     d C_ke2           s                   like(TBEke2)
028500110616     d C_tab           s                   like(TBEcod)
028600160609     d KeyNtc          s                   like(NTCnk1)
028700110802     d ParmPgm         s             10
028800111107     d sav_AUT         s                   like(d§ABCaut)
028900121217     d sav_s01nrr      s                   like(s01nrr)
029000150513     d wCompCE         s              5s 3 inz
029100110809     d wOggi           s              8s 0 inz
029200110809     d wKscAlfa        s              7a   inz
029300110809     d wTip            s                   like(VIRtip)
029400110809     d wUd             s                   like(VTFud)
029500120227     d wUteAbi         s                   like(OTAA1cabi)
029600160304     d wKscAlfa8       s              8a   inz
029700160304     d wKscFact        s              8a   inz
029800080208
029900090508      //---------------------------------------------------------------
030000090508      //?Definizione procedure usate.
030100090508      //---------------------------------------------------------------
030200110801       // - Dettaglio abilitazioni
030300110801     d tnta61c         pr                  extpgm('TNTA61C')
030400110801     d  kpjba                              likeds(KPJBA)    const
030500110802     d  Tnta61ds                           likeds(TNTA61DS)
030600110801     d  ParmPgm                      10
030700110808
030800110808       // - Interrogazione particolarità giacenza
030900110808     d trtb69r         pr                  extpgm('TRTB69R')
031000110808     d  kpjba                              likeds(KPJBA)    const
031100110808
031200110808       // - Interrogazione particolarità consegna
031300110808     d trtb70r         pr                  extpgm('TRTB70R')
031400110808     d  kpjba                              likeds(KPJBA)    const
031500120227
031600120227       // -?Ricerca cliente
031700120227     d xclir           pr                  extpgm('XCLIR')
031800120227     d  xclirds                            likeds(XCLIRDS)
031900080206
032000080626      //---------------------------------------------------------------
032100160609      //?Prototipi.
032200080626      //---------------------------------------------------------------
032300130729      /copy gaitrasrc/srcprotopr,fnlv55r
032400111028      /copy gaitrasrc/srcprotopr,tibs02r
032500080626      /copy gaitrasrc/srcprotopr,tibs34r
032600110812      /copy gaitrasrc/srcprotopi,tibs69r
032700110615      /copy gaitrasrc/srcprotopr,tibs69r
032800110615      /copy gaitrasrc/srcprotopr,tntaa1c
032900110801
033000110801       // - Esecuzione comando di sistema
033100110801      /copy gaitrasrc/srcProtoPR,QCMDEXC
033200100624
033300080206      //---------------------------------------------------------------
033400080206      //?Definizione key-list.
033500080206      //---------------------------------------------------------------
033600100720       // - File TABEL00F
033700100720     d k03tabel      e ds                  extname(TABEL00F:*key)
033800100720     d                                     prefix(k_)
033900111027
034000111027      //---------------------------------------------------------------
034100111027      //?Indicatori per la stampa.
034200111027      //---------------------------------------------------------------
034300111027      * 80 - Stampo nuovo codice cliente quando richiamato da duplica
034400111027      * 90 - Salto pagina
034500080206
034600080206      //---------------------------------------------------------------
034700080206
034800080206      //---------------------------------------------------------------
034900080206      //?M A I N - L I N E
035000080206      //---------------------------------------------------------------
035100080206     c     *Entry        plist
035200100720     c                   parm                    kpjba
035300110615     c                   parm                    tnta61ds
035400080206
035500080206      /free
035600080206
035700100622       //?Operazioni iniziali
035800080206       exsr RoutInz;
035900080206
036000100622       //?Gestione video
036100080206       DOW $Fine = *off;
036200100624         SELECT;
036300110615           WHEN $Video = 'D1';
036400110615             exsr GesD01;
036500100719           WHEN $Video = 'S1';
036600100719             exsr GesS01;
036700160609           WHEN $Video = 'W1';
036800160609             exsr GesW01;
036900111027           WHEN $Video = 'ST';
037000111027             exsr Stampa;
037100100624           OTHER;
037200080206             $Fine = *on;
037300100624         ENDSL;
037400080206       ENDDO;
037500080206
037600100622       //?Operazioni finali
037700080206       exsr RoutEnd;
037800080206
037900080206       //--------------------------------------------------------------
038000080206       //?Operazioni iniziali.
038100080206       //--------------------------------------------------------------
038200080206       BEGSR RoutInz;
038300080206
038400100622       //?Impostazione campi "fissi"
038500110411         VT1pgm = SDSpgm;
038600110616         VT1tit = '* ABILITAZIONI CLIENTE *';
038700110616         k_TBLkut = 1;
038800120227
038900120227       //?Controllo se pgm richiamato o se è da menù
039000120227         wDaMenu = (%subst(kpjbu:1:1) = 'M');
039100120227         IF wDaMenu;
039200120227           clear TNTA61DS;
039300120227         ENDIF;
039400110615
039500110615       //?Controllo se pgm richiamato o se è da menù
039600120227         IF  %parms > 1 and not wDaMenu;
039700110615           $Richiamato = *on;
039800110617           F03Attivo = *off;
039900110617           F12Attivo = *on;
040000111026           $Video = 'S1';
040100111026           $InzS01 = *on;
040200111026           IF  TA61tla = 'C';
040300111026             $Controlla = *on;
040400111026           ENDIF;
040500111027           IF  TA61tla = 'P';
040600111027             $Stampa = *on;
040700111027           ENDIF;
040800110615         ELSE;
040900110617           F03attivo = *on;
041000110617           F12Attivo = *on;
041100110616           $Video = 'D1';
041200110616           $InzD01 = *on;
041300110615         ENDIF;
041400120227
041500120227       //?Pulisco campi output
041600120227         IF  $Richiamato;
041700120227           clear TA61flg;
041800120227           clear TA61err;
041900120227           clear TA61msg;
042000120227         ENDIF;
042100080206
042200100622       //?Reperimento dati job
042300080206         exsr DatiJob;
042400110411
042500110615       //?Se non ho il codice cliente esco dal pgm con errore al chiamante
042600120227         IF  TA61ksc = 0 and $Richiamato;
042700110615           TA61err = '1';
042800110615           TA61msg = $Msg(01);
042900110411           $Fine = *on;
043000110809           leavesr;
043100110411         ENDIF;
043200110809
043300110809       //?Imposto la data del giorno
043400110809         wOggi = %dec(%date());
043500110808
043600110808       //?Imposto in che sistema informativo sono
043700110808       //?S.I. di Prova o S.I. di Produzione
043800110808         $Prova = (%subst(knsif : 7 : 1) = 'P');
043900110907
044000110907       //?Imposto se utente di sede
044100110907         $SedeUte = (DUTpou = 046);
044200110808
044300110808       //?Apro il file tabelle
044400110808         exsr OpenFileTbe;
044500110615
044600110615       //?Carico Sk tabella abilitazionei
044700110615         clear xx;
044800110616         C_tab = 'ABC';
044900110615         setll C_tab TNTBE01L;
045000110615         reade C_tab TNTBE01L;
045100110615
045200110615         DOW not %eof(TNTBE01L);
045300110615           IF  TBEatb = *blanks;
045400110615             xx += 1;
045500110722             dABC       = TBEuni;
045600110722             $Abc(xx)   = TBEke1;
045700110722             $Des(xx)   = d§ABCdes;
045800110722             $Aut(xx)   = d§ABCaut;
045900110722             $Ord(xx)   = d§ABCord;
046000110722             $Ordw(xx)  = d§ABCord;
046100110722             $File(xx)  = d§ABCfile;
046200110722             $Pgm(xx)   = d§ABCpgm;
046300110808             $Rpers(xx) = d§ABCrpers;
046400110808             $Ipers(xx) = d§ABCipers;
046500110808             $Files(xx) = d§ABCfiles;
046600160609             $IntW(xx)  = d§ABCintw;
046700110615           ENDIF;
046800110615           reade C_tab TNTBE01L;
046900110615         ENDDO;
047000110615
047100110615       //?Ordino la sk
047200110615         sorta $Ordw;
047300111107
047400111107       //?Apro il file tabelle
047500111107         exsr OpenFileTab;
047600111107
047700111107       //?Carico Sk tabella 7R + chi autorizza
047800111107         clear xx;
047900111107         k_TBLcod = '7R';
048000111107         setll (k_TBLkut:k_TBLcod) TABEL00F;
048100111107         reade (k_TBLkut:k_TBLcod) TABEL00F;
048200111107
048300111107         DOW not %eof(TABEL00F);
048400111107           IF  TBLflg = *blanks;
048500111107             xx += 1;
048600111107             ds7R       = TBLuni;
048700111107             $7R(xx)    = %subst(TBLkey:1:2);
048800111107             $7Raut(xx) = §7Raut;
048900111107           ENDIF;
049000111107           reade (k_TBLkut:k_TBLcod) TABEL00F;
049100111107         ENDDO;
049200111107
049300111107       //?Carico Sk tabella 7W + chi autorizza
049400111107         clear xx;
049500111107         k_TBLcod = '7W';
049600111107         setll (k_TBLkut:k_TBLcod) TABEL00F;
049700111107         reade (k_TBLkut:k_TBLcod) TABEL00F;
049800111107
049900111107         DOW not %eof(TABEL00F);
050000111107           IF  TBLflg = *blanks;
050100111107             xx += 1;
050200111107             ds7W       = TBLuni;
050300111107             $7W(xx)    = %subst(TBLkey:1:2);
050400111107             $7Waut(xx) = §7Waut;
050500111107           ENDIF;
050600111107           reade (k_TBLkut:k_TBLcod) TABEL00F;
050700111107         ENDDO;
050800120227
050900120227       //?Se da Menù cerco il codice abilitazione utente
051000120227         reset TNTAA1DS;
051100120227         ITAA1caut = '§utegtc';
051200120227         kpjbu     = TNTAA1DS;
051300120227         tntaa1c (kpjba);
051400120227         TNTAA1DS = kpjbu;
051500120227
051600120227         IF  OTAA1fabi = 'N';
051700120227           wErrGrave   = *on;
051800120227           ErrMessage  = *on;
051900120227           ErrGenerico = *on;
052000120227           V1Cmsg = $Msg(01);
052100120227           leavesr;
052200120227         ENDIF;
052300120227
052400120227         wUteAbi = OTAA1cabi;
052500130729
052600130729       //?Cerco il terminal della filiale del cliente
052700130729         IF  TA61ksc > 0;
052800130729           clear FNLV55DS;
052900130729           D55tpt = 'P';
053000130729           D55lin = %dec(%subst(%editc(TA61ksc:'X'):1:3):3:0);
053100130729           D55drf = %dec(%date());
053200130729           fnlv55r (fnlv55ds);
053300130729           IF  D55err <> *blanks;
053400130729             D55tfp = D55lin;
053500130729           ENDIF;
053600130729         ENDIF;
053700110802
053800110802         exec sql set option dynusrprf = *owner,
053900110802                             closqlcsr = *endmod;
054000090714
054100080206       ENDSR;
054200080206
054300080206       //--------------------------------------------------------------
054400080206       //?Reperimento Dati del job (Utente/Operativi).
054500080206       //--------------------------------------------------------------
054600080206       BEGSR DatiJob;
054700080206
054800080206         in(E) §AzUte;
054900110615         IF  not %error;
055000080206           in(E) §DatiUte;
055100110615         ENDIF;
055200110615         IF  %error or RSut = *blanks;
055300080206           clear TIBS34ds;
055400080206           tibs34r(tibs34ds);
055500080206           in §AzUte;
055600080206           in §DatiUte;
055700110615         ENDIF;
055800080206
055900080206       ENDSR;
056000110808
056100110808       //--------------------------------------------------------------
056200110808       //?Apro il file TNTBE00F.
056300110808       //--------------------------------------------------------------
056400110808       BEGSR OpenFileTbe;
056500110808
056600110808       //?Chiudo il file se aperto
056700110808         IF  %open(TNTBE01L);
056800110808           close TNTBE01L;
056900110808         ENDIF;
057000110808
057100110808       //?Apro il file
057200110808         SELECT;
057300110808
057400110907           WHEN  ($Sede or $SedeUte) and $Prova;
057500110808             wLibTbe = 'GAITRAGRPS/TNTBE01L';
057600110907           WHEN  ($Sede or $SedeUte) and not $Prova;
057700110808             wLibTbe = 'GAITRAGRU/TNTBE01L';
057800110907           WHEN  $Prova;
057900110808             wLibTbe = 'FILTRAGRPF/TNTBE01L';
058000110907           WHEN  not $Prova;
058100110808             wLibTbe = 'FILTRAGRU/TNTBE01L';
058200110808
058300110808         ENDSL;
058400110808
058500110808         open  TNTBE01L;
058600110808
058700110808       ENDSR;
058800110808
058900110808       //--------------------------------------------------------------
059000110808       //?Apro il file TABEL00F.
059100110808       //--------------------------------------------------------------
059200110808       BEGSR OpenFileTab;
059300110808
059400110808       //?Chiudo il file se aperto
059500110808         IF  %open(TABEL00F);
059600110808           close TABEL00F;
059700110808         ENDIF;
059800110808
059900110808       //?Apro il file
060000110808         SELECT;
060100110808
060200110907           WHEN  ($Sede or $SedeUte) and $Prova;
060300110808             wLibTab = 'GAITRAGRPS/TABEL00F';
060400110907           WHEN  ($Sede or $SedeUte) and not $Prova;
060500110808             wLibTab = 'GAITRAGRU/TABEL00F';
060600110907           WHEN  $Prova;
060700110808             wLibTab = 'FILTRAGRPF/TABEL00F';
060800110907           WHEN  not $Prova;
060900110808             wLibTab = 'FILTRAGRU/TABEL00F';
061000110808
061100110808         ENDSL;
061200110808
061300110808         open  TABEL00F;
061400110808
061500110808       ENDSR;
061600110615
061700110615       //--------------------------------------------------------------
061800110615       //?Gestione videata D01.
061900110615       //--------------------------------------------------------------
062000110615       BEGSR GesD01;
062100110615
062200110615       //?Inizializzazione videata
062300110615         IF  $InzD01;
062400110615           exsr InzD01;
062500110615           $InzD01  = *off;
062600110615         ENDIF;
062700110615
062800110615       //?Emissione Testata
062900110615         write  TA61T01;
063000110615
063100110615       //?Emissione videata
063200120227         exfmt  TA61D01;
063300110615         reset ErrMessage;
063400110615         reset ErrGenerico;
063500110615         clear V1Cmsg;
063600110615
063700110615         SELECT;
063800120227
063900120227       //?- Errore grave esco dal pgm
064000120227           WHEN  wErrGrave;
064100120227             $Fine = *on;
064200110615
064300110615       //?- F03=Fine
064400110615           WHEN  dsp_aid = c_F03;
064500110615             exsr F03D01;
064600110615
064700110615       //?Invio
064800110615           OTHER;
064900110615             exsr CtrD01;
065000110615             IF  ErrGenerico;
065100110615               leavesr;
065200110615             ENDIF;
065300120227             TA61ksc = %dec(V01ksc:7:0);
065400110615             $Video  = 'S1';
065500110615             $InzS01 = *on;
065600110615
065700110615         ENDSL;
065800110615
065900110615       ENDSR;
066000110615
066100110615       //--------------------------------------------------------------
066200110615       //?Inizializzazione videata D01.
066300110615       //--------------------------------------------------------------
066400110615       BEGSR InzD01;
066500110615
066600120227         clear V01ksc;
066700110615         clear V1Dksc;
066800110615
066900110615       ENDSR;
067000110615
067100110615       //--------------------------------------------------------------
067200110615       //?Controllo videata D01.
067300110615       //--------------------------------------------------------------
067400110615       BEGSR CtrD01;
067500120227
067600120227       //?Ricerca del cliente
067700120227         IF  %scan('?' : V01ksc) > 0;
067800120227           ErrGenerico = *on;
067900120227           PosCurKSC   = *on;
068000120227           clear xCLIrds;
068100120227           DXCaut = wUteAbi;
068200120227           DXCcap = DUTkci;
068300120227           xclir (xCLIrds);
068400120227           ksa = DXCksc;
068500120227           dsksa = ksa(1);
068600120227           ksc(1) = dsksn;
068700120227           V01ksc = %editc(ksc(1):'X');
068800120227         ENDIF;
068900110615
069000110615       //?Cliente obbligatorio
069100120227         IF  V01ksc = *zeros or V01ksc = *blanks;
069200110615           ErrMessage  = *on;
069300110615           ErrGenerico = *on;
069400110615           PosCurKsc   = *on;
069500110615           V1Cmsg = $Msg(01);
069600110615           leavesr;
069700110615         ENDIF;
069800120227
069900120227       //?Solo valori numerici
070000120227         IF  %check(digits:V01ksc) > 0;
070100120227           ErrMessage  = *on;
070200120227           ErrGenerico = *on;
070300120227           PosCurKSC   = *on;
070400120227           V1Cmsg      = $Msg(01);
070500120227           leavesr;
070600120227         ENDIF;
070700110615
070800110615       //?Cliente valido
070900110615         clear tibs69ds;
071000120227         I69kac = %dec(V01ksc:7:0);
071100110615         TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
071200110615         IF  O69err <> *blanks;
071300110615           ErrMessage  = *on;
071400110615           ErrGenerico = *on;
071500110615           PosCurKsc   = *on;
071600110615           V1Cmsg = $Msg(01);
071700110615           leavesr;
071800110615         ENDIF;
071900110615         V1Dksc = ACOrag;
072000110615
072100110615       //?Utente abilitato al cliente
072200110615         clear TNTAA1DS;
072300110615         ITAA1caut = '§utecli';
072400120227         ITAA1ksc  = %dec(V01ksc:7:0);
072500110615         kpjbu     = tntaa1ds;
072600110615         tntaa1c (kpjba);
072700110615         TNTAA1DS = kpjbu;
072800110615         IF  OTAA1fabi = 'N';
072900110615           ErrMessage  = *on;
073000110615           ErrGenerico = *on;
073100110615           PosCurKsc   = *on;
073200110615           V1Cmsg      = $Msg(01);
073300110615           leavesr;
073400110615         ENDIF;
073500110615
073600110615       ENDSR;
073700100622
073800100622       //--------------------------------------------------------------
073900100622       //?Gestione videata S01.
074000100622       //--------------------------------------------------------------
074100100622       BEGSR GesS01;
074200100622
074300100622       //?Inizializzazione videata
074400100622         IF  $InzS01;
074500100622           exsr InzS01;
074600100622           $InzS01  = *off;
074700130729           $Bsp = *off;
074800110927           $Cli = *off;
074900100622         ENDIF;
075000111026
075100111026       //?Se richiamato per controllo non emetto il subfile
075200111026       //?ma vado a fine
075300111026         IF  $Controlla;
075400111026           $Fine = *on;
075500111026           leavesr;
075600111026         ENDIF;
075700111027
075800111027       //?Se richiamato per stampa non emetto il subfile
075900111027       //?ma vado nella routine di stampa
076000111027         IF  $Stampa;
076100111027           $Video  = 'ST';
076200111027           leavesr;
076300111027         ENDIF;
076400110511
076500110511       //?Visualizzazione del SFL (se ci sono dati)
076600110511         SflDsp_N = (S01nrr <= *zeros);
076700110511
076800110511       //?Posizionamento cursore al 1° rcd della pagina
076900110809         IF  V1Ccsr > *zero;
077000110809           V1Crcd = V1Ccsr;
077100110511         ELSE;
077200110809           V1Crcd = 1;
077300110511         ENDIF;
077400110511
077500110615         V1Ccsr = V1Crcd;
077600110411
077700110722       //?Imposto cliente in testata subfile
077800110615         IF  $Richiamato;
077900110615           V1Cksc = TA61ksc;
078000110615           clear V1Dksc;
078100110615           clear tibs69ds;
078200110615           I69kac = V1Cksc;
078300110615           TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
078400110615           V1Dksc = ACOrag;
078500110615         ENDIF;
078600100622
078700100622       //?Emissione Testata e Piede con tasti funzionali abilitati
078800110615         write  TA61T01;
078900110615         write  TA61Z01;
079000100622
079100100622       //?Emissione videata
079200110615         exfmt  TA61C01;
079300100622         reset ErrMessage;
079400100622         reset ErrGenerico;
079500110615         clear V1Cmsg;
079600100622
079700100622         SELECT;
079800100719
079900100719       //?- F03=Fine
080000100719           WHEN  dsp_aid = c_F03;
080100110615             exsr F03D01;
080200111027
080300111027       //?- F08=Stampa
080400121204           WHEN  dsp_aid = c_F08 and S01nrr > 0;
080500111027             exsr Stampa;
080600110617
080700110617       //?- F12=Ritorno
080800110617           WHEN  dsp_aid = c_F12;
080900110617             exsr F12S01;
081000100622
081100100622       //?Invio
081200100622           OTHER;
081300121010             IF  S01nrr = 0;
081400121010               leavesr;
081500121010             ENDIF;
081600110615             IF  V1Ccsr > *zero;
081700110412               exsr OpzS01;
081800110412               IF  ErrGenerico;
081900110412                 leavesr;
082000110412               ENDIF;
082100110412             ENDIF;
082200100622
082300100622         ENDSL;
082400100622
082500100622       ENDSR;
082600100622
082700100622       //--------------------------------------------------------------
082800100622       //?Inizializzazione videata S01.
082900100622       //--------------------------------------------------------------
083000100622       BEGSR InzS01;
083100100622
083200111026       //?Pulizia subfile se NON richiamato per controllo
083300111026         IF  not $Controlla;
083400111026           exsr PulS01;
083500111026         ENDIF;
083600110411
083700110511       //?Caricamento subfile
083800111026       //?sempre perchè nella routine faccio già i controlli se
083900111026       //?scrivere o meno il subfile
084000110511         exsr Ries01;
084100121217
084200121217         sav_s01nrr = S01nrr;
084300110511
084400110511         SflEnd = *on;
084500100622
084600100622       ENDSR;
084700100624
084800100624       //--------------------------------------------------------------
084900100624       //?Pulizia SFL01.
085000100624       //--------------------------------------------------------------
085100100624       BEGSR PulS01;
085200100624
085300100624       //?Pulizia subfile
085400100624         SflDsp_N    = *on;
085500100624         SflDspCtl_N = *on;
085600110615         write  TA61C01;
085700100624         SflDspCtl_N = *off;
085800100624         SflEnd      = *off;
085900100624
086000110615         clear V1Crcd;
086100110615         clear V1Ccsr;
086200100624         clear S01nrr;
086300110615         clear V1Cmsg;
086400100720
086500100624         ErrMessage  = *off;
086600100624         ErrGenerico = *off;
086700100720
086800100624         WindDspF    = IndDspF;
086900100624         reset WindDspFb;
087000100624         IndDspF     = WindDspF;
087100100624
087200100624       ENDSR ;
087300100622
087400100622       //--------------------------------------------------------------
087500110615       //?Riempimento S01.
087600100622       //--------------------------------------------------------------
087700100622       BEGSR RieS01;
087800110511
087900110615       //?Carico in base alla tabella (ordinata)
088000110615         xx = 1;
088100110615         FOR xx by 1 to %elem($Ordw);
088200110615
088300110615           IF  $Ordw(xx) > 0;
088400110615             yy = %lookup($Ordw(xx) : $Ord);
088500110615             IF  yy > 0;
088600110808               exsr CarS01;
088700111026           //?Se richiamato per controllo
088800111026           //?ed ho trovato almeno 1 abilitazione
088900111026           //?esco dal ciclo
089000111026               IF  $Controlla and $RecOK;
089100111026                 leavesr;
089200111026               ENDIF;
089300110615             ENDIF;
089400110615           ENDIF;
089500110616
089600110615         ENDFOR;
089700110615
089800110615       ENDSR ;
089900110808
090000110808       //--------------------------------------------------------------
090100110808       //?Caricamento S01.
090200110808       //--------------------------------------------------------------
090300110808       BEGSR CarS01;
090400110808
090500110616         $RecOK = *off;
090600110808
090700110808       //?Imposto se leggere file di sede o da S.I.
090800110808         $Sede = ($Files(yy) = 'S');
090900110616
091000110616       //?Controllo se il cliente ha l'abilitazione che sto leggendo
091100110616         SELECT;
091200110722         //?Ricerca personalizzata x key ABC
091300110804           WHEN  $Rpers(yy) = 'S';
091400110722             exsr da_ABC;
091500110616         //?Da TABEL00F
091600110616           WHEN  $File(yy) = 'TABEL00F';
091700110616             exsr da_TABEL;
091800110616         //?Da TNTBE00F
091900110616           WHEN  $File(yy) = 'TNTBE00F';
092000110616             exsr da_TNTBE;
092100110616         ENDSL;
092200110808
092300110808       //?Se il cliente è abilitato scrivo il subfile
092400111026       //?ma solo se NON richiamato per controllo
092500111026         IF  $Recok and not $Controlla;
092600110808           exsr WrtS01;
092700110808         ENDIF;
092800110808
092900110808       ENDSR ;
093000110808
093100110808       //--------------------------------------------------------------
093200110808       //?Scrivo il subfile.
093300110808       //--------------------------------------------------------------
093400110808       BEGSR WrtS01;
093500110808
093600110808         PosCurOpz = *off;
093700110808
093800160609       //?Proteggo il campo scelta se non è prevista interrogazione
093900160609         IF  V1Hpgm = *blanks and V1Hnota = *blanks;
094000110808           Proteggi = *on;
094100110808         ELSE;
094200110808           Proteggi = *off;
094300110808         ENDIF;
094400110808
094500110808         V1Hin40 = Proteggi;
094600110808
094700110808         S01nrr += 1;
094800110808         write  TA61S01;
094900110615
095000111027       ENDSR;
095100160609
095200160609       //--------------------------------------------------------------
095300160609       //?Gestione videata W01.
095400160609       //--------------------------------------------------------------
095500160609       BEGSR GesW01;
095600160609
095700160609       //?Inizializzazione videata
095800160609         IF  $InzW01;
095900160609           exsr InzW01;
096000160609           $InzW01  = *off;
096100160609         ENDIF;
096200160609
096300160609       //?Emissione videata
096400160609         exfmt  TA61W01;
096500160609
096600160609         SELECT;
096700160609
096800160609       //?- F12=Ritorno
096900160609           WHEN  dsp_aid = c_F12;
097000160609             exsr F12W01;
097100160609
097200160609       //?Invio
097300160609           OTHER;
097400160609
097500160609         ENDSL;
097600160609
097700160609       ENDSR;
097800160609
097900160609       //--------------------------------------------------------------
098000160609       //?Inizializzazione videata W01.
098100160609       //--------------------------------------------------------------
098200160609       BEGSR InzW01;
098300160609
098400160609         wnota = V1Hnota;
098500160609
098600160609       ENDSR;
098700160609
098800160609       //--------------------------------------------------------------
098900160609       //?Gestione tasto funzionale F12 da videata W01.
099000160609       //?F12=Ritorno
099100160609       //--------------------------------------------------------------
099200160609       BEGSR F12W01;
099300160609
099400160609       //?Ritorno al subfile
099500160609         $Video  = 'S1';
099600160609
099700160609       ENDSR;
099800110722
099900110722       //--------------------------------------------------------------
100000110722       //?Ricerca personalizzata.
100100110722       //--------------------------------------------------------------
100200110722       BEGSR da_ABC;
100300110927
100400110927       //?Tabella CLI l'aggancio solo una volta
100500130729           IF  %subst($Abc(yy):1:3) = 'CLI' and not $Cli;
100600130729             clear dCLI;
100700110927         //?Apro il file
100800130729             exsr OpenFileTbe;
100900130729             C_tab = %subst($Abc(yy):1:3);
101000130729             C_ke1 = %editc(TA61ksc:'X');
101100130729             chain (C_tab : C_ke1) TNTBE01L;
101200130729             IF  %found(TNTBE01L) and TBEatb = *blanks;
101300130729               dCLI = TBEuni;
101400130729               $Cli = *on;
101500130729             ENDIF;
101600130729           ENDIF;
101700130729
101800160125       //?File FIBSP lo aggancio solo una volta
101900130729           IF  %subst($Abc(yy):1:3) = 'BSP' and not $Bsp;
102000160125             chain (TA61ksc) FIBSP02L;
102100160125             IF  %found(FIBSP02L);
102200130729               $Bsp = *on;
102300130729             ENDIF;
102400130729           ENDIF;
102500110722
102600110722       //?Personalizzato
102700110722         SELECT;
102800110802           WHEN  $Abc(yy) = 'FATTURAZIONE';
102900110802             exsr Fat;
103000110802           WHEN  $Abc(yy) = 'PAG.C/A';
103100110802             exsr PagCA;
103200151116           WHEN  $Abc(yy) = 'PAG.C/A ASS';
103300151116             exsr PagCAASS;
103400151116           WHEN  $Abc(yy) = 'PAG.DANNI';
103500151116             exsr PagDN;
103600151116           WHEN  $Abc(yy) = 'PAG.DANNI ASS';
103700151116             exsr PagDNASS;
103800151116           WHEN  $Abc(yy) = 'PAG.N.ACC.';
103900151116             exsr PagNA;
104000151116           WHEN  $Abc(yy) = 'PAG.N.ACC. ASS';
104100151116             exsr PagNAASS;
104200160304           WHEN  $Abc(yy) = 'C/ASSEGNI';
104300160304             exsr CAssegni;
104400110803           WHEN  $Abc(yy) = 'CONTO ECONOMICO';
104500110803             exsr ContoEco;
104600110803           WHEN  $Abc(yy) = 'ESENZIONE IVA';
104700110803             exsr EsIVA;
104800110809           WHEN  $Abc(yy) = 'ORM';
104900110809             exsr OrmVao;
105000110926           WHEN  $Abc(yy) = 'CLI LDV';
105100110926             exsr tabCLILDV;
105200110926           WHEN  $Abc(yy) = 'CLI DPD';
105300110926             exsr tabCLIDPD;
105400110926           WHEN  $Abc(yy) = 'CLI LNA';
105500110926             exsr tabCLILNA;
105600110926           WHEN  $Abc(yy) = 'CLI LVP';
105700110926             exsr tabCLILVP;
105800110926           WHEN  $Abc(yy) = 'CLI DKB';
105900110926             exsr tabCLIDKB;
106000110926           WHEN  $Abc(yy) = 'CLI DCR';
106100110926             exsr tabCLIDCR;
106200110926           WHEN  $Abc(yy) = 'CLI GCP';
106300110926             exsr tabCLIGCP;
106400110926           WHEN  $Abc(yy) = 'CLI BRD';
106500110926             exsr tabCLIBRD;
106600110926           WHEN  $Abc(yy) = 'CLI BAN';
106700110926             exsr tabCLIBAN;
106800120620           WHEN  $Abc(yy) = 'CLI EMD';
106900120620             exsr tabCLIEMD;
107000130321           WHEN  $Abc(yy) = 'CLI PDF';
107100130321             exsr tabCLIPDF;
107200131018           WHEN  $Abc(yy) = 'CLI PSM';
107300131018             exsr tabCLIPSM;
107400151109           WHEN  $Abc(yy) = 'CLI EMDP';
107500151109             exsr tabCLIEMDP;
107600110803           WHEN  $Abc(yy) = 'DCC';
107700110803             exsr tabDCC;
107800120621           WHEN  $Abc(yy) = 'FLB';
107900120621             exsr tabFLB;
108000120604           WHEN  $Abc(yy) = 'JDC';
108100120604             exsr tabJDC;
108200110913           WHEN  $Abc(yy) = 'LAC';
108300110913             exsr tabLAC;
108400130906           WHEN  $Abc(yy) = 'LCA';
108500130906             exsr tabLCA;
108600110805           WHEN  $Abc(yy) = 'LLT';
108700110805             exsr tabLLT;
108800130729           WHEN  $Abc(yy) = 'BSP SOS';
108900160125             exsr fileBSPSOS;
109000130729           WHEN  $Abc(yy) = 'BSP SOD';
109100160125             exsr fileBSPSOD;
109200160125           WHEN  $Abc(yy) = 'BSP FPA';
109300160125             exsr fileBSPFPA;
109400110809           WHEN  $Abc(yy) = '3C';
109500110809             exsr tab3C;
109600110915             exsr tab3C_VAB;
109700110811           WHEN  $Abc(yy) = '3K VAC';
109800110811             exsr tab3K_VAC;
109900110811           WHEN  $Abc(yy) = '3K VAG';
110000110811             exsr tab3K_VAG;
110100110811           WHEN  $Abc(yy) = '3K VAI';
110200110811             exsr tab3K_VAI;
110300110811           WHEN  $Abc(yy) = '3K VGD';
110400110811             exsr tab3K_VGD;
110500160304           WHEN  $Abc(yy) = 'VAP';
110600160304             exsr tabVAP;
110700110809           WHEN  $Abc(yy) = '3Q';
110800110809             exsr tab3Q;
110900140428           WHEN  $Abc(yy) = 'COMODATO';
111000140428             exsr Comodato;
111100110811           WHEN  $Abc(yy) = '4C VAS';
111200110811             exsr tab4C_VAS;
111300110811           WHEN  $Abc(yy) = '4C VAF';
111400110811             exsr tab4C_VAF;
111500110722           WHEN  $Abc(yy) = '7U';
111600110722             exsr tab7U;
111700160609           WHEN  $Abc(yy) = 'ALERT SMS';
111800160609             exsr AlertSMS;
111900160609           WHEN  $Abc(yy) = 'ALERT MAIL';
112000160609             exsr AlertMail;
112100110722         ENDSL;
112200110722
112300110722       ENDSR ;
112400110616
112500110616       //--------------------------------------------------------------
112600110616       //?Cerco su TABEL00F.
112700110616       //--------------------------------------------------------------
112800110616       BEGSR da_TABEL;
112900110808
113000110808       //?Apro il file
113100110808         exsr OpenFileTab;
113200110616
113300110722       //?Vado diretta con il codice cliente in key tabella
113400110722         k_TBLcod = %subst($Abc(yy):1:2);
113500110722         k_TBLkey = %editc(TA61ksc:'X');
113600110722         chain %kds(K03tabel) TABEL00F;
113700110805         IF  not %found(TABEL00F) or TBLflg <> *blanks;
113800110805           leavesr;
113900110805         ENDIF;
114000110805
114100110722           $RecOk = *on;
114200110805
114300110805       //?Imposto i dati da emettere nel subfile
114400111026         exsr Dati_x_S01;
114500110616
114600110616       ENDSR ;
114700110616
114800110616       //--------------------------------------------------------------
114900110616       //?Cerco su TNTBE00F.
115000110616       //--------------------------------------------------------------
115100110616       BEGSR da_TNTBE;
115200110808
115300110808       //?Apro il file
115400110808         exsr OpenFileTbe;
115500110616
115600110722       //?Vado diretta con il codice cliente in key tabella
115700110722         C_tab = %subst($Abc(yy):1:3);
115800110722         C_ke1 = %editc(TA61ksc:'X');
115900110722         chain (C_tab : C_ke1) TNTBE01L;
116000110805         IF  not %found(TNTBE01L) or TBEatb <> *blanks;
116100110805           leavesr;
116200110805         ENDIF;
116300110805
116400110805         $RecOk = *on;
116500110805
116600110805       //?Imposto i dati da emettere nel subfile
116700111026         exsr Dati_x_S01;
116800110616
116900110616       ENDSR ;
117000110802
117100110802       //--------------------------------------------------------------
117200110802       //?Fatturazione.
117300110802       //--------------------------------------------------------------
117400110802       BEGSR Fat;
117500110802
117600110802         CLPkut = 1;
117700110802         CLPkcc = Dutkci;
117800110802         CLPksc = TA61ksc;
117900110802         chain (CLPkut:CLPkcc:CLPksc) CNCLP00F;
118000110802         IF  not %found(CNCLP00F) or CLPfft <> 1;
118100110802           leavesr;
118200110802         ENDIF;
118300110802
118400110802         $RecOk = *on;
118500110802
118600110802       //?Imposto i dati da emettere nel subfile
118700110802         exsr Dati_x_S01;
118800110802
118900110802       ENDSR ;
119000110802
119100110802       //--------------------------------------------------------------
119200151116       //?Pagamento C/A estero
119300110802       //--------------------------------------------------------------
119400110802       BEGSR PagCA;
119500110802
119600110802         CLPkut = 1;
119700110802         CLPkcc = Dutkci;
119800110802         CLPksc = TA61ksc;
119900110802         chain (CLPkut:CLPkcc:CLPksc) CNCLP00F;
120000110802         IF  not %found(CNCLP00F) or
120100151116             CLPfpc <> '6';
120200110802           leavesr;
120300110802         ENDIF;
120400110802
120500110802         $RecOk = *on;
120600110802
120700110802       //?Imposto i dati da emettere nel subfile
120800110802         exsr Dati_x_S01;
120900110802
121000110802       ENDSR ;
121100151116
121200151116       //--------------------------------------------------------------
121300151116       //?Pagamento C/A assegno traenza
121400151116       //--------------------------------------------------------------
121500151116       BEGSR PagCAASS;
121600151116
121700151116         CLPkut = 1;
121800151116         CLPkcc = Dutkci;
121900151116         CLPksc = TA61ksc;
122000151116         chain (CLPkut:CLPkcc:CLPksc) CNCLP00F;
122100151116         IF  not %found(CNCLP00F) or
122200151116             CLPfpc <> '1';
122300151116           leavesr;
122400151116         ENDIF;
122500151116
122600151116         $RecOk = *on;
122700151116
122800151116       //?Imposto i dati da emettere nel subfile
122900151116         exsr Dati_x_S01;
123000151116
123100151116       ENDSR ;
123200151116
123300151116       //--------------------------------------------------------------
123400151116       //?Pagamento Danni estero
123500151116       //--------------------------------------------------------------
123600151116       BEGSR PagDN;
123700151116
123800151116         CLSksc = TA61ksc;
123900151116         chain (CLSksc) FNCLS01L;
124000151116         IF  not %found(FNCLS01L) or
124100151116             %subst(CLStic:1:1) <> '6';
124200151116           leavesr;
124300151116         ENDIF;
124400151116
124500151116         $RecOk = *on;
124600151116
124700151116       //?Imposto i dati da emettere nel subfile
124800151116         exsr Dati_x_S01;
124900151116
125000151116       ENDSR ;
125100151116
125200151116       //--------------------------------------------------------------
125300151116       //?Pagamento Danni assegno traenza
125400151116       //--------------------------------------------------------------
125500151116       BEGSR PagDNASS;
125600151116
125700151116         CLSksc = TA61ksc;
125800151116         chain (CLSksc) FNCLS01L;
125900151116         IF  not %found(FNCLS01L) or
126000151116             %subst(CLStic:1:1) <> '1';
126100151116           leavesr;
126200151116         ENDIF;
126300151116
126400151116         $RecOk = *on;
126500151116
126600151116       //?Imposto i dati da emettere nel subfile
126700151116         exsr Dati_x_S01;
126800151116
126900151116       ENDSR ;
127000151116
127100151116       //--------------------------------------------------------------
127200151116       //?Pagamento Note Accredito estero
127300151116       //--------------------------------------------------------------
127400151116       BEGSR PagNA;
127500151116
127600151116         CLSksc = TA61ksc;
127700151116         chain (CLSksc) FNCLS01L;
127800151116         IF  not %found(FNCLS01L) or
127900151116             %subst(CLStic:2:1) <> '6';
128000151116           leavesr;
128100151116         ENDIF;
128200151116
128300151116         $RecOk = *on;
128400151116
128500151116       //?Imposto i dati da emettere nel subfile
128600151116         exsr Dati_x_S01;
128700151116
128800151116       ENDSR ;
128900151116
129000151116       //--------------------------------------------------------------
129100151116       //?Pagamento Note Accredito assegno traenza
129200151116       //--------------------------------------------------------------
129300151116       BEGSR PagNAASS;
129400151116
129500151116         CLSksc = TA61ksc;
129600151116         chain (CLSksc) FNCLS01L;
129700151116         IF  not %found(FNCLS01L) or
129800151116             %subst(CLStic:2:1) <> '1';
129900151116           leavesr;
130000151116         ENDIF;
130100151116
130200151116         $RecOk = *on;
130300151116
130400151116       //?Imposto i dati da emettere nel subfile
130500151116         exsr Dati_x_S01;
130600151116
130700151116       ENDSR ;
130800160304
130900160304       //--------------------------------------------------------------
131000160304       //?C/Assegni.
131100160304       //--------------------------------------------------------------
131200160304       BEGSR CAssegni;
131300160304
131400160304         wKscAlfa8 = *all'0';
131500160304         %subst(wKscAlfa8:2:7) = %editc(TA61ksc:'X');
131600160304         clear wKscFact;
131700160304         exec sql
131800160304         select RCOkscfact into :wKscFact
131900160304         FROM anrco00f
132000160304         WHERE  RCOsocieta = '201' and
132100160304                RCOsnatura = 'C' and
132200160304                RCOksc = :wKscAlfa8;
132300160304
132400160304         IF  wKscFact = *blanks;
132500160304           leavesr;
132600160304         ENDIF;
132700160304
132800160304         $RecOk = *on;
132900160304
133000160304       //?Imposto i dati da emettere nel subfile
133100160304         exsr Dati_x_S01;
133200160304         wKscAlfa = %subst(wKscFact:2:7);
133300160304         V1Sdes = %trim(V1Sdes) + ' ' + wKscAlfa;
133400160304
133500160304       ENDSR ;
133600110803
133700110803       //--------------------------------------------------------------
133800110803       //?Conto Economico.
133900110803       //--------------------------------------------------------------
134000110803       BEGSR ContoEco;
134100110803
134200110803         setll TA61ksc ECEVD02L;
134300110803         reade TA61ksc ECEVD02L;
134400110803         DOW not %eof(ECEVD02L);
134500110803         //?Se trovo anche solo 1 personalizzazione valida ad oggi
134600110803         //?emetto nel subfile
134700110803           IF  EVDdst >= %dec(%date());
134800110803             $RecOk = *on;
134900110803           //?Imposto i dati da emettere nel subfile
135000110803             exsr Dati_x_S01;
135100110803             leave;
135200110803           ENDIF;
135300110803         reade TA61ksc ECEVD02L;
135400110803         ENDDO;
135500110803
135600110803       ENDSR ;
135700110803
135800110803       //--------------------------------------------------------------
135900110803       //?Esenzione IVA.
136000110803       //--------------------------------------------------------------
136100110803       BEGSR EsIVA;
136200110803
136300110803         INDkut = 1;
136400110803         INDkcc = Dutkci;
136500110803         INDksc = TA61ksc;
136600110803         chain (INDkut:INDkcc:INDksc) CNIND00F;
136700110803         IF  not %found(CNIND00F);
136800110803           leavesr;
136900110803         ENDIF;
137000110805
137100110803         IF INDnpr <> *zeros or INDdpr <> *zeros or
137200110803            INDnpn <> *zeros;
137300110803
137400110803           $RecOk = *on;
137500110803
137600110803         //?Imposto i dati da emettere nel subfile
137700110803           exsr Dati_x_S01;
137800110803
137900110803         ENDIF;
138000110803
138100110803       ENDSR ;
138200110809
138300110809       //--------------------------------------------------------------
138400110809       //?Invio ORM da VAS.
138500110809       //--------------------------------------------------------------
138600110809       BEGSR OrmVao;
138700110809
138800110809       //?Controllo se il KSC è abilitato all'invio degli ORM
138900110809         wKscAlfa = %editc(TA61ksC:'X');
139000110809         wTip = 'VO';
139100110809         exsr  Abi_VAS;
139200110809         IF  wKscAlfa <> *blanks;
139300110809           $RecOk = *on;
139400110809           exsr Dati_x_S01;
139500110809         ENDIF;
139600110809
139700110809       ENDSR ;
139800110809
139900110809       //--------------------------------------------------------------
140000110927       //?Tabella CLI campo LDV
140100110809       //--------------------------------------------------------------
140200110927       BEGSR tabCLILDV;
140300110809
140400110927       //?Imposto i dati da emettere nel subfile
140500110927         IF  §CLIldv = 'S';
140600110927           exsr Dati_x_S01;
140700110809           exsr WrtS01;
140800110809         ENDIF;
140900110927
141000110927       ENDSR;
141100110927
141200110927       //--------------------------------------------------------------
141300110927       //?Tabella CLI campo DPD
141400110927       //--------------------------------------------------------------
141500110927       BEGSR tabCLIDPD;
141600110927
141700110927       //?Imposto i dati da emettere nel subfile
141800110927         IF  §CLIdpd = 'S';
141900110927           exsr Dati_x_S01;
142000110809           exsr WrtS01;
142100110809         ENDIF;
142200110927
142300110927       ENDSR ;
142400110927
142500110927       //--------------------------------------------------------------
142600110927       //?Tabella CLI campo LNA
142700110927       //--------------------------------------------------------------
142800110927       BEGSR tabCLILNA;
142900110927
143000110927       //?Imposto i dati da emettere nel subfile
143100110809         IF  §CLIlna = 'S';
143200110927           exsr Dati_x_S01;
143300110809           exsr WrtS01;
143400110809         ENDIF;
143500110927
143600110927       ENDSR ;
143700110927
143800110927       //--------------------------------------------------------------
143900110927       //?Tabella CLI campo LVP
144000110927       //--------------------------------------------------------------
144100110927       BEGSR tabCLILVP;
144200110927
144300110927       //?Imposto i dati da emettere nel subfile
144400110809         IF  §CLIlvp = 'S';
144500110927           exsr Dati_x_S01;
144600110809           exsr WrtS01;
144700110809         ENDIF;
144800110927
144900110927       ENDSR ;
145000110927
145100110927       //--------------------------------------------------------------
145200110927       //?Tabella CLI campo DKB
145300110927       //--------------------------------------------------------------
145400110927       BEGSR tabCLIDKB;
145500110927
145600110927       //?Imposto i dati da emettere nel subfile
145700110809         IF  §CLIdkb = 'S';
145800110927           exsr Dati_x_S01;
145900110927           exsr WrtS01;
146000110809         ENDIF;
146100110927
146200110927       ENDSR ;
146300110927
146400110927       //--------------------------------------------------------------
146500110927       //?Tabella CLI campo DCR
146600110927       //--------------------------------------------------------------
146700110927       BEGSR tabCLIDCR;
146800110927
146900110927       //?Imposto i dati da emettere nel subfile
147000110809         IF  §CLIdcr = 'S';
147100110927           exsr Dati_x_S01;
147200110809           exsr WrtS01;
147300110809         ENDIF;
147400110927
147500110927       ENDSR ;
147600110927
147700110927       //--------------------------------------------------------------
147800110927       //?Tabella CLI campo GCP
147900110927       //--------------------------------------------------------------
148000110927       BEGSR tabCLIGCP;
148100110927
148200110927       //?Imposto i dati da emettere nel subfile
148300110809         IF  §CLIgcp = 'S';
148400110927           exsr Dati_x_S01;
148500110809           exsr WrtS01;
148600110809         ENDIF;
148700110927
148800110927       ENDSR ;
148900110927
149000110927       //--------------------------------------------------------------
149100110927       //?Tabella CLI campo BRD
149200110927       //--------------------------------------------------------------
149300110927       BEGSR tabCLIBRD;
149400110927
149500110927       //?Imposto i dati da emettere nel subfile
149600110809         IF  §CLIbrd = 'S';
149700110927           exsr Dati_x_S01;
149800110809           exsr WrtS01;
149900110809         ENDIF;
150000110809
150100110809       ENDSR ;
150200110927
150300110927       //--------------------------------------------------------------
150400110927       //?Tabella CLI campo BAN
150500110927       //--------------------------------------------------------------
150600110927       BEGSR tabCLIBAN;
150700110927
150800110927       //?Imposto i dati da emettere nel subfile
150900110927         IF  §CLIban = 'S';
151000110927           exsr Dati_x_S01;
151100110927           exsr WrtS01;
151200110927         ENDIF;
151300110927
151400110927       ENDSR ;
151500120620
151600120620       //--------------------------------------------------------------
151700120620       //?Tabella CLI campo EMD
151800120620       //--------------------------------------------------------------
151900120620       BEGSR tabCLIEMD;
152000120620
152100120620       //?Imposto i dati da emettere nel subfile
152200120620         IF  §CLIemd = 'S';
152300120620           exsr Dati_x_S01;
152400120620           exsr WrtS01;
152500120620         ENDIF;
152600120620
152700120620       ENDSR ;
152800130320
152900130321       //--------------------------------------------------------------
153000130321       //?Tabella CLI campo PDF
153100130321       //--------------------------------------------------------------
153200130321       BEGSR tabCLIPDF;
153300130321
153400130321       //?Imposto i dati da emettere nel subfile
153500130321         IF  §CLIpdfp <> *blanks;
153600130321           exsr Dati_x_S01;
153700130321           exsr WrtS01;
153800130321         ENDIF;
153900130321
154000130321       ENDSR ;
154100131018
154200131018       //--------------------------------------------------------------
154300131018       //?Tabella CLI campo PSM
154400131018       //--------------------------------------------------------------
154500131018       BEGSR tabCLIPSM;
154600131018
154700131018       //?Imposto i dati da emettere nel subfile
154800160108         IF  §CLIpsm <> *blanks and §CLIpsm <> *zeros;
154900131018           exsr Dati_x_S01;
155000131018           exsr WrtS01;
155100131018         ENDIF;
155200131018
155300131018       ENDSR ;
155400151109
155500151109       //--------------------------------------------------------------
155600151109       //?Tabella CLI campo EMDP
155700151109       //--------------------------------------------------------------
155800151109       BEGSR tabCLIEMDP;
155900151109
156000151109       //?Imposto i dati da emettere nel subfile
156100151109         IF  §CLIemdp = 'S';
156200151109           exsr Dati_x_S01;
156300151109           exsr WrtS01;
156400151109         ENDIF;
156500151109
156600151109       ENDSR ;
156700110803
156800110803       //--------------------------------------------------------------
156900110803       //?Tabella DCC.
157000110803       //--------------------------------------------------------------
157100110803       BEGSR tabDCC;
157200110808
157300110808       //?Apro il file
157400110808         exsr OpenFileTbe;
157500110913
157600110913         clear dDCC;
157700110913         C_tab = %subst($Abc(yy):1:3);
157800110913         setll C_tab TNTBE01L;
157900110913         reade C_tab TNTBE01L;
158000110913         DOW not %eof(TNTBE01L);
158100110913
158200110913           IF  TBEatb = *blanks;
158300110913             dDCC = TBEuni;
158400110913             IF  §DCCksc = TA61ksc;
158500110913               $RecOk = *on;
158600110913             //?Imposto i dati da emettere nel subfile
158700110913               exsr Dati_x_S01;
158800110913               leave;
158900110913             ENDIF;
159000110913           ENDIF;
159100110913
159200110913           reade C_tab TNTBE01L;
159300110913         ENDDO;
159400110913
159500110913       //?Imposto la riga da emettere a subfile
159600110913         IF  $RecOk;
159700110913           V1Sdes = 'Assegnata serie ' + %subst(TBEke1:1:4) +
159800110913                    ' per DISK C con prefisso "CC"';
159900110913         ENDIF;
160000110803
160100110803       ENDSR ;
160200120621
160300120621       //--------------------------------------------------------------
160400120621       //?Tabella FLB.
160500120621       //--------------------------------------------------------------
160600120621       BEGSR tabFLB;
160700120621
160800120621       //?Apro il file
160900120621         exsr OpenFileTbe;
161000120621
161100120621         clear wFLBuni;
161200120621         C_tab = %subst($Abc(yy):1:3);
161300120621         setll C_tab TNTBE01L;
161400120621         reade C_tab TNTBE01L;
161500120621         DOW not %eof(TNTBE01L);
161600120621
161700120621           IF  TBEatb = *blanks;
161800120621             wFLBuni = TBEuni;
161900120621             IF  %lookup(%editc(TA61ksc:'X'):skksc) > 0;
162000120621             //?Imposto i dati da emettere nel subfile
162100120621             //?posso avere più righe per cliente
162200120621               exsr Dati_x_S01;
162300120621               V1Sdes = 'Forza LNA cappario ' + %subst(TBEke1:1:3) +
162400120621                        ' in LNA ' + %subst(TBEke2:1:3);
162500120621               exsr WrtS01;
162600120621             ENDIF;
162700120621           ENDIF;
162800120621
162900120621           reade C_tab TNTBE01L;
163000120621         ENDDO;
163100120621
163200120621
163300120621       ENDSR ;
163400120604
163500120604       //--------------------------------------------------------------
163600120604       //?Tabella JDC.
163700120604       //--------------------------------------------------------------
163800120604       BEGSR tabJDC;
163900120604
164000120604       //?Apro il file e forzo la lettura da Sede
164100120604         $Sede = *on;
164200120604         exsr OpenFileTbe;
164300120604
164400120604         clear dJDC;
164500120604         C_tab = %subst($Abc(yy):1:3);
164600120604         C_ke1 = %editc(TA61ksc:'X');
164700120604         chain (C_tab : C_ke1) TNTBE01L;
164800120604         IF  not %found(TNTBE01L) or TBEatb <> *blanks;
164900120604           leavesr;
165000120604         ENDIF;
165100120604
165200120604         dJDC = TBEuni;
165300120604
165400120604       //?Se tipo elaborazione = 'N' non scrivo il rcd nel sbfl
165500120604         IF  §JDCaut = 'N';
165600120604           leavesr;
165700120604         ENDIF;
165800120604
165900120604         $RecOk = *on;
166000120604
166100120604       //?Imposto i dati da emettere nel subfile
166200120604         exsr Dati_x_S01;
166300120604
166400120604       ENDSR ;
166500110722
166600110913       //--------------------------------------------------------------
166700110913       //?Tabella LAC.
166800110913       //--------------------------------------------------------------
166900110913       BEGSR tabLAC;
167000110913
167100110913       //?Apro il file e forzo la lettura da Sede
167200110913         $Sede = *on;
167300110913         exsr OpenFileTbe;
167400110913
167500150513         sav_AUT  = $Aut(yy);
167600150513
167700110913         clear dLAC;
167800110913         C_tab = %subst($Abc(yy):1:3);
167900110913         C_ke1 = %editc(TA61ksc:'X');
168000110913         chain (C_tab : C_ke1) TNTBE01L;
168100110913         IF  not %found(TNTBE01L) or TBEatb <> *blanks;
168200110913           leavesr;
168300110913         ENDIF;
168400110913
168500110913         dLAC = TBEuni;
168600110913
168700110913       //?Se tipo elaborazione = 'N' non scrivo il rcd nel sbfl
168800110913         IF  §LACaut = 'N';
168900110913           leavesr;
169000110913         ENDIF;
169100110913
169200110913         $RecOk = *on;
169300150513
169400150513       //?Se cliente con NO addebito
169500150513       //?devo impostare come autorizzazione la 'DG' e non 'ICT'
169600150513         IF  §LACtad = 'N';
169700150513           $Aut(yy) = 'G';
169800150513         ENDIF;
169900150513       //?Se cliente con personalizzazione delle competenze
170000150513         exec sql
170100150513         select EVDitr into :wCompCE
170200150513         FROM ecevd00f
170300150513         WHERE EVDcli = :TA61ksc and
170400150513               EVDvoc = 199 and
170500150513               EVDttv = 'K' and
170600150513               EVDddt <= :woggi and
170700150513               EVDdst >= :woggi;
170800150513           IF  sqlcod = 100 or sqlcod < 0;
170900150513           ELSE;
171000150513             $Aut(yy) = 'G';
171100150513           ENDIF;
171200110913
171300110913       //?Imposto i dati da emettere nel subfile
171400110913         exsr Dati_x_S01;
171500150513         $Aut(yy) = sav_AUT;
171600110913
171700110913       ENDSR ;
171800130906
171900130906       //--------------------------------------------------------------
172000130906       //?Tabella LCA.
172100130906       //--------------------------------------------------------------
172200130906       BEGSR tabLCA;
172300130906
172400130906       //?Apro il file
172500130906         exsr OpenFileTbe;
172600130906
172700130906         clear dLCA;
172800130906         C_tab = %subst($Abc(yy):1:3);
172900130906         setll C_tab TNTBE01L;
173000130906         reade C_tab TNTBE01L;
173100130906         DOW not %eof(TNTBE01L);
173200130906
173300130906           IF  TBEatb = *blanks;
173400130906             IF  %subst(TBEke2:9:7) = %editc(TA61ksc:'X');
173500130906               dLCA = TBEuni;
173600130906               $RecOk = *on;
173700130906             //?Imposto i dati da emettere nel subfile
173800130906               exsr Dati_x_S01;
173900130906               leave;
174000130906             ENDIF;
174100130906           ENDIF;
174200130906
174300130906           reade C_tab TNTBE01L;
174400130906         ENDDO;
174500130906
174600130906       ENDSR ;
174700110805
174800110805       //--------------------------------------------------------------
174900110805       //?Tabella LLT.
175000110805       //--------------------------------------------------------------
175100110805       BEGSR tabLLT;
175200110808
175300110808       //?Apro il file
175400110808         exsr OpenFileTbe;
175500110805
175600110805         C_tab = %subst($Abc(yy):1:3);
175700110805         clear C_ke1;
175800110805         %subst(C_ke1:9:7) = %editc(TA61ksc:'X');
175900110805         chain (C_tab : C_ke1) TNTBE01L;
176000110805         IF  not %found(TNTBE01L) or TBEatb <> *blanks;
176100110805           leavesr;
176200110805         ENDIF;
176300110805
176400110805         $RecOk = *on;
176500110805
176600110805         dLLT = TBEuni;
176700110805
176800110805       //?Imposto i dati da emettere nel subfile
176900110805         exsr Dati_x_S01;
177000110805
177100110805         IF  §LLTimp = 0;
177200110915           V1Sdes = 'Gestione danni sempre con Pratica Assicurativa';
177300110805         ELSE;
177400110915           V1Sdes = 'Gestione danni in L.T. fino a EUR ' +
177500110914                    %trim(%editc(§LLtimp:'4'));
177600110805         ENDIF;
177700110805
177800110805       ENDSR ;
177900130729
178000130729       //--------------------------------------------------------------
178100160125       //?File FIBSP per Sospensione x colli non spuntati
178200130729       //--------------------------------------------------------------
178300160125       BEGSR fileBSPSOS;
178400130729
178500130729       //?Imposto i dati da emettere nel subfile
178600160125         IF  $Bsp and
178700160125             BSPfpa = *blanks and
178800160125             BSPaut = *blanks;
178900160125           exsr Dati_x_S01;
179000160125           exsr WrtS01;
179100130729         ENDIF;
179200130729
179300130729       ENDSR ;
179400130729
179500130729       //--------------------------------------------------------------
179600160125       //?File FIBSP per Sospensione + Data consegna richiesta
179700130729       //--------------------------------------------------------------
179800160125       BEGSR fileBSPSOD;
179900130729
180000130729       //?Imposto i dati da emettere nel subfile
180100160125         IF  $Bsp and
180200160125             BSPfpa = *blanks and BSPdcrp = 'S' and
180300160125             BSPaut = *blanks;
180400130729           exsr Dati_x_S01;
180500130729           exsr WrtS01;
180600130729         ENDIF;
180700130729
180800130729       ENDSR ;
180900160125
181000160125       //--------------------------------------------------------------
181100160125       //?File FIBSP per Sospensione + Sposta bolle ad altra filiale
181200160125       //--------------------------------------------------------------
181300160125       BEGSR fileBSPFPA;
181400160125
181500160125       //?Imposto i dati da emettere nel subfile
181600160125         IF  $Bsp and
181700160125             BSPfpa = 'S';
181800160125           exsr Dati_x_S01;
181900160125           exsr WrtS01;
182000160125         ENDIF;
182100160125
182200160125       ENDSR ;
182300110805
182400110722       //--------------------------------------------------------------
182500110722       //?Tabella 3C.
182600110722       //--------------------------------------------------------------
182700110722       BEGSR tab3C;
182800110808
182900110808       //?Apro il file
183000110808         exsr OpenFileTab;
183100110809
183200110809         k_TBLcod = %subst($Abc(yy):1:2);
183300110809         k_TBLkey = %editc(TA61ksc:'X');
183400110809         chain %kds(K03tabel) TABEL00F;
183500110809         IF  not %found(TABEL00F) or TBLflg <> *blanks;
183600110809           leavesr;
183700110809         ENDIF;
183800110809
183900110809         ds3C = TBLuni;
184000110812
184100110812         $RecOk = *on;
184200110809
184300110812       //?Controllo se il cliente o l'unificante sono abilitati al VAS
184400110812         wKscAlfa = %editc(§3Ccks:'X');
184500110812         wUd  = 'U';
184600110812         exsr  Abi_VAB;
184700110812       //?Se non trovo con unificante ed è diverso provo con KSC
184800110812         IF  wKscAlfa = *blanks and TA61ksc <> §3Ccks;
184900110809           wKscAlfa = %editc(TA61ksc:'X');
185000110809           exsr  Abi_VAB;
185100110812         ENDIF;
185200120503
185300120503       //?Se supporto EDI devo controllare se abilitato ai servizi VAS
185400120503       //?con tipo file "EI"
185500120503         IF  §3Ccba = 'EDI';
185600120503           wKscAlfa = %editc(§3Ccks:'X');
185700120503           wTip = 'EI';
185800120503           exsr  Abi_VAS;
185900120503         //?Se non trovo con unificante ed è diverso provo con KSC
186000120503           IF  wKscAlfa = *blanks and TA61ksc <> §3Ccks;
186100120503             wKscAlfa = %editc(TA61ksc:'X');
186200120503             exsr  Abi_VAS;
186300120503           ENDIF;
186400120503         ENDIF;
186500110812
186600110812       //?Imposto i dati da emettere nel subfile
186700110812         exsr Dati_x_S01;
186800110913
186900110913       //?Se EasySpedWeb è per forza abilitato ai servizi VAS
187000110913         IF  §3Ccba = 'ESWEB';
187100110913           leavesr;
187200110913         ENDIF;
187300110915
187400110915       //?Se supporto SNA è per forza abilitato ai servizi VAS
187500110915         IF  §3Ccba = 'SNA';
187600110915           leavesr;
187700110915         ENDIF;
187800110812
187900110812       //?Se non ho trovato abilitazione VAS lo indico nella descrizione
188000110812         IF  wKscAlfa = *blanks;
188100110812           V1Sdes = %trim(V1Sdes) + ' - non abilitato a VAS';
188200110812         ENDIF;
188300110915
188400110915       //?Se non abilitato e non c'è la serie e nemmeno l'accorpamento
188500110915       //?bolle non devo scrivere la riga nel subfile
188600110915         IF  wKscAlfa = *blanks and §3Cnrs = 0 and §3Cabd = *blanks;
188700110915           $RecOk = *off;
188800110915         ENDIF;
188900110722
189000110722       ENDSR ;
189100110915
189200110915       //--------------------------------------------------------------
189300110915       //?Tabella 3C VAB - Abilitazione VAB x no tab.3C
189400110915       //--------------------------------------------------------------
189500110915       BEGSR tab3C_VAB;
189600110915
189700110915       //?Se il cliente non è in tabella 3C provo a vedere lo stesso
189800110915       //?se è abilitato ai VAS
189900110915         IF  not $RecOk;
190000110915           wKscAlfa = %editc(TA61ksc:'X');
190100110915           exsr  Abi_VAB;
190200110915
190300110915         //?Se è abilitato ai VAS
190400110915           IF  wKscAlfa <> *blanks;
190500110915             $RecOk = *on;
190600110915           //?Imposto i dati da emettere nel subfile
190700110915           //?ma non do la possibilità di interrogare il dettaglio
190800110915             exsr Dati_x_S01;
190900110915             clear V1Hpgm;
191000110915           ENDIF;
191100110915         ENDIF;
191200110915
191300110915       ENDSR ;
191400110811
191500110811       //--------------------------------------------------------------
191600110811       //?Tabella 3K VAC - Consegne
191700110811       //--------------------------------------------------------------
191800110811       BEGSR tab3K_VAC;
191900110811
192000110811       //?Apro il file
192100110811         exsr OpenFileTab;
192200110811
192300110811         k_TBLcod = %subst($Abc(yy):1:2);
192400110811         k_TBLkey = %editc(TA61ksc:'X');
192500110811         chain %kds(K03tabel) TABEL00F;
192600110811         IF  not %found(TABEL00F) or TBLflg <> *blanks;
192700110811           leavesr;
192800110811         ENDIF;
192900110811
193000110811         ds3K = TBLuni;
193100110811
193200110811       //?Scrivo riga se trovo il codice unificante e i relativi flag
193300110811         IF  §3Kcks = *zeros or
193400110811            (§3Kcq1 = *blanks and §3Kcq2 = *blanks and
193500110811             §3Kcq3 = *blanks and §3Kcq4 = *blanks and
193600110811             §3Kcq5 = *blanks and §3Kcq8 = *blanks);
193700110811           leavesr;
193800110811         ENDIF;
193900110811
194000110811         $RecOk = *on;
194100110811
194200110811       //?Controllo se il cliente o l'unificante sono abilitati al VAS
194300110811         wKscAlfa = %editc(§3Kcks:'X');
194400110811         wTip = 'VC';
194500110811         wUd  = 'D';
194600110811         exsr  Abi_VAS;
194700110811       //?Se non trovo con unificante ed è diverso provo con KSC
194800110811         IF  wKscAlfa = *blanks and TA61ksc <> §3Kcks;
194900110811           wKscAlfa = %editc(TA61ksc:'X');
195000110811           exsr  Abi_VAS;
195100110811         ENDIF;
195200110811
195300110811       //?Imposto i dati da emettere nel subfile
195400110811         exsr Dati_x_S01;
195500110811
195600110811       //?Se non ho trovato abilitazione VAS lo indico nella descrizione
195700110811         IF  wKscAlfa = *blanks;
195800110811           V1Sdes = %trim(V1Sdes) + ' - non abilitato a VAS';
195900110811         ENDIF;
196000110811
196100110811       ENDSR ;
196200110811
196300110811       //--------------------------------------------------------------
196400110811       //?Tabella 3K VAG - Giacenze
196500110811       //--------------------------------------------------------------
196600110811       BEGSR tab3K_VAG;
196700110811
196800110811       //?Apro il file
196900110811         exsr OpenFileTab;
197000110811
197100110811         k_TBLcod = %subst($Abc(yy):1:2);
197200110811         k_TBLkey = %editc(TA61ksc:'X');
197300110811         chain %kds(K03tabel) TABEL00F;
197400110811         IF  not %found(TABEL00F) or TBLflg <> *blanks;
197500110811           leavesr;
197600110811         ENDIF;
197700110811
197800110811         ds3K = TBLuni;
197900110811
198000110811       //?Scrivo riga se trovo il codice unificante e i relativi flag
198100110811       //?devo fare 2 controlli
198200110811
198300110811       //?Un controllo con unificante VAG + flag (l'invio di questi
198400110811       //?dati viene scatenato dal file giacenze)
198500110811         IF  §3Kgks <> *zeros and
198600110811            (§3Kgqi <> *blanks or §3Kapp <> *blanks);
198700110811           $RecOk = *on;
198800110811           wKscAlfa = %editc(§3Kgks:'X');
198900110811         ENDIF;
199000110811       //?l'altro con unificante VAC + flag (l'invio di questi dati
199100110811       //?NON viene scatenato dal file giacenze)
199200110811         IF  §3Kcks <> *zeros and (§3Knmc <> *blanks or
199300110811             §3Kcq6 <> *blanks or §3Kcq7 <> *blanks);
199400110811           $RecOk = *on;
199500110811           wKscAlfa = %editc(§3Kcks:'X');
199600110811         ENDIF;
199700110811
199800110811       //?Se non i controlli non hanno dato esito positivo vado via
199900110811         IF  not $RecOk;
200000110811           leavesr;
200100110811         ENDIF;
200200110811
200300110811       //?Controllo se il cliente o l'unificante sono abilitati al VAS
200400110811         wTip = 'VG';
200500110811         wUd  = 'D';
200600110811         exsr  Abi_VAS;
200700110811       //?Se non trovo con unificante ed è diverso provo con KSC
200800110811         IF  wKscAlfa = *blanks and TA61ksc <> §3Kcks;
200900110811           wKscAlfa = %editc(TA61ksc:'X');
201000110811           exsr  Abi_VAS;
201100110811         ENDIF;
201200110811
201300110811       //?Imposto i dati da emettere nel subfile
201400110811         exsr Dati_x_S01;
201500110811
201600110811       //?Se non ho trovato abilitazione VAS lo indico nella descrizione
201700110811         IF  wKscAlfa = *blanks;
201800110811           V1Sdes = %trim(V1Sdes) + ' - non abilitato a VAS';
201900110811         ENDIF;
202000110811
202100110811       ENDSR ;
202200110811
202300110811       //--------------------------------------------------------------
202400110811       //?Tabella 3K VAI - Comunicazioni al cliente
202500110811       //--------------------------------------------------------------
202600110811       BEGSR tab3K_VAI;
202700110811
202800110811       //?Apro il file
202900110811         exsr OpenFileTab;
203000110811
203100110811         k_TBLcod = %subst($Abc(yy):1:2);
203200110811         k_TBLkey = %editc(TA61ksc:'X');
203300110811         chain %kds(K03tabel) TABEL00F;
203400110811         IF  not %found(TABEL00F) or TBLflg <> *blanks;
203500110811           leavesr;
203600110811         ENDIF;
203700110811
203800110811         ds3K = TBLuni;
203900110811
204000110811       //?Scrivo riga se trovo il codice unificante e i relativi flag
204100110811         IF  §3Kcks = *zeros or
204200110811            (§3Kcma = *blanks and §3Kcba = *blanks);
204300110811           leavesr;
204400110811         ENDIF;
204500110811
204600110811         $RecOk = *on;
204700110811
204800110811       //?Controllo se il cliente o l'unificante sono abilitati al VAS
204900110811         wKscAlfa = %editc(§3Kcks:'X');
205000110811         wTip = 'VI';
205100110811         wUd  = 'D';
205200110811         exsr  Abi_VAS;
205300110811       //?Se non trovo con unificante ed è diverso provo con KSC
205400110811         IF  wKscAlfa = *blanks and TA61ksc <> §3Kcks;
205500110811           wKscAlfa = %editc(TA61ksc:'X');
205600110811           exsr  Abi_VAS;
205700110811         ENDIF;
205800110811
205900110811       //?Imposto i dati da emettere nel subfile
206000110811         exsr Dati_x_S01;
206100110811
206200110811       //?Se non ho trovato abilitazione VAS lo indico nella descrizione
206300110811         IF  wKscAlfa = *blanks;
206400110811           V1Sdes = %trim(V1Sdes) + ' - non abilitato a VAS';
206500110811         ENDIF;
206600110811
206700110811       ENDSR ;
206800110811
206900110811       //--------------------------------------------------------------
207000110811       //?Tabella 3K VGD - Dati C.A. completi
207100110811       //--------------------------------------------------------------
207200110811       BEGSR tab3K_VGD;
207300110811
207400110811       //?Apro il file
207500110811         exsr OpenFileTab;
207600110811
207700110811         k_TBLcod = %subst($Abc(yy):1:2);
207800110811         k_TBLkey = %editc(TA61ksc:'X');
207900110811         chain %kds(K03tabel) TABEL00F;
208000110811         IF  not %found(TABEL00F) or TBLflg <> *blanks;
208100110811           leavesr;
208200110811         ENDIF;
208300110811
208400110811         ds3K = TBLuni;
208500110811
208600110811       //?Scrivo riga se trovo il codice unificante e i relativi flag
208700110811         IF  §3Kgks = *zeros or §3Kca = *blanks;
208800110811           leavesr;
208900110811         ENDIF;
209000110811
209100110811         $RecOk = *on;
209200110811
209300110811       //?Imposto i dati da emettere nel subfile
209400110811         exsr Dati_x_S01;
209500110812
209600110812       //?Se l'unificnate è diverso decodifico e metto nella descrizione
209700110812         IF  §3Kgks <> TA61ksc;
209800110812           clear tibs69ds;
209900110812           I69kac = §3Kgks;
210000110812           TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
210100110812           IF  O69err = *blanks;
210200110812             V1Sdes = %trim(V1Sdes) + ' a ' +
210300110812                      %trim(%editc(§3Kgks:'4')) +
210400110812                      ' ' + ACOrag;
210500110812           ENDIF;
210600110812         ENDIF;
210700110812
210800110811       ENDSR ;
210900160304
211000160304       //--------------------------------------------------------------
211100160304       //?Tabella VAP - Esiti Ritiri.
211200160304       //--------------------------------------------------------------
211300160304       BEGSR tabVAP;
211400160304
211500160304       //?Apro il file
211600160304         exsr OpenFileTbe;
211700160304
211800160304         C_tab = %subst($Abc(yy):1:3);
211900160316         C_ke1 = '0' + %editc(TA61ksc:'X');
212000160304         chain (C_tab : C_ke1) TNTBE01L;
212100160316         IF  not %found(TNTBE01L) or TBEatb <> *blanks;
212200160304           leavesr;
212300160304         ENDIF;
212400160304
212500160304         $RecOk = *on;
212600160304
212700160304       //?Controllo se il cliente o l'unificante sono abilitati al VAS
212800160304         wKscAlfa = %subst(TBEke2:1:7);
212900160304         wTip = 'VP';
213000160304         wUd  = 'D';
213100160304         exsr  Abi_VAS;
213200160304       //?Se non trovo con unificante ed è diverso provo con KSC
213300160304         IF  wKscAlfa = *blanks and
213400160304             %editc(TA61ksc:'X') <> %subst(TBEke2:1:7);
213500160304           wKscAlfa = %editc(TA61ksc:'X');
213600160304           exsr  Abi_VAS;
213700160304         ENDIF;
213800160304
213900160304       //?Imposto i dati da emettere nel subfile
214000160304         exsr Dati_x_S01;
214100160304
214200160304       //?Se non ho trovato abilitazione VAS lo indico nella descrizione
214300160304         IF  wKscAlfa = *blanks;
214400160304           V1Sdes = %trim(V1Sdes) + ' - non abilitato a VAS';
214500160304         ENDIF;
214600160304
214700160304       ENDSR ;
214800110811
214900110809       //--------------------------------------------------------------
215000110809       //?Tabella 3Q.
215100110809       //--------------------------------------------------------------
215200110809       BEGSR tab3Q;
215300110809
215400110809       //?Apro il file
215500110809         exsr OpenFileTab;
215600110809
215700110809         k_TBLcod = %subst($Abc(yy):1:2);
215800110809         k_TBLkey = %editc(TA61ksc:'X');
215900110809         chain %kds(K03tabel) TABEL00F;
216000110809         IF  not %found(TABEL00F) or TBLflg <> *blanks;
216100110809           leavesr;
216200110809         ENDIF;
216300110809
216400110809         ds3Q = TBLuni;
216500110811
216600110811       //?Scrivo riga se trovo il codice unificante
216700110811         IF  §3Qcks = *zeros;
216800110811           leavesr;
216900110811         ENDIF;
217000110811
217100110811         $RecOk = *on;
217200110809
217300110811       //?Controllo se il cliente o l'unificante sono abilitati al VAS
217400110811         wKscAlfa = %editc(§3Qcks:'X');
217500110811         wTip = 'BT';
217600110811         wUd  = 'D';
217700110811         exsr  Abi_VAS;
217800110811       //?Se non trovo con unificante ed è diverso provo con KSC
217900110811         IF  wKscAlfa = *blanks and TA61ksc <> §3Qcks;
218000110809           wKscAlfa = %editc(TA61ksc:'X');
218100110809           exsr  Abi_VAS;
218200110811         ENDIF;
218300110811
218400110811       //?Imposto i dati da emettere nel subfile
218500110811         exsr Dati_x_S01;
218600110811
218700110811       //?Se non ho trovato abilitazione VAS lo indico nella descrizione
218800110811         IF  wKscAlfa = *blanks;
218900110811           V1Sdes = %trim(V1Sdes) + ' - non abilitato a VAS';
219000110811         ENDIF;
219100110809
219200110809       ENDSR ;
219300110809
219400110722       //--------------------------------------------------------------
219500140428       //?File UNCMD - Contratti Comodato
219600110722       //--------------------------------------------------------------
219700140428       BEGSR Comodato;
219800110808
219900140428         setll TA61ksc UNCMD01L;
220000140428         reade TA61ksc UNCMD01L;
220100140428         DOW not %eof(UNCMD01L);
220200140428         //?Se trovo anche solo 1 personalizzazione non annullata o chiusa
220300140428         //?emetto nel subfile
220400140428           IF  cmdcfg<>'A' and cmdcfg<>'C';
220500140428             $RecOk = *on;
220600140428           //?Imposto i dati da emettere nel subfile
220700140428             exsr Dati_x_S01;
220800140428             leave;
220900140428           ENDIF;
221000140428         reade TA61ksc UNCMD01L;
221100140428         ENDDO;
221200110722
221300110722       ENDSR ;
221400110811
221500110811       //--------------------------------------------------------------
221600110811       //?Tabella 4C VAS - contrassegni.
221700110811       //--------------------------------------------------------------
221800110811       BEGSR tab4C_VAS;
221900110811
222000120207       //?Apro il file e forzo la lettura da Sede
222100120207         $Sede = *on;
222200110811         exsr OpenFileTab;
222300110811
222400110811         k_TBLcod = %subst($Abc(yy):1:2);
222500110811         k_TBLkey = %editc(TA61ksc:'X');
222600110811         chain %kds(K03tabel) TABEL00F;
222700110811         IF  not %found(TABEL00F) or TBLflg <> *blanks;
222800110811           leavesr;
222900110811         ENDIF;
223000110811
223100110811         ds4C = TBLuni;
223200110811
223300110811       //?Scrivo riga se trovo il codice unificante e i relativi flag
223400110811         IF  §4Ccks = *zeros or
223500110811            (§4Cfsc = *blanks and §4Cfan = *blanks and §4Cflg = *blanks);
223600110811           leavesr;
223700110811         ENDIF;
223800110811
223900110811         $RecOk = *on;
224000110811
224100110811       //?Controllo se il cliente o l'unificante sono abilitati al VAS
224200110811         wKscAlfa = %editc(§4Ccks:'X');
224300110811         wTip = 'VS';
224400110811         wUd  = 'D';
224500110811         exsr  Abi_VAS;
224600110811       //?Se non trovo con unificante ed è diverso provo con KSC
224700110811         IF  wKscAlfa = *blanks and TA61ksc <> §4Ccks;
224800110811           wKscAlfa = %editc(TA61ksc:'X');
224900110811           exsr  Abi_VAS;
225000110811         ENDIF;
225100110811
225200110811       //?Imposto i dati da emettere nel subfile
225300110811         exsr Dati_x_S01;
225400110811
225500110811       //?Se non ho trovato abilitazione VAS lo indico nella descrizione
225600110811         IF  wKscAlfa = *blanks;
225700110811           V1Sdes = %trim(V1Sdes) + ' - non abilitato a VAS';
225800110811         ENDIF;
225900110811
226000110811       ENDSR ;
226100110809
226200110809       //--------------------------------------------------------------
226300110811       //?Tabella 4C VAF - fatture.
226400110809       //--------------------------------------------------------------
226500110811       BEGSR tab4C_VAF;
226600110809
226700120207       //?Apro il file e forzo la lettura da Sede
226800120207         $Sede = *on;
226900110809         exsr OpenFileTab;
227000110809
227100110809         k_TBLcod = %subst($Abc(yy):1:2);
227200110809         k_TBLkey = %editc(TA61ksc:'X');
227300110809         chain %kds(K03tabel) TABEL00F;
227400110809         IF  not %found(TABEL00F) or TBLflg <> *blanks;
227500110809           leavesr;
227600110809         ENDIF;
227700110809
227800110809         ds4C = TBLuni;
227900110811
228000110811       //?Scrivo riga se trovo il codice unificante
228100110811         IF  §4Cfks = *zeros;
228200110811           leavesr;
228300110811         ENDIF;
228400110811
228500110811         $RecOk = *on;
228600110811
228700110811       //?Controllo se il cliente o l'unificante sono abilitati al VAS
228800110811         wKscAlfa = %editc(§4Cfks:'X');
228900110811         wTip = 'VF';
229000110811         wUd  = 'D';
229100110811         exsr  Abi_VAS;
229200110811       //?Se non trovo con unificante ed è diverso provo con KSC
229300110811         IF  wKscAlfa = *blanks and TA61ksc <> §4Cfks;
229400110809           wKscAlfa = %editc(TA61ksc:'X');
229500110809           exsr  Abi_VAS;
229600110811         ENDIF;
229700110811
229800110811       //?Imposto i dati da emettere nel subfile
229900110811         exsr Dati_x_S01;
230000110811
230100110811       //?Se non ho trovato abilitazione VAS lo indico nella descrizione
230200110811         IF  wKscAlfa = *blanks;
230300110811           V1Sdes = %trim(V1Sdes) + '- non abilitato a VAS';
230400110809         ENDIF;
230500110809
230600110809       ENDSR ;
230700110722
230800110722       //--------------------------------------------------------------
230900110722       //?Tabella 7U.
231000110722       //--------------------------------------------------------------
231100110722       BEGSR tab7U;
231200110808
231300110808       //?Apro il file
231400110808         exsr OpenFileTab;
231500110722
231600110805         k_TBLcod = %subst($Abc(yy):1:2);
231700111107         sav_AUT  = $Aut(yy);
231800110805
231900110808       //?Leggo per particolarità giacenza
232000110805         k_TBLkey = %editc(TA61ksc:'X') + 'Q';
232100110805         chain %kds(K03tabel) TABEL00F;
232200110805         IF  %found(TABEL00F) and TBLflg = *blanks;
232300110805
232400110805           ds7U = TBLuni;
232500110805
232600110808         //?Creo 1 riga di subfile x ogni particolarità giacenza
232700110808           zz = 1;
232800110808           FOR zz by 1 to %elem($Part);
232900110808             IF  $Part(zz) <> *blanks;
233000111107           //?Cerco in tab. 7W chi autorizza
233100111107               ww = %lookup($Part(zz):$7W);
233200111107               IF  ww > 0 and $7Waut(ww) <> *blanks;
233300111107                 $Aut(yy) = $7Waut(ww);
233400110808               exsr Dati_x_S01;
233500110808               V1Sdes = %trim(V1Sdes) + ' ' + 'Giacenza ' + $Part(zz);
233600110808               V1Hpgm = 'TB69';
233700110808               V1Hipers = 'S';
233800110808               V1Hpart = $Part(zz);
233900160609               clear V1Hnota;
234000110808               exsr WrtS01;
234100111107               ENDIF;
234200110808             ENDIF;
234300110808           ENDFOR;
234400111107           $Aut(yy) = sav_AUT;
234500110808         //?Creo 1 riga di subfile x ogni flag
234600110808           IF  §7Usre <> *blanks;
234700110808             exsr Dati_x_S01;
234800110808             V1Sdes = %trim(V1Sdes) + ' ' +
234900110808                     'Reso anche con trazioni ridotte';
235000110808             clear V1Hpgm;
235100110808             clear V1Hipers;
235200110808             clear V1Hpart;
235300160609             clear V1Hnota;
235400110808             exsr WrtS01;
235500110808           ENDIF;
235600110808           IF  §7Uldcr <> *blanks;
235700110808             exsr Dati_x_S01;
235800110808             V1Sdes = %trim(V1Sdes) + ' ' + §7Uldcr +
235900110808                      ' GG oltre limite apertura giacenza' +
236000110808                      ' per data cons. richiesta';
236100110808             clear V1Hpgm;
236200110808             clear V1Hipers;
236300110808             clear V1Hpart;
236400160609             clear V1Hnota;
236500110808             exsr WrtS01;
236600110808           ENDIF;
236700110805         ENDIF;
236800110808
236900110808       //?Leggo per particolarità consegna
237000110808         k_TBLkey = %editc(TA61ksc:'X') + 'R';
237100110808         chain %kds(K03tabel) TABEL00F;
237200110808         IF  %found(TABEL00F) and TBLflg = *blanks;
237300110808
237400110808           ds7U = TBLuni;
237500110808
237600110808         //?Creo 1 riga di subfile x ogni particolarità consegna
237700110808           zz = 1;
237800110808           FOR zz by 1 to %elem($Part);
237900110808             IF  $Part(zz) <> *blanks;
238000111107           //?Cerco in tab. 7R chi autorizza
238100111107               ww = %lookup($Part(zz):$7R);
238200111107               IF  ww > 0 and $7Raut(ww) <> *blanks;
238300111107                 $Aut(yy) = $7Raut(ww);
238400110808               exsr Dati_x_S01;
238500110808               V1Sdes = %trim(V1Sdes) + ' ' + 'Consegna ' + $Part(zz);
238600110808               V1Hpgm = 'TB70';
238700110808               V1Hipers = 'S';
238800110808               V1Hpart = $Part(zz);
238900160609               clear V1Hnota;
239000110808               exsr WrtS01;
239100111107               ENDIF;
239200110808             ENDIF;
239300110808           ENDFOR;
239400110808         ENDIF;
239500110722
239600110722       ENDSR ;
239700160609
239800160609       //--------------------------------------------------------------
239900160609       //?File TFNTC - Alert SMS
240000160609       //--------------------------------------------------------------
240100160609       BEGSR AlertSMS;
240200160609
240300160609         %subst(KeyNtc:1:4) = %editc(DUTkci:'X');
240400160609         %subst(KeyNtc:5:7) = %editc(TA61ksc:'X');
240500160609
240600160609       //?Cerco Nota 'AS'
240700160609         exec sql
240800160609         select NTCrnt into :NTCrnt
240900160609         FROM TFNTC00F
241000160609         WHERE NTCapl = 'C' and
241100160609               NTCnk1 = :KeyNtc and
241200160609               NTCtnt = 'AS';
241300160609           IF  sqlcod = 100 or sqlcod < 0;
241400160609             leavesr;
241500160609           ENDIF;
241600160609
241700160609           $RecOk = *on;
241800160609         //?Imposto i dati da emettere nel subfile
241900160609           exsr Dati_x_S01;
242000160609           V1Hnota = NTCrnt;
242100160609
242200160609       ENDSR ;
242300160609
242400160609       //--------------------------------------------------------------
242500160609       //?File TFNTC - Alert MAIL
242600160609       //--------------------------------------------------------------
242700160609       BEGSR AlertMAIL;
242800160609
242900160609         %subst(KeyNtc:1:4) = %editc(DUTkci:'X');
243000160609         %subst(KeyNtc:5:7) = %editc(TA61ksc:'X');
243100160609
243200160609       //?Cerco Nota 'AS'
243300160609         exec sql
243400160609         select NTCrnt into :NTCrnt
243500160609         FROM TFNTC00F
243600160609         WHERE NTCapl = 'C' and
243700160609               NTCnk1 = :KeyNtc and
243800160609               NTCtnt = 'AM';
243900160609           IF  sqlcod = 100 or sqlcod < 0;
244000160609             leavesr;
244100160609           ENDIF;
244200160609
244300160609           $RecOk = *on;
244400160609         //?Imposto i dati da emettere nel subfile
244500160609           exsr Dati_x_S01;
244600160609           V1Hnota = NTCrnt;
244700160609
244800160609       ENDSR ;
244900110802
245000110802       //--------------------------------------------------------------
245100110802       //?Dati in S01.
245200110802       //--------------------------------------------------------------
245300110802       BEGSR Dati_x_S01;
245400110802
245500110802         clear  TA61S01;
245600110804         V1Hkey   = $Abc(yy);
245700110804         V1Hpgm   = $Pgm(yy);
245800110804         V1Hipers = $Ipers(yy);
245900110804         V1Sdes   = $Des(yy);
246000160609         clear V1Hnota;
246100111028         clear V1Saut;
246200111028         clear TIBS02ds;
246300111028         T02mod = 'C';
246400111028         T02cod = 'RAC';
246500111028         T02ke1 = $Aut(yy);
246600111028         T02sif = KNSIF;
246700111028         TNTBE_RicercaControllo  (kpjba : tibs02ds);
246800111028         IF  T02err = *blanks;
246900111028           V1Saut = T02Uni;
247000111028         ENDIF;
247100110802
247200110802       ENDSR ;
247300110809
247400110809       //--------------------------------------------------------------
247500110809       //?Controllo abilitazioni VAB
247600110809       //--------------------------------------------------------------
247700110809       BEGSR Abi_VAB;
247800110809
247900110809       //?Sql diviso pe ambiente, prova o produzione
248000110809         IF  $Prova;
248100110809           exec sql
248200110809           SELECT distinct VIRksc into :VIRksc
248300110809           FROM GAITRAGRPS/TIVIR00F
248400110809           WHERE :wOggi between VIRdti and VIRdtf and
248500110809                 :wKscAlfa = substr(VIRksc, 2, 7) and
248600110907                 (VIRfit in('FIVABWWR', 'EDIVABWR') or
248700110809                 VIRtip in (SELECT VTFtip
248800110809                  FROM GAITRAGRPS/TIVTF00F
248900110809                  WHERE VTFud = :wUd and
249000110907                  VTFfit in('FIVABWWR', 'EDIVABWR')));
249100110809         ELSE;
249200110809           exec sql
249300110809           SELECT distinct VIRksc into :VIRksc
249400110809           FROM GAITRAGRU/TIVIR00F
249500110809           WHERE :wOggi between VIRdti and VIRdtf and
249600110809                 :wKscAlfa = substr(VIRksc, 2, 7) and
249700110907                 (VIRfit in('FIVABWWR', 'EDIVABWR') or
249800110809                 VIRtip in (SELECT VTFtip
249900110809                  FROM GAITRAGRU/TIVTF00F
250000110809                  WHERE VTFud = :wUd and
250100110907                  VTFfit in('FIVABWWR', 'EDIVABWR')));
250200110809         ENDIF;
250300110809
250400110809       //?Non trovo il codice pulisco il campo wKscAlfa
250500110809         IF sqlcod <> 0;
250600110809           clear wKscAlfa;
250700110809         ENDIF;
250800110809
250900110809       ENDSR;
251000110809
251100110809       //--------------------------------------------------------------
251200110809       //?Controllo abilitazioni VAS.
251300110809       //--------------------------------------------------------------
251400110809       BEGSR Abi_VAS;
251500110809
251600110809       //?Sql diviso pe ambiente, prova o produzione
251700110809         IF  $Prova;
251800110809           exec sql
251900110809           SELECT distinct VIRksc into :VIRksc
252000110809           FROM GAITRAGRPS/TIVIR00F
252100110809           WHERE :wOggi between VIRdti and VIRdtf and
252200110809                 :wKscAlfa = substr(VIRksc, 2, 7) and
252300110809                 (VIRtip = :wTip or VIRtip in
252400110809                 (SELECT VTFtip
252500110809                  FROM GAITRAGRPS/TIVTF00F
252600110809                  WHERE VTFud = :wUd and VTFtip = :wTip));
252700110809         ELSE;
252800110809           exec sql
252900110809           SELECT distinct VIRksc into :VIRksc
253000110809           FROM GAITRAGRU/TIVIR00F
253100110809           WHERE :wOggi between VIRdti and VIRdtf and
253200110809                 :wKscAlfa = substr(VIRksc, 2, 7) and
253300110809                 (VIRtip = :wTip or VIRtip in
253400110809                 (SELECT VTFtip
253500110809                  FROM GAITRAGRU/TIVTF00F
253600110809                  WHERE VTFud = :wUd and VTFtip = :wTip));
253700110809         ENDIF;
253800110809
253900110809       //?Non trovo il codice pulisco il campo wKscAlfa
254000110809         IF sqlcod <> 0;
254100110809           clear wKscAlfa;
254200110809         ENDIF;
254300110809
254400110809       ENDSR;
254500100720
254600100720       //--------------------------------------------------------------
254700110615       //?Gestione tasto funzionale F03 da videata D01/S01.
254800100720       //?F03=Fine
254900100720       //--------------------------------------------------------------
255000110615       BEGSR F03D01;
255100100720
255200110809       //?Chiusura del programma
255300100720         $Fine = *on;
255400100720
255500100720       ENDSR;
255600110617
255700110617       //--------------------------------------------------------------
255800110617       //?Gestione tasto funzionale F12 da videata S01.
255900110617       //?F12=Ritorno
256000110617       //--------------------------------------------------------------
256100110617       BEGSR F12S01;
256200110617
256300110617         //?Chiusura del programma se pgm richiamato
256400110617         IF  $Richiamato;
256500110617           $Fine = *on;
256600110617           leavesr;
256700110617         ENDIF;
256800110617
256900110617         //?Ritorno alla prima videata se pgm NON richiamato
257000110617         IF  not $Richiamato;
257100110617           $Video  = 'D1';
257200110617           $InzD01 = *on;
257300110617           leavesr;
257400110617         ENDIF;
257500110617
257600110617       ENDSR;
257700100622
257800100622       //--------------------------------------------------------------
257900100622       //?Gestione opzioni subfile
258000100622       //--------------------------------------------------------------
258100100622       BEGSR OpzS01;
258200100720
258300110615         readc TA61S01;
258400100622
258500110615         DOW  NOT  %eof(TNTA61D);
258600100622
258700100722           SflNxtChg = *off;
258800100622           WindDspF  = IndDspF;
258900100622           reset WindDspFb;
259000100622           IndDspF   = WindDspF;
259100110615           V1Crcd    = S01nrr;
259200110307
259300110307           PosCurOpz = *off;
259400100622
259500100622           SELECT;
259600100622
259700100624             //?- Nessuna opzione
259800110615             WHEN  V1sopz  = *blank;
259900110411
260000110411             //?- 5 = Visualizza
260100110615             WHEN  V1Sopz  = '5';
260200110615               //?richiamo pgm dettaglio
260300110804               exsr Visualizza;
260400100622
260500100624             //?- ? = Opzione NON valida
260600100622             OTHER;
260700100624               ErrMessage  = *on;
260800100624               ErrGenerico = *on;
260900100622               PosCurOpz   = *on;
261000110615               V1Cmsg = $Msg(02);
261100100622
261200100622           ENDSL;
261300100622
261400100624           //?Aggiornamento sfl
261500100624           SELECT;
261600100624             WHEN  ErrMessage;
261700100622               SflNxtChg = *on;
261800110615               V1Ccsr    = V1Crcd;
261900100624             WHEN  ErrGenerico;
262000110615               V1Ccsr    = V1Crcd;
262100110615               clear  V1Sopz;
262200100624             OTHER;
262300110615               V1Ccsr    = V1Crcd;
262400110615               clear  V1Sopz;
262500100624           ENDSL;
262600110802
262700110802           Proteggi = V1Hin40;
262800100622
262900110615           update TA61S01;
263000100622
263100100624           IF  ErrMessage or ErrGenerico;
263200100622             leave;
263300100624           ENDIF;
263400100622
263500110615           readc TA61S01;
263600100622
263700100622         ENDDO;
263800100622
263900100622       ENDSR;
264000110804
264100110804       //--------------------------------------------------------------
264200110804       //?Visualizzazione a dettaglio.
264300110804       //--------------------------------------------------------------
264400110804       BEGSR Visualizza;
264500160609
264600160609         SELECT;
264700160609       //?Visualizzazione in window
264800160609         WHEN  V1Hnota <> *blanks;
264900160609           $Video = 'W1';
265000160609           $InzW01 = *on;
265100110804       //?Visualizzazione standard
265200160609         WHEN  V1Hipers = *blanks;
265300110804           exsr Call_pgm;
265400110804       //?Visualizzazione personalizzata
265500160609         OTHER;
265600110804           exsr Call_pgm_ABC;
265700160609         ENDSL;
265800110804
265900110804       ENDSR;
266000100720
266100100720       //--------------------------------------------------------------
266200110802       //?Richiamo programma di interrogazione a dettaglio.
266300100720       //--------------------------------------------------------------
266400110802       BEGSR Call_pgm;
266500110802
266600111017         clear kpjbu;
266700110802         ParmPgm = V1Hpgm;
266800110803         tnta61c (kpjba:Tnta61ds:ParmPgm);
266900110804         IF  TA61err = *on;
267000110804           ErrMessage  = *on;
267100110804           ErrGenerico = *on;
267200110804           PosCurOpz   = *on;
267300110804           V1Cmsg = TA61msg;
267400110804         ENDIF;
267500100720
267600100720       ENDSR;
267700110804
267800110804       //--------------------------------------------------------------
267900110804       //?Richiamo programma personalizzato per abilitazione.
268000110804       //--------------------------------------------------------------
268100110804       BEGSR Call_pgm_ABC;
268200110804
268300110808         SELECT;
268400110811
268500110808           WHEN  V1Hkey = '7U' and V1Hpgm = 'TB69';
268600110808             clear ds7PQRS;
268700110808             ds7ric = 'S';
268800110808             ds7ges = 'V';
268900110808             ds7cod = V1Hpart;
269000110808             kpjbu = ds7PQRS;
269100110808             trtb69r (kpjba);
269200111017
269300110808           WHEN  V1Hkey = '7U' and V1Hpgm = 'TB70';
269400110808             clear ds7PQRS;
269500110808             ds7ric = 'S';
269600110808             ds7ges = 'V';
269700110808             ds7cod = V1Hpart;
269800110808             kpjbu = ds7PQRS;
269900110808             trtb70r (kpjba);
270000110811
270100110808         ENDSL;
270200110804
270300110804       ENDSR;
270400111027
270500111027       //--------------------------------------------------------------
270600111027       //?Stampa.
270700111027       //--------------------------------------------------------------
270800111027       BEGSR Stampa;
270900111027
271000111027       //?Richiamato con nuovo codice
271100111027         *in80 = TA61newksc > 0;
271200111027
271300111027       //?Apro il file di stampa
271400111027         open TNTA61P;
271500111027
271600111027         clear S01nrr;
271700111027         $End = *off;
271800121217
271900121217       //?Se il cliente non ha abiltiazioni attive
272000121217       //?e non sono passata dal subfile stampo una riga di avviso
272100121217         IF  $Video = 'ST' and sav_s01nrr = 0;
272200121217           clear V1Dksc;
272300121217           clear tibs69ds;
272400121217           I69kac = TA61ksc;
272500121217           TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
272600121217           V1Dksc = ACOrag;
272700121217           STPksc  = TA61ksc;
272800121217           STPrag  = V1Dksc;
272900121217           STPkscn = TA61newksc;
273000121217           write TA61testa;
273100121217           STPdes = 'Nessuna abilitazione presente';
273200121217           write TA61riga;
273300121217           $End = *on;
273400121217           $Testa = *on;
273500121217         ENDIF;
273600111027
273700111027       //?Leggo il subfile caricato
273800111027         DOW not $End;
273900111027           S01nrr += 1;
274000111027           chain S01nrr TA61S01;
274100111027           IF  not %found;
274200111027             $End = *on;
274300111027             leave;
274400111027           ENDIF;
274500111027
274600111027         //?Stampo la testata
274700111027           IF  not $Testa or *in90;
274800111104           //?Se non sono passata dal subfile cerco la ragione sociale
274900111104             IF  $Video = 'ST';
275000111104               clear V1Dksc;
275100111104               clear tibs69ds;
275200111104               I69kac = TA61ksc;
275300111104               TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
275400111104               V1Dksc = ACOrag;
275500111104             ENDIF;
275600111027             STPksc  = TA61ksc;
275700111027             STPrag  = V1Dksc;
275800111027             STPkscn = TA61newksc;
275900111027             write TA61testa;
276000111027             *in90 = *off;
276100111027             $Testa = *on;
276200111027           ENDIF;
276300111027
276400111027           STPdes = V1Sdes;
276500111027           STPaut = V1Saut;
276600111027           write TA61riga;
276700111027
276800111027           update TA61S01;
276900111027
277000111027         ENDDO;
277100111027
277200111027         //?Stampo la fine se ho stampato almeno la testata
277300111027         IF  $Testa;
277400111027           write TA61fine;
277500111027         ENDIF;
277600111027
277700111027         close TNTA61P;
277800111104         $Fine = *on;
277900111027
278000111027       ENDSR ;
278100080206
278200080206       //--------------------------------------------------------------
278300080206       //?Operazioni finali.
278400080206       //--------------------------------------------------------------
278500080206       BEGSR RoutEnd;
278600111026
278700111026       //?Se richiamato per controllo imposto il flag di ritorno
278800111026         IF  $Controlla;
278900111026           TA61flg = $RecOK;
279000111026         ENDIF;
279100080206
279200080206         *inLR = *on;
279300080206         return;
279400080206
279500080206       ENDSR;
279600080206
279700080206      /end-free
279800100624
279900080206       //--------------------------------------------------------------
280000080206       //?Schiere a tempo di compilazione.
280100080206       //--------------------------------------------------------------
280200080206
280300080206** - $MSG -------------------------------------------------------------------*
280400110615Codice cliente errato o non in gestione all'utente.                            1
280500110411Opzione errata                                                                 2
