000100080206      //--------------------------------------------------------------
000200110615      //?TNTA61R - INTERROGAZIONE ABILITAZIONI CLIENTI
000300080206      //--------------------------------------------------------------
000400080206
000500090407     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600090601     h dftactgrp(*no) actgrp(*caller)
000700080206
000800080206      //---------------------------------------------------------------
000900080206      //?Dichiarazione file.
001000080206      //---------------------------------------------------------------
001100110802
001200110802      // - File Anagrafiche
001300110803     fCNIND00F  if   e           k disk
001400110802     fCNCLP00F  if   e           k disk
001500151116     fFNCLS01L  if   e           k disk
001600110803
001700110803      // - File Conto Economico
001800110803     fECEVD02L  if   e           k disk
001900140428      // - Contratti Comodato
002000140428     fUNCMD01l  if   e           k disk
002100160125      // - Bolle in sospensione
002200160125     fFIBSP02L  if   e           k disk
002300100624
002400110615      // - File Tabelle
002500110801     fTABEL00F  if   e           k disk    usropn
002600110808     f                                     extfile(wLibTab)
002700110801     fTNTBE01L  if   e           k disk    usropn
002800110808     f                                     extfile(wLibTbe)
002900110411
003000110615      // - Video
003100110615     fTNTA61D   cf   e             workstn
003200110615     f                                     sfile(TA61S01 : S01nrr)
003300080208     f                                     indds(IndDspF)
003400080206     f                                     infds(InfDspF)
003500111027
003600111027      // - Stampa
003700111027     fTNTA61P   o    e             printer oflind(*in90) usropn
003800080206
003900080206      //---------------------------------------------------------------
004000090406      //?Definizione costanti.
004100080206      //---------------------------------------------------------------
004200080207
004300080207      // - Tasti funzionali a video
004400080207     d c_F01           c                   const(x'31')
004500080207     d c_F02           c                   const(x'32')
004600080207     d c_F03           c                   const(x'33')
004700090406     d c_F04           c                   const(x'34')
004800080207     d c_F05           c                   const(x'35')
004900080207     d c_F06           c                   const(x'36')
005000080207     d c_F07           c                   const(x'37')
005100080207     d c_F08           c                   const(x'38')
005200080207     d c_F09           c                   const(x'39')
005300080207     d c_F10           c                   const(x'3A')
005400090303     d c_F11           c                   const(x'3B')
005500080207     d c_F12           c                   const(x'3C')
005600080207     d c_F13           c                   const(x'B1')
005700080207     d c_F14           c                   const(x'B2')
005800080207     d c_F15           c                   const(x'B3')
005900080207     d c_F16           c                   const(x'B4')
006000080207     d c_F17           c                   const(x'B5')
006100080207     d c_F18           c                   const(x'B6')
006200080207     d c_F19           c                   const(x'B7')
006300080207     d c_F20           c                   const(x'B8')
006400080207     d c_F21           c                   const(x'B9')
006500080207     d c_F22           c                   const(x'BA')
006600080207     d c_F23           c                   const(x'BB')
006700080207     d c_F24           c                   const(x'BC')
006800080207     d c_Enter         c                   const(x'F1')
006900080207     d c_RollDown      c                   const(x'F4')
007000080207     d c_RollUp        c                   const(x'F5')
007100100211
007200100211     d Digits          c                   const('0123456789')
007300080206
007400080206      //---------------------------------------------------------------
007500080206      //?Definizione schiere.
007600080206      //---------------------------------------------------------------
007700110615
007800110615      // - Sk per Tabella ABC
007900110615     d $Abc            s             15    dim(999)
008000111028     d $Aut            s              1    dim(999)
008100110615     d $Des            s             60    dim(999)
008200110615     d $File           s             10    dim(999)
008300111107     d $Files          s              1    dim(999)
008400111107     d $Ipers          s              1    dim(999)
008500110615     d $Ord            s              3s 0 dim(999)
008600110615     d $Ordw           s              3s 0 dim(999)
008700110615     d $Pgm            s             10    dim(999)
008800111107     d $Rpers          s              1    dim(999)
008900111107
009000111107      // - Sk per Tabella 7R
009100111107     d $7R             s              2    dim(999)
009200111107     d $7Raut          s              1    dim(999)
009300111107
009400111107      // - Sk per Tabella 7W
009500111107     d $7W             s              2    dim(999)
009600111107     d $7Waut          s              1    dim(999)
009700120227
009800120227       // -?Schiere per gestione codici cliente di ritorno da XCLIR
009900120227     d ksa             s              4    dim(30)
010000120227     d ksc             s              7  0 dim(30)
010100110805
010200080206      // - Messaggi di errore
010300100408     d $Msg            s             78    dim(20) ctdata perrcd(1)
010400080206
010500080206      //---------------------------------------------------------------
010600080206      //?Definizione aree dati.
010700080206      //---------------------------------------------------------------
010800080206
010900080206      // - Dati utente
011000080206     d §AzUte        e ds                  extname(AZUTE00F)
011100080206     d                                     dtaara
011200080206     d §DatiUte      e ds                  extname(dDatiUte)
011300080206     d                                     dtaara
011400080206
011500080206      //---------------------------------------------------------------
011600080206      //?Definizione strutture dati.
011700080206      //---------------------------------------------------------------
011800080206
011900080206      // - Status
012000080206     d Psds           sds
012100080206     d   SDSpgm          *proc
012200080206
012300080206      // - InfDS
012400080206     d InfDspF         ds
012500080207     d  dsp_aid              369    369a
012600080207     d  sfl_rrn              376    377i 0
012700080207     d  min_nrr              378    379i 0
012800080207     d  num_rcds             380    381i 0
012900080206
013000080206      // - Indicatori su DspF
013100080208     d IndDspF         ds
013200110722        // - Indicatori di attivazione tasti funzione
013300110722     d  F03Attivo                     1n   overlay(IndDspF : 03)
013400110722     d  F12Attivo                     1n   overlay(IndDspF : 12)
013500100720        // - Indicatori di errore in videata
013600100720     d  ErrMessage                    1n   overlay(IndDspF : 28)
013700080206        // - Indicatori di gestione del subfile
013800080206     d  SflDsp_N                      1n   overlay(IndDspF : 30)
013900080208     d  SflDspCtl_N                   1n   overlay(IndDspF : 31)
014000080206     d  SflNxtChg                     1n   overlay(IndDspF : 32)
014100080206     d  SflEnd                        1n   overlay(IndDspF : 33)
014200110722        // - Indicatore di protezione campi
014300110722     d  Proteggi                       n   overlay(IndDspF : 40)
014400100624        // - Indicatori di errore
014500100624     d  PosCurOpz                     1n   overlay(IndDspF : 50)
014600110615     d  PosCurKsc                     1n   overlay(IndDspF : 51)
014700090422        // - Indicatori di errore generico
014800100719     d  ErrGenerico                   1n   overlay(IndDspF : 99)
014900090407
015000090407     d WindDspF        ds                  inz
015100090407     d   WindDspFa             1     49    inz(*zero)
015200090407     d   WindDspFb            50     99    inz(*zero)
015300120227
015400120227       // -?ds x ricerca clienti
015500120227     d                 ds
015600120227     d dsksn                   1      4p 0
015700120227     d dsksa                   1      4
015800120621
015900120621       // -?ds x tabella FLB
016000120621     d wFLBuni         ds
016100120621     d  skksc                  1    252    dim(36)
016200080206
016300080206      // - Parametri ricevuti
016400080206     d KPJBA         e ds
016500110615     d TNTA61DS      e ds
016600130729
016700130729       // -?ds x pgm FNLV55R
016800130729     d FNLV55DS      e ds                  inz
016900111028
017000111028      // - Ricerca/Controllo tabelle
017100111028     d TIBS02ds      e ds                  inz
017200080206
017300080206      // - Reperimento dati utente
017400080206     d TIBS34ds      e ds
017500110615
017600110615      // - Controllo abilitazioni
017700110615     d TNTAA1DS      e ds                  inz
017800120227
017900120227       // -?Ricerca clienti
018000120227     d xCLIrds       e ds
018100110616
018200110616      // - Tabella ABC
018300110616     d dABC          e ds                  inz
018400110809
018500110809      // - Tabella CLI
018600110809     d dCLI          e ds                  inz
018700110803
018800110803      // - Tabella DCC
018900110803     d dDCC          e ds                  inz
019000110913
019100120604      // - Tabella JDC
019200120604     d dJDC          e ds                  inz
019300120604
019400110913      // - Tabella LAC
019500110913     d dLAC          e ds                  inz
019600130906
019700130906      // - Tabella LCA
019800130906     d dLCA          e ds                  inz
019900110805
020000110805      // - Tabella LLT
020100110805     d dLLT          e ds                  inz
020200110809
020300110809      // - Tabella 3C
020400110809     d ds3C          e ds                  inz
020500110809
020600110809      // - Tabella 3K
020700110809     d ds3K          e ds                  inz
020800110617
020900110809      // - Tabella 3Q
021000110809     d ds3Q          e ds                  inz
021100110809
021200110809      // - Tabella 3R
021300110809     d ds3R          e ds                  inz
021400110809
021500110809      // - Tabella 4C
021600110809     d ds4C          e ds                  inz
021700110808
021800110808      // - Tabella 7x --> Particolarità P-Q-R-S
021900110808     d ds7PQRS       e ds                  inz
022000111107
022100111107      // - Tabella 7R
022200111107     d ds7R          e ds                  inz
022300110805
022400110805      // - Tabella 7U
022500110805     d ds7U          e ds                  inz
022600110808     d  $Part                  1     60    dim(30)
022700111107
022800111107      // - Tabella 7W
022900111107     d ds7W          e ds                  inz
023000110809
023100110809      // - File TIVIR00F
023200110809     d TIVIR00F      e ds
023300110809
023400110809      // - File TIVTF00F
023500110809     d TIVTF00F      e ds
023600160304
023700160304      // - File ANRCO00F
023800160304     d ANRCO00F      e ds
023900100428
024000080206      //---------------------------------------------------------------
024100080206      //?Definizione variabili globali.
024200080206      //---------------------------------------------------------------
024300080206
024400080206      // - Flags booleani
024500120227     d wDaMenu         s               n   inz(*off)
024600120227     d wErrGrave       s               n   inz(*off)
024700130729     d $Bsp            s               n   inz(*off)
024800110927     d $Cli            s               n   inz(*off)
024900110615     d $Controlla      s               n   inz(*off)
025000110809     d $End            s               n   inz(*off)
025100100720     d $Fine           s               n   inz(*off)
025200110615     d $InzD01         s               n   inz(*on)
025300100720     d $InzS01         s               n   inz(*on)
025400110808     d $Prova          s               n   inz(*off)
025500080208     d $RecOK          s               n   inz(*off)
025600110615     d $Richiamato     s               n   inz(*off)
025700110808     d $Sede           s               n   inz(*off)
025800110907     d $SedeUte        s               n   inz(*off)
025900111027     d $Stampa         s               n   inz(*off)
026000111027     d $Testa          s               n   inz(*off)
026100080206
026200080206      // - Indici di schiera
026300111107     d ww              s              4  0 inz
026400100312     d xx              s              4  0 inz
026500110615     d yy              s              4  0 inz
026600110808     d zz              s              4  0 inz
026700080206
026800080207      // - Campi associati al video
026900100719     d $Video          s              2    inz('S1')
027000080208     d S01nrr          s              4  0 inz
027100110801
027200110801      // - Campi per richiamo QCMDEXC
027300110801     d Qcmd            s            128    inz
027400110802
027500110802      // - Stringa SQL da eseguire
027600110802     d wSQL            s           2048    Varying        inz
027700110808
027800110808      // - Nome libreria file tabella
027900110808     d wLibTab         s             21a   inz
028000110808     d wLibTbe         s             21a   inz
028100110809     d wLibVir         s             21a   inz
028200110809     d wLibVtf         s             21a   inz
028300090401
028400080206      // - Campi di comodo
028500110616     d C_ke1           s                   like(TBEke1)
028600130729     d C_ke2           s                   like(TBEke2)
028700110616     d C_tab           s                   like(TBEcod)
028800110802     d ParmPgm         s             10
028900111107     d sav_AUT         s                   like(d§ABCaut)
029000121217     d sav_s01nrr      s                   like(s01nrr)
029100150513     d wCompCE         s              5s 3 inz
029200110809     d wOggi           s              8s 0 inz
029300110809     d wKscAlfa        s              7a   inz
029400110809     d wTip            s                   like(VIRtip)
029500110809     d wUd             s                   like(VTFud)
029600120227     d wUteAbi         s                   like(OTAA1cabi)
029700160304     d wKscAlfa8       s              8a   inz
029800160304     d wKscFact        s              8a   inz
029900080208
030000090508      //---------------------------------------------------------------
030100090508      //?Definizione procedure usate.
030200090508      //---------------------------------------------------------------
030300110801
030400110801       // - Dettaglio abilitazioni
030500110801     d tnta61c         pr                  extpgm('TNTA61C')
030600110801     d  kpjba                              likeds(KPJBA)    const
030700110802     d  Tnta61ds                           likeds(TNTA61DS)
030800110801     d  ParmPgm                      10
030900110808
031000110808       // - Interrogazione particolarità giacenza
031100110808     d trtb69r         pr                  extpgm('TRTB69R')
031200110808     d  kpjba                              likeds(KPJBA)    const
031300110808
031400110808       // - Interrogazione particolarità consegna
031500110808     d trtb70r         pr                  extpgm('TRTB70R')
031600110808     d  kpjba                              likeds(KPJBA)    const
031700120227
031800120227       // -?Ricerca cliente
031900120227     d xclir           pr                  extpgm('XCLIR')
032000120227     d  xclirds                            likeds(XCLIRDS)
032100080206
032200080626      //---------------------------------------------------------------
032300080626      //?prototipi
032400080626      //---------------------------------------------------------------
032500111027
032600130729      /copy gaitrasrc/srcprotopr,fnlv55r
032700111028      /copy gaitrasrc/srcprotopr,tibs02r
032800080626      /copy gaitrasrc/srcprotopr,tibs34r
032900110812      /copy gaitrasrc/srcprotopi,tibs69r
033000110615      /copy gaitrasrc/srcprotopr,tibs69r
033100110615      /copy gaitrasrc/srcprotopr,tntaa1c
033200110801
033300110801       // - Esecuzione comando di sistema
033400110801      /copy gaitrasrc/srcProtoPR,QCMDEXC
033500100624
033600080206      //---------------------------------------------------------------
033700080206      //?Definizione key-list.
033800080206      //---------------------------------------------------------------
033900100720
034000100720       // - File TABEL00F
034100100720     d k03tabel      e ds                  extname(TABEL00F:*key)
034200100720     d                                     prefix(k_)
034300111027
034400111027      //---------------------------------------------------------------
034500111027      //?Indicatori per la stampa.
034600111027      //---------------------------------------------------------------
034700111027
034800111027      * 80 - Stampo nuovo codice cliente quando richiamato da duplica
034900111027      * 90 - Salto pagina
035000080206
035100080206      //---------------------------------------------------------------
035200080206
035300080206      //---------------------------------------------------------------
035400080206      //?M A I N - L I N E
035500080206      //---------------------------------------------------------------
035600080206
035700080206     c     *Entry        plist
035800100720     c                   parm                    kpjba
035900110615     c                   parm                    tnta61ds
036000080206
036100080206      /free
036200080206
036300100622       //?Operazioni iniziali
036400080206       exsr RoutInz;
036500080206
036600100622       //?Gestione video
036700080206       DOW $Fine = *off;
036800100624         SELECT;
036900110615           WHEN $Video = 'D1';
037000110615             exsr GesD01;
037100100719           WHEN $Video = 'S1';
037200100719             exsr GesS01;
037300111027           WHEN $Video = 'ST';
037400111027             exsr Stampa;
037500100624           OTHER;
037600080206             $Fine = *on;
037700100624         ENDSL;
037800080206       ENDDO;
037900080206
038000100622       //?Operazioni finali
038100080206       exsr RoutEnd;
038200080206
038300080206       //--------------------------------------------------------------
038400080206       //?Operazioni iniziali.
038500080206       //--------------------------------------------------------------
038600080206       BEGSR RoutInz;
038700080206
038800100622       //?Impostazione campi "fissi"
038900110411         VT1pgm = SDSpgm;
039000110616         VT1tit = '* ABILITAZIONI CLIENTE *';
039100110616         k_TBLkut = 1;
039200120227
039300120227       //?Controllo se pgm richiamato o se è da menù
039400120227         wDaMenu = (%subst(kpjbu:1:1) = 'M');
039500120227         IF wDaMenu;
039600120227           clear TNTA61DS;
039700120227         ENDIF;
039800110615
039900110615       //?Controllo se pgm richiamato o se è da menù
040000120227         IF  %parms > 1 and not wDaMenu;
040100110615           $Richiamato = *on;
040200110617           F03Attivo = *off;
040300110617           F12Attivo = *on;
040400111026           $Video = 'S1';
040500111026           $InzS01 = *on;
040600111026           IF  TA61tla = 'C';
040700111026             $Controlla = *on;
040800111026           ENDIF;
040900111027           IF  TA61tla = 'P';
041000111027             $Stampa = *on;
041100111027           ENDIF;
041200110615         ELSE;
041300110617           F03attivo = *on;
041400110617           F12Attivo = *on;
041500110616           $Video = 'D1';
041600110616           $InzD01 = *on;
041700110615         ENDIF;
041800120227
041900120227       //?Pulisco campi output
042000120227         IF  $Richiamato;
042100120227           clear TA61flg;
042200120227           clear TA61err;
042300120227           clear TA61msg;
042400120227         ENDIF;
042500080206
042600100622       //?Reperimento dati job
042700080206         exsr DatiJob;
042800110411
042900110615       //?Se non ho il codice cliente esco dal pgm con errore al chiamante
043000120227         IF  TA61ksc = 0 and $Richiamato;
043100110615           TA61err = '1';
043200110615           TA61msg = $Msg(01);
043300110411           $Fine = *on;
043400110809           leavesr;
043500110411         ENDIF;
043600110809
043700110809       //?Imposto la data del giorno
043800110809         wOggi = %dec(%date());
043900110808
044000110808       //?Imposto in che sistema informativo sono
044100110808       //?S.I. di Prova o S.I. di Produzione
044200110808         $Prova = (%subst(knsif : 7 : 1) = 'P');
044300110907
044400110907       //?Imposto se utente di sede
044500110907         $SedeUte = (DUTpou = 046);
044600110808
044700110808       //?Apro il file tabelle
044800110808         exsr OpenFileTbe;
044900110615
045000110615       //?Carico Sk tabella abilitazionei
045100110615         clear xx;
045200110616         C_tab = 'ABC';
045300110615         setll C_tab TNTBE01L;
045400110615         reade C_tab TNTBE01L;
045500110615
045600110615         DOW not %eof(TNTBE01L);
045700110615           IF  TBEatb = *blanks;
045800110615             xx += 1;
045900110722             dABC       = TBEuni;
046000110722             $Abc(xx)   = TBEke1;
046100110722             $Des(xx)   = d§ABCdes;
046200110722             $Aut(xx)   = d§ABCaut;
046300110722             $Ord(xx)   = d§ABCord;
046400110722             $Ordw(xx)  = d§ABCord;
046500110722             $File(xx)  = d§ABCfile;
046600110722             $Pgm(xx)   = d§ABCpgm;
046700110808             $Rpers(xx) = d§ABCrpers;
046800110808             $Ipers(xx) = d§ABCipers;
046900110808             $Files(xx) = d§ABCfiles;
047000110615           ENDIF;
047100110615           reade C_tab TNTBE01L;
047200110615         ENDDO;
047300110615
047400110615       //?Ordino la sk
047500110615         sorta $Ordw;
047600111107
047700111107       //?Apro il file tabelle
047800111107         exsr OpenFileTab;
047900111107
048000111107       //?Carico Sk tabella 7R + chi autorizza
048100111107         clear xx;
048200111107         k_TBLcod = '7R';
048300111107         setll (k_TBLkut:k_TBLcod) TABEL00F;
048400111107         reade (k_TBLkut:k_TBLcod) TABEL00F;
048500111107
048600111107         DOW not %eof(TABEL00F);
048700111107           IF  TBLflg = *blanks;
048800111107             xx += 1;
048900111107             ds7R       = TBLuni;
049000111107             $7R(xx)    = %subst(TBLkey:1:2);
049100111107             $7Raut(xx) = §7Raut;
049200111107           ENDIF;
049300111107           reade (k_TBLkut:k_TBLcod) TABEL00F;
049400111107         ENDDO;
049500111107
049600111107       //?Carico Sk tabella 7W + chi autorizza
049700111107         clear xx;
049800111107         k_TBLcod = '7W';
049900111107         setll (k_TBLkut:k_TBLcod) TABEL00F;
050000111107         reade (k_TBLkut:k_TBLcod) TABEL00F;
050100111107
050200111107         DOW not %eof(TABEL00F);
050300111107           IF  TBLflg = *blanks;
050400111107             xx += 1;
050500111107             ds7W       = TBLuni;
050600111107             $7W(xx)    = %subst(TBLkey:1:2);
050700111107             $7Waut(xx) = §7Waut;
050800111107           ENDIF;
050900111107           reade (k_TBLkut:k_TBLcod) TABEL00F;
051000111107         ENDDO;
051100120227
051200120227       //?Se da Menù cerco il codice abilitazione utente
051300120227         reset TNTAA1DS;
051400120227         ITAA1caut = '§utegtc';
051500120227         kpjbu     = TNTAA1DS;
051600120227         tntaa1c (kpjba);
051700120227         TNTAA1DS = kpjbu;
051800120227
051900120227         IF  OTAA1fabi = 'N';
052000120227           wErrGrave   = *on;
052100120227           ErrMessage  = *on;
052200120227           ErrGenerico = *on;
052300120227           V1Cmsg = $Msg(01);
052400120227           leavesr;
052500120227         ENDIF;
052600120227
052700120227         wUteAbi = OTAA1cabi;
052800130729
052900130729       //?Cerco il terminal della filiale del cliente
053000130729         IF  TA61ksc > 0;
053100130729           clear FNLV55DS;
053200130729           D55tpt = 'P';
053300130729           D55lin = %dec(%subst(%editc(TA61ksc:'X'):1:3):3:0);
053400130729           D55drf = %dec(%date());
053500130729           fnlv55r (fnlv55ds);
053600130729           IF  D55err <> *blanks;
053700130729             D55tfp = D55lin;
053800130729           ENDIF;
053900130729         ENDIF;
054000110802
054100110802         exec sql set option dynusrprf = *owner,
054200110802                             closqlcsr = *endmod;
054300090714
054400080206       ENDSR;
054500080206
054600080206       //--------------------------------------------------------------
054700080206       //?Reperimento Dati del job (Utente/Operativi).
054800080206       //--------------------------------------------------------------
054900080206       BEGSR DatiJob;
055000080206
055100080206         in(E) §AzUte;
055200110615         IF  not %error;
055300080206           in(E) §DatiUte;
055400110615         ENDIF;
055500110615         IF  %error or RSut = *blanks;
055600080206           clear TIBS34ds;
055700080206           tibs34r(tibs34ds);
055800080206           in §AzUte;
055900080206           in §DatiUte;
056000110615         ENDIF;
056100080206
056200080206       ENDSR;
056300110808
056400110808       //--------------------------------------------------------------
056500110808       //?Apro il file TNTBE00F.
056600110808       //--------------------------------------------------------------
056700110808       BEGSR OpenFileTbe;
056800110808
056900110808       //?Chiudo il file se aperto
057000110808         IF  %open(TNTBE01L);
057100110808           close TNTBE01L;
057200110808         ENDIF;
057300110808
057400110808       //?Apro il file
057500110808         SELECT;
057600110808
057700110907           WHEN  ($Sede or $SedeUte) and $Prova;
057800110808             wLibTbe = 'GAITRAGRPS/TNTBE01L';
057900110907           WHEN  ($Sede or $SedeUte) and not $Prova;
058000110808             wLibTbe = 'GAITRAGRU/TNTBE01L';
058100110907           WHEN  $Prova;
058200110808             wLibTbe = 'FILTRAGRPF/TNTBE01L';
058300110907           WHEN  not $Prova;
058400110808             wLibTbe = 'FILTRAGRU/TNTBE01L';
058500110808
058600110808         ENDSL;
058700110808
058800110808         open  TNTBE01L;
058900110808
059000110808       ENDSR;
059100110808
059200110808       //--------------------------------------------------------------
059300110808       //?Apro il file TABEL00F.
059400110808       //--------------------------------------------------------------
059500110808       BEGSR OpenFileTab;
059600110808
059700110808       //?Chiudo il file se aperto
059800110808         IF  %open(TABEL00F);
059900110808           close TABEL00F;
060000110808         ENDIF;
060100110808
060200110808       //?Apro il file
060300110808         SELECT;
060400110808
060500110907           WHEN  ($Sede or $SedeUte) and $Prova;
060600110808             wLibTab = 'GAITRAGRPS/TABEL00F';
060700110907           WHEN  ($Sede or $SedeUte) and not $Prova;
060800110808             wLibTab = 'GAITRAGRU/TABEL00F';
060900110907           WHEN  $Prova;
061000110808             wLibTab = 'FILTRAGRPF/TABEL00F';
061100110907           WHEN  not $Prova;
061200110808             wLibTab = 'FILTRAGRU/TABEL00F';
061300110808
061400110808         ENDSL;
061500110808
061600110808         open  TABEL00F;
061700110808
061800110808       ENDSR;
061900110615
062000110615       //--------------------------------------------------------------
062100110615       //?Gestione videata D01.
062200110615       //--------------------------------------------------------------
062300110615       BEGSR GesD01;
062400110615
062500110615       //?Inizializzazione videata
062600110615         IF  $InzD01;
062700110615           exsr InzD01;
062800110615           $InzD01  = *off;
062900110615         ENDIF;
063000110615
063100110615       //?Emissione Testata
063200110615         write  TA61T01;
063300110615
063400110615       //?Emissione videata
063500120227         exfmt  TA61D01;
063600110615         reset ErrMessage;
063700110615         reset ErrGenerico;
063800110615         clear V1Cmsg;
063900110615
064000110615         SELECT;
064100120227
064200120227       //?- Errore grave esco dal pgm
064300120227           WHEN  wErrGrave;
064400120227             $Fine = *on;
064500110615
064600110615       //?- F03=Fine
064700110615           WHEN  dsp_aid = c_F03;
064800110615             exsr F03D01;
064900110615
065000110615       //?Invio
065100110615           OTHER;
065200110615             exsr CtrD01;
065300110615             IF  ErrGenerico;
065400110615               leavesr;
065500110615             ENDIF;
065600120227             TA61ksc = %dec(V01ksc:7:0);
065700110615             $Video  = 'S1';
065800110615             $InzS01 = *on;
065900110615
066000110615         ENDSL;
066100110615
066200110615       ENDSR;
066300110615
066400110615       //--------------------------------------------------------------
066500110615       //?Inizializzazione videata D01.
066600110615       //--------------------------------------------------------------
066700110615       BEGSR InzD01;
066800110615
066900120227         clear V01ksc;
067000110615         clear V1Dksc;
067100110615
067200110615       ENDSR;
067300110615
067400110615       //--------------------------------------------------------------
067500110615       //?Controllo videata D01.
067600110615       //--------------------------------------------------------------
067700110615       BEGSR CtrD01;
067800120227
067900120227       //?Ricerca del cliente
068000120227         IF  %scan('?' : V01ksc) > 0;
068100120227           ErrGenerico = *on;
068200120227           PosCurKSC   = *on;
068300120227           clear xCLIrds;
068400120227           DXCaut = wUteAbi;
068500120227           DXCcap = DUTkci;
068600120227           xclir (xCLIrds);
068700120227           ksa = DXCksc;
068800120227           dsksa = ksa(1);
068900120227           ksc(1) = dsksn;
069000120227           V01ksc = %editc(ksc(1):'X');
069100120227         ENDIF;
069200110615
069300110615       //?Cliente obbligatorio
069400120227         IF  V01ksc = *zeros or V01ksc = *blanks;
069500110615           ErrMessage  = *on;
069600110615           ErrGenerico = *on;
069700110615           PosCurKsc   = *on;
069800110615           V1Cmsg = $Msg(01);
069900110615           leavesr;
070000110615         ENDIF;
070100120227
070200120227       //?Solo valori numerici
070300120227         IF  %check(digits:V01ksc) > 0;
070400120227           ErrMessage  = *on;
070500120227           ErrGenerico = *on;
070600120227           PosCurKSC   = *on;
070700120227           V1Cmsg      = $Msg(01);
070800120227           leavesr;
070900120227         ENDIF;
071000110615
071100110615       //?Cliente valido
071200110615         clear tibs69ds;
071300120227         I69kac = %dec(V01ksc:7:0);
071400110615         TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
071500110615         IF  O69err <> *blanks;
071600110615           ErrMessage  = *on;
071700110615           ErrGenerico = *on;
071800110615           PosCurKsc   = *on;
071900110615           V1Cmsg = $Msg(01);
072000110615           leavesr;
072100110615         ENDIF;
072200110615         V1Dksc = ACOrag;
072300110615
072400110615       //?Utente abilitato al cliente
072500110615         clear TNTAA1DS;
072600110615         ITAA1caut = '§utecli';
072700120227         ITAA1ksc  = %dec(V01ksc:7:0);
072800110615         kpjbu     = tntaa1ds;
072900110615         tntaa1c (kpjba);
073000110615         TNTAA1DS = kpjbu;
073100110615         IF  OTAA1fabi = 'N';
073200110615           ErrMessage  = *on;
073300110615           ErrGenerico = *on;
073400110615           PosCurKsc   = *on;
073500110615           V1Cmsg      = $Msg(01);
073600110615           leavesr;
073700110615         ENDIF;
073800110615
073900110615       ENDSR;
074000100622
074100100622       //--------------------------------------------------------------
074200100622       //?Gestione videata S01.
074300100622       //--------------------------------------------------------------
074400100622       BEGSR GesS01;
074500100622
074600100622       //?Inizializzazione videata
074700100622         IF  $InzS01;
074800100622           exsr InzS01;
074900100622           $InzS01  = *off;
075000130729           $Bsp = *off;
075100110927           $Cli = *off;
075200100622         ENDIF;
075300111026
075400111026       //?Se richiamato per controllo non emetto il subfile
075500111026       //?ma vado a fine
075600111026         IF  $Controlla;
075700111026           $Fine = *on;
075800111026           leavesr;
075900111026         ENDIF;
076000111027
076100111027       //?Se richiamato per stampa non emetto il subfile
076200111027       //?ma vado nella routine di stampa
076300111027         IF  $Stampa;
076400111027           $Video  = 'ST';
076500111027           leavesr;
076600111027         ENDIF;
076700110511
076800110511       //?Visualizzazione del SFL (se ci sono dati)
076900110511         SflDsp_N = (S01nrr <= *zeros);
077000110511
077100110511       //?Posizionamento cursore al 1° rcd della pagina
077200110809         IF  V1Ccsr > *zero;
077300110809           V1Crcd = V1Ccsr;
077400110511         ELSE;
077500110809           V1Crcd = 1;
077600110511         ENDIF;
077700110511
077800110615         V1Ccsr = V1Crcd;
077900110411
078000110722       //?Imposto cliente in testata subfile
078100110615         IF  $Richiamato;
078200110615           V1Cksc = TA61ksc;
078300110615           clear V1Dksc;
078400110615           clear tibs69ds;
078500110615           I69kac = V1Cksc;
078600110615           TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
078700110615           V1Dksc = ACOrag;
078800110615         ENDIF;
078900100622
079000100622       //?Emissione Testata e Piede con tasti funzionali abilitati
079100110615         write  TA61T01;
079200110615         write  TA61Z01;
079300100622
079400100622       //?Emissione videata
079500110615         exfmt  TA61C01;
079600100622         reset ErrMessage;
079700100622         reset ErrGenerico;
079800110615         clear V1Cmsg;
079900100622
080000100622         SELECT;
080100100719
080200100719       //?- F03=Fine
080300100719           WHEN  dsp_aid = c_F03;
080400110615             exsr F03D01;
080500111027
080600111027       //?- F08=Stampa
080700121204           WHEN  dsp_aid = c_F08 and S01nrr > 0;
080800111027             exsr Stampa;
080900110617
081000110617       //?- F12=Ritorno
081100110617           WHEN  dsp_aid = c_F12;
081200110617             exsr F12S01;
081300100622
081400100622       //?Invio
081500100622           OTHER;
081600121010             IF  S01nrr = 0;
081700121010               leavesr;
081800121010             ENDIF;
081900110615             IF  V1Ccsr > *zero;
082000110412               exsr OpzS01;
082100110412               IF  ErrGenerico;
082200110412                 leavesr;
082300110412               ENDIF;
082400110412             ENDIF;
082500100622
082600100622         ENDSL;
082700100622
082800100622       ENDSR;
082900100622
083000100622       //--------------------------------------------------------------
083100100622       //?Inizializzazione videata S01.
083200100622       //--------------------------------------------------------------
083300100622       BEGSR InzS01;
083400100622
083500111026       //?Pulizia subfile se NON richiamato per controllo
083600111026         IF  not $Controlla;
083700111026           exsr PulS01;
083800111026         ENDIF;
083900110411
084000110511       //?Caricamento subfile
084100111026       //?sempre perchè nella routine faccio già i controlli se
084200111026       //?scrivere o meno il subfile
084300110511         exsr Ries01;
084400121217
084500121217         sav_s01nrr = S01nrr;
084600110511
084700110511         SflEnd = *on;
084800100622
084900100622       ENDSR;
085000100624
085100100624       //--------------------------------------------------------------
085200100624       //?Pulizia SFL01.
085300100624       //--------------------------------------------------------------
085400100624       BEGSR PulS01;
085500100624
085600100624       //?Pulizia subfile
085700100624         SflDsp_N    = *on;
085800100624         SflDspCtl_N = *on;
085900110615         write  TA61C01;
086000100624         SflDspCtl_N = *off;
086100100624         SflEnd      = *off;
086200100624
086300110615         clear V1Crcd;
086400110615         clear V1Ccsr;
086500100624         clear S01nrr;
086600110615         clear V1Cmsg;
086700100720
086800100624         ErrMessage  = *off;
086900100624         ErrGenerico = *off;
087000100720
087100100624         WindDspF    = IndDspF;
087200100624         reset WindDspFb;
087300100624         IndDspF     = WindDspF;
087400100624
087500100624       ENDSR ;
087600100622
087700100622       //--------------------------------------------------------------
087800110615       //?Riempimento S01.
087900100622       //--------------------------------------------------------------
088000100622       BEGSR RieS01;
088100110511
088200110615       //?Carico in base alla tabella (ordinata)
088300110615         xx = 1;
088400110615         FOR xx by 1 to %elem($Ordw);
088500110615
088600110615           IF  $Ordw(xx) > 0;
088700110615             yy = %lookup($Ordw(xx) : $Ord);
088800110615             IF  yy > 0;
088900110808               exsr CarS01;
089000111026           //?Se richiamato per controllo
089100111026           //?ed ho trovato almeno 1 abilitazione
089200111026           //?esco dal ciclo
089300111026               IF  $Controlla and $RecOK;
089400111026                 leavesr;
089500111026               ENDIF;
089600110615             ENDIF;
089700110615           ENDIF;
089800110616
089900110615         ENDFOR;
090000110615
090100110615       ENDSR ;
090200110808
090300110808       //--------------------------------------------------------------
090400110808       //?Caricamento S01.
090500110808       //--------------------------------------------------------------
090600110808       BEGSR CarS01;
090700110808
090800110616         $RecOK = *off;
090900110808
091000110808       //?Imposto se leggere file di sede o da S.I.
091100110808         $Sede = ($Files(yy) = 'S');
091200110616
091300110616       //?Controllo se il cliente ha l'abilitazione che sto leggendo
091400110616         SELECT;
091500110722         //?Ricerca personalizzata x key ABC
091600110804           WHEN  $Rpers(yy) = 'S';
091700110722             exsr da_ABC;
091800110616         //?Da TABEL00F
091900110616           WHEN  $File(yy) = 'TABEL00F';
092000110616             exsr da_TABEL;
092100110616         //?Da TNTBE00F
092200110616           WHEN  $File(yy) = 'TNTBE00F';
092300110616             exsr da_TNTBE;
092400110616         ENDSL;
092500110808
092600110808       //?Se il cliente è abilitato scrivo il subfile
092700111026       //?ma solo se NON richiamato per controllo
092800111026         IF  $Recok and not $Controlla;
092900110808           exsr WrtS01;
093000110808         ENDIF;
093100110808
093200110808       ENDSR ;
093300110808
093400110808       //--------------------------------------------------------------
093500110808       //?Scrivo il subfile.
093600110808       //--------------------------------------------------------------
093700110808       BEGSR WrtS01;
093800110808
093900110808         PosCurOpz = *off;
094000110808
094100110808       //?Se non c'è il pgm di interrogazione proteggo il campo opzione
094200110808         IF  V1Hpgm = *blanks;
094300110808           Proteggi = *on;
094400110808         ELSE;
094500110808           Proteggi = *off;
094600110808         ENDIF;
094700110808
094800110808         V1Hin40 = Proteggi;
094900110808
095000110808         S01nrr += 1;
095100110808         write  TA61S01;
095200110615
095300111027       ENDSR;
095400110722
095500110722       //--------------------------------------------------------------
095600110722       //?Ricerca personalizzata.
095700110722       //--------------------------------------------------------------
095800110722       BEGSR da_ABC;
095900110927
096000110927       //?Tabella CLI l'aggancio solo una volta
096100130729           IF  %subst($Abc(yy):1:3) = 'CLI' and not $Cli;
096200130729             clear dCLI;
096300110927         //?Apro il file
096400130729             exsr OpenFileTbe;
096500130729             C_tab = %subst($Abc(yy):1:3);
096600130729             C_ke1 = %editc(TA61ksc:'X');
096700130729             chain (C_tab : C_ke1) TNTBE01L;
096800130729             IF  %found(TNTBE01L) and TBEatb = *blanks;
096900130729               dCLI = TBEuni;
097000130729               $Cli = *on;
097100130729             ENDIF;
097200130729           ENDIF;
097300130729
097400160125       //?File FIBSP lo aggancio solo una volta
097500130729           IF  %subst($Abc(yy):1:3) = 'BSP' and not $Bsp;
097600160125             chain (TA61ksc) FIBSP02L;
097700160125             IF  %found(FIBSP02L);
097800130729               $Bsp = *on;
097900130729             ENDIF;
098000130729           ENDIF;
098100110722
098200110722       //?Personalizzato
098300110722         SELECT;
098400110802           WHEN  $Abc(yy) = 'FATTURAZIONE';
098500110802             exsr Fat;
098600110802           WHEN  $Abc(yy) = 'PAG.C/A';
098700110802             exsr PagCA;
098800151116           WHEN  $Abc(yy) = 'PAG.C/A ASS';
098900151116             exsr PagCAASS;
099000151116           WHEN  $Abc(yy) = 'PAG.DANNI';
099100151116             exsr PagDN;
099200151116           WHEN  $Abc(yy) = 'PAG.DANNI ASS';
099300151116             exsr PagDNASS;
099400151116           WHEN  $Abc(yy) = 'PAG.N.ACC.';
099500151116             exsr PagNA;
099600151116           WHEN  $Abc(yy) = 'PAG.N.ACC. ASS';
099700151116             exsr PagNAASS;
099800160304           WHEN  $Abc(yy) = 'C/ASSEGNI';
099900160304             exsr CAssegni;
100000110803           WHEN  $Abc(yy) = 'CONTO ECONOMICO';
100100110803             exsr ContoEco;
100200110803           WHEN  $Abc(yy) = 'ESENZIONE IVA';
100300110803             exsr EsIVA;
100400110809           WHEN  $Abc(yy) = 'ORM';
100500110809             exsr OrmVao;
100600110926           WHEN  $Abc(yy) = 'CLI LDV';
100700110926             exsr tabCLILDV;
100800110926           WHEN  $Abc(yy) = 'CLI DPD';
100900110926             exsr tabCLIDPD;
101000110926           WHEN  $Abc(yy) = 'CLI LNA';
101100110926             exsr tabCLILNA;
101200110926           WHEN  $Abc(yy) = 'CLI LVP';
101300110926             exsr tabCLILVP;
101400110926           WHEN  $Abc(yy) = 'CLI DKB';
101500110926             exsr tabCLIDKB;
101600110926           WHEN  $Abc(yy) = 'CLI DCR';
101700110926             exsr tabCLIDCR;
101800110926           WHEN  $Abc(yy) = 'CLI GCP';
101900110926             exsr tabCLIGCP;
102000110926           WHEN  $Abc(yy) = 'CLI BRD';
102100110926             exsr tabCLIBRD;
102200110926           WHEN  $Abc(yy) = 'CLI BAN';
102300110926             exsr tabCLIBAN;
102400120620           WHEN  $Abc(yy) = 'CLI EMD';
102500120620             exsr tabCLIEMD;
102600130321           WHEN  $Abc(yy) = 'CLI PDF';
102700130321             exsr tabCLIPDF;
102800131018           WHEN  $Abc(yy) = 'CLI PSM';
102900131018             exsr tabCLIPSM;
103000151109           WHEN  $Abc(yy) = 'CLI EMDP';
103100151109             exsr tabCLIEMDP;
103200110803           WHEN  $Abc(yy) = 'DCC';
103300110803             exsr tabDCC;
103400120621           WHEN  $Abc(yy) = 'FLB';
103500120621             exsr tabFLB;
103600120604           WHEN  $Abc(yy) = 'JDC';
103700120604             exsr tabJDC;
103800110913           WHEN  $Abc(yy) = 'LAC';
103900110913             exsr tabLAC;
104000130906           WHEN  $Abc(yy) = 'LCA';
104100130906             exsr tabLCA;
104200110805           WHEN  $Abc(yy) = 'LLT';
104300110805             exsr tabLLT;
104400130729           WHEN  $Abc(yy) = 'BSP SOS';
104500160125             exsr fileBSPSOS;
104600130729           WHEN  $Abc(yy) = 'BSP SOD';
104700160125             exsr fileBSPSOD;
104800160125           WHEN  $Abc(yy) = 'BSP FPA';
104900160125             exsr fileBSPFPA;
105000110809           WHEN  $Abc(yy) = '3C';
105100110809             exsr tab3C;
105200110915             exsr tab3C_VAB;
105300110811           WHEN  $Abc(yy) = '3K VAC';
105400110811             exsr tab3K_VAC;
105500110811           WHEN  $Abc(yy) = '3K VAG';
105600110811             exsr tab3K_VAG;
105700110811           WHEN  $Abc(yy) = '3K VAI';
105800110811             exsr tab3K_VAI;
105900110811           WHEN  $Abc(yy) = '3K VGD';
106000110811             exsr tab3K_VGD;
106100160304           WHEN  $Abc(yy) = 'VAP';
106200160304             exsr tabVAP;
106300110809           WHEN  $Abc(yy) = '3Q';
106400110809             exsr tab3Q;
106500140428           WHEN  $Abc(yy) = 'COMODATO';
106600140428             exsr Comodato;
106700110811           WHEN  $Abc(yy) = '4C VAS';
106800110811             exsr tab4C_VAS;
106900110811           WHEN  $Abc(yy) = '4C VAF';
107000110811             exsr tab4C_VAF;
107100110722           WHEN  $Abc(yy) = '7U';
107200110722             exsr tab7U;
107300110722         ENDSL;
107400110722
107500110722       ENDSR ;
107600110616
107700110616       //--------------------------------------------------------------
107800110616       //?Cerco su TABEL00F.
107900110616       //--------------------------------------------------------------
108000110616       BEGSR da_TABEL;
108100110808
108200110808       //?Apro il file
108300110808         exsr OpenFileTab;
108400110616
108500110722       //?Vado diretta con il codice cliente in key tabella
108600110722         k_TBLcod = %subst($Abc(yy):1:2);
108700110722         k_TBLkey = %editc(TA61ksc:'X');
108800110722         chain %kds(K03tabel) TABEL00F;
108900110805         IF  not %found(TABEL00F) or TBLflg <> *blanks;
109000110805           leavesr;
109100110805         ENDIF;
109200110805
109300110722           $RecOk = *on;
109400110805
109500110805       //?Imposto i dati da emettere nel subfile
109600111026         exsr Dati_x_S01;
109700110616
109800110616       ENDSR ;
109900110616
110000110616       //--------------------------------------------------------------
110100110616       //?Cerco su TNTBE00F.
110200110616       //--------------------------------------------------------------
110300110616       BEGSR da_TNTBE;
110400110808
110500110808       //?Apro il file
110600110808         exsr OpenFileTbe;
110700110616
110800110722       //?Vado diretta con il codice cliente in key tabella
110900110722         C_tab = %subst($Abc(yy):1:3);
111000110722         C_ke1 = %editc(TA61ksc:'X');
111100110722         chain (C_tab : C_ke1) TNTBE01L;
111200110805         IF  not %found(TNTBE01L) or TBEatb <> *blanks;
111300110805           leavesr;
111400110805         ENDIF;
111500110805
111600110805         $RecOk = *on;
111700110805
111800110805       //?Imposto i dati da emettere nel subfile
111900111026         exsr Dati_x_S01;
112000110616
112100110616       ENDSR ;
112200110802
112300110802       //--------------------------------------------------------------
112400110802       //?Fatturazione.
112500110802       //--------------------------------------------------------------
112600110802       BEGSR Fat;
112700110802
112800110802         CLPkut = 1;
112900110802         CLPkcc = Dutkci;
113000110802         CLPksc = TA61ksc;
113100110802         chain (CLPkut:CLPkcc:CLPksc) CNCLP00F;
113200110802         IF  not %found(CNCLP00F) or CLPfft <> 1;
113300110802           leavesr;
113400110802         ENDIF;
113500110802
113600110802         $RecOk = *on;
113700110802
113800110802       //?Imposto i dati da emettere nel subfile
113900110802         exsr Dati_x_S01;
114000110802
114100110802       ENDSR ;
114200110802
114300110802       //--------------------------------------------------------------
114400151116       //?Pagamento C/A estero
114500110802       //--------------------------------------------------------------
114600110802       BEGSR PagCA;
114700110802
114800110802         CLPkut = 1;
114900110802         CLPkcc = Dutkci;
115000110802         CLPksc = TA61ksc;
115100110802         chain (CLPkut:CLPkcc:CLPksc) CNCLP00F;
115200110802         IF  not %found(CNCLP00F) or
115300151116             CLPfpc <> '6';
115400110802           leavesr;
115500110802         ENDIF;
115600110802
115700110802         $RecOk = *on;
115800110802
115900110802       //?Imposto i dati da emettere nel subfile
116000110802         exsr Dati_x_S01;
116100110802
116200110802       ENDSR ;
116300151116
116400151116       //--------------------------------------------------------------
116500151116       //?Pagamento C/A assegno traenza
116600151116       //--------------------------------------------------------------
116700151116       BEGSR PagCAASS;
116800151116
116900151116         CLPkut = 1;
117000151116         CLPkcc = Dutkci;
117100151116         CLPksc = TA61ksc;
117200151116         chain (CLPkut:CLPkcc:CLPksc) CNCLP00F;
117300151116         IF  not %found(CNCLP00F) or
117400151116             CLPfpc <> '1';
117500151116           leavesr;
117600151116         ENDIF;
117700151116
117800151116         $RecOk = *on;
117900151116
118000151116       //?Imposto i dati da emettere nel subfile
118100151116         exsr Dati_x_S01;
118200151116
118300151116       ENDSR ;
118400151116
118500151116       //--------------------------------------------------------------
118600151116       //?Pagamento Danni estero
118700151116       //--------------------------------------------------------------
118800151116       BEGSR PagDN;
118900151116
119000151116         CLSksc = TA61ksc;
119100151116         chain (CLSksc) FNCLS01L;
119200151116         IF  not %found(FNCLS01L) or
119300151116             %subst(CLStic:1:1) <> '6';
119400151116           leavesr;
119500151116         ENDIF;
119600151116
119700151116         $RecOk = *on;
119800151116
119900151116       //?Imposto i dati da emettere nel subfile
120000151116         exsr Dati_x_S01;
120100151116
120200151116       ENDSR ;
120300151116
120400151116       //--------------------------------------------------------------
120500151116       //?Pagamento Danni assegno traenza
120600151116       //--------------------------------------------------------------
120700151116       BEGSR PagDNASS;
120800151116
120900151116         CLSksc = TA61ksc;
121000151116         chain (CLSksc) FNCLS01L;
121100151116         IF  not %found(FNCLS01L) or
121200151116             %subst(CLStic:1:1) <> '1';
121300151116           leavesr;
121400151116         ENDIF;
121500151116
121600151116         $RecOk = *on;
121700151116
121800151116       //?Imposto i dati da emettere nel subfile
121900151116         exsr Dati_x_S01;
122000151116
122100151116       ENDSR ;
122200151116
122300151116       //--------------------------------------------------------------
122400151116       //?Pagamento Note Accredito estero
122500151116       //--------------------------------------------------------------
122600151116       BEGSR PagNA;
122700151116
122800151116         CLSksc = TA61ksc;
122900151116         chain (CLSksc) FNCLS01L;
123000151116         IF  not %found(FNCLS01L) or
123100151116             %subst(CLStic:2:1) <> '6';
123200151116           leavesr;
123300151116         ENDIF;
123400151116
123500151116         $RecOk = *on;
123600151116
123700151116       //?Imposto i dati da emettere nel subfile
123800151116         exsr Dati_x_S01;
123900151116
124000151116       ENDSR ;
124100151116
124200151116       //--------------------------------------------------------------
124300151116       //?Pagamento Note Accredito assegno traenza
124400151116       //--------------------------------------------------------------
124500151116       BEGSR PagNAASS;
124600151116
124700151116         CLSksc = TA61ksc;
124800151116         chain (CLSksc) FNCLS01L;
124900151116         IF  not %found(FNCLS01L) or
125000151116             %subst(CLStic:2:1) <> '1';
125100151116           leavesr;
125200151116         ENDIF;
125300151116
125400151116         $RecOk = *on;
125500151116
125600151116       //?Imposto i dati da emettere nel subfile
125700151116         exsr Dati_x_S01;
125800151116
125900151116       ENDSR ;
126000160304
126100160304       //--------------------------------------------------------------
126200160304       //?C/Assegni.
126300160304       //--------------------------------------------------------------
126400160304       BEGSR CAssegni;
126500160304
126600160304         wKscAlfa8 = *all'0';
126700160304         %subst(wKscAlfa8:2:7) = %editc(TA61ksc:'X');
126800160304         clear wKscFact;
126900160304         exec sql
127000160304         select RCOkscfact into :wKscFact
127100160304         FROM anrco00f
127200160304         WHERE  RCOsocieta = '201' and
127300160304                RCOsnatura = 'C' and
127400160304                RCOksc = :wKscAlfa8;
127500160304
127600160304         IF  wKscFact = *blanks;
127700160304           leavesr;
127800160304         ENDIF;
127900160304
128000160304         $RecOk = *on;
128100160304
128200160304       //?Imposto i dati da emettere nel subfile
128300160304         exsr Dati_x_S01;
128400160304         wKscAlfa = %subst(wKscFact:2:7);
128500160304         V1Sdes = %trim(V1Sdes) + ' ' + wKscAlfa;
128600160304
128700160304       ENDSR ;
128800110803
128900110803       //--------------------------------------------------------------
129000110803       //?Conto Economico.
129100110803       //--------------------------------------------------------------
129200110803       BEGSR ContoEco;
129300110803
129400110803         setll TA61ksc ECEVD02L;
129500110803         reade TA61ksc ECEVD02L;
129600110803         DOW not %eof(ECEVD02L);
129700110803         //?Se trovo anche solo 1 personalizzazione valida ad oggi
129800110803         //?emetto nel subfile
129900110803           IF  EVDdst >= %dec(%date());
130000110803             $RecOk = *on;
130100110803           //?Imposto i dati da emettere nel subfile
130200110803             exsr Dati_x_S01;
130300110803             leave;
130400110803           ENDIF;
130500110803         reade TA61ksc ECEVD02L;
130600110803         ENDDO;
130700110803
130800110803       ENDSR ;
130900110803
131000110803       //--------------------------------------------------------------
131100110803       //?Esenzione IVA.
131200110803       //--------------------------------------------------------------
131300110803       BEGSR EsIVA;
131400110803
131500110803         INDkut = 1;
131600110803         INDkcc = Dutkci;
131700110803         INDksc = TA61ksc;
131800110803         chain (INDkut:INDkcc:INDksc) CNIND00F;
131900110803         IF  not %found(CNIND00F);
132000110803           leavesr;
132100110803         ENDIF;
132200110805
132300110803         IF INDnpr <> *zeros or INDdpr <> *zeros or
132400110803            INDnpn <> *zeros;
132500110803
132600110803           $RecOk = *on;
132700110803
132800110803         //?Imposto i dati da emettere nel subfile
132900110803           exsr Dati_x_S01;
133000110803
133100110803         ENDIF;
133200110803
133300110803       ENDSR ;
133400110809
133500110809       //--------------------------------------------------------------
133600110809       //?Invio ORM da VAS.
133700110809       //--------------------------------------------------------------
133800110809       BEGSR OrmVao;
133900110809
134000110809       //?Controllo se il KSC è abilitato all'invio degli ORM
134100110809         wKscAlfa = %editc(TA61ksC:'X');
134200110809         wTip = 'VO';
134300110809         exsr  Abi_VAS;
134400110809         IF  wKscAlfa <> *blanks;
134500110809           $RecOk = *on;
134600110809           exsr Dati_x_S01;
134700110809         ENDIF;
134800110809
134900110809       ENDSR ;
135000110809
135100110809       //--------------------------------------------------------------
135200110927       //?Tabella CLI campo LDV
135300110809       //--------------------------------------------------------------
135400110927       BEGSR tabCLILDV;
135500110809
135600110927       //?Imposto i dati da emettere nel subfile
135700110927         IF  §CLIldv = 'S';
135800110927           exsr Dati_x_S01;
135900110809           exsr WrtS01;
136000110809         ENDIF;
136100110927
136200110927       ENDSR;
136300110927
136400110927       //--------------------------------------------------------------
136500110927       //?Tabella CLI campo DPD
136600110927       //--------------------------------------------------------------
136700110927       BEGSR tabCLIDPD;
136800110927
136900110927       //?Imposto i dati da emettere nel subfile
137000110927         IF  §CLIdpd = 'S';
137100110927           exsr Dati_x_S01;
137200110809           exsr WrtS01;
137300110809         ENDIF;
137400110927
137500110927       ENDSR ;
137600110927
137700110927       //--------------------------------------------------------------
137800110927       //?Tabella CLI campo LNA
137900110927       //--------------------------------------------------------------
138000110927       BEGSR tabCLILNA;
138100110927
138200110927       //?Imposto i dati da emettere nel subfile
138300110809         IF  §CLIlna = 'S';
138400110927           exsr Dati_x_S01;
138500110809           exsr WrtS01;
138600110809         ENDIF;
138700110927
138800110927       ENDSR ;
138900110927
139000110927       //--------------------------------------------------------------
139100110927       //?Tabella CLI campo LVP
139200110927       //--------------------------------------------------------------
139300110927       BEGSR tabCLILVP;
139400110927
139500110927       //?Imposto i dati da emettere nel subfile
139600110809         IF  §CLIlvp = 'S';
139700110927           exsr Dati_x_S01;
139800110809           exsr WrtS01;
139900110809         ENDIF;
140000110927
140100110927       ENDSR ;
140200110927
140300110927       //--------------------------------------------------------------
140400110927       //?Tabella CLI campo DKB
140500110927       //--------------------------------------------------------------
140600110927       BEGSR tabCLIDKB;
140700110927
140800110927       //?Imposto i dati da emettere nel subfile
140900110809         IF  §CLIdkb = 'S';
141000110927           exsr Dati_x_S01;
141100110927           exsr WrtS01;
141200110809         ENDIF;
141300110927
141400110927       ENDSR ;
141500110927
141600110927       //--------------------------------------------------------------
141700110927       //?Tabella CLI campo DCR
141800110927       //--------------------------------------------------------------
141900110927       BEGSR tabCLIDCR;
142000110927
142100110927       //?Imposto i dati da emettere nel subfile
142200110809         IF  §CLIdcr = 'S';
142300110927           exsr Dati_x_S01;
142400110809           exsr WrtS01;
142500110809         ENDIF;
142600110927
142700110927       ENDSR ;
142800110927
142900110927       //--------------------------------------------------------------
143000110927       //?Tabella CLI campo GCP
143100110927       //--------------------------------------------------------------
143200110927       BEGSR tabCLIGCP;
143300110927
143400110927       //?Imposto i dati da emettere nel subfile
143500110809         IF  §CLIgcp = 'S';
143600110927           exsr Dati_x_S01;
143700110809           exsr WrtS01;
143800110809         ENDIF;
143900110927
144000110927       ENDSR ;
144100110927
144200110927       //--------------------------------------------------------------
144300110927       //?Tabella CLI campo BRD
144400110927       //--------------------------------------------------------------
144500110927       BEGSR tabCLIBRD;
144600110927
144700110927       //?Imposto i dati da emettere nel subfile
144800110809         IF  §CLIbrd = 'S';
144900110927           exsr Dati_x_S01;
145000110809           exsr WrtS01;
145100110809         ENDIF;
145200110809
145300110809       ENDSR ;
145400110927
145500110927       //--------------------------------------------------------------
145600110927       //?Tabella CLI campo BAN
145700110927       //--------------------------------------------------------------
145800110927       BEGSR tabCLIBAN;
145900110927
146000110927       //?Imposto i dati da emettere nel subfile
146100110927         IF  §CLIban = 'S';
146200110927           exsr Dati_x_S01;
146300110927           exsr WrtS01;
146400110927         ENDIF;
146500110927
146600110927       ENDSR ;
146700120620
146800120620       //--------------------------------------------------------------
146900120620       //?Tabella CLI campo EMD
147000120620       //--------------------------------------------------------------
147100120620       BEGSR tabCLIEMD;
147200120620
147300120620       //?Imposto i dati da emettere nel subfile
147400120620         IF  §CLIemd = 'S';
147500120620           exsr Dati_x_S01;
147600120620           exsr WrtS01;
147700120620         ENDIF;
147800120620
147900120620       ENDSR ;
148000130320
148100130321       //--------------------------------------------------------------
148200130321       //?Tabella CLI campo PDF
148300130321       //--------------------------------------------------------------
148400130321       BEGSR tabCLIPDF;
148500130321
148600130321       //?Imposto i dati da emettere nel subfile
148700130321         IF  §CLIpdfp <> *blanks;
148800130321           exsr Dati_x_S01;
148900130321           exsr WrtS01;
149000130321         ENDIF;
149100130321
149200130321       ENDSR ;
149300131018
149400131018       //--------------------------------------------------------------
149500131018       //?Tabella CLI campo PSM
149600131018       //--------------------------------------------------------------
149700131018       BEGSR tabCLIPSM;
149800131018
149900131018       //?Imposto i dati da emettere nel subfile
150000160108         IF  §CLIpsm <> *blanks and §CLIpsm <> *zeros;
150100131018           exsr Dati_x_S01;
150200131018           exsr WrtS01;
150300131018         ENDIF;
150400131018
150500131018       ENDSR ;
150600151109
150700151109       //--------------------------------------------------------------
150800151109       //?Tabella CLI campo EMDP
150900151109       //--------------------------------------------------------------
151000151109       BEGSR tabCLIEMDP;
151100151109
151200151109       //?Imposto i dati da emettere nel subfile
151300151109         IF  §CLIemdp = 'S';
151400151109           exsr Dati_x_S01;
151500151109           exsr WrtS01;
151600151109         ENDIF;
151700151109
151800151109       ENDSR ;
151900110803
152000110803       //--------------------------------------------------------------
152100110803       //?Tabella DCC.
152200110803       //--------------------------------------------------------------
152300110803       BEGSR tabDCC;
152400110808
152500110808       //?Apro il file
152600110808         exsr OpenFileTbe;
152700110913
152800110913         clear dDCC;
152900110913         C_tab = %subst($Abc(yy):1:3);
153000110913         setll C_tab TNTBE01L;
153100110913         reade C_tab TNTBE01L;
153200110913         DOW not %eof(TNTBE01L);
153300110913
153400110913           IF  TBEatb = *blanks;
153500110913             dDCC = TBEuni;
153600110913             IF  §DCCksc = TA61ksc;
153700110913               $RecOk = *on;
153800110913             //?Imposto i dati da emettere nel subfile
153900110913               exsr Dati_x_S01;
154000110913               leave;
154100110913             ENDIF;
154200110913           ENDIF;
154300110913
154400110913           reade C_tab TNTBE01L;
154500110913         ENDDO;
154600110913
154700110913       //?Imposto la riga da emettere a subfile
154800110913         IF  $RecOk;
154900110913           V1Sdes = 'Assegnata serie ' + %subst(TBEke1:1:4) +
155000110913                    ' per DISK C con prefisso "CC"';
155100110913         ENDIF;
155200110803
155300110803       ENDSR ;
155400120621
155500120621       //--------------------------------------------------------------
155600120621       //?Tabella FLB.
155700120621       //--------------------------------------------------------------
155800120621       BEGSR tabFLB;
155900120621
156000120621       //?Apro il file
156100120621         exsr OpenFileTbe;
156200120621
156300120621         clear wFLBuni;
156400120621         C_tab = %subst($Abc(yy):1:3);
156500120621         setll C_tab TNTBE01L;
156600120621         reade C_tab TNTBE01L;
156700120621         DOW not %eof(TNTBE01L);
156800120621
156900120621           IF  TBEatb = *blanks;
157000120621             wFLBuni = TBEuni;
157100120621             IF  %lookup(%editc(TA61ksc:'X'):skksc) > 0;
157200120621             //?Imposto i dati da emettere nel subfile
157300120621             //?posso avere più righe per cliente
157400120621               exsr Dati_x_S01;
157500120621               V1Sdes = 'Forza LNA cappario ' + %subst(TBEke1:1:3) +
157600120621                        ' in LNA ' + %subst(TBEke2:1:3);
157700120621               exsr WrtS01;
157800120621             ENDIF;
157900120621           ENDIF;
158000120621
158100120621           reade C_tab TNTBE01L;
158200120621         ENDDO;
158300120621
158400120621
158500120621       ENDSR ;
158600120604
158700120604       //--------------------------------------------------------------
158800120604       //?Tabella JDC.
158900120604       //--------------------------------------------------------------
159000120604       BEGSR tabJDC;
159100120604
159200120604       //?Apro il file e forzo la lettura da Sede
159300120604         $Sede = *on;
159400120604         exsr OpenFileTbe;
159500120604
159600120604         clear dJDC;
159700120604         C_tab = %subst($Abc(yy):1:3);
159800120604         C_ke1 = %editc(TA61ksc:'X');
159900120604         chain (C_tab : C_ke1) TNTBE01L;
160000120604         IF  not %found(TNTBE01L) or TBEatb <> *blanks;
160100120604           leavesr;
160200120604         ENDIF;
160300120604
160400120604         dJDC = TBEuni;
160500120604
160600120604       //?Se tipo elaborazione = 'N' non scrivo il rcd nel sbfl
160700120604         IF  §JDCaut = 'N';
160800120604           leavesr;
160900120604         ENDIF;
161000120604
161100120604         $RecOk = *on;
161200120604
161300120604       //?Imposto i dati da emettere nel subfile
161400120604         exsr Dati_x_S01;
161500120604
161600120604       ENDSR ;
161700110722
161800110913       //--------------------------------------------------------------
161900110913       //?Tabella LAC.
162000110913       //--------------------------------------------------------------
162100110913       BEGSR tabLAC;
162200110913
162300110913       //?Apro il file e forzo la lettura da Sede
162400110913         $Sede = *on;
162500110913         exsr OpenFileTbe;
162600110913
162700150513         sav_AUT  = $Aut(yy);
162800150513
162900110913         clear dLAC;
163000110913         C_tab = %subst($Abc(yy):1:3);
163100110913         C_ke1 = %editc(TA61ksc:'X');
163200110913         chain (C_tab : C_ke1) TNTBE01L;
163300110913         IF  not %found(TNTBE01L) or TBEatb <> *blanks;
163400110913           leavesr;
163500110913         ENDIF;
163600110913
163700110913         dLAC = TBEuni;
163800110913
163900110913       //?Se tipo elaborazione = 'N' non scrivo il rcd nel sbfl
164000110913         IF  §LACaut = 'N';
164100110913           leavesr;
164200110913         ENDIF;
164300110913
164400110913         $RecOk = *on;
164500150513
164600150513       //?Se cliente con NO addebito
164700150513       //?devo impostare come autorizzazione la 'DG' e non 'ICT'
164800150513         IF  §LACtad = 'N';
164900150513           $Aut(yy) = 'G';
165000150513         ENDIF;
165100150513       //?Se cliente con personalizzazione delle competenze
165200150513         exec sql
165300150513         select EVDitr into :wCompCE
165400150513         FROM ecevd00f
165500150513         WHERE EVDcli = :TA61ksc and
165600150513               EVDvoc = 199 and
165700150513               EVDttv = 'K' and
165800150513               EVDddt <= :woggi and
165900150513               EVDdst >= :woggi;
166000150513           IF  sqlcod = 100 or sqlcod < 0;
166100150513           ELSE;
166200150513             $Aut(yy) = 'G';
166300150513           ENDIF;
166400110913
166500110913       //?Imposto i dati da emettere nel subfile
166600110913         exsr Dati_x_S01;
166700150513         $Aut(yy) = sav_AUT;
166800110913
166900110913       ENDSR ;
167000130906
167100130906       //--------------------------------------------------------------
167200130906       //?Tabella LCA.
167300130906       //--------------------------------------------------------------
167400130906       BEGSR tabLCA;
167500130906
167600130906       //?Apro il file
167700130906         exsr OpenFileTbe;
167800130906
167900130906         clear dLCA;
168000130906         C_tab = %subst($Abc(yy):1:3);
168100130906         setll C_tab TNTBE01L;
168200130906         reade C_tab TNTBE01L;
168300130906         DOW not %eof(TNTBE01L);
168400130906
168500130906           IF  TBEatb = *blanks;
168600130906             IF  %subst(TBEke2:9:7) = %editc(TA61ksc:'X');
168700130906               dLCA = TBEuni;
168800130906               $RecOk = *on;
168900130906             //?Imposto i dati da emettere nel subfile
169000130906               exsr Dati_x_S01;
169100130906               leave;
169200130906             ENDIF;
169300130906           ENDIF;
169400130906
169500130906           reade C_tab TNTBE01L;
169600130906         ENDDO;
169700130906
169800130906       ENDSR ;
169900110805
170000110805       //--------------------------------------------------------------
170100110805       //?Tabella LLT.
170200110805       //--------------------------------------------------------------
170300110805       BEGSR tabLLT;
170400110808
170500110808       //?Apro il file
170600110808         exsr OpenFileTbe;
170700110805
170800110805         C_tab = %subst($Abc(yy):1:3);
170900110805         clear C_ke1;
171000110805         %subst(C_ke1:9:7) = %editc(TA61ksc:'X');
171100110805         chain (C_tab : C_ke1) TNTBE01L;
171200110805         IF  not %found(TNTBE01L) or TBEatb <> *blanks;
171300110805           leavesr;
171400110805         ENDIF;
171500110805
171600110805         $RecOk = *on;
171700110805
171800110805         dLLT = TBEuni;
171900110805
172000110805       //?Imposto i dati da emettere nel subfile
172100110805         exsr Dati_x_S01;
172200110805
172300110805         IF  §LLTimp = 0;
172400110915           V1Sdes = 'Gestione danni sempre con Pratica Assicurativa';
172500110805         ELSE;
172600110915           V1Sdes = 'Gestione danni in L.T. fino a EUR ' +
172700110914                    %trim(%editc(§LLtimp:'4'));
172800110805         ENDIF;
172900110805
173000110805       ENDSR ;
173100130729
173200130729       //--------------------------------------------------------------
173300160125       //?File FIBSP per Sospensione x colli non spuntati
173400130729       //--------------------------------------------------------------
173500160125       BEGSR fileBSPSOS;
173600130729
173700130729       //?Imposto i dati da emettere nel subfile
173800160125         IF  $Bsp and
173900160125             BSPfpa = *blanks and
174000160125             BSPaut = *blanks;
174100160125           exsr Dati_x_S01;
174200160125           exsr WrtS01;
174300130729         ENDIF;
174400130729
174500130729       ENDSR ;
174600130729
174700130729       //--------------------------------------------------------------
174800160125       //?File FIBSP per Sospensione + Data consegna richiesta
174900130729       //--------------------------------------------------------------
175000160125       BEGSR fileBSPSOD;
175100130729
175200130729       //?Imposto i dati da emettere nel subfile
175300160125         IF  $Bsp and
175400160125             BSPfpa = *blanks and BSPdcrp = 'S' and
175500160125             BSPaut = *blanks;
175600130729           exsr Dati_x_S01;
175700130729           exsr WrtS01;
175800130729         ENDIF;
175900130729
176000130729       ENDSR ;
176100160125
176200160125       //--------------------------------------------------------------
176300160125       //?File FIBSP per Sospensione + Sposta bolle ad altra filiale
176400160125       //--------------------------------------------------------------
176500160125       BEGSR fileBSPFPA;
176600160125
176700160125       //?Imposto i dati da emettere nel subfile
176800160125         IF  $Bsp and
176900160125             BSPfpa = 'S';
177000160125           exsr Dati_x_S01;
177100160125           exsr WrtS01;
177200160125         ENDIF;
177300160125
177400160125       ENDSR ;
177500110805
177600110722       //--------------------------------------------------------------
177700110722       //?Tabella 3C.
177800110722       //--------------------------------------------------------------
177900110722       BEGSR tab3C;
178000110808
178100110808       //?Apro il file
178200110808         exsr OpenFileTab;
178300110809
178400110809         k_TBLcod = %subst($Abc(yy):1:2);
178500110809         k_TBLkey = %editc(TA61ksc:'X');
178600110809         chain %kds(K03tabel) TABEL00F;
178700110809         IF  not %found(TABEL00F) or TBLflg <> *blanks;
178800110809           leavesr;
178900110809         ENDIF;
179000110809
179100110809         ds3C = TBLuni;
179200110812
179300110812         $RecOk = *on;
179400110809
179500110812       //?Controllo se il cliente o l'unificante sono abilitati al VAS
179600110812         wKscAlfa = %editc(§3Ccks:'X');
179700110812         wUd  = 'U';
179800110812         exsr  Abi_VAB;
179900110812       //?Se non trovo con unificante ed è diverso provo con KSC
180000110812         IF  wKscAlfa = *blanks and TA61ksc <> §3Ccks;
180100110809           wKscAlfa = %editc(TA61ksc:'X');
180200110809           exsr  Abi_VAB;
180300110812         ENDIF;
180400120503
180500120503       //?Se supporto EDI devo controllare se abilitato ai servizi VAS
180600120503       //?con tipo file "EI"
180700120503         IF  §3Ccba = 'EDI';
180800120503           wKscAlfa = %editc(§3Ccks:'X');
180900120503           wTip = 'EI';
181000120503           exsr  Abi_VAS;
181100120503         //?Se non trovo con unificante ed è diverso provo con KSC
181200120503           IF  wKscAlfa = *blanks and TA61ksc <> §3Ccks;
181300120503             wKscAlfa = %editc(TA61ksc:'X');
181400120503             exsr  Abi_VAS;
181500120503           ENDIF;
181600120503         ENDIF;
181700110812
181800110812       //?Imposto i dati da emettere nel subfile
181900110812         exsr Dati_x_S01;
182000110913
182100110913       //?Se EasySpedWeb è per forza abilitato ai servizi VAS
182200110913         IF  §3Ccba = 'ESWEB';
182300110913           leavesr;
182400110913         ENDIF;
182500110915
182600110915       //?Se supporto SNA è per forza abilitato ai servizi VAS
182700110915         IF  §3Ccba = 'SNA';
182800110915           leavesr;
182900110915         ENDIF;
183000110812
183100110812       //?Se non ho trovato abilitazione VAS lo indico nella descrizione
183200110812         IF  wKscAlfa = *blanks;
183300110812           V1Sdes = %trim(V1Sdes) + ' - non abilitato a VAS';
183400110812         ENDIF;
183500110915
183600110915       //?Se non abilitato e non c'è la serie e nemmeno l'accorpamento
183700110915       //?bolle non devo scrivere la riga nel subfile
183800110915         IF  wKscAlfa = *blanks and §3Cnrs = 0 and §3Cabd = *blanks;
183900110915           $RecOk = *off;
184000110915         ENDIF;
184100110722
184200110722       ENDSR ;
184300110915
184400110915       //--------------------------------------------------------------
184500110915       //?Tabella 3C VAB - Abilitazione VAB x no tab.3C
184600110915       //--------------------------------------------------------------
184700110915       BEGSR tab3C_VAB;
184800110915
184900110915       //?Se il cliente non è in tabella 3C provo a vedere lo stesso
185000110915       //?se è abilitato ai VAS
185100110915         IF  not $RecOk;
185200110915           wKscAlfa = %editc(TA61ksc:'X');
185300110915           exsr  Abi_VAB;
185400110915
185500110915         //?Se è abilitato ai VAS
185600110915           IF  wKscAlfa <> *blanks;
185700110915             $RecOk = *on;
185800110915           //?Imposto i dati da emettere nel subfile
185900110915           //?ma non do la possibilità di interrogare il dettaglio
186000110915             exsr Dati_x_S01;
186100110915             clear V1Hpgm;
186200110915           ENDIF;
186300110915         ENDIF;
186400110915
186500110915       ENDSR ;
186600110811
186700110811       //--------------------------------------------------------------
186800110811       //?Tabella 3K VAC - Consegne
186900110811       //--------------------------------------------------------------
187000110811       BEGSR tab3K_VAC;
187100110811
187200110811       //?Apro il file
187300110811         exsr OpenFileTab;
187400110811
187500110811         k_TBLcod = %subst($Abc(yy):1:2);
187600110811         k_TBLkey = %editc(TA61ksc:'X');
187700110811         chain %kds(K03tabel) TABEL00F;
187800110811         IF  not %found(TABEL00F) or TBLflg <> *blanks;
187900110811           leavesr;
188000110811         ENDIF;
188100110811
188200110811         ds3K = TBLuni;
188300110811
188400110811       //?Scrivo riga se trovo il codice unificante e i relativi flag
188500110811         IF  §3Kcks = *zeros or
188600110811            (§3Kcq1 = *blanks and §3Kcq2 = *blanks and
188700110811             §3Kcq3 = *blanks and §3Kcq4 = *blanks and
188800110811             §3Kcq5 = *blanks and §3Kcq8 = *blanks);
188900110811           leavesr;
189000110811         ENDIF;
189100110811
189200110811         $RecOk = *on;
189300110811
189400110811       //?Controllo se il cliente o l'unificante sono abilitati al VAS
189500110811         wKscAlfa = %editc(§3Kcks:'X');
189600110811         wTip = 'VC';
189700110811         wUd  = 'D';
189800110811         exsr  Abi_VAS;
189900110811       //?Se non trovo con unificante ed è diverso provo con KSC
190000110811         IF  wKscAlfa = *blanks and TA61ksc <> §3Kcks;
190100110811           wKscAlfa = %editc(TA61ksc:'X');
190200110811           exsr  Abi_VAS;
190300110811         ENDIF;
190400110811
190500110811       //?Imposto i dati da emettere nel subfile
190600110811         exsr Dati_x_S01;
190700110811
190800110811       //?Se non ho trovato abilitazione VAS lo indico nella descrizione
190900110811         IF  wKscAlfa = *blanks;
191000110811           V1Sdes = %trim(V1Sdes) + ' - non abilitato a VAS';
191100110811         ENDIF;
191200110811
191300110811       ENDSR ;
191400110811
191500110811       //--------------------------------------------------------------
191600110811       //?Tabella 3K VAG - Giacenze
191700110811       //--------------------------------------------------------------
191800110811       BEGSR tab3K_VAG;
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
193300110811       //?devo fare 2 controlli
193400110811
193500110811       //?Un controllo con unificante VAG + flag (l'invio di questi
193600110811       //?dati viene scatenato dal file giacenze)
193700110811         IF  §3Kgks <> *zeros and
193800110811            (§3Kgqi <> *blanks or §3Kapp <> *blanks);
193900110811           $RecOk = *on;
194000110811           wKscAlfa = %editc(§3Kgks:'X');
194100110811         ENDIF;
194200110811       //?l'altro con unificante VAC + flag (l'invio di questi dati
194300110811       //?NON viene scatenato dal file giacenze)
194400110811         IF  §3Kcks <> *zeros and (§3Knmc <> *blanks or
194500110811             §3Kcq6 <> *blanks or §3Kcq7 <> *blanks);
194600110811           $RecOk = *on;
194700110811           wKscAlfa = %editc(§3Kcks:'X');
194800110811         ENDIF;
194900110811
195000110811       //?Se non i controlli non hanno dato esito positivo vado via
195100110811         IF  not $RecOk;
195200110811           leavesr;
195300110811         ENDIF;
195400110811
195500110811       //?Controllo se il cliente o l'unificante sono abilitati al VAS
195600110811         wTip = 'VG';
195700110811         wUd  = 'D';
195800110811         exsr  Abi_VAS;
195900110811       //?Se non trovo con unificante ed è diverso provo con KSC
196000110811         IF  wKscAlfa = *blanks and TA61ksc <> §3Kcks;
196100110811           wKscAlfa = %editc(TA61ksc:'X');
196200110811           exsr  Abi_VAS;
196300110811         ENDIF;
196400110811
196500110811       //?Imposto i dati da emettere nel subfile
196600110811         exsr Dati_x_S01;
196700110811
196800110811       //?Se non ho trovato abilitazione VAS lo indico nella descrizione
196900110811         IF  wKscAlfa = *blanks;
197000110811           V1Sdes = %trim(V1Sdes) + ' - non abilitato a VAS';
197100110811         ENDIF;
197200110811
197300110811       ENDSR ;
197400110811
197500110811       //--------------------------------------------------------------
197600110811       //?Tabella 3K VAI - Comunicazioni al cliente
197700110811       //--------------------------------------------------------------
197800110811       BEGSR tab3K_VAI;
197900110811
198000110811       //?Apro il file
198100110811         exsr OpenFileTab;
198200110811
198300110811         k_TBLcod = %subst($Abc(yy):1:2);
198400110811         k_TBLkey = %editc(TA61ksc:'X');
198500110811         chain %kds(K03tabel) TABEL00F;
198600110811         IF  not %found(TABEL00F) or TBLflg <> *blanks;
198700110811           leavesr;
198800110811         ENDIF;
198900110811
199000110811         ds3K = TBLuni;
199100110811
199200110811       //?Scrivo riga se trovo il codice unificante e i relativi flag
199300110811         IF  §3Kcks = *zeros or
199400110811            (§3Kcma = *blanks and §3Kcba = *blanks);
199500110811           leavesr;
199600110811         ENDIF;
199700110811
199800110811         $RecOk = *on;
199900110811
200000110811       //?Controllo se il cliente o l'unificante sono abilitati al VAS
200100110811         wKscAlfa = %editc(§3Kcks:'X');
200200110811         wTip = 'VI';
200300110811         wUd  = 'D';
200400110811         exsr  Abi_VAS;
200500110811       //?Se non trovo con unificante ed è diverso provo con KSC
200600110811         IF  wKscAlfa = *blanks and TA61ksc <> §3Kcks;
200700110811           wKscAlfa = %editc(TA61ksc:'X');
200800110811           exsr  Abi_VAS;
200900110811         ENDIF;
201000110811
201100110811       //?Imposto i dati da emettere nel subfile
201200110811         exsr Dati_x_S01;
201300110811
201400110811       //?Se non ho trovato abilitazione VAS lo indico nella descrizione
201500110811         IF  wKscAlfa = *blanks;
201600110811           V1Sdes = %trim(V1Sdes) + ' - non abilitato a VAS';
201700110811         ENDIF;
201800110811
201900110811       ENDSR ;
202000110811
202100110811       //--------------------------------------------------------------
202200110811       //?Tabella 3K VGD - Dati C.A. completi
202300110811       //--------------------------------------------------------------
202400110811       BEGSR tab3K_VGD;
202500110811
202600110811       //?Apro il file
202700110811         exsr OpenFileTab;
202800110811
202900110811         k_TBLcod = %subst($Abc(yy):1:2);
203000110811         k_TBLkey = %editc(TA61ksc:'X');
203100110811         chain %kds(K03tabel) TABEL00F;
203200110811         IF  not %found(TABEL00F) or TBLflg <> *blanks;
203300110811           leavesr;
203400110811         ENDIF;
203500110811
203600110811         ds3K = TBLuni;
203700110811
203800110811       //?Scrivo riga se trovo il codice unificante e i relativi flag
203900110811         IF  §3Kgks = *zeros or §3Kca = *blanks;
204000110811           leavesr;
204100110811         ENDIF;
204200110811
204300110811         $RecOk = *on;
204400110811
204500110811       //?Imposto i dati da emettere nel subfile
204600110811         exsr Dati_x_S01;
204700110812
204800110812       //?Se l'unificnate è diverso decodifico e metto nella descrizione
204900110812         IF  §3Kgks <> TA61ksc;
205000110812           clear tibs69ds;
205100110812           I69kac = §3Kgks;
205200110812           TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
205300110812           IF  O69err = *blanks;
205400110812             V1Sdes = %trim(V1Sdes) + ' a ' +
205500110812                      %trim(%editc(§3Kgks:'4')) +
205600110812                      ' ' + ACOrag;
205700110812           ENDIF;
205800110812         ENDIF;
205900110812
206000110811       ENDSR ;
206100160304
206200160304       //--------------------------------------------------------------
206300160304       //?Tabella VAP - Esiti Ritiri.
206400160304       //--------------------------------------------------------------
206500160304       BEGSR tabVAP;
206600160304
206700160304       //?Apro il file
206800160304         exsr OpenFileTbe;
206900160304
207000160304         C_tab = %subst($Abc(yy):1:3);
207100160304         C_ke1 = %editc(TA61ksc:'X');
207200160304         chain (C_tab : C_ke1) TNTBE01L;
207300160304         IF  %found(TNTBE01L) and TBEatb = *blanks;
207400160304           leavesr;
207500160304         ENDIF;
207600160304
207700160304         $RecOk = *on;
207800160304
207900160304       //?Controllo se il cliente o l'unificante sono abilitati al VAS
208000160304         wKscAlfa = %subst(TBEke2:1:7);
208100160304         wTip = 'VP';
208200160304         wUd  = 'D';
208300160304         exsr  Abi_VAS;
208400160304       //?Se non trovo con unificante ed è diverso provo con KSC
208500160304         IF  wKscAlfa = *blanks and
208600160304             %editc(TA61ksc:'X') <> %subst(TBEke2:1:7);
208700160304           wKscAlfa = %editc(TA61ksc:'X');
208800160304           exsr  Abi_VAS;
208900160304         ENDIF;
209000160304
209100160304       //?Imposto i dati da emettere nel subfile
209200160304         exsr Dati_x_S01;
209300160304
209400160304       //?Se non ho trovato abilitazione VAS lo indico nella descrizione
209500160304         IF  wKscAlfa = *blanks;
209600160304           V1Sdes = %trim(V1Sdes) + ' - non abilitato a VAS';
209700160304         ENDIF;
209800160304
209900160304       ENDSR ;
210000110811
210100110809       //--------------------------------------------------------------
210200110809       //?Tabella 3Q.
210300110809       //--------------------------------------------------------------
210400110809       BEGSR tab3Q;
210500110809
210600110809       //?Apro il file
210700110809         exsr OpenFileTab;
210800110809
210900110809         k_TBLcod = %subst($Abc(yy):1:2);
211000110809         k_TBLkey = %editc(TA61ksc:'X');
211100110809         chain %kds(K03tabel) TABEL00F;
211200110809         IF  not %found(TABEL00F) or TBLflg <> *blanks;
211300110809           leavesr;
211400110809         ENDIF;
211500110809
211600110809         ds3Q = TBLuni;
211700110811
211800110811       //?Scrivo riga se trovo il codice unificante
211900110811         IF  §3Qcks = *zeros;
212000110811           leavesr;
212100110811         ENDIF;
212200110811
212300110811         $RecOk = *on;
212400110809
212500110811       //?Controllo se il cliente o l'unificante sono abilitati al VAS
212600110811         wKscAlfa = %editc(§3Qcks:'X');
212700110811         wTip = 'BT';
212800110811         wUd  = 'D';
212900110811         exsr  Abi_VAS;
213000110811       //?Se non trovo con unificante ed è diverso provo con KSC
213100110811         IF  wKscAlfa = *blanks and TA61ksc <> §3Qcks;
213200110809           wKscAlfa = %editc(TA61ksc:'X');
213300110809           exsr  Abi_VAS;
213400110811         ENDIF;
213500110811
213600110811       //?Imposto i dati da emettere nel subfile
213700110811         exsr Dati_x_S01;
213800110811
213900110811       //?Se non ho trovato abilitazione VAS lo indico nella descrizione
214000110811         IF  wKscAlfa = *blanks;
214100110811           V1Sdes = %trim(V1Sdes) + ' - non abilitato a VAS';
214200110811         ENDIF;
214300110809
214400110809       ENDSR ;
214500110809
214600110722       //--------------------------------------------------------------
214700140428       //?File UNCMD - Contratti Comodato
214800110722       //--------------------------------------------------------------
214900140428       BEGSR Comodato;
215000110808
215100140428         setll TA61ksc UNCMD01L;
215200140428         reade TA61ksc UNCMD01L;
215300140428         DOW not %eof(UNCMD01L);
215400140428         //?Se trovo anche solo 1 personalizzazione non annullata o chiusa
215500140428         //?emetto nel subfile
215600140428           IF  cmdcfg<>'A' and cmdcfg<>'C';
215700140428             $RecOk = *on;
215800140428           //?Imposto i dati da emettere nel subfile
215900140428             exsr Dati_x_S01;
216000140428             leave;
216100140428           ENDIF;
216200140428         reade TA61ksc UNCMD01L;
216300140428         ENDDO;
216400110722
216500110722       ENDSR ;
216600110811
216700110811       //--------------------------------------------------------------
216800110811       //?Tabella 4C VAS - contrassegni.
216900110811       //--------------------------------------------------------------
217000110811       BEGSR tab4C_VAS;
217100110811
217200120207       //?Apro il file e forzo la lettura da Sede
217300120207         $Sede = *on;
217400110811         exsr OpenFileTab;
217500110811
217600110811         k_TBLcod = %subst($Abc(yy):1:2);
217700110811         k_TBLkey = %editc(TA61ksc:'X');
217800110811         chain %kds(K03tabel) TABEL00F;
217900110811         IF  not %found(TABEL00F) or TBLflg <> *blanks;
218000110811           leavesr;
218100110811         ENDIF;
218200110811
218300110811         ds4C = TBLuni;
218400110811
218500110811       //?Scrivo riga se trovo il codice unificante e i relativi flag
218600110811         IF  §4Ccks = *zeros or
218700110811            (§4Cfsc = *blanks and §4Cfan = *blanks and §4Cflg = *blanks);
218800110811           leavesr;
218900110811         ENDIF;
219000110811
219100110811         $RecOk = *on;
219200110811
219300110811       //?Controllo se il cliente o l'unificante sono abilitati al VAS
219400110811         wKscAlfa = %editc(§4Ccks:'X');
219500110811         wTip = 'VS';
219600110811         wUd  = 'D';
219700110811         exsr  Abi_VAS;
219800110811       //?Se non trovo con unificante ed è diverso provo con KSC
219900110811         IF  wKscAlfa = *blanks and TA61ksc <> §4Ccks;
220000110811           wKscAlfa = %editc(TA61ksc:'X');
220100110811           exsr  Abi_VAS;
220200110811         ENDIF;
220300110811
220400110811       //?Imposto i dati da emettere nel subfile
220500110811         exsr Dati_x_S01;
220600110811
220700110811       //?Se non ho trovato abilitazione VAS lo indico nella descrizione
220800110811         IF  wKscAlfa = *blanks;
220900110811           V1Sdes = %trim(V1Sdes) + ' - non abilitato a VAS';
221000110811         ENDIF;
221100110811
221200110811       ENDSR ;
221300110809
221400110809       //--------------------------------------------------------------
221500110811       //?Tabella 4C VAF - fatture.
221600110809       //--------------------------------------------------------------
221700110811       BEGSR tab4C_VAF;
221800110809
221900120207       //?Apro il file e forzo la lettura da Sede
222000120207         $Sede = *on;
222100110809         exsr OpenFileTab;
222200110809
222300110809         k_TBLcod = %subst($Abc(yy):1:2);
222400110809         k_TBLkey = %editc(TA61ksc:'X');
222500110809         chain %kds(K03tabel) TABEL00F;
222600110809         IF  not %found(TABEL00F) or TBLflg <> *blanks;
222700110809           leavesr;
222800110809         ENDIF;
222900110809
223000110809         ds4C = TBLuni;
223100110811
223200110811       //?Scrivo riga se trovo il codice unificante
223300110811         IF  §4Cfks = *zeros;
223400110811           leavesr;
223500110811         ENDIF;
223600110811
223700110811         $RecOk = *on;
223800110811
223900110811       //?Controllo se il cliente o l'unificante sono abilitati al VAS
224000110811         wKscAlfa = %editc(§4Cfks:'X');
224100110811         wTip = 'VF';
224200110811         wUd  = 'D';
224300110811         exsr  Abi_VAS;
224400110811       //?Se non trovo con unificante ed è diverso provo con KSC
224500110811         IF  wKscAlfa = *blanks and TA61ksc <> §4Cfks;
224600110809           wKscAlfa = %editc(TA61ksc:'X');
224700110809           exsr  Abi_VAS;
224800110811         ENDIF;
224900110811
225000110811       //?Imposto i dati da emettere nel subfile
225100110811         exsr Dati_x_S01;
225200110811
225300110811       //?Se non ho trovato abilitazione VAS lo indico nella descrizione
225400110811         IF  wKscAlfa = *blanks;
225500110811           V1Sdes = %trim(V1Sdes) + '- non abilitato a VAS';
225600110809         ENDIF;
225700110809
225800110809       ENDSR ;
225900110722
226000110722       //--------------------------------------------------------------
226100110722       //?Tabella 7U.
226200110722       //--------------------------------------------------------------
226300110722       BEGSR tab7U;
226400110808
226500110808       //?Apro il file
226600110808         exsr OpenFileTab;
226700110722
226800110805         k_TBLcod = %subst($Abc(yy):1:2);
226900111107         sav_AUT  = $Aut(yy);
227000110805
227100110808       //?Leggo per particolarità giacenza
227200110805         k_TBLkey = %editc(TA61ksc:'X') + 'Q';
227300110805         chain %kds(K03tabel) TABEL00F;
227400110805         IF  %found(TABEL00F) and TBLflg = *blanks;
227500110805
227600110805           ds7U = TBLuni;
227700110805
227800110808         //?Creo 1 riga di subfile x ogni particolarità giacenza
227900110808           zz = 1;
228000110808           FOR zz by 1 to %elem($Part);
228100110808             IF  $Part(zz) <> *blanks;
228200111107           //?Cerco in tab. 7W chi autorizza
228300111107               ww = %lookup($Part(zz):$7W);
228400111107               IF  ww > 0 and $7Waut(ww) <> *blanks;
228500111107                 $Aut(yy) = $7Waut(ww);
228600110808               exsr Dati_x_S01;
228700110808               V1Sdes = %trim(V1Sdes) + ' ' + 'Giacenza ' + $Part(zz);
228800110808               V1Hpgm = 'TB69';
228900110808               V1Hipers = 'S';
229000110808               V1Hpart = $Part(zz);
229100110808               exsr WrtS01;
229200111107               ENDIF;
229300110808             ENDIF;
229400110808           ENDFOR;
229500111107           $Aut(yy) = sav_AUT;
229600110808         //?Creo 1 riga di subfile x ogni flag
229700110808           IF  §7Usre <> *blanks;
229800110808             exsr Dati_x_S01;
229900110808             V1Sdes = %trim(V1Sdes) + ' ' +
230000110808                     'Reso anche con trazioni ridotte';
230100110808             clear V1Hpgm;
230200110808             clear V1Hipers;
230300110808             clear V1Hpart;
230400110808             exsr WrtS01;
230500110808           ENDIF;
230600110808           IF  §7Uldcr <> *blanks;
230700110808             exsr Dati_x_S01;
230800110808             V1Sdes = %trim(V1Sdes) + ' ' + §7Uldcr +
230900110808                      ' GG oltre limite apertura giacenza' +
231000110808                      ' per data cons. richiesta';
231100110808             clear V1Hpgm;
231200110808             clear V1Hipers;
231300110808             clear V1Hpart;
231400110808             exsr WrtS01;
231500110808           ENDIF;
231600110805         ENDIF;
231700110808
231800110808       //?Leggo per particolarità consegna
231900110808         k_TBLkey = %editc(TA61ksc:'X') + 'R';
232000110808         chain %kds(K03tabel) TABEL00F;
232100110808         IF  %found(TABEL00F) and TBLflg = *blanks;
232200110808
232300110808           ds7U = TBLuni;
232400110808
232500110808         //?Creo 1 riga di subfile x ogni particolarità consegna
232600110808           zz = 1;
232700110808           FOR zz by 1 to %elem($Part);
232800110808             IF  $Part(zz) <> *blanks;
232900111107           //?Cerco in tab. 7R chi autorizza
233000111107               ww = %lookup($Part(zz):$7R);
233100111107               IF  ww > 0 and $7Raut(ww) <> *blanks;
233200111107                 $Aut(yy) = $7Raut(ww);
233300110808               exsr Dati_x_S01;
233400110808               V1Sdes = %trim(V1Sdes) + ' ' + 'Consegna ' + $Part(zz);
233500110808               V1Hpgm = 'TB70';
233600110808               V1Hipers = 'S';
233700110808               V1Hpart = $Part(zz);
233800110808               exsr WrtS01;
233900111107               ENDIF;
234000110808             ENDIF;
234100110808           ENDFOR;
234200110808         ENDIF;
234300110722
234400110722       ENDSR ;
234500110802
234600110802       //--------------------------------------------------------------
234700110802       //?Dati in S01.
234800110802       //--------------------------------------------------------------
234900110802       BEGSR Dati_x_S01;
235000110802
235100110802         clear  TA61S01;
235200110804         V1Hkey   = $Abc(yy);
235300110804         V1Hpgm   = $Pgm(yy);
235400110804         V1Hipers = $Ipers(yy);
235500110804         V1Sdes   = $Des(yy);
235600111028         clear V1Saut;
235700111028         clear TIBS02ds;
235800111028         T02mod = 'C';
235900111028         T02cod = 'RAC';
236000111028         T02ke1 = $Aut(yy);
236100111028         T02sif = KNSIF;
236200111028         TNTBE_RicercaControllo  (kpjba : tibs02ds);
236300111028         IF  T02err = *blanks;
236400111028           V1Saut = T02Uni;
236500111028         ENDIF;
236600110802
236700110802       ENDSR ;
236800110809
236900110809       //--------------------------------------------------------------
237000110809       //?Controllo abilitazioni VAB
237100110809       //--------------------------------------------------------------
237200110809       BEGSR Abi_VAB;
237300110809
237400110809       //?Sql diviso pe ambiente, prova o produzione
237500110809         IF  $Prova;
237600110809           exec sql
237700110809           SELECT distinct VIRksc into :VIRksc
237800110809           FROM GAITRAGRPS/TIVIR00F
237900110809           WHERE :wOggi between VIRdti and VIRdtf and
238000110809                 :wKscAlfa = substr(VIRksc, 2, 7) and
238100110907                 (VIRfit in('FIVABWWR', 'EDIVABWR') or
238200110809                 VIRtip in (SELECT VTFtip
238300110809                  FROM GAITRAGRPS/TIVTF00F
238400110809                  WHERE VTFud = :wUd and
238500110907                  VTFfit in('FIVABWWR', 'EDIVABWR')));
238600110809         ELSE;
238700110809           exec sql
238800110809           SELECT distinct VIRksc into :VIRksc
238900110809           FROM GAITRAGRU/TIVIR00F
239000110809           WHERE :wOggi between VIRdti and VIRdtf and
239100110809                 :wKscAlfa = substr(VIRksc, 2, 7) and
239200110907                 (VIRfit in('FIVABWWR', 'EDIVABWR') or
239300110809                 VIRtip in (SELECT VTFtip
239400110809                  FROM GAITRAGRU/TIVTF00F
239500110809                  WHERE VTFud = :wUd and
239600110907                  VTFfit in('FIVABWWR', 'EDIVABWR')));
239700110809         ENDIF;
239800110809
239900110809       //?Non trovo il codice pulisco il campo wKscAlfa
240000110809         IF sqlcod <> 0;
240100110809           clear wKscAlfa;
240200110809         ENDIF;
240300110809
240400110809       ENDSR;
240500110809
240600110809       //--------------------------------------------------------------
240700110809       //?Controllo abilitazioni VAS.
240800110809       //--------------------------------------------------------------
240900110809       BEGSR Abi_VAS;
241000110809
241100110809       //?Sql diviso pe ambiente, prova o produzione
241200110809         IF  $Prova;
241300110809           exec sql
241400110809           SELECT distinct VIRksc into :VIRksc
241500110809           FROM GAITRAGRPS/TIVIR00F
241600110809           WHERE :wOggi between VIRdti and VIRdtf and
241700110809                 :wKscAlfa = substr(VIRksc, 2, 7) and
241800110809                 (VIRtip = :wTip or VIRtip in
241900110809                 (SELECT VTFtip
242000110809                  FROM GAITRAGRPS/TIVTF00F
242100110809                  WHERE VTFud = :wUd and VTFtip = :wTip));
242200110809         ELSE;
242300110809           exec sql
242400110809           SELECT distinct VIRksc into :VIRksc
242500110809           FROM GAITRAGRU/TIVIR00F
242600110809           WHERE :wOggi between VIRdti and VIRdtf and
242700110809                 :wKscAlfa = substr(VIRksc, 2, 7) and
242800110809                 (VIRtip = :wTip or VIRtip in
242900110809                 (SELECT VTFtip
243000110809                  FROM GAITRAGRU/TIVTF00F
243100110809                  WHERE VTFud = :wUd and VTFtip = :wTip));
243200110809         ENDIF;
243300110809
243400110809       //?Non trovo il codice pulisco il campo wKscAlfa
243500110809         IF sqlcod <> 0;
243600110809           clear wKscAlfa;
243700110809         ENDIF;
243800110809
243900110809       ENDSR;
244000100720
244100100720       //--------------------------------------------------------------
244200110615       //?Gestione tasto funzionale F03 da videata D01/S01.
244300100720       //?F03=Fine
244400100720       //--------------------------------------------------------------
244500110615       BEGSR F03D01;
244600100720
244700110809       //?Chiusura del programma
244800100720         $Fine = *on;
244900100720
245000100720       ENDSR;
245100110617
245200110617       //--------------------------------------------------------------
245300110617       //?Gestione tasto funzionale F12 da videata S01.
245400110617       //?F12=Ritorno
245500110617       //--------------------------------------------------------------
245600110617       BEGSR F12S01;
245700110617
245800110617         //?Chiusura del programma se pgm richiamato
245900110617         IF  $Richiamato;
246000110617           $Fine = *on;
246100110617           leavesr;
246200110617         ENDIF;
246300110617
246400110617         //?Ritorno alla prima videata se pgm NON richiamato
246500110617         IF  not $Richiamato;
246600110617           $Video  = 'D1';
246700110617           $InzD01 = *on;
246800110617           leavesr;
246900110617         ENDIF;
247000110617
247100110617       ENDSR;
247200100622
247300100622       //--------------------------------------------------------------
247400100622       //?Gestione opzioni subfile
247500100622       //--------------------------------------------------------------
247600100622       BEGSR OpzS01;
247700100720
247800110615         readc TA61S01;
247900100622
248000110615         DOW  NOT  %eof(TNTA61D);
248100100622
248200100722           SflNxtChg = *off;
248300100622           WindDspF  = IndDspF;
248400100622           reset WindDspFb;
248500100622           IndDspF   = WindDspF;
248600110615           V1Crcd    = S01nrr;
248700110307
248800110307           PosCurOpz = *off;
248900100622
249000100622           SELECT;
249100100622
249200100624             //?- Nessuna opzione
249300110615             WHEN  V1sopz  = *blank;
249400110411
249500110411             //?- 5 = Visualizza
249600110615             WHEN  V1Sopz  = '5';
249700110615               //?richiamo pgm dettaglio
249800110804               exsr Visualizza;
249900100622
250000100624             //?- ? = Opzione NON valida
250100100622             OTHER;
250200100624               ErrMessage  = *on;
250300100624               ErrGenerico = *on;
250400100622               PosCurOpz   = *on;
250500110615               V1Cmsg = $Msg(02);
250600100622
250700100622           ENDSL;
250800100622
250900100624           //?Aggiornamento sfl
251000100624           SELECT;
251100100624             WHEN  ErrMessage;
251200100622               SflNxtChg = *on;
251300110615               V1Ccsr    = V1Crcd;
251400100624             WHEN  ErrGenerico;
251500110615               V1Ccsr    = V1Crcd;
251600110615               clear  V1Sopz;
251700100624             OTHER;
251800110615               V1Ccsr    = V1Crcd;
251900110615               clear  V1Sopz;
252000100624           ENDSL;
252100110802
252200110802           Proteggi = V1Hin40;
252300100622
252400110615           update TA61S01;
252500100622
252600100624           IF  ErrMessage or ErrGenerico;
252700100622             leave;
252800100624           ENDIF;
252900100622
253000110615           readc TA61S01;
253100100622
253200100622         ENDDO;
253300100622
253400100622       ENDSR;
253500110804
253600110804       //--------------------------------------------------------------
253700110804       //?Visualizzazione a dettaglio.
253800110804       //--------------------------------------------------------------
253900110804       BEGSR Visualizza;
254000110804
254100110804       //?Visualizzazione standard
254200110804         IF  V1Hipers = *blanks;
254300110804           exsr Call_pgm;
254400110804       //?Visualizzazione personalizzata
254500110804         ELSE;
254600110804           exsr Call_pgm_ABC;
254700110804         ENDIF;
254800110804
254900110804       ENDSR;
255000100720
255100100720       //--------------------------------------------------------------
255200110802       //?Richiamo programma di interrogazione a dettaglio.
255300100720       //--------------------------------------------------------------
255400110802       BEGSR Call_pgm;
255500110802
255600111017         clear kpjbu;
255700110802         ParmPgm = V1Hpgm;
255800110803         tnta61c (kpjba:Tnta61ds:ParmPgm);
255900110804         IF  TA61err = *on;
256000110804           ErrMessage  = *on;
256100110804           ErrGenerico = *on;
256200110804           PosCurOpz   = *on;
256300110804           V1Cmsg = TA61msg;
256400110804         ENDIF;
256500100720
256600100720       ENDSR;
256700110804
256800110804       //--------------------------------------------------------------
256900110804       //?Richiamo programma personalizzato per abilitazione.
257000110804       //--------------------------------------------------------------
257100110804       BEGSR Call_pgm_ABC;
257200110804
257300110808         SELECT;
257400110811
257500110808           WHEN  V1Hkey = '7U' and V1Hpgm = 'TB69';
257600110808             clear ds7PQRS;
257700110808             ds7ric = 'S';
257800110808             ds7ges = 'V';
257900110808             ds7cod = V1Hpart;
258000110808             kpjbu = ds7PQRS;
258100110808             trtb69r (kpjba);
258200111017
258300110808           WHEN  V1Hkey = '7U' and V1Hpgm = 'TB70';
258400110808             clear ds7PQRS;
258500110808             ds7ric = 'S';
258600110808             ds7ges = 'V';
258700110808             ds7cod = V1Hpart;
258800110808             kpjbu = ds7PQRS;
258900110808             trtb70r (kpjba);
259000110811
259100110808         ENDSL;
259200110804
259300110804       ENDSR;
259400111027
259500111027       //--------------------------------------------------------------
259600111027       //?Stampa.
259700111027       //--------------------------------------------------------------
259800111027       BEGSR Stampa;
259900111027
260000111027       //?Richiamato con nuovo codice
260100111027         *in80 = TA61newksc > 0;
260200111027
260300111027       //?Apro il file di stampa
260400111027         open TNTA61P;
260500111027
260600111027         clear S01nrr;
260700111027         $End = *off;
260800121217
260900121217       //?Se il cliente non ha abiltiazioni attive
261000121217       //?e non sono passata dal subfile stampo una riga di avviso
261100121217         IF  $Video = 'ST' and sav_s01nrr = 0;
261200121217           clear V1Dksc;
261300121217           clear tibs69ds;
261400121217           I69kac = TA61ksc;
261500121217           TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
261600121217           V1Dksc = ACOrag;
261700121217           STPksc  = TA61ksc;
261800121217           STPrag  = V1Dksc;
261900121217           STPkscn = TA61newksc;
262000121217           write TA61testa;
262100121217           STPdes = 'Nessuna abilitazione presente';
262200121217           write TA61riga;
262300121217           $End = *on;
262400121217           $Testa = *on;
262500121217         ENDIF;
262600111027
262700111027       //?Leggo il subfile caricato
262800111027         DOW not $End;
262900111027           S01nrr += 1;
263000111027           chain S01nrr TA61S01;
263100111027           IF  not %found;
263200111027             $End = *on;
263300111027             leave;
263400111027           ENDIF;
263500111027
263600111027         //?Stampo la testata
263700111027           IF  not $Testa or *in90;
263800111104           //?Se non sono passata dal subfile cerco la ragione sociale
263900111104             IF  $Video = 'ST';
264000111104               clear V1Dksc;
264100111104               clear tibs69ds;
264200111104               I69kac = TA61ksc;
264300111104               TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
264400111104               V1Dksc = ACOrag;
264500111104             ENDIF;
264600111027             STPksc  = TA61ksc;
264700111027             STPrag  = V1Dksc;
264800111027             STPkscn = TA61newksc;
264900111027             write TA61testa;
265000111027             *in90 = *off;
265100111027             $Testa = *on;
265200111027           ENDIF;
265300111027
265400111027           STPdes = V1Sdes;
265500111027           STPaut = V1Saut;
265600111027           write TA61riga;
265700111027
265800111027           update TA61S01;
265900111027
266000111027         ENDDO;
266100111027
266200111027         //?Stampo la fine se ho stampato almeno la testata
266300111027         IF  $Testa;
266400111027           write TA61fine;
266500111027         ENDIF;
266600111027
266700111027         close TNTA61P;
266800111104         $Fine = *on;
266900111027
267000111027       ENDSR ;
267100080206
267200080206       //--------------------------------------------------------------
267300080206       //?Operazioni finali.
267400080206       //--------------------------------------------------------------
267500080206       BEGSR RoutEnd;
267600111026
267700111026       //?Se richiamato per controllo imposto il flag di ritorno
267800111026         IF  $Controlla;
267900111026           TA61flg = $RecOK;
268000111026         ENDIF;
268100080206
268200080206         *inLR = *on;
268300080206         return;
268400080206
268500080206       ENDSR;
268600080206
268700080206      /end-free
268800100624
268900080206       //--------------------------------------------------------------
269000080206       //?Schiere a tempo di compilazione.
269100080206       //--------------------------------------------------------------
269200080206
269300080206** - $MSG -------------------------------------------------------------------*
269400110615Codice cliente errato o non in gestione all'utente.                            1
269500110411Opzione errata                                                                 2
