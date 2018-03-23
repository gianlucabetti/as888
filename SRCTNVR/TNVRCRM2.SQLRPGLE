000100100413      //---------------------------------------------------------------
000200100414      //?TNVRCRM2 - Da file work scrivo nuovo file trattative
000300100413      //---------------------------------------------------------------
000400100413
000500100413     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000600100413
000700100413      //---------------------------------------------------------------
000800100413      //?Dichiarazione file.
000900100413      //---------------------------------------------------------------
001000100415     fCNACO16L  if   e           k disk
001100100415     fTABEL00F  if   e           k disk
001200100415     fTFNTC01L  if   e           k disk    rename(TFNTC:TFNTC01)
001300100415     f                                     prefix(W_)
001400100415     fTFNTC00F  o    e             disk
001500100415     fTIATC00F  o    e             disk
001600100413     fTICPN00F  o    e             disk
001700100415     fTICPS00F  o    e             disk
001800100413     fTIVIS00F  o    e             disk
001900100413     fTIVOF00F  o    e             disk
002000100415     fTNOFM01L  if   e           k disk
002100100414     fTNVIS05L  if   e           k disk    prefix(N_)
002200100414     fTNVRCRMP  o    e             printer oflind(*in99)
002300100413
002400100413      //---------------------------------------------------------------
002500100413      //?Definizione costanti.
002600100413      //---------------------------------------------------------------
002700100413
002800100413      //---------------------------------------------------------------
002900100413      //?Definizione schiere.
003000100413      //---------------------------------------------------------------
003100100513
003200100513     d filiali         s              3a   dim(10)
003300090715
003400100413      //---------------------------------------------------------------
003500100413      //?Definizione aree dati.
003600100413      //---------------------------------------------------------------
003700100413
003800100413      //---------------------------------------------------------------
003900100413      //?Definizione strutture dati.
004000100413      //---------------------------------------------------------------
004100100415
004200100415     d KPJBA         e ds
004300100415
004400100415      // - Ricerca ultimo numero attività
004500100415     d TRUL33ds      e ds                  inz
004600100415
004700100415      // - Aggiunge/Toglie gg/mm dalla data
004800100415     d XGIOLAVds     e ds                  inz
004900100413
005000100413      // File work
005100100413     d WFVISds       e ds                  extname(WFVIS00F)
005200100413
005300100413      // ds x campo TAMflo
005400100413     d dsta01        e ds
005500100415
005600100415      // - Tabella 01 = Codici Commerciali
005700100415     d ds01          e ds                  inz
005800100415
005900100413      //---------------------------------------------------------------
006000100413      //?Definizione variabili globali.
006100100413      //---------------------------------------------------------------
006200100413
006300100413      // - Flags booleani
006400100413     d $End            s               n   inz(*off)
006500100413     d $Fine           s               n   inz(*off)
006600100415     d $MaiCodificato  s               n   inz(*off)
006700100513     d $NotaIN         s               n   inz(*off)
006800100419     d $NotaF          s               n   inz(*off)
006900100414     d $Stampa         s               n   inz(*off)
007000100415     d $Vof            s               n   inz(*off)
007100100413
007200100413      // - Campi di comodo
007300100415     d old_WIScmm      s                   like(WIScmm)
007400100415     d old_WIScpo      s                   like(WIScpo)
007500100415     d old_WISksc      s                   like(WISksc)
007600100415     d sav_CPNpno      s                   like(CPNpno)
007700100513     d TotCps          s              7s 0
007800100513     d TotFfz          s              7s 0
007900100513     d TotNote         s              7s 0
008000100513     d TotTrat         s              7s 0
008100100513     d Tot30           s              7s 0
008200100513     d Tot80           s              7s 0
008300100513     d Tot91           s              7s 0
008400100415     d wCad            s                   like(ATCcad)
008500100415     d wDad            s                   like(ATCdad)
008600100416     d wData           s              8  0
008700100415     d wDataOff        s              8  0
008800100415     d wDataOggi       s              8  0
008900100414     d wData_ISO       s               d   datfmt(*iso)
009000100416     d wHda            s                   like(ATChda)
009100100415     d wOfi            s                   like(ATCofi)
009200100415     d wOii            s                   like(ATCoii)
009300100415     d wRiga           s              2  0
009400100415     d wTat            s                   like(ATCtat)
009500100413
009600100413      //---------------------------------------------------------------
009700100413      //?Definizione procedure esterne.
009800100413      //---------------------------------------------------------------
009900100415
010000100415      // - Aggiunge/Toglie gg/mm dalla data
010100100415     d Xgiolav         pr                  extpgm('XGIOLAV')
010200100415     d  xgiolavds                          likeds(xgiolavds)
010300100413
010400100413      //---------------------------------------------------------------
010500100413      //?prototipi
010600100413      //---------------------------------------------------------------
010700100414
010800100415      /copy gaitrasrc/srcprotopr,trul33r
010900100413
011000100413      //---------------------------------------------------------------
011100100413      //?Definizione key-list.
011200100413      //---------------------------------------------------------------
011300100415
011400100415       // - File TABEL00F
011500100415     d k03tabel      e ds                  extname(TABEL00F:*key)
011600100415     d                                     prefix(k_)
011700100413
011800100413      //---------------------------------------------------------------
011900100413      //?Riepilogo indicatori.
012000100413      //---------------------------------------------------------------
012100100414
012200100414      // 99    : Salto pagina
012300100413
012400100413      //---------------------------------------------------------------
012500100413      //?M A I N - L I N E
012600100413      //---------------------------------------------------------------
012700100413
012800100413     c     *Entry        plist
012900100513     c                   parm                    filiali
013000100413
013100100413      /free
013200100413
013300100413       //?Operazioni iniziali
013400100513       exsr RoutInz;
013500100413
013600100413       //?Scrivo i nuovi file per le trattative
013700100413       IF  not $Fine;
013800100513         exsr Elabora;
013900100413       ENDIF;
014000100413
014100100413       //?Operazioni finali
014200100513       exsr RoutEnd;
014300100413
014400100413       //--------------------------------------------------------------
014500100413       //?Operazioni iniziali.
014600100413       //--------------------------------------------------------------
014700100513       BEGSR RoutInz;
014800100414
014900100414         wData_ISO = %date();
015000100415         wDataOggi = %dec(wData_ISO);
015100100413
015200100413         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
015300100416
015400100416       //?Impostazione campi "fissi"
015500100416         k_TBLkut = 1;
015600100513
015700100513         clear TotCps;
015800100513         clear TotFfz;
015900100513         clear TotNote;
016000100513         clear TotTrat;
016100100513         clear Tot30;
016200100513         clear Tot80;
016300100513         clear Tot91;
016400100413
016500100413       ENDSR;
016600100413
016700100413       //--------------------------------------------------------------
016800100422       //?Elaboro.
016900100413       //--------------------------------------------------------------
017000100513       BEGSR Elabora;
017100100413
017200100413         $End = *off;
017300100413
017400100513       //?Leggo file di WORK solo visite da trascodificare
017500100413         exec sql
017600100413         DECLARE VIS cursor for
017700100413         SELECT  WFVIS00F.*
017800100413         FROM WFVIS00F
017900100513         WHERE WISnewf <> 'N'
018000100513         ORDER by WISnrv;
018100100413
018200100413         exec sql OPEN VIS;
018300100413
018400100413         DOW  not $End;
018500100413           exec sql FETCH next from VIS into :WFVISds;
018600100413           IF  sqlcod = 100 or sqlcod < 0;
018700100413             $End = *on;
018800100413             leave;
018900100413           ENDIF;
019000100413
019100100413           exsr Scrivi;
019200100513         //?Conto le trattative scritte
019300100513           TotTrat += 1;
019400100513
019500100413         ENDDO;
019600100413
019700100413         exec sql CLOSE VIS;
019800100513
019900100513
020000100513         $End = *off;
020100100513
020200100513       //?Leggo file di WORK solo visite NON da trascodificare e Nuove
020300100513         exec sql
020400100513         DECLARE VISN cursor for
020500100513         SELECT  WFVIS00F.*
020600100513         FROM WFVIS00F
020700100513         WHERE WISnewf = 'N' and WIStpv = 'N'
020800100513         ORDER by WISnrv;
020900100513
021000100513         exec sql OPEN VISN;
021100100513
021200100513         DOW  not $End;
021300100513           exec sql FETCH next from VISN into :WFVISds;
021400100513           IF  sqlcod = 100 or sqlcod < 0;
021500100513             $End = *on;
021600100513             leave;
021700100513           ENDIF;
021800100513
021900100513           exsr Scrivi_NoteIN;
022000100513
022100100513         ENDDO;
022200100513
022300100513         exec sql CLOSE VISN;
022400100513
022500100513       //?Stampa totali
022600100513         wRiga = 00;
022700100513         exsr Stampa;
022800100513
022900100513       ENDSR;
023000100413
023100100413       //--------------------------------------------------------------
023200100413       //?Scrivo i nuovi file.
023300100413       //--------------------------------------------------------------
023400100413       BEGSR Scrivi;
023500100419
023600100413       //?scrivo la trattativa
023700100413         exsr Scrivi_Vis;
023800100413
023900100413       //?scrivo la testata delle offerte
024000100413         exsr Scrivi_Vof;
024100100414
024200100414       //?scrivo le note
024300100414         exsr Scrivi_Note;
024400100415
024500100415       //?scrivo le attività e gli stati
024600100419       //?se trattativa NON fittizia
024700100513         IF  WISnffz = *blanks;
024800100419           exsr Scrivi_NewFile;
024900100419         ENDIF;
025000100413
025100100413       ENDSR;
025200100419
025300100419       //--------------------------------------------------------------
025400100419       //?Scrivo note INTERNE.
025500100419       //--------------------------------------------------------------
025600100419       BEGSR Scrivi_NoteIN;
025700100419
025800100419         clear sav_CPNpno;
025900100513         $NotaIN = *off;
026000100419
026100100419         w_NTCapl = 'V';
026200100419         w_NTCnk1 = '0151' + %editc(WISnrv:'X');
026300100419         clear w_NTCnk2;
026400100419         w_NTCtnt = 'IN';
026500100419         setll  (w_NTCapl : w_NTCnk1 : w_NTCnk2 : w_NTCtnt) TFNTC01L;
026600100419         reade  (w_NTCapl : w_NTCnk1 : w_NTCnk2 : w_NTCtnt) TFNTC01L;
026700100419         DOW  not %eof(TFNTC01L);
026800100513         //?se NTCntr = 0 non trascodifico sono cose vecchie e poche
026900100419           IF  w_NTCntr > 0;
027000100419             VIScpo = WIScpo;
027100100419             clear VISksc;
027200100419             clear VISnrv;
027300100419             exsr Scrivi_TICPN;
027400100513             $NotaIN = *on;
027500100419           ENDIF;
027600100419
027700100419           reade  (w_NTCapl : w_NTCnk1 : w_NTCnk2 : w_NTCtnt) TFNTC01L;
027800100419         ENDDO;
027900100513
028000100513         //?Conto le trattative non scritte ma di cui ho scritto note
028100100513         IF  $NotaIN;
028200100513           TotNote += 1;
028300100513         ENDIF;
028400100419
028500100419       ENDSR;
028600100413
028700100413       //--------------------------------------------------------------
028800100413       //?Scrivo file trattative.
028900100413       //--------------------------------------------------------------
029000100413       BEGSR Scrivi_Vis;
029100100413
029200100413         clear TIVIS000;
029300100413         VIScmm  = WIScmm;
029400100413         VIScmmi = WIScmm;
029500100513         VISdat  = WISdvi;
029600100513         VIStpv  = WISntpv;
029700100513         VISffz  = WISnffz;
029800100513         VIScpo  = WIScpo;
029900100513         VISksc  = WISksc;
030000100513         VISnrv  = WISnrv;
030100100513         VISfil  = %dec(%subst(%editc(VIScmm:'X'):1:3):3:0);
030200100413
030300100419         $NotaF = *off;
030400100415       //?Se ho modificato la trattativa da buona a fittizia scrivo nota
030500100419       //?ma lo faccio dopo che ho scritto le altre note INTERNE
030600100513         IF  WISffz = *blanks and WISnffz = 'S';
030700100513           $NotaF = *on;
030800100413         ENDIF;
030900100517
031000100517         VISrag = WISrag;
031100100517         VIScnw = WIScnw;
031200100413
031300100413         write TIVIS000;
031400100413
031500100413       ENDSR;
031600100413
031700100413       //--------------------------------------------------------------
031800100413       //?Scrivo file testata offerte.
031900100413       //--------------------------------------------------------------
032000100413       BEGSR Scrivi_Vof;
032100100415
032200100415         $Vof = *off;
032300100415         clear wDataOff;
032400100413
032500100413         setll VISnrv TNOFM01L;
032600100413         reade VISnrv TNOFM01L;
032700100413         DOW not %eof(TNOFM01L);
032800100413
032900100415           $Vof = *on;
033000100413           dsta01 = TAMflo;
033100100413
033200100413           clear TIVOF000;
033300100413           VOFnrv = TAMksc;
033400100413           VOFctr = TAMctr;
033500100413           VOFprg = TAMprg;
033600100413         //?Tipo tariffa italia/estero
033700100413           VOFtpt = TAMfie;
033800100413         //?Tipo tariffa DPD/FedEx
033900100413           IF  §tadpd = 'S';
034000100413             VOFtpt = 'D';
034100100413           ENDIF;
034200100413           IF  §tafed = 'S';
034300100413             VOFtpt = 'F';
034400100413           ENDIF;
034500100413         //?data presentazione offerta la recupero dal TNVIS in base al tipo tariffa
034600100413           IF  VOFtpt = 'I';
034700100514             VOFdpo = WISdpo;
034800100413           ELSE;
034900100514             VOFdpo = WISdoe;
035000100413           ENDIF;
035100100413           IF  VOFdpo = 0;
035200100413             VOFdpo = TAMduv;
035300100415             wRiga = 02;
035400100513             exsr Stampa;
035500100413           ENDIF;
035600100413
035700100413           write TIVOF000;
035800100415
035900100415           //?tengo la data presentazione offerta più alta
036000100426           IF  wDataOff < VOFdpo;
036100100426             wDataOff = VOFdpo;
036200100426           ENDIF;
036300100413
036400100413           reade VISnrv TNOFM01L;
036500100413         ENDDO;
036600100413
036700100413       ENDSR;
036800100414
036900100414       //--------------------------------------------------------------
037000100414       //?Scrivo file note.
037100100414       //--------------------------------------------------------------
037200100414       BEGSR Scrivi_Note;
037300100414
037400100415         clear sav_CPNpno;
037500100415
037600100415         w_NTCapl = 'V';
037700100415         w_NTCnk1 = '0151' + %editc(VISnrv:'X');
037800100415         setll  (w_NTCapl : w_NTCnk1) TFNTC01L;
037900100415         reade  (w_NTCapl : w_NTCnk1) TFNTC01L;
038000100414         DOW  not %eof(TFNTC01L);
038100100414         //?Se trattativa legata a cliente
038200100415         //?le note di tipo 10 le scrivo con 'T'
038300100414         //?le note IN le scrivo sul nuovo TICPN00F
038400100414           IF  VISksc > 0;
038500100414           //?Note tariffa '10'
038600100415             IF  w_NTCtnt = '10';
038700100415               exsr Scrivi_TFNTC;
038800100415             ENDIF;
038900100414           //?Note interne 'IN' (se NTCntr = 0 non trascodifico sono cose vecchie e poche)
039000100416             IF  w_NTCtnt = 'IN' and w_NTCntr > 0;
039100100415               exsr Scrivi_TICPN;
039200100414             ENDIF;
039300100414           ENDIF;
039400100415         //?Se trattativa NON legata a cliente
039500100415         //?le note diverse da IN e da 30 le scrivo con 'T'
039600100415         //?le 30 non le trascodifico
039700100415         //?le note IN le scrivo sul nuovo TICPN00F
039800100415           IF  VISksc = 0;
039900100415           //?Note tariffa NON 'IN' e NON '30'
040000100415             IF  w_NTCtnt <> 'IN' and w_NTCtnt <> '30';
040100100415               exsr Scrivi_TFNTC;
040200100415             ENDIF;
040300100415           //?Note interne 'IN' (se NTCntr = 0 non trascodifico sono cose vecchie e poche)
040400100416             IF  w_NTCtnt = 'IN' and w_NTCntr > 0;
040500100415               exsr Scrivi_TICPN;
040600100415             ENDIF;
040700100415           ENDIF;
040800100414
040900100415           reade  (w_NTCapl : w_NTCnk1) TFNTC01L;
041000100414         ENDDO;
041100100419
041200100419       //?Scrivo la nota relativa al cambio tipo trattativa
041300100419         IF  $NotaF;
041400100419           exsr Scrivi_Nota_F;
041500100513         //?conto quante note x cambio trattativa scrivo
041600100513           TotFfz += 1;
041700100419         ENDIF;
041800100414
041900100414       ENDSR;
042000100415
042100100415       //--------------------------------------------------------------
042200100415       //?Scrivo le attività e gli stati
042300100415       //--------------------------------------------------------------
042400100415       BEGSR Scrivi_NewFile;
042500100415
042600100415       //?se potenziale mai codificato
042700100415         $MaiCodificato = *off;
042800100415         chain VIScpo CNACO16L;
042900100415         IF  not %found(CNACO16L);
043000100415           $MaiCodificato = *on;
043100100419         //?Scrivo Stato '2' sul potenziale se mai codificato
043200100419           exsr Scrivi_TICPS;
043300100513         //?Conto quanti stati '2' ho creato
043400100513           TotCps += 1;
043500100415         ENDIF;
043600100419
043700100415       //?Trattativa con offerte
043800100415         IF  $Vof;
043900100415         //?Schedulo nuova attività 30-Contatto Post Offerta
044000100415           wTat = 'T';
044100100415           wCad = '30';
044200100513         //?Data attività schedulata a data presentazione offerta + 10 gg. lavorativi
044300100415           clear XGIOLAVDS;
044400100426         //?se data presentazione offerta minore data visita tengo data visita
044500100426           IF  wDataOff < WISdvi;
044600100426             IXGLdata = WISdvi;
044700100426           ELSE;
044800100426             IXGLdata = wDataOff;
044900100426           ENDIF;
045000100415           IXGLadd  = 'S';
045100100513           IXGLgg   = 10;
045200100415           IXGLpa   = 'P';
045300100415           IXGLfil = %dec(%subst(%editc(VIScmm:'X'):1:3):3:0);
045400100415           XGIOLAV (xgiolavds);
045500100415           wDad = OXGLdata;
045600100415           IF  WIShmv > 0;
045700100415             wHda = WIShmv * 100;
045800100415           ELSE;
045900100419             wHda = 103000;
046000100415           ENDIF;
046100100415           clear wOii;
046200100415           clear wOfi;
046300100415         //?Stacco nuovo numero attività
046400100415           exsr Stacca_Numero;
046500100423         //?Scrivo la nuova attività
046600100415           exsr Scrivi_TIATC;
046700100513         //?Conto quante attività 30
046800100513           Tot30 += 1;
046900100415         ENDIF;
047000100415
047100100415       //?Trattativa senza offerte
047200100415         IF  not $Vof;
047300100415         //?Data trattativa maggiore data trascodifica
047400100415           IF  VISdat > wDataOggi;
047500100415           //?Schedulo nuova attività 91-Appuntamento
047600100415             wTat = 'A';
047700100415             wCad = '91';
047800100415             wDad = VISdat;
047900100415             IF  WIShmv > 0;
048000100415               wHda = WIShmv * 100;
048100100415             ELSE;
048200100419               wHda = 103000;
048300100415             ENDIF;
048400100416             wOii = wHda / 100;
048500100416             wOfi = wHda / 100;
048600100415          //?Stacco nuovo numero attività
048700100415             exsr Stacca_Numero;
048800100423          //?Scrivo la nuova attività
048900100415             exsr Scrivi_TIATC;
049000100513          //?Conto quante attività 91
049100100513             Tot91 += 1;
049200100415           ENDIF;
049300100415         //?Data trattativa minore uguale data trascodifica
049400100415           IF  VISdat <= wDataOggi;
049500100415           //?Se trattativa di tipo 'N' nuova o 'A' aumento/incremento
049600100415           //?Schedulo nuova attività 80-Offerta da inviare
049700100415             IF  VIStpv = 'N' or VIStpv = 'A';
049800100415               wTat = 'O';
049900100415               wCad = '80';
050000100419               wDad = VISdat;
050100100415               IF  WIShmv > 0;
050200100415                 wHda = WIShmv * 100;
050300100415               ELSE;
050400100419                 wHda = 103000;
050500100415               ENDIF;
050600100415               clear wOii;
050700100415               clear wOfi;
050800100415            //?Stacco nuovo numero attività
050900100415               exsr Stacca_Numero;
051000100423            //?Scrivo la nuova attività
051100100415               exsr Scrivi_TIATC;
051200100513          //?Conto quante attività 80
051300100513             Tot80 += 1;
051400100415             ENDIF;
051500100415           ENDIF;
051600100415         ENDIF;
051700100415
051800100415       ENDSR;
051900100415
052000100415       //--------------------------------------------------------------
052100100415       //?Scrivo file TFNTC.
052200100415       //--------------------------------------------------------------
052300100415       BEGSR Scrivi_TFNTC;
052400100415
052500100415         clear TFNTC;
052600100415         NTCapl = 'T';
052700100415         NTCnk1 = w_NTCnk1;
052800100415         NTCnk2 = w_NTCnk2;
052900100415         NTCtnt = w_NTCtnt;
053000100415         NTCrnt = w_NTCrnt;
053100100415         NTCsns = w_NTCsns;
053200100416         NTCflt = 'W';
053300100415         NTCntr = w_NTCntr;
053400100415         write TFNTC;
053500100415
053600100415       ENDSR;
053700100415
053800100415       //--------------------------------------------------------------
053900100415       //?Scrivo file TICPN.
054000100415       //--------------------------------------------------------------
054100100415       BEGSR Scrivi_TICPN;
054200100415
054300100415         clear TICPN000;
054400100415         CPNcpo = VIScpo;
054500100415         CPNksc = VISksc;
054600100415         CPNnrv = VISnrv;
054700100416       //?Costruisco la data lunga 8 da data lunga 6
054800100416         IF  w_NTCntr  < 000101;
054900100416           wData = w_NTCntr + 19000000;
055000100416         ELSE;
055100100416           wData = w_NTCntr + 20000000;
055200100416         ENDIF;
055300100416         CPNdim = wData;
055400100428         CPNhim = 235700;
055500100415         CPNsns = 'S';
055600100416         CPNnot = w_NTCrnt;
055700100419         CPNpno = sav_CPNpno + 1;
055800100503         CPNpos = 046;
055900100503         CPNpru = 'BATCH';
056000100415         write TICPN000;
056100100415         sav_CPNpno = CPNpno;
056200100415
056300100415       ENDSR;
056400100419
056500100419       //--------------------------------------------------------------
056600100419       //?Scrivo nota per modifica da trattativa buona a trattativa Fittizia
056700100419       //--------------------------------------------------------------
056800100419       BEGSR Scrivi_Nota_F;
056900100419
057000100419         clear TICPN000;
057100100419         CPNcpo = VIScpo;
057200100419         CPNksc = VISksc;
057300100419         CPNnrv = VISnrv;
057400100419         CPNdim = wDataOggi;
057500100429         CPNhim = 235700;
057600100419         CPNsns = 'S';
057700100419         CPNnot = 'Trattativa modificata in FITTIZIA';
057800100419         CPNpno = sav_CPNpno + 1;
057900100503         CPNpos = 046;
058000100503         CPNpru = 'BATCH';
058100100419         write TICPN000;
058200100419
058300100419       ENDSR;
058400100415
058500100415       //--------------------------------------------------------------
058600100415       //?Scrivo file TICPS.
058700100415       //--------------------------------------------------------------
058800100415       BEGSR Scrivi_TICPS;
058900100415
059000100415         clear TICPS000;
059100100415         CPScpo = VIScpo;
059200100415         CPScmm = VIScmm;
059300100415         clear ds01;
059400100419         k_TBLcod = '01';
059500100415         k_TBLkey = %editc(CPScmm:'X');
059600100415         chain  %kds(K03tabel) TABEL00F;
059700100415         IF  %found(TABEL00F);
059800100415           ds01 = TBLuni;
059900100415         ENDIF;
060000100415         CPScmd = §01age;
060100100415         CPScmmi = CPScmm;
060200100415         CPScmdi = CPScmd;
060300100415         CPSfst = '2 ';
060400100415         CPSdst = VISdat;
060500100415         CPSksc = VISksc;
060600100419         CPSnrv = VISnrv;
060700100415         CPSdim = wDataOggi;
060800100419         CPShim = 1030;
060900100415         CPSpru = 'BATCH';
061000100415         CPSfle = %dec(%subst(%editc(CPScmm:'X'):1:3):3:0);
061100100415         write TICPS000;
061200100415
061300100415       ENDSR;
061400100415
061500100415       //--------------------------------------------------------------
061600100415       //?Stacco nuovo numero attività.
061700100415       //--------------------------------------------------------------
061800100415       BEGSR Stacca_Numero;
061900100415
062000100415         clear TRUL33ds;
062100100415         I33tla = 'L';
062200100415         I33ope = 0;
062300100415         I33cnu = 070;
062400100415         I33num = 1;
062500100415         I33aaa = *year;
062600100415         kpjbu  = TRUL33ds;
062700100415         trul33r (kpjba);
062800100415         TRUL33ds = kpjbu;
062900100415
063000100415       ENDSR;
063100100415
063200100415       //--------------------------------------------------------------
063300100415       //?Scrivo file TIATC.
063400100415       //--------------------------------------------------------------
063500100415       BEGSR Scrivi_TIATC;
063600100415
063700100415         clear TIATC000;
063800100415         ATCtat  = wTat;
063900100415         ATCatn  = O33Nrf;
064000100415         ATCatnp = 1;
064100100415         ATCcpo  = VIScpo;
064200100415         ATCksc  = VISksc;
064300100415         ATCnrv  = VISnrv;
064400100415         ATCcad  = wCad;
064500100415         ATCdad  = wDad;
064600100415         ATChda  = wHda;
064700100415         ATCcmm  = VIScmm;
064800100415         ATCoii  = wOii;
064900100415         ATCofi  = wOfi;
065000100415         ATCco3  = VIScmm;
065100100415         IF  $MaiCodificato;
065200100415           ATCcnw = 'P';
065300100415         ELSE;
065400100415           ATCcnw = 'C';
065500100415         ENDIF;
065600100415         ATCdim  = %dec(%date());
065700100415         ATChim  = %dec(%time());
065800100415         ATCpri  = 'BATCH';
065900100415         write TIATC000;
066000100415
066100100415       ENDSR;
066200100414
066300100414       //--------------------------------------------------------------
066400100414       //?Stampa errori.
066500100414       //--------------------------------------------------------------
066600100513       BEGSR Stampa;
066700100414
066800100414         IF  not $Stampa;
066900100414           write CRMT01;
067000100414           $Stampa = *on;
067100100426           DESpgm = 'Scrive Trattative e stati legati alle trattative';
067200100517           fil01 = %dec(Filiali(01):3:0);
067300100517           fil02 = %dec(Filiali(02):3:0);
067400100517           fil03 = %dec(Filiali(03):3:0);
067500100517           fil04 = %dec(Filiali(04):3:0);
067600100517           fil05 = %dec(Filiali(05):3:0);
067700100517           fil06 = %dec(Filiali(06):3:0);
067800100517           fil07 = %dec(Filiali(07):3:0);
067900100517           fil08 = %dec(Filiali(08):3:0);
068000100517           fil09 = %dec(Filiali(09):3:0);
068100100517           fil10 = %dec(Filiali(10):3:0);
068200100426           write CRMT02;
068300100414         ENDIF;
068400100414
068500100415         IF  wRiga = 02;
068600100415           write CRMD02;
068700100415         ENDIF;
068800100513
068900100513         IF  wRiga = 00;
069000100513           write CRMTOTT;
069100100513         ENDIF;
069200100414
069300100414       ENDSR;
069400100413
069500100413       //--------------------------------------------------------------
069600100413       //?Operazioni finali.
069700100413       //--------------------------------------------------------------
069800100513       BEGSR RoutEnd;
069900100413
070000100413         *inLR = *on;
070100100413         return;
070200100413
070300100413       ENDSR;
070400100413
070500100413      /end-free
