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
003100090715
003200100413      //---------------------------------------------------------------
003300100413      //?Definizione aree dati.
003400100413      //---------------------------------------------------------------
003500100413
003600100413      //---------------------------------------------------------------
003700100413      //?Definizione strutture dati.
003800100413      //---------------------------------------------------------------
003900100415
004000100415     d KPJBA         e ds
004100100415
004200100415      // - Ricerca ultimo numero attività
004300100415     d TRUL33ds      e ds                  inz
004400100415
004500100415      // - Aggiunge/Toglie gg/mm dalla data
004600100415     d XGIOLAVds     e ds                  inz
004700100413
004800100413      // File work
004900100413     d WFVISds       e ds                  extname(WFVIS00F)
005000100413
005100100413      // ds x campo TAMflo
005200100413     d dsta01        e ds
005300100415
005400100415      // - Tabella 01 = Codici Commerciali
005500100415     d ds01          e ds                  inz
005600100415
005700100413      //---------------------------------------------------------------
005800100413      //?Definizione variabili globali.
005900100413      //---------------------------------------------------------------
006000100413
006100100413      // - Flags booleani
006200100413     d $End            s               n   inz(*off)
006300100413     d $Fine           s               n   inz(*off)
006400100415     d $MaiCodificato  s               n   inz(*off)
006500100419     d $NotaF          s               n   inz(*off)
006600100414     d $Stampa         s               n   inz(*off)
006700100415     d $Vof            s               n   inz(*off)
006800100413
006900100413      // - Parametro ricevuto
007000100413     d filiale         s              3
007100100413
007200100413      // - Campi di comodo
007300100415     d old_WIScmm      s                   like(WIScmm)
007400100415     d old_WIScpo      s                   like(WIScpo)
007500100415     d old_WISksc      s                   like(WISksc)
007600100415     d sav_CPNpno      s                   like(CPNpno)
007700100415     d wCad            s                   like(ATCcad)
007800100415     d wDad            s                   like(ATCdad)
007900100416     d wData           s              8  0
008000100416     d wDataMax        s              8  0
008100100415     d wDataMin        s              8  0
008200100416     d wDataMinDel     s              8  0
008300100415     d wDataNew        s              8  0
008400100415     d wDataOff        s              8  0
008500100415     d wDataOggi       s              8  0
008600100414     d wData_ISO       s               d   datfmt(*iso)
008700100416     d wHda            s                   like(ATChda)
008800100415     d wOfi            s                   like(ATCofi)
008900100415     d wOii            s                   like(ATCoii)
009000100415     d wRiga           s              2  0
009100100415     d wTat            s                   like(ATCtat)
009200100413
009300100413      //---------------------------------------------------------------
009400100413      //?Definizione procedure esterne.
009500100413      //---------------------------------------------------------------
009600100415
009700100415      // - Aggiunge/Toglie gg/mm dalla data
009800100415     d Xgiolav         pr                  extpgm('XGIOLAV')
009900100415     d  xgiolavds                          likeds(xgiolavds)
010000100413
010100100413      //---------------------------------------------------------------
010200100413      //?prototipi
010300100413      //---------------------------------------------------------------
010400100414
010500100415      /copy gaitrasrc/srcprotopr,trul33r
010600100413
010700100413      //---------------------------------------------------------------
010800100413      //?Definizione key-list.
010900100413      //---------------------------------------------------------------
011000100415
011100100415       // - File TABEL00F
011200100415     d k03tabel      e ds                  extname(TABEL00F:*key)
011300100415     d                                     prefix(k_)
011400100413
011500100413      //---------------------------------------------------------------
011600100413      //?Riepilogo indicatori.
011700100413      //---------------------------------------------------------------
011800100414
011900100414      // 99    : Salto pagina
012000100413
012100100413      //---------------------------------------------------------------
012200100413      //?M A I N - L I N E
012300100413      //---------------------------------------------------------------
012400100413
012500100413     c     *Entry        plist
012600100413     c                   parm                    filiale
012700100413
012800100413      /free
012900100413
013000100413       //?Operazioni iniziali
013100100413       exsr sr_RoutInz;
013200100413
013300100413       //?Scrivo i nuovi file per le trattative
013400100413       IF  not $Fine;
013500100413         exsr sr_Elabora;
013600100413       ENDIF;
013700100413
013800100413       //?Operazioni finali
013900100413       exsr sr_RoutEnd;
014000100413
014100100413       //--------------------------------------------------------------
014200100413       //?Operazioni iniziali.
014300100413       //--------------------------------------------------------------
014400100413       BEGSR sr_RoutInz;
014500100414
014600100414         wData_ISO = %date();
014700100415         wDataOggi = %dec(wData_ISO);
014800100419       //?Calcolo data limite vecchiaia visite (non serve più)
014900100415         wDataMin = %dec(wData_ISO - %months(10));
015000100415       //?Calcolo data minima per tenere le visite
015100100419         wDataMinDel = 20100101;
015200100419       //?Calcolo data limite massimo visite (Sql da fare prima)
015300100419         wDataMax = 20100831;
015400100419       //?Calcolo nuova data trattativa nel caso che non si modifichi a mano
015500100415         clear XGIOLAVDS;
015600100415         IXGLdata = %dec(%date());
015700100415         IXGLadd  = 'S';
015800100415         IXGLgg   = 10;
015900100415         IXGLpa   = 'P';
016000100415         IXGLlav  = 'S';
016100100422         IXGLfil = 001;
016200100415         XGIOLAV (xgiolavds);
016300100415         IF  OXGLerr = *blanks;
016400100415           wDataNew = OXGLdata;
016500100415         ELSE;
016600100415           wDataNew = %dec(wData_ISO);
016700100415         ENDIF;
016800100413
016900100413         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
017000100416
017100100416       //?Impostazione campi "fissi"
017200100416         k_TBLkut = 1;
017300100413
017400100413       ENDSR;
017500100413
017600100413       //--------------------------------------------------------------
017700100422       //?Elaboro.
017800100413       //--------------------------------------------------------------
017900100413       BEGSR sr_Elabora;
018000100413
018100100413         $End = *off;
018200100413
018300100413         exec sql
018400100413         DECLARE VIS cursor for
018500100413         SELECT  WFVIS00F.*
018600100413         FROM WFVIS00F
018700100414         ORDER by WIScmm, WISCPO, WISksc, WISdvi desc,
018800100414                  WIShmv desc, WISnrv desc;
018900100413
019000100413         exec sql OPEN VIS;
019100100413
019200100413         DOW  not $End;
019300100413           exec sql FETCH next from VIS into :WFVISds;
019400100413           IF  sqlcod = 100 or sqlcod < 0;
019500100413             $End = *on;
019600100413             leave;
019700100413           ENDIF;
019800100413
019900100413           //?per ogni commerciale/potenziale/cliente devo avere solo una trattativa NON fittizia
020000100416           IF  WIScmm <> old_WIScmm or WIScpo <> old_WIScpo or
020100100413               WISksc <> old_WISksc;
020200100413             //?scrivo i file nuovi
020300100413             exsr Scrivi;
020400100415             //?la trattativa che ho scritto è buona salvo la chiave di rottura
020500100414             IF  VISffz = *blanks;
020600100413               old_WIScmm = WIScmm;
020700100413               old_WIScpo = WIScpo;
020800100413               old_WISksc = WISksc;
020900100413             ENDIF;
021000100413             iter;
021100100413           ENDIF;
021200100413           //?scrivo i file nuovi con trattativa fittizia
021300100413           WISffz = 'F';
021400100413           exsr Scrivi;
021500100413         ENDDO;
021600100413
021700100413         exec sql CLOSE VIS;
021800100413
021900100413       ENDSR;
022000100413
022100100413       //--------------------------------------------------------------
022200100413       //?Scrivo i nuovi file.
022300100413       //--------------------------------------------------------------
022400100413       BEGSR Scrivi;
022500100415
022600100421       //?se trattativa più vecchia del 2010 non trascodifico
022700100421       //?riporto solo le note INTERNE nel caso di trattativa N=Nuova
022800100421         IF  WISdvi < wDataMinDel;
022900100421           IF  WIStpv = 'N';
023000100421             exsr Scrivi_NoteIN;
023100100421           ENDIF;
023200100415           leavesr;
023300100415         ENDIF;
023400100419
023500100419       //?se trattativa M=Mantenimento o C=Incasso senza offerte con data <= data trascodifica
023600100421       //?non trascodifico
023700100419         IF  (WIStpv = 'M' or WIStpv = 'C') and WISoff = *blanks and
023800100419              WISdvi <= wDataOggi;
023900100419           leavesr;
024000100419         ENDIF;
024100100413
024200100413       //?scrivo la trattativa
024300100413         exsr Scrivi_Vis;
024400100413
024500100413       //?scrivo la testata delle offerte
024600100413         exsr Scrivi_Vof;
024700100414
024800100414       //?scrivo le note
024900100414         exsr Scrivi_Note;
025000100415
025100100415       //?scrivo le attività e gli stati
025200100419       //?se trattativa NON fittizia
025300100419         IF  VISffz <> 'S';
025400100419           exsr Scrivi_NewFile;
025500100419         ENDIF;
025600100413
025700100413       ENDSR;
025800100419
025900100419       //--------------------------------------------------------------
026000100419       //?Scrivo note INTERNE.
026100100419       //--------------------------------------------------------------
026200100419       BEGSR Scrivi_NoteIN;
026300100419
026400100419         clear sav_CPNpno;
026500100419
026600100419         w_NTCapl = 'V';
026700100419         w_NTCnk1 = '0151' + %editc(WISnrv:'X');
026800100419         clear w_NTCnk2;
026900100419         w_NTCtnt = 'IN';
027000100419         setll  (w_NTCapl : w_NTCnk1 : w_NTCnk2 : w_NTCtnt) TFNTC01L;
027100100419         reade  (w_NTCapl : w_NTCnk1 : w_NTCnk2 : w_NTCtnt) TFNTC01L;
027200100419         DOW  not %eof(TFNTC01L);
027300100419         //?se NTCntr = 0 non trascodifico sono cose vecchie e poche)
027400100419           IF  w_NTCntr > 0;
027500100419             VIScpo = WIScpo;
027600100419             clear VISksc;
027700100419             clear VISnrv;
027800100419             exsr Scrivi_TICPN;
027900100419           ENDIF;
028000100419
028100100419           reade  (w_NTCapl : w_NTCnk1 : w_NTCnk2 : w_NTCtnt) TFNTC01L;
028200100419         ENDDO;
028300100419
028400100419       ENDSR;
028500100413
028600100413       //--------------------------------------------------------------
028700100413       //?Scrivo file trattative.
028800100413       //--------------------------------------------------------------
028900100413       BEGSR Scrivi_Vis;
029000100413
029100100413         clear TIVIS000;
029200100413         VIScmm  = WIScmm;
029300100413         VIScmmi = WIScmm;
029400100415       //?Se data trattativa > data massima prevista cambio la data e stampo
029500100419         IF  WISdvi > wDataMax;
029600100419           VISdat = wDataNew;
029700100419           wRiga = 03;
029800100419           exsr sr_Stampa;
029900100419         ELSE;
030000100419           VISdat  = WISdvi;
030100100419         ENDIF;
030200100419
030300100419       //?Tipo trattativa
030400100419         VIStpv = WIStpv;
030500100419       //?Se tipo trattativa 'I' ex incremento divente 'A' aumento/incremento
030600100413         IF  WIStpv = 'I';
030700100413           VIStpv = 'A';
030800100413         ENDIF;
030900100415       //?Se tipo trattativa 'C' incasso o 'M' mantenimento con offerte devono
031000100413       //?diventare 'A'
031100100419         IF  (WIStpv = 'C' or WIStpv = 'M') and WISoff = 'S';
031200100419           VIStpv = 'A';
031300100413         ENDIF;
031400100413
031500100415       //?Se trattativa di PROVA diventa FITTIZIA
031600100413         IF  WISffz = 'P';
031700100413           VISffz = 'S';
031800100413         ENDIF;
031900100415       //?Se trattativa FITTIZIA imposto 'S' per FITTIZIA
032000100413         IF  WISffz = 'F';
032100100413           VISffz = 'S';
032200100413         ENDIF;
032300100413
032400100413         VIScpo = WIScpo;
032500100413         VISksc = WISksc;
032600100413         VISnrv = WISnrv;
032700100413         VISfil = %dec(%subst(%editc(VIScmm:'X'):1:3):3:0);
032800100413
032900100419         $NotaF = *off;
033000100413         chain VISnrv TNVIS05L;
033100100413         IF  %found(TNVIS05L);
033200100413           VIScnw = n_VIScnw;
033300100413           VISrag = n_VISrag;
033400100415       //?Se ho modificato la trattativa da buona a fittizia scrivo nota
033500100419       //?ma lo faccio dopo che ho scritto le altre note INTERNE
033600100414           IF  n_VISffz = *blanks and VISffz = 'S';
033700100419             $NotaF = *on;
033800100413           ENDIF;
033900100413         ENDIF;
034000100413
034100100413         write TIVIS000;
034200100413
034300100413       ENDSR;
034400100413
034500100413       //--------------------------------------------------------------
034600100413       //?Scrivo file testata offerte.
034700100413       //--------------------------------------------------------------
034800100413       BEGSR Scrivi_Vof;
034900100415
035000100415         $Vof = *off;
035100100415         clear wDataOff;
035200100413
035300100413         setll VISnrv TNOFM01L;
035400100413         reade VISnrv TNOFM01L;
035500100413         DOW not %eof(TNOFM01L);
035600100413
035700100415           $Vof = *on;
035800100413           dsta01 = TAMflo;
035900100413
036000100413           clear TIVOF000;
036100100413           VOFnrv = TAMksc;
036200100413           VOFctr = TAMctr;
036300100413           VOFprg = TAMprg;
036400100413         //?Tipo tariffa italia/estero
036500100413           VOFtpt = TAMfie;
036600100413         //?Tipo tariffa DPD/FedEx
036700100413           IF  §tadpd = 'S';
036800100413             VOFtpt = 'D';
036900100413           ENDIF;
037000100413           IF  §tafed = 'S';
037100100413             VOFtpt = 'F';
037200100413           ENDIF;
037300100413         //?data presentazione offerta la recupero dal TNVIS in base al tipo tariffa
037400100413           IF  VOFtpt = 'I';
037500100413             VOFdpo = n_VISdpo;
037600100413           ELSE;
037700100413             VOFdpo = n_VISdoe;
037800100413           ENDIF;
037900100413           IF  VOFdpo = 0;
038000100413             VOFdpo = TAMduv;
038100100415             wRiga = 02;
038200100414             exsr sr_Stampa;
038300100413           ENDIF;
038400100413
038500100413           write TIVOF000;
038600100415
038700100415           //?tengo la data presentazione offerta più alta
038800100426           IF  wDataOff < VOFdpo;
038900100426             wDataOff = VOFdpo;
039000100426           ENDIF;
039100100413
039200100413           reade VISnrv TNOFM01L;
039300100413         ENDDO;
039400100413
039500100413       ENDSR;
039600100414
039700100414       //--------------------------------------------------------------
039800100414       //?Scrivo file note.
039900100414       //--------------------------------------------------------------
040000100414       BEGSR Scrivi_Note;
040100100414
040200100415         clear sav_CPNpno;
040300100415
040400100415         w_NTCapl = 'V';
040500100415         w_NTCnk1 = '0151' + %editc(VISnrv:'X');
040600100415         setll  (w_NTCapl : w_NTCnk1) TFNTC01L;
040700100415         reade  (w_NTCapl : w_NTCnk1) TFNTC01L;
040800100414         DOW  not %eof(TFNTC01L);
040900100414         //?Se trattativa legata a cliente
041000100415         //?le note di tipo 10 le scrivo con 'T'
041100100414         //?le note IN le scrivo sul nuovo TICPN00F
041200100414           IF  VISksc > 0;
041300100414           //?Note tariffa '10'
041400100415             IF  w_NTCtnt = '10';
041500100415               exsr Scrivi_TFNTC;
041600100415             ENDIF;
041700100414           //?Note interne 'IN' (se NTCntr = 0 non trascodifico sono cose vecchie e poche)
041800100416             IF  w_NTCtnt = 'IN' and w_NTCntr > 0;
041900100415               exsr Scrivi_TICPN;
042000100414             ENDIF;
042100100414           ENDIF;
042200100415         //?Se trattativa NON legata a cliente
042300100415         //?le note diverse da IN e da 30 le scrivo con 'T'
042400100415         //?le 30 non le trascodifico
042500100415         //?le note IN le scrivo sul nuovo TICPN00F
042600100415           IF  VISksc = 0;
042700100415           //?Note tariffa NON 'IN' e NON '30'
042800100415             IF  w_NTCtnt <> 'IN' and w_NTCtnt <> '30';
042900100415               exsr Scrivi_TFNTC;
043000100415             ENDIF;
043100100415           //?Note interne 'IN' (se NTCntr = 0 non trascodifico sono cose vecchie e poche)
043200100416             IF  w_NTCtnt = 'IN' and w_NTCntr > 0;
043300100415               exsr Scrivi_TICPN;
043400100415             ENDIF;
043500100415           ENDIF;
043600100414
043700100415           reade  (w_NTCapl : w_NTCnk1) TFNTC01L;
043800100414         ENDDO;
043900100419
044000100419       //?Scrivo la nota relativa al cambio tipo trattativa
044100100419         IF  $NotaF;
044200100419           exsr Scrivi_Nota_F;
044300100419         ENDIF;
044400100414
044500100414       ENDSR;
044600100415
044700100415       //--------------------------------------------------------------
044800100415       //?Scrivo le attività e gli stati
044900100415       //--------------------------------------------------------------
045000100415       BEGSR Scrivi_NewFile;
045100100415
045200100415       //?se potenziale mai codificato
045300100415         $MaiCodificato = *off;
045400100415         chain VIScpo CNACO16L;
045500100415         IF  not %found(CNACO16L);
045600100415           $MaiCodificato = *on;
045700100419         //?Scrivo Stato '2' sul potenziale se mai codificato
045800100419           exsr Scrivi_TICPS;
045900100415         ENDIF;
046000100419
046100100415       //?Trattativa con offerte
046200100415         IF  $Vof;
046300100415         //?Schedulo nuova attività 30-Contatto Post Offerta
046400100415           wTat = 'T';
046500100415           wCad = '30';
046600100415         //?Data attività schedulata a data presentazione offerta + 15 gg. lavorativi
046700100415           clear XGIOLAVDS;
046800100426         //?se data presentazione offerta minore data visita tengo data visita
046900100426           IF  wDataOff < WISdvi;
047000100426             IXGLdata = WISdvi;
047100100426           ELSE;
047200100426             IXGLdata = wDataOff;
047300100426           ENDIF;
047400100415           IXGLadd  = 'S';
047500100415           IXGLgg   = 15;
047600100415           IXGLpa   = 'P';
047700100415           IXGLlav  = 'S';
047800100415           IXGLfil = %dec(%subst(%editc(VIScmm:'X'):1:3):3:0);
047900100415           XGIOLAV (xgiolavds);
048000100415           wDad = OXGLdata;
048100100415           IF  WIShmv > 0;
048200100415             wHda = WIShmv * 100;
048300100415           ELSE;
048400100419             wHda = 103000;
048500100415           ENDIF;
048600100415           clear wOii;
048700100415           clear wOfi;
048800100415         //?Stacco nuovo numero attività
048900100415           exsr Stacca_Numero;
049000100423         //?Scrivo la nuova attività
049100100415           exsr Scrivi_TIATC;
049200100415         ENDIF;
049300100415
049400100415       //?Trattativa senza offerte
049500100415         IF  not $Vof;
049600100415         //?Data trattativa maggiore data trascodifica
049700100415           IF  VISdat > wDataOggi;
049800100415           //?Schedulo nuova attività 91-Appuntamento
049900100415             wTat = 'A';
050000100415             wCad = '91';
050100100415             wDad = VISdat;
050200100415             IF  WIShmv > 0;
050300100415               wHda = WIShmv * 100;
050400100415             ELSE;
050500100419               wHda = 103000;
050600100415             ENDIF;
050700100416             wOii = wHda / 100;
050800100416             wOfi = wHda / 100;
050900100415          //?Stacco nuovo numero attività
051000100415             exsr Stacca_Numero;
051100100423          //?Scrivo la nuova attività
051200100415             exsr Scrivi_TIATC;
051300100415           ENDIF;
051400100415         //?Data trattativa minore uguale data trascodifica
051500100415           IF  VISdat <= wDataOggi;
051600100415           //?Se trattativa di tipo 'N' nuova o 'A' aumento/incremento
051700100415           //?Schedulo nuova attività 80-Offerta da inviare
051800100415             IF  VIStpv = 'N' or VIStpv = 'A';
051900100415               wTat = 'O';
052000100415               wCad = '80';
052100100419               wDad = VISdat;
052200100415               IF  WIShmv > 0;
052300100415                 wHda = WIShmv * 100;
052400100415               ELSE;
052500100419                 wHda = 103000;
052600100415               ENDIF;
052700100415               clear wOii;
052800100415               clear wOfi;
052900100415            //?Stacco nuovo numero attività
053000100415               exsr Stacca_Numero;
053100100423            //?Scrivo la nuova attività
053200100415               exsr Scrivi_TIATC;
053300100415             ENDIF;
053400100415           ENDIF;
053500100415         ENDIF;
053600100415
053700100415       ENDSR;
053800100415
053900100415       //--------------------------------------------------------------
054000100415       //?Scrivo file TFNTC.
054100100415       //--------------------------------------------------------------
054200100415       BEGSR Scrivi_TFNTC;
054300100415
054400100415         clear TFNTC;
054500100415         NTCapl = 'T';
054600100415         NTCnk1 = w_NTCnk1;
054700100415         NTCnk2 = w_NTCnk2;
054800100415         NTCtnt = w_NTCtnt;
054900100415         NTCrnt = w_NTCrnt;
055000100415         NTCsns = w_NTCsns;
055100100416         NTCflt = 'W';
055200100415         NTCntr = w_NTCntr;
055300100415         write TFNTC;
055400100415
055500100415       ENDSR;
055600100415
055700100415       //--------------------------------------------------------------
055800100415       //?Scrivo file TICPN.
055900100415       //--------------------------------------------------------------
056000100415       BEGSR Scrivi_TICPN;
056100100415
056200100415         clear TICPN000;
056300100415         CPNcpo = VIScpo;
056400100415         CPNksc = VISksc;
056500100415         CPNnrv = VISnrv;
056600100416       //?Costruisco la data lunga 8 da data lunga 6
056700100416         IF  w_NTCntr  < 000101;
056800100416           wData = w_NTCntr + 19000000;
056900100416         ELSE;
057000100416           wData = w_NTCntr + 20000000;
057100100416         ENDIF;
057200100416         CPNdim = wData;
057300100428         CPNhim = 235700;
057400100415         CPNsns = 'S';
057500100416         CPNnot = w_NTCrnt;
057600100419         CPNpno = sav_CPNpno + 1;
057700100503         CPNpos = 046;
057800100503         CPNpru = 'BATCH';
057900100415         write TICPN000;
058000100415         sav_CPNpno = CPNpno;
058100100415
058200100415       ENDSR;
058300100419
058400100419       //--------------------------------------------------------------
058500100419       //?Scrivo nota per modifica da trattativa buona a trattativa Fittizia
058600100419       //--------------------------------------------------------------
058700100419       BEGSR Scrivi_Nota_F;
058800100419
058900100419         clear TICPN000;
059000100419         CPNcpo = VIScpo;
059100100419         CPNksc = VISksc;
059200100419         CPNnrv = VISnrv;
059300100419         CPNdim = wDataOggi;
059400100429         CPNhim = 235700;
059500100419         CPNsns = 'S';
059600100419         CPNnot = 'Trattativa modificata in FITTIZIA';
059700100419         CPNpno = sav_CPNpno + 1;
059800100503         CPNpos = 046;
059900100503         CPNpru = 'BATCH';
060000100419         write TICPN000;
060100100419
060200100419       ENDSR;
060300100415
060400100415       //--------------------------------------------------------------
060500100415       //?Scrivo file TICPS.
060600100415       //--------------------------------------------------------------
060700100415       BEGSR Scrivi_TICPS;
060800100415
060900100415         clear TICPS000;
061000100415         CPScpo = VIScpo;
061100100415         CPScmm = VIScmm;
061200100415         clear ds01;
061300100419         k_TBLcod = '01';
061400100415         k_TBLkey = %editc(CPScmm:'X');
061500100415         chain  %kds(K03tabel) TABEL00F;
061600100415         IF  %found(TABEL00F);
061700100415           ds01 = TBLuni;
061800100415         ENDIF;
061900100415         CPScmd = §01age;
062000100415         CPScmmi = CPScmm;
062100100415         CPScmdi = CPScmd;
062200100415         CPSfst = '2 ';
062300100415         CPSdst = VISdat;
062400100415         CPSksc = VISksc;
062500100419         CPSnrv = VISnrv;
062600100415         CPSdim = wDataOggi;
062700100419         CPShim = 1030;
062800100415         CPSpru = 'BATCH';
062900100415         CPSfle = %dec(%subst(%editc(CPScmm:'X'):1:3):3:0);
063000100415         write TICPS000;
063100100415
063200100415       ENDSR;
063300100415
063400100415       //--------------------------------------------------------------
063500100415       //?Stacco nuovo numero attività.
063600100415       //--------------------------------------------------------------
063700100415       BEGSR Stacca_Numero;
063800100415
063900100415         clear TRUL33ds;
064000100415         I33tla = 'L';
064100100415         I33ope = 0;
064200100415         I33cnu = 070;
064300100415         I33num = 1;
064400100415         I33aaa = *year;
064500100415         kpjbu  = TRUL33ds;
064600100415         trul33r (kpjba);
064700100415         TRUL33ds = kpjbu;
064800100415
064900100415       ENDSR;
065000100415
065100100415       //--------------------------------------------------------------
065200100415       //?Scrivo file TIATC.
065300100415       //--------------------------------------------------------------
065400100415       BEGSR Scrivi_TIATC;
065500100415
065600100415         clear TIATC000;
065700100415         ATCtat  = wTat;
065800100415         ATCatn  = O33Nrf;
065900100415         ATCatnp = 1;
066000100415         ATCcpo  = VIScpo;
066100100415         ATCksc  = VISksc;
066200100415         ATCnrv  = VISnrv;
066300100415         ATCcad  = wCad;
066400100415         ATCdad  = wDad;
066500100415         ATChda  = wHda;
066600100415         ATCcmm  = VIScmm;
066700100415         ATCoii  = wOii;
066800100415         ATCofi  = wOfi;
066900100415         ATCco3  = VIScmm;
067000100415         IF  $MaiCodificato;
067100100415           ATCcnw = 'P';
067200100415         ELSE;
067300100415           ATCcnw = 'C';
067400100415         ENDIF;
067500100415         ATCdim  = %dec(%date());
067600100415         ATChim  = %dec(%time());
067700100415         ATCpri  = 'BATCH';
067800100415         write TIATC000;
067900100415
068000100415       ENDSR;
068100100414
068200100414       //--------------------------------------------------------------
068300100414       //?Stampa errori.
068400100414       //--------------------------------------------------------------
068500100414       BEGSR sr_Stampa;
068600100414
068700100414         IF  not $Stampa;
068800100414           write CRMT01;
068900100414           $Stampa = *on;
069000100426           DESpgm = 'Scrive Trattative e stati legati alle trattative';
069100100426           write CRMT02;
069200100414         ENDIF;
069300100414
069400100415         IF  wRiga = 02;
069500100415           write CRMD02;
069600100415         ENDIF;
069700100415
069800100415         IF  wRiga = 03;
069900100415           write CRMD03;
070000100415         ENDIF;
070100100414
070200100414       ENDSR;
070300100413
070400100413       //--------------------------------------------------------------
070500100413       //?Operazioni finali.
070600100413       //--------------------------------------------------------------
070700100413       BEGSR sr_RoutEnd;
070800100413
070900100413         *inLR = *on;
071000100413         return;
071100100413
071200100413       ENDSR;
071300100413
071400100413      /end-free
