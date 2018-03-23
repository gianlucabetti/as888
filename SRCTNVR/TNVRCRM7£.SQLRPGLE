000100100413      //---------------------------------------------------------------
000200100421      //?TNVRCRM4 - Da nuovo TICPS genero attività
000300100413      //---------------------------------------------------------------
000400100413
000500100413     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000600100413
000700100413      //---------------------------------------------------------------
000800100413      //?Dichiarazione file.
000900100413      //---------------------------------------------------------------
001000100415     fCNACO16L  if   e           k disk
001100100415     fTABEL00F  if   e           k disk
001200100415     fTIATC00F  o    e             disk
001300100422     fTICPS01L  if   e           k disk    prefix(s_)
001400100422     fTIVIS05L  if   e           k disk
001500100421     fTNCPO01L  if   e           k disk
001600100414     fTNVRCRMP  o    e             printer oflind(*in99)
001700100413
001800100413      //---------------------------------------------------------------
001900100413      //?Definizione costanti.
002000100413      //---------------------------------------------------------------
002100100413
002200100413      //---------------------------------------------------------------
002300100413      //?Definizione schiere.
002400100413      //---------------------------------------------------------------
002500100513
002600100513     d filiali         s              3a   dim(10)
002700100514     d $Pog            s              3  0 dim(2500)
002800090715
002900100413      //---------------------------------------------------------------
003000100413      //?Definizione aree dati.
003100100413      //---------------------------------------------------------------
003200100413
003300100413      //---------------------------------------------------------------
003400100413      //?Definizione strutture dati.
003500100413      //---------------------------------------------------------------
003600100415
003700100415     d KPJBA         e ds
003800100514
003900100514      // Organigramma
004000100514     d AZORG00F      e ds                  extname(AZORG00F)
004100100421
004200100421      // File stati potenziali
004300100421     d TICPSds       e ds                  extname(TICPS00F)
004400100415
004500100415      // - Reperimento filiali
004600100415     d TRUL31DS      e ds
004700100415     d pog                    10    759  0 dim(250)
004800100415
004900100415      // - Ricerca ultimo numero attività
005000100415     d TRUL33ds      e ds                  inz
005100100415
005200100415      // - Tabella 01 = Codici Commerciali
005300100415     d ds01          e ds                  inz
005400100422
005500100422      // - DS per MMGG  ggi
005600100422     d wOggiMMGG       ds
005700100422     d wOggiMM                        2  0
005800100422     d wOggiGG                        2  0
005900100422
006000100422      // - DS per MMGG data stato
006100100422     d wDstMMGG        ds
006200100422     d wDstMM                         2  0
006300100422     d wDstGG                         2  0
006400100415
006500100413      //---------------------------------------------------------------
006600100413      //?Definizione variabili globali.
006700100413      //---------------------------------------------------------------
006800100413
006900100413      // - Flags booleani
007000100427     d $Del            s               n   inz(*off)
007100100413     d $End            s               n   inz(*off)
007200100413     d $Fine           s               n   inz(*off)
007300100422     d $NoVis          s               n   inz(*off)
007400100414     d $Stampa         s               n   inz(*off)
007500100513
007600100513      // - Indici di schiera
007700100513     d xx              s              4  0 inz
007800100513     d yy              s              4  0 inz
007900100513     d zz              s              4  0 inz
008000100413
008100100413      // - Campi di comodo
008200100422     d old_CPScpo      s                   like(CPScpo)
008300100514     d TotAtc          s              7s 0
008400100603     d TotAtcCod       s              7s 0
008500100519     d TotNoAtc        s              7s 0
008600100603     d TotNoAtcC       s              7s 0
008700100415     d wCad            s                   like(ATCcad)
008800100422     d wCmm            s                   like(ATCcmm)
008900100415     d wDad            s                   like(ATCdad)
009000100416     d wData           s              8  0
009100100415     d wDataOggi       s              8  0
009200100427     d wDataOggi2      s              8  0
009300100414     d wData_ISO       s               d   datfmt(*iso)
009400100513     d wFlo            s              8
009500100427     d wgg             s              2  0 inz(20)
009600100415     d wRiga           s              2  0
009700100415     d wTat            s                   like(ATCtat)
009800100413
009900100413      //---------------------------------------------------------------
010000100413      //?Definizione procedure esterne.
010100100413      //---------------------------------------------------------------
010200100413
010300100413      //---------------------------------------------------------------
010400100413      //?prototipi
010500100413      //---------------------------------------------------------------
010600100414
010700100422      /copy gaitrasrc/srcprotopr,trul31r
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
014800100513
014900100513         yy = 1;
015000100513
015100100513       //?Ciclo per filiali passate
015200100513         FOR  yy BY 1 to 10;
015300100514
015400100514           IF  filiali(yy) <> *zeros and filiali(yy) <> *blanks;
015500100422
015600100513         //?Recupero filiali del 'RA' della filiale ricevuta
015700100513             clear TRUL31DS;
015800100513             I31abi = 'RA';
015900100513             I31cpo = %dec(filiali(yy):3:0);
016000100513             callp trul31r (kpjba : trul31ds);
016100100513             IF  O31pog <= *zeros;
016200100513               $Fine = *on;
016300100514               leavesr;
016400100513             ENDIF;
016500100513
016600100513         //?Memorizzo le filiali appena caricate in un'unica schiera
016700100514             zz = %lookup(*zeros:$Pog);
016800100514             %subarr($Pog:zz:250) = pog;
016900100513
017000100513           ENDIF;
017100100513
017200100513         ENDFOR;
017300100514
017400100514         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
017500100514
017600100514       //?Carico nella schiera anche le filiali già attivate in precedenza
017700100514         $End = *off;
017800100514
017900100514         exec sql
018000100514         DECLARE FIL cursor for
018100100514         SELECT  ORGfil
018200100514         FROM AZORG00F
018300100514         WHERE substr(ORGde1, 11, 8) <> '  '
018400100514         ORDER by ORGfil;
018500100514
018600100514         exec sql OPEN FIL;
018700100514
018800100514         DOW  not $End;
018900100514           exec sql FETCH next from FIL into :ORGfil;
019000100514           IF  sqlcod = 100 or sqlcod < 0;
019100100514             $End = *on;
019200100514             leave;
019300100514           ENDIF;
019400100514
019500100514           clear xx;
019600100514           xx = %lookup(*zeros : $Pog);
019700100514           IF  xx > 0;
019800100514             $Pog(xx) = ORGfil;
019900100514           ENDIF;
020000100514
020100100514         ENDDO;
020200100514
020300100514         exec sql CLOSE FIL;
020400100414
020500100414         wData_ISO = %date();
020600100415         wDataOggi = %dec(wData_ISO);
020700100422         wOggiMM   = %subdt(wData_ISO:*months);
020800100422         wOggiGG   = %subdt(wData_ISO:*days);
020900100427         wDataOggi2= %dec(wData_ISO + %days(wgg));
021000100513
021100100513         wFlo = %editc(wDataOggi:'X');
021200100416
021300100416       //?Impostazione campi "fissi"
021400100416         k_TBLkut = 1;
021500100514
021600100514         clear TotAtc;
021700100603         clear TotAtcCod;
021800100519         clear TotNoAtc;
021900100603         clear TotNoAtcC;
022000100413
022100100413       ENDSR;
022200100413
022300100413       //--------------------------------------------------------------
022400100422       //?Elaboro.
022500100413       //--------------------------------------------------------------
022600100513       BEGSR Elabora;
022700100413
022800100413         $End = *off;
022900100422         clear old_CPScpo;
023000100413
023100100413         exec sql
023200100421         DECLARE CPS cursor for
023300100422         SELECT  *
023400100422         FROM TICPS00F
023500100513         WHERE substr(CPSflo, 1, 8) = :wFlo
023600100422         ORDER by CPScpo, CPSdst desc;
023700100413
023800100421         exec sql OPEN CPS;
023900100413
024000100413         DOW  not $End;
024100100421           exec sql FETCH next from CPS into :TICPSds;
024200100413           IF  sqlcod = 100 or sqlcod < 0;
024300100413             $End = *on;
024400100413             leave;
024500100413           ENDIF;
024600100422
024700100422         //?Faccio tutto a rottura di potenziale
024800100422           IF  CPScpo <> old_CPScpo;
024900100426
025000100426             old_CPScpo = CPScpo;
025100100422
025200100422           //?Se potenziale non esiste stampo errore e non faccio niente
025300100422             chain CPScpo TNCPO01L;
025400100422             IF  not %found(TNCPO01L);
025500100422               STPcpo = CPScpo;
025600100422               wRiga = 04;
025700100513               exsr Stampa;
025800100422              iter;
025900100422             ENDIF;
026000100413
026100100422           //?Se potenziale mai codificato
026200100422           //?devo pianificare nuova attività in base all'ultimo stato
026300100422             chain CPScpo CNACO16L;
026400100422             IF  not %found(CNACO16L);
026500100422             //?L'attività la scrivo solo se non esiste già uno stato '2' sul nuovo file stati
026600100422             //?se non c'è già un nuovo stato '2' sul potenziale
026700100422               chain (CPScpo : '2 ') TICPS01L;
026800100422               IF  not %Found(TICPS01L);
026900100422               //?scrivo nuovo file attività
027000100422                 exsr Scrivi;
027100100422               ENDIF;
027200100422             ENDIF;
027300100521
027400100521           //?Se potenziale codificato e data ultimo stato > oggi
027500100521           //?devo pianificare nuova attività '60'
027600100521             IF  %found(CNACO16L) and CPSdst > wDataOggi;
027700100603             //?Non controllo se c'è stato 2 perchè inutile visto che lo stato lo scrivo solo
027800100603             //?per i mai codificati
027900100603             //?scrivo nuovo file attività
028000100603               exsr Scrivi60;
028100100521             ENDIF;
028200100422
028300100422           ENDIF;
028400100422
028500100413         ENDDO;
028600100413
028700100421         exec sql CLOSE CPS;
028800100514
028900100514       //?Stampa totali
029000100514         wRiga = 00;
029100100514         exsr Stampa;
029200100413
029300100413       ENDSR;
029400100413
029500100413       //--------------------------------------------------------------
029600100413       //?Scrivo i nuovi file.
029700100413       //--------------------------------------------------------------
029800100413       BEGSR Scrivi;
029900100422
030000100422         $NoVis = *off;
030100100422         clear wCad;
030200100422         clear wCmm;
030300100422         clear wDad;
030400100422         clear wTat;
030500100422
030600100422       //?Controllo se la trattativa che c'è sullo stato esiste già sul nuovo file
030700100422       //?oppure è chiusa o fittizia
030800100422         chain CPSnrv TIVIS05L;
030900100422         IF  not %Found(TIVIS05L) or VISdch > 0 or VISffz = 'S';
031000100422           $NoVis = *on;
031100100422         ENDIF;
031200100415
031300100513       //?controllo se il commerciale dello stato fa parte dell'area che sto trascodificando
031400100513         IF  %lookup(%dec(%subst(%editc(CPScmm:'X'):1:3):3:0):$pog) > 0;
031500100513         //?fa parte dell'area creo attività
031600100513           wCmm = CPScmm;
031700100513         //?Stacco nuovo numero attività
031800100513           exsr Stacca_Numero;
031900100513         //?Scrivo la nuova attività
032000100513           exsr Scrivi_TIATC;
032100100514         //?conto quante attività creo
032200100514           TotAtc += 1;
032300100513         ELSE;
032400100513         //?non fa parte dell'area
032500100513         //?controllo il commerciale del potenziale
032600100513           IF  %lookup(%dec(%subst(%editc(CPOcmm:'X'):1:3):3:0):$pog) > 0;
032700100513           //?fa parte dell'area creo attività
032800100513             wCmm = CPOcmm;
032900100513           //?Stacco nuovo numero attività
033000100513             exsr Stacca_Numero;
033100100513           //?Scrivo la nuova attività
033200100513             exsr Scrivi_TIATC;
033300100514           //?conto quante attività creo
033400100514             TotAtc += 1;
033500100513           ELSE;
033600100513           //?non fa parte dell'area stampo e non creo attività
033700100513             wRiga = 06;
033800100513             exsr Stampa;
033900100519           //?conto quante attività NON creo
034000100519             TotNoAtc += 1;
034100100513           ENDIF;
034200100513         ENDIF;
034300100413
034400100413       ENDSR;
034500100521
034600100521       //--------------------------------------------------------------
034700100521       //?Scrivo i nuovi file.
034800100521       //--------------------------------------------------------------
034900100521       BEGSR Scrivi60;
035000100521
035100100521       //?controllo se il commerciale dello stato fa parte dell'area che sto trascodificando
035200100521         IF  %lookup(%dec(%subst(%editc(CPScmm:'X'):1:3):3:0):$pog) > 0;
035300100521         //?fa parte dell'area creo attività
035400100521           wCmm = CPScmm;
035500100521         //?Stacco nuovo numero attività
035600100521           exsr Stacca_Numero;
035700100521         //?Scrivo la nuova attività
035800100521           exsr Scrivi_TIATC60;
035900100521         //?conto quante attività creo
036000100603           TotAtcCod += 1;
036100100521         ELSE;
036200100521         //?non fa parte dell'area
036300100521         //?controllo il commerciale del potenziale
036400100521           IF  %lookup(%dec(%subst(%editc(CPOcmm:'X'):1:3):3:0):$pog) > 0;
036500100521           //?fa parte dell'area creo attività
036600100521             wCmm = CPOcmm;
036700100521           //?Stacco nuovo numero attività
036800100521             exsr Stacca_Numero;
036900100521           //?Scrivo la nuova attività
037000100521             exsr Scrivi_TIATC60;
037100100521           //?conto quante attività creo
037200100603             TotAtcCod += 1;
037300100521           ELSE;
037400100521           //?non fa parte dell'area stampo e non creo attività
037500100521             wRiga = 06;
037600100521             exsr Stampa;
037700100521           //?conto quante attività NON creo
037800100603             TotNoAtcC += 1;
037900100521           ENDIF;
038000100521         ENDIF;
038100100521
038200100521       ENDSR;
038300100415
038400100415       //--------------------------------------------------------------
038500100415       //?Stacco nuovo numero attività.
038600100415       //--------------------------------------------------------------
038700100415       BEGSR Stacca_Numero;
038800100415
038900100415         clear TRUL33ds;
039000100415         I33tla = 'L';
039100100415         I33ope = 0;
039200100415         I33cnu = 070;
039300100415         I33num = 1;
039400100415         I33aaa = *year;
039500100415         kpjbu  = TRUL33ds;
039600100415         trul33r (kpjba);
039700100415         TRUL33ds = kpjbu;
039800100415
039900100415       ENDSR;
040000100415
040100100415       //--------------------------------------------------------------
040200100415       //?Scrivo file TIATC.
040300100415       //--------------------------------------------------------------
040400100415       BEGSR Scrivi_TIATC;
040500100422
040600100422       //?Imposto il tipo e la causale attività in base allo stato
040700100427         SELECT;
040800100427           WHEN  CPSfst = 'A0' or CPSfst = 'A1';
040900100427             wTat = 'T';
041000100427             wCad = '60';
041100100427           WHEN  $NoVis and (CPSfst = 'A2' or CPSfst = 'A3');
041200100427             wTat = 'T';
041300100427             wCad = '60';
041400100427           WHEN  CPSfst = 'A4';
041500100427             wTat = 'T';
041600100427             wCad = '60';
041700100513           //?Stampo errore
041800100513             wRiga = 07;
041900100513             exsr Stampa;
042000100427           WHEN  CPSfst = 'A7';
042100100427             wTat = 'T';
042200100427             wCad = '60';
042300100513           //?Stampo errore
042400100513             wRiga = 07;
042500100513             exsr Stampa;
042600100427           WHEN  CPSfst = 'A8';
042700100427             wTat = 'T';
042800100427             wCad = '60';
042900100427           WHEN  CPSfst = 'A5' or CPSfst = 'A6' or CPSfst = 'A9';
043000100427             wTat = 'T';
043100100427             wCad = '24';
043200100427          ENDSL;
043300100422
043400100422       //?Imposto la data
043500100427         SELECT;
043600100427           WHEN  CPSdst >= wDataOggi;
043700100427             wDad = CPSdst;
043800100427           WHEN  CPSdst > %dec(%date(wDataOggi) - %months(2));
043900100427             wDad = %dec(%date(CPSdst) + %months(3));
044000100427           OTHER;
044100100427             wData_ISO = %date(CPSdst);
044200100427             wDstMM    = %subdt(wData_ISO:*months);
044300100427             wDstGG    = %subdt(wData_ISO:*days);
044400100427             IF  wDstMMGG < wOggiMMGG;
044500100514               wData_ISO = wData_ISO + %months(7);
044600100514               wDstMM    = %subdt(wData_ISO:*months);
044700100514               wDstGG    = %subdt(wData_ISO:*days);
044800100427             ENDIF;
044900100514             wDad = 20100000 + (wDstMM *100) + wDSTGG;
045000100427         ENDSL;
045100100415
045200100415         clear TIATC000;
045300100415         ATCtat  = wTat;
045400100415         ATCatn  = O33Nrf;
045500100415         ATCatnp = 1;
045600100422         ATCcpo  = CPScpo;
045700100415         ATCcad  = wCad;
045800100415         ATCdad  = wDad;
045900100422         ATChda  = 103000;
046000100422         ATCcmm  = wCmm;
046100100422         ATCco3  = wCmm;
046200100422         ATCcnw = 'P';
046300100415         ATCdim  = %dec(%date());
046400100415         ATChim  = %dec(%time());
046500100415         ATCpri  = 'BATCH';
046600100415         write TIATC000;
046700100415
046800100415       ENDSR;
046900100521
047000100521       //--------------------------------------------------------------
047100100521       //?Scrivo file TIATC attività 60.
047200100521       //--------------------------------------------------------------
047300100521       BEGSR Scrivi_TIATC60;
047400100521
047500100521         wTat = 'T';
047600100521         wCad = '60';
047700100521
047800100521       //?Imposto la data
047900100521         wDad = CPSdst;
048000100521
048100100521         clear TIATC000;
048200100521         ATCtat  = wTat;
048300100521         ATCatn  = O33Nrf;
048400100521         ATCatnp = 1;
048500100521         ATCcpo  = CPScpo;
048600100521         ATCcad  = wCad;
048700100521         ATCdad  = wDad;
048800100521         ATChda  = 103000;
048900100521         ATCcmm  = wCmm;
049000100521         ATCco3  = wCmm;
049100100521         ATCcnw = 'C';
049200100521         ATCdim  = %dec(%date());
049300100521         ATChim  = %dec(%time());
049400100521         ATCpri  = 'BATCH';
049500100521         write TIATC000;
049600100521
049700100521       ENDSR;
049800100414
049900100414       //--------------------------------------------------------------
050000100414       //?Stampa errori.
050100100414       //--------------------------------------------------------------
050200100513       BEGSR Stampa;
050300100414
050400100519         IF  not $Stampa or *in99;
050500100414           write CRMT01;
050600100414           $Stampa = *on;
050700100426           DESpgm = 'Scrive attività da nuovi stati potenziali';
050800100517           fil01 = %dec(Filiali(01):3:0);
050900100517           fil02 = %dec(Filiali(02):3:0);
051000100517           fil03 = %dec(Filiali(03):3:0);
051100100517           fil04 = %dec(Filiali(04):3:0);
051200100517           fil05 = %dec(Filiali(05):3:0);
051300100517           fil06 = %dec(Filiali(06):3:0);
051400100517           fil07 = %dec(Filiali(07):3:0);
051500100517           fil08 = %dec(Filiali(08):3:0);
051600100517           fil09 = %dec(Filiali(09):3:0);
051700100517           fil10 = %dec(Filiali(10):3:0);
051800100426           write CRMT02;
051900100519           *in99 = *off;
052000100414         ENDIF;
052100100414
052200100422         IF  wRiga = 04;
052300100426           STPdes = 'Stato senza file TNCPO non creo attività.';
052400100422           write CRMD04;
052500100415         ENDIF;
052600100422
052700100422         IF  wRiga = 05;
052800100422           write CRMD05;
052900100422         ENDIF;
053000100422
053100100422         IF  wRiga = 06;
053200100422           write CRMD06;
053300100422         ENDIF;
053400100422
053500100422         IF  wRiga = 07;
053600100422           write CRMD07;
053700100422         ENDIF;
053800100514
053900100514         IF  wRiga = 00;
054000100514           write CRMATC;
054100100514         ENDIF;
054200100414
054300100414       ENDSR;
054400100413
054500100413       //--------------------------------------------------------------
054600100413       //?Operazioni finali.
054700100413       //--------------------------------------------------------------
054800100513       BEGSR RoutEnd;
054900100413
055000100413         *inLR = *on;
055100100413         return;
055200100413
055300100413       ENDSR;
055400100413
055500100413      /end-free
