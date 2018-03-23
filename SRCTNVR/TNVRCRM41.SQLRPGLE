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
001300100421     fTNCPO01L  if   e           k disk
001400100609     fTNVRCRMP1 o    e             printer oflind(*in99)
001500100413
001600100413      //---------------------------------------------------------------
001700100413      //?Definizione costanti.
001800100413      //---------------------------------------------------------------
001900100413
002000100413      //---------------------------------------------------------------
002100100413      //?Definizione schiere.
002200100413      //---------------------------------------------------------------
002300100513
002400100513     d filiali         s              3a   dim(10)
002500100514     d $Pog            s              3  0 dim(2500)
002600090715
002700100413      //---------------------------------------------------------------
002800100413      //?Definizione aree dati.
002900100413      //---------------------------------------------------------------
003000100413
003100100413      //---------------------------------------------------------------
003200100413      //?Definizione strutture dati.
003300100413      //---------------------------------------------------------------
003400100415
003500100415     d KPJBA         e ds
003600100514
003700100514      // Organigramma
003800100514     d AZORG00F      e ds                  extname(AZORG00F)
003900100608
004000100608      // File attività
004100100608     d TIATCds       e ds                  extname(TIATC00F)
004200100608     d                                     prefix(a_)
004300100421
004400100421      // File stati potenziali
004500100421     d TICPSds       e ds                  extname(TICPS00F)
004600100415
004700100415      // - Reperimento filiali
004800100415     d TRUL31DS      e ds
004900100415     d pog                    10    759  0 dim(250)
005000100415
005100100415      // - Ricerca ultimo numero attività
005200100415     d TRUL33ds      e ds                  inz
005300100415
005400100415      // - Tabella 01 = Codici Commerciali
005500100415     d ds01          e ds                  inz
005600100415
005700100413      //---------------------------------------------------------------
005800100413      //?Definizione variabili globali.
005900100413      //---------------------------------------------------------------
006000100413
006100100413      // - Flags booleani
006200100427     d $Del            s               n   inz(*off)
006300100413     d $End            s               n   inz(*off)
006400100413     d $Fine           s               n   inz(*off)
006500100422     d $NoVis          s               n   inz(*off)
006600100414     d $Stampa         s               n   inz(*off)
006700100513
006800100513      // - Indici di schiera
006900100513     d xx              s              4  0 inz
007000100513     d yy              s              4  0 inz
007100100513     d zz              s              4  0 inz
007200100413
007300100413      // - Campi di comodo
007400100608     d NrAtt           s              5i 0
007500100422     d old_CPScpo      s                   like(CPScpo)
007600100603     d TotAtcCod       s              7s 0
007700100603     d TotNoAtcC       s              7s 0
007800100609     d TotPot          s              7s 0
007900100415     d wCad            s                   like(ATCcad)
008000100422     d wCmm            s                   like(ATCcmm)
008100100415     d wDad            s                   like(ATCdad)
008200100415     d wDataOggi       s              8  0
008300100414     d wData_ISO       s               d   datfmt(*iso)
008400100610     d wFlo            s              1
008500100427     d wgg             s              2  0 inz(20)
008600100415     d wRiga           s              2  0
008700100415     d wTat            s                   like(ATCtat)
008800100413
008900100413      //---------------------------------------------------------------
009000100413      //?Definizione procedure esterne.
009100100413      //---------------------------------------------------------------
009200100413
009300100413      //---------------------------------------------------------------
009400100413      //?prototipi
009500100413      //---------------------------------------------------------------
009600100414
009700100422      /copy gaitrasrc/srcprotopr,trul31r
009800100415      /copy gaitrasrc/srcprotopr,trul33r
009900100413
010000100413      //---------------------------------------------------------------
010100100413      //?Definizione key-list.
010200100413      //---------------------------------------------------------------
010300100415
010400100415       // - File TABEL00F
010500100415     d k03tabel      e ds                  extname(TABEL00F:*key)
010600100415     d                                     prefix(k_)
010700100413
010800100413      //---------------------------------------------------------------
010900100413      //?Riepilogo indicatori.
011000100413      //---------------------------------------------------------------
011100100414
011200100414      // 99    : Salto pagina
011300100413
011400100413      //---------------------------------------------------------------
011500100413      //?M A I N - L I N E
011600100413      //---------------------------------------------------------------
011700100413
011800100413     c     *Entry        plist
011900100513     c                   parm                    filiali
012000100413
012100100413      /free
012200100413
012300100413       //?Operazioni iniziali
012400100513       exsr RoutInz;
012500100413
012600100413       //?Scrivo i nuovi file per le trattative
012700100413       IF  not $Fine;
012800100513         exsr Elabora;
012900100413       ENDIF;
013000100413
013100100413       //?Operazioni finali
013200100513       exsr RoutEnd;
013300100413
013400100413       //--------------------------------------------------------------
013500100413       //?Operazioni iniziali.
013600100413       //--------------------------------------------------------------
013700100513       BEGSR RoutInz;
013800100513
013900100513         yy = 1;
014000100513
014100100513       //?Ciclo per filiali passate
014200100513         FOR  yy BY 1 to 10;
014300100514
014400100514           IF  filiali(yy) <> *zeros and filiali(yy) <> *blanks;
014500100422
014600100513         //?Recupero filiali del 'RA' della filiale ricevuta
014700100513             clear TRUL31DS;
014800100513             I31abi = 'RA';
014900100513             I31cpo = %dec(filiali(yy):3:0);
015000100513             callp trul31r (kpjba : trul31ds);
015100100513             IF  O31pog <= *zeros;
015200100513               $Fine = *on;
015300100514               leavesr;
015400100513             ENDIF;
015500100513
015600100513         //?Memorizzo le filiali appena caricate in un'unica schiera
015700100514             zz = %lookup(*zeros:$Pog);
015800100514             %subarr($Pog:zz:250) = pog;
015900100513
016000100513           ENDIF;
016100100513
016200100513         ENDFOR;
016300100514
016400100514         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
016500100514
016600100514       //?Carico nella schiera anche le filiali già attivate in precedenza
016700100514         $End = *off;
016800100514
016900100514         exec sql
017000100514         DECLARE FIL cursor for
017100100514         SELECT  ORGfil
017200100514         FROM AZORG00F
017300100514         WHERE substr(ORGde1, 11, 8) <> '  '
017400100514         ORDER by ORGfil;
017500100514
017600100514         exec sql OPEN FIL;
017700100514
017800100514         DOW  not $End;
017900100514           exec sql FETCH next from FIL into :ORGfil;
018000100514           IF  sqlcod = 100 or sqlcod < 0;
018100100514             $End = *on;
018200100514             leave;
018300100514           ENDIF;
018400100514
018500100514           clear xx;
018600100514           xx = %lookup(*zeros : $Pog);
018700100514           IF  xx > 0;
018800100514             $Pog(xx) = ORGfil;
018900100514           ENDIF;
019000100514
019100100514         ENDDO;
019200100514
019300100514         exec sql CLOSE FIL;
019400100414
019500100414         wData_ISO = %date();
019600100415         wDataOggi = %dec(wData_ISO);
019700100610         wDataOggi = 20100501;
019800100513
019900100609         wFlo = 'B';
020000100416
020100100416       //?Impostazione campi "fissi"
020200100416         k_TBLkut = 1;
020300100514
020400100603         clear TotAtcCod;
020500100603         clear TotNoAtcC;
020600100609         clear TotPot;
020700100413
020800100413       ENDSR;
020900100413
021000100413       //--------------------------------------------------------------
021100100422       //?Elaboro.
021200100413       //--------------------------------------------------------------
021300100513       BEGSR Elabora;
021400100413
021500100413         $End = *off;
021600100422         clear old_CPScpo;
021700100413
021800100413         exec sql
021900100421         DECLARE CPS cursor for
022000100422         SELECT  *
022100100422         FROM TICPS00F
022200100609         WHERE substr(CPSflo, 1, 1) = :wFlo
022300100422         ORDER by CPScpo, CPSdst desc;
022400100413
022500100421         exec sql OPEN CPS;
022600100413
022700100413         DOW  not $End;
022800100421           exec sql FETCH next from CPS into :TICPSds;
022900100413           IF  sqlcod = 100 or sqlcod < 0;
023000100413             $End = *on;
023100100413             leave;
023200100413           ENDIF;
023300100422
023400100422         //?Faccio tutto a rottura di potenziale
023500100422           IF  CPScpo <> old_CPScpo;
023600100426
023700100426             old_CPScpo = CPScpo;
023800100422
023900100422           //?Se potenziale non esiste stampo errore e non faccio niente
024000100422             chain CPScpo TNCPO01L;
024100100422             IF  not %found(TNCPO01L);
024200100422               STPcpo = CPScpo;
024300100422               wRiga = 04;
024400100513               exsr Stampa;
024500100422              iter;
024600100422             ENDIF;
024700100521
024800100521           //?Se potenziale codificato e data ultimo stato > oggi
024900100521           //?devo pianificare nuova attività '60'
025000100608             chain CPScpo CNACO16L;
025100100521             IF  %found(CNACO16L) and CPSdst > wDataOggi;
025200100610             //?Non creo attività se già ne esistono...chiuse o aperte
025300100608               exec sql
025400100608                select count(*) into :NrAtt from tiatc00f
025500100610                where atccpo = :CPScpo;
025600100608                //?trovo almeno un'attività non scrivo nuova attività
025700100608                IF  NrAtt > *zeros;
025800100609                  TotPot += 1;
025900100608                  iter;
026000100608                ENDIF;
026100100608
026200100610               exsr Scrivi60;
026300100610             ENDIF;
026400100422
026500100422           ENDIF;
026600100422
026700100413         ENDDO;
026800100413
026900100421         exec sql CLOSE CPS;
027000100514
027100100514       //?Stampa totali
027200100514         wRiga = 00;
027300100514         exsr Stampa;
027400100413
027500100413       ENDSR;
027600100521
027700100521       //--------------------------------------------------------------
027800100521       //?Scrivo i nuovi file.
027900100521       //--------------------------------------------------------------
028000100521       BEGSR Scrivi60;
028100100521
028200100521       //?controllo se il commerciale dello stato fa parte dell'area che sto trascodificando
028300100521         IF  %lookup(%dec(%subst(%editc(CPScmm:'X'):1:3):3:0):$pog) > 0;
028400100521         //?fa parte dell'area creo attività
028500100521           wCmm = CPScmm;
028600100521         //?Stacco nuovo numero attività
028700100521           exsr Stacca_Numero;
028800100521         //?Scrivo la nuova attività
028900100521           exsr Scrivi_TIATC60;
029000100521         //?conto quante attività creo
029100100603           TotAtcCod += 1;
029200100521         ELSE;
029300100521         //?non fa parte dell'area
029400100521         //?controllo il commerciale del potenziale
029500100521           IF  %lookup(%dec(%subst(%editc(CPOcmm:'X'):1:3):3:0):$pog) > 0;
029600100521           //?fa parte dell'area creo attività
029700100521             wCmm = CPOcmm;
029800100521           //?Stacco nuovo numero attività
029900100521             exsr Stacca_Numero;
030000100521           //?Scrivo la nuova attività
030100100521             exsr Scrivi_TIATC60;
030200100521           //?conto quante attività creo
030300100603             TotAtcCod += 1;
030400100521           ELSE;
030500100521           //?non fa parte dell'area stampo e non creo attività
030600100521             wRiga = 06;
030700100521             exsr Stampa;
030800100521           //?conto quante attività NON creo
030900100603             TotNoAtcC += 1;
031000100521           ENDIF;
031100100521         ENDIF;
031200100521
031300100521       ENDSR;
031400100415
031500100415       //--------------------------------------------------------------
031600100415       //?Stacco nuovo numero attività.
031700100415       //--------------------------------------------------------------
031800100415       BEGSR Stacca_Numero;
031900100415
032000100415         clear TRUL33ds;
032100100415         I33tla = 'L';
032200100415         I33ope = 0;
032300100415         I33cnu = 070;
032400100415         I33num = 1;
032500100415         I33aaa = *year;
032600100415         kpjbu  = TRUL33ds;
032700100415         trul33r (kpjba);
032800100415         TRUL33ds = kpjbu;
032900100415
033000100415       ENDSR;
033100100415
033200100521       //--------------------------------------------------------------
033300100521       //?Scrivo file TIATC attività 60.
033400100521       //--------------------------------------------------------------
033500100521       BEGSR Scrivi_TIATC60;
033600100521
033700100521         wTat = 'T';
033800100521         wCad = '60';
033900100521
034000100521       //?Imposto la data
034100100521         wDad = CPSdst;
034200100521
034300100521         clear TIATC000;
034400100521         ATCtat  = wTat;
034500100521         ATCatn  = O33Nrf;
034600100521         ATCatnp = 1;
034700100521         ATCcpo  = CPScpo;
034800100521         ATCcad  = wCad;
034900100521         ATCdad  = wDad;
035000100521         ATChda  = 103000;
035100100521         ATCcmm  = wCmm;
035200100521         ATCco3  = wCmm;
035300100521         ATCcnw = 'C';
035400100521         ATCdim  = %dec(%date());
035500100521         ATChim  = %dec(%time());
035600100521         ATCpri  = 'BATCH';
035700100521         write TIATC000;
035800100521
035900100521       ENDSR;
036000100414
036100100414       //--------------------------------------------------------------
036200100414       //?Stampa errori.
036300100414       //--------------------------------------------------------------
036400100513       BEGSR Stampa;
036500100414
036600100519         IF  not $Stampa or *in99;
036700100414           write CRMT01;
036800100414           $Stampa = *on;
036900100426           DESpgm = 'Scrive attività da nuovi stati potenziali';
037000100517           fil01 = %dec(Filiali(01):3:0);
037100100517           fil02 = %dec(Filiali(02):3:0);
037200100517           fil03 = %dec(Filiali(03):3:0);
037300100517           fil04 = %dec(Filiali(04):3:0);
037400100517           fil05 = %dec(Filiali(05):3:0);
037500100517           fil06 = %dec(Filiali(06):3:0);
037600100517           fil07 = %dec(Filiali(07):3:0);
037700100517           fil08 = %dec(Filiali(08):3:0);
037800100517           fil09 = %dec(Filiali(09):3:0);
037900100517           fil10 = %dec(Filiali(10):3:0);
038000100426           write CRMT02;
038100100519           *in99 = *off;
038200100414         ENDIF;
038300100414
038400100422         IF  wRiga = 04;
038500100426           STPdes = 'Stato senza file TNCPO non creo attività.';
038600100422           write CRMD04;
038700100415         ENDIF;
038800100422
038900100422         IF  wRiga = 06;
039000100422           write CRMD06;
039100100422         ENDIF;
039200100514
039300100514         IF  wRiga = 00;
039400100514           write CRMATC;
039500100514         ENDIF;
039600100414
039700100414       ENDSR;
039800100413
039900100413       //--------------------------------------------------------------
040000100413       //?Operazioni finali.
040100100413       //--------------------------------------------------------------
040200100513       BEGSR RoutEnd;
040300100413
040400100413         *inLR = *on;
040500100413         return;
040600100413
040700100413       ENDSR;
040800100413
040900100413      /end-free
