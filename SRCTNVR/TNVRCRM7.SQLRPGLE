000100100413      //---------------------------------------------------------------
000200100726      //?TNVRCRM7 - Genero attività su potenziali mai codificati
000300100413      //---------------------------------------------------------------
000400100413
000500100413     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000600100413
000700100413      //---------------------------------------------------------------
000800100413      //?Dichiarazione file.
000900100413      //---------------------------------------------------------------
001000100726     fTIATC04L  if   e           k disk    rename(TIATC000:TIATC04)
001100100415     fTIATC00F  o    e             disk
001200100726     fTICPS11L  if   e           k disk
001300100726     fTIVIS05L  if   e           k disk
001400100414     fTNVRCRMP  o    e             printer oflind(*in99)
001500100413
001600100413      //---------------------------------------------------------------
001700100413      //?Definizione costanti.
001800100413      //---------------------------------------------------------------
001900100413
002000100413      //---------------------------------------------------------------
002100100413      //?Definizione schiere.
002200100413      //---------------------------------------------------------------
002300100513
002400100726     d aggiorna        s              2a
002500090715
002600100413      //---------------------------------------------------------------
002700100413      //?Definizione aree dati.
002800100413      //---------------------------------------------------------------
002900100413
003000100413      //---------------------------------------------------------------
003100100413      //?Definizione strutture dati.
003200100413      //---------------------------------------------------------------
003300100415
003400100415     d KPJBA         e ds
003500100726
003600100726      // File potenziali
003700100726     d TNCPOds       e ds                  extname(TNCPO00F)
003800100415
003900100415      // - Ricerca ultimo numero attività
004000100415     d TRUL33ds      e ds                  inz
004100100422
004200100422      // - DS per MMGG  ggi
004300100422     d wOggiMMGG       ds
004400100422     d wOggiMM                        2  0
004500100422     d wOggiGG                        2  0
004600100422
004700100422      // - DS per MMGG data stato
004800100422     d wDstMMGG        ds
004900100422     d wDstMM                         2  0
005000100422     d wDstGG                         2  0
005100100415
005200100413      //---------------------------------------------------------------
005300100413      //?Definizione variabili globali.
005400100413      //---------------------------------------------------------------
005500100413
005600100413      // - Flags booleani
005700100413     d $End            s               n   inz(*off)
005800100422     d $NoVis          s               n   inz(*off)
005900100414     d $Stampa         s               n   inz(*off)
006000100413
006100100413      // - Campi di comodo
006200100726     d NrAtt           s              5i 0
006300100726     d NrVis           s              5i 0
006400100514     d TotAtc          s              7s 0
006500100811     d TotAtc24        s              7s 0
006600100811     d TotAtc60        s              7s 0
006700100415     d wCad            s                   like(ATCcad)
006800100415     d wDad            s                   like(ATCdad)
006900100415     d wDataOggi       s              8  0
007000100414     d wData_ISO       s               d   datfmt(*iso)
007100100427     d wgg             s              2  0 inz(20)
007200100415     d wRiga           s              2  0
007300100415     d wTat            s                   like(ATCtat)
007400100413
007500100413      //---------------------------------------------------------------
007600100413      //?Definizione procedure esterne.
007700100413      //---------------------------------------------------------------
007800100413
007900100413      //---------------------------------------------------------------
008000100413      //?prototipi
008100100413      //---------------------------------------------------------------
008200100414
008300100415      /copy gaitrasrc/srcprotopr,trul33r
008400100413
008500100413      //---------------------------------------------------------------
008600100413      //?Definizione key-list.
008700100413      //---------------------------------------------------------------
008800100415
008900100413
009000100413      //---------------------------------------------------------------
009100100413      //?Riepilogo indicatori.
009200100413      //---------------------------------------------------------------
009300100414
009400100414      // 99    : Salto pagina
009500100413
009600100413      //---------------------------------------------------------------
009700100413      //?M A I N - L I N E
009800100413      //---------------------------------------------------------------
009900100413
010000100413     c     *Entry        plist
010100100726     c                   parm                    aggiorna
010200100413
010300100413      /free
010400100413
010500100413       //?Operazioni iniziali
010600100513       exsr RoutInz;
010700100413
010800100726       //?Elaboro
010900100726       exsr Elabora;
011000100413
011100100413       //?Operazioni finali
011200100513       exsr RoutEnd;
011300100413
011400100413       //--------------------------------------------------------------
011500100413       //?Operazioni iniziali.
011600100413       //--------------------------------------------------------------
011700100513       BEGSR RoutInz;
011800100726
011900100726         IF  aggiorna <> 'SI' and aggiorna <> 'NO';
012000100726           aggiorna = 'NO';
012100100726         ENDIF;
012200100414
012300100414         wData_ISO = %date();
012400100415         wDataOggi = %dec(wData_ISO);
012500100422         wOggiMM   = %subdt(wData_ISO:*months);
012600100422         wOggiGG   = %subdt(wData_ISO:*days);
012700100726
012800100726         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
012900100514
013000100514         clear TotAtc;
013100100811         clear TotAtc24;
013200100811         clear TotAtc60;
013300100413
013400100413       ENDSR;
013500100413
013600100413       //--------------------------------------------------------------
013700100422       //?Elaboro.
013800100413       //--------------------------------------------------------------
013900100513       BEGSR Elabora;
014000100413
014100100413         $End = *off;
014200100413
014300100726       //?Estraggo solo i potenziali mai codificati
014400100413         exec sql
014500100726         DECLARE CPO cursor for
014600100726         SELECT  cpocpo, cpocmm
014700100726         FROM TNCPO00F exception join CNACO00F
014800100726         ON cpocpo = acolib
014900100726         ORDER by CPOcpo;
015000100413
015100100726         exec sql OPEN CPO;
015200100413
015300100413         DOW  not $End;
015400100726           exec sql FETCH next from CPO into :CPOCPO, :CPOCMM;
015500100413           IF  sqlcod = 100 or sqlcod < 0;
015600100413             $End = *on;
015700100413             leave;
015800100413           ENDIF;
015900100422
016000100726         //?Controllo se potenziale ha azioni o trattative aperte
016100100726           exsr CtrAtt;
016200100726
016300100726         //?Se NON trovo attività o trattative
016400100726           IF  NrAtt = 0 and NrVis = 0;
016500100726
016600100726           //?Genero nuova attività
016700100726             exsr Scrivi;
016800100726
016900100726           ENDIF;
017000100413         ENDDO;
017100100413
017200100726         exec sql CLOSE CPO;
017300100514
017400100514       //?Stampa totali
017500100514         wRiga = 00;
017600100514         exsr Stampa;
017700100413
017800100413       ENDSR;
017900100726
018000100726       //--------------------------------------------------------------
018100100726       //?Controllo attività/trattative.
018200100726       //--------------------------------------------------------------
018300100726       BEGSR CtrAtt;
018400100726
018500100726         clear NrAtt;
018600100726         clear NrVis;
018700100726
018800100726       //?Controllo se il potenziale ha attività aperte
018900100726       //?oppure è stato chiuso con attività 14 o 15 annullo potenziale
019000100726         setll cpocpo TIATC04L;
019100100726         reade cpocpo TIATC04L;
019200100726         DOW  not %Eof;
019300100726           IF  atcdco = 0;
019400100726             NrAtt = 1;
019500100726             leave;
019600100726           ENDIF;
019700100726           IF  atccac = '14' or atccac = '15';
019800100726             NrAtt = 1;
019900100726             leave;
020000100726           ENDIF;
020100100726           reade cpocpo TIATC04L;
020200100726         ENDDO;
020300100726
020400100726       //?Se ho trovato attività esco
020500100726         IF  NrAtt > 0;
020600100726           leavesr;
020700100726         ENDIF;
020800100726
020900100726       //?Se non ho trovato attività controllo se il potenziale ha trattative aperte
021000100726           exec sql
021100100726           select count(*) into :NrVis from tivis00f
021200100726           where viscpo = :CPOcpo and visdch = 0 and visffz = ' ';
021300100726
021400100726       ENDSR;
021500100413
021600100413       //--------------------------------------------------------------
021700100726       //?Scrivo nuova attività.
021800100413       //--------------------------------------------------------------
021900100413       BEGSR Scrivi;
022000100422
022100100422         $NoVis = *off;
022200100422         clear wCad;
022300100422         clear wDad;
022400100422         clear wTat;
022500100726
022600100726       //?Aggancio l'ultimo stato del potenziale
022700100726         chain cpocpo TICPS11L;
022800100726       //?Se non ha stati non creo attività
022900100726         IF  not %Found(TICPS11L);
023000100726           leavesr;
023100100726         ENDIF;
023200100422
023300100422       //?Controllo se la trattativa che c'è sullo stato esiste già sul nuovo file
023400100422       //?oppure è chiusa o fittizia
023500100726         IF  CPSnrv > 0;
023600100726           chain CPSnrv TIVIS05L;
023700100726           IF  not %Found(TIVIS05L) or VISdch > 0 or VISffz = 'S';
023800100726             $NoVis = *on;
023900100726           ENDIF;
024000100726         ENDIF;
024100100415
024200100726       //?Stacco nuovo numero attività
024300100726         IF  aggiorna = 'SI';
024400100513           exsr Stacca_Numero;
024500100513         //?Scrivo la nuova attività
024600100726           exsr Scrivi_TIATC;
024700100726         ENDIF;
024800100811
024900100811       //?Se non devo scrivere le nuove attività conto quante per causale
025000100811         SELECT;
025100100811           WHEN  CPSfst = '2 ' or CPSfst = 'A0' or
025200100811                 CPSfst = 'A1' or CPSfst = 'A4' or
025300100811                 CPSfst = 'A7' or CPSfst = 'A8';
025400100811               TotAtc60 += 1;
025500100811           WHEN  $NoVis and (CPSfst = 'A2' or CPSfst = 'A3');
025600100811               TotAtc60 += 1;
025700100811           WHEN  CPSfst = 'A5' or CPSfst = 'A6' or CPSfst = 'A9';
025800100811               TotAtc24 += 1;
025900100811         ENDSL;
026000100726
026100100726       //?conto quante attività creo
026200100726         TotAtc += 1;
026300100413
026400100413       ENDSR;
026500100415
026600100415       //--------------------------------------------------------------
026700100415       //?Stacco nuovo numero attività.
026800100415       //--------------------------------------------------------------
026900100415       BEGSR Stacca_Numero;
027000100415
027100100415         clear TRUL33ds;
027200100415         I33tla = 'L';
027300100415         I33ope = 0;
027400100415         I33cnu = 070;
027500100415         I33num = 1;
027600100415         I33aaa = *year;
027700100415         kpjbu  = TRUL33ds;
027800100415         trul33r (kpjba);
027900100415         TRUL33ds = kpjbu;
028000100415
028100100415       ENDSR;
028200100415
028300100415       //--------------------------------------------------------------
028400100415       //?Scrivo file TIATC.
028500100415       //--------------------------------------------------------------
028600100415       BEGSR Scrivi_TIATC;
028700100422
028800100422       //?Imposto il tipo e la causale attività in base allo stato
028900100427         SELECT;
029000100726           WHEN  CPSfst = '2 ';
029100100726             wTat = 'T';
029200100726             wCad = '60';
029300100427           WHEN  CPSfst = 'A0' or CPSfst = 'A1';
029400100427             wTat = 'T';
029500100427             wCad = '60';
029600100427           WHEN  $NoVis and (CPSfst = 'A2' or CPSfst = 'A3');
029700100427             wTat = 'T';
029800100427             wCad = '60';
029900100427           WHEN  CPSfst = 'A4';
030000100427             wTat = 'T';
030100100427             wCad = '60';
030200100513           //?Stampo errore
030300100513             wRiga = 07;
030400100513             exsr Stampa;
030500100427           WHEN  CPSfst = 'A7';
030600100427             wTat = 'T';
030700100427             wCad = '60';
030800100513           //?Stampo errore
030900100513             wRiga = 07;
031000100513             exsr Stampa;
031100100427           WHEN  CPSfst = 'A8';
031200100427             wTat = 'T';
031300100427             wCad = '60';
031400100427           WHEN  CPSfst = 'A5' or CPSfst = 'A6' or CPSfst = 'A9';
031500100427             wTat = 'T';
031600100427             wCad = '24';
031700100427          ENDSL;
031800100422
031900100422       //?Imposto la data
032000100427         SELECT;
032100100811           WHEN  CPSdst > wDataOggi;
032200100427             wDad = CPSdst;
032300100811           WHEN  CPSdst >= %dec(%date(wDataOggi) - %months(2));
032400100427             wDad = %dec(%date(CPSdst) + %months(3));
032500100427           OTHER;
032600100427             wData_ISO = %date(CPSdst);
032700100427             wDstMM    = %subdt(wData_ISO:*months);
032800100427             wDstGG    = %subdt(wData_ISO:*days);
032900100811
033000100811             IF  wDstMMGG <= wOggiMMGG;
033100100811
033200100811               wDad      = 20100000 + (%subdt(wData_ISO:*months) *100) +
033300100811                                     (%subdt(wData_ISO:*days));
033400100811               wDad      = %dec(%date(wDad) + %months(wOggiMM + 1));
033500100811
033600100811               //wData_ISO = wData_ISO + %months(7);
033700100811               //wDstMM    = %subdt(wData_ISO:*months);
033800100811               //wDstGG    = %subdt(wData_ISO:*days);
033900100811
034000100811             ELSE;
034100100811
034200100811               wDad = 20100000 + (%subdt(wData_ISO:*months) *100) +
034300100811                                 (%subdt(wData_ISO:*days));
034400100427             ENDIF;
034500100811
034600100427         ENDSL;
034700100415
034800100415         clear TIATC000;
034900100415         ATCtat  = wTat;
035000100415         ATCatn  = O33Nrf;
035100100415         ATCatnp = 1;
035200100422         ATCcpo  = CPScpo;
035300100415         ATCcad  = wCad;
035400100415         ATCdad  = wDad;
035500100422         ATChda  = 103000;
035600100726         IF CPScmm > 0;
035700100726           ATCcmm  = CPScmm;
035800100726           ATCco3  = CPScmm;
035900100726         ELSE;
036000100726           ATCcmm  = CPOcmm;
036100100726           ATCco3  = CPOcmm;
036200100726         ENDIF;
036300100422         ATCcnw = 'P';
036400100415         ATCdim  = %dec(%date());
036500100415         ATChim  = %dec(%time());
036600100415         ATCpri  = 'BATCH';
036700100415         write TIATC000;
036800100415
036900100415       ENDSR;
037000100414
037100100414       //--------------------------------------------------------------
037200100414       //?Stampa errori.
037300100414       //--------------------------------------------------------------
037400100513       BEGSR Stampa;
037500100414
037600100519         IF  not $Stampa or *in99;
037700100414           write CRMT01;
037800100414           $Stampa = *on;
037900100726           DESpgm = 'Nuove attività su potenziali mai codificati';
038000100426           write CRMT02;
038100100519           *in99 = *off;
038200100414         ENDIF;
038300100811
038400100811         IF  wRiga = 07;
038500100811           write CRMD07;
038600100811         ENDIF;
038700100514
038800100514         IF  wRiga = 00;
038900100811           write CRMATC1;
039000100514         ENDIF;
039100100414
039200100414       ENDSR;
039300100413
039400100413       //--------------------------------------------------------------
039500100413       //?Operazioni finali.
039600100413       //--------------------------------------------------------------
039700100513       BEGSR RoutEnd;
039800100413
039900100413         *inLR = *on;
040000100413         return;
040100100413
040200100413       ENDSR;
040300100413
040400100413      /end-free
