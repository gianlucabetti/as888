000100161006      //---------------------------------------------------------------
000200161006      //?      Rielabora Fatturato Giornaliero su bolle contabilizzate
000300161006      //---------------------------------------------------------------
000400161006     h Decedit('0,') Datedit(*dmy.) Option(*nodebugio)
000500161006
000600161006      //---------------------------------------------------------------
000700161006      //?Dichiarazione file.
000800161006      //---------------------------------------------------------------
000900161006     fAZORG01L  if   e           k disk
001000161006     fTABEL00F  if   e           k disk
001100161006     fTITAS38C  if   e           k disk
001200161006     fTITA730C  if   e           k disk
001300161006     fTFFGC01L  uf a e           k disk
001400161006
001500161006      //---------------------------------------------------------------
001600161006      //?Definizione costanti.
001700161006      //---------------------------------------------------------------
001800161006
001900161006      //---------------------------------------------------------------
002000161006      //?Definizione schiere.
002100161006      //---------------------------------------------------------------
002200161006     d TBO             s              2    dim(30)
002300161006
002400161006      //---------------------------------------------------------------
002500161006      //?Definizione aree dati.
002600161006      //---------------------------------------------------------------
002700161006
002800161006      //---------------------------------------------------------------
002900161006      //?Definizione strutture dati.
003000161006      //---------------------------------------------------------------
003100161006     d Kpjba         e ds
003200161006
003300161006     d dsTB          e ds                  occurs(30) inz
003400161006
003500161006     d dTAS          e ds                  prefix(§)
003600161006
003700161006     d dTASV         e ds                  prefix(§)
003800161006     d  TSV                    1     20    dim(20)                              Dtasv: Sigla Varia
003900161006     d  TVA                   21    140P 3 dim(20)                              Dtasv: Imp. Varia
004000161006     d  TEV                  141    160    dim(20)                              Dtasv: Esenz. Varia
004100161006
004200161006      // - Tabella 05 = Codici Area
004300161006     d ds05          e ds                  inz
004400161006      // - Tabella 17 = Direzioni commerciali
004500161006     d ds17          e ds                  inz
004600161006
004700161006     d Param           ds
004800161006     d  dataini                1      8  0
004900161006     d  aas                    1      4  0
005000161006     d  mgs                    5      8  0
005100161006     d  datafin                9     16  0
005200161006
005300161006      //---------------------------------------------------------------
005400161006      //?Definizione variabili globali.
005500161006      //---------------------------------------------------------------
005600161006      // - Flags booleani
005700161006     d wEoF            s               n   inz(*off)
005800161006
005900161006      // - Indici di schiera
006000161006     d xx              s              4s 0
006100161006
006200161006      // - Campi di comodo
006300161006     d wFIL            s              3s 0
006400161006
006500161006      //---------------------------------------------------------------
006600161006      //?Definizione procedure esterne.
006700161006      //---------------------------------------------------------------
006800161006
006900161006      //---------------------------------------------------------------
007000161006      //?Definizione prototipi.
007100161006      //---------------------------------------------------------------
007200161006
007300161006      //---------------------------------------------------------------
007400161006      //?Definizione key-list.
007500161006      //---------------------------------------------------------------
007600161006
007700161006      //---------------------------------------------------------------
007800161006      //?Riepilogo indicatori.
007900161006      //---------------------------------------------------------------
008000161006
008100161006      //---------------------------------------------------------------
008200161006      //?M A I N - L I N E
008300161006      //---------------------------------------------------------------
008400161006     c     *ENTRY        PLIST
008500161006     c                   PARM                    KPJBA
008600161006
008700161006      /free
008800161006
008900161006       //?Operazioni iniziali
009000161006       exsr RoutInz;
009100161006
009200161006       //?Elabora file
009300161006       exsr Elabora;
009400161006
009500161006       //?Operazioni finali
009600161006       exsr RoutEnd;
009700161006
009800161006       //--------------------------------------------------------------
009900161006       //?Operazioni iniziali.
010000161006       //--------------------------------------------------------------
010100161006       BEGSR RoutInz;
010200161006
010300161006       //?Impostazione opzioni per SQL?
010400161006         exec sql set option DynUsrPrf = *Owner, CloSqlCsr = *EndMod;
010500161006
010600161006       //?Passo i dati della KPJBU
010700161006         param = kpjbu;
010800161006
010900161006       //?Carico i tipi bolla + relativa DS
011000161006         wEoF = *off;
011100161006         xx = 1;
011200161006         TBLkut = 1;
011300161006         TBLcod = 'TB';
011400161006         setll (TBLkut:TBLcod) TABEL00F;
011500161006         reade (TBLkut:TBLcod) TABEL00F;
011600161006         DOW  not wEoF;
011700161006           IF  %eof(TABEL00F);
011800161006             leave;
011900161006           ENDIF;
012000161006           IF  TBLflg = *blanks;
012100161006             %occur(dsTB) = xx;
012200161006             dsTB = TBLuni;
012300161006             TBO(xx) = TBLkey;
012400161006             xx += 1;
012500161006           ENDIF;
012600161006           reade (TBLkut:TBLcod) TABEL00F;
012700161006         ENDDO;
012800161006
012900161006       ENDSR;
013000161006
013100161006       //--------------------------------------------------------------
013200161006       //?Elabora file
013300161006       //--------------------------------------------------------------
013400161006       BEGSR Elabora;
013500161006
013600161006         wEoF = *off;
013700161006
013800161006         setll (aas:mgs) TITAS38C;
013900161006         read  TITAS38C;
014000161006         DOW  not wEoF;
014100161006           IF  %eof(TITAS38C);
014200161006             wEoF = *on;
014300161006             leave;
014400161006           ENDIF;
014500161006         //?Elaboro solo le bolle con data spedizione nel range di date richiesto
014600161006           IF  (TASaas * 10000 + TASmgs) >= dataini and
014700161006               (TASaas * 10000 + TASmgs) <= datafin and
014800161006         //?e contabilizzate
014900161006                TASfic <> *blanks;
015000161006           //?Solo le bolle con flag FCB = '1' (bolle da fatturare)
015100161006             xx = 1;
015200161006             xx = %lookup(TAStbl : TBO);
015300161006             IF  xx > 0;
015400161006               %occur(dsTB) = xx;
015500161006               IF  §TBfcb = '1';
015600161006                 exsr FatGioTas;
015700161006               ENDIF;
015800161006             ENDIF;
015900161006           ENDIF;
016000161006
016100161006           read  TITAS38C;
016200161006         ENDDO;
016300161006
016400161006       ENDSR;
016500161006
016600161006       //--------------------------------------------------------------
016700161006       //?Bolle per Fatturato Giornaliero.
016800161006       //?dati da TITAS.
016900161006       //--------------------------------------------------------------
017000161006       BEGSR  FatGioTas;
017100161006
017200161006       //?Visto che bolla già contabilizzata non devo controllare niente
017300161006       //?e nemmeno tassare, prendo i dati da TITAS
017400161006       //?valorizzando le DS DTAS e DTASV
017500161006         clear dTASv;
017600161006         TSV(1) = TASsv1;
017700161006         TVA(1) = TASva1;
017800161006         TSV(2) = TASsv2;
017900161006         TVA(2) = TASva2;
018000161006         TSV(3) = TASsv3;
018100161006         TVA(3) = TASva3;
018200161006       //?Se la terza varia è piena cerco su TITA7 eventuali altre varie
018300161006         IF  TASsv3 <> *blanks;
018400161006           xx = 4;
018500161006           setll (TASaas:TASlnp:TASnrs:TASnsp:TAStbl) TITA730C;
018600161006           reade (TASaas:TASlnp:TASnrs:TASnsp:TAStbl) TITA730C;
018700161006           DOW  not %eof(TITA730C) and xx < 20;
018800161006             xx += 1;
018900161006             TSV(xx) = TA7svn;
019000161006             TVA(xx) = TA7van;
019100161006             reade (TASaas:TASlnp:TASnrs:TASnsp:TAStbl) TITA730C;
019200161006           ENDDO;
019300161006         ENDIF;
019400161006
019500161006         clear dTAS;
019600161006         §TASksc = TASksc;
019700161006         §TASdsp = TASaas * 10000 + TASmgs;
019800161006         §TASpor = TASpor;
019900161006         §TASimv = TASimv;
020000161006
020100161006         exsr agg_TFFGC;
020200161006
020300161006       ENDSR;
020400161006
020500161006       //--------------------------------------------------------------
020600161006       //?Scrivo/Aggiorno fatturato giornaliero.
020700161006       //--------------------------------------------------------------
020800161006       BEGSR  agg_TFFGC;
020900161006
021000161006         wFIL = %dec(%subst(%editc(§TASksc:'X'):1:3):3:0);
021100161006         chain (§TASdsp:wFIL) TFFGC01L;
021200161006
021300161006       //?Se c'è già un rcd aggiorno con i dati variabili
021400161006         IF  %found(TFFGC01L);
021500161006           exsr imposta_TFFGC;
021600161006           update TFFGC000;
021700161006         ENDIF;
021800161006
021900161006       //?Se rcd nuovo prima imposto i dati fissi + key poi i dati variabili
022000161006         IF  not %found(TFFGC01L);
022100161006           clear TFFGC000;
022200161006           FGCDATA = §TASdsp;
022300161006           FGCFIL  = wFIL;
022400161006
022500161006         //?valorizzo area e distretto + decodifiche
022600161006           chain wFIL AZORG01L;
022700161006           IF  %found(AZORG01L);
022800161006             FGCfild = ORGdes;
022900161006             FGCdit  = ORGfl3;
023000161006             FGCare  = ORGcar;
023100161006           ENDIF;
023200161006           clear ds05;
023300161006           TBLkut = 1;
023400161006           TBLcod = '05';
023500161006           TBLkey = %editc(FGCare:'X');
023600161006           chain (TBLkut:TBLcod:TBLkey) TABEL00F;
023700161006           IF  %found(TABEL00F);
023800161006             ds05 = TBLuni;
023900161006           ENDIF;
024000161006           FGCared = §05des;
024100161006           clear ds17;
024200161006           TBLkut = 1;
024300161006           TBLcod = '17';
024400161006           TBLkey = FGCdit;
024500161006           chain (TBLkut:TBLcod:TBLkey) TABEL00F;
024600161006           IF  %found( TABEL00F );
024700161006             ds17 = TBLuni;
024800161006           ENDIF;
024900161006           FGCditd = §17des;
025000161006
025100161006           FGCdtela = %dec(%date());
025200161006
025300161006         //?valorizzo i dati variabili
025400161006           exsr imposta_TFFGC;
025500161006           write TFFGC000;
025600161006         ENDIF;
025700161006
025800161006       ENDSR;
025900161006
026000161006       //--------------------------------------------------------------
026100161006       //?Imposto dati per il file Fatturato Giornaliero.
026200161006       //--------------------------------------------------------------
026300161006       BEGSR  imposta_TFFGC;
026400161006
026500161006       //?spedizioni tassate
026600161006         FGCtost += 1;
026700161006
026800161006       //?porto e imponibile
026900161006         FGCtpor += §TASpor;
027000161006         FGCtimp += §TASimv;
027100161006
027200161006       //?varie
027300161006         xx = 1;
027400161006         FOR xx by 1 to %elem(TSV);
027500161006           SELECT;
027600161006             WHEN  TSV(xx) = '2 ';
027700161006               FGCva2 += TVA(xx);
027800161006             WHEN  TSV(xx) = '5 ';
027900161006               FGCva5 += TVA(xx);
028000161006             WHEN  TSV(xx) = '& ';
028100161006               FGCvapa += TVA(xx);
028200161006             WHEN  TSV(xx) = '* ';
028300161006               FGCvadir += TVA(xx);
028400161006             WHEN  TSV(xx) = '+ ';
028500161006               FGCvasca += TVA(xx);
028600161006             WHEN  TSV(xx) = '= ';
028700161006               FGCvarba += TVA(xx);
028800161006             WHEN  TSV(xx) = 'a ';
028900161006               FGCvaaa += TVA(xx);
029000161006             WHEN  TSV(xx) = 'A ';
029100161006               FGCvaa += TVA(xx);
029200161006             WHEN  TSV(xx) = 'b ';
029300161006               FGCvabb += TVA(xx);
029400161006             WHEN  TSV(xx) = 'B ';
029500161006               FGCvab += TVA(xx);
029600161006             WHEN  TSV(xx) = 'c ';
029700161006               FGCvacc += TVA(xx);
029800161006             WHEN  TSV(xx) = 'C ';
029900161006               FGCvac += TVA(xx);
030000161006             WHEN  TSV(xx) = 'd ';
030100161006               FGCvadd += TVA(xx);
030200161006             WHEN  TSV(xx) = 'D ';
030300161006               FGCvad += TVA(xx);
030400161006             WHEN  TSV(xx) = 'e ';
030500161006               FGCvaee += TVA(xx);
030600161006             WHEN  TSV(xx) = 'f ';
030700161006               FGCvaff += TVA(xx);
030800161006             WHEN  TSV(xx) = 'F ';
030900161006               FGCvaf += TVA(xx);
031000161006             WHEN  TSV(xx) = 'G ';
031100161006               FGCvag += TVA(xx);
031200161006             WHEN  TSV(xx) = 'h ';
031300161006               FGCvahh += TVA(xx);
031400161006             WHEN  TSV(xx) = 'H ';
031500161006               FGCvah += TVA(xx);
031600161006             WHEN  TSV(xx) = 'I ';
031700161006               FGCvai += TVA(xx);
031800161006             WHEN  TSV(xx) = 'J ';
031900161006               FGCvaj += TVA(xx);
032000161006             WHEN  TSV(xx) = 'K ';
032100161006               FGCvak += TVA(xx);
032200161006             WHEN  TSV(xx) = 'L ';
032300161006               FGCval += TVA(xx);
032400161006             WHEN  TSV(xx) = 'm ';
032500161006               FGCvamm += TVA(xx);
032600161006             WHEN  TSV(xx) = 'M ';
032700161006               FGCvam += TVA(xx);
032800161006             WHEN  TSV(xx) = 'N ';
032900161006               FGCvan += TVA(xx);
033000161006             WHEN  TSV(xx) = 'O ';
033100161006               FGCvao += TVA(xx);
033200161006             WHEN  TSV(xx) = 'P ';
033300161006               FGCvap += TVA(xx);
033400161006             WHEN  TSV(xx) = 'Q ';
033500161006               FGCvaq += TVA(xx);
033600161006             WHEN  TSV(xx) = 'R ';
033700161006               FGCvar += TVA(xx);
033800161006             WHEN  TSV(xx) = 'S ';
033900161006               FGCvas += TVA(xx);
034000161006             WHEN  TSV(xx) = 'T ';
034100161006               FGCvat += TVA(xx);
034200161006             WHEN  TSV(xx) = 'U ';
034300161006               FGCvau += TVA(xx);
034400161006             WHEN  TSV(xx) = 'W ';
034500161006               FGCvaw += TVA(xx);
034600161006             WHEN  TSV(xx) = 'X ';
034700161006               FGCvax += TVA(xx);
034800161006             WHEN  TSV(xx) = 'Y ';
034900161006               FGCvay += TVA(xx);
035000161006             WHEN  TSV(xx) = 'Z ';
035100161006               FGCvaz += TVA(xx);
035200161006             WHEN  TSV(xx) = 't ';
035300161006               FGCvapr += TVA(xx);
035400161006             WHEN  TSV(xx) = 'k ';
035500161006               FGCvapk += TVA(xx);
035600161006           ENDSL;
035700161006         ENDFOR;
035800161006
035900161006       ENDSR;
036000161006
036100161006      /end-free
036200161006
036300161006       //--------------------------------------------------------------
036400161006       //?Operazioni finali.
036500161006       //--------------------------------------------------------------
036600161006       BEGSR RoutEnd;
036700161006
036800161006         *inLR = *on;
036900161006         return;
037000161006
037100161006       ENDSR;
037200161006
037300161006      /end-free
