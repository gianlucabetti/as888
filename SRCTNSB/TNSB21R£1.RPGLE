000100090717      *PARMS dbgview(*source)
000200090612       //--------------------------------------------------------------
000300090612       //?TNSB21R - Statistica consumi LdV e Segnacolli                ?
000400090612       //--------------------------------------------------------------
000500090612
000600090612     h decedit('0,') datedit(*ymd.) option(*nodebugio)
000700090612
000800090612       //--------------------------------------------------------------
000900090612       //?Dichiarazione file.                                          ?
001000090612       //--------------------------------------------------------------
001100090612
001200090715       // - Spedizioni x data spedizione
001300090714     fTITAS38C  if   e           k disk
001400090715
001500090715       // - O.R.M. x filiale di ritiro / data ritiro richiesta
001600090715     fFNORM13L  if   e           k disk
001700090612
001800090715       // - Organigramma
001900090612     fAZORG01L  if   e           k disk
002000090715
002100090715       // - Tabelle
002200090612     fTABEL00F  if   e           k disk
002300090612
002400090715       // - WF statistica Consumi moduli cartacei per LdV e O.R.M.
002500090721     fWFCLS00F  o    e             disk    usropn
002600090612
002700090612       //--------------------------------------------------------------
002800090612       //?Definizione costanti.                                        ?
002900090612       //--------------------------------------------------------------
003000090612
003100091005     d c_kut           c                   const(1)
003200090612
003300090612       //--------------------------------------------------------------
003400090612       //?Definizione schiere.                                         ?
003500090612       //--------------------------------------------------------------
003600090612
003700091005       // - Filiali DPD
003800091005     d $FilDPD         s                   like(TASlna)   dim(15)   inz
003900091005
004000090715       // - Codici bolla x Spedizioni di recupero (da scartare)
004100090721     d $CBOrec         s                   like(TAScbo)   dim(50)   inz
004200090715
004300090715       // - Codici trattamento merce "Disk C"
004400090721     d $CTMdkC         s                   like(TASctm)   dim(50)   inz
004500090721       // - Flag "Disk C Bartolini" ("B") / "Disk C Cliente" ("C")
004600090721     d $CTMdkCx        s                   like(§1Bdkc)   dim(50)   inz
004700090806
004800090806       // - Codici filiale con network estero
004900090806       //   (per le quali NON vanno elaborati i dati delle spedizioni)
005000090806     d $FilNtwE        s                   like(WLSfil)   dim(50)   inz
005100090721
005200090721       // - Filiali nel work-file WFCLS00F
005300090721     d $FilWF          s                   like(WLSfil)   dim(500)  inz
005400090721       // - Records del work-file WFCLS00F
005500090721     d $RecWF          s                   like(ds_WFCLS) dim(500)  inz
005600090612
005700090612       //--------------------------------------------------------------
005800090612       //?Definizione aree dati.                                       ?
005900090612       //--------------------------------------------------------------
006000090612
006100090612       // - Dati utente
006200090612     d §AzUte        e ds                  extname(AZUTE00F)
006300090612     d                                     dtaara
006400090612     d §DatiUte      e ds                  extname(dDatiUte)
006500090612     d                                     dtaara
006600090612
006700090612       //--------------------------------------------------------------
006800090612       //?Definizione strutture dati.                                  ?
006900090612       //--------------------------------------------------------------
007000090612
007100090612       // - Parametri ricevuti
007200090612     d KPJBA         e ds
007300090715     d TNSB21ds      e ds                  inz
007400090612
007500090612       // - Reperimento dati utente
007600090612     d TIBS34ds      e ds
007700090715
007800090715       // - Tab. "1B" = Codici Trattamento Merce
007900090715     d ds1B          e ds                  inz
008000090612       // - Tab. "3A" = Codici Bolla
008100090612     d ds3A          e ds                  inz
008200091005       // - Tab. "05" = Codici Area
008300091005     d ds05          e ds                  inz
008400090715
008500090806       // - 143ª descrizione organigramma
008600090806     d Og143         e ds                  inz
008700090715       // - 150ª descrizione organigramma
008800090715     d Og150         e ds                  inz
008900090714
009000090714       // - Reperimento capofila £6 (ma anche terminal par/arr...)
009100090714     d FNLV55ds      e ds                  inz
009200090714     d   D55tpt      e                     inz('6')
009300090721
009400090721       // - Struttura dati di comodo per la definizione della schiera
009500090722       //   $RecWF (con i campi incrementati leggendo TITAS)
009600090722     ***d ds_WFCLS      e ds                  inz  extname(WFCLS00F)
009700090722     d ds_WFCLS        ds                  inz
009800090722     d   WLSfil                            inz
009900091007     d***WLStfp                            inz
010000091007     d***WLSorm                            inz
010100090722     d   WLSlvp                            inz
010200090722     d   WLSlva                            inz
010300091007     d   WLSnca                            inz
010400090722     d   WLSecs                            inz
010500090722     d   WLSecc                            inz
010600090722     d   WLSecBs                           inz
010700090722     d   WLSecBc                           inz
010800091007     d   WLSecBsD                          inz
010900091007     d   WLSecBcD                          inz
011000090806     d   WLSeNbcs                          inz
011100090806     d   WLSeNbcc                          inz
011200090612
011300090612       // - Campi di comodo
011400090612     d ds_Time14       ds            14    inz
011500090715     d   wDate                        8  0 inz
011600090715     d   wTime                        6  0 inz
011700090714
011800090714     d ds_DataISO      ds             8    inz
011900090715     d   wDataAA                      4  0 inz
012000090715     d   wDataMG                      4  0 inz
012100090612
012200090612       //--------------------------------------------------------------
012300090612       //?Definizione variabili globali.                               ?
012400090612       //--------------------------------------------------------------
012500090612
012600090612       // - Flag booleani
012700090714     d $EoF            s               n   inz
012800090714     d $Ok             s               n   inz
012900090720     d $DDTsi          s               n   inz
013000090715
013100090715       // - Indici di schiera
013200090715     d xx              s              3  0 inz
013300090721     d yy              s              3  0 inz
013400090612
013500090715       // - Campi per memorizzazione codici filiale
013600090715     d wCFlnp          s                   like(ORGfil)  inz
013700090721     d w_WLSfil        s                   like(WLSfil)  inz
013800090612
013900090612       // - Campi per richiamo QCMDEXC
014000090612     d Qcmd            s            128    inz
014100090612
014200090612       //--------------------------------------------------------------
014300090612       //?Definizione procedure usate.                                 ?
014400090612       //--------------------------------------------------------------
014500090612
014600090612       // - Reperimento dati utente
014700090612      /copy gaitrasrc/srcProtoPR,TIBS34R
014800090714
014900090714       // - Reperimento terminal filiale
015000090714      /copy gaitrasrc/srcProtoPR,FNLV55R
015100090612
015200090612       // - Esecuzione comando di sistema
015300090612      /copy gaitrasrc/srcProtoPR,QCMDEXC
015400090612
015500090612       //--------------------------------------------------------------
015600090612       //?Definizione key-list.                                        ?
015700090612       //--------------------------------------------------------------
015800090612
015900090715     d k03titas38    e ds                  extname(TITAS38C : *key)
016000090715     d                                     inz  prefix(k_)
016100090612
016200090715     d k03tabel00    e ds                  extname(TABEL00F : *key)
016300090715     d                                     inz
016400090612
016500090612       //--------------------------------------------------------------
016600090612       //?M A I N - L I N E.                                           ?
016700090612       //--------------------------------------------------------------
016800090612
016900090612     c     *Entry        plist
017000090612     c                   parm                    KPJBA
017100090612
017200090612      /free
017300090612
017400090715       //Operazioni iniziali?
017500090612       exsr  sr_RoutInz;
017600090612
017700090715
017800090715       //Ciclo di lettura file spedizioni (TITAS)?
017900090715       clear  k03titas38;
018000090715       k_TASaas = wDataAA;
018100090715       k_TASmgs = wDataMG;
018200090715       k_TASksc = *loval;
018300090715       setll  %kds(k03titas38)  TITAS38C;
018400090714       read   TITAS38C;
018500090612
018600090714       DoW  NOT  %eof(TITAS38C);
018700090612
018800090714         // -?Selezione singolo record di TITAS?
018900090612         exsr  sr_Selez;
019000090714
019100090714         select;
019200090714
019300090714           // -?Fine ciclo di lettura?
019400090714           when  $EoF = *on;
019500090714             leave;
019600090714
019700090721           // -?Aggiornamento dati del work-file?
019800090714           when  $OK;
019900090721             exsr  sr_UpdRec;
020000090714
020100090714         endsl;
020200090612
020300090714         // -?Lettura record successivo di TITAS?
020400090714         read  TITAS38C;
020500090612
020600090714       EndDo;
020700090715
020800090715       //Operazioni finali?
020900090612       exsr  sr_RoutEnd;
021000090715
021100090612
021200090612       //--------------------------------------------------------------
021300090612       //?Operazioni iniziali.                                         ?
021400090612       //--------------------------------------------------------------
021500090612       BEGSR  sr_RoutInz;
021600090612
021700090612         *inLR = *on;
021800090715
021900090715         TNSB21ds = kpjbu;
022000090612
022100090715         // -Reperimento dati job?
022200090612         exsr  sr_DatiJob;
022300090715
022400090715         // -Intabellamento cod. bolla ("3A") per sped. di recupero?
022500090715         clear  xx;
022600090715         clear  k03tabel00;
022700091005         TBLkut = c_kut;
022800090715         TBLcod = '3A';
022900090715         setll  %kds(k03tabel00)  TABEL;
023000090715         reade  %kds(k03tabel00 : 2)  TABEL;
023100090715         DoW  NOT %eof(TABEL00F);
023200090715           if  TBLflg = *blank;
023300090715             ds3A = TBLuni;
023400090715             if  §3Arbl = 'R'   or   §3Arbl = 'C';
023500090715               xx += 1;
023600090715               $CBOrec(xx) = TBLkey;
023700090715             endif;
023800090715           endif;
023900090715           reade  %kds(k03tabel00 : 2)  TABEL;
024000090715         EndDo;
024100090715
024200090715         // -Intabellamento cod. trattamento merce ("1B") per "Disk C"?
024300090715         clear  xx;
024400090715         clear  k03tabel00;
024500091005         TBLkut = c_kut;
024600090715         TBLcod = '1B';
024700090715         setll  %kds(k03tabel00)  TABEL;
024800090715         reade  %kds(k03tabel00 : 2)  TABEL;
024900090715         DoW  NOT %eof(TABEL00F);
025000090715           if  TBLflg = *blank;
025100090715             ds1B = TBLuni;
025200090721             //if  §1Bf12 = 'S'   or   §1Bf12 = 'E'   or   §1Bf12 = 'B';
025300090721             if  §1Bdkc = 'B'   or   §1Bdkc = 'C';
025400090715               xx += 1;
025500090721               $CTMdkC(xx)  = TBLkey;
025600090721               $CTMdkCx(xx) = §1Bdkc;
025700090715             endif;
025800090715           endif;
025900090715           reade  %kds(k03tabel00 : 2)  TABEL;
026000090715         EndDo;
026100090806
026200090806         // -Intabellamento filiali con network estero?
026300090806         clear  xx;
026400091005         clear  yy;
026500090806         setll (*loval) AZORG;
026600090806         read  AZORG;
026700090806         DoW  Not %eof(AZORG01L);
026800090806           Og143 = ORGde3;
026900090806           if  §ogNTW = 'DPD'   or          // DPD
027000090806               §ogNTW = 'EEX'   or          // EuroExpress
027100090806               §ogNTW = 'EUP'   or          // Europolitan (Estero+SDI)
027200090806               §ogNTW = 'FED';              // FedEx
027300090806             xx += 1;
027400090806             $FilNtwE(xx) = ORGfil;
027500091005             if  §ogNTW = 'DPD';
027600091005               yy += 1;
027700091005               $FilDPD(yy) = ORGfil;
027800091005             endif;
027900090806           endif;
028000090806           read  AZORG;
028100090806         EndDo;
028200090715
028300090715         // -Impostazione data iniziale?
028400090715         ds_DataISO = %editc( SB21dti : 'X' );
028500090715
028600090715         // -Reperimento Data & Ora correnti?
028700090715         ds_Time14 = %subst( %char( %dec( %timestamp() ) )
028800090715                             : 1 : 14 );
028900090612
029000090612       ENDSR;
029100090612
029200090612       //--------------------------------------------------------------
029300090714       //?Reperimento Dati del job (Utente/Operativi).                 ?
029400090612       //--------------------------------------------------------------
029500090612       BEGSR  sr_DatiJob;
029600090612
029700090612         in(E) §AzUte;
029800090612         if NOT %error;
029900090612           in(E) §DatiUte;
030000090612         endif;
030100090612         if %error or RSut = *blanks;
030200090612           clear TIBS34ds;
030300090612           tibs34r(tibs34ds);
030400090612           in §AzUte;
030500090612           in §DatiUte;
030600090612         endif;
030700090612
030800090612       ENDSR;
030900090612
031000090612       //--------------------------------------------------------------
031100090714       //?Selezione record da TITAS                                    ?
031200090612       //--------------------------------------------------------------
031300090612       BEGSR  sr_Selez;
031400090612
031500090612         $Ok = *off;
031600090714
031700090714         select;
031800090714
031900090715           // -Data spedizione oltre il limite?
032000090715           when  (TASaas * 10000) + TASmgs > SB21dtf;
032100090714             $EoF = *on;
032200090714             leavesr;
032300090715
032400090715           // -Tipo bolla?
032500090715           when  TAStbl <> 'F1'   and   TAStbl <> 'A2';
032600090715             leavesr;
032700090715
032800090715           // -Spedizione senza segnacolli?
032900090715           when  TASncd = *zero;
033000090715             leavesr;
033100090715
033200090715           // -Spedizione di recupero?
033300090715           when  %lookup( TAScbo : $CBOrec ) > *zero;
033400090715             leavesr;
033500090612
033600090715         endsl;
033700090715
033800090715         $Ok = *on;
033900090612
034000090612       ENDSR;
034100090612
034200090612       //--------------------------------------------------------------
034300090721       //?Aggiornamento dati del work-file (con dati da TITAS)         ?
034400090612       //--------------------------------------------------------------
034500090721       BEGSR  sr_UpdRec;
034600090721
034700090721         // -Verifica se spedizione "DDT SÌ"?
034800090721         $DDTsi = (TASll1 = 'C' or TASll1 = 'S' or TASll1 = *blank);
034900090721
035000090721         // -Reperimento capofila £6 della LNP?
035100090721         //   (serve comunque per gli O.R.M.)
035200090721         if  TASlnp <> D55lin;
035300090721           reset  FNLV55ds;
035400090721           //D55tpt = '6'                   (già così)
035500090721           D55lin = TASlnp;
035600090721           D55drf = wDate;
035700090721           fnlv55r ( FNLV55ds );
035800090721           if  D55err <> *blank;
035900090721             D55tfa = D55lin;
036000090721           endif;
036100090721         endif;
036200090721         wCFlnp = D55tfa;
036300090715
036400090721         // -Impostazione capofila £6 per la filiale?
036500090721         If  $DDTsi;
036600090721
036700090721           w_WLSfil = wCFlnp;               // LNP per "DDT SÌ"
036800090721
036900090721         Else;
037000090721
037100090721           // -?Reperimento capofila £6 della LNA?
037200090721           if  TASlna <> D55lin;
037300090721             reset  FNLV55ds;
037400090721             //D55tpt = '6'                 (già così)
037500090721             D55lin = TASlna;
037600090721             D55drf = wDate;
037700090721             fnlv55r ( FNLV55ds );
037800090721             if  D55err <> *blank;
037900090721               D55tfa = D55lin;
038000090721             endif;
038100090721           endif;
038200090721           w_WLSfil = D55tfa;               // LNA altrimenti
038300090721
038400090721         EndIf;
038500090715
038600090721
038700090721         //Reperimento posizione elemento dati per la filiale?
038800090721         xx = %lookup(w_WLSfil : $FilWF);
038900090721         if  xx = *zero;
039000090721           xx = %lookup(*zero : $FilWF);
039100090721           if  xx = *zero;                  // Esaurita schiera:
039200090721             xx = *hival;                   // => GENERA ERRORE!
039300090721           endif;
039400090721           $FilWF(xx) = w_WLSfil;
039500090721           clear  ds_WFCLS;
039600090722         else;
039700090722           ds_WFCLS = $RecWF(xx);
039800090722         endif;
039900090722
040000090612
040100090721         //Aggiornamento dati per la filiale?
040200090722         WLSfil = $FilWF(xx);
040300090721
040400090806         // -Numero LdV (scartando network esteri)?
040500090806         select;
040600090806           when  %lookup(WLSfil : $FilNtwE) > *zero;
040700090806           when  $DDTsi;
040800090806             WLSlvp += 1;                   // in partenza ("DDT SÌ")
040900090806           other;
041000090806             WLSlva += 1;                   // in arrivo
041100091007             WLSnca += TASncl;              // in arrivo
041200090806         endsl;
041300090721
041400090721         $RecWF(xx) = ds_WFCLS;
041500090715
041600090721         // -Reperimento posizione elemento dati per la LNP?
041700090722         If  wCFlnp <> $FilWF(xx);
041800090722           xx = %lookup(wCFlnp : $FilWF);
041900090722           if  xx = *zero;
042000090722             xx = %lookup(*zero : $FilWF);
042100090722             if  xx = *zero;                // Esaurita schiera:
042200090722               xx = *hival;                 // => GENERA ERRORE!
042300090722             endif;
042400090722             $FilWF(xx) = wCFlnp;
042500090722             clear  ds_WFCLS;
042600090722           else;
042700090722             ds_WFCLS = $RecWF(xx);
042800090722           endif;
042900090722         EndIf;
043000090721
043100090721         // -Dettaglio spedizioni e colli (x LNP)?
043200090721         yy = %lookup( TASctm : $CTMdkC );
043300090715         select;
043400090715           // -?Disk-C?
043500090721           when  yy > *zero;
043600090715             WLSecs += 1;
043700090715             WLSecc += TASncl;
043800090721             if  $CTMdkCx(yy) = 'B';
043900091005               if  %lookup(TASlna : $FilDPD) = *zero;
044000091005                 WLSecBs += 1;
044100091005                 WLSecBc += TASncl;
044200091005               else;
044300091005                 WLSecBsD += 1;
044400091005                 WLSecBcD += TASncl;
044500091005               endif;
044600090721             endif;
044700090715           // -?Disk-B?
044800090715           when  TASnrs > *zero;
044900090715           // -?né Disk-C né Disk-B?
045000090715           other;
045100090806             WLSeNbcs += 1;
045200090806             WLSeNbcc += TASncl;
045300090715         endsl;
045400090721
045500090721         $RecWF(xx) = ds_WFCLS;
045600090612
045700090612       ENDSR;
045800090612
045900090612       //--------------------------------------------------------------
046000090714       //?Operazioni finali.                                           ?
046100090612       //--------------------------------------------------------------
046200090612       BEGSR  sr_RoutEnd;
046300090721
046400090721         // -Registrazione dati elaborati nel work-file?
046500090721         exsr  sr_WrtWFCLS;
046600090612
046700090721         // -Uscita?
046800090612         return;
046900090612
047000090612       ENDSR;
047100090721
047200090721       //--------------------------------------------------------------
047300090721       //?Registrazione dati nel file WFCLS00F                         ?
047400090721       //--------------------------------------------------------------
047500090721       BEGSR  sr_WrtWFCLS;
047600090721
047700090806         // -Pulizia / Apertura del work-file?
047800090806         Qcmd = 'CLRPFM file(';
047900090806         if  %subst( knsif : 7 : 1 ) = 'P';
048000090806            Qcmd = %trimr(Qcmd) + 'GAITRAAZP';
048100090806         else;
048200090806            Qcmd = %trimr(Qcmd) + 'GAITRAAZM';
048300090806         endif;
048400090806         Qcmd = %trimr(Qcmd) + '/WFCLS00F)';
048500090806         ExecuteCommand ( Qcmd : %size(Qcmd) );
048600090806
048700090721         open  WFCLS00F;
048800090721
048900090721         // -Registrazione record nel work-file?
049000090721         FOR  xx = 1  TO  %elem($FilWF);
049100090721
049200090721           if  $FilWF(xx) = *zero;
049300090721             leave;
049400090721           endif;
049500090721
049600090721           clear  WFCLS000;
049700090721
049800090721           // -?Reperimento dati relativi alle spedizioni (già calc.)?
049900091008           //  ?ed impostazione campi del WF con tali dati (già calc.)?
050000090721           ds_WFCLS = $RecWF(xx);
050100090721
050200090721           // -?Impostazione campi chiave se nuovo record?
050300090721           WLSute  = knmus;            // - Utente
050400090721           WLSdta  = wDate;            // - Data di lancio
050500090721           WLSdti  = SB21dti;          // - Data inizio periodo
050600090721           WLSdtf  = SB21dtf;          // - Data fine periodo
050700090721           WLSfil  = $FilWF(xx);       // - Filiale
050800090721           exsr  sr_NewFil;
050900090721
051000090721           // -?Conteggio numero O.R.M. per la filiale nel periodo?
051100090806           //  ?(NON per network esteri)?
051200090806           if  %lookup(WLSfil : $FilNtwE) = *zero;
051300090806             exsr  sr_Calc_ORM;
051400090806           endif;
051500090721
051600090721           // -?Scrittura/Aggiornamento del record?
051700090721           write  WFCLS000;
051800090721           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
051900090721
052000090721         ENDFOR;
052100090721
052200090721         // -Chiusura del work-file?
052300090721         close WFCLS00F;
052400090721
052500090721       ENDSR;
052600090721
052700090721       //--------------------------------------------------------------
052800090721       //?Reperimento dati della filiale                               ?
052900090721       //--------------------------------------------------------------
053000090721       BEGSR  sr_NewFil;
053100090721
053200090721         // -Reperimento dati anagrafici?
053300091005         clear  Og150;
053400091008         clear  ORGdes;
053500091008         clear  ORGcar;
053600091005         clear  ds05;
053700090721         chain  (WLSfil)  AZORG;
053800090721         if  %found(AZORG01L);
053900091008           WLSfilD = ORGdes;           // - Decodifica filiale
054000091008           WLScar  = ORGcar;           // - Codice area
054100090721           Og150 = ORGdf0;
054200091008           WLStis  = §OGtis;           // - Tipo impianto smistacolli
054300091008           WLSpck  = §OGpck;           // - Flag picking (da Og150)
054400091005           TBLkut = c_kut;
054500091005           TBLcod = '05';
054600091005           TBLkey = %editc(ORGcar : 'X');
054700091005           chain  %kds(k03tabel00)  TABEL;
054800091005           if  %found(TABEL00F)   and   TBLflg = *blank;
054900091005             ds05 = TBLuni;
055000091005           endif;
055100091008           WLScard = §05des;           // - Decodifica area
055200090721         endif;
055300090721
055400090721         // -Reperimento terminal di partenza (x O.R.M.)?
055500090721         clear  FNLV55ds;
055600090721         if  ORGfag = 'A'   or   ORGfag = 'F';
055700090721           D55tpt = 'P';
055800090721           D55lin = WLSfil;
055900090721           D55drf = wDate;
056000090721           fnlv55r ( FNLV55ds );
056100090721           if  D55err <> *blank;
056200090721             D55tfp = D55lin;
056300090721           endif;
056400090721         endif;
056500091008         WLStfp = D55tfp;              // - Terminal partenza
056600091007         chain  (D55tfp)  AZORG;
056700091007         if  %found(AZORG01L);
056800091008           WLStfpD = ORGdes;           // - Decodifica terminal partenza
056900091007         endif;
057000091008
057100090721
057200090721       ENDSR;
057300090721
057400090721       //--------------------------------------------------------------
057500090721       //?Calcolo n° di O.R.M. nel periodo per il cliente              ?
057600090721       //--------------------------------------------------------------
057700090721       BEGSR  sr_Calc_ORM;
057800090721
057900090721         //Ciclo di elaborazione O.R.M. (FNORM)?
058000090721         setll  (WLSfil)  FNORM000;
058100090721         reade  (WLSfil)  FNORM000;
058200090721
058300090721         DoW  NOT %eof(FNORM13L);
058400090721
058500090721           // -?Incremento numero di O.R.M.?
058600090721           if  ORMdao >= SB21dti   and
058700090721               ORMdao <= SB21dtf;
058800090721             WLSorm += 1;
058900090721           endif;
059000090721
059100090721           // -?Lettura record successivo di FNORM?
059200090721           reade  (WLSfil)  FNORM000;
059300090721
059400090721         EndDo;
059500090721
059600090721       ENDSR;
059700090612
059800090612      /end-free
