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
001400140305     fFIAR531C  if   e           k disk
001500090715
001600090715       // - O.R.M. x filiale di ritiro / data ritiro richiesta
001700090715     fFNORM13L  if   e           k disk
001800090612
001900090715       // - Organigramma
002000090612     fAZORG01L  if   e           k disk
002100090715
002200090715       // - Tabelle
002300090612     fTABEL00F  if   e           k disk
002400140305
002500090612
002600090715       // - WF statistica Consumi moduli cartacei per LdV e O.R.M.
002700140305     fWFCLS11l  uf   e           k disk    usropn
002800140305     f                                     rename(wfcls000:wfcls011)
002900140305     fWFCLS10F  o    e             disk    usropn
003000090612
003100090612       //--------------------------------------------------------------
003200090612       //?Definizione costanti.                                        ?
003300090612       //--------------------------------------------------------------
003400090612
003500091005     d c_kut           c                   const(1)
003600090612
003700090612       //--------------------------------------------------------------
003800090612       //?Definizione schiere.                                         ?
003900090612       //--------------------------------------------------------------
004000090612
004100091005       // - Filiali DPD
004200091005     d $FilDPD         s                   like(TASlna)   dim(15)   inz
004300091005
004400090715       // - Codici bolla x Spedizioni di recupero (da scartare)
004500090721     d $CBOrec         s                   like(TAScbo)   dim(50)   inz
004600090715
004700090715       // - Codici trattamento merce "Disk C"
004800090721     d $CTMdkC         s                   like(TASctm)   dim(50)   inz
004900090721       // - Flag "Disk C Bartolini" ("B") / "Disk C Cliente" ("C")
005000090721     d $CTMdkCx        s                   like(§1Bdkc)   dim(50)   inz
005100090806
005200090806       // - Codici filiale con network estero
005300090806       //   (per le quali NON vanno elaborati i dati delle spedizioni)
005400090806     d $FilNtwE        s                   like(WLSfil)   dim(50)   inz
005500090721
005600140305       // - Filiali nel work-file WFCLS10F
005700090721     d $FilWF          s                   like(WLSfil)   dim(500)  inz
005800140305       // - Records del work-file WFCLS10F
005900090721     d $RecWF          s                   like(ds_WFCLS) dim(500)  inz
006000090612
006100090612       //--------------------------------------------------------------
006200090612       //?Definizione aree dati.                                       ?
006300090612       //--------------------------------------------------------------
006400090612
006500090612       // - Dati utente
006600090612     d §AzUte        e ds                  extname(AZUTE00F)
006700090612     d                                     dtaara
006800090612     d §DatiUte      e ds                  extname(dDatiUte)
006900090612     d                                     dtaara
007000090612
007100090612       //--------------------------------------------------------------
007200090612       //?Definizione strutture dati.                                  ?
007300090612       //--------------------------------------------------------------
007400090612
007500090612       // - Parametri ricevuti
007600090612     d KPJBA         e ds
007700090715     d TNSB21ds      e ds                  inz
007800140305     d dar5gen       e ds                  inz
007900090612
008000090612       // - Reperimento dati utente
008100090612     d TIBS34ds      e ds
008200090715
008300090715       // - Tab. "1B" = Codici Trattamento Merce
008400090715     d ds1B          e ds                  inz
008500090612       // - Tab. "3A" = Codici Bolla
008600090612     d ds3A          e ds                  inz
008700091005       // - Tab. "05" = Codici Area
008800091005     d ds05          e ds                  inz
008900140305
009000140305     d ds3C          e ds                  inz
009100090715
009200090806       // - 143ª descrizione organigramma
009300090806     d Og143         e ds                  inz
009400090715       // - 150ª descrizione organigramma
009500090715     d Og150         e ds                  inz
009600090714
009700090714       // - Reperimento capofila £6 (ma anche terminal par/arr...)
009800090714     d FNLV55ds      e ds                  inz
009900090714     d   D55tpt      e                     inz('6')
010000090721
010100090721       // - Struttura dati di comodo per la definizione della schiera
010200090722       //   $RecWF (con i campi incrementati leggendo TITAS)
010300140305     ***d ds_WFCLS      e ds                  inz  extname(WFCLS10F)
010400090722     d ds_WFCLS        ds                  inz
010500090722     d   WLSfil                            inz
010600091007     d***WLStfp                            inz
010700091007     d***WLSorm                            inz
010800090722     d   WLSlvp                            inz
010900140305     d   WLSlvaNPDA                        inz
011000140305     d   WLSlvaSPDA                        inz
011100140305     d   WLSlpkl                           inz
011200091007     d   WLSnca                            inz
011300090722     d   WLSecs                            inz
011400090722     d   WLSecc                            inz
011500090722     d   WLSecBs                           inz
011600090722     d   WLSecBc                           inz
011700091007     d   WLSecBsD                          inz
011800091007     d   WLSecBcD                          inz
011900140305     d   WLSeBB                            inz
012000140305     d   WLSeBBws                          inz
012100090806     d   WLSeNbcs                          inz
012200090806     d   WLSeNbcc                          inz
012300090612
012400090612       // - Campi di comodo
012500090612     d ds_Time14       ds            14    inz
012600090715     d   wDate                        8  0 inz
012700090715     d   wTime                        6  0 inz
012800090714
012900090714     d ds_DataISO      ds             8    inz
013000090715     d   wDataAA                      4  0 inz
013100090715     d   wDataMG                      4  0 inz
013200090612
013300090612       //--------------------------------------------------------------
013400090612       //?Definizione variabili globali.                               ?
013500090612       //--------------------------------------------------------------
013600090612
013700090612       // - Flag booleani
013800090714     d $EoF            s               n   inz
013900090714     d $Ok             s               n   inz
014000090720     d $DDTsi          s               n   inz
014100090715
014200090715       // - Indici di schiera
014300090715     d xx              s              3  0 inz
014400090721     d yy              s              3  0 inz
014500140305
014600140305     d totRotolo       s              9  0 inz
014700090612
014800090715       // - Campi per memorizzazione codici filiale
014900090715     d wCFlnp          s                   like(ORGfil)  inz
015000090721     d w_WLSfil        s                   like(WLSfil)  inz
015100140305     d ktrd            s                   like(ar5trd)  inz('GEN')
015200140305     d savtfp          s                   like(WLStfp)  inz
015300140305     d newtfp          s                   like(WLStfp)  inz
015400140305     d newfil          s                   like(WLSfil)  inz
015500090612
015600090612       // - Campi per richiamo QCMDEXC
015700090612     d Qcmd            s            128    inz
015800090612
015900090612       //--------------------------------------------------------------
016000090612       //?Definizione procedure usate.                                 ?
016100090612       //--------------------------------------------------------------
016200090612
016300090612       // - Reperimento dati utente
016400090612      /copy gaitrasrc/srcProtoPR,TIBS34R
016500090714
016600090714       // - Reperimento terminal filiale
016700090714      /copy gaitrasrc/srcProtoPR,FNLV55R
016800090612
016900090612       // - Esecuzione comando di sistema
017000090612      /copy gaitrasrc/srcProtoPR,QCMDEXC
017100090612
017200090612       //--------------------------------------------------------------
017300090612       //?Definizione key-list.                                        ?
017400090612       //--------------------------------------------------------------
017500090612
017600090715     d k03titas38    e ds                  extname(TITAS38C : *key)
017700090715     d                                     inz  prefix(k_)
017800090612
017900090715     d k03tabel00    e ds                  extname(TABEL00F : *key)
018000090715     d                                     inz
018100090612
018200090612       //--------------------------------------------------------------
018300090612       //?M A I N - L I N E.                                           ?
018400090612       //--------------------------------------------------------------
018500090612
018600090612     c     *Entry        plist
018700090612     c                   parm                    KPJBA
018800090612
018900090612      /free
019000090612
019100090715       //Operazioni iniziali?
019200090612       exsr  sr_RoutInz;
019300090612
019400090715
019500090715       //Ciclo di lettura file spedizioni (TITAS)?
019600090715       clear  k03titas38;
019700090715       k_TASaas = wDataAA;
019800090715       k_TASmgs = wDataMG;
019900090715       k_TASksc = *loval;
020000090715       setll  %kds(k03titas38)  TITAS38C;
020100090714       read   TITAS38C;
020200090612
020300090714       DoW  NOT  %eof(TITAS38C);
020400090612
020500090714         // -?Selezione singolo record di TITAS?
020600090612         exsr  sr_Selez;
020700090714
020800090714         select;
020900090714
021000090714           // -?Fine ciclo di lettura?
021100090714           when  $EoF = *on;
021200090714             leave;
021300090714
021400090721           // -?Aggiornamento dati del work-file?
021500090714           when  $OK;
021600090721             exsr  sr_UpdRec;
021700090714
021800090714         endsl;
021900090612
022000090714         // -?Lettura record successivo di TITAS?
022100090714         read  TITAS38C;
022200090612
022300090714       EndDo;
022400140305
022500140305         // -Registrazione dati elaborati nel work-file?
022600140305         exsr  sr_WrtWFCLS;
022700140305
022800140305       //rileggo i record e imposto i totali
022900140305       exsr  sr_ScriviTot;
023000090715
023100090715       //Operazioni finali?
023200090612       exsr  sr_RoutEnd;
023300090715
023400090612
023500090612       //--------------------------------------------------------------
023600090612       //?Operazioni iniziali.                                         ?
023700090612       //--------------------------------------------------------------
023800090612       BEGSR  sr_RoutInz;
023900090612
024000090612         *inLR = *on;
024100090715
024200090715         TNSB21ds = kpjbu;
024300090612
024400090715         // -Reperimento dati job?
024500090612         exsr  sr_DatiJob;
024600090715
024700090715         // -Intabellamento cod. bolla ("3A") per sped. di recupero?
024800090715         clear  xx;
024900090715         clear  k03tabel00;
025000091005         TBLkut = c_kut;
025100090715         TBLcod = '3A';
025200090715         setll  %kds(k03tabel00)  TABEL;
025300090715         reade  %kds(k03tabel00 : 2)  TABEL;
025400090715         DoW  NOT %eof(TABEL00F);
025500090715           if  TBLflg = *blank;
025600090715             ds3A = TBLuni;
025700100409        //     if  §3Arbl = 'R'   or   §3Arbl = 'C';
025800100409             if  §3Arbl = 'R'   ;
025900090715               xx += 1;
026000090715               $CBOrec(xx) = TBLkey;
026100090715             endif;
026200090715           endif;
026300090715           reade  %kds(k03tabel00 : 2)  TABEL;
026400090715         EndDo;
026500090715
026600090715         // -Intabellamento cod. trattamento merce ("1B") per "Disk C"?
026700090715         clear  xx;
026800090715         clear  k03tabel00;
026900091005         TBLkut = c_kut;
027000090715         TBLcod = '1B';
027100090715         setll  %kds(k03tabel00)  TABEL;
027200090715         reade  %kds(k03tabel00 : 2)  TABEL;
027300090715         DoW  NOT %eof(TABEL00F);
027400090715           if  TBLflg = *blank;
027500090715             ds1B = TBLuni;
027600090721             //if  §1Bf12 = 'S'   or   §1Bf12 = 'E'   or   §1Bf12 = 'B';
027700090721             if  §1Bdkc = 'B'   or   §1Bdkc = 'C';
027800090715               xx += 1;
027900090721               $CTMdkC(xx)  = TBLkey;
028000090721               $CTMdkCx(xx) = §1Bdkc;
028100090715             endif;
028200090715           endif;
028300090715           reade  %kds(k03tabel00 : 2)  TABEL;
028400090715         EndDo;
028500090806
028600090806         // -Intabellamento filiali con network estero?
028700090806         clear  xx;
028800091005         clear  yy;
028900090806         setll (*loval) AZORG;
029000090806         read  AZORG;
029100090806         DoW  Not %eof(AZORG01L);
029200090806           Og143 = ORGde3;
029300090806           if  §ogNTW = 'DPD'   or          // DPD
029400090806               §ogNTW = 'EEX'   or          // EuroExpress
029500090806               §ogNTW = 'EUP'   or          // Europolitan (Estero+SDI)
029600090806               §ogNTW = 'FED';              // FedEx
029700090806             xx += 1;
029800090806             $FilNtwE(xx) = ORGfil;
029900091005             if  §ogNTW = 'DPD';
030000091005               yy += 1;
030100091005               $FilDPD(yy) = ORGfil;
030200091005             endif;
030300090806           endif;
030400090806           read  AZORG;
030500090806         EndDo;
030600090715
030700090715         // -Impostazione data iniziale?
030800090715         ds_DataISO = %editc( SB21dti : 'X' );
030900090715
031000090715         // -Reperimento Data & Ora correnti?
031100090715         ds_Time14 = %subst( %char( %dec( %timestamp() ) )
031200090715                             : 1 : 14 );
031300090612
031400090612       ENDSR;
031500090612
031600090612       //--------------------------------------------------------------
031700090714       //?Reperimento Dati del job (Utente/Operativi).                 ?
031800090612       //--------------------------------------------------------------
031900090612       BEGSR  sr_DatiJob;
032000090612
032100090612         in(E) §AzUte;
032200090612         if NOT %error;
032300090612           in(E) §DatiUte;
032400090612         endif;
032500090612         if %error or RSut = *blanks;
032600090612           clear TIBS34ds;
032700090612           tibs34r(tibs34ds);
032800090612           in §AzUte;
032900090612           in §DatiUte;
033000090612         endif;
033100090612
033200090612       ENDSR;
033300090612
033400090612       //--------------------------------------------------------------
033500090714       //?Selezione record da TITAS                                    ?
033600090612       //--------------------------------------------------------------
033700090612       BEGSR  sr_Selez;
033800090612
033900090612         $Ok = *off;
034000090714
034100090714         select;
034200090714
034300090715           // -Data spedizione oltre il limite?
034400090715           when  (TASaas * 10000) + TASmgs > SB21dtf;
034500090714             $EoF = *on;
034600090714             leavesr;
034700090715
034800090715           // -Tipo bolla?
034900100409           // when  TAStbl <> 'F1'   and   TAStbl <> 'A2';
035000100409           //  leavesr;
035100090715
035200090715           // -Spedizione senza segnacolli?
035300100409           // when  TASncd = *zero and TASFNS = *blank;
035400100409           //  leavesr;
035500090715
035600100409           // -Escluse solo Spedizioni di recupero?
035700090715           when  %lookup( TAScbo : $CBOrec ) > *zero;
035800090715             leavesr;
035900090612
036000090715         endsl;
036100090715
036200090715         $Ok = *on;
036300090612
036400090612       ENDSR;
036500090612
036600090612       //--------------------------------------------------------------
036700090721       //?Aggiornamento dati del work-file (con dati da TITAS)         ?
036800090612       //--------------------------------------------------------------
036900090721       BEGSR  sr_UpdRec;
037000140305
037100140305         // -Lettura fiar5 ?
037200140305         chain (tasaas:taslnp:tasnrs:tasnsp:ktrd) fiar531c  ;
037300140305         if %found(fiar531c) ;
037400140305         dar5gen=ar5uni  ;
037500140305         else  ;
037600140305         clear dar5gen  ;
037700140305         endif  ;
037800090721
037900090721         // -Reperimento capofila £6 della LNP?
038000090721         //   (serve comunque per gli O.R.M.)
038100090721         if  TASlnp <> D55lin;
038200090721           reset  FNLV55ds;
038300090721           //D55tpt = '6'                   (già così)
038400090721           D55lin = TASlnp;
038500090721           D55drf = wDate;
038600090721           fnlv55r ( FNLV55ds );
038700090721           if  D55err <> *blank;
038800090721             D55tfa = D55lin;
038900090721           endif;
039000090721         endif;
039100090721         wCFlnp = D55tfa;
039200090715
039300090721         // -Impostazione capofila £6 per la filiale?
039400100412         // If  $DDTsi;
039500090721
039600100412         //  w_WLSfil = wCFlnp;               // LNP per "DDT SÌ"
039700090721
039800100412         //Else;
039900090721
040000100412           // -?Reperimento SEMPRE capofila £6 della LNA?
040100090721           if  TASlna <> D55lin;
040200090721             reset  FNLV55ds;
040300090721             //D55tpt = '6'                 (già così)
040400090721             D55lin = TASlna;
040500090721             D55drf = wDate;
040600090721             fnlv55r ( FNLV55ds );
040700090721             if  D55err <> *blank;
040800090721               D55tfa = D55lin;
040900090721             endif;
041000100412            endif;
041100090721           w_WLSfil = D55tfa;               // LNA altrimenti
041200090721
041300100412         //EndIf;
041400090715
041500100412         //conteggio colli per linea di arrivo:?
041600090721
041700090721         //Reperimento posizione elemento dati per la filiale?
041800090721         xx = %lookup(w_WLSfil : $FilWF);
041900090721         if  xx = *zero;
042000090721           xx = %lookup(*zero : $FilWF);
042100090721           if  xx = *zero;                  // Esaurita schiera:
042200090721             xx = *hival;                   // => GENERA ERRORE!
042300090721           endif;
042400090721           $FilWF(xx) = w_WLSfil;
042500090721           clear  ds_WFCLS;
042600090722         else;
042700090722           ds_WFCLS = $RecWF(xx);
042800090722         endif;
042900100412
043000100412         // -Numero LdV (scartando network esteri)?
043100140305         WLSfil = $FilWF(xx);
043200140305           if    %lookup(WLSfil : $FilNtwE) <= *zero;
043300140331           //  con PDA
043400140331             if §AR5PDACO='  '  ;
043500140331             WLSlvaspda += 1;                   // in arrivoPDA
043600140331             else  ;
043700140331             WLSlvanpda += 1;                   // in arrivo noPDA
043800140331             endif  ;
043900140331
044000140305             WLSnca += TASncl;                 // in arrivo
044100140305
044200140305             // verifico se ha packing list o pdf
044300140305             if §ar5alx='S' or §ar5als='S'  ;
044400140305             WLSlpkl += 1;
044500140305             endif  ;
044600140305           endif   ;
044700140305             $RecWF(xx) = ds_WFCLS;
044800090722
044900090612
045000090721         //Aggiornamento dati per la filiale?
045100100412         //WLSfil = $FilWF(xx);
045200090721
045300090806         // -Numero LdV (scartando network esteri)?
045400100412         //select;
045500100412         //  when  %lookup(WLSfil : $FilNtwE) > *zero;
045600100412         //  when  $DDTsi;
045700100412         //    WLSlvp += 1;                   // in partenza ("DDT SÌ")
045800100412         //  other;
045900100412         //    WLSlva += 1;                   // in arrivo
046000100412         //    WLSnca += TASncl;              // in arrivo
046100100412         //endsl;
046200090721
046300100412         //$RecWF(xx) = ds_WFCLS;
046400090715
046500090721         // -Reperimento posizione elemento dati per la LNP?
046600090722         If  wCFlnp <> $FilWF(xx);
046700090722           xx = %lookup(wCFlnp : $FilWF);
046800090722           if  xx = *zero;
046900090722             xx = %lookup(*zero : $FilWF);
047000090722             if  xx = *zero;                // Esaurita schiera:
047100090722               xx = *hival;                 // => GENERA ERRORE!
047200090722             endif;
047300090722             $FilWF(xx) = wCFlnp;
047400090722             clear  ds_WFCLS;
047500090722           else;
047600090722             ds_WFCLS = $RecWF(xx);
047700090722           endif;
047800090722         EndIf;
047900100412
048000100412         // -Verifica se spedizione "DDT SÌ"?
048100100412         $DDTsi = (TASll1 = 'C' or TASll1 = 'S' or TASll1 = *blank);
048200100412
048300100412         // -Numero LdV DDT SI escluso export?
048400100412           if    $DDTsi  ;
048500100412             WLSlvp += 1;                   // in partenza ("DDT SÌ")
048600100412           endif   ;
048700090721
048800090721         // -Dettaglio spedizioni e colli (x LNP)?
048900090721         yy = %lookup( TASctm : $CTMdkC );
049000090715         select;
049100090715           // -?Disk-C?
049200090721           when  yy > *zero;
049300090715             WLSecs += 1;
049400090715             WLSecc += TASncl;
049500090721             if  $CTMdkCx(yy) = 'B';
049600091005               if  %lookup(TASlna : $FilDPD) = *zero;
049700091005                 WLSecBs += 1;
049800091005                 WLSecBc += TASncl;
049900091005               else;
050000091005                 WLSecBsD += 1;
050100091005                 WLSecBcD += TASncl;
050200091005               endif;
050300090721             endif;
050400090715           // -?Disk-B?
050500090715           when  TASnrs > *zero;
050600140305             WLSebb += TASncl;
050700140305             // di cui easy web/sped
050800140305
050900140305             tblkey=%editc(tasccm:'X')  ;
051000140305           chain  (1:'3C': tblkey)  TABEL;
051100140305           if %found(tabel00f) ;
051200140305           ds3c=tbluni  ;
051300140305           if §3ccba='ESWEB' or §3ccba='ESYSP'  ;
051400140305             WLSEBBWS  += TASncl;
051500140305           endif;
051600140305           endif;
051700140305
051800090715           // -?né Disk-C né Disk-B?
051900090715           other;
052000090806             WLSeNbcs += 1;
052100090806             WLSeNbcc += TASncl;
052200090715         endsl;
052300090721
052400090721         $RecWF(xx) = ds_WFCLS;
052500090612
052600090612       ENDSR;
052700140305
052800140305       //--------------------------------------------------------------
052900140305       //?Rileggo per scrivere i totali                                ?
053000140305       //--------------------------------------------------------------
053100140305       BEGSR  sr_ScriviTot;
053200140305       clear totrotolo  ;
053300140305       clear savtfp   ;
053400140305
053500140305       setll *loval   wfcls11l  ;
053600140305       read  wfcls11l  ;
053700140305       dow  not %eof(wfcls11l)  ;
053800140305
053900140305       // totale LDV
054000140305       // DDT si  + pack.list  MAGGIORATO DEL 10%
054100140305       WLSLDVT = WLSLVP + (WLSLPKL*110/100) ;
054200140305
054300140305       //  con   PDA
054400140305       // +  bolle arrivo si pda + 10% /4
054500140305       if WLSLVASPDA> 0  ;
054600140305       WLSLDVT +=  ((wlslvaspda*110/100)/4) ;
054700140305       endif  ;
054800140305
054900140305       // senza PDA
055000140305       // +  bolle arrivo si pda + 10% /2
055100140305       if WLSLVAnPDA> 0  ;
055200140305       WLSLDVT +=  ((wlslvanpda*110/100)/2) ;
055300140305       endif  ;
055400140305
055500140305       // etichette ARANCIONI solo se filiale 2=liv con picking
055600140305       //  colli arrivati maggiorati del 10%
055700140305       if WLSPCK = 'S' and wlstfp<>wlsfil  ;
055800140305         WLSETAR  = (WLSNCA*110/100)  ;
055900140305       endif  ;
056000140305
056100140305       // Etichette IN PIEGA
056200140305       WLSETPG  = WLSENBCC  ;
056300140305
056400140305       update wfcls011  ;
056500140305
056600140305       // Etichette in ROTOLO  ;
056700140305       //  sommo i disK C di tutto il terminal e poi scrivo solo sul terminal
056800140305       if savtfp<>wlstfp and savtfp>0  ;
056900140305       newtfp=wlstfp  ;
057000140305       newfil=wlsfil  ;
057100140305       chain (savtfp:savtfp)   wfcls11l  ;
057200140305       if %found(wfcls11l)  ;
057300140305       WLSETROT = totRotolo  ;
057400140305       update wfcls011  ;
057500140305       chain (newtfp:newfil)   wfcls11l  ;
057600140305       endif  ;
057700140305
057800140305       clear totrotolo  ;
057900140305       endif  ;
058000140305
058100140305       savtfp=wlstfp  ;
058200140305       totROTOLO += WLSECC  ;
058300140305
058400140305       read  wfcls11l  ;
058500140305       enddo  ;
058600140305
058700140305       ENDSR;
058800090612
058900090612       //--------------------------------------------------------------
059000090714       //?Operazioni finali.                                           ?
059100090612       //--------------------------------------------------------------
059200090612       BEGSR  sr_RoutEnd;
059300090721
059400090612
059500090721         // -Uscita?
059600090612         return;
059700090612
059800090612       ENDSR;
059900090721
060000090721       //--------------------------------------------------------------
060100140305       //?Registrazione dati nel file WFCLS10F                         ?
060200090721       //--------------------------------------------------------------
060300090721       BEGSR  sr_WrtWFCLS;
060400090721
060500090806         // -Pulizia / Apertura del work-file?
060600090806         Qcmd = 'CLRPFM file(';
060700090806         if  %subst( knsif : 7 : 1 ) = 'P';
060800090806            Qcmd = %trimr(Qcmd) + 'GAITRAAZP';
060900090806         else;
061000090806            Qcmd = %trimr(Qcmd) + 'GAITRAAZM';
061100090806         endif;
061200140305         Qcmd = %trimr(Qcmd) + '/WFCLS10F)';
061300090806         ExecuteCommand ( Qcmd : %size(Qcmd) );
061400090806
061500140305         open  WFCLS10F;
061600140305         open  WFCLS11l;
061700090721
061800090721         // -Registrazione record nel work-file?
061900090721         FOR  xx = 1  TO  %elem($FilWF);
062000090721
062100090721           if  $FilWF(xx) = *zero;
062200090721             leave;
062300090721           endif;
062400090721
062500090721           clear  WFCLS000;
062600090721
062700090721           // -?Reperimento dati relativi alle spedizioni (già calc.)?
062800091008           //  ?ed impostazione campi del WF con tali dati (già calc.)?
062900090721           ds_WFCLS = $RecWF(xx);
063000090721
063100090721           // -?Impostazione campi chiave se nuovo record?
063200090721           WLSute  = knmus;            // - Utente
063300090721           WLSdta  = wDate;            // - Data di lancio
063400090721           WLSdti  = SB21dti;          // - Data inizio periodo
063500090721           WLSdtf  = SB21dtf;          // - Data fine periodo
063600090721           WLSfil  = $FilWF(xx);       // - Filiale
063700090721           exsr  sr_NewFil;
063800090721
063900090721           // -?Conteggio numero O.R.M. per la filiale nel periodo?
064000090806           //  ?(NON per network esteri)?
064100090806           if  %lookup(WLSfil : $FilNtwE) = *zero;
064200090806             exsr  sr_Calc_ORM;
064300090806           endif;
064400090721
064500090721           // -?Scrittura/Aggiornamento del record?
064600090721           write  WFCLS000;
064700090721           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
064800090721
064900090721         ENDFOR;
065000090721
065100090721         // -Chiusura del work-file?
065200140305         close WFCLS10F;
065300090721
065400090721       ENDSR;
065500090721
065600090721       //--------------------------------------------------------------
065700090721       //?Reperimento dati della filiale                               ?
065800090721       //--------------------------------------------------------------
065900090721       BEGSR  sr_NewFil;
066000090721
066100090721         // -Reperimento dati anagrafici?
066200091005         clear  Og150;
066300091008         clear  ORGdes;
066400091008         clear  ORGcar;
066500091005         clear  ds05;
066600090721         chain  (WLSfil)  AZORG;
066700090721         if  %found(AZORG01L);
066800091008           WLSfilD = ORGdes;           // - Decodifica filiale
066900091008           WLScar  = ORGcar;           // - Codice area
067000090721           Og150 = ORGdf0;
067100091008           WLStis  = §OGtis;           // - Tipo impianto smistacolli
067200091008           WLSpck  = §OGpck;           // - Flag picking (da Og150)
067300091005           TBLkut = c_kut;
067400091005           TBLcod = '05';
067500091005           TBLkey = %editc(ORGcar : 'X');
067600091005           chain  %kds(k03tabel00)  TABEL;
067700091005           if  %found(TABEL00F)   and   TBLflg = *blank;
067800091005             ds05 = TBLuni;
067900091005           endif;
068000091008           WLScard = §05des;           // - Decodifica area
068100090721         endif;
068200090721
068300090721         // -Reperimento terminal di partenza (x O.R.M.)?
068400090721         clear  FNLV55ds;
068500090721         if  ORGfag = 'A'   or   ORGfag = 'F';
068600090721           D55tpt = 'P';
068700090721           D55lin = WLSfil;
068800090721           D55drf = wDate;
068900090721           fnlv55r ( FNLV55ds );
069000090721           if  D55err <> *blank;
069100090721             D55tfp = D55lin;
069200090721           endif;
069300090721         endif;
069400091008         WLStfp = D55tfp;              // - Terminal partenza
069500091007         chain  (D55tfp)  AZORG;
069600091007         if  %found(AZORG01L);
069700091008           WLStfpD = ORGdes;           // - Decodifica terminal partenza
069800091007         endif;
069900091008
070000090721
070100090721       ENDSR;
070200090721
070300090721       //--------------------------------------------------------------
070400090721       //?Calcolo n° di O.R.M. nel periodo per il cliente              ?
070500090721       //--------------------------------------------------------------
070600090721       BEGSR  sr_Calc_ORM;
070700090721
070800090721         //Ciclo di elaborazione O.R.M. (FNORM)?
070900090721         setll  (WLSfil)  FNORM000;
071000090721         reade  (WLSfil)  FNORM000;
071100090721
071200090721         DoW  NOT %eof(FNORM13L);
071300090721
071400090721           // -?Incremento numero di O.R.M.?
071500090721           if  ORMdao >= SB21dti   and
071600090721               ORMdao <= SB21dtf;
071700090721             WLSorm += 1;
071800090721           endif;
071900090721
072000090721           // -?Lettura record successivo di FNORM?
072100090721           reade  (WLSfil)  FNORM000;
072200090721
072300090721         EndDo;
072400090721
072500090721       ENDSR;
072600090612
072700090612      /end-free
