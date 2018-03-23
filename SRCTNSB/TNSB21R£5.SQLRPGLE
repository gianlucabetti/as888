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
013200170112
013300170112       // -?Stringa SQL da eseguire?
013400170112     d wSQL            s           2048    Varying        inz
013500170112     d tot             s              9  0
013600090612
013700090612       //--------------------------------------------------------------
013800090612       //?Definizione variabili globali.                               ?
013900090612       //--------------------------------------------------------------
014000090612
014100090612       // - Flag booleani
014200090714     d $EoF            s               n   inz
014300090714     d $Ok             s               n   inz
014400090720     d $DDTsi          s               n   inz
014500090715
014600090715       // - Indici di schiera
014700090715     d xx              s              3  0 inz
014800090721     d yy              s              3  0 inz
014900140305
015000140305     d totRotolo       s              9  0 inz
015100140520     d WLSnca164       s              9  0 inz
015200090612
015300090715       // - Campi per memorizzazione codici filiale
015400090715     d wCFlnp          s                   like(ORGfil)  inz
015500090721     d w_WLSfil        s                   like(WLSfil)  inz
015600140305     d ktrd            s                   like(ar5trd)  inz('GEN')
015700140305     d savtfp          s                   like(WLStfp)  inz
015800140305     d newtfp          s                   like(WLStfp)  inz
015900140305     d newfil          s                   like(WLSfil)  inz
016000140520     d savlnp          s                   like(d55lin)  inz
016100140520     d savlna          s                   like(d55lin)  inz
016200140520     d sav£6P          s                   like(d55lin)  inz
016300140520     d sav£6A          s                   like(d55lin)  inz
016400090612
016500090612       //--------------------------------------------------------------
016600090612       //?Definizione procedure usate.                                 ?
016700090612       //--------------------------------------------------------------
016800090612
016900090612       // - Reperimento dati utente
017000090612      /copy gaitrasrc/srcProtoPR,TIBS34R
017100090714
017200090714       // - Reperimento terminal filiale
017300090714      /copy gaitrasrc/srcProtoPR,FNLV55R
017400090612
017500090612       // - Esecuzione comando di sistema
017600090612      /copy gaitrasrc/srcProtoPR,QCMDEXC
017700090612
017800170112       // -?Parametri API QCAPCMD (Process Commands)?
017900170112     d Qcmd            s           2048    inz  varying
018000170112       // -?Parametri gestione errori API.?
018100170112      /copy qsysinc/qRpgleSrc,QUSEC
018200170112      /copy qSysInc/qRpgleSrc,QCAPCMD
018300170112       // -?API QCAPCMD (Process Commands)?
018400170112      /copy gaitrasrc/srcProtoPR,QCAPCMD
018500170112
018600090612       //--------------------------------------------------------------
018700090612       //?Definizione key-list.                                        ?
018800090612       //--------------------------------------------------------------
018900090612
019000090715     d k03titas38    e ds                  extname(TITAS38C : *key)
019100090715     d                                     inz  prefix(k_)
019200090612
019300090715     d k03tabel00    e ds                  extname(TABEL00F : *key)
019400090715     d                                     inz
019500090612
019600090612       //--------------------------------------------------------------
019700090612       //?M A I N - L I N E.                                           ?
019800090612       //--------------------------------------------------------------
019900090612
020000090612     c     *Entry        plist
020100090612     c                   parm                    KPJBA
020200090612
020300090612      /free
020400090612
020500090715       //Operazioni iniziali?
020600090612       exsr  sr_RoutInz;
020700090612
020800090715
020900090715       //Ciclo di lettura file spedizioni (TITAS)?
021000090715       clear  k03titas38;
021100090715       k_TASaas = wDataAA;
021200090715       k_TASmgs = wDataMG;
021300090715       k_TASksc = *loval;
021400090715       setll  %kds(k03titas38)  TITAS38C;
021500090714       read   TITAS38C;
021600090612
021700090714       DoW  NOT  %eof(TITAS38C);
021800090612
021900090714         // -?Selezione singolo record di TITAS?
022000090612         exsr  sr_Selez;
022100090714
022200090714         select;
022300090714
022400090714           // -?Fine ciclo di lettura?
022500090714           when  $EoF = *on;
022600090714             leave;
022700090714
022800090721           // -?Aggiornamento dati del work-file?
022900090714           when  $OK;
023000090721             exsr  sr_UpdRec;
023100090714
023200090714         endsl;
023300090612
023400090714         // -?Lettura record successivo di TITAS?
023500090714         read  TITAS38C;
023600090612
023700090714       EndDo;
023800140305
023900140305         // -Registrazione dati elaborati nel work-file?
024000140305         exsr  sr_WrtWFCLS;
024100140305
024200140305       //rileggo i record e imposto i totali
024300140305       exsr  sr_ScriviTot;
024400090715
024500090715       //Operazioni finali?
024600090612       exsr  sr_RoutEnd;
024700090715
024800090612
024900090612       //--------------------------------------------------------------
025000090612       //?Operazioni iniziali.                                         ?
025100090612       //--------------------------------------------------------------
025200090612       BEGSR  sr_RoutInz;
025300090612
025400090612         *inLR = *on;
025500090715
025600090715         TNSB21ds = kpjbu;
025700090612
025800090715         // -Reperimento dati job?
025900090612         exsr  sr_DatiJob;
026000090715
026100090715         // -Intabellamento cod. bolla ("3A") per sped. di recupero?
026200090715         clear  xx;
026300090715         clear  k03tabel00;
026400091005         TBLkut = c_kut;
026500090715         TBLcod = '3A';
026600090715         setll  %kds(k03tabel00)  TABEL;
026700090715         reade  %kds(k03tabel00 : 2)  TABEL;
026800090715         DoW  NOT %eof(TABEL00F);
026900090715           if  TBLflg = *blank;
027000090715             ds3A = TBLuni;
027100100409        //     if  §3Arbl = 'R'   or   §3Arbl = 'C';
027200100409             if  §3Arbl = 'R'   ;
027300090715               xx += 1;
027400090715               $CBOrec(xx) = TBLkey;
027500090715             endif;
027600090715           endif;
027700090715           reade  %kds(k03tabel00 : 2)  TABEL;
027800090715         EndDo;
027900090715
028000090715         // -Intabellamento cod. trattamento merce ("1B") per "Disk C"?
028100090715         clear  xx;
028200090715         clear  k03tabel00;
028300091005         TBLkut = c_kut;
028400090715         TBLcod = '1B';
028500090715         setll  %kds(k03tabel00)  TABEL;
028600090715         reade  %kds(k03tabel00 : 2)  TABEL;
028700090715         DoW  NOT %eof(TABEL00F);
028800090715           if  TBLflg = *blank;
028900090715             ds1B = TBLuni;
029000090721             //if  §1Bf12 = 'S'   or   §1Bf12 = 'E'   or   §1Bf12 = 'B';
029100090721             if  §1Bdkc = 'B'   or   §1Bdkc = 'C';
029200090715               xx += 1;
029300090721               $CTMdkC(xx)  = TBLkey;
029400090721               $CTMdkCx(xx) = §1Bdkc;
029500090715             endif;
029600090715           endif;
029700090715           reade  %kds(k03tabel00 : 2)  TABEL;
029800090715         EndDo;
029900090806
030000090806         // -Intabellamento filiali con network estero?
030100090806         clear  xx;
030200091005         clear  yy;
030300090806         setll (*loval) AZORG;
030400090806         read  AZORG;
030500090806         DoW  Not %eof(AZORG01L);
030600090806           Og143 = ORGde3;
030700090806           if  §ogNTW = 'DPD'   or          // DPD
030800090806               §ogNTW = 'EEX'   or          // EuroExpress
030900090806               §ogNTW = 'EUP'   or          // Europolitan (Estero+SDI)
031000090806               §ogNTW = 'FED';              // FedEx
031100090806             xx += 1;
031200090806             $FilNtwE(xx) = ORGfil;
031300091005             if  §ogNTW = 'DPD';
031400091005               yy += 1;
031500091005               $FilDPD(yy) = ORGfil;
031600091005             endif;
031700090806           endif;
031800090806           read  AZORG;
031900090806         EndDo;
032000090715
032100090715         // -Impostazione data iniziale?
032200090715         ds_DataISO = %editc( SB21dti : 'X' );
032300090715
032400090715         // -Reperimento Data & Ora correnti?
032500090715         ds_Time14 = %subst( %char( %dec( %timestamp() ) )
032600090715                             : 1 : 14 );
032700090612
032800090612       ENDSR;
032900090612
033000090612       //--------------------------------------------------------------
033100090714       //?Reperimento Dati del job (Utente/Operativi).                 ?
033200090612       //--------------------------------------------------------------
033300090612       BEGSR  sr_DatiJob;
033400090612
033500090612         in(E) §AzUte;
033600090612         if NOT %error;
033700090612           in(E) §DatiUte;
033800090612         endif;
033900090612         if %error or RSut = *blanks;
034000090612           clear TIBS34ds;
034100090612           tibs34r(tibs34ds);
034200090612           in §AzUte;
034300090612           in §DatiUte;
034400090612         endif;
034500090612
034600090612       ENDSR;
034700090612
034800090612       //--------------------------------------------------------------
034900090714       //?Selezione record da TITAS                                    ?
035000090612       //--------------------------------------------------------------
035100090612       BEGSR  sr_Selez;
035200090612
035300090612         $Ok = *off;
035400090714
035500090714         select;
035600090714
035700090715           // -Data spedizione oltre il limite?
035800090715           when  (TASaas * 10000) + TASmgs > SB21dtf;
035900090714             $EoF = *on;
036000090714             leavesr;
036100090715
036200090715           // -Tipo bolla?
036300100409           // when  TAStbl <> 'F1'   and   TAStbl <> 'A2';
036400100409           //  leavesr;
036500090715
036600090715           // -Spedizione senza segnacolli?
036700100409           // when  TASncd = *zero and TASFNS = *blank;
036800100409           //  leavesr;
036900090715
037000100409           // -Escluse solo Spedizioni di recupero?
037100090715           when  %lookup( TAScbo : $CBOrec ) > *zero;
037200090715             leavesr;
037300090612
037400090715         endsl;
037500090715
037600090715         $Ok = *on;
037700090612
037800090612       ENDSR;
037900090612
038000090612       //--------------------------------------------------------------
038100090721       //?Aggiornamento dati del work-file (con dati da TITAS)         ?
038200090612       //--------------------------------------------------------------
038300090721       BEGSR  sr_UpdRec;
038400140305
038500140305         // -Lettura fiar5 ?
038600140305         chain (tasaas:taslnp:tasnrs:tasnsp:ktrd) fiar531c  ;
038700140305         if %found(fiar531c) ;
038800140305         dar5gen=ar5uni  ;
038900140305         else  ;
039000140305         clear dar5gen  ;
039100140305         endif  ;
039200090721
039300090721         // -Reperimento capofila £6 della LNP?
039400090721         //   (serve comunque per gli O.R.M.)
039500140520         if  TASlnp <> SAVlnp;
039600090721           reset  FNLV55ds;
039700090721           //D55tpt = '6'                   (già così)
039800090721           D55lin = TASlnp;
039900090721           D55drf = wDate;
040000090721           fnlv55r ( FNLV55ds );
040100090721           if  D55err <> *blank;
040200090721             D55tfa = D55lin;
040300090721           endif;
040400140520          savlnp=d55lin  ;
040500140520          sav£6P=d55tfa  ;
040600090721         endif;
040700140520
040800140520         wCFlnp = sav£6P;
040900090715
041000090721         // -Impostazione capofila £6 per la filiale?
041100100412         // If  $DDTsi;
041200090721
041300100412         //  w_WLSfil = wCFlnp;               // LNP per "DDT SÌ"
041400090721
041500100412         //Else;
041600090721
041700100412           // -?Reperimento SEMPRE capofila £6 della LNA?
041800140520           if  TASlna <> SAVlna;
041900090721             reset  FNLV55ds;
042000090721             //D55tpt = '6'                 (già così)
042100090721             D55lin = TASlna;
042200090721             D55drf = wDate;
042300090721             fnlv55r ( FNLV55ds );
042400090721             if  D55err <> *blank;
042500090721               D55tfa = D55lin;
042600090721             endif;
042700140520             sav£6A= d55tfa  ;
042800140520             savlna= d55lin  ;
042900100412            endif;
043000140520           w_WLSfil = sav£6A;               // LNA altrimenti
043100090721
043200100412         //EndIf;
043300090715
043400100412         //conteggio colli per linea di arrivo:?
043500090721
043600090721         //Reperimento posizione elemento dati per la filiale?
043700090721         xx = %lookup(w_WLSfil : $FilWF);
043800090721         if  xx = *zero;
043900090721           xx = %lookup(*zero : $FilWF);
044000090721           if  xx = *zero;                  // Esaurita schiera:
044100090721             xx = *hival;                   // => GENERA ERRORE!
044200090721           endif;
044300090721           $FilWF(xx) = w_WLSfil;
044400090721           clear  ds_WFCLS;
044500090722         else;
044600090722           ds_WFCLS = $RecWF(xx);
044700090722         endif;
044800100412
044900100412         // -Numero LdV (scartando network esteri)?
045000140305         WLSfil = $FilWF(xx);
045100140305           if    %lookup(WLSfil : $FilNtwE) <= *zero;
045200140331           //  con PDA
045300140331             if §AR5PDACO='  '  ;
045400140331             WLSlvaspda += 1;                   // in arrivoPDA
045500140331             else  ;
045600140331             WLSlvanpda += 1;                   // in arrivo noPDA
045700140331             endif  ;
045800140331
045900140305             WLSnca += TASncl;                 // in arrivo
046000140305
046100140305             // verifico se ha packing list o pdf
046200140305             if §ar5alx='S' or §ar5als='S'  ;
046300150331             WLSlpkl += 1;
046400140305             endif  ;
046500140305           endif   ;
046600140305             $RecWF(xx) = ds_WFCLS;
046700140520
046800140520         // -CHIODO per la filiale 164 : ?
046900140520         // -  sommo anche i colli dei secondi livello ?
047000140520         // -  per il totale delle etichette ARANCIONI ?
047100140520         clear  FNLV55ds;
047200140520           D55tpt = 'A';
047300140520           D55lin = w_WLSfil ;
047400140520           D55lnp = taslnp   ;
047500140520           D55drf = wDate;
047600140520           fnlv55r ( FNLV55ds );
047700140520           if  D55err <> *blank;
047800140520             D55tfa = D55lin;
047900140520           endif;
048000140520
048100140520          if d55tfa=164  ;
048200140520             WLSnca164 += TASncl;                 // in arrivo
048300140520          endif  ;
048400090612
048500090721         //Aggiornamento dati per la filiale?
048600100412         //WLSfil = $FilWF(xx);
048700090721
048800090806         // -Numero LdV (scartando network esteri)?
048900100412         //select;
049000100412         //  when  %lookup(WLSfil : $FilNtwE) > *zero;
049100100412         //  when  $DDTsi;
049200100412         //    WLSlvp += 1;                   // in partenza ("DDT SÌ")
049300100412         //  other;
049400100412         //    WLSlva += 1;                   // in arrivo
049500100412         //    WLSnca += TASncl;              // in arrivo
049600100412         //endsl;
049700090721
049800100412         //$RecWF(xx) = ds_WFCLS;
049900090715
050000090721         // -Reperimento posizione elemento dati per la LNP?
050100090722         If  wCFlnp <> $FilWF(xx);
050200090722           xx = %lookup(wCFlnp : $FilWF);
050300090722           if  xx = *zero;
050400090722             xx = %lookup(*zero : $FilWF);
050500090722             if  xx = *zero;                // Esaurita schiera:
050600090722               xx = *hival;                 // => GENERA ERRORE!
050700090722             endif;
050800090722             $FilWF(xx) = wCFlnp;
050900090722             clear  ds_WFCLS;
051000090722           else;
051100090722             ds_WFCLS = $RecWF(xx);
051200090722           endif;
051300090722         EndIf;
051400100412
051500100412         // -Verifica se spedizione "DDT SÌ"?
051600100412         $DDTsi = (TASll1 = 'C' or TASll1 = 'S' or TASll1 = *blank);
051700100412
051800100412         // -Numero LdV DDT SI escluso export?
051900100412           if    $DDTsi  ;
052000100412             WLSlvp += 1;                   // in partenza ("DDT SÌ")
052100100412           endif   ;
052200090721
052300090721         // -Dettaglio spedizioni e colli (x LNP)?
052400090721         yy = %lookup( TASctm : $CTMdkC );
052500090715         select;
052600090715           // -?Disk-C?
052700090721           when  yy > *zero;
052800090715             WLSecs += 1;
052900090715             WLSecc += TASncl;
053000090721             if  $CTMdkCx(yy) = 'B';
053100091005               if  %lookup(TASlna : $FilDPD) = *zero;
053200091005                 WLSecBs += 1;
053300091005                 WLSecBc += TASncl;
053400091005               else;
053500091005                 WLSecBsD += 1;
053600091005                 WLSecBcD += TASncl;
053700091005               endif;
053800090721             endif;
053900090715           // -?Disk-B?
054000090715           when  TASnrs > *zero;
054100140305             WLSebb += TASncl;
054200140305             // di cui easy web/sped
054300140305
054400140305             tblkey=%editc(tasccm:'X')  ;
054500140305           chain  (1:'3C': tblkey)  TABEL;
054600140305           if %found(tabel00f) ;
054700140305           ds3c=tbluni  ;
054800140305           if §3ccba='ESWEB' or §3ccba='ESYSP'  ;
054900140305             WLSEBBWS  += TASncl;
055000140305           endif;
055100140305           endif;
055200140305
055300090715           // -?né Disk-C né Disk-B?
055400090715           other;
055500090806             WLSeNbcs += 1;
055600090806             WLSeNbcc += TASncl;
055700090715         endsl;
055800090721
055900090721         $RecWF(xx) = ds_WFCLS;
056000090612
056100090612       ENDSR;
056200140305
056300140305       //--------------------------------------------------------------
056400140305       //?Rileggo per scrivere i totali                                ?
056500140305       //--------------------------------------------------------------
056600140305       BEGSR  sr_ScriviTot;
056700140305       clear totrotolo  ;
056800140305       clear savtfp   ;
056900140305
057000140305       setll *loval   wfcls11l  ;
057100140305       read  wfcls11l  ;
057200140305       dow  not %eof(wfcls11l)  ;
057300140305
057400150331       // totale fogli A4 LDV
057500150331       // DDT si
057600150331       WLSLDVT = WLSLVP ;
057700150331       // Pack.list di cui raddoppio le pagine MAGGIORATO DEL 10%
057800150331       //           ( la media pack.list è infatti 1,4)
057900150331       if wlslpkl>0 ;
058000150331       WLSLDVT +=  ((WLSLPKL*2)*110/100) ;
058100150331       endif  ;
058200140305
058300140305       //  con   PDA
058400140305       // +  bolle arrivo si pda + 10% /4
058500140305       if WLSLVASPDA> 0  ;
058600140305       WLSLDVT +=  ((wlslvaspda*110/100)/4) ;
058700140305       endif  ;
058800140305
058900140305       // senza PDA
059000140305       // +  bolle arrivo si pda + 10% /2
059100140305       if WLSLVAnPDA> 0  ;
059200140305       WLSLDVT +=  ((wlslvanpda*110/100)/2) ;
059300140305       endif  ;
059400140305
059500140305       // etichette ARANCIONI solo se filiale 2=liv con picking
059600140305       //  colli arrivati maggiorati del 10%
059700140305       if WLSPCK = 'S' and wlstfp<>wlsfil  ;
059800140520         // se si tratta della filiale di 164 sonno anche i colli dei secondi liv in arrivo
059900140520         if wlsfil=164  ;
060000140520           WLSETAR  = (WLSnca164*110/100)  ;
060100140520         else  ;
060200140520           WLSETAR  = (WLSNCA*110/100)  ;
060300140520         endif  ;
060400140520       endif  ;
060500140305
060600140305       // Etichette IN PIEGA
060700140305       WLSETPG  = WLSENBCC  ;
060800170112
060900170112       // Calcolo le etichette WIFI utilizzate
061000170112       exsr Eti_WF   ;
061100140305
061200140305       update wfcls011  ;
061300140305
061400140305       // Etichette in ROTOLO  ;
061500140305       //  sommo i disK C di tutto il terminal e poi scrivo solo sul terminal
061600140305       if savtfp<>wlstfp and savtfp>0  ;
061700140305       newtfp=wlstfp  ;
061800140305       newfil=wlsfil  ;
061900140305       chain (savtfp:savtfp)   wfcls11l  ;
062000140305       if %found(wfcls11l)  ;
062100140305       WLSETROT = totRotolo  ;
062200140305       update wfcls011  ;
062300140305       chain (newtfp:newfil)   wfcls11l  ;
062400140305       endif  ;
062500140305
062600140305       clear totrotolo  ;
062700140305       endif  ;
062800140305
062900140305       savtfp=wlstfp  ;
063000140305       totROTOLO += WLSECC  ;
063100140305
063200140305       read  wfcls11l  ;
063300140305       enddo  ;
063400140305
063500140305       ENDSR;
063600090612
063700090612       //--------------------------------------------------------------
063800090714       //?Operazioni finali.                                           ?
063900090612       //--------------------------------------------------------------
064000090612       BEGSR  sr_RoutEnd;
064100090721
064200090612
064300090721         // -Uscita?
064400090612         return;
064500090612
064600090612       ENDSR;
064700170112       //--------------------------------------------------------------
064800170112       //?calcolo etichette wifi utilizzate                            ?
064900170112       //--------------------------------------------------------------
065000170112       BEGSR  Eti_WF;
065100170112
065200170112         clear wsql  ;
065300170112         if  %subst(knsif:7:1)='P' ;
065400170113         wsql='select sum(msspwf)  from filtragrpf/'  ;
065500170112         else  ;
065600170113         wsql='select sum(msspwf)  from filtragru/'  ;
065700170112         endif  ;
065800170112
065900170112           wSQL += 'fnmss00f where mssfil=' + %editc(wlsfil:'X') +
066000170112           ' and mssdfv between ' + %editc(wlsdti :'X') + ' and '+
066100170113             %editc(wlsdtf :'X') + ' and mssnpg=5' ;
066200170112
066300170112         exec sql   prepare S1   from :wSQL;
066400170112         exec sql   declare C1   cursor for S1;
066500170112
066600170112         // -?Apertura del cursore?
066700170112         exec sql   open C1;
066800170112
066900170112         // -?Ciclo di lettura wf?
067000170112         exec sql   fetch next   from C1   into :tot    ;
067100170112
067200170113         if   SQLcode =  0  ;
067300170112
067400170112           WLSETWF5= tot  ;
067500170113
067600170112           endif  ;
067700170112
067800170112         exec sql   close C1;
067900170113
068000170113         clear wsql  ;
068100170112         if  %subst(knsif:7:1)='P' ;
068200170113         wsql='select sum(msspwf)  from filtragrpf/'  ;
068300170112         else  ;
068400170113         wsql='select sum(msspwf)  from filtragru/'  ;
068500170112         endif  ;
068600170112
068700170112           wSQL += 'fnmss00f where mssfil=' + %editc(wlsfil:'X') +
068800170112           ' and mssdfv between ' + %editc(wlsdti :'X') + ' and '+
068900170113             %editc(wlsdtf :'X') + ' and mssnpg=2' ;
069000170112
069100170112         exec sql   prepare S2   from :wSQL;
069200170112         exec sql   declare C2   cursor for S2;
069300170112
069400170112         // -?Apertura del cursore?
069500170112         exec sql   open C2;
069600170112
069700170112         // -?Ciclo di lettura wf?
069800170112         exec sql   fetch next   from C2   into :tot    ;
069900170112
070000170113         if   SQLcode =  0  ;
070100170112
070200170112           WLSETWF2= tot  ;
070300170113
070400170112           endif  ;
070500170112
070600170112         exec sql   close C2;
070700170112
070800170112       ENDSR;
070900090721       //--------------------------------------------------------------
071000140305       //?Registrazione dati nel file WFCLS10F                         ?
071100090721       //--------------------------------------------------------------
071200090721       BEGSR  sr_WrtWFCLS;
071300090721
071400090806         // -Pulizia / Apertura del work-file?
071500090806         Qcmd = 'CLRPFM file(';
071600090806         if  %subst( knsif : 7 : 1 ) = 'P';
071700090806            Qcmd = %trimr(Qcmd) + 'GAITRAAZP';
071800090806         else;
071900090806            Qcmd = %trimr(Qcmd) + 'GAITRAAZM';
072000090806         endif;
072100140305         Qcmd = %trimr(Qcmd) + '/WFCLS10F)';
072200090806         ExecuteCommand ( Qcmd : %size(Qcmd) );
072300090806
072400140305         open  WFCLS10F;
072500140305         open  WFCLS11l;
072600090721
072700090721         // -Registrazione record nel work-file?
072800090721         FOR  xx = 1  TO  %elem($FilWF);
072900090721
073000090721           if  $FilWF(xx) = *zero;
073100090721             leave;
073200090721           endif;
073300090721
073400090721           clear  WFCLS000;
073500090721
073600090721           // -?Reperimento dati relativi alle spedizioni (già calc.)?
073700091008           //  ?ed impostazione campi del WF con tali dati (già calc.)?
073800090721           ds_WFCLS = $RecWF(xx);
073900090721
074000090721           // -?Impostazione campi chiave se nuovo record?
074100090721           WLSute  = knmus;            // - Utente
074200090721           WLSdta  = wDate;            // - Data di lancio
074300090721           WLSdti  = SB21dti;          // - Data inizio periodo
074400090721           WLSdtf  = SB21dtf;          // - Data fine periodo
074500090721           WLSfil  = $FilWF(xx);       // - Filiale
074600090721           exsr  sr_NewFil;
074700090721
074800090721           // -?Conteggio numero O.R.M. per la filiale nel periodo?
074900090806           //  ?(NON per network esteri)?
075000090806           if  %lookup(WLSfil : $FilNtwE) = *zero;
075100090806             exsr  sr_Calc_ORM;
075200090806           endif;
075300090721
075400090721           // -?Scrittura/Aggiornamento del record?
075500090721           write  WFCLS000;
075600090721           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
075700090721
075800090721         ENDFOR;
075900090721
076000090721         // -Chiusura del work-file?
076100140305         close WFCLS10F;
076200090721
076300090721       ENDSR;
076400090721
076500090721       //--------------------------------------------------------------
076600090721       //?Reperimento dati della filiale                               ?
076700090721       //--------------------------------------------------------------
076800090721       BEGSR  sr_NewFil;
076900090721
077000090721         // -Reperimento dati anagrafici?
077100091005         clear  Og150;
077200091008         clear  ORGdes;
077300091008         clear  ORGcar;
077400091005         clear  ds05;
077500090721         chain  (WLSfil)  AZORG;
077600090721         if  %found(AZORG01L);
077700091008           WLSfilD = ORGdes;           // - Decodifica filiale
077800091008           WLScar  = ORGcar;           // - Codice area
077900090721           Og150 = ORGdf0;
078000091008           WLStis  = §OGtis;           // - Tipo impianto smistacolli
078100091008           WLSpck  = §OGpck;           // - Flag picking (da Og150)
078200091005           TBLkut = c_kut;
078300091005           TBLcod = '05';
078400091005           TBLkey = %editc(ORGcar : 'X');
078500091005           chain  %kds(k03tabel00)  TABEL;
078600091005           if  %found(TABEL00F)   and   TBLflg = *blank;
078700091005             ds05 = TBLuni;
078800091005           endif;
078900091008           WLScard = §05des;           // - Decodifica area
079000090721         endif;
079100090721
079200090721         // -Reperimento terminal di partenza (x O.R.M.)?
079300090721         clear  FNLV55ds;
079400090721         if  ORGfag = 'A'   or   ORGfag = 'F';
079500090721           D55tpt = 'P';
079600090721           D55lin = WLSfil;
079700090721           D55drf = wDate;
079800090721           fnlv55r ( FNLV55ds );
079900090721           if  D55err <> *blank;
080000090721             D55tfp = D55lin;
080100090721           endif;
080200090721         endif;
080300091008         WLStfp = D55tfp;              // - Terminal partenza
080400091007         chain  (D55tfp)  AZORG;
080500091007         if  %found(AZORG01L);
080600091008           WLStfpD = ORGdes;           // - Decodifica terminal partenza
080700091007         endif;
080800091008
080900090721
081000090721       ENDSR;
081100090721
081200090721       //--------------------------------------------------------------
081300090721       //?Calcolo n° di O.R.M. nel periodo per il cliente              ?
081400090721       //--------------------------------------------------------------
081500090721       BEGSR  sr_Calc_ORM;
081600090721
081700090721         //Ciclo di elaborazione O.R.M. (FNORM)?
081800090721         setll  (WLSfil)  FNORM000;
081900090721         reade  (WLSfil)  FNORM000;
082000090721
082100090721         DoW  NOT %eof(FNORM13L);
082200090721
082300090721           // -?Incremento numero di O.R.M.?
082400090721           if  ORMdao >= SB21dti   and
082500090721               ORMdao <= SB21dtf;
082600090721             WLSorm += 1;
082700090721           endif;
082800090721
082900090721           // -?Lettura record successivo di FNORM?
083000090721           reade  (WLSfil)  FNORM000;
083100090721
083200090721         EndDo;
083300090721
083400090721       ENDSR;
083500090612
083600090612      /end-free
