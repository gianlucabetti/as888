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
002700180205     fwfcls11l  uf   e           k disk    usropn
002800140305     f                                     rename(wfcls000:wfcls011)
002900180205     fwfcls10F  o    e             disk    usropn
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
005600180205       // - Filiali nel work-file WFCLS10F
005700090721     d $FilWF          s                   like(WLSfil)   dim(500)  inz
005800180205       // - Records del work-file WFCLS10F
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
010300180205     ***d ds_WFCLS      e ds                  inz  extname(WFCLS10F)
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
013600180202     d totpick         s              9  0
013700180202     d wtfatotpick     s              9  0
013800090612
013900090612       //--------------------------------------------------------------
014000090612       //?Definizione variabili globali.                               ?
014100090612       //--------------------------------------------------------------
014200090612
014300090612       // - Flag booleani
014400090714     d $EoF            s               n   inz
014500090714     d $Ok             s               n   inz
014600090720     d $DDTsi          s               n   inz
014700090715
014800090715       // - Indici di schiera
014900090715     d xx              s              3  0 inz
015000090721     d yy              s              3  0 inz
015100140305
015200140305     d totRotolo       s              9  0 inz
015300140520     d WLSnca164       s              9  0 inz
015400090612
015500090715       // - Campi per memorizzazione codici filiale
015600090715     d wCFlnp          s                   like(ORGfil)  inz
015700090721     d w_WLSfil        s                   like(WLSfil)  inz
015800140305     d ktrd            s                   like(ar5trd)  inz('GEN')
015900140305     d savtfp          s                   like(WLStfp)  inz
016000140305     d newtfp          s                   like(WLStfp)  inz
016100140305     d newfil          s                   like(WLSfil)  inz
016200140520     d savlnp          s                   like(d55lin)  inz
016300140520     d savlna          s                   like(d55lin)  inz
016400140520     d sav£6P          s                   like(d55lin)  inz
016500140520     d sav£6A          s                   like(d55lin)  inz
016600090612
016700090612       //--------------------------------------------------------------
016800090612       //?Definizione procedure usate.                                 ?
016900090612       //--------------------------------------------------------------
017000090612
017100090612       // - Reperimento dati utente
017200090612      /copy gaitrasrc/srcProtoPR,TIBS34R
017300090714
017400090714       // - Reperimento terminal filiale
017500090714      /copy gaitrasrc/srcProtoPR,FNLV55R
017600090612
017700090612       // - Esecuzione comando di sistema
017800090612      /copy gaitrasrc/srcProtoPR,QCMDEXC
017900090612
018000170112       // -?Parametri API QCAPCMD (Process Commands)?
018100170112     d Qcmd            s           2048    inz  varying
018200170112       // -?Parametri gestione errori API.?
018300170112      /copy qsysinc/qRpgleSrc,QUSEC
018400170112      /copy qSysInc/qRpgleSrc,QCAPCMD
018500170112       // -?API QCAPCMD (Process Commands)?
018600170112      /copy gaitrasrc/srcProtoPR,QCAPCMD
018700170112
018800090612       //--------------------------------------------------------------
018900090612       //?Definizione key-list.                                        ?
019000090612       //--------------------------------------------------------------
019100090612
019200090715     d k03titas38    e ds                  extname(TITAS38C : *key)
019300090715     d                                     inz  prefix(k_)
019400090612
019500090715     d k03tabel00    e ds                  extname(TABEL00F : *key)
019600090715     d                                     inz
019700090612
019800090612       //--------------------------------------------------------------
019900090612       //?M A I N - L I N E.                                           ?
020000090612       //--------------------------------------------------------------
020100090612
020200090612     c     *Entry        plist
020300090612     c                   parm                    KPJBA
020400090612
020500090612      /free
020600090612
020700090715       //Operazioni iniziali?
020800090612       exsr  sr_RoutInz;
020900090612
021000090715
021100090715       //Ciclo di lettura file spedizioni (TITAS)?
021200090715       clear  k03titas38;
021300090715       k_TASaas = wDataAA;
021400090715       k_TASmgs = wDataMG;
021500090715       k_TASksc = *loval;
021600090715       setll  %kds(k03titas38)  TITAS38C;
021700090714       read   TITAS38C;
021800090612
021900090714       DoW  NOT  %eof(TITAS38C);
022000090612
022100090714         // -?Selezione singolo record di TITAS?
022200090612         exsr  sr_Selez;
022300090714
022400090714         select;
022500090714
022600090714           // -?Fine ciclo di lettura?
022700090714           when  $EoF = *on;
022800090714             leave;
022900090714
023000090721           // -?Aggiornamento dati del work-file?
023100090714           when  $OK;
023200090721             exsr  sr_UpdRec;
023300090714
023400090714         endsl;
023500090612
023600090714         // -?Lettura record successivo di TITAS?
023700090714         read  TITAS38C;
023800090612
023900090714       EndDo;
024000140305
024100140305         // -Registrazione dati elaborati nel work-file?
024200140305         exsr  sr_WrtWFCLS;
024300140305
024400140305       //rileggo i record e imposto i totali
024500140305       exsr  sr_ScriviTot;
024600090715
024700090715       //Operazioni finali?
024800090612       exsr  sr_RoutEnd;
024900090715
025000090612
025100090612       //--------------------------------------------------------------
025200090612       //?Operazioni iniziali.                                         ?
025300090612       //--------------------------------------------------------------
025400090612       BEGSR  sr_RoutInz;
025500090612
025600090612         *inLR = *on;
025700090715
025800090715         TNSB21ds = kpjbu;
025900090612
026000090715         // -Reperimento dati job?
026100090612         exsr  sr_DatiJob;
026200090715
026300090715         // -Intabellamento cod. bolla ("3A") per sped. di recupero?
026400090715         clear  xx;
026500090715         clear  k03tabel00;
026600091005         TBLkut = c_kut;
026700090715         TBLcod = '3A';
026800090715         setll  %kds(k03tabel00)  TABEL;
026900090715         reade  %kds(k03tabel00 : 2)  TABEL;
027000090715         DoW  NOT %eof(TABEL00F);
027100090715           if  TBLflg = *blank;
027200090715             ds3A = TBLuni;
027300100409        //     if  §3Arbl = 'R'   or   §3Arbl = 'C';
027400100409             if  §3Arbl = 'R'   ;
027500090715               xx += 1;
027600090715               $CBOrec(xx) = TBLkey;
027700090715             endif;
027800090715           endif;
027900090715           reade  %kds(k03tabel00 : 2)  TABEL;
028000090715         EndDo;
028100090715
028200090715         // -Intabellamento cod. trattamento merce ("1B") per "Disk C"?
028300090715         clear  xx;
028400090715         clear  k03tabel00;
028500091005         TBLkut = c_kut;
028600090715         TBLcod = '1B';
028700090715         setll  %kds(k03tabel00)  TABEL;
028800090715         reade  %kds(k03tabel00 : 2)  TABEL;
028900090715         DoW  NOT %eof(TABEL00F);
029000090715           if  TBLflg = *blank;
029100090715             ds1B = TBLuni;
029200090721             //if  §1Bf12 = 'S'   or   §1Bf12 = 'E'   or   §1Bf12 = 'B';
029300090721             if  §1Bdkc = 'B'   or   §1Bdkc = 'C';
029400090715               xx += 1;
029500090721               $CTMdkC(xx)  = TBLkey;
029600090721               $CTMdkCx(xx) = §1Bdkc;
029700090715             endif;
029800090715           endif;
029900090715           reade  %kds(k03tabel00 : 2)  TABEL;
030000090715         EndDo;
030100090806
030200090806         // -Intabellamento filiali con network estero?
030300090806         clear  xx;
030400091005         clear  yy;
030500090806         setll (*loval) AZORG;
030600090806         read  AZORG;
030700090806         DoW  Not %eof(AZORG01L);
030800090806           Og143 = ORGde3;
030900090806           if  §ogNTW = 'DPD'   or          // DPD
031000090806               §ogNTW = 'EEX'   or          // EuroExpress
031100090806               §ogNTW = 'EUP'   or          // Europolitan (Estero+SDI)
031200090806               §ogNTW = 'FED';              // FedEx
031300090806             xx += 1;
031400090806             $FilNtwE(xx) = ORGfil;
031500091005             if  §ogNTW = 'DPD';
031600091005               yy += 1;
031700091005               $FilDPD(yy) = ORGfil;
031800091005             endif;
031900090806           endif;
032000090806           read  AZORG;
032100090806         EndDo;
032200090715
032300090715         // -Impostazione data iniziale?
032400090715         ds_DataISO = %editc( SB21dti : 'X' );
032500090715
032600090715         // -Reperimento Data & Ora correnti?
032700090715         ds_Time14 = %subst( %char( %dec( %timestamp() ) )
032800090715                             : 1 : 14 );
032900090612
033000090612       ENDSR;
033100090612
033200090612       //--------------------------------------------------------------
033300090714       //?Reperimento Dati del job (Utente/Operativi).                 ?
033400090612       //--------------------------------------------------------------
033500090612       BEGSR  sr_DatiJob;
033600090612
033700090612         in(E) §AzUte;
033800090612         if NOT %error;
033900090612           in(E) §DatiUte;
034000090612         endif;
034100090612         if %error or RSut = *blanks;
034200090612           clear TIBS34ds;
034300090612           tibs34r(tibs34ds);
034400090612           in §AzUte;
034500090612           in §DatiUte;
034600090612         endif;
034700090612
034800090612       ENDSR;
034900090612
035000090612       //--------------------------------------------------------------
035100090714       //?Selezione record da TITAS                                    ?
035200090612       //--------------------------------------------------------------
035300090612       BEGSR  sr_Selez;
035400090612
035500090612         $Ok = *off;
035600090714
035700090714         select;
035800090714
035900090715           // -Data spedizione oltre il limite?
036000090715           when  (TASaas * 10000) + TASmgs > SB21dtf;
036100090714             $EoF = *on;
036200090714             leavesr;
036300090715
036400090715           // -Tipo bolla?
036500100409           // when  TAStbl <> 'F1'   and   TAStbl <> 'A2';
036600100409           //  leavesr;
036700090715
036800090715           // -Spedizione senza segnacolli?
036900100409           // when  TASncd = *zero and TASFNS = *blank;
037000100409           //  leavesr;
037100090715
037200100409           // -Escluse solo Spedizioni di recupero?
037300090715           when  %lookup( TAScbo : $CBOrec ) > *zero;
037400090715             leavesr;
037500090612
037600090715         endsl;
037700090715
037800090715         $Ok = *on;
037900090612
038000090612       ENDSR;
038100090612
038200090612       //--------------------------------------------------------------
038300090721       //?Aggiornamento dati del work-file (con dati da TITAS)         ?
038400090612       //--------------------------------------------------------------
038500090721       BEGSR  sr_UpdRec;
038600140305
038700140305         // -Lettura fiar5 ?
038800140305         chain (tasaas:taslnp:tasnrs:tasnsp:ktrd) fiar531c  ;
038900140305         if %found(fiar531c) ;
039000140305         dar5gen=ar5uni  ;
039100140305         else  ;
039200140305         clear dar5gen  ;
039300140305         endif  ;
039400090721
039500090721         // -Reperimento capofila £6 della LNP?
039600090721         //   (serve comunque per gli O.R.M.)
039700140520         if  TASlnp <> SAVlnp;
039800090721           reset  FNLV55ds;
039900090721           //D55tpt = '6'                   (già così)
040000090721           D55lin = TASlnp;
040100090721           D55drf = wDate;
040200090721           fnlv55r ( FNLV55ds );
040300090721           if  D55err <> *blank;
040400090721             D55tfa = D55lin;
040500090721           endif;
040600140520          savlnp=d55lin  ;
040700140520          sav£6P=d55tfa  ;
040800090721         endif;
040900140520
041000140520         wCFlnp = sav£6P;
041100090715
041200090721         // -Impostazione capofila £6 per la filiale?
041300100412         // If  $DDTsi;
041400090721
041500100412         //  w_WLSfil = wCFlnp;               // LNP per "DDT SÌ"
041600090721
041700100412         //Else;
041800090721
041900100412           // -?Reperimento SEMPRE capofila £6 della LNA?
042000140520           if  TASlna <> SAVlna;
042100090721             reset  FNLV55ds;
042200090721             //D55tpt = '6'                 (già così)
042300090721             D55lin = TASlna;
042400090721             D55drf = wDate;
042500090721             fnlv55r ( FNLV55ds );
042600090721             if  D55err <> *blank;
042700090721               D55tfa = D55lin;
042800090721             endif;
042900140520             sav£6A= d55tfa  ;
043000140520             savlna= d55lin  ;
043100100412            endif;
043200140520           w_WLSfil = sav£6A;               // LNA altrimenti
043300090721
043400100412         //EndIf;
043500090715
043600100412         //conteggio colli per linea di arrivo:?
043700090721
043800090721         //Reperimento posizione elemento dati per la filiale?
043900090721         xx = %lookup(w_WLSfil : $FilWF);
044000090721         if  xx = *zero;
044100090721           xx = %lookup(*zero : $FilWF);
044200090721           if  xx = *zero;                  // Esaurita schiera:
044300090721             xx = *hival;                   // => GENERA ERRORE!
044400090721           endif;
044500090721           $FilWF(xx) = w_WLSfil;
044600090721           clear  ds_WFCLS;
044700090722         else;
044800090722           ds_WFCLS = $RecWF(xx);
044900090722         endif;
045000100412
045100100412         // -Numero LdV (scartando network esteri)?
045200140305         WLSfil = $FilWF(xx);
045300140305           if    %lookup(WLSfil : $FilNtwE) <= *zero;
045400140331           //  con PDA
045500140331             if §AR5PDACO='  '  ;
045600140331             WLSlvaspda += 1;                   // in arrivoPDA
045700140331             else  ;
045800140331             WLSlvanpda += 1;                   // in arrivo noPDA
045900140331             endif  ;
046000140331
046100140305             WLSnca += TASncl;                 // in arrivo
046200140305
046300140305             // verifico se ha packing list o pdf
046400140305             if §ar5alx='S' or §ar5als='S'  ;
046500150331             WLSlpkl += 1;
046600140305             endif  ;
046700140305           endif   ;
046800170619            // totalizzo colli in arrivo anche per la DPD
046900170619               if  %lookup(wlsfil : $FilDPD) > *zero;
047000170619             WLSnca += TASncl;                 // in arrivo
047100170619             endif  ;
047200170619
047300140305             $RecWF(xx) = ds_WFCLS;
047400140520
047500140520         // -CHIODO per la filiale 164 : ?
047600140520         // -  sommo anche i colli dei secondi livello ?
047700140520         // -  per il totale delle etichette ARANCIONI ?
047800140520         clear  FNLV55ds;
047900140520           D55tpt = 'A';
048000140520           D55lin = w_WLSfil ;
048100140520           D55lnp = taslnp   ;
048200140520           D55drf = wDate;
048300140520           fnlv55r ( FNLV55ds );
048400140520           if  D55err <> *blank;
048500140520             D55tfa = D55lin;
048600140520           endif;
048700140520
048800140520          if d55tfa=164  ;
048900140520             WLSnca164 += TASncl;                 // in arrivo
049000140520          endif  ;
049100090612
049200090721         //Aggiornamento dati per la filiale?
049300100412         //WLSfil = $FilWF(xx);
049400090721
049500090806         // -Numero LdV (scartando network esteri)?
049600100412         //select;
049700100412         //  when  %lookup(WLSfil : $FilNtwE) > *zero;
049800100412         //  when  $DDTsi;
049900100412         //    WLSlvp += 1;                   // in partenza ("DDT SÌ")
050000100412         //  other;
050100100412         //    WLSlva += 1;                   // in arrivo
050200100412         //    WLSnca += TASncl;              // in arrivo
050300100412         //endsl;
050400090721
050500100412         //$RecWF(xx) = ds_WFCLS;
050600090715
050700090721         // -Reperimento posizione elemento dati per la LNP?
050800090722         If  wCFlnp <> $FilWF(xx);
050900090722           xx = %lookup(wCFlnp : $FilWF);
051000090722           if  xx = *zero;
051100090722             xx = %lookup(*zero : $FilWF);
051200090722             if  xx = *zero;                // Esaurita schiera:
051300090722               xx = *hival;                 // => GENERA ERRORE!
051400090722             endif;
051500090722             $FilWF(xx) = wCFlnp;
051600090722             clear  ds_WFCLS;
051700090722           else;
051800090722             ds_WFCLS = $RecWF(xx);
051900090722           endif;
052000090722         EndIf;
052100100412
052200100412         // -Verifica se spedizione "DDT SÌ"?
052300100412         $DDTsi = (TASll1 = 'C' or TASll1 = 'S' or TASll1 = *blank);
052400100412
052500100412         // -Numero LdV DDT SI escluso export?
052600100412           if    $DDTsi  ;
052700100412             WLSlvp += 1;                   // in partenza ("DDT SÌ")
052800100412           endif   ;
052900090721
053000090721         // -Dettaglio spedizioni e colli (x LNP)?
053100090721         yy = %lookup( TASctm : $CTMdkC );
053200090715         select;
053300090715           // -?Disk-C?
053400090721           when  yy > *zero;
053500090715             WLSecs += 1;
053600090715             WLSecc += TASncl;
053700090721             if  $CTMdkCx(yy) = 'B';
053800091005               if  %lookup(TASlna : $FilDPD) = *zero;
053900091005                 WLSecBs += 1;
054000091005                 WLSecBc += TASncl;
054100091005               else;
054200091005                 WLSecBsD += 1;
054300091005                 WLSecBcD += TASncl;
054400091005               endif;
054500090721             endif;
054600090715           // -?Disk-B?
054700090715           when  TASnrs > *zero;
054800140305             WLSebb += TASncl;
054900140305             // di cui easy web/sped
055000140305
055100140305             tblkey=%editc(tasccm:'X')  ;
055200140305           chain  (1:'3C': tblkey)  TABEL;
055300140305           if %found(tabel00f) ;
055400140305           ds3c=tbluni  ;
055500140305           if §3ccba='ESWEB' or §3ccba='ESYSP'  ;
055600140305             WLSEBBWS  += TASncl;
055700140305           endif;
055800140305           endif;
055900140305
056000090715           // -?né Disk-C né Disk-B?
056100090715           other;
056200090806             WLSeNbcs += 1;
056300090806             WLSeNbcc += TASncl;
056400090715         endsl;
056500090721
056600090721         $RecWF(xx) = ds_WFCLS;
056700090612
056800090612       ENDSR;
056900140305
057000140305       //--------------------------------------------------------------
057100140305       //?Rileggo per scrivere i totali                                ?
057200140305       //--------------------------------------------------------------
057300140305       BEGSR  sr_ScriviTot;
057400140305       clear totrotolo  ;
057500140305       clear savtfp   ;
057600140305
057700180205       setll *loval   WFCLS11l  ;
057800180205       read  WFCLS11l  ;
057900180205       dow  not %eof(WFCLS11l)  ;
058000140305
058100150331       // totale fogli A4 LDV
058200150331       // DDT si
058300150331       WLSLDVT = WLSLVP ;
058400180202       clear WLSLDVPDA ;
058500150331       // Pack.list di cui raddoppio le pagine MAGGIORATO DEL 10%
058600150331       //           ( la media pack.list è infatti 1,4)
058700150331       if wlslpkl>0 ;
058800150331       WLSLDVT +=  ((WLSLPKL*2)*110/100) ;
058900150331       endif  ;
059000140305
059100140305       //  con   PDA
059200140305       // +  bolle arrivo si pda + 10% /4
059300140305       if WLSLVASPDA> 0  ;
059400140305       WLSLDVT +=  ((wlslvaspda*110/100)/4) ;
059500180202
059600180202       // a parte memorizzo anche i totali ldv per pda
059700180202       WLSLDVpda=  ((wlslvaspda*110/100)/4) ;
059800140305       endif  ;
059900140305
060000140305       // senza PDA
060100140305       // +  bolle arrivo si pda + 10% /2
060200140305       if WLSLVAnPDA> 0  ;
060300140305       WLSLDVT +=  ((wlslvanpda*110/100)/2) ;
060400140305       endif  ;
060500140305
060600140305       // etichette ARANCIONI solo se filiale 2=liv con picking
060700140305       //  colli arrivati maggiorati del 10%
060800140305       if WLSPCK = 'S' and wlstfp<>wlsfil  ;
060900140520         // se si tratta della filiale di 164 sonno anche i colli dei secondi liv in arrivo
061000140520         if wlsfil=164  ;
061100140520           WLSETAR  = (WLSnca164*110/100)  ;
061200140520         else  ;
061300140520           WLSETAR  = (WLSNCA*110/100)  ;
061400140520         endif  ;
061500140520       endif  ;
061600140305
061700140305       // Etichette IN PIEGA
061800140305       WLSETPG  = WLSENBCC  ;
061900170112
062000170112       // Calcolo le etichette WIFI utilizzate
062100170112       exsr Eti_WF   ;
062200140305
062300140305       update wfcls011  ;
062400140305
062500140305       // Etichette in ROTOLO  ;
062600140305       //  sommo i disK C di tutto il terminal e poi scrivo solo sul terminal
062700140305       if savtfp<>wlstfp and savtfp>0  ;
062800140305       newtfp=wlstfp  ;
062900140305       newfil=wlsfil  ;
063000180205       chain (savtfp:savtfp)   WFCLS11l  ;
063100180205       if %found(WFCLS11l)  ;
063200140305       WLSETROT = totRotolo  ;
063300180202
063400180202       // aggiungo le etichette bianche x picking - le wifi cat 2
063500180202       exsr eti_pick   ;
063600180202       WLSETROT += wtfatotpick;
063700180202       WLSETROT -= WLSETWF2   ;
063800180202       update wfcls011  ;
063900180202
064000180205       chain (newtfp:newfil)   WFCLS11l  ;
064100140305       endif  ;
064200140305
064300140305       clear totrotolo  ;
064400140305       endif  ;
064500140305
064600140305       savtfp=wlstfp  ;
064700140305       totROTOLO += WLSECC  ;
064800140305
064900180205       read  WFCLS11l  ;
065000140305       enddo  ;
065100140305
065200140305       ENDSR;
065300090612
065400090612       //--------------------------------------------------------------
065500090714       //?Operazioni finali.                                           ?
065600090612       //--------------------------------------------------------------
065700090612       BEGSR  sr_RoutEnd;
065800090721
065900090612
066000090721         // -Uscita?
066100090612         return;
066200090612
066300090612       ENDSR;
066400170112       //--------------------------------------------------------------
066500170112       //?calcolo etichette wifi utilizzate                            ?
066600170112       //--------------------------------------------------------------
066700170112       BEGSR  Eti_WF;
066800170112
066900170112         clear wsql  ;
067000170112         if  %subst(knsif:7:1)='P' ;
067100170113         wsql='select sum(msspwf)  from filtragrpf/'  ;
067200170112         else  ;
067300170113         wsql='select sum(msspwf)  from filtragru/'  ;
067400170112         endif  ;
067500170112
067600170112           wSQL += 'fnmss00f where mssfil=' + %editc(wlsfil:'X') +
067700170112           ' and mssdfv between ' + %editc(wlsdti :'X') + ' and '+
067800170113             %editc(wlsdtf :'X') + ' and mssnpg=5' ;
067900170112
068000170112         exec sql   prepare S1   from :wSQL;
068100170112         exec sql   declare C1   cursor for S1;
068200170112
068300170112         // -?Apertura del cursore?
068400170112         exec sql   open C1;
068500170112
068600170112         // -?Ciclo di lettura wf?
068700170112         exec sql   fetch next   from C1   into :tot    ;
068800170112
068900170113         if   SQLcode =  0  ;
069000170112
069100170112           WLSETWF5= tot  ;
069200170113
069300170112           endif  ;
069400170112
069500170112         exec sql   close C1;
069600170113
069700170113         clear wsql  ;
069800170112         if  %subst(knsif:7:1)='P' ;
069900170113         wsql='select sum(msspwf)  from filtragrpf/'  ;
070000170112         else  ;
070100170113         wsql='select sum(msspwf)  from filtragru/'  ;
070200170112         endif  ;
070300170112
070400170112           wSQL += 'fnmss00f where mssfil=' + %editc(wlsfil:'X') +
070500170112           ' and mssdfv between ' + %editc(wlsdti :'X') + ' and '+
070600170113             %editc(wlsdtf :'X') + ' and mssnpg=2' ;
070700170112
070800170112         exec sql   prepare S2   from :wSQL;
070900170112         exec sql   declare C2   cursor for S2;
071000170112
071100170112         // -?Apertura del cursore?
071200170112         exec sql   open C2;
071300170112
071400170112         // -?Ciclo di lettura wf?
071500170112         exec sql   fetch next   from C2   into :tot    ;
071600170112
071700170113         if   SQLcode =  0  ;
071800170112
071900170112           WLSETWF2= tot  ;
072000170113
072100170112           endif  ;
072200170112
072300170112         exec sql   close C2;
072400170112
072500170112       ENDSR;
072600180202       //--------------------------------------------------------------
072700180202       //?calcolo etichette bianche per picking dei terminal
072800180202       //--------------------------------------------------------------
072900180202       BEGSR  Eti_pick  ;
073000180202
073100180202         clear wsql  ;
073200180202         if  %subst(knsif:7:1)='P' ;
073300180202         wsql='select sum(msspnc)  from filtragrpf/'  ;
073400180202         else  ;
073500180202         wsql='select sum(msspnc)  from filtragru/'  ;
073600180202         endif  ;
073700180202
073800180202           wSQL += 'fnmss00f where mssfil=' + %editc(wlsfil:'X') +
073900180202           ' and mssdfv between ' + %editc(wlsdti :'X') + ' and '+
074000180202             %editc(wlsdtf :'X') + ' and mssnpg= 2  and mssapl=''I''';
074100180202
074200180202         exec sql   prepare S3   from :wSQL;
074300180202         exec sql   declare C3   cursor for S3;
074400180202
074500180202         // -?Apertura del cursore?
074600180202         exec sql   open C3;
074700180202
074800180202         // -?Ciclo di lettura wf?
074900180202         exec sql   fetch next   from C3   into :totpick ;
075000180202
075100180202         if   SQLcode =  0  ;
075200180202
075300180202           wtfatotpick= totpick ;
075400180202
075500180202           endif  ;
075600180202
075700180202         exec sql   close C3;
075800180202
075900180202
076000180202       ENDSR;
076100090721       //--------------------------------------------------------------
076200180205       //?Registrazione dati nel file WFCLS10F                         ?
076300090721       //--------------------------------------------------------------
076400090721       BEGSR  sr_WrtWFCLS;
076500090721
076600090806         // -Pulizia / Apertura del work-file?
076700090806         Qcmd = 'CLRPFM file(';
076800090806         if  %subst( knsif : 7 : 1 ) = 'P';
076900090806            Qcmd = %trimr(Qcmd) + 'GAITRAAZP';
077000090806         else;
077100090806            Qcmd = %trimr(Qcmd) + 'GAITRAAZM';
077200090806         endif;
077300180205         Qcmd = %trimr(Qcmd) + '/WFCLS10F)';
077400090806         ExecuteCommand ( Qcmd : %size(Qcmd) );
077500090806
077600180205         open  WFCLS10F;
077700180205         open  WFCLS11l;
077800090721
077900090721         // -Registrazione record nel work-file?
078000090721         FOR  xx = 1  TO  %elem($FilWF);
078100090721
078200090721           if  $FilWF(xx) = *zero;
078300090721             leave;
078400090721           endif;
078500090721
078600090721           clear  WFCLS000;
078700090721
078800090721           // -?Reperimento dati relativi alle spedizioni (già calc.)?
078900091008           //  ?ed impostazione campi del WF con tali dati (già calc.)?
079000090721           ds_WFCLS = $RecWF(xx);
079100090721
079200090721           // -?Impostazione campi chiave se nuovo record?
079300090721           WLSute  = knmus;            // - Utente
079400090721           WLSdta  = wDate;            // - Data di lancio
079500090721           WLSdti  = SB21dti;          // - Data inizio periodo
079600090721           WLSdtf  = SB21dtf;          // - Data fine periodo
079700090721           WLSfil  = $FilWF(xx);       // - Filiale
079800090721           exsr  sr_NewFil;
079900090721
080000090721           // -?Conteggio numero O.R.M. per la filiale nel periodo?
080100090806           //  ?(NON per network esteri)?
080200090806           if  %lookup(WLSfil : $FilNtwE) = *zero;
080300090806             exsr  sr_Calc_ORM;
080400090806           endif;
080500090721
080600090721           // -?Scrittura/Aggiornamento del record?
080700090721           write  WFCLS000;
080800090721           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
080900090721
081000090721         ENDFOR;
081100090721
081200090721         // -Chiusura del work-file?
081300180205         close WFCLS10F;
081400090721
081500090721       ENDSR;
081600090721
081700090721       //--------------------------------------------------------------
081800090721       //?Reperimento dati della filiale                               ?
081900090721       //--------------------------------------------------------------
082000090721       BEGSR  sr_NewFil;
082100090721
082200090721         // -Reperimento dati anagrafici?
082300091005         clear  Og150;
082400091008         clear  ORGdes;
082500091008         clear  ORGcar;
082600091005         clear  ds05;
082700090721         chain  (WLSfil)  AZORG;
082800090721         if  %found(AZORG01L);
082900091008           WLSfilD = ORGdes;           // - Decodifica filiale
083000091008           WLScar  = ORGcar;           // - Codice area
083100090721           Og150 = ORGdf0;
083200091008           WLStis  = §OGtis;           // - Tipo impianto smistacolli
083300091008           WLSpck  = §OGpck;           // - Flag picking (da Og150)
083400091005           TBLkut = c_kut;
083500091005           TBLcod = '05';
083600091005           TBLkey = %editc(ORGcar : 'X');
083700091005           chain  %kds(k03tabel00)  TABEL;
083800091005           if  %found(TABEL00F)   and   TBLflg = *blank;
083900091005             ds05 = TBLuni;
084000091005           endif;
084100091008           WLScard = §05des;           // - Decodifica area
084200090721         endif;
084300090721
084400090721         // -Reperimento terminal di partenza (x O.R.M.)?
084500090721         clear  FNLV55ds;
084600090721         if  ORGfag = 'A'   or   ORGfag = 'F';
084700090721           D55tpt = 'P';
084800090721           D55lin = WLSfil;
084900090721           D55drf = wDate;
085000090721           fnlv55r ( FNLV55ds );
085100090721           if  D55err <> *blank;
085200090721             D55tfp = D55lin;
085300090721           endif;
085400090721         endif;
085500091008         WLStfp = D55tfp;              // - Terminal partenza
085600091007         chain  (D55tfp)  AZORG;
085700091007         if  %found(AZORG01L);
085800091008           WLStfpD = ORGdes;           // - Decodifica terminal partenza
085900091007         endif;
086000091008
086100090721
086200090721       ENDSR;
086300090721
086400090721       //--------------------------------------------------------------
086500090721       //?Calcolo n° di O.R.M. nel periodo per il cliente              ?
086600090721       //--------------------------------------------------------------
086700090721       BEGSR  sr_Calc_ORM;
086800090721
086900090721         //Ciclo di elaborazione O.R.M. (FNORM)?
087000090721         setll  (WLSfil)  FNORM000;
087100090721         reade  (WLSfil)  FNORM000;
087200090721
087300090721         DoW  NOT %eof(FNORM13L);
087400090721
087500090721           // -?Incremento numero di O.R.M.?
087600090721           if  ORMdao >= SB21dti   and
087700090721               ORMdao <= SB21dtf;
087800090721             WLSorm += 1;
087900090721           endif;
088000090721
088100090721           // -?Lettura record successivo di FNORM?
088200090721           reade  (WLSfil)  FNORM000;
088300090721
088400090721         EndDo;
088500090721
088600090721       ENDSR;
088700090612
088800090612      /end-free
