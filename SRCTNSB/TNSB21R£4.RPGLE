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
014700140520     d WLSnca164       s              9  0 inz
014800090612
014900090715       // - Campi per memorizzazione codici filiale
015000090715     d wCFlnp          s                   like(ORGfil)  inz
015100090721     d w_WLSfil        s                   like(WLSfil)  inz
015200140305     d ktrd            s                   like(ar5trd)  inz('GEN')
015300140305     d savtfp          s                   like(WLStfp)  inz
015400140305     d newtfp          s                   like(WLStfp)  inz
015500140305     d newfil          s                   like(WLSfil)  inz
015600140520     d savlnp          s                   like(d55lin)  inz
015700140520     d savlna          s                   like(d55lin)  inz
015800140520     d sav£6P          s                   like(d55lin)  inz
015900140520     d sav£6A          s                   like(d55lin)  inz
016000090612
016100090612       // - Campi per richiamo QCMDEXC
016200090612     d Qcmd            s            128    inz
016300090612
016400090612       //--------------------------------------------------------------
016500090612       //?Definizione procedure usate.                                 ?
016600090612       //--------------------------------------------------------------
016700090612
016800090612       // - Reperimento dati utente
016900090612      /copy gaitrasrc/srcProtoPR,TIBS34R
017000090714
017100090714       // - Reperimento terminal filiale
017200090714      /copy gaitrasrc/srcProtoPR,FNLV55R
017300090612
017400090612       // - Esecuzione comando di sistema
017500090612      /copy gaitrasrc/srcProtoPR,QCMDEXC
017600090612
017700090612       //--------------------------------------------------------------
017800090612       //?Definizione key-list.                                        ?
017900090612       //--------------------------------------------------------------
018000090612
018100090715     d k03titas38    e ds                  extname(TITAS38C : *key)
018200090715     d                                     inz  prefix(k_)
018300090612
018400090715     d k03tabel00    e ds                  extname(TABEL00F : *key)
018500090715     d                                     inz
018600090612
018700090612       //--------------------------------------------------------------
018800090612       //?M A I N - L I N E.                                           ?
018900090612       //--------------------------------------------------------------
019000090612
019100090612     c     *Entry        plist
019200090612     c                   parm                    KPJBA
019300090612
019400090612      /free
019500090612
019600090715       //Operazioni iniziali?
019700090612       exsr  sr_RoutInz;
019800090612
019900090715
020000090715       //Ciclo di lettura file spedizioni (TITAS)?
020100090715       clear  k03titas38;
020200090715       k_TASaas = wDataAA;
020300090715       k_TASmgs = wDataMG;
020400090715       k_TASksc = *loval;
020500090715       setll  %kds(k03titas38)  TITAS38C;
020600090714       read   TITAS38C;
020700090612
020800090714       DoW  NOT  %eof(TITAS38C);
020900090612
021000090714         // -?Selezione singolo record di TITAS?
021100090612         exsr  sr_Selez;
021200090714
021300090714         select;
021400090714
021500090714           // -?Fine ciclo di lettura?
021600090714           when  $EoF = *on;
021700090714             leave;
021800090714
021900090721           // -?Aggiornamento dati del work-file?
022000090714           when  $OK;
022100090721             exsr  sr_UpdRec;
022200090714
022300090714         endsl;
022400090612
022500090714         // -?Lettura record successivo di TITAS?
022600090714         read  TITAS38C;
022700090612
022800090714       EndDo;
022900140305
023000140305         // -Registrazione dati elaborati nel work-file?
023100140305         exsr  sr_WrtWFCLS;
023200140305
023300140305       //rileggo i record e imposto i totali
023400140305       exsr  sr_ScriviTot;
023500090715
023600090715       //Operazioni finali?
023700090612       exsr  sr_RoutEnd;
023800090715
023900090612
024000090612       //--------------------------------------------------------------
024100090612       //?Operazioni iniziali.                                         ?
024200090612       //--------------------------------------------------------------
024300090612       BEGSR  sr_RoutInz;
024400090612
024500090612         *inLR = *on;
024600090715
024700090715         TNSB21ds = kpjbu;
024800090612
024900090715         // -Reperimento dati job?
025000090612         exsr  sr_DatiJob;
025100090715
025200090715         // -Intabellamento cod. bolla ("3A") per sped. di recupero?
025300090715         clear  xx;
025400090715         clear  k03tabel00;
025500091005         TBLkut = c_kut;
025600090715         TBLcod = '3A';
025700090715         setll  %kds(k03tabel00)  TABEL;
025800090715         reade  %kds(k03tabel00 : 2)  TABEL;
025900090715         DoW  NOT %eof(TABEL00F);
026000090715           if  TBLflg = *blank;
026100090715             ds3A = TBLuni;
026200100409        //     if  §3Arbl = 'R'   or   §3Arbl = 'C';
026300100409             if  §3Arbl = 'R'   ;
026400090715               xx += 1;
026500090715               $CBOrec(xx) = TBLkey;
026600090715             endif;
026700090715           endif;
026800090715           reade  %kds(k03tabel00 : 2)  TABEL;
026900090715         EndDo;
027000090715
027100090715         // -Intabellamento cod. trattamento merce ("1B") per "Disk C"?
027200090715         clear  xx;
027300090715         clear  k03tabel00;
027400091005         TBLkut = c_kut;
027500090715         TBLcod = '1B';
027600090715         setll  %kds(k03tabel00)  TABEL;
027700090715         reade  %kds(k03tabel00 : 2)  TABEL;
027800090715         DoW  NOT %eof(TABEL00F);
027900090715           if  TBLflg = *blank;
028000090715             ds1B = TBLuni;
028100090721             //if  §1Bf12 = 'S'   or   §1Bf12 = 'E'   or   §1Bf12 = 'B';
028200090721             if  §1Bdkc = 'B'   or   §1Bdkc = 'C';
028300090715               xx += 1;
028400090721               $CTMdkC(xx)  = TBLkey;
028500090721               $CTMdkCx(xx) = §1Bdkc;
028600090715             endif;
028700090715           endif;
028800090715           reade  %kds(k03tabel00 : 2)  TABEL;
028900090715         EndDo;
029000090806
029100090806         // -Intabellamento filiali con network estero?
029200090806         clear  xx;
029300091005         clear  yy;
029400090806         setll (*loval) AZORG;
029500090806         read  AZORG;
029600090806         DoW  Not %eof(AZORG01L);
029700090806           Og143 = ORGde3;
029800090806           if  §ogNTW = 'DPD'   or          // DPD
029900090806               §ogNTW = 'EEX'   or          // EuroExpress
030000090806               §ogNTW = 'EUP'   or          // Europolitan (Estero+SDI)
030100090806               §ogNTW = 'FED';              // FedEx
030200090806             xx += 1;
030300090806             $FilNtwE(xx) = ORGfil;
030400091005             if  §ogNTW = 'DPD';
030500091005               yy += 1;
030600091005               $FilDPD(yy) = ORGfil;
030700091005             endif;
030800090806           endif;
030900090806           read  AZORG;
031000090806         EndDo;
031100090715
031200090715         // -Impostazione data iniziale?
031300090715         ds_DataISO = %editc( SB21dti : 'X' );
031400090715
031500090715         // -Reperimento Data & Ora correnti?
031600090715         ds_Time14 = %subst( %char( %dec( %timestamp() ) )
031700090715                             : 1 : 14 );
031800090612
031900090612       ENDSR;
032000090612
032100090612       //--------------------------------------------------------------
032200090714       //?Reperimento Dati del job (Utente/Operativi).                 ?
032300090612       //--------------------------------------------------------------
032400090612       BEGSR  sr_DatiJob;
032500090612
032600090612         in(E) §AzUte;
032700090612         if NOT %error;
032800090612           in(E) §DatiUte;
032900090612         endif;
033000090612         if %error or RSut = *blanks;
033100090612           clear TIBS34ds;
033200090612           tibs34r(tibs34ds);
033300090612           in §AzUte;
033400090612           in §DatiUte;
033500090612         endif;
033600090612
033700090612       ENDSR;
033800090612
033900090612       //--------------------------------------------------------------
034000090714       //?Selezione record da TITAS                                    ?
034100090612       //--------------------------------------------------------------
034200090612       BEGSR  sr_Selez;
034300090612
034400090612         $Ok = *off;
034500090714
034600090714         select;
034700090714
034800090715           // -Data spedizione oltre il limite?
034900090715           when  (TASaas * 10000) + TASmgs > SB21dtf;
035000090714             $EoF = *on;
035100090714             leavesr;
035200090715
035300090715           // -Tipo bolla?
035400100409           // when  TAStbl <> 'F1'   and   TAStbl <> 'A2';
035500100409           //  leavesr;
035600090715
035700090715           // -Spedizione senza segnacolli?
035800100409           // when  TASncd = *zero and TASFNS = *blank;
035900100409           //  leavesr;
036000090715
036100100409           // -Escluse solo Spedizioni di recupero?
036200090715           when  %lookup( TAScbo : $CBOrec ) > *zero;
036300090715             leavesr;
036400090612
036500090715         endsl;
036600090715
036700090715         $Ok = *on;
036800090612
036900090612       ENDSR;
037000090612
037100090612       //--------------------------------------------------------------
037200090721       //?Aggiornamento dati del work-file (con dati da TITAS)         ?
037300090612       //--------------------------------------------------------------
037400090721       BEGSR  sr_UpdRec;
037500140305
037600140305         // -Lettura fiar5 ?
037700140305         chain (tasaas:taslnp:tasnrs:tasnsp:ktrd) fiar531c  ;
037800140305         if %found(fiar531c) ;
037900140305         dar5gen=ar5uni  ;
038000140305         else  ;
038100140305         clear dar5gen  ;
038200140305         endif  ;
038300090721
038400090721         // -Reperimento capofila £6 della LNP?
038500090721         //   (serve comunque per gli O.R.M.)
038600140520         if  TASlnp <> SAVlnp;
038700090721           reset  FNLV55ds;
038800090721           //D55tpt = '6'                   (già così)
038900090721           D55lin = TASlnp;
039000090721           D55drf = wDate;
039100090721           fnlv55r ( FNLV55ds );
039200090721           if  D55err <> *blank;
039300090721             D55tfa = D55lin;
039400090721           endif;
039500140520          savlnp=d55lin  ;
039600140520          sav£6P=d55tfa  ;
039700090721         endif;
039800140520
039900140520         wCFlnp = sav£6P;
040000090715
040100090721         // -Impostazione capofila £6 per la filiale?
040200100412         // If  $DDTsi;
040300090721
040400100412         //  w_WLSfil = wCFlnp;               // LNP per "DDT SÌ"
040500090721
040600100412         //Else;
040700090721
040800100412           // -?Reperimento SEMPRE capofila £6 della LNA?
040900140520           if  TASlna <> SAVlna;
041000090721             reset  FNLV55ds;
041100090721             //D55tpt = '6'                 (già così)
041200090721             D55lin = TASlna;
041300090721             D55drf = wDate;
041400090721             fnlv55r ( FNLV55ds );
041500090721             if  D55err <> *blank;
041600090721               D55tfa = D55lin;
041700090721             endif;
041800140520             sav£6A= d55tfa  ;
041900140520             savlna= d55lin  ;
042000100412            endif;
042100140520           w_WLSfil = sav£6A;               // LNA altrimenti
042200090721
042300100412         //EndIf;
042400090715
042500100412         //conteggio colli per linea di arrivo:?
042600090721
042700090721         //Reperimento posizione elemento dati per la filiale?
042800090721         xx = %lookup(w_WLSfil : $FilWF);
042900090721         if  xx = *zero;
043000090721           xx = %lookup(*zero : $FilWF);
043100090721           if  xx = *zero;                  // Esaurita schiera:
043200090721             xx = *hival;                   // => GENERA ERRORE!
043300090721           endif;
043400090721           $FilWF(xx) = w_WLSfil;
043500090721           clear  ds_WFCLS;
043600090722         else;
043700090722           ds_WFCLS = $RecWF(xx);
043800090722         endif;
043900100412
044000100412         // -Numero LdV (scartando network esteri)?
044100140305         WLSfil = $FilWF(xx);
044200140305           if    %lookup(WLSfil : $FilNtwE) <= *zero;
044300140331           //  con PDA
044400140331             if §AR5PDACO='  '  ;
044500140331             WLSlvaspda += 1;                   // in arrivoPDA
044600140331             else  ;
044700140331             WLSlvanpda += 1;                   // in arrivo noPDA
044800140331             endif  ;
044900140331
045000140305             WLSnca += TASncl;                 // in arrivo
045100140305
045200140305             // verifico se ha packing list o pdf
045300140305             if §ar5alx='S' or §ar5als='S'  ;
045400150331             WLSlpkl += 1;
045500140305             endif  ;
045600140305           endif   ;
045700140305             $RecWF(xx) = ds_WFCLS;
045800140520
045900140520         // -CHIODO per la filiale 164 : ?
046000140520         // -  sommo anche i colli dei secondi livello ?
046100140520         // -  per il totale delle etichette ARANCIONI ?
046200140520         clear  FNLV55ds;
046300140520           D55tpt = 'A';
046400140520           D55lin = w_WLSfil ;
046500140520           D55lnp = taslnp   ;
046600140520           D55drf = wDate;
046700140520           fnlv55r ( FNLV55ds );
046800140520           if  D55err <> *blank;
046900140520             D55tfa = D55lin;
047000140520           endif;
047100140520
047200140520          if d55tfa=164  ;
047300140520             WLSnca164 += TASncl;                 // in arrivo
047400140520          endif  ;
047500090612
047600090721         //Aggiornamento dati per la filiale?
047700100412         //WLSfil = $FilWF(xx);
047800090721
047900090806         // -Numero LdV (scartando network esteri)?
048000100412         //select;
048100100412         //  when  %lookup(WLSfil : $FilNtwE) > *zero;
048200100412         //  when  $DDTsi;
048300100412         //    WLSlvp += 1;                   // in partenza ("DDT SÌ")
048400100412         //  other;
048500100412         //    WLSlva += 1;                   // in arrivo
048600100412         //    WLSnca += TASncl;              // in arrivo
048700100412         //endsl;
048800090721
048900100412         //$RecWF(xx) = ds_WFCLS;
049000090715
049100090721         // -Reperimento posizione elemento dati per la LNP?
049200090722         If  wCFlnp <> $FilWF(xx);
049300090722           xx = %lookup(wCFlnp : $FilWF);
049400090722           if  xx = *zero;
049500090722             xx = %lookup(*zero : $FilWF);
049600090722             if  xx = *zero;                // Esaurita schiera:
049700090722               xx = *hival;                 // => GENERA ERRORE!
049800090722             endif;
049900090722             $FilWF(xx) = wCFlnp;
050000090722             clear  ds_WFCLS;
050100090722           else;
050200090722             ds_WFCLS = $RecWF(xx);
050300090722           endif;
050400090722         EndIf;
050500100412
050600100412         // -Verifica se spedizione "DDT SÌ"?
050700100412         $DDTsi = (TASll1 = 'C' or TASll1 = 'S' or TASll1 = *blank);
050800100412
050900100412         // -Numero LdV DDT SI escluso export?
051000100412           if    $DDTsi  ;
051100100412             WLSlvp += 1;                   // in partenza ("DDT SÌ")
051200100412           endif   ;
051300090721
051400090721         // -Dettaglio spedizioni e colli (x LNP)?
051500090721         yy = %lookup( TASctm : $CTMdkC );
051600090715         select;
051700090715           // -?Disk-C?
051800090721           when  yy > *zero;
051900090715             WLSecs += 1;
052000090715             WLSecc += TASncl;
052100090721             if  $CTMdkCx(yy) = 'B';
052200091005               if  %lookup(TASlna : $FilDPD) = *zero;
052300091005                 WLSecBs += 1;
052400091005                 WLSecBc += TASncl;
052500091005               else;
052600091005                 WLSecBsD += 1;
052700091005                 WLSecBcD += TASncl;
052800091005               endif;
052900090721             endif;
053000090715           // -?Disk-B?
053100090715           when  TASnrs > *zero;
053200140305             WLSebb += TASncl;
053300140305             // di cui easy web/sped
053400140305
053500140305             tblkey=%editc(tasccm:'X')  ;
053600140305           chain  (1:'3C': tblkey)  TABEL;
053700140305           if %found(tabel00f) ;
053800140305           ds3c=tbluni  ;
053900140305           if §3ccba='ESWEB' or §3ccba='ESYSP'  ;
054000140305             WLSEBBWS  += TASncl;
054100140305           endif;
054200140305           endif;
054300140305
054400090715           // -?né Disk-C né Disk-B?
054500090715           other;
054600090806             WLSeNbcs += 1;
054700090806             WLSeNbcc += TASncl;
054800090715         endsl;
054900090721
055000090721         $RecWF(xx) = ds_WFCLS;
055100090612
055200090612       ENDSR;
055300140305
055400140305       //--------------------------------------------------------------
055500140305       //?Rileggo per scrivere i totali                                ?
055600140305       //--------------------------------------------------------------
055700140305       BEGSR  sr_ScriviTot;
055800140305       clear totrotolo  ;
055900140305       clear savtfp   ;
056000140305
056100140305       setll *loval   wfcls11l  ;
056200140305       read  wfcls11l  ;
056300140305       dow  not %eof(wfcls11l)  ;
056400140305
056500150331       // totale fogli A4 LDV
056600150331       // DDT si
056700150331       WLSLDVT = WLSLVP ;
056800150331       // Pack.list di cui raddoppio le pagine MAGGIORATO DEL 10%
056900150331       //           ( la media pack.list è infatti 1,4)
057000150331       if wlslpkl>0 ;
057100150331       WLSLDVT +=  ((WLSLPKL*2)*110/100) ;
057200150331       endif  ;
057300140305
057400140305       //  con   PDA
057500140305       // +  bolle arrivo si pda + 10% /4
057600140305       if WLSLVASPDA> 0  ;
057700140305       WLSLDVT +=  ((wlslvaspda*110/100)/4) ;
057800140305       endif  ;
057900140305
058000140305       // senza PDA
058100140305       // +  bolle arrivo si pda + 10% /2
058200140305       if WLSLVAnPDA> 0  ;
058300140305       WLSLDVT +=  ((wlslvanpda*110/100)/2) ;
058400140305       endif  ;
058500140305
058600140305       // etichette ARANCIONI solo se filiale 2=liv con picking
058700140305       //  colli arrivati maggiorati del 10%
058800140305       if WLSPCK = 'S' and wlstfp<>wlsfil  ;
058900140520         // se si tratta della filiale di 164 sonno anche i colli dei secondi liv in arrivo
059000140520         if wlsfil=164  ;
059100140520           WLSETAR  = (WLSnca164*110/100)  ;
059200140520         else  ;
059300140520           WLSETAR  = (WLSNCA*110/100)  ;
059400140520         endif  ;
059500140520       endif  ;
059600140305
059700140305       // Etichette IN PIEGA
059800140305       WLSETPG  = WLSENBCC  ;
059900140305
060000140305       update wfcls011  ;
060100140305
060200140305       // Etichette in ROTOLO  ;
060300140305       //  sommo i disK C di tutto il terminal e poi scrivo solo sul terminal
060400140305       if savtfp<>wlstfp and savtfp>0  ;
060500140305       newtfp=wlstfp  ;
060600140305       newfil=wlsfil  ;
060700140305       chain (savtfp:savtfp)   wfcls11l  ;
060800140305       if %found(wfcls11l)  ;
060900140305       WLSETROT = totRotolo  ;
061000140305       update wfcls011  ;
061100140305       chain (newtfp:newfil)   wfcls11l  ;
061200140305       endif  ;
061300140305
061400140305       clear totrotolo  ;
061500140305       endif  ;
061600140305
061700140305       savtfp=wlstfp  ;
061800140305       totROTOLO += WLSECC  ;
061900140305
062000140305       read  wfcls11l  ;
062100140305       enddo  ;
062200140305
062300140305       ENDSR;
062400090612
062500090612       //--------------------------------------------------------------
062600090714       //?Operazioni finali.                                           ?
062700090612       //--------------------------------------------------------------
062800090612       BEGSR  sr_RoutEnd;
062900090721
063000090612
063100090721         // -Uscita?
063200090612         return;
063300090612
063400090612       ENDSR;
063500090721
063600090721       //--------------------------------------------------------------
063700140305       //?Registrazione dati nel file WFCLS10F                         ?
063800090721       //--------------------------------------------------------------
063900090721       BEGSR  sr_WrtWFCLS;
064000090721
064100090806         // -Pulizia / Apertura del work-file?
064200090806         Qcmd = 'CLRPFM file(';
064300090806         if  %subst( knsif : 7 : 1 ) = 'P';
064400090806            Qcmd = %trimr(Qcmd) + 'GAITRAAZP';
064500090806         else;
064600090806            Qcmd = %trimr(Qcmd) + 'GAITRAAZM';
064700090806         endif;
064800140305         Qcmd = %trimr(Qcmd) + '/WFCLS10F)';
064900090806         ExecuteCommand ( Qcmd : %size(Qcmd) );
065000090806
065100140305         open  WFCLS10F;
065200140305         open  WFCLS11l;
065300090721
065400090721         // -Registrazione record nel work-file?
065500090721         FOR  xx = 1  TO  %elem($FilWF);
065600090721
065700090721           if  $FilWF(xx) = *zero;
065800090721             leave;
065900090721           endif;
066000090721
066100090721           clear  WFCLS000;
066200090721
066300090721           // -?Reperimento dati relativi alle spedizioni (già calc.)?
066400091008           //  ?ed impostazione campi del WF con tali dati (già calc.)?
066500090721           ds_WFCLS = $RecWF(xx);
066600090721
066700090721           // -?Impostazione campi chiave se nuovo record?
066800090721           WLSute  = knmus;            // - Utente
066900090721           WLSdta  = wDate;            // - Data di lancio
067000090721           WLSdti  = SB21dti;          // - Data inizio periodo
067100090721           WLSdtf  = SB21dtf;          // - Data fine periodo
067200090721           WLSfil  = $FilWF(xx);       // - Filiale
067300090721           exsr  sr_NewFil;
067400090721
067500090721           // -?Conteggio numero O.R.M. per la filiale nel periodo?
067600090806           //  ?(NON per network esteri)?
067700090806           if  %lookup(WLSfil : $FilNtwE) = *zero;
067800090806             exsr  sr_Calc_ORM;
067900090806           endif;
068000090721
068100090721           // -?Scrittura/Aggiornamento del record?
068200090721           write  WFCLS000;
068300090721           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
068400090721
068500090721         ENDFOR;
068600090721
068700090721         // -Chiusura del work-file?
068800140305         close WFCLS10F;
068900090721
069000090721       ENDSR;
069100090721
069200090721       //--------------------------------------------------------------
069300090721       //?Reperimento dati della filiale                               ?
069400090721       //--------------------------------------------------------------
069500090721       BEGSR  sr_NewFil;
069600090721
069700090721         // -Reperimento dati anagrafici?
069800091005         clear  Og150;
069900091008         clear  ORGdes;
070000091008         clear  ORGcar;
070100091005         clear  ds05;
070200090721         chain  (WLSfil)  AZORG;
070300090721         if  %found(AZORG01L);
070400091008           WLSfilD = ORGdes;           // - Decodifica filiale
070500091008           WLScar  = ORGcar;           // - Codice area
070600090721           Og150 = ORGdf0;
070700091008           WLStis  = §OGtis;           // - Tipo impianto smistacolli
070800091008           WLSpck  = §OGpck;           // - Flag picking (da Og150)
070900091005           TBLkut = c_kut;
071000091005           TBLcod = '05';
071100091005           TBLkey = %editc(ORGcar : 'X');
071200091005           chain  %kds(k03tabel00)  TABEL;
071300091005           if  %found(TABEL00F)   and   TBLflg = *blank;
071400091005             ds05 = TBLuni;
071500091005           endif;
071600091008           WLScard = §05des;           // - Decodifica area
071700090721         endif;
071800090721
071900090721         // -Reperimento terminal di partenza (x O.R.M.)?
072000090721         clear  FNLV55ds;
072100090721         if  ORGfag = 'A'   or   ORGfag = 'F';
072200090721           D55tpt = 'P';
072300090721           D55lin = WLSfil;
072400090721           D55drf = wDate;
072500090721           fnlv55r ( FNLV55ds );
072600090721           if  D55err <> *blank;
072700090721             D55tfp = D55lin;
072800090721           endif;
072900090721         endif;
073000091008         WLStfp = D55tfp;              // - Terminal partenza
073100091007         chain  (D55tfp)  AZORG;
073200091007         if  %found(AZORG01L);
073300091008           WLStfpD = ORGdes;           // - Decodifica terminal partenza
073400091007         endif;
073500091008
073600090721
073700090721       ENDSR;
073800090721
073900090721       //--------------------------------------------------------------
074000090721       //?Calcolo n° di O.R.M. nel periodo per il cliente              ?
074100090721       //--------------------------------------------------------------
074200090721       BEGSR  sr_Calc_ORM;
074300090721
074400090721         //Ciclo di elaborazione O.R.M. (FNORM)?
074500090721         setll  (WLSfil)  FNORM000;
074600090721         reade  (WLSfil)  FNORM000;
074700090721
074800090721         DoW  NOT %eof(FNORM13L);
074900090721
075000090721           // -?Incremento numero di O.R.M.?
075100090721           if  ORMdao >= SB21dti   and
075200090721               ORMdao <= SB21dtf;
075300090721             WLSorm += 1;
075400090721           endif;
075500090721
075600090721           // -?Lettura record successivo di FNORM?
075700090721           reade  (WLSfil)  FNORM000;
075800090721
075900090721         EndDo;
076000090721
076100090721       ENDSR;
076200090612
076300090612      /end-free
