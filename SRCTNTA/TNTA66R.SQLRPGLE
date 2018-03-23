000100160729      //--------------------------------------------------------------
000200160729      //?TNTA66R - STATISTICA COMMERCIALE
000300160729      //--------------------------------------------------------------
000400160729     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000500160729
000600160729      //---------------------------------------------------------------
000700160729      //?Dichiarazione file.
000800160729      //---------------------------------------------------------------
000900160729      //?File Potenziali
001000160801     fTNCPO08L  if   e           k disk
001100160729     fTNCPO01L  if   e           k disk    rename(TNCPO000:TNCPO001)
001200160729
001300160729      //?File Trattative
001400160729     fTIVIS05L  if   e           k disk
001500160729
001600160729      //?File Anagrafiche Clienti
001700160729     fCNCLP00F  if   e           k disk
001800160729     fTFCLP00F  if   e           k disk    rename(CNCLP000:TFCLP000)
001900160729
002000160729      //?File Statistica Commerciale
002100160801     fTFSAC02L  uf a e           k disk    rename(TFSAC000:TFSAC02)
002200160801     fTFSAC01L  uf a e           k disk
002300160729
002400160729      //?File Organigramma
002500160729     fAZORG01L  if   e           k disk
002600160729      //?File Tabelle
002700160729
002800160729     fTNTBE01L  uf   e           k disk
002900160728     fTABEL00F  if   e           k disk
003000160729
003100160729      //---------------------------------------------------------------
003200160729      //?Definizione costanti.
003300160729      //---------------------------------------------------------------
003400160729      //?Elementi per schiera Cataegoria Cliente
003500160729     d c_elem_IC       c                   const(10)
003600160729
003700160729      //---------------------------------------------------------------
003800160729      //?Definizione schiere.
003900160729      //---------------------------------------------------------------
004000160729      //?Categoria Cliente
004100160729     d sk_IC           s              1a   dim(c_elem_IC)  inz
004200160729
004300160729      //?Totali per Visite Valide (Flag Fittizio = ' ')
004400160729     d TO1             s              7  0 dim(c_elem_IC)
004500160729     d TO2             s              7  0 dim(c_elem_IC)
004600160729     d TO3             s              7  0 dim(c_elem_IC)
004700160729     d OFE             s              7  0 dim(c_elem_IC)
004800160729     d COE             s              7  0 dim(c_elem_IC)
004900160729
005000160729      //---------------------------------------------------------------
005100160729      //?Definizione aree dati.
005200160729      //---------------------------------------------------------------
005300160729      // - Dati utente
005400160729     d §AzUte        e ds                  extname(AZUTE00F)
005500160729     d                                     dtaara
005600160729     d §DatiUte      e ds                  extname(dDatiUte)
005700160729     d                                     dtaara
005800160729
005900160729      //---------------------------------------------------------------
006000160729      //?Definizione strutture dati.
006100160729      //---------------------------------------------------------------
006200160729     d KPJBA         e ds
006300160729
006400160729      //?Ricerca Tabelle
006500160729     d TIBS02DS      e ds
006600160729
006700160729      //?Controllo dati Utente
006800160729     d TIBS34DS      e ds
006900160729
007000160729      //?File Attività Commerciali
007100160729     d TIATC00F      e ds                  extname(TIATC00F)
007200160801
007300160801      //?File Offerte
007400160801     d TIVOF00F      e ds                  extname(TIVOF00F)
007500160729
007600160729      //?Tabella SDF - Statistiche di Filiale
007700160729     d dSDF          e ds
007800160729
007900160729      //?Tabella TTR - Tipo Trattativa
008000160729     d dTTR          e ds
008100160729
008200160801      //?Dati campo CPORST di
008300160729     d dCPO01        e ds                  inz
008400160729
008500160729      //?Parametri passati nella KPJBU
008600160729        // paramm --> Anno e Mese
008700160729        // parctr --> 'N' = Elaboro senza controllare se già elaborato
008800160729        //            ' ' = Controllo se già elaborato se sì non elaboro
008900160729     d param           ds
009000160729     d  paramm                 1      6
009100160729     d  parctr                 7      7
009200160729
009300160729      //---------------------------------------------------------------
009400160729      //?Definizione variabili globali.
009500160729      //---------------------------------------------------------------
009600160729      // - Flags booleani
009700160729     d Aggio           s               n   inz
009800160801     d VisNewOld       s               n   inz
009900160801     d wFine           s               n   inz
010000021017
010100160729      // - Indici di schiera
010200160729     d IndiceFLT       s              3s 0 inz
010300160729     d yy              s              4s 0 inz
010400160729
010500160729       // - Stringa SQL da eseguire
010600160729     d wSQL            s           4096    varying
010700160729
010800160729      // - Campi di comodo data
010900160729     d Data_ISO        s               d   datfmt(*iso)
011000160801     d DataFin         s              8s 0 inz
011100160801     d DataIni         s              8s 0 inz
011200160801     d wAAMM           s              6s 0 inz
011300160801     d wAAMMcpo        s              6s 0 inz
011400160801     d wAAMMvof        s              6s 0 inz
011500160801     d wAnno           s              4s 0 inz
011600160801     d wData           s              8s 0 inz
011700160801     d wMese           s              2s 0 inz
011800160729
011900160729      // - Campi di comodo
012000160801     d DataPrimaCON    s              8s 0 inz
012100160801     d DataPrimaOFF    s              8s 0 inz
012200160801     d k_kut           s              1s 0 inz(1)
012300160729     d Oggi            s              8s 0 inz
012400160801     d savcmm          s                   like(ATCcmm)
012500160801     d savcpo          s                   like(ATCcpo)
012600160801     d savflt          s                   like(CPOflt)
012700160801     d savksc          s                   like(ATCksc)
012800160801     d savnrv          s                   like(ATCnrv)
012900160801     d savtpv          s                   like(VIStpv)
013000160729     d wApl            s                   like(SACapl)
013100160801     d wCLV            s                   like(CLPclv)
013200160801     d wWrite          s              2a   inz
013300160729
013400160729      //---------------------------------------------------------------
013500160729      //?Definizione procedure usate.
013600160729      //---------------------------------------------------------------
013700160729
013800160729      //---------------------------------------------------------------
013900160729      //?Definizione Prototipi.
014000160729      //---------------------------------------------------------------
014100160801      /copy gaitrasrc/srcprotopr,TIBS02R
014200160729      /copy gaitrasrc/srcprotopr,TIBS34R
014300160729
014400160729      //---------------------------------------------------------------
014500160729      //?Definizione key-list.
014600160729      //---------------------------------------------------------------
014700160729      //?File TNCPO08L
014800160801     d k08tncpo      e ds                  extname(TNCPO08L:*key)
014900160729     d                                     prefix(k_)
015000160729      //?File TFSAC01L
015100160729     d k01tfsac      e ds                  extname(TFSAC01L:*key)
015200160729     d                                     prefix(k_)
015300160801      //?File TNTBE01L
015400160801     d k01tntbe      e ds                  extname(TNTBE01L:*key)
015500160801     d                                     prefix(k_)
015600160801      //?File TABEL00F
015700160801     d k00tabel      e ds                  extname(TABEL00F:*key)
015800160801     d                                     prefix(k_)
015900160729
016000160729      //---------------------------------------------------------------
016100160729      //?Indicatori utilizzati
016200160729      //---------------------------------------------------------------
016300160729
016400160729      //---------------------------------------------------------------
016500160729      //?M A I N - L I N E
016600160729      //---------------------------------------------------------------
016700160729
016800160729     c     *Entry        plist
016900160801     c                   parm                    kpjba
017000160729
017100160729       //?Operazioni iniziali
017200160729       exsr RoutInz;
017300160729
017400160729       //?Elaborazione
017500160729       exsr Elabora;
017600160729
017700160729       //?Aggiorno tabella SDF con data ultima elaborazione
017800160729       IF  wAAMM > §SDFstcu;
017900160729         chain (k_TBEcod:k_TBEke1) TNTBE01L;
018000160729         IF  %found(TNTBE01L);
018100160729           dSDF = TBEuni;
018200160729           §SDFstcu = wAAMM;
018300160729           TBEuni = dSDF;
018400160729           update TNTBE000;
018500160729         ENDIF;
018600160729       ENDIF;
018700160729
018800160729       //?Operazioni finali
018900160729       exsr RoutEnd;
019000160729
019100160729       //--------------------------------------------------------------
019200160729       //?Operazioni iniziali.
019300160729       //--------------------------------------------------------------
019400160729       BEGSR RoutInz;
019500160729
019600160729         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
019700160729
019800160729         param = kpjbu;
019900160729
020000160729       //?Reperimento dati job
020100160729         exsr DatiJob;
020200160729
020300160729         k_TBLkut = 1;
020400160729
020500160729       //?Imposto oggi
020600160729         Oggi = %dec(%date());
020700160729
020800160729       //?Carico sk Importanza Clienti
020900160729         clear yy;
021000160729         k_TBLcod = 'IC';
021100160729         setll (k_TBLkut:k_TBLcod) TABEL00F;
021200160729         reade (k_TBLkut:k_TBLcod) TABEL00F;
021300160729
021400160729         DOW not %eof(TABEL00F);
021500160729           IF  TBLflg = *blanks;
021600160729             yy += 1;
021700160729             sk_IC(yy) = %subst(TBLkey:1:1);
021800160729           ENDIF;
021900160729           reade (k_TBLkut:k_TBLcod) TABEL00F;
022000160729         ENDDO;
022100160729
022200160729       //?Controllo se è stato passato anno/mese
022300160729         IF  paramm > *zeros;
022400160729           wAnno = %dec(%subst(paramm:1:4):4:0);
022500160729           wMese = %dec(%subst(paramm:5:2):2:0);
022600160729         ELSE;
022700160729       //?Imposto oggi - 1 mese
022800160729           Data_ISO = %date(Oggi);
022900160729           Data_ISO = Data_ISO - %months(1);
023000160729           wData = %dec(Data_ISO);
023100160729           wAnno = %dec(%subst(%editc(wData:'X'):1:4):4:0);
023200160729           wMese = %dec(%subst(%editc(wData:'X'):5:2):2:0);
023300160729         ENDIF;
023400160729
023500160729         wAAMM = (wAnno * 100) + wMese;
023600160729
023700160729       //?Cerco orgfel per agganciare la tabella SDF
023800160729         chain (simfel) AZORG01L;
023900160729         IF  not %found(AZORG01L);
024000160729           clear ORGfel;
024100160729         ENDIF;
024200160729
024300160906         k_TBEcod = 'SDF';
024400160906         k_TBEke1 = %editc(ORGfel:'X');
024500160906         chain(n) (k_TBEcod:k_TBEke1) TNTBE01L;
024600160729         IF  not %found(TNTBE01L);
024700160729           exsr Routend;
024800160729           leavesr;
024900160729         ENDIF;
025000160729         dSDF = TBEuni;
025100160729
025200160729       //?Se già elaborata esco dal pgm
025300160729         IF  parctr = *blanks and wAAMM <= §SDFstcu;
025400160729           exsr Routend;
025500160729           leavesr;
025600160729         ENDIF;
025700160729
025800160729       //?Se già elaborata cancello e rielaboro
025900160729         setll (wAnno:wMese) TFSAC02L;
026000160729         reade (wAnno:wMese) TFSAC02L;
026100160729         DOW  not %eof(TFSAC02L);
026200160801           delete TFSAC02;
026300160729           reade (wAnno:wMese) TFSAC02L;
026400160729         ENDDO;
026500160729
026600160729       ENDSR;
026700160729
026800160729       //--------------------------------------------------------------
026900160729       //?Reperimento Dati del job (Utente/Operativi).
027000160729       //--------------------------------------------------------------
027100160729       BEGSR DatiJob;
027200160729
027300160729         in(E) §AzUte;
027400160729         IF  NOT %error;
027500160729           in(E) §DatiUte;
027600160729         ENDIF;
027700160729         IF  %error or RSut = *blanks;
027800160729           clear TIBS34ds;
027900160729           tibs34r(tibs34ds);
028000160729           in §AzUte;
028100160729           in §DatiUte;
028200160729         ENDIF;
028300160729
028400160729       ENDSR;
028500160729
028600160729       //--------------------------------------------------------------
028700160729       //?Elaborazione.
028800160729       //--------------------------------------------------------------
028900160729       BEGSR Elabora;
029000160729
029100160729         Aggio = *off;
029200160729
029300160729       //?POTENZIALI
029400160729         wApl = 'P';
029500160729       //?Leggo i potenziali da contattare
029600160729       //?quindi con commerciale a 0
029700160729       //?e con data primo contatto non impostata
029800160729         clear k_CPOcmm;
029900160729         setll (k_CPOcmm) TNCPO08L;
030000160729         reade (k_CPOcmm) TNCPO08L;
030100160729         DOW  not %eof(TNCPO08L);
030200160729         //?escludo potenziali annullati e quelli senza filiale gestione
030300160729           IF  CPOatb = *blanks and CPOflt > 0;
030400160729             dCPO01 = CPOrst;
030500160729             IF  §CPOdtpcon = *blanks or §CPOdtpcon = *zeros;
030600160801               IF  savflt <> CPOflt;
030700160801                 IF  savflt > 0;
030800160801               //?Rottura filiale
030900160729                   exsr Rot_FLT;
031000160729                 ENDIF;
031100160801                 savflt = CPOflt;
031200160729               ENDIF;
031300160729               TO1(1) += 1;
031400160801               IndiceFLT = savflt;
031500160729             ENDIF;
031600160729           ENDIF;
031700160729           reade (k_CPOcmm) TNCPO08L;
031800160729         ENDDO;
031900160801
032000160801       //?Rottura filiale
032100160729         exsr Rot_FLT;
032200160729
032300160729       //?Leggo la prima azione
032400160729       //?se maggiore del mese di elaborazione
032500160729       //?nel mese che sto calcolando era un cliente da contattare
032600160801         clear savflt;
032700160729         k_CPOcmm = 1;
032800160729         setll (k_CPOcmm) TNCPO08L;
032900160801         read TNCPO08L;
033000160729         DOW  not %eof(TNCPO08L);
033100160729         //?escludo potenziali annullati
033200160729           IF  CPOatb = *blanks;
033300160729             dCPO01 = CPOrst;
033400160729             IF  §CPOdtpcon <> *blanks and §CPOdtpcon > *zeros;
033500160801               wAAMMcpo = %dec(%subst(§CPOdtpcon:1:6):6:0);
033600160801               IF  wAAMMcpo > wAAMM;
033700160801                 IF  savflt <> CPOflt;
033800160801                   IF  savflt > 0;
033900160801                 //?Rottura filiale
034000160729                     exsr Rot_FLT;
034100160729                   ENDIF;
034200160801                   savflt = CPOflt;
034300160729                 ENDIF;
034400160729                 TO1(1) += 1;
034500160801                 IndiceFLT = savflt;
034600160729               ENDIF;
034700160729             ENDIF;
034800160729           ENDIF;
034900160801           read TNCPO08L;
035000160729         ENDDO;
035100160801
035200160801       //?Rottura filiale
035300160729         exsr Rot_FLT;
035400160729
035500160729       //?CRM
035600160729         Aggio = *on;
035700160729       //?Trattative
035800160729         exsr NEWvis;
035900160729       //?Offerte ITALIA
036000160729         exsr NEWoff;
036100160729       //?Offerte ESTERO
036200160729         exsr NEWestoff;
036300160729       //?Offerte ITALIA convalidate
036400160729         exsr NEWconval;
036500160729       //?Offerte ESTERO convalidate
036600160729         exsr NEWestcon;
036700160729
036800160729       ENDSR;
036900160729
037000160729       //--------------------------------------------------------------
037100160729       //?Rottura filiale.
037200160729       //--------------------------------------------------------------
037300160729       BEGSR Rot_FLT;
037400160729
037500160729         IF  TO1(1) > 0;
037600160729
037700160801           k_SACcmm = savflt * 10000;
037800160729           chain (k_SACcmm:wAnno:wMese:wApl) TFSAC01L;
037900160729           IF  not %found(TFSAC01L);
038000160729             clear TFSAC000;
038100160729             SACcmm = k_SACcmm;
038200160729             SACaaa = wAnno;
038300160729             SACmmm = wMese;
038400160729             SACapl = wApl;
038500160729           ENDIF;
038600160729           clear SACftr;
038700160801           SACdtr = Oggi;
038800160729           SACto1 += TO1(1);
038900160729           IF  not %found(TFSAC01L);
039000160729             write TFSAC000;
039100160729           ELSE;
039200160729             update TFSAC000;
039300160729           ENDIF;
039400160729    1
039500160729         ENDIF;
039600160729
039700160729         clear TO1;
039800160729
039900160729       ENDSR;
040000160729
040100160729       //--------------------------------------------------------------
040200160729       //?Trattative.
040300160729       //--------------------------------------------------------------
040400160729       BEGSR NEWvis;
040500160729
040600160801         clear DataIni;
040700160801         clear DataFin;
040800160801         DataIni = (wAAMM * 100) + 01;
040900160801         DataFin = (wAAMM * 100) + 31;
041000160801
041100160801       //?Cerco i Clienti Nuovi
041200160801         clear savtpv;
041300160801         wFine = *off;
041400160801         wSQL = 'SELECT ATCcmm, ATCnrv, ATCcpo, ATCksc -
041500160801                 FROM TIATC00F -
041600160801                 WHERE ATCtat = ''A'' and ATCdad between ' +
041700160801                 %editc(DataIni:'X') + ' and ' +
041800160801                 %editc(DataFin:'X') +
041900160801                ' and ATCest = ''S'' and ATCksc = 0 -
042000160801                 ORDER by ATCcmm';
042100160801
042200160801       //?Dichiarazione cursore
042300160801         exec sql
042400160801         prepare S1 from :wSQL;
042500160801         exec sql
042600160801         DECLARE A1 CURSOR for S1;
042700160801
042800160801       //?Apertura del cursore
042900160801         exec sql
043000160801         OPEN A1;
043100160801
043200160801         IF sqlcode < 0;
043300160801           wFine = *on;
043400160801           exec sql
043500160801           CLOSE A1;
043600160801           leavesr;
043700160801         ENDIF;
043800160801
043900160801         DOW  not wFine;
044000160801           exec sql
044100160801           FETCH NEXT from A1 into :ATCcmm, :ATCnrv, :ATCcpo, :ATCksc;
044200160801           IF sqlcod = 100 or sqlcod < 0;
044300160801             wFine = *on;
044400160801             leave;
044500160801           ENDIF;
044600160801
044700160801       //?Conto gli appuntamenti
044800160801           exsr ContaApp;
044900160801         ENDDO;
045000160801
045100160801       //?Rottura tipo visita
045200160801         exsr Rot_TPV;
045300160801
045400160801       //?Chiusura del cursore
045500160801         exec sql
045600160801         CLOSE A1;
045700160729
045800160801       //?Cerco i Clienti Esistenti
045900160801         clear savtpv;
046000160801         wFine = *off;
046100160801         wSQL = 'SELECT ATCcmm, ATCnrv, ATCcpo, ATCksc -
046200160801                 FROM TIATC00F -
046300160801                 WHERE ATCtat = ''A'' and ATCdad between ' +
046400160801                 %editc(DataIni:'X') + ' and ' +
046500160801                 %editc(DataFin:'X') +
046600160801                ' and ATCest = ''S'' and ATCksc > 0 -
046700160801                 ORDER by ATCcmm';
046800160801
046900160801       //?Dichiarazione cursore
047000160801         exec sql
047100160801         prepare S2 from :wSQL;
047200160801         exec sql
047300160801         DECLARE A2 CURSOR for S2;
047400160801
047500160801       //?Apertura del cursore
047600160801         exec sql
047700160801         OPEN A2;
047800160801
047900160801         IF sqlcode < 0;
048000160801           wFine = *on;
048100160801           exec sql
048200160801           CLOSE A2;
048300160801           leavesr;
048400160801         ENDIF;
048500160801
048600160801         DOW  not wFine;
048700160801           exec sql
048800160801           FETCH NEXT from A2 into :ATCcmm, :ATCnrv, :ATCcpo, :ATCksc;
048900160801           IF sqlcod = 100 or sqlcod < 0;
049000160801             wFine = *on;
049100160801             leave;
049200160801           ENDIF;
049300160801
049400160801       //?Conto gli appuntamenti
049500160801           exsr ContaApp;
049600160801         ENDDO;
049700160801
049800160801       //?Rottura tipo visita
049900160801         exsr Rot_TPV;
050000160801
050100160801       //?Chiusura del cursore
050200160801         exec sql
050300160801         CLOSE A2;
050400160801
050500160729       ENDSR;
050600160801
050700160801       //--------------------------------------------------------------
050800160801       //?Offerte ITALIA.
050900160801       //--------------------------------------------------------------
051000160801       BEGSR NEWoff;
051100160801
051200160801         clear savcmm;
051300160801         clear savtpv;
051400160801         wFine = *off;
051500160801
051600160801       //?Offerte Italia
051700160801         wSQL = 'SELECT distinct min(VOFdpo), VOFnrv -
051800160801                 FROM TIVOF11L -
051900160801                 WHERE VOFtpt = ''I'' -
052000160801                 GROUP by VOFnrv -
052100160801                 ORDER by VOFnrv';
052200160801
052300160801       //?Dichiarazione cursore
052400160801         exec sql
052500160801         prepare S3 from :wSQL;
052600160801         exec sql
052700160801         DECLARE A3 CURSOR for S3;
052800160801
052900160801       //?Apertura del cursore
053000160801         exec sql
053100160801         OPEN A3;
053200160801
053300160801         IF sqlcode < 0;
053400160801           wFine = *on;
053500160801           exec sql
053600160801           CLOSE A3;
053700160801           leavesr;
053800160801         ENDIF;
053900160801
054000160801         DOW  not wFine;
054100160801           exec sql
054200160801           FETCH NEXT from A3 into :VOFdpo, :VOFnrv;
054300160801           IF sqlcod = 100 or sqlcod < 0;
054400160801             wFine = *on;
054500160801             leave;
054600160801           ENDIF;
054700160801
054800160801         //?Solo la prima trattativa nella data elaborazione
054900160801           wAAMMvof = %dec(%subst(%editc(VOFdpo:'X'):1:6):6:0);
055000160801           IF  wAAMMvof = wAAMM;
055100160801             chain (VOFnrv) TIVIS05L;
055200160801             IF  %found(TIVIS05L) and VISffz = *blanks;
055300160801           //?Rottura tipo visita per Rottura Commerciale
055400160801               IF  VIScmm <> savcmm;
055500160801                 IF  savcmm > 0;
055600160801                   exsr Rot_TPV;
055700160801                 ENDIF;
055800160801                 savcmm = VIScmm;
055900160801               ENDIF;
056000160801           //?Rottura tipo visita
056100160801           //?o per cliente > 0 se tipo visita vale per entambi
056200160801               IF  VIStpv <> savtpv or
056300160801                  (VisNewOld and ATCksc > 0);
056400160801                 exsr Rot_TPV;
056500160801               ENDIF;
056600160801
056700160801               savksc = VISksc;
056800160801               savcpo = VIScpo;
056900160801               savnrv = VISnrv;
057000160801               exsr ImpCliente;
057100160801
057200160801               TO2(yy) += 1;
057300160801
057400160801             ENDIF;
057500160801           ENDIF;
057600160801
057700160801         ENDDO;
057800160801
057900160801       //?Rottura tipo visita
058000160801         exsr Rot_TPV;
058100160801
058200160801       //?Chiusura del cursore
058300160801         exec sql
058400160801         CLOSE A3;
058500160801
058600160801       ENDSR;
058700160801
058800160801       //--------------------------------------------------------------
058900160801       //?Offerte ESTERO.
059000160801       //--------------------------------------------------------------
059100160801       BEGSR NEWestoff;
059200160801
059300160801         clear savcmm;
059400160801         clear savtpv;
059500160801         wFine = *off;
059600160801
059700160801       //?Offerte Estero
059800160801         wSQL = 'SELECT distinct min(VOFdpo), VOFnrv -
059900160801                 FROM TIVOF11L -
060000160801                 WHERE VOFtpt <> ''I'' -
060100160801                 GROUP by VOFnrv -
060200160801                 ORDER by VOFnrv';
060300160801
060400160801       //?Dichiarazione cursore
060500160801         exec sql
060600160801         prepare S4 from :wSQL;
060700160801         exec sql
060800160801         DECLARE A4 CURSOR for S4;
060900160801
061000160801       //?Apertura del cursore
061100160801         exec sql
061200160801         OPEN A4;
061300160801
061400160801         IF sqlcode < 0;
061500160801           wFine = *on;
061600160801           exec sql
061700160801           CLOSE A4;
061800160801           leavesr;
061900160801         ENDIF;
062000160801
062100160801         DOW  not wFine;
062200160801           exec sql
062300160801           FETCH NEXT from A4 into :VOFdpo, :VOFnrv;
062400160801           IF sqlcod = 100 or sqlcod < 0;
062500160801             wFine = *on;
062600160801             leave;
062700160801           ENDIF;
062800160801
062900160801         //?Solo la prima trattativa nella data elaborazione
063000160801           wAAMMvof = %dec(%subst(%editc(VOFdpo:'X'):1:6):6:0);
063100160801           IF  wAAMMvof = wAAMM;
063200160801             chain (VOFnrv) TIVIS05L;
063300160801             IF  %found(TIVIS05L) and VISffz = *blanks;
063400160801           //?Rottura tipo visita per Rottura Commerciale
063500160801               IF  VIScmm <> savcmm;
063600160801                 IF  savcmm > 0;
063700160801                   exsr Rot_TPV;
063800160801                 ENDIF;
063900160801                 savcmm = VIScmm;
064000160801               ENDIF;
064100160801           //?Rottura tipo visita
064200160801           //?o per cliente > 0 se tipo visita vale per entambi
064300160801               IF  VIStpv <> savtpv or
064400160801                  (VisNewOld and ATCksc > 0);
064500160801                 exsr Rot_TPV;
064600160801               ENDIF;
064700160801
064800160801               savksc = VISksc;
064900160801               savcpo = VIScpo;
065000160801               savnrv = VISnrv;
065100160801               exsr ImpCliente;
065200160801
065300160801               OFE(yy) += 1;
065400160801
065500160801             ENDIF;
065600160801           ENDIF;
065700160801
065800160801         ENDDO;
065900160801
066000160801       //?Rottura tipo visita
066100160801         exsr Rot_TPV;
066200160801
066300160801       //?Chiusura del cursore
066400160801         exec sql
066500160801         CLOSE A4;
066600160801
066700160801       ENDSR;
066800160801
066900160801       //--------------------------------------------------------------
067000160801       //?Offerte ITALIA Convalidate.
067100160801       //--------------------------------------------------------------
067200160801       BEGSR NEWconval;
067300160801
067400160801         clear savcmm;
067500160801         clear savtpv;
067600160801         wFine = *off;
067700160801
067800160801       //?Offerte Estero
067900160801         wSQL = 'SELECT distinct min(VOFdco), VOFnrv -
068000160801                 FROM TIVOF11L -
068100160801                 WHERE VOFtpt = ''I'' and VOFeso = ''C'' -
068200160801                 GROUP by VOFnrv -
068300160801                 ORDER by VOFnrv';
068400160801
068500160801       //?Dichiarazione cursore
068600160801         exec sql
068700160801         prepare S5 from :wSQL;
068800160801         exec sql
068900160801         DECLARE A5 CURSOR for S5;
069000160801
069100160801       //?Apertura del cursore
069200160801         exec sql
069300160801         OPEN A5;
069400160801
069500160801         IF sqlcode < 0;
069600160801           wFine = *on;
069700160801           exec sql
069800160801           CLOSE A5;
069900160801           leavesr;
070000160801         ENDIF;
070100160801
070200160801         DOW  not wFine;
070300160801           exec sql
070400160801           FETCH NEXT from A5 into :VOFdco, :VOFnrv;
070500160801           IF sqlcod = 100 or sqlcod < 0;
070600160801             wFine = *on;
070700160801             leave;
070800160801           ENDIF;
070900160801
071000160801         //?Solo la prima trattativa nella data elaborazione
071100160801           wAAMMvof = %dec(%subst(%editc(VOFdco:'X'):1:6):6:0);
071200160801           IF  wAAMMvof = wAAMM;
071300160801             chain (VOFnrv) TIVIS05L;
071400160801             IF  %found(TIVIS05L) and VISffz = *blanks;
071500160801           //?Rottura tipo visita per Rottura Commerciale
071600160801               IF  VIScmm <> savcmm;
071700160801                 IF  savcmm > 0;
071800160801                   exsr Rot_TPV;
071900160801                 ENDIF;
072000160801                 savcmm = VIScmm;
072100160801               ENDIF;
072200160801           //?Rottura tipo visita
072300160801           //?o per cliente > 0 se tipo visita vale per entambi
072400160801               IF  VIStpv <> savtpv or
072500160801                  (VisNewOld and ATCksc > 0);
072600160801                 exsr Rot_TPV;
072700160801               ENDIF;
072800160801
072900160801               savksc = VISksc;
073000160801               savcpo = VIScpo;
073100160801               savnrv = VISnrv;
073200160801               exsr ImpCliente;
073300160801
073400160801               TO3(yy) += 1;
073500160801
073600160801             ENDIF;
073700160801           ENDIF;
073800160801
073900160801         ENDDO;
074000160801
074100160801       //?Rottura tipo visita
074200160801         exsr Rot_TPV;
074300160801
074400160801       //?Chiusura del cursore
074500160801         exec sql
074600160801         CLOSE A5;
074700160801
074800160801       ENDSR;
074900160801
075000160801       //--------------------------------------------------------------
075100160801       //?Offerte ESTERO Convalidate.
075200160801       //--------------------------------------------------------------
075300160801       BEGSR NEWestcon;
075400160801
075500160801         clear savcmm;
075600160801         clear savtpv;
075700160801         wFine = *off;
075800160801
075900160801       //?Offerte Estero
076000160801         wSQL = 'SELECT distinct min(VOFdco), VOFnrv -
076100160801                 FROM TIVOF11L -
076200160801                 WHERE VOFtpt <> ''I'' and VOFeso = ''C'' -
076300160801                 GROUP by VOFnrv -
076400160801                 ORDER by VOFnrv';
076500160801
076600160801       //?Dichiarazione cursore
076700160801         exec sql
076800160801         prepare S6 from :wSQL;
076900160801         exec sql
077000160801         DECLARE A6 CURSOR for S6;
077100160801
077200160801       //?Apertura del cursore
077300160801         exec sql
077400160801         OPEN A6;
077500160801
077600160801         IF sqlcode < 0;
077700160801           wFine = *on;
077800160801           exec sql
077900160801           CLOSE A6;
078000160801           leavesr;
078100160801         ENDIF;
078200160801
078300160801         DOW  not wFine;
078400160801           exec sql
078500160801           FETCH NEXT from A6 into :VOFdco, :VOFnrv;
078600160801           IF sqlcod = 100 or sqlcod < 0;
078700160801             wFine = *on;
078800160801             leave;
078900160801           ENDIF;
079000160801
079100160801         //?Solo la prima trattativa nella data elaborazione
079200160801           wAAMMvof = %dec(%subst(%editc(VOFdco:'X'):1:6):6:0);
079300160801           IF  wAAMMvof = wAAMM;
079400160801             chain (VOFnrv) TIVIS05L;
079500160801             IF  %found(TIVIS05L) and VISffz = *blanks;
079600160801           //?Rottura tipo visita per Rottura Commerciale
079700160801               IF  VIScmm <> savcmm;
079800160801                 IF  savcmm > 0;
079900160801                   exsr Rot_TPV;
080000160801                 ENDIF;
080100160801                 savcmm = VIScmm;
080200160801               ENDIF;
080300160801           //?Rottura tipo visita
080400160801           //?o per cliente > 0 se tipo visita vale per entambi
080500160801               IF  VIStpv <> savtpv or
080600160801                  (VisNewOld and ATCksc > 0);
080700160801                 exsr Rot_TPV;
080800160801               ENDIF;
080900160801
081000160801               savksc = VISksc;
081100160801               savcpo = VIScpo;
081200160801               savnrv = VISnrv;
081300160801               exsr ImpCliente;
081400160801
081500160801               COE(yy) += 1;
081600160801
081700160801             ENDIF;
081800160801           ENDIF;
081900160801
082000160801         ENDDO;
082100160801
082200160801       //?Rottura tipo visita
082300160801         exsr Rot_TPV;
082400160801
082500160801       //?Chiusura del cursore
082600160801         exec sql
082700160801         CLOSE A6;
082800160801
082900160801       ENDSR;
083000160801
083100160801       //--------------------------------------------------------------
083200160801       //?Conto Appuntamenti di TIATC.
083300160801       //--------------------------------------------------------------
083400160801       BEGSR ContaApp;
083500160801
083600160801         chain (ATCnrv) TIVIS05L;
083700160801         IF  not %found(TIVIS05L);
083800160801           IF  ATCksc = 0;
083900160801             VIStpv = 'N';
084000160801           ELSE;
084100160801             VIStpv = 'A';
084200160801           ENDIF;
084300160801         ENDIF;
084400160801
084500160801       //?Rottura tipo visita per Rottura Commerciale
084600160801         IF  ATCcmm <> savcmm;
084700160801           IF  savcmm > 0;
084800160801             exsr Rot_TPV;
084900160801           ENDIF;
085000160801           savcmm = ATCcmm;
085100160801         ENDIF;
085200160801
085300160801       //?Rottura tipo visita
085400160801       //?o per cliente > 0 se tipo visita vale per entambi
085500160801         IF  VIStpv <> savtpv or
085600160801            (VisNewOld and ATCksc > 0);
085700160801           exsr Rot_TPV;
085800160801         ENDIF;
085900160801
086000160801         savksc = ATCksc;
086100160801         savcpo = ATCcpo;
086200160801         savnrv = ATCnrv;
086300160801         exsr ImpCliente;
086400160801
086500160801         TO1(yy) += 1;
086600160801
086700160801       ENDSR;
086800160801
086900160801       //--------------------------------------------------------------
087000160801       //?Rottura tipo visita.
087100160801       //--------------------------------------------------------------
087200160801       BEGSR Rot_TPV;
087300160801
087400160801         IF  savtpv <> *blanks;
087500160801           clear TFSAC000;
087600160801           SACcmm = savcmm;
087700160801           SACaaa = wAnno;
087800160801           SACmmm = wMese;
087900160801           IF  wApl = 'N';
088000160801             SACapl = wApl;
088100160801           ELSE;
088200160801             SACapl = 'C';
088300160801           ENDIF;
088400160801           SACtpv = savtpv;
088500160801           clear SACftr;
088600160801           SACdtr = Oggi;
088700160801
088800160801           FOR yy = 1 to %elem(sk_IC);
088900160801         //?Scrivo le visite in un rcd a parte
089000160801             IF  TO1(yy) > 0;
089100160801               wWrite = '1 ';
089200160801               exsr Scrivi;
089300160801             ENDIF;
089400160801             IF  TO2(yy) > 0 or TO3(yy) > 0;
089500160801               wWrite = '2 ';
089600160801               exsr Scrivi;
089700160801             ENDIF;
089800160801             IF  OFE(yy) > 0 or COE(yy) > 0;
089900160801               wWrite = '2E';
090000160801               exsr Scrivi;
090100160801             ENDIF;
090200160801           ENDFOR;
090300160801         ENDIF;
090400160801
090500160801       //?Se il tipo visita è valido sia per i clienti nuovi che per
090600160801       //?i non nuovi ma cambia subito il tipo visita
090700160801       //?non eseguo il 2 giro
090800160801         IF  VisNewOld and VIStpv <> savtpv;
090900160801           VisNewOld = *off;
091000160801         ENDIF;
091100160801
091200160801       //?Nuovo tipo Visita
091300160801         IF  not VisNewOld;
091400160801           clear TIBS02DS;
091500160801           clear dTTR;
091600160801           T02mod = 'C';
091700160801           T02cod = 'TTR';
091800160801           T02sif = knsif;
091900160801           T02ke1 = VIStpv;
092000160801           TNTBE_RicercaControllo (kpjba:tibs02ds);
092100160801           IF  T02err = *blanks;
092200160801             dTTR = T02uni;
092300160801           ENDIF;
092400160801
092500160801         //?Se tipo visita valido per clienti nuovi e non devo fare 2 giri
092600160801           IF  §TTRvcn = 'E';
092700160801           //?prima elaboro sempre i clienti nuovi
092800160801             wApl = 'N';
092900160801             VisNewOld = *on;
093000160801           ELSE;
093100160801             wApl = §TTRvcn;
093200160801           ENDIF;
093300160801         ELSE;
093400160801           wApl = 'O';
093500160801           VisNewOld = *off;
093600160801         ENDIF;
093700160801
093800160801       //?Pulizia Schiere
093900160801         clear TO1;
094000160801         clear TO2;
094100160801         clear TO3;
094200160801         clear OFE;
094300160801         clear COE;
094400160801
094500160801         savtpv = VIStpv;
094600160801
094700160801       ENDSR;
094800160801
094900160801       //--------------------------------------------------------------
095000160801       //?Cerco Importanza Cliente.
095100160801       //--------------------------------------------------------------
095200160801       BEGSR ImpCliente;
095300160801
095400160801         clear wclv;
095500160801
095600160801       //?Cerco su anagrafica provvisoria
095700160801         chain (k_kut:DUTkci:savnrv) TFCLP00F;
095800160801         IF  %found(TFCLP00F) and CLPclv <> *blanks;
095900160801           wclv = CLPclv;
096000160801         ENDIF;
096100160801
096200160801       //?Cerco su anagrafica PdC
096300160801         IF  wclv = *blanks and savksc > 0;
096400160802           chain (k_kut:DUTkci:savksc) CNCLP00F;
096500160802           IF  %found(CNCLP00F) and CLPclv <> *blanks;
096600160801             wclv = CLPclv;
096700160801           ENDIF;
096800160801         ENDIF;
096900160801
097000160801       //?Cerco su anagrafica Potenziale
097100160801         IF  wclv = *blanks and savcpo > 0;
097200160801           chain (ATCcpo) TNCPO01L;
097300160801           IF  %found(TNCPO01L);
097400160801             wclv = CPOftr;
097500160801           ENDIF;
097600160801         ENDIF;
097700160801
097800160801       //?se non l'ho trovata su nessun archivio imposto 'C' di default
097900160801         IF  wclv = *blanks;
098000160801           wclv = 'C';
098100160801         ENDIF;
098200160801
098300160801         yy = %lookup(wclv:sk_IC);
098400160801
098500160801       ENDSR;
098600160801
098700160801       //--------------------------------------------------------------
098800160801       //?Scrivo TFSAC.
098900160801       //--------------------------------------------------------------
099000160801       BEGSR Scrivi;
099100160801
099200160801         clear SACto1;
099300160801         clear SACto2;
099400160801         clear SACto3;
099500160801         clear SACto4;
099600160801         clear SACfie;
099700160801         clear SACffz;
099800160801
099900160801       //?Tipologia cliente
100000160801         SACclv = sk_IC(yy);
100100160801
100200160801       //?Memorizza i dati
100300160801         SELECT;
100400160801       //?Visite Tariffe Buone
100500160801         WHEN  wWrite = '1 ';
100600160801           IF  Aggio;
100700160801             k_SACcmm = SACcmm;
100800160801             k_SACaaa = SACaaa;
100900160801             k_SACmmm = SACmmm;
101000160801             k_SACapl = SACapl;
101100160801             k_SACtpv = SACtpv;
101200160801             k_SACclv = SACclv;
101300160801             k_SACfie = SACfie;
101400160801             k_SACffz = SACffz;
101500160801             chain %kds(K01tfsac) TFSAC01L;
101600160801           ENDIF;
101700160801           IF  not Aggio or not %found(TFSAC01L);
101800160801             SACto1 = TO1(yy);
101900160801           ELSE;
102000160801             SACto1 += TO1(yy);
102100160801           ENDIF;
102200160801       //?Offerte e convalide buone Italia
102300160801         WHEN  wWrite = '2 ';
102400160801           SACfie = 'I';
102500160801           IF  Aggio;
102600160801             k_SACcmm = SACcmm;
102700160801             k_SACaaa = SACaaa;
102800160801             k_SACmmm = SACmmm;
102900160801             k_SACapl = SACapl;
103000160801             k_SACtpv = SACtpv;
103100160801             k_SACclv = SACclv;
103200160801             k_SACfie = SACfie;
103300160801             k_SACffz = SACffz;
103400160801             chain %kds(K01tfsac) TFSAC01L;
103500160801           ENDIF;
103600160801           IF  not Aggio or not %found(TFSAC01L);
103700160801             SACto2 = TO2(yy);
103800160801             SACto3 = TO3(yy);
103900160801           ELSE;
104000160801             SACto2 += TO2(yy);
104100160801             SACto3 += TO3(yy);
104200160801           ENDIF;
104300160801       //?Offerte e convalide buone Estere
104400160801         WHEN  wWrite = '2E';
104500160801           SACfie = 'E';
104600160801           IF  Aggio;
104700160801             k_SACcmm = SACcmm;
104800160801             k_SACaaa = SACaaa;
104900160801             k_SACmmm = SACmmm;
105000160801             k_SACapl = SACapl;
105100160801             k_SACtpv = SACtpv;
105200160801             k_SACclv = SACclv;
105300160801             k_SACfie = SACfie;
105400160801             k_SACffz = SACffz;
105500160801             chain %kds(K01tfsac) TFSAC01L;
105600160801           ENDIF;
105700160801           IF  not Aggio or not %found(TFSAC01L);
105800160801             SACto2 = OFE(yy);
105900160801             SACto3 = COE(yy);
106000160801           ELSE;
106100160801             SACto2 += OFE(yy);
106200160801             SACto3 += COE(yy);
106300160801           ENDIF;
106400160801         ENDSL;
106500160801
106600160801         IF  not Aggio or not %found(TFSAC01L);
106700160801           SACcmm = k_SACcmm;
106800160801           SACaaa = k_SACaaa;
106900160801           SACmmm = k_SACmmm;
107000160801           SACapl = k_SACapl;
107100160801           SACtpv = k_SACtpv;
107200160801           SACclv = k_SACclv;
107300160801           SACfie = k_SACfie;
107400160801           SACffz = k_SACffz;
107500160801           write TFSAC000;
107600160801         ELSE;
107700160801           update TFSAC000;
107800160801         ENDIF;
107900160801
108000160801       ENDSR;
108100160729
108200160729       //--------------------------------------------------------------
108300160729       //?Operazioni finali.
108400160729       //--------------------------------------------------------------
108500160729       BEGSR RoutEnd;
108600160729
108700160729         *inLR = *on;
108800160729         return;
108900160729
109000160729       ENDSR;
109100160729
109200160729      /end-free
