000100101108      //--------------------------------------------------------------
000200101108      //?TRMKQ1R - Creezione file Qlickview - Attività
000300101108      //--------------------------------------------------------------
000400101108     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000500101108
000600101108      //---------------------------------------------------------------
000700101108      //?Dichiarazione file.
000800101108      //---------------------------------------------------------------
000900101108
001000101108      // - File Organigramma
001100101108     fAZORG01L  if   e           k disk
001200130918      // - Anagrafica Commerciali
001300130918     fAZCMM01L  if   e           k disk
001400101108      // - File attività
001500101108     fTIATC01L  uf   e           k disk
001600101108      // - File per QL
001700101108     fTIATCE0F  o    e             disk
001800101108     fTIATCI0F  o    e             disk
001900140912     fTICPIT1L  uf a e           k disk
002000101110      // - File Trattative
002100110803     fTIVIS05L  if   e           k disk
002200101108      // - File Potenziali
002300101108     fTNCPO01L  if   e           k disk
002400101108      // - Nuove Tabelle
002500101108     fTNTBE01L  if   e           k disk
002600101108
002700101108      //---------------------------------------------------------------
002800101108      //?Definizione costanti.
002900101108      //---------------------------------------------------------------
003000101108
003100101108      //---------------------------------------------------------------
003200101108      //?Definizione schiere.
003300101108      //---------------------------------------------------------------
003400101108
003500101108      // - Codice commerciale
003600101108     d $AGE            s              7  0 dim(9000)
003700101108      // - Codice unificante
003800101108     d $AGEU           s              7  0 dim(9000)
003900101108
004000101108      // - Tipo attività da elaborare
004100101108     d $TTA            s              1    dim(10)
004200101108
004300101108      //---------------------------------------------------------------
004400101108      //?Definizione aree dati.
004500101108      //---------------------------------------------------------------
004600101108
004700101108      //---------------------------------------------------------------
004800101108      //?Definizione strutture dati.
004900101108      //---------------------------------------------------------------
005000101108
005100101108      // - File Attività
005200101108     d ATCDS         e ds                  extname(TIATC00F)
005300101108     d                                     prefix(w_)
005400101110
005500101110      // - File Info Commerciali
005600101110     d CPIDS         e ds                  extname(TICPI00F)
005700101108
005800101108      // - FLO del file attività
005900101108     d dATC01        e ds                  inz
006000101111
006100101111      // - IFS motivo non affidamento
006200101111     d dIFS          e ds                  inz
006300101108
006400101108      // - Tabella TTA = Tipo Attività
006500101108     d dtta          e ds                  inz
006600101110
006700101110      // - Parametri ricevuti
006800101110     d KPJBA         e ds
006900101108
007000101108       // - Reperimento dati anagrafici
007100101108     d TIBS69ds      e ds
007200101108     d DS_cnaco      e ds                  inz extname(CNACO00F)
007300101108     d DS_cnind      e ds                  inz extname(CNIND00F)
007400101108     d DS_cnclp      e ds                  inz extname(CNCLP00F)
007500101108     d DS_fncls      e ds                  inz extname(FNCLS00F)
007600101108
007700101108      //---------------------------------------------------------------
007800101108      //?Definizione variabili globali.
007900101108      //---------------------------------------------------------------
008000101108
008100101108      // - Flags booleani
008200101110     d $EndAtt         s               n   inz(*off)
008300101110     d $EndCpi         s               n   inz(*off)
008400101110     d $NoOkRcd        s               n   inz(*off)
008500101111     d $OkInfo         s               n   inz(*off)
008600101108
008700101108      // - Indici di schiera
008800101108     d xx              s              4  0 inz
008900101108     d yy              s              4  0 inz
009000101108
009100101108      // - Campi di comodo
009200101110     d wage            s                   like(ATCcmm)
009300110118     d wcap            s                   like(ATEcap)
009400110118     d wclv            s                   like(ATEclv)
009500110118     d wctm            s                   like(ATEctm)
009600101108     d wc05age         s                   like(ATEcmar)
009700101108     d wc17age         s                   like(ATEcmdi)
009800101108     d wdesage         s                   like(ATEcmmd)
009900101108     d wfilage         s                   like(ATEcmfi)
010000101108     d wfunage         s                   like(ATEcmfu)
010100110118     d wrag            s                   like(ATErag)
010200101108
010300101108       // - Stringhe SQL da eseguire
010400101108     d wSQL            s           2048    Varying        inz
010500101108
010600101108      //---------------------------------------------------------------
010700101108      //?Definizione procedure usate.
010800101108      //---------------------------------------------------------------
010900101108
011000101108      //---------------------------------------------------------------
011100101108      //?Prototipi usati.
011200101108      //---------------------------------------------------------------
011300101108
011400101108      /copy gaitrasrc/srcprotopr,tibs69r
011500101108
011600101108      //---------------------------------------------------------------
011700101108      //?Definizione key-list.
011800101108      //---------------------------------------------------------------
011900101108
012000101108
012100101108      //---------------------------------------------------------------
012200101108      //?Riepilogo indicatori.
012300101108      //---------------------------------------------------------------
012400101108
012500101108      //---------------------------------------------------------------
012600101108      //?M A I N - L I N E
012700101108      //---------------------------------------------------------------
012800101110
012900101110     c     *Entry        plist
013000101110     c                   parm                    KPJBA
013100101108
013200101108      /free
013300101108
013400101108       //?Operazioni iniziali
013500101108       exsr RoutInz;
013600101108
013700101110       exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
013800101110       $EndAtt = *off;
013900101108
014000101108       //?Imposto la stringa SQL
014100101108       exsr PrepSql;
014200101108
014300101108       //?Preparo il cursore
014400101108       exec sql prepare S1 from :wSQL;
014500101108
014600101108       exec sql declare ATT cursor for S1;
014700101108
014800101108       //?Apro il cursore
014900101108       exec sql open ATT;
015000101108       IF  sqlcode < 0;
015100101110         $EndAtt = *on;
015200101108       ENDIF;
015300101108
015400101108       //?Leggo le attività non ancora elaborate
015500101110       DOW  not $EndAtt;
015600101108         exec sql fetch next from ATT into :ATCDS;
015700101108         IF  sqlcod = 100 or sqlcod < 0;
015800101110           $EndAtt = *on;
015900101108           leave;
016000101108         ENDIF;
016100101108
016200101108         //?Elaboro il record
016300101108         exsr Elabora;
016400101108
016500101108       ENDDO;
016600101108
016700101108       exec sql close ATT;
016800101108
016900101108       //?Operazioni finali
017000101108       exsr RoutEnd;
017100101108
017200101108       //--------------------------------------------------------------
017300101108       //?Operazioni iniziali.
017400101108       //--------------------------------------------------------------
017500101108       BEGSR RoutInz;
017600101108
017700101108         //?Tipo attività da elaborare
017800101108         clear xx;
017900101108         setll ('TTA') TNTBE01L;
018000101108         reade ('TTA') TNTBE01L;
018100101108
018200101108         DOW not %eof(TNTBE01L);
018300101108           IF  TBEatb = *blanks;
018400101108             dTTA = TBEuni;
018500101108             IF  §TTAleg = 'S';
018600101108               xx += 1;
018700101108               $TTA(xx) = TBEke1;
018800101108             ENDIF;
018900101108           ENDIF;
019000101108           reade ('TTA') TNTBE01L;
019100101108         ENDDO;
019200101108
019300101108         //?Codici commerciali + codice unificante
019400101108         clear xx;
019500130918         setll  (*loval)  AZCMM000;
019600130918         read   AZCMM000;
019700101108
019800130918         DOW not %eof(AZCMM01L);
019900130918           xx += 1;
020000130918           $AGE(xx)  = CMMcod;
020100130918           $AGEU(xx) = CMMuni;
020200130918           read  AZCMM000;
020300101108         ENDDO;
020400101108
020500101108       ENDSR;
020600101108
020700101108       //--------------------------------------------------------------
020800101110       //?Imposto la stringa SQL.
020900101108       //--------------------------------------------------------------
021000101108       BEGSR PrepSql;
021100101108
021200101108         clear wSQL;
021300101110         xx = 1;
021400101110         yy = 0;
021500101108
021600101108         wSQL = 'select * from TIATC00F where ATCtas = '' '' and +
021700101108                 ATCtat in(';
021800101108         FOR xx by 1 to %elem($TTA);
021900101108           IF $TTA(xx) <> *blanks;
022000101108             IF yy > 0;
022100101108               wSQL += ', ';
022200101108             ELSE;
022300101108               yy = 1;
022400101108             ENDIF;
022500101108             wSQL += '''';
022600101108             wSQL += $TTA(xx);
022700101108             wSQL += '''';
022800101108           ENDIF;
022900101108         ENDFOR;
023000101108
023100101110         wSQL += ') order by ATCatn, ATCatnp for fetch only';
023200101108
023300101108       ENDSR;
023400101108
023500101108       //--------------------------------------------------------------
023600101108       //?Elaborazione record.
023700101108       //--------------------------------------------------------------
023800101108       BEGSR Elabora;
023900101108
024000101108         dATC01 = w_ATCflo;
024100101108
024200101108       //?Attività concluse
024300101108         IF  w_ATCdco > 0;
024400101108           exsr Concluse;
024500101108         ENDIF;
024600101108
024700101108       //?Attività in movimento
024800101108         IF  w_ATCdco = 0;
024900101108           exsr Movimento;
025000101108         ENDIF;
025100101108
025200101108       ENDSR;
025300101108
025400101108       //--------------------------------------------------------------
025500101108       //?Scrivo il file delle attività concluse.
025600101108       //--------------------------------------------------------------
025700101108       BEGSR Concluse;
025800101124
025900101124       //?Controllo subito se il rcd dell'attività è allocato
026000101124         chain(e) (w_ATCtat:w_ATCatn:w_ATCatnp) TIATC01L;
026100101124       //?se allocato esco dalla routine così leggo altra attività
026200101124       //?questa attività la scriverò alla prossima elaborazione
026300101124         IF  %error;
026400101124           leavesr;
026500101124         ENDIF;
026600101108
026700101110         $NoOkRcd = *off;
026800101110         clear TIATCE00;
026900101108
027000101110       //?Controllo se trattativa fittizia, in questo caso non devo scrivere il rcd
027100101116       //?ma flaggo lo stesso come rcd elaborato così non lo elaboro più
027200101110         exsr CtrNrv;
027300101110         IF  $NoOkRcd;
027400101116           IF  %found(TIATC01L);
027500101116             ATCtas = 'S';
027600101116             update TIATC000;
027700101116           ENDIF;
027800101110           leavesr;
027900101110         ENDIF;
028000101110
028100101108         ATEatn  = w_ATCatn;
028200101108         ATEatnp = w_ATCatnp;
028300101108         ATEtat  = w_ATCtat;
028400101222         ATEdim  = w_ATCdim;
028500101222         ATEhim  = w_ATChim;
028600101108         ATEdad  = w_ATCdad;
028700101108         ATEhda  = w_ATChda;
028800101108         ATEcad  = w_ATCcad;
028900101108
029000101108       //?Cerco commerciale unificante del commerciale che deve
029100101108       //?eseguire l'attività
029200101108         IF  w_ATCcmm > 0;
029300101110           wage = w_ATCcmm;
029400101110           xx =  %lookup(wage:$AGE);
029500101108           IF  xx > 0;
029600101108             ATEcmm = $AGEU(xx);
029700101110           ELSE;
029800101110             ATEcmm = wage;
029900101108           ENDIF;
030000101108           exsr DecAgeU;
030100101108           ATEcmfu = wfunage;
030200101108           ATEcmmd = wdesage;
030300101108           ATEcmfi = wfilage;
030400101108           ATEcmar = wc05age;
030500101108           ATEcmdi = wc17age;
030600101108         ENDIF;
030700101108
030800101108       //?Se attività Appuntamento imposto anche chi ha inserito l'attività
030900101108         IF  w_ATCtat = 'A';
031000101110           wage = w_ATCco3;
031100101110           xx =  %lookup(wage:$AGE);
031200101108           IF  xx > 0;
031300101108             ATEco3 = $AGEU(xx);
031400101110           ELSE;
031500101110             ATEco3 = wage;
031600101108           ENDIF;
031700101108           exsr DecAgeU;
031800101108           ATEcofu = wfunage;
031900101108           ATEco3d = wdesage;
032000101108           ATEcofi = wfilage;
032100101108           ATEcoar = wc05age;
032200101108           ATEcodi = wc17age;
032300101108         ENDIF;
032400101108
032500101108         ATEcnw = w_ATCcnw;
032600101108         ATEksc = w_ATCksc;
032700101108         ATEcpo = w_ATCcpo;
032800101108
032900101108       //?Dati cliente/potenziale
033000101108         exsr DecCli;
033100110118         ATErag = wrag;
033200110118         ATEclv = wclv;
033300110118         ATEctm = wctm;
033400110118         ATEcap = wcap;
033500101108
033600101108         ATEcpom = §ATCmaitr;
033700101110         ATEnrv = w_ATCnrv;
033800101110         IF  w_ATCnra > 0;
033900101110           ATEaptr = 'S';
034000101110           ATEnrv = w_ATCnra;
034100111013         //?Se attività che apre trattativa devo scrivere le info comm.
034200111013         //?Se già scritta aggiorno il potenziale e pulisco il tiket
034300140912           chain ATEnrv TICPIT1L;
034400140912           IF  %found(TICPIT1L);
034500111013             IF  CPQcpo <> ATEcpo;
034600111013               CPQcpo = ATEcpo;
034700111013               CPQtck = %char(%timestamp:*iso0);
034800111013             ENDIF;
034900140912               update TICPIT00;
035000111013         //?Se non trovo la info la scrivo
035100111013           ELSE;
035200111013             exsr ElabInfo;
035300111013           ENDIF;
035400101110         ENDIF;
035500101110
035600101110       //?Cerco commerciale unificante del commerciale che ha
035700101110       //?eseguito l'attività
035800101110         IF  w_ATCcco > 0;
035900101110           wage = w_ATCcco;
036000101110           xx =  %lookup(wage:$AGE);
036100101110           IF  xx > 0;
036200101110             ATEcco = $AGEU(xx);
036300101110           ELSE;
036400101110             ATEcco = wage;
036500101110           ENDIF;
036600101110           exsr DecAgeU;
036700101110           ATEccfu = wfunage;
036800101110           ATEccod = wdesage;
036900101110           ATEccfi = wfilage;
037000101110           ATEccar = wc05age;
037100101110           ATEccdi = wc17age;
037200101110         ENDIF;
037300101110
037400101110         ATEdco = w_ATCdco;
037500101110         ATEhco = w_ATChco;
037600101110         ATEcac = w_ATCcac;
037700101110
037800101110         IF  w_ATCtat = 'A';
037900101110           ATEesa = w_ATCest;
038000101110         ENDIF;
038100101110
038200101110         ATEest = §ATCesito;
038300110401         ATEcpo1 = §ATCcapo1;
038400110401         ATEcpo2 = §ATCcapo2;
038500101110
038600101110         ATEtck = %char(%timestamp:*iso0);
038700101110
038800101110         write TIATCE00;
038900101110
039000101110       //?Aggiorno rcd come già elaborato
039100101110         IF  %found(TIATC01L);
039200101110           ATCtas = 'S';
039300101110           update TIATC000;
039400101110         ENDIF;
039500101108
039600101108       ENDSR;
039700101108
039800101108       //--------------------------------------------------------------
039900101108       //?Scrivo il file delle attività in movimento.
040000101108       //--------------------------------------------------------------
040100101108       BEGSR Movimento;
040200101110
040300101110         $NoOkRcd = *off;
040400101110         clear TIATCI00;
040500101110
040600101110       //?Controllo se trattativa fittizia, in questo caso non devo scrivere il rcd
040700101110         exsr CtrNrv;
040800101110         IF  $NoOkRcd;
040900101110           leavesr;
041000101110         ENDIF;
041100101110
041200101110         ATIatn  = w_ATCatn;
041300101110         ATIatnp = w_ATCatnp;
041400101110         ATItat  = w_ATCtat;
041500101222         ATIdim  = w_ATCdim;
041600101222         ATIhim  = w_ATChim;
041700101110         ATIdad  = w_ATCdad;
041800101110         ATIhda  = w_ATChda;
041900101110         ATIcad  = w_ATCcad;
042000101110
042100101110       //?Cerco commerciale unificante del commerciale che deve
042200101110       //?eseguire l'attività
042300101110         IF  w_ATCcmm > 0;
042400101110           wage = w_ATCcmm;
042500101110           xx =  %lookup(wage:$AGE);
042600101110           IF  xx > 0;
042700101110             ATIcmm = $AGEU(xx);
042800101110           ELSE;
042900101110             ATIcmm = wage;
043000101110           ENDIF;
043100101110           exsr DecAgeU;
043200101110           ATIcmfu = wfunage;
043300101110           ATIcmmd = wdesage;
043400101110           ATIcmfi = wfilage;
043500101110           ATIcmar = wc05age;
043600101110           ATIcmdi = wc17age;
043700101110         ENDIF;
043800101110
043900101110       //?Se attività Appuntamento imposto anche chi ha inserito l'attività
044000101110         IF  w_ATCtat = 'A';
044100101110           wage = w_ATCco3;
044200101110           xx =  %lookup(wage:$AGE);
044300101110           IF  xx > 0;
044400101110             ATIco3 = $AGEU(xx);
044500101110           ELSE;
044600101110             ATIco3 = wage;
044700101110           ENDIF;
044800101110           exsr DecAgeU;
044900101110           ATIcofu = wfunage;
045000101110           ATIco3d = wdesage;
045100101110           ATIcofi = wfilage;
045200101110           ATIcoar = wc05age;
045300101110           ATIcodi = wc17age;
045400101110         ENDIF;
045500101110
045600101110         ATIcnw = w_ATCcnw;
045700101110         ATIksc = w_ATCksc;
045800101110         ATIcpo = w_ATCcpo;
045900101110
046000101110       //?Dati cliente/potenziale
046100101110         exsr DecCli;
046200110118         ATIrag = wrag;
046300110118         ATIclv = wclv;
046400110118         ATIctm = wctm;
046500110118         ATIcap = wcap;
046600101110
046700101110         ATInrv = w_ATCnrv;
046800110401         ATIcpo1 = §ATCcapo1;
046900110401         ATIcpo2 = §ATCcapo2;
047000101110
047100101110         write TIATCI00;
047200101108
047300101108       ENDSR;
047400101110
047500101110       //--------------------------------------------------------------
047600101110       //?Controllo se trattativa fittizia.
047700101110       //--------------------------------------------------------------
047800101110       BEGSR CtrNrv;
047900101110
048000101110         SELECT;
048100101110           WHEN  w_ATCnra > 0;
048200101110             chain w_ATCnra TIVIS05L;
048300101110           WHEN  w_ATCnrv > 0;
048400101110             chain w_ATCnrv TIVIS05L;
048500101110           OTHER;
048600101110             leavesr;
048700101110         ENDSL;
048800101110
048900101110           IF  %found(TIVIS05L) and VISffz = 'S';
049000101110             $NoOkRcd = *on;
049100101110           ENDIF;
049200101110
049300101110       ENDSR;
049400101108
049500101108       //--------------------------------------------------------------
049600101108       //?Decodifico agente unificante.
049700101108       //--------------------------------------------------------------
049800101108       BEGSR DecAgeU;
049900101108
050000101108         clear wdesage;
050100101108         clear wfunage;
050200101108         clear wfilage;
050300101108         clear wc17age;
050400101108         clear wc05age;
050500101110
050600101110         IF  xx > 0;
050700130918           chain  ($AGEU(xx))  AZCMM000;
050800130918           IF  %found( AZCMM01L );
050900130918             wdesage = CMMdes;
051000130918             wfunage = CMMfun;
051100101110           ENDIF;
051200101110           wage    = $AGEU(xx);
051300101110         ENDIF;
051400101108
051500101108       //?Filiale
051600101110         wfilage = %dec(%subst(%editc(wage:'X'):1:3):3:0);
051700101108         chain  wfilage AZORG01L;
051800101108         IF  %found(AZORG01L);
051900101108           wc17age = ORGfl3;
052000101108           wc05age = ORGcar;
052100101108         ENDIF;
052200101108
052300101108       ENDSR;
052400101108
052500101108       //--------------------------------------------------------------
052600101108       //?Decodifico cliente/potenziale.
052700101108       //--------------------------------------------------------------
052800101108       BEGSR DecCli;
052900110118
053000110118         clear wrag;
053100110118         clear wclv;
053200110118         clear wctm;
053300110118         clear wcap;
053400101108
053500101108       //?Se presente recupero i dati da anagrafica clienti
053600101108         IF  w_ATCksc > 0;
053700101108           clear  tibs69ds;
053800101108           I69kac = w_ATCksc;
053900101108           I69kin = w_ATCksc;
054000101108           I69kcp = w_ATCksc;
054100101108           TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
054200110118           wrag = ACOrag;
054300110118           wclv = CLPclv;
054400110118           wctm = ACOitc;
054500110118           wcap = INDcae;
054600101108         ELSE;
054700101108           chain w_ATCcpo TNCPO01L;
054800101108           IF  %found(TNCPO01L);
054900110118             wrag = CPOrag;
055000110118             wclv = CPOftr;
055100110118             wctm = CPOsct;
055200110118             wcap = CPOcap;
055300101108           ENDIF;
055400101108         ENDIF;
055500101108
055600101108       ENDSR;
055700101110
055800101110       //--------------------------------------------------------------
055900101110       //?Scrivo il file delle INFO commerciali.
056000101110       //--------------------------------------------------------------
056100101110       BEGSR ElabInfo;
056200101110
056300101110         $EndCpi = *off;
056400101111         $OKinfo = *off;
056500140912         clear TICPIT00;
056600101110
056700101110         exec sql declare CPI cursor for
056800101110         select * from TICPI00F
056900101110         where CPIcpo = :w_ATCcpo
057000101110         order by CPItpf;
057100101110
057200101110         exec sql open CPI;
057300101110         IF  sqlcode < 0;
057400101110           $EndCpi = *on;
057500101110         ENDIF;
057600101110
057700101110         DOW  not $EndCpi;
057800101110           exec sql fetch next from CPI into :CPIDS;
057900101110           IF  sqlcod = 100 or sqlcod < 0;
058000101110             $EndCpi = *on;
058100101110             leave;
058200101110           ENDIF;
058300101110
058400101110       //?Imposto i dati per scrivere il record
058500101111           CPQnrv = w_ATCnra;
058600101111           CPQcpo = w_ATCcpo;
058700101110
058800101111           IF  CPQdim <= CPIdim;
058900101111             CPQdim = CPIdim;
059000101111           ENDIF;
059100101110
059200101111           SELECT;
059300101110         //?Fatturato/Spedizioni totale
059400101111             WHEN  CPItpf = 'SPT';
059500101112               $OKinfo = *on;
059600101111               CPQfatt = CPIpft;
059700101111               CPQnspt = CPIsna;
059800101110         //?Ripartizioni del fatturato
059900101111             WHEN  CPItpf = 'PRF';
060000101112               $OKinfo = *on;
060100101111               CPQipf5 = CPIval;
060200101111             WHEN  CPItpf = 'PRO';
060300101112               $OKinfo = *on;
060400101111               CPQimes = CPIval;
060500101111             WHEN  CPItpf = 'AER';
060600101112               $OKinfo = *on;
060700101111               CPQeva = CPIval;
060800101111             WHEN  CPItpf = 'CAE';
060900101112               $OKinfo = *on;
061000101111               CPQesc = CPIval;
061100101111             WHEN  CPItpf = 'ATR';
061200101112               $OKinfo = *on;
061300101111               CPQaltro = CPIval;
061400140912         //?Fatturato non affidato a BRT
061500140912             WHEN  CPItpf = 'NRF';
061600140912               $OKinfo = *on;
061700140912               CPQnipf5 = CPIpft;
061800140912             WHEN  CPItpf = 'NRO';
061900140912               $OKinfo = *on;
062000140912               CPQnimes = CPIpft;
062100140912             WHEN  CPItpf = 'NER';
062200140912               $OKinfo = *on;
062300140912               CPQneva = CPIpft;
062400140912             WHEN  CPItpf = 'NAE';
062500140912               $OKinfo = *on;
062600140912               CPQnesc = CPIpft;
062700140912             WHEN  CPItpf = 'NTR';
062800140912               $OKinfo = *on;
062900140912               CPQnaltro = CPIpft;
063000101110         //?Caratteristiche delle spedizioni
063100101111             WHEN  CPItpf = 'PRI';
063200101112               $OKinfo = *on;
063300101111               CPQpriv = CPIval;
063400101111             WHEN  CPItpf = 'ECM';
063500101112               $OKinfo = *on;
063600101111               CPQecomm = CPIval;
063700101111             WHEN  CPItpf = 'GDO';
063800101112               $OKinfo = *on;
063900101111               CPQgdo = CPIval;
064000101111             WHEN  CPItpf = 'HPR';
064100101112               $OKinfo = *on;
064200101111               CPQprior = CPIval;
064300110712             WHEN  CPItpf = 'SBX' and CPIfsn = 'S';
064400110712               $OKinfo = *on;
064500110712               CPQsecbx = CPIfsn;
064600101117             WHEN  CPItpf = 'KMS';
064700101117               $OKinfo = *on;
064800101117               CPQpmas = CPIval;
064900101111         //?Concorrenti
065000101116             WHEN  CPItpf = '10' and CPIspf = '_BAR' and CPIval <> 0;
065100101112               $OKinfo = *on;
065200101111               CPQ_bar = CPIval;
065300101116             WHEN  CPItpf = '10' and CPIspf = '_MEP' and CPIval <> 0;
065400101112               $OKinfo = *on;
065500101111               CPQ_mep = CPIval;
065600101116             WHEN  CPItpf = '10' and CPIspf = 'ARCO' and CPIval <> 0;
065700101112               $OKinfo = *on;
065800101111               CPQarco = CPIval;
065900101116             WHEN  CPItpf = '10' and CPIspf = 'ARTO' and CPIval <> 0;
066000101112               $OKinfo = *on;
066100101111               CPQarto = CPIval;
066200101116             WHEN  CPItpf = '10' and CPIspf = 'AWS' and CPIval <> 0;
066300101112               $OKinfo = *on;
066400101111               CPQaws  = CPIval;
066500101116             WHEN  CPItpf = '10' and CPIspf = 'BARS' and CPIval <> 0;
066600101112               $OKinfo = *on;
066700101111               CPQbars = CPIval;
066800101116             WHEN  CPItpf = '10' and CPIspf = 'DHL' and CPIval <> 0;
066900101112               $OKinfo = *on;
067000101111               CPQdhl  = CPIval;
067100101116             WHEN  CPItpf = '10' and CPIspf = 'EXEC' and CPIval <> 0;
067200101112               $OKinfo = *on;
067300101111               CPQexec = CPIval;
067400140912             WHEN  CPItpf = '10' and CPIspf = 'FEDE' and CPIval <> 0;
067500140912               $OKinfo = *on;
067600140912               CPQfede = CPIval;
067700101116             WHEN  CPItpf = '10' and CPIspf = 'FERC' and CPIval <> 0;
067800101112               $OKinfo = *on;
067900101111               CPQferc = CPIval;
068000101116             WHEN  CPItpf = '10' and CPIspf = 'MAIL' and CPIval <> 0;
068100101112               $OKinfo = *on;
068200101111               CPQmail = CPIval;
068300101116             WHEN  CPItpf = '10' and CPIspf = 'MTN' and CPIval <> 0;
068400101112               $OKinfo = *on;
068500101111               CPQmtn  = CPIval;
068600101116             WHEN  CPItpf = '10' and CPIspf = 'ONEE' and CPIval <> 0;
068700101112               $OKinfo = *on;
068800101111               CPQonee = CPIval;
068900101116             WHEN  CPItpf = '10' and CPIspf = 'PALL' and CPIval <> 0;
069000101112               $OKinfo = *on;
069100101111               CPQpall = CPIval;
069200101116             WHEN  CPItpf = '10' and CPIspf = 'PALW' and CPIval <> 0;
069300101112               $OKinfo = *on;
069400101111               CPQpalw = CPIval;
069500140912             WHEN  CPItpf = '10' and CPIspf = 'POST' and CPIval <> 0;
069600140912               $OKinfo = *on;
069700140912               CPQpost = CPIval;
069800101116             WHEN  CPItpf = '10' and CPIspf = 'SDA' and CPIval <> 0;
069900101112               $OKinfo = *on;
070000101111               CPQsda  = CPIval;
070100101116             WHEN  CPItpf = '10' and CPIspf = 'SUSA' and CPIval <> 0;
070200101112               $OKinfo = *on;
070300101111               CPQsusa = CPIval;
070400101116             WHEN  CPItpf = '10' and CPIspf = 'TNT' and CPIval <> 0;
070500101112               $OKinfo = *on;
070600101111               CPQtnt  = CPIval;
070700140912             WHEN  CPItpf = '10' and CPIspf = 'TNTP' and CPIval <> 0;
070800140912               $OKinfo = *on;
070900140912               CPQtntp = CPIval;
071000101116             WHEN  CPItpf = '10' and CPIspf = 'UPS' and CPIval <> 0;
071100101112               $OKinfo = *on;
071200101111               CPQups  = CPIval;
071300101116             WHEN  CPItpf = '10' and CPIspf = '8NSC' and CPIval <> 0;
071400101112               $OKinfo = *on;
071500101111               CPQ8nsc = CPIval;
071600101116             WHEN  CPItpf = '10' and CPIspf = '9ATR' and CPIval <> 0;
071700101112               $OKinfo = *on;
071800101111               CPQ9atr = CPIval;
071900101111         //?Motivo di non affidamento
072000101111             WHEN  CPItpf = 'MOI' and CPIfsn = 'S' and CPQmoi1 = *blanks;
072100101112               $OKinfo = *on;
072200101111               clear dIFS;
072300101111               chain ('IFS' : CPItpf : CPIspf) TNTBE01L;
072400101111               IF  %found(TNTBE01L);
072500101111                 dIFS = TBEuni;
072600101111               ENDIF;
072700101111               CPQmoi1 = CPIspf + '-' + §IFSdes;
072800101111             WHEN  CPItpf = 'MOI' and CPIfsn = 'S' and CPQmoi1 <> *blanks and
072900101111                   CPQmoi2 = *blanks;
073000101112               $OKinfo = *on;
073100101111               clear dIFS;
073200101111               chain ('IFS' : CPItpf : CPIspf) TNTBE01L;
073300101111               IF  %found(TNTBE01L);
073400101111                 dIFS = TBEuni;
073500101111               ENDIF;
073600101111               CPQmoi2 = CPIspf + '-' + §IFSdes;
073700101111         //?Dati relativi alla logistica
073800101111             WHEN  CPItpf = 'LOG' and CPIfsn = 'S';
073900101112               $OKinfo = *on;
074000101111               CPQlog = 'S';
074100101111             WHEN  CPItpf = 'OUT' and CPIfsn = 'S';
074200101112               $OKinfo = *on;
074300101111               CPQout = 'S';
074400140912             WHEN  CPItpf = 'MQL' and CPIval <> 0;
074500140912               $OKinfo = *on;
074600140912               CPQmql = CPIval;
074700140912             WHEN  CPItpf = 'PPL' and CPIval <> 0;
074800140912               $OKinfo = *on;
074900140912               CPQppl = CPIval;
075000101111         //?Operatori di logistica
075100101111             WHEN  CPItpf = '40' and CPIspf = '_BAR';
075200101112               $OKinfo = *on;
075300101111               CPQl_bar = 'S';
075400101111             WHEN  CPItpf = '40' and CPIspf = 'ARTO';
075500101112               $OKinfo = *on;
075600101111               CPQlarto = 'S';
075700101111             WHEN  CPItpf = '40' and CPIspf = 'CEVA';
075800101112               $OKinfo = *on;
075900101111               CPQlceva = 'S';
076000140912             WHEN  CPItpf = '40' and CPIspf = 'CHIA';
076100140912               $OKinfo = *on;
076200140912               CPQlchia = 'S';
076300101111             WHEN  CPItpf = '40' and CPIspf = 'DSV';
076400101112               $OKinfo = *on;
076500101111               CPQldsv  = 'S';
076600101111             WHEN  CPItpf = '40' and CPIspf = 'EXEL';
076700101112               $OKinfo = *on;
076800101111               CPQlexel = 'S';
076900101111             WHEN  CPItpf = '40' and CPIspf = 'FERC';
077000101112               $OKinfo = *on;
077100101111               CPQlferc = 'S';
077200140912             WHEN  CPItpf = '40' and CPIspf = 'FMLO';
077300140912               $OKinfo = *on;
077400140912               CPQlfmlo = 'S';
077500101111             WHEN  CPItpf = '40' and CPIspf = 'GEOD';
077600101112               $OKinfo = *on;
077700101111               CPQlgeod = 'S';
077800101111             WHEN  CPItpf = '40' and CPIspf = 'KUNE';
077900101112               $OKinfo = *on;
078000101111               CPQlkune = 'S';
078100140912             WHEN  CPItpf = '40' and CPIspf = 'MOVI';
078200140912               $OKinfo = *on;
078300140912               CPQlmovi = 'S';
078400101111             WHEN  CPItpf = '40' and CPIspf = 'NORB';
078500101112               $OKinfo = *on;
078600101111               CPQlnorb = 'S';
078700101111             WHEN  CPItpf = '40' and CPIspf = 'NUMB';
078800101112               $OKinfo = *on;
078900101111               CPQlnumb = 'S';
079000101111             WHEN  CPItpf = '40' and CPIspf = 'SIFT';
079100101112               $OKinfo = *on;
079200101111               CPQlsift = 'S';
079300101111             WHEN  CPItpf = '40' and CPIspf = '8NSC';
079400101112               $OKinfo = *on;
079500101111               CPQl8nsc = 'S';
079600101111             WHEN  CPItpf = '40' and CPIspf = '9ATR';
079700101112               $OKinfo = *on;
079800101111               CPQl9atr = 'S';
079900140912         //?Paesi Estero
080000140912             WHEN  CPItpf = '50' and CPIspf = '0005';
080100140912               $OKinfo = *on;
080200140912               CPQpd = 'S';
080300140912             WHEN  CPItpf = '50' and CPIspf = '0010';
080400140912               $OKinfo = *on;
080500140912               CPQpf = 'S';
080600140912             WHEN  CPItpf = '50' and CPIspf = '0015';
080700140912               $OKinfo = *on;
080800140912               CPQpe = 'S';
080900140912             WHEN  CPItpf = '50' and CPIspf = '0020';
081000140912               $OKinfo = *on;
081100140912               CPQpb = 'S';
081200140912             WHEN  CPItpf = '50' and CPIspf = '0025';
081300140912               $OKinfo = *on;
081400140912               CPQpa = 'S';
081500140912             WHEN  CPItpf = '50' and CPIspf = '0030';
081600140912               $OKinfo = *on;
081700140912               CPQpslo = 'S';
081800140912             WHEN  CPItpf = '50' and CPIspf = '0035';
081900140912               $OKinfo = *on;
082000140915               CPQpch = 'S';
082100140912             WHEN  CPItpf = '50' and CPIspf = '0040';
082200140912               $OKinfo = *on;
082300140912               CPQpgb = 'S';
082400140912             WHEN  CPItpf = '50' and CPIspf = '0045';
082500140912               $OKinfo = *on;
082600140912               CPQps = 'S';
082700140912             WHEN  CPItpf = '50' and CPIspf = '0050';
082800140912               $OKinfo = *on;
082900140912               CPQpest = 'S';
083000140912             WHEN  CPItpf = '50' and CPIspf = '0055';
083100140912               $OKinfo = *on;
083200140912               CPQpgr = 'S';
083300140912             WHEN  CPItpf = '50' and CPIspf = '0060';
083400140912               $OKinfo = *on;
083500140912               CPQpru = 'S';
083600140912             WHEN  CPItpf = '50' and CPIspf = '0065';
083700140912               $OKinfo = *on;
083800140912               CPQptr = 'S';
083900140912             WHEN  CPItpf = '50' and CPIspf = '1000';
084000140912               $OKinfo = *on;
084100140912               CPQpanord = 'S';
084200140912             WHEN  CPItpf = '50' and CPIspf = '1010';
084300140912               $OKinfo = *on;
084400140912               CPQpasud = 'S';
084500140912             WHEN  CPItpf = '50' and CPIspf = '2005';
084600140912               $OKinfo = *on;
084700140912               CPQpasia = 'S';
084800140912             WHEN  CPItpf = '50' and CPIspf = '3000';
084900140912               $OKinfo = *on;
085000140912               CPQpother = 'S';
085100101111           ENDSL;
085200101110         ENDDO;
085300101111
085400101111       //?Scrivo il rcd delle info commerciali
085500101111         IF  $OKinfo;
085600101122           CPQtck = %char(%timestamp:*iso0);
085700140912           write  TICPIT00;
085800101111         ENDIF;
085900101110
086000101110         exec sql close CPI;
086100101110
086200101110       ENDSR;
086300101108
086400101108       //--------------------------------------------------------------
086500101108       //?Operazioni finali.
086600101108       //--------------------------------------------------------------
086700101108       BEGSR RoutEnd;
086800101108
086900101108         *inLR = *on;
087000101108         return;
087100101108
087200101108       ENDSR;
087300101108
087400101108      /end-free
