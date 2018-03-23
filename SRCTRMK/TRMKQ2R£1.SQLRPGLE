000100101111      //--------------------------------------------------------------
000200101111      //?TRMKQ2R - Creezione file Qlickview - Trattative
000300101111      //--------------------------------------------------------------
000400101111     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000500101111
000600101111      //---------------------------------------------------------------
000700101111      //?Dichiarazione file.
000800101111      //---------------------------------------------------------------
000900101111
001000101111      // - File Organigramma
001100101111     fAZORG01L  if   e           k disk
001200101111      // - File Tabelle
001300101111     fTABEL00F  if   e           k disk
001400101111      // - File per QL
001500101111     fTIVIIC0F  o    e             disk
001600101111     fTIVIII0F  o    e             disk
001700101111      // - File Trattative
001800110803     fTIVIS05L  uf   e           k disk
001900101111      // - File per QL
002000101111     fTIVISC0F  o    e             disk
002100101111     fTIVISI0F  o    e             disk
002200101111     fTIVOFC0F  o    e             disk
002300101111     fTIVOFI0F  o    e             disk
002400101111      // - Nuove Tabelle
002500101111     fTNTBE01L  if   e           k disk
002600101111
002700101111      //---------------------------------------------------------------
002800101111      //?Definizione costanti.
002900101111      //---------------------------------------------------------------
003000101111
003100101111      //---------------------------------------------------------------
003200101111      //?Definizione schiere.
003300101111      //---------------------------------------------------------------
003400101111
003500101111      // - Codice commerciale
003600101111     d $AGE            s              7  0 dim(9000)
003700101111      // - Codice unificante
003800101111     d $AGEU           s              7  0 dim(9000)
003900101111
004000101111      //---------------------------------------------------------------
004100101111      //?Definizione aree dati.
004200101111      //---------------------------------------------------------------
004300101111
004400101111      //---------------------------------------------------------------
004500101111      //?Definizione strutture dati.
004600101111      //---------------------------------------------------------------
004700101111
004800101111      // - Tabella 01 = Codici Commerciali
004900101111     d ds01          e ds                  inz
005000101111
005100101111      // - Parametri ricevuti
005200101111     d KPJBA         e ds
005300101111
005400101111      // - File Info trattative
005500101111     d VIIDS         e ds                  extname(TIVII00F)
005600101111     d                                     prefix(w_)
005700101111
005800101111      // - File trattative
005900101111     d VISDS         e ds                  extname(TIVIS00F)
006000101111     d                                     prefix(w_)
006100101111
006200101111      // - File offerte
006300101111     d VOFDS         e ds                  extname(TIVOF00F)
006400101111
006500101111      // - FLO del file trattative
006600101111     d dVIS01        e ds                  inz
006700110331
006800110331       // - Reperimento dati anagrafici
006900110331     d TIBS69ds      e ds
007000110331     d DS_cnaco      e ds                  inz extname(CNACO00F)
007100110331     d DS_cnind      e ds                  inz extname(CNIND00F)
007200110331     d DS_cnclp      e ds                  inz extname(CNCLP00F)
007300110331     d DS_fncls      e ds                  inz extname(FNCLS00F)
007400101111
007500101111      //---------------------------------------------------------------
007600101111      //?Definizione variabili globali.
007700101111      //---------------------------------------------------------------
007800101111
007900101111      // - Flags booleani
008000101111     d $EndVii         s               n   inz(*off)
008100101111     d $EndVis         s               n   inz(*off)
008200101111     d $EndVof         s               n   inz(*off)
008300101111     d $OkInfo         s               n   inz(*off)
008400101111
008500101111      // - Indici di schiera
008600101111     d xx              s              4  0 inz
008700101111     d yy              s              4  0 inz
008800101111
008900101111      // - Campi di comodo
009000101111     d wage            s                   like(VIScmm)
009100110331     d wcap            s                   like(VOCcap)
009200110331     d wclv            s                   like(VOCclv)
009300110331     d wctm            s                   like(VOCctm)
009400101111     d wc05age         s                   like(VICcmar)
009500101111     d wc17age         s                   like(VICcmdi)
009600101111     d wdesage         s                   like(VICcmmd)
009700101111     d wfilage         s                   like(VICcmfi)
009800101111     d wfunage         s                   like(VICcmfu)
009900110331     d wrag            s                   like(VOCrag)
010000101111
010100101111
010200101111       // - Stringhe SQL da eseguire
010300101111     d wSQL            s           2048    Varying        inz
010400101111     d wSQLa           s           2048    Varying        inz
010500101111
010600101111      //---------------------------------------------------------------
010700101111      //?Definizione procedure usate.
010800101111      //---------------------------------------------------------------
010900101111
011000101111      //---------------------------------------------------------------
011100101111      //?Prototipi usati.
011200101111      //---------------------------------------------------------------
011300110331
011400110331      /copy gaitrasrc/srcprotopr,tibs69r
011500101111
011600101111      //---------------------------------------------------------------
011700101111      //?Definizione key-list.
011800101111      //---------------------------------------------------------------
011900101111
012000101111       // - File TABEL00F
012100101111     d k03tabel      e ds                  extname(TABEL00F:*key)
012200101111     d                                     prefix(k_)
012300101111     d                                     inz
012400101111
012500101111      //---------------------------------------------------------------
012600101111      //?Riepilogo indicatori.
012700101111      //---------------------------------------------------------------
012800101111
012900101111      //---------------------------------------------------------------
013000101111      //?M A I N - L I N E
013100101111      //---------------------------------------------------------------
013200101111
013300101111     c     *Entry        plist
013400101111     c                   parm                    KPJBA
013500101111
013600101111      /free
013700101111
013800101111       //?Operazioni iniziali
013900101111       exsr RoutInz;
014000101111
014100101111       exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
014200101111       $EndVis = *off;
014300101111
014400101111       //?Imposto la stringa SQL
014500101111       exsr PrepSql;
014600101111
014700101111       //?Preparo il cursore
014800101111       exec sql prepare S1 from :wSQL;
014900101111
015000101111       exec sql declare VIS cursor for S1;
015100101111
015200101111       //?Apro il cursore
015300101111       exec sql open VIS;
015400101111       IF  sqlcode < 0;
015500101111         $EndVis = *on;
015600101111       ENDIF;
015700101111
015800101111       //?Leggo le trattative ancora da elaborare
015900101111       DOW  not $EndVis;
016000101111         exec sql fetch next from VIS into :VISDS;
016100101111         IF  sqlcod = 100 or sqlcod < 0;
016200101111           $EndVis = *on;
016300101111           leave;
016400101111         ENDIF;
016500101111
016600101111         //?Elaboro il record
016700101111         exsr Elabora;
016800101111
016900101111       ENDDO;
017000101111
017100101111       exec sql close VIS;
017200101111
017300101111       //?Operazioni finali
017400101111       exsr RoutEnd;
017500101111
017600101111       //--------------------------------------------------------------
017700101111       //?Operazioni iniziali.
017800101111       //--------------------------------------------------------------
017900101111       BEGSR RoutInz;
018000101111
018100101111         //?Imposto campi fissi
018200101111         k_TBLkut = 1;
018300101111         k_TBLcod = '01';
018400101111
018500101111         //?Codici commerciali + codice unificante
018600101111         clear xx;
018700101111         setll  %kds(k03tabel:2) TABEL00F;
018800101111         reade  %kds(k03tabel:2) TABEL00F;
018900101111
019000101111         DOW not %eof(TABEL00F);
019100101111           IF  TBLflg = *blanks;
019200101111             ds01 = TBLuni;
019300101111             xx += 1;
019400101111             $AGE(xx) = %dec(%subst(TBLkey:1:7):7:0);
019500101111             $AGEU(xx) = §01rgf;
019600101111           ENDIF;
019700101111           reade  %kds(k03tabel:2) TABEL00F;
019800101111         ENDDO;
019900101111
020000101111       ENDSR;
020100101111
020200101111       //--------------------------------------------------------------
020300101111       //?Imposto la stringa SQL.
020400101111       //--------------------------------------------------------------
020500101111       BEGSR PrepSql;
020600101111
020700101111         clear wSQL;
020800101111
020900101123         wSQL = 'select * from ';
021000101123
021100101123
021200110803         wSQL += 'TIVIS00F ';
021300101123         wSQL += 'where VISffz = '' '' and ';
021400101123         wSQL += 'substr(VISflo, 2, 1) = '' '' ';
021500101123         wSQL += 'order by VISnrv';
021600101111
021700101111       ENDSR;
021800101111
021900101111       //--------------------------------------------------------------
022000101111       //?Elaborazione record.
022100101111       //--------------------------------------------------------------
022200101111       BEGSR Elabora;
022300101111
022400101111         dVIS01 = w_VISflo;
022500101111
022600101111       //?Trattative concluse
022700101111         IF  w_VISdch > 0;
022800101108           exsr Concluse;
022900101108         ENDIF;
023000101108
023100101111       //?Trattative in corso
023200101111         IF  w_VISdch = 0;
023300101111           exsr InCorso;
023400101108         ENDIF;
023500101108
023600101108       ENDSR;
023700101108
023800101108       //--------------------------------------------------------------
023900101111       //?Scrivo i file delle trattative concluse.
024000101108       //--------------------------------------------------------------
024100101108       BEGSR Concluse;
024200101124
024300101124       //?Controllo subito se il rcd della trattativa è allocato
024400101124         chain(e) (w_VISnrv) TIVIS05L;
024500101124       //?se allocato esco dalla routine così leggo altra trattativa
024600101124       //?questa trattativa la scriverò alla prossima elaborazione
024700101124         IF  %error;
024800101124           leavesr;
024900101124         ENDIF;
025000101108
025100101111         clear TIVISC00;
025200101110
025300101111         VICnrv  = w_VISnrv;
025400101111         VICtpv  = w_VIStpv;
025500101111         VICdat  = w_VISdat;
025600101108
025700101111       //?Cerco commerciale unificante del commerciale gestione trattativa
025800101111         IF  w_VIScmm > 0;
025900101111           wage = w_VIScmm;
026000101110           xx =  %lookup(wage:$AGE);
026100101108           IF  xx > 0;
026200101111             VICcmm = $AGEU(xx);
026300101110           ELSE;
026400101111             VICcmm = wage;
026500101108           ENDIF;
026600101108           exsr DecAgeU;
026700101111           VICcmfu = wfunage;
026800101111           VICcmmd = wdesage;
026900101111           VICcmfi = wfilage;
027000101111           VICcmar = wc05age;
027100101111           VICcmdi = wc17age;
027200101108         ENDIF;
027300101111
027400101111       //?Controllo se trattativa con offerte
027500101111       //?e scrivo relativo file se presenti offerte convalidate
027600101111         exsr CtrVof;
027700101111
027800101111         VICdch  = w_VISdch;
027900101111         VICesi  = w_VISesi;
028000101111         VICesi2 = §VISesito;
028100110401         VICcpot = §VIScatpo;
028200101110
028300101111         VICtck  = %char(%timestamp:*iso0);
028400101110
028500101111         write TIVISC00;
028600101110
028700101110       //?Aggiorno rcd come già elaborato
028800101111         IF  %found(TIVIS05L);
028900101111           dVIS01 = VISflo;
029000101111           §VISelaql = 'S';
029100101111           VISflo = dVIS01;
029200101111           update TIVIS000;
029300101110         ENDIF;
029400101111
029500101111       //?Scrivo file info trattative
029600101111         exsr InfoVIC;
029700101108
029800101108       ENDSR;
029900101108
030000101108       //--------------------------------------------------------------
030100101111       //?Scrivo il file delle trattative in corso.
030200101108       //--------------------------------------------------------------
030300101111       BEGSR InCorso;
030400101110
030500101111         clear TIVISI00;
030600101111
030700101111         VIInrv  = w_VISnrv;
030800101111         VIItpv  = w_VIStpv;
030900101111         VIIdat  = w_VISdat;
031000101111
031100101111       //?Cerco commerciale unificante del commerciale gestione trattativa
031200101111         IF  w_VIScmm > 0;
031300101111           wage = w_VIScmm;
031400101111           xx =  %lookup(wage:$AGE);
031500101111           IF  xx > 0;
031600101111             VIIcmm = $AGEU(xx);
031700101111           ELSE;
031800101111             VIIcmm = wage;
031900101111           ENDIF;
032000101111           exsr DecAgeU;
032100101111           VIIcmfu = wfunage;
032200101111           VIIcmmd = wdesage;
032300101111           VIIcmfi = wfilage;
032400101111           VIIcmar = wc05age;
032500101111           VIIcmdi = wc17age;
032600101111         ENDIF;
032700101111
032800101111       //?Controllo se trattativa con offerte
032900101111       //?e scrivo relativo file se presenti offerte convalidate
033000101111         exsr CtrVof;
033100101111
033200110401         VIIcpot = §VIScatpo;
033300101111
033400101111         write TIVISI00;
033500101111
033600101111       //?Scrivo file info trattative
033700101111         exsr InfoVII;
033800101108
033900101108       ENDSR;
034000101108
034100101108       //--------------------------------------------------------------
034200101108       //?Decodifico agente unificante.
034300101108       //--------------------------------------------------------------
034400101108       BEGSR DecAgeU;
034500101108
034600101108         clear wdesage;
034700101108         clear wfunage;
034800101108         clear wfilage;
034900101108         clear wc17age;
035000101108         clear wc05age;
035100101110
035200101110         IF  xx > 0;
035300101110           clear ds01;
035400101110           k_tblcod = '01';
035500101110           k_tblkey = %editc($AGEU(xx):'X');
035600101110           chain  %kds(K03tabel) TABEL00F;
035700101110           IF  %found( TABEL00F );
035800101110             ds01 = TBLuni;
035900101110           ENDIF;
036000101110           wdesage = §01age;
036100101110           wfunage = §01fun;
036200101110           wage    = $AGEU(xx);
036300101110         ENDIF;
036400101108
036500101108       //?Filiale
036600101110         wfilage = %dec(%subst(%editc(wage:'X'):1:3):3:0);
036700101108         chain  wfilage AZORG01L;
036800101108         IF  %found(AZORG01L);
036900101108           wc17age = ORGfl3;
037000101108           wc05age = ORGcar;
037100101108         ENDIF;
037200101108
037300101108       ENDSR;
037400101111
037500101111       //--------------------------------------------------------------
037600101111       //?Controllo se trattativa con offerte.
037700101111       //--------------------------------------------------------------
037800101111       BEGSR CtrVof;
037900101111
038000101111         $EndVof = *off;
038100101111
038200101111         wSQLa = 'select * from ';
038300101111
038400101111
038500110803         wSQLa += 'TIVOF00F ';
038600101111         wSQLa += 'where VOFnrv = ' + %editc(w_VISnrv:'X') +
038700101111                  ' order by VOFctr, VOFprg';
038800101111
038900101111         exec sql prepare S2 from :wSQLa;
039000101111
039100101111         exec sql declare VOF cursor for S2;
039200101111
039300101111         exec sql open VOF;
039400101111
039500101111         IF  sqlcode < 0;
039600101111           $EndVof = *on;
039700101111         ENDIF;
039800101111
039900101111         DOW  not $EndVof;
040000101111           exec sql fetch next from VOF into :VOFDS;
040100101111           IF  sqlcod = 100 or sqlcod < 0;
040200101111             $EndVof = *on;
040300101111             leave;
040400101111           ENDIF;
040500101111
040600101111         //?Imposto i dati relativi alla presenza di offerte
040700101111           SELECT;
040800101111             WHEN  VOFtpt = 'I' and w_VISdch > 0;
040900101111               VICpoi = 'S';
041000101111             WHEN  VOFtpt = 'I' and w_VISdch = 0;
041100101111               VIIpoi = 'S';
041200101111             WHEN  VOFtpt = 'E' and w_VISdch > 0;
041300101112               VICpoe = 'S';
041400101111             WHEN  VOFtpt = 'E' and w_VISdch = 0;
041500101112               VIIpoe = 'S';
041600101111             WHEN  VOFtpt = 'D' and w_VISdch > 0;
041700101112               VICpod = 'S';
041800101111             WHEN  VOFtpt = 'D' and w_VISdch = 0;
041900101112               VIIpod = 'S';
042000101111             WHEN  VOFtpt = 'F' and w_VISdch > 0;
042100101112               VICpof = 'S';
042200101111             WHEN  VOFtpt = 'F' and w_VISdch = 0;
042300101112               VIIpof = 'S';
042400101111           ENDSL;
042500110331
042600110331         //?Recupero i dati anagrafici
042700120126           clear wrag;
042800120126           clear wclv;
042900120126           clear wctm;
043000120126           clear wcap;
043100110331           IF  VOFksc > 0;
043200110331             clear  tibs69ds;
043300110331             I69kac = VOFksc;
043400110331             I69kin = VOFksc;
043500110331             I69kcp = VOFksc;
043600110331             TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
043700110331             wrag = ACOrag;
043800110331             wclv = CLPclv;
043900110331             wctm = ACOitc;
044000110331             wcap = INDcae;
044100110331           ENDIF;
044200101111
044300101111         //?Scrivo i file delle offerte x QL
044400101111         //?se sto elaborando le trattative concluse
044500101111         //?devo considerare solo le offerte convalidate
044600101111           IF  w_VISdch > 0 and VOFeso = 'C';
044700101111             clear TIVOFC00;
044800101111             VOCnrv = VOFnrv;
044900101111             VOCctr = VOFctr;
045000101111             VOCprg = VOFprg;
045100101111             VOCtpt = VOFtpt;
045200101111             VOCdpo = VOFdpo;
045300101111             VOCdpa = VOFdpa;
045400101111             VOCdra = VOFdra;
045500101111             VOCdco = VOFdco;
045600101111             VOCksc = VOFksc;
045700110331             VOCrag = wrag;
045800110331             VOCclv = wclv;
045900110331             VOCctm = wctm;
046000110331             VOCcap = wcap;
046100101111             VOCtck = %char(%timestamp:*iso0);
046200101111             write TIVOFC00;
046300101111           ENDIF;
046400101111
046500101116         //?se sto elaborando le trattative in corso
046600101111         //?devo considerare solo le offerte convalidate e/o accettate
046700101111           IF  w_VISdch = 0 and (VOFeso = 'C' or VOFeso = 'A');
046800101111             clear TIVOFI00;
046900101111             VOInrv = VOFnrv;
047000101111             VOIctr = VOFctr;
047100101111             VOIprg = VOFprg;
047200101111             VOItpt = VOFtpt;
047300101111             VOIdpo = VOFdpo;
047400101111             VOIeso = VOFeso;
047500101111             VOIdpa = VOFdpa;
047600101111             VOIdra = VOFdra;
047700101111             VOIdco = VOFdco;
047800101111             VOIksc = VOFksc;
047900110331             VOIrag = wrag;
048000110331             VOIclv = wclv;
048100110331             VOIctm = wctm;
048200110331             VOIcap = wcap;
048300101111             write TIVOFI00;
048400101111           ENDIF;
048500101111
048600101111         ENDDO;
048700101111
048800101111         exec sql close VOF;
048900101111
049000101111       ENDSR;
049100101111
049200101111       //--------------------------------------------------------------
049300101111       //?Scrivo file Info trattative concluse.
049400101111       //--------------------------------------------------------------
049500101111       BEGSR InfoVIC;
049600101111
049700101111         $EndVii = *off;
049800101111         $OKinfo = *off;
049900101111         clear TIVIIC00;
050000101111
050100101111         wSQLa = 'select * from ';
050200101111
050300110927         wSQLa += 'TIVII00F ';
050400101111
050500101111         wSQLa += 'where VIInrv = ' + %editc(w_VISnrv:'X') +
050600101111                  ' order by VIItpf';
050700101111
050800101111         exec sql prepare S3 from :wSQLa;
050900101111
051000101111         exec sql declare VIIC cursor for S3;
051100101111
051200101111         exec sql open VIIC;
051300101111
051400101110         IF  sqlcode < 0;
051500101111           $EndVii = *on;
051600101110         ENDIF;
051700101110
051800101111         DOW  not $EndVii;
051900101111           exec sql fetch next from VIIC into :VIIDS;
052000101110           IF  sqlcod = 100 or sqlcod < 0;
052100101111             $EndVii = *on;
052200101110             leave;
052300101110           ENDIF;
052400101110
052500101110       //?Imposto i dati per scrivere il record
052600101111           $OKinfo = *on;
052700101111           VICnrv = w_VIInrv;
052800101110
052900101111           SELECT;
053000101110         //?Fatturato/Spedizioni totale
053100101111             WHEN  w_VIItpf = 'TRT';
053200101111               VICfatt = w_VIIpft;
053300101111               VICnspt = w_VIIsna;
053400101111         //?Fatturato/Spedizioni estero
053500101111             WHEN  w_VIItpf = 'TRE';
053600101111               VICfate = w_VIIpft;
053700101111               VICnspe = w_VIIsna;
053800110203         //?Aumento/Sconto
053900110203             WHEN  w_VIItpf = 'A/S';
054000110504               VICausc = w_VIIvald;
054100101111         //?Delta
054200101111             WHEN  w_VIItpf = 'DEL';
054300101111               VICdel = w_VIIval;
054400101111         //?Peso
054500101111             WHEN  w_VIItpf = 'KMS';
054600101111               VICpkg = w_VIIval;
054700101111         //?Concorrenti
054800101111             WHEN  w_VIItpf = '10' and w_VIIspf = 'ARCO';
054900101111               VICarco = 'S';
055000101111             WHEN  w_VIItpf = '10' and w_VIIspf = 'ARTO';
055100101111               VICarto = 'S';
055200101111             WHEN  w_VIItpf = '10' and w_VIIspf = 'AWS';
055300101111               VICaws  = 'S';
055400101111             WHEN  w_VIItpf = '10' and w_VIIspf = 'BARS';
055500101111               VICbars = 'S';
055600101111             WHEN  w_VIItpf = '10' and w_VIIspf = 'DHL';
055700101111               VICdhl  = 'S';
055800101111             WHEN  w_VIItpf = '10' and w_VIIspf = 'EXEC';
055900101111               VICexec = 'S';
056000101111             WHEN  w_VIItpf = '10' and w_VIIspf = 'FERC';
056100101111               VICferc = 'S';
056200101111             WHEN  w_VIItpf = '10' and w_VIIspf = 'MAIL';
056300101111               VICmail = 'S';
056400101111             WHEN  w_VIItpf = '10' and w_VIIspf = 'MEP';
056500101111               VICmep  = 'S';
056600101111             WHEN  w_VIItpf = '10' and w_VIIspf = 'MTN';
056700101111               VICmtn  = 'S';
056800101111             WHEN  w_VIItpf = '10' and w_VIIspf = 'ONEE';
056900101111               VIConee = 'S';
057000101111             WHEN  w_VIItpf = '10' and w_VIIspf = 'PALL';
057100101111               VICpall = 'S';
057200101111             WHEN  w_VIItpf = '10' and w_VIIspf = 'PALW';
057300101111               VICpalw = 'S';
057400101111             WHEN  w_VIItpf = '10' and w_VIIspf = 'SDA';
057500101111               VICsda  = 'S';
057600101111             WHEN  w_VIItpf = '10' and w_VIIspf = 'SUSA';
057700101111               VICsusa = 'S';
057800101111             WHEN  w_VIItpf = '10' and w_VIIspf = 'TNT';
057900101111               VICtnt  = 'S';
058000101111             WHEN  w_VIItpf = '10' and w_VIIspf = 'UPS';
058100101111               VICups  = 'S';
058200101111             WHEN  w_VIItpf = '10' and w_VIIspf = '9910';
058300101111               VICaltr = 'S';
058400101111             WHEN  w_VIItpf = '10' and w_VIIspf = '8810';
058500101111               VICness = 'S';
058600101111           ENDSL;
058700101110         ENDDO;
058800101111
058900101111       //?Scrivo il rcd delle info commerciali
059000101111         IF  $OKinfo;
059100101111           VICtck = %char(%timestamp:*iso0);
059200101111           write  TIVIIC00;
059300101111         ENDIF;
059400101110
059500101111         exec sql close VIIC;
059600101110
059700101110       ENDSR;
059800101111
059900101111       //--------------------------------------------------------------
060000101111       //?Scrivo file Info trattative in corso.
060100101111       //--------------------------------------------------------------
060200101111       BEGSR InfoVII;
060300101111
060400101111         $EndVii = *off;
060500101111         $OKinfo = *off;
060600101111         clear TIVIII00;
060700101111
060800101111         wSQLa = 'select * from ';
060900101111
061000110927         wSQLa += 'TIVII00F ';
061100101111
061200101111         wSQLa += 'where VIInrv = ' + %editc(w_VISnrv:'X') +
061300101111                  ' order by VIItpf';
061400101111
061500101111         exec sql prepare S4 from :wSQLa;
061600101111
061700101111         exec sql declare VIII cursor for S4;
061800101111
061900101111         exec sql open VIII;
062000101111
062100101111         IF  sqlcode < 0;
062200101111           $EndVii = *on;
062300101111         ENDIF;
062400101111
062500101111         DOW  not $EndVii;
062600101111           exec sql fetch next from VIII into :VIIDS;
062700101111           IF  sqlcod = 100 or sqlcod < 0;
062800101111             $EndVii = *on;
062900101111             leave;
063000101111           ENDIF;
063100101111
063200101111       //?Imposto i dati per scrivere il record
063300101111           $OKinfo = *on;
063400101111           VIInrv = w_VIInrv;
063500101111
063600101111           SELECT;
063700101111         //?Fatturato/Spedizioni totale
063800101111             WHEN  w_VIItpf = 'TRT';
063900101111               VIIfatt = w_VIIpft;
064000101111               VIInspt = w_VIIsna;
064100101111         //?Fatturato/Spedizioni estero
064200101111             WHEN  w_VIItpf = 'TRE';
064300101111               VIIfate = w_VIIpft;
064400101111               VIInspe = w_VIIsna;
064500110203         //?Aumento/Sconto
064600110203             WHEN  w_VIItpf = 'A/S';
064700110504               VIIausc = w_VIIvald;
064800101111         //?Delta
064900101111             WHEN  w_VIItpf = 'DEL';
065000101111               VIIdel = w_VIIval;
065100101111         //?Peso
065200101111             WHEN  w_VIItpf = 'KMS';
065300101111               VIIpkg = w_VIIval;
065400101111         //?Concorrenti
065500101111             WHEN  w_VIItpf = '10' and w_VIIspf = 'ARCO';
065600101111               VIIarco = 'S';
065700101111             WHEN  w_VIItpf = '10' and w_VIIspf = 'ARTO';
065800101111               VIIarto = 'S';
065900101111             WHEN  w_VIItpf = '10' and w_VIIspf = 'AWS';
066000101111               VIIaws  = 'S';
066100101111             WHEN  w_VIItpf = '10' and w_VIIspf = 'BARS';
066200101111               VIIbars = 'S';
066300101111             WHEN  w_VIItpf = '10' and w_VIIspf = 'DHL';
066400101111               VIIdhl  = 'S';
066500101111             WHEN  w_VIItpf = '10' and w_VIIspf = 'EXEC';
066600101111               VIIexec = 'S';
066700101111             WHEN  w_VIItpf = '10' and w_VIIspf = 'FERC';
066800101111               VIIferc = 'S';
066900101111             WHEN  w_VIItpf = '10' and w_VIIspf = 'MAIL';
067000101111               VIImail = 'S';
067100101111             WHEN  w_VIItpf = '10' and w_VIIspf = 'MEP';
067200101111               VIImep  = 'S';
067300101111             WHEN  w_VIItpf = '10' and w_VIIspf = 'MTN';
067400101111               VIImtn  = 'S';
067500101111             WHEN  w_VIItpf = '10' and w_VIIspf = 'ONEE';
067600101111               VIIonee = 'S';
067700101111             WHEN  w_VIItpf = '10' and w_VIIspf = 'PALL';
067800101111               VIIpall = 'S';
067900101111             WHEN  w_VIItpf = '10' and w_VIIspf = 'PALW';
068000101111               VIIpalw = 'S';
068100101111             WHEN  w_VIItpf = '10' and w_VIIspf = 'SDA';
068200101111               VIIsda  = 'S';
068300101111             WHEN  w_VIItpf = '10' and w_VIIspf = 'SUSA';
068400101111               VIIsusa = 'S';
068500101111             WHEN  w_VIItpf = '10' and w_VIIspf = 'TNT';
068600101111               VIItnt  = 'S';
068700101111             WHEN  w_VIItpf = '10' and w_VIIspf = 'UPS';
068800101111               VIIups  = 'S';
068900101111             WHEN  w_VIItpf = '10' and w_VIIspf = '9910';
069000101111               VIIaltr = 'S';
069100101111             WHEN  w_VIItpf = '10' and w_VIIspf = '8810';
069200101111               VIIness = 'S';
069300101111           ENDSL;
069400101111         ENDDO;
069500101111
069600101111       //?Scrivo il rcd delle info commerciali
069700101111         IF  $OKinfo;
069800101111           write  TIVIII00;
069900101111         ENDIF;
070000101111
070100101111         exec sql close VIII;
070200101111
070300101111       ENDSR;
070400101108
070500101108       //--------------------------------------------------------------
070600101108       //?Operazioni finali.
070700101108       //--------------------------------------------------------------
070800101108       BEGSR RoutEnd;
070900101108
071000101108         *inLR = *on;
071100101108         return;
071200101108
071300101108       ENDSR;
071400101108
071500101108      /end-free
