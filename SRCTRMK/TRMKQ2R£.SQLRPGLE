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
001200130918      // - Anagrafica Commerciali
001300130918     fAZCMM01L  if   e           k disk
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
004800101111      // - Parametri ricevuti
004900101111     d KPJBA         e ds
005000101111
005100101111      // - File Info trattative
005200101111     d VIIDS         e ds                  extname(TIVII00F)
005300101111     d                                     prefix(w_)
005400101111
005500101111      // - File trattative
005600101111     d VISDS         e ds                  extname(TIVIS00F)
005700101111     d                                     prefix(w_)
005800101111
005900101111      // - File offerte
006000101111     d VOFDS         e ds                  extname(TIVOF00F)
006100101111
006200101111      // - FLO del file trattative
006300101111     d dVIS01        e ds                  inz
006400110331
006500110331       // - Reperimento dati anagrafici
006600110331     d TIBS69ds      e ds
006700110331     d DS_cnaco      e ds                  inz extname(CNACO00F)
006800110331     d DS_cnind      e ds                  inz extname(CNIND00F)
006900110331     d DS_cnclp      e ds                  inz extname(CNCLP00F)
007000110331     d DS_fncls      e ds                  inz extname(FNCLS00F)
007100101111
007200101111      //---------------------------------------------------------------
007300101111      //?Definizione variabili globali.
007400101111      //---------------------------------------------------------------
007500101111
007600101111      // - Flags booleani
007700101111     d $EndVii         s               n   inz(*off)
007800101111     d $EndVis         s               n   inz(*off)
007900101111     d $EndVof         s               n   inz(*off)
008000101111     d $OkInfo         s               n   inz(*off)
008100101111
008200101111      // - Indici di schiera
008300101111     d xx              s              4  0 inz
008400101111
008500101111      // - Campi di comodo
008600101111     d wage            s                   like(VIScmm)
008700110331     d wcap            s                   like(VOCcap)
008800110331     d wclv            s                   like(VOCclv)
008900110331     d wctm            s                   like(VOCctm)
009000101111     d wc05age         s                   like(VICcmar)
009100101111     d wc17age         s                   like(VICcmdi)
009200101111     d wdesage         s                   like(VICcmmd)
009300101111     d wfilage         s                   like(VICcmfi)
009400101111     d wfunage         s                   like(VICcmfu)
009500110331     d wrag            s                   like(VOCrag)
009600101111
009700101111
009800101111       // - Stringhe SQL da eseguire
009900101111     d wSQL            s           2048    Varying        inz
010000101111     d wSQLa           s           2048    Varying        inz
010100101111
010200101111      //---------------------------------------------------------------
010300101111      //?Definizione procedure usate.
010400101111      //---------------------------------------------------------------
010500101111
010600101111      //---------------------------------------------------------------
010700101111      //?Prototipi usati.
010800101111      //---------------------------------------------------------------
010900110331
011000110331      /copy gaitrasrc/srcprotopr,tibs69r
011100101111
011200101111      //---------------------------------------------------------------
011300101111      //?Definizione key-list.
011400101111      //---------------------------------------------------------------
011500101111
011600101111
011700101111      //---------------------------------------------------------------
011800101111      //?Riepilogo indicatori.
011900101111      //---------------------------------------------------------------
012000101111
012100101111      //---------------------------------------------------------------
012200101111      //?M A I N - L I N E
012300101111      //---------------------------------------------------------------
012400101111
012500101111     c     *Entry        plist
012600101111     c                   parm                    KPJBA
012700101111
012800101111      /free
012900101111
013000101111       //?Operazioni iniziali
013100101111       exsr RoutInz;
013200101111
013300101111       exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
013400101111       $EndVis = *off;
013500101111
013600101111       //?Imposto la stringa SQL
013700101111       exsr PrepSql;
013800101111
013900101111       //?Preparo il cursore
014000101111       exec sql prepare S1 from :wSQL;
014100101111
014200101111       exec sql declare VIS cursor for S1;
014300101111
014400101111       //?Apro il cursore
014500101111       exec sql open VIS;
014600101111       IF  sqlcode < 0;
014700101111         $EndVis = *on;
014800101111       ENDIF;
014900101111
015000101111       //?Leggo le trattative ancora da elaborare
015100101111       DOW  not $EndVis;
015200101111         exec sql fetch next from VIS into :VISDS;
015300101111         IF  sqlcod = 100 or sqlcod < 0;
015400101111           $EndVis = *on;
015500101111           leave;
015600101111         ENDIF;
015700101111
015800101111         //?Elaboro il record
015900101111         exsr Elabora;
016000101111
016100101111       ENDDO;
016200101111
016300101111       exec sql close VIS;
016400101111
016500101111       //?Operazioni finali
016600101111       exsr RoutEnd;
016700101111
016800101111       //--------------------------------------------------------------
016900101111       //?Operazioni iniziali.
017000101111       //--------------------------------------------------------------
017100101111       BEGSR RoutInz;
017200101111
017300101111         //?Codici commerciali + codice unificante
017400101111         clear xx;
017500130918         setll  (*loval)  AZCMM000;
017600130918         read   AZCMM000;
017700101111
017800130918         DOW not %eof(AZCMM01L);
017900130918           xx += 1;
018000130918           $AGE(xx)  = CMMcod;
018100130918           $AGEU(xx) = CMMuni;
018200130918           read   AZCMM000;
018300101111         ENDDO;
018400101111
018500101111       ENDSR;
018600101111
018700101111       //--------------------------------------------------------------
018800101111       //?Imposto la stringa SQL.
018900101111       //--------------------------------------------------------------
019000101111       BEGSR PrepSql;
019100101111
019200101111         clear wSQL;
019300101111
019400101123         wSQL = 'select * from ';
019500101123
019600101123
019700110803         wSQL += 'TIVIS00F ';
019800101123         wSQL += 'where VISffz = '' '' and ';
019900101123         wSQL += 'substr(VISflo, 2, 1) = '' '' ';
020000101123         wSQL += 'order by VISnrv';
020100101111
020200101111       ENDSR;
020300101111
020400101111       //--------------------------------------------------------------
020500101111       //?Elaborazione record.
020600101111       //--------------------------------------------------------------
020700101111       BEGSR Elabora;
020800101111
020900101111         dVIS01 = w_VISflo;
021000101111
021100101111       //?Trattative concluse
021200101111         IF  w_VISdch > 0;
021300101108           exsr Concluse;
021400101108         ENDIF;
021500101108
021600101111       //?Trattative in corso
021700101111         IF  w_VISdch = 0;
021800101111           exsr InCorso;
021900101108         ENDIF;
022000101108
022100101108       ENDSR;
022200101108
022300101108       //--------------------------------------------------------------
022400101111       //?Scrivo i file delle trattative concluse.
022500101108       //--------------------------------------------------------------
022600101108       BEGSR Concluse;
022700101124
022800101124       //?Controllo subito se il rcd della trattativa è allocato
022900101124         chain(e) (w_VISnrv) TIVIS05L;
023000101124       //?se allocato esco dalla routine così leggo altra trattativa
023100101124       //?questa trattativa la scriverò alla prossima elaborazione
023200101124         IF  %error;
023300101124           leavesr;
023400101124         ENDIF;
023500101108
023600101111         clear TIVISC00;
023700101110
023800101111         VICnrv  = w_VISnrv;
023900101111         VICtpv  = w_VIStpv;
024000101111         VICdat  = w_VISdat;
024100101108
024200101111       //?Cerco commerciale unificante del commerciale gestione trattativa
024300101111         IF  w_VIScmm > 0;
024400101111           wage = w_VIScmm;
024500101110           xx =  %lookup(wage:$AGE);
024600101108           IF  xx > 0;
024700101111             VICcmm = $AGEU(xx);
024800101110           ELSE;
024900101111             VICcmm = wage;
025000101108           ENDIF;
025100101108           exsr DecAgeU;
025200101111           VICcmfu = wfunage;
025300101111           VICcmmd = wdesage;
025400101111           VICcmfi = wfilage;
025500101111           VICcmar = wc05age;
025600101111           VICcmdi = wc17age;
025700101108         ENDIF;
025800101111
025900101111       //?Controllo se trattativa con offerte
026000101111       //?e scrivo relativo file se presenti offerte convalidate
026100101111         exsr CtrVof;
026200101111
026300101111         VICdch  = w_VISdch;
026400101111         VICesi  = w_VISesi;
026500101111         VICesi2 = §VISesito;
026600110401         VICcpot = §VIScatpo;
026700101110
026800101111         VICtck  = %char(%timestamp:*iso0);
026900101110
027000101111         write TIVISC00;
027100101110
027200101110       //?Aggiorno rcd come già elaborato
027300101111         IF  %found(TIVIS05L);
027400101111           dVIS01 = VISflo;
027500101111           §VISelaql = 'S';
027600101111           VISflo = dVIS01;
027700101111           update TIVIS000;
027800101110         ENDIF;
027900101111
028000101111       //?Scrivo file info trattative
028100101111         exsr InfoVIC;
028200101108
028300101108       ENDSR;
028400101108
028500101108       //--------------------------------------------------------------
028600101111       //?Scrivo il file delle trattative in corso.
028700101108       //--------------------------------------------------------------
028800101111       BEGSR InCorso;
028900101110
029000101111         clear TIVISI00;
029100101111
029200101111         VIInrv  = w_VISnrv;
029300101111         VIItpv  = w_VIStpv;
029400101111         VIIdat  = w_VISdat;
029500101111
029600101111       //?Cerco commerciale unificante del commerciale gestione trattativa
029700101111         IF  w_VIScmm > 0;
029800101111           wage = w_VIScmm;
029900101111           xx =  %lookup(wage:$AGE);
030000101111           IF  xx > 0;
030100101111             VIIcmm = $AGEU(xx);
030200101111           ELSE;
030300101111             VIIcmm = wage;
030400101111           ENDIF;
030500101111           exsr DecAgeU;
030600101111           VIIcmfu = wfunage;
030700101111           VIIcmmd = wdesage;
030800101111           VIIcmfi = wfilage;
030900101111           VIIcmar = wc05age;
031000101111           VIIcmdi = wc17age;
031100101111         ENDIF;
031200101111
031300101111       //?Controllo se trattativa con offerte
031400101111       //?e scrivo relativo file se presenti offerte convalidate
031500101111         exsr CtrVof;
031600101111
031700110401         VIIcpot = §VIScatpo;
031800101111
031900101111         write TIVISI00;
032000101111
032100101111       //?Scrivo file info trattative
032200101111         exsr InfoVII;
032300101108
032400101108       ENDSR;
032500101108
032600101108       //--------------------------------------------------------------
032700101108       //?Decodifico agente unificante.
032800101108       //--------------------------------------------------------------
032900101108       BEGSR DecAgeU;
033000101108
033100101108         clear wdesage;
033200101108         clear wfunage;
033300101108         clear wfilage;
033400101108         clear wc17age;
033500101108         clear wc05age;
033600101110
033700101110         IF  xx > 0;
033800130918           chain  ($AGEU(xx))  AZCMM000;
033900130918           IF  %found( AZCMM01L );
034000130918             wdesage = CMMdes;
034100130918             wfunage = CMMfun;
034200101110           ENDIF;
034300101110           wage    = $AGEU(xx);
034400101110         ENDIF;
034500101108
034600101108       //?Filiale
034700101110         wfilage = %dec(%subst(%editc(wage:'X'):1:3):3:0);
034800101108         chain  wfilage AZORG01L;
034900101108         IF  %found(AZORG01L);
035000101108           wc17age = ORGfl3;
035100101108           wc05age = ORGcar;
035200101108         ENDIF;
035300101108
035400101108       ENDSR;
035500101111
035600101111       //--------------------------------------------------------------
035700101111       //?Controllo se trattativa con offerte.
035800101111       //--------------------------------------------------------------
035900101111       BEGSR CtrVof;
036000101111
036100101111         $EndVof = *off;
036200101111
036300101111         wSQLa = 'select * from ';
036400101111
036500101111
036600110803         wSQLa += 'TIVOF00F ';
036700101111         wSQLa += 'where VOFnrv = ' + %editc(w_VISnrv:'X') +
036800101111                  ' order by VOFctr, VOFprg';
036900101111
037000101111         exec sql prepare S2 from :wSQLa;
037100101111
037200101111         exec sql declare VOF cursor for S2;
037300101111
037400101111         exec sql open VOF;
037500101111
037600101111         IF  sqlcode < 0;
037700101111           $EndVof = *on;
037800101111         ENDIF;
037900101111
038000101111         DOW  not $EndVof;
038100101111           exec sql fetch next from VOF into :VOFDS;
038200101111           IF  sqlcod = 100 or sqlcod < 0;
038300101111             $EndVof = *on;
038400101111             leave;
038500101111           ENDIF;
038600101111
038700101111         //?Imposto i dati relativi alla presenza di offerte
038800101111           SELECT;
038900101111             WHEN  VOFtpt = 'I' and w_VISdch > 0;
039000101111               VICpoi = 'S';
039100101111             WHEN  VOFtpt = 'I' and w_VISdch = 0;
039200101111               VIIpoi = 'S';
039300101111             WHEN  VOFtpt = 'E' and w_VISdch > 0;
039400101112               VICpoe = 'S';
039500101111             WHEN  VOFtpt = 'E' and w_VISdch = 0;
039600101112               VIIpoe = 'S';
039700101111             WHEN  VOFtpt = 'D' and w_VISdch > 0;
039800101112               VICpod = 'S';
039900101111             WHEN  VOFtpt = 'D' and w_VISdch = 0;
040000101112               VIIpod = 'S';
040100101111             WHEN  VOFtpt = 'F' and w_VISdch > 0;
040200101112               VICpof = 'S';
040300101111             WHEN  VOFtpt = 'F' and w_VISdch = 0;
040400101112               VIIpof = 'S';
040500101111           ENDSL;
040600110331
040700110331         //?Recupero i dati anagrafici
040800120126           clear wrag;
040900120126           clear wclv;
041000120126           clear wctm;
041100120126           clear wcap;
041200110331           IF  VOFksc > 0;
041300110331             clear  tibs69ds;
041400110331             I69kac = VOFksc;
041500110331             I69kin = VOFksc;
041600110331             I69kcp = VOFksc;
041700110331             TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
041800110331             wrag = ACOrag;
041900110331             wclv = CLPclv;
042000110331             wctm = ACOitc;
042100110331             wcap = INDcae;
042200110331           ENDIF;
042300101111
042400101111         //?Scrivo i file delle offerte x QL
042500101111         //?se sto elaborando le trattative concluse
042600101111         //?devo considerare solo le offerte convalidate
042700101111           IF  w_VISdch > 0 and VOFeso = 'C';
042800101111             clear TIVOFC00;
042900101111             VOCnrv = VOFnrv;
043000101111             VOCctr = VOFctr;
043100101111             VOCprg = VOFprg;
043200101111             VOCtpt = VOFtpt;
043300101111             VOCdpo = VOFdpo;
043400101111             VOCdpa = VOFdpa;
043500101111             VOCdra = VOFdra;
043600101111             VOCdco = VOFdco;
043700101111             VOCksc = VOFksc;
043800110331             VOCrag = wrag;
043900110331             VOCclv = wclv;
044000110331             VOCctm = wctm;
044100110331             VOCcap = wcap;
044200101111             VOCtck = %char(%timestamp:*iso0);
044300101111             write TIVOFC00;
044400101111           ENDIF;
044500101111
044600101116         //?se sto elaborando le trattative in corso
044700101111         //?devo considerare solo le offerte convalidate e/o accettate
044800101111           IF  w_VISdch = 0 and (VOFeso = 'C' or VOFeso = 'A');
044900101111             clear TIVOFI00;
045000101111             VOInrv = VOFnrv;
045100101111             VOIctr = VOFctr;
045200101111             VOIprg = VOFprg;
045300101111             VOItpt = VOFtpt;
045400101111             VOIdpo = VOFdpo;
045500101111             VOIeso = VOFeso;
045600101111             VOIdpa = VOFdpa;
045700101111             VOIdra = VOFdra;
045800101111             VOIdco = VOFdco;
045900101111             VOIksc = VOFksc;
046000110331             VOIrag = wrag;
046100110331             VOIclv = wclv;
046200110331             VOIctm = wctm;
046300110331             VOIcap = wcap;
046400101111             write TIVOFI00;
046500101111           ENDIF;
046600101111
046700101111         ENDDO;
046800101111
046900101111         exec sql close VOF;
047000101111
047100101111       ENDSR;
047200101111
047300101111       //--------------------------------------------------------------
047400101111       //?Scrivo file Info trattative concluse.
047500101111       //--------------------------------------------------------------
047600101111       BEGSR InfoVIC;
047700101111
047800101111         $EndVii = *off;
047900101111         $OKinfo = *off;
048000101111         clear TIVIIC00;
048100101111
048200101111         wSQLa = 'select * from ';
048300101111
048400110927         wSQLa += 'TIVII00F ';
048500101111
048600101111         wSQLa += 'where VIInrv = ' + %editc(w_VISnrv:'X') +
048700101111                  ' order by VIItpf';
048800101111
048900101111         exec sql prepare S3 from :wSQLa;
049000101111
049100101111         exec sql declare VIIC cursor for S3;
049200101111
049300101111         exec sql open VIIC;
049400101111
049500101110         IF  sqlcode < 0;
049600101111           $EndVii = *on;
049700101110         ENDIF;
049800101110
049900101111         DOW  not $EndVii;
050000101111           exec sql fetch next from VIIC into :VIIDS;
050100101110           IF  sqlcod = 100 or sqlcod < 0;
050200101111             $EndVii = *on;
050300101110             leave;
050400101110           ENDIF;
050500101110
050600101110       //?Imposto i dati per scrivere il record
050700101111           $OKinfo = *on;
050800101111           VICnrv = w_VIInrv;
050900101110
051000101111           SELECT;
051100101110         //?Fatturato/Spedizioni totale
051200101111             WHEN  w_VIItpf = 'TRT';
051300101111               VICfatt = w_VIIpft;
051400101111               VICnspt = w_VIIsna;
051500101111         //?Fatturato/Spedizioni estero
051600101111             WHEN  w_VIItpf = 'TRE';
051700101111               VICfate = w_VIIpft;
051800101111               VICnspe = w_VIIsna;
051900110203         //?Aumento/Sconto
052000110203             WHEN  w_VIItpf = 'A/S';
052100110504               VICausc = w_VIIvald;
052200101111         //?Delta
052300101111             WHEN  w_VIItpf = 'DEL';
052400101111               VICdel = w_VIIval;
052500101111         //?Peso
052600101111             WHEN  w_VIItpf = 'KMS';
052700101111               VICpkg = w_VIIval;
052800101111         //?Concorrenti
052900101111             WHEN  w_VIItpf = '10' and w_VIIspf = 'ARCO';
053000101111               VICarco = 'S';
053100101111             WHEN  w_VIItpf = '10' and w_VIIspf = 'ARTO';
053200101111               VICarto = 'S';
053300101111             WHEN  w_VIItpf = '10' and w_VIIspf = 'AWS';
053400101111               VICaws  = 'S';
053500101111             WHEN  w_VIItpf = '10' and w_VIIspf = 'BARS';
053600101111               VICbars = 'S';
053700101111             WHEN  w_VIItpf = '10' and w_VIIspf = 'DHL';
053800101111               VICdhl  = 'S';
053900101111             WHEN  w_VIItpf = '10' and w_VIIspf = 'EXEC';
054000101111               VICexec = 'S';
054100101111             WHEN  w_VIItpf = '10' and w_VIIspf = 'FERC';
054200101111               VICferc = 'S';
054300101111             WHEN  w_VIItpf = '10' and w_VIIspf = 'MAIL';
054400101111               VICmail = 'S';
054500101111             WHEN  w_VIItpf = '10' and w_VIIspf = 'MEP';
054600101111               VICmep  = 'S';
054700101111             WHEN  w_VIItpf = '10' and w_VIIspf = 'MTN';
054800101111               VICmtn  = 'S';
054900101111             WHEN  w_VIItpf = '10' and w_VIIspf = 'ONEE';
055000101111               VIConee = 'S';
055100101111             WHEN  w_VIItpf = '10' and w_VIIspf = 'PALL';
055200101111               VICpall = 'S';
055300101111             WHEN  w_VIItpf = '10' and w_VIIspf = 'PALW';
055400101111               VICpalw = 'S';
055500101111             WHEN  w_VIItpf = '10' and w_VIIspf = 'SDA';
055600101111               VICsda  = 'S';
055700101111             WHEN  w_VIItpf = '10' and w_VIIspf = 'SUSA';
055800101111               VICsusa = 'S';
055900101111             WHEN  w_VIItpf = '10' and w_VIIspf = 'TNT';
056000101111               VICtnt  = 'S';
056100101111             WHEN  w_VIItpf = '10' and w_VIIspf = 'UPS';
056200101111               VICups  = 'S';
056300101111             WHEN  w_VIItpf = '10' and w_VIIspf = '9910';
056400101111               VICaltr = 'S';
056500101111             WHEN  w_VIItpf = '10' and w_VIIspf = '8810';
056600101111               VICness = 'S';
056700101111           ENDSL;
056800101110         ENDDO;
056900101111
057000101111       //?Scrivo il rcd delle info commerciali
057100101111         IF  $OKinfo;
057200101111           VICtck = %char(%timestamp:*iso0);
057300101111           write  TIVIIC00;
057400101111         ENDIF;
057500101110
057600101111         exec sql close VIIC;
057700101110
057800101110       ENDSR;
057900101111
058000101111       //--------------------------------------------------------------
058100101111       //?Scrivo file Info trattative in corso.
058200101111       //--------------------------------------------------------------
058300101111       BEGSR InfoVII;
058400101111
058500101111         $EndVii = *off;
058600101111         $OKinfo = *off;
058700101111         clear TIVIII00;
058800101111
058900101111         wSQLa = 'select * from ';
059000101111
059100110927         wSQLa += 'TIVII00F ';
059200101111
059300101111         wSQLa += 'where VIInrv = ' + %editc(w_VISnrv:'X') +
059400101111                  ' order by VIItpf';
059500101111
059600101111         exec sql prepare S4 from :wSQLa;
059700101111
059800101111         exec sql declare VIII cursor for S4;
059900101111
060000101111         exec sql open VIII;
060100101111
060200101111         IF  sqlcode < 0;
060300101111           $EndVii = *on;
060400101111         ENDIF;
060500101111
060600101111         DOW  not $EndVii;
060700101111           exec sql fetch next from VIII into :VIIDS;
060800101111           IF  sqlcod = 100 or sqlcod < 0;
060900101111             $EndVii = *on;
061000101111             leave;
061100101111           ENDIF;
061200101111
061300101111       //?Imposto i dati per scrivere il record
061400101111           $OKinfo = *on;
061500101111           VIInrv = w_VIInrv;
061600101111
061700101111           SELECT;
061800101111         //?Fatturato/Spedizioni totale
061900101111             WHEN  w_VIItpf = 'TRT';
062000101111               VIIfatt = w_VIIpft;
062100101111               VIInspt = w_VIIsna;
062200101111         //?Fatturato/Spedizioni estero
062300101111             WHEN  w_VIItpf = 'TRE';
062400101111               VIIfate = w_VIIpft;
062500101111               VIInspe = w_VIIsna;
062600110203         //?Aumento/Sconto
062700110203             WHEN  w_VIItpf = 'A/S';
062800110504               VIIausc = w_VIIvald;
062900101111         //?Delta
063000101111             WHEN  w_VIItpf = 'DEL';
063100101111               VIIdel = w_VIIval;
063200101111         //?Peso
063300101111             WHEN  w_VIItpf = 'KMS';
063400101111               VIIpkg = w_VIIval;
063500101111         //?Concorrenti
063600101111             WHEN  w_VIItpf = '10' and w_VIIspf = 'ARCO';
063700101111               VIIarco = 'S';
063800101111             WHEN  w_VIItpf = '10' and w_VIIspf = 'ARTO';
063900101111               VIIarto = 'S';
064000101111             WHEN  w_VIItpf = '10' and w_VIIspf = 'AWS';
064100101111               VIIaws  = 'S';
064200101111             WHEN  w_VIItpf = '10' and w_VIIspf = 'BARS';
064300101111               VIIbars = 'S';
064400101111             WHEN  w_VIItpf = '10' and w_VIIspf = 'DHL';
064500101111               VIIdhl  = 'S';
064600101111             WHEN  w_VIItpf = '10' and w_VIIspf = 'EXEC';
064700101111               VIIexec = 'S';
064800101111             WHEN  w_VIItpf = '10' and w_VIIspf = 'FERC';
064900101111               VIIferc = 'S';
065000101111             WHEN  w_VIItpf = '10' and w_VIIspf = 'MAIL';
065100101111               VIImail = 'S';
065200101111             WHEN  w_VIItpf = '10' and w_VIIspf = 'MEP';
065300101111               VIImep  = 'S';
065400101111             WHEN  w_VIItpf = '10' and w_VIIspf = 'MTN';
065500101111               VIImtn  = 'S';
065600101111             WHEN  w_VIItpf = '10' and w_VIIspf = 'ONEE';
065700101111               VIIonee = 'S';
065800101111             WHEN  w_VIItpf = '10' and w_VIIspf = 'PALL';
065900101111               VIIpall = 'S';
066000101111             WHEN  w_VIItpf = '10' and w_VIIspf = 'PALW';
066100101111               VIIpalw = 'S';
066200101111             WHEN  w_VIItpf = '10' and w_VIIspf = 'SDA';
066300101111               VIIsda  = 'S';
066400101111             WHEN  w_VIItpf = '10' and w_VIIspf = 'SUSA';
066500101111               VIIsusa = 'S';
066600101111             WHEN  w_VIItpf = '10' and w_VIIspf = 'TNT';
066700101111               VIItnt  = 'S';
066800101111             WHEN  w_VIItpf = '10' and w_VIIspf = 'UPS';
066900101111               VIIups  = 'S';
067000101111             WHEN  w_VIItpf = '10' and w_VIIspf = '9910';
067100101111               VIIaltr = 'S';
067200101111             WHEN  w_VIItpf = '10' and w_VIIspf = '8810';
067300101111               VIIness = 'S';
067400101111           ENDSL;
067500101111         ENDDO;
067600101111
067700101111       //?Scrivo il rcd delle info commerciali
067800101111         IF  $OKinfo;
067900101111           write  TIVIII00;
068000101111         ENDIF;
068100101111
068200101111         exec sql close VIII;
068300101111
068400101111       ENDSR;
068500101108
068600101108       //--------------------------------------------------------------
068700101108       //?Operazioni finali.
068800101108       //--------------------------------------------------------------
068900101108       BEGSR RoutEnd;
069000101108
069100101108         *inLR = *on;
069200101108         return;
069300101108
069400101108       ENDSR;
069500101108
069600101108      /end-free
