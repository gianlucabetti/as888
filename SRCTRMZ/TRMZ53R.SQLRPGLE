000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200041103     c**********************************************************************
000300041103     C* IL PROGRAMMA NON RISPETTA LE LUNGHEZZE DEL FILE RICEVUTO MA LE
000400041103     C* ADATTA A QUELLE DEL FILE AGIP
000500041103     c**********************************************************************
000600011206     F*          STATISTICA CONSUMI                        *
000700041122     fagip      o    e             disk    rename(agip:agip000) commit
000800041103     fYco3070P  o    f  198        printer
000900041103     F                                     OFLIND(*INOF)
001000041103     d rsut            s             20
001100041103     D ut§dse        E DS                  extname(ut§dse0f)
001200041124     D*------------------
001300041124     D* DS "TISI95R" - CONTROLLO CAP
001400041124     D*------------------
001500041124     D DSSI95        E DS                  EXTNAME(TISI95DS)
001600041102     d* data fattura
001700041102     ddtfat            s              8
001800041102     d* numero fattura
001900041102     dnrdoc            s              8
002000041103     d* segno
002100041103     dsegnof           s              1
002200041102     d* numero voucher
002300041102     dnrvou            s              8
002400041102     d* data rifornimento
002500041102     ddttra            s              8
002600041102     d* ora rifornimento
002700041102     doratra           s              4
002800041102     d* targa
002900041103     dtarga            s             15
003000041103     d* fai da te
003100041103     dfai              s              1
003200041103     d* indirizzo punto vendita
003300041103     dind              s             24
003400041103     dloc              s             20
003500041103     dcap              s             12
003600041103     d* codice prodotto
003700041103     dcodpro           s              2
003800041103     d* descrizione prodotto
003900041103     ddespro           s             12
004000041103     d* chilometraggio
004100041102     dkm               s              7
004200041102     d* valuta
004300041103     dvalutaf          s              3
004400041102     d* quantità
004500041102     dqtan             s             10  2
004600041102     dqta              s             10
004700041102     d* prezzo unitario
004800041102     dprzuni           s              8
004900041102     dprzunin          s              8  3
005000041102     c* importo sconto
005100041103     dscontoF          s              7
005200041102     dsconton          s              7  2
005300041102     c* aliquota iva
005400041102     diva              s              5
005500041102     divan             s              5  2
005600041102     c* importo iva
005700041102     dimpiva           s             12
005800041102     dimpivan          s             12  2
005900041102     c* importo al netto d'iva
006000041102     dimpnet           s             12
006100041102     d* importo lordo già scontato
006200041102     dlordo            s             12
006300041103     d* festivo
006400041103     dfestivof         s              1
006500041103     d* tipo fatturaz.
006600041103     dtpfat            s              2
006700041103     d* tipo transaz.
006800041103     dtptra            s              1
006900041122     D WrkSqlCmd       S           1024
007000020607     C*---------------------------------------------------------------*
007100041123     c* leggo solo le fatture nazionali (02) + le transizioni
007200041123     c* relative a merci o servizi (T) + i record di dettaglio (10) +
007300080528     c* codice prodotto <= 33 (solo carburante)
007400041122     c                   eval      wrksqlcmd='SELECT substr(f1, 3, 8),         -
007500041122     C                             substr(f1, 13, 8), substr(f1, 68, 1),       -
007600041122     C                             substr(f1, 70, 8), substr(f1, 78, 8),       -
007700041122     C                             substr(f1, 86, 4),                          -
007800041122     C                             substr(f1, 90, 15), substr(f1, 121, 7),     -
007900041122     C                             substr(f1, 146, 1), substr(f1, 147, 1),     -
008000041122     c                             substr(f1, 216, 24),                        -
008100041122     C                             substr(f1, 248, 12), substr(f1, 260, 20),   -
008200041122     c                             substr(f1, 300, 2),                         -
008300041122     C                             substr(f1, 302, 12), substr(f1, 334, 3),    -
008400041122     c                             substr(f1, 340, 10),                        -
008500041122     C                             substr(f1, 357, 8), substr(f1, 388, 7),     -
008600041122     c                             substr(f1, 395, 5),                         -
008700041122     C                             substr(f1, 400, 12), substr(f1, 412, 12),   -
008800041122     c                             substr(f1, 424, 12)                         -
008900041122     C                             FROM pceni WHERE substr(f1, 1, 2) =''10''   -
009000041122     c                             and substr(f1, 11, 2) =''02''               -
009100041123     C                             and substr(f1, 69, 1) =''T''                -
009200041123     C                             and substr(f1, 300, 2) between ''21'' and   -
009300080528     C                             ''33'''
009400021211     C*
009500041122     c* Lettura pceni
009600041122     C/EXEC SQL
009700041122     C+ PREPARE S1 FROM :WrkSqlCmd
009800041122     C/END-EXEC
009900041122
010000041122     C/EXEC SQL
010100041122     C+ DECLARE A1 CURSOR FOR S1
010200041122     C/END-EXEC
010300041122
010400041122     C/EXEC SQL
010500041122     C+ OPEN A1
010600041122     C/END-EXEC
010700041122
010800041122     C                   DOU       SqlCod <> 0
010900041122     C/EXEC SQL
011000041122     C+ FETCH NEXT FROM A1 INTO :dtfat, :nrdoc, :segnof, :nrvou, :dttra,
011100041122     C+ :oratra, :targa, :km, :festivof, :fai, :ind, :cap, :loc, :codpro,
011200041122     C+ :despro, :valutaf, :qta, :przuni, :scontof, :iva, :impiva,
011300041122     C+ :impnet, :lordo
011400041122     C/END-EXEC
011500041102     C                   SELECT
011600041102     C                   when      sqlcod < 0
011700021211     C                   call      'X66CHGJOB'
011800030113     c                   seton                                        h1
011900041102     C                   when      sqlcod <> 100
012000041102     c                   exsr      srwrite
012100041102     C                   ENDsl
012200041122     C                   ENDDO
012300041122     C/EXEC SQL
012400041122     C+ CLOSE A1
012500041122     C/END-EXEC
012600041122     c                   commit
012700041123     c* leggo solo le fatture nazionali (02) + le transizioni
012800041123     c* relative a merci o servizi (T) + i record di dettaglio (10) +
012900080528     c* codice prodotto > 33 (ciò che non è carburante)
013000041123     c                   seton                                        03
013100041123     c                   setoff                                       0102
013200041123     c                   eval      wrksqlcmd='SELECT substr(f1, 300, 2)        -
013300041123     C                             FROM pceni WHERE substr(f1, 1, 2) =''10''   -
013400041123     c                             and substr(f1, 11, 2) =''02''               -
013500041123     C                             and substr(f1, 69, 1) =''T''                -
013600041123     C                             and (substr(f1, 300, 2) < ''21'' or         -
013700080528     C                             substr(f1, 300, 2) > ''33'')                -
013800041123     c                             GROUP BY substr(f1, 300, 2)'
013900041123     C/EXEC SQL
014000041123     C+ PREPARE S4 FROM :WrkSqlCmd
014100041123     C/END-EXEC
014200041123
014300041123     C/EXEC SQL
014400041123     C+ DECLARE d1 CURSOR FOR S4
014500041123     C/END-EXEC
014600041123
014700041123     C/EXEC SQL
014800041123     C+ OPEN d1
014900041123     C/END-EXEC
015000041123
015100041123     C                   DOU       SqlCod <> 0
015200041123     C/EXEC SQL
015300041123     C+ FETCH NEXT FROM d1 INTO :codpro
015400041123     C/END-EXEC
015500041123     C                   SELECT
015600041123     C                   when      sqlcod < 0
015700041123     C                   call      'X66CHGJOB'
015800041123     c                   seton                                        h1
015900041123     C                   when      sqlcod <> 100
016000041123     c                   exsr      srstampa
016100041123     C                   ENDsl
016200041123     C                   ENDDO
016300041123     C/EXEC SQL
016400041123     C+ CLOSE d1
016500041123     C/END-EXEC
016600041103     c* leggo tutto ciò che non è fatture nazionale (02)
016700041103     c                   seton                                        01
016800041123     c                   setoff                                       0203
016900041122     c                   eval      wrksqlcmd='SELECT substr(f1, 11, 2)         -
017000041122     C                             FROM pceni WHERE substr(f1, 1, 2) =''10''   -
017100041122     c                             and substr(f1, 11, 2)<>''02''               -
017200041122     c                             GROUP BY substr(f1, 11, 2)'
017300041122     C/EXEC SQL
017400041122     C+ PREPARE S2 FROM :WrkSqlCmd
017500041122     C/END-EXEC
017600041122
017700041122     C/EXEC SQL
017800041122     C+ DECLARE b1 CURSOR FOR S2
017900041122     C/END-EXEC
018000041122
018100041122     C/EXEC SQL
018200041122     C+ OPEN b1
018300041122     C/END-EXEC
018400041122
018500041122     C                   DOU       SqlCod <> 0
018600041122     C/EXEC SQL
018700041122     C+ FETCH NEXT FROM b1 INTO :tpfat
018800041122     C/END-EXEC
018900041122     C                   SELECT
019000041122     C                   when      sqlcod < 0
019100041122     C                   call      'X66CHGJOB'
019200041122     c                   seton                                        h1
019300041122     C                   when      sqlcod <> 100
019400041122     c                   exsr      srstampa
019500041122     C                   ENDsl
019600041122     C                   ENDDO
019700041122     C/EXEC SQL
019800041122     C+ CLOSE b1
019900041122     C/END-EXEC
020000041123     c* leggo tutto ciò che non è transazione relativa a merci o servizi 'T'
020100041103     c                   seton                                        02
020200041123     c                   setoff                                       0103
020300041122     c                   eval      wrksqlcmd='SELECT substr(f1, 69, 1)         -
020400041122     C                             FROM pceni WHERE substr(f1, 1, 2) =''10''   -
020500041122     C                             and substr(f1, 69, 1)<>''T''                -
020600041122     c                             GROUP BY substr(f1, 69, 1)'
020700041122     C/EXEC SQL
020800041122     C+ PREPARE S3 FROM :WrkSqlCmd
020900041122     C/END-EXEC
021000041122
021100041122     C/EXEC SQL
021200041122     C+ DECLARE c1 CURSOR FOR S3
021300041122     C/END-EXEC
021400041122
021500041122     C/EXEC SQL
021600041122     C+ OPEN c1
021700041122     C/END-EXEC
021800041122
021900041122     C                   DOU       SqlCod <> 0
022000041122     C/EXEC SQL
022100041122     C+ FETCH NEXT FROM c1 INTO :tpfat
022200041122     C/END-EXEC
022300041122     C                   SELECT
022400041122     C                   when      sqlcod < 0
022500041122     C                   call      'X66CHGJOB'
022600041122     c                   seton                                        h1
022700041122     C                   when      sqlcod <> 100
022800041122     c                   exsr      srstampa
022900041122     C                   ENDsl
023000041122     C                   ENDDO
023100041122     C/EXEC SQL
023200041122     C+ CLOSE c1
023300041122     C/END-EXEC
023400041124     C* CHIUDE I PROGRAMMA DI UTILITA' RICHIAMATI
023500041124     C                   CLEAR                   DSSI95                         *CONTROLLA CAP
023600041124     C                   MOVEL     'C'           I95TLA
023700041124     C                   CALL      'TISI95R'
023800041124     C                   PARM                    DSSI95
023900041102     c                   seton                                        lr
024000041102     c**********************************************************************
024100041102     c     srwrite       begsr
024200041102     c**********************************************************************
024300041103     c                   clear                   agip000
024400041102     C* targa
024500041103     c                   movel     targa         NOMINATIVO
024600041103     c* voucher
024700041103     c                   move      *all'0'       NUMER00003
024800041103     c                   move      nrvou         NUMER00003
024900041102     c* data utilizzo
025000041103     c                   move      dttra         DATA_00003
025100041103     c* ora utilizzo
025200041103     c                   move      *all'0'       ORA_V00001
025300041103     c                   movel     oratra        ORA_V00001
025400041103     c* indirizzo
025500041103     c                   movel     ind           INDIR00001
025600041103     c                   movel     loc           LOCAL00001
025700041103     c* utilizzo il campo informazioni supplementari oper memorizzare il cap
025800041103     c                   movel     cap           INFOR00001
025900041124     C* chiamo funzione cappario per provincia
026000041124     c                   clear                   dssi95
026100041124     C                   MOVEL     *BLANKS       I95TLA                         *TIPO LANCIO
026200041124     C                   MOVEL     '3'           I95TCN                         *CONTROLLO ESISTENZA
026300041124     C                   MOVEL     cap           I95CAP                         *CAP
026400041124     C                   move      dttra         I95DAT                         *DATA RIFERIMENTO
026500041124     C                   CALL      'TISI95R'
026600041124     C                   PARM                    DSSI95
026700041124IF  3C     O95ERR        IFeq      *BLANKS                                      *NON TROVATO
026800041124     c                   MOVEL     O95prv        PROV_PV
026900041124     c                   end
027000041103     c* prodotto
027100041103     c                   movel     codpro        COD_P00001
027200041103     c                   movel     despro        DESCR00001
027300041103     c* unità di misura forzo lt
027400041103     c                   movel     'LT'          UN_MI00001
027500041103     c* festivo
027600041103     c                   movel     festivof      FESTIVO
027700041103     c* fai da te
027800041103     c                   if        fai = 'Y'
027900041103     c                   movel     'S'           FAI_DA_TE
028000041103     c                   else
028100041103     c                   movel     fai           FAI_DA_TE
028200041103     c                   end
028300041103     c* quantità
028400041103     c                   move      qta           qtan
028500041103     c                   move      qtan          QUANTITA
028600041102     c* km
028700041103     c                   move      km            CHILOMETRI
028800041102     c* data fattura
028900041103     c                   move      dtfat         DATA_00001
029000041102     c* numero fattura
029100041103     c                   move      *all'0'       NUMER00001
029200041103     c                   move      nrdoc         NUMER00001
029300041103     c* segno
029400041103     c                   if        segnof = 'D'
029500041103     c                   movel     '+'           SEGNO
029600041103     c                   else
029700041103     c                   movel     '-'           SEGNO
029800041103     c                   end
029900041103     c* valuta
030000041103     c                   movel     valutaf       VALUTA
030100041102     c* prezzo unitario
030200041103     c                   move      przuni        przunin
030300041103     c                   move      przunin       PREZZO
030400041102     c* prezzo totale
030500041103     c                   move      lordo         IMPORTO
030600041102     c* aliquota iva
030700041102     c                   move      iva           ivan
030800041220     c                   z-add     ivan          comiva            2 0
030900041103     c                   move      comiva        ALIQU00001
031000041102     c* importo iva
031100041102     c                   move      impiva        impivan
031200041103     c                   move      impivan       IMPOR00001
031300041102     c* imponibile
031400041103     c                   move      impnet        IMPONIBILE
031500041102     c* sconto
031600041103     c                   move      scontof       sconton
031700041103     c                   move      sconton       SCONTO
031800041103     c                   write     agip000
031900041102     c                   endsr
032000041103     c**********************************************************************
032100041103     c     srstampa      begsr
032200041103     c**********************************************************************
032300041103     c                   if        *inof
032400041103     c                   except    testa
032500041103     c                   setoff                                       of
032600041103     c                   end
032700041103     c*
032800041103     c                   clear                   desfat           40
032900041103     c                   clear                   destra           40
033000041103     c* errori tipo fatture
033100041103     c                   if        *in01
033200041103     c                   select
033300041103     c                   when      tpfat = '03'
033400041103     c                   eval      desfat = 'NOTA DEBITO/CREDITO NAZIONALE'
033500041103     c                   when      tpfat = '04' or tpfat = '05'
033600041103     c                   eval      desfat = 'FATTURA INTERNAZIONALE'
033700041103     c                   when      tpfat = '06' or tpfat = '07'
033800041103     c                   eval      desfat = 'NOTA DEBITO/CREDITO INTERNAZIONALE'
033900041103     c                   when      tpfat = '08'
034000041103     c                   eval      desfat = 'NOTA DEBITO/CREDITO PREMI'
034100041103     c                   when      tpfat = '09'
034200041103     c                   eval      desfat = 'DISTINTA SOFID'
034300041103     c                   endsl
034400041103     c                   except    errfat
034500041103     c                   end
034600041103     c* errori transazioni
034700041103     c                   if        *in02
034800041103     c                   select
034900041103     c                   when      tptra = 'S'
035000041103     c                   eval      destra = 'OPERAZIONI PARTICOLARI'
035100041103     c                   when      tptra = 'F'
035200041103     c                   eval      destra = 'RICARICHI E COSTI SERVIZI'
035300041103     c                   when      tptra = 'B'
035400041103     c                   eval      destra = 'PREMI'
035500041103     c                   endsl
035600041103     c                   except    errtra
035700041103     c                   end
035800041123     c* errori codici prodotto <> da carburante
035900041123     c                   if        *in03
036000041123     c                   except    errpro
036100041123     c                   end
036200041103     c                   endsr
036300041103     C**********************************************************************
036400041103     C     *inzsr        BEGSR
036500041103     C**********************************************************************
036600041103     C                   Z-ADD     1             CODUT
036700041103     C                   CALL      'X§PARUT'
036800041103     C                   PARM                    UT§DSE
036900041103     C                   MOVEL     RAGUT         RSUT
037000041103     c                   seton                                        of
037100041103     C                   ENDSR
037200041103     OYco3070P  E            TESTA          2 02
037300041103     O                       RSUT                20
037400041103     O                                         + 45 'LISTA ERRORI DURANTE'
037500041103     O                                         +  1 'IL CARICAMENTO DEL F'
037600041103     O                                         +  0 'ILE AGIP'
037700041103     O                                          150 'TRMZ53R'
037800041103     O                       UDATE              170 '  /  /  '
037900041103     O                       PAGE1         Z    198
038000041103     O          e            errfat      1
038100041103     O                                         +  0 'Il '
038200041103     O                                         +  0 'documento'
038300041103     o                       tpfat             +  1
038400041103     o                       desfat            +  1
038500041124     O                                         +  1 'è stato escluso'
038600041103     O                                         +  1 'dall''elaborazione'
038700041103     O                                         +  1 'VERIFICARE !'
038800041103     O          e            errtra      1
038900041103     O                                         +  0 'La '
039000041103     O                                         +  0 'transazione'
039100041103     o                       tptra             +  1
039200041103     o                       destra            +  1
039300041103     O                                         +  1 'è stata esclusa'
039400041103     O                                         +  1 'dall''elaborazione'
039500041103     O                                         +  1 'VERIFICARE !'
039600041123     O          e            errpro      1
039700041123     O                                         +  0 'Sono presenti delle '
039800041124     O                                         +  0 'transazioni '
039900041123     O                                         +  0 'con codice prodotto '
040000041123     O                                         +  0 'non gestito:'
040100041123     o                       codpro            +  1
040200041123     O                                         +  1 'VERIFICARE !'
