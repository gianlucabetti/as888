000100040820     H DECEDIT('0,') DATEDIT(*dmy.)
000200150205      **********************************************************************
000300150210      * TISIGER1 invio e/c bancali al cliente
000400150205      **********************************************************************
000500150210     FAZCGB00F  uf   e             disk    infds(cgbds)
000600150210     FAZAGB01L  if   e           k disk
000700150210      *
000800150216     FTISIGEP   o    E             PRINTER OFLIND(*in66) usropn
000900150205      **********************************************************************
001000150210     D*------------------
001100150210     D* DS REPERIMENTO DATI UTENTE
001200150210     D*-------------------
001300150210     D BS69DS        E DS                  EXTNAME(TIBS69DS) INZ
001400150210     D ACODS         E DS                  EXTNAME(CNACO00F) INZ
001500150210     D INDDS         E DS                  EXTNAME(CNIND00F) INZ
001600150210     D CLPDS         E DS                  EXTNAME(CNCLP00F) INZ
001700150210     D CLSDS         E DS                  EXTNAME(FNCLS00F) INZ
001800150210     D*------------------
001900150210     D* ARCHITETTURA
002000150210     D*------------------
002100150210     D KPJBA         E DS
002200150210      *
002300150211     D WrkSqlCmd       S           1024
002400150210     d azMGBds       E DS                  extname(AZMGB00F)
002500150210      *
002600150210     d CGBds           DS
002700150210     d  cgb_nrr              397    400b 0
002800150210     D*
002900040820     D WLBDAT          DS
003000040820     D  G02DAT                 1      8  0
003100040820     D  G02INV                 9     16  0
003200040820     D  G02ERR                17     17
003300040820     D  G02TGI                18     22  0
003400040820      *
003500150210     D savCOD          s                   like(mgbCOD)
003600150210      *
003700150210     D data_Oggi       s                   like(G02INV) INZ(0)
003800150210     D data_Ieri       s               d   datfmt(*eur)
003900150216     D data_SALDOini...
004000150216     D                 s               d   datfmt(*eur)
004100150206     d dataiso         s               d   datfmt(*iso)
004200150206     d dataeur         s               d   datfmt(*eur)
004300150210     d OGGIiso         s               d   datfmt(*iso)
004400150210     d DataLimite      s               d   datfmt(*iso)
004500040820      *
004600150210     D  almeno_uno     s              1    inz(' ')
004700150210     D  Solo_una_volta...
004800150210     D                 s              1    inz('S')
004900150210     D  Primo_MOV      s              1    inz('S')
005000150211     D  NewCOD         s              1
005100150223     D  tesSCO         s                   like(cgbsco)
005200150223     D  mgbQTAp        s                   like(mgbQTA)
005300150223     D  mgbQTAm        s                   like(mgbQTA)
005400150223     D  genFRAN        s              7s 0
005500150205      * ?------------------------------------------------------------------ */
005600150205     D Digits          C                   CONST('0123456789')
005700150205      * ?------------------------------------------------------------------ */
005800150206     C     *ENTRY        PLIST
005900150210     c                   parm                    KPJBA
006000150210     c                   movel     kpjbu         CGBrrn            9 0
006100150210      *
006200150210      * imposta subito come chiudere
006300150210     C                   SETON                                        LR
006400150210      *
006500150206      *  imposta UDATE di elaborazione
006600150210     C                   TIME                    W0140            14 0
006700150206     C                   move      *date         G02DAT
006800150206     C                   MOVE      *ZEROS        G02INV
006900150206     C                   MOVE      *BLANKS       G02ERR
007000150206     C                   CALL      'XSRDA8'
007100150206     C                   PARM                    WLBDAT
007200150206     C                   Z-ADD     G02INV        data_oggi
007300150206     C     *iso          move      data_oggi     dataiso
007400150210     c                   move      dataiso       oggiISO
007500150210     c                   subdur    1:*d          DataISO
007600150210     C                   move      DataISO       Data_Fine         8 0
007700150210     C                   move      DataISO       Data_Ieri
007800150206      **
007900150210      *  aggancia subito l'Anagrafica
008000150210     c                   clear                   trovato_CGB       1
008100150210     c     CGBrrn        chain     azCGB00F
008200150210     c                   if        %Found(azCGB00F)
008300150210     c                   move      'S'           trovato_CGB
008400150210      **                                               usati dopo nell'sql
008500150210     C                   z-add     CGBfre        Freq              3 0
008600150210     C                   move      CGBksu        Unificante        8
008700150210     C                   move      CGBcod1       CodArticolo1     15
008800150210     C                   move      CGBcod2       CodArticolo2     15
008900150210     C                   move      CGBcod3       CodArticolo3     15
009000150216      *
009100150210     C                   z-add     CGBdte        Data_Inizio       8 0
009200150216      *
009300150216      * si memorizza il giorno prima dell'inizio del range per il SALDO iniziale
009400150216     c                   if        Data_Inizio >0
009500150216     C     *iso          move      Data_Inizio   DataLimite
009600150216     c                   subdur    1:*d          DataLimite
009700150216     c                   move      DataLimite    DATAiniziale      8 0
009800150216     c                   move      DataLimite    Data_SALDOini
009900150216      * calcola il SALDO iniziale
010000150216     c                   exsr      SALDOINIZ
010100150216     c                   end
010200150216      *
010300150210     C                   move      CGBksu        codCLI            8 0
010400150210     C*
010500150210     C* REPERISCE la Rag.Sociale
010600150210     C                   CLEAR                   BS69DS
010700150210     C                   CLEAR                   ACODS
010800150210     C                   MOVEL     KNSIF         I69SIF                         *S.INFORMATIVO
010900150210     C                   MOVE      codCLI        I69KAC                         *CLIENTE X CNCLP
011000150210     C                   CALL      'TIBS69R'
011100150210     C                   PARM                    BS69DS
011200150210     C                   PARM                    ACODS
011300150210     C                   PARM                    INDDS
011400150210     C                   PARM                    CLPDS
011500150210     C                   PARM                    CLSDS
011600150210IF  1C     O69ERR        IFNE      '1'                                          *ERRORE
011700150223     C                   movel     ACORAG        tesCLI
011800150223     C                   z-add     ACOksc        tesCOD
011900150210E   1C                   ENDIF
012000150210     C*
012100150210     C                   z-add     CGBsco        tesSCO
012200150210     c                   end
012300150210      **
012400150216     c                   open      TISIGEP
012500150210      **
012600150209      ** -----------  descrizione SQL:
012700150210      ** record da leggere dei Movimenti:
012800150209      ** sono quelli che:
012900150210      **   che hanno il Tipo "C"(Cliente)
013000150210      **    (AND)
013100150210      **   nel range di date che va dall'ultimo invio mail a ieri
013200150210      **    (AND)
013300150210      **   che appartengono allo stesso codice unificante
013400150210      **    ( precisiamo che i movimenti sono già tutti sull'unificante )
013500150210      **    (AND)
013600150210      **   che appartengono ai codici articoli presenti in anagrafica
013700150210      **    (ORDINATI x Data e Ora movimento)
013800150209      ** -----------
013900150223      **  per avere i campi positivi accorpati e i negativi accorpati x stessa data
014000150223      **   c'è un file primario al quale in LEFT OUTER JOIN sono affiancati 2 file
014100150223      **   identici divisi per valori positivi e negativi.
014200150223      **  Le regole di where sono le stesse per tutti e 3 i files.
014300150223      **
014400150211     C/EXEC SQL
014500150223     C+ DECLARE MOV_MGB CURSOR FOR
014600150223     c+ select a.mgbcod, a.mgbdat,
014700150223     c+ CASE WHEN qtapiu  IS NOT NULL THEN qtapiu  ELSE 0 END,
014800150223     c+ CASE WHEN qtameno IS NOT NULL THEN qtameno ELSE 0 END
014900150223     c+       from (
015000150223     c+ select mgbcod, mgbdat
015100150223     c+ from azMGB00f where mgbTIP ='C' and mgbSOG = :Unificante
015200150223     c+ and mgbDAT between :Data_Inizio and :Data_Fine and
015300150223     c+ mgbCOD in (:CodArticolo1, :CodArticolo2, :CodArticolo3)
015400150223     c+ AND mgbCAU in (SELECT substr(tbeKE1, 1, 4) FROM
015500150223     c+ TNTBE00F WHERE tbeCOD = 'GGB' AND tbeatb=' ')
015600150223     c+ Group by mgbCOD, mgbDAT  Order by mgbCOD, mgbDAT )  a
015700150223     c+       left outer JOIN
015800150223     c+ (select mgbcod, mgbdat, sum(mgbqta) as qtapiu
015900150223     c+ from azMGB00f where mgbTIP ='C' and mgbSOG = :Unificante
016000150223     c+ and mgbDAT between :Data_Inizio and :Data_Fine and
016100150223     c+ mgbCOD in (:CodArticolo1, :CodArticolo2, :CodArticolo3)
016200150223     c+ AND mgbCAU in (SELECT substr(tbeKE1, 1, 4) FROM
016300150223     c+ TNTBE00F WHERE tbeCOD = 'GGB' AND tbeatb=' ')
016400150223     c+ and mgbQTA >=0           Group by mgbCOD, mgbDAT)  b
016500150223     c+       ON
016600150223     c+ a.mgbCOD=b.mgbCOD and a.mgbDAT=b.mgbDAT
016700150223     c+       left outer JOIN
016800150223     c+ (select mgbcod, mgbdat, sum(mgbqta) as qtameno
016900150223     c+ from azMGB00f where mgbTIP ='C' and mgbSOG = :Unificante
017000150223     c+ and mgbDAT between :Data_Inizio and :Data_Fine and
017100150223     c+ mgbCOD in (:CodArticolo1, :CodArticolo2, :CodArticolo3)
017200150223     c+ AND mgbCAU in (SELECT substr(tbeKE1, 1, 4) FROM
017300150223     c+ TNTBE00F WHERE tbeCOD = 'GGB' AND tbeatb=' ')
017400150223     c+ and mgbQTA < 0           Group by mgbCOD, mgbDAT)  c
017500150223     c+       ON
017600150223     c+ a.mgbCOD=c.mgbCOD and a.mgbDAT=c.mgbDAT
017700150226     c+ order by a.mgbcod, a.mgbdat
017800150223     C/END-EXEC
017900150211
018000011210     C/EXEC SQL
018100150210     C+ OPEN MOV_MGB
018200011210     C/END-EXEC
018300040820     C                   DOU       SqlCod <> 0
018400011205     C/EXEC SQL
018500150223     C+ FETCH NEXT FROM MOV_MGB INTO :mgbCOD, :mgbDAT, :mgbQTAp, :mgbQTAm
018600011205     C/END-EXEC
018700011210     C                   SELECT
018800041007     c*
018900041007     c* a fine file Totali x rotture
019000041007     C                   WHEN      SqlCod = 100
019100150210      *
019200041007     c                   if        almeno_uno ='S'
019300150210     c                   exsr      TOTALsav
019400041007     c                   else
019500150205     c                   exsr      NO_dati
019600041007     c                   end
019700150210     **
019800041007     c                   leave
019900041007     **
020000041007     C                   WHEN      SqlCod < 0
020100041007     C                   seton                                        H1
020200041007     c                   goto      fine
020300040820     c*
020400041007     C                   OTHER
020500150210     c*
020600150223     c                   eval      mgbqta = mgbqtap + mgbqtam
020700150210     c*
020800150210     c* Totale x rottura cod.Bancale
020900150210     c                   exsr      TOTALsav
021000040820     c* DETTAGLIO
021100150210     c                   exsr      unMOVIMENTO
021200150210     c                   write     RigaMOV
021300150210
021400011210     C                   ENDSL
021500040820
021600011205     C                   ENDDO
021700011205     C/EXEC SQL
021800150210     C+ CLOSE MOV_MGB
021900011205     C/END-EXEC
022000040823      *
022100150210     C* totali generali
022200150210     c                   if        almeno_uno ='S'
022300150210     c                   exsr      Tot_GENERALE
022400150210     c                   end
022500150210      *
022600150216     c                   close     TISIGEP
022700150211      *
022800150211      * AGGIORNA il FILE
022900150211      **  aveva agganciato l'anagrafica in testa al pgm
023000150211      **    per mail inviata aggiorna la data dell'anagrafica
023100150211     c                   IF        trovato_CGB = 'S'
023200150211      **         Oggi Data invio Mail
023300150211     c                   z-add     data_oggi     CGBdte
023400150211      *   aggiorna il record
023500150211     c                   update    azCGB000
023600150211     c                   endIF
023700150210      *
023800041007     c     fine          tag
023900150210     c                   RETURN
024000150211      * ?------------------------------------------------------------------ */
024100150211      *
024200150211      * ?------------------------------------------------------------------ */
024300150211     C     unMOVIMENTO   begSR
024400150211      **
024500150211     c                   eval      almeno_uno ='S'
024600150211      **
024700150211      **  Se la Data_Inizio = 0 -> unificante inserito e non è mai stata inviata
024800150211      **  la mail, occorre verificare se sono passati i giorni necessari dal primo
024900150211      **  movimento inserito.  Altrimenti USCIRE dal PGM.
025000150211     c                   if        Data_Inizio = 0 and primo_Mov = 'S'
025100150211      **
025200150211     c                   eval      primo_Mov = 'N'
025300150211     c     OggiISO       subdur    freq:*d       DataLimite
025400150211     c                   move      DataLimite    DATA_8            8 0
025500150216      **
025600150211      *  se NON sono ancora passati i TOT giorni esce dal PGM
025700150211     c                   if        mgbDAT > DATA_8
025800150211     c                   RETURN
025900150211     c                   end
026000150216      ***
026100150216      * si memorizza il giorno prima calcolato per il SALDO iniziale
026200150216     C     *iso          move      mgbDAT        DataLimite
026300150216     c                   subdur    1:*d          DataLimite
026400150216     C                   move      DataLimite    Data_SALDOini
026500150216     c                   clear                   iniqtaPOS
026600150216     c                   clear                   iniqtaNEG
026700150211      **
026800150216     c                   endIF
026900150216      *----
027000150216     c* Intestazione Stampa da inglobare alla Mail
027100150216     c*     senza gli Overflow
027200150216      *                 * ------------------ *
027300150216     c                   if        Solo_una_volta = 'S'
027400150216      *                 * ------------------ *
027500150216     c                   move      Data_SALDOini INIdat
027600150216      *
027700150216     c                   write     inTESTAz
027800150216     c                   write     saldoINI
027900150216     c                   write     tesRIGA
028000150216      *                 * ------------------ *
028100150216     c                   eval      Solo_una_volta = 'N'
028200150216     c                   end
028300150216     c*
028400150211      *----
028500150211      **  prepara la riga da stampare
028600150211     c                   clear                   DETDAT
028700150211     c                   clear                   DETCOD
028800150211     c                   clear                   DETQTAPOS
028900150211     c                   clear                   DETQTANEG
029000150211     C                   add       mgbQTA        TotNRbancali
029100150211      *
029200150211     C     *iso          move      mgbDAT        dataiso
029300150211     c                   move      dataiso       dataeur
029400150211     c                   move      dataeur       detDAT
029500150211      *
029600150211     c     mgbCOD        chain     AZAGB01L
029700150211     c                   if        %Found(AZAGB01L) and
029800150211     c                                newCOD = 'S'
029900150211     c                   movel     agbDES        detCOD
030000150211     c                   eval         newCOD = 'N'
030100150211     c                   end
030200150223      *
030300150223     C                   z-add     mgbQTAp       detQTAPOS
030400150223     C                   z-sub     mgbQTAm       detQTANEG
030500150223      *
030600150223     C                   add       mgbQTAp       savQTAPOS
030700150223     C                   sub       mgbQTAm       savQTANEG
030800150211      *
030900150211     C                   ENDSR
031000150210      * ?------------------------------------------------------------------ */
031100150210      *
031200150210      * ?------------------------------------------------------------------ */
031300150210     C     TOTALsav      begSR
031400150210      *
031500150210      * ROTTURA x
031600150210      *  codice articolo Bancale o ( EoF )
031700150210     c                   if        mgbCOD <> savCOD or SqlCod = 100
031800150211     c                   eval         newCOD = 'S'
031900150210      *
032000150210      *   stampa i totali del giro precedente
032100150210     c                   if             almeno_uno ='S'
032200150211      *
032300150211      *  Riga totale bancali
032400150210     c                   write     TOTbanc
032500150211      *
032600150211     C                   add       savQTAPOS     genTOTPOS
032700150211     C                   add       savQTANEG     genTOTNEG
032800150211     c                   clear                   savQTAPOS
032900150211     c                   clear                   savQTANEG
033000150213      * separatore
033100150213     c                   if        SqlCod <> 100
033200150213     c                   write     tesRIGA
033300150213     c                   end
033400150210     c                   end
033500150210      * totalizza
033600150211     C                   add       TotNRbancali  GENtot           11 0
033700150210      **  pulizia totale x Tipo Bancale
033800150210     C                   clear                   TotNRbancali     11 0
033900150210     c                   eval      savCOD = mgbCOD
034000150210     c                   end
034100150210      *
034200150210     C                   ENDSR
034300150210      * ?------------------------------------------------------------------ */
034400150210      *
034500150210      * ?------------------------------------------------------------------ */
034600150210     C     Tot_GENERALE  begSR
034700150210      **
034800150210      **   stampa il tot.generale
034900150210     c                   move      data_Ieri     GENDAT
035000150211      *   riga stampa
035100150210     c                   write     totalGEN
035200150211      **
035300150211      **  solo se dobbiamo restituire
035400150223     c                   if        (genTOTNEG - genTOTPOS) > 0
035500150223     c                   eval(h)   GENfran =(genTOTNEG - genTOTPOS)
035600150223      * se però c'è la % di franchigia,
035700150223     c                   if        tesSCO > 0
035800150223     c                   eval(h)   GENfran =(genTOTNEG - genTOTPOS)  -
035900150223     c                                      (genTOTNEG - genTOTPOS)*tesSCO/100
036000150223     c                   end
036100150223      *
036200150223     c                   eval      SALRENDE = 'Saldo a rendere di ' +
036300150223     c                             %trim(%editc(GENfran:'Z')) +
036400150223     c                             ' bancali.'
036500150223      *
036600150223     c                   if        tesSCO > 0
036700150223     c                   eval      SALRENDE = %Trim(SALRENDE) +
036800150223     c                             ' Al netto della Franchigia del ' +
036900150223     c                              %Trim(%editc(tesSCO:'Z')) + ' %'
037000150223     c                   end
037100150211      *
037200150211      *   riga del RENDERE al NETTO della FRANCHIGIA concordata
037300150211     c                   write     totFRANC
037400150223     c                   end
037500150210      **----------
037600150210      *
037700150210     c                   ENDSR
037800150210      * ?------------------------------------------------------------------ */
037900150210      *
038000150210      * ?------------------------------------------------------------------ */
038100150210     C     NO_dati       begSR
038200150210      **
038300150210      **  non deve fare nulla
038400150210      **
038500150210     C                   ENDSR
038600150206      * ?------------------------------------------------------------------ */
038700150206     C* INIZIALIZZAZIONE
038800150206      * ?------------------------------------------------------------------ */
038900150206     C     *inzsr        BEGSR
039000150206     c*
039100150211     c*
039200150206     C                   ENDSR
039300150216      * ?------------------------------------------------------------------ */
039400150216     C* cerco il saldo Iniziale con istruzione SQL
039500150216      * ?------------------------------------------------------------------ */
039600150216     C     SALDOINIZ     BEGSR
039700150216      *
039800150216     c                   clear                   TOT_INIZIO       11 0
039900150216     C/EXEC SQL
040000150216     C+ SELECT sum(mgbqta) INTO :TOT_INIZIO from azMGB00f where
040100150216     c+   mgbTIP ='C' and mgbDAT <= :DataINIZIALE  and mgbSOG = :Unificante
040200150216     c+ AND mgbCOD in (:CodArticolo1, :CodArticolo2, :CodArticolo3)
040300150216     c+ AND mgbCAU in
040400150216     c+    (SELECT substr(tbeKE1, 1, 4) FROM TNTBE00F WHERE
040500150216     c+    tbeCOD = 'GGB' AND tbeatb=' ')
040600150216     C/END-EXEC
040700150216      **
040800150216     c                   if        TOT_INIZIO >= 0
040900150216     c                   z-add     tot_Inizio    INIqtaPOS
041000150216     c                   else
041100150216     c                   z-sub     tot_Inizio    INIqtaNEG
041200150216     c                   end
041300150216      **
041400150216     C                   add       INIqtaPOS     genTOTPOS
041500150216     C                   add       INIqtaNEG     genTOTNEG
041600150216      **
041700150216     C                   ENDSR
041800150216      * ?------------------------------------------------------------------ */
