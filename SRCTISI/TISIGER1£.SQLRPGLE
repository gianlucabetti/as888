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
004900150211     D  NewCOD         s              1
005000150223     D  tesSCO         s                   like(cgbsco)
005100150223     D  mgbQTAp        s                   like(mgbQTA)
005200150223     D  mgbQTAm        s                   like(mgbQTA)
005300150223     D  genFRAN        s              7s 0
005400150205      * ?------------------------------------------------------------------ */
005500150205     D Digits          C                   CONST('0123456789')
005600150205      * ?------------------------------------------------------------------ */
005700150206     C     *ENTRY        PLIST
005800150210     c                   parm                    KPJBA
005900150210     c                   movel     kpjbu         CGBrrn            9 0
006000150210      *
006100150210      * imposta subito come chiudere
006200150210     C                   SETON                                        LR
006300150210      *
006400150206      *  imposta UDATE di elaborazione
006500150210     C                   TIME                    W0140            14 0
006600150206     C                   move      *date         G02DAT
006700150206     C                   MOVE      *ZEROS        G02INV
006800150206     C                   MOVE      *BLANKS       G02ERR
006900150206     C                   CALL      'XSRDA8'
007000150206     C                   PARM                    WLBDAT
007100150206     C                   Z-ADD     G02INV        data_oggi
007200150206     C     *iso          move      data_oggi     dataiso
007300150210     c                   move      dataiso       oggiISO
007400150210     c                   subdur    1:*d          DataISO
007500150210     C                   move      DataISO       Data_Fine         8 0
007600150210     C                   move      DataISO       Data_Ieri
007700150206      **
007800150210      *  aggancia subito l'Anagrafica
007900150210     c                   clear                   trovato_CGB       1
008000150210     c     CGBrrn        chain     azCGB00F
008100150210     c                   if        %Found(azCGB00F)
008200150210     c                   move      'S'           trovato_CGB
008300150210      **                                               usati dopo nell'sql
008400150210     C                   z-add     CGBfre        Freq              3 0
008500150210     C                   move      CGBksu        Unificante        8
008600150210     C                   move      CGBcod1       CodArticolo1     15
008700150210     C                   move      CGBcod2       CodArticolo2     15
008800150210     C                   move      CGBcod3       CodArticolo3     15
008900150216      *
009000150210     C                   z-add     CGBdte        Data_Inizio       8 0
009100150226      **
009200150226      **  SALDO INIZIALE
009300150226      **
009400150226      **  se la Data_Inizio = 0 -> non è mai stata inviata la mail.
009500150226     c                   if        Data_Inizio =0
009600150226      **
009700150226      **     la imposta con la data attivazione dall'anagrafica
009800150226     C                   move      cgbDTI        Data_Inizio
009900150226      **
010000150226      **  occorre verificare se sono passati i giorni necessari dal primo
010100150226      **  movimento inserito x inviare la mail.  Altrimenti USCIRE dal PGM.
010200150226     c     OggiISO       subdur    freq:*d       DataLimite
010300150226     c                   move      DataLimite    DATA_8            8 0
010400150226     c                   clear                   Data_PrimoMOV     8 0
010500150226     C/EXEC SQL
010600150226     C+ SELECT MIN(mgbdat) INTO :Data_PrimoMOV   from azMGB00f where
010700150226     c+   mgbTIP ='C' and mgbDAT >= :Data_Inizio  and mgbSOG = :Unificante
010800150226     c+ AND mgbCOD in (:CodArticolo1, :CodArticolo2, :CodArticolo3)
010900150226     c+ AND mgbCAU in
011000150226     c+               (SELECT substr(tbeKE1, 1, 4) FROM TNTBE00F WHERE
011100150226     c+                tbeCOD = 'GGB' AND tbeatb=' ')
011200150226     C/END-EXEC
011300150226      *  se NON sono ancora passati i TOT giorni esce dal PGM
011400150226     c                   if        Data_PrimoMOV > DATA_8
011500150226     c                   RETURN
011600150226     c                   end
011700150226      *
011800150226      *  imposta la DATA ed il SALDO iniziale a ZERO
011900150226     C     *iso          move      Data_Inizio   DataLimite
012000150226     C                   move      DataLimite    Data_SALDOini
012100150226     c                   clear                   iniqtaPOS
012200150226     c                   clear                   iniqtaNEG
012300150226      ***
012400150226     c                   ELSE
012500150226      ***
012600150226      * INVECE, SE AVEVA L'ULTIMA DATA MAIL SULL'ANAGRAFICA,
012700150226      * si memorizza il giorno prima dell'inizio del range
012800150226      * per calcolare il SALDO iniziale
012900150216     C     *iso          move      Data_Inizio   DataLimite
013000150216     c                   subdur    1:*d          DataLimite
013100150216     c                   move      DataLimite    DATAiniziale      8 0
013200150216     c                   move      DataLimite    Data_SALDOini
013300150216     c                   exsr      SALDOINIZ
013400150216     c                   end
013500150216      *
013600150210     C                   move      CGBksu        codCLI            8 0
013700150210     C*
013800150210     C* REPERISCE la Rag.Sociale
013900150210     C                   CLEAR                   BS69DS
014000150210     C                   CLEAR                   ACODS
014100150210     C                   MOVEL     KNSIF         I69SIF                         *S.INFORMATIVO
014200150210     C                   MOVE      codCLI        I69KAC                         *CLIENTE X CNCLP
014300150210     C                   CALL      'TIBS69R'
014400150210     C                   PARM                    BS69DS
014500150210     C                   PARM                    ACODS
014600150210     C                   PARM                    INDDS
014700150210     C                   PARM                    CLPDS
014800150210     C                   PARM                    CLSDS
014900150210IF  1C     O69ERR        IFNE      '1'                                          *ERRORE
015000150223     C                   movel     ACORAG        tesCLI
015100150223     C                   z-add     ACOksc        tesCOD
015200150210E   1C                   ENDIF
015300150210     C*
015400150210     C                   z-add     CGBsco        tesSCO
015500150210     c                   end
015600150226      **
015700150216     c                   open      TISIGEP
015800150210      **
015900150209      ** -----------  descrizione SQL:
016000150210      ** record da leggere dei Movimenti:
016100150209      ** sono quelli che:
016200150210      **   che hanno il Tipo "C"(Cliente)
016300150210      **    (AND)
016400150210      **   nel range di date che va dall'ultimo invio mail a ieri
016500150210      **    (AND)
016600150210      **   che appartengono allo stesso codice unificante
016700150210      **    ( precisiamo che i movimenti sono già tutti sull'unificante )
016800150210      **    (AND)
016900150210      **   che appartengono ai codici articoli presenti in anagrafica
017000150210      **    (ORDINATI x Data e Ora movimento)
017100150209      ** -----------
017200150223      **  per avere i campi positivi accorpati e i negativi accorpati x stessa data
017300150223      **   c'è un file primario al quale in LEFT OUTER JOIN sono affiancati 2 file
017400150223      **   identici divisi per valori positivi e negativi.
017500150223      **  Le regole di where sono le stesse per tutti e 3 i files.
017600150223      **
017700150211     C/EXEC SQL
017800150223     C+ DECLARE MOV_MGB CURSOR FOR
017900150223     c+ select a.mgbcod, a.mgbdat,
018000150223     c+ CASE WHEN qtapiu  IS NOT NULL THEN qtapiu  ELSE 0 END,
018100150223     c+ CASE WHEN qtameno IS NOT NULL THEN qtameno ELSE 0 END
018200150223     c+       from (
018300150223     c+ select mgbcod, mgbdat
018400150223     c+ from azMGB00f where mgbTIP ='C' and mgbSOG = :Unificante
018500150223     c+ and mgbDAT between :Data_Inizio and :Data_Fine and
018600150223     c+ mgbCOD in (:CodArticolo1, :CodArticolo2, :CodArticolo3)
018700150223     c+ AND mgbCAU in (SELECT substr(tbeKE1, 1, 4) FROM
018800150223     c+ TNTBE00F WHERE tbeCOD = 'GGB' AND tbeatb=' ')
018900150223     c+ Group by mgbCOD, mgbDAT  Order by mgbCOD, mgbDAT )  a
019000150223     c+       left outer JOIN
019100150223     c+ (select mgbcod, mgbdat, sum(mgbqta) as qtapiu
019200150223     c+ from azMGB00f where mgbTIP ='C' and mgbSOG = :Unificante
019300150223     c+ and mgbDAT between :Data_Inizio and :Data_Fine and
019400150223     c+ mgbCOD in (:CodArticolo1, :CodArticolo2, :CodArticolo3)
019500150223     c+ AND mgbCAU in (SELECT substr(tbeKE1, 1, 4) FROM
019600150223     c+ TNTBE00F WHERE tbeCOD = 'GGB' AND tbeatb=' ')
019700150223     c+ and mgbQTA >=0           Group by mgbCOD, mgbDAT)  b
019800150223     c+       ON
019900150223     c+ a.mgbCOD=b.mgbCOD and a.mgbDAT=b.mgbDAT
020000150223     c+       left outer JOIN
020100150223     c+ (select mgbcod, mgbdat, sum(mgbqta) as qtameno
020200150223     c+ from azMGB00f where mgbTIP ='C' and mgbSOG = :Unificante
020300150223     c+ and mgbDAT between :Data_Inizio and :Data_Fine and
020400150223     c+ mgbCOD in (:CodArticolo1, :CodArticolo2, :CodArticolo3)
020500150223     c+ AND mgbCAU in (SELECT substr(tbeKE1, 1, 4) FROM
020600150223     c+ TNTBE00F WHERE tbeCOD = 'GGB' AND tbeatb=' ')
020700150223     c+ and mgbQTA < 0           Group by mgbCOD, mgbDAT)  c
020800150223     c+       ON
020900150223     c+ a.mgbCOD=c.mgbCOD and a.mgbDAT=c.mgbDAT
021000150226     c+ order by a.mgbcod, a.mgbdat
021100150223     C/END-EXEC
021200150211
021300011210     C/EXEC SQL
021400150210     C+ OPEN MOV_MGB
021500011210     C/END-EXEC
021600040820     C                   DOU       SqlCod <> 0
021700011205     C/EXEC SQL
021800150223     C+ FETCH NEXT FROM MOV_MGB INTO :mgbCOD, :mgbDAT, :mgbQTAp, :mgbQTAm
021900011205     C/END-EXEC
022000011210     C                   SELECT
022100041007     c*
022200041007     c* a fine file Totali x rotture
022300041007     C                   WHEN      SqlCod = 100
022400150210      *
022500041007     c                   if        almeno_uno ='S'
022600150210     c                   exsr      TOTALsav
022700041007     c                   else
022800150205     c                   exsr      NO_dati
022900041007     c                   end
023000150210     **
023100041007     c                   leave
023200041007     **
023300041007     C                   WHEN      SqlCod < 0
023400041007     C                   seton                                        H1
023500041007     c                   goto      fine
023600040820     c*
023700041007     C                   OTHER
023800150210     c*
023900150223     c                   eval      mgbqta = mgbqtap + mgbqtam
024000150210     c*
024100150210     c* Totale x rottura cod.Bancale
024200150210     c                   exsr      TOTALsav
024300040820     c* DETTAGLIO
024400150210     c                   exsr      unMOVIMENTO
024500150210     c                   write     RigaMOV
024600150210
024700011210     C                   ENDSL
024800040820
024900011205     C                   ENDDO
025000011205     C/EXEC SQL
025100150210     C+ CLOSE MOV_MGB
025200011205     C/END-EXEC
025300040823      *
025400150210     C* totali generali
025500150210     c                   if        almeno_uno ='S'
025600150210     c                   exsr      Tot_GENERALE
025700150210     c                   end
025800150210      *
025900150216     c                   close     TISIGEP
026000150211      *
026100150211      * AGGIORNA il FILE
026200150211      **  aveva agganciato l'anagrafica in testa al pgm
026300150211      **    per mail inviata aggiorna la data dell'anagrafica
026400150211     c                   IF        trovato_CGB = 'S'
026500150211      **         Oggi Data invio Mail
026600150211     c                   z-add     data_oggi     CGBdte
026700150211      *   aggiorna il record
026800150211     c                   update    azCGB000
026900150211     c                   endIF
027000150210      *
027100041007     c     fine          tag
027200150210     c                   RETURN
027300150211      * ?------------------------------------------------------------------ */
027400150211      *
027500150211      * ?------------------------------------------------------------------ */
027600150211     C     unMOVIMENTO   begSR
027700150211      **
027800150211     c                   eval      almeno_uno ='S'
027900150216      *----
028000150216     c* Intestazione Stampa da inglobare alla Mail
028100150216     c*     senza gli Overflow
028200150216      *                 * ------------------ *
028300150216     c                   if        Solo_una_volta = 'S'
028400150216      *                 * ------------------ *
028500150216     c                   move      Data_SALDOini INIdat
028600150216      *
028700150216     c                   write     inTESTAz
028800150216     c                   write     saldoINI
028900150216     c                   write     tesRIGA
029000150216      *                 * ------------------ *
029100150216     c                   eval      Solo_una_volta = 'N'
029200150216     c                   end
029300150216     c*
029400150211      *----
029500150211      **  prepara la riga da stampare
029600150211     c                   clear                   DETDAT
029700150211     c                   clear                   DETCOD
029800150211     c                   clear                   DETQTAPOS
029900150211     c                   clear                   DETQTANEG
030000150211     C                   add       mgbQTA        TotNRbancali
030100150211      *
030200150211     C     *iso          move      mgbDAT        dataiso
030300150211     c                   move      dataiso       dataeur
030400150211     c                   move      dataeur       detDAT
030500150211      *
030600150211     c     mgbCOD        chain     AZAGB01L
030700150211     c                   if        %Found(AZAGB01L) and
030800150211     c                                newCOD = 'S'
030900150211     c                   movel     agbDES        detCOD
031000150211     c                   eval         newCOD = 'N'
031100150211     c                   end
031200150223      *
031300150223     C                   z-add     mgbQTAp       detQTAPOS
031400150223     C                   z-sub     mgbQTAm       detQTANEG
031500150223      *
031600150223     C                   add       mgbQTAp       savQTAPOS
031700150223     C                   sub       mgbQTAm       savQTANEG
031800150211      *
031900150211     C                   ENDSR
032000150210      * ?------------------------------------------------------------------ */
032100150210      *
032200150210      * ?------------------------------------------------------------------ */
032300150210     C     TOTALsav      begSR
032400150210      *
032500150210      * ROTTURA x
032600150210      *  codice articolo Bancale o ( EoF )
032700150210     c                   if        mgbCOD <> savCOD or SqlCod = 100
032800150211     c                   eval         newCOD = 'S'
032900150210      *
033000150210      *   stampa i totali del giro precedente
033100150210     c                   if             almeno_uno ='S'
033200150211      *
033300150211      *  Riga totale bancali
033400150210     c                   write     TOTbanc
033500150211      *
033600150211     C                   add       savQTAPOS     genTOTPOS
033700150211     C                   add       savQTANEG     genTOTNEG
033800150211     c                   clear                   savQTAPOS
033900150211     c                   clear                   savQTANEG
034000150213      * separatore
034100150213     c                   if        SqlCod <> 100
034200150213     c                   write     tesRIGA
034300150213     c                   end
034400150210     c                   end
034500150210      * totalizza
034600150211     C                   add       TotNRbancali  GENtot           11 0
034700150210      **  pulizia totale x Tipo Bancale
034800150210     C                   clear                   TotNRbancali     11 0
034900150210     c                   eval      savCOD = mgbCOD
035000150210     c                   end
035100150210      *
035200150210     C                   ENDSR
035300150210      * ?------------------------------------------------------------------ */
035400150210      *
035500150210      * ?------------------------------------------------------------------ */
035600150210     C     Tot_GENERALE  begSR
035700150210      **
035800150210      **   stampa il tot.generale
035900150210     c                   move      data_Ieri     GENDAT
036000150211      *   riga stampa
036100150210     c                   write     totalGEN
036200150211      **
036300150211      **  solo se dobbiamo restituire
036400150223     c                   if        (genTOTNEG - genTOTPOS) > 0
036500150223     c                   eval(h)   GENfran =(genTOTNEG - genTOTPOS)
036600150223      * se però c'è la % di franchigia,
036700150223     c                   if        tesSCO > 0
036800150223     c                   eval(h)   GENfran =(genTOTNEG - genTOTPOS)  -
036900150223     c                                      (genTOTNEG - genTOTPOS)*tesSCO/100
037000150223     c                   end
037100150223      *
037200150223     c                   eval      SALRENDE = 'Saldo a rendere di ' +
037300150223     c                             %trim(%editc(GENfran:'Z')) +
037400150223     c                             ' bancali.'
037500150223      *
037600150223     c                   if        tesSCO > 0
037700150223     c                   eval      SALRENDE = %Trim(SALRENDE) +
037800150223     c                             ' Al netto della Franchigia del ' +
037900150223     c                              %Trim(%editc(tesSCO:'Z')) + ' %'
038000150223     c                   end
038100150211      *
038200150211      *   riga del RENDERE al NETTO della FRANCHIGIA concordata
038300150211     c                   write     totFRANC
038400150223     c                   end
038500150313     c                   write     ASTERISC
038600150210      **----------
038700150210      *
038800150210     c                   ENDSR
038900150210      * ?------------------------------------------------------------------ */
039000150210      *
039100150210      * ?------------------------------------------------------------------ */
039200150210     C     NO_dati       begSR
039300150210      **
039400150210      **  non deve fare nulla
039500150210      **
039600150210     C                   ENDSR
039700150206      * ?------------------------------------------------------------------ */
039800150206     C* INIZIALIZZAZIONE
039900150206      * ?------------------------------------------------------------------ */
040000150206     C     *inzsr        BEGSR
040100150206     c*
040200150211     c*
040300150206     C                   ENDSR
040400150216      * ?------------------------------------------------------------------ */
040500150216     C* cerco il saldo Iniziale con istruzione SQL
040600150216      * ?------------------------------------------------------------------ */
040700150216     C     SALDOINIZ     BEGSR
040800150216      *
040900150216     c                   clear                   TOT_INIZIO       11 0
041000150216     C/EXEC SQL
041100150216     C+ SELECT sum(mgbqta) INTO :TOT_INIZIO from azMGB00f where
041200150216     c+   mgbTIP ='C' and mgbDAT <= :DataINIZIALE  and mgbSOG = :Unificante
041300150216     c+ AND mgbCOD in (:CodArticolo1, :CodArticolo2, :CodArticolo3)
041400150216     c+ AND mgbCAU in
041500150216     c+    (SELECT substr(tbeKE1, 1, 4) FROM TNTBE00F WHERE
041600150216     c+    tbeCOD = 'GGB' AND tbeatb=' ')
041700150216     C/END-EXEC
041800150216      **
041900150216     c                   if        TOT_INIZIO >= 0
042000150216     c                   z-add     tot_Inizio    INIqtaPOS
042100150216     c                   else
042200150216     c                   z-sub     tot_Inizio    INIqtaNEG
042300150216     c                   end
042400150216      **
042500150216     C                   add       INIqtaPOS     genTOTPOS
042600150216     C                   add       INIqtaNEG     genTOTNEG
042700150216      **
042800150216     C                   ENDSR
042900150216      * ?------------------------------------------------------------------ */
