000100040820     H DECEDIT('0,') DATEDIT(*dmy.)
000200050510      **********************************************************************
000300050524     ffiapd01l  if   E           K DISK
000400051027-->  fTnvrab84p o    E             PRINTER OFLIND(*in66)
000500050510      **-----------------------------------------------------------------***
000600040820     d KPJBA         E DS
000700050511     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
000800040820     D*
000900051027-->  D DsPassaggio   e DS                  EXTNAME(TISE85DS)
001000050510     D fiftt00f      e DS
001100050510     D azorg00f      e DS
001200040820     D*
001300040820     D WLBDAT          DS
001400040820     D  G02DAT                 1      8  0
001500040820     D  G02INV                 9     16  0
001600040820     D  G02ERR                17     17
001700040820     D  G02TGI                18     22  0
001800040820      *
001900040820     D  data_Oggi      s                   like(G02INV) INZ(0)
002000040826     D  almeno_uno     s              1
002100050511     D*-------------
002200050511     D* Reperimento nome PGM
002300050511     D STATUS         SDS           333
002400050511     D  DSPGM            *PROC
002500050511     D*-------------
002600050511      *
002700040820     D digits          C                   CONST('0123456789')
002800050511      * ?------------------------------------------------------------------ */
002900051027      *  Spedizioni trattate in settmbre 2005 con CTM: 7P,7Q,7R,7T da TITAS
003000050511      * ?------------------------------------------------------------------ */
003100051027     c                   clear                   totsped09         7 0
003200051027     C/EXEC SQL
003300051027     C+ SELECT count(*) INTO :TOTSPED09 FROM edpab/etierr09
003400051027     C/END-EXEC
003500051027      ***                                                nr: 628.343
003600051027      * ?------------------------------------------------------------------ */
003700051027      * ?- tutte i costi di RITIRI di settembre da FIFCE
003800051027      * ?-  creato EDPAB/FCERIT09 =
003900051027      * ?-
004000051027      * ?-    SELECT * FROM fifce00f WHERE FCETSR ='R' and fceddc between
004100051027      * ?-    20050901 and 20050930 and fcecce= '997'
004200051027      ***   (considerare che il costo etichettatura va attribuito alla voce 997)
004300051027      ***                            Tutte le Bolle sett.: 1.005.385
004400051027      * ?------------------------------------------------------------------ */
004500051027      ***   Prendo un valore di massima di tutte le Etichettature di Ritiro di Settmebre
004600051027      * ?-  SELECT sum(fceice) FROM edpab/fcerit09
004700051027      ***   (costo totale della voce 997 su tutte le bolle)
004800051027      ***                                               Eur: 128.681,366
004900051027      **
005000051027      **    Occorre considerare poi solo quelle pagate !!!! e non tutte .
005100051027      * ?------------------------------------------------------------------ */
005200051027      *  - tutte le spedizioni su distinte di RITIRI di settembre da FIFTD
005300051027      * ?-  creato EDPAB/FTDRIT09 =
005400051027      * ?-
005500051027      * ?-    select * from fiftd00f where ftdtsr='R' and ftdddc between
005600051027      * ?-    20050901 and 20050930
005700051027      ***
005800051027      * ?------------------------------------------------------------------ */
005900051027      *  - TESTATE Distinte RITIRI di settembre SENZA pagamento Etichettatura FIFTT
006000051027      * ?- creato EDPAB/FTTRIT09N =
006100051027      * ?-
006200051027      * ?-    SELECT * FROM fiftt00f WHERE ftttsr='R' and fttpep ='N' and fttddc
006300051027      * ?-    between 20050901 and 20050930
006400051027      ***
006500051027      * ?------------------------------------------------------------------ */
006600051027      *  - TESTATE Distinte RITIRI di settembre CON pagamento Etichettatura FIFTT
006700051027      * ?- creato EDPAB/FTTRIT09S =
006800051027      * ?-
006900051027      * ?-    SELECT * FROM fiftt00f WHERE ftttsr='R' and fttpep ='S' and fttddc
007000051027      * ?-    between 20050901 and 20050930
007100051027      ***
007200051027      * ?------------------------------------------------------------------ */
007300051027      *  Tot Sped. Ritirate senza Etichettatura da distinte Ritiri
007400051027      *     da TITAS prende le spedizioni presenti sui Dettagli Padr.
007500051027      *     di quei documenti dove NON è pagata l'etichettatura.
007600051027      * ?------------------------------------------------------------------ */
007700051027     c                   clear                   nonpaga09         7 0
007800051027      ***
007900051027     C/EXEC SQL
008000051027     C+ select count(*) INTO :nonpaga09
008100051027     C+ from edpab/ETIERR09 where
008200051027     C+ digits(tasaas)!!digits(tasLNP)!!digits(tasNRS)!!digits(tasnsp) IN
008300051027     C+ (SELECT
008400051027     C+ digits(ftdaas)!!digits(ftdLNP)!!digits(ftdNRS)!!digits(ftdnsp)
008500051027     C+ FROM edpab/ftdrit09 WHERE
008600051027     C+ digits(ftdpdr)!!digits(ftdndc)!!digits(ftdddc) in
008700051027     C+ (SELECT digits(fttpdr)!!digits(fttndc)!!digits(fttddc)
008800051027     C+ FROM edpab/fttrit09N))
008900051027     C/END-EXEC
009000051027      ***                                                nr: 295.473
009100051027      * ?-
009200051027      * ?-
009300051027      * ?------------------------------------------------------------------ */
009400051027      * ?- creato EDPAB/FTDRIT09N
009500051027      * ?-
009600051027      * ?-   select *
009700051027      * ?-   from edpab/ETIERR09 where
009800051027      * ?-   digits(tasaas)!!digits(tasLNP)!!digits(tasNRS)!!digits(tasnsp) IN
009900051027      * ?-   (SELECT
010000051027      * ?-   digits(ftdaas)!!digits(ftdLNP)!!digits(ftdNRS)!!digits(ftdnsp)
010100051027      * ?-   FROM edpab/ftdrit09 WHERE
010200051027      * ?-   digits(ftdpdr)!!digits(ftdndc)!!digits(ftdddc) in
010300051027      * ?-   (SELECT digits(fttpdr)!!digits(fttndc)!!digits(fttddc)
010400051027      * ?-   FROM edpab/fttrit09N))
010500051027      * ?-
010600051027      * ?-
010700051027      * ?------------------------------------------------------------------ */
010800051027      * ?- creato EDPAB/FTDRIT09S
010900051027      * ?-
011000051027      * ?-   select *
011100051027      * ?-   from edpab/ETIERR09 where
011200051027      * ?-   digits(tasaas)!!digits(tasLNP)!!digits(tasNRS)!!digits(tasnsp) IN
011300051027      * ?-   (SELECT
011400051027      * ?-   digits(ftdaas)!!digits(ftdLNP)!!digits(ftdNRS)!!digits(ftdnsp)
011500051027      * ?-   FROM edpab/ftdrit09 WHERE
011600051027      * ?-   digits(ftdpdr)!!digits(ftdndc)!!digits(ftdddc) in
011700051027      * ?-   (SELECT digits(fttpdr)!!digits(fttndc)!!digits(fttddc)
011800051027      * ?-   FROM edpab/fttrit09S))
011900051027      * ?-
012000051027      * ?-
012100051027      * ?-
012200051027      * ?-
012300051027      * ?-
012400051027      * ?------------------------------------------------------------------ */
012500051027      *  Tot Sped. Ritirate CON  Etichettatura da distinte Ritiri
012600051027      *     da TITAS prende le spedizioni presenti sui Dettagli Padr.
012700051027      *     di quei documenti dove è pagata l'etichettatura.
012800051027      * ?------------------------------------------------------------------ */
012900051027     c                   clear                   sipaga09          7 0
013000051027      ***
013100051027     C/EXEC SQL
013200051027     C+ select count(*) INTO :sipaga09
013300051027     C+ from edpab/ETIERR09 where
013400051027     C+ digits(tasaas)!!digits(tasLNP)!!digits(tasNRS)!!digits(tasnsp) IN
013500051027     C+ (SELECT
013600051027     C+ digits(ftdaas)!!digits(ftdLNP)!!digits(ftdNRS)!!digits(ftdnsp)
013700051027     C+ FROM edpab/ftdrit09 WHERE
013800051027     C+ digits(ftdpdr)!!digits(ftdndc)!!digits(ftdddc) in
013900051027     C+ (SELECT digits(fttpdr)!!digits(fttndc)!!digits(fttddc)
014000051027     C+ FROM edpab/fttrit09S))
014100051027     C/END-EXEC
014200051027      ***                                                nr:  81.045
014300051027      * ?------------------------------------------------------------------ */
014400051027      *     da TITAS prende quelli presenti in Dettagli Pagati
014500051027      * ?- creato EDPAB/TASFTD09
014600051027      * ?-
014700051027      * ?-    select *
014800051027      * ?-    from edpab/etierr09 where
014900051027      * ?-    digits(tasaas)!!digits(tasLNP)!!digits(tasNRS)!!digits(tasnsp) IN
015000051027      * ?-    (SELECT
015100051027      * ?-    digits(ftdaas)!!digits(ftdLNP)!!digits(ftdNRS)!!digits(ftdnsp)
015200051027      * ?-    FROM edpab/ftdrit09 WHERE
015300051027      * ?-    digits(ftdpdr)!!digits(ftdndc)!!digits(ftdddc) in
015400051027      * ?-    (SELECT digits(fttpdr)!!digits(fttndc)!!digits(fttddc)
015500051027      * ?-    FROM edpab/fttrit09S))
015600051027      ***
015700051027      * ?------------------------------------------------------------------ */
015800051027      *     da FIFTD prende quelli presenti in Dettagli Pagati
015900051027      * ?- creato EDPAB/FTDRIT09V
016000051027      * ?-
016100051027      * ?-  SELECT  *
016200051027      * ?-  FROM edpab/ftdrit09 WHERE
016300051027      * ?-  digits(ftdpdr)!!digits(ftdndc)!!digits(ftdddc) in
016400051027      * ?-  (SELECT digits(fttpdr)!!digits(fttndc)!!digits(fttddc)
016500051027      * ?-  FROM edpab/fttrit09S)
016600051027      ***                                                   Nr.885.952
016700051027      ***   ma sono tutte le bolle ritirate con tutti i tipi di Tratt.Merce
016800051027      * ?------------------------------------------------------------------ */
016900051027      * ?- spedizioni su distinte di RITIRI di settembre nel dettaglio
017000051027      *     da FIFTD prende quelli presenti in Dettagli Pagati e su TITAS come
017100051027      *      Cod.Tratt.merce 7P/7Q/7R/7T
017200051027      *       (Questo è un di cui di tutto il FIFTD Pagato)
017300051027      * ?- creato EDPAB/FTDSEL09S
017400051027      * ?-
017500051027      * ?-  select * from (
017600051027      * ?-  SELECT * FROM edpab/ftdrit09 WHERE
017700051027      * ?-  digits(ftdpdr)!!digits(ftdndc)!!digits(ftdddc) in
017800051027      * ?-  (SELECT digits(fttpdr)!!digits(fttndc)!!digits(fttddc)
017900051027      * ?-  FROM edpab/fttrit09S) ) as afile
018000051027      * ?-   where
018100051027      * ?-  digits(ftdaas)!!digits(ftdlnp)!!digits(ftdnrs)!!digits(ftdnsp) in
018200051027      * ?-  (select
018300051027      * ?-  digits(tasaas)!!digits(taslnp)!!digits(tasnrs)!!digits(tasnsp)
018400051027      * ?-  from edpab/tasftd09)
018500051027      ***                                                nr:  81.045
018600051027      ***   ma qui ci sono anche i padr. pagati a giornata o senza tar.ETI/ETC
018700051027      ***
018800051027      * ?------------------------------------------------------------------ */
018900051027      * ?- TARIFFE ETI e ETC in vigore:
019000051027      *     da FIFGT prende le tariffe ETI o ETC legate ai dettagli FIFTD
019100051027      *      che dovevano essere Pagati
019200051027      * ?- creato EDPAB/FGTSEL09S
019300051027      * ?-
019400051027      * ?-  SELECT * FROM filtra201/fifgt00f WHERE fgttsr= 'R' and
019500051027      * ?-  fgtsml=' ' and FGTDST >= 20050901 and fgtddt <= 20050930 and fgtpdr
019600051027      * ?-  in ( SELECT fptpdr FROM filtra201/fifpt00f WHERE fptsml= ' ' and
019700051027      * ?-  fptctd in ('ETI', 'ETC') and fptpdr in ( SELECT ftdpdr FROM
019800051027      * ?-  EDPAB/ftdsel09s))   order by fgtpdr
019900051027      * ?-   si deve vedere quante bolle sono state pagate a giornata.
020000051027      ***          in tutto il nr.delle tariffe sono             nr:  184
020100051027      * ?------------------------------------------------------------------ */
020200051027      * ?-
020300051027      * ?-
020400051027      * ?-
020500051027      * ?------------------------------------------------------------------ */
020600051027      ***  Tutte le spedizaioni di TITAS che non sono state valorizzate su FTD
020700051027      ***   x padroncini fittizi
020800051027      * ?- creato EDPAB/TASVAL09N
020900051027      * ?-
021000051027      * ?-  SELECT * FROM edpab/etierr09 WHERE
021100051027      * ?-  digits(tasaas)!!digits(tasLNP)!!digits(tasNRS)!!digits(tasnsp) not
021200051027      * ?-  in (SELECT
021300051027      * ?-  digits(ftdaas)!!digits(ftdLNP)!!digits(ftdNRS)!!digits(ftdnsp) FROM
021400051027      * ?-  edpab/ftdrit09 ) order BY TASPDR
021500051027      * ?-
021600051027      * ?-
021700051027      * ?------------------------------------------------------------------ */
021800051027      ***  Tutte le spedizaioni di TITAS che sono state valorizzate su FTD
021900051027      * ?- creato EDPAB/TASVAL09S
022000051027      * ?-
022100051027      * ?-  SELECT * FROM edpab/etierr09 WHERE
022200051027      * ?-  digits(tasaas)!!digits(tasLNP)!!digits(tasNRS)!!digits(tasnsp)
022300051027      * ?-  in (SELECT
022400051027      * ?-  digits(ftdaas)!!digits(ftdLNP)!!digits(ftdNRS)!!digits(ftdnsp) FROM
022500051027      * ?-  edpab/ftdrit09 ) order BY TASPDR
022600051027      * ?------------------------------------------------------------------ */
022700051027      * ?-
022800051027      * ?-
022900051027      * ?-
023000051027      * ?-
023100051027      * ?-
023200051027      * ?------------------------------------------------------------------ */
023300051027      * ?-  COSTI da FTD
023400051027      ***
023500051027      ***  Fra gli FIFTD selezionati solo quelli che hanno un valore >0 sulla BASE
023600051027      * ?-   SELECT count(*) FROM edpab/ftdsel09s
023700051027      * ?-   where ftditt>0
023800051027      ***                                                nr. righe: 34.188
023900051027      * ?-   SELECT sum(FTDITT) FROM edpab/ftdsel09s
024000051027      * ?-   where ftditt>0
024100051027      ***                                  Valore in Euro tot Base: 17.496,689
024200051027      ***    ma questo è il valore complessivo della base (no accessori)
024300051027      * ?------------------------------------------------------------------ */
024400051027      * ?-  COSTI da FCE
024500051027      ***
024600051027      ***  Valore di tutte le voci "997" delle spedizioni dell'FTD --> 81.045
024700051027      ***
024800051027      * ?-  SELECT sum(fceice) FROM edpab/fcerit09 WHERE FCECCE ='997' and
024900051027      * ?-  digits(fceaas)!!digits(fceLNP)!!digits(fceNRS )!!digits(fcensp) in
025000051027      * ?-  (SELECT
025100051027      * ?-  digits(ftdaas)!!digits(ftdLNP)!!digits(ftdNRS)!!digits(ftdnsp) FROM
025200051027      * ?-  edpab/ftdsel09s)
025300051027      ***                         il valore complessivo in Euro è di 1.514,073
025400051027      ***
025500051027      * ?------------------------------------------------------------------ */
025600051027      ***
025700051031      *     creata base dati delle bolle da valorizzare con ETI da FIFTD e da
025800051031      *      TITAS dove ci sono il nr.colli da calcolare in seguito con la
025900051031      *      tariffa ETI x simulare la valorizzazione delle etichettature.
026000051031      * ?- creato EDPAB/JOIN09S
026100051027      * ?-
026200051031      * ?- SELECT FTDSEL09S.FTDPDR, FTDSEL09S.FTDTSR, FTDSEL09S.FTDFGS,
026300051031      * ?- FTDSEL09S.FTDNDC, FTDSEL09S.FTDDDC, FTDSEL09S.FTDAAS,
026400051031      * ?- FTDSEL09S.FTDLNP, FTDSEL09S.FTDNRS, FTDSEL09S.FTDNSP,
026500051031      * ?- FTDSEL09S.FTDSET, FTDSEL09S.FTDNCE, FTDSEL09S.FTDNCL,
026600051031      * ?- FTDSEL09S.FTDPKL, FTDSEL09S.FTDVLU, FTDSEL09S.FTDSIC,
026700051031      * ?- FTDSEL09S.FTDFRM, FTDSEL09S.FTDFGC, FTDSEL09S.FTDTMC,
026800051031      * ?- FTDSEL09S.FTDCMC, FTDSEL09S.FTDCTP, FTDSEL09S.FTDITT,
026900051031      * ?- FTDSEL09S.FTDITA, FTDRIT09S.TASAAS, FTDRIT09S.TASLNP,
027000051031      * ?- FTDRIT09S.TASNRS, FTDRIT09S.TASNSP, FTDRIT09S.TASTBL,
027100051031      * ?- FTDRIT09S.TASCBO, FTDRIT09S.TASMGS, FTDRIT09S.TASNCL,
027200051031      * ?- FTDRIT09S.TASPDR, FTDRIT09S.TASDRT, FTDRIT09S.TASHRT,
027300051031      * ?- FTDRIT09S.TASNRT, FTDRIT09S.TASFPP, FTDRIT09S.TASCTM   FROM
027400051031      * ?- edpab/ftdsel09s join edpab/ftdrit09s on ftdaas = tasaas and ftdlnp
027500051031      * ?- = taslnp and ftdnrs = tasnrs and ftdnsp = tasnsp ORDER BY
027600051031      * ?- FTDSEL09S.FTDPDR, FTDSEL09S.FTDDDC, FTDSEL09S.FTDNDC
027700051031      * ?-
027800051031      * ?-
027900051031      * ?-
028000051031      * ?-
028100051027      * ?-
028200051027      ***
028300051027      ***
028400051027      ***
028500051027      ***
028600051027      ***
028700051027      ***
028800051027      ***
028900051027     C* - - - - - - - - - - - - - - - - - - - - - - - - - -
029000050510     c* Totali
029100050510     c                   exsr      totalizza
029200050510     C* - - - - - - - - - - - - - - - - - - - - - - - - - -
029300040823      *
029400041007     c     fine          tag
029500020326     C                   SETON                                        LR
029600050511     ?****************************************************************************
029700050511      *?   Stampa      File di Work Una riga x P.O.
029800050511     ?****************************************************************************
029900050511     c     PRT_rig_det   begsr
030000050511      *
030100050511     c                   if        *in66
030200050511      *                 * ------------------ *
030300050511     c                   write     testa
030400050511      *                 * ------------------ *
030500050511     c                   setoff                                       66
030600050511     c                   end
030700050511      *
030800050511      *
030900050511      *                 * ------------------ *
031000050511     c                   write     dettagli
031100050511      *                 * ------------------ *
031200050511      *
031300050510     c                   endsr
031400050511     ?****************************************************************************
031500050511      *?   Stampa      Avviso NO DATI x la SELEZIONE
031600050511     ?****************************************************************************
031700050511     c     PRT_NO_Dati   begsr
031800050511      *
031900050511     c                   if        *in66
032000050511      *                 * ------------------ *
032100050511     c                   write     testa
032200050511      *                 * ------------------ *
032300050511     c                   setoff                                       66
032400050511     c                   end
032500050511      *
032600050511      *                 * ------------------ *
032700050511     c                   write     nodati
032800050511      *                 * ------------------ *
032900050511      *
033000050511     c                   endsr
033100050511     ?****************************************************************************
033200050511      *?   INIZIALIZZAZIONE
033300050511     ?****************************************************************************
033400040820     C     *inzsr        BEGSR
033500050511      **
033600040820     C     *ENTRY        PLIST
033700040820     C                   PARM                    KPJBA
033800050510     c                   movel     kpjbu         DsPassaggio
033900050511      **
034000050511     C                   Z-ADD     1             CODUT
034100050511     C                   CALL      'X§PARUT'
034200050511     C                   PARM                    UT§DSE
034300050511     C                   MOVEL     RAGUT         RSUT             20
034400041103      *
034500040820      *  imposta UDATE di elaborazione
034600040820     C                   move      *date         G02DAT
034700040820     C                   MOVE      *ZEROS        G02INV
034800040820     C                   MOVE      *BLANKS       G02ERR
034900040820     C                   CALL      'XSRDA8'
035000040820     C                   PARM                    WLBDAT
035100040820     C                   Z-ADD     G02INV        data_oggi
035200050511     C                   move      G02dat        day
035300040820     C                   TIME                    W0140            14 0
035400050511     C                   MOVEl     W0140         ore
035500040823      **
035600050511     c                   seton                                        66
035700040825     c                   clear                   prima_volta       1
035800040826     c                   clear                   almeno_uno
035900050511     C                   MOVEL     DSPGM         NOMPGM
036000050510     c*
036100040820     C                   ENDSR
036200050511     ?****************************************************************************
