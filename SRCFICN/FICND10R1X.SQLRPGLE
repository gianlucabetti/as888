000100030117      *****************************************************************
000200110822      *    Calcola il Costo Medio Giornaliero di Periodo Lavorato
000300030117      *****************************************************************
000400120403     Fwficnd10CMO    E             disk
000500110822      *
000600110823     FFIFPT02L  IF   E           K DISK
000700110823     FFITGT01L  IF   E           K DISK
000800030117      *****************************************************************
000900030131      *
001000030129      *****************************************************************
001100030117     d KPJBA         E DS
001200091112     D ficnd7ds      e DS
001300091112     D ddatiute      e ds
001400091112     D azuteds       e ds                  extname(AZUTE00F)
001500091112     D tibs34ds      E DS                  inz
001600110822      * -------------------------------
001700110822      *   Campi usati per lettura SQL
001800110822      * -------------------------------
002000120403     d WFICND10F     E DS
002100110822      *
002200120403     d  AUTISTA_sql    s                   like(CM10PDR)
002300120403     d  NOME____sql    s                   like(CM10RSC)
002301120404     d  SERVIZIOsql    s              1
002302120404     d  GIORNI__sql    s              3  0
002400120403     d  PORTATA_sql    s                   like(CM10POR)
002401120403     d  VOLUME__sql    s                   like(CM10VOL)
002402120404     d  MASSA___sql    s                   like(CM10MCP)
002403120403     d  KM______sql    s                   like(CM10KM)
002501120403      *
002700120403     d  COSTI___sql    s                   like(CM10COSTI)
002800120403     d  COMPE___sql    s                   like(CM10COMP)
003101120404     d  PESOMOV_sql    s                   like(CM10KFA)
003102120404     d  VOLMOV__sql    s                   like(CM10TVL)
003103120403      *
003104120403     d  NSP_____sql    s                   like(CM10NSP)
003105120403     d  SNA_____sql    s                   like(CM10SNA)
003106120403     d  CLA_____sql    s                   like(CM10CLA)
003107120403     d  BLAF____sql    s                   like(CM10BLAF)
003108120403     d  BLNC____sql    s                   like(CM10BLNC)
003109120403     d  RMAF____sql    s                   like(CM10RMAF)
003110120403     d  RMFA____sql    s                   like(CM10RMFA)
003111120403      *
003200030117     D WLBDAT          DS
003300030117     D  G02DAT                 1      8  0
003400030117     D  G02INV                 9     16  0
003500030117     D  G02ERR                17     17
003600030117     D  G02TGI                18     22  0
003700030121      *-
003800030212     D  dataiso        S               d   inz
003900030212     D  dataeur        S               d   inz datfmt(*eur)
004000030117      *****************************************************************
004100030117     C     *ENTRY        PLIST
004200030117     C                   PARM                    KPJBA
004300091112     C                   movel     KPJBU         ficnd7ds
004400091113     c*
004500091112     c     *dtaara       define    §azute        azuteds
004600091112     c     *dtaara       define    §datiute      ddatiute
004700091112     C                   in(E)     *dtaara
004800091112     C                   IF        %Error  or  RSUT = *blanks
004900091112     C                   call      'TIBS34R'
005000091112     C                   parm                    Tibs34Ds
005100091112     C                   in        *dtaara
005200091112     c                   ENDIF
005300110822      *
005400120404     **  Il record da caricare sul WFICND10CM è calcolato direttamente dall'SQL poichè
005500120404     **   elaborare i Totali per Autista del precedente file WFICND10F non è altro che
005600120404     **   mettere in GROUP BY la chiave x AUTISTA +
005601120404     **    (tutte le decodifiche anagrafiche [uguali su tutte le righe]) e calcolare
005602120404      *         semplicemente le somme dei campi da Totalizzare.
005603120404      *  Inoltre,
005604120404      *   volendo anche conoscere x ogni Autista i Giorni Lavorati nel periodo,
005605120404      *   si mette in JOIN x Autista il risultato ottenuto con
005606120404      *    un'altra elaborazione del precedente file WFICND10F nel quale, con un
005607120404      *    Count(*) su GROUP BY di Data, si contano le giornate presenti x l'Autista.
005609120404      *
005700110822     **  -----------------------------------------------------------------------------------
005800110822     **  DESCRIZIONE dell'SQL ORDINATO x CODICE AUTISTA
005900110822     **  -----------------------------------------------------------------------------------
006000120404     **  Selezione (CAMPI ANAGRAFICI+ GIORNI LAVORATI+ le CAMPI TOTALIZZATI da scrivere sul File)
006100110822     **         ricavati da
006200110822     **
006300120404     **      (File AUTISTA + campi_anagrafici, somma COSTI/COMPETENZE/PESI/VOLUMI.... )
006400120404     **         le somme dei campi a valore sono ottenute elaborando il file in
006800120404     **           Group By x AUTISTA/DATA   (in una sottoelaborazione)
007100120404     **         ed in JOIN x Codice AUTISTA
007200120404     **         ( altra elaborazione File
007300120404     **          in Group By x AUTISTA x ricavare i Giorni Lavorati)
007600120404     **
007700110822     ** - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
007800110822     C/EXEC SQL
007900110822     C+ Declare A1 Cursor for
007901120404      *****
008002120404      ***     Campi da scrivere sul file di Lavoro già Totalizzati
008003120404      ***  (L'Autista e le sue decodifiche anagrafiche sono tutte uguali sul
008004120404      ***   file precedentemente elaborato quindi le posso usare in Group By)
008005120404      *****
008006120404     c+ SELECT Autista, RagSoc, Servizio,
008007120404     c+        GIORNI_LAVORATI, Peso, Volume, massa, KiloMT,
008101120404     c+           tot_costi,
008102120404     c+           tot_competenze,
008103120404     c+           tot_peso_Movimentato,
008104120404     c+           tot_vol_Movimentato,
008105120404     c+           tot_Sped_Fatte,
008106120404     c+           tot_Stop_Pagati,
008107120404     c+           tot_Colli,
008108120404     c+           tot_BolleAffidate,
008109120404     c+           tot_BolleNONCons,
008110120404     c+           tot_ORMAffidati,
008111120404     c+           tot_ORMFatti
008112120404      *****
008113120404      ***   PRIMO FILE: contiene le Totalizzazioni del Periodo richiesto
008114120404      ***      x Autista
008115120404      *****
008116120404      *** >>>>>------->>>>>
008300120404     c+ FROM  ( SELECT Autista, RagSoc, Servizio,
008401120404     c+                Peso, Volume, massa, KiloMT,
008501120404     c+          sum(costi)           AS tot_costi,
008502120404     c+          sum(competenze)      AS tot_competenze,
008503120404     c+          sum(PesoMovimentato) AS tot_peso_Movimentato,
008504120404     c+          sum(VolMovimentato)  AS tot_vol_Movimentato,
008505120404     c+          sum(NRSpedFatte)     AS tot_Sped_Fatte,
008506120404     c+          sum(StopPagati)      AS tot_Stop_Pagati,
008507120404     c+          sum(ColliNOIsole)    AS tot_Colli,
008508120404     c+          sum(BolleAffidate)   AS tot_BolleAffidate,
008509120404     c+          sum(BolleNONCons)    AS tot_BolleNONCons,
008510120404     c+          sum(ORMAffidati)     AS tot_ORMAffidati,
008511120404     c+          sum(ORMFatti)        AS tot_ORMFatti
008513120404      *****
008514120404      ***      Elaborazione perGIORNATA in GROUP BY Autista/Data
008515120404      *****      totalizza x Autista tutto quanto c'è nella Giornata
008516120404      *** >>>>>
008900120404     c+         FROM ( SELECT
009100120404     c+                 fn10pdr        AS Autista,
009101120404     c+                 fn10rsc        AS RagSoc,
009102120404     c+                 fn10tsr        AS Servizio,
009103120404     c+                 fn10pkl        AS Peso,
009104120404     c+                 fn10vlu        AS Volume,
009105120404     c+                 fn10mcp        AS massa,
009106120404     c+                 fn10km         AS KiloMT,
009107120404     c+                 FN10DDC        AS data,
009200120404     c+                 sum(fn10costi) AS costi,
009201120404     c+                 sum(fn10comp)  AS competenze,
009202120404     c+                 sum(fn10KFA)   AS PesoMovimentato,
009203120404     c+                 sum(fn10TVL)   AS VolMovimentato,
009204120404     c+                 sum(fn10NSP)   AS NRSpedFatte,
009205120404     c+                 sum(fn10SNA)   AS StopPagati,
009206120404     c+                 sum(fn10CLA)   AS ColliNoIsole,
009207120404     c+                 sum(fn10BLAF)  AS BolleAffidate,
009208120404     c+                 sum(fn10BLNC)  AS BolleNONCons,
009209120404     c+                 sum(fn10RMAF)  AS OrmAffidati,
009210120404     c+                 sum(fn10RMFA)  AS OrmFATTI
009400120404     c+                FROM  wficnd10f
009401120404      ***
009500120404     c+                GROUP BY FN10PDR, fn10rsc, FN10TSR, fn10pkl,
009600120404     c+                  fn10vlu, fn10mcp, fn10km, FN10DDC
009601120404      ***
009602120404      ***    Con questo GROUP BY fondamentalmente x Autista e Data
009603120404      ***    mi totalizza per giornata sia Consegne che Ritiri dell'autista
009604120404      ***
009605120404     c+               ) AS perDATA
009606120404      *** >>>>>
009607120404      *****
009610120404     c+         GROUP BY
009611120404     c+         Autista, RagSoc, Servizio, Peso, Volume, massa, KiloMT
009612120404      ***
009613120404      ***    Con questo GROUP BY fondamentalmente x Autista
009614120404      ***     totalizza tutto il Periodo dell'Autista
009615120404      ***
009616120404     c+       ) AS perPERIODO
009617120404      *** >>>>>------->>>>>
009618120404      *****
009619120404      ***       per avere anche i Giorni lavorati
009620120404      *****
009700120404     c+     JOIN
009701120404      *****
009702120404      ***    SECONDO FILE:
009703120404      ***       Mette in Join x Autista il conto dei giorni presenti
009704120404      ***       nel Periodo x autista.
009705120404      *****
009800120404     c+       ( SELECT   fn10pdr, fn10tsr, count(*) as GIORNI_LAVORATI
010000120404     c+           FROM  ( SELECT  fn10pdr, fn10tsr, fn10ddc
010200120404     c+                    FROM   wficnd10f
010300120404     c+                    GROUP BY fn10pdr, fn10tsr, fn10ddc) AS gg
010500120404     c+            GROUP BY fn10pdr, fn10tsr
010600120404     c+        ) AS giornilav
010601120404      *****
010602120404      ***   Chiave di Join l'AUTISTA
010603120404      *****
010700120404     c+     ON fn10pdr = autista and fn10tsr = servizio
010701120404      *****
010702120404      ***    Quindi ordinato per Autista ha il PRIMO file con i totali del Periodo
010703120404      ***    con i Giorni Lavorati nel Pariodo dal SECONDO file.
010704120404      *****
011500120404     c+ ORDER BY autista, servizio
011501120404      *****
011502120404      *****
011600110822     C/END-EXEC
011700110822     ** -----------------------------------------------
011800110822      *          apertura cursore
011900110822     C/EXEC SQL
012000110822     C+ OPEN A1
012100110822     C/END-EXEC
012200110822     ** -----------------------------------------------
012300110822     C                   DOU       SqlCod <  0
012400110822      *          lettura cursore
012500110822     C/EXEC SQL
012600120403     C+ Fetch Next From A1 Into :AUTISTA_sql,
012700120403     C+                         :NOME____sql,
012702120404     C+                         :SERVIZIOsql,
012703120404     C+                         :GIORNI__sql,
012704120404     C+                         :PORTATA_sql,
012705120404     C+                         :VOLUME__sql,
012800120403     C+                         :MASSA___sql,
013100120403     C+                         :KM______sql,
013300120403     C+                         :COSTI___sql,
013400120404     C+                         :COMPE___sql,
013401120404     C+                         :PESOMOV_sql,
013402120404     C+                         :VOLMOV__sql,
013403120404     C+                         :NSP_____sql,
013404120404     C+                         :SNA_____sql,
013405120404     C+                         :CLA_____sql,
013406120404     C+                         :BLAF____sql,
013407120404     C+                         :BLNC____sql,
013408120404     C+                         :RMAF____sql,
013409120404     C+                         :RMFA____sql
013500110822     C/END-EXEC
013600110822      *
013700110822     C                   SELECT
013800110822     c*
013900110822     c* a fine file
014000110822     C                   WHEN      SqlCod = 100
014100110822     c                   leave
014200110822     **
014300110822     C                   WHEN      SqlCod < 0
014400110822     C                   seton                                        H1
014500110822     c                   leave
014600110822     c*
014700110822     C                   OTHER
014800110822     c*
014900110822     c                   exsr      scrive_riga
015000110822     c*
015100110822     C                   ENDSL
015200110822      *
015300110822     C                   ENDDO
015400110822     ** -----------------------------------------------
015500110822     C/EXEC SQL
015600110822     C+ Close A1
015700110822     C/END-EXEC
015800110822     **
015900030415     C                   SETON                                        LR
016000030825      *--------------------------------------------------------------*
016100110822      *  scrivo record
016200030825      *--------------------------------------------------------------*
016300110822     c     scrive_riga   BEGSR
016400110823      *
016500120404     c                   clear                   wficnd10M
016700120403     c                   eval      CM10da     =  PARDD1
016800120403     c                   eval      CM10a      =  PARDD2
016900120403     c                   eval      CM10PDR    =  AUTISTA_sql
017000120403     c                   eval      CM10RSC    =  NOME____sql
017001120404     c                   eval      CM10GLA    =  GIORNI__sql
017200120403     c                   eval      CM10POR    =  PORTATA_sql
017201120403     c                   eval      CM10VOL    =  VOLUME__sql
017202120404     c                   eval      CM10MCP    =  MASSA___sql
017203120403     c                   eval      CM10KM     =  KM______sql
017400120403     c                   eval      CM10COSTI  =  COSTI___sql
017600120403     c                   eval      CM10COMP   =  COMPE___sql
017601120404     c                   eval      CM10KFA    =  PESOMOV_sql
017602120404     c                   eval      CM10TVL    =  VOLMOV__sql
017604120403     c                   eval      CM10NSP    =  NSP_____sql
017605120403     c                   eval      CM10SNA    =  SNA_____sql
017606120403     c                   eval      CM10CLA    =  CLA_____sql
017607120403     c                   eval      CM10BLAF   =  BLAF____sql
017608120403     c                   eval      CM10BLNC   =  BLNC____sql
017609120403     c                   eval      CM10RMAF   =  RMAF____sql
017610120403     c                   eval      CM10RMFA   =  RMFA____sql
017611120403      *
017800110823      *  Dati basati sulle Tariffe
017900110822     c                   exsr      ctrl_tariffa
018000110823      *
018100120403     c                   eval      CM10TARUGU = stessa_tariffa
018200120403     c                   eval      CM10NOTAR  = No_Tariffa
018300110822      *
018400120404     c                   write     wficnD10M
018500110822      *
018600091113     c                   ENDSR
018700091113      *--------------------------------------------------------------*
018800110822      *  dati sulle tariffe
018900110822      *--------------------------------------------------------------*
019000110822     c     ctrl_tariffa  BEGSR
019100110822      *
019200110823     c     xAUT          klist
019300110823     c                   kFLD                    TGTPDR
019400110823     c                   kFLD                    TGTSML
019500110823      *
019600110823     c     xAUT_fPT      klist
019700110823     c                   kFLD                    fPTPDR
019800110823     c                   kFLD                    fPTSML
019900110823      *
020000110823     c                   eval        TGTPDR = AUTISTA_sql
020100110823     c                   eval        TGTSML = ' '
020200110823      *
020300110823     c                   move      'N'           stessa_tariffa    1
020400110823      *
020500110823     c     xAUT          setll     fiTGT01l
020600110823     c     xAUT          reade     fiTGT01l
020700110823     c                   dow       not %EoF(fiTGT01l)
020800110823     c                   if        tgtDDT <= PARDD1 and
020900110823     c                             tgtDST >= PARDD2
021000110823      * Se utilizzata un'unica tariffa all'interno del periodo selezionato
021100110830     c                   move      ' '           stessa_tariffa
021200110823     c                   leave
021300110823     c                   end
021400110823     c     xAUT          reade     fiTGT01l
021500110823     c                   end
021600110823      *
021700110823      *   Se i calcoli sono basati sulla stessa tariffa,
021800110823      *  Controllo se ci sono tariffe a Giornata
021900110823     c                   clear                   GIORN_Intera      1
022000110823     c                   clear                   GIORN_Mezza       1
022100110823     c                   clear                   GIORN_MinGaran    1
022200110831     c                   clear                   GIORN_Bonus       1
022300110831     c                   clear                   GIORN_Etichett    1
022400110831     c                   clear                   GIORN_Picking     1
022500110831     c                   move      'N'           No_Tariffa        1
022600110823      *
022700110831     c                   if          stessa_tariffa = 'N'
022800110831     c                   move      ' '           No_Tariffa
022900110823      *
023000110831     c                   elseIf      stessa_tariffa = ' '
023100110831      *
023200110823     c                   eval           fPTPDR  = TGTpdr
023300110823     c                   eval           fPTSML  = ' '
023400110830      *
023500110823      *  Nell'ambito dello stesso progressivo
023600110823      *   Quali tariffe ci sono a giornata ?
023700110823     c     xAUT_fPT      setll     fifPT02l
023800110823     c     xAUT_fPT      reade     fifPT02l
023900110830      *
024000110830     c                   dow       not %EoF(fifPT02l)
024100110831      *
024200110831      *  solo per il progressivo
024300110831     c                   if             fPTPRG  = TGTprg
024400110831      * Tariffa non trovata in assoluto perchè eliminata
024500110831     c                   move      ' '           No_Tariffa
024600110831      *
024700110831      *   solo per tariffe a giornata
024800110831     c                   if             fPTTSR  = 'G' and fPTCTR  = 999
024900110831      *
025000110823      *      Nell'ambito dello stesso Progressivo di periodo
025100110823     c                   if          fptCTD = 'TIG'
025200110823     c                   eval          GIORN_Intera   = 'S'
025300120403     c                   z-add     fptATA        CM10TIG
025400110831      *
025500110823     c                   elseIf      fptCTD = 'TMG'
025600110823     c                   eval          GIORN_Mezza    = 'S'
025700120403     c                   z-add     fptATA        CM10TMG
025800110831      *
025900110823     c                   elseIf      fptCTD = 'MGA'
026000110823     c                   eval          GIORN_MinGaran = 'S'
026100120403     c                   z-add     fptATA        CM10MGA
026200110831      *
026300110831      *
026400110831     c                   elseIf      fptCTD = 'BON'
026500110831     c                   eval          GIORN_Bonus    = 'S'
026600120403     c                   z-add     fptATA        CM10BON
026700110831      *
026800110831     c                   elseIf      fptCTD = 'ETI'
026900110831     c                   eval          GIORN_Etichett = 'S'
027000120403     c                   z-add     fptATA        CM10ETI
027100110831      *
027200110831     c                   elseIf      fptCTD = 'PCK'
027300110831     c                   eval          GIORN_Picking  = 'S'
027400120403     c                   z-add     fptATA        CM10PCK
027500110831      *
027600110823     c                   end
027700110831      *
027800110831     c                   end
027900110831     c                   end
028000110831      *
028100110823     c     xAUT_fPT      reade     fifPT02l
028200110823     c                   end
028300110823      *
028400110823     c                   end
028500110823      *
028600110822      *
028700110822     c                   ENDSR
028800110822      *--------------------------------------------------------------*
