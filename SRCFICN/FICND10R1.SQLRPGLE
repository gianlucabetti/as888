000100030117      *****************************************************************
000200110822      *    Calcola il Costo Medio Giornaliero di Periodo Lavorato
000300030117      *****************************************************************
000400120404     Fwficnd10CMO    E             disk    infds(CM10ds)
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
001600120404     D Wsav10CM      e ds                  extname(Wficnd10CM)
001700120404     d                                     Prefix(sv_)
001800120404      *
001900120404     d CM10ds          ds
002000120404     d  CM10_nrr             397    400b 0
002100120404      *
002200110822      * -------------------------------
002300110822      *   Campi usati per lettura SQL
002400110822      * -------------------------------
002500120403     d WFICND10F     E DS
002600110822      *
002700120403     d  AUTISTA_sql    s                   like(CM10PDR)
002800120403     d  NOME____sql    s                   like(CM10RSC)
002900120404      *
003000120404     d  SERVIZIOsql    s              1
003100120404     d  GIORNI__sql    s              3  0
003200120404      *
003300120403     d  PORTATA_sql    s                   like(CM10POR)
003400120403     d  VOLUME__sql    s                   like(CM10VOL)
003500120404     d  MASSA___sql    s                   like(CM10MCP)
003600120403     d  KM______sql    s                   like(CM10KM)
003700120523     d  PDA_____sql    s                   like(CM10PDA)
003800120403      *
003900120404     d  COSTI___sql    s                   like(CM10COS_C)
004000120404     d  COMPE___sql    s                   like(CM10COM_C)
004100120404     d  PESOMOV_sql    s                   like(CM10KFA_C)
004200120404     d  VOLMOV__sql    s                   like(CM10TVL_C)
004300120403      *
004400120404     d  NSP_____sql    s                   like(CM10NSP_C)
004500120404     d  SNA_____sql    s                   like(CM10SNA_C)
004600120404     d  CLA_____sql    s                   like(CM10CLA_C)
004700120403     d  BLAF____sql    s                   like(CM10BLAF)
004800120403     d  BLNC____sql    s                   like(CM10BLNC)
004900120403     d  RMAF____sql    s                   like(CM10RMAF)
005000120403     d  RMFA____sql    s                   like(CM10RMFA)
005100120404      *
005200120404     d  salva_CM10PDR  s                   like(sv_CM10PDR)
005300120403      *
005400030117     D WLBDAT          DS
005500030117     D  G02DAT                 1      8  0
005600030117     D  G02INV                 9     16  0
005700030117     D  G02ERR                17     17
005800030117     D  G02TGI                18     22  0
005900030121      *-
006000030212     D  dataiso        S               d   inz
006100030212     D  dataeur        S               d   inz datfmt(*eur)
006200030117      *****************************************************************
006300030117     C     *ENTRY        PLIST
006400030117     C                   PARM                    KPJBA
006500091112     C                   movel     KPJBU         ficnd7ds
006600091113     c*
006700091112     c     *dtaara       define    §azute        azuteds
006800091112     c     *dtaara       define    §datiute      ddatiute
006900091112     C                   in(E)     *dtaara
007000091112     C                   IF        %Error  or  RSUT = *blanks
007100091112     C                   call      'TIBS34R'
007200091112     C                   parm                    Tibs34Ds
007300091112     C                   in        *dtaara
007400091112     c                   ENDIF
007500110822      *
007600120404      *  Prima pulizia fuori ciclo Record di Work per parcheggiare i dati
007700120404      *   in parte Consegne e in parte Ritiri
007800120404     c                   clear                   Wsav10CM
007900120404      *
008000120404     **  Il record da caricare sul WFICND10CM è calcolato direttamente dall'SQL poichè
008100120404     **   elaborare i Totali per Autista del precedente file WFICND10F non è altro che
008200120404     **   mettere in GROUP BY la chiave x AUTISTA +
008300120404     **    (tutte le decodifiche anagrafiche [uguali su tutte le righe]) e calcolare
008400120404      *         semplicemente le somme dei campi da Totalizzare.
008500120404      *  Inoltre,
008600120404      *   volendo anche conoscere x ogni Autista i Giorni Lavorati nel periodo,
008700120404      *   si mette in JOIN x Autista il risultato ottenuto con
008800120404      *    un'altra elaborazione del precedente file WFICND10F nel quale, con un
008900120404      *    Count(*) su GROUP BY di Data, si contano le giornate presenti x l'Autista.
009000120404      *
009100110822     **  -----------------------------------------------------------------------------------
009200110822     **  DESCRIZIONE dell'SQL ORDINATO x CODICE AUTISTA
009300110822     **  -----------------------------------------------------------------------------------
009400120404     **  Selezione (CAMPI ANAGRAFICI+ GIORNI LAVORATI+ le CAMPI TOTALIZZATI da scrivere sul File)
009500110822     **         ricavati da
009600110822     **
009700120404     **      (File AUTISTA + campi_anagrafici, somma COSTI/COMPETENZE/PESI/VOLUMI.... )
009800120404     **         le somme dei campi a valore sono ottenute elaborando il file in
009900120404     **           Group By x AUTISTA/DATA   (in una sottoelaborazione)
010000120404     **         ed in JOIN x Codice AUTISTA
010100120404     **         ( altra elaborazione File
010200120404     **          in Group By x AUTISTA x ricavare i Giorni Lavorati)
010300120404     **
010400110822     ** - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
010500110822     C/EXEC SQL
010600110822     C+ Declare A1 Cursor for
010700120404      *****
010800120404      ***     Campi da scrivere sul file di Lavoro già Totalizzati
010900120404      ***  (L'Autista e le sue decodifiche anagrafiche sono tutte uguali sul
011000120404      ***   file precedentemente elaborato quindi le posso usare in Group By)
011100120404      *****
011200120404     c+ SELECT Autista, RagSoc, Servizio,
011300120523     c+        GIORNI_LAVORATI, Peso, Volume, massa, KiloMT, PDAaut,
011400120404     c+           tot_costi,
011500120404     c+           tot_competenze,
011600120404     c+           tot_peso_Movimentato,
011700120404     c+           tot_vol_Movimentato,
011800120404     c+           tot_Sped_Fatte,
011900120404     c+           tot_Stop_Pagati,
012000120404     c+           tot_Colli,
012100120404     c+           tot_BolleAffidate,
012200120404     c+           tot_BolleNONCons,
012300120404     c+           tot_ORMAffidati,
012400120404     c+           tot_ORMFatti
012500120404      *****
012600120404      ***   PRIMO FILE: contiene le Totalizzazioni del Periodo richiesto
012700120404      ***      x Autista
012800120404      *****
012900120404      *** >>>>>------->>>>>
013000120404     c+ FROM  ( SELECT Autista, RagSoc, Servizio,
013100120523     c+                Peso, Volume, massa, KiloMT, PDAaut,
013200120404     c+          sum(costi)           AS tot_costi,
013300120404     c+          sum(competenze)      AS tot_competenze,
013400120404     c+          sum(PesoMovimentato) AS tot_peso_Movimentato,
013500120404     c+          sum(VolMovimentato)  AS tot_vol_Movimentato,
013600120404     c+          sum(NRSpedFatte)     AS tot_Sped_Fatte,
013700120404     c+          sum(StopPagati)      AS tot_Stop_Pagati,
013800120404     c+          sum(ColliNOIsole)    AS tot_Colli,
013900120404     c+          sum(BolleAffidate)   AS tot_BolleAffidate,
014000120404     c+          sum(BolleNONCons)    AS tot_BolleNONCons,
014100120404     c+          sum(ORMAffidati)     AS tot_ORMAffidati,
014200120404     c+          sum(ORMFatti)        AS tot_ORMFatti
014300120404      *****
014400120404      ***      Elaborazione perGIORNATA in GROUP BY Autista/Data
014500120404      *****      totalizza x Autista tutto quanto c'è nella Giornata
014600120404      *** >>>>>
014700120404     c+         FROM ( SELECT
014800120404     c+                 fn10pdr        AS Autista,
014900120404     c+                 fn10rsc        AS RagSoc,
015000120404     c+                 fn10tsr        AS Servizio,
015100120404     c+                 fn10pkl        AS Peso,
015200120404     c+                 fn10vlu        AS Volume,
015300120404     c+                 fn10mcp        AS massa,
015400120404     c+                 fn10km         AS KiloMT,
015500120523     c+                 fn10PDA        AS PDAaut,
015600120404     c+                 FN10DDC        AS data,
015700120404     c+                 sum(fn10costi) AS costi,
015800120404     c+                 sum(fn10comp)  AS competenze,
015900120404     c+                 sum(fn10KFA)   AS PesoMovimentato,
016000120404     c+                 sum(fn10TVL)   AS VolMovimentato,
016100120404     c+                 sum(fn10NSP)   AS NRSpedFatte,
016200120404     c+                 sum(fn10SNA)   AS StopPagati,
016300120404     c+                 sum(fn10CLA)   AS ColliNoIsole,
016400120404     c+                 sum(fn10BLAF)  AS BolleAffidate,
016500120404     c+                 sum(fn10BLNC)  AS BolleNONCons,
016600120404     c+                 sum(fn10RMAF)  AS OrmAffidati,
016700120404     c+                 sum(fn10RMFA)  AS OrmFATTI
016800120404     c+                FROM  wficnd10f
016900120404      ***
017000120404     c+                GROUP BY FN10PDR, fn10rsc, FN10TSR, fn10pkl,
017100120523     c+                  fn10vlu, fn10mcp, fn10km, fn10pda, FN10DDC
017200120404      ***
017300120404      ***    Con questo GROUP BY fondamentalmente x Autista e Data
017400120404      ***    mi totalizza per giornata sia Consegne che Ritiri dell'autista
017500120404      ***
017600120404     c+               ) AS perDATA
017700120404      *** >>>>>
017800120404      *****
017900120404     c+         GROUP BY
018000120523     c+         Autista, RagSoc, Servizio, Peso,
018100120523     c+           Volume, massa, KiloMT, PDAaut
018200120404      ***
018300120404      ***    Con questo GROUP BY fondamentalmente x Autista
018400120404      ***     totalizza tutto il Periodo dell'Autista
018500120404      ***
018600120404     c+       ) AS perPERIODO
018700120404      *** >>>>>------->>>>>
018800120404      *****
018900120404      ***       per avere anche i Giorni lavorati
019000120404      *****
019100120404     c+     JOIN
019200120404      *****
019300120404      ***    SECONDO FILE:
019400120404      ***       Mette in Join x Autista il conto dei giorni presenti
019500120404      ***       nel Periodo x autista.
019600120404      *****
019700120404     c+       ( SELECT   fn10pdr, fn10tsr, count(*) as GIORNI_LAVORATI
019800120404     c+           FROM  ( SELECT  fn10pdr, fn10tsr, fn10ddc
019900120404     c+                    FROM   wficnd10f
020000120404     c+                    GROUP BY fn10pdr, fn10tsr, fn10ddc) AS gg
020100120404     c+            GROUP BY fn10pdr, fn10tsr
020200120404     c+        ) AS giornilav
020300120404      *****
020400120404      ***   Chiave di Join l'AUTISTA
020500120404      *****
020600120404     c+     ON fn10pdr = autista and fn10tsr = servizio
020700120404      *****
020800120404      ***    Quindi ordinato per Autista ha il PRIMO file con i totali del Periodo
020900120404      ***    con i Giorni Lavorati nel Pariodo dal SECONDO file.
021000120404      *****
021100120404     c+ ORDER BY autista, servizio
021200120404      *****
021300120404      *****
021400110822     C/END-EXEC
021500110822     ** -----------------------------------------------
021600110822      *          apertura cursore
021700110822     C/EXEC SQL
021800110822     C+ OPEN A1
021900110822     C/END-EXEC
022000110822     ** -----------------------------------------------
022100110822     C                   DOU       SqlCod <  0
022200110822      *          lettura cursore
022300110822     C/EXEC SQL
022400120403     C+ Fetch Next From A1 Into :AUTISTA_sql,
022500120403     C+                         :NOME____sql,
022600120404     C+                         :SERVIZIOsql,
022700120404     C+                         :GIORNI__sql,
022800120404     C+                         :PORTATA_sql,
022900120404     C+                         :VOLUME__sql,
023000120403     C+                         :MASSA___sql,
023100120403     C+                         :KM______sql,
023200120523     C+                         :PDA_____sql,
023300120403     C+                         :COSTI___sql,
023400120404     C+                         :COMPE___sql,
023500120404     C+                         :PESOMOV_sql,
023600120404     C+                         :VOLMOV__sql,
023700120404     C+                         :NSP_____sql,
023800120404     C+                         :SNA_____sql,
023900120404     C+                         :CLA_____sql,
024000120404     C+                         :BLAF____sql,
024100120404     C+                         :BLNC____sql,
024200120404     C+                         :RMAF____sql,
024300120404     C+                         :RMFA____sql
024400110822     C/END-EXEC
024500110822     C                   SELECT
024600110822     c* a fine file
024700110822     C                   WHEN      SqlCod = 100
024800120404      *   deve scrivere l'ultimo che aveva precedentemente letto prima del EoF
024900120404     c                   if        sv_CM10PDR   >  0
025000120404      *                 *-----------------------*
025100120404     c                   exsr      SCRIVE_Record
025200120404      *                 *-----------------------*
025300120404     c                   end
025400120404      *
025500110822     c                   leave
025600110822     **
025700110822     C                   WHEN      SqlCod < 0
025800110822     C                   seton                                        H1
025900110822     c                   leave
026000110822     c*
026100110822     C                   OTHER
026200110822     c*
026300120404     c                   exsr      Imposta_Riga
026400110822     c*
026500110822     C                   ENDSL
026600110822      *
026700110822     C                   ENDDO
026800110822     ** -----------------------------------------------
026900110822     C/EXEC SQL
027000110822     C+ Close A1
027100110822     C/END-EXEC
027200110822     **
027300030415     C                   SETON                                        LR
027400030825      *--------------------------------------------------------------*
027500110822      *  scrivo record
027600030825      *--------------------------------------------------------------*
027700120404     c     Imposta_Riga  BEGSR
027800110823      *
027900120404      *  Scrive riga a Rottura di codice AUTISTA
028000120404     c                   if        sv_CM10PDR  <>  AUTISTA_sql  and
028100120404     c                             sv_CM10PDR   >  0
028200120404      *                 *-----------------------*
028300120404     c                   exsr      SCRIVE_Record
028400120404      *                 *-----------------------*
028500120404      * Pulisce i campi del wrk ma salva e reimposta l'Autista
028600120404     c                   eval      salva_CM10PDR = sv_CM10PDR
028700120404     c                   clear                   Wsav10CM
028800120404     c                   eval      sv_CM10PDR = salva_CM10PDR
028900120404      *
029000120404     c                   end
029100120404      *
029200120404      *  imposta i campi di salvataggio
029300120404      *   da scaricare in seguito a rottura di Autista
029400120404     c                   eval      sv_CM10da     =  PARDD1
029500120404     c                   eval      sv_CM10a      =  PARDD2
029600120404      *
029700120404     c                   eval      sv_CM10PDR    =  AUTISTA_sql
029800120404      *
029900120404     c                   eval      sv_CM10RSC    =  NOME____sql
030000120404     c                   eval      sv_CM10GLA    =  GIORNI__sql
030100120404     c                   eval      sv_CM10POR    =  PORTATA_sql
030200120404     c                   eval      sv_CM10VOL    =  VOLUME__sql
030300120404     c                   eval      sv_CM10MCP    =  MASSA___sql
030400120404     c                   eval      sv_CM10KM     =  KM______sql
030500120523     c                   eval      sv_CM10PDA    =  PDA_____sql
030600120404      *  Consegne
030700120404     c                   if          SERVIZIOsql ='C'
030800120404     c                   eval      sv_CM10COS_C  =  COSTI___sql
030900120404     c                   eval      sv_CM10COM_C  =  COMPE___sql
031000120404     c                   eval      sv_CM10KFA_C  =  PESOMOV_sql
031100120404     c                   eval      sv_CM10TVL_C  =  VOLMOV__sql
031200120404     c                   eval      sv_CM10NSP_C  =  NSP_____sql
031300120404     c                   eval      sv_CM10SNA_C  =  SNA_____sql
031400120404     c                   eval      sv_CM10CLA_C  =  CLA_____sql
031500120404     c                   eval      sv_CM10BLAF   =  BLAF____sql
031600120404     c                   eval      sv_CM10BLNC   =  BLNC____sql
031700120404      *
031800120404     c                   elseIF      SERVIZIOsql ='R'
031900120404     c                   eval      sv_CM10COS_R  =  COSTI___sql
032000120404     c                   eval      sv_CM10COM_R  =  COMPE___sql
032100120404     c                   eval      sv_CM10KFA_R  =  PESOMOV_sql
032200120404     c                   eval      sv_CM10TVL_R  =  VOLMOV__sql
032300120404     c                   eval      sv_CM10NSP_R  =  NSP_____sql
032400120404     c                   eval      sv_CM10SNA_R  =  SNA_____sql
032500120404     c                   eval      sv_CM10CLA_R  =  CLA_____sql
032600120404     c                   eval      sv_CM10RMAF   =  RMAF____sql
032700120404     c                   eval      sv_CM10RMFA   =  RMFA____sql
032800120404      *
032900120404     c                   endIF
033000110822      *
033100091113     c                   ENDSR
033200091113      *--------------------------------------------------------------*
033300120404      *  A rottura di Codice AUTISTA
033400110822      *--------------------------------------------------------------*
033500120404     c     SCRIVE_Record BEGSR
033600110822      *
033700120404     c                   clear                   wficnd10M
033800120404     c                   eval         CM10da     =  sv_CM10da
033900120404     c                   eval         CM10a      =  sv_CM10a
034000120404      *
034100120404     c                   eval         CM10PDR    =  sv_CM10PDR
034200120404      *
034300120404     c                   eval         CM10RSC    =  sv_CM10RSC
034400120404     c                   eval         CM10GLA    =  sv_CM10GLA
034500120404     c                   eval         CM10POR    =  sv_CM10POR
034600120404     c                   eval         CM10VOL    =  sv_CM10VOL
034700120404     c                   eval         CM10MCP    =  sv_CM10MCP
034800120404     c                   eval         CM10KM     =  sv_CM10KM
034900120523     c                   eval         CM10PDA    =  sv_CM10PDA
035000120404      *  Consegne
035100120404     c                   eval         CM10COS_C  =  sv_CM10COS_C
035200120404     c                   eval         CM10COM_C  =  sv_CM10COM_C
035300120404     c                   eval         CM10KFA_C  =  sv_CM10KFA_C
035400120404     c                   eval         CM10TVL_C  =  sv_CM10TVL_C
035500120404     c                   eval         CM10NSP_C  =  sv_CM10NSP_C
035600120404     c                   eval         CM10SNA_C  =  sv_CM10SNA_C
035700120404     c                   eval         CM10CLA_C  =  sv_CM10CLA_C
035800120404     c                   eval         CM10BLAF   =  sv_CM10BLAF
035900120404     c                   eval         CM10BLNC   =  sv_CM10BLNC
036000120404      *
036100120404     c                   eval         CM10COS_R  =  sv_CM10COS_R
036200120404     c                   eval         CM10COM_R  =  sv_CM10COM_R
036300120404     c                   eval         CM10KFA_R  =  sv_CM10KFA_R
036400120404     c                   eval         CM10TVL_R  =  sv_CM10TVL_R
036500120404     c                   eval         CM10NSP_R  =  sv_CM10NSP_R
036600120404     c                   eval         CM10SNA_R  =  sv_CM10SNA_R
036700120404     c                   eval         CM10CLA_R  =  sv_CM10CLA_R
036800120404     c                   eval         CM10RMAF   =  sv_CM10RMAF
036900120404     c                   eval         CM10RMFA   =  sv_CM10RMFA
037000120404      *
037100120404      *  Dati basati sulle Tariffe
037200120404     c                   exsr      ctrl_tariffa
037300120404      *
037400120404     c                   eval      CM10TARUGU = stessa_tariffa
037500120404     c                   eval      CM10NOTAR  = No_Tariffa
037600120404      *
037700120404     c                   write     wficnD10M
037800120404      *
037900120404     c                   ENDSR
038000120404      *--------------------------------------------------------------*
038100120404      *  dati sulle tariffe
038200120404      *--------------------------------------------------------------*
038300120404     c     ctrl_tariffa  BEGSR
038400120404      *
038500110823     c     xAUT          klist
038600110823     c                   kFLD                    TGTPDR
038700110823     c                   kFLD                    TGTSML
038800110823      *
038900110823     c     xAUT_fPT      klist
039000110823     c                   kFLD                    fPTPDR
039100110823     c                   kFLD                    fPTSML
039200110823      *
039300120404     c                   eval        TGTPDR = CM10PDR
039400110823     c                   eval        TGTSML = ' '
039500110823      *
039600110823     c                   move      'N'           stessa_tariffa    1
039700110823      *
039800110823     c     xAUT          setll     fiTGT01l
039900110823     c     xAUT          reade     fiTGT01l
040000110823     c                   dow       not %EoF(fiTGT01l)
040100110823     c                   if        tgtDDT <= PARDD1 and
040200110823     c                             tgtDST >= PARDD2
040300110823      * Se utilizzata un'unica tariffa all'interno del periodo selezionato
040400110830     c                   move      ' '           stessa_tariffa
040500110823     c                   leave
040600110823     c                   end
040700110823     c     xAUT          reade     fiTGT01l
040800110823     c                   end
040900110823      *
041000110823      *   Se i calcoli sono basati sulla stessa tariffa,
041100110823      *  Controllo se ci sono tariffe a Giornata
041200110823     c                   clear                   GIORN_Intera      1
041300110823     c                   clear                   GIORN_Mezza       1
041400110823     c                   clear                   GIORN_MinGaran    1
041500110831     c                   clear                   GIORN_Bonus       1
041600110831     c                   clear                   GIORN_Etichett    1
041700110831     c                   clear                   GIORN_Picking     1
041800110831     c                   move      'N'           No_Tariffa        1
041900110823      *
042000110831     c                   if          stessa_tariffa = 'N'
042100110831     c                   move      ' '           No_Tariffa
042200110823      *
042300110831     c                   elseIf      stessa_tariffa = ' '
042400110831      *
042500110823     c                   eval           fPTPDR  = TGTpdr
042600110823     c                   eval           fPTSML  = ' '
042700110830      *
042800110823      *  Nell'ambito dello stesso progressivo
042900110823      *   Quali tariffe ci sono a giornata ?
043000110823     c     xAUT_fPT      setll     fifPT02l
043100110823     c     xAUT_fPT      reade     fifPT02l
043200110830      *
043300110830     c                   dow       not %EoF(fifPT02l)
043400110831      *
043500110831      *  solo per il progressivo
043600110831     c                   if             fPTPRG  = TGTprg
043700110831      * Tariffa non trovata in assoluto perchè eliminata
043800110831     c                   move      ' '           No_Tariffa
043900110831      *
044000110831      *   solo per tariffe a giornata
044100110831     c                   if             fPTTSR  = 'G' and fPTCTR  = 999
044200110831      *
044300110823      *      Nell'ambito dello stesso Progressivo di periodo
044400110823     c                   if          fptCTD = 'TIG'
044500110823     c                   eval          GIORN_Intera   = 'S'
044600120403     c                   z-add     fptATA        CM10TIG
044700110831      *
044800110823     c                   elseIf      fptCTD = 'TMG'
044900110823     c                   eval          GIORN_Mezza    = 'S'
045000120403     c                   z-add     fptATA        CM10TMG
045100110831      *
045200110823     c                   elseIf      fptCTD = 'MGA'
045300110823     c                   eval          GIORN_MinGaran = 'S'
045400120403     c                   z-add     fptATA        CM10MGA
045500110831      *
045600110831      *
045700110831     c                   elseIf      fptCTD = 'BON'
045800110831     c                   eval          GIORN_Bonus    = 'S'
045900120403     c                   z-add     fptATA        CM10BON
046000110831      *
046100110831     c                   elseIf      fptCTD = 'ETI'
046200110831     c                   eval          GIORN_Etichett = 'S'
046300120403     c                   z-add     fptATA        CM10ETI
046400110831      *
046500110831     c                   elseIf      fptCTD = 'PCK'
046600110831     c                   eval          GIORN_Picking  = 'S'
046700120403     c                   z-add     fptATA        CM10PCK
046800110831      *
046900110823     c                   end
047000110831      *
047100110831     c                   end
047200110831     c                   end
047300110831      *
047400110823     c     xAUT_fPT      reade     fifPT02l
047500110823     c                   end
047600110823      *
047700110823     c                   end
047800110823      *
047900110822      *
048000110822     c                   ENDSR
048100110822      *--------------------------------------------------------------*
