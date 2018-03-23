000100121224     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000200960530     H DECEDIT('0,') DATEDIT(*DMY/)
000300090626      *                                                                     *
000400090626      *? PARAMETRI:  INPUT IL TIPO DI FILE 784 O 781
000500090626      *?               E RESTITUISCE SE CI SONO DEI 784 O 781
000600090626      *---------------------------------------------------------------------*
000700090626      *
000800111214     Fwftmp781  uf   e             disk    rename(trtct14r:RCV781)
000900090629     F                                     INFDS(recDS781)
001000111214     Fwftmp784  uf   e             disk    rename(trtct14r:RCV784)
001100090629     F                                     INFDS(recDS784)
001200090626      *
001300090626      *---------------------------------------------------------------------*
001400090629     D EDST00        E DS                  EXTNAME(EDST00DS)
001500090629     D EDSD05        E DS                  EXTNAME(EDSD05DS)
001600090629      *
001700050829     D WDAT            DS          1950
001800090629     D  STAPRG                 1      4  0
001900090629     D  STATPR                 5      8
002000090629      *
002100090629      * numero relativo di record
002200090629     D recDS781        DS
002300090629     D  NRec781              397    400B 0
002400090626      *
002500090629     D recDS784        DS
002600090629     D  NRec784              397    400B 0
002700090629      *
002800090630     d svNRec          s                   like(NRec781)
002900090629      *
003000090630     d tsNRec          s                   like(NRec781)
003100090629      *
003200090630     d NRecPrimoDett   s                   like(NRec781)
003300090630      *
003400050831      *parametri
003500090626     D  TIPO           s              4
003600090626     D  TIPO784        s              1
003700090626     D  TIPO781        s              1
003800090630     D  siDettaglio    s              1
003900090630     D  esci           s              1
004000050831      *---------------------------------------------------------------------------
004100090626      *? Seleziona il Flat File
004200090626      *---------------------------------------------------------------------------
004300090626     C     *ENTRY        PLIST
004400090626     C                   PARM                    TIPO784
004500090626     C                   PARM                    TIPO781
004600090626      *
004700090626     C                   clear                   TIPO784
004800090629     C                   clear                   TIPO781
004900090626      *
005000090629      *   Elimina stati di ATR
005100090626     c                   exsr      file784
005200090629      *
005300090629      *   Elimina stati di Delivery
005400090626     c                   exsr      file781
005500050831
005600050831     C                   SETON                                        LR
005700090626      *------------------------------------------------------------
005800090626     c     file784       begsr
005900090626      *
006000090701     C                   clear                   tsnrec
006100090701     C                   clear                   svnrec
006200090701     c                   clear                   NRecPrimoDett
006300090701     C                   clear                   siDettaglio
006400090701      *
006500111214     C                   READ      wftmp784
006600090626      *
006700111214     C                   dow       not %EOF(wftmp784)
006800050829     c                   movel     flatrec       wdat
006900090629      *
007000090629      * Tipo Riga Iniziale
007100090629     c                   if        statpr = 'ST00'
007200090701      * Aggiorna TESTATA
007300090701     c                   eval      EDST00 = wdat
007400090701     C                   MOVEL     '784'         TA1001
007500090701     c                   eval      flatrec = EDST00
007600111214     c                   update    RCV784
007700090701      *
007800090629      *  Solo alla testata successiva
007900090701     c                   if        tsNRec > 0 and siDettaglio = ' '
008000090701     c                             and NRecPrimoDett > 0
008100090629      *  Se non ci sono dettagli per la testata precedente occorre eliminarla
008200090629     c                   exsr      elimina_TES784
008300090629     c                   end
008400090630      * resetta il flag
008500090701     c                   clear                   NRecPrimoDett
008600090630     C                   eval        siDettaglio = *blank
008700090630     c                   z-add     NRec784       tsNRec
008800090701      *
008900090626     c                   end
009000090630      *
009100090629      *  Inizio Dettaglio
009200090629     c                   if        statpr = 'SD00'
009300090630     c                   z-add     NRec784       svNRec
009400090630     c                   if        NRecPrimoDett = 0
009500090630     c                   z-add     NRec784       NRecPrimoDett
009600090629     c                   end
009700090630     c                   end
009800090629      *  STATUS
009900090629     c                   IF        statpr = 'SD05'
010000090629     c                   eval      EDSD05 = wdat
010100090629      **   se sono stati ATR deve eliminare il dettaglio
010200111214     c                   if        DB9011 = '1  ' or DB9011 = '3  '
010300111214     c                             or DB9011 = '29 '
010400090629     c                   exsr      elimina_784
010500090629     c                   else
010600090629     C                   eval        TIPO784 = 'S'
010700090630     C                   eval        siDettaglio  = 'S'
010800090629     c                   end
010900090629     c                   endIF
011000090629      *
011100111214     C                   READ      wftmp784
011200090626     c                   end
011300090701      **
011400090701      *  Attenzione se a fine File si deve eliminare la testata
011500111214     C                   IF            %EOF(wftmp784)
011600090701      *  Solo alla testata successiva
011700090701     c                   if        tsNRec > 0 and siDettaglio = ' '
011800090701      *  Se non ci sono dettagli per la testata precedente occorre eliminarla
011900090701     c                   exsr      elimina_TES784
012000090701     c                   end
012100090701     c                   endIF
012200090701      **
012300050831     c                   endsr
012400960530     C*----------------------------------------------------------------
012500090629     c     elimina_784   begsr
012600090629      *
012700090630     c                   move      *blank        esci
012800090629      *
012900111214     c     svNRec        setll     wftmp784
013000111214     C                   READ      wftmp784
013100090629      *
013200111214     C                   dow       not %EOF(wftmp784)
013300090629     c                   movel     flatrec       wdat
013400090629      *
013500090701     c                   IF          statpr = 'SD00' or statpr = 'ST00'
013600090629      *
013700090630     c                   if           esci =' '
013800090630     c                   move      'S'           esci
013900090629     c                   else
014000090629      *  Riposiziona prima di uscire x il ciclo principale
014100090630     c                   z-add     NRec784       svNRec
014200111214     c     svNRec        setLL     wftmp784
014300090629     c                   leave
014400090629     c                   end
014500090629      *
014600090629     c                   endIF
014700090629      *
014800090629     C                   delete    RCV784
014900111214     C                   READ      wftmp784
015000090629     c                   end
015100090629      *
015200090629      * è uscito dopo aver riposizionato il record per il ciclo principale
015300090629      *   che dovrà rileggere il primo record di dettaglio
015400090629      *
015500090629     c                   endsr
015600090629     C*----------------------------------------------------------------
015700090629     c     elimina_TES784begsr
015800090630      *
015900111214     c     tsNRec        setll     wftmp784
016000111214     C                   READ      wftmp784
016100090630      *
016200120105      * Potrei avere più testate e più dettagli quindi x sicurezza
016300120105      *   controlla che la testata da cancellare sia precedente
016400120105     C                   dow       not %EOF(wftmp784) and
016500120105     C                             ( NRec784<NRecPrimoDett or NRecPrimoDett=0)
016600090630     c                   z-add     NRec784       svNRec
016700090630     C                   delete    RCV784
016800111214     C                   READ      wftmp784
016900090630     c                   end
017000090630      *
017100090630      * è uscito ADESSO riposiziona il record per il ciclo principale
017200090630      *   che dovrà rileggere il primo record di dettaglio
017300111214     c     svNRec        setLL     wftmp784
017400090629      *
017500090629     c                   endsr
017600090629     C*----------------------------------------------------------------
017700090629     c     file781       begsr
017800090701      *
017900090701     C                   clear                   tsnrec
018000090701     C                   clear                   svnrec
018100090701     c                   clear                   NRecPrimoDett
018200090701     C                   clear                   siDettaglio
018300090629      *
018400111214     C                   READ      wftmp781
018500090629      *
018600111214     C                   dow       not %EOF(wftmp781)
018700090629     c                   movel     flatrec       wdat
018800090629      *
018900090629      * Tipo Riga Iniziale
019000090629     c                   if        statpr = 'ST00'
019100090701      * Aggiorna TESTATA
019200090701     c                   eval      EDST00 = wdat
019300090701     C                   MOVEL     '781'         TA1001
019400090701     c                   eval      flatrec = EDST00
019500111214     c                   update    RCV781
019600090701      *
019700090630      *  Solo alla testata successiva
019800090630     c                   if        tsNRec > 0 and siDettaglio = ' '
019900090701     c                             and NRecPrimoDett > 0
020000090630      *  Se non ci sono dettagli per la testata precedente occorre eliminarla
020100090630     c                   exsr      elimina_TES781
020200090630     c                   end
020300090630      * resetta il flag
020400090701     c                   clear                   NRecPrimoDett
020500090630     C                   eval        siDettaglio = *blank
020600090630     c                   z-add     NRec781       tsNRec
020700090629     c                   end
020800090629      *  Inizio Dettaglio
020900090629     c                   if        statpr = 'SD00'
021000090630     c                   z-add     NRec781       svNRec
021100090630     c                   if        NRecPrimoDett = 0
021200090630     c                   z-add     NRec781       NRecPrimoDett
021300090630     c                   end
021400090629     c                   end
021500090629      *  STATUS
021600090629     c                   IF        statpr = 'SD05'
021700090629     c                   eval      EDSD05 = wdat
021800090629      **   se sono stati ATR deve eliminare il dettaglio
021900111214     c                   if        DB9011 = '1  ' or DB9011 = '3  '
022000111214     c                             or DB9011 = '29 '
022100090629     C                   eval        TIPO781 = 'S'
022200090630     C                   eval        siDettaglio  = 'S'
022300090629     c                   else
022400090629     c                   exsr      elimina_781
022500090629     c                   end
022600090629     c                   endIF
022700090629      *
022800111214     C                   READ      wftmp781
022900090629     c                   end
023000090629      *
023100090701      **
023200090701      *  Attenzione se a fine File si deve eliminare la testata
023300111214     C                   IF            %EOF(wftmp781)
023400090701      *  Solo alla testata successiva
023500090701     c                   if        tsNRec > 0 and siDettaglio = ' '
023600090701      *  Se non ci sono dettagli per la testata precedente occorre eliminarla
023700090701     c                   exsr      elimina_TES781
023800090701     c                   end
023900090701     c                   endIF
024000090701      **
024100090629     c                   endsr
024200090629     C*----------------------------------------------------------------
024300090629     c     elimina_781   begsr
024400090629      *
024500090630     c                   move      *blank        esci
024600090629      *
024700111214     c     svNRec        setll     wftmp781
024800111214     C                   READ      wftmp781
024900090629      *
025000111214     C                   dow       not %EOF(wftmp781)
025100090629     c                   movel     flatrec       wdat
025200090629      *
025300090701     c                   IF          statpr = 'SD00' or statpr = 'ST00'
025400090629      *
025500090630     c                   if           esci =' '
025600090630     c                   move      'S'           esci
025700090629     c                   else
025800090629      *  Riposiziona prima di uscire x il ciclo principale
025900090630     c                   z-add     NRec781       svNRec
026000111214     c     svNRec        setLL     wftmp781
026100090629     c                   leave
026200090629     c                   end
026300090629      *
026400090629     c                   endIF
026500090629      *
026600090629     C                   delete    RCV781
026700111214     C                   READ      wftmp781
026800090629     c                   end
026900090629      *
027000090629      * è uscito dopo aver riposizionato il record per il ciclo principale
027100090629      *   che dovrà rileggere il primo record di dettaglio
027200090629      *
027300090629     c                   endsr
027400090629     C*----------------------------------------------------------------
027500090630     c     elimina_TES781begsr
027600090630      *
027700111214     c     tsNRec        setll     wftmp781
027800111214     C                   READ      wftmp781
027900090630      *
028000120105      * Potrei avere più testate e più dettagli quindi x sicurezza
028100120105      *   controlla che la testata da cancellare sia precedente
028200120105     C                   dow       not %EOF(wftmp781) and
028300120105     C                             ( NRec781<NRecPrimoDett or NRecPrimoDett=0)
028400090630     c                   z-add     NRec781       svNRec
028500090630     C                   delete    RCV781
028600111214     C                   READ      wftmp781
028700090630     c                   end
028800090630      *
028900090630      * è uscito ADESSO riposiziona il record per il ciclo principale
029000090630      *   che dovrà rileggere il primo record di dettaglio
029100111214     c     svNRec        setLL     wftmp781
029200090630      *
029300090630     c                   endsr
029400090630     C*----------------------------------------------------------------
