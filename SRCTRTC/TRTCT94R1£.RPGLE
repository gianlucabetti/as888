000100960530     H DECEDIT('0,') DATEDIT(*DMY/)
000200090626      *                                                                     *
000300090626      *? PARAMETRI:  INPUT IL TIPO DI FILE 784 O 781
000400090626      *?               E RESTITUISCE SE CI SONO DEI 784 O 781
000500090626      *---------------------------------------------------------------------*
000600090626      *
000700101018     Fwfrcv781  uf   e             disk    rename(trtct94r:RCV781)
000800090629     F                                     INFDS(recDS781)
000900101018     Fwfrcv784  uf   e             disk    rename(trtct94r:RCV784)
001000090629     F                                     INFDS(recDS784)
001100090626      *
001200090626      *---------------------------------------------------------------------*
001300090629     D EDST00        E DS                  EXTNAME(EDST00DS)
001400090629     D EDSD05        E DS                  EXTNAME(EDSD05DS)
001500090629      *
001600050829     D WDAT            DS          1950
001700090629     D  STAPRG                 1      4  0
001800090629     D  STATPR                 5      8
001900090629      *
002000090629      * numero relativo di record
002100090629     D recDS781        DS
002200090629     D  NRec781              397    400B 0
002300090626      *
002400090629     D recDS784        DS
002500090629     D  NRec784              397    400B 0
002600090629      *
002700090630     d svNRec          s                   like(NRec781)
002800090629      *
002900090630     d tsNRec          s                   like(NRec781)
003000090629      *
003100090630     d NRecPrimoDett   s                   like(NRec781)
003200090630      *
003300050831      *parametri
003400090626     D  TIPO           s              4
003500090626     D  TIPO784        s              1
003600090626     D  TIPO781        s              1
003700090630     D  siDettaglio    s              1
003800090630     D  esci           s              1
003900050831      *---------------------------------------------------------------------------
004000090626      *? Seleziona il Flat File
004100090626      *---------------------------------------------------------------------------
004200090626     C     *ENTRY        PLIST
004300090626     C                   PARM                    TIPO784
004400090626     C                   PARM                    TIPO781
004500090626      *
004600090626     C                   clear                   TIPO784
004700090629     C                   clear                   TIPO781
004800090626      *
004900090629      *   Elimina stati di ATR
005000090626     c                   exsr      file784
005100090629      *
005200090629      *   Elimina stati di Delivery
005300090626     c                   exsr      file781
005400050831
005500050831     C                   SETON                                        LR
005600090626      *------------------------------------------------------------
005700090626     c     file784       begsr
005800090626      *
005900090701     C                   clear                   tsnrec
006000090701     C                   clear                   svnrec
006100090701     c                   clear                   NRecPrimoDett
006200090701     C                   clear                   siDettaglio
006300090701      *
006400090626     C                   READ      wfrcv784
006500090626      *
006600090626     C                   dow       not %EOF(wfrcv784)
006700050829     c                   movel     flatrec       wdat
006800090629      *
006900090629      * Tipo Riga Iniziale
007000090629     c                   if        statpr = 'ST00'
007100090701      * Aggiorna TESTATA
007200090701     c                   eval      EDST00 = wdat
007300090701     C                   MOVEL     '784'         TA1001
007400090701     c                   eval      flatrec = EDST00
007500090701     c                   update    RCV784
007600090701      *
007700090629      *  Solo alla testata successiva
007800090701     c                   if        tsNRec > 0 and siDettaglio = ' '
007900090701     c                             and NRecPrimoDett > 0
008000090629      *  Se non ci sono dettagli per la testata precedente occorre eliminarla
008100090629     c                   exsr      elimina_TES784
008200090629     c                   end
008300090630      * resetta il flag
008400090701     c                   clear                   NRecPrimoDett
008500090630     C                   eval        siDettaglio = *blank
008600090630     c                   z-add     NRec784       tsNRec
008700090701      *
008800090626     c                   end
008900090630      *
009000090629      *  Inizio Dettaglio
009100090629     c                   if        statpr = 'SD00'
009200090630     c                   z-add     NRec784       svNRec
009300090630     c                   if        NRecPrimoDett = 0
009400090630     c                   z-add     NRec784       NRecPrimoDett
009500090629     c                   end
009600090630     c                   end
009700090629      *  STATUS
009800090629     c                   IF        statpr = 'SD05'
009900090629     c                   eval      EDSD05 = wdat
010000090629      **   se sono stati ATR deve eliminare il dettaglio
010100090629     c                   if        DB9011 = '1  ' or DB9011 = '3  '
010200090629     c                   exsr      elimina_784
010300090629     c                   else
010400090629     C                   eval        TIPO784 = 'S'
010500090630     C                   eval        siDettaglio  = 'S'
010600090629     c                   end
010700090629     c                   endIF
010800090629      *
010900090626     C                   READ      wfrcv784
011000090626     c                   end
011100090701      **
011200090701      *  Attenzione se a fine File si deve eliminare la testata
011300090701     C                   IF            %EOF(wfrcv784)
011400090701      *  Solo alla testata successiva
011500090701     c                   if        tsNRec > 0 and siDettaglio = ' '
011600090701      *  Se non ci sono dettagli per la testata precedente occorre eliminarla
011700090701     c                   exsr      elimina_TES784
011800090701     c                   end
011900090701     c                   endIF
012000090701      **
012100050831     c                   endsr
012200960530     C*----------------------------------------------------------------
012300090629     c     elimina_784   begsr
012400090629      *
012500090630     c                   move      *blank        esci
012600090629      *
012700090630     c     svNRec        setll     wfrcv784
012800090629     C                   READ      wfrcv784
012900090629      *
013000090629     C                   dow       not %EOF(wfrcv784)
013100090629     c                   movel     flatrec       wdat
013200090629      *
013300090701     c                   IF          statpr = 'SD00' or statpr = 'ST00'
013400090629      *
013500090630     c                   if           esci =' '
013600090630     c                   move      'S'           esci
013700090629     c                   else
013800090629      *  Riposiziona prima di uscire x il ciclo principale
013900090630     c                   z-add     NRec784       svNRec
014000090630     c     svNRec        setLL     wfrcv784
014100090629     c                   leave
014200090629     c                   end
014300090629      *
014400090629     c                   endIF
014500090629      *
014600090629     C                   delete    RCV784
014700090629     C                   READ      wfrcv784
014800090629     c                   end
014900090629      *
015000090629      * è uscito dopo aver riposizionato il record per il ciclo principale
015100090629      *   che dovrà rileggere il primo record di dettaglio
015200090629      *
015300090629     c                   endsr
015400090629     C*----------------------------------------------------------------
015500090629     c     elimina_TES784begsr
015600090630      *
015700090630     c     tsNRec        setll     wfrcv784
015800090630     C                   READ      wfrcv784
015900090630      *
016000090630     C                   dow       not %EOF(wfrcv784) and NRec784 <NRecPrimoDett
016100090630     c                   z-add     NRec784       svNRec
016200090630     C                   delete    RCV784
016300090630     C                   READ      wfrcv784
016400090630     c                   end
016500090630      *
016600090630      * è uscito ADESSO riposiziona il record per il ciclo principale
016700090630      *   che dovrà rileggere il primo record di dettaglio
016800090630     c     svNRec        setLL     wfrcv784
016900090629      *
017000090629     c                   endsr
017100090629     C*----------------------------------------------------------------
017200090629     c     file781       begsr
017300090701      *
017400090701     C                   clear                   tsnrec
017500090701     C                   clear                   svnrec
017600090701     c                   clear                   NRecPrimoDett
017700090701     C                   clear                   siDettaglio
017800090629      *
017900090629     C                   READ      wfrcv781
018000090629      *
018100090629     C                   dow       not %EOF(wfrcv781)
018200090629     c                   movel     flatrec       wdat
018300090629      *
018400090629      * Tipo Riga Iniziale
018500090629     c                   if        statpr = 'ST00'
018600090701      * Aggiorna TESTATA
018700090701     c                   eval      EDST00 = wdat
018800090701     C                   MOVEL     '781'         TA1001
018900090701     c                   eval      flatrec = EDST00
019000090701     c                   update    RCV781
019100090701      *
019200090630      *  Solo alla testata successiva
019300090630     c                   if        tsNRec > 0 and siDettaglio = ' '
019400090701     c                             and NRecPrimoDett > 0
019500090630      *  Se non ci sono dettagli per la testata precedente occorre eliminarla
019600090630     c                   exsr      elimina_TES781
019700090630     c                   end
019800090630      * resetta il flag
019900090701     c                   clear                   NRecPrimoDett
020000090630     C                   eval        siDettaglio = *blank
020100090630     c                   z-add     NRec781       tsNRec
020200090629     c                   end
020300090629      *  Inizio Dettaglio
020400090629     c                   if        statpr = 'SD00'
020500090630     c                   z-add     NRec781       svNRec
020600090630     c                   if        NRecPrimoDett = 0
020700090630     c                   z-add     NRec781       NRecPrimoDett
020800090630     c                   end
020900090629     c                   end
021000090629      *  STATUS
021100090629     c                   IF        statpr = 'SD05'
021200090629     c                   eval      EDSD05 = wdat
021300090629      **   se sono stati ATR deve eliminare il dettaglio
021400090629     c                   if        DB9011 = '1  ' or DB9011 = '3  '
021500090629     C                   eval        TIPO781 = 'S'
021600090630     C                   eval        siDettaglio  = 'S'
021700090629     c                   else
021800090629     c                   exsr      elimina_781
021900090629     c                   end
022000090629     c                   endIF
022100090629      *
022200090629     C                   READ      wfrcv781
022300090629     c                   end
022400090629      *
022500090701      **
022600090701      *  Attenzione se a fine File si deve eliminare la testata
022700090701     C                   IF            %EOF(wfrcv781)
022800090701      *  Solo alla testata successiva
022900090701     c                   if        tsNRec > 0 and siDettaglio = ' '
023000090701      *  Se non ci sono dettagli per la testata precedente occorre eliminarla
023100090701     c                   exsr      elimina_TES781
023200090701     c                   end
023300090701     c                   endIF
023400090701      **
023500090629     c                   endsr
023600090629     C*----------------------------------------------------------------
023700090629     c     elimina_781   begsr
023800090629      *
023900090630     c                   move      *blank        esci
024000090629      *
024100090630     c     svNRec        setll     wfrcv781
024200090629     C                   READ      wfrcv781
024300090629      *
024400090629     C                   dow       not %EOF(wfrcv781)
024500090629     c                   movel     flatrec       wdat
024600090629      *
024700090701     c                   IF          statpr = 'SD00' or statpr = 'ST00'
024800090629      *
024900090630     c                   if           esci =' '
025000090630     c                   move      'S'           esci
025100090629     c                   else
025200090629      *  Riposiziona prima di uscire x il ciclo principale
025300090630     c                   z-add     NRec781       svNRec
025400090630     c     svNRec        setLL     wfrcv781
025500090629     c                   leave
025600090629     c                   end
025700090629      *
025800090629     c                   endIF
025900090629      *
026000090629     C                   delete    RCV781
026100090629     C                   READ      wfrcv781
026200090629     c                   end
026300090629      *
026400090629      * è uscito dopo aver riposizionato il record per il ciclo principale
026500090629      *   che dovrà rileggere il primo record di dettaglio
026600090629      *
026700090629     c                   endsr
026800090629     C*----------------------------------------------------------------
026900090630     c     elimina_TES781begsr
027000090630      *
027100090630     c     tsNRec        setll     wfrcv781
027200090630     C                   READ      wfrcv781
027300090630      *
027400090630     C                   dow       not %EOF(wfrcv781) and NRec781 <NRecPrimoDett
027500090630     c                   z-add     NRec781       svNRec
027600090630     C                   delete    RCV781
027700090630     C                   READ      wfrcv781
027800090630     c                   end
027900090630      *
028000090630      * è uscito ADESSO riposiziona il record per il ciclo principale
028100090630      *   che dovrà rileggere il primo record di dettaglio
028200090630     c     svNRec        setLL     wfrcv781
028300090630      *
028400090630     c                   endsr
028500090630     C*----------------------------------------------------------------
