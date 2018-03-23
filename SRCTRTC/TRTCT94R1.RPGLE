000100121224     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000200960530     H DECEDIT('0,') DATEDIT(*DMY/)
000300090626      *                                                                     *
000400090626      *? PARAMETRI:  INPUT IL TIPO DI FILE 784 O 781
000500090626      *?               E RESTITUISCE SE CI SONO DEI 784 O 781
000600090626      *---------------------------------------------------------------------*
000700090626      *
000800101018     Fwfrcv781  uf   e             disk    rename(trtct94r:RCV781)
000900090629     F                                     INFDS(recDS781)
001000101018     Fwfrcv784  uf   e             disk    rename(trtct94r:RCV784)
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
006500090626     C                   READ      wfrcv784
006600090626      *
006700090626     C                   dow       not %EOF(wfrcv784)
006800050829     c                   movel     flatrec       wdat
006900090629      *
007000090629      * Tipo Riga Iniziale
007100090629     c                   if        statpr = 'ST00'
007200090701      * Aggiorna TESTATA
007300090701     c                   eval      EDST00 = wdat
007400090701     C                   MOVEL     '784'         TA1001
007500090701     c                   eval      flatrec = EDST00
007600090701     c                   update    RCV784
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
010200090629     c                   if        DB9011 = '1  ' or DB9011 = '3  '
010300090629     c                   exsr      elimina_784
010400090629     c                   else
010500090629     C                   eval        TIPO784 = 'S'
010600090630     C                   eval        siDettaglio  = 'S'
010700090629     c                   end
010800090629     c                   endIF
010900090629      *
011000090626     C                   READ      wfrcv784
011100090626     c                   end
011200090701      **
011300090701      *  Attenzione se a fine File si deve eliminare la testata
011400090701     C                   IF            %EOF(wfrcv784)
011500090701      *  Solo alla testata successiva
011600090701     c                   if        tsNRec > 0 and siDettaglio = ' '
011700090701      *  Se non ci sono dettagli per la testata precedente occorre eliminarla
011800090701     c                   exsr      elimina_TES784
011900090701     c                   end
012000090701     c                   endIF
012100090701      **
012200050831     c                   endsr
012300960530     C*----------------------------------------------------------------
012400090629     c     elimina_784   begsr
012500090629      *
012600090630     c                   move      *blank        esci
012700090629      *
012800090630     c     svNRec        setll     wfrcv784
012900090629     C                   READ      wfrcv784
013000090629      *
013100090629     C                   dow       not %EOF(wfrcv784)
013200090629     c                   movel     flatrec       wdat
013300090629      *
013400090701     c                   IF          statpr = 'SD00' or statpr = 'ST00'
013500090629      *
013600090630     c                   if           esci =' '
013700090630     c                   move      'S'           esci
013800090629     c                   else
013900090629      *  Riposiziona prima di uscire x il ciclo principale
014000090630     c                   z-add     NRec784       svNRec
014100090630     c     svNRec        setLL     wfrcv784
014200090629     c                   leave
014300090629     c                   end
014400090629      *
014500090629     c                   endIF
014600090629      *
014700090629     C                   delete    RCV784
014800090629     C                   READ      wfrcv784
014900090629     c                   end
015000090629      *
015100090629      * è uscito dopo aver riposizionato il record per il ciclo principale
015200090629      *   che dovrà rileggere il primo record di dettaglio
015300090629      *
015400090629     c                   endsr
015500090629     C*----------------------------------------------------------------
015600090629     c     elimina_TES784begsr
015700090630      *
015800090630     c     tsNRec        setll     wfrcv784
015900090630     C                   READ      wfrcv784
016000090630      *
016100090630     C                   dow       not %EOF(wfrcv784) and NRec784 <NRecPrimoDett
016200090630     c                   z-add     NRec784       svNRec
016300090630     C                   delete    RCV784
016400090630     C                   READ      wfrcv784
016500090630     c                   end
016600090630      *
016700090630      * è uscito ADESSO riposiziona il record per il ciclo principale
016800090630      *   che dovrà rileggere il primo record di dettaglio
016900090630     c     svNRec        setLL     wfrcv784
017000090629      *
017100090629     c                   endsr
017200090629     C*----------------------------------------------------------------
017300090629     c     file781       begsr
017400090701      *
017500090701     C                   clear                   tsnrec
017600090701     C                   clear                   svnrec
017700090701     c                   clear                   NRecPrimoDett
017800090701     C                   clear                   siDettaglio
017900090629      *
018000090629     C                   READ      wfrcv781
018100090629      *
018200090629     C                   dow       not %EOF(wfrcv781)
018300090629     c                   movel     flatrec       wdat
018400090629      *
018500090629      * Tipo Riga Iniziale
018600090629     c                   if        statpr = 'ST00'
018700090701      * Aggiorna TESTATA
018800090701     c                   eval      EDST00 = wdat
018900090701     C                   MOVEL     '781'         TA1001
019000090701     c                   eval      flatrec = EDST00
019100090701     c                   update    RCV781
019200090701      *
019300090630      *  Solo alla testata successiva
019400090630     c                   if        tsNRec > 0 and siDettaglio = ' '
019500090701     c                             and NRecPrimoDett > 0
019600090630      *  Se non ci sono dettagli per la testata precedente occorre eliminarla
019700090630     c                   exsr      elimina_TES781
019800090630     c                   end
019900090630      * resetta il flag
020000090701     c                   clear                   NRecPrimoDett
020100090630     C                   eval        siDettaglio = *blank
020200090630     c                   z-add     NRec781       tsNRec
020300090629     c                   end
020400090629      *  Inizio Dettaglio
020500090629     c                   if        statpr = 'SD00'
020600090630     c                   z-add     NRec781       svNRec
020700090630     c                   if        NRecPrimoDett = 0
020800090630     c                   z-add     NRec781       NRecPrimoDett
020900090630     c                   end
021000090629     c                   end
021100090629      *  STATUS
021200090629     c                   IF        statpr = 'SD05'
021300090629     c                   eval      EDSD05 = wdat
021400090629      **   se sono stati ATR deve eliminare il dettaglio
021500090629     c                   if        DB9011 = '1  ' or DB9011 = '3  '
021600090629     C                   eval        TIPO781 = 'S'
021700090630     C                   eval        siDettaglio  = 'S'
021800090629     c                   else
021900090629     c                   exsr      elimina_781
022000090629     c                   end
022100090629     c                   endIF
022200090629      *
022300090629     C                   READ      wfrcv781
022400090629     c                   end
022500090629      *
022600090701      **
022700090701      *  Attenzione se a fine File si deve eliminare la testata
022800090701     C                   IF            %EOF(wfrcv781)
022900090701      *  Solo alla testata successiva
023000090701     c                   if        tsNRec > 0 and siDettaglio = ' '
023100090701      *  Se non ci sono dettagli per la testata precedente occorre eliminarla
023200090701     c                   exsr      elimina_TES781
023300090701     c                   end
023400090701     c                   endIF
023500090701      **
023600090629     c                   endsr
023700090629     C*----------------------------------------------------------------
023800090629     c     elimina_781   begsr
023900090629      *
024000090630     c                   move      *blank        esci
024100090629      *
024200090630     c     svNRec        setll     wfrcv781
024300090629     C                   READ      wfrcv781
024400090629      *
024500090629     C                   dow       not %EOF(wfrcv781)
024600090629     c                   movel     flatrec       wdat
024700090629      *
024800090701     c                   IF          statpr = 'SD00' or statpr = 'ST00'
024900090629      *
025000090630     c                   if           esci =' '
025100090630     c                   move      'S'           esci
025200090629     c                   else
025300090629      *  Riposiziona prima di uscire x il ciclo principale
025400090630     c                   z-add     NRec781       svNRec
025500090630     c     svNRec        setLL     wfrcv781
025600090629     c                   leave
025700090629     c                   end
025800090629      *
025900090629     c                   endIF
026000090629      *
026100090629     C                   delete    RCV781
026200090629     C                   READ      wfrcv781
026300090629     c                   end
026400090629      *
026500090629      * è uscito dopo aver riposizionato il record per il ciclo principale
026600090629      *   che dovrà rileggere il primo record di dettaglio
026700090629      *
026800090629     c                   endsr
026900090629     C*----------------------------------------------------------------
027000090630     c     elimina_TES781begsr
027100090630      *
027200090630     c     tsNRec        setll     wfrcv781
027300090630     C                   READ      wfrcv781
027400090630      *
027500090630     C                   dow       not %EOF(wfrcv781) and NRec781 <NRecPrimoDett
027600090630     c                   z-add     NRec781       svNRec
027700090630     C                   delete    RCV781
027800090630     C                   READ      wfrcv781
027900090630     c                   end
028000090630      *
028100090630      * è uscito ADESSO riposiziona il record per il ciclo principale
028200090630      *   che dovrà rileggere il primo record di dettaglio
028300090630     c     svNRec        setLL     wfrcv781
028400090630      *
028500090630     c                   endsr
028600090630     C*----------------------------------------------------------------
