000100960530     H DECEDIT('0,') DATEDIT(*DMY/)
000200090626      *                                                                     *
000300090626      *? PARAMETRI:  INPUT IL TIPO DI FILE 784 O 781
000400090626      *?               E RESTITUISCE SE CI SONO DEI 784 O 781
000500090626      *---------------------------------------------------------------------*
000600090626      *
000700111214     Fwftmp781  uf   e             disk    rename(trtct14r:RCV781)
000800090629     F                                     INFDS(recDS781)
000900111214     Fwftmp784  uf   e             disk    rename(trtct14r:RCV784)
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
006400111214     C                   READ      wftmp784
006500090626      *
006600111214     C                   dow       not %EOF(wftmp784)
006700050829     c                   movel     flatrec       wdat
006800090629      *
006900090629      * Tipo Riga Iniziale
007000090629     c                   if        statpr = 'ST00'
007100090701      * Aggiorna TESTATA
007200090701     c                   eval      EDST00 = wdat
007300090701     C                   MOVEL     '784'         TA1001
007400090701     c                   eval      flatrec = EDST00
007500111214     c                   update    RCV784
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
010100111214     c                   if        DB9011 = '1  ' or DB9011 = '3  '
010200111214     c                             or DB9011 = '29 '
010300090629     c                   exsr      elimina_784
010400090629     c                   else
010500090629     C                   eval        TIPO784 = 'S'
010600090630     C                   eval        siDettaglio  = 'S'
010700090629     c                   end
010800090629     c                   endIF
010900090629      *
011000111214     C                   READ      wftmp784
011100090626     c                   end
011200090701      **
011300090701      *  Attenzione se a fine File si deve eliminare la testata
011400111214     C                   IF            %EOF(wftmp784)
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
012800111214     c     svNRec        setll     wftmp784
012900111214     C                   READ      wftmp784
013000090629      *
013100111214     C                   dow       not %EOF(wftmp784)
013200090629     c                   movel     flatrec       wdat
013300090629      *
013400090701     c                   IF          statpr = 'SD00' or statpr = 'ST00'
013500090629      *
013600090630     c                   if           esci =' '
013700090630     c                   move      'S'           esci
013800090629     c                   else
013900090629      *  Riposiziona prima di uscire x il ciclo principale
014000090630     c                   z-add     NRec784       svNRec
014100111214     c     svNRec        setLL     wftmp784
014200090629     c                   leave
014300090629     c                   end
014400090629      *
014500090629     c                   endIF
014600090629      *
014700090629     C                   delete    RCV784
014800111214     C                   READ      wftmp784
014900090629     c                   end
015000090629      *
015100090629      * è uscito dopo aver riposizionato il record per il ciclo principale
015200090629      *   che dovrà rileggere il primo record di dettaglio
015300090629      *
015400090629     c                   endsr
015500090629     C*----------------------------------------------------------------
015600090629     c     elimina_TES784begsr
015700090630      *
015800111214     c     tsNRec        setll     wftmp784
015900111214     C                   READ      wftmp784
016000090630      *
016100120105      * Potrei avere più testate e più dettagli quindi x sicurezza
016200120105      *   controlla che la testata da cancellare sia precedente
016300120105     C                   dow       not %EOF(wftmp784) and
016400120105     C                             ( NRec784<NRecPrimoDett or NRecPrimoDett=0)
016500090630     c                   z-add     NRec784       svNRec
016600090630     C                   delete    RCV784
016700111214     C                   READ      wftmp784
016800090630     c                   end
016900090630      *
017000090630      * è uscito ADESSO riposiziona il record per il ciclo principale
017100090630      *   che dovrà rileggere il primo record di dettaglio
017200111214     c     svNRec        setLL     wftmp784
017300090629      *
017400090629     c                   endsr
017500090629     C*----------------------------------------------------------------
017600090629     c     file781       begsr
017700090701      *
017800090701     C                   clear                   tsnrec
017900090701     C                   clear                   svnrec
018000090701     c                   clear                   NRecPrimoDett
018100090701     C                   clear                   siDettaglio
018200090629      *
018300111214     C                   READ      wftmp781
018400090629      *
018500111214     C                   dow       not %EOF(wftmp781)
018600090629     c                   movel     flatrec       wdat
018700090629      *
018800090629      * Tipo Riga Iniziale
018900090629     c                   if        statpr = 'ST00'
019000090701      * Aggiorna TESTATA
019100090701     c                   eval      EDST00 = wdat
019200090701     C                   MOVEL     '781'         TA1001
019300090701     c                   eval      flatrec = EDST00
019400111214     c                   update    RCV781
019500090701      *
019600090630      *  Solo alla testata successiva
019700090630     c                   if        tsNRec > 0 and siDettaglio = ' '
019800090701     c                             and NRecPrimoDett > 0
019900090630      *  Se non ci sono dettagli per la testata precedente occorre eliminarla
020000090630     c                   exsr      elimina_TES781
020100090630     c                   end
020200090630      * resetta il flag
020300090701     c                   clear                   NRecPrimoDett
020400090630     C                   eval        siDettaglio = *blank
020500090630     c                   z-add     NRec781       tsNRec
020600090629     c                   end
020700090629      *  Inizio Dettaglio
020800090629     c                   if        statpr = 'SD00'
020900090630     c                   z-add     NRec781       svNRec
021000090630     c                   if        NRecPrimoDett = 0
021100090630     c                   z-add     NRec781       NRecPrimoDett
021200090630     c                   end
021300090629     c                   end
021400090629      *  STATUS
021500090629     c                   IF        statpr = 'SD05'
021600090629     c                   eval      EDSD05 = wdat
021700090629      **   se sono stati ATR deve eliminare il dettaglio
021800111214     c                   if        DB9011 = '1  ' or DB9011 = '3  '
021900111214     c                             or DB9011 = '29 '
022000090629     C                   eval        TIPO781 = 'S'
022100090630     C                   eval        siDettaglio  = 'S'
022200090629     c                   else
022300090629     c                   exsr      elimina_781
022400090629     c                   end
022500090629     c                   endIF
022600090629      *
022700111214     C                   READ      wftmp781
022800090629     c                   end
022900090629      *
023000090701      **
023100090701      *  Attenzione se a fine File si deve eliminare la testata
023200111214     C                   IF            %EOF(wftmp781)
023300090701      *  Solo alla testata successiva
023400090701     c                   if        tsNRec > 0 and siDettaglio = ' '
023500090701      *  Se non ci sono dettagli per la testata precedente occorre eliminarla
023600090701     c                   exsr      elimina_TES781
023700090701     c                   end
023800090701     c                   endIF
023900090701      **
024000090629     c                   endsr
024100090629     C*----------------------------------------------------------------
024200090629     c     elimina_781   begsr
024300090629      *
024400090630     c                   move      *blank        esci
024500090629      *
024600111214     c     svNRec        setll     wftmp781
024700111214     C                   READ      wftmp781
024800090629      *
024900111214     C                   dow       not %EOF(wftmp781)
025000090629     c                   movel     flatrec       wdat
025100090629      *
025200090701     c                   IF          statpr = 'SD00' or statpr = 'ST00'
025300090629      *
025400090630     c                   if           esci =' '
025500090630     c                   move      'S'           esci
025600090629     c                   else
025700090629      *  Riposiziona prima di uscire x il ciclo principale
025800090630     c                   z-add     NRec781       svNRec
025900111214     c     svNRec        setLL     wftmp781
026000090629     c                   leave
026100090629     c                   end
026200090629      *
026300090629     c                   endIF
026400090629      *
026500090629     C                   delete    RCV781
026600111214     C                   READ      wftmp781
026700090629     c                   end
026800090629      *
026900090629      * è uscito dopo aver riposizionato il record per il ciclo principale
027000090629      *   che dovrà rileggere il primo record di dettaglio
027100090629      *
027200090629     c                   endsr
027300090629     C*----------------------------------------------------------------
027400090630     c     elimina_TES781begsr
027500090630      *
027600111214     c     tsNRec        setll     wftmp781
027700111214     C                   READ      wftmp781
027800090630      *
027900120105      * Potrei avere più testate e più dettagli quindi x sicurezza
028000120105      *   controlla che la testata da cancellare sia precedente
028100120105     C                   dow       not %EOF(wftmp781) and
028200120105     C                             ( NRec781<NRecPrimoDett or NRecPrimoDett=0)
028300090630     c                   z-add     NRec781       svNRec
028400090630     C                   delete    RCV781
028500111214     C                   READ      wftmp781
028600090630     c                   end
028700090630      *
028800090630      * è uscito ADESSO riposiziona il record per il ciclo principale
028900090630      *   che dovrà rileggere il primo record di dettaglio
029000111214     c     svNRec        setLL     wftmp781
029100090630      *
029200090630     c                   endsr
029300090630     C*----------------------------------------------------------------
