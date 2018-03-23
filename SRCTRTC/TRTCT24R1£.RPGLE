000100121224     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000200960530     H DECEDIT('0,') DATEDIT(*DMY/)
000300090626      *                                                                     *
000400090626      *? PARAMETRI:  INPUT IL TIPO DI FILE 784 O 781
000500090626      *?               E RESTITUISCE SE CI SONO DEI 784 O 781
000600140718      *?   x GEODIS in più elimina degli stati Doppi ricevuti
000700140718      *?     senza ore e minuti.
000800140718      *?  ad esempio ATR arrivato OK che generano duplicato senza hh:mm
000900140718      *?   In ogni caso togliamo tutti gli stati che arrivano con 0000
001000140718      *?     senza ore e minuti sulle date.
001100090626      *---------------------------------------------------------------------*
001200090626      *
001300101018     Fwfrcv781  uf   e             disk    rename(trtct94r:RCV781)
001400090629     F                                     INFDS(recDS781)
001500101018     Fwfrcv784  uf   e             disk    rename(trtct94r:RCV784)
001600090629     F                                     INFDS(recDS784)
001700090626      *
001800090626      *---------------------------------------------------------------------*
001900090629     D EDST00        E DS                  EXTNAME(EDST00DS)
002000090629     D EDSD05        E DS                  EXTNAME(EDSD05DS)
002100090629      *
002200050829     D WDAT            DS          1950
002300090629     D  STAPRG                 1      4  0
002400090629     D  STATPR                 5      8
002500090629      *
002600090629      * numero relativo di record
002700090629     D recDS781        DS
002800090629     D  NRec781              397    400B 0
002900090626      *
003000090629     D recDS784        DS
003100090629     D  NRec784              397    400B 0
003200090629      *
003300090630     d svNRec          s                   like(NRec781)
003400090629      *
003500090630     d tsNRec          s                   like(NRec781)
003600090629      *
003700090630     d NRecPrimoDett   s                   like(NRec781)
003800090630      *
003900050831      *parametri
004000090626     D  TIPO           s              4
004100090626     D  TIPO784        s              1
004200090626     D  TIPO781        s              1
004300090630     D  siDettaglio    s              1
004400090630     D  esci           s              1
004500140718     D  scarta         s              1
004600050831      *---------------------------------------------------------------------------
004700090626      *? Seleziona il Flat File
004800090626      *---------------------------------------------------------------------------
004900090626     C     *ENTRY        PLIST
005000090626     C                   PARM                    TIPO784
005100090626     C                   PARM                    TIPO781
005200090626      *
005300090626     C                   clear                   TIPO784
005400090629     C                   clear                   TIPO781
005500090626      *
005600090629      *   Elimina stati di ATR
005700090626     c                   exsr      file784
005800090629      *
005900090629      *   Elimina stati di Delivery
006000090626     c                   exsr      file781
006100050831
006200050831     C                   SETON                                        LR
006300090626      *------------------------------------------------------------
006400090626     c     file784       begsr
006500090626      *
006600090701     C                   clear                   tsnrec
006700090701     C                   clear                   svnrec
006800090701     c                   clear                   NRecPrimoDett
006900090701     C                   clear                   siDettaglio
007000090701      *
007100090626     C                   READ      wfrcv784
007200090626      *
007300090626     C                   dow       not %EOF(wfrcv784)
007400050829     c                   movel     flatrec       wdat
007500090629      *
007600090629      * Tipo Riga Iniziale
007700090629     c                   if        statpr = 'ST00'
007800090701      * Aggiorna TESTATA
007900090701     c                   eval      EDST00 = wdat
008000090701     C                   MOVEL     '784'         TA1001
008100090701     c                   eval      flatrec = EDST00
008200090701     c                   update    RCV784
008300090701      *
008400090629      *  Solo alla testata successiva
008500090701     c                   if        tsNRec > 0 and siDettaglio = ' '
008600090701     c                             and NRecPrimoDett > 0
008700090629      *  Se non ci sono dettagli per la testata precedente occorre eliminarla
008800090629     c                   exsr      elimina_TES784
008900090629     c                   end
009000090630      * resetta il flag
009100090701     c                   clear                   NRecPrimoDett
009200090630     C                   eval        siDettaglio = *blank
009300090630     c                   z-add     NRec784       tsNRec
009400090701      *
009500090626     c                   end
009600090630      *
009700090629      *  Inizio Dettaglio
009800090629     c                   if        statpr = 'SD00'
009900090630     c                   z-add     NRec784       svNRec
010000090630     c                   if        NRecPrimoDett = 0
010100090630     c                   z-add     NRec784       NRecPrimoDett
010200090629     c                   end
010300090630     c                   end
010400090629      *  STATUS
010500090629     c                   IF        statpr = 'SD05'
010600140718     c                   eval      scarta ='N'
010700090629     c                   eval      EDSD05 = wdat
010800140721      *
010900140721      * Fatto apposta x GEODIS CALBER: x non ricevere dati doppi eliminati gli stati
011000140721      * con Data e ora = 0000
011100140721     c                   if        %subst(DB2380:9:4)='0000'
011200140721     c                   eval      scarta ='S'
011300140721     c                   end
011400140721      *
011500090629      **   se sono stati ATR deve eliminare il dettaglio
011600090629     c                   if        DB9011 = '1  ' or DB9011 = '3  '
011700140718     c                                    or scarta ='S'
011800090629     c                   exsr      elimina_784
011900090629     c                   else
012000090629     C                   eval        TIPO784 = 'S'
012100090630     C                   eval        siDettaglio  = 'S'
012200090629     c                   end
012300090629     c                   endIF
012400090629      *
012500090626     C                   READ      wfrcv784
012600090626     c                   end
012700090701      **
012800090701      *  Attenzione se a fine File si deve eliminare la testata
012900090701     C                   IF            %EOF(wfrcv784)
013000090701      *  Solo alla testata successiva
013100090701     c                   if        tsNRec > 0 and siDettaglio = ' '
013200090701      *  Se non ci sono dettagli per la testata precedente occorre eliminarla
013300090701     c                   exsr      elimina_TES784
013400090701     c                   end
013500090701     c                   endIF
013600090701      **
013700050831     c                   endsr
013800960530     C*----------------------------------------------------------------
013900090629     c     elimina_784   begsr
014000090629      *
014100090630     c                   move      *blank        esci
014200090629      *
014300090630     c     svNRec        setll     wfrcv784
014400090629     C                   READ      wfrcv784
014500090629      *
014600090629     C                   dow       not %EOF(wfrcv784)
014700090629     c                   movel     flatrec       wdat
014800090629      *
014900090701     c                   IF          statpr = 'SD00' or statpr = 'ST00'
015000090629      *
015100090630     c                   if           esci =' '
015200090630     c                   move      'S'           esci
015300090629     c                   else
015400090629      *  Riposiziona prima di uscire x il ciclo principale
015500090630     c                   z-add     NRec784       svNRec
015600090630     c     svNRec        setLL     wfrcv784
015700090629     c                   leave
015800090629     c                   end
015900090629      *
016000090629     c                   endIF
016100090629      *
016200090629     C                   delete    RCV784
016300090629     C                   READ      wfrcv784
016400090629     c                   end
016500090629      *
016600090629      * è uscito dopo aver riposizionato il record per il ciclo principale
016700090629      *   che dovrà rileggere il primo record di dettaglio
016800090629      *
016900090629     c                   endsr
017000090629     C*----------------------------------------------------------------
017100090629     c     elimina_TES784begsr
017200090630      *
017300090630     c     tsNRec        setll     wfrcv784
017400090630     C                   READ      wfrcv784
017500090630      *
017600090630     C                   dow       not %EOF(wfrcv784) and NRec784 <NRecPrimoDett
017700090630     c                   z-add     NRec784       svNRec
017800090630     C                   delete    RCV784
017900090630     C                   READ      wfrcv784
018000090630     c                   end
018100090630      *
018200090630      * è uscito ADESSO riposiziona il record per il ciclo principale
018300090630      *   che dovrà rileggere il primo record di dettaglio
018400090630     c     svNRec        setLL     wfrcv784
018500090629      *
018600090629     c                   endsr
018700090629     C*----------------------------------------------------------------
018800090629     c     file781       begsr
018900090701      *
019000090701     C                   clear                   tsnrec
019100090701     C                   clear                   svnrec
019200090701     c                   clear                   NRecPrimoDett
019300090701     C                   clear                   siDettaglio
019400090629      *
019500090629     C                   READ      wfrcv781
019600090629      *
019700090629     C                   dow       not %EOF(wfrcv781)
019800090629     c                   movel     flatrec       wdat
019900090629      *
020000090629      * Tipo Riga Iniziale
020100090629     c                   if        statpr = 'ST00'
020200090701      * Aggiorna TESTATA
020300090701     c                   eval      EDST00 = wdat
020400090701     C                   MOVEL     '781'         TA1001
020500090701     c                   eval      flatrec = EDST00
020600090701     c                   update    RCV781
020700090701      *
020800090630      *  Solo alla testata successiva
020900090630     c                   if        tsNRec > 0 and siDettaglio = ' '
021000090701     c                             and NRecPrimoDett > 0
021100090630      *  Se non ci sono dettagli per la testata precedente occorre eliminarla
021200090630     c                   exsr      elimina_TES781
021300090630     c                   end
021400090630      * resetta il flag
021500090701     c                   clear                   NRecPrimoDett
021600090630     C                   eval        siDettaglio = *blank
021700090630     c                   z-add     NRec781       tsNRec
021800090629     c                   end
021900090629      *  Inizio Dettaglio
022000090629     c                   if        statpr = 'SD00'
022100090630     c                   z-add     NRec781       svNRec
022200090630     c                   if        NRecPrimoDett = 0
022300090630     c                   z-add     NRec781       NRecPrimoDett
022400090630     c                   end
022500090629     c                   end
022600090629      *  STATUS
022700090629     c                   IF        statpr = 'SD05'
022800140718     c                   eval      scarta ='N'
022900090629     c                   eval      EDSD05 = wdat
023000140721      *
023100140721      * Fatto apposta x GEODIS CALBER: x non ricevere dati doppi eliminati gli stati
023200140721      * con Data e ora = 0000
023300140721     c                   if        %subst(DB2380:9:4)='0000'
023400140721     c                   eval      scarta ='S'
023500140721     c                   end
023600140721      *
023700090629      **   se sono stati ATR deve eliminare il dettaglio
023800140721     c                   if        DB9011 = '1  ' and scarta ='N'
023900140721     c                               or
024000140721     c                             DB9011 = '3  ' and scarta ='N'
024100140721      *
024200140721      *  E NON devono essere scartati perchè hhmm=0000
024300090629     C                   eval        TIPO781 = 'S'
024400090630     C                   eval        siDettaglio  = 'S'
024500140721      *
024600090629     c                   else
024700090629     c                   exsr      elimina_781
024800090629     c                   end
024900140721      *
025000090629     c                   endIF
025100090629      *
025200090629     C                   READ      wfrcv781
025300090629     c                   end
025400090629      *
025500090701      **
025600090701      *  Attenzione se a fine File si deve eliminare la testata
025700090701     C                   IF            %EOF(wfrcv781)
025800090701      *  Solo alla testata successiva
025900090701     c                   if        tsNRec > 0 and siDettaglio = ' '
026000090701      *  Se non ci sono dettagli per la testata precedente occorre eliminarla
026100090701     c                   exsr      elimina_TES781
026200090701     c                   end
026300090701     c                   endIF
026400090701      **
026500090629     c                   endsr
026600090629     C*----------------------------------------------------------------
026700090629     c     elimina_781   begsr
026800090629      *
026900090630     c                   move      *blank        esci
027000090629      *
027100090630     c     svNRec        setll     wfrcv781
027200090629     C                   READ      wfrcv781
027300090629      *
027400090629     C                   dow       not %EOF(wfrcv781)
027500090629     c                   movel     flatrec       wdat
027600090629      *
027700090701     c                   IF          statpr = 'SD00' or statpr = 'ST00'
027800090629      *
027900090630     c                   if           esci =' '
028000090630     c                   move      'S'           esci
028100090629     c                   else
028200090629      *  Riposiziona prima di uscire x il ciclo principale
028300090630     c                   z-add     NRec781       svNRec
028400090630     c     svNRec        setLL     wfrcv781
028500090629     c                   leave
028600090629     c                   end
028700090629      *
028800090629     c                   endIF
028900090629      *
029000090629     C                   delete    RCV781
029100090629     C                   READ      wfrcv781
029200090629     c                   end
029300090629      *
029400090629      * è uscito dopo aver riposizionato il record per il ciclo principale
029500090629      *   che dovrà rileggere il primo record di dettaglio
029600090629      *
029700090629     c                   endsr
029800090629     C*----------------------------------------------------------------
029900090630     c     elimina_TES781begsr
030000090630      *
030100090630     c     tsNRec        setll     wfrcv781
030200090630     C                   READ      wfrcv781
030300090630      *
030400090630     C                   dow       not %EOF(wfrcv781) and NRec781 <NRecPrimoDett
030500090630     c                   z-add     NRec781       svNRec
030600090630     C                   delete    RCV781
030700090630     C                   READ      wfrcv781
030800090630     c                   end
030900090630      *
031000090630      * è uscito ADESSO riposiziona il record per il ciclo principale
031100090630      *   che dovrà rileggere il primo record di dettaglio
031200090630     c     svNRec        setLL     wfrcv781
031300090630      *
031400090630     c                   endsr
031500090630     C*----------------------------------------------------------------
