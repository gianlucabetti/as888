000100121224     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000200960530     H DECEDIT('0,') DATEDIT(*DMY/)
000300180314      //                                                                     *
000400180314      //? PARAMETRI:  INPUT IL TIPO DI FILE 784 O 781
000500180314      //?               E RESTITUISCE SE CI SONO DEI 784 O 781
000600180314      //---------------------------------------------------------------------*
000700180314      //
000800180314     Fwftmp781  uf   e             disk    rename(trtct20r:RCV781)
000900090629     F                                     INFDS(recDS781)
001000180314     Fwftmp784  uf   e             disk    rename(trtct20r:RCV784)
001100090629     F                                     INFDS(recDS784)
001200180314      //
001300180314      //---------------------------------------------------------------------*
001400090629     D EDST00        E DS                  EXTNAME(EDST00DS)
001500090629     D EDSD05        E DS                  EXTNAME(EDSD05DS)
001600180314      //
001700050829     D WDAT            DS          1950
001800090629     D  STAPRG                 1      4  0
001900090629     D  STATPR                 5      8
002000180314      //
002100180314      // numero relativo di record
002200090629     D recDS781        DS
002300090629     D  NRec781              397    400B 0
002400180314      //
002500090629     D recDS784        DS
002600090629     D  NRec784              397    400B 0
002700180314      //
002800090630     d svNRec          s                   like(NRec781)
002900180314      //
003000090630     d tsNRec          s                   like(NRec781)
003100180314      //
003200090630     d NRecPrimoDett   s                   like(NRec781)
003300180314      //
003400180314      //parametri
003500090626     D  TIPO           s              4
003600090626     D  TIPO784        s              1
003700090626     D  TIPO781        s              1
003800090630     D  siDettaglio    s              1
003900090630     D  esci           s              1
004000180314      //---------------------------------------------------------------------------
004100180314      //? Seleziona il Flat File
004200180314      //---------------------------------------------------------------------------
004300090626     C     *ENTRY        PLIST
004400090626     C                   PARM                    TIPO784
004500090626     C                   PARM                    TIPO781
004600180314      //
004601180314      /FREE
004700180314       clear TIPO784;
004800180314       clear TIPO781;
004900180314       //
005000180314       //   Elimina stati di ATR
005100180314       exsr file784;
005200180314       //
005300180314       //   Elimina stati di Delivery
005400180314       exsr file781;
005500050831
005600180314       *INLR = *ON;
005700180314       //------------------------------------------------------------
005800180314       begsr file784;
005900180314         //
006000180314         clear tsnrec;
006100180314         clear svnrec;
006200180314         clear NRecPrimoDett;
006300180314         clear siDettaglio;
006400180314         //
006500180314         READ wftmp784;
006600180314         //
006700180314         dow not %EOF(wftmp784);
006701180314      /END-FREE
006800050829     c                   movel     flatrec       wdat
006900180314      //
007000180314      // Tipo Riga Iniziale
007001180314      /FREE
007100180314           if statpr = 'ST00';
007200180314             // Aggiorna TESTATA
007300180314             EDST00 = wdat;
007301180314      /END-FREE
007400090701     C                   MOVEL     '784'         TA1001
007401180314      /FREE
007500180314             flatrec = EDST00;
007600180314             update RCV784;
007700180314             //
007800180314             //  Solo alla testata successiva
007900180314             if tsNRec > 0 and siDettaglio = ' '
008000180314                   and NRecPrimoDett > 0;
008100180314               //  Se non ci sono dettagli per la testata precedente occorre eliminarla
008200180314               exsr elimina_TES784;
008300180314             ENDIF;
008400180314             // resetta il flag
008500180314             clear NRecPrimoDett;
008600180314             eval siDettaglio = *blank;
008601180314      /END-FREE
008700090630     c                   z-add     NRec784       tsNRec
008800180314      //
008801180314      /FREE
008900180314           ENDIF;
009000180314           //
009100180314           //  Inizio Dettaglio
009200180314           if statpr = 'SD00';
009201180314      /END-FREE
009300090630     c                   z-add     NRec784       svNRec
009301180314      /FREE
009400180314             if NRecPrimoDett = 0;
009401180314      /END-FREE
009500090630     c                   z-add     NRec784       NRecPrimoDett
009501180314      /FREE
009600180314             ENDIF;
009700180314           ENDIF;
009800180314           //  STATUS
009900180314           IF statpr = 'SD05';
010000180314             EDSD05 = wdat;
010100180314             //*   se sono stati ATR deve eliminare il dettaglio
010200180314             if DB9011 = '1  ' or DB9011 = '3  ';
010300180314               exsr elimina_784;
010400180314             else;
010500180314               eval TIPO784 = 'S';
010600180314               eval siDettaglio  = 'S';
010700180314             ENDIF;
010800180314           endIF;
010900180314           //
011000180314           READ wftmp784;
011100180314         ENDDO;
011200180314         //*
011300180314         //  Attenzione se a fine File si deve eliminare la testata
011400180314         IF %EOF(wftmp784);
011500180314           //  Solo alla testata successiva
011600180314           if tsNRec > 0 and siDettaglio = ' ';
011700180314             //  Se non ci sono dettagli per la testata precedente occorre eliminarla
011800180314             exsr elimina_TES784;
011900180314           ENDIF;
012000180314         endIF;
012100180314         //*
012200180314       endsr;
012300180314       //----------------------------------------------------------------
012400180314       begsr elimina_784;
012500180314         //
012501180314      /END-FREE
012600090630     c                   move      *blank        esci
012700180314      //
012701180314      /FREE
012800180314         setll svNRec wftmp784;
012900180314         READ wftmp784;
013000180314         //
013100180314         dow not %EOF(wftmp784);
013101180314      /END-FREE
013200090629     c                   movel     flatrec       wdat
013300180314      //
013301180314      /FREE
013400180314           IF statpr = 'SD00' or statpr = 'ST00';
013500180314             //
013600180314             if esci =' ';
013601180314      /END-FREE
013700090630     c                   move      'S'           esci
013701180314      /FREE
013800180314             else;
013900180314               //  Riposiziona prima di uscire x il ciclo principale
013901180314      /END-FREE
014000090630     c                   z-add     NRec784       svNRec
014001180314      /FREE
014100180314               setLL svNRec wftmp784;
014200180314               leave;
014300180314             ENDIF;
014400180314             //
014500180314           endIF;
014600180314           //
014700180314           delete RCV784;
014800180314           READ wftmp784;
014900180314         ENDDO;
015000180314         //
015100180314         // è uscito dopo aver riposizionato il record per il ciclo principale
015200180314         //   che dovrà rileggere il primo record di dettaglio
015300180314         //
015400180314       endsr;
015500180314       //----------------------------------------------------------------
015600180314       begsr elimina_TES784;
015700180314         //
015800180314         setll tsNRec wftmp784;
015900180314         READ wftmp784;
016000180314         //
016100180314         // Potrei avere più testate e più dettagli quindi x sicurezza
016200180314         //   controlla che la testata da cancellare sia precedente
016300180314         dow not %EOF(wftmp784) and
016400180314               ( NRec784<NRecPrimoDett or NRecPrimoDett=0);
016401180314      /END-FREE
016500090630     c                   z-add     NRec784       svNRec
016501180314      /FREE
016600180314           delete RCV784;
016700180314           READ wftmp784;
016800180314         ENDDO;
016900180314         //
017000180314         // è uscito ADESSO riposiziona il record per il ciclo principale
017100180314         //   che dovrà rileggere il primo record di dettaglio
017200180314         setLL svNRec wftmp784;
017300180314         //
017400180314       endsr;
017500180314       //----------------------------------------------------------------
017600180314       begsr file781;
017700180314         //
017800180314         clear tsnrec;
017900180314         clear svnrec;
018000180314         clear NRecPrimoDett;
018100180314         clear siDettaglio;
018200180314         //
018300180314         READ wftmp781;
018400180314         //
018500180314         dow not %EOF(wftmp781);
018501180314      /END-FREE
018600090629     c                   movel     flatrec       wdat
018700180314      //
018800180314      // Tipo Riga Iniziale
018801180314      /FREE
018900180314           if statpr = 'ST00';
019000180314             // Aggiorna TESTATA
019100180314             EDST00 = wdat;
019101180314      /END-FREE
019200090701     C                   MOVEL     '781'         TA1001
019201180314      /FREE
019300180314             flatrec = EDST00;
019400180314             update RCV781;
019500180314             //
019600180314             //  Solo alla testata successiva
019700180314             if tsNRec > 0 and siDettaglio = ' '
019800180314                   and NRecPrimoDett > 0;
019900180314               //  Se non ci sono dettagli per la testata precedente occorre eliminarla
020000180314               exsr elimina_TES781;
020100180314             ENDIF;
020200180314             // resetta il flag
020300180314             clear NRecPrimoDett;
020400180314             eval siDettaglio = *blank;
020401180314      /END-FREE
020500090630     c                   z-add     NRec781       tsNRec
020501180314      /FREE
020600180314           ENDIF;
020700180314           //  Inizio Dettaglio
020800180314           if statpr = 'SD00';
020801180314      /END-FREE
020900090630     c                   z-add     NRec781       svNRec
020901180314      /FREE
021000180314             if NRecPrimoDett = 0;
021001180314      /END-FREE
021100090630     c                   z-add     NRec781       NRecPrimoDett
021101180314      /FREE
021200180314             ENDIF;
021300180314           ENDIF;
021400180314           //  STATUS
021500180314           IF statpr = 'SD05';
021600180314             EDSD05 = wdat;
021700180314             //*   se sono stati ATR deve eliminare il dettaglio
021800180314             if DB9011 = '1  ' or DB9011 = '3  ';
021900180314               eval TIPO781 = 'S';
022000180314               eval siDettaglio  = 'S';
022100180314             else;
022200180314               exsr elimina_781;
022300180314             ENDIF;
022400180314           endIF;
022500180314           //
022600180314           READ wftmp781;
022700180314         ENDDO;
022800180314         //
022900180314         //*
023000180314         //  Attenzione se a fine File si deve eliminare la testata
023100180314         IF %EOF(wftmp781);
023200180314           //  Solo alla testata successiva
023300180314           if tsNRec > 0 and siDettaglio = ' ';
023400180314             //  Se non ci sono dettagli per la testata precedente occorre eliminarla
023500180314             exsr elimina_TES781;
023600180314           ENDIF;
023700180314         endIF;
023800180314         //*
023900180314       endsr;
024000180314       //----------------------------------------------------------------
024100180314       begsr elimina_781;
024200180314         //
024201180314      /END-FREE
024300090630     c                   move      *blank        esci
024400180314      //
024401180314      /FREE
024500180314         setll svNRec wftmp781;
024600180314         READ wftmp781;
024700180314         //
024800180314         dow not %EOF(wftmp781);
024801180314      /END-FREE
024900090629     c                   movel     flatrec       wdat
025000180314      //
025001180314      /FREE
025100180314           IF statpr = 'SD00' or statpr = 'ST00';
025200180314             //
025300180314             if esci =' ';
025301180314      /END-FREE
025400090630     c                   move      'S'           esci
025401180314      /FREE
025500180314             else;
025600180314               //  Riposiziona prima di uscire x il ciclo principale
025601180314      /END-FREE
025700090630     c                   z-add     NRec781       svNRec
025701180314      /FREE
025800180314               setLL svNRec wftmp781;
025900180314               leave;
026000180314             ENDIF;
026100180314             //
026200180314           endIF;
026300180314           //
026400180314           delete RCV781;
026500180314           READ wftmp781;
026600180314         ENDDO;
026700180314         //
026800180314         // è uscito dopo aver riposizionato il record per il ciclo principale
026900180314         //   che dovrà rileggere il primo record di dettaglio
027000180314         //
027100180314       endsr;
027200180314       //----------------------------------------------------------------
027300180314       begsr elimina_TES781;
027400180314         //
027500180314         setll tsNRec wftmp781;
027600180314         READ wftmp781;
027700180314         //
027800180314         // Potrei avere più testate e più dettagli quindi x sicurezza
027900180314         //   controlla che la testata da cancellare sia precedente
028000180314         dow not %EOF(wftmp781) and
028100180314               ( NRec781<NRecPrimoDett or NRecPrimoDett=0);
028101180314      /END-FREE
028200090630     c                   z-add     NRec781       svNRec
028201180314      /FREE
028300180314           delete RCV781;
028400180314           READ wftmp781;
028500180314         ENDDO;
028600180314         //
028700180314         // è uscito ADESSO riposiziona il record per il ciclo principale
028800180314         //   che dovrà rileggere il primo record di dettaglio
028900180314         setLL svNRec wftmp781;
029000180314         //
029100180314       endsr;
029200180314       //----------------------------------------------------------------
029201180314      /END-FREE
