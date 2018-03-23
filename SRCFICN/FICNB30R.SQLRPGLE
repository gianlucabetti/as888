000100081003      *=====================================================================*
000200081121      *? Controllo Distinte/bolle ancora aperete x la filiale prima di                    ?
000300081121      *? procedere con la Fattur. a seguito legge 133/08 adeguamento Carbur.              ?
000400081003      *=====================================================================*
000500081121      *? Questa Routine può essere chiamata in due modi:                                  ?
000600081121      *? per filiale e senza data: si scatenano i controlli solo se siamo                 ?
000700081121      *? nel periodo di autofatturazione cioè  nel periodo espresso dalle                 ?
000800081008      *? 2 date presenti sulla tabella BLC. dalla data Protocollo alla data
000900081008      *? data di Blocco.
001000081121      *? per aut e data distinta: si scatenano i controlli a prescindere                  ?
001100081121      *? dal periodo di autofatturazione                                                  ?
001200081008      *=====================================================================*
001300040310     H DECEDIT('0,') DATEDIT(*YMD.)
001400080930      *
001500080930     FTntbe01L  IF   E           K DISK
001600080930      *
001700040128     D data_iso        s               d   datfmt(*iso)
001800040128     D data2           s               d   datfmt(*iso)
001900081003     D ficnb30ds     e ds
002000080930      *
002100080930     d dBLC          e ds
002200081001      *=====================================================================*
002300080930     C     KTbe          KLIST
002400080930     C                   KFLD                    tbeCOD
002500080930     C                   KFLD                    tbeKE1
002600081001      *=====================================================================*
002700080930     c     *entry        plist
002800081003     c                   parm                    ficnb30ds
002900081003      *
003000081003      * Campi da ritornare al chiamante:
003100081121     c                   clear                   Tipoerrore
003200081003     c                   clear                   Msg_errore
003300081003     c                   seton                                        LR
003400081003      * imposta la data del giorno
003500081003     c                   move      *date         data80            8 0
003600080930      *
003700081003      *? Se richiamato in modalità di controllo per "Conferma Valorizzazioni"             ?
003800081003      *? devono essere impostati i campi: Da Autotrasportatore ad Autotrasportatore       ?
003900081003      *? e la data distinta da controllare.                                               ?
004000081121      *? A prescindere però verifico anche per filale se siamo nel periodo                ?
004100081121      *? di autofatturazione                                                              ?
004200081003     c                   if        CODAUT_DA  > 0 and
004300081003     c                             CODAUT_A   > 0 and
004400081003     c                             DATA_DOCUM > 0
004500081003      *
004600081003     c                   exsr      CTRL_VALOR
004700141105      *
004800141105     c                   else
004900081003      *
005000081003      *? Se richiamato per il controllo sul periodo di fatturazione sul mese precedente   ?
005100081121      *? si deve ricevere impostato solo il campo Codice Filiale per        i deve        ?
005200081121      *? eseguire il controllo generale su tutti gli AUTotrasportatori       filiale      ?
005300081003      *
005400081003     c                   exsr      CTRL_FILIALE
005500141105     c                   end
005600080930      *
005700080930      *
005800080930      * =====================================================================
005900080930      *  procede al controllo delle distinte
006000080930      * =====================================================================
006100081003      *? Distinte Aperte sulle Consegne (ARB)    Tipo Errore -->'C'                       ?
006200080930     c                   z-add     0             totale           15 0
006300081029     c                   if        codpdd = 'T'
006400080930     C/EXEC SQL
006500080930     C+ SELECT count(*) INTO :totale FROM
006600080930     C+  FILTRA201/fidst00f join filtra201/fnarb00f on
006700080930     C+  dstpdr = arbpdc and dstnfv=arbndc
006800080930     C+ WHERE  dstdfv between :DaData and :aData AND
006900080930     C+     dstatb=' ' and dstnpg=4 and dstFCF = ' '
007000080930     C+     and dstPDR IN
007100081120     C+ (SELECT apdPDR FROM fiapd00f
007200081029     C+  WHERE apdATB = ' ' and apdTIP = 'A'
007300080930     C+        and ApdPDR between :DaPadr and :aPadr)
007400080930     C/END-EXEC
007500081029     c                   else
007600081029     C/EXEC SQL
007700081121     C+ SELECT count(*) INTO :totale FROM FILTRA201/fidst00f join
007800081121     C+ filtra201/fnarb00f on dstpdr = arbpdc and dstnfv=arbndc WHERE
007900081121     C+ dstdfv between :DaData and :aData AND dstatb=' ' and dstnpg=4 and
008000081121     C+ dstFCF = ' ' and dstPDR IN (SELECT apdPDR FROM fiapd00f WHERE
008100081121     C+ apdATB = ' ' and apdTIP = 'A' and apdPDD <> 'S' and ApdPDR
008200081121     C+ between :DaPadr and :aPadr)
008300081029     C/END-EXEC
008400081029     c                   end
008500080930      *
008600080930      *  Esce segnalando l'errore:
008700080930     c                   if        totale > 0
008800081003     c                   movel     'C'           Tipoerrore
008900081008     c                   eval      Msg_errore = 'Ci sono Spedizioni in Consegn-
009000081121     c                             a su Distinte da Chiudere'
009100080930     c                   return
009200080930     c                   end
009300080930      * =====================================================================
009400081003      *? Distinte Ritiri (BLP) con Bolle non Partite      Tipo Errore -->'P'                   ?
009500080930     c                   z-add     0             totale           15 0
009600081029     c                   if        codpdd = 'T'
009700080930     C/EXEC SQL
009800080930     C+ SELECT count(*) INTO :totale FROM
009900080930     C+  FILTRA201/fidst00f join filtra201/fnblp00f on
010000081124     C+  dstpdr = blppdr and dstdfv=blpdrt
010100080930     C+ WHERE  dstdfv between :DaData and :aData AND
010200080930     C+     dstatb=' ' and dstnpg=4 and blpFT1= ' '
010300080930     C+     and dstPDR IN
010400081120     C+ (SELECT apdPDR FROM fiapd00f
010500081029     C+  WHERE apdATB = ' ' and apdTIP = 'A'
010600080930     C+        and ApdPDR between :DaPadr and :aPadr)
010700080930     C/END-EXEC
010800081029     c                   else
010900081029     C/EXEC SQL
011000081029     C+ SELECT count(*) INTO :totale FROM FILTRA201/fidst00f join
011100081124     C+ filtra201/fnblp00f on dstpdr = blppdr and dstdfv=blpdrt WHERE
011200081029     C+ dstdfv between :DaData and :aData AND dstatb=' ' and dstnpg=4 and
011300081120     C+ blpFT1= ' ' and dstPDR IN (SELECT apdPDR FROM fiapd00f
011400081029     C+ WHERE apdATB = ' ' and apdTIP = 'A' and apdPDD <> 'S' and ApdPDR
011500081029     C+ between :DaPadr and :aPadr)
011600081029     C/END-EXEC
011700081029     c                   end
011800080930      *
011900080930      *  Esce segnalando l'errore:
012000080930     c                   if        totale > 0
012100081003     c                   movel     'P'           Tipoerrore
012200081008     c                   eval      Msg_errore = 'Ci sono Sepdizioni Ritirat-
012300081121     c                             e non ancora Partite'
012400080930     c                   return
012500080930     c                   end
012600080930     c
012700081001      * =====================================================================
012800081003      *? Autotrasportatori senza la Massa in Anagrafica    Tipo Errore -->'A'                ?
012900081001     c                   z-add     0             totale           15 0
013000090825     c                   if        ctrmassa = 'S'
013100081029     c                   if        codpdd = 'T'
013200081001     C/EXEC SQL
013300081001     C+ SELECT count(*) INTO :totale FROM
013400081120     C+     fiapd00f
013500081029     C+  WHERE apdATB = ' ' and apdTIP = 'A'
013600081001     C+        and ApdPDR between :DaPadr and :aPadr    AND
013700081001     C+        substr(apdflr, 9, 7) in ('0000000', '       ')
013800081001     C/END-EXEC
013900081029     c                   else
014000081029     C/EXEC SQL
014100081029     C+ SELECT count(*) INTO :totale FROM
014200081120     C+     fiapd00f
014300081029     C+  WHERE apdATB = ' ' and apdTIP = 'A' and apdPDD <> 'S'
014400081029     C+        and ApdPDR between :DaPadr and :aPadr    AND
014500081029     C+        substr(apdflr, 9, 7) in ('0000000', '       ')
014600081029     C/END-EXEC
014700081029     c                   end
014800081001      *
014900081001      *  Esce segnalando l'errore:
015000081001     c                   if        totale > 0
015100081003     c                   movel     'A'           Tipoerrore
015200081001     c                   eval      Msg_errore = 'Anagr.Autotr.senz-
015300081120     c                             a Valore MASSA COMPL.KG su Filiale : ' +
015400081001     c                             %editc(CodFiliale:'X')
015500081001     c                   return
015600081001     c                   end
015700090825     c                   end
015800081001     c
015900081001      * =====================================================================
016000081003      *? Controllo Valorizzazioni Aperte         Tipo Errore -->'V'                       ?
016100081001     c                   z-add     0             totale           15 0
016200081029     c                   if        codpdd = 'T'
016300081001     C/EXEC SQL
016400081120     C+ SELECT count(*) INTO :totale FROM Fiftt00F
016500081001     C+ WHERE  fttDDC between :DaData and :aData AND
016600141105     C+     fttatb=' ' and fttFVL <> :TIPO_VALOR
016700081001     C+     and fttPDR IN
016800081120     C+ (SELECT apdPDR FROM fiapd00f
016900081029     C+  WHERE apdATB = ' ' and apdTIP = 'A'
017000081001     C+        and ApdPDR between :DaPadr and :aPadr)
017100081001     C/END-EXEC
017200081029     c                   else
017300081029     C/EXEC SQL
017400081120     C+ SELECT count(*) INTO :totale FROM Fiftt00F
017500081029     C+ WHERE  fttDDC between :DaData and :aData AND
017600141105     C+     fttatb=' ' and fttFVL <> :TIPO_VALOR
017700081029     C+     and fttPDR IN
017800081120     C+ (SELECT apdPDR FROM fiapd00f
017900081029     C+  WHERE apdATB = ' ' and apdTIP = 'A' and apdPDD <> 'S'
018000081029     C+        and ApdPDR between :DaPadr and :aPadr)
018100081029     C/END-EXEC
018200081029     c                   end
018300081001      *
018400081001      *  Esce segnalando l'errore:
018500081001     c                   if        totale > 0
018600081003     c                   movel     'V'           Tipoerrore
018700141105     c                   eval      Msg_errore = 'Distinte non correttemente val-
018800141105     c                             orizzate'
018900081001     c                   return
019000081001     c                   end
019100081001     c
019200080930      * ------
019300040128      *
019400081003      *? Uscita dal programma:                                                            ?
019500080930     c                   return
019600080930      * =====================================================================
019700081003      *  Imposta i campi per controllo da Conferma Valorizzazioni
019800081003      * =====================================================================
019900081003     c     CTRL_VALOR    Begsr
020000081121      *
020100081003     c                   z-add     CODAUT_DA     DaPadr
020200081003     c                   z-add     CODAUT_A      aPadr
020300081121     c                   z-add     DATA_DOCUM    DaData            8 0
020400081121     c                   z-add     DATA_DOCUM    aData             8 0
020500081003      *
020600081003     c                   Endsr
020700081003      * =====================================================================
020800081003      *  Imposta i campi per controllo sul mese precedente la fatturazione
020900081003      * =====================================================================
021000081003     c     CTRL_FILIALE  Begsr
021100081003      *
021200081121      *
021300081121     C                   movel     'BLC'         tbeCOD
021400081121     C                   movel(p)  '1'           tbeKe1
021500081121     C     KTbe          chain     tntbe01l
021600081121     c                   if        %Found(tntbe01l)
021700081121     c                   eval      dblc = tbeuni
021800081121      * tabella non trovata : fuori dal programma
021900081121     c                   else
022000081121     c                   return
022100081121     c                   end
022200081121      * Se fuori dal range di data Protocollo o data blocco
022300081121      *  non deve eseguire il programma di controllo per filale se
022400081121      *  richiesto per aut e data
022500081121      * Se entro il range prende il mese di riferimento:
022600081121     c                   if        data80 >= §BLCDTP and
022700081121     c                             data80 <= §BLCDBL
022800081121      *  Verifico se siamo nel periodo di autofatturazione
022900081003      *
023000081003      * imposta il range dei Padroncini x i test sulla filiale
023100081003     c     CodFiliale    mult      10000         DaPadr            7 0
023200081003     c     DaPadr        add       9999          aPadr             7 0
023300081003      *
023400081003      * imposta anche il range delle date del mese precedente da analizzare
023500081121     C                   CLEAR                   DADATA
023600081121     c                   move      §BLCDTP       ADATA
023700081121     c                   else
023800081121     c                   return
023900081121     c                   end
024000081003      *
024100081003     c                   Endsr
024200081003      * =====================================================================
