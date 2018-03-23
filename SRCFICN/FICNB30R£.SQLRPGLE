000100081003      *=====================================================================*
000200081121      *? Controllo Distinte/bolle ancora aperete x la filiale prima di                    ?
000300081121      *? procedere con la Fattur. a seguito legge 133/08 adeguamento Carbur.              ?
000400081003      *=====================================================================*
000500081121      *? Questa Routine può essere chiamata in due modi:                                  ?
000501081121      *? per filiale e senza data: si scatenano i controlli solo se siamo                 ?
000502081121      *? nel periodo di autofatturazione cioè  nel periodo espresso dalle                 ?
000600081008      *? 2 date presenti sulla tabella BLC. dalla data Protocollo alla data
000700081008      *? data di Blocco.
000701081121      *? per aut e data distinta: si scatenano i controlli a prescindere                  ?
000702081121      *? dal periodo di autofatturazione                                                  ?
000800081008      *=====================================================================*
000900040310     H DECEDIT('0,') DATEDIT(*YMD.)
001000080930      *
001100080930     FTntbe01L  IF   E           K DISK
001200080930      *
001300040128     D data_iso        s               d   datfmt(*iso)
001400040128     D data2           s               d   datfmt(*iso)
001500081003     D ficnb30ds     e ds
001600080930      *
001700080930     d dBLC          e ds
001800081001      *=====================================================================*
001900080930     C     KTbe          KLIST
002000080930     C                   KFLD                    tbeCOD
002100080930     C                   KFLD                    tbeKE1
002200081001      *=====================================================================*
002300080930     c     *entry        plist
002400081003     c                   parm                    ficnb30ds
002500081003      *
002600081003      * Campi da ritornare al chiamante:
002701081121     c                   clear                   Tipoerrore
002800081003     c                   clear                   Msg_errore
002900081003     c                   seton                                        LR
003000081003      * imposta la data del giorno
003100081003     c                   move      *date         data80            8 0
003200080930      *
003300081003      *? Se richiamato in modalità di controllo per "Conferma Valorizzazioni"             ?
003400081003      *? devono essere impostati i campi: Da Autotrasportatore ad Autotrasportatore       ?
003500081003      *? e la data distinta da controllare.                                               ?
003501081121      *? A prescindere però verifico anche per filale se siamo nel periodo                ?
003502081121      *? di autofatturazione                                                              ?
003600081003     c                   if        CODAUT_DA  > 0 and
003700081003     c                             CODAUT_A   > 0 and
003800081003     c                             DATA_DOCUM > 0
003900081003      *
004000081003     c                   exsr      CTRL_VALOR
004001081121     c                   end
004100081003      *
004200081003      *? Se richiamato per il controllo sul periodo di fatturazione sul mese precedente   ?
004300081121      *? si deve ricevere impostato solo il campo Codice Filiale per        i deve        ?
004400081121      *? eseguire il controllo generale su tutti gli AUTotrasportatori       filiale      ?
004700081003      *
004800081003     c                   exsr      CTRL_FILIALE
004900080930      *
005100080930      *
005200080930      * =====================================================================
005300080930      *  procede al controllo delle distinte
005400080930      * =====================================================================
005500081003      *? Distinte Aperte sulle Consegne (ARB)    Tipo Errore -->'C'                       ?
005600080930     c                   z-add     0             totale           15 0
005601081029     c                   if        codpdd = 'T'
005700080930     C/EXEC SQL
005800080930     C+ SELECT count(*) INTO :totale FROM
005900080930     C+  FILTRA201/fidst00f join filtra201/fnarb00f on
006000080930     C+  dstpdr = arbpdc and dstnfv=arbndc
006100080930     C+ WHERE  dstdfv between :DaData and :aData AND
006200080930     C+     dstatb=' ' and dstnpg=4 and dstFCF = ' '
006300080930     C+     and dstPDR IN
006400081120     C+ (SELECT apdPDR FROM fiapd00f
006500081029     C+  WHERE apdATB = ' ' and apdTIP = 'A'
006600080930     C+        and ApdPDR between :DaPadr and :aPadr)
006700080930     C/END-EXEC
006701081029     c                   else
006702081029     C/EXEC SQL
006703081121     C+ SELECT count(*) INTO :totale FROM FILTRA201/fidst00f join
006704081121     C+ filtra201/fnarb00f on dstpdr = arbpdc and dstnfv=arbndc WHERE
006705081121     C+ dstdfv between :DaData and :aData AND dstatb=' ' and dstnpg=4 and
006706081121     C+ dstFCF = ' ' and dstPDR IN (SELECT apdPDR FROM fiapd00f WHERE
006707081121     C+ apdATB = ' ' and apdTIP = 'A' and apdPDD <> 'S' and ApdPDR
006708081121     C+ between :DaPadr and :aPadr)
006712081029     C/END-EXEC
006713081029     c                   end
006800080930      *
006900080930      *  Esce segnalando l'errore:
007000080930     c                   if        totale > 0
007100081003     c                   movel     'C'           Tipoerrore
007200081008     c                   eval      Msg_errore = 'Ci sono Spedizioni in Consegn-
007300081121     c                             a su Distinte da Chiudere'
007500080930     c                   return
007600080930     c                   end
007700080930      * =====================================================================
007800081003      *? Distinte Ritiri (BLP) con Bolle non Partite      Tipo Errore -->'P'                   ?
007900080930     c                   z-add     0             totale           15 0
007901081029     c                   if        codpdd = 'T'
008000080930     C/EXEC SQL
008100080930     C+ SELECT count(*) INTO :totale FROM
008200080930     C+  FILTRA201/fidst00f join filtra201/fnblp00f on
008300081124     C+  dstpdr = blppdr and dstdfv=blpdrt
008400080930     C+ WHERE  dstdfv between :DaData and :aData AND
008500080930     C+     dstatb=' ' and dstnpg=4 and blpFT1= ' '
008600080930     C+     and dstPDR IN
008700081120     C+ (SELECT apdPDR FROM fiapd00f
008800081029     C+  WHERE apdATB = ' ' and apdTIP = 'A'
008900080930     C+        and ApdPDR between :DaPadr and :aPadr)
009000080930     C/END-EXEC
009001081029     c                   else
009002081029     C/EXEC SQL
009003081029     C+ SELECT count(*) INTO :totale FROM FILTRA201/fidst00f join
009004081124     C+ filtra201/fnblp00f on dstpdr = blppdr and dstdfv=blpdrt WHERE
009005081029     C+ dstdfv between :DaData and :aData AND dstatb=' ' and dstnpg=4 and
009006081120     C+ blpFT1= ' ' and dstPDR IN (SELECT apdPDR FROM fiapd00f
009007081029     C+ WHERE apdATB = ' ' and apdTIP = 'A' and apdPDD <> 'S' and ApdPDR
009008081029     C+ between :DaPadr and :aPadr)
009012081029     C/END-EXEC
009013081029     c                   end
009100080930      *
009200080930      *  Esce segnalando l'errore:
009300080930     c                   if        totale > 0
009400081003     c                   movel     'P'           Tipoerrore
009500081008     c                   eval      Msg_errore = 'Ci sono Sepdizioni Ritirat-
009600081121     c                             e non ancora Partite'
009800080930     c                   return
009900080930     c                   end
010000080930     c
010100081001      * =====================================================================
010200081003      *? Autotrasportatori senza la Massa in Anagrafica    Tipo Errore -->'A'                ?
010300081001     c                   z-add     0             totale           15 0
010301090825     c                   if        ctrmassa = 'S'
010302081029     c                   if        codpdd = 'T'
010400081001     C/EXEC SQL
010500081001     C+ SELECT count(*) INTO :totale FROM
010600081120     C+     fiapd00f
010700081029     C+  WHERE apdATB = ' ' and apdTIP = 'A'
010800081001     C+        and ApdPDR between :DaPadr and :aPadr    AND
010900081001     C+        substr(apdflr, 9, 7) in ('0000000', '       ')
011000081001     C/END-EXEC
011001081029     c                   else
011002081029     C/EXEC SQL
011003081029     C+ SELECT count(*) INTO :totale FROM
011004081120     C+     fiapd00f
011005081029     C+  WHERE apdATB = ' ' and apdTIP = 'A' and apdPDD <> 'S'
011006081029     C+        and ApdPDR between :DaPadr and :aPadr    AND
011007081029     C+        substr(apdflr, 9, 7) in ('0000000', '       ')
011008081029     C/END-EXEC
011009081029     c                   end
011100081001      *
011200081001      *  Esce segnalando l'errore:
011300081001     c                   if        totale > 0
011400081003     c                   movel     'A'           Tipoerrore
011500081001     c                   eval      Msg_errore = 'Anagr.Autotr.senz-
011600081120     c                             a Valore MASSA COMPL.KG su Filiale : ' +
011700081001     c                             %editc(CodFiliale:'X')
011800081001     c                   return
011900081001     c                   end
011901090825     c                   end
012000081001     c
012100081001      * =====================================================================
012200081003      *? Controllo Valorizzazioni Aperte         Tipo Errore -->'V'                       ?
012300081001     c                   z-add     0             totale           15 0
012301081029     c                   if        codpdd = 'T'
012400081001     C/EXEC SQL
012500081120     C+ SELECT count(*) INTO :totale FROM Fiftt00F
012600081001     C+ WHERE  fttDDC between :DaData and :aData AND
012700081001     C+     fttatb=' ' and fttFVL in (' ', 'B', 'V')
012800081001     C+     and fttPDR IN
012900081120     C+ (SELECT apdPDR FROM fiapd00f
013000081029     C+  WHERE apdATB = ' ' and apdTIP = 'A'
013100081001     C+        and ApdPDR between :DaPadr and :aPadr)
013200081001     C/END-EXEC
013201081029     c                   else
013202081029     C/EXEC SQL
013203081120     C+ SELECT count(*) INTO :totale FROM Fiftt00F
013204081029     C+ WHERE  fttDDC between :DaData and :aData AND
013205081029     C+     fttatb=' ' and fttFVL in (' ', 'B', 'V')
013206081029     C+     and fttPDR IN
013207081120     C+ (SELECT apdPDR FROM fiapd00f
013208081029     C+  WHERE apdATB = ' ' and apdTIP = 'A' and apdPDD <> 'S'
013209081029     C+        and ApdPDR between :DaPadr and :aPadr)
013210081029     C/END-EXEC
013211081029     c                   end
013300081001      *
013400081001      *  Esce segnalando l'errore:
013500081001     c                   if        totale > 0
013600081003     c                   movel     'V'           Tipoerrore
013700081001     c                   eval      Msg_errore = 'Ci sono Autotrasportatori anco-
013800081121     c                             ra da valorizzare'
014000081001     c                   return
014100081001     c                   end
014200081001     c
014300080930      * ------
014400040128      *
014500081003      *? Uscita dal programma:                                                            ?
014600080930     c                   return
014700080930      * =====================================================================
014800081003      *  Imposta i campi per controllo da Conferma Valorizzazioni
014900081003      * =====================================================================
015000081003     c     CTRL_VALOR    Begsr
015001081121      *
015500081003     c                   z-add     CODAUT_DA     DaPadr
015600081003     c                   z-add     CODAUT_A      aPadr
015701081121     c                   z-add     DATA_DOCUM    DaData            8 0
015800081121     c                   z-add     DATA_DOCUM    aData             8 0
015900081003      *
016000081003     c                   Endsr
016100081003      * =====================================================================
016200081003      *  Imposta i campi per controllo sul mese precedente la fatturazione
016300081003      * =====================================================================
016400081003     c     CTRL_FILIALE  Begsr
016500081003      *
016501081121      *
016502081121     C                   movel     'BLC'         tbeCOD
016503081121     C                   movel(p)  '1'           tbeKe1
016504081121     C     KTbe          chain     tntbe01l
016505081121     c                   if        %Found(tntbe01l)
016506081121     c                   eval      dblc = tbeuni
016507081121      * tabella non trovata : fuori dal programma
016508081121     c                   else
016509081121     c                   return
016510081121     c                   end
016511081121      * Se fuori dal range di data Protocollo o data blocco
016512081121      *  non deve eseguire il programma di controllo per filale se
016513081121      *  richiesto per aut e data
016514081121      * Se entro il range prende il mese di riferimento:
016515081121     c                   if        data80 >= §BLCDTP and
016516081121     c                             data80 <= §BLCDBL
016600081121      *  Verifico se siamo nel periodo di autofatturazione
018900081003      *
019000081003      * imposta il range dei Padroncini x i test sulla filiale
019100081003     c     CodFiliale    mult      10000         DaPadr            7 0
019200081003     c     DaPadr        add       9999          aPadr             7 0
019300081003      *
019400081003      * imposta anche il range delle date del mese precedente da analizzare
019401081121     C                   CLEAR                   DADATA
019500081121     c                   move      §BLCDTP       ADATA
019501081121     c                   else
019502081121     c                   return
019503081121     c                   end
020500081003      *
020600081003     c                   Endsr
020700081003      * =====================================================================
