000100060614     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000200060614     H BNDDIR('QC2LE')
000300050414     H DECEDIT('0,') DATEDIT(*YMD/)
000400060609      **?************************************************************************
000500090203      *  TRASCODIFICA : SEGMENTO      -      STANDARD EDIFACT                   *
000600060612      **?************************************************************************
000700090203      * Il pgm restituisce la DS specifica del segmento con i campi esplicitati
000800060609      **?************************************************************************
000900100716     FedMAP01l  IF   E           K DISK
001000090203      *----------------------------------------------------*
001100090204     d  Segmento       s           2048
001200090204     d  esito          s              1
001300090205     d  Tipo_segmento  s              3
001400100716     d keyUNBCLI       s             35
001500100716     d keyTIPOMSG      s              6
001600100716     d keyVERSION      s              3
001700100716     d keyRELEASE      s              3
001800100716     d keyAGENCY       s              3
001900100716     d keyASSOCIA      s              6
002000940321      *----------------------------------------------------*
002100090205     D Campi_segmento  ds
002200090205     D  SKsgm_Campo                   4    DIM(50)
002300090205     D Campi_obbligat  ds
002400090205     D  SKsgm_Obbligo                 1    DIM(50)
002500090205     D Campi_AlfaNum   ds
002600090205     D  SKsgm_AlfaNum                 1    DIM(50)
002700090205     D Livello_Campi   ds
002800090205     D  SKsgm_Livello                 1s 0 DIM(50)
002900090205     D  SKsgm_Lungo    s              5s 0 DIM(50)
003000090209     D  SKsgm_Decim    s              5s 0 DIM(50)
003100090203      *----
003200090205     D Valore_campi    ds
003300090205     D  SKout_valFLD               1000    DIM(50)
003400090205     D Lung_Valori     ds
003500090205     D  SKout_Lungo                   5s 0 DIM(50)
003600090205     D Valori_Alfanum  ds
003700090205     D  SKout_AlfaNum                 1    DIM(50)
003800090205     D Valori_inErr    ds
003900090205     D  SKout_Errori                  1    DIM(50)
004000090206     D Descr_Errore    ds
004100090206     D  SKout_DesErr                 50    DIM(50)
004200090205      *
004300090205     D  SKout_Diviso   s              1    DIM(50)
004400090204     D  SKout_Decimal  s              1s 0 DIM(50)
004500090203      *----------------------------------------------------*
004600090204     D xCampi          s              3s 0
004700090203     D Sav_CATEGO      S              4
004800090209     D xByte           s              3s 0
004900090203      *-------------------
005000090203     D Tipo_error      S              1  0
005100060612      *-------------------
005200000223     D W0140           S             14  0
005300991129     D WORA            S              6  0
005400991129     D WDTGIO          S              8  0
005500991129     D DATEU           S              8  0
005600991129     D DATA_eur        S               D   DATFMT(*eur)
005700060612      * ?================================================================== */
005800060612      * ?   * Campi da tradurre con interi
005900060612     D atoi            PR            10I 0
006000060612     D                                     EXTPROC('atoi')
006100060612     D  Char                           *
006200060612     D                                     VALUE
006300060612     D                                     OPTIONS(*STRING)
006400060612      * ?   * Campi da tradurre con decimali
006500060612     D atof            PR             8F
006600060612     D                                     EXTPROC('atof')
006700060612     D  Char                           *
006800060612     D                                     VALUE
006900060612     D                                     OPTIONS(*STRING)
007000060612      * ?   *--------------------------------------------------------------*
007100060612      * ?   ( Descrizione Campi x scomposizione FLAT record )
007200060612      * ?   *--------------------------------------------------------------*
007300060612      * ?   * Campi x decodifica * (INPUT  del Record)
007400090130     D  Dati           s           2048
007500090130     D Separa_segmen   s              1
007600060612     D Decimal_Separ   s              1
007700090204     d  lung_Dati      s              5s 0
007800090203      *
007900060612      * ?   * Campi decodificati * (OUTPUT del Record)
008000060612      *       Schiere di Output / Campi di Output
008100090130     D  Alfa1000       s           1000
008200060612     d  Sk             s              3u 0
008300060612     D    NR_campi     s              3u 0
008400090130      *
008500060612     D  Campi_Record   ds
008600090130     D    Dato_sk                  1000    DIM(50)
008700060612     D  Campi_Lunghi   ds
008800110302     D    LunDato_sk                  5s 0 DIM(50)
008900060612     D Campi_Numerici  ds
009000090130     D    DatoNum_sk                  1    DIM(50)
009100060612     D Campi_Decimali  ds
009200090130     D    Decimal_sk                  3u 0 DIM(50)
009300060612      * ?* ------------------------------------------------------ *
009400060710     D Digits          C                   '0123456789'
009500090206     D ERR_inCAMPO     C                   'Nel Campo '
009600090206     D ERR_alfaNUM     C                   ' Presenti caratteri NON Numerici'
009700060612      * ?================================================================== */
009800060612      *   Ciclo principale
009900090203      * ?================================================================== */
010000090203     c     *ENTRY        PLIST
010100090203      * in
010200090205     C                   parm                    Segmento
010300100716     C                   parm                    keyUNBCLI
010400100716     C                   parm                    keyTIPOMSG
010500100716     C                   parm                    keyVERSION
010600100716     C                   parm                    keyRELEASE
010700100716     C                   parm                    keyAGENCY
010800100716     C                   parm                    keyASSOCIA
010900090203      * out
011000090205     C                   parm                    esito
011100090205     C                   parm                    Tipo_segmento
011200090205     C                   parm                    Campi_Segmento
011300090205     C                   parm                    Campi_Obbligat
011400090205     C                   parm                    Campi_AlfaNum
011500090205     C                   parm                    Livello_Campi
011600090205     C                   parm                    Valore_Campi
011700090205     C                   parm                    Lung_Valori
011800090205     C                   parm                    Valori_Alfanum
011900090205     C                   parm                    Valori_inErr
012000090206     C                   parm                    Descr_Errore
012100090203      *
012200090203      *  pulizia campi (poichè chiude in RT):
012300090203     C                   clear                   Esito
012400090203     C                   clear                   Dati
012500060614      *
012600090203      *  Imposta il Tipo segmento ricevuto dal segmento stesso
012700090203     c                   eval      Tipo_segmento = %subst(Segmento:1:3)
012800090203      *
012900090203      *  se non trova la descrizione sul file segmenti segnala ed esce
013000090203      *   subito dal programma dando l'errore.
013100090203      * ?              /*---------------------- */
013200100716     c     kMAP_POR      SETLL     edMAP01L
013300100716     c                   if        not %Equal(edMAP01L)
013400090205     C                   eval      esito = 'E'
013500090203     c                   goto      FINE
013600090203     c                   end
013700090203      **
013800090203      **  Prepara le schiere per recepire i valori dei campi
013900090203      * ?              /*---------------------- */
014000090203     c                   exsr      Carica_Schiere
014100090203      * ?              /*---------------------- */
014200090204      **
014300090204      **  Divide prima i MacroCampi separati da (+)
014400090204      *                /*---------------------- */
014500090204     c                   exsr      First_Level
014600090204      *                /*---------------------- */
014700090204      **
014800090204      ** Deve scendere ad un secondo Livello
014900090204      **   e  deve fare delle considerazioni sui campi seprati da (:)
015000090204      *                /*---------------------- */
015100090204     c                   exsr      Second_Level
015200090204      *                /*---------------------- */
015300090203      *
015400090206      *  se il punto interrogativo nei campi precede un carattere
015500090206      *   particolare il punto interrogativo deve essere sostituito con Blank
015600090206      *                /*---------------------- */
015700090206     c                   exsr      Punto_interr
015800090206      *                /*---------------------- */
015900090206      *
016000090209      * Elimina le virgole o i punti se i campi devono riportare solo interi
016100090209      *                /*---------------------- */
016200090209     c                   exsr      Senza_Decimali
016300090209      *                /*---------------------- */
016400090209      *
016500090206      *                /*---------------------- */
016600090206     c                   exsr      Errori
016700090206      *                /*---------------------- */
016800090206      *
016900090203     c     FINE          tag
017000090212     c                   SETON                                        RT
017100090203      * ?------------------------------------------------------------------ */
017200090203      *?      Suddivide i campi della riga con carattere divisorio (;)
017300090203      * ?------------------------------------------------------------------ */
017400090203     c     Split_Rec     Begsr
017500090203      **
017600090203     c                   clear                   Nr_campi
017700090203     c                   clear                   Campi_Record
017800090203     c                   clear                   Campi_Lunghi
017900090203     c                   clear                   Campi_Numerici
018000090203     c                   clear                   Campi_Decimali
018100090203     c                   movel     '.'           Decimal_Separ
018200090203     c                   call      'TRTCT00R'
018300090203      * ?     Input
018400090203     c                   parm                    Separa_segmen
018500090203     c                   parm                    Decimal_Separ
018600090203     c                   parm                    dati
018700090203      * ?     Output  su schiere di 50  elementi
018800090203     c                   parm                    Nr_campi
018900090203     c                   parm                    Campi_Record                   * i dati
019000090203     c                   parm                    Campi_Lunghi                   * lunghezza dati
019100090203     c                   parm                    Campi_Numerici                 * se numerici
019200090203     c                   parm                    Campi_Decimali                 * quanti decimali
019300090203      **
019400090203     c                   Endsr
019500060612      * ?------------------------------------------------------------------ */
019600090203      *?      Inizializza le schiere per incasellare i dati riceuti posizionalmente
019700060612      * ?------------------------------------------------------------------ */
019800090203     c     Carica_SchiereBegsr
019900090203      **
020000090203      **  deve impostare i valori decodificati nelle schiere in modo posizionale.
020100090203      **    se presenti dei campi con divisore (:) occorrerà eseguire un ulteriore
020200090203      **     split dei campi aggregati dopo il primo giro.
020300090203     c                   clear                   SKsgm_Campo
020400090203     c                   clear                   SKsgm_Livello
020500090203     c                   clear                   SKsgm_Obbligo
020600090203     c                   clear                   SKsgm_AlfaNum
020700090203     c                   clear                   SKsgm_Lungo
020800090209     c                   clear                   SKsgm_Decim
020900090203      **
021000090205     c                   clear                   SKout_ValFld
021100090203     c                   clear                   SKout_AlfaNum
021200090203     c                   clear                   SKout_Lungo
021300090203     c                   clear                   SKout_Decimal
021400090203     c                   clear                   SKout_Errori
021500090206     c                   clear                   SKout_DesErr
021600090205     c                   clear                   SKout_Diviso
021700090203      **
021800090203     c                   clear                   sav_CATEGO
021900090203     c                   clear                   sgm               3 0
022000090203     c                   clear                   quanti_1          3 0
022100090203     c                   clear                   quanti_2          3 0
022200090203     c                   clear                   quanti            3 0
022300090204     c                   clear                   xquanti           3 0
022400090203      **
022500100716     c     kMAP_POR      reade     edMAP01L
022600100716     c                   dow       not %EoF(edMAP01L)
022700090203      **
022800090203      **  Esclude la testata che descrive il segmento
022900090203      **   ed incrementa numeratore di schiera
023000100716     c                   if        MAPSEQUEN > 0
023100090203     c                   add       1             sgm
023200090203      **
023300090203      **  imposta o un campo o la DS di un gruppo di campi
023400100716     c                   if        MAPNCAMPO = *blank and MAPCATEGO <> *blank or
023500100716     c                             MAPNCAMPO <> *blank and MAPCATEGO = *blank
023600090203      *
023700100716     c                   eval      SKsgm_Campo(sgm)   = MAPCATEGO
023800100716     c                   eval      Sav_CATEGO         = MAPCATEGO
023900090203      *
024000090203     c                   if        SKsgm_Campo(sgm) = *blank
024100100716     c                   eval      SKsgm_Campo(sgm) = MAPNCAMPO
024200090203     c                   End
024300090203      *
024400090203     c                   eval      SKsgm_Livello(sgm) = 1
024500090203     c                   add       1             quanti_1
024600100716     c                   eval      SKsgm_Obbligo(sgm) = MAPOBBLIG
024700100716     c                   eval      SKsgm_AlfaNum(sgm) = MAPTIPOD
024800100716     c                   eval      SKsgm_Lungo(sgm)   = MAPLUNGH
024900100716     c                   eval      SKsgm_Decim(sgm)   = MAPDecim
025000090203      *
025100100716     c                   ElseIF    MAPNCAMPO <> *blank and MAPCATEGO <> *blank
025200100716     c                             and MAPCATEGO = sav_CATEGO
025300090203      *
025400100716     c                   eval      SKsgm_Campo(sgm)   = MAPNCAMPO
025500090203     c                   eval      SKsgm_Livello(sgm) = 2
025600090203     c                   add       1             quanti_2
025700100716     c                   eval      SKsgm_Obbligo(sgm) = MAPOBBLIG
025800100716     c                   eval      SKsgm_AlfaNum(sgm) = MAPTIPOD
025900100716     c                   eval      SKsgm_Lungo(sgm)   = MAPLUNGH
026000100716     c                   eval      SKsgm_Decim(sgm)   = MAPDecim
026100090203      *
026200090203     c                   End
026300090203      **
026400090203     c                   End
026500090203      **
026600100716     c     kMAP_POR      reade     edMAP01L
026700090203     c                   EndDo
026800090204      **
026900090204      ** memorizza quanti campi deve considerare x le schiere seguenti
027000090204     c                   z-add     sgm           xCampi
027100090203      **
027200090203     c                   Endsr
027300090203      * ?------------------------------------------------------------------ */
027400090203      *?      imposta i campi sulle schiere
027500090203      * ?------------------------------------------------------------------ */
027600090204     c     First_Level   Begsr
027700090203      **
027800090204      *   Sposta il record a tracciato libero sul campo di lavoro: DATI
027900090204     C                   eval      Dati = Segmento
028000090204     c                   movel     '+'           Separa_segmen
028100090204      *                /*---------------------- */
028200090204     c                   exsr      Split_Rec
028300090204      *                /*---------------------- */
028400090204      **
028500090204      **  ha diviso il segmento in campi che come primo livello possono essere
028600090204      **   anche insieme di sottocampi il Tipo dati delle DS non è né "A" né "N".
028700090204     c                   z-add     quanti_1      quanti
028800090204      **
028900060612      **   Controlli Formali sui campi: Obbligatorietà e controllo Numerico
029000090204     c                   clear                   position          3 0
029100090204     c                   z-add     0             DaQui
029200090203      **
029300090203      * ? 1° campo :  è il tipo SEGMENTO che non viene riportato nelle schiere né nella DS
029400090203     C                   z-add     1             sk
029500090203      * ?-----> Errore
029600090203     c                   If        LunDato_sk(sk) =  0
029700090205     c                   eval      esito = 'E'
029800090205     c                   Else
029900090205     c                   eval      tipo_segmento = %subst(Dato_sk(sk):1:+
030000090205     c                                              LunDato_sk(sk))
030100090205     c                   End
030200090203      **
030300090203      * ? Carica le schiere del Livello passato se ci sono dei dati
030400090203      **     Trova gli elementi di schiera dove impostare i dati
030500090204     c                   do        quanti
030600090203      **
030700090205      * cerca il primo disponibile del Livello 1 saltando
030800090205      *  il primo che contiene il Tipo Segmento precedentemente rilevato
030900090205      **
031000090205     c                   add       1             DaQui
031100090205     c     daQui         do        xCampi        sgm
031200090205     c                   if        SKsgm_Livello(sgm) = 1
031300090205     c                   z-add     sgm           daQui             3 0
031400090205     c                   leave
031500090205     c                   end
031600090205     c                   enddo
031700090203      **
031800090203     C                   add       1             sk
031900090203     c                   If        LunDato_sk(sk) >  0
032000090205     c                   eval      SKout_ValFld(DaQui) = %subst(Dato_sk(sk):1:+
032100060612     c                                              LunDato_sk(sk))
032200090205     c                   eval      SKout_Lungo(DaQui)   = LunDato_sk(sk)
032300090205     c                   if        DatoNum_sk(sk) = 'N'
032400090205     c                   eval      SKout_AlfaNum(DaQui) = 'A'
032500090205     c                   else
032600090205     c                   eval      SKout_AlfaNum(DaQui) = 'N'
032700090205     c                   end
032800090205     c                   eval      position = %scan(':':SKout_ValFld(DaQui))
032900090204      *
033000090204     c                   if        position > 0
033100090204     c                   eval      SKout_Diviso(DaQui) = ':'
033200060612     c                   End
033300090204     c                   End
033400060612      **
033500090203     c                   EndDO
033600060612      **
033700060612     c                   Endsr
033800090204      *  ------------------------------------------------------------------ */
033900090204      *       Deve scomporre le DS nei sottocampi
034000090204      *  ------------------------------------------------------------------ */
034100090204     c     Second_Level  Begsr
034200090204      **
034300090204     c                   z-add     quanti_2      quanti
034400090204     c                   z-add     1             sgm_salva         3 0
034500090204      **
034600090204      * ?  Cicla x decodificare  i sottocampi
034700090204     c                   DOW       sgm_salva <= 50
034800090204      **
034900090204     c                   exsr      Cerca_DSLVL_1
035000090204      **
035100090204      **   se deve uscire da tutto  perchè è arrivato in fondo
035200090204      **     forza l'uscita
035300090204     c                   if        Finito = 'S'
035400090204     c                   Leave
035500090204     c                   end
035600090204      **
035700090204      * ?  se il campo ha dei sotto campi
035800090204     c                   if        SKout_Diviso(sgm) = ':'
035900090204      * ?  con  Diviso:
036000090204      **    deve splittare nuovamente i campi
036100090205     C                   eval      Dati = SKout_ValFld(sgm)
036200090204     c                   movel     ':'           Separa_segmen
036300090204      *                /*---------------------- */
036400090204     c                   exsr      Split_Rec
036500090204      *                /*---------------------- */
036600090204      **
036700090204      ** imposta i campi di secondo livello relativi
036800090204      **  alla DS precedentemente suddivisa
036900090204     c                   do        Nr_Campi      xx                3 0
037000090204      *
037100090204     c                   add       1             sgm_salva
037200090205      *
037300090204     c                   if        SKsgm_Livello(sgm_salva) = 2
037400090205     c                   eval      SKout_ValFld(sgm_salva)  = Dato_sk(xx)
037500090205     c                   eval      SKout_Lungo(sgm_salva)   = LunDato_sk(xx)
037600090205     c                   if        DatoNum_sk(xx) = 'N'
037700090205     c                   eval      SKout_AlfaNum(sgm_salva) = 'A'
037800090205     c                   else
037900090205     c                   eval      SKout_AlfaNum(sgm_salva) = 'N'
038000090205     c                   end
038100090204     c                   else
038200090204     c                   Leave
038300090204     c                   end
038400090204      *
038500090204     c                   EndDo
038600090204      **
038700090204     c                   Else
038800090204      * ? senza Diviso:
038900090204      **    deve prendere la Variabile ed impostare il campo successivo
039000090204     c                   add       1             sgm_salva
039100090204     c                   if        SKsgm_Livello(sgm_salva) = 2
039200090205     c                   eval      SKout_ValFld(sgm_salva) =
039300090205     c                                       SKout_ValFld(sgm_salva -1)
039400090205     c                   eval      SKout_Lungo(sgm_salva) =
039500090205     c                                        SKout_Lungo(sgm_salva -1)
039600090205     c                   if        DatoNum_sk(sgm_salva -1) = 'N'
039700090205     c                   eval      SKout_AlfaNum(sgm_salva) = 'A'
039800090205     c                   else
039900090205     c                   eval      SKout_AlfaNum(sgm_salva) = 'N'
040000090205     c                   end
040100090205     c
040200090204     c                   end
040300090204      **
040400090204     c                   End
040500090204      **
040600090204     c                   EndDO
040700090204      **
040800090204     c                   Endsr
040900090204      *-------------------------------------------------------------------------
041000090204      *     Cerca se ci sono delle DS a livello 1  da riportare sui campi seguenti
041100090204      *  ------------------------------------------------------------------ */    §
041200090204     c     Cerca_DSLVL_1 Begsr
041300090204      **
041400090204     c                   clear                   Finito            1
041500090204      **
041600090204      **  Si rilegge le schiere pre-caricate al Primo Livello di decodifica segmento
041700090204      **   e deve andare ad inputare i campi definiti di secondo livello come parti
041800090204      **    dei campi DS di primo Livello
041900090204     c     sgm_salva     Do        50            sgm
042000090204      *
042100090204      *  esce forzatamente quando ha finito tutto quello che doveva splittare
042200090204     c                   if        SKsgm_Livello(sgm) = 0
042300090204     c                   move      'S'           Finito
042400090204     c                   leave
042500090204     c                   end
042600090204      *
042700090204     c                   if        SKsgm_Livello(sgm)  =  1  and
042800090204     c                             SKsgm_AlfaNum(sgm) <> 'A' and
042900090204     c                             SKsgm_AlfaNum(sgm) <> 'N'
043000090204      *
043100090204      * si tratta di una DS che potenzialmente può contenere più campi.
043200090204      *  Come capirlo ?!
043300090204      *     Nella schiera SKout_diviso se c'è un segno di (:) allora ci sono più campi
043400090204      *   altrimenti esiste un solo valore che deve essere inserito nel primo campo
043500090204      *   successivo all'elemento appena trovato della schiera.
043600090204     c                   eval      sgm_salva = sgm
043700090204     c                   leave
043800090204      *
043900090204     c                   end
044000090204      *
044100090204     c                   EndDo
044200090204      **
044300090204     c                   Endsr
044400060614      * ?------------------------------------------------------------------ */
044500090206      *  Elimina il punto interrogativo dove serve a far leggere come
044600090206      *   carattere normale un carattere particolare x la gestione del segmento
044700090206      * ?------------------------------------------------------------------ */
044800090206     C     Punto_interr  BEGSR
044900090206      **
045000090206     c                   do        50            sgm
045100090206     c                   clear                   position
045200090206     c                   eval      position = %scan('?':SKout_ValFld(sgm):1)
045300101109      **
045400101109     c     Altro_Puntint tag
045500101109      **
045600090206     c                   if        position > 0
045700090206      * se seguito da (' + :)
045800090206     c                   if        %subst(SKout_ValFld(sgm):position+1:1) = ''''
045900090206     c                                 or
046000090206     c                             %subst(SKout_ValFld(sgm):position+1:1) = '+'
046100090206     c                                 or
046200090206     c                             %subst(SKout_ValFld(sgm):position+1:1) = ':'
046300101109     c                                 or
046400101109     c                             %subst(SKout_ValFld(sgm):position+1:1) = '.'
046500101109     c                                 or
046600101109     c                             %subst(SKout_ValFld(sgm):position+1:1) = '?'
046700101109      *
046800101109      * sostituisce il (?) con il carattere speciale
046900101109      *  e poi abblenca dove stava prima
047000101109     c************       eval      %subst(SKout_ValFld(sgm):position:1) =
047100101109     c************                 %subst(SKout_ValFld(sgm):position+1:1)
047200101109     c************       eval      %subst(SKout_ValFld(sgm):position+1:1) = ' '
047300101109      * e
047400101109      ** invece :
047500101109      **** Deve spostare in avanti di 1 tutti i caratteri alla destra del (?)
047600101109     c                   eval      %subst(SKout_ValFld(sgm):position:
047700101109     c                             %len(SKout_ValFld(sgm)) - position)
047800101109     c                                =
047900101109     c                             %subst(SKout_ValFld(sgm):position + 1 :
048000101109     c                             %len(SKout_ValFld(sgm)) - position)
048100101109      * e
048200101109      * l'ultimo carattere viene abblencato
048300101109     c                   eval      %subst(SKout_ValFld(sgm):
048400101109     c                             %len(SKout_ValFld(sgm)):1) = ' '
048500101109      ******
048600101109     c                   clear                   newPosition       3 0
048700101109      ** poi deve cercare se c'è ancora un altro (?) da sostituire da lì in avanti
048800101109     c                   eval      NewPosition = %scan('?':SKout_ValFld(sgm):
048900101109     c                             position)
049000101109     c                   if        NewPosition > 0
049100101109      * se lo ha trovato riesegue queste operazioni da quel punto in avanti
049200101109     c                   z-add     NewPosition   Position
049300101109     c                   goto      Altro_Puntint
049400101109     c                   End
049500101109      ******
049600090206     c                   End
049700090206     c                   End
049800101109      **
049900101109      **
050000090206     c                   Enddo
050100090206      **
050200090206     c                   Endsr
050300090206      * ?------------------------------------------------------------------ */
050400090209      *  Toglie virgole o punti da campi numerici senza decimali
050500090206      * ?------------------------------------------------------------------ */
050600090209     C     Senza_DecimaliBEGSR
050700090206      **
050800090209      **
050900090209      **  Si rilegge le schiere e sui campi numerici valuta se presenti virgole
051000090209      **   o punti per eliminarli
051100090209     c                   Do        50            sgm
051200090209      *
051300090209      *  esce forzatamente quando ha finito tutto quello che doveva splittare
051400090209     c                   if        SKsgm_Livello(sgm) = 0
051500090209     c                   move      'S'           Finito
051600090209     c                   leave
051700090209     c                   end
051800090209      *
051900090209     c                   if        SKsgm_Livello(sgm)  =  2  and
052000090209     c                             SKsgm_AlfaNum(sgm)  = 'N'
052100090209      *
052200090209     c                   z-add     0             position
052300090209      * prova con il punto
052400090209     c                   eval      position = %scan('.':SKout_ValFld(sgm):1)
052500090209      *
052600090209     c                   if        position = 0
052700090209      * prova con la virgola
052800090209     c                   eval      position = %scan(',':SKout_ValFld(sgm):1)
052900090209     c                   end
053000090209      *
053100090616      *  Se ha trovato da sostituire il Separatore Decimale del campo
053200090616     c                   clear                   conta_decimali    3 0
053300090616      *
053400101109      *  Se è stato passato un campo numerico vuoto occorre intercettarlo e riempirlo
053500101109      *   con zeri per NON far rompere il programma.
053600101109     c                   IF        SKsgm_Lungo(sgm)    > 0   and
053700101109     c                             SKout_ValFld(sgm)   = *blank
053800101109      *
053900101109     c                   eval      SKout_ValFld(sgm) = *all'0'
054000101109     c                   eval      SKout_Lungo(sgm)  = SKsgm_Lungo(sgm)
054100101109     c                   eval      SKout_AlfaNum(sgm)= 'N'
054200101109      *
054300101109     c                   end
054400101109      *
054500101109      * quindi rileva la posizione
054600090209     c                   if        position > 0
054700101109      *
054800090209     c                   eval      xByte = SKsgm_Lungo(sgm)
054900090209      *
055000090616      *  Dal Separatore in avanti
055100090209     c     position      Do        xByte         xx                3 0
055200090616      *
055300090616      *  si deve controllare se vengono passati tutti i decimali che ci si aspettava
055400090616     c                   if        conta_decimali = SKsgm_Decim(sgm) and
055500090616     c                                SKsgm_Decim(sgm) > 0
055600101109      **
055700101109      **  prima di uscire deve comunque pulire i bytes rimanenti sulla destra
055800101109     c     xx            Do        xByte         yy                3 0
055900101109     c                   eval      %subst(SKout_ValFld(sgm):yy:1) = ' '
056000101109     c                   end
056100101109      **
056200090616     c                   leave
056300090616     c                   end
056400090616      *
056500090616      * Se Blank deve impostarlo a (0) come decimale valido
056600090616     c                   if        %subst(SKout_ValFld(sgm):xx+1:1) = ' '
056700090616     c                   eval      %subst(SKout_ValFld(sgm):xx:1) = '0'
056800090616     c                   else
056900090616      *
057000090616      * sposta in avanti di 1 posizione il carattere seguente
057100090209     c                   eval      %subst(SKout_ValFld(sgm):xx:1) =
057200090209     c                             %subst(SKout_ValFld(sgm):xx+1:1)
057300090630      * e mette blank al suo posto
057400101109     c***************    eval      %subst(SKout_ValFld(sgm):xx+1:1) = *blank
057500090616     c                   end
057600090616      *
057700090616     c                   add       1             conta_decimali
057800090616      *
057900090209     c                   endDo
058000090616      *
058100090616      *  Reimposta la giusta lunghezza del campo
058200090616      *   aggiunge i decimali e toglie (1) equivale al separatore decimale.
058300090616     c                   eval      SKout_Lungo(sgm) =
058400090616     c                               SKout_Lungo(sgm) + SKsgm_Decim(sgm) - 1
058500090209      *
058600090616     c                   else
058700090616      *
058800090616      *  Se il campo avrebbe dovuto avere dei decimali e hanno inviato un campo
058900090616      * con solo degli interi occorre aggiungere tanti zeri quanti sono i decimali.
059000090616     c                   if           SKsgm_Decim(sgm) > 0
059100090616      *
059200090616     c                   eval      conta_decimali = SKsgm_Decim(sgm)
059300090616     c                   eval      xx = SKout_Lungo(sgm) + 1
059400090616      *
059500090616     c                   do        conta_decimali
059600090616     c                   eval      %subst(SKout_ValFld(sgm):xx:1) = '0'
059700090616     c                   eval      xx = xx + 1
059800090616     c                   enddo
059900090616      *
060000090616      *  Reimposta la giusta lunghezza del campo
060100090616     c                   eval      SKout_Lungo(sgm) =
060200090616     c                               SKout_Lungo(sgm) + SKsgm_Decim(sgm)
060300090616      *
060400090616     c                   end
060500090616     c                   end
060600090209      *
060700090209     c                   end
060800090209      *
060900090209     c                   EndDo
061000090209      **
061100090209      **
061200090209     c                   Endsr
061300090209      * ?------------------------------------------------------------------ */
061400090209      *  Controlla eventuali Errori
061500090209      * ?------------------------------------------------------------------ */
061600090209     C     Errori        BEGSR
061700090209      **
061800090206      **  Se obbligatorio ='S'
061900090206      **     altrimenti
062000090206      **  Se opzionale ='N'
062100090206     c                   do        50            sgm
062200090206     c                   if        SKsgm_Obbligo(sgm) = 'C'
062300090206     c                   eval         SKsgm_Obbligo(sgm) = 'N'
062400090206     c                   elseIf    SKsgm_Obbligo(sgm) = 'M'
062500090206     c                   eval         SKsgm_Obbligo(sgm) = 'S'
062600090206     c                   end
062700090206     c                   enddo
062800090206      **
062900090206     c                   do        50            sgm
063000090206     c                   if        SKsgm_AlfaNum(sgm) = 'N' and
063100090206     c                             SKout_AlfaNum(sgm) <>'N' and
063200090206     c                             SKout_Lungo(sgm) > 0
063300090206     c                   eval       SKout_Errori(sgm) = 'E'
063400090206     c                   eval      SKout_DesErr(sgm) = ERR_inCAMPO +
063500090206     c                             SKsgm_Campo(sgm) + ERR_AlfaNUM
063600090206     c                   eval      esito = 'E'
063700090206     c                   end
063800090206     c                   Enddo
063900090206      **
064000090206     c                   Endsr
064100090206      * ?------------------------------------------------------------------ */
064200090206      *   DEFINIZIONE CHIAVI                               *
064300090206      *----------------------------------------------------------------
064400090206     C     *INZSR        BEGSR
064500090206      *------------------
064600971216      * Recupero data e ora
064700971216     C                   TIME                    WORA
064800991124     C                   TIME                    W0140
064900991124      * UDATE IN GGMMAAAA
065000991124     C                   MOVE      W0140         WDTGIO
065100991124      * UDATE IN AAAAMMGG
065200991124     C     *eur          MOVEL     WDTGIO        DATA_eur
065300991124     C     *iso          MOVEL     DATA_eur      dateu
065400050414      *
065500100716     c     kMAP_POR      klist
065600100716     c                   kfld                    keyUNBCLI
065700100716     c                   kfld                    keyTIPOMSG
065800100716     c                   kfld                    keyVERSION
065900100716     c                   kfld                    keyRELEASE
066000100716     c                   kfld                    keyAGENCY
066100100716     c                   kfld                    keyASSOCIA
066200090605     c                   kfld                    Tipo_segmento
066300090605      *
066400991124     C                   ENDSR
066500060621      * ?------------------------------------------------------------------ */
066600060621      *?      X non bloccare in nessun caso il traduttore CLIENTI
066700060621      * ?------------------------------------------------------------------ */
066800060621     C     *pssr         BEGSR
066900060621     C
067000090205     C                   eval      esito = 'E'
067100060621     C                   ENDSR     '*CANCL'
067200060621     C
067300090204      *  ------------------------------------------------------------------ */
