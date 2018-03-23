000100060614     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000200060614     H BNDDIR('QC2LE')
000300050414     H DECEDIT('0,') DATEDIT(*YMD/)
000400060609      **?************************************************************************
000500090203      *  TRASCODIFICA : SEGMENTO      -      STANDARD EDIFACT                   *
000600060612      **?************************************************************************
000700090203      * Il pgm restituisce la DS specifica del segmento con i campi esplicitati
000800060609      **?************************************************************************
000900100716     FEDMAP01l  IF   E           K DISK
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
012200100716     c     kMAP          klist
012300100716     c                   kfld                    keyUNBCLI
012400100716     c                   kfld                    keyTIPOMSG
012500100716     c                   kfld                    keyVERSION
012600100716     c                   kfld                    keyRELEASE
012700100716     c                   kfld                    keyAGENCY
012800100716     c                   kfld                    keyASSOCIA
012900100716     c                   kfld                    Tipo_segmento
013000100716      *
013100090203      *  pulizia campi (poichè chiude in RT):
013200090203     C                   clear                   Esito
013300090203     C                   clear                   Dati
013400060614      *
013500090203      *  Imposta il Tipo segmento ricevuto dal segmento stesso
013600090203     c                   eval      Tipo_segmento = %subst(Segmento:1:3)
013700090203      *
013800090203      *  se non trova la descrizione sul file segmenti segnala ed esce
013900090203      *   subito dal programma dando l'errore.
014000090203      * ?              /*---------------------- */
014100110530     c     prova_senza   tag
014200100716     c     kMAP          SETLL     EDMAP01L
014300100716     c                   if        not %Equal(EDMAP01L)
014400110530      **  Prima tenta con l'UNB completo come eccezione, poi, se non riesce, tenta con
014500110530      **  l'UNB parziale dei soli primi 31 caratteri con il qualificatore vuoto e,
014600110530      **  se non riesce ancora allora ritenta genericamente senza UNB.
014700110530      **   Se dopo questi tentativi,non riesce allora esce con esito ("E") di errore.
014800110530     c                   if         keyUNBCLI <> ' ' and
014900110530     c                             %subst(keyUNBCLI:32:4)<> *blank
015000110530      ** senza qualificatore
015100110530     c                   move      '    '        keyUNBCLI
015200110530     c                   goto      prova_senza
015300110530      **
015400110530     c                   elseIf     keyUNBCLI <> ' '
015500110530      ** senza l'UNB
015600110530     c                   clear                   keyUNBCLI
015700110530     c                   goto      prova_senza
015800110530      **
015900110530     c                   end
016000110530      **
016100090205     C                   eval      esito = 'E'
016200090203     c                   goto      FINE
016300090203     c                   end
016400090203      **
016500090203      **  Prepara le schiere per recepire i valori dei campi
016600090203      * ?              /*---------------------- */
016700090203     c                   exsr      Carica_Schiere
016800090203      * ?              /*---------------------- */
016900090204      **
017000090204      **  Divide prima i MacroCampi separati da (+)
017100090204      *                /*---------------------- */
017200090204     c                   exsr      First_Level
017300090204      *                /*---------------------- */
017400090204      **
017500090204      ** Deve scendere ad un secondo Livello
017600090204      **   e  deve fare delle considerazioni sui campi seprati da (:)
017700090204      *                /*---------------------- */
017800090204     c                   exsr      Second_Level
017900090204      *                /*---------------------- */
018000090203      *
018100090206      *  se il punto interrogativo nei campi precede un carattere
018200090206      *   particolare il punto interrogativo deve essere sostituito con Blank
018300090206      *                /*---------------------- */
018400090206     c                   exsr      Punto_interr
018500090206      *                /*---------------------- */
018600090206      *
018700090209      * Elimina le virgole o i punti se i campi devono riportare solo interi
018800090209      *                /*---------------------- */
018900090209     c                   exsr      Senza_Decimali
019000090209      *                /*---------------------- */
019100090209      *
019200090206      *                /*---------------------- */
019300090206     c                   exsr      Errori
019400090206      *                /*---------------------- */
019500090206      *
019600090203     c     FINE          tag
019700090212     c                   SETON                                        RT
019800090203      * ?------------------------------------------------------------------ */
019900090203      *?      Suddivide i campi della riga con carattere divisorio (;)
020000090203      * ?------------------------------------------------------------------ */
020100090203     c     Split_Rec     Begsr
020200090203      **
020300090203     c                   clear                   Nr_campi
020400090203     c                   clear                   Campi_Record
020500090203     c                   clear                   Campi_Lunghi
020600090203     c                   clear                   Campi_Numerici
020700090203     c                   clear                   Campi_Decimali
020800090203     c                   movel     '.'           Decimal_Separ
020900090203     c                   call      'TRTCT00R'
021000090203      * ?     Input
021100090203     c                   parm                    Separa_segmen
021200090203     c                   parm                    Decimal_Separ
021300090203     c                   parm                    dati
021400090203      * ?     Output  su schiere di 50  elementi
021500090203     c                   parm                    Nr_campi
021600090203     c                   parm                    Campi_Record                   * i dati
021700090203     c                   parm                    Campi_Lunghi                   * lunghezza dati
021800090203     c                   parm                    Campi_Numerici                 * se numerici
021900090203     c                   parm                    Campi_Decimali                 * quanti decimali
022000110621     c                   parm                    esito
022100090203      **
022200090203     c                   Endsr
022300060612      * ?------------------------------------------------------------------ */
022400090203      *?      Inizializza le schiere per incasellare i dati riceuti posizionalmente
022500060612      * ?------------------------------------------------------------------ */
022600090203     c     Carica_SchiereBegsr
022700090203      **
022800090203      **  deve impostare i valori decodificati nelle schiere in modo posizionale.
022900090203      **    se presenti dei campi con divisore (:) occorrerà eseguire un ulteriore
023000090203      **     split dei campi aggregati dopo il primo giro.
023100090203     c                   clear                   SKsgm_Campo
023200090203     c                   clear                   SKsgm_Livello
023300090203     c                   clear                   SKsgm_Obbligo
023400090203     c                   clear                   SKsgm_AlfaNum
023500090203     c                   clear                   SKsgm_Lungo
023600090209     c                   clear                   SKsgm_Decim
023700090203      **
023800090205     c                   clear                   SKout_ValFld
023900090203     c                   clear                   SKout_AlfaNum
024000090203     c                   clear                   SKout_Lungo
024100090203     c                   clear                   SKout_Decimal
024200090203     c                   clear                   SKout_Errori
024300090206     c                   clear                   SKout_DesErr
024400090205     c                   clear                   SKout_Diviso
024500090203      **
024600090203     c                   clear                   sav_CATEGO
024700090203     c                   clear                   sgm               3 0
024800090203     c                   clear                   quanti_1          3 0
024900090203     c                   clear                   quanti_2          3 0
025000090203     c                   clear                   quanti            3 0
025100090204     c                   clear                   xquanti           3 0
025200090203      **
025300100716     c     kMAP          reade     EDMAP01L
025400100716     c                   dow       not %EoF(EDMAP01L)
025500090203      **
025600090203      **  Esclude la testata che descrive il segmento
025700090203      **   ed incrementa numeratore di schiera
025800100716     c                   if        MAPSEQUEN > 0
025900090203     c                   add       1             sgm
026000090203      **
026100090203      **  imposta o un campo o la DS di un gruppo di campi
026200100716     c                   if        MAPNCAMPO = *blank and MAPCATEGO <> *blank or
026300100716     c                             MAPNCAMPO <> *blank and MAPCATEGO = *blank
026400090203      *
026500100716     c                   eval      SKsgm_Campo(sgm)   = MAPCATEGO
026600100716     c                   eval      Sav_CATEGO         = MAPCATEGO
026700090203      *
026800090203     c                   if        SKsgm_Campo(sgm) = *blank
026900100716     c                   eval      SKsgm_Campo(sgm) = MAPNCAMPO
027000090203     c                   End
027100090203      *
027200090203     c                   eval      SKsgm_Livello(sgm) = 1
027300090203     c                   add       1             quanti_1
027400100716     c                   eval      SKsgm_Obbligo(sgm) = MAPOBBLIG
027500100716     c                   eval      SKsgm_AlfaNum(sgm) = MAPTIPOD
027600100716     c                   eval      SKsgm_Lungo(sgm)   = MAPLUNGH
027700100716     c                   eval      SKsgm_Decim(sgm)   = MAPDecim
027800090203      *
027900100716     c                   ElseIF    MAPNCAMPO <> *blank and MAPCATEGO <> *blank
028000100716     c                             and MAPCATEGO = sav_CATEGO
028100090203      *
028200100716     c                   eval      SKsgm_Campo(sgm)   = MAPNCAMPO
028300090203     c                   eval      SKsgm_Livello(sgm) = 2
028400090203     c                   add       1             quanti_2
028500100716     c                   eval      SKsgm_Obbligo(sgm) = MAPOBBLIG
028600100716     c                   eval      SKsgm_AlfaNum(sgm) = MAPTIPOD
028700100716     c                   eval      SKsgm_Lungo(sgm)   = MAPLUNGH
028800100716     c                   eval      SKsgm_Decim(sgm)   = MAPDecim
028900090203      *
029000090203     c                   End
029100090203      **
029200090203     c                   End
029300090203      **
029400100716     c     kMAP          reade     EDMAP01L
029500090203     c                   EndDo
029600090204      **
029700090204      ** memorizza quanti campi deve considerare x le schiere seguenti
029800090204     c                   z-add     sgm           xCampi
029900090203      **
030000090203     c                   Endsr
030100090203      * ?------------------------------------------------------------------ */
030200090203      *?      imposta i campi sulle schiere
030300090203      * ?------------------------------------------------------------------ */
030400090204     c     First_Level   Begsr
030500090203      **
030600090204      *   Sposta il record a tracciato libero sul campo di lavoro: DATI
030700090204     C                   eval      Dati = Segmento
030800090204     c                   movel     '+'           Separa_segmen
030900090204      *                /*---------------------- */
031000090204     c                   exsr      Split_Rec
031100090204      *                /*---------------------- */
031200090204      **
031300090204      **  ha diviso il segmento in campi che come primo livello possono essere
031400090204      **   anche insieme di sottocampi il Tipo dati delle DS non è né "A" né "N".
031500090204     c                   z-add     quanti_1      quanti
031600090204      **
031700060612      **   Controlli Formali sui campi: Obbligatorietà e controllo Numerico
031800090204     c                   clear                   position          3 0
031900090204     c                   z-add     0             DaQui
032000090203      **
032100090203      * ? 1° campo :  è il tipo SEGMENTO che non viene riportato nelle schiere né nella DS
032200090203     C                   z-add     1             sk
032300090203      * ?-----> Errore
032400090203     c                   If        LunDato_sk(sk) =  0
032500090205     c                   eval      esito = 'E'
032600090205     c                   Else
032700090205     c                   eval      tipo_segmento = %subst(Dato_sk(sk):1:+
032800090205     c                                              LunDato_sk(sk))
032900090205     c                   End
033000090203      **
033100090203      * ? Carica le schiere del Livello passato se ci sono dei dati
033200090203      **     Trova gli elementi di schiera dove impostare i dati
033300090204     c                   do        quanti
033400090203      **
033500090205      * cerca il primo disponibile del Livello 1 saltando
033600090205      *  il primo che contiene il Tipo Segmento precedentemente rilevato
033700090205      **
033800090205     c                   add       1             DaQui
033900090205     c     daQui         do        xCampi        sgm
034000090205     c                   if        SKsgm_Livello(sgm) = 1
034100090205     c                   z-add     sgm           daQui             3 0
034200090205     c                   leave
034300090205     c                   end
034400090205     c                   enddo
034500090203      **
034600090203     C                   add       1             sk
034700090203     c                   If        LunDato_sk(sk) >  0
034800090205     c                   eval      SKout_ValFld(DaQui) = %subst(Dato_sk(sk):1:+
034900060612     c                                              LunDato_sk(sk))
035000090205     c                   eval      SKout_Lungo(DaQui)   = LunDato_sk(sk)
035100090205     c                   if        DatoNum_sk(sk) = 'N'
035200090205     c                   eval      SKout_AlfaNum(DaQui) = 'A'
035300090205     c                   else
035400090205     c                   eval      SKout_AlfaNum(DaQui) = 'N'
035500090205     c                   end
035600090205     c                   eval      position = %scan(':':SKout_ValFld(DaQui))
035700090204      *
035800090204     c                   if        position > 0
035900090204     c                   eval      SKout_Diviso(DaQui) = ':'
036000060612     c                   End
036100090204     c                   End
036200060612      **
036300090203     c                   EndDO
036400060612      **
036500060612     c                   Endsr
036600090204      *  ------------------------------------------------------------------ */
036700090204      *       Deve scomporre le DS nei sottocampi
036800090204      *  ------------------------------------------------------------------ */
036900090204     c     Second_Level  Begsr
037000090204      **
037100090204     c                   z-add     quanti_2      quanti
037200090204     c                   z-add     1             sgm_salva         3 0
037300090204      **
037400090204      * ?  Cicla x decodificare  i sottocampi
037500090204     c                   DOW       sgm_salva <= 50
037600090204      **
037700090204     c                   exsr      Cerca_DSLVL_1
037800090204      **
037900090204      **   se deve uscire da tutto  perchè è arrivato in fondo
038000090204      **     forza l'uscita
038100090204     c                   if        Finito = 'S'
038200090204     c                   Leave
038300090204     c                   end
038400090204      **
038500090204      * ?  se il campo ha dei sotto campi
038600090204     c                   if        SKout_Diviso(sgm) = ':'
038700090204      * ?  con  Diviso:
038800090204      **    deve splittare nuovamente i campi
038900090205     C                   eval      Dati = SKout_ValFld(sgm)
039000090204     c                   movel     ':'           Separa_segmen
039100090204      *                /*---------------------- */
039200090204     c                   exsr      Split_Rec
039300090204      *                /*---------------------- */
039400090204      **
039500090204      ** imposta i campi di secondo livello relativi
039600090204      **  alla DS precedentemente suddivisa
039700090204     c                   do        Nr_Campi      xx                3 0
039800090204      *
039900090204     c                   add       1             sgm_salva
040000090205      *
040100090204     c                   if        SKsgm_Livello(sgm_salva) = 2
040200090205     c                   eval      SKout_ValFld(sgm_salva)  = Dato_sk(xx)
040300090205     c                   eval      SKout_Lungo(sgm_salva)   = LunDato_sk(xx)
040400090205     c                   if        DatoNum_sk(xx) = 'N'
040500090205     c                   eval      SKout_AlfaNum(sgm_salva) = 'A'
040600090205     c                   else
040700090205     c                   eval      SKout_AlfaNum(sgm_salva) = 'N'
040800090205     c                   end
040900090204     c                   else
041000090204     c                   Leave
041100090204     c                   end
041200090204      *
041300090204     c                   EndDo
041400090204      **
041500090204     c                   Else
041600090204      * ? senza Diviso:
041700090204      **    deve prendere la Variabile ed impostare il campo successivo
041800090204     c                   add       1             sgm_salva
041900090204     c                   if        SKsgm_Livello(sgm_salva) = 2
042000090205     c                   eval      SKout_ValFld(sgm_salva) =
042100090205     c                                       SKout_ValFld(sgm_salva -1)
042200090205     c                   eval      SKout_Lungo(sgm_salva) =
042300090205     c                                        SKout_Lungo(sgm_salva -1)
042400090205     c                   if        DatoNum_sk(sgm_salva -1) = 'N'
042500090205     c                   eval      SKout_AlfaNum(sgm_salva) = 'A'
042600090205     c                   else
042700090205     c                   eval      SKout_AlfaNum(sgm_salva) = 'N'
042800090205     c                   end
042900090205     c
043000090204     c                   end
043100090204      **
043200090204     c                   End
043300090204      **
043400090204     c                   EndDO
043500090204      **
043600090204     c                   Endsr
043700090204      *-------------------------------------------------------------------------
043800090204      *     Cerca se ci sono delle DS a livello 1  da riportare sui campi seguenti
043900090204      *  ------------------------------------------------------------------ */    §
044000090204     c     Cerca_DSLVL_1 Begsr
044100090204      **
044200090204     c                   clear                   Finito            1
044300090204      **
044400090204      **  Si rilegge le schiere pre-caricate al Primo Livello di decodifica segmento
044500090204      **   e deve andare ad inputare i campi definiti di secondo livello come parti
044600090204      **    dei campi DS di primo Livello
044700090204     c     sgm_salva     Do        50            sgm
044800090204      *
044900090204      *  esce forzatamente quando ha finito tutto quello che doveva splittare
045000090204     c                   if        SKsgm_Livello(sgm) = 0
045100090204     c                   move      'S'           Finito
045200090204     c                   leave
045300090204     c                   end
045400090204      *
045500090204     c                   if        SKsgm_Livello(sgm)  =  1  and
045600090204     c                             SKsgm_AlfaNum(sgm) <> 'A' and
045700090204     c                             SKsgm_AlfaNum(sgm) <> 'N'
045800090204      *
045900090204      * si tratta di una DS che potenzialmente può contenere più campi.
046000090204      *  Come capirlo ?!
046100090204      *     Nella schiera SKout_diviso se c'è un segno di (:) allora ci sono più campi
046200090204      *   altrimenti esiste un solo valore che deve essere inserito nel primo campo
046300090204      *   successivo all'elemento appena trovato della schiera.
046400090204     c                   eval      sgm_salva = sgm
046500090204     c                   leave
046600090204      *
046700090204     c                   end
046800090204      *
046900090204     c                   EndDo
047000090204      **
047100090204     c                   Endsr
047200060614      * ?------------------------------------------------------------------ */
047300090206      *  Elimina il punto interrogativo dove serve a far leggere come
047400090206      *   carattere normale un carattere particolare x la gestione del segmento
047500090206      * ?------------------------------------------------------------------ */
047600090206     C     Punto_interr  BEGSR
047700090206      **
047800090206     c                   do        50            sgm
047900090206     c                   clear                   position
048000090206     c                   eval      position = %scan('?':SKout_ValFld(sgm):1)
048100100312      **
048200100312     c     Altro_Puntint tag
048300100312      **
048400090206     c                   if        position > 0
048500090206      * se seguito da (' + :)
048600090206     c                   if        %subst(SKout_ValFld(sgm):position+1:1) = ''''
048700090206     c                                 or
048800090206     c                             %subst(SKout_ValFld(sgm):position+1:1) = '+'
048900090206     c                                 or
049000090206     c                             %subst(SKout_ValFld(sgm):position+1:1) = ':'
049100100311     c                                 or
049200100311     c                             %subst(SKout_ValFld(sgm):position+1:1) = '.'
049300100311     c                                 or
049400100311     c                             %subst(SKout_ValFld(sgm):position+1:1) = '?'
049500100312      *
049600100312      * sostituisce il (?) con il carattere speciale
049700100312      *  e poi abblenca dove stava prima
049800100312     c************       eval      %subst(SKout_ValFld(sgm):position:1) =
049900100312     c************                 %subst(SKout_ValFld(sgm):position+1:1)
050000100312     c************       eval      %subst(SKout_ValFld(sgm):position+1:1) = ' '
050100100312      * e
050200100312      ** invece :
050300100312      **** Deve spostare in avanti di 1 tutti i caratteri alla destra del (?)
050400100312     c                   eval      %subst(SKout_ValFld(sgm):position:
050500100312     c                             %len(SKout_ValFld(sgm)) - position)
050600100312     c                                =
050700100312     c                             %subst(SKout_ValFld(sgm):position + 1 :
050800100312     c                             %len(SKout_ValFld(sgm)) - position)
050900100312      * e
051000100312      * l'ultimo carattere viene abblencato
051100100312     c                   eval      %subst(SKout_ValFld(sgm):
051200100312     c                             %len(SKout_ValFld(sgm)):1) = ' '
051300100312      ******
051400100312     c                   clear                   newPosition       3 0
051500100312      ** poi deve cercare se c'è ancora un altro (?) da sostituire da lì in avanti
051600100312     c                   eval      NewPosition = %scan('?':SKout_ValFld(sgm):
051700100312     c                             position)
051800100312     c                   if        NewPosition > 0
051900100312      * se lo ha trovato riesegue queste operazioni da quel punto in avanti
052000100312     c                   z-add     NewPosition   Position
052100100312     c                   goto      Altro_Puntint
052200100312     c                   End
052300100301      ******
052400090206     c                   End
052500090206     c                   End
052600100312      **
052700100312      **
052800090206     c                   Enddo
052900090206      **
053000090206     c                   Endsr
053100090206      * ?------------------------------------------------------------------ */
053200090209      *  Toglie virgole o punti da campi numerici senza decimali
053300090206      * ?------------------------------------------------------------------ */
053400090209     C     Senza_DecimaliBEGSR
053500090206      **
053600090209      **
053700090209      **  Si rilegge le schiere e sui campi numerici valuta se presenti virgole
053800090209      **   o punti per eliminarli
053900090209     c                   Do        50            sgm
054000090209      *
054100090209      *  esce forzatamente quando ha finito tutto quello che doveva splittare
054200090209     c                   if        SKsgm_Livello(sgm) = 0
054300090209     c                   move      'S'           Finito
054400090209     c                   leave
054500090209     c                   end
054600090209      *
054700090209     c                   if        SKsgm_Livello(sgm)  =  2  and
054800090209     c                             SKsgm_AlfaNum(sgm)  = 'N'
054900090209      *
055000090209     c                   z-add     0             position
055100090209      * prova con il punto
055200090209     c                   eval      position = %scan('.':SKout_ValFld(sgm):1)
055300090209      *
055400090209     c                   if        position = 0
055500090209      * prova con la virgola
055600090209     c                   eval      position = %scan(',':SKout_ValFld(sgm):1)
055700090209     c                   end
055800090209      *
055900090616      *  Se ha trovato da sostituire il Separatore Decimale del campo
056000090616     c                   clear                   conta_decimali    3 0
056100100629      *
056200100629      *  Se è stato passato un campo numerico vuoto occorre intercettarlo e riempirlo
056300100629      *   con zeri per NON far rompere il programma.
056400100629     c                   IF        SKsgm_Lungo(sgm)    > 0   and
056500100629     c                             SKout_ValFld(sgm)   = *blank
056600100629      *
056700100629     c                   eval      SKout_ValFld(sgm) = *all'0'
056800100629     c                   eval      SKout_Lungo(sgm)  = SKsgm_Lungo(sgm)
056900100629     c                   eval      SKout_AlfaNum(sgm)= 'N'
057000100629      *
057100100629     c                   end
057200090616      *
057300100629      * quindi rileva la posizione
057400090209     c                   if        position > 0
057500100629      *
057600090209     c                   eval      xByte = SKsgm_Lungo(sgm)
057700090209      *
057800090616      *  Dal Separatore in avanti
057900090209     c     position      Do        xByte         xx                3 0
058000090616      *
058100090616      *  si deve controllare se vengono passati tutti i decimali che ci si aspettava
058200090616     c                   if        conta_decimali = SKsgm_Decim(sgm) and
058300090616     c                                SKsgm_Decim(sgm) > 0
058400090825      **
058500090825      **  prima di uscire deve comunque pulire i bytes rimanenti sulla destra
058600090825     c     xx            Do        xByte         yy                3 0
058700090825     c                   eval      %subst(SKout_ValFld(sgm):yy:1) = ' '
058800090825     c                   end
058900090825      **
059000090616     c                   leave
059100090616     c                   end
059200090616      *
059300090616      * Se Blank deve impostarlo a (0) come decimale valido
059400090616     c                   if        %subst(SKout_ValFld(sgm):xx+1:1) = ' '
059500090616     c                   eval      %subst(SKout_ValFld(sgm):xx:1) = '0'
059600090616     c                   else
059700090616      *
059800090616      * sposta in avanti di 1 posizione il carattere seguente
059900090209     c                   eval      %subst(SKout_ValFld(sgm):xx:1) =
060000090209     c                             %subst(SKout_ValFld(sgm):xx+1:1)
060100090616     c                   end
060200090616      *
060300090616     c                   add       1             conta_decimali
060400090616      *
060500090209     c                   endDo
060600090209      *
060700090616      *  Reimposta la giusta lunghezza del campo
060800090616      *   aggiunge i decimali e toglie (1) equivale al separatore decimale.
060900090616     c                   eval      SKout_Lungo(sgm) =
061000090616     c                               SKout_Lungo(sgm) + SKsgm_Decim(sgm) - 1
061100090616      *
061200090616     c                   else
061300100629      *
061400090616      *  Se il campo avrebbe dovuto avere dei decimali e hanno inviato un campo
061500090616      * con solo degli interi occorre aggiungere tanti zeri quanti sono i decimali.
061600090616     c                   if           SKsgm_Decim(sgm) > 0
061700090616      *
061800090616     c                   eval      conta_decimali = SKsgm_Decim(sgm)
061900090616     c                   eval      xx = SKout_Lungo(sgm) + 1
062000090616      *
062100090616     c                   do        conta_decimali
062200090616     c                   eval      %subst(SKout_ValFld(sgm):xx:1) = '0'
062300090616     c                   eval      xx = xx + 1
062400090616     c                   enddo
062500090616      *
062600090616      *  Reimposta la giusta lunghezza del campo
062700090616     c                   eval      SKout_Lungo(sgm) =
062800090616     c                               SKout_Lungo(sgm) + SKsgm_Decim(sgm)
062900090616      *
063000090616     c                   end
063100090209     c                   end
063200090209      *
063300090209     c                   end
063400090209      *
063500090209     c                   EndDo
063600090209      **
063700090209      **
063800090209     c                   Endsr
063900090209      * ?------------------------------------------------------------------ */
064000090209      *  Controlla eventuali Errori
064100090209      * ?------------------------------------------------------------------ */
064200090209     C     Errori        BEGSR
064300090209      **
064400090206      **  Se obbligatorio ='S'
064500090206      **     altrimenti
064600090206      **  Se opzionale ='N'
064700090206     c                   do        50            sgm
064800090206     c                   if        SKsgm_Obbligo(sgm) = 'C'
064900090206     c                   eval         SKsgm_Obbligo(sgm) = 'N'
065000090206     c                   elseIf    SKsgm_Obbligo(sgm) = 'M'
065100090206     c                   eval         SKsgm_Obbligo(sgm) = 'S'
065200090206     c                   end
065300090206     c                   enddo
065400090206      **
065500090206     c                   do        50            sgm
065600090206     c                   if        SKsgm_AlfaNum(sgm) = 'N' and
065700090206     c                             SKout_AlfaNum(sgm) <>'N' and
065800090206     c                             SKout_Lungo(sgm) > 0
065900090206     c                   eval       SKout_Errori(sgm) = 'E'
066000090206     c                   eval      SKout_DesErr(sgm) = ERR_inCAMPO +
066100090206     c                             SKsgm_Campo(sgm) + ERR_AlfaNUM
066200090206     c                   eval      esito = 'E'
066300090206     c                   end
066400090206     c                   Enddo
066500090206      **
066600090206     c                   Endsr
066700090206      * ?------------------------------------------------------------------ */
066800090206      *   DEFINIZIONE CHIAVI                               *
066900090206      *----------------------------------------------------------------
067000090206     C     *INZSR        BEGSR
067100090206      *------------------
067200971216      * Recupero data e ora
067300971216     C                   TIME                    WORA
067400991124     C                   TIME                    W0140
067500991124      * UDATE IN GGMMAAAA
067600991124     C                   MOVE      W0140         WDTGIO
067700991124      * UDATE IN AAAAMMGG
067800991124     C     *eur          MOVEL     WDTGIO        DATA_eur
067900991124     C     *iso          MOVEL     DATA_eur      dateu
068000050414      *
068100991124     C                   ENDSR
068200060621      * ?------------------------------------------------------------------ */
068300060621      *?      X non bloccare in nessun caso il traduttore CLIENTI
068400060621      * ?------------------------------------------------------------------ */
068500060621     C     *pssr         BEGSR
068600060621     C
068700090205     C                   eval      esito = 'E'
068800060621     C                   ENDSR     '*CANCL'
068900060621     C
069000090204      *  ------------------------------------------------------------------ */
