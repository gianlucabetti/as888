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
014100100716     c     kMAP          SETLL     EDMAP01L
014200100716     c                   if        not %Equal(EDMAP01L)
014300090205     C                   eval      esito = 'E'
014400090203     c                   goto      FINE
014500090203     c                   end
014600090203      **
014700090203      **  Prepara le schiere per recepire i valori dei campi
014800090203      * ?              /*---------------------- */
014900090203     c                   exsr      Carica_Schiere
015000090203      * ?              /*---------------------- */
015100090204      **
015200090204      **  Divide prima i MacroCampi separati da (+)
015300090204      *                /*---------------------- */
015400090204     c                   exsr      First_Level
015500090204      *                /*---------------------- */
015600090204      **
015700090204      ** Deve scendere ad un secondo Livello
015800090204      **   e  deve fare delle considerazioni sui campi seprati da (:)
015900090204      *                /*---------------------- */
016000090204     c                   exsr      Second_Level
016100090204      *                /*---------------------- */
016200090203      *
016300090206      *  se il punto interrogativo nei campi precede un carattere
016400090206      *   particolare il punto interrogativo deve essere sostituito con Blank
016500090206      *                /*---------------------- */
016600090206     c                   exsr      Punto_interr
016700090206      *                /*---------------------- */
016800090206      *
016900090209      * Elimina le virgole o i punti se i campi devono riportare solo interi
017000090209      *                /*---------------------- */
017100090209     c                   exsr      Senza_Decimali
017200090209      *                /*---------------------- */
017300090209      *
017400090206      *                /*---------------------- */
017500090206     c                   exsr      Errori
017600090206      *                /*---------------------- */
017700090206      *
017800090203     c     FINE          tag
017900090212     c                   SETON                                        RT
018000090203      * ?------------------------------------------------------------------ */
018100090203      *?      Suddivide i campi della riga con carattere divisorio (;)
018200090203      * ?------------------------------------------------------------------ */
018300090203     c     Split_Rec     Begsr
018400090203      **
018500090203     c                   clear                   Nr_campi
018600090203     c                   clear                   Campi_Record
018700090203     c                   clear                   Campi_Lunghi
018800090203     c                   clear                   Campi_Numerici
018900090203     c                   clear                   Campi_Decimali
019000090203     c                   movel     '.'           Decimal_Separ
019100090203     c                   call      'TRTCT00R'
019200090203      * ?     Input
019300090203     c                   parm                    Separa_segmen
019400090203     c                   parm                    Decimal_Separ
019500090203     c                   parm                    dati
019600090203      * ?     Output  su schiere di 50  elementi
019700090203     c                   parm                    Nr_campi
019800090203     c                   parm                    Campi_Record                   * i dati
019900090203     c                   parm                    Campi_Lunghi                   * lunghezza dati
020000090203     c                   parm                    Campi_Numerici                 * se numerici
020100090203     c                   parm                    Campi_Decimali                 * quanti decimali
020200090203      **
020300090203     c                   Endsr
020400060612      * ?------------------------------------------------------------------ */
020500090203      *?      Inizializza le schiere per incasellare i dati riceuti posizionalmente
020600060612      * ?------------------------------------------------------------------ */
020700090203     c     Carica_SchiereBegsr
020800090203      **
020900090203      **  deve impostare i valori decodificati nelle schiere in modo posizionale.
021000090203      **    se presenti dei campi con divisore (:) occorrerà eseguire un ulteriore
021100090203      **     split dei campi aggregati dopo il primo giro.
021200090203     c                   clear                   SKsgm_Campo
021300090203     c                   clear                   SKsgm_Livello
021400090203     c                   clear                   SKsgm_Obbligo
021500090203     c                   clear                   SKsgm_AlfaNum
021600090203     c                   clear                   SKsgm_Lungo
021700090209     c                   clear                   SKsgm_Decim
021800090203      **
021900090205     c                   clear                   SKout_ValFld
022000090203     c                   clear                   SKout_AlfaNum
022100090203     c                   clear                   SKout_Lungo
022200090203     c                   clear                   SKout_Decimal
022300090203     c                   clear                   SKout_Errori
022400090206     c                   clear                   SKout_DesErr
022500090205     c                   clear                   SKout_Diviso
022600090203      **
022700090203     c                   clear                   sav_CATEGO
022800090203     c                   clear                   sgm               3 0
022900090203     c                   clear                   quanti_1          3 0
023000090203     c                   clear                   quanti_2          3 0
023100090203     c                   clear                   quanti            3 0
023200090204     c                   clear                   xquanti           3 0
023300090203      **
023400100716     c     kMAP          reade     EDMAP01L
023500100716     c                   dow       not %EoF(EDMAP01L)
023600090203      **
023700090203      **  Esclude la testata che descrive il segmento
023800090203      **   ed incrementa numeratore di schiera
023900100716     c                   if        MAPSEQUEN > 0
024000090203     c                   add       1             sgm
024100090203      **
024200090203      **  imposta o un campo o la DS di un gruppo di campi
024300100716     c                   if        MAPNCAMPO = *blank and MAPCATEGO <> *blank or
024400100716     c                             MAPNCAMPO <> *blank and MAPCATEGO = *blank
024500090203      *
024600100716     c                   eval      SKsgm_Campo(sgm)   = MAPCATEGO
024700100716     c                   eval      Sav_CATEGO         = MAPCATEGO
024800090203      *
024900090203     c                   if        SKsgm_Campo(sgm) = *blank
025000100716     c                   eval      SKsgm_Campo(sgm) = MAPNCAMPO
025100090203     c                   End
025200090203      *
025300090203     c                   eval      SKsgm_Livello(sgm) = 1
025400090203     c                   add       1             quanti_1
025500100716     c                   eval      SKsgm_Obbligo(sgm) = MAPOBBLIG
025600100716     c                   eval      SKsgm_AlfaNum(sgm) = MAPTIPOD
025700100716     c                   eval      SKsgm_Lungo(sgm)   = MAPLUNGH
025800100716     c                   eval      SKsgm_Decim(sgm)   = MAPDecim
025900090203      *
026000100716     c                   ElseIF    MAPNCAMPO <> *blank and MAPCATEGO <> *blank
026100100716     c                             and MAPCATEGO = sav_CATEGO
026200090203      *
026300100716     c                   eval      SKsgm_Campo(sgm)   = MAPNCAMPO
026400090203     c                   eval      SKsgm_Livello(sgm) = 2
026500090203     c                   add       1             quanti_2
026600100716     c                   eval      SKsgm_Obbligo(sgm) = MAPOBBLIG
026700100716     c                   eval      SKsgm_AlfaNum(sgm) = MAPTIPOD
026800100716     c                   eval      SKsgm_Lungo(sgm)   = MAPLUNGH
026900100716     c                   eval      SKsgm_Decim(sgm)   = MAPDecim
027000090203      *
027100090203     c                   End
027200090203      **
027300090203     c                   End
027400090203      **
027500100716     c     kMAP          reade     EDMAP01L
027600090203     c                   EndDo
027700090204      **
027800090204      ** memorizza quanti campi deve considerare x le schiere seguenti
027900090204     c                   z-add     sgm           xCampi
028000090203      **
028100090203     c                   Endsr
028200090203      * ?------------------------------------------------------------------ */
028300090203      *?      imposta i campi sulle schiere
028400090203      * ?------------------------------------------------------------------ */
028500090204     c     First_Level   Begsr
028600090203      **
028700090204      *   Sposta il record a tracciato libero sul campo di lavoro: DATI
028800090204     C                   eval      Dati = Segmento
028900090204     c                   movel     '+'           Separa_segmen
029000090204      *                /*---------------------- */
029100090204     c                   exsr      Split_Rec
029200090204      *                /*---------------------- */
029300090204      **
029400090204      **  ha diviso il segmento in campi che come primo livello possono essere
029500090204      **   anche insieme di sottocampi il Tipo dati delle DS non è né "A" né "N".
029600090204     c                   z-add     quanti_1      quanti
029700090204      **
029800060612      **   Controlli Formali sui campi: Obbligatorietà e controllo Numerico
029900090204     c                   clear                   position          3 0
030000090204     c                   z-add     0             DaQui
030100090203      **
030200090203      * ? 1° campo :  è il tipo SEGMENTO che non viene riportato nelle schiere né nella DS
030300090203     C                   z-add     1             sk
030400090203      * ?-----> Errore
030500090203     c                   If        LunDato_sk(sk) =  0
030600090205     c                   eval      esito = 'E'
030700090205     c                   Else
030800090205     c                   eval      tipo_segmento = %subst(Dato_sk(sk):1:+
030900090205     c                                              LunDato_sk(sk))
031000090205     c                   End
031100090203      **
031200090203      * ? Carica le schiere del Livello passato se ci sono dei dati
031300090203      **     Trova gli elementi di schiera dove impostare i dati
031400090204     c                   do        quanti
031500090203      **
031600090205      * cerca il primo disponibile del Livello 1 saltando
031700090205      *  il primo che contiene il Tipo Segmento precedentemente rilevato
031800090205      **
031900090205     c                   add       1             DaQui
032000090205     c     daQui         do        xCampi        sgm
032100090205     c                   if        SKsgm_Livello(sgm) = 1
032200090205     c                   z-add     sgm           daQui             3 0
032300090205     c                   leave
032400090205     c                   end
032500090205     c                   enddo
032600090203      **
032700090203     C                   add       1             sk
032800090203     c                   If        LunDato_sk(sk) >  0
032900090205     c                   eval      SKout_ValFld(DaQui) = %subst(Dato_sk(sk):1:+
033000060612     c                                              LunDato_sk(sk))
033100090205     c                   eval      SKout_Lungo(DaQui)   = LunDato_sk(sk)
033200090205     c                   if        DatoNum_sk(sk) = 'N'
033300090205     c                   eval      SKout_AlfaNum(DaQui) = 'A'
033400090205     c                   else
033500090205     c                   eval      SKout_AlfaNum(DaQui) = 'N'
033600090205     c                   end
033700090205     c                   eval      position = %scan(':':SKout_ValFld(DaQui))
033800090204      *
033900090204     c                   if        position > 0
034000090204     c                   eval      SKout_Diviso(DaQui) = ':'
034100060612     c                   End
034200090204     c                   End
034300060612      **
034400090203     c                   EndDO
034500060612      **
034600060612     c                   Endsr
034700090204      *  ------------------------------------------------------------------ */
034800090204      *       Deve scomporre le DS nei sottocampi
034900090204      *  ------------------------------------------------------------------ */
035000090204     c     Second_Level  Begsr
035100090204      **
035200090204     c                   z-add     quanti_2      quanti
035300090204     c                   z-add     1             sgm_salva         3 0
035400090204      **
035500090204      * ?  Cicla x decodificare  i sottocampi
035600090204     c                   DOW       sgm_salva <= 50
035700090204      **
035800090204     c                   exsr      Cerca_DSLVL_1
035900090204      **
036000090204      **   se deve uscire da tutto  perchè è arrivato in fondo
036100090204      **     forza l'uscita
036200090204     c                   if        Finito = 'S'
036300090204     c                   Leave
036400090204     c                   end
036500090204      **
036600090204      * ?  se il campo ha dei sotto campi
036700090204     c                   if        SKout_Diviso(sgm) = ':'
036800090204      * ?  con  Diviso:
036900090204      **    deve splittare nuovamente i campi
037000090205     C                   eval      Dati = SKout_ValFld(sgm)
037100090204     c                   movel     ':'           Separa_segmen
037200090204      *                /*---------------------- */
037300090204     c                   exsr      Split_Rec
037400090204      *                /*---------------------- */
037500090204      **
037600090204      ** imposta i campi di secondo livello relativi
037700090204      **  alla DS precedentemente suddivisa
037800090204     c                   do        Nr_Campi      xx                3 0
037900090204      *
038000090204     c                   add       1             sgm_salva
038100090205      *
038200090204     c                   if        SKsgm_Livello(sgm_salva) = 2
038300090205     c                   eval      SKout_ValFld(sgm_salva)  = Dato_sk(xx)
038400090205     c                   eval      SKout_Lungo(sgm_salva)   = LunDato_sk(xx)
038500090205     c                   if        DatoNum_sk(xx) = 'N'
038600090205     c                   eval      SKout_AlfaNum(sgm_salva) = 'A'
038700090205     c                   else
038800090205     c                   eval      SKout_AlfaNum(sgm_salva) = 'N'
038900090205     c                   end
039000090204     c                   else
039100090204     c                   Leave
039200090204     c                   end
039300090204      *
039400090204     c                   EndDo
039500090204      **
039600090204     c                   Else
039700090204      * ? senza Diviso:
039800090204      **    deve prendere la Variabile ed impostare il campo successivo
039900090204     c                   add       1             sgm_salva
040000090204     c                   if        SKsgm_Livello(sgm_salva) = 2
040100090205     c                   eval      SKout_ValFld(sgm_salva) =
040200090205     c                                       SKout_ValFld(sgm_salva -1)
040300090205     c                   eval      SKout_Lungo(sgm_salva) =
040400090205     c                                        SKout_Lungo(sgm_salva -1)
040500090205     c                   if        DatoNum_sk(sgm_salva -1) = 'N'
040600090205     c                   eval      SKout_AlfaNum(sgm_salva) = 'A'
040700090205     c                   else
040800090205     c                   eval      SKout_AlfaNum(sgm_salva) = 'N'
040900090205     c                   end
041000090205     c
041100090204     c                   end
041200090204      **
041300090204     c                   End
041400090204      **
041500090204     c                   EndDO
041600090204      **
041700090204     c                   Endsr
041800090204      *-------------------------------------------------------------------------
041900090204      *     Cerca se ci sono delle DS a livello 1  da riportare sui campi seguenti
042000090204      *  ------------------------------------------------------------------ */    §
042100090204     c     Cerca_DSLVL_1 Begsr
042200090204      **
042300090204     c                   clear                   Finito            1
042400090204      **
042500090204      **  Si rilegge le schiere pre-caricate al Primo Livello di decodifica segmento
042600090204      **   e deve andare ad inputare i campi definiti di secondo livello come parti
042700090204      **    dei campi DS di primo Livello
042800090204     c     sgm_salva     Do        50            sgm
042900090204      *
043000090204      *  esce forzatamente quando ha finito tutto quello che doveva splittare
043100090204     c                   if        SKsgm_Livello(sgm) = 0
043200090204     c                   move      'S'           Finito
043300090204     c                   leave
043400090204     c                   end
043500090204      *
043600090204     c                   if        SKsgm_Livello(sgm)  =  1  and
043700090204     c                             SKsgm_AlfaNum(sgm) <> 'A' and
043800090204     c                             SKsgm_AlfaNum(sgm) <> 'N'
043900090204      *
044000090204      * si tratta di una DS che potenzialmente può contenere più campi.
044100090204      *  Come capirlo ?!
044200090204      *     Nella schiera SKout_diviso se c'è un segno di (:) allora ci sono più campi
044300090204      *   altrimenti esiste un solo valore che deve essere inserito nel primo campo
044400090204      *   successivo all'elemento appena trovato della schiera.
044500090204     c                   eval      sgm_salva = sgm
044600090204     c                   leave
044700090204      *
044800090204     c                   end
044900090204      *
045000090204     c                   EndDo
045100090204      **
045200090204     c                   Endsr
045300060614      * ?------------------------------------------------------------------ */
045400090206      *  Elimina il punto interrogativo dove serve a far leggere come
045500090206      *   carattere normale un carattere particolare x la gestione del segmento
045600090206      * ?------------------------------------------------------------------ */
045700090206     C     Punto_interr  BEGSR
045800090206      **
045900090206     c                   do        50            sgm
046000090206     c                   clear                   position
046100090206     c                   eval      position = %scan('?':SKout_ValFld(sgm):1)
046200100312      **
046300100312     c     Altro_Puntint tag
046400100312      **
046500090206     c                   if        position > 0
046600090206      * se seguito da (' + :)
046700090206     c                   if        %subst(SKout_ValFld(sgm):position+1:1) = ''''
046800090206     c                                 or
046900090206     c                             %subst(SKout_ValFld(sgm):position+1:1) = '+'
047000090206     c                                 or
047100090206     c                             %subst(SKout_ValFld(sgm):position+1:1) = ':'
047200100311     c                                 or
047300100311     c                             %subst(SKout_ValFld(sgm):position+1:1) = '.'
047400100311     c                                 or
047500100311     c                             %subst(SKout_ValFld(sgm):position+1:1) = '?'
047600100312      *
047700100312      * sostituisce il (?) con il carattere speciale
047800100312      *  e poi abblenca dove stava prima
047900100312     c************       eval      %subst(SKout_ValFld(sgm):position:1) =
048000100312     c************                 %subst(SKout_ValFld(sgm):position+1:1)
048100100312     c************       eval      %subst(SKout_ValFld(sgm):position+1:1) = ' '
048200100312      * e
048300100312      ** invece :
048400100312      **** Deve spostare in avanti di 1 tutti i caratteri alla destra del (?)
048500100312     c                   eval      %subst(SKout_ValFld(sgm):position:
048600100312     c                             %len(SKout_ValFld(sgm)) - position)
048700100312     c                                =
048800100312     c                             %subst(SKout_ValFld(sgm):position + 1 :
048900100312     c                             %len(SKout_ValFld(sgm)) - position)
049000100312      * e
049100100312      * l'ultimo carattere viene abblencato
049200100312     c                   eval      %subst(SKout_ValFld(sgm):
049300100312     c                             %len(SKout_ValFld(sgm)):1) = ' '
049400100312      ******
049500100312     c                   clear                   newPosition       3 0
049600100312      ** poi deve cercare se c'è ancora un altro (?) da sostituire da lì in avanti
049700100312     c                   eval      NewPosition = %scan('?':SKout_ValFld(sgm):
049800100312     c                             position)
049900100312     c                   if        NewPosition > 0
050000100312      * se lo ha trovato riesegue queste operazioni da quel punto in avanti
050100100312     c                   z-add     NewPosition   Position
050200100312     c                   goto      Altro_Puntint
050300100312     c                   End
050400100301      ******
050500090206     c                   End
050600090206     c                   End
050700100312      **
050800100312      **
050900090206     c                   Enddo
051000090206      **
051100090206     c                   Endsr
051200090206      * ?------------------------------------------------------------------ */
051300090209      *  Toglie virgole o punti da campi numerici senza decimali
051400090206      * ?------------------------------------------------------------------ */
051500090209     C     Senza_DecimaliBEGSR
051600090206      **
051700090209      **
051800090209      **  Si rilegge le schiere e sui campi numerici valuta se presenti virgole
051900090209      **   o punti per eliminarli
052000090209     c                   Do        50            sgm
052100090209      *
052200090209      *  esce forzatamente quando ha finito tutto quello che doveva splittare
052300090209     c                   if        SKsgm_Livello(sgm) = 0
052400090209     c                   move      'S'           Finito
052500090209     c                   leave
052600090209     c                   end
052700090209      *
052800090209     c                   if        SKsgm_Livello(sgm)  =  2  and
052900090209     c                             SKsgm_AlfaNum(sgm)  = 'N'
053000090209      *
053100090209     c                   z-add     0             position
053200090209      * prova con il punto
053300090209     c                   eval      position = %scan('.':SKout_ValFld(sgm):1)
053400090209      *
053500090209     c                   if        position = 0
053600090209      * prova con la virgola
053700090209     c                   eval      position = %scan(',':SKout_ValFld(sgm):1)
053800090209     c                   end
053900090209      *
054000090616      *  Se ha trovato da sostituire il Separatore Decimale del campo
054100090616     c                   clear                   conta_decimali    3 0
054200100629      *
054300100629      *  Se è stato passato un campo numerico vuoto occorre intercettarlo e riempirlo
054400100629      *   con zeri per NON far rompere il programma.
054500100629     c                   IF        SKsgm_Lungo(sgm)    > 0   and
054600100629     c                             SKout_ValFld(sgm)   = *blank
054700100629      *
054800100629     c                   eval      SKout_ValFld(sgm) = *all'0'
054900100629     c                   eval      SKout_Lungo(sgm)  = SKsgm_Lungo(sgm)
055000100629     c                   eval      SKout_AlfaNum(sgm)= 'N'
055100100629      *
055200100629     c                   end
055300090616      *
055400100629      * quindi rileva la posizione
055500090209     c                   if        position > 0
055600100629      *
055700090209     c                   eval      xByte = SKsgm_Lungo(sgm)
055800090209      *
055900090616      *  Dal Separatore in avanti
056000090209     c     position      Do        xByte         xx                3 0
056100090616      *
056200090616      *  si deve controllare se vengono passati tutti i decimali che ci si aspettava
056300090616     c                   if        conta_decimali = SKsgm_Decim(sgm) and
056400090616     c                                SKsgm_Decim(sgm) > 0
056500090825      **
056600090825      **  prima di uscire deve comunque pulire i bytes rimanenti sulla destra
056700090825     c     xx            Do        xByte         yy                3 0
056800090825     c                   eval      %subst(SKout_ValFld(sgm):yy:1) = ' '
056900090825     c                   end
057000090825      **
057100090616     c                   leave
057200090616     c                   end
057300090616      *
057400090616      * Se Blank deve impostarlo a (0) come decimale valido
057500090616     c                   if        %subst(SKout_ValFld(sgm):xx+1:1) = ' '
057600090616     c                   eval      %subst(SKout_ValFld(sgm):xx:1) = '0'
057700090616     c                   else
057800090616      *
057900090616      * sposta in avanti di 1 posizione il carattere seguente
058000090209     c                   eval      %subst(SKout_ValFld(sgm):xx:1) =
058100090209     c                             %subst(SKout_ValFld(sgm):xx+1:1)
058200090616     c                   end
058300090616      *
058400090616     c                   add       1             conta_decimali
058500090616      *
058600090209     c                   endDo
058700090209      *
058800090616      *  Reimposta la giusta lunghezza del campo
058900090616      *   aggiunge i decimali e toglie (1) equivale al separatore decimale.
059000090616     c                   eval      SKout_Lungo(sgm) =
059100090616     c                               SKout_Lungo(sgm) + SKsgm_Decim(sgm) - 1
059200090616      *
059300090616     c                   else
059400100629      *
059500090616      *  Se il campo avrebbe dovuto avere dei decimali e hanno inviato un campo
059600090616      * con solo degli interi occorre aggiungere tanti zeri quanti sono i decimali.
059700090616     c                   if           SKsgm_Decim(sgm) > 0
059800090616      *
059900090616     c                   eval      conta_decimali = SKsgm_Decim(sgm)
060000090616     c                   eval      xx = SKout_Lungo(sgm) + 1
060100090616      *
060200090616     c                   do        conta_decimali
060300090616     c                   eval      %subst(SKout_ValFld(sgm):xx:1) = '0'
060400090616     c                   eval      xx = xx + 1
060500090616     c                   enddo
060600090616      *
060700090616      *  Reimposta la giusta lunghezza del campo
060800090616     c                   eval      SKout_Lungo(sgm) =
060900090616     c                               SKout_Lungo(sgm) + SKsgm_Decim(sgm)
061000090616      *
061100090616     c                   end
061200090209     c                   end
061300090209      *
061400090209     c                   end
061500090209      *
061600090209     c                   EndDo
061700090209      **
061800090209      **
061900090209     c                   Endsr
062000090209      * ?------------------------------------------------------------------ */
062100090209      *  Controlla eventuali Errori
062200090209      * ?------------------------------------------------------------------ */
062300090209     C     Errori        BEGSR
062400090209      **
062500090206      **  Se obbligatorio ='S'
062600090206      **     altrimenti
062700090206      **  Se opzionale ='N'
062800090206     c                   do        50            sgm
062900090206     c                   if        SKsgm_Obbligo(sgm) = 'C'
063000090206     c                   eval         SKsgm_Obbligo(sgm) = 'N'
063100090206     c                   elseIf    SKsgm_Obbligo(sgm) = 'M'
063200090206     c                   eval         SKsgm_Obbligo(sgm) = 'S'
063300090206     c                   end
063400090206     c                   enddo
063500090206      **
063600090206     c                   do        50            sgm
063700090206     c                   if        SKsgm_AlfaNum(sgm) = 'N' and
063800090206     c                             SKout_AlfaNum(sgm) <>'N' and
063900090206     c                             SKout_Lungo(sgm) > 0
064000090206     c                   eval       SKout_Errori(sgm) = 'E'
064100090206     c                   eval      SKout_DesErr(sgm) = ERR_inCAMPO +
064200090206     c                             SKsgm_Campo(sgm) + ERR_AlfaNUM
064300090206     c                   eval      esito = 'E'
064400090206     c                   end
064500090206     c                   Enddo
064600090206      **
064700090206     c                   Endsr
064800090206      * ?------------------------------------------------------------------ */
064900090206      *   DEFINIZIONE CHIAVI                               *
065000090206      *----------------------------------------------------------------
065100090206     C     *INZSR        BEGSR
065200090206      *------------------
065300971216      * Recupero data e ora
065400971216     C                   TIME                    WORA
065500991124     C                   TIME                    W0140
065600991124      * UDATE IN GGMMAAAA
065700991124     C                   MOVE      W0140         WDTGIO
065800991124      * UDATE IN AAAAMMGG
065900991124     C     *eur          MOVEL     WDTGIO        DATA_eur
066000991124     C     *iso          MOVEL     DATA_eur      dateu
066100050414      *
066200991124     C                   ENDSR
066300060621      * ?------------------------------------------------------------------ */
066400060621      *?      X non bloccare in nessun caso il traduttore CLIENTI
066500060621      * ?------------------------------------------------------------------ */
066600060621     C     *pssr         BEGSR
066700060621     C
066800090205     C                   eval      esito = 'E'
066900060621     C                   ENDSR     '*CANCL'
067000060621     C
067100090204      *  ------------------------------------------------------------------ */
