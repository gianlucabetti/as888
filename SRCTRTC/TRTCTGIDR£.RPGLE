000100090202      *-------------------------------------------------------------------------------
000200090206      * Riceve il tipo Segmento
000300090206      *        il segmento da suddividere e da impostare nella relativa DS
000400090202      * ------------------------------------------------------------------------
000500090202      * Riceve:
000600090206      *  il Tipo Segmento
000700090206      *  la stringa da suddividere       in  DATI
000800090202      *
000900090202      * Restituisce:
001000090206      *  la DS relativa al segmento esaminato
001100090206      *  l'esito
001200090206      *  la schiera degli errori
001300090206      *  la schiera della descrizione degli errori
001400090206      *
001500090202      *-------------------------------------------------------------------------------
001600050713      * ?Definizioni x gestire lo split del record passato
001700050713      * ?Dati di Input  (Record ricevuto dal pgm chiamante e da splittare)
001800090206     d  Tipo_segmento  s              3
001900100716      *
002000100716     d keyUNBCLI       s             35
002100100716     d keyTIPOMSG      s              6
002200100716     d keyVERSION      s              3
002300100716     d keyRELEASE      s              3
002400100716     d keyAGENCY       s              3
002500100716     d keyASSOCIA      s              6
002600100716      *
002700090206     d  Segmento       s           2048
002800090209  ** D  DSsegmento   e ds                  EXTNAME(EDS00GID)
002900090209  ** d    campo_01   e                     extFLD(GID1496)
003000090209  ** d    campo_02   e                     extFLD(GID722420)
003100090209  ** d    campo_03   e                     extFLD(GID706520)
003200090209  ** d    campo_04   e                     extFLD(GID113120)
003300090209  ** d    campo_05   e                     extFLD(GID305520)
003400090209  ** d    campo_06   e                     extFLD(GID706420)
003500090209  ** d    campo_07   e                     extFLD(GID723320)
003600090209  ** d    campo_08   e                     extFLD(GID722430)
003700090209  ** d    campo_09   e                     extFLD(GID706530)
003800090209  ** d    campo_10   e                     extFLD(GID113130)
003900090209  ** d    campo_11   e                     extFLD(GID305530)
004000090209  ** d    campo_12   e                     extFLD(GID706430)
004100090209  ** d    campo_13   e                     extFLD(GID723330)
004200090209  ** d    campo_14   e                     extFLD(GID722440)
004300090209  ** d    campo_15   e                     extFLD(GID706540)
004400090209  ** d    campo_16   e                     extFLD(GID113140)
004500090209  ** d    campo_17   e                     extFLD(GID305540)
004600090209  ** d    campo_18   e                     extFLD(GID706440)
004700090209  ** d    campo_19   e                     extFLD(GID723340)
004800090209  ** d    campo_20   e                     extFLD(GID722450)
004900090209  ** d    campo_21   e                     extFLD(GID706550)
005000090209  ** d    campo_22   e                     extFLD(GID113150)
005100090209  ** d    campo_23   e                     extFLD(GID305550)
005200090209  ** d    campo_24   e                     extFLD(GID706450)
005300090209  ** d    campo_25   e                     extFLD(GID723350)
005400090209  ** d    campo_26   e                     extFLD(GID722460)
005500090209  ** d    campo_27   e                     extFLD(GID706560)
005600090209  ** d    campo_28   e                     extFLD(GID113160)
005700090209  ** d    campo_29   e                     extFLD(GID305560)
005800090209  ** d    campo_30   e                     extFLD(GID706460)
005900090209  ** d    campo_31   e                     extFLD(GID723360)
006000090206      *----------------------------------------------------*
006100090206     d  esito          s              1
006200090206     D Campi_segmento  ds
006300090206     D  SKsgm_Campo                   4    DIM(50)
006400090206     D Campi_obbligat  ds
006500090206     D  SKsgm_Obbligo                 1    DIM(50)
006600090206     D Campi_AlfaNum   ds
006700090206     D  SKsgm_AlfaNum                 1    DIM(50)
006800090206     D Livello_Campi   ds
006900090206     D  SKsgm_Livello                 1s 0 DIM(50)
007000090206     D  SKsgm_Lungo    s              5s 0 DIM(50)
007100090206      *----
007200090206     D Valore_campi    ds
007300090206     D  SKout_valFLD               1000    DIM(50)
007400090206     D Lung_Valori     ds
007500090206     D  SKout_Lungo                   5s 0 DIM(50)
007600090206     D Valori_Alfanum  ds
007700090206     D  SKout_AlfaNum                 1    DIM(50)
007800090206     D Valori_inErr    ds
007900090206     D  SKout_Errori                  1    DIM(50)
008000090206     D Descr_Errore    ds
008100090206     D  SKout_DesErr                 50    DIM(50)
008200090206      *
008300090206     D  SKout_Diviso   s              1    DIM(50)
008400090206     D  SKout_Decimal  s              1s 0 DIM(50)
008500050713      * ?------------------------------------------------------------------ */
008600050629     D Digits          C                   CONST('0123456789')
008700050628      * ?------------------------------------------------------------------ */
008800050628     c     *entry        plist
008900050629      * ?     Input
009000090206     c                   parm                    Tipo_segmento
009100090206     c                   parm                    Segmento
009200100716     C                   parm                    keyUNBCLI
009300100716     C                   parm                    keyTIPOMSG
009400100716     C                   parm                    keyVERSION
009500100716     C                   parm                    keyRELEASE
009600100716     C                   parm                    keyAGENCY
009700100716     C                   parm                    keyASSOCIA
009800050629      * ?     Output
009900090206     c                   parm                    Esito
010000090206     c                   parm                    DSsegmento
010100090206     c                   parm                    Valori_inErr
010200090206     c                   parm                    Descr_Errore
010300050713      **
010400050713      ** Pulizia DS da restituire
010500090206     c                   clear                   Esito
010600090206     c                   clear                   DSsegmento
010700090206     c                   clear                   Valori_inErr
010800090206     c                   clear                   Descr_Errore
010900050629      **
011000090206     c                   clear                   SKsgm_Campo
011100090206     c                   clear                   SKsgm_Livello
011200090206     c                   clear                   SKsgm_Obbligo
011300090206     c                   clear                   SKsgm_AlfaNum
011400090206     c                   clear                   SKsgm_Lungo
011500090206      **
011600090206     c                   clear                   SKout_ValFld
011700090206     c                   clear                   SKout_AlfaNum
011800090206     c                   clear                   SKout_Lungo
011900090206     c                   clear                   SKout_Decimal
012000090206     c                   clear                   SKout_Errori
012100090206     c                   clear                   SKout_DesErr
012200090206      **
012300090209     c                   CALL      'TRTCT01R'
012400090206      * in
012500090206     C                   parm                    Segmento
012600100716     C                   parm                    keyUNBCLI
012700100716     C                   parm                    keyTIPOMSG
012800100716     C                   parm                    keyVERSION
012900100716     C                   parm                    keyRELEASE
013000100716     C                   parm                    keyAGENCY
013100100716     C                   parm                    keyASSOCIA
013200090206      * out
013300090206     C                   parm                    esito
013400090206     C                   parm                    Tipo_segmento
013500090206     C                   parm                    Campi_Segmento
013600090206     C                   parm                    Campi_Obbligat
013700090206     C                   parm                    Campi_AlfaNum
013800090206     C                   parm                    Livello_Campi
013900090206     C                   parm                    Valore_Campi
014000090206     C                   parm                    Lung_Valori
014100090206     C                   parm                    Valori_Alfanum
014200090206     C                   parm                    Valori_inErr
014300090206     C                   parm                    Descr_Errore
014400090206      **
014500090206      *   se c'è stato un caso di errore non deve caricare il campo errato
014600090206      **
014700090206     c                   z-add     0             Nr_fld            3 0
014800090206      *  max 50 campi
014900090206     c                   do        50            sgm               3 0
015000090206      *
015100090206     c                   if        SKsgm_CAMPO(sgm) = *blank
015200090206     c                   leave
015300090206     c                   end
015400090206      **
015500090206      **  solo se non ci sono errori sul campo specifico
015600090206     c                   if        SKout_Errori(sgm) = *blank
015700090206     c                   if        SKsgm_AlfaNUM(sgm) ='A' or
015800090206     c                             SKsgm_AlfaNUM(sgm) ='N'
015900090206     c                   add       1             Nr_fld
016000090206     c                   EXSR      CARICA_DS
016100090206     c                   end
016200090206     c                   end
016300090206      *
016400090206     c                   endDo
016500090206      *
016600090212     c                   seton                                        RT
016700050628      * ?------------------------------------------------------------------ */
016800090206 **  c     CARICA_DS     begsr
016900090206      *
017000090211     c                   if        SKout_ValFld(sgm) <> *blank
017100090206     c                   select
017200090206 **  c                   when      Nr_fld   = 1
017300090209 **  c                   eval      campo_01 = %int(%trimR(SKout_ValFld(sgm)))
017400090206     c                   when      Nr_fld   = 2
017500090209     c                   eval      campo_02 = %int(%trimR(SKout_ValFld(sgm)))
017600090206     c                   when      Nr_fld   = 3
017700090206     c                   eval      campo_03 = SKout_ValFld(sgm)
017800090209     c                   when      Nr_fld   = 4
017900090209     c                   eval      campo_04 = SKout_ValFld(sgm)
018000090209     c                   when      Nr_fld   = 5
018100090209     c                   eval      campo_05 = SKout_ValFld(sgm)
018200090209     c                   when      Nr_fld   = 6
018300090209     c                   eval      campo_06 = SKout_ValFld(sgm)
018400090209     c                   when      Nr_fld   = 7
018500090209     c                   eval      campo_07 = SKout_ValFld(sgm)
018600090209     c                   when      Nr_fld   = 8
018700090209     c                   eval      campo_08 = %int(%trimR(SKout_ValFld(sgm)))
018800090209     c                   when      Nr_fld   = 9
018900090209     c                   eval      campo_09 = SKout_ValFld(sgm)
019000090209     c                   when      Nr_fld   = 10
019100090209     c                   eval      campo_10 = SKout_ValFld(sgm)
019200090209     c                   when      Nr_fld   = 11
019300090209     c                   eval      campo_11 = SKout_ValFld(sgm)
019400090209     c                   when      Nr_fld   = 12
019500090209     c                   eval      campo_12 = SKout_ValFld(sgm)
019600090209     c                   when      Nr_fld   = 13
019700090209     c                   eval      campo_13 = SKout_ValFld(sgm)
019800090209     c                   when      Nr_fld   = 14
019900090209     c                   eval      campo_14 = %int(%trimR(SKout_ValFld(sgm)))
020000090209     c                   when      Nr_fld   = 15
020100090209     c                   eval      campo_15 = SKout_ValFld(sgm)
020200090209     c                   when      Nr_fld   = 16
020300090209     c                   eval      campo_16 = SKout_ValFld(sgm)
020400090209     c                   when      Nr_fld   = 17
020500090209     c                   eval      campo_17 = SKout_ValFld(sgm)
020600090209     c                   when      Nr_fld   = 18
020700090209     c                   eval      campo_18 = SKout_ValFld(sgm)
020800090209     c                   when      Nr_fld   = 19
020900090209     c                   eval      campo_19 = SKout_ValFld(sgm)
021000090209     c                   when      Nr_fld   = 20
021100090209     c                   eval      campo_20 = %int(%trimR(SKout_ValFld(sgm)))
021200090209     c                   when      Nr_fld   = 21
021300090209     c                   eval      campo_21 = SKout_ValFld(sgm)
021400090209     c                   when      Nr_fld   = 22
021500090209     c                   eval      campo_22 = SKout_ValFld(sgm)
021600090209     c                   when      Nr_fld   = 23
021700090209     c                   eval      campo_23 = SKout_ValFld(sgm)
021800090209     c                   when      Nr_fld   = 24
021900090209     c                   eval      campo_24 = SKout_ValFld(sgm)
022000090209     c                   when      Nr_fld   = 25
022100090209     c                   eval      campo_25 = SKout_ValFld(sgm)
022200090209     c                   when      Nr_fld   = 26
022300090209     c                   eval      campo_26 = %int(%trimR(SKout_ValFld(sgm)))
022400090209     c                   when      Nr_fld   = 27
022500090209     c                   eval      campo_27 = SKout_ValFld(sgm)
022600090209     c                   when      Nr_fld   = 28
022700090209     c                   eval      campo_28 = SKout_ValFld(sgm)
022800090209     c                   when      Nr_fld   = 29
022900090209     c                   eval      campo_29 = SKout_ValFld(sgm)
023000090209     c                   when      Nr_fld   = 30
023100090209     c                   eval      campo_30 = SKout_ValFld(sgm)
023200090209     c                   when      Nr_fld   = 31
023300090209     c                   eval      campo_31 = SKout_ValFld(sgm)
023400090206     c                   endsl
023500090211     c                   end
023600090206      *
023700090206     c                   endsr
023800090206      * ?------------------------------------------------------------------ */
023900110621      * ?------------------------------------------------------------------ */
024000110621      *?      X non bloccare in nessun caso il traduttore CLIENTI
024100110621      * ?------------------------------------------------------------------ */
024200110621     C     *pssr         BEGSR
024300110621     C
024400110621     C                   eval      esito = 'E'
024500110621     C                   ENDSR     '*CANCL'
024600110621     C
024700110621      *  ------------------------------------------------------------------ */
