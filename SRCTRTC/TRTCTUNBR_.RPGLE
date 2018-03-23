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
002800090209  ** D  DSsegmento   e ds                  EXTNAME(EDS00UNB)
002900090209  ** d    campo_01   e                     extFLD(UNB0001)
003000090209  ** d    campo_02   e                     extFLD(UNB0002)
003100090209  ** d    campo_03   e                     extFLD(UNB0004)
003200090209  ** d    campo_04   e                     extFLD(UNB000720)
003300090209  ** d    campo_05   e                     extFLD(UNB0008)
003400090209  ** d    campo_06   e                     extFLD(UNB0010)
003500090209  ** d    campo_07   e                     extFLD(UNB000730)
003600090209  ** d    campo_08   e                     extFLD(UNB0014)
003700090209  ** d    campo_09   e                     extFLD(UNB0017)
003800090209  ** d    campo_10   e                     extFLD(UNB0019)
003900090209  ** d    campo_11   e                     extFLD(UNB0020)
004000090209  ** d    campo_12   e                     extFLD(UNB0022)
004100090209  ** d    campo_13   e                     extFLD(UNB0025)
004200090209  ** d    campo_14   e                     extFLD(UNB0026)
004300090209  ** d    campo_15   e                     extFLD(UNB0029)
004400090209  ** d    campo_16   e                     extFLD(UNB0031)
004500090209  ** d    campo_17   e                     extFLD(UNB0032)
004600090209  ** d    campo_18   e                     extFLD(UNB0035)
004700090206      *----------------------------------------------------*
004800090206     d  esito          s              1
004900090206     D Campi_segmento  ds
005000090206     D  SKsgm_Campo                   4    DIM(50)
005100090206     D Campi_obbligat  ds
005200090206     D  SKsgm_Obbligo                 1    DIM(50)
005300090206     D Campi_AlfaNum   ds
005400090206     D  SKsgm_AlfaNum                 1    DIM(50)
005500090206     D Livello_Campi   ds
005600090206     D  SKsgm_Livello                 1s 0 DIM(50)
005700090206     D  SKsgm_Lungo    s              5s 0 DIM(50)
005800090206      *----
005900090206     D Valore_campi    ds
006000090206     D  SKout_valFLD               1000    DIM(50)
006100090206     D Lung_Valori     ds
006200090206     D  SKout_Lungo                   5s 0 DIM(50)
006300090206     D Valori_Alfanum  ds
006400090206     D  SKout_AlfaNum                 1    DIM(50)
006500090206     D Valori_inErr    ds
006600090206     D  SKout_Errori                  1    DIM(50)
006700090206     D Descr_Errore    ds
006800090206     D  SKout_DesErr                 50    DIM(50)
006900090206      *
007000090206     D  SKout_Diviso   s              1    DIM(50)
007100090206     D  SKout_Decimal  s              1s 0 DIM(50)
007200050713      * ?------------------------------------------------------------------ */
007300050629     D Digits          C                   CONST('0123456789')
007400050628      * ?------------------------------------------------------------------ */
007500050628     c     *entry        plist
007600050629      * ?     Input
007700090206     c                   parm                    Tipo_segmento
007800090206     c                   parm                    Segmento
007900100716     C                   parm                    keyUNBCLI
008000100716     C                   parm                    keyTIPOMSG
008100100716     C                   parm                    keyVERSION
008200100716     C                   parm                    keyRELEASE
008300100716     C                   parm                    keyAGENCY
008400100716     C                   parm                    keyASSOCIA
008500050629      * ?     Output
008600090206     c                   parm                    Esito
008700090206     c                   parm                    DSsegmento
008800090206     c                   parm                    Valori_inErr
008900090206     c                   parm                    Descr_Errore
009000050713      **
009100050713      ** Pulizia DS da restituire
009200090206     c                   clear                   Esito
009300090206     c                   clear                   DSsegmento
009400090206     c                   clear                   Valori_inErr
009500090206     c                   clear                   Descr_Errore
009600050629      **
009700090206     c                   clear                   SKsgm_Campo
009800090206     c                   clear                   SKsgm_Livello
009900090206     c                   clear                   SKsgm_Obbligo
010000090206     c                   clear                   SKsgm_AlfaNum
010100090206     c                   clear                   SKsgm_Lungo
010200090206      **
010300090206     c                   clear                   SKout_ValFld
010400090206     c                   clear                   SKout_AlfaNum
010500090206     c                   clear                   SKout_Lungo
010600090206     c                   clear                   SKout_Decimal
010700090206     c                   clear                   SKout_Errori
010800090206     c                   clear                   SKout_DesErr
010900090206      **
011000090209     c                   CALL      'TRTCT01R'
011100090206      * in
011200090206     C                   parm                    Segmento
011300100716     C                   parm                    keyUNBCLI
011400100716     C                   parm                    keyTIPOMSG
011500100716     C                   parm                    keyVERSION
011600100716     C                   parm                    keyRELEASE
011700100716     C                   parm                    keyAGENCY
011800100716     C                   parm                    keyASSOCIA
011900090206      * out
012000090206     C                   parm                    esito
012100090206     C                   parm                    Tipo_segmento
012200090206     C                   parm                    Campi_Segmento
012300090206     C                   parm                    Campi_Obbligat
012400090206     C                   parm                    Campi_AlfaNum
012500090206     C                   parm                    Livello_Campi
012600090206     C                   parm                    Valore_Campi
012700090206     C                   parm                    Lung_Valori
012800090206     C                   parm                    Valori_Alfanum
012900090206     C                   parm                    Valori_inErr
013000090206     C                   parm                    Descr_Errore
013100090206      **
013200090206      *   se c'è stato un caso di errore non deve caricare il campo errato
013300090206      **
013400090206     c                   z-add     0             Nr_fld            3 0
013500090206      *  max 50 campi
013600090206     c                   do        50            sgm               3 0
013700090206      *
013800090206     c                   if        SKsgm_CAMPO(sgm) = *blank
013900090206     c                   leave
014000090206     c                   end
014100090206      **
014200090206      **  solo se non ci sono errori sul campo specifico
014300090206     c                   if        SKout_Errori(sgm) = *blank
014400090206     c                   if        SKsgm_AlfaNUM(sgm) ='A' or
014500090206     c                             SKsgm_AlfaNUM(sgm) ='N'
014600090206     c                   add       1             Nr_fld
014700090206     c                   EXSR      CARICA_DS
014800090206     c                   end
014900090206     c                   end
015000090206      *
015100090206     c                   endDo
015200090206      *
015300090212     c                   seton                                        RT
015400050628      * ?------------------------------------------------------------------ */
015500090206 **  c     CARICA_DS     begsr
015600090206      *
015700090211     c                   if        SKout_ValFld(sgm) <> *blank
015800090206     c                   select
015900090206 **  c                   when      Nr_fld   = 1
016000090209 **  c                   eval      campo_01 = SKout_ValFld(sgm)
016100090206     c                   when      Nr_fld   = 2
016200090209     c                   eval      campo_02 = %int(%trimR(SKout_ValFld(sgm)))
016300090206     c                   when      Nr_fld   = 3
016400090206     c                   eval      campo_03 = SKout_ValFld(sgm)
016500101005     c                   movel(p)  campo_03      keyUNBCLI
016600090209     c                   when      Nr_fld   = 4
016700090209     c                   eval      campo_04 = SKout_ValFld(sgm)
016800101005     c                   move      campo_04      keyUNBCLI
016900090209     c                   when      Nr_fld   = 5
017000090209     c                   eval      campo_05 = SKout_ValFld(sgm)
017100090209     c                   when      Nr_fld   = 6
017200090209     c                   eval      campo_06 = SKout_ValFld(sgm)
017300090209     c                   when      Nr_fld   = 7
017400090209     c                   eval      campo_07 = SKout_ValFld(sgm)
017500090209     c                   when      Nr_fld   = 8
017600090209     c                   eval      campo_08 = SKout_ValFld(sgm)
017700090209     c                   when      Nr_fld   = 9
017800090209     c                   eval      campo_09 = %int(%trimR(SKout_ValFld(sgm)))
017900090209     c                   when      Nr_fld   = 10
018000090209     c                   eval      campo_10 = %int(%trimR(SKout_ValFld(sgm)))
018100090209     c                   when      Nr_fld   = 11
018200090209     c                   eval      campo_11 = SKout_ValFld(sgm)
018300090209     c                   when      Nr_fld   = 12
018400090209     c                   eval      campo_12 = SKout_ValFld(sgm)
018500090209     c                   when      Nr_fld   = 13
018600090209     c                   eval      campo_13 = SKout_ValFld(sgm)
018700090209     c                   when      Nr_fld   = 14
018800090209     c                   eval      campo_14 = SKout_ValFld(sgm)
018900090209     c                   when      Nr_fld   = 15
019000090209     c                   eval      campo_15 = SKout_ValFld(sgm)
019100090209     c                   when      Nr_fld   = 16
019200090209     c                   eval      campo_16 = %int(%trimR(SKout_ValFld(sgm)))
019300090209     c                   when      Nr_fld   = 17
019400090209     c                   eval      campo_17 = SKout_ValFld(sgm)
019500090209     c                   when      Nr_fld   = 18
019600090209     c                   eval      campo_18 = %int(%trimR(SKout_ValFld(sgm)))
019700090206     c                   endsl
019800090211     c                   end
019900090206      *
020000090206     c                   endsr
020100090206      * ?------------------------------------------------------------------ */
020200110621      * ?------------------------------------------------------------------ */
020300110621      *?      X non bloccare in nessun caso il traduttore CLIENTI
020400110621      * ?------------------------------------------------------------------ */
020500110621     C     *pssr         BEGSR
020600110621     C
020700110621     C                   eval      esito = 'E'
020800110621     C                   ENDSR     '*CANCL'
020900110621     C
021000110621      *  ------------------------------------------------------------------ */
