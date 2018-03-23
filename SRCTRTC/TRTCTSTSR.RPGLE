000100121224     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000200090202      *-------------------------------------------------------------------------------
000300090206      * Riceve il tipo Segmento
000400090206      *        il segmento da suddividere e da impostare nella relativa DS
000500090202      * ------------------------------------------------------------------------
000600090202      * Riceve:
000700090206      *  il Tipo Segmento
000800090206      *  la stringa da suddividere       in  DATI
000900090202      *
001000090202      * Restituisce:
001100090206      *  la DS relativa al segmento esaminato
001200090206      *  l'esito
001300090206      *  la schiera degli errori
001400090206      *  la schiera della descrizione degli errori
001500090206      *
001600090202      *-------------------------------------------------------------------------------
001700050713      * ?Definizioni x gestire lo split del record passato
001800050713      * ?Dati di Input  (Record ricevuto dal pgm chiamante e da splittare)
001900090206     d  Tipo_segmento  s              3
002000100716      *
002100100716     d keyUNBCLI       s             35
002200100716     d keyTIPOMSG      s              6
002300100716     d keyVERSION      s              3
002400100716     d keyRELEASE      s              3
002500100716     d keyAGENCY       s              3
002600100716     d keyASSOCIA      s              6
002700100716      *
002800090206     d  Segmento       s           2048
002900100712  ** D  DSsegmento   e ds                  EXTNAME(EDS00STS)
003000100712  ** d    campo_01   e                     extFLD(STS9015)
003100100712  ** d    campo_02   e                     extFLD(STS113110)
003200100712  ** d    campo_03   e                     extFLD(STS305510)
003300100712  ** d    campo_04   e                     extFLD(STS9011)
003400100712  ** d    campo_05   e                     extFLD(STS113120)
003500100712  ** d    campo_06   e                     extFLD(STS305520)
003600100712  ** d    campo_07   e                     extFLD(STS901330)
003700100712  ** d    campo_08   e                     extFLD(STS113130)
003800100712  ** d    campo_09   e                     extFLD(STS305530)
003900100712  ** d    campo_10   e                     extFLD(STS901230)
004000100712  ** d    campo_11   e                     extFLD(STS901340)
004100100712  ** d    campo_12   e                     extFLD(STS113140)
004200100712  ** d    campo_13   e                     extFLD(STS305540)
004300100712  ** d    campo_14   e                     extFLD(STS901240)
004400100712  ** d    campo_15   e                     extFLD(STS901350)
004500100712  ** d    campo_16   e                     extFLD(STS113150)
004600100712  ** d    campo_17   e                     extFLD(STS305550)
004700100712  ** d    campo_18   e                     extFLD(STS901250)
004800100712  ** d    campo_19   e                     extFLD(STS901360)
004900100712  ** d    campo_20   e                     extFLD(STS113160)
005000100712  ** d    campo_21   e                     extFLD(STS305560)
005100100712  ** d    campo_22   e                     extFLD(STS901260)
005200100712  ** d    campo_23   e                     extFLD(STS901370)
005300100712  ** d    campo_24   e                     extFLD(STS113170)
005400100712  ** d    campo_25   e                     extFLD(STS305570)
005500100712  ** d    campo_26   e                     extFLD(STS901270)
005600090206      *----------------------------------------------------*
005700090206     d  esito          s              1
005800090206     D Campi_segmento  ds
005900090206     D  SKsgm_Campo                   4    DIM(50)
006000090206     D Campi_obbligat  ds
006100090206     D  SKsgm_Obbligo                 1    DIM(50)
006200090206     D Campi_AlfaNum   ds
006300090206     D  SKsgm_AlfaNum                 1    DIM(50)
006400090206     D Livello_Campi   ds
006500090206     D  SKsgm_Livello                 1s 0 DIM(50)
006600090206     D  SKsgm_Lungo    s              5s 0 DIM(50)
006700090206      *----
006800090206     D Valore_campi    ds
006900090206     D  SKout_valFLD               1000    DIM(50)
007000090206     D Lung_Valori     ds
007100090206     D  SKout_Lungo                   5s 0 DIM(50)
007200090206     D Valori_Alfanum  ds
007300090206     D  SKout_AlfaNum                 1    DIM(50)
007400090206     D Valori_inErr    ds
007500090206     D  SKout_Errori                  1    DIM(50)
007600090206     D Descr_Errore    ds
007700090206     D  SKout_DesErr                 50    DIM(50)
007800090206      *
007900090206     D  SKout_Diviso   s              1    DIM(50)
008000090206     D  SKout_Decimal  s              1s 0 DIM(50)
008100050713      * ?------------------------------------------------------------------ */
008200050629     D Digits          C                   CONST('0123456789')
008300050628      * ?------------------------------------------------------------------ */
008400050628     c     *entry        plist
008500050629      * ?     Input
008600090206     c                   parm                    Tipo_segmento
008700090206     c                   parm                    Segmento
008800100716     C                   parm                    keyUNBCLI
008900100716     C                   parm                    keyTIPOMSG
009000100716     C                   parm                    keyVERSION
009100100716     C                   parm                    keyRELEASE
009200100716     C                   parm                    keyAGENCY
009300100716     C                   parm                    keyASSOCIA
009400050629      * ?     Output
009500090206     c                   parm                    Esito
009600090206     c                   parm                    DSsegmento
009700090206     c                   parm                    Valori_inErr
009800090206     c                   parm                    Descr_Errore
009900050713      **
010000050713      ** Pulizia DS da restituire
010100090206     c                   clear                   Esito
010200090206     c                   clear                   DSsegmento
010300090206     c                   clear                   Valori_inErr
010400090206     c                   clear                   Descr_Errore
010500050629      **
010600090206     c                   clear                   SKsgm_Campo
010700090206     c                   clear                   SKsgm_Livello
010800090206     c                   clear                   SKsgm_Obbligo
010900090206     c                   clear                   SKsgm_AlfaNum
011000090206     c                   clear                   SKsgm_Lungo
011100090206      **
011200090206     c                   clear                   SKout_ValFld
011300090206     c                   clear                   SKout_AlfaNum
011400090206     c                   clear                   SKout_Lungo
011500090206     c                   clear                   SKout_Decimal
011600090206     c                   clear                   SKout_Errori
011700090206     c                   clear                   SKout_DesErr
011800090206      **
011900090209     c                   CALL      'TRTCT01R'
012000090206      * in
012100090206     C                   parm                    Segmento
012200100716     C                   parm                    keyUNBCLI
012300100716     C                   parm                    keyTIPOMSG
012400100716     C                   parm                    keyVERSION
012500100716     C                   parm                    keyRELEASE
012600100716     C                   parm                    keyAGENCY
012700100716     C                   parm                    keyASSOCIA
012800090206      * out
012900090206     C                   parm                    esito
013000090206     C                   parm                    Tipo_segmento
013100090206     C                   parm                    Campi_Segmento
013200090206     C                   parm                    Campi_Obbligat
013300090206     C                   parm                    Campi_AlfaNum
013400090206     C                   parm                    Livello_Campi
013500090206     C                   parm                    Valore_Campi
013600090206     C                   parm                    Lung_Valori
013700090206     C                   parm                    Valori_Alfanum
013800090206     C                   parm                    Valori_inErr
013900090206     C                   parm                    Descr_Errore
014000090206      **
014100090206      *   se c'è stato un caso di errore non deve caricare il campo errato
014200090206      **
014300090206     c                   z-add     0             Nr_fld            3 0
014400090206      *  max 50 campi
014500090206     c                   do        50            sgm               3 0
014600090206      *
014700090206     c                   if        SKsgm_CAMPO(sgm) = *blank
014800090206     c                   leave
014900090206     c                   end
015000090206      **
015100090206      **  solo se non ci sono errori sul campo specifico
015200090206     c                   if        SKout_Errori(sgm) = *blank
015300090206     c                   if        SKsgm_AlfaNUM(sgm) ='A' or
015400090206     c                             SKsgm_AlfaNUM(sgm) ='N'
015500090206     c                   add       1             Nr_fld
015600090206     c                   EXSR      CARICA_DS
015700090206     c                   end
015800090206     c                   end
015900090206      *
016000090206     c                   endDo
016100090206      *
016200090212     c                   seton                                        RT
016300050628      * ?------------------------------------------------------------------ */
016400090206 **  c     CARICA_DS     begsr
016500090206      *
016600090211     c                   if        SKout_ValFld(sgm) <> *blank
016700090206     c                   select
016800090206 **  c                   when      Nr_fld   = 1
016900090206 **  c                   eval      campo_01 = SKout_ValFld(sgm)
017000090206     c                   when      Nr_fld   = 2
017100090206 **  c                   eval      campo_02 = SKout_ValFld(sgm)
017200090206     c                   when      Nr_fld   = 3
017300090206 **  c                   eval      campo_03 = SKout_ValFld(sgm)
017400090206     c                   when      Nr_fld   = 4
017500090206 **  c                   eval      campo_04 = SKout_ValFld(sgm)
017600090206     c                   when      Nr_fld   = 5
017700090206 **  c                   eval      campo_05 = SKout_ValFld(sgm)
017800090206     c                   when      Nr_fld   = 6
017900090206 **  c                   eval      campo_06 = SKout_ValFld(sgm)
018000090206     c                   when      Nr_fld   = 7
018100090206 **  c                   eval      campo_07 = SKout_ValFld(sgm)
018200090206     c                   when      Nr_fld   = 8
018300090206 **  c                   eval      campo_08 = SKout_ValFld(sgm)
018400090206     c                   when      Nr_fld   = 9
018500090206 **  c                   eval      campo_09 = SKout_ValFld(sgm)
018600090206     c                   when      Nr_fld   = 10
018700090206 **  c                   eval      campo_10 = SKout_ValFld(sgm)
018800090206     c                   when      Nr_fld   = 11
018900090206 **  c                   eval      campo_11 = SKout_ValFld(sgm)
019000090206     c                   when      Nr_fld   = 12
019100090206 **  c                   eval      campo_12 = SKout_ValFld(sgm)
019200090206     c                   when      Nr_fld   = 13
019300090206 **  c                   eval      campo_13 = SKout_ValFld(sgm)
019400090206     c                   when      Nr_fld   = 14
019500090206 **  c                   eval      campo_14 = SKout_ValFld(sgm)
019600090206     c                   when      Nr_fld   = 15
019700090206 **  c                   eval      campo_15 = SKout_ValFld(sgm)
019800090206     c                   when      Nr_fld   = 16
019900090206 **  c                   eval      campo_16 = SKout_ValFld(sgm)
020000090206     c                   when      Nr_fld   = 17
020100090206 **  c                   eval      campo_17 = SKout_ValFld(sgm)
020200090206     c                   when      Nr_fld   = 18
020300090206 **  c                   eval      campo_18 = SKout_ValFld(sgm)
020400090206     c                   when      Nr_fld   = 19
020500090206 **  c                   eval      campo_19 = SKout_ValFld(sgm)
020600090206     c                   when      Nr_fld   = 20
020700090206 **  c                   eval      campo_20 = SKout_ValFld(sgm)
020800090206     c                   when      Nr_fld   = 21
020900090206 **  c                   eval      campo_21 = SKout_ValFld(sgm)
021000090206     c                   when      Nr_fld   = 22
021100090206 **  c                   eval      campo_22 = SKout_ValFld(sgm)
021200100712     c                   when      Nr_fld   = 23
021300100712 **  c                   eval      campo_23 = SKout_ValFld(sgm)
021400100712     c                   when      Nr_fld   = 24
021500100712 **  c                   eval      campo_24 = SKout_ValFld(sgm)
021600100712     c                   when      Nr_fld   = 25
021700100712 **  c                   eval      campo_25 = SKout_ValFld(sgm)
021800100712     c                   when      Nr_fld   = 26
021900100712 **  c                   eval      campo_26 = SKout_ValFld(sgm)
022000090206     c                   endsl
022100090211     c                   end
022200090206      *
022300090206     c                   endsr
022400090206      * ?------------------------------------------------------------------ */
022500110621      * ?------------------------------------------------------------------ */
022600110621      *?      X non bloccare in nessun caso il traduttore CLIENTI
022700110621      * ?------------------------------------------------------------------ */
022800110621     C     *pssr         BEGSR
022900110621     C
023000110621     C                   eval      esito = 'E'
023100110621     C                   ENDSR     '*CANCL'
023200110621     C
023300110621      *  ------------------------------------------------------------------ */
