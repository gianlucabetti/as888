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
002900100707     d  Stringa2048    s           2048
003000090209  ** D  DSsegmento   e ds                  EXTNAME(EDS00MEA)
003100090209  ** d    campo_01   e                     extFLD(MEA6311)
003200090209  ** d    campo_02   e                     extFLD(MEA6313)
003300090209  ** d    campo_03   e                     extFLD(MEA6321)
003400090209  ** d    campo_04   e                     extFLD(MEA6155)
003500090209  ** d    campo_05   e                     extFLD(MEA6154)
003600090209  ** d    campo_06   e                     extFLD(MEA6411)
003700090209  ** d    campo_07   e                     extFLD(MEA6314)
003800090209  ** d    campo_08   e                     extFLD(MEA6162)
003900090209  ** d    campo_09   e                     extFLD(MEA6152)
004000090209  ** d    campo_10   e                     extFLD(MEA6432)
004100090209  ** d    campo_11   e                     extFLD(MEA7383)
004200090206      *----------------------------------------------------*
004300090206     d  esito          s              1
004400090206     D Campi_segmento  ds
004500090206     D  SKsgm_Campo                   4    DIM(50)
004600090206     D Campi_obbligat  ds
004700090206     D  SKsgm_Obbligo                 1    DIM(50)
004800090206     D Campi_AlfaNum   ds
004900090206     D  SKsgm_AlfaNum                 1    DIM(50)
005000090206     D Livello_Campi   ds
005100090206     D  SKsgm_Livello                 1s 0 DIM(50)
005200090206     D  SKsgm_Lungo    s              5s 0 DIM(50)
005300090206      *----
005400090206     D Valore_campi    ds
005500090206     D  SKout_valFLD               1000    DIM(50)
005600090206     D Lung_Valori     ds
005700090206     D  SKout_Lungo                   5s 0 DIM(50)
005800090206     D Valori_Alfanum  ds
005900090206     D  SKout_AlfaNum                 1    DIM(50)
006000090206     D Valori_inErr    ds
006100090206     D  SKout_Errori                  1    DIM(50)
006200090206     D Descr_Errore    ds
006300090206     D  SKout_DesErr                 50    DIM(50)
006400090206      *
006500090206     D  SKout_Diviso   s              1    DIM(50)
006600090206     D  SKout_Decimal  s              1s 0 DIM(50)
006700050713      * ?------------------------------------------------------------------ */
006800050629     D Digits          C                   CONST('0123456789')
006900050628      * ?------------------------------------------------------------------ */
007000050628     c     *entry        plist
007100050629      * ?     Input
007200090206     c                   parm                    Tipo_segmento
007300090206     c                   parm                    Segmento
007400100716     C                   parm                    keyUNBCLI
007500100716     C                   parm                    keyTIPOMSG
007600100716     C                   parm                    keyVERSION
007700100716     C                   parm                    keyRELEASE
007800100716     C                   parm                    keyAGENCY
007900100716     C                   parm                    keyASSOCIA
008000050629      * ?     Output
008100090206     c                   parm                    Esito
008200090206     c                   parm                    DSsegmento
008300090206     c                   parm                    Valori_inErr
008400090206     c                   parm                    Descr_Errore
008500050713      **
008600050713      ** Pulizia DS da restituire
008700090206     c                   clear                   Esito
008800090206     c                   clear                   DSsegmento
008900090206     c                   clear                   Valori_inErr
009000090206     c                   clear                   Descr_Errore
009100050629      **
009200090206     c                   clear                   SKsgm_Campo
009300090206     c                   clear                   SKsgm_Livello
009400090206     c                   clear                   SKsgm_Obbligo
009500090206     c                   clear                   SKsgm_AlfaNum
009600090206     c                   clear                   SKsgm_Lungo
009700090206      **
009800090206     c                   clear                   SKout_ValFld
009900090206     c                   clear                   SKout_AlfaNum
010000090206     c                   clear                   SKout_Lungo
010100090206     c                   clear                   SKout_Decimal
010200090206     c                   clear                   SKout_Errori
010300090206     c                   clear                   SKout_DesErr
010400090206      **
010500100707      **  Attenzione ai decimali espressi con Virgola anzichè con il Decimal Point
010600100707     c                   clear                   Stringa2048
010700100707     C     ','           scan      Segmento
010800100707     c                   if        %Found
010900100707     C     ',':'.'       XLATE     Segmento      Stringa2048                    * se (,) -> (.)decim
011000100707     c                   eval      Segmento = Stringa2048
011100100707     c                   end
011200100707      **
011300090209     c                   CALL      'TRTCT01R'
011400090206      * in
011500090206     C                   parm                    Segmento
011600100716     C                   parm                    keyUNBCLI
011700100716     C                   parm                    keyTIPOMSG
011800100716     C                   parm                    keyVERSION
011900100716     C                   parm                    keyRELEASE
012000100716     C                   parm                    keyAGENCY
012100100716     C                   parm                    keyASSOCIA
012200090206      * out
012300090206     C                   parm                    esito
012400090206     C                   parm                    Tipo_segmento
012500090206     C                   parm                    Campi_Segmento
012600090206     C                   parm                    Campi_Obbligat
012700090206     C                   parm                    Campi_AlfaNum
012800090206     C                   parm                    Livello_Campi
012900090206     C                   parm                    Valore_Campi
013000090206     C                   parm                    Lung_Valori
013100090206     C                   parm                    Valori_Alfanum
013200090206     C                   parm                    Valori_inErr
013300090206     C                   parm                    Descr_Errore
013400090206      **
013500090206      *   se c'è stato un caso di errore non deve caricare il campo errato
013600090206      **
013700090206     c                   z-add     0             Nr_fld            3 0
013800090206      *  max 50 campi
013900090206     c                   do        50            sgm               3 0
014000090206      *
014100090206     c                   if        SKsgm_CAMPO(sgm) = *blank
014200090206     c                   leave
014300090206     c                   end
014400090206      **
014500090206      **  solo se non ci sono errori sul campo specifico
014600090206     c                   if        SKout_Errori(sgm) = *blank
014700090206     c                   if        SKsgm_AlfaNUM(sgm) ='A' or
014800090206     c                             SKsgm_AlfaNUM(sgm) ='N'
014900090206     c                   add       1             Nr_fld
015000090206     c                   EXSR      CARICA_DS
015100090206     c                   end
015200090206     c                   end
015300090206      *
015400090206     c                   endDo
015500090206      *
015600090212     c                   seton                                        RT
015700050628      * ?------------------------------------------------------------------ */
015800090206 **  c     CARICA_DS     begsr
015900090206      *
016000090211     c                   if        SKout_ValFld(sgm) <> *blank
016100090206     c                   select
016200090206 **  c                   when      Nr_fld   = 1
016300090209 **  c                   eval      campo_01 = SKout_ValFld(sgm)
016400090206     c                   when      Nr_fld   = 2
016500090209     c                   eval      campo_02 = SKout_ValFld(sgm)
016600090206     c                   when      Nr_fld   = 3
016700090206     c                   eval      campo_03 = SKout_ValFld(sgm)
016800090209     c                   when      Nr_fld   = 4
016900090209     c                   eval      campo_04 = SKout_ValFld(sgm)
017000090209     c                   when      Nr_fld   = 5
017100090209     c                   eval      campo_05 = SKout_ValFld(sgm)
017200090209     c                   when      Nr_fld   = 6
017300090209     c                   eval      campo_06 = SKout_ValFld(sgm)
017400090209     c                   when      Nr_fld   = 7
017500090210     c                   eval      campo_07 = %int(%trimR(SKout_ValFld(sgm)))
017600090209     c                   when      Nr_fld   = 8
017700090209     c                   eval      campo_08 = %int(%trimR(SKout_ValFld(sgm)))
017800090209     c                   when      Nr_fld   = 9
017900090209     c                   eval      campo_09 = %int(%trimR(SKout_ValFld(sgm)))
018000090209     c                   when      Nr_fld   = 10
018100090209     c                   eval      campo_10 = %int(%trimR(SKout_ValFld(sgm)))
018200090209     c                   when      Nr_fld   = 11
018300090209     c                   eval      campo_11 = SKout_ValFld(sgm)
018400090206     c                   endsl
018500090211     c                   end
018600090206      *
018700090206     c                   endsr
018800090206      * ?------------------------------------------------------------------ */
018900110621      * ?------------------------------------------------------------------ */
019000110621      *?      X non bloccare in nessun caso il traduttore CLIENTI
019100110621      * ?------------------------------------------------------------------ */
019200110621     C     *pssr         BEGSR
019300110621     C
019400110621     C                   eval      esito = 'E'
019500110621     C                   ENDSR     '*CANCL'
019600110621     C
019700110621      *  ------------------------------------------------------------------ */
