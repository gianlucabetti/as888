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
002900090206     D  DSsegmento   e ds                  EXTNAME(EDS00BGM)
003000090206  ** d    campo_01   e                     extFLD(BGM1001)
003100090206  ** d    campo_02   e                     extFLD(BGM1131)
003200090206  ** d    campo_03   e                     extFLD(BGM3055)
003300090206  ** d    campo_04   e                     extFLD(BGM1000)
003400090206  ** d    campo_05   e                     extFLD(BGM1004)
003500090206  ** d    campo_06   e                     extFLD(BGM1056)
003600090206  ** d    campo_07   e                     extFLD(BGM1060)
003700090206  ** d    campo_08   e                     extFLD(BGM1225)
003800090206  ** d    campo_09   e                     extFLD(BGM4343)
003900090206      *----------------------------------------------------*
004000090206     d  esito          s              1
004100090206     D Campi_segmento  ds
004200090206     D  SKsgm_Campo                   4    DIM(50)
004300090206     D Campi_obbligat  ds
004400090206     D  SKsgm_Obbligo                 1    DIM(50)
004500090206     D Campi_AlfaNum   ds
004600090206     D  SKsgm_AlfaNum                 1    DIM(50)
004700090206     D Livello_Campi   ds
004800090206     D  SKsgm_Livello                 1s 0 DIM(50)
004900090206     D  SKsgm_Lungo    s              5s 0 DIM(50)
005000090206      *----
005100090206     D Valore_campi    ds
005200090206     D  SKout_valFLD               1000    DIM(50)
005300090206     D Lung_Valori     ds
005400090206     D  SKout_Lungo                   5s 0 DIM(50)
005500090206     D Valori_Alfanum  ds
005600090206     D  SKout_AlfaNum                 1    DIM(50)
005700090206     D Valori_inErr    ds
005800090206     D  SKout_Errori                  1    DIM(50)
005900090206     D Descr_Errore    ds
006000090206     D  SKout_DesErr                 50    DIM(50)
006100090206      *
006200090206     D  SKout_Diviso   s              1    DIM(50)
006300090206     D  SKout_Decimal  s              1s 0 DIM(50)
006400050713      * ?------------------------------------------------------------------ */
006500050629     D Digits          C                   CONST('0123456789')
006600050628      * ?------------------------------------------------------------------ */
006700050628     c     *entry        plist
006800050629      * ?     Input
006900090206     c                   parm                    Tipo_segmento
007000090206     c                   parm                    Segmento
007100100716     C                   parm                    keyUNBCLI
007200100716     C                   parm                    keyTIPOMSG
007300100716     C                   parm                    keyVERSION
007400100716     C                   parm                    keyRELEASE
007500100716     C                   parm                    keyAGENCY
007600100716     C                   parm                    keyASSOCIA
007700050629      * ?     Output
007800090206     c                   parm                    Esito
007900090206     c                   parm                    DSsegmento
008000090206     c                   parm                    Valori_inErr
008100090206     c                   parm                    Descr_Errore
008200050713      **
008300050713      ** Pulizia DS da restituire
008400090206     c                   clear                   Esito
008500090206     c                   clear                   DSsegmento
008600090206     c                   clear                   Valori_inErr
008700090206     c                   clear                   Descr_Errore
008800050629      **
008900090206     c                   clear                   SKsgm_Campo
009000090206     c                   clear                   SKsgm_Livello
009100090206     c                   clear                   SKsgm_Obbligo
009200090206     c                   clear                   SKsgm_AlfaNum
009300090206     c                   clear                   SKsgm_Lungo
009400090206      **
009500090206     c                   clear                   SKout_ValFld
009600090206     c                   clear                   SKout_AlfaNum
009700090206     c                   clear                   SKout_Lungo
009800090206     c                   clear                   SKout_Decimal
009900090206     c                   clear                   SKout_Errori
010000090206     c                   clear                   SKout_DesErr
010100090206      **
010200090209     c                   CALL      'TRTCT01R'
010300090206      * in
010400090206     C                   parm                    Segmento
010500100716     C                   parm                    keyUNBCLI
010600100716     C                   parm                    keyTIPOMSG
010700100716     C                   parm                    keyVERSION
010800100716     C                   parm                    keyRELEASE
010900100716     C                   parm                    keyAGENCY
011000100716     C                   parm                    keyASSOCIA
011100090206      * out
011200090206     C                   parm                    esito
011300090206     C                   parm                    Tipo_segmento
011400090206     C                   parm                    Campi_Segmento
011500090206     C                   parm                    Campi_Obbligat
011600090206     C                   parm                    Campi_AlfaNum
011700090206     C                   parm                    Livello_Campi
011800090206     C                   parm                    Valore_Campi
011900090206     C                   parm                    Lung_Valori
012000090206     C                   parm                    Valori_Alfanum
012100090206     C                   parm                    Valori_inErr
012200090206     C                   parm                    Descr_Errore
012300090206      **
012400090206      *   se c'è stato un caso di errore non deve caricare il campo errato
012500090206      **
012600090206     c                   z-add     0             Nr_fld            3 0
012700090206      *  max 50 campi
012800090206     c                   do        50            sgm               3 0
012900090206      *
013000090206     c                   if        SKsgm_CAMPO(sgm) = *blank
013100090206     c                   leave
013200090206     c                   end
013300090206      **
013400090206      **  solo se non ci sono errori sul campo specifico
013500090206     c                   if        SKout_Errori(sgm) = *blank
013600090206     c                   if        SKsgm_AlfaNUM(sgm) ='A' or
013700090206     c                             SKsgm_AlfaNUM(sgm) ='N'
013800090206     c                   add       1             Nr_fld
013900090206     c                   EXSR      CARICA_DS
014000090206     c                   end
014100090206     c                   end
014200090206      *
014300090206     c                   endDo
014400090206      *
014500090212     c                   seton                                        RT
014600050628      * ?------------------------------------------------------------------ */
014700090206     c     CARICA_DS     begsr
014800090206      *
014900090211     c                   if        SKout_ValFld(sgm) <> *blank
015000090206     c                   select
015100090206     c                   when      Nr_fld   = 1
015200090206     c                   eval      campo_01 = SKout_ValFld(sgm)
015300090206     c                   when      Nr_fld   = 2
015400090206     c                   eval      campo_02 = SKout_ValFld(sgm)
015500090206     c                   when      Nr_fld   = 3
015600090206     c                   eval      campo_03 = SKout_ValFld(sgm)
015700090206     c                   when      Nr_fld   = 4
015800090206     c                   eval      campo_04 = SKout_ValFld(sgm)
015900090206     c                   when      Nr_fld   = 5
016000090206     c                   eval      campo_05 = SKout_ValFld(sgm)
016100090206     c                   when      Nr_fld   = 6
016200090206     c                   eval      campo_06 = SKout_ValFld(sgm)
016300090206     c                   when      Nr_fld   = 7
016400090206     c                   eval      campo_07 = SKout_ValFld(sgm)
016500090206     c                   when      Nr_fld   = 8
016600090206     c                   eval      campo_08 = SKout_ValFld(sgm)
016700090206     c                   when      Nr_fld   = 9
016800090206     c                   eval      campo_09 = SKout_ValFld(sgm)
016900090206     c                   endsl
017000090211     c                   end
017100090206      *
017200090206     c                   endsr
017300090206      * ?------------------------------------------------------------------ */
017400110621      * ?------------------------------------------------------------------ */
017500110621      *?      X non bloccare in nessun caso il traduttore CLIENTI
017600110621      * ?------------------------------------------------------------------ */
017700110621     C     *pssr         BEGSR
017800110621     C
017900110621     C                   eval      esito = 'E'
018000110621     C                   ENDSR     '*CANCL'
018100110621     C
018200110621      *  ------------------------------------------------------------------ */
