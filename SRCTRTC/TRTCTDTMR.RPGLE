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
002900090206  ** D  DSsegmento   e ds                  EXTNAME(EDS00DTM)
003000090206  ** d    campo_01   e                     extFLD(DTM2005)
003100090206  ** d    campo_02   e                     extFLD(DTM2380)
003200090206  ** d    campo_03   e                     extFLD(DTM2379)
003300090206      *----------------------------------------------------*
003400090206     d  esito          s              1
003500090206     D Campi_segmento  ds
003600090206     D  SKsgm_Campo                   4    DIM(50)
003700090206     D Campi_obbligat  ds
003800090206     D  SKsgm_Obbligo                 1    DIM(50)
003900090206     D Campi_AlfaNum   ds
004000090206     D  SKsgm_AlfaNum                 1    DIM(50)
004100090206     D Livello_Campi   ds
004200090206     D  SKsgm_Livello                 1s 0 DIM(50)
004300090206     D  SKsgm_Lungo    s              5s 0 DIM(50)
004400090206      *----
004500090206     D Valore_campi    ds
004600090206     D  SKout_valFLD               1000    DIM(50)
004700090206     D Lung_Valori     ds
004800090206     D  SKout_Lungo                   5s 0 DIM(50)
004900090206     D Valori_Alfanum  ds
005000090206     D  SKout_AlfaNum                 1    DIM(50)
005100090206     D Valori_inErr    ds
005200090206     D  SKout_Errori                  1    DIM(50)
005300090206     D Descr_Errore    ds
005400090206     D  SKout_DesErr                 50    DIM(50)
005500090206      *
005600090206     D  SKout_Diviso   s              1    DIM(50)
005700090206     D  SKout_Decimal  s              1s 0 DIM(50)
005800050713      * ?------------------------------------------------------------------ */
005900050629     D Digits          C                   CONST('0123456789')
006000050628      * ?------------------------------------------------------------------ */
006100050628     c     *entry        plist
006200050629      * ?     Input
006300090206     c                   parm                    Tipo_segmento
006400090206     c                   parm                    Segmento
006500100716     C                   parm                    keyUNBCLI
006600100716     C                   parm                    keyTIPOMSG
006700100716     C                   parm                    keyVERSION
006800100716     C                   parm                    keyRELEASE
006900100716     C                   parm                    keyAGENCY
007000100716     C                   parm                    keyASSOCIA
007100050629      * ?     Output
007200090206     c                   parm                    Esito
007300090206     c                   parm                    DSsegmento
007400090206     c                   parm                    Valori_inErr
007500090206     c                   parm                    Descr_Errore
007600050713      **
007700050713      ** Pulizia DS da restituire
007800090206     c                   clear                   Esito
007900090206     c                   clear                   DSsegmento
008000090206     c                   clear                   Valori_inErr
008100090206     c                   clear                   Descr_Errore
008200050629      **
008300090206     c                   clear                   SKsgm_Campo
008400090206     c                   clear                   SKsgm_Livello
008500090206     c                   clear                   SKsgm_Obbligo
008600090206     c                   clear                   SKsgm_AlfaNum
008700090206     c                   clear                   SKsgm_Lungo
008800090206      **
008900090206     c                   clear                   SKout_ValFld
009000090206     c                   clear                   SKout_AlfaNum
009100090206     c                   clear                   SKout_Lungo
009200090206     c                   clear                   SKout_Decimal
009300090206     c                   clear                   SKout_Errori
009400090206     c                   clear                   SKout_DesErr
009500090206      **
009600090209     c                   CALL      'TRTCT01R'
009700090206      * in
009800090206     C                   parm                    Segmento
009900100716     C                   parm                    keyUNBCLI
010000100716     C                   parm                    keyTIPOMSG
010100100716     C                   parm                    keyVERSION
010200100716     C                   parm                    keyRELEASE
010300100716     C                   parm                    keyAGENCY
010400100716     C                   parm                    keyASSOCIA
010500090206      * out
010600090206     C                   parm                    esito
010700090206     C                   parm                    Tipo_segmento
010800090206     C                   parm                    Campi_Segmento
010900090206     C                   parm                    Campi_Obbligat
011000090206     C                   parm                    Campi_AlfaNum
011100090206     C                   parm                    Livello_Campi
011200090206     C                   parm                    Valore_Campi
011300090206     C                   parm                    Lung_Valori
011400090206     C                   parm                    Valori_Alfanum
011500090206     C                   parm                    Valori_inErr
011600090206     C                   parm                    Descr_Errore
011700090206      **
011800090206      *   se c'è stato un caso di errore non deve caricare il campo errato
011900090206      **
012000090206     c                   z-add     0             Nr_fld            3 0
012100090206      *  max 50 campi
012200090206     c                   do        50            sgm               3 0
012300090206      *
012400090206     c                   if        SKsgm_CAMPO(sgm) = *blank
012500090206     c                   leave
012600090206     c                   end
012700090206      **
012800090206      **  solo se non ci sono errori sul campo specifico
012900090206     c                   if        SKout_Errori(sgm) = *blank
013000090206     c                   if        SKsgm_AlfaNUM(sgm) ='A' or
013100090206     c                             SKsgm_AlfaNUM(sgm) ='N'
013200090206     c                   add       1             Nr_fld
013300090206     c                   EXSR      CARICA_DS
013400090206     c                   end
013500090206     c                   end
013600090206      *
013700090206     c                   endDo
013800090206      *
013900090212     c                   seton                                        RT
014000050628      * ?------------------------------------------------------------------ */
014100090206 **  c     CARICA_DS     begsr
014200090206      *
014300090211     c                   if        SKout_ValFld(sgm) <> *blank
014400090206     c                   select
014500090206 **  c                   when      Nr_fld   = 1
014600090206 **  c                   eval      campo_01 = SKout_ValFld(sgm)
014700090206     c                   when      Nr_fld   = 2
014800090206 **  c                   eval      campo_02 = SKout_ValFld(sgm)
014900090206     c                   when      Nr_fld   = 3
015000090206 **  c                   eval      campo_03 = SKout_ValFld(sgm)
015100090206     c                   endsl
015200090211     c                   end
015300090206      *
015400090206     c                   endsr
015500090206      * ?------------------------------------------------------------------ */
015600110621      * ?------------------------------------------------------------------ */
015700110621      *?      X non bloccare in nessun caso il traduttore CLIENTI
015800110621      * ?------------------------------------------------------------------ */
015900110621     C     *pssr         BEGSR
016000110621     C
016100110621     C                   eval      esito = 'E'
016200110621     C                   ENDSR     '*CANCL'
016300110621     C
016400110621      *  ------------------------------------------------------------------ */
