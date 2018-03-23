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
002900090206  ** D  DSsegmento   e ds                  EXTNAME(EDS00UNA)
003000090206  ** d    campo_01   e                     extFLD(UNA1)
003100090206  ** d    campo_02   e                     extFLD(UNA2)
003200090206  ** d    campo_03   e                     extFLD(UNA3)
003300090206  ** d    campo_04   e                     extFLD(UNA4)
003400090206  ** d    campo_05   e                     extFLD(UNA5)
003500090206  ** d    campo_06   e                     extFLD(UNA6)
003600090206      *----------------------------------------------------*
003700090206     d  esito          s              1
003800090206     D Campi_segmento  ds
003900090206     D  SKsgm_Campo                   4    DIM(50)
004000090206     D Campi_obbligat  ds
004100090206     D  SKsgm_Obbligo                 1    DIM(50)
004200090206     D Campi_AlfaNum   ds
004300090206     D  SKsgm_AlfaNum                 1    DIM(50)
004400090206     D Livello_Campi   ds
004500090206     D  SKsgm_Livello                 1s 0 DIM(50)
004600090206     D  SKsgm_Lungo    s              5s 0 DIM(50)
004700090206      *----
004800090206     D Valore_campi    ds
004900090206     D  SKout_valFLD               1000    DIM(50)
005000090206     D Lung_Valori     ds
005100090206     D  SKout_Lungo                   5s 0 DIM(50)
005200090206     D Valori_Alfanum  ds
005300090206     D  SKout_AlfaNum                 1    DIM(50)
005400090206     D Valori_inErr    ds
005500090206     D  SKout_Errori                  1    DIM(50)
005600090206     D Descr_Errore    ds
005700090206     D  SKout_DesErr                 50    DIM(50)
005800090206      *
005900090206     D  SKout_Diviso   s              1    DIM(50)
006000090206     D  SKout_Decimal  s              1s 0 DIM(50)
006100050713      * ?------------------------------------------------------------------ */
006200050629     D Digits          C                   CONST('0123456789')
006300050628      * ?------------------------------------------------------------------ */
006400050628     c     *entry        plist
006500050629      * ?     Input
006600090206     c                   parm                    Tipo_segmento
006700090206     c                   parm                    Segmento
006800100716     C                   parm                    keyUNBCLI
006900100716     C                   parm                    keyTIPOMSG
007000100716     C                   parm                    keyVERSION
007100100716     C                   parm                    keyRELEASE
007200100716     C                   parm                    keyAGENCY
007300100716     C                   parm                    keyASSOCIA
007400050629      * ?     Output
007500090206     c                   parm                    Esito
007600090206     c                   parm                    DSsegmento
007700090206     c                   parm                    Valori_inErr
007800090206     c                   parm                    Descr_Errore
007900050713      **
008000050713      ** Pulizia DS da restituire
008100090206     c                   clear                   Esito
008200090206     c                   clear                   DSsegmento
008300090206     c                   clear                   Valori_inErr
008400090206     c                   clear                   Descr_Errore
008500050629      **
008600090206     c                   clear                   SKsgm_Campo
008700090206     c                   clear                   SKsgm_Livello
008800090206     c                   clear                   SKsgm_Obbligo
008900090206     c                   clear                   SKsgm_AlfaNum
009000090206     c                   clear                   SKsgm_Lungo
009100090206      **
009200090206     c                   clear                   SKout_ValFld
009300090206     c                   clear                   SKout_AlfaNum
009400090206     c                   clear                   SKout_Lungo
009500090206     c                   clear                   SKout_Decimal
009600090206     c                   clear                   SKout_Errori
009700090206     c                   clear                   SKout_DesErr
009800090206      **
009900090209     c                   CALL      'TRTCT01R'
010000090206      * in
010100090206     C                   parm                    Segmento
010200100716     C                   parm                    keyUNBCLI
010300100716     C                   parm                    keyTIPOMSG
010400100716     C                   parm                    keyVERSION
010500100716     C                   parm                    keyRELEASE
010600100716     C                   parm                    keyAGENCY
010700100716     C                   parm                    keyASSOCIA
010800090206      * out
010900090206     C                   parm                    esito
011000090206     C                   parm                    Tipo_segmento
011100090206     C                   parm                    Campi_Segmento
011200090206     C                   parm                    Campi_Obbligat
011300090206     C                   parm                    Campi_AlfaNum
011400090206     C                   parm                    Livello_Campi
011500090206     C                   parm                    Valore_Campi
011600090206     C                   parm                    Lung_Valori
011700090206     C                   parm                    Valori_Alfanum
011800090206     C                   parm                    Valori_inErr
011900090206     C                   parm                    Descr_Errore
012000090206      **
012100090206      *   se c'è stato un caso di errore non deve caricare il campo errato
012200090206      **
012300090206     c                   z-add     0             Nr_fld            3 0
012400090206      *  max 50 campi
012500090206     c                   do        50            sgm               3 0
012600090206      *
012700090206     c                   if        SKsgm_CAMPO(sgm) = *blank
012800090206     c                   leave
012900090206     c                   end
013000090206      **
013100090206      **  solo se non ci sono errori sul campo specifico
013200090206     c                   if        SKout_Errori(sgm) = *blank
013300090206     c                   if        SKsgm_AlfaNUM(sgm) ='A' or
013400090206     c                             SKsgm_AlfaNUM(sgm) ='N'
013500090206     c                   add       1             Nr_fld
013600090206     c                   EXSR      CARICA_DS
013700090206     c                   end
013800090206     c                   end
013900090206      *
014000090206     c                   endDo
014100090206      *
014200090212     c                   seton                                        RT
014300050628      * ?------------------------------------------------------------------ */
014400090206 **  c     CARICA_DS     begsr
014500090206      *
014600090211     c                   if        SKout_ValFld(sgm) <> *blank
014700090206     c                   select
014800090206 **  c                   when      Nr_fld   = 1
014900090206 **  c                   eval      campo_01 = SKout_ValFld(sgm)
015000090206     c                   when      Nr_fld   = 2
015100090206 **  c                   eval      campo_02 = SKout_ValFld(sgm)
015200090206     c                   when      Nr_fld   = 3
015300090206 **  c                   eval      campo_03 = SKout_ValFld(sgm)
015400090206     c                   when      Nr_fld   = 4
015500090206 **  c                   eval      campo_04 = SKout_ValFld(sgm)
015600090206     c                   when      Nr_fld   = 5
015700090206 **  c                   eval      campo_05 = SKout_ValFld(sgm)
015800090206     c                   when      Nr_fld   = 6
015900090206 **  c                   eval      campo_06 = SKout_ValFld(sgm)
016000090206     c                   endsl
016100090211     c                   end
016200090206      *
016300090206     c                   endsr
016400090206      * ?------------------------------------------------------------------ */
016500110621      * ?------------------------------------------------------------------ */
016600110621      *?      X non bloccare in nessun caso il traduttore CLIENTI
016700110621      * ?------------------------------------------------------------------ */
016800110621     C     *pssr         BEGSR
016900110621     C
017000110621     C                   eval      esito = 'E'
017100110621     C                   ENDSR     '*CANCL'
017200110621     C
017300110621      *  ------------------------------------------------------------------ */
