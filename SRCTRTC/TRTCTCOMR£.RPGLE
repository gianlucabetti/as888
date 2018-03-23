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
002800090206  ** D  DSsegmento   e ds                  EXTNAME(EDS00COM)
002900090206  ** d    campo_01   e                     extFLD(COM3148)
003000090206  ** d    campo_02   e                     extFLD(COM3155)
003100090206      *----------------------------------------------------*
003200090206     d  esito          s              1
003300090206     D Campi_segmento  ds
003400090206     D  SKsgm_Campo                   4    DIM(50)
003500090206     D Campi_obbligat  ds
003600090206     D  SKsgm_Obbligo                 1    DIM(50)
003700090206     D Campi_AlfaNum   ds
003800090206     D  SKsgm_AlfaNum                 1    DIM(50)
003900090206     D Livello_Campi   ds
004000090206     D  SKsgm_Livello                 1s 0 DIM(50)
004100090206     D  SKsgm_Lungo    s              5s 0 DIM(50)
004200090206      *----
004300090206     D Valore_campi    ds
004400090206     D  SKout_valFLD               1000    DIM(50)
004500090206     D Lung_Valori     ds
004600090206     D  SKout_Lungo                   5s 0 DIM(50)
004700090206     D Valori_Alfanum  ds
004800090206     D  SKout_AlfaNum                 1    DIM(50)
004900090206     D Valori_inErr    ds
005000090206     D  SKout_Errori                  1    DIM(50)
005100090206     D Descr_Errore    ds
005200090206     D  SKout_DesErr                 50    DIM(50)
005300090206      *
005400090206     D  SKout_Diviso   s              1    DIM(50)
005500090206     D  SKout_Decimal  s              1s 0 DIM(50)
005600050713      * ?------------------------------------------------------------------ */
005700050629     D Digits          C                   CONST('0123456789')
005800050628      * ?------------------------------------------------------------------ */
005900050628     c     *entry        plist
006000050629      * ?     Input
006100090206     c                   parm                    Tipo_segmento
006200090206     c                   parm                    Segmento
006300100716     C                   parm                    keyUNBCLI
006400100716     C                   parm                    keyTIPOMSG
006500100716     C                   parm                    keyVERSION
006600100716     C                   parm                    keyRELEASE
006700100716     C                   parm                    keyAGENCY
006800100716     C                   parm                    keyASSOCIA
006900050629      * ?     Output
007000090206     c                   parm                    Esito
007100090206     c                   parm                    DSsegmento
007200090206     c                   parm                    Valori_inErr
007300090206     c                   parm                    Descr_Errore
007400050713      **
007500050713      ** Pulizia DS da restituire
007600090206     c                   clear                   Esito
007700090206     c                   clear                   DSsegmento
007800090206     c                   clear                   Valori_inErr
007900090206     c                   clear                   Descr_Errore
008000050629      **
008100090206     c                   clear                   SKsgm_Campo
008200090206     c                   clear                   SKsgm_Livello
008300090206     c                   clear                   SKsgm_Obbligo
008400090206     c                   clear                   SKsgm_AlfaNum
008500090206     c                   clear                   SKsgm_Lungo
008600090206      **
008700090206     c                   clear                   SKout_ValFld
008800090206     c                   clear                   SKout_AlfaNum
008900090206     c                   clear                   SKout_Lungo
009000090206     c                   clear                   SKout_Decimal
009100090206     c                   clear                   SKout_Errori
009200090206     c                   clear                   SKout_DesErr
009300090206      **
009400090209     c                   CALL      'TRTCT01R'
009500090206      * in
009600090206     C                   parm                    Segmento
009700100716     C                   parm                    keyUNBCLI
009800100716     C                   parm                    keyTIPOMSG
009900100716     C                   parm                    keyVERSION
010000100716     C                   parm                    keyRELEASE
010100100716     C                   parm                    keyAGENCY
010200100716     C                   parm                    keyASSOCIA
010300090206      * out
010400090206     C                   parm                    esito
010500090206     C                   parm                    Tipo_segmento
010600090206     C                   parm                    Campi_Segmento
010700090206     C                   parm                    Campi_Obbligat
010800090206     C                   parm                    Campi_AlfaNum
010900090206     C                   parm                    Livello_Campi
011000090206     C                   parm                    Valore_Campi
011100090206     C                   parm                    Lung_Valori
011200090206     C                   parm                    Valori_Alfanum
011300090206     C                   parm                    Valori_inErr
011400090206     C                   parm                    Descr_Errore
011500090206      **
011600090206      *   se c'è stato un caso di errore non deve caricare il campo errato
011700090206      **
011800090206     c                   z-add     0             Nr_fld            3 0
011900090206      *  max 50 campi
012000090206     c                   do        50            sgm               3 0
012100090206      *
012200090206     c                   if        SKsgm_CAMPO(sgm) = *blank
012300090206     c                   leave
012400090206     c                   end
012500090206      **
012600090206      **  solo se non ci sono errori sul campo specifico
012700090206     c                   if        SKout_Errori(sgm) = *blank
012800090206     c                   if        SKsgm_AlfaNUM(sgm) ='A' or
012900090206     c                             SKsgm_AlfaNUM(sgm) ='N'
013000090206     c                   add       1             Nr_fld
013100090206     c                   EXSR      CARICA_DS
013200090206     c                   end
013300090206     c                   end
013400090206      *
013500090206     c                   endDo
013600090206      *
013700090212     c                   seton                                        RT
013800050628      * ?------------------------------------------------------------------ */
013900090206 **  c     CARICA_DS     begsr
014000090206      *
014100090211     c                   if        SKout_ValFld(sgm) <> *blank
014200090206     c                   select
014300090206 **  c                   when      Nr_fld   = 1
014400090206 **  c                   eval      campo_01 = SKout_ValFld(sgm)
014500090206     c                   when      Nr_fld   = 2
014600090206 **  c                   eval      campo_02 = SKout_ValFld(sgm)
014700090206     c                   endsl
014800090211     c                   end
014900090206      *
015000090206     c                   endsr
015100090206      * ?------------------------------------------------------------------ */
015200110621      * ?------------------------------------------------------------------ */
015300110621      *?      X non bloccare in nessun caso il traduttore CLIENTI
015400110621      * ?------------------------------------------------------------------ */
015500110621     C     *pssr         BEGSR
015600110621     C
015700110621     C                   eval      esito = 'E'
015800110621     C                   ENDSR     '*CANCL'
015900110621     C
016000110621      *  ------------------------------------------------------------------ */
