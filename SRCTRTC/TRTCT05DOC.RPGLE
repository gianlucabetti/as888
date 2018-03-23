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
002900090605  ** D  DSsegmento   e ds                  EXTNAME(EDPORDOC)
003000090605  ** d    campo_01   e                     extFLD(DOC1001)
003100090605  ** d    campo_02   e                     extFLD(DOC1131)
003200090605  ** d    campo_03   e                     extFLD(DOC1000)
003300090605  ** d    campo_04   e                     extFLD(DOC3153)
003400090605  ** d    campo_05   e                     extFLD(DOC1220)
003500090605  ** d    campo_06   e                     extFLD(DOC1791)
003600090605  ** d    campo_07   e                     extFLD(DOC1004)
003700090605  ** d    campo_08   e                     extFLD(DOC1788)
003800090206      *----------------------------------------------------*
003900090206     d  esito          s              1
004000090206     D Campi_segmento  ds
004100090206     D  SKsgm_Campo                   4    DIM(50)
004200090206     D Campi_obbligat  ds
004300090206     D  SKsgm_Obbligo                 1    DIM(50)
004400090206     D Campi_AlfaNum   ds
004500090206     D  SKsgm_AlfaNum                 1    DIM(50)
004600090206     D Livello_Campi   ds
004700090206     D  SKsgm_Livello                 1s 0 DIM(50)
004800090206     D  SKsgm_Lungo    s              5s 0 DIM(50)
004900090206      *----
005000090206     D Valore_campi    ds
005100090206     D  SKout_valFLD               1000    DIM(50)
005200090206     D Lung_Valori     ds
005300090206     D  SKout_Lungo                   5s 0 DIM(50)
005400090206     D Valori_Alfanum  ds
005500090206     D  SKout_AlfaNum                 1    DIM(50)
005600090206     D Valori_inErr    ds
005700090206     D  SKout_Errori                  1    DIM(50)
005800090206     D Descr_Errore    ds
005900090206     D  SKout_DesErr                 50    DIM(50)
006000090206      *
006100090206     D  SKout_Diviso   s              1    DIM(50)
006200090206     D  SKout_Decimal  s              1s 0 DIM(50)
006300050713      * ?------------------------------------------------------------------ */
006400050629     D Digits          C                   CONST('0123456789')
006500050628      * ?------------------------------------------------------------------ */
006600050628     c     *entry        plist
006700050629      * ?     Input
006800090206     c                   parm                    Tipo_segmento
006900090206     c                   parm                    Segmento
007000100716     C                   parm                    keyUNBCLI
007100100716     C                   parm                    keyTIPOMSG
007200100716     C                   parm                    keyVERSION
007300100716     C                   parm                    keyRELEASE
007400100716     C                   parm                    keyAGENCY
007500100716     C                   parm                    keyASSOCIA
007600050629      * ?     Output
007700090206     c                   parm                    Esito
007800090206     c                   parm                    DSsegmento
007900090206     c                   parm                    Valori_inErr
008000090206     c                   parm                    Descr_Errore
008100050713      **
008200050713      ** Pulizia DS da restituire
008300090206     c                   clear                   Esito
008400090206     c                   clear                   DSsegmento
008500090206     c                   clear                   Valori_inErr
008600090206     c                   clear                   Descr_Errore
008700050629      **
008800090206     c                   clear                   SKsgm_Campo
008900090206     c                   clear                   SKsgm_Livello
009000090206     c                   clear                   SKsgm_Obbligo
009100090206     c                   clear                   SKsgm_AlfaNum
009200090206     c                   clear                   SKsgm_Lungo
009300090206      **
009400090206     c                   clear                   SKout_ValFld
009500090206     c                   clear                   SKout_AlfaNum
009600090206     c                   clear                   SKout_Lungo
009700090206     c                   clear                   SKout_Decimal
009800090206     c                   clear                   SKout_Errori
009900090206     c                   clear                   SKout_DesErr
010000090206      **
010100090605     c                   CALL      'TRTCT05R1'
010200090206      * in
010300090206     C                   parm                    Segmento
010400100716     C                   parm                    keyUNBCLI
010500100716     C                   parm                    keyTIPOMSG
010600100716     C                   parm                    keyVERSION
010700100716     C                   parm                    keyRELEASE
010800100716     C                   parm                    keyAGENCY
010900100716     C                   parm                    keyASSOCIA
011000090206      * out
011100090206     C                   parm                    esito
011200090206     C                   parm                    Tipo_segmento
011300090206     C                   parm                    Campi_Segmento
011400090206     C                   parm                    Campi_Obbligat
011500090206     C                   parm                    Campi_AlfaNum
011600090206     C                   parm                    Livello_Campi
011700090206     C                   parm                    Valore_Campi
011800090206     C                   parm                    Lung_Valori
011900090206     C                   parm                    Valori_Alfanum
012000090206     C                   parm                    Valori_inErr
012100090206     C                   parm                    Descr_Errore
012200090206      **
012300090206      *   se c'è stato un caso di errore non deve caricare il campo errato
012400090206      **
012500090206     c                   z-add     0             Nr_fld            3 0
012600090206      *  max 50 campi
012700090206     c                   do        50            sgm               3 0
012800090206      *
012900090206     c                   if        SKsgm_CAMPO(sgm) = *blank
013000090206     c                   leave
013100090206     c                   end
013200090206      **
013300090206      **  solo se non ci sono errori sul campo specifico
013400090206     c                   if        SKout_Errori(sgm) = *blank
013500090206     c                   if        SKsgm_AlfaNUM(sgm) ='A' or
013600090206     c                             SKsgm_AlfaNUM(sgm) ='N'
013700090206     c                   add       1             Nr_fld
013800090206     c                   EXSR      CARICA_DS
013900090206     c                   end
014000090206     c                   end
014100090206      *
014200090206     c                   endDo
014300090206      *
014400090212     c                   seton                                        RT
014500050628      * ?------------------------------------------------------------------ */
014600090206 **  c     CARICA_DS     begsr
014700090206      *
014800090211     c                   if        SKout_ValFld(sgm) <> *blank
014900090206     c                   select
015000090206 **  c                   when      Nr_fld   = 1
015100090206 **  c                   eval      campo_01 = SKout_ValFld(sgm)
015200090206     c                   when      Nr_fld   = 2
015300090206 **  c                   eval      campo_02 = SKout_ValFld(sgm)
015400090206     c                   when      Nr_fld   = 3
015500090206 **  c                   eval      campo_03 = SKout_ValFld(sgm)
015600090605     c                   when      Nr_fld   = 4
015700090605 **  c                   eval      campo_04 = SKout_ValFld(sgm)
015800090605     c                   when      Nr_fld   = 5
015900090605 **  c                   eval      campo_05 = %int(%trimR(SKout_ValFld(sgm)))
016000090605     c                   when      Nr_fld   = 6
016100090605 **  c                   eval      campo_06 = SKout_ValFld(sgm)
016200090605     c                   when      Nr_fld   = 7
016300090605 **  c                   eval      campo_07 = SKout_ValFld(sgm)
016400090605     c                   when      Nr_fld   = 8
016500090605 **  c                   eval      campo_08 = SKout_ValFld(sgm)
016600090206     c                   endsl
016700090211     c                   end
016800090206      *
016900090206     c                   endsr
017000090206      * ?------------------------------------------------------------------ */
017100110621      * ?------------------------------------------------------------------ */
017200110621      *?      X non bloccare in nessun caso il traduttore CLIENTI
017300110621      * ?------------------------------------------------------------------ */
017400110621     C     *pssr         BEGSR
017500110621     C
017600110621     C                   eval      esito = 'E'
017700110621     C                   ENDSR     '*CANCL'
017800110621     C
017900110621      *  ------------------------------------------------------------------ */
