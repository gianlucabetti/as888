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
002800100316  ** D  DSsegmento   e ds                  EXTNAME(EDS00DGS)
002900100316  ** d    campo_01   e                     extFLD(DGS8273)
003000100316  ** d    campo_02   e                     extFLD(DGS8351)
003100100316  ** d    campo_03   e                     extFLD(DGS8078)
003200100316  ** d    campo_04   e                     extFLD(DGS8092)
003300100316  ** d    campo_05   e                     extFLD(DGS7124)
003400100316  ** d    campo_06   e                     extFLD(DGS7088)
003500090206      *----------------------------------------------------*
003600090206     d  esito          s              1
003700090206     D Campi_segmento  ds
003800090206     D  SKsgm_Campo                   4    DIM(50)
003900090206     D Campi_obbligat  ds
004000090206     D  SKsgm_Obbligo                 1    DIM(50)
004100090206     D Campi_AlfaNum   ds
004200090206     D  SKsgm_AlfaNum                 1    DIM(50)
004300090206     D Livello_Campi   ds
004400090206     D  SKsgm_Livello                 1s 0 DIM(50)
004500090206     D  SKsgm_Lungo    s              5s 0 DIM(50)
004600090206      *----
004700090206     D Valore_campi    ds
004800090206     D  SKout_valFLD               1000    DIM(50)
004900090206     D Lung_Valori     ds
005000090206     D  SKout_Lungo                   5s 0 DIM(50)
005100090206     D Valori_Alfanum  ds
005200090206     D  SKout_AlfaNum                 1    DIM(50)
005300090206     D Valori_inErr    ds
005400090206     D  SKout_Errori                  1    DIM(50)
005500090206     D Descr_Errore    ds
005600090206     D  SKout_DesErr                 50    DIM(50)
005700090206      *
005800090206     D  SKout_Diviso   s              1    DIM(50)
005900090206     D  SKout_Decimal  s              1s 0 DIM(50)
006000050713      * ?------------------------------------------------------------------ */
006100050629     D Digits          C                   CONST('0123456789')
006200050628      * ?------------------------------------------------------------------ */
006300050628     c     *entry        plist
006400050629      * ?     Input
006500090206     c                   parm                    Tipo_segmento
006600090206     c                   parm                    Segmento
006700100716     C                   parm                    keyUNBCLI
006800100716     C                   parm                    keyTIPOMSG
006900100716     C                   parm                    keyVERSION
007000100716     C                   parm                    keyRELEASE
007100100716     C                   parm                    keyAGENCY
007200100716     C                   parm                    keyASSOCIA
007300050629      * ?     Output
007400090206     c                   parm                    Esito
007500090206     c                   parm                    DSsegmento
007600090206     c                   parm                    Valori_inErr
007700090206     c                   parm                    Descr_Errore
007800050713      **
007900050713      ** Pulizia DS da restituire
008000090206     c                   clear                   Esito
008100090206     c                   clear                   DSsegmento
008200090206     c                   clear                   Valori_inErr
008300090206     c                   clear                   Descr_Errore
008400050629      **
008500090206     c                   clear                   SKsgm_Campo
008600090206     c                   clear                   SKsgm_Livello
008700090206     c                   clear                   SKsgm_Obbligo
008800090206     c                   clear                   SKsgm_AlfaNum
008900090206     c                   clear                   SKsgm_Lungo
009000090206      **
009100090206     c                   clear                   SKout_ValFld
009200090206     c                   clear                   SKout_AlfaNum
009300090206     c                   clear                   SKout_Lungo
009400090206     c                   clear                   SKout_Decimal
009500090206     c                   clear                   SKout_Errori
009600090206     c                   clear                   SKout_DesErr
009700090206      **
009800090209     c                   CALL      'TRTCT01R'
009900090206      * in
010000090206     C                   parm                    Segmento
010100100716     C                   parm                    keyUNBCLI
010200100716     C                   parm                    keyTIPOMSG
010300100716     C                   parm                    keyVERSION
010400100716     C                   parm                    keyRELEASE
010500100716     C                   parm                    keyAGENCY
010600100716     C                   parm                    keyASSOCIA
010700090206      * out
010800090206     C                   parm                    esito
010900090206     C                   parm                    Tipo_segmento
011000090206     C                   parm                    Campi_Segmento
011100090206     C                   parm                    Campi_Obbligat
011200090206     C                   parm                    Campi_AlfaNum
011300090206     C                   parm                    Livello_Campi
011400090206     C                   parm                    Valore_Campi
011500090206     C                   parm                    Lung_Valori
011600090206     C                   parm                    Valori_Alfanum
011700090206     C                   parm                    Valori_inErr
011800090206     C                   parm                    Descr_Errore
011900090206      **
012000090206      *   se c'è stato un caso di errore non deve caricare il campo errato
012100090206      **
012200090206     c                   z-add     0             Nr_fld            3 0
012300090206      *  max 50 campi
012400090206     c                   do        50            sgm               3 0
012500090206      *
012600090206     c                   if        SKsgm_CAMPO(sgm) = *blank
012700090206     c                   leave
012800090206     c                   end
012900090206      **
013000090206      **  solo se non ci sono errori sul campo specifico
013100090206     c                   if        SKout_Errori(sgm) = *blank
013200090206     c                   if        SKsgm_AlfaNUM(sgm) ='A' or
013300090206     c                             SKsgm_AlfaNUM(sgm) ='N'
013400090206     c                   add       1             Nr_fld
013500090206     c                   EXSR      CARICA_DS
013600090206     c                   end
013700090206     c                   end
013800090206      *
013900090206     c                   endDo
014000090206      *
014100090212     c                   seton                                        RT
014200050628      * ?------------------------------------------------------------------ */
014300090206 **  c     CARICA_DS     begsr
014400090206      *
014500090211     c                   if         SKout_ValFld(sgm) <> *blank
014600100316      *
014700090206     c                   select
014800090206 **  c                   when      Nr_fld   = 1
014900100316     c                   eval      campo_01 = SKout_ValFld(sgm)
015000090206     c                   when      Nr_fld   = 2
015100100316     c                   eval      campo_02 = SKout_ValFld(sgm)
015200100316     c                   when      Nr_fld   = 3
015300100316     c                   eval      campo_03 = SKout_ValFld(sgm)
015400100316     c                   when      Nr_fld   = 4
015500100316     c                   eval      campo_04 = SKout_ValFld(sgm)
015600100316     c                   when      Nr_fld   = 5
015700100316     c                   eval      campo_05 = %int(%trimR(SKout_ValFld(sgm)))
015800100316     c                   when      Nr_fld   = 6
015900100316     c                   eval      campo_06 = SKout_ValFld(sgm)
016000090206     c                   endsl
016100100316      *
016200090211     c                   end
016300090206      *
016400090206     c                   endsr
016500090206      * ?------------------------------------------------------------------ */
016600110621      * ?------------------------------------------------------------------ */
016700110621      *?      X non bloccare in nessun caso il traduttore CLIENTI
016800110621      * ?------------------------------------------------------------------ */
016900110621     C     *pssr         BEGSR
017000110621     C
017100110621     C                   eval      esito = 'E'
017200110621     C                   ENDSR     '*CANCL'
017300110621     C
017400110621      *  ------------------------------------------------------------------ */
