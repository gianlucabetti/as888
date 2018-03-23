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
002800090206  ** D  DSsegmento   e ds                  EXTNAME(EDS00PCI)
002900090206  ** d    campo_01   e                     extFLD(PCI4233)
003000090206  ** d    campo_02   e                     extFLD(PCI71021)
003100090206  ** d    campo_03   e                     extFLD(PCI71022)
003200090206  ** d    campo_04   e                     extFLD(PCI71023)
003300090206  ** d    campo_05   e                     extFLD(PCI71024)
003400090206  ** d    campo_06   e                     extFLD(PCI71025)
003500090206  ** d    campo_07   e                     extFLD(PCI71026)
003600090206  ** d    campo_08   e                     extFLD(PCI71027)
003700090206  ** d    campo_09   e                     extFLD(PCI71028)
003800090206  ** d    campo_10   e                     extFLD(PCI71029)
003900090206  ** d    campo_11   e                     extFLD(PCI710210)
004000090206  ** d    campo_12   e                     extFLD(PCI8196)
004100090206  ** d    campo_13   e                     extFLD(PCI7511)
004200090206  ** d    campo_14   e                     extFLD(PCI1131)
004300090206  ** d    campo_15   e                     extFLD(PCI3055)
004400090206      *----------------------------------------------------*
004500090206     d  esito          s              1
004600090206     D Campi_segmento  ds
004700090206     D  SKsgm_Campo                   4    DIM(50)
004800090206     D Campi_obbligat  ds
004900090206     D  SKsgm_Obbligo                 1    DIM(50)
005000090206     D Campi_AlfaNum   ds
005100090206     D  SKsgm_AlfaNum                 1    DIM(50)
005200090206     D Livello_Campi   ds
005300090206     D  SKsgm_Livello                 1s 0 DIM(50)
005400090206     D  SKsgm_Lungo    s              5s 0 DIM(50)
005500090206      *----
005600090206     D Valore_campi    ds
005700090206     D  SKout_valFLD               1000    DIM(50)
005800090206     D Lung_Valori     ds
005900090206     D  SKout_Lungo                   5s 0 DIM(50)
006000090206     D Valori_Alfanum  ds
006100090206     D  SKout_AlfaNum                 1    DIM(50)
006200090206     D Valori_inErr    ds
006300090206     D  SKout_Errori                  1    DIM(50)
006400090206     D Descr_Errore    ds
006500090206     D  SKout_DesErr                 50    DIM(50)
006600090206      *
006700090206     D  SKout_Diviso   s              1    DIM(50)
006800090206     D  SKout_Decimal  s              1s 0 DIM(50)
006900050713      * ?------------------------------------------------------------------ */
007000050629     D Digits          C                   CONST('0123456789')
007100050628      * ?------------------------------------------------------------------ */
007200050628     c     *entry        plist
007300050629      * ?     Input
007400090206     c                   parm                    Tipo_segmento
007500090206     c                   parm                    Segmento
007600100716     C                   parm                    keyUNBCLI
007700100716     C                   parm                    keyTIPOMSG
007800100716     C                   parm                    keyVERSION
007900100716     C                   parm                    keyRELEASE
008000100716     C                   parm                    keyAGENCY
008100100716     C                   parm                    keyASSOCIA
008200050629      * ?     Output
008300090206     c                   parm                    Esito
008400090206     c                   parm                    DSsegmento
008500090206     c                   parm                    Valori_inErr
008600090206     c                   parm                    Descr_Errore
008700050713      **
008800050713      ** Pulizia DS da restituire
008900090206     c                   clear                   Esito
009000090206     c                   clear                   DSsegmento
009100090206     c                   clear                   Valori_inErr
009200090206     c                   clear                   Descr_Errore
009300050629      **
009400090206     c                   clear                   SKsgm_Campo
009500090206     c                   clear                   SKsgm_Livello
009600090206     c                   clear                   SKsgm_Obbligo
009700090206     c                   clear                   SKsgm_AlfaNum
009800090206     c                   clear                   SKsgm_Lungo
009900090206      **
010000090206     c                   clear                   SKout_ValFld
010100090206     c                   clear                   SKout_AlfaNum
010200090206     c                   clear                   SKout_Lungo
010300090206     c                   clear                   SKout_Decimal
010400090206     c                   clear                   SKout_Errori
010500090206     c                   clear                   SKout_DesErr
010600090206      **
010700090209     c                   CALL      'TRTCT01R'
010800090206      * in
010900090206     C                   parm                    Segmento
011000100716     C                   parm                    keyUNBCLI
011100100716     C                   parm                    keyTIPOMSG
011200100716     C                   parm                    keyVERSION
011300100716     C                   parm                    keyRELEASE
011400100716     C                   parm                    keyAGENCY
011500100716     C                   parm                    keyASSOCIA
011600090206      * out
011700090206     C                   parm                    esito
011800090206     C                   parm                    Tipo_segmento
011900090206     C                   parm                    Campi_Segmento
012000090206     C                   parm                    Campi_Obbligat
012100090206     C                   parm                    Campi_AlfaNum
012200090206     C                   parm                    Livello_Campi
012300090206     C                   parm                    Valore_Campi
012400090206     C                   parm                    Lung_Valori
012500090206     C                   parm                    Valori_Alfanum
012600090206     C                   parm                    Valori_inErr
012700090206     C                   parm                    Descr_Errore
012800090206      **
012900090206      *   se c'è stato un caso di errore non deve caricare il campo errato
013000090206      **
013100090206     c                   z-add     0             Nr_fld            3 0
013200090206      *  max 50 campi
013300090206     c                   do        50            sgm               3 0
013400090206      *
013500090206     c                   if        SKsgm_CAMPO(sgm) = *blank
013600090206     c                   leave
013700090206     c                   end
013800090206      **
013900090206      **  solo se non ci sono errori sul campo specifico
014000090206     c                   if        SKout_Errori(sgm) = *blank
014100090206     c                   if        SKsgm_AlfaNUM(sgm) ='A' or
014200090206     c                             SKsgm_AlfaNUM(sgm) ='N'
014300090206     c                   add       1             Nr_fld
014400090206     c                   EXSR      CARICA_DS
014500090206     c                   end
014600090206     c                   end
014700090206      *
014800090206     c                   endDo
014900090206      *
015000090212     c                   seton                                        RT
015100050628      * ?------------------------------------------------------------------ */
015200090206 **  c     CARICA_DS     begsr
015300090206      *
015400090211 **  c                   if        SKout_ValFld(sgm) <> *blank
015500090206     c                   select
015600090206 **  c                   when      Nr_fld   = 1
015700090206 **  c                   eval      campo_01 = SKout_ValFld(sgm)
015800090206     c                   when      Nr_fld   = 2
015900090206 **  c                   eval      campo_02 = SKout_ValFld(sgm)
016000090206     c                   when      Nr_fld   = 3
016100090206 **  c                   eval      campo_03 = SKout_ValFld(sgm)
016200090206     c                   when      Nr_fld   = 4
016300090206 **  c                   eval      campo_04 = SKout_ValFld(sgm)
016400090206     c                   when      Nr_fld   = 5
016500090206 **  c                   eval      campo_05 = SKout_ValFld(sgm)
016600090206     c                   when      Nr_fld   = 6
016700090206 **  c                   eval      campo_06 = SKout_ValFld(sgm)
016800090206     c                   when      Nr_fld   = 7
016900090206 **  c                   eval      campo_07 = SKout_ValFld(sgm)
017000090206     c                   when      Nr_fld   = 8
017100090206 **  c                   eval      campo_08 = SKout_ValFld(sgm)
017200090206     c                   when      Nr_fld   = 9
017300090206 **  c                   eval      campo_09 = SKout_ValFld(sgm)
017400090206     c                   when      Nr_fld   = 10
017500090206 **  c                   eval      campo_10 = SKout_ValFld(sgm)
017600090206     c                   when      Nr_fld   = 11
017700090206 **  c                   eval      campo_11 = SKout_ValFld(sgm)
017800090206     c                   when      Nr_fld   = 12
017900090206 **  c                   eval      campo_12 = SKout_ValFld(sgm)
018000090206     c                   when      Nr_fld   = 13
018100090206 **  c                   eval      campo_13 = SKout_ValFld(sgm)
018200090206     c                   when      Nr_fld   = 14
018300090206 **  c                   eval      campo_14 = SKout_ValFld(sgm)
018400090206     c                   when      Nr_fld   = 15
018500090206 **  c                   eval      campo_15 = SKout_ValFld(sgm)
018600090206     c                   endsl
018700090211     c                   end
018800090206      *
018900090206     c                   endsr
019000090206      * ?------------------------------------------------------------------ */
019100110621      * ?------------------------------------------------------------------ */
019200110621      *?      X non bloccare in nessun caso il traduttore CLIENTI
019300110621      * ?------------------------------------------------------------------ */
019400110621     C     *pssr         BEGSR
019500110621     C
019600110621     C                   eval      esito = 'E'
019700110621     C                   ENDSR     '*CANCL'
019800110621     C
019900110621      *  ------------------------------------------------------------------ */
