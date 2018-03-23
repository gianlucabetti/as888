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
002900100811  ** D  DSsegmento   e ds                  EXTNAME(EDS96GID)
003000090209  ** d    campo_01   e                     extFLD(GID1496)
003100090209  ** d    campo_02   e                     extFLD(GID722420)
003200090209  ** d    campo_03   e                     extFLD(GID706520)
003300090209  ** d    campo_04   e                     extFLD(GID113120)
003400090209  ** d    campo_05   e                     extFLD(GID305520)
003500090209  ** d    campo_06   e                     extFLD(GID706420)
003600100811  ** d    campo_07   e                     extFLD(GID722430)
003700100811  ** d    campo_08   e                     extFLD(GID706530)
003800100811  ** d    campo_09   e                     extFLD(GID113130)
003900100811  ** d    campo_10   e                     extFLD(GID305530)
004000100811  ** d    campo_11   e                     extFLD(GID706430)
004100100811  ** d    campo_12   e                     extFLD(GID722440)
004200100811  ** d    campo_13   e                     extFLD(GID706540)
004300100811  ** d    campo_14   e                     extFLD(GID113140)
004400100811  ** d    campo_15   e                     extFLD(GID305540)
004500100811  ** d    campo_16   e                     extFLD(GID706440)
004600090206      *----------------------------------------------------*
004700090206     d  esito          s              1
004800090206     D Campi_segmento  ds
004900090206     D  SKsgm_Campo                   4    DIM(50)
005000090206     D Campi_obbligat  ds
005100090206     D  SKsgm_Obbligo                 1    DIM(50)
005200090206     D Campi_AlfaNum   ds
005300090206     D  SKsgm_AlfaNum                 1    DIM(50)
005400090206     D Livello_Campi   ds
005500090206     D  SKsgm_Livello                 1s 0 DIM(50)
005600090206     D  SKsgm_Lungo    s              5s 0 DIM(50)
005700090206      *----
005800090206     D Valore_campi    ds
005900090206     D  SKout_valFLD               1000    DIM(50)
006000090206     D Lung_Valori     ds
006100090206     D  SKout_Lungo                   5s 0 DIM(50)
006200090206     D Valori_Alfanum  ds
006300090206     D  SKout_AlfaNum                 1    DIM(50)
006400090206     D Valori_inErr    ds
006500090206     D  SKout_Errori                  1    DIM(50)
006600090206     D Descr_Errore    ds
006700090206     D  SKout_DesErr                 50    DIM(50)
006800090206      *
006900090206     D  SKout_Diviso   s              1    DIM(50)
007000090206     D  SKout_Decimal  s              1s 0 DIM(50)
007100050713      * ?------------------------------------------------------------------ */
007200050629     D Digits          C                   CONST('0123456789')
007300050628      * ?------------------------------------------------------------------ */
007400050628     c     *entry        plist
007500050629      * ?     Input
007600090206     c                   parm                    Tipo_segmento
007700090206     c                   parm                    Segmento
007800100716     C                   parm                    keyUNBCLI
007900100716     C                   parm                    keyTIPOMSG
008000100716     C                   parm                    keyVERSION
008100100716     C                   parm                    keyRELEASE
008200100716     C                   parm                    keyAGENCY
008300100716     C                   parm                    keyASSOCIA
008400050629      * ?     Output
008500090206     c                   parm                    Esito
008600090206     c                   parm                    DSsegmento
008700090206     c                   parm                    Valori_inErr
008800090206     c                   parm                    Descr_Errore
008900050713      **
009000050713      ** Pulizia DS da restituire
009100090206     c                   clear                   Esito
009200090206     c                   clear                   DSsegmento
009300090206     c                   clear                   Valori_inErr
009400090206     c                   clear                   Descr_Errore
009500050629      **
009600090206     c                   clear                   SKsgm_Campo
009700090206     c                   clear                   SKsgm_Livello
009800090206     c                   clear                   SKsgm_Obbligo
009900090206     c                   clear                   SKsgm_AlfaNum
010000090206     c                   clear                   SKsgm_Lungo
010100090206      **
010200090206     c                   clear                   SKout_ValFld
010300090206     c                   clear                   SKout_AlfaNum
010400090206     c                   clear                   SKout_Lungo
010500090206     c                   clear                   SKout_Decimal
010600090206     c                   clear                   SKout_Errori
010700090206     c                   clear                   SKout_DesErr
010800090206      **
010900090209     c                   CALL      'TRTCT01R'
011000090206      * in
011100090206     C                   parm                    Segmento
011200100716     C                   parm                    keyUNBCLI
011300100811     C                   parm      'IFCSUM'      keyTIPOMSG
011400100811     C                   parm      'D'           keyVERSION
011500100811     C                   parm      '96A'         keyRELEASE
011600100811     C                   parm      'UN'          keyAGENCY
011700100716     C                   parm                    keyASSOCIA
011800090206      * out
011900090206     C                   parm                    esito
012000090206     C                   parm                    Tipo_segmento
012100090206     C                   parm                    Campi_Segmento
012200090206     C                   parm                    Campi_Obbligat
012300090206     C                   parm                    Campi_AlfaNum
012400090206     C                   parm                    Livello_Campi
012500090206     C                   parm                    Valore_Campi
012600090206     C                   parm                    Lung_Valori
012700090206     C                   parm                    Valori_Alfanum
012800090206     C                   parm                    Valori_inErr
012900090206     C                   parm                    Descr_Errore
013000090206      **
013100090206      *   se c'è stato un caso di errore non deve caricare il campo errato
013200090206      **
013300090206     c                   z-add     0             Nr_fld            3 0
013400090206      *  max 50 campi
013500090206     c                   do        50            sgm               3 0
013600090206      *
013700090206     c                   if        SKsgm_CAMPO(sgm) = *blank
013800090206     c                   leave
013900090206     c                   end
014000090206      **
014100090206      **  solo se non ci sono errori sul campo specifico
014200090206     c                   if        SKout_Errori(sgm) = *blank
014300090206     c                   if        SKsgm_AlfaNUM(sgm) ='A' or
014400090206     c                             SKsgm_AlfaNUM(sgm) ='N'
014500090206     c                   add       1             Nr_fld
014600090206     c                   EXSR      CARICA_DS
014700090206     c                   end
014800090206     c                   end
014900090206      *
015000090206     c                   endDo
015100090206      *
015200090212     c                   seton                                        RT
015300050628      * ?------------------------------------------------------------------ */
015400090206 **  c     CARICA_DS     begsr
015500090206      *
015600090211     c                   if        SKout_ValFld(sgm) <> *blank
015700090206     c                   select
015800090206 **  c                   when      Nr_fld   = 1
015900090209 **  c                   eval      campo_01 = %int(%trimR(SKout_ValFld(sgm)))
016000090206     c                   when      Nr_fld   = 2
016100090209     c                   eval      campo_02 = %int(%trimR(SKout_ValFld(sgm)))
016200090206     c                   when      Nr_fld   = 3
016300090206     c                   eval      campo_03 = SKout_ValFld(sgm)
016400090209     c                   when      Nr_fld   = 4
016500090209     c                   eval      campo_04 = SKout_ValFld(sgm)
016600090209     c                   when      Nr_fld   = 5
016700090209     c                   eval      campo_05 = SKout_ValFld(sgm)
016800090209     c                   when      Nr_fld   = 6
016900090209     c                   eval      campo_06 = SKout_ValFld(sgm)
017000090209     c                   when      Nr_fld   = 7
017100100811     c                   eval      campo_07 = %int(%trimR(SKout_ValFld(sgm)))
017200090209     c                   when      Nr_fld   = 8
017300100811     c                   eval      campo_08 = SKout_ValFld(sgm)
017400090209     c                   when      Nr_fld   = 9
017500090209     c                   eval      campo_09 = SKout_ValFld(sgm)
017600090209     c                   when      Nr_fld   = 10
017700090209     c                   eval      campo_10 = SKout_ValFld(sgm)
017800090209     c                   when      Nr_fld   = 11
017900090209     c                   eval      campo_11 = SKout_ValFld(sgm)
018000090209     c                   when      Nr_fld   = 12
018100100811     c                   eval      campo_12 = %int(%trimR(SKout_ValFld(sgm)))
018200090209     c                   when      Nr_fld   = 13
018300090209     c                   eval      campo_13 = SKout_ValFld(sgm)
018400090209     c                   when      Nr_fld   = 14
018500100811     c                   eval      campo_14 = SKout_ValFld(sgm)
018600090209     c                   when      Nr_fld   = 15
018700090209     c                   eval      campo_15 = SKout_ValFld(sgm)
018800090209     c                   when      Nr_fld   = 16
018900090209     c                   eval      campo_16 = SKout_ValFld(sgm)
019000090206     c                   endsl
019100090211     c                   end
019200090206      *
019300090206     c                   endsr
019400090206      * ?------------------------------------------------------------------ */
019500110621      * ?------------------------------------------------------------------ */
019600110621      *?      X non bloccare in nessun caso il traduttore CLIENTI
019700110621      * ?------------------------------------------------------------------ */
019800110621     C     *pssr         BEGSR
019900110621     C
020000110621     C                   eval      esito = 'E'
020100110621     C                   ENDSR     '*CANCL'
020200110621     C
020300110621      *  ------------------------------------------------------------------ */
