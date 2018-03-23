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
002900090206  ** D  DSsegmento   e ds                  EXTNAME(EDS00TDT)
003000090206  ** d    campo_01   e                     extFLD(TDT8051)
003100090206  ** d    campo_02   e                     extFLD(TDT8028)
003200090206  ** d    campo_03   e                     extFLD(TDT8067)
003300090206  ** d    campo_04   e                     extFLD(TDT8066)
003400090206  ** d    campo_05   e                     extFLD(TDT8179)
003500090206  ** d    campo_06   e                     extFLD(TDT113140)
003600090206  ** d    campo_07   e                     extFLD(TDT305540)
003700090206  ** d    campo_08   e                     extFLD(TDT8178)
003800090206  ** d    campo_09   e                     extFLD(TDT3127)
003900090206  ** d    campo_10   e                     extFLD(TDT113150)
004000090206  ** d    campo_11   e                     extFLD(TDT305550)
004100090206  ** d    campo_12   e                     extFLD(TDT3126)
004200090206  ** d    campo_13   e                     extFLD(TDT8101)
004300090206  ** d    campo_14   e                     extFLD(TDT8457)
004400090206  ** d    campo_15   e                     extFLD(TDT8459)
004500090206  ** d    campo_16   e                     extFLD(TDT7130)
004600090206  ** d    campo_17   e                     extFLD(TDT8213)
004700090206  ** d    campo_18   e                     extFLD(TDT113180)
004800090206  ** d    campo_19   e                     extFLD(TDT305580)
004900090206  ** d    campo_20   e                     extFLD(TDT8212)
005000090206  ** d    campo_21   e                     extFLD(TDT8453)
005100090206  ** d    campo_22   e                     extFLD(TDT8281)
005200090206      *----------------------------------------------------*
005300090206     d  esito          s              1
005400090206     D Campi_segmento  ds
005500090206     D  SKsgm_Campo                   4    DIM(50)
005600090206     D Campi_obbligat  ds
005700090206     D  SKsgm_Obbligo                 1    DIM(50)
005800090206     D Campi_AlfaNum   ds
005900090206     D  SKsgm_AlfaNum                 1    DIM(50)
006000090206     D Livello_Campi   ds
006100090206     D  SKsgm_Livello                 1s 0 DIM(50)
006200090206     D  SKsgm_Lungo    s              5s 0 DIM(50)
006300090206      *----
006400090206     D Valore_campi    ds
006500090206     D  SKout_valFLD               1000    DIM(50)
006600090206     D Lung_Valori     ds
006700090206     D  SKout_Lungo                   5s 0 DIM(50)
006800090206     D Valori_Alfanum  ds
006900090206     D  SKout_AlfaNum                 1    DIM(50)
007000090206     D Valori_inErr    ds
007100090206     D  SKout_Errori                  1    DIM(50)
007200090206     D Descr_Errore    ds
007300090206     D  SKout_DesErr                 50    DIM(50)
007400090206      *
007500090206     D  SKout_Diviso   s              1    DIM(50)
007600090206     D  SKout_Decimal  s              1s 0 DIM(50)
007700050713      * ?------------------------------------------------------------------ */
007800050629     D Digits          C                   CONST('0123456789')
007900050628      * ?------------------------------------------------------------------ */
008000050628     c     *entry        plist
008100050629      * ?     Input
008200090206     c                   parm                    Tipo_segmento
008300090206     c                   parm                    Segmento
008400100716     C                   parm                    keyUNBCLI
008500100716     C                   parm                    keyTIPOMSG
008600100716     C                   parm                    keyVERSION
008700100716     C                   parm                    keyRELEASE
008800100716     C                   parm                    keyAGENCY
008900100716     C                   parm                    keyASSOCIA
009000050629      * ?     Output
009100090206     c                   parm                    Esito
009200090206     c                   parm                    DSsegmento
009300090206     c                   parm                    Valori_inErr
009400090206     c                   parm                    Descr_Errore
009500050713      **
009600050713      ** Pulizia DS da restituire
009700090206     c                   clear                   Esito
009800090206     c                   clear                   DSsegmento
009900090206     c                   clear                   Valori_inErr
010000090206     c                   clear                   Descr_Errore
010100050629      **
010200090206     c                   clear                   SKsgm_Campo
010300090206     c                   clear                   SKsgm_Livello
010400090206     c                   clear                   SKsgm_Obbligo
010500090206     c                   clear                   SKsgm_AlfaNum
010600090206     c                   clear                   SKsgm_Lungo
010700090206      **
010800090206     c                   clear                   SKout_ValFld
010900090206     c                   clear                   SKout_AlfaNum
011000090206     c                   clear                   SKout_Lungo
011100090206     c                   clear                   SKout_Decimal
011200090206     c                   clear                   SKout_Errori
011300090206     c                   clear                   SKout_DesErr
011400090206      **
011500090209     c                   CALL      'TRTCT01R'
011600090206      * in
011700090206     C                   parm                    Segmento
011800100716     C                   parm                    keyUNBCLI
011900100716     C                   parm                    keyTIPOMSG
012000100716     C                   parm                    keyVERSION
012100100716     C                   parm                    keyRELEASE
012200100716     C                   parm                    keyAGENCY
012300100716     C                   parm                    keyASSOCIA
012400090206      * out
012500090206     C                   parm                    esito
012600090206     C                   parm                    Tipo_segmento
012700090206     C                   parm                    Campi_Segmento
012800090206     C                   parm                    Campi_Obbligat
012900090206     C                   parm                    Campi_AlfaNum
013000090206     C                   parm                    Livello_Campi
013100090206     C                   parm                    Valore_Campi
013200090206     C                   parm                    Lung_Valori
013300090206     C                   parm                    Valori_Alfanum
013400090206     C                   parm                    Valori_inErr
013500090206     C                   parm                    Descr_Errore
013600090206      **
013700090206      *   se c'è stato un caso di errore non deve caricare il campo errato
013800090206      **
013900090206     c                   z-add     0             Nr_fld            3 0
014000090206      *  max 50 campi
014100090206     c                   do        50            sgm               3 0
014200090206      *
014300090206     c                   if        SKsgm_CAMPO(sgm) = *blank
014400090206     c                   leave
014500090206     c                   end
014600090206      **
014700090206      **  solo se non ci sono errori sul campo specifico
014800090206     c                   if        SKout_Errori(sgm) = *blank
014900090206     c                   if        SKsgm_AlfaNUM(sgm) ='A' or
015000090206     c                             SKsgm_AlfaNUM(sgm) ='N'
015100090206     c                   add       1             Nr_fld
015200090206     c                   EXSR      CARICA_DS
015300090206     c                   end
015400090206     c                   end
015500090206      *
015600090206     c                   endDo
015700090206      *
015800090212     c                   seton                                        RT
015900050628      * ?------------------------------------------------------------------ */
016000090206 **  c     CARICA_DS     begsr
016100090206      *
016200090211     c                   if        SKout_ValFld(sgm) <> *blank
016300090206     c                   select
016400090206 **  c                   when      Nr_fld   = 1
016500090206 **  c                   eval      campo_01 = SKout_ValFld(sgm)
016600090206     c                   when      Nr_fld   = 2
016700090206 **  c                   eval      campo_02 = SKout_ValFld(sgm)
016800090206     c                   when      Nr_fld   = 3
016900090206 **  c                   eval      campo_03 = SKout_ValFld(sgm)
017000090206     c                   when      Nr_fld   = 4
017100090206 **  c                   eval      campo_04 = SKout_ValFld(sgm)
017200090206     c                   when      Nr_fld   = 5
017300090206 **  c                   eval      campo_05 = SKout_ValFld(sgm)
017400090206     c                   when      Nr_fld   = 6
017500090206 **  c                   eval      campo_06 = SKout_ValFld(sgm)
017600090206     c                   when      Nr_fld   = 7
017700090206 **  c                   eval      campo_07 = SKout_ValFld(sgm)
017800090206     c                   when      Nr_fld   = 8
017900090206 **  c                   eval      campo_08 = SKout_ValFld(sgm)
018000090206     c                   when      Nr_fld   = 9
018100090206 **  c                   eval      campo_09 = SKout_ValFld(sgm)
018200090206     c                   when      Nr_fld   = 10
018300090206 **  c                   eval      campo_10 = SKout_ValFld(sgm)
018400090206     c                   when      Nr_fld   = 11
018500090206 **  c                   eval      campo_11 = SKout_ValFld(sgm)
018600090206     c                   when      Nr_fld   = 12
018700090206 **  c                   eval      campo_12 = SKout_ValFld(sgm)
018800090206     c                   when      Nr_fld   = 13
018900090206 **  c                   eval      campo_13 = SKout_ValFld(sgm)
019000090206     c                   when      Nr_fld   = 14
019100090206 **  c                   eval      campo_14 = SKout_ValFld(sgm)
019200090206     c                   when      Nr_fld   = 15
019300090206 **  c                   eval      campo_15 = SKout_ValFld(sgm)
019400090206     c                   when      Nr_fld   = 16
019500090206 **  c                   eval      campo_16 = SKout_ValFld(sgm)
019600090206     c                   when      Nr_fld   = 17
019700090206 **  c                   eval      campo_17 = SKout_ValFld(sgm)
019800090206     c                   when      Nr_fld   = 18
019900090206 **  c                   eval      campo_18 = SKout_ValFld(sgm)
020000090206     c                   when      Nr_fld   = 19
020100090206 **  c                   eval      campo_19 = SKout_ValFld(sgm)
020200090206     c                   when      Nr_fld   = 20
020300090206 **  c                   eval      campo_20 = SKout_ValFld(sgm)
020400090206     c                   when      Nr_fld   = 21
020500090206 **  c                   eval      campo_21 = SKout_ValFld(sgm)
020600090206     c                   when      Nr_fld   = 22
020700090206 **  c                   eval      campo_22 = SKout_ValFld(sgm)
020800090206     c                   endsl
020900090211     c                   end
021000090206      *
021100090206     c                   endsr
021200090206      * ?------------------------------------------------------------------ */
021300110621      * ?------------------------------------------------------------------ */
021400110621      *?      X non bloccare in nessun caso il traduttore CLIENTI
021500110621      * ?------------------------------------------------------------------ */
021600110621     C     *pssr         BEGSR
021700110621     C
021800110621     C                   eval      esito = 'E'
021900110621     C                   ENDSR     '*CANCL'
022000110621     C
022100110621      *  ------------------------------------------------------------------ */
