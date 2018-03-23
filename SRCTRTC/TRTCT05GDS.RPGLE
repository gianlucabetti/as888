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
002900090609  ** D  DSsegmento   e ds                  EXTNAME(EDPORGDS)
003000090609  ** d    campo_01   e                     extFLD(GDS7781)
003100090609  ** d    campo_02   e                     extFLD(GDS70021)
003200090609  ** d    campo_03   e                     extFLD(GDS70022)
003300090609  ** d    campo_04   e                     extFLD(GDS70023)
003400090609  ** d    campo_05   e                     extFLD(GDS70024)
003500090609  ** d    campo_06   e                     extFLD(GDS70025)
003600090609  ** d    campo_07   e                     extFLD(GDS70026)
003700090609  ** d    campo_08   e                     extFLD(GDS70027)
003800090609  ** d    campo_09   e                     extFLD(GDS70028)
003900090609  ** d    campo_10   e                     extFLD(GDS70029)
004000090609  ** d    campo_11   e                     extFLD(GDS700210)
004100090609  ** d    campo_12   e                     extFLD(GDS7020)
004200090609  ** d    campo_13   e                     extFLD(GDS7023)
004300090609  ** d    campo_14   e                     extFLD(GDS1131)
004400090609  ** d    campo_15   e                     extFLD(GDS77871)
004500090609  ** d    campo_16   e                     extFLD(GDS77872)
004600090609  ** d    campo_17   e                     extFLD(GDS77873)
004700090609  ** d    campo_18   e                     extFLD(GDS77874)
004800090609  ** d    campo_19   e                     extFLD(GDS77875)
004900090206      *----------------------------------------------------*
005000090206     d  esito          s              1
005100090206     D Campi_segmento  ds
005200090206     D  SKsgm_Campo                   4    DIM(50)
005300090206     D Campi_obbligat  ds
005400090206     D  SKsgm_Obbligo                 1    DIM(50)
005500090206     D Campi_AlfaNum   ds
005600090206     D  SKsgm_AlfaNum                 1    DIM(50)
005700090206     D Livello_Campi   ds
005800090206     D  SKsgm_Livello                 1s 0 DIM(50)
005900090206     D  SKsgm_Lungo    s              5s 0 DIM(50)
006000090206      *----
006100090206     D Valore_campi    ds
006200090206     D  SKout_valFLD               1000    DIM(50)
006300090206     D Lung_Valori     ds
006400090206     D  SKout_Lungo                   5s 0 DIM(50)
006500090206     D Valori_Alfanum  ds
006600090206     D  SKout_AlfaNum                 1    DIM(50)
006700090206     D Valori_inErr    ds
006800090206     D  SKout_Errori                  1    DIM(50)
006900090206     D Descr_Errore    ds
007000090206     D  SKout_DesErr                 50    DIM(50)
007100090206      *
007200090206     D  SKout_Diviso   s              1    DIM(50)
007300090206     D  SKout_Decimal  s              1s 0 DIM(50)
007400050713      * ?------------------------------------------------------------------ */
007500050629     D Digits          C                   CONST('0123456789')
007600050628      * ?------------------------------------------------------------------ */
007700050628     c     *entry        plist
007800050629      * ?     Input
007900090206     c                   parm                    Tipo_segmento
008000090206     c                   parm                    Segmento
008100100716     C                   parm                    keyUNBCLI
008200100716     C                   parm                    keyTIPOMSG
008300100716     C                   parm                    keyVERSION
008400100716     C                   parm                    keyRELEASE
008500100716     C                   parm                    keyAGENCY
008600100716     C                   parm                    keyASSOCIA
008700050629      * ?     Output
008800090206     c                   parm                    Esito
008900090206     c                   parm                    DSsegmento
009000090206     c                   parm                    Valori_inErr
009100090206     c                   parm                    Descr_Errore
009200050713      **
009300050713      ** Pulizia DS da restituire
009400090206     c                   clear                   Esito
009500090206     c                   clear                   DSsegmento
009600090206     c                   clear                   Valori_inErr
009700090206     c                   clear                   Descr_Errore
009800050629      **
009900090206     c                   clear                   SKsgm_Campo
010000090206     c                   clear                   SKsgm_Livello
010100090206     c                   clear                   SKsgm_Obbligo
010200090206     c                   clear                   SKsgm_AlfaNum
010300090206     c                   clear                   SKsgm_Lungo
010400090206      **
010500090206     c                   clear                   SKout_ValFld
010600090206     c                   clear                   SKout_AlfaNum
010700090206     c                   clear                   SKout_Lungo
010800090206     c                   clear                   SKout_Decimal
010900090206     c                   clear                   SKout_Errori
011000090206     c                   clear                   SKout_DesErr
011100090206      **
011200090609     c                   CALL      'TRTCT05R1'
011300090206      * in
011400090206     C                   parm                    Segmento
011500100716     C                   parm                    keyUNBCLI
011600100716     C                   parm                    keyTIPOMSG
011700100716     C                   parm                    keyVERSION
011800100716     C                   parm                    keyRELEASE
011900100716     C                   parm                    keyAGENCY
012000100716     C                   parm                    keyASSOCIA
012100090206      * out
012200090206     C                   parm                    esito
012300090206     C                   parm                    Tipo_segmento
012400090206     C                   parm                    Campi_Segmento
012500090206     C                   parm                    Campi_Obbligat
012600090206     C                   parm                    Campi_AlfaNum
012700090206     C                   parm                    Livello_Campi
012800090206     C                   parm                    Valore_Campi
012900090206     C                   parm                    Lung_Valori
013000090206     C                   parm                    Valori_Alfanum
013100090206     C                   parm                    Valori_inErr
013200090206     C                   parm                    Descr_Errore
013300090206      **
013400090206      *   se c'è stato un caso di errore non deve caricare il campo errato
013500090206      **
013600090206     c                   z-add     0             Nr_fld            3 0
013700090206      *  max 50 campi
013800090206     c                   do        50            sgm               3 0
013900090206      *
014000090206     c                   if        SKsgm_CAMPO(sgm) = *blank
014100090206     c                   leave
014200090206     c                   end
014300090206      **
014400090206      **  solo se non ci sono errori sul campo specifico
014500090206     c                   if        SKout_Errori(sgm) = *blank
014600090206     c                   if        SKsgm_AlfaNUM(sgm) ='A' or
014700090206     c                             SKsgm_AlfaNUM(sgm) ='N'
014800090206     c                   add       1             Nr_fld
014900090206     c                   EXSR      CARICA_DS
015000090206     c                   end
015100090206     c                   end
015200090206      *
015300090206     c                   endDo
015400090206      *
015500090212     c                   seton                                        RT
015600050628      * ?------------------------------------------------------------------ */
015700090206 **  c     CARICA_DS     begsr
015800090206      *
015900090211     c                   if        SKout_ValFld(sgm) <> *blank
016000090206     c                   select
016100090206 **  c                   when      Nr_fld   = 1
016200090609 **  c                   eval      campo_01 = SKout_ValFld(sgm)
016300090206     c                   when      Nr_fld   = 2
016400090609     c                   eval      campo_02 = SKout_ValFld(sgm)
016500090206     c                   when      Nr_fld   = 3
016600090609     c                   eval      campo_03 = SKout_ValFld(sgm)
016700090209     c                   when      Nr_fld   = 4
016800090209     c                   eval      campo_04 = SKout_ValFld(sgm)
016900090209     c                   when      Nr_fld   = 5
017000090609     c                   eval      campo_05 = SKout_ValFld(sgm)
017100090209     c                   when      Nr_fld   = 6
017200090609     c                   eval      campo_06 = SKout_ValFld(sgm)
017300090209     c                   when      Nr_fld   = 7
017400090209     c                   eval      campo_07 = SKout_ValFld(sgm)
017500090209     c                   when      Nr_fld   = 8
017600090609     c                   eval      campo_08 = SKout_ValFld(sgm)
017700090209     c                   when      Nr_fld   = 9
017800090609     c                   eval      campo_09 = SKout_ValFld(sgm)
017900090209     c                   when      Nr_fld   = 10
018000090209     c                   eval      campo_10 = SKout_ValFld(sgm)
018100090209     c                   when      Nr_fld   = 11
018200090209     c                   eval      campo_11 = SKout_ValFld(sgm)
018300090209     c                   when      Nr_fld   = 12
018400090209     c                   eval      campo_12 = SKout_ValFld(sgm)
018500090209     c                   when      Nr_fld   = 13
018600090209     c                   eval      campo_13 = SKout_ValFld(sgm)
018700090209     c                   when      Nr_fld   = 14
018800090609     c                   eval      campo_14 = SKout_ValFld(sgm)
018900090209     c                   when      Nr_fld   = 15
019000090209     c                   eval      campo_15 = SKout_ValFld(sgm)
019100090609     c                   when      Nr_fld   = 16
019200090609     c                   eval      campo_16 = SKout_ValFld(sgm)
019300090609     c                   when      Nr_fld   = 17
019400090609     c                   eval      campo_17 = SKout_ValFld(sgm)
019500090609     c                   when      Nr_fld   = 18
019600090609     c                   eval      campo_18 = SKout_ValFld(sgm)
019700090609     c                   when      Nr_fld   = 19
019800090609     c                   eval      campo_19 = SKout_ValFld(sgm)
019900090206     c                   endsl
020000090211     c                   end
020100090206      *
020200090206     c                   endsr
020300090206      * ?------------------------------------------------------------------ */
020400110621      * ?------------------------------------------------------------------ */
020500110621      *?      X non bloccare in nessun caso il traduttore CLIENTI
020600110621      * ?------------------------------------------------------------------ */
020700110621     C     *pssr         BEGSR
020800110621     C
020900110621     C                   eval      esito = 'E'
021000110621     C                   ENDSR     '*CANCL'
021100110621     C
021200110621      *  ------------------------------------------------------------------ */
