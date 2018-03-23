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
002900100811  ** D  DSsegmento   e ds                  EXTNAME(EDS96DGS)
003000100811  ** d    campo_01   e                     extFLD(DGS8273)
003100100811  ** d    campo_02   e                     extFLD(DGS8351)
003200100811  ** d    campo_03   e                     extFLD(DGS8078)
003300100811  ** d    campo_04   e                     extFLD(DGS8092)
003400100811  ** d    campo_05   e                     extFLD(DGS7124)
003500100811  ** d    campo_06   e                     extFLD(DGS7088)
003600100811  ** d    campo_07   e                     extFLD(DGS7106)
003700100811  ** d    campo_08   e                     extFLD(DGS6411)
003800100811  ** d    campo_09   e                     extFLD(DGS8339)
003900100811  ** d    campo_10   e                     extFLD(DGS8364)
004000100811  ** d    campo_11   e                     extFLD(DGS8410)
004100100811  ** d    campo_12   e                     extFLD(DGS8126)
004200100811  ** d    campo_13   e                     extFLD(DGS8158)
004300100811  ** d    campo_14   e                     extFLD(DGS8186)
004400100811  ** d    campo_15   e                     extFLD(DGS824611)
004500100811  ** d    campo_16   e                     extFLD(DGS824612)
004600100811  ** d    campo_17   e                     extFLD(DGS824613)
004700100811  ** d    campo_18   e                     extFLD(DGS8255)
004800100811  ** d    campo_19   e                     extFLD(DGS8325)
004900100811  ** d    campo_20   e                     extFLD(DGS8211)
005000090206      *----------------------------------------------------*
005100090206     d  esito          s              1
005200090206     D Campi_segmento  ds
005300090206     D  SKsgm_Campo                   4    DIM(50)
005400090206     D Campi_obbligat  ds
005500090206     D  SKsgm_Obbligo                 1    DIM(50)
005600090206     D Campi_AlfaNum   ds
005700090206     D  SKsgm_AlfaNum                 1    DIM(50)
005800090206     D Livello_Campi   ds
005900090206     D  SKsgm_Livello                 1s 0 DIM(50)
006000090206     D  SKsgm_Lungo    s              5s 0 DIM(50)
006100090206      *----
006200090206     D Valore_campi    ds
006300090206     D  SKout_valFLD               1000    DIM(50)
006400090206     D Lung_Valori     ds
006500090206     D  SKout_Lungo                   5s 0 DIM(50)
006600090206     D Valori_Alfanum  ds
006700090206     D  SKout_AlfaNum                 1    DIM(50)
006800090206     D Valori_inErr    ds
006900090206     D  SKout_Errori                  1    DIM(50)
007000090206     D Descr_Errore    ds
007100090206     D  SKout_DesErr                 50    DIM(50)
007200090206      *
007300090206     D  SKout_Diviso   s              1    DIM(50)
007400090206     D  SKout_Decimal  s              1s 0 DIM(50)
007500050713      * ?------------------------------------------------------------------ */
007600050629     D Digits          C                   CONST('0123456789')
007700050628      * ?------------------------------------------------------------------ */
007800050628     c     *entry        plist
007900050629      * ?     Input
008000090206     c                   parm                    Tipo_segmento
008100090206     c                   parm                    Segmento
008200100716     C                   parm                    keyUNBCLI
008300100716     C                   parm                    keyTIPOMSG
008400100716     C                   parm                    keyVERSION
008500100716     C                   parm                    keyRELEASE
008600100716     C                   parm                    keyAGENCY
008700100716     C                   parm                    keyASSOCIA
008800050629      * ?     Output
008900090206     c                   parm                    Esito
009000090206     c                   parm                    DSsegmento
009100090206     c                   parm                    Valori_inErr
009200090206     c                   parm                    Descr_Errore
009300050713      **
009400050713      ** Pulizia DS da restituire
009500090206     c                   clear                   Esito
009600090206     c                   clear                   DSsegmento
009700090206     c                   clear                   Valori_inErr
009800090206     c                   clear                   Descr_Errore
009900050629      **
010000090206     c                   clear                   SKsgm_Campo
010100090206     c                   clear                   SKsgm_Livello
010200090206     c                   clear                   SKsgm_Obbligo
010300090206     c                   clear                   SKsgm_AlfaNum
010400090206     c                   clear                   SKsgm_Lungo
010500090206      **
010600090206     c                   clear                   SKout_ValFld
010700090206     c                   clear                   SKout_AlfaNum
010800090206     c                   clear                   SKout_Lungo
010900090206     c                   clear                   SKout_Decimal
011000090206     c                   clear                   SKout_Errori
011100090206     c                   clear                   SKout_DesErr
011200090206      **
011300090209     c                   CALL      'TRTCT01R'
011400090206      * in
011500090206     C                   parm                    Segmento
011600100716     C                   parm                    keyUNBCLI
011700100811     C                   parm      'IFCSUM'      keyTIPOMSG
011800100811     C                   parm      'D'           keyVERSION
011900100811     C                   parm      '96A'         keyRELEASE
012000100811     C                   parm      'UN'          keyAGENCY
012100100716     C                   parm                    keyASSOCIA
012200090206      * out
012300090206     C                   parm                    esito
012400090206     C                   parm                    Tipo_segmento
012500090206     C                   parm                    Campi_Segmento
012600090206     C                   parm                    Campi_Obbligat
012700090206     C                   parm                    Campi_AlfaNum
012800090206     C                   parm                    Livello_Campi
012900090206     C                   parm                    Valore_Campi
013000090206     C                   parm                    Lung_Valori
013100090206     C                   parm                    Valori_Alfanum
013200090206     C                   parm                    Valori_inErr
013300090206     C                   parm                    Descr_Errore
013400090206      **
013500090206      *   se c'è stato un caso di errore non deve caricare il campo errato
013600090206      **
013700090206     c                   z-add     0             Nr_fld            3 0
013800090206      *  max 50 campi
013900090206     c                   do        50            sgm               3 0
014000090206      *
014100090206     c                   if        SKsgm_CAMPO(sgm) = *blank
014200090206     c                   leave
014300090206     c                   end
014400090206      **
014500090206      **  solo se non ci sono errori sul campo specifico
014600090206     c                   if        SKout_Errori(sgm) = *blank
014700090206     c                   if        SKsgm_AlfaNUM(sgm) ='A' or
014800090206     c                             SKsgm_AlfaNUM(sgm) ='N'
014900090206     c                   add       1             Nr_fld
015000090206     c                   EXSR      CARICA_DS
015100090206     c                   end
015200090206     c                   end
015300090206      *
015400090206     c                   endDo
015500090206      *
015600090212     c                   seton                                        RT
015700050628      * ?------------------------------------------------------------------ */
015800090206 **  c     CARICA_DS     begsr
015900090206      *
016000090211     c                   if         SKout_ValFld(sgm) <> *blank
016100100316      *
016200090206     c                   select
016300090206 **  c                   when      Nr_fld   = 1
016400100316     c                   eval      campo_01 = SKout_ValFld(sgm)
016500090206     c                   when      Nr_fld   = 2
016600100316     c                   eval      campo_02 = SKout_ValFld(sgm)
016700100316     c                   when      Nr_fld   = 3
016800100316     c                   eval      campo_03 = SKout_ValFld(sgm)
016900100316     c                   when      Nr_fld   = 4
017000100316     c                   eval      campo_04 = SKout_ValFld(sgm)
017100100316     c                   when      Nr_fld   = 5
017200100316     c                   eval      campo_05 = %int(%trimR(SKout_ValFld(sgm)))
017300100316     c                   when      Nr_fld   = 6
017400100316     c                   eval      campo_06 = SKout_ValFld(sgm)
017500100811     c                   when      Nr_fld   = 7
017600100811     c                   eval      campo_07 = SKout_ValFld(sgm)
017700100811     c                   when      Nr_fld   = 8
017800100811     c                   eval      campo_08 = SKout_ValFld(sgm)
017900100811     c                   when      Nr_fld   = 9
018000100811     c                   eval      campo_09 = SKout_ValFld(sgm)
018100100811     c                   when      Nr_fld   = 10
018200100811     c                   eval      campo_10 = SKout_ValFld(sgm)
018300100811     c                   when      Nr_fld   = 11
018400100811     c                   eval      campo_11 = SKout_ValFld(sgm)
018500100811     c                   when      Nr_fld   = 12
018600100811     c                   eval      campo_12 = SKout_ValFld(sgm)
018700100811     c                   when      Nr_fld   = 13
018800100811     c                   eval      campo_13 = SKout_ValFld(sgm)
018900100811     c                   when      Nr_fld   = 14
019000100811     c                   eval      campo_14 = SKout_ValFld(sgm)
019100100811     c                   when      Nr_fld   = 15
019200100811     c                   eval      campo_15 = SKout_ValFld(sgm)
019300100811     c                   when      Nr_fld   = 16
019400100811     c                   eval      campo_16 = SKout_ValFld(sgm)
019500100811     c                   when      Nr_fld   = 17
019600100811     c                   eval      campo_17 = SKout_ValFld(sgm)
019700100811     c                   when      Nr_fld   = 18
019800100811     c                   eval      campo_18 = SKout_ValFld(sgm)
019900100811     c                   when      Nr_fld   = 19
020000100811     c                   eval      campo_19 = SKout_ValFld(sgm)
020100100811     c                   when      Nr_fld   = 20
020200100811     c                   eval      campo_20 = SKout_ValFld(sgm)
020300090206     c                   endsl
020400100316      *
020500090211     c                   end
020600090206      *
020700090206     c                   endsr
020800090206      * ?------------------------------------------------------------------ */
020900110621      * ?------------------------------------------------------------------ */
021000110621      *?      X non bloccare in nessun caso il traduttore CLIENTI
021100110621      * ?------------------------------------------------------------------ */
021200110621     C     *pssr         BEGSR
021300110621     C
021400110621     C                   eval      esito = 'E'
021500110621     C                   ENDSR     '*CANCL'
021600110621     C
021700110621      *  ------------------------------------------------------------------ */
