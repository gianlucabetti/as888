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
002900100811  ** D  DSsegmento   e ds                  EXTNAME(EDS96PIA)
003000100811  ** d    campo_01   e                     extFLD(PIA4347)
003100100811  ** d    campo_02   e                     extFLD(PIA714010)
003200100811  ** d    campo_03   e                     extFLD(PIA714310)
003300100811  ** d    campo_04   e                     extFLD(PIA113110)
003400100811  ** d    campo_05   e                     extFLD(PIA305510)
003500100811  ** d    campo_06   e                     extFLD(PIA714020)
003600100811  ** d    campo_07   e                     extFLD(PIA714320)
003700100811  ** d    campo_08   e                     extFLD(PIA113120)
003800100811  ** d    campo_09   e                     extFLD(PIA305520)
003900100811  ** d    campo_10   e                     extFLD(PIA714030)
004000100811  ** d    campo_11   e                     extFLD(PIA714330)
004100100811  ** d    campo_12   e                     extFLD(PIA113130)
004200100811  ** d    campo_13   e                     extFLD(PIA305530)
004300100811  ** d    campo_14   e                     extFLD(PIA714040)
004400100811  ** d    campo_15   e                     extFLD(PIA714340)
004500100811  ** d    campo_16   e                     extFLD(PIA113140)
004600100811  ** d    campo_17   e                     extFLD(PIA305540)
004700100811  ** d    campo_18   e                     extFLD(PIA714050)
004800100811  ** d    campo_19   e                     extFLD(PIA714350)
004900100811  ** d    campo_20   e                     extFLD(PIA113150)
005000100811  ** d    campo_21   e                     extFLD(PIA305550)
005100090206      *----------------------------------------------------*
005200090206     d  esito          s              1
005300090206     D Campi_segmento  ds
005400090206     D  SKsgm_Campo                   4    DIM(50)
005500090206     D Campi_obbligat  ds
005600090206     D  SKsgm_Obbligo                 1    DIM(50)
005700090206     D Campi_AlfaNum   ds
005800090206     D  SKsgm_AlfaNum                 1    DIM(50)
005900090206     D Livello_Campi   ds
006000090206     D  SKsgm_Livello                 1s 0 DIM(50)
006100090206     D  SKsgm_Lungo    s              5s 0 DIM(50)
006200090206      *----
006300090206     D Valore_campi    ds
006400090206     D  SKout_valFLD               1000    DIM(50)
006500090206     D Lung_Valori     ds
006600090206     D  SKout_Lungo                   5s 0 DIM(50)
006700090206     D Valori_Alfanum  ds
006800090206     D  SKout_AlfaNum                 1    DIM(50)
006900090206     D Valori_inErr    ds
007000090206     D  SKout_Errori                  1    DIM(50)
007100090206     D Descr_Errore    ds
007200090206     D  SKout_DesErr                 50    DIM(50)
007300090206      *
007400090206     D  SKout_Diviso   s              1    DIM(50)
007500090206     D  SKout_Decimal  s              1s 0 DIM(50)
007600050713      * ?------------------------------------------------------------------ */
007700050629     D Digits          C                   CONST('0123456789')
007800050628      * ?------------------------------------------------------------------ */
007900050628     c     *entry        plist
008000050629      * ?     Input
008100090206     c                   parm                    Tipo_segmento
008200090206     c                   parm                    Segmento
008300100716     C                   parm                    keyUNBCLI
008400100716     C                   parm                    keyTIPOMSG
008500100716     C                   parm                    keyVERSION
008600100716     C                   parm                    keyRELEASE
008700100716     C                   parm                    keyAGENCY
008800100716     C                   parm                    keyASSOCIA
008900050629      * ?     Output
009000090206     c                   parm                    Esito
009100090206     c                   parm                    DSsegmento
009200090206     c                   parm                    Valori_inErr
009300090206     c                   parm                    Descr_Errore
009400050713      **
009500050713      ** Pulizia DS da restituire
009600090206     c                   clear                   Esito
009700090206     c                   clear                   DSsegmento
009800090206     c                   clear                   Valori_inErr
009900090206     c                   clear                   Descr_Errore
010000050629      **
010100090206     c                   clear                   SKsgm_Campo
010200090206     c                   clear                   SKsgm_Livello
010300090206     c                   clear                   SKsgm_Obbligo
010400090206     c                   clear                   SKsgm_AlfaNum
010500090206     c                   clear                   SKsgm_Lungo
010600090206      **
010700090206     c                   clear                   SKout_ValFld
010800090206     c                   clear                   SKout_AlfaNum
010900090206     c                   clear                   SKout_Lungo
011000090206     c                   clear                   SKout_Decimal
011100090206     c                   clear                   SKout_Errori
011200090206     c                   clear                   SKout_DesErr
011300090206      **
011400090209     c                   CALL      'TRTCT01R'
011500090206      * in
011600090206     C                   parm                    Segmento
011700100716     C                   parm                    keyUNBCLI
011800100811     C                   parm      'IFCSUM'      keyTIPOMSG
011900100811     C                   parm      'D'           keyVERSION
012000100811     C                   parm      '96A'         keyRELEASE
012100100811     C                   parm      'UN'          keyAGENCY
012200100716     C                   parm                    keyASSOCIA
012300090206      * out
012400090206     C                   parm                    esito
012500090206     C                   parm                    Tipo_segmento
012600090206     C                   parm                    Campi_Segmento
012700090206     C                   parm                    Campi_Obbligat
012800090206     C                   parm                    Campi_AlfaNum
012900090206     C                   parm                    Livello_Campi
013000090206     C                   parm                    Valore_Campi
013100090206     C                   parm                    Lung_Valori
013200090206     C                   parm                    Valori_Alfanum
013300090206     C                   parm                    Valori_inErr
013400090206     C                   parm                    Descr_Errore
013500090206      **
013600090206      *   se c'è stato un caso di errore non deve caricare il campo errato
013700090206      **
013800090206     c                   z-add     0             Nr_fld            3 0
013900090206      *  max 50 campi
014000090206     c                   do        50            sgm               3 0
014100090206      *
014200090206     c                   if        SKsgm_CAMPO(sgm) = *blank
014300090206     c                   leave
014400090206     c                   end
014500090206      **
014600090206      **  solo se non ci sono errori sul campo specifico
014700090206     c                   if        SKout_Errori(sgm) = *blank
014800090206     c                   if        SKsgm_AlfaNUM(sgm) ='A' or
014900090206     c                             SKsgm_AlfaNUM(sgm) ='N'
015000090206     c                   add       1             Nr_fld
015100090206     c                   EXSR      CARICA_DS
015200090206     c                   end
015300090206     c                   end
015400090206      *
015500090206     c                   endDo
015600090206      *
015700090212     c                   seton                                        RT
015800050628      * ?------------------------------------------------------------------ */
015900090206 **  c     CARICA_DS     begsr
016000090206      *
016100090211     c                   if         SKout_ValFld(sgm) <> *blank
016200100316      *
016300090206     c                   select
016400090206 **  c                   when      Nr_fld   = 1
016500100316     c                   eval      campo_01 = SKout_ValFld(sgm)
016600090206     c                   when      Nr_fld   = 2
016700100316     c                   eval      campo_02 = SKout_ValFld(sgm)
016800100316     c                   when      Nr_fld   = 3
016900100316     c                   eval      campo_03 = SKout_ValFld(sgm)
017000100811     c                   when      Nr_fld   = 4
017100100811     c                   eval      campo_04 = SKout_ValFld(sgm)
017200100811     c                   when      Nr_fld   = 5
017300100811     c                   eval      campo_05 = SKout_ValFld(sgm)
017400100811     c                   when      Nr_fld   = 6
017500100811     c                   eval      campo_06 = SKout_ValFld(sgm)
017600100811     c                   when      Nr_fld   = 7
017700100811     c                   eval      campo_07 = SKout_ValFld(sgm)
017800100811     c                   when      Nr_fld   = 8
017900100811     c                   eval      campo_08 = SKout_ValFld(sgm)
018000100811     c                   when      Nr_fld   = 9
018100100811     c                   eval      campo_09 = SKout_ValFld(sgm)
018200100811     c                   when      Nr_fld   = 10
018300100811     c                   eval      campo_10 = SKout_ValFld(sgm)
018400100811     c                   when      Nr_fld   = 11
018500100811     c                   eval      campo_11 = SKout_ValFld(sgm)
018600100811     c                   when      Nr_fld   = 12
018700100811     c                   eval      campo_12 = SKout_ValFld(sgm)
018800100811     c                   when      Nr_fld   = 13
018900100811     c                   eval      campo_13 = SKout_ValFld(sgm)
019000100811     c                   when      Nr_fld   = 14
019100100811     c                   eval      campo_14 = SKout_ValFld(sgm)
019200100811     c                   when      Nr_fld   = 15
019300100811     c                   eval      campo_15 = SKout_ValFld(sgm)
019400100811     c                   when      Nr_fld   = 16
019500100811     c                   eval      campo_16 = SKout_ValFld(sgm)
019600100811     c                   when      Nr_fld   = 17
019700100811     c                   eval      campo_17 = SKout_ValFld(sgm)
019800100811     c                   when      Nr_fld   = 18
019900100811     c                   eval      campo_18 = SKout_ValFld(sgm)
020000100811     c                   when      Nr_fld   = 19
020100100811     c                   eval      campo_19 = SKout_ValFld(sgm)
020200100811     c                   when      Nr_fld   = 20
020300100811     c                   eval      campo_20 = SKout_ValFld(sgm)
020400100811     c                   when      Nr_fld   = 21
020500100811     c                   eval      campo_21 = SKout_ValFld(sgm)
020600090206     c                   endsl
020700100316      *
020800090211     c                   end
020900090206      *
021000090206     c                   endsr
021100090206      * ?------------------------------------------------------------------ */
021200110621      * ?------------------------------------------------------------------ */
021300110621      *?      X non bloccare in nessun caso il traduttore CLIENTI
021400110621      * ?------------------------------------------------------------------ */
021500110621     C     *pssr         BEGSR
021600110621     C
021700110621     C                   eval      esito = 'E'
021800110621     C                   ENDSR     '*CANCL'
021900110621     C
022000110621      *  ------------------------------------------------------------------ */
