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
002900100811  ** D  DSsegmento   e ds                  EXTNAME(EDS96NAD)
003000090206  ** d    campo_01   e                     extFLD(NAD3035)
003100090206  ** d    campo_02   e                     extFLD(NAD3039)
003200090206  ** d    campo_03   e                     extFLD(NAD113120)
003300090206  ** d    campo_04   e                     extFLD(NAD305520)
003400090206  ** d    campo_05   e                     extFLD(NAD31241)
003500090206  ** d    campo_06   e                     extFLD(NAD31242)
003600090206  ** d    campo_07   e                     extFLD(NAD31243)
003700090206  ** d    campo_08   e                     extFLD(NAD31244)
003800090206  ** d    campo_09   e                     extFLD(NAD31245)
003900090206  ** d    campo_10   e                     extFLD(NAD30361)
004000090206  ** d    campo_11   e                     extFLD(NAD30362)
004100090206  ** d    campo_12   e                     extFLD(NAD30363)
004200090206  ** d    campo_13   e                     extFLD(NAD30364)
004300090206  ** d    campo_14   e                     extFLD(NAD30365)
004400100730  ** d    campo_15   e                     extFLD(NAD3045)
004500090206  ** d    campo_16   e                     extFLD(NAD30421)
004600090206  ** d    campo_17   e                     extFLD(NAD30422)
004700090206  ** d    campo_18   e                     extFLD(NAD30423)
004800090206  ** d    campo_19   e                     extFLD(NAD30424)
004900090206  ** d    campo_20   e                     extFLD(NAD3164)
005000090206  ** d    campo_21   e                     extFLD(NAD3229)
005100100811  ** d    campo_22   e                     extFLD(NAD3251)
005200100811  ** d    campo_23   e                     extFLD(NAD3207)
005300090206      *----------------------------------------------------*
005400090206     d  esito          s              1
005500090206     D Campi_segmento  ds
005600090206     D  SKsgm_Campo                   4    DIM(50)
005700090206     D Campi_obbligat  ds
005800090206     D  SKsgm_Obbligo                 1    DIM(50)
005900090206     D Campi_AlfaNum   ds
006000090206     D  SKsgm_AlfaNum                 1    DIM(50)
006100090206     D Livello_Campi   ds
006200090206     D  SKsgm_Livello                 1s 0 DIM(50)
006300090206     D  SKsgm_Lungo    s              5s 0 DIM(50)
006400090206      *----
006500090206     D Valore_campi    ds
006600090206     D  SKout_valFLD               1000    DIM(50)
006700090206     D Lung_Valori     ds
006800090206     D  SKout_Lungo                   5s 0 DIM(50)
006900090206     D Valori_Alfanum  ds
007000090206     D  SKout_AlfaNum                 1    DIM(50)
007100090206     D Valori_inErr    ds
007200090206     D  SKout_Errori                  1    DIM(50)
007300090206     D Descr_Errore    ds
007400090206     D  SKout_DesErr                 50    DIM(50)
007500090206      *
007600090206     D  SKout_Diviso   s              1    DIM(50)
007700090206     D  SKout_Decimal  s              1s 0 DIM(50)
007800050713      * ?------------------------------------------------------------------ */
007900050629     D Digits          C                   CONST('0123456789')
008000050628      * ?------------------------------------------------------------------ */
008100050628     c     *entry        plist
008200050629      * ?     Input
008300090206     c                   parm                    Tipo_segmento
008400090206     c                   parm                    Segmento
008500100716     C                   parm                    keyUNBCLI
008600100716     C                   parm                    keyTIPOMSG
008700100716     C                   parm                    keyVERSION
008800100716     C                   parm                    keyRELEASE
008900100716     C                   parm                    keyAGENCY
009000100716     C                   parm                    keyASSOCIA
009100050629      * ?     Output
009200090206     c                   parm                    Esito
009300090206     c                   parm                    DSsegmento
009400090206     c                   parm                    Valori_inErr
009500090206     c                   parm                    Descr_Errore
009600050713      **
009700050713      ** Pulizia DS da restituire
009800090206     c                   clear                   Esito
009900090206     c                   clear                   DSsegmento
010000090206     c                   clear                   Valori_inErr
010100090206     c                   clear                   Descr_Errore
010200050629      **
010300090206     c                   clear                   SKsgm_Campo
010400090206     c                   clear                   SKsgm_Livello
010500090206     c                   clear                   SKsgm_Obbligo
010600090206     c                   clear                   SKsgm_AlfaNum
010700090206     c                   clear                   SKsgm_Lungo
010800090206      **
010900090206     c                   clear                   SKout_ValFld
011000090206     c                   clear                   SKout_AlfaNum
011100090206     c                   clear                   SKout_Lungo
011200090206     c                   clear                   SKout_Decimal
011300090206     c                   clear                   SKout_Errori
011400090206     c                   clear                   SKout_DesErr
011500090206      **
011600090209     c                   CALL      'TRTCT01R'
011700090206      * in
011800090206     C                   parm                    Segmento
011900100716     C                   parm                    keyUNBCLI
012000100811     C                   parm      'IFCSUM'      keyTIPOMSG
012100100811     C                   parm      'D'           keyVERSION
012200100811     C                   parm      '96A'         keyRELEASE
012300100811     C                   parm      'UN'          keyAGENCY
012400100716     C                   parm                    keyASSOCIA
012500090206      * out
012600090206     C                   parm                    esito
012700090206     C                   parm                    Tipo_segmento
012800090206     C                   parm                    Campi_Segmento
012900090206     C                   parm                    Campi_Obbligat
013000090206     C                   parm                    Campi_AlfaNum
013100090206     C                   parm                    Livello_Campi
013200090206     C                   parm                    Valore_Campi
013300090206     C                   parm                    Lung_Valori
013400090206     C                   parm                    Valori_Alfanum
013500090206     C                   parm                    Valori_inErr
013600090206     C                   parm                    Descr_Errore
013700090206      **
013800090206      *   se c'è stato un caso di errore non deve caricare il campo errato
013900090206      **
014000090206     c                   z-add     0             Nr_fld            3 0
014100090206      *  max 50 campi
014200090206     c                   do        50            sgm               3 0
014300090206      *
014400090206     c                   if        SKsgm_CAMPO(sgm) = *blank
014500090206     c                   leave
014600090206     c                   end
014700090206      **
014800090206      **  solo se non ci sono errori sul campo specifico
014900090206     c                   if        SKout_Errori(sgm) = *blank
015000090206     c                   if        SKsgm_AlfaNUM(sgm) ='A' or
015100090206     c                             SKsgm_AlfaNUM(sgm) ='N'
015200090206     c                   add       1             Nr_fld
015300090206     c                   EXSR      CARICA_DS
015400090206     c                   end
015500090206     c                   end
015600090206      *
015700090206     c                   endDo
015800090206      *
015900090212     c                   seton                                        RT
016000050628      * ?------------------------------------------------------------------ */
016100090206 **  c     CARICA_DS     begsr
016200090206      *
016300090211 **  c                   if        SKout_ValFld(sgm) <> *blank
016400090206     c                   select
016500090206 **  c                   when      Nr_fld   = 1
016600090206 **  c                   eval      campo_01 = SKout_ValFld(sgm)
016700090206     c                   when      Nr_fld   = 2
016800090206 **  c                   eval      campo_02 = SKout_ValFld(sgm)
016900090206     c                   when      Nr_fld   = 3
017000090206 **  c                   eval      campo_03 = SKout_ValFld(sgm)
017100090206     c                   when      Nr_fld   = 4
017200090206 **  c                   eval      campo_04 = SKout_ValFld(sgm)
017300090206     c                   when      Nr_fld   = 5
017400090206 **  c                   eval      campo_05 = SKout_ValFld(sgm)
017500090206     c                   when      Nr_fld   = 6
017600090206 **  c                   eval      campo_06 = SKout_ValFld(sgm)
017700090206     c                   when      Nr_fld   = 7
017800090206 **  c                   eval      campo_07 = SKout_ValFld(sgm)
017900090206     c                   when      Nr_fld   = 8
018000090206 **  c                   eval      campo_08 = SKout_ValFld(sgm)
018100090206     c                   when      Nr_fld   = 9
018200090206 **  c                   eval      campo_09 = SKout_ValFld(sgm)
018300090206     c                   when      Nr_fld   = 10
018400090206 **  c                   eval      campo_10 = SKout_ValFld(sgm)
018500090206     c                   when      Nr_fld   = 11
018600090206 **  c                   eval      campo_11 = SKout_ValFld(sgm)
018700090206     c                   when      Nr_fld   = 12
018800090206 **  c                   eval      campo_12 = SKout_ValFld(sgm)
018900090206     c                   when      Nr_fld   = 13
019000090206 **  c                   eval      campo_13 = SKout_ValFld(sgm)
019100090206     c                   when      Nr_fld   = 14
019200090206 **  c                   eval      campo_14 = SKout_ValFld(sgm)
019300090206     c                   when      Nr_fld   = 15
019400090206 **  c                   eval      campo_15 = SKout_ValFld(sgm)
019500090206     c                   when      Nr_fld   = 16
019600090206 **  c                   eval      campo_16 = SKout_ValFld(sgm)
019700090206     c                   when      Nr_fld   = 17
019800090206 **  c                   eval      campo_17 = SKout_ValFld(sgm)
019900090206     c                   when      Nr_fld   = 18
020000090206 **  c                   eval      campo_18 = SKout_ValFld(sgm)
020100090206     c                   when      Nr_fld   = 19
020200090206 **  c                   eval      campo_19 = SKout_ValFld(sgm)
020300090206     c                   when      Nr_fld   = 20
020400090206 **  c                   eval      campo_20 = SKout_ValFld(sgm)
020500090206     c                   when      Nr_fld   = 21
020600090206 **  c                   eval      campo_21 = SKout_ValFld(sgm)
020700090206     c                   when      Nr_fld   = 22
020800090206 **  c                   eval      campo_22 = SKout_ValFld(sgm)
020900090206     c                   when      Nr_fld   = 23
021000090206 **  c                   eval      campo_23 = SKout_ValFld(sgm)
021100090206     c                   endsl
021200090211     c                   end
021300090206      *
021400090206     c                   endsr
021500090206      * ?------------------------------------------------------------------ */
021600110621      * ?------------------------------------------------------------------ */
021700110621      *?      X non bloccare in nessun caso il traduttore CLIENTI
021800110621      * ?------------------------------------------------------------------ */
021900110621     C     *pssr         BEGSR
022000110621     C
022100110621     C                   eval      esito = 'E'
022200110621     C                   ENDSR     '*CANCL'
022300110621     C
022400110621      *  ------------------------------------------------------------------ */
