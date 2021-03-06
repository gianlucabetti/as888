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
002800090206  ** D  DSsegmento   e ds                  EXTNAME(EDS00NAD)
002900090206  ** d    campo_01   e                     extFLD(NAD3035)
003000090206  ** d    campo_02   e                     extFLD(NAD3039)
003100090206  ** d    campo_03   e                     extFLD(NAD113120)
003200090206  ** d    campo_04   e                     extFLD(NAD305520)
003300090206  ** d    campo_05   e                     extFLD(NAD31241)
003400090206  ** d    campo_06   e                     extFLD(NAD31242)
003500090206  ** d    campo_07   e                     extFLD(NAD31243)
003600090206  ** d    campo_08   e                     extFLD(NAD31244)
003700090206  ** d    campo_09   e                     extFLD(NAD31245)
003800090206  ** d    campo_10   e                     extFLD(NAD30361)
003900090206  ** d    campo_11   e                     extFLD(NAD30362)
004000090206  ** d    campo_12   e                     extFLD(NAD30363)
004100090206  ** d    campo_13   e                     extFLD(NAD30364)
004200090206  ** d    campo_14   e                     extFLD(NAD30365)
004300100730  ** d    campo_15   e                     extFLD(NAD3045)
004400090206  ** d    campo_16   e                     extFLD(NAD30421)
004500090206  ** d    campo_17   e                     extFLD(NAD30422)
004600090206  ** d    campo_18   e                     extFLD(NAD30423)
004700090206  ** d    campo_19   e                     extFLD(NAD30424)
004800090206  ** d    campo_20   e                     extFLD(NAD3164)
004900090206  ** d    campo_21   e                     extFLD(NAD3229)
005000090206  ** d    campo_22   e                     extFLD(NAD113170)
005100090206  ** d    campo_23   e                     extFLD(NAD305570)
005200090206  ** d    campo_24   e                     extFLD(NAD3228)
005300090206  ** d    campo_25   e                     extFLD(NAD3251)
005400090206  ** d    campo_26   e                     extFLD(NAD3207)
005500090206      *----------------------------------------------------*
005600090206     d  esito          s              1
005700090206     D Campi_segmento  ds
005800090206     D  SKsgm_Campo                   4    DIM(50)
005900090206     D Campi_obbligat  ds
006000090206     D  SKsgm_Obbligo                 1    DIM(50)
006100090206     D Campi_AlfaNum   ds
006200090206     D  SKsgm_AlfaNum                 1    DIM(50)
006300090206     D Livello_Campi   ds
006400090206     D  SKsgm_Livello                 1s 0 DIM(50)
006500090206     D  SKsgm_Lungo    s              5s 0 DIM(50)
006600090206      *----
006700090206     D Valore_campi    ds
006800090206     D  SKout_valFLD               1000    DIM(50)
006900090206     D Lung_Valori     ds
007000090206     D  SKout_Lungo                   5s 0 DIM(50)
007100090206     D Valori_Alfanum  ds
007200090206     D  SKout_AlfaNum                 1    DIM(50)
007300090206     D Valori_inErr    ds
007400090206     D  SKout_Errori                  1    DIM(50)
007500090206     D Descr_Errore    ds
007600090206     D  SKout_DesErr                 50    DIM(50)
007700090206      *
007800090206     D  SKout_Diviso   s              1    DIM(50)
007900090206     D  SKout_Decimal  s              1s 0 DIM(50)
008000050713      * ?------------------------------------------------------------------ */
008100050629     D Digits          C                   CONST('0123456789')
008200050628      * ?------------------------------------------------------------------ */
008300050628     c     *entry        plist
008400050629      * ?     Input
008500090206     c                   parm                    Tipo_segmento
008600090206     c                   parm                    Segmento
008700100716     C                   parm                    keyUNBCLI
008800100716     C                   parm                    keyTIPOMSG
008900100716     C                   parm                    keyVERSION
009000100716     C                   parm                    keyRELEASE
009100100716     C                   parm                    keyAGENCY
009200100716     C                   parm                    keyASSOCIA
009300050629      * ?     Output
009400090206     c                   parm                    Esito
009500090206     c                   parm                    DSsegmento
009600090206     c                   parm                    Valori_inErr
009700090206     c                   parm                    Descr_Errore
009800050713      **
009900050713      ** Pulizia DS da restituire
010000090206     c                   clear                   Esito
010100090206     c                   clear                   DSsegmento
010200090206     c                   clear                   Valori_inErr
010300090206     c                   clear                   Descr_Errore
010400050629      **
010500090206     c                   clear                   SKsgm_Campo
010600090206     c                   clear                   SKsgm_Livello
010700090206     c                   clear                   SKsgm_Obbligo
010800090206     c                   clear                   SKsgm_AlfaNum
010900090206     c                   clear                   SKsgm_Lungo
011000090206      **
011100090206     c                   clear                   SKout_ValFld
011200090206     c                   clear                   SKout_AlfaNum
011300090206     c                   clear                   SKout_Lungo
011400090206     c                   clear                   SKout_Decimal
011500090206     c                   clear                   SKout_Errori
011600090206     c                   clear                   SKout_DesErr
011700090206      **
011800090209     c                   CALL      'TRTCT01R'
011900090206      * in
012000090206     C                   parm                    Segmento
012100100716     C                   parm                    keyUNBCLI
012200100716     C                   parm                    keyTIPOMSG
012300100716     C                   parm                    keyVERSION
012400100716     C                   parm                    keyRELEASE
012500100716     C                   parm                    keyAGENCY
012600100716     C                   parm                    keyASSOCIA
012700090206      * out
012800090206     C                   parm                    esito
012900090206     C                   parm                    Tipo_segmento
013000090206     C                   parm                    Campi_Segmento
013100090206     C                   parm                    Campi_Obbligat
013200090206     C                   parm                    Campi_AlfaNum
013300090206     C                   parm                    Livello_Campi
013400090206     C                   parm                    Valore_Campi
013500090206     C                   parm                    Lung_Valori
013600090206     C                   parm                    Valori_Alfanum
013700090206     C                   parm                    Valori_inErr
013800090206     C                   parm                    Descr_Errore
013900090206      **
014000090206      *   se c'� stato un caso di errore non deve caricare il campo errato
014100090206      **
014200090206     c                   z-add     0             Nr_fld            3 0
014300090206      *  max 50 campi
014400090206     c                   do        50            sgm               3 0
014500090206      *
014600090206     c                   if        SKsgm_CAMPO(sgm) = *blank
014700090206     c                   leave
014800090206     c                   end
014900090206      **
015000090206      **  solo se non ci sono errori sul campo specifico
015100090206     c                   if        SKout_Errori(sgm) = *blank
015200090206     c                   if        SKsgm_AlfaNUM(sgm) ='A' or
015300090206     c                             SKsgm_AlfaNUM(sgm) ='N'
015400090206     c                   add       1             Nr_fld
015500090206     c                   EXSR      CARICA_DS
015600090206     c                   end
015700090206     c                   end
015800090206      *
015900090206     c                   endDo
016000090206      *
016100090212     c                   seton                                        RT
016200050628      * ?------------------------------------------------------------------ */
016300090206 **  c     CARICA_DS     begsr
016400090206      *
016500090211 **  c                   if        SKout_ValFld(sgm) <> *blank
016600090206     c                   select
016700090206 **  c                   when      Nr_fld   = 1
016800090206 **  c                   eval      campo_01 = SKout_ValFld(sgm)
016900090206     c                   when      Nr_fld   = 2
017000090206 **  c                   eval      campo_02 = SKout_ValFld(sgm)
017100090206     c                   when      Nr_fld   = 3
017200090206 **  c                   eval      campo_03 = SKout_ValFld(sgm)
017300090206     c                   when      Nr_fld   = 4
017400090206 **  c                   eval      campo_04 = SKout_ValFld(sgm)
017500090206     c                   when      Nr_fld   = 5
017600090206 **  c                   eval      campo_05 = SKout_ValFld(sgm)
017700090206     c                   when      Nr_fld   = 6
017800090206 **  c                   eval      campo_06 = SKout_ValFld(sgm)
017900090206     c                   when      Nr_fld   = 7
018000090206 **  c                   eval      campo_07 = SKout_ValFld(sgm)
018100090206     c                   when      Nr_fld   = 8
018200090206 **  c                   eval      campo_08 = SKout_ValFld(sgm)
018300090206     c                   when      Nr_fld   = 9
018400090206 **  c                   eval      campo_09 = SKout_ValFld(sgm)
018500090206     c                   when      Nr_fld   = 10
018600090206 **  c                   eval      campo_10 = SKout_ValFld(sgm)
018700090206     c                   when      Nr_fld   = 11
018800090206 **  c                   eval      campo_11 = SKout_ValFld(sgm)
018900090206     c                   when      Nr_fld   = 12
019000090206 **  c                   eval      campo_12 = SKout_ValFld(sgm)
019100090206     c                   when      Nr_fld   = 13
019200090206 **  c                   eval      campo_13 = SKout_ValFld(sgm)
019300090206     c                   when      Nr_fld   = 14
019400090206 **  c                   eval      campo_14 = SKout_ValFld(sgm)
019500090206     c                   when      Nr_fld   = 15
019600090206 **  c                   eval      campo_15 = SKout_ValFld(sgm)
019700090206     c                   when      Nr_fld   = 16
019800090206 **  c                   eval      campo_16 = SKout_ValFld(sgm)
019900090206     c                   when      Nr_fld   = 17
020000090206 **  c                   eval      campo_17 = SKout_ValFld(sgm)
020100090206     c                   when      Nr_fld   = 18
020200090206 **  c                   eval      campo_18 = SKout_ValFld(sgm)
020300090206     c                   when      Nr_fld   = 19
020400090206 **  c                   eval      campo_19 = SKout_ValFld(sgm)
020500090206     c                   when      Nr_fld   = 20
020600090206 **  c                   eval      campo_20 = SKout_ValFld(sgm)
020700090206     c                   when      Nr_fld   = 21
020800090206 **  c                   eval      campo_21 = SKout_ValFld(sgm)
020900090206     c                   when      Nr_fld   = 22
021000090206 **  c                   eval      campo_22 = SKout_ValFld(sgm)
021100090206     c                   when      Nr_fld   = 23
021200090206 **  c                   eval      campo_23 = SKout_ValFld(sgm)
021300090206     c                   when      Nr_fld   = 24
021400090206 **  c                   eval      campo_24 = SKout_ValFld(sgm)
021500090206     c                   when      Nr_fld   = 25
021600090206 **  c                   eval      campo_25 = SKout_ValFld(sgm)
021700090206     c                   when      Nr_fld   = 26
021800090206 **  c                   eval      campo_26 = SKout_ValFld(sgm)
021900090206     c                   endsl
022000090211     c                   end
022100090206      *
022200090206     c                   endsr
022300090206      * ?------------------------------------------------------------------ */
022400110621      * ?------------------------------------------------------------------ */
022500110621      *?      X non bloccare in nessun caso il traduttore CLIENTI
022600110621      * ?------------------------------------------------------------------ */
022700110621     C     *pssr         BEGSR
022800110621     C
022900110621     C                   eval      esito = 'E'
023000110621     C                   ENDSR     '*CANCL'
023100110621     C
023200110621      *  ------------------------------------------------------------------ */
