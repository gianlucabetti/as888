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
002900090206  ** D  DSsegmento   e ds                  EXTNAME(EDS00PCI)
003000090206  ** d    campo_01   e                     extFLD(PCI4233)
003100090206  ** d    campo_02   e                     extFLD(PCI71021)
003200090206  ** d    campo_03   e                     extFLD(PCI71022)
003300090206  ** d    campo_04   e                     extFLD(PCI71023)
003400090206  ** d    campo_05   e                     extFLD(PCI71024)
003500090206  ** d    campo_06   e                     extFLD(PCI71025)
003600090206  ** d    campo_07   e                     extFLD(PCI71026)
003700090206  ** d    campo_08   e                     extFLD(PCI71027)
003800090206  ** d    campo_09   e                     extFLD(PCI71028)
003900090206  ** d    campo_10   e                     extFLD(PCI71029)
004000090206  ** d    campo_11   e                     extFLD(PCI710210)
004100090206  ** d    campo_12   e                     extFLD(PCI8196)
004200090206  ** d    campo_13   e                     extFLD(PCI7511)
004300090206  ** d    campo_14   e                     extFLD(PCI1131)
004400090206  ** d    campo_15   e                     extFLD(PCI3055)
004500090206      *----------------------------------------------------*
004600090206     d  esito          s              1
004700090206     D Campi_segmento  ds
004800090206     D  SKsgm_Campo                   4    DIM(50)
004900090206     D Campi_obbligat  ds
005000090206     D  SKsgm_Obbligo                 1    DIM(50)
005100090206     D Campi_AlfaNum   ds
005200090206     D  SKsgm_AlfaNum                 1    DIM(50)
005300090206     D Livello_Campi   ds
005400090206     D  SKsgm_Livello                 1s 0 DIM(50)
005500090206     D  SKsgm_Lungo    s              5s 0 DIM(50)
005600090206      *----
005700090206     D Valore_campi    ds
005800090206     D  SKout_valFLD               1000    DIM(50)
005900090206     D Lung_Valori     ds
006000090206     D  SKout_Lungo                   5s 0 DIM(50)
006100090206     D Valori_Alfanum  ds
006200090206     D  SKout_AlfaNum                 1    DIM(50)
006300090206     D Valori_inErr    ds
006400090206     D  SKout_Errori                  1    DIM(50)
006500090206     D Descr_Errore    ds
006600090206     D  SKout_DesErr                 50    DIM(50)
006700090206      *
006800090206     D  SKout_Diviso   s              1    DIM(50)
006900090206     D  SKout_Decimal  s              1s 0 DIM(50)
007000050713      * ?------------------------------------------------------------------ */
007100050629     D Digits          C                   CONST('0123456789')
007200050628      * ?------------------------------------------------------------------ */
007300050628     c     *entry        plist
007400050629      * ?     Input
007500090206     c                   parm                    Tipo_segmento
007600090206     c                   parm                    Segmento
007700100716     C                   parm                    keyUNBCLI
007800100716     C                   parm                    keyTIPOMSG
007900100716     C                   parm                    keyVERSION
008000100716     C                   parm                    keyRELEASE
008100100716     C                   parm                    keyAGENCY
008200100716     C                   parm                    keyASSOCIA
008300050629      * ?     Output
008400090206     c                   parm                    Esito
008500090206     c                   parm                    DSsegmento
008600090206     c                   parm                    Valori_inErr
008700090206     c                   parm                    Descr_Errore
008800050713      **
008900050713      ** Pulizia DS da restituire
009000090206     c                   clear                   Esito
009100090206     c                   clear                   DSsegmento
009200090206     c                   clear                   Valori_inErr
009300090206     c                   clear                   Descr_Errore
009400050629      **
009500090206     c                   clear                   SKsgm_Campo
009600090206     c                   clear                   SKsgm_Livello
009700090206     c                   clear                   SKsgm_Obbligo
009800090206     c                   clear                   SKsgm_AlfaNum
009900090206     c                   clear                   SKsgm_Lungo
010000090206      **
010100090206     c                   clear                   SKout_ValFld
010200090206     c                   clear                   SKout_AlfaNum
010300090206     c                   clear                   SKout_Lungo
010400090206     c                   clear                   SKout_Decimal
010500090206     c                   clear                   SKout_Errori
010600090206     c                   clear                   SKout_DesErr
010700090206      **
010800090209     c                   CALL      'TRTCT01R'
010900090206      * in
011000090206     C                   parm                    Segmento
011100100716     C                   parm                    keyUNBCLI
011200100716     C                   parm                    keyTIPOMSG
011300100716     C                   parm                    keyVERSION
011400100716     C                   parm                    keyRELEASE
011500100716     C                   parm                    keyAGENCY
011600100716     C                   parm                    keyASSOCIA
011700090206      * out
011800090206     C                   parm                    esito
011900090206     C                   parm                    Tipo_segmento
012000090206     C                   parm                    Campi_Segmento
012100090206     C                   parm                    Campi_Obbligat
012200090206     C                   parm                    Campi_AlfaNum
012300090206     C                   parm                    Livello_Campi
012400090206     C                   parm                    Valore_Campi
012500090206     C                   parm                    Lung_Valori
012600090206     C                   parm                    Valori_Alfanum
012700090206     C                   parm                    Valori_inErr
012800090206     C                   parm                    Descr_Errore
012900090206      **
013000090206      *   se c'è stato un caso di errore non deve caricare il campo errato
013100090206      **
013200090206     c                   z-add     0             Nr_fld            3 0
013300090206      *  max 50 campi
013400090206     c                   do        50            sgm               3 0
013500090206      *
013600090206     c                   if        SKsgm_CAMPO(sgm) = *blank
013700090206     c                   leave
013800090206     c                   end
013900090206      **
014000090206      **  solo se non ci sono errori sul campo specifico
014100090206     c                   if        SKout_Errori(sgm) = *blank
014200090206     c                   if        SKsgm_AlfaNUM(sgm) ='A' or
014300090206     c                             SKsgm_AlfaNUM(sgm) ='N'
014400090206     c                   add       1             Nr_fld
014500090206     c                   EXSR      CARICA_DS
014600090206     c                   end
014700090206     c                   end
014800090206      *
014900090206     c                   endDo
015000090206      *
015100090212     c                   seton                                        RT
015200050628      * ?------------------------------------------------------------------ */
015300090206 **  c     CARICA_DS     begsr
015400090206      *
015500090211 **  c                   if        SKout_ValFld(sgm) <> *blank
015600090206     c                   select
015700090206 **  c                   when      Nr_fld   = 1
015800090206 **  c                   eval      campo_01 = SKout_ValFld(sgm)
015900090206     c                   when      Nr_fld   = 2
016000090206 **  c                   eval      campo_02 = SKout_ValFld(sgm)
016100090206     c                   when      Nr_fld   = 3
016200090206 **  c                   eval      campo_03 = SKout_ValFld(sgm)
016300090206     c                   when      Nr_fld   = 4
016400090206 **  c                   eval      campo_04 = SKout_ValFld(sgm)
016500090206     c                   when      Nr_fld   = 5
016600090206 **  c                   eval      campo_05 = SKout_ValFld(sgm)
016700090206     c                   when      Nr_fld   = 6
016800090206 **  c                   eval      campo_06 = SKout_ValFld(sgm)
016900090206     c                   when      Nr_fld   = 7
017000090206 **  c                   eval      campo_07 = SKout_ValFld(sgm)
017100090206     c                   when      Nr_fld   = 8
017200090206 **  c                   eval      campo_08 = SKout_ValFld(sgm)
017300090206     c                   when      Nr_fld   = 9
017400090206 **  c                   eval      campo_09 = SKout_ValFld(sgm)
017500090206     c                   when      Nr_fld   = 10
017600090206 **  c                   eval      campo_10 = SKout_ValFld(sgm)
017700090206     c                   when      Nr_fld   = 11
017800090206 **  c                   eval      campo_11 = SKout_ValFld(sgm)
017900090206     c                   when      Nr_fld   = 12
018000090206 **  c                   eval      campo_12 = SKout_ValFld(sgm)
018100090206     c                   when      Nr_fld   = 13
018200090206 **  c                   eval      campo_13 = SKout_ValFld(sgm)
018300090206     c                   when      Nr_fld   = 14
018400090206 **  c                   eval      campo_14 = SKout_ValFld(sgm)
018500090206     c                   when      Nr_fld   = 15
018600090206 **  c                   eval      campo_15 = SKout_ValFld(sgm)
018700090206     c                   endsl
018800090211     c                   end
018900090206      *
019000090206     c                   endsr
019100090206      * ?------------------------------------------------------------------ */
019200110621      * ?------------------------------------------------------------------ */
019300110621      *?      X non bloccare in nessun caso il traduttore CLIENTI
019400110621      * ?------------------------------------------------------------------ */
019500110621     C     *pssr         BEGSR
019600110621     C
019700110621     C                   eval      esito = 'E'
019800110621     C                   ENDSR     '*CANCL'
019900110621     C
020000110621      *  ------------------------------------------------------------------ */
