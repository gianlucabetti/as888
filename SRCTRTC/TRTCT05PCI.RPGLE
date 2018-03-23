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
002900090609  ** D  DSsegmento   e ds                  EXTNAME(EDPORPCI)
003000090609  ** d    campo_01   e                     extFLD(PCI4233)
003100090609  ** d    campo_02   e                     extFLD(PCI70021)
003200090609  ** d    campo_03   e                     extFLD(PCI70022)
003300090609  ** d    campo_04   e                     extFLD(PCI70023)
003400090609  ** d    campo_05   e                     extFLD(PCI70024)
003500090609  ** d    campo_06   e                     extFLD(PCI70025)
003600090609  ** d    campo_07   e                     extFLD(PCI70026)
003700090609  ** d    campo_08   e                     extFLD(PCI70027)
003800090609  ** d    campo_09   e                     extFLD(PCI70028)
003900090609  ** d    campo_10   e                     extFLD(PCI70029)
004000090609  ** d    campo_11   e                     extFLD(PCI700210)
004100090609  ** d    campo_12   e                     extFLD(PCI1154)
004200090609  ** d    campo_13   e                     extFLD(PCI1153)
004300090609  ** d    campo_14   e                     extFLD(PCI1156)
004400090609  ** d    campo_15   e                     extFLD(PCI2001)
004500090609  ** d    campo_16   e                     extFLD(PCI2005)
004600090609  ** d    campo_17   e                     extFLD(PCI8731)
004700090206      *----------------------------------------------------*
004800090206     d  esito          s              1
004900090206     D Campi_segmento  ds
005000090206     D  SKsgm_Campo                   4    DIM(50)
005100090206     D Campi_obbligat  ds
005200090206     D  SKsgm_Obbligo                 1    DIM(50)
005300090206     D Campi_AlfaNum   ds
005400090206     D  SKsgm_AlfaNum                 1    DIM(50)
005500090206     D Livello_Campi   ds
005600090206     D  SKsgm_Livello                 1s 0 DIM(50)
005700090206     D  SKsgm_Lungo    s              5s 0 DIM(50)
005800090206      *----
005900090206     D Valore_campi    ds
006000090206     D  SKout_valFLD               1000    DIM(50)
006100090206     D Lung_Valori     ds
006200090206     D  SKout_Lungo                   5s 0 DIM(50)
006300090206     D Valori_Alfanum  ds
006400090206     D  SKout_AlfaNum                 1    DIM(50)
006500090206     D Valori_inErr    ds
006600090206     D  SKout_Errori                  1    DIM(50)
006700090206     D Descr_Errore    ds
006800090206     D  SKout_DesErr                 50    DIM(50)
006900090206      *
007000090206     D  SKout_Diviso   s              1    DIM(50)
007100090206     D  SKout_Decimal  s              1s 0 DIM(50)
007200050713      * ?------------------------------------------------------------------ */
007300050629     D Digits          C                   CONST('0123456789')
007400050628      * ?------------------------------------------------------------------ */
007500050628     c     *entry        plist
007600050629      * ?     Input
007700090206     c                   parm                    Tipo_segmento
007800090206     c                   parm                    Segmento
007900100716     C                   parm                    keyUNBCLI
008000100716     C                   parm                    keyTIPOMSG
008100100716     C                   parm                    keyVERSION
008200100716     C                   parm                    keyRELEASE
008300100716     C                   parm                    keyAGENCY
008400100716     C                   parm                    keyASSOCIA
008500050629      * ?     Output
008600090206     c                   parm                    Esito
008700090206     c                   parm                    DSsegmento
008800090206     c                   parm                    Valori_inErr
008900090206     c                   parm                    Descr_Errore
009000050713      **
009100050713      ** Pulizia DS da restituire
009200090206     c                   clear                   Esito
009300090206     c                   clear                   DSsegmento
009400090206     c                   clear                   Valori_inErr
009500090206     c                   clear                   Descr_Errore
009600050629      **
009700090206     c                   clear                   SKsgm_Campo
009800090206     c                   clear                   SKsgm_Livello
009900090206     c                   clear                   SKsgm_Obbligo
010000090206     c                   clear                   SKsgm_AlfaNum
010100090206     c                   clear                   SKsgm_Lungo
010200090206      **
010300090206     c                   clear                   SKout_ValFld
010400090206     c                   clear                   SKout_AlfaNum
010500090206     c                   clear                   SKout_Lungo
010600090206     c                   clear                   SKout_Decimal
010700090206     c                   clear                   SKout_Errori
010800090206     c                   clear                   SKout_DesErr
010900090206      **
011000090609     c                   CALL      'TRTCT05R1'
011100090206      * in
011200090206     C                   parm                    Segmento
011300100716     C                   parm                    keyUNBCLI
011400100716     C                   parm                    keyTIPOMSG
011500100716     C                   parm                    keyVERSION
011600100716     C                   parm                    keyRELEASE
011700100716     C                   parm                    keyAGENCY
011800100716     C                   parm                    keyASSOCIA
011900090206      * out
012000090206     C                   parm                    esito
012100090206     C                   parm                    Tipo_segmento
012200090206     C                   parm                    Campi_Segmento
012300090206     C                   parm                    Campi_Obbligat
012400090206     C                   parm                    Campi_AlfaNum
012500090206     C                   parm                    Livello_Campi
012600090206     C                   parm                    Valore_Campi
012700090206     C                   parm                    Lung_Valori
012800090206     C                   parm                    Valori_Alfanum
012900090206     C                   parm                    Valori_inErr
013000090206     C                   parm                    Descr_Errore
013100090206      **
013200090206      *   se c'è stato un caso di errore non deve caricare il campo errato
013300090206      **
013400090206     c                   z-add     0             Nr_fld            3 0
013500090206      *  max 50 campi
013600090206     c                   do        50            sgm               3 0
013700090206      *
013800090206     c                   if        SKsgm_CAMPO(sgm) = *blank
013900090206     c                   leave
014000090206     c                   end
014100090206      **
014200090206      **  solo se non ci sono errori sul campo specifico
014300090206     c                   if        SKout_Errori(sgm) = *blank
014400090206     c                   if        SKsgm_AlfaNUM(sgm) ='A' or
014500090206     c                             SKsgm_AlfaNUM(sgm) ='N'
014600090206     c                   add       1             Nr_fld
014700090206     c                   EXSR      CARICA_DS
014800090206     c                   end
014900090206     c                   end
015000090206      *
015100090206     c                   endDo
015200090206      *
015300090212     c                   seton                                        RT
015400050628      * ?------------------------------------------------------------------ */
015500090206 **  c     CARICA_DS     begsr
015600090206      *
015700090211 **  c                   if        SKout_ValFld(sgm) <> *blank
015800090206     c                   select
015900090206 **  c                   when      Nr_fld   = 1
016000090206 **  c                   eval      campo_01 = SKout_ValFld(sgm)
016100090206     c                   when      Nr_fld   = 2
016200090206 **  c                   eval      campo_02 = SKout_ValFld(sgm)
016300090206     c                   when      Nr_fld   = 3
016400090206 **  c                   eval      campo_03 = SKout_ValFld(sgm)
016500090206     c                   when      Nr_fld   = 4
016600090206 **  c                   eval      campo_04 = SKout_ValFld(sgm)
016700090206     c                   when      Nr_fld   = 5
016800090206 **  c                   eval      campo_05 = SKout_ValFld(sgm)
016900090206     c                   when      Nr_fld   = 6
017000090206 **  c                   eval      campo_06 = SKout_ValFld(sgm)
017100090206     c                   when      Nr_fld   = 7
017200090206 **  c                   eval      campo_07 = SKout_ValFld(sgm)
017300090206     c                   when      Nr_fld   = 8
017400090206 **  c                   eval      campo_08 = SKout_ValFld(sgm)
017500090206     c                   when      Nr_fld   = 9
017600090206 **  c                   eval      campo_09 = SKout_ValFld(sgm)
017700090206     c                   when      Nr_fld   = 10
017800090206 **  c                   eval      campo_10 = SKout_ValFld(sgm)
017900090206     c                   when      Nr_fld   = 11
018000090206 **  c                   eval      campo_11 = SKout_ValFld(sgm)
018100090206     c                   when      Nr_fld   = 12
018200090206 **  c                   eval      campo_12 = SKout_ValFld(sgm)
018300090206     c                   when      Nr_fld   = 13
018400090206 **  c                   eval      campo_13 = SKout_ValFld(sgm)
018500090206     c                   when      Nr_fld   = 14
018600090206 **  c                   eval      campo_14 = SKout_ValFld(sgm)
018700090206     c                   when      Nr_fld   = 15
018800090609 **  c                   eval      campo_15 = %int(%trimR(SKout_ValFld(sgm)))
018900090609     c                   when      Nr_fld   = 16
019000090609 **  c                   eval      campo_16 = %int(%trimR(SKout_ValFld(sgm)))
019100090609     c                   when      Nr_fld   = 17
019200090609 **  c                   eval      campo_17 = SKout_ValFld(sgm)
019300090206     c                   endsl
019400090211     c                   end
019500090206      *
019600090206     c                   endsr
019700090206      * ?------------------------------------------------------------------ */
019800110621      * ?------------------------------------------------------------------ */
019900110621      *?      X non bloccare in nessun caso il traduttore CLIENTI
020000110621      * ?------------------------------------------------------------------ */
020100110621     C     *pssr         BEGSR
020200110621     C
020300110621     C                   eval      esito = 'E'
020400110621     C                   ENDSR     '*CANCL'
020500110621     C
020600110621      *  ------------------------------------------------------------------ */
