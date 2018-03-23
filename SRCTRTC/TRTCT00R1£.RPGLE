000100060614     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000200060614     H BNDDIR('QC2LE')
000300050414     H DECEDIT('0,') DATEDIT(*YMD/)
000400100719      *-------------------------------------------------------------------------*
000500100719      *  TRASCODIFICA : SEGMENTO   richiamando il programma del SEGMENTO        *
000600100719      * Il pgm restituisce la DS specifica del segmento con i campi esplicitati *
000700060609      **?************************************************************************
000800100719      **  Elenco DS di tutti i Segmenti da tradurre
000900100719      **?************************************************************************
001000100811     D edS00BGM      E DS                  prefix(S93A)
001100100811???? D edS00CNI      E DS                  prefix(S93A)
001200100811     D edS00CNT      E DS                  prefix(S93A)
001300100811???? D edS00CUX      E DS                  prefix(S93A)
001400100811     D edS00COM      E DS                  prefix(S93A)
001500100811     D edS00CTA      E DS                  prefix(S93A)
001600100811???? D edS00DGS      E DS                  prefix(S93A)
001700100811     D edS00DTM      E DS                  prefix(S93A)
001800100811???? D edS00EQD      E DS                  prefix(S93A)
001900100811???? D edS00EQN      E DS                  prefix(S93A)
002000100811     D edS00GID      E DS                  prefix(S93A)
002100100811???? D edS00LOC      E DS                  prefix(S93A)
002200100811     D edS00MEA      E DS                  prefix(S93A)
002300100811???? D edS00MOA      E DS                  prefix(S93A)
002400100811     D edS00NAD      E DS                  prefix(S93A)
002500100811???? D edS00PIA      E DS                  prefix(S93A)
002600100811     D edS00PCI      E DS                  prefix(S93A)
002700100811     D edS00RFF      E DS                  prefix(S93A)
002800100811???? D edS00SEL      E DS                  prefix(S93A)
002900100811???? D edS00STS      E DS                  prefix(S93A)
003000100811     D edS00TDT      E DS                  prefix(S93A)
003100100811     D edS00TOD      E DS                  prefix(S93A)
003200100811     D edS00FTX      E DS                  prefix(S93A)
003300100811     D edS00TSR      E DS                  prefix(S93A)
003400100811     D edS00UNA      E DS                  prefix(S93A)
003500100811     D edS00UNB      E DS                  prefix(S93A)
003600100811     D edS00UNH      E DS                  prefix(S93A)
003700100811     D edS00UNT      E DS                  prefix(S93A)
003800100811     D edS00UNZ      E DS                  prefix(S93A)
003900100719      *----------------------------------------------------*
004000100811     D edS96BGM      E DS                  prefix(D96A)
004100100811???? D edS96CNI      E DS                  prefix(D96A)
004200100811     D edS96CNT      E DS                  prefix(D96A)
004300100811     D edS96COM      E DS                  prefix(D96A)
004400100811     D edS96CTA      E DS                  prefix(D96A)
004500100811???? D edS96DGS      E DS                  prefix(D96A)
004600100811     D edS96DIM      E DS                  prefix(D96A)
004700100811     D edS96DOC      E DS                  prefix(D96A)
004800100811     D edS96DTM      E DS                  prefix(D96A)
004900100811???? D edS96EQD      E DS                  prefix(D96A)
005000100811???? D edS96EQN      E DS                  prefix(D96A)
005100100811     D edS96FTX      E DS                  prefix(D96A)
005200100811     D edS96GID      E DS                  prefix(D96A)
005300100811     D edS96GIN      E DS                  prefix(D96A)
005400100811     D edS96GOR      E DS                  prefix(D96A)
005500100811     D edS96HAN      E DS                  prefix(D96A)
005600100811???? D edS96LOC      E DS                  prefix(D96A)
005700100811     D edS96MEA      E DS                  prefix(D96A)
005800100811???? D edS96MOA      E DS                  prefix(D96A)
005900100811     D edS96NAD      E DS                  prefix(D96A)
006000100811     D edS96PCI      E DS                  prefix(D96A)
006100100811???? D edS96PIA      E DS                  prefix(D96A)
006200100811     D edS96RFF      E DS                  prefix(D96A)
006300100811     D edS96RNG      E DS                  prefix(D96A)
006400100811???? D edS96SEL      E DS                  prefix(D96A)
006500100811     D edS96TCC      E DS                  prefix(D96A)
006600100811     D edS96TDT      E DS                  prefix(D96A)
006700100811     D edS96TMD      E DS                  prefix(D96A)
006800100811     D edS96TMP      E DS                  prefix(D96A)
006900100811     D edS96TOD      E DS                  prefix(D96A)
007000100811     D edS96TSR      E DS                  prefix(D96A)
007100100811     D edS96UNA      E DS                  prefix(D96A)
007200100811     D edS96UNB      E DS                  prefix(D96A)
007300100811     D edS96UNH      E DS                  prefix(D96A)
007400100811     D edS96UNT      E DS                  prefix(D96A)
007500100811     D edS96UNZ      E DS                  prefix(D96A)
007600100811      *----------------------------------------------------*
007700100719     D Errore          s              1
007800100719     d  esito          s              1
007900090204     d  Segmento       s           2048
008000100719     d  DsGenerica     s           2048
008100100719     D Trovato_seg     S              1
008200100719     d  Tipo_seg       s              3
008300100716     d keyUNBCLI       s             35
008400100716     d keyTIPOMSG      s              6
008500100716     d keyVERSION      s              3
008600100716     d keyRELEASE      s              3
008700100716     d keyAGENCY       s              3
008800100716     d keyASSOCIA      s              6
008900100719     D Valori_inErr    ds
009000100719     D  SKout_Errori                  1    DIM(50)
009100100719     D Descr_Errore    ds
009200100719     D  SKout_DesErr                 50    DIM(50)
009300060612      *-------------------
009400000223     D W0140           S             14  0
009500991129     D WORA            S              6  0
009600991129     D WDTGIO          S              8  0
009700991129     D DATEU           S              8  0
009800991129     D DATA_eur        S               D   DATFMT(*eur)
009900060612      * ?================================================================== */
010000060612      * ?   ( Descrizione Campi x scomposizione FLAT record )
010100100719      * ?================================================================== */
010200060612      * ?   * Campi x decodifica * (INPUT  del Record)
010300090130     D  Dati           s           2048
010400090130     D Separa_segmen   s              1
010500060612     D Decimal_Separ   s              1
010600090204     d  lung_Dati      s              5s 0
010700060612      * ?* ------------------------------------------------------ *
010800060710     D Digits          C                   '0123456789'
010900060612      * ?================================================================== */
011000060612      *   Ciclo principale
011100090203      * ?================================================================== */
011200090203     c     *ENTRY        PLIST
011300100719     c                   parm                    tipo_seg
011400100719     c                   parm                    segmento
011500100719     C                   parm                    keyUNBCLI
011600100719     C                   parm                    keyTIPOMSG
011700100719     C                   parm                    keyVERSION
011800100719     C                   parm                    keyRELEASE
011900100719     C                   parm                    keyAGENCY
012000100719     C                   parm                    keyASSOCIA
012100100719      *output
012200100719     c                   parm                    Errore
012300100719     c                   parm                    DsGenerica
012400100719     c                   parm                    Valori_inErr
012500100719     c                   parm                    Descr_Errore
012600100719     c                   parm                    trovato_seg
012700100719      *
012800100719      **--------
012900090203     C                   clear                   Esito
013000100719     C                   clear                   Trovato_seg
013100100719     c                   clear                   DsGenerica
013200100719     c                   clear                   Errore
013300100719     c                   clear                   Valori_inErr
013400100719     c                   clear                   Descr_Errore
013500100719      **--------
013600100719     c                   clear                   edS00BGM
013700100719     c                   clear                   edS00CNI
013800100719     c                   clear                   edS00CNT
013900100719     c                   clear                   edS00CUX
014000100719     c                   clear                   edS00COM
014100100719     c                   clear                   edS00CTA
014200100719     c                   clear                   edS00DGS
014300100719     c                   clear                   edS00DTM
014400100719     c                   clear                   edS00EQD
014500100719     c                   clear                   edS00EQN
014600100719     c                   clear                   edS00GID
014700100719     c                   clear                   edS00LOC
014800100719     c                   clear                   edS00MEA
014900100719     c                   clear                   edS00MOA
015000100719     c                   clear                   edS00NAD
015100100719     c                   clear                   edS00PIA
015200100719     c                   clear                   edS00PCI
015300100719     c                   clear                   edS00RFF
015400100719     c                   clear                   edS00SEL
015500100723     c                   clear                   edS00STS
015600100719     c                   clear                   edS00TDT
015700100719     c                   clear                   edS00TOD
015800100719     c                   clear                   edS00FTX
015900100719     c                   clear                   edS00TSR
016000100719     c                   clear                   edS00UNA
016100100719     c                   clear                   edS00UNB
016200100719     c                   clear                   edS00UNH
016300100719     c                   clear                   edS00UNT
016400100719     c                   clear                   edS00UNZ
016500100811      **--------
016600100811     c                   clear                   edS96BGM
016700100811     c                   clear                   edS96CNI
016800100811     c                   clear                   edS96CNT
016900100811     c                   clear                   edS96COM
017000100811     c                   clear                   edS96CTA
017100100811     c                   clear                   edS96DGS
017200100811     c                   clear                   edS96DIM
017300100811     c                   clear                   edS96DOC
017400100811     c                   clear                   edS96DTM
017500100811     c                   clear                   edS96EQD
017600100811     c                   clear                   edS96EQN
017700100811     c                   clear                   edS96FTX
017800100811     c                   clear                   edS96GID
017900100811     c                   clear                   edS96GIN
018000100811     c                   clear                   edS96GOR
018100100811     c                   clear                   edS96HAN
018200100811     c                   clear                   edS96LOC
018300100811     c                   clear                   edS96MEA
018400100811     c                   clear                   edS96MOA
018500100811     c                   clear                   edS96NAD
018600100811     c                   clear                   edS96PCI
018700100811     c                   clear                   edS96PIA
018800100811     c                   clear                   edS96RFF
018900100811     c                   clear                   edS96RNG
019000100811     c                   clear                   edS96SEL
019100100811     c                   clear                   edS96TCC
019200100811     c                   clear                   edS96TDT
019300100811     c                   clear                   edS96TMD
019400100811     c                   clear                   edS96TMP
019500100811     c                   clear                   edS96TOD
019600100811     c                   clear                   edS96TSR
019700100811     c                   clear                   edS96UNA
019800100811     c                   clear                   edS96UNB
019900100811     c                   clear                   edS96UNH
020000100811     c                   clear                   edS96UNT
020100100811     c                   clear                   edS96UNZ
020200100719      **--------                                 ---------
020300100719     c                   move      'S'           Trovato_Seg
020400100811      **--------                                 ---------
020500110530     C                   IF              keyTIPOMSG = *blank
020600100811     C                             and   keyVERSION = *blank
020700100811     C                             and   keyRELEASE = *blank
020800100811     C                             and   keyAGENCY  = *blank
020900100811     C                             and   keyASSOCIA = *blank
021000100811     c                   exsr      Vers_S93A
021100100811     c*
021200110530     C                   elseIf          keyTIPOMSG = 'IFCSUM'
021300100811     C                             and   keyVERSION = 'D'
021400100811     C                             and   keyRELEASE = '96A'
021500100811     C                             and   keyAGENCY  = 'UN'
021600100811     C                             and   keyASSOCIA = *blank
021700100811     c                   exsr      Vers_D96A
021800100811     c*
021900100811     c                   end
022000100719      **
022100090203     c     FINE          tag
022200100719     c                   SETON                                        LR
022300090203      * ?------------------------------------------------------------------ */
022400100719      *   DEFINIZIONE CHIAVI                                                *
022500100719      * ?------------------------------------------------------------------ */
022600090206     C     *INZSR        BEGSR
022700090206      *------------------
022800971216      * Recupero data e ora
022900971216     C                   TIME                    WORA
023000991124     C                   TIME                    W0140
023100991124      * UDATE IN GGMMAAAA
023200991124     C                   MOVE      W0140         WDTGIO
023300991124      * UDATE IN AAAAMMGG
023400991124     C     *eur          MOVEL     WDTGIO        DATA_eur
023500991124     C     *iso          MOVEL     DATA_eur      dateu
023600050414      *
023700991124     C                   ENDSR
023800100811      * ?------------------------------------------------------------------ */
023900100811      *?    IFCSUM S93A lo standard BARTOLINI EEX
024000100811      * ?------------------------------------------------------------------ */
024100100811     c     VERS_S93A     BEGSR
024200100811      *
024300100811      **--------                                 ---------
024400100811     c                   select
024500100811      *
024600100811      *
024700100811     c                   when      tipo_seg = 'UNA'
024800100811      *
024900100811     c                   call      'TRTCTUNAR'
025000100811     c                   parm                    tipo_seg
025100100811     c                   parm                    segmento
025200100811     C                   parm                    keyUNBCLI
025300100811     C                   parm                    keyTIPOMSG
025400100811     C                   parm                    keyVERSION
025500100811     C                   parm                    keyRELEASE
025600100811     C                   parm                    keyAGENCY
025700100811     C                   parm                    keyASSOCIA
025800100811     c                   parm                    Errore
025900100811     c                   parm                    EDS00UNA
026000100811     c                   parm                    Valori_inErr
026100100811     c                   parm                    Descr_Errore
026200100811      *
026300100811     C                   eval       DsGenerica = EDS00UNA
026400100811      *
026500100811      * --------
026600100811      *
026700100811     c                   when      tipo_seg = 'UNB'
026800100811      *
026900100811     c                   call      'TRTCTUNBR'
027000100811     c                   parm                    tipo_seg
027100100811     c                   parm                    segmento
027200100811     C                   parm                    keyUNBCLI
027300100811     C                   parm                    keyTIPOMSG
027400100811     C                   parm                    keyVERSION
027500100811     C                   parm                    keyRELEASE
027600100811     C                   parm                    keyAGENCY
027700100811     C                   parm                    keyASSOCIA
027800100811     c                   parm                    Errore
027900100811     c                   parm                    EDS00UNB
028000100811     c                   parm                    Valori_inErr
028100100811     c                   parm                    Descr_Errore
028200100811      *
028300100811     C                   eval       DsGenerica = EDS00UNB
028400100811      *
028500100811      * --------
028600100811      *
028700100811     c                   when      tipo_seg = 'UNH'
028800100811      *
028900100811     c                   call      'TRTCTUNHR'
029000100811     c                   parm                    tipo_seg
029100100811     c                   parm                    segmento
029200100811     c                   parm                    keyUNBCLI
029300100811     C                   parm                    keyTIPOMSG
029400100811     C                   parm                    keyVERSION
029500100811     C                   parm                    keyRELEASE
029600100811     C                   parm                    keyAGENCY
029700100811     C                   parm                    keyASSOCIA
029800100811     c                   parm                    Errore
029900100811     c                   parm                    EDS00UNH
030000100811     c                   parm                    Valori_inErr
030100100811     c                   parm                    Descr_Errore
030200100811      *
030300100811     C                   eval       DsGenerica = EDS00UNH
030400100811      *
030500100811      * --------
030600100811      *
030700100811     c                   when      tipo_seg = 'BGM'
030800100811      *
030900100811     c                   call      'TRTCTBGMR'
031000100811     c                   parm                    tipo_seg
031100100811     c                   parm                    segmento
031200100811     c                   parm                    keyUNBCLI
031300100811     C                   parm                    keyTIPOMSG
031400100811     C                   parm                    keyVERSION
031500100811     C                   parm                    keyRELEASE
031600100811     C                   parm                    keyAGENCY
031700100811     C                   parm                    keyASSOCIA
031800100811     c                   parm                    Errore
031900100811     c                   parm                    EDS00BGM
032000100811     c                   parm                    Valori_inErr
032100100811     c                   parm                    Descr_Errore
032200100811      *
032300100811     C                   eval       DsGenerica = EDS00BGM
032400100811      *
032500100811      **--------
032600100811      *
032700100811     c                   when      tipo_seg = 'DTM'
032800100811      *
032900100811     c                   call      'TRTCTDTMR'
033000100811     c                   parm                    tipo_seg
033100100811     c                   parm                    segmento
033200100811     c                   parm                    keyUNBCLI
033300100811     C                   parm                    keyTIPOMSG
033400100811     C                   parm                    keyVERSION
033500100811     C                   parm                    keyRELEASE
033600100811     C                   parm                    keyAGENCY
033700100811     C                   parm                    keyASSOCIA
033800100811     c                   parm                    Errore
033900100811     c                   parm                    EDS00DTM
034000100811     c                   parm                    Valori_inErr
034100100811     c                   parm                    Descr_Errore
034200100811      *
034300100811     C                   eval       DsGenerica = EDS00DTM
034400100811      *
034500100811      * --------
034600100811      *
034700100811     c                   when      tipo_seg = 'TSR'
034800100811      *
034900100811     c                   call      'TRTCTTSRR'
035000100811     c                   parm                    tipo_seg
035100100811     c                   parm                    segmento
035200100811     c                   parm                    keyUNBCLI
035300100811     C                   parm                    keyTIPOMSG
035400100811     C                   parm                    keyVERSION
035500100811     C                   parm                    keyRELEASE
035600100811     C                   parm                    keyAGENCY
035700100811     C                   parm                    keyASSOCIA
035800100811     c                   parm                    Errore
035900100811     c                   parm                    EDS00TSR
036000100811     c                   parm                    Valori_inErr
036100100811     c                   parm                    Descr_Errore
036200100811      *
036300100811     C                   eval       DsGenerica = EDS00TSR
036400100811      *
036500100811      **--------
036600100811      *
036700100811     c                   when      tipo_seg = 'CNT'
036800100811      *
036900100811     c                   call      'TRTCTCNTR'
037000100811     c                   parm                    tipo_seg
037100100811     c                   parm                    segmento
037200100811     c                   parm                    keyUNBCLI
037300100811     C                   parm                    keyTIPOMSG
037400100811     C                   parm                    keyVERSION
037500100811     C                   parm                    keyRELEASE
037600100811     C                   parm                    keyAGENCY
037700100811     C                   parm                    keyASSOCIA
037800100811     c                   parm                    Errore
037900100811     c                   parm                    EDS00CNT
038000100811     c                   parm                    Valori_inErr
038100100811     c                   parm                    Descr_Errore
038200100811      *
038300100811     C                   eval       DsGenerica = EDS00CNT
038400100811      *
038500100811      **--------
038600100811      *
038700100811     c                   when      tipo_seg = 'CUX'
038800100811      *
038900100811     c                   call      'TRTCTCUXR'
039000100811     c                   parm                    tipo_seg
039100100811     c                   parm                    segmento
039200100811     C                   parm                    keyUNBCLI
039300100811     C                   parm                    keyTIPOMSG
039400100811     C                   parm                    keyVERSION
039500100811     C                   parm                    keyRELEASE
039600100811     C                   parm                    keyAGENCY
039700100811     C                   parm                    keyASSOCIA
039800100811     c                   parm                    Errore
039900100811     c                   parm                    EDS00CUX
040000100811     c                   parm                    Valori_inErr
040100100811     c                   parm                    Descr_Errore
040200100811      *
040300100811     C                   eval       DsGenerica = EDS00CUX
040400100811      *
040500100811      **--------
040600100811      *
040700100811     c                   when      tipo_seg = 'TOD'
040800100811      *
040900100811     c                   call      'TRTCTTODR'
041000100811     c                   parm                    tipo_seg
041100100811     c                   parm                    segmento
041200100811     c                   parm                    keyUNBCLI
041300100811     C                   parm                    keyTIPOMSG
041400100811     C                   parm                    keyVERSION
041500100811     C                   parm                    keyRELEASE
041600100811     C                   parm                    keyAGENCY
041700100811     C                   parm                    keyASSOCIA
041800100811     c                   parm                    Errore
041900100811     c                   parm                    EDS00TOD
042000100811     c                   parm                    Valori_inErr
042100100811     c                   parm                    Descr_Errore
042200100811      *
042300100811     C                   eval       DsGenerica = EDS00TOD
042400100811      *
042500100811      **--------
042600100811      *
042700100811     c                   when      tipo_seg = 'RFF'
042800100811      *
042900100811     c                   call      'TRTCTRFFR'
043000100811     c                   parm                    tipo_seg
043100100811     c                   parm                    segmento
043200100811     c                   parm                    keyUNBCLI
043300100811     C                   parm                    keyTIPOMSG
043400100811     C                   parm                    keyVERSION
043500100811     C                   parm                    keyRELEASE
043600100811     C                   parm                    keyAGENCY
043700100811     C                   parm                    keyASSOCIA
043800100811     c                   parm                    Errore
043900100811     c                   parm                    EDS00RFF
044000100811     c                   parm                    Valori_inErr
044100100811     c                   parm                    Descr_Errore
044200100811      *
044300100811     C                   eval       DsGenerica = EDS00RFF
044400100811      *
044500100811      **--------
044600100811      *
044700100811     c                   when      tipo_seg = 'TDT'
044800100811      *
044900100811     c                   call      'TRTCTTDTR'
045000100811     c                   parm                    tipo_seg
045100100811     c                   parm                    segmento
045200100811     c                   parm                    keyUNBCLI
045300100811     C                   parm                    keyTIPOMSG
045400100811     C                   parm                    keyVERSION
045500100811     C                   parm                    keyRELEASE
045600100811     C                   parm                    keyAGENCY
045700100811     C                   parm                    keyASSOCIA
045800100811     c                   parm                    Errore
045900100811     c                   parm                    EDS00TDT
046000100811     c                   parm                    Valori_inErr
046100100811     c                   parm                    Descr_Errore
046200100811      *
046300100811     C                   eval       DsGenerica = EDS00TDT
046400100811      *
046500100811      **--------
046600100811      *
046700100811     c                   when      tipo_seg = 'MOA'
046800100811      *
046900100811     c                   call      'TRTCTMOAR'
047000100811     c                   parm                    tipo_seg
047100100811     c                   parm                    segmento
047200100811     c                   parm                    keyUNBCLI
047300100811     C                   parm                    keyTIPOMSG
047400100811     C                   parm                    keyVERSION
047500100811     C                   parm                    keyRELEASE
047600100811     C                   parm                    keyAGENCY
047700100811     C                   parm                    keyASSOCIA
047800100811     c                   parm                    Errore
047900100811     c                   parm                    EDS00MOA
048000100811     c                   parm                    Valori_inErr
048100100811     c                   parm                    Descr_Errore
048200100811      *
048300100811     C                   eval       DsGenerica = EDS00MOA
048400100811      *
048500100811      **--------
048600100811      *
048700100811     c                   when      tipo_seg = 'FTX'
048800100811      *
048900100811     c                   call      'TRTCTFTXR'
049000100811     c                   parm                    tipo_seg
049100100811     c                   parm                    segmento
049200100811     c                   parm                    keyUNBCLI
049300100811     C                   parm                    keyTIPOMSG
049400100811     C                   parm                    keyVERSION
049500100811     C                   parm                    keyRELEASE
049600100811     C                   parm                    keyAGENCY
049700100811     C                   parm                    keyASSOCIA
049800100811     c                   parm                    Errore
049900100811     c                   parm                    EDS00FTX
050000100811     c                   parm                    Valori_inErr
050100100811     c                   parm                    Descr_Errore
050200100811      *
050300100811     C                   eval       DsGenerica = EDS00FTX
050400100811      *
050500100811      **--------
050600100811      *
050700100811     c                   when      tipo_seg = 'NAD'
050800100811      *
050900100811     c                   call      'TRTCTNADR'
051000100811     c                   parm                    tipo_seg
051100100811     c                   parm                    segmento
051200100811     c                   parm                    keyUNBCLI
051300100811     C                   parm                    keyTIPOMSG
051400100811     C                   parm                    keyVERSION
051500100811     C                   parm                    keyRELEASE
051600100811     C                   parm                    keyAGENCY
051700100811     C                   parm                    keyASSOCIA
051800100811     c                   parm                    Errore
051900100811     c                   parm                    EDS00NAD
052000100811     c                   parm                    Valori_inErr
052100100811     c                   parm                    Descr_Errore
052200100811      *
052300100811     C                   eval       DsGenerica = EDS00NAD
052400100811      *
052500100811      **--------
052600100811      *
052700100811     c                   when      tipo_seg = 'CTA'
052800100811      *
052900100811     c                   call      'TRTCTCTAR'
053000100811     c                   parm                    tipo_seg
053100100811     c                   parm                    segmento
053200100811     c                   parm                    keyUNBCLI
053300100811     C                   parm                    keyTIPOMSG
053400100811     C                   parm                    keyVERSION
053500100811     C                   parm                    keyRELEASE
053600100811     C                   parm                    keyAGENCY
053700100811     C                   parm                    keyASSOCIA
053800100811     c                   parm                    Errore
053900100811     c                   parm                    EDS00CTA
054000100811     c                   parm                    Valori_inErr
054100100811     c                   parm                    Descr_Errore
054200100811      *
054300100811     C                   eval       DsGenerica = EDS00CTA
054400100811      *
054500100811      **--------
054600100811      *
054700100811     c                   when      tipo_seg = 'COM'
054800100811      *
054900100811     c                   call      'TRTCTCOMR'
055000100811     c                   parm                    tipo_seg
055100100811     c                   parm                    segmento
055200100811     c                   parm                    keyUNBCLI
055300100811     C                   parm                    keyTIPOMSG
055400100811     C                   parm                    keyVERSION
055500100811     C                   parm                    keyRELEASE
055600100811     C                   parm                    keyAGENCY
055700100811     C                   parm                    keyASSOCIA
055800100811     c                   parm                    Errore
055900100811     c                   parm                    EDS00COM
056000100811     c                   parm                    Valori_inErr
056100100811     c                   parm                    Descr_Errore
056200100811      *
056300100811     C                   eval       DsGenerica = EDS00COM
056400100811      *
056500100811      **--------
056600100811      *
056700100811     c                   when      tipo_seg = 'GID'
056800100811      *
056900100811     c                   call      'TRTCTGIDR'
057000100811     c                   parm                    tipo_seg
057100100811     c                   parm                    segmento
057200100811     c                   parm                    keyUNBCLI
057300100811     C                   parm                    keyTIPOMSG
057400100811     C                   parm                    keyVERSION
057500100811     C                   parm                    keyRELEASE
057600100811     C                   parm                    keyAGENCY
057700100811     C                   parm                    keyASSOCIA
057800100811     c                   parm                    Errore
057900100811     c                   parm                    EDS00GID
058000100811     c                   parm                    Valori_inErr
058100100811     c                   parm                    Descr_Errore
058200100811      *
058300100811     C                   eval       DsGenerica = EDS00GID
058400100811      *
058500100811      **--------
058600100811      *
058700100811     c                   when      tipo_seg = 'MEA'
058800100811      *
058900100811     c                   call      'TRTCTMEAR'
059000100811     c                   parm                    tipo_seg
059100100811     c                   parm                    segmento
059200100811     c                   parm                    keyUNBCLI
059300100811     C                   parm                    keyTIPOMSG
059400100811     C                   parm                    keyVERSION
059500100811     C                   parm                    keyRELEASE
059600100811     C                   parm                    keyAGENCY
059700100811     C                   parm                    keyASSOCIA
059800100811     c                   parm                    Errore
059900100811     c                   parm                    EDS00MEA
060000100811     c                   parm                    Valori_inErr
060100100811     c                   parm                    Descr_Errore
060200100811      *
060300100811     C                   eval       DsGenerica = EDS00MEA
060400100811      *
060500100811      **--------
060600100811      *
060700100811     c                   when      tipo_seg = 'PCI'
060800100811      *
060900100811     c                   call      'TRTCTPCIR'
061000100811     c                   parm                    tipo_seg
061100100811     c                   parm                    segmento
061200100811     c                   parm                    keyUNBCLI
061300100811     C                   parm                    keyTIPOMSG
061400100811     C                   parm                    keyVERSION
061500100811     C                   parm                    keyRELEASE
061600100811     C                   parm                    keyAGENCY
061700100811     C                   parm                    keyASSOCIA
061800100811     c                   parm                    Errore
061900100811     c                   parm                    EDS00PCI
062000100811     c                   parm                    Valori_inErr
062100100811     c                   parm                    Descr_Errore
062200100811      *
062300100811     C                   eval       DsGenerica = EDS00PCI
062400100811      *
062500100811      **--------
062600100811      *
062700100811     c                   when      tipo_seg = 'UNT'
062800100811      *
062900100811     c                   call      'TRTCTUNTR'
063000100811     c                   parm                    tipo_seg
063100100811     c                   parm                    segmento
063200100811     c                   parm                    keyUNBCLI
063300100811     C                   parm                    keyTIPOMSG
063400100811     C                   parm                    keyVERSION
063500100811     C                   parm                    keyRELEASE
063600100811     C                   parm                    keyAGENCY
063700100811     C                   parm                    keyASSOCIA
063800100811     c                   parm                    Errore
063900100811     c                   parm                    EDS00UNT
064000100811     c                   parm                    Valori_inErr
064100100811     c                   parm                    Descr_Errore
064200100811      *
064300100811     C                   eval       DsGenerica = EDS00UNT
064400100811      *
064500100811      **--------
064600100811      *
064700100811     c                   when      tipo_seg = 'EQN'
064800100811      *
064900100811     c                   call      'TRTCTEQNR'
065000100811     c                   parm                    tipo_seg
065100100811     c                   parm                    segmento
065200100811     c                   parm                    keyUNBCLI
065300100811     C                   parm                    keyTIPOMSG
065400100811     C                   parm                    keyVERSION
065500100811     C                   parm                    keyRELEASE
065600100811     C                   parm                    keyAGENCY
065700100811     C                   parm                    keyASSOCIA
065800100811     c                   parm                    Errore
065900100811     c                   parm                    EDS00EQN
066000100811     c                   parm                    Valori_inErr
066100100811     c                   parm                    Descr_Errore
066200100811      *
066300100811     C                   eval       DsGenerica = EDS00EQN
066400100811      *
066500100811      **--------
066600100811      *
066700100811     c                   when      tipo_seg = 'EQD'
066800100811      *
066900100811     c                   call      'TRTCTEQDR'
067000100811     c                   parm                    tipo_seg
067100100811     c                   parm                    segmento
067200100811     c                   parm                    keyUNBCLI
067300100811     C                   parm                    keyTIPOMSG
067400100811     C                   parm                    keyVERSION
067500100811     C                   parm                    keyRELEASE
067600100811     C                   parm                    keyAGENCY
067700100811     C                   parm                    keyASSOCIA
067800100811     c                   parm                    Errore
067900100811     c                   parm                    EDS00EQD
068000100811     c                   parm                    Valori_inErr
068100100811     c                   parm                    Descr_Errore
068200100811      *
068300100811     C                   eval       DsGenerica = EDS00EQD
068400100811      *
068500100811      **--------
068600100811      *
068700100811     c                   when      tipo_seg = 'LOC'
068800100811      *
068900100811     c                   call      'TRTCTLOCR'
069000100811     c                   parm                    tipo_seg
069100100811     c                   parm                    segmento
069200100811     c                   parm                    keyUNBCLI
069300100811     C                   parm                    keyTIPOMSG
069400100811     C                   parm                    keyVERSION
069500100811     C                   parm                    keyRELEASE
069600100811     C                   parm                    keyAGENCY
069700100811     C                   parm                    keyASSOCIA
069800100811     c                   parm                    Errore
069900100811     c                   parm                    EDS00LOC
070000100811     c                   parm                    Valori_inErr
070100100811     c                   parm                    Descr_Errore
070200100811      *
070300100811     C                   eval       DsGenerica = EDS00LOC
070400100811      *
070500100811      **--------
070600100811      *
070700100811     c                   when      tipo_seg = 'PIA'
070800100811      *
070900100811     c                   call      'TRTCTPIAR'
071000100811     c                   parm                    tipo_seg
071100100811     c                   parm                    segmento
071200100811     c                   parm                    keyUNBCLI
071300100811     C                   parm                    keyTIPOMSG
071400100811     C                   parm                    keyVERSION
071500100811     C                   parm                    keyRELEASE
071600100811     C                   parm                    keyAGENCY
071700100811     C                   parm                    keyASSOCIA
071800100811     c                   parm                    Errore
071900100811     c                   parm                    EDS00PIA
072000100811     c                   parm                    Valori_inErr
072100100811     c                   parm                    Descr_Errore
072200100811      *
072300100811     C                   eval       DsGenerica = EDS00PIA
072400100811      *
072500100811      **--------
072600100811      *
072700100811     c                   when      tipo_seg = 'SEL'
072800100811      *
072900100811     c                   call      'TRTCTSELR'
073000100811     c                   parm                    tipo_seg
073100100811     c                   parm                    segmento
073200100811     c                   parm                    keyUNBCLI
073300100811     C                   parm                    keyTIPOMSG
073400100811     C                   parm                    keyVERSION
073500100811     C                   parm                    keyRELEASE
073600100811     C                   parm                    keyAGENCY
073700100811     C                   parm                    keyASSOCIA
073800100811     c                   parm                    Errore
073900100811     c                   parm                    EDS00SEL
074000100811     c                   parm                    Valori_inErr
074100100811     c                   parm                    Descr_Errore
074200100811      *
074300100811     C                   eval       DsGenerica = EDS00SEL
074400100811      *
074500100811      **--------
074600100811      *
074700100811     c                   when      tipo_seg = 'STS'
074800100811      *
074900100811     c                   call      'TRTCTSTSR'
075000100811     c                   parm                    tipo_seg
075100100811     c                   parm                    segmento
075200100811     c                   parm                    keyUNBCLI
075300100811     C                   parm                    keyTIPOMSG
075400100811     C                   parm                    keyVERSION
075500100811     C                   parm                    keyRELEASE
075600100811     C                   parm                    keyAGENCY
075700100811     C                   parm                    keyASSOCIA
075800100811     c                   parm                    Errore
075900100811     c                   parm                    EDS00STS
076000100811     c                   parm                    Valori_inErr
076100100811     c                   parm                    Descr_Errore
076200100811      *
076300100811     C                   eval       DsGenerica = EDS00STS
076400100811      *
076500100811      **--------
076600100811      *
076700100811     c                   when      tipo_seg = 'DGS'
076800100811      *
076900100811     c                   call      'TRTCTDGSR'
077000100811     c                   parm                    tipo_seg
077100100811     c                   parm                    segmento
077200100811     c                   parm                    keyUNBCLI
077300100811     C                   parm                    keyTIPOMSG
077400100811     C                   parm                    keyVERSION
077500100811     C                   parm                    keyRELEASE
077600100811     C                   parm                    keyAGENCY
077700100811     C                   parm                    keyASSOCIA
077800100811     c                   parm                    Errore
077900100811     c                   parm                    EDS00DGS
078000100811     c                   parm                    Valori_inErr
078100100811     c                   parm                    Descr_Errore
078200100811      *
078300100811     C                   eval       DsGenerica = EDS00DGS
078400100811      *
078500100811      **--------
078600100811      *
078700100811     c                   when      tipo_seg = 'CNI'
078800100811      *
078900100811     c                   call      'TRTCTCNIR'
079000100811     c                   parm                    tipo_seg
079100100811     c                   parm                    segmento
079200100811     c                   parm                    keyUNBCLI
079300100811     C                   parm                    keyTIPOMSG
079400100811     C                   parm                    keyVERSION
079500100811     C                   parm                    keyRELEASE
079600100811     C                   parm                    keyAGENCY
079700100811     C                   parm                    keyASSOCIA
079800100811     c                   parm                    Errore
079900100811     c                   parm                    EDS00CNI
080000100811     c                   parm                    Valori_inErr
080100100811     c                   parm                    Descr_Errore
080200100811      *
080300100811     C                   eval       DsGenerica = EDS00CNI
080400100811      *
080500100811      **--------
080600100811      *
080700100811     c                   when      tipo_seg = 'UNZ'
080800100811      *
080900100811     c                   call      'TRTCTUNZR'
081000100811     c                   parm                    tipo_seg
081100100811     c                   parm                    segmento
081200100811     c                   parm                    keyUNBCLI
081300100811     C                   parm                    keyTIPOMSG
081400100811     C                   parm                    keyVERSION
081500100811     C                   parm                    keyRELEASE
081600100811     C                   parm                    keyAGENCY
081700100811     C                   parm                    keyASSOCIA
081800100811     c                   parm                    Errore
081900100811     c                   parm                    EDS00UNZ
082000100811     c                   parm                    Valori_inErr
082100100811     c                   parm                    Descr_Errore
082200100811      *
082300100811     C                   eval       DsGenerica = EDS00UNZ
082400100811      *
082500100811      **--------
082600100811      *
082700100811     c                   OTHER
082800100811      **--------
082900100811     c                   move      'N'           Trovato_Seg
083000100811     c                   move      'E'           Errore
083100100811     c                   eval       Descr_Errore = 'SGM non trovato'
083200100811     c                   eval       Valori_inErr = tipo_seg
083300100811      **--------
083400100811     c                   endSL
083500100811      *
083600100811     C                   ENDSR
083700060621      * ?------------------------------------------------------------------ */
083800100811      *?    IFCSUM D96A lo standard successivo      (da LVMH in poi)
083900060621      * ?------------------------------------------------------------------ */
084000100811     c     VERS_D96A     BEGSR
084100100811      *
084200100811     c                   select
084300100811      **--------                                 ---------
084400100811      *
084500100811     c                   when      tipo_seg = 'BGM'
084600100811      *
084700100811     c                   call      'TRTCT96BGM'
084800100811     c                   parm                    tipo_seg
084900100811     c                   parm                    segmento
085000100811     c                   parm                    keyUNBCLI
085100100811     C                   parm                    keyTIPOMSG
085200100811     C                   parm                    keyVERSION
085300100811     C                   parm                    keyRELEASE
085400100811     C                   parm                    keyAGENCY
085500100811     C                   parm                    keyASSOCIA
085600100811     c                   parm                    Errore
085700100811     c                   parm                    EDS96BGM
085800100811     c                   parm                    Valori_inErr
085900100811     c                   parm                    Descr_Errore
086000100811      *
086100100811     C                   eval       DsGenerica = EDS96BGM
086200100811      *
086300100811      * --------
086400100811      *
086500100811     c                   when      tipo_seg = 'CNI'
086600100811      *
086700100920     c                   call      'TRTCT96CNI'
086800100811     c                   parm                    tipo_seg
086900100811     c                   parm                    segmento
087000100811     c                   parm                    keyUNBCLI
087100100811     C                   parm                    keyTIPOMSG
087200100811     C                   parm                    keyVERSION
087300100811     C                   parm                    keyRELEASE
087400100811     C                   parm                    keyAGENCY
087500100811     C                   parm                    keyASSOCIA
087600100811     c                   parm                    Errore
087700100811     c                   parm                    EDS96CNI
087800100811     c                   parm                    Valori_inErr
087900100811     c                   parm                    Descr_Errore
088000100811      *
088100100811     C                   eval       DsGenerica = EDS96CNI
088200100811      *
088300100811      * --------
088400100811      *
088500100811     c                   when      tipo_seg = 'CNT'
088600100811      *
088700100811     c                   call      'TRTCT96CNT'
088800100811     c                   parm                    tipo_seg
088900100811     c                   parm                    segmento
089000100811     c                   parm                    keyUNBCLI
089100100811     C                   parm                    keyTIPOMSG
089200100811     C                   parm                    keyVERSION
089300100811     C                   parm                    keyRELEASE
089400100811     C                   parm                    keyAGENCY
089500100811     C                   parm                    keyASSOCIA
089600100811     c                   parm                    Errore
089700100811     c                   parm                    EDS96CNT
089800100811     c                   parm                    Valori_inErr
089900100811     c                   parm                    Descr_Errore
090000100811      *
090100100811     C                   eval       DsGenerica = EDS96CNT
090200100811      *
090300100811      * --------
090400100811      *
090500100811     c                   when      tipo_seg = 'COM'
090600100811      *
090700100811     c                   call      'TRTCT96COM'
090800100811     c                   parm                    tipo_seg
090900100811     c                   parm                    segmento
091000100811     c                   parm                    keyUNBCLI
091100100811     C                   parm                    keyTIPOMSG
091200100811     C                   parm                    keyVERSION
091300100811     C                   parm                    keyRELEASE
091400100811     C                   parm                    keyAGENCY
091500100811     C                   parm                    keyASSOCIA
091600100811     c                   parm                    Errore
091700100811     c                   parm                    EDS96COM
091800100811     c                   parm                    Valori_inErr
091900100811     c                   parm                    Descr_Errore
092000100811      *
092100100811     C                   eval       DsGenerica = EDS96COM
092200100811      *
092300100811      * --------
092400100811      *
092500100811     c                   when      tipo_seg = 'CTA'
092600100811      *
092700100811     c                   call      'TRTCT96CTA'
092800100811     c                   parm                    tipo_seg
092900100811     c                   parm                    segmento
093000100811     c                   parm                    keyUNBCLI
093100100811     C                   parm                    keyTIPOMSG
093200100811     C                   parm                    keyVERSION
093300100811     C                   parm                    keyRELEASE
093400100811     C                   parm                    keyAGENCY
093500100811     C                   parm                    keyASSOCIA
093600100811     c                   parm                    Errore
093700100811     c                   parm                    EDS96CTA
093800100811     c                   parm                    Valori_inErr
093900100811     c                   parm                    Descr_Errore
094000100811      *
094100100811     C                   eval       DsGenerica = EDS96CTA
094200100811      *
094300100811      * --------
094400100811      *
094500100811     c                   when      tipo_seg = 'DGS'
094600100811      *
094700100920     c                   call      'TRTCT96DGS'
094800100811     c                   parm                    tipo_seg
094900100811     c                   parm                    segmento
095000100811     c                   parm                    keyUNBCLI
095100100811     C                   parm                    keyTIPOMSG
095200100811     C                   parm                    keyVERSION
095300100811     C                   parm                    keyRELEASE
095400100811     C                   parm                    keyAGENCY
095500100811     C                   parm                    keyASSOCIA
095600100811     c                   parm                    Errore
095700100811     c                   parm                    EDS96DGS
095800100811     c                   parm                    Valori_inErr
095900100811     c                   parm                    Descr_Errore
096000100811      *
096100100811     C                   eval       DsGenerica = EDS96DGS
096200100811      *
096300100811      * --------
096400100811      *
096500100811     c                   when      tipo_seg = 'DIM'
096600100811      *
096700100920     c                   call      'TRTCT96DIM'
096800100811     c                   parm                    tipo_seg
096900100811     c                   parm                    segmento
097000100811     c                   parm                    keyUNBCLI
097100100811     C                   parm                    keyTIPOMSG
097200100811     C                   parm                    keyVERSION
097300100811     C                   parm                    keyRELEASE
097400100811     C                   parm                    keyAGENCY
097500100811     C                   parm                    keyASSOCIA
097600100811     c                   parm                    Errore
097700100811     c                   parm                    EDS96DIM
097800100811     c                   parm                    Valori_inErr
097900100811     c                   parm                    Descr_Errore
098000100811      *
098100100811     C                   eval       DsGenerica = EDS96DIM
098200100811      *
098300100811      * --------
098400100811      *
098500100811     c                   when      tipo_seg = 'DOC'
098600100811      *
098700100920     c                   call      'TRTCT96DOC'
098800100811     c                   parm                    tipo_seg
098900100811     c                   parm                    segmento
099000100811     c                   parm                    keyUNBCLI
099100100811     C                   parm                    keyTIPOMSG
099200100811     C                   parm                    keyVERSION
099300100811     C                   parm                    keyRELEASE
099400100811     C                   parm                    keyAGENCY
099500100811     C                   parm                    keyASSOCIA
099600100811     c                   parm                    Errore
099700100811     c                   parm                    EDS96DOC
099800100811     c                   parm                    Valori_inErr
099900100811     c                   parm                    Descr_Errore
100000100811      *
100100100811     C                   eval       DsGenerica = EDS96DOC
100200100811      *
100300100811      * --------
100400100811      *
100500100811     c                   when      tipo_seg = 'DTM'
100600100811      *
100700100811     c                   call      'TRTCT96DTM'
100800100811     c                   parm                    tipo_seg
100900100811     c                   parm                    segmento
101000100811     c                   parm                    keyUNBCLI
101100100811     C                   parm                    keyTIPOMSG
101200100811     C                   parm                    keyVERSION
101300100811     C                   parm                    keyRELEASE
101400100811     C                   parm                    keyAGENCY
101500100811     C                   parm                    keyASSOCIA
101600100811     c                   parm                    Errore
101700100811     c                   parm                    EDS96DTM
101800100811     c                   parm                    Valori_inErr
101900100811     c                   parm                    Descr_Errore
102000100811      *
102100100811     C                   eval       DsGenerica = EDS96DTM
102200100811      *
102300100811      * --------
102400100811      *
102500100811     c                   when      tipo_seg = 'EQD'
102600100811      *
102700100811     c                   call      'TRTCT96EQD'
102800100811     c                   parm                    tipo_seg
102900100811     c                   parm                    segmento
103000100811     c                   parm                    keyUNBCLI
103100100811     C                   parm                    keyTIPOMSG
103200100811     C                   parm                    keyVERSION
103300100811     C                   parm                    keyRELEASE
103400100811     C                   parm                    keyAGENCY
103500100811     C                   parm                    keyASSOCIA
103600100811     c                   parm                    Errore
103700100811     c                   parm                    EDS96EQD
103800100811     c                   parm                    Valori_inErr
103900100811     c                   parm                    Descr_Errore
104000100811      *
104100100811     C                   eval       DsGenerica = EDS96EQD
104200100811      *
104300100811      * --------
104400100811      *
104500100811     c                   when      tipo_seg = 'EQN'
104600100811      *
104700100811     c                   call      'TRTCT96EQN'
104800100811     c                   parm                    tipo_seg
104900100811     c                   parm                    segmento
105000100811     c                   parm                    keyUNBCLI
105100100811     C                   parm                    keyTIPOMSG
105200100811     C                   parm                    keyVERSION
105300100811     C                   parm                    keyRELEASE
105400100811     C                   parm                    keyAGENCY
105500100811     C                   parm                    keyASSOCIA
105600100811     c                   parm                    Errore
105700100811     c                   parm                    EDS96EQN
105800100811     c                   parm                    Valori_inErr
105900100811     c                   parm                    Descr_Errore
106000100811      *
106100100811     C                   eval       DsGenerica = EDS96EQN
106200100811      *
106300100811      * --------
106400100811      *
106500100811     c                   when      tipo_seg = 'FTX'
106600100811      *
106700100811     c                   call      'TRTCT96FTX'
106800100811     c                   parm                    tipo_seg
106900100811     c                   parm                    segmento
107000100811     c                   parm                    keyUNBCLI
107100100811     C                   parm                    keyTIPOMSG
107200100811     C                   parm                    keyVERSION
107300100811     C                   parm                    keyRELEASE
107400100811     C                   parm                    keyAGENCY
107500100811     C                   parm                    keyASSOCIA
107600100811     c                   parm                    Errore
107700100811     c                   parm                    EDS96FTX
107800100811     c                   parm                    Valori_inErr
107900100811     c                   parm                    Descr_Errore
108000100811      *
108100100811     C                   eval       DsGenerica = EDS96FTX
108200100811      *
108300100811      * --------
108400100811      *
108500100811     c                   when      tipo_seg = 'GID'
108600100811      *
108700100811     c                   call      'TRTCT96GID'
108800100811     c                   parm                    tipo_seg
108900100811     c                   parm                    segmento
109000100811     c                   parm                    keyUNBCLI
109100100811     C                   parm                    keyTIPOMSG
109200100811     C                   parm                    keyVERSION
109300100811     C                   parm                    keyRELEASE
109400100811     C                   parm                    keyAGENCY
109500100811     C                   parm                    keyASSOCIA
109600100811     c                   parm                    Errore
109700100811     c                   parm                    EDS96GID
109800100811     c                   parm                    Valori_inErr
109900100811     c                   parm                    Descr_Errore
110000100811      *
110100100811     C                   eval       DsGenerica = EDS96GID
110200100811      *
110300100811      * --------
110400100811      *
110500100811     c                   when      tipo_seg = 'GIN'
110600100811      *
110700100811     c                   call      'TRTCT96GIN'
110800100811     c                   parm                    tipo_seg
110900100811     c                   parm                    segmento
111000100811     c                   parm                    keyUNBCLI
111100100811     C                   parm                    keyTIPOMSG
111200100811     C                   parm                    keyVERSION
111300100811     C                   parm                    keyRELEASE
111400100811     C                   parm                    keyAGENCY
111500100811     C                   parm                    keyASSOCIA
111600100811     c                   parm                    Errore
111700100811     c                   parm                    EDS96GIN
111800100811     c                   parm                    Valori_inErr
111900100811     c                   parm                    Descr_Errore
112000100811      *
112100100811     C                   eval       DsGenerica = EDS96GIN
112200100811      *
112300100811      * --------
112400100811      *
112500100811     c                   when      tipo_seg = 'GOR'
112600100811      *
112700100811     c                   call      'TRTCT96GOR'
112800100811     c                   parm                    tipo_seg
112900100811     c                   parm                    segmento
113000100811     c                   parm                    keyUNBCLI
113100100811     C                   parm                    keyTIPOMSG
113200100811     C                   parm                    keyVERSION
113300100811     C                   parm                    keyRELEASE
113400100811     C                   parm                    keyAGENCY
113500100811     C                   parm                    keyASSOCIA
113600100811     c                   parm                    Errore
113700100811     c                   parm                    EDS96GOR
113800100811     c                   parm                    Valori_inErr
113900100811     c                   parm                    Descr_Errore
114000100811      *
114100100811     C                   eval       DsGenerica = EDS96GOR
114200100811      *
114300100811      * --------
114400100811      *
114500100811     c                   when      tipo_seg = 'HAN'
114600100811      *
114700100811     c                   call      'TRTCT96HAN'
114800100811     c                   parm                    tipo_seg
114900100811     c                   parm                    segmento
115000100811     c                   parm                    keyUNBCLI
115100100811     C                   parm                    keyTIPOMSG
115200100811     C                   parm                    keyVERSION
115300100811     C                   parm                    keyRELEASE
115400100811     C                   parm                    keyAGENCY
115500100811     C                   parm                    keyASSOCIA
115600100811     c                   parm                    Errore
115700100811     c                   parm                    EDS96HAN
115800100811     c                   parm                    Valori_inErr
115900100811     c                   parm                    Descr_Errore
116000100811      *
116100100811     C                   eval       DsGenerica = EDS96HAN
116200100811      *
116300100811      * --------
116400100811      *
116500100811     c                   when      tipo_seg = 'LOC'
116600100811      *
116700100811     c                   call      'TRTCT96LOC'
116800100811     c                   parm                    tipo_seg
116900100811     c                   parm                    segmento
117000100811     c                   parm                    keyUNBCLI
117100100811     C                   parm                    keyTIPOMSG
117200100811     C                   parm                    keyVERSION
117300100811     C                   parm                    keyRELEASE
117400100811     C                   parm                    keyAGENCY
117500100811     C                   parm                    keyASSOCIA
117600100811     c                   parm                    Errore
117700100811     c                   parm                    EDS96LOC
117800100811     c                   parm                    Valori_inErr
117900100811     c                   parm                    Descr_Errore
118000100811      *
118100100811     C                   eval       DsGenerica = EDS96LOC
118200100811      *
118300100811      * --------
118400100811      *
118500100811     c                   when      tipo_seg = 'MEA'
118600100811      *
118700100811     c                   call      'TRTCT96MEA'
118800100811     c                   parm                    tipo_seg
118900100811     c                   parm                    segmento
119000100811     c                   parm                    keyUNBCLI
119100100811     C                   parm                    keyTIPOMSG
119200100811     C                   parm                    keyVERSION
119300100811     C                   parm                    keyRELEASE
119400100811     C                   parm                    keyAGENCY
119500100811     C                   parm                    keyASSOCIA
119600100811     c                   parm                    Errore
119700100811     c                   parm                    EDS96MEA
119800100811     c                   parm                    Valori_inErr
119900100811     c                   parm                    Descr_Errore
120000100811      *
120100100811     C                   eval       DsGenerica = EDS96MEA
120200100811      *
120300100811      * --------
120400100811      *
120500100811     c                   when      tipo_seg = 'MOA'
120600100811      *
120700100811     c                   call      'TRTCT96MOA'
120800100811     c                   parm                    tipo_seg
120900100811     c                   parm                    segmento
121000100811     c                   parm                    keyUNBCLI
121100100811     C                   parm                    keyTIPOMSG
121200100811     C                   parm                    keyVERSION
121300100811     C                   parm                    keyRELEASE
121400100811     C                   parm                    keyAGENCY
121500100811     C                   parm                    keyASSOCIA
121600100811     c                   parm                    Errore
121700100811     c                   parm                    EDS96MOA
121800100811     c                   parm                    Valori_inErr
121900100811     c                   parm                    Descr_Errore
122000100811      *
122100100811     C                   eval       DsGenerica = EDS96MOA
122200100811      *
122300100811      * --------
122400100811      *
122500100811     c                   when      tipo_seg = 'NAD'
122600100811      *
122700100811     c                   call      'TRTCT96NAD'
122800100811     c                   parm                    tipo_seg
122900100811     c                   parm                    segmento
123000100811     c                   parm                    keyUNBCLI
123100100811     C                   parm                    keyTIPOMSG
123200100811     C                   parm                    keyVERSION
123300100811     C                   parm                    keyRELEASE
123400100811     C                   parm                    keyAGENCY
123500100811     C                   parm                    keyASSOCIA
123600100811     c                   parm                    Errore
123700100811     c                   parm                    EDS96NAD
123800100811     c                   parm                    Valori_inErr
123900100811     c                   parm                    Descr_Errore
124000100811      *
124100100811     C                   eval       DsGenerica = EDS96NAD
124200100811      *
124300100811      * --------
124400100811      *
124500100811     c                   when      tipo_seg = 'PCI'
124600100811      *
124700100811     c                   call      'TRTCT96PCI'
124800100811     c                   parm                    tipo_seg
124900100811     c                   parm                    segmento
125000100811     c                   parm                    keyUNBCLI
125100100811     C                   parm                    keyTIPOMSG
125200100811     C                   parm                    keyVERSION
125300100811     C                   parm                    keyRELEASE
125400100811     C                   parm                    keyAGENCY
125500100811     C                   parm                    keyASSOCIA
125600100811     c                   parm                    Errore
125700100811     c                   parm                    EDS96PCI
125800100811     c                   parm                    Valori_inErr
125900100811     c                   parm                    Descr_Errore
126000100811      *
126100100811     C                   eval       DsGenerica = EDS96PCI
126200100811      *
126300100811      * --------
126400100811      *
126500100811     c                   when      tipo_seg = 'PIA'
126600100811      *
126700100811     c                   call      'TRTCT96PIA'
126800100811     c                   parm                    tipo_seg
126900100811     c                   parm                    segmento
127000100811     c                   parm                    keyUNBCLI
127100100811     C                   parm                    keyTIPOMSG
127200100811     C                   parm                    keyVERSION
127300100811     C                   parm                    keyRELEASE
127400100811     C                   parm                    keyAGENCY
127500100811     C                   parm                    keyASSOCIA
127600100811     c                   parm                    Errore
127700100811     c                   parm                    EDS96PIA
127800100811     c                   parm                    Valori_inErr
127900100811     c                   parm                    Descr_Errore
128000100811      *
128100100811     C                   eval       DsGenerica = EDS96PIA
128200100811      *
128300100811      * --------
128400100811      *
128500100811     c                   when      tipo_seg = 'RFF'
128600100811      *
128700100811     c                   call      'TRTCT96RFF'
128800100811     c                   parm                    tipo_seg
128900100811     c                   parm                    segmento
129000100811     c                   parm                    keyUNBCLI
129100100811     C                   parm                    keyTIPOMSG
129200100811     C                   parm                    keyVERSION
129300100811     C                   parm                    keyRELEASE
129400100811     C                   parm                    keyAGENCY
129500100811     C                   parm                    keyASSOCIA
129600100811     c                   parm                    Errore
129700100811     c                   parm                    EDS96RFF
129800100811     c                   parm                    Valori_inErr
129900100811     c                   parm                    Descr_Errore
130000100811      *
130100100811     C                   eval       DsGenerica = EDS96RFF
130200100811      *
130300100811      * --------
130400100811      *
130500100811     c                   when      tipo_seg = 'RNG'
130600100811      *
130700100811     c                   call      'TRTCT96RNG'
130800100811     c                   parm                    tipo_seg
130900100811     c                   parm                    segmento
131000100811     c                   parm                    keyUNBCLI
131100100811     C                   parm                    keyTIPOMSG
131200100811     C                   parm                    keyVERSION
131300100811     C                   parm                    keyRELEASE
131400100811     C                   parm                    keyAGENCY
131500100811     C                   parm                    keyASSOCIA
131600100811     c                   parm                    Errore
131700100811     c                   parm                    EDS96RNG
131800100811     c                   parm                    Valori_inErr
131900100811     c                   parm                    Descr_Errore
132000100811      *
132100100811     C                   eval       DsGenerica = EDS96RNG
132200100811      *
132300100811      * --------
132400100811      *
132500100811     c                   when      tipo_seg = 'SEL'
132600100811      *
132700100811     c                   call      'TRTCT96SEL'
132800100811     c                   parm                    tipo_seg
132900100811     c                   parm                    segmento
133000100811     c                   parm                    keyUNBCLI
133100100811     C                   parm                    keyTIPOMSG
133200100811     C                   parm                    keyVERSION
133300100811     C                   parm                    keyRELEASE
133400100811     C                   parm                    keyAGENCY
133500100811     C                   parm                    keyASSOCIA
133600100811     c                   parm                    Errore
133700100811     c                   parm                    EDS96SEL
133800100811     c                   parm                    Valori_inErr
133900100811     c                   parm                    Descr_Errore
134000100811      *
134100100811     C                   eval       DsGenerica = EDS96SEL
134200100811      *
134300100811      * --------
134400100811      *
134500100811     c                   when      tipo_seg = 'TCC'
134600100811      *
134700100811     c                   call      'TRTCT96TCC'
134800100811     c                   parm                    tipo_seg
134900100811     c                   parm                    segmento
135000100811     c                   parm                    keyUNBCLI
135100100811     C                   parm                    keyTIPOMSG
135200100811     C                   parm                    keyVERSION
135300100811     C                   parm                    keyRELEASE
135400100811     C                   parm                    keyAGENCY
135500100811     C                   parm                    keyASSOCIA
135600100811     c                   parm                    Errore
135700100811     c                   parm                    EDS96TCC
135800100811     c                   parm                    Valori_inErr
135900100811     c                   parm                    Descr_Errore
136000100811      *
136100100811     C                   eval       DsGenerica = EDS96TCC
136200100811      *
136300100811      * --------
136400100811      *
136500100811     c                   when      tipo_seg = 'TDT'
136600100811      *
136700100811     c                   call      'TRTCT96TDT'
136800100811     c                   parm                    tipo_seg
136900100811     c                   parm                    segmento
137000100811     c                   parm                    keyUNBCLI
137100100811     C                   parm                    keyTIPOMSG
137200100811     C                   parm                    keyVERSION
137300100811     C                   parm                    keyRELEASE
137400100811     C                   parm                    keyAGENCY
137500100811     C                   parm                    keyASSOCIA
137600100811     c                   parm                    Errore
137700100811     c                   parm                    EDS96TDT
137800100811     c                   parm                    Valori_inErr
137900100811     c                   parm                    Descr_Errore
138000100811      *
138100100811     C                   eval       DsGenerica = EDS96TDT
138200100811      *
138300100811      * --------
138400100811      *
138500100811     c                   when      tipo_seg = 'TMD'
138600100811      *
138700100811     c                   call      'TRTCT96TMD'
138800100811     c                   parm                    tipo_seg
138900100811     c                   parm                    segmento
139000100811     c                   parm                    keyUNBCLI
139100100811     C                   parm                    keyTIPOMSG
139200100811     C                   parm                    keyVERSION
139300100811     C                   parm                    keyRELEASE
139400100811     C                   parm                    keyAGENCY
139500100811     C                   parm                    keyASSOCIA
139600100811     c                   parm                    Errore
139700100811     c                   parm                    EDS96TMD
139800100811     c                   parm                    Valori_inErr
139900100811     c                   parm                    Descr_Errore
140000100811      *
140100100811     C                   eval       DsGenerica = EDS96TMD
140200100811      *
140300100811      * --------
140400100811      *
140500100811     c                   when      tipo_seg = 'TMP'
140600100811      *
140700100811     c                   call      'TRTCT96TMP'
140800100811     c                   parm                    tipo_seg
140900100811     c                   parm                    segmento
141000100811     c                   parm                    keyUNBCLI
141100100811     C                   parm                    keyTIPOMSG
141200100811     C                   parm                    keyVERSION
141300100811     C                   parm                    keyRELEASE
141400100811     C                   parm                    keyAGENCY
141500100811     C                   parm                    keyASSOCIA
141600100811     c                   parm                    Errore
141700100811     c                   parm                    EDS96TMP
141800100811     c                   parm                    Valori_inErr
141900100811     c                   parm                    Descr_Errore
142000100811      *
142100100811     C                   eval       DsGenerica = EDS96TMP
142200100811      *
142300100811      * --------
142400100811      *
142500100811     c                   when      tipo_seg = 'TOD'
142600100811      *
142700100811     c                   call      'TRTCT96TOD'
142800100811     c                   parm                    tipo_seg
142900100811     c                   parm                    segmento
143000100811     c                   parm                    keyUNBCLI
143100100811     C                   parm                    keyTIPOMSG
143200100811     C                   parm                    keyVERSION
143300100811     C                   parm                    keyRELEASE
143400100811     C                   parm                    keyAGENCY
143500100811     C                   parm                    keyASSOCIA
143600100811     c                   parm                    Errore
143700100811     c                   parm                    EDS96TOD
143800100811     c                   parm                    Valori_inErr
143900100811     c                   parm                    Descr_Errore
144000100811      *
144100100811     C                   eval       DsGenerica = EDS96TOD
144200100811      *
144300100811      * --------
144400100811      *
144500100811     c                   when      tipo_seg = 'TSR'
144600100811      *
144700100811     c                   call      'TRTCT96TSR'
144800100811     c                   parm                    tipo_seg
144900100811     c                   parm                    segmento
145000100811     c                   parm                    keyUNBCLI
145100100811     C                   parm                    keyTIPOMSG
145200100811     C                   parm                    keyVERSION
145300100811     C                   parm                    keyRELEASE
145400100811     C                   parm                    keyAGENCY
145500100811     C                   parm                    keyASSOCIA
145600100811     c                   parm                    Errore
145700100811     c                   parm                    EDS96TSR
145800100811     c                   parm                    Valori_inErr
145900100811     c                   parm                    Descr_Errore
146000100811      *
146100100811     C                   eval       DsGenerica = EDS96TSR
146200100811      *
146300100811      * --------
146400100811      *
146500100811     c                   when      tipo_seg = 'UNA'
146600100811      *
146700100811     c                   call      'TRTCT96UNA'
146800100811     c                   parm                    tipo_seg
146900100811     c                   parm                    segmento
147000100811     C                   parm                    keyUNBCLI
147100100811     C                   parm                    keyTIPOMSG
147200100811     C                   parm                    keyVERSION
147300100811     C                   parm                    keyRELEASE
147400100811     C                   parm                    keyAGENCY
147500100811     C                   parm                    keyASSOCIA
147600100811     c                   parm                    Errore
147700100811     c                   parm                    EDS96UNA
147800100811     c                   parm                    Valori_inErr
147900100811     c                   parm                    Descr_Errore
148000100811      *
148100100811     C                   eval       DsGenerica = EDS96UNA
148200100811      *
148300100811      * --------
148400100811      *
148500100811     c                   when      tipo_seg = 'UNB'
148600100811      *
148700100811     c                   call      'TRTCT96UNB'
148800100811     c                   parm                    tipo_seg
148900100811     c                   parm                    segmento
149000100811     C                   parm                    keyUNBCLI
149100100811     C                   parm                    keyTIPOMSG
149200100811     C                   parm                    keyVERSION
149300100811     C                   parm                    keyRELEASE
149400100811     C                   parm                    keyAGENCY
149500100811     C                   parm                    keyASSOCIA
149600100811     c                   parm                    Errore
149700100811     c                   parm                    EDS96UNB
149800100811     c                   parm                    Valori_inErr
149900100811     c                   parm                    Descr_Errore
150000100811      *
150100100811     C                   eval       DsGenerica = EDS96UNB
150200100811      *
150300100811      * --------
150400100811      *
150500100811     c                   when      tipo_seg = 'UNH'
150600100811      *
150700100811     c                   call      'TRTCT96UNH'
150800100811     c                   parm                    tipo_seg
150900100811     c                   parm                    segmento
151000100811     c                   parm                    keyUNBCLI
151100100811     C                   parm                    keyTIPOMSG
151200100811     C                   parm                    keyVERSION
151300100811     C                   parm                    keyRELEASE
151400100811     C                   parm                    keyAGENCY
151500100811     C                   parm                    keyASSOCIA
151600100811     c                   parm                    Errore
151700100811     c                   parm                    EDS96UNH
151800100811     c                   parm                    Valori_inErr
151900100811     c                   parm                    Descr_Errore
152000100811      *
152100100811     C                   eval       DsGenerica = EDS96UNH
152200100811      *
152300100811      * --------
152400100811      *
152500100811     c                   when      tipo_seg = 'UNT'
152600100811      *
152700100811     c                   call      'TRTCT96UNT'
152800100811     c                   parm                    tipo_seg
152900100811     c                   parm                    segmento
153000100811     c                   parm                    keyUNBCLI
153100100811     C                   parm                    keyTIPOMSG
153200100811     C                   parm                    keyVERSION
153300100811     C                   parm                    keyRELEASE
153400100811     C                   parm                    keyAGENCY
153500100811     C                   parm                    keyASSOCIA
153600100811     c                   parm                    Errore
153700100811     c                   parm                    EDS96UNT
153800100811     c                   parm                    Valori_inErr
153900100811     c                   parm                    Descr_Errore
154000100811      *
154100100811     C                   eval       DsGenerica = EDS96UNT
154200100811      *
154300100811      * --------
154400100811      *
154500100811     c                   when      tipo_seg = 'UNZ'
154600100811      *
154700100920     c                   call      'TRTCT96UNZ'
154800100811     c                   parm                    tipo_seg
154900100811     c                   parm                    segmento
155000101005     c                   parm                    keyUNBCLI
155100100811     C                   parm                    keyTIPOMSG
155200100811     C                   parm                    keyVERSION
155300100811     C                   parm                    keyRELEASE
155400100811     C                   parm                    keyAGENCY
155500100811     C                   parm                    keyASSOCIA
155600100811     c                   parm                    Errore
155700100811     c                   parm                    EDS96UNZ
155800100811     c                   parm                    Valori_inErr
155900100811     c                   parm                    Descr_Errore
156000100811      *
156100100811     C                   eval       DsGenerica = EDS96UNZ
156200100811      *
156300100811      * --------
156400100811      *
156500100811     c                   OTHER
156600100811      **--------
156700100811     c                   move      'N'           Trovato_Seg
156800100811     c                   move      'E'           Errore
156900100811     c                   eval       Descr_Errore = 'SGM non trovato'
157000100811     c                   eval       Valori_inErr = tipo_seg
157100100811      **--------
157200100811     c                   endSL
157300100811      *
157400100811     C                   ENDSR
157500100811      * ?------------------------------------------------------------------ */
157600100811      *?      X non bloccare in nessun caso il traduttore CLIENTI
157700100811      * ?------------------------------------------------------------------ */
157800100811     C     *pssr         BEGSR
157900100811     C
158000090205     C                   eval      esito = 'E'
158100060621     C                   ENDSR     '*CANCL'
158200060621     C
158300090204      *  ------------------------------------------------------------------ */
