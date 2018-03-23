000100070601     h DECEDIT('0,') DATEDIT(*dmy.)
000200020409     H**********************************************************************
000300070531     FAZORG01L  IF   E           K DISK
000400140612     FAZCMM01L  IF   E           K DISK    usropn
000500070601     FTABEL00F  IF   E           K DISK
000600070531     fwfFPV00f  uf a E           k disk
000700070601     ftise9aP   o    E             PRINTER OFLIND(*in66) usropn
000800040820     H**********************************************************************
000900040820     d KPJBA         E DS
001000070530     D tise9ads      e DS
001100110823     D wadfpv0f      e DS
001200070531     D*
001300041103     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001400041103     D CNCR80        E DS
001500110511     D CNCR42        E DS
001600040820     D*
001700070531     D*------------------
001800070531     D* ORGANIGRAMMA
001900070531     D*------------------
002000070531     D FIL             S              3  0 DIM(500)                             *FILIALI
002100070531     D DFIL            S             20    DIM(500)
002200070601     D  desFILIALE     s             20
002300070531     D ARE             S              3  0 DIM(500)                             *AREE
002400070531     D DARE            S             20    DIM(500)
002500070601     D  desAREA        s             20
002600070601     D DIS             S              1    DIM(500)                             *DISTRETTO
002700070601     D DDIS            S             20    DIM(500)
002800070601     D  desDISTRETTO   s             20
002900070531     D*------------------
003000070531     D* COMMERCIALI
003100070531     D*------------------
003200100202     D AGE             S              7  0 DIM(5000)                            *CODICI AGENTE
003300100202     D RGF             S              7  0 DIM(5000)                            *CODICI RAGGRUPPAMEN
003400100202     D DAGE            S             25    DIM(5000)                            *DECODIFICHE AGENTI
003500070531     D*------------------
003600070531     D* DS COMMERCIALI
003700070531     D*------------------
003800140612     D****** DS01          E DS
003900070719     D*------------------
004000070719     D* DS Cat.SEAT
004100070719     D*------------------
004200070719     D DS1L          E DS
004300070531     D*------------------
004400070531     D* DS REPERIMENTO DATI UTENTE
004500070531     D*-------------------
004600070531     D BS10DS        E DS                  EXTNAME(TIBS10DS)
004700070531     D BS69DS        E DS                  EXTNAME(TIBS69DS) INZ
004800070531     D ACODS         E DS                  EXTNAME(CNACO00F) INZ
004900070531     D INDDS         E DS                  EXTNAME(CNIND00F) INZ
005000070531     D CLPDS         E DS                  EXTNAME(CNCLP00F) INZ
005100070531     D CLSDS         E DS                  EXTNAME(FNCLS00F) INZ
005200070531     D*------------------
005300040820     D WLBDAT          DS
005400040820     D  G02DAT                 1      8  0
005500040820     D  G02INV                 9     16  0
005600040820     D  G02ERR                17     17
005700040820     D  G02TGI                18     22  0
005800040820      *
005900070530      *?  Stringa SQL per lettura File Principale...  ?
006000040820     D WrkSqlCmd       S           1024
006100070601      *
006200070601      *?  Definizione campi LIKE ...  ?
006300040820     D  data_Oggi      s                   like(G02INV) INZ(0)
006400070601     D  CLIENTE_salva  s                   like(wfdCLI)
006500070601     D  Ktb_KUT        s                   like(tblKUT)
006600070601     D  Ktb_COD        s                   like(tblCOD)
006700070601     D  Ktb_KEY        s                   like(tblKEY)
006800070601     D  KorFILIALE     s                   like(orgFIL)
006900070601     D  desCLIENTE     s                   like(acoRAG)
007000140612     D**desRAGGRage    s                   like(§01AGE)
007100140612     D  desRAGGRage    s                   like(cmmDES)
007200070601     D  wCLIENTE       s                   like(clpKSC)
007300070601     D  wUNIFICANTE    s                   like(clpKSC)
007400070601     D  wAGENTE        s                   like(clpAGE)
007500070601     D  wRAGGRage      s                   like(clpAGE)
007600070601     D  wAREA          s                   like(orgCAR)
007700070601     D  wDISTRETTO     s                   like(orgFL3)
007800070601     D  wDISTRCOM      s                   like(orgFL3)
007900070601     D  wFILIALE       s                   like(orgFIL)
008000070601     D  wBLOCCATO      s              1
008100070601     D  wVIA           s                   like(INDvia)
008200070601     D  wLOCALITA      s                   like(INDcit)
008300070601     D  wCAP           s                   like(INDcap)
008400070601     D  wCAPestero     s                   like(INDcae)
008500070601     D  wPROVINCIA     s                   like(INDprv)
008600070601     D  wSTATO         s                   like(INDsta)
008700070601     D  wQUALITA       s                   like(CLPCLV)
008800070719     D  wCAT_Cliente   s             25
008900070601      *
009000040826     D  almeno_uno     s              1
009100070530      *
009200040820     D digits          C                   CONST('0123456789')
009300040820      *****************************************************************
009400070530     c                   Exsr      Inizio
009500070530
009600070530      *?  Imposta la Stringa SLQ...  ?
009700011210     C/EXEC SQL
009800011210     C+ PREPARE S1 FROM :WrkSqlCmd
009900011210     C/END-EXEC
010000011210
010100070530      *?  Dichiara il Cursore SLQ...  ?
010200011210     C/EXEC SQL
010300011210     C+ DECLARE A1 CURSOR FOR S1
010400011210     C/END-EXEC
010500011210
010600070530      *?      Apre il Cursore SLQ...  ?
010700011210     C/EXEC SQL
010800011210     C+ OPEN A1
010900011210     C/END-EXEC
011000040820
011100040820     C                   DOU       SqlCod <> 0
011200040823     c*
011300070530      *?     Legge il Cursore SLQ...  ?
011400011205     C/EXEC SQL
011500110823     C+ FETCH NEXT FROM A1 INTO :WADFPV0F
011600011205     C/END-EXEC
011700011210     C                   SELECT
011800041007     c*
011900041007     c* a fine file Totali x rotture
012000041007     C                   WHEN      SqlCod = 100
012100041007     c                   leave
012200041007     **
012300041007     C                   WHEN      SqlCod < 0
012400041007     C                   seton                                        H1
012500041007     c                   goto      fine
012600040820     c*
012700041007     C                   OTHER
012800070531     c*  Dettaglio Riga
012900070531     c                   exsr      DETTAGLIO
013000040823     c* Totalizzo
013100040826     c                   move      'S'           almeno_uno
013200040823      *
013300011210     C                   ENDSL
013400040820
013500011205     C                   ENDDO
013600011205     C/EXEC SQL
013700011205     C+ CLOSE A1
013800011205     C/END-EXEC
013900041007     c     fine          tag
014000020326     C                   SETON                                        LR
014100040820     C**************************************************************************
014200040820     C* INIZIALIZZAZIONE
014300040820     C**************************************************************************
014400070530     C     Inizio        BEGSR
014500070531     C*
014600070531     C* CHIAVE LETTURA TABEL00F - COMPLETA
014700070601     C     KeyTAB        KLIST
014800070601     C                   KFLD                    Ktb_KUT                        *CODICE UTENTE
014900070601     C                   KFLD                    Ktb_COD                        *CODICE TABELLA
015000070601     C                   KFLD                    Ktb_KEY                        *CHIAVE TABELLA
015100070601      *
015200070601     C* CHIAVE LETTURA TABEL00F - PARZIALE
015300070601     C     KeyTA2        KLIST
015400070601     C                   KFLD                    Ktb_KUT                        *CODICE UTENTE
015500070601     C                   KFLD                    Ktb_COD                        *CODICE TABELLA
015600070601      **
015700070601     C     KeyFPV        KLIST
015800070601     c                   kfld                    fpvCLI
015900070601     c                   kfld                    fpvANN
016000070601     c                   kfld                    fpvMES
016100070601     c                   kfld                    fpvNTW
016200070601     c                   kfld                    fpvIOE
016300040820      **
016400070601      *?    Entrata programma    ?
016500040820     C     *ENTRY        PLIST
016600040820     C                   PARM                    KPJBA
016700070529     c                   movel     kpjbu         tise9ads
016800041103      *
016900041103     C                   Z-ADD     1             CODUT
017000041103     C                   CALL      'X§PARUT'
017100041103     C                   PARM                    UT§DSE
017200041103     C                   MOVEL     REC80         CNCR80
017300110511     C                   MOVEL     REC42         CNCR42
017400110511     C*****              MOVEL     RAGUT         RSUT             20
017500110511     C                   MOVEL     simRGS        RSUT             20
017600040820      *
017700040820      *  imposta UDATE di elaborazione
017800040820     C                   move      *date         G02DAT
017900040820     C                   MOVE      *ZEROS        G02INV
018000040820     C                   MOVE      *BLANKS       G02ERR
018100040820     C                   CALL      'XSRDA8'
018200040820     C                   PARM                    WLBDAT
018300040820     C                   Z-ADD     G02INV        data_oggi
018400040820     C                   move      data_oggi     day
018500040820     C                   TIME                    W0140            14 0
018600040820     C                   MOVEl     W0140         ore
018700040823      **
018800070531     c                   move      d9adel        p9adel
018900070531     c                   move      d9aora        p9aora
019000070530     c                   move      d9acu         p9acu
019100070530     c                   move      d9aQUA1       p9aQUA1
019200070530     c                   move      d9aQUA2       p9aQUA2
019300070530     c                   move      d9aQUA3       p9aQUA3
019400070530     c                   move      d9aQUA4       p9aQUA4
019500070530     c                   move      d9aQUA5       p9aQUA5
019600070530     c                   move      d9aIOE        p9aIOE
019700070530     c                   move      d9aNTW        p9aNTW
019800070601     c                   move      d9aNTWsin     p9aSINntw
019900070530     c                   move      d9aDIS        p9aDIS
020000070530     c                   move      d9aAREA       p9aAREA
020100070530     c                   move      d9aFIL        p9aFIL
020200070530     c                   move      d9aANNOC      p9aANNOC
020300070530     c                   move      d9aDMESC      p9aDMESC
020400070530     c                   move      d9aAMESC      p9aAMESC
020500070530     c                   move      d9aSINAC      p9aSINAC
020600070530     c                   move      d9aANNOP      p9aANNOP
020700070530     c                   move      d9aDMESP      p9aDMESP
020800070530     c                   move      d9aAMESP      p9aAMESP
020900070530     c                   move      d9aSINAP      p9aSINAP
021000040823      **
021100070531      ***   Stampa parametri di lancio  ?
021200070601     c                   open      tise9aP
021300070530     c                   write     PARAMETRI
021400070601     c                   close     tise9aP
021500070530      **
021600040825     c                   clear                   prima_volta       1
021700040826     c                   clear                   almeno_uno
021800070531     c*
021900070531      ***   carica tabelle di decodifica ?
022000070531     c                   exsr      caricaTABELLE
022100070531     c*
022200070531     C* Compone il primo giorno della data finale
022300070531     c                   if        d9aDMesC >0 or d9aAMesC >0
022400070531     C                   movel     d9aAnnoC      WDanm             6 0          *DATA finale
022500070531     C                   move      d9aAMesC      WDanm             6 0
022600070531     C                   movel     WDanm         WData_Finale      8 0
022700070531     C                   MOVE      28            WData_Finale
022800070531     c                   else
022900070531     C                   movel     d9aAnnoP      WDanm             6 0          *DATA finale
023000070531     C                   move      d9aAMesP      WDanm             6 0
023100070531     C                   movel     WDanm         WData_Finale      8 0
023200070531     C                   MOVE      28            WData_Finale
023300070531     c                   end
023400081114     C*
023500081114     C*------- GIORNO   di   F I N E   M E S E ---------------------*
023600081114     C*
023700081114     C*  Ultimo giorno del mese
023800081114     C                   MOVE      WData_Finale  MMGG_4            4 0
023900081114     C                   MOVEL     MMGG_4        Mm_2              2 0
024000081114     C                   MOVEl     WData_Finale  Anno_4            4 0
024100081114      * se è febbraio
024200081114     c                   If        Mm_2 = 2
024300081114      *
024400081114      *  potrebbe essere bisestile 29 gg.
024500081114     c     Anno_4        div       4             risulta           3 0
024600081114     c                   mvr                     resto             3 0
024700081114      *  Se bisestile
024800081114     c                   If        resto = 0
024900081114     C                   MOVE      '29'          WData_Finale
025000081114     c                   end
025100081114      *
025200081114     c                   else
025300081114      *
025400081114      *  se invece Aprile, Giugno, Settembre, Novembre con 30 gg.
025500081114     c                   If           Mm_2 = 4 or Mm_2 = 6 or
025600081114     c                                Mm_2 = 9 or Mm_2 = 11
025700081114     C                   MOVE      '30'          WData_Finale
025800081114     c                   else
025900081114     C                   MOVE      '31'          WData_Finale
026000081114     c                   end
026100081114      *
026200081114     c                   endIf
026300081114     C*
026400081114     C*----------- F I N E    M E S E ---------------------------*
026500081114     C*
026600070531      **
026700070531      ***-------------------------------------------------------*
026800070531      *?  MI COMPONGO L'ISTRUZIONE SQL                         ?*
026900110823      *?  che legge il file WaDFPV0F delle statistiche DELTA   ?*
027000070531      ***-------------------------------------------------------*
027100040820     C                   EVAL      WrkSqlCmd
027200040820     C                             =
027300110823     C                             'SELECT * from WADFPV0F where '
027400070530      ***----------------------
027500070530      ***   PERIODO    ?
027600070530      ***----------------------
027700070530      **  + Periodo anno in corso
027800070530     c                   if        d9aDMesC >0 or d9aAMesC >0
027900070530
028000070530     C                   EVAL      WrkSqlCmd
028100070530     C                             =
028200070530     C                             %TRIMR(WrkSqlCmd)
028300070530     c                             + ' ((wfdANN =' + %trim(%char(d9aAnnoC))
028400070530     c                             + ' and wfdMES between '
028500070530     c                             + %trim(%char(d9aDMesC)) + ' and '
028600070530     c                             + %trim(%char(d9aAMesC)) + ')'
028700070530     c                   end
028800070530      *
028900070530      * Se non c'è l'anno in corso
029000070530     c                   if        d9aDMesC =0 and d9aAMesC =0
029100070530     C                   EVAL      WrkSqlCmd
029200070530     C                             =
029300070530     C                             %TRIMR(WrkSqlCmd)
029400070530     c                             + ' ( '
029500070530     c                   end
029600070530      *
029700070530      * Mette in OR i periodi dei 2 anni
029800070530     c                   if        (d9aDMesC >0 or d9aAMesC >0) and
029900070530     c                             (d9aDMesP >0 or d9aAMesP >0)
030000070530     C                   EVAL      WrkSqlCmd
030100070530     C                             =
030200070530     C                             %TRIMR(WrkSqlCmd)
030300070530     c                             + ' or '
030400070530     c                   end
030500070530      *
030600070530      **  + Periodo anno precedente
030700070530     c                   if        d9aDMesP >0 or d9aAMesP >0
030800070530     C                   EVAL      WrkSqlCmd
030900070530     C                             =
031000070530     C                             %TRIMR(WrkSqlCmd)
031100070530     c                             + ' (wfdANN =' + %trim(%char(d9aAnnoP))
031200070530     c                             + ' and wfdMES between '
031300070530     c                             + %trim(%char(d9aDMesP)) + ' and '
031400070530     c                             + %trim(%char(d9aAMesP)) + ')'
031500070530     c                   end
031600070530      *  Chiude il periodo
031700070530     C                   EVAL      WrkSqlCmd
031800070530     C                             =
031900070530     C                             %TRIMR(WrkSqlCmd)
032000070530     c                             + ') '
032100070608      ***----------------------
032200070608      ***  ITALIA/ESTERO   ?
032300070608      ***----------------------
032400070608     c                   if        d9aIoE <> ' '
032500070608     C                   EVAL      WrkSqlCmd
032600070608     C                             =
032700070608     C                             %TRIMR(WrkSqlCmd)
032800070608     c                             + ' and wfdioe = ' + '''' + %trim(d9aIoE) +
032900070608     c                              ''''
033000070608     c                   end
033100070608      ***----------------------
033200070608      ***   NETWORK  ?
033300070608      ***----------------------
033400070608     c                   if        d9aNTW <> ' '
033500070608     C                   EVAL      WrkSqlCmd
033600070608     C                             =
033700070608     C                             %TRIMR(WrkSqlCmd)
033800070608     c                             + ' and wfdntw = ' + '''' + %trim(d9aNTW) +
033900070608     c                              ''''
034000070608     c                   end
034100070608      *
034200070608      ***----------------------
034300070608      ***  solo x NON UNIFICANTE
034400070608      ***----------------------
034500070608     c                   if        d9aCU = 'C'
034600070608      *
034700070530      ***----------------------
034800070530      ***   DISTRETTO      ?
034900070530      ***----------------------
035000070530     c                   if        d9aDIS <> ' '
035100070530     C                   EVAL      WrkSqlCmd
035200070530     C                             =
035300070530     C                             %TRIMR(WrkSqlCmd)
035400070530     c                             + ' and wfddiv = ' + '''' + %trim(d9aDIS) +
035500070530     c                              ''''
035600070530     c                   end
035700070530      ***----------------------
035800070530      ***    AREA          ?
035900070530      ***----------------------
036000070530     c                   if        d9aAREA <> ' '
036100070530     C                   EVAL      WrkSqlCmd
036200070530     C                             =
036300070530     C                             %TRIMR(WrkSqlCmd)
036400070530     c                             + ' and wfdare = ' + %trim(d9aArea)
036500070530     c                   end
036600070530      ***----------------------
036700070530      ***    FILIALE       ?
036800070530      ***----------------------
036900070530     c                   if        d9aFIL  <> ' '
037000070530     C                   EVAL      WrkSqlCmd
037100070530     C                             =
037200070530     C                             %TRIMR(WrkSqlCmd)
037300070530     c                             + ' and wfdfil = ' + %trim(d9aFIL)
037400070530     c                   end
037500070608      *
037600070608     c                   end
037700070608      ***----------------------
037800070608      ***  solo x NON UNIFICANTE
037900070608      ***----------------------
038000070608      *
038100070530      ***----------------------
038200070530      ***   Ordinamento di lettura   ?
038300070530      ***----------------------
038400040820     C                   EVAL      WrkSqlCmd
038500040820     C                             =
038600040820     C                             %TRIMR(WrkSqlCmd)
038700040820     c                             +
038800070531     c                             ' ORDER BY wfdCLI, wfdANN, wfdMES, wfdNTW'
038900040820      **
039000040820     C                   ENDSR
039100020326     C**************************************************************************
039200070531      *?  Scrittura del Workfile ...  ?   WFFPV00F x Cliente o Unificante
039300070530     C**************************************************************************
039400070531     C     DETTAGLIO     begsr
039500070531      *
039600070601      *  Se si tratta di Anno in Corso  in Sintesi      ?
039700070601      *  si deve impostare il mese genericamente a (0)  ?
039800070531     c                   if        wfdANN = d9aAnnoC and d9aSINaC = 'S'
039900070601     C                   Z-ADD     99            wfdMES
040000070531     C                   End
040100070531      *
040200070601      *  Se si tratta di Anno Precedente  in Sintesi    ?
040300070601      *  si deve impostare il mese genericamente a (0)  ?
040400070531     c                   if        wfdANN = d9aAnnoP and d9aSINaP = 'S'
040500070601     C                   Z-ADD     99            wfdMES
040600070531     C                   End
040700070531      *
040800070601      *  Se richiesta stampa sintetica x il Network
040900070601     c                   if        d9aNTWsin = 'S'
041000070601     C                   MOVE      *ALL'9'       wfdNTW
041100070601     C                   End
041200070601      *
041300070601      *  ?    Decodifica il Cliente a ogni cambio codice Cliente    ?
041400070601      *  esce con il campo ?  wCLIENTE  ? per impostare la chiave x il file
041500070601     c                   if        wfdCLI <> CLIENTE_salva
041600070601     c                   eval      CLIENTE_salva  = wfdCLI
041700070608      *
041800070601     c                   exsr      Decod_Cliente
041900070608      *
042000070608     c                   move      'S'           Record_OK         1
042100070608     c                   exsr      Filtro_Cliente
042200070601     c                   end
042300070531      *
042400070608      * Solo se OK
042500070608     c                   If        Record_OK = 'S'
042600070608      *
042700070601      *    Imposta la chiave e aggancia il    ?  WFFPV00F  ?
042800070601     c                   move      wCLIENTE      fpvCLI
042900070601     c                   move      wfdANN        fpvANN
043000070601     c                   move      wfdMES        fpvMES
043100070601     c                   move      wfdNTW        fpvNTW
043200070601     c                   move      wfdIOE        fpvIOE
043300070601      *
043400070601      ***   CHAIN al FILE da ESTRARRE di WORK   ?
043500070601     C     KeyFPV        chain     wfFPV00F
043600070601      *
043700070601     c                   if        %found(wfFPV00F)
043800070601      ***   Aggiorna   ?
043900070601     c                   exsr      FPV_upd_campi
044000070601     c                   update    wfFPV000
044100070601      *
044200070601     c                   else
044300070601      ***   Scrive     ?
044400070601     c                   exsr      FPV_wri_campi
044500070601     c                   exsr      FPV_upd_campi
044600070601     c                   write     wfFPV000
044700070601      *
044800070601     c                   end
044900070608      *
045000070608     c                   endIf
045100070601      *
045200070601     C                   EndSR
045300070601     C*------------------------------------------------------------------------*
045400070601      *?     Campi da Scrivere   sul record del file da estrarre   ?
045500070601     C*------------------------------------------------------------------------*
045600070601     C     FPV_wri_campi BEGSR
045700070601      *
045800070601     c                   clear                   wfFPV000
045900070601      *  Inizializza il campo
046000070601     c                   eval      fpvBLO = 'B'
046100070601      ***   chiave univoca  ?
046200070601     c                   move      wCLIENTE      fpvCLI
046300070601     c                   move      wfdANN        fpvANN
046400070601     c                   move      wfdMES        fpvMES
046500070601     c                   move      wfdNTW        fpvNTW
046600070601     c                   move      wfdIOE        fpvIOE
046700070601      *
046800070601      *   Decodificati in base al codice cliente (o unificante)
046900070601     c                   eval      fpvTCL = d9aCU
047000070601     c                   eval      fpvCLU = wUNIFICANTE
047100070601     c                   eval      fpvCLD = desCLIENTE
047200070719     c                   eval      fpvITC = wCAT_Cliente
047300070601     c                   eval      fpvCLV = wQUALITA
047400070601     c                   eval      fpvIND = wVIA
047500070601     c                   movel     wCAP          fpvCAP
047600070601     c                   eval      fpvLOC = wLOCALITA
047700070601     c                   eval      fpvPRV = wPROVINCIA
047800070601     c                   eval      fpvNAZ = wSTATO
047900070601     c                   eval      fpvDIV = wDISTRETTO
048000070601     c                   eval      fpvDID = desDISTRETTO
048100070601     c                   eval      fpvARE = wAREA
048200070601     c                   eval      fpvARD = desAREA
048300070601     c                   eval      fpvFIL = wFILIALE
048400070601     c                   eval      fpvFID = desFILIALE
048500070601     c                   eval      fpvDIC = wDISTRCOM
048600070601     c                   eval      fpvRGF = wRAGGRage
048700070601     c                   eval      fpvRGD = desRAGGRage
048800070601     c                   eval      fpvAGE = wAGENTE
048900070601      *
049000070530     C                   EndSR
049100070531     C*------------------------------------------------------------------------*
049200070601      *?    Campi da Aggiornare sul record del file da estrarre   ?
049300070531     C*------------------------------------------------------------------------*
049400070601     C     FPV_upd_campi BEGSR
049500070601      *
049600070601      *  Bloccato (basta che uno NON sia bloccato x NON bloccare gli altri)
049700070601     c                   if        wBLOCCATO = *blank
049800070601     c                   eval      fpvBLO = *blank
049900070601     c                   end
050000070601      *
050100070601      *    in base alla fascia di Peso incasella SPEDIZIONI PESI VOLUMI e RICAVI
050200070601     c                   if                 wfdFPE = '01'
050300070601     c                   add       WFDNSP        FPVNSP1
050400070601     c                   add       WFDPKG        FPVPES1
050500070601     c                   add       WFDVOL        FPVVOL1
050600070601     c                   add       WFDRRN        FPVRRN1
050700070601     c                   add       WFDRPN        FPVRPN1
050800070601     c                   end
050900070601      *
051000070601     c                   if                 wfdFPE = '02'
051100070601     c                   add       WFDNSP        FPVNSP2
051200070601     c                   add       WFDPKG        FPVPES2
051300070601     c                   add       WFDVOL        FPVVOL2
051400070601     c                   add       WFDRRN        FPVRRN2
051500070601     c                   add       WFDRPN        FPVRPN2
051600070601     c                   end
051700070601      *
051800070601     c                   if                 wfdFPE = '03'
051900070601     c                   add       WFDNSP        FPVNSP3
052000070601     c                   add       WFDPKG        FPVPES3
052100070601     c                   add       WFDVOL        FPVVOL3
052200070601     c                   add       WFDRRN        FPVRRN3
052300070601     c                   add       WFDRPN        FPVRPN3
052400070601     c                   end
052500070601      *
052600070601     c                   if                 wfdFPE = '04'
052700070601     c                   add       WFDNSP        FPVNSP4
052800070601     c                   add       WFDPKG        FPVPES4
052900070601     c                   add       WFDVOL        FPVVOL4
053000070601     c                   add       WFDRRN        FPVRRN4
053100070601     c                   add       WFDRPN        FPVRPN4
053200070601     c                   end
053300070601      *
053400070601     c                   if                 wfdFPE = '05'
053500070601     c                   add       WFDNSP        FPVNSP5
053600070601     c                   add       WFDPKG        FPVPES5
053700070601     c                   add       WFDVOL        FPVVOL5
053800070601     c                   add       WFDRRN        FPVRRN5
053900070601     c                   add       WFDRPN        FPVRPN5
054000070601     c                   end
054100070601      *
054200070601     c                   if                 wfdFPE = '06'
054300070601     c                   add       WFDNSP        FPVNSP6
054400070601     c                   add       WFDPKG        FPVPES6
054500070601     c                   add       WFDVOL        FPVVOL6
054600070601     c                   add       WFDRRN        FPVRRN6
054700070601     c                   add       WFDRPN        FPVRPN6
054800070601     c                   end
054900070601      *
055000070601     c                   if                 wfdFPE = '07'
055100070601     c                   add       WFDNSP        FPVNSP7
055200070601     c                   add       WFDPKG        FPVPES7
055300070601     c                   add       WFDVOL        FPVVOL7
055400070601     c                   add       WFDRRN        FPVRRN7
055500070601     c                   add       WFDRPN        FPVRPN7
055600070601     c                   end
055700070601      *
055800070601     c                   if                 wfdFPE = '08'
055900070601     c                   add       WFDNSP        FPVNSP8
056000070601     c                   add       WFDPKG        FPVPES8
056100070601     c                   add       WFDVOL        FPVVOL8
056200070601     c                   add       WFDRRN        FPVRRN8
056300070601     c                   add       WFDRPN        FPVRPN8
056400070601     c                   end
056500070601      *
056600070601     c                   if                 wfdFPE = '09'
056700070601     c                   add       WFDNSP        FPVNSP9
056800070601     c                   add       WFDPKG        FPVPES9
056900070601     c                   add       WFDVOL        FPVVOL9
057000070601     c                   add       WFDRRN        FPVRRN9
057100070601     c                   add       WFDRPN        FPVRPN9
057200070601     c                   end
057300070601      *
057400070601     c                   if                 wfdFPE = '10'
057500070601     c                   add       WFDNSP        FPVNSP0
057600070601     c                   add       WFDPKG        FPVPES0
057700070601     c                   add       WFDVOL        FPVVOL0
057800070601     c                   add       WFDRRN        FPVRRN0
057900070601     c                   add       WFDRPN        FPVRPN0
058000070601     c                   end
058100070601      *
058200070601     C                   EndSR
058300070601     C*------------------------------------------------------------------------*
058400070608      *?   Controlla se il Cliente è entro le selzioni del Video          ?
058500070601     C*------------------------------------------------------------------------*
058600070608     C     Filtro_ClienteBEGSR
058700070601     C*
058800070608     C* Filiale Selezionata
058900070608     c                   if        D9AFIL <> *blank
059000070608     c                   move      wFiliale      aFiliale          3
059100070608     c                   if        D9AFIL <>     aFiliale
059200070608     c                   eval      record_OK = 'N'
059300070608     C                   End
059400070608     C                   End
059500070608     C*
059600070608     C* Area    Selezionata
059700070608     c                   if        D9AAREA <> *blank
059800070608     c                   move      wArea         aArea             3
059900070608     c                   if        D9AAREA <>    aArea
060000070608     c                   eval      record_OK = 'N'
060100070608     C                   End
060200070608     C                   End
060300070608     C*
060400070608     C* Distretto Selezionato
060500070608     c                   if        D9ADIS  <> *blank
060600070608     c                   move      wDistretto    aDistretto        1
060700070608     c                   if        D9ADIS  <>    aDistretto
060800070608     c                   eval      record_OK = 'N'
060900070608     C                   End
061000070608     C                   End
061100070608      *
061200070608     C* Qualità Selezionata
061300070608     c                   if        D9AQUA1 <> ' ' or
061400070608     c                             D9AQUA2 <> ' ' or
061500070608     c                             D9AQUA3 <> ' ' or
061600070608     c                             D9AQUA4 <> ' ' or
061700070608     c                             D9AQUA5 <> ' '
061800070608     c                   if        D9AQUA1 =  wQualita or
061900070608     c                             D9AQUA2 =  wQualita or
062000070608     c                             D9AQUA3 =  wQualita or
062100070608     c                             D9AQUA4 =  wQualita or
062200070608     c                             D9AQUA5 =  wQualita
062300070608     c                   else
062400070608     c                   eval      record_OK = 'N'
062500070608     C                   End
062600070608     C                   End
062700070608      *
062800070608     C                   EndSR
062900070608     C*------------------------------------------------------------------------*
063000070608      *?     OPERAZIONI PER decodificare il NUOVO CLIENTE appena letto    ?
063100070608     C*------------------------------------------------------------------------*
063200070608     C     Decod_Cliente BEGSR
063300070608     C*
063400070601     C*  A livello  Cliente verifica se bloccato
063500070601     C                   CLEAR                   BS69DS
063600070601     C                   CLEAR                   CLPDS
063700070601     C                   MOVEL     KNSIF         I69SIF                         *S.INFORMATIVO
063800070604     C                   MOVEL     CLIENTE_salva I69KAC                         *CLIENTE X CNACO
063900070601     C                   CALL      'TIBS69R'
064000070601     C                   PARM                    BS69DS
064100070601     C                   PARM                    ACODS
064200070601     C                   PARM                    INDDS
064300070601     C                   PARM                    CLPDS
064400070601     C                   PARM                    CLSDS
064500070601      *
064600070601     C                   MOVEL     *BLANKS       wBLOCCATO                      *SE NON TROVATO
064700070601IF  1C     O69ERR        ifNE      '1'                                          *NO ERRORE
064800130315IF  1C     acoabl        andne     *blank                                       *cliente bloccato
064900130315IF  1C     acoabl        andne     '*'                                          *cliente bloccato
065000070601     C                   MOVEL     'B'           wBLOCCATO                      *BLOCCO CLIENTE
065100070601     c                   end
065200070531     C*
065300070601      ***   IMPOSTA IL CLIENTE DA SCRIVERE SUL WFILE -CLIENTE O UNIFICANTE-   ?
065400070531     C                   CLEAR                   BS10DS
065500070601     C                   Z-ADD     WData_Finale  d10DRF                         *DATA FINALE LANCIO
065600070601     C                   MOVEL     'ST'          d10TLE                         *TIPO LEGAME
065700070601     C                   MOVEL     'P'           d10PAF                         *CERCA PADRE
065800070601     C                   MOVE      wfdCLI        d10COD
065900070531     C                   CALL      'TIBS10R'
066000070531     C                   PARM                    BS10DS
066100070601      *  Non Unificante
066200070601     C                   Z-ADD     wfdCLI        wCLIENTE
066300070601     C                   Z-ADD     wfdCLI        wUNIFICANTE
066400070601IF  2C     d10ERR        IFEQ      *BLANKS                                      *HA UN PADRE
066500070601     C     d10COP        ANDNE     *ZEROS
066600070601     C                   Z-ADD     d10COP        wUNIFICANTE                    *UNIFICANTE
066700070601      * UNificante
066800070601    1C     d9acu         ifeq      'U'                                          *PER UNIFICANTE
066900070601     C                   Z-ADD     wUNIFICANTE   wCLIENTE
067000070601    1C                   end
067100070531E   2C                   ENDIF
067200070531     C*
067300070601      ***   REPERISCO FILIALE/AREA/DIVISIONE DEL CLIENTE  ?
067400070601     C                   movel     wCLIENTE      wFILIALE                       *FIL+XXXX
067500070601     C                   Z-add     1             F
067600070601     C                   SEToff                                           99
067700070601     C     wFILIALE      LOOKUP    FIL(F)                                 99
067800070531IF  1C     *IN99         IFEQ      *ON
067900070601     C                   movel     DFIL(F)       desFILIALE
068000070601     C                   Z-ADD     ARE(F)        wAREA
068100070601     C                   movel     DARE(F)       desAREA
068200070601     C                   MOVEL     DIS(F)        wDISTRETTO
068300070601     C                   movel     DDIS(F)       desDISTRETTO
068400070531X   1C                   ELSE
068500070601     C                   Z-ADD     *ZEROS        wAREA
068600070601     C                   MOVEL     *BLANKS       desAREA
068700070601     C                   MOVEL     *BLANKS       wDISTRETTO
068800070601     C                   MOVEL     *BLANKS       desDISTRETTO
068900070601     C                   movel     *BLANKS       desFILIALE
069000070531E   1C                   ENDIF
069100070531     C*
069200070601      ***   REPERISCE IL COMMERCIALE e DATI ANGRAFICI   ?
069300070601     C*       SE NON ESISTE IMPOSTA IL CODICE AGENTE = FIL + 0000
069400070531     C                   CLEAR                   BS69DS
069500070531     C                   CLEAR                   CLPDS
069600070531     C                   MOVEL     KNSIF         I69SIF                         *S.INFORMATIVO
069700070601     C                   MOVEL     wCLIENTE      I69KAC                         *CLIENTE X CNACO
069800070601     C                   MOVEL     wCLIENTE      I69KIN                         *CLIENTE X CNIND
069900070601     C                   MOVEL     wCLIENTE      I69KCP                         *CLIENTE X CNCLP
070000070531     C                   CALL      'TIBS69R'
070100070531     C                   PARM                    BS69DS
070200070531     C                   PARM                    ACODS
070300070531     C                   PARM                    INDDS
070400070531     C                   PARM                    CLPDS
070500070531     C                   PARM                    CLSDS
070600070604IF  1C     O69ERR        ifNE      '1'                                          *NO ERRORE
070700130315IF  1C     acoabl        andne     *blank                                       *cliente bloccato
070800130315IF  1C     acoabl        andne     '*'                                          *cliente bloccato
070900070604     c     wbloccato     andne     *blank
071000070604     C                   MOVEL     'B'           wBLOCCATO                      *BLOCCO CLIENTE
071100070604     c                   end
071200070601     C                   MOVEL     *ALL'*'       desCLIENTE                     *SE NON TROVATO
071300070531IF  1C     O69ERR        IFNE      '1'                                          *NO ERRORE
071400070601      *  DATI ANAGRAFICI DEL CLIENTE   ?
071500070601     C                   MOVEL     acoRAG        desCLIENTE                     *RAGIONE SOCIALE
071600070601     C                   MOVEL     INDvia        wVIA                           *Indirizzo
071700070601     C                   MOVEL     INDcap        wCAP                           *CAP
071800070601     C                   MOVEL     INDcae        wCAPestero                     *CAP Europeo
071900070601     C                   MOVEL     INDprv        wPROVINCIA                     *Provincia
072000070601     C                   MOVEL     INDcit        wLOCALITA                      *Città
072100070601     C                   MOVEL     INDsta        wSTATO                         *Nazione
072200070719     c                   move      acoITC        wCAT_Cliente                   *Categoria SEAT
072300070719      *
072400070719      *  Categoria Cliente (SEAT)  ?
072500070719     C                   MOVEL     *BLANKS       wCAT_Cliente                   *SE NON TROVATO
072600070719     C                   Z-ADD     1             Ktb_KUT
072700070719     C                   MOVEL     '1L'          Ktb_COD
072800070719     C                   MOVEL     *BLANKS       Ktb_KEY
072900070719     C                   MOVE      acoITC        NUM5              5 0
073000070719     C                   MOVEL     NUM5          Ktb_KEY
073100070719     C     KeyTAB        CHAIN     TABEL00F
073200070719IF  1C                   IF        %Found(Tabel00F)
073300070719     C                   MOVEL     tblUNI        DS1L
073400070719     C                   MOVEL     §1LDES        wCAT_Cliente                   *CATEGORIA SEAT
073500070719E   1C                   ENDIF
073600070719      *
073700070531E   1C                   ENDIF
073800070601      *
073900070531IF  1C     O69ERR        IFEQ      '1'                                          *ERRORE
074000070601     C     clpAGE        OREQ      *ZEROS                                       *CODICE AGENTE VUOTO
074100070601     C                   MOVEL     wCLIENTE      NUM3              3 0          *CODICE FILIALE +
074200070601     C                   Z-ADD     *ZEROS        wAGENTE                        *AGENTE A ZERO  =
074300070601     C                   MOVEL     NUM3          wAGENTE                        *FIL+0000
074400070531X   1C                   ELSE                                                   *NO ERRORE
074500070601     C                   Z-ADD     clpAGE        wAGENTE                        *CODICE AGENTE
074600070531E   1C                   ENDIF
074700070531     C*
074800070531     C* REPERISCE IL CODICE QUALITA'
074900070601     C                   MOVEL     'C'           wQUALITA
075000070601IF  1C     clpCLV        IFNE      *BLANKS
075100070601     C                   MOVEL     CLPCLV        wQUALITA
075200070531E   1C                   ENDIF
075300070531     C*
075400070531     C* REPERISCO IL RAGGRUPPAMENTO FILIALE COMMERCIALE E RELATIVA DESCRIZIONE
075500070601     C                   Z-ADD     1             C                 5 0
075600070531     C                   SETOFF                                           99
075700070601     C     wAGENTE       LOOKUP    AGE(C)                                 99
075800070531IF  1C     *IN99         IFEQ      *OFF
075900070601     C                   Z-ADD     wAGENTE       wRAGGRage                      RAGGRUPP = COD AGENT
076000070601     C                   MOVEL     *ALL'*'       desRAGGRage                    DESCRIZIONE RAGGRUP
076100070531X   1C                   ELSE
076200070531IF  2C     RGF(C)        IFEQ      *ZEROS
076300070601     C                   Z-ADD     wAGENTE       wRAGGRage
076400070601     C                   MOVEL     *ALL'*'       desRAGGRage                    DESCRIZIONE RAGGRUP
076500070531X   2C                   ELSE
076600070601     C                   Z-ADD     RGF(C)        wRAGGRage
076700070531     C*
076800070531     C* DESCRIZIONE CODICE AGENTE = DESCRIZIONE CODICE RAGGRUPPAMENTO
076900070531     C                   Z-ADD     1             C
077000070531     C                   SETOFF                                           99
077100070601     C     wRAGGRage     LOOKUP    AGE(C)                                 99
077200070531IF  3C     *IN99         IFEQ      *ON
077300070601     C                   MOVEL     DAGE(C)       desRAGGRage                    DESCRIZIONE RAGGR.TO
077400070531X   3C                   ELSE
077500070601     C                   MOVEL     *ALL'*'       desRAGGRage                    DESCRIZIONE RAGGR.TO
077600070531E   3C                   ENDIF
077700070531E   2C                   ENDIF
077800070531E   1C                   ENDIF
077900070531     C*
078000070531     C* REPERISCO LA DIVISIONE DEL RAGGRUPPAMENTO COMMERCIALE
078100070531     C                   Z-ADD     1             F
078200070531     C                   SETOFF                                           99
078300070601     C                   MOVEL     wRAGGRage     NUM3                           *FIL+XXXX
078400070531     C     NUM3          LOOKUP    FIL(F)                                 99
078500070531IF  1C     *IN99         IFEQ      *ON
078600070601     C                   MOVEL     DIS(F)        wDISTRCOM
078700070531X   1C                   ELSE
078800070601     C                   MOVEL     *BLANKS       wDISTRCOM
078900070531E   1C                   ENDIF
079000070601     C*
079100070531     C                   ENDSR
079200070531     C*------------------------------------------------------------------------*
079300070531     C* carTAB - CARICA I DATI IN TABELLE
079400070531     C*------------------------------------------------------------------------*
079500070531     C     caricaTABELLE BEGSR
079600070531     C*
079700070531     C* ORGANIGRAMMA
079800070531     C                   Z-ADD     *ZEROS        F
079900070601     C                   Z-ADD     *ZEROS        KorFILIALE
080000070601     C     KorFILIALE    SetLL     AZORG01L
080100070601     C                   Read      AZORG01L                               99
080200070531DO  1C     *IN99         DOWEQ     '0'
080300070601IF  2C     orgFVA        IFEQ      *BLANKS                                      *NO ANNULLATI
080400070601IF  3C     orgFAG        IFNE      'V'                                          *FILIALE/AGENZIA
080500070601     C                   ADD       1             F                 5 0
080600070601     C                   Z-ADD     orgFIL        FIL(F)
080700070601     C                   MOVEL     orgDES        DFIL(F)
080800070601     C                   Z-ADD     1             Ktb_KUT
080900070601     C                   MOVEL     '05'          Ktb_COD
081000070601     C                   MOVEL     *BLANKS       Ktb_KEY
081100070601     C                   MOVEL     orgCAR        Ktb_KEY
081200070601     C     KeyTAB        CHAIN     TABEL00F                           99
081300070531IF  5C     *IN99         IFEQ      *OFF
081400070601     C                   Z-ADD     orgCAR        ARE(F)
081500070531     C                   MOVEL     TBLUNI        DARE(F)
081600070531E   5C                   ENDIF
081700070601     C                   Z-ADD     1             Ktb_KUT
081800070601     C                   MOVEL     '17'          Ktb_COD
081900070601     C                   MOVEL     *BLANKS       Ktb_KEY
082000070601     C                   MOVEL     orgFL3        Ktb_KEY
082100070601     C     KeyTAB        CHAIN     TABEL00F                           99
082200070531IF  5C     *IN99         IFEQ      *OFF
082300070601     C                   MOVEL     orgFL3        DIS(F)
082400070601     C                   MOVEL     TblUNI        DDIS(F)
082500070531E   5C                   ENDIF
082600070531E   3C                   ENDIF
082700070531E   2C                   ENDIF
082800070531     C                   READ      AZORG01L                               99
082900070531E   1C                   ENDDO
083000070531     C*--------------------
083100070531     C* COMMERCIALI
083200070531     C*--------------------
083300140612     C************       Z-ADD     *ZEROS        C
083400140612     C************       Z-ADD     1             Ktb_KUT
083500140612     C************       MOVEL     '01'          Ktb_COD
083600140612     C***  KeyTA2        SetLL     TABEL00F
083700140612     C***  KeyTA2        ReadE     TABEL00F                               99
083800140612DO  1C***  *IN99         DOWEQ     *OFF
083900140612IF  2C***  TblFLG        IFEQ      *BLANKS
084000140612     C************       ADD       1             C
084100140612     C************       MOVEL     TblUNI        DS01
084200140612     C************       MOVEL     TblKEY        AGE(C)
084300140612     C************       MOVEL     §01RGF        RGF(C)
084400140612     C************       MOVEL     §01AGE        DAGE(C)
084500140612E   2C************       end
084600140612     C***  KeyTA2        ReadE     TABEL00F                               99
084700140612E   1C************       ENDDO
084800140612      *-----
084900140612     C                   Z-ADD     *ZEROS        C
085000140612     c                   open      azCMM01l
085100140612     C     *loval        SETLL     azCMM01l
085200140612     C                   READ      azCMM01l
085300140612DO  1C                   DOW       not %Eof(azCMM01l)
085400140612IF  2C                   IF        cmmATB = *blank
085500140612     C                   ADD       1             C
085600140612     C                   z-add     cmmCOD        AGE(C)
085700140612     C                   z-add     cmmUNI        RGF(C)
085800140612     C                   MOVEL     cmmDES        DAGE(C)
085900140612E   2C                   ENDIF
086000140612     C                   READ      azCMM01l
086100140612E   1C                   ENDDO
086200140612     c                   close     azCMM01l
086300070531     C*
086400070531     C                   ENDSR
086500070531     C*------------------------------------------------------------------------*
