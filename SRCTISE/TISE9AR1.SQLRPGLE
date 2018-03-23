000100141219     /*END
000200070601     h DECEDIT('0,') DATEDIT(*dmy.)
000300020409     H**********************************************************************
000400070531     FAZORG01L  IF   E           K DISK
000500140612     FAZCMM01L  IF   E           K DISK    usropn
000600070601     FTABEL00F  IF   E           K DISK
000700070531     fwfFPV00f  uf a E           k disk
000800070601     ftise9aP   o    E             PRINTER OFLIND(*in66) usropn
000900040820     H**********************************************************************
001000040820     d KPJBA         E DS
001100070530     D tise9ads      e DS
001200110823     D wadfpv0f      e DS
001300070531     D*
001400041103     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001500041103     D CNCR80        E DS
001600110511     D CNCR42        E DS
001700040820     D*
001800070531     D*------------------
001900070531     D* ORGANIGRAMMA
002000070531     D*------------------
002100070531     D FIL             S              3  0 DIM(500)                             *FILIALI
002200070531     D DFIL            S             20    DIM(500)
002300070601     D  desFILIALE     s             20
002400070531     D ARE             S              3  0 DIM(500)                             *AREE
002500070531     D DARE            S             20    DIM(500)
002600070601     D  desAREA        s             20
002700070601     D DIS             S              1    DIM(500)                             *DISTRETTO
002800070601     D DDIS            S             20    DIM(500)
002900070601     D  desDISTRETTO   s             20
003000070531     D*------------------
003100070531     D* COMMERCIALI
003200070531     D*------------------
003300141219     d c_Max_Cmm       c                   const(9000)
003400141219     D AGE             S              7  0 DIM(c_Max_Cmm)                       *CODICI AGENTE
003500141219     D RGF             S              7  0 DIM(c_Max_Cmm)                       *CODICI RAGGRUPPAMEN
003600141219     D DAGE            S             25    DIM(c_Max_Cmm)                       *DECODIFICHE AGENTI
003700070531     D*------------------
003800070531     D* DS COMMERCIALI
003900070531     D*------------------
004000140612     D****** DS01          E DS
004100070719     D*------------------
004200070719     D* DS Cat.SEAT
004300070719     D*------------------
004400070719     D DS1L          E DS
004500070531     D*------------------
004600070531     D* DS REPERIMENTO DATI UTENTE
004700070531     D*-------------------
004800070531     D BS10DS        E DS                  EXTNAME(TIBS10DS)
004900070531     D BS69DS        E DS                  EXTNAME(TIBS69DS) INZ
005000070531     D ACODS         E DS                  EXTNAME(CNACO00F) INZ
005100070531     D INDDS         E DS                  EXTNAME(CNIND00F) INZ
005200070531     D CLPDS         E DS                  EXTNAME(CNCLP00F) INZ
005300070531     D CLSDS         E DS                  EXTNAME(FNCLS00F) INZ
005400070531     D*------------------
005500040820     D WLBDAT          DS
005600040820     D  G02DAT                 1      8  0
005700040820     D  G02INV                 9     16  0
005800040820     D  G02ERR                17     17
005900040820     D  G02TGI                18     22  0
006000040820      *
006100070530      *?  Stringa SQL per lettura File Principale...  ?
006200040820     D WrkSqlCmd       S           1024
006300070601      *
006400070601      *?  Definizione campi LIKE ...  ?
006500040820     D  data_Oggi      s                   like(G02INV) INZ(0)
006600070601     D  CLIENTE_salva  s                   like(wfdCLI)
006700070601     D  Ktb_KUT        s                   like(tblKUT)
006800070601     D  Ktb_COD        s                   like(tblCOD)
006900070601     D  Ktb_KEY        s                   like(tblKEY)
007000070601     D  KorFILIALE     s                   like(orgFIL)
007100070601     D  desCLIENTE     s                   like(acoRAG)
007200140612     D**desRAGGRage    s                   like(§01AGE)
007300140612     D  desRAGGRage    s                   like(cmmDES)
007400070601     D  wCLIENTE       s                   like(clpKSC)
007500070601     D  wUNIFICANTE    s                   like(clpKSC)
007600070601     D  wAGENTE        s                   like(clpAGE)
007700070601     D  wRAGGRage      s                   like(clpAGE)
007800070601     D  wAREA          s                   like(orgCAR)
007900070601     D  wDISTRETTO     s                   like(orgFL3)
008000070601     D  wDISTRCOM      s                   like(orgFL3)
008100070601     D  wFILIALE       s                   like(orgFIL)
008200070601     D  wBLOCCATO      s              1
008300070601     D  wVIA           s                   like(INDvia)
008400070601     D  wLOCALITA      s                   like(INDcit)
008500070601     D  wCAP           s                   like(INDcap)
008600070601     D  wCAPestero     s                   like(INDcae)
008700070601     D  wPROVINCIA     s                   like(INDprv)
008800070601     D  wSTATO         s                   like(INDsta)
008900070601     D  wQUALITA       s                   like(CLPCLV)
009000070719     D  wCAT_Cliente   s             25
009100070601      *
009200040826     D  almeno_uno     s              1
009300070530      *
009400040820     D digits          C                   CONST('0123456789')
009500040820      *****************************************************************
009600070530     c                   Exsr      Inizio
009700070530
009800070530      *?  Imposta la Stringa SLQ...  ?
009900011210     C/EXEC SQL
010000011210     C+ PREPARE S1 FROM :WrkSqlCmd
010100011210     C/END-EXEC
010200011210
010300070530      *?  Dichiara il Cursore SLQ...  ?
010400011210     C/EXEC SQL
010500011210     C+ DECLARE A1 CURSOR FOR S1
010600011210     C/END-EXEC
010700011210
010800070530      *?      Apre il Cursore SLQ...  ?
010900011210     C/EXEC SQL
011000011210     C+ OPEN A1
011100011210     C/END-EXEC
011200040820
011300040820     C                   DOU       SqlCod <> 0
011400040823     c*
011500070530      *?     Legge il Cursore SLQ...  ?
011600011205     C/EXEC SQL
011700110823     C+ FETCH NEXT FROM A1 INTO :WADFPV0F
011800011205     C/END-EXEC
011900011210     C                   SELECT
012000041007     c*
012100041007     c* a fine file Totali x rotture
012200041007     C                   WHEN      SqlCod = 100
012300041007     c                   leave
012400041007     **
012500041007     C                   WHEN      SqlCod < 0
012600041007     C                   seton                                        H1
012700041007     c                   goto      fine
012800040820     c*
012900041007     C                   OTHER
013000070531     c*  Dettaglio Riga
013100070531     c                   exsr      DETTAGLIO
013200040823     c* Totalizzo
013300040826     c                   move      'S'           almeno_uno
013400040823      *
013500011210     C                   ENDSL
013600040820
013700011205     C                   ENDDO
013800011205     C/EXEC SQL
013900011205     C+ CLOSE A1
014000011205     C/END-EXEC
014100041007     c     fine          tag
014200020326     C                   SETON                                        LR
014300040820     C**************************************************************************
014400040820     C* INIZIALIZZAZIONE
014500040820     C**************************************************************************
014600070530     C     Inizio        BEGSR
014700070531     C*
014800070531     C* CHIAVE LETTURA TABEL00F - COMPLETA
014900070601     C     KeyTAB        KLIST
015000070601     C                   KFLD                    Ktb_KUT                        *CODICE UTENTE
015100070601     C                   KFLD                    Ktb_COD                        *CODICE TABELLA
015200070601     C                   KFLD                    Ktb_KEY                        *CHIAVE TABELLA
015300070601      *
015400070601     C* CHIAVE LETTURA TABEL00F - PARZIALE
015500070601     C     KeyTA2        KLIST
015600070601     C                   KFLD                    Ktb_KUT                        *CODICE UTENTE
015700070601     C                   KFLD                    Ktb_COD                        *CODICE TABELLA
015800070601      **
015900070601     C     KeyFPV        KLIST
016000070601     c                   kfld                    fpvCLI
016100070601     c                   kfld                    fpvANN
016200070601     c                   kfld                    fpvMES
016300070601     c                   kfld                    fpvNTW
016400070601     c                   kfld                    fpvIOE
016500040820      **
016600070601      *?    Entrata programma    ?
016700040820     C     *ENTRY        PLIST
016800040820     C                   PARM                    KPJBA
016900070529     c                   movel     kpjbu         tise9ads
017000041103      *
017100041103     C                   Z-ADD     1             CODUT
017200041103     C                   CALL      'X§PARUT'
017300041103     C                   PARM                    UT§DSE
017400041103     C                   MOVEL     REC80         CNCR80
017500110511     C                   MOVEL     REC42         CNCR42
017600110511     C*****              MOVEL     RAGUT         RSUT             20
017700110511     C                   MOVEL     simRGS        RSUT             20
017800040820      *
017900040820      *  imposta UDATE di elaborazione
018000040820     C                   move      *date         G02DAT
018100040820     C                   MOVE      *ZEROS        G02INV
018200040820     C                   MOVE      *BLANKS       G02ERR
018300040820     C                   CALL      'XSRDA8'
018400040820     C                   PARM                    WLBDAT
018500040820     C                   Z-ADD     G02INV        data_oggi
018600040820     C                   move      data_oggi     day
018700040820     C                   TIME                    W0140            14 0
018800040820     C                   MOVEl     W0140         ore
018900040823      **
019000070531     c                   move      d9adel        p9adel
019100070531     c                   move      d9aora        p9aora
019200070530     c                   move      d9acu         p9acu
019300070530     c                   move      d9aQUA1       p9aQUA1
019400070530     c                   move      d9aQUA2       p9aQUA2
019500070530     c                   move      d9aQUA3       p9aQUA3
019600070530     c                   move      d9aQUA4       p9aQUA4
019700070530     c                   move      d9aQUA5       p9aQUA5
019800070530     c                   move      d9aIOE        p9aIOE
019900070530     c                   move      d9aNTW        p9aNTW
020000070601     c                   move      d9aNTWsin     p9aSINntw
020100070530     c                   move      d9aDIS        p9aDIS
020200070530     c                   move      d9aAREA       p9aAREA
020300070530     c                   move      d9aFIL        p9aFIL
020400070530     c                   move      d9aANNOC      p9aANNOC
020500070530     c                   move      d9aDMESC      p9aDMESC
020600070530     c                   move      d9aAMESC      p9aAMESC
020700070530     c                   move      d9aSINAC      p9aSINAC
020800070530     c                   move      d9aANNOP      p9aANNOP
020900070530     c                   move      d9aDMESP      p9aDMESP
021000070530     c                   move      d9aAMESP      p9aAMESP
021100070530     c                   move      d9aSINAP      p9aSINAP
021200040823      **
021300070531      ***   Stampa parametri di lancio  ?
021400070601     c                   open      tise9aP
021500070530     c                   write     PARAMETRI
021600070601     c                   close     tise9aP
021700070530      **
021800040825     c                   clear                   prima_volta       1
021900040826     c                   clear                   almeno_uno
022000070531     c*
022100070531      ***   carica tabelle di decodifica ?
022200070531     c                   exsr      caricaTABELLE
022300070531     c*
022400070531     C* Compone il primo giorno della data finale
022500070531     c                   if        d9aDMesC >0 or d9aAMesC >0
022600070531     C                   movel     d9aAnnoC      WDanm             6 0          *DATA finale
022700070531     C                   move      d9aAMesC      WDanm             6 0
022800070531     C                   movel     WDanm         WData_Finale      8 0
022900070531     C                   MOVE      28            WData_Finale
023000070531     c                   else
023100070531     C                   movel     d9aAnnoP      WDanm             6 0          *DATA finale
023200070531     C                   move      d9aAMesP      WDanm             6 0
023300070531     C                   movel     WDanm         WData_Finale      8 0
023400070531     C                   MOVE      28            WData_Finale
023500070531     c                   end
023600081114     C*
023700081114     C*------- GIORNO   di   F I N E   M E S E ---------------------*
023800081114     C*
023900081114     C*  Ultimo giorno del mese
024000081114     C                   MOVE      WData_Finale  MMGG_4            4 0
024100081114     C                   MOVEL     MMGG_4        Mm_2              2 0
024200081114     C                   MOVEl     WData_Finale  Anno_4            4 0
024300081114      * se è febbraio
024400081114     c                   If        Mm_2 = 2
024500081114      *
024600081114      *  potrebbe essere bisestile 29 gg.
024700081114     c     Anno_4        div       4             risulta           3 0
024800081114     c                   mvr                     resto             3 0
024900081114      *  Se bisestile
025000081114     c                   If        resto = 0
025100081114     C                   MOVE      '29'          WData_Finale
025200081114     c                   end
025300081114      *
025400081114     c                   else
025500081114      *
025600081114      *  se invece Aprile, Giugno, Settembre, Novembre con 30 gg.
025700081114     c                   If           Mm_2 = 4 or Mm_2 = 6 or
025800081114     c                                Mm_2 = 9 or Mm_2 = 11
025900081114     C                   MOVE      '30'          WData_Finale
026000081114     c                   else
026100081114     C                   MOVE      '31'          WData_Finale
026200081114     c                   end
026300081114      *
026400081114     c                   endIf
026500081114     C*
026600081114     C*----------- F I N E    M E S E ---------------------------*
026700081114     C*
026800070531      **
026900070531      ***-------------------------------------------------------*
027000070531      *?  MI COMPONGO L'ISTRUZIONE SQL                         ?*
027100110823      *?  che legge il file WaDFPV0F delle statistiche DELTA   ?*
027200070531      ***-------------------------------------------------------*
027300040820     C                   EVAL      WrkSqlCmd
027400040820     C                             =
027500110823     C                             'SELECT * from WADFPV0F where '
027600070530      ***----------------------
027700070530      ***   PERIODO    ?
027800070530      ***----------------------
027900070530      **  + Periodo anno in corso
028000070530     c                   if        d9aDMesC >0 or d9aAMesC >0
028100070530
028200070530     C                   EVAL      WrkSqlCmd
028300070530     C                             =
028400070530     C                             %TRIMR(WrkSqlCmd)
028500070530     c                             + ' ((wfdANN =' + %trim(%char(d9aAnnoC))
028600070530     c                             + ' and wfdMES between '
028700070530     c                             + %trim(%char(d9aDMesC)) + ' and '
028800070530     c                             + %trim(%char(d9aAMesC)) + ')'
028900070530     c                   end
029000070530      *
029100070530      * Se non c'è l'anno in corso
029200070530     c                   if        d9aDMesC =0 and d9aAMesC =0
029300070530     C                   EVAL      WrkSqlCmd
029400070530     C                             =
029500070530     C                             %TRIMR(WrkSqlCmd)
029600070530     c                             + ' ( '
029700070530     c                   end
029800070530      *
029900070530      * Mette in OR i periodi dei 2 anni
030000070530     c                   if        (d9aDMesC >0 or d9aAMesC >0) and
030100070530     c                             (d9aDMesP >0 or d9aAMesP >0)
030200070530     C                   EVAL      WrkSqlCmd
030300070530     C                             =
030400070530     C                             %TRIMR(WrkSqlCmd)
030500070530     c                             + ' or '
030600070530     c                   end
030700070530      *
030800070530      **  + Periodo anno precedente
030900070530     c                   if        d9aDMesP >0 or d9aAMesP >0
031000070530     C                   EVAL      WrkSqlCmd
031100070530     C                             =
031200070530     C                             %TRIMR(WrkSqlCmd)
031300070530     c                             + ' (wfdANN =' + %trim(%char(d9aAnnoP))
031400070530     c                             + ' and wfdMES between '
031500070530     c                             + %trim(%char(d9aDMesP)) + ' and '
031600070530     c                             + %trim(%char(d9aAMesP)) + ')'
031700070530     c                   end
031800070530      *  Chiude il periodo
031900070530     C                   EVAL      WrkSqlCmd
032000070530     C                             =
032100070530     C                             %TRIMR(WrkSqlCmd)
032200070530     c                             + ') '
032300070608      ***----------------------
032400070608      ***  ITALIA/ESTERO   ?
032500070608      ***----------------------
032600070608     c                   if        d9aIoE <> ' '
032700070608     C                   EVAL      WrkSqlCmd
032800070608     C                             =
032900070608     C                             %TRIMR(WrkSqlCmd)
033000070608     c                             + ' and wfdioe = ' + '''' + %trim(d9aIoE) +
033100070608     c                              ''''
033200070608     c                   end
033300070608      ***----------------------
033400070608      ***   NETWORK  ?
033500070608      ***----------------------
033600070608     c                   if        d9aNTW <> ' '
033700070608     C                   EVAL      WrkSqlCmd
033800070608     C                             =
033900070608     C                             %TRIMR(WrkSqlCmd)
034000070608     c                             + ' and wfdntw = ' + '''' + %trim(d9aNTW) +
034100070608     c                              ''''
034200070608     c                   end
034300070608      *
034400070608      ***----------------------
034500070608      ***  solo x NON UNIFICANTE
034600070608      ***----------------------
034700070608     c                   if        d9aCU = 'C'
034800070608      *
034900070530      ***----------------------
035000070530      ***   DISTRETTO      ?
035100070530      ***----------------------
035200070530     c                   if        d9aDIS <> ' '
035300070530     C                   EVAL      WrkSqlCmd
035400070530     C                             =
035500070530     C                             %TRIMR(WrkSqlCmd)
035600070530     c                             + ' and wfddiv = ' + '''' + %trim(d9aDIS) +
035700070530     c                              ''''
035800070530     c                   end
035900070530      ***----------------------
036000070530      ***    AREA          ?
036100070530      ***----------------------
036200070530     c                   if        d9aAREA <> ' '
036300070530     C                   EVAL      WrkSqlCmd
036400070530     C                             =
036500070530     C                             %TRIMR(WrkSqlCmd)
036600070530     c                             + ' and wfdare = ' + %trim(d9aArea)
036700070530     c                   end
036800070530      ***----------------------
036900070530      ***    FILIALE       ?
037000070530      ***----------------------
037100070530     c                   if        d9aFIL  <> ' '
037200070530     C                   EVAL      WrkSqlCmd
037300070530     C                             =
037400070530     C                             %TRIMR(WrkSqlCmd)
037500070530     c                             + ' and wfdfil = ' + %trim(d9aFIL)
037600070530     c                   end
037700070608      *
037800070608     c                   end
037900070608      ***----------------------
038000070608      ***  solo x NON UNIFICANTE
038100070608      ***----------------------
038200070608      *
038300070530      ***----------------------
038400070530      ***   Ordinamento di lettura   ?
038500070530      ***----------------------
038600040820     C                   EVAL      WrkSqlCmd
038700040820     C                             =
038800040820     C                             %TRIMR(WrkSqlCmd)
038900040820     c                             +
039000070531     c                             ' ORDER BY wfdCLI, wfdANN, wfdMES, wfdNTW'
039100040820      **
039200040820     C                   ENDSR
039300020326     C**************************************************************************
039400070531      *?  Scrittura del Workfile ...  ?   WFFPV00F x Cliente o Unificante
039500070530     C**************************************************************************
039600070531     C     DETTAGLIO     begsr
039700070531      *
039800070601      *  Se si tratta di Anno in Corso  in Sintesi      ?
039900070601      *  si deve impostare il mese genericamente a (0)  ?
040000070531     c                   if        wfdANN = d9aAnnoC and d9aSINaC = 'S'
040100070601     C                   Z-ADD     99            wfdMES
040200070531     C                   End
040300070531      *
040400070601      *  Se si tratta di Anno Precedente  in Sintesi    ?
040500070601      *  si deve impostare il mese genericamente a (0)  ?
040600070531     c                   if        wfdANN = d9aAnnoP and d9aSINaP = 'S'
040700070601     C                   Z-ADD     99            wfdMES
040800070531     C                   End
040900070531      *
041000070601      *  Se richiesta stampa sintetica x il Network
041100070601     c                   if        d9aNTWsin = 'S'
041200070601     C                   MOVE      *ALL'9'       wfdNTW
041300070601     C                   End
041400070601      *
041500070601      *  ?    Decodifica il Cliente a ogni cambio codice Cliente    ?
041600070601      *  esce con il campo ?  wCLIENTE  ? per impostare la chiave x il file
041700070601     c                   if        wfdCLI <> CLIENTE_salva
041800070601     c                   eval      CLIENTE_salva  = wfdCLI
041900070608      *
042000070601     c                   exsr      Decod_Cliente
042100070608      *
042200070608     c                   move      'S'           Record_OK         1
042300070608     c                   exsr      Filtro_Cliente
042400070601     c                   end
042500070531      *
042600070608      * Solo se OK
042700070608     c                   If        Record_OK = 'S'
042800070608      *
042900070601      *    Imposta la chiave e aggancia il    ?  WFFPV00F  ?
043000070601     c                   move      wCLIENTE      fpvCLI
043100070601     c                   move      wfdANN        fpvANN
043200070601     c                   move      wfdMES        fpvMES
043300070601     c                   move      wfdNTW        fpvNTW
043400070601     c                   move      wfdIOE        fpvIOE
043500070601      *
043600070601      ***   CHAIN al FILE da ESTRARRE di WORK   ?
043700070601     C     KeyFPV        chain     wfFPV00F
043800070601      *
043900070601     c                   if        %found(wfFPV00F)
044000070601      ***   Aggiorna   ?
044100070601     c                   exsr      FPV_upd_campi
044200070601     c                   update    wfFPV000
044300070601      *
044400070601     c                   else
044500070601      ***   Scrive     ?
044600070601     c                   exsr      FPV_wri_campi
044700070601     c                   exsr      FPV_upd_campi
044800070601     c                   write     wfFPV000
044900070601      *
045000070601     c                   end
045100070608      *
045200070608     c                   endIf
045300070601      *
045400070601     C                   EndSR
045500070601     C*------------------------------------------------------------------------*
045600070601      *?     Campi da Scrivere   sul record del file da estrarre   ?
045700070601     C*------------------------------------------------------------------------*
045800070601     C     FPV_wri_campi BEGSR
045900070601      *
046000070601     c                   clear                   wfFPV000
046100070601      *  Inizializza il campo
046200070601     c                   eval      fpvBLO = 'B'
046300070601      ***   chiave univoca  ?
046400070601     c                   move      wCLIENTE      fpvCLI
046500070601     c                   move      wfdANN        fpvANN
046600070601     c                   move      wfdMES        fpvMES
046700070601     c                   move      wfdNTW        fpvNTW
046800070601     c                   move      wfdIOE        fpvIOE
046900070601      *
047000070601      *   Decodificati in base al codice cliente (o unificante)
047100070601     c                   eval      fpvTCL = d9aCU
047200070601     c                   eval      fpvCLU = wUNIFICANTE
047300070601     c                   eval      fpvCLD = desCLIENTE
047400070719     c                   eval      fpvITC = wCAT_Cliente
047500070601     c                   eval      fpvCLV = wQUALITA
047600070601     c                   eval      fpvIND = wVIA
047700070601     c                   movel     wCAP          fpvCAP
047800070601     c                   eval      fpvLOC = wLOCALITA
047900070601     c                   eval      fpvPRV = wPROVINCIA
048000070601     c                   eval      fpvNAZ = wSTATO
048100070601     c                   eval      fpvDIV = wDISTRETTO
048200070601     c                   eval      fpvDID = desDISTRETTO
048300070601     c                   eval      fpvARE = wAREA
048400070601     c                   eval      fpvARD = desAREA
048500070601     c                   eval      fpvFIL = wFILIALE
048600070601     c                   eval      fpvFID = desFILIALE
048700070601     c                   eval      fpvDIC = wDISTRCOM
048800070601     c                   eval      fpvRGF = wRAGGRage
048900070601     c                   eval      fpvRGD = desRAGGRage
049000070601     c                   eval      fpvAGE = wAGENTE
049100070601      *
049200070530     C                   EndSR
049300070531     C*------------------------------------------------------------------------*
049400070601      *?    Campi da Aggiornare sul record del file da estrarre   ?
049500070531     C*------------------------------------------------------------------------*
049600070601     C     FPV_upd_campi BEGSR
049700070601      *
049800070601      *  Bloccato (basta che uno NON sia bloccato x NON bloccare gli altri)
049900070601     c                   if        wBLOCCATO = *blank
050000070601     c                   eval      fpvBLO = *blank
050100070601     c                   end
050200070601      *
050300070601      *    in base alla fascia di Peso incasella SPEDIZIONI PESI VOLUMI e RICAVI
050400070601     c                   if                 wfdFPE = '01'
050500070601     c                   add       WFDNSP        FPVNSP1
050600070601     c                   add       WFDPKG        FPVPES1
050700070601     c                   add       WFDVOL        FPVVOL1
050800070601     c                   add       WFDRRN        FPVRRN1
050900070601     c                   add       WFDRPN        FPVRPN1
051000070601     c                   end
051100070601      *
051200070601     c                   if                 wfdFPE = '02'
051300070601     c                   add       WFDNSP        FPVNSP2
051400070601     c                   add       WFDPKG        FPVPES2
051500070601     c                   add       WFDVOL        FPVVOL2
051600070601     c                   add       WFDRRN        FPVRRN2
051700070601     c                   add       WFDRPN        FPVRPN2
051800070601     c                   end
051900070601      *
052000070601     c                   if                 wfdFPE = '03'
052100070601     c                   add       WFDNSP        FPVNSP3
052200070601     c                   add       WFDPKG        FPVPES3
052300070601     c                   add       WFDVOL        FPVVOL3
052400070601     c                   add       WFDRRN        FPVRRN3
052500070601     c                   add       WFDRPN        FPVRPN3
052600070601     c                   end
052700070601      *
052800070601     c                   if                 wfdFPE = '04'
052900070601     c                   add       WFDNSP        FPVNSP4
053000070601     c                   add       WFDPKG        FPVPES4
053100070601     c                   add       WFDVOL        FPVVOL4
053200070601     c                   add       WFDRRN        FPVRRN4
053300070601     c                   add       WFDRPN        FPVRPN4
053400070601     c                   end
053500070601      *
053600070601     c                   if                 wfdFPE = '05'
053700070601     c                   add       WFDNSP        FPVNSP5
053800070601     c                   add       WFDPKG        FPVPES5
053900070601     c                   add       WFDVOL        FPVVOL5
054000070601     c                   add       WFDRRN        FPVRRN5
054100070601     c                   add       WFDRPN        FPVRPN5
054200070601     c                   end
054300070601      *
054400070601     c                   if                 wfdFPE = '06'
054500070601     c                   add       WFDNSP        FPVNSP6
054600070601     c                   add       WFDPKG        FPVPES6
054700070601     c                   add       WFDVOL        FPVVOL6
054800070601     c                   add       WFDRRN        FPVRRN6
054900070601     c                   add       WFDRPN        FPVRPN6
055000070601     c                   end
055100070601      *
055200070601     c                   if                 wfdFPE = '07'
055300070601     c                   add       WFDNSP        FPVNSP7
055400070601     c                   add       WFDPKG        FPVPES7
055500070601     c                   add       WFDVOL        FPVVOL7
055600070601     c                   add       WFDRRN        FPVRRN7
055700070601     c                   add       WFDRPN        FPVRPN7
055800070601     c                   end
055900070601      *
056000070601     c                   if                 wfdFPE = '08'
056100070601     c                   add       WFDNSP        FPVNSP8
056200070601     c                   add       WFDPKG        FPVPES8
056300070601     c                   add       WFDVOL        FPVVOL8
056400070601     c                   add       WFDRRN        FPVRRN8
056500070601     c                   add       WFDRPN        FPVRPN8
056600070601     c                   end
056700070601      *
056800070601     c                   if                 wfdFPE = '09'
056900070601     c                   add       WFDNSP        FPVNSP9
057000070601     c                   add       WFDPKG        FPVPES9
057100070601     c                   add       WFDVOL        FPVVOL9
057200070601     c                   add       WFDRRN        FPVRRN9
057300070601     c                   add       WFDRPN        FPVRPN9
057400070601     c                   end
057500070601      *
057600070601     c                   if                 wfdFPE = '10'
057700070601     c                   add       WFDNSP        FPVNSP0
057800070601     c                   add       WFDPKG        FPVPES0
057900070601     c                   add       WFDVOL        FPVVOL0
058000070601     c                   add       WFDRRN        FPVRRN0
058100070601     c                   add       WFDRPN        FPVRPN0
058200070601     c                   end
058300070601      *
058400070601     C                   EndSR
058500070601     C*------------------------------------------------------------------------*
058600070608      *?   Controlla se il Cliente è entro le selzioni del Video          ?
058700070601     C*------------------------------------------------------------------------*
058800070608     C     Filtro_ClienteBEGSR
058900070601     C*
059000070608     C* Filiale Selezionata
059100070608     c                   if        D9AFIL <> *blank
059200070608     c                   move      wFiliale      aFiliale          3
059300070608     c                   if        D9AFIL <>     aFiliale
059400070608     c                   eval      record_OK = 'N'
059500070608     C                   End
059600070608     C                   End
059700070608     C*
059800070608     C* Area    Selezionata
059900070608     c                   if        D9AAREA <> *blank
060000070608     c                   move      wArea         aArea             3
060100070608     c                   if        D9AAREA <>    aArea
060200070608     c                   eval      record_OK = 'N'
060300070608     C                   End
060400070608     C                   End
060500070608     C*
060600070608     C* Distretto Selezionato
060700070608     c                   if        D9ADIS  <> *blank
060800070608     c                   move      wDistretto    aDistretto        1
060900070608     c                   if        D9ADIS  <>    aDistretto
061000070608     c                   eval      record_OK = 'N'
061100070608     C                   End
061200070608     C                   End
061300070608      *
061400070608     C* Qualità Selezionata
061500070608     c                   if        D9AQUA1 <> ' ' or
061600070608     c                             D9AQUA2 <> ' ' or
061700070608     c                             D9AQUA3 <> ' ' or
061800070608     c                             D9AQUA4 <> ' ' or
061900070608     c                             D9AQUA5 <> ' '
062000070608     c                   if        D9AQUA1 =  wQualita or
062100070608     c                             D9AQUA2 =  wQualita or
062200070608     c                             D9AQUA3 =  wQualita or
062300070608     c                             D9AQUA4 =  wQualita or
062400070608     c                             D9AQUA5 =  wQualita
062500070608     c                   else
062600070608     c                   eval      record_OK = 'N'
062700070608     C                   End
062800070608     C                   End
062900070608      *
063000070608     C                   EndSR
063100070608     C*------------------------------------------------------------------------*
063200070608      *?     OPERAZIONI PER decodificare il NUOVO CLIENTE appena letto    ?
063300070608     C*------------------------------------------------------------------------*
063400070608     C     Decod_Cliente BEGSR
063500070608     C*
063600070601     C*  A livello  Cliente verifica se bloccato
063700070601     C                   CLEAR                   BS69DS
063800070601     C                   CLEAR                   CLPDS
063900070601     C                   MOVEL     KNSIF         I69SIF                         *S.INFORMATIVO
064000070604     C                   MOVEL     CLIENTE_salva I69KAC                         *CLIENTE X CNACO
064100070601     C                   CALL      'TIBS69R'
064200070601     C                   PARM                    BS69DS
064300070601     C                   PARM                    ACODS
064400070601     C                   PARM                    INDDS
064500070601     C                   PARM                    CLPDS
064600070601     C                   PARM                    CLSDS
064700070601      *
064800070601     C                   MOVEL     *BLANKS       wBLOCCATO                      *SE NON TROVATO
064900070601IF  1C     O69ERR        ifNE      '1'                                          *NO ERRORE
065000130315IF  1C     acoabl        andne     *blank                                       *cliente bloccato
065100130315IF  1C     acoabl        andne     '*'                                          *cliente bloccato
065200070601     C                   MOVEL     'B'           wBLOCCATO                      *BLOCCO CLIENTE
065300070601     c                   end
065400070531     C*
065500070601      ***   IMPOSTA IL CLIENTE DA SCRIVERE SUL WFILE -CLIENTE O UNIFICANTE-   ?
065600070531     C                   CLEAR                   BS10DS
065700070601     C                   Z-ADD     WData_Finale  d10DRF                         *DATA FINALE LANCIO
065800070601     C                   MOVEL     'ST'          d10TLE                         *TIPO LEGAME
065900070601     C                   MOVEL     'P'           d10PAF                         *CERCA PADRE
066000070601     C                   MOVE      wfdCLI        d10COD
066100070531     C                   CALL      'TIBS10R'
066200070531     C                   PARM                    BS10DS
066300070601      *  Non Unificante
066400070601     C                   Z-ADD     wfdCLI        wCLIENTE
066500070601     C                   Z-ADD     wfdCLI        wUNIFICANTE
066600070601IF  2C     d10ERR        IFEQ      *BLANKS                                      *HA UN PADRE
066700070601     C     d10COP        ANDNE     *ZEROS
066800070601     C                   Z-ADD     d10COP        wUNIFICANTE                    *UNIFICANTE
066900070601      * UNificante
067000070601    1C     d9acu         ifeq      'U'                                          *PER UNIFICANTE
067100070601     C                   Z-ADD     wUNIFICANTE   wCLIENTE
067200070601    1C                   end
067300070531E   2C                   ENDIF
067400070531     C*
067500070601      ***   REPERISCO FILIALE/AREA/DIVISIONE DEL CLIENTE  ?
067600070601     C                   movel     wCLIENTE      wFILIALE                       *FIL+XXXX
067700070601     C                   Z-add     1             F
067800070601     C                   SEToff                                           99
067900070601     C     wFILIALE      LOOKUP    FIL(F)                                 99
068000070531IF  1C     *IN99         IFEQ      *ON
068100070601     C                   movel     DFIL(F)       desFILIALE
068200070601     C                   Z-ADD     ARE(F)        wAREA
068300070601     C                   movel     DARE(F)       desAREA
068400070601     C                   MOVEL     DIS(F)        wDISTRETTO
068500070601     C                   movel     DDIS(F)       desDISTRETTO
068600070531X   1C                   ELSE
068700070601     C                   Z-ADD     *ZEROS        wAREA
068800070601     C                   MOVEL     *BLANKS       desAREA
068900070601     C                   MOVEL     *BLANKS       wDISTRETTO
069000070601     C                   MOVEL     *BLANKS       desDISTRETTO
069100070601     C                   movel     *BLANKS       desFILIALE
069200070531E   1C                   ENDIF
069300070531     C*
069400070601      ***   REPERISCE IL COMMERCIALE e DATI ANGRAFICI   ?
069500070601     C*       SE NON ESISTE IMPOSTA IL CODICE AGENTE = FIL + 0000
069600070531     C                   CLEAR                   BS69DS
069700070531     C                   CLEAR                   CLPDS
069800070531     C                   MOVEL     KNSIF         I69SIF                         *S.INFORMATIVO
069900070601     C                   MOVEL     wCLIENTE      I69KAC                         *CLIENTE X CNACO
070000070601     C                   MOVEL     wCLIENTE      I69KIN                         *CLIENTE X CNIND
070100070601     C                   MOVEL     wCLIENTE      I69KCP                         *CLIENTE X CNCLP
070200070531     C                   CALL      'TIBS69R'
070300070531     C                   PARM                    BS69DS
070400070531     C                   PARM                    ACODS
070500070531     C                   PARM                    INDDS
070600070531     C                   PARM                    CLPDS
070700070531     C                   PARM                    CLSDS
070800070604IF  1C     O69ERR        ifNE      '1'                                          *NO ERRORE
070900130315IF  1C     acoabl        andne     *blank                                       *cliente bloccato
071000130315IF  1C     acoabl        andne     '*'                                          *cliente bloccato
071100070604     c     wbloccato     andne     *blank
071200070604     C                   MOVEL     'B'           wBLOCCATO                      *BLOCCO CLIENTE
071300070604     c                   end
071400070601     C                   MOVEL     *ALL'*'       desCLIENTE                     *SE NON TROVATO
071500070531IF  1C     O69ERR        IFNE      '1'                                          *NO ERRORE
071600070601      *  DATI ANAGRAFICI DEL CLIENTE   ?
071700070601     C                   MOVEL     acoRAG        desCLIENTE                     *RAGIONE SOCIALE
071800070601     C                   MOVEL     INDvia        wVIA                           *Indirizzo
071900070601     C                   MOVEL     INDcap        wCAP                           *CAP
072000070601     C                   MOVEL     INDcae        wCAPestero                     *CAP Europeo
072100070601     C                   MOVEL     INDprv        wPROVINCIA                     *Provincia
072200070601     C                   MOVEL     INDcit        wLOCALITA                      *Città
072300070601     C                   MOVEL     INDsta        wSTATO                         *Nazione
072400070719     c                   move      acoITC        wCAT_Cliente                   *Categoria SEAT
072500070719      *
072600070719      *  Categoria Cliente (SEAT)  ?
072700070719     C                   MOVEL     *BLANKS       wCAT_Cliente                   *SE NON TROVATO
072800070719     C                   Z-ADD     1             Ktb_KUT
072900070719     C                   MOVEL     '1L'          Ktb_COD
073000070719     C                   MOVEL     *BLANKS       Ktb_KEY
073100070719     C                   MOVE      acoITC        NUM5              5 0
073200070719     C                   MOVEL     NUM5          Ktb_KEY
073300070719     C     KeyTAB        CHAIN     TABEL00F
073400070719IF  1C                   IF        %Found(Tabel00F)
073500070719     C                   MOVEL     tblUNI        DS1L
073600070719     C                   MOVEL     §1LDES        wCAT_Cliente                   *CATEGORIA SEAT
073700070719E   1C                   ENDIF
073800070719      *
073900070531E   1C                   ENDIF
074000070601      *
074100070531IF  1C     O69ERR        IFEQ      '1'                                          *ERRORE
074200070601     C     clpAGE        OREQ      *ZEROS                                       *CODICE AGENTE VUOTO
074300070601     C                   MOVEL     wCLIENTE      NUM3              3 0          *CODICE FILIALE +
074400070601     C                   Z-ADD     *ZEROS        wAGENTE                        *AGENTE A ZERO  =
074500070601     C                   MOVEL     NUM3          wAGENTE                        *FIL+0000
074600070531X   1C                   ELSE                                                   *NO ERRORE
074700070601     C                   Z-ADD     clpAGE        wAGENTE                        *CODICE AGENTE
074800070531E   1C                   ENDIF
074900070531     C*
075000070531     C* REPERISCE IL CODICE QUALITA'
075100070601     C                   MOVEL     'C'           wQUALITA
075200070601IF  1C     clpCLV        IFNE      *BLANKS
075300070601     C                   MOVEL     CLPCLV        wQUALITA
075400070531E   1C                   ENDIF
075500070531     C*
075600070531     C* REPERISCO IL RAGGRUPPAMENTO FILIALE COMMERCIALE E RELATIVA DESCRIZIONE
075700070601     C                   Z-ADD     1             C                 5 0
075800070531     C                   SETOFF                                           99
075900070601     C     wAGENTE       LOOKUP    AGE(C)                                 99
076000070531IF  1C     *IN99         IFEQ      *OFF
076100070601     C                   Z-ADD     wAGENTE       wRAGGRage                      RAGGRUPP = COD AGENT
076200070601     C                   MOVEL     *ALL'*'       desRAGGRage                    DESCRIZIONE RAGGRUP
076300070531X   1C                   ELSE
076400070531IF  2C     RGF(C)        IFEQ      *ZEROS
076500070601     C                   Z-ADD     wAGENTE       wRAGGRage
076600070601     C                   MOVEL     *ALL'*'       desRAGGRage                    DESCRIZIONE RAGGRUP
076700070531X   2C                   ELSE
076800070601     C                   Z-ADD     RGF(C)        wRAGGRage
076900070531     C*
077000070531     C* DESCRIZIONE CODICE AGENTE = DESCRIZIONE CODICE RAGGRUPPAMENTO
077100070531     C                   Z-ADD     1             C
077200070531     C                   SETOFF                                           99
077300070601     C     wRAGGRage     LOOKUP    AGE(C)                                 99
077400070531IF  3C     *IN99         IFEQ      *ON
077500070601     C                   MOVEL     DAGE(C)       desRAGGRage                    DESCRIZIONE RAGGR.TO
077600070531X   3C                   ELSE
077700070601     C                   MOVEL     *ALL'*'       desRAGGRage                    DESCRIZIONE RAGGR.TO
077800070531E   3C                   ENDIF
077900070531E   2C                   ENDIF
078000070531E   1C                   ENDIF
078100070531     C*
078200070531     C* REPERISCO LA DIVISIONE DEL RAGGRUPPAMENTO COMMERCIALE
078300070531     C                   Z-ADD     1             F
078400070531     C                   SETOFF                                           99
078500070601     C                   MOVEL     wRAGGRage     NUM3                           *FIL+XXXX
078600070531     C     NUM3          LOOKUP    FIL(F)                                 99
078700070531IF  1C     *IN99         IFEQ      *ON
078800070601     C                   MOVEL     DIS(F)        wDISTRCOM
078900070531X   1C                   ELSE
079000070601     C                   MOVEL     *BLANKS       wDISTRCOM
079100070531E   1C                   ENDIF
079200070601     C*
079300070531     C                   ENDSR
079400070531     C*------------------------------------------------------------------------*
079500070531     C* carTAB - CARICA I DATI IN TABELLE
079600070531     C*------------------------------------------------------------------------*
079700070531     C     caricaTABELLE BEGSR
079800070531     C*
079900070531     C* ORGANIGRAMMA
080000070531     C                   Z-ADD     *ZEROS        F
080100070601     C                   Z-ADD     *ZEROS        KorFILIALE
080200070601     C     KorFILIALE    SetLL     AZORG01L
080300070601     C                   Read      AZORG01L                               99
080400070531DO  1C     *IN99         DOWEQ     '0'
080500070601IF  2C     orgFVA        IFEQ      *BLANKS                                      *NO ANNULLATI
080600070601IF  3C     orgFAG        IFNE      'V'                                          *FILIALE/AGENZIA
080700070601     C                   ADD       1             F                 5 0
080800070601     C                   Z-ADD     orgFIL        FIL(F)
080900070601     C                   MOVEL     orgDES        DFIL(F)
081000070601     C                   Z-ADD     1             Ktb_KUT
081100070601     C                   MOVEL     '05'          Ktb_COD
081200070601     C                   MOVEL     *BLANKS       Ktb_KEY
081300070601     C                   MOVEL     orgCAR        Ktb_KEY
081400070601     C     KeyTAB        CHAIN     TABEL00F                           99
081500070531IF  5C     *IN99         IFEQ      *OFF
081600070601     C                   Z-ADD     orgCAR        ARE(F)
081700070531     C                   MOVEL     TBLUNI        DARE(F)
081800070531E   5C                   ENDIF
081900070601     C                   Z-ADD     1             Ktb_KUT
082000070601     C                   MOVEL     '17'          Ktb_COD
082100070601     C                   MOVEL     *BLANKS       Ktb_KEY
082200070601     C                   MOVEL     orgFL3        Ktb_KEY
082300070601     C     KeyTAB        CHAIN     TABEL00F                           99
082400070531IF  5C     *IN99         IFEQ      *OFF
082500070601     C                   MOVEL     orgFL3        DIS(F)
082600070601     C                   MOVEL     TblUNI        DDIS(F)
082700070531E   5C                   ENDIF
082800070531E   3C                   ENDIF
082900070531E   2C                   ENDIF
083000070531     C                   READ      AZORG01L                               99
083100070531E   1C                   ENDDO
083200070531     C*--------------------
083300070531     C* COMMERCIALI
083400070531     C*--------------------
083500140612     C************       Z-ADD     *ZEROS        C
083600140612     C************       Z-ADD     1             Ktb_KUT
083700140612     C************       MOVEL     '01'          Ktb_COD
083800140612     C***  KeyTA2        SetLL     TABEL00F
083900140612     C***  KeyTA2        ReadE     TABEL00F                               99
084000140612DO  1C***  *IN99         DOWEQ     *OFF
084100140612IF  2C***  TblFLG        IFEQ      *BLANKS
084200140612     C************       ADD       1             C
084300140612     C************       MOVEL     TblUNI        DS01
084400140612     C************       MOVEL     TblKEY        AGE(C)
084500140612     C************       MOVEL     §01RGF        RGF(C)
084600140612     C************       MOVEL     §01AGE        DAGE(C)
084700140612E   2C************       end
084800140612     C***  KeyTA2        ReadE     TABEL00F                               99
084900140612E   1C************       ENDDO
085000140612      *-----
085100140612     C                   Z-ADD     *ZEROS        C
085200140612     c                   open      azCMM01l
085300140612     C     *loval        SETLL     azCMM01l
085400140612     C                   READ      azCMM01l
085500140612DO  1C                   DOW       not %Eof(azCMM01l)
085600140612IF  2C                   IF        cmmATB = *blank
085700140612     C                   ADD       1             C
085800140612     C                   z-add     cmmCOD        AGE(C)
085900140612     C                   z-add     cmmUNI        RGF(C)
086000140612     C                   MOVEL     cmmDES        DAGE(C)
086100140612E   2C                   ENDIF
086200140612     C                   READ      azCMM01l
086300140612E   1C                   ENDDO
086400140612     c                   close     azCMM01l
086500070531     C*
086600070531     C                   ENDSR
086700070531     C*------------------------------------------------------------------------*
