000100070601     h DECEDIT('0,') DATEDIT(*dmy.)
000200020409     H**********************************************************************
000300070531     FAZORG01L  IF   E           K DISK
000400070601     FTABEL00F  IF   E           K DISK
000500070531     fwfFPV00f  uf a E           k disk
000600070601     ftise9aP   o    E             PRINTER OFLIND(*in66) usropn
000700040820     H**********************************************************************
000800040820     d KPJBA         E DS
000900070530     D tise9ads      e DS
001000110823     D wadfpv0f      e DS
001100070531     D*
001200041103     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001300041103     D CNCR80        E DS
001400110511     D CNCR42        E DS
001500040820     D*
001600070531     D*------------------
001700070531     D* ORGANIGRAMMA
001800070531     D*------------------
001900070531     D FIL             S              3  0 DIM(500)                             *FILIALI
002000070531     D DFIL            S             20    DIM(500)
002100070601     D  desFILIALE     s             20
002200070531     D ARE             S              3  0 DIM(500)                             *AREE
002300070531     D DARE            S             20    DIM(500)
002400070601     D  desAREA        s             20
002500070601     D DIS             S              1    DIM(500)                             *DISTRETTO
002600070601     D DDIS            S             20    DIM(500)
002700070601     D  desDISTRETTO   s             20
002800070531     D*------------------
002900070531     D* COMMERCIALI
003000070531     D*------------------
003100100202     D AGE             S              7  0 DIM(5000)                            *CODICI AGENTE
003200100202     D RGF             S              7  0 DIM(5000)                            *CODICI RAGGRUPPAMEN
003300100202     D DAGE            S             25    DIM(5000)                            *DECODIFICHE AGENTI
003400070531     D*------------------
003500070531     D* DS COMMERCIALI
003600070531     D*------------------
003700070531     D DS01          E DS
003800070719     D*------------------
003900070719     D* DS Cat.SEAT
004000070719     D*------------------
004100070719     D DS1L          E DS
004200070531     D*------------------
004300070531     D* DS REPERIMENTO DATI UTENTE
004400070531     D*-------------------
004500070531     D BS10DS        E DS                  EXTNAME(TIBS10DS)
004600070531     D BS69DS        E DS                  EXTNAME(TIBS69DS) INZ
004700070531     D ACODS         E DS                  EXTNAME(CNACO00F) INZ
004800070531     D INDDS         E DS                  EXTNAME(CNIND00F) INZ
004900070531     D CLPDS         E DS                  EXTNAME(CNCLP00F) INZ
005000070531     D CLSDS         E DS                  EXTNAME(FNCLS00F) INZ
005100070531     D*------------------
005200040820     D WLBDAT          DS
005300040820     D  G02DAT                 1      8  0
005400040820     D  G02INV                 9     16  0
005500040820     D  G02ERR                17     17
005600040820     D  G02TGI                18     22  0
005700040820      *
005800070530      *?  Stringa SQL per lettura File Principale...  ?
005900040820     D WrkSqlCmd       S           1024
006000070601      *
006100070601      *?  Definizione campi LIKE ...  ?
006200040820     D  data_Oggi      s                   like(G02INV) INZ(0)
006300070601     D  CLIENTE_salva  s                   like(wfdCLI)
006400070601     D  Ktb_KUT        s                   like(tblKUT)
006500070601     D  Ktb_COD        s                   like(tblCOD)
006600070601     D  Ktb_KEY        s                   like(tblKEY)
006700070601     D  KorFILIALE     s                   like(orgFIL)
006800070601     D  desCLIENTE     s                   like(acoRAG)
006900070601     D  desRAGGRage    s                   like(§01AGE)
007000070601     D  wCLIENTE       s                   like(clpKSC)
007100070601     D  wUNIFICANTE    s                   like(clpKSC)
007200070601     D  wAGENTE        s                   like(clpAGE)
007300070601     D  wRAGGRage      s                   like(clpAGE)
007400070601     D  wAREA          s                   like(orgCAR)
007500070601     D  wDISTRETTO     s                   like(orgFL3)
007600070601     D  wDISTRCOM      s                   like(orgFL3)
007700070601     D  wFILIALE       s                   like(orgFIL)
007800070601     D  wBLOCCATO      s              1
007900070601     D  wVIA           s                   like(INDvia)
008000070601     D  wLOCALITA      s                   like(INDcit)
008100070601     D  wCAP           s                   like(INDcap)
008200070601     D  wCAPestero     s                   like(INDcae)
008300070601     D  wPROVINCIA     s                   like(INDprv)
008400070601     D  wSTATO         s                   like(INDsta)
008500070601     D  wQUALITA       s                   like(CLPCLV)
008600070719     D  wCAT_Cliente   s             25
008700070601      *
008800040826     D  almeno_uno     s              1
008900070530      *
009000040820     D digits          C                   CONST('0123456789')
009100040820      *****************************************************************
009200070530     c                   Exsr      Inizio
009300070530
009400070530      *?  Imposta la Stringa SLQ...  ?
009500011210     C/EXEC SQL
009600011210     C+ PREPARE S1 FROM :WrkSqlCmd
009700011210     C/END-EXEC
009800011210
009900070530      *?  Dichiara il Cursore SLQ...  ?
010000011210     C/EXEC SQL
010100011210     C+ DECLARE A1 CURSOR FOR S1
010200011210     C/END-EXEC
010300011210
010400070530      *?      Apre il Cursore SLQ...  ?
010500011210     C/EXEC SQL
010600011210     C+ OPEN A1
010700011210     C/END-EXEC
010800040820
010900040820     C                   DOU       SqlCod <> 0
011000040823     c*
011100070530      *?     Legge il Cursore SLQ...  ?
011200011205     C/EXEC SQL
011300110823     C+ FETCH NEXT FROM A1 INTO :WADFPV0F
011400011205     C/END-EXEC
011500011210     C                   SELECT
011600041007     c*
011700041007     c* a fine file Totali x rotture
011800041007     C                   WHEN      SqlCod = 100
011900041007     c                   leave
012000041007     **
012100041007     C                   WHEN      SqlCod < 0
012200041007     C                   seton                                        H1
012300041007     c                   goto      fine
012400040820     c*
012500041007     C                   OTHER
012600070531     c*  Dettaglio Riga
012700070531     c                   exsr      DETTAGLIO
012800040823     c* Totalizzo
012900040826     c                   move      'S'           almeno_uno
013000040823      *
013100011210     C                   ENDSL
013200040820
013300011205     C                   ENDDO
013400011205     C/EXEC SQL
013500011205     C+ CLOSE A1
013600011205     C/END-EXEC
013700041007     c     fine          tag
013800020326     C                   SETON                                        LR
013900040820     C**************************************************************************
014000040820     C* INIZIALIZZAZIONE
014100040820     C**************************************************************************
014200070530     C     Inizio        BEGSR
014300070531     C*
014400070531     C* CHIAVE LETTURA TABEL00F - COMPLETA
014500070601     C     KeyTAB        KLIST
014600070601     C                   KFLD                    Ktb_KUT                        *CODICE UTENTE
014700070601     C                   KFLD                    Ktb_COD                        *CODICE TABELLA
014800070601     C                   KFLD                    Ktb_KEY                        *CHIAVE TABELLA
014900070601      *
015000070601     C* CHIAVE LETTURA TABEL00F - PARZIALE
015100070601     C     KeyTA2        KLIST
015200070601     C                   KFLD                    Ktb_KUT                        *CODICE UTENTE
015300070601     C                   KFLD                    Ktb_COD                        *CODICE TABELLA
015400070601      **
015500070601     C     KeyFPV        KLIST
015600070601     c                   kfld                    fpvCLI
015700070601     c                   kfld                    fpvANN
015800070601     c                   kfld                    fpvMES
015900070601     c                   kfld                    fpvNTW
016000070601     c                   kfld                    fpvIOE
016100040820      **
016200070601      *?    Entrata programma    ?
016300040820     C     *ENTRY        PLIST
016400040820     C                   PARM                    KPJBA
016500070529     c                   movel     kpjbu         tise9ads
016600041103      *
016700041103     C                   Z-ADD     1             CODUT
016800041103     C                   CALL      'X§PARUT'
016900041103     C                   PARM                    UT§DSE
017000041103     C                   MOVEL     REC80         CNCR80
017100110511     C                   MOVEL     REC42         CNCR42
017200110511     C*****              MOVEL     RAGUT         RSUT             20
017300110511     C                   MOVEL     simRGS        RSUT             20
017400040820      *
017500040820      *  imposta UDATE di elaborazione
017600040820     C                   move      *date         G02DAT
017700040820     C                   MOVE      *ZEROS        G02INV
017800040820     C                   MOVE      *BLANKS       G02ERR
017900040820     C                   CALL      'XSRDA8'
018000040820     C                   PARM                    WLBDAT
018100040820     C                   Z-ADD     G02INV        data_oggi
018200040820     C                   move      data_oggi     day
018300040820     C                   TIME                    W0140            14 0
018400040820     C                   MOVEl     W0140         ore
018500040823      **
018600070531     c                   move      d9adel        p9adel
018700070531     c                   move      d9aora        p9aora
018800070530     c                   move      d9acu         p9acu
018900070530     c                   move      d9aQUA1       p9aQUA1
019000070530     c                   move      d9aQUA2       p9aQUA2
019100070530     c                   move      d9aQUA3       p9aQUA3
019200070530     c                   move      d9aQUA4       p9aQUA4
019300070530     c                   move      d9aQUA5       p9aQUA5
019400070530     c                   move      d9aIOE        p9aIOE
019500070530     c                   move      d9aNTW        p9aNTW
019600070601     c                   move      d9aNTWsin     p9aSINntw
019700070530     c                   move      d9aDIS        p9aDIS
019800070530     c                   move      d9aAREA       p9aAREA
019900070530     c                   move      d9aFIL        p9aFIL
020000070530     c                   move      d9aANNOC      p9aANNOC
020100070530     c                   move      d9aDMESC      p9aDMESC
020200070530     c                   move      d9aAMESC      p9aAMESC
020300070530     c                   move      d9aSINAC      p9aSINAC
020400070530     c                   move      d9aANNOP      p9aANNOP
020500070530     c                   move      d9aDMESP      p9aDMESP
020600070530     c                   move      d9aAMESP      p9aAMESP
020700070530     c                   move      d9aSINAP      p9aSINAP
020800040823      **
020900070531      ***   Stampa parametri di lancio  ?
021000070601     c                   open      tise9aP
021100070530     c                   write     PARAMETRI
021200070601     c                   close     tise9aP
021300070530      **
021400040825     c                   clear                   prima_volta       1
021500040826     c                   clear                   almeno_uno
021600070531     c*
021700070531      ***   carica tabelle di decodifica ?
021800070531     c                   exsr      caricaTABELLE
021900070531     c*
022000070531     C* Compone il primo giorno della data finale
022100070531     c                   if        d9aDMesC >0 or d9aAMesC >0
022200070531     C                   movel     d9aAnnoC      WDanm             6 0          *DATA finale
022300070531     C                   move      d9aAMesC      WDanm             6 0
022400070531     C                   movel     WDanm         WData_Finale      8 0
022500070531     C                   MOVE      28            WData_Finale
022600070531     c                   else
022700070531     C                   movel     d9aAnnoP      WDanm             6 0          *DATA finale
022800070531     C                   move      d9aAMesP      WDanm             6 0
022900070531     C                   movel     WDanm         WData_Finale      8 0
023000070531     C                   MOVE      28            WData_Finale
023100070531     c                   end
023200081114     C*
023300081114     C*------- GIORNO   di   F I N E   M E S E ---------------------*
023400081114     C*
023500081114     C*  Ultimo giorno del mese
023600081114     C                   MOVE      WData_Finale  MMGG_4            4 0
023700081114     C                   MOVEL     MMGG_4        Mm_2              2 0
023800081114     C                   MOVEl     WData_Finale  Anno_4            4 0
023900081114      * se è febbraio
024000081114     c                   If        Mm_2 = 2
024100081114      *
024200081114      *  potrebbe essere bisestile 29 gg.
024300081114     c     Anno_4        div       4             risulta           3 0
024400081114     c                   mvr                     resto             3 0
024500081114      *  Se bisestile
024600081114     c                   If        resto = 0
024700081114     C                   MOVE      '29'          WData_Finale
024800081114     c                   end
024900081114      *
025000081114     c                   else
025100081114      *
025200081114      *  se invece Aprile, Giugno, Settembre, Novembre con 30 gg.
025300081114     c                   If           Mm_2 = 4 or Mm_2 = 6 or
025400081114     c                                Mm_2 = 9 or Mm_2 = 11
025500081114     C                   MOVE      '30'          WData_Finale
025600081114     c                   else
025700081114     C                   MOVE      '31'          WData_Finale
025800081114     c                   end
025900081114      *
026000081114     c                   endIf
026100081114     C*
026200081114     C*----------- F I N E    M E S E ---------------------------*
026300081114     C*
026400070531      **
026500070531      ***-------------------------------------------------------*
026600070531      *?  MI COMPONGO L'ISTRUZIONE SQL                         ?*
026700110823      *?  che legge il file WaDFPV0F delle statistiche DELTA   ?*
026800070531      ***-------------------------------------------------------*
026900040820     C                   EVAL      WrkSqlCmd
027000040820     C                             =
027100110823     C                             'SELECT * from WADFPV0F where '
027200070530      ***----------------------
027300070530      ***   PERIODO    ?
027400070530      ***----------------------
027500070530      **  + Periodo anno in corso
027600070530     c                   if        d9aDMesC >0 or d9aAMesC >0
027700070530
027800070530     C                   EVAL      WrkSqlCmd
027900070530     C                             =
028000070530     C                             %TRIMR(WrkSqlCmd)
028100070530     c                             + ' ((wfdANN =' + %trim(%char(d9aAnnoC))
028200070530     c                             + ' and wfdMES between '
028300070530     c                             + %trim(%char(d9aDMesC)) + ' and '
028400070530     c                             + %trim(%char(d9aAMesC)) + ')'
028500070530     c                   end
028600070530      *
028700070530      * Se non c'è l'anno in corso
028800070530     c                   if        d9aDMesC =0 and d9aAMesC =0
028900070530     C                   EVAL      WrkSqlCmd
029000070530     C                             =
029100070530     C                             %TRIMR(WrkSqlCmd)
029200070530     c                             + ' ( '
029300070530     c                   end
029400070530      *
029500070530      * Mette in OR i periodi dei 2 anni
029600070530     c                   if        (d9aDMesC >0 or d9aAMesC >0) and
029700070530     c                             (d9aDMesP >0 or d9aAMesP >0)
029800070530     C                   EVAL      WrkSqlCmd
029900070530     C                             =
030000070530     C                             %TRIMR(WrkSqlCmd)
030100070530     c                             + ' or '
030200070530     c                   end
030300070530      *
030400070530      **  + Periodo anno precedente
030500070530     c                   if        d9aDMesP >0 or d9aAMesP >0
030600070530     C                   EVAL      WrkSqlCmd
030700070530     C                             =
030800070530     C                             %TRIMR(WrkSqlCmd)
030900070530     c                             + ' (wfdANN =' + %trim(%char(d9aAnnoP))
031000070530     c                             + ' and wfdMES between '
031100070530     c                             + %trim(%char(d9aDMesP)) + ' and '
031200070530     c                             + %trim(%char(d9aAMesP)) + ')'
031300070530     c                   end
031400070530      *  Chiude il periodo
031500070530     C                   EVAL      WrkSqlCmd
031600070530     C                             =
031700070530     C                             %TRIMR(WrkSqlCmd)
031800070530     c                             + ') '
031900070608      ***----------------------
032000070608      ***  ITALIA/ESTERO   ?
032100070608      ***----------------------
032200070608     c                   if        d9aIoE <> ' '
032300070608     C                   EVAL      WrkSqlCmd
032400070608     C                             =
032500070608     C                             %TRIMR(WrkSqlCmd)
032600070608     c                             + ' and wfdioe = ' + '''' + %trim(d9aIoE) +
032700070608     c                              ''''
032800070608     c                   end
032900070608      ***----------------------
033000070608      ***   NETWORK  ?
033100070608      ***----------------------
033200070608     c                   if        d9aNTW <> ' '
033300070608     C                   EVAL      WrkSqlCmd
033400070608     C                             =
033500070608     C                             %TRIMR(WrkSqlCmd)
033600070608     c                             + ' and wfdntw = ' + '''' + %trim(d9aNTW) +
033700070608     c                              ''''
033800070608     c                   end
033900070608      *
034000070608      ***----------------------
034100070608      ***  solo x NON UNIFICANTE
034200070608      ***----------------------
034300070608     c                   if        d9aCU = 'C'
034400070608      *
034500070530      ***----------------------
034600070530      ***   DISTRETTO      ?
034700070530      ***----------------------
034800070530     c                   if        d9aDIS <> ' '
034900070530     C                   EVAL      WrkSqlCmd
035000070530     C                             =
035100070530     C                             %TRIMR(WrkSqlCmd)
035200070530     c                             + ' and wfddiv = ' + '''' + %trim(d9aDIS) +
035300070530     c                              ''''
035400070530     c                   end
035500070530      ***----------------------
035600070530      ***    AREA          ?
035700070530      ***----------------------
035800070530     c                   if        d9aAREA <> ' '
035900070530     C                   EVAL      WrkSqlCmd
036000070530     C                             =
036100070530     C                             %TRIMR(WrkSqlCmd)
036200070530     c                             + ' and wfdare = ' + %trim(d9aArea)
036300070530     c                   end
036400070530      ***----------------------
036500070530      ***    FILIALE       ?
036600070530      ***----------------------
036700070530     c                   if        d9aFIL  <> ' '
036800070530     C                   EVAL      WrkSqlCmd
036900070530     C                             =
037000070530     C                             %TRIMR(WrkSqlCmd)
037100070530     c                             + ' and wfdfil = ' + %trim(d9aFIL)
037200070530     c                   end
037300070608      *
037400070608     c                   end
037500070608      ***----------------------
037600070608      ***  solo x NON UNIFICANTE
037700070608      ***----------------------
037800070608      *
037900070530      ***----------------------
038000070530      ***   Ordinamento di lettura   ?
038100070530      ***----------------------
038200040820     C                   EVAL      WrkSqlCmd
038300040820     C                             =
038400040820     C                             %TRIMR(WrkSqlCmd)
038500040820     c                             +
038600070531     c                             ' ORDER BY wfdCLI, wfdANN, wfdMES, wfdNTW'
038700040820      **
038800040820     C                   ENDSR
038900020326     C**************************************************************************
039000070531      *?  Scrittura del Workfile ...  ?   WFFPV00F x Cliente o Unificante
039100070530     C**************************************************************************
039200070531     C     DETTAGLIO     begsr
039300070531      *
039400070601      *  Se si tratta di Anno in Corso  in Sintesi      ?
039500070601      *  si deve impostare il mese genericamente a (0)  ?
039600070531     c                   if        wfdANN = d9aAnnoC and d9aSINaC = 'S'
039700070601     C                   Z-ADD     99            wfdMES
039800070531     C                   End
039900070531      *
040000070601      *  Se si tratta di Anno Precedente  in Sintesi    ?
040100070601      *  si deve impostare il mese genericamente a (0)  ?
040200070531     c                   if        wfdANN = d9aAnnoP and d9aSINaP = 'S'
040300070601     C                   Z-ADD     99            wfdMES
040400070531     C                   End
040500070531      *
040600070601      *  Se richiesta stampa sintetica x il Network
040700070601     c                   if        d9aNTWsin = 'S'
040800070601     C                   MOVE      *ALL'9'       wfdNTW
040900070601     C                   End
041000070601      *
041100070601      *  ?    Decodifica il Cliente a ogni cambio codice Cliente    ?
041200070601      *  esce con il campo ?  wCLIENTE  ? per impostare la chiave x il file
041300070601     c                   if        wfdCLI <> CLIENTE_salva
041400070601     c                   eval      CLIENTE_salva  = wfdCLI
041500070608      *
041600070601     c                   exsr      Decod_Cliente
041700070608      *
041800070608     c                   move      'S'           Record_OK         1
041900070608     c                   exsr      Filtro_Cliente
042000070601     c                   end
042100070531      *
042200070608      * Solo se OK
042300070608     c                   If        Record_OK = 'S'
042400070608      *
042500070601      *    Imposta la chiave e aggancia il    ?  WFFPV00F  ?
042600070601     c                   move      wCLIENTE      fpvCLI
042700070601     c                   move      wfdANN        fpvANN
042800070601     c                   move      wfdMES        fpvMES
042900070601     c                   move      wfdNTW        fpvNTW
043000070601     c                   move      wfdIOE        fpvIOE
043100070601      *
043200070601      ***   CHAIN al FILE da ESTRARRE di WORK   ?
043300070601     C     KeyFPV        chain     wfFPV00F
043400070601      *
043500070601     c                   if        %found(wfFPV00F)
043600070601      ***   Aggiorna   ?
043700070601     c                   exsr      FPV_upd_campi
043800070601     c                   update    wfFPV000
043900070601      *
044000070601     c                   else
044100070601      ***   Scrive     ?
044200070601     c                   exsr      FPV_wri_campi
044300070601     c                   exsr      FPV_upd_campi
044400070601     c                   write     wfFPV000
044500070601      *
044600070601     c                   end
044700070608      *
044800070608     c                   endIf
044900070601      *
045000070601     C                   EndSR
045100070601     C*------------------------------------------------------------------------*
045200070601      *?     Campi da Scrivere   sul record del file da estrarre   ?
045300070601     C*------------------------------------------------------------------------*
045400070601     C     FPV_wri_campi BEGSR
045500070601      *
045600070601     c                   clear                   wfFPV000
045700070601      *  Inizializza il campo
045800070601     c                   eval      fpvBLO = 'B'
045900070601      ***   chiave univoca  ?
046000070601     c                   move      wCLIENTE      fpvCLI
046100070601     c                   move      wfdANN        fpvANN
046200070601     c                   move      wfdMES        fpvMES
046300070601     c                   move      wfdNTW        fpvNTW
046400070601     c                   move      wfdIOE        fpvIOE
046500070601      *
046600070601      *   Decodificati in base al codice cliente (o unificante)
046700070601     c                   eval      fpvTCL = d9aCU
046800070601     c                   eval      fpvCLU = wUNIFICANTE
046900070601     c                   eval      fpvCLD = desCLIENTE
047000070719     c                   eval      fpvITC = wCAT_Cliente
047100070601     c                   eval      fpvCLV = wQUALITA
047200070601     c                   eval      fpvIND = wVIA
047300070601     c                   movel     wCAP          fpvCAP
047400070601     c                   eval      fpvLOC = wLOCALITA
047500070601     c                   eval      fpvPRV = wPROVINCIA
047600070601     c                   eval      fpvNAZ = wSTATO
047700070601     c                   eval      fpvDIV = wDISTRETTO
047800070601     c                   eval      fpvDID = desDISTRETTO
047900070601     c                   eval      fpvARE = wAREA
048000070601     c                   eval      fpvARD = desAREA
048100070601     c                   eval      fpvFIL = wFILIALE
048200070601     c                   eval      fpvFID = desFILIALE
048300070601     c                   eval      fpvDIC = wDISTRCOM
048400070601     c                   eval      fpvRGF = wRAGGRage
048500070601     c                   eval      fpvRGD = desRAGGRage
048600070601     c                   eval      fpvAGE = wAGENTE
048700070601      *
048800070530     C                   EndSR
048900070531     C*------------------------------------------------------------------------*
049000070601      *?    Campi da Aggiornare sul record del file da estrarre   ?
049100070531     C*------------------------------------------------------------------------*
049200070601     C     FPV_upd_campi BEGSR
049300070601      *
049400070601      *  Bloccato (basta che uno NON sia bloccato x NON bloccare gli altri)
049500070601     c                   if        wBLOCCATO = *blank
049600070601     c                   eval      fpvBLO = *blank
049700070601     c                   end
049800070601      *
049900070601      *    in base alla fascia di Peso incasella SPEDIZIONI PESI VOLUMI e RICAVI
050000070601     c                   if                 wfdFPE = '01'
050100070601     c                   add       WFDNSP        FPVNSP1
050200070601     c                   add       WFDPKG        FPVPES1
050300070601     c                   add       WFDVOL        FPVVOL1
050400070601     c                   add       WFDRRN        FPVRRN1
050500070601     c                   add       WFDRPN        FPVRPN1
050600070601     c                   end
050700070601      *
050800070601     c                   if                 wfdFPE = '02'
050900070601     c                   add       WFDNSP        FPVNSP2
051000070601     c                   add       WFDPKG        FPVPES2
051100070601     c                   add       WFDVOL        FPVVOL2
051200070601     c                   add       WFDRRN        FPVRRN2
051300070601     c                   add       WFDRPN        FPVRPN2
051400070601     c                   end
051500070601      *
051600070601     c                   if                 wfdFPE = '03'
051700070601     c                   add       WFDNSP        FPVNSP3
051800070601     c                   add       WFDPKG        FPVPES3
051900070601     c                   add       WFDVOL        FPVVOL3
052000070601     c                   add       WFDRRN        FPVRRN3
052100070601     c                   add       WFDRPN        FPVRPN3
052200070601     c                   end
052300070601      *
052400070601     c                   if                 wfdFPE = '04'
052500070601     c                   add       WFDNSP        FPVNSP4
052600070601     c                   add       WFDPKG        FPVPES4
052700070601     c                   add       WFDVOL        FPVVOL4
052800070601     c                   add       WFDRRN        FPVRRN4
052900070601     c                   add       WFDRPN        FPVRPN4
053000070601     c                   end
053100070601      *
053200070601     c                   if                 wfdFPE = '05'
053300070601     c                   add       WFDNSP        FPVNSP5
053400070601     c                   add       WFDPKG        FPVPES5
053500070601     c                   add       WFDVOL        FPVVOL5
053600070601     c                   add       WFDRRN        FPVRRN5
053700070601     c                   add       WFDRPN        FPVRPN5
053800070601     c                   end
053900070601      *
054000070601     c                   if                 wfdFPE = '06'
054100070601     c                   add       WFDNSP        FPVNSP6
054200070601     c                   add       WFDPKG        FPVPES6
054300070601     c                   add       WFDVOL        FPVVOL6
054400070601     c                   add       WFDRRN        FPVRRN6
054500070601     c                   add       WFDRPN        FPVRPN6
054600070601     c                   end
054700070601      *
054800070601     c                   if                 wfdFPE = '07'
054900070601     c                   add       WFDNSP        FPVNSP7
055000070601     c                   add       WFDPKG        FPVPES7
055100070601     c                   add       WFDVOL        FPVVOL7
055200070601     c                   add       WFDRRN        FPVRRN7
055300070601     c                   add       WFDRPN        FPVRPN7
055400070601     c                   end
055500070601      *
055600070601     c                   if                 wfdFPE = '08'
055700070601     c                   add       WFDNSP        FPVNSP8
055800070601     c                   add       WFDPKG        FPVPES8
055900070601     c                   add       WFDVOL        FPVVOL8
056000070601     c                   add       WFDRRN        FPVRRN8
056100070601     c                   add       WFDRPN        FPVRPN8
056200070601     c                   end
056300070601      *
056400070601     c                   if                 wfdFPE = '09'
056500070601     c                   add       WFDNSP        FPVNSP9
056600070601     c                   add       WFDPKG        FPVPES9
056700070601     c                   add       WFDVOL        FPVVOL9
056800070601     c                   add       WFDRRN        FPVRRN9
056900070601     c                   add       WFDRPN        FPVRPN9
057000070601     c                   end
057100070601      *
057200070601     c                   if                 wfdFPE = '10'
057300070601     c                   add       WFDNSP        FPVNSP0
057400070601     c                   add       WFDPKG        FPVPES0
057500070601     c                   add       WFDVOL        FPVVOL0
057600070601     c                   add       WFDRRN        FPVRRN0
057700070601     c                   add       WFDRPN        FPVRPN0
057800070601     c                   end
057900070601      *
058000070601     C                   EndSR
058100070601     C*------------------------------------------------------------------------*
058200070608      *?   Controlla se il Cliente è entro le selzioni del Video          ?
058300070601     C*------------------------------------------------------------------------*
058400070608     C     Filtro_ClienteBEGSR
058500070601     C*
058600070608     C* Filiale Selezionata
058700070608     c                   if        D9AFIL <> *blank
058800070608     c                   move      wFiliale      aFiliale          3
058900070608     c                   if        D9AFIL <>     aFiliale
059000070608     c                   eval      record_OK = 'N'
059100070608     C                   End
059200070608     C                   End
059300070608     C*
059400070608     C* Area    Selezionata
059500070608     c                   if        D9AAREA <> *blank
059600070608     c                   move      wArea         aArea             3
059700070608     c                   if        D9AAREA <>    aArea
059800070608     c                   eval      record_OK = 'N'
059900070608     C                   End
060000070608     C                   End
060100070608     C*
060200070608     C* Distretto Selezionato
060300070608     c                   if        D9ADIS  <> *blank
060400070608     c                   move      wDistretto    aDistretto        1
060500070608     c                   if        D9ADIS  <>    aDistretto
060600070608     c                   eval      record_OK = 'N'
060700070608     C                   End
060800070608     C                   End
060900070608      *
061000070608     C* Qualità Selezionata
061100070608     c                   if        D9AQUA1 <> ' ' or
061200070608     c                             D9AQUA2 <> ' ' or
061300070608     c                             D9AQUA3 <> ' ' or
061400070608     c                             D9AQUA4 <> ' ' or
061500070608     c                             D9AQUA5 <> ' '
061600070608     c                   if        D9AQUA1 =  wQualita or
061700070608     c                             D9AQUA2 =  wQualita or
061800070608     c                             D9AQUA3 =  wQualita or
061900070608     c                             D9AQUA4 =  wQualita or
062000070608     c                             D9AQUA5 =  wQualita
062100070608     c                   else
062200070608     c                   eval      record_OK = 'N'
062300070608     C                   End
062400070608     C                   End
062500070608      *
062600070608     C                   EndSR
062700070608     C*------------------------------------------------------------------------*
062800070608      *?     OPERAZIONI PER decodificare il NUOVO CLIENTE appena letto    ?
062900070608     C*------------------------------------------------------------------------*
063000070608     C     Decod_Cliente BEGSR
063100070608     C*
063200070601     C*  A livello  Cliente verifica se bloccato
063300070601     C                   CLEAR                   BS69DS
063400070601     C                   CLEAR                   CLPDS
063500070601     C                   MOVEL     KNSIF         I69SIF                         *S.INFORMATIVO
063600070604     C                   MOVEL     CLIENTE_salva I69KAC                         *CLIENTE X CNACO
063700070601     C                   CALL      'TIBS69R'
063800070601     C                   PARM                    BS69DS
063900070601     C                   PARM                    ACODS
064000070601     C                   PARM                    INDDS
064100070601     C                   PARM                    CLPDS
064200070601     C                   PARM                    CLSDS
064300070601      *
064400070601     C                   MOVEL     *BLANKS       wBLOCCATO                      *SE NON TROVATO
064500070601IF  1C     O69ERR        ifNE      '1'                                          *NO ERRORE
064600130315IF  1C     acoabl        andne     *blank                                       *cliente bloccato
064601130315IF  1C     acoabl        andne     '*'                                          *cliente bloccato
064700070601     C                   MOVEL     'B'           wBLOCCATO                      *BLOCCO CLIENTE
064800070601     c                   end
064900070531     C*
065000070601      ***   IMPOSTA IL CLIENTE DA SCRIVERE SUL WFILE -CLIENTE O UNIFICANTE-   ?
065100070531     C                   CLEAR                   BS10DS
065200070601     C                   Z-ADD     WData_Finale  d10DRF                         *DATA FINALE LANCIO
065300070601     C                   MOVEL     'ST'          d10TLE                         *TIPO LEGAME
065400070601     C                   MOVEL     'P'           d10PAF                         *CERCA PADRE
065500070601     C                   MOVE      wfdCLI        d10COD
065600070531     C                   CALL      'TIBS10R'
065700070531     C                   PARM                    BS10DS
065800070601      *  Non Unificante
065900070601     C                   Z-ADD     wfdCLI        wCLIENTE
066000070601     C                   Z-ADD     wfdCLI        wUNIFICANTE
066100070601IF  2C     d10ERR        IFEQ      *BLANKS                                      *HA UN PADRE
066200070601     C     d10COP        ANDNE     *ZEROS
066300070601     C                   Z-ADD     d10COP        wUNIFICANTE                    *UNIFICANTE
066400070601      * UNificante
066500070601    1C     d9acu         ifeq      'U'                                          *PER UNIFICANTE
066600070601     C                   Z-ADD     wUNIFICANTE   wCLIENTE
066700070601    1C                   end
066800070531E   2C                   ENDIF
066900070531     C*
067000070601      ***   REPERISCO FILIALE/AREA/DIVISIONE DEL CLIENTE  ?
067100070601     C                   movel     wCLIENTE      wFILIALE                       *FIL+XXXX
067200070601     C                   Z-add     1             F
067300070601     C                   SEToff                                           99
067400070601     C     wFILIALE      LOOKUP    FIL(F)                                 99
067500070531IF  1C     *IN99         IFEQ      *ON
067600070601     C                   movel     DFIL(F)       desFILIALE
067700070601     C                   Z-ADD     ARE(F)        wAREA
067800070601     C                   movel     DARE(F)       desAREA
067900070601     C                   MOVEL     DIS(F)        wDISTRETTO
068000070601     C                   movel     DDIS(F)       desDISTRETTO
068100070531X   1C                   ELSE
068200070601     C                   Z-ADD     *ZEROS        wAREA
068300070601     C                   MOVEL     *BLANKS       desAREA
068400070601     C                   MOVEL     *BLANKS       wDISTRETTO
068500070601     C                   MOVEL     *BLANKS       desDISTRETTO
068600070601     C                   movel     *BLANKS       desFILIALE
068700070531E   1C                   ENDIF
068800070531     C*
068900070601      ***   REPERISCE IL COMMERCIALE e DATI ANGRAFICI   ?
069000070601     C*       SE NON ESISTE IMPOSTA IL CODICE AGENTE = FIL + 0000
069100070531     C                   CLEAR                   BS69DS
069200070531     C                   CLEAR                   CLPDS
069300070531     C                   MOVEL     KNSIF         I69SIF                         *S.INFORMATIVO
069400070601     C                   MOVEL     wCLIENTE      I69KAC                         *CLIENTE X CNACO
069500070601     C                   MOVEL     wCLIENTE      I69KIN                         *CLIENTE X CNIND
069600070601     C                   MOVEL     wCLIENTE      I69KCP                         *CLIENTE X CNCLP
069700070531     C                   CALL      'TIBS69R'
069800070531     C                   PARM                    BS69DS
069900070531     C                   PARM                    ACODS
070000070531     C                   PARM                    INDDS
070100070531     C                   PARM                    CLPDS
070200070531     C                   PARM                    CLSDS
070300070604IF  1C     O69ERR        ifNE      '1'                                          *NO ERRORE
070400130315IF  1C     acoabl        andne     *blank                                       *cliente bloccato
070401130315IF  1C     acoabl        andne     '*'                                          *cliente bloccato
070500070604     c     wbloccato     andne     *blank
070600070604     C                   MOVEL     'B'           wBLOCCATO                      *BLOCCO CLIENTE
070700070604     c                   end
070800070601     C                   MOVEL     *ALL'*'       desCLIENTE                     *SE NON TROVATO
070900070531IF  1C     O69ERR        IFNE      '1'                                          *NO ERRORE
071000070601      *  DATI ANAGRAFICI DEL CLIENTE   ?
071100070601     C                   MOVEL     acoRAG        desCLIENTE                     *RAGIONE SOCIALE
071200070601     C                   MOVEL     INDvia        wVIA                           *Indirizzo
071300070601     C                   MOVEL     INDcap        wCAP                           *CAP
071400070601     C                   MOVEL     INDcae        wCAPestero                     *CAP Europeo
071500070601     C                   MOVEL     INDprv        wPROVINCIA                     *Provincia
071600070601     C                   MOVEL     INDcit        wLOCALITA                      *Città
071700070601     C                   MOVEL     INDsta        wSTATO                         *Nazione
071800070719     c                   move      acoITC        wCAT_Cliente                   *Categoria SEAT
071900070719      *
072000070719      *  Categoria Cliente (SEAT)  ?
072100070719     C                   MOVEL     *BLANKS       wCAT_Cliente                   *SE NON TROVATO
072200070719     C                   Z-ADD     1             Ktb_KUT
072300070719     C                   MOVEL     '1L'          Ktb_COD
072400070719     C                   MOVEL     *BLANKS       Ktb_KEY
072500070719     C                   MOVE      acoITC        NUM5              5 0
072600070719     C                   MOVEL     NUM5          Ktb_KEY
072700070719     C     KeyTAB        CHAIN     TABEL00F
072800070719IF  1C                   IF        %Found(Tabel00F)
072900070719     C                   MOVEL     tblUNI        DS1L
073000070719     C                   MOVEL     §1LDES        wCAT_Cliente                   *CATEGORIA SEAT
073100070719E   1C                   ENDIF
073200070719      *
073300070531E   1C                   ENDIF
073400070601      *
073500070531IF  1C     O69ERR        IFEQ      '1'                                          *ERRORE
073600070601     C     clpAGE        OREQ      *ZEROS                                       *CODICE AGENTE VUOTO
073700070601     C                   MOVEL     wCLIENTE      NUM3              3 0          *CODICE FILIALE +
073800070601     C                   Z-ADD     *ZEROS        wAGENTE                        *AGENTE A ZERO  =
073900070601     C                   MOVEL     NUM3          wAGENTE                        *FIL+0000
074000070531X   1C                   ELSE                                                   *NO ERRORE
074100070601     C                   Z-ADD     clpAGE        wAGENTE                        *CODICE AGENTE
074200070531E   1C                   ENDIF
074300070531     C*
074400070531     C* REPERISCE IL CODICE QUALITA'
074500070601     C                   MOVEL     'C'           wQUALITA
074600070601IF  1C     clpCLV        IFNE      *BLANKS
074700070601     C                   MOVEL     CLPCLV        wQUALITA
074800070531E   1C                   ENDIF
074900070531     C*
075000070531     C* REPERISCO IL RAGGRUPPAMENTO FILIALE COMMERCIALE E RELATIVA DESCRIZIONE
075100070601     C                   Z-ADD     1             C                 5 0
075200070531     C                   SETOFF                                           99
075300070601     C     wAGENTE       LOOKUP    AGE(C)                                 99
075400070531IF  1C     *IN99         IFEQ      *OFF
075500070601     C                   Z-ADD     wAGENTE       wRAGGRage                      RAGGRUPP = COD AGENT
075600070601     C                   MOVEL     *ALL'*'       desRAGGRage                    DESCRIZIONE RAGGRUP
075700070531X   1C                   ELSE
075800070531IF  2C     RGF(C)        IFEQ      *ZEROS
075900070601     C                   Z-ADD     wAGENTE       wRAGGRage
076000070601     C                   MOVEL     *ALL'*'       desRAGGRage                    DESCRIZIONE RAGGRUP
076100070531X   2C                   ELSE
076200070601     C                   Z-ADD     RGF(C)        wRAGGRage
076300070531     C*
076400070531     C* DESCRIZIONE CODICE AGENTE = DESCRIZIONE CODICE RAGGRUPPAMENTO
076500070531     C                   Z-ADD     1             C
076600070531     C                   SETOFF                                           99
076700070601     C     wRAGGRage     LOOKUP    AGE(C)                                 99
076800070531IF  3C     *IN99         IFEQ      *ON
076900070601     C                   MOVEL     DAGE(C)       desRAGGRage                    DESCRIZIONE RAGGR.TO
077000070531X   3C                   ELSE
077100070601     C                   MOVEL     *ALL'*'       desRAGGRage                    DESCRIZIONE RAGGR.TO
077200070531E   3C                   ENDIF
077300070531E   2C                   ENDIF
077400070531E   1C                   ENDIF
077500070531     C*
077600070531     C* REPERISCO LA DIVISIONE DEL RAGGRUPPAMENTO COMMERCIALE
077700070531     C                   Z-ADD     1             F
077800070531     C                   SETOFF                                           99
077900070601     C                   MOVEL     wRAGGRage     NUM3                           *FIL+XXXX
078000070531     C     NUM3          LOOKUP    FIL(F)                                 99
078100070531IF  1C     *IN99         IFEQ      *ON
078200070601     C                   MOVEL     DIS(F)        wDISTRCOM
078300070531X   1C                   ELSE
078400070601     C                   MOVEL     *BLANKS       wDISTRCOM
078500070531E   1C                   ENDIF
078600070601     C*
078700070531     C                   ENDSR
078800070531     C*------------------------------------------------------------------------*
078900070531     C* carTAB - CARICA I DATI IN TABELLE
079000070531     C*------------------------------------------------------------------------*
079100070531     C     caricaTABELLE BEGSR
079200070531     C*
079300070531     C* ORGANIGRAMMA
079400070531     C                   Z-ADD     *ZEROS        F
079500070601     C                   Z-ADD     *ZEROS        KorFILIALE
079600070601     C     KorFILIALE    SetLL     AZORG01L
079700070601     C                   Read      AZORG01L                               99
079800070531DO  1C     *IN99         DOWEQ     '0'
079900070601IF  2C     orgFVA        IFEQ      *BLANKS                                      *NO ANNULLATI
080000070601IF  3C     orgFAG        IFNE      'V'                                          *FILIALE/AGENZIA
080100070601     C                   ADD       1             F                 5 0
080200070601     C                   Z-ADD     orgFIL        FIL(F)
080300070601     C                   MOVEL     orgDES        DFIL(F)
080400070601     C                   Z-ADD     1             Ktb_KUT
080500070601     C                   MOVEL     '05'          Ktb_COD
080600070601     C                   MOVEL     *BLANKS       Ktb_KEY
080700070601     C                   MOVEL     orgCAR        Ktb_KEY
080800070601     C     KeyTAB        CHAIN     TABEL00F                           99
080900070531IF  5C     *IN99         IFEQ      *OFF
081000070601     C                   Z-ADD     orgCAR        ARE(F)
081100070531     C                   MOVEL     TBLUNI        DARE(F)
081200070531E   5C                   ENDIF
081300070601     C                   Z-ADD     1             Ktb_KUT
081400070601     C                   MOVEL     '17'          Ktb_COD
081500070601     C                   MOVEL     *BLANKS       Ktb_KEY
081600070601     C                   MOVEL     orgFL3        Ktb_KEY
081700070601     C     KeyTAB        CHAIN     TABEL00F                           99
081800070531IF  5C     *IN99         IFEQ      *OFF
081900070601     C                   MOVEL     orgFL3        DIS(F)
082000070601     C                   MOVEL     TblUNI        DDIS(F)
082100070531E   5C                   ENDIF
082200070531E   3C                   ENDIF
082300070531E   2C                   ENDIF
082400070531     C                   READ      AZORG01L                               99
082500070531E   1C                   ENDDO
082600070531     C*--------------------
082700070531     C* COMMERCIALI
082800070531     C*--------------------
082900070531     C                   Z-ADD     *ZEROS        C
083000070601     C                   Z-ADD     1             Ktb_KUT
083100070601     C                   MOVEL     '01'          Ktb_COD
083200070601     C     KeyTA2        SetLL     TABEL00F
083300070601     C     KeyTA2        ReadE     TABEL00F                               99
083400070531DO  1C     *IN99         DOWEQ     *OFF
083500070601IF  2C     TblFLG        IFEQ      *BLANKS
083600070531     C                   ADD       1             C
083700070601     C                   MOVEL     TblUNI        DS01
083800070601     C                   MOVEL     TblKEY        AGE(C)
083900070531     C                   MOVEL     §01RGF        RGF(C)
084000070531     C                   MOVEL     §01AGE        DAGE(C)
084100070601E   2C                   end
084200070601     C     KeyTA2        ReadE     TABEL00F                               99
084300070531E   1C                   ENDDO
084400070531     C*
084500070531     C                   ENDSR
084600070531     C*------------------------------------------------------------------------*
