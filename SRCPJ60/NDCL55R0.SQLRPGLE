000100981202     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND) ACTGRP(QILE)
000200031202     H*PARMS COMMIT(*NONE) DATFMT(*ISO) RDB(*NONE)
000300031202     H*PARMS DYNUSRPRF(*OWNER)
000400941130     H DECEDIT('0,') DATEDIT(*DMY.)
000500031202      *
000600060207      *       -----------------------------------------------------
000700060207      *                     Recupero crediti
000800060712      *        Lancio stampa lettere sollecito
000900060712      *        Sostituisce NDCL55R e NDCL54R1 (obsoleti)
001000031202      *       -----------------------------------------------------
001100960319      *
001200060207     D*-------------
001300060712     FNdPiw00F  UF   E             DISK
001400060712     FNDEpa02L  UF   E           K DISK
001500031211     D*-------------
001600060712     D NdpiW         E DS                  EXTNAME(NdPiw00f)
001700060209     D NRRSql          S              9B 0
001800060208     D*-------------
001900031202     D* Passaggio Parametri
002000031202     D KPJBA         E DS
002100060208     D*-------------
002200060208     D XSOCDS          S           1000
002300060208     D SOC001        E DS                  EXTNAME(XSOC001DS)
002400031202     D*-------------
002500060207     D* Parametri in ricezione
002600060712     D NDCL52DS      E DS
002700060712     D*-------------
002800060712     D* Parametri passaggio stampa lettere
002900060712     D NDCL55DS      E DS
003000031202     D*-------------
003100031202     D* Stringa per SQL
003200031202     D SQ              DS          5000
003300060208     D*-------------
003400031202     D FETCH           C                   CONST('FOR FETCH ONLY')
003500060712     D* ordinamento cliente
003600060712     D ORDER1          C                   CONST('Order by  piwke1,  -
003700060712     D                                     piwdrp, piwnrp, piwserpar')
003800060712     D* ordinamento agente/cliente
003900060712     D ORDER2          C                   CONST('Order by  piwke1,  -
004000060712     D                                     piwke2, piwdrp, piwnrp, piwserpar')
004100060712     D* Costanti per selezionare promemoria o no
004200060712     D promem          C                   CONST('piwke6 =')
004300060712     D nopromem        C                   CONST('piwke6 <>')
004400031204     D*-------------
004500031204     D* Reperimento nome PGM
004600031204     D STATUS         SDS           333
004700031204     D  DSPGM            *PROC
004800031204     D  PARMS            *PARMS
004900031204     D  SDSUSR               254    263
005000031204     D  JOB_NUM              264    269S 0
005100941130     C*----------------------------------------------------*
005200941130     C*                MAIN LINE PROGRAM
005300941130     C*----------------------------------------------------*
005400941130     C*
005500941130     C* inizializzazione variabili
005600060207     C                   EXSR      InzVar
005700941130     C*
005800060207     C* esecuzione
005900060207     C                   EXSR      Esegui
006000941130     C*
006100941130     C* fine programma
006200060207     C                   EXSR      EndPgm
006300941130     C*
006400941130     C/EJECT
006500941130     C************************************************************
006600941130     C* FINE PROGRAMMA
006700941130     C************************************************************
006800941130     C     ENDPGM        BEGSR
006900060712     C*
007000060712     C                   SETON                                            LR
007100060712     C                   RETURN
007200941130     C*
007300941130     C                   ENDSR
007400941130     C/EJECT
007500941130     C************************************************************
007600060208     C* Esecuzione programma
007700941130     C************************************************************
007800060207     C     Esegui        BEGSR
007900060712     c*
008000060208     C* Preparazione stringa sql
008100060208     C                   EXSR      InzSQL
008200060208     C* Esecuzione
008300060208     C                   EXSR      ExeSql
008400060712     C* Chiusura prtf
008500060712     C                   EXSR      CloSql
008600060207     c*
008700031202     C                   ENDSR
008800060208     C************************************************************
008900060208     C* Esecuzione Sql
009000060208     C************************************************************
009100060208     C     ExeSql        BEGSR
009200060208      *
009300060208     C/EXEC SQL
009400060208     C+     PREPARE S1 FROM :SQ
009500060208     C/END-EXEC
009600060208      *
009700060208     C/EXEC SQL
009800060208     C+     DECLARE A1 CURSOR FOR S1
009900060208     C/END-EXEC
010000060208     C*
010100060208     C/EXEC SQL
010200060208     C+     OPEN A1
010300060208     C/END-EXEC
010400060712     C*
010500060712     C/EXEC SQL
010600060712     C+     FETCH A1 INTO :NDPIW, :NRRSQL
010700060712     C/END-EXEC
010800060208     c                   move      *on           OkSqlCod          1
010900060712     c                   exsr      ImpSqlCod
011000060725     c*
011100060725b1   c     OkSqlcod      ifeq      *on
011200060712     c* Ordinamento per cliente
011300060712     c                   if        L52Ord='1'
011400060713     c                   movel     Piwke1        WrKKey           20
011500060712     c                   else
011600060712     c* Ordinamento per agente/cliente
011700060713     c                   movel     piwke1        WrkKey
011800060713     c                   move      piwke2        WrkKey
011900060712     c                   endif
012000060725     c                   endif
012100060331     C*
012200060712b1   c     OkSqlcod      doweq     *on
012300060712     c* a rottura chiave, chiamo stampa lettera che ritorna il numero
012400060712     c* sollecito o promemoria
012500060712     c     Wrkkey        ifne      SavKey
012600060713     c                   move      Wrkkey        SavKey           20
012700060712     c* chiama stampa lettera
012800060712     c                   exsr      CallStpLet
012900060712     c                   endif
013000060712     c*
013100060712     c*  se stampa definitiva aggiorna ogni record letto
013200060712     c                   if        L52Def ='S'
013300060712     c                   exsr      Aggiorna
013400060712     c                   endif
013500060712     c*
013600060712     C/EXEC SQL
013700060712     C+     FETCH A1 INTO :NDPIW, :NRRSQL
013800060712     C/END-EXEC
013900060712     c                   exsr      ImpSqlCod
014000060712     c*
014100060712     c* Ordinamento 1 cliente/partita
014200060712     c                   if        L52Ord='1'
014300060712     c                   movel     piwke1        wrkKey           20
014400060712     c                   else
014500060712     c* Ordinamento 2 agente/cliente/partita
014600060712     c                   movel     piwke1        wrkKey
014700060712     c                   move      piwke2        wrkKey
014800060712     c                   endif
014900060331     c*
015000060712 e1  C                   enddo
015100060208     c*
015200060208     c                   endsr
015300060208     C*
015400060403     C************************************************************
015500060712     C* Operazioni primo cliente/o agente/cliente
015600060403     C************************************************************
015700060712     C     CallStpLet    BEGSR
015800060403     c*
015900060712     C* STAMPA LETTERA
016000060712     C                   move      PIWSAP        KscAgel55
016100060712     C                   move      PIWKSC        KscL55
016200060712     C     PIWFLA        IFEQ      'S'
016300060712     C                   Z-ADD     PIWNUM        NumL55
016400060712     C                   ELSE
016500060712     C                   MOVE      *ZEROS        NumL55
016600060712     C                   endif
016700060712     C                   MOVEL     NDCL52DS      KPJBU
016800060712     C*
016900060712     C                   MOVE      Promemoria    PromemL55
017000090529     C                   SELECT
017100090529     C     L52LET        WHENEQ    'Y'
017200090529     C                   CALL      'YL000001R'
017300090529     C                   PARM                    KPJBA
017400090529     C                   PARM                    NDCL55DS
017500090529     C     L52LET        WHENEQ    '1'
017600060712     C                   CALL      'NDCL55R1'
017700060712     C                   PARM                    KPJBA
017800060712     C                   PARM                    NDCL55DS
017900090529     C                   OTHER
018000060712     C                   CALL      'NDCL55R2'
018100060712     C                   PARM                    KPJBA
018200060712     C                   PARM                    NDCL55DS
018300090529     C                   ENDSL
018400060712     c*
018500060403     c                   endsr
018600060209     C************************************************************
018700060712     C* Aggiorna file con flag di stampato
018800060209     C************************************************************
018900060209     C     Aggiorna      BEGSR
019000060209     C*
019100060712     c     NrrSql        chain     NdPIW00F
019200060712     c                   if        %found(ndPIW00f)
019300060712     c                             and piwfla <> 'S'
019400060712     C*
019500060712     c                   move      L52CSI        EpaCsi
019600060712     c                   move      L52Dva        EpaDva
019700060712     c                   move      L52Kcc        EpaKcc
019800060712     C                   move      PIWKSC        Epaksc
019900060712     C                   move      PIWDRP        EpaDrp
020000060712     C                   move      PIWNRP        EpaNrp
020100060712     C                   move      PIWSERPAR     EpaSerPar
020200060712     C*
020300060712     C     K07Epa02      chain     NDepa02L
020400060712     C                   If        %Found(NdEpa02L)
020500060712     C                   MOVE      'S'           EPAFLA
020600060712     C                   Z-ADD     NumL55        EPANUM
020700060712     C                   move      L52DFS        EPADAT
020800060712     C                   EXCEPT    AGGEPA
020900060712     C                   endif
021000060712     c*
021100060712     C                   MOVE      'S'           PIWFLA
021200060712     C                   Z-ADD     NumL55        PIWNUM
021300060712     C                   move      L52DFS        PIWDAT
021400060712     c                   update    ndpiw000
021500060209     c                   endif
021600060209     c*
021700060209     c                   ENDSR
021800060712     C/EJECT
021900060712     C************************************************************
022000060712     C* Ultima chiamata per chiudere prtf
022100060712     C************************************************************
022200060712     C     CloSql        BEGSR
022300060712     c*
022400060712     c* Ultima chiamata per chiudere prtf
022500060712     C                   Z-ADD     *all'9'       NumL55
022600060712     C                   MOVEL     NDCL52DS      KPJBU
022700060712     C*
022800090529     C                   SELECT
022900090529     C     L52LET        WHENEQ    'Y'
023000090529     C     L52LET        WHENEQ    '1'
023100060712     C                   CALL      'NDCL55R1'
023200060712     C                   PARM                    KPJBA
023300060712     C                   PARM                    NDCL55DS
023400090529     C                   OTHER
023500060712     C                   CALL      'NDCL55R2'
023600060712     C                   PARM                    KPJBA
023700060712     C                   PARM                    NDCL55DS
023800090529     C                   ENDSL
023900060712     C*
024000060712     C                   EXSR      CLOCUR
024100060712     C*
024200060712     C                   ENDSR
024300060712     C/EJECT
024400941130     C************************************************************
024500941130     C* CHIUSURA CURSORE
024600941130     C************************************************************
024700941130     C     CLOCUR        BEGSR
024800941130     C*
024900060712     C/EXEC SQL
025000060712     C+     CLOSE A1
025100060712     C/END-EXEC
025200941201     C*
025300941130     C                   ENDSR
025400941130     C/EJECT
025500941130     C************************************************************
025600941130     C* OPERAZIONI INIZIALI
025700941130     C************************************************************
025800941130     C     *INZSR        BEGSR
025900941130     C*
026000941130     C* Reperimento parametri
026100941130     C*
026200941130     C     *ENTRY        PLIST
026300031202     C                   parm                    kpjba
026400060712     C                   parm                    Promemoria        1
026500060712     c*
026600060712     C                   MOVEL     KPJBU         NDCL52DS
026700060712     C*
026800060712     C                   exsr      CloCur
026900041015     C*
027000941130     C                   ENDSR
027100060208     c*
027200060208     C***************************************
027300060209     C* Elenco KLIST usate
027400060208     C***************************************
027500060208     C     klist         BEGSR
027600060209     C*
027700060712     C     K07Epa02      KLIST
027800060712     C                   KFLD                    EpaCsi
027900060712     C                   KFLD                    EpaDva
028000060712     C                   KFLD                    EpaKcc
028100060712     C                   KFLD                    EpaKsc
028200060712     C                   KFLD                    EpaDrp
028300060712     C                   KFLD                    EpaNrp
028400060712     C                   KFLD                    EpaSerPar
028500060208     c*
028600060208     c                   ENDSR
028700941130     C************************************************************
028800941130     C* INIZIALIZZAZIONE VARIABILI
028900941130     C************************************************************
029000941130     C     INZVAR        BEGSR
029100031202     c*
029200060208     c* chiamata a xsoc
029300060208    >C                   MOVEL     'SOC001'      TIPXSC
029400060712     C                   MOVEL     L52Csi        SOCXSC
029500060208     C                   CALLb     'XSOC'
029600060208     C                   PARM                    TIPXSC            6
029700060208     C                   PARM                    SOCXSC            3
029800060208     C                   PARM                    CDSXSC            9 0
029900060208     C                   PARM                    MODXSC            3
030000060208     C                   PARM      *blanks       RTNXSC            1
030100060208     C                   PARM                    XSOCDS
030200060208     C                   PARM                    KPJBA
030300060208     C     RTNXSC        IFNE      '1'
030400060208    >C                   MOVEL     XSOCDS        SOC001
030500060208     C                   ELSE
030600060208     C                   seton                                        LR
030700060208     C                   return
030800060208     C                   ENDIF
030900060209     C*
031000060209     C                   ENDSR
031100060208     C************************************************************
031200060208     C* SQL
031300060208     C************************************************************
031400060208     C     InzSql        BEGSR
031500060208     c*
031600060712     C                   eval      SQ='Select NDPIW00F.*, RRN(NDPiw00F) +
031700060712     C                             From NDPIW00F'
031800060208     C*
031900060208     C     SQ            CAT       'WHERE':1     SQ
032000060712     c* SE promemoria
032100060712     c                   if        ProMemoria = '0'
032200060712     C* seleziono no promemoria
032300060712     C                   CAT       Nopromem:1    SQ
032400060712     C                   Cat       '''':0        Sq
032500060712     C                   Cat       'PM        ':0Sq
032600060209     C                   Cat       '''':0        Sq
032700060712     c                   else
032800060712     C* seleziono promemoria
032900060712     C                   CAT       Promem:1      SQ
033000060712     C                   Cat       '''':0        Sq
033100060712     C                   Cat       'PM        ':0Sq
033200060712     C                   Cat       '''':0        Sq
033300060712     c                   endif
033400060208     c*
033500060712     c                   if        L52Ord='1'
033600060208     C     SQ            CAT       order1:1      SQ
033700060208     c                   else
033800060208     C     SQ            CAT       order2:1      SQ
033900060208     c                   endif
034000060208     C     SQ            CAT       FETCH:1       SQ
034100060208     c*
034200060208     C                   ENDSR
034300060208     C/EJECT
034400060208     C************************************************************
034500060208     C* Imposta Ok sqlcod
034600060208     C************************************************************
034700060208     C     ImpSqlCod     BEGSR
034800060208     c*
034900060208     c                   select
035000060208     c* Errore sql eseguo cambio job per ottenere spool del joblog
035100060208     c                   when      sqlcod < 0
035200060208     c                   call      'X66CHGJOB'
035300060208     c                   eval      OkSqlCod = *off
035400060208     c* fine lettura
035500060208     c                   when      sqlcod = 100
035600060208     c                   eval      OkSqlCod = *off
035700060208     c* ok sql
035800060208     c                   other
035900060208     c                   eval      OkSqlCod = *on
036000060208     c*
036100060208     C                   endsl
036200060208     c*
036300060208     c                   endsr
036400060208     C/EJECT
036500060712     C*****************************************************************
036600060712     ONDEPA000  E            AGGEPA
036700060712     O                       EPAFLA
036800060712     O                       EPANUM
036900060712     O                       EPADAT
