000100001005     H DECEDIT('0,') DATEDIT(*DMY.)
000200021203      *------------------------------------------------------------------------*
000300021203      * DATA BASE
000400021203      *------------------------------------------------------------------------*
000500030314     FTNVRAB8D  cF   E             WORKSTN
000600021203     FFnevb01L  iF   E           K DISK
000700021203     FFnAr401L  iF   E           K DISK
000800021203     FFnCde00F  O  A E           K DISK
000900021203     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
001000021203      *------------------------------------------------------------------------*
001100021203     d conta           s              3  0
001200021203     d RIF_DPD         s             12
001300021203     d data_dcm        s                   like(EVBdev)
001400021203     d MIC_s_n         s              1
001500021203      *------------------------------------------------------------------------*
001600001005     D stringaSQL      s           5000    INZ(*blanks)
001700021203     D SqlDS         E DS                  EXTNAME(FnArb00F:FnArb000)
001800001005      *------------------------------------------------------------------------*
001900030314     c                   clear                   tnvrab8
002000030314      *
002100030314     c     video         tag
002200030314     c                   exfmt     tnvrab8
002300030314     c   KC              goto      fine
002400030314     c                   move      d8anno        anno4             4
002500030314     c                   move      d8dmg         dmg4              4
002600030314     c                   move      d8amg         amg4              4
002700030314     c     d8anno        mult      10000         adata             8 0
002800030314     c                   add       d8amg         adata
002900030314      *
003000030314     C                   eval      StringaSQL = 'Select * from FNARB00F where '+
003100030314     C                             ' ArbDCM = 0 and ArbLNA in (190, 191, 195) '+
003200030314     C                             ' and ArbAAS = ' + anno4 +
003300030314     C                             ' and ArbMGS between ' + dmg4 +
003400030314     C                             ' and ' + amg4
003500030314     c                   eval      sqlr1 = %subst(stringaSql:001:075)
003600030314     c                   eval      sqlr2 = %subst(stringaSql:076:150)
003700030314     c                   eval      sqlr3 = %subst(stringaSql:151:225)
003800030314     c                   eval      sqlr4 = %subst(stringaSql:226:300)
003900030314      *
004000030314     c  NKF              goto      video
004100030314      *
004200001005      * imposta la parte comune a tutti gli ordinamenti
004300030314      *****
004400030314     C*****              eval      StringaSQL = 'Select * from FNARB00F where '+
004500030314     C*****                        ' ArbDCM = 0 and ArbLNA in (190, 191, 195) '+
004600030314     C*****                        ' and ArbAAS = 2002 and ArbMGS between ' +
004700030314     C*****                        ' 0831 and 1128'
004800001005      *
004900001005     C/EXEC SQL
005000001005     C+ prepare S1 from :StringaSQL
005100001005     C/END-EXEC
005200001005      *
005300001005     C/EXEC SQL
005400001005     C+ declare C1 cursor for S1
005500001005     C/END-EXEC
005600001005      *
005700001005     C/EXEC SQL
005800001005     C+ open C1
005900001005     C/END-EXEC
006000021203     C                   DOU       SqlCod = 100 OR SqlCod < 0
006100001005     C/EXEC SQL
006200001005     C+ Fetch C1 into :SqlDS
006300001005     C/END-EXEC
006400021203     C                   SELECT
006500021203     C                   WHEN      SqlCod = 100
006600021203      *
006700021203     C                   WHEN      SqlCod >= 0
006800021203     C                   EXSR      EXESQL
006900021203      *
007000021203     C                   OTHER
007100021203     C                   EXSR      SqlError
007200021203     C                   ENDSL
007300021203      *
007400021203     C                   ENDDO
007500001005     C/EXEC SQL
007600001005     C+ close C1
007700001005     C/END-EXEC
007800030314     c     fine          tag
007900000204     C                   SETON                                        LR
008000000204     C*------------------------------------------------------------------------*
008100000204     C* EXESQL - ESECUZIONE COMANDI NAVIGAZIONALI
008200000204     C*------------------------------------------------------------------------*
008300000204     C     EXESQL        BEGSR
008400000204     C*
008500021203     c                   eval      MIC_s_n ='N'
008600021203     c                   eval      conta = 0
008700030318     c                   clear                   Last_eve          3
008800021203     C*
008900021203     C     kEVB          setll     fnevb01l
009000021203     C     kEVB          reade     fnevb01l
009100021203     C*
009200021203     c                   dow       not %eof(fnevb01l)
009300021203     C*
009400021203     c                   eval      conta = conta + 1
009500021203     C*
009600021203     c                   if        evbcev = 'MIC'
009700021203     c                   eval      MIC_s_n ='S'
009800021203     c                   eval      data_dcm = EVBDEV
009900021203     c                   end
010000030318     C*
010100030318     C* memorizza l'ultimo evento
010200030318     c                   movel     EvbCEV        Last_eve
010300021203     C*
010400021203     C     kEVB          reade     fnevb01l
010500021203     c                   enddo
010600021203     C*
010700030318     C*  Se l'ultimo evento è un MIC relativamente alla bolla
010800030318     C*   la bolla deve essere consegnata
010900030318     C*
011000021203     C* solo se presente un solo record su eventi ed è un MIC
011100021203     C*  può scrivere il FNCDE00F
011200021203     c                   if        conta = 1 and MIC_s_n = 'S'
011300030318     c                             and data_dcm <= adata
011400030314     c********                     and data_dcm < 20021129
011500030324     c                             or Last_eve='MIC'
011600021203      *
011700021203      * deve prendere il riferimento DPD
011800021203     c     kAR4          chain     FnAr401L
011900021203      *
012000021203     c                   if        %found(FnAr401L)
012100021203     c                   eval      RIF_DPD = %subst(AR4NOT:1:12)
012200021203     c                   endIf
012300021203     C*
012400021203     c                   clear                   fncde000
012500021203     c                   eval      CDEAAS = arbaas
012600021203     c                   eval      CDELNP = arblnp
012700021203     c                   eval      CDENRS = arbnrs
012800021203     c                   eval      CDENSP = arbnsp
012900021203     c                   eval      CDELNA = arblna
013000021203     c                   move      RIF_DPD       CDERMN
013100021203     c                   eval      CDEDCM = data_dcm
013200021203     c                   eval      CDEHMC = 1212
013300021203     c                   eval      CDESTS = '03C'
013400021203     c                   eval      CDEMOT = 'DELIVERY'
013500021203     C*
013600021203     c                   add       1             contabol          7 0
013700021203     c                   write     fncde000
013800021203     c   OF              EXCEPT    TESTA
013900021203     c                   EXCEPT    WRI_REC
014000021203     C*
014100021203     c                   endIf
014200021203     C*
014300000204     C                   ENDSR
014400990901     C*------------------------------------------------------------------------*
014500990901     C* *INZSR - OPERAZIONI INIZIALI
014600990901     C*------------------------------------------------------------------------*
014700990901     C     *INZSR        BEGSR
014800000204     C*
014900001005     C     kEVB          klist
015000021203     C                   kfld                    arbAAS
015100021203     C                   kfld                    arbLNP
015200021203     C                   kfld                    arbNRS
015300021203     C                   kfld                    arbNSP
015400021203     C*
015500021203     C     kAR4          klist
015600021203     C                   kfld                    arbAAS
015700021203     C                   kfld                    arbLNP
015800021203     C                   kfld                    arbNRS
015900021203     C                   kfld                    arbNSP
016000021203     C                   kfld                    AR4TRC
016100021203     C                   move      'F'           AR4TRC
016200001005     C*
016300021203     c                   z-add     0             contabol          7 0
016400990901     C                   ENDSR
016500021203     C*---------------------------------------------------------------*
016600021203     C* per emissione errore su SQL
016700021203     C*---------------------------------------------------------------*
016800021203     C     SqlError      BEGSR
016900021203      *
017000021203      * Forzo la stampa del JOBLOG.
017100021203      *   e chiude il pgm forzatamente
017200021203     C                   CALL      'X66CHGJOB'
017300021203     C                   EVAL      *INLR = *ON
017400021203     C                   RETURN
017500021203      *
017600021203     C                   ENDSR
017700021203     C*---------------------------------------------------------------*
017800021203     OQSYSPRT   E            TESTA            02
017900021203     O                                            1 ' '
018000021203     O                                         + 20 'Lista FNCDE records creati'
018100021203     O                                         +  1 'x chiudere Bolle aperte'
018200021203     O                                         +  1 'DPD'
018300021203     O          E            WRI_REC     1
018400021203     o                       contabol      z      7
018500021203     O                       CDEAAS        Z   +  3
018600021203     O                       CDELNP        Z   +  1
018700021203     O                       CDENRS        Z   +  1
018800021203     O                       CDENSP        Z   +  1
018900021203     O                       CDELNA            +  1
019000021203     O                       RIF_DPD           +  1
019100021203     O                       CDEDCM            +  2
019200021203     O                       CDEHMC            +  2
019300021203     O                       CDESTS            +  2
019400021203     O                       CDEMOT            +  2
