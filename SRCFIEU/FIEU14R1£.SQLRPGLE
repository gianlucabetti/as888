000100040820     H DECEDIT('0,') DATEDIT(*dmy.)
000200020409     H**********************************************************************
000300180124     FTIDP301L  iF a E           k DISK    rename(TIDP3000:TIDP301)
000400180124     FTIDP302L  uF a E           k DISK    rename(TIDP3000:TIDP302)
000500180124     FFNEVB04L  iF   E           k DISK
000600040820     H**********************************************************************
000700040820     d KPJBA         E DS
000800120508      *
000900180124     D TIDP3DS       e DS                  extname(TIDP301L)
001000180124     D svDP3DS       e DS                  extname(TIDP301L)
001100180124     d                                     prefix(sv_)
001200180124      *
001300120508     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001400120508     D CNCR80        E DS
001500040820     D*
001600040820     D WLBDAT          DS
001700040820     D  G02DAT                 1      8  0
001800040820     D  G02INV                 9     16  0
001900040820     D  G02ERR                17     17
002000040820     D  G02TGI                18     22  0
002100040820      *
002200040820     D WrkSqlCmd       S           1024
002300040820     D  data_Oggi      s                   like(G02INV) INZ(0)
002400040826     D  almeno_uno     s              1
002500180123      *
002600180123     D BRT16           DS
002700180123     D  AAS16                  1      4  0
002800180123     D  LNP16                  5      7  0
002900180123     D  NRS16                  8      9  0
003000180123     D  NSP16                 10     16  0
003100180123      *
003200180123     d  Bolla16        s             16
003300180124     d  DP3Data        s             12s 0
003400180124     d  EVBData        s             12s 0
003500040820      *
003600040820     D digits          C                   CONST('0123456789')
003700040820      *****************************************************************
003800011210     C/EXEC SQL
003900011210     C+ PREPARE S1 FROM :WrkSqlCmd
004000011210     C/END-EXEC
004100011210
004200011210     C/EXEC SQL
004300011210     C+ DECLARE A1 CURSOR FOR S1
004400011210     C/END-EXEC
004500011210
004600011210     C/EXEC SQL
004700011210     C+ OPEN A1
004800011210     C/END-EXEC
004900040820
005000040820     C                   DOU       SqlCod <> 0
005100011205     C/EXEC SQL
005200180123     C+ FETCH NEXT FROM A1 INTO :Bolla16, :DP3Data, :EVBData
005300011205     C/END-EXEC
005400011210     C                   SELECT
005500041007     c*
005600041007     c* a fine file Totali x rotture
005700041007     C                   WHEN      SqlCod = 100
005800120508     c*
005900041007     c                   if        almeno_uno ='S'
006000041007     c                   end
006100120510     c*
006200041007     c                   leave
006300120508      *
006400120508     **  x qualsiasi errore
006500041007     C                   WHEN      SqlCod < 0
006600120510      *
006700041007     C                   seton                                        H1
006800041007     c                   goto      fine
006900040820     c*
007000041007     C                   OTHER
007100120510      *
007200120508     c                   move      'S'           almeno_uno
007300120508     c*
007400040820     c* DETTAGLIO
007500120508     c                   if        *in66
007600040823     c                   setoff                                       66
007700040825     c                   eval      prima_volta = 'N'
007800040823     c                   end
007900120510     c*
008000180123     c*  Esegue i ragionamenti di Dettaglio sulla riga letta con SQL
008100180123     c                   exsr      RIGA_SQL
008200011210     C                   ENDSL
008300040820
008400011205     C                   ENDDO
008500011205     C/EXEC SQL
008600011205     C+ CLOSE A1
008700011205     C/END-EXEC
008800011205     c*
008900040820     C* totali generali
009000041007     c                   if        almeno_uno ='S'
009100041007     c                   end
009200040823      *
009300041007     c     fine          tag
009400020326     C                   SETON                                        LR
009500040820     C**************************************************************************
009600040820     C* INIZIALIZZAZIONE
009700040820     C**************************************************************************
009800040820     C     *inzsr        BEGSR
009900040820      **
010000040820     C     *ENTRY        PLIST
010100040820     C                   PARM                    KPJBA
010200041103      *
010300041103     C                   Z-ADD     1             CODUT
010400041103     C                   CALL      'X§PARUT'
010500041103     C                   PARM                    UT§DSE
010600041103     C                   MOVEL     REC80         CNCR80
010700041103     C                   MOVEL     RAGUT         RSUT             20
010800041103     C*
010900040820      *
011000040820      *  imposta UDATE di elaborazione
011100040820     C                   move      *date         G02DAT
011200040820     C                   MOVE      *ZEROS        G02INV
011300040820     C                   MOVE      *BLANKS       G02ERR
011400040820     C                   CALL      'XSRDA8'
011500040820     C                   PARM                    WLBDAT
011600040820     C                   Z-ADD     G02INV        data_oggi
011700120510     C                   move      data_oggi     day               8 0
011800040820     C                   TIME                    W0140            14 0
011900120510     C                   MOVEl     W0140         ore               6 0
012000040823      **
012100040823     c                   seton                                        66
012200040825     c                   clear                   prima_volta       1
012300040826     c                   clear                   almeno_uno
012400040820     c*
012500040820     c* compongo istruzione sql
012600040820     c                   exsr      istruz
012700040820     c*
012800180124     c     KDP3_dat0     klist
012900180123     c                   kfld                    data_80           8 0
013000180123     c                   kfld                    aas16
013100180123     c                   kfld                    lnp16
013200180123     c                   kfld                    nrs16
013300180123     c                   kfld                    nsp16
013400180124     c*
013500180124     c     KDP3_bolla    klist
013600180124     c                   kfld                    aas16
013700180124     c                   kfld                    lnp16
013800180124     c                   kfld                    nrs16
013900180124     c                   kfld                    nsp16
014000180123     c*
014100180123     c     KEVB_MIC      klist
014200180123     c                   kfld                    aas16
014300180123     c                   kfld                    lnp16
014400180123     c                   kfld                    nrs16
014500180123     c                   kfld                    nsp16
014600180123     c                   kfld                    eveMIC            3
014700180123     c                   move      'MIC'         eveMIC
014800180123     c*
014900040820     C                   ENDSR
015000040820     C**************************************************************************
015100040820     C* mi compongo l'istruzione sql
015200040820     C**************************************************************************
015300040820     C     istruz        BEGSR                                                  *
015400040820      **
015500120508     c* MI COMPONGO L'ISTRUZIONE SQL che legge il file
015600120508      **
015700040820     C                   EVAL      WrkSqlCmd
015800180123     C                             = 'WITH   DP3ORM as (SELECT digits(dp3aas)  '
015900180123     c                             + 'concat digits(dp3lnp) concat digits(dp3nr'
016000180123     c                             + 's) concat digits(dp3nsp) as bollaDP3,    '
016100180123     c                             + 'MAX(dp3dev *10000 +dp3hev) as dataDP3    '
016200180123     c                             + 'FROM  tidp300f  WHERE  DP3CEV =''ORM''   '
016300180123     c                             + 'and DP3EVD =''03'' GROUP BY digits(dp3aas'
016400180123     c                             + ') concat digits(dp3lnp) concat digits(dp3'
016500180123     c                             + 'nrs) concat digits(dp3nsp)),             '
016600180123     c                             + 'EVBORM as ( SELECT digits(evbaas) concat '
016700180123     c                             + 'digits(evblnp) concat digits(evbnrs) conc'
016800180123     c                             + 'at digits(evbnsp) as bollaEVB,           '
016900180123     c                             + 'MAX(evbdev *10000 +evbhev) as dataEVB    '
017000180123     c                             + 'FROM  fnevb00f  WHERE  evbCEV =''MIC''   '
017100180123     c                             + 'and evbdev >20180101   GROUP BY          '
017200180123     c                             + 'digits(evbaas) concat digits(evblnp) conc'
017300180123     c                             + 'at digits(evbnrs) concat digits(evbnsp)) '
017400180123     c                             + 'SELECT bollaDP3, dataDP3, dataEVB        '
017500180123     c                             + 'FROM DP3ORM  join EVBORM on bollaDP3     '
017600180123     c                             + '= bollaEVB and dataDP3 <> dataEVB        '
017700040820      **
017800040820     C                   ENDSR
017900120510     C**************************************************************************
018000180124      *  ogni dettaglio letto
018100120510     C**************************************************************************
018200180123     C     RIGA_SQL      BEGSR
018300120510      *
018400180123      *  Se la Data del DP3 è (0) allora significa che NON è mai andato via nulla
018500180123      *    (Attenzione i MIC su EVB potrebbero essere più di uno - in tal caso
018600180123      *     il primo va aggiornato poi bisogna scrivere altre righe DP3 uguali)
018700180124      * in tal caso occorre scrivere nuove righe DP3
018800180124      *    per mandare via i MIC successivi
018900180124      *
019000120510     c                   exsr      AGGIORNA_DP3
019100180123      *
019200120510     C                   ENDSR
019300120510     C**************************************************************************
019400180123      *  Aggiorna sia il MIC dell'ORM con il primo MIC sugli eventi            )
019500120510     C**************************************************************************
019600120510     C     AGGIORNA_DP3  BEGSR
019700120510      *
019800180123     c                   move      bolla16       brt16
019900180123     c                   z-add     0             data_80           8 0
020000180124      * Solo sui "MIC"
020100180123     c     KEVB_MIC      setll     fnevb04l
020200180123     c     KEVB_MIC      reade     fnevb04l
020300180123     c                   dow       not %EoF(fnevb04l)
020400180124      *
020500180124      * controlla se aggiornato o no
020600180124     c                   move      'N'           stato_AGG         1
020700180124      *
020800180124     c                   if          dp3data = 0
020900180124     c                                  or
021000180124     c                               dp3data > 0 and
021100180124     c                               dp3data < evbdev*10000 + evbhev
021200180124      *
021300180124      *       se c'era il record del MIC già pronto con data = 0
021400180124     C     KDP3_dat0     setll     tiDP302L
021500180124     C     KDP3_dat0     reade     tiDP302L
021600180123     c                   dow       not %EoF(tiDP302L)
021700180123     c                   if        dp3cev ='ORM' and dp3evd ='03 '
021800180124     c                   move      'S'           stato_AGG
021900180124      *--  Salva la DS completa
022000180124      *--   (può servire per scrivere dopo se ci sono altri "MIC")
022100180124     c                   movel     tidp3ds       svDP3ds
022200180123     c                   z-add     EVBdev        dp3dev
022300180123     c                   z-add     EVBhev        dp3hev
022400180124     c                   z-add     EVBfle        dp3fle
022500180124     c                   update    tidp302
022600180123     c                   leave
022700180123     c                   end
022800180124     C     KDP3_dat0     reade     tiDP302L
022900180124     c                   enddo
023000120510      *
023100180124     c*******            If          Stato_AGG ='S'
023200180124     c*******            eval      tidp3ds = svDP3ds
023300180124     c*******            z-add     EVBdev        dp3dev
023400180124     c*******            z-add     EVBhev        dp3hev
023500180124     c*******            z-add     EVBfle        dp3fle
023600180124     c*******            write     tidp302
023700180124     c*******            end
023800180124      *---
023900180124      *--- se non è passato per il record con data evento = (0)
024000180124     c                   IF          Stato_AGG ='N'
024100180124      *---
024200180124     C     KDP3_bolla    setll     TIDP301L
024300180124     C     KDP3_bolla    reade     TIDP301L
024400180124     c                   dow       not %EoF(TIDP301L)
024500180124     c                   if        dp3cev ='ORM' and dp3evd ='03 '
024600180124     c                                and
024700180124     c                             dp3dev *10000+ dp3hev = DP3Data
024800180124     c                                and
024900180124     c                             dp3dev *10000+ dp3hev < EVBDev*10000+EVBHev
025000180124      * scrive una nuova riga ed esce
025100180124     c                   z-add     EVBdev        dp3dev
025200180124     c                   z-add     EVBhev        dp3hev
025300180124     c                   z-add     EVBfle        dp3fle
025400180124     c                   write     tidp301
025500180124     c                   leave
025600180124     c                   end
025700180124      *
025800180124     C     KDP3_bolla    reade     TIDP301L
025900180124     c                   enddo
026000180124      *---
026100180124     c                   endIF
026200180124     c                   endIF
026300180123      *
026400180123     c     KEVB_MIC      reade     fnevb04l
026500180124     c                   enddo
026600180123      *
026700120510     C                   ENDSR
026800180124     C**************************************************************************
