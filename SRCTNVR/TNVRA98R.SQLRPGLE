000100980127     H DECEDIT('0,') DATEDIT(*YMD.)
000200981106      *---------------------------------------------------------------------*
000300031106      *   Per COMPILARE il pgm :
000400031106      *    bisogna eseguire la CREATE TABLE di questo SQL per generare
000500031106      *    sulla libreria il WORK FILE che poi verrà scritto dal pgm stesso.
000600031106      *---------------------------------------------------------------------*
000700031106     Ffiapd01l  IF   E           K DISK
000800031105     FTITAS30C  IF   E           K DISK
000900031105     FEccet30C  IF   E           K DISK
001000031105     Ffiftd05l  IF   E           K DISK
001100031105     FFifcewrk  o  a E             DISK    rename(FIFCEwrk:FIFCEW)
001200031105     F                                     usropn
001300031105      *---------------------------------------------------------------------*
001400031106     D crt             S             70    DIM(21) CTDATA PERRCD(1)
001500031106     D txt             S             70    DIM(21) CTDATA PERRCD(1)
001600031106     D hdg             S             70    DIM(21) CTDATA PERRCD(1)
001700031106     D sql             S             70    DIM(5) CTDATA PERRCD(1)
001800031105      *---------------------------------------------------------------------*
001900031106     D stringaSQL      s           7000    INZ(*blanks)
002000031105     D SqlDS         E DS                  EXTNAME(FIFCE00F:FIFCE000)
002100980127      *---------------------------------------------------------------------*
002200980128      * DS
002300980128      *---------------------------------------------------------------------*
002400031031     D DateDS          DS
002500031031     D  Da8a                   1      8
002600031031     D  A8a                    9     16
002700980127      *  Ds per inversione data
002800961003     D WLBDA8          DS
002900980127     D  G08DAT                 1      8  0
003000980127     D  G08INV                 9     16  0
003100980127     D  G08ERR                17     17
003200980127     D  G08TGI                18     22  0
003300961003     D                SDS
003400961003     D  NOMPGM                 1     10
003500031031      *------------------------------------------------------------------------*
003600031105     IECCET000      10
003700031105     IECCETP00      20
003800031105     ITITAS000      01
003900031105     ITITAS010      02
004000031105     ITITASP00      03
004100981106      *---------------------------------------------------------------------*
004200980127      * Ciclo principale
004300980127      *---------------------------------------------------------------------*
004400031106      **     Crea la tabella in EDPSED come wrkfile risultante:
004500031105      **
004600031106      ** CREATE TABLE EDPSED/FIFCEWRK (
004700031106      ** FCEPDR NUMERIC (7, 0) NOT NULL WITH DEFAULT,
004800031106      ** FCENDC NUMERIC (7, 0) NOT NULL WITH DEFAULT,
004900031106      ** FCEDDC NUMERIC (8, 0) NOT NULL WITH DEFAULT,
005000031106      ** FCEAAS NUMERIC (4, 0) NOT NULL WITH DEFAULT,
005100031106      ** FCELNP NUMERIC (3, 0) NOT NULL WITH DEFAULT,
005200031106      ** FCENRS NUMERIC (2, 0) NOT NULL WITH DEFAULT,
005300031106      ** FCENSP NUMERIC (7, 0) NOT NULL WITH DEFAULT,
005400031106      ** FTDDCM NUMERIC (8, 0) NOT NULL WITH DEFAULT,
005500031106      ** FTDSIC CHARACTER (1) NOT NULL WITH DEFAULT,
005600031107      ** FTDCMC CHARACTER (3) NOT NULL WITH DEFAULT,
005700031106      ** CETARD NUMERIC (11, 3) NOT NULL WITH DEFAULT,
005800031106      ** APDPDD CHARACTER (1) NOT NULL WITH DEFAULT,
005900031106      ** TASMGS NUMERIC (4, 0) NOT NULL WITH DEFAULT,
006000031106      ** TASDCM NUMERIC (8, 0) NOT NULL WITH DEFAULT,
006100031106      ** TASDDC NUMERIC (8, 0) NOT NULL WITH DEFAULT,
006200031106      ** TASNDC NUMERIC (7, 0) NOT NULL WITH DEFAULT,
006300031106      ** TASPDC NUMERIC (7, 0) NOT NULL WITH DEFAULT,
006400031106      ** TASCCA CHARACTER (1) NOT NULL WITH DEFAULT,
006500031106      ** TASFBR CHARACTER (1) NOT NULL WITH DEFAULT)
006600031105     C                   clear                   StringaSQL
006700031106     C                   eval      StringaSQL = crt(1) + crt(2) + crt(3) +
006800031106     C                             crt(4) + crt(5) + crt(6) + crt(7) + crt(8) +
006900031106     C                             crt(9) + crt(10) + crt(11) + crt(12) +
007000031106     C                             crt(13) + crt(14) + crt(15) + crt(16) +
007100031106     C                             crt(17) + crt(18) + crt(19) + crt(20) +
007200031106     C                             crt(21)
007300031105     C/EXEC SQL
007400031105     C+ EXECUTE IMMEDIATE :StringaSQL
007500031105     C/END-EXEC
007600031106      **
007700031106      **   Dopo aver generato il file scrivo i TESTI dei campi
007800031106      **
007900031106      **  LABEL ON COLUMN EDPSED/FIFCEWRK (
008000031106      **  FCEPDR TEXT IS 'Padroncino',
008100031106      **  FCENDC TEXT IS 'Nr.Doc.FCE',
008200031106      **  FCEDDC TEXT IS 'Data Doc.FCE',
008300031106      **  FCEAAS TEXT IS 'Anno',
008400031106      **  FCELNP TEXT IS 'Lnp',
008500031106      **  FCENRS TEXT IS 'Nrs',
008600031106      **  FCENSP TEXT IS 'Nsp',
008700031106      **  FTDDCM TEXT IS 'Data Consegna FTD',
008800031106      **  FTDSIC TEXT IS 'Solo Incasso FTD',
008900031107      **  FTDCMC TEXT IS 'Mancata Cons. FTD',
009000031106      **  CETARD TEXT IS 'Competenze Arrivo ECCET',
009100031106      **  APDPDD TEXT IS 'Autista Fittizio',
009200031106      **  TASMGS TEXT IS 'Data Spedizione TAS',
009300031106      **  TASDCM TEXT IS 'Data Consegna TAS',
009400031106      **  TASDDC TEXT IS 'Data Doc TAS',
009500031106      **  TASNDC TEXT IS 'Num. Doc TAS',
009600031106      **  TASPDC TEXT IS 'Autista TAS',
009700031106      **  TASCCA TEXT IS 'Cod. Consegna Anomala',
009800031106      **  TASFBR TEXT IS 'Bolla Rientro')
009900031106      **
010000031106     C                   clear                   StringaSQL
010100031106     C                   eval      StringaSQL = txt(1) + txt(2) + txt(3) +
010200031106     C                             txt(4) + txt(5) + txt(6) + txt(7) + txt(8) +
010300031106     C                             txt(9) + txt(10) + txt(11) + txt(12) +
010400031106     C                             txt(13) + txt(14) + txt(15) + txt(16) +
010500031106     C                             txt(17) + txt(18) + txt(19) + txt(20) +
010600031106     C                             txt(21)
010700031106     C/EXEC SQL
010800031106     C+ EXECUTE IMMEDIATE :StringaSQL
010900031106     C/END-EXEC
011000031106      **
011100031106      **   Dopo aver generato il file scrivo i COLHDG dei campi
011200031106      **
011300031106      **  LABEL ON COLUMN EDPSED/FIFCEWRK (
011400031106      **  FCEPDR IS 'Autista',
011500031106      **  FCENDC IS 'Num.Doc.',
011600031106      **  FCEDDC IS 'Data Doc.',
011700031106      **  FCEAAS IS 'Anno',
011800031106      **  FCELNP IS 'Linea',
011900031106      **  FCENRS IS 'Serie',
012000031106      **  FCENSP IS 'Numero',
012100031106      **  FTDDCM IS 'Data Cons.',
012200031106      **  FTDSIC IS 'Incasso',
012300031107      **  FTDCMC IS 'Manc.Cns',
012400031106      **  CETARD IS 'Comp.Arrivo',
012500031106      **  APDPDD IS 'Pdr.Fitt.',
012600031106      **  TASMGS IS 'Data Sped.',
012700031106      **  TASDCM IS 'Data Cns.TAS',
012800031106      **  TASDDC IS 'Data Doc TAS',
012900031106      **  TASNDC IS 'Num. Doc TAS',
013000031106      **  TASPDC IS 'Autista TAS',
013100031106      **  TASCCA IS 'Cns.Anomala',
013200031106      **  TASFBR IS 'Bolla Rientro')
013300031106      **
013400031106     C                   clear                   StringaSQL
013500031106     C                   eval      StringaSQL = hdg(1) + hdg(2) + hdg(3) +
013600031106     C                             hdg(4) + hdg(5) + hdg(6) + hdg(7) + hdg(8) +
013700031106     C                             hdg(9) + hdg(10) + hdg(11) + hdg(12) +
013800031106     C                             hdg(13) + hdg(14) + hdg(15) + hdg(16) +
013900031106     C                             hdg(17) + hdg(18) + hdg(19) + hdg(20) +
014000031106     C                             hdg(21)
014100031106     C/EXEC SQL
014200031106     C+ EXECUTE IMMEDIATE :StringaSQL
014300031106     C/END-EXEC
014400031106      **
014500031105      *
014600031105      * apre il WRKFILE x scriverlo
014700031105     C                   open      FIFCEWRK
014800031105      *
014900031105      ** eseguito interattivamente:
015000031105      **  trova quelli messi in consegna + volte
015100031105      *
015200031106      **  SELECT
015300031106      **  FCEAAS, FCELNP, FCENRS, FCENSP, fceddc, fcendc, fcepdr
015400031106      **  FROM FIFCE00F      WHERE fcetsr='C'
015500031106      **  GROUP BY FCEAAS, FCELNP, FCENRS, FCENSP, fceddc, fcendc, fcepdr
015600031106      **  ORDER BY FCEAAS, FCELNP, FCENRS, FCENSP, fceddc, fcendc, fcepdr
015700031105      *
015800031105     C                   clear                   StringaSQL
015900031106     C                   eval      StringaSQL = sql(1) + sql(2) +
016000031106     C                             sql(3) + sql(4) + sql(5)
016100031105     C/EXEC SQL
016200031105     C+ prepare S1 from :StringaSQL
016300031105     C/END-EXEC
016400031105      *
016500031105     C/EXEC SQL
016600031105     C+ declare C1 cursor for S1
016700031105     C/END-EXEC
016800031105      *
016900031105     C/EXEC SQL
017000031105     C+ open C1
017100031105     C/END-EXEC
017200031105      *
017300031105     C/EXEC SQL
017400031106     C+ FETCH NEXT FROM C1 INTO :FCEAAS, :FCELNP, :FCENRS, :FCENSP,
017500031106     C+ :fceddc, :fcendc, :fcepdr
017600031105     C/END-EXEC
017700031105      *
017800031105     C                   dow       sqlcod=*zeros
017900031105      *
018000031105     C* ESECUZIONE COMANDI NAVIGAZIONALI
018100031105     C                   EXSR      Elabora
018200031105      *
018300031105     c                   write     fifcew
018400031105      *
018500031105     C/EXEC SQL
018600031106     C+ FETCH NEXT FROM C1 INTO :FCEAAS, :FCELNP, :FCENRS, :FCENSP,
018700031106     C+ :fceddc, :fcendc, :fcepdr
018800031105     C/END-EXEC
018900031105     C                   ENDDO
019000031105      *
019100031105     C/EXEC SQL
019200031105     C+ close C1
019300031105     C/END-EXEC
019400031105     C*
019500031105     C                   SETON                                        LR
019600980127      *----------------------------------------------------*
019700031031      *
019800980127      *----------------------------------------------------*
019900031031     C     elabora       BEGSR
020000031105      *
020100031106      * prende dati di FTD se c'è
020200031106      * prende da valorizzazioni di filiale
020300031105     C     kftd          chain     fiftd05l
020400031105     c                   if        not %found(fiftd05l)
020500031106     c                   move      *all'9'       ftddcm
020600031106     c                   move      *all'9'       ftdsic
020700031107     c                   move      *all'9'       ftdcmc
020800031106     c                   end
020900031106      *
021000031106      * prende dati di APD se c'è
021100031106      * prende da anagrafica di sede
021200031106     C     kapd          chain     fiapd01l
021300031106     c                   if        not %found(fiapd01l)
021400031106     c                   move      *all'9'       apdpdd
021500031106     c                   end
021600031106      *
021700031106      * prende dati di TAS se c'è
021800031106      * prende da tassazione di sede
021900031105     C     kspe          chain     titas30C
022000031105     c                   if        not %found(titas30C)
022100031106     c                   move      *all'9'       tasmgs
022200031106     c                   move      *all'9'       tasdcm
022300031106     c                   move      *all'9'       tasddc
022400031106     c                   move      *all'9'       tasndc
022500031106     c                   move      *all'9'       taspdc
022600031106     c                   move      *all'9'       tascca
022700031106     c                   move      *all'9'       tasfbr
022800031106     c                   end
022900031105      *
023000031106      * prende dati di CET se c'è
023100031105      * prende la competenza di arrivo
023200031105     C     kspe          chain     eccet30C
023300031105     c                   if        not %found(eccet30C)
023400031106     c                   move      *all'9'       cetard
023500031105     c                   end
023600031031      *
023700031031     C                   ENDSR
023800031031      *----------------------------------------------------*
023900031031      *   *INZSR: Operazioni iniziali                      *
024000031031      *----------------------------------------------------*
024100031031     C     *INZSR        BEGSR
024200031105      *
024300031105     c     *like         define    fceaas        savaas
024400031105     c     *like         define    fcelnp        savlnp
024500031105     c     *like         define    fcenrs        savnrs
024600031105     c     *like         define    fcensp        savnsp
024700031031      *
024800031105     C     kftd          klist
024900031105     C                   kfld                    fcePDR
025000031106     C                   kfld                    tipTSR
025100031105     C                   kfld                    fceNDC
025200031105     C                   kfld                    fceDDC
025300031105     C                   kfld                    fceAAS
025400031105     C                   kfld                    fceLNP
025500031105     C                   kfld                    fceNRS
025600031105     C                   kfld                    fceNSP
025700031106      *
025800031106     C                   move      'C'           tipTSR            1
025900031105      *
026000031105     C     kspe          klist
026100031105     C                   kfld                    fceAAS
026200031105     C                   kfld                    fceLNP
026300031105     C                   kfld                    fceNRS
026400031105     C                   kfld                    fceNSP
026500031105      *
026600031105     C     kapd          klist
026700031105     C                   kfld                    tipo
026800031105     C                   kfld                    fcePDR
026900031105     C                   move      'A'           tipo              1
027000031031      *
027100031106     c                   z-add     0             conta             5 0
027200031106      *
027300031031     C                   ENDSR
027400031031      *----------------------------------------------------*
027500031106**
027600031106CREATE TABLE EDPSED/FIFCEWRK (
027700031106FCEPDR NUMERIC (7, 0) NOT NULL WITH DEFAULT,
027800031106FCENDC NUMERIC (7, 0) NOT NULL WITH DEFAULT,
027900031106FCEDDC NUMERIC (8, 0) NOT NULL WITH DEFAULT,
028000031106FCEAAS NUMERIC (4, 0) NOT NULL WITH DEFAULT,
028100031106FCELNP NUMERIC (3, 0) NOT NULL WITH DEFAULT,
028200031106FCENRS NUMERIC (2, 0) NOT NULL WITH DEFAULT,
028300031106FCENSP NUMERIC (7, 0) NOT NULL WITH DEFAULT,
028400031106FTDDCM NUMERIC (8, 0) NOT NULL WITH DEFAULT,
028500031106FTDSIC CHARACTER (1) NOT NULL WITH DEFAULT,
028600031107FTDCMC CHARACTER (3) NOT NULL WITH DEFAULT,
028700031106CETARD NUMERIC (11, 3) NOT NULL WITH DEFAULT,
028800031106APDPDD CHARACTER (1) NOT NULL WITH DEFAULT,
028900031106TASMGS NUMERIC (4, 0) NOT NULL WITH DEFAULT,
029000031106TASDCM NUMERIC (8, 0) NOT NULL WITH DEFAULT,
029100031106TASDDC NUMERIC (8, 0) NOT NULL WITH DEFAULT,
029200031106TASNDC NUMERIC (7, 0) NOT NULL WITH DEFAULT,
029300031106TASPDC NUMERIC (7, 0) NOT NULL WITH DEFAULT,
029400031106TASCCA CHARACTER (1) NOT NULL WITH DEFAULT,
029500031106TASFBR CHARACTER (1) NOT NULL WITH DEFAULT)
029600031106**
029700031106LABEL ON COLUMN EDPSED/FIFCEWRK (
029800031106FCEPDR TEXT IS 'Padroncino',
029900031106FCENDC TEXT IS 'Nr.Doc.FCE',
030000031106FCEDDC TEXT IS 'Data Doc.FCE',
030100031106FCEAAS TEXT IS 'Anno',
030200031106FCELNP TEXT IS 'Lnp',
030300031106FCENRS TEXT IS 'Nrs',
030400031106FCENSP TEXT IS 'Nsp',
030500031106FTDDCM TEXT IS 'Data Consegna FTD',
030600031106FTDSIC TEXT IS 'Solo Incasso FTD',
030700031107FTDCMC TEXT IS 'Mancata Cons. FTD',
030800031106CETARD TEXT IS 'Competenze Arrivo ECCET',
030900031106APDPDD TEXT IS 'Autista Fittizio',
031000031106TASMGS TEXT IS 'Data Spedizione TAS',
031100031106TASDCM TEXT IS 'Data Consegna TAS',
031200031106TASDDC TEXT IS 'Data Doc TAS',
031300031106TASNDC TEXT IS 'Num. Doc TAS',
031400031106TASPDC TEXT IS 'Autista TAS',
031500031106TASCCA TEXT IS 'Cod. Consegna Anomala',
031600031106TASFBR TEXT IS 'Bolla Rientro')
031700031106**
031800031106LABEL ON COLUMN EDPSED/FIFCEWRK (
031900031106FCEPDR IS 'Autista',
032000031106FCENDC IS 'Num.Doc.',
032100031106FCEDDC IS 'Data Doc.',
032200031106FCEAAS IS 'Anno',
032300031106FCELNP IS 'Linea',
032400031106FCENRS IS 'Serie',
032500031106FCENSP IS 'Numero',
032600031106FTDDCM IS 'Data Cons.',
032700031106FTDSIC IS 'Incasso',
032800031107FTDCMC IS 'Manc.Cns',
032900031106CETARD IS 'Comp.Arrivo',
033000031106APDPDD IS 'Pdr.Fitt.',
033100031106TASMGS IS 'Data Sped.',
033200031106TASDCM IS 'Data Cns.TAS',
033300031106TASDDC IS 'Data Doc TAS',
033400031106TASNDC IS 'Num. Doc TAS',
033500031106TASPDC IS 'Autista TAS',
033600031106TASCCA IS 'Cns.Anomala',
033700031106TASFBR IS 'Bolla Rientro')
033800031106**
033900031106SELECT
034000031106FCEAAS, FCELNP, FCENRS, FCENSP, fceddc, fcendc, fcepdr
034100031106FROM FIFCE00F      WHERE fcetsr='C'
034200031106GROUP BY FCEAAS, FCELNP, FCENRS, FCENSP, fceddc, fcendc, fcepdr
034300031106ORDER BY FCEAAS, FCELNP, FCENRS, FCENSP, fceddc, fcendc, fcepdr
