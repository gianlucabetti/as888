000100990322     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200981019     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300050204      *--------------------------------------------------------------*
000400050204      * Questo programma serve per poter allineare FIFCE scritto in
000500050204      * filiale in modo errato ove non è possibile spalmare corretta-
000600050204      * mente le tariffe sulle voci di C/E i costi dovuti.
000700050204      * In tal modo si ottiene un allineamento fra FIFTT e FIFCE x non
000800050204      * creare scompensi in seguito sui saldi fra COSTI e COMPETENZE.
000900050204      * I casi da rettificare sono sporadici e non rappresentano la
001000050204      * normalità della procedura di conferma valorizzazioni Padroncini
001100050204      * comunque questo pgm serve per coprire le eccezioni non gestibili.
001200050204      *--------------------------------------------------------------*
001300050204     ffifce01l  uf   e           k disk
001400050204      *--------------------------------------------------------------*
001500020624     d kpjba         e ds
001600020624     D param           DS
001700020624     D  datadal                       8s 0
001800020624     D  dataal                        8s 0
001900050204      *
002000050204     D  sql_pdr        s                   like(fcepdr)
002100050204     D  sql_ddc        s                   like(fceddc)
002200050204     D  sql_tsr        s                   like(fcetsr)
002300050204     D  sql_fgs        s                   like(fcefgs)
002400050204     D  sql_ndc        s                   like(fcendc)
002500050204     D  sql_cce        s                   like(fcecce)
002600161111     D  sql_aas        s                   like(fceaas)
002700161111     D  sql_lnp        s                   like(fcelnp)
002800161111     D  sql_nrs        s                   like(fcenrs)
002900161111     D  sql_nsp        s                   like(fcensp)
003000050204     D  sql_ice        s                   like(fceice)
003100161111     D  sql_fceice     s                   like(fceice)
003200050204     D  sql_newice     s              9  3
003300050204     D  sql_difsum     s              9  3
003400050204     D  sql_fcesum     s              9  3
003500050204     D  sql_fttsum     s              9  3
003600050208      *
003700050208     D  sav_pdr        s                   like(fcepdr)
003800050208     D  sav_ddc        s                   like(fceddc)
003900050208     D  sav_ndc        s                   like(fcendc)
004000050208      *---------------------
004100050208     D SndMg01         S             10                                         Message type
004200050208     D                                     INZ('*INFO')
004300050208     D SndMg02         S             10                                         Deluvery mode
004400050208     D                                     INZ('*BREAK')
004500050208     D SndMg03         S            256                                         Message text
004600050208     D SndMg04         S             10I 0                                      Length of text
004700050208     D                                     INZ(%SIZE(SndMg03))
004800050208     D SndMg05         S             10                                         User profile
004900050208     D                                     DIM(299)
005000050208     D SndMg06         S             10I 0                                      Number of user
005100050208     D SndMg07         S             10I 0                                      Message sent indic.
005200050208     D SndMg08         S             10I 0                                      Function requested
005300050208     D SndMg10         S              1                                         Show display
005400050208     D                                     INZ('N')
005500050208     D SndMg11         S             20                                         Qualified MSGQ name
005600050208     D SndMg12         S              4                                         Name type indicator
005700050208     D                                     INZ('*USR')
005800050208      * indice di scihera
005900050208     D X               S              5I 0
006000050208      *
006100050208     D/COPY QSYSINC/QRPGLESRC,QUSEC
006200000000     C*---------------------------------------------------------------*
006300020624     c     *entry        plist
006400020624     c                   parm                    kpjba
006500020624     c                   movel     kpjbu         param
006600020313     C/EXEC SQL
006700050204     C+ Declare A1 Cursor for
006800160902     C+ with
006900160902     C+
007000161111     C+ A (fttpdr, fttddc, totFTT) AS (
007100161111     C+ SELECT fttpdr, fttddc, sum(fttitt + fttita + ftttpe + ftttbn +
007200161111     C+ ftttim + fttmnt) as TOTFTT  FROM fiftt00f
007300161111     C+ WHERE FTTTSR=' ' and fttftr = 'R' and  fttfvl= 'C' and fttddc
007400161111     C+ between :datadal and :dataal   GROUP BY fttpdr, fttddc),
007500160902     C+
007600161111     C+ B (fcepdr, fceddc, totFCE) AS (
007700161111     C+ SELECT fcepdr,fceddc, sum(fceice) as TOTFCE  FROM fifce00f
007800160902     C+ WHERE fceddc  between :datadal and :dataal
007900160902     C+ GROUP BY fcepdr, fceddc),
008000160902     C+
008100161111     C+ dif (fcepdr, fceddc, fceSUM, fttSUM, difSUM) as (
008200161111     C+ SELECT b.fcepdr, b.fceddc, decimal(b.totFCE, 9, 3) as FCESUM,
008300161111     C+ decimal(a.totFTT, 9, 3) as FTTSUM,
008400161111     C+ decimal(b.totFCE - a.totFTT , 9, 3) as difSUM
008500161111     C+ FROM a join b on a.fttpdr = b.fcepdr
008600161111     C+ and a.fttddc = b.fceddc
008700161111     C+ WHERE totFCE - totFTT > 1 or totFCE - totFTT < -1 )
008800160902     C+
008900161111     C+ SELECT * from (select
009000161111     C+  fce.FCEPDR, fce.FCEDDC, fce.FCETSR, fce.FCEFGS, fce.FCENDC,
009100161111     C+ fce.FCECCE,  fce.FCEAAS, fce.FCELNP, fce.FCENRS, fce.FCENSP,
009200161111     C+ difsum,  fce.fceice, case  when  fceSUM <> 0 then
009300161111     C+ decimal(fce.FCEICE * fttSUM/fceSUM, 9, 3) else 0 end as NEWICE
009400161111     C+ FROM dif join fifce00f as fce
009500161111     C+   ON dif.fcepdr = fce.fcepdr and dif.fceddc = fce.fceddc) as h
009600161111     C+ WHERE h.fceice <> newice
009700020313     C/END-EXEC
009800050204      *          apertura cursore
009900050204     C/EXEC SQL
010000050204     C+ OPEN A1
010100050204     C/END-EXEC
010200050204     C                   DOU       SqlCod <> 0
010300050204      *          lettura cursore
010400050204     C/EXEC SQL
010500161111     C+ Fetch Next From A1 Into
010600161111     C+ :sql_pdr, :sql_ddc, :sql_tsr, :sql_fgs, :sql_ndc, :sql_cce,
010700161111     C+ :sql_aas, :sql_lnp, :sql_nrs, :sql_nsp, :sql_difsum,
010800161111     C+ :sql_fceice, :sql_newice
010900050204     C/END-EXEC
011000050204     C                   SELECT
011100050204     **
011200050204     C                   WHEN      SqlCod = 100
011300050204     c                   leave
011400050204     c*
011500050204     C                   WHEN      SqlCod < 0
011600050204     C                   seton                                        H1
011700050204     c                   leave
011800050204     c*
011900050204     C                   OTHER
012000050204     **   aggiorna l'importo ricalcolato e riproporzionato
012100050204     c*     sulla riga nel NEWICE
012200050204     c                   exsr      AGG_FIFCE
012300050204     **
012400050204     C                   ENDSL
012500050204      *
012600050204     C                   ENDDO
012700050204      *
012800050204     C/EXEC SQL
012900050204     C+ Close A1
013000050204     C/END-EXEC
013100050204     C*
013200050204     C     FINE          TAG
013300020624     c                   seton                                        lr
013400011129     C*--------------------------------------------------------------------
013500050204     C*  Aggiorna il record su FIFCE con l'importo corretto/riproporzionato
013600050204     C*--------------------------------------------------------------------
013700050204     c     AGG_FIFCE     Begsr
013800050204     C*
013900050204     c     Kfce1         klist
014000050204     c                   kfld                    sql_fgs
014100050204     c                   kfld                    sql_pdr
014200050204     c                   kfld                    sql_tsr
014300050204     c                   kfld                    sql_ndc
014400050204     c                   kfld                    sql_ddc
014500050204     c                   kfld                    sql_aas
014600050204     c                   kfld                    sql_lnp
014700050204     c                   kfld                    sql_nrs
014800050204     c                   kfld                    sql_nsp
014900050204     c                   kfld                    sql_cce
015000050208     C*
015100050208     C*  Invia messaggio di controllo a rottura di documento disallineato
015200050209     c                   if        sql_ndc <> sav_ndc or
015300050209     c                             sql_ddc <> sav_ddc or
015400050208     c                             sql_pdr <> sav_pdr
015500050209     c                   eval      sav_ndc = sql_ndc
015600050209     c                   eval      sav_ddc = sql_ddc
015700050209     c                   eval      sav_pdr = sql_pdr
015800050214     c                   if        sql_difsum > 50 or sql_difsum < -50
015900050208     c                   z-add     1             x
016000050208     C                   exsr      CalQEZSNDMG
016100050208     c                   end
016200050214     c                   end
016300050204     C*
016400050204     c     Kfce1         chain     Fifce01l
016500050204     c                   if        %Found(Fifce01l)
016600050204     C* nuovo importo sulla riga
016700050204     C                   z-add     sql_newice    fceice
016800050204     c                   Update    fifce000
016900050204     c                   End
017000050204     C*
017100050204     c                   Endsr
017200050208     ***********************************************************************
017300050208     **
017400050208     ** Send Message (QEZSNDMG) API
017500050208     **
017600050208     ***********************************************************************
017700050208     C     CalQEZSNDMG   BEGSR
017800050208      *
017900050208     ** Invio messaggio all'utente.
018000050208     C                   EVAL      SndMg03 = 'COSTI/COMPETENZE: differenze in +
018100050209     C                              filiale fra FCE e FTT. Aut.:' +
018200050209     C                             %Trim(%editc(sql_pdr:'Z')) +  ' NDC:' +
018300050209     C                             %Trim(%editc(sql_ndc:'Z')) +  ' DDC:' +
018400160902     c                             %Trim(%editw(sql_ddc:'    /  /  '))
018500050208     C                   EVAL      SndMg05(x) = 'EDPAB'
018600050208     C                   EVAL      SndMg06 = x
018700050208     C                   CLEAR                   QUSEC
018800050208     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
018900050208
019000050208     C                   CALL      'QEZSNDMG'
019100050208     C                   PARM                    SndMg01
019200050208     C                   PARM                    SndMg02
019300050208     C                   PARM                    SndMg03
019400050208     C                   PARM                    SndMg04
019500050208     C                   PARM                    SndMg05
019600050208     C                   PARM                    SndMg06
019700050208     C                   PARM                    SndMg07
019800050208     C                   PARM                    SndMg08
019900050208     C                   PARM                    Qusec
020000050208     C                   PARM                    SndMg10
020100050208     C                   PARM                    SndMg11
020200050208     C                   PARM                    SndMg12
020300050208      *
020400050208     C                   ENDSR
020500050208      **********************************************************************
