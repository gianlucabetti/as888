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
002600050204     D  sql_ice        s                   like(fceice)
002700050204     D  sql_newice     s              9  3
002800050204     D  sql_difsum     s              9  3
002900050204     D  sql_fcesum     s              9  3
003000050204     D  sql_fttsum     s              9  3
003100050204     D  sql_aas        s                   like(fceaas)
003200050204     D  sql_lnp        s                   like(fcelnp)
003300050204     D  sql_nrs        s                   like(fcenrs)
003400050204     D  sql_nsp        s                   like(fcensp)
003500050208      *
003600050208     D  sav_pdr        s                   like(fcepdr)
003700050208     D  sav_ddc        s                   like(fceddc)
003800050208     D  sav_ndc        s                   like(fcendc)
003900050208      *---------------------
004000050208     D SndMg01         S             10                                         Message type
004100050208     D                                     INZ('*INFO')
004200050208     D SndMg02         S             10                                         Deluvery mode
004300050208     D                                     INZ('*BREAK')
004400050208     D SndMg03         S            256                                         Message text
004500050208     D SndMg04         S             10I 0                                      Length of text
004600050208     D                                     INZ(%SIZE(SndMg03))
004700050208     D SndMg05         S             10                                         User profile
004800050208     D                                     DIM(299)
004900050208     D SndMg06         S             10I 0                                      Number of user
005000050208     D SndMg07         S             10I 0                                      Message sent indic.
005100050208     D SndMg08         S             10I 0                                      Function requested
005200050208     D SndMg10         S              1                                         Show display
005300050208     D                                     INZ('N')
005400050208     D SndMg11         S             20                                         Qualified MSGQ name
005500050208     D SndMg12         S              4                                         Name type indicator
005600050208     D                                     INZ('*USR')
005700050208      * indice di scihera
005800050208     D X               S              5I 0
005900050208      *
006000050208     D/COPY QSYSINC/QRPGLESRC,QUSEC
006100000000     C*---------------------------------------------------------------*
006200020624     c     *entry        plist
006300020624     c                   parm                    kpjba
006400020624     c                   movel     kpjbu         param
006500020313     C/EXEC SQL
006600050204     C+ Declare A1 Cursor for
006700050204     C+ select  FCEPDR, FCEDDC, FCETSR, FCEFGS, FCENDC, FCECCE, FCEICE,
006800050204     C+ NEWICE, DIFSUM, FCESUM, FTTSUM, FCEAAS, FCELNP, FCENRS, FCENSP
006900050317     C+ from (select new.*, case when fcesum <> 0 then
007000050317     C+ decimal((fceice * fttsum)/fcesum, 9, 3) else 0 end NEWICE
007100050204     C+ from (select * from fifce00f as FCE JOIN (select a.fcepdr PADRON,
007200050204     C+ a.fceddc DATDOC , decimal(a.totice, 9, 3) FCESUM, decimal(B.totsum,
007300050204     C+ 9, 3) FTTSUM, decimal(a.totice -b.totsum , 9, 3)DIFSUM from (select
007400050204     C+ fcepdr,fceddc, sum(fceice) totice from fifce00f where fceddc
007500050204     C+ between :datadal and :dataal  group by fcepdr, fceddc) as A JOIN
007600050204     C+ (SELECT fttpdr, fttddc, sum(fttitt + fttita + ftttpe + ftttbn +
007700060123     C+ ftttim + fttmnt) TOTSUM FROM fiftt00f WHERE FTTTSR=' ' and
007701060123     C+ fttftr = 'R' and fttfvl= 'C' and fttddc
007800050204     C+ between :datadal and :dataal  GROUP BY fttpdr, fttddc) as B ON
007900050204     C+ fttpdr=fcepdr and fttddc=fceddc WHERE (a.totice-b.totsum >1 or
008000050204     C+ a.totice -b.totsum<-1)) as DIF ON fce.fcepdr=PADRON and
008100050204     C+ fce.fceddc=DATDOC ) as NEW) as AGG WHERE fceice<> newice
008200050204     C+ FOR READ ONLY OPTIMIZE FOR 100 ROWS
008300020313     C/END-EXEC
008400050204      *          apertura cursore
008500050204     C/EXEC SQL
008600050204     C+ OPEN A1
008700050204     C/END-EXEC
008800050204     C                   DOU       SqlCod <> 0
008900050204      *          lettura cursore
009000050204     C/EXEC SQL
009100050204     C+ Fetch Next From A1 Into :sql_pdr, :sql_ddc, :sql_tsr, :sql_fgs,
009200050204     C+ :sql_ndc, :sql_cce, :sql_ice, :sql_newice, :sql_difsum, :sql_fcesum,
009300050204     C+ :sql_fttsum, :sql_aas, :sql_lnp, :sql_nrs, :sql_nsp
009400050204     C/END-EXEC
009500050204     C                   SELECT
009600050204     **
009700050204     C                   WHEN      SqlCod = 100
009800050204     c                   leave
009900050204     c*
010000050204     C                   WHEN      SqlCod < 0
010100050204     C                   seton                                        H1
010200050204     c                   leave
010300050204     c*
010400050204     C                   OTHER
010500050204     **   aggiorna l'importo ricalcolato e riproporzionato
010600050204     c*     sulla riga nel NEWICE
010700050204     c                   exsr      AGG_FIFCE
010800050204     **
010900050204     C                   ENDSL
011000050204      *
011100050204     C                   ENDDO
011200050204      *
011300050204     C/EXEC SQL
011400050204     C+ Close A1
011500050204     C/END-EXEC
011600050204     C*
011700050204     C     FINE          TAG
011800020624     c                   seton                                        lr
011900011129     C*--------------------------------------------------------------------
012000050204     C*  Aggiorna il record su FIFCE con l'importo corretto/riproporzionato
012100050204     C*--------------------------------------------------------------------
012200050204     c     AGG_FIFCE     Begsr
012300050204     C*
012400050204     c     Kfce1         klist
012500050204     c                   kfld                    sql_fgs
012600050204     c                   kfld                    sql_pdr
012700050204     c                   kfld                    sql_tsr
012800050204     c                   kfld                    sql_ndc
012900050204     c                   kfld                    sql_ddc
013000050204     c                   kfld                    sql_aas
013100050204     c                   kfld                    sql_lnp
013200050204     c                   kfld                    sql_nrs
013300050204     c                   kfld                    sql_nsp
013400050204     c                   kfld                    sql_cce
013500050208     C*
013600050208     C*  Invia messaggio di controllo a rottura di documento disallineato
013700050209     c                   if        sql_ndc <> sav_ndc or
013800050209     c                             sql_ddc <> sav_ddc or
013900050208     c                             sql_pdr <> sav_pdr
014000050209     c                   eval      sav_ndc = sql_ndc
014100050209     c                   eval      sav_ddc = sql_ddc
014200050209     c                   eval      sav_pdr = sql_pdr
014300050214     c                   if        sql_difsum > 50 or sql_difsum < -50
014400050208     c                   z-add     1             x
014500050208     C                   exsr      CalQEZSNDMG
014600050208     c                   end
014700050214     c                   end
014800050204     C*
014900050204     c     Kfce1         chain     Fifce01l
015000050204     c                   if        %Found(Fifce01l)
015100050204     C* nuovo importo sulla riga
015200050204     C                   z-add     sql_newice    fceice
015300050204     c                   Update    fifce000
015400050204     c                   End
015500050204     C*
015600050204     c                   Endsr
015700050208     ***********************************************************************
015800050208     **
015900050208     ** Send Message (QEZSNDMG) API
016000050208     **
016100050208     ***********************************************************************
016200050208     C     CalQEZSNDMG   BEGSR
016300050208      *
016400050208     ** Invio messaggio all'utente.
016500050208     C                   EVAL      SndMg03 = 'COSTI/COMPETENZE: differenze in +
016600050209     C                              filiale fra FCE e FTT. Aut.:' +
016700050209     C                             %Trim(%editc(sql_pdr:'Z')) +  ' NDC:' +
016800050209     C                             %Trim(%editc(sql_ndc:'Z')) +  ' DDC:' +
016900050208     c                             %Trim(%editw(sql_ddc:'    /  /  ')) +
017000050209     c                             ' Valore FCE:' +
017100050209     c                             %Trim(%editc(sql_fcesum:'K')) +
017200050209     c                             ' Valore FTT:' +
017300050208     c                             %Trim(%editc(sql_fttsum:'K'))
017400050208     C                   EVAL      SndMg05(x) = 'EDPAB'
017500050208     C                   EVAL      SndMg06 = x
017600050208     C                   CLEAR                   QUSEC
017700050208     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
017800050208
017900050208     C                   CALL      'QEZSNDMG'
018000050208     C                   PARM                    SndMg01
018100050208     C                   PARM                    SndMg02
018200050208     C                   PARM                    SndMg03
018300050208     C                   PARM                    SndMg04
018400050208     C                   PARM                    SndMg05
018500050208     C                   PARM                    SndMg06
018600050208     C                   PARM                    SndMg07
018700050208     C                   PARM                    SndMg08
018800050208     C                   PARM                    Qusec
018900050208     C                   PARM                    SndMg10
019000050208     C                   PARM                    SndMg11
019100050208     C                   PARM                    SndMg12
019200050208      *
019300050208     C                   ENDSR
019400050208      **********************************************************************
