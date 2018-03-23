000100950524     H DECEDIT('0,') DATEDIT(*DMY.)
000200070205     h dftactgrp(*no) actgrp(*caller)
000300070322     H* FIDG18R *----------------------------------------------------*
000400070322     H*         assegnazione automatica distinte consegna
000500070322     H*--------------------------------------------------------------*
000600070222     FFidg18p   O    E             PRINTER OFLIND(*IN80)
000700070228     FAZORG01L  IF   E           K DISK
000701080326     Ffidsf02l  IF   E           K DISK
000800950524      *--------------------------------------------------------------*
000900070222     ddtaiso           s               d
001000070222     ddtaeur           s               d   datfmt(*Eur)
001100070202     D fidstds       e ds                  extname(fidst00f)
001200050520     D ddatiute      e ds
001300030910     D azuteds       e ds                  extname(AZUTE00F)
001400030910     D tibs34ds      E DS                  inz
001500950524     D KPJBA         E DS
001600070205     D Fidg17ds      E DS
001700070206     D Fidg19ds      E DS
001800070205     D Fidg20ds      E DS
001900070316     D Fidg23ds      E DS
002000070216     D Fidg26ds      E DS
002100070228     D OG146         E DS                  INZ
002101071107     D OG147         E DS                  INZ
002102071107     D OG150         E DS                  INZ
002200950524     C*--------------------------------------------------------------*
002300950524     C*  Ciclo principale
002400950524     C*--------------------------------------------------------------*
002500070411     c* pulisco i riferimenti di ARG per le spedizioni non in distinta
002600070830     c* (ARBNDC=0) solo se p.o. partito
002602070830     c                   if        dateu8 >= comdda
002700070411     c                   clear                   fidg20ds
002800070411     c                   eval      d20cmti= 'S'
002801071107     c                   eval      d20cleari=d17cleari
002802071210     c                   eval      d20ldcri=d17ldcri
002803071107     c                   eval      d20dtai=d17dtai
002804070827     c                   eval      d20orai=d17orai
002805150430     c                   eval      d20p88i=d17p88i
002900070412     c                   eval      d20fgsi= d17fgsi
003000070411     C                   EVAL      D20PAG = PAGES
003100070411     C                   EVAL      D20RIGA= RIGA
003200070411     c                   eval      kpjbu = fidg20ds
003300070411     c                   call      'FIDG29R'
003400070411     c                   parm                    kpjba
003500070411     c                   eval      fidg20ds = kpjbu
003600070411     c                   eval      pages = d20pag
003700070411     c                   eval      riga = d20riga
003800070411     c                   if        d20erro<> ' '
003900070411     c                   exsr      srfine
004000070411     c                   end
004001070830     c                   end
004100070205      *  leggo in Fidst02l ordinato per distinta ed alloco tutti i record
004200070322      * da elaborare perchè sono in commit *all (per ora non è vero)
004300061003     C/EXEC SQL
004400070205     C+ DECLARE B1 CURSOR FOR SELECT fidst02l.* FROM fidst02l WHERE
004500070827     C+ dstfgs =:d17fgsi and dstnpg=4 and dstdfv = :d17dfvi and dstatb =
004600070827     C+ ' ' and dstnfv between :d17nfdi and :d17nfai and dstfcf = ' ' and
004601070827     C+ dstfasd = ' '
004700061003     C/END-EXEC
004800061003
004900061003     C/EXEC SQL
005000061003     C+ OPEN b1
005100061003     C/END-EXEC
005200061003
005300061003     C                   do        *hival
005400061003
005500061003     C/EXEC SQL
005600070205     C+ FETCH NEXT FROM b1 INTO :fidstds
005700061003     C/END-EXEC
005800061003
005900061003     C                   select
006000061003     C                   when      sqlcod = 100
006100061003     c                   leave
006200070205     C                   WHEN      SqlCod < 0
006300070205     c* record allocato
006400070205     c                   if        sqlcod <> -913
006500070205     C                   seton                                        H1
006600070205     c                   end
006700070222     c                   seton                                        81
006800070222     c                   exsr      srstampa
006900070206     c                   exsr      srfine
007000070205     c                   other
007100080326     c* verifico che non esista già una fase
007101080326     c                   eval      dsftrd = 'STP'
007102080326     c     kdsf          setll     fidsf02l                               42
007103080326     c                   if        not *in42 and (dstpda='E' or dstpda='C')
007104080326     c                   eval      dsftrd = 'PDC'
007105080326     c     kdsf          setll     fidsf02l                               42
007106080326     c                   end
007107080326     c                   if        *in42
007108080326     c                   iter
007109080326     c                   end
007401080326     c* mattina/pomeriggio
007402080326     c                   if        d17fppi <> ' ' and dstfpp <> d17fppi
007403080326     c                   iter
007404080326     c                   end
007500070302     c* parcel/messaggerie
007600070302     c                   if        (d17tpmi= 'P' and dsttpm <>' ') or
007700070302     c                             (d17tpmi= 'M' and dsttpm <>'M')
007800070302     c                   iter
007900070302     c                   end
008000070410     c* se devo pulire i riferimenti della distinta chiamo il pgm FIDG20R
008100070627     c                   if        d17cleari= 'S' and dstfasd = ' '
008200070205     c                   clear                   fidg20ds
008300070208     c                   eval      d20cmti= 'S'
008400071107     c                   eval      d20cleari=d17cleari
008401071107     c                   eval      d20clrman=d17clrman
008402071210     c                   eval      d20ldcri=d17ldcri
008403071210     c                   eval      d20dtai=d17dtai
008404070824     c                   eval      d20orai=d17orai
008405150430     c                   eval      d20p88i=d17p88i
008500070313     c                   eval      d20nfvi= dstnfv
008600070206     c                   eval      d20dfvi= dstdfv
008700070206     c                   eval      d20fgsi= dstfgs
008800070206     c                   eval      d20fppi= dstfpp
008900070206     c                   eval      d20tpmi= dsttpm
009000070323     C                   EVAL      D20PAG = PAGES
009100070323     C                   EVAL      D20RIGA= RIGA
009200070205     c                   eval      kpjbu = fidg20ds
009300070205     c                   call      'FIDG20R'
009400070205     c                   parm                    kpjba
009500070215     c                   eval      fidg20ds = kpjbu
009600070326     c                   eval      pages = d20pag
009700070326     c                   eval      riga = d20riga
009800070206     c                   if        d20erro<> ' '
009900070205     c                   exsr      srfine
010000070205     c                   end
010100070205     c                   end
010200061003     C                   endsl
010300061003     C*
010400061003     C                   enddo
010500061003
010600061003     C/EXEC SQL
010700061003     C+ CLOSE b1
010800061003     C/END-EXEC
010900070410     c* se esco senza errore 913 vuol dire che tutti i record
011000070206     c* delle distinte che mi interessano sono allocati e posso procedere
011100070411     c* (per ora non è vero sono in CMT *CHG)
011200070206     c                   clear                   fidg19ds
011300070208     c                   eval      d19cmti= 'S'
011400070206     C                   eval      d19FGSi = d17FGSi
011500070206     C                   eval      d19Dfvi = d17Dfvi
011600070206     C                   eval      d19NfDi = d17NfDi
011700070206     C                   eval      d19NfAi = d17NfAi
011800070206     C                   eval      d19reti = d17reti
011900070206     C                   eval      d19cleari= d17cleari
012000070206     C                   eval      d19FPpi = d17fppi
012100070206     C                   eval      d19tpmi = d17tpmi
012101071210     C                   eval      d19ldcri = d17ldcri
012102070824     C                   eval      d19dtai = d17dtai
012103070824     C                   eval      d19orai = d17orai
012105150430     C                   eval      d19p88i = d17p88i
012200070323     C                   EVAL      D19PAG = PAGES
012300070323     C                   EVAL      D19RIGA= RIGA
012400070206     c                   eval      kpjbu = fidg19ds
012500070205     c                   call      'FIDG19R'
012600070205     c                   parm                    kpjba
012700070215     c                   eval      fidg19ds = kpjbu
012800070326     c                   eval      pages = d19pag
012900070326     c                   eval      riga = d19riga
013000070205     c* se ritorno senza codice d'errore commit tutto
013100070206     c                   if        d19erro= ' '
013200070206     c                   if        d17cmti= 'S'
013300070205     c                   commit
013400070206     c                   end
013500070301     c* aggiorno ARB solo se p.o. partito
013600070313     c                   if        dateu8 >= comdda
013700070316     c                   clear                   fidg23ds
013800070316     c                   eval      d23cmti= 'S'
013900070316     C                   eval      d23FGSi = d17FGSi
014000070316     C                   eval      d23Dfvi = d17Dfvi
014100070316     C                   eval      d23NfDi = d17NfDi
014200070316     C                   eval      d23NfAi = d17NfAi
014300070316     C                   eval      d23FPpi = d17fppi
014400070316     C                   eval      d23tpmi = d17tpmi
014401071210     C                   eval      d23ldcri = d17ldcri
014402071210     C                   eval      d23dtai = d17dtai
014403071107     C                   eval      d23orai = d17orai
014404150430     C                   eval      d23p88i = d17p88i
014405071107     C                   eval      d23cleari= d17cleari
014500070323     C                   EVAL      D23PAG = PAGES
014600070323     C                   EVAL      D23RIGA= RIGA
014700070316     c                   eval      kpjbu = fidg23ds
014800070313     c                   call      'FIDG23C'
014900070313     c                   parm                    kpjba
015000070316     c                   eval      fidg23ds = kpjbu
015100070326     c                   eval      pages = d23pag
015200070326     c                   eval      riga = d23riga
015300070316     c                   if        d23erro<>' '
015400070313     c                   exsr      srfine
015500070313     c                   end
015600070313     c                   end
015700070327     c* aggiorno FIDST con valori assegnato
015800070327     c                   eval      kpjbu = fidg19ds
015900070327     c                   call      'FIDG24R'
016000070327     c                   parm                    kpjba
016100070327     c                   eval      fidg19ds = kpjbu
016200070327     c                   eval      pages = d19pag
016300070327     c                   eval      riga = d19riga
016400070327     c                   if        d19erro<>' '
016500070327     c                   exsr      srfine
016600070327     c                   end
016700070228     c* lancio la stampa
016800070313     c                   clear                   fidg26ds
016900070313     C                   eval      d26fil = d17fgsi
017000070313     C                   eval      d26dat = d17dfvi
017001070830     C                   eval      d26fppi= d17fppi
017002071211     C                   eval      D26DTDCR =d17ldcri
017003071210     C                   eval      d26dtai= d17dtai
017004150728     C                   eval      d26orai= d17orai
017005150728     C                   eval      d26p88i= d17p88i
017100070619     C                   eval      D26spea= 'S'
017200070619     C                   eval      D26noas= 'S'
017201100208     C                   eval      D26ATTE= 'N'
017300070619     C                   eval      d26pame= d17tpmi
017400070313     c                   eval      kpjbu = fidg26ds
017500070313     c                   call      'FIDG26R'
017600070313     c                   parm                    kpjba
017700070205     c                   else
017800070205     c                   exsr      srfine
017900070205     c                   end
018000950525     C*
018100070206     c                   eval      kpjbu = fidg17ds
018200070223     c                   write     finest
018300950524     C                   SETON                                        LR
018400070205     C**************************************************************************
018500070205     C* esco con codice d'errore
018600070205     C**************************************************************************
018700070205     C     srfine        BEGSR
018800070205     c*
018900070206     c                   if        d17cmti= 'S'
019000070205     c                   rolbk
019100070206     c                   end
019200070205     c*
019300070223     c                   write     finest
019400070206     c                   eval      d17erro= '1'
019500070206     c                   eval      kpjbu = fidg17ds
019600070205     c                   seton                                        lr
019700070205     c                   return
019800070205     c*
019900070205     c                   endsr
020000070222     C*------------------------------------------------------------------------*
020100070222     C* stampa
020200070222     C*------------------------------------------------------------------------*
020300070222     C     srstampa      BEGSR
020301071107     c* stampo solo per assegnaz. automatica
020302071107     c                   if        kcdaz= 'DG17' or kcdaz='DG18'
020400070222     c                   move      d17dfvi       dtaiso
020500070222     c                   move      dtaiso        dtaeur
020600070222     c                   move      dtaeur        prtdfv
020601071107     c* decodifico parametri di lancio
020602071210     c                   if        d17ldcri <> 0
020603071210     c                   move      d17ldcri      dtaiso
020604071210     c                   move      dtaiso        dtaeur
020605071210     c                   move      dtaeur        decldcr
020606071210     c                   else
020607071210     c                   clear                   decldcr
020608071210     c                   end
020609071107     c                   if        *in71
020610071107     c                   move      d17dtai       dtaiso
020611071107     c                   move      dtaiso        dtaeur
020612071107     c                   move      dtaeur        decdta
020613071107     c                   eval      decora = d17orai
020614071107     c                   end
020615150505     c                   if        d17p88i= 'N'
020616150515     c                   eval      decp88r='Spediz. pistola 88 NO'
020617150430     c                   else
020618150515     c                   clear                   decp88r
020620150430     c                   end
020621071107     c                   if        d17cleari = 'S'
020622071107     c                   eval      decclear='SI'
020623071107     c                   else
020624071107     c                   eval      decclear='NO'
020625071107     c                   end
020626071107     c                   if        *in70
020627071107     c                   select
020628071107     c                   when      d17tpmi ='M'
020629071107     c                   eval      dectpm ='MESSAGGERIE'
020630071107     c                   when      d17tpmi ='P'
020631071107     c                   eval      dectpm ='PARCEL'
020632071107     c                   when      d17tpmi =' '
020633071107     c                   eval      dectpm ='TUTTE'
020634071107     c                   endsl
020635071107     c                   end
020636071107     c                   select
020637071107     c                   when      d17fppi ='M'
020638071107     c                   eval      decfpp ='MAT'
020639071107     c                   when      d17fppi ='P'
020640071107     c                   eval      decfpp ='POM'
020641071107     c                   when      d17fppi =' '
020642071107     c                   eval      decfpp ='TUTTE'
020643071107     c                   endsl
020700070323     C* PAGINA IN STAMPA
020800070323     C                   IF        *IN80 OR RIGA >=60
020900070323     C                   SETON                                        80
021000070323     C                   ADD       1             PAGES
021100071107     C                   Z-ADD     5             RIGA              3 0
021200070323     C                   END
021300070222     c*
021400070222     c   80              write     testa
021402071107     c   80              write     testap
021500070222     c                   setoff                                       80
021600070222     c   81              write     riga1
021700070323     C   81              ADD       1             RIGA
021800070222     c                   setoff                                       81
021801071107     c                   end
021900070222     c*
022000070222     c                   endsr
022100950525     C*--------------------------------------------------------------*
022200950525     C*  Operazioni iniziali
022300950525     C*--------------------------------------------------------------*
022400950525     C     *INZSR        BEGSR
022500950525     C*
022600950525     C     *ENTRY        PLIST
022700950525     C                   PARM                    KPJBA
022800070202     C                   MOVEL(P)  KPJBU         Fidg17ds
022900070206     c                   clear                   d17erro
023000030718     c*
023100030718     c     *dtaara       define    §azute        azuteds
023200030718     c     *dtaara       define    §datiute      ddatiute
023300030718     C                   in(E)     *dtaara
023400030718     C                   IF        %Error  or  RSUT = *blanks
023500030718     C                   call      'TIBS34R'
023600030718     C                   parm                    Tibs34Ds
023700030718     C                   in        *dtaara
023800030718     c                   ENDIF
023900070228     c*
024000070228     C                   TIME                    WHHUDT           14 0
024100070228     C                   MOVEL     WHHUDT        ORA               6 0
024200070510     C                   MOVE      WHHUDT        UDATE8            8 0
024300070510     c                   move      udate8        dtaeur
024400070510     c                   move      dtaeur        dtaiso
024500070510      *
024600070510     c******             move      *date         dtaiso
024700070228     c                   move      dtaiso        dateu8            8 0
024800070510      *
024900070302     C     d17FGSi       CHAIN     AZORG01l
025000070228    1C                   IF        %found(azorg01l)
025001071107     c                   movel     orgdf0        og150
025100070228     C                   MOVEL     ORGde6        og146
025101071107     C                   MOVEL     ORGDE7        og147
025102071107     c     §OGPKM        comp      'S'                                    70
025103071107     c     §OGdoada      comp      'S'                                    71
025200070228     c* se data distinte automatiche = 31/12/2039 p.o. in fase di test
025300070228     c* se data distinte automatiche < oggi p.o. partito
025400070228     c                   if        §ogdda = ' '
025500070228     c                   exsr      srfine
025600070228     c                   else
025700070228     c                   move      §ogdda        comdda            8 0
025800070228     c                   end
025900070228     c                   end
026000950525     C*  Definisco chiavi di accesso
026100080326     C     Kdsf          KLIST
026200080326     C                   KFLD                    dstnpg
026300080326     C                   KFLD                    dstnfv
026400080326     C                   KFLD                    dstfgs
026401080326     C                   KFLD                    dsftrd
026402080326     c*
026900070223     C                   TIME                    TIMES             6 0
027000070223     C                   TIME                    WHDAT            12 0
027100070223     C                   MOVE      WHDAT         WOGGI             6 0
027200070223     c* stampo testa
027300070223     c                   seton                                        80
027400070223     c                   exsr      srstampa
027500950525     C                   ENDSR
