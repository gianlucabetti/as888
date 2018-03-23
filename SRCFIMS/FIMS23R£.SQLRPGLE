000100060928      *---------------------------------------------------------------*
000200101118      * file saldi RPVP
000300060928      *---------------------------------------------------------------*
000400060928
000500060928     h decedit('0,') datedit(*dmy/)
000600060928
000700060928      *---------------------------------------------------------------*
000800101118     fFNMSP11L  Uf A e           k disk
000900101214     fFNMSA12L  if   e           k disk    USROPN extfile(WFNMSA)
001000060928      *---------------------------------------------------------------*
001100060928      *
001200060928      * - Parametri per richiamo al pgm di controllo profilo utenti
001300101122     d kpjba         e ds
001400101122     d TIBS34ds      e ds
001500060928      * - Ds di riferimento al file esterno AzUte00F
001600060928     d AZUTEds       e ds                  extname(AZUTE00F)
001700060928      * - Ds per dati organigramma
001800060928     d DDatiUte      e ds
001900101210     d
002000101210     d DVPO          e ds
002100101210     D DSBS02        E DS                  EXTNAME(TIBS02DS)
002200060928      *
002300060928      * - Parametri in INPUT
002400060928     d Param           ds                                                       *DS parametri
002500060928     d  PARfil                 1      3  0                                      - filiale
002600101118     d  PARdfv                 4     11  0                                      - data foglio
002700101118     d  PARamb                12     12                                         - ambito spunte
002800101210     d  PARvol                13     19                                         - ambito spunte
002900101210     d  PARpes                20     24                                         - ambito spunte
003000060928      *
003100060928      * - Controlla data
003200060928     d WLBdat          ds                  inz
003300060928     d  G08dat                        8  0 inz
003400060928     d  G08inv                        8  0 inz
003500060928     d  G08err                        1    inz('3')
003600060928     d  G08tgi                        5  0 inz
003700060928      * - Calcolo data
003800060928     d WGIdat          ds                  inz
003900060928     d  GIOdat                        8  0 inz
004000060928     d  GIOinv                        8  0 inz
004100060928     d  GIOtgi                        5  0 inz
004200060928      *
004300060928      *  ?V A R I A B I L I?  - - - - - - - - - - - - - - - - - - - - *
004400060928      *
004500060928      * - Campi chiave riferiti al data base
004600101118     d Kfil            s                   like(MSpfil)  inz
004700101118     d Kapl            s                   like(MSpapl)  inz
004800101118     d Kdfv            s                   like(MSpdfv)  inz
004900101119     d Knpg            s                   like(MSPnpg)  inz
005000101210     d Wvol            s              5  3
005100101210     d Wpes            s              7  1
005200101210     d totcol          s              7  0
005300101210     d totpes          s              9  1
005400101210     d totvol          s              9  3
005500101210     d totmin          s              9  0
005600101210     d TipoW           s              1
005700101119
005800101119     d wrkgetlista     s           4096    varying
005900101119
006000101119     d Dataeur         s               d   datfmt(*eur)
006100101119     d Dataiso         s               d   datfmt(*iso)
006200101119     d Dataymd         s               d   datfmt(*ymd)
006300101119     d Datadmy         s               d   datfmt(*dmy)
006400101119     d Datasys         s               d   inz(*sys)
006500101214     d
006600101214     d Wfnmsa          s             21    inz('FILTRAGRU /FNMSA12L')
006700101214     d libfil          s              9    inz('FILTRA201')
006800060928      *
006900060928      *  ?K E Y - L I S T?  - - - - - - - - - - - - - - - - - - - - - *
007000060928      *
007100101119      * - FNMSA00F
007200101119     c     Kmsp          klist
007300101118     c                   kfld                    Kapl
007400101118     c                   kfld                    Kfil
007500101119     c                   kfld                    Kdfv
007600101119     c                   kfld                    Knpg
007700101119     c     Kmsa          klist
007800101119     c                   kfld                    Kapl
007900101119     c                   kfld                    Kfil
008000060928
008100060928      *===============================================================*
008200060928      *?  I N D I C A T O R I                                        ?*
008300060928      *---------------------------------------------------------------*
008400060928      *  10 - Comodo                                                  *
008500060928      *===============================================================*
008600060928      *
008700101119      * Elaboro solo applicazione "P",
008800060928      *
008900101118     c                   movel     'P'           kapl
009000101118     c                   clear                   kfil
009100101118     c
009200101119     c* se passata sostiuisco udate - 1 coon la data passata
009300101119     c                   if        pardfv>0
009400101118     c                   eval      kdfv=pardfv
009500101119     c                   endif
009600101210     c* Peso e volume
009700101210     c                   if        parvol >*zeros
009800101210     c                   movel     parvol        Wvol
009900101210     c                   else
010000101210     c                   movel     §vporpvpv     Wvol
010100101210     c                   endif
010200101210     c                   if        parpes >*zeros
010300101210     c                   movel     parpes        Wpes
010400101210     c                   else
010500101210     c                   movel     §vporpvpp     Wpes
010600101210     c                   endif
010700101214
010800101214     c                   if        parfil>0
010900101214     c                   eval      kfil=parfil
011000101214
011100101214     c                   if        paramb=' ' or paramb='P'
011200101214     c                   eval      knpg=5
011300101214     c                   exsr      PrepaSQL
011400101214     c                   endif
011500101214     c
011600101214     c                   if        paramb=' ' or paramb='A'
011700101214     c                   eval      knpg=2
011800101214     c                   exsr      PrepaSQL
011900101214     c                   endif
012000101214     c
012100101214     c                   else
012200101118     c
012300101118     c* Ciclo sulle filiali per trovarle tutte con la  P
012400101118     c     kmsa          setgt     fnmsa12l
012500101118     c     kapl          reade     fnmsa12l
012600101118     c
012700101118     c                   dow       not %eof(fnmsa12l)
012800101118     C                   eval      kfil=msafil
012900101118     c
013000101119     c                   if        paramb=' ' or paramb='P'
013100101118     c                   eval      knpg=5
013200101119     c                   exsr      PrepaSQL
013300101119     c                   endif
013400101119     c
013500101119     c                   if        paramb=' ' or paramb='A'
013600101118     c                   eval      knpg=2
013700101119     c                   exsr      PrepaSQL
013800101118     c                   endif
013900101118     c
014000101118     c     kmsa          setgt     fnmsa12l
014100101118     c     kapl          reade     fnmsa12l
014200101118     c
014300101118     c                   enddo
014400101214     c                   endif
014500101118     c
014600060928      *
014700060928      * Fine
014800101119     c                   eval      *inLR   = *on
014900060928
015000060928      *---------------------------------------------------------------*
015100060928      *?Operazioni iniziali                                          ?*
015200060928      *---------------------------------------------------------------*
015300060928     c     *InzSR        BEGSR
015400101210     c
015500101210     c     *Entry        plist
015600101210     c                   parm                    kpjba
015700101210     c                   movel     kpjbu         param
015800060928      *
015900060928      * Reperimento dati job
016000060928     c                   exsr      DatiJob
016100060928      *
016200101119     c* Udate - 1
016300101119     c                   eval      dataiso=datasys
016400101119     c                   subdur    1:*d          dataiso
016500101119     c                   eval      kdfv=%dec(dataiso)
016600101210     c* Tabella VPO
016700101210     C                   CLEAR                   dsbs02
016800101210     c                   clear                   dvpo
016900101210     C                   MOVEL     'C'           T02MOD
017000101210     C                   MOVEL     KNSIF         T02SIF
017100101210     C                   MOVEL     'VPO'         T02COD
017200101210     c                   movel(P)  'VPO'         t02ke1
017300101210     C                   CALL      'TIBS02R'
017400101210     C                   PARM                    KPJBA
017500101210     C                   PARM                    dsbs02
017600101210    2C                   IF        T02ERR = *BLANKS
017700101210     C                   MOVEL     T02uni        dvpo
017800101210    2C                   ENDIF
017900060928      *
018000101214     c                   open(e)   fnmsa12l
018100101214     c                   if        not %open(fnmsa12l)
018200101214     c                   eval      %subst(Wfnmsa:7:4)='GRPF'
018300101214     c                   eval      libfil='FILTRAPRD'
018400101214     C                   OPEN      fnmsa12l
018500101214     c                   endif
018600101214     c
018700060928     c                   ENDSR
018800101119      *---------------------------------------------------------------*
018900101119      *?Reperimento dati del job (Utente/Operativi)                  ?*
019000101119      *---------------------------------------------------------------*
019100101119     c     DatiJob       BEGSR
019200101119      *
019300101119     c     *dtaara       define    §azute        AZUTEds
019400101119     c     *dtaara       define    §datiute      dDatiUte
019500101119      *
019600101119     c                   in(E)     *dtaara
019700101119     c                   if        %error or RSUT = *blanks
019800101119     c                   clear                   TIBS34ds
019900101119     c                   call      'TIBS34R'
020000101119     c                   parm                    TIBS34ds
020100101119     c                   in        *dtaara
020200101119     c                   endif
020300101119      *
020400101119     c                   ENDSR
020500060928
020600101119      *---------------------------------------------------------------*
020700101119      *?prepara SQL                                                  ?*
020800101119      *---------------------------------------------------------------*
020900101119     c     PrepaSQL      BEGSR
021000101210     c                   clear                   wrkgetlista
021100101119     c                   eval      wrkgetlista = 'select count(*),  +
021200101119     c                              sum(msapes), sum(msavol), sum(msapussor) +
021300101214     c                              FROM ' + %subst(Wfnmsa:1:10)  +
021400101214     c                                 '/FNMSA10F where msafil=' +
021500101119     c                              %editc(kfil:'X')+
021600101119     c                              ' and msadfv=' + %editc(kdfv:'X') +
021700101119     c                              ' and msaapl=''P'' and msanpg='   +
021800101119     c                              %editc(knpg:'X')
021900101119     C/EXEC SQL
022000101119     C+ PREPARE s1 FROM :wrkgetlista
022100101119     C/END-EXEC
022200101119     C
022300101119     C/EXEC SQL
022400101119     C+ DECLARE A1 CURSOR FOR S1
022500101119     C/END-EXEC
022600101119     C
022700101119     C/EXEC SQL
022800101119     C+ OPEN a1
022900101119     C/END-EXEC
023000101119
023100101119      *?Leggo il file
023200101119     C/EXEC SQL
023300101119     C+ FETCH NEXT FROM a1  into :totmin, :totpes, :totvol, :totcol
023400101119     C/END-EXEC
023500101119
023600101210     c                   if        sqlcod = 0
023700101210     c                   eval      tipoW='R'
023800101119     c                   exsr      ScriviMSP
023900101210     c                   endif
024000101119     C/EXEC SQL
024100101119     C+ CLOSE a1
024200101119     C/END-EXEC
024300101210     c
024400101210     c* Eseguo SQL su bolle partenza o bolle arrivo per contate i colli bollettati/arrivati
024500101210     c                   if        knpg=5
024600101210     c                   exsr      BollePar
024700101210     c                   else
024800101210     c                   exsr      BolleArr
024900101210     c                   endif
025000101210
025100101210     c                   ENDSR
025200101210      *---------------------------------------------------------------*
025300101210      *?prepara SQL  BOLLE                                           ?*
025400101210      *---------------------------------------------------------------*
025500101210     c     PrepaSQLB     BEGSR
025600101210     C/EXEC SQL
025700101210     C+ PREPARE s2 FROM :wrkgetlista
025800101210     C/END-EXEC
025900101210     C
026000101210     C/EXEC SQL
026100101210     C+ DECLARE A2 CURSOR FOR S2
026200101210     C/END-EXEC
026300101210     C
026400101210     C/EXEC SQL
026500101210     C+ OPEN a2
026600101210     C/END-EXEC
026700101210
026800101210      *?Leggo il file
026900101210     C/EXEC SQL
027000101210     C+ FETCH NEXT FROM a2  into :totcol
027100101210     C/END-EXEC
027200101210
027300101210     c                   if        sqlcod = 0
027400101210     c                   eval      tipoW='B'
027500101210     c                   exsr      ScriviMSP
027600101210     c                   endif
027700101210     C/EXEC SQL
027800101210     C+ CLOSE a2
027900101210     C/END-EXEC
028000101210
028100101210     c                   ENDSR
028200101210      *---------------------------------------------------------------*
028300101210      *?Leggo bolle partenza FNBLP per data partenza ultimo collo
028400101210      *---------------------------------------------------------------*
028500101210     c     BollePar      BEGSR
028600101210     c                   clear                   wrkgetlista
028700101210     c
028800101210     c                   eval      wrkgetlista = 'select sum(blpncl) +
028900101214     c                              FROM ' + libfil  +
029000101214     c                                 '/FNBLP00F where blpduc=' +
029100101210     c                              %editc(kdfv:'X')+
029200101210     c                              ' and blptfp=' +%editc(kfil:'X')  +
029300101210     c                              ' and (blpvlf>' +%editc(Wvol:'4')  +
029400101210     c                              ' or blppkf>' +%editc(Wpes:'4') +')'
029500101210     c
029600101210     c                   exsr      PrepasqlB
029700101119     c
029800101119     c                   ENDSR
029900101210      *---------------------------------------------------------------*
030000101210      *?Leggo bolle arrivo   FNARB per data arrivo primo collo
030100101210      *---------------------------------------------------------------*
030200101210     c     BolleArr      BEGSR
030300101210     c                   clear                   wrkgetlista
030400101210     c
030500101210     c                   eval      wrkgetlista = 'select sum(arbncl)+
030600101214     c                              FROM ' + libfil  +
030700101214     c                                 '/FNARB00F where ARBDAM=' +
030800101210     c                              %editc(kdfv:'X')+
030900101210     c                              ' and ARBTFA=' +%editc(kfil:'X')  +
031000101210     c                              ' and (arbvlf>' +%editc(Wvol:'4')  +
031100101210     c                              ' or arbpkf>' +%editc(Wpes:'4')+')'
031200101210     c
031300101210     c                   exsr      PrepasqlB
031400101210     c
031500101210     c                   ENDSR
031600101119      *---------------------------------------------------------------*
031700101210      *?Scrivo record in MSA --> se già presente lo updato azzerando  ?*
031800101119      *---------------------------------------------------------------*
031900101119     c     ScriviMSP     BEGSR
032000101119     c     kmsp          chain     fnmsp11l
032100101119     c                   if        not %found(fnmsp11l)
032200101119     c                   clear                   fnmsp000
032300101119     c                   eval      mspnpg=knpg
032400101119     c                   eval      mspfil=kfil
032500101119     c                   eval      mspdfv=kdfv
032600101119     c                   eval      mspapl=kapl
032700101119     c                   endif
032800101210     c                   if        tipoW='R'
032900101119     c                   eval      mspncca=totcol
033000101119     c                   eval      msppkca=totpes
033100101119     c                   eval      mspvlca=totvol
033200101119     c                   eval      msptlm =totmin
033300101210     c                   else
033400101210     c                   eval      mspcnbo=totcol
033500101210     c                   endif
033600101119     c*
033700101119     c                   if        %found(fnmsp11l)
033800101119     c                   update    fnmsp000
033900101119     c                   else
034000101119     c                   write     fnmsp000
034100101119     c                   endif
034200101119     c
034300101119     c                   ENDSR
