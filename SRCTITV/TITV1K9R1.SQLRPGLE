000100941109     H*------------------------------------------------------------------------*
000200110225     h* DWL: trad. TIVGD bolle consegnate TITAS 0014250  SPOT                  *
000300941109     H*------------------------------------------------------------------------*
000400930128     H DECEDIT('0,') DATEDIT(*DMY.)
000500941109     F*------------------------------------------------------------------------*
000600081205     fcnaco00f  if   e           k disk
000700081205     ftabel00f  if   e           k disk
000800081210     ffnlbl01l  if   e           k disk
000900151016     FFIAR531C  IF   E           K DISK
001000081209     ftita430c  IF   E           K DISK
001100060829     FTIVGD00F  o    E             DISK
001200060828     d
001300060828     D KPJBA         E DS
001400070129     D tibs02ds      E DS
001500070305     D DVGDFLO       E DS
001600081205     D Dtasco_I      E DS
001700081209     D DTA4a         E DS
001800081209     D DSQT1         E DS
001900081210     D DSTB          E DS
002000151016     D DAR5FAT       E DS
002100060828      *
002200060829     d Dataiso         s               d   datfmt(*iso)
002300060829     d Dataeur         s               d   datfmt(*eur)
002400070124     d ktbkut          s                   like(tblkut) inz(1)
002500080926     D Kcod            S                   LIKE(tblcod)
002600080926     D Kkey            S                   LIKE(tblkey)
002700081205     D Kccc            S                   LIKE(acokcc) inz(151)
002800081209     d wTara           s                   Like(§QtTpc)
002900081209     d wNtara          s                   Like(TasPkf)
003000081209     d KTRC            s                   Like(Ta4trc) inz('A')
003100081209     d wrkgetlista     s           4096    varying
003200081209     d titasds       e ds                  extname(titas10f)
003300090616     d Wksu            s              8
003400080926
003500941109     C*------------------------------------------------------------------------*
003600060424     c     *entry        plist
003700060424     c                   parm                    kpjba
003800090616     c
003900120313     c                   movel     20120301      primese
004000120313     c                   movel     20120310      ultmese
004100090616     c
004200101116     c                   exsr      READBOLLE
004300090616     C
004400000000     C                   SETON                                        LR
004500090616     c*-----------------------------------------------------------------------
004600101116     C     READBOLLE     BEGSR
004700090616     c* Cliente unificante
004800120313     c                   eval      Wksu='01340232'
004900090616     c
005000090616      *?Imposto la parte fissa dell'istruzione sql
005100110225     c                   eval      wrkgetlista =
005200110225     c                             'select * from titas10f where ' +
005300120313     c                             'tasccm  in(1340232) ' +
005400110225     c                             'and tasdcm between '+
005500110225     c                              %editc(primese:'X') + ' and '  +
005600110225     c                              %editc(ultmese:'X') +
005700110225     c                             ' union ' +
005800120313     c                             'select * from titas00f where ' +
005900120313     c                             'tasccm  in(1340232) ' +
006000110225     c                             'and tasdcm between '+
006100110225     c                              %editc(primese:'X') + ' and '  +
006200110225     c                              %editc(ultmese:'X')
006300090616      *?Ordinamento
006400090616     c                   eval      wrkgetlista = wrkgetlista +
006500090616     c                             ' order by tasdcm '
006600090616      *?Solo lettura
006700090616     c                   eval      wrkgetlista = wrkgetlista + ' for read only'
006800090616     C/EXEC SQL
006900090616     C+ DECLARE A1 CURSOR FOR S1
007000090616     C/END-EXEC
007100090616     C
007200090616     c                   exsr      EseguiSQL
007300090616     c
007400110225     c
007500090616     c                   ENDSR
007600090616     c*------------------------------------------------------------------
007700090616     c     EseguiSQL     BEGSR
007800090616     c
007900090616     C/EXEC SQL
008000090616     C+ PREPARE s1 FROM :wrkgetlista
008100090616     C/END-EXEC
008200090616     C
008300090616     C/EXEC SQL
008400090616     C+ OPEN a1
008500090616     C/END-EXEC
008600090616
008700090616      *?Leggo il file
008800090616     c                   do        *hival
008900090616     C/EXEC SQL
009000090616     C+ FETCH NEXT FROM a1 INTO :titasds
009100090616     C/END-EXEC
009200090616
009300090616     c                   select
009400090616
009500090616     c                   when      sqlcod = 100
009600090616     c                   leave
009700090616     c                   when      sqlcod < 0
009800090616     c                   seton                                        H1
009900090616     c                   eval      *InLr = *On
010000090616     c                   return
010100090616     c
010200090616     c                   other
010300090616     c                   exsr      Elabora
010400090616     c                   endsl
010500090616
010600090616     c                   enddo
010700090616
010800090616     C/EXEC SQL
010900090616     C+ CLOSE a1
011000090616     C/END-EXEC
011100090616     c                   ENDSR
011200060828     c*------------------------------------------------------------------
011300060828     c     *INZSR        BEGSR
011400080926     c
011500081210     C     kaco          KLIST
011600070124     C                   KFLD                    KTBKUT
011700081205     C                   KFLD                    kccc
011800081205     C                   KFLD                    tasccm
011900080926     C     ktab          KLIST
012000080926     C                   KFLD                    KTBKUT
012100080926     C                   KFLD                    kcod
012200080926     C                   KFLD                    kkey
012300081209     C     kta4          KLIST
012400081209     C                   KFLD                    tasaas
012500081209     C                   KFLD                    taslnp
012600081209     C                   KFLD                    tasnrs
012700081209     C                   KFLD                    tasnsp
012800081210     C                   KFLD                    ktrc
012900081210     C     KLBL          KLIST
013000081210     C                   KFLD                    tasaas
013100081210     C                   KFLD                    taslnp
013200081210     C                   KFLD                    tasnrs
013300081210     C                   KFLD                    tasnsp
013400151016     C*
013500151016     C* FIAR531C - Parziale
013600151016     C     KEYar531_P    KLIST
013700151016     C                   KFLD                    tasAAS
013800151016     C                   KFLD                    tasLNP
013900151016     C                   KFLD                    tasNRS
014000151016     C                   KFLD                    tasNSP
014100151016     C                   KFLD                    ar5TRD
014200081209     c
014300060829     c* Data di elaborazione
014400060829     c                   time                    w0140            14 0
014500060829     C                   MOVE      w0140         w0080             8 0
014600060829     c     *eur          movel     w0080         dataeur
014700060829     c                   movel     dataeur       dataiso
014800081205     c* data corrente
014900081205     c     *iso          movel     dataiso       datcor            8 0
015000081205     c* devo elaborare il mese precedente
015100081205     c                   extrct    dataiso:*d    giorni            3 0
015200081205     c     dataiso       subdur    giorni:*d     dataiso
015300081205     c     *iso          movel     dataiso       ultmese           8 0
015400081205     c                   extrct    dataiso:*d    giorni            3 0
015500081205     c                   sub       1             giorni
015600081205     c     dataiso       subdur    giorni:*d     dataiso
015700081205     c     *iso          movel     dataiso       primese           8 0
015800060829     c
015900060828     c                   ENDSR
016000060828     c*------------------------------------------------------------------
016100060829     c* Elaborazione per scrittura file TIVGD00F
016200060828     c*------------------------------------------------------------------
016300060828     c     ELABORA       BEGSR
016400081210     c* Escludo le bolle figlie perchè diamo i dati sulla mamma
016500081210     C*
016600081210     C     KLBL          CHAIN     FNLBL01L                           23
016700081210    1c                   if        *in23
016800081210     c* Escludo le bolle di recupero
016900081210     C                   Movel     'TB'          Kcod
017000081210     c                   Movel(P)  tastbl        Kkey
017100081210     c     Ktab          Chain     Tabel00f
017200081210    2c                   If        %Found(Tabel00f)
017300081210     c                   Movel     TblUni        dsTB
017400081210     c                   else
017500081210     c                   clear                   dsTB
017600081210    2c                   endif
017700081210     c
017800081210    2c                   if        §tbrbl<>'R'
017900081210     c
018000060829     c* Preparo la scrittura del file TIVGD
018100060829     c                   clear                   tivgd000
018200081210     c                   eval      vgdtip='VC'
018300101116     c                   eval      vgdksu='01020001'
018400060829     c                   eval      vgdtsc='WW'
018500060829     c
018600060829     c                   eval      vgddaT=datcor
018700081210     c                   eval      vgdpgm='TITV1K9R'
018800070305     c                   clear                   DVGDFLO
018900080930     c*****              movel     'P'           §VGDFELA
019000080930     c*****              movel     DVGDFLO       vgdflo
019100060829     c
019200080926     c* Imposto i dati in DS
019300081205     c                   clear                   Dtasco_I
019400081205     c                   eval      §TACaas=%editc(tasaas:'Z')
019500081205     c                   eval      §taclnp=%editc(taslnp:'Z')
019600081205     c                   eval      §TACnrs=%Editc(tasnrs:'Z')
019700081205     c                   eval      §TACnsp=%Editc(tasnsp:'Z')
019800081205     c                   eval      §TACmgs=%Editc(tasmgs:'Z')
019900081205     c                   eval      §TACrmn=%Editc(tasrmn:'Z')
020000081209     c     kta4          chain     tita430c
020100081209     c                   if        %found(tita430c)
020200081209     c                   eval      dta4a=ta4not
020300081209     c                   eval      §TACrma=§ta4arma
020400081209     c                   endif
020500081205     c                   eval      §TACrsd=tasrsd
020600081205     c                   eval      §TACind=tasind
020700081205     c                   eval      §TAClod=taslod
020800081209     c                   eval      §TACprd=tasprd
020900081205     c                   eval      §TACcad=tascad
021000081205     c                   eval      §TACnzd=tasnzd
021100081205     c                   eval      §TACccm=tasccm
021200081205     c* chain su cnaco00f per avere ragione sociale mittente
021300081205     c     kaco          chain     cnaco00f
021400081205     c                   if        %found(cnaco00f)
021500081205     c                   eval      §TACrsm=acorag
021600081205     c                   else
021700081205     c                   eval      §TACRSM=*all'?'
021800081205     c                   endif
021900081205     c                   eval      §TACdcm=tasdcm
022000081209     c                   eval      §TAChmc=tashmc
022100081205     c                   eval      §TACcca=tascca
022200081205     c                   eval      §TACnrc=tasnrc
022300081209     c                   eval      §TACncl=tasncl
022400081209     c                   eval      §TACvlf=tasvlf
022500081209     c
022600081209      * Imposto il peso fatturato
022700151020If  1c                   If        TasFpf = 'R' OR TasFpf = 'D'
022800081209     c                   Z-Add     TasPkf        §TACpkf
022900081209     c                   endif
023000081209     c
023100081209      * Prendo il peso da cml - la tara
023200081209     c                   Clear                   dsQt1
023300081209     c                   Movel     'QT'          Kcod
023400081210     c                   Movel(P)  1             Kkey
023500081209     c     Ktab          Chain     Tabel00f
023600081209    1c                   If        %Found(Tabel00f) and TblFlg = *Blanks
023700081209     c                   Movel     TblUni        dsQt1
023800081209    1c                   EndIf
023900150716     C*
024000150716     C                   IF        TASDFT <= §QTDST
024100150716     C     §QTTCO        MULT      TASNCP        WTARA
024200150716     C     TASPKC        SUB(h)    WTARA         WNTARA
024300150716     C                   ELSE
024400150716     C     §QTTPC        MULT      TASNCP        WTARA
024500150716     C     TASPKC        SUB(h)    WTARA         WNTARA
024600150716     C                   ENDIF
024700081209     c*
024800081209     c* Se applicato vdl-tara--> lo imposto
024900081209If  1c                   If        TasFpf = 'V'
025000081209     c                   Z-Add     wNtara        §TACpkf
025100081209    1c                   endif
025200081209    c
025300081209     c* Se non c'e', imposto il più alto dei 2
025400081209    1c                   if        §TACpkf=0
025500081209     c                   select
025600081209    2c                   when      taspkc=0
025700081209     c                   Z-Add     TasPkf        §TACpkf
025800081209    2c                   when      taspkf>=wntara and taspkf<=taspkc
025900081209     c                   Z-Add     TasPkf        §TACpkf
026000081209    2c                   when      wntara>taspkf
026100081209     c                   Z-Add     wNtara        §TACpkf
026200081209     c                   other
026300081209     c                   Z-Add     TasPkf        §TACpkf
026400081209    2c                   endsl
026500081209     c*
026600081209    1c                   EndIf
026700151016     C*
026800151016     C* Eseguo eventuali "forzature"
026900151016     C                   EXSR      RTVAR5FAT
027000081209     c
027100081209     c                   eval      VGDDTA =%subst(Dtasco_I:1:%size(Dtasco_I))
027200081209     c*
027300081209     c                   write     tivgd000
027400081210    2c                   endif
027500081210    1c                   endif
027600070315     c*
027700060828     c                   ENDSR
027800151016
027900151016
028000151016
028100151016     C     RTVAR5FAT     BEGSR
028200151016     C*
028300151016     C* Se, presente, considero il peso/volume DESUNTO al pari del VDL TOTALE
028400151016     C                   SETOFF                                       7071
028500151016     C                   EVAL      ar5TRD = 'FAT'
028600151016     C                   CLEAR                   DAR5FAT
028700151016     C     KEYar531_P    CHAIN     FIAR531C
028800151016     C                   IF        %found(FIAR531C)
028900151016     C                   MOVEL     AR5UNI        DAR5FAT
029000151016     C                   IF        §AR5PKTAS > *zeros
029100151016     C                   SETON                                        70
029200151016     C                   EVAL      §TACPKF = §AR5PKTAS
029300151016     C                   ENDIF
029400151016     C                   IF        §AR5VLTAS > *zeros
029500151016     C                   SETON                                        71
029600151016     C                   EVAL      §TACVLF = §AR5VLTAS
029700151016     C                   ENDIF
029800151016     C                   ENDIF
029900151016     C*
030000151016     C                   ENDSR
