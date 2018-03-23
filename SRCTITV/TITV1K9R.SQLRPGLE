000100941109     H*------------------------------------------------------------------------*
000200100330     h* DWL: trad. TIVGD bolle consegnate TITAS 0013824, 0013971, 0014856
000300081210     H* Ritorno dati a cliente - bolle consegnate   x BTCESAB                  *
000400090616     H* Ritorno dati a cliente - bolle consegnate   x RIMINI FIERA             *
000500941109     H*------------------------------------------------------------------------*
000600930128     H DECEDIT('0,') DATEDIT(*DMY.)
000700941109     F*------------------------------------------------------------------------*
000800081205     fcnaco00f  if   e           k disk
000900081205     ftabel00f  if   e           k disk
001000081210     ffnlbl01l  if   e           k disk
001100151016     FFIAR531C  IF   E           K DISK
001200081209     ftita430c  IF   E           K DISK
001300060829     FTIVGD00F  o    E             DISK
001400060828     d
001500060828     D KPJBA         E DS
001600070129     D tibs02ds      E DS
001700070305     D DVGDFLO       E DS
001800081205     D Dtasco_I      E DS
001900081209     D DTA4a         E DS
002000081209     D DSQT1         E DS
002100081210     D DSTB          E DS
002200151016     D DAR5FAT       E DS
002300090616     d*
002400090616     D TV1K9DS         DS
002500090616     d itvk9dtp                1      8
002600090616     d itvk9dtu                9     16
002700090616     d itvk9btces             17     17
002800090616     d itvk9Fiera             18     18
002900091012     d itvk9Grillo            19     19
003000140929     d itvk9toyota            20     20
003100080926     d
003200060828      *
003300060829     d Dataiso         s               d   datfmt(*iso)
003400060829     d Dataeur         s               d   datfmt(*eur)
003500070124     d ktbkut          s                   like(tblkut) inz(1)
003600080926     D Kcod            S                   LIKE(tblcod)
003700080926     D Kkey            S                   LIKE(tblkey)
003800081205     D Kccc            S                   LIKE(acokcc) inz(151)
003900081209     d wTara           s                   Like(§QtTpc)
004000081209     d wNtara          s                   Like(TasPkf)
004100081209     d KTRC            s                   Like(Ta4trc) inz('A')
004200081209     d wrkgetlista     s           4096    varying
004300081209     d titasds       e ds                  extname(titas10f)
004400090616     d Wksu            s              8
004500080926
004600941109     C*------------------------------------------------------------------------*
004700060424     c     *entry        plist
004800060424     c                   parm                    kpjba
004900090616     c                   movel     kpjbu         tv1k9ds
005000090616     c
005100090616     c* verifico se passate delle date con lancio manuale
005200090616     c                   if        itvk9dtp>*zeros and itvk9dtu>*zeros
005300090616     c                   movel     itvk9dtp      primese
005400090616     c                   movel     itvk9dtu      ultmese
005500090616     c                   endif
005600090616     c
005700090616     C* BTCESAB
005800090616     c                   if        itvk9btces<>'N'
005900090616     C                   EXSR      BTCESAB
006000090616     c                   endif
006100090616     C* Rimini Fiera
006200090616     c                   if        itvk9fiera<>'N'
006300090616     C                   EXSR      RiFiera
006400090616     c                   endif
006500140929     C* TOYOTA
006600140929     c                   if        itvk9toyota<>'N'
006700140929     C                   EXSR      TOYOTA
006800140929     c                   endif
006900091012     C* Grillo SPA
007000121016     c* ES - 16/10/12 tolto su indicazione di Fabrizio
007100121016     c***                if        itvk9grillo<>'N'
007200121016     C***                EXSR      Grillo
007300121016     c***                endif
007400090616     C
007500000000     C                   SETON                                        LR
007600090616     c*-----------------------------------------------------------------------
007700090616     C     BTCESAB       BEGSR
007800090616     c* Cliente unificante
007900100330     c                   eval      Wksu='00014856'
008000090616     c
008100090616      *?Imposto la parte fissa dell'istruzione sql
008200090616     c                   eval      wrkgetlista =
008300090616     c                             'select titas10f.* from titas10f where' +
008400090616     c                             ' tasdcm between '+
008500090616     c                              %editc(primese:'X') + ' and ' +
008600090616     c                              %editc(ultmese:'X') +' and ' +
008700100330     c                             'tasccm  in(0013824, 0013971, 0014856)'
008800090616      *?Ordinamento
008900090616     c                   eval      wrkgetlista = wrkgetlista +
009000090616     c                             ' order by tasdcm '
009100090616      *?Solo lettura
009200090616     c                   eval      wrkgetlista = wrkgetlista + ' for read only'
009300090616     C/EXEC SQL
009400090616     C+ DECLARE A1 CURSOR FOR S1
009500090616     C/END-EXEC
009600090616     C
009700090616     c                   exsr      EseguiSQL
009800090616     c
009900090616     c
010000090616     c                   ENDSR
010100090616     c*-----------------------------------------------------------------------
010200090616     C     Rifiera       BEGSR
010300090616     c* Cliente unificante
010400090616     c                   eval      Wksu='00240948'
010500090616     c
010600090616      *?Imposto la parte fissa dell'istruzione sql
010700090616     c                   eval      wrkgetlista =
010800090616     c                             'select titas10f.* from titas10f where' +
010900090616     c                             ' tasdcm between '+
011000090616     c                              %editc(primese:'X') + ' and ' +
011100090616     c                              %editc(ultmese:'X') +' and ' +
011200090617     c                             'tasccm  in(0240948)'
011300090616      *?Ordinamento
011400090616     c                   eval      wrkgetlista = wrkgetlista +
011500090616     c                             ' order by tasdcm '
011600090616      *?Solo lettura
011700090616     c                   eval      wrkgetlista = wrkgetlista + ' for read only'
011800090616     c
011900090616     c                   exsr      EseguiSQL
012000090616
012100090616     c                   ENDSR
012200140929     c*-----------------------------------------------------------------------
012300140929     C     TOYOTA        BEGSR
012400140929     c* Cliente unificante
012500140929     c                   eval      Wksu='00014857'
012600140929     c
012700140929      *?Imposto la parte fissa dell'istruzione sql
012800140929     c                   eval      wrkgetlista =
012900140929     c                             'select titas10f.* from titas10f where' +
013000140929     c                             ' tasdcm between '+
013100140929     c                              %editc(primese:'X') + ' and ' +
013200140929     c                              %editc(ultmese:'X') +' and ' +
013300140929     c                             'tasccm  in(0014857)'
013400140929      *?Ordinamento
013500140929     c                   eval      wrkgetlista = wrkgetlista +
013600140929     c                             ' order by tasdcm '
013700140929      *?Solo lettura
013800140929     c                   eval      wrkgetlista = wrkgetlista + ' for read only'
013900140929     C
014000140929     c                   exsr      EseguiSQL
014100140929     c
014200140929     c
014300140929     c                   ENDSR
014400091012     c*-----------------------------------------------------------------------
014500121016     C***  Grillo        BEGSR
014600091012     c* Cliente unificante
014700121016     c***                eval      Wksu='01310112'
014800121016     c***
014900091012      *?Imposto la parte fissa dell'istruzione sql
015000121016     c***                eval      wrkgetlista =
015100121016     c***                          'select titas10f.* from titas10f where' +
015200121016     c***                          ' tasdcm between '+
015300121016     c***                           %editc(primese:'X') + ' and ' +
015400121016     c***                           %editc(ultmese:'X') +' and ' +
015500121016     c***                          'tasccm  in(1310112, 2750948, 1310513)'
015600091012      *?Ordinamento
015700121016     c***                eval      wrkgetlista = wrkgetlista +
015800121016     c***                          ' order by tasdcm '
015900091012      *?Solo lettura
016000121016     c***                eval      wrkgetlista = wrkgetlista + ' for read only'
016100121016     c***
016200121016     c***                exsr      EseguiSQL
016300121016     c***
016400121016     c***                ENDSR
016500090616     c*------------------------------------------------------------------
016600090616     c     EseguiSQL     BEGSR
016700090616     c
016800090616     C/EXEC SQL
016900090616     C+ PREPARE s1 FROM :wrkgetlista
017000090616     C/END-EXEC
017100090616     C
017200090616     C/EXEC SQL
017300090616     C+ OPEN a1
017400090616     C/END-EXEC
017500090616
017600090616      *?Leggo il file
017700090616     c                   do        *hival
017800090616     C/EXEC SQL
017900090616     C+ FETCH NEXT FROM a1 INTO :titasds
018000090616     C/END-EXEC
018100090616
018200090616     c                   select
018300090616
018400090616     c                   when      sqlcod = 100
018500090616     c                   leave
018600090616     c                   when      sqlcod < 0
018700090616     c                   seton                                        H1
018800090616     c                   eval      *InLr = *On
018900090616     c                   return
019000090616     c
019100090616     c                   other
019200090616     c                   exsr      Elabora
019300090616     c                   endsl
019400090616
019500090616     c                   enddo
019600090616
019700090616     C/EXEC SQL
019800090616     C+ CLOSE a1
019900090616     C/END-EXEC
020000090616     c                   ENDSR
020100060828     c*------------------------------------------------------------------
020200060828     c     *INZSR        BEGSR
020300080926     c
020400081210     C     kaco          KLIST
020500070124     C                   KFLD                    KTBKUT
020600081205     C                   KFLD                    kccc
020700081205     C                   KFLD                    tasccm
020800080926     C     ktab          KLIST
020900080926     C                   KFLD                    KTBKUT
021000080926     C                   KFLD                    kcod
021100080926     C                   KFLD                    kkey
021200081209     C     kta4          KLIST
021300081209     C                   KFLD                    tasaas
021400081209     C                   KFLD                    taslnp
021500081209     C                   KFLD                    tasnrs
021600081209     C                   KFLD                    tasnsp
021700081210     C                   KFLD                    ktrc
021800081210     C     KLBL          KLIST
021900081210     C                   KFLD                    tasaas
022000081210     C                   KFLD                    taslnp
022100081210     C                   KFLD                    tasnrs
022200081210     C                   KFLD                    tasnsp
022300151016     C*
022400151016     C* FIAR531C - Parziale
022500151016     C     KEYar531_P    KLIST
022600151016     C                   KFLD                    tasAAS
022700151016     C                   KFLD                    tasLNP
022800151016     C                   KFLD                    tasNRS
022900151016     C                   KFLD                    tasNSP
023000151016     C                   KFLD                    ar5TRD
023100081209     c
023200060829     c* Data di elaborazione
023300060829     c                   time                    w0140            14 0
023400060829     C                   MOVE      w0140         w0080             8 0
023500060829     c     *eur          movel     w0080         dataeur
023600060829     c                   movel     dataeur       dataiso
023700081205     c* data corrente
023800081205     c     *iso          movel     dataiso       datcor            8 0
023900081205     c* devo elaborare il mese precedente
024000081205     c                   extrct    dataiso:*d    giorni            3 0
024100081205     c     dataiso       subdur    giorni:*d     dataiso
024200081205     c     *iso          movel     dataiso       ultmese           8 0
024300081205     c                   extrct    dataiso:*d    giorni            3 0
024400081205     c                   sub       1             giorni
024500081205     c     dataiso       subdur    giorni:*d     dataiso
024600081205     c     *iso          movel     dataiso       primese           8 0
024700060829     c
024800060828     c                   ENDSR
024900060828     c*------------------------------------------------------------------
025000060829     c* Elaborazione per scrittura file TIVGD00F
025100060828     c*------------------------------------------------------------------
025200060828     c     ELABORA       BEGSR
025300081210     c* Escludo le bolle figlie perchè diamo i dati sulla mamma
025400081210     C*
025500081210     C     KLBL          CHAIN     FNLBL01L                           23
025600081210    1c                   if        *in23
025700081210     c* Escludo le bolle di recupero
025800081210     C                   Movel     'TB'          Kcod
025900081210     c                   Movel(P)  tastbl        Kkey
026000081210     c     Ktab          Chain     Tabel00f
026100081210    2c                   If        %Found(Tabel00f)
026200081210     c                   Movel     TblUni        dsTB
026300081210     c                   else
026400081210     c                   clear                   dsTB
026500081210    2c                   endif
026600081210     c
026700081210    2c                   if        §tbrbl<>'R'
026800081210     c
026900060829     c* Preparo la scrittura del file TIVGD
027000060829     c                   clear                   tivgd000
027100081210     c                   eval      vgdtip='VC'
027200090616     c                   eval      vgdksu=wksu
027300060829     c                   eval      vgdtsc='WW'
027400060829     c
027500060829     c                   eval      vgddaT=datcor
027600081210     c                   eval      vgdpgm='TITV1K9R'
027700070305     c                   clear                   DVGDFLO
027800080930     c*****              movel     'P'           §VGDFELA
027900080930     c*****              movel     DVGDFLO       vgdflo
028000060829     c
028100080926     c* Imposto i dati in DS
028200081205     c                   clear                   Dtasco_I
028300081205     c                   eval      §TACaas=%editc(tasaas:'Z')
028400081205     c                   eval      §taclnp=%editc(taslnp:'Z')
028500081205     c                   eval      §TACnrs=%Editc(tasnrs:'Z')
028600081205     c                   eval      §TACnsp=%Editc(tasnsp:'Z')
028700081205     c                   eval      §TACmgs=%Editc(tasmgs:'Z')
028800081205     c                   eval      §TACrmn=%Editc(tasrmn:'Z')
028900081209     c     kta4          chain     tita430c
029000081209     c                   if        %found(tita430c)
029100081209     c                   eval      dta4a=ta4not
029200081209     c                   eval      §TACrma=§ta4arma
029300081209     c                   endif
029400081205     c                   eval      §TACrsd=tasrsd
029500081205     c                   eval      §TACind=tasind
029600081205     c                   eval      §TAClod=taslod
029700081209     c                   eval      §TACprd=tasprd
029800081205     c                   eval      §TACcad=tascad
029900081205     c                   eval      §TACnzd=tasnzd
030000081205     c                   eval      §TACccm=tasccm
030100081205     c* chain su cnaco00f per avere ragione sociale mittente
030200081205     c     kaco          chain     cnaco00f
030300081205     c                   if        %found(cnaco00f)
030400081205     c                   eval      §TACrsm=acorag
030500081205     c                   else
030600081205     c                   eval      §TACRSM=*all'?'
030700081205     c                   endif
030800081205     c                   eval      §TACdcm=tasdcm
030900081209     c                   eval      §TAChmc=tashmc
031000081205     c                   eval      §TACcca=tascca
031100081205     c                   eval      §TACnrc=tasnrc
031200081209     c                   eval      §TACncl=tasncl
031300081209     c                   eval      §TACvlf=tasvlf
031400081209     c
031500081209      * Imposto il peso fatturato
031600151020If  1c                   If        TasFpf = 'R' OR TasFpf = 'D'
031700081209     c                   Z-Add     TasPkf        §TACpkf
031800081209     c                   endif
031900081209     c
032000081209      * Prendo il peso da cml - la tara
032100081209     c                   Clear                   dsQt1
032200081209     c                   Movel     'QT'          Kcod
032300081210     c                   Movel(P)  1             Kkey
032400081209     c     Ktab          Chain     Tabel00f
032500081209    1c                   If        %Found(Tabel00f) and TblFlg = *Blanks
032600081209     c                   Movel     TblUni        dsQt1
032700081209    1c                   EndIf
032800150716     C*
032900150716     C                   IF        TASDFT <= §QTDST
033000150716     C     §QTTCO        MULT      TASNCP        WTARA
033100150716     C     TASPKC        SUB(h)    WTARA         WNTARA
033200150716     C                   ELSE
033300150716     C     §QTTPC        MULT      TASNCP        WTARA
033400150716     C     TASPKC        SUB(h)    WTARA         WNTARA
033500150716     C                   ENDIF
033600081209     c*
033700081209     c* Se applicato vdl-tara--> lo imposto
033800081209If  1c                   If        TasFpf = 'V'
033900081209     c                   Z-Add     wNtara        §TACpkf
034000081209    1c                   endif
034100081209    c
034200081209     c* Se non c'e', imposto il più alto dei 2
034300081209    1c                   if        §TACpkf=0
034400081209     c                   select
034500081209    2c                   when      taspkc=0
034600081209     c                   Z-Add     TasPkf        §TACpkf
034700081209    2c                   when      taspkf>=wntara and taspkf<=taspkc
034800081209     c                   Z-Add     TasPkf        §TACpkf
034900081209    2c                   when      wntara>taspkf
035000081209     c                   Z-Add     wNtara        §TACpkf
035100081209     c                   other
035200081209     c                   Z-Add     TasPkf        §TACpkf
035300081209    2c                   endsl
035400081209     c*
035500081209    1c                   EndIf
035600151016     C*
035700151016     C* Eseguo eventuali "forzature"
035800151016     C                   EXSR      RTVAR5FAT
035900081209     c
036000081209     c                   eval      VGDDTA =%subst(Dtasco_I:1:%size(Dtasco_I))
036100081209     c*
036200081209     c                   write     tivgd000
036300081210    2c                   endif
036400081210    1c                   endif
036500070315     c*
036600060828     c                   ENDSR
036700151016
036800151016
036900151016
037000151016     C     RTVAR5FAT     BEGSR
037100151016     C*
037200151016     C* Se, presente, considero il peso/volume DESUNTO al pari del VDL TOTALE
037300151016     C                   SETOFF                                       7071
037400151016     C                   EVAL      ar5TRD = 'FAT'
037500151016     C                   CLEAR                   DAR5FAT
037600151016     C     KEYar531_P    CHAIN     FIAR531C
037700151016     C                   IF        %found(FIAR531C)
037800151016     C                   MOVEL     AR5UNI        DAR5FAT
037900151016     C                   IF        §AR5PKTAS > *zeros
038000151016     C                   SETON                                        70
038100151016     C                   EVAL      §TACPKF = §AR5PKTAS
038200151016     C                   ENDIF
038300151016     C                   IF        §AR5VLTAS > *zeros
038400151016     C                   SETON                                        71
038500151016     C                   EVAL      §TACVLF = §AR5VLTAS
038600151016     C                   ENDIF
038700151016     C                   ENDIF
038800151016     C*
038900151016     C                   ENDSR
