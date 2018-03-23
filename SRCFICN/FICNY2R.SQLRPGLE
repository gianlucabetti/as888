000100120731     H DECEDIT('0,') DATEDIT(*YMD.)
000200120731     h dftactgrp(*no) actgrp(*caller)
000300120731ab   H BNDDIR('TIBS') EXTBININT(*YES) option(*nodebugio)
000400120725     H* FICNy2R *---------------------------------------------------*
000500081110     H*                                                              *
000600120725     H* Varia i codici dei fornitori su tutti i codici autisti
000700120725     H* del fornitore passato
000800120725     H* sia città che aff/def
000900120725     h* poi
001000111122     H* Duplica le tariffe in essere x la data decorrenza            *
001100111122     H*  facendole scadere il giorno prima.                          *
001200081209     H*                                                              *
001300120725     H* riceve come parametro la Data decorrenza e i codici vecchio e nuovo
001400120725     H*  del fornitore e la società di appartenenza                  *
001500000000     H*--------------------------------------------------------------*
001600120727     Ffifgt01L  uF   E           K DISK    commit
001700120727     Ffitgt01L  uF a E           K DISK    commit
001800120727     Ffiadt01L  uF a E           K DISK    commit
001900120727     Ffiadd01L  uF a E           K DISK    commit
001901170511     Ffiapd00f  uF   E             DISK    commit
002000081211      * -------------------------------------------------------------
002100111122     D KPJBA         E DS
002300120726     D ficny1ds      E DS
002400081230      *
002500111122     D oldPRG          s                   like(tgtPrg)
002600111122     D newPRG          s                   like(tgtPrg)
002700090109      *
002800111123     D DATA_eur        S               D   DATFMT(*eur)
002900111123     D DATA_iso        S               D   DATFMT(*iso)
003000111123      *
003100111122     D ficn06ds      E DS
003101170511     D fiapdds       E DS                  extname(fiapd00f)
003102170511     d rrnapd          s              9  0
003103170511     d fnlv36ds      e ds                  inz
003104170511     d fiapda        e ds                  extname(fiapd00f)
003105170511     d                                     prefix(a)
003106170511     d fiapdb        e ds                  extname(fiapd00f)
003107170511     d                                     prefix(b)
003108170511     d fiapd4A       e ds                  extname(fiapd40f)
003109170511     d                                     prefix(a)
003110170511     d fiapd4b       e ds                  extname(fiapd40f)
003111170511     d                                     prefix(b)
003112170511     c*
003200111123     C*------------------------------------------------------------------------*
003300111123     C*        - OPERAZIONI INIZIALI
003400111123     C*------------------------------------------------------------------------*
003500111123     C     *ENTRY        PLIST
003600120726     C                   PARM                    ficny1ds
003700120726      *
003800120725     c     kfgt          klist
003900120725     c                   kfld                    tgtpdr
004000120725     c                   kfld                    tgtsml
004100120725      *
004200120725     c     kadd          klist
004300120725     c                   kfld                    adtpdr
004400120725     c                   kfld                    oldprg
004500120725      *
004600120726     c                   move      ny1frno       ny1frnon          7 0
004700120726     c                   move      ny1frnn       ny1frnnn          7 0
004800120725      *
004900120726     C                   move      ny1data       data_iso
005000120725     c     data_iso      subdur    1:*days       data_iso
005100120725     C                   move      data_iso      datascad          8 0
005101170511      * variazioni
005102170511     c                   clear                   fnlv36ds
005103170511     c                   clear                   fiapdB
005104170511     c                   clear                   fiapdA
005105170511     c                   clear                   fiapd4b
005106170511     c                   clear                   fiapd4A
005107170511     c                   eval      ILV36PRU  = knmus
005108170511     c                   eval      ILV36NOJ  = knmeb
005109170511     c                   eval      ILV36PGM  = 'FICNY2R'
005110170511     c                   eval      ILV36CTA  = 'M'
005200111123      *
005300090423      **
005400120725     c* aggiornamento di massa di tutti i codici autisti a cui viene impostato il
005500120725     c* nuovo codice fornitore
005600120725      * ?   ------------------------------------------------------
005700120725     C/EXEC SQL
005701170511     C+ DECLARE b1 CURSOR FOR SELECT rrn(fiapd00f) FROM fiapd00f
005900120726     c+ where apdcsf = :ny1soco and apdksc = :ny1frnon and apdtip <> 'C'
006000120725     c+ and apdatb = ' '
006100120725     C/END-EXEC
006101170511     C/EXEC SQL
006102170511     C+ OPEN b1
006103170511     C/END-EXEC
006104170511
006105170511     C                   DOU       SqlCod <> 0
006106170511     C/EXEC SQL
006107170511     C+ FETCH NEXT FROM b1 INTO :rrnapd
006108170511     C/END-EXEC
006109170511     c*
006110170511     C                   SELECT
006111170511     C                   WHEN      SqlCod = 0
006112170511     c     rrnapd        chain     fiapd00f
006113170511     c                   if        %found(fiapd00f)
006114170511     c                   eval      fiapdB = fiapdds
006115170511     c                   eval      apdcsf = ny1socn
006116170511     c                   eval      apdksc = ny1frnnn
006117170511     C                   UPDATE    fiapd000
006118170511     c                   eval      fiapdA = fiapdds
006119170511     c                   exsr      variazione
006120170511     c                   end
006121170511     C                   WHEN      SqlCod = 100
006122170511     C                   WHEN      SqlCod <> 0
006123170511      * Forzo la stampa del JOBLOG.
006124170511     C                   CALL      'X66CHGJOB'
006125170511     C                   ENDSL
006126170511     C                   ENDDO
006127170511     C/EXEC SQL
006128170511     C+ CLOSE b1
006129170511     C/END-EXEC
006200120725     c                   exsr      citta
006300120725     c*
006400120725     c                   exsr      affdef
006500120725     c
006600120725     C                   SETON                                        LR
006601170511     C*------------------------------------------------------------------------*
006602170511     C* richiama pgm variazioni
006603170511     C*------------------------------------------------------------------------*
006604170511     C     variazione    BEGSR
006605170511      * preparazione dati richiamo PGM variazioni per ANNULLAMENTO
006606170511      *richiama pgm registrazione variazioni
006607171206     c                   call      'FNLV36R'
006608170511     c                   parm                    fnlv36ds
006609170511     c                   parm                    fiapdb
006610170511     c                   parm                    fiapda
006611170511     c                   parm                    fiapd4b
006612170511     c                   parm                    fiapd4A
006613170511     c                   ENDsr
006700120725     c*___________________________________________________________________
006800120725     c     citta         begsr
006900120725     C*___________________________________________________________________
007000120725      **  rileggo i codici autisti con i codici variati per agganciare le tariffe
007100120725     C*
007200090422     C/EXEC SQL DECLARE A1 CURSOR FOR
007300120725     C+ select * FROM fiapd00f
007400120726     c+  where apdcsf = :ny1socn and apdksc = :ny1frnnn and apdtip = 'A'
007500120725     C+   order by apdpdr
007600111122     C/END-EXEC
007700111122
007800111122     C*=====
007900120725     C* Segnala l'errore
008000111122     C*=====
008100170511     c                   if        sqlCODE < 0
008200081110     c                   dump(a)
008300081110     c                   return
008400081110     c                   end
008500081110     C/EXEC SQL
008600081110     C+ OPEN A1
008700081110     C/END-EXEC
008800111122     C*=====
008900120725     C* Segnala l'errore
009000111122     C*=====
009100081110     c                   if        sqlCODE < 0
009200081110     c                   dump(a)
009300081110     c                   return
009400081110     c                   end
009500081209      * resto nel ciclo finchè....
009600081110     C                   DOU       SqlCod < 0
009700081110      * lettura cursore
009800081110     C/EXEC SQL
009900170511     C+ Fetch Next From A1 Into :fiapdds
010000081110     C/END-EXEC
010100111122     C*=====
010200111122      *  EoF
010300081110     C                   if        SqlCod = 100
010400081110     c                   Leave
010500111122     C*=====
010600081110      *  x errori
010700120725     C                   elseif    SqlCod < 0
010800081110     c                   dump(a)
010900081110     c                   leave
011000081110     C                   ENDif
011100090428      *
011200120725     C     apdpdr        setll     fitgt01L
011300120725     c                   do        *hival
011400120725     c     apdpdr        reade     fitgt01l
011500120725     c                   if        %eof(fitgt01L)
011600120725     c                   leave
011700120725     c                   endif
011800111205      *
011900120725      * bypasso la tariffa se:
012000120725      * non è convalidata
012100120725      * è annullata
012200120725      * è di simulazione
012300120731      * la data scadenza è inferiore alla data scadenza della società
012400120731      * la data decorrenza è maggiore della data del cambio societario
012500120725     c                   IF        tgtdts = 0 or tgtsml <> ' ' or
012600120725     c                             tgtatb <> *blank or
012700120731     c                             tgtdst < DATAscad or
012800120726     c                             tgtddt >= ny1data
012900120725     c                   iter
013000120725     c                   endif
013100111122      *   Mette in SCADENZA
013200120726     c                   z-add     DATAscad      tgtDST
013300111122     c                   eval        oldPRG = tgtPRG
013400111205      *
013500111122     c                   update    fiTGT000
013600111122      *
013700111122      *   Mette in Decorrenza il nuovo Progressivo
013800111122     c                   eval        tgtPRG = tgtPRG + 1
013900111122     c                   eval        newPRG = tgtPRG
014000120726     c                   movel     ny1data       tgtDDT
014100111122     c                   z-add     20391231      tgtDST
014200111122      *
014300120725     c                   z-add     0             tgtDUV
014400120725     c                   eval        tgtPRU = 'Cnv0812'
014500111122      *
014600111122     c                   z-add     0             TGTPCAR
014700111122     c                   z-add     0             TGTCCKM
014800111130     c                   z-add     *date         TGTDTS
014900111130     c                   z-add     0             TGTDCN
015000111130     c                   z-add     0             TGTDRC
015100120725     c                   z-add     0             TGTnrc
015200120725     c                   z-add     0             TGTfil
015300120725     c                   clear                   tgtflr
015400120726     c                   move      ny1socn       tgtsoc
015500120726     c                   move      ny1frnn       tgtcdf
015600111122      *  quindi scrive il progressivo seguente
015700111122     c                   write     fitgt000
015800120725      *
015900120725     c                   exsr      COPIA_TAR
016000120725      *
016100120725     c                   enddo
016200090428      *----------------------------->>>>>
016300081110     C                   ENDDO
016400081110     C/EXEC SQL
016500081110     C+ Close A1
016600081110     C/END-EXEC
016700120725     C                   endsr
016800090424     C*---------------------------------------------------------------*
016900111122      *  Copia la tariffa
017000090424      *---------------------------------------------------------------*
017100111122     c     COPIA_tar     begSR
017200111129      *
017300120725     c     kfgt          setll     fifgt01l
017400120725     c                   do        *hival
017500120725     c     kfgt          reade     fifgt01l
017600120725     c                   if        %eof(fifgt01l)
017700120725     c                   leave
017800120725     c                   endif
017900120725     c                   if        fgtprg <> oldprg or
018000120725     c                             fgttsr = 'X'     or
018100120725     c                             fgttsr = 'P'     or
018200120725     c                             fgttsr = 'T'
018300120725     c                   iter
018400120725     c                   endif
018500120726      *aggiorna il vecchio
018600120726     c                   move      datascad      fgtdst
018700120726     c                   update    fifgt000
018800120726      *chiama duplica
018900111122     c                   clear                   ficn06ds
019000111123     c                   movel     fgtPDR        FILOLD
019100111123     c                   move      fgtPDR        PDROLD
019200111123     c                   move      fgtTSR        TSROLD
019300111123     c                   z-add     fgtCTR        CTROLD
019400111122     c                   z-add     oldPRG        PRGOLD
019500111123     c                   movel     fgtPDR        FILNEW
019600111123     c                   move      fgtPDR        PDRNEW
019700111122     c                   z-add     newPRG        PRGNEW
019800111122      *
019900111123     c                   eval      kpjbu = ficn06ds
020000111122     c                   call      'FICN06R'
020100111122     c                   parm                    KPJBA
020200120725     c                   enddo
020300111129      *
020400111122     c                   endSR
020500120725     c*___________________________________________________________________
020600120725     c     affdef        begsr
020700120725     C*___________________________________________________________________
020800120725      **  rileggo i codici autisti con i codici variati per agganciare le tariffe
020900120725     C*
021000120726     C/EXEC SQL DECLARE A2 CURSOR FOR
021100120725     C+ select * FROM fiapd00f
021200120726     c+  where apdcsf = :ny1socn and apdksc = :ny1frnnn and apdtip = 'D'
021300120725     C+   order by apdpdr
021400120725     C/END-EXEC
021500120725
021600120725     C*=====
021700120725     C* Segnalare l'errore in qualche modo, p.e. stampando il JOBLOG
021800120725     C*=====
021900120725     c                   if        sqlCODE < 0
022000120725     c                   dump(a)
022100120725     c                   return
022200120725     c                   end
022300120725     C/EXEC SQL
022400120726     C+ OPEN A2
022500120725     C/END-EXEC
022600120725     C*=====
022700120725     C* Segnalare l'errore in qualche modo, p.e. stampando il JOBLOG
022800120725     C*=====
022900120725     c                   if        sqlCODE < 0
023000120725     c                   dump(a)
023100120725     c                   return
023200120725     c                   end
023300120725      * >>>>>>>
023400120725      * resto nel ciclo finchè....
023500120725     C                   DOU       SqlCod < 0
023600120725      * lettura cursore
023700120725     C/EXEC SQL
023800170511     C+ Fetch Next From A2 Into :fiapdds
023900120725     C/END-EXEC
024000120725     C*=====
024100120725      *  EoF
024200120725     C                   if        SqlCod = 100
024300120725     c                   Leave
024400120725     C*=====
024500120725      *  x errori
024600120725     C                   elseif    SqlCod < 0
024700120725     c                   dump(a)
024800120725     c                   leave
024900120725     C                   ENDif
025000120725      *
025100120726     C     apdpdr        setll     fiadt01L
025200120725     c                   do        *hival
025300120726     c     apdpdr        reade     fiadt01l
025400120726     c                   if        %eof(fiadt01L)
025500120725     c                   leave
025600120725     c                   endif
025700160923     c                   clear                   si                1
025800160923     C/EXEC SQL
025900160923     C+  SELECT '1' into :si
026000160923     C+   FROM fitgt00f WHERE tgtatb = ' ' and tgtpdr =:apdpdr and
026100160923     C+   tgtsoc = :ny1socn and tgtcdf = :ny1frnn and tgtddt= :ny1data
026200160923     C+  FETCH first row only
026300160923     C/END-EXEC
026400160923     c                   if        si = '1'
026500160923     c                   iter
026600160923     c                   endif
026700120725      *
026800120725      * bypasso la tariffa se:
026900120725      * non è convalidata
027000120725      * è annullata
027100120731      * la data scadenza è inferiore alla data scadenza della società
027200120731      * la data decorrenza è maggiore della data del cambio societario
027300120725     c                   IF        adtDts = 0 or
027400120725     c                             adtatb <> *blank or
027500120731     c                             adtdst < DATAscad or
027600120726     c                             adtddt >= ny1data
027700120725     c                   iter
027800120725     c                   endif
027900120725      *   Mette in SCADENZA
028000120726     c                   z-add     dATAscad      adtDST
028100120725     c                   eval        oldPRG = adtPRG
028200120725      *
028300120725     c                   update    fiadt000
028400120725      *
028500120725      *   Mette in Decorrenza il nuovo Progressivo
028600120725     c                   eval        adtPRG = adtPRG + 1
028700120725     c                   eval        newPRG = adtPRG
028800120726     c                   movel     ny1data       adtDDT
028900120725     c                   z-add     20391231      adtDST
029000120725      *
029100120725     c                   z-add     0             adtDUV
029200120725     c                   z-add     0             adtPCAR
029300120725     c                   z-add     *date         adtDTS
029400120725     c                   z-add     0             adtDCN
029500120725     c                   z-add     0             adtDRC
029600120725     c                   z-add     0             adtnrc
029700120725     c                   z-add     0             adtfil
029800120725     c                   z-add     0             adtunodc
029900120725     c                   z-add     0             adtdfa
030000120725     c                   clear                   adtflr
030100120726     c                   move      ny1socn       adtcsf
030200120726     c                   move      ny1frnn       adtcdf
030300120725      *  quindi scrive il progressivo seguente
030400120725     c                   write     fiadt000
030500120725      *
030600120725     c                   exsr      COPIA_aff
030700120725      *
030800120725     c                   enddo
030900120725      *----------------------------->>>>>
031000120725     C                   ENDDO
031100120725     C/EXEC SQL
031200120726     C+ Close A2
031300120725     C/END-EXEC
031400120725     C                   endsr
031500120725     C*---------------------------------------------------------------*
031600120725      *  Copia la tariffa affluenza defluenza
031700120725      *---------------------------------------------------------------*
031800120725     c     COPIA_aff     begSR
031900120725      *
032000120725     c     kadd          setll     fiadd01l
032100120725     c                   do        *hival
032200120725     c     kadd          reade     fiadd01l
032300120725     c                   if        %eof(fiadd01l)
032400120725     c                   leave
032500120725     c                   endif
032600120725     c                   if        addatb <> *blank
032700120725     c                   iter
032800120725     c                   endif
032900120725     c                   z-add     newPRG        addprg
033000120725      *
033100120725     c                   write     fiadd000
033200120725     c                   enddo
033300120725      *
033400120725     c                   endSR
