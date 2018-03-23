000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200020606     f********************************************************************
000300020605     F*          passaggio dati da PC ad AS x AGIP         *
000400020606     f********************************************************************
000500020606     f* il pgm legge il file mzagi00f dove sono memorizzati i movimenti
000600020606     f* delle tessere agip non annullati (i record annullati sono i movi-
000700020606     f* menti ricevuti precedentemente), scrive il file MZIMP00F con
000800020801     f* tipo intervento CB=carburante con km e qta.
000900020606     f********************************************************************
001000020605     Fmzagi01l  uf   E           K DISK
001100020606     Fmzimp01l  uf a E           K DISK
001200020605     Fmzame02l  if   E           K DISK
001201021031     Fmzamh02l  if   E           K DISK
001300020801     FmzTIM01L  IF   E           K DISK
001400020610     FmzIMP03l  if   E           K DISK    RENAME(MZIMP000:MZIMP03)
001500020610     f                                     prefix(£)
001600020606     fqsysprt   o    f  132        printer oflind(*inof)
001700020606     D ut§dse        E DS                  extname(ut§dse0f)
001800020606     D kpjba         E DS
001900020607     D mzagi         E DS                  extname(mzagi00f)
002000020605     D Ptr             S               *
002100020605     D                                     INZ(%ADDR(agitga))
002200020605     D targa           S             11
002300020605     D                                     BASED(Ptr)
002400020606     d com08           s              8  0
002500020606     d rsut            s             20
002600020606     d targas          s                   like(targa)
002700020606     d kms             s                   like(agikm)
002800020607     d qtas            s                   like(impqta)
002900020607     d arro            s                   like(impqta)
003000020709     d agidtus         s                   like(agidtu)
003100020606     d dataiso         s               d
003200020709     d dataeur         s               d   datfmt(*eur)
003300020605     c* lettura per targa e data rifornimento
003400020709     c     ' '           setll     mzagi01l
003500020605     c                   do        *hival
003600020709     c     ' '           reade     mzagi01l
003700020605     c                   if        %eof(mzagi01l)
003800020605     c                   leave
003900020605     c                   end
004000020705     c* ROTTURA DI TARGA
004100020705     c                   if        targa <> targas
004200020606     c* aggancio anagrafico per reperire il numero di riferimento auto
004300020605     c                   exsr      srame
004400020606     c                   if        amenrf = 0
004500020606     c                   seton                                        22
004600020801     c                   exsr      stampa
004601021025     c                   movel     *blanks       agiann
004700020801     c     kagi          setgt     mzagi01l
004800020606     c                   iter
004900020605     c                   end
005000020605     c                   movel     targa         targas
005100020606     c                   end
005200020705     C* controllo che non esista già un intervento in quel gg con km diversi
005300020610     c                   setoff                                       23
005400020610     c     kimp2         setll     mzimp03l
005500020610     c                   do        *hival
005600020610     c     kimp2         reade     mzimp03l
005700020610     c                   if        %eof(mzimp03l)
005800020610     c                   leave
005900020610     c                   end
006000020801     C                   MOVEL     *blanks       timuni
006100020801     C     KTIM          CHAIN     mzTIM01L
006200020801     c                   if        £impint = 'CB' or timuni <> 'KM'
006300020610     c                   iter
006400020610     c                   end
006500020610     c                   if        £impkm <> agikm
006600020610     c                   seton                                        23
006700020610     c                   exsr      stampa
006800020610     c                   leave
006900020610     c                   end
007000020610     c                   enddo
007100020722     c                   if        *in23
007200020722     c                   iter
007300020722     c                   end
007400020801     c* controllo congruenza carburante gg prima e gg dopo
007500020801     c* carburante manuale
007600020801     c                   movel     '£1'          impint
007700020801     c                   exsr      srcarb
007800020801     c                   if        flgiter = *on
007900020722     c                   iter
008000020709     c                   end
008100020801     c* carburante agip
008200020801     c                   movel     'CB'          impint
008300020801     c                   exsr      srcarb
008400020801     c                   if        flgiter = *on
008500020801     c                   iter
008600020801     c                   end
008700020801     c* scrittura dell'evento CB carburante
008800020605     c                   z-add     agikm         impkm
008900020705     c                   z-add     agiqta        impqta
009000020605     c                   write     mzimp000
009100020605     c* annullo il record MZAGI00F appena letto per non considerarlo più
009200020722     c                   exsr      srann
009300020605     c*
009400020605     c                   enddo
009401021025     c* stampa nessuna anomalia se non ci sono stati errori
009402021025     c                   if        not *in90
009403021025     c                   seton                                        24
009404021025     c                   exsr      stampa
009405021025     c                   end
009500020605     c                   seton                                        lr
009600020605     C**********************************************************************
009700020801     C** controllo carburante
009800020605     C**********************************************************************
009900020801     C     srcarb        BEGSR
010000020801     c* controllo che i km siano minori del gg successivo
010100020801     c                   movel     *off          flgiter           1
010200020801     c                   z-add     amenrf        impnrf
010300020801     c                   z-add     agidtu        impdat
010400020801     c     kimp          setgt     mzimp01l
010500020801     c     kimp1         readpe    mzimp01l
010600020801     c                   if        not %eof(mzimp01l)
010700020801     c                             and impdat > agidtu and impkm < agikm
010800020801     c                   seton                                        19
010900020801     c                   movel     *on           flgiter
011000020801     c                   exsr      stampa
011100020801     c                   end
011200020801     c* controllo che i km siano maggiori del gg precedente
011300020801     c                   if        flgiter = *off
011400020801     c                   z-add     agidtu        impdat
011500020801     c     kimp          setll     mzimp01l
011600020801     c     kimp1         readpe    mzimp01l
011700020801     c                   if        not %eof(mzimp01l)
011800020801     c                             and impdat < agidtu and impkm > agikm
011900020801     c                   seton                                        20
012000020801     c                   movel     *on           flgiter
012100020801     c                   exsr      stampa
012200020801     c                   end
012300020801     c                   end
012400020801     c* esiste già il gg inserito e i km e le qta sono diverse
012500020801     c                   if        flgiter = *off
012600020801     c                   z-add     agidtu        impdat
012700020801     c     kimp          chain     mzimp01l
012800020801     c                   if        %found(mzimp01l) and
012900020801     c                             (impkm <> agikm or impqta <> agiqta)
013000020801     c                   seton                                        21
013100020801     c                   movel     *on           flgiter
013200020801     c                   exsr      stampa
013300020801     c                   end
013400020801     c                   end
013500020801     c*
013600020722     c                   endsr
013700020801     C**********************************************************************
013800020801     C** annulla il record
013900020801     C**********************************************************************
014000020801     C     srann         BEGSR
014100020801     c                   movel     'A'           agiann
014200020801     c                   except    aggio
014300020801     c                   endsr
014400020606     C**********************************************************************
014500020606     c* aggancio anagrafico automezzi
014600020606     C**********************************************************************
014700020606     C     srame         BEGSR
014800020606     c*
014900020606     c                   z-add     0             amenrf
014901021031     c                   clear                   ameuti
015000020606     c     kame          chain     mzame02l
015001021031     c                   if        not %found(mzame02l)
015002021031     c     kame          chain     mzamh02l
015003021031     c                   if        %found(mzamh02l)
015004021031     c                   z-add     amhnrf        amenrf
015005021031     c                   end
015006021031     c                   end
015100020606     c*
015200020606     C                   ENDSR
015300020606     C**********************************************************************
015400020606     c* stampa errori
015500020606     C**********************************************************************
015600020606     C     stampa        BEGSR
015700021025     c* ind. 90 mi indica se ci sono stati errori
015800021025     c  n24              seton                                        90
015801021025     c   of              except    testa
015900020606     c                   setoff                                       of
016000020709     c                   move      agidtu        dataiso
016100020709     c                   move      dataiso       dataeur
016200020709     c                   move      dataeur       agidtus
016300020709     c   19              except    errfut
016400020709     c   20              except    errpas
016500020709     c   21              except    errdta
016600020709     c   22              except    errana
016700020610     c   23              except    errint
016701021025     c   24              except    fine
016800021025     c                   movea     '0000'        *in(19)
016900020722     c* annulla comunque il record
016901021029     c                   if        not *in24 and not *in22
017000020722     c                   exsr      srann
017001021025     c                   end
017100020606     c*
017200020606     C                   ENDSR
017300020606     C**********************************************************************
017400020606     C     *inzsr        BEGSR
017500020606     C**********************************************************************
017600020605     C     *ENTRY        PLIST
017700020605     C                   PARM                    KPJBA
017800020606     C                   Z-ADD     1             CODUT
017900020606     C                   CALL      'X§PARUT'
018000020606     C                   PARM                    UT§DSE
018100020606     C                   MOVEL     RAGUT         RSUT
018200020606     c                   seton                                        of
018300020605     C* POSIZIONAMENTO TABELLE                             *
018400020605     C     Kimp          KLIST                                                  *
018500020605     C                   KFLD                    amenrf                         *
018600020605     C                   KFLD                    impint                         *
018700020605     C                   KFLD                    impdat                         *
018800020606     C     Kimp1         KLIST                                                  *
018900020606     C                   KFLD                    amenrf                         *
019000020606     C                   KFLD                    impint                         *
019100020610     C     Kimp2         KLIST                                                  *
019200020610     C                   KFLD                    amenrf                         *
019300020610     C                   KFLD                    agidtu                         *
019400020606     C     Kagi          KLIST                                                  *
019500020709     C                   KFLD                    agiann                         *
019600020709     C                   KFLD                    agitga                         *
019700020606     C                   KFLD                    agitgn                         *
019800020606     C     Kame          KLIST                                                  *
019900020606     C                   KFLD                    agitga                         *
020000020606     C                   KFLD                    agitgn                         *
020100020801     C* LETTURA DI     mzTIM000                            *
020200020801     C     KTIM          KLIST                                                  *
020300020801     C                   KFLD                    TIMTIP                         *
020400020801     C                   KFLD                    £impINT                        *
020500020801     C                   KFLD                    ameuti                         *
020600020801     C                   MOVEL     'P'           TIMTIP
020700020605     C                   ENDSR
020800020606     OQSYSPRT   E            TESTA          2 02
020900020606     O                       RSUT                20
021000020606     O                                         + 25 'LISTA ERRORI DURANTE'
021100020606     O                                         +  1 'IL CARICAMENTO DEL F'
021200020606     O                                         +  0 'ILE AGIP'
021300020606     O                                          113 'TRMZ51R'
021400020606     O                       UDATE              127 '  /  /  '
021500020606     O                       PAGE1         Z    132
021600020709     O          e            errfut      1
021700020709     O                                         +  0 'Esiste già una'
021800020801     O                       impint            +  1
021900020801     O                                         +  0 ' successiva con nr.    km '
022000020716     O                                         +  0 '< di qu'
022100020709     O                                         +  0 'elli che si vuole a'
022200021108     O                                         +  0 'ggiungere.'
022201021108     O          e            errfut      1
022202021108     O                                            4 'Rif:'
022300020722     o                       agitga            +  1
022400020722     o                       agitgn            +  1
022500020801     O                                         +  1 'data'
022600020709     o                       agidtus       y   +  1
022700020709     O                                         +  1 'km'
022800020709     o                       agikm         4   +  1
022801021107     O                                         +  1 'qta'
022802021107     o                       agiqta        4   +  1
022900020709     O          e            errpas      1
023000020705     O                                         +  0 'Esiste già una'
023100020801     O                       impint            +  1
023200020801     O                                         +  0 ' precedente con nr. km '
023300020716     O                                         +  0 '> di qu'
023400020705     O                                         +  0 'elli che si vuole a'
023500021108     O                                         +  0 'ggiungere.'
023501021108     O          e            errpas      1
023502021108     O                                            4 'Rif:'
023600020722     o                       agitga            +  1
023700020722     o                       agitgn            +  1
023800020801     O                                         +  1 'data'
023900020709     o                       agidtus       y   +  1
024000020705     O                                         +  1 'km'
024100020709     o                       agikm         4   +  1
024101021107     O                                         +  1 'qta'
024102021107     o                       agiqta        4   +  1
024200020709     O          e            errdta      1
024300020709     O                                         +  0 'Esiste già una'
024400020801     O                       impint            +  1
024500020801     O                                         +  0 ' con nr. km o '
024600020709     O                                         +  0 'qta <> da qu'
024700020709     O                                         +  0 'elli che si vuole a'
024800021108     O                                         +  0 'ggiungere.'
024801021108     O          e            errdta      1
024802021108     O                                            4 'Rif:'
024900020722     o                       agitga            +  1
025000020722     o                       agitgn            +  1
025100020801     O                                         +  1 'data'
025200020709     o                       agidtus       y   +  1
025300020709     O                                         +  1 'km'
025400020709     o                       agikm         4   +  1
025500020709     O                                         +  1 'qta'
025600020709     o                       agiqta        4   +  1
025700020606     O          e            errana      1
025800020606     O                                         +  0 'Non esiste in anagrafi'
025900020606     O                                         +  0 'co automezzi la targa'
026000020606     o                       agitga            +  1
026100020606     o                       agitgn            +  1
026200020610     O          e            errint      1
026300020705     O                                         +  1 'Esiste un intervento '
026400020705     O                                         +  0 'con km <> da qu'
026500020705     O                                         +  0 'elli che si vogliono a'
026600021108     O                                         +  0 'ggiungere.'
026601021108     O          e            errint      1
026602021108     O                                            4 'Rif:'
026700020722     o                       agitga            +  1
026800020722     o                       agitgn            +  1
026900020801     O                                         +  1 'data'
027000020709     o                       agidtus       y   +  1
027100020705     O                                         +  1 'km'
027200020709     o                       agikm         4   +  1
027201021107     O                                         +  1 'qta'
027202021107     o                       agiqta        4   +  1
027203021025     O          e            fine        1
027204021025     O                                         + 50 '*** NESSUNA ANOMALIA ***'
027300020709     Omzagi000  E            aggio
027400020709     o                       agiann
