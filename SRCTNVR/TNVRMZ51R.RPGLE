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
001300021031     Fmzamh02l  if   E           K DISK
001400020801     FmzTIM01L  IF   E           K DISK
001500020610     FmzIMP03l  if   E           K DISK    RENAME(MZIMP000:MZIMP03)
001600020610     f                                     prefix(£)
001700020606     fqsysprt   o    f  132        printer oflind(*inof)
001800020606     D ut§dse        E DS                  extname(ut§dse0f)
001900020606     D kpjba         E DS
002000020607     D mzagi         E DS                  extname(mzagi00f)
002100020605     D Ptr             S               *
002200020605     D                                     INZ(%ADDR(agitga))
002300020605     D targa           S             11
002400020605     D                                     BASED(Ptr)
002500020606     d com08           s              8  0
002600020606     d rsut            s             20
002700141202     d targas          s                   like(targa)
002800020606     d kms             s                   like(agikm)
002900020607     d qtas            s                   like(impqta)
003000020607     d arro            s                   like(impqta)
003100020709     d agidtus         s                   like(agidtu)
003200020606     d dataiso         s               d
003300020709     d dataeur         s               d   datfmt(*eur)
003400020605     c* lettura per targa e data rifornimento
003500141202     c     'A'           setll     mzagi01l
003600020605     c                   do        *hival
003700141202     c     'A'           reade     mzagi01l
003800020605     c                   if        %eof(mzagi01l)
003900020605     c                   leave
004000020605     c                   end
004100141219     c                   if        agidtu < 20120101 or
004200141219     c                             agidtu > 20121231
004300141202     c                   iter
004400141202     c                   end
004500020705     c* ROTTURA DI TARGA
004600020705     c                   if        targa <> targas
004700020606     c* aggancio anagrafico per reperire il numero di riferimento auto
004800020605     c                   exsr      srame
004900020606     c                   if        amenrf = 0
005000020801     c     kagi          setgt     mzagi01l
005100020606     c                   iter
005200020605     c                   end
005300020605     c                   movel     targa         targas
005400020606     c                   end
005500020705     C* controllo che non esista già un intervento in quel gg con km diversi
005600020610     c                   setoff                                       23
005700020610     c     kimp2         setll     mzimp03l
005800020610     c                   do        *hival
005900020610     c     kimp2         reade     mzimp03l
006000020610     c                   if        %eof(mzimp03l)
006100020610     c                   leave
006200020610     c                   end
006300020801     C                   MOVEL     *blanks       timuni
006400020801     C     KTIM          CHAIN     mzTIM01L
006500020801     c                   if        £impint = 'CB' or timuni <> 'KM'
006600020610     c                   iter
006700020610     c                   end
006800020610     c                   if        £impkm <> agikm
006900020610     c                   seton                                        23
007000020610     c                   exsr      stampa
007100020610     c                   leave
007200020610     c                   end
007300020610     c                   enddo
007400020722     c                   if        *in23
007500020722     c                   iter
007600020722     c                   end
007700020801     c* carburante agip
007800020801     c                   movel     'CB'          impint
007900020801     c                   exsr      srcarb
008000020801     c                   if        flgiter = *on
008100020801     c                   iter
008200020801     c                   end
008300020801     c* scrittura dell'evento CB carburante
008400020605     c                   z-add     agikm         impkm
008500020705     c                   z-add     agiqta        impqta
008600020605     c                   write     mzimp000
008700141202     c* annullo il record MZAGI00F appena letto per non considerarlo più
008800141202     c                   exsr      srann
008900020605     c                   enddo
009000021025     c* stampa nessuna anomalia se non ci sono stati errori
009100021025     c                   if        not *in90
009200021025     c                   seton                                        24
009300021025     c                   exsr      stampa
009400021025     c                   end
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
014100141202     c                   movel     '*'           agiann
014200020801     c                   except    aggio
014300020801     c                   endsr
014400020606     C**********************************************************************
014500020606     c* aggancio anagrafico automezzi
014600020606     C**********************************************************************
014700020606     C     srame         BEGSR
014800020606     c*
014900020606     c                   z-add     0             amenrf
015000021031     c                   clear                   ameuti
015100020606     c     kame          chain     mzame02l
015200021031     c                   if        not %found(mzame02l)
015300021031     c     kame          chain     mzamh02l
015400021031     c                   if        %found(mzamh02l)
015500021031     c                   z-add     amhnrf        amenrf
015600021031     c                   end
015700141202     c                   else
015800141202     c                   clear                   amenrf
015900021031     c                   end
016000020606     c*
016100020606     C                   ENDSR
016200020606     C**********************************************************************
016300020606     c* stampa errori
016400020606     C**********************************************************************
016500020606     C     stampa        BEGSR
016600021025     c* ind. 90 mi indica se ci sono stati errori
016700021025     c  n24              seton                                        90
016800021025     c   of              except    testa
016900020606     c                   setoff                                       of
017000020709     c                   move      agidtu        dataiso
017100020709     c                   move      dataiso       dataeur
017200020709     c                   move      dataeur       agidtus
017300020709     c   19              except    errfut
017400020709     c   20              except    errpas
017500020709     c   21              except    errdta
017600020709     c   22              except    errana
017700020610     c   23              except    errint
017800021025     c   24              except    fine
017900021025     c                   movea     '0000'        *in(19)
018000020722     c* annulla comunque il record
018100021029     c                   if        not *in24 and not *in22
018200020722     c                   exsr      srann
018300021025     c                   end
018400020606     c*
018500020606     C                   ENDSR
018600020606     C**********************************************************************
018700020606     C     *inzsr        BEGSR
018800020606     C**********************************************************************
018900020605     C     *ENTRY        PLIST
019000020605     C                   PARM                    KPJBA
019100020606     C                   Z-ADD     1             CODUT
019200020606     C                   CALL      'X§PARUT'
019300020606     C                   PARM                    UT§DSE
019400020606     C                   MOVEL     RAGUT         RSUT
019500020606     c                   seton                                        of
019600020605     C* POSIZIONAMENTO TABELLE                             *
019700020605     C     Kimp          KLIST                                                  *
019800020605     C                   KFLD                    amenrf                         *
019900020605     C                   KFLD                    impint                         *
020000020605     C                   KFLD                    impdat                         *
020100020606     C     Kimp1         KLIST                                                  *
020200020606     C                   KFLD                    amenrf                         *
020300020606     C                   KFLD                    impint                         *
020400020610     C     Kimp2         KLIST                                                  *
020500020610     C                   KFLD                    amenrf                         *
020600020610     C                   KFLD                    agidtu                         *
020700020606     C     Kagi          KLIST                                                  *
020800020709     C                   KFLD                    agiann                         *
020900020709     C                   KFLD                    agitga                         *
021000020606     C                   KFLD                    agitgn                         *
021100020606     C     Kame          KLIST                                                  *
021200020606     C                   KFLD                    agitga                         *
021300020606     C                   KFLD                    agitgn                         *
021400020801     C* LETTURA DI     mzTIM000                            *
021500020801     C     KTIM          KLIST                                                  *
021600020801     C                   KFLD                    TIMTIP                         *
021700020801     C                   KFLD                    £impINT                        *
021800020801     C                   KFLD                    ameuti                         *
021900020801     C                   MOVEL     'P'           TIMTIP
022000020605     C                   ENDSR
022100020606     OQSYSPRT   E            TESTA          2 02
022200020606     O                       RSUT                20
022300020606     O                                         + 25 'LISTA ERRORI DURANTE'
022400020606     O                                         +  1 'IL CARICAMENTO DEL F'
022500020606     O                                         +  0 'ILE AGIP'
022600020606     O                                          113 'TRMZ51R'
022700020606     O                       UDATE              127 '  /  /  '
022800020606     O                       PAGE1         Z    132
022900020709     O          e            errfut      1
023000020709     O                                         +  0 'Esiste già una'
023100020801     O                       impint            +  1
023200020801     O                                         +  0 ' successiva con nr.    km '
023300020716     O                                         +  0 '< di qu'
023400020709     O                                         +  0 'elli che si vuole a'
023500021108     O                                         +  0 'ggiungere.'
023600021108     O          e            errfut      1
023700021108     O                                            4 'Rif:'
023800020722     o                       agitga            +  1
023900020722     o                       agitgn            +  1
024000020801     O                                         +  1 'data'
024100020709     o                       agidtus       y   +  1
024200020709     O                                         +  1 'km'
024300020709     o                       agikm         4   +  1
024400021107     O                                         +  1 'qta'
024500021107     o                       agiqta        4   +  1
024600141202     o                       amenrf        4   +  1
024700020709     O          e            errpas      1
024800020705     O                                         +  0 'Esiste già una'
024900020801     O                       impint            +  1
025000020801     O                                         +  0 ' precedente con nr. km '
025100020716     O                                         +  0 '> di qu'
025200020705     O                                         +  0 'elli che si vuole a'
025300021108     O                                         +  0 'ggiungere.'
025400021108     O          e            errpas      1
025500021108     O                                            4 'Rif:'
025600020722     o                       agitga            +  1
025700020722     o                       agitgn            +  1
025800020801     O                                         +  1 'data'
025900020709     o                       agidtus       y   +  1
026000020705     O                                         +  1 'km'
026100020709     o                       agikm         4   +  1
026200021107     O                                         +  1 'qta'
026300021107     o                       agiqta        4   +  1
026400141202     o                       amenrf        4   +  1
026500020709     O          e            errdta      1
026600020709     O                                         +  0 'Esiste già una'
026700020801     O                       impint            +  1
026800020801     O                                         +  0 ' con nr. km o '
026900020709     O                                         +  0 'qta <> da qu'
027000020709     O                                         +  0 'elli che si vuole a'
027100021108     O                                         +  0 'ggiungere.'
027200021108     O          e            errdta      1
027300021108     O                                            4 'Rif:'
027400020722     o                       agitga            +  1
027500020722     o                       agitgn            +  1
027600020801     O                                         +  1 'data'
027700020709     o                       agidtus       y   +  1
027800020709     O                                         +  1 'km'
027900020709     o                       agikm         4   +  1
028000020709     O                                         +  1 'qta'
028100020709     o                       agiqta        4   +  1
028200020606     O          e            errana      1
028300020606     O                                         +  0 'Non esiste in anagrafi'
028400020606     O                                         +  0 'co automezzi la targa'
028500020606     o                       agitga            +  1
028600020606     o                       agitgn            +  1
028700020610     O          e            errint      1
028800020705     O                                         +  1 'Esiste un intervento '
028900020705     O                                         +  0 'con km <> da qu'
029000020705     O                                         +  0 'elli che si vogliono a'
029100021108     O                                         +  0 'ggiungere.'
029200021108     O          e            errint      1
029300021108     O                                            4 'Rif:'
029400020722     o                       agitga            +  1
029500020722     o                       agitgn            +  1
029600020801     O                                         +  1 'data'
029700020709     o                       agidtus       y   +  1
029800020705     O                                         +  1 'km'
029900020709     o                       agikm         4   +  1
030000021107     O                                         +  1 'qta'
030100021107     o                       agiqta        4   +  1
030200021025     O          e            fine        1
030300021025     O                                         + 50 '*** NESSUNA ANOMALIA ***'
030400020709     Omzagi000  E            aggio
030500020709     o                       agiann
