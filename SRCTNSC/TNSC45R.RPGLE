000100141127     H*
000200981019     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300000000     H*--------------------------------------------------------------*
000400150520     Ftnsc45d   CF   E             WORKSTN
000500150521     Fwfm2k02l  if   e           k disk
000600150521     Fwfm2k03l  if   e           k disk    rename(wfm2k000:wfm2k3)
000700150521     Fazcmm01l  if   e           k disk
000800031105     Fazorg01l  if   e           k disk
000900060921     ftabel00f  if   e           k disk
001000031028     Fprtf198   O    F  198        PRINTER prtctl(prtds)
001100020304     dkpjba          e ds
001200030630     d Tibs36Ds      e ds
001300020404     d prtds           ds
001400020404     d   spab                         3  0
001500020404     d   spaa                         3  0
001600020404     d   skab                         3  0
001700020404     d   skaa                         3  0
001800020404     d   line                         3  0
001900020305     d dataiso         s               d   datfmt(*iso)
002000020305     d dataeur         s               d   datfmt(*eur)
002100030630     d wdatiso         s               d   datfmt(*iso)
002200030903     d wdateur         s               d   datfmt(*eur)
002300040503     D wlbda8          ds
002400040503     D  g02dat                 1      8  0
002500040503     D  g02inv                 9     16  0
002600040503     D  g02err                17     17
002700040503     D  g02tgi                18     22  0
002800060928     D err             S             78    DIM(7) CTDATA PERRCD(1)
002900000000     C*---------------------------------------------------------------*
003000020304      *
003100031105     c                   do        *hival
003200031105     c                   exfmt     video1
003300031105     c                   clear                   $msg
003400150521     c                   setoff                                       919296
003500031105     c   kc              leave
003600031105     c                   exsr      controv1
003700031105     c   96              iter
003800060921     c                   if        *inkf
003900150521     c                   if        v1cage = 0
004000150521      *per po
004100150520     c                   exsr      m2k
004200150521     c                   else
004300150521      *per agente
004400150521     c                   exsr      m2k3
004500150521     c                   endif
004600060921     c                   endif
004700150520      *
004800031105     c                   enddo
004900020308     c                   seton                                        lr
005000990113     c*-------------------------------------------------
005100150520     c     m2k           begsr
005200990113     c*-------------------------------------------------
005300150521     c     v1cfil        setll     wfm2k03l
005400981029     c                   do        *hival
005500150521      * legge tutto
005600150521     c                   if        v1cfil = 0
005700150521     c                   read      wfm2k03l                               98
005800150521     c                   else
005900150521      * solo una filiale
006000150521     c     v1cfil        reade     wfm2k03l                               98
006100150521     c                   endif
006200031105     c   98              leave
006300150520     c                   if        v1cfil <> *zero and
006400150520     c                             m2kpoan <> v1cfil
006500040212     c                   iter
006600040212     c                   endif
006700150521     c                   if        m2kclio <> m2kclin or
006800150521     c                             m2kinmo <> m2kinmn
006900150520     c                   exsr      stampa
007000150521     c                   else
007100150521     c                   iter
007200150521     c                   endif
007300150521      *
007400150520     c                   enddo
007500020304
007600020304     c                   endsr
007700981029      *
007800150521     c*-------------------------------------------------
007900150521     c     m2k3          begsr
008000150521     c*-------------------------------------------------
008100150521     c     v1cage        setll     wfm2k02l
008200150521     c                   do        *hival
008300150521     c     v1cage        reade     wfm2k02l                               98
008400150521     c   98              leave
008500150521     c                   exsr      stampa
008600150521     c                   if        m2kclio <> m2kclin or
008700150521     c                             m2kinmo <> m2kinmn
008800150521     c                   exsr      stampa
008900150521     c                   else
009000150521     c                   iter
009100150521     c                   endif
009200150521     c                   enddo
009300150521      *
009400150521     c                   endsr
009500150521      *
009600040503      *------------------------------------------------------
009700040503     c     stampa        begsr
009800040503      *------------------------------------------------------
009900150521     c                   if        line > 60
010000150520     c                   move      m2kdta        dataiso
010100150520     c                   move      dataiso       dataeur
010200150520     c                   move      dataeur       m2kdtas           8 0
010300150521      *decodifica area
010400150521     C                   movel     '05'          kcod
010500150521     C                   movel(p)  M2KARAN       kkey
010600150521     C     ktab          chain     tabel00f
010700150521     c                   if        %found(tabel00f) and tblflg = *blank
010800150521     C                   movel     tbluni        cared            25
010900150521     C                   end
011000150521      *decodifica distretto
011100150521     C                   movel     '17'          kcod
011200150521     C                   movel(p)  M2KDITN       kkey
011300150521     C     ktab          chain     tabel00f
011400150521     c                   if        %found(tabel00f) and tblflg = *blank
011500150521     C                   movel     tbluni        dird             25
011600150521     C                   end
011700150521      * stampa testata
011800040503     c                   except    testata
011900040503     c                   endif
012000150521      * stampa testata
012100150521     c                   if        m2krgfn <> exage
012200150521     c                   clear                   exaged
012300150521     c                   movel     m2krgfn       exage             7 0
012400150521     c     exage         chain     azcmm01l
012500150521     c                   if        %found(azcmm01l)
012600150521     c                   movel     cmmdes        exaged           25
012700150521     c                   endif
012800150521     c                   except    testata2
012900150521     c                   endif
013000040503     c                   except    detta
013100040503     c                   endsr
013200031105     c*-------------------------------------------------
013300031105     c     controv1      begsr
013400031105     c*-------------------------------------------------
013500031105     c                   clear                   v1dfil
013600031105     c                   if        v1cfil > 0
013700031105     c     v1cfil        chain     azorg01l
013800031105     c                   if        %found(azorg01l)
013900031105     c                   movel     orgdes        v1dfil
014000031105     c                   else
014100150521     c                   seton                                        9296
014200031105     c                   endif
014300031105     c                   endif
014400060921      * x sede parzializza area o distretto
014500150521     c                   clear                   v1dage
014600150521     c                   if        v1cage > 0
014700150521     c     v1cage        chain     azcmm01l
014800150521     c                   if        %found(azcmm01l)
014900150521     c                   movel     cmmdes        v1dage
015000150521     c                   else
015100150521     c                   movel     *all'?'       v1dage
015200150521     c                   seton                                        9196
015300150521     c                   endif
015400150521     c                   endif
015500060921     C*
015600040503     c
015700060928     c     endrut        endsr
015800011026     c*-------------------------------------------------
015900011026     c     *inzsr        begsr
016000011026     c*-------------------------------------------------
016100020304     c     *entry        plist
016200020304     c                   parm                    kpjba
016300011026
016400060921     c     ktab          klist
016500060921     c                   kfld                    kkut              1 0
016600060921     c                   kfld                    kcod              2
016700060921     c                   kfld                    kkey              8
016800060921     c                   z-add     1             kkut
016900060921     c                   move      '05'          kcod
017000060921
017100060928     c     korg          klist
017200060928     c                   kfld                    o36dis
017300060928     c                   kfld                    areak             3 0
017400060928
017500020218     C                   time                    w0120            14 0
017600020218     C                   move      w0120         wdat              8 0
017700030630     c                   move      wdat          dataeur
017800030630     c                   move      dataeur       dataiso
017900030630     c                   move      dataiso       wdatiso
018000030630
018100150521     c                   move      *all'_'       linea198        198
018200020404     c                   z-add     99            line
018300030630      *profilo di filiale estraggo solo quelli relativi al P.O.
018400030630     C                   CLEAR                   Tibs36Ds
018500030630     C                   EVAL      I36ute = knmus
018600030630     C                   CALL      'TIBS36R'
018700030630     C                   PARM                    tibs36ds
018800060928     C                   movel     o36car        o36cara           3
018900031105     c                   move      o36pou        v1cfil
019000031105     c     v1cfil        chain     azorg01l
019100031105     c                   if        %found(azorg01l)
019200031105     c                   movel     orgdes        v1dfil
019300031105     c                   endif
019400011026     c                   endsr
019500020404     C*--------------------------------------------------------------------
019600031028     Oprtf198   E            testata        1 02
019700150520     o                                           10 'TNSC45R'
019800020404     o                       knmus               22
019900150521     o                                          110 'Lista indirizzi recapito -
020000150520     o                                              C/assegni '
020100150521     o                                          140 'Situazione al '
020200150520     o                       m2kdtas       y  +   1
020300150520     o                       udate         y  +   5
020400020404     O                       page          z  +   1
020500020404     O          E            testata     1  1
020600150520     o                                           50 'Anagrafica attuale'
020700150520     o                                          150 'Anagrafica precedente'
020800150521     O          E            testata     1  1
020900150521     o                                           10 'Divisione'
021000150521     o                       m2kditn             12
021100150521     o                       dird                45
021200150521     o                                           55 'Area'
021300150521     o                       M2KARAN             60
021400150521     o                       cared               90
021500150521     O          E            testata     1
021600150521     o                                            8 'Cliente'
021700150521     o                                           24 'Ragione sociale'
021800150521     o                                           47 'Tp'
021900150521     o                                           51 'Ter'
022000150521     o                                           99 '|'
022100150521     o                                          108 'Cliente'
022200150521     o                                          124 'Ragione sociale'
022300150521     o                                          147 'Tp'
022400150521     o                                          151 'Ter'
022500150521     O          E            testata     0
022600150521     o                                            8 'Cliente'
022700150521     o                                           24 'Ragione sociale'
022800150521     o                                           47 'Tp'
022900150521     o                                           51 'Ter'
023000150521     o                                           99 '|'
023100150521     o                                          108 'Cliente'
023200150521     o                                          124 'Ragione sociale'
023300150521     o                                          147 'Tp'
023400150521     o                                          151 'Ter'
023500150521     O          E            testata     0
023600150521     o                                            8 'Cliente'
023700150521     o                                           24 'Ragione sociale'
023800150521     o                                           47 'Tp'
023900150521     o                                           51 'Ter'
024000150521     o                                           99 '|'
024100150521     o                                          108 'Cliente'
024200150521     o                                          124 'Ragione sociale'
024300150521     o                                          147 'Tp'
024400150521     o                                          151 'Ter'
024500150521     O          E            testata     1
024600150521     o                       linea198           198
024700150521     O          E            testata2    2
024800150521     o                                            7 'Agente'
024900150521     o                       exage               15
025000150521     o                       exaged              50
025100150521     O          E            testata2    0
025200150521     o                                            7 'Agente'
025300150521     o                       exage               15
025400150521     o                       exaged              50
025500150521     O          E            testata2    0
025600150521     o                                            7 'Agente'
025700150521     o                       exage               15
025800150521     o                       exaged              50
025900020404     O          E            detta       1
026000150521     o                       M2KCLIn              8
026100150521     o                       M2KRSMn             44
026200150521     o                       M2KTPAn             47
026300150521     o                       M2KTFAn             51
026400150521     o                       M2Knspn       z     62
026500150520     o                                           99 '|'
026600150521     o                       M2KCLIO            108
026700150521     o                       M2KRSMO            144
026800150521     o                       M2KTPAO            147
026900150521     o                       M2KTFAO            151
027000150521     o                       M2Knspo       z    162
027100150520     O          E            detta       1
027200150520     o                       m2kpoan              3
027300150520     o                       m2kzscn              6
027400150520     o                       m2kinmn             42
027500150520     o                       M2Klomn             78
027600150520     o                       M2KCamn             87
027700150520     o                       M2Kprmn             90
027800150520     o                                           99 '|'
027900150521     o                       m2kpoao            103
028000150521     o                       m2kzsco            106
028100150521     o                       m2kinmo            142
028200150521     o                       M2Klomo            178
028300150521     o                       M2KCamo            187
028400150521     o                       M2Kprmo            190
028500060928      *___________________________________________________________________
028600060928** ERR
028700060928Profilo di area non può selezionare distretto                                  1
028800060928Il distretto richiesto è incongruente con il profilo                           2
028900060928Il P.O richiesto non appartiene all'area di competenza                         3
029000060928Il P.O richiesto non appartiene al distretto di competenza                     4
029100060928Selezionare solo uno fra P.O / Area / Distretto                                5
029200060928L'area non è di competenza al profilo
029300060928L'area selezionata non appartiene al distretto di competenza
