000100980904     H*------------------------------------------------------------------------*
000200980904     H DECEDIT('0,') DATEDIT(*DMY.)
000300980904     F*------------------------------------------------------------------------*
000400980904     F* DATA BASE
000500980904     F*------------------------------------------------------------------------*
000600150105     FTITAS38C  IF   E           K DISK
000700150108     Ffiar531c  IF   E           K DISK
000800020410     FPRTF198   O    F  198        PRINTER
000900150105     Fqsysprt   O    F  132        PRINTER
001000150108     D skerr           S              4  0 DIM(10000)
001100150108     D skerr_c         S              7  0 DIM(10000)
001200150107     d
001300150107     D UNisi           S              4  0 DIM(10000)
001400150108     D unino           S             11  0 DIM(10000)
001500150108     D unino_O         S             11  0 DIM(10000)
001600150107     D unisi_c         S              7  0 DIM(10000)
001700150107     D unino_c         S              7  0 DIM(10000)
001800150109     D ccm             S              7  0 DIM(10000)
001900150109     D ccm_c           S              7  0 DIM(10000)
002000150108     d*
002100150108     D
002200150108     D ranKS           S              7  0 DIM(10000)
002300150108     D ranno           S              8  0 DIM(10000)
002400150108     D ranno_c         S              7  0 DIM(10000)
002500150108     D ranKS_c         S              7  0 DIM(10000)
002600150105     D wrkoggiiso      s               d
002700150105     D a               s                   like(k)
002800150105     D b               s                   like(k)
002900150105     D c               s                   like(k)
003000150105     D d               s                   like(k)
003100150105     D e               s                   like(k)
003200150105     D f               s                   like(k)
003300150105     D g               s                   like(k)
003400150105     D h               s                   like(k)
003500150105     D i               s                   like(k)
003600150108     D Y               s                   like(k)
003700150105     D KPJBA         E DS
003800150105     D TRULORSDS     E DS
003900150105     D TRULOR2DS     E DS
004000150108     D Dar5gen       E DS
004100150107     d winizio         s               t   timfmt(*iso)
004200150107     d wfine           s               t   timfmt(*iso)
004300980904     C*------------------------------------------------------------------------*
004400980904     C* MAIN LINE
004500980904     C*------------------------------------------------------------------------*
004600150105     c     *entry        plist
004700150105     c                   parm                    kpjba
004800150105     c                   movel     kpjbu         par_date         16
004900150105     c                   movel     par_date      data_dal          8
005000150105     c                   move      par_date      data_al           8
005100150105     c                   clear                   kdal
005200150105     c                   clear                   kal
005300150105     c                   if        data_dal>*zeros and data_al>*zeros
005400150105     c                   movel     data_dal      kdal              8 0
005500150105     c                   movel     data_al       kal               8 0
005600150105     c                   endif
005700150109     c                   clear                   wtas              1
005800150109     c                   clear                   insbrt            1
005900150109     c* alla 17a posizione  flag se controllo HCR di ar5 o sempre TASHCR
006000150109     c                   eval      wtas = %subst(kpjbu:17:1)
006100150109     c* alla 18a posizione  flag se uso orari min max o STD
006200150109     c                   clear                   wtipoor           1
006300150109     c                   eval      wtipoOr= %subst(kpjbu:18:1)
006400150109
006500150109     c                   if        wtipoor='M'
006600150112     c                   seton                                        21
006700150109     c                   endif
006800150109     c
006900150108     c     kar5G         klist
007000150108     c                   kfld                    tasaas
007100150108     c                   kfld                    taslnp
007200150108     c                   kfld                    tasnrs
007300150108     c                   kfld                    tasnsp
007400150108     c                   kfld                    ktrd              3
007500150108     c                   eval      KTRD='GEN'
007600150105     c
007700150105     c* Se le date sono a zero imposto mese precedente
007800150105     c                   if        kdal    =0
007900150105     c                   endif
008000150105     c
008100150105     c                   clear                   contaok
008200150105     c                   clear                   contanook
008300150108     c                   clear                   contarange
008400980904     C*
008500980904     C* POSIZIONAMENTO E PRIMA LETTURA BOLLE
008600980904     C                   EXSR      SETFIL
008700980904     C*
008800980904     C* CICLO FINO A FINE FILE
008900980904DO  1C     $FINE         DOWEQ     'N'
009000980904     C*
009100150105     c                   exsr      Elabora
009200980904     C*
009300980904     C* LETTURA SUCCESSIVA BOLLE
009400980904     C                   EXSR      LETFIL
009500980904E   1C                   ENDDO                                                  *FINE FILE
009600150129
009700150129     c                   except    totb
009800980904     C*
009900150108     c* 1) stampa totali finali
010000150108     c                   clear                   totale           13 0
010100150108     c                   clear                   percok            3 0
010200150108     c                   clear                   percnook          3 0
010300150108     c                   clear                   percrange         3 0
010400150109     c                   clear                   percccm           3 0
010500150108     c                   eval      totale=contaok+contanook+contarange
010600150108     c                   eval(h)   percok=(contaok*100)/totale
010700150108     c                   eval(h)   percnook=(contanook*100)/totale
010800150108     c                   eval(h)   percrange=(contarange*100)/totale
010900150108     c* stampo
011000150108     c                   except    stot
011100150108     c
011200150108     c* 2) range per presenti nelle bolle
011300150108     c*
011400150105     c                   z-add     1             k
011500150108     c                   dow       ranno(k)>0
011600150105     c                   eval      a=k+1
011700150105     c                   eval      b=k+2
011800150105     c                   eval      c=k+3
011900150105     c                   eval      d=k+4
012000150108     c                   except    sran
012100150108     c                   add       5             k
012200150105     c                   enddo
012300020410     C*
012400150108     c* 3) range per presenti nelle bolle per cliente
012500150108     c*
012600150108     c                   z-add     1             k
012700150108     c                   dow       ranks(k)>0
012800150108     c                   eval      a=k+1
012900150108     c                   eval      b=k+2
013000150108     c                   eval      c=k+3
013100150108     c                   eval      d=k+4
013200150108     c                   eval      e=k+5
013300150108     c                   except    sranks
013400150108     c                   add       6             k
013500150108     c                   enddo
013600150108     c* 3) Stampo gli orari fuori orari servizi per cliente
013700150109     c                   clear                   totclip
013800150109     c                   clear                   totcliG
013900150109     c                   clear                   savccm            7 0
014000150108     c                   clear                   totccm
014100150108     c                   movea     unino         unino_o
014200150108     c                   sorta     unino_o
014300150108     c                   eval      k=1
014400150108     c
014500150108     c                   dow       k<=10000
014600150108     c                   if        unino_o(K)>0
014700150108     c                   movel     unino_o(K)    k_ccm             7 0
014800150108     c                   if        k_ccm<>savccm and savccm>0
014900150109     c* stampo se totale > 9
015000150109     c                   if        totccm>9
015100150109     c                   z-add     1             y
015200150109     c     savccm        lookup    ccm(y)                                 30
015300150109     c                   clear                   percccm
015400150109     c                   eval      percccm=(totccm*100)/ccm_c(Y)
015500150109     c
015600150108     c                   except    sfuori
015700150109     c                   add       1             totcliG           7 0
015800150109     c                   else
015900150109     c                   add       1             totcliP           7 0
016000150109     c                   endif
016100150108     c                   clear                   totccm           11 0
016200150108     c                   endif
016300150108     c                   eval      savccm=k_ccm
016400150108     c
016500150108     c                   eval      Y=1
016600150108     c     unino_o(K)    lookup    unino(Y)                               31
016700150109     c                   if        *in31=*on
016800150108     c                   add       unino_C(Y)    totccm
016900150108     c                   endif
017000150108     c                   endif
017100150108     c
017200150108     c                   add       1             k
017300150108     c                   enddo
017400150108     c
017500150109     c                   if        savccm>0 and totccm>9
017600150109     c                   z-add     1             y
017700150109     c     savccm        lookup    ccm(y)                                 30
017800150109     c                   clear                   percccm
017900150109     c                   eval      percccm=(totccm*100)/ccm_c(Y)
018000150109
018100150108     c                   except    sfuori
018200150108     c                   endif
018300150109
018400150109     c                   except    sfuoriT
018500150108     c
018600150108     c* 4) stampo errori
018700150108     c                   z-add     1             k
018800150108     c                   dow       skerr(k)>0
018900150108     c                   eval      a=k+1
019000150108     c                   eval      b=k+2
019100150108     c                   eval      c=k+3
019200150108     c                   except    serr
019300150108     c                   add       4             k
019400150108     c                   enddo
019500150108     c
019600980904     C                   SETON                                        LR
019700980904     C*------------------------------------------------------------------------*
019800980904     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA                          -BOLLE-
019900980904     C*------------------------------------------------------------------------*
020000980904     C     SETFIL        BEGSR
020100980904     C*
020200150105     C                   movel     kdal          KASAAS                         *ANNO INIZIALE
020300150105     C                   Z-ADD     kdal          KASMGS                         *MM/GG INIZIALE
020400990802     C     KEYTAS        SETLL     TITAS38C                           99        *FINE ARCHIVIO
020500980904IF  1C     *IN99         IFEQ      *ON
020600980904     C                   MOVEL     'S'           $FINE                          *FINE PGM
020700980904X   1C                   ELSE
020800980904     C                   EXSR      LETFIL                                       *LETTURA RECORD
020900980904E   1C                   ENDIF
021000980904     C*
021100980904     C                   ENDSR
021200980904     C*------------------------------------------------------------------------*
021300980904     C* LETFIL - LETTURA PROSSIMO RECORD                                 -BOLLE-
021400980904     C*------------------------------------------------------------------------*
021500980904     C     LETFIL        BEGSR
021600980904     C*
021700980904     C* LETTURA FINO A:
021800980904     C*  - FINE FILE
021900980904     C*  - TROVATO RECORD VALIDO
022000980904     C                   MOVEL     'N'           WRECOK
022100980904DO  1C     $FINE         DOWEQ     'N'
022200980904     C     WRECOK        ANDEQ     'N'
022300020422     C                   SETOFF                                       010203
022400990802     C                   READ      TITAS38C                               99
022500980904IF  2C     *IN99         IFEQ      *ON                                          *FINE ARCHIVIO
022600980904     C                   MOVEL     'S'           $FINE
022700980904X   2C                   ELSE
022800980904     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
022900980904E   2C                   ENDIF
023000980904E   1C                   ENDDO
023100980904     C*
023200980904     C                   ENDSR
023300980904     C*------------------------------------------------------------------------*
023400980904     C* CHKREC - CONTROLLA VALIDITA' RECORD                              -BOLLE-
023500980904     C*------------------------------------------------------------------------*
023600980904     C     CHKREC        BEGSR
023700020410     C*
023800020410     C                   MOVEL     'S'           WRECOK                         *RECORD VALIDO
023900020403     C*
024000020403     C* PERIODO OLTRE IL RICHIESTO, ESCE
024100150105     c                   z-add     tasmgs        wdata             8 0
024200150105     c                   movel     tasaas        wdata
024300150105     C                   IF        wdata>kal
024400020403     C                   MOVEL     'S'           $FINE
024500020410     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
024600150105     c                   leavesr
024700020403     C                   ENDIF
024800150105     c* escludo bolle senza orario
024900150109     c                   if        tashcr=0 and tasdcr=0
025000150105     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
025100150105     c                   leavesr
025200150105     C                   ENDIF
025300150109
025400150109     c                   if        tasccm=0
025500150109     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
025600150109     c                   leavesr
025700150109     C                   ENDIF
025800020410     C*
025900150105     c* Escludo bolle di recupero
026000150105     c                   if        tasctm='4 ' or tasctm='4F'
026100020410     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
026200150105     c                   leavesr
026300020410     C                   ENDIF
026400150105     c* solo disk A B C
026500150105     c                   if        tasnrs>0 or tassop='%' or
026600150105     c                             tassop='§' or tassop=':'
026700150105     c                   else
026800150105     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
026900150105     c                   leavesr
027000150105     C                   ENDIF
027100980904     C*
027200980904     C                   ENDSR
027300150105     C*------------------------------------------------------------------------*
027400150105     C* elabora
027500150105     C*------------------------------------------------------------------------*
027600150105     C     elabora       BEGSR
027700150108     c                   movel     'N'           wbolrange         1
027800150109     c
027900150109     c     kar5G         chain     fiar531c
028000150109     c                   clear                   dar5gen
028100150109     c                   if        %found(fiar531c)
028200150109     c                   eval      dar5gen=ar5uni
028300150109     c                   endif
028400150109     c
028500150109     c                   eval      insbrt='N'
028600150109     c                   if        §AR5MDP='S' and §ar5hcr<>%editc(tashcr:'X')
028700150109     c                   eval      insbrt='S'
028800150109     c                   endif
028900150109     c
029000150109     c                   if        wtas<>'S'
029100150109     c                   if        §AR5MDP='S'
029200150109     c                   if        §ar5hcr>*zeros
029300150109     c                   movel     §ar5hcr       tashcr
029400150109     c                   else
029500150109     c                   clear                   tashcr
029600150109     c                   endif
029700150109     c                   endif
029800150109     c                   endif
029900150109
030000150109     c                   if         tashcr>0
030100150109     c                   movel     tashcr        HH                2 0
030200150109     c                   move      tashcr        MM                2 0
030300150129     c                   add       1             bolle             9 0
030400150129     c
030500150129     c                   if        tashcr>=0800 and tashcr<=2000
030600150129     c                   add       1             bolleok           9 0
030700150129     c                   endif
030800150105     C*
030900150105     c* richiamo TRULORSR
031000150105     c                   clear                   trulorSds
031100150105     c                   clear                   trulor2ds
031200150105     c                   eval       IOREtfp = tastfp
031300150105     c                   eval       IOREtfa = tastfa
031400150105     c                   eval       IOREdcr = tasdcr
031500150105     c                   eval       IOREhcr = tashcr
031600150105     c                   eval       IOREtcr = tastcr
031700150105     c                   eval       IOREfil = taslna
031800150105     c                   eval       IOREcap = tascad
031900150105     c                   eval       IOREloc = taslod
032000150105     c                   eval       IOREnar = tasnzd
032100150105     c                   eval       iorehti=9999
032200150105     c* come riferimento prendo sempre la data di oggi
032300150105     C                   time                    wrkoggiiso
032400150105     C     *iso          move      wrkoggiiso    ioredta
032500150105     c
032600150105     c                   eval       IOREtser = 'C'
032700150105     c                   call      'TRULORSR'
032800150105     c                   parm                    kpjba
032900150105     c                   parm                    trulorSds
033000150105     c                   parm                    trulor2ds
033100150108     c*
033200150108     c                   move      tasccm        w004a             4
033300150109     c                   if        wtipoor ='M'
033400150109     c                   eval      OOR2STIS=OOR2MIIS
033500150109     c                   eval      OOR2STFS=OOR2MxfS
033600150109     c                   endif
033700150105     c*
033800150108     c* Se i minuti dell'orario sono compresi tra le ore 10,00 e le ore 20,00
033900150105     c* è probabile che si tratti di un range
034000150108     c* escludo o codici 8888 e 9999 come mittente
034100150108    1c                   if        mm< 10 or mm>20 or
034200150108     c                             mm<hh or w004a='9999' or w004a='8888'
034300150108     c                             or w004a='0000' or hh=0   or
034400150112     c                             (insbrt='S' and wtas='S')
034500150109
034600150109     c                   eval      k=1
034700150109     c     tasccm        lookup    ccm(k)                                 30
034800150109     c                   if        not *in30
034900150109     c                   z-add     1             k                 5 0
035000150109     c     0000000       lookup    ccm(K)                                 30
035100150109     c   30              z-add     tasccm        ccm(k)
035200150109     c                   endif
035300150109     c                   add       1             ccm_c(K)
035400150105     c
035500150105    2c                   if        tashcr>=OOR2STIS  and
035600150105     c                              tashcr<=OOR2STFS
035700150105     c                   add       1             contaok           9 0
035800150109     c                   If        insbrt='S'
035900150108     c                   add       1             noiok             9 0
036000150108     c                   endif
036100150107
036200150107     c                   eval      k=1
036300150107     c     tashcr        lookup    unisi(k)                               30
036400150107     c                   if        not *in30
036500150107     c                   z-add     1             k                 5 0
036600150107     c     0000          lookup    unisi(K)                               30
036700150107     c   30              z-add     tashcr        unisi(k)
036800150107     c                   endif
036900150107     c                   add       1             unisi_c(K)
037000150107     c
037100150105     c                   else
037200150105     c                   add       1             contaNOOK         9 0
037300150109     c                   If        insbrt='S'
037400150108     c                   add       1             noinook           9 0
037500150108     c                   endif
037600150107
037700150107     c                   eval      k=1
037800150108     c                   z-add     tashcr        w0110            11 0
037900150108     c                   movel     tasccm        w0110
038000150108     c     w0110         lookup    unino(k)                               30
038100150107     c                   if        not *in30
038200150107     c                   z-add     1             k                 5 0
038300150108     c     00000000000   lookup    unino(K)                               30
038400150108     c   30              z-add     w0110         unino(k)
038500150108     c                   except    errtre
038600150107     c                   endif
038700150107     c                   add       1             unino_c(K)
038800150108     c**                 except    erruno
038900150105    2c                   endif
039000150105     c
039100150105   x1c                   else
039200150105     c* RANGE
039300150108     c                   movel     'S'           wbolrange         1
039400150108     c                   add       1             CONTARANGE        9 0
039500150109     c                   If        insbrt='S'
039600150108     c                   add       1             noirange          9 0
039700150108     c                   endif
039800150105     c                   clear                   or_inizio         4 0
039900150105     c                   movel     hh            or_inizio
040000150105     c                   clear                   or_fine           4 0
040100150105     c                   movel     mm            or_fine
040200150108     c*
040300150107     c                   z-add     or_fine       wrange            8 0
040400150107     c                   movel     or_inizio     wrange            8 0
040500150108     c
040600150108     c* Per ora i range li tratto sempre come errore
040700150108     c* memorizzo come range
040800150108     c                   eval      k=1
040900150108     c     wrange        lookup    ranno(k)                               30
041000150108
041100150108     c                   if        not *in30
041200150108     c                   z-add     1             k                 5 0
041300150108     c     00000000      lookup    ranno(K)                               30
041400150108     c   30              z-add     wrange        ranno(k)
041500150108     c                   endif
041600150108     c                   add       1             ranno_c(K)
041700150108     c* memorizzo per ksc
041800150108     c                   eval      k=1
041900150108     c     tasccm        lookup    ranks(k)                               30
042000150108
042100150108     c                   if        not *in30
042200150108     c                   z-add     1             k                 5 0
042300150108     c     0000000       lookup    ranks(K)                               30
042400150108     c   30              z-add     tasccm        ranks(k)
042500150108     c                   endif
042600150108     c                   add       1             ranks_c(K)
042700150108     c
042800150108    1c                   endif
042900150107     c*
043000150108     c**                 if        or_inizio<OOR2STIS
043100150108     c**                 eval      winizio=%time(OOR2STIs*100)
043200150108     c**                 else
043300150108     c**                 eval      winizio=%time(or_inizio*100)
043400150108     c**                 endif
043500150108     c**                 if        or_fine  >OOR2STfS
043600150108     c**                 eval      wfine  =%time(OOR2STfS*100)
043700150108     c**                 else
043800150108     c**                 eval      wfine  =%time(or_fine*100)
043900150108     c**                 endif
044000150108     c**
044100150108     c**
044200150108    2c**                 if        or_inizio>=OOR2STIS  and
044300150108     c**                           or_fine<=OOR2STFS
044400150108     c**                 add       1             rangeok           9 0
044500150108     c**
044600150108     c**                 eval      k=1
044700150108     c**   wrange        lookup    ransi(k)                               30
044800150108     c**
044900150108     c**                 if        not *in30
045000150108     c**                 z-add     1             k                 5 0
045100150108     c**   00000000      lookup    ransi(K)                               30
045200150108     c** 30              z-add     wrange        ransi(k)
045300150108     c**                 endif
045400150108     c**                 add       1             ransi_c(K)
045500150108     c**
045600150108     c**                 else
045700150108     c**                 add       1             rangeNOOK         9 0
045800150108     c**
045900150108     c**                 eval      k=1
046000150108     c**   wrange        lookup    ranno(k)                               30
046100150108     c**
046200150108     c**                 if        not *in30
046300150108     c**                 z-add     1             k                 5 0
046400150108     c**   00000000      lookup    ranno(K)                               30
046500150108     c** 30              z-add     wrange        ranno(k)
046600150108     c**                 endif
046700150108     c**                 add       1             ranno_c(K)
046800150108     c**                 except    errdue
046900150108    2c**                 endif
047000150108     c**
047100150108     c**                 z-add     1             k                 5 0
047200150109     c**   wrange        lookup    skra(K)                                30
047300150105
047400150107     c**                 if        not *in30
047500150107     c**                 z-add     1             k                 5 0
047600150107     c**   00000000      lookup    skra(K)                                30
047700150107     c** 30              z-add     wrange        skra(k)
047800150107     c**                 endif
047900150108    1c**                 endif
048000150108     C
048100150105     c* memorizzo gli orari errati
048200150105     c                   clear                   memoerr           1
048300150108     c                   if        hh<7  or hh>17
048400150105     c                   eval      memoerr='S'
048500150105     c                   endif
048600150108     C
048700150108    1c                   if        wbolrange='N'
048800150105     c                   if        mm<>00 and mm<>15 and mm<>30 and
048900150105     c                             mm<>45
049000150105     c                   eval      memoerr='S'
049100150105     c                   endif
049200150105     c                   endif
049300150105     c
049400150105     c                   if        memoerr='S'
049500150108     c                   eval      k=1
049600150109     c     tashcr        lookup    skerr(K)                               30
049700150105
049800150105     c                   if        not *in30
049900150105     c                   z-add     1             k                 5 0
050000150105     c     0000          lookup    skerr(K)                               30
050100150107     c   30              z-add     tashcr        skerr(k)
050200150105     c                   endif
050300150108     c                   add       1             skerr_c(K)
050400150105     c                   endif
050500150105     c
050600150109     c                   endif
050700150109
050800150105     C                   ENDSR
050900980904     C*------------------------------------------------------------------------*
051000980904     C* *INZSR - OPERAZIONI INIZIALI
051100980904     C*------------------------------------------------------------------------*
051200980904     C     *INZSR        BEGSR
051300980904     C*
051400980904     C* VARIABILI RIFERITE AL DATABASE
051500990802     C     *LIKE         DEFINE    TASAAS        KASAAS                         *TITAS00F
051600981215     C     *LIKE         DEFINE    TASMGS        KASMGS
051700020410     C*
051800020410     C* VARIABILI DI WRK
051900020410     C                   MOVEL     'N'           $FINE             1            *FINE PGM
052000020410     C                   MOVEL     'S'           WRECOK            1            *VALIDITA' RECORD
052100020410     C                   Z-ADD     *zeros        COUNTMOD          6 0          *CONTA BOLLE MODIF.
052200980904     C*
052300980904     C* CHAIVI DI LETTURA
052400981215     C     KEYTAS        KLIST                                                  *WFREP00F
052500981215     C                   KFLD                    KASAAS                          -ANNO
052600981215     C                   KFLD                    KASMGS                          -MESE/GIORNO
052700980904     C*
052800980904     C                   ENDSR
052900020410
053000150105     Oqsysprt   E            erruno      1
053100150105     O                       tasaas
053200150105     O                       taslnp        z  +   1
053300150105     O                       tasnrs        z  +   1
053400150105     O                       tasnsp        z  +   1
053500150108     O                       tasccm           +   1
053600150108     O                       taslod           +   2
053700150105     O                       tasprd           +   1
053800150105     O                                        +   1 'ORARI SERVIZI'
053900150105     O                       OOR2STIS         +   1 '  :  '
054000150105     O                       OOR2STFS         +   1 '  :  '
054100150105     O                                        +   1 'bolla'
054200150105     O                       tashcr           +   1 '  :  '
054300150108     Oqsysprt   E            errtre      1
054400150108     O                                        +   1 'cliente      '
054500150108     O                       tasccm           +   1
054600150108     O                                        +   1 'ORARI SERVIZI'
054700150108     O                       OOR2STIS         +   1 '  :  '
054800150108     O                       OOR2STFS         +   1 '  :  '
054900150108     O                                        +   1 'bolla fuori'
055000150108     O                       tashcr           +   1 '  :  '
055100150105     Oqsysprt   E            errdue      1
055200150105     O                       tasaas
055300150105     O                       taslnp        z  +   1
055400150105     O                       tasnrs        z  +   1
055500150105     O                       tasnsp        z  +   1
055600150108     O                       tasccm           +   1
055700150108     O                       taslod           +   2
055800150105     O                       tasprd           +   1
055900150108     O                                        +   2 'ORARI SERVIZI'
056000150105     O                       OOR2STIS         +   1 '  :  '
056100150105     O                       OOR2STFS         +   1 '  :  '
056200150108     O                                        +   2 'range'
056300150105     O                       or_inizio        +   1 '  :  '
056400150105     O                       or_fine          +   1 '  :  '
056500150129     Oprtf198   E            totb        1
056600150129     O                                              'Totale bolle con HCR '
056700150129     O                                        +   0 'periodo dal'
056800150129     O                       kdal             +   1 '    /  /  '
056900150129     O                                        +   1 'al'
057000150129     O                       kal              +   1 '    /  /  '
057100150129     O                       bolle         2  +   0
057200150129     O                                        +   4 'di cui dalle 8,00 alle '
057300150129     O                                        +   1 ' 20,00'
057400150129     O                       bolleok       2  +   1
057500150108     Oprtf198   E            stot        1
057600150108     O                                              'Totale bolle con HCR '
057700150108     O                                        +   0 'periodo dal'
057800150108     O                       kdal             +   1 '    /  /  '
057900150108     O                                        +   1 'al'
058000150108     O                       kal              +   1 '    /  /  '
058100150108     O                       totale        2  +   0
058200150108     O                                        +   4 'di cui OK'
058300150108     O                       contaok       2  +   1
058400150108     o                       percok        z  +   1
058500150108     O                                        +   0 '%'
058600150109     O                                        +   1 'di cui FUORI orari'
058700150109     O                                        +   1 'di cui FUORI orari'
058800150108     O                       contanook     2  +   1
058900150108     o                       percnook      z  +   1
059000150108     O                                        +   0 '%'
059100150108     O                                        +   1 ' di cui RANGE'
059200150108     O                       contarange    2  +   1
059300150108     o                       percrange     z  +   1
059400150108     O                                        +   0 '%'
059500150108     Oprtf198   E            stot        1
059600150112     o              n21                          19 'Orari STANDARD'
059700150112     o               21                          19 'Orari MIN/MAX '
059800150112     o                                        +   5 'usa sempre TASHCR(S/ ) '
059900150109     O                       wtas             +   2
060000150109     O                                           83 'di cui da BRT'
060100150108     O                       noiok         2  +   1
060200150108     O                                        +   6 '         di cui da BRT'
060300150108     O                       noinook       2  +   1
060400150108     O                                        +   4 'di cui da BRT'
060500150108     O                       noirange      2  +   1
060600150108     Oprtf198   E            sfuori      1
060700150108     O                                        +   5 'Cliente'
060800150108     O                       savccm           +   1
060900150108     O                                        +   2 'con totale bolle'
061000150109     O                                        +   1 ' FUORI orari'
061100150108     O                       totccm        2  +   1
061200150109     O                                        +   1 ' su bolle con HCR'
061300150109     O                       ccm_c(Y)      2  +   1
061400150109     O                       percccm       z  +   1
061500150109     O                                        +   0 '%'
061600150109     Oprtf198   E            sfuoriT     1
061700150109     O                                        +   5 'Num.clienti'
061800150109     O                                        +   1 'con molte bolle FUORI'
061900150109     o                       totcliG       2  +   1
062000150109     O                                        +   3 'con poche bolle FUORI'
062100150109     o                       totcliP       2  +   1
062200150108     Oprtf198   E            serr        1
062300150108     O                                        +   5 'HCR errato'
062400150108     O                       skerr(k)         +   1 '  :  '
062500150108     O                                        +   2 'con totale bolle'
062600150108     O                       skerr_c(K)    2  +   1
062700150108     O                       skerr(a)         +   5 '  :  '
062800150108     O                                        +   2 'con totale bolle'
062900150108     O                       skerr_c(a)    2  +   1
063000150108     O                       skerr(b)         +   5 '  :  '
063100150108     O                                        +   2 'con totale bolle'
063200150108     O                       skerr_c(b)    2  +   1
063300150108     O                       skerr(c)         +   5 '  :  '
063400150108     O                                        +   2 'con totale bolle'
063500150108     O                       skerr_c(c)    2  +   1
063600150108     Oprtf198   E            sran        1
063700150108     O                                        +   5 'RANGE presenti in bolle'
063800150108     O                       ranno(k)         +   1 '  :  -  :  '
063900150108     o                                        +   1 'tot.bol'
064000150108     O                       ranno_c(k)    2  +   1
064100150108     O                       ranno(a)         +   3 '  :  -  :  '
064200150108     o                                        +   1 'bolle'
064300150108     O                       ranno_c(a)    2  +   1
064400150108     O                       ranno(b)         +   3 '  :  -  :  '
064500150108     o                                        +   1 'bolle'
064600150108     O                       ranno_c(b)    2  +   1
064700150108     O                       ranno(c)         +   3 '  :  -  :  '
064800150108     o                                        +   1 'bolle'
064900150108     O                       ranno_c(c)    2  +   1
065000150108     O                       ranno(d)         +   3 '  :  -  :  '
065100150108     o                                        +   1 'bolle'
065200150108     O                       ranno_c(d)    2  +   1
065300150108     Oprtf198   E            sranks      1
065400150108     O                                        +   5 'RANGE presenti per CCM '
065500150108     O                       ranks(k)         +   1
065600150108     o                                        +   1 'bolle'
065700150108     O                       ranks_c(k)    2  +   1
065800150108     O                       ranks(a)         +   3
065900150108     o                                        +   1 'bolle'
066000150108     O                       ranks_c(a)    2  +   1
066100150108     O                       ranks(b)         +   3
066200150108     o                                        +   1 'bolle'
066300150108     O                       ranks_c(b)    2  +   1
066400150108     O                       ranks(c)         +   3
066500150108     o                                        +   1 'bolle'
066600150108     O                       ranks_c(c)    2  +   1
066700150108     O                       ranks(d)         +   3
066800150108     o                                        +   1 'bolle'
066900150108     O                       ranks_c(d)    2  +   1
067000150108     O                       ranks(e)         +   3
067100150108     o                                        +   1 'bolle'
067200150108     O                       ranks_c(e)    2  +   1
