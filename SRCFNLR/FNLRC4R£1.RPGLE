000100941109     H*------------------------------------------------------------------------*
000200150914     H* controalert : scrittura file spia                                      *
000300941109     H*------------------------------------------------------------------------*
000400930128     H DECEDIT('0,') DATEDIT(*DMY.)
000500140805     H DFTACTGRP(*NO) ACTGRP('BARTVAS')
000600941109     F*------------------------------------------------------------------------*
000700070124     ftabel00f  if   e           k disk
000800060911     fTNTBE01L  IF   E           K DISK
000900060828     Ffnarb54l  IF   E           K DISK
001000150922     Ffiar505l  IF   E           K DISK
001100060828     FFNARB01L  IF   E           K DISK
001200060828     F                                     RENAME(FNARB000:FNARB001)
001300070328     FFNLBL02L  IF   E           K DISK
001400070328     FFNLBL01L  IF   E           K DISK
001500070328     F                                     RENAME(FNlbl000:FNlbl001)
001600070124     d
001700070124     D REC             S              2    DIM(50)
001800070326     D CS              S              2    DIM(50)
001900060828     d
002000150914     d* DS ricevuta
002100150914     d FNLRc4DS      E ds
002200060831     d
002300060828     D KPJBA         E DS
002400070129     D tibs02ds      E DS
002500060829     D TRUL33DS      E DS
002600080115     D FNLV55DS      E DS
002700060828     D trul06ds      E DS
002800060828     D  L6                     1     90  0 dim(30)
002900110629
003000060828      * ds per il controllo della presenza di CA per la spedizione richiesta
003100060828     d FIDN12DS      e ds
003200060828     d  skKey                 26    305    dim(20)
003300060828     d  skAnn                306    325    dim(20)
003400060828     d  skDca                326    485  0 dim(20)
003500060828     d  skFca                486    545  0 dim(20)
003600060828     d  skDch                546    705  0 dim(20)
003700060828     d  skCch                706    745    dim(20)
003800060828
003900060828     d dsKey           ds
004000060828     d  dsaac                         4  0
004100060828     d  dsfil                         3  0
004200060828     d  dsnca                         7  0
004300060828      *
004400060828     D ARBDS         E DS                  EXTNAME(FNARB00F)
004500060828     D WSAVDS          DS           700
004600060911     D DCCH          E DS
004700070124     D DS3A          E DS
004800060424     d
004900060828     d II              s              2  0 inz(0)
005000060828     d xx              s              3  0 inz(0)
005100060828     d yy              s              3  0 inz(0)
005200060828     d KFIL            s              3  0
005300060828     d KDCM            s              8  0 inz(0)
005400060831     d Wstacca         s              1    inz(' ')
005500060906     d contaNSPS       s                   like(LTENSPS)
005600060831     d WIDB            s                   like(LTEIDB)
005700060828     d Escbolla        s              1
005800080116     d Wcambiapoc      s              1
005900080116     d POL6            s              1
006000060911     D Wtbecod         S                   LIKE(TBEcod)
006100060829     d Dataiso         s               d   datfmt(*iso)
006200060829     d Dataeur         s               d   datfmt(*eur)
006300070124     d ktbkut          s                   like(tblkut) inz(1)
006400070124     d ktbcod          s                   like(tblcod)
006500070430     d wvol            s                   like(arbvlf)
006600070430     d wpes            s                   like(arbpkf)
006700941109     C*------------------------------------------------------------------------*
006800060424     c     *entry        plist
006900060424     c                   parm                    kpjba
007000150914     c                   movel     kpjbu         fnlrc4ds
007100070529     c
007200080115     c
007300080115     c* Verifico se è un p.o. di £6
007400080115     c                   clear                   fnlv55ds
007500150914     c                   movel     ilrc4lna      d55lin
007600150914     c                   movel     ilrc4dat      d55drf
007700080115     c                   movel     '6'           d55tpt
007800080115     c                   movel     'L'           d55tla
007900080115     c                   call      'FNLV55R'
008000080115     c                   parm                    fnlv55ds
008100080115     c
008200080115     c                   clear                   pol6
008300080115     c                   if        d55tfa<>ilrl1lna
008400080115     c                   eval      pol6='S'
008500080115     c                   endif
008600070406     c*
008700080115     c* Carico la £6 alla data di elaborazione
008800060828     c                   clear                   trul06ds
008900150914     c                   movel     IlRc6LNA      d06key
009000080115     c                   move      'S'           d06key
009100060828     c                   movel     '£6'          d06cod
009200150914     c                   movel     ilrc6dat      d06drf
009300080115     c                   movel     trul06ds      kpjbu
009400060828     c                   call      'TRUL06R'
009500060828     c                   parm                    kpjba
009600060828     c                   movel     kpjbu         trul06ds
009700150922     c
009800150922     c* Lettura file fiar505l alert ancora da inviare
009900150922     c
010000150922     c
010100070129     c
010200060828     c* elaborazione per linea di arrivo della £6 caricata
010300060828     c                   z-add     1             xx
010400060828    4c                   dow       l6(xx)>0
010500060828     c                   eval      kfil=l6(xx)
010600060828     c
010700060828     c     karb          setll     fnarb54l
010800060828     c     karb          reade     fnarb54l
010900070529     c
011000060828    6c                   dow       not %eof(fnarb54l)
011100060828     c
011200060828     c                   exsr      ESCLUDI
011300080116     c
011400080116     c* Se cambia il p.o. di consegna, aggiorno FIARG
011500080118    7c****               if        Escbolla='S' and wcambiapoc='S'
011600080118     c
011700080118    7c                   if        Wcambiapoc='S'
011800080116    8c                   if        lrl1contrbl='S'
011900080116     c                   clear                   Escbolla
012000080116   x8c                   else
012100080116     c
012200080116     c     karb3         chain     fiarg01l
012300080116     c* se è uno dei giri forzati, aggiorno solo p.o. consegna
012400080116     c*  altrimenti pulisco i campi
012500111010   8ac                   if        %found(fiarg01l)
012600080116    9c                   if        (argcgi='101' or argcgi='102' or
012700080116     c                             argcgi='CS ' or (%subst(argcgi:1:7)=
012800080116     c                             'RSM4789')) and argtgi='M'
012900080116     c
013000080116   10c                   if        pol6=' '
013100080116     c                   eval      argpoc=ilrl1lna
013200080116     c                   else
013300080116     c                   eval      argpoc=d55tfa
013400080116   10c                   endif
013500080116     c
013600080116   x9c                   else
013700080116     c                   clear                   argcgi
013800080116     c                   clear                   argtgi
013900080116     c                   clear                   argttl
014000080116     c                   clear                   argpoc
014100080116     c                   clear                   argsqg
014200080116     c                   clear                   argidb
014300080118     c                   clear                   arglno
014400080118     c                   clear                   argerr
014500080116     c                   clear                   Escbolla
014600080116    9c                   endif
014700080116     c
014800080116     c                   update    fiarg000
014900080116    8c                   endif
015000111010   8ac                   endif
015100080116    7c                   endif
015200110629
015300110629     c* Se devo aggiornare le bolle e c'e' già il giro --> verifico se mettere o
015400110629     c*  togliere il centro storico solo se procedura attiva (viene restituito alla prima call
015500110629     c*
015600111010   8ac                   if        not %found(fiarg01l)
015700111010     c                   clear                   argcgi
015800111010     c                   endif
015900111010     c
016000110629     c                   if        lrl1contrbl=' ' and argcgi<>*blanks
016100110713     c                             and olvstprop<>'N' and Escbolla='S'
016200110629     c                   clear                   fnlvstds
016300110629     c                   eval      ilvstaas=arbaas
016400110629     c                   eval      ilvstlnp=arblnp
016500110629     c                   eval      ilvstnrs=arbnrs
016600110629     c                   eval      ilvstnsp=arbnsp
016700110629     c                   eval      ilvstmgs=arbmgs
016800110629     c                   eval      ilvstfin=arbfin
016900110629     c                   eval      ilvstcgi=argcgi
017000110701     c                   eval      ilvstpoc=ilrl1lna
017100110629     c                   eval      ilvstdat=ilrl1dat
017200110629     c                   call      'FNLVSTR'
017300110629     c                   parm                    kpjba
017400110629     c                   parm                    fnlvstds
017500110629     c                   endif
017600060828     c
017700060828    7c                   if        Escbolla=' '
017800070529     c* Verifico se devo solo contare le bolle elaborabili
017900070529     c                   if        lrl1contrbl='S'
018000070529     c                   add       1             lrl1Onumbolle
018100070529     c                   else
018200101119     c* Se ho scritto già numero bolle massimo, eseguo invio prima di procedere
018300101119     c                   if        contansps>bollemax
018400101119     c                   exsr      InvATellus
018500101119     c                   endif
018600101119     c
018700060828     c                   exsr      ELABORA
018800070529     c                   endif
018900080116     c                   endif
019000080116     c*
019100060828     c
019200070529     c                   if        lrl1contrbl=' ' or
019300070529     c                             lrl1Onumbolle<=lrl1botlls
019400060828     c     karb          reade     fnarb54l
019500070529     c                   else
019600070601     c* Nel controllo ho già verificato che ci sono da elaborare +
019700070529     c*  bolle del minimo richiesto--> esco dal pgm
019800070529     c                   eval      lrl1Ominimo=' '
019900070529     c                   leave
020000070529     c                   endif
020100070529     c
020200060828    6c                   enddo
020300060828     c
020400060828     c                   add       1             xx
020500060828    4c                   enddo
020600930128     C*
020700101119     c
020800060829     c* Finita l'elaborazione chiamo pgm per invio dati a TELLUS
020900060829     c* Imposto la DS
021000070529     c* solo se non è richiamo di controllo
021100070919    1c                   if        lrl1contrbl=' '
021200101119     c                   exsr      InvATellus
021300070620     c
021400070919   x1c                   else
021500070529     c
021600070529     c                   movel     fnlrl1ds      kpjbu
021700070919    1c                   endif
021800110701
021900110701     c                   clear                   fnlvstds
022000110701     c                   eval      ilvstTLA='C'
022100110701     c                   call      'FNLVSTR'
022200110701     c                   parm                    kpjba
022300110701     c                   parm                    fnlvstds
022400060829     c
022500000000     C                   SETON                                        LR
022600060828     c*------------------------------------------------------------------
022700060828     c     *INZSR        BEGSR
022800060828     c     KARB          klist
022900060828     c                   kfld                    kfil
023000060828     c                   kfld                    kdcm
023100060828     C     KARB3         KLIST
023200060828     C                   KFLD                    ARBAAS
023300060828     C                   KFLD                    ARBLNP
023400060828     C                   KFLD                    ARBNRS
023500060828     C                   KFLD                    ARBNSP
023600060828     C     KARB1         KLIST
023700060828     C                   KFLD                    LBLAAN
023800060828     C                   KFLD                    LBLLPN
023900060828     C                   KFLD                    LBLNRN
024000060828     C                   KFLD                    LBLNSN
024100070124     C     KE2TAB        KLIST
024200070124     C                   KFLD                    KTBKUT
024300070124     C                   KFLD                    KTBCOD
024400060829     c* Data di elaborazione
024500060829     c                   time                    w0140            14 0
024600060829     C                   MOVE      w0140         w0080             8 0          *DATA (8) IN G/M/AA
024700060829     c     *eur          movel     w0080         dataeur
024800060829     c                   movel     dataeur       dataiso
024900060829     c     *iso          movel     dataiso       datcor            8 0
025000060911     c
025100060911     c* Carico le causali chiusura c,a, che prevedono la consegna della
025200060911     c*  merce
025300060911     c                   z-add     *zeros        xx
025400060911     C                   MOVEL     'CCH'         Wtbecod
025500060911     c     Wtbecod       chain     tntbe000                           31
025600060911      *
025700060911     c                   DOW       *in31 = *off
025800060911     c                   movel     TBEuni        DCCH
025900060911      * Se il S.I. è indicato deve essere uguale al mio, l'anomalia non deve essere per gli Eventi
026000060911     c                   IF        (TBEsif = knsif  or  TBEsif = *blanks)
026100060911     c                             and TBEatb = *BLANKS  and  §cchccco= 'S'
026200060911     c                   add       1             xx
026300060911     c                   movel     TBEke1        cchco(XX)
026400060911     c                   endif
026500060911     c     Wtbecod       reade     tntbe000                               31
026600060911     c                   enddo
026700070124     c
026800070326     c* Carico tipi record di recupero  e C/S
026900070124     C                   Z-ADD     1             KTBKUT
027000070124     C                   MOVE      '3A'          KTBCOD
027100070124     C                   CLEAR                   IY                4 0
027200070326     C                   CLEAR                   IZ                4 0
027300070124     C     KE2TAB        SETLL     TABEL00F
027400070124     C     KE2TAB        READE     TABEL00F                               98
027500070124     C     *IN98         DOWEQ     *OFF
027600070124     C                   MOVEL     TBLUNI        DS3a
027700070412     c* Recuperi
027800070124     C     §3arbl        IFEQ      'R'
027900070124     C                   ADD       1             IY
028000070124     C                   MOVEL     TBLKEY        rec(IY)
028100070124     C                   ENDIF
028200070412     c* C/S
028300070326     C     §3arbl        IFEQ      'C'
028400070412     c     tblkey        andne     '£ '
028500070412     c* Escludo forzatamente bolle "£ "
028600070326     C                   ADD       1             IZ
028700070326     C                   MOVEL     TBLKEY        cs(IZ)
028800070326     C                   ENDIF
028900070124     C     KE2TAB        READE     TABEL00F                               98
029000070124     C                   ENDDO
029100070124     c
029200060829     c
029300060828     c                   ENDSR
029400060828     c*------------------------------------------------------------------
029500060828     c* Esclusione delle spedizioni da non passare a Tellus
029600070507     c*  compresi i giri forzati
029700060828     c*------------------------------------------------------------------
029800060828     c     ESCLUDI       BEGSR
029900060828     c                   clear                   Escbolla
030000060828     c* 1) Con la lettura per data consegna =0
030100060828     c*    ho già escluso le spedizioni consegnate
030200060828     c*    Escludo anche:
030300060828     c
030400070507     c* 7) con C.A. aperta
030500060828     c                   clear                   fidn12ds
030600060828     c                   eval      i12aas = arbaas
030700060828     c                   eval      i12lnp = arblnp
030800060828     c                   eval      i12nrs = arbnrs
030900060828     c                   eval      i12nsp = arbnsp
031000060828     c                   eval      i12fel = 'E'
031100060828     c                   eval      i12fan = 'E'
031200060828      *
031300060828     c                   call      'FIDN12R'
031400060828     c                   parm                    fidn12ds
031500060828      *
031600060828      * se non ci sono errori
031700060828     c                   if        o12err = ' '
031800060828      * se numero di CA trovate maggiore di zero
031900060828     c                   if        o12nca > 0
032000060828     c
032100060828     c                   z-add     *zeros        ii
032200060828     c     1             do        o12nca        ii
032300060828     c                   movea     skkey(ii)     dskey
032400060828     C                   Z-ADD     skdch(ii)     dctdch            8 0
032500060911     C                   movel     skcch(ii)     dctcch            2
032600060828      *
032700060911     C* -SE ESISTE C.A. APERTA  --> ESCO
032800060828     c                   If        dctdch = 0
032900060828     c                   eval      Escbolla='S'
033000060828     c                   leave
033100060828     c                   endif
033200060911     c
033300060911     c* Se c.a. chiusa con causale che non prevede consegna esco
033400060911     c                   if        dctcch<>*blanks
033500060911     c     dctcch        lookup    cchco                                  35
033600060911     c                   if        not *in35
033700060911     c                   eval      Escbolla='S'
033800060911     c                   leave
033900060911     c                   endif
034000060911     c                   endif
034100060911     c
034200060828     C                   ENDDO
034300060828      *
034400060828     c                   endif
034500060828     c                   endif
034600060828     C*
034700060828     C* -SE ESISTE C.A. APERTA  --> ESCO
034800060828    1C     Escbolla      IFEQ      'S'
034900060828     C                   GOTO      ENDescludi
035000060828    1C                   ENDIF
035100070202     c
035200070202     c* Senza testare il record di giacenza, escludo tutte le bolle
035300070202     c*  con blocco G
035400070202    1c                   if        arbfbc='G'
035500070202     c                   eval      Escbolla='S'
035600070202     C                   GOTO      ENDescludi
035700070202    3C                   ENDIF
035800070507     c
035900070507     c* 9) con tipo bolla di recupero
036000070507    1c     arbcbo        lookup    REC                                    35
036100070507     c                   if        *in35
036200070507     c                   eval      Escbolla='S'
036300070507     c                   goto      Endescludi
036400070507     c                   endif
036500060828     C*
036600060828     c     Endescludi    ENDSR
