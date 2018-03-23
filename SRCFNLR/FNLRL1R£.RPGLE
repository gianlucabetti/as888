000100941109     H*------------------------------------------------------------------------*
000200060828     H* scambio dati da Bartolini   a  Tellus                                  *
000300941109     H*------------------------------------------------------------------------*
000400930128     H DECEDIT('0,') DATEDIT(*DMY.)
000500140805     H DFTACTGRP(*NO) ACTGRP('BARTVAS')
000600941109     F*------------------------------------------------------------------------*
000700070124     ftabel00f  if   e           k disk
000800060911     fTNTBE01L  IF   E           K DISK
000900060828     Ffnarb54l  IF   E           K DISK
001000060828     FFNARB01L  IF   E           K DISK
001100060828     F                                     RENAME(FNARB000:FNARB001)
001200080115     FFIARG01L  uF A E           K DISK
001300070328     FFNLBL02L  IF   E           K DISK
001400070328     FFNLBL01L  IF   E           K DISK
001500070328     F                                     RENAME(FNlbl000:FNlbl001)
001600060829     F
001700090604     FTIVGD00F  uf a E             DISK
001800070919     FFILTE01L  uf a E           K DISK
001900070124     d
002000070124     D cchco           s              2    dim(30)
002100070124     D REC             S              2    DIM(50)
002200070326     D CS              S              2    DIM(50)
002300060828     d
002400070124     d* DS ricevuta: p.o. in gestione da elaborare
002500060831     d FNLRL1DS        ds
002600060831     d  ILRL1lna                      3s 0
002700070406     d  ILRL1dat                      8s 0
002800060905     d  ILRL1dis                      1a
002900070207     d  ILRL1tst                      1a
003000070523     d  ILRL1SenGiro                  1a
003100070529     d  LRL1contrBL                   1a
003200070529     d  LRL1botlls                    2s 0
003300070529     d* Dati di output
003400070529     d* "S"--> numero minimo bolle da elaborare non raggiunto
003500070529     d  LRL1Ominimo                   1a
003600070529     d  LRL1Onumbolle                 3s 0
003700060831     d
003800070207     D DLTEFLO       E DS
003900060829     D DTLLSO        E DS
004000070129     D DTEP          E DS
004100101119     D Dvpotellus    E DS
004200060828     D KPJBA         E DS
004300070129     D tibs02ds      E DS
004400070129     D TISI95DS      E DS
004500060829     D TIS781DS      E DS
004600070207     D TIS781DFLO    E DS
004700070305     D DVGDFLO       E DS
004800060829     D TRUL33DS      E DS
004900080115     D FNLV55DS      E DS
005000060828     D trul06ds      E DS
005100060828     D  L6                     1     90  0 dim(30)
005200110629
005300110629     D fnlvstds      E DS
005400060828      * ds per il controllo della presenza di CA per la spedizione richiesta
005500060828     d FIDN12DS      e ds
005600060828     d  skKey                 26    305    dim(20)
005700060828     d  skAnn                306    325    dim(20)
005800060828     d  skDca                326    485  0 dim(20)
005900060828     d  skFca                486    545  0 dim(20)
006000060828     d  skDch                546    705  0 dim(20)
006100060828     d  skCch                706    745    dim(20)
006200060828
006300060828     d dsKey           ds
006400060828     d  dsaac                         4  0
006500060828     d  dsfil                         3  0
006600060828     d  dsnca                         7  0
006700060828      *
006800060828     D ARBDS         E DS                  EXTNAME(FNARB00F)
006900060828     D WSAVDS          DS           700
007000060911     D DCCH          E DS
007100070124     D DS3A          E DS
007200060424     d
007300060828     d II              s              2  0 inz(0)
007400060828     d xx              s              3  0 inz(0)
007500060828     d yy              s              3  0 inz(0)
007600060828     d KFIL            s              3  0
007700060828     d KDCM            s              8  0 inz(0)
007800060831     d Wstacca         s              1    inz(' ')
007900060906     d contaNSPS       s                   like(LTENSPS)
008000060831     d WIDB            s                   like(LTEIDB)
008100060828     d Escbolla        s              1
008200080116     d Wcambiapoc      s              1
008300080116     d POL6            s              1
008400060911     D Wtbecod         S                   LIKE(TBEcod)
008500060829     d Dataiso         s               d   datfmt(*iso)
008600060829     d Dataeur         s               d   datfmt(*eur)
008700070124     d ktbkut          s                   like(tblkut) inz(1)
008800070124     d ktbcod          s                   like(tblcod)
008900070430     d wvol            s                   like(arbvlf)
009000070430     d wpes            s                   like(arbpkf)
009100941109     C*------------------------------------------------------------------------*
009200060424     c     *entry        plist
009300060424     c                   parm                    kpjba
009400060831     c                   movel     kpjbu         fnlrl1ds
009500070529     c
009600070529     c                   clear                   lrl1Ominimo
009700070529     c                   clear                   lrl1Onumbolle
009800101119     c                   clear                   wstacca
009900070529     c*
010000070529     c                   if        lrl1contrbl='S'
010100070601     c                   eval      lrl1Ominimo='S'
010200070529     c                   endif
010300070406     c*
010400070406     c* Reperisco versioni giri x filiale/data richieste
010500070406     c                   call      'FIDG11R1'
010600070406     c                   parm                    ILRL1lna
010700070406     c                   parm                    ILRL1dat
010800070406     c                   parm      *blanks       Out_KeyVer        8
010900070416     c                   parm      *blanks       Out_VerDes       30
011000070406     c                   parm      *blanks       Out_Esito         1
011100080115     c
011200080115     c* Verifico se è un p.o. di £6
011300080115     c                   clear                   fnlv55ds
011400080115     c                   movel     ilrl1lna      d55lin
011500080115     c                   movel     ilrl1dat      d55drf
011600080115     c                   movel     '6'           d55tpt
011700080115     c                   movel     'L'           d55tla
011800080115     c                   call      'FNLV55R'
011900080115     c                   parm                    fnlv55ds
012000080115     c
012100080115     c                   clear                   pol6
012200080115     c                   if        d55tfa<>ilrl1lna
012300080115     c                   eval      pol6='S'
012400080115     c                   endif
012500070406     c*
012600080115     c* Carico la £6 alla data di elaborazione
012700060828     c                   clear                   trul06ds
012800060831     c                   movel     IlRL1LNA      d06key
012900080115     c                   move      'S'           d06key
013000060828     c                   movel     '£6'          d06cod
013100080115     c                   movel     ilrl1dat      d06drf
013200080115     c                   movel     trul06ds      kpjbu
013300060828     c                   call      'TRUL06R'
013400060828     c                   parm                    kpjba
013500060828     c                   movel     kpjbu         trul06ds
013600070129     c
013700070129     C                   CLEAR                   tibs02ds
013800070129     c                   clear                   dtep
013900070129     C                   MOVEL     'C'           T02MOD
014000070129     C                   MOVEL     KNSIF         T02SIF
014100070129     C                   MOVEL     'TEP'         T02COD
014200070129     c                   movel(P)  ilrl1lna      t02ke1
014300070129     C                   CALL      'TIBS02R'
014400070129     C                   PARM                    KPJBA
014500070129     C                   PARM                    tibs02ds
014600070129    2C                   IF        T02ERR = *BLANKS
014700070129     C                   MOVEL     T02uni        dtep
014800070129    2C                   ENDIF
014900070129     c
015000101119     c* Carico il numero max di bolle per file
015100101119     C                   CLEAR                   tibs02ds
015200101119     c                   clear                   bollemax
015300101119     C                   MOVEL     'C'           T02MOD
015400101119     C                   MOVEL     KNSIF         T02SIF
015500101119     C                   MOVEL     'VPO'         T02COD
015600101119     c                   movel(P)  'TELLUS'      t02ke1
015700101119     C                   CALL      'TIBS02R'
015800101119     C                   PARM                    KPJBA
015900101119     C                   PARM                    tibs02ds
016000101119    2C                   IF        T02ERR = *BLANKS
016100101119     C                   MOVEL     T02uni        dvpotellus
016200101119     C                   MOVEL     §vpobomax     bollemax          5 0
016300101119    2C                   ENDIF
016400060828     c
016500060828     c* elaborazione per linea di arrivo della £6 caricata
016600060828     c                   z-add     1             xx
016700060828    4c                   dow       l6(xx)>0
016800060828     c                   eval      kfil=l6(xx)
016900060828     c
017000060828     c     karb          setll     fnarb54l
017100060828     c     karb          reade     fnarb54l
017200070529     c
017300060828    6c                   dow       not %eof(fnarb54l)
017400060828     c
017500060828     c                   exsr      ESCLUDI
017600080116     c
017700080116     c* Se cambia il p.o. di consegna, aggiorno FIARG
017800080118    7c****               if        Escbolla='S' and wcambiapoc='S'
017900080118     c
018000080118    7c                   if        Wcambiapoc='S'
018100080116    8c                   if        lrl1contrbl='S'
018200080116     c                   clear                   Escbolla
018300080116   x8c                   else
018400080116     c
018500080116     c     karb3         chain     fiarg01l
018600080116     c* se è uno dei giri forzati, aggiorno solo p.o. consegna
018700080116     c*  altrimenti pulisco i campi
018800111010   8ac                   if        %found(fiarg01l)
018900080116    9c                   if        (argcgi='101' or argcgi='102' or
019000080116     c                             argcgi='CS ' or (%subst(argcgi:1:7)=
019100080116     c                             'RSM4789')) and argtgi='M'
019200080116     c
019300080116   10c                   if        pol6=' '
019400080116     c                   eval      argpoc=ilrl1lna
019500080116     c                   else
019600080116     c                   eval      argpoc=d55tfa
019700080116   10c                   endif
019800080116     c
019900080116   x9c                   else
020000080116     c                   clear                   argcgi
020100080116     c                   clear                   argtgi
020200080116     c                   clear                   argttl
020300080116     c                   clear                   argpoc
020400080116     c                   clear                   argsqg
020500080116     c                   clear                   argidb
020600080118     c                   clear                   arglno
020700080118     c                   clear                   argerr
020800080116     c                   clear                   Escbolla
020900080116    9c                   endif
021000080116     c
021100080116     c                   update    fiarg000
021200080116    8c                   endif
021300111010   8ac                   endif
021400080116    7c                   endif
021500110629
021600110629     c* Se devo aggiornare le bolle e c'e' già il giro --> verifico se mettere o
021700110629     c*  togliere il centro storico solo se procedura attiva (viene restituito alla prima call
021800110629     c*
021900111010   8ac                   if        not %found(fiarg01l)
022000111010     c                   clear                   argcgi
022100111010     c                   endif
022200111010     c
022300110629     c                   if        lrl1contrbl=' ' and argcgi<>*blanks
022400110713     c                             and olvstprop<>'N' and Escbolla='S'
022500110629     c                   clear                   fnlvstds
022600110629     c                   eval      ilvstaas=arbaas
022700110629     c                   eval      ilvstlnp=arblnp
022800110629     c                   eval      ilvstnrs=arbnrs
022900110629     c                   eval      ilvstnsp=arbnsp
023000110629     c                   eval      ilvstmgs=arbmgs
023100110629     c                   eval      ilvstfin=arbfin
023200110629     c                   eval      ilvstcgi=argcgi
023300110701     c                   eval      ilvstpoc=ilrl1lna
023400110629     c                   eval      ilvstdat=ilrl1dat
023500110629     c                   call      'FNLVSTR'
023600110629     c                   parm                    kpjba
023700110629     c                   parm                    fnlvstds
023800110629     c                   endif
023900060828     c
024000060828    7c                   if        Escbolla=' '
024100070529     c* Verifico se devo solo contare le bolle elaborabili
024200070529     c                   if        lrl1contrbl='S'
024300070529     c                   add       1             lrl1Onumbolle
024400070529     c                   else
024500101119     c* Se ho scritto già numero bolle massimo, eseguo invio prima di procedere
024600101119     c                   if        contansps>bollemax
024700101119     c                   exsr      InvATellus
024800101119     c                   endif
024900101119     c
025000060828     c                   exsr      ELABORA
025100070529     c                   endif
025200080116     c                   endif
025300080116     c*
025400060828     c
025500070529     c                   if        lrl1contrbl=' ' or
025600070529     c                             lrl1Onumbolle<=lrl1botlls
025700060828     c     karb          reade     fnarb54l
025800070529     c                   else
025900070601     c* Nel controllo ho già verificato che ci sono da elaborare +
026000070529     c*  bolle del minimo richiesto--> esco dal pgm
026100070529     c                   eval      lrl1Ominimo=' '
026200070529     c                   leave
026300070529     c                   endif
026400070529     c
026500060828    6c                   enddo
026600060828     c
026700060828     c                   add       1             xx
026800060828    4c                   enddo
026900930128     C*
027000101119     c
027100060829     c* Finita l'elaborazione chiamo pgm per invio dati a TELLUS
027200060829     c* Imposto la DS
027300070529     c* solo se non è richiamo di controllo
027400070919    1c                   if        lrl1contrbl=' '
027500101119     c                   exsr      InvATellus
027600070620     c
027700070919   x1c                   else
027800070529     c
027900070529     c                   movel     fnlrl1ds      kpjbu
028000070919    1c                   endif
028100110701
028200110701     c                   clear                   fnlvstds
028300110701     c                   eval      ilvstTLA='C'
028400110701     c                   call      'FNLVSTR'
028500110701     c                   parm                    kpjba
028600110701     c                   parm                    fnlvstds
028700060829     c
028800000000     C                   SETON                                        LR
028900060828     c*------------------------------------------------------------------
029000060828     c     *INZSR        BEGSR
029100060828     c     KARB          klist
029200060828     c                   kfld                    kfil
029300060828     c                   kfld                    kdcm
029400060828     C     KARB3         KLIST
029500060828     C                   KFLD                    ARBAAS
029600060828     C                   KFLD                    ARBLNP
029700060828     C                   KFLD                    ARBNRS
029800060828     C                   KFLD                    ARBNSP
029900060828     C     KARB1         KLIST
030000060828     C                   KFLD                    LBLAAN
030100060828     C                   KFLD                    LBLLPN
030200060828     C                   KFLD                    LBLNRN
030300060828     C                   KFLD                    LBLNSN
030400070124     C     KE2TAB        KLIST
030500070124     C                   KFLD                    KTBKUT
030600070124     C                   KFLD                    KTBCOD
030700060829     c* Data di elaborazione
030800060829     c                   time                    w0140            14 0
030900060829     C                   MOVE      w0140         w0080             8 0          *DATA (8) IN G/M/AA
031000060829     c     *eur          movel     w0080         dataeur
031100060829     c                   movel     dataeur       dataiso
031200060829     c     *iso          movel     dataiso       datcor            8 0
031300060911     c
031400060911     c* Carico le causali chiusura c,a, che prevedono la consegna della
031500060911     c*  merce
031600060911     c                   z-add     *zeros        xx
031700060911     C                   MOVEL     'CCH'         Wtbecod
031800060911     c     Wtbecod       chain     tntbe000                           31
031900060911      *
032000060911     c                   DOW       *in31 = *off
032100060911     c                   movel     TBEuni        DCCH
032200060911      * Se il S.I. è indicato deve essere uguale al mio, l'anomalia non deve essere per gli Eventi
032300060911     c                   IF        (TBEsif = knsif  or  TBEsif = *blanks)
032400060911     c                             and TBEatb = *BLANKS  and  §cchccco= 'S'
032500060911     c                   add       1             xx
032600060911     c                   movel     TBEke1        cchco(XX)
032700060911     c                   endif
032800060911     c     Wtbecod       reade     tntbe000                               31
032900060911     c                   enddo
033000070124     c
033100070326     c* Carico tipi record di recupero  e C/S
033200070124     C                   Z-ADD     1             KTBKUT
033300070124     C                   MOVE      '3A'          KTBCOD
033400070124     C                   CLEAR                   IY                4 0
033500070326     C                   CLEAR                   IZ                4 0
033600070124     C     KE2TAB        SETLL     TABEL00F
033700070124     C     KE2TAB        READE     TABEL00F                               98
033800070124     C     *IN98         DOWEQ     *OFF
033900070124     C                   MOVEL     TBLUNI        DS3a
034000070412     c* Recuperi
034100070124     C     §3arbl        IFEQ      'R'
034200070124     C                   ADD       1             IY
034300070124     C                   MOVEL     TBLKEY        rec(IY)
034400070124     C                   ENDIF
034500070412     c* C/S
034600070326     C     §3arbl        IFEQ      'C'
034700070412     c     tblkey        andne     '£ '
034800070412     c* Escludo forzatamente bolle "£ "
034900070326     C                   ADD       1             IZ
035000070326     C                   MOVEL     TBLKEY        cs(IZ)
035100070326     C                   ENDIF
035200070124     C     KE2TAB        READE     TABEL00F                               98
035300070124     C                   ENDDO
035400070124     c
035500060829     c
035600060828     c                   ENDSR
035700101119     c*------------------------------------------------------------------
035800101119     c*  Trasmissione dati a Tellus
035900101119     c*------------------------------------------------------------------
036000101119     c     InvATellus    BEGSR
036100101119    2c                   if        contansps>0
036200101119     c
036300101119     c* Scrittura di un record in FILTE00F
036400101119     c                   exsr      ScriviFILTE
036500101119     c
036600101119     c                   clear                   tis781ds
036700101119     c                   eval      §781tip='SO'
036800101119    3c                   if        ILRL1tst = 'S'
036900101119     c                   eval      §781ksu='00TELTST'
037000101119     c                   else
037100101119     c                   eval      §781ksu='00TELLUS'
037200101119    3c                   endif
037300101119     c                   eval      §781tsc='WW'
037400101119     c                   eval      §781daT=datcor
037500101119     c                   eval      §781pgm='TLL' + %editc(widb:'X')
037600101119     c* Costruisco il nome da dare al file d output finale
037700101119     c*                  eval      §781out='S'+%editc(ILRL1lna:'X')+
037800101119     c*                            %subst(%editc(widb:'X'):2:6)                 * 'S'+FIL+IDB.TXT
037900160308     c                   eval      §781out='SS'+%subst(%editc(widb:'X'):3:5)+
038000160308     c                             %editc(ILRL1lna:'X')                         * 'SS'+IDB+FIL.TXT
038100101119     c* Imposto la "S" per il cambio del S.I.
038200101119     c                   clear                   TIS781DFLO
038300101119     c                   movel     'S'           §781flocsi
038400101119     c                   movel     'P'           §781floela
038500101119     c                   movel     TIS781DFLO    §781flo
038600101119     c*
038700101119     c* Forzo lo scarico del buffer d output del TIVGD
038800101119     c                   feod      TIVGD00F
038900101119     c
039000101119     c* Finita l'elaborazione chiamo pgm
039100101119     c                   CALL      'TIS781C1'
039200101119     c                   parm      *blanks       ESITO             1
039300101119     c                   parm                    tis781ds
039400101119     c                   parm      *blanks       vgdprg           10
039500101119     c
039600101119     c* richaino filte per aggiornare vgdprg
039700101119     c     widb          chain(e)  filte01l
039800101119    3c                   if        not %error
039900111128     c
040000101119    4c                   if        vgdprg=*blanks
040100101119     c                   eval      ltehdl='ANNULLATO'
040200101119     c                   eval      lteprg='*NULL'
040300111128     c                   eval      ltedatu=datcor
040400111128     c                   time                    lteorau
040500101119     c                   else
040600111128     c
040700111128     c                   if        ltehdl<>'ANNULLATO'
040800101119     c                   eval      ltehdl=*blanks
040900111128     c                   endif
041000111128     c
041100101119     c                   eval      lteprg=vgdprg
041200101119    4c                   endif
041300111128     c
041400101119     c                   update    filte000
041500101119    3c                   endif
041600101119    2c                   endif
041700101119     c* Pulisco per staccare un nuovo numero
041800101119     c                   clear                   wstacca
041900101119     c                   clear                   contansps
042000101119     c                   ENDSR
042100060828     c*------------------------------------------------------------------
042200060828     c* Esclusione delle spedizioni da non passare a Tellus
042300070507     c*  compresi i giri forzati
042400060828     c*------------------------------------------------------------------
042500060828     c     ESCLUDI       BEGSR
042600060828     c                   clear                   Escbolla
042700080116     c                   clear                   Wcambiapoc
042800060828     c* 1) Con la lettura per data consegna =0
042900060828     c*    ho già escluso le spedizioni consegnate
043000060828     c*    Escludo anche:
043100070507     c
043200080115     c     karb3         chain(N)  fiarg01l
043300080116     c
043400080116     c* Verifico se cambia il p.o. di consegna
043500080116     c                   if        %found(fiarg01l) and
043600080116     c                             ((argpoc<>ilrl1lna and pol6=' ') or
043700080116     c                             (argpoc<>d55tfa and pol6='S'))
043800080116     c                   eval      wcambiapoc='S'
043900080116     c* se sono nel giro dei controlli faccio sicuramente elaborare
044000080116     c*  anche se le bolle fossero poche per aggiornare il p.o. di
044100080116     c*  consegna
044200080116     c                   if        lrl1contrBL='S'
044300080116     c                   eval      lrl1Onumbolle=900
044400080116     c                   endif
044500080116     c                   endif
044600080116     c
044700080116     c
044800080116     c* 2) spedizioni con calcolo giro MANUALE
044900070507     c                   if        %found(fiarg01l) and argtgi='M'
045000070507     c                   eval      Escbolla='S'
045100070507     c                   goto      Endescludi
045200070507     c                   endif
045300070523     c
045400070523     c* 2a) Se richiesto, escludo anche le spedizioni che hanno già il giro
045500070601     c                   if        ilrl1SenGiro='S' and %found(fiarg01l)
045600070601     c                   if        argcgi<>*blanks
045700070523     c                   eval      Escbolla='S'
045800070601     c                   goto      Endescludi
045900070601     c                   else
046000070601     c* Rielaboro solo alcuni codici di errore
046100070621     c                   if        (argerr='  ' and arglno=*blanks) or
046200070621     c                             argerr='02' or argerr='03' or
046300070621     c                             argerr='04' or argerr='51' or
046400070621     c                             argerr='52'
046500070621     c                   else
046600070621     c                   eval      Escbolla='S'
046700070621     c                   goto      Endescludi
046800070621     c                   endif
046900070621     c
047000070621     c**                 if        argerr<>'  ' and argerr<>'02'
047100070621     c**                           and argerr<>'03' and argerr<>'04'
047200070621     c**                           and argerr<>'51' and argerr<>'52'
047300070621     c**                 eval      Escbolla='S'
047400070621     c**                 goto      Endescludi
047500070621     c**                 endif
047600070601     c                   endif
047700070601     c                   endif
047800070507     c*
047900070507     c* 3) A questo punto forzo i giri e poi finisco le esclusioni
048000070507     c*                           scrivo direttamente FIARG
048100070507     c                   exsr      ForzaGiri
048200070507     c* Se giro forzato, esco
048300070507     c                   if        wforza<>' '
048400070507     c                   eval      Escbolla='S'
048500070507     C                   GOTO      ENDescludi
048600070507    3C                   ENDIF
048700060828     c
048800070507     c* 4) Spedizioni già in distinta
048900070507     c* 5) Spedizioni in Fermo deposito
049000070507     c* 6) Spedizioni bloccate ("*" è per il cambio di porto)
049100060905     c                   if        (arbndc>0 and arbddc>0 and ilrl1dis=' ') or
049200070507     c                             arbffd<>' '  or
049300060828     c                             arbfbc='B' or arbfbc='*'
049400060828     c                   eval      Escbolla='S'
049500060828     c                   goto      Endescludi
049600060828     c                   endif
049700060828     c
049800070507     c* 7) con C.A. aperta
049900060828     c                   clear                   fidn12ds
050000060828     c                   eval      i12aas = arbaas
050100060828     c                   eval      i12lnp = arblnp
050200060828     c                   eval      i12nrs = arbnrs
050300060828     c                   eval      i12nsp = arbnsp
050400060828     c                   eval      i12fel = 'E'
050500060828     c                   eval      i12fan = 'E'
050600060828      *
050700060828     c                   call      'FIDN12R'
050800060828     c                   parm                    fidn12ds
050900060828      *
051000060828      * se non ci sono errori
051100060828     c                   if        o12err = ' '
051200060828      * se numero di CA trovate maggiore di zero
051300060828     c                   if        o12nca > 0
051400060828     c
051500060828     c                   z-add     *zeros        ii
051600060828     c     1             do        o12nca        ii
051700060828     c                   movea     skkey(ii)     dskey
051800060828     C                   Z-ADD     skdch(ii)     dctdch            8 0
051900060911     C                   movel     skcch(ii)     dctcch            2
052000060828      *
052100060911     C* -SE ESISTE C.A. APERTA  --> ESCO
052200060828     c                   If        dctdch = 0
052300060828     c                   eval      Escbolla='S'
052400060828     c                   leave
052500060828     c                   endif
052600060911     c
052700060911     c* Se c.a. chiusa con causale che non prevede consegna esco
052800060911     c                   if        dctcch<>*blanks
052900060911     c     dctcch        lookup    cchco                                  35
053000060911     c                   if        not *in35
053100060911     c                   eval      Escbolla='S'
053200060911     c                   leave
053300060911     c                   endif
053400060911     c                   endif
053500060911     c
053600060828     C                   ENDDO
053700060828      *
053800060828     c                   endif
053900060828     c                   endif
054000060828     C*
054100060828     C* -SE ESISTE C.A. APERTA  --> ESCO
054200060828    1C     Escbolla      IFEQ      'S'
054300060828     C                   GOTO      ENDescludi
054400060828    1C                   ENDIF
054500060828     c
054600070507     c* 8) con giacenza aperta
054700070202    1c***                if        arbfbc='G'
054800070202     c***  karb3         chain     tigcp21l
054900070202    2c***                if        %found(tigcp21l)
055000070202     c* Se la fase < 35 --> significa giacenza aperta senza disposizioni
055100060828     c*                     in arrivo
055200070202    3c***                if        gcpfas<35
055300070202     c***                eval      Escbolla='S'
055400070202     C***                GOTO      ENDescludi
055500070202    3C***                ENDIF
055600060828     c
055700060828     c*  Altrimenti verifico se ha una figlia in consegna con lnp=lna
055800070202     C***                MOVE      ARBDS         WSAVDS
055900060828     C*
056000070202     C***  KARB3         SETLL     FNLBL02L
056100070202     C***  KARB3         READE     FNLBL02L                               37
056200070202    3C***  *IN37         DOWEQ     *OFF
056300070202    4C***  LBLLPN        IFEQ      LBLLAN
056400070202     C***  KARB1         CHAIN     FNARB01L                           38
056500070202    5C***  *IN38         IFEQ      *OFF
056600070202     C***  arbndc        andgt     0
056700070202     C***                SETON                                        37
056800070202     C***                MOVEL     'S'           Escbolla
056900070202    5C***                ENDIF
057000070202    4C***                ENDIF
057100070202     C**N37KARB3         READE     FNLBL02L                               37
057200070202    3C***                ENDDO
057300060828     C*
057400060828     C* RIMETTO I CAMPI SALVATI IN PRECEDENZA
057500070202     C***                MOVE      WSAVDS        ARBDS
057600070202    3C***  Escbolla      IFEQ      'S'
057700070202     C***                GOTO      ENDescludi
057800070202    3C***                ENDIF
057900070202    2C***                ENDIF
058000070202    1c***                ENDIF
058100070202     c
058200070202     c* Senza testare il record di giacenza, escludo tutte le bolle
058300070202     c*  con blocco G
058400070202    1c                   if        arbfbc='G'
058500070202     c                   eval      Escbolla='S'
058600070202     C                   GOTO      ENDescludi
058700070202    3C                   ENDIF
058800070507     c
058900070507     c* 9) con tipo bolla di recupero
059000070507    1c     arbcbo        lookup    REC                                    35
059100070507     c                   if        *in35
059200070507     c                   eval      Escbolla='S'
059300070507     c                   goto      Endescludi
059400070507     c                   endif
059500060828     C*
059600060828     c     Endescludi    ENDSR
059700060828     c*------------------------------------------------------------------
059800060829     c* Elaborazione per scrittura file TIVGD00F
059900060828     c*------------------------------------------------------------------
060000060828     c     ELABORA       BEGSR
060100070315     c*
060200060829     c* Stacco l'ID elaboratore una volta sola
060300060829     c                   if        wstacca=' '
060400060829     c                   clear                   trul33ds
060500060829     c                   movel     'L'           i33tla
060600060829     c                   z-add     60            i33cnu
060700060829     c                   z-add     1             i33num
060800060829     c                   movel     trul33ds      kpjbu
060900060829     c                   call      'TRUL33R'
061000060829     c                   PARM                    kpjba
061100060829     c                   movel     kpjbu         trul33ds
061200060829     c
061300070118     c                   if        o33err<>*zeros
061400060829     c                   z-add     1             o33nri
061500060829     c                   endif
061600060831     c
061700060831     c* imposto il numero staccato in un campo lungo 7
061800060831     c                   z-add     o33nri        widb
061900060829     c
062000060829     c                   eval      wstacca='S'
062100060829     c                   endif
062200060829     c
062300060829     c* Preparo la scrittura del file TIVGD
062400060829     c                   clear                   tivgd000
062500070329     c                   eval      vgdtip='SO'
062600070207     c                   if        ILRL1tst = 'S'
062700070207     c                   eval      vgdksu='00TELTST'
062800070207     c                   else
062900070207     c                   eval      vgdksu='00TELLUS'
063000070207     c                   endif
063100060829     c                   eval      vgdtsc='WW'
063200060829     c
063300060829     c                   eval      vgddaT=datcor
063400060831     c                   eval      vgdpgm='TLL' + %editc(widb:'X')
063500070305     c                   clear                   DVGDFLO
063600070305     c                   movel     'P'           §VGDFELA
063700070305     c                   movel     DVGDFLO       vgdflo
063800060829     c
063900060829     c* Imposto i dati della spedizione
064000060829     c                   clear                   DTLLSO
064100060829     c                   eval      §TLLSOAAS=arbaas
064200060829     c                   eval      §TLLSOlnp=arblnp
064300060829     c                   eval      §TLLSOnrs=arbnrs
064400060829     c                   eval      §TLLSOnsp=arbnsp
064500060831     c                   eval      §TLLSOidb=widb
064600080115     c*****              eval      §TLLSOpoc=ILRL1lna
064700080115     c                   if        pol6=' '
064800080115     c                   eval      §TLLSOpoc=ilrl1lna
064900080115     c                   else
065000080115     c                   eval      §TLLSOpoc=d55tfa
065100080115     c                   endif
065200080115     c
065300060829     c                   eval      §TLLSOind=arbind
065400060829     c                   eval      §TLLSOcad=arbcad
065500060829     c                   eval      §TLLSOlod=arblod
065600060829     c                   eval      §TLLSOprd=arbprd
065700060829     c                   eval      §TLLSOnzd='ITA'
065800060925     c* se supermercato ambito specifico
065900070129     c****               if        arbtc1='S' or  arbtc2='S'
066000070612     c****               eval      §tllsoain='s'
066100070129     c****               endif
066200061213     c
066300061213     c* il tisi95 lo richiamo lo stesso per avere la località normalizzata
066400061213     c* ma se supermercati, non imposto §tllsoAIN
066500061213     c                   exsr      EXETISI95
066600060829     c
066700070108     c* Provincia e provincia alternativa
066800070108     c                   if        o95err=' '
066900070108     c                   eval      §TLLSOPRD = o95prv
067000070108     c                   eval      §TLLSOPRA = o95pra
067100070108     c* Località normalizzata riempita solo se diversa
067200070206     c                   if        o95loc<>arblod
067300070524     C                   EVAL      §tllsoLODN  = O95LOC
067400070206     c                   endif
067500060901     c                   endif
067600070508     c*
067700070125     c                   clear                   §tllsocgi
067800070406     c                   eval      §tllsokey = Out_KeyVer
067900070406     c                   move      '.'           §tllsoflo
068000070315     c*
068100080404     c                   eval      VGDDTA = %subst(DTLLSO:1:%size(DTLLSO))
068200070315     c*
068300060829     c                   write     tivgd000
068400070315     c*
068500070315     c                   if        contansps<99999
068600060906     c                   add       1             contansps
068700060831     c                   endif
068800070315     c*
068900060828     c                   ENDSR
069000070326     c*------------------------------------------------------------------
069100070326     c* Forzatura giri appositi e scrivo direttamente FIARG
069200070326     c*   se non ancora presente
069300070326     c*------------------------------------------------------------------
069400070326     c     ForzaGIRI     BEGSR
069500070326     c                   clear                   Wforza            1
069600070326     c
069700070326     c* 1) Per C/Servizi
069800070326     c     arbcbo        lookup    CS                                     35
069900070326     c                   if        *in35
070000070328     c* solo se non è bolla figlia da legami bolla
070100070328     c     karb3         setll     fnlbl01l
070200070328     c                   if        not %equal(fnlbl01l)
070300070326     c                   eval      wforza='C'
070400070326     c                   endif
070500070328     c                   endif
070600070328     c
070700070326     c* 2) Per LNA 101 e 102
070800070326     c                   if        arblna=101 or arblna=102
070900070326     c                   eval      wforza='L'
071000070326     c                   endif
071100070716     c*
071200070716     c* Forzatura x San Marino in quanto mancanti le sezioni d censimento istat.
071300070716     c                   if        wforza=*blanks
071400070716     c                   if        arbPRD = 'RN'    and
071500070716     c                             arbCAD >= '47890' and
071600070716     c                             arbCAD <= '47899'
071700070716     c                   eval      wforza='S'
071800070716     c                   endif
071900070716     c                   endif
072000070326
072100070326     c
072200070326     c                   if        wforza<>' '
072300070326     c* Scrivo direttamente il FIARG
072400070326     c                   clear                   FIARG000
072500070326     c                   if        ILRL1tst = 'S'
072600070326     c                   eval      argaas = 9999
072700070326     c                   else
072800070326     c                   eval      argaas = arbaas
072900070326     c                   endif
073000070326     c                   eval      arglnp = arblnp
073100070326     c                   eval      argnrs = arbnrs
073200070326     c                   eval      argnsp = arbnsp
073300070326     c                   eval      arglna = arblna
073400070326     c                   eval      argidb = widb
073500080115     c                   if        pol6=' '
073600070326     c                   eval      argpoc = ILRL1lna
073700080115     c                   else
073800080115     c                   eval      argpoc = d55tfa
073900080115     c                   endif
074000070326     c                   eval      argttl = *blanks
074100070326     c                   eval      argtgi = 'M'
074200070326     c*
074300070326     c                   select
074400070326     c                   when      arblna=101
074500070326     c                   eval      argcgi='101'
074600070326     c
074700070326     c                   when      arblna=102
074800070326     c                   eval      argcgi='102'
074900070326     c                   when      wforza='C'
075000070326     c                   eval      argcgi='CS '
075100070716     c                   when      wforza='S'
075200070716     c                   eval      argcgi='RSM'+%trim(arbCAD)
075300070326     c                   endsl
075400070716     c*
075500070326     c                   write     fiarg000                             99
075600070326     c                   endif
075700070326     c                   ENDSR
075800060829     C*------------------------------------------------------------------------*
075900060829     C* EXETISI95 - Routine di reperimento dati instradamento per recupero ambito
076000060829     C*------------------------------------------------------------------------*
076100060829     C     EXETISI95     BEGSR
076200070430     c* Determino peso e volume VLD da utilizzare per l'instradamento
076300070430     c*  anche se diverso dal bollettato e da un risultato diverso dalla
076400070430     c*  bolla, meglio essere precisi il + possibile un qeusto momento
076500070430     c                   clear                   wvol
076600070430     c                   clear                   wpes
076700070430     c                   if        arbncr=arbncl
076800070430     C                   Z-ADD     arbVLC        WVOL
076900070430   X4C                   ELSE
077000070430    5C     arbVLC        IFGE      arbVLF
077100070430     C                   Z-ADD     arbVLC        WVOL
077200070430   X5C                   ELSE
077300070430     C                   Z-ADD     arbVLF        WVOL
077400070430    5C                   ENDIF
077500070430    4C                   ENDIF
077600070430     C* peso   : - SE TOTALE  , SOMMO arbpkc
077700070430     C*          - SE PARZIALE, SOMMO arbPKC (SE MAGGIORE)
077800070430     C*                               ALTRIMENTI SOMMO BLPPKF
077900070430     C     arbNCp        ifeq      arbNCL
078000070430     C                   Z-ADD     arbpkC        Wpes
078100070430     C*
078200070430   X4C                   ELSE
078300070430    5C     arbpkC        IFGE      arbpkF
078400070430     C                   Z-ADD     arbpkC        Wpes
078500070430   X5C                   ELSE
078600070430     C                   Z-ADD     arbpkF        Wpes
078700070430    5C                   ENDIF
078800070430    4C                   ENDIF
078900060829     C*
079000060829     C                   CLEAR                   TISI95DS
079100060829     C                   EVAL      I95TCN = '7'
079200080123     C***                EVAL      I95DAT = arbAAS*10000+arbMGS
079300080123     C                   EVAL      I95DAT = datcor
079400060829     C                   EVAL      I95NAR = arbNZD
079500060829     C                   EVAL      I95PRV = arbPRD
079600060829     C                   EVAL      I95CAP = arbCAD
079700060829     C                   EVAL      I95LOC = arbLOD
079800070430     C***                EVAL      I95LKG = arbPKB
079900070430     C                   EVAL      I95LKG = wpes
080000070430     C***                EVAL      I95LMC = arbVLB
080100070430     C                   EVAL      I95LMC = wvol
080200060829     C                   EVAL      I95FFD = arbFFD
080300060829     C                   EVAL      I95TSP = arbTSP
080400060829     C                   EVAL      I95FRE = 'S'
080500060829     C                   EVAL      I95TFP = arbTFP
080600060829     C                   CALL      'TISI95R'
080700060829     C                   PARM                    TISI95DS
080800060829     C*
080900070430     c* Imposto §tllsoAIN in base a cappario se:
081000070430     c* 1) non c'e' tabella              e non è supermercato
081100070430     c* 2)     c'e' tabella solo con "P"  - sempre
081200070430     c* 3)     c'e' tabella solo con "S" e non è supermercato
081300070430     c                   select
081400070430     c                   when      (§tepsup=' ' and
081500070430     c                             arbtc1<>'S' and  arbtc2<>'S') or
081600070430     c                              §tepsup='P'  and §teplkg=0 or
081700070430     c                             (§tepsup='S'  and §teplkg=0 and
081800070430     c                             arbtc1<>'S' and  arbtc2<>'S')
081900061213     c
082000060829     C* Quindi verifico, in ordine, reperimento x LOC - ECC - CAP
082100060829     C                   IF        O95ERR <> *blanks
082200060829     C                   EVAL      §TLLSOerr= 'P'
082300060829     C                   ELSE
082400060829     C*
082500060829     C* X determinare l'ambito della linea/zona d arrivo dell'instradamento reperito
082600060829     C* testo nell'ordine: LOC - ECC - CAP
082700060829     C*
082800060829     C* => LOC:
082900060829     C                   IF        O95LIV = 'L'
083000070430     C* LNA/ZNC Standard
083100070430     C                   IF        O95LNA = O95LLA AND
083200070430     C                             O95ZNC = O95LZC
083300070430     C                   EVAL      §tllsoAIN = '='
083400070430     C                   ENDIF
083500060829     C* LNA/ZNC Sotto
083600060829     C                   IF        O95LNA = O95LLS AND
083700060829     C                             O95ZNC = O95LZS
083800060925     C                   EVAL      §tllsoAIN = '<'
083900060925     C                   ENDIF
084000060829     C* LNA/ZNC Oltre
084100060829     C                   IF        O95LNA = O95LLO AND
084200060829     C                             O95ZNC = O95LZO
084300060925     C                   EVAL      §tllsoAIN = '>'
084400060925     C                   ENDIF
084500060829     C*
084600060829     C                   ELSE
084700060829     C*
084800060829     C* => ECC:
084900060829     C                   IF        O95TFP > *zeros
085000070430     C* LNA/ZNC Standard
085100070430     C                   IF        O95LNA = O95ELA AND
085200070430     C                             O95ZNC = O95EZC
085300070430     C                   EVAL      §tllsoAIN = '='
085400070430     C                   ENDIF
085500060829     C* LNA/ZNC Sotto
085600060829     C                   IF        O95LNA = O95ELS AND
085700060829     C                             O95ZNC = O95EZS
085800060925     C                   EVAL      §tllsoAIN = '<'
085900060925     C                   ENDIF
086000060829     C* LNA/ZNC Oltre
086100060829     C                   IF        O95LNA = O95ELO AND
086200060829     C                             O95ZNC = O95EZO
086300060925     C                   EVAL      §tllsoAIN = '>'
086400060925     C                   ENDIF
086500060829     C*
086600060829     C                   ELSE
086700060829     C*
086800060829     C* => CAP:
086900070430     C* LNA/ZNC Standard
087000070430     C                   IF        O95LNA = O95CLA AND
087100070430     C                             O95ZNC = O95CZC
087200070430     C                   EVAL      §tllsoAIN = '='
087300070430     C                   ENDIF
087400060829     C* LNA/ZNC Sotto
087500060829     C                   IF        O95LNA = O95CLS AND
087600060829     C                             O95ZNC = O95CZS
087700060925     C                   EVAL      §tllsoAIN = '<'
087800060925     C                   ENDIF
087900060829     C* LNA/ZNC Oltre
088000060829     C                   IF        O95LNA = O95CLO AND
088100060829     C                             O95ZNC = O95CZO
088200060925     C                   EVAL      §tllsoAIN = '>'
088300060925     C                   ENDIF
088400060829     C*
088500060829     C                   ENDIF
088600060829     C                   ENDIF
088700060829     C                   ENDIF
088800060901     c
088900060925     c* Se errore §tllsoain riempito comunque con lo Standard
089000060901     c                   if        §tllsoain=*blanks
089100060925     C                   EVAL      §tllsoAIN = '='
089200070430     C                   ENDIF
089300070430     c
089400070430     c* Se supermercato imposto ambito =  "S"
089500070612     c* Imposto §tllsoAIN = "s-supermercato" se:
089600070430     c* 1) non c'e' tabella         ed  è supermercato
089700070612     c* 3)     c'e' tabella con "S" ed  è supermercato
089800070430     c                   when      (§tepsup =' ' or §tepsup='S') and
089900070430     c                             (arbtc1='S' or arbtc2='S')
090000070612     C                   EVAL      §tllsoAIN = 's'
090100070430     c                   other
090200070430     c
090300070430     c* Imposto da tabella
090400070430     c* Limite SOTTO personalizzato
090500070430     c* I due valori dovrebbero essere in AND per il controllo
090600070430     c*  del limite sotto, ma visto che nel cappario sono in OR
090700070430     c*  anche qui li metto in OR allo stesso modo
090800070430     c                   if        wpes<§teplks or  wvol<§teplms
090900070430     C                   EVAL      §tllsoAIN = '<'
091000070430     c                   endif
091100070430     c
091200070430     c* Limite SOPRA personalizzato
091300070430     c                   if        wpes>§teplkg or  wvol>§teplmc
091400070430     C                   EVAL      §tllsoAIN = '>'
091500070430     c                   endif
091600070430     c                   if        §tllsoain=*blanks
091700070430     C                   EVAL      §tllsoAIN = '='
091800070430     C                   ENDIF
091900070430     c                   ENDSL
092000070129     c*
092100070430     c* Reimposto solo se ci sono i limiti
092200070430     c*
092300070129     c* Limite SOTTO personalizzato
092400070129     c* I due valori dovrebbero essere in AND per il controll c
092500070129     c*  del limite sotto, ma visto che nel cappario sono in OR
092600070129     c*  anche qui li metto in OR allo stesso modo
092700070430     c***                if        §tllsoain='<' and §teplks>0
092800070430     c***                if        arbpkb<§teplks or  arbvlb<§teplms
092900070430     c***                else
093000070430     C***                EVAL      §tllsoAIN = '='
093100070430     c***                endif
093200070430     c***                endif
093300070129     c
093400070129     c* Limite SOPRA personalizzato
093500070430     c***                if        §tllsoain='>' and §teplkg>0
093600070430     c***                if        arbpkb>§teplkg or  arbvlb>§teplmc
093700070430     c***                else
093800070430     C***                EVAL      §tllsoAIN = '='
093900070430     c***                endif
094000070430     c***                endif
094100060829     C*
094200070129     C******             ENDIF
094300060829     C                   ENDSR
094400060829     c*------------------------------------------------------------------
094500060829     c* Scrittura di un record in FILTE00F
094600060829     c*------------------------------------------------------------------
094700060829     c     ScriviFILTE   BEGSR
094800060829     c                   clear                   filte000
094900060831     c                   eval      lteidb=widb
095000060831     c                   eval      ltepoe=ilrl1lna
095100070919     c*****              eval      lteprg=vgdprg
095200080121     c                   eval      ltehdl='IN CORSO'
095300060831     c                   eval      ltedat=datcor
095400060831     c                   time                    lteora
095500060906     c                   eval      ltensps=contansps
095600070207     c                   clear                   DLTEFLO
095700070207     c                   eval      §FLOTST = ILRL1tst
095800070207     c                   eval      lteflo = DLTEFLO
095900060831     c                   write     filte000
096000060829     c                   endsr
