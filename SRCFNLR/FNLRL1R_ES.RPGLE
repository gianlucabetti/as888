000100941109     H*------------------------------------------------------------------------*
000200060828     H* scambio dati da Bartolini   a  Tellus                                  *
000300941109     H*------------------------------------------------------------------------*
000400930128     H DECEDIT('0,') DATEDIT(*DMY.)
000500090604     H DFTACTGRP(*NO) ACTGRP(*NEW)
000600941109     F*------------------------------------------------------------------------*
000700110627     ffigrg04l  if   e           k disk
000800110627     ftabel00f  if   e           k disk
000900060911     fTNTBE01L  IF   E           K DISK
001000060828     Ffnarb54l  IF   E           K DISK
001100060828     FFNARB01L  IF   E           K DISK
001200060828     F                                     RENAME(FNARB000:FNARB001)
001300080115     FFIARG01L  uF A E           K DISK
001400070328     FFNLBL02L  IF   E           K DISK
001500070328     FFNLBL01L  IF   E           K DISK
001600070328     F                                     RENAME(FNlbl000:FNlbl001)
001700060829     F
001800090604     FTIVGD00F  uf a E             DISK
001900070919     FFILTE01L  uf a E           K DISK
002000070124     d
002100070124     D cchco           s              2    dim(30)
002200070124     D REC             S              2    DIM(50)
002300070326     D CS              S              2    DIM(50)
002400110627     D GiriSt          S             10    DIM(50)
002500060828     d
002600070124     d* DS ricevuta: p.o. in gestione da elaborare
002700060831     d FNLRL1DS        ds
002800060831     d  ILRL1lna                      3s 0
002900070406     d  ILRL1dat                      8s 0
003000060905     d  ILRL1dis                      1a
003100070207     d  ILRL1tst                      1a
003200070523     d  ILRL1SenGiro                  1a
003300070529     d  LRL1contrBL                   1a
003400070529     d  LRL1botlls                    2s 0
003500070529     d* Dati di output
003600070529     d* "S"--> numero minimo bolle da elaborare non raggiunto
003700070529     d  LRL1Ominimo                   1a
003800070529     d  LRL1Onumbolle                 3s 0
003900060831     d
004000070207     D DLTEFLO       E DS
004100060829     D DTLLSO        E DS
004200070129     D DTEP          E DS
004300101119     D Dvpotellus    E DS
004400110627     D Dvpodeco      E DS
004500060828     D KPJBA         E DS
004600070129     D tibs02ds      E DS
004700070129     D TISI95DS      E DS
004800060829     D TIS781DS      E DS
004900070207     D TIS781DFLO    E DS
005000070305     D DVGDFLO       E DS
005100060829     D TRUL33DS      E DS
005200080115     D FNLV55DS      E DS
005300060828     D trul06ds      E DS
005400060828     D  L6                     1     90  0 dim(30)
005500060828      * ds per il controllo della presenza di CA per la spedizione richiesta
005600060828     d FIDN12DS      e ds
005700060828     d  skKey                 26    305    dim(20)
005800060828     d  skAnn                306    325    dim(20)
005900060828     d  skDca                326    485  0 dim(20)
006000060828     d  skFca                486    545  0 dim(20)
006100060828     d  skDch                546    705  0 dim(20)
006200060828     d  skCch                706    745    dim(20)
006300060828
006400060828     d dsKey           ds
006500060828     d  dsaac                         4  0
006600060828     d  dsfil                         3  0
006700060828     d  dsnca                         7  0
006800060828      *
006900060828     D ARBDS         E DS                  EXTNAME(FNARB00F)
007000060828     D WSAVDS          DS           700
007100060911     D DCCH          E DS
007200070124     D DS3A          E DS
007300110627     D dgrgflo       E DS
007400060424     d
007500060828     d II              s              2  0 inz(0)
007600060828     d xx              s              3  0 inz(0)
007700060828     d yy              s              3  0 inz(0)
007800060828     d KFIL            s              3  0
007900060828     d KDCM            s              8  0 inz(0)
008000060831     d Wstacca         s              1    inz(' ')
008100060906     d contaNSPS       s                   like(LTENSPS)
008200060831     d WIDB            s                   like(LTEIDB)
008300060828     d Escbolla        s              1
008400080116     d Wcambiapoc      s              1
008500080116     d POL6            s              1
008600060911     D Wtbecod         S                   LIKE(TBEcod)
008700060829     d Dataiso         s               d   datfmt(*iso)
008800060829     d Dataeur         s               d   datfmt(*eur)
008900070124     d ktbkut          s                   like(tblkut) inz(1)
009000070124     d ktbcod          s                   like(tblcod)
009100070430     d wvol            s                   like(arbvlf)
009200070430     d wpes            s                   like(arbpkf)
009300941109     C*------------------------------------------------------------------------*
009400060424     c     *entry        plist
009500060424     c                   parm                    kpjba
009600060831     c                   movel     kpjbu         fnlrl1ds
009700070529     c
009800070529     c                   clear                   lrl1Ominimo
009900070529     c                   clear                   lrl1Onumbolle
010000101119     c                   clear                   wstacca
010100070529     c*
010200070529     c                   if        lrl1contrbl='S'
010300070601     c                   eval      lrl1Ominimo='S'
010400070529     c                   endif
010500070406     c*
010600070406     c* Reperisco versioni giri x filiale/data richieste
010700070406     c                   call      'FIDG11R1'
010800070406     c                   parm                    ILRL1lna
010900070406     c                   parm                    ILRL1dat
011000070406     c                   parm      *blanks       Out_KeyVer        8
011100070416     c                   parm      *blanks       Out_VerDes       30
011200070406     c                   parm      *blanks       Out_Esito         1
011300080115     c
011400080115     c* Verifico se è un p.o. di £6
011500080115     c                   clear                   fnlv55ds
011600080115     c                   movel     ilrl1lna      d55lin
011700080115     c                   movel     ilrl1dat      d55drf
011800080115     c                   movel     '6'           d55tpt
011900080115     c                   movel     'L'           d55tla
012000080115     c                   call      'FNLV55R'
012100080115     c                   parm                    fnlv55ds
012200080115     c
012300080115     c                   clear                   pol6
012400080115     c                   if        d55tfa<>ilrl1lna
012500080115     c                   eval      pol6='S'
012600080115     c                   endif
012700070406     c*
012800080115     c* Carico la £6 alla data di elaborazione
012900060828     c                   clear                   trul06ds
013000060831     c                   movel     IlRL1LNA      d06key
013100080115     c                   move      'S'           d06key
013200060828     c                   movel     '£6'          d06cod
013300080115     c                   movel     ilrl1dat      d06drf
013400080115     c                   movel     trul06ds      kpjbu
013500060828     c                   call      'TRUL06R'
013600060828     c                   parm                    kpjba
013700060828     c                   movel     kpjbu         trul06ds
013800070129     c
013900070129     C                   CLEAR                   tibs02ds
014000070129     c                   clear                   dtep
014100070129     C                   MOVEL     'C'           T02MOD
014200070129     C                   MOVEL     KNSIF         T02SIF
014300070129     C                   MOVEL     'TEP'         T02COD
014400070129     c                   movel(P)  ilrl1lna      t02ke1
014500070129     C                   CALL      'TIBS02R'
014600070129     C                   PARM                    KPJBA
014700070129     C                   PARM                    tibs02ds
014800070129    2C                   IF        T02ERR = *BLANKS
014900070129     C                   MOVEL     T02uni        dtep
015000070129    2C                   ENDIF
015100070129     c
015200101119     c* Carico il numero max di bolle per file
015300101119     C                   CLEAR                   tibs02ds
015400101119     c                   clear                   bollemax
015500101119     C                   MOVEL     'C'           T02MOD
015600101119     C                   MOVEL     KNSIF         T02SIF
015700101119     C                   MOVEL     'VPO'         T02COD
015800101119     c                   movel(P)  'TELLUS'      t02ke1
015900101119     C                   CALL      'TIBS02R'
016000101119     C                   PARM                    KPJBA
016100101119     C                   PARM                    tibs02ds
016200101119    2C                   IF        T02ERR = *BLANKS
016300101119     C                   MOVEL     T02uni        dvpotellus
016400101119     C                   MOVEL     §vpobomax     bollemax          5 0
016500101119    2C                   ENDIF
016600110627     c
016700110627     c* Carico data attivazione centro storico
016800110627     C                   CLEAR                   tibs02ds
016900110627     c                   clear                   dvpodeco
017000110627     C                   MOVEL     'C'           T02MOD
017100110627     C                   MOVEL     KNSIF         T02SIF
017200110627     C                   MOVEL     'VPO'         T02COD
017300110627     c                   movel(P)  'DECO'        t02ke1
017400110627     C                   CALL      'TIBS02R'
017500110627     C                   PARM                    KPJBA
017600110627     C                   PARM                    tibs02ds
017700110627    2C                   IF        T02ERR = *BLANKS
017800110627     C                   MOVEL     T02uni        dvpodeco
017900110627     c                   else
018000110627     c                   eval      §vpocsto=20391231
018100110627    2C                   ENDIF
018200110627     c
018300110627     c* Se udate >= data attivazione --> leggo i giri della filiale che sono centri storici
018400110627     c*  e li carico in skiera
018500110627     c                   if        datcor>=§vpocsto
018600110627     c     ILRL1lna      setll     figrg04l
018700110627     c     ILRL1lna      reade     figrg04l
018800110627     c
018900110627     c                   dow       not %eof(figrg04l)
019000110627     c                   if        grgatb=' ' and ILRL1dat>=grgdde and
019100110627     c                             ILRL1dat <=grgscad
019200110627     c                   eval      dgrgflo=grgflo
019300110627     c                   if        §grgcsto='S'
019400110627     c                   add       1             xx
019500110627     c                   eval      giriSt(xx) =grgcgi
019600110627     c
019700110627     c     ILRL1lna      reade     figrg04l
019800110627     c                   enddo
019900060828     c
020000060828     c* elaborazione per linea di arrivo della £6 caricata
020100060828     c                   z-add     1             xx
020200060828    4c                   dow       l6(xx)>0
020300060828     c                   eval      kfil=l6(xx)
020400060828     c
020500060828     c     karb          setll     fnarb54l
020600060828     c     karb          reade     fnarb54l
020700070529     c
020800060828    6c                   dow       not %eof(fnarb54l)
020900060828     c
021000060828     c                   exsr      ESCLUDI
021100080116     c
021200080116     c* Se cambia il p.o. di consegna, aggiorno FIARG
021300080118    7c****               if        Escbolla='S' and wcambiapoc='S'
021400080118     c
021500080118    7c                   if        Wcambiapoc='S'
021600080116    8c                   if        lrl1contrbl='S'
021700080116     c                   clear                   Escbolla
021800080116   x8c                   else
021900080116     c
022000080116     c     karb3         chain     fiarg01l
022100080116     c* se è uno dei giri forzati, aggiorno solo p.o. consegna
022200080116     c*  altrimenti pulisco i campi
022300080116    9c                   if        (argcgi='101' or argcgi='102' or
022400080116     c                             argcgi='CS ' or (%subst(argcgi:1:7)=
022500080116     c                             'RSM4789')) and argtgi='M'
022600080116     c
022700080116   10c                   if        pol6=' '
022800080116     c                   eval      argpoc=ilrl1lna
022900080116     c                   else
023000080116     c                   eval      argpoc=d55tfa
023100080116   10c                   endif
023200080116     c
023300080116   x9c                   else
023400080116     c                   clear                   argcgi
023500080116     c                   clear                   argtgi
023600080116     c                   clear                   argttl
023700080116     c                   clear                   argpoc
023800080116     c                   clear                   argsqg
023900080116     c                   clear                   argidb
024000080118     c                   clear                   arglno
024100080118     c                   clear                   argerr
024200080116     c                   clear                   Escbolla
024300080116    9c                   endif
024400080116     c
024500080116     c                   update    fiarg000
024600080116    8c                   endif
024700080116    7c                   endif
024800110627     c* Se devo aggiornare le bolle e c'e' già il giro --> verifico se mettere o
024900110627     c*  togliere il centro storico
025000110627     c                   if        lrl1contrbl=' ' and argcgi<>*blanks
025100110627     c                   eval      Indx=%lookup(argcgi:giriSt)
025200110627     c                   if        Indx>0
025300060828     c
025400060828    7c                   if        Escbolla=' '
025500070529     c* Verifico se devo solo contare le bolle elaborabili
025600070529     c                   if        lrl1contrbl='S'
025700070529     c                   add       1             lrl1Onumbolle
025800070529     c                   else
025900101119     c* Se ho scritto già numero bolle massimo, eseguo invio prima di procedere
026000101119     c                   if        contansps>bollemax
026100101119     c                   exsr      InvATellus
026200101119     c                   endif
026300101119     c
026400060828     c                   exsr      ELABORA
026500070529     c                   endif
026600080116     c                   endif
026700080116     c*
026800060828     c
026900070529     c                   if        lrl1contrbl=' ' or
027000070529     c                             lrl1Onumbolle<=lrl1botlls
027100060828     c     karb          reade     fnarb54l
027200070529     c                   else
027300070601     c* Nel controllo ho già verificato che ci sono da elaborare +
027400070529     c*  bolle del minimo richiesto--> esco dal pgm
027500070529     c                   eval      lrl1Ominimo=' '
027600070529     c                   leave
027700070529     c                   endif
027800070529     c
027900060828    6c                   enddo
028000060828     c
028100060828     c                   add       1             xx
028200060828    4c                   enddo
028300930128     C*
028400101119     c
028500060829     c* Finita l'elaborazione chiamo pgm per invio dati a TELLUS
028600060829     c* Imposto la DS
028700070529     c* solo se non è richiamo di controllo
028800070919    1c                   if        lrl1contrbl=' '
028900101119     c                   exsr      InvATellus
029000070620     c
029100070919   x1c                   else
029200070529     c
029300070529     c                   movel     fnlrl1ds      kpjbu
029400070919    1c                   endif
029500060829     c
029600000000     C                   SETON                                        LR
029700060828     c*------------------------------------------------------------------
029800060828     c     *INZSR        BEGSR
029900060828     c     KARB          klist
030000060828     c                   kfld                    kfil
030100060828     c                   kfld                    kdcm
030200060828     C     KARB3         KLIST
030300060828     C                   KFLD                    ARBAAS
030400060828     C                   KFLD                    ARBLNP
030500060828     C                   KFLD                    ARBNRS
030600060828     C                   KFLD                    ARBNSP
030700060828     C     KARB1         KLIST
030800060828     C                   KFLD                    LBLAAN
030900060828     C                   KFLD                    LBLLPN
031000060828     C                   KFLD                    LBLNRN
031100060828     C                   KFLD                    LBLNSN
031200070124     C     KE2TAB        KLIST
031300070124     C                   KFLD                    KTBKUT
031400070124     C                   KFLD                    KTBCOD
031500060829     c* Data di elaborazione
031600060829     c                   time                    w0140            14 0
031700060829     C                   MOVE      w0140         w0080             8 0          *DATA (8) IN G/M/AA
031800060829     c     *eur          movel     w0080         dataeur
031900060829     c                   movel     dataeur       dataiso
032000060829     c     *iso          movel     dataiso       datcor            8 0
032100060911     c
032200060911     c* Carico le causali chiusura c,a, che prevedono la consegna della
032300060911     c*  merce
032400060911     c                   z-add     *zeros        xx
032500060911     C                   MOVEL     'CCH'         Wtbecod
032600060911     c     Wtbecod       chain     tntbe000                           31
032700060911      *
032800060911     c                   DOW       *in31 = *off
032900060911     c                   movel     TBEuni        DCCH
033000060911      * Se il S.I. è indicato deve essere uguale al mio, l'anomalia non deve essere per gli Eventi
033100060911     c                   IF        (TBEsif = knsif  or  TBEsif = *blanks)
033200060911     c                             and TBEatb = *BLANKS  and  §cchccco= 'S'
033300060911     c                   add       1             xx
033400060911     c                   movel     TBEke1        cchco(XX)
033500060911     c                   endif
033600060911     c     Wtbecod       reade     tntbe000                               31
033700060911     c                   enddo
033800070124     c
033900070326     c* Carico tipi record di recupero  e C/S
034000070124     C                   Z-ADD     1             KTBKUT
034100070124     C                   MOVE      '3A'          KTBCOD
034200070124     C                   CLEAR                   IY                4 0
034300070326     C                   CLEAR                   IZ                4 0
034400070124     C     KE2TAB        SETLL     TABEL00F
034500070124     C     KE2TAB        READE     TABEL00F                               98
034600070124     C     *IN98         DOWEQ     *OFF
034700070124     C                   MOVEL     TBLUNI        DS3a
034800070412     c* Recuperi
034900070124     C     §3arbl        IFEQ      'R'
035000070124     C                   ADD       1             IY
035100070124     C                   MOVEL     TBLKEY        rec(IY)
035200070124     C                   ENDIF
035300070412     c* C/S
035400070326     C     §3arbl        IFEQ      'C'
035500070412     c     tblkey        andne     '£ '
035600070412     c* Escludo forzatamente bolle "£ "
035700070326     C                   ADD       1             IZ
035800070326     C                   MOVEL     TBLKEY        cs(IZ)
035900070326     C                   ENDIF
036000070124     C     KE2TAB        READE     TABEL00F                               98
036100070124     C                   ENDDO
036200070124     c
036300060829     c
036400060828     c                   ENDSR
036500101119     c*------------------------------------------------------------------
036600101119     c*  Trasmissione dati a Tellus
036700101119     c*------------------------------------------------------------------
036800101119     c     InvATellus    BEGSR
036900101119    2c                   if        contansps>0
037000101119     c
037100101119     c* Scrittura di un record in FILTE00F
037200101119     c                   exsr      ScriviFILTE
037300101119     c
037400101119     c                   clear                   tis781ds
037500101119     c                   eval      §781tip='SO'
037600101119    3c                   if        ILRL1tst = 'S'
037700101119     c                   eval      §781ksu='00TELTST'
037800101119     c                   else
037900101119     c                   eval      §781ksu='00TELLUS'
038000101119    3c                   endif
038100101119     c                   eval      §781tsc='WW'
038200101119     c                   eval      §781daT=datcor
038300101119     c                   eval      §781pgm='TLL' + %editc(widb:'X')
038400101119     c* Costruisco il nome da dare al file d output finale
038500101119     c*                  eval      §781out='S'+%editc(ILRL1lna:'X')+
038600101119     c*                            %subst(%editc(widb:'X'):2:6)                 * 'S'+FIL+IDB.TXT
038700101119     c                   eval      §781out='S'+%subst(%editc(widb:'X'):2:6)+
038800101119     c                             %editc(ILRL1lna:'X')                         * 'S'+IDB+FIL.TXT
038900101119     c* Imposto la "S" per il cambio del S.I.
039000101119     c                   clear                   TIS781DFLO
039100101119     c                   movel     'S'           §781flocsi
039200101119     c                   movel     'P'           §781floela
039300101119     c                   movel     TIS781DFLO    §781flo
039400101119     c*
039500101119     c* Forzo lo scarico del buffer d output del TIVGD
039600101119     c                   feod      TIVGD00F
039700101119     c
039800101119     c* Finita l'elaborazione chiamo pgm
039900101119     c                   CALL      'TIS781C1'
040000101119     c                   parm      *blanks       ESITO             1
040100101119     c                   parm                    tis781ds
040200101119     c                   parm      *blanks       vgdprg           10
040300101119     c
040400101119     c* richaino filte per aggiornare vgdprg
040500101119     c     widb          chain(e)  filte01l
040600101119    3c                   if        not %error
040700101119    4c                   if        vgdprg=*blanks
040800101119     c                   eval      ltehdl='ANNULLATO'
040900101119     c                   eval      lteprg='*NULL'
041000101119     c                   else
041100101119     c                   eval      ltehdl=*blanks
041200101119     c                   eval      lteprg=vgdprg
041300101119    4c                   endif
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
