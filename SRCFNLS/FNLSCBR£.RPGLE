000100120810     /*END
000200950523     H DECEDIT('0,') DATEDIT(*DMY.)
000300120810     h option(*nodebugio)
000400000908
000500000523      *****************************************************************
000600000908      *                                                               *
000700000908      *    Utility   CANCELLAZIONE BOLLE PARTENZA                     *
000800000908      *                                                               *
000900000908      *****************************************************************
001000000908
001100000908     FTABEL00F  IF   E           K DISK
001200000919     FAZORG01L  IF   E           K DISK
001300000908     FCNACO00F  IF   E           K DISK
001400030220     FFNBLP70L  IF   E           K DISK
001500000920     FFNBLP01L  IF   E           K DISK    RENAME(FNBLP000:FNBLP1)
001600000908     FFNBLT01L  IF   E           K DISK
001700000920     FFNBLT27L  IF   E           K DISK    RENAME(FNBLT000:FNBLT27)
001800070116     FFNBRV07L  IF   E           K DISK
001900120810     FFiVAB02L  UF   E           K DISK
002000030220     FFiVAD01L  UF   E           K DISK
002100030220     FFiVAT01L  UF   E           K DISK
002200040604     FFiVAX01L  UF   E           K DISK
002300120810     ffirab02l  Uf   e           k disk
002400000908     FFNLSCBD   CF   E             WORKSTN
002500000908
002600000919     D L1              S              3  0 DIM(30)
002700030220     d Lnp             s              3  0 Dim(30)
002800000908     D paxccc          s                   like(acokcc)
002900000908     D paxdmt          s                   like(acorag)
003000000908     D keyaas          s                   like(blpaas)
003100000908     D keymgs          s                   like(blpmgs)
003200000908     D keycod          s                   like(tblcod)
003300000908     D keykey          s                   like(tblkey)
003400030220     d KeyLnp          s                   Like(BlpLnp)
003500120810     d KEYfgs          s                   like(VABfgs)  inz
003600030220     d w0030           s              3  0
003700030220     d xx              s              2  0
003800000908
003900900514     D KPJBA         E DS
004000000908     D DS3A          E DS
004100000920     D OG143         E DS
004200000908     D DSLS63        E DS                  EXTNAME(FNLS63DS)
004300030220     d Fnlse5ds      e ds
004400091130     D trtc86drds    E DS
004500000908
004600000908      * Parametri azienda e tabelle ricerca conti
004700000908
004800120810       // -?Parametri x Controllo profilo utenti?
004900120810     d TIBS34ds      e ds
005000120810       // -?Ds di riferimento al file esterno AZUTE00F?
005100120810     d AZUTEds       e ds                  extname(AZUTE00F)
005200120810       // -?Ds per dati organigramma?
005300120810     d dDATIUTE      e ds
005400120810
005500000919     D DSUL06        E DS                  EXTNAME(TRUL06DS)
005600000919     D  LIN                    1     90  0
005700000919     D                                     DIM(30)
005800000908
005900000908      * Parametri per controllo data e transform in giorni - XSRDA8 -
006000000908
006100000523     D WLBDAT          DS                  INZ
006200000523     D  G02DAT                 1      8  0
006300000523     D  G02INV                 9     16  0
006400000523     D  G02ERR                17     17
006500000523     D  G02TGI                18     22  0
006600000908
006700000523      *****************************************************************
006800000908      *                                                               *
006900000908      *    OPERAZIONI INIZIALI                                        *
007000000908      *                                                               *
007100000523      *****************************************************************
007200000908
007300000523     C     *ENTRY        PLIST
007400000523     C                   PARM                    KPJBA
007500000908
007600000908      *    Recupero ragione sociale ditta da emettere a video
007700000908
007800120810     c     *dtaara       define    §azute        AZUTEds
007900120810     c     *dtaara       define    §datiute      dDATIUTE
008000120810     c                   in(E)     *dtaara
008100120810     c                   if        %ERROR or RSUT = *blanks
008200120810     c                   clear                   Tibs34Ds
008300120810     c                   call      'TIBS34R'
008400120810     c                   parm                    Tibs34Ds
008500120810     c                   in        *dtaara
008600120810     c                   endif
008700120810
008800000919     C                   CLEAR                   DSUL06
008900000919     C                   MOVE      '£1'          D06COD
009000000919     C                   MOVEL     SIMFEL        D06KEY
009100000919     C                   MOVEL     DSUL06        KPJBU
009200000919     C                   CALL      'TRUL06R'
009300000919     C                   PARM                    KPJBA
009400000919     C                   MOVEL     KPJBU         DSUL06
009500000919     C                   MOVEA     LIN           L1
009600000908
009700120810     C                   Z-ADD     1             CODUT             1 0
009800000908
009900000908      *    Accesso a CNACO00F
010000000908
010100000908     C     KACO          KLIST
010200000908     C                   KFLD                    CODUT
010300120810     C                   KFLD                    dutKCI
010400000908     C                   KFLD                    VIDCLI
010500000908
010600000908      *    Accesso a TABEL00F
010700000908
010800000908     C     KTAB          KLIST
010900000908     C                   KFLD                    CODUT
011000000908     C                   KFLD                    KEYCOD
011100000908     C                   KFLD                    KEYKEY
011200000908     C                   move      '3A'          KEYCOD
011300000908
011400000908      *    Accesso a FNBLP40L
011500000908
011600000908     C     KBLP          KLIST
011700030220     c                   Kfld                    KeyLnp
011800000908     C                   KFLD                    KEYAAS
011900000908     C                   KFLD                    KEYMGS
012000000908
012100000908      *    Accesso a FNBLT01L
012200000908
012300000908     C     KBLT          KLIST
012400000908     C                   KFLD                    BLPAAS
012500000908     C                   KFLD                    BLPLNP
012600000908     C                   KFLD                    BLPNRS
012700000908     C                   KFLD                    BLPNSP
012800000908
012900070116      *    Accesso a FNBRV07L
013000000908
013100000908     C     KBRV          KLIST
013200000908     C                   KFLD                    BLTFLS
013300000908     C                   KFLD                    BLTLNA
013400000908     C                   KFLD                    BLTNRS
013500000908     C                   KFLD                    BLTNSC
013600000920
013700000920      *    Accesso a FNBLP01L
013800000920
013900000920     C     KBLP1         KLIST
014000000920     C                   KFLD                    BLTAAS
014100000920     C                   KFLD                    BLTLNP
014200000920     C                   KFLD                    BLTNRS
014300000920     C                   KFLD                    BLTNSP
014400000920
014500000920      *    Accesso a FNBLT27L
014600000920
014700000920     C     KBLT27        KLIST
014800000920     C                   KFLD                    VABLNP
014900000920     C                   KFLD                    VABLNA
015000000920     C                   KFLD                    VABNRS
015100000920     C                   KFLD                    VABNCD
015200120810
015300120810      *    Accesso a FIVAB02L
015400120810
015500120810     C     K02VAB02      klist
015600120810     C                   kfld                    KEYfgs
015700120810     C                   kfld                    VIDcli
015800000920
015900030220      *    Accesso a FiVAD01L e FiVAT01L
016000000920
016100000920     C     KVADET        KLIST
016200030220     c                   Kfld                    VabFgs
016300000920     C                   KFLD                    VABCCM
016400000920     C                   KFLD                    VABAAS
016500000920     C                   KFLD                    VABLNP
016600000920     C                   KFLD                    VABNRS
016700000920     C                   KFLD                    VABNSP
016800000908
016900000523      *****************************************************************
017000000908      *                                                               *
017100000908      *    ROUTINE PRINCIPALE                                         *
017200000908      *                                                               *
017300000523      *****************************************************************
017400000908
017500000908      *    Imposto valori iniziali file video
017600000908
017700120810     C                   z-add     DUTpou        vidlnp
017800120810     C                   clear                   viddea
017900000908     C                   z-add     0             vidcli
018000000908     C                   clear                   viddes
018100000919     C                   z-add     0             vidnrs
018200000919     C                   move      'N'           vidcon
018300000908     C                   move      *DATE         viddts
018400000908     C                   move      '2'           vidspu
018500000919     C                   move      'N'           vidnco
018600000919     C                   move      '2'           viddop
018700000908     C                   move      'S'           vidsim
018800000919     C                   clear                   vidsc1
018900000919     C                   clear                   vidsc2
019000120810
019100120810     C     VIDlnp        chain     AZORG
019200120810     C                   if        %found(AZORG01L)
019300120810     C                   movel     ORGdes        VIDdea
019400120810     C                   endif
019500000908
019600000908      *    ===============
019700000908      *    EMISSIONE VIDEO
019800000908      *    ===============
019900000908
020000000523     C     EMET0         TAG
020100000908     C                   EXFMT     fnlscb1
020200000908
020300000908      *    F3=Fine
020400000908
020500000908     C   KC              goto      FINE
020600000908
020700000908      *    ===========================
020800000908      *    CONTROLLO PARAMETRI IMMESSI
020900000908      *    ===========================
021000000908
021100000908     C                   setoff                                       0304
021200000919     C                   clear                   vidsc1
021300000919     C                   clear                   vidsc2
021400000908
021500000919      *    1) Ricerca Cliente Mittente
021600000908
021700000908     C     viddes        ifne      *blanks
021800000908     C     vidcli        andeq     0
021900000908
022000000908     C                   movel     rsut          PAXDUT           30
022100000908     C                   z-add     9             PAXSTA
022200120810     C                   z-add     dutKCI        PAXCCC
022300000908     C                   movel(P)  viddes        PAXDMT
022400000908     C                   z-add     1             PAXNUM
022500000908
022600000908     C                   CALL      'XALFA3BR'
022700000908     c                   PARM                    PAXDUT
022800000908     C                   PARM                    CODUT
022900000908     C                   PARM                    PAXDMT
023000000908     C                   PARM                    PAXCCC
023100000908     C                   PARM                    PAXSTA            1 0
023200000908     C                   PARM                    PAXFLR           90
023300000908     C                   PARM                    PAXDIT            3
023400000908     C                   PARM                    PAXNUM            2 0
023500000908     C                   PARM                    PAXKCM           80
023600000908     C                   PARM                    PAXKSM          140
023700000908     C                   PARM                    PAXKDM           60
023800000908
023900000908    2C     PAXSTA        IFGT      -1
024000000908     C                   movel     paxksm        vidcli
024100000908     C                   movel(P)  paxdmt        viddes
024200000908     C                   else
024300000908     C                   clear                   vidcli
024400000908     C                   endif
024500000908     C                   goto      EMET0
024600000908      *                  ---------------
024700000908
024800000908     C                   endif
024900000908
025000120810      *    2) Controllo Fil. Partenza
025100120810
025200120810     C     vidlnp        ifeq      0
025300120810     C                   clear                   viddea
025400120810     C                   write     FNLSCB1
025500120810     C                   seton                                        20
025600120810     C                   goto      EMET0
025700120810      *                  ---------------
025800120810     C                   endif
025900120810
026000120810     c                   Clear                   Og143
026100120810     C     vidlnp        chain     azorg01l
026200120810      *
026300120810     C                   IF        %found
026400120810      *
026500120810     C                   movel     orgdes        viddea
026600120810     C                   write     FNLSCB1
026700120810     C                   Movel     Orgde3        Og143
026800120810     C     orgfva        ifne      ' '
026900120810     C                   seton                                        22
027000120810     C                   goto      EMET0
027100120810      *                  ---------------
027200120810     C                   endif
027300120810     C     orgfag        ifne      'A'
027400120810     C     orgfag        andne     'F'
027500120810     C                   seton                                        22
027600120810     C                   goto      EMET0
027700120810      *                  ---------------
027800120810     C                   endif
027900120810      *   non può essere un p.o. Poste
028000120810     c                   If        §OgNtw = 'PPT'
028100120810     c                   Eval      *In28 = *On
028200120810     c                   GoTo      Emet0
028300120810      *                  ---------------
028400120810     c                   EndIf
028500120810      *   BLPLNP o VABFGS dev'essere comunque nella £1 dell'utente
028600120810     C*//  vidcon        ifeq      'S'
028700120810     C     vidlnp        lookup    L1                                     27
028800120810     C     *in27         ifeq      *off
028900120810     C                   seton                                        23
029000120810     C                   goto      EMET0
029100120810      *                  ---------------
029200120810     C                   endif
029300120810     C*//                endif
029400120810      *
029500120810     C                   ELSE
029600120810      *
029700120810     C                   eval      viddea = *all'? '
029800120810     C                   write     FNLSCB1
029900120810     C                   seton                                        22
030000120810     C                   goto      EMET0
030100120810      *                  ---------------
030200120810      *
030300120810     C                   ENDIF
030400120810
030500120810      *    3) O Cliente O Serie
030600000919
030700000919     C     vidcli        ifeq      0
030800000919     C     vidnrs        andeq     0
030900000919     C                   clear                   viddes
031000000919     C                   seton                                        21
031100000919     C                   goto      EMET0
031200000919      *                  ---------------
031300000919     C                   endif
031400000919
031500000919     C     vidcli        ifne      0
031600120810     C     vidnrs        andne     0
031700000919     C                   clear                   viddes
031800000919     C                   seton                                        21
031900000919     C                   goto      EMET0
032000000919      *                  ---------------
032100000919     C                   endif
032200000919
032300000919      *    4) Controllo Cliente in Anagrafica
032400000919
032500000919     C     vidcli        ifne      0
032600000908     C     kaco          chain     CNACO00F
032700000908     C                   IF        %FOUND
032800000908     C     acoflg        ifeq      '*'
032900000908     C                   clear                   viddes
033000000908     C                   seton                                        01
033100000908     C                   goto      EMET0
033200000908      *                  ---------------
033300000908     C                   else
033400000908     C                   movel     acorag        viddes
033500000908     C                   endif
033600000908     C                   ELSE
033700000908     C                   clear                   viddes
033800000908     C                   seton                                        01
033900000908     C                   goto      EMET0
034000000908      *                  ---------------
034100000908     C                   ENDIF
034200030220      *   non può essere un cliente Poste
034300030220     c                   Clear                   Og143
034400030220     c                   Movel     Vidcli        w0030
034500030220     c     w0030         Chain     Azorg01l
034600030220     c                   If        %Found(Azorg01l)
034700030220     c                   Movel     Orgde3        Og143
034800030220     c                   EndIf
034900030220     c                   If        §OgNtw = 'PPT'
035000030220     c                   Eval      *In26 = *On
035100030220     c                   GoTo      Emet0
035200030220      *                  ---------------
035300030220     c                   EndIf
035400000919     C                   else
035500000919     C                   clear                   viddes
035600000919     C                   clear                   vidcli
035700000919     C                   endif
035800000919
035900120810      *    5) Cancellazione Confermate o NON Confermate
036000000919
036100000919     C     vidcon        ifeq      'S'
036200000919     C     vidnco        andeq     'S'
036300000919     C                   seton                                        41
036400000919     C                   goto      EMET0
036500000919      *                  ---------------
036600000919     C                   endif
036700000919
036800000919     C     vidcon        ifeq      'N'
036900000919     C     vidnco        andeq     'N'
037000000919     C                   seton                                        41
037100000919     C                   goto      EMET0
037200000919      *                  ---------------
037300000919     C                   endif
037400000919
037500120810      *    6) Data
037600000908
037700000919     C     vidcon        ifeq      'S'
037800000908     C                   Z-ADD     viddts        G02DAT
037900000523     C                   MOVEL     *BLANK        G02ERR
038000000523     C                   CALL      'XSRDA8'
038100000523     C                   PARM                    WLBDAT
038200000523     C                   MOVEL     G02INV        DATA$$            8 0
038300000908
038400000523     C     G02ERR        IFEQ      '1'
038500000908     C     viddts        OREQ      0
038600000908     C                   SETON                                        02
038700000908     C                   goto      EMET0
038800000908      *                  ---------------
038900000523     C                   ENDIF
039000000908     C                   Z-ADD     G02DAT        viddts
039100000919     C                   endif
039200000908
039300120810      *    7) Non premuto F6
039400000908
039500000908     C  NKF              goto      EMET0
039600000908      *                  ---------------
039700000908
039800000908      *    ====================================
039900000908      *    CONTROLLI PASSATI: CANCELLO LE BOLLE
040000000908      *    ====================================
040100000919
040200000919     C                   WRITE     fnlscb2
040300000919     C                   seton                                        0304
040400000919     C     vidsim        ifeq      'A'
040500000919     C                   seton                                        10
040600000919     C                   else
040700000919     C                   setoff                                       10
040800000919     C                   endif
040900000919     C                   z-add     0             VIDNRB
041000000908
041100000919     C     vidcon        ifeq      'S'
041200000919     C                   move      '<----'       vidsc1
041300000908     C                   exsr      DLTBLP
041400000919      *                  ================
041500000919     C                   endif
041600000919
041700000919     C     vidnco        ifeq      'S'
041800000919     C                   move      '<----'       vidsc2
041900000919     C                   exsr      DLTVAB
042000000919      *                  ================
042100000919     C                   endif
042200000919
042300000908     C                   goto      EMET0
042400000908
042500000523      *    FINE PROGRAMMA
042600000908
042700000523     C     FINE          TAG
042800000925     c                   clear                   dsls63
042900000925     c                   movel     'C'           i63tla
043000000925     c                   movel     dsls63        kpjbu
043100000925     c                   call      'FNLS63R'
043200000925     c                   parm                    kpjba
043300030220
043400030220      *    Prima di fine richiamo il pgm per pulizia bolle obsolete
043500030220      *    in modalità 'C' se il richiamo precedente era andato a
043600030220      *    buon fine
043700030220     c                   If        Oe5Err <> 'E'
043800030220     c                   Clear                   Fnlse5ds
043900030220     c                   Eval      Ie5Tla = 'C'
044000030220     c                   Call      'FNLSE5R'
044100030220     c                   Parm                    Fnlse5ds
044200030220     c                   EndIf
044300000925     c*
044400000523     C                   SETON                                        LR
044500000908
044600000523      *****************************************************************
044700000908      *                                                               *
044800000908      *    SUBROUTINES                                                *
044900000908      *                                                               *
045000000523      *****************************************************************
045100000908
045200000525      *---------------------------------------------------------------*
045300000919      *    Subroutine di cancellazione bolle CONFERMATE               *
045400000525      *---------------------------------------------------------------*
045500000908
045600000908     C     DLTBLP        begsr
045700030220
045800120810     c                   clear                   LNP
045900120810     c*//                If        VidLnp > 0
046000030220     c                   Eval      Lnp(1) = VidLnp
046100120810     c*//                Else
046200120810     c*//                Movea     Lin           Lnp
046300120810     c*//                EndIf
046400000908     C                   movel     data$$        keyaas
046500000908     C                   move      data$$        keymgs
046600000908
046700030220     c                   Do        30            xx
046800120810
046900030220     c                   If        Lnp(xx) = *Zeros
047000030220     c                   Leave
047100030220     c                   EndIf
047200030220     c                   Movel     Lnp(xx)       KeyLnp
047300120810
047400030220      *    =============================
047500030220      *    LEGGO LE BOLLE PER LNP E DATA
047600030220      *    =============================
047700000908
047800030220     c     Kblp          Setll     Fnblp70l
047900000908===  C                   DO        *hival
048000000908
048100030220     C     kblp          reade     fnblp70l                               30
048200000908     C   30              leave
048300000908
048400000908      *    Escludo quelle già trasmesse
048500000908
048600000908     C     blpft1        ifne      ' '
048700000908     C                   iter
048800000908     C                   endif
048900000908
049000000919      *    SE ELABORAZIONE PER MITTENTE
049100000919      *    Escludo se il mittente non è quello digitato a video
049200000908      *    NB: - per le bolle franco il cli.mittente è BLPKSC
049300000908      *        - per le bolle assegn il cli.mittente è BLPCCM
049400000908
049500000919+++  C     VIDCLI        ifne      0
049600000908     C                   movel(P)  blpcbo        keykey
049700000908     C     ktab          chain     tabel00f
049800000908---  C                   if        %found
049900000908     C                   movel     tbluni        ds3a
050000120810     C                   movel     §3Atb1        Wfrass            1
050100000908     C     Wfrass        ifne      'A'
050200000908     C     Wfrass        andne     'F'
050300000908     C                   iter
050400000908     C                   endif
050500000908      *              Se Assegnato
050600000908     C     Wfrass        ifeq      'A'
050700000908     C     VIDCLI        ifne      blpccm
050800000908     C                   iter
050900000908     C                   endif
051000000908      *              Se Franco
051100000908     C                   else
051200000908     C     VIDCLI        ifne      blpksc
051300000908     C                   iter
051400000908     C                   endif
051500000908     C                   endif
051600000908---  C                   else
051700000908     C                   iter
051800000919---  C                   endif
051900000919+++  C                   endif
052000000919
052100120810      *    SE ELABORAZIONE PER NRS
052200120810      *    Escludo se NRS non corrisponde al video
052300000908
052400120810+++  C     VIDNRS        ifne      0
052500030220     C     VIDNRS        andne     blpnrs
052600000919     C                   iter
052700000919+++  C                   endif
052800000919
052900000908      *    Escludo se vi sono spunte e non voglio cancellare
053000000908
053100000908     C     VIDSPU        ifeq      '2'
053200000908     C                   setoff                                       33
053300000908     C     kblt          setll     fnblt01l
053400000908     C                   do        *hival
053500000908     C     kblt          reade     fnblt01l                               34
053600000908     C   34              leave
053700070116     C     kbrv          chain     fnbrv07l
053800000908     C                   if        %found
053900000908     C                   seton                                        33
054000000908     C                   leave
054100000908     C                   endif
054200000908     C                   enddo
054300000908     C   33              iter
054400000908     C                   endif
054500000908
054600000908      *    OK CANCELLO
054700000908      *    ===========
054800000908
054900000908     C                   add       1             vidnrb
055000000908     C                   setoff                                       03
055100000908
055200000908      *    Lancio il programma di cancellazione bolla singola
055300000908      *    Se scelto "AGGIORNAMENTO" a video
055400000908
055500000908     C     vidsim        ifeq      'A'
055600000908     C                   CLEAR                   DSLS63
055700000908     C                   MOVE      BLPAAS        I63AAS
055800000908     C                   MOVE      BLPLNP        I63LNP
055900000908     C                   MOVE      BLPNRS        I63NRS
056000000908     C                   MOVE      BLPNSP        I63NSP
056100000908     C                   MOVEL     DSLS63        KPJBU
056200000908     C                   CALL      'FNLS63R'
056300000908     C                   PARM                    KPJBA
056400091130     c* richiamo pgm per cancellare eventuali record su firab
056500091130     c                   clear                   trtc86drds
056600091130     c                   eval      i86mod='DC'
056700091130     c                   z-add     blpaas        i86aasb
056800091130     c                   z-add     blplnp        i86lnpb
056900091130     c                   z-add     blpnrs        i86nrsb
057000091130     c                   z-add     blpnsp        i86nspb
057100091130     c                   call      'TRTC86DRR'
057200091130     c                   parm                    trtc86drds
057300000908     C                   endif
057400000908
057500000908===  C                   ENDDO
057600030220
057700030220     c                   EndDo
057800000908
057900000908     C                   endsr
058000000919
058100000919      *---------------------------------------------------------------*
058200000919      *    Subroutine di cancellazione bolle NON CONFERMATE           *
058300000919      *---------------------------------------------------------------*
058400000919
058500000919     C     DLTVAB        begsr
058600030220
058700030220      *    Per prima cosa richiamo il pgm oer pulizia bolle obsolete
058800030220     c                   Clear                   Fnlse5ds
058900030220     c                   Eval      Ie5Pou = Simfel
059000030220     c                   Call      'FNLSE5R'
059100030220     c                   Parm                    Fnlse5ds
059200000919
059300000920      *    Verifico quale file devo cancellare e per quale chiave:
059400000920      *    *IN46 = *ON .. Posso posizionarmi per codice cliente
059500000920
059600000920     C     VIDCLI        ifne      0
059700000920     C                   seton                                        46
059800000920     C                   else
059900000920     C                   setoff                                       46
060000000920     C                   endif
060100000920
060200000920      *    ===========================
060300000920      *    Mi posiziono per la lettura
060400000920      *    ===========================
060500000920
060600120810     C                   eval      KEYfgs = VIDlnp
060700120810     C   46k02vab02      setll     fivab000
060800120810     C  N46KEYfgs        setll     fivab000
060900000920
061000000920      *    ==============
061100000920      *    LEGGO LE BOLLE
061200000920      *    ==============
061300000920
061400120810===  C                   DO        *hival
061500000920
061600120810     C   46k02vab02      reade     fivab000
061700120810     C  N46KEYfgs        reade     fivab000
061800000920
061900120810     C                   if        %eof(FIVAB02L)
062000000920     C                   leave
062100000920     C                   endif
062200030220
062300120810      *    Controllo il p.o. gestione --> deve essere in L1
062400120810      *   ?(controllo qui superfluo: VABFGS è uguale a VIDLNP?
062500120810      *   ?- che è già stata controllata essere nella £1...)?
062600120810     c*//  VabFgs        Lookup    L1                                     30
062700120810     c*//                If        Not *In30
062800120810     c*//                Iter
062900120810     c*//                EndIf
063000000920
063100000920      *    Controllo le linee partenza e le serie
063200000920      *    (se non sto leggendo per cliente)
063300120810      *   ?(ora il controllo è eseguito SOLO sulla Fil. Gestione?
063400120810      *   ?- che è in chiave di lettura...)?
063500000920     C     *in46         ifeq      *off
063600120810     C*//  VIDLNP        ifne      vablnp
063700120810     C*//                iter
063800120810     C*//                endif
063900000920     C     VIDNRS        ifne      vabnrs
064000000920     C                   iter
064100000920     C                   endif
064200000920     C                   endif
064300000920
064400000920      *    CONTROLLO DETTAGLIO SEGNACOLLI:
064500000920
064600000920     C     kblt27        chain     fnblt27l                           27
064700000920
064800000920      *    Se ho richiesto solo le bolle con segnacolli doppi
064900000920      *    non considero se non ho trovato record di BLT
065000000920
065100000920     C   27VIDDOP        ifeq      '2'
065200000920     C                   iter
065300000920     C                   endif
065400000920
065500000919      *    OK CANCELLO
065600000919      *    ===========
065700000919
065800000919     C                   add       1             vidnrb
065900000919     C                   setoff                                       03
066000000920
066100000920      *    Cancello solo se è stato scelto "AGGIORNAMENTO"
066200000919
066300120810---  C     VIDSIM        IfEq      'A'
066400000920
066500091130      *    Cancellazione bolla normale (FiVAD01L + FiVAT01L + FIVAX01L
066600091130      *    + FIRAB02L e per ultimo FiVAB01L)
066700000920
066800000920...  C     vabnsp        ifgt      0
066900000920
067000030220     C     kvadet        setll     fivad01l
067100000920     C                   do        *hival
067200030220     C     kvadet        reade     fivad01l
067300000920     C                   if        %eof
067400000920     C                   leave
067500000920     C                   endif
067600000920
067700000920      *                  ------------------
067800030220     C                   delete    fivad000
067900000920      *                  ------------------
068000000920
068100000920     C                   enddo
068200000920
068300030220     C     kvadet        setll     fivat01l
068400000920     C                   do        *hival
068500030220     C     kvadet        reade     fivat01l
068600000920     C                   if        %eof
068700000920     C                   leave
068800000920     C                   endif
068900000920
069000000920      *                  ------------------
069100030220     C                   delete    fivat000
069200000920      *                  ------------------
069300000920
069400000920     C                   enddo
069500040604
069600040604     C     kvadet        setll     fivax01l
069700040604     C                   do        *hival
069800040604     C     kvadet        reade     fivax01l
069900040604     C                   if        %eof
070000040604     C                   leave
070100040604     C                   endif
070200040604
070300040604      *                  ------------------
070400040604     C                   delete    fivax000
070500040604      *                  ------------------
070600040604
070700040604     C                   enddo
070800091130      *
070900091130     c     kvadet        setll     firab02l
071000091130     c                   do        *hival
071100091130     c     kvadet        reade     firab02l
071200091130     c                   if        %eof(firab02l)
071300091130     c                   leave
071400091130     c                   endif
071500091130
071600091130      *                  ------------------
071700091130     c                   delete    firab000
071800091130      *                  ------------------
071900091130
072000091130     c                   enddo
072100000920
072200000920...  C                   endif
072300000920
072400000920      *                  ------------------
072500030220     C                   delete    fivab000
072600000920      *                  ------------------
072700000920
072800120810---  C                   EndIf
072900000920
073000120810===  C                   ENDDO
073100000920
073200000919     C                   endsr
