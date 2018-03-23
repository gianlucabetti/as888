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
002400130627     FUBVASDB1D CF   E             WORKSTN
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
018300130627     C                   move      *DATE         viddtsD
018400130627     C                   move      *DATE         viddtsA
018500130627     C                   move      '1'           vidspu
018600000919     C                   move      'N'           vidnco
018700130627     C                   move      '1'           viddop
018800000908     C                   move      'S'           vidsim
018900000919     C                   clear                   vidsc1
019000000919     C                   clear                   vidsc2
019100120810
019200120810     C     VIDlnp        chain     AZORG
019300120810     C                   if        %found(AZORG01L)
019400120810     C                   movel     ORGdes        VIDdea
019500120810     C                   endif
019600000908
019700000908      *    ===============
019800000908      *    EMISSIONE VIDEO
019900000908      *    ===============
020000000908
020100000523     C     EMET0         TAG
020200130627     C                   EXFMT     VDB1D01
020300000908
020400000908      *    F3=Fine
020500000908
020600000908     C   KC              goto      FINE
020700000908
020800000908      *    ===========================
020900000908      *    CONTROLLO PARAMETRI IMMESSI
021000000908      *    ===========================
021100000908
021200000908     C                   setoff                                       0304
021300000919     C                   clear                   vidsc1
021400000919     C                   clear                   vidsc2
021500000908
021600000919      *    1) Ricerca Cliente Mittente
021700000908
021800000908     C     viddes        ifne      *blanks
021900000908     C     vidcli        andeq     0
022000000908
022100000908     C                   movel     rsut          PAXDUT           30
022200000908     C                   z-add     9             PAXSTA
022300120810     C                   z-add     dutKCI        PAXCCC
022400000908     C                   movel(P)  viddes        PAXDMT
022500000908     C                   z-add     1             PAXNUM
022600000908
022700000908     C                   CALL      'XALFA3BR'
022800000908     c                   PARM                    PAXDUT
022900000908     C                   PARM                    CODUT
023000000908     C                   PARM                    PAXDMT
023100000908     C                   PARM                    PAXCCC
023200000908     C                   PARM                    PAXSTA            1 0
023300000908     C                   PARM                    PAXFLR           90
023400000908     C                   PARM                    PAXDIT            3
023500000908     C                   PARM                    PAXNUM            2 0
023600000908     C                   PARM                    PAXKCM           80
023700000908     C                   PARM                    PAXKSM          140
023800000908     C                   PARM                    PAXKDM           60
023900000908
024000000908    2C     PAXSTA        IFGT      -1
024100000908     C                   movel     paxksm        vidcli
024200000908     C                   movel(P)  paxdmt        viddes
024300000908     C                   else
024400000908     C                   clear                   vidcli
024500000908     C                   endif
024600000908     C                   goto      EMET0
024700000908      *                  ---------------
024800000908
024900000908     C                   endif
025000000908
025100120810      *    2) Controllo Fil. Partenza
025200120810
025300120810     C     vidlnp        ifeq      0
025400120810     C                   clear                   viddea
025500130626     C                   write     VDB1D01
025600120810     C                   seton                                        20
025700120810     C                   goto      EMET0
025800120810      *                  ---------------
025900120810     C                   endif
026000120810
026100120810     c                   Clear                   Og143
026200120810     C     vidlnp        chain     azorg01l
026300120810      *
026400120810     C                   IF        %found
026500120810      *
026600120810     C                   movel     orgdes        viddea
026700130626     C                   write     VDB1D01
026800120810     C                   Movel     Orgde3        Og143
026900120810     C     orgfva        ifne      ' '
027000120810     C                   seton                                        22
027100120810     C                   goto      EMET0
027200120810      *                  ---------------
027300120810     C                   endif
027400120810     C     orgfag        ifne      'A'
027500120810     C     orgfag        andne     'F'
027600120810     C                   seton                                        22
027700120810     C                   goto      EMET0
027800120810      *                  ---------------
027900120810     C                   endif
028000120810      *   non può essere un p.o. Poste
028100120810     c                   If        §OgNtw = 'PPT'
028200120810     c                   Eval      *In28 = *On
028300120810     c                   GoTo      Emet0
028400120810      *                  ---------------
028500120810     c                   EndIf
028600120810      *   BLPLNP o VABFGS dev'essere comunque nella £1 dell'utente
028700120810     C*//  vidcon        ifeq      'S'
028800120810     C     vidlnp        lookup    L1                                     27
028900120810     C     *in27         ifeq      *off
029000120810     C                   seton                                        23
029100120810     C                   goto      EMET0
029200120810      *                  ---------------
029300120810     C                   endif
029400120810     C*//                endif
029500120810      *
029600120810     C                   ELSE
029700120810      *
029800120810     C                   eval      viddea = *all'? '
029900130626     C                   write     VDB1D01
030000120810     C                   seton                                        22
030100120810     C                   goto      EMET0
030200120810      *                  ---------------
030300120810      *
030400120810     C                   ENDIF
030500120810
030600120810      *    3) O Cliente O Serie
030700000919
030800000919     C     vidcli        ifeq      0
030900000919     C     vidnrs        andeq     0
031000000919     C                   clear                   viddes
031100000919     C                   seton                                        21
031200000919     C                   goto      EMET0
031300000919      *                  ---------------
031400000919     C                   endif
031500000919
031600000919     C     vidcli        ifne      0
031700120810     C     vidnrs        andne     0
031800000919     C                   clear                   viddes
031900000919     C                   seton                                        21
032000000919     C                   goto      EMET0
032100000919      *                  ---------------
032200000919     C                   endif
032300000919
032400000919      *    4) Controllo Cliente in Anagrafica
032500000919
032600000919     C     vidcli        ifne      0
032700000908     C     kaco          chain     CNACO00F
032800000908     C                   IF        %FOUND
032900000908     C     acoflg        ifeq      '*'
033000000908     C                   clear                   viddes
033100000908     C                   seton                                        01
033200000908     C                   goto      EMET0
033300000908      *                  ---------------
033400000908     C                   else
033500000908     C                   movel     acorag        viddes
033600000908     C                   endif
033700000908     C                   ELSE
033800000908     C                   clear                   viddes
033900000908     C                   seton                                        01
034000000908     C                   goto      EMET0
034100000908      *                  ---------------
034200000908     C                   ENDIF
034300030220      *   non può essere un cliente Poste
034400030220     c                   Clear                   Og143
034500030220     c                   Movel     Vidcli        w0030
034600030220     c     w0030         Chain     Azorg01l
034700030220     c                   If        %Found(Azorg01l)
034800030220     c                   Movel     Orgde3        Og143
034900030220     c                   EndIf
035000030220     c                   If        §OgNtw = 'PPT'
035100030220     c                   Eval      *In26 = *On
035200030220     c                   GoTo      Emet0
035300030220      *                  ---------------
035400030220     c                   EndIf
035500000919     C                   else
035600000919     C                   clear                   viddes
035700000919     C                   clear                   vidcli
035800000919     C                   endif
035900000919
036000120810      *    5) Cancellazione Confermate o NON Confermate
036100000919
036200000919     C     vidcon        ifeq      'S'
036300000919     C     vidnco        andeq     'S'
036400000919     C                   seton                                        41
036500000919     C                   goto      EMET0
036600000919      *                  ---------------
036700000919     C                   endif
036800000919
036900000919     C     vidcon        ifeq      'N'
037000000919     C     vidnco        andeq     'N'
037100000919     C                   seton                                        41
037200000919     C                   goto      EMET0
037300000919      *                  ---------------
037400000919     C                   endif
037500000919
037600130627      *    6) Data DA
037700000908
037800000919     C     vidcon        ifeq      'S'
037900130627     C* se la data è stata azzerata, metto 1/01/2000 per poter leggere le spedizioni dall'inizio
038000130627     C                   IF        VIDDTSD = 0
038100130627     C                   EVAL      VIDDTSD = 1012000
038200130627     C                   ENDIF
038300130627     C                   Z-ADD     viddtsD       G02DAT
038400000523     C                   MOVEL     *BLANK        G02ERR
038500000523     C                   CALL      'XSRDA8'
038600000523     C                   PARM                    WLBDAT
038700000523     C                   MOVEL     G02INV        DATA$$            8 0
038800000908
038900000523     C     G02ERR        IFEQ      '1'
039000130627     C     viddtsD       OREQ      0
039100000908     C                   SETON                                        02
039200000908     C                   goto      EMET0
039300000908      *                  ---------------
039400000523     C                   ENDIF
039500130627     C                   Z-ADD     G02DAT        viddtsD
039600000919     C                   endif
039700130627
039800130627      *    6) Data A
039900130627
040000130627     C     vidcon        ifeq      'S'
040100130627     C* se la data è stata azzerata, metto oggi      per poter leggere le spedizioni dall'inizio
040200130627     C                   IF        VIDDTSA = 0
040300130627     C                   EVAL      VIDDTSA = *date
040400130627     C                   ENDIF
040500130627     C                   Z-ADD     viddtsA       G02DAT
040600130627     C                   MOVEL     *BLANK        G02ERR
040700130627     C                   CALL      'XSRDA8'
040800130627     C                   PARM                    WLBDAT
040900130627     C                   MOVEL     G02INV        DATA££            8 0
041000130627
041100130627     C     G02ERR        IFEQ      '1'
041200130627     C     viddtsA       OREQ      0
041300130627     C                   SETON                                        12
041400130627     C                   goto      EMET0
041500130627      *                  ---------------
041600130627     C                   ENDIF
041700130627     C                   Z-ADD     G02DAT        viddtsA
041800130627     C                   endif
041900000908
042000120810      *    7) Non premuto F6
042100000908
042200000908     C  NKF              goto      EMET0
042300000908      *                  ---------------
042400000908
042500000908      *    ====================================
042600000908      *    CONTROLLI PASSATI: CANCELLO LE BOLLE
042700000908      *    ====================================
042800000919
042900130626     C                   WRITE     VDB1W01
043000000919     C                   seton                                        0304
043100000919     C     vidsim        ifeq      'A'
043200000919     C                   seton                                        10
043300000919     C                   else
043400000919     C                   setoff                                       10
043500000919     C                   endif
043600000919     C                   z-add     0             VIDNRB
043700000908
043800000919     C     vidcon        ifeq      'S'
043900000919     C                   move      '<----'       vidsc1
044000000908     C                   exsr      DLTBLP
044100000919      *                  ================
044200000919     C                   endif
044300000919
044400000919     C     vidnco        ifeq      'S'
044500000919     C                   move      '<----'       vidsc2
044600000919     C                   exsr      DLTVAB
044700000919      *                  ================
044800000919     C                   endif
044900000919
045000000908     C                   goto      EMET0
045100000908
045200000523      *    FINE PROGRAMMA
045300000908
045400000523     C     FINE          TAG
045500000925     c                   clear                   dsls63
045600000925     c                   movel     'C'           i63tla
045700000925     c                   movel     dsls63        kpjbu
045800000925     c                   call      'FNLS63R'
045900000925     c                   parm                    kpjba
046000030220
046100030220      *    Prima di fine richiamo il pgm per pulizia bolle obsolete
046200030220      *    in modalità 'C' se il richiamo precedente era andato a
046300030220      *    buon fine
046400030220     c                   If        Oe5Err <> 'E'
046500030220     c                   Clear                   Fnlse5ds
046600030220     c                   Eval      Ie5Tla = 'C'
046700030220     c                   Call      'FNLSE5R'
046800030220     c                   Parm                    Fnlse5ds
046900030220     c                   EndIf
047000000925     c*
047100000523     C                   SETON                                        LR
047200000908
047300000523      *****************************************************************
047400000908      *                                                               *
047500000908      *    SUBROUTINES                                                *
047600000908      *                                                               *
047700000523      *****************************************************************
047800000908
047900000525      *---------------------------------------------------------------*
048000000919      *    Subroutine di cancellazione bolle CONFERMATE               *
048100000525      *---------------------------------------------------------------*
048200000908
048300000908     C     DLTBLP        begsr
048400030220
048500120810     c                   clear                   LNP
048600120810     c*//                If        VidLnp > 0
048700030220     c                   Eval      Lnp(1) = VidLnp
048800120810     c*//                Else
048900120810     c*//                Movea     Lin           Lnp
049000120810     c*//                EndIf
049100000908     C                   movel     data$$        keyaas
049200000908     C                   move      data$$        keymgs
049300000908
049400030220     c                   Do        30            xx
049500120810
049600030220     c                   If        Lnp(xx) = *Zeros
049700030220     c                   Leave
049800030220     c                   EndIf
049900030220     c                   Movel     Lnp(xx)       KeyLnp
050000120810
050100030220      *    =============================
050200030220      *    LEGGO LE BOLLE PER LNP E DATA
050300030220      *    =============================
050400000908
050500030220     c     Kblp          Setll     Fnblp70l
050600000908===  C                   DO        *hival
050700000908
050800130627     C     keylnp        reade     fnblp70l                               30
050900000908     C   30              leave
051000130627     C                   IF        BLPAAS * 10000 + BLPMGS > Data££
051100130627     C                   leave
051200130627     C                   ENDIF
051300000908
051400000908      *    Escludo quelle già trasmesse
051500000908
051600000908     C     blpft1        ifne      ' '
051700000908     C                   iter
051800000908     C                   endif
051900000908
052000000919      *    SE ELABORAZIONE PER MITTENTE
052100000919      *    Escludo se il mittente non è quello digitato a video
052200000908      *    NB: - per le bolle franco il cli.mittente è BLPKSC
052300000908      *        - per le bolle assegn il cli.mittente è BLPCCM
052400000908
052500000919+++  C     VIDCLI        ifne      0
052600000908     C                   movel(P)  blpcbo        keykey
052700000908     C     ktab          chain     tabel00f
052800000908---  C                   if        %found
052900000908     C                   movel     tbluni        ds3a
053000120810     C                   movel     §3Atb1        Wfrass            1
053100000908     C     Wfrass        ifne      'A'
053200000908     C     Wfrass        andne     'F'
053300000908     C                   iter
053400000908     C                   endif
053500000908      *              Se Assegnato
053600000908     C     Wfrass        ifeq      'A'
053700000908     C     VIDCLI        ifne      blpccm
053800000908     C                   iter
053900000908     C                   endif
054000000908      *              Se Franco
054100000908     C                   else
054200000908     C     VIDCLI        ifne      blpksc
054300000908     C                   iter
054400000908     C                   endif
054500000908     C                   endif
054600000908---  C                   else
054700000908     C                   iter
054800000919---  C                   endif
054900000919+++  C                   endif
055000000919
055100120810      *    SE ELABORAZIONE PER NRS
055200120810      *    Escludo se NRS non corrisponde al video
055300000908
055400120810+++  C     VIDNRS        ifne      0
055500030220     C     VIDNRS        andne     blpnrs
055600000919     C                   iter
055700000919+++  C                   endif
055800000919
055900000908      *    Escludo se vi sono spunte e non voglio cancellare
056000000908
056100000908     C     VIDSPU        ifeq      '2'
056200000908     C                   setoff                                       33
056300000908     C     kblt          setll     fnblt01l
056400000908     C                   do        *hival
056500000908     C     kblt          reade     fnblt01l                               34
056600000908     C   34              leave
056700070116     C     kbrv          chain     fnbrv07l
056800000908     C                   if        %found
056900000908     C                   seton                                        33
057000000908     C                   leave
057100000908     C                   endif
057200000908     C                   enddo
057300000908     C   33              iter
057400000908     C                   endif
057500000908
057600000908      *    OK CANCELLO
057700000908      *    ===========
057800000908
057900000908     C                   add       1             vidnrb
058000000908     C                   setoff                                       03
058100000908
058200000908      *    Lancio il programma di cancellazione bolla singola
058300000908      *    Se scelto "AGGIORNAMENTO" a video
058400000908
058500000908     C     vidsim        ifeq      'A'
058600000908     C                   CLEAR                   DSLS63
058700000908     C                   MOVE      BLPAAS        I63AAS
058800000908     C                   MOVE      BLPLNP        I63LNP
058900000908     C                   MOVE      BLPNRS        I63NRS
059000000908     C                   MOVE      BLPNSP        I63NSP
059100000908     C                   MOVEL     DSLS63        KPJBU
059200000908     C                   CALL      'FNLS63R'
059300000908     C                   PARM                    KPJBA
059400091130     c* richiamo pgm per cancellare eventuali record su firab
059500091130     c                   clear                   trtc86drds
059600091130     c                   eval      i86mod='DC'
059700091130     c                   z-add     blpaas        i86aasb
059800091130     c                   z-add     blplnp        i86lnpb
059900091130     c                   z-add     blpnrs        i86nrsb
060000091130     c                   z-add     blpnsp        i86nspb
060100091130     c                   call      'TRTC86DRR'
060200091130     c                   parm                    trtc86drds
060300000908     C                   endif
060400000908
060500000908===  C                   ENDDO
060600030220
060700030220     c                   EndDo
060800000908
060900000908     C                   endsr
061000000919
061100000919      *---------------------------------------------------------------*
061200000919      *    Subroutine di cancellazione bolle NON CONFERMATE           *
061300000919      *---------------------------------------------------------------*
061400000919
061500000919     C     DLTVAB        begsr
061600030220
061700030220      *    Per prima cosa richiamo il pgm oer pulizia bolle obsolete
061800030220     c                   Clear                   Fnlse5ds
061900030220     c                   Eval      Ie5Pou = Simfel
062000030220     c                   Call      'FNLSE5R'
062100030220     c                   Parm                    Fnlse5ds
062200000919
062300000920      *    Verifico quale file devo cancellare e per quale chiave:
062400000920      *    *IN46 = *ON .. Posso posizionarmi per codice cliente
062500000920
062600000920     C     VIDCLI        ifne      0
062700000920     C                   seton                                        46
062800000920     C                   else
062900000920     C                   setoff                                       46
063000000920     C                   endif
063100000920
063200000920      *    ===========================
063300000920      *    Mi posiziono per la lettura
063400000920      *    ===========================
063500000920
063600120810     C                   eval      KEYfgs = VIDlnp
063700120810     C   46k02vab02      setll     fivab000
063800120810     C  N46KEYfgs        setll     fivab000
063900000920
064000000920      *    ==============
064100000920      *    LEGGO LE BOLLE
064200000920      *    ==============
064300000920
064400120810===  C                   DO        *hival
064500000920
064600120810     C   46k02vab02      reade     fivab000
064700120810     C  N46KEYfgs        reade     fivab000
064800000920
064900120810     C                   if        %eof(FIVAB02L)
065000000920     C                   leave
065100000920     C                   endif
065200030220
065300120810      *    Controllo il p.o. gestione --> deve essere in L1
065400120810      *   ?(controllo qui superfluo: VABFGS è uguale a VIDLNP?
065500120810      *   ?- che è già stata controllata essere nella £1...)?
065600120810     c*//  VabFgs        Lookup    L1                                     30
065700120810     c*//                If        Not *In30
065800120810     c*//                Iter
065900120810     c*//                EndIf
066000000920
066100000920      *    Controllo le linee partenza e le serie
066200000920      *    (se non sto leggendo per cliente)
066300120810      *   ?(ora il controllo è eseguito SOLO sulla Fil. Gestione?
066400120810      *   ?- che è in chiave di lettura...)?
066500000920     C     *in46         ifeq      *off
066600120810     C*//  VIDLNP        ifne      vablnp
066700120810     C*//                iter
066800120810     C*//                endif
066900000920     C     VIDNRS        ifne      vabnrs
067000000920     C                   iter
067100000920     C                   endif
067200000920     C                   endif
067300000920
067400000920      *    CONTROLLO DETTAGLIO SEGNACOLLI:
067500000920
067600000920     C     kblt27        chain     fnblt27l                           27
067700000920
067800000920      *    Se ho richiesto solo le bolle con segnacolli doppi
067900000920      *    non considero se non ho trovato record di BLT
068000000920
068100000920     C   27VIDDOP        ifeq      '2'
068200000920     C                   iter
068300000920     C                   endif
068400000920
068500000919      *    OK CANCELLO
068600000919      *    ===========
068700000919
068800000919     C                   add       1             vidnrb
068900000919     C                   setoff                                       03
069000000920
069100000920      *    Cancello solo se è stato scelto "AGGIORNAMENTO"
069200000919
069300120810---  C     VIDSIM        IfEq      'A'
069400000920
069500091130      *    Cancellazione bolla normale (FiVAD01L + FiVAT01L + FIVAX01L
069600091130      *    + FIRAB02L e per ultimo FiVAB01L)
069700000920
069800000920...  C     vabnsp        ifgt      0
069900000920
070000030220     C     kvadet        setll     fivad01l
070100000920     C                   do        *hival
070200030220     C     kvadet        reade     fivad01l
070300000920     C                   if        %eof
070400000920     C                   leave
070500000920     C                   endif
070600000920
070700000920      *                  ------------------
070800030220     C                   delete    fivad000
070900000920      *                  ------------------
071000000920
071100000920     C                   enddo
071200000920
071300030220     C     kvadet        setll     fivat01l
071400000920     C                   do        *hival
071500030220     C     kvadet        reade     fivat01l
071600000920     C                   if        %eof
071700000920     C                   leave
071800000920     C                   endif
071900000920
072000000920      *                  ------------------
072100030220     C                   delete    fivat000
072200000920      *                  ------------------
072300000920
072400000920     C                   enddo
072500040604
072600040604     C     kvadet        setll     fivax01l
072700040604     C                   do        *hival
072800040604     C     kvadet        reade     fivax01l
072900040604     C                   if        %eof
073000040604     C                   leave
073100040604     C                   endif
073200040604
073300040604      *                  ------------------
073400040604     C                   delete    fivax000
073500040604      *                  ------------------
073600040604
073700040604     C                   enddo
073800091130      *
073900091130     c     kvadet        setll     firab02l
074000091130     c                   do        *hival
074100091130     c     kvadet        reade     firab02l
074200091130     c                   if        %eof(firab02l)
074300091130     c                   leave
074400091130     c                   endif
074500091130
074600091130      *                  ------------------
074700091130     c                   delete    firab000
074800091130      *                  ------------------
074900091130
075000091130     c                   enddo
075100000920
075200000920...  C                   endif
075300000920
075400000920      *                  ------------------
075500030220     C                   delete    fivab000
075600000920      *                  ------------------
075700000920
075800120810---  C                   EndIf
075900000920
076000120810===  C                   ENDDO
076100000920
076200000919     C                   endsr
