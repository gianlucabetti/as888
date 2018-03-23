000100890921     H DECEDIT('0,') DATEDIT(*DMY.)
000200890921     H*----------------------------------------------------*
000300000124     H*   CREA  ARCHIVIO BOLLE FATTURATE  AL CLIENTE       *
000400030516     H*                                                    *
000600030516     H* relese *CURRENT  (TGTRLS)                          *
001200890921     H*----------------------------------------------------*
001300890921     F*
001400120510     FWURTH_758 iF   E             DISK
001401120510     F                                     RENAME(wurth_758:wurth)
001402120510     FTITAS30C  IF   E           K DISK
001500120510     FTITA730C  IF   E           K DISK
001600990817     FTITA430C  IF   E           K DISK
001700050905     FFNORM01L  IF   E           K DISK
001800051219     fAZORG01L  if   e           k disk
001900910830     FTABEL00F  IF   E           K DISK
002200040531     FFIAR531C  IF   E           K DISK
002300120510     Fwfvaf00f  uf A E             DISK
003000030515     D VALNUM          S             10    DIM(1) CTDATA PERRCD(10)
003100050906     D VSV             S              1    DIM(30)                              SIGLA VARIE
003200990910     D VVA             S             11  3 DIM(30)                              IMPOR VARIE
003501120510     D DS4C          E DS
003800050906     D DSQT1         E DS
004000050906     D DS$2          E DS
004100030516     D DTA4A         E DS
004101120510     D DTA44         E DS
004800050905      *
005400010402     D*------------------
005500010402     D* DS di ridefinizione estensione riferimenti bolle (TITA4) x tipo record M (ORM)
005600010402     D*------------------
005700010402     D  DSBL4M       E DS
005800010402     D*------------------
005900030917      * DS FIAR5
006000030917     d dAr5Bnb       e ds
006400051219
006500051219     d Og143         e ds                  inz
006600060127
006800030917      *
006900021212     D Wtara           s                   like(§QTTPC)
007000021212     D Wntara          s                   like(TASPKF)
007100030917
007200030917     d kAr5Trd         s                   Like(Ar5Trd)
007300030515
007400030515     D Rma15           s             15
007500030515     D WRma15          s             15
007600030515     D II              s              2  0
007700030515     D Pos             s              2  0
007800111026     D Var             s              9
008001120510     D kaas            s                   like(tasaas) inz(2012)
008100010402
008200101025     C*------------------------------------------------------------------
008601120510     C                   EXSR      DEFCAM
008602120510      *
008603120510     c                   do        *hival
008604120510     c* leggo file conentente le spedizioni da elaborare
008605120510     c                   read      wurth_758
008606120510     c                   if        %eof(wurth_758)
008607120510     c                   leave
008608120510     c                   endif
008609120510     C                   EXSR      WRTFIL                                        LEGGE/SCRIVE
008610120510     c                   enddo
009300101025     c
009400921023     C                   SETON                                        LR
015400101025     C*----------------------------------------------------*
015500101025     c* operazioni  iniziali
015600101025     C*----------------------------------------------------*
015700910830     C     DEFCAM        BEGSR
015800050905      *
016800000721      *
017900030917      * FIAR
018000030917     c     kFiar5        Klist
018100030917     c                   Kfld                    TasAas
018200030917     c                   Kfld                    TasLnp
018300030917     c                   Kfld                    TasNrs
018400030917     c                   Kfld                    TasNsp
018500030917     c                   Kfld                    kAr5Trd
018600990817     C* TITAS
018700050905     C     KTAS          KLIST
018800120510     C                   KFLD                    kaas
018900120510     C                   KFLD                    lnp
018901120510     C                   KFLD                    nrs
018902120510     C                   KFLD                    nsp
019000990817     C* TITA7
019100050905     C     KTA7          KLIST
019200941122     C                   KFLD                    TASAAS
019300941122     C                   KFLD                    TASLNP
019400941122     C                   KFLD                    TASNRS
019500941122     C                   KFLD                    TASNSP
019600050905     C                   KFLD                    TASTBL
019700990817     C     *LIKE         DEFINE    TA4TRC        KA4TRC
019800990910     C     *LIKE         DEFINE    TASVA1        WVA
019900990910     C     *LIKE         DEFINE    TASSV1        WSV
020000050905     C     KTA4          KLIST
020100990817     C                   KFLD                    TASAAS
020200990817     C                   KFLD                    TASLNP
020300990817     C                   KFLD                    TASNRS
020400990817     C                   KFLD                    TASNSP
020500990817     C                   KFLD                    KA4TRC
020600010402     C*
020700050905     C* Chiave primaria su file ORM (Ordini Ritiro Merci)
020800050905     C     KORM          KLIST
020900050905     C                   KFLD                    §B4POE
021000050905     C                   KFLD                    §B4NSR
021100050905     C                   KFLD                    §B4NOR
021200050905     C                   KFLD                    §B4NRV
021300990817     C*
021400990817     C* LEGGE IL RECORD SIGLE VARIE
021500990817     C     *LIKE         DEFINE    TBLKUT        KTBKUT
021600990817     C     *LIKE         DEFINE    TBLCOD        KTBCOD
021700990817     C     *LIKE         DEFINE    TBLKEY        KTBKEY
022400990817     C     KEYTAB        KLIST
022500990817     C                   KFLD                    KTBKUT
022600990817     C                   KFLD                    KTBCOD
022700990817     C                   KFLD                    KTBKEY
022800021212      *
022900021212      ** Aggancio tabella $2 - Varie forzate a programma
023000990817     C                   Z-ADD     1             KTBKUT
023100990817     C                   MOVEL     '$2'          KTBCOD
023200990817     C                   MOVEL     1             KTBKEY
023300990817     C     KEYTAB        CHAIN     TABEL00F                           99
023400990817IF  1C     *IN99         IFEQ      *OFF
023500990817     C     TBLFLG        ANDEQ     *BLANKS
023600990817     C                   MOVEL     TBLUNI        DS$2
023700990817E   1C                   ENDIF
023800021212      *
023900050905      ** Aggancio tabella QT - campi standard tassazione
024000021212     C                   MOVEL     'QT'          KTBCOD
024100021212     C                   MOVEL     1             KTBKEY
024200021212     C     KEYTAB        CHAIN     TABEL00F                           99
024300021212IF  1C     *IN99         IFEQ      *OFF
024400021212     C     TBLFLG        ANDEQ     *BLANKS
024500021212     C                   MOVEL     TBLUNI        DSQT1
024600021212E   1C                   ENDIF
025200990817     C*
025300910830     C                   ENDSR
025400050906     C*--------------------------------------------------------
025500050905     C* WRTFIL    LEGGE TITAS00F/TITAS10F E SCRIVE TIVAF00T   *
025600000124     C*--------------------------------------------------------
025700921023     C     WRTFIL        BEGSR
025800120510     C     KTAS          chain     TITAS30C
025801120510     c                   if        %found(titas30c) and tasnft>0
025802120510     c* tabella 4c
025803120510     C                   Z-ADD     1             KTBKUT
025804120510     C                   MOVEL     '4C'          KTBCOD
025805120510     C                   MOVEL(p)  tasksc        KTBKEY
025806120510     C                   clear                   ds4c
025807120510     c     keytab        chain     tabel00f
025808120510     c                   if        %found(tabel00f)
025809120510     c                   eval      ds4c=tbluni
025810120510     c                   endif
025811120510
027900101025     c                   exsr      SCriviDati
031800940910     C                   END
032100000124     C                   ENDSR
032200101025     C*-----------------------------------------------------------*
032300101025     C* Scrivo dati nel file TIVAF
032400101025     C*-----------------------------------------------------------*
032500101025     c     Scrividati    BEGSR
032610120510
032700101025     C* Inizializzo il buffer d output
032800120510     C                   clear                   wfvaf000
032900101025     C*
033000101025     C                   MOVEL     TASTBL        VAFTBL
033100101025     C                   Z-ADD     TASLNP        VAFLNP
033200101025     C                   Z-ADD     TASMGS        VAFMGS
033300101025     C                   Z-ADD     TASAAS        VAFAAS
033400101025     C                   Z-ADD     TASNRS        VAFNRS
033500101025     C                   Z-ADD     TASNSP        VAFNSP
033600101025     C                   Z-ADD     TASKSC        VAFKSC
033700101025     C                   MOVEL     TASFIN        VAFFIN
033800101025     C                   Z-ADD     TASLNA        VAFLNA
033900101025      * se particolità varia = 'O' recupero da FIAR5 il numero colli originali
034000101025      *
034100101025     c                   If        %Subst(TasGva:1:1) = 'O'
034200101025
034300101025     c                   eval      kAr5Trd  = 'BNB'
034400101025     c                   clear                   dAr5Bnb
034500101025     c     kFiar5        Chain     Fiar531c
034600101025     c                   If        %Found(Fiar531c)
034700101025     c                   Movel     Ar5Uni        dAr5Bnb
034800101025     c                   EndIf
034900101025      * Colli originali
035000101025     c                   Eval      VafNcl = §Ar5bcor
035100101025
035200101025     c                   else
035300101025      * Colli TITAS
035400101025     C                   Z-ADD     TASNCL        VAFNCL
035500101025     c                   endif
035600101025
035700101025     C                   Z-ADD     TASVLF        VAFVLF
035800101025     C                   Z-ADD     TASPOR        VAFPOR
035900101025      * PESO
036000101025     C                   Z-ADD     TASPKF        VAFPKB
036100101025     C                   IF        TASFPF = 'V'
036200101025     C     §QTTPC        MULT      TASNCP        WTARA
036300101025     C     TASPKC        SUB(h)    WTARA         WNTARA
036400101025    *C                   Z-ADD     WNTARA        VAFPKC
036500101025     C                   ELSE
036600101025    *C                   Z-ADD     *zeros        VAFPKC
036700101025     C                   ENDIF
036800101025     C* AZZERO CAMPI NON SEMPRE PRESENTI
036900101025     C                   MOVE      *ZERO         WSV
037000101025     C                   MOVE      *ZERO         WVA
037100101025     C                   MOVEA     *BLANKS       VSV
037200101025     C                   MOVEA     *ZERO         VVA
037300101025     C                   CLEAR                   A
037400101025     C                   CLEAR                   VAFSV1
037500101025     C                   CLEAR                   VAFVA1
037600101025     C                   CLEAR                   VAFSV2
037700101025     C                   CLEAR                   VAFVA2
037800101025     C                   CLEAR                   VAFSV3
037900101025     C                   CLEAR                   VAFVA3
038000101025     C                   CLEAR                   VAFSV4
038100101025     C                   CLEAR                   VAFVA4
038200101025     C                   CLEAR                   VAFSV5
038300101025     C                   CLEAR                   VAFVA5
038400101025     C                   CLEAR                   VAFSV6
038500101025     C                   CLEAR                   VAFVA6
038600101025     C                   CLEAR                   VAFSV7
038700101025     C                   CLEAR                   VAFVA7
038800101025     C                   CLEAR                   VAFSV8
038900101025     C                   CLEAR                   VAFVA8
039000101025     C                   CLEAR                   VAFVAX
039100101025     C*
039200101025     C                   MOVEL     TASSV1        WSV
039300101025     C                   Z-ADD     TASVA1        WVA
039400101025     C                   EXSR      CONTR
039500101025     C                   MOVEL     TASSV2        WSV
039600101025     C                   Z-ADD     TASVA2        WVA
039700101025     C                   EXSR      CONTR
039800101025     C                   MOVEL     TASSV3        WSV
039900101025     C                   Z-ADD     TASVA3        WVA
040000101025     C                   EXSR      CONTR
040100101025     C*
040200101025     C                   Z-ADD     TASDFT        VAFDFT
040300101025     C                   Z-ADD     TASNFT        VAFNFT
040400101025      *
040500101025     c                   if        tascbo <> 'FO'
040600101025     C                   Z-ADD     TASRMN        VAFRMN
040700101025     c                   endif
040800101025      *
040900101025     C                   MOVEL     TASFTC        VAFFTC
041000101025     C                   MOVEL     TASTSP        VAFTSP
041100101025     C                   MOVEL     TASCTS        VAFCTS
041200101025     C                   MOVEL     TASFAP        VAFFAP
041300101025     C                   Z-ADD     TASCTR        VAFCTR
041400101025     C                   Z-ADD     TASQFT        VAFQFT
041500101025     C                   MOVEL     TASTC2        VAFTC2
041600101025     C                   Z-ADD     TASIMV        VAFIMV
041700101025     C                   Z-ADD     TASFIV        VAFFIV
041800101025     C                   MOVEL     TASDIV        VAFDIV
041900101025     C                   Z-ADD     TASDRT        VAFDRT
042000101025     C                   MOVEL     TASNAS        VAFNAS
042100101025     C                   MOVEL     TASCAD        VAFCAD
042200101025     C                   MOVEL     TASLOD        VAFLOD
042300101025     C                   MOVEL     TASPRD        VAFPRD
042400101025     C                   MOVEL     TASNZD        VAFNZD
042500101025     C                   MOVEL     TASVAS        VAFVAS
042600101025     C                   Z-ADD     TASIAS        VAFIAS
042700101025    *C                   MOVEL     *blanks       VAFFPC
042800101025    *C                   MOVEL     *blanks       VAFFVC
042900101025    *C                   Z-ADD     *zeros        VAFVLC
043000101025     C     TASSV3        IFNE      *BLANK
043100101025     C                   EXSR      SRTA7
043200101025     C                   END
043300101025     C                   MOVEL     *BLANKS       VAFRMA
043400101025     C                   MOVEL     'A'           KA4TRC
043500101025     C     KTA4          CHAIN     TITA430C                           59
043600101025     C* se codice bolla diverso da 'FO'(richiesta bolla firmata)valorizzo RMA con RMA
043700101025     c                   if        tascbo <> 'FO' and *in59 = *OFF
043800101025     C                   MOVEL     TA4NOT        VAFRMA
043900101025     C                   ENDIF
044000101025     C* se codice bolla uguale a 'FO'(richiesta bolla firmata)valorizzo RMA con RMN e viceversa
044100101025     c                   if        tascbo = 'FO' and *in59 = *OFF
044200101025     C                   MOVEL     TA4NOT        DTA4A
044300101025     c                   if        §ta4arma <> *blanks
044400101025     C                   MOVEL     §TA4ARMA      Rma15
044500101025      * verifico se campo numerico escludendo i blanks
044600101025     c                   eval      II = 1
044700101025     c                   dow       II <= %len(%trim(Rma15))
044800101025      *
044900101025     c                   If        %subst(%trim(Rma15):II:1) <> *blank
045000101025     c                   eval      WRma15 = (%trim(Wrma15)
045100101025     c                                      + %subst(%trim(Rma15):II:1))
045200101025     c                   endif
045300101025      *
045400101025     c                   eval      II = II + 1
045500101025      *
045600101025     c                   enddo
045700101025     c                   clear                   rma15
045800101025     c                   evalr     rma15 = %trim(Wrma15)
045900101025      *
046000101025     c                   eval      II = 1
046100101025     c                   setoff                                       29
046200101025     c                   dow       II <= %len(%trim(Rma15))
046300101025      *
046400101025     c                   If        %subst(%trim(Rma15):II:1) <> *blank
046500101025     c                   eval      pos = %scan(%subst(%trim(Rma15):II:1):
046600101025     c                             VALNUM(1))
046700101025     c                   if        pos = 0
046800101025     c                   seton                                        29
046900101025     c                   leave
047000101025     c                   endif
047100101025     c                   endif
047200101025      *
047300101025     c                   eval      II = II + 1
047400101025      *
047500101025     c                   enddo
047600101025     c                   if        *in29 = *OFF
047700101025     c                   eval      VAFRMA= %editc(tasrmn:'Z')
047800101025     C                   MOVEL     rma15         VAFRMN
047900101025     c                   else
048000101025     C                   EVAL      VAFRMA = §ta4arma
048100101025     C                   EVAL      VAFRMN = tasrmn
048200101025     c                   endif
048300101025      *
048400101025     C                   endif
048500101025     C                   endif
048600101025     C*
048700101025     C* SCRIVO LE VARIE
048800101025     C                   Z-ADD     1             A
048900101025     C     VSV(A)        DOWNE     ' '
049000101025     C                   SELECT
049100101025     C     VAFSV1        WHENEQ    ' '
049200101025     C                   MOVEL     VSV(A)        VAFSV1
049300101025     C                   Z-ADD     VVA(A)        VAFVA1
049400101025     C*
049500101025     C     VAFSV2        WHENEQ    ' '
049600101025     C                   MOVEL     VSV(A)        VAFSV2
049700101025     C                   Z-ADD     VVA(A)        VAFVA2
049800101025     C*
049900101025     C     VAFSV3        WHENEQ    ' '
050000101025     C                   MOVEL     VSV(A)        VAFSV3
050100101025     C                   Z-ADD     VVA(A)        VAFVA3
050200101025     C*
050300101025     C     VAFSV4        WHENEQ    ' '
050400101025     C                   MOVEL     VSV(A)        VAFSV4
050500101025     C                   Z-ADD     VVA(A)        VAFVA4
050600101025     C*
050700101025     C     VAFSV5        WHENEQ    ' '
050800101025     C                   MOVEL     VSV(A)        VAFSV5
050900101025     C                   Z-ADD     VVA(A)        VAFVA5
051000101025     C*
051100101025     C     VAFSV6        WHENEQ    ' '
051200101025     C                   MOVEL     VSV(A)        VAFSV6
051300101025     C                   Z-ADD     VVA(A)        VAFVA6
051400101025     C*
051500101025     C     VAFSV7        WHENEQ    ' '
051600101025     C                   MOVEL     VSV(A)        VAFSV7
051700101025     C                   Z-ADD     VVA(A)        VAFVA7
051800101025     C*
051900101025     C     VAFSV8        WHENEQ    ' '
052000101025     C                   MOVEL     VSV(A)        VAFSV8
052100101025     C                   Z-ADD     VVA(A)        VAFVA8
052200101025     C                   OTHER
052300101025     C                   ADD       VVA(A)        VAFVAX
052400101025     C                   ENDSL
052500101025     C*
052600101025     C                   ADD       1             A
052700101025     C                   ENDDO
052800101025      * Imposto campo VAFRMX in base alla tabella 4C o se cliente DPD
052900101025     c                   Clear                   VafRmx
053000101025     C                   MOVEL     VAFKSC        WLB3              3 0
053100101025     c     WLB3          chain     AZORG
053200101025     c                   if            %found(AZORG01L)
053300101025     c                             and ORGfva = *blanks
053400101025     c                   movel     ORGde3        Og143
053500101025     c                   else
053600101025     c                   clear                   Og143
053700101025     c                   endif
053800101025
053900101025     c                   Select
054000101025
054100101025     C* Recupera Riferimento Alfanumerico da ORM
054200101025     C                   When      §4cRfo = 'S'
054300101025     C                   MOVEL     'M'           KA4TRC
054400101025     C     KTA4          CHAIN     TITA430C                           59
054500101025     C     *IN59         IFEQ      *OFF
054600101025     C                   MOVEL(P)  TA4NOT        DSBL4M
054700101025     C     KORM          CHAIN     FNORM01L                           61
054800101025     C     *IN61         IFEQ      *OFF
054900101025     C                   MOVEL     ORMRFA        VAFRMX
055000101025     C                   ENDIF
055100101025     C                   ENDIF
056800101025
056900101025     C* PEZZA PER METTERE DATA CONSEGNA PER CONTROLLO DPD
057100101025     c                   When      §OGntw = 'DPD'
057200101025     C                   movel     TASDCM        VAFRMX
057300101025
057400101025     c                   EndSl
057401120510     c* prelevo cup / cig
057402120510     C                   MOVEL     '4'           KA4TRC
057403120510     C     KTA4          CHAIN     TITA430C                           59
057404120510     c                   if        %found(tita430c)
057405120510     c                   movel     ta4not        dta44
057406120510     c                   eval      vafcup=§TA4CUP
057407120510     c                   eval      vafcig=§TA4Cig
057408120510     c                   endif
057409120510
057410120510     c                   write     wfvaf000
057500101025     c
057600101025     c                   ENDSR
057700000124     C*-----------------------------------------------------------*
057800000124     C* SOMMA IL VALORE DELLE VARIE DI TAS7 OLTRE LA 8° IN VAFVAX *
057900000124     C*-----------------------------------------------------------*
058000941122     C     SRTA7         BEGSR
058100990817     C*
058200990823     C     KTA7          SETLL     TITA730C
058300990823     C     KTA7          READE     TITA730C                               59
058400990817DO  1C     *IN59         DOWEQ     *OFF
058500990910     C                   MOVEL     TA7SVN        WSV
058600990910     C                   Z-ADD     TA7VAN        WVA
058700990910     C                   EXSR      CONTR
058800990910     C*
058900990823     C     KTA7          READE     TITA730C                               59
059000990817E   1C                   ENDDO
059100990817     C*
059200941122     C                   ENDSR
059300990910     C*--------------------------------------------------------
059400000124     C* CONTROLLO SE VARIE GIA' PRESENTI
059500000124     C*--------------------------------------------------------
059600990910     C     CONTR         BEGSR
059700000124IF  1C     WSV           IFNE      ' '
059800990910     C*
059900990910IF  4C     WSV           IFNE      §$2BOL                                       *NO BOLLA e NO IVA
060000990910     C     WSV           ANDNE     §$2IVA
060100990910     C                   ADD       1             A                 3 0
060200990910     C                   MOVEL     WSV           VSV(A)
060300990910     C                   Z-ADD     WVA           VVA(A)
060400990910E   4C                   ENDIF
060500000124E   1C                   ENDIF
060600000124     C                   ENDSR
060700050905
072000990817     C*--------------------------------------------------------
073100030515**         VALNUM
0732000305150123456789
