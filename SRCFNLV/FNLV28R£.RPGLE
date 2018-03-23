000100000725     H DECEDIT('0,') DATEDIT(*DMY.)
000200070115     h dftactgrp(*no) actgrp(*caller)
000300000809      * FNLV24R *----------------------------------------------------*
000400021129      *      GESTIONE ANAGRAFICA AUTOTRASP/COOPERATIVE
000500911204      *--------------------------------------------------------------*
000600090408     FFNLV28D   CF   E             WORKSTN
000700940809     F                                     SFILE(LV24S02:NRR2)
001000080107     Fazorg01L  IF   E           K DISK
001100090330     Fanrco98j  IF   E           K DISK
001800090408     Ffiapd01L  iF   E           K DISK
002000940809     D*
002500090408     D MSG             S             78    DIM(2) CTDATA PERRCD(1)
003300060203      *
003400060203     D KPJBA         E DS
003500060203      * - Parametri x Controllo profilo utenti
003600060203     d TIBS34ds      e ds
003700060203      * - Ds di riferimento al file esterno AZUTE00F
003800060203     d AZUTEds       e ds                  extname(AZUTE00F)
003900060203     d dUTE01        e ds
004000060203      * - Ds per dati organigramma
004100060203     d DDatiUte      e ds
004200060203      *
004300031121      * Passaggio Parametri al pgm FNLV55R
004400031121     D FNLV55DS      e ds                  inz
007600031121      *
007700031121      * DS per inversione data
007800031121     D WLBDAT          DS                  INZ
007900031121     D  G02DAT                 1      8  0
008000031121     D  G02INV                 9     16  0
008100031121     D  G02ERR                17     17
008200031121     D  G02TGI                18     22  0
008300031121      *
008400040421     D mess_err        S              1    INZ(*ON)
008500060203     D wDATE           S              8  0 inz
008700060203     d dataiso         s               d   datfmt(*iso)
008800071205     d NotUsed         S             16A
008900071205     d ReturnSize      S              9B 0
009000071205     d SizeList        S              9B 0
009100071205     d WrkSort         S              1
009200071205     d*---
009300080109     d nrr2            S              3  0
009400080109     d stmrec1         S              3  0
009500071205     d*---
009600071205     d MaxKey          C                   2
009700071205     d Ascendente      C                   1
009800071205     d Discendente     C                   2
009900071205     d Carattere       C                   6
010000071205     d Put             C                   1
010100071205     d EndPut          C                   2
010200071205     d Get             C                   3
010300071205     d Numerico        C                   8
010400071205     d*---
010500071205     d SflRcd          DS
010600071205     d  vidsel
010700071205     d  vscpdr
010800090408     d  vscrsf
010900090408     d  vsccsf
011000090408     d  vscksc
011100090408     d  vscrss
011400071205     d  Selected                      1A
011500071205
011600071205      /COPY QSYSINC/QRPGLESRC,QLGSORT
011700071205     d QLGKL                         16    DIM(MaxKey)
011800071205     d  QLGSP00                       9B 0 OVERLAY(QLGKL:00001)
011900071205     d  QLGSS00                       9B 0 OVERLAY(QLGKL:00005)
012000071205     d  QLGDT00                       9B 0 OVERLAY(QLGKL:00009)
012100071205     d  QLGSO00                       9B 0 OVERLAY(QLGKL:00013)
012200071205
012300071205      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
012400071205      /COPY QSYSINC/QRPGLESRC,QUSEC
014400920921     C*****************************************************************
014500000000     C     *ENTRY        PLIST
014600000000     C                   PARM                    KPJBA
014800060203      * Reperisco dati job
014900060203     c                   exsr      DatiJob
015100060203     C                   MOVEL     RSUT          V1CDSA
015101090408     c                   eval      v1ctip = 'A'
024401090408     c*
024501090408     c                   do        *hival
025500920918     C*
025900940809     C                   WRITE     LV24T01
026200090408     C                   EXFMT     LV24D01
026300940809     C*
026400090408     C                   SETOFF                                       285758
026500940809     C*
026600071205     C* CMD3 - FINE LAVORO
026700090408     C   KC              leave
028400031121     C*  SOLO PER AFFLUENZE/DEFLUENZE
028500031121     C*  il codice fornitore deve appartenere ad un terminal
028600031121     C                   IF        v1ctip='D'
028700031121     c                   movel     v1fPad        cterm             3 0
028800031121     c                   movel     cterm         svterm            3 0
028900031121     C                   reset                   FNLV55DS
029000031121     C                   eval      D55lin  = cterm
029100031121     C                   eval      D55drf  = wDATE
029200031121     C                   call      'FNLV55R'
029300031121     C                   parm                    FNLV55DS
029400031121     C                   IF        d55tfp <> cterm  and
029500031121     C                             d55tfa <> cterm
029600090408     C                   MOVEL     MSG(2)        V1CMSG
029700031121     C                   SETON                                        2857
029800090408     C                   iter
029900031121     c                   end
030000031121     c                   end
031200030129RM*  C* deve essere in azorg01l
031300030129RM*  C     V1FPAD        chain     azorg01l
031400030129     c                   if        %found(azorg01l)
031500030203     c                   if        orgfag <> 'A' and orgfag <> 'F'
031600030203     c                   setoff                                       45
031700030203     c                   else
031800030129     c                   seton                                        45
031900030203     c                   end
032000030129     c                   else
032100030129     c                   setoff                                       45
032200030129     c                   end
032500950314RM*  C* Filiale non presente in tabella
032600950314RM* 3C     *IN45         IFEQ      *OFF
032700090408RM*  C                   MOVEL     MSG(2)        V1CMSG
032800950314RM*  C                   SETON                                        2857
032900090408RM*  C                   iter
033000950314RM* 3C                   END
034800911204     C                   EXSR      CARICA
034900920918     C* SE NON CARICA NIENTE ESCO
035000940809     C     NRR2          IFEQ      0
035100090408     C                   iter
035200920918     C                   END
035300920918     C*
035400940809     C                   WRITE     LV24T01
035500940809     C                   WRITE     LV24Z02
035600071205     C                   Z-ADD     nrr2          STMREC1
035700090408     C                   do        *hival
035800940809     C                   EXFMT     LV24C02
035900920918     C* CMD3 - FINE LAVORO
035901090408     c   KL
036000090408     Cor KC              leave
037100071205     C* F11 = Cambia ordinamento
037200071205if  1C                   IF        *INKK
037300071205     C                   EXSR      F11GES
037400090408     C                   iter
037500071205e   1C                   ENDIF
037700920918     C* CONTROLLO DATI SUBFILE
037800920923     C                   SETOFF                                       3017
038000090408     C                   DO        *hival
038001090408     C                   READC     LV24S02                                30
038002090408     c                   if        *in30
038003090408     c                   leave
038004090408     c                   end
038200920918     C     VIDSEL        IFEQ      '1'
038201090408     c                   movel     vscpdr        compdr            7
038700090408     C                   eval      %subst(kpjbu: 1: 8) = compdr + v1ctip
039001090408     c                   return
039002090408     c                   end
040900090408     C                   ENDdo
040902090408     C                   ENDdo
040903090408     C   KC              leave
040904090408     C                   ENDdo
045300000726      *
045400901108     C                   SETON                                        LR
045500060203      *
045507090408     C**************************************************************************
045508090408     C*    CONTROLLo fornitore in proj
045509090408     C**************************************************************************
045510090408     C     srrco         BEGSR
045511090408     c                   move      *all'0'       rcoksc
045512090408     c                   move      vscksc        rcoksc
045514090408     C     Krco          CHAIN     anrco98j                           90
045515090408     c                   if        not *in90
045516090408     C                   MOVEL(p)  sogdes        vscrss
045520090408     c                   else
045521090408     C                   MOVEL     MSG(1)        V1CMSG
045522090408     C                   SETON                                        289056
045523090408    2C                   END
045524090408    2C                   ENDsr
045600071205      *-------------------------------------------------------------------------
045700071205      * F11GES - TASTO FUNZIONE F11 --> Ragione sociale/cod. aut.
045800071205      *-------------------------------------------------------------------------
045900071205     C     F11GES        BEGSR
046000071205      * IMPOSTO ENTRATA AD 'A'
046100071205     C                   EVAL      rec2  = 1
046200071205      *
046300071205     C                   CLEAR                   QLGSCB
046400071205     C                   CLEAR                   QLGSCB00
046500071205
046600071205     ** 1 campo chiave.
046700071205     C                   EVAL      QLGNBRK    = 1
046800071205
046900071205     C                   IF        *in51=*on
047000071205     ** IL P.O è a posizione 3, numerico, 3 byte, ascending.
047100071205     C                   EVAL      QLGSP      = 2
047200071205     C                   EVAL      QLGSS      = %SIZE(vscpdr)
047300071205     C                   EVAL      QLGDT      = Numerico
047400071205     C                   EVAL      QLGSO      = Ascendente
047500071205     C                   EVAL      QLGKL(1)   = QLGSKL
047600071205     C                   setoff                                       51
047700071205     C                   ELSE
047800071205      *
047900071205     ** La ragione sociale è a posizione 6, 20 byte, char, ascending.
048000071205     C                   EVAL      QLGSP      = 9
048100090408     C                   EVAL      QLGSS      = %SIZE(vscrsf)
048200071205     C                   EVAL      QLGDT      = Carattere
048300071205     C                   EVAL      QLGSO      = Ascendente
048400071205     C                   EVAL      QLGKL(1)   = QLGSKL
048500071205     C                   seton                                        51
048600071205     C                   ENDIF
048700071205
048800071205      * Load other sort parameters.
048900071205     C                   EVAL      QLGLB     = 80 + 16 * MaxKey
049000071205     C                   EVAL      QLGRL     = %SIZE(SflRcd) - 1
049100071205     C                   EVAL      QLGRT     = 8
049200071205     C                   EVAL      QLGOKL    = 80
049300071205     C                   EVAL      QLGLKE    = 16
049400071205     C                   EVAL      QLGLSS    = 290
049500071205
049600071205      * Initialize Sort I/O API fields.
049700071205     C                   EVAL      QLGRL00  = QLGRL
049800071205     C                   EVAL      QLGRC00  = 1
049900071205     C                   CLEAR                   QUSEI
050000071205     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
050100071205
050200071205      * First step - Initialize the sort routine.
050300071205     C                   CALL      'QLGSORT'
050400071205     C                   PARM                    QLGSCB
050500071205     C                   PARM                    NotUsed
050600071205     C                   PARM                    NotUsed
050700071205     C                   PARM                    SizeList
050800071205     C                   PARM                    ReturnSize
050900071205     C                   PARM                    QUSEC
051000071205
051100071205      * Next step - Write records to I/O routine.
051200071205     C                   EVAL      QLGRT00 = Put
051300071205
051400080109     C                   DO        STMREC1       x                 3 0
051500071205
051600071205     C     x             CHAIN     LV24S02
051700071205
051800071205     ** Solo le righe con Selected = 'Y' sono riordinate,
051900071205     ** quindi per fare un ordinamento di tutte le righe
052000071205     ** metto 'Y' sempre.
052100071205     C                   EVAL      Selected = 'Y'
052200071205
052300071205     C                   CLEAR                   QUSEI
052400071205     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
052500071205     C                   CALL      'QLGSRTIO'
052600071205     C                   PARM                    QLGSCB00
052700071205     C                   PARM                    SflRcd
052800071205     C                   PARM                    NotUsed
052900071205     C                   PARM                    SizeList
053000071205     C                   PARM                    NotUsed
053100071205     C                   PARM                    QUSEC
053200071205
053300071205     C                   ENDDO
053400071205
053500071205      * Next step - Signal end of input, clear subfile for reload.
053600071205     C                   EVAL      QLGRT00 = EndPut
053700071205     C                   CLEAR                   QUSEI
053800071205     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
053900071205
054000071205     C                   CALL      'QLGSRTIO'
054100071205     C                   PARM                    QLGSCB00
054200071205     C                   PARM                    SflRcd
054300071205     C                   PARM                    NotUsed
054400071205     C                   PARM                    SizeList
054500071205     C                   PARM                    NotUsed
054600071205     C                   PARM                    QUSEC
054700071205
054800071205     C                   SETOFF                                       21
054900071205     C                   WRITE     LV24C02
055000071205     C                   SETON                                        21
055100071205     C                   WRITE     LV24T01
055200071205     C                   WRITE     LV24Z02
055300071205
055400071205      * Final step - Write the records back to the subfile.
055500071205     C                   EVAL      QLGRT00 = Get
055600071205
055700071205     C                   DO        STMREC1       x
055800071205     C                   CLEAR                   QUSEI
055900071205     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
056000071205     C                   CALL      'QLGSRTIO'
056100071205     C                   PARM                    QLGSCB00
056200071205     C                   PARM                    NotUsed
056300071205     C                   PARM                    SflRcd
056400071205     C                   PARM                    QLGRL00
056500071205     C                   PARM                    NotUsed
056600071205     C                   PARM                    QUSEC
056700071205      *
056800071205     c                   z-add     x             nrr2
056900071205     C                   WRITE     LV24S02
057000071205     C                   ENDDO
057100071205
057200071205      *
057300071205     C                   ENDSR
057400060203      *---------------------------------------------------------------*
057500060203      * Reperimento Dati del job (Utente/Operativi)                   *
057600060203      *---------------------------------------------------------------*
057700060203     c     DatiJob       BEGSR
057800060203      *
057900060203     c     *dtaara       define    §azute        azuteds
058000060203     c     *dtaara       define    §datiute      ddatiute
058100060203      *
058200060203     c                   in(E)     *dtaara
058300060203     c                   IF        %ERROR or RSUT = *blanks
058400060203     c                   clear                   Tibs34Ds
058500060203     c                   call      'TIBS34R'
058600060203     c                   parm                    Tibs34Ds
058700060203     c                   in        *dtaara
058800060203     c                   ENDIF
058900060203      *
059000060203     c                   ENDSR
067900940809     C**************************************************************************
068000940809     C*    CARICO DATI NEL SUBFILE
068100940809     C**************************************************************************
068200910116     C     CARICA        BEGSR
068300940809     C*
068400920918     C* PULIZIA SUBFILE
068500940809     C                   SETOFF                                       2021
068600940809     C                   WRITE     LV24C02
068700940809     C                   SETON                                        21
068800920918     C*
068900071205     C                   Z-ADD     0             NRR2
069000920918     C*
069100021129     C     v1ctip        SETLL     fiapd01L
069101090408     c                   do        *hival
069200090408     C     v1ctip        READE     fiapd01L
069300021016     C*
069301090408     c                   if        %eof(fiapd01l)
069302090408     c                   leave
069303090408     c                   end
069500090408     C* ESCLUDO ANNULATI E QUELLI SENZA CODICE FORNITORE
070000000726     C     APDATB        IFNE      ' '
070100090408     C                   iter
070200000726     C                   ENDIF
070201090408     C     APDpdd        IFeq      'S'
070204090408     C                   iter
070205090408     C                   ENDIF
070400021016      *
070401090408RM*  C                   MOVEL     APDPDR        WFIL              3 0
071000090408     C     WFIL          IFNE      v1fpad
071100940809     C     WFIL          ANDNE     *ZERO
071200090408     C                   iter
071400920922     C                   END
071500021016      *
075300940809     C                   MOVEL     *BLANKS       VIDSEL
075400940809     C                   MOVEL     APDPDR        VSCPDR
075500090408     C                   MOVEL     APDRSF        VSCRSF
075600000427      *
078300090408     C                   movel     APDcsf        VSCCSF
078301090408     C                   Z-ADD     APDKSC        VSCKSC
078302090408     c*
078304090408     c                   exsr      srrco
078600920917     C*
078700940809     C                   ADD       1             NRR2
078800940809     C  N20              SETON                                        20
078900940809     C                   WRITE     LV24S02
079000920917     C*
079300910116     C                   END
079400940809     C                   Z-ADD     1             REC2
079500940809     C*
079600910116     C                   ENDSR
102805090331     c*
189200940809     C**************************************************************************
189300940809     C*     R O U T I N E     I N I Z I A L E
189400940809     C**************************************************************************
189500940809     C     *INZSR        BEGSR
189600940809     C*
189700940809     C     *LIKE         DEFINE    APDPDR        KPDR
189701090408     C     Krco          KLIST
189702090408     C                   KFLD                    vsccsf
189703090408     C                   KFLD                    rcosnatura
189704090408     C                   KFLD                    rcoksc
189705090408     c                   eval      rcosnatura = 'F'
193400031121      * Data odierna in formato AAAAMMGG
193500031121     C                   clear                   WLBDAT
193600031121     C                   move      *date         G02dat
193700031121     C                   call      'XSRDA8'
193800031121     C                   parm                    WLBDAT
193900031121     C                   move      G02inv        wDATE
194000070108     C                   move      G02inv        wDATEc            8
194100031121      *
194200000808     C*
194300940809     C                   ENDSR
194400940809     C**************************************************************************
194900940809**
195500030129Codice fornitore inesistente                                                   6
196100940909Codice filiale errata o non gestita                                           12
