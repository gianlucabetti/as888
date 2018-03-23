000100891030     H DFTNAME(CN0500) DATEDIT(*YMD)
000200010830     F* TRTB37R0*-----------------------------------------------------*
000300920723     F*-----------------*                                             *
000400010830     F*              - GESTIONE TABELLA "TIPI INCASSO"                *
000500920723     F*---------------------------------------------------------------*
000600010830     FTRTB37D0  CF   E             WORKSTN
000700150907     F                                     SFILE(TB37DSF1:nrr1)
000800150831     FCNclp00F  IF   E           K DISK
000900150813     FCNACO00F  IF   E           K DISK
001000170309     F*//tabel00f  uF   E           K DISK
001100170309     fTABEL00F  if   e           k disk
001200150813     D ana300ds      E DS
001300150901     D dstm          E DS
001400930208     D PARAM           DS
001500150814     D  VIDFLG                        2
001600150831     D PARAM2          DS
001700150831     D  VIDFLG2                       2
001800150831     D  parcli                        7
001900150831     D  paropz                        1
002000930322     D KPJBA         E DS
002100920723     D  LIBSYS                92    100
002200920723     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
002300150831     d datads          DS
002400150831     d  aa                            2  0
002500150831     d  mm                            2  0
002600150831     d  gg                            2  0
002700150831     d datads1         DS
002800150831     d  gg1                           2  0
002900150831     d  mm1                           2  0
003000150831     d  aa1                           2  0
003100150901     D chk             S              2    DIM(9)
003200150904      *---------------------------------------------------------------------
003300150904      * specifiche per ordinamento forzato sfl
003400150904     d NotUsed         S             16A
003500150904     d ReturnSize      S              9B 0
003600150904     d SizeList        S              9B 0
003700150904     d righe           S              4  0
003800150904     d w_ord           S              1  0         inz(1)
003900170317     d xy              s              5  0         inz
004000150904     d*---
004100150904     d MaxKey          C                   2
004200150904     d Ascendente      C                   1
004300150904     d Discendente     C                   2
004400150904     d Carattere       C                   6
004500150904     d Put             C                   1
004600150904     d EndPut          C                   2
004700150904     d Get             C                   3
004800150904     d Numerico        C                   8
004900150904     d SflRcd          DS
005000150904     d  VSKSC
005100150904     d  VSDES
005200150904     d  VMULT
005300150904     d  VSSCE
005400150904     d  VSTM
005500150904     d  VSCA
005600150904     d  VSBA
005700150904     d  VSBB
005800150904     d  VSBC
005900150904     d  VSSC
006000150904     d  VSTO
006100150904     d  VSS2
006200150904     d  VSDUS
006300150904     d  VSFVE
006400150904     d  VSKSB
006500150904     d  VSGG1
006600150904     d  VSGG2
006700150904     d  VSFTI
006800150904     d  VSTP
006900150904     d  VH60
007000150904     d  VH61
007100150904     d  Selected                      1A
007200150904
007300150904      /COPY QSYSINC/QRPGLESRC,QLGSORT
007400150904     d QLGKL                         16    DIM(MaxKey)
007500150904     d  QLGSP00                       9B 0 OVERLAY(QLGKL:00001)
007600150904     d  QLGSS00                       9B 0 OVERLAY(QLGKL:00005)
007700150904     d  QLGDT00                       9B 0 OVERLAY(QLGKL:00009)
007800150904     d  QLGSO00                       9B 0 OVERLAY(QLGKL:00013)
007900150904      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
008000150904      /COPY QSYSINC/QRPGLESRC,QUSEC
008100920731     C*****************************************************************
008200891030     C     *ENTRY        PLIST
008300891030     C                   PARM                    KPJBA
008400010830      *
008500891030     C                   Z-ADD     1             CODUT
008600920723     C                   CALL      'X§PARUT'
008700920723     C                   PARM                    UT§DSE
008800891030     C                   MOVEL     RAGUT         RSUT
008900920731     C                   MOVEL     RAGUT         PARDUT           30
009000150813     C     KACO          KLIST
009100150813     C                   KFLD                    CODUT
009200150813     C                   KFLD                    KCC               4 0
009300150813     C                   KFLD                    KsC               7 0
009400150813     C                   z-add     151           Kcc
009500150901     C* CHIAVE X POSIZIONAMENTO TABELLE
009600150901     C     KTAB          KLIST
009700150901     C                   KFLD                    CODUT
009800150901     C                   KFLD                    COD               2
009900150901     C                   KFLD                    KEY               8
010000150901     C     KTAB2         KLIST
010100150901     C                   KFLD                    CODUT
010200150901     C                   KFLD                    COD
010300920723     C*
010400920723     C*---------------------------------------------------------------*
010500010830     C                   MOVEL     *BLANKS       V1SCE
010600920204     C*
010700150907     c                   do        *hival
010800150813     C                   EXFMT     TB37D01
010900010830     C** F3=FINE
011000150907     C   KC              leave
011100150813     C   KD              EXSR      call300
011200150813     C     v1cli         IFNE      *ZEROS
011300150813     C     v1cli         andne     *blank
011400150901     c                   move      v1cli         ksc
011500150813     C     KACO          CHAIN     CNACO000                           30
011600150813     C     ACOFLG        IFNE      ' '
011700150813     C     *IN30         OREQ      '1'
011800150813     C* CODICE BANCA   ANNULLATO
011900150813     C                   movel     *all'?'       vdcli
012000150904     C                   clear                   trovato           1
012100150813     c                   else
012200150813     c                   movel     acorag        vdcli
012300150904     C                   move      'X'           trovato
012400150813     C                   endif
012500150813     C                   endif
012600150904     C**
012700150831     c                   if        v1sce <> *blank and v1cli <> *blank
012800150904     c                             and trovato = 'X'
012900150831     C                   MOVEL     V1SCE         VIDFLG2
013000150831     C                   MOVEL     V1cli         parcli
013100150831     C                   MOVEL     *blank        paropz
013200150831     C                   MOVEL     PARAM2        KPJBU
013300010830     C                   CALL      'TRTB37R'
013400010830     C                   PARM                    KPJBA
013500150831     c                   else
013600150831     c                   exsr      gesnewsfl
013700150831     c                   endif
013800150907     c                   enddo
013900920731     C*
014000891030     C                   SETON                                        LR
014100150813     c*___________________________________________________________
014200150813     c     call300       begsr
014300150813     c*___________________________________________________________
014400150813     C*
014500150813     C                   RESET                   ana300ds
014600150813     C                   MOVEL     '01'          OPZ300
014700150813     C                   MOVEL     ana300ds      KPJBU
014800150813     C                   CALL      'ANA300R'
014900150813     C                   PARM                    KPJBA
015000150813     C                   MOVEL     KPJBU         ana300ds
015100150813     C     KSC300        IFNE      *BLANK
015200150813     C                   MOVE      ksc300        v1cli
015300150813     C                   ELSE
015400150813     C                   MOVEL     *zero         v1cli
015500150813     C                   ENDIF
015600150813     C*
015700150813     C                   endsr
015800920731     C*
015900150831     C**-------------------------------------------------------------**
016000150831     C** Gestione nuovo  SUBFILE                                     **
016100150831     C**-------------------------------------------------------------**
016200150831     C     gesnewsfl     BEGSR
016300150907     c                   eval      vf11  = 'F11=Ordina Rag.Soc'
016400150831     c                   exsr      carsf2
016500150831     C                   ENDSR
016600150831     C**-------------------------------------------------------------**
016700150831     C** CARICAMENTO DEL SUBFILE                                     **
016800150831     C**-------------------------------------------------------------**
016900150831     C     carsf2        BEGSR
017000150902     c     ricarica      tag
017100150907     C                   Z-ADD     0             nrr1              4 0
017200150831     C*
017300150831     C* PULIZIA SUBFILE
017400150831     C                   SETON                                        36
017500150831     C                   WRITE     TB37DCT1
017600150831     C                   SETOFF                                       36
017700150831     C* CARICO SUBFILE
017800150831     C                   MOVEL     'TM'          COD
017900150901     C                   MOVEL     v1cli         KEY
018000150901     C     KTAB          SETLL     TABEL
018100170309     C*//  KTAB2         READE(N)  TABEL                                  30
018200170309     C     KTAB2         READE     TABEL                                  30
018300150831     C*
018400150831     C     *IN30         DOWEQ     '0'
018500150831     C* ESCLUDO ANNULLATI
018600150831     C     TBLFLG        IFEQ      ' '
018700150831     C                   MOVEL     TBLUNI        DSTM
018800150831     C                   MOVEL     ' '           VsSCE
018900150831     C                   SETOFF                                       5556
019000150831     C*
019100150831     C* CARICO I CODICI CLIENTI CON TIPO INCASSO richiesto
019200150831     C                   MOVEL     TBLKEY        VsKSC
019300150831     C                   MOVEL     §TMRAG        Vsdes
019400150901      *
019500150901     C                   clear                   chk
019600150831     C                   MOVEL     §tmfto        Vsto
019700150901     c                   if        vsto <> *blank
019800150901     C                   MOVEL     'TO'          chk(1)
019900150901     c                   endif
020000150831     C                   MOVEL     §tmfba        Vsba
020100150901     c                   if        vsba <> *blank
020200150901     C                   MOVEL     'BA'          chk(2)
020300150901     c                   endif
020400150831     C                   MOVEL     §tmfca        Vsca
020500150901     c                   if        vsca <> *blank
020600150901     C                   MOVEL     'CA'          chk(3)
020700150901     c                   endif
020800150831     C                   MOVEL     §tmftm        Vstm
020900150901     c                   if        vstm <> *blank
021000150901     C                   MOVEL     'TM'          chk(4)
021100150901     c                   endif
021200150901     C                   MOVEL     §tmfbc        Vsbc
021300150901     c                   if        vsbc <> *blank
021400150901     C                   MOVEL     'BC'          chk(5)
021500150901     c                   endif
021600150901     C                   MOVEL     §tmfsc        Vssc
021700150901     c                   if        vssc <> *blank
021800150901     C                   MOVEL     'SC'          chk(6)
021900150901     c                   endif
022000150831     C                   MOVEL     §TMfbb        Vsbb
022100150901     c                   if        vsbb <> *blank
022200150901     C                   MOVEL     'BB'          chk(7)
022300150901     c                   endif
022400150831     C                   MOVEL     §TMfs2        Vss2
022500150901     c                   if        vss2 <> *blank
022600150901     C                   MOVEL     'S2'          chk(8)
022700150901     c                   endif
022800150901     C                   MOVEL     §TMftp        Vstp
022900150901     c                   if        vstp <> *blank
023000150901     C                   MOVEL     'TP'          chk(9)
023100150901     c                   endif
023200150904      *
023300150904     c                   if        v1sce <> *blank
023400150904     c     v1sce         lookup    chk                                    61
023500150904     c  n61              goto      bypass
023600150904     c                   endif
023700150904      *
023800150831     c                   clear                   vmult             2
023900150831      * esegue test per capire se è multiabilitato
024000150831     c                   z-add     1             xx                3 0
024100150901     c                   do        9             xx
024200150831     c                   if        chk(xx) <> *blank
024300150831     c                   if        vmult = *blank
024400150831     c                   move      chk(xx)       vmult
024500150831     c                   else
024600150831     c                   move      'XX'          vmult
024700150831     c                   endif
024800150831     c                   endif
024900150831     c                   enddo
025000150831     C*seconda riga
025100150831     C                   MOVEL     §TMFVE        VsFVE
025200150831     C                   MOVEL     §TMKSB        VsKSB
025300150831     C                   MOVEL     §TMgg1        Vsgg1
025400150831     C                   MOVEL     §TMgg2        Vsgg2
025500150831     C                   MOVEL     §TMFTI        VsFTI
025600150904     c                   setoff                                       60
025700150904     c                   clear                   vh60
025800150904     c                   if        vsgg1 = 0 and vsgg2 = 0 and vsksb = 0 and
025900150904     c                             vsfti = *blank and vsfve = *blank
026000150904     c                   seton                                        60
026100150904     c                   move      '1'           vh60
026200150904     c                   endif
026300150831      *ricerca data ultima bolla
026400150831     c                   clear                   vsdus
026500150831     c                   z-add     151           kcc
026600150831     c                   z-add     vsksc         ksc
026700150831     c     kaco          chain     cnclp00f
026800150831     c                   if        %found(cnclp00f) and
026900150831     c                             clpdus <> 0
027000150831     C                   MOVE      clpdus        datads
027100150831     C                   MOVE      aa            aa1
027200150831     C                   MOVE      mm            mm1
027300150831     C                   MOVE      gg            gg1
027400150831     C                   MOVE      datads1       vsdus
027500150831     c                   endif
027600150907     c                   if        nrr1= 9999
027700150831     c                   leave
027800150831     C                   END
027900150907     C                   ADD       1             nrr1
028000150831     C                   WRITE     TB37DSF1
028100150831     C                   END
028200150904     c     bypass        tag
028300170309     C*//  KTAB2         READE(N)  TABEL                                  30
028400170309     C     KTAB2         READE     TABEL                                  30
028500150831     C                   END
028600150831     C*
028700150831     C* EMETTO SUBFILE
028800150907     C     nrr1          IFGT      0
028900150831     C*
029000150907     C                   Z-ADD     1             c1rrn
029100150907     C                   Z-ADD     c1rrn         wc1rrn
029200150907     C                   Z-ADD     1             rec1
029300150831     C     SOPRA2        TAG
029400150907     C                   Z-ADD     wc1rrn        rec1
029500150831     C                   WRITE     TB37D021
029600150831     C                   EXFMT     TB37DCT1
029700150907     c                   z-add     c1rrn         wc1rrn            5 0
029800150831     C*
029900150901     c                   if        *inkf
030000150903     c                   movel(p)  param         kpjbu
030100150901     c                   call      'TRTB37R1'
030200150903     c                   parm                    kpjba
030300150903     c                   movel     kpjbu         param
030400150901     C                   clear                   PARAM2
030500150901     c                   movel     vidflg        vidflg2
030600150907     C                   clear                   vidflg
030700150902     c                   if        vidflg2 <> *blank
030800150901     C                   MOVEL     PARAM2        KPJBU
030900150901     C                   CALL      'TRTB37R'
031000150901     C                   PARM                    KPJBA
031100150902     c                   endif
031200150902     c                   goto      ricarica
031300150901     c                   endif
031400150907     C                   IF        *inkk
031500150904     c                   exsr      ordina
031600150904     c                   endif
031700150831     C* CMD12 - RITORNO
031800150907     C   kl              leavesr
031900150831     C*
032000150831     C* CONTROLLO DATI SUBFILE
032100150831     C                   READC     TB37DSF1                               31
032200150907     c   31              goto      sopra2
032300150831     C  N31Vssce         IFEQ      '2'
032400150831      *richiamo pgm per esplosione del record se esiste più di un tipo incasso
032500150831      * richiamo videata di selezione tipo incasso
032600150831     c                   if        vmult = 'XX'
032700150903     c                   movel(p)  param         kpjbu
032800150903     c                   call      'TRTB37R1'
032900150903     c                   parm                    kpjba
033000150903     c                   movel     kpjbu         param
033100150902     c                   move      vidflg        vidflg2
033200150907     c                   clear                   vidflg
033300150902     c                   else
033400150902     C                   MOVEL     Vmult         VIDFLG2
033500150831     c                   endif
033600150831      * richiamo pgm manutenzione tipo incasso
033700150902     c                   if        vidflg2 <> *blank
033800150831     C                   MOVE      Vsksc         parcli
033900150831     C                   MOVEL     *blank        paropz
034000150831     C                   MOVEL     PARAM2        KPJBU
034100150831     c                   call      'TRTB37R'
034200150831     c                   parm                    kpjba
034300150902     c                   endif
034400150902      *
034500150902     c                   move      *blank        vssce
034600150902     c                   update    tb37dsf1
034700150908     C                   GOTO      ricarica
034800150831     C                   END
034900150908     C* N31Vssce         IFEQ      '4'
035000150908     C*                  MOVEL     Vsksc         KEY
035100150831      *aggiorna
035200150908     c*    ktab          chain     tabel00f
035300150908     c*                  if        %found(tabel00f)
035400150908     c*                  move      '*'           tblflg
035500150908     c*                  move      *blank        tblftr
035600150908     c*                  z-add     0             tbldtr
035700150908     c*                  update    tabel
035800150908     c*                  endif
035900150908     c*                  move      *blank        vssce
036000150908     c*                  update    tb37dsf1
036100150908     C*                  GOTO      ricarica
036200150908     C*                  END
036300150831     C*
036400150831     C                   END
036500150831     C                   ENDSR
036600150904      *-------------------------------------------------------------------------
036700150904      * ordina - riscrive il sfl ordinato
036800150904      *-------------------------------------------------------------------------
036900150904     C     ordina        BEGSR
037000150907     C                   EVAL      righe = nrr1
037100150904      * IMPOSTO ENTRATA AD 'A'
037200150907     C                   EVAL      nrr1  = 0
037300150904      *
037400150904     C                   CLEAR                   QLGSCB
037500150904     C                   CLEAR                   QLGSCB00
037600150904
037700150904     ** 2 campi chiave.
037800150904     C                   EVAL      QLGNBRK    = 1
037900150904     c                   select
038000150904     c* ordinamento per prv/rag.sociale destinatario
038100150904     c                   when      w_ord=1
038200150904     **  rag. sociale cliente  2 campo
038300150904     C                   EVAL      QLGSP      = 1 + %SIZE(vsksc)
038400150904     C                   EVAL      QLGSS      = %SIZE(vsdes)
038500150904     C                   EVAL      QLGDT      = carattere
038600150904     C                   EVAL      QLGSO      = Ascendente
038700150904     C                   EVAL      QLGKL(1)   = QLGSKL
038800150904     c                   eval      w_ord = 2
038900150904     c                   eval      vf11  = 'F11=Ordina Cliente'
039000150904     c* ordinamento per rag.sociale/prv destinatario
039100150904     c                   when      w_ord=2
039200150904     C                   EVAL      QLGSP      = 1
039300150904     C                   EVAL      QLGSS      = %SIZE(vsksc)
039400150904     C                   EVAL      QLGDT      = carattere
039500150904     C                   EVAL      QLGSO      = Ascendente
039600150904     C                   EVAL      QLGKL(1)   = QLGSKL
039700150904     c                   eval      w_ord = 1
039800150904     c                   eval      vf11  = 'F11=Ordina Rag.Soc'
039900150904     ** ragione sociale a posizione 1, carattere, 24 byte,
040000150904     c                   endsl
040100150904
040200150904      * Load other sort parameters.
040300150904     C                   EVAL      QLGLB     = 80 + 16 * MaxKey
040400150904     C                   EVAL      QLGRL     = %SIZE(SflRcd) - 1
040500150904     C                   EVAL      QLGRT     = 8
040600150904     C                   EVAL      QLGOKL    = 80
040700150904     C                   EVAL      QLGLKE    = 16
040800150904     C                   EVAL      QLGLSS    = 290
040900150904
041000150904      * Initialize Sort I/O API fields.
041100150904     C                   EVAL      QLGRL00  = QLGRL
041200150904     C                   EVAL      QLGRC00  = 1
041300150904     C                   CLEAR                   QUSEI
041400150904     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
041500150904
041600150904      * First step - Initialize the sort routine.
041700150904     C                   CALL      'QLGSORT'
041800150904     C                   PARM                    QLGSCB
041900150904     C                   PARM                    NotUsed
042000150904     C                   PARM                    NotUsed
042100150904     C                   PARM                    SizeList
042200150904     C                   PARM                    ReturnSize
042300150904     C                   PARM                    QUSEC
042400150904
042500150904      * Next step - Write records to I/O routine.
042600150904     C                   EVAL      QLGRT00 = Put
042700150904
042800150904     C                   DO        *hival        cx                4 0
042900150904
043000150904     C     cx            CHAIN     TB37DSF1                           50
043100150904     c   50              leave
043200150904     ** Solo le righe con Selected = 'Y' sono riordinate,
043300150904     ** quindi per fare un ordinamento di tutte le righe
043400150904     ** metto 'Y' sempre.
043500150904     C                   EVAL      Selected = 'Y'
043600150904
043700150904     C                   CLEAR                   QUSEI
043800150904     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
043900150904     C                   CALL      'QLGSRTIO'
044000150904     C                   PARM                    QLGSCB00
044100150904     C                   PARM                    SflRcd
044200150904     C                   PARM                    NotUsed
044300150904     C                   PARM                    SizeList
044400150904     C                   PARM                    NotUsed
044500150904     C                   PARM                    QUSEC
044600150904
044700150904     C                   ENDDO
044800150904
044900150904      * Next step - Signal end of input, clear subfile for reload.
045000150904     C                   EVAL      QLGRT00 = EndPut
045100150904     C                   CLEAR                   QUSEI
045200150904     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
045300150904
045400150904     C                   CALL      'QLGSRTIO'
045500150904     C                   PARM                    QLGSCB00
045600150904     C                   PARM                    SflRcd
045700150904     C                   PARM                    NotUsed
045800150904     C                   PARM                    SizeList
045900150904     C                   PARM                    NotUsed
046000150904     C                   PARM                    QUSEC
046100150904
046200150904     C                   MOVEL     '1'           *IN36
046300150904     C                   WRITE     tb37dct1
046400150904     C                   MOVEL     '0'           *IN36
046500150904
046600150904      * Final step - Write the records back to the subfile.
046700150904     C                   EVAL      QLGRT00 = Get
046800170317     C*//                DO        righe         x                 4 0
046900170317     c                   FOR       xy = 1  To  Righe
047000150904     C                   CLEAR                   QUSEI
047100150904     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
047200150904     C                   CALL      'QLGSRTIO'
047300150904     C                   PARM                    QLGSCB00
047400150904     C                   PARM                    NotUsed
047500150904     C                   PARM                    SflRcd
047600150904     C                   PARM                    QLGRL00
047700150904     C                   PARM                    NotUsed
047800150904     C                   PARM                    QUSEC
047900150904      *
048000170317     c                   z-add     xy            nrr1
048100150904     C                   WRITE     TB37DSF1
048200170317     C                   ENDFOR
048300170317     C*//                ENDDO
048400150904
048500150904      *
048600150904     C                   ENDSR
