000100891030     H DFTNAME(CN0500) DATEDIT(*YMD)
000200010830     F* TRTB37R0*-----------------------------------------------------*
000300920723     F*-----------------*                                             *
000400010830     F*              - GESTIONE TABELLA "TIPI INCASSO"                *
000500920723     F*---------------------------------------------------------------*
000600010830     FTRTB37D0  CF   E             WORKSTN
000700150907     F                                     SFILE(TB37DSF1:nrr1)
000800150831     FCNclp00F  IF   E           K DISK
000900150813     FCNACO00F  IF   E           K DISK
001000150901     Ftabel00f  uF   E           K DISK
001100150813     D ana300ds      E DS
001200150901     D dstm          E DS
001300930208     D PARAM           DS
001400150814     D  VIDFLG                        2
001500150831     D PARAM2          DS
001600150831     D  VIDFLG2                       2
001700150831     D  parcli                        7
001800150831     D  paropz                        1
001900930322     D KPJBA         E DS
002000920723     D  LIBSYS                92    100
002100920723     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
002200150831     d datads          DS
002300150831     d  aa                            2  0
002400150831     d  mm                            2  0
002500150831     d  gg                            2  0
002600150831     d datads1         DS
002700150831     d  gg1                           2  0
002800150831     d  mm1                           2  0
002900150831     d  aa1                           2  0
003000150901     D chk             S              2    DIM(9)
003100150904      *---------------------------------------------------------------------
003200150904      * specifiche per ordinamento forzato sfl
003300150904     d NotUsed         S             16A
003400150904     d ReturnSize      S              9B 0
003500150904     d SizeList        S              9B 0
003600150904     d righe           S              4  0
003700150904     d w_ord           S              1  0         inz(1)
003800150904     d*---
003900150904     d MaxKey          C                   2
004000150904     d Ascendente      C                   1
004100150904     d Discendente     C                   2
004200150904     d Carattere       C                   6
004300150904     d Put             C                   1
004400150904     d EndPut          C                   2
004500150904     d Get             C                   3
004600150904     d Numerico        C                   8
004700150904     d SflRcd          DS
004800150904     d  VSKSC
004900150904     d  VSDES
005000150904     d  VMULT
005100150904     d  VSSCE
005200150904     d  VSTM
005300150904     d  VSCA
005400150904     d  VSBA
005500150904     d  VSBB
005600150904     d  VSBC
005700150904     d  VSSC
005800150904     d  VSTO
005900150904     d  VSS2
006000150904     d  VSDUS
006100150904     d  VSFVE
006200150904     d  VSKSB
006300150904     d  VSGG1
006400150904     d  VSGG2
006500150904     d  VSFTI
006600150904     d  VSTP
006700150904     d  VH60
006800150904     d  VH61
006900150904     d  Selected                      1A
007000150904
007100150904      /COPY QSYSINC/QRPGLESRC,QLGSORT
007200150904     d QLGKL                         16    DIM(MaxKey)
007300150904     d  QLGSP00                       9B 0 OVERLAY(QLGKL:00001)
007400150904     d  QLGSS00                       9B 0 OVERLAY(QLGKL:00005)
007500150904     d  QLGDT00                       9B 0 OVERLAY(QLGKL:00009)
007600150904     d  QLGSO00                       9B 0 OVERLAY(QLGKL:00013)
007700150904      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
007800150904      /COPY QSYSINC/QRPGLESRC,QUSEC
007900920731     C*****************************************************************
008000891030     C     *ENTRY        PLIST
008100891030     C                   PARM                    KPJBA
008200010830      *
008300891030     C                   Z-ADD     1             CODUT
008400920723     C                   CALL      'X§PARUT'
008500920723     C                   PARM                    UT§DSE
008600891030     C                   MOVEL     RAGUT         RSUT
008700920731     C                   MOVEL     RAGUT         PARDUT           30
008800150813     C     KACO          KLIST
008900150813     C                   KFLD                    CODUT
009000150813     C                   KFLD                    KCC               4 0
009100150813     C                   KFLD                    KsC               7 0
009200150813     C                   z-add     151           Kcc
009300150901     C* CHIAVE X POSIZIONAMENTO TABELLE
009400150901     C     KTAB          KLIST
009500150901     C                   KFLD                    CODUT
009600150901     C                   KFLD                    COD               2
009700150901     C                   KFLD                    KEY               8
009800150901     C     KTAB2         KLIST
009900150901     C                   KFLD                    CODUT
010000150901     C                   KFLD                    COD
010100920723     C*
010200920723     C*---------------------------------------------------------------*
010300010830     C                   MOVEL     *BLANKS       V1SCE
010400920204     C*
010500150907     c                   do        *hival
010600150813     C                   EXFMT     TB37D01
010700010830     C** F3=FINE
010800150907     C   KC              leave
010900150813     C   KD              EXSR      call300
011000150813     C     v1cli         IFNE      *ZEROS
011100150813     C     v1cli         andne     *blank
011200150901     c                   move      v1cli         ksc
011300150813     C     KACO          CHAIN     CNACO000                           30
011400150813     C     ACOFLG        IFNE      ' '
011500150813     C     *IN30         OREQ      '1'
011600150813     C* CODICE BANCA   ANNULLATO
011700150813     C                   movel     *all'?'       vdcli
011800150904     C                   clear                   trovato           1
011900150813     c                   else
012000150813     c                   movel     acorag        vdcli
012100150904     C                   move      'X'           trovato
012200150813     C                   endif
012300150813     C                   endif
012400150904     C**
012500150831     c                   if        v1sce <> *blank and v1cli <> *blank
012600150904     c                             and trovato = 'X'
012700150831     C                   MOVEL     V1SCE         VIDFLG2
012800150831     C                   MOVEL     V1cli         parcli
012900150831     C                   MOVEL     *blank        paropz
013000150831     C                   MOVEL     PARAM2        KPJBU
013100010830     C                   CALL      'TRTB37R'
013200010830     C                   PARM                    KPJBA
013300150831     c                   else
013400150831     c                   exsr      gesnewsfl
013500150831     c                   endif
013600150907     c                   enddo
013700920731     C*
013800891030     C                   SETON                                        LR
013900150813     c*___________________________________________________________
014000150813     c     call300       begsr
014100150813     c*___________________________________________________________
014200150813     C*
014300150813     C                   RESET                   ana300ds
014400150813     C                   MOVEL     '01'          OPZ300
014500150813     C                   MOVEL     ana300ds      KPJBU
014600150813     C                   CALL      'ANA300R'
014700150813     C                   PARM                    KPJBA
014800150813     C                   MOVEL     KPJBU         ana300ds
014900150813     C     KSC300        IFNE      *BLANK
015000150813     C                   MOVE      ksc300        v1cli
015100150813     C                   ELSE
015200150813     C                   MOVEL     *zero         v1cli
015300150813     C                   ENDIF
015400150813     C*
015500150813     C                   endsr
015600920731     C*
015700150831     C**-------------------------------------------------------------**
015800150831     C** Gestione nuovo  SUBFILE                                     **
015900150831     C**-------------------------------------------------------------**
016000150831     C     gesnewsfl     BEGSR
016100150907     c                   eval      vf11  = 'F11=Ordina Rag.Soc'
016200150831     c                   exsr      carsf2
016300150831     C                   ENDSR
016400150831     C**-------------------------------------------------------------**
016500150831     C** CARICAMENTO DEL SUBFILE                                     **
016600150831     C**-------------------------------------------------------------**
016700150831     C     carsf2        BEGSR
016800150902     c     ricarica      tag
016900150907     C                   Z-ADD     0             nrr1              4 0
017000150831     C*
017100150831     C* PULIZIA SUBFILE
017200150831     C                   SETON                                        36
017300150831     C                   WRITE     TB37DCT1
017400150831     C                   SETOFF                                       36
017500150831     C* CARICO SUBFILE
017600150831     C                   MOVEL     'TM'          COD
017700150901     C                   MOVEL     v1cli         KEY
017800150901     C     KTAB          SETLL     TABEL
017900150901     C     KTAB2         READE(N)  TABEL                                  30
018000150831     C*
018100150831     C     *IN30         DOWEQ     '0'
018200150831     C* ESCLUDO ANNULLATI
018300150831     C     TBLFLG        IFEQ      ' '
018400150831     C                   MOVEL     TBLUNI        DSTM
018500150831     C                   MOVEL     ' '           VsSCE
018600150831     C                   SETOFF                                       5556
018700150831     C*
018800150831     C* CARICO I CODICI CLIENTI CON TIPO INCASSO richiesto
018900150831     C                   MOVEL     TBLKEY        VsKSC
019000150831     C                   MOVEL     §TMRAG        Vsdes
019100150901      *
019200150901     C                   clear                   chk
019300150831     C                   MOVEL     §tmfto        Vsto
019400150901     c                   if        vsto <> *blank
019500150901     C                   MOVEL     'TO'          chk(1)
019600150901     c                   endif
019700150831     C                   MOVEL     §tmfba        Vsba
019800150901     c                   if        vsba <> *blank
019900150901     C                   MOVEL     'BA'          chk(2)
020000150901     c                   endif
020100150831     C                   MOVEL     §tmfca        Vsca
020200150901     c                   if        vsca <> *blank
020300150901     C                   MOVEL     'CA'          chk(3)
020400150901     c                   endif
020500150831     C                   MOVEL     §tmftm        Vstm
020600150901     c                   if        vstm <> *blank
020700150901     C                   MOVEL     'TM'          chk(4)
020800150901     c                   endif
020900150901     C                   MOVEL     §tmfbc        Vsbc
021000150901     c                   if        vsbc <> *blank
021100150901     C                   MOVEL     'BC'          chk(5)
021200150901     c                   endif
021300150901     C                   MOVEL     §tmfsc        Vssc
021400150901     c                   if        vssc <> *blank
021500150901     C                   MOVEL     'SC'          chk(6)
021600150901     c                   endif
021700150831     C                   MOVEL     §TMfbb        Vsbb
021800150901     c                   if        vsbb <> *blank
021900150901     C                   MOVEL     'BB'          chk(7)
022000150901     c                   endif
022100150831     C                   MOVEL     §TMfs2        Vss2
022200150901     c                   if        vss2 <> *blank
022300150901     C                   MOVEL     'S2'          chk(8)
022400150901     c                   endif
022500150901     C                   MOVEL     §TMftp        Vstp
022600150901     c                   if        vstp <> *blank
022700150901     C                   MOVEL     'TP'          chk(9)
022800150901     c                   endif
022900150904      *
023000150904     c                   if        v1sce <> *blank
023100150904     c     v1sce         lookup    chk                                    61
023200150904     c  n61              goto      bypass
023300150904     c                   endif
023400150904      *
023500150831     c                   clear                   vmult             2
023600150831      * esegue test per capire se è multiabilitato
023700150831     c                   z-add     1             xx                3 0
023800150901     c                   do        9             xx
023900150831     c                   if        chk(xx) <> *blank
024000150831     c                   if        vmult = *blank
024100150831     c                   move      chk(xx)       vmult
024200150831     c                   else
024300150831     c                   move      'XX'          vmult
024400150831     c                   endif
024500150831     c                   endif
024600150831     c                   enddo
024700150831     C*seconda riga
024800150831     C                   MOVEL     §TMFVE        VsFVE
024900150831     C                   MOVEL     §TMKSB        VsKSB
025000150831     C                   MOVEL     §TMgg1        Vsgg1
025100150831     C                   MOVEL     §TMgg2        Vsgg2
025200150831     C                   MOVEL     §TMFTI        VsFTI
025300150904     c                   setoff                                       60
025400150904     c                   clear                   vh60
025500150904     c                   if        vsgg1 = 0 and vsgg2 = 0 and vsksb = 0 and
025600150904     c                             vsfti = *blank and vsfve = *blank
025700150904     c                   seton                                        60
025800150904     c                   move      '1'           vh60
025900150904     c                   endif
026000150831      *ricerca data ultima bolla
026100150831     c                   clear                   vsdus
026200150831     c                   z-add     151           kcc
026300150831     c                   z-add     vsksc         ksc
026400150831     c     kaco          chain     cnclp00f
026500150831     c                   if        %found(cnclp00f) and
026600150831     c                             clpdus <> 0
026700150831     C                   MOVE      clpdus        datads
026800150831     C                   MOVE      aa            aa1
026900150831     C                   MOVE      mm            mm1
027000150831     C                   MOVE      gg            gg1
027100150831     C                   MOVE      datads1       vsdus
027200150831     c                   endif
027300150907     c                   if        nrr1= 9999
027400150831     c                   leave
027500150831     C                   END
027600150907     C                   ADD       1             nrr1
027700150831     C                   WRITE     TB37DSF1
027800150831     C                   END
027900150904     c     bypass        tag
028000150901     C     KTAB2         READE(N)  TABEL                                  30
028100150831     C                   END
028200150831     C*
028300150831     C* EMETTO SUBFILE
028400150907     C     nrr1          IFGT      0
028500150831     C*
028600150907     C                   Z-ADD     1             c1rrn
028700150907     C                   Z-ADD     c1rrn         wc1rrn
028800150907     C                   Z-ADD     1             rec1
028900150831     C     SOPRA2        TAG
029000150907     C                   Z-ADD     wc1rrn        rec1
029100150831     C                   WRITE     TB37D021
029200150831     C                   EXFMT     TB37DCT1
029300150907     c                   z-add     c1rrn         wc1rrn            5 0
029400150831     C*
029500150901     c                   if        *inkf
029600150903     c                   movel(p)  param         kpjbu
029700150901     c                   call      'TRTB37R1'
029800150903     c                   parm                    kpjba
029900150903     c                   movel     kpjbu         param
030000150901     C                   clear                   PARAM2
030100150901     c                   movel     vidflg        vidflg2
030200150907     C                   clear                   vidflg
030300150902     c                   if        vidflg2 <> *blank
030400150901     C                   MOVEL     PARAM2        KPJBU
030500150901     C                   CALL      'TRTB37R'
030600150901     C                   PARM                    KPJBA
030700150902     c                   endif
030800150902     c                   goto      ricarica
030900150901     c                   endif
031000150907     C                   IF        *inkk
031100150904     c                   exsr      ordina
031200150904     c                   endif
031300150831     C* CMD12 - RITORNO
031400150907     C   kl              leavesr
031500150831     C*
031600150831     C* CONTROLLO DATI SUBFILE
031700150831     C                   READC     TB37DSF1                               31
031800150907     c   31              goto      sopra2
031900150831     C  N31Vssce         IFEQ      '2'
032000150831      *richiamo pgm per esplosione del record se esiste più di un tipo incasso
032100150831      * richiamo videata di selezione tipo incasso
032200150831     c                   if        vmult = 'XX'
032300150903     c                   movel(p)  param         kpjbu
032400150903     c                   call      'TRTB37R1'
032500150903     c                   parm                    kpjba
032600150903     c                   movel     kpjbu         param
032700150902     c                   move      vidflg        vidflg2
032800150907     c                   clear                   vidflg
032900150902     c                   else
033000150902     C                   MOVEL     Vmult         VIDFLG2
033100150831     c                   endif
033200150831      * richiamo pgm manutenzione tipo incasso
033300150902     c                   if        vidflg2 <> *blank
033400150831     C                   MOVE      Vsksc         parcli
033500150831     C                   MOVEL     *blank        paropz
033600150831     C                   MOVEL     PARAM2        KPJBU
033700150831     c                   call      'TRTB37R'
033800150831     c                   parm                    kpjba
033900150902     c                   endif
034000150902      *
034100150902     c                   move      *blank        vssce
034200150902     c                   update    tb37dsf1
034300150908     C                   GOTO      ricarica
034400150831     C                   END
034500150908     C* N31Vssce         IFEQ      '4'
034600150908     C*                  MOVEL     Vsksc         KEY
034700150831      *aggiorna
034800150908     c*    ktab          chain     tabel00f
034900150908     c*                  if        %found(tabel00f)
035000150908     c*                  move      '*'           tblflg
035100150908     c*                  move      *blank        tblftr
035200150908     c*                  z-add     0             tbldtr
035300150908     c*                  update    tabel
035400150908     c*                  endif
035500150908     c*                  move      *blank        vssce
035600150908     c*                  update    tb37dsf1
035700150908     C*                  GOTO      ricarica
035800150908     C*                  END
035900150831     C*
036000150831     C                   END
036100150831     C                   ENDSR
036200150904      *-------------------------------------------------------------------------
036300150904      * ordina - riscrive il sfl ordinato
036400150904      *-------------------------------------------------------------------------
036500150904     C     ordina        BEGSR
036600150907     C                   EVAL      righe = nrr1
036700150904      * IMPOSTO ENTRATA AD 'A'
036800150907     C                   EVAL      nrr1  = 0
036900150904      *
037000150904     C                   CLEAR                   QLGSCB
037100150904     C                   CLEAR                   QLGSCB00
037200150904
037300150904     ** 2 campi chiave.
037400150904     C                   EVAL      QLGNBRK    = 1
037500150904     c                   select
037600150904     c* ordinamento per prv/rag.sociale destinatario
037700150904     c                   when      w_ord=1
037800150904     **  rag. sociale cliente  2 campo
037900150904     C                   EVAL      QLGSP      = 1 + %SIZE(vsksc)
038000150904     C                   EVAL      QLGSS      = %SIZE(vsdes)
038100150904     C                   EVAL      QLGDT      = carattere
038200150904     C                   EVAL      QLGSO      = Ascendente
038300150904     C                   EVAL      QLGKL(1)   = QLGSKL
038400150904     c                   eval      w_ord = 2
038500150904     c                   eval      vf11  = 'F11=Ordina Cliente'
038600150904     c* ordinamento per rag.sociale/prv destinatario
038700150904     c                   when      w_ord=2
038800150904     C                   EVAL      QLGSP      = 1
038900150904     C                   EVAL      QLGSS      = %SIZE(vsksc)
039000150904     C                   EVAL      QLGDT      = carattere
039100150904     C                   EVAL      QLGSO      = Ascendente
039200150904     C                   EVAL      QLGKL(1)   = QLGSKL
039300150904     c                   eval      w_ord = 1
039400150904     c                   eval      vf11  = 'F11=Ordina Rag.Soc'
039500150904     ** ragione sociale a posizione 1, carattere, 24 byte,
039600150904     c                   endsl
039700150904
039800150904      * Load other sort parameters.
039900150904     C                   EVAL      QLGLB     = 80 + 16 * MaxKey
040000150904     C                   EVAL      QLGRL     = %SIZE(SflRcd) - 1
040100150904     C                   EVAL      QLGRT     = 8
040200150904     C                   EVAL      QLGOKL    = 80
040300150904     C                   EVAL      QLGLKE    = 16
040400150904     C                   EVAL      QLGLSS    = 290
040500150904
040600150904      * Initialize Sort I/O API fields.
040700150904     C                   EVAL      QLGRL00  = QLGRL
040800150904     C                   EVAL      QLGRC00  = 1
040900150904     C                   CLEAR                   QUSEI
041000150904     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
041100150904
041200150904      * First step - Initialize the sort routine.
041300150904     C                   CALL      'QLGSORT'
041400150904     C                   PARM                    QLGSCB
041500150904     C                   PARM                    NotUsed
041600150904     C                   PARM                    NotUsed
041700150904     C                   PARM                    SizeList
041800150904     C                   PARM                    ReturnSize
041900150904     C                   PARM                    QUSEC
042000150904
042100150904      * Next step - Write records to I/O routine.
042200150904     C                   EVAL      QLGRT00 = Put
042300150904
042400150904     C                   DO        *hival        cx                4 0
042500150904
042600150904     C     cx            CHAIN     TB37DSF1                           50
042700150904     c   50              leave
042800150904     ** Solo le righe con Selected = 'Y' sono riordinate,
042900150904     ** quindi per fare un ordinamento di tutte le righe
043000150904     ** metto 'Y' sempre.
043100150904     C                   EVAL      Selected = 'Y'
043200150904
043300150904     C                   CLEAR                   QUSEI
043400150904     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
043500150904     C                   CALL      'QLGSRTIO'
043600150904     C                   PARM                    QLGSCB00
043700150904     C                   PARM                    SflRcd
043800150904     C                   PARM                    NotUsed
043900150904     C                   PARM                    SizeList
044000150904     C                   PARM                    NotUsed
044100150904     C                   PARM                    QUSEC
044200150904
044300150904     C                   ENDDO
044400150904
044500150904      * Next step - Signal end of input, clear subfile for reload.
044600150904     C                   EVAL      QLGRT00 = EndPut
044700150904     C                   CLEAR                   QUSEI
044800150904     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
044900150904
045000150904     C                   CALL      'QLGSRTIO'
045100150904     C                   PARM                    QLGSCB00
045200150904     C                   PARM                    SflRcd
045300150904     C                   PARM                    NotUsed
045400150904     C                   PARM                    SizeList
045500150904     C                   PARM                    NotUsed
045600150904     C                   PARM                    QUSEC
045700150904
045800150904     C                   MOVEL     '1'           *IN36
045900150904     C                   WRITE     tb37dct1
046000150904     C                   MOVEL     '0'           *IN36
046100150904
046200150904      * Final step - Write the records back to the subfile.
046300150904     C                   EVAL      QLGRT00 = Get
046400150904     C                   DO        righe         x                 4 0
046500150904     C                   CLEAR                   QUSEI
046600150904     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
046700150904     C                   CALL      'QLGSRTIO'
046800150904     C                   PARM                    QLGSCB00
046900150904     C                   PARM                    NotUsed
047000150904     C                   PARM                    SflRcd
047100150904     C                   PARM                    QLGRL00
047200150904     C                   PARM                    NotUsed
047300150904     C                   PARM                    QUSEC
047400150904      *
047500150907     c                   z-add     x             nrr1
047600150904     C                   WRITE     TB37DSF1
047700150904     C                   ENDDO
047800150904
047900150904      *
048000150904     C                   ENDSR
