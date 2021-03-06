000100950718     H*------------------------------------------------------------------------*
000200011120     H* RICERCA E SCELTA TABELLA CON SELEZIONE MULTIPLA                        *
000300011218     H* PERMETTE LA RICERCA E LA SELEZIONE PER AUTISTA O PER TARGA          O  *
000401011120     H*                                                                        *
000402011120     h* PARAMETRI:
000403011218     h* ordinamento     1  I  (*blanks = codice  1=descizione)
000404011218     h* autista         2  O
000405011218     h* numero rifer.   7,0O
000408011218     h* codice ritorno  1  O  (comando premuto)
000600950718     H*------------------------------------------------------------------------*
000700950718     H DECEDIT('0,') DATEDIT(*DMY.)
000800950718     F*------------------------------------------------------------------------*
000900950718     F* DATA BASE
001000950718     F*------------------------------------------------------------------------*
001100011218     FTRmz41D   CF   E             WORKSTN
001200011120     F                                     SFILE(SFL01:RRN1)
001800011218     Fmzame04l  IF   E           K DISK
001801011218     FTABEL00F  IF   E           K DISK
015400011120      **********************************************************************
015500011120      * Constants
015600011120      *
015700011120      *     MaxKey - Maximum number of key fields allowed by this program
015800011120      *
015900011120      **********************************************************************
016000011120     D MaxKey          C                    1
016101011120     D Descrizione     C                   '1'
016300011120     D Ascendente      C                   1
016500011120     D Carattere       C                   6
016600011120     D Put             C                   1
016700011120     D EndPut          C                   2
016800011120     D Get             C                   3
017000011120      **********************************************************************
017100011120      * Standalone fields
017200011120      *
017300011120      *     RRN1       - Relative record number for subfile
017400011120      *     SizeList   - Size of list
017500011120      *     ReturnSize - Size of list returned by sort API
017600011120      **********************************************************************
017700011120     D NotUsed         S             16A
017800011120     D ReturnSize      S              9B 0
018000011120     D SizeList        S              9B 0
018100011120     D RrnLast         S              5i 0
018200011120     D WrkSort         S              1
018202011120     D VDESS           S                   LIKE(VDES)
018203011120     D rrn1            S              5i 0
018206011121     D len             S              2  0
018400011120      **********************************************************************
018500011120      * Data Structures
018700011120      *     SflRcd     - The entire subfile record to pass to the sort
018800011120      *     QLGSCB     - The sort request block for the QLGSORT API
018900011120      *     QLGSCB00   - The sort request block for the QLGSRTIO API
019000011120      *     QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB
019100011120      *     QUSEC      - Error structure for the QLGSORT API
019200011120      **********************************************************************
019300011120     D SflRcd          DS
019400011120     D  S01OPZ
019401011120     D  S01Cod
019600011120     D  S01Des
019601011218     D  S01tga
019602011218     D  S01tgn
019603011218     D  hnrf
019800011120     D  Selected                      1A
019900011120
020000011120      /COPY QSYSINC/QRPGLESRC,QLGSORT
020100011120     D QLGKL                         16    DIM(MaxKey)
020200011120     D  QLGSP00                       9B 0 OVERLAY(QLGKL:00001)
020300011120     D  QLGSS00                       9B 0 OVERLAY(QLGKL:00005)
020400011120     D  QLGDT00                       9B 0 OVERLAY(QLGKL:00009)
020500011120     D  QLGSO00                       9B 0 OVERLAY(QLGKL:00013)
020600011120
020700011120      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
020800011120      /COPY QSYSINC/QRPGLESRC,QUSEC
020900011120     c* ordina il sfl in base al parametro passato (ORDINA)
021200011120     C                   EXSR      Initialize
021201011121     c                   if        rrn1 = 0
021202011121     c                   seton                                        41
021203011121     c                   end
021600011120     c*
022000011120     C                   DO        *HIVAL
022001011218     c*Descrizione della ricerca
022002011218     c                   if        wrksort = ' '
022003011218     c                   eval      destps = 'Descrizione'
022004011218     c                   else
022005011218     c                   eval      destps = 'Codice'
022006011218     c                   end
022007011121     c*
022008011121     C                   WRITE     TOP
022009011121     C                   WRITE     BOT
022200011120     C                   EXFMT     CTL01
022201011126     c                   if        *inkl
022202011126     c                   move      'L'           cmd
022300011126     C                   LEAVE
022301011126     c                   end
022302011121     c                   if        rrn1 = 0
022303011121     c                   iter
022304011121     c                   end
022701011121     C                   SELECT
022702011121     c*Rivisualizza
022703011121     C                   WHEN      *INKE
022704011121     C                   EXSR      Initialize
022705011121     c*Cambio ordinamento
022706011121     C                   WHEN      *INKK
022707011121     C                   EXSR      Sort
022708011121     C                   ENDSL
022709011121     c*Controlli
022710011120     c                   exsr      ctr
022711011121     c*effettuata una scelta
022712011218     c                   if        codaut <> ' '
022713011126     c                   clear                   cmd
022714011120     c                   leave
022715011120     c                   end
023100011120     C                   ENDDO
023300011120     C                   SETON                                        LR
023600011120      **********************************************************************
023900011120     C     Initialize    BEGSR
024000011121     c*
024200011120     C                   SETON                                        40
024300011120     C                   WRITE     CTL01
024301011120     C                   SETOFF                                       40
024600011120     C                   CLEAR                   RRN1
025000011218     c*Carico il subfile in ordine di codice autista
025100011218     C     codaut        SETLL     mzame04l
025200011120     C                   DO        *HIVAL
025300011218     C                   READ      mzame04l
025600011218     C                   IF        %eof(mzame04l)
025601011120     C                   LEAVE
025602011120     C                   END
025603011218     c                   if        codaut <> *blanks
025604011218     c                             and amecau <> codaut
025605011218     c                   leave
025606011218     c                   end
025800011218     C                   MOVEL(P)  amecau        S01COD
025801011218     C                   MOVEL(P)  amecau        TBLKEY
025802011218     c     ktab          chain     tabel00f
025803011218     c                   if        %found(tabel00f)
025900011120     C                   MOVEL(P)  TBLUNI        S01DES
025901011218     C                   MOVEL     amenrf        hnrf
025902011218     C                   MOVEL     ametga        S01tga
025903011218     C                   MOVEL     ametgn        S01tgn
026200011120     C                   ADD       1             RRN1
026300011120     C                   WRITE     SFL01
026400011120     C                   ENDIF
026600011120     C                   ENDDO
026601011219     C                   CLEAR                   CODAUT
027000011120     C                   EVAL      RrnLast = Rrn1
027001011120     C                   EVAL      rcdnbr  = 1
027002011120     c* se l'ordinamento richiesto in entrata � per descrizione sorto il sfl
027700011120     C                   EVAL      WrkSort = ordina
027701011219     c                   exsr      sort
028000011120     C                   ENDSR
028100011120
028200011120      **********************************************************************
028300011120      * Subroutine - Sort
028400011120      *   This subroutine sorts the subfile records.
028500011120     C     Sort          BEGSR
028501011121     C                   CLEAR                   vdes
028502011121     C                   CLEAR                   vdess
028503011121     C                   EVAL      rcdnbr  = 1
028600011120
028700011120      * Initialize the key fields to sort on. There is one extra field not in
028800011120      * the subfile -- Selected -- that is added to the record. This field
028900011120      * is used to place selected records in front of nonselected records.
029000011120
029100011120     C                   CLEAR                   QLGSCB
029200011120     C                   CLEAR                   QLGSCB00
029300011120
029400011120     ** Gestione della scelta ordinamento.
029500011120     C                   SELECT
029600011120     ** Ordinamento per descrizione.
029700011120     C                   WHEN      WrkSort = Descrizione
029900011120     ** 1 campo chiave.
030000011120     C                   EVAL      QLGNBRK    = 1
030200011120     ** La descrizione �
030300011121     ** a posizone 10, 25 byte, char, ascending.
030400011218     C                   EVAL      QLGSP      = 4
030500011120     C                   EVAL      QLGSS      = %SIZE(S01Des)
030600011120     C                   EVAL      QLGDT      = Carattere
030700011120     C                   EVAL      QLGSO      = Ascendente
030800011120     C                   EVAL      QLGKL(1)   = QLGSKL
031400011120     ** Al prossimo F11 ordino per CODICE
031500011120     C                   EVAL      WrkSort = ' '
031700011120     ** Ordinamento per Codice
031800011120     C                   WHEN      WrkSort = ' '
032100011120     C                   EVAL      QLGNBRK    = 1
032300011120     ** Il codice �
032400011120     ** a posizione 2, 7 byte, char, ascending
032500011120     C                   EVAL      QLGSP      = 2
032600011120     C                   EVAL      QLGSS      = %SIZE(S01cod)
032700011120     C                   EVAL      QLGDT      = Carattere
032800011120     C                   EVAL      QLGSO      = Ascendente
032900011120     C                   EVAL      QLGKL(1)   = QLGSKL
034300011120     ** Al prossimo F11 ordino per descrizione.
034400011120     C                   EVAL      WrkSort = Descrizione
034600011120     C                   ENDSL
034800011120      * Load other sort parameters.
034900011120     C                   EVAL      QLGLB     = 80 + 16 * MaxKey
035000011120     C                   EVAL      QLGRL     = %SIZE(SflRcd) - 1
035100011120     C                   EVAL      QLGRT     = 8
035200011120     C                   EVAL      QLGOKL    = 80
035300011120     C                   EVAL      QLGLKE    = 16
035400011120     C                   EVAL      QLGLSS    = 290
035600011120      * Initialize Sort I/O API fields.
035700011120     C                   EVAL      QLGRL00  = QLGRL
035800011120     C                   EVAL      QLGRC00  = 1
035900011120     C                   CLEAR                   QUSEI
036000011120     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
036200011120      * First step - Initialize the sort routine.
036300011120     C                   CALL      'QLGSORT'
036400011120     C                   PARM                    QLGSCB
036500011120     C                   PARM                    NotUsed
036600011120     C                   PARM                    NotUsed
036700011120     C                   PARM                    SizeList
036800011120     C                   PARM                    ReturnSize
036900011120     C                   PARM                    QUSEC
037100011120      * Next step - Write records to I/O routine.
037200011120     C                   EVAL      QLGRT00 = Put
037400011121     C                   DO        RrnLast       x
037600011121     C     x             CHAIN     SFL01
037800011120     ** Solo le righe con Selected = 'Y' sono riordinate,
037900011120     ** quindi per fare un ordinamento di tutte le righe
038000011120     ** metto 'Y' sempre.
038100011120     C                   EVAL      Selected = 'Y'
038300011120     C                   CLEAR                   QUSEI
038400011120     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
038500011120     C                   CALL      'QLGSRTIO'
038600011120     C                   PARM                    QLGSCB00
038700011120     C                   PARM                    SflRcd
038800011120     C                   PARM                    NotUsed
038900011120     C                   PARM                    SizeList
039000011120     C                   PARM                    NotUsed
039100011120     C                   PARM                    QUSEC
039300011120     C                   ENDDO
039500011120      * Next step - Signal end of input, clear subfile for reload.
039600011120     C                   EVAL      QLGRT00 = EndPut
039700011120     C                   CLEAR                   QUSEI
039800011120     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
039900011120
040000011120     C                   CALL      'QLGSRTIO'
040100011120     C                   PARM                    QLGSCB00
040200011120     C                   PARM                    SflRcd
040300011120     C                   PARM                    NotUsed
040400011120     C                   PARM                    SizeList
040500011120     C                   PARM                    NotUsed
040600011120     C                   PARM                    QUSEC
040700011120     c                   seton                                        40
041000011120     C                   WRITE     Ctl01
041100011120     C                   setoff                                       40
041300011120      * Final step - Write the records back to the subfile.
041400011120     C                   EVAL      QLGRT00 = Get
041600011121     C                   DO        RrnLast       x
041700011120     C                   CLEAR                   QUSEI
041800011120     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
041900011120     C                   CALL      'QLGSRTIO'
042000011120     C                   PARM                    QLGSCB00
042100011120     C                   PARM                    NotUsed
042200011120     C                   PARM                    SflRcd
042300011120     C                   PARM                    QLGRL00
042400011120     C                   PARM                    NotUsed
042500011120     C                   PARM                    QUSEC
042501011121     c                   z-add     x             rrn1
042600011120     C                   WRITE     Sfl01
042700011120     C                   ENDDO
043100011120     C                   ENDSR
043101011120     c**********************************************************************
043300011120     ** Controlli Sfl
043301011120     c**********************************************************************
043400011120     C     ctr           BEGSR
043414011120     C*
043415011121     c                   setoff                                       41
043416011120     c                   do        *hival        X                 4 0
043417011120     c     X             CHAIN     sfl01                              01
043418011120     C   01              LEAVE
043419011120     c                   if        s01opz <> ' '
043420011218     c                   movel     s01cod        codaut
043423011218     c                   move      hnrf          codnrf
043425011120     c                   leave
043426011120     c                   end
043427011120     C* CONTROLLO SE EFFETTUATA RICERCA
043431011121     C                   IF        VDES <> ' ' AND VDES <> VDESS
043432011121     c                   seton                                        41
043434011121     C                   if        (wrksort = ' ' and VDES <= S01DES) or
043435011121     C                             (wrksort = '1' and VDES <= S01cod)
043436011121     c                   setoff                                       41
043437011120     C                   Z-ADD     X             rcdnbr
043438011121     C                   MOVEL     VDES          VDESS
043440011120     C                   LEAVE
043441011120     C                   END
043442011121     C                   END
043444011120     C                   ENDDO
043445011121     C  n41              clear                   VDES
043446011120     C*
043447011120     C                   ENDSR
043448011120     c**********************************************************************
043449011120     ** Operazioni iniziali.
043450011120     c**********************************************************************
043451011120     C     *INZSR        BEGSR
043452011120     C*
043453011120     C     *ENTRY        PLIST
043454011218     C                   PARM                    ORDINA            1
043455011218     C                   PARM                    CODAUT            2
043456011218     C                   PARM                    CODNRF            7 0
043459011218     C                   PARM                    CMD               1
043460011218     c                   clear                   CMD
043462011218     c                   clear                   CODNRF
043465011121     C                   EVAL      TblKut = 1
043466011218     C                   EVAL      Tblcod = 'XN'
043467011120     C*
043600011120     C     KTAB          KLIST
043700011120     C                   KFLD                    TblKut
043800011218     C                   KFLD                    tblcod
043801011218     C                   KFLD                    TBLKEY
043918011121     c*
044100011120     C                   ENDSR
