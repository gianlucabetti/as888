000100950718     H*------------------------------------------------------------------------*
000200011120     H* RICERCA E SCELTA TABELLA CON SELEZIONE MULTIPLA                        *
000300011120     H* PERMETTE LA RICERCA E LA SELEZIONE PER CHIAVE TABELLA O PER         O  *
000400011120     H* DESCRIZIONE                                                            *
000401011120     H*                                                                        *
000402011120     h* PARAMETRI:
000404151102     h* ordinamento     1  I  (F=filiale S=Sede " "=descrizione)
000406151102     h* chiave tabella  4  O
000600950718     H*------------------------------------------------------------------------*
000700950718     H DECEDIT('0,') DATEDIT(*DMY.)
000800950718     F*------------------------------------------------------------------------*
000900950718     F* DATA BASE
001000950718     F*------------------------------------------------------------------------*
001100151102     FTRtbtced  CF   E             WORKSTN
001200011120     F                                     SFILE(SFL01:RRN1)
001800151102     FTntbe01l  IF   E           K DISK
001801151102     D dtce          e DS
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
018204011121     D com45           S             45    inz
018205011121     D blanks          S             45    inz
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
022001011121     c*Descrizione della ricerca
022002011120     c                   if        wrksort = ' '
022003011120     c                   eval      destps = 'Descrizione'
022004011120     c                   else
022005011120     c                   eval      destps = 'Codice'
022006011120     c                   end
022007011121     c*
022008011121     C                   WRITE     TOP
022009011121     C                   WRITE     BOT
022200011120     C                   EXFMT     CTL01
022302011121     c                   if        rrn1 = 0
022303011121     c                   iter
022304011121     c                   end
022701011121     C                   SELECT
022702151102     c*Rivisualizza
022703151102     C                   WHEN      *INKl
022704151102     C                   leave
022705011121     c*Rivisualizza
022706011121     C                   WHEN      *INKE
022707011121     C                   EXSR      Initialize
022708011121     c*Cambio ordinamento
022709151102     C*                  WHEN      *INKK
022710151102     C*                  EXSR      Sort
022711011121     C                   ENDSL
022712011121     c*Controlli
022713011120     c                   exsr      ctr
022714011121     c*effettuata una scelta
022715151102     c                   if        keytabo <> *blanks
022716151102     c                   if        keytabo = '********'
022717151102     c                   clear                   keytabo
022718020218     c                   end
022719011120     c                   leave
022720011120     c                   end
023100011120     C                   ENDDO
023300011120     C                   SETON                                        LR
023600011120      **********************************************************************
023900011120     C     Initialize    BEGSR
024000011121     c*
024200011120     C                   SETON                                        40
024300011120     C                   WRITE     CTL01
024301011120     C                   SETOFF                                       40
024600011120     C                   CLEAR                   RRN1
025000011121     c*Carico il subfile in ordine di codice
025100151102     C     'TCE'         SETLL     Tntbe01l
025200011120     C                   DO        *HIVAL
025300151102     C     'TCE'         READE     Tntbe01l
025600151102     C                   IF        %eof(Tntbe01l)
025601011120     C                   LEAVE
025602011120     C                   END
025603151102     C                   IF        tbeatb <> *blank
025604151102     C                   iter
025605151102     C                   END
025800151102     C                   MOVEL(P)  TBeke1        S01COD
025900151102     C                   MOVEL(P)  TbeUNI        dtce
025901151102     C                   MOVEL(P)  §tcedes       S01DES
025902151102     C                   IF        Tipo = 'F' and §tceute ='S' or
025903151102     C                             Tipo = 'S' and §tceute ='F'
025904151102     C                   iter
025905151102     C                   END
026200011120     C                   ADD       1             RRN1
026300011120     C                   WRITE     SFL01
026600011120     C                   ENDDO
027000011120     C                   EVAL      RrnLast = Rrn1
027001011120     C                   EVAL      rcdnbr  = 1
027002011120     c* se l'ordinamento richiesto in entrata è per descrizione sorto il sfl
027700151102     C*                  EVAL      WrkSort = ordina
027701151102     c*                  exsr      sort
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
030200011120     ** La descrizione è
030300011121     ** a posizone 10, 25 byte, char, ascending.
030400011121     C                   EVAL      QLGSP      = 10
030500011120     C                   EVAL      QLGSS      = %SIZE(S01Des)
030600011120     C                   EVAL      QLGDT      = Carattere
030700011120     C                   EVAL      QLGSO      = Ascendente
030800011120     C                   EVAL      QLGKL(1)   = QLGSKL
031400011120     ** Al prossimo F11 ordino per CODICE
031500011120     C                   EVAL      WrkSort = ' '
031700011120     ** Ordinamento per Codice
031800011120     C                   WHEN      WrkSort = ' '
032100011120     C                   EVAL      QLGNBRK    = 1
032300011120     ** Il codice è
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
043419020218     c                   if        s01opz <> *blanks
043420151102     c                   movel     s01cod        keytabo
043421151102     c                   if        keytabo = *blanks
043422151102     c                   movel     *all'*'       keytabo
043423020218     c                   end
043424011120     c                   leave
043425011120     c                   end
043426011120     C* CONTROLLO SE EFFETTUATA RICERCA
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
043518011120     c**********************************************************************
043519011120     ** Operazioni iniziali.
043520011120     c**********************************************************************
043521011120     C     *INZSR        BEGSR
043522011120     C*
043523011120     C     *ENTRY        PLIST
043524151102     C                   PARM                    tipo              1
043526151102     C                   PARM                    keytabo           3
043529151102     c                   clear                   keytabo
043918011121     c*
044100011120     C                   ENDSR
