000100020121     H DECEDIT('0,') DATEDIT(*DMY.) option(*nodebugio)
000200020121      *--------------------------------------------------------------------------------------------*
000300020121      * Interrogazione tabella Stati 'STA'                                                         *
000400020121      * Permette la ricerca per descrizione                                                        *
000500020121      *--------------------------------------------------------------------------------------------*
000600020121
000700020121      * PARAMETRI:
000800020121      * Nazione         3  I
000900020121      * Stato           3  I
001000020121      * Ordinamento     1  I  (*blanks = codice  1=descizione)
001100020121
001200020121      *--------------------------------------------------------------------------------------------*
001300020121
001400020121     ftntb20d   cf   e             workstn
001500020121     f                                     sfile(tb20s01:nrr)
001600020121     ftntbe01l  uf a e           k disk
001700011120      **********************************************************************
001800011120      * Constants
001900011120      *
002000011120      *     MaxKey - Maximum number of key fields allowed by this program
002100011120      *
002200011120      **********************************************************************
002300011120     D MaxKey          C                    1
002400011120     D Descrizione     C                   '1'
002500011120     D Ascendente      C                   1
002600011120     D Carattere       C                   6
002700011120     D Put             C                   1
002800011120     D EndPut          C                   2
002900011120     D Get             C                   3
003000011120      **********************************************************************
003100011120      * Standalone fields
003200011120      *
003300020121      *     Nrr        - Relative record number for subfile
003400011120      *     SizeList   - Size of list
003500011120      *     ReturnSize - Size of list returned by sort API
003600011120      **********************************************************************
003700011120     D NotUsed         S             16A
003800011120     D ReturnSize      S              9B 0
003900011120     D SizeList        S              9B 0
004000011120     D RrnLast         S              5i 0
004100011120     D WrkSort         S              1
004200020121     d vdes            s                   like(v1ddes)
004300020121     d nrr             s              5i 0
004400011120      **********************************************************************
004500011120      * Data Structures
004600011120      *     SflRcd     - The entire subfile record to pass to the sort
004700011120      *     QLGSCB     - The sort request block for the QLGSORT API
004800011120      *     QLGSCB00   - The sort request block for the QLGSRTIO API
004900011120      *     QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB
005000011120      *     QUSEC      - Error structure for the QLGSORT API
005100011120      **********************************************************************
005200011120     D SflRcd          DS
005300020121     d  vs1sce
005400020121     d  vs1nzd
005500020121     d  vs1prv
005600020121     d  vs1des
005700011120     D  Selected                      1A
005800020121
005900020121     d kcod            s                   like(tbecod)
006000020121     d klin            s                   like(tbelin)
006100020121     d ksif            s                   like(tbesif)
006200020121     d kke1            s                   like(tbeke1)
006300020121     d sav_nzd         s                   like(v1dnzd)
006400020121
006500020121     d param01         ds
006600020121     d  p01nzd                        3
006700020121     d  p01prv                        2
006800020121     d  p01ord                        1
006900020121     d kpjba         e ds
007000020121
007100020121     d v1tipr          c                   const('Interrogazione tabella STATI')
007200020121
007300011120
007400011120      /COPY QSYSINC/QRPGLESRC,QLGSORT
007500011120     D QLGKL                         16    DIM(MaxKey)
007600011120     D  QLGSP00                       9B 0 OVERLAY(QLGKL:00001)
007700011120     D  QLGSS00                       9B 0 OVERLAY(QLGKL:00005)
007800011120     D  QLGDT00                       9B 0 OVERLAY(QLGKL:00009)
007900011120     D  QLGSO00                       9B 0 OVERLAY(QLGKL:00013)
008000011120
008100011120      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
008200011120      /COPY QSYSINC/QRPGLESRC,QUSEC
008300020121
008400020121      * Ordina il sfl in base al parametro passato
008500020121     c                   exsr      Initialize
008600020121     c                   if        nrr = 0
008700011121     c                   seton                                        41
008800020121     c                   endif
008900020121
009000020121     c                   do        *hival
009100020121
009200020121     c                   write     tb20d01
009300020121     c                   write     tb20z01
009400020121     c                   exfmt     tb20c01
009500020121
009600011126     c                   if        *inkl
009700020121     c                   leave
009800020121     c                   endif
009900020121
010000020205     c**!!!              if        nrr = 0
010100020205     c**!!!              iter
010200020205     c**!!!              endif
010300020121
010400020121     c                   select
010500020121
010600020121      * Cambio nazione
010700020121     c                   when      v1dnzd <> sav_nzd
010800020123     c                   exsr      Initialize
010900020121
011000020121      * Rivisualizza
011100020121     c                   when      *inke
011200020121     c                   exsr      Initialize
011300020121
011400020121      * Cambio ordinamento
011500020121     c                   when      *inkk
011600020121     c                   exsr      Sort
011700020121
011800020121     c                   endsl
011900020205
012000020205     c                   if        nrr = 0
012100020205     c                   seton                                        41
012200020205     c                   iter
012300020205     c                   endif
012400020121
012500020121      * Controlli
012600020123     c                   exsr      Ctr01
012700020121
012800020121      * Effettuata una scelta
012900020121     c                   if        p01prv <> ' '
013000011120     c                   leave
013100020121     c                   endif
013200020121
013300020121     c                   enddo
013400020121
013500020121     c                   movel(p)  v1dnzd        p01nzd
013600020121     c                   movel(p)  wrksort       p01ord
013700020121     c                   movel     param01       kpjbu
013800020121
013900020121     c                   eval      *inlr = *on
014000020121
014100020121      *------------------------------------------------------------------*
014200020121     c     Initialize    begsr
014300020121
014400020121     c                   eval      *in40 = *on
014500020121     c                   write     tb20c01
014600020121     c                   eval      *in40 = *off
014700020121     c                   clear                   nrr
014800020121
014900020121     c                   if        v1dnzd = *blanks
015000020121     c                   eval      v1dnzd = p01nzd
015100020121     c                   endif
015200020207     c*                  if        v1dnzd <> *blanks
015300020207     c                   eval      sav_nzd = v1dnzd
015400020207     c*                  endif
015500020121     c                   eval      kke1 = sav_nzd
015600020121
015700020121      * Carico il subfile in ordine di codice
015800020121     c     ktntbe        setll     tntbe01l
015900020121     c                   do        *hival
016000020121     c                   if        kke1 <> *blanks
016100020121     c     ktntbe        reade     tntbe01l
016200020121     c                   else
016300020121     c     kcod          reade     tntbe01l
016400020121     c                   endif
016500020121
016600020121      * Fine file
016700020121     c                   if        %eof(tntbe01l)
016800020121     c                   leave
016900020121     c                   endif
017000020123
017100020123      * Escludo annullati
017200020123     c                   if        tbeatb <> *blanks
017300020123     c                   iter
017400020123     c                   endif
017500020123
017600020123      * Escludo se non del mio S.I.
017700020123     c                   if        tbesif <> *blanks and
017800020123     c                             tbesif <> knsif
017900020123     c                   iter
018000020123     c                   endif
018100020121
018200020121     c                   movel(p)  tbeke1        vs1nzd
018300020121     c                   movel(p)  tbeke2        vs1prv
018400020121     c                   movel(p)  tbeuni        vs1des
018500020121     c                   add       1             nrr
018600020121     c                   write     tb20s01
018700020121     c                   enddo
018800020121
018900020121     c                   eval      RrnLast = nrr
019000020121     c                   eval      rcdnbr  = 1
019100020121
019200020121      * Se l'ordinamento richiesto in entrata � per descrizione sorto il sfl
019300020121     c                   eval      WrkSort = p01ord
019400011219     c                   exsr      sort
019500020121
019600020121     c                   endsr
019700011120
019800011120      **********************************************************************
019900011120      * Subroutine - Sort
020000011120      *   This subroutine sorts the subfile records.
020100011120     C     Sort          BEGSR
020200020121     C                   CLEAR                   vs1des
020300020121     C                   CLEAR                   vdes
020400011121     C                   EVAL      rcdnbr  = 1
020500011120
020600011120      * Initialize the key fields to sort on. There is one extra field not in
020700011120      * the subfile -- Selected -- that is added to the record. This field
020800011120      * is used to place selected records in front of nonselected records.
020900011120
021000011120     C                   CLEAR                   QLGSCB
021100011120     C                   CLEAR                   QLGSCB00
021200011120
021300011120     ** Gestione della scelta ordinamento.
021400011120     C                   SELECT
021500011120     ** Ordinamento per descrizione.
021600011120     C                   WHEN      WrkSort = Descrizione
021700011120     ** 1 campo chiave.
021800011120     C                   EVAL      QLGNBRK    = 1
021900011120     ** La descrizione �
022000020121     ** a posizone 7, 30 byte, char, ascending.
022100020121     C                   EVAL      QLGSP      = 7
022200020121     C                   EVAL      QLGSS      = %SIZE(vs1des)
022300011120     C                   EVAL      QLGDT      = Carattere
022400011120     C                   EVAL      QLGSO      = Ascendente
022500011120     C                   EVAL      QLGKL(1)   = QLGSKL
022600011120     ** Al prossimo F11 ordino per CODICE
022700011120     C                   EVAL      WrkSort = ' '
022800011120     ** Ordinamento per Codice
022900011120     C                   WHEN      WrkSort = ' '
023000011120     C                   EVAL      QLGNBRK    = 1
023100011120     ** Il codice �
023200020121     ** a posizione 5, 2 byte, char, ascending
023300020121     C                   EVAL      QLGSP      = 5
023400020121     C                   EVAL      QLGSS      = %SIZE(vs1prv)
023500011120     C                   EVAL      QLGDT      = Carattere
023600011120     C                   EVAL      QLGSO      = Ascendente
023700011120     C                   EVAL      QLGKL(1)   = QLGSKL
023800011120     ** Al prossimo F11 ordino per descrizione.
023900011120     C                   EVAL      WrkSort = Descrizione
024000011120     C                   ENDSL
024100011120      * Load other sort parameters.
024200011120     C                   EVAL      QLGLB     = 80 + 16 * MaxKey
024300011120     C                   EVAL      QLGRL     = %SIZE(SflRcd) - 1
024400011120     C                   EVAL      QLGRT     = 8
024500011120     C                   EVAL      QLGOKL    = 80
024600011120     C                   EVAL      QLGLKE    = 16
024700011120     C                   EVAL      QLGLSS    = 290
024800011120      * Initialize Sort I/O API fields.
024900011120     C                   EVAL      QLGRL00  = QLGRL
025000011120     C                   EVAL      QLGRC00  = 1
025100011120     C                   CLEAR                   QUSEI
025200011120     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
025300011120      * First step - Initialize the sort routine.
025400011120     C                   CALL      'QLGSORT'
025500011120     C                   PARM                    QLGSCB
025600011120     C                   PARM                    NotUsed
025700011120     C                   PARM                    NotUsed
025800011120     C                   PARM                    SizeList
025900011120     C                   PARM                    ReturnSize
026000011120     C                   PARM                    QUSEC
026100011120      * Next step - Write records to I/O routine.
026200011120     C                   EVAL      QLGRT00 = Put
026300011121     C                   DO        RrnLast       x
026400020121     C     x             CHAIN     tb20s01
026500011120     ** Solo le righe con Selected = 'Y' sono riordinate,
026600011120     ** quindi per fare un ordinamento di tutte le righe
026700011120     ** metto 'Y' sempre.
026800011120     C                   EVAL      Selected = 'Y'
026900011120     C                   CLEAR                   QUSEI
027000011120     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
027100011120     C                   CALL      'QLGSRTIO'
027200011120     C                   PARM                    QLGSCB00
027300011120     C                   PARM                    SflRcd
027400011120     C                   PARM                    NotUsed
027500011120     C                   PARM                    SizeList
027600011120     C                   PARM                    NotUsed
027700011120     C                   PARM                    QUSEC
027800011120     C                   ENDDO
027900011120      * Next step - Signal end of input, clear subfile for reload.
028000011120     C                   EVAL      QLGRT00 = EndPut
028100011120     C                   CLEAR                   QUSEI
028200011120     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
028300011120
028400011120     C                   CALL      'QLGSRTIO'
028500011120     C                   PARM                    QLGSCB00
028600011120     C                   PARM                    SflRcd
028700011120     C                   PARM                    NotUsed
028800011120     C                   PARM                    SizeList
028900011120     C                   PARM                    NotUsed
029000011120     C                   PARM                    QUSEC
029100020121     C                   seton                                        40
029200020121     C                   WRITE     tb20c01
029300011120     C                   setoff                                       40
029400011120      * Final step - Write the records back to the subfile.
029500011120     C                   EVAL      QLGRT00 = Get
029600011121     C                   DO        RrnLast       x
029700011120     C                   CLEAR                   QUSEI
029800011120     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
029900011120     C                   CALL      'QLGSRTIO'
030000011120     C                   PARM                    QLGSCB00
030100011120     C                   PARM                    NotUsed
030200011120     C                   PARM                    SflRcd
030300011120     C                   PARM                    QLGRL00
030400011120     C                   PARM                    NotUsed
030500011120     C                   PARM                    QUSEC
030600020121     C                   z-add     x             nrr
030700020121     C                   WRITE     tb20s01
030800011120     C                   ENDDO
030900011120     C                   ENDSR
031000020121
031100020121      *------------------------------------------------------------------*
031200020121     c     ctr01         begsr
031300020121
031400020121     c                   eval      *in41 = *off
031500011120     c                   do        *hival        X                 4 0
031600020121     c     X             chain     tb20s01                            01
031700020121     c   01              leave
031800020121
031900020121     c                   if        vs1sce <> ' '
032000020121     c                   movel(p)  vs1prv        p01prv
032100011120     c                   leave
032200011120     c                   end
032300020121
032400020121      * Controllo se effettuata ricerca
032500020121     c                   if        v1ddes <> ' ' and v1ddes <> vdes
032600020121     c                   eval      *in41 = *on
032700020121     c                   if        (wrksort = ' ' and v1ddes <= vs1des) or
032800020121     c                             (wrksort = '1' and v1ddes <= vs1prv)
032900020121     c                   eval      *in41 = *off
033000020121     c                   z-add     x             rcdnbr
033100020121     c                   movel     v1ddes        vdes
033200020121     c                   leave
033300020121     c                   endif
033400020121     c                   endif
033500020121     c                   enddo
033600020121     c  n41              clear                   v1ddes
033700020121
033800020121     c                   endsr
033900020121
034000020121      *------------------------------------------------------------------*
034100020121      *                   ROUTINE INIZIALE
034200020121      *------------------------------------------------------------------*
034300020121
034400020121     c     *inzsr        begsr
034500020121
034600020121     c     *entry        plist
034700020121     c                   parm                    kpjba
034800020121
034900020121     c                   movel     kpjbu         param01
035000020121
035100020121     c     ktntbe        klist
035200020121     c                   kfld                    kcod
035300020121     c                   kfld                    kke1
035400020121
035500020121     c                   eval      kcod = 'STA'
035600020121
035700020121     c                   endsr
