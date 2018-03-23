000100020121     H DECEDIT('0,') DATEDIT(*DMY.) option(*nodebugio)
000200020121      *--------------------------------------------------------------------------------------------*
000300021114      * Interrogazione tabella Azioni Schedultae 'SCH'                                             *
000400021114      * Permette la ricerca per frequenza/priorità                                                 *
000500020121      *--------------------------------------------------------------------------------------------*
000600020121
000700020121      * PARAMETRI:
000800021114      * Azione          4  I
000900051027      * Ordinamento     1  I  (*blanks = Codice;
001000051027      *                        1 = Esecuguire in/Frequenza/Priorità/Sottomissione/Codice;
001100051027      *                        2 = Frequenza/Priorità)
001200021114      * Sistema inf.   10  I
001300021114      * Chiave tabella  4  O
001400021114      * Codice ritorno  1  O  (comando premuto)
001500020121
001600020121      *--------------------------------------------------------------------------------------------*
001700020121
001800021114     ftntb281d  cf   e             workstn
001900021114     f                                     sfile(tb281s01:nrr)
002000020121     ftntbe01l  uf a e           k disk
002100011120      **********************************************************************
002200011120      * Constants
002300011120      *
002400011120      *     MaxKey - Maximum number of key fields allowed by this program
002500011120      *
002600011120      **********************************************************************
002700051027     D MaxKey          C                    5
002800011120     D Ascendente      C                   1
002900011120     D Carattere       C                   6
003000011120     D Put             C                   1
003100011120     D EndPut          C                   2
003200011120     D Get             C                   3
003300011120      **********************************************************************
003400011120      * Standalone fields
003500011120      *
003600020121      *     Nrr        - Relative record number for subfile
003700011120      *     SizeList   - Size of list
003800011120      *     ReturnSize - Size of list returned by sort API
003900011120      **********************************************************************
004000011120     D NotUsed         S             16A
004100011120     D ReturnSize      S              9B 0
004200011120     D SizeList        S              9B 0
004300011120     D RrnLast         S              5i 0
004400051027     d Nrr             s              5i 0
004500051027     D WrkSort         S              1    inz('1')
004600011120      **********************************************************************
004700011120      * Data Structures
004800011120      *     SflRcd     - The entire subfile record to pass to the sort
004900011120      *     QLGSCB     - The sort request block for the QLGSORT API
005000011120      *     QLGSCB00   - The sort request block for the QLGSRTIO API
005100011120      *     QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB
005200011120      *     QUSEC      - Error structure for the QLGSORT API
005300011120      **********************************************************************
005400011120     D SflRcd          DS
005500020121     d  vs1sce
005600021114     d  vs1sch
005700020121     d  vs1des
005800021114     d  vs1ese
005900021114     d  vs1fre
006000021114     d  vs1pri
006100021114     d  vs1tps
006200021114     d  vs1job
006300021114     d  vs1ute
006400021114     d  vs1or1
006500021114     d  vs1or2
006600021114     d  vs1or3
006700051027     d  vs1or4
006800051027     d  vs1or5
006900011120     D  Selected                      1A
007000021114
007100051027     d KPJBA         e ds
007200021114     d param01         ds
007300021114     d  p01sch                        4
007400021114     d  p01ord                        1
007500021114     d  p01sif                       10
007600021114     d  p01ke1                        4
007700021114     d  p01rit                        1
007800021114
007900051027      * - Parametri x Controllo profilo utenti
008000051027     d TIBS34DS      e ds
008100051027      * - Ds di riferimento al file esterno AZUTE00F
008200051027     d AZUTEds       e ds                  extname(AZUTE00F)
008300051027      * - Ds per dati organigramma
008400051027     d DDatiUte      e ds
008500051027
008600051027     d dSCH          e ds                  inz
008700020121
008800011120      /COPY QSYSINC/QRPGLESRC,QLGSORT
008900011120     D QLGKL                         16    DIM(MaxKey)
009000011120     D  QLGSP00                       9B 0 OVERLAY(QLGKL:00001)
009100011120     D  QLGSS00                       9B 0 OVERLAY(QLGKL:00005)
009200011120     D  QLGDT00                       9B 0 OVERLAY(QLGKL:00009)
009300011120     D  QLGSO00                       9B 0 OVERLAY(QLGKL:00013)
009400011120
009500011120      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
009600011120      /COPY QSYSINC/QRPGLESRC,QUSEC
009700051027
009800051027     d kcod            s                   like(tbecod)  inz
009900051027     d klin            s                   like(tbelin)  inz
010000051027     d ksif            s                   like(tbesif)  inz
010100051027     d kke1            s                   like(tbeke1)  inz
010200051027      *
010300051027     d Scod1           s                   like(Vcod1)   inz
010400051027     d Sesec           s                   like(Vesec)   inz
010500051027     d Sfreq           s                   like(Vfreq)   inz
010600051027     d Sprio           s                   like(Vprio)   inz
010700051027     d Ssott           s                   like(Vsott)   inz
010800051027     d Scod2           s                   like(Vcod2)   inz
010900051027
011000051027     d XX              s              4  0 inz
011100020121
011200020121      * Ordina il sfl in base al parametro passato
011300020121     c                   exsr      Initialize
011400020121     c                   if        nrr = 0
011500011121     c                   seton                                        41
011600020121     c                   endif
011700020121
011800020121     c                   do        *hival
011900021114      *Descrizione della ricerca
012000051027     c                   select
012100051027     c                   when      WrkSort = '0'
012200051027     c                   eval      *in42 = *off
012300051027     c                   when      WrkSort = '1'
012400051027     c                   eval      *in42 = *on
012500051027     c                   endsl
012600020121
012700021114     c                   write     tb281t01
012800021114     c                   write     tb281z01
012900021114     c                   exfmt     tb281c01
013000021114
013100021114     c                   if        *inkc
013200021114     c                   move      'C'           p01rit
013300021114     c                   leave
013400021114     c                   endif
013500020121
013600011126     c                   if        *inkl
013700021114     c                   move      'L'           p01rit
013800020121     c                   leave
013900020121     c                   endif
014000020121
014100020121     c                   select
014200020121
014300020121      * Rivisualizza
014400020121     c                   when      *inke
014500020121     c                   exsr      Initialize
014600020121
014700051027      * F11 = Cambio ordinamento
014800020121     c                   when      *inkk
014900051027     c                   select
015000051027     c                   when      WrkSort = '0'
015100051027     c                   eval      WrkSort = '1'
015200051027     c                   when      WrkSort = '1'
015300051027     c                   eval      WrkSort = '0'
015400051027     c                   endsl
015500020121     c                   exsr      Sort
015600020121
015700020121     c                   endsl
015800020121
015900020121      * Controlli
016000020123     c                   exsr      Ctr01
016100020121
016200020121      * Effettuata una scelta
016300021114     c                   if        p01ke1 <> *Blanks
016400021114     c                   Clear                   p01rit
016500011120     c                   leave
016600020121     c                   endif
016700020121
016800020121     c                   enddo
016900020121
017000020121     c                   movel     param01       kpjbu
017100020121
017200020121     c                   eval      *inlr = *on
017300020121
017400020121      *------------------------------------------------------------------*
017500020121     c     Initialize    begsr
017600020121
017700020121     c                   eval      *in40 = *on
017800021114     c                   write     tb281c01
017900020121     c                   eval      *in40 = *off
018000020121     c                   clear                   nrr
018100020121
018200020121      * Carico il subfile in ordine di codice
018300051027     c     kcod          setll     tntbe000
018400020121     c                   do        *hival
018500051027     c     kcod          reade     tntbe000
018600020121
018700020121      * Fine file
018800020121     c                   if        %eof(tntbe01l)
018900020121     c                   leave
019000020121     c                   endif
019100020123
019200020123      * Escludo annullati
019300020123     c                   if        tbeatb <> *blanks
019400020123     c                   iter
019500020123     c                   endif
019600020123
019700021114      * Escludo se non del S.I. richiesto
019800020123     c                   if        tbesif <> *blanks and
019900021114     c                             tbesif <> p01sif
020000020123     c                   iter
020100020123     c                   endif
020200020121
020300021114     c                   movel(p)  tbeke1        vs1sch
020400021114     c                   movel(p)  tbeuni        Dsch
020500021114     c                   movel     d§schdes      vs1des
020600051027     c                   clear                   vs1ese
020700051027     c                   Select
020800051027     c                   when      d§schese = 'E'
020900021114     c                   movel     'S/Po'        vs1ese
021000051027     c                   when      d§schese = 'P'
021100021114     c                   movel     'P.O.'        vs1ese
021200051027     c                   when      d§schese = 'S'
021300021114     c                   movel     'Sede'        vs1ese
021400051027     c                   EndSl
021500051027     c                   clear                   vs1fre
021600051027     c                   Select
021700051027     c                   when      d§schFre = 'G'
021800021114     c                   movel     'Gior'        vs1fre
021900051027     c                   when      d§schFre = 'S'
022000021114     c                   movel     'Sett'        vs1fre
022100051027     c                   when      d§schFre = 'M'
022200050222     c                   movel     'Mens'        vs1fre
022300051027     c                   EndSl
022400021114     c                   move      d§schpri      vs1pri
022500051027     c                   clear                   vs1tps
022600021114     c                   If        d§schtps = 'M'
022700021114     c                   movel     'Mult'        vs1tps
022800021114     c                   EndIf
022900021114     c                   movel     d§schjob      vs1job
023000021114     c                   movel     d§schute      vs1ute
023100021114     c                   movel     d§schor1      vs1or1
023200021114     c                   movel     d§schor2      vs1or2
023300021114     c                   movel     d§schor3      vs1or3
023400021114     c                   movel     d§schor4      vs1or4
023500021114     c                   movel     d§schor5      vs1or5
023600020121     c                   add       1             nrr
023700021114     c                   write     tb281s01
023800020121     c                   enddo
023900020121
024000020121     c                   eval      RrnLast = nrr
024100020121     c                   eval      rcdnbr  = 1
024200020121
024300051027      * Se è richiesto un ordinamento (valido): sorto il sfl
024400051027     c                   if            P01ord  = '0'
024500051027     c                             or  P01ord  = '1'
024600020121     c                   eval      WrkSort = p01ord
024700051027     c                   endif
024800011219     c                   exsr      sort
024900020121
025000020121     c                   endsr
025100011120
025200011120      **********************************************************************
025300011120      * Subroutine - Sort
025400011120      *   This subroutine sorts the subfile records.
025500011120     C     Sort          BEGSR
025600051027
025700051027     c                   select
025800051027     c                   when      WrkSort = '0'
025900051027     c                   clear                   Vcod1
026000051027     c                   clear                   Scod1
026100051027     c                   when      WrkSort = '1'
026200051027     c                   clear                   Vesec
026300051027     c                   clear                   Vfreq
026400051027     c                   clear                   Vprio
026500051027     c                   clear                   Vsott
026600051027     c                   clear                   Vcod2
026700051027     c                   clear                   Sesec
026800051027     c                   clear                   Sfreq
026900051027     c                   clear                   Sprio
027000051027     c                   clear                   Ssott
027100051027     c                   clear                   Scod2
027200051027     c                   endsl
027300011121     C                   EVAL      rcdnbr  = 1
027400011120
027500011120      * Initialize the key fields to sort on. There is one extra field not in
027600011120      * the subfile -- Selected -- that is added to the record. This field
027700011120      * is used to place selected records in front of nonselected records.
027800011120
027900051027     C                   CLEAR                   QLGKL
028000051027     C                   CLEAR                   QLGSKL
028100011120     C                   CLEAR                   QLGSCB
028200011120     C                   CLEAR                   QLGSCB00
028300011120
028400011120     ** Gestione della scelta ordinamento.
028500011120     C                   SELECT
028600051027     ** Ordinamento per Esec./Freq./Priorità/Sottom./Azione
028700051027     c                   WHEN      WrkSort = '1'
028800051027     ** 5 campi chiave
028900051027     c                   EVAL      QLGNBRK    = 5
029000051027     ** 1° campo: Esecuzione (P.O./Sede/entrambi)
029100051027     **           a posizone 41,  4 byte, char, ascending.
029200051027     c                   eval      QLGSP      = 41
029300051027     c                   eval      QLGSS      = %size(VS1ese)
029400051027     c                   eval      QLGDT      = Carattere
029500051027     c                   eval      QLGSO      = Ascendente
029600051027     c                   eval      QLGKL(1)   = QLGSKL
029700051027     ** 2° campo: Frequenza
029800051027     **           a posizone 45,  4 byte, char, ascending.
029900051027     c                   eval      QLGSP      = 45
030000051027     c                   eval      QLGSS      = %size(VS1fre)
030100051027     c                   eval      QLGDT      = Carattere
030200051027     c                   eval      QLGSO      = Ascendente
030300051027     c                   eval      QLGKL(2)   = QLGSKL
030400051027     ** 3° campo: Priorità
030500051027     **           a posizone 49,  4 byte, char, ascending.
030600051027     c                   eval      QLGSP      = 49
030700051027     c                   eval      QLGSS      = %size(VS1tps)
030800051027     c                   eval      QLGDT      = Carattere
030900051027     c                   eval      QLGSO      = Ascendente
031000051027     c                   eval      QLGKL(3)   = QLGSKL
031100051027     ** 4° campo: Sottomissione
031200051027     **           a posizone 53, 10 byte, char, ascending.
031300051027     c                   eval      QLGSP      = 53
031400051027     c                   eval      QLGSS      = %size(VS1job)
031500051027     c                   eval      QLGDT      = Carattere
031600051027     c                   eval      QLGSO      = Ascendente
031700051027     c                   eval      QLGKL(4)   = QLGSKL
031800051027     ** 5° campo: Azione schedulata
031900051027     **           a posizone  2, 4 byte, char, ascending.
032000051027     c                   eval      QLGSP      = 2
032100051027     c                   eval      QLGSS      = %SIZE(VS1sch)
032200051027     c                   eval      QLGDT      = Carattere
032300051027     c                   eval      QLGSO      = Ascendente
032400051027     c                   eval      QLGKL(5)   = QLGSKL
032500021114
032600011120     ** Ordinamento per Codice
032700051027     C                   WHEN      WrkSort = '0'
032800011120     C                   EVAL      QLGNBRK    = 1
032900011120     ** Il codice è
033000021114     ** a posizione 2, 4 byte, char, ascending
033100021114     C                   EVAL      QLGSP      = 2
033200021114     C                   EVAL      QLGSS      = %SIZE(vs1sch)
033300011120     C                   EVAL      QLGDT      = Carattere
033400011120     C                   EVAL      QLGSO      = Ascendente
033500011120     C                   EVAL      QLGKL(1)   = QLGSKL
033600051027
033700011120     C                   ENDSL
033800011120      * Load other sort parameters.
033900011120     C                   EVAL      QLGLB     = 80 + 16 * MaxKey
034000011120     C                   EVAL      QLGRL     = %SIZE(SflRcd) - 1
034100011120     C                   EVAL      QLGRT     = 8
034200011120     C                   EVAL      QLGOKL    = 80
034300011120     C                   EVAL      QLGLKE    = 16
034400011120     C                   EVAL      QLGLSS    = 290
034500011120      * Initialize Sort I/O API fields.
034600011120     C                   EVAL      QLGRL00  = QLGRL
034700011120     C                   EVAL      QLGRC00  = 1
034800011120     C                   CLEAR                   QUSEI
034900011120     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
035000011120      * First step - Initialize the sort routine.
035100011120     C                   CALL      'QLGSORT'
035200011120     C                   PARM                    QLGSCB
035300011120     C                   PARM                    NotUsed
035400011120     C                   PARM                    NotUsed
035500011120     C                   PARM                    SizeList
035600011120     C                   PARM                    ReturnSize
035700011120     C                   PARM                    QUSEC
035800011120      * Next step - Write records to I/O routine.
035900011120     C                   EVAL      QLGRT00 = Put
036000051027     C                   DO        RrnLast       xx
036100051027     C     xx            CHAIN     tb281s01
036200011120     ** Solo le righe con Selected = 'Y' sono riordinate,
036300011120     ** quindi per fare un ordinamento di tutte le righe
036400011120     ** metto 'Y' sempre.
036500011120     C                   EVAL      Selected = 'Y'
036600011120     C                   CLEAR                   QUSEI
036700011120     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
036800011120     C                   CALL      'QLGSRTIO'
036900011120     C                   PARM                    QLGSCB00
037000011120     C                   PARM                    SflRcd
037100011120     C                   PARM                    NotUsed
037200011120     C                   PARM                    SizeList
037300011120     C                   PARM                    NotUsed
037400011120     C                   PARM                    QUSEC
037500011120     C                   ENDDO
037600011120      * Next step - Signal end of input, clear subfile for reload.
037700011120     C                   EVAL      QLGRT00 = EndPut
037800011120     C                   CLEAR                   QUSEI
037900011120     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
038000011120
038100011120     C                   CALL      'QLGSRTIO'
038200011120     C                   PARM                    QLGSCB00
038300011120     C                   PARM                    SflRcd
038400011120     C                   PARM                    NotUsed
038500011120     C                   PARM                    SizeList
038600011120     C                   PARM                    NotUsed
038700011120     C                   PARM                    QUSEC
038800020121     C                   seton                                        40
038900021114     C                   WRITE     tb281c01
039000011120     C                   setoff                                       40
039100011120      * Final step - Write the records back to the subfile.
039200011120     C                   EVAL      QLGRT00 = Get
039300051027     C                   DO        RrnLast       xx
039400011120     C                   CLEAR                   QUSEI
039500011120     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
039600011120     C                   CALL      'QLGSRTIO'
039700011120     C                   PARM                    QLGSCB00
039800011120     C                   PARM                    NotUsed
039900011120     C                   PARM                    SflRcd
040000011120     C                   PARM                    QLGRL00
040100011120     C                   PARM                    NotUsed
040200011120     C                   PARM                    QUSEC
040300051027     C                   z-add     xx            nrr
040400021114     C                   WRITE     tb281s01
040500011120     C                   ENDDO
040600011120     C                   ENDSR
040700020121
040800020121      *------------------------------------------------------------------*
040900051027     c     Ctr01         begsr
041000020121
041100020121     c                   eval      *in41 = *off
041200051027
041300051027do  1c                   do        *hival        xx
041400051027
041500051027     c     xx            chain     tb281s01                           01
041600020121     c   01              leave
041700020121
041800020121     c                   if        vs1sce <> ' '
041900021114     c                   movel(p)  vs1sch        p01ke1
042000011120     c                   leave
042100011120     c                   end
042200020121
042300051027      * Controllo se effettuata ricerca
042400051027if  2c                   if            (WrkSort  = '0'
042500051027     c                             and  Vcod1   <> *blanks
042600051027     c                             and  Vcod1   <> Scod1)
042700051027     c                             OR  (WrkSort  = '1'
042800051027     c                             and (Vesec   <> *blanks
042900051027     c                             and  Vesec   <> Sesec)
043000051027     c                              or (Vfreq   <> *blanks
043100051027     c                             and  Vfreq   <> Sfreq)
043200051027     c                              or (Vprio   <> *blanks
043300051027     c                             and  Vprio   <> Sprio)
043400051027     c                              or (Vsott   <> *blanks
043500051027     c                             and  Vsott   <> Ssott)
043600051027     c                              or (Vcod2   <> *blanks
043700051027     c                             and  Vcod2   <> Scod2))
043800020121     c                   eval      *in41 = *on
043900051027if  3c                   if            (WrkSort  = '0'
044000051027     c                             and  Vcod1   <= VS1sch)
044100051027     c                             OR  (WrkSort  = '1'
044200051027     c                             and  Vesec   <= VS1ese
044300051027     c                             and  Vfreq   <= VS1fre
044400051027     c                             and  Vprio   <= VS1pri
044500051027     c                             and  Vsott   <= VS1tps
044600051027     c                             and  Vcod2   <= VS1sch)
044700020121     c                   eval      *in41 = *off
044800051027     c                   z-add     xx            RcdNbr
044900051027     c                   select
045000051027     c                   when      WrkSort = '0'
045100051027     c                   movel     Vcod1         Scod1
045200051027     c                   when      WrkSort = '1'
045300051027     c                   movel     Vesec         Sesec
045400051027     c                   movel     Vfreq         Sfreq
045500051027     c                   movel     Vprio         Sprio
045600051027     c                   movel     Vsott         Ssott
045700051027     c                   movel     Vcod2         Scod2
045800051027     c                   endsl
045900020121     c                   leave
046000051027e   3c                   endif
046100051027e   2c                   endif
046200051027
046300051027e   1c                   enddo
046400051027
046500051027     c                   if        *in41   = *off
046600051027     c                   select
046700051027     c                   when      WrkSort = '0'
046800051027     c                   clear                   Vcod1
046900051027     c                   when      WrkSort = '1'
047000051027     c                   clear                   Vesec
047100051027     c                   clear                   Vfreq
047200051027     c                   clear                   Vprio
047300051027     c                   clear                   Vsott
047400051027     c                   clear                   Vcod2
047500051027     c                   endsl
047600051027     c                   endif
047700020121
047800020121     c                   endsr
047900020121
048000020121      *------------------------------------------------------------------*
048100020121      *                   ROUTINE INIZIALE
048200020121      *------------------------------------------------------------------*
048300020121
048400020121     c     *inzsr        begsr
048500020121
048600020121     c     *entry        plist
048700021114     c                   Parm                    kpjba
048800051027
048900051027      * Reperimento Dati del job (Utente/Operativi)
049000051027     c     *dtaara       define    §azute        azuteds
049100051027     c     *dtaara       define    §datiute      ddatiute
049200051027     c                   in(E)     *dtaara
049300051027     c                   IF        %ERROR or RSUT = *blanks
049400051027     c                   clear                   Tibs34Ds
049500051027     c                   call      'TIBS34R'
049600051027     c                   parm                    Tibs34Ds
049700051027     c                   in        *dtaara
049800051027     c                   ENDIF
049900020121
050000020121     c                   movel     kpjbu         param01
050100020121
050200020121     c     ktntbe        klist
050300020121     c                   kfld                    kcod
050400020121     c                   kfld                    kke1
050500020121
050600021114     c                   eval      kcod = 'SCH'
050700020121
050800020121     c                   endsr
