000100110603     /*END
000200020121     H DECEDIT('0,') DATEDIT(*DMY.) option(*nodebugio)
000300090423     h dftactgrp(*no) actgrp(*caller)
000400020121      *--------------------------------------------------------------------------------------------*
000500021114      * Interrogazione tabella Azioni Schedultae 'SCH'                                             *
000600021114      * Permette la ricerca per frequenza/priorità                                                 *
000700020121      *--------------------------------------------------------------------------------------------*
000800020121
000900020121      * PARAMETRI:
001000021114      * Azione          4  I
001100051027      * Ordinamento     1  I  (*blanks = Codice;
001200131129      *                        1 = Eseguire in Frequenza/Priorità/Sottomissione/Codice)
001300021114      * Sistema inf.   10  I
001400021114      * Chiave tabella  4  O
001500021114      * Codice ritorno  1  O  (comando premuto)
001600020121
001700020121      *--------------------------------------------------------------------------------------------*
001800020121
001900090319     ftntbe01l  if   e           k disk
002000090319      *
002100021114     ftntb281d  cf   e             workstn
002200021114     f                                     sfile(tb281s01:nrr)
002300090420
002400090420     D GIORSET         s              3    dim(7) ctdata perrcd(7)              Messaggi video
002500090420     D GIORSET2        s              2    dim(7) ctdata perrcd(7)              Messaggi video
002600090420     D numset          s              1  0 dim(7)                               Orari
002700090420
002800011120      **********************************************************************
002900011120      * Constants
003000011120      *
003100011120      *     MaxKey - Maximum number of key fields allowed by this program
003200011120      *
003300011120      **********************************************************************
003400090420     D MaxKey          C                   6
003500011120     D Ascendente      C                   1
003600011120     D Carattere       C                   6
003700011120     D Put             C                   1
003800011120     D EndPut          C                   2
003900011120     D Get             C                   3
004000011120      **********************************************************************
004100011120      * Standalone fields
004200011120      *
004300020121      *     Nrr        - Relative record number for subfile
004400011120      *     SizeList   - Size of list
004500011120      *     ReturnSize - Size of list returned by sort API
004600011120      **********************************************************************
004700090420     D NotUsed         S             16A   inz
004800090420     D ReturnSize      S              9B 0 inz
004900090420     D SizeList        S              9B 0 inz
005000090420     D RrnLast         S              5i 0 inz
005100090420     d Nrr             s              5i 0 inz
005200051027     D WrkSort         S              1    inz('1')
005300011120      **********************************************************************
005400011120      * Data Structures
005500011120      *     SflRcd     - The entire subfile record to pass to the sort
005600011120      *     QLGSCB     - The sort request block for the QLGSORT API
005700011120      *     QLGSCB00   - The sort request block for the QLGSRTIO API
005800011120      *     QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB
005900011120      *     QUSEC      - Error structure for the QLGSORT API
006000011120      **********************************************************************
006100090420     D SflRcd          DS                  inz
006200090420     d  vs1sce                             inz
006300090420     d  vs1sch                             inz
006400090420     d  vh1prg                             inz
006500090420     d  vs1des                             inz
006600090420     d  vs1ese                             inz
006700090420     d  vs1fre                             inz
006800090420     d  vs1gio                             inz
006900090420     d  vs1pri                             inz
007000090420     d  vs1tps                             inz
007100090420     d  vs1job                             inz
007200090420     d  vs1ute                             inz
007300090420     d  vs1or1                             inz
007400090420     d  vs1or2                             inz
007500090420     d  vs1or3                             inz
007600090420     d  vs1or4                             inz
007700090420     d  vs1or5                             inz
007800121009     D  VH1Sosp                            inz
007900090420     D  Selected                      1A
008000090420
008100090420      /COPY QSYSINC/QRPGLESRC,QLGSORT
008200090420     D QLGKL                         16    DIM(MaxKey)
008300090420     D  QLGSP00                       9B 0 OVERLAY(QLGKL:00001)
008400090420     D  QLGSS00                       9B 0 OVERLAY(QLGKL:00005)
008500090420     D  QLGDT00                       9B 0 OVERLAY(QLGKL:00009)
008600090420     D  QLGSO00                       9B 0 OVERLAY(QLGKL:00013)
008700090420
008800090420      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
008900090420      /COPY QSYSINC/QRPGLESRC,QUSEC
009000090319
009100090319     d Status         sds
009200090319     d  SDSpgm           *proc
009300021114
009400051027     d KPJBA         e ds
009500021114     d param01         ds
009600021114     d  p01sch                        4
009700021114     d  p01ord                        1
009800021114     d  p01sif                       10
009900101005     d  p01opz                        1
010000021114     d  p01ke1                        4
010100080721     d  p01ke2                       15
010200110603     d  p01cnd                       20
010300021114     d  p01rit                        1
010400161212     d  p01init                       1
010500021114
010600051027      * - Parametri x Controllo profilo utenti
010700051027     d TIBS34DS      e ds
010800051027      * - Ds di riferimento al file esterno AZUTE00F
010900051027     d AZUTEds       e ds                  extname(AZUTE00F)
011000051027      * - Ds per dati organigramma
011100051027     d DDatiUte      e ds
011200051027
011300051027     d dSCH          e ds                  inz
011400060428     D  GGGiorn              125    131    DIM(7)
011500051027
011600051027     d kcod            s                   like(tbecod)  inz
011700051027     d klin            s                   like(tbelin)  inz
011800051027     d ksif            s                   like(tbesif)  inz
011900051027     d kke1            s                   like(tbeke1)  inz
012000051027      *
012100051027     d Scod1           s                   like(Vcod1)   inz
012200051027     d Sfreq           s                   like(Vfreq)   inz
012300051027     d Sprio           s                   like(Vprio)   inz
012400051027     d Ssott           s                   like(Vsott)   inz
012500051027     d Scod2           s                   like(Vcod2)   inz
012600051027
012700090423     d XX              s              5  0 inz
012800090423
012900090423     d $F24            s               n   inz
013000161212     d wScelta         s              1    inz
013100020121
013200161212     c                   movel     kpjbu         param01
013300161213
013400161213     c                   select
013500161213
013600161213      * se richiesto di inizializzare
013700161213     c                   when      p01init =*blank
013800020121      * Ordina il sfl in base al parametro passato
013900020121     c                   exsr      Initialize
014000161212     c                   eval      *in01=*on
014100161213
014200161213      * se richiesto di proseguire con la successiva scelta
014300161213     c                   when      p01init = 'N'
014400161212      * Controlli
014500161212     c                   exsr      Ctr01
014600161212
014700161212     c                   select
014800161212      * se non è stata effettuata nessuna scelta
014900161212     c                   when      wScelta = *blank
015000161212      * inizializzo il SFL
015100161212     c                   exsr      Initialize
015200161212     c                   eval      *in01=*on
015300161212      * annullamento
015400161213     c***                when      wScelta = '4'
015500161213     c***                exsr      sr_Annulla
015600161212      * qualunque altra
015700161212     c                   other
015800161212     c                   if        p01ke1 <> *Blanks
015900161212     c                   Clear                   p01rit
016000161212     c                   endif
016100161212
016200161212     c                   endsl
016300161213
016400161213      * se richiesto di presentare il sfl com'è
016500161213     c                   when      p01init = 'R'
016600161213      * entro nel ciclo
016700161213     c                   eval      *in01=*on
016800161213
016900161213     c                   endsl
017000020121
017100161212     c***                do        *hival
017200161212     c                   dow       *in01=*on
017300021114      *Descrizione della ricerca
017400051027     c                   select
017500051027     c                   when      WrkSort = '0'
017600170120     c                   eval      *in42 = *on
017700051027     c                   when      WrkSort = '1'
017800170120     c                   eval      *in42 = *off
017900051027     c                   endsl
018000090319
018100090423     c                   eval      *in41 = (Nrr = *zeros)
018200090319     c                   eval      *in09 = (Nrr > *zeros)
018300020121
018400021114     c                   write     tb281t01
018500090423     c                   if        Not $F24
018600021114     c                   write     tb281z01
018700090423     c                   else
018800090423     c                   write     TB281Z02
018900090423     c                   endif
019000021114     c                   exfmt     tb281c01
019100021114
019200090319     c                   select
019300090319
019400090319      * F3 = Fine
019500090423     c                   when      *inKC
019600021114     c                   move      'C'           p01rit
019700021114     c                   leave
019800090423
019900090423      * F4 = Ricerca azione
020000090423     c                   when      *inKD
020100090423     c                   exsr      sr_Select
020200090423     c                   iter
020300020121
020400090319      * F5 = Rivisualizza
020500090423     c                   when      *inKE
020600020121     c                   exsr      Initialize
020700020121
020800051027      * F11 = Cambio ordinamento
020900090423     c                   when      *inKK
021000051027     c                   select
021100051027     c                   when      WrkSort = '0'
021200051027     c                   eval      WrkSort = '1'
021300051027     c                   when      WrkSort = '1'
021400131128     c                   eval      WrkSort = '0'
021500051027     c                   endsl
021600020121     c                   exsr      Sort
021700090423
021800090423      * F12 = Ritorno
021900090423     c                   when      *inKL
022000090423     c                   move      'L'           p01rit
022100090423     c                   leave
022200090423
022300090423      * F24 = Altri tasti
022400090423     c                   when      *inKY
022500090423     c                   if        $F24 = *off
022600090423     c                   eval      $F24 = *on
022700090423     c                   else
022800090423     c                   eval      $F24 = *off
022900090423     c                   endif
023000090423     c                   iter
023100020121
023200020121     c                   endsl
023300020121
023400020121      * Controlli
023500170516     c*//                if        nrr > 0
023600020123     c                   exsr      Ctr01
023700170516     c*//                endif
023800020121
023900020121      * Effettuata una scelta
024000161212     c                   select
024100161212      * se non è stata effettuata nessuna scelta
024200161212     c                   when      wScelta = *blank
024300161212      * annullamento
024400161213     c***                when      wScelta = '4'
024500161213     c***                exsr      sr_Annulla
024600161212      * qualunque altra
024700161212     c                   other
024800161212     c                   if        p01ke1 <> *Blanks
024900161212     c                   Clear                   p01rit
025000161212     c                   leave
025100161212     c                   endif
025200161212     c                   endsl
025300020121
025400020121     c                   enddo
025500020121
025600110603     c                   eval      P01ord = WrkSort
025700110603     c                   eval      P01cnd = W01cnd
025800020121     c                   movel     param01       kpjbu
025900020121
026000161212     c                   if        p01rit = 'L' or
026100161212     c                             p01rit = 'C'
026200161212     c                   eval      *inlr = *on
026300161212     c                   else
026400161212     c                   eval      *inrt = *on
026500161212     c                   return
026600161212     c                   endif
026700020121
026800020121      *------------------------------------------------------------------*
026900020121     c     Initialize    begsr
027000020121
027100110603     c                   eval      W01cnd = P01cnd
027200110603
027300020121     c                   eval      *in40 = *on
027400021114     c                   write     tb281c01
027500020121     c                   eval      *in40 = *off
027600090423     c                   eval      *in39 = *off
027700020121     c                   clear                   nrr
027800020121
027900020121      * Carico il subfile in ordine di codice
028000051027     c     kcod          setll     tntbe000
028100020121     c                   do        *hival
028200051027     c     kcod          reade     tntbe000
028300020121
028400090423     c                   SELECT
028500090423
028600020121      * Fine file
028700090423     c                   WHEN      %eof(tntbe01l)
028800090423     c                   eval      *in39 = *on
028900020121     c                   leave
029000020123
029100020123      * Escludo annullati
029200090423     c                   WHEN      tbeatb <> *blanks
029300020123     c                   iter
029400020123
029500021114      * Escludo se non del S.I. richiesto
029600090423     c                   WHEN      tbesif <> *blanks and
029700021114     c                             tbesif <> p01sif
029800020123     c                   iter
029900090423
030000090423     c                   ENDSL
030100090423
030200090423     c                   movel(p)  tbeuni        Dsch
030300090423
030400090423      * Seleziono per contenuto della descrizione
030500090423     c                   if        W01cnd <> *blanks  and
030600090423     c                             %scan( %trim(W01cnd) : %trimr(d§SCHdes) )
030700090423     c                                    =  *zeros
030800090423     c                   iter
030900090423     c                   endif
031000020121
031100090422     c                   clear                   tb281s01
031200090422
031300090320     c                   move      TBEke2        VH1prg
031400021114     c                   movel(p)  tbeke1        vs1sch
031500090320     c                   if        TBEke2 = *blank
031600090320     c                   clear                   VS1prg
031700090320     c                   else
031800090320     c                   eval      VS1prg = %editc( %int( %subst( TBEke2
031900090320     c                                      : %len(TBEke2) - %len(VS1prg) + 1
032000090320     c                                      : %len(VS1prg) ) ) : 'Z' )
032100090320     c                   endif
032200121009     C* metto il codice azione in alta intensità se questa è sospesa
032300121009     C                   SETOFF                                       21
032400121009     C                   EVAL      VH1Sosp = d§SCHSosp
032500121009     C                   IF        d§SCHSosp = 'S'
032600121009     C                   SETON                                        21
032700121009     C                   ENDIF
032800021114     c                   movel     d§schdes      vs1des
032900051027     c                   clear                   vs1ese
033000051027     c                   Select
033100051027     c                   when      d§schese = 'E'
033200021114     c                   movel     'S/Po'        vs1ese
033300051027     c                   when      d§schese = 'P'
033400021114     c                   movel     'P.O.'        vs1ese
033500051027     c                   when      d§schese = 'S'
033600021114     c                   movel     'Sede'        vs1ese
033700051027     c                   EndSl
033800060428     c
033900051027     c                   clear                   vs1fre
034000060428     c                   clear                   vs1gio
034100051027     c                   Select
034200051027     c                   when      d§schFre = 'G'
034300060428     c                   movel     'GIOR'        vs1fre
034400060428     c* Conto quanti giorni sono pieni, per vedere come scrivere
034500060428     c                   clear                   conta             2 0
034600060428     c                   clear                   numset
034700060428     c
034800060428     c                   do        7             c                 1 0
034900060428     c                   if        ggGiorn(c)<>' '
035000060428     c                   add       1             conta
035100060428     c                   movel     ggGiorn(c)    numset(c)
035200060428     c                   endif
035300060428     c                   enddo
035400060428     c*
035500060428     c* Fino a 4 giorni, scrivo spaziato
035600060428     c                   select
035700060428     c                   when      conta=0 or conta=7
035800060428     c                   movea     giorset2      vs1gio
035900060428     c
036000060428     c                   when      conta<=4
036100060428     c                   z-add     1             c
036200060428     c                   dow       numset(c)>0
036300060428     c                   z-add     numset(c)     y
036400060428     c                   if        c>1
036500060428     c                   eval      vs1gio=%trim(vs1gio)+' '+giorset2(y)
036600060428     c                   else
036700060428     c                   eval      vs1gio=giorset2(y)
036800060428     c                   endif
036900060428     c                   add       1             c
037000060428     c                   enddo
037100060428     c                   other
037200060428     c                   z-add     1             c
037300060428     c                   dow       numset(c)>0
037400060428     c                   z-add     numset(c)     y                 1 0
037500060428     c                   eval      vs1gio=%trim(vs1gio)+giorset2(y)
037600060428     c                   add       1             c
037700060428     c                   enddo
037800060428     c                   endsl
037900060428     c
038000051027     c                   when      d§schFre = 'S'
038100060428     c                   movel     'SETT'        vs1fre
038200060428     c* Imposto il giorno
038300060428     c                   if        d§schg1s=' '
038400060428     c                   movel     'Sab'         vs1gio
038500060428     c                   else
038600060428     c                   movel     d§schg1s      c
038700060428     c                   movel     giorset(c)    vs1gio
038800060428     c                   endif
038900060428     c
039000051027     c                   when      d§schFre = 'M'
039100060428     c                   movel     'MENS'        vs1fre
039200060428     c                   movel     d§schggm      vs1gio
039300051027     c                   EndSl
039400021114     c                   move      d§schpri      vs1pri
039500051027     c                   clear                   vs1tps
039600021114     c                   If        d§schtps = 'M'
039700021114     c                   movel     'Mult'        vs1tps
039800021114     c                   EndIf
039900021114     c                   movel     d§schjob      vs1job
040000021114     c                   movel     d§schute      vs1ute
040100021114     c                   movel     d§schor1      vs1or1
040200021114     c                   movel     d§schor2      vs1or2
040300021114     c                   movel     d§schor3      vs1or3
040400021114     c                   movel     d§schor4      vs1or4
040500021114     c                   movel     d§schor5      vs1or5
040600170120
040700170120      * Adesso che ho recuperato tutti i dati per formare il rcd del sfl
040800170120      * escludo se campi non richiesto da sflctl
040900170120     c                   IF        vcod1  <> *blanks and
041000170120     c                             vcod1  <> vs1sch      OR
041100170120     c                             vfreq  <> *blanks and
041200170120     c                             vfreq  <> vs1fre      OR
041300170120     c                             vprio  <> *blanks and
041400170120     c                             vprio  <> vs1pri      OR
041500170120     c                             vsott  <> *blanks and
041600170120     c                             vsott  <> vs1tps
041700170120     c                   iter
041800170120     c                   endif
041900170120
042000020121     c                   add       1             nrr
042100021114     c                   write     tb281s01
042200020121     c                   enddo
042300020121
042400020121     c                   eval      RrnLast = nrr
042500020121     c                   eval      rcdnbr  = 1
042600020121
042700051027      * Se è richiesto un ordinamento (valido): sorto il sfl
042800051027     c                   if            P01ord  = '0'
042900051027     c                             or  P01ord  = '1'
043000020121     c                   eval      WrkSort = p01ord
043100051027     c                   endif
043200011219     c                   exsr      sort
043300020121
043400020121     c                   endsr
043500011120
043600011120      **********************************************************************
043700011120      * Subroutine - Sort
043800011120      *   This subroutine sorts the subfile records.
043900011120     C     Sort          BEGSR
044000051027
044100170120     c***                select
044200170120     c***                when      WrkSort = '0'
044300170120     c***                clear                   Vcod1
044400170120     c***                clear                   Scod1
044500170120     c***                when      WrkSort = '1'
044600170120     c***                clear                   Vfreq
044700170120     c***                clear                   Vprio
044800170120     c***                clear                   Vsott
044900170120     c***                clear                   Vcod2
045000170120     c***                clear                   Sfreq
045100170120     c***                clear                   Sprio
045200170120     c***                clear                   Ssott
045300170120     c***                clear                   Scod2
045400170120     c***                endsl
045500011121     C                   EVAL      rcdnbr  = 1
045600011120
045700011120      * Initialize the key fields to sort on. There is one extra field not in
045800011120      * the subfile -- Selected -- that is added to the record. This field
045900011120      * is used to place selected records in front of nonselected records.
046000011120
046100051027     C                   CLEAR                   QLGKL
046200051027     C                   CLEAR                   QLGSKL
046300011120     C                   CLEAR                   QLGSCB
046400011120     C                   CLEAR                   QLGSCB00
046500011120
046600011120     ** Gestione della scelta ordinamento.
046700011120     C                   SELECT
046800051027     ** Ordinamento per Esec./Freq./Priorità/Sottom./Azione
046900051027     c                   WHEN      WrkSort = '1'
047000090420     ** 6 campi chiave
047100090420     c                   EVAL      QLGNBRK    = 6
047200090420     ** 1° campo: Esecuzione (Filiale/Sede/entrambi)
047300131128     **           a posizone 46,  4 byte, char, ascending.
047400131128     c                   eval      QLGSP      = 46
047500051027     c                   eval      QLGSS      = %size(VS1ese)
047600051027     c                   eval      QLGDT      = Carattere
047700051027     c                   eval      QLGSO      = Ascendente
047800051027     c                   eval      QLGKL(1)   = QLGSKL
047900051027     ** 2° campo: Frequenza
048000131128     **           a posizone 50,  4+13 byte, char, ascending.
048100131128     c                   eval      QLGSP      = 50
048200131128     c                   eval      QLGSS      = %size(VS1fre) + %size(VS1gio)
048300051027     c                   eval      QLGDT      = Carattere
048400051027     c                   eval      QLGSO      = Ascendente
048500051027     c                   eval      QLGKL(2)   = QLGSKL
048600090420     ** 3° campo: Priorità + primo carattere del tps
048700131128     **           a posizone 67,  4 byte, char, ascending.
048800131128     c                   eval      QLGSP      = 67
048900131128     c                   eval      QLGSS      = %size(VS1pri) + 1
049000051027     c                   eval      QLGDT      = Carattere
049100051027     c                   eval      QLGSO      = Ascendente
049200051027     c                   eval      QLGKL(3)   = QLGSKL
049300051027     ** 4° campo: Sottomissione
049400131128     **           a posizone 74, 10 byte, char, ascending.
049500131128     c                   eval      QLGSP      = 74
049600051027     c                   eval      QLGSS      = %size(VS1job)
049700051027     c                   eval      QLGDT      = Carattere
049800051027     c                   eval      QLGSO      = Ascendente
049900051027     c                   eval      QLGKL(4)   = QLGSKL
050000051027     ** 5° campo: Azione schedulata
050100131128     **           a posizone  2, 4 byte, char, ascending.
050200131128     c                   eval      QLGSP      = 2
050300051027     c                   eval      QLGSS      = %SIZE(VS1sch)
050400051027     c                   eval      QLGDT      = Carattere
050500051027     c                   eval      QLGSO      = Ascendente
050600051027     c                   eval      QLGKL(5)   = QLGSKL
050700090420     ** 6° campo: Progressivo per parametri dell'azione schedulata
050800090420     **           a posizone  6, 15 byte, char, ascending.
050900090420     c                   eval      QLGSP      = 6
051000090420     c                   eval      QLGSS      = %SIZE(VH1prg)
051100090420     c                   eval      QLGDT      = Carattere
051200090420     c                   eval      QLGSO      = Ascendente
051300090420     c                   eval      QLGKL(6)   = QLGSKL
051400021114
051500011120     ** Ordinamento per Codice
051600051027     C                   WHEN      WrkSort = '0'
051700090420     ** 2 campi chiave
051800090420     C                   EVAL      QLGNBRK    = 2
051900090420     ** 1° campo: Azione schedulata
052000090420     **           a posizione 2, 4 byte, char, ascending
052100021114     C                   EVAL      QLGSP      = 2
052200131128     C                   EVAL      QLGSS      = %SIZE(VS1sch)
052300011120     C                   EVAL      QLGDT      = Carattere
052400011120     C                   EVAL      QLGSO      = Ascendente
052500011120     C                   EVAL      QLGKL(1)   = QLGSKL
052600090420     ** 2° campo: Progressivo x parametri dell'azione schedulata
052700090420     **           a posizone  6, 15 byte, char, ascending.
052800090420     c                   eval      QLGSP      = 6
052900090420     c                   eval      QLGSS      = %SIZE(VH1prg)
053000090420     c                   eval      QLGDT      = Carattere
053100090420     c                   eval      QLGSO      = Ascendente
053200090420     c                   eval      QLGKL(2)   = QLGSKL
053300051027
053400011120     C                   ENDSL
053500011120      * Load other sort parameters.
053600011120     C                   EVAL      QLGLB     = 80 + 16 * MaxKey
053700011120     C                   EVAL      QLGRL     = %SIZE(SflRcd) - 1
053800011120     C                   EVAL      QLGRT     = 8
053900011120     C                   EVAL      QLGOKL    = 80
054000011120     C                   EVAL      QLGLKE    = 16
054100011120     C                   EVAL      QLGLSS    = 290
054200011120      * Initialize Sort I/O API fields.
054300011120     C                   EVAL      QLGRL00  = QLGRL
054400011120     C                   EVAL      QLGRC00  = 1
054500011120     C                   CLEAR                   QUSEI
054600011120     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
054700011120      * First step - Initialize the sort routine.
054800011120     C                   CALL      'QLGSORT'
054900011120     C                   PARM                    QLGSCB
055000011120     C                   PARM                    NotUsed
055100011120     C                   PARM                    NotUsed
055200011120     C                   PARM                    SizeList
055300011120     C                   PARM                    ReturnSize
055400011120     C                   PARM                    QUSEC
055500011120      * Next step - Write records to I/O routine.
055600011120     C                   EVAL      QLGRT00 = Put
055700051027     C                   DO        RrnLast       xx
055800051027     C     xx            CHAIN     tb281s01
055900011120     ** Solo le righe con Selected = 'Y' sono riordinate,
056000011120     ** quindi per fare un ordinamento di tutte le righe
056100011120     ** metto 'Y' sempre.
056200011120     C                   EVAL      Selected = 'Y'
056300011120     C                   CLEAR                   QUSEI
056400011120     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
056500011120     C                   CALL      'QLGSRTIO'
056600011120     C                   PARM                    QLGSCB00
056700011120     C                   PARM                    SflRcd
056800011120     C                   PARM                    NotUsed
056900011120     C                   PARM                    SizeList
057000011120     C                   PARM                    NotUsed
057100011120     C                   PARM                    QUSEC
057200011120     C                   ENDDO
057300011120      * Next step - Signal end of input, clear subfile for reload.
057400011120     C                   EVAL      QLGRT00 = EndPut
057500011120     C                   CLEAR                   QUSEI
057600011120     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
057700011120
057800011120     C                   CALL      'QLGSRTIO'
057900011120     C                   PARM                    QLGSCB00
058000011120     C                   PARM                    SflRcd
058100011120     C                   PARM                    NotUsed
058200011120     C                   PARM                    SizeList
058300011120     C                   PARM                    NotUsed
058400011120     C                   PARM                    QUSEC
058500020121     C                   seton                                        40
058600021114     C                   WRITE     tb281c01
058700011120     C                   setoff                                       40
058800011120      * Final step - Write the records back to the subfile.
058900011120     C                   EVAL      QLGRT00 = Get
059000051027     C                   DO        RrnLast       xx
059100011120     C                   CLEAR                   QUSEI
059200011120     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
059300011120     C                   CALL      'QLGSRTIO'
059400011120     C                   PARM                    QLGSCB00
059500011120     C                   PARM                    NotUsed
059600011120     C                   PARM                    SflRcd
059700011120     C                   PARM                    QLGRL00
059800011120     C                   PARM                    NotUsed
059900011120     C                   PARM                    QUSEC
060000051027     C                   z-add     xx            nrr
060100090422     c                   if        VH1prg > *zeros
060200090420     c                   eval      VS1prg = %editc( %int( %subst( VH1prg
060300090420     c                                      : %len(VH1prg) - %len(VS1prg) + 1
060400090420     c                                      : %len(VS1prg) ) ) : 'Z' )
060500090422     c                   else
060600090422     c                   clear                   VS1prg
060700090422     c                   endif
060800121009     C* metto il codice azione in alta intensità se questa è sospesa
060900121009     C                   SETOFF                                       21
061000121009     C                   IF        VH1Sosp = 'S'
061100121009     C                   SETON                                        21
061200121009     C                   ENDIF
061300021114     C                   WRITE     tb281s01
061400011120     C                   ENDDO
061500011120     C                   ENDSR
061600090423
061700090423      /free
061800090423       //---------------------------------------------------------------------
061900090423       //?Richiesta selezioni per record nel subfile S01.                     ?
062000090423       //---------------------------------------------------------------------
062100090423       BEGSR  sr_Select;
062200090423
062300090423         //?Inizializzazione videata?
062400090423         // - Meglio ripresentare l'ultima selezione!
062500090423         //clear  TB281W01;
062600090423
062700090423         //?Emissione videata?
062800090423         exfmt TB281W01;
062900090423
063000090423         //?F12=Ritorno?
063100090423         if  *inKL;
063200090423           leavesr;
063300090423         endif;
063400090423
063500090423         //?Invio?
063600110603         P01cnd = W01cnd;
063700090423         exsr Initialize;
063800090423
063900090423       ENDSR;
064000090423      /end-free
064100020121
064200020121      *------------------------------------------------------------------*
064300051027     c     Ctr01         begsr
064400170120
064500170120      * se 1 dei filtri è valorizzato diversamente da prima
064600170120     c                   IF        vcod1  <> scod1       OR
064700170120     c                             vfreq  <> sfreq       OR
064800170120     c                             vprio  <> sprio       OR
064900170120     c                             vsott  <> ssott
065000170120      * inizializzo il SFL
065100170120     c                   exsr      Initialize
065200170120      *
065300170120     c                   movel     Vcod1         Scod1
065400170120     c                   movel     Vfreq         Sfreq
065500170120     c                   movel     Vprio         Sprio
065600170120     c                   movel     Vsott         Ssott
065700170120     c                   movel     Vcod2         Scod2
065800170120     c                   endif
065900170120
066000020121     c                   eval      *in41 = *off
066100161213     c                   eval      wScelta = *blank
066200170516
066300170516     c                   if        nrr = 0
066400170516     c                   leavesr
066500170516     c                   endif
066600051027
066700161213do  1c***                do        *hival        xx
066800161213do  1c                   do        *hival
066900051027
067000161213     c***  xx            chain     tb281s01                           01
067100161213     c                   readc     tb281s01                               01
067200161212     c   01              leave
067300020121
067400020121     c                   if        vs1sce <> ' '
067500161209     c* se l'opzione è la cancellazione, non la torno indietro al pgm chiamante ma l'eseguo qua
067600161212     c                   eval      wScelta = vs1sce
067700161209     c                   eval      p01opz =  VS1sce
067800021114     c                   movel(p)  vs1sch        p01ke1
067900090320     c                   move      VH1prg        p01ke2
068000161212     c                   clear                   VS1sce
068100161213     c                   update    tb281s01
068200011120     c                   leave
068300161209     c                   end
068400170120      // spostate specifiche in sr ctr_sflctl però non servono più
068500051027e   1c                   enddo
068600051027
068700170120     c***                if        *in41   = *off
068800170120     c***                select
068900170120     c***                when      WrkSort = '0'
069000170120     c***                clear                   Vcod1
069100170120     c***                when      WrkSort = '1'
069200170120     c***                clear                   Vfreq
069300170120     c***                clear                   Vprio
069400170120     c***                clear                   Vsott
069500170120     c***                clear                   Vcod2
069600170120     c***                endsl
069700170120     c***                endif
069800020121
069900020121     c                   endsr
070000020121
070100020121      *------------------------------------------------------------------*
070200170120      * Test campi del sflctl (l'ordinamento a oggi)
070300020121      *------------------------------------------------------------------*
070400020121
070500170120        begsr ctr_sflctl;
070600161209
070700170120      * Controllo se effettuata ricerca
070800170120if  2c                   if            (WrkSort  = '0'
070900170120     c                             and  Vcod1   <> *blanks
071000170120     c                             and  Vcod1   <> Scod1)
071100170120     c                             OR  (WrkSort  = '1'
071200170120     c                             and (Vfreq   <> *blanks
071300170120     c                             and  Vfreq   <> Sfreq)
071400170120     c                              or (Vprio   <> *blanks
071500170120     c                             and  Vprio   <> Sprio)
071600170120     c                              or (Vsott   <> *blanks
071700170120     c                             and  Vsott   <> Ssott)
071800170120     c                              or (Vcod2   <> *blanks
071900170120     c                             and  Vcod2   <> Scod2))
072000170120     c                   eval      *in41 = *on
072100170120if  3c                   if            (WrkSort  = '0'
072200170120     c                             and  Vcod1   <= VS1sch)
072300170120     c                             OR  (WrkSort  = '1'
072400170120     c                             and  Vfreq   <= VS1fre
072500170120     c                             and  Vprio   <= VS1pri
072600170120     c                             and  Vsott   <= VS1tps
072700170120     c                             and  Vcod2   <= VS1sch)
072800170120     c                   eval      *in41 = *off
072900170120     c                   z-add     xx            RcdNbr
073000170120     c                   select
073100170120     c                   when      WrkSort = '0'
073200170120     c                   movel     Vcod1         Scod1
073300170120     c                   when      WrkSort = '1'
073400170120     c                   movel     Vfreq         Sfreq
073500170120     c                   movel     Vprio         Sprio
073600170120     c                   movel     Vsott         Ssott
073700170120     c                   movel     Vcod2         Scod2
073800170120     c                   endsl
073900170120     c***                leave
074000170120e   3c                   endif
074100170120e   2c                   endif
074200170120
074300170120        endsr;
074400170120
074500170120      *------------------------------------------------------------------*
074600170120      * Cancellazione rcd
074700170120      *------------------------------------------------------------------*
074800170120
074900170120        begsr sr_Annulla;
075000170120
075100161209          // se il rcd ha un progressivo, devo cancellare anche il file di dettaglio
075200161209          if  p01ke2 <> *blank;
075300161209            exec sql
075400161209            update tntbe00f
075500161209            set tbeatb='A', tbeftt=' '
075600161209            where TBECOD = 'SCK' and
075700161209            TBEKE1 = :p01ke2 and
075800161209            TBEKE2 = ' ' and
075900161209            TBELIN = ' ';
076000161209          endif;
076100161209
076200161209          exec sql
076300161209          update tntbe00f
076400161209          set tbeatb='A', tbeftt=' '
076500161209          where TBECOD = 'SCH' and
076600161209          TBEKE1 = :p01ke1 and
076700161209          TBEKE2 = :p01ke2 and
076800161209          TBELIN = ' ' and
076900161209          TBESIF = :p01SIf;
077000161209
077100161209          // pulisco questi dati perché non devo andare nel pgm di gestione dell'elemento di
077200161209          // tabella
077300161209          clear p01ke1;
077400161209          clear p01ke2;
077500161209
077600161209          exsr Initialize;
077700161209
077800161209        endsr;
077900161209
078000161209      *------------------------------------------------------------------*
078100161209      *                   ROUTINE INIZIALE
078200161209      *------------------------------------------------------------------*
078300161209
078400161209     c     *inzsr        begsr
078500161209
078600020121     c     *entry        plist
078700021114     c                   Parm                    kpjba
078800051027
078900051027      * Reperimento Dati del job (Utente/Operativi)
079000051027     c     *dtaara       define    §azute        azuteds
079100051027     c     *dtaara       define    §datiute      ddatiute
079200051027     c                   in(E)     *dtaara
079300051027     c                   IF        %ERROR or RSUT = *blanks
079400051027     c                   clear                   Tibs34Ds
079500051027     c                   call      'TIBS34R'
079600051027     c                   parm                    Tibs34Ds
079700051027     c                   in        *dtaara
079800051027     c                   ENDIF
079900020121
080000020121     c                   movel     kpjbu         param01
080100020121
080200020121     c     ktntbe        klist
080300020121     c                   kfld                    kcod
080400020121     c                   kfld                    kke1
080500020121
080600090319     c                   movel     SDSpgm        V1Tpgm
080700131129     c                   if        %subst( p01sif : 1 : 3 ) = 'GAI'
080800131129      * - ' nel Sistema Informativo di  SEDE '
080900131129     c                   eval      V1Ttxt2 = %trimr(V1Ttxt2) + ' SEDE   '
081000131129     c                   eval      V1Ttxt2 = ' nel Sistema Informativo di  +
081100131129     c                                        SEDE '
081200131129     c                   else
081300131129      * - 'nel Sistema Informativo di FILIALE'
081400131129     c                   eval      V1Ttxt2 = 'nel Sistema Informativo di +
081500131129     c                                        FILIALE'
081600131129     c                   endif
081700021114     c                   eval      kcod = 'SCH'
081800020121
081900020121     c                   endsr
082000060428     c
082100060428** GIORSET-  GIORNI DELLA settimana                                          *
082200060428LunMarMerGioVenSabDom
082300060428** GIORSET2-  GIORNI DELLA settimana                                         *
082400060428LuMaMeGiVeSaDo
