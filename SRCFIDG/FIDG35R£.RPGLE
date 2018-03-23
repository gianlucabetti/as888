000100950523     H DECEDIT('0,') DATEDIT(*DMY.)
000200070427     H* FIDG35R *-----------------------------------------------------*
000300070427     H*     Visualizza valori assegnati in distinta x GIRO
000400930225     H*---------------------------------------------------------------*
000500070427     FFidg35D   CF   E             WORKSTN
000600070427     F                                     SFILE(dg02S01:NRR)
000700070927     FazORG01L  IF   E           K DISK
000800070427     Ffidst08L  IF   E           K DISK
000900070427     Ffidst41L  IF   E           K DISK
001000070724     Ffidst61L  IF   E           K DISK
001100070427     Ffiapd01L  IF   E           K DISK
001200030724     d dtaiso          s               d
001300030724     d dtaeur          s               d   datfmt(*eur)
001400070927     D OG147         E DS                  INZ
001500070427     D fidg35ds      E DS
001600070427     D fidg09ds      E DS
001700900514     D KPJBA         E DS
001800061002     d* Parametri x Controllo profilo utenti
001900061002     d TIBS34ds      e ds
002000061002     d AZUTEds       e ds                  extname(AZUTE00F)
002100061002     d* Ds per dati organigramma
002200061002     d DDatiUte      e ds
002300070724     d NotUsed         S             16A
002400070724     d ReturnSize      S              9B 0
002500070724     d SizeList        S              9B 0
002600070724     d WrkSort         S              1
002700070724     d*---
002800070724     d Rec1            S              3  0                                      *n° rel sf
002900070724     d stmrec1         S              3  0                                      *n° rel sf x stampa
003000070724     d*---
003100070724     d MaxKey          C                   2
003200070724     d Ascendente      C                   1
003300070724     d Discendente     C                   2
003400070724     d Carattere       C                   6
003500070724     d Put             C                   1
003600070724     d EndPut          C                   2
003700070724     d Get             C                   3
003800070724     d Numerico        C                   8
003900070724     d*---
004000070724     d SflRcd          DS
004100070724     d  vscopz
004200070724     d  vsctp
004300070724     d  vsctass
004400070724     d  vsccgi
004500070724     d  vsdcgi
004600070724     d  vscprgg
004700070724     d  vscsql
004800070724     d  vscnbl
004900070724     d  vscpkl
005000070724     d  vscvlu
005100070724     d  Selected                      1A
005200070724
005300070724      /COPY QSYSINC/QRPGLESRC,QLGSORT
005400070724     d QLGKL                         16    DIM(MaxKey)
005500070724     d  QLGSP00                       9B 0 OVERLAY(QLGKL:00001)
005600070724     d  QLGSS00                       9B 0 OVERLAY(QLGKL:00005)
005700070724     d  QLGDT00                       9B 0 OVERLAY(QLGKL:00009)
005800070724     d  QLGSO00                       9B 0 OVERLAY(QLGKL:00013)
005900070724
006000070724      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
006100070724      /COPY QSYSINC/QRPGLESRC,QUSEC
006200061002     C*
006300950525     C* PULIZIA SUBFILE
006400070903     C                   SETON                                        21
006500070427     C                   WRITE     dg02c01
006600950525     C                   SETOFF                                       21
006700950525     C* CARICAMENTO SUBFILE
006800070724     C                   Z-ADD     0             NRR               4 0
006900070831     c                   if        d35tpi = 'C' or d35tpi = ' '
007000070724     C                   EXSR      RIES01c
007100070724     c                   end
007200070831     c                   if        d35tpi = 'R' or d35tpi = ' '
007300070724     C                   EXSR      RIES01r
007400070724     c                   end
007500070724     C                   Z-ADD     1             REC
007600070724     C                   Z-ADD     nrr           stmrec1
007700950525     C* SE NON E' STATO CARICATO NESSUN RECORD ESCO
007800950525     C     NRR           IFEQ      0
007900070528     c*                  eval      d35erro= '1'
008000070528     c*                  exsr      srfine
008100070528     c                   setoff                                       20
008200070528     c                   else
008300070528     c                   seton                                        20
008400070427     C                   end
008500070427     c* campi in testa
008600070427     c                   exsr      srtesta
008700070427     c*
008800950525     C* EMISSIONE FORMATO
008900070427     C                   WRITE     dg02T01
009000070427     C                   WRITE     dg02d02
009100061002     C                   do        *hival
009200070427     C                   EXFMT     dg02c01
009300950607     C** F3 - FINE
009400070116    1C                   IF        *inkc or *inkl
009500070831     c   kl              move      '12'          d35funO
009600070831     c   kc              move      '03'          d35funO
009700070516     c                   leave
009800950525    1C                   ENDIF
009900070903     C* F09 = Ordinamento x Giro
010000070903     c                   setoff                                       505152
010100070903if  1C                   IF        *INKI
010200070903     c                   seton                                        50
010300070903     C                   EXSR      GES_Ordina
010400070724e   1C                   ENDIF
010500070903     C* F10 = Ordinamento
010600070903if  1C                   IF        *INKJ
010700070903     c                   seton                                        51
010800070903     C                   EXSR      GES_Ordina
010900070903e   1C                   ENDIF
011000070903     C* F11 = Cambia ordinamento
011100070903if  1C                   IF        *INKK
011200070903     c                   seton                                        52
011300070903     C                   EXSR      GES_Ordina
011400070903e   1C                   ENDIF
011500070516     C* Controllo opzioni
011600070529     c                   if        nrr <> 0
011700070516     c                   EXSR      CTRS01
011800070529     c                   end
011900070516     c                   if        d35opzo <> ' '
012000070516     c                   leave
012100070516     c                   end
012200061002     c                   enddo
012300070516     c                   exsr      srfine
012400070427     c*
012500070112     C**********************************************************************
012600070427     C*--- CARICAMENTO dati ctl --------------------------------------*
012700070112     C**********************************************************************
012800070427     C     srtesta       BEGSR
012900070427     c     kdst8         chain     fidst08l
013000070427     c                   if        not %found(fidst08l)
013100070516     c                   eval      d35erro= '1'
013200070427     c                   exsr      srfine
013300070427     c                   end
013400070516     c                   eval      vfgs=d35fgsi
013500070927
013600070927     C*  Controlla Con il P.O. la data abilitazione
013700070927     c                   CLEAR                   Abilita_ORM       1
013800070927     C                   SETOFF                                       55
013900070927     C     vFGS          CHAIN     AZORG01l
014000070927    1C                   IF        %found(azorg01l)
014100070927     C                   MOVEL     ORGde7        og147
014200070927      * Abilitazione a trattare gli ORM
014300070927     c                   movel     §ogcGIO       Abilita_ORM
014400070927      * ORM attivati
014500070927     c                   if        Abilita_ORM <>*blank
014600070927     C                   SETON                                        55
014700070927     c                   end
014800070927     c                   end
014900070927
015000070516     c                   eval      vnfv = d35ndci
015100070516     c                   move      d35ddci       dtaiso
015200070427     c                   move      dtaiso        vdfv
015300070427     c                   eval      vpdr = dstpdr
015400070427     c     kapd          chain     fiapd01l
015500070427     c                   if        not %found(fiapd01l)
015600070427     c                   clear                   vdpdr
015700070427     c                   else
015800070427     c                   eval      vdpdr = apdrsc
015900070427     c                   end
016000070427     c                   eval      vpkl = dstpkl
016100070427     c                   eval      vvlu = dstvlu
016200070517     c                   eval      vnftl= dstnftl
016300070427     c                   eval      vpkla= dstpkla
016400070427     c                   eval      vvlua= dstvlua
016500070517     c                   eval      vnftla= dststpa
016600070427     c                   eval      vnbla= dstnbla
016700070724     c                   eval      vrpkl = dstrpkl
016800070724     c                   eval      vrvlu = dstrvlu
016900070724     c                   eval      vrnftl= dstrnftl
017000070724     c                   eval      vrpkla= dstrpkla
017100070724     c                   eval      vrvlua= dstrvlua
017200070724     c                   eval      vrnftla= dstrstpa
017300070724     c                   eval      vrnbla= dstrnbla
017400070427     c* mattina/pomeriggio
017500070427     c                   select
017600070427     c                   when      dstfpp = 'M'
017700070427     c                   eval      vdfpp = 'Mattina'
017800070427     c                   when      dstfpp = 'P'
017900070427     c                   eval      vdfpp = 'Pomeriggio'
018000070427     c                   endsl
018100070427     c                   endsr
018200070427     C**********************************************************************
018300070427     C*--- CARICAMENTO SUBFILE ---------------------------------------*
018400070427     C**********************************************************************
018500070724     C     RIES01c       BEGSR
018600070427     c*
018700070516     c                   clear                   vscopz
018800070724     c                   eval      vsctp = 'C'
018900070427     c     kdst          setll     fidst41l
019000070112     c                   do        *hival
019100070427     c     kdst          reade     fidst41l
019200070427     c                   if        %eof(fidst41l)
019300070112     c                   leave
019400070112     c                   end
019500070112     c*
019600070427     C                   eval      vscFGSG = DST4FGSG
019700070516     C                   eval      vsctass = DST4tass
019800070427     C                   eval      vsccgi  = DST4cgi
019900070427     C                   eval      vscprgg = DST4prgg
020000070427     C                   eval      vscsql  = DST4sql
020100070427     C                   eval      vscpkl  = DST4pkl
020200070427     C                   eval      vscvlu  = DST4vlu
020300070427     C                   eval      vscnbl  = DST4nbl
020400070427     c* decodifico il giro
020500070724     c                   clear                   fidg09ds
020600070724     c                   eval      d09itug = 'C'
020700070427     c                   exsr      srcgi
020800950525     C*
020900950525     C                   ADD       1             NRR
021000070427     C                   WRITE     dg02s01
021100950525    1C                   ENDDO
021200950525     C*
021300950525     C                   ENDSR
021400070724     C**********************************************************************
021500070724     C*--- CARICAMENTO SUBFILE ---------------------------------------*
021600070724     C**********************************************************************
021700070724     C     RIES01r       BEGSR
021800070724     c*
021900070724     c                   clear                   vscopz
022000070724     c                   eval      vsctp = 'R'
022100070724     c     kdst          setll     fidst61l
022200070724     c                   do        *hival
022300070724     c     kdst          reade     fidst61l
022400070724     c                   if        %eof(fidst61l)
022500070724     c                   leave
022600070724     c                   end
022700070724     c*
022800070724     C                   eval      vscFGSG = DST6FGSG
022900070724     C                   eval      vsctass = DST6tass
023000070724     C                   eval      vsccgi  = DST6cgi
023100070724     C                   eval      vscprgg = DST6prgg
023200070724     C                   eval      vscsql  = DST6sql
023300070724     C                   eval      vscpkl  = DST6pkl
023400070724     C                   eval      vscvlu  = DST6vlu
023500070724     C                   eval      vscnbl  = DST6nbl
023600070724     c* decodifico il giro
023700070724     c                   clear                   fidg09ds
023800070724     c                   eval      d09itug = 'R'
023900070724     c                   exsr      srcgi
024000070724     C*
024100070724     C                   ADD       1             NRR
024200070724     C                   WRITE     dg02s01
024300070724    1C                   ENDDO
024400070724     C*
024500070724     C                   ENDSR
024600070516     C**************************************************************************
024700070516     C* CONTROLLO DATI SUBFILE
024800070516     C**************************************************************************
024900070516     C     CTRS01        BEGSR
025000070516     C*
025100070516    0C                   do        *hival        nn                5 0
025200070516     C     nn            chain     dg02S01                            31
025300070516    1C   31              leave
025400070516     C* CONTROLLI
025500070516     C                   IF        vscopz = '1' or
025600070516     C                             vscopz = '2' or
025700070516     C                             vscopz = '3'
025800070516     C                   eval      d35opzo = vscopz
025900070516     C                   eval      d35tasso = vsctass
026000070516     C                   eval      d35fgsgo = vscfgsg
026100070516     C                   eval      d35cgio = vsccgi
026200070724     C                   eval      d35tpo = vsctp
026300070516     C                   leave
026400070516     c                   end
026500070516     c                   enddo
026600070516     C                   Z-ADD     1             REC
026700070516     C*
026800070516     C                   ENDSR
026900070427     C**************************************************************************
027000070427     C* controllo e decodifico giro
027100070427     C**************************************************************************
027200070427     C     Srcgi         BEGSR
027300070427     C*
027400070427     c                   eval      D09IOP0 = '001'
027500070427     c                   eval      d09ifgs = vscfgsg
027600070427     c                   eval      d09icgi = vsccgi
027700070516     c                   eval      d09idat = d35ddci
027800070427     c                   eval      kpjbu = fidg09ds
027900070427     c                   call      'FIDG09R'
028000070427     c                   parm                    kpjba
028100070427     c                   eval      fidg09ds = kpjbu
028200070427     c                   if        d09oerr = '1'
028300070427     c                   eval      vsdcgi = *all'?'
028400070427     c                   else
028500070427     c                   eval      vsdcgi = d09odes
028600070427     c                   end
028700070427     C*
028800070427     C                   ENDSR
028900070112     C**********************************************************************
029000070112     C*--- fine ------------------------------------------------------*
029100070112     C**********************************************************************
029200070112     C     srfine        BEGSR
029300070112     C*
029400070427     c                   movel     fidg35ds      kpjbu
029500070112     C                   SETON                                        LR
029600070112     c                   return
029700070112     C*
029800070112     C                   ENDSR
029900070724      *--------------------------------------------------------------------------------------------*
030000070831      * F11GES - TASTO FUNZIONE F11 --> Tipo C/R  o Giro
030100070724      *--------------------------------------------------------------------------------------------*
030200070903     C     GES_Ordina    BEGSR
030300070724      * IMPOSTO ENTRATA AD 'A'
030400070724     C                   EVAL      rec  = 1
030500070724      *
030600070724     C                   CLEAR                   QLGSCB
030700070724     C                   CLEAR                   QLGSCB00
030800070724
030900070724     ** 1 campo chiave.
031000070831     C                   EVAL      QLGNBRK    = 2
031100070724
031200070903     c                   select
031300070903
031400070903     C                   When      *in50
031500070903     ** il giro è a posizione 4, 10 byte, char, ascending.
031600070903     C                   EVAL      QLGSP      = 4
031700070903     C                   EVAL      QLGSS      = %SIZE(vsccgi)
031800070903     C                   EVAL      QLGDT      = Carattere
031900070903     C                   EVAL      QLGSO      = Ascendente
032000070903     C                   EVAL      QLGKL(1)   = QLGSKL
032100070903      *
032200070903     ** IL tipo pianificaz. è a posizione 2, alfa, 1 byte, ascending.
032300070903     C                   EVAL      QLGSP      = 2
032400070903     C                   EVAL      QLGSS      = %SIZE(vsctp)
032500070903     C                   EVAL      QLGDT      = carattere
032600070903     C                   EVAL      QLGSO      = Ascendente
032700070903     C                   EVAL      QLGKL(2)   = QLGSKL
032800070903      *
032900070903     C                   When      *in51
033000070724     ** IL tipo pianificaz. è a posizione 2, alfa, 1 byte, ascending.
033100070724     C                   EVAL      QLGSP      = 2
033200070724     C                   EVAL      QLGSS      = %SIZE(vsctp)
033300070724     C                   EVAL      QLGDT      = carattere
033400070724     C                   EVAL      QLGSO      = Ascendente
033500070724     C                   EVAL      QLGKL(1)   = QLGSKL
033600070831      *
033700070831     ** il giro è a posizione 4, 10 byte, char, ascending.
033800070831     C                   EVAL      QLGSP      = 4
033900070831     C                   EVAL      QLGSS      = %SIZE(vsccgi)
034000070831     C                   EVAL      QLGDT      = Carattere
034100070831     C                   EVAL      QLGSO      = Ascendente
034200070831     C                   EVAL      QLGKL(2)   = QLGSKL
034300070903      *
034400070903     C                   When      *in52
034500070903     ** IL tipo pianificaz. è a posizione 2, alfa, 1 byte, ascending.
034600070903     C                   EVAL      QLGSP      = 2
034700070903     C                   EVAL      QLGSS      = %SIZE(vsctp)
034800070903     C                   EVAL      QLGDT      = carattere
034900070903     C                   EVAL      QLGSO      = Discendente
035000070903     C                   EVAL      QLGKL(1)   = QLGSKL
035100070903      *
035200070903     ** il giro è a posizione 4, 10 byte, char, ascending.
035300070903     C                   EVAL      QLGSP      = 4
035400070903     C                   EVAL      QLGSS      = %SIZE(vsccgi)
035500070903     C                   EVAL      QLGDT      = Carattere
035600070903     C                   EVAL      QLGSO      = Ascendente
035700070903     C                   EVAL      QLGKL(2)   = QLGSKL
035800070831      *
035900070903     C                   ENDsl
036000070724
036100070724      * Load other sort parameters.
036200070724     C                   EVAL      QLGLB     = 80 + 16 * MaxKey
036300070724     C                   EVAL      QLGRL     = %SIZE(SflRcd) - 1
036400070724     C                   EVAL      QLGRT     = 8
036500070724     C                   EVAL      QLGOKL    = 80
036600070724     C                   EVAL      QLGLKE    = 16
036700070724     C                   EVAL      QLGLSS    = 290
036800070724
036900070724      * Initialize Sort I/O API fields.
037000070724     C                   EVAL      QLGRL00  = QLGRL
037100070724     C                   EVAL      QLGRC00  = 1
037200070724     C                   CLEAR                   QUSEI
037300070724     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
037400070724
037500070724      * First step - Initialize the sort routine.
037600070724     C                   CALL      'QLGSORT'
037700070724     C                   PARM                    QLGSCB
037800070724     C                   PARM                    NotUsed
037900070724     C                   PARM                    NotUsed
038000070724     C                   PARM                    SizeList
038100070724     C                   PARM                    ReturnSize
038200070724     C                   PARM                    QUSEC
038300070724
038400070724      * Next step - Write records to I/O routine.
038500070724     C                   EVAL      QLGRT00 = Put
038600070724
038700070724     C                   DO        stmrec1       x                 3 0
038800070724
038900070724     C     x             CHAIN     dg02s01
039000070724
039100070724     ** Solo le righe con Selected = 'Y' sono riordinate,
039200070724     ** quindi per fare un ordinamento di tutte le righe
039300070724     ** metto 'Y' sempre.
039400070724     C                   EVAL      Selected = 'Y'
039500070724
039600070724     C                   CLEAR                   QUSEI
039700070724     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
039800070724     C                   CALL      'QLGSRTIO'
039900070724     C                   PARM                    QLGSCB00
040000070724     C                   PARM                    SflRcd
040100070724     C                   PARM                    NotUsed
040200070724     C                   PARM                    SizeList
040300070724     C                   PARM                    NotUsed
040400070724     C                   PARM                    QUSEC
040500070724
040600070724     C                   ENDDO
040700070724
040800070724      * Next step - Signal end of input, clear subfile for reload.
040900070724     C                   EVAL      QLGRT00 = EndPut
041000070724     C                   CLEAR                   QUSEI
041100070724     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
041200070724
041300070724     C                   CALL      'QLGSRTIO'
041400070724     C                   PARM                    QLGSCB00
041500070724     C                   PARM                    SflRcd
041600070724     C                   PARM                    NotUsed
041700070724     C                   PARM                    SizeList
041800070724     C                   PARM                    NotUsed
041900070724     C                   PARM                    QUSEC
042000070724
042100070724     C                   SETON                                        21
042200070724     C                   WRITE     dg02c01
042300070724     C                   SETOFF                                       21
042400070724     C                   WRITE     dg02T01
042500070724     C                   WRITE     dg02d02
042600070724
042700070724      * Final step - Write the records back to the subfile.
042800070724     C                   EVAL      QLGRT00 = Get
042900070724
043000070724     C                   DO        stmrec1       x
043100070724     C                   CLEAR                   QUSEI
043200070724     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
043300070724     C                   CALL      'QLGSRTIO'
043400070724     C                   PARM                    QLGSCB00
043500070724     C                   PARM                    NotUsed
043600070724     C                   PARM                    SflRcd
043700070724     C                   PARM                    QLGRL00
043800070724     C                   PARM                    NotUsed
043900070724     C                   PARM                    QUSEC
044000070724      *
044100070724     c                   z-add     x             nrr
044200070724     C                   WRITE     dg02s01
044300070724     C                   ENDDO
044400070724      *
044500070724     C                   ENDSR
044600070112     C**********************************************************************
044700070112     C*--- INZSR INIZIALE --------------------------------------------*
044800070112     C**********************************************************************
044900070112     C     *INZSR        BEGSR
045000061002     C*
045100061002     C     *ENTRY        PLIST
045200061002     C                   PARM                    KPJBA
045300070112      * Reperisco dati job
045400070112     c                   exsr      DatiJob
045500070427     c                   eval      vdsa = rsut
045600070116     c*
045700070112     c                   if        kpjbu = ' '
045800070427     c                   clear                   fidg35ds
045900070112     c                   else
046000070427     C                   MOVEL     KPJBU         fidg35ds
046100070112     c                   end
046200070516     c                   clear                   d35opzo
046300070516     c                   clear                   d35tasso
046400070516     c                   clear                   d35fgsgo
046500070516     c                   clear                   d35cgio
046600070516     c                   clear                   d35erro
046700070516     c* abilito opzioni
046800070528     c     d35opzi       comp      'S'                                2323
046900061002
047000070427     c     Kdst          Klist
047100070516     c                   Kfld                    d35fgsi
047200070516     c                   Kfld                    d35npgi
047300070516     c                   Kfld                    d35ndci
047400070516     c                   Kfld                    d35ddci
047500070427     c     Kdst8         Klist
047600070516     c                   Kfld                    d35fgsi
047700070516     c                   Kfld                    d35npgi
047800070516     c                   Kfld                    d35ddci
047900070516     c                   Kfld                    d35ndci
048000070427     c     Kapd          Klist
048100070427     c                   Kfld                    dsttipa
048200070427     c                   Kfld                    dstpdr
048300061002     C                   ENDSR
048400061002      *---------------------------------------------------------------*
048500061002      * Reperimento Dati del job (Utente/Operativi)                   *
048600061002      *---------------------------------------------------------------*
048700061002     c     DatiJob       BEGSR
048800061002      *
048900061002     c     *dtaara       define    §azute        azuteds
049000061002     c     *dtaara       define    §datiute      ddatiute
049100061002      *
049200061002     c                   in(E)     *dtaara
049300061002     c                   IF        %ERROR or RSUT = *blanks
049400061002     c                   clear                   Tibs34Ds
049500061002     c                   call      'TIBS34R'
049600061002     c                   parm                    Tibs34Ds
049700061002     c                   in        *dtaara
049800061002     c                   ENDIF
049900061002      *
050000061002     c                   ENDSR
