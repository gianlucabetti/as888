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
009200071126     c                   setoff                                       99
009300070427     C                   EXFMT     dg02c01
009400950607     C** F3 - FINE
009500070116    1C                   IF        *inkc or *inkl
009600070831     c   kl              move      '12'          d35funO
009700070831     c   kc              move      '03'          d35funO
009800070516     c                   leave
009900950525    1C                   ENDIF
010000070903     C* F09 = Ordinamento x Giro
010100070903     c                   setoff                                       505152
010200070903if  1C                   IF        *INKI
010300070903     c                   seton                                        50
010400070903     C                   EXSR      GES_Ordina
010500070724e   1C                   ENDIF
010600070903     C* F10 = Ordinamento
010700070903if  1C                   IF        *INKJ
010800070903     c                   seton                                        51
010900070903     C                   EXSR      GES_Ordina
011000070903e   1C                   ENDIF
011100070903     C* F11 = Cambia ordinamento
011200070903if  1C                   IF        *INKK
011300070903     c                   seton                                        52
011400070903     C                   EXSR      GES_Ordina
011500070903e   1C                   ENDIF
011600070516     C* Controllo opzioni
011700070529     c                   if        nrr <> 0
011800070516     c                   EXSR      CTRS01
011900070529     c                   end
012000070516     c                   if        d35opzo <> ' '
012100070516     c                   leave
012200070516     c                   end
012300061002     c                   enddo
012400070516     c                   exsr      srfine
012500070427     c*
012600070112     C**********************************************************************
012700070427     C*--- CARICAMENTO dati ctl --------------------------------------*
012800070112     C**********************************************************************
012900070427     C     srtesta       BEGSR
013000070427     c     kdst8         chain     fidst08l
013100070427     c                   if        not %found(fidst08l)
013200070516     c                   eval      d35erro= '1'
013300070427     c                   exsr      srfine
013400070427     c                   end
013500070516     c                   eval      vfgs=d35fgsi
013600070927
013700070927     C*  Controlla Con il P.O. la data abilitazione
013800070927     c                   CLEAR                   Abilita_ORM       1
013900070927     C                   SETOFF                                       55
014000070927     C     vFGS          CHAIN     AZORG01l
014100070927    1C                   IF        %found(azorg01l)
014200070927     C                   MOVEL     ORGde7        og147
014300070927      * Abilitazione a trattare gli ORM
014400070927     c                   movel     §ogcGIO       Abilita_ORM
014500070927      * ORM attivati
014600070927     c                   if        Abilita_ORM <>*blank
014700070927     C                   SETON                                        55
014800070927     c                   end
014900070927     c                   end
015000070927
015100070516     c                   eval      vnfv = d35ndci
015200070516     c                   move      d35ddci       dtaiso
015300070427     c                   move      dtaiso        vdfv
015400070427     c                   eval      vpdr = dstpdr
015500070427     c     kapd          chain     fiapd01l
015600070427     c                   if        not %found(fiapd01l)
015700070427     c                   clear                   vdpdr
015800070427     c                   else
015900070427     c                   eval      vdpdr = apdrsc
016000070427     c                   end
016100070427     c                   eval      vpkl = dstpkl
016200070427     c                   eval      vvlu = dstvlu
016300070517     c                   eval      vnftl= dstnftl
016400070427     c                   eval      vpkla= dstpkla
016500070427     c                   eval      vvlua= dstvlua
016600070517     c                   eval      vnftla= dststpa
016700070427     c                   eval      vnbla= dstnbla
016800070724     c                   eval      vrpkl = dstrpkl
016900070724     c                   eval      vrvlu = dstrvlu
017000070724     c                   eval      vrnftl= dstrnftl
017100070724     c                   eval      vrpkla= dstrpkla
017200070724     c                   eval      vrvlua= dstrvlua
017300070724     c                   eval      vrnftla= dstrstpa
017400070724     c                   eval      vrnbla= dstrnbla
017500070427     c* mattina/pomeriggio
017600070427     c                   select
017700070427     c                   when      dstfpp = 'M'
017800070427     c                   eval      vdfpp = 'Mattina'
017900070427     c                   when      dstfpp = 'P'
018000070427     c                   eval      vdfpp = 'Pomeriggio'
018100070427     c                   endsl
018200070427     c                   endsr
018300070427     C**********************************************************************
018400070427     C*--- CARICAMENTO SUBFILE ---------------------------------------*
018500070427     C**********************************************************************
018600070724     C     RIES01c       BEGSR
018700070427     c*
018800070516     c                   clear                   vscopz
018900070724     c                   eval      vsctp = 'C'
019000070427     c     kdst          setll     fidst41l
019100070112     c                   do        *hival
019200070427     c     kdst          reade     fidst41l
019300070427     c                   if        %eof(fidst41l)
019400070112     c                   leave
019500070112     c                   end
019600070112     c*
019700070427     C                   eval      vscFGSG = DST4FGSG
019800070516     C                   eval      vsctass = DST4tass
019900070427     C                   eval      vsccgi  = DST4cgi
020000070427     C                   eval      vscprgg = DST4prgg
020100070427     C                   eval      vscsql  = DST4sql
020200070427     C                   eval      vscpkl  = DST4pkl
020300070427     C                   eval      vscvlu  = DST4vlu
020400070427     C                   eval      vscnbl  = DST4nbl
020500070427     c* decodifico il giro
020600070724     c                   clear                   fidg09ds
020700070724     c                   eval      d09itug = 'C'
020800070427     c                   exsr      srcgi
020900950525     C*
021000950525     C                   ADD       1             NRR
021100070427     C                   WRITE     dg02s01
021200950525    1C                   ENDDO
021300950525     C*
021400950525     C                   ENDSR
021500070724     C**********************************************************************
021600070724     C*--- CARICAMENTO SUBFILE ---------------------------------------*
021700070724     C**********************************************************************
021800070724     C     RIES01r       BEGSR
021900070724     c*
022000070724     c                   clear                   vscopz
022100070724     c                   eval      vsctp = 'R'
022200070724     c     kdst          setll     fidst61l
022300070724     c                   do        *hival
022400070724     c     kdst          reade     fidst61l
022500070724     c                   if        %eof(fidst61l)
022600070724     c                   leave
022700070724     c                   end
022800070724     c*
022900070724     C                   eval      vscFGSG = DST6FGSG
023000070724     C                   eval      vsctass = DST6tass
023100070724     C                   eval      vsccgi  = DST6cgi
023200070724     C                   eval      vscprgg = DST6prgg
023300070724     C                   eval      vscsql  = DST6sql
023400070724     C                   eval      vscpkl  = DST6pkl
023500070724     C                   eval      vscvlu  = DST6vlu
023600070724     C                   eval      vscnbl  = DST6nbl
023700070724     c* decodifico il giro
023800070724     c                   clear                   fidg09ds
023900070724     c                   eval      d09itug = 'R'
024000070724     c                   exsr      srcgi
024100070724     C*
024200070724     C                   ADD       1             NRR
024300070724     C                   WRITE     dg02s01
024400070724    1C                   ENDDO
024500070724     C*
024600070724     C                   ENDSR
024700070516     C**************************************************************************
024800070516     C* CONTROLLO DATI SUBFILE
024900070516     C**************************************************************************
025000070516     C     CTRS01        BEGSR
025100070516     C*
025200071126     C                   clear                   PiuDiUno          4 0
025300071126     C*
025400070516    0C                   do        *hival        nn                5 0
025500070516     C     nn            chain     dg02S01                            31
025600070516    1C   31              leave
025700070516     C* CONTROLLI
025800070516     C                   IF        vscopz = '1' or
025900070516     C                             vscopz = '2' or
026000070516     C                             vscopz = '3'
026100071126     C                   add       1             PiuDiUno
026200070516     C                   eval      d35opzo = vscopz
026300070516     C                   eval      d35tasso = vsctass
026400070516     C                   eval      d35fgsgo = vscfgsg
026500070516     C                   eval      d35cgio = vsccgi
026600070724     C                   eval      d35tpo = vsctp
026700071126      *
026800071126     C     PiuDiUno      comp      1                                  24
026900071126     c   24              clear                   d35opzo
027000071126     c   24              seton                                        99
027100071126     C                   update    dg02S01
027200071126      *
027300070516     c                   end
027400071126      *
027500070516     c                   enddo
027600070516     C                   Z-ADD     1             REC
027700070516     C*
027800070516     C                   ENDSR
027900070427     C**************************************************************************
028000070427     C* controllo e decodifico giro
028100070427     C**************************************************************************
028200070427     C     Srcgi         BEGSR
028300070427     C*
028400070427     c                   eval      D09IOP0 = '001'
028500070427     c                   eval      d09ifgs = vscfgsg
028600070427     c                   eval      d09icgi = vsccgi
028700070516     c                   eval      d09idat = d35ddci
028800070427     c                   eval      kpjbu = fidg09ds
028900070427     c                   call      'FIDG09R'
029000070427     c                   parm                    kpjba
029100070427     c                   eval      fidg09ds = kpjbu
029200070427     c                   if        d09oerr = '1'
029300070427     c                   eval      vsdcgi = *all'?'
029400070427     c                   else
029500070427     c                   eval      vsdcgi = d09odes
029600070427     c                   end
029700070427     C*
029800070427     C                   ENDSR
029900070112     C**********************************************************************
030000070112     C*--- fine ------------------------------------------------------*
030100070112     C**********************************************************************
030200070112     C     srfine        BEGSR
030300070112     C*
030400070427     c                   movel     fidg35ds      kpjbu
030500070112     C                   SETON                                        LR
030600070112     c                   return
030700070112     C*
030800070112     C                   ENDSR
030900070724      *--------------------------------------------------------------------------------------------*
031000070831      * F11GES - TASTO FUNZIONE F11 --> Tipo C/R  o Giro
031100070724      *--------------------------------------------------------------------------------------------*
031200070903     C     GES_Ordina    BEGSR
031300070724      * IMPOSTO ENTRATA AD 'A'
031400070724     C                   EVAL      rec  = 1
031500070724      *
031600070724     C                   CLEAR                   QLGSCB
031700070724     C                   CLEAR                   QLGSCB00
031800070724
031900070724     ** 1 campo chiave.
032000070831     C                   EVAL      QLGNBRK    = 2
032100070724
032200070903     c                   select
032300070903
032400070903     C                   When      *in50
032500070903     ** il giro è a posizione 4, 10 byte, char, ascending.
032600070903     C                   EVAL      QLGSP      = 4
032700070903     C                   EVAL      QLGSS      = %SIZE(vsccgi)
032800070903     C                   EVAL      QLGDT      = Carattere
032900070903     C                   EVAL      QLGSO      = Ascendente
033000070903     C                   EVAL      QLGKL(1)   = QLGSKL
033100070903      *
033200070903     ** IL tipo pianificaz. è a posizione 2, alfa, 1 byte, ascending.
033300070903     C                   EVAL      QLGSP      = 2
033400070903     C                   EVAL      QLGSS      = %SIZE(vsctp)
033500070903     C                   EVAL      QLGDT      = carattere
033600070903     C                   EVAL      QLGSO      = Ascendente
033700070903     C                   EVAL      QLGKL(2)   = QLGSKL
033800070903      *
033900070903     C                   When      *in51
034000070724     ** IL tipo pianificaz. è a posizione 2, alfa, 1 byte, ascending.
034100070724     C                   EVAL      QLGSP      = 2
034200070724     C                   EVAL      QLGSS      = %SIZE(vsctp)
034300070724     C                   EVAL      QLGDT      = carattere
034400070724     C                   EVAL      QLGSO      = Ascendente
034500070724     C                   EVAL      QLGKL(1)   = QLGSKL
034600070831      *
034700070831     ** il giro è a posizione 4, 10 byte, char, ascending.
034800070831     C                   EVAL      QLGSP      = 4
034900070831     C                   EVAL      QLGSS      = %SIZE(vsccgi)
035000070831     C                   EVAL      QLGDT      = Carattere
035100070831     C                   EVAL      QLGSO      = Ascendente
035200070831     C                   EVAL      QLGKL(2)   = QLGSKL
035300070903      *
035400070903     C                   When      *in52
035500070903     ** IL tipo pianificaz. è a posizione 2, alfa, 1 byte, ascending.
035600070903     C                   EVAL      QLGSP      = 2
035700070903     C                   EVAL      QLGSS      = %SIZE(vsctp)
035800070903     C                   EVAL      QLGDT      = carattere
035900070903     C                   EVAL      QLGSO      = Discendente
036000070903     C                   EVAL      QLGKL(1)   = QLGSKL
036100070903      *
036200070903     ** il giro è a posizione 4, 10 byte, char, ascending.
036300070903     C                   EVAL      QLGSP      = 4
036400070903     C                   EVAL      QLGSS      = %SIZE(vsccgi)
036500070903     C                   EVAL      QLGDT      = Carattere
036600070903     C                   EVAL      QLGSO      = Ascendente
036700070903     C                   EVAL      QLGKL(2)   = QLGSKL
036800070831      *
036900070903     C                   ENDsl
037000070724
037100070724      * Load other sort parameters.
037200070724     C                   EVAL      QLGLB     = 80 + 16 * MaxKey
037300070724     C                   EVAL      QLGRL     = %SIZE(SflRcd) - 1
037400070724     C                   EVAL      QLGRT     = 8
037500070724     C                   EVAL      QLGOKL    = 80
037600070724     C                   EVAL      QLGLKE    = 16
037700070724     C                   EVAL      QLGLSS    = 290
037800070724
037900070724      * Initialize Sort I/O API fields.
038000070724     C                   EVAL      QLGRL00  = QLGRL
038100070724     C                   EVAL      QLGRC00  = 1
038200070724     C                   CLEAR                   QUSEI
038300070724     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
038400070724
038500070724      * First step - Initialize the sort routine.
038600070724     C                   CALL      'QLGSORT'
038700070724     C                   PARM                    QLGSCB
038800070724     C                   PARM                    NotUsed
038900070724     C                   PARM                    NotUsed
039000070724     C                   PARM                    SizeList
039100070724     C                   PARM                    ReturnSize
039200070724     C                   PARM                    QUSEC
039300070724
039400070724      * Next step - Write records to I/O routine.
039500070724     C                   EVAL      QLGRT00 = Put
039600070724
039700070724     C                   DO        stmrec1       x                 3 0
039800070724
039900070724     C     x             CHAIN     dg02s01
040000070724
040100070724     ** Solo le righe con Selected = 'Y' sono riordinate,
040200070724     ** quindi per fare un ordinamento di tutte le righe
040300070724     ** metto 'Y' sempre.
040400070724     C                   EVAL      Selected = 'Y'
040500070724
040600070724     C                   CLEAR                   QUSEI
040700070724     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
040800070724     C                   CALL      'QLGSRTIO'
040900070724     C                   PARM                    QLGSCB00
041000070724     C                   PARM                    SflRcd
041100070724     C                   PARM                    NotUsed
041200070724     C                   PARM                    SizeList
041300070724     C                   PARM                    NotUsed
041400070724     C                   PARM                    QUSEC
041500070724
041600070724     C                   ENDDO
041700070724
041800070724      * Next step - Signal end of input, clear subfile for reload.
041900070724     C                   EVAL      QLGRT00 = EndPut
042000070724     C                   CLEAR                   QUSEI
042100070724     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
042200070724
042300070724     C                   CALL      'QLGSRTIO'
042400070724     C                   PARM                    QLGSCB00
042500070724     C                   PARM                    SflRcd
042600070724     C                   PARM                    NotUsed
042700070724     C                   PARM                    SizeList
042800070724     C                   PARM                    NotUsed
042900070724     C                   PARM                    QUSEC
043000070724
043100070724     C                   SETON                                        21
043200070724     C                   WRITE     dg02c01
043300070724     C                   SETOFF                                       21
043400070724     C                   WRITE     dg02T01
043500070724     C                   WRITE     dg02d02
043600070724
043700070724      * Final step - Write the records back to the subfile.
043800070724     C                   EVAL      QLGRT00 = Get
043900070724
044000070724     C                   DO        stmrec1       x
044100070724     C                   CLEAR                   QUSEI
044200070724     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
044300070724     C                   CALL      'QLGSRTIO'
044400070724     C                   PARM                    QLGSCB00
044500070724     C                   PARM                    NotUsed
044600070724     C                   PARM                    SflRcd
044700070724     C                   PARM                    QLGRL00
044800070724     C                   PARM                    NotUsed
044900070724     C                   PARM                    QUSEC
045000070724      *
045100070724     c                   z-add     x             nrr
045200070724     C                   WRITE     dg02s01
045300070724     C                   ENDDO
045400070724      *
045500070724     C                   ENDSR
045600070112     C**********************************************************************
045700070112     C*--- INZSR INIZIALE --------------------------------------------*
045800070112     C**********************************************************************
045900070112     C     *INZSR        BEGSR
046000061002     C*
046100061002     C     *ENTRY        PLIST
046200061002     C                   PARM                    KPJBA
046300070112      * Reperisco dati job
046400070112     c                   exsr      DatiJob
046500070427     c                   eval      vdsa = rsut
046600070116     c*
046700070112     c                   if        kpjbu = ' '
046800070427     c                   clear                   fidg35ds
046900070112     c                   else
047000070427     C                   MOVEL     KPJBU         fidg35ds
047100070112     c                   end
047200070516     c                   clear                   d35opzo
047300070516     c                   clear                   d35tasso
047400070516     c                   clear                   d35fgsgo
047500070516     c                   clear                   d35cgio
047600070516     c                   clear                   d35erro
047700070516     c* abilito opzioni
047800070528     c     d35opzi       comp      'S'                                2323
047900061002
048000070427     c     Kdst          Klist
048100070516     c                   Kfld                    d35fgsi
048200070516     c                   Kfld                    d35npgi
048300070516     c                   Kfld                    d35ndci
048400070516     c                   Kfld                    d35ddci
048500070427     c     Kdst8         Klist
048600070516     c                   Kfld                    d35fgsi
048700070516     c                   Kfld                    d35npgi
048800070516     c                   Kfld                    d35ddci
048900070516     c                   Kfld                    d35ndci
049000070427     c     Kapd          Klist
049100070427     c                   Kfld                    dsttipa
049200070427     c                   Kfld                    dstpdr
049300061002     C                   ENDSR
049400061002      *---------------------------------------------------------------*
049500061002      * Reperimento Dati del job (Utente/Operativi)                   *
049600061002      *---------------------------------------------------------------*
049700061002     c     DatiJob       BEGSR
049800061002      *
049900061002     c     *dtaara       define    §azute        azuteds
050000061002     c     *dtaara       define    §datiute      ddatiute
050100061002      *
050200061002     c                   in(E)     *dtaara
050300061002     c                   IF        %ERROR or RSUT = *blanks
050400061002     c                   clear                   Tibs34Ds
050500061002     c                   call      'TIBS34R'
050600061002     c                   parm                    Tibs34Ds
050700061002     c                   in        *dtaara
050800061002     c                   ENDIF
050900061002      *
051000061002     c                   ENDSR
