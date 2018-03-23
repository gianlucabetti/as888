000100950523     H DECEDIT('0,') DATEDIT(*DMY.)
000200070427     H* FIDG35R *-----------------------------------------------------*
000300070427     H*     Visualizza valori assegnati in distinta x GIRO
000400930225     H*---------------------------------------------------------------*
000500070427     FFidg35D   CF   E             WORKSTN
000600070427     F                                     SFILE(dg02S01:NRR)
000700070427     Ffidst08L  IF   E           K DISK
000800070427     Ffidst41L  IF   E           K DISK
000801070724     Ffidst61L  IF   E           K DISK
000900070427     Ffiapd01L  IF   E           K DISK
001000030724     d dtaiso          s               d
001100030724     d dtaeur          s               d   datfmt(*eur)
001200070427     D fidg35ds      E DS
001300070427     D fidg09ds      E DS
001400900514     D KPJBA         E DS
001500061002     d* Parametri x Controllo profilo utenti
001600061002     d TIBS34ds      e ds
001700061002     d AZUTEds       e ds                  extname(AZUTE00F)
001800061002     d* Ds per dati organigramma
001900061002     d DDatiUte      e ds
001901070724     d NotUsed         S             16A
001902070724     d ReturnSize      S              9B 0
001903070724     d SizeList        S              9B 0
001904070724     d WrkSort         S              1
001905070724     d*---
001906070724     d Rec1            S              3  0                                      *n° rel sf
001907070724     d stmrec1         S              3  0                                      *n° rel sf x stampa
001908070724     d*---
001909070724     d MaxKey          C                   2
001910070724     d Ascendente      C                   1
001911070724     d Discendente     C                   2
001912070724     d Carattere       C                   6
001913070724     d Put             C                   1
001914070724     d EndPut          C                   2
001915070724     d Get             C                   3
001916070724     d Numerico        C                   8
001917070724     d*---
001918070724     d SflRcd          DS
001919070724     d  vscopz
001920070724     d  vsctp
001921070724     d  vsctass
001922070724     d  vsccgi
001923070724     d  vsdcgi
001924070724     d  vscprgg
001925070724     d  vscsql
001926070724     d  vscnbl
001927070724     d  vscpkl
001928070724     d  vscvlu
001929070724     d  Selected                      1A
001930070724
001931070724      /COPY QSYSINC/QRPGLESRC,QLGSORT
001932070724     d QLGKL                         16    DIM(MaxKey)
001933070724     d  QLGSP00                       9B 0 OVERLAY(QLGKL:00001)
001934070724     d  QLGSS00                       9B 0 OVERLAY(QLGKL:00005)
001935070724     d  QLGDT00                       9B 0 OVERLAY(QLGKL:00009)
001936070724     d  QLGSO00                       9B 0 OVERLAY(QLGKL:00013)
001937070724
001938070724      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
001939070724      /COPY QSYSINC/QRPGLESRC,QUSEC
002000061002     C*
002100950525     C* PULIZIA SUBFILE
002200070724     C                   SETON                                        2150
002300070427     C                   WRITE     dg02c01
002400950525     C                   SETOFF                                       21
002500950525     C* CARICAMENTO SUBFILE
002501070724     C                   Z-ADD     0             NRR               4 0
002502070724     c                   if        d35tpi = 'C' or d35tpi = 'E'
002600070724     C                   EXSR      RIES01c
002601070724     c                   end
002602070724     c                   if        d35tpi = 'R' or d35tpi = 'E'
002603070724     C                   EXSR      RIES01r
002604070724     c                   end
002605070724     C                   Z-ADD     1             REC
002606070724     C                   Z-ADD     nrr           stmrec1
002700950525     C* SE NON E' STATO CARICATO NESSUN RECORD ESCO
002800950525     C     NRR           IFEQ      0
002900070528     c*                  eval      d35erro= '1'
003000070528     c*                  exsr      srfine
003100070528     c                   setoff                                       20
003200070528     c                   else
003300070528     c                   seton                                        20
003400070427     C                   end
003500070427     c* campi in testa
003600070427     c                   exsr      srtesta
003700070427     c*
003800950525     C* EMISSIONE FORMATO
003900070427     C                   WRITE     dg02T01
004000070427     C                   WRITE     dg02d02
004100061002     C                   do        *hival
004200070427     C                   EXFMT     dg02c01
004300950607     C** F3 - FINE
004400070116    1C                   IF        *inkc or *inkl
004500070516     c                   leave
004600950525    1C                   ENDIF
004601070724     C* F11 = Cambia ordinamento
004602070724if  1C                   IF        *INKK
004603070724     C                   EXSR      F11GES
004604070724e   1C                   ENDIF
004700070516     C* Controllo opzioni
004800070529     c                   if        nrr <> 0
004900070516     c                   EXSR      CTRS01
005000070529     c                   end
005100070516     c                   if        d35opzo <> ' '
005200070516     c                   leave
005300070516     c                   end
005400061002     c                   enddo
005500070516     c                   exsr      srfine
005600070427     c*
005700070112     C**********************************************************************
005800070427     C*--- CARICAMENTO dati ctl --------------------------------------*
005900070112     C**********************************************************************
006000070427     C     srtesta       BEGSR
006100070427     c     kdst8         chain     fidst08l
006200070427     c                   if        not %found(fidst08l)
006300070516     c                   eval      d35erro= '1'
006400070427     c                   exsr      srfine
006500070427     c                   end
006600070516     c                   eval      vfgs=d35fgsi
006700070516     c                   eval      vnfv = d35ndci
006800070516     c                   move      d35ddci       dtaiso
006900070427     c                   move      dtaiso        vdfv
007000070427     c                   eval      vpdr = dstpdr
007100070427     c     kapd          chain     fiapd01l
007200070427     c                   if        not %found(fiapd01l)
007300070427     c                   clear                   vdpdr
007400070427     c                   else
007500070427     c                   eval      vdpdr = apdrsc
007600070427     c                   end
007700070427     c                   eval      vpkl = dstpkl
007800070427     c                   eval      vvlu = dstvlu
007900070517     c                   eval      vnftl= dstnftl
008000070427     c                   eval      vpkla= dstpkla
008100070427     c                   eval      vvlua= dstvlua
008200070517     c                   eval      vnftla= dststpa
008300070427     c                   eval      vnbla= dstnbla
008301070724     c                   eval      vrpkl = dstrpkl
008302070724     c                   eval      vrvlu = dstrvlu
008303070724     c                   eval      vrnftl= dstrnftl
008304070724     c                   eval      vrpkla= dstrpkla
008305070724     c                   eval      vrvlua= dstrvlua
008306070724     c                   eval      vrnftla= dstrstpa
008307070724     c                   eval      vrnbla= dstrnbla
008400070427     c* mattina/pomeriggio
008500070427     c                   select
008600070427     c                   when      dstfpp = 'M'
008700070427     c                   eval      vdfpp = 'Mattina'
008800070427     c                   when      dstfpp = 'P'
008900070427     c                   eval      vdfpp = 'Pomeriggio'
009000070427     c                   endsl
009100070427     c                   endsr
009200070427     C**********************************************************************
009300070427     C*--- CARICAMENTO SUBFILE ---------------------------------------*
009400070427     C**********************************************************************
009500070724     C     RIES01c       BEGSR
009600070427     c*
009700070516     c                   clear                   vscopz
009701070724     c                   eval      vsctp = 'C'
009900070427     c     kdst          setll     fidst41l
010000070112     c                   do        *hival
010100070427     c     kdst          reade     fidst41l
010200070427     c                   if        %eof(fidst41l)
010300070112     c                   leave
010400070112     c                   end
010500070112     c*
010600070427     C                   eval      vscFGSG = DST4FGSG
010700070516     C                   eval      vsctass = DST4tass
010800070427     C                   eval      vsccgi  = DST4cgi
010900070427     C                   eval      vscprgg = DST4prgg
011000070427     C                   eval      vscsql  = DST4sql
011100070427     C                   eval      vscpkl  = DST4pkl
011200070427     C                   eval      vscvlu  = DST4vlu
011300070427     C                   eval      vscnbl  = DST4nbl
011400070427     c* decodifico il giro
011401070724     c                   clear                   fidg09ds
011402070724     c                   eval      d09itug = 'C'
011500070427     c                   exsr      srcgi
011600950525     C*
011700950525     C                   ADD       1             NRR
011800070427     C                   WRITE     dg02s01
011900950525    1C                   ENDDO
012000950525     C*
012200950525     C                   ENDSR
012201070724     C**********************************************************************
012202070724     C*--- CARICAMENTO SUBFILE ---------------------------------------*
012203070724     C**********************************************************************
012204070724     C     RIES01r       BEGSR
012205070724     c*
012206070724     c                   clear                   vscopz
012207070724     c                   eval      vsctp = 'R'
012208070724     c     kdst          setll     fidst61l
012209070724     c                   do        *hival
012210070724     c     kdst          reade     fidst61l
012211070724     c                   if        %eof(fidst61l)
012212070724     c                   leave
012213070724     c                   end
012214070724     c*
012215070724     C                   eval      vscFGSG = DST6FGSG
012216070724     C                   eval      vsctass = DST6tass
012217070724     C                   eval      vsccgi  = DST6cgi
012218070724     C                   eval      vscprgg = DST6prgg
012219070724     C                   eval      vscsql  = DST6sql
012220070724     C                   eval      vscpkl  = DST6pkl
012221070724     C                   eval      vscvlu  = DST6vlu
012222070724     C                   eval      vscnbl  = DST6nbl
012223070724     c* decodifico il giro
012224070724     c                   clear                   fidg09ds
012225070724     c                   eval      d09itug = 'R'
012226070724     c                   exsr      srcgi
012227070724     C*
012228070724     C                   ADD       1             NRR
012229070724     C                   WRITE     dg02s01
012230070724    1C                   ENDDO
012231070724     C*
012232070724     C                   ENDSR
012300070516     C**************************************************************************
012400070516     C* CONTROLLO DATI SUBFILE
012500070516     C**************************************************************************
012600070516     C     CTRS01        BEGSR
012700070516     C*
012800070516    0C                   do        *hival        nn                5 0
012900070516     C     nn            chain     dg02S01                            31
013000070516    1C   31              leave
013100070516     C* CONTROLLI
013200070516     C                   IF        vscopz = '1' or
013300070516     C                             vscopz = '2' or
013400070516     C                             vscopz = '3'
013500070516     C                   eval      d35opzo = vscopz
013600070516     C                   eval      d35tasso = vsctass
013700070516     C                   eval      d35fgsgo = vscfgsg
013800070516     C                   eval      d35cgio = vsccgi
013801070724     C                   eval      d35tpo = vsctp
013900070516     C                   leave
014000070516     c                   end
014100070516     c                   enddo
014200070516     C                   Z-ADD     1             REC
014300070516     C*
014400070516     C                   ENDSR
014500070427     C**************************************************************************
014600070427     C* controllo e decodifico giro
014700070427     C**************************************************************************
014800070427     C     Srcgi         BEGSR
014900070427     C*
015100070427     c                   eval      D09IOP0 = '001'
015200070427     c                   eval      d09ifgs = vscfgsg
015300070427     c                   eval      d09icgi = vsccgi
015400070516     c                   eval      d09idat = d35ddci
015600070427     c                   eval      kpjbu = fidg09ds
015700070427     c                   call      'FIDG09R'
015800070427     c                   parm                    kpjba
015900070427     c                   eval      fidg09ds = kpjbu
016000070427     c                   if        d09oerr = '1'
016100070427     c                   eval      vsdcgi = *all'?'
016200070427     c                   else
016300070427     c                   eval      vsdcgi = d09odes
016400070427     c                   end
016500070427     C*
016600070427     C                   ENDSR
016700070112     C**********************************************************************
016800070112     C*--- fine ------------------------------------------------------*
016900070112     C**********************************************************************
017000070112     C     srfine        BEGSR
017100070112     C*
017200070427     c                   movel     fidg35ds      kpjbu
017300070112     C                   SETON                                        LR
017400070112     c                   return
017500070112     C*
017600070112     C                   ENDSR
017601070724      *--------------------------------------------------------------------------------------------*
017602070724      * F11GES - TASTO FUNZIONE F11 --> Ragione sociale/P.O
017603070724      *--------------------------------------------------------------------------------------------*
017604070724     C     F11GES        BEGSR
017607070724      * IMPOSTO ENTRATA AD 'A'
017608070724     C                   EVAL      rec  = 1
017609070724      *
017610070724     C                   CLEAR                   QLGSCB
017611070724     C                   CLEAR                   QLGSCB00
017612070724
017613070724     ** 1 campo chiave.
017614070724     C                   EVAL      QLGNBRK    = 1
017615070724
017616070724     C                   IF        *in50=*off
017617070724     ** IL tipo pianificaz. è a posizione 2, alfa, 1 byte, ascending.
017618070724     C                   EVAL      QLGSP      = 2
017619070724     C                   EVAL      QLGSS      = %SIZE(vsctp)
017620070724     C                   EVAL      QLGDT      = carattere
017621070724     C                   EVAL      QLGSO      = Ascendente
017622070724     C                   EVAL      QLGKL(1)   = QLGSKL
017623070724     C                   seton                                        50
017624070724     C                   ELSE
017625070724      *
017626070724     ** il giro è a posizione 4, 10 byte, char, ascending.
017627070724     C                   EVAL      QLGSP      = 4
017628070724     C                   EVAL      QLGSS      = %SIZE(vsccgi)
017629070724     C                   EVAL      QLGDT      = Carattere
017630070724     C                   EVAL      QLGSO      = Ascendente
017631070724     C                   EVAL      QLGKL(1)   = QLGSKL
017632070724     C                   setoff                                       50
017633070724     C                   ENDIF
017634070724
017635070724      * Load other sort parameters.
017636070724     C                   EVAL      QLGLB     = 80 + 16 * MaxKey
017637070724     C                   EVAL      QLGRL     = %SIZE(SflRcd) - 1
017638070724     C                   EVAL      QLGRT     = 8
017639070724     C                   EVAL      QLGOKL    = 80
017640070724     C                   EVAL      QLGLKE    = 16
017641070724     C                   EVAL      QLGLSS    = 290
017642070724
017643070724      * Initialize Sort I/O API fields.
017644070724     C                   EVAL      QLGRL00  = QLGRL
017645070724     C                   EVAL      QLGRC00  = 1
017646070724     C                   CLEAR                   QUSEI
017647070724     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
017648070724
017649070724      * First step - Initialize the sort routine.
017650070724     C                   CALL      'QLGSORT'
017651070724     C                   PARM                    QLGSCB
017652070724     C                   PARM                    NotUsed
017653070724     C                   PARM                    NotUsed
017654070724     C                   PARM                    SizeList
017655070724     C                   PARM                    ReturnSize
017656070724     C                   PARM                    QUSEC
017657070724
017658070724      * Next step - Write records to I/O routine.
017659070724     C                   EVAL      QLGRT00 = Put
017660070724
017661070724     C                   DO        stmrec1       x                 3 0
017662070724
017663070724     C     x             CHAIN     dg02s01
017664070724
017665070724     ** Solo le righe con Selected = 'Y' sono riordinate,
017666070724     ** quindi per fare un ordinamento di tutte le righe
017667070724     ** metto 'Y' sempre.
017668070724     C                   EVAL      Selected = 'Y'
017669070724
017670070724     C                   CLEAR                   QUSEI
017671070724     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
017672070724     C                   CALL      'QLGSRTIO'
017673070724     C                   PARM                    QLGSCB00
017674070724     C                   PARM                    SflRcd
017675070724     C                   PARM                    NotUsed
017676070724     C                   PARM                    SizeList
017677070724     C                   PARM                    NotUsed
017678070724     C                   PARM                    QUSEC
017679070724
017680070724     C                   ENDDO
017681070724
017682070724      * Next step - Signal end of input, clear subfile for reload.
017683070724     C                   EVAL      QLGRT00 = EndPut
017684070724     C                   CLEAR                   QUSEI
017685070724     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
017686070724
017687070724     C                   CALL      'QLGSRTIO'
017688070724     C                   PARM                    QLGSCB00
017689070724     C                   PARM                    SflRcd
017690070724     C                   PARM                    NotUsed
017691070724     C                   PARM                    SizeList
017692070724     C                   PARM                    NotUsed
017693070724     C                   PARM                    QUSEC
017694070724
017695070724     C                   SETON                                        21
017696070724     C                   WRITE     dg02c01
017697070724     C                   SETOFF                                       21
017698070724     C                   WRITE     dg02T01
017699070724     C                   WRITE     dg02d02
017703070724
017704070724      * Final step - Write the records back to the subfile.
017705070724     C                   EVAL      QLGRT00 = Get
017706070724
017707070724     C                   DO        stmrec1       x
017708070724     C                   CLEAR                   QUSEI
017709070724     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
017710070724     C                   CALL      'QLGSRTIO'
017711070724     C                   PARM                    QLGSCB00
017712070724     C                   PARM                    NotUsed
017713070724     C                   PARM                    SflRcd
017714070724     C                   PARM                    QLGRL00
017715070724     C                   PARM                    NotUsed
017716070724     C                   PARM                    QUSEC
017717070724      *
017718070724     c                   z-add     x             nrr
017719070724     C                   WRITE     dg02s01
017720070724     C                   ENDDO
017723070724      *
017724070724     C                   ENDSR
017725070112     C**********************************************************************
017800070112     C*--- INZSR INIZIALE --------------------------------------------*
017900070112     C**********************************************************************
018000070112     C     *INZSR        BEGSR
018100061002     C*
018200061002     C     *ENTRY        PLIST
018300061002     C                   PARM                    KPJBA
018400070112      * Reperisco dati job
018500070112     c                   exsr      DatiJob
018600070427     c                   eval      vdsa = rsut
018700070116     c*
018800070112     c                   if        kpjbu = ' '
018900070427     c                   clear                   fidg35ds
019000070112     c                   else
019100070427     C                   MOVEL     KPJBU         fidg35ds
019200070112     c                   end
019300070516     c                   clear                   d35opzo
019400070516     c                   clear                   d35tasso
019500070516     c                   clear                   d35fgsgo
019600070516     c                   clear                   d35cgio
019700070516     c                   clear                   d35erro
019800070516     c* abilito opzioni
019900070528     c     d35opzi       comp      'S'                                2323
020000061002
020100070427     c     Kdst          Klist
020200070516     c                   Kfld                    d35fgsi
020300070516     c                   Kfld                    d35npgi
020400070516     c                   Kfld                    d35ndci
020500070516     c                   Kfld                    d35ddci
020600070427     c     Kdst8         Klist
020700070516     c                   Kfld                    d35fgsi
020800070516     c                   Kfld                    d35npgi
020900070516     c                   Kfld                    d35ddci
021000070516     c                   Kfld                    d35ndci
021100070427     c     Kapd          Klist
021200070427     c                   Kfld                    dsttipa
021300070427     c                   Kfld                    dstpdr
021400061002     C                   ENDSR
021500061002      *---------------------------------------------------------------*
021600061002      * Reperimento Dati del job (Utente/Operativi)                   *
021700061002      *---------------------------------------------------------------*
021800061002     c     DatiJob       BEGSR
021900061002      *
022000061002     c     *dtaara       define    §azute        azuteds
022100061002     c     *dtaara       define    §datiute      ddatiute
022200061002      *
022300061002     c                   in(E)     *dtaara
022400061002     c                   IF        %ERROR or RSUT = *blanks
022500061002     c                   clear                   Tibs34Ds
022600061002     c                   call      'TIBS34R'
022700061002     c                   parm                    Tibs34Ds
022800061002     c                   in        *dtaara
022900061002     c                   ENDIF
023000061002      *
023100061002     c                   ENDSR
