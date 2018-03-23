000100030701     H DECEDIT('0,') DATEDIT(*ymd.)
000200020419      * FNLR72R *----------------------------------------------------*
000300020419      *         - RISTAMPA BOLLE PARTENZE ALL'ARRIVO
000400020419      *--------------------------------------------------------------*
000500941227     FFNLR72D   CF   E             WORKSTN
000600910207     FAZORG01L  IF   E           K DISK
000700920422     FTABEL00F  IF   E           K DISK
000800941227     FFNARB01L  IF   E           K DISK
000900060216     FFiAR401L  IF   E           K DISK
001000030410     fFnart27l  if   e           k Disk
001001081028     fFnbrv07l  if   e           k Disk
001100020419
001200030506     d Msg             s             78    Dim(5) Ctdata Perrcd(1)
001300050420     D £6              S              3  0 DIM(30)                              FIL GESTITE
001301081028     D NPS             S              2  0 DIM(101)
001400030506
001500030410     d Dataiso         s               d   datfmt(*iso)
001600030410     d Dataoggi        s              8  0
001700030410     d Datascad        s              8  0
001800020419     d wa              s              2  0
001900030410     d wgiorni         s              3  0
002000050419     d wnoter          s              1
002100070424     d woggi           s              8
002200080414     d waltra_a        s              1
002201081028     d xy              s              4  0
002202081028     d W_pstreale      s              1
002300020419
002400900523     D PARAM           DS
002500910207     D  VIDLNP                11     13  0
002600941227     D  DSD                   14     21  0
002700941227     D  DSA                   22     29  0
002800941227     D  VIDSED                30     31  0
002900941227     D  VIDNSD                32     38  0
003000941227     D  VIDAAS                39     42  0
003100941227     D  VIDSEA                43     44  0
003200941227     D  VIDNSA                45     51  0
003300941227     D  VIDLNA                52     54  0
003400970715     D  TIRIS                 55     55
003500971219     D  VIDSDI                56     56
003600980407     D  VIDDDT                57     57
003700030410     d  VidFls                58     60  0
003800030410     d  VidNrs                61     62  0
003900030410     d  VidNsc                63     69  0
004000030410     d  VidLnas               70     72  0
004100070419     d  wlnadstauto           73     73
004200030612     d* - parametri da TRUL90R:
004300030612     d* · pgm.   per stampa LDV
004400030612     d  wD90psl              157    166
004500030612     d* · stampante per LDV in formato "A4" (laser)
004600030612     d  wD90prb4             167    176
004700030612     d* · stampante per LDV in formato "A5" (laser)
004800030612     d  wD90prb5             177    186
004900030612     d* · modulo per stampa LDV su "A4"     (laser)
005000030612     d  wD90mdb4             187    196
005100030612     d* · modulo per stampa LDV su "A5"     (laser)
005200030612     d  wD90mdb5             197    206
005300030612     d* · pgm di stampa etichette segnacolli
005400030612     d  wD90pss              207    216
005500030612     d* · modulo per stampa bolle/fatture   (ad aghi)
005600030612     d  wD90mdb              217    226
005700030612     d* · modulo per stampa etichette
005800030612     d  wD90mde              227    236
005900030612     d* · stampante per bolle/fatture       (ad aghi)
006000030612     d  wD90prb              237    246
006100030612     d* · stampante per etichette
006200030612     d  wD90pre              247    256
006300030612      *
006400941227     D WLBDAT          DS
006500941227     D  G02DAT                 1      8  0
006600941227     D  G02INV                 9     16  0
006700941227     D  G02ERR                17     17
006800941227     D  G02TGI                18     22  0
006900920422     D* RICHIAMO PGM DI INTERROGAZIONE BOLLE ARRIVI
007000920422     D PARAM2          DS
007100990204     D  PA1AAS                14     17  0
007200990204     D  PA1LNP                18     20  0
007300990204     D  PA1NRS                21     22  0
007400990204     D  PA1NSP                23     29  0
007500941227     D  VI2FLG                31     31
007600030612      * Parametri per stampa BOLLE ed ETICHETTE
007700030612     d TRUL90DS      e ds                  inz
007800030612     d   D90rse      e                     inz('S')
007900030630     d   D90rsb      e                     inz('L')
008000050420      * Parametri per richiamo a TRUL09R
008100050420     d Trul09ds      e ds
008200050420     d  TFA                   19    108  0 dim(30)
008300030612      *
008400990203     D DS3A          E DS
008500990203     D DS3C          E DS
008600020207     D OG143         E DS
008700070419     d og146         e ds
008800040527     D OG150         E DS
008900941227      *
009000900517     D KPJBA         E DS
009100920422     D CNCR80        E DS
009200920422     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
009300020419      * ds per trul06r
009400020419     d trul06ds      e ds
009500020419     d  lin                    1     90  0 dim(30)
009600030410
009700030410     d Tibs02ds      e ds
009800050419     d fnlv55ds      e ds
009900030410     d dRsa          e ds
010000031028     d dsbl4a        e ds
010001081028     d ds7j          e ds
010100050419     D                 DS
010200050419     D  arbAAS                 1      4  0
010300050419     D  arbmgs                 5      8  0
010400050419     D  arbdsp                 1      8  0
010500030410
010600910412     C****************************************************************
010700910412     C*  RIEPILOGO INDICATORI
010800910412     C***************************************************************
010900910412     C* 30    - DI COMODO
011000030506      ***!!! 31    - Password
011100050420     C* 40-75 - ERRORE
011200910412     C* 90    - ERRORE GENERICO
011300910412     C*****************************************************************
011400000000     C     *ENTRY        PLIST
011500000000     C                   PARM                    KPJBA
011600000000     C                   Z-ADD     1             CODUT
011700920422     C                   CALL      'X§PARUT'
011800920422     C                   PARM                    UT§DSE
011900000000     C                   MOVEL     RAGUT         RSUT             20
012000920422     C                   MOVEL     REC80         CNCR80
012100910205     C*---------------------------------------------------------------*
012200920422     C     KARB          KLIST
012300920422     C                   KFLD                    VIDAAS
012400920422     C                   KFLD                    VIDLNP
012500920422     C                   KFLD                    VIDSED
012600920422     C                   KFLD                    VIDNSD
012700060216     C* ACCESSO FiAR401L
012800970131     C     KAR4          KLIST
012900970131     C                   KFLD                    VIDAAS
013000970131     C                   KFLD                    VIDLNP
013100970131     C                   KFLD                    VIDSED
013200970131     C                   KFLD                    VIDNSD
013300970131     C                   KFLD                    WTRC
013400920422     C* ACCESSO   TABEL                                    *
013500920422     C     KTAB2         KLIST                                                  *
013600920422     C                   KFLD                    CODUT                          *
013700920422     C                   KFLD                    COD               2            *
013800920422     C                   KFLD                    KEY               8            *
013801081028     C     KTAB          KLIST                                                  *
013802081028     C                   KFLD                    CODUT                          *
013803081028     C                   KFLD                    COD               2            *
013900030410      * Fnart27L
014000030410     c     kArt27        Klist
014100030410     c                   Kfld                    VidFls
014200030410     c                   Kfld                    VidLnas
014300030410     c                   Kfld                    VidNrs
014400030410     c                   Kfld                    VidNsc
014500050419     C     KARB01        KLIST
014600050419     C                   KFLD                    artAAS
014700050419     C                   KFLD                    artLNP
014800050419     C                   KFLD                    artnrs
014900050419     C                   KFLD                    artnsp
014901081028
015000030410
015100970131     C* DEFINIZIONE CAMPI
015200970131     C     *LIKE         DEFINE    AR4TRC        WTRC
015300970203     C* IMPOSTAZIONE CAMPI FISSI
015400970203     C                   MOVEL     'A'           WTRC
015500070424
015600070424     c                   z-add     *date         dataoggi
015700070424     c                   move      dataoggi      woggi
015800070424
015900941227     C*---------------------------------------------------------------*
016000050419     C* CARICO tabelle
016100920422     C                   EXSR      CARTAB
016200030612      * Richiesta stampanti
016300030612     c                   reset                   TRUL90DS
016400030612     c                   call      'TRUL90R'
016500030612     c                   parm                    KPJBA
016600030612     c                   parm                    TRUL90DS
016700941227     C**
016800941227     C* F3 - FINE
016900030612     C     D90F3         IFEQ      '1'
017000941227     C                   GOTO      FINE
017100941227     C                   ENDIF
017200030612      * Parametri x ovrprtf
017300030612      * · stampante per etichette
017400030612     c                   movel     D90pre        wD90pre
017500030612      * · modulo per stampa etichette
017600030612     c                   movel     D90mde        wD90mde
017700030612      * · pgm. di stampa etichette segnacolli
017800030612     c                   movel     D90pss        wD90pss
017900030612      * · stampante per bolle/fatture ..... - ad aghi
018000030612     c                   movel     D90prb        wD90prb
018100030612      * · modulo per stampa bolle/fatture . - ad aghi
018200030612     c                   movel     D90mdb        wD90mdb
018300030612      * · stampante per LDV in formato "A4" - laser
018400030612     c                   movel     D90prb4       wD90prb4
018500030612      * · stampante per LDV in formato "A5" - laser
018600030612     c                   movel     D90prb5       wD90prb5
018700030612      * · modulo per stampa LDV su "A4" ... - laser
018800030612     c                   movel     D90mdb4       wD90mdb4
018900030612      * · modulo per stampa LDV su "A5" ... - laser
019000030612     c                   movel     D90mdb5       wD90mdb5
019100030612      * · pgm.   per stampa LDV ........... - ad aghi o laser
019200030612     c                   movel     D90psl        wD90psl
019300900517     C*
019400950411     C     INIZIO        TAG
019500920422     C                   MOVEL     *ZEROS        VIDLNP
019600920422     C                   MOVEL     *ZEROS        VIDLNA
019700941227     C                   TIME                    W0140            14 0
019800941227     C                   MOVE      W0140         VIDAAS
019900941227     C*
020000910903     C                   MOVEL     *ZEROS        VIDDSD
020100910408     C                   MOVEL     *ZEROS        VIDDSA
020200910903     C                   MOVEL     *ZEROS        VIDSED
020300910903     C                   MOVEL     *ZEROS        VIDSEA
020400910903     C                   MOVEL     *ZEROS        VIDNSD
020500910903     C                   MOVEL     *ZEROS        VIDNSA
020600980407     C                   MOVEL     ' '           VIDDDT
020700030410     c                   Clear                   VidFls
020800030410     c                   Clear                   VidNrs
020900030410     c                   Clear                   VidNsc
021000030410     c                   Clear                   VidLnas
021100941227     C*
021200920422     C     FOR01         TAG
021300941227     C                   EXFMT     LR72D01
021400920421     C** CMD3 - FINE LAVORO
021500900509     C   KC              GOTO      FINE
021600030506
021700030506     c                   Eval      *In28 = *Off
021800030506
021900920422     C** CMD7 - INTERROGAZIONE BOLLE ARRIVI
022000920422     C   KG              DO
022100990204     C                   MOVEL     '2'           VI2FLG
022200920422     C                   MOVEL     PARAM2        KPJBU
022300941227     C                   CALL      'FNLR36R'
022400920422     C                   PARM                    KPJBA
022500990204     C                   MOVEL     KPJBU         PARAM2
022600990204     C                   MOVEL     PA1AAS        VIDAAS
022700990204     C                   MOVEL     PA1LNP        VIDLNP
022800990204     C                   MOVEL     PA1NRS        VIDSED
022900990204     C                   MOVEL     PA1NSP        VIDNSD
023000030506      * se non è stato scelto niente l'anno torna a 0
023100030506     c                   If        VidAas = *Zeros
023200030506     c                   Move      W0140         VidAas
023300030506     c                   EndIf
023400920422     C                   GOTO      FOR01
023500920422     C                   END
023600910205     C* CONTROLLI FORMATO
023700910205     C                   EXSR      CONTR
023800971218     C  NKF
023900971218     CANNKA
024000971218     CANNKH
024100920421     COR 90              GOTO      FOR01
024200901214     C*
024300910207     C* RISTAMPA BOLLE
024400970715     C     *INKF         IFEQ      *ON
024500970715     C                   MOVEL     'S'           TIRIS
024600040527     c     wmde          ifeq      'S'
024700040527     c                   movel     'DpdLarge  '  wd90mde
024800040527     c                   endif
024900970715     C                   ENDIF
025000970715     C     *INKA         IFEQ      *ON
025100970715     C                   MOVEL     'B'           TIRIS
025200970715     C                   ENDIF
025300971218     C     *INKH         IFEQ      *ON
025400971218     C                   MOVEL     'D'           TIRIS
025500971218     C                   ENDIF
025600970715     C**
025700900523     C                   MOVEL     PARAM         KPJBU
025800941227     C                   MOVEL     'LR73'        KCOAZ
025900040520     C***                CALL      'BCH10'
026000040520     C***                movel(p)  'FNLR73C'     Wpgm             10
026100040520     C                   movel(p)  'BCH10'       Wpgm             10
026200040520     C                   call      Wpgm
026300900523     C                   PARM                    KPJBA
026400030506
026500040315     c**!!!              Eval      VidMsg = Msg(1)
026600040315     c**!!!              Eval      *In28 = *On
026700920421     C*
026800950411     C                   GOTO      INIZIO
026900000000     C     FINE          TAG
027000000000     C                   SETON                                        LR
027100920422     C*
027200920422     C*--- CARICO TABELLE --------------------------------------------*
027300920422     C     CARTAB        BEGSR
027400020419     C* CARICO FILIALI GESTITE £1
027500050419     c**                 clear                   trul06ds
027600050419     c**                 eval      d06cod = '£1'
027700050419     c**                 movel     simfel        d06key
027800050419     c**                 movel     trul06ds      kpjbu
027900050419     c**                 call      'TRUL06R'
028000050419     c**                 parm                    kpjba
028100050419     c**                 movel     kpjbu         trul06ds
028200050419     c**                 movea     lin           l1
028300050419     c**
028400050419     C* CARICO FILIALI GESTITE £6
028500050419     c                   clear                   trul06ds
028600050419     c                   eval      d06cod = '£6'
028700050419     c                   movel     simpou        d06key
028800050419     c                   movel     'L'           d06tla
028900050419     c                   movel     trul06ds      kpjbu
029000050419     c                   call      'TRUL06R'
029100050419     c                   parm                    kpjba
029200050419     c                   movel     kpjbu         trul06ds
029300050420     c                   movea     lin           £6
029400050420     c
029500050420     c                   Clear                   Trul09ds
029600050420     c                   Eval      I09Mod = 'TFU'
029700050420     c                   Eval      I09Tfa = simpou
029800050420     c                   Call      'TRUL09R'
029900050420     c                   Parm                    Trul09ds
030000050420     c
030100050420     c* se lai non è terminal di nessuno metto comunque lai in mec
030200050420     c                   if        tfa(1) = *zeros
030300050420     c                   movea     £6            tfa
030400050420     c                   endif
030500050419     c
030600040527     c*
030700040527     c* Verifico se presente almeno un p.o. DPD con ogsea='S' per sapere se
030800040527     c* devo passare tipo modulo "DPDLARGE"
030900040527     c                   z-add     1             xx                2 0
031000040527     c                   clear                   wmde
031100050419     c     tfa(xx)       dowgt     *zeros
031200040527     c     xx            andle     30
031300040527     c     wmde          andeq     *blanks
031400050419     c     tfa(xx)       chain     azorg01l
031500040527     c                   if        %found(azorg01l)
031600040527     c                   movel     orgde3        og143
031700040527     c                   movel     orgdf0        og150
031800040527     c     §ogntw        ifeq      'DPD'
031900040527     c     §ogsea        andeq     'S'
032000040527     c                   movel     'S'           wmde              1
032100040527     c                   endif
032200040527     c                   endif
032300040527     c                   add       1             xx
032400040527     c                   enddo
032401081028     c* carico codici pistola non automatici
032404081028     C                   MOVEL     '7J'          COD
032405081028     C                   Z-ADD     1             xy
032406081028     C     KTAB          SETLL     TABEL
032409081028    2C                   DO        *hival
032410081028     C     KTAB          READE     TABEL
032411081028     c                   if        %eof(tabel00f)
032412081028     c                   leave
032413081028     c                   endif
032415081028    3C     TBLFLG        IFEQ      ' '
032416081028     C                   MOVEL     TBLUNI        DS7J
032417081028     c     §7jrps        ifne      'A'
032418081028     C                   MOVEL     TBLKEY        NPS(xy)
032419081028     C                   ADD       1             xy
032420081028     c                   endif
032421081028    3C                   END
032424081028    2C                   ENDDO
032500970717     C**
032600920422     C                   ENDSR
032700920422     C*
032800910205     C* CONTROLLO FORMATO -------------------------------------------
032900910205     C     CONTR         BEGSR
033000910205     C                   SETOFF                                       90
033100920422     C                   Z-ADD     0             DSA
033200920422     C                   Z-ADD     0             DSD
033300030506
033400030506      * se premuto F8 e non c'è il segnacollo subito errore
033500030506If  1c                   If        *InKh and Vidnsc = *Zeros
033600030506     c                   Seton                                        6490
033700030506     c                   GoTo      EndCtr
033800050420    1c                   EndIf
033900030414
034000030506     c**!!!              If        VidNsc = *Zeros
034100030506     c**!!!              Eval      *In31 = *Off
034200030506     c**!!!              EndIf
034300970102     C*
034400970102     C* IMPOSTO PER BENE L'ANNO
034500970102     C* SE L'ANNO E' IMMESSO DI DUE CIFRE, METTO LE ALTRE DUE
034600050420    1C     VIDAAS        IFLT      100
034700970102     C     VIDAAS        IFGT      60
034800970102     C                   ADD       1900          VIDAAS
034900970102     C                   ELSE
035000970102     C                   ADD       2000          VIDAAS
035100970102     C                   ENDIF
035200050420    1C                   ENDIF
035300910412     C*
035400910207     C* LINEA DI PARTENZA OBBLIGATORIA
035500030410      * se non è stato inserito il segnacollo
035600050420    1C     VIDLNP        IFEQ      0
035700050420    2c                   If        VidNsc = *Zeros
035800030410     c                   Seton                                        4790
035900030410     c                   GoTo      EndCtr
036000050420    2c                   EndIf
036100030410     C**!!!              SETON                                        4090
036200030410     C**!!!              GOTO      ENDCTR
036300050420   x1C                   ELSE
036400910207     C     VIDLNP        CHAIN     AZORG                              30
036500910207     C   30              SETON                                        4090
036600910207     C   30              GOTO      ENDCTR
036700050420    1C                   END
036800030410
036900030410      * NO SEGNACOLLO
037000030410If  0c                   If        VidNsc = *Zeros
037100030410      * se immessa linea di partenza e non il segnacollo
037200050420    1c                   If        VidLnp > *Zeros
037300910903     C* IMMETTERE O LA DATA O IL NUMERO SPEDIZIONE
037400050420    2C     VIDDSD        IFEQ      0
037500920421     C     VIDNSD        ANDEQ     0
037600910903     C                   SETON                                        5290
037700910903     C                   GOTO      ENDCTR
037800050420    2C                   END
037900050420    2C     VIDDSD        IFNE      0
038000920421     C     VIDNSD        ANDNE     0
038100910903     C                   SETON                                        5290
038200910903     C                   GOTO      ENDCTR
038300050420    2C                   END
038400050420    1c                   EndIf
038500030410      * se immesso il numero di spedizione immettere anche la linea di partenza
038600050420    1c                   If        (VidNsd > *Zeros or VidNsa > *Zeros)
038700030410     c                              and VidLnp = *Zeros
038800030410     c                   Seton                                        4090
038900030410     c                   GoTo      EndCtr
039000050420    1c                   EndIf
039100910903     C* NUMERO SPEDIZIONE AL > DAL
039200050420    1C     VIDNSA        IFNE      0
039300920707     C                   Z-ADD     VIDNSA        COMSPA
039400920707     C                   MOVEL     VIDSEA        COMSPA            9 0
039500910903     C*
039600920707     C                   Z-ADD     VIDNSD        COMSPD
039700920707     C                   MOVEL     VIDSED        COMSPD            9 0
039800910903     C*
039900050420    2C     COMSPA        IFLT      COMSPD
040000910903     C                   SETON                                        4590
040100910903     C                   GOTO      ENDCTR
040200050420    2C                   END
040300920707     C* LA DIFFERENZA NON PUO' ESSERE > DI 5000
040400920707     C     COMSPA        SUB       COMSPD        RESTO             9 0
040500050420    2C     RESTO         IFGT      5000
040600920707     C                   SETON                                        5590
040700920707     C                   GOTO      ENDCTR
040800050420    2C                   END
040900920707     C*
041000910903     C* DAL > 0 SE IMMESSO AL
041100050420    2C     VIDNSD        IFEQ      *ZEROS
041200910903     C                   SETON                                        4690
041300910903     C                   GOTO      ENDCTR
041400050420    2C                   END
041500050420    1C                   END
041600910903     C* SE IMMESSA SERIE IMMETTERE ANCHE NUMERO
041700050420    1C     VIDSED        IFNE      0
041800920421     C     VIDNSD        ANDEQ     0
041900910903     C                   SETON                                        5390
042000910903     C                   GOTO      ENDCTR
042100050420    1C                   END
042200050420    1C     VIDSEA        IFNE      0
042300920421     C     VIDNSA        ANDEQ     0
042400910903     C                   SETON                                        5490
042500910903     C                   GOTO      ENDCTR
042600050420    1C                   END
042700920422     C* SE IMMESSO UNO SPECIFICO NUMERO CONTROLLO SE ESISTE BOLLA
042800980409    1C     VIDNSA        IFEQ      0
042900920422     C     VIDNSD        ANDNE     0
043000971219     C     KARB          CHAIN     FNARB01L                           30
043100971219     C   30              SETON                                        5190
043200971219     C   30              GOTO      ENDCTR
043300050419     c
043400050419     c* se immessa linea di arrivo congruente con quella della bolla
043500050420    2c                   if        vidlna>0 and vidlna<>arblna
043600050420     C                   SETON                                        7590
043700050419     C                   GOTO      ENDCTR
043800050420    2c                   endif
043900050420     c* Controllo il terminal di arrivo della bolla o = simpou o
044000050420     c*   lna = simpou o sua £6
044100050420    2c                   if        arbtfa<>simpou
044200050420     c     arblna        lookup    £6                                     30
044300050420    3c                   if        not *in30
044400050420     C                   SETON                                        7290
044500050420     C                   GOTO      ENDCTR
044600050420    3c                   endif
044700050420    2c                   endif
044800070419
044900070419      * controllo se lna gestita da altra filiale
045000070419     c                   clear                   fnlv55ds
045100070419     c                   movel     '6'           d55tpt
045200070419     c                   z-add     arblna        d55lin
045300070419     c                   movel     arbaas        d55drf
045400070419     c                   move      arbmgs        d55drf
045500070419     c                   call      'FNLV55R'
045600070419     c                   parm                    fnlv55ds
045700070419     c                   eval      wlnadstauto = *off
045800070419      * è una filiale gestita controllo se la lna stessa ha la procedura distinte automatiche attiva
045900070419     c                   if        d55lin <> d55tfa
046000070419     c                   clear                   og146
046100070419     c     d55lin        chain     azorg01l
046200070419     c                   if        %found(azorg01l)
046300070419     c                   eval      og146 = orgde6
046400070419     c                   endif
046500070419     c                   if        §ogdda <> *zeros and §ogdda <> *blanks and
046600070424     c                             §ogdda <> '20391231' and woggi >= §ogdda
046700070419     c                   eval      wlnadstauto = *on
046800070419     c                   endif
046900070419     c                   endif
047000070419      * la lna non ha la procedura attiva....controllo quindi la sua capofila di £6
047100070419     c                   if        wlnadstauto = *off
047200070419     c     d55tfa        chain     azorg01l
047300070419     c                   if        %found(azorg01l)
047400070419     c                   eval      og146 = orgde6
047500070419     c                   endif
047600070419     c                   if        §ogdda <> *zeros and §ogdda <> *blanks and
047700070424     c                             §ogdda <> '20391231' and woggi >= §ogdda
047800070419     c                   eval      wlnadstauto = *on
047900070419     c                   endif
048000070419     c                   endif
048100970131     C*
048200970203     C* BOLLA ESISTENTE: CONTROLLO CHE SIA GIA' STATA STAMPATA
048300060216     C*   ALTRIMENTI ERRORE (SE NON TROVO IL RECORD IN FiAR4 CONSIDERO
048400970131     C*   LA BOLLA COME SE FOSSE GIA' STATA STAMPATA)
048500980409    2C     *INKA         IFEQ      *ON
048600031028     c                   Clear                   dsbl4a
048700060216     C     KAR4          CHAIN     FiAR401L                           32
048800980409    3C     *IN32         IFEQ      *OFF
048900031028     C**!!!              MOVEL     AR4NOT        WBAM              1
049000031028     c                   Movel     Ar4Not        dsbl4a
049100031028     c                   Move      §b4abm        wbam              1
049200980409    4C     WBAM          IFEQ      'N'
049300070419     c     wlnadstauto   andeq     *off
049400980521     C                   SETON                                        5990
049500970131     C                   GOTO      ENDCTR
049600980409    4C                   ENDIF
049700980409   X3C                   ELSE
049800980409     C                   MOVEL     'S'           WBAM              1
049900980409    3C                   ENDIF
050000980409     C** SE RICHIESTO SI NO DDT VERIFICO SE CORRISPONDE ALLA BOLLA RICH
050100980409    3C     VIDDDT        IFEQ      'S'
050200980409     C     WBAM          ANDNE     'S'
050300980409     C     WBAM          ANDNE     'C'
050400031028     c     wbam          andne     'P'
050500031028     c     wbam          andne     'Q'
050600980409     C                   SETON                                        5890
050700980409     C                   GOTO      ENDCTR
050800980409    3C                   ENDIF
050900980409    3C     VIDDDT        IFEQ      'N'
051000980409    4C     WBAM          IFEQ      'S'
051100980409     C     WBAM          OREQ      'C'
051200031028     c     wbam          oreq      'P'
051300031028     c     wbam          oreq      'Q'
051400980409     C                   SETON                                        4490
051500980409     C                   GOTO      ENDCTR
051600980409    4C                   ENDIF
051700980409    3C                   ENDIF
051800990201     C**
051900990201     C** CONTROLLO SE POSSO RISTAMPARE LA BOLLA IN ARRIVO DA TABELLA 3C
052000990204    3C     ARBNRS        IFGT      0
052100990201     C* VEDO SE BOLLA FRANCO O ASSEGNATO
052200990201     C                   MOVEL     '3A'          COD
052300990201     C                   MOVEL(P)  ARBCBO        KEY
052400990203     C     KTAB2         CHAIN     TABEL                              30
052500990204    4C     *IN30         IFEQ      *OFF
052600990201     C                   MOVEL     TBLUNI        DS3A
052700990201     C                   ELSE
052800990201     C                   CLEAR                   DS3A
052900990204    4C                   ENDIF
053000990201     C                   MOVEL     §3ATB1        W001A             1
053100990201     C* ASSEGNATO
053200990204    4C     W001A         IFEQ      'A'
053300990201     C                   MOVEL(P)  ARBCCM        KEY
053400990201     C                   ELSE
053500990201     C* FRANCO
053600990201     C                   MOVEL(P)  ARBKSC        KEY
053700990204    4C                   ENDIF
053800990201     C**
053900990204     C                   MOVEL     '3C'          COD
054000990203     C     KTAB2         CHAIN     TABEL                              30
054100990204    4C     *IN30         IFEQ      *OFF
054200990201     C                   MOVEL     TBLUNI        DS3C
054300990201     C                   ELSE
054400990201     C                   CLEAR                   DS3C
054500990204    4C                   ENDIF
054600990201     C* NON RISTAMPABILE
054700990204    4C     §3CRSB        IFEQ      'N'
054800990201     C                   SETON                                        6090
054900990201     C                   GOTO      ENDCTR
055000990204    4C                   ENDIF
055100990204    3C                   ENDIF
055101101203     c* Se porto assegnato non tassato non si può mai ristampare
055102101203     c                   if        arbaca=' '
055104101206     C                   SETON                                        7390
055105101203     c                   GoTo      EndCtr
055106101203     c                   endif
055200990204    2C                   ENDIF
055300030410      * se richiesta stampa segnacolli estero la linea di arrivo della bolla deve essere estera
055400030410 If 2c                   If        *InKf
055500030410     c                   Clear                   Og143
055600030410     c     ArbLna        Chain     Azorg01l
055700050420    3c                   If        %Found(Azorg01l)
055800030410     c                   Movel     Orgde3        Og143
055900050420    3c                   EndIf
056000040527     c***                If        §OgNtw <> 'DPD' and §OgNtw <> 'EEX'
056100040527     c***                          and §OgNtw <> 'FED'
056200050420    3c                   If        §OgNtw <> 'DPD'
056300030410     c                   Seton                                        5790
056400030410     c                   GoTo      EndCtr
056500050420    3c                   EndIf
056600030410    2c                   EndIf
056700990201    1C                   ENDIF
056800920422     C**
056900910903     C* DATA SPEDIZIONE  DAL
057000050420    1C     VIDDSD        IFNE      0
057100920421     C                   MOVE      VIDDSD        G02DAT
057200920421     C                   MOVEL     *BLANK        G02ERR
057300941227     C                   CALL      'XSRDA8'
057400920421     C                   PARM                    WLBDAT
057500050420    2C     G02ERR        IFEQ      '1'
057600920421     C                   SETON                                        41  90
057700920421     C                   GOTO      ENDCTR
057800050420    2C                   END
057900941227     C                   Z-ADD     G02INV        DSD
058000941227     C                   Z-ADD     G02DAT        VIDDSD
058100050420    1C                   END
058200910903     C* SE IMMESSO AL IMMETTERE ANCHE DAL
058300050420    1C     VIDDSA        IFNE      0
058400050420    2C     VIDDSD        IFEQ      0
058500910903     C                   SETON                                        4190
058600910903     C                   GOTO      ENDCTR
058700050420    2C                   END
058800910903     C* DATA SPEDIZIONE  AL
058900910207     C*  SE NON IMMESSA = AL DAL
059000920421     C                   MOVE      VIDDSA        G02DAT
059100920421     C                   MOVEL     *BLANK        G02ERR
059200941227     C                   CALL      'XSRDA8'
059300920421     C                   PARM                    WLBDAT
059400050420    2C     G02ERR        IFEQ      '1'
059500920421     C                   SETON                                        42  90
059600920421     C                   GOTO      ENDCTR
059700050420    2C                   END
059800941227     C                   Z-ADD     G02INV        DSA
059900941227     C                   Z-ADD     G02DAT        VIDDSA
060000910207     C* NON < DATA DAL
060100050420    2C     DSA           IFLT      DSD
060200910207     C                   SETON                                        4390
060300910207     C                   GOTO      ENDCTR
060400050420    2C                   END
060500050420    1C                   END
060600941227     C*
060700920422     C* CONTROLLO SE IMMESSA LINEA DI ARRIVO
060800050420    1c                   if        (*inka and vidlna=0 and vidnsd=0) or
060900050419     c                             (*inka and vidlna=0 and vidnsd>0 and
061000050419     c                             vidnsa>0)
061100050419     C                   SETON                                        7190
061200050419     C                   GOTO      ENDCTR
061300050419     c                   goto      endctr
061400050420    1c                   endif
061500050419     c
061600050420    1c                   if        vidlna>0
061700050420     C     VIDLNA        LOOKUP    £6                                     30
061800050420    2c                   if        not *in30
061900050420     c*
062000050420     c* Controllo con la linea di partenza il terminal
062100050420    3c                   if        vidlnp>0
062200050420     c                   clear                   fnlv55ds
062300050420     c                   eval      d55tpt='A'
062400050420     c                   eval      d55lin=vidlna
062500050420     c                   eval      d55drf=DSD
062600050420     c                   eval      d55lnp=vidlnp
062700050420     c                   call      'FNLV55R'
062800050420     c                   parm                    fnlv55ds
062900050420    4c                   if        d55tfa<>simpou
063000050420     c                   Seton                                        5090
063100050420     c                   GoTo      EndCtr
063200050420    4c                   endif
063300050420     C* SE IMMESSA DATA AL controllo alla data AL il terminal
063400050420    4c                   if        DSA>0
063500050420     c                   clear                   fnlv55ds
063600050420     c                   eval      d55tpt='A'
063700050420     c                   eval      d55lin=vidlna
063800050420     c                   eval      d55drf=DSA
063900050420     c                   eval      d55lnp=vidlnp
064000050420     c                   call      'FNLV55R'
064100050420     c                   parm                    fnlv55ds
064200050420    5c                   if        d55tfa<>simpou
064300050420     c                   Seton                                        7490
064400050420     c                   GoTo      EndCtr
064500050420    5c                   endif
064600050420    4c                   endif
064700050420    3c                   endif
064800050420    2c                   endif
064900050420    1c                   endif
065000050420     C* CONTROLLO CONGRUENZA PARZIALIZZAZIONE RISTAMPA SEGNAC. ESTERO
065100030410If  1c                   If        *InKf and VidLna > *Zeros
065200030410     c                   Clear                   Og143
065300030410     c     VidLna        Chain     Azorg01l
065400030410     c                   If        %Found(Azorg01l)
065500030410     c                   Movel     Orgde3        Og143
065600030410     c                   EndIf
065700040527     c***                If        §OgNtw <> 'DPD' and §OgNtw <> 'EEX'
065800040527     c***                          and §OgNtw <> 'FED'
065900040527     c                   If        §OgNtw <> 'DPD'
066000030410     c                   Seton                                        5690
066100030410     c                   GoTo      EndCtr
066200030410     c                   EndIf
066300030410    1c                   EndIf
066400030410    0c                   EndIf
066500030410
066600030410      * SEGNACOLLO
066700030410      * se immesso il numero mettere anche gli altri dati
066800030410If  1c                   If        VidNsc > *Zeros
066900030410      * non immettere linea di partenza - spedizione - data
067000030410     c                   If        VidLnp > *Zeros or VidNsd > *Zeros or
067100030410     c                             VidDsd > *Zeros or VidLna > *Zeros or
067200030410     c                             VidDdt <> *Blanks
067300030410     c                   Seton                                        6990
067400030410     c                   GoTo      EndCtr
067500030410     c                   EndIf
067600030410      * p.o. segnacollo
067700030410If  2c                   If        VidFls = *Zeros
067800030410     c                   Seton                                        4890
067900030410     c                   GoTo      EndCtr
068000030410   x2c                   Else
068100030410     c     VidFls        Chain     Azorg01l
068200030410     c                   If        Not %Found(Azorg01l)
068300030410     c                   Seton                                        4890
068400030410     c                   GoTo      EndCtr
068500030410     c                   EndIf
068600030410    2c                   EndIf
068700030410      * Linea arrivo segnacollo
068800030410If  2c                   If        VidLnas = *Zeros
068900030410     c                   Seton                                        4990
069000030410     c                   GoTo      EndCtr
069100030410   x2c                   Else
069200030410      * il segnacollo deve esistere
069300030410     c     kArt27        Chain     Fnart27l
069400050420    3c                   if        %found(fnart27l)
069500050419     c     kArb01        Chain     Fnarb01l
069600050420    3c                   endif
069700050419     c
069800050420    3c                   If        Not %Found(Fnart27l)   or
069900050419     c                             Not %Found(Fnarb01l)
070000030410     c                   Seton                                        6290
070100030410     c                   GoTo      EndCtr
070200050420    3c                   EndIf
070300080402     c* Il seguente controllo lo faccio solo se richiesta ristampa
070400080402     c* segnacolli export Dpd. Per la ristamap del segnacollo Bartolini
070500080402     c* occorre dare la possbilità alla filiale di arrivo di ristampare
070600080402     c* anche se collo di altra area di arrivo
070700080414     c                   eval      waltra_a=' '
070800080402     c     VidLnas       Lookup    £6                                     30
070900080402    3c                   if        not *in30 and arbtfa<>simpou
071000080414     c                   eval      waltra_a='1'
071100080414     c                   if        *inkf
071200080402     c                   Seton                                        4990
071300080402     c                   GoTo      EndCtr
071400080402    3c                   EndIf
071500080402     c                   endif
071600050419     c
071700050420    2c                   endif
071800050419     c
071900030410      * se F6 la linea di arrivo del segnacollo deve essere estera
072000030410If  2c                   If        *InKf
072100030410     c                   Clear                   Og143
072200030410     c     VidLnas       Chain     Azorg01l
072300030410     c                   If        %Found(Azorg01l)
072400030410     c                   Movel     Orgde3        Og143
072500030410     c                   EndIf
072600040527     c***                If        §OgNtw <> 'DPD' and §OgNtw <> 'EEX'
072700040527     c***                          and §OgNtw <> 'FED'
072800040527     c                   If        §OgNtw <> 'DPD'
072900030410     c                   Seton                                        6390
073000030410     c                   GoTo      EndCtr
073100030410     c                   EndIf
073200030512     c                   GoTo      EndCtr
073300030410    2c                   EndIf
073400030410      * non ammesso F1
073500030410     c                   If        *InKa
073600030410     c                   Seton                                        6190
073700030410     c                   GoTo      EndCtr
073800030410     c                   EndIf
073900030514    1c**!!!              EndIf
074000030410      * se premuto F8
074100030506If  1c**!!!              If        *InKh
074200030410      * deve essere presente il segnacollo
074300030506     c**!!!              If        VidNsc = *Zeros and
074400030506     c**!!!                        (VidNsd > *Zeros or VidDsd > *Zeros)
074500030506     c**!!!              Seton                                        6490
074600030506     c**!!!              GoTo      EndCtr
074700030506     c**!!!              EndIf
074800030415      * deve esistere la tabella RSA per il p.o. utente
074900030415     c                   Clear                   Tibs02ds
075000030415     c                   Clear                   dRsa
075100030415     c                   Eval      T02Mod = 'C'
075200030415     c                   Eval      T02Sif = knsif
075300030415     c                   Eval      T02Cod = 'RSA'
075400030415     c                   Movel(p)  SimPou        T02Ke1
075500030415     c                   Call      'TIBS02R'
075600030415     c                   Parm                    Kpjba
075700030415     c                   Parm                    Tibs02ds
075800030415If  2c                   If        T02Err <> *Blanks
075900030415     c                   Seton                                        6590
076000030415     c                   GoTo      EndCtr
076100030415   x2c                   Else
076200030415     c                   Movel     T02Uni        Drsa
076300030415    2c                   EndIf
076301081028     c* Se richiesta la ristampa del segnacollo bartolini e trattasi
076302081028     c* di richiesta effettuata nell'area di arrivo del collo verifico
076303081028     c* se presenti spunte reali per lo stesso segnacollo. Infatti
076304081028     c* se presenti spunte la password è da richiedere altrimenti no
076305081028     c                   eval      w_pstreale=' '
076306081028    2c                   if        *inkh and waltra_a=' '
076307081028     c     kart27        setll     fnbrv07l
076308081028    3c                   do        *hival
076309081028     c     kart27        reade     fnbrv07l
076310081028    4c                   if        %eof(fnbrv07l)
076311081028     c                   leave
076312081028    4c                   endif
076313081028     c     brvnps        lookup    nps                                    30
076314081028    4c                   if        *in30
076315081028     c                   eval      w_pstreale='R'
076316081028     c                   leave
076317081028    4c                   endif
076318081028    3c                   enddo
076319081028    2c                   endif
076400030410      * devo richiedere la password
076500030506     c**!!!              If        Not *In31
076600030506     c**!!!              Seton                                        3190
076700030506     c**!!!              Eval      *Inkh = *Off
076800030506     c**!!!              GoTo      EndCtr
076900030506     c**!!!              EndIf
077000030506    1c**!!!              EndIf
077100030410      * Controllo la password
077200030506If  1c**!!!              If        *In31
077300081028   1ac                   If        *inkf or (*inkh and waltra_a=' ' and
077301081028     c                             w_pstreale='R')
077400030506     c                   If        VidPwd = *Blanks
077500030506     c                   Seton                                        7090
077600030506     c                   GoTo      EndCtr
077700030506     c                   EndIf
077800030410      * stessa password
077900030410     c                   If        VidPwd <> §RsaPwd
078000030410     c                   Seton                                        6690
078100030410     c                   GoTo      EndCtr
078200030410     c                   EndIf
078300030410      * data immissione/variazione password
078400030410     c                   Clear                   wlbdat
078500030410     c                   Z-Add     §RsaDta       G02Dat
078600030410     c                   Call      'XSRDA8'
078700030410     c                   Parm                    Wlbdat
078800030410     c                   Movel     G02Inv        Dataiso
078900030410      * Scadenza password
079000030410     c                   Clear                   Tibs02ds
079100030410     c                   Eval      T02Mod = 'C'
079200030410     c                   Eval      T02Sif = knsif
079300030410     c                   Eval      T02Cod = 'RSA'
079400030410     c                   Movel(p)  'PSW'         T02Ke1
079500030410     c                   Call      'TIBS02R'
079600030410     c                   Parm                    Kpjba
079700030410     c                   Parm                    Tibs02ds
079800030410If  2c                   If        T02Err <> *Blanks
079900030410     c                   Seton                                        6890
080000030410     c                   GoTo      EndCtr
080100030410   x2c                   Else
080200030410     c                   Movel     T02Uni        wgiorni
080300030410     c                   Adddur    wgiorni:*d    Dataiso
080400030410     c                   Move      Dataiso       Datascad
080500030410     c                   Z-Add     *Date         Dataoggi
080600030410     c                   If        Dataoggi > DataScad
080700030410     c                   Seton                                        6790
080800030410     c                   GoTo      EndCtr
080900030410     c                   EndIf
081000030410    2c                   EndIf
081100080402   1ac                   EndIf
081200030514    1c                   EndIf
081300910903     C*
081400910205     C     ENDCTR        ENDSR
081500030506**         MSG
081600030506La ristampa è stata sottomessa in coda                                        01
