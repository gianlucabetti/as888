000100060307     H DFTACTGRP(*NO) ACTGRP('QILE') BNDDIR('PJXBND':'TRULCIF')
000200060302     H DECEDIT('0,') DATEDIT(*DMY.)
000300060224      * -------------------------------------------------------------*
000400060224      *     - cambia tipo incasso se non eseguita ancora chiusura op.*
000500960702     F*--------------------------------------------------------------*
000600060224     Fcnc0w5D   CF   E             WORKSTN
000700060224     FFNARB01L  UF   E           K DISK    commit
000800060227     FFiinc01L  uF   E           K DISK    commit
000900060224     Fcnass01L  uF a E           K DISK    commit
000901060412     Ffiar901L  uF a E           K DISK    commit
000902060414     Ffiari01L  uF   E           K DISK    commit
001100060224     FFnlbl01L  iF   E           K DISK
001200030730     FTABEL00F  iF   E           K DISK
001300940321      *---------------------------------------------------------------
001400030730     d* schiere
001500030730      *---------------------------------------------------------------
001600030730     d* ds esterne
001700030730      *---------------------------------------------------------------
001800030730     D KPJBA         E DS
001900030730     D ddatiute      e ds
002000060301     D fnarbdssav    e ds                  extname(fnarb00F) prefix(s)
002100060301     D fnarbds       e ds                  extname(fnarb00F)
002200060301     D fnarbmds      e ds                  extname(fnarb00F) prefix(m)
002300060301     D azuteds       e ds                  extname(AZUTE00F)
002400060309     D fiar6ds       e ds                  extname(fiar600F)
002500060309     D fiar9ds       e ds                  extname(fiar900F)
002600030730     D tibs34ds      E DS                  inz
002700060307     D ds3a          E DS                  inz
002800060307     D ds1a          E DS                  inz
002900060307     D ds4f          E DS                  inz
003000060301     c*po gestione
003100060301     d trul06ds      e ds
003200060301     d  lint                   1     90  0 dim(30)
003201060915     d  £1             s              3  0 dim(30)
003300060224      * DS per richiamo pgm FNLR36R
003400060224     D DSLR36          DS
003500060224     D  P36AAS                14     17  0
003600060224     D  P36LNP                18     20  0
003700060224     D  P36NRS                21     22  0
003800060224     D  P36NSP                23     29  0
003900060224     D  P36F03                30     30
004000060224     D  P36FLG                31     31
004100030730     d dtaeur          s               d   datfmt(*eur)
004200030730     d dtaiso          s               d
004201060915     D dutpouc         s                   LIKE(dutpou)
004300060307     D chkBollaIncassata...
004400060307     D                 PR            10I 0
004500060307     D  aas                                LIKE(arbaas)
004600060307     D  lnp                                LIKE(arblnp)
004700060307     D  nrs                                LIKE(arbnrs)
004800060307     D  nsp                                LIKE(arbnsp)
004900060307     D  cbo                                LIKE(arbcbo)
005000060307     D                                     OPTIONS(*OMIT)
005100060307     D  datiCbo                      89A
005200060310     D                                     OPTIONS(*OMIT)
005300060307     D  tpd                           1A
005400060307     D  esito                        10I 0
005500060307     D  err                           1A   DIM(10)
005600060307     D  msg                          80A   DIM(10)
005700060307     D                                     OPTIONS(*OMIT)
005800060310     D  datiFattura               32767A
005900060307     D                                     OPTIONS(*OMIT:*VARSIZE)
006000060307     D                                     VARYING
006100060310     D  datiContrassegno...
006200060307     D                            32767A
006300060307     D                                     OPTIONS(*OMIT:*VARSIZE)
006400060307     D                                     VARYING
006500060307     D chkIncassoPO    PR            10I 0
006600060321     D  soc                           3a
006700060321     D                                     CONST
006800060321     D  fgs                           3  0
006900060307     D  tic                                LIKE(inctii)
007000060307     D  tpd                           1A   CONST
007100060307     D  dati1a                             LIKE(tblUni)
007200060307     D                                     OPTIONS(*OMIT)
007300060321     D  datiY4O                     256A
007400060321     D                                     OPTIONS(*OMIT)
007500060307     D  nra                                LIKE(assnra)
007600060307     D                                     OPTIONS(*OMIT)
007700060307     D  abi                                LIKE(assabi)
007800060307     D                                     OPTIONS(*OMIT)
007900060307     D  cab                                LIKE(asscab)
008000060307     D                                     OPTIONS(*OMIT)
008100060307     D  dte                          10D   DATFMT(*ISO)
008200060307     D                                     OPTIONS(*OMIT)
008300060307     D                                     CONST
008400060307     D  esito                        10I 0
008500060307     D                                     OPTIONS(*OMIT)
008600060307     D  err                           1A
008700060307     D                                     DIM(10)
008800060307     D  msg                          80A   OPTIONS(*OMIT)
008900060307     D                                     DIM(10)
009000060321     D  prmKpjba                           OPTIONS(*OMIT:*NOPASS)
009100060321     D                                     LIKEDS(kpjba)
009200060307     D prmEsito        S             10I 0
009300060307     D esito           S             10I 0
009400060307     D err             S              1    DIM(10)                              SKI CHIAVI
009500060310     D msg             S             80    DIM(10)                              SKI CHIAVI
009600060307     D C3A             S              2    DIM(100)                             SKI CHIAVI
009700060310     D D3A             S                   like(tbluni) DIM(100)                SKI TAB. 3
009800060321     D fgs             S                   like(arblnp)                         SKI TAB. 3
009900060307     D nra             S                   like(assnra)                         SKI TAB. 3
010000060307     D kaas            S                   like(arbaas)                         SKI TAB. 3
010100060307     D klnp            S                   like(arblnp)                         SKI TAB. 3
010200060307     D knrs            S                   like(arbnrs)                         SKI TAB. 3
010300060307     D knsp            S                   like(arbnsp)                         SKI TAB. 3
010400060308     D savabi          S                   like(assabi)                         SKI TAB. 3
010500060308     D savcab          S                   like(asscab)                         SKI TAB. 3
010600060310     Ddatifat          s          32767A
010700060310     D                                     VARYING
010800060310     Ddatica           s          32767A
010900060310     D                                     VARYING
011000911016      *---------------------------------------------------------------*
011100911016      * FLUSSO PRINCIPALE                                             *
011200911016      *---------------------------------------------------------------*
011300030730     C                   DO        *HIVAL
011400030730      *
011500060308     c                   if        *in90
011600060308     c                   setoff                                       90
011700030730     C                   write     LR98D01
011800030730     c                   seton                                        90
011900060308     c                   end
012000030730     c*
012100030730     C                   EXFMT     LR98D01
012200030730     c* fine lavoro
012300941107     C     *INKC         IFEQ      '1'
012400030730     C                   LEAVE
012500941107     C                   END
012600030730     c* controlli
012700030730     c                   exsr      srctr
012800060412     c   kc              leave
012801060412     c   90              iter
013400030730     c*aggiorna
013500060302     c                   if        *inkf
013600030730     c                   exsr      sraggio
013700030730     c   90              iter
013800030730     c*inizializza
013900030730     c                   exsr      srinz
014000030730     c                   end
014100030730     c                   enddo
014200941107     C*
014300030730     C                   SETON                                        LR
014400030730     c**********************************************************************
014500030730     C     srinz         BEGSR
014600030730     c**********************************************************************
014700060224     C* Pulisco i campi che devo visualizzare nel fm2
014800060301     C                   MOVE      *year         V03AAS
014900060224     C                   clear                   V03LNP
015000060224     C                   clear                   V03NRS
015100060224     C                   clear                   V03NSP
015200060307     C                   clear                   V03tpd
015300060307     C                   clear                   V03dtd
015301060412     C                   eval       V03sce = '1'
015400060227     C                   clear                   V03ticp
015500060227     C                   clear                   V03dtip
015600060307     C                   clear                   V03nrap
015700060227     C                   clear                   V03abip
015800060227     C                   clear                   V03cabp
015900060307     C                   clear                   V03dtep
016000060224     C                   clear                   V03ticd
016100060227     C                   clear                   V03dtid
016200060307     C                   clear                   V03nrad
016300060224     C                   clear                   V03abid
016400060224     C                   clear                   V03cabd
016500060307     C                   clear                   V03dted
016600060307     c                   clear                   indkb             1
016700060308     c                   clear                   savabi
016800060308     c                   clear                   savcab
016900030730     c                   endsr
017000030730     c**********************************************************************
017100030730     C     srctr         BEGSR
017200030730     c**********************************************************************
017300030620     C                   SETOFF                                       90
017301060412     c                   clear                   v03msg
017400030730     c                   do
017500030730     C*F7-INTERROGAZIONE BOLLE ARRIVI
017600030730     C                   if        *inkg
017700941109     C                   MOVEL     '2'           P36FLG
017800060301     C                   Z-ADD     V03AAS        P36AAS
017900941108     C                   Z-ADD     V03LNP        P36LNP
018000941108     C                   Z-ADD     V03NSP        P36NSP
018100941108     C                   Z-ADD     V03NRS        P36NRS
018200941109     C                   MOVEL     ' '           P36F03
018300941109     C     P36NSP        IFGT      0
018400941109     C                   MOVEL     '1'           P36FLG
018500941109     C                   END
018600941108     C                   MOVEL     DSLR36        KPJBU
018700941108     C                   CALL      'FNLR36R'
018800911128     C                   PARM                    KPJBA
018900941109     C                   MOVEL     KPJBU         DSLR36
019000941109     C     P36F03        IFNE      '1'
019100941215     C     V03NSP        ANDEQ     0
019200060301     C                   Z-ADD     P36AAS        V03AAS
019300941109     C                   Z-ADD     P36LNP        V03LNP
019400941109     C                   Z-ADD     P36NRS        V03NRS
019500941109     C                   Z-ADD     P36NSP        V03NSP
019600941109     C                   END
019700911128     C                   SETON                                        90
019800030730     c                   leave
019900911128     C                   END
020400060307     C* tipo documento
020500060321     c                   clear                   v03dtd
020600060321     c                   setoff                                       15
020700060307     C     V03tpd        IFEQ      *blanks
020800060310     C     v03tpd        oreq      'V'
020900060307     C                   SETON                                        1590
021000030730     c                   leave
021100060307     c                   else
021101060412     C* RICHIESTA tipo documento
021102060412     C     '?'           SCAN      v03tpd                                 89
021103060412IF  2C     *IN89         IFEQ      *ON
021104060412     c                   eval      codut = 1
021105060412     C                   MOVEL     '4F'          COD
021106060412     C                   MOVEL     *BLANKS       KEY
021107060412     C                   MOVEL     *BLANKS       DES
021108060412     C                   CALL      'X§TABER'
021109060412     C                   PARM                    CODUT             1 0
021110060412     C                   PARM                    COD               2
021111060412     C                   PARM                    KEY               8
021112060412     C                   PARM                    DES              25
021113060412     C                   MOVEL     KEY           V03tpd
021117060412    2C                   END
021200060307     c                   movel     '4F'          tblcod
021300060307     c                   movel(p)  v03tpd        tblkey
021400060307     c     ktab1         chain     tabel00f
021500060307     c                   if        %found(tabel00f)
021600060307     c                   movel     tbluni        ds4f
021700060307     c                   movel     §4fdes        v03dtd
021800060307     c                   else
021900060307     C                   SETON                                        1590
022000060307     c                   leave
022100911017     C                   END
022200060307     C                   END
022201060412     c* scelta
022202060412     c                   if        v03sce = '1' and
022203060412     c                             v03tpd <> 'P' and v03tpd <> 'C'
022204060412     C                   SETON                                        90
022205060412     c                   eval      v03msg = 'La rettifica del tipo incasso -
022206060412     c                             è ammessa solo per pre-pagati o c/assegni'
022207060412     c                   leave
022208060412     C                   END
022216060412     c*
022217060412     c                   if        v03sce = '2' and v03tpd = 'P'
022218060412     C                   SETON                                        90
022219060412     c                   eval      v03msg = 'La rettifica errato incasso -
022220060412     c                              non è ammessa per pre-pagati'
022221060412     c                   leave
022222060412     C                   END
022223060915     C*F18 - cambio po incasso
022224060915     C                   if        *inks
022225060915     C                   SETON                                        45
022226060915     c                   leave
022227060915     c                   end
022228060915     c* controllo che il po scelto sia nell'area
022229060915     c                   if        *in45 and v03lna <> dutpou
022230060915     c                   z-add     1             xx
022231060915     c     v03lna        lookup    £1(xx)                                 19
022232060915     c                   if        not *in19
022233060915     C                   SETON                                        1890
022234060915     c                   leave
022235060915     c                   end
022236060915     c                   eval      dutpouc = v03lna
022237060915     c                   exsr      sr£6
022238060915     c                   end
022300060307     C* Controllo se la spedizione è esistente o valida su video
022400060307     C     V03AAS        IFEQ      *ZEROS
022500060307     C                   SETON                                        0190
022600060307     c                   leave
022700060307     C                   END
022800911017     C     V03LNP        IFEQ      *ZEROS
022900030730     C                   SETON                                        0290
023000030730     c                   leave
023100911017     C                   END
023200911017     C     V03NSP        IFEQ      *ZEROS
023300030730     C                   SETON                                        0390
023400030730     c                   leave
023500911017     C                   END
023600060228     C* AGGANCIO FNARB x c/ass o assegnato oppure FIINC x prepagati
023700060301     C                   Z-ADD     v03aas        KAAS
023800060301     C                   Z-ADD     v03lnp        KLNP
023900060307     C                   Z-ADD     v03NRs        KNRS
024000060301     C                   Z-ADD     v03NSP        KNSP
024100060307     c* prepagato
024200060307     c                   if        v03tpd = 'P'
024300060307     C     Kinc          CHAIN(n)  Fiinc01L
024400060307     c                   if        not %found(fiinc01l)
024500060307     c                   seton                                        0590
024600060307     c                   leave
024700060307     c                   else
024800060307     c                   exsr      ctrpre
024801060412     c   90              leave
024900060307     c                   end
025000060307     c                   else
025100060321     C                   clear                   V03ticp
025200060321     C                   clear                   V03dtip
025300060321     C                   clear                   V03nrap
025400060321     C                   clear                   V03abip
025500060321     C                   clear                   V03cabp
025600060321     C                   clear                   V03dtep
025700060321     C                   clear                   V03ticd
025800060321     C                   clear                   V03dtid
025900060321     C                   clear                   V03nrad
026000060321     C                   clear                   V03abid
026100060321     C                   clear                   V03cabd
026200060321     C                   clear                   V03dted
026300060227     C     Kspe          CHAIN(n)  FNARB01L
026400060307     c                   if        not %found(fnarb01l)
026500060307     c                   seton                                        0590
026600060307     c                   leave
026700060224     c                   else
026800060301     c                   movel     fnarbds       fnarbdssav
026900060227     c                   exsr      ctrdis
026901060412     c   90              leave
027000060307     c                   end
027100060307     c                   end
027201060412     c* se scelta tipo incasso o errore intestazione assegno emetto
027202060412     c* la window
027203060412     c                   if        v03sce = '1'
027204060412     c                   exsr      srwindow
027205060412     c                   end
027206060412     c                   enddo
027300030730     c*
027400911017     C                   ENDSR
027500060224      *---------------------------------------------------------*
027600060227      *  Controllo c/ass. o assegnato                           *
027700060224      *---------------------------------------------------------*
027800060412     C     srwindow      BEGSR
027803060412     c*
027804060412     C                   DO        *HIVAL
027805060412      *
027806060412     c                   if        *in90
027807060412     c                   setoff                                       90
027808060412     C                   write     LR98W01
027809060412     c                   seton                                        90
027810060412     c                   end
027811060412     c*
027812060412     C                   EXFMT     LR98W01
027813060412     c* fine lavoro
027814060412     C     *INKC         IFEQ      '1'
027815060412     C                   LEAVE
027816060412     C                   END
027817060412     c* controlli
027818060412     c                   exsr      srctrW
027819060412     c   kl              leave
027820060412     c   90              iter
027821060412     c* non procedo con l'aggiornamento se ho errore abi cab e non ho pre-
027822060412     c* muto f2 x forzarlo
027823060412     c                   if        *in50 and indkb = ' '
027824060412     c                   iter
027825060412     c                   end
027826060412     c*aggiorna
027827060412     c                   if        *inkf
027828060412     c                   exsr      sraggio
027829060412     c   90              iter
027830060412     c*inizializza
027831060412     c                   exsr      srinz
027832060412     c                   leave
027833060412     c                   end
027834060412     c                   enddo
027835060412     c*
027836060412     C                   ENDSR
027837060412      *---------------------------------------------------------*
027838060412      *  Controllo c/ass. o assegnato                           *
027839060412      *---------------------------------------------------------*
027840060412     C     srctrw        BEGSR
027841060412     C                   SETOFF                                       90
027842060412     c                   clear                   v03msgw
027843060412     c                   do
027844060412     C*F12= giuda
027845060412     C                   if        *inkl
027846060412     c                   leave
027847060412     C                   END
027848060412     C*F2-forzatura abi cab
027849060412     C                   if        *inkb
027850060412     c                   eval      indkb = '1'
027851060412     C                   END
027852060412     c* controllo che sia campiato qualcosa
027853060412     c                   if        v03ticp = v03ticd and
027854060412     c                             v03nrap = v03nrad and
027855060412     c                             v03abip = v03abid and
027856060412     c                             v03cabp = v03cabd and
027857060412     c                             v03dtep = v03dted
027858060412     c                   seton                                        90
027859060412     c                   eval      v03msgw= 'I riferimenti incasso prima -
027860060412     c                             sono uguali a quelli dopo'
027861060412     c                   leave
027862060412     c                   end
027863060412     c* tipo incasso dopo
027864060412     c                   clear                   v03dtid
027865060412     C     '?'           SCAN      V03TICd                                44
027866060412     C     *IN44         IFEQ      '1'
027867060412     C                   MOVE      *BLANKS       tblkey
027868060412     C                   MOVEL     '1A'          tblCOD
027869060412     C                   CALL      'X§TABER'
027870060412     C                   PARM                    tblKUT
027871060412     C                   PARM                    tblCOD
027872060412     C                   PARM                    tblKEY
027873060412     C                   PARM                    §DES             30
027874060412     C                   MOVEL(p)  tblKEY        V03TICd
027875060412     C                   MOVEL     §des          V03dtid
027876060412     c                   else
027877060412     c                   exsr      srticd
027878060412     c   10              leave
027888060412     c                   end
027889060412     c* controllo la data
027890060412     c                   clear                   dtaiso
0278910604121    c                   if        v03dted = 0
027892060412     c                   if        v03abid <> 0
027893060412     C                   SETON                                        1390
027894060412     c                   leave
027895060412     c                   end
027896060412x1   c                   else
027897060412     c     *eur          test(d)                 v03dted                13
0278980604122    c                   if        *in13
027899060412     c     *dmy          test(d)                 v03dted                13
0279000604123    c                   if        not *in13
027901060412     c     *dmy          move      v03dted       dtaeur
027902060412     c                   move      dtaeur        v03dted
027903060412     c                   move      dtaeur        dtaiso
027904060412x3   c                   else
027905060412     c                   seton                                        90
027906060412     c                   leave
0279070604123    c                   end
027908060412     c                   else
027909060412     c                   move      v03dted       dtaeur
027910060412     c                   move      dtaeur        dtaiso
0279110604122    c                   end
0279120604121    c                   end
027913060412     c                   eval      nra = v03nrad
027914060412     c* salvo i riferimenti abi e cab per gestire f2 di forzatura
027915060412     c                   if        savabi <> v03abid or
027916060412     c                             savcab <> v03cabd
027917060412     c                   clear                   indkb
027918060412     c                   eval      savabi = v03abid
027919060412     c                   eval      savcab = v03cabd
027920060412     c                   end
027921060412     c* controlli nuovo tipo incasso
027922060412     c                   clear                   tbluni
027923060412     c                   eval      fgs = dutpou
027924060412     c                   callp     chkincassopo('201':fgs:v03ticd:v03tpd:tbluni
027925060412     c                             :*omit:nra
027926060412     c                             :v03abid:v03cabd:dtaiso:esito:err:*omit
027927060412     c                             :*omit)
027928060412     c                   eval      v03nrad =nra
027929060412     c*
0279300604121    c                   if        esito <>0
0279310604123    c                   do        10            xx                2 0
027932060412     c                   if        err(xx) = ' '
027933060412     c                   leave
027934060412     c                   end
0279350604124    c                   select
027936060412     c* tipo incasso errato
027937060412     c                   when      err(xx) = 'J'
027938060412     c                   seton                                        1090
027939060412     c* numero assegno errato
027940060412     c                   when      err(xx) = 'C' or
027941060412     c                             err(xx) = 'D' or
027942060412     c                             err(xx) = 'L'
027943060412     c                   seton                                        1190
027944060412     c* abi o cab obbligatori
027945060412     c                   when      err(xx) = 'E' or
027946060412     c                             err(xx) = 'K' or
027947060412     c                             err(xx) = 'M'
027948060412     c                   seton                                        1290
027949060412     c* se abi o cab errari abilito F2 di forzatura
027950060412     c                   when      (err(xx) = 'F' or
027951060412     c                             err(xx) = 'G') and indkb = ' '
027952060412     c                   seton                                        50
027953060412     c                   eval      v03msgw= 'ABI o CAB inesistenti F2 per for-
027954060412     c                             zarli'
027955060412     c* data assegno errato
027956060412     c                   when      err(xx) = 'H' or
027957060412     c                             err(xx) = 'I' or
027958060414     c                             err(xx) = 'W' or
027959060412     c                             err(xx) = 'N'
027960060412     c                   seton                                        1390
027961060414     C                   OTHER
027962060414     c                   seton                                        90
027963060414     c                   eval      v03msgw= msg(xx)
0279640604124    c                   endsl
0279650604123    c                   enddo
027966060412     c                   else
027967060412     c                   eval      ds1a = tbluni
0279680604121    c                   end
0279690604121    c                   enddo
027970060412     c*
027971060412     C                   ENDSR
027972060412      *---------------------------------------------------------*
027973060412      *  Controllo c/ass. o assegnato                           *
027974060412      *---------------------------------------------------------*
027975060412     C     CTRDIS        BEGSR
027976060310     c                   clear                   v03msg
027977060412     c                   clear                   fiar6ds
027978060412     c                   clear                   fiar9ds
027979060412     c                   clear                   datifat
027980060412     c                   clear                   datica
028000060228     c                   do
028100060228     C* controllo il mandato d'intrito
028200060309     c                   if        arbnmi <>0
028300060228     c                   seton                                        0690
028400060228     c                   leave
028500060228     c                   end
028600060228     C* Controllo se linea di arrivo abilitata
028700060228     C     ARBLNA        LOOKUP    LINT                                   70
028800060228     c                   if        not *in70
028900060915     C                   SETON                                        0490
029000060228     c                   leave
029100060228     c                   end
030400060302     C* Controllo se esiste bolla mamma
030500060310     c                   clear                   fnarbmds
030600060228     C     KSPE          CHAIN     FNLBL01L
030700060228     c* per cambio di porto il contrassegno è sulla mamma
030800060228     C                   IF        %found(fnlbl01l) and lbllan = lbllap
030900060224     C                   Z-ADD     LBLAAP        KAAS
031000060224     C                   Z-ADD     LBLLPP        KLNP
031100060224     C                   Z-ADD     LBLNRP        KNRS
031200060224     C                   Z-ADD     LBLNSP        KNSP
031300060301     C     KSPE          CHAIN(n)  FNARB01L
031400060224     C* Memorizzo il legame esistente per i successivi accessi su FNARB
031500060228     C                   IF        %found(fnarb01l) and arbcca <> '2' and
031600060228     C                             arbcca <> '6'
031700060301     C                   movel     fnarbds       fnarbmds
031800060224     C                   END
031900060301     C                   movel     fnarbdssav    fnarbds
032000060224     C                   END
032100060309     C* controllo se c'è il contrassegno o la fattura e che siano incassati
032200060309     C                   Z-ADD     1             X
032300060309     C                   CLEAR                   DS3A
032400060309     C     ARBCBO        LOOKUP    C3A(X)                                 95
032500060309     C     *IN95         IFEQ      '1'
032600060309     C                   MOVEL     D3A(X)        DS3A
032700060309     C                   END
032800060309     c                   eval      tbluni = ds3a
032900060418     c* se scelta cambio tipo incasso e il tipo documento non è solo
033000060418     c* contrassegno errore
033001060418     c                   movel     §3atb1        porto             1
033100060418     c                   if        v03sce = '1' and porto <> 'F'
033300060418     c                   seton                                        1590
033400060309     c                   leave
033500060309     c                   end
033501060418     c* se non sono previsti incassi con fattura immediata e
033502060418     c* non ho contrassegni
033503060418     c                   if        §3ati1 <> 'A' and §3ati2 <> 'A' and
033504060418     c                             §3afca = 0
033505060418     c                   seton                                        1690
033506060418     c                   leave
033507060418     c                   end
033600060309     c* controllo bolla
033900060307     c                   callp     CHKBOLLAincassata(
034000060307     c                             arbaas:arblnp:arbnrs:arbnsp
034100060310     c                             :arbcbo:tbluni:v03tpd:esito:err:msg
034200060310     c                             :datifat:datica)
034300060310     c* se errore oppure contrassegno sulla mamma
034400060321     c                   if        esito <>0 and marbaas <> 0
034500060309     c                   clear                   esito
034600060309     c                   callp     CHKBOLLAincassata(
034700060309     c                             marbaas:marblnp:marbnrs:marbnsp
034800060310     c                             :marbcbo:*omit:v03tpd:esito:err:msg
034900060310     c                             :*omit:datica)
035000060309     c                   end
035100060310     C*
0352000603101    c                   if        esito = 0
035300060310     c                   movel     datifat       fiar6ds
035400060310     c                   movel     datica        fiar9ds
035500080521     c* ASSEGNATO no segue fattura
0356000805222    c                   if        ar6aas <> 0
035700060309     c* senza contrassegno
0358000603103    c                   if        §3afca = 0
035900080522     c* segue fattura
036000080522     c                   if        ar6dft = 0
036100080522     c                   seton                                        1690
036200060228     c                   leave
036300060227     C                   END
036301080522     c* assegnato non incassato
036302080522     c                   if        arbica = ' ' or arbica = 'R'
036303080522     c                   seton                                        0790
036304080522     c                   leave
036305080522     C                   END
036400060309     c* assegnato già contabilizzato
036500060309     c                   if        arbica = 'Y'
036600060309     c                   seton                                        0690
036700060309     c                   leave
036800060309     C                   END
036900060309     c* ASSEGNATO + C/ASSEGNO
037000060310x3   c                   else
037100060309     c* assegnato e contrassegno non incassati
037200060309     c                   if        (arbica = ' ' or arbica = 'R') and
037300060309     c                             (arbicc = ' ' or arbicc = 'R')
037400060322     c                   seton                                        079008
037500060309     c                   leave
037600060309     C                   END
037700060309     c* assegnato e contrassegno già contabilizzati
037800060309     c                   if        arbica = 'Y' and
037900060309     c                             arbicc = 'Y'
038000060322     c                   seton                                        0690
038100060309     c                   leave
038200060309     C                   END
0383000603103    C                   END
038400060310x2   c                   else
038500060309     c* SOLO CONTRASSEGNO
0386000603103    C     §3AFCA        IFGT      0                                            --- 01 ---
038700060309     c* c/assegno non incassato
038800060309     c                   if        (arbicc = ' ' or arbicc = 'R')
038900060321     c                   seton                                        0890
039000060309     c                   leave
039100060309     C                   END
039200060309     c* c/assegno già contabilizzato
039300060309     c                   if        arbicc = 'Y'
039400060322     c                   seton                                        0690
039500060309     c                   leave
039600060309     C                   END
0397000603103    C                   END
0398000603102    C                   END
039900060310x1   c                   else
040000060310     c                   do        10            xx
040100060310     c                   if        err(xx) = ' '
040200060310     c                   leave
040300060310     c                   end
040400060310     c                   movel     msg(xx)       v03msg
040500060310     c                   seton                                        90
040600060310     c                   leave
0407000603102    C                   ENDdo
0408000603101    c                   end
040803060412     c* se scelto modifica tipo incasso
040804060412     c     kass1         chain     cnass01l
040805060414     c                   if        %found(cnass01l) and asstpd = 'C'
041001060412     c* imposto i campi tipo incasso prima se scelta errore intestazione
041005060412     c                   eval      v03nrap = assnra
041007060412     c                   eval      v03abip = assabi
041009060412     c                   eval      v03cabp = asscab
041017060412     c                   if        assdte <> 0
041018060412     c                   movel     assdte        dtaiso
041019060412     c                   movel     dtaiso        dtaeur
041020060412     c                   movel     dtaeur        v03dtep
041022060412     c                   else
041023060412     c                   eval      v03dtep = 0
041025060412     c                   end
041026060412     c                   end
041027060412     c* tipo incasso prima
041028060412     c                   eval      v03ticp = ar9tic
041031060412     c                   exsr      srticp
041035060412     c*
041036060412     C                   ENDdo
041100060224     C*
041200060224     C                   ENDSR
041300060228      *---------------------------------------------------------*
041400060228      *  Controllo prepagato                                    *
041500060228      *---------------------------------------------------------*
041600060228     C     CTRPRE        BEGSR
041700060308     c                   setoff                                       50
041800060308     c                   clear                   v03msg
041900060228     c                   do
042000060228     C* controllo il mandato d'intrito
042100060228     c                   if        incnmi <>0
042200060228     c                   seton                                        0690
042300060228     c                   leave
042400060228     c                   end
042500060228     C* Controllo se linea di arrivo abilitata
042600060228     C     inclnp        LOOKUP    LINT                                   70
042700060228     c                   if        not *in70
042800060228     C                   SETON                                        0490
042900060228     c                   leave
043000060228     c                   end
043100060301     c* controllo spedizione
043200060307     c                   callp     CHKBOLLAincassata(
043300060307     c                             incaas:inclnp:incnrs:incnsp
043400060310     c                             :*omit:*omit:v03tpd:esito:err:msg
043500060307     c                             :*omit:*omit)
043600060321     c                   if        esito <>0
043700060310     c                   do        10            xx
043800060310     c                   if        err(xx) = ' '
043900060310     c                   leave
044000060310     c                   end
044100060310     c                   movel     msg(xx)       v03msg
044200060310     c                   seton                                        90
044300060310     c                   leave
0444000603102    C                   ENDdo
044500060301     c                   end
044600060301     c* pre-pagato non incassato
044700060301     c                   call      'YCOW0121R'
044800060307     C                   PARM                    incAas
044900060307     C                   PARM                    incLnp
045000060307     C                   PARM                    incNrs
045100060307     C                   PARM                    incNsp
045200060307     C                   PARM                    incFiv
045300060307     C                   PARM                    incDft
045400060307     C                   PARM                    incNft
045500060307     C                   PARM                    prmEsito
045600060307     c                   if        prmesito = 0
045700060301     C                   SETON                                        0990
045800060228     c                   leave
045900060228     c                   end
046000060307     c* imposto i campi tipo incasso prima
046100060307     c                   eval      v03ticp = inctii
046200060307     c                   eval      v03nrap = incnra
046300060307     c                   eval      v03abip = incabi
046400060307     c                   eval      v03cabp = inccab
046500060308     c                   if        incdte <> 0
046600060307     c                   movel     incdte        dtaiso
046700060307     c                   movel     dtaiso        dtaeur
046800060307     c                   movel     dtaeur        v03dtep
046900060308     c                   else
047000060308     c                   eval      v03dtep = 0
047100060308     c                   end
047101060412     c* tipo incasso prima
047102060412     c                   exsr      srticp
059300060228     C                   ENDdo
059400060228     C*
059500060228     C                   ENDSR
059600030730     C*-----------------------------------------------------*
059700060412     C* srticp  - tipo incasso prima                        *
059800030730     C*-----------------------------------------------------*
059900060412     c     srticp        begsr
059901060412     C*
059902060412     C*  tipo incasso prima
059903060412     c                   clear                   v03dtip
059904060412     C                   MOVEL     '1A'          tblCOD
059905060412     C                   MOVEl(p)  v03ticp       tblkey
059906060412     c     ktab1         chain     tabel00f
059907060412     c                   if        %found(tabel00f)
059908060412     c                   movel     tbluni        ds1a
059909060412     C                   MOVEL     §1ades        V03dtip
059910060412     c                   end
059911060412     C*
059912060412     C                   ENDSR
059913060412     C*-----------------------------------------------------*
059914060412     C* srticp  - tipo incasso dopo                         *
059915060412     C*-----------------------------------------------------*
059916060412     c     srticd        begsr
059918060412     C*  tipo incasso prima
059919060412     c                   clear                   v03dtid
059920060412     C                   MOVEL     '1A'          tblCOD
059921060412     C                   MOVEl(p)  v03ticd       tblkey
059922060412     c     ktab1         chain     tabel00f
059923060412     c                   if        %found(tabel00f)
059924060412     c                   movel     tbluni        ds1a
059925060412     C                   MOVEL     §1ades        V03dtid
059926060412     c                   else
059927060412     C                   SETON                                        1090
059929060412     c                   end
059931060412     C*
059932060412     C                   ENDSR
059933060412     C*-----------------------------------------------------*
059934060412     C* sraggio - aggiorno FNARB, anomalie ed eventi        *
059935060412     C*-----------------------------------------------------*
059936060412     c     sraggio       begsr
059937060413     c* ERRATO INCASSO
060000060301     C* tipo c/assegno o assegnato aggiorno FNARB come ritorno all'incasso
060100060301     c* e cancello CNASS
0602000604121    c                   if        v03tpd <>'P' and v03sce = '2'
060300060301     C                   Z-ADD     v03aas        KAAS
060400060301     C                   Z-ADD     v03lnp        KLNP
060500060307     C                   Z-ADD     v03NRs        KNRS
060600060301     C                   Z-ADD     v03NSP        KNSP
060700060301     C     Kspe          CHAIN     FNARB01L
0608000604122    c                   if        %found(fnarb01l)
060900080521     c                   if        arbica <> ' ' and ar6dft <> 0 and
060901080521     c                             (§3ati1 = 'A' or §3ati2 = 'A')
061000060301     c                   eval      arbica = 'R'
061100060301     c                   end
0612000805213    c                   if        arbicc <> ' ' and
061201080521     c                             §3afca <> 0
061300060301     c                   eval      arbicc = 'R'
061301060414     c                   clear                   arbmif
0614000604123    c                   end
061500030730     C                   UPDATE    FNARB000
0616000604122    c                   end
062001060726     c     kass1         setll     cnass01l
062002060726     c                   do        *hival
062003060726     c     kass1         reade     cnass01l
062004060726     c                   if        %eof(cnass01l)
062005060726     c                   leave
062006060726     c                   end
062007060413     C                   delete    cnass000
062008060726     c                   enddo
062009060414     c* FIARI
062010060414     c     kspe          setll     fiari01l
062011060414     c                   do        *hival
062012060414     c     kspe          reade     fiari01l
062013060414     c                   if        %eof(fiari01l)
062014060414     c                   leave
062015060414     c                   end
062016060414     c                   if        ARIIMP <> 0
062017060414     c                   if        ARIIPP = 0
062018060414     C                   delete    fiari000
062019060414     c                   else
062020060414     c                   clear                   ariimp
062021060414     C                   update    fiari000
062022060414     c                   end
062023060414     c                   end
062024060414     c                   enddo
0620250604121    c                   end
062026060414     c* a prescindere dalla scelta aggiorno il tipo incasso del contrassegno
0620270604141    c                   if        v03tpd = 'C' or v03tpd = 'T'
062028060414     C                   Z-ADD     v03aas        KAAS
062029060414     C                   Z-ADD     v03lnp        KLNP
062030060414     C                   Z-ADD     v03NRs        KNRS
062031060414     C                   Z-ADD     v03NSP        KNSP
062032060414     c     kspe          chain     fiar901l
0620330604142    c                   if        not %found(fiar901l) and marbaas <> 0
062034060414     C                   Z-ADD     marbaas       KAAS
062035060414     C                   Z-ADD     marblnp       KLNP
062036060414     C                   Z-ADD     marbnrs       KNRS
062037060414     C                   Z-ADD     marbnsp       KNSP
062038060414     c     kspe          chain     fiar901l
0620390604142    c                   end
0620400604142    c                   if        %found(fiar901l)
062041060414     c                   if        v03sce = '1'
062042060414     c                   eval      ar9tic = v03ticd
062043060414     c                   else
062044060414     c                   clear                   ar9tic
062045060414     c                   end
062046060414     c                   update    fiar9000
0620470604142    c                   end
0620480604141    c                   end
062049060413     c* CAMBIO TIPO INCASSO
062050060413     c                   if        v03sce = '1'
062100060301     C* tipo pre-pagato aggiorno FIINC con i nuovi dati d'incasso
0623000604121    c                   if        v03tpd = 'P'
062400060307     C     Kinc          CHAIN     Fiinc01L
0625000604122    c                   if        %found(fiinc01l)
062600060307     c                   eval      inctii = v03ticd
062700060307     c                   eval      INCNRA = nra
0628000604123    c                   if        v03dted <> 0
062900060307     c                   move      dtaiso        INCDTE
063000060412x3   c                   else
063100060308     c                   eval      incdte = 0
0632000604123    c                   end
063300060307     c                   eval      INCABI = v03abid
063400060307     c                   eval      INCCAB = v03cabd
063500060301     c                   eval      INCTPA = §1AFCA
063501060413     c                   if        §1afmb = 'B'
063502060413     c                   eval      inctpi = ' '
063503060413     c                   else
063600060412     c                   eval      INCTPI = §1AFMB
0636010604132    c                   end
063700060301     C                   UPDATE    Fiinc000
0638000604122    c                   end
0638010604121    c                   end
063802060412     c* sia per pre-pagati che per contrassegni aggiorno il cnass
0638030604121    c                   if        v03tpd = 'P' or v03tpd = 'C'
063900060307     c     kass1         chain     cnass01l
064000060308     c* se il nuovo tipo incasso non prevede assegno deleto nel caso ci sia
0641000604122    c                   if        §1asas = ' '
064200060414     c                   if        %found(cnass01l) and asstpd = v03tpd
064300060308     c                   delete    cnass000
064400060308     c                   end
064500060412x2   c                   else
064600060412     c                   eval      assnra = nra
0647010604123    c                   if        v03dted <> 0
064702060412     c                   move      dtaiso        assDTE
064703060412x3   c                   else
064704060412     c                   eval      assdte = 0
0647050604123    c                   end
064800060412     c                   eval      assABI = v03abid
064900060412     c                   eval      assCAB = v03cabd
064901060418     c                   eval      assTPA = §1afca
064902060414     c                   if        §1afmb = 'B'
064903060414     c                   eval      asstpi = ' '
064904060414     c                   else
064905060412     c                   eval      asstpi = §1AFMB
064906060414     c                   end
064907060412     C* per contrassegno lo deve trovare
0650000604143    c                   if        %found(cnass01l) and asstpd = v03tpd
065100060301     C                   update    cnass000
065200060412x3   c                   else
065300060412     c                   eval      assFle = dutpou
065400060412     c                   eval      assTpd = v03Tpd
065500060412     c                   eval      assAas = v03Aas
065600060412     c                   eval      assLnp = v03Lnp
065700060412     c                   eval      assNrs = v03Nrs
065800060412     c                   eval      assNsp = v03Nsp
065801060412     c                   if        v03tpd = 'P'
065900060301     c                   eval      assCas = incImp
066000060301     c                   eval      assVca = incDiv
066001060412     c                   else
066002060412     c                   eval      assCas = ar9cas
066003060412     c                   eval      assVca = ar9vca
066004060412     c                   end
066100060412     c                   eval      assTpa = §1afca
066102060414     c                   if        §1afmb = 'B'
066103060414     c                   eval      inctpi = ' '
066104060414     c                   else
066200060412     c                   eval      asstpi = §1AFMB
0662010604143    c                   end
066202060412     c                   eval      assfve = ' '
066300060324     c                   eval      assftr = ' '
066400060301     C                   write     cnass000
0665010604123    c                   end
0666000604122    c                   end
0667000604121    c                   end
0667200604131    c                   end
066800030730     c                   commit
066900030730     c*
067000030730     c                   endsr
067100940322     C*-----------------------------------------------------*
067200060915     C*    carico £6 del po incasso
067300941107     C*-----------------------------------------------------*
067400060915     C     sr£6          BEGSR
067501060915     C* Aggancio la £6 e recupero le linee a loro associate
067502060915     c                   clear                   trul06ds
067503060915     c                   eval      d06cod = '£6'
067504060915     c                   movel     dutpouc       d06key
067505060915     c                   movel     trul06ds      kpjbu
067506060915     c                   call      'TRUL06R'
067507060915     c                   parm                    kpjba
067508060915     c                   movel     kpjbu         trul06ds
067509060915     c*
067510060915     c                   endsr
067511060915     C*-----------------------------------------------------*
067512060915     C*    Operazioni iniziali
067513060915     C*-----------------------------------------------------*
067514060915     C     *INZSR        BEGSR
067515060915     C*
067600940322     C     *ENTRY        PLIST
067700940322     C                   PARM                    KPJBA
067800020905     c*
067900020905     c     *dtaara       define    §azute        azuteds
068000020905     c     *dtaara       define    §datiute      ddatiute
068100020905     C                   in(E)     *dtaara
068200020905     C                   IF        %Error  or  RSUT = *blanks
068300020905     C                   call      'TIBS34R'
068400020905     C                   parm                    Tibs34Ds
068500020905     C                   in        *dtaara
068600020905     c                   ENDIF
068601060915     c* carico £1
068602060915     c                   clear                   trul06ds
068603060915     c                   eval      d06cod = '£1'
068604060915     c                   movel     dutpou        d06key
068605060915     c                   movel     trul06ds      kpjbu
068606060915     c                   call      'TRUL06R'
068607060915     c                   parm                    kpjba
068608060915     c                   movel     kpjbu         trul06ds
068609060915     c                   movea     lint          £1
068610060915     c* carico £6
068611060915     c                   move      dutpou        dutpouc
068612060915     c                   exsr      sr£6
068613060915     c* se primo livello abilito f18 cambio po incasso
069401060915     C     Dutlpo        IFEQ      '1'
069402060915     C                   SETON                                        44
069403060915     C                   END
069500940322      *---------------------------------------------------------------*
069600940322      * CHIAVI                                                        *
069700940322      *---------------------------------------------------------------*
069800060307     C     KTAB          KLIST
069900060307     C                   KFLD                    tblKUT
070000060307     C                   KFLD                    tblCOD
070100060307     C     KTAB1         KLIST
070200060307     C                   KFLD                    tblKUT
070300060307     C                   KFLD                    tblCOD
070400060307     C                   KFLD                    tblkey
070500060307     C                   Z-ADD     1             tblKUT
070600060307     C     KASS1         KLIST
070700060307     C                   KFLD                    dutpou
070800060307     C                   KFLD                    v03lnp
070900060307     C                   KFLD                    v03aas
071000060307     C                   KFLD                    v03nrs
071100060307     C                   KFLD                    v03nsp
071200060307     C     KASS2         KLIST
071300060307     C                   KFLD                    dutpou
071400060307     C                   KFLD                    nra
071500060307     C                   KFLD                    v03abid
071600060307     C                   KFLD                    v03cabd
071700060301     C     Kspe          KLIST
071800060301     C                   KFLD                    KAAS
071900060301     C                   KFLD                    KLNP
072000060301     C                   KFLD                    KNRS
072100060301     C                   KFLD                    KNSP
072200060301     C     Kinc          KLIST
072300060301     C                   KFLD                    v03AAS
072400060301     C                   KFLD                    v03LNP
072500060301     C                   KFLD                    v03NRS
072600060301     C                   KFLD                    v03NSP
072700060307     C                   KFLD                    v03tpd
072800030730     C*
072900060301     C                   MOVEL     'CNC0W5R'     VIDPGM
073000060301     C                   MOVE      *year         V03AAS
073100060307     c*
073200060307     C                   Z-ADD     0             X                 3 0
073300060307     C                   MOVEL     '3A'          tblCOD
073400060307     C     KTAB          setll     TABEL00F
073500060307     c                   do        *hival
073600060307     C     KTAB          reade     TABEL00F
073700060307     C                   if        %eof(tabel00f)
073800060307     c                   leave
073900060307     c                   end
074000060307     C     X             iflt      100
074100060307     C     TBLFLG        andeq     ' '
074200060307     C                   ADD       1             X
074300060307     C                   MOVEL     TBLKEY        C3A(X)
074400060307     C                   MOVEL     TBLUNI        D3A(X)
074500060307     C                   END
074600060307     C                   ENDdo
074601060412     c*
074602060412     c                   eval      v03sce = '1'
074700940322     C*
074800940322     C                   ENDSR
