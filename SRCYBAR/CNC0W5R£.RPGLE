000100060307     H DFTACTGRP(*NO) ACTGRP('QILE') BNDDIR('PJXBND':'TRULCIF')
000200060302     H DECEDIT('0,') DATEDIT(*DMY.)
000300060224      * -------------------------------------------------------------*
000400060224      *     - cambia tipo incasso se non eseguita ancora chiusura op.*
000500960702     F*--------------------------------------------------------------*
000600060224     Fcnc0w5D   CF   E             WORKSTN
000700060224     FFNARB01L  UF   E           K DISK    commit
000800060227     FFiinc01L  uF   E           K DISK    commit
000900060224     Fcnass01L  uF a E           K DISK    commit
001000120220     Ffncss03L  uF   E           K DISK    commit
001100120125     Ftncsm03L  uF   E           K DISK    commit
001200120220     Ffncss01L  iF   E           K DISK    commit rename(fncss000:fncss001)
001201120322     Ffiar501l  iF   E           K DISK
001300060412     Ffiar901L  uF a E           K DISK    commit
001400060414     Ffiari01L  uF   E           K DISK    commit
001500060224     FFnlbl01L  iF   E           K DISK
001600111110     FFnlbl02L  iF   E           K DISK    rename(fnlbl000:fnlbl002)
001700030730     FTABEL00F  iF   E           K DISK
001800940321      *---------------------------------------------------------------
001900030730     d* schiere
002000030730      *---------------------------------------------------------------
002100030730     d* ds esterne
002200030730      *---------------------------------------------------------------
002300030730     D KPJBA         E DS
002301120322     D dar5gen       e ds                  inz
002400030730     D ddatiute      e ds
002500060301     D fnarbdssav    e ds                  extname(fnarb00F) prefix(s)
002600120220     D fncssds       e ds                  extname(fncss00F)
002700111109     D fnarbds       e ds                  extname(fnarb00F)
002800060301     D fnarbmds      e ds                  extname(fnarb00F) prefix(m)
002900060301     D azuteds       e ds                  extname(AZUTE00F)
003000060309     D fiar6ds       e ds                  extname(fiar600F)
003100060309     D fiar9ds       e ds                  extname(fiar900F)
003200030730     D tibs34ds      E DS                  inz
003300060307     D ds3a          E DS                  inz
003400060307     D ds1a          E DS                  inz
003500060307     D ds4f          E DS                  inz
003600120210     D TNSC30DS      E DS                  inz
003700060301     c*po gestione
003800060301     d trul06ds      e ds
003900060301     d  lint                   1     90  0 dim(30)
004000060915     d  £1             s              3  0 dim(30)
004100060224      * DS per richiamo pgm FNLR36R
004200060224     D DSLR36          DS
004300060224     D  P36AAS                14     17  0
004400060224     D  P36LNP                18     20  0
004500060224     D  P36NRS                21     22  0
004600060224     D  P36NSP                23     29  0
004700060224     D  P36F03                30     30
004800060224     D  P36FLG                31     31
004900030730     d dtaeur          s               d   datfmt(*eur)
005000030730     d dtaiso          s               d
005100060915     D dutpouc         s                   LIKE(dutpou)
005200060307     D chkBollaIncassata...
005300060307     D                 PR            10I 0
005400060307     D  aas                                LIKE(arbaas)
005500060307     D  lnp                                LIKE(arblnp)
005600060307     D  nrs                                LIKE(arbnrs)
005700060307     D  nsp                                LIKE(arbnsp)
005800060307     D  cbo                                LIKE(arbcbo)
005900060307     D                                     OPTIONS(*OMIT)
006000060307     D  datiCbo                      89A
006100060310     D                                     OPTIONS(*OMIT)
006200060307     D  tpd                           1A
006300060307     D  esito                        10I 0
006400060307     D  err                           1A   DIM(10)
006500060307     D  msg                          80A   DIM(10)
006600060307     D                                     OPTIONS(*OMIT)
006700060310     D  datiFattura               32767A
006800060307     D                                     OPTIONS(*OMIT:*VARSIZE)
006900060307     D                                     VARYING
007000060310     D  datiContrassegno...
007100060307     D                            32767A
007200060307     D                                     OPTIONS(*OMIT:*VARSIZE)
007300060307     D                                     VARYING
007400060307     D chkIncassoPO    PR            10I 0
007500060321     D  soc                           3a
007600060321     D                                     CONST
007700060321     D  fgs                           3  0
007800060307     D  tic                                LIKE(inctii)
007900060307     D  tpd                           1A   CONST
008000060307     D  dati1a                             LIKE(tblUni)
008100060307     D                                     OPTIONS(*OMIT)
008200060321     D  datiY4O                     256A
008300060321     D                                     OPTIONS(*OMIT)
008400060307     D  nra                                LIKE(assnra)
008500060307     D                                     OPTIONS(*OMIT)
008600060307     D  abi                                LIKE(assabi)
008700060307     D                                     OPTIONS(*OMIT)
008800060307     D  cab                                LIKE(asscab)
008900060307     D                                     OPTIONS(*OMIT)
009000060307     D  dte                          10D   DATFMT(*ISO)
009100060307     D                                     OPTIONS(*OMIT)
009200060307     D                                     CONST
009300060307     D  esito                        10I 0
009400060307     D                                     OPTIONS(*OMIT)
009500060307     D  err                           1A
009600060307     D                                     DIM(10)
009700060307     D  msg                          80A   OPTIONS(*OMIT)
009800060307     D                                     DIM(10)
009900060321     D  prmKpjba                           OPTIONS(*OMIT:*NOPASS)
010000060321     D                                     LIKEDS(kpjba)
010100060307     D prmEsito        S             10I 0
010200060307     D esito           S             10I 0
010300111110     D cassm           S              1
010400060307     D err             S              1    DIM(10)                              SKI CHIAVI
010500060310     D msg             S             80    DIM(10)                              SKI CHIAVI
010600060307     D C3A             S              2    DIM(100)                             SKI CHIAVI
010700060310     D D3A             S                   like(tbluni) DIM(100)                SKI TAB. 3
010800060321     D fgs             S                   like(arblnp)                         SKI TAB. 3
010900060307     D nra             S                   like(assnra)                         SKI TAB. 3
011000060307     D kaas            S                   like(arbaas)                         SKI TAB. 3
011100060307     D klnp            S                   like(arblnp)                         SKI TAB. 3
011200060307     D knrs            S                   like(arbnrs)                         SKI TAB. 3
011300060307     D knsp            S                   like(arbnsp)                         SKI TAB. 3
011400060308     D savabi          S                   like(assabi)                         SKI TAB. 3
011500060308     D savcab          S                   like(asscab)                         SKI TAB. 3
011600111025     D savaas          S                   like(assaas)                         SKI TAB. 3
011700111025     D savlnp          S                   like(asslnp)                         SKI TAB. 3
011800111025     D savnrs          S                   like(assnrs)                         SKI TAB. 3
011900111025     D savnsp          S                   like(assnsp)                         SKI TAB. 3
012000060310     Ddatifat          s          32767A
012100060310     D                                     VARYING
012200060310     Ddatica           s          32767A
012300060310     D                                     VARYING
012400911016      *---------------------------------------------------------------*
012500911016      * FLUSSO PRINCIPALE                                             *
012600911016      *---------------------------------------------------------------*
012700030730     C                   DO        *HIVAL
012800030730      *
012900060308     c                   if        *in90
013000060308     c                   setoff                                       90
013100030730     C                   write     LR98D01
013200030730     c                   seton                                        90
013300060308     c                   end
013400030730     c*
013500030730     C                   EXFMT     LR98D01
013600030730     c* fine lavoro
013700941107     C     *INKC         IFEQ      '1'
013800030730     C                   LEAVE
013900941107     C                   END
014000030730     c* controlli
014100030730     c                   exsr      srctr
014200060412     c   kc              leave
014300060412     c   90              iter
014400111025     c* non procedo con l'aggiornamento se ho + assegni x una spedizione
014500111103     c* o più spedizioni per un assegno. F8 x forzare la riapertura di
014600111025     c* tutte le spedizioni o la cancellazione di tutti gli assegni
014700111108     c                   if        *in51 and indkh = ' '
014800111025     c                   iter
014900111025     c                   end
015000030730     c*aggiorna
015100111104     c                   if        *inkf or *inkh
015200030730     c                   exsr      sraggio
015300030730     c   90              iter
015400030730     c*inizializza
015500030730     c                   exsr      srinz
015600030730     c                   end
015700030730     c                   enddo
015800941107     C*
015900030730     C                   SETON                                        LR
016000030730     c**********************************************************************
016100030730     C     srinz         BEGSR
016200030730     c**********************************************************************
016300060224     C* Pulisco i campi che devo visualizzare nel fm2
016400060301     C                   MOVE      *year         V03AAS
016500060224     C                   clear                   V03LNP
016600060224     C                   clear                   V03NRS
016700060224     C                   clear                   V03NSP
016800060307     C                   clear                   V03tpd
016900060307     C                   clear                   V03dtd
017000060412     C                   eval       V03sce = '1'
017100060227     C                   clear                   V03ticp
017200060227     C                   clear                   V03dtip
017300060307     C                   clear                   V03nrap
017400060227     C                   clear                   V03abip
017500060227     C                   clear                   V03cabp
017600060307     C                   clear                   V03dtep
017700060224     C                   clear                   V03ticd
017800060227     C                   clear                   V03dtid
017900060307     C                   clear                   V03nrad
018000060224     C                   clear                   V03abid
018100060224     C                   clear                   V03cabd
018200060307     C                   clear                   V03dted
018300060307     c                   clear                   indkb             1
018400111025     c                   clear                   indkh             1
018500060308     c                   clear                   savabi
018600060308     c                   clear                   savcab
018700111025     c                   clear                   savaas
018800111025     c                   clear                   savlnp
018900111025     c                   clear                   savnrs
019000111025     c                   clear                   savnsp
019100111109     c                   setoff                                       5152
019200030730     c                   endsr
019300030730     c**********************************************************************
019400030730     C     srctr         BEGSR
019500030730     c**********************************************************************
019600030620     C                   SETOFF                                       90
019700060412     c                   clear                   v03msg
019800030730     c                   do
019900111025     C*F8-forzatura x + ass/sped
020000111025     C                   if        *inkh
020100111025     c                   eval      indkh = '1'
020200111025     C                   END
020300111025     C*F9-interrogazione + ass/sped
020400111025     C                   if        *inki
020500111025     c                   exsr      intasssped
020600111025     C                   END
020700030730     C*F7-INTERROGAZIONE BOLLE ARRIVI
020800030730     C                   if        *inkg
020900941109     C                   MOVEL     '2'           P36FLG
021000060301     C                   Z-ADD     V03AAS        P36AAS
021100941108     C                   Z-ADD     V03LNP        P36LNP
021200941108     C                   Z-ADD     V03NSP        P36NSP
021300941108     C                   Z-ADD     V03NRS        P36NRS
021400941109     C                   MOVEL     ' '           P36F03
021500941109     C     P36NSP        IFGT      0
021600941109     C                   MOVEL     '1'           P36FLG
021700941109     C                   END
021800941108     C                   MOVEL     DSLR36        KPJBU
021900941108     C                   CALL      'FNLR36R'
022000911128     C                   PARM                    KPJBA
022100941109     C                   MOVEL     KPJBU         DSLR36
022200941109     C     P36F03        IFNE      '1'
022300941215     C     V03NSP        ANDEQ     0
022400060301     C                   Z-ADD     P36AAS        V03AAS
022500941109     C                   Z-ADD     P36LNP        V03LNP
022600941109     C                   Z-ADD     P36NRS        V03NRS
022700941109     C                   Z-ADD     P36NSP        V03NSP
022800941109     C                   END
022900911128     C                   SETON                                        90
023000030730     c                   leave
023100911128     C                   END
023200060307     C* tipo documento
023300060321     c                   clear                   v03dtd
023400060321     c                   setoff                                       15
023500060307     C     V03tpd        IFEQ      *blanks
023600060310     C     v03tpd        oreq      'V'
023700060307     C                   SETON                                        1590
023800030730     c                   leave
023900060307     c                   else
024000060412     C* RICHIESTA tipo documento
024100060412     C     '?'           SCAN      v03tpd                                 89
024200060412IF  2C     *IN89         IFEQ      *ON
024300060412     c                   eval      codut = 1
024400060412     C                   MOVEL     '4F'          COD
024500060412     C                   MOVEL     *BLANKS       KEY
024600060412     C                   MOVEL     *BLANKS       DES
024700060412     C                   CALL      'X§TABER'
024800060412     C                   PARM                    CODUT             1 0
024900060412     C                   PARM                    COD               2
025000060412     C                   PARM                    KEY               8
025100060412     C                   PARM                    DES              25
025200060412     C                   MOVEL     KEY           V03tpd
025300060412    2C                   END
025400060307     c                   movel     '4F'          tblcod
025500060307     c                   movel(p)  v03tpd        tblkey
025600060307     c     ktab1         chain     tabel00f
025700060307     c                   if        %found(tabel00f)
025800060307     c                   movel     tbluni        ds4f
025900060307     c                   movel     §4fdes        v03dtd
026000060307     c                   else
026100060307     C                   SETON                                        1590
026200060307     c                   leave
026300911017     C                   END
026400060307     C                   END
026500060412     c* scelta
026600060412     c                   if        v03sce = '1' and
026700060412     c                             v03tpd <> 'P' and v03tpd <> 'C'
026800060412     C                   SETON                                        90
026900060412     c                   eval      v03msg = 'La rettifica del tipo incasso -
027000060412     c                             è ammessa solo per pre-pagati o c/assegni'
027100060412     c                   leave
027200060412     C                   END
027300060412     c*
027400060412     c                   if        v03sce = '2' and v03tpd = 'P'
027500060412     C                   SETON                                        90
027600060412     c                   eval      v03msg = 'La rettifica errato incasso -
027700060412     c                              non è ammessa per pre-pagati'
027800060412     c                   leave
027900060412     C                   END
028000060915     C*F18 - cambio po incasso
028100060915     C                   if        *inks
028200060915     C                   SETON                                        45
028300060915     c                   leave
028400060915     c                   end
028500060915     c* controllo che il po scelto sia nell'area
028600060915     c                   if        *in45 and v03lna <> dutpou
028700060915     c                   z-add     1             xx
028800060915     c     v03lna        lookup    £1(xx)                                 19
028900060915     c                   if        not *in19
029000060915     C                   SETON                                        1890
029100060915     c                   leave
029200060915     c                   end
029300060915     c                   eval      dutpouc = v03lna
029400060915     c                   exsr      sr£6
029500060915     c                   end
029600060307     C* Controllo se la spedizione è esistente o valida su video
029700060307     C     V03AAS        IFEQ      *ZEROS
029800060307     C                   SETON                                        0190
029900060307     c                   leave
030000060307     C                   END
030100911017     C     V03LNP        IFEQ      *ZEROS
030200030730     C                   SETON                                        0290
030300030730     c                   leave
030400911017     C                   END
030500911017     C     V03NSP        IFEQ      *ZEROS
030600030730     C                   SETON                                        0390
030700030730     c                   leave
030800911017     C                   END
030900060228     C* AGGANCIO FNARB x c/ass o assegnato oppure FIINC x prepagati
031000060301     C                   Z-ADD     v03aas        KAAS
031100060301     C                   Z-ADD     v03lnp        KLNP
031200060307     C                   Z-ADD     v03NRs        KNRS
031300060301     C                   Z-ADD     v03NSP        KNSP
031400060307     c* prepagato
031500060307     c                   if        v03tpd = 'P'
031600060307     C     Kinc          CHAIN(n)  Fiinc01L
031700060307     c                   if        not %found(fiinc01l)
031800060307     c                   seton                                        0590
031900060307     c                   leave
032000060307     c                   else
032100060307     c                   exsr      ctrpre
032200060412     c   90              leave
032300060307     c                   end
032400060307     c                   else
032500060321     C                   clear                   V03ticp
032600060321     C                   clear                   V03dtip
032700060321     C                   clear                   V03nrap
032800060321     C                   clear                   V03abip
032900060321     C                   clear                   V03cabp
033000060321     C                   clear                   V03dtep
033100060321     C                   clear                   V03ticd
033200060321     C                   clear                   V03dtid
033300060321     C                   clear                   V03nrad
033400060321     C                   clear                   V03abid
033500060321     C                   clear                   V03cabd
033600060321     C                   clear                   V03dted
033700060227     C     Kspe          CHAIN(n)  FNARB01L
033800060307     c                   if        not %found(fnarb01l)
033900060307     c                   seton                                        0590
034000060307     c                   leave
034100060224     c                   else
034200060301     c                   movel     fnarbds       fnarbdssav
034300060227     c                   exsr      ctrdis
034400060412     c   90              leave
034500060307     c                   end
034600060307     c                   end
034700060412     c* se scelta tipo incasso o errore intestazione assegno emetto
034800060412     c* la window
034900060412     c                   if        v03sce = '1'
035000060412     c                   exsr      srwindow
035100060412     c                   end
035200060412     c                   enddo
035300030730     c*
035400911017     C                   ENDSR
035500060224      *---------------------------------------------------------*
035600060227      *  Controllo c/ass. o assegnato                           *
035700060224      *---------------------------------------------------------*
035800060412     C     srwindow      BEGSR
035900060412     c*
036000060412     C                   DO        *HIVAL
036100060412      *
036200060412     c                   if        *in90
036300060412     c                   setoff                                       90
036400060412     C                   write     LR98W01
036500060412     c                   seton                                        90
036600060412     c                   end
036700060412     c*
036800060412     C                   EXFMT     LR98W01
036900060412     c* fine lavoro
037000060412     C     *INKC         IFEQ      '1'
037100060412     C                   LEAVE
037200060412     C                   END
037300060412     c* controlli
037400060412     c                   exsr      srctrW
037500060412     c   kl              leave
037600060412     c   90              iter
037700060412     c* non procedo con l'aggiornamento se ho errore abi cab e non ho pre-
037800060412     c* muto f2 x forzarlo
037900060412     c                   if        *in50 and indkb = ' '
038000060412     c                   iter
038100060412     c                   end
038200060412     c*aggiorna
038300060412     c                   if        *inkf
038400060412     c                   exsr      sraggio
038500060412     c   90              iter
038600060412     c*inizializza
038700060412     c                   exsr      srinz
038800060412     c                   leave
038900060412     c                   end
039000060412     c                   enddo
039100060412     c*
039200060412     C                   ENDSR
039300060412      *---------------------------------------------------------*
039400060412      *  Controllo c/ass. o assegnato                           *
039500060412      *---------------------------------------------------------*
039600060412     C     srctrw        BEGSR
039700060412     C                   SETOFF                                       90
039800060412     c                   clear                   v03msgw
039900060412     c                   do
040000060412     C*F12= giuda
040100060412     C                   if        *inkl
040200060412     c                   leave
040300060412     C                   END
040400060412     C*F2-forzatura abi cab
040500060412     C                   if        *inkb
040600060412     c                   eval      indkb = '1'
040700060412     C                   END
040800060412     c* controllo che sia campiato qualcosa
040900060412     c                   if        v03ticp = v03ticd and
041000060412     c                             v03nrap = v03nrad and
041100060412     c                             v03abip = v03abid and
041200060412     c                             v03cabp = v03cabd and
041300060412     c                             v03dtep = v03dted
041400060412     c                   seton                                        90
041500060412     c                   eval      v03msgw= 'I riferimenti incasso prima -
041600060412     c                             sono uguali a quelli dopo'
041700060412     c                   leave
041800060412     c                   end
041900060412     c* tipo incasso dopo
042000060412     c                   clear                   v03dtid
042100060412     C     '?'           SCAN      V03TICd                                44
042200060412     C     *IN44         IFEQ      '1'
042300060412     C                   MOVE      *BLANKS       tblkey
042400060412     C                   MOVEL     '1A'          tblCOD
042500060412     C                   CALL      'X§TABER'
042600060412     C                   PARM                    tblKUT
042700060412     C                   PARM                    tblCOD
042800060412     C                   PARM                    tblKEY
042900060412     C                   PARM                    §DES             30
043000060412     C                   MOVEL(p)  tblKEY        V03TICd
043100060412     C                   MOVEL     §des          V03dtid
043200060412     c                   else
043300060412     c                   exsr      srticd
043400060412     c   10              leave
043500060412     c                   end
043600060412     c* controllo la data
043700060412     c                   clear                   dtaiso
0438000604121    c                   if        v03dted = 0
043900060412     c                   if        v03abid <> 0
044000060412     C                   SETON                                        1390
044100060412     c                   leave
044200060412     c                   end
044300060412x1   c                   else
044400060412     c     *eur          test(d)                 v03dted                13
0445000604122    c                   if        *in13
044600060412     c     *dmy          test(d)                 v03dted                13
0447000604123    c                   if        not *in13
044800060412     c     *dmy          move      v03dted       dtaeur
044900060412     c                   move      dtaeur        v03dted
045000060412     c                   move      dtaeur        dtaiso
045100060412x3   c                   else
045200060412     c                   seton                                        90
045300060412     c                   leave
0454000604123    c                   end
045500060412     c                   else
045600060412     c                   move      v03dted       dtaeur
045700060412     c                   move      dtaeur        dtaiso
0458000604122    c                   end
0459000604121    c                   end
046000060412     c                   eval      nra = v03nrad
046100060412     c* salvo i riferimenti abi e cab per gestire f2 di forzatura
046200060412     c                   if        savabi <> v03abid or
046300060412     c                             savcab <> v03cabd
046400060412     c                   clear                   indkb
046500060412     c                   eval      savabi = v03abid
046600060412     c                   eval      savcab = v03cabd
046700060412     c                   end
046800060412     c* controlli nuovo tipo incasso
046900060412     c                   clear                   tbluni
047000060412     c                   eval      fgs = dutpou
047100060412     c                   callp     chkincassopo('201':fgs:v03ticd:v03tpd:tbluni
047200060412     c                             :*omit:nra
047300060412     c                             :v03abid:v03cabd:dtaiso:esito:err:*omit
047400060412     c                             :*omit)
047500060412     c                   eval      v03nrad =nra
047600060412     c*
0477000604121    c                   if        esito <>0
0478000604123    c                   do        10            xx                2 0
047900060412     c                   if        err(xx) = ' '
048000060412     c                   leave
048100060412     c                   end
0482000604124    c                   select
048300060412     c* tipo incasso errato
048400060412     c                   when      err(xx) = 'J'
048500060412     c                   seton                                        1090
048600060412     c* numero assegno errato
048700060412     c                   when      err(xx) = 'C' or
048800060412     c                             err(xx) = 'D' or
048900060412     c                             err(xx) = 'L'
049000060412     c                   seton                                        1190
049100060412     c* abi o cab obbligatori
049200060412     c                   when      err(xx) = 'E' or
049300060412     c                             err(xx) = 'K' or
049400060412     c                             err(xx) = 'M'
049500060412     c                   seton                                        1290
049600060412     c* se abi o cab errari abilito F2 di forzatura
049700060412     c                   when      (err(xx) = 'F' or
049800060412     c                             err(xx) = 'G') and indkb = ' '
049900060412     c                   seton                                        50
050000060412     c                   eval      v03msgw= 'ABI o CAB inesistenti F2 per for-
050100060412     c                             zarli'
050200060412     c* data assegno errato
050300060412     c                   when      err(xx) = 'H' or
050400060412     c                             err(xx) = 'I' or
050500060414     c                             err(xx) = 'W' or
050600060412     c                             err(xx) = 'N'
050700060412     c                   seton                                        1390
050800060414     C                   OTHER
050900060414     c                   seton                                        90
051000060414     c                   eval      v03msgw= msg(xx)
0511000604124    c                   endsl
0512000604123    c                   enddo
051300060412     c                   else
051400060412     c                   eval      ds1a = tbluni
0515000604121    c                   end
0516000604121    c                   enddo
051700060412     c*
051800060412     C                   ENDSR
051900060412      *---------------------------------------------------------*
052000060412      *  Controllo c/ass. o assegnato                           *
052100060412      *---------------------------------------------------------*
052200060412     C     CTRDIS        BEGSR
052300060310     c                   clear                   v03msg
052400060412     c                   clear                   fiar6ds
052500060412     c                   clear                   fiar9ds
052600060412     c                   clear                   datifat
052700060412     c                   clear                   datica
052800060228     c                   do
052900060228     C* controllo il mandato d'intrito
053000060309     c                   if        arbnmi <>0
053100060228     c                   seton                                        0690
053200060228     c                   leave
053300060228     c                   end
053400060228     C* Controllo se linea di arrivo abilitata
053500060228     C     ARBLNA        LOOKUP    LINT                                   70
053600060228     c                   if        not *in70
053700060915     C                   SETON                                        0490
053800060228     c                   leave
053900060228     c                   end
054000111110     C* Controllo che la spedizione non sia una mamma
054100111110     C     Kspe          CHAIN     FNLBL02L
054200111110     c                   if        %found(fnlbl02l)
054300111110     C                   SETON                                        1790
054400111110     c                   leave
054500111110     c                   end
054600060302     C* Controllo se esiste bolla mamma
054700060310     c                   clear                   fnarbmds
054800060228     C     KSPE          CHAIN     FNLBL01L
054900060228     c* per cambio di porto il contrassegno è sulla mamma
055000060228     C                   IF        %found(fnlbl01l) and lbllan = lbllap
055100060224     C                   Z-ADD     LBLAAP        KAAS
055200060224     C                   Z-ADD     LBLLPP        KLNP
055300060224     C                   Z-ADD     LBLNRP        KNRS
055400060224     C                   Z-ADD     LBLNSP        KNSP
055500060301     C     KSPE          CHAIN(n)  FNARB01L
055600060224     C* Memorizzo il legame esistente per i successivi accessi su FNARB
055700060228     C                   IF        %found(fnarb01l) and arbcca <> '2' and
055800060228     C                             arbcca <> '6'
055900060301     C                   movel     fnarbds       fnarbmds
056000060224     C                   END
056100060301     C                   movel     fnarbdssav    fnarbds
056200060224     C                   END
056300060309     C* controllo se c'è il contrassegno o la fattura e che siano incassati
056400060309     C                   Z-ADD     1             X
056500060309     C                   CLEAR                   DS3A
056600060309     C     ARBCBO        LOOKUP    C3A(X)                                 95
056700060309     C     *IN95         IFEQ      '1'
056800060309     C                   MOVEL     D3A(X)        DS3A
056900060309     C                   END
057000060309     c                   eval      tbluni = ds3a
057100060418     c* se scelta cambio tipo incasso e il tipo documento non è solo
057200060418     c* contrassegno errore
057300060418     c                   movel     §3atb1        porto             1
057400060418     c                   if        v03sce = '1' and porto <> 'F'
057500060418     c                   seton                                        1590
057600060309     c                   leave
057700060309     c                   end
057800060418     c* se non sono previsti incassi con fattura immediata e
057900060418     c* non ho contrassegni
058000060418     c                   if        §3ati1 <> 'A' and §3ati2 <> 'A' and
058100060418     c                             §3afca = 0
058200060418     c                   seton                                        1690
058300060418     c                   leave
058400060418     c                   end
058500060309     c* controllo bolla
058600060307     c                   callp     CHKBOLLAincassata(
058700060307     c                             arbaas:arblnp:arbnrs:arbnsp
058800060310     c                             :arbcbo:tbluni:v03tpd:esito:err:msg
058900060310     c                             :datifat:datica)
059000060310     c* se errore oppure contrassegno sulla mamma
059100060321     c                   if        esito <>0 and marbaas <> 0
059200060309     c                   clear                   esito
059300060309     c                   callp     CHKBOLLAincassata(
059400060309     c                             marbaas:marblnp:marbnrs:marbnsp
059500060310     c                             :marbcbo:*omit:v03tpd:esito:err:msg
059600060310     c                             :*omit:datica)
059700060309     c                   end
059800060310     C*
0599000603101    c                   if        esito = 0
060000060310     c                   movel     datifat       fiar6ds
060100060310     c                   movel     datica        fiar9ds
060200080521     c* ASSEGNATO no segue fattura
0603000805222    c                   if        ar6aas <> 0
060400060309     c* senza contrassegno
0605000603103    c                   if        §3afca = 0
060600080522     c* segue fattura
060700080522     c                   if        ar6dft = 0
060800080522     c                   seton                                        1690
060900060228     c                   leave
061000060227     C                   END
061100080522     c* assegnato non incassato
061200080522     c                   if        arbica = ' ' or arbica = 'R'
061300080522     c                   seton                                        0790
061400080522     c                   leave
061500080522     C                   END
061600060309     c* assegnato già contabilizzato
061700060309     c                   if        arbica = 'Y'
061800060309     c                   seton                                        0690
061900060309     c                   leave
062000060309     C                   END
062100060309     c* ASSEGNATO + C/ASSEGNO
062200060310x3   c                   else
062300060309     c* assegnato e contrassegno non incassati
062400090708     c                   if        arbica = ' ' or arbica = 'R' or
062500090708     c                             arbicc = ' ' or arbicc = 'R'
062600060322     c                   seton                                        079008
062700060309     c                   leave
062800060309     C                   END
062900060309     c* assegnato e contrassegno già contabilizzati
063000090708     c                   if        arbica = 'Y' or
063100060309     c                             arbicc = 'Y'
063200060322     c                   seton                                        0690
063300060309     c                   leave
063400060309     C                   END
0635000603103    C                   END
063600060310x2   c                   else
063700060309     c* SOLO CONTRASSEGNO
0638000603103    C     §3AFCA        IFGT      0                                            --- 01 ---
063900060309     c* c/assegno non incassato
064000060309     c                   if        (arbicc = ' ' or arbicc = 'R')
064100060321     c                   seton                                        0890
064200060309     c                   leave
064300060309     C                   END
064400060309     c* c/assegno già contabilizzato
064500060309     c                   if        arbicc = 'Y'
064600060322     c                   seton                                        0690
064700060309     c                   leave
064800060309     C                   END
0649000603103    C                   END
0650000603102    C                   END
065100060310x1   c                   else
065200060310     c                   do        10            xx
065300060310     c                   if        err(xx) = ' '
065400060310     c                   leave
065500060310     c                   end
065600060310     c                   movel     msg(xx)       v03msg
065700060310     c                   seton                                        90
065800060310     c                   leave
0659000603102    C                   ENDdo
0660000603101    c                   end
066100111025     c* aggancio l'assegno
066200111025     c                   setoff                                       52
066300111109     c     kass          chain(n)  cnass01l
066400111109     c                   if        not %found(cnass01l) and marbaas <> 0
066500111109     C                   Z-ADD     marbaas       KAAS
066600111109     C                   Z-ADD     marblnp       KLNP
066700111109     C                   Z-ADD     marbNRs       KNRS
066800111109     C                   Z-ADD     marbNSP       KNSP
066900111109     c     kass          chain(n)  cnass01l
067000111109     c                   end
067100111109     c                   if        %found(cnass01l)
067200120125     c* se incasso con multiassegno è possibile solo annullare l'incasso
067300120125     c* non si può variare il tipo
067400120125     c* se nel numero assegno sono presenti dei blank significa che la
067500120125     c* spedizione è incassata senza la gestione dei multi assegni
067600120125     c* ma con l'inserimento solo degli ultimi 4 numeri
067700120125     c     ' '           scan      assnra                                 52
067800120127     c                   if        not *in52 and assabi = 0
067900111025     c                   seton                                        52
068000111025     c                   if        v03sce = '1'
068100111025     c                   seton                                        2090
068200111025     c                   leave
068300111025     c                   else
068400111025     c* salvo i riferimenti sped. per gestire f8 di forzatura
068500111025     c                   if        savaas <> v03aas or
068600111025     c                             savlnp <> v03lnp or
068700111025     c                             savnrs <> v03nrs or
068800111025     c                             savnsp <> v03nsp
068900111025     c                   clear                   indkh
069000111025     c                   eval      savaas = v03aas
069100111025     c                   eval      savlnp = v03lnp
069200111025     c                   eval      savnrs = v03nrs
069300111025     c                   eval      savnsp = v03nsp
069400111025     c                   end
069500111111     c                   if        indkh = ' '
069600111111     c                   seton                                        51
069700111111     c                   end
069800111111     c                   end
069900111111     c                   end
070000111025     c* imposto i campi tipo incasso prima
070100111025     c                   if        asstpd = 'C'
070200111025     c                   eval      v03nrap = assnra
070300111025     c                   eval      v03abip = assabi
070400111025     c                   eval      v03cabp = asscab
070500111025     c                   if        assdte <> 0
070600111025     c                   movel     assdte        dtaiso
070700111025     c                   movel     dtaiso        dtaeur
070800111025     c                   movel     dtaeur        v03dtep
070900111025     c                   else
071000111025     c                   eval      v03dtep = 0
071100111025     c                   end
071200111025     c                   end
071300060412     c                   end
071400060412     c* tipo incasso prima
071500060412     c                   eval      v03ticp = ar9tic
071600060412     c                   exsr      srticp
071700060412     c*
071800060412     C                   ENDdo
071900060224     C*
072000060224     C                   ENDSR
072100060228      *---------------------------------------------------------*
072200060228      *  Controllo prepagato                                    *
072300060228      *---------------------------------------------------------*
072400060228     C     CTRPRE        BEGSR
072500060308     c                   setoff                                       50
072600060308     c                   clear                   v03msg
072700060228     c                   do
072800060228     C* controllo il mandato d'intrito
072900060228     c                   if        incnmi <>0
073000060228     c                   seton                                        0690
073100060228     c                   leave
073200060228     c                   end
073300060228     C* Controllo se linea di arrivo abilitata
073400060228     C     inclnp        LOOKUP    LINT                                   70
073500060228     c                   if        not *in70
073600060228     C                   SETON                                        0490
073700060228     c                   leave
073800060228     c                   end
073900060301     c* controllo spedizione
074000060307     c                   callp     CHKBOLLAincassata(
074100060307     c                             incaas:inclnp:incnrs:incnsp
074200060310     c                             :*omit:*omit:v03tpd:esito:err:msg
074300060307     c                             :*omit:*omit)
074400060321     c                   if        esito <>0
074500060310     c                   do        10            xx
074600060310     c                   if        err(xx) = ' '
074700060310     c                   leave
074800060310     c                   end
074900060310     c                   movel     msg(xx)       v03msg
075000060310     c                   seton                                        90
075100060310     c                   leave
0752000603102    C                   ENDdo
075300060301     c                   end
075400060301     c* pre-pagato non incassato
075500060301     c                   call      'YCOW0121R'
075600060307     C                   PARM                    incAas
075700060307     C                   PARM                    incLnp
075800060307     C                   PARM                    incNrs
075900060307     C                   PARM                    incNsp
076000060307     C                   PARM                    incFiv
076100060307     C                   PARM                    incDft
076200060307     C                   PARM                    incNft
076300060307     C                   PARM                    prmEsito
076400060307     c                   if        prmesito = 0
076500060301     C                   SETON                                        0990
076600060228     c                   leave
076700060228     c                   end
076800060307     c* imposto i campi tipo incasso prima
076900060307     c                   eval      v03ticp = inctii
077000060307     c                   eval      v03nrap = incnra
077100060307     c                   eval      v03abip = incabi
077200060307     c                   eval      v03cabp = inccab
077300060308     c                   if        incdte <> 0
077400060307     c                   movel     incdte        dtaiso
077500060307     c                   movel     dtaiso        dtaeur
077600060307     c                   movel     dtaeur        v03dtep
077700060308     c                   else
077800060308     c                   eval      v03dtep = 0
077900060308     c                   end
078000060412     c* tipo incasso prima
078100060412     c                   exsr      srticp
078200060228     C                   ENDdo
078300060228     C*
078400060228     C                   ENDSR
078500030730     C*-----------------------------------------------------*
078600060412     C* srticp  - tipo incasso prima                        *
078700030730     C*-----------------------------------------------------*
078800060412     c     srticp        begsr
078900060412     C*
079000060412     C*  tipo incasso prima
079100060412     c                   clear                   v03dtip
079200060412     C                   MOVEL     '1A'          tblCOD
079300060412     C                   MOVEl(p)  v03ticp       tblkey
079400060412     c     ktab1         chain     tabel00f
079500060412     c                   if        %found(tabel00f)
079600060412     c                   movel     tbluni        ds1a
079700060412     C                   MOVEL     §1ades        V03dtip
079800060412     c                   end
079900060412     C*
080000060412     C                   ENDSR
080100060412     C*-----------------------------------------------------*
080200060412     C* srticp  - tipo incasso dopo                         *
080300060412     C*-----------------------------------------------------*
080400060412     c     srticd        begsr
080500060412     C*  tipo incasso prima
080600060412     c                   clear                   v03dtid
080700060412     C                   MOVEL     '1A'          tblCOD
080800060412     C                   MOVEl(p)  v03ticd       tblkey
080900060412     c     ktab1         chain     tabel00f
081000060412     c                   if        %found(tabel00f)
081100060412     c                   movel     tbluni        ds1a
081200060412     C                   MOVEL     §1ades        V03dtid
081300060412     c                   else
081400060412     C                   SETON                                        1090
081500060412     c                   end
081600060412     C*
081700060412     C                   ENDSR
081800060412     C*-----------------------------------------------------*
081900060412     C* sraggio - aggiorno FNARB, anomalie ed eventi        *
082000060412     C*-----------------------------------------------------*
082100060412     c     sraggio       begsr
082200111109     C                   Z-ADD     v03aas        KAAS
082300111109     C                   Z-ADD     v03lnp        KLNP
082400111109     C                   Z-ADD     v03NRs        KNRS
082500111109     C                   Z-ADD     v03NSP        KNSP
082600060413     c* ERRATO INCASSO
082700060301     C* tipo c/assegno o assegnato aggiorno FNARB come ritorno all'incasso
082800060301     c* e cancello CNASS
082900111109     c                   if        v03tpd <>'P' and v03sce = '2'
083000111109     c* CNASS
083100111109     c     kass          setll     cnass01l                               55
083200111109     c                   if        not *in55 and marbaas <>0
083300111109     C                   Z-ADD     marbaas       KAAS
083400111109     C                   Z-ADD     marblnp       KLNP
083500111109     C                   Z-ADD     marbNRs       KNRS
083600111109     C                   Z-ADD     marbNSP       KNSP
083700111109     c     kass          setll     cnass01l
083800111109     c                   end
083900111025     c                   do        *hival
084000111109     c     kass          reade     cnass01l
084100111025     c                   if        %eof(cnass01l)
084200111025     c                   leave
084300111025     c                   end
084400111025     C                   delete    cnass000
084500111025     c* più assegni o più spedizioni
084600120125     c     ' '           scan      assnra                                 52
084700120127     c                   if        not *in52 and assabi = 0
084800111025     c                   exsr      srasssped
084900111025     c                   end
085000111025     c                   enddo
085100111025     c* FNARB
085200111109     C                   Z-ADD     v03aas        KAAS
085300111109     C                   Z-ADD     v03lnp        KLNP
085400111109     C                   Z-ADD     v03NRs        KNRS
085500111109     C                   Z-ADD     v03NSP        KNSP
085600060301     C     Kspe          CHAIN     FNARB01L
0857000604122    c                   if        %found(fnarb01l)
085800080521     c                   if        arbica <> ' ' and ar6dft <> 0 and
085900120125     c                             arbica <> 'R' and
086000080521     c                             (§3ati1 = 'A' or §3ati2 = 'A')
086100060301     c                   eval      arbica = 'R'
086200111110     c                   clear                   arbmif
086300060301     c                   end
0864001201253    c                   if        arbicc <> ' ' and  arbicc <> 'R'
086500120125     c                             and §3afca <> 0
086600060301     c                   eval      arbicc = 'R'
0867000604123    c                   end
086800030730     C                   UPDATE    FNARB000
0869000604122    c                   end
087000111110     c* FIARI (x asssegnati)
087100111109     C                   Z-ADD     v03aas        KAAS
087200111109     C                   Z-ADD     v03lnp        KLNP
087300111109     C                   Z-ADD     v03NRs        KNRS
087400111109     C                   Z-ADD     v03NSP        KNSP
087500111109     c     kspe          setll     fiari01l                               55
087600060414     c                   do        *hival
087700111109     c     kspe          reade     fiari01l
087800060414     c                   if        %eof(fiari01l)
087900060414     c                   leave
088000060414     c                   end
088100060414     c                   if        ARIIMP <> 0
088200060414     c                   if        ARIIPP = 0
088300060414     C                   delete    fiari000
088400060414     c                   else
088500060414     c                   clear                   ariimp
088600060414     C                   update    fiari000
088700060414     c                   end
088800060414     c                   end
088900060414     c                   enddo
0890000604121    c                   end
089100060414     c* a prescindere dalla scelta aggiorno il tipo incasso del contrassegno
089200111110     c                   eval      cassm = *off
0893000604141    c                   if        v03tpd = 'C' or v03tpd = 'T'
089400060414     C                   Z-ADD     v03aas        KAAS
089500060414     C                   Z-ADD     v03lnp        KLNP
089600060414     C                   Z-ADD     v03NRs        KNRS
089700060414     C                   Z-ADD     v03NSP        KNSP
089800060414     c     kspe          chain     fiar901l
0899000604142    c                   if        not %found(fiar901l) and marbaas <> 0
090000060414     C                   Z-ADD     marbaas       KAAS
090100060414     C                   Z-ADD     marblnp       KLNP
090200060414     C                   Z-ADD     marbnrs       KNRS
090300060414     C                   Z-ADD     marbnsp       KNSP
090400111110     c     kspe          chain     fiar901l
0905001111102    c                   if        %found(fiar901l)
090600111110     c                   eval      cassm = *on
0907001111102    c                   end
0908001111102    c                   end
0909000604142    c                   if        %found(fiar901l)
091000060414     c                   if        v03sce = '1'
091100060414     c                   eval      ar9tic = v03ticd
091200060414     c                   else
091301120322     c                   exsr      srticbol
091302120322     c                   eval      ar9tic = savtici
091400060414     c                   end
091500060414     c                   update    fiar9000
0916000604142    c                   end
091700111110     c* metto il ritorno all'incasso anche sulla mamma se il c/ass.
091800111110     c* è su di lei
0919001111102    c                   if        marbaas <> 0  and cassm = *on
092000111110     c     kspe          chain     fnarb01l
0921001201253    c                   if        arbicc <> ' ' and arbicc <> 'R'
092200111110     c                   eval      arbicc = 'R'
0923001111103    c                   end
092400111110     C                   UPDATE    FNARB000
0925001111101    c                   end
0926001111101    c                   end
092700060413     c* CAMBIO TIPO INCASSO
092800060413     c                   if        v03sce = '1'
092900060301     C* tipo pre-pagato aggiorno FIINC con i nuovi dati d'incasso
0930000604121    c                   if        v03tpd = 'P'
093100060307     C     Kinc          CHAIN     Fiinc01L
0932000604122    c                   if        %found(fiinc01l)
093300060307     c                   eval      inctii = v03ticd
093400060307     c                   eval      INCNRA = nra
0935000604123    c                   if        v03dted <> 0
093600060307     c                   move      dtaiso        INCDTE
093700060412x3   c                   else
093800060308     c                   eval      incdte = 0
0939000604123    c                   end
094000060307     c                   eval      INCABI = v03abid
094100060307     c                   eval      INCCAB = v03cabd
094200060301     c                   eval      INCTPA = §1AFCA
094300060413     c                   if        §1afmb = 'B'
094400060413     c                   eval      inctpi = ' '
094500060413     c                   else
094600060412     c                   eval      INCTPI = §1AFMB
0947000604132    c                   end
094800060301     C                   UPDATE    Fiinc000
0949000604122    c                   end
0950000604121    c                   end
095100060412     c* sia per pre-pagati che per contrassegni aggiorno il cnass
0952000604121    c                   if        v03tpd = 'P' or v03tpd = 'C'
095300111109     C                   Z-ADD     v03aas        KAAS
095400111109     C                   Z-ADD     v03lnp        KLNP
095500111109     C                   Z-ADD     v03NRs        KNRS
095600111109     C                   Z-ADD     v03NSP        KNSP
095700111109     c     kass          chain     cnass01l
095800111109     c                   if        not %found(cnass01l) and marbaas <>0
095900111109     C                   Z-ADD     marbaas       KAAS
096000111109     C                   Z-ADD     marblnp       KLNP
096100111109     C                   Z-ADD     marbnrs       KNRS
096200111109     C                   Z-ADD     marbnsp       KNSP
096300111109     c     kass          chain     cnass01l
096400111109     c                   end
096500060308     c* se il nuovo tipo incasso non prevede assegno deleto nel caso ci sia
0966000604122    c                   if        §1asas = ' '
096700060414     c                   if        %found(cnass01l) and asstpd = v03tpd
096800060308     c                   delete    cnass000
096900060308     c                   end
097000060412x2   c                   else
097100060412     c                   eval      assnra = nra
0972000604123    c                   if        v03dted <> 0
097300060412     c                   move      dtaiso        assDTE
097400060412x3   c                   else
097500060412     c                   eval      assdte = 0
0976000604123    c                   end
097700060412     c                   eval      assABI = v03abid
097800060412     c                   eval      assCAB = v03cabd
097900060418     c                   eval      assTPA = §1afca
098000060414     c                   if        §1afmb = 'B'
098100060414     c                   eval      asstpi = ' '
098200060414     c                   else
098300060412     c                   eval      asstpi = §1AFMB
098400060414     c                   end
098500060412     C* per contrassegno lo deve trovare
0986000604143    c                   if        %found(cnass01l) and asstpd = v03tpd
098700060301     C                   update    cnass000
098800060412x3   c                   else
098900060412     c                   eval      assFle = dutpou
099000060412     c                   eval      assTpd = v03Tpd
099100060412     c                   eval      assAas = v03Aas
099200060412     c                   eval      assLnp = v03Lnp
099300060412     c                   eval      assNrs = v03Nrs
099400060412     c                   eval      assNsp = v03Nsp
099500060412     c                   if        v03tpd = 'P'
099600060301     c                   eval      assCas = incImp
099700060301     c                   eval      assVca = incDiv
099800060412     c                   else
099900060412     c                   eval      assCas = ar9cas
100000060412     c                   eval      assVca = ar9vca
100100060412     c                   end
100200060412     c                   eval      assTpa = §1afca
100300060414     c                   if        §1afmb = 'B'
100400060414     c                   eval      inctpi = ' '
100500060414     c                   else
100600060412     c                   eval      asstpi = §1AFMB
1007000604143    c                   end
100800060412     c                   eval      assfve = ' '
100900060324     c                   eval      assftr = ' '
101000060301     C                   write     cnass000
1011000604123    c                   end
1012000604122    c                   end
1013000604121    c                   end
1014000604131    c                   end
101500030730     c                   commit
101600030730     c*
101700030730     c                   endsr
101701120322     C*-----------------------------------------------------*
101702120322     c* reperisce il tipo incasso bollettato altrimenti pulisce ...
101704120322     C*-----------------------------------------------------*
101705120322     C     srticbol      BEGSR
101707120322     c                   clear                   savtici           2
101708120322     c                   if        ar9aas > 2009
101709120322     c     kar5          chain     fiar501l
101710120322     c                   if        %found(fiar501l)
101711120322     c                   movel     ar5uni        dar5gen
101712120322     c                   movel     §AR5TICI      savtici
101713120322     c                   end
101714120322     c                   end
101715120322     c                   endsr
101800111025     C*-----------------------------------------------------*
101900111025     C* aggiorno le spedizioni come ritorno all'incasso c/ass
102000111104     C* o cancello i più assegni per la stessa spedizione
102100111025     C*-----------------------------------------------------*
102200111025     C     srasssped     BEGSR
102300111025     c* più assegni per la stessa spedizione
102400120220     c                   clear                   fncssds
102500111109     C                   Z-ADD     v03aas        KAAS
102600111109     C                   Z-ADD     v03lnp        KLNP
102700111109     C                   Z-ADD     v03NRs        KNRS
102800111109     C                   Z-ADD     v03NSP        KNSP
102900120220     c     kspe          chain     fncss01l
103000120220     c                   if        %found(fncss01l)
103100120220     c     csskey        setll     fncss03l
103200111025     c                   do        *hival
103300120220     c     csskey        reade     fncss03l
103400120220     c                   if        %eof(fncss03l)
103500111025     c                   leave
103600111025     c                   end
103700120125     c* cancello gli assegni
103800120125     c     csskey        setll     tncsm03l
103900111025     c                   do        *hival
104000120125     c     csskey        reade     tncsm03l
104100120125     c                   if        %eof(tncsm03l)
104200111025     c                   leave
104300111025     c                   end
104400120125     c                   delete    tncsm000
104500120125     c                   enddo
104600120125     c* CNASS
104700120125     C     Kasscss       CHAIN     cnass01L
1048001201252    c                   if        not %found(cnass01l) and cssnsp <> cssnso
104900120125     C     Kasscsso      CHAIN     cnass01L
1050001201252    c                   end
1051001201252    c                   if        %found(cnass01l)
105200120125     C                   delete    cnass000
1053001201252    c                   end
105400120125     c* FNARB spedizione incassata
105500120125     C     Kcss          CHAIN     FNARB01L
1056001110252    c                   if        %found(fnarb01l)
1057001201253    c                   if        arbicc <> ' '  and arbicc <> 'R'
105800111025     c                   eval      arbicc = 'R'
1059001110253    c                   end
106000111025     C                   UPDATE    FNARB000
1061001110252    c                   end
106200120125     c* FNARB spedizione originale
106300120125     c                   if        cssaao <> 0 and cssaao <> cssaas
106400120125     C     Kcsso         CHAIN     FNARB01L
1065001111102    c                   if        %found(fnarb01l)
1066001201253    c                   if        arbicc <> ' ' and arbicc <> 'R'
106700111110     c                   eval      arbicc = 'R'
1068001111103    c                   end
106900111110     C                   UPDATE    FNARB000
1070001111102    c                   end
1071001201252    c                   end
107200111104     c* FIAR9
107300120125     c     kcss          chain     fiar901l
1074001201252    c                   if        not %found(fiar901l) and cssnsp <> cssnso
107500120125     c     kcsso         chain     fiar901l
107600120125     c                   end
1077001201252    c                   if        %found(fiar901l)
107801120322     c                   exsr      srticbol
107802120322     c                   eval      ar9tic = savtici
107900111104     c                   update    fiar9000
108000111104     c                   end
108100120125     C*
108200120220     c                   delete    fncss000
108300120125     c                   enddo
1084001201252    c                   end
108500111025     c                   endsr
108600111025     C*-----------------------------------------------------*
108700120125     C* interrogo le spedizioni multiassegni
108800111025     C*-----------------------------------------------------*
108900111025     C     intasssped    BEGSR
109000120125     c* più assegni per la stessa spedizione
109100120125     c     ' '           scan      assnra                                 52
109200120127     c                   if        not *in52  and assabi = 0
109300120210     c                   clear                   tnsc30ds
109400120210     c                   eval      sc30cmtI = 'N'
109500120210     c                   eval      sc30TPCI = 'V'
109600120125     c                   if        marbaas <> 0
109700120210     c                   eval      sc30AASI = marbaas
109800120210     c                   eval      sc30LNPI = marblnp
109900120210     c                   eval      sc30NRSI = marbNRs
110000120210     c                   eval      sc30NSPI = marbNSP
110100120125     c                   else
110200120210     c                   eval      sc30AASI = v03aas
110300120210     c                   eval      sc30LNPI = v03lnp
110400120210     c                   eval      sc30NRSI = v03nrs
110500120210     c                   eval      sc30NSPI = v03nsp
110600120125     c                   end
110700120210     c                   eval      sc30AAnI = v03aas
110800120210     c                   eval      sc30LPnI = v03lnp
110900120210     c                   eval      sc30NRnI = v03nrs
111000120210     c                   eval      sc30NSnI = v03nsp
111100120210     c                   eval      kpjbu=tnsc30ds
111200120210     c                   call      'TNSC30R'
111300120125     C                   PARM                    kpjba
111400120210     c                   movel     kpjbu         tnsc30ds
111500111025     c                   end
111600111025     c                   endsr
111700940322     C*-----------------------------------------------------*
111800060915     C*    carico £6 del po incasso
111900941107     C*-----------------------------------------------------*
112000060915     C     sr£6          BEGSR
112100060915     C* Aggancio la £6 e recupero le linee a loro associate
112200060915     c                   clear                   trul06ds
112300060915     c                   eval      d06cod = '£6'
112400060915     c                   movel     dutpouc       d06key
112500060915     c                   movel     trul06ds      kpjbu
112600060915     c                   call      'TRUL06R'
112700060915     c                   parm                    kpjba
112800060915     c                   movel     kpjbu         trul06ds
112900060915     c*
113000060915     c                   endsr
113100060915     C*-----------------------------------------------------*
113200060915     C*    Operazioni iniziali
113300060915     C*-----------------------------------------------------*
113400060915     C     *INZSR        BEGSR
113500060915     C*
113600940322     C     *ENTRY        PLIST
113700940322     C                   PARM                    KPJBA
113800020905     c*
113900020905     c     *dtaara       define    §azute        azuteds
114000020905     c     *dtaara       define    §datiute      ddatiute
114100020905     C                   in(E)     *dtaara
114200020905     C                   IF        %Error  or  RSUT = *blanks
114300020905     C                   call      'TIBS34R'
114400020905     C                   parm                    Tibs34Ds
114500020905     C                   in        *dtaara
114600020905     c                   ENDIF
114700060915     c* carico £1
114800060915     c                   clear                   trul06ds
114900060915     c                   eval      d06cod = '£1'
115000060915     c                   movel     dutpou        d06key
115100060915     c                   movel     trul06ds      kpjbu
115200060915     c                   call      'TRUL06R'
115300060915     c                   parm                    kpjba
115400060915     c                   movel     kpjbu         trul06ds
115500060915     c                   movea     lint          £1
115600060915     c* carico £6
115700060915     c                   move      dutpou        dutpouc
115800060915     c                   exsr      sr£6
115900060915     c* se primo livello abilito f18 cambio po incasso
116000060915     C     Dutlpo        IFEQ      '1'
116100060915     C                   SETON                                        44
116200060915     C                   END
116300940322      *---------------------------------------------------------------*
116400940322      * CHIAVI                                                        *
116500940322      *---------------------------------------------------------------*
116501120322     C     Kar5          KLIST
116502120322     C                   KFLD                    ar9aas
116503120322     C                   KFLD                    ar9lnp
116504120322     C                   KFLD                    ar9nrs
116505120322     C                   KFLD                    ar9nsp
116506120322     C                   KFLD                    ar5trd
116507120322     c                   eval      ar5trd = 'GEN'
116600060307     C     KTAB          KLIST
116700060307     C                   KFLD                    tblKUT
116800060307     C                   KFLD                    tblCOD
116900060307     C     KTAB1         KLIST
117000060307     C                   KFLD                    tblKUT
117100060307     C                   KFLD                    tblCOD
117200060307     C                   KFLD                    tblkey
117300060307     C                   Z-ADD     1             tblKUT
117400111109     C     KASS          KLIST
117500060307     C                   KFLD                    dutpou
117600111109     C                   KFLD                    klnp
117700111109     C                   KFLD                    kaas
117800111109     C                   KFLD                    knrs
117900111109     C                   KFLD                    knsp
118000060301     C     Kspe          KLIST
118100060301     C                   KFLD                    KAAS
118200060301     C                   KFLD                    KLNP
118300060301     C                   KFLD                    KNRS
118400060301     C                   KFLD                    KNSP
118500120125     C     Kcsm          KLIST
118600120125     C                   KFLD                    cSmTPI
118700120125     C                   KFLD                    cSmTPA
118800120125     C                   KFLD                    cSmNRA
118900120125     C                   KFLD                    cSmABI
119000120125     C                   KFLD                    cSmCAB
119100120125     C                   KFLD                    cSmDTE
119200120125     C     Kcss          KLIST
119300111025     C                   KFLD                    cssAAS
119400111025     C                   KFLD                    cssLNP
119500111025     C                   KFLD                    cssNRS
119600111025     C                   KFLD                    cssNSP
119700120125     C     Kcsso         KLIST
119800120125     C                   KFLD                    cssAAO
119900120125     C                   KFLD                    cssLNO
120000120125     C                   KFLD                    cssNRO
120100120125     C                   KFLD                    cssNSO
120200120125     C     Kasscss       KLIST
120300120125     C                   KFLD                    dutpou
120400120125     C                   KFLD                    cssLNP
120500120125     C                   KFLD                    cssAAS
120600120125     C                   KFLD                    cssNRS
120700120125     C                   KFLD                    cssNSP
120800120125     C     Kasscsso      KLIST
120900120125     C                   KFLD                    dutpou
121000120125     C                   KFLD                    cssLNO
121100120125     C                   KFLD                    cssAAO
121200120125     C                   KFLD                    cssNRO
121300120125     C                   KFLD                    cssNSO
121400060301     C     Kinc          KLIST
121500060301     C                   KFLD                    v03AAS
121600060301     C                   KFLD                    v03LNP
121700060301     C                   KFLD                    v03NRS
121800060301     C                   KFLD                    v03NSP
121900060307     C                   KFLD                    v03tpd
122000030730     C*
122100060301     C                   MOVEL     'CNC0W5R'     VIDPGM
122200060301     C                   MOVE      *year         V03AAS
122300060307     c*
122400060307     C                   Z-ADD     0             X                 3 0
122500060307     C                   MOVEL     '3A'          tblCOD
122600060307     C     KTAB          setll     TABEL00F
122700060307     c                   do        *hival
122800060307     C     KTAB          reade     TABEL00F
122900060307     C                   if        %eof(tabel00f)
123000060307     c                   leave
123100060307     c                   end
123200060307     C     X             iflt      100
123300060307     C     TBLFLG        andeq     ' '
123400060307     C                   ADD       1             X
123500060307     C                   MOVEL     TBLKEY        C3A(X)
123600060307     C                   MOVEL     TBLUNI        D3A(X)
123700060307     C                   END
123800060307     C                   ENDdo
123900060412     c*
124000060412     c                   eval      v03sce = '1'
124100940322     C*
124200940322     C                   ENDSR
