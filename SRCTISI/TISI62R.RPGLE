000100050907      *------------------------------------------------------------------------*
000200050907      * STAMPA DELIVERY TIME - ESEGUE                                          *
000300050907      *------------------------------------------------------------------------*
000400090610     h decedit('0,') datedit(*dmy.) option(*nodebugio)
000500050907      *------------------------------------------------------------------------*
000600050907      * DATA BASE
000700050907      *------------------------------------------------------------------------*
000800050908     fWADLY01L  if   e           k disk    usropn
000900050908     fWADLY02L  if   e           k disk    usropn
001000050908     f                                     rename(WADLY000:WADLY2)
001100050908     fWADLY03L  if   e           k disk    usropn
001200050908     f                                     rename(WADLY000:WADLY3)
001300050908     fWADLY04L  if   e           k disk    usropn
001400050908     f                                     rename(WADLY000:WADLY4)
001500090609     fWADLY06L  if   e           k disk    usropn
001600090609     f                                     rename(WADLY000:WADLY6)
001700050908     fTABEL00F  if   e           k disk
001800050908     fAZORG01L  if   e           k disk
001900050907      *
002000090612     fLET017    o    f  116        PRINTER usropn infds(prtdly)
002100050908     f                                     oflind(*INOF)
002200050908     F                                     formlen(66)
002300090612     F                                     formofl(63)
002400050907      *------------------------------------------------------------------------*
002500050907      * SCHIERE
002600050907      *------------------------------------------------------------------------*
002700050907     D $CMD            S            100    DIM(1) CTDATA PERRCD(1)
002800091204     D $RIG            S             95    DIM(51) CTDATA PERRCD(1)
002900090605
003000050907      *-------------------
003100050907      * REGIONI
003200050907      *-------------------
003300980217     D RAP             S              2    DIM(50)
003400980217     D RDES            S             20    DIM(50)
003500050907      *-------------------
003600050907      * PROVINCIE
003700050907      *-------------------
003800980217     D PRO             S              2    DIM(300)
003900980217     D PDES            S             20    DIM(300)
004000050907      *-------------------
004100050907      * NOTE
004200050907      *-------------------
004300980218     D NOTE            S             75    DIM(5)
004400050907      *-------------------
004500050907      * TEMPI PROVINCIE
004600050907      *-------------------
004700050907     D REG             S              2    DIM(20)  inz
004800050907      * MULTIPLE -20 REGIONI-
004900050907      *-------------------
005000050907      * TEMPI IN STAMPA
005100050907      *-------------------
005200110117     D SDES            S             24    DIM(15)  inz
005300110117     d Scor            s             60    dim(15)  inz
005400110117     D SINP            S              2  0 DIM(15)  inz
005500110117     D STCX            S              3  0 DIM(15)  inz
005600110117     D STCN            S              3  0 DIM(15)  inz
005700110117     D STPX            S              3  0 DIM(15)  inz
005800110117     D STPN            S              3  0 DIM(15)  inz
005900110117     D STIX            S              3  0 DIM(15)  inz
006000110117     D STIN            S              3  0 DIM(15)  inz
006100110117     D STDX            S              3  0 DIM(15)  inz
006200110117     D STDN            S              3  0 DIM(15)  inz
006300110117     D SELE            S              1    DIM(15)  inz(*off)
006400050907      *-------------------
006500050907      * DI LAVORO
006600050907      *-------------------
006700110117     D SS2             S              2    DIM(15)  inz
006800110117     d ss5             s                   dim(15)  inz  like(DLYcor)
006900050907      *-------------------
007000050907      * LOCALITA' DA STAMPARE
007100050907      *-------------------
007200050909     D LOC             S             89    DIM(30)  inz
007300050907      *------------------------------------------------------------------------*
007400050907      * INPUT
007500050907      *------------------------------------------------------------------------*
007600050907      *-------------------
007700050907      * ARCHITETTURA
007800050907      *-------------------
007900050907     D KPJBA         E DS
008000050907      *-------------------
008100050907      * PARAMETRI DI LANCIO
008200050907      *-------------------
008300050907     D PARAM           DS
008400050907     D  PARLNP                 1      3
008500050907     D  PARTEM                 4      4
008600050907     D  PARTSP                 5      5
008700050907     D  PARISO                 6      6
008800050907     D  PARDIS                 7      7
008900050907     D  PARSSA                 8      8
009000050907     D  PARND1                 9     83
009100050907     D  PARND2                84    158
009200050907     D  PARND3               159    233
009300090608     d  parlin               234    234
009400090608     d  parold               235    235
009500050907      *------------------
009600050907      * DS MULTIPLA TEMPI PROVINCIE -20 REGIONI-
009700050907      *------------------
009800050907      * (3:2)+1=2,5 ; (2X12)=24
009900980216     D DREG            DS                  OCCURS(20) INZ
010000110117     d  Prv                                like(DLYprv) dim(15) inz
010100110117     d  Tcx                                like(DLYtrc) dim(15) inz
010200110117     d  Tcn                                like(DLYtrc) dim(15) inz
010300110117     d  Tpx                                like(DLYtrc) dim(15) inz
010400110117     d  Tpn                                like(DLYtrc) dim(15) inz
010500110117     d  Tix                                like(DLYtrc) dim(15) inz
010600110117     d  Tin                                like(DLYtrc) dim(15) inz
010700110117     d  Tdx                                like(DLYtrc) dim(15) inz
010800110117     d  Tdn                                like(DLYtrc) dim(15) inz
010900110117     d  Cor                                like(DLYcor) dim(15) inz
011000110117     d  $Ele                          1                 dim(15) inz(*off)
011100050907      *-------------------
011200050907      * DS REPERIMENTO DATI UTENTE
011300050907      *-------------------
011400020919     D TIBS34DS      E DS
011500020919     D DDATIUTE      E DS
011600020919     D AZUTEDS       E DS                  extname(AZUTE00F)
011700050907      *-------------------
011800050907      * DS CODICI TASSAZIONE
011900050907      *-------------------
012000050907     D DSCT          E DS
012100050907      *-------------------
012200050907      * DS PROVINCIE
012300050907      *-------------------
012400050907     D DSPR          E DS
012500050907      *------------------
012600050907      * DS REPERIMENTO TERMINALs
012700050907      *------------------
012800050907     D FNLV55ds      E DS
012900090605     d
013000090605     d* tipi  servizio
013100090605     D ds5E          E DS
013200050908      *-------------------
013300050907      * DS CONTROLLA DATA (8)
013400050908      *-------------------
013500050907     D WLBDA8          DS                  inz
013600050907     D  G08DAT                 1      8  0 inz
013700050907     D  G08INV                 9     16  0 inz
013800050907     D  G08ERR                17     17    inz
013900050907     D  G08TGI                18     22  0 inz
014000050907      *-------------------
014100050907      * COSTANTI
014200050907      *-------------------
014300980218     D DECTMC          C                   CONST('Città')
014400170717     d dectmc2         c                   const(' Town')
014500060804     d dectmc3         c                   const('Ville')
014600060804     d dectmc4         c                   const('Stadt')
014700980218     D DECTMP          C                   CONST('Provincia')
014800170717     d dectmp2         c                   const(' Province')
014900170717     d dectmp3         c                   const(' Province')
015000170717     d dectmp4         c                   const('  Provinz')
015100170717     D DECTMI          C                   CONST('  Isole')
015200170717     D DECTMI2         C                   CONST(' Island')
015300170717     D DECTMI3         C                   CONST('   Îles')
015400170717     D DECTMI4         C                   CONST(' Inseln')
015500170717     D DECTMD          C                   CONST('   Loc.Disagiate')
015600170717     D DECTMD2         C                   CONST('s   Remote Area ')
015700170717     D DECTMD3         C                   CONST('   Loc.Éloignées')
015800170717     D DECTMD4         C                   CONST(' Entlegener Orte')
015900060612
016000090619     d isole           c                   const('Isole minori: previsti ulteri-
016100090619     d                                     ori 2 gg lavorativi.')
016200090619     d isole2          c                   const('Smaller islands: 2 more worki-
016300090619     d                                     ng days allowed.')
016400090619     d isole3          c                   const('Îles mineures: 2 jours ouvrab-
016500090619     d                                     les supplémentaires sont prévus.')
016600090619     d isole4          c                   const('Kleinere Inseln: zuzüglich 2 -
016700090619     d                                     weitere Werktage.')
016800090921     d
016900050907      *-------------------
017000050907      * VARIABILI RIFERITE AL DATABASE
017100050907      *-------------------
017200050913     d wPARtem         s                   like(PARTEM) inz
017300050907     d KTBKUT          s                   like(TBLKUT) inz
017400050907     d KTBCOD          s                   like(TBLCOD) inz
017500090605     d KTBkey          s                   like(TBLkey) inz
017600050907     d KORFIL          s                   like(ORGFIL) inz
017700050907     d KWDLNP          s                   like(DLYLNP) inz
017800050907     d KWDCOR          s                   like(DLYCOR) inz
017900050907     d WTEM            s                   like(DLYTRC) inz
018000050907     d WREG            s                   like(DLYRAP) inz
018100050907     d WPRV            s                   like(DLYPRV) inz
018200050908     d WLOC            s                   like(DLYLOC) inz
018300050907     d WCOR            s                   like(DLYCOR) inz
018400050907     d WTCN            s                   like(DLYTRI) inz
018500050907     d WTPN            s                   like(DLYTRI) inz
018600050907     d WTIN            s                   like(DLYTRI) inz
018700050907     d WTDN            s                   like(DLYTRI) inz
018800050907     d DEPLNP          s                   like(DLYLNP) inz
018900050907     d DEPREG          s                   like(DLYRAP) inz
019000050907     d DEPCOR          s                   like(DLYCOR) inz
019100050907     d DEPTEM          s                   like(DLYTRC) inz
019200050907      *-------------------
019300050907      * VARIABILI DI CONTROLLO
019400050907      *-------------------
019500050907     d $FINE           s              1    inz(*off)
019600050907     d $FINL           s              1    inz(*off)
019700050908     d $RecOK          s              1    inz(*off)
019800050907     d $DECR           s              1    inz(*off)
019900050907     d $TUG            s              1    inz(*off)
020000050907      *-------------------
020100050907      * VARIABILI IN STAMPA
020200050907      *-------------------
020300050907     d STMTSP          s             35    inz
020400050907     d STMTMC          s              5    inz
020500050907     d STMTMP          s              9    inz
020600170717     d STMTMI          s              7    inz
020700170717     d STMTMD          s             16    inz
020800050909     d STMREG          s             15    inz
020900050909     d STMPRV          s             35    inz
021000050907     d STMTCN          s              3  0 inz
021100050907     d STMTDC          s              1    inz
021200050907     d STMTCX          s              3  0 inz
021300050907     d STMTPN          s              3  0 inz
021400050907     d STMTDP          s              1    inz
021500050907     d STMTPX          s              3  0 inz
021600050907     d STMTIN          s              3  0 inz
021700050907     d STMTDI          s              1    inz
021800050907     d STMTIX          s              3  0 inz
021900050907     d STMTDN          s              3  0 inz
022000050907     d STMTDD          s              1    inz
022100050907     d STMTDX          s              3  0 inz
022200050909     d STMTEM          s              3  0 inz
022300050909     d STMLOC          s             90    inz
022400060612     d stmisole        s             75    inz
022500091204     d riga            s             95    inz
022600090609     d TempoLoc        s              3  0 inz
022700090609     d TempoPRV        s              3  0 inz
022800090609     d Savreg          s              2    inz
022900090609     d Savprv          s              2    inz
023000090609     d Savloc          s                   like(dlyloc)
023100090609     d rigaloc         s            100    inz
023200090611     d wstpreg         s               n
023300090611     d wstpprv         s               n
023400090611     d stpprv          s                   like(dlyprv)
023500090611     d stplocdta       s             35
023600050907      *-------------------
023700050907      * INDICI DI SCHIERA
023800050907      *-------------------
023900050907     d XC              s              4  0 inz
024000050907     d XI              s              4  0 inz
024100050907     d XJ              s              4  0 inz
024200050907     d XP              s              4  0 inz
024300050907     d XR              s              4  0 inz
024400050908     d XL              s              2  0 inz
024500090612
024600090612     d prtdly          ds
024700090612     d  curline              367    368i 0
024800050908      *-------------------
024900050907      * CHIAVI DI LETTURA
025000050908      *-------------------
025100050907     C     KEYTAB        KLIST
025200050907     C                   KFLD                    KTBKUT
025300050907     C                   KFLD                    KTBCOD
025400090605     C     Ktab          KLIST
025500090605     C                   KFLD                    KTBKUT
025600090605     C                   KFLD                    KTBCOD
025700090605     C                   KFLD                    KTBkey
025800050907     C     KEYORG        KLIST
025900050907     C                   KFLD                    KORFIL
026000050907     C     KEYDLY        KLIST
026100050907     C                   KFLD                    KWDLNP
026200050907     C     KE2DLY        KLIST
026300050907     C                   KFLD                    KWDLNP
026400050907     C                   KFLD                    KWDCOR
026500090609     C     Kdly6         KLIST
026600090609     C                   KFLD                    kwdlnp
026700090609     C                   KFLD                    partsp
026800050907      *------------------------------------------------------------------------*
026900050907      * MAIN LINE
027000050907      *------------------------------------------------------------------------*
027100090921
027200090921     c                   select
027300090921    0c                   when      partsp='H'
027400090922     c                   seton                                        15
027500090921     c* Nuova stampa per tipo H-H 10 30 solo a modo nuovo
027600090921     c                   exsr      Stampa_E
027700090921
027800090921     c* Nuova stampa per tipo E-priority a modo nuovo
027900090921    0c                   when      partsp='E' and parold<>'S'
028000090922     c                   seton                                        15
028100090609     c                   exsr      Stampa_E
028200090921   x0c                   other
028300980217     C*
028400980217     C* MEMORIZZA I TEMPI PER PROVINCIA DI TUTTE LE REGIONI
028500980217     C                   EXSR      LETTEM
028600960702     C*
028700960702     C* STAMPA L'INTESTAZIONE
028800980218     C                   EXSR      STAINT
028900960702     C*
029000980217     C* PER TUTTE LE REGIONI MEMORIZZATE
029100050907DO  1C     1             DO        20            XR
029200050907IF  2C     REG(XR)       IFNE      *BLANKS
029300960702     C*
029400980217     C* STAMPA TEMPI REGIONE
029500980217     C                   EXSR      STAREG
029600960702E   2C                   ENDIF
029700960702E   1C                   ENDDO
029800960702     C*
029900980217     C* FINE PROSPETTO
030000050913if  1C                   if        PARiso = 'N'
030100060612     c   11              eval      stmisole = isole
030200060612     c   12              eval      stmisole = isole2
030300060612     c   13              eval      stmisole = isole3
030400060612     c   14              eval      stmisole = isole4
030500050907     C                   except    PrtTxt1
030600050913e   1c                   endif
030700980218IF  1C     PARTSP        IFEQ      'D'
030800980218     C                   EXCEPT    NOD
030900980218E   1C                   ENDIF
031000980226IF  1C     PARND1        IFNE      *BLANKS
031100980226     C     PARND2        ORNE      *BLANKS
031200980226     C     PARND3        ORNE      *BLANKS
031300020919     C                   EXCEPT    NOT1
031400980226E   1C                   ENDIF
031500090609     c
031600090921    0c                   endsl
031700090609     c*
031800980218     C                   EXCEPT    FIR
031900980219     C* OPERAZIONI FINALI
032000980219     C                   EXSR      FINSR
032100960702     C*
032200050907     C                   EVAL      *INLR = *ON
032300090609      *------------------------------------------------------------------------*
032400090921      * Stampa a modo nuovo tipo E-priority    e H - 10 30
032500090609      *------------------------------------------------------------------------*
032600090609     c     Stampa_E      BEGSR
032700090609     c
032800090609     C* STAMPA L'INTESTAZIONE
032900090609     c                   exsr      StaINT
033000090609     c
033100090609     c                   clear                   savreg
033200090623     C                   MOVEL     depLNP        KWDLNP
033300090609     c
033400090609     c* Stampa tempo 12 e 24
033500090609     c                   exsr      Lettura_E
033600090609     c* Stampa 36 e oltre
033700090609     c
033800091006      * Salto pagina
033900090612     c                   if        curline >= 60
034000090612     C                   EXCEPT    SAL
034100090612     c                   endif
034200090612     c                   if        *inof
034300090612     c                   exsr      States_E
034400090612     c                   endif
034500090612     C                   EXCEPT    TES_E1
034600091006      * stampo riga tratteggio
034700091130     c                   eval      riga = *all'-'
034800090612     C                   EXCEPT    TES_E2
034900091130     c                   clear                   riga
035000090921
035100090609     c* stampo testata diversa per servizio "E"
035200090921     c                   if        partsp='E'
035300090609     C   11              MOVEL     $rig(2)       riga
035400090619     C   12              MOVEL     $rig(6)       riga
035500090619     C   13              MOVEL     $rig(9)       riga
035600090619     C   14              MOVEL     $rig(12)      riga
035700090921     c                   endif
035800090921     c* stampo testata diversa per servizio "H"
035900090921     c                   if        partsp='H'
036000090921     C   11              MOVEL     $rig(22)      riga
036100090921     C   12              MOVEL     $rig(26)      riga
036200090921     C   13              MOVEL     $rig(29)      riga
036300090921     C   14              MOVEL     $rig(32)      riga
036400090921     c                   endif
036500090921     c
036600090609     c                   if        *inof
036700090609     c                   exsr      States_E
036800090612     c                   endif
036900090609     c                   except    TES_E
037000091218     c
037100091218     c* Per "H" in tedesco, stampo una riga in più
037200100105     c** 14              if        partsp='H'
037300100105     c**                 MOVEL     $rig(33)      riga
037400100105     c**                 except    TES_E
037500100105     c**                 endif
037600091204
037700091006     c
037800091218     c* Per "E" stampo un'altra riga
037900091006     C                   IF        PARTSP='E'
038000091204
038100100105     c***                if        *in14
038200100105     c***                MOVEL     $rig(13)      riga
038300100105     C***                EXCEPT    TES_E
038400100105     c***                endif
038500091204
038600091006     C   11              MOVEL     $rig(3)       riga
038700091006     C   12              MOVEL     $rig(7)       riga
038800091006     C   13              MOVEL     $rig(10)      riga
038900091204     C   14              MOVEL     $rig(14)      riga
039000090921     C
039100091204     C                   EXCEPT    TES_E
039200091204     c**                 if        *in14
039300091204     c**                 MOVEL     $rig(34)      riga
039400091204     C**                 EXCEPT    TES_E
039500091204     c**                 endif
039600091006     C                   ENDIF
039700091006
039800091006     c* Frase nuova per il sito internet
039900091006     c                   except    TES_E1
040000091006     c                   exsr      staxinternet
040100091006     c
040200091006     c
040300090612      * stampo riga tratteggio
040400091130     c                   eval      riga = *all'-'
040500090612     C                   EXCEPT    TES_E2
040600091130     c                   clear                   riga
040700090609     c
040800090921     c                   if        partsp='E'
040900090609     c                   eval      Tempoloc=036
041000090609     c                   eval      Tempoprv=000
041100090921     c                   endif
041200090921     c                   if        partsp='H'
041300090921     c                   eval      Tempoloc=034
041400090921     c                   eval      Tempoprv=034
041500090921     c                   endif
041600090609     c
041700090609     c                   exsr      Lettura_E
041800090609     c
041900100108     c                   if        curline <  60
042000090612     C                   EXCEPT    TES_E1
042100100108     c                   endif
042200091006      * Salto pagina
042300091006     c                   if        curline >= 62
042400091006     C                   EXCEPT    SAL
042500091006     c                   endif
042600091006     c                   if        *inof
042700091006     c                   exsr      States_E
042800091006     c                   endif
042900100108     c
043000100108     c                   if        curline <  61
043100091006     C                   EXCEPT    TES_E1
043200100108     c                   endif
043300091006     c
043400090921     c                   if        partsp='E'
043500090619     C   11              eval      riga = isole
043600090619     C   12              eval      riga = isole2
043700090619     C   13              eval      riga = isole3
043800090619     C   14              eval      riga = isole4
043900090925     c                   except    TES_E
044000091006     C                   ENDIF
044100091006
044200091006     c* Riga delle località non in elenco
044300091006     C                   IF        PARTSP='H'
044400091006     C   11              MOVEL     $rig(23)      riga
044500091006     C   12              MOVEL     $rig(27)      riga
044600091006     C   13              MOVEL     $rig(30)      riga
044700091218     C   14              MOVEL     $rig(34)      riga
044800091006     C
044900091006     C                   EXCEPT    TES_E
045000091006     C                   ENDIF
045100090609     c
045200090609     c                   ENDSR
045300090609      *------------------------------------------------------------------------*
045400090609      * Lettura lf wadly per nuova stampa tipo  E_priority
045500090609      *------------------------------------------------------------------------*
045600090609     c     LETTURA_E     BEGSR
045700090609     c* Lettura file
045800090609     c     kdly6         setll     wadly06l
045900090609     c     kdly6         reade     wadly06l
046000090609    1c                   dow       not %eof(wadly06l)
046100090609     c
046200090609     c*  Verifico se i tempi sono quelli previsti
046300090609    2c                   if        (partem='I' and (dlytri=tempoprv or
046400090609     c                             dlytri=tempoloc or
046500090609     c                             (tempoprv=00 and dlytri>tempoloc))) or
046600090609     c                             (partem='C' and (dlytrc=tempoprv or
046700090609     c                             dlytrc=tempoloc or
046800090609     c                             (tempoprv=00 and dlytrc>tempoloc)))
046900090609     c
047000090609     c* A cambio regione stampo la decodifica
047100090609    3c                   if        savreg<>dlyrap
047200090611      * stampo l'ultima località della regione precedente
047300090609    4c                   if        rigaloc<>*blanks
047400090609     c                   exsr      StaloD_E
047500090609    4c                   endif
047600090611      * a cambio regione imposto flag spento, la regione la stampo se ho dei dat
047700090611      * da stampare
047800090611     c                   eval      wstpreg = *off
047900090609     c
048000090611      * decodifico regione
048100090609     c                   eval      XI       =  1
048200090609     c                   eval      *in98    =  *off
048300090609     c     dlyrap        lookup    Rap(XI)                                98
048400090609if  4c                   if        *in98    =  *on
048500090609     c                   movel(p)  Rdes(XI)      STMreg
048600090609x   4c                   else
048700090609     c                   movel     *all'*'       STMreg
048800090609e   4c                   endif
048900090609     c* e stampo
049000090611     c**********         exsr      STATES_E
049100090611     C**********         EXCEPT    REG_E
049200090609     c
049300090609     c                   eval      savreg=dlyrap
049400090609     c                   clear                   savprv
049500090609    3c                   endif
049600090609     c
049700090609     c* Memorizzo la provincia da stampare e la località se
049800090609     c*  nel tempo previsto
049900090609    3c                   if        savprv<>dlyprv
050000090609     c* Stampo l'ultima località della provincia precedente
050100090609    4c                   if        rigaloc<>*blanks
050200090609     c                   exsr      StaloD_E
050300090609    4c                   endif
050400090611      * a cambio provincia imposto flag spento, la provincia la stampo se ho dei
050500090611      * da stampare
050600090611     c                   eval      wstpprv = *off
050700090609     c
050800090611     c********           eval      rigaloc=dlyprv+'    .'
050900090609
051000090609     c                   eval      savprv=dlyprv
051100090609    3c                   endif
051200090609     c
051300090609     c* Memorizzo la località se da stampare
051400090609    3c                   if        dlyloc<>*blanks  and dlyloc<> savloc
051500090609     c
051600090609    4c                   if        (partem='I' and (dlytri=tempoloc))  or
051700090609     c                             (partem='C' and (dlytrc=tempoloc))
051800090609     c*
051900090609      * Se lo spazio disponibile sulla riga in esame [LOC(XL)] è
052000090609      *  sufficiente per elencare anche la località in esame [WLOC]
052100090609      *  => aggiungo la località [WLOC] su questa riga [LOC(XL)]
052200090609if  5c                   IF          ( %len( rigaloc )
052300090611     c                             -   %len( %trimr(rigaloc) ) )
052400090611     c                             >   (%len( %trimr(dlyLoc) ) +1)
052500090609      *
052600090611     c                   if        rigaloc = *blanks
052700090611     c                   eval      rigaloc   =  %trim(dlyLoc)
052800090609x   6c                   else
052900090609     c                   eval      rigaloc   =  %trim(rigaloc )
053000090609     c                                       +  ','
053100090609     c                                       +  %trim(dlyLoc)
053200090609e   6c                   endif
053300090609   x5c                   else
053400090611     c* non ci sto più nel campo
053500090609     c* Stampo la riga di località
053600090609     c                   exsr      Stalod_E
053700090611     c                   eval      rigaloc   =  %trim(dlyLoc)
053800090609    5c                   endif
053900090609     c
054000090609     c                   eval      savloc=dlyloc
054100090609    4c                   endif
054200090609    3c                   endif
054300090609     c
054400090609    2c                   endif
054500090609     c
054600090609     c
054700090609     c     kdly6         reade     wadly06l
054800090609    1c                   enddo
054900090609     c
055000090609     c* Stampo l'ultima località della provincia precedente
055100090609    1c                   if        rigaloc<>*blanks
055200090609     c                   exsr      StaloD_E
055300090609    1c                   endif
055400090609     c
055500090609     c                   ENDSR
055600050907      *------------------------------------------------------------------------*
055700050907      * STAREG - STAMPA TEMPI REGIONE
055800050907      *------------------------------------------------------------------------*
055900050908     c     STAREG        BEGSR
056000050908      *
056100050908      * PREPARAZIONE DATI
056200050908     c                   exsr      StaReg_1
056300050908      *
056400050908     c                   exsr      StaReg_2
056500050908      *
056600050908     c                   ENDSR
056700050908      *------------------------------------------------------------------------*
056800050908      * STAREG_1 -?PREPARAZIONE?TEMPI REGIONE
056900050908      *------------------------------------------------------------------------*
057000050908     C     StaReg_1      BEGSR
057100050908      *
057200050908      * AZZERA LE VARIABILI DI LAVORO
057300050908     C                   clear                   XC
057400050908     C                   clear                   XP
057500050908     C                   clear                   XI
057600050908     C                   clear                   XJ
057700980217     C                   CLEAR                   SDES
057800980219     C                   CLEAR                   SCOR
057900980217     C                   CLEAR                   SINP
058000980217     C                   CLEAR                   STCX
058100980217     C                   CLEAR                   STCN
058200980217     C                   CLEAR                   STPX
058300980217     C                   CLEAR                   STPN
058400980217     C                   CLEAR                   STIX
058500980217     C                   CLEAR                   STIN
058600980217     C                   CLEAR                   STDX
058700980217     C                   CLEAR                   STDN
058800050912     C                   RESET                   SELE
058900050907     C                   MOVEL     *OFF          $DECR
059000050908      *
059100050908      * SI POSIZIONA SUI DATI DELLA REGIONE DA STAMPARE
059200050907     C     XR            OCCUR     DREG
059300050908      *
059400050908      * RAGGRUPPA LE PROVINCIE CHE HANNO TEMPI UGUALI
059500050908      * - Posizioni 1 (capoluogo) e 2 (1ª di altre province)
059600050908     C     1             DO        2             XC
059700050908     C                   MOVEL     TCX(XC)       STCX(XC)
059800050908     C                   MOVEL     TCN(XC)       STCN(XC)
059900050908     C                   MOVEL     TPX(XC)       STPX(XC)
060000050908     C                   MOVEL     TPN(XC)       STPN(XC)
060100050908     C                   MOVEL     TIX(XC)       STIX(XC)
060200050908     C                   MOVEL     TIN(XC)       STIN(XC)
060300050908     C                   MOVEL     TDX(XC)       STDX(XC)
060400050908     C                   MOVEL     TDN(XC)       STDN(XC)
060500050908     C                   MOVEL     PRV(XC)       SDES(XC)
060600050908     C                   MOVEL     COR(XC)       SCOR(XC)
060700050912     C                   MOVEL     $Ele(XC)      SEle(XC)
060800050908     C                   Z-ADD     1             SINP(XC)
060900050908     C                   ENDDO
061000050908      * TEMPI UGUALI
061100110117DO  1C     3             DO        15            XP
061200050907     C                   MOVEL     *OFF          $TUG
061300050907IF  2C     PRV(XP)       IFNE      *BLANKS
061400110117DO  3C     2             DO        15            XI
061500050907IF  4C     SDES(XI)      IFNE      *BLANKS
061600980218IF  5C     PARSSA        IFEQ      'S'
061700050907IF  6C     TCX(XP)       IFEQ      STCX(XI)
061800050907     C     TCN(XP)       ANDEQ     STCN(XI)
061900050907     C     TPX(XP)       ANDEQ     STPX(XI)
062000050907     C     TPN(XP)       ANDEQ     STPN(XI)
062100050907     C     TIX(XP)       ANDEQ     STIX(XI)
062200050907     C     TIN(XP)       ANDEQ     STIN(XI)
062300050907     C     TDX(XP)       ANDEQ     STDX(XI)
062400050907     C     TDN(XP)       ANDEQ     STDN(XI)
062500050907     C                   ADD       1             SINP(XI)
062600050907     C                   MOVEA(P)  SDES(XI)      SS2
062700050908     C                   EVAL      SS2(SINP(XI)) = PRV(XP)
062800980218     C                   MOVEA(P)  SS2           A24              24
062900050907     C                   MOVEL(P)  A24           SDES(XI)
063000051116     c                   movea(p)  Scor(XI)      ss5
063100051116     c                   eval      ss5(Sinp(XI)) =
063200051116     c                             %subst( Cor(XP) : 1 :
063300051116     c                             %len( ss5(Sinp(XI)) ) )
063400051116     c                   movea(p)  ss5           a60              60
063500051116     c                   movel(p)  a60           Scor(XI)
063600050907     C                   MOVEL     *ON           $TUG
063700980218E   6C                   ENDIF
063800980218E   5C                   ENDIF
063900980218E   4C                   ENDIF
064000980217E   3C                   ENDDO
064100050908      * TEMPI DIVERSI
064200050907IF  3C     $TUG          IFEQ      *OFF
064300980219     C     PARSSA        OREQ      'A'
064400050907     C                   ADD       1             XC
064500050907     C                   MOVEL     TCX(XP)       STCX(XC)
064600050907     C                   MOVEL     TCN(XP)       STCN(XC)
064700050907     C                   MOVEL     TPX(XP)       STPX(XC)
064800050907     C                   MOVEL     TPN(XP)       STPN(XC)
064900050907     C                   MOVEL     TIX(XP)       STIX(XC)
065000050907     C                   MOVEL     TIN(XP)       STIN(XC)
065100050907     C                   MOVEL     TDX(XP)       STDX(XC)
065200050907     C                   MOVEL     TDN(XP)       STDN(XC)
065300050907     C                   MOVEL     PRV(XP)       SDES(XC)
065400050907     C                   MOVEL     COR(XP)       SCOR(XC)
065500050912     C                   MOVEL     $Ele(XP)      SEle(XC)
065600050907     C                   Z-ADD     1             SINP(XC)
065700980218E   3C                   ENDIF
065800980217E   2C                   ENDIF
065900980217E   1C                   ENDDO
066000050908      *
066100050908     C                   ENDSR
066200050908      *------------------------------------------------------------------------*
066300050908      * STAREG_2 -?STAMPA?DEI TEMPI REGIONE
066400050908      *------------------------------------------------------------------------*
066500050908     c     StaReg_2      BEGSR
066600050908      *
066700050908      * STAMPA DEI TEMPI "GENERICI" PER TUTTE LE PROVINCE DELLA REGIONE
066800050908      * - Decodifica la regione
066900050908     c                   eval      XI       =  1
067000050908     c                   eval      *in98    =  *off
067100050908     c     Reg(XR)       lookup    Rap(XI)                                98
067200050908if  1c                   if        *in98    =  *on
067300050908     c                   movel(p)  Rdes(XI)      STMreg
067400050908x   1c                   else
067500050908     c                   movel     *all'*'       STMreg
067600050908e   1c                   endif
067700050909      * Raggruppa solo se non sono previste eccezioni per il
067800050909      * capoluogo della regione (1ª provincia)
067900050909     c                   eval      XI       =  1
068000050913if  1c                   if             Sele(XI) =  *off
068100050913     c                             or   PARssa   =  'S'
068200050908      * - Imposta la costante per tutte le province della regione
068300050908     c                   movel     *all'.'       STMprv
068400050908      * - Imposta i tempi di resa indicativi per la regione
068500050908     c                   exsr      StaReg_3
068600050913x   1c                   else
068700050913     c                   clear                   XI
068800050913e   1c                   endif
068900050908      *
069000050908      * STAMPA DEI TEMPI "SPECIFICI" PER LE ECCEZIONI
069100050909do  1c                   DOW       XI       <  %elem(Stcn)
069200050909     c                   eval      XI       =  XI + 1
069300050908      *
069400050908if  2c                   IF             Sdes(XI) <> *blanks
069500050912     c                             and((Stcn(XI) <> Stcn(1)
069600050913     c                               or Stcx(XI) <> Stcx(1)
069700050913     c                               or Stpn(XI) <> Stpn(1)
069800050913     c                               or Stpx(XI) <> Stpx(1)
069900050913     c                               or Stin(XI) <> Stin(1)
070000050913     c                               or Stix(XI) <> Stix(1)
070100050913     c                               or Stdn(XI) <> Stdn(1)
070200050913     c                               or Stdx(XI) <> Stdx(1)
070300050913     c                               or Sele(XI) =  *on
070400050913     c                               and  PARssa =  'A')
070500050913     c                               or XI       =  1
070600050913     c                               or Sele(1)  =  *on
070700050913     c                               and  PARssa =  'A')
070800050909      *
070900050909if  3c                   if        XI       >  1
071000050909     c                   clear                   STMreg
071100050909e   3c                   endif
071200050909      *
071300050909if  3c                   IF        Sinp(XI) >  1
071400050908      *
071500050908      * - "Concatena" le sigle delle province
071600050908     c                   movea     Sdes(XI)      Ss2
071700050908do  4c                   do        Sinp(XI)      XJ
071800050909if  5c                   if        XJ       =  1
071900050908     c                   movel(p)  Ss2(XJ)       STMprv
072000050908x   5c                   else
072100050909     c                   eval      STMprv   =  %trim(STMprv)
072200050909     c                                      +  '/'
072300050909     c                                      +  Ss2(XJ)
072400050908e   5c                   endif
072500050908e   4c                   enddo
072600050908      *
072700050908x   3c                   ELSE
072800050908      *
072900050908      * - Decodifica la singola provincia
073000050908     c                   movel     Sdes(XI)      Wprv
073100050909     c                   eval      XJ       =  1
073200050909     c                   eval      *in98    =  *off
073300050908     c     Wprv          lookup    Pro(XJ)                                98
073400050909if  4c                   if        *in98    =  *on
073500050908     c                   movel(P)  Pdes(XJ)      STMprv
073600050908x   4c                   else
073700050909     c                   eval      STMprv   =  *all'*'
073800050908e   4c                   endif
073900050908      *
074000050908e   3c                   ENDIF
074100050915      * - aggiunge "..." finali
074200050915     c                   eval      %subst( STMprv :
074300050915     c                                     %len(%trimr(STMprv)) + 2)
074400050915     c                             = *all'.'
074500050908      *
074600050908      * - Imposta i tempi di resa indicativi per le province
074700050908     c                   exsr      StaReg_3
074800050908      *
074900050908e   2c                   ENDIF
075000050908      *
075100050908e   1c                   ENDDO
075200050908      *
075300050908     c                   ENDSR
075400050908      *------------------------------------------------------------------------*
075500050908      * StaReg_3 - Imposta i tempi di resa indicativi per le province
075600050908      *            e stampa la riga
075700050908      *------------------------------------------------------------------------*
075800050908     c     StaReg_3      BEGSR
075900050908      *
076000050908      * Città
076100050908     c                   eval      STMtcn   =  Stcn(XI)
076200050908if  1c                   if        Stcx(XI) <> Stcn(XI)
076300050908     c                   eval      STMtdc   =  '/'
076400050908     c                   eval      STMtcx   =  Stcx(XI)
076500050908x   1c                   else
076600050908     c                   clear                   STMtdc
076700050908     c                   clear                   STMtcx
076800050908e   1c                   endif
076900050908      * Provincia
077000050908     c                   eval      STMtpn   =  Stpn(XI)
077100050908if  1c                   if        Stpx(XI) <> Stpn(XI)
077200050908     c                   eval      STMtdp   =  '/'
077300050908     c                   eval      STMtpx   =  Stpx(XI)
077400050908x   1c                   else
077500050908     c                   clear                   STMtdp
077600050908     c                   clear                   STMtpx
077700050908e   1c                   endif
077800050908      * Isola
077900050908     c                   eval      STMtin   =  Stin(XI)
078000050908if  1c                   if        Stix(XI) <> Stin(XI)
078100050908     c                   eval      STMtdi   =  '/'
078200050908     c                   eval      STMtix   =  STIX(XI)
078300050908x   1c                   else
078400050908     c                   clear                   STMTDI
078500050908     c                   clear                   STMTIX
078600050908e   1c                   endif
078700050908      * Zone Disagiate
078800050908     c                   eval      STMtdn   =  STDN(XI)
078900050908if  1c                   if        Stdx(XI) <> Stdn(XI)
079000050908     c                   eval      STMtdd   =  '/'
079100050908     c                   eval      STMtdx   =  STDX(XI)
079200050908x   1c                   else
079300050908     c                   clear                   STMTDD
079400050908     c                   clear                   STMTDX
079500050908e   1c                   endif
079600050908      * > Stampa
079700050908     c                   exsr      StaTES
079800050908     c                   except    SIN
079900050908      * - Stampa analitica
080000050908if  1c                   if        PARssa   =  'A'
080100050908     c                   movel     Scor(XI)      Wcor
080200050908     c                   z-add     Stcn(XI)      Wtcn
080300050908     c                   z-add     Stpn(XI)      Wtpn
080400050908     c                   z-add     Stin(XI)      Wtin
080500050908     c                   z-add     Stdn(XI)      Wtdn
080600050908     c                   exsr      MemLOC
080700090604     c                   endif
080800050908      *
080900050908     c                   ENDSR
081000050907      *------------------------------------------------------------------------*
081100050907      * STAINT - STAMPA L'INTESTAZIONE
081200050907      *------------------------------------------------------------------------*
081300980218     C     STAINT        BEGSR
081400050909      *---
081500050909      * INTESTAZIONE
081600050909      *---
081700980219     C                   MOVEL     PARLNP        KORFIL
081800050907     C     KEYORG        CHAIN     AZORG                              98
081900090605     C* AGGANCIO  tipo servizio, in lingua se richiesto
082000090605     c   11              movel     1             ktbkut
082100090605     c  n11              movel     parlin        ktbkut
082200090605     c                   eval      ktbcod='5E'
082300090605     c                   clear                   ktbkey
082400090605     c                   eval      ktbkey=partsp
082500090605     c     ktab          chain     tabel00f
082600090605     c                   movel     tbluni        ds5e
082700090605     c                   eval      stmtsp=§5eddly
082800980225     C*
082900050913if  1C                   if        PARtem  = 'I'
083000050913     C                   eval      wPARtem = PARtem
083100050913e   1C                   endif
083200090611      * imposto localita + data
083300090611     c                   clear                   stplocdta
083400090611     c                   eval      stplocdta = orgloc
083500090611     c                   eval      stplocdta = %trim(stplocdta) + ','
083600090611     c   11              eval      stplocdta = %trim(stplocdta) + ' li'
083700090611     c   13              eval      stplocdta = %trim(stplocdta) + ' le'
083800090611     c                   eval      stplocdta = %trim(stplocdta) + ' ' +
083900090611     c                             %subst(%editc(%dec(udate):'X'):1:2)+
084000090611     c                             '/' +
084100090611     c                             %subst(%editc(%dec(udate):'X'):3:2)+
084200090611     c                             '/' +
084300090611     c                             %subst(%editc(%dec(udate):'X'):5:2)
084400090605     c
084500980218     C                   EXCEPT    INT
084600050909      *---
084700090922      * TESTATA   per servizio "E" nuovo
084800050909      *---
084900090922     c                   select
085000090922     c                   when      partsp='E' and parold=' '
085100090612     C                   EXCEPT    TES_E1
085200090921
085300090605     C   11              MOVEL     $rig(1)       riga
085400090619     C   12              MOVEL     $rig(5)       riga
085500090619     C   13              MOVEL     $rig(8)       riga
085600090619     C   14              MOVEL     $rig(11)      riga
085700090605     C                   EXCEPT    TES_E
085800090612     c
085900091006     C   11              MOVEL     $rig(3)       riga
086000091006     C   12              MOVEL     $rig(7)       riga
086100091006     C   13              MOVEL     $rig(10)      riga
086200091204     C   14              MOVEL     $rig(14)      riga
086300091006     C                   EXCEPT    TES_E
086400091204     c**                 if        *in14
086500091204     c**                 MOVEL     $rig(14)      riga
086600091204     C**                 EXCEPT    TES_E
086700091204     c**                 endif
086800091006     c
086900091006     c* Nuova frase per le località
087000091006     C                   EXCEPT    TES_E1
087100091006     c                   exsr      StaxInternet
087200091006
087300090609     c
087400090609     c* Imposto  le ore per il primo giro 12 e 24
087500090609     c                   eval      tempoloc=012
087600090609     c                   eval      tempoprv=024
087700090921     c*
087800090922      *---
087900090922      * TESTATA   per servizio "H"
088000090922      *---
088100090922     c                   when      partsp='H'
088200090922     C                   EXCEPT    TES_E1
088300090921
088400090921     C   11              MOVEL     $rig(21)      riga
088500090921     C   12              MOVEL     $rig(25)      riga
088600090921     C   13              MOVEL     $rig(28)      riga
088700090921     C   14              MOVEL     $rig(31)      riga
088800090921     C                   EXCEPT    TES_E
088900091006     C                   EXCEPT    TES_E1
089000090921     c
089100091006     C***11              MOVEL     $rig(23)      riga
089200091006     C***12              MOVEL     $rig(27)      riga
089300091006     C***13              MOVEL     $rig(30)      riga
089400091006     C***14              MOVEL     $rig(33)      riga
089500091006     C***                EXCEPT    TES_E
089600091006     c***                if        *in14
089700091006     c***                MOVEL     $rig(34)      riga
089800091006     C***                EXCEPT    TES_E
089900091006     c***                endif
090000090925     c* Nuova frase per le località
090100091006     c                   exsr      StaxInternet
090200090925
090300090921     c
090400090921     c* Imposto  le ore per il primo giro 10
090500090921     c                   eval      tempoloc=010
090600090921     c                   eval      tempoprv=010
090700090922     c                   other
090800090921     c
090900090922     c* altri tipi servizio
091000090922     C   11              MOVEL     DECTMC        STMTMC
091100090922     C   11              MOVEL     DECTMP        STMTMP
091200090922     c   12              movel     dectmc2       stmtmc
091300090922     c   12              movel     dectmp2       stmtmp
091400090922     c   13              movel     dectmc3       stmtmc
091500090922     c   13              movel     dectmp3       stmtmp
091600090922     c   14              movel     dectmc4       stmtmc
091700090922     c   14              movel     dectmp4       stmtmp
091800090922IF  1C     PARISO        IFEQ      'S'
091900170717     C   11              MOVEL     DECTMI        STMTMI
092000170717     C   12              MOVEL     DECTMI2       STMTMI
092100170717     C   13              MOVEL     DECTMI3       STMTMI
092200170717     C   14              MOVEL     DECTMI4       STMTMI
092300090922E   1C                   ENDIF
092400090922IF  1C     PARDIS        IFEQ      'S'
092500170717     C   11              MOVEL     DECTMD        STMTMD
092600170717     C   12              MOVEL     DECTMD2       STMTMD
092700170717     C   13              MOVEL     DECTMD3       STMTMD
092800170717     C   14              MOVEL     DECTMD4       STMTMD
092900090922E   1C                   ENDIF
093000090922     C                   if            PARiso = 'S'
093100090922     C                             or  PARdis = 'S'
093200090922     C                   eval      *in41 = *on
093300090922     C                   else
093400090922     C                   eval      *in41 = *off
093500090922     C                   endif
093600090922     C                   EXCEPT    TES
093700090922     c                   ENDSL
093800090922     c*
093900050909      *
094000980218     C                   ENDSR
094100091006      *------------------------------------------------------------------------*
094200091006      * Stampa frase per il sito internet solo per "E" e "H"
094300091006      *------------------------------------------------------------------------*
094400091006     c     StaxInternet  BEGSR
094500091006     c* Italiano
094600091006     c                   if        *in11
094700091218     c                   eval      riga=%trim($rig(36))
094800100105     c                   eval      riga=%trim(riga)+' '+stmtsp
094900091006     C                   EXCEPT    TES_E
095000091218     c                   eval      riga=$rig(37)
095100091203     C                   EXCEPT    TES_E
095200091218     c                   eval      riga=$rig(38)
095300091006     C                   EXCEPT    TES_E
095400091006     c                   endif
095500091006     c* Inglese
095600091006     c                   if        *in12
095700100105     C                   IF        PARTSP='E'
095800100105     c                   eval      riga=%trim($rig(39))
095900100105     c                   else
096000100105     c                   eval      riga=%trim($rig(40))
096100100105     c                   endif
096200091006     C                   EXCEPT    TES_E
096300091203     c                   eval      riga=$rig(41)
096400091203     C                   EXCEPT    TES_E
096500091203     c                   eval      riga=$rig(42)
096600091006     C                   EXCEPT    TES_E
096700091218     c                   eval      riga=$rig(43)
096800091218     C                   EXCEPT    TES_E
096900091006     c                   endif
097000091006     c* Francese
097100091006     c                   if        *in13
097200091130     c                   eval      riga=%trim($rig(44))
097300100105     c                   eval      riga=%trim(riga)+' '+stmtsp
097400091006     C                   EXCEPT    TES_E
097500091130     c                   eval      riga=$rig(45)
097600091130     C                   EXCEPT    TES_E
097700091130     c                   eval      riga=$rig(46)
097800091006     C                   EXCEPT    TES_E
097900091218     c                   eval      riga=$rig(47)
098000091218     C                   EXCEPT    TES_E
098100091006     c                   endif
098200091006     c* Tedesco
098300091006     c                   if        *in14
098400091218     c                   eval      riga=%trim($rig(48))
098500100105     c                   eval      riga=%trim(riga)+' '+stmtsp
098600091006     C                   EXCEPT    TES_E
098700091218     c                   eval      riga=$rig(49)
098800091006     C                   EXCEPT    TES_E
098900091218     c                   eval      riga=$rig(50)
099000091204     C                   EXCEPT    TES_E
099100091006     c                   endif
099200091006     c                   ENDSR
099300050907      *------------------------------------------------------------------------*
099400050907      * STATES - STAMPA TESTATA
099500050907      *------------------------------------------------------------------------*
099600980218     C     STATES        BEGSR
099700980218     C*
099800980218     C* SE FINE PAGINA, STAMPA TESTATA
099900980218IF  1C     *INOF         IFEQ      *ON
100000980220     C                   EXCEPT    SAL
100100980220     C                   EXCEPT    TES
100200980218     C                   SETOFF                                       OF
100300980218E   1C                   ENDIF
100400980217     C*
100500980217     C                   ENDSR
100600090609      *------------------------------------------------------------------------*
100700090609     c* Testata per E-prioriry
100800090609      *------------------------------------------------------------------------*
100900090609     c     STATES_E      BEGSR
101000090609    4c                   if        *inof
101100090609     C                   EXCEPT    SAL
101200090610     C**!!!              EXCEPT    tes_E
101300090609     C                   SETOFF                                       OF
101400090609E   4C                   ENDIF
101500090609     c                   ENDSR
101600090609      *------------------------------------------------------------------------*
101700090609      * Stampa località per E-priority
101800090609      *------------------------------------------------------------------------*
101900090609     C     STALOD_E      BEGSR
102000090610
102100090609     c                   exsr      states_E
102200090611
102300090611      * se non ho ancora stampato la regione lo faccio ora
102400090611     c                   if        not wstpreg
102500090611     c                   except    reg_e
102600090611     c                   eval      wstpreg = *on
102700090611     c                   endif
102800090611
102900090611      * se non ho ancora stampato la provincia lo faccio ora
103000090611     c                   if        not wstpprv
103100090611     c                   eval      stpprv = savprv
103200090611     c                   eval      wstpprv = *on
103300090611     c                   else
103400090611     c                   clear                   stpprv
103500090611     c                   endif
103600090609     c
103700090609     c                   except    lod_e
103800090609     c                   clear                   rigaloc
103900090609     c
104000090609     c                   ENDSR
104100050907      *------------------------------------------------------------------------*
104200050907      * MEMLOC - MEMORIZZA LE LOCALITA' DELLA PROVINCIA
104300050907      *------------------------------------------------------------------------*
104400980219     C     MEMLOC        BEGSR
104500980219     C*
104600980219     C* REIMPOSTA LE VARIABILI DI LAVORO
104700050907     C                   MOVEL     *OFF          $FINL
104800980219     C                   CLEAR                   LOC
104900090604     C                   CLEAR                   wloc
105000980219     C*
105100980219     C* POSIZIONAMENTO E PRIMA LETTURA -LOCALITA' PROVINCIA-
105200980219     C                   EXSR      SETLOC
105300980219     C*
105400980219     C* CICLO FINO A FINE FILE
105500050907DO  1C     $FINL         DOWEQ     *OFF
105600980219     C*
105700980219     C* OPERAZIONI PER NUOVO TEMPO
105800980219     C                   EXSR      NEWTEM
105900980219     C*
106000980219     C* CICLO FINO A ROTTURA TEMPO
106100050907DO  2C     $FINL         DOWEQ     *OFF
106200980219     C     WTEM          ANDEQ     DEPTEM
106300980219     C*
106400980219     C* COMPATTA LA LOCALITA'
106500050908     C                   MOVEL     DLYLOC        WLOC
106600980219     C                   EXSR      COMLOC
106700980219     C*
106800980219     C* LETTURA RECORD SUCCESSIVO -LOCALITA' PROVINCIA-
106900980219     C                   EXSR      LETLOC
107000980219E   2C                   ENDDO
107100980219     C*
107200980219     C* STAMPA LE LOCALITA' DELLA PROVINCIA
107300980219     C                   EXSR      STALOC
107400980219E   1C                   ENDDO
107500980218     C*
107600980218     C                   ENDSR
107700050907      *------------------------------------------------------------------------*
107800050907      * SETLOC - POSIZIONAMENTO E PRIMA LETTURA -LOCALITA' PROVINCIA-
107900050907      *------------------------------------------------------------------------*
108000980219     C     SETLOC        BEGSR
108100980219     C*
108200980219     C* POSIZIONAMENTO IN BASE AL TIPO TEMPO RICHIESTO
108300980219     C                   MOVEL     DEPLNP        KWDLNP
108400051115     C                   MOVEL     WCOR          KWDCOR
108500980219IF  1C     PARTEM        IFEQ      'I'
108600050907     C     KE2DLY        SETLL     WADLY3                             99
108700980219X   1C                   ELSE
108800050907     C     KE2DLY        SETLL     WADLY4                             99
108900980219E   1C                   ENDIF
109000980219IF  1C     *IN99         IFEQ      *ON
109100050907     C                   MOVEL     *ON           $FINL
109200980219X   1C                   ELSE
109300980219     C                   EXSR      LETLOC
109400980219E   1C                   ENDIF
109500980219     C*
109600980219     C                   ENDSR
109700050907      *------------------------------------------------------------------------*
109800050907      * LETLOC - LETTURA PROSSIMO RECORD -LOCALITA' PROVINCIA-
109900050907      *------------------------------------------------------------------------*
110000980219     C     LETLOC        BEGSR
110100980219     C*
110200980219     C* LEGGE FINO A:
110300980219     C*     - FINE FILE
110400980219     C*     - TROVATO RECORD VALIDO
110500050908     C                   MOVEL     *OFF          $RECOK
110600050907DO  1C     $FINL         DOWEQ     *OFF
110700050908     C     $RECOK        ANDEQ     *OFF
110800980219IF  2C     PARTEM        IFEQ      'I'
110900050907     C     KE2DLY        READE     WADLY3                                 99
111000980219X   2C                   ELSE
111100050907     C     KE2DLY        READE     WADLY4                                 99
111200980219E   2C                   ENDIF
111300980219IF  2C     *IN99         IFEQ      *ON
111400050907     C                   MOVEL     *ON           $FINL
111500980219X   2C                   ELSE
111600980219     C                   EXSR      CHKREC
111700051115IF  2C     $RECOK        IFEQ      *ON
111800980219     C                   EXSR      REPDAT
111900980219     C                   EXSR      CHKLOC
112000980219E   3C                   ENDIF
112100980219E   2C                   ENDIF
112200980219E   1C                   ENDDO
112300980219     C*
112400980219     C                   ENDSR
112500050907      *------------------------------------------------------------------------*
112600050907      * CHKLOC - CONTROLLA RECORD -LOCALITA' PROVINCIA-
112700050907      *------------------------------------------------------------------------*
112800980219     C     CHKLOC        BEGSR
112900051114      *
113000050908     C                   MOVEL     *OFF          $RECOK
113100051114      *
113200051114if  1c                   IF            (DLYiso = 'C'
113300051114     c                             and  wTEM   > wTCN
113400051114     c                             OR   DLYiso = 'P'
113500051114     c                             and  wTEM   > wTPN
113600051114     c                             OR   PARiso = 'S'
113700051114     c                             and  DLYiso = 'I'
113800051114     c                             and  wTEM   > wTIN
113900051114     c                             OR   PARdis = 'S'
114000051114     c                             and  DLYiso = 'D'
114100051114     c                             and  wTEM   > wTDN)
114200051114     c                             and  DLYdlv = 'S'
114300051114     c                   eval      $RecOK = *on
114400051114e   1c                   endif
114500051114      *
114600980219     C                   ENDSR
114700050907      *------------------------------------------------------------------------*
114800050907      * NEWTEM - OPERAZIONI PER NUOVO TEMPO
114900050907      *------------------------------------------------------------------------*
115000050908     c     NEWTEM        BEGSR
115100050908      *
115200050908      * MEMORIZZA IL NUOVO CODICE IN UN DEPOSITO
115300050908     c                   movel     wTem          DEPtem
115400050908      *
115500050908     c                   ENDSR
115600050907      *------------------------------------------------------------------------*
115700050907      * COMLOC - COMPATTA LE LOCALITA'
115800050907      *------------------------------------------------------------------------*
115900050908     c     COMLOC        BEGSR
116000050908      *
116100050909if  1c                   if        wLoc      = *blanks
116200050908     c                   leavesr
116300050908e   1c                   endif
116400050908      *
116500050909     c                   eval      XL        = 1
116600050909do  1c                   DOW       XL       <= %elem(Loc)
116700050908      *
116800050908      * Se lo spazio disponibile sulla riga in esame [LOC(XL)] è
116900050908      *  sufficiente per elencare anche la località in esame [WLOC]
117000050908      *  => aggiungo la località [WLOC] su questa riga [LOC(XL)]
117100050909if  2c                   IF          ( %len( Loc(XL) )
117200050908     c                             -   %len( %trimr(Loc(XL)) ) )
117300050909     c                             >   %len( %trimr(wLoc) )
117400050908      *
117500050909if  3c                   if        Loc(XL)   =  *blanks
117600050908     c                   movel(p)  wLoc          Loc(XL)
117700050908x   3c                   else
117800050909     c                   eval      Loc(XL)   =  %trim(Loc(XL))
117900050909     c                                       +  ','
118000050909     c                                       +  %trim(wLoc)
118100050908e   3c                   endif
118200050908     c                   leave
118300050908      *
118400050908      * Altrimenti vado sulla riga successiva ([LOC(XL+1)]
118500050908x   2c                   else
118600050908     c                   add       1             XL
118700050908e   2c                   endif
118800050908      *
118900050908e   1c                   ENDDO
119000050908      *
119100050908     c                   ENDSR
119200050907      *------------------------------------------------------------------------*
119300050907      * STALOC - STAMPA LE LOCALITA' DELLA PROVINCIA DEL TEMPO
119400050907      *------------------------------------------------------------------------*
119500980219     C     STALOC        BEGSR
119600050909      *
119700980219IF  1C     LOC(1)        IFNE      *BLANKS
119800050907DO  2C     1             DO        30            XL
119900050907IF  3C     LOC(XL)       IFNE      *BLANKS
120000050907IF  4C     XL            IFEQ      1
120100050907     C                   Z-ADD     DEPTEM        STMTEM
120200980219X   4C                   ELSE
120300980219     C                   Z-ADD     *ZEROS        STMTEM
120400980219E   4C                   ENDIF
120500050909if  4c                   if            XL        <  %elem(Loc)
120600050909     c                             and Loc(XL+1) <> *blanks
120700050909     c                   eval      STMloc = %trimr(Loc(XL))
120800050909     c                                    + ','
120900050909x   4c                   else
121000050909     C                   MOVEL(p)  LOC(XL)       STMLOC
121100050909e   4c                   endif
121200980219     C                   EXSR      STATES
121300980219     C                   EXCEPT    LOD
121400980219E   3C                   ENDIF
121500980219E   2C                   ENDDO
121600980219E   1C                   ENDIF
121700050909      *
121800980219     C                   CLEAR                   LOC
121900050909      *
122000980219     C                   ENDSR
122100050907      *------------------------------------------------------------------------*
122200050907      * LETTEM - MEMORIZZA I TEMPI PER PROVINCIA DI TUTTE LE REGIONI
122300050907      *------------------------------------------------------------------------*
122400980217     C     LETTEM        BEGSR
122500980217     C*
122600980217     C* IMPOSTA LE VARIABILI DI LAVORO
122700050907     C                   Z-ADD     *ZEROS        XR
122800050907     C                   Z-ADD     *ZEROS        XP
122900960702     C*
123000050318     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA -FILE-
123100960702     C                   EXSR      SETFIL
123200960702     C*
123300960702     C* CICLO FINO A FINE FILE
123400050907DO  1C     $FINE         DOWEQ     *OFF
123500960702     C*
123600960702     C* OPERAZIONI PER NUOVA REGIONE
123700980217     C                   EXSR      NEWREG
123800960702     C*
123900980217     C* CICLO FINO A ROTTURA REGIONE
124000050907DO  2C     $FINE         DOWEQ     *OFF
124100980217     C     WREG          ANDEQ     DEPREG
124200980217     C*
124300980217     C* OPERAZIONI PER NUOVA PROVINCIA
124400980217     C                   EXSR      NEWPRV
124500980217     C*
124600980217     C* CICLO FINO A ROTTURA PROVINCIA
124700050907DO  3C     $FINE         DOWEQ     *OFF
124800980217     C     WREG          ANDEQ     DEPREG
124900980217     C     DLYCOR        ANDEQ     DEPCOR
125000960702     C*
125100980217     C* MEMORIZZA I TEMPI MINIMI E MASSIMI
125200980217     C                   EXSR      MEMTEM
125300980219     C*
125400050318     C* LETTURA RECORD SUCCESSIVO -FILE-
125500960702     C                   EXSR      LETFIL
125600980217E   3C                   ENDDO
125700980217E   2C                   ENDDO
125800960702E   1C                   ENDDO
125900960702     C*
126000960702     C                   ENDSR
126100050907      *------------------------------------------------------------------------*
126200050907      * MEMTEM - MEMORIZZA I TEMPI MINIMI E MASSIMI
126300050907      *------------------------------------------------------------------------*
126400980217     C     MEMTEM        BEGSR
126500980217     C* CITTA'
126600980217IF  1C     DLYISO        IFEQ      'C'
126700050907IF  2C     WTEM          IFLT      TCN(XP)
126800050907     C     TCN(XP)       OREQ      *ZEROS
126900050907     C                   Z-ADD     WTEM          TCN(XP)
127000980217E   2C                   ENDIF
127100050907IF  2C     WTEM          IFGT      TCX(XP)
127200050907     C     TCX(XP)       OREQ      *ZEROS
127300050907     C                   Z-ADD     WTEM          TCX(XP)
127400980217E   2C                   ENDIF
127500980217E   1C                   ENDIF
127600980217     C* PROVINCIA
127700980217IF  1C     DLYISO        IFEQ      'P'
127800050907IF  2C     WTEM          IFLT      TPN(XP)
127900050907     C     TPN(XP)       OREQ      *ZEROS
128000050907     C                   Z-ADD     WTEM          TPN(XP)
128100980217E   2C                   ENDIF
128200050907IF  2C     WTEM          IFGT      TPX(XP)
128300050907     C     TPX(XP)       OREQ      *ZEROS
128400050907     C                   Z-ADD     WTEM          TPX(XP)
128500980217E   2C                   ENDIF
128600980217E   1C                   ENDIF
128700980217     C* ISOLA
128800980217IF  1C     PARISO        IFEQ      'S'
128900980217IF  2C     DLYISO        IFEQ      'I'
129000050907IF  3C     WTEM          IFLT      TIN(XP)
129100050907     C     TIN(XP)       OREQ      *ZEROS
129200050907     C                   Z-ADD     WTEM          TIN(XP)
129300980217E   3C                   ENDIF
129400050907IF  3C     WTEM          IFGT      TIX(XP)
129500050907     C     TIX(XP)       OREQ      *ZEROS
129600050907     C                   Z-ADD     WTEM          TIX(XP)
129700980217E   3C                   ENDIF
129800980217E   2C                   ENDIF
129900980217E   1C                   ENDIF
130000980217     C* DISAGIATE
130100980217IF  1C     PARDIS        IFEQ      'S'
130200980219IF  2C     DLYISO        IFEQ      'D'
130300050907IF  3C     WTEM          IFLT      TDN(XP)
130400050907     C     TDN(XP)       OREQ      *ZEROS
130500050907     C                   Z-ADD     WTEM          TDN(XP)
130600980217E   3C                   ENDIF
130700050907IF  3C     WTEM          IFGT      TDX(XP)
130800050907     C     TDX(XP)       OREQ      *ZEROS
130900050907     C                   Z-ADD     WTEM          TDX(XP)
131000980217E   3C                   ENDIF
131100980217E   2C                   ENDIF
131200980217E   1C                   ENDIF
131300050912     C*
131400050912     C* Memorizzo se località elencabili per provincia
131500050913     C*  (se richiesta la stampa analitica)
131600050913if  1c                   if        PARssa   = 'A'
131700050913     c                   z-add     TCN(xp)       Wtcn
131800050913     c                   z-add     TPN(xp)       Wtpn
131900050913     c                   z-add     TIN(xp)       Wtin
132000050913     c                   z-add     TDN(xp)       Wtdn
132100050913     C                   exsr      ChkLoc
132200050913     C                   eval      $Ele(xp) = $RecOK
132300050913e   1C                   endif
132400980217     C*
132500980217     C                   ENDSR
132600050907      *------------------------------------------------------------------------*
132700050907      * SETFIL - POSIZIONAMENTO E PRIMA LETTURA DEL FILE -FILE-
132800050907      *------------------------------------------------------------------------*
132900980217     C     SETFIL        BEGSR
133000980217     C*
133100980217     C* POSIZIONAMENTO IN BASE AL TIPO TEMPO RICHIESTO
133200980219     C                   MOVEL     DEPLNP        KWDLNP
133300980217IF  1C     PARTEM        IFEQ      'I'
133400050907     C     KEYDLY        SETLL     WADLY000                           99
133500980217X   1C                   ELSE
133600050907     C     KEYDLY        SETLL     WADLY2                             99
133700960702E   1C                   ENDIF
133800960702IF  1C     *IN99         IFEQ      *ON
133900050907     C                   MOVEL     *ON           $FINE
134000960702X   1C                   ELSE
134100960702     C                   EXSR      LETFIL
134200960702E   1C                   ENDIF
134300960702     C*
134400960702     C                   ENDSR
134500050907      *------------------------------------------------------------------------*
134600050907      * LETFIL - LETTURA PROSSIMO RECORD -FILE-
134700050907      *------------------------------------------------------------------------*
134800960702     C     LETFIL        BEGSR
134900980217     C*
135000980217     C* LEGGE FINO A:
135100980217     C*     - FINE FILE
135200980217     C*     - TROVATO RECORD VALIDO
135300050908     C                   MOVEL     *OFF          $RECOK
135400050907DO  1C     $FINE         DOWEQ     *OFF
135500050908     C     $RECOK        ANDEQ     *OFF
135600980217IF  2C     PARTEM        IFEQ      'I'
135700050907     C     KEYDLY        READE     WADLY000                               99
135800980217X   2C                   ELSE
135900050907     C     KEYDLY        READE     WADLY2                                 99
136000980217E   2C                   ENDIF
136100980217IF  2C     *IN99         IFEQ      *ON
136200050907     C                   MOVEL     *ON           $FINE
136300980217X   2C                   ELSE
136400980217     C                   EXSR      CHKREC
136500051115IF  3C     $RECOK        IFEQ      *ON
136600980217     C                   EXSR      REPDAT
136700980217E   3C                   ENDIF
136800980217E   2C                   ENDIF
136900980217E   1C                   ENDDO
137000960702     C*
137100960702     C                   ENDSR
137200050907      *------------------------------------------------------------------------*
137300050907      * CHKREC - CONTROLLA VALIDITA' RECORD -FILE-
137400050907      *------------------------------------------------------------------------*
137500960702     C     CHKREC        BEGSR
137600960702     C*
137700050908     C                   MOVEL     *ON           $RECOK
137800960702     C*
137900980217     C* TIPO SERVIZIO NON RICHIESTO, ESCLUDE
138000980218IF  1C     PARTSP        IFEQ      'C'
138100980218     C     DLYTSP        ANDNE     'C'
138200980220     C     PARTSP        OREQ      'D'
138300980220     C     DLYTSP        ANDNE     'C'
138400980218     C     PARTSP        OREQ      'E'
138500980218     C     DLYTSP        ANDNE     'E'
138600050908     C                   MOVEL     *OFF          $RECOK
138700980217E   1C                   ENDIF
138800980217     C*
138900980217     C* CAP/LOCALITA' ISOLA NON RICHIESTO, ESCLUDE
139000980217IF  1C     PARISO        IFEQ      'N'
139100980217IF  2C     DLYISO        IFEQ      'I'
139200050908     C                   MOVEL     *OFF          $RECOK
139300980217E   2C                   ENDIF
139400980217E   1C                   ENDIF
139500980217     C*
139600980217     C* CAP/LOCALITA' DISAGIATO NON RICHIESTO, ESCLUDE
139700980217IF  1C     PARDIS        IFEQ      'N'
139800980217IF  2C     DLYISO        IFEQ      'D'
139900050908     C                   MOVEL     *OFF          $RECOK
140000980217E   2C                   ENDIF
140100980217E   1C                   ENDIF
140200070220      *
140300070220      * Se si tratta di CI/MD/OG/OT: non sono vere province!
140400070220     c                   if             DLYprv =  'CI'
140500080221     c                             or   DLYprv =  'VS'
140600070220     c                             or   DLYprv =  'OG'
140700070220     c                             or   DLYprv =  'OT'
140800070220     c                   eval      $RecOK = *off
140900070220     c                   leavesr
141000070220     c                   endif
141100980217     C*
141200960702     C                   ENDSR
141300050907      *------------------------------------------------------------------------*
141400050907      * REPDAT - REPERISCE DATI
141500050907      *------------------------------------------------------------------------*
141600980217     C     REPDAT        BEGSR
141700980217     C* TEMPO
141800980217     C                   Z-ADD     *ZEROS        WTEM
141900980217IF  1C     PARTEM        IFEQ      'I'
142000980217     C                   Z-ADD     DLYTRI        WTEM
142100980217X   1C                   ELSE
142200980217     C                   Z-ADD     DLYTRC        WTEM
142300980217E   1C                   ENDIF
142400980217     C* REGIONE
142500980217     C                   MOVEL     DLYCOR        WREG
142600980217     C*
142700980217     C                   ENDSR
142800050907      *------------------------------------------------------------------------*
142900050907      * NEWREG - OPERAZIONI PER NUOVA REGIONE
143000050907      *------------------------------------------------------------------------*
143100980217     C     NEWREG        BEGSR
143200980217     C*
143300980217     C* MEMORIZZA IL NUOVO CODICE IN UN DEPOSITO
143400980217     C                   MOVEL     DLYCOR        DEPREG
143500980217     C*
143600980217     C* REIMPOSTA L'INDICE PROVINCIE
143700050907     C                   Z-ADD     *ZEROS        XP
143800980217     C*
143900980217     C* INCREMENTA L'INDICE REGIONI
144000050907     C                   ADD       1             XR
144100980217     C*
144200980217     C* AZZERA LE VARIABILI DI LAVORO
144300050907     C     XR            OCCUR     DREG
144400980217     C                   CLEAR                   PRV
144500980217     C                   CLEAR                   TCX
144600980217     C                   CLEAR                   TCN
144700980217     C                   CLEAR                   TPX
144800980217     C                   CLEAR                   TPN
144900980217     C                   CLEAR                   TIX
145000980217     C                   CLEAR                   TIN
145100980217     C                   CLEAR                   TDX
145200980217     C                   CLEAR                   TDN
145300980219     C                   CLEAR                   COR
145400980217     C*
145500980217     C* MEMORIZZA IL CODICE REGIONE
145600050907     C                   MOVEL     DLYRAP        REG(XR)
145700980217     C*
145800980217     C                   ENDSR
145900050907      *------------------------------------------------------------------------*
146000050907      * NEWPRV - OPERAZIONI PER NUOVA PROVINCIA
146100050907      *------------------------------------------------------------------------*
146200980217     C     NEWPRV        BEGSR
146300980217     C*
146400980217     C* MEMORIZZA IL NUOVO CODICE IN UN DEPOSITO
146500980217     C                   MOVEL     DLYCOR        DEPCOR
146600980217     C*
146700980217     C* INCREMENTA L'INDICE PROVINCIE
146800050907     C                   ADD       1             XP
146900980217     C*
147000980217     C* MEMORIZZA IL CODICE PROVINCIA
147100050907     C                   MOVEL     DLYPRV        PRV(XP)
147200980219     C*
147300980219     C* MEMORIZZA IL CODICE ORDINAMENTO
147400050907     C                   MOVEL     DLYCOR        COR(XP)
147500980217     C*
147600980217     C                   ENDSR
147700050907      *------------------------------------------------------------------------*
147800050907      * FINSR - OPERAZIONI FINALI
147900050907      *------------------------------------------------------------------------*
148000980219     C     FINSR         BEGSR
148100050909      *---
148200050909      * FILE
148300050909      *---
148400980219     C* WORKFILE
148500090922     c***                if        partsp<>'E' or parold='S'
148600090922IF  1C***  PARTEM        IFEQ      'I'
148700090922     c                   if        %open(wadly01l)
148800050318     C                   CLOSE     WADLY01L
148900050318     C                   CLOSE     WADLY03L
149000090922     c                   endif
149100090922     c
149200090922X   1C***                ELSE
149300090922     c                   if        %open(wadly02l)
149400050318     C                   CLOSE     WADLY02L
149500050318     C                   CLOSE     WADLY04L
149600980216E   1C                   ENDIF
149700090922     c*****              else
149800090922     c                   if        %open(wadly06l)
149900090609     C                   CLOSE     WADLY06L
150000090609     c                   endif
150100980216     C* PRINTER FILE
150200011105     C                   CLOSE     LET017
150300050909      *---
150400050909      * PROGRAMMI
150500050909      *---
150600050907     C                   CLEAR                   FNLV55ds
150700980219     C                   MOVEL     'C'           D55TLA
150800980219     C                   CALL      'FNLV55R'
150900050907     C                   PARM                    FNLV55ds
151000980216     C*
151100960702     C                   ENDSR
151200050907      *------------------------------------------------------------------------*
151300050907      * OPNFIL - APERTURA FILES
151400050907      *------------------------------------------------------------------------*
151500960702     C     OPNFIL        BEGSR
151600050909      *---
151700050909      * WORKFILE
151800050909      *---
151900090922     c                   select
152000090922     c                   when      partsp='E' and parold=' '
152100090922     C                   OPEN      WADLY06L
152200090922     c                   when      partsp='H'
152300090922     c                   OPEN      WADLY06L
152400090922     c                   other
152500090922     c
152600980216IF  1C     PARTEM        IFEQ      'I'
152700050318     C                   OPEN      WADLY01L
152800050318     C                   OPEN      WADLY03L
152900980216X   1C                   ELSE
153000050318     C                   OPEN      WADLY02L
153100050318     C                   OPEN      WADLY04L
153200980216E   1C                   ENDIF
153300090922     c
153400090922     c                   ENDSL
153500050909      *---
153600050909      * PRINTER FILE
153700050909      *---
153800980216     C                   CALL      'QCMDEXC'
153900050907     C                   PARM      $Cmd(1)       COMMAN          100
154000050907     C                   PARM      100           LUNG             15 5
154100011105     C                   OPEN      LET017
154200980216     C*
154300980217     C                   ENDSR
154400050907      *------------------------------------------------------------------------*
154500050907      * CARTAB - CARICAMENTO TABELLE OCCORRENTI
154600050907      *------------------------------------------------------------------------*
154700980217     C     CARTAB        BEGSR
154800090605     c
154900050909      *---
155000050909      * REGIONI
155100050909      *---
155200050907     C                   Z-ADD     *ZEROS        XI
155300020919     C                   Z-ADD     1             KTBKUT
155400980217     C                   MOVEL     'CT'          KTBCOD
155500050907     C     KEYTAB        SETLL     TABEL
155600050907     C     KEYTAB        READE     TABEL                                  99
155700980217DO  1C     *IN99         DOWEQ     *OFF
155800980217IF  2C     TBLFLG        IFEQ      *BLANKS
155900980217     C                   MOVEL     TBLUNI        DSCT
156000980218     C                   MOVE      §CTCOR        A1                1
156100980218IF  3C     §CTNAR        IFEQ      *BLANKS
156200980218     C     A1            ANDEQ     '9'
156300050907     C                   ADD       1             XI
156400050907     C                   MOVEL     §CTRAP        RAP(XI)
156500050907     C                   MOVEL     §CTDES        RDES(XI)
156600980217E   3C                   ENDIF
156700980217E   2C                   ENDIF
156800050907     C     KEYTAB        READE     TABEL                                  99
156900980217E   1C                   ENDDO
157000050909      *---
157100050909      * PROVINCIE
157200050909      *---
157300050907     C                   Z-ADD     *ZEROS        XI
157400020919     C                   Z-ADD     1             KTBKUT
157500980217     C                   MOVEL     'PR'          KTBCOD
157600050907     C     KEYTAB        SETLL     TABEL
157700050907     C     KEYTAB        READE     TABEL                                  99
157800980217DO  1C     *IN99         DOWEQ     *OFF
157900980217IF  2C     TBLFLG        IFEQ      *BLANKS
158000980217     C                   MOVEL     TBLUNI        DSPR
158100050907     C                   ADD       1             XI
158200050907     C                   MOVEL     TBLKEY        PRO(XI)
158300050907     C                   MOVEL     §PRDES        PDES(XI)
158400980217E   2C                   ENDIF
158500050907     C     KEYTAB        READE     TABEL                                  99
158600980217E   1C                   ENDDO
158700050909      *---
158800050909      * NOTE
158900050909      *---
159000050907     C                   Z-ADD     *ZEROS        XI
159100090605     C                   Z-ADD     1             KTBKUT
159200980218     C                   MOVEL     '9C'          KTBCOD
159300050907     C     KEYTAB        SETLL     TABEL
159400050907     C     KEYTAB        READE     TABEL                                  99
159500980218DO  1C     *IN99         DOWEQ     *OFF
159600980218IF  2C     TBLFLG        IFEQ      *BLANKS
159700050907     C                   ADD       1             XI
159800050907     C                   MOVEL     TBLUNI        NOTE(XI)
159900980218E   2C                   ENDIF
160000050907     C     KEYTAB        READE     TABEL                                  99
160100980218E   1C                   ENDDO
160200980217     C*
160300960702     C                   ENDSR
160400050907      *-------------------------------------------------------------------------
160500050907      * REPERISCE I DATI UTENTE
160600050907      *-------------------------------------------------------------------------
160700020919     C     REPDATIUTE    BEGSR
160800050915      *
160900050915      * INIZIALIZZA VARIABILI DI WRK
161000020919     C                   CLEAR                   TIBS34DS
161100020919     C                   CLEAR                   AZUTEDS
161200020919     C                   CLEAR                   DDATIUTE
161300050915      *
161400020919     C     *DTAARA       DEFINE    §azute        azuteds
161500020919     C     *DTAARA       DEFINE    §datiute      ddatiute
161600020919     C                   IN(E)     *DTAARA
161700020919if  1C                   IF        %Error
161800020919     c                   EVAL      I34Tla = 'L'
161900020919     C                   CALL      'TIBS34R'
162000020919     C                   PARM                    Tibs34Ds
162100020919     C                   IN        *DTAARA
162200020919e   1C                   ENDIF
162300050915      *
162400020919     C                   ENDSR
162500050907      *------------------------------------------------------------------------*
162600050907      * *INZSR - IMPOSTAZIONI INIZIALI
162700050907      *------------------------------------------------------------------------*
162800960702     C     *INZSR        BEGSR
162900980216     C*---
163000960702     C* RICEVIMENTO PARAMETRI
163100980216     C*---
163200960702     C     *ENTRY        PLIST
163300960702     C                   PARM                    KPJBA
163400960702     C                   MOVEL     KPJBU         PARAM
163500980219     C*
163600960702     C* REPERISCE I DATI UTENTE
163700020919     C                   EXSR      REPDATIUTE
163800980219     C*
163900980219     C* CALCOLA LA DATA CORRENTE
164000980219     C                   TIME                    N14              14 0
164100980219     C                   MOVE      N14           N8                8 0
164200980219     C                   Z-ADD     N8            G08DAT
164300980219     C                   Z-ADD     *ZEROS        G08INV
164400980219     C                   MOVEL     '0'           G08ERR
164500980219     C                   CALL      'XSRDA8'
164600980219     C                   PARM                    WLBDA8
164700980219     C                   Z-ADD     G08INV        DATCOR            8 0
164800980219     C*
164900980219     C* RECUPERA IL TERMINAL PARTENZA DELLA FILIALE RICHIESTA
165000980219     C                   MOVEL     *BLANKS       D55TLA
165100980219     C                   MOVEL     'P'           D55TPT
165200980219     C                   MOVEL     PARLNP        D55LIN
165300980219     C                   Z-ADD     DATCOR        D55DRF
165400980219     C                   CALL      'FNLV55R'
165500050907     C                   PARM                    FNLV55ds
165600980219IF  1C     D55ERR        IFNE      *BLANKS
165700980219     C                   MOVEL     PARLNP        DEPLNP
165800980219E   1C                   ENDIF
165900050318     C                   Z-ADD     D55TFP        DEPLNP
166000980219     C*
166100980217     C* CARICAMENTO TABELLE OCCORRENTI
166200980217     C                   EXSR      CARTAB
166300960702     C*
166400960702     C* APERTURA FILES
166500960702     C                   EXSR      OPNFIL
166600060612
166700060612      * controllo in che lingua devo stampare
166800060612      * italiano
166900061117     ***c                   eval      *in11 = (parlin = 'I')
167000061117     c                   eval      *in11 = (PARlin = 'I'  or
167100061120     c                                      PARlin = '1'  or
167200061117     c                                      PARlin = *blanks)
167300060612      * inglese
167400060613     c                   eval      *in12 = (parlin = '2')
167500060612      * francese
167600060613     c                   eval      *in13 = (parlin = '3')
167700060612      * tedesco
167800060613     c                   eval      *in14 = (parlin = '4')
167900960702     C*
168000090605     c* Vedo se richiesto servizio "E" --> permessa solo stampa analitica
168100090608     c                   if        partsp='E' and parold=' '
168200090605     c                   eval      parssa='A'
168300090605     c                   endif
168400100702     c
168500100702     c* se non passato tipo servizio imposto "C"
168600100702     c                   if        partsp=' '
168700100702     c                   eval      partsp='C'
168800100702     c                   endif
168900090605     c
169000960702     C                   ENDSR
169100050907      *------------------------------------------------------------------------*
169200050907      * OUTPUT
169300050907      *------------------------------------------------------------------------*
169400100108     OLET017    E            INT              10
169500070312     O               11                          20 'Filiale di'
169600170717     o              N11                          15 'Depot'
169700170717     o**             13                          20 'Depot'
169800170717     o**             14                          20 'Depot'
169900980217     O          E            INT         1
170000980226     O                       ORGDES              30
170100980217     O          E            INT         1
170200980226     O                       ORGIND              30
170300980217     O          E            INT         1
170400980226     O                       ORGCPF              15
170500980226     O                       ORGLOC              36
170600980217     O          E            INT         1
170700980226     O                       ORGTEL              23
170800980217     O          E            INT         1
170900980226     O                       ORGFAX              23
171000980217     O          E            INT         2
171100090611     o                       stplocdta           45
171200980217     O*------------------------
171300100108     O          E            INT              20
171400980226     O                       STMTSP              45
171500170718     O               11n15                       85 'Tempi di resa indicativi'
171600090605     o               12n15                       88 'Approximate delivery times'
171700170718     o               13n15                       80 'Délais de livraison'
171800170718     o               13n15                       91 'indicatifs'
171900170718     o               14n15                       85 'Ungefähre Zustellfristen'
172000090605     O                 n15   wPARTEM            114
172100090610     O          E            INT         0  0
172200090610     O                       STMTSP              45
172300170717     O               11n15                       85 'Tempi di resa indicativi'
172400090610     o               12n15                       88 'Approximate delivery times'
172500170717     o               13n15                       80 'Délais de livraison'
172600170717     o               13n15                       91 'indicatifs'
172700170717     o               14n15                       85 'Ungefähre Zustellfristen'
172800980220     O*------------------------
172900980220     O          E            SAL               9
173000980220     O*------------------------
173100980220     O          E            TES         2
173200170717     O                       STMTMC              66
173300170717     O                       STMTMP              77
173400170717     O                       STMTMI              84
173500170717     O                       STMTMD             100
173600980225     O          E            TES         1  1
173700050908     O                                           80 '-------------------'
173800050908     O               41                         100 '--------------------'
173900090605     O*------------------------
174000090612     O          E            TES_E1      1
174100090612     o                                            1 ' '
174200090612     O          E            TES_E2      1
174300091204     O                       riga               105
174400090612     O*------------------------
174500090612     O          E            TES_E       1
174600091204     O                       riga               105
174700090610     O          E            TES_E       0  0
174800091204     O                       riga               105
174900090609     O*------------------------
175000090609     O          E            reg_E       2
175100090609     O                       STMREG              25
175200090611     O          E            reg_E       0  0
175300090611     O                       STMREG              25
175400090609     O          E            LOD_E       1
175500090611     o                       stpprv              12
175600090611     O                       rigaloc            116
175700980217     O*------------------------
175800980217     O          E            SIN         1
175900980226     O                       STMREG              25
176000050908     O                       STMPRV              60
176100050908     O                       STMTCN        Z     64
176200050908     O                       STMTDC              65
176300050908     O                       STMTCX        Z     68
176400170717     O                       STMTPN        Z     73
176500170717     O                       STMTDP              74
176600170717     O                       STMTPX        Z     77
176700170717     O                       STMTIN        Z     82
176800170717     O                       STMTDI              83
176900170717     O                       STMTIX        Z     86
177000050908     O                       STMTDN        Z     93
177100050908     O                       STMTDD              94
177200050908     O                       STMTDX        Z     97
177300980219     O*------------------------
177400980219     O          E            LOD         1
177500050908     O                       STMTEM        Z     23
177600050908     O                       STMLOC             115
177700050907     O*------------------------
177800050907     O          E            PrtTxt1     2
177900060612     O                       stmisole         +  10
178000980220     O*------------------------
178100980220     O          E            NOD         2
178200050913     O                       NOTE(1)          +  10
178300980225     O          E            NOD         1
178400050913     O                       NOTE(2)          +  10
178500980220     O          E            NOD         1
178600050913     O                       NOTE(3)          +  10
178700980218     O*------------------------
178800020919     O          E            NOT1        2
178900050913     O                       PARND1           +  10
179000020919     O          E            NOT1        1
179100050913     O                       PARND2           +  10
179200020919     O          E            NOT1        1
179300050913     O                       PARND3           +  10
179400980218     O*------------------------
179500980218     O          E            FIR         2
179600110505     O                                           90 'BRT S.p.A.    '
179700980218     O          E            FIR         1
179800070312     O               11                          73 'Filiale di'
179900070312     O               11      ORGDES              94
180000170717     o              n11                          73 'Depot'
180100170717     o              n11      ORGDES              94
180200170717     o**             13                          73 'Filiale de'
180300170717     o**             13      ORGDES              94
180400170717     o**             14                          73 'Filiale in'
180500170717     o**             14      ORGDES              94
180600050907      *------------------------------------------------------------------------*
180700050907**   $CMD -100-
180800050908OVRPRTF FILE(LET017) PAGESIZE(66 115 *ROWCOL) FORMTYPE(LET1) USRDTA('Delivery')
180900090619**   $rig -90-
181000091203Località servite entro le ore 12:00 del giorno lavorativo successivo al ritiro.
181100091203Località servite entro le ore 12:00 del 2° giorno lavorativo successivo al ritir
181200091203Le località non inserite in elenco sono servite entro la medesima giornata.
181300090619--------------------------------------------------------------------------------
181400091203Places where we deliver within 12 o'clock of the working day after the pick up.
181500091203Places where we deliver within 12 o'clock of the second working day after the pi
181600091203We deliver in the places not included in the list within the same day.
181700091203Localités ou nous livrons avant 12 heures du jour ouvrable successif à l'enlèvem
181800091203Localités ou nous livrons avant 12 heures du deuxième jour ouvrable successif à
181900091203Nous livrons dans les localités qui ne sont pas dans la liste dans le même jour.
182000100105Die Ortschaften werden innerhalb 12:00 Uhr am Tag nach der Abholung beliefert.
182100100105Die Ortschaften werden innerhalb 12:00 Uhr am zweiten Tag nach der Abholung beli
182200100105
182300091204Die nicht vorhandenen Orstschaft in der Liste werden am selben Tag beliefert.
182400090921
182500090921
182600090921
182700090921
182800090921
182900090921
183000091203Località servite entro le ore 10:30 del giorno lavorativo successivo al ritiro.
183100091203Località servite entro le ore 10:30 del 2° giorno lavorativo successivo al ritir
183200091006Le località non inserite in elenco non sono servite.
183300090921
183400091203Places where we deliver within 10:30 a.m. of the working day after the pick up.
183500091203Places where we deliver within 10:30 a.m. of the second working day after the pi
183600091218We do not have any service in the places which are not in the list.
183700091130Localités ou nous livrons avant 10:30 du jour ouvrable successif à l'enlèvement.
183800091130Localités ou nous livrons avant 10:30 du deuxième jour ouvrable successif à l'en
183900091218Les localités qui ne sont pas dans la liste ne sont pas servies.
184000100105Die Ortschaften werden innerhalb 10.30 Uhr am Tag nach der Abholung beliefert.
184100100105Die Ortschaften werden innerhalb 10:30 Uhr am zweiten Tag nach der Abholung beli
184200100105
184300091218Die Orte, die nicht in der Liste sind, werden nicht bedient.
184400090925
184500100105L'elenco aggiornato delle località e dei CAP serviti dal
184600120220è sempre consultabile sul sito Internet www.BRT.it nella sezione
184700091218servizi on-line, o in home page, cliccando su "Calcola tempi di consegna".
184800110505The updated list of the places and ZIP codes where BRT delivers within 12.
184900110505The updated list of the places and ZIP codes where BRT delivers within 10.
185000120220can always be consulted on the Internet site www.BRT.it in the section "se
185100091222(online services) or, in the home page, by clicking on "Calcola tempi di consegn
185200091218(calculate the delivery times).
185300100105La liste mise à jour des localitès et des codes postales où il y a le
185400120220est toujours à disposition sur le site Internet www.BRT.it dans la section
185500091218en ligne, ou, dans l'home page, en cliquant sur "Calcola tempi di consegna"
185600091218(calcule les délais de livraison).
185700100105Die vollstandig Liste der Ortschaften der belieferten von
185800120220ist immer auf web-site einsehbar auf web site www.BRT.it in der Abteilung
185900100105oder auf der home page, beim klichen auf Berechnen die Ziet von der Lieferung.
186000091218
