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
014400060630     d dectmc2         c                   const('Town ')
014500060804     d dectmc3         c                   const('Ville')
014600060804     d dectmc4         c                   const('Stadt')
014700980218     D DECTMP          C                   CONST('Provincia')
014800060630     d dectmp2         c                   const('Province ')
014900060804     d dectmp3         c                   const('Province ')
015000060804     d dectmp4         c                   const('Provinz  ')
015100980218     D DECTMI          C                   CONST('Isole')
015200980217     D DECTMD          C                   CONST('L.Disagiate')
015300060612
015400090619     d isole           c                   const('Isole minori: previsti ulteri-
015500090619     d                                     ori 2 gg lavorativi.')
015600090619     d isole2          c                   const('Smaller islands: 2 more worki-
015700090619     d                                     ng days allowed.')
015800090619     d isole3          c                   const('Îles mineures: 2 jours ouvrab-
015900090619     d                                     les supplémentaires sont prévus.')
016000090619     d isole4          c                   const('Kleinere Inseln: zuzüglich 2 -
016100090619     d                                     weitere Werktage.')
016200090921     d
016300050907      *-------------------
016400050907      * VARIABILI RIFERITE AL DATABASE
016500050907      *-------------------
016600050913     d wPARtem         s                   like(PARTEM) inz
016700050907     d KTBKUT          s                   like(TBLKUT) inz
016800050907     d KTBCOD          s                   like(TBLCOD) inz
016900090605     d KTBkey          s                   like(TBLkey) inz
017000050907     d KORFIL          s                   like(ORGFIL) inz
017100050907     d KWDLNP          s                   like(DLYLNP) inz
017200050907     d KWDCOR          s                   like(DLYCOR) inz
017300050907     d WTEM            s                   like(DLYTRC) inz
017400050907     d WREG            s                   like(DLYRAP) inz
017500050907     d WPRV            s                   like(DLYPRV) inz
017600050908     d WLOC            s                   like(DLYLOC) inz
017700050907     d WCOR            s                   like(DLYCOR) inz
017800050907     d WTCN            s                   like(DLYTRI) inz
017900050907     d WTPN            s                   like(DLYTRI) inz
018000050907     d WTIN            s                   like(DLYTRI) inz
018100050907     d WTDN            s                   like(DLYTRI) inz
018200050907     d DEPLNP          s                   like(DLYLNP) inz
018300050907     d DEPREG          s                   like(DLYRAP) inz
018400050907     d DEPCOR          s                   like(DLYCOR) inz
018500050907     d DEPTEM          s                   like(DLYTRC) inz
018600050907      *-------------------
018700050907      * VARIABILI DI CONTROLLO
018800050907      *-------------------
018900050907     d $FINE           s              1    inz(*off)
019000050907     d $FINL           s              1    inz(*off)
019100050908     d $RecOK          s              1    inz(*off)
019200050907     d $DECR           s              1    inz(*off)
019300050907     d $TUG            s              1    inz(*off)
019400050907      *-------------------
019500050907      * VARIABILI IN STAMPA
019600050907      *-------------------
019700050907     d STMTSP          s             35    inz
019800050907     d STMTMC          s              5    inz
019900050907     d STMTMP          s              9    inz
020000050907     d STMTMI          s              5    inz
020100050907     d STMTMD          s             11    inz
020200050909     d STMREG          s             15    inz
020300050909     d STMPRV          s             35    inz
020400050907     d STMTCN          s              3  0 inz
020500050907     d STMTDC          s              1    inz
020600050907     d STMTCX          s              3  0 inz
020700050907     d STMTPN          s              3  0 inz
020800050907     d STMTDP          s              1    inz
020900050907     d STMTPX          s              3  0 inz
021000050907     d STMTIN          s              3  0 inz
021100050907     d STMTDI          s              1    inz
021200050907     d STMTIX          s              3  0 inz
021300050907     d STMTDN          s              3  0 inz
021400050907     d STMTDD          s              1    inz
021500050907     d STMTDX          s              3  0 inz
021600050909     d STMTEM          s              3  0 inz
021700050909     d STMLOC          s             90    inz
021800060612     d stmisole        s             75    inz
021900091204     d riga            s             95    inz
022000090609     d TempoLoc        s              3  0 inz
022100090609     d TempoPRV        s              3  0 inz
022200090609     d Savreg          s              2    inz
022300090609     d Savprv          s              2    inz
022400090609     d Savloc          s                   like(dlyloc)
022500090609     d rigaloc         s            100    inz
022600090611     d wstpreg         s               n
022700090611     d wstpprv         s               n
022800090611     d stpprv          s                   like(dlyprv)
022900090611     d stplocdta       s             35
023000050907      *-------------------
023100050907      * INDICI DI SCHIERA
023200050907      *-------------------
023300050907     d XC              s              4  0 inz
023400050907     d XI              s              4  0 inz
023500050907     d XJ              s              4  0 inz
023600050907     d XP              s              4  0 inz
023700050907     d XR              s              4  0 inz
023800050908     d XL              s              2  0 inz
023900090612
024000090612     d prtdly          ds
024100090612     d  curline              367    368i 0
024200050908      *-------------------
024300050907      * CHIAVI DI LETTURA
024400050908      *-------------------
024500050907     C     KEYTAB        KLIST
024600050907     C                   KFLD                    KTBKUT
024700050907     C                   KFLD                    KTBCOD
024800090605     C     Ktab          KLIST
024900090605     C                   KFLD                    KTBKUT
025000090605     C                   KFLD                    KTBCOD
025100090605     C                   KFLD                    KTBkey
025200050907     C     KEYORG        KLIST
025300050907     C                   KFLD                    KORFIL
025400050907     C     KEYDLY        KLIST
025500050907     C                   KFLD                    KWDLNP
025600050907     C     KE2DLY        KLIST
025700050907     C                   KFLD                    KWDLNP
025800050907     C                   KFLD                    KWDCOR
025900090609     C     Kdly6         KLIST
026000090609     C                   KFLD                    kwdlnp
026100090609     C                   KFLD                    partsp
026200050907      *------------------------------------------------------------------------*
026300050907      * MAIN LINE
026400050907      *------------------------------------------------------------------------*
026500090921
026600090921     c                   select
026700090921    0c                   when      partsp='H'
026800090922     c                   seton                                        15
026900090921     c* Nuova stampa per tipo H-H 10 30 solo a modo nuovo
027000090921     c                   exsr      Stampa_E
027100090921
027200090921     c* Nuova stampa per tipo E-priority a modo nuovo
027300090921    0c                   when      partsp='E' and parold<>'S'
027400090922     c                   seton                                        15
027500090609     c                   exsr      Stampa_E
027600090921   x0c                   other
027700980217     C*
027800980217     C* MEMORIZZA I TEMPI PER PROVINCIA DI TUTTE LE REGIONI
027900980217     C                   EXSR      LETTEM
028000960702     C*
028100960702     C* STAMPA L'INTESTAZIONE
028200980218     C                   EXSR      STAINT
028300960702     C*
028400980217     C* PER TUTTE LE REGIONI MEMORIZZATE
028500050907DO  1C     1             DO        20            XR
028600050907IF  2C     REG(XR)       IFNE      *BLANKS
028700960702     C*
028800980217     C* STAMPA TEMPI REGIONE
028900980217     C                   EXSR      STAREG
029000960702E   2C                   ENDIF
029100960702E   1C                   ENDDO
029200960702     C*
029300980217     C* FINE PROSPETTO
029400050913if  1C                   if        PARiso = 'N'
029500060612     c   11              eval      stmisole = isole
029600060612     c   12              eval      stmisole = isole2
029700060612     c   13              eval      stmisole = isole3
029800060612     c   14              eval      stmisole = isole4
029900050907     C                   except    PrtTxt1
030000050913e   1c                   endif
030100980218IF  1C     PARTSP        IFEQ      'D'
030200980218     C                   EXCEPT    NOD
030300980218E   1C                   ENDIF
030400980226IF  1C     PARND1        IFNE      *BLANKS
030500980226     C     PARND2        ORNE      *BLANKS
030600980226     C     PARND3        ORNE      *BLANKS
030700020919     C                   EXCEPT    NOT1
030800980226E   1C                   ENDIF
030900090609     c
031000090921    0c                   endsl
031100090609     c*
031200980218     C                   EXCEPT    FIR
031300980219     C* OPERAZIONI FINALI
031400980219     C                   EXSR      FINSR
031500960702     C*
031600050907     C                   EVAL      *INLR = *ON
031700090609      *------------------------------------------------------------------------*
031800090921      * Stampa a modo nuovo tipo E-priority    e H - 10 30
031900090609      *------------------------------------------------------------------------*
032000090609     c     Stampa_E      BEGSR
032100090609     c
032200090609     C* STAMPA L'INTESTAZIONE
032300090609     c                   exsr      StaINT
032400090609     c
032500090609     c                   clear                   savreg
032600090623     C                   MOVEL     depLNP        KWDLNP
032700090609     c
032800090609     c* Stampa tempo 12 e 24
032900090609     c                   exsr      Lettura_E
033000090609     c* Stampa 36 e oltre
033100090609     c
033200091006      * Salto pagina
033300090612     c                   if        curline >= 60
033400090612     C                   EXCEPT    SAL
033500090612     c                   endif
033600090612     c                   if        *inof
033700090612     c                   exsr      States_E
033800090612     c                   endif
033900090612     C                   EXCEPT    TES_E1
034000091006      * stampo riga tratteggio
034100091130     c                   eval      riga = *all'-'
034200090612     C                   EXCEPT    TES_E2
034300091130     c                   clear                   riga
034400090921
034500090609     c* stampo testata diversa per servizio "E"
034600090921     c                   if        partsp='E'
034700090609     C   11              MOVEL     $rig(2)       riga
034800090619     C   12              MOVEL     $rig(6)       riga
034900090619     C   13              MOVEL     $rig(9)       riga
035000090619     C   14              MOVEL     $rig(12)      riga
035100090921     c                   endif
035200090921     c* stampo testata diversa per servizio "H"
035300090921     c                   if        partsp='H'
035400090921     C   11              MOVEL     $rig(22)      riga
035500090921     C   12              MOVEL     $rig(26)      riga
035600090921     C   13              MOVEL     $rig(29)      riga
035700090921     C   14              MOVEL     $rig(32)      riga
035800090921     c                   endif
035900090921     c
036000090609     c                   if        *inof
036100090609     c                   exsr      States_E
036200090612     c                   endif
036300090609     c                   except    TES_E
036400091218     c
036500091218     c* Per "H" in tedesco, stampo una riga in più
036600100105     c** 14              if        partsp='H'
036700100105     c**                 MOVEL     $rig(33)      riga
036800100105     c**                 except    TES_E
036900100105     c**                 endif
037000091204
037100091006     c
037200091218     c* Per "E" stampo un'altra riga
037300091006     C                   IF        PARTSP='E'
037400091204
037500100105     c***                if        *in14
037600100105     c***                MOVEL     $rig(13)      riga
037700100105     C***                EXCEPT    TES_E
037800100105     c***                endif
037900091204
038000091006     C   11              MOVEL     $rig(3)       riga
038100091006     C   12              MOVEL     $rig(7)       riga
038200091006     C   13              MOVEL     $rig(10)      riga
038300091204     C   14              MOVEL     $rig(14)      riga
038400090921     C
038500091204     C                   EXCEPT    TES_E
038600091204     c**                 if        *in14
038700091204     c**                 MOVEL     $rig(34)      riga
038800091204     C**                 EXCEPT    TES_E
038900091204     c**                 endif
039000091006     C                   ENDIF
039100091006
039200091006     c* Frase nuova per il sito internet
039300091006     c                   except    TES_E1
039400091006     c                   exsr      staxinternet
039500091006     c
039600091006     c
039700090612      * stampo riga tratteggio
039800091130     c                   eval      riga = *all'-'
039900090612     C                   EXCEPT    TES_E2
040000091130     c                   clear                   riga
040100090609     c
040200090921     c                   if        partsp='E'
040300090609     c                   eval      Tempoloc=036
040400090609     c                   eval      Tempoprv=000
040500090921     c                   endif
040600090921     c                   if        partsp='H'
040700090921     c                   eval      Tempoloc=034
040800090921     c                   eval      Tempoprv=034
040900090921     c                   endif
041000090609     c
041100090609     c                   exsr      Lettura_E
041200090609     c
041300100108     c                   if        curline <  60
041400090612     C                   EXCEPT    TES_E1
041500100108     c                   endif
041600091006      * Salto pagina
041700091006     c                   if        curline >= 62
041800091006     C                   EXCEPT    SAL
041900091006     c                   endif
042000091006     c                   if        *inof
042100091006     c                   exsr      States_E
042200091006     c                   endif
042300100108     c
042400100108     c                   if        curline <  61
042500091006     C                   EXCEPT    TES_E1
042600100108     c                   endif
042700091006     c
042800090921     c                   if        partsp='E'
042900090619     C   11              eval      riga = isole
043000090619     C   12              eval      riga = isole2
043100090619     C   13              eval      riga = isole3
043200090619     C   14              eval      riga = isole4
043300090925     c                   except    TES_E
043400091006     C                   ENDIF
043500091006
043600091006     c* Riga delle località non in elenco
043700091006     C                   IF        PARTSP='H'
043800091006     C   11              MOVEL     $rig(23)      riga
043900091006     C   12              MOVEL     $rig(27)      riga
044000091006     C   13              MOVEL     $rig(30)      riga
044100091218     C   14              MOVEL     $rig(34)      riga
044200091006     C
044300091006     C                   EXCEPT    TES_E
044400091006     C                   ENDIF
044500090609     c
044600090609     c                   ENDSR
044700090609      *------------------------------------------------------------------------*
044800090609      * Lettura lf wadly per nuova stampa tipo  E_priority
044900090609      *------------------------------------------------------------------------*
045000090609     c     LETTURA_E     BEGSR
045100090609     c* Lettura file
045200090609     c     kdly6         setll     wadly06l
045300090609     c     kdly6         reade     wadly06l
045400090609    1c                   dow       not %eof(wadly06l)
045500090609     c
045600090609     c*  Verifico se i tempi sono quelli previsti
045700090609    2c                   if        (partem='I' and (dlytri=tempoprv or
045800090609     c                             dlytri=tempoloc or
045900090609     c                             (tempoprv=00 and dlytri>tempoloc))) or
046000090609     c                             (partem='C' and (dlytrc=tempoprv or
046100090609     c                             dlytrc=tempoloc or
046200090609     c                             (tempoprv=00 and dlytrc>tempoloc)))
046300090609     c
046400090609     c* A cambio regione stampo la decodifica
046500090609    3c                   if        savreg<>dlyrap
046600090611      * stampo l'ultima località della regione precedente
046700090609    4c                   if        rigaloc<>*blanks
046800090609     c                   exsr      StaloD_E
046900090609    4c                   endif
047000090611      * a cambio regione imposto flag spento, la regione la stampo se ho dei dat
047100090611      * da stampare
047200090611     c                   eval      wstpreg = *off
047300090609     c
047400090611      * decodifico regione
047500090609     c                   eval      XI       =  1
047600090609     c                   eval      *in98    =  *off
047700090609     c     dlyrap        lookup    Rap(XI)                                98
047800090609if  4c                   if        *in98    =  *on
047900090609     c                   movel(p)  Rdes(XI)      STMreg
048000090609x   4c                   else
048100090609     c                   movel     *all'*'       STMreg
048200090609e   4c                   endif
048300090609     c* e stampo
048400090611     c**********         exsr      STATES_E
048500090611     C**********         EXCEPT    REG_E
048600090609     c
048700090609     c                   eval      savreg=dlyrap
048800090609     c                   clear                   savprv
048900090609    3c                   endif
049000090609     c
049100090609     c* Memorizzo la provincia da stampare e la località se
049200090609     c*  nel tempo previsto
049300090609    3c                   if        savprv<>dlyprv
049400090609     c* Stampo l'ultima località della provincia precedente
049500090609    4c                   if        rigaloc<>*blanks
049600090609     c                   exsr      StaloD_E
049700090609    4c                   endif
049800090611      * a cambio provincia imposto flag spento, la provincia la stampo se ho dei
049900090611      * da stampare
050000090611     c                   eval      wstpprv = *off
050100090609     c
050200090611     c********           eval      rigaloc=dlyprv+'    .'
050300090609
050400090609     c                   eval      savprv=dlyprv
050500090609    3c                   endif
050600090609     c
050700090609     c* Memorizzo la località se da stampare
050800090609    3c                   if        dlyloc<>*blanks  and dlyloc<> savloc
050900090609     c
051000090609    4c                   if        (partem='I' and (dlytri=tempoloc))  or
051100090609     c                             (partem='C' and (dlytrc=tempoloc))
051200090609     c*
051300090609      * Se lo spazio disponibile sulla riga in esame [LOC(XL)] è
051400090609      *  sufficiente per elencare anche la località in esame [WLOC]
051500090609      *  => aggiungo la località [WLOC] su questa riga [LOC(XL)]
051600090609if  5c                   IF          ( %len( rigaloc )
051700090611     c                             -   %len( %trimr(rigaloc) ) )
051800090611     c                             >   (%len( %trimr(dlyLoc) ) +1)
051900090609      *
052000090611     c                   if        rigaloc = *blanks
052100090611     c                   eval      rigaloc   =  %trim(dlyLoc)
052200090609x   6c                   else
052300090609     c                   eval      rigaloc   =  %trim(rigaloc )
052400090609     c                                       +  ','
052500090609     c                                       +  %trim(dlyLoc)
052600090609e   6c                   endif
052700090609   x5c                   else
052800090611     c* non ci sto più nel campo
052900090609     c* Stampo la riga di località
053000090609     c                   exsr      Stalod_E
053100090611     c                   eval      rigaloc   =  %trim(dlyLoc)
053200090609    5c                   endif
053300090609     c
053400090609     c                   eval      savloc=dlyloc
053500090609    4c                   endif
053600090609    3c                   endif
053700090609     c
053800090609    2c                   endif
053900090609     c
054000090609     c
054100090609     c     kdly6         reade     wadly06l
054200090609    1c                   enddo
054300090609     c
054400090609     c* Stampo l'ultima località della provincia precedente
054500090609    1c                   if        rigaloc<>*blanks
054600090609     c                   exsr      StaloD_E
054700090609    1c                   endif
054800090609     c
054900090609     c                   ENDSR
055000050907      *------------------------------------------------------------------------*
055100050907      * STAREG - STAMPA TEMPI REGIONE
055200050907      *------------------------------------------------------------------------*
055300050908     c     STAREG        BEGSR
055400050908      *
055500050908      * PREPARAZIONE DATI
055600050908     c                   exsr      StaReg_1
055700050908      *
055800050908     c                   exsr      StaReg_2
055900050908      *
056000050908     c                   ENDSR
056100050908      *------------------------------------------------------------------------*
056200050908      * STAREG_1 -?PREPARAZIONE?TEMPI REGIONE
056300050908      *------------------------------------------------------------------------*
056400050908     C     StaReg_1      BEGSR
056500050908      *
056600050908      * AZZERA LE VARIABILI DI LAVORO
056700050908     C                   clear                   XC
056800050908     C                   clear                   XP
056900050908     C                   clear                   XI
057000050908     C                   clear                   XJ
057100980217     C                   CLEAR                   SDES
057200980219     C                   CLEAR                   SCOR
057300980217     C                   CLEAR                   SINP
057400980217     C                   CLEAR                   STCX
057500980217     C                   CLEAR                   STCN
057600980217     C                   CLEAR                   STPX
057700980217     C                   CLEAR                   STPN
057800980217     C                   CLEAR                   STIX
057900980217     C                   CLEAR                   STIN
058000980217     C                   CLEAR                   STDX
058100980217     C                   CLEAR                   STDN
058200050912     C                   RESET                   SELE
058300050907     C                   MOVEL     *OFF          $DECR
058400050908      *
058500050908      * SI POSIZIONA SUI DATI DELLA REGIONE DA STAMPARE
058600050907     C     XR            OCCUR     DREG
058700050908      *
058800050908      * RAGGRUPPA LE PROVINCIE CHE HANNO TEMPI UGUALI
058900050908      * - Posizioni 1 (capoluogo) e 2 (1ª di altre province)
059000050908     C     1             DO        2             XC
059100050908     C                   MOVEL     TCX(XC)       STCX(XC)
059200050908     C                   MOVEL     TCN(XC)       STCN(XC)
059300050908     C                   MOVEL     TPX(XC)       STPX(XC)
059400050908     C                   MOVEL     TPN(XC)       STPN(XC)
059500050908     C                   MOVEL     TIX(XC)       STIX(XC)
059600050908     C                   MOVEL     TIN(XC)       STIN(XC)
059700050908     C                   MOVEL     TDX(XC)       STDX(XC)
059800050908     C                   MOVEL     TDN(XC)       STDN(XC)
059900050908     C                   MOVEL     PRV(XC)       SDES(XC)
060000050908     C                   MOVEL     COR(XC)       SCOR(XC)
060100050912     C                   MOVEL     $Ele(XC)      SEle(XC)
060200050908     C                   Z-ADD     1             SINP(XC)
060300050908     C                   ENDDO
060400050908      * TEMPI UGUALI
060500110117DO  1C     3             DO        15            XP
060600050907     C                   MOVEL     *OFF          $TUG
060700050907IF  2C     PRV(XP)       IFNE      *BLANKS
060800110117DO  3C     2             DO        15            XI
060900050907IF  4C     SDES(XI)      IFNE      *BLANKS
061000980218IF  5C     PARSSA        IFEQ      'S'
061100050907IF  6C     TCX(XP)       IFEQ      STCX(XI)
061200050907     C     TCN(XP)       ANDEQ     STCN(XI)
061300050907     C     TPX(XP)       ANDEQ     STPX(XI)
061400050907     C     TPN(XP)       ANDEQ     STPN(XI)
061500050907     C     TIX(XP)       ANDEQ     STIX(XI)
061600050907     C     TIN(XP)       ANDEQ     STIN(XI)
061700050907     C     TDX(XP)       ANDEQ     STDX(XI)
061800050907     C     TDN(XP)       ANDEQ     STDN(XI)
061900050907     C                   ADD       1             SINP(XI)
062000050907     C                   MOVEA(P)  SDES(XI)      SS2
062100050908     C                   EVAL      SS2(SINP(XI)) = PRV(XP)
062200980218     C                   MOVEA(P)  SS2           A24              24
062300050907     C                   MOVEL(P)  A24           SDES(XI)
062400051116     c                   movea(p)  Scor(XI)      ss5
062500051116     c                   eval      ss5(Sinp(XI)) =
062600051116     c                             %subst( Cor(XP) : 1 :
062700051116     c                             %len( ss5(Sinp(XI)) ) )
062800051116     c                   movea(p)  ss5           a60              60
062900051116     c                   movel(p)  a60           Scor(XI)
063000050907     C                   MOVEL     *ON           $TUG
063100980218E   6C                   ENDIF
063200980218E   5C                   ENDIF
063300980218E   4C                   ENDIF
063400980217E   3C                   ENDDO
063500050908      * TEMPI DIVERSI
063600050907IF  3C     $TUG          IFEQ      *OFF
063700980219     C     PARSSA        OREQ      'A'
063800050907     C                   ADD       1             XC
063900050907     C                   MOVEL     TCX(XP)       STCX(XC)
064000050907     C                   MOVEL     TCN(XP)       STCN(XC)
064100050907     C                   MOVEL     TPX(XP)       STPX(XC)
064200050907     C                   MOVEL     TPN(XP)       STPN(XC)
064300050907     C                   MOVEL     TIX(XP)       STIX(XC)
064400050907     C                   MOVEL     TIN(XP)       STIN(XC)
064500050907     C                   MOVEL     TDX(XP)       STDX(XC)
064600050907     C                   MOVEL     TDN(XP)       STDN(XC)
064700050907     C                   MOVEL     PRV(XP)       SDES(XC)
064800050907     C                   MOVEL     COR(XP)       SCOR(XC)
064900050912     C                   MOVEL     $Ele(XP)      SEle(XC)
065000050907     C                   Z-ADD     1             SINP(XC)
065100980218E   3C                   ENDIF
065200980217E   2C                   ENDIF
065300980217E   1C                   ENDDO
065400050908      *
065500050908     C                   ENDSR
065600050908      *------------------------------------------------------------------------*
065700050908      * STAREG_2 -?STAMPA?DEI TEMPI REGIONE
065800050908      *------------------------------------------------------------------------*
065900050908     c     StaReg_2      BEGSR
066000050908      *
066100050908      * STAMPA DEI TEMPI "GENERICI" PER TUTTE LE PROVINCE DELLA REGIONE
066200050908      * - Decodifica la regione
066300050908     c                   eval      XI       =  1
066400050908     c                   eval      *in98    =  *off
066500050908     c     Reg(XR)       lookup    Rap(XI)                                98
066600050908if  1c                   if        *in98    =  *on
066700050908     c                   movel(p)  Rdes(XI)      STMreg
066800050908x   1c                   else
066900050908     c                   movel     *all'*'       STMreg
067000050908e   1c                   endif
067100050909      * Raggruppa solo se non sono previste eccezioni per il
067200050909      * capoluogo della regione (1ª provincia)
067300050909     c                   eval      XI       =  1
067400050913if  1c                   if             Sele(XI) =  *off
067500050913     c                             or   PARssa   =  'S'
067600050908      * - Imposta la costante per tutte le province della regione
067700050908     c                   movel     *all'.'       STMprv
067800050908      * - Imposta i tempi di resa indicativi per la regione
067900050908     c                   exsr      StaReg_3
068000050913x   1c                   else
068100050913     c                   clear                   XI
068200050913e   1c                   endif
068300050908      *
068400050908      * STAMPA DEI TEMPI "SPECIFICI" PER LE ECCEZIONI
068500050909do  1c                   DOW       XI       <  %elem(Stcn)
068600050909     c                   eval      XI       =  XI + 1
068700050908      *
068800050908if  2c                   IF             Sdes(XI) <> *blanks
068900050912     c                             and((Stcn(XI) <> Stcn(1)
069000050913     c                               or Stcx(XI) <> Stcx(1)
069100050913     c                               or Stpn(XI) <> Stpn(1)
069200050913     c                               or Stpx(XI) <> Stpx(1)
069300050913     c                               or Stin(XI) <> Stin(1)
069400050913     c                               or Stix(XI) <> Stix(1)
069500050913     c                               or Stdn(XI) <> Stdn(1)
069600050913     c                               or Stdx(XI) <> Stdx(1)
069700050913     c                               or Sele(XI) =  *on
069800050913     c                               and  PARssa =  'A')
069900050913     c                               or XI       =  1
070000050913     c                               or Sele(1)  =  *on
070100050913     c                               and  PARssa =  'A')
070200050909      *
070300050909if  3c                   if        XI       >  1
070400050909     c                   clear                   STMreg
070500050909e   3c                   endif
070600050909      *
070700050909if  3c                   IF        Sinp(XI) >  1
070800050908      *
070900050908      * - "Concatena" le sigle delle province
071000050908     c                   movea     Sdes(XI)      Ss2
071100050908do  4c                   do        Sinp(XI)      XJ
071200050909if  5c                   if        XJ       =  1
071300050908     c                   movel(p)  Ss2(XJ)       STMprv
071400050908x   5c                   else
071500050909     c                   eval      STMprv   =  %trim(STMprv)
071600050909     c                                      +  '/'
071700050909     c                                      +  Ss2(XJ)
071800050908e   5c                   endif
071900050908e   4c                   enddo
072000050908      *
072100050908x   3c                   ELSE
072200050908      *
072300050908      * - Decodifica la singola provincia
072400050908     c                   movel     Sdes(XI)      Wprv
072500050909     c                   eval      XJ       =  1
072600050909     c                   eval      *in98    =  *off
072700050908     c     Wprv          lookup    Pro(XJ)                                98
072800050909if  4c                   if        *in98    =  *on
072900050908     c                   movel(P)  Pdes(XJ)      STMprv
073000050908x   4c                   else
073100050909     c                   eval      STMprv   =  *all'*'
073200050908e   4c                   endif
073300050908      *
073400050908e   3c                   ENDIF
073500050915      * - aggiunge "..." finali
073600050915     c                   eval      %subst( STMprv :
073700050915     c                                     %len(%trimr(STMprv)) + 2)
073800050915     c                             = *all'.'
073900050908      *
074000050908      * - Imposta i tempi di resa indicativi per le province
074100050908     c                   exsr      StaReg_3
074200050908      *
074300050908e   2c                   ENDIF
074400050908      *
074500050908e   1c                   ENDDO
074600050908      *
074700050908     c                   ENDSR
074800050908      *------------------------------------------------------------------------*
074900050908      * StaReg_3 - Imposta i tempi di resa indicativi per le province
075000050908      *            e stampa la riga
075100050908      *------------------------------------------------------------------------*
075200050908     c     StaReg_3      BEGSR
075300050908      *
075400050908      * Città
075500050908     c                   eval      STMtcn   =  Stcn(XI)
075600050908if  1c                   if        Stcx(XI) <> Stcn(XI)
075700050908     c                   eval      STMtdc   =  '/'
075800050908     c                   eval      STMtcx   =  Stcx(XI)
075900050908x   1c                   else
076000050908     c                   clear                   STMtdc
076100050908     c                   clear                   STMtcx
076200050908e   1c                   endif
076300050908      * Provincia
076400050908     c                   eval      STMtpn   =  Stpn(XI)
076500050908if  1c                   if        Stpx(XI) <> Stpn(XI)
076600050908     c                   eval      STMtdp   =  '/'
076700050908     c                   eval      STMtpx   =  Stpx(XI)
076800050908x   1c                   else
076900050908     c                   clear                   STMtdp
077000050908     c                   clear                   STMtpx
077100050908e   1c                   endif
077200050908      * Isola
077300050908     c                   eval      STMtin   =  Stin(XI)
077400050908if  1c                   if        Stix(XI) <> Stin(XI)
077500050908     c                   eval      STMtdi   =  '/'
077600050908     c                   eval      STMtix   =  STIX(XI)
077700050908x   1c                   else
077800050908     c                   clear                   STMTDI
077900050908     c                   clear                   STMTIX
078000050908e   1c                   endif
078100050908      * Zone Disagiate
078200050908     c                   eval      STMtdn   =  STDN(XI)
078300050908if  1c                   if        Stdx(XI) <> Stdn(XI)
078400050908     c                   eval      STMtdd   =  '/'
078500050908     c                   eval      STMtdx   =  STDX(XI)
078600050908x   1c                   else
078700050908     c                   clear                   STMTDD
078800050908     c                   clear                   STMTDX
078900050908e   1c                   endif
079000050908      * > Stampa
079100050908     c                   exsr      StaTES
079200050908     c                   except    SIN
079300050908      * - Stampa analitica
079400050908if  1c                   if        PARssa   =  'A'
079500050908     c                   movel     Scor(XI)      Wcor
079600050908     c                   z-add     Stcn(XI)      Wtcn
079700050908     c                   z-add     Stpn(XI)      Wtpn
079800050908     c                   z-add     Stin(XI)      Wtin
079900050908     c                   z-add     Stdn(XI)      Wtdn
080000050908     c                   exsr      MemLOC
080100090604     c                   endif
080200050908      *
080300050908     c                   ENDSR
080400050907      *------------------------------------------------------------------------*
080500050907      * STAINT - STAMPA L'INTESTAZIONE
080600050907      *------------------------------------------------------------------------*
080700980218     C     STAINT        BEGSR
080800050909      *---
080900050909      * INTESTAZIONE
081000050909      *---
081100980219     C                   MOVEL     PARLNP        KORFIL
081200050907     C     KEYORG        CHAIN     AZORG                              98
081300090605     C* AGGANCIO  tipo servizio, in lingua se richiesto
081400090605     c   11              movel     1             ktbkut
081500090605     c  n11              movel     parlin        ktbkut
081600090605     c                   eval      ktbcod='5E'
081700090605     c                   clear                   ktbkey
081800090605     c                   eval      ktbkey=partsp
081900090605     c     ktab          chain     tabel00f
082000090605     c                   movel     tbluni        ds5e
082100090605     c                   eval      stmtsp=§5eddly
082200980225     C*
082300050913if  1C                   if        PARtem  = 'I'
082400050913     C                   eval      wPARtem = PARtem
082500050913e   1C                   endif
082600090611      * imposto localita + data
082700090611     c                   clear                   stplocdta
082800090611     c                   eval      stplocdta = orgloc
082900090611     c                   eval      stplocdta = %trim(stplocdta) + ','
083000090611     c   11              eval      stplocdta = %trim(stplocdta) + ' li'
083100090611     c   13              eval      stplocdta = %trim(stplocdta) + ' le'
083200090611     c                   eval      stplocdta = %trim(stplocdta) + ' ' +
083300090611     c                             %subst(%editc(%dec(udate):'X'):1:2)+
083400090611     c                             '/' +
083500090611     c                             %subst(%editc(%dec(udate):'X'):3:2)+
083600090611     c                             '/' +
083700090611     c                             %subst(%editc(%dec(udate):'X'):5:2)
083800090605     c
083900980218     C                   EXCEPT    INT
084000050909      *---
084100090922      * TESTATA   per servizio "E" nuovo
084200050909      *---
084300090922     c                   select
084400090922     c                   when      partsp='E' and parold=' '
084500090612     C                   EXCEPT    TES_E1
084600090921
084700090605     C   11              MOVEL     $rig(1)       riga
084800090619     C   12              MOVEL     $rig(5)       riga
084900090619     C   13              MOVEL     $rig(8)       riga
085000090619     C   14              MOVEL     $rig(11)      riga
085100090605     C                   EXCEPT    TES_E
085200090612     c
085300091006     C   11              MOVEL     $rig(3)       riga
085400091006     C   12              MOVEL     $rig(7)       riga
085500091006     C   13              MOVEL     $rig(10)      riga
085600091204     C   14              MOVEL     $rig(14)      riga
085700091006     C                   EXCEPT    TES_E
085800091204     c**                 if        *in14
085900091204     c**                 MOVEL     $rig(14)      riga
086000091204     C**                 EXCEPT    TES_E
086100091204     c**                 endif
086200091006     c
086300091006     c* Nuova frase per le località
086400091006     C                   EXCEPT    TES_E1
086500091006     c                   exsr      StaxInternet
086600091006
086700090609     c
086800090609     c* Imposto  le ore per il primo giro 12 e 24
086900090609     c                   eval      tempoloc=012
087000090609     c                   eval      tempoprv=024
087100090921     c*
087200090922      *---
087300090922      * TESTATA   per servizio "H"
087400090922      *---
087500090922     c                   when      partsp='H'
087600090922     C                   EXCEPT    TES_E1
087700090921
087800090921     C   11              MOVEL     $rig(21)      riga
087900090921     C   12              MOVEL     $rig(25)      riga
088000090921     C   13              MOVEL     $rig(28)      riga
088100090921     C   14              MOVEL     $rig(31)      riga
088200090921     C                   EXCEPT    TES_E
088300091006     C                   EXCEPT    TES_E1
088400090921     c
088500091006     C***11              MOVEL     $rig(23)      riga
088600091006     C***12              MOVEL     $rig(27)      riga
088700091006     C***13              MOVEL     $rig(30)      riga
088800091006     C***14              MOVEL     $rig(33)      riga
088900091006     C***                EXCEPT    TES_E
089000091006     c***                if        *in14
089100091006     c***                MOVEL     $rig(34)      riga
089200091006     C***                EXCEPT    TES_E
089300091006     c***                endif
089400090925     c* Nuova frase per le località
089500091006     c                   exsr      StaxInternet
089600090925
089700090921     c
089800090921     c* Imposto  le ore per il primo giro 10
089900090921     c                   eval      tempoloc=010
090000090921     c                   eval      tempoprv=010
090100090922     c                   other
090200090921     c
090300090922     c* altri tipi servizio
090400090922     C   11              MOVEL     DECTMC        STMTMC
090500090922     C   11              MOVEL     DECTMP        STMTMP
090600090922     c   12              movel     dectmc2       stmtmc
090700090922     c   12              movel     dectmp2       stmtmp
090800090922     c   13              movel     dectmc3       stmtmc
090900090922     c   13              movel     dectmp3       stmtmp
091000090922     c   14              movel     dectmc4       stmtmc
091100090922     c   14              movel     dectmp4       stmtmp
091200090922IF  1C     PARISO        IFEQ      'S'
091300090922     C                   MOVEL     DECTMI        STMTMI
091400090922E   1C                   ENDIF
091500090922IF  1C     PARDIS        IFEQ      'S'
091600090922     C                   MOVEL     DECTMD        STMTMD
091700090922E   1C                   ENDIF
091800090922     C                   if            PARiso = 'S'
091900090922     C                             or  PARdis = 'S'
092000090922     C                   eval      *in41 = *on
092100090922     C                   else
092200090922     C                   eval      *in41 = *off
092300090922     C                   endif
092400090922     C                   EXCEPT    TES
092500090922     c                   ENDSL
092600090922     c*
092700050909      *
092800980218     C                   ENDSR
092900091006      *------------------------------------------------------------------------*
093000091006      * Stampa frase per il sito internet solo per "E" e "H"
093100091006      *------------------------------------------------------------------------*
093200091006     c     StaxInternet  BEGSR
093300091006     c* Italiano
093400091006     c                   if        *in11
093500091218     c                   eval      riga=%trim($rig(36))
093600100105     c                   eval      riga=%trim(riga)+' '+stmtsp
093700091006     C                   EXCEPT    TES_E
093800091218     c                   eval      riga=$rig(37)
093900091203     C                   EXCEPT    TES_E
094000091218     c                   eval      riga=$rig(38)
094100091006     C                   EXCEPT    TES_E
094200091006     c                   endif
094300091006     c* Inglese
094400091006     c                   if        *in12
094500100105     C                   IF        PARTSP='E'
094600100105     c                   eval      riga=%trim($rig(39))
094700100105     c                   else
094800100105     c                   eval      riga=%trim($rig(40))
094900100105     c                   endif
095000091006     C                   EXCEPT    TES_E
095100091203     c                   eval      riga=$rig(41)
095200091203     C                   EXCEPT    TES_E
095300091203     c                   eval      riga=$rig(42)
095400091006     C                   EXCEPT    TES_E
095500091218     c                   eval      riga=$rig(43)
095600091218     C                   EXCEPT    TES_E
095700091006     c                   endif
095800091006     c* Francese
095900091006     c                   if        *in13
096000091130     c                   eval      riga=%trim($rig(44))
096100100105     c                   eval      riga=%trim(riga)+' '+stmtsp
096200091006     C                   EXCEPT    TES_E
096300091130     c                   eval      riga=$rig(45)
096400091130     C                   EXCEPT    TES_E
096500091130     c                   eval      riga=$rig(46)
096600091006     C                   EXCEPT    TES_E
096700091218     c                   eval      riga=$rig(47)
096800091218     C                   EXCEPT    TES_E
096900091006     c                   endif
097000091006     c* Tedesco
097100091006     c                   if        *in14
097200091218     c                   eval      riga=%trim($rig(48))
097300100105     c                   eval      riga=%trim(riga)+' '+stmtsp
097400091006     C                   EXCEPT    TES_E
097500091218     c                   eval      riga=$rig(49)
097600091006     C                   EXCEPT    TES_E
097700091218     c                   eval      riga=$rig(50)
097800091204     C                   EXCEPT    TES_E
097900091006     c                   endif
098000091006     c                   ENDSR
098100050907      *------------------------------------------------------------------------*
098200050907      * STATES - STAMPA TESTATA
098300050907      *------------------------------------------------------------------------*
098400980218     C     STATES        BEGSR
098500980218     C*
098600980218     C* SE FINE PAGINA, STAMPA TESTATA
098700980218IF  1C     *INOF         IFEQ      *ON
098800980220     C                   EXCEPT    SAL
098900980220     C                   EXCEPT    TES
099000980218     C                   SETOFF                                       OF
099100980218E   1C                   ENDIF
099200980217     C*
099300980217     C                   ENDSR
099400090609      *------------------------------------------------------------------------*
099500090609     c* Testata per E-prioriry
099600090609      *------------------------------------------------------------------------*
099700090609     c     STATES_E      BEGSR
099800090609    4c                   if        *inof
099900090609     C                   EXCEPT    SAL
100000090610     C**!!!              EXCEPT    tes_E
100100090609     C                   SETOFF                                       OF
100200090609E   4C                   ENDIF
100300090609     c                   ENDSR
100400090609      *------------------------------------------------------------------------*
100500090609      * Stampa località per E-priority
100600090609      *------------------------------------------------------------------------*
100700090609     C     STALOD_E      BEGSR
100800090610
100900090609     c                   exsr      states_E
101000090611
101100090611      * se non ho ancora stampato la regione lo faccio ora
101200090611     c                   if        not wstpreg
101300090611     c                   except    reg_e
101400090611     c                   eval      wstpreg = *on
101500090611     c                   endif
101600090611
101700090611      * se non ho ancora stampato la provincia lo faccio ora
101800090611     c                   if        not wstpprv
101900090611     c                   eval      stpprv = savprv
102000090611     c                   eval      wstpprv = *on
102100090611     c                   else
102200090611     c                   clear                   stpprv
102300090611     c                   endif
102400090609     c
102500090609     c                   except    lod_e
102600090609     c                   clear                   rigaloc
102700090609     c
102800090609     c                   ENDSR
102900050907      *------------------------------------------------------------------------*
103000050907      * MEMLOC - MEMORIZZA LE LOCALITA' DELLA PROVINCIA
103100050907      *------------------------------------------------------------------------*
103200980219     C     MEMLOC        BEGSR
103300980219     C*
103400980219     C* REIMPOSTA LE VARIABILI DI LAVORO
103500050907     C                   MOVEL     *OFF          $FINL
103600980219     C                   CLEAR                   LOC
103700090604     C                   CLEAR                   wloc
103800980219     C*
103900980219     C* POSIZIONAMENTO E PRIMA LETTURA -LOCALITA' PROVINCIA-
104000980219     C                   EXSR      SETLOC
104100980219     C*
104200980219     C* CICLO FINO A FINE FILE
104300050907DO  1C     $FINL         DOWEQ     *OFF
104400980219     C*
104500980219     C* OPERAZIONI PER NUOVO TEMPO
104600980219     C                   EXSR      NEWTEM
104700980219     C*
104800980219     C* CICLO FINO A ROTTURA TEMPO
104900050907DO  2C     $FINL         DOWEQ     *OFF
105000980219     C     WTEM          ANDEQ     DEPTEM
105100980219     C*
105200980219     C* COMPATTA LA LOCALITA'
105300050908     C                   MOVEL     DLYLOC        WLOC
105400980219     C                   EXSR      COMLOC
105500980219     C*
105600980219     C* LETTURA RECORD SUCCESSIVO -LOCALITA' PROVINCIA-
105700980219     C                   EXSR      LETLOC
105800980219E   2C                   ENDDO
105900980219     C*
106000980219     C* STAMPA LE LOCALITA' DELLA PROVINCIA
106100980219     C                   EXSR      STALOC
106200980219E   1C                   ENDDO
106300980218     C*
106400980218     C                   ENDSR
106500050907      *------------------------------------------------------------------------*
106600050907      * SETLOC - POSIZIONAMENTO E PRIMA LETTURA -LOCALITA' PROVINCIA-
106700050907      *------------------------------------------------------------------------*
106800980219     C     SETLOC        BEGSR
106900980219     C*
107000980219     C* POSIZIONAMENTO IN BASE AL TIPO TEMPO RICHIESTO
107100980219     C                   MOVEL     DEPLNP        KWDLNP
107200051115     C                   MOVEL     WCOR          KWDCOR
107300980219IF  1C     PARTEM        IFEQ      'I'
107400050907     C     KE2DLY        SETLL     WADLY3                             99
107500980219X   1C                   ELSE
107600050907     C     KE2DLY        SETLL     WADLY4                             99
107700980219E   1C                   ENDIF
107800980219IF  1C     *IN99         IFEQ      *ON
107900050907     C                   MOVEL     *ON           $FINL
108000980219X   1C                   ELSE
108100980219     C                   EXSR      LETLOC
108200980219E   1C                   ENDIF
108300980219     C*
108400980219     C                   ENDSR
108500050907      *------------------------------------------------------------------------*
108600050907      * LETLOC - LETTURA PROSSIMO RECORD -LOCALITA' PROVINCIA-
108700050907      *------------------------------------------------------------------------*
108800980219     C     LETLOC        BEGSR
108900980219     C*
109000980219     C* LEGGE FINO A:
109100980219     C*     - FINE FILE
109200980219     C*     - TROVATO RECORD VALIDO
109300050908     C                   MOVEL     *OFF          $RECOK
109400050907DO  1C     $FINL         DOWEQ     *OFF
109500050908     C     $RECOK        ANDEQ     *OFF
109600980219IF  2C     PARTEM        IFEQ      'I'
109700050907     C     KE2DLY        READE     WADLY3                                 99
109800980219X   2C                   ELSE
109900050907     C     KE2DLY        READE     WADLY4                                 99
110000980219E   2C                   ENDIF
110100980219IF  2C     *IN99         IFEQ      *ON
110200050907     C                   MOVEL     *ON           $FINL
110300980219X   2C                   ELSE
110400980219     C                   EXSR      CHKREC
110500051115IF  2C     $RECOK        IFEQ      *ON
110600980219     C                   EXSR      REPDAT
110700980219     C                   EXSR      CHKLOC
110800980219E   3C                   ENDIF
110900980219E   2C                   ENDIF
111000980219E   1C                   ENDDO
111100980219     C*
111200980219     C                   ENDSR
111300050907      *------------------------------------------------------------------------*
111400050907      * CHKLOC - CONTROLLA RECORD -LOCALITA' PROVINCIA-
111500050907      *------------------------------------------------------------------------*
111600980219     C     CHKLOC        BEGSR
111700051114      *
111800050908     C                   MOVEL     *OFF          $RECOK
111900051114      *
112000051114if  1c                   IF            (DLYiso = 'C'
112100051114     c                             and  wTEM   > wTCN
112200051114     c                             OR   DLYiso = 'P'
112300051114     c                             and  wTEM   > wTPN
112400051114     c                             OR   PARiso = 'S'
112500051114     c                             and  DLYiso = 'I'
112600051114     c                             and  wTEM   > wTIN
112700051114     c                             OR   PARdis = 'S'
112800051114     c                             and  DLYiso = 'D'
112900051114     c                             and  wTEM   > wTDN)
113000051114     c                             and  DLYdlv = 'S'
113100051114     c                   eval      $RecOK = *on
113200051114e   1c                   endif
113300051114      *
113400980219     C                   ENDSR
113500050907      *------------------------------------------------------------------------*
113600050907      * NEWTEM - OPERAZIONI PER NUOVO TEMPO
113700050907      *------------------------------------------------------------------------*
113800050908     c     NEWTEM        BEGSR
113900050908      *
114000050908      * MEMORIZZA IL NUOVO CODICE IN UN DEPOSITO
114100050908     c                   movel     wTem          DEPtem
114200050908      *
114300050908     c                   ENDSR
114400050907      *------------------------------------------------------------------------*
114500050907      * COMLOC - COMPATTA LE LOCALITA'
114600050907      *------------------------------------------------------------------------*
114700050908     c     COMLOC        BEGSR
114800050908      *
114900050909if  1c                   if        wLoc      = *blanks
115000050908     c                   leavesr
115100050908e   1c                   endif
115200050908      *
115300050909     c                   eval      XL        = 1
115400050909do  1c                   DOW       XL       <= %elem(Loc)
115500050908      *
115600050908      * Se lo spazio disponibile sulla riga in esame [LOC(XL)] è
115700050908      *  sufficiente per elencare anche la località in esame [WLOC]
115800050908      *  => aggiungo la località [WLOC] su questa riga [LOC(XL)]
115900050909if  2c                   IF          ( %len( Loc(XL) )
116000050908     c                             -   %len( %trimr(Loc(XL)) ) )
116100050909     c                             >   %len( %trimr(wLoc) )
116200050908      *
116300050909if  3c                   if        Loc(XL)   =  *blanks
116400050908     c                   movel(p)  wLoc          Loc(XL)
116500050908x   3c                   else
116600050909     c                   eval      Loc(XL)   =  %trim(Loc(XL))
116700050909     c                                       +  ','
116800050909     c                                       +  %trim(wLoc)
116900050908e   3c                   endif
117000050908     c                   leave
117100050908      *
117200050908      * Altrimenti vado sulla riga successiva ([LOC(XL+1)]
117300050908x   2c                   else
117400050908     c                   add       1             XL
117500050908e   2c                   endif
117600050908      *
117700050908e   1c                   ENDDO
117800050908      *
117900050908     c                   ENDSR
118000050907      *------------------------------------------------------------------------*
118100050907      * STALOC - STAMPA LE LOCALITA' DELLA PROVINCIA DEL TEMPO
118200050907      *------------------------------------------------------------------------*
118300980219     C     STALOC        BEGSR
118400050909      *
118500980219IF  1C     LOC(1)        IFNE      *BLANKS
118600050907DO  2C     1             DO        30            XL
118700050907IF  3C     LOC(XL)       IFNE      *BLANKS
118800050907IF  4C     XL            IFEQ      1
118900050907     C                   Z-ADD     DEPTEM        STMTEM
119000980219X   4C                   ELSE
119100980219     C                   Z-ADD     *ZEROS        STMTEM
119200980219E   4C                   ENDIF
119300050909if  4c                   if            XL        <  %elem(Loc)
119400050909     c                             and Loc(XL+1) <> *blanks
119500050909     c                   eval      STMloc = %trimr(Loc(XL))
119600050909     c                                    + ','
119700050909x   4c                   else
119800050909     C                   MOVEL(p)  LOC(XL)       STMLOC
119900050909e   4c                   endif
120000980219     C                   EXSR      STATES
120100980219     C                   EXCEPT    LOD
120200980219E   3C                   ENDIF
120300980219E   2C                   ENDDO
120400980219E   1C                   ENDIF
120500050909      *
120600980219     C                   CLEAR                   LOC
120700050909      *
120800980219     C                   ENDSR
120900050907      *------------------------------------------------------------------------*
121000050907      * LETTEM - MEMORIZZA I TEMPI PER PROVINCIA DI TUTTE LE REGIONI
121100050907      *------------------------------------------------------------------------*
121200980217     C     LETTEM        BEGSR
121300980217     C*
121400980217     C* IMPOSTA LE VARIABILI DI LAVORO
121500050907     C                   Z-ADD     *ZEROS        XR
121600050907     C                   Z-ADD     *ZEROS        XP
121700960702     C*
121800050318     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA -FILE-
121900960702     C                   EXSR      SETFIL
122000960702     C*
122100960702     C* CICLO FINO A FINE FILE
122200050907DO  1C     $FINE         DOWEQ     *OFF
122300960702     C*
122400960702     C* OPERAZIONI PER NUOVA REGIONE
122500980217     C                   EXSR      NEWREG
122600960702     C*
122700980217     C* CICLO FINO A ROTTURA REGIONE
122800050907DO  2C     $FINE         DOWEQ     *OFF
122900980217     C     WREG          ANDEQ     DEPREG
123000980217     C*
123100980217     C* OPERAZIONI PER NUOVA PROVINCIA
123200980217     C                   EXSR      NEWPRV
123300980217     C*
123400980217     C* CICLO FINO A ROTTURA PROVINCIA
123500050907DO  3C     $FINE         DOWEQ     *OFF
123600980217     C     WREG          ANDEQ     DEPREG
123700980217     C     DLYCOR        ANDEQ     DEPCOR
123800960702     C*
123900980217     C* MEMORIZZA I TEMPI MINIMI E MASSIMI
124000980217     C                   EXSR      MEMTEM
124100980219     C*
124200050318     C* LETTURA RECORD SUCCESSIVO -FILE-
124300960702     C                   EXSR      LETFIL
124400980217E   3C                   ENDDO
124500980217E   2C                   ENDDO
124600960702E   1C                   ENDDO
124700960702     C*
124800960702     C                   ENDSR
124900050907      *------------------------------------------------------------------------*
125000050907      * MEMTEM - MEMORIZZA I TEMPI MINIMI E MASSIMI
125100050907      *------------------------------------------------------------------------*
125200980217     C     MEMTEM        BEGSR
125300980217     C* CITTA'
125400980217IF  1C     DLYISO        IFEQ      'C'
125500050907IF  2C     WTEM          IFLT      TCN(XP)
125600050907     C     TCN(XP)       OREQ      *ZEROS
125700050907     C                   Z-ADD     WTEM          TCN(XP)
125800980217E   2C                   ENDIF
125900050907IF  2C     WTEM          IFGT      TCX(XP)
126000050907     C     TCX(XP)       OREQ      *ZEROS
126100050907     C                   Z-ADD     WTEM          TCX(XP)
126200980217E   2C                   ENDIF
126300980217E   1C                   ENDIF
126400980217     C* PROVINCIA
126500980217IF  1C     DLYISO        IFEQ      'P'
126600050907IF  2C     WTEM          IFLT      TPN(XP)
126700050907     C     TPN(XP)       OREQ      *ZEROS
126800050907     C                   Z-ADD     WTEM          TPN(XP)
126900980217E   2C                   ENDIF
127000050907IF  2C     WTEM          IFGT      TPX(XP)
127100050907     C     TPX(XP)       OREQ      *ZEROS
127200050907     C                   Z-ADD     WTEM          TPX(XP)
127300980217E   2C                   ENDIF
127400980217E   1C                   ENDIF
127500980217     C* ISOLA
127600980217IF  1C     PARISO        IFEQ      'S'
127700980217IF  2C     DLYISO        IFEQ      'I'
127800050907IF  3C     WTEM          IFLT      TIN(XP)
127900050907     C     TIN(XP)       OREQ      *ZEROS
128000050907     C                   Z-ADD     WTEM          TIN(XP)
128100980217E   3C                   ENDIF
128200050907IF  3C     WTEM          IFGT      TIX(XP)
128300050907     C     TIX(XP)       OREQ      *ZEROS
128400050907     C                   Z-ADD     WTEM          TIX(XP)
128500980217E   3C                   ENDIF
128600980217E   2C                   ENDIF
128700980217E   1C                   ENDIF
128800980217     C* DISAGIATE
128900980217IF  1C     PARDIS        IFEQ      'S'
129000980219IF  2C     DLYISO        IFEQ      'D'
129100050907IF  3C     WTEM          IFLT      TDN(XP)
129200050907     C     TDN(XP)       OREQ      *ZEROS
129300050907     C                   Z-ADD     WTEM          TDN(XP)
129400980217E   3C                   ENDIF
129500050907IF  3C     WTEM          IFGT      TDX(XP)
129600050907     C     TDX(XP)       OREQ      *ZEROS
129700050907     C                   Z-ADD     WTEM          TDX(XP)
129800980217E   3C                   ENDIF
129900980217E   2C                   ENDIF
130000980217E   1C                   ENDIF
130100050912     C*
130200050912     C* Memorizzo se località elencabili per provincia
130300050913     C*  (se richiesta la stampa analitica)
130400050913if  1c                   if        PARssa   = 'A'
130500050913     c                   z-add     TCN(xp)       Wtcn
130600050913     c                   z-add     TPN(xp)       Wtpn
130700050913     c                   z-add     TIN(xp)       Wtin
130800050913     c                   z-add     TDN(xp)       Wtdn
130900050913     C                   exsr      ChkLoc
131000050913     C                   eval      $Ele(xp) = $RecOK
131100050913e   1C                   endif
131200980217     C*
131300980217     C                   ENDSR
131400050907      *------------------------------------------------------------------------*
131500050907      * SETFIL - POSIZIONAMENTO E PRIMA LETTURA DEL FILE -FILE-
131600050907      *------------------------------------------------------------------------*
131700980217     C     SETFIL        BEGSR
131800980217     C*
131900980217     C* POSIZIONAMENTO IN BASE AL TIPO TEMPO RICHIESTO
132000980219     C                   MOVEL     DEPLNP        KWDLNP
132100980217IF  1C     PARTEM        IFEQ      'I'
132200050907     C     KEYDLY        SETLL     WADLY000                           99
132300980217X   1C                   ELSE
132400050907     C     KEYDLY        SETLL     WADLY2                             99
132500960702E   1C                   ENDIF
132600960702IF  1C     *IN99         IFEQ      *ON
132700050907     C                   MOVEL     *ON           $FINE
132800960702X   1C                   ELSE
132900960702     C                   EXSR      LETFIL
133000960702E   1C                   ENDIF
133100960702     C*
133200960702     C                   ENDSR
133300050907      *------------------------------------------------------------------------*
133400050907      * LETFIL - LETTURA PROSSIMO RECORD -FILE-
133500050907      *------------------------------------------------------------------------*
133600960702     C     LETFIL        BEGSR
133700980217     C*
133800980217     C* LEGGE FINO A:
133900980217     C*     - FINE FILE
134000980217     C*     - TROVATO RECORD VALIDO
134100050908     C                   MOVEL     *OFF          $RECOK
134200050907DO  1C     $FINE         DOWEQ     *OFF
134300050908     C     $RECOK        ANDEQ     *OFF
134400980217IF  2C     PARTEM        IFEQ      'I'
134500050907     C     KEYDLY        READE     WADLY000                               99
134600980217X   2C                   ELSE
134700050907     C     KEYDLY        READE     WADLY2                                 99
134800980217E   2C                   ENDIF
134900980217IF  2C     *IN99         IFEQ      *ON
135000050907     C                   MOVEL     *ON           $FINE
135100980217X   2C                   ELSE
135200980217     C                   EXSR      CHKREC
135300051115IF  3C     $RECOK        IFEQ      *ON
135400980217     C                   EXSR      REPDAT
135500980217E   3C                   ENDIF
135600980217E   2C                   ENDIF
135700980217E   1C                   ENDDO
135800960702     C*
135900960702     C                   ENDSR
136000050907      *------------------------------------------------------------------------*
136100050907      * CHKREC - CONTROLLA VALIDITA' RECORD -FILE-
136200050907      *------------------------------------------------------------------------*
136300960702     C     CHKREC        BEGSR
136400960702     C*
136500050908     C                   MOVEL     *ON           $RECOK
136600960702     C*
136700980217     C* TIPO SERVIZIO NON RICHIESTO, ESCLUDE
136800980218IF  1C     PARTSP        IFEQ      'C'
136900980218     C     DLYTSP        ANDNE     'C'
137000980220     C     PARTSP        OREQ      'D'
137100980220     C     DLYTSP        ANDNE     'C'
137200980218     C     PARTSP        OREQ      'E'
137300980218     C     DLYTSP        ANDNE     'E'
137400050908     C                   MOVEL     *OFF          $RECOK
137500980217E   1C                   ENDIF
137600980217     C*
137700980217     C* CAP/LOCALITA' ISOLA NON RICHIESTO, ESCLUDE
137800980217IF  1C     PARISO        IFEQ      'N'
137900980217IF  2C     DLYISO        IFEQ      'I'
138000050908     C                   MOVEL     *OFF          $RECOK
138100980217E   2C                   ENDIF
138200980217E   1C                   ENDIF
138300980217     C*
138400980217     C* CAP/LOCALITA' DISAGIATO NON RICHIESTO, ESCLUDE
138500980217IF  1C     PARDIS        IFEQ      'N'
138600980217IF  2C     DLYISO        IFEQ      'D'
138700050908     C                   MOVEL     *OFF          $RECOK
138800980217E   2C                   ENDIF
138900980217E   1C                   ENDIF
139000070220      *
139100070220      * Se si tratta di CI/MD/OG/OT: non sono vere province!
139200070220     c                   if             DLYprv =  'CI'
139300080221     c                             or   DLYprv =  'VS'
139400070220     c                             or   DLYprv =  'OG'
139500070220     c                             or   DLYprv =  'OT'
139600070220     c                   eval      $RecOK = *off
139700070220     c                   leavesr
139800070220     c                   endif
139900980217     C*
140000960702     C                   ENDSR
140100050907      *------------------------------------------------------------------------*
140200050907      * REPDAT - REPERISCE DATI
140300050907      *------------------------------------------------------------------------*
140400980217     C     REPDAT        BEGSR
140500980217     C* TEMPO
140600980217     C                   Z-ADD     *ZEROS        WTEM
140700980217IF  1C     PARTEM        IFEQ      'I'
140800980217     C                   Z-ADD     DLYTRI        WTEM
140900980217X   1C                   ELSE
141000980217     C                   Z-ADD     DLYTRC        WTEM
141100980217E   1C                   ENDIF
141200980217     C* REGIONE
141300980217     C                   MOVEL     DLYCOR        WREG
141400980217     C*
141500980217     C                   ENDSR
141600050907      *------------------------------------------------------------------------*
141700050907      * NEWREG - OPERAZIONI PER NUOVA REGIONE
141800050907      *------------------------------------------------------------------------*
141900980217     C     NEWREG        BEGSR
142000980217     C*
142100980217     C* MEMORIZZA IL NUOVO CODICE IN UN DEPOSITO
142200980217     C                   MOVEL     DLYCOR        DEPREG
142300980217     C*
142400980217     C* REIMPOSTA L'INDICE PROVINCIE
142500050907     C                   Z-ADD     *ZEROS        XP
142600980217     C*
142700980217     C* INCREMENTA L'INDICE REGIONI
142800050907     C                   ADD       1             XR
142900980217     C*
143000980217     C* AZZERA LE VARIABILI DI LAVORO
143100050907     C     XR            OCCUR     DREG
143200980217     C                   CLEAR                   PRV
143300980217     C                   CLEAR                   TCX
143400980217     C                   CLEAR                   TCN
143500980217     C                   CLEAR                   TPX
143600980217     C                   CLEAR                   TPN
143700980217     C                   CLEAR                   TIX
143800980217     C                   CLEAR                   TIN
143900980217     C                   CLEAR                   TDX
144000980217     C                   CLEAR                   TDN
144100980219     C                   CLEAR                   COR
144200980217     C*
144300980217     C* MEMORIZZA IL CODICE REGIONE
144400050907     C                   MOVEL     DLYRAP        REG(XR)
144500980217     C*
144600980217     C                   ENDSR
144700050907      *------------------------------------------------------------------------*
144800050907      * NEWPRV - OPERAZIONI PER NUOVA PROVINCIA
144900050907      *------------------------------------------------------------------------*
145000980217     C     NEWPRV        BEGSR
145100980217     C*
145200980217     C* MEMORIZZA IL NUOVO CODICE IN UN DEPOSITO
145300980217     C                   MOVEL     DLYCOR        DEPCOR
145400980217     C*
145500980217     C* INCREMENTA L'INDICE PROVINCIE
145600050907     C                   ADD       1             XP
145700980217     C*
145800980217     C* MEMORIZZA IL CODICE PROVINCIA
145900050907     C                   MOVEL     DLYPRV        PRV(XP)
146000980219     C*
146100980219     C* MEMORIZZA IL CODICE ORDINAMENTO
146200050907     C                   MOVEL     DLYCOR        COR(XP)
146300980217     C*
146400980217     C                   ENDSR
146500050907      *------------------------------------------------------------------------*
146600050907      * FINSR - OPERAZIONI FINALI
146700050907      *------------------------------------------------------------------------*
146800980219     C     FINSR         BEGSR
146900050909      *---
147000050909      * FILE
147100050909      *---
147200980219     C* WORKFILE
147300090922     c***                if        partsp<>'E' or parold='S'
147400090922IF  1C***  PARTEM        IFEQ      'I'
147500090922     c                   if        %open(wadly01l)
147600050318     C                   CLOSE     WADLY01L
147700050318     C                   CLOSE     WADLY03L
147800090922     c                   endif
147900090922     c
148000090922X   1C***                ELSE
148100090922     c                   if        %open(wadly02l)
148200050318     C                   CLOSE     WADLY02L
148300050318     C                   CLOSE     WADLY04L
148400980216E   1C                   ENDIF
148500090922     c*****              else
148600090922     c                   if        %open(wadly06l)
148700090609     C                   CLOSE     WADLY06L
148800090609     c                   endif
148900980216     C* PRINTER FILE
149000011105     C                   CLOSE     LET017
149100050909      *---
149200050909      * PROGRAMMI
149300050909      *---
149400050907     C                   CLEAR                   FNLV55ds
149500980219     C                   MOVEL     'C'           D55TLA
149600980219     C                   CALL      'FNLV55R'
149700050907     C                   PARM                    FNLV55ds
149800980216     C*
149900960702     C                   ENDSR
150000050907      *------------------------------------------------------------------------*
150100050907      * OPNFIL - APERTURA FILES
150200050907      *------------------------------------------------------------------------*
150300960702     C     OPNFIL        BEGSR
150400050909      *---
150500050909      * WORKFILE
150600050909      *---
150700090922     c                   select
150800090922     c                   when      partsp='E' and parold=' '
150900090922     C                   OPEN      WADLY06L
151000090922     c                   when      partsp='H'
151100090922     c                   OPEN      WADLY06L
151200090922     c                   other
151300090922     c
151400980216IF  1C     PARTEM        IFEQ      'I'
151500050318     C                   OPEN      WADLY01L
151600050318     C                   OPEN      WADLY03L
151700980216X   1C                   ELSE
151800050318     C                   OPEN      WADLY02L
151900050318     C                   OPEN      WADLY04L
152000980216E   1C                   ENDIF
152100090922     c
152200090922     c                   ENDSL
152300050909      *---
152400050909      * PRINTER FILE
152500050909      *---
152600980216     C                   CALL      'QCMDEXC'
152700050907     C                   PARM      $Cmd(1)       COMMAN          100
152800050907     C                   PARM      100           LUNG             15 5
152900011105     C                   OPEN      LET017
153000980216     C*
153100980217     C                   ENDSR
153200050907      *------------------------------------------------------------------------*
153300050907      * CARTAB - CARICAMENTO TABELLE OCCORRENTI
153400050907      *------------------------------------------------------------------------*
153500980217     C     CARTAB        BEGSR
153600090605     c
153700050909      *---
153800050909      * REGIONI
153900050909      *---
154000050907     C                   Z-ADD     *ZEROS        XI
154100020919     C                   Z-ADD     1             KTBKUT
154200980217     C                   MOVEL     'CT'          KTBCOD
154300050907     C     KEYTAB        SETLL     TABEL
154400050907     C     KEYTAB        READE     TABEL                                  99
154500980217DO  1C     *IN99         DOWEQ     *OFF
154600980217IF  2C     TBLFLG        IFEQ      *BLANKS
154700980217     C                   MOVEL     TBLUNI        DSCT
154800980218     C                   MOVE      §CTCOR        A1                1
154900980218IF  3C     §CTNAR        IFEQ      *BLANKS
155000980218     C     A1            ANDEQ     '9'
155100050907     C                   ADD       1             XI
155200050907     C                   MOVEL     §CTRAP        RAP(XI)
155300050907     C                   MOVEL     §CTDES        RDES(XI)
155400980217E   3C                   ENDIF
155500980217E   2C                   ENDIF
155600050907     C     KEYTAB        READE     TABEL                                  99
155700980217E   1C                   ENDDO
155800050909      *---
155900050909      * PROVINCIE
156000050909      *---
156100050907     C                   Z-ADD     *ZEROS        XI
156200020919     C                   Z-ADD     1             KTBKUT
156300980217     C                   MOVEL     'PR'          KTBCOD
156400050907     C     KEYTAB        SETLL     TABEL
156500050907     C     KEYTAB        READE     TABEL                                  99
156600980217DO  1C     *IN99         DOWEQ     *OFF
156700980217IF  2C     TBLFLG        IFEQ      *BLANKS
156800980217     C                   MOVEL     TBLUNI        DSPR
156900050907     C                   ADD       1             XI
157000050907     C                   MOVEL     TBLKEY        PRO(XI)
157100050907     C                   MOVEL     §PRDES        PDES(XI)
157200980217E   2C                   ENDIF
157300050907     C     KEYTAB        READE     TABEL                                  99
157400980217E   1C                   ENDDO
157500050909      *---
157600050909      * NOTE
157700050909      *---
157800050907     C                   Z-ADD     *ZEROS        XI
157900090605     C                   Z-ADD     1             KTBKUT
158000980218     C                   MOVEL     '9C'          KTBCOD
158100050907     C     KEYTAB        SETLL     TABEL
158200050907     C     KEYTAB        READE     TABEL                                  99
158300980218DO  1C     *IN99         DOWEQ     *OFF
158400980218IF  2C     TBLFLG        IFEQ      *BLANKS
158500050907     C                   ADD       1             XI
158600050907     C                   MOVEL     TBLUNI        NOTE(XI)
158700980218E   2C                   ENDIF
158800050907     C     KEYTAB        READE     TABEL                                  99
158900980218E   1C                   ENDDO
159000980217     C*
159100960702     C                   ENDSR
159200050907      *-------------------------------------------------------------------------
159300050907      * REPERISCE I DATI UTENTE
159400050907      *-------------------------------------------------------------------------
159500020919     C     REPDATIUTE    BEGSR
159600050915      *
159700050915      * INIZIALIZZA VARIABILI DI WRK
159800020919     C                   CLEAR                   TIBS34DS
159900020919     C                   CLEAR                   AZUTEDS
160000020919     C                   CLEAR                   DDATIUTE
160100050915      *
160200020919     C     *DTAARA       DEFINE    §azute        azuteds
160300020919     C     *DTAARA       DEFINE    §datiute      ddatiute
160400020919     C                   IN(E)     *DTAARA
160500020919if  1C                   IF        %Error
160600020919     c                   EVAL      I34Tla = 'L'
160700020919     C                   CALL      'TIBS34R'
160800020919     C                   PARM                    Tibs34Ds
160900020919     C                   IN        *DTAARA
161000020919e   1C                   ENDIF
161100050915      *
161200020919     C                   ENDSR
161300050907      *------------------------------------------------------------------------*
161400050907      * *INZSR - IMPOSTAZIONI INIZIALI
161500050907      *------------------------------------------------------------------------*
161600960702     C     *INZSR        BEGSR
161700980216     C*---
161800960702     C* RICEVIMENTO PARAMETRI
161900980216     C*---
162000960702     C     *ENTRY        PLIST
162100960702     C                   PARM                    KPJBA
162200960702     C                   MOVEL     KPJBU         PARAM
162300980219     C*
162400960702     C* REPERISCE I DATI UTENTE
162500020919     C                   EXSR      REPDATIUTE
162600980219     C*
162700980219     C* CALCOLA LA DATA CORRENTE
162800980219     C                   TIME                    N14              14 0
162900980219     C                   MOVE      N14           N8                8 0
163000980219     C                   Z-ADD     N8            G08DAT
163100980219     C                   Z-ADD     *ZEROS        G08INV
163200980219     C                   MOVEL     '0'           G08ERR
163300980219     C                   CALL      'XSRDA8'
163400980219     C                   PARM                    WLBDA8
163500980219     C                   Z-ADD     G08INV        DATCOR            8 0
163600980219     C*
163700980219     C* RECUPERA IL TERMINAL PARTENZA DELLA FILIALE RICHIESTA
163800980219     C                   MOVEL     *BLANKS       D55TLA
163900980219     C                   MOVEL     'P'           D55TPT
164000980219     C                   MOVEL     PARLNP        D55LIN
164100980219     C                   Z-ADD     DATCOR        D55DRF
164200980219     C                   CALL      'FNLV55R'
164300050907     C                   PARM                    FNLV55ds
164400980219IF  1C     D55ERR        IFNE      *BLANKS
164500980219     C                   MOVEL     PARLNP        DEPLNP
164600980219E   1C                   ENDIF
164700050318     C                   Z-ADD     D55TFP        DEPLNP
164800980219     C*
164900980217     C* CARICAMENTO TABELLE OCCORRENTI
165000980217     C                   EXSR      CARTAB
165100960702     C*
165200960702     C* APERTURA FILES
165300960702     C                   EXSR      OPNFIL
165400060612
165500060612      * controllo in che lingua devo stampare
165600060612      * italiano
165700061117     ***c                   eval      *in11 = (parlin = 'I')
165800061117     c                   eval      *in11 = (PARlin = 'I'  or
165900061120     c                                      PARlin = '1'  or
166000061117     c                                      PARlin = *blanks)
166100060612      * inglese
166200060613     c                   eval      *in12 = (parlin = '2')
166300060612      * francese
166400060613     c                   eval      *in13 = (parlin = '3')
166500060612      * tedesco
166600060613     c                   eval      *in14 = (parlin = '4')
166700960702     C*
166800090605     c* Vedo se richiesto servizio "E" --> permessa solo stampa analitica
166900090608     c                   if        partsp='E' and parold=' '
167000090605     c                   eval      parssa='A'
167100090605     c                   endif
167200100702     c
167300100702     c* se non passato tipo servizio imposto "C"
167400100702     c                   if        partsp=' '
167500100702     c                   eval      partsp='C'
167600100702     c                   endif
167700090605     c
167800960702     C                   ENDSR
167900050907      *------------------------------------------------------------------------*
168000050907      * OUTPUT
168100050907      *------------------------------------------------------------------------*
168200100108     OLET017    E            INT              10
168300070312     O               11                          20 'Filiale di'
168400070314     o               12                          16 'Branch'
168500070314     o               13                          20 'Filiale de'
168600070314     o               14                          20 'Filiale in'
168700980217     O          E            INT         1
168800980226     O                       ORGDES              30
168900980217     O          E            INT         1
169000980226     O                       ORGIND              30
169100980217     O          E            INT         1
169200980226     O                       ORGCPF              15
169300980226     O                       ORGLOC              36
169400980217     O          E            INT         1
169500980226     O                       ORGTEL              23
169600980217     O          E            INT         1
169700980226     O                       ORGFAX              23
169800980217     O          E            INT         2
169900090611     O**********             ORGLOC              30
170000090611     O**********                                 31 ','
170100090611     O**********     11                          34 'li'
170200090611     O**********     13                          34 'le'
170300090611     O**********     11      UDATE               43 '  /  /  '
170400090611     O**********     13      UDATE               43 '  /  /  '
170500090611     o**********    n11n13   UDATE               40 '  /  /  '
170600090611     o                       stplocdta           45
170700980217     O*------------------------
170800100108     O          E            INT              20
170900980226     O                       STMTSP              45
171000090605     O               11n15                       86 'Tempi di resa indicativi'
171100090605     o               12n15                       88 'Approximate delivery times'
171200090605     o               13n15                       81 'Délais de livraison'
171300090605     o               13n15                       92 'indicatifs'
171400090605     o               14n15                       86 'Ungefähre Zustellfristen'
171500090605     O                 n15   wPARTEM            114
171600090610     O          E            INT         0  0
171700090610     O                       STMTSP              45
171800090610     O               11n15                       86 'Tempi di resa indicativi'
171900090610     o               12n15                       88 'Approximate delivery times'
172000090610     o               13n15                       81 'Délais de livraison'
172100090610     o               13n15                       92 'indicatifs'
172200090610     o               14n15                       86 'Ungefähre Zustellfristen'
172300980220     O*------------------------
172400980220     O          E            SAL               9
172500980220     O*------------------------
172600980220     O          E            TES         2
172700050908     O                       STMTMC              67
172800050908     O                       STMTMP              79
172900050908     O                       STMTMI              86
173000050908     O                       STMTMD              99
173100980225     O          E            TES         1  1
173200050908     O                                           80 '-------------------'
173300050908     O               41                         100 '--------------------'
173400090605     O*------------------------
173500090612     O          E            TES_E1      1
173600090612     o                                            1 ' '
173700090612     O          E            TES_E2      1
173800091204     O                       riga               105
173900090612     O*------------------------
174000090612     O          E            TES_E       1
174100091204     O                       riga               105
174200090610     O          E            TES_E       0  0
174300091204     O                       riga               105
174400090609     O*------------------------
174500090609     O          E            reg_E       2
174600090609     O                       STMREG              25
174700090611     O          E            reg_E       0  0
174800090611     O                       STMREG              25
174900090609     O          E            LOD_E       1
175000090611     o                       stpprv              12
175100090611     O                       rigaloc            116
175200980217     O*------------------------
175300980217     O          E            SIN         1
175400980226     O                       STMREG              25
175500050908     O                       STMPRV              60
175600050908     O                       STMTCN        Z     64
175700050908     O                       STMTDC              65
175800050908     O                       STMTCX        Z     68
175900050908     O                       STMTPN        Z     74
176000050908     O                       STMTDP              75
176100050908     O                       STMTPX        Z     78
176200050908     O                       STMTIN        Z     83
176300050908     O                       STMTDI              84
176400050908     O                       STMTIX        Z     87
176500050908     O                       STMTDN        Z     93
176600050908     O                       STMTDD              94
176700050908     O                       STMTDX        Z     97
176800980219     O*------------------------
176900980219     O          E            LOD         1
177000050908     O                       STMTEM        Z     23
177100050908     O                       STMLOC             115
177200050907     O*------------------------
177300050907     O          E            PrtTxt1     2
177400060612     O                       stmisole         +  10
177500980220     O*------------------------
177600980220     O          E            NOD         2
177700050913     O                       NOTE(1)          +  10
177800980225     O          E            NOD         1
177900050913     O                       NOTE(2)          +  10
178000980220     O          E            NOD         1
178100050913     O                       NOTE(3)          +  10
178200980218     O*------------------------
178300020919     O          E            NOT1        2
178400050913     O                       PARND1           +  10
178500020919     O          E            NOT1        1
178600050913     O                       PARND2           +  10
178700020919     O          E            NOT1        1
178800050913     O                       PARND3           +  10
178900980218     O*------------------------
179000980218     O          E            FIR         2
179100050915     O                                           90 'BARTOLINI S.p.A.    '
179200980218     O          E            FIR         1
179300070312     O               11                          73 'Filiale di'
179400070312     O               11      ORGDES              94
179500070314     o               12                          73 'Branch'
179600070314     o               12      ORGDES              94
179700070314     o               13                          73 'Filiale de'
179800070314     o               13      ORGDES              94
179900070314     o               14                          73 'Filiale in'
180000070314     o               14      ORGDES              94
180100050907      *------------------------------------------------------------------------*
180200050907**   $CMD -100-
180300050908OVRPRTF FILE(LET017) PAGESIZE(66 115 *ROWCOL) FORMTYPE(LET1) USRDTA('Delivery')
180400090619**   $rig -90-
180500091203Località servite entro le ore 12:00 del giorno lavorativo successivo al ritiro.
180600091203Località servite entro le ore 12:00 del 2° giorno lavorativo successivo al ritir
180700091203Le località non inserite in elenco sono servite entro la medesima giornata.
180800090619--------------------------------------------------------------------------------
180900091203Places where we deliver within 12 o'clock of the working day after the pick up.
181000091203Places where we deliver within 12 o'clock of the second working day after the pi
181100091203We deliver in the places not included in the list within the same day.
181200091203Localités ou nous livrons avant 12 heures du jour ouvrable successif à l'enlèvem
181300091203Localités ou nous livrons avant 12 heures du deuxième jour ouvrable successif à
181400091203Nous livrons dans les localités qui ne sont pas dans la liste dans le même jour.
181500100105Die Ortschaften werden innerhalb 12:00 Uhr am Tag nach der Abholung beliefert.
181600100105Die Ortschaften werden innerhalb 12:00 Uhr am zweiten Tag nach der Abholung beli
181700100105
181800091204Die nicht vorhandenen Orstschaft in der Liste werden am selben Tag beliefert.
181900090921
182000090921
182100090921
182200090921
182300090921
182400090921
182500091203Località servite entro le ore 10:30 del giorno lavorativo successivo al ritiro.
182600091203Località servite entro le ore 10:30 del 2° giorno lavorativo successivo al ritir
182700091006Le località non inserite in elenco non sono servite.
182800090921
182900091203Places where we deliver within 10:30 a.m. of the working day after the pick up.
183000091203Places where we deliver within 10:30 a.m. of the second working day after the pi
183100091218We do not have any service in the places which are not in the list.
183200091130Localités ou nous livrons avant 10:30 du jour ouvrable successif à l'enlèvement.
183300091130Localités ou nous livrons avant 10:30 du deuxième jour ouvrable successif à l'en
183400091218Les localités qui ne sont pas dans la liste ne sont pas servies.
183500100105Die Ortschaften werden innerhalb 10.30 Uhr am Tag nach der Abholung beliefert.
183600100105Die Ortschaften werden innerhalb 10:30 Uhr am zweiten Tag nach der Abholung beli
183700100105
183800091218Die Orte, die nicht in der Liste sind, werden nicht bedient.
183900090925
184000100105L'elenco aggiornato delle località e dei CAP serviti dal
184100091218è sempre consultabile sul sito Internet www.bartolini.it nella sezione
184200091218servizi on-line, o in home page, cliccando su "Calcola tempi di consegna".
184300100105The updated list of the places and ZIP codes where BARTOLINI delivers within 12.
184400100105The updated list of the places and ZIP codes where BARTOLINI delivers within 10.
184500091218can always be consulted on the Internet site www.bartolini.it in the section "se
184600091222(online services) or, in the home page, by clicking on "Calcola tempi di consegn
184700091218(calculate the delivery times).
184800100105La liste mise à jour des localitès et des codes postales où il y a le
184900091218est toujours à disposition sur le site Internet www.bartolini.it dans la section
185000091218en ligne, ou, dans l'home page, en cliquant sur "Calcola tempi di consegna"
185100091218(calcule les délais de livraison).
185200100105Die vollstandig Liste der Ortschaften der belieferten von
185300100105ist immer auf web-site einsehbar auf web site www.bartolini.it in der Abteilung
185400100105oder auf der home page, beim klichen auf Berechnen die Ziet von der Lieferung.
185500091218
