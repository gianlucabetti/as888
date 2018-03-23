000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200060828      * TRTC83R1*----------------------------------------------------*
000300020225      *    MANIFEST SPEDIZIONI PER ESTERO
000400020225      *--------------------------------------------------------------*
000500090402     Ftitas30c  iF   E           K DISK    extfile(WTAs30) usropn
000600090402     Ftita430c  UF a E           K DISK    COMMIT
000700090402     f                                     extfile(WTA430) usropn
000800090402     FFNARB55L  UF   E           K DISK    COMMIT
000900020225      *---------
001000970124     FFNARB01L  UF   E           K DISK    COMMIT
001100020226     f                                     RENAME(FNARB000:FNARB001)
001200020225      *---------
001300060213     Ffiar401L  IF   E           K DISK
001400020225      *---------
001500061127     Ffiar501L  IF   E           K DISK
001600061127      *---------
001700051114     FFIAR901L  IF   E           K DISK
001800020225      *---------
001900970123     FFNART55L  UF   E           K DISK    COMMIT
002000020226     f                                     RENAME(FNART000:FNART055)
002100020225      *---------
002200970123     FFNART27L  UF   E           K DISK    COMMIT
002300020225      *---------
002400020225     FFNART01L  IF   E           K DISK    RENAME(FNART000:FNART001)
002500020225      *---------
002600070123     FFNBRV01L  UF   E           K DISK    commit
002700020225      *---------
002800070123     FFNBRV07L  IF   E           K DISK    RENAME(FNBRV000:FNBRV7)
002900020225      *---------
003000140625     FFNBRV05L  UF A E           K DISK    COMMIT
003100140625     f                                     RENAME(FNBRV000:FNBRV5)
003200070123     FFNBRVE1L  IF A E           K DISK    RENAME(FNBRV000:FNBRV00E)
003300020225      *---------
003400970123     FFNEVS02L  IF A E           K DISK    COMMIT
003500020225      *---------
003600970124     FFNEVS01L  O  A E             DISK    COMMIT
003700020226     f                                     USROPN
003800020226     f                                     RENAME(FNEVS000:FNEVS1)
003900020225      *---------
004000990519     FFNANM10L  UF   E           K DISK    COMMIT
004100020225      *---------
004200020225     FFNANM02L  IF   E           K DISK    RENAME(FNANM000:FNANM2)
004300020226     f                                     INFDS(FLAN2)
004400020225      *---------
004500931229     FTABEL00F  IF   E           K DISK
004600020225      *---------
004700021009     FAZORG01L  IF   E           K DISK
004800021009      *---------
004900961127     FCNACO00F  IF   E           K DISK
005000961127     FCNIND00F  IF   E           K DISK
005100110510     FTntbe01l  IF   E           K DISK
005200961220     FEDTAB01L  IF   E           K DISK
005300050523     F****TREXP00F  O  A E             DISK    COMMIT
005400050523     FWFMAN00F  O  A E             DISK
005500970127     FFNFGV01L  IF   E           K DISK
005600020225      *--------- manifest in forma estesa
005700041109     FMANPTN    O    F  112        PRINTER OFLIND(*INOF)
005800020226     f                                     USROPN
005900100224     f                                     FORMLEN(66)
006000020226     f                                     FORMOFL(44)
006100020225      *--------- manifest in forma sintetica
006200970110     FMANPTS    O    F  132        PRINTER OFLIND(*INOC)
006300020226     f                                     USROPN
006400020225      *--------- manifest per la dogana
006500041109     FMANDGN    O    F  112        PRINTER OFLIND(*INOD)
006600020226     f                                     USROPN
006700100224     f                                     FORMLEN(66)
006800020226     f                                     FORMOFL(44)
006900020225      *--------- manifest delle spedizioni COD
007000041109     FMANCOD    O    F  112        PRINTER OFLIND(*INOG)
007100020226     f                                     USROPN
007200100224     f                                     FORMLEN(66)
007300020226     f                                     FORMOFL(44)
007400020225      *--------- lista discordanze su carta
007500970114     FLISDIC    O    F  132        PRINTER OFLIND(*INOE)
007600020226     f                                     USROPN
007700020225      *--------- lista discordanze via fax
007800970114     FLISDIS    O    F  132        PRINTER OFLIND(*INOB)
007900020226     f                                     USROPN
008000100224     f                                     FORMLEN(66)
008100020226     f                                     FORMOFL(44)
008200020225
008300020225      *--------------------------------------------------------------*
008400020225      *  SCHIERE
008500020225      *--------------------------------------------------------------*
008600961216     D ST              S             66    DIM(21) CTDATA PERRCD(1)             RIGHE STAMPA
008700961217     D DIS             S             66    DIM(23) CTDATA PERRCD(1)             RIGHE ST DISC
008800041109     d D46             s             46    dim(10) CTDATA PERRCD(1)             x lung.112 prtf
008900041021     D CMD             S             69    DIM(12) CTDATA PERRCD(1)             OVRPRTF
009000961129     D NSC             S              8    DIM(13)                              SEGNAC.NON SEQ.
009100961129     D K08             S              1    DIM(8)                               DI COMODO
009200970110     D MAS             S             66    DIM(11) CTDATA PERRCD(1)             MANIFEST SINT.
009300980126     D CAT             S              1  0 DIM(20)                              CATEGORIE
009400980126     D NST             S              1    DIM(20)                              NR.SPUNTE GG.
009500040416     d wdta            s               d   datfmt(*Iso)
009600040416     d wdta1           s               d   datfmt(*Iso)
009700020225
009800020225      *--------------------------------------------------------------*
009900020225      * DS
010000020225      *--------------------------------------------------------------*
010100150825     D DSQT1         E DS                  INZ
010200110510     D dvpobrt       e DS
010300931229     D                 DS
010400941221     D  ARBAAS                 1      4  0
010500941221     D  ARBMGS                 5      8  0
010600941221     D  ARBDSP                 1      8  0
010700020225      *
010800960716     D WLBDA8          DS
010900960716     D  G02DAT                 1      8  0
011000960716     D  G02INV                 9     16  0
011100960716     D  G02ERR                17     17
011200960716     D  G02TGI                18     22  0
011300020225      *
011400960530     D                 DS
011500960530     D  AA                     1      4  0
011600960530     D  MM                     5      6  0
011700960530     D  GG                     7      8  0
011800960530     D  DATA                   1      8  0
011900020225      *
012000931229     D                 DS
012100960530     D  AAS                    1      4  0
012200960530     D  LNP                    5      7  0
012300960530     D  NRS                    8      9  0
012400960530     D  NSP                   10     16  0
012500960530     D  ARTSPE                 1     16  0
012600020225      *
012700020225     D TCUDS           DS
012800020225     D  F3                     3      3
012900020225     D  F4                     4      4
013000020225     D  F56                    5      6
013100020225      *      PER PRENDERE FASE ANOMALIA              -TRUL11R-
013200020225     D PARAM9          DS
013300020225     D  PA9TLA                 1      1    INZ('L')
013400020225     D  PA9NPG                 2      2    INZ('1')
013500020225     D  PA9SPG                 3      3    INZ(' ')
013600020225     D  FASE                   4      4    INZ
013700020225      *
013800020225     D                 DS
013900020225     D  OUTQPR                29     38
014000020225     D  CMDPRT                 1     69
014100020225      *     KEY PER CHAIN SUI FV
014200020225     D                 DS
014300020225     D  BRVNPG                 1      1  0
014400020225     D  BRVNFV                 2      7  0
014500020225     D  BRVFGS                 8     10  0
014600020225     D  BRVKEY                 1     10  0
014700020225      *
014800020225     D TRUL06DS      E DS
014900020225     D  £1                     1     90  0 DIM(30)                              Fil. gestite
015000020225      *
015100020225     D UT§DSE0F      E DS
015200020225     D  TCU                  398    697    DIM(50)                              PARAM FLG7/8,TIPC
015300020225     D  KCU                  698    847P 0 DIM(50)                              PARAM CAPI CONTO
015400020226     D                                     PACKEVEN
015500020225      *
015600020225     D KPJBA         E DS
015700020225     D  LIBSYS                92    100
015800020225      *
015900961115     D FLAN2           DS
016000961115     D  ANMNR2               397    400B 0
016100020225      *
016200020226     D OG143         E DS
016300020226     D DS3A          E DS
016400020225     D DS7A2         E DS
016500020225     D DS7N          E DS
016600090402     D dta4K         E DS
016700090402     D CNCR80        E DS
016800110511     D CNCR42        E DS
016900020225     D FNLR33DS      E DS
017000020225     D TRUL42DS      E DS
017100020225     D TRTC82DS      E DS
017200020228     D TRTC83DS1     E DS
017300020225     D EDIDS1A       E DS
017400130215      *-------
017500130215      *  Ds per reperimento tabella PT e campo risultato
017600130215     D DSPT          E DS                  EXTNAME(EDIDSPT)
017700130215     D DSPZ          E DS                  EXTNAME(EDIDSPZ)
017800130215      *-------
017900061127     D dar5gen       e ds
018000041020      *
018100041020     D* ds per ind. email
018200041020     Dtrul44ds       E DS                  INZ
018300050913     D trtcm1ds      E DS
018400060918     d
018500020225      *--------------------------------------------------------------*
018600020225      * DEFINIZIONE COSTANTI
018700020225      *--------------------------------------------------------------*
018800021009     D Return          C                   'R'
018900021009     D LR              C                   'L'
019000021009     D Eseguito        C                   '0'
019100021009     D Errore          C                   '1'
019200021009     D Generica        C                   '0'
019300021009     D POAmministr     C                   '1'
019400021009     D FaxIn           C                   '2'
019500021009     D FaxOut          C                   '3'
019600021009     D LetVettura      C                   '4'
019700021009      *
019800021009     D PrmFine         S              1    INZ(LR)
019900021009     D PrmTpOutQ       S              1
020000021009      *
020100021009     D PrmFilS         S              3S 0
020200021009     D PrmOutQ         S             10
020300021009     D PrmOutQLib      S             10
020400021009     D PrmEsito        S              1
020500021009      *
020600021009     D MyOutQ          S             10
020700021009     D MyOutQLib       S             10
020800021009      *
020900021009     D FaxInOutq       S             10
021000021009     D FaxInLibq       S             10
021100021009      *
021200021009     D FaxOutOutq      S             10
021300021009     D FaxOutLibq      S             10
021400041109      *
021500041109     D Email_PTN       S              1
021600041109     D Email_COD       S              1
021700041109     D Email_DGN       S              1
021800041109      *
021900041109     D PRT_PTN_TES     S              1
022000041109     D PRT_COD_TES     S              1
022100041109     D PRT_DGN_TES     S              1
022200021009      * --------------------------------------------------------
022300961128     D COS1            C                   CONST('PRIMA DATA INOLTRO :')
022400020225      *             'INITIAL DATE TRANSM.'C         COS2
022500020225
022600020225      *--------------------------------------------------------------*
022700020225      * DEFINIZIONE CAMPI
022800020225      *--------------------------------------------------------------*
022900020225     D SAVCBO          S                   LIKE(ARBcbo)
023000020225     D STATBO          S              3
023100020225     D Wdis4           S              1
023200060918     D XX              S              3  0
023300060918     D YY              S              3  0
023400020808     D*
023500020808     D KDT3            S                   LIKE(ARBDT3)
023600020808     D KAAS            S                   LIKE(ARBAAS)
023700020808     D KNSP            S                   LIKE(ARBNSP)
023800020808     D*
023900020808     D WKPJBU          S                   LIKE(KPJBU )
024000020808     D KFLS            S                   LIKE(ARTFLS)
024100020808     D KLNA            S                   LIKE(ARTLNA)
024200020808     D KNRS            S                   LIKE(ARTNRS)
024300020808     D KNSC            S                   LIKE(ARTNSC)
024400020808     D WAAS            S                   LIKE(ARTAAS)
024500020808     D WLNP            S                   LIKE(ARTLNP)
024600020808     D WNSP            S                   LIKE(ARTNSP)
024700020808     D KDTR            S                   LIKE(ARTDTR)
024800020808     D KTRC            S                   LIKE(AR4TRC)
024900020808     D STANT1          S                   LIKE(AR4NOT)
025000020808     D STANT2          S                   LIKE(AR4NOT)
025100020808     D*
025200020808     D KNPG            S                   LIKE(BRVNPG)
025300020808     D KNFV            S                   LIKE(BRVNFV)
025400020808     D D82NFS          S                   LIKE(BRVNFV)
025500020808     D KFGS            S                   LIKE(BRVFGS)
025600020808     D KLNP            S                   LIKE(BRVLNP)
025700020808     D KFLE            S                   LIKE(BRVFLE)
025800020808     D WRVZNC          S                   LIKE(BRVZNC)
025900040225     D*SA1KEY          S                   LIKE(BRVKEY)
026000020808     D*
026100020808     D KCEV            S                   LIKE(EVSCEV)
026200020808     D WCODIS          S                   LIKE(EVSCEV)
026300020808     D WDTPAR          S                   LIKE(EVSDAT)
026400020808     D WDTPA1          S                   LIKE(EVSDAT)
026500020808     D WFIL            S                   LIKE(EVSFIL)
026600020808     D WNPS            S                   LIKE(EVSNPS)
026700020808     D WDEV            S                   LIKE(EVSDEV)
026800040415     D WDEV2a          S                   LIKE(EVSDEV)
026900020808     D*
027000020808     D KCAA            S                   LIKE(ANMCAA)
027100020808     D DFV06           S                   LIKE(ANMDAO)
027200020808     D*
027300020808     D KKUT            S                   LIKE(TBLKUT)
027400020808     D KCOD            S                   LIKE(TBLCOD)
027500020808     D KKEY            S                   LIKE(TBLKEY)
027600020808     D*
027700020808     D SAVSPE          S                   LIKE(ARTSPE)
027800020808     D WPAG            S                   LIKE(PAGE  )
027900020808     D WPAG1           S                   LIKE(PAGE  )
028000020808     D*
028100020808     D PARDFV          S                   LIKE(D82DFV)
028200020808     D STADSP          S                   LIKE(D82DFV)
028300020808     D UDATE8          S                   LIKE(D82DFV)
028400020808     D*
028500020808     D STPFAX          S                   LIKE(INDTLF)
028600020808     D*
028700020808     D LNAEXP          S                   LIKE(D82LNP)
028800020808     D WTRC            S                   LIKE(TRETRC)
028900020808     D KECOD           S                   LIKE(TABCOD)
029000020808     D KEKEY           S                   LIKE(TABKEY)
029100020808     D STATIC          S                   LIKE(§1ADES)
029200090402     d
029300090402     d WTA430          s             21    inz('GAITRAGRU /TITA430C')
029400090402     d WTAS30          s             21    inz('GAITRAGRU /TITAS30C')
029500020808      *
029600020808     D WARFOR          S              1    inz
029700060918     D wcreadis        S              1    inz
029800070220     D wtimez          S               z
029900070220     D dataiso         S               d   datfmt(*iso)
030000070220     D oraiso          S               t   timfmt(*iso)
030100070220
030200070220     D                SDS
030300070220     D  NOMPGM                 1     10
030400120821     D FEDEX           C                   CONST('D')
030500120821     D NETEXP          C                   CONST('N')
030600120821     D SCANDPD         C                   CONST('P')
030700120821     D SCANDPDEDI      C                   CONST('S')
030800120821     D EDIESTAMPA      C                   CONST('Z')
030900120821     D EDINOPRINT      C                   CONST('W')
031000120821     D PRINTNOEDI      C                   CONST('M')
031100120821     D SOLOSTAMPA      C                   CONST(' ')
031200140311
031300140311      ** ______
031400140311      ***  per gestire il passaggio dei tracciati SCAN e GEOPOST
031500140311      ***    valori:     V=Vecchio / N= Nuovo / E=Entrambe
031600140311      **
031700140311     D DPD_OLD_NEW     s              1    inz('E')
031800140311     d flagHDE         s              1    inz('H')
031900140311      **
032000140311      ** ______
032100020225
032200090402     ITITAS000      81
032300090402     ITITAS010      82
032400090402     ITITASP00      83
032500090402     I
032600090402     ITITA4000      84
032700090402     ITITA4P00      85
032800020225      ****************************************************************
032900020225      *  RIEPILOGO INDICATORI
033000020225      ***************************************************************
033100060918      * 01    - un collo della spedizione in  IMA
033200020225      * 02    - STAMPO 'PARTIAL' NEL MANIFEST SE INOLTRO PARZIALE
033300020225      * 04    - STAMPATO QUALCOSA NEL MANIFEST PER IL PARTNER
033400020225      * 05    - STAMPATO QUALCOSA NEL MANIFEST SPEDIZIONI C.O.D.
033500020225      * 06    - STAMPATO QUALCOSA NEL MANIFEST PER LA DOGANA
033600020225      * 08    - STAMPATO QUALCOSA NELLA LISTA DISCORDANZE
033700020225      * 09    - ON: COLLO A TERRA
033800020225      * 12    - PRIMA RIGA DI STAMPA ELENCO SEGNACOLLI PER DISC.4
033900020225      * 14    - ON - IN LISTA DISCORD. NON STAMPO IL P.O. SPUNTA COLLO
034000020225      * 15    - ON - HO LA KEY PER SPEDIZIONE PER STAMPA
034100020225      * 16    - ON - HO LA KEY PER SEGNACOLLO PER STAMPA
034200020225      * 17    - ON - HO LA SERIE
034300060913      * 18    - ON - STAMPO DISCORDANZA E4  - E8
034400020225      * 20    - ON - ELABORAZIONE DEFINITIVA
034500020225      * 22    - ON - FOGLIO VIAGGIO BIS
034600020226      * 31/32 - DI COMODO
034700061127      * 33    - STAMPO I DATI RELATIVI AL CONTATTO NOME/TEL
034800020225      * 34    - STAMPO I DATI RELATIVI AL C/ASSEGNO
034900020225      * 35/36 - STAMPO I DATI RELATIVI ALLE NOTE
035000020225      * 42    - CREO SPUNTA DA CAMPI DI FNANM
035100020225      * 43    - CREO SPUNTA DA CAMPI DI FNART
035200020225
035300020225      *****************************************************************
035400020225      * CICLO PRINCIPALE
035500020225      *****************************************************************
035600020225      * SE FV GIA' CHIUSO VADO A FINE PGM
035700020808     ***C     D82NFV        CHAIN     FNFGV01L
035800020808     C     K02FGV01      CHAIN     FNFGV01L
035900040317     C                   if        %found(FNFGV01L) and FGVFCF = 'S'
036000970127     C                   GOTO      FINEL
036100020225     C                   endif
036200020225      *
036300020225      * Reperisco dati Partner in base al codice cliente
036400960529     C                   EXSR      DATPAT
036500041020      *
036600020225      * Ovrprtf su file Printer
036700940131     C                   EXSR      OVRPRT
036800020225      *
036900020225      * GIRO SU ARB PER SPEDIZIONI DEL MANIFEST E DISCORDANZA 4
037000120823      *                *------------------*
037100961111     C                   EXSR      REDSPE
037200120823      *                *------------------*
037300020225      * GIRO SU ART E ANOMALIE PER LE ALTRE DISCORDANZE E AGGIORNARE
037400020225      * ARTDTR PER I COLLI CHE PARTONO
037500020225      * Se elaborazione definitiva o se richiesta lista discordanze
037600020225     c                   IF        *in20 = *on  or  D82lds = 'SI'
037700961113     C                   EXSR      CTRDIS
037800020225     C                   ENDIF
037900020225      *
038000020225      * FINE STAMPE
038100020225    1C     *IN08         IFEQ      *ON                                          LISTA DISCORD.
038200120821     c     d82ftt        ifne      FEDEX
038300040423
038400040423     c                   if        FAX_Disc_Est = 'S'
038500020225     C   20              EXCEPT    FINDIS                                       via fax
038600040423     c                   end
038700040423
038800020301     c                   endif
038900020225     C                   EXCEPT    FINDIC                                       su carta
039000020225   x1C                   ELSE
039100020225    2C     D82LDS        IFEQ      'SI'
039200970313     C* Al partner trasmetto LISDIS vuoto solo se ho stampato qualcosa
039300020225     C* nel manifest (solo se definitivo
039400020301     c                   if        *IN04 = *on AND *IN20 = *ON
039500120821     c                             and d82ftt <> FEDEX
039600040423
039700040423     c                   if        FAX_Disc_Est = 'S'
039800020225     C                   ADD       1             WPAG
039900020225     C                   EXCEPT    TESDIS
040000020225     C                   EXCEPT    DETDI3
040100040423     c                   end
040200040423
040300020225    3C                   endif
040400970114     C                   ADD       1             WPAG1
040500970114     C                   EXCEPT    TESDIC
040600970114     C                   EXCEPT    DETDC3
040700020225    2C                   ENDIF
040800020225    1C                   ENDIF
040900020225      *
041000961213     C     *IN04         IFEQ      *ON                                          MANIFEST PARTN
041100020225      *
041200020225     C                   if        *IN20 = *ON                                  MANIFEST PARTN
041300020225     C   OF              EXCEPT    TESTA
041400020225     C                   EXCEPT    TOTAL
041500020225     C                   EXCEPT    FINE
041600020225     c                   else
041700020225     C   OC              EXCEPT    TESSIN
041800020225     C                   EXCEPT    TOTSIN
041900020225     C                   EXCEPT    FINSIN
042000020225     c                   endif
042100020225      *
042200970110     C                   ELSE
042300020225     C                   if        D82STB = 'SI' and *in20 = *off
042400020225     C                   EXCEPT    TESSIN
042500020225     C                   EXCEPT    FINSIN
042600020225     C                   endif
042700970115     C                   ENDIF
042800961212     C*
042900961213     C     *IN06         IFEQ      *ON                                          MANIFEST DOG
043000120821     c     d82ftt        andne     FEDEX
043100961212     C   OD              EXCEPT    TESDGN
043200961212     C                   EXCEPT    TOTDGN
043300961212     C                   EXCEPT    FINDGN
043400961212     C                   ENDIF
043500020225      *
043600961213     C     *IN05         IFEQ      *ON                                          MANIFEST COD
043700120821     c     d82ftt        andne     FEDEX
043800961121     C   OG              EXCEPT    TESCOD
043900961121     C                   EXCEPT    TOTCOD
044000961121     C                   EXCEPT    FINCOD
044100961121     C                   ENDIF
044200020225      *
044300020225      * Per elaborazione definitiva
044400020225      * Richiamo scrittura file x ultima volta con numero sped = 0
044500020225     C                   SELECT
044600020225     C                   WHEN      *IN20 = *off
044700020917      * EUROEXPRESS in sede
044800051221      *   o DPD x SEUR (EDI)
044900120821     c                   WHEN      D82FTT = EDINOPRINT
045000120821     c                              or
045100120821     c                             D82FTT = EDIESTAMPA
045200120821     c                              or
045300120821     c                             D82FTT = SCANDPDEDI
045400020225     C                   clear                   D82AAS
045500020225     C                   clear                   D82LNP
045600020225     C                   clear                   D82NRS
045700020225     C                   clear                   D82NSP
045800020225     C                   MOVEL     TRTC82DS      KPJBU
045900960530     C                   CALL      'TRTC83R2'
046000960530     C                   PARM                    KPJBA
046100120823      **
046200120821      * GEL   (Scrittura del file NET EXPRESS EUROPE)
046300120821     c                   WHEN      D82FTT = NETEXP
046400120821     C                   clear                   F83AAS
046500120821     C                   clear                   F83LNP
046600120821     C                   clear                   F83NRS
046700120821     C                   clear                   F83NSP
046800120821     c                   clear                   f83dfv
046900120821     C                   clear                   KPJBU
047000120821     C                   MOVEL     TRTC83DS1     KPJBU
047100120905     C                   CALL      'TRTC83G2R'
047200120821     C                   PARM                    KPJBA
047300120821      * REIMPOSTO KPJBU "ORIGINALE"
047400120821     C                   MOVEL     WKPJBU        KPJBU
047500120821      *
047600020225      * FEDEX
047700120821     c                   WHEN      D82FTT = FEDEX
047800020228     C                   clear                   F83AAS
047900020228     C                   clear                   F83LNP
048000020228     C                   clear                   F83NRS
048100020228     C                   clear                   F83NSP
048200020228     c                   clear                   f83dfv
048300020228     C                   MOVEL     TRTC83DS1     KPJBU
048400020225     C                   CALL      'TRTC83R21'
048500971230     C                   PARM                    KPJBA
048600020304      * REIMPOSTO KPJBU "ORIGINALE"
048700020304     C                   MOVEL     WKPJBU        KPJBU
048800981106      *
048900020225     C                   ENDSL
049000020225      *
049100020225      * SCRIVO RECORD IN WORK FILE PER LA LNA APPENA ELABORATA
049200970527     C                   CLEAR                   TREXP000
049300970527     C  N20              MOVEL     'P'           TRETRC
049400120821     C   20              MOVEL     'D'           TRETRC
049500970527     C                   MOVE      D82NFV        TRENFV
049600970527     C                   MOVE      SIMFEL        TRELNP
049700970527     C                   MOVE      LNAEXP        TRELNA
049800020225      * MEMORIZZO TOTALI SOLO SE PROVVISORIO O SE ELABORAZ.PER DOGANA
049900020225      * (INFATTI LA STAMPA DEI TOTALI DEL FV SERVE PER LA COMPILAZIONE
050000020225      * DEL MODULO CMR E DA TRASMETTERE ALLA DOGANA INSIEME AL MANIFEST
050100020301     C                   IF        *IN20 = *OFF  or (D82DGN = 'SI' and
050200120821     C                             d82ftt <> FEDEX)
050300970527     C                   Z-ADD     TOTPES        TRETOP
050400050520     C                   Z-ADD     totvlf        TRETOv
050500970527     C                   Z-ADD     TOTNCL        TRETOC
050600020225     C                   ENDIF
050700020225      *
050800970527     C                   WRITE     TREXP000                             31
050900020225      *
051000020225      * Reimposto LNA salvata nella DS
051100970123     C                   MOVE      LNAEXP        D82LNP
051200020225      *
051300970127     C     FINEL         TAG
051400020225      *
051500020225      *
051600020225     C                   eval      *inLR = *ON
051700960529     C*****************************************************************
051800970114     C*  Reperisco i dati del partner
051900960529     C*****************************************************************
052000960529     C     DATPAT        BEGSR
052100960529     C*
052200961127     C                   MOVEL     *ALL'?'       STADLA           25
052300960529     C*  Decodifico lna
052400020226     C     LNAEXP        CHAIN     AZORG
052500020226     C                   movel     orgde3        OG143
052600020226     C                   MOVEL(P)  ORGDES        STADLA
052700970110     C*
052800970110     C                   MOVEL     STADLA        STADLS           15
052900970117     C*  Reperisco numero fax e ragione sociale partner
053000961127     C*
053100961127     C     KIND          CHAIN     CNACO00F                           31
053200961217     C  N31              MOVEL     ACORAG        STARAG           48
053300961127     C   31              CLEAR                   STARAG
053400970110     C                   MOVEL     STARAG        STARGS           30
053500961127     C*
053600961122     C                   CLEAR                   STPFAX
053700961122     C     KIND          CHAIN     CNIND00F                           31
053800961122     C     *IN31         IFEQ      *OFF
053900020225     C                   CLEAR                   TRUL42DS
054000961122     C                   MOVEL     INDTLF        D42FAX
054100961122     C                   CALL      'TRUL42R'
054200020225     C                   PARM                    TRUL42DS
054300961122     C     D42ERR        IFEQ      ' '
054400961122     C                   MOVEL     INDTLF        STPFAX
054500961122     C                   END
054600961216     C                   END
054700970117     C*
054800970117     C* PER COMODITA' MUOVO RAGIONE SOCIALE DOGANA IN CAMPO LUNGO 48
054900970117     C                   MOVEL     D82RSX        STARSX           48
055000041020     C*
055100041020     C* Nuovo Concetto:
055200041020     C*  L'indirizzo E-mail è prioritario rispetto al FAX ossia se presente
055300041020     C*  in anagrafica oltre al nr.FAX l'invio del MANIFEST non viene più
055400041020     C*  eseguito mediante FAX ma per posta Elettronica.
055500041020     C* Prende indirizzo e-mail del Partner in anagrafica Note.
055600041020     C*
055700041020     C*
055800960529     C* Pulisco  i campi di totale x partner
055900960529     C                   CLEAR                   TOTSPE
056000960529     C                   CLEAR                   TOTNCL
056100961122     C                   CLEAR                   TOTPES
056200960529     C                   CLEAR                   TOTVLF
056300961204     C                   CLEAR                   CODSPE
056400961204     C                   CLEAR                   CODNCL
056500961204     C                   CLEAR                   CODPES
056600961204     C                   CLEAR                   CODVLF
056700960530     C*
056800960530     C                   ENDSR
056900960529     C*****************************************************************
057000960529     C* OVRPRTF: Ovrprtf file di stampa
057100960529     C*****************************************************************
057200960529     C     OVRPRT        BEGSR
057300021009     C*
057400041021     C                   Exsr      Code_FAX
057500041020     C*
057600971230     C                   Z-ADD     69            LUNG
057700961213     C**
057800961213     C* OVRPRTF MANIFEST PER PARTNER
057900961213     C**
058000961213    1C     D82STB        IFEQ      'SI'
058100970205     C* E l a b o r a z i o n e   d e f i n i t i v a
058200961213    2C     *IN20         IFEQ      *ON
058300041020     C**
058400970205    3C                   SELECT
058500041020     C**
058600970224     C* Se ho trovato il n.fax, ed è previsto l'invio della stampa
058700020917     c* (ftt = ' ' )
058800970205     C* --> metto spool in faxin per invio fax in automatico
058900970205     C     STPFAX        WHENNE    *BLANKS
059000120821     C     D82FTT        ANDEQ     EDIESTAMPA
059100120821     C     STPFAX        orNE      *BLANKS
059200120821     C     D82FTT        ANDEQ     NETEXP
059300971230     C                   MOVEL     CMD(2)        CMDPRT
059400021009     C                   MOVEL     FaxInOutQ     OUTQPR
059500971230     C                   MOVEL     CMDPRT        COMMAN
059600041020     C**
059700020917     C* Se è prevista solo la trasmiEDI  (FTT='W')
059800970205     c* --> metto spool in faxout per avere una copia di sicurezza
059900970205     C*     nel caso la trasmissione non dovesse riuscire
060000120821     C     D82FTT        WHENEQ    EDINOPRINT
060100120821     C     D82FTT        OREQ      FEDEX
060200120821     C     D82FTT        OREQ      SCANDPD
060300120821     C     D82FTT        OREQ      SCANDPDEDI
060400971230     C                   MOVEL     CMD(10)       CMDPRT
060500021009     C                   MOVEL     FaxOutOutQ    OUTQPR
060600971230     C                   MOVEL     CMDPRT        COMMAN
060700041020     C**
060800970205     C* altrimenti lo lascio nella coda di stampa di default
060900970205     C                   OTHER
061000961213     C                   MOVEL     CMD(1)        COMMAN
061100970205    3C                   ENDSL
061200041021     C**
061300041021     C**  però adesso bisogna fare Attenzione:  --> E-MAIL !!!!
061400041021     C**    se c'è l'indirizzo E-mail e prima era previsto l'invio con il FAX,
061500041021     C**    non deve essere più inviato il FAX ma tutto viene trasmesso con e-mail.
061600041021     C**  Il TRUL44R esegue anche l'OVRPRTF x e-mail.
061700120821     C     D82FTT        ifeq      EDIESTAMPA
061800120821     C     D82FTT        oreq      PRINTNOEDI
061900120821     C     D82FTT        oreq      NETEXP
062000041021     c                   clear                   trul44ds
062100050913     c                   clear                   trtcm1ds
062200041021     c                   eval      d44pgm = 'TRTC82R'
062300041021     c                   move      d82cpt        d44ksc
062400041021     c                   eval      d44apl = 'C'
062500041118     c                   eval      d44nk1 = '0151' + %editc(D82CPT:'X')
062600041021     c                   eval      d44tnt = '86'
062700041021     c                   eval      d44scope='*JOB'
062800041021     c                   eval      d44ovr = 'S'
062900120305     c***66              eval      d44obj = '*OBJM*Bartolini Shipment '+
063000120305     c   66              eval      d44obj = '*OBJM*Bartolini-BRT Shipment '+
063100110609     c                             'Manifest del ' + %editw(PARDFV:'  /  /    ')
063200110609     c                              + ' - ' + %trim(Starag)
063300120305     c**N66              eval      d44obj = '*OBJM*Bartolini-BRT Shipment '+
063400120305     c  N66              eval      d44obj = '*OBJM*BRT Shipment '+
063500110609     c                             'Manifest del ' + %editw(PARDFV:'  -  -    ')
063600110609     c                              + ' - ' + %trim(Starag)
063700041021     c                   eval      d44prtf = 'MANPTN'
063800041021     c                   call      'TRUL44R'
063900041021     c                   parm                    trul44ds
064000050913     c                   parm                    trtcm1ds
064100041025      **  Se tutto OK salta la QCMDEXEC
064200041025      **   con la vecchia OVRPRTF
064300041109     c     d44err        ifeq      *blank
064400041109     c                   eval      Email_PTN='S'
064500041109     c                   goto      gia_fatta
064600041109     c                   end
064700041021     c                   end
064800041021     C**
064900041109     C**  Viene eseguita se non è stata mandata via E-mail
065000041109     C**   notare il TAG seguente
065100970115     C                   CALL      'QCMDEXC'                            90
065200970115     C                   PARM                    COMMAN           80
065300970115     C                   PARM                    LUNG             15 5
065400041021     c     gia_fatta     tag
065500041020     C**
065600041020     C**  quindi apre il Manifest x il Partner
065700970110     C                   OPEN      MANPTN
065800041020     C**
065900970110   X2C                   ELSE
066000970205     C* E l a b o r a z i o n e   p r o v v i s o r i a
066100970110     C* Se elaborazione provvisoria stampo il manifest in forma sinte-
066200970110     C* tica
066300970110     C                   MOVEL     CMD(9)        COMMAN
066400970110     C                   CALL      'QCMDEXC'                            90
066500970110     C                   PARM                    COMMAN           80
066600970110     C                   PARM                    LUNG             15 5
066700970110     C                   OPEN      MANPTS
066800970110    2C                   END
066900961213    1C                   END
067000041020     C** --------------
067100041020     C**
067200961213     C* OVRPRTF MANIFEST PER DOGANA
067300020301     c* Per Fedex non devo mai stampare il manifest per dogana
067400961213     C**
067500961213    1C     D82DGN        IFEQ      'SI'
067600120821     c     D82FTT        andne     FEDEX
067700041020     C**
067800970114     C* Se ho trovato il n.fax --> metto in faxin per invio fax autom.
067900970117    2C     D82DGX        IFNE      *BLANKS
068000041021     C**
068100971230     C                   MOVEL     CMD(6)        CMDPRT
068200021009     C                   MOVEL     FaxInOutQ     OUTQPR
068300971230     C                   MOVEL     CMDPRT        COMMAN
068400041020     C**
068500961213   X2C                   ELSE
068600961213     C* altrimenti lo lascio nella coda di stampa di default
068700961213     C                   MOVEL     CMD(5)        COMMAN
068800961213    2C                   END
068900041021     C**
069000041021     C**  però adesso bisogna fare Attenzione:  --> E-MAIL !!!!
069100041021     C**    se c'è l'indirizzo E-mail e prima era previsto l'invio con il FAX,
069200041021     C**    non deve essere più inviato il FAX ma tutto viene trasmesso con e-mail.
069300041021     C**  L'OVRPRTF è in base ai parametri restituiti dal TRUL44R.
069400041021     C**      è possibile eseguire tramite il TRUL44R l'OVRPRTF necessaria.
069500041021     c                   clear                   trul44ds
069600050913     c                   clear                   trtcm1ds
069700041021     c                   eval      d44pgm = 'TRTC82R'
069800041025     c                   eval      d44cl = '200'
069900041021     c                   move      D82CPT        d44ksc
070000041021     c                   eval      d44scope='*JOB'
070100041021     c                   eval      d44ovr = 'S'
070200120305     c***66              eval      d44obj = '*OBJM*Bartolini Shipment '+
070300120305     c   66              eval      d44obj = '*OBJM*Bartolini-BRT Shipment '+
070400110609     c                             'Manifest del ' + %editw(PARDFV:'  /  /    ')
070500110609     c                              + ' - ' + %trim(D82RSX)
070600120305     c**N66              eval      d44obj = '*OBJM*Bartolini-BRT Shipment '+
070700120305     c  N66              eval      d44obj = '*OBJM*BRT Shipment '+
070800110608     c                             'Manifest del ' + %editw(PARDFV:'  .  .    ')
070900110608     c                              + ' - ' + %trim(D82RSX)
071000041021     c                   eval      d44prtf = 'MANDGN'
071100041021     c                   call      'TRUL44R'
071200041021     c                   parm                    trul44ds
071300050913     c                   parm                    trtcm1ds
071400041025      **  Se tutto OK salta la QCMDEXEC
071500041025      **   con la vecchia OVRPRTF
071600041109     c     d44err        ifeq      *blank
071700041109     c                   eval      Email_DGN='S'
071800041109     c                   goto      salta_OVRPRTF
071900041109     c                   end
072000041020      **
072100041109     C**  Viene eseguita se non è stata mandata via E-mail
072200041109     C**   notare il TAG seguente
072300970115     C                   CALL      'QCMDEXC'                            90
072400970115     C                   PARM                    COMMAN           80
072500970115     C                   PARM                    LUNG             15 5
072600041021     c     salta_OVRPRTF tag
072700041020      **
072800961213     C                   OPEN      MANDGN
072900961213    1C                   END
073000961213     C**
073100961213     C* OVRPRTF MANIFEST PER SPEDIZIONI C.O.D.
073200020301     c* Per fedex non stampo mai il manifest delle sped. c.o.d.
073300961213     C**
073400961213    1C     D82COD        IFEQ      'SI'
073500120821     c     D82FTT        ANDNE     FEDEX
073600041020     C**
073700961213     C* Se elaboraz.defin. e ho trovato il n.fax --> metto spool in
073800961213     C* in faxin per invio fax in automatico
073900961213    2C     *IN20         IFEQ      *ON
074000961213     C     STPFAX        ANDNE     *BLANKS
074100041021     C**
074200971230     C                   MOVEL     CMD(4)        CMDPRT
074300021009     C                   MOVEL     FaxInOutQ     OUTQPR
074400971230     C                   MOVEL     CMDPRT        COMMAN
074500041020     C**
074600961213   X2C                   ELSE
074700961213     C* altrimenti lo lascio nella coda di stampa di default
074800961213     C                   MOVEL     CMD(3)        COMMAN
074900961213    2C                   END
075000041021     C**
075100041021     C**  però adesso bisogna fare Attenzione:  --> E-MAIL !!!!
075200041021     C**    se c'è l'indirizzo E-mail e prima era previsto l'invio con il FAX,
075300041021     C**    non deve essere più inviato il FAX ma tutto viene trasmesso con e-mail.
075400041021     C**  Per l'OVRPRTF tramite posta elettronica si utilizza il TRUL44R
075500041021     C**  che imposta i parametri oltre che reperisce anche l'indirizzo e-mail.
075600041021    2C     *IN20         IFEQ      *ON
075700041021     c                   clear                   trul44ds
075800050913     c                   clear                   trtcm1ds
075900041021     c                   eval      d44pgm = 'TRTC82R'
076000041021     c                   move      d82cpt        d44ksc
076100041021     c                   eval      d44apl = 'C'
076200041118     c                   eval      d44nk1 = '0151' + %editc(D82CPT:'X')
076300041021     c                   eval      d44tnt = '86'
076400041021     c                   eval      d44scope='*JOB'
076500041021     c                   eval      d44ovr = 'S'
076600120305     c***66              eval      d44obj = '*OBJM*Bartolini C.O.D. '+
076700120305     c   66              eval      d44obj = '*OBJM*Bartolini-BRT C.O.D. '+
076800110609     c                             'Manifest del ' + %editw(PARDFV:'  /  /    ')
076900110609     c                              + ' - ' + %trim(Starag)
077000120305     c**N66              eval      d44obj = '*OBJM*Bartolini-BRT C.O.D. '+
077100120305     c  N66              eval      d44obj = '*OBJM*BRT C.O.D. '+
077200110608     c                             'Manifest del ' + %editw(PARDFV:'  .  .    ')
077300110608     c                              + ' - ' + %trim(Starag)
077400041021     c                   eval      d44prtf = 'MANCOD'
077500041021     c                   call      'TRUL44R'
077600041021     c                   parm                    trul44ds
077700050913     c                   parm                    trtcm1ds
077800041025      **  Se tutto OK salta la QCMDEXEC
077900041025      **   con la vecchia OVRPRTF
078000041109     c     d44err        ifeq      *blank
078100041109     c                   eval      Email_COD='S'
078200041109     c                   goto      salta_OVR
078300041109     c                   end
078400041021    1C                   END
078500041020     C**
078600041109     C**  Viene eseguita se non è stata mandata via E-mail
078700041109     C**   notare il TAG seguente
078800970115     C                   CALL      'QCMDEXC'                            90
078900970115     C                   PARM                    COMMAN           80
079000970115     C                   PARM                    LUNG             15 5
079100041021     c     salta_OVR     tag
079200041021     C**
079300961213     C                   OPEN      MANCOD
079400961213    1C                   END
079500961213     C**
079600961213     C* OVRPRTF PER LISTA DISCORDANZE
079700961213     C**
079800970110   1AC     D82LDS        IFEQ      'SI'
079900970114     C* lista discordanze via fax se elaborazione definitiva e ho il n.
080000020301     C* fax
080100961213    1C     *IN20         IFEQ      *ON
080200120821     c     d82ftt        andne     FEDEX
080300040423
080400040423     c                   if        FAX_Disc_Est = 'S'
080500970115     C     STPFAX        IFNE      *BLANKS
080600971230     C                   MOVEL     CMD(8)        CMDPRT
080700021009     C                   MOVEL     FaxInOutQ     OUTQPR
080800971230     C                   MOVEL     CMDPRT        COMMAN
080900970115     C                   ELSE
081000970115     C                   MOVEL     CMD(12)       COMMAN
081100970115     C                   END
081200970115     C                   CALL      'QCMDEXC'                            90
081300970115     C                   PARM                    COMMAN           80
081400970115     C                   PARM                    LUNG             15 5
081500970114     C                   OPEN      LISDIS
081600040423     c                   end
081700040423
081800970114    1C                   END
081900970114     C*
082000970114     C* lista discordanze sempre su carta
082100961213     C                   MOVEL     CMD(7)        COMMAN
082200041021     C*
082300961213     C                   CALL      'QCMDEXC'                            90
082400961213     C                   PARM                    COMMAN           80
082500961213     C                   PARM                    LUNG             15 5
082600041021     C*
082700970114     C                   OPEN      LISDIC
082800970110   1AC                   END
082900971219     C*
083000971219     C*  Accendo indicatori di eccedenza
083100971219     C                   SETON                                        OFOGOD
083200971219     C                   SETON                                        OCOEOB
083300961121     C*
083400960529     C                   ENDSR
083500020225      *****************************************************************
083600041020      * Imposta le code x invio FAX
083700020225      *****************************************************************
083800041021     C     Code_FAX      BEGSR
083900020225      *
084000041020     ** Reperisco la coda delle stampe FAXIN .
084100041020     C                   CALL      'TRULOUTQ'
084200041020     ** Parametri di input.
084300041020     C                   PARM                    PrmFine
084400041020     C                   PARM      SimPOU        PrmFilS
084500041020     C                   PARM      FaxIn         PrmTpOutQ
084600041020     ** Parametri di output.
084700041020     C     MyOutQ        PARM      *BLANK        PrmOutQ
084800041020     C     MyOutQLib     PARM      *BLANK        PrmOutQLib
084900041020     C                   PARM      *BLANK        PrmEsito
085000041020     ** Se il reperimento della coda non è riuscito, creo lo spool in QDKT.
085100041020     C                   IF        PrmEsito = Errore
085200041020     C                   EVAL      MyOutQ = 'QDKT'
085300041020     C                   EVAL      MyOutQLib = '*LIBL'
085400041020     C                   ENDIF
085500041020      *  Se non ha trovato imposta la coda generica
085600041020     C                   IF        MyOutQ = *BLANK
085700041020     C                   EVAL      MyOutQ = 'QDKT'
085800041020     C                   EVAL      MyOutQLib = '*LIBL'
085900041020     C                   ENDIF
086000041020      *
086100041020     C                   EVAL      FaxInOutQ = MyOutQ
086200041020     C                   EVAL      FaxInLibQ = MyOutQLib
086300041020     C**
086400041020     C*---------------------------------------------------------
086500041020     ** Reperisco la coda delle stampe FAXOUT.
086600041020     C                   CALL      'TRULOUTQ'
086700041020     ** Parametri di input.
086800041020     C                   PARM                    PrmFine
086900041020     C                   PARM      SimPOU        PrmFilS
087000041020     C                   PARM      FaxOut        PrmTpOutQ
087100041020     ** Parametri di output.
087200041020     C     MyOutQ        PARM      *BLANK        PrmOutQ
087300041020     C     MyOutQLib     PARM      *BLANK        PrmOutQLib
087400041020     C                   PARM      *BLANK        PrmEsito
087500041020     ** Se il reperimento della coda non è riuscito, creo lo spool in QDKT.
087600041020     C                   IF        PrmEsito = Errore
087700041020     C                   EVAL      MyOutQ = 'QDKT'
087800041020     C                   EVAL      MyOutQLib = '*LIBL'
087900041020     C                   ENDIF
088000041020      *  Se non ha trovato imposta la coda generica
088100041020     C                   IF        MyOutQ = *BLANK
088200041020     C                   EVAL      MyOutQ = 'QDKT'
088300041020     C                   EVAL      MyOutQLib = '*LIBL'
088400041020     C                   ENDIF
088500041020      *
088600041020     C                   EVAL      FaxOutOutQ = MyOutQ
088700041020     C                   EVAL      FaxOutLibQ = MyOutQLib
088800041020      *
088900041020     C                   ENDSR
089000041020      *****************************************************************
089100041020      * LETTURA DELLE SPEDIZIONI
089200041020      *****************************************************************
089300041020     C     REDSPE        BEGSR
089400041020      *
089500020225      * LEGGO V.L. SU FNARB PER LNA E DATA TRASMISSIONE = zero
089600020225      * (I RECORD GIA' TRASMESSI NON DEVONO ESSERE RIELABORATI)
089700020225     C                   clear                   KDT3
089800961113     C     KARB55        SETLL     FNARB55L
089900020225     C     KARB55        READE     FNARB55L
090000020225      *
090100020225    1C                   DOW       not %EOF(FNARB55L)
090200020225      *
090300020225      * Escludo locali e spedizioni consegnate
090400020225    2c                   IF        ARBlnp <> ARBlna  and  ARBdcm = *zeros
090500020225      *
090600020225      * Leggo ds3a - tabella codici bolla se varia arbcbo
090700020225    3C     ARBCBO        IFNE      SAVCBO
090800961112     C                   MOVEL     '3A'          KCOD
090900961112     C                   MOVEL(P)  ARBCBO        KKEY
091000961112     C                   EXSR      CTABEL
091100961114     C  N31              MOVEL     TBLUNI        DS3A
091200961114     C   31              CLEAR                   DS3A
091300961112     C                   MOVE      ARBCBO        SAVCBO
091400020225    3C                   ENDIF
091500020225      * Escludo bolle di recupero
091600020225    3c                   IF        §3Arbl <> 'R'
091700020225      *
091800020225      * SFLAGGO BOLLA IN OGNI CASO (POTREBBE ESSERCI 'T' IN SEGUITO
091900020225      * A PRECEDENTE ELABORAZIONE PROVVISORIA MA TALE 'T' POTREBBE NON
092000020225      * ESSERE PIU' VERA)
092100020225     C                   clear                   ARBFT3
092200060918     C                   CLEAR                   CreaE8            1
092300060918     C                   CLEAR                   CreaE7x           1
092400020225      * LEGGO DETTAGLIO COLLI
092500060918     c* Primo giro: per no invio spedizioni parziali
092600060918     c*             non creo le discordanze
092700060918     c                   if        d82isc='S'
092800060918     c                   eval      wcreadis=' '
092900060918     c                   else
093000060918     c                   eval      wcreadis='S'
093100060918     c                   endif
093200060918     c
093300961112     C                   EXSR      REDART
093400060918     c
093500060918     c* Se devo creare discordanze e stampare faccio un secondo giro
093600060918     c*  solo per INVII no parziali
093700060918     c                   if        d82isc='S' and
093800060918     c                             (creae8='S' or creae7x='S')
093900060918     c                   eval      wcreadis='S'
094000060918     C                   EXSR      REDART
094100060918     c                   endif
094200060918     c*
094300020225      **
094400020225      * LA SPEDIZIONE FA PARTE DEL MANIFEST
094500060913      * PER FEDEX       INCLUDO SOLO QUELLE CON ABILITAZIONE MERCE ARR > 1
094600060913      * Se invio TOTALE includo solo quelle con abilitazione merce arr > 1
094700060915      *  e se non in IMA
094800020225      **------------------------------------------------------
094900120821    4C                   IF        (ARBFT3 ='T' and d82ftt <> FEDEX and
095000060913     c                              d82isc=' ') or
095100120821    4C                             (d82ftt = FEDEX and ARBAMA > 1)  or
095200060915    4C                             (d82isc ='S' and ARBAMA > 1 and
095300060918     c                              arbft3='T' and not *in01)
095400060913      *
095500020225      * CODICE BOLLA
095600020225      *  EuroExpress e FedEX
095700020225    6C                   select
095800020225     c                   when      §3ATB1 = 'FS'
095900970219     C                   MOVEL     'EXW'         STATBO
096000020225     c                   when      §3ATB1 = 'AS'
096100970219     C                   MOVEL     'FHC'         STATBO
096200020226     c                   when      %SUBST(§3ATB1:1:1) = 'A'
096300020225     c                             or ARBFBR = 'F'
096400970219     C                   MOVEL     'EXW'         STATBO
096500020225     C                   other
096600020225     C                   MOVEL     'FHC'         STATBO
096700020225    6C                   endsl
096800030317      * Per Fedex
096900150825      * adesso per tutti vale la precedente regola di FEDEX x il PESO/VOLUME
097000150825     c*********          if        d82ftt = FEDEX
097100030318      *
097200030317      * calcola il Volume/Peso da CML o da Bollettato
097300030317     c                   exsr      VOL_PES
097400030317      *
097500150825     C                   Z-ADD     wpes          PESPRT            7 1
097600030317      *
097700150825     c*********          else
097800150825      ********
097900020225      * Determino peso tassabile per le spedizioni in assegnato
098000020225      * (E'il maggiore fra arbpkf e il risultato della moltiplicazione)
098100150825    5C**** STATBO        IFEQ      'EXW'
098200150825     C**** §7AKPM        MULT(H)   ARBVLF        PESPRT            7 1
098300150825    6C****               if        ARBPKF > PESPRT
098400150825     C****               Z-ADD     ARBPKF        PESPRT
098500150825    6C****               endif
098600150825   x5C****               ELSE
098700020225      * Per le spedizioni in franco invece,stampo il peso da fatturare
098800150825     C****               Z-ADD     ARBPKF        PESPRT
098900150825    5C****               END
099000150825     c*********          endIF
099100030317      *
099200020225      * Totale spedizione/colli/peso e volume
099300020225      * N.B.:I totali mi servono anche se elaborazione provvisoria, no
099400020225      *      dogana in quento dovrò stampare i totali del foglio viaggi
099500020225    5C     D82STB        IFEQ      'SI'
099600970219     C     D82DGN        OREQ      'SI'
099700970219     C     *IN20         OREQ      *OFF
099800970219     C                   ADD       1             TOTSPE            5 0
099900970219     C                   ADD       ARBNCL        TOTNCL            7 0
100000970219     C                   ADD       PESPRT        TOTPES           10 1
100100150825     c**********         if        d82ftt = FEDEX
100200150825     C                   add       wvol          TOTVLF            7 3
100300150825     c**********         else
100400150825     C**********         ADD       ARBVLF        TOTVLF            7 3
100500150825     c**********         end
100600020225    5C                   ENDIF
100700020225      *
100800020225      * STAMPO MANIFEST SOLO SE RICHIESTO:
100900020225      * - Manifest per partner
101000020225    5C     D82STB        IFEQ      'SI'
101100020225      * - Manifest per dogana
101200970219     C     D82DGN        OREQ      'SI'
101300120821     c     d82ftt        andne     FEDEX
101400020225      * - Manifest spedizioni C.O.D. e questa è spedizione C.O.D.
101500970219     C     D82COD        OREQ      'SI'
101600961112     C     §3AFCA        ANDGT     0
101700120821     c     d82ftt        andne     FEDEX
101800961121     C                   EXSR      PRTMAN
101900020225    5C                   ENDIF
102000020225      *
102100020225      * FLEGGO FNARB E SCRIVO RCD PER TRASMISSIONE
102200020226    5C                   SELECT
102300041021      **
102400020225      * "Provvisorio": aggiorno ARBFT3 per l'elaborazione successiva
102500020225     c                   when      *in20 = *off
102600020225     C                   EXCEPT    AGGFT3
102700041021      **
102800020225      * "Definitivo" e "No trasmissione": fleggo la bolla come trasmessa
102900050104      *  o se invio solo via E-Mail
103000120821     c                   when      D82FTT = SOLOSTAMPA
103100120821     c                              or
103200120821     c                             D82FTT = PRINTNOEDI
103300020225     C                   MOVEL     'T'           ARBFT3
103400020225     C                   Z-ADD     D82DFV        ARBDT3
103500090402     c* data manifest in filiale
103600090402     C                   EXCEPT    AGGTRA
103700090402     c* Data manifest in sede
103800090402     c                   Exsr      AggDataM
103900120823      **
104000120823      * GEL   (Scrittura del file NET EXPRESS EUROPE)
104100120823      * "Definitivo" e "Trasmissione": scrivo record per trasmissione,
104200120823      *   il pgm che trasmette flegga la bolla
104300120823     c                   WHEN      D82FTT = NETEXP
104400120823     C                   EXCEPT    LIBER
104500120823     C                   Z-ADD     ARBAAS        F83AAS
104600120823     C                   Z-ADD     ARBLNP        F83LNP
104700120823     C                   Z-ADD     ARBNRS        F83NRS
104800120823     C                   Z-ADD     ARBNSP        F83NSP
104900120823     c                   z-add     d82dfv        f83dfv
105000120823     C                   MOVEL     TRTC83DS1     KPJBU
105100120905     C                   CALL      'TRTC83G2R'
105200120823     C                   PARM                    KPJBA
105300120823      * REIMPOSTO KPJBU "ORIGINALE"
105400120823     C                   MOVEL     WKPJBU        KPJBU
105500120823     c* Data manifest in sede
105600120823     c                   Exsr      AggDataM
105700041021      **
105800020225      * "Definitivo" e "Trasmissione FED": scrivo record per trasmissione,
105900020225      *   il pgm che trasmette flegga la bolla
106000120821     c                   when      D82FTT = FEDEX
106100020225     C                   EXCEPT    LIBER
106200020228     C                   Z-ADD     ARBAAS        F83AAS
106300020228     C                   Z-ADD     ARBLNP        F83LNP
106400020228     C                   Z-ADD     ARBNRS        F83NRS
106500020228     C                   Z-ADD     ARBNSP        F83NSP
106600020228     c                   z-add     d82dfv        f83dfv
106700020228     C                   MOVEL     TRTC83DS1     KPJBU
106800020225     C                   CALL      'TRTC83R21'
106900020225     C                   PARM                    KPJBA
107000020304      * REIMPOSTO KPJBU "ORIGINALE"
107100020304     C                   MOVEL     WKPJBU        KPJBU
107200090402     c* Data manifest in sede
107300090402     c                   Exsr      AggDataM
107400041021      **
107500020225      * "Definitivo" e "Trasmissione EEX": scrivo record per trasmissione,
107600020225      *   il pgm che trasmette flegga la bolla
107700020226     c                   other
107800060109      *
107900060109      * ?dal 9/01/2006 -  DPD esegue il Manifest Definitivo + invio SCAN
108000060109      * ?....invio SCAN DPD oltre EDI se tipo trasm.--> ("S")
108100060109      * ?    al posto del vecchio FIEU25R dal giro degli SCAN DPD schedulato
108200060109     c                   if        wOGGI >= 20060109
108300120821     c                   if        D82FTT = SCANDPDEDI or D82FTT = SCANDPD
108400060109
108500140311      * vecchio invio SCAN a DPD (no GEOPOST file ma vecchi SCAN)
108600140311
108700140311     c                   if        DPD_OLD_NEW = 'V' or
108800140311     c                             DPD_OLD_NEW = 'E'
108900140311      **
109000060109     C                   Z-ADD     ARBAAS        F83AAS
109100060109     C                   Z-ADD     ARBLNP        F83LNP
109200060109     C                   Z-ADD     ARBNRS        F83NRS
109300060109     C                   Z-ADD     ARBNSP        F83NSP
109400060109     c                   z-add     d82dfv        f83dfv
109500060109     C                   MOVEL     TRTC83DS1     KPJBU
109600060619     C                   CALL      'TRTC83R12'
109700060109     C                   PARM                    KPJBA
109800060109      * REIMPOSTO KPJBU "ORIGINALE"
109900060109     C                   MOVEL     WKPJBU        KPJBU
110000060109      **
110100140311     c                   end
110200140311      **
110300140311      * Invio file GEOPOST Nuovo tracciato
110400140311
110500140311     c                   if        DPD_OLD_NEW = 'N' or
110600140311     c                             DPD_OLD_NEW = 'E'
110700140311      **
110800140311      ** la prima volta deve fare la testata e in seguito tutti i dettagli
110900140311      **  il GOTO è fatto solo quindi per il primo record poichè il flagHDE è
111000140311      **  inizializzato a 'H' Head TESTATA
111100140311      **
111200140311     c     Ri_esegui     tag
111300140311     C                   Z-ADD     ARBAAS        F83AAS
111400140311     C                   Z-ADD     ARBLNP        F83LNP
111500140311     C                   Z-ADD     ARBNRS        F83NRS
111600140311     C                   Z-ADD     ARBNSP        F83NSP
111700140311     c                   z-add     d82dfv        f83dfv
111800140311      *
111900140311     C                   MOVEL     TRTC83DS1     KPJBU
112000140311     C                   call      'TRTC83R13'
112100140311     C                   PARM                    KPJBA
112200140311     C                   PARM                    flagHDE           1
112300140311      *
112400140311      * Solo dopo la prima volta x poter scrivere la testata
112500140311     C                   if            flagHDE ='H'
112600140311     C                   movel     'D'           flagHDE
112700140311     c                   goto      ri_esegui
112800140311     c                   end
112900140311      *
113000140311      * REIMPOSTO KPJBU "ORIGINALE"
113100140311     C                   MOVEL     WKPJBU        KPJBU
113200140311      **
113300140311     c                   end
113400140311      **
113500120821     c                   if        D82FTT  = SCANDPD
113600060109     C                   MOVEL     'T'           ARBFT3
113700060109     C                   Z-ADD     D82DFV        ARBDT3
113800090402     c* Data manifest in filiale
113900090402     C                   EXCEPT    AGGTRA
114000090402     c* Data manifest in sede
114100090402     C                   Exsr      AggDataM
114200060109     c                   end
114300060109      *
114400060109     c                   end
114500060109     c                   end
114600051221
114700051223      * ?Non deve eseguire l'EDI x le Trasmissioni DPD non a SEUR
114800120821     c                   if        D82FTT <> SCANDPD
114900051223
115000961112     C                   EXCEPT    LIBER
115100961112     C                   Z-ADD     ARBAAS        D82AAS
115200961112     C                   Z-ADD     ARBLNP        D82LNP
115300961112     C                   Z-ADD     ARBNRS        D82NRS
115400961112     C                   Z-ADD     ARBNSP        D82NSP
115500020225     C                   MOVEL     TRTC82DS      KPJBU
115600961112     C                   CALL      'TRTC83R2'
115700961112     C                   PARM                    KPJBA
115800090402     c* Data manifest in sede
115900090402     C                   Exsr      AggDataM
116000051223
116100051223     c                   end
116200020225      *
116300020225    5C                   ENDSL
116400020225      **
116500020225      * SE RECORD NON DA INVIARE AGGIORNO ARBFT3 PER "PULIRLO"
116600020225   X4C                   ELSE
116700041021      **
116800020304     c* se sto elaborando manifest per fedex arbft3 potrebbe essere = 'T'
116900020304     c* (spedizione parziale) ma se entro qua lo devo clearare
117000020304     C                   clear                   ARBFT3
117100970319     C                   EXCEPT    AGGFT3
117200020225    4C                   ENDIF
117300020225      *
117400020225    3c                   ENDIF
117500020225    2c                   ENDIF
117600020225     C     KARB55        READE     FNARB55L
117700961120    1C                   ENDDO
117800961112     C*
117900140311      *
118000140311      *  Solo per DPD nuovo tracciato GEOPOST, se è passato dal Dettaglio
118100140311      *  generando il file, il flag è impostato a "D" altrimenti sarebbe ancora
118200140311      *  impostato a "T" come da inizializzazione.
118300140311      *   A questo punto deve chiudere il file che ha aperto e scritto in Dettaglio
118400140311     C                   if            flagHDE ='D'
118500140311      **
118600140311      ** il flag HDE ha funzionato come sintesi del concetto che il programma
118700140311      **  è stato richiamato per fare un Manifest x DPD ed inoltre in modalità
118800140311      **  GEOPOST (altrimenti non varrebbe "D" in nessun altro caso)
118900140311     C                   movel     'E'           flagHDE
119000140311     C                   clear                   KPJBU
119100140311     C                   call      'TRTC83R13'
119200140311     C                   PARM                    KPJBA
119300140311     C                   PARM                    flagHDE
119400140311      **
119500140513      * REIMPOSTO KPJBU "ORIGINALE"
119600140513     C                   MOVEL     WKPJBU        KPJBU
119700140513      *
119800140311     c                   end
119900140311      *
120000961112     C                   ENDSR
120100090402     C*****************************************************************
120200090402     C*    Aggiorna data manifest in filiale e sede                   *
120300090402     C*****************************************************************
120400090402     c     AggDataM      BEGSR
120500090402     c                   setoff                                       818283
120600090402     c                   setoff                                       8485
120700090402     c                   clear                   dta4k
120800090402     c*
120900090402     c
121000090402     c* Anche in sede
121100090402     c                   movel     'K'           ktrc
121200090402     c     kar4          chain     tita430c
121300090402    1c                   if        %found(tita430c)
121400090402     c                   movel     ta4not        dta4k
121500090402   x1c                   else
121600090402     c
121700120823     c     karB          chain     titas30c
121800090402     c                   clear                   ta4not
121900090402    1c                   endif
122000090402     c
122100090403     c                   movel     d82dfv        §ta4kdmf
122200090402     c                   movel     dta4k         ta4not
122300090402    1c                   if        %found(tita430c)
122400090402     c   84              update    tita4000
122500090402     c   85              update    tita4p00
122600090402   x1c                   else
122700090403    2c                   if        %found(titas30c)
122800090402     c                   movel     arbaas        ta4aas
122900090402     c                   movel     arblnp        ta4lnp
123000090402     c                   movel     arbnrs        ta4nrs
123100090402     c                   movel     arbnsp        ta4nsp
123200090402     c                   movel     'K'           ta4trc
123300090402     c  n83              write     tita4000
123400090402     c   83              write     tita4p00
123500090402    2c                   endif
123600090402    1c                   endif
123700090402     c
123800090402     C                   ENDSR
123900961114     C*****************************************************************
124000961114     C*    LETTURA DETTAGLIO COLLI                                    *
124100961114     C*****************************************************************
124200961114     C     REDART        BEGSR
124300020225      *
124400961115     C                   CLEAR                   WDTPAR
124500961115     C                   CLEAR                   WFIL
124600961120     C                   CLEAR                   WNPS
124700020225     C                   clear                   WDIS4
124800020225     C                   clear                   XX
124900020225     C                   clear                   YY
125000961218     C                   CLEAR                   NSC
125100020225     c                   eval      *in02 = *off
125200060918     c                   eval      *in01 = *off
125300020225     c                   eval      *in12 = *on
125400020225      *
125500120823     C     KARb          SETLL     FNART01L
125600120823     C     KARb          READE     FNART01L                               31
125700961114    1C     *IN31         DOWEQ     *OFF
125800961121     C*
125900961121     C                   MOVE      ARTFLS        KFLS
126000961121     C                   MOVE      ARTLNA        KLNA
126100961121     C                   MOVE      ARTNRS        KNRS
126200961121     C                   MOVE      ARTNSC        KNSC
126300970116     C*
126400970116    2C     ARTDTR        IFGT      *ZEROS
126500970116     C                   Z-ADD     645           KCAA
126600970116     C                   EXSR      CTRANM
126700970116    3C     *IN09         IFEQ      *OFF
126800060913     c
126900970116     C* MEMORIZZO CHE LA SPEDIZIONE E' DA STAMPARE NEL MANIFEST
127000970116     C                   MOVE      'T'           ARBFT3
127100060913     c
127200970116     C* CONTROLLI PER DISCORDANZA E4-INVIO RECORD:COLLO GIA' INVIATO
127300970116    4C     *IN20         IFEQ      *ON
127400970116     C     D82LDS        OREQ      'SI'
127500970116     C* Controllo che collo non sia nuovamente in partenza (anom.620)
127600970116     C                   Z-ADD     620           KCAA
127700990519     C     KANM2         SETLL     FNANM02L                               31
127800970116    5C     *IN31         IFEQ      *OFF
127900970116    6C     WDIS4         IFEQ      *BLANKS
128000060913     c
128100060918     c* Per INVII anche parziali
128200060918    7c                   if        d82isc=' '
128300060913     c                   eval      kcev='E4'
128400060913     C                   EXSR      GEDIS48
128500060918     c
128600060918     C                   MOVE      '1'           WDIS4
128700060918    7c                   endif
128800060918     c*
128900060918     c* Per INVII solo totali o forzati
129000060918    7c                   if        d82isc='S'
129100060918     c
129200060918     c* 1° giro: Memorizzo che devo creare l'anomalia
129300060918    8c                   if        wcreadis=' '
129400060918     c
129500060918     c*  Se bolla abilitata la differenza la fa l'IMA
129600060918    9c                   if        arbama>1
129700060918     c
129800060918     c* Se non c'e' ancora nulla devo creare la E8
129900060918     c                   if        creae8=' '
130000060918     c                   eval      creae8='S'
130100060918     c                   endif
130200060918     c* Se non devo creare la S8 allora memorizzo il persiste E7A
130300060918     c                   if        creae8='N'
130400060918     c                   eval      creae7x='S'
130500060918     c                   endif
130600060918   x9c                   else
130700060918     c
130800060918     c* scrivo il persiste manca invio record
130900060918     c                   eval      creae7x='S'
131000060918    9c                   endif
131100060918     c*
131200060918   x8c                   else
131300060918     c* 2° giro: stampo un base alle momrizzazioni per giro precedente
131400060918    9c                   if        creaE8='S'
131500060918     c                   eval      kcev='E8'
131600060918     C                   EXSR      GEDIS48
131700060918     C                   MOVE      '1'           WDIS4
131800060918    9c                   endif
131900060918     c
132000060915     c* scrivo il persiste manca invio record
132100060918    9c                   if        creaE7x='S'
132200060915     c                   exsr      CTRE7X
132300060918    9c                   endif
132400060918    8c                   endif
132500060918    7c                   endif
132600060913     c
132700970116    6C                   END
132800970116     C*
132900970116     C* STAMPA RIGA SEGNACOLLI GIA' INVIATI
133000060918     c*  per la E4 o la E8 al 2° giro se creata
133100970116    6C     D82LDS        IFEQ      'SI'
133200060918     c
133300060918   6ac                   if        d82isc=' ' or
133400060918     c                             (D82isc='S' and wcreadis='S' and
133500060918     c                              creae8='S')
133600060915     c
133700020225    7C     XX            IFGE      13
133800060918     C                   MOVE      ','           NSC(XX)
133900060918     c                   exsr      Stagiainv
134000060918     c
134100970116     C                   SETOFF                                       12
134200020225     C                   MOVE      *ZEROS        XX
134300970116     C                   CLEAR                   NSC
134400060918    7C                   ENDif
134500970116     C* Memorizzo in schiera di stampa il numero segnacollo
134600020225     C                   ADD       1             XX
134700970116     C* Richiamo routine per abblencare zeri non significativi
134800970116     C                   MOVE      ARTNSC        W007A             7
134900970116     C                   MOVEA     W007A         K08
135000970116     C                   EXSR      ABBL
135100020225     C                   MOVEA     K08           NSC(XX)
135200970116     C* Metto "," nell'elemento di schiera precedente
135300020225    7C     XX            IFGT      1
135400020225     C     XX            SUB       1             YY
135500020225     C                   MOVE      ','           NSC(YY)
135600970116    7C                   END
135700060915   6aC                   END
135800060918     c
135900060915    6C                   END
136000970116    5C                   END
136100970116    4C                   END
136200970116    3C                   END
136300020225      ***
136400970116   X2C                   ELSE
136500990412      * SE HO  COLLO ARRIVATO MA NON SPARATO FORZO L'ARRIVO   Solo se no bis
136600990412     C                   CLEAR                   WARFOR
136700001127     C**********
136800001127     C***  ASTERISCATO PERCHÈ NON SI VUOLE PIÙ LA GENERAZIONE DEL MANIFEST
136900001127     C***  CON PARTENZE SPARATE SOLO DA FILIALI DI PARTENZA.
137000001127     C***  QUESTO ERA STATO ATTIVATO PRECEDETEMENTE IL 12/04/99
137100001127     C***  ORA SI È TORNATI INDIETRO.(VEDERE NOTE DI ND)
137200001127     C**********
137300001127    3C********** ARTDAM    IFEQ *ZEROS
137400001127     C********** *IN22     ANDEQ*OFF
137500001127     C**********           EXSR ARFORZ
137600001127    3C**********           ENDIF
137700001127     C***
137800160323     c* se si tratta di una bolla dal 16 al 21/3 del cliente 1663286   serie 20
137900160323     c*  forzo l'invio
138000160323     c                   if        artnrs=20 and  arbksc=1163286 and
138100160323     c                             artaas=2016 and arbmgs>=0316 and arbmgs<=0321
138200160323     c                   eval      warfor='S'
138300160323     c                   eval      arbama=3
138400160323     c                   endif
138500160323     c
138600990412     C* COLLO NON INVIATO MA ARRIVATO O CON ARRIVO FORZATO
138700970116    3C     ARTDAM        IFGT      *ZEROS
138800970116     C     ARTDAM        ANDLE     D82DFV
138900990412     C     WARFOR        OREQ      'S'
139000990412      *
139100970117    4C     *IN22         IFEQ      *OFF
139200970116     C* NO BIS: VERIFICO SE COLLO A TERRA
139300970116     C                   Z-ADD     645           KCAA
139400970116     C                   EXSR      CTRANM
139500970116   X4C                   ELSE
139600970116     C* SI BIS: VERIFICO SE IN PARTENZA
139700970117     C                   Z-ADD     1             KNPG
139800070123     C     KBRV1         SETLL     FNBRV01L                               31
139900020225     c   31              eval      *in09 = *off
140000020225     c  N31              eval      *in09 = *on
140100970116    4C                   END
140200970116     C* 09 OFF --> LA SPEDIZIONE E' DA STAMPARE NEL MANIFEST
140300970116    4C     *IN09         IFEQ      *OFF
140400060913     c
140500970116     C                   MOVE      'T'           ARBFT3
140600060913     c
140700970116   X4C                   ELSE
140800060918     c
140900060918     C* SE IL COLLO NON PARTE in IMA, ACCENDO INDICATORE DI PARTIAL
141000060918     c* In questo caso non devo creare l'anomalia E8 ma un persiste
141100060918     c                   if        creae8='S'
141200060918     c                   eval      creae7x='S'
141300060918     c                   endif
141400060918     c*
141500060918     c                   eval      creae8='N'
141600060918     c
141700020225     C                   eval        *in02 = *on
141800060918     C                   eval        *in01 = *on
141900970116    4C                   END
142000970116   X3C                   ELSE
142100970116     C* SE IL COLLO NON E' ARRIVATO ACCENDO INDICATORE DI PARTIAL
142200020225     C                   eval        *in02 = *on
142300970116    3C                   END
142400970116    2C                   END
142500020225      *
142600020225      * SE NON DEVO NE' STAMPARE NE' CREARE LE DISCORDANZE E AI FINI
142700020225      * DEL MANIFEST HO GIA' RILEVATO QUELLO CHE MI SERVIVA (FT3='T'
142800020225      * E 02 ON) ESCO DAL CICLO
142900970110     C     *IN20         IFEQ      *OFF
143000970110     C     D82LDS        ANDEQ     'NO'
143100970110     C     *IN02         ANDEQ     *ON
143200970110     C     ARBFT3        ANDEQ     'T'
143300970110     C                   SETON                                        31
143400970110     C                   ELSE
143500120823     C     KARb          READE     FNART01L                               31
143600970110     C                   END
143700020225      *
143800961119    1C                   ENDDO
143900961129     C*
144000961127     C* Stampa elenco segnacolli a fine ciclo
144100060918    1C     NSC(1)        IFNE      *BLANKS
144200060918     c                   exsr      STAGIAINV
144300060918     c                   endif
144400060918     c
144500961218     C* stampo riga vuota per distanziarla
144600961127     C* dalla prossima discordanza che verrà eventualmente stampata
144700060918    1C     WDIS4         IFEQ      '1'
144800970114     C     D82LDS        ANDEQ     'SI'
144900120821    2c     d82ftt        ifne      FEDEX
145000040423
145100060918    3c                   if        FAX_Disc_Est = 'S'
145200970115     C   20              EXCEPT    FIT
145300060918    3c                   end
145400040423
145500060918    2c                   endif
145600970114     C                   EXCEPT    FITC
145700060918    1C                   END
145800961114     C*
145900961114     C                   ENDSR
146000060918     *** *****************************************************************
146100060918     *** * stampa riga segnacolli già inviati
146200060918     *** *****************************************************************
146300060918     c     STAGIAINV     BEGSR
146400060918     C* Stampo riga con elenco segnacolli
146500060918    8C   20*INOB         IFEQ      *ON
146600120821     c     d82ftt        andne     FEDEX
146700060918
146800060918     c                   if        FAX_Disc_Est = 'S'
146900060918     C                   ADD       1             WPAG
147000060918     C                   EXCEPT    TESDIS
147100060918     c                   end
147200060918
147300060918    8C                   END
147400060918     c
147500060918    8C     *INOE         IFEQ      *ON
147600060918     C                   ADD       1             WPAG1
147700060918     C                   EXCEPT    TESDIC
147800060918    8C                   END
147900060918     c
148000120821    8c     d82ftt        ifne      FEDEX
148100060918    9c                   if        FAX_Disc_Est = 'S'
148200060918     C   20              EXCEPT    RIGCNS
148300060918    9c                   end
148400060918    8c                   endif
148500060918     c
148600060918     C                   EXCEPT    RIGCNC
148700060918     c
148800060918     c                   ENDSR
148900961213     C*****************************************************************
149000961213     C* STAMPA MANIFEST ----------------------------------------------*
149100961213     C*****************************************************************
149200961213     C     PRTMAN        BEGSR
149300961213     C*
149400961213     C                   CLEAR                   MITCPI
149500961213     C                   CLEAR                   DESCPI
149600961220     C                   CLEAR                   STATIC
149700061127     C                   CLEAR                   STACNA           35
149800061127     C                   CLEAR                   STACTE           25
149900961213     C*
150000961213     C* ACCORCIO LA LUNGHEZZA DI ALCUNI CAMPI
150100961213     C                   Z-ADD     ARBRMN        STARMN            9 0
150200961213     C                   MOVEL     ARBRSM        STARSM           25
150300130808     C                   MOVEL     ARBINM        STAINM           25
150400961213     C                   MOVEL     ARBRSD        STARSD           25
150500961213     C                   MOVEL     ARBLOM        STALOM           20
150600961213     C                   MOVEL     ARBIND        STAIND           25
150700961213     C                   MOVEL     ARBLOD        STALOD           20
150800961213     C*
150900970110     C* SE DEVO STAMPARE SOLO IL MANIFEST SINTETICO NON OCCORRE FARE
151000970110     C* LE SEGUENTI OPERAZIONI
151100970110     C     *IN20         IFEQ      *ON
151200970110     C     D82DGN        OREQ      'SI'
151300120821     c     d82ftt        andne     FEDEX
151400970110     C     D82COD        OREQ      'SI'
151500970110     C     §3AFCA        ANDGT     0
151600120821     c     d82ftt        andne     FEDEX
151700061127      *
151800061127      * Prendo i dati del Contatto Nome e Telefono
151900061127     c     KAR5          chain     fiar501l                           33
152000061127     c                   if        %Found(fiar501l)
152100061127     c                   Movel     Ar5Uni        DAr5Gen
152200061127     c                   movel     §ar5ref       STACNA
152300061127     c                   movel     §ar5teld      STACTE
152400061127      * se i campi sono vuoti non deve stamparli..... Accende quindi il 33
152500061127      *   come se non avesse trovato il record.
152600061127     c                   if        STACNA=*blank and STACTE=*blank
152700061127     c                   seton                                        33
152800061127     c                   end
152900061127     c                   end
153000130215      ***
153100970110     C* Prendo i dati del c/assegno
153200120823     C     KARb          CHAIN     FIAR901L                           34
153300130215      ***
153400961220     C* Reperisco decodifica tipo incasso contrassegno
153500961220     C     *IN34         IFEQ      *OFF
153600041109     c                   z-add     ar9cas        prtcas            9 3
153700130215      *
153800961220     C                   MOVEL     '1A'          KECOD
153900961220     C                   MOVEL(P)  AR9TIC        KEKEY
154000130215      ***
154100130215     c                   z-add     1             tentativi         3 0
154200130215      * prima ci prova se c'è un suffisso particolare
154300130215      *   quindi preimposta anche la possibilità di cercare 2 volte
154400130215     c                   if        §pzSUFFtab <> *blank
154500130215     C                   MOVE      §pzSUFFtab    KEKEY
154600130215     c                   z-add     2             tentativi
154700130215     c                   end
154800130215      *
154900130215     C                   CLEAR                   edids1a
155000130215      *** può fare max 2 tentativi:
155100130215     c                   do        tentativi
155200130215     C     KETAB         CHAIN     EDTAB01L
155300130215     c                   if        %Found(EDTAB01L)
155400130215     C                   MOVEL     TABUNI        edids1a
155500961220     C                   MOVEL     §1ADES        STATIC
155600130215      * se ha trovato esce dopo aver impostato il campo per la stampa
155700130215     c                   LEAVE
155800130215     c                   else
155900130215      * serve x il secondo giro senza suffisso
156000130215     C                   MOVE      *blank        Due_Blanks        2
156100130215     C                   MOVE      Due_Blanks    KEKEY
156200130215     c                   end
156300130215      ***
156400130215     c                   EndDO
156500130215      ***
156600961220     C                   END
156700970110     C* Prendo le note
156800961213     C                   MOVEL     '8'           KTRC                           *NOTE1
156900060213     C     KAR4          CHAIN     fiar401L                           35
157000961213     C  N35              MOVEL     AR4NOT        STANT1
157100961213     C                   MOVEL     '9'           KTRC                           *NOTE2
157200060213     C     KAR4          CHAIN     fiar401L                           36
157300961213     C  N36              MOVEL     AR4NOT        STANT2
157400961213     C*
157500961213     C                   MOVEL     'P'           KTRC                           *P.IVA
157600060213     C     KAR4          CHAIN     fiar401L                           37
157700961213     C  N37              MOVEL     AR4NOT        MITCPI           16
157800961213     C  N37              MOVE      AR4NOT        DESCPI           16
157900130808
158000130808      * se non c'è partita IVA del mittente cerco il codice fiscale del mittente
158100130808     c                   IF        MITCPI = *blanks
158200130808     c                   eval      ktrc = 'Q'
158300130808     c     kar4          chain     fiar401l                           37
158400130808     c  N37              movel     AR4NOT        MITCPI
158500130808     c                   ENDIF
158600100223     C*
158700100223     C*  ragione sociale aggiuntiva di altri 35 + i primi 10 con il finale della
158800100223     C*  ragione sociale destinatario.
158900100223     C                   clear                   RagAdd45         45
159000100223     c                   eval      RagAdd45 = %subst(arbRSD:26:10)
159100100223      *  in più se c'è anche la parte addizionale
159200120821     C                   MOVEL     'D'           KTRC                           *Ragione Sociale
159300100223     C     KAR4          CHAIN     fiar401L                           23         aggiuntiva
159400100223     C  N23              MOVE      AR4NOT        RagAdd45
159500100223     C*
159600100223     C                   clear                   IndAdd10         10
159700100223     c                   eval      IndAdd10 = %subst(arbIND:26:10)
159800100223     C*
159900100224     C                   clear                   CitAdd15         15
160000100224     c                   eval      CitAdd15 = %subst(arbLOD:21:15)
160100130808      * Altri 10 caratteri per l'indirizzo del mittente
160200130808     C                   clear                   IndMAdd10        10
160300130808     c                   eval      IndMAdd10 = %subst(arbINM:26:10)
160400100223     C*
160500100223     C*  stampa righe aggiuntive
160600100223     c                   setoff                                       242526
160700130808     c                   eval      *in27 = *off
160800100223     c                   if        RagAdd45 <> *blank
160900100223     c                   setoN                                        24
161000100223     c                   end
161100961213     C*
161200100223     c                   if        IndAdd10 <> *blank
161300100223     c                   setoN                                        25
161400100223     c                   end
161500100223     C*
161600100224     c                   if        CitAdd15 <> *blank
161700100223     c                   setoN                                        26
161800100223     c                   end
161900130808      *
162000130808     c                   IF        IndMAdd10 <> *blank
162100130808     c                   eval      *in27 = *on
162200130808     c                   ENDIF
162300100223     C*
162400970110     C* data spedizione
162500961213     C                   Z-ADD     ARBDSP        DATA
162600961213     C                   Z-ADD     MM            GGMM
162700961213     C                   MOVEL     GG            GGMM
162800961213     C                   Z-ADD     AA            STADSP
162900961213     C                   MOVEL     GGMM          STADSP
163000970110     C* numero spedizione compattato e non
163100961213     C                   MOVE      ARBLNP        §LB2              2 0
163200961213     C                   MOVE      ARBNSP        §LB8              8 0
163300961213     C                   MOVEL     §LB2          §LB8
163400961213     C                   MOVEL     ARBNRS        §LB9
163500961213     C                   MOVE      ARBNSP        §LB9              9 0
163600961213     C                   MOVEL     ARBLNP        §LB12            12 0
163700961213     C                   MOVE      §LB9          §LB12            12 0
163800100330     C*
163900100330     C                   MOVE      ARBAAS        §LB2
164000100330     C                   MOVEL     §LB2          §LB5              5
164100100330     C                   MOVE      ARBLNP        §LB5
164200100330     C                   MOVE      §LB9          §LB14            14 0
164300100330     C                   MOVEL     §LB5          §LB14
164400970110     C*
164500970110     C                   END
164600961213     C*
164700961213     C* STAMPA MANIFEST PER PARTNER
164800970110     C   20
164900970110     CAN OF
165000970110     CORN20
165100970110     CAN OCD82STB        IFEQ      'SI'
165200961213     C* 04 on - stampa qualcosa nel manifest
165300961213     C                   SETON                                        04
165400041109     c                   if        Email_PTN = *blank or
165500041109     c                             PRT_PTN_tes = *blank
165600041109     c                   eval      PRT_PTN_tes = 'S'
165700970110     C   20              EXCEPT    TESTA
165800041109     c                   end
165900970110     C  N20              EXCEPT    TESSIN
166000970110     C                   SETOFF                                       OFOC
166100970110     C                   ENDIF
166200961213     C*
166300961213     C* STAMPA MANIFEST PER DOGANA
166400961213     C     D82DGN        IFEQ      'SI'
166500120821     c     d82ftt        andne     FEDEX
166600961213     C     *INOD         ANDEQ     *ON
166700961213     C* 06 on - stampa qualcosa nel manifest
166800961213     C                   SETON                                        06
166900041109     c                   if        Email_DGN = *blank or
167000041109     c                             PRT_DGN_tes = *blank
167100041109     c                   eval      PRT_DGN_tes = 'S'
167200961213     C                   EXCEPT    TESDGN
167300041109     c                   end
167400961213     C                   SETOFF                                       OD
167500961213     C                   ENDIF
167600961213     C*
167700961213     C     D82STB        IFEQ      'SI'
167800970110     C   20              EXCEPT    DET
167900970110     C  N20              EXCEPT    DETSIN
168000021125      * Stampa il Valore Merce Dichiarato solo su Manifest Sintetico Provvisorio
168100021204     c   20              if        arbvmd <> 0
168200021204     C                   except    VMDdet
168300021125     c                   end
168400961213     C                   END
168500961213     C*
168600961213     C     D82DGN        IFEQ      'SI'
168700120821     c     d82ftt        andne     FEDEX
168800961213     C                   EXCEPT    DETDGN
168900961213     C                   END
169000961213     C*
169100961213     C* STAMPA MANIFEST DEI C.O.D.
169200961213     C     D82COD        IFEQ      'SI'
169300961213     C     §3AFCA        ANDGT     0
169400120821     c     d82ftt        andne     FEDEX
169500961213     C*
169600961213     C     *INOG         IFEQ      *ON
169700961213     C* 05 On - Stampa qualcosa nel manifest
169800961213     C                   SETON                                        05
169900041109     c                   if        Email_COD = *blank or
170000041109     c                             PRT_COD_tes = *blank
170100041109     c                   eval      PRT_COD_tes = 'S'
170200961213     C                   EXCEPT    TESCOD
170300041109     c                   end
170400961213     C                   SETOFF                                       OG
170500961213     C                   ENDIF
170600961213     C*
170700961213     C                   ADD       1             CODSPE            5 0
170800961213     C                   ADD       ARBNCL        CODNCL            7 0
170900961213     C                   ADD       PESPRT        CODPES           10 1
171000030318     C*
171100030318     C*  questa subroutine non dovrebbe essere mai chiamata per Fedex
171200030318     C*   ma è stato mantenuto il test sul volume per rispettare le logiche
171300030318     C*     applicate alla Fedex.......non si sa mai ......
171400150825     c*********          if        d82ftt = FEDEX
171500150825     C                   ADD       wvol          CODVLF            7 3
171600150825     c*********          else
171700150825     C*********          ADD       ARBVLF        CODVLF            7 3
171800150825     c*********          end
171900961213     C                   EXCEPT    DETCOD
172000961213     C                   END
172100961213     C*
172200961213     C                   ENDSR
172300961119     C*****************************************************************
172400060913     C* CREAZIONE EVENTO E4 o E8 + STAMPA
172500961119     C*****************************************************************
172600060913     C     GEDIS48       BEGSR
172700961127     C*
172800060913     C******             MOVE      'E4 '         KCEV
172900970117     C****
173000970117     C* STAMPO LA DISCORDANZA
173100970117     C****
173200970117     C     D82LDS        IFEQ      'SI'
173300970117     C                   MOVE      ARBAAS        WAAS
173400970117     C                   MOVE      ARBLNP        WLNP
173500970117     C                   MOVE      ARBNSP        WNSP
173600970117     C                   EXSR      PRTDIS
173700970117     C                   END
173800961120     C*
173900961120     C****
174000961120     C* SE ELABORAZIONE DEFINITIVA CREO EVENTO
174100961120     C****
174200961205    1C     *IN20         IFEQ      *ON
174300961120     C                   CLEAR                   FNEVS1
174400961120     C*
174500961120     C                   MOVE      ARBAAS        EVSAAS
174600961120     C                   MOVE      ARBLNP        EVSLNP
174700961120     C                   MOVE      ARBNRS        EVSNRS
174800961120     C                   MOVE      ARBNSP        EVSNSP
174900961120     C                   MOVE      ARBLNA        EVSLNA
175000961120     C*
175100961120     C                   CLEAR                   EVSFLS
175200961120     C                   CLEAR                   EVSNSC
175300961120     C*
175400961212     C                   MOVE      D82DFV        EVSDEV
175500961120     C                   MOVE      KCEV          EVSCEV
175600961120     C*
175700961120     C                   WRITE     FNEVS1
175800961120    1C                   END
175900961119     C*
176000961119     C                   ENDSR
176100961113     C*****************************************************************
176200961113     C* VERIFICA DISCORDANZE
176300961113     C*****************************************************************
176400961113     C     CTRDIS        BEGSR
176500961115     C*
176600961115     C                   CLEAR                   WDTPAR
176700961115     C                   CLEAR                   WFIL
176800961121     C*----------------------------------------------------------------
176900961120     C* CONTROLLO DISCORDANZE 2/2A - COLLO INOLTRATO AL PARTNER SENZA
177000961120     C*                              RECORD BOLLA
177100961121     C*----------------------------------------------------------------
177200020301     c* Per Fedex il giro sui manca record non deve essere fatto
177300120821     c     D82FTT        ifne      FEDEX
177400020301     c*
177500961115     C                   SETOFF                                       43
177600961121     C* LEGGO ANOMALIE 10-MANCA RECORD BOLLA ARR., APERTE, PER LNA
177700961113     C                   Z-ADD     10            KCAA
177800990519     C     KANM10        SETLL     FNANM10L
177900990519     C     KANM10        READE(N)  FNANM10L                               31
178000961114    1C     *IN31         DOWEQ     *OFF
178100970121     C* CONSIDERO SOLO LE APERTE OPPURE LE CHIUSE CON CCH = 'PR'
178200970121    2C     ANMDAO        IFLE      D82DFV
178300970121     C     ANMDCH        ANDEQ     *ZEROS
178400970121     C     ANMDAO        ORLE      D82DFV
178500970121     C     ANMDCH        ANDGT     *ZEROS
178600970121     C     ANMCCH        ANDEQ     'PR'
178700961203     C* ESCLUDO LOCALI
178800970117    3C     ANMLNP        IFGT      *ZEROS
178900961203     C     ANMLNP        CABEQ     ANMLNA        REDA10
179000970117   X3C                   ELSE
179100961203     C     ANMFLS        CABEQ     ANMLNA        REDA10
179200970117    3C                   END
179300970117     C* VERIFICO SE COLLO E' IN PARTENZA
179400970117     C* Se fv no bis verifico se collo a terra
179500961120     C                   MOVE      ANMFLS        KFLS
179600961120     C                   MOVE      ANMLNA        KLNA
179700961120     C                   MOVE      ANMNRS        KNRS
179800961120     C                   MOVE      ANMSCN        KNSC
179900970117    3C     *IN22         IFEQ      *OFF
180000961122     C                   Z-ADD     645           KCAA
180100961122     C                   EXSR      CTRANM
180200970117   X3C                   ELSE
180300970117     C* Se fv bis verifico se sparato in defluenza su questo fv
180400970117     C                   EXSR      CHKBRV
180500970117    3C                   END
180600970117     C* 09 OFF --> COLLO IN PARTENZA
180700961120    3C     *IN09         IFEQ      *OFF
180800961120     C* VERIFICO ESISTENZA DISCORDANZA 2-COLLO INOLTRATO NEL GIORNO
180900961120     C*                                  SENZA BOLLA
181000961113     C                   MOVE      'E2 '         KCEV
181100961121     C     KEVS2         SETGT     FNEVS02L
181200961127     C     KEVS2         READPE    FNEVS02L                               31
181300970117    4C     *IN31         IFEQ      *ON
181400961121     C* CREO E STAMPO DISCORDANZA 2
181500060913     C                   EXSR      GEDIS27
181600970117   X4C                   ELSE
181700970117   4AC     EVSDEV        IFLT      D82DFV
181800970117     C     *IN22         ANDEQ     *OFF
181900961121     C* ESISTE EVENTO 2 CREATO IN UN GIORNO PRECEDENTE --> MEMORIZZO LA
182000961129     C* DATA E LA PISTOLA
182100961120     C                   MOVE      EVSDEV        WDEV
182200040415     C                   MOVE      EVSDEV        WDEV2a
182300961129     C                   MOVE      EVSNPS        WNPS
182400961121     C* CERCO ANCHE SE ESISTE EVENTO 2A
182500961120     C                   MOVE      'E2A'         KCEV
182600970120     C* SE TROVO E2A IN DATA DEL FOGLIO NON FACCIO NIENTE
182700120823     C     KEVS2A        SETLL     FNEVS02L                               31
182800970120     C     *IN31         CABEQ     *ON           REDA10
182900961120     C     KEVS2         SETGT     FNEVS02L
183000961127     C     KEVS2         READPE    FNEVS02L                               31
183100961121    5C     *IN31         IFEQ      *OFF
183200970117     C* TENGO LA DATA EVENTO MAGGIORE FRA EVENTO E2 ED EVENTO E2A
183300970117    6C     EVSDEV        IFGT      WDEV
183400961120     C                   MOVE      EVSDEV        WDEV
183500970117    6C                   END
183600970117    5C                   END
183700961121     C* CERCO SE ESISTE EVENTO 6-COLLO DATO PER INOLTRATO MA A TERRA
183800961121     C* IN DATA > WDEV
183900961121     C                   MOVE      'E6 '         KCEV
184000961121     C     KEVS2B        SETGT     FNEVS02L
184100961127     C     KEVS2         READE     FNEVS02L                               31
184200961121    5C     *IN31         IFEQ      *OFF
184300970120     C     EVSDEV        ANDLT     D82DFV
184400961121     C                   MOVE      'E2 '         KCEV
184500060913     C                   EXSR      GEDIS27
184600961121   X5C                   ELSE
184700961121     C* CREO EVENTO E2A SOLO SE IL COLLO NON E' IN PARTENZA OGGI
184800961121     C                   Z-ADD     620           KCAA
184900990519     C     KANM2         SETLL     FNANM02L                               31
185000970122     C  N31              MOVE      'E6B'         KCEV
185100120823     C  N31KEVS2A        SETLL     FNEVS02L                               31
185200961121    6C     *IN31         IFEQ      *OFF
185300040419     c* Memorizzo, se esiste, data discordanza E6B se > data E2 più recente
185400040419     c     kevs2         readpe    fnevs02l                               31
185500040419     c     *in31         ifeq      *off
185600040419     c     evsdev        andgt     wdev2a
185700040419     c                   eval      wdev2a=evsdev
185800040419     c                   endif
185900040415     c* creo la discordanza solo se non sono passati più di x giorni
186000040415     c* dalla data dell'E2/E6B più recente
186100040416     c     *iso          Movel     wdev2a        wdta
186200040416     c     *iso          Movel     d82dfv        wdta1
186300040416     c                   Adddur    §7agde:*D     wdta
186400040416     c     wdta          ifge      wdta1
186500961121     C                   MOVE      'E2A'         KCEV
186600060913     C                   EXSR      GEDIS27
186700040416     c                   endif
186800961121    6C                   END
186900961121    5C                   END
187000970117   4AC                   END
187100970117    4C                   END
187200961120    3C                   END
187300961120    2C                   END
187400961129     C     REDA10        TAG
187500961128     C                   Z-ADD     10            KCAA
187600990519     C     KANM10        READE(N)  FNANM10L                               31
187700961120    1C                   ENDDO
187800020301     c*
187900020301     c                   endif
188000961120     C*
188100961121     C*----------------------------------------------------------------
188200961114     C* LEGGO COLLI DELLE BOLLE CON DTR = 0 E:
188300961114     C* - AGGIORNO DTR SE COLLO ARRIVATO E NON E' A TERRA
188400961114     C* - RILEVO DISCORDANZE 1, 1A, 3, 5, 5A
188500961121     C*----------------------------------------------------------------
188600961115     C                   SETOFF                                       42
188700961114     C                   MOVE      *ZEROS        KDTR
188800961114     C     KART55        SETLL     FNART55L
188900961114     C   20KART55        READE     FNART55L                               31
189000961114     C  N20KART55        READE(N)  FNART55L                               31
189100961114    1C     *IN31         DOWEQ     *OFF
189200970627     C* ESCLUDO COLLI LOCALI
189300961203     C     ARTLNP        IFEQ      ARTLNA
189400961203     C   20              UNLOCK    FNART55L
189500961203     C                   GOTO      LEG55
189600961203     C                   END
189700961114     C*
189800961115     C                   CLEAR                   WFIL
189900961115     C                   CLEAR                   WCODIS
190000961120     C                   CLEAR                   WNPS
190100961120     C                   CLEAR                   WFNPS
190200961114     C*
190300961114     C* A CAMBIO SPEDIZIONE CHAINO FNARB
190400961114     C                   MOVEL     ARTAAS        AAS
190500961114     C                   MOVEL     ARTLNP        LNP
190600961114     C                   MOVEL     ARTNRS        NRS
190700961114     C                   MOVEL     ARTNSP        NSP
190800961114    2C     ARTSPE        IFNE      SAVSPE
190900970124     C     KARB1         CHAIN(N)  FNARB01L                           31
191000961114     C                   MOVE      ARTSPE        SAVSPE
191100961114    2C                   END
191200990412      * SE HO  COLLO ARRIVATO MA NON SPARATO FORZO L'ARRIVO   Solo se no bis
191300990412     C                   CLEAR                   WARFOR
191400160323     c* se si tratta di una bolla dal 16 al 21/3 del cliente 1663286   serie 20
191500160323     c*  forzo l'invio
191600160323     c                   if        artnrs=20 and  arbksc=1163286 and
191700160323     c                             artaas=2016 and arbmgs>=0316 and arbmgs<=0321
191800160323     c                   eval      warfor='S'
191900160323     c                   eval      arbama=3
192000160323     c                   endif
192100961114     C*
192200961114    2C                   SELECT
192300990412     C* C O L L O   N O N   A R R I V A T O  e non forzato l'arrivo
192400970116     C* O ARRIVATO IN DATA > ALLA DATA DEL FOGLIO DI DEFLUENZA (SOLO SE
192500970116     C* NO BIS)
192600961114     C     ARTDAM        WHENEQ    *ZEROS
192700990412     C     WARFOR        ANDEQ     *BLANKS
192800970117     C     *IN22         ANDEQ     *OFF
192900961224     C     ARTDAM        ORGT      D82DFV
193000970117     C     *IN22         ANDEQ     *OFF
193100961121     C   20              UNLOCK    FNART55L
193200961114     C* - RELATIVO A SPEDIZIONE DEL MANIFEST DEL GIORNO --> DISC. 1A
193300961212    3C     ARBDT3        IFEQ      D82DFV
193400961114     C     ARBDT3        OREQ      *ZEROS
193500961114     C     ARBFT3        ANDEQ     'T'
193600961115     C                   MOVE      'E1A'         WCODIS
193700961114   X3C                   ELSE
193800961114     C* - RELATIVO A SPEDIZIONE DI MANIFEST PRECEDENTE ---> DISC. 5A
193900961114    4C     ARBDT3        IFGT      *ZEROS
194000961212     C     ARBDT3        ANDLT     D82DFV
194100040416     c* creo la discordanza solo se non sono passati più di x giorni
194200040416     c* dalla data del'invio del record bolla
194300040416     c     *iso          Movel     arbdt3        wdta
194400040416     c     *iso          Movel     d82dfv        wdta1
194500040416     c                   Adddur    §7agde:*D     wdta
194600040416     c     wdta          ifge      wdta1
194700961115     C                   MOVE      'E5A'         WCODIS
194800040416     c                   endif
194900961114    4C                   END
195000961114    3C                   END
195100961120     C*
195200961115    3C     WCODIS        IFNE      *BLANKS
195300961127     C                   MOVE      WCODIS        KCEV
195400961127     C                   MOVE      ARTFLS        KFLS
195500961127     C                   MOVE      ARTLNA        KLNA
195600961127     C                   MOVE      ARTNRS        KNRS
195700961127     C                   MOVE      ARTNSC        KNSC
195800961127     C                   MOVE      ARTAAS        WAAS
195900961127     C                   MOVE      ARTLNP        WLNP
196000961127     C                   MOVE      ARTNSP        WNSP
196100970117     C* STAMPO DISCORDANZA
196200970117     C     D82LDS        IFEQ      'SI'
196300970117     C                   EXSR      PRTDIS
196400970117     C                   END
196500961114     C* SE ELABORAZIONE DEFINITIVA CREO L'EVENTO
196600961127     C   20              EXSR      CRTEVS
196700961114    3C                   END
196800961114     C*
196900961114     C* C O L L O    A R R I V A T O
197000990412     C* o forzato l'arrivo (solo se no BIS)
197100961212     C     ARTDAM        WHENLE    D82DFV
197200990412     C     WARFOR        OREQ      'S'
197300961115     C                   MOVE      ARTFLS        KFLS
197400961115     C                   MOVE      ARTLNA        KLNA
197500961115     C                   MOVE      ARTNRS        KNRS
197600961115     C                   MOVE      ARTNSC        KNSC
197700970116     C*
197800970117   3AC     *IN22         IFEQ      *OFF
197900961122     C                   Z-ADD     645           KCAA
198000961115     C                   EXSR      CTRANM
198100961115     C* COLLO A TERRA
198200961115    3C     *IN09         IFEQ      *ON
198300961120     C* MEMORIZZO NUMERO PISTOLA
198400961120     C                   MOVE      ANMNPS        WNPS
198500961114     C* - DISCORDANZA 1
198600961212    4C     ARBDT3        IFEQ      D82DFV
198700961114     C     ARBDT3        OREQ      *ZEROS
198800961114     C     ARBFT3        ANDEQ     'T'
198900961114     C                   MOVE      'E1 '         WCODIS
199000961115     C                   MOVE      ANMFLE        WFIL
199100961114   X4C                   ELSE
199200961114     C* - DISCORDANZA 5
199300961114    5C     ARBDT3        IFGT      *ZEROS
199400961212     C     ARBDT3        ANDLT     D82DFV
199500961114     C                   MOVE      'E5 '         WCODIS
199600961115     C                   MOVE      ANMFLE        WFIL
199700961114    5C                   END
199800961114    4C                   END
199900970116    3C                   END
200000970116  X3AC                   ELSE
200100970116     C* SE BIS VERIFICO SE IN PARTENZA
200200970116     C                   EXSR      CHKBRV
200300970116   3AC                   END
200400970116     C*
200500970116    3C     *IN09         IFEQ      *OFF
200600961122    4C     ARBDT3        IFGT      *ZEROS
200700961212     C     ARBDT3        ANDLT     D82DFV
200800970627     C     ARTDCM        ANDEQ     *ZEROS
200900970116     C* E3 - RECORD GIA' INVIATO:INVIO COLLO
201000961114     C                   MOVE      'E3 '         WCODIS
201100970117    4C                   END
201200961224     C* DETERMINO NUMERO PISTOLA E SE ESISTE SPUNTA DI DEFLUENZA
201300970116     C* (SOLO SE NO BIS)
201400970117     C  N22              EXSR      CHKBRV
201500961122    3C                   END
201600060913     C*
201700060913     c*
201800060913     C* COLLO ARRIVATO DA INVIARE MA NON INVIATO record bolla perchè
201900060913     c*  invio parziale
202000060913     c                   if        not *in09 and arbft3=' ' and d82isc='S'
202100060915     c                             and   artdcm=0
202200060913     c* Se sto inviando collo --> disc. E7 altrimenti avviso
202300060913     c* che c'è ancora una parziale (E7A)
202400060913     c                   exsr      CTRE7x
202500060913     c                   endif
202600060913     c
202700060913     c
202800961114    3C     WCODIS        IFNE      *BLANKS
202900961127     C                   MOVE      WCODIS        KCEV
203000961127     C                   MOVE      ARTAAS        WAAS
203100961127     C                   MOVE      ARTLNP        WLNP
203200961127     C                   MOVE      ARTNSP        WNSP
203300970117     C* STAMPO DISCORDANZA
203400970117     C     D82LDS        IFEQ      'SI'
203500970117     C                   EXSR      PRTDIS
203600970117     C                   END
203700961114     C* SE ELABORAZIONE DEFINITIVA CREO EVENTO
203800961127     C   20              EXSR      CRTEVS
203900961114    3C                   END
204000970117     C* SE COLLO IN PARTENZA AGGIORNO ARTDTR
204100970117    3C     *IN09         IFEQ      *OFF
204200961205     C     *IN20         ANDEQ     *ON
204300970627     C     ARTDCM        ANDEQ     *ZEROS
204400120821     c     d82ftt        andne     FEDEX
204500020304     c* per fedex lo deve fare solo se sono arrivati tutti i colli
204600020304    3C     *IN09         oreq      *OFF
204700020304     C     *IN20         ANDEQ     *ON
204800020304     C     ARTDCM        ANDEQ     *ZEROS
204900020304     c     arbft3        andeq     'T'
205000961120     C* RICHIAMO ROUTINE PER AUTOGENERARE SPUNTA PARTENZA SE NON C'E'
205100961115     C* E CHIUDERE EVENTUALI ANOMALIE 145 E 146 DEI GIORNI PRECEDENTI
205200961115     C                   SETON                                        43
205300961115     C                   EXSR      PARCOL
205400961212     C                   MOVE      D82DFV        ARTDTR
205500961114     C                   UPDATE    FNART055
205600961114   X3C                   ELSE
205700961114     C   20              UNLOCK    FNART55L
205800961114    3C                   END
205900961114     C*
206000961114    2C                   ENDSL
206100961203     C*
206200961203     C     LEG55         TAG
206300961203     C*
206400961114     C   20KART55        READE     FNART55L                               31
206500961114     C  N20KART55        READE(N)  FNART55L                               31
206600961114    1C                   ENDDO
206700961121     C*----------------------------------------------------------------
206800961120     C* CONTROLLO DISCORDANZE 6A - COLLO DATO PER INOLTR.MA IN PARTENZA
206900961121     C*----------------------------------------------------------------
207000961115     C                   CLEAR                   WFIL
207100961115     C* LEGGO ANOMALIE 620 PER LNA
207200961115     C                   Z-ADD     620           KCAA
207300990519     C     KANM10        SETLL     FNANM10L
207400990519     C   20KANM10        READE     FNANM10L                               31
207500990519     C  N20KANM10        READE(N)  FNANM10L                               31
207600961121    1C     *IN31         DOWEQ     *OFF
207700961203     C* ESCLUDO LOCALI
207800961203     C     ANMLNP        IFGT      *ZEROS
207900961203     C     ANMLNP        ANDEQ     ANMLNA
208000961203     C     ANMLNP        OREQ      *ZEROS
208100961203     C     ANMFLS        ANDEQ     ANMLNA
208200990519     C   20              UNLOCK    FNANM10L
208300961203     C                   GOTO      LEG620
208400961203     C                   END
208500970121     C*
208600970121     C                   MOVE      ANMFLS        KFLS
208700970121     C                   MOVE      ANMLNA        KLNA
208800970121     C                   MOVE      ANMNRS        KNRS
208900970121     C                   MOVE      ANMSCN        KNSC
209000970121     C*
209100970121     C* SE BIS CERCO SPUNTA DEFLUENZA
209200970117     C   22              EXSR      CHKBRV
209300961203     C*
209400970117     C     *IN22         IFEQ      *ON
209500970117     C     *IN09         ANDEQ     *OFF
209600970117     C     *IN22         OREQ      *OFF
209700961118     C* DETERMINO QUANDO IL COLLO E' STATO DATO PER PARTITO
209800961118     C                   EXSR      RCRDTP
209900961205     C                   EXSR      GEDE6A
210000970117     C                   END
210100961121     C*
210200961203     C     LEG620        TAG
210300961129     C                   Z-ADD     620           KCAA
210400990519     C   20KANM10        READE     FNANM10L                               31
210500990519     C  N20KANM10        READE(N)  FNANM10L                               31
210600961121    1C                   ENDDO
210700961121     C*----------------------------------------------------------------
210800961120     C* CONTROLLO DISCORDANZE 6  - COLLO DATO PER INOLTRATO MA A TERRA
210900961121     C*----------------------------------------------------------------
211000970117     C* LEGGO ANOMALIE 600 PER LNA SOLO SE NO BIS
211100970117   1AC     *IN22         IFEQ      *OFF
211200961115     C                   Z-ADD     600           KCAA
211300990519     C     KANM10        SETLL     FNANM10L
211400990519     C   20KANM10        READE     FNANM10L                               31
211500990519     C  N20KANM10        READE(N)  FNANM10L                               31
211600961121    1C     *IN31         DOWEQ     *OFF
211700961203     C* ESCLUDO LOCALI
211800961203     C     ANMLNP        IFGT      *ZEROS
211900961203     C     ANMLNP        ANDEQ     ANMLNA
212000961203     C     ANMLNP        OREQ      *ZEROS
212100961203     C     ANMFLS        ANDEQ     ANMLNA
212200990519     C   20              UNLOCK    FNANM10L
212300961203     C                   GOTO      LEG600
212400961203     C                   END
212500961203     C*
212600961118     C* DETERMINO DATA IN CUI IL COLLO E' STATO DATO PER PARTITO
212700961121     C                   MOVE      ANMFLS        KFLS
212800961121     C                   MOVE      ANMLNA        KLNA
212900961121     C                   MOVE      ANMNRS        KNRS
213000961121     C                   MOVE      ANMSCN        KNSC
213100961118     C                   EXSR      RCRDTP
213200961205     C*
213300961205     C                   MOVE      ANMFLE        WFIL
213400961205     C                   MOVE      ANMNPS        WNPS
213500961205     C*
213600961205     C                   SETON                                        09
213700961205     C* IL COLLO PUO' NON ESSERE PIU' A TERRA SE L'ANOMALIA 600 CHE
213800961205     C* STO LEGGENDO HA DATA < ALLA DATA DEL FOGLIO DI DEFL. CHE STO
213900961205     C* CHIUDENDO
214000961212     C     ANMDAO        IFLT      D82DFV
214100961205     C                   Z-ADD     645           KCAA
214200961205     C                   EXSR      CTRANM
214300961205     C                   END
214400961205     C*
214500961205    2C     *IN09         IFEQ      *ON
214600961127     C                   MOVE      'E6 '         KCEV
214700970117     C* STAMPO LA DISCORDANZA
214800970117     C     D82LDS        IFEQ      'SI'
214900970117     C                   EXSR      PRTDIS
215000970117     C                   END
215100961115     C*
215200961113     C* ELABORAZIONE DEFINITIVA
215300961205     C   20              EXSR      CRTEVS
215400961121     C*
215500961121     C*
215600961121     C* COMPLETO AGGIORNAMENTI SE ELABORAZIONE DEFINITIVA
215700961121     C* SE C'E' RECORD BOLLA AGGIORNO ARTDTR=0
215800961205    3C     *IN20         IFEQ      *ON
215900961205    4C     WBOLLA        IFEQ      'S'
216000961121     C                   MOVE      *ZEROS        ARTDTR
216100961121     C                   UPDATE    FNART000
216200961205    4C                   END
216300961121     C* CANCELLO ANOMALIA
216400990519     C                   DELETE    FNANM000
216500961205    3C                   END
216600961205   X2C                   ELSE
216700961205     C* IL COLLO NON E' PIU' A TERRA MA E' IN PARTENZA
216800961205     C                   CLEAR                   WFIL
216900961205     C                   EXSR      GEDE6A
217000961205    2C                   END
217100961203     C*
217200961203     C     LEG600        TAG
217300961203     C*
217400961205     C                   Z-ADD     600           KCAA
217500990519     C   20KANM10        READE     FNANM10L                               31
217600990519     C  N20KANM10        READE(N)  FNANM10L                               31
217700961121    1C                   ENDDO
217800970117   1AC                   END
217900961113     C                   ENDSR
218000060913     c*****************************************************************
218100060913     C* VERIFICO ESISTENZA DISCORDANZA E7-spedizione parziale ma invio
218200060913     C*                                  il collo
218300060913     c*****************************************************************
218400060913     c     CTRE7x        BEGSR
218500060913     C                   MOVE      'E7 '         KCEV
218600060913     C     KEVS2         SETGT     FNEVS02L
218700060913     C     KEVS2         READPE    FNEVS02L                               31
218800060913    4C     *IN31         IFEQ      *ON
218900060913     C* CREO E STAMPO DISCORDANZA E7
219000060913     C                   EXSR      GEDIS27
219100060913   X4C                   ELSE
219200060913   4AC     EVSDEV        IFLT      D82DFV
219300060913     C     *IN22         ANDEQ     *OFF
219400060913     C* ESISTE EVENTO 2 CREATO IN UN GIORNO PRECEDENTE --> MEMORIZZO LA
219500060913     C* DATA E LA PISTOLA
219600060913     C                   MOVE      EVSDEV        WDEV
219700060913     C                   MOVE      EVSNPS        WNPS
219800060913     C* CERCO ANCHE SE ESISTE EVENTO 7A
219900060913     C                   MOVE      'E7A'         KCEV
220000060913     C* SE TROVO E7A IN DATA DEL FOGLIO NON FACCIO NIENTE
220100120823     C     KEVS2A        SETLL     FNEVS02L                               31
220200060913     C     *IN31         CABEQ     *ON           fineCTRE7
220300060913     C     KEVS2         SETGT     FNEVS02L
220400060913     C     KEVS2         READPE    FNEVS02L                               31
220500060913    5C     *IN31         IFEQ      *OFF
220600060913     C* TENGO LA DATA EVENTO MAGGIORE FRA EVENTO E2 ED EVENTO E2A
220700060913    6C     EVSDEV        IFGT      WDEV
220800060913     C                   MOVE      EVSDEV        WDEV
220900060913    6C                   END
221000060913    5C                   END
221100060913     C* CERCO SE ESISTE EVENTO 6-COLLO DATO PER INOLTRATO MA A TERRA
221200060913     C* IN DATA > WDEV
221300060913     C                   MOVE      'E6 '         KCEV
221400060913     C     KEVS2B        SETGT     FNEVS02L
221500060913     C     KEVS2         READE     FNEVS02L                               31
221600060913    5C     *IN31         IFEQ      *OFF
221700060913     C     EVSDEV        ANDLT     D82DFV
221800060913     C                   MOVE      'E7 '         KCEV
221900060913     C                   EXSR      GEDIS27
222000060913   X5C                   ELSE
222100060913     C* CREO EVENTO E7A SOLO SE IL COLLO NON E' IN PARTENZA OGGI
222200060913     C                   Z-ADD     620           KCAA
222300060913     C     KANM2         SETLL     FNANM02L                               31
222400060913     C  N31              MOVE      'E7B'         KCEV
222500120823     C  N31KEVS2A        SETLL     FNEVS02L                               31
222600060913    6C     *IN31         IFEQ      *OFF
222700060913     C                   MOVE      'E7A'         KCEV
222800060913     C                   EXSR      GEDIS27
222900060913    6C                   END
223000060913    5C                   END
223100060913   4AC                   END
223200060913    4C                   END
223300060913    cc     FINECTRE7     ENDSR
223400961205     C*****************************************************************
223500961205     C* STAMPA + CREAZIONE DISCORDANZA 6A
223600961205     C*****************************************************************
223700961205     C     GEDE6A        BEGSR
223800970124     C* VERIFICO SE ESISTE GIA' DISCORDANZA: POTREBBE ESSERE, INFATTI,
223900970124     C* GIA' STATA CREATA DALLA PRECEDENTE CHIUSURA DEL BIS
224000970124     C* SE ESISTE GIA' NON FACCIO NIENTE
224100970124     C                   MOVEL     'E6A'         KCEV
224200970124     C     KEVS2A        SETLL     FNEVS02L                               31
224300060913     c                   if        not *in31
224400060913     C                   MOVEL     'E6B'         KCEV
224500060913     C     KEVS2A        SETLL     FNEVS02L                               31
224600060913     c                   endif
224700060913     c                   if        not *in31
224800060913     C                   MOVEL     'E7B'         KCEV
224900060913     C     KEVS2A        SETLL     FNEVS02L                               31
225000060913     c                   endif
225100060913     c
225200970124    1C     *IN31         IFEQ      *OFF
225300961205     C*
225400961205     C* DETERMINO NUMERO PISTOLA E SE ESISTE SPUNTA DI DEFLUENZA
225500970117     C  N22              EXSR      CHKBRV
225600970117     C*
225700970117     C     WBOLLA        IFEQ      'S'
225800060913     c* Se spedizioni completa E6A, se non inviata bolla per
225900060913     c*  invio solo parziale, E7B
226000060913     c                   if        d82isc=' ' or arbft3='T'
226100970117     C                   MOVE      'E6A'         KCEV
226200060913     c                   else
226300060913     C                   MOVE      'E7B'         KCEV
226400060913     c                   endif
226500060913     c
226600970117     C                   ELSE
226700970117     C                   MOVE      'E6B'         KCEV
226800970117     C                   END
226900970117     C* STAMPO DISCORDANZA
227000970117     C     D82LDS        IFEQ      'SI'
227100970117     C                   EXSR      PRTDIS
227200970117     C                   END
227300970117     C*
227400961205     C* ELABORAZIONE DEFINITIVA
227500961205    2C     *IN20         IFEQ      *ON
227600970117     C* RICHIAMO ROUTINE PER AUTOGENERARE SPUNTA PARTENZA SE NON C'E'
227700970117     C* E CHIUDERE EVENTUALI ANOMALIE 145 E 146 DEI GIORNI PRECEDENTI
227800970117     C     WBOLLA        COMP      'S'                                424243
227900970117     C                   EXSR      PARCOL
228000970117     C*
228100961205     C                   EXSR      CRTEVS
228200961205     C*
228300961205     C* SE ELAB.DEFINITIVA E C'è REC.BOLLA AGGIORNO ARTDTR=DATA EVENTO
228400961205    3C     WBOLLA        IFEQ      'S'
228500961205     C                   MOVE      EVSDEV        ARTDTR
228600961205     C                   UPDATE    FNART000
228700961205    3C                   END
228800961205     C* CANCELLO ANOMALIA
228900990519     C                   DELETE    FNANM000
229000961205    2C                   END
229100970124    1C                   END
229200961205     C                   ENDSR
229300961120     C*****************************************************************
229400060913     C* STAMPA + CREAZIONE DISCORDANZA E2/E2A/E7/E7A
229500961120     C*****************************************************************
229600060913     C     GEDIS27       BEGSR
229700961120     C*
229800961121     C* SE DISCORDANZA 2 DETERMINO SE ESISTE LA SPUNTA DI DEFLUENZA
229900961121     C* E IL NUMERO PISTOLA
230000961121     C     KCEV          IFEQ      'E2 '
230100970117     C     *IN22         ANDEQ     *OFF
230200060913     c
230300060913     C     KCEV          orEQ      'E7 '
230400060913     C     *IN22         ANDEQ     *OFF
230500961121     C                   EXSR      CHKBRV
230600961120     C                   END
230700970117     C*
230800961127     C                   CLEAR                   WAAS
230900961127     C                   CLEAR                   WLNP
231000961127     C                   CLEAR                   WNSP
231100970117     C***
231200970117     C*  STAMPO DISCORDANZA
231300970117     C     D82LDS        IFEQ      'SI'
231400970117     C                   EXSR      PRTDIS
231500970117     C                   END
231600961120     C****
231700060913     C* CREO EVENTO
231800961205    4C     *IN20         IFEQ      *ON
231900970117     C* SOLO SE CREO DISCORDANZA 2 (ESCLUDO QUINDI SE DISC. 2A):
232000970117     C* RICHIAMO ROUTINE PER AUTOGENERARE SPUNTA PARTENZA SE NON C'E'
232100970117     C* E CHIUDERE EVENTUALI ANOMALIE 145 E 146 DEI GIORNI PRECEDENTI
232200970117     C     KCEV          IFEQ      'E2 '
232300060913     C     KCEV          oreq      'E7 '
232400970117     C                   SETON                                        42
232500970117     C                   EXSR      PARCOL
232600970117     C                   END
232700970117     C*
232800961120     C                   EXSR      CRTEVS
232900961120     C*
233000961120    4C                   ENDIF
233100961120     C*
233200961120     C                   ENDSR
233300961121     C*****************************************************************
233400961121     C*    DETERMINO ESISTENZA SPUNTA DI DEFLUENZA E NUMERO PISTOLA   *
233500961121     C*****************************************************************
233600961121     C     CHKBRV        BEGSR
233700961121     C*
233800961121     C                   CLEAR                   WFNPS
233900961121     C                   Z-ADD     1             KNPG
234000070123     C     KBRV1         CHAIN(n)  FNBRV01L                           31
234100961121     C     *IN31         IFEQ      *OFF
234200961121     C                   MOVE      BRVNPS        WNPS
234300961121     C                   ELSE
234400961121     C                   MOVE      88            WNPS
234500961122     C                   MOVE      '1'           WFNPS             1
234600961121     C                   END
234700970116     C*
234800970116     C* SE FV BIS IMPOSTO *IN09
234900970117     C   22WFNPS         IFEQ      '1'
235000970116     C                   SETON                                        09
235100970116     C                   ELSE
235200970116     C                   SETOFF                                       09
235300970116     C                   END
235400961121     C                   ENDSR
235500961118     C*****************************************************************
235600961118     C*    DETERMINO QUANDO COLLO E' STATO DATO PER PARTITO           *
235700961118     C*****************************************************************
235800961118     C     RCRDTP        BEGSR
235900961118     C*
236000961118     C                   CLEAR                   WDTPAR
236100961118     C                   MOVE      *BLANKS       WBOLLA            1
236200961118     C*
236300961118     C  N20KART27        CHAIN(N)  FNART27L                           31
236400961118     C   20KART27        CHAIN     FNART27L                           31
236500961118     C     *IN31         IFEQ      *OFF
236600961118     C                   MOVE      'S'           WBOLLA
236700961118     C                   MOVE      ARTDTR        WDTPAR
236800961118     C                   MOVE      ARTAAS        WAAS
236900961118     C                   MOVE      ARTLNP        WLNP
237000961118     C                   MOVE      ARTNSP        WNSP
237100060913     c
237200060913     c* Se devo inviare soltanto le spedizioni complete, chaino anche
237300060913     c*  la bolla per sapere se inviata
237400060913     c                   if        d82isc='S'
237500060913     c     karb1         chain(n)  fnarb01l
237600060913     c                   endif
237700060913     c
237800961118     C                   ELSE
237900970602     C                   CLEAR                   WAAS
238000961118     C                   MOVE      ANMAAS        WAAS
238100961118     C                   MOVE      ANMLNP        WLNP
238200961118     C                   MOVE      ANMNSP        WNSP
238300961118     C                   MOVE      'E2 '         KCEV
238400961121     C     KEVS2         SETGT     FNEVS02L
238500961127     C     KEVS2         READPE    FNEVS02L                               31
238600961118     C  N31              MOVE      EVSDEV        WDTPAR
238700060913     c*
238800961118     C                   END
238900961118     C                   ENDSR
239000961112     C*****************************************************************
239100961112     C*    SCRITTURA EVENTO                                           *
239200961112     C*****************************************************************
239300961113     C     CRTEVS        BEGSR
239400961113     C                   CLEAR                   FNEVS000
239500961115     C*
239600961115     C                   MOVE      WAAS          EVSAAS
239700961115     C                   MOVE      WLNP          EVSLNP
239800961115     C                   MOVE      WNSP          EVSNSP
239900961115     C*
240000961115     C                   MOVE      KFLS          EVSFLS
240100961115     C                   MOVE      KLNA          EVSLNA
240200961115     C                   MOVE      KNRS          EVSNRS
240300961115     C                   MOVE      KNSC          EVSNSC
240400961120     C                   MOVE      WNPS          EVSNPS
240500961115     C*
240600961212     C                   MOVE      D82DFV        EVSDEV
240700961115     C                   MOVE      KCEV          EVSCEV
240800961115     C* SE COLLO A TERRA MEMORIZZO FILIALE CHE HA SPARATO IL COLLO
240900961115     C                   MOVE      WFIL          EVSFIL
241000961115     C*
241100961115     C* EVSDAT --> PER ORA MI SERVE PER DISCORDANZA 6 PER MEMORIZZARE
241200961115     C*            LA DATA IN CUI IL COLLO ERA STATO DATO PER PARTITO
241300961115     C                   MOVE      WDTPAR        EVSDAT
241400961125     C     WDTPAR        IFGT      *ZEROS
241500961115     C                   MOVEL     COS1          EVSDES
241600961125     C                   END
241700961115     C*
241800961115     C                   WRITE     FNEVS000
241900120823     C*
242000961112     C                   ENDSR
242100961114     C*****************************************************************
242200961122     C*    RICERCA ANOMALIA 645  IN DATA = DATA FOGLIO DEFLUENZA      *
242300961114     C*****************************************************************
242400961114     C     CTRANM        BEGSR
242500020225     C                   SETOFF                                       09
242600020225     C     KANM2A        CHAIN     FNANM02L                           31
242700020225     C  N31              SETON                                        09
242800961114     C                   ENDSR
242900961115     C*****************************************************************
243000961115     C*    OPERAZIONI PER COLLO CHE PARTE                             *
243100961115     C*****************************************************************
243200961115     C     PARCOL        BEGSR
243300961122     C* CHIUSURA ANOMALIE 145 E 146
243400961122     C                   Z-ADD     145           KCAA
243500961118    1C                   DO        2
243600990519     C     KANM2         CHAIN     FNANM02L                           31
243700961122    2C     *IN31         IFEQ      *OFF
243800020225     C                   CLEAR                   FNLR33DS
243900961212     C                   MOVE      D82DFV        D33DCH
244000961115     C                   MOVE      ANMNR2        D33NRR
244100961115     C* PRENDO FASE ANOMALIA
244200961115     C                   CALL      'TRUL11R'
244300961115     C                   PARM                    PARAM9
244400961115     C                   MOVE      FASE          D33FSC
244500961115     C                   CALL      'FNLR33R'
244600020225     C                   PARM                    FNLR33DS
244700961118    3C                   END
244800961115     C* RIPETO GIRO PER 146
244900961115     C                   Z-ADD     146           KCAA
245000961118    1C                   ENDDO
245100961115     C*
245200961115     C* AUTOGENERAZIONE SPUNTA PARTENZA 88 SE NON ESISTE
245300961120     C     WFNPS         IFEQ      '1'
245400961120     C* SE DEVO AUTOGENERARE SPUNTA PARTENZA DI COLLO CON MANCA RECORD
245500961120     C* BOLLA ARRIVO CERCO LA ZONA LEGGENDO UNA QUALUNQUE SPUNTA DEL
245600961120     C* COLLO
245700961120     C     *IN42         IFEQ      *ON
245800961120     C                   CLEAR                   WRVZNC
245900070123     C     KBRV7         CHAIN     FNBRV07L                           31
246000961120     C     *IN31         IFEQ      *OFF
246100961120     C                   MOVE      BRVZNC        WRVZNC
246200961120     C                   END
246300961120     C                   END
246400980126     C*
246500980126     C                   EXSR      DOPPIE
246600980126     C*
246700140625     C   40              CLEAR                   FNBRV5
246800040303     c  n40*in41         ifeq      *off
246900040303    7C     NST(WA)       orne      '1'                                          NR.SPUNTE GG.
247000070123     C                   CLEAR                   FNBRV000
247100040303     c                   else
247200140625     C                   CLEAR                   FNBRV5
247300040303     c                   endif
247400961115     C*
247500961212     C                   Z-ADD     SIMFEL        BRVFGS                         FIL GESTIONE
247600000208     C                   Z-ADD     SIMFEL        BRVPES                         FIL GESTIONE
247700000208     C                   Z-ADD     D82NFV        BRVNFV                         NUM.FOGLIO
247800961115     C                   Z-ADD     1             BRVNPG                         CAT.FOGLIO
247900961115     C                   Z-ADD     SIMFEL        BRVFLE                         FIL.ELABORAT
248000070220     c                   time                    wtimez
248100070220     c* millisecondi
248200070220     c                   extrct    wtimez:*ms    millisec          6 0
248300070220     c                   movel     millisec      brvmis
248400070220     c* data immissione
248500070220     c                   movel     wtimez        dataiso
248600070220     c                   movel     dataiso       brvdfs
248700070220     c* ora immissione
248800070220     c                   move      wtimez        oraiso
248900070220     c                   movel     oraiso        brvhms
249000070220     c*
249100070220     c                   movel     'A'           brvtap
249200070220     c                   eval      brvpru='§'+ nompgm
249300961204     C                   MOVE      KFLS          BRVLNP
249400961204     C                   MOVE      KLNA          BRVLNA
249500961204     C                   MOVE      KNRS          BRVNRS
249600961204     C                   MOVE      KNSC          BRVNSC
249700961115     C*
249800961204     C*  DA ANOMALIE
249900961204     C   42              MOVE      WRVZNC        BRVZNC                          ZONA
250000961204     C*  DA FNART
250100961204     C   43              MOVE      ARBZNC        BRVZNC                          ZONA
250200961115     C*
250300961115     C                   Z-ADD     88            BRVNPS
250400961115     C*
250500140625     C   40              WRITE     FNBRV5
250600040303     c  n40*in41         ifeq      *off
250700040303    7C     NST(WA)       orne      '1'                                          NR.SPUNTE GG.
250800070123     C                   update    fnbrv000
250900040303     c                   else
251000140625     C                   update    fnbrv5
251100040303     c                   endif
251200961115     C                   END
251300961115     C*
251400961115     C                   ENDSR
251500980126     C**************************************************************************
251600040303     C* VERIFICO SE esiste già la spunta:
251700040303     C* 1) per p.o. se previsto dalla categoria, tabella 7n (4)
251800040303     C* 2) per num foglio (npg+nfv+fgs) se da tenere tutte
251900980126     C**************************************************************************
252000980126     C     DOPPIE        BEGSR
252100040303     C*
252200040303     C                   Z-ADD     1             KNPG                           CATEGORIA
252300040303     C                   Z-ADD     SIMFEL        KFLE
252400040303     C*
252500040303     C* VERIFICO IL FLAG NELLA DS7N
252600040303     C                   Z-ADD     1             WA                3 0
252700040303     C     1             LOOKUP    CAT(WA)                                41     ON=TROVATO
252800040303     c     *in41         ifeq      *off
252900040303    7C     NST(WA)       orne      '1'                                          NR.SPUNTE GG.
253000140625     C     KBRV1         CHAIN     FNBRV01L                           40
253100040303     c                   else
253200140625     C     KBRV3         CHAIN     FNBRV05L                           40
253300040303     c                   endif
253400040303     c
253500070123     C* SE C'E' L'ANOMALIA "E" SCriVO FILE FNBRVE0F
253600040303     C  n40BRVCAN        IFEQ      'E'
253700070123     C     KBRVE         SETLL     FNBRVE1L                               99
253800070123     C  N99              WRITE     FNBRV00E
253900040303     C                   ENDIF
254000980126     C*
254100980126     C                   ENDSR
254200961112     C*****************************************************************
254300961112     C*    CHAIN TABEL00F                                             *
254400961112     C*****************************************************************
254500961112     C     CTABEL        BEGSR
254600961112     C     KTAB          CHAIN     TABEL                              31
254700961114     C  N31TBLFLG        IFNE      ' '
254800961112     C                   SETON                                        31
254900961112     C                   END
255000961112     C                   ENDSR
255100961129     C**************************************************************************
255200961129     C*  ABBLANCA ZERO NON SIGNIFICATIVI
255300961129     C**************************************************************************
255400961129     C     ABBL          BEGSR
255500961129     C*
255600020225     C                   Z-ADD     1             YY
255700020225     C     K08(YY)       DOWEQ     '0'
255800020225     C     YY            ANDLT     8
255900020225     C                   MOVEL     ' '           K08(YY)
256000020225     C                   ADD       1             YY
256100961129     C                   ENDDO
256200961129     C*
256300961129     C                   ENDSR
256400961121     C*****************************************************************
256500961121     C* STAMPA DISCORDANZE -------------------------------------------*
256600961121     C*****************************************************************
256700961121     C     PRTDIS        BEGSR
256800961121     C*
256900961122     C                   SETON                                        08
257000961125     C*
257100961125     C     WFIL          COMP      *ZEROS                                 14
257200961127     C*
257300961127     C     WNSP          COMP      *ZEROS                             1515
257400961127     C     KNSC          COMP      *ZEROS                             1616
257500961127     C     KNRS          COMP      *ZEROS                             1717
257600961128     C*
257700961129     C     KCEV          COMP      'E4 '                                  18
257800060918     C  n18KCEV          COMP      'E8 '                                  18
257900961127     C*
258000961127     C* STAMPO ANNO DI 2
258100961127     C                   MOVE      WAAS          WPAAS             2 0
258200961122     C*
258300961122     C* DECODIFICO CAUSALE EVENTO
258400961122     C                   MOVE      '8E'          KCOD
258500961122     C                   MOVEL(P)  KCEV          KKEY
258600970114     C* prendo la descrizione in italiano
258700970114     C     KTAB          CHAIN     TABEL00F                           31
258800970114     C  N31              MOVEL     TBLUNI        DCEV             60
258900970114     C   31              CLEAR                   DCEV
259000970114     C* se devo inviarlo anche via fax prendo anche la descrizione in
259100970114     C* inglese
259200120821     c     d82ftt        ifne      FEDEX
259300970115     C   20              MOVE      '1'           KKEY
259400970115     C   20KTAB          CHAIN     TABEL00F                           31
259500970115     C  N31
259600970115     CAN 20              MOVEL     TBLUNI        DCEVE            60
259700970115     C   31
259800970115     CAN 20              CLEAR                   DCEVE
259900970115    1C   20*INOB         IFEQ      *ON
260000040423
260100040423     c                   if        FAX_Disc_Est = 'S'
260200961122     C                   ADD       1             WPAG
260300961121     C                   EXCEPT    TESDIS
260400970114     C                   SETOFF                                       OB
260500040423     c                   end
260600040423
260700961129    1C                   END
260800040423
260900040423     c                   if        FAX_Disc_Est = 'S'
261000970115     C   20              EXCEPT    DETDIS
261100040423     c                   end
261200040423
261300020301     c                   endif
261400970114    1C     *INOE         IFEQ      *ON
261500970114     C                   ADD       1             WPAG1
261600970114     C                   EXCEPT    TESDIC
261700970114     C                   SETOFF                                       OE
261800970114    1C                   END
261900970114     C                   EXCEPT    DETDIC
262000961122     C* STAMPA DATA PARTENZA COLLO SE DISCORDANZA 6 O 6A
262100961129    1C     WDTPAR        IFGT      *ZEROS
262200961122     C                   CLEAR                   WLBDA8
262300961122     C                   MOVE      WDTPAR        G02INV
262400961122     C                   MOVE      '3'           G02ERR
262500961122     C                   CALL      'XSRDA8'
262600961122     C                   PARM                    WLBDA8
262700961122     C                   MOVE      G02DAT        WDTPA1
262800120821     c     d82ftt        ifne      FEDEX
262900970115    2C   20*INOB         IFEQ      *ON
263000040423
263100040423     c                   if        FAX_Disc_Est = 'S'
263200961122     C                   ADD       1             WPAG
263300961122     C                   EXCEPT    TESDIS
263400970114     C                   SETOFF                                       OB
263500040423     c                   end
263600040423
263700961129    2C                   END
263800040423
263900040423     c                   if        FAX_Disc_Est = 'S'
264000970115     C   20              EXCEPT    DETDI2
264100970115     C   20              EXCEPT    FIT
264200040423     c                   end
264300040423
264400020301     c                   endif
264500970114    2C     *INOE         IFEQ      *ON
264600970114     C                   ADD       1             WPAG1
264700970114     C                   EXCEPT    TESDIC
264800970114     C                   SETOFF                                       OE
264900970114    2C                   END
265000970114     C                   EXCEPT    DETDC2
265100970114     C                   EXCEPT    FITC
265200961129   X1C                   ELSE
265300961129     C     KCEV          IFNE      'E4 '
265400120821     c     d82ftt        ifne      FEDEX
265500040423
265600040423     c                   if        FAX_Disc_Est = 'S'
265700970115     C   20              EXCEPT    FIT
265800040423     c                   end
265900040423
266000020301     c                   endif
266100970114     C                   EXCEPT    FITC
266200961129     C                   END
266300961129    1C                   END
266400961121     C*
266500961121     C                   ENDSR
266600960529     C*-----------------------------------------------------------------*
266700960529     C*  Operazioni iniziali
266800960529     C*-----------------------------------------------------------------*
266900960529     C     *INZSR        BEGSR
267000960529     C*
267100960529     C*  Reperisco dati da video
267200960529     C     *ENTRY        PLIST
267300960529     C                   PARM                    KPJBA
267400020225     C                   MOVEL     KPJBU         TRTC82DS
267500040423
267600040423     ** Maggio 2004:
267700120823     ** Non si vuole più mandare il FAX discordanze al Partner
267800040423     c                   move      'N'           FAX_Disc_Est      1
267900040423
268000070123     C* MUOVO N.FOGLIO IN CAMPO LUNGO 6,0 PER CHAIN SU FNBRV
268100000208     C                   Z-ADD     D82NFV        D82NFS
268200990421      * SALVO KPJBU "ORIGINALE"
268300990421     C                   MOVEL     KPJBU         WKPJBU
268400971230      *
268500971230     C* MI SALVO LINEA DI ARRIVO ESTERA (ALTRIMENTI SI SPORCA NEL
268600971230     C* RICHIAMARE IL TRTC83R2)
268700961211     C                   MOVE      D82LNP        LNAEXP
268800961129     C*  Controllo se devo scrivere il nr.spedizione compattato
268900961129     C     D82SPC        COMP      'S'                                    21
269000960529     C*  Richiamo XPARUT
269100960529     C                   Z-ADD     1             CODUT
269200960529     C                   CALL      'X§PARUT'
269300020225     C                   PARM                    UT§DSE0F
269400960529     C                   MOVEL     REC80         CNCR80
269500110511     C                   MOVEL     REC42         CNCR42
269600110511     C                   MOVEL     simRGS        RSUT             20
269700110511     C                   MOVEL     simRGS        RSUTM            19
269800020225     C* RICERCA CAPOCONTI
269900961122     C                   DO        50            X                 2 0
270000961122     C                   MOVE      TCU(X)        TCUDS
270100961122     C     F56           IFEQ      'CG'
270200961122     C     F4            ANDEQ     '1'
270300961122     C     F3            ANDEQ     '0'
270400961122     C                   Z-ADD     KCU(X)        KCI               4 0
270500020225     C                   ENDIF
270600020225     C                   ENDDO
270700961122     C* REPERISCO KG PER METRO CUBO PER CALCOLO PESO TASSABILE
270800961122     C                   MOVEL     '7A'          KCOD
270900961122     C                   MOVEL(P)  '2'           KKEY
271000961122     C     KTAB          CHAIN     TABEL00F                           31
271100961122     C  N31              MOVEL     TBLUNI        DS7A2
271200961122     C   31              CLEAR                   DS7A2
271300150825     C*
271400150825     C** CARICAMENTO DATI VARI TASSAZIONE
271500150825     C                   MOVEL     'QT'          kcod
271600150825     C                   MOVEL(P)  '1'           kKEY
271700150825     C     KTAB          CHAIN     TABEL00F                           31
271800150825     C  N31              MOVEL     TBLUNI        DSQT1
271900150825     C   31              CLEAR                   DSQT1
272000150825     C*
272100980126     C* SERVE SAPERE SE NELLE VARIE CATEGORIE FOGLI POSSO
272200980126     C* AVERE SPUNTE DOPPIE OPPURE NO
272300980126     C                   Z-ADD     *ZERO         WA                             CONTATORE
272400980126     C                   MOVEL     '7N'          KCOD
272500980126     C     KTAB2         SETLL     TABEL00F
272600980126     C     KTAB2         READE     TABEL00F                               31
272700980126     C     *IN31         DOWEQ     *OFF
272800980126     C     TBLFLG        IFEQ      *BLANKS
272900980126     C                   ADD       1             WA
273000980126     C                   MOVEL     TBLUNI        DS7N
273100980126     C                   MOVEL     TBLKEY        CAT(WA)
273200980126     C                   MOVEL     §7NNST        NST(WA)
273300980126     C                   ENDIF
273400980126     C     KTAB2         READE     TABEL00F                               31
273500980126     C                   ENDDO
273600990412      * CARICO TABELLA £1 SE NON SONO IN SEDE
273700990412     C     SIMFEL        IFNE      *ZEROS
273800020225     C                   CLEAR                   TRUL06DS
273900990412     C                   CLEAR                   KPJBU
274000990412     C                   MOVEL     '£1'          D06COD
274100990412     C                   MOVEL     SIMFEL        D06KEY
274200990412     C                   MOVEL     'L'           D06TLA
274300020225     C                   MOVEL     TRUL06DS      KPJBU
274400990412      *
274500990412     C                   CALL      'TRUL06R'
274600990412     C                   PARM                    KPJBA
274700020225     C                   MOVEL     KPJBU         TRUL06DS
274800990421      * REIMPOSTO KPJBU "ORIGINALE"
274900990421     C                   MOVEL     WKPJBU        KPJBU
275000990421     C                   END
275100090402     c
275200090402     c                   open(e)   tita430c
275300090402     c                   if        not %open(tita430c)
275400090402     c                   eval      %subst(WTA430:7:4)='GRPS'
275500090402     c                   eval      %subst(WTAS30:7:4)='GRPS'
275600090402     c                   open      tita430c
275700090402     c                   endif
275800090402     c                   open      titas30c
275900130215     c
276000960529     C*---------------------------------------------------------------*
276100960529     C* Definisco chiavi di accesso ai file
276200961114     C     KARB1         KLIST
276300961114     C                   KFLD                    ARTAAS
276400961114     C                   KFLD                    ARTLNP
276500961114     C                   KFLD                    ARTNRS
276600961114     C                   KFLD                    ARTNSP
276700961113     C     KARB55        KLIST
276800961211     C                   KFLD                    LNAEXP
276900961113     C                   KFLD                    KDT3
277000120823     C     KARB          KLIST
277100961121     C                   KFLD                    ARBAAS
277200961121     C                   KFLD                    ARBLNP
277300961121     C                   KFLD                    ARBNRS
277400961121     C                   KFLD                    ARBNSP
277500960529     C     KAR4          KLIST
277600961121     C                   KFLD                    ARBAAS
277700961121     C                   KFLD                    ARBLNP
277800961121     C                   KFLD                    ARBNRS
277900961121     C                   KFLD                    ARBNSP
278000960529     C                   KFLD                    KTRC
278100061127      * contatto nome e telefono (Tipo record [GEN])
278200061127     C     KAR5          KLIST
278300061127     C                   KFLD                    ARBAAS
278400061127     C                   KFLD                    ARBLNP
278500061127     C                   KFLD                    ARBNRS
278600061127     C                   KFLD                    ARBNSP
278700061127     C                   KFLD                    KGEN
278800061127     c                   movel     'GEN'         KGEN              3
278900960529     C     KTAB          KLIST
279000961122     C                   KFLD                    CODUT
279100960529     C                   KFLD                    KCOD
279200960529     C                   KFLD                    KKEY
279300980126     C     KTAB2         KLIST
279400980126     C                   KFLD                    CODUT
279500980126     C                   KFLD                    KCOD
279600961220     C     KETAB         KLIST
279700961220     C                   KFLD                    KECOD
279800961220     C                   KFLD                    KEKEY
279900130215     C     KEDI          KLIST
280000130215     C                   KFLD                    tabcod
280100130215     C                   KFLD                    tabkey
280200961113     C     KANM10        KLIST
280300961113     C                   KFLD                    KCAA
280400961211     C                   KFLD                    LNAEXP
280500961115     C     KANM2         KLIST
280600961115     C                   KFLD                    KFLS
280700961115     C                   KFLD                    KLNA
280800961115     C                   KFLD                    KNRS
280900961122     C                   KFLD                    KNSC
281000961115     C                   KFLD                    KCAA
281100961122     C     KANM2A        KLIST
281200961122     C                   KFLD                    KFLS
281300961122     C                   KFLD                    KLNA
281400961122     C                   KFLD                    KNRS
281500961122     C                   KFLD                    KNSC
281600961122     C                   KFLD                    KCAA
281700961212     C                   KFLD                    DFV06
281800961115     C     KEVS2         KLIST
281900961115     C                   KFLD                    KFLS
282000961115     C                   KFLD                    KLNA
282100961115     C                   KFLD                    KNRS
282200961122     C                   KFLD                    KNSC
282300961115     C                   KFLD                    KCEV
282400120823      **
282500961115     C     KEVS2A        KLIST
282600961115     C                   KFLD                    KFLS
282700961115     C                   KFLD                    KLNA
282800961115     C                   KFLD                    KNRS
282900961122     C                   KFLD                    KNSC
283000961115     C                   KFLD                    KCEV
283100961212     C                   KFLD                    D82DFV
283200120823      **
283300961119     C     KEVS2B        KLIST
283400961119     C                   KFLD                    KFLS
283500961119     C                   KFLD                    KLNA
283600961119     C                   KFLD                    KNRS
283700961122     C                   KFLD                    KNSC
283800961119     C                   KFLD                    KCEV
283900961120     C                   KFLD                    WDEV
284000120823      **
284100961121     C     KART27        KLIST
284200961121     C                   KFLD                    KFLS
284300961121     C                   KFLD                    KLNA
284400961121     C                   KFLD                    KNRS
284500961121     C                   KFLD                    KNSC
284600120823      **
284700120823     C     KBRV7         KLIST
284800120823     C                   KFLD                    KFLS
284900120823     C                   KFLD                    KLNA
285000120823     C                   KFLD                    KNRS
285100120823     C                   KFLD                    KNSC
285200120823      **
285300961121     C     KART55        KLIST
285400961211     C                   KFLD                    LNAEXP
285500961121     C                   KFLD                    KDTR
285600961120     C     KBRV1         KLIST
285700961118     C                   KFLD                    KNPG
285800000208     C                   KFLD                    D82NFS
285900961212     C                   KFLD                    SIMFEL
286000961118     C                   KFLD                    KFLS
286100961118     C                   KFLD                    KLNA
286200961118     C                   KFLD                    KNRS
286300961122     C                   KFLD                    KNSC
286400140625     C     KBRV3         KLIST
286500980126     C                   KFLD                    KNPG                           ..CATEGORIA
286600980126     C                   KFLD                    KFLS                           ..LNP
286700980126     C                   KFLD                    KLNA                           ..LNA
286800980126     C                   KFLD                    KNRS                           ..SERIE
286900980126     C                   KFLD                    KNSC                           ..SEGNACOLLO
287000980126     C                   KFLD                    KFLE                           ..FIL.GESTIONE
287100961122     C*
287200961122     C     KIND          KLIST
287300961122     C                   KFLD                    CODUT
287400961122     C                   KFLD                    KCI
287500961122     C                   KFLD                    D82CPT
287600010613     C     KBRVE         KLIST
287700010613     C                   KFLD                    BRVNPG
287800010613     C                   KFLD                    BRVNFV
287900010613     C                   KFLD                    BRVFGS
288000010613     C                   KFLD                    BRVLNP
288100010613     C                   KFLD                    BRVLNA
288200010613     C                   KFLD                    BRVNRS
288300010613     C                   KFLD                    BRVNSC
288400020808     c     K02FGV01      klist
288500020808     c                   kfld                    D82nfv
288600020808     C                   kfld                    SIMfel
288700110510     C     ktbe          klist
288800110510     C                   kfld                    tbecod
288900110510     C                   kfld                    tbeke1
289000110510     C                   kfld                    tbeke2
289100960529     C*---------------------------------------------------------------*
289200960529     C                   TIME                    UTIME             6 0
289300960529     C* PRENDO LA DATA DEL GIORNO DA TIME
289400960529     C                   TIME                    W0140            14 0
289500960716     C                   MOVE      W0140         UDATE8
289600960716     C                   MOVE      UDATE8        G02DAT
289700960716     C                   MOVEL     *BLANK        G02ERR
289800960716     C                   CALL      'XSRDA8'
289900960716     C                   PARM                    WLBDA8
290000960716     C                   Z-ADD     G02INV        WOGGI             8 0           UDATE
290100130215      *------------
290200130215     C* Caricamento Tabella Partner
290300130215     C                   MOVEL     'PT'          TABCOD
290400130215     C     TABCOD        setll     EDTAB01L
290500130215     C     TABCOD        reade     EDTAB01L
290600130215      *
290700130215     C                   DOW       not %EoF(edtab01l)
290800130215      *
290900130215     C     TABFLG        IFEQ      *BLANKS
291000130215     c                   eval      dsPT = tabuni
291100130215      *
291200130215      * deve uscire con un solo record  tramite il codice KSC del PARTNER
291300130215      *  se ci fossero più codici KSC uguali per motivi particolari il primo
291400130215      *  in ordine di KEY DEVE essere, x regola, quello del Partner mentre il
291500130215      *  secondo è definito come CLIENTE sempre.
291600130215     C     D82CPT        ifeq      §PTKSC
291700130215      *
291800130215      *** al suo interno carica anche l'altra tabella correlata
291900130215      **   in questo caso la PZ e poi esce
292000130215     C                   MOVEL     'PZ'          TABCOD
292100130215     C     Kedi          chain     EDTAB01L
292200130215     c                   if        %Found(edtab01l)
292300130215     c                   eval      dsPZ = tabuni
292400130215     c                   end
292500130215      *
292600130215     c                   leave
292700130215     c                   end
292800130215      *
292900130215     C                   END
293000130215      *
293100130215     C     TABCOD        READE     EDTAB01L
293200130215     C                   END
293300130215      *------------
293400110510      *
293500110510     c                   move      'VPO'         tbecod
293600110510     c                   movel     'BRT'         tbeke1
293700110510     c                   clear                   tbeke2
293800110510     c     ktbe          chain     tntbe01l
293900110510     c                   if        %found(tntbe01l)
294000110510     c                   movel     tbeuni        dvpobrt
294100110510     c                   if        Woggi  <= §VPOBRTDTA
294200110510     c                   seton                                        66
294300110510     c                   endif
294400110510     c                   endif
294500120305      *
294600120305      ***cambio di marchio in BRT finale x il 15/3
294700120305     c                   setoff                                       66
294800120305     c                   if        Woggi  <  20120315
294900120305     c                   seton                                        66
295000120305     c                   endif
295100961114     C*
295200961114     C     D82DEF        COMP      'SI'                                   20
295300970117     C     D82FVB        COMP      'B'                                    22
295400970114     C*
295500970114     C*  IMPOSTO DATA FOGLIO VIAGGIO PER STAMPA
295600970114     C                   Z-ADD     D82DFV        DATA
295700970114     C                   Z-ADD     MM            GGMM              4 0
295800970114     C                   MOVEL     GG            GGMM
295900970114     C                   Z-ADD     AA            PARDFV
296000970114     C                   MOVEL     GGMM          PARDFV
296100970114     C                   MOVE      D82DFV        DFV06
296200970114     C*  Data CMR
296300970114     C                   Z-ADD     D82DCM        DATA
296400970114     C                   Z-ADD     MM            GGMM              4 0
296500970114     C                   MOVEL     GG            GGMM
296600970114     C                   Z-ADD     AA            DATCMR            8 0
296700970114     C                   MOVEL     GGMM          DATCMR
296800020225      *
296900970114     C   20              OPEN      FNEVS01L
297000020225      *
297100020225      * IMPOSTO TIPO RECORD PER LETTURE WORK FILE
297200970113     C  N20              MOVE      'P'           WTRC
297300120821     C   20              MOVE      'D'           WTRC
297400020225      ** LETTURA/AGGIORNAMENTO FITTIZIO PER GIUSTIFICARE L'APERTURA
297500020225      ** IN UPDATE (DEVO FARLO PER UTILIZZARE IL FILE SOTTO COMIT VISTO CHE
297600020225      ** E' UTILIZZATO IN UPDATE NEI PGM DI SCRITTURA RCD PER TRASMISSIONE)
297700970124     C     *LOVAL        SETLL     FNARB01L
297800970124     C                   READ      FNARB01L                               31
297900970124     C  N31              EXCEPT    FITTIZ
298000020225      *
298100960529     C                   ENDSR
298200150825     C*****************************************************************
298300150825     C*  Reperisce Peso/Volume o da CML o da Bollettato
298400150825     C*****************************************************************
298500150825     C     VOL_PES       BEGSR
298600150825     C*
298700150825     c     *like         define    arbvlb        wvol
298800150825     c     *like         define    arbpkb        wpes
298900150825     C     *LIKE         DEFINE    §qttpc        WTARA
299000150825      *
299100150825      * Prima cerca il volume fra bollettato e CML
299200150825     C     arbncR        ifeq      arbncl
299300150825      * se è considerata un'unica spedizione
299400150825     C                   z-add     ARBVLC        WVOL
299500150825     C                   else
299600150825     C*
299700150825     C     ARBVLC        ifge      ARBVLb
299800150825     C                   z-add     ARBVLC        WVOL
299900150825     C                   else
300000150825     C                   z-add     ARBVLb        WVOL
300100150825     C                   end
300200150825     C*
300300150825     C                   endif
300400150825     C* PESO VDL - TARA
300500150825     C     §QTTPC        MULT      arbncp        WTARA
300600150825     C                   Z-ADD     arbPKC        wpes
300700150825     c                   sub(h)    wtara         wpes
300800150825      *
300900150825      * Poi cerca il peso fra bollettato e CML
301000150825     C     arbncP        ifeq      arbncl
301100150825      * se è considerata un'unica spedizione
301200150825     C                   ELSE
301300150825     C*  altrimenti se il bollettato è superiore passo il Bollettato.
301400150825     C     wpes          iflt      arbPKb
301500150825     C                   Z-ADD     arbPKb        wpes
301600150825     C                   end
301700150825     C*
301800150825     C                   ENDIF
301900150825     C*
302000150825     C                   ENDSR
302100150825     C*--------------------------------------------------------------*
302200961023     OFNARB000  E            LIBER
302300961023     OFNARB000  E            AGGTRA
302400961023     O                       ARBFT3
302500961023     O                       ARBDT3
302600961112     OFNARB000  E            AGGFT3
302700961112     O                       ARBFT3
302800970124     OFNARB001  E            FITTIZ
302900961212     O*****************************************************************
303000961212     O* MANIFEST PER IL PARTNER
303100961212     O*****************************************************************
303200961213     OMANPTN    E            TESTA            02
303300041109     O                       D46(1)             112
303400961217     O                       STARAG              75
303500961217     O                       STPFAX              97
303600041109     O              N20                         112 '* PROVVISORIO'
303700041109     O               20                         112 '* DEFINITIVO '
303800961212     O          E            TESTA            03
303900931229     O                       ST(1)               66
304000041109     O                       D46(2)             112
304100961217     O                       UDATE8              54 '  /  /    '
304200961217     O                       UTIME               89 '  :  :  '
304300041109     O                       PAGE1         Z    111
304400961212     O          E            TESTA            04
304500961217     O                       ST(4)               66
304600041109     O                       D46(3)             112
304700961217     O                       RSUTM               63
304800961217     O                       STADLA             106
304900961212     O          E            TESTA            05
305000961217     O                       ST(3)               66
305100041109     O                       D46(4)             112
305200961217     O                       UDATE8              54 '  /  /    '
305300041109     O                       D82CMR             112
305400961212     O          E            TESTA            06
305500961217     O                       ST(19)              66
305600041109     O                       D46(5)             112
305700041109     O                       DATCMR              87 '  /  /    '
305800961212     O          E            TESTA          1 08
305900931229     O                       ST(6)               66
306000041109     O                       D46(6)             112
306100021204      *--------
306200931229     O          E            DET         1
306300100330     O              N21      §LB14               16
306400961129     O               21      §LB8                16
306500941222     O                       STARMN        Z   +  1
306600941222     O                       STARSM            +  1
306700941222     O                       STARSD            +  2
306800940111     O                       STATBO            +  1
306900940111     O                       ARBNCL        Z   +  2
307000961125     O                       PESPRT        4   +  2
307100041109     O                    N34PRTCAS        4   +  2
307200100223     O          E    24      DET         1
307300100223     O                       RagADD45            99
307400931229     O          E            DET         1
307500941221     O                       STADSP              16 '  /  /    '
307600130808     O***************        ARBCAM              36
307700130808     O***************        STALOM              53
307800130808     O                       STAINM              52
307900941222     O                       STAIND              79
308000961011     O                       ARBNAS             100
308100041109     O                    N34AR9VCA             112
308200130808     O          E    25N27   DET         1
308300100223     O                       IndADD10            64
308400130808     O          E    27N25   DET         1
308500130808     O                       IndMAdd10           37
308600130808     O          E    27 25   DET         1
308700130808     O                       IndMAdd10           37
308800130808     O                       IndADD10            64
308900941222     O          E            DET         1
309000940111     O                     02                    13 '* PARTIAL *'
309100130808     O***************        ARBRMA              30
309200130808     O***************                            37 'P.IVA:'
309300130808     O***************        MITCPI              53
309400130808     O                       ARBCAM              36
309500130808     O                       STALOM              53
309600950614     O                       ARBCAD              63
309700950614     O                       STALOD              84
309800950614     O                                           91 'P.IVA:'
309900950614     O                       DESCPI             107
310000100223     O          E    26      DET         1
310100100224     O                       CitADD15            69
310200130808     O          E            DET         1
310300130808     O                       ARBRMA              30
310400130808     O                                           37 'P.IVA:'
310500130808     O                       MITCPI              53
310600961220     O          E   N34      DET         1
310700041109     O                       STATIC             112
310800961121     O          E   N36      DET         1
310900950304     O         OR   N35
311000041109     O                    N35STANT1              75
311100041109     O                    N36STANT2             111
311200961121     O          E   N36      DET         0  1
311300961121     O         OR   N35
311400041109     O                    N35STANT1              75
311500041109     O                    N36STANT2             111
311600100223      * Contatto Nom./tel.
311700100223     O          E   N33      DET         1
311800100223     O                                           37 'Contact Name:'
311900100223     O                       STACNA              72
312000100223     O                                           80 'Tel.:'
312100100223     O                       STACTE             106
312200100223      *
312300941222     O* EXCPT FITTIZIA
312400941222     O          E          36DET         1  0
312500041109     O                                          112 ' '
312600021204     O* valore merce dichiarato
312700021204     O          E            VMDDET         1
312800021204     O                                           37 'Valore Merce:'
312900021204     O                       ARBVMD        2  +   1
313000021204     O                       ARBVAD           +   1
313100021204     O*
313200940111     O          E            TOTAL       1
313300940111     O                       ST(15)              66
313400041109     O                       D46(7)             112
313500940111     O          E            TOTAL       1
313600940111     O                       ST(17)              66
313700041109     O                       D46(8)             112
313800940111     O                       TOTSPE        Z     24
313900940111     O                       TOTNCL        Z     50
314000961125     O                       TOTPES        4     79
314100961125     O                       TOTVLF        4    104
314200940111     O          E            TOTAL       1
314300940111     O                       ST(15)              66
314400041109     O                       D46(7)             112
314500931229     O          E            FINE        1
314600931229     O                                           69 '***  END  OF  PRINT  ***'
314700961121     O*
314800970110     O*****************************************************************
314900970110     O* MANIFEST PER IL PARTNER IN FORMA SINTETICA
315000970110     O*****************************************************************
315100970110     OMANPTS    E            TESSIN           02
315200970110     O                       MAS(1)              66
315300970110     O                       MAS(2)             132
315400970110     O                       RSUT                20
315500970110     O                       SIMFEL              24
315600970110     O                       UDATE8             121 '  /  /    '
315700970110     O                       PAGE1         Z    132
315800970110     O          E            TESSIN           03
315900970110     O                       MAS(3)              66
316000970110     O                       MAS(4)             132
316100970110     O                       KNSIF               10
316200970110     O                       KNMUS               21
316300000208     O                       D82NFV              40
316400000208     O               22                          44 'BIS'
316500970110     O                       STADLS              65
316600970110     O                       STARGS             105
316700970114     O                       UTIME              119 '  :  :  '
316800970110     O          E            TESSIN         1 05
316900970110     O                       MAS(5)              66
317000970110     O                       MAS(7)             132
317100970110     O          E            DETSIN         1
317200970110     O                       ARBAAS               4
317300970110     O                       ARBLNP               8
317400970110     O                       ARBNRS        Z     12
317500970110     O                       ARBNSP        Z     20
317600970110     O                       ARBNAS              37
317700970110     O                       ARBRSM              74
317800020917     O                       ARBRMA              91
317900970110     O                       ARBRMN        Z    108
318000970110     O                       ARBNCL        Z    115
318100970110     O                       PESPRT        4    125
318200970110     O          E            TOTSIN         1
318300970110     O                       MAS(9)              66
318400970110     O                       MAS(9)             132
318500970110     O          E            TOTSIN         1
318600970110     O                       MAS(10)             66
318700970110     O                       MAS(11)            132
318800970110     O                       TOTSPE        Z     24
318900970110     O                       TOTNCL        Z     49
319000970110     O                       TOTPES        4     77
319100970110     O                       TOTVLF        4    103
319200970110     O          E            TOTSIN      1
319300970110     O                       MAS(9)              66
319400970110     O                       MAS(9)             132
319500970110     O          E            FINSIN      1
319600970110     O                                           69 '***   FINE  STAMPA   ***'
319700970110     O*
319800970110     O*
319900961212     O*****************************************************************
320000961212     O* MANIFEST SPEDIZIONI C.O.D.
320100961212     O*****************************************************************
320200961213     OMANCOD    E            TESCOD           02
320300041109     O                       D46(1)             112
320400961217     O                       STARAG              75
320500961217     O                       STPFAX              97
320600041109     O              N20                         112 '* PROVVISORIO'
320700041109     O               20                         112 '* DEFINITIVO '
320800961212     O          E            TESCOD           03
320900961121     O                       ST(1)               66
321000041109     O                       D46(2)             112
321100961217     O                       UDATE8              54 '  /  /    '
321200961217     O                       UTIME               89 '  :  :  '
321300041109     O                       PAGE2         Z    111
321400961212     O          E            TESCOD           04
321500961217     O                       ST(4)               66
321600041109     O                       D46(3)             112
321700961217     O                       RSUTM               63
321800961217     O                       STADLA             106
321900961212     O          E            TESCOD           05
322000961217     O                       ST(3)               66
322100041109     O                       D46(4)             112
322200961217     O                       UDATE8              54 '  /  /    '
322300041109     O                       D82CMR             112
322400961212     O          E            TESCOD           06
322500961217     O                       ST(19)              66
322600041109     O                       D46(5)             112
322700041109     O                       DATCMR              87 '  /  /    '
322800961216     O          E            TESCOD           07
322900041109     O                                          112 '* MANIFEST C.O.D.'
323000961212     O          E            TESCOD         1 08
323100961121     O                       ST(6)               66
323200041109     O                       D46(6)             112
323300961121     O          E            DETCOD      1
323400100330     O              N21      §LB14               16
323500961129     O               21      §LB8                16
323600961121     O                       STARMN        Z   +  1
323700961121     O                       STARSM            +  1
323800961121     O                       STARSD            +  2
323900961121     O                       STATBO            +  1
324000961121     O                       ARBNCL        Z   +  2
324100961125     O                       PESPRT        4   +  2
324200041109     O                    N34PRTCAS        4   +  2
324300961121     O          E            DETCOD      1
324400961121     O                       STADSP              16 '  /  /    '
324500130808     O**************         ARBCAM              36
324600130808     O**************         STALOM              53
324700130808     O                       STAINM              52
324800961121     O                       STAIND              79
324900961121     O                       ARBNAS             100
325000041109     O                    N34AR9VCA             112
325100961121     O          E            DETCOD      1
325200961121     O                     02                    13 '* PARTIAL *'
325300130808     O**************         ARBRMA              30
325400130808     O**************                             37 'P.IVA:'
325500130808     O**************         MITCPI              53
325600130808     O                       ARBCAM              36
325700130808     O                       STALOM              53
325800961121     O                       ARBCAD              63
325900961121     O                       STALOD              84
326000961121     O                                           91 'P.IVA:'
326100961121     O                       DESCPI             107
326200130808     O          E            DETCOD      1
326300130808     O                       ARBRMA              30
326400130808     O                                           37 'P.IVA:'
326500130808     O                       MITCPI              53
326600061127      * Contatto Nome/tel.
326700061127     O          E   N33      DETCOD      1
326800061127     O                                           37 'Contact Name:'
326900061127     O                       STACNA              72
327000061127     O                                           80 'Tel.:'
327100061127     O                       STACTE             106
327200061127     O
327300061127      *
327400961220     O          E   N34      DETCOD      1
327500041109     O                       STATIC             112
327600961121     O          E   N36      DETCOD      1
327700961121     O         OR   N35
327800041109     O                    N35STANT1              75
327900041109     O                    N36STANT2             111
328000961121     O          E   N36      DETCOD      0  1
328100961121     O         OR   N35
328200041109     O                    N35STANT1              75
328300041109     O                    N36STANT2             111
328400961121     O* EXCPT FITTIZIA
328500961121     O          E          36DETCOD      1  0
328600041109     O                                          112 ' '
328700961121     O          E            TOTCOD      1
328800961121     O                       ST(15)              66
328900041109     O                       D46(7)             112
329000961121     O          E            TOTCOD      1
329100961121     O                       ST(17)              66
329200041109     O                       D46(8)             112
329300961204     O                       CODSPE        Z     24
329400961204     O                       CODNCL        Z     50
329500961204     O                       CODPES        4     79
329600961204     O                       CODVLF        4    104
329700961121     O          E            TOTCOD      1
329800961121     O                       ST(15)              66
329900041109     O                       D46(7)             112
330000961121     O          E            FINCOD      1
330100961121     O                                           69 '***  END  OF  PRINT  ***'
330200961212     O*****************************************************************
330300961212     O* MANIFEST PER LA DOGANA
330400961212     O*****************************************************************
330500961213     OMANDGN    E            TESDGN           02
330600041109     O                       D46(1)             112
330700970117     O                       STARSX              75
330800970117     O                       D82DGX              97
330900041109     O*               N20               112 '* PROVVISORIO'
331000041109     O*                20               112 '* DEFINITIVO '
331100961212     O          E            TESDGN           03
331200961212     O                       ST(1)               66
331300041109     O                       D46(2)             112
331400961217     O                       UDATE8              54 '  /  /    '
331500961217     O                       UTIME               89 '  :  :  '
331600041109     O                       PAGE3         Z    111
331700961212     O          E            TESDGN           04
331800961217     O                       ST(4)               66
331900041109     O                       D46(3)             112
332000961217     O                       RSUTM               63
332100961217     O                       STADLA             106
332200961212     O          E            TESDGN           05
332300961217     O                       ST(3)               66
332400041109     O                       D46(4)             112
332500961217     O                       UDATE8              54 '  /  /    '
332600041109     O                       D82CMR             112
332700961212     O          E            TESDGN           06
332800961217     O                       ST(19)              66
332900041109     O                       D46(5)             112
333000041109     O                       DATCMR              87 '  /  /    '
333100961212     O          E            TESDGN         1 08
333200961212     O                       ST(6)               66
333300041109     O                       D46(6)             112
333400961212     O          E            DETDGN      1
333500100330     O              N21      §LB14               16
333600961212     O               21      §LB8                16
333700961212     O                       STARMN        Z   +  1
333800961212     O                       STARSM            +  1
333900961212     O                       STARSD            +  2
334000961212     O                       STATBO            +  1
334100961212     O                       ARBNCL        Z   +  2
334200961212     O                       PESPRT        4   +  2
334300041109     O                    N34PRTCAS        4   +  2
334400961212     O          E            DETDGN      1
334500961212     O                       STADSP              16 '  /  /    '
334600130808     O                       STAINM              52
334700130808     O*************          ARBCAM              36
334800130808     O*************          STALOM              53
334900130808     O                       STAINM              52
335000961212     O                       STAIND              79
335100961212     O                       ARBNAS             100
335200041109     O                    N34AR9VCA             112
335300961212     O          E            DETDGN      1
335400961218     O                     02                    13 '* PARTIAL *'
335500130808     O**************         ARBRMA              30
335600130808     O**************                             37 'P.IVA:'
335700130808     O**************         MITCPI              53
335800130808     O                       ARBCAM              36
335900130808     O                       STALOM              53
336000961212     O                       ARBCAD              63
336100961212     O                       STALOD              84
336200961212     O                                           91 'P.IVA:'
336300961212     O                       DESCPI             107
336400130808     O          E            DETDGN      1
336500130808     O                       ARBRMA              30
336600130808     O                                           37 'P.IVA:'
336700130808     O                       MITCPI              53
336800961220     O          E   N34      DETDGN      1
336900041109     O                       STATIC             112
337000961212     O          E   N36      DETDGN      1
337100961212     O         OR   N35
337200041109     O                    N35STANT1              75
337300041109     O                    N36STANT2             111
337400961212     O          E   N36      DETDGN      0  1
337500961212     O         OR   N35
337600041109     O                    N35STANT1              75
337700041109     O                    N36STANT2             111
337800961212     O* EXCPT FITTIZIA
337900961212     O          E          36DETDGN      1  0
338000041109     O                                          112 ' '
338100961212     O          E            TOTDGN      1
338200961212     O                       ST(15)              66
338300041109     O                       D46(7)             112
338400961212     O          E            TOTDGN      1
338500961212     O                       ST(17)              66
338600041109     O                       D46(8)             112
338700961212     O                       TOTSPE        Z     24
338800961212     O                       TOTNCL        Z     50
338900961212     O                       TOTPES        4     79
339000961212     O                       TOTVLF        4    104
339100961212     O          E            TOTDGN      1
339200961212     O                       ST(15)              66
339300041109     O                       D46(7)             112
339400961212     O          E            FINDGN      1
339500961212     O                                           69 '***  END  OF  PRINT  ***'
339600941222     O*
339700961212     O*****************************************************************
339800970114     O* LISTA DISCORDANZE VIA FAX
339900961212     O*****************************************************************
340000961213     OLISDIS    E            TESDIS           02
340100961217     O                       DIS(21)            132
340200961217     O                       STARAG              75
340300961217     O                       STPFAX              97
340400041109     O              N20                         112 '* PROVVISORIA'
340500041109     O               20                         112 '* DEFINITIVA '
340600961127     O          E            TESDIS           04
340700961125     O              N20      DIS(1)              66
340800961125     O              N20      DIS(2)             132
340900961125     O               20      DIS(17)             66
341000961125     O               20      DIS(18)            132
341100961125     O              N20      UDATE8              85 '  /  /    '
341200961129     O               20      UDATE8              67 '  /  /    '
341300961217     O                       UTIME              100 '  :  :  '
341400041109     O                       WPAG          Z    116
341500961127     O          E            TESDIS           06
341600961217     O                       DIS(3)              66
341700961217     O                       DIS(4)             132
341800961217     O                       D82NFV        Z     50
341900961217     O                       PARDFV              70 '  /  /    '
342000961217     O                       STADLA             115
342100961217     O          E            TESDIS           07
342200961217     O                       DIS(22)             66
342300961217     O                       DATCMR              55 '  /  /    '
342400961217     O                       D82CMR              91
342500961217     O          E            TESDIS           09
342600961122     O                       DIS(5)              66
342700961122     O                       DIS(8)             132
342800961217     O          E            TESDIS         1 10
342900961122     O                       DIS(6)              66
343000961122     O                       DIS(9)             132
343100961122     O          E            DETDIS         1
343200961127     O               16      KFLS                 3
343300961127     O               16      KLNA                 7
343400961129     O               16 17N18KNRS                10
343500961129     O               16 17N18                    11 '-'
343600961129     O               16   N18KNSC          Z     18
343700961129     O                  14   WNPS                22
343800961127     O               15      WPAAS               26
343900961127     O               15      WLNP                30
344000961127     O               15 17   KNRS                34
344100961127     O               15 17                       35 '-'
344200961127     O               15      WNSP          Z     42
344300961122     O                       KCEV                47
344400961122     O                                           48 '-'
344500970114     O                       DCEVE              108
344600961128     O                 N14   WFIL               112
344700961129     O                 N14   WNPS               115
344800961125     O          E            DETDI2         1
344900961202     O*                        COSPRT    68
345000961127     O                       WDTPA1              79 '  /  /    '
345100961129     O*
345200961129     O          E            RIGCNS         1
345300961129     O               12                           8 'PARCELS:'
345400961129     O               17      KNRS                10
345500961129     O               17                          11 '-'
345600961129     O                       NSC(1)              19
345700961129     O                       NSC(2)              27
345800961129     O                       NSC(3)              35
345900961129     O                       NSC(4)              43
346000961129     O                       NSC(5)              51
346100961129     O                       NSC(6)              59
346200961129     O                       NSC(7)              67
346300961129     O                       NSC(8)              75
346400961129     O                       NSC(9)              83
346500961129     O                       NSC(10)             91
346600961129     O                       NSC(11)             99
346700961129     O                       NSC(12)            107
346800961129     O                       NSC(13)            115
346900961127     O          E            FIT         1
347000961122     O*                                     ' '
347100961122     O          E            FINDIS      1
347200961127     O               20                          75 '***  END OF PRINT  ***'
347300961127     O              N20                          75 '***  FINE STAMPA  ***'
347400961217     O          E            DETDI3           19
347500961125     O              N20      DIS(15)             66
347600961125     O              N20      DIS(16)            132
347700961125     O               20      DIS(19)             66
347800970114     O*****************************************************************
347900970114     O* LISTA DISCORDANZE SU CARTA
348000970114     O*****************************************************************
348100970114     OLISDIC    E            TESDIC           02
348200970114     O                       DIS(21)            132
348300970114     O                       STARAG              75
348400970114     O                       STPFAX              97
348500041109     O              N20                         112 '* PROVVISORIA'
348600041109     O               20                         112 '* DEFINITIVA '
348700970114     O          E            TESDIC           04
348800970114     O                       DIS(1)              66
348900970114     O                       DIS(2)             132
349000970114     O                       UDATE8              85 '  /  /    '
349100970114     O                       UTIME              100 '  :  :  '
349200970114     O                       WPAG1         Z    116
349300970114     O          E            TESDIC           06
349400970114     O                       DIS(3)              66
349500970114     O                       DIS(4)             132
349600970114     O                       D82NFV        Z     50
349700970117     O               22                          54 'BIS'
349800970114     O                       PARDFV              70 '  /  /    '
349900970114     O                       STADLA             115
350000970114     O          E            TESDIC           07
350100970114     O                       DIS(22)             66
350200970114     O                       DATCMR              55 '  /  /    '
350300970114     O                       D82CMR              91
350400970114     O          E            TESDIC           09
350500970114     O                       DIS(5)              66
350600970114     O                       DIS(8)             132
350700970114     O          E            TESDIC         1 10
350800970114     O                       DIS(6)              66
350900970114     O                       DIS(9)             132
351000970114     O          E            DETDIC         1
351100970114     O               16      KFLS                 3
351200970114     O               16      KLNA                 7
351300970114     O               16 17N18KNRS                10
351400970114     O               16 17N18                    11 '-'
351500970114     O               16   N18KNSC          Z     18
351600970114     O                  14   WNPS                22
351700970114     O               15      WPAAS               26
351800970114     O               15      WLNP                30
351900970114     O               15 17   KNRS                34
352000970114     O               15 17                       35 '-'
352100970114     O               15      WNSP          Z     42
352200970114     O                       KCEV                47
352300970114     O                                           48 '-'
352400970114     O                       DCEV               108
352500970114     O                 N14   WFIL               112
352600970114     O                 N14   WNPS               115
352700970114     O          E            DETDC2         1
352800970114     O*                        COSPRT    68
352900970114     O                       WDTPA1              79 '  /  /    '
353000970114     O*
353100970114     O          E            RIGCNC         1
353200970114     O               12                           8 'PARCELS:'
353300970114     O               17      KNRS                10
353400970114     O               17                          11 '-'
353500970114     O                       NSC(1)              19
353600970114     O                       NSC(2)              27
353700970114     O                       NSC(3)              35
353800970114     O                       NSC(4)              43
353900970114     O                       NSC(5)              51
354000970114     O                       NSC(6)              59
354100970114     O                       NSC(7)              67
354200970114     O                       NSC(8)              75
354300970114     O                       NSC(9)              83
354400970114     O                       NSC(10)             91
354500970114     O                       NSC(11)             99
354600970114     O                       NSC(12)            107
354700970114     O                       NSC(13)            115
354800970114     O          E            FITC        1
354900970114     O*                                     ' '
355000970114     O          E            FINDIC      1
355100970114     O                                           75 '***  FINE STAMPA  ***'
355200970114     O          E            DETDC3           19
355300970114     O                       DIS(15)             66
355400970114     O                       DIS(16)            132
355500970110**
355600961217                           Date printed    :XX/XX/XXXX          Ti  1
355700961217me printed    :XX:XX:XX               Page:  xxxx                   2
355800961216                           Shipment date   :xx/xx/xxxx          CM  3
355900961216                           Exporting branch:1234567890123456789 Im  4
356000961216porting branch:1234567890123456789012345                            5
356100960223    Shipmentno    Rif.Exp  Exporter                   Consignee     6
356200960223    XXXXXXXXXXXX XXXXXXXXX xxxxxxxxxxxxxxxxxxxxxxxxx  XXXXXXXXXXXX  7
356300961011        XX/XX/XX           xxxxxxxxx xxxxxxxxxxxxxxxx xxxxxxxxxxxx  8
356400961011  * PARTIAL *  xxxxxxxxxxxxxxx P.IVA:xxxxxxxxxxxxxxxx xxxxxxxxx xx  9
356500961125              Imp Parcels   Weight    C.O.D.                       10
356600961011XXXXXXXXXXXXX xxx  xxxxx  xxxxxx,x  xxxxxxxxxx,xxx                  1
356700961011XXxxxxxxxxxxx      123456789012345             xxx                  2
356800961011XXxxxxxxxxxxxxxxxx P.IVA: xxxxxxxxxx                                3
356900940131
357000940111------------------------------------------------------------------  5
357100940111--------------------------------------------------                  6
357200961125  Total Shipments: xxxxx    Total Parcels: xxxxxxx    Total Weight  7
357300961125: xxxxxxxxx,x   Total Volume: xxxx,xxx                              8
357400961216                                                                CM 19
357500961216R Number      :12345678901234567890123456789012345                 20
357600961216R Date        :xx/xx/xxxx                                          21
357700931229**
357800961217                          ** LISTA DISCORDANZE SPEDIZIONI PER L'ES  1
357900961223TERO DEL XX/XX/XXXX **    XX:XX:XX       PAG. XXXX
358000961217                           FOGLIO DEFLUENZA :XXXXX      DEL XX/XX/  3
358100961217XXXX    LINEA DI ARRIVO:1234567890123456789012345                   4
358200961128  P  A  R  C  E  L  S    S H I P M E N T                            5
358300961122LHD LHA  NUMBER    O.R. YR LHD   NUMBER                             6
358400961121XXX XXX XX XXXXXXX  XX  XX XXX  XX XXXXXXX  XXX-123456789012345678  7
358500961128                                           SPUNTA                   8
358600961128                                           P.O O.R                  9
358700961128901234567890123456789012345678901234567890 XXX XX                  10
358800961127                                                123456789012345678 11
35890096112790 XX/XX/XXXX                                                      12
359000961129PARCELS:XX-1234567,1234567,1234567,1234567,1234567,1234567,1234567 13
359100961129,1234567,1234567,1234567,1234567,1234567,1234567                   14
359200970121                **  N E S S U N A   D I S C O R D A N Z A   R I S  15
359300970121C O N T R A T A  **                                                16
359400961217                                *** DISCREPANCY LIST DTD XX/XX/XXX 17
359500961223X ***                     XX:XX:XX       PAG. XXXX                 18
359600961212                             **  N O   D I S C R E P A N C Y  **   19
359700961217                           123456789012345678901234567890123456789 20
359800961219012345678 *Fax:1234567890123456                                    21
359900961217                           CMR..............:XX/XX/XXXX 1234567890 22
3600009612171234567890123456789012345                                          23
360100041109** D46
360200041109_________ *Fax:________________                                     1
360300041109me printed    :XX:XX:XX           Page:  xxxx                       2
360400041021porting branch:_________________________                            3
360500041109R Number  :___________________________________                      4
360600041109R Date    :xx/xx/xxxx                                               5
360700041021              Imp Parcels   Weight    C.O.D.                        6
360800041109----------------------------------------------                      7
360900041021: xxxxxxxxx,x   Total Volume: xxxx,xxx                              8
361000041021                                                                    9
361100041021                                                                   10
361200961213** CMD
361300961213OVRPRTF MANPTN QSYSPRT SAVE(*YES)                                    1
361400971230OVRPRTF MANPTN QSYSPRT OUTQ(FAXIN     ) SAVE(*YES)                   2
361500961213OVRPRTF MANCOD QSYSPRT SAVE(*YES)                                    3
361600971230OVRPRTF MANCOD QSYSPRT OUTQ(FAXIN     ) SAVE(*YES)                   4
361700961213OVRPRTF MANDGN QSYSPRT SAVE(*YES)                                    5
361800971230OVRPRTF MANDGN QSYSPRT OUTQ(FAXIN     ) SAVE(*YES)                   6
361900990512OVRPRTF LISDIC QSYSPRT SAVE(*YES)       HOLD(*YES)                   7
362000971230OVRPRTF LISDIS QSYSPRT OUTQ(FAXIN     ) SAVE(*YES)                   8
362100970110OVRPRTF MANPTS QSYSPRT SAVE(*YES)                                    9
362200971230OVRPRTF MANPTN QSYSPRT OUTQ(FAXOUT    ) SAVE(*YES) FORMTYPE('SAVEDI')10
362300971230                                                                     11
362400971230OVRPRTF LISDIS QSYSPRT SAVE(*YES)                                    12
362500970110**
362600970110XXXXXXXXXXXXXXXXXXXX/XXX                  ** MANIFEST SPEDIZIONI E   1
362700970110XPORT SINTETICO **               TRTC83R1    XX/XX/XXXX  PAG. XXXX   2
362800970121XXXXXXXXXX XXXXXXXXXX  Foglio Defl.XXXXX      Lna:123456789012345    3
362900970115 Partner:123456789012345678901234567890      XX:XX:XX                4
363000970110Anno Lnp Ser  Numero  Natura merce     Ragione sociale mittente      5
363100970110XXXX XXX  XX XXXXXXX  123456789012345  123456789012345678901234567   6
363200970110          Rif.mitt.alfab.  Rif.mitt.numer.  Colli    Peso            7
36330097011089012345  123456789012345  123456789012345  12345  123456,7          8
363400970110------------------------------------------------------------------   9
363500970110Totale Spedizioni: xxxxx    Totale Colli: xxxxxxx    Totale Peso:   10
363600970110xxxxxxxxx,x   Totale Volume: xxxx,xxx                               11
