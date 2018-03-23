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
010100110510     D dvpobrt       e DS
010200931229     D                 DS
010300941221     D  ARBAAS                 1      4  0
010400941221     D  ARBMGS                 5      8  0
010500941221     D  ARBDSP                 1      8  0
010600020225      *
010700960716     D WLBDA8          DS
010800960716     D  G02DAT                 1      8  0
010900960716     D  G02INV                 9     16  0
011000960716     D  G02ERR                17     17
011100960716     D  G02TGI                18     22  0
011200020225      *
011300960530     D                 DS
011400960530     D  AA                     1      4  0
011500960530     D  MM                     5      6  0
011600960530     D  GG                     7      8  0
011700960530     D  DATA                   1      8  0
011800020225      *
011900931229     D                 DS
012000960530     D  AAS                    1      4  0
012100960530     D  LNP                    5      7  0
012200960530     D  NRS                    8      9  0
012300960530     D  NSP                   10     16  0
012400960530     D  ARTSPE                 1     16  0
012500020225      *
012600020225     D TCUDS           DS
012700020225     D  F3                     3      3
012800020225     D  F4                     4      4
012900020225     D  F56                    5      6
013000020225      *      PER PRENDERE FASE ANOMALIA              -TRUL11R-
013100020225     D PARAM9          DS
013200020225     D  PA9TLA                 1      1    INZ('L')
013300020225     D  PA9NPG                 2      2    INZ('1')
013400020225     D  PA9SPG                 3      3    INZ(' ')
013500020225     D  FASE                   4      4    INZ
013600020225      *
013700020225     D                 DS
013800020225     D  OUTQPR                29     38
013900020225     D  CMDPRT                 1     69
014000020225      *     KEY PER CHAIN SUI FV
014100020225     D                 DS
014200020225     D  BRVNPG                 1      1  0
014300020225     D  BRVNFV                 2      7  0
014400020225     D  BRVFGS                 8     10  0
014500020225     D  BRVKEY                 1     10  0
014600020225      *
014700020225     D TRUL06DS      E DS
014800020225     D  £1                     1     90  0 DIM(30)                              Fil. gestite
014900020225      *
015000020225     D UT§DSE0F      E DS
015100020225     D  TCU                  398    697    DIM(50)                              PARAM FLG7/8,TIPC
015200020225     D  KCU                  698    847P 0 DIM(50)                              PARAM CAPI CONTO
015300020226     D                                     PACKEVEN
015400020225      *
015500020225     D KPJBA         E DS
015600020225     D  LIBSYS                92    100
015700020225      *
015800961115     D FLAN2           DS
015900961115     D  ANMNR2               397    400B 0
016000020225      *
016100020226     D OG143         E DS
016200020226     D DS3A          E DS
016300020225     D DS7A2         E DS
016400020225     D DS7N          E DS
016500090402     D dta4K         E DS
016600090402     D CNCR80        E DS
016700110511     D CNCR42        E DS
016800020225     D FNLR33DS      E DS
016900020225     D TRUL42DS      E DS
017000020225     D TRTC82DS      E DS
017100020228     D TRTC83DS1     E DS
017200020225     D EDIDS1A       E DS
017300130215      *-------
017400130215      *  Ds per reperimento tabella PT e campo risultato
017500130215     D DSPT          E DS                  EXTNAME(EDIDSPT)
017600130215     D DSPZ          E DS                  EXTNAME(EDIDSPZ)
017700130215      *-------
017800061127     D dar5gen       e ds
017900041020      *
018000041020     D* ds per ind. email
018100041020     Dtrul44ds       E DS                  INZ
018200050913     D trtcm1ds      E DS
018300060918     d
018400020225      *--------------------------------------------------------------*
018500020225      * DEFINIZIONE COSTANTI
018600020225      *--------------------------------------------------------------*
018700021009     D Return          C                   'R'
018800021009     D LR              C                   'L'
018900021009     D Eseguito        C                   '0'
019000021009     D Errore          C                   '1'
019100021009     D Generica        C                   '0'
019200021009     D POAmministr     C                   '1'
019300021009     D FaxIn           C                   '2'
019400021009     D FaxOut          C                   '3'
019500021009     D LetVettura      C                   '4'
019600021009      *
019700021009     D PrmFine         S              1    INZ(LR)
019800021009     D PrmTpOutQ       S              1
019900021009      *
020000021009     D PrmFilS         S              3S 0
020100021009     D PrmOutQ         S             10
020200021009     D PrmOutQLib      S             10
020300021009     D PrmEsito        S              1
020400021009      *
020500021009     D MyOutQ          S             10
020600021009     D MyOutQLib       S             10
020700021009      *
020800021009     D FaxInOutq       S             10
020900021009     D FaxInLibq       S             10
021000021009      *
021100021009     D FaxOutOutq      S             10
021200021009     D FaxOutLibq      S             10
021300041109      *
021400041109     D Email_PTN       S              1
021500041109     D Email_COD       S              1
021600041109     D Email_DGN       S              1
021700041109      *
021800041109     D PRT_PTN_TES     S              1
021900041109     D PRT_COD_TES     S              1
022000041109     D PRT_DGN_TES     S              1
022100021009      * --------------------------------------------------------
022200961128     D COS1            C                   CONST('PRIMA DATA INOLTRO :')
022300020225      *             'INITIAL DATE TRANSM.'C         COS2
022400020225
022500020225      *--------------------------------------------------------------*
022600020225      * DEFINIZIONE CAMPI
022700020225      *--------------------------------------------------------------*
022800020225     D SAVCBO          S                   LIKE(ARBcbo)
022900020225     D STATBO          S              3
023000020225     D Wdis4           S              1
023100060918     D XX              S              3  0
023200060918     D YY              S              3  0
023300020808     D*
023400020808     D KDT3            S                   LIKE(ARBDT3)
023500020808     D KAAS            S                   LIKE(ARBAAS)
023600020808     D KNSP            S                   LIKE(ARBNSP)
023700020808     D*
023800020808     D WKPJBU          S                   LIKE(KPJBU )
023900020808     D KFLS            S                   LIKE(ARTFLS)
024000020808     D KLNA            S                   LIKE(ARTLNA)
024100020808     D KNRS            S                   LIKE(ARTNRS)
024200020808     D KNSC            S                   LIKE(ARTNSC)
024300020808     D WAAS            S                   LIKE(ARTAAS)
024400020808     D WLNP            S                   LIKE(ARTLNP)
024500020808     D WNSP            S                   LIKE(ARTNSP)
024600020808     D KDTR            S                   LIKE(ARTDTR)
024700020808     D KTRC            S                   LIKE(AR4TRC)
024800020808     D STANT1          S                   LIKE(AR4NOT)
024900020808     D STANT2          S                   LIKE(AR4NOT)
025000020808     D*
025100020808     D KNPG            S                   LIKE(BRVNPG)
025200020808     D KNFV            S                   LIKE(BRVNFV)
025300020808     D D82NFS          S                   LIKE(BRVNFV)
025400020808     D KFGS            S                   LIKE(BRVFGS)
025500020808     D KLNP            S                   LIKE(BRVLNP)
025600020808     D KFLE            S                   LIKE(BRVFLE)
025700020808     D WRVZNC          S                   LIKE(BRVZNC)
025800040225     D*SA1KEY          S                   LIKE(BRVKEY)
025900020808     D*
026000020808     D KCEV            S                   LIKE(EVSCEV)
026100020808     D WCODIS          S                   LIKE(EVSCEV)
026200020808     D WDTPAR          S                   LIKE(EVSDAT)
026300020808     D WDTPA1          S                   LIKE(EVSDAT)
026400020808     D WFIL            S                   LIKE(EVSFIL)
026500020808     D WNPS            S                   LIKE(EVSNPS)
026600020808     D WDEV            S                   LIKE(EVSDEV)
026700040415     D WDEV2a          S                   LIKE(EVSDEV)
026800020808     D*
026900020808     D KCAA            S                   LIKE(ANMCAA)
027000020808     D DFV06           S                   LIKE(ANMDAO)
027100020808     D*
027200020808     D KKUT            S                   LIKE(TBLKUT)
027300020808     D KCOD            S                   LIKE(TBLCOD)
027400020808     D KKEY            S                   LIKE(TBLKEY)
027500020808     D*
027600020808     D SAVSPE          S                   LIKE(ARTSPE)
027700020808     D WPAG            S                   LIKE(PAGE  )
027800020808     D WPAG1           S                   LIKE(PAGE  )
027900020808     D*
028000020808     D PARDFV          S                   LIKE(D82DFV)
028100020808     D STADSP          S                   LIKE(D82DFV)
028200020808     D UDATE8          S                   LIKE(D82DFV)
028300020808     D*
028400020808     D STPFAX          S                   LIKE(INDTLF)
028500020808     D*
028600020808     D LNAEXP          S                   LIKE(D82LNP)
028700020808     D WTRC            S                   LIKE(TRETRC)
028800020808     D KECOD           S                   LIKE(TABCOD)
028900020808     D KEKEY           S                   LIKE(TABKEY)
029000020808     D STATIC          S                   LIKE(§1ADES)
029100090402     d
029200090402     d WTA430          s             21    inz('GAITRAGRU /TITA430C')
029300090402     d WTAS30          s             21    inz('GAITRAGRU /TITAS30C')
029400020808      *
029500020808     D WARFOR          S              1    inz
029600060918     D wcreadis        S              1    inz
029700070220     D wtimez          S               z
029800070220     D dataiso         S               d   datfmt(*iso)
029900070220     D oraiso          S               t   timfmt(*iso)
030000070220
030100070220     D                SDS
030200070220     D  NOMPGM                 1     10
030300120821     D FEDEX           C                   CONST('D')
030400120821     D NETEXP          C                   CONST('N')
030500120821     D SCANDPD         C                   CONST('P')
030600120821     D SCANDPDEDI      C                   CONST('S')
030700120821     D EDIESTAMPA      C                   CONST('Z')
030800120821     D EDINOPRINT      C                   CONST('W')
030900120821     D PRINTNOEDI      C                   CONST('M')
031000120821     D SOLOSTAMPA      C                   CONST(' ')
031100140311
031200140311      ** ______
031300140311      ***  per gestire il passaggio dei tracciati SCAN e GEOPOST
031400140311      ***    valori:     V=Vecchio / N= Nuovo / E=Entrambe
031500140311      **
031600140311     D DPD_OLD_NEW     s              1    inz('E')
031700140311     d flagHDE         s              1    inz('H')
031800140311      **
031900140311      ** ______
032000020225
032100090402     ITITAS000      81
032200090402     ITITAS010      82
032300090402     ITITASP00      83
032400090402     I
032500090402     ITITA4000      84
032600090402     ITITA4P00      85
032700020225      ****************************************************************
032800020225      *  RIEPILOGO INDICATORI
032900020225      ***************************************************************
033000060918      * 01    - un collo della spedizione in  IMA
033100020225      * 02    - STAMPO 'PARTIAL' NEL MANIFEST SE INOLTRO PARZIALE
033200020225      * 04    - STAMPATO QUALCOSA NEL MANIFEST PER IL PARTNER
033300020225      * 05    - STAMPATO QUALCOSA NEL MANIFEST SPEDIZIONI C.O.D.
033400020225      * 06    - STAMPATO QUALCOSA NEL MANIFEST PER LA DOGANA
033500020225      * 08    - STAMPATO QUALCOSA NELLA LISTA DISCORDANZE
033600020225      * 09    - ON: COLLO A TERRA
033700020225      * 12    - PRIMA RIGA DI STAMPA ELENCO SEGNACOLLI PER DISC.4
033800020225      * 14    - ON - IN LISTA DISCORD. NON STAMPO IL P.O. SPUNTA COLLO
033900020225      * 15    - ON - HO LA KEY PER SPEDIZIONE PER STAMPA
034000020225      * 16    - ON - HO LA KEY PER SEGNACOLLO PER STAMPA
034100020225      * 17    - ON - HO LA SERIE
034200060913      * 18    - ON - STAMPO DISCORDANZA E4  - E8
034300020225      * 20    - ON - ELABORAZIONE DEFINITIVA
034400020225      * 22    - ON - FOGLIO VIAGGIO BIS
034500020226      * 31/32 - DI COMODO
034600061127      * 33    - STAMPO I DATI RELATIVI AL CONTATTO NOME/TEL
034700020225      * 34    - STAMPO I DATI RELATIVI AL C/ASSEGNO
034800020225      * 35/36 - STAMPO I DATI RELATIVI ALLE NOTE
034900020225      * 42    - CREO SPUNTA DA CAMPI DI FNANM
035000020225      * 43    - CREO SPUNTA DA CAMPI DI FNART
035100020225
035200020225      *****************************************************************
035300020225      * CICLO PRINCIPALE
035400020225      *****************************************************************
035500020225      * SE FV GIA' CHIUSO VADO A FINE PGM
035600020808     ***C     D82NFV        CHAIN     FNFGV01L
035700020808     C     K02FGV01      CHAIN     FNFGV01L
035800040317     C                   if        %found(FNFGV01L) and FGVFCF = 'S'
035900970127     C                   GOTO      FINEL
036000020225     C                   endif
036100020225      *
036200020225      * Reperisco dati Partner in base al codice cliente
036300960529     C                   EXSR      DATPAT
036400041020      *
036500020225      * Ovrprtf su file Printer
036600940131     C                   EXSR      OVRPRT
036700020225      *
036800020225      * GIRO SU ARB PER SPEDIZIONI DEL MANIFEST E DISCORDANZA 4
036900120823      *                *------------------*
037000961111     C                   EXSR      REDSPE
037100120823      *                *------------------*
037200020225      * GIRO SU ART E ANOMALIE PER LE ALTRE DISCORDANZE E AGGIORNARE
037300020225      * ARTDTR PER I COLLI CHE PARTONO
037400020225      * Se elaborazione definitiva o se richiesta lista discordanze
037500020225     c                   IF        *in20 = *on  or  D82lds = 'SI'
037600961113     C                   EXSR      CTRDIS
037700020225     C                   ENDIF
037800020225      *
037900020225      * FINE STAMPE
038000020225    1C     *IN08         IFEQ      *ON                                          LISTA DISCORD.
038100120821     c     d82ftt        ifne      FEDEX
038200040423
038300040423     c                   if        FAX_Disc_Est = 'S'
038400020225     C   20              EXCEPT    FINDIS                                       via fax
038500040423     c                   end
038600040423
038700020301     c                   endif
038800020225     C                   EXCEPT    FINDIC                                       su carta
038900020225   x1C                   ELSE
039000020225    2C     D82LDS        IFEQ      'SI'
039100970313     C* Al partner trasmetto LISDIS vuoto solo se ho stampato qualcosa
039200020225     C* nel manifest (solo se definitivo
039300020301     c                   if        *IN04 = *on AND *IN20 = *ON
039400120821     c                             and d82ftt <> FEDEX
039500040423
039600040423     c                   if        FAX_Disc_Est = 'S'
039700020225     C                   ADD       1             WPAG
039800020225     C                   EXCEPT    TESDIS
039900020225     C                   EXCEPT    DETDI3
040000040423     c                   end
040100040423
040200020225    3C                   endif
040300970114     C                   ADD       1             WPAG1
040400970114     C                   EXCEPT    TESDIC
040500970114     C                   EXCEPT    DETDC3
040600020225    2C                   ENDIF
040700020225    1C                   ENDIF
040800020225      *
040900961213     C     *IN04         IFEQ      *ON                                          MANIFEST PARTN
041000020225      *
041100020225     C                   if        *IN20 = *ON                                  MANIFEST PARTN
041200020225     C   OF              EXCEPT    TESTA
041300020225     C                   EXCEPT    TOTAL
041400020225     C                   EXCEPT    FINE
041500020225     c                   else
041600020225     C   OC              EXCEPT    TESSIN
041700020225     C                   EXCEPT    TOTSIN
041800020225     C                   EXCEPT    FINSIN
041900020225     c                   endif
042000020225      *
042100970110     C                   ELSE
042200020225     C                   if        D82STB = 'SI' and *in20 = *off
042300020225     C                   EXCEPT    TESSIN
042400020225     C                   EXCEPT    FINSIN
042500020225     C                   endif
042600970115     C                   ENDIF
042700961212     C*
042800961213     C     *IN06         IFEQ      *ON                                          MANIFEST DOG
042900120821     c     d82ftt        andne     FEDEX
043000961212     C   OD              EXCEPT    TESDGN
043100961212     C                   EXCEPT    TOTDGN
043200961212     C                   EXCEPT    FINDGN
043300961212     C                   ENDIF
043400020225      *
043500961213     C     *IN05         IFEQ      *ON                                          MANIFEST COD
043600120821     c     d82ftt        andne     FEDEX
043700961121     C   OG              EXCEPT    TESCOD
043800961121     C                   EXCEPT    TOTCOD
043900961121     C                   EXCEPT    FINCOD
044000961121     C                   ENDIF
044100020225      *
044200020225      * Per elaborazione definitiva
044300020225      * Richiamo scrittura file x ultima volta con numero sped = 0
044400020225     C                   SELECT
044500020225     C                   WHEN      *IN20 = *off
044600020917      * EUROEXPRESS in sede
044700051221      *   o DPD x SEUR (EDI)
044800120821     c                   WHEN      D82FTT = EDINOPRINT
044900120821     c                              or
045000120821     c                             D82FTT = EDIESTAMPA
045100120821     c                              or
045200120821     c                             D82FTT = SCANDPDEDI
045300020225     C                   clear                   D82AAS
045400020225     C                   clear                   D82LNP
045500020225     C                   clear                   D82NRS
045600020225     C                   clear                   D82NSP
045700020225     C                   MOVEL     TRTC82DS      KPJBU
045800960530     C                   CALL      'TRTC83R2'
045900960530     C                   PARM                    KPJBA
046000120823      **
046100120821      * GEL   (Scrittura del file NET EXPRESS EUROPE)
046200120821     c                   WHEN      D82FTT = NETEXP
046300120821     C                   clear                   F83AAS
046400120821     C                   clear                   F83LNP
046500120821     C                   clear                   F83NRS
046600120821     C                   clear                   F83NSP
046700120821     c                   clear                   f83dfv
046800120821     C                   clear                   KPJBU
046900120821     C                   MOVEL     TRTC83DS1     KPJBU
047000120905     C                   CALL      'TRTC83G2R'
047100120821     C                   PARM                    KPJBA
047200120821      * REIMPOSTO KPJBU "ORIGINALE"
047300120821     C                   MOVEL     WKPJBU        KPJBU
047400120821      *
047500020225      * FEDEX
047600120821     c                   WHEN      D82FTT = FEDEX
047700020228     C                   clear                   F83AAS
047800020228     C                   clear                   F83LNP
047900020228     C                   clear                   F83NRS
048000020228     C                   clear                   F83NSP
048100020228     c                   clear                   f83dfv
048200020228     C                   MOVEL     TRTC83DS1     KPJBU
048300020225     C                   CALL      'TRTC83R21'
048400971230     C                   PARM                    KPJBA
048500020304      * REIMPOSTO KPJBU "ORIGINALE"
048600020304     C                   MOVEL     WKPJBU        KPJBU
048700981106      *
048800020225     C                   ENDSL
048900020225      *
049000020225      * SCRIVO RECORD IN WORK FILE PER LA LNA APPENA ELABORATA
049100970527     C                   CLEAR                   TREXP000
049200970527     C  N20              MOVEL     'P'           TRETRC
049300120821     C   20              MOVEL     'D'           TRETRC
049400970527     C                   MOVE      D82NFV        TRENFV
049500970527     C                   MOVE      SIMFEL        TRELNP
049600970527     C                   MOVE      LNAEXP        TRELNA
049700020225      * MEMORIZZO TOTALI SOLO SE PROVVISORIO O SE ELABORAZ.PER DOGANA
049800020225      * (INFATTI LA STAMPA DEI TOTALI DEL FV SERVE PER LA COMPILAZIONE
049900020225      * DEL MODULO CMR E DA TRASMETTERE ALLA DOGANA INSIEME AL MANIFEST
050000020301     C                   IF        *IN20 = *OFF  or (D82DGN = 'SI' and
050100120821     C                             d82ftt <> FEDEX)
050200970527     C                   Z-ADD     TOTPES        TRETOP
050300050520     C                   Z-ADD     totvlf        TRETOv
050400970527     C                   Z-ADD     TOTNCL        TRETOC
050500020225     C                   ENDIF
050600020225      *
050700970527     C                   WRITE     TREXP000                             31
050800020225      *
050900020225      * Reimposto LNA salvata nella DS
051000970123     C                   MOVE      LNAEXP        D82LNP
051100020225      *
051200970127     C     FINEL         TAG
051300020225      *
051400020225      *
051500020225     C                   eval      *inLR = *ON
051600960529     C*****************************************************************
051700970114     C*  Reperisco i dati del partner
051800960529     C*****************************************************************
051900960529     C     DATPAT        BEGSR
052000960529     C*
052100961127     C                   MOVEL     *ALL'?'       STADLA           25
052200960529     C*  Decodifico lna
052300020226     C     LNAEXP        CHAIN     AZORG
052400020226     C                   movel     orgde3        OG143
052500020226     C                   MOVEL(P)  ORGDES        STADLA
052600970110     C*
052700970110     C                   MOVEL     STADLA        STADLS           15
052800970117     C*  Reperisco numero fax e ragione sociale partner
052900961127     C*
053000961127     C     KIND          CHAIN     CNACO00F                           31
053100961217     C  N31              MOVEL     ACORAG        STARAG           48
053200961127     C   31              CLEAR                   STARAG
053300970110     C                   MOVEL     STARAG        STARGS           30
053400961127     C*
053500961122     C                   CLEAR                   STPFAX
053600961122     C     KIND          CHAIN     CNIND00F                           31
053700961122     C     *IN31         IFEQ      *OFF
053800020225     C                   CLEAR                   TRUL42DS
053900961122     C                   MOVEL     INDTLF        D42FAX
054000961122     C                   CALL      'TRUL42R'
054100020225     C                   PARM                    TRUL42DS
054200961122     C     D42ERR        IFEQ      ' '
054300961122     C                   MOVEL     INDTLF        STPFAX
054400961122     C                   END
054500961216     C                   END
054600970117     C*
054700970117     C* PER COMODITA' MUOVO RAGIONE SOCIALE DOGANA IN CAMPO LUNGO 48
054800970117     C                   MOVEL     D82RSX        STARSX           48
054900041020     C*
055000041020     C* Nuovo Concetto:
055100041020     C*  L'indirizzo E-mail è prioritario rispetto al FAX ossia se presente
055200041020     C*  in anagrafica oltre al nr.FAX l'invio del MANIFEST non viene più
055300041020     C*  eseguito mediante FAX ma per posta Elettronica.
055400041020     C* Prende indirizzo e-mail del Partner in anagrafica Note.
055500041020     C*
055600041020     C*
055700960529     C* Pulisco  i campi di totale x partner
055800960529     C                   CLEAR                   TOTSPE
055900960529     C                   CLEAR                   TOTNCL
056000961122     C                   CLEAR                   TOTPES
056100960529     C                   CLEAR                   TOTVLF
056200961204     C                   CLEAR                   CODSPE
056300961204     C                   CLEAR                   CODNCL
056400961204     C                   CLEAR                   CODPES
056500961204     C                   CLEAR                   CODVLF
056600960530     C*
056700960530     C                   ENDSR
056800960529     C*****************************************************************
056900960529     C* OVRPRTF: Ovrprtf file di stampa
057000960529     C*****************************************************************
057100960529     C     OVRPRT        BEGSR
057200021009     C*
057300041021     C                   Exsr      Code_FAX
057400041020     C*
057500971230     C                   Z-ADD     69            LUNG
057600961213     C**
057700961213     C* OVRPRTF MANIFEST PER PARTNER
057800961213     C**
057900961213    1C     D82STB        IFEQ      'SI'
058000970205     C* E l a b o r a z i o n e   d e f i n i t i v a
058100961213    2C     *IN20         IFEQ      *ON
058200041020     C**
058300970205    3C                   SELECT
058400041020     C**
058500970224     C* Se ho trovato il n.fax, ed è previsto l'invio della stampa
058600020917     c* (ftt = ' ' )
058700970205     C* --> metto spool in faxin per invio fax in automatico
058800970205     C     STPFAX        WHENNE    *BLANKS
058900120821     C     D82FTT        ANDEQ     EDIESTAMPA
059000120821     C     STPFAX        orNE      *BLANKS
059100120821     C     D82FTT        ANDEQ     NETEXP
059200971230     C                   MOVEL     CMD(2)        CMDPRT
059300021009     C                   MOVEL     FaxInOutQ     OUTQPR
059400971230     C                   MOVEL     CMDPRT        COMMAN
059500041020     C**
059600020917     C* Se è prevista solo la trasmiEDI  (FTT='W')
059700970205     c* --> metto spool in faxout per avere una copia di sicurezza
059800970205     C*     nel caso la trasmissione non dovesse riuscire
059900120821     C     D82FTT        WHENEQ    EDINOPRINT
060000120821     C     D82FTT        OREQ      FEDEX
060100120821     C     D82FTT        OREQ      SCANDPD
060200120821     C     D82FTT        OREQ      SCANDPDEDI
060300971230     C                   MOVEL     CMD(10)       CMDPRT
060400021009     C                   MOVEL     FaxOutOutQ    OUTQPR
060500971230     C                   MOVEL     CMDPRT        COMMAN
060600041020     C**
060700970205     C* altrimenti lo lascio nella coda di stampa di default
060800970205     C                   OTHER
060900961213     C                   MOVEL     CMD(1)        COMMAN
061000970205    3C                   ENDSL
061100041021     C**
061200041021     C**  però adesso bisogna fare Attenzione:  --> E-MAIL !!!!
061300041021     C**    se c'è l'indirizzo E-mail e prima era previsto l'invio con il FAX,
061400041021     C**    non deve essere più inviato il FAX ma tutto viene trasmesso con e-mail.
061500041021     C**  Il TRUL44R esegue anche l'OVRPRTF x e-mail.
061600120821     C     D82FTT        ifeq      EDIESTAMPA
061700120821     C     D82FTT        oreq      PRINTNOEDI
061800120821     C     D82FTT        oreq      NETEXP
061900041021     c                   clear                   trul44ds
062000050913     c                   clear                   trtcm1ds
062100041021     c                   eval      d44pgm = 'TRTC82R'
062200041021     c                   move      d82cpt        d44ksc
062300041021     c                   eval      d44apl = 'C'
062400041118     c                   eval      d44nk1 = '0151' + %editc(D82CPT:'X')
062500041021     c                   eval      d44tnt = '86'
062600041021     c                   eval      d44scope='*JOB'
062700041021     c                   eval      d44ovr = 'S'
062800120305     c***66              eval      d44obj = '*OBJM*Bartolini Shipment '+
062900120305     c   66              eval      d44obj = '*OBJM*Bartolini-BRT Shipment '+
063000110609     c                             'Manifest del ' + %editw(PARDFV:'  /  /    ')
063100110609     c                              + ' - ' + %trim(Starag)
063200120305     c**N66              eval      d44obj = '*OBJM*Bartolini-BRT Shipment '+
063300120305     c  N66              eval      d44obj = '*OBJM*BRT Shipment '+
063400110609     c                             'Manifest del ' + %editw(PARDFV:'  -  -    ')
063500110609     c                              + ' - ' + %trim(Starag)
063600041021     c                   eval      d44prtf = 'MANPTN'
063700041021     c                   call      'TRUL44R'
063800041021     c                   parm                    trul44ds
063900050913     c                   parm                    trtcm1ds
064000041025      **  Se tutto OK salta la QCMDEXEC
064100041025      **   con la vecchia OVRPRTF
064200041109     c     d44err        ifeq      *blank
064300041109     c                   eval      Email_PTN='S'
064400041109     c                   goto      gia_fatta
064500041109     c                   end
064600041021     c                   end
064700041021     C**
064800041109     C**  Viene eseguita se non è stata mandata via E-mail
064900041109     C**   notare il TAG seguente
065000970115     C                   CALL      'QCMDEXC'                            90
065100970115     C                   PARM                    COMMAN           80
065200970115     C                   PARM                    LUNG             15 5
065300041021     c     gia_fatta     tag
065400041020     C**
065500041020     C**  quindi apre il Manifest x il Partner
065600970110     C                   OPEN      MANPTN
065700041020     C**
065800970110   X2C                   ELSE
065900970205     C* E l a b o r a z i o n e   p r o v v i s o r i a
066000970110     C* Se elaborazione provvisoria stampo il manifest in forma sinte-
066100970110     C* tica
066200970110     C                   MOVEL     CMD(9)        COMMAN
066300970110     C                   CALL      'QCMDEXC'                            90
066400970110     C                   PARM                    COMMAN           80
066500970110     C                   PARM                    LUNG             15 5
066600970110     C                   OPEN      MANPTS
066700970110    2C                   END
066800961213    1C                   END
066900041020     C** --------------
067000041020     C**
067100961213     C* OVRPRTF MANIFEST PER DOGANA
067200020301     c* Per Fedex non devo mai stampare il manifest per dogana
067300961213     C**
067400961213    1C     D82DGN        IFEQ      'SI'
067500120821     c     D82FTT        andne     FEDEX
067600041020     C**
067700970114     C* Se ho trovato il n.fax --> metto in faxin per invio fax autom.
067800970117    2C     D82DGX        IFNE      *BLANKS
067900041021     C**
068000971230     C                   MOVEL     CMD(6)        CMDPRT
068100021009     C                   MOVEL     FaxInOutQ     OUTQPR
068200971230     C                   MOVEL     CMDPRT        COMMAN
068300041020     C**
068400961213   X2C                   ELSE
068500961213     C* altrimenti lo lascio nella coda di stampa di default
068600961213     C                   MOVEL     CMD(5)        COMMAN
068700961213    2C                   END
068800041021     C**
068900041021     C**  però adesso bisogna fare Attenzione:  --> E-MAIL !!!!
069000041021     C**    se c'è l'indirizzo E-mail e prima era previsto l'invio con il FAX,
069100041021     C**    non deve essere più inviato il FAX ma tutto viene trasmesso con e-mail.
069200041021     C**  L'OVRPRTF è in base ai parametri restituiti dal TRUL44R.
069300041021     C**      è possibile eseguire tramite il TRUL44R l'OVRPRTF necessaria.
069400041021     c                   clear                   trul44ds
069500050913     c                   clear                   trtcm1ds
069600041021     c                   eval      d44pgm = 'TRTC82R'
069700041025     c                   eval      d44cl = '200'
069800041021     c                   move      D82CPT        d44ksc
069900041021     c                   eval      d44scope='*JOB'
070000041021     c                   eval      d44ovr = 'S'
070100120305     c***66              eval      d44obj = '*OBJM*Bartolini Shipment '+
070200120305     c   66              eval      d44obj = '*OBJM*Bartolini-BRT Shipment '+
070300110609     c                             'Manifest del ' + %editw(PARDFV:'  /  /    ')
070400110609     c                              + ' - ' + %trim(D82RSX)
070500120305     c**N66              eval      d44obj = '*OBJM*Bartolini-BRT Shipment '+
070600120305     c  N66              eval      d44obj = '*OBJM*BRT Shipment '+
070700110608     c                             'Manifest del ' + %editw(PARDFV:'  .  .    ')
070800110608     c                              + ' - ' + %trim(D82RSX)
070900041021     c                   eval      d44prtf = 'MANDGN'
071000041021     c                   call      'TRUL44R'
071100041021     c                   parm                    trul44ds
071200050913     c                   parm                    trtcm1ds
071300041025      **  Se tutto OK salta la QCMDEXEC
071400041025      **   con la vecchia OVRPRTF
071500041109     c     d44err        ifeq      *blank
071600041109     c                   eval      Email_DGN='S'
071700041109     c                   goto      salta_OVRPRTF
071800041109     c                   end
071900041020      **
072000041109     C**  Viene eseguita se non è stata mandata via E-mail
072100041109     C**   notare il TAG seguente
072200970115     C                   CALL      'QCMDEXC'                            90
072300970115     C                   PARM                    COMMAN           80
072400970115     C                   PARM                    LUNG             15 5
072500041021     c     salta_OVRPRTF tag
072600041020      **
072700961213     C                   OPEN      MANDGN
072800961213    1C                   END
072900961213     C**
073000961213     C* OVRPRTF MANIFEST PER SPEDIZIONI C.O.D.
073100020301     c* Per fedex non stampo mai il manifest delle sped. c.o.d.
073200961213     C**
073300961213    1C     D82COD        IFEQ      'SI'
073400120821     c     D82FTT        ANDNE     FEDEX
073500041020     C**
073600961213     C* Se elaboraz.defin. e ho trovato il n.fax --> metto spool in
073700961213     C* in faxin per invio fax in automatico
073800961213    2C     *IN20         IFEQ      *ON
073900961213     C     STPFAX        ANDNE     *BLANKS
074000041021     C**
074100971230     C                   MOVEL     CMD(4)        CMDPRT
074200021009     C                   MOVEL     FaxInOutQ     OUTQPR
074300971230     C                   MOVEL     CMDPRT        COMMAN
074400041020     C**
074500961213   X2C                   ELSE
074600961213     C* altrimenti lo lascio nella coda di stampa di default
074700961213     C                   MOVEL     CMD(3)        COMMAN
074800961213    2C                   END
074900041021     C**
075000041021     C**  però adesso bisogna fare Attenzione:  --> E-MAIL !!!!
075100041021     C**    se c'è l'indirizzo E-mail e prima era previsto l'invio con il FAX,
075200041021     C**    non deve essere più inviato il FAX ma tutto viene trasmesso con e-mail.
075300041021     C**  Per l'OVRPRTF tramite posta elettronica si utilizza il TRUL44R
075400041021     C**  che imposta i parametri oltre che reperisce anche l'indirizzo e-mail.
075500041021    2C     *IN20         IFEQ      *ON
075600041021     c                   clear                   trul44ds
075700050913     c                   clear                   trtcm1ds
075800041021     c                   eval      d44pgm = 'TRTC82R'
075900041021     c                   move      d82cpt        d44ksc
076000041021     c                   eval      d44apl = 'C'
076100041118     c                   eval      d44nk1 = '0151' + %editc(D82CPT:'X')
076200041021     c                   eval      d44tnt = '86'
076300041021     c                   eval      d44scope='*JOB'
076400041021     c                   eval      d44ovr = 'S'
076500120305     c***66              eval      d44obj = '*OBJM*Bartolini C.O.D. '+
076600120305     c   66              eval      d44obj = '*OBJM*Bartolini-BRT C.O.D. '+
076700110609     c                             'Manifest del ' + %editw(PARDFV:'  /  /    ')
076800110609     c                              + ' - ' + %trim(Starag)
076900120305     c**N66              eval      d44obj = '*OBJM*Bartolini-BRT C.O.D. '+
077000120305     c  N66              eval      d44obj = '*OBJM*BRT C.O.D. '+
077100110608     c                             'Manifest del ' + %editw(PARDFV:'  .  .    ')
077200110608     c                              + ' - ' + %trim(Starag)
077300041021     c                   eval      d44prtf = 'MANCOD'
077400041021     c                   call      'TRUL44R'
077500041021     c                   parm                    trul44ds
077600050913     c                   parm                    trtcm1ds
077700041025      **  Se tutto OK salta la QCMDEXEC
077800041025      **   con la vecchia OVRPRTF
077900041109     c     d44err        ifeq      *blank
078000041109     c                   eval      Email_COD='S'
078100041109     c                   goto      salta_OVR
078200041109     c                   end
078300041021    1C                   END
078400041020     C**
078500041109     C**  Viene eseguita se non è stata mandata via E-mail
078600041109     C**   notare il TAG seguente
078700970115     C                   CALL      'QCMDEXC'                            90
078800970115     C                   PARM                    COMMAN           80
078900970115     C                   PARM                    LUNG             15 5
079000041021     c     salta_OVR     tag
079100041021     C**
079200961213     C                   OPEN      MANCOD
079300961213    1C                   END
079400961213     C**
079500961213     C* OVRPRTF PER LISTA DISCORDANZE
079600961213     C**
079700970110   1AC     D82LDS        IFEQ      'SI'
079800970114     C* lista discordanze via fax se elaborazione definitiva e ho il n.
079900020301     C* fax
080000961213    1C     *IN20         IFEQ      *ON
080100120821     c     d82ftt        andne     FEDEX
080200040423
080300040423     c                   if        FAX_Disc_Est = 'S'
080400970115     C     STPFAX        IFNE      *BLANKS
080500971230     C                   MOVEL     CMD(8)        CMDPRT
080600021009     C                   MOVEL     FaxInOutQ     OUTQPR
080700971230     C                   MOVEL     CMDPRT        COMMAN
080800970115     C                   ELSE
080900970115     C                   MOVEL     CMD(12)       COMMAN
081000970115     C                   END
081100970115     C                   CALL      'QCMDEXC'                            90
081200970115     C                   PARM                    COMMAN           80
081300970115     C                   PARM                    LUNG             15 5
081400970114     C                   OPEN      LISDIS
081500040423     c                   end
081600040423
081700970114    1C                   END
081800970114     C*
081900970114     C* lista discordanze sempre su carta
082000961213     C                   MOVEL     CMD(7)        COMMAN
082100041021     C*
082200961213     C                   CALL      'QCMDEXC'                            90
082300961213     C                   PARM                    COMMAN           80
082400961213     C                   PARM                    LUNG             15 5
082500041021     C*
082600970114     C                   OPEN      LISDIC
082700970110   1AC                   END
082800971219     C*
082900971219     C*  Accendo indicatori di eccedenza
083000971219     C                   SETON                                        OFOGOD
083100971219     C                   SETON                                        OCOEOB
083200961121     C*
083300960529     C                   ENDSR
083400020225      *****************************************************************
083500041020      * Imposta le code x invio FAX
083600020225      *****************************************************************
083700041021     C     Code_FAX      BEGSR
083800020225      *
083900041020     ** Reperisco la coda delle stampe FAXIN .
084000041020     C                   CALL      'TRULOUTQ'
084100041020     ** Parametri di input.
084200041020     C                   PARM                    PrmFine
084300041020     C                   PARM      SimPOU        PrmFilS
084400041020     C                   PARM      FaxIn         PrmTpOutQ
084500041020     ** Parametri di output.
084600041020     C     MyOutQ        PARM      *BLANK        PrmOutQ
084700041020     C     MyOutQLib     PARM      *BLANK        PrmOutQLib
084800041020     C                   PARM      *BLANK        PrmEsito
084900041020     ** Se il reperimento della coda non è riuscito, creo lo spool in QDKT.
085000041020     C                   IF        PrmEsito = Errore
085100041020     C                   EVAL      MyOutQ = 'QDKT'
085200041020     C                   EVAL      MyOutQLib = '*LIBL'
085300041020     C                   ENDIF
085400041020      *  Se non ha trovato imposta la coda generica
085500041020     C                   IF        MyOutQ = *BLANK
085600041020     C                   EVAL      MyOutQ = 'QDKT'
085700041020     C                   EVAL      MyOutQLib = '*LIBL'
085800041020     C                   ENDIF
085900041020      *
086000041020     C                   EVAL      FaxInOutQ = MyOutQ
086100041020     C                   EVAL      FaxInLibQ = MyOutQLib
086200041020     C**
086300041020     C*---------------------------------------------------------
086400041020     ** Reperisco la coda delle stampe FAXOUT.
086500041020     C                   CALL      'TRULOUTQ'
086600041020     ** Parametri di input.
086700041020     C                   PARM                    PrmFine
086800041020     C                   PARM      SimPOU        PrmFilS
086900041020     C                   PARM      FaxOut        PrmTpOutQ
087000041020     ** Parametri di output.
087100041020     C     MyOutQ        PARM      *BLANK        PrmOutQ
087200041020     C     MyOutQLib     PARM      *BLANK        PrmOutQLib
087300041020     C                   PARM      *BLANK        PrmEsito
087400041020     ** Se il reperimento della coda non è riuscito, creo lo spool in QDKT.
087500041020     C                   IF        PrmEsito = Errore
087600041020     C                   EVAL      MyOutQ = 'QDKT'
087700041020     C                   EVAL      MyOutQLib = '*LIBL'
087800041020     C                   ENDIF
087900041020      *  Se non ha trovato imposta la coda generica
088000041020     C                   IF        MyOutQ = *BLANK
088100041020     C                   EVAL      MyOutQ = 'QDKT'
088200041020     C                   EVAL      MyOutQLib = '*LIBL'
088300041020     C                   ENDIF
088400041020      *
088500041020     C                   EVAL      FaxOutOutQ = MyOutQ
088600041020     C                   EVAL      FaxOutLibQ = MyOutQLib
088700041020      *
088800041020     C                   ENDSR
088900041020      *****************************************************************
089000041020      * LETTURA DELLE SPEDIZIONI
089100041020      *****************************************************************
089200041020     C     REDSPE        BEGSR
089300041020      *
089400020225      * LEGGO V.L. SU FNARB PER LNA E DATA TRASMISSIONE = zero
089500020225      * (I RECORD GIA' TRASMESSI NON DEVONO ESSERE RIELABORATI)
089600020225     C                   clear                   KDT3
089700961113     C     KARB55        SETLL     FNARB55L
089800020225     C     KARB55        READE     FNARB55L
089900020225      *
090000020225    1C                   DOW       not %EOF(FNARB55L)
090100020225      *
090200020225      * Escludo locali e spedizioni consegnate
090300020225    2c                   IF        ARBlnp <> ARBlna  and  ARBdcm = *zeros
090400020225      *
090500020225      * Leggo ds3a - tabella codici bolla se varia arbcbo
090600020225    3C     ARBCBO        IFNE      SAVCBO
090700961112     C                   MOVEL     '3A'          KCOD
090800961112     C                   MOVEL(P)  ARBCBO        KKEY
090900961112     C                   EXSR      CTABEL
091000961114     C  N31              MOVEL     TBLUNI        DS3A
091100961114     C   31              CLEAR                   DS3A
091200961112     C                   MOVE      ARBCBO        SAVCBO
091300020225    3C                   ENDIF
091400020225      * Escludo bolle di recupero
091500020225    3c                   IF        §3Arbl <> 'R'
091600020225      *
091700020225      * SFLAGGO BOLLA IN OGNI CASO (POTREBBE ESSERCI 'T' IN SEGUITO
091800020225      * A PRECEDENTE ELABORAZIONE PROVVISORIA MA TALE 'T' POTREBBE NON
091900020225      * ESSERE PIU' VERA)
092000020225     C                   clear                   ARBFT3
092100060918     C                   CLEAR                   CreaE8            1
092200060918     C                   CLEAR                   CreaE7x           1
092300020225      * LEGGO DETTAGLIO COLLI
092400060918     c* Primo giro: per no invio spedizioni parziali
092500060918     c*             non creo le discordanze
092600060918     c                   if        d82isc='S'
092700060918     c                   eval      wcreadis=' '
092800060918     c                   else
092900060918     c                   eval      wcreadis='S'
093000060918     c                   endif
093100060918     c
093200961112     C                   EXSR      REDART
093300060918     c
093400060918     c* Se devo creare discordanze e stampare faccio un secondo giro
093500060918     c*  solo per INVII no parziali
093600060918     c                   if        d82isc='S' and
093700060918     c                             (creae8='S' or creae7x='S')
093800060918     c                   eval      wcreadis='S'
093900060918     C                   EXSR      REDART
094000060918     c                   endif
094100060918     c*
094200020225      **
094300020225      * LA SPEDIZIONE FA PARTE DEL MANIFEST
094400060913      * PER FEDEX       INCLUDO SOLO QUELLE CON ABILITAZIONE MERCE ARR > 1
094500060913      * Se invio TOTALE includo solo quelle con abilitazione merce arr > 1
094600060915      *  e se non in IMA
094700020225      **------------------------------------------------------
094800120821    4C                   IF        (ARBFT3 ='T' and d82ftt <> FEDEX and
094900060913     c                              d82isc=' ') or
095000120821    4C                             (d82ftt = FEDEX and ARBAMA > 1)  or
095100060915    4C                             (d82isc ='S' and ARBAMA > 1 and
095200060918     c                              arbft3='T' and not *in01)
095300060913      *
095400020225      * CODICE BOLLA
095500020225      *  EuroExpress e FedEX
095600020225    6C                   select
095700020225     c                   when      §3ATB1 = 'FS'
095800970219     C                   MOVEL     'EXW'         STATBO
095900020225     c                   when      §3ATB1 = 'AS'
096000970219     C                   MOVEL     'FHC'         STATBO
096100020226     c                   when      %SUBST(§3ATB1:1:1) = 'A'
096200020225     c                             or ARBFBR = 'F'
096300970219     C                   MOVEL     'EXW'         STATBO
096400020225     C                   other
096500020225     C                   MOVEL     'FHC'         STATBO
096600020225    6C                   endsl
096700030317      * Per Fedex
096800120821     c                   if        d82ftt = FEDEX
096900030318      *
097000030317      * calcola il Volume/Peso da CML o da Bollettato
097100030317     c                   exsr      VOL_PES
097200030317      *
097300030317     C                   Z-ADD     wpes          PESPRT
097400030317      *
097500030317     c                   else
097600020225      *
097700020225      * Determino peso tassabile per le spedizioni in assegnato
097800020225      * (E'il maggiore fra arbpkf e il risultato della moltiplicazione)
097900020225    5C     STATBO        IFEQ      'EXW'
098000970219     C     §7AKPM        MULT(H)   ARBVLF        PESPRT            7 1
098100020225    6C                   if        ARBPKF > PESPRT
098200970219     C                   Z-ADD     ARBPKF        PESPRT
098300020225    6C                   endif
098400020225   x5C                   ELSE
098500020225      * Per le spedizioni in franco invece,stampo il peso da fatturare
098600970219     C                   Z-ADD     ARBPKF        PESPRT
098700020225    5C                   END
098800030317     c                   endIF
098900030317      *
099000020225      * Totale spedizione/colli/peso e volume
099100020225      * N.B.:I totali mi servono anche se elaborazione provvisoria, no
099200020225      *      dogana in quento dovrò stampare i totali del foglio viaggi
099300020225    5C     D82STB        IFEQ      'SI'
099400970219     C     D82DGN        OREQ      'SI'
099500970219     C     *IN20         OREQ      *OFF
099600970219     C                   ADD       1             TOTSPE            5 0
099700970219     C                   ADD       ARBNCL        TOTNCL            7 0
099800970219     C                   ADD       PESPRT        TOTPES           10 1
099900120821     c                   if        d82ftt = FEDEX
100000030317     C                   add       wvol          TOTVLF
100100030317     c                   else
100200970219     C                   ADD       ARBVLF        TOTVLF            7 3
100300030317     c                   end
100400020225    5C                   ENDIF
100500020225      *
100600020225      * STAMPO MANIFEST SOLO SE RICHIESTO:
100700020225      * - Manifest per partner
100800020225    5C     D82STB        IFEQ      'SI'
100900020225      * - Manifest per dogana
101000970219     C     D82DGN        OREQ      'SI'
101100120821     c     d82ftt        andne     FEDEX
101200020225      * - Manifest spedizioni C.O.D. e questa è spedizione C.O.D.
101300970219     C     D82COD        OREQ      'SI'
101400961112     C     §3AFCA        ANDGT     0
101500120821     c     d82ftt        andne     FEDEX
101600961121     C                   EXSR      PRTMAN
101700020225    5C                   ENDIF
101800020225      *
101900020225      * FLEGGO FNARB E SCRIVO RCD PER TRASMISSIONE
102000020226    5C                   SELECT
102100041021      **
102200020225      * "Provvisorio": aggiorno ARBFT3 per l'elaborazione successiva
102300020225     c                   when      *in20 = *off
102400020225     C                   EXCEPT    AGGFT3
102500041021      **
102600020225      * "Definitivo" e "No trasmissione": fleggo la bolla come trasmessa
102700050104      *  o se invio solo via E-Mail
102800120821     c                   when      D82FTT = SOLOSTAMPA
102900120821     c                              or
103000120821     c                             D82FTT = PRINTNOEDI
103100020225     C                   MOVEL     'T'           ARBFT3
103200020225     C                   Z-ADD     D82DFV        ARBDT3
103300090402     c* data manifest in filiale
103400090402     C                   EXCEPT    AGGTRA
103500090402     c* Data manifest in sede
103600090402     c                   Exsr      AggDataM
103700120823      **
103800120823      * GEL   (Scrittura del file NET EXPRESS EUROPE)
103900120823      * "Definitivo" e "Trasmissione": scrivo record per trasmissione,
104000120823      *   il pgm che trasmette flegga la bolla
104100120823     c                   WHEN      D82FTT = NETEXP
104200120823     C                   EXCEPT    LIBER
104300120823     C                   Z-ADD     ARBAAS        F83AAS
104400120823     C                   Z-ADD     ARBLNP        F83LNP
104500120823     C                   Z-ADD     ARBNRS        F83NRS
104600120823     C                   Z-ADD     ARBNSP        F83NSP
104700120823     c                   z-add     d82dfv        f83dfv
104800120823     C                   MOVEL     TRTC83DS1     KPJBU
104900120905     C                   CALL      'TRTC83G2R'
105000120823     C                   PARM                    KPJBA
105100120823      * REIMPOSTO KPJBU "ORIGINALE"
105200120823     C                   MOVEL     WKPJBU        KPJBU
105300120823     c* Data manifest in sede
105400120823     c                   Exsr      AggDataM
105500041021      **
105600020225      * "Definitivo" e "Trasmissione FED": scrivo record per trasmissione,
105700020225      *   il pgm che trasmette flegga la bolla
105800120821     c                   when      D82FTT = FEDEX
105900020225     C                   EXCEPT    LIBER
106000020228     C                   Z-ADD     ARBAAS        F83AAS
106100020228     C                   Z-ADD     ARBLNP        F83LNP
106200020228     C                   Z-ADD     ARBNRS        F83NRS
106300020228     C                   Z-ADD     ARBNSP        F83NSP
106400020228     c                   z-add     d82dfv        f83dfv
106500020228     C                   MOVEL     TRTC83DS1     KPJBU
106600020225     C                   CALL      'TRTC83R21'
106700020225     C                   PARM                    KPJBA
106800020304      * REIMPOSTO KPJBU "ORIGINALE"
106900020304     C                   MOVEL     WKPJBU        KPJBU
107000090402     c* Data manifest in sede
107100090402     c                   Exsr      AggDataM
107200041021      **
107300020225      * "Definitivo" e "Trasmissione EEX": scrivo record per trasmissione,
107400020225      *   il pgm che trasmette flegga la bolla
107500020226     c                   other
107600060109      *
107700060109      * ?dal 9/01/2006 -  DPD esegue il Manifest Definitivo + invio SCAN
107800060109      * ?....invio SCAN DPD oltre EDI se tipo trasm.--> ("S")
107900060109      * ?    al posto del vecchio FIEU25R dal giro degli SCAN DPD schedulato
108000060109     c                   if        wOGGI >= 20060109
108100120821     c                   if        D82FTT = SCANDPDEDI or D82FTT = SCANDPD
108200060109
108300140311      * vecchio invio SCAN a DPD (no GEOPOST file ma vecchi SCAN)
108400140311
108500140311     c                   if        DPD_OLD_NEW = 'V' or
108600140311     c                             DPD_OLD_NEW = 'E'
108700140311      **
108800060109     C                   Z-ADD     ARBAAS        F83AAS
108900060109     C                   Z-ADD     ARBLNP        F83LNP
109000060109     C                   Z-ADD     ARBNRS        F83NRS
109100060109     C                   Z-ADD     ARBNSP        F83NSP
109200060109     c                   z-add     d82dfv        f83dfv
109300060109     C                   MOVEL     TRTC83DS1     KPJBU
109400060619     C                   CALL      'TRTC83R12'
109500060109     C                   PARM                    KPJBA
109600060109      * REIMPOSTO KPJBU "ORIGINALE"
109700060109     C                   MOVEL     WKPJBU        KPJBU
109800060109      **
109900140311     c                   end
110000140311      **
110100140311      * Invio file GEOPOST Nuovo tracciato
110200140311
110300140311     c                   if        DPD_OLD_NEW = 'N' or
110400140311     c                             DPD_OLD_NEW = 'E'
110500140311      **
110600140311      ** la prima volta deve fare la testata e in seguito tutti i dettagli
110700140311      **  il GOTO è fatto solo quindi per il primo record poichè il flagHDE è
110800140311      **  inizializzato a 'H' Head TESTATA
110900140311      **
111000140311     c     Ri_esegui     tag
111100140311     C                   Z-ADD     ARBAAS        F83AAS
111200140311     C                   Z-ADD     ARBLNP        F83LNP
111300140311     C                   Z-ADD     ARBNRS        F83NRS
111400140311     C                   Z-ADD     ARBNSP        F83NSP
111500140311     c                   z-add     d82dfv        f83dfv
111600140311      *
111700140311     C                   MOVEL     TRTC83DS1     KPJBU
111800140311     C                   call      'TRTC83R13'
111900140311     C                   PARM                    KPJBA
112000140311     C                   PARM                    flagHDE           1
112100140311      *
112200140311      * Solo dopo la prima volta x poter scrivere la testata
112300140311     C                   if            flagHDE ='H'
112400140311     C                   movel     'D'           flagHDE
112500140311     c                   goto      ri_esegui
112600140311     c                   end
112700140311      *
112800140311      * REIMPOSTO KPJBU "ORIGINALE"
112900140311     C                   MOVEL     WKPJBU        KPJBU
113000140311      **
113100140311     c                   end
113200140311      **
113300120821     c                   if        D82FTT  = SCANDPD
113400060109     C                   MOVEL     'T'           ARBFT3
113500060109     C                   Z-ADD     D82DFV        ARBDT3
113600090402     c* Data manifest in filiale
113700090402     C                   EXCEPT    AGGTRA
113800090402     c* Data manifest in sede
113900090402     C                   Exsr      AggDataM
114000060109     c                   end
114100060109      *
114200060109     c                   end
114300060109     c                   end
114400051221
114500051223      * ?Non deve eseguire l'EDI x le Trasmissioni DPD non a SEUR
114600120821     c                   if        D82FTT <> SCANDPD
114700051223
114800961112     C                   EXCEPT    LIBER
114900961112     C                   Z-ADD     ARBAAS        D82AAS
115000961112     C                   Z-ADD     ARBLNP        D82LNP
115100961112     C                   Z-ADD     ARBNRS        D82NRS
115200961112     C                   Z-ADD     ARBNSP        D82NSP
115300020225     C                   MOVEL     TRTC82DS      KPJBU
115400961112     C                   CALL      'TRTC83R2'
115500961112     C                   PARM                    KPJBA
115600090402     c* Data manifest in sede
115700090402     C                   Exsr      AggDataM
115800051223
115900051223     c                   end
116000020225      *
116100020225    5C                   ENDSL
116200020225      **
116300020225      * SE RECORD NON DA INVIARE AGGIORNO ARBFT3 PER "PULIRLO"
116400020225   X4C                   ELSE
116500041021      **
116600020304     c* se sto elaborando manifest per fedex arbft3 potrebbe essere = 'T'
116700020304     c* (spedizione parziale) ma se entro qua lo devo clearare
116800020304     C                   clear                   ARBFT3
116900970319     C                   EXCEPT    AGGFT3
117000020225    4C                   ENDIF
117100020225      *
117200020225    3c                   ENDIF
117300020225    2c                   ENDIF
117400020225     C     KARB55        READE     FNARB55L
117500961120    1C                   ENDDO
117600961112     C*
117700140311      *
117800140311      *  Solo per DPD nuovo tracciato GEOPOST, se è passato dal Dettaglio
117900140311      *  generando il file, il flag è impostato a "D" altrimenti sarebbe ancora
118000140311      *  impostato a "T" come da inizializzazione.
118100140311      *   A questo punto deve chiudere il file che ha aperto e scritto in Dettaglio
118200140311     C                   if            flagHDE ='D'
118300140311      **
118400140311      ** il flag HDE ha funzionato come sintesi del concetto che il programma
118500140311      **  è stato richiamato per fare un Manifest x DPD ed inoltre in modalità
118600140311      **  GEOPOST (altrimenti non varrebbe "D" in nessun altro caso)
118700140311     C                   movel     'E'           flagHDE
118800140311     C                   clear                   KPJBU
118900140311     C                   call      'TRTC83R13'
119000140311     C                   PARM                    KPJBA
119100140311     C                   PARM                    flagHDE
119200140311      **
119300140513      * REIMPOSTO KPJBU "ORIGINALE"
119400140513     C                   MOVEL     WKPJBU        KPJBU
119500140513      *
119600140311     c                   end
119700140311      *
119800961112     C                   ENDSR
119900090402     C*****************************************************************
120000090402     C*    Aggiorna data manifest in filiale e sede                   *
120100090402     C*****************************************************************
120200090402     c     AggDataM      BEGSR
120300090402     c                   setoff                                       818283
120400090402     c                   setoff                                       8485
120500090402     c                   clear                   dta4k
120600090402     c*
120700090402     c
120800090402     c* Anche in sede
120900090402     c                   movel     'K'           ktrc
121000090402     c     kar4          chain     tita430c
121100090402    1c                   if        %found(tita430c)
121200090402     c                   movel     ta4not        dta4k
121300090402   x1c                   else
121400090402     c
121500120823     c     karB          chain     titas30c
121600090402     c                   clear                   ta4not
121700090402    1c                   endif
121800090402     c
121900090403     c                   movel     d82dfv        §ta4kdmf
122000090402     c                   movel     dta4k         ta4not
122100090402    1c                   if        %found(tita430c)
122200090402     c   84              update    tita4000
122300090402     c   85              update    tita4p00
122400090402   x1c                   else
122500090403    2c                   if        %found(titas30c)
122600090402     c                   movel     arbaas        ta4aas
122700090402     c                   movel     arblnp        ta4lnp
122800090402     c                   movel     arbnrs        ta4nrs
122900090402     c                   movel     arbnsp        ta4nsp
123000090402     c                   movel     'K'           ta4trc
123100090402     c  n83              write     tita4000
123200090402     c   83              write     tita4p00
123300090402    2c                   endif
123400090402    1c                   endif
123500090402     c
123600090402     C                   ENDSR
123700961114     C*****************************************************************
123800961114     C*    LETTURA DETTAGLIO COLLI                                    *
123900961114     C*****************************************************************
124000961114     C     REDART        BEGSR
124100020225      *
124200961115     C                   CLEAR                   WDTPAR
124300961115     C                   CLEAR                   WFIL
124400961120     C                   CLEAR                   WNPS
124500020225     C                   clear                   WDIS4
124600020225     C                   clear                   XX
124700020225     C                   clear                   YY
124800961218     C                   CLEAR                   NSC
124900020225     c                   eval      *in02 = *off
125000060918     c                   eval      *in01 = *off
125100020225     c                   eval      *in12 = *on
125200020225      *
125300120823     C     KARb          SETLL     FNART01L
125400120823     C     KARb          READE     FNART01L                               31
125500961114    1C     *IN31         DOWEQ     *OFF
125600961121     C*
125700961121     C                   MOVE      ARTFLS        KFLS
125800961121     C                   MOVE      ARTLNA        KLNA
125900961121     C                   MOVE      ARTNRS        KNRS
126000961121     C                   MOVE      ARTNSC        KNSC
126100970116     C*
126200970116    2C     ARTDTR        IFGT      *ZEROS
126300970116     C                   Z-ADD     645           KCAA
126400970116     C                   EXSR      CTRANM
126500970116    3C     *IN09         IFEQ      *OFF
126600060913     c
126700970116     C* MEMORIZZO CHE LA SPEDIZIONE E' DA STAMPARE NEL MANIFEST
126800970116     C                   MOVE      'T'           ARBFT3
126900060913     c
127000970116     C* CONTROLLI PER DISCORDANZA E4-INVIO RECORD:COLLO GIA' INVIATO
127100970116    4C     *IN20         IFEQ      *ON
127200970116     C     D82LDS        OREQ      'SI'
127300970116     C* Controllo che collo non sia nuovamente in partenza (anom.620)
127400970116     C                   Z-ADD     620           KCAA
127500990519     C     KANM2         SETLL     FNANM02L                               31
127600970116    5C     *IN31         IFEQ      *OFF
127700970116    6C     WDIS4         IFEQ      *BLANKS
127800060913     c
127900060918     c* Per INVII anche parziali
128000060918    7c                   if        d82isc=' '
128100060913     c                   eval      kcev='E4'
128200060913     C                   EXSR      GEDIS48
128300060918     c
128400060918     C                   MOVE      '1'           WDIS4
128500060918    7c                   endif
128600060918     c*
128700060918     c* Per INVII solo totali o forzati
128800060918    7c                   if        d82isc='S'
128900060918     c
129000060918     c* 1° giro: Memorizzo che devo creare l'anomalia
129100060918    8c                   if        wcreadis=' '
129200060918     c
129300060918     c*  Se bolla abilitata la differenza la fa l'IMA
129400060918    9c                   if        arbama>1
129500060918     c
129600060918     c* Se non c'e' ancora nulla devo creare la E8
129700060918     c                   if        creae8=' '
129800060918     c                   eval      creae8='S'
129900060918     c                   endif
130000060918     c* Se non devo creare la S8 allora memorizzo il persiste E7A
130100060918     c                   if        creae8='N'
130200060918     c                   eval      creae7x='S'
130300060918     c                   endif
130400060918   x9c                   else
130500060918     c
130600060918     c* scrivo il persiste manca invio record
130700060918     c                   eval      creae7x='S'
130800060918    9c                   endif
130900060918     c*
131000060918   x8c                   else
131100060918     c* 2° giro: stampo un base alle momrizzazioni per giro precedente
131200060918    9c                   if        creaE8='S'
131300060918     c                   eval      kcev='E8'
131400060918     C                   EXSR      GEDIS48
131500060918     C                   MOVE      '1'           WDIS4
131600060918    9c                   endif
131700060918     c
131800060915     c* scrivo il persiste manca invio record
131900060918    9c                   if        creaE7x='S'
132000060915     c                   exsr      CTRE7X
132100060918    9c                   endif
132200060918    8c                   endif
132300060918    7c                   endif
132400060913     c
132500970116    6C                   END
132600970116     C*
132700970116     C* STAMPA RIGA SEGNACOLLI GIA' INVIATI
132800060918     c*  per la E4 o la E8 al 2° giro se creata
132900970116    6C     D82LDS        IFEQ      'SI'
133000060918     c
133100060918   6ac                   if        d82isc=' ' or
133200060918     c                             (D82isc='S' and wcreadis='S' and
133300060918     c                              creae8='S')
133400060915     c
133500020225    7C     XX            IFGE      13
133600060918     C                   MOVE      ','           NSC(XX)
133700060918     c                   exsr      Stagiainv
133800060918     c
133900970116     C                   SETOFF                                       12
134000020225     C                   MOVE      *ZEROS        XX
134100970116     C                   CLEAR                   NSC
134200060918    7C                   ENDif
134300970116     C* Memorizzo in schiera di stampa il numero segnacollo
134400020225     C                   ADD       1             XX
134500970116     C* Richiamo routine per abblencare zeri non significativi
134600970116     C                   MOVE      ARTNSC        W007A             7
134700970116     C                   MOVEA     W007A         K08
134800970116     C                   EXSR      ABBL
134900020225     C                   MOVEA     K08           NSC(XX)
135000970116     C* Metto "," nell'elemento di schiera precedente
135100020225    7C     XX            IFGT      1
135200020225     C     XX            SUB       1             YY
135300020225     C                   MOVE      ','           NSC(YY)
135400970116    7C                   END
135500060915   6aC                   END
135600060918     c
135700060915    6C                   END
135800970116    5C                   END
135900970116    4C                   END
136000970116    3C                   END
136100020225      ***
136200970116   X2C                   ELSE
136300990412      * SE HO  COLLO ARRIVATO MA NON SPARATO FORZO L'ARRIVO   Solo se no bis
136400990412     C                   CLEAR                   WARFOR
136500001127     C**********
136600001127     C***  ASTERISCATO PERCHÈ NON SI VUOLE PIÙ LA GENERAZIONE DEL MANIFEST
136700001127     C***  CON PARTENZE SPARATE SOLO DA FILIALI DI PARTENZA.
136800001127     C***  QUESTO ERA STATO ATTIVATO PRECEDETEMENTE IL 12/04/99
136900001127     C***  ORA SI È TORNATI INDIETRO.(VEDERE NOTE DI ND)
137000001127     C**********
137100001127    3C********** ARTDAM    IFEQ *ZEROS
137200001127     C********** *IN22     ANDEQ*OFF
137300001127     C**********           EXSR ARFORZ
137400001127    3C**********           ENDIF
137500001127     C***
137600990412     C* COLLO NON INVIATO MA ARRIVATO O CON ARRIVO FORZATO
137700970116    3C     ARTDAM        IFGT      *ZEROS
137800970116     C     ARTDAM        ANDLE     D82DFV
137900990412     C     WARFOR        OREQ      'S'
138000990412      *
138100970117    4C     *IN22         IFEQ      *OFF
138200970116     C* NO BIS: VERIFICO SE COLLO A TERRA
138300970116     C                   Z-ADD     645           KCAA
138400970116     C                   EXSR      CTRANM
138500970116   X4C                   ELSE
138600970116     C* SI BIS: VERIFICO SE IN PARTENZA
138700970117     C                   Z-ADD     1             KNPG
138800070123     C     KBRV1         SETLL     FNBRV01L                               31
138900020225     c   31              eval      *in09 = *off
139000020225     c  N31              eval      *in09 = *on
139100970116    4C                   END
139200970116     C* 09 OFF --> LA SPEDIZIONE E' DA STAMPARE NEL MANIFEST
139300970116    4C     *IN09         IFEQ      *OFF
139400060913     c
139500970116     C                   MOVE      'T'           ARBFT3
139600060913     c
139700970116   X4C                   ELSE
139800060918     c
139900060918     C* SE IL COLLO NON PARTE in IMA, ACCENDO INDICATORE DI PARTIAL
140000060918     c* In questo caso non devo creare l'anomalia E8 ma un persiste
140100060918     c                   if        creae8='S'
140200060918     c                   eval      creae7x='S'
140300060918     c                   endif
140400060918     c*
140500060918     c                   eval      creae8='N'
140600060918     c
140700020225     C                   eval        *in02 = *on
140800060918     C                   eval        *in01 = *on
140900970116    4C                   END
141000970116   X3C                   ELSE
141100970116     C* SE IL COLLO NON E' ARRIVATO ACCENDO INDICATORE DI PARTIAL
141200020225     C                   eval        *in02 = *on
141300970116    3C                   END
141400970116    2C                   END
141500020225      *
141600020225      * SE NON DEVO NE' STAMPARE NE' CREARE LE DISCORDANZE E AI FINI
141700020225      * DEL MANIFEST HO GIA' RILEVATO QUELLO CHE MI SERVIVA (FT3='T'
141800020225      * E 02 ON) ESCO DAL CICLO
141900970110     C     *IN20         IFEQ      *OFF
142000970110     C     D82LDS        ANDEQ     'NO'
142100970110     C     *IN02         ANDEQ     *ON
142200970110     C     ARBFT3        ANDEQ     'T'
142300970110     C                   SETON                                        31
142400970110     C                   ELSE
142500120823     C     KARb          READE     FNART01L                               31
142600970110     C                   END
142700020225      *
142800961119    1C                   ENDDO
142900961129     C*
143000961127     C* Stampa elenco segnacolli a fine ciclo
143100060918    1C     NSC(1)        IFNE      *BLANKS
143200060918     c                   exsr      STAGIAINV
143300060918     c                   endif
143400060918     c
143500961218     C* stampo riga vuota per distanziarla
143600961127     C* dalla prossima discordanza che verrà eventualmente stampata
143700060918    1C     WDIS4         IFEQ      '1'
143800970114     C     D82LDS        ANDEQ     'SI'
143900120821    2c     d82ftt        ifne      FEDEX
144000040423
144100060918    3c                   if        FAX_Disc_Est = 'S'
144200970115     C   20              EXCEPT    FIT
144300060918    3c                   end
144400040423
144500060918    2c                   endif
144600970114     C                   EXCEPT    FITC
144700060918    1C                   END
144800961114     C*
144900961114     C                   ENDSR
145000060918     *** *****************************************************************
145100060918     *** * stampa riga segnacolli già inviati
145200060918     *** *****************************************************************
145300060918     c     STAGIAINV     BEGSR
145400060918     C* Stampo riga con elenco segnacolli
145500060918    8C   20*INOB         IFEQ      *ON
145600120821     c     d82ftt        andne     FEDEX
145700060918
145800060918     c                   if        FAX_Disc_Est = 'S'
145900060918     C                   ADD       1             WPAG
146000060918     C                   EXCEPT    TESDIS
146100060918     c                   end
146200060918
146300060918    8C                   END
146400060918     c
146500060918    8C     *INOE         IFEQ      *ON
146600060918     C                   ADD       1             WPAG1
146700060918     C                   EXCEPT    TESDIC
146800060918    8C                   END
146900060918     c
147000120821    8c     d82ftt        ifne      FEDEX
147100060918    9c                   if        FAX_Disc_Est = 'S'
147200060918     C   20              EXCEPT    RIGCNS
147300060918    9c                   end
147400060918    8c                   endif
147500060918     c
147600060918     C                   EXCEPT    RIGCNC
147700060918     c
147800060918     c                   ENDSR
147900961213     C*****************************************************************
148000961213     C* STAMPA MANIFEST ----------------------------------------------*
148100961213     C*****************************************************************
148200961213     C     PRTMAN        BEGSR
148300961213     C*
148400961213     C                   CLEAR                   MITCPI
148500961213     C                   CLEAR                   DESCPI
148600961220     C                   CLEAR                   STATIC
148700061127     C                   CLEAR                   STACNA           35
148800061127     C                   CLEAR                   STACTE           25
148900961213     C*
149000961213     C* ACCORCIO LA LUNGHEZZA DI ALCUNI CAMPI
149100961213     C                   Z-ADD     ARBRMN        STARMN            9 0
149200961213     C                   MOVEL     ARBRSM        STARSM           25
149300130808     C                   MOVEL     ARBINM        STAINM           25
149400961213     C                   MOVEL     ARBRSD        STARSD           25
149500961213     C                   MOVEL     ARBLOM        STALOM           20
149600961213     C                   MOVEL     ARBIND        STAIND           25
149700961213     C                   MOVEL     ARBLOD        STALOD           20
149800961213     C*
149900970110     C* SE DEVO STAMPARE SOLO IL MANIFEST SINTETICO NON OCCORRE FARE
150000970110     C* LE SEGUENTI OPERAZIONI
150100970110     C     *IN20         IFEQ      *ON
150200970110     C     D82DGN        OREQ      'SI'
150300120821     c     d82ftt        andne     FEDEX
150400970110     C     D82COD        OREQ      'SI'
150500970110     C     §3AFCA        ANDGT     0
150600120821     c     d82ftt        andne     FEDEX
150700061127      *
150800061127      * Prendo i dati del Contatto Nome e Telefono
150900061127     c     KAR5          chain     fiar501l                           33
151000061127     c                   if        %Found(fiar501l)
151100061127     c                   Movel     Ar5Uni        DAr5Gen
151200061127     c                   movel     §ar5ref       STACNA
151300061127     c                   movel     §ar5teld      STACTE
151400061127      * se i campi sono vuoti non deve stamparli..... Accende quindi il 33
151500061127      *   come se non avesse trovato il record.
151600061127     c                   if        STACNA=*blank and STACTE=*blank
151700061127     c                   seton                                        33
151800061127     c                   end
151900061127     c                   end
152000130215      ***
152100970110     C* Prendo i dati del c/assegno
152200120823     C     KARb          CHAIN     FIAR901L                           34
152300130215      ***
152400961220     C* Reperisco decodifica tipo incasso contrassegno
152500961220     C     *IN34         IFEQ      *OFF
152600041109     c                   z-add     ar9cas        prtcas            9 3
152700130215      *
152800961220     C                   MOVEL     '1A'          KECOD
152900961220     C                   MOVEL(P)  AR9TIC        KEKEY
153000130215      ***
153100130215     c                   z-add     1             tentativi         3 0
153200130215      * prima ci prova se c'è un suffisso particolare
153300130215      *   quindi preimposta anche la possibilità di cercare 2 volte
153400130215     c                   if        §pzSUFFtab <> *blank
153500130215     C                   MOVE      §pzSUFFtab    KEKEY
153600130215     c                   z-add     2             tentativi
153700130215     c                   end
153800130215      *
153900130215     C                   CLEAR                   edids1a
154000130215      *** può fare max 2 tentativi:
154100130215     c                   do        tentativi
154200130215     C     KETAB         CHAIN     EDTAB01L
154300130215     c                   if        %Found(EDTAB01L)
154400130215     C                   MOVEL     TABUNI        edids1a
154500961220     C                   MOVEL     §1ADES        STATIC
154600130215      * se ha trovato esce dopo aver impostato il campo per la stampa
154700130215     c                   LEAVE
154800130215     c                   else
154900130215      * serve x il secondo giro senza suffisso
155000130215     C                   MOVE      *blank        Due_Blanks        2
155100130215     C                   MOVE      Due_Blanks    KEKEY
155200130215     c                   end
155300130215      ***
155400130215     c                   EndDO
155500130215      ***
155600961220     C                   END
155700970110     C* Prendo le note
155800961213     C                   MOVEL     '8'           KTRC                           *NOTE1
155900060213     C     KAR4          CHAIN     fiar401L                           35
156000961213     C  N35              MOVEL     AR4NOT        STANT1
156100961213     C                   MOVEL     '9'           KTRC                           *NOTE2
156200060213     C     KAR4          CHAIN     fiar401L                           36
156300961213     C  N36              MOVEL     AR4NOT        STANT2
156400961213     C*
156500961213     C                   MOVEL     'P'           KTRC                           *P.IVA
156600060213     C     KAR4          CHAIN     fiar401L                           37
156700961213     C  N37              MOVEL     AR4NOT        MITCPI           16
156800961213     C  N37              MOVE      AR4NOT        DESCPI           16
156900130808
157000130808      * se non c'è partita IVA del mittente cerco il codice fiscale del mittente
157100130808     c                   IF        MITCPI = *blanks
157200130808     c                   eval      ktrc = 'Q'
157300130808     c     kar4          chain     fiar401l                           37
157400130808     c  N37              movel     AR4NOT        MITCPI
157500130808     c                   ENDIF
157600100223     C*
157700100223     C*  ragione sociale aggiuntiva di altri 35 + i primi 10 con il finale della
157800100223     C*  ragione sociale destinatario.
157900100223     C                   clear                   RagAdd45         45
158000100223     c                   eval      RagAdd45 = %subst(arbRSD:26:10)
158100100223      *  in più se c'è anche la parte addizionale
158200120821     C                   MOVEL     'D'           KTRC                           *Ragione Sociale
158300100223     C     KAR4          CHAIN     fiar401L                           23         aggiuntiva
158400100223     C  N23              MOVE      AR4NOT        RagAdd45
158500100223     C*
158600100223     C                   clear                   IndAdd10         10
158700100223     c                   eval      IndAdd10 = %subst(arbIND:26:10)
158800100223     C*
158900100224     C                   clear                   CitAdd15         15
159000100224     c                   eval      CitAdd15 = %subst(arbLOD:21:15)
159100130808      * Altri 10 caratteri per l'indirizzo del mittente
159200130808     C                   clear                   IndMAdd10        10
159300130808     c                   eval      IndMAdd10 = %subst(arbINM:26:10)
159400100223     C*
159500100223     C*  stampa righe aggiuntive
159600100223     c                   setoff                                       242526
159700130808     c                   eval      *in27 = *off
159800100223     c                   if        RagAdd45 <> *blank
159900100223     c                   setoN                                        24
160000100223     c                   end
160100961213     C*
160200100223     c                   if        IndAdd10 <> *blank
160300100223     c                   setoN                                        25
160400100223     c                   end
160500100223     C*
160600100224     c                   if        CitAdd15 <> *blank
160700100223     c                   setoN                                        26
160800100223     c                   end
160900130808      *
161000130808     c                   IF        IndMAdd10 <> *blank
161100130808     c                   eval      *in27 = *on
161200130808     c                   ENDIF
161300100223     C*
161400970110     C* data spedizione
161500961213     C                   Z-ADD     ARBDSP        DATA
161600961213     C                   Z-ADD     MM            GGMM
161700961213     C                   MOVEL     GG            GGMM
161800961213     C                   Z-ADD     AA            STADSP
161900961213     C                   MOVEL     GGMM          STADSP
162000970110     C* numero spedizione compattato e non
162100961213     C                   MOVE      ARBLNP        §LB2              2 0
162200961213     C                   MOVE      ARBNSP        §LB8              8 0
162300961213     C                   MOVEL     §LB2          §LB8
162400961213     C                   MOVEL     ARBNRS        §LB9
162500961213     C                   MOVE      ARBNSP        §LB9              9 0
162600961213     C                   MOVEL     ARBLNP        §LB12            12 0
162700961213     C                   MOVE      §LB9          §LB12            12 0
162800100330     C*
162900100330     C                   MOVE      ARBAAS        §LB2
163000100330     C                   MOVEL     §LB2          §LB5              5
163100100330     C                   MOVE      ARBLNP        §LB5
163200100330     C                   MOVE      §LB9          §LB14            14 0
163300100330     C                   MOVEL     §LB5          §LB14
163400970110     C*
163500970110     C                   END
163600961213     C*
163700961213     C* STAMPA MANIFEST PER PARTNER
163800970110     C   20
163900970110     CAN OF
164000970110     CORN20
164100970110     CAN OCD82STB        IFEQ      'SI'
164200961213     C* 04 on - stampa qualcosa nel manifest
164300961213     C                   SETON                                        04
164400041109     c                   if        Email_PTN = *blank or
164500041109     c                             PRT_PTN_tes = *blank
164600041109     c                   eval      PRT_PTN_tes = 'S'
164700970110     C   20              EXCEPT    TESTA
164800041109     c                   end
164900970110     C  N20              EXCEPT    TESSIN
165000970110     C                   SETOFF                                       OFOC
165100970110     C                   ENDIF
165200961213     C*
165300961213     C* STAMPA MANIFEST PER DOGANA
165400961213     C     D82DGN        IFEQ      'SI'
165500120821     c     d82ftt        andne     FEDEX
165600961213     C     *INOD         ANDEQ     *ON
165700961213     C* 06 on - stampa qualcosa nel manifest
165800961213     C                   SETON                                        06
165900041109     c                   if        Email_DGN = *blank or
166000041109     c                             PRT_DGN_tes = *blank
166100041109     c                   eval      PRT_DGN_tes = 'S'
166200961213     C                   EXCEPT    TESDGN
166300041109     c                   end
166400961213     C                   SETOFF                                       OD
166500961213     C                   ENDIF
166600961213     C*
166700961213     C     D82STB        IFEQ      'SI'
166800970110     C   20              EXCEPT    DET
166900970110     C  N20              EXCEPT    DETSIN
167000021125      * Stampa il Valore Merce Dichiarato solo su Manifest Sintetico Provvisorio
167100021204     c   20              if        arbvmd <> 0
167200021204     C                   except    VMDdet
167300021125     c                   end
167400961213     C                   END
167500961213     C*
167600961213     C     D82DGN        IFEQ      'SI'
167700120821     c     d82ftt        andne     FEDEX
167800961213     C                   EXCEPT    DETDGN
167900961213     C                   END
168000961213     C*
168100961213     C* STAMPA MANIFEST DEI C.O.D.
168200961213     C     D82COD        IFEQ      'SI'
168300961213     C     §3AFCA        ANDGT     0
168400120821     c     d82ftt        andne     FEDEX
168500961213     C*
168600961213     C     *INOG         IFEQ      *ON
168700961213     C* 05 On - Stampa qualcosa nel manifest
168800961213     C                   SETON                                        05
168900041109     c                   if        Email_COD = *blank or
169000041109     c                             PRT_COD_tes = *blank
169100041109     c                   eval      PRT_COD_tes = 'S'
169200961213     C                   EXCEPT    TESCOD
169300041109     c                   end
169400961213     C                   SETOFF                                       OG
169500961213     C                   ENDIF
169600961213     C*
169700961213     C                   ADD       1             CODSPE            5 0
169800961213     C                   ADD       ARBNCL        CODNCL            7 0
169900961213     C                   ADD       PESPRT        CODPES           10 1
170000030318     C*
170100030318     C*  questa subroutine non dovrebbe essere mai chiamata per Fedex
170200030318     C*   ma è stato mantenuto il test sul volume per rispettare le logiche
170300030318     C*     applicate alla Fedex.......non si sa mai ......
170400120821     c                   if        d82ftt = FEDEX
170500030317     C                   ADD       wvol          CODVLF
170600030317     c                   else
170700961213     C                   ADD       ARBVLF        CODVLF            7 3
170800030317     c                   end
170900961213     C                   EXCEPT    DETCOD
171000961213     C                   END
171100961213     C*
171200961213     C                   ENDSR
171300961119     C*****************************************************************
171400060913     C* CREAZIONE EVENTO E4 o E8 + STAMPA
171500961119     C*****************************************************************
171600060913     C     GEDIS48       BEGSR
171700961127     C*
171800060913     C******             MOVE      'E4 '         KCEV
171900970117     C****
172000970117     C* STAMPO LA DISCORDANZA
172100970117     C****
172200970117     C     D82LDS        IFEQ      'SI'
172300970117     C                   MOVE      ARBAAS        WAAS
172400970117     C                   MOVE      ARBLNP        WLNP
172500970117     C                   MOVE      ARBNSP        WNSP
172600970117     C                   EXSR      PRTDIS
172700970117     C                   END
172800961120     C*
172900961120     C****
173000961120     C* SE ELABORAZIONE DEFINITIVA CREO EVENTO
173100961120     C****
173200961205    1C     *IN20         IFEQ      *ON
173300961120     C                   CLEAR                   FNEVS1
173400961120     C*
173500961120     C                   MOVE      ARBAAS        EVSAAS
173600961120     C                   MOVE      ARBLNP        EVSLNP
173700961120     C                   MOVE      ARBNRS        EVSNRS
173800961120     C                   MOVE      ARBNSP        EVSNSP
173900961120     C                   MOVE      ARBLNA        EVSLNA
174000961120     C*
174100961120     C                   CLEAR                   EVSFLS
174200961120     C                   CLEAR                   EVSNSC
174300961120     C*
174400961212     C                   MOVE      D82DFV        EVSDEV
174500961120     C                   MOVE      KCEV          EVSCEV
174600961120     C*
174700961120     C                   WRITE     FNEVS1
174800961120    1C                   END
174900961119     C*
175000961119     C                   ENDSR
175100961113     C*****************************************************************
175200961113     C* VERIFICA DISCORDANZE
175300961113     C*****************************************************************
175400961113     C     CTRDIS        BEGSR
175500961115     C*
175600961115     C                   CLEAR                   WDTPAR
175700961115     C                   CLEAR                   WFIL
175800961121     C*----------------------------------------------------------------
175900961120     C* CONTROLLO DISCORDANZE 2/2A - COLLO INOLTRATO AL PARTNER SENZA
176000961120     C*                              RECORD BOLLA
176100961121     C*----------------------------------------------------------------
176200020301     c* Per Fedex il giro sui manca record non deve essere fatto
176300120821     c     D82FTT        ifne      FEDEX
176400020301     c*
176500961115     C                   SETOFF                                       43
176600961121     C* LEGGO ANOMALIE 10-MANCA RECORD BOLLA ARR., APERTE, PER LNA
176700961113     C                   Z-ADD     10            KCAA
176800990519     C     KANM10        SETLL     FNANM10L
176900990519     C     KANM10        READE(N)  FNANM10L                               31
177000961114    1C     *IN31         DOWEQ     *OFF
177100970121     C* CONSIDERO SOLO LE APERTE OPPURE LE CHIUSE CON CCH = 'PR'
177200970121    2C     ANMDAO        IFLE      D82DFV
177300970121     C     ANMDCH        ANDEQ     *ZEROS
177400970121     C     ANMDAO        ORLE      D82DFV
177500970121     C     ANMDCH        ANDGT     *ZEROS
177600970121     C     ANMCCH        ANDEQ     'PR'
177700961203     C* ESCLUDO LOCALI
177800970117    3C     ANMLNP        IFGT      *ZEROS
177900961203     C     ANMLNP        CABEQ     ANMLNA        REDA10
178000970117   X3C                   ELSE
178100961203     C     ANMFLS        CABEQ     ANMLNA        REDA10
178200970117    3C                   END
178300970117     C* VERIFICO SE COLLO E' IN PARTENZA
178400970117     C* Se fv no bis verifico se collo a terra
178500961120     C                   MOVE      ANMFLS        KFLS
178600961120     C                   MOVE      ANMLNA        KLNA
178700961120     C                   MOVE      ANMNRS        KNRS
178800961120     C                   MOVE      ANMSCN        KNSC
178900970117    3C     *IN22         IFEQ      *OFF
179000961122     C                   Z-ADD     645           KCAA
179100961122     C                   EXSR      CTRANM
179200970117   X3C                   ELSE
179300970117     C* Se fv bis verifico se sparato in defluenza su questo fv
179400970117     C                   EXSR      CHKBRV
179500970117    3C                   END
179600970117     C* 09 OFF --> COLLO IN PARTENZA
179700961120    3C     *IN09         IFEQ      *OFF
179800961120     C* VERIFICO ESISTENZA DISCORDANZA 2-COLLO INOLTRATO NEL GIORNO
179900961120     C*                                  SENZA BOLLA
180000961113     C                   MOVE      'E2 '         KCEV
180100961121     C     KEVS2         SETGT     FNEVS02L
180200961127     C     KEVS2         READPE    FNEVS02L                               31
180300970117    4C     *IN31         IFEQ      *ON
180400961121     C* CREO E STAMPO DISCORDANZA 2
180500060913     C                   EXSR      GEDIS27
180600970117   X4C                   ELSE
180700970117   4AC     EVSDEV        IFLT      D82DFV
180800970117     C     *IN22         ANDEQ     *OFF
180900961121     C* ESISTE EVENTO 2 CREATO IN UN GIORNO PRECEDENTE --> MEMORIZZO LA
181000961129     C* DATA E LA PISTOLA
181100961120     C                   MOVE      EVSDEV        WDEV
181200040415     C                   MOVE      EVSDEV        WDEV2a
181300961129     C                   MOVE      EVSNPS        WNPS
181400961121     C* CERCO ANCHE SE ESISTE EVENTO 2A
181500961120     C                   MOVE      'E2A'         KCEV
181600970120     C* SE TROVO E2A IN DATA DEL FOGLIO NON FACCIO NIENTE
181700120823     C     KEVS2A        SETLL     FNEVS02L                               31
181800970120     C     *IN31         CABEQ     *ON           REDA10
181900961120     C     KEVS2         SETGT     FNEVS02L
182000961127     C     KEVS2         READPE    FNEVS02L                               31
182100961121    5C     *IN31         IFEQ      *OFF
182200970117     C* TENGO LA DATA EVENTO MAGGIORE FRA EVENTO E2 ED EVENTO E2A
182300970117    6C     EVSDEV        IFGT      WDEV
182400961120     C                   MOVE      EVSDEV        WDEV
182500970117    6C                   END
182600970117    5C                   END
182700961121     C* CERCO SE ESISTE EVENTO 6-COLLO DATO PER INOLTRATO MA A TERRA
182800961121     C* IN DATA > WDEV
182900961121     C                   MOVE      'E6 '         KCEV
183000961121     C     KEVS2B        SETGT     FNEVS02L
183100961127     C     KEVS2         READE     FNEVS02L                               31
183200961121    5C     *IN31         IFEQ      *OFF
183300970120     C     EVSDEV        ANDLT     D82DFV
183400961121     C                   MOVE      'E2 '         KCEV
183500060913     C                   EXSR      GEDIS27
183600961121   X5C                   ELSE
183700961121     C* CREO EVENTO E2A SOLO SE IL COLLO NON E' IN PARTENZA OGGI
183800961121     C                   Z-ADD     620           KCAA
183900990519     C     KANM2         SETLL     FNANM02L                               31
184000970122     C  N31              MOVE      'E6B'         KCEV
184100120823     C  N31KEVS2A        SETLL     FNEVS02L                               31
184200961121    6C     *IN31         IFEQ      *OFF
184300040419     c* Memorizzo, se esiste, data discordanza E6B se > data E2 più recente
184400040419     c     kevs2         readpe    fnevs02l                               31
184500040419     c     *in31         ifeq      *off
184600040419     c     evsdev        andgt     wdev2a
184700040419     c                   eval      wdev2a=evsdev
184800040419     c                   endif
184900040415     c* creo la discordanza solo se non sono passati più di x giorni
185000040415     c* dalla data dell'E2/E6B più recente
185100040416     c     *iso          Movel     wdev2a        wdta
185200040416     c     *iso          Movel     d82dfv        wdta1
185300040416     c                   Adddur    §7agde:*D     wdta
185400040416     c     wdta          ifge      wdta1
185500961121     C                   MOVE      'E2A'         KCEV
185600060913     C                   EXSR      GEDIS27
185700040416     c                   endif
185800961121    6C                   END
185900961121    5C                   END
186000970117   4AC                   END
186100970117    4C                   END
186200961120    3C                   END
186300961120    2C                   END
186400961129     C     REDA10        TAG
186500961128     C                   Z-ADD     10            KCAA
186600990519     C     KANM10        READE(N)  FNANM10L                               31
186700961120    1C                   ENDDO
186800020301     c*
186900020301     c                   endif
187000961120     C*
187100961121     C*----------------------------------------------------------------
187200961114     C* LEGGO COLLI DELLE BOLLE CON DTR = 0 E:
187300961114     C* - AGGIORNO DTR SE COLLO ARRIVATO E NON E' A TERRA
187400961114     C* - RILEVO DISCORDANZE 1, 1A, 3, 5, 5A
187500961121     C*----------------------------------------------------------------
187600961115     C                   SETOFF                                       42
187700961114     C                   MOVE      *ZEROS        KDTR
187800961114     C     KART55        SETLL     FNART55L
187900961114     C   20KART55        READE     FNART55L                               31
188000961114     C  N20KART55        READE(N)  FNART55L                               31
188100961114    1C     *IN31         DOWEQ     *OFF
188200970627     C* ESCLUDO COLLI LOCALI
188300961203     C     ARTLNP        IFEQ      ARTLNA
188400961203     C   20              UNLOCK    FNART55L
188500961203     C                   GOTO      LEG55
188600961203     C                   END
188700961114     C*
188800961115     C                   CLEAR                   WFIL
188900961115     C                   CLEAR                   WCODIS
189000961120     C                   CLEAR                   WNPS
189100961120     C                   CLEAR                   WFNPS
189200961114     C*
189300961114     C* A CAMBIO SPEDIZIONE CHAINO FNARB
189400961114     C                   MOVEL     ARTAAS        AAS
189500961114     C                   MOVEL     ARTLNP        LNP
189600961114     C                   MOVEL     ARTNRS        NRS
189700961114     C                   MOVEL     ARTNSP        NSP
189800961114    2C     ARTSPE        IFNE      SAVSPE
189900970124     C     KARB1         CHAIN(N)  FNARB01L                           31
190000961114     C                   MOVE      ARTSPE        SAVSPE
190100961114    2C                   END
190200990412      * SE HO  COLLO ARRIVATO MA NON SPARATO FORZO L'ARRIVO   Solo se no bis
190300990412     C                   CLEAR                   WARFOR
190400961114     C*
190500961114    2C                   SELECT
190600990412     C* C O L L O   N O N   A R R I V A T O  e non forzato l'arrivo
190700970116     C* O ARRIVATO IN DATA > ALLA DATA DEL FOGLIO DI DEFLUENZA (SOLO SE
190800970116     C* NO BIS)
190900961114     C     ARTDAM        WHENEQ    *ZEROS
191000990412     C     WARFOR        ANDEQ     *BLANKS
191100970117     C     *IN22         ANDEQ     *OFF
191200961224     C     ARTDAM        ORGT      D82DFV
191300970117     C     *IN22         ANDEQ     *OFF
191400961121     C   20              UNLOCK    FNART55L
191500961114     C* - RELATIVO A SPEDIZIONE DEL MANIFEST DEL GIORNO --> DISC. 1A
191600961212    3C     ARBDT3        IFEQ      D82DFV
191700961114     C     ARBDT3        OREQ      *ZEROS
191800961114     C     ARBFT3        ANDEQ     'T'
191900961115     C                   MOVE      'E1A'         WCODIS
192000961114   X3C                   ELSE
192100961114     C* - RELATIVO A SPEDIZIONE DI MANIFEST PRECEDENTE ---> DISC. 5A
192200961114    4C     ARBDT3        IFGT      *ZEROS
192300961212     C     ARBDT3        ANDLT     D82DFV
192400040416     c* creo la discordanza solo se non sono passati più di x giorni
192500040416     c* dalla data del'invio del record bolla
192600040416     c     *iso          Movel     arbdt3        wdta
192700040416     c     *iso          Movel     d82dfv        wdta1
192800040416     c                   Adddur    §7agde:*D     wdta
192900040416     c     wdta          ifge      wdta1
193000961115     C                   MOVE      'E5A'         WCODIS
193100040416     c                   endif
193200961114    4C                   END
193300961114    3C                   END
193400961120     C*
193500961115    3C     WCODIS        IFNE      *BLANKS
193600961127     C                   MOVE      WCODIS        KCEV
193700961127     C                   MOVE      ARTFLS        KFLS
193800961127     C                   MOVE      ARTLNA        KLNA
193900961127     C                   MOVE      ARTNRS        KNRS
194000961127     C                   MOVE      ARTNSC        KNSC
194100961127     C                   MOVE      ARTAAS        WAAS
194200961127     C                   MOVE      ARTLNP        WLNP
194300961127     C                   MOVE      ARTNSP        WNSP
194400970117     C* STAMPO DISCORDANZA
194500970117     C     D82LDS        IFEQ      'SI'
194600970117     C                   EXSR      PRTDIS
194700970117     C                   END
194800961114     C* SE ELABORAZIONE DEFINITIVA CREO L'EVENTO
194900961127     C   20              EXSR      CRTEVS
195000961114    3C                   END
195100961114     C*
195200961114     C* C O L L O    A R R I V A T O
195300990412     C* o forzato l'arrivo (solo se no BIS)
195400961212     C     ARTDAM        WHENLE    D82DFV
195500990412     C     WARFOR        OREQ      'S'
195600961115     C                   MOVE      ARTFLS        KFLS
195700961115     C                   MOVE      ARTLNA        KLNA
195800961115     C                   MOVE      ARTNRS        KNRS
195900961115     C                   MOVE      ARTNSC        KNSC
196000970116     C*
196100970117   3AC     *IN22         IFEQ      *OFF
196200961122     C                   Z-ADD     645           KCAA
196300961115     C                   EXSR      CTRANM
196400961115     C* COLLO A TERRA
196500961115    3C     *IN09         IFEQ      *ON
196600961120     C* MEMORIZZO NUMERO PISTOLA
196700961120     C                   MOVE      ANMNPS        WNPS
196800961114     C* - DISCORDANZA 1
196900961212    4C     ARBDT3        IFEQ      D82DFV
197000961114     C     ARBDT3        OREQ      *ZEROS
197100961114     C     ARBFT3        ANDEQ     'T'
197200961114     C                   MOVE      'E1 '         WCODIS
197300961115     C                   MOVE      ANMFLE        WFIL
197400961114   X4C                   ELSE
197500961114     C* - DISCORDANZA 5
197600961114    5C     ARBDT3        IFGT      *ZEROS
197700961212     C     ARBDT3        ANDLT     D82DFV
197800961114     C                   MOVE      'E5 '         WCODIS
197900961115     C                   MOVE      ANMFLE        WFIL
198000961114    5C                   END
198100961114    4C                   END
198200970116    3C                   END
198300970116  X3AC                   ELSE
198400970116     C* SE BIS VERIFICO SE IN PARTENZA
198500970116     C                   EXSR      CHKBRV
198600970116   3AC                   END
198700970116     C*
198800970116    3C     *IN09         IFEQ      *OFF
198900961122    4C     ARBDT3        IFGT      *ZEROS
199000961212     C     ARBDT3        ANDLT     D82DFV
199100970627     C     ARTDCM        ANDEQ     *ZEROS
199200970116     C* E3 - RECORD GIA' INVIATO:INVIO COLLO
199300961114     C                   MOVE      'E3 '         WCODIS
199400970117    4C                   END
199500961224     C* DETERMINO NUMERO PISTOLA E SE ESISTE SPUNTA DI DEFLUENZA
199600970116     C* (SOLO SE NO BIS)
199700970117     C  N22              EXSR      CHKBRV
199800961122    3C                   END
199900060913     C*
200000060913     c*
200100060913     C* COLLO ARRIVATO DA INVIARE MA NON INVIATO record bolla perchè
200200060913     c*  invio parziale
200300060913     c                   if        not *in09 and arbft3=' ' and d82isc='S'
200400060915     c                             and   artdcm=0
200500060913     c* Se sto inviando collo --> disc. E7 altrimenti avviso
200600060913     c* che c'è ancora una parziale (E7A)
200700060913     c                   exsr      CTRE7x
200800060913     c                   endif
200900060913     c
201000060913     c
201100961114    3C     WCODIS        IFNE      *BLANKS
201200961127     C                   MOVE      WCODIS        KCEV
201300961127     C                   MOVE      ARTAAS        WAAS
201400961127     C                   MOVE      ARTLNP        WLNP
201500961127     C                   MOVE      ARTNSP        WNSP
201600970117     C* STAMPO DISCORDANZA
201700970117     C     D82LDS        IFEQ      'SI'
201800970117     C                   EXSR      PRTDIS
201900970117     C                   END
202000961114     C* SE ELABORAZIONE DEFINITIVA CREO EVENTO
202100961127     C   20              EXSR      CRTEVS
202200961114    3C                   END
202300970117     C* SE COLLO IN PARTENZA AGGIORNO ARTDTR
202400970117    3C     *IN09         IFEQ      *OFF
202500961205     C     *IN20         ANDEQ     *ON
202600970627     C     ARTDCM        ANDEQ     *ZEROS
202700120821     c     d82ftt        andne     FEDEX
202800020304     c* per fedex lo deve fare solo se sono arrivati tutti i colli
202900020304    3C     *IN09         oreq      *OFF
203000020304     C     *IN20         ANDEQ     *ON
203100020304     C     ARTDCM        ANDEQ     *ZEROS
203200020304     c     arbft3        andeq     'T'
203300961120     C* RICHIAMO ROUTINE PER AUTOGENERARE SPUNTA PARTENZA SE NON C'E'
203400961115     C* E CHIUDERE EVENTUALI ANOMALIE 145 E 146 DEI GIORNI PRECEDENTI
203500961115     C                   SETON                                        43
203600961115     C                   EXSR      PARCOL
203700961212     C                   MOVE      D82DFV        ARTDTR
203800961114     C                   UPDATE    FNART055
203900961114   X3C                   ELSE
204000961114     C   20              UNLOCK    FNART55L
204100961114    3C                   END
204200961114     C*
204300961114    2C                   ENDSL
204400961203     C*
204500961203     C     LEG55         TAG
204600961203     C*
204700961114     C   20KART55        READE     FNART55L                               31
204800961114     C  N20KART55        READE(N)  FNART55L                               31
204900961114    1C                   ENDDO
205000961121     C*----------------------------------------------------------------
205100961120     C* CONTROLLO DISCORDANZE 6A - COLLO DATO PER INOLTR.MA IN PARTENZA
205200961121     C*----------------------------------------------------------------
205300961115     C                   CLEAR                   WFIL
205400961115     C* LEGGO ANOMALIE 620 PER LNA
205500961115     C                   Z-ADD     620           KCAA
205600990519     C     KANM10        SETLL     FNANM10L
205700990519     C   20KANM10        READE     FNANM10L                               31
205800990519     C  N20KANM10        READE(N)  FNANM10L                               31
205900961121    1C     *IN31         DOWEQ     *OFF
206000961203     C* ESCLUDO LOCALI
206100961203     C     ANMLNP        IFGT      *ZEROS
206200961203     C     ANMLNP        ANDEQ     ANMLNA
206300961203     C     ANMLNP        OREQ      *ZEROS
206400961203     C     ANMFLS        ANDEQ     ANMLNA
206500990519     C   20              UNLOCK    FNANM10L
206600961203     C                   GOTO      LEG620
206700961203     C                   END
206800970121     C*
206900970121     C                   MOVE      ANMFLS        KFLS
207000970121     C                   MOVE      ANMLNA        KLNA
207100970121     C                   MOVE      ANMNRS        KNRS
207200970121     C                   MOVE      ANMSCN        KNSC
207300970121     C*
207400970121     C* SE BIS CERCO SPUNTA DEFLUENZA
207500970117     C   22              EXSR      CHKBRV
207600961203     C*
207700970117     C     *IN22         IFEQ      *ON
207800970117     C     *IN09         ANDEQ     *OFF
207900970117     C     *IN22         OREQ      *OFF
208000961118     C* DETERMINO QUANDO IL COLLO E' STATO DATO PER PARTITO
208100961118     C                   EXSR      RCRDTP
208200961205     C                   EXSR      GEDE6A
208300970117     C                   END
208400961121     C*
208500961203     C     LEG620        TAG
208600961129     C                   Z-ADD     620           KCAA
208700990519     C   20KANM10        READE     FNANM10L                               31
208800990519     C  N20KANM10        READE(N)  FNANM10L                               31
208900961121    1C                   ENDDO
209000961121     C*----------------------------------------------------------------
209100961120     C* CONTROLLO DISCORDANZE 6  - COLLO DATO PER INOLTRATO MA A TERRA
209200961121     C*----------------------------------------------------------------
209300970117     C* LEGGO ANOMALIE 600 PER LNA SOLO SE NO BIS
209400970117   1AC     *IN22         IFEQ      *OFF
209500961115     C                   Z-ADD     600           KCAA
209600990519     C     KANM10        SETLL     FNANM10L
209700990519     C   20KANM10        READE     FNANM10L                               31
209800990519     C  N20KANM10        READE(N)  FNANM10L                               31
209900961121    1C     *IN31         DOWEQ     *OFF
210000961203     C* ESCLUDO LOCALI
210100961203     C     ANMLNP        IFGT      *ZEROS
210200961203     C     ANMLNP        ANDEQ     ANMLNA
210300961203     C     ANMLNP        OREQ      *ZEROS
210400961203     C     ANMFLS        ANDEQ     ANMLNA
210500990519     C   20              UNLOCK    FNANM10L
210600961203     C                   GOTO      LEG600
210700961203     C                   END
210800961203     C*
210900961118     C* DETERMINO DATA IN CUI IL COLLO E' STATO DATO PER PARTITO
211000961121     C                   MOVE      ANMFLS        KFLS
211100961121     C                   MOVE      ANMLNA        KLNA
211200961121     C                   MOVE      ANMNRS        KNRS
211300961121     C                   MOVE      ANMSCN        KNSC
211400961118     C                   EXSR      RCRDTP
211500961205     C*
211600961205     C                   MOVE      ANMFLE        WFIL
211700961205     C                   MOVE      ANMNPS        WNPS
211800961205     C*
211900961205     C                   SETON                                        09
212000961205     C* IL COLLO PUO' NON ESSERE PIU' A TERRA SE L'ANOMALIA 600 CHE
212100961205     C* STO LEGGENDO HA DATA < ALLA DATA DEL FOGLIO DI DEFL. CHE STO
212200961205     C* CHIUDENDO
212300961212     C     ANMDAO        IFLT      D82DFV
212400961205     C                   Z-ADD     645           KCAA
212500961205     C                   EXSR      CTRANM
212600961205     C                   END
212700961205     C*
212800961205    2C     *IN09         IFEQ      *ON
212900961127     C                   MOVE      'E6 '         KCEV
213000970117     C* STAMPO LA DISCORDANZA
213100970117     C     D82LDS        IFEQ      'SI'
213200970117     C                   EXSR      PRTDIS
213300970117     C                   END
213400961115     C*
213500961113     C* ELABORAZIONE DEFINITIVA
213600961205     C   20              EXSR      CRTEVS
213700961121     C*
213800961121     C*
213900961121     C* COMPLETO AGGIORNAMENTI SE ELABORAZIONE DEFINITIVA
214000961121     C* SE C'E' RECORD BOLLA AGGIORNO ARTDTR=0
214100961205    3C     *IN20         IFEQ      *ON
214200961205    4C     WBOLLA        IFEQ      'S'
214300961121     C                   MOVE      *ZEROS        ARTDTR
214400961121     C                   UPDATE    FNART000
214500961205    4C                   END
214600961121     C* CANCELLO ANOMALIA
214700990519     C                   DELETE    FNANM000
214800961205    3C                   END
214900961205   X2C                   ELSE
215000961205     C* IL COLLO NON E' PIU' A TERRA MA E' IN PARTENZA
215100961205     C                   CLEAR                   WFIL
215200961205     C                   EXSR      GEDE6A
215300961205    2C                   END
215400961203     C*
215500961203     C     LEG600        TAG
215600961203     C*
215700961205     C                   Z-ADD     600           KCAA
215800990519     C   20KANM10        READE     FNANM10L                               31
215900990519     C  N20KANM10        READE(N)  FNANM10L                               31
216000961121    1C                   ENDDO
216100970117   1AC                   END
216200961113     C                   ENDSR
216300060913     c*****************************************************************
216400060913     C* VERIFICO ESISTENZA DISCORDANZA E7-spedizione parziale ma invio
216500060913     C*                                  il collo
216600060913     c*****************************************************************
216700060913     c     CTRE7x        BEGSR
216800060913     C                   MOVE      'E7 '         KCEV
216900060913     C     KEVS2         SETGT     FNEVS02L
217000060913     C     KEVS2         READPE    FNEVS02L                               31
217100060913    4C     *IN31         IFEQ      *ON
217200060913     C* CREO E STAMPO DISCORDANZA E7
217300060913     C                   EXSR      GEDIS27
217400060913   X4C                   ELSE
217500060913   4AC     EVSDEV        IFLT      D82DFV
217600060913     C     *IN22         ANDEQ     *OFF
217700060913     C* ESISTE EVENTO 2 CREATO IN UN GIORNO PRECEDENTE --> MEMORIZZO LA
217800060913     C* DATA E LA PISTOLA
217900060913     C                   MOVE      EVSDEV        WDEV
218000060913     C                   MOVE      EVSNPS        WNPS
218100060913     C* CERCO ANCHE SE ESISTE EVENTO 7A
218200060913     C                   MOVE      'E7A'         KCEV
218300060913     C* SE TROVO E7A IN DATA DEL FOGLIO NON FACCIO NIENTE
218400120823     C     KEVS2A        SETLL     FNEVS02L                               31
218500060913     C     *IN31         CABEQ     *ON           fineCTRE7
218600060913     C     KEVS2         SETGT     FNEVS02L
218700060913     C     KEVS2         READPE    FNEVS02L                               31
218800060913    5C     *IN31         IFEQ      *OFF
218900060913     C* TENGO LA DATA EVENTO MAGGIORE FRA EVENTO E2 ED EVENTO E2A
219000060913    6C     EVSDEV        IFGT      WDEV
219100060913     C                   MOVE      EVSDEV        WDEV
219200060913    6C                   END
219300060913    5C                   END
219400060913     C* CERCO SE ESISTE EVENTO 6-COLLO DATO PER INOLTRATO MA A TERRA
219500060913     C* IN DATA > WDEV
219600060913     C                   MOVE      'E6 '         KCEV
219700060913     C     KEVS2B        SETGT     FNEVS02L
219800060913     C     KEVS2         READE     FNEVS02L                               31
219900060913    5C     *IN31         IFEQ      *OFF
220000060913     C     EVSDEV        ANDLT     D82DFV
220100060913     C                   MOVE      'E7 '         KCEV
220200060913     C                   EXSR      GEDIS27
220300060913   X5C                   ELSE
220400060913     C* CREO EVENTO E7A SOLO SE IL COLLO NON E' IN PARTENZA OGGI
220500060913     C                   Z-ADD     620           KCAA
220600060913     C     KANM2         SETLL     FNANM02L                               31
220700060913     C  N31              MOVE      'E7B'         KCEV
220800120823     C  N31KEVS2A        SETLL     FNEVS02L                               31
220900060913    6C     *IN31         IFEQ      *OFF
221000060913     C                   MOVE      'E7A'         KCEV
221100060913     C                   EXSR      GEDIS27
221200060913    6C                   END
221300060913    5C                   END
221400060913   4AC                   END
221500060913    4C                   END
221600060913    cc     FINECTRE7     ENDSR
221700961205     C*****************************************************************
221800961205     C* STAMPA + CREAZIONE DISCORDANZA 6A
221900961205     C*****************************************************************
222000961205     C     GEDE6A        BEGSR
222100970124     C* VERIFICO SE ESISTE GIA' DISCORDANZA: POTREBBE ESSERE, INFATTI,
222200970124     C* GIA' STATA CREATA DALLA PRECEDENTE CHIUSURA DEL BIS
222300970124     C* SE ESISTE GIA' NON FACCIO NIENTE
222400970124     C                   MOVEL     'E6A'         KCEV
222500970124     C     KEVS2A        SETLL     FNEVS02L                               31
222600060913     c                   if        not *in31
222700060913     C                   MOVEL     'E6B'         KCEV
222800060913     C     KEVS2A        SETLL     FNEVS02L                               31
222900060913     c                   endif
223000060913     c                   if        not *in31
223100060913     C                   MOVEL     'E7B'         KCEV
223200060913     C     KEVS2A        SETLL     FNEVS02L                               31
223300060913     c                   endif
223400060913     c
223500970124    1C     *IN31         IFEQ      *OFF
223600961205     C*
223700961205     C* DETERMINO NUMERO PISTOLA E SE ESISTE SPUNTA DI DEFLUENZA
223800970117     C  N22              EXSR      CHKBRV
223900970117     C*
224000970117     C     WBOLLA        IFEQ      'S'
224100060913     c* Se spedizioni completa E6A, se non inviata bolla per
224200060913     c*  invio solo parziale, E7B
224300060913     c                   if        d82isc=' ' or arbft3='T'
224400970117     C                   MOVE      'E6A'         KCEV
224500060913     c                   else
224600060913     C                   MOVE      'E7B'         KCEV
224700060913     c                   endif
224800060913     c
224900970117     C                   ELSE
225000970117     C                   MOVE      'E6B'         KCEV
225100970117     C                   END
225200970117     C* STAMPO DISCORDANZA
225300970117     C     D82LDS        IFEQ      'SI'
225400970117     C                   EXSR      PRTDIS
225500970117     C                   END
225600970117     C*
225700961205     C* ELABORAZIONE DEFINITIVA
225800961205    2C     *IN20         IFEQ      *ON
225900970117     C* RICHIAMO ROUTINE PER AUTOGENERARE SPUNTA PARTENZA SE NON C'E'
226000970117     C* E CHIUDERE EVENTUALI ANOMALIE 145 E 146 DEI GIORNI PRECEDENTI
226100970117     C     WBOLLA        COMP      'S'                                424243
226200970117     C                   EXSR      PARCOL
226300970117     C*
226400961205     C                   EXSR      CRTEVS
226500961205     C*
226600961205     C* SE ELAB.DEFINITIVA E C'è REC.BOLLA AGGIORNO ARTDTR=DATA EVENTO
226700961205    3C     WBOLLA        IFEQ      'S'
226800961205     C                   MOVE      EVSDEV        ARTDTR
226900961205     C                   UPDATE    FNART000
227000961205    3C                   END
227100961205     C* CANCELLO ANOMALIA
227200990519     C                   DELETE    FNANM000
227300961205    2C                   END
227400970124    1C                   END
227500961205     C                   ENDSR
227600961120     C*****************************************************************
227700060913     C* STAMPA + CREAZIONE DISCORDANZA E2/E2A/E7/E7A
227800961120     C*****************************************************************
227900060913     C     GEDIS27       BEGSR
228000961120     C*
228100961121     C* SE DISCORDANZA 2 DETERMINO SE ESISTE LA SPUNTA DI DEFLUENZA
228200961121     C* E IL NUMERO PISTOLA
228300961121     C     KCEV          IFEQ      'E2 '
228400970117     C     *IN22         ANDEQ     *OFF
228500060913     c
228600060913     C     KCEV          orEQ      'E7 '
228700060913     C     *IN22         ANDEQ     *OFF
228800961121     C                   EXSR      CHKBRV
228900961120     C                   END
229000970117     C*
229100961127     C                   CLEAR                   WAAS
229200961127     C                   CLEAR                   WLNP
229300961127     C                   CLEAR                   WNSP
229400970117     C***
229500970117     C*  STAMPO DISCORDANZA
229600970117     C     D82LDS        IFEQ      'SI'
229700970117     C                   EXSR      PRTDIS
229800970117     C                   END
229900961120     C****
230000060913     C* CREO EVENTO
230100961205    4C     *IN20         IFEQ      *ON
230200970117     C* SOLO SE CREO DISCORDANZA 2 (ESCLUDO QUINDI SE DISC. 2A):
230300970117     C* RICHIAMO ROUTINE PER AUTOGENERARE SPUNTA PARTENZA SE NON C'E'
230400970117     C* E CHIUDERE EVENTUALI ANOMALIE 145 E 146 DEI GIORNI PRECEDENTI
230500970117     C     KCEV          IFEQ      'E2 '
230600060913     C     KCEV          oreq      'E7 '
230700970117     C                   SETON                                        42
230800970117     C                   EXSR      PARCOL
230900970117     C                   END
231000970117     C*
231100961120     C                   EXSR      CRTEVS
231200961120     C*
231300961120    4C                   ENDIF
231400961120     C*
231500961120     C                   ENDSR
231600961121     C*****************************************************************
231700961121     C*    DETERMINO ESISTENZA SPUNTA DI DEFLUENZA E NUMERO PISTOLA   *
231800961121     C*****************************************************************
231900961121     C     CHKBRV        BEGSR
232000961121     C*
232100961121     C                   CLEAR                   WFNPS
232200961121     C                   Z-ADD     1             KNPG
232300070123     C     KBRV1         CHAIN(n)  FNBRV01L                           31
232400961121     C     *IN31         IFEQ      *OFF
232500961121     C                   MOVE      BRVNPS        WNPS
232600961121     C                   ELSE
232700961121     C                   MOVE      88            WNPS
232800961122     C                   MOVE      '1'           WFNPS             1
232900961121     C                   END
233000970116     C*
233100970116     C* SE FV BIS IMPOSTO *IN09
233200970117     C   22WFNPS         IFEQ      '1'
233300970116     C                   SETON                                        09
233400970116     C                   ELSE
233500970116     C                   SETOFF                                       09
233600970116     C                   END
233700961121     C                   ENDSR
233800961118     C*****************************************************************
233900961118     C*    DETERMINO QUANDO COLLO E' STATO DATO PER PARTITO           *
234000961118     C*****************************************************************
234100961118     C     RCRDTP        BEGSR
234200961118     C*
234300961118     C                   CLEAR                   WDTPAR
234400961118     C                   MOVE      *BLANKS       WBOLLA            1
234500961118     C*
234600961118     C  N20KART27        CHAIN(N)  FNART27L                           31
234700961118     C   20KART27        CHAIN     FNART27L                           31
234800961118     C     *IN31         IFEQ      *OFF
234900961118     C                   MOVE      'S'           WBOLLA
235000961118     C                   MOVE      ARTDTR        WDTPAR
235100961118     C                   MOVE      ARTAAS        WAAS
235200961118     C                   MOVE      ARTLNP        WLNP
235300961118     C                   MOVE      ARTNSP        WNSP
235400060913     c
235500060913     c* Se devo inviare soltanto le spedizioni complete, chaino anche
235600060913     c*  la bolla per sapere se inviata
235700060913     c                   if        d82isc='S'
235800060913     c     karb1         chain(n)  fnarb01l
235900060913     c                   endif
236000060913     c
236100961118     C                   ELSE
236200970602     C                   CLEAR                   WAAS
236300961118     C                   MOVE      ANMAAS        WAAS
236400961118     C                   MOVE      ANMLNP        WLNP
236500961118     C                   MOVE      ANMNSP        WNSP
236600961118     C                   MOVE      'E2 '         KCEV
236700961121     C     KEVS2         SETGT     FNEVS02L
236800961127     C     KEVS2         READPE    FNEVS02L                               31
236900961118     C  N31              MOVE      EVSDEV        WDTPAR
237000060913     c*
237100961118     C                   END
237200961118     C                   ENDSR
237300961112     C*****************************************************************
237400961112     C*    SCRITTURA EVENTO                                           *
237500961112     C*****************************************************************
237600961113     C     CRTEVS        BEGSR
237700961113     C                   CLEAR                   FNEVS000
237800961115     C*
237900961115     C                   MOVE      WAAS          EVSAAS
238000961115     C                   MOVE      WLNP          EVSLNP
238100961115     C                   MOVE      WNSP          EVSNSP
238200961115     C*
238300961115     C                   MOVE      KFLS          EVSFLS
238400961115     C                   MOVE      KLNA          EVSLNA
238500961115     C                   MOVE      KNRS          EVSNRS
238600961115     C                   MOVE      KNSC          EVSNSC
238700961120     C                   MOVE      WNPS          EVSNPS
238800961115     C*
238900961212     C                   MOVE      D82DFV        EVSDEV
239000961115     C                   MOVE      KCEV          EVSCEV
239100961115     C* SE COLLO A TERRA MEMORIZZO FILIALE CHE HA SPARATO IL COLLO
239200961115     C                   MOVE      WFIL          EVSFIL
239300961115     C*
239400961115     C* EVSDAT --> PER ORA MI SERVE PER DISCORDANZA 6 PER MEMORIZZARE
239500961115     C*            LA DATA IN CUI IL COLLO ERA STATO DATO PER PARTITO
239600961115     C                   MOVE      WDTPAR        EVSDAT
239700961125     C     WDTPAR        IFGT      *ZEROS
239800961115     C                   MOVEL     COS1          EVSDES
239900961125     C                   END
240000961115     C*
240100961115     C                   WRITE     FNEVS000
240200120823     C*
240300961112     C                   ENDSR
240400961114     C*****************************************************************
240500961122     C*    RICERCA ANOMALIA 645  IN DATA = DATA FOGLIO DEFLUENZA      *
240600961114     C*****************************************************************
240700961114     C     CTRANM        BEGSR
240800020225     C                   SETOFF                                       09
240900020225     C     KANM2A        CHAIN     FNANM02L                           31
241000020225     C  N31              SETON                                        09
241100961114     C                   ENDSR
241200961115     C*****************************************************************
241300961115     C*    OPERAZIONI PER COLLO CHE PARTE                             *
241400961115     C*****************************************************************
241500961115     C     PARCOL        BEGSR
241600961122     C* CHIUSURA ANOMALIE 145 E 146
241700961122     C                   Z-ADD     145           KCAA
241800961118    1C                   DO        2
241900990519     C     KANM2         CHAIN     FNANM02L                           31
242000961122    2C     *IN31         IFEQ      *OFF
242100020225     C                   CLEAR                   FNLR33DS
242200961212     C                   MOVE      D82DFV        D33DCH
242300961115     C                   MOVE      ANMNR2        D33NRR
242400961115     C* PRENDO FASE ANOMALIA
242500961115     C                   CALL      'TRUL11R'
242600961115     C                   PARM                    PARAM9
242700961115     C                   MOVE      FASE          D33FSC
242800961115     C                   CALL      'FNLR33R'
242900020225     C                   PARM                    FNLR33DS
243000961118    3C                   END
243100961115     C* RIPETO GIRO PER 146
243200961115     C                   Z-ADD     146           KCAA
243300961118    1C                   ENDDO
243400961115     C*
243500961115     C* AUTOGENERAZIONE SPUNTA PARTENZA 88 SE NON ESISTE
243600961120     C     WFNPS         IFEQ      '1'
243700961120     C* SE DEVO AUTOGENERARE SPUNTA PARTENZA DI COLLO CON MANCA RECORD
243800961120     C* BOLLA ARRIVO CERCO LA ZONA LEGGENDO UNA QUALUNQUE SPUNTA DEL
243900961120     C* COLLO
244000961120     C     *IN42         IFEQ      *ON
244100961120     C                   CLEAR                   WRVZNC
244200070123     C     KBRV7         CHAIN     FNBRV07L                           31
244300961120     C     *IN31         IFEQ      *OFF
244400961120     C                   MOVE      BRVZNC        WRVZNC
244500961120     C                   END
244600961120     C                   END
244700980126     C*
244800980126     C                   EXSR      DOPPIE
244900980126     C*
245000140625     C   40              CLEAR                   FNBRV5
245100040303     c  n40*in41         ifeq      *off
245200040303    7C     NST(WA)       orne      '1'                                          NR.SPUNTE GG.
245300070123     C                   CLEAR                   FNBRV000
245400040303     c                   else
245500140625     C                   CLEAR                   FNBRV5
245600040303     c                   endif
245700961115     C*
245800961212     C                   Z-ADD     SIMFEL        BRVFGS                         FIL GESTIONE
245900000208     C                   Z-ADD     SIMFEL        BRVPES                         FIL GESTIONE
246000000208     C                   Z-ADD     D82NFV        BRVNFV                         NUM.FOGLIO
246100961115     C                   Z-ADD     1             BRVNPG                         CAT.FOGLIO
246200961115     C                   Z-ADD     SIMFEL        BRVFLE                         FIL.ELABORAT
246300070220     c                   time                    wtimez
246400070220     c* millisecondi
246500070220     c                   extrct    wtimez:*ms    millisec          6 0
246600070220     c                   movel     millisec      brvmis
246700070220     c* data immissione
246800070220     c                   movel     wtimez        dataiso
246900070220     c                   movel     dataiso       brvdfs
247000070220     c* ora immissione
247100070220     c                   move      wtimez        oraiso
247200070220     c                   movel     oraiso        brvhms
247300070220     c*
247400070220     c                   movel     'A'           brvtap
247500070220     c                   eval      brvpru='§'+ nompgm
247600961204     C                   MOVE      KFLS          BRVLNP
247700961204     C                   MOVE      KLNA          BRVLNA
247800961204     C                   MOVE      KNRS          BRVNRS
247900961204     C                   MOVE      KNSC          BRVNSC
248000961115     C*
248100961204     C*  DA ANOMALIE
248200961204     C   42              MOVE      WRVZNC        BRVZNC                          ZONA
248300961204     C*  DA FNART
248400961204     C   43              MOVE      ARBZNC        BRVZNC                          ZONA
248500961115     C*
248600961115     C                   Z-ADD     88            BRVNPS
248700961115     C*
248800140625     C   40              WRITE     FNBRV5
248900040303     c  n40*in41         ifeq      *off
249000040303    7C     NST(WA)       orne      '1'                                          NR.SPUNTE GG.
249100070123     C                   update    fnbrv000
249200040303     c                   else
249300140625     C                   update    fnbrv5
249400040303     c                   endif
249500961115     C                   END
249600961115     C*
249700961115     C                   ENDSR
249800980126     C**************************************************************************
249900040303     C* VERIFICO SE esiste già la spunta:
250000040303     C* 1) per p.o. se previsto dalla categoria, tabella 7n (4)
250100040303     C* 2) per num foglio (npg+nfv+fgs) se da tenere tutte
250200980126     C**************************************************************************
250300980126     C     DOPPIE        BEGSR
250400040303     C*
250500040303     C                   Z-ADD     1             KNPG                           CATEGORIA
250600040303     C                   Z-ADD     SIMFEL        KFLE
250700040303     C*
250800040303     C* VERIFICO IL FLAG NELLA DS7N
250900040303     C                   Z-ADD     1             WA                3 0
251000040303     C     1             LOOKUP    CAT(WA)                                41     ON=TROVATO
251100040303     c     *in41         ifeq      *off
251200040303    7C     NST(WA)       orne      '1'                                          NR.SPUNTE GG.
251300140625     C     KBRV1         CHAIN     FNBRV01L                           40
251400040303     c                   else
251500140625     C     KBRV3         CHAIN     FNBRV05L                           40
251600040303     c                   endif
251700040303     c
251800070123     C* SE C'E' L'ANOMALIA "E" SCriVO FILE FNBRVE0F
251900040303     C  n40BRVCAN        IFEQ      'E'
252000070123     C     KBRVE         SETLL     FNBRVE1L                               99
252100070123     C  N99              WRITE     FNBRV00E
252200040303     C                   ENDIF
252300980126     C*
252400980126     C                   ENDSR
252500961112     C*****************************************************************
252600961112     C*    CHAIN TABEL00F                                             *
252700961112     C*****************************************************************
252800961112     C     CTABEL        BEGSR
252900961112     C     KTAB          CHAIN     TABEL                              31
253000961114     C  N31TBLFLG        IFNE      ' '
253100961112     C                   SETON                                        31
253200961112     C                   END
253300961112     C                   ENDSR
253400961129     C**************************************************************************
253500961129     C*  ABBLANCA ZERO NON SIGNIFICATIVI
253600961129     C**************************************************************************
253700961129     C     ABBL          BEGSR
253800961129     C*
253900020225     C                   Z-ADD     1             YY
254000020225     C     K08(YY)       DOWEQ     '0'
254100020225     C     YY            ANDLT     8
254200020225     C                   MOVEL     ' '           K08(YY)
254300020225     C                   ADD       1             YY
254400961129     C                   ENDDO
254500961129     C*
254600961129     C                   ENDSR
254700961121     C*****************************************************************
254800961121     C* STAMPA DISCORDANZE -------------------------------------------*
254900961121     C*****************************************************************
255000961121     C     PRTDIS        BEGSR
255100961121     C*
255200961122     C                   SETON                                        08
255300961125     C*
255400961125     C     WFIL          COMP      *ZEROS                                 14
255500961127     C*
255600961127     C     WNSP          COMP      *ZEROS                             1515
255700961127     C     KNSC          COMP      *ZEROS                             1616
255800961127     C     KNRS          COMP      *ZEROS                             1717
255900961128     C*
256000961129     C     KCEV          COMP      'E4 '                                  18
256100060918     C  n18KCEV          COMP      'E8 '                                  18
256200961127     C*
256300961127     C* STAMPO ANNO DI 2
256400961127     C                   MOVE      WAAS          WPAAS             2 0
256500961122     C*
256600961122     C* DECODIFICO CAUSALE EVENTO
256700961122     C                   MOVE      '8E'          KCOD
256800961122     C                   MOVEL(P)  KCEV          KKEY
256900970114     C* prendo la descrizione in italiano
257000970114     C     KTAB          CHAIN     TABEL00F                           31
257100970114     C  N31              MOVEL     TBLUNI        DCEV             60
257200970114     C   31              CLEAR                   DCEV
257300970114     C* se devo inviarlo anche via fax prendo anche la descrizione in
257400970114     C* inglese
257500120821     c     d82ftt        ifne      FEDEX
257600970115     C   20              MOVE      '1'           KKEY
257700970115     C   20KTAB          CHAIN     TABEL00F                           31
257800970115     C  N31
257900970115     CAN 20              MOVEL     TBLUNI        DCEVE            60
258000970115     C   31
258100970115     CAN 20              CLEAR                   DCEVE
258200970115    1C   20*INOB         IFEQ      *ON
258300040423
258400040423     c                   if        FAX_Disc_Est = 'S'
258500961122     C                   ADD       1             WPAG
258600961121     C                   EXCEPT    TESDIS
258700970114     C                   SETOFF                                       OB
258800040423     c                   end
258900040423
259000961129    1C                   END
259100040423
259200040423     c                   if        FAX_Disc_Est = 'S'
259300970115     C   20              EXCEPT    DETDIS
259400040423     c                   end
259500040423
259600020301     c                   endif
259700970114    1C     *INOE         IFEQ      *ON
259800970114     C                   ADD       1             WPAG1
259900970114     C                   EXCEPT    TESDIC
260000970114     C                   SETOFF                                       OE
260100970114    1C                   END
260200970114     C                   EXCEPT    DETDIC
260300961122     C* STAMPA DATA PARTENZA COLLO SE DISCORDANZA 6 O 6A
260400961129    1C     WDTPAR        IFGT      *ZEROS
260500961122     C                   CLEAR                   WLBDA8
260600961122     C                   MOVE      WDTPAR        G02INV
260700961122     C                   MOVE      '3'           G02ERR
260800961122     C                   CALL      'XSRDA8'
260900961122     C                   PARM                    WLBDA8
261000961122     C                   MOVE      G02DAT        WDTPA1
261100120821     c     d82ftt        ifne      FEDEX
261200970115    2C   20*INOB         IFEQ      *ON
261300040423
261400040423     c                   if        FAX_Disc_Est = 'S'
261500961122     C                   ADD       1             WPAG
261600961122     C                   EXCEPT    TESDIS
261700970114     C                   SETOFF                                       OB
261800040423     c                   end
261900040423
262000961129    2C                   END
262100040423
262200040423     c                   if        FAX_Disc_Est = 'S'
262300970115     C   20              EXCEPT    DETDI2
262400970115     C   20              EXCEPT    FIT
262500040423     c                   end
262600040423
262700020301     c                   endif
262800970114    2C     *INOE         IFEQ      *ON
262900970114     C                   ADD       1             WPAG1
263000970114     C                   EXCEPT    TESDIC
263100970114     C                   SETOFF                                       OE
263200970114    2C                   END
263300970114     C                   EXCEPT    DETDC2
263400970114     C                   EXCEPT    FITC
263500961129   X1C                   ELSE
263600961129     C     KCEV          IFNE      'E4 '
263700120821     c     d82ftt        ifne      FEDEX
263800040423
263900040423     c                   if        FAX_Disc_Est = 'S'
264000970115     C   20              EXCEPT    FIT
264100040423     c                   end
264200040423
264300020301     c                   endif
264400970114     C                   EXCEPT    FITC
264500961129     C                   END
264600961129    1C                   END
264700961121     C*
264800961121     C                   ENDSR
264900960529     C*-----------------------------------------------------------------*
265000960529     C*  Operazioni iniziali
265100960529     C*-----------------------------------------------------------------*
265200960529     C     *INZSR        BEGSR
265300960529     C*
265400960529     C*  Reperisco dati da video
265500960529     C     *ENTRY        PLIST
265600960529     C                   PARM                    KPJBA
265700020225     C                   MOVEL     KPJBU         TRTC82DS
265800040423
265900040423     ** Maggio 2004:
266000120823     ** Non si vuole più mandare il FAX discordanze al Partner
266100040423     c                   move      'N'           FAX_Disc_Est      1
266200040423
266300070123     C* MUOVO N.FOGLIO IN CAMPO LUNGO 6,0 PER CHAIN SU FNBRV
266400000208     C                   Z-ADD     D82NFV        D82NFS
266500990421      * SALVO KPJBU "ORIGINALE"
266600990421     C                   MOVEL     KPJBU         WKPJBU
266700971230      *
266800971230     C* MI SALVO LINEA DI ARRIVO ESTERA (ALTRIMENTI SI SPORCA NEL
266900971230     C* RICHIAMARE IL TRTC83R2)
267000961211     C                   MOVE      D82LNP        LNAEXP
267100961129     C*  Controllo se devo scrivere il nr.spedizione compattato
267200961129     C     D82SPC        COMP      'S'                                    21
267300960529     C*  Richiamo XPARUT
267400960529     C                   Z-ADD     1             CODUT
267500960529     C                   CALL      'X§PARUT'
267600020225     C                   PARM                    UT§DSE0F
267700960529     C                   MOVEL     REC80         CNCR80
267800110511     C                   MOVEL     REC42         CNCR42
267900110511     C                   MOVEL     simRGS        RSUT             20
268000110511     C                   MOVEL     simRGS        RSUTM            19
268100020225     C* RICERCA CAPOCONTI
268200961122     C                   DO        50            X                 2 0
268300961122     C                   MOVE      TCU(X)        TCUDS
268400961122     C     F56           IFEQ      'CG'
268500961122     C     F4            ANDEQ     '1'
268600961122     C     F3            ANDEQ     '0'
268700961122     C                   Z-ADD     KCU(X)        KCI               4 0
268800020225     C                   ENDIF
268900020225     C                   ENDDO
269000961122     C* REPERISCO KG PER METRO CUBO PER CALCOLO PESO TASSABILE
269100961122     C                   MOVEL     '7A'          KCOD
269200961122     C                   MOVEL(P)  '2'           KKEY
269300961122     C     KTAB          CHAIN     TABEL00F                           31
269400961122     C  N31              MOVEL     TBLUNI        DS7A2
269500961122     C   31              CLEAR                   DS7A2
269600980126     C* SERVE SAPERE SE NELLE VARIE CATEGORIE FOGLI POSSO
269700980126     C* AVERE SPUNTE DOPPIE OPPURE NO
269800980126     C                   Z-ADD     *ZERO         WA                             CONTATORE
269900980126     C                   MOVEL     '7N'          KCOD
270000980126     C     KTAB2         SETLL     TABEL00F
270100980126     C     KTAB2         READE     TABEL00F                               31
270200980126     C     *IN31         DOWEQ     *OFF
270300980126     C     TBLFLG        IFEQ      *BLANKS
270400980126     C                   ADD       1             WA
270500980126     C                   MOVEL     TBLUNI        DS7N
270600980126     C                   MOVEL     TBLKEY        CAT(WA)
270700980126     C                   MOVEL     §7NNST        NST(WA)
270800980126     C                   ENDIF
270900980126     C     KTAB2         READE     TABEL00F                               31
271000980126     C                   ENDDO
271100990412      * CARICO TABELLA £1 SE NON SONO IN SEDE
271200990412     C     SIMFEL        IFNE      *ZEROS
271300020225     C                   CLEAR                   TRUL06DS
271400990412     C                   CLEAR                   KPJBU
271500990412     C                   MOVEL     '£1'          D06COD
271600990412     C                   MOVEL     SIMFEL        D06KEY
271700990412     C                   MOVEL     'L'           D06TLA
271800020225     C                   MOVEL     TRUL06DS      KPJBU
271900990412      *
272000990412     C                   CALL      'TRUL06R'
272100990412     C                   PARM                    KPJBA
272200020225     C                   MOVEL     KPJBU         TRUL06DS
272300990421      * REIMPOSTO KPJBU "ORIGINALE"
272400990421     C                   MOVEL     WKPJBU        KPJBU
272500990421     C                   END
272600090402     c
272700090402     c                   open(e)   tita430c
272800090402     c                   if        not %open(tita430c)
272900090402     c                   eval      %subst(WTA430:7:4)='GRPS'
273000090402     c                   eval      %subst(WTAS30:7:4)='GRPS'
273100090402     c                   open      tita430c
273200090402     c                   endif
273300090402     c                   open      titas30c
273400130215     c
273500960529     C*---------------------------------------------------------------*
273600960529     C* Definisco chiavi di accesso ai file
273700961114     C     KARB1         KLIST
273800961114     C                   KFLD                    ARTAAS
273900961114     C                   KFLD                    ARTLNP
274000961114     C                   KFLD                    ARTNRS
274100961114     C                   KFLD                    ARTNSP
274200961113     C     KARB55        KLIST
274300961211     C                   KFLD                    LNAEXP
274400961113     C                   KFLD                    KDT3
274500120823     C     KARB          KLIST
274600961121     C                   KFLD                    ARBAAS
274700961121     C                   KFLD                    ARBLNP
274800961121     C                   KFLD                    ARBNRS
274900961121     C                   KFLD                    ARBNSP
275000960529     C     KAR4          KLIST
275100961121     C                   KFLD                    ARBAAS
275200961121     C                   KFLD                    ARBLNP
275300961121     C                   KFLD                    ARBNRS
275400961121     C                   KFLD                    ARBNSP
275500960529     C                   KFLD                    KTRC
275600061127      * contatto nome e telefono (Tipo record [GEN])
275700061127     C     KAR5          KLIST
275800061127     C                   KFLD                    ARBAAS
275900061127     C                   KFLD                    ARBLNP
276000061127     C                   KFLD                    ARBNRS
276100061127     C                   KFLD                    ARBNSP
276200061127     C                   KFLD                    KGEN
276300061127     c                   movel     'GEN'         KGEN              3
276400960529     C     KTAB          KLIST
276500961122     C                   KFLD                    CODUT
276600960529     C                   KFLD                    KCOD
276700960529     C                   KFLD                    KKEY
276800980126     C     KTAB2         KLIST
276900980126     C                   KFLD                    CODUT
277000980126     C                   KFLD                    KCOD
277100961220     C     KETAB         KLIST
277200961220     C                   KFLD                    KECOD
277300961220     C                   KFLD                    KEKEY
277400130215     C     KEDI          KLIST
277500130215     C                   KFLD                    tabcod
277600130215     C                   KFLD                    tabkey
277700961113     C     KANM10        KLIST
277800961113     C                   KFLD                    KCAA
277900961211     C                   KFLD                    LNAEXP
278000961115     C     KANM2         KLIST
278100961115     C                   KFLD                    KFLS
278200961115     C                   KFLD                    KLNA
278300961115     C                   KFLD                    KNRS
278400961122     C                   KFLD                    KNSC
278500961115     C                   KFLD                    KCAA
278600961122     C     KANM2A        KLIST
278700961122     C                   KFLD                    KFLS
278800961122     C                   KFLD                    KLNA
278900961122     C                   KFLD                    KNRS
279000961122     C                   KFLD                    KNSC
279100961122     C                   KFLD                    KCAA
279200961212     C                   KFLD                    DFV06
279300961115     C     KEVS2         KLIST
279400961115     C                   KFLD                    KFLS
279500961115     C                   KFLD                    KLNA
279600961115     C                   KFLD                    KNRS
279700961122     C                   KFLD                    KNSC
279800961115     C                   KFLD                    KCEV
279900120823      **
280000961115     C     KEVS2A        KLIST
280100961115     C                   KFLD                    KFLS
280200961115     C                   KFLD                    KLNA
280300961115     C                   KFLD                    KNRS
280400961122     C                   KFLD                    KNSC
280500961115     C                   KFLD                    KCEV
280600961212     C                   KFLD                    D82DFV
280700120823      **
280800961119     C     KEVS2B        KLIST
280900961119     C                   KFLD                    KFLS
281000961119     C                   KFLD                    KLNA
281100961119     C                   KFLD                    KNRS
281200961122     C                   KFLD                    KNSC
281300961119     C                   KFLD                    KCEV
281400961120     C                   KFLD                    WDEV
281500120823      **
281600961121     C     KART27        KLIST
281700961121     C                   KFLD                    KFLS
281800961121     C                   KFLD                    KLNA
281900961121     C                   KFLD                    KNRS
282000961121     C                   KFLD                    KNSC
282100120823      **
282200120823     C     KBRV7         KLIST
282300120823     C                   KFLD                    KFLS
282400120823     C                   KFLD                    KLNA
282500120823     C                   KFLD                    KNRS
282600120823     C                   KFLD                    KNSC
282700120823      **
282800961121     C     KART55        KLIST
282900961211     C                   KFLD                    LNAEXP
283000961121     C                   KFLD                    KDTR
283100961120     C     KBRV1         KLIST
283200961118     C                   KFLD                    KNPG
283300000208     C                   KFLD                    D82NFS
283400961212     C                   KFLD                    SIMFEL
283500961118     C                   KFLD                    KFLS
283600961118     C                   KFLD                    KLNA
283700961118     C                   KFLD                    KNRS
283800961122     C                   KFLD                    KNSC
283900140625     C     KBRV3         KLIST
284000980126     C                   KFLD                    KNPG                           ..CATEGORIA
284100980126     C                   KFLD                    KFLS                           ..LNP
284200980126     C                   KFLD                    KLNA                           ..LNA
284300980126     C                   KFLD                    KNRS                           ..SERIE
284400980126     C                   KFLD                    KNSC                           ..SEGNACOLLO
284500980126     C                   KFLD                    KFLE                           ..FIL.GESTIONE
284600961122     C*
284700961122     C     KIND          KLIST
284800961122     C                   KFLD                    CODUT
284900961122     C                   KFLD                    KCI
285000961122     C                   KFLD                    D82CPT
285100010613     C     KBRVE         KLIST
285200010613     C                   KFLD                    BRVNPG
285300010613     C                   KFLD                    BRVNFV
285400010613     C                   KFLD                    BRVFGS
285500010613     C                   KFLD                    BRVLNP
285600010613     C                   KFLD                    BRVLNA
285700010613     C                   KFLD                    BRVNRS
285800010613     C                   KFLD                    BRVNSC
285900020808     c     K02FGV01      klist
286000020808     c                   kfld                    D82nfv
286100020808     C                   kfld                    SIMfel
286200110510     C     ktbe          klist
286300110510     C                   kfld                    tbecod
286400110510     C                   kfld                    tbeke1
286500110510     C                   kfld                    tbeke2
286600960529     C*---------------------------------------------------------------*
286700960529     C                   TIME                    UTIME             6 0
286800960529     C* PRENDO LA DATA DEL GIORNO DA TIME
286900960529     C                   TIME                    W0140            14 0
287000960716     C                   MOVE      W0140         UDATE8
287100960716     C                   MOVE      UDATE8        G02DAT
287200960716     C                   MOVEL     *BLANK        G02ERR
287300960716     C                   CALL      'XSRDA8'
287400960716     C                   PARM                    WLBDA8
287500960716     C                   Z-ADD     G02INV        WOGGI             8 0           UDATE
287600130215      *------------
287700130215     C* Caricamento Tabella Partner
287800130215     C                   MOVEL     'PT'          TABCOD
287900130215     C     TABCOD        setll     EDTAB01L
288000130215     C     TABCOD        reade     EDTAB01L
288100130215      *
288200130215     C                   DOW       not %EoF(edtab01l)
288300130215      *
288400130215     C     TABFLG        IFEQ      *BLANKS
288500130215     c                   eval      dsPT = tabuni
288600130215      *
288700130215      * deve uscire con un solo record  tramite il codice KSC del PARTNER
288800130215      *  se ci fossero più codici KSC uguali per motivi particolari il primo
288900130215      *  in ordine di KEY DEVE essere, x regola, quello del Partner mentre il
289000130215      *  secondo è definito come CLIENTE sempre.
289100130215     C     D82CPT        ifeq      §PTKSC
289200130215      *
289300130215      *** al suo interno carica anche l'altra tabella correlata
289400130215      **   in questo caso la PZ e poi esce
289500130215     C                   MOVEL     'PZ'          TABCOD
289600130215     C     Kedi          chain     EDTAB01L
289700130215     c                   if        %Found(edtab01l)
289800130215     c                   eval      dsPZ = tabuni
289900130215     c                   end
290000130215      *
290100130215     c                   leave
290200130215     c                   end
290300130215      *
290400130215     C                   END
290500130215      *
290600130215     C     TABCOD        READE     EDTAB01L
290700130215     C                   END
290800130215      *------------
290900110510      *
291000110510     c                   move      'VPO'         tbecod
291100110510     c                   movel     'BRT'         tbeke1
291200110510     c                   clear                   tbeke2
291300110510     c     ktbe          chain     tntbe01l
291400110510     c                   if        %found(tntbe01l)
291500110510     c                   movel     tbeuni        dvpobrt
291600110510     c                   if        Woggi  <= §VPOBRTDTA
291700110510     c                   seton                                        66
291800110510     c                   endif
291900110510     c                   endif
292000120305      *
292100120305      ***cambio di marchio in BRT finale x il 15/3
292200120305     c                   setoff                                       66
292300120305     c                   if        Woggi  <  20120315
292400120305     c                   seton                                        66
292500120305     c                   endif
292600961114     C*
292700961114     C     D82DEF        COMP      'SI'                                   20
292800970117     C     D82FVB        COMP      'B'                                    22
292900970114     C*
293000970114     C*  IMPOSTO DATA FOGLIO VIAGGIO PER STAMPA
293100970114     C                   Z-ADD     D82DFV        DATA
293200970114     C                   Z-ADD     MM            GGMM              4 0
293300970114     C                   MOVEL     GG            GGMM
293400970114     C                   Z-ADD     AA            PARDFV
293500970114     C                   MOVEL     GGMM          PARDFV
293600970114     C                   MOVE      D82DFV        DFV06
293700970114     C*  Data CMR
293800970114     C                   Z-ADD     D82DCM        DATA
293900970114     C                   Z-ADD     MM            GGMM              4 0
294000970114     C                   MOVEL     GG            GGMM
294100970114     C                   Z-ADD     AA            DATCMR            8 0
294200970114     C                   MOVEL     GGMM          DATCMR
294300020225      *
294400970114     C   20              OPEN      FNEVS01L
294500020225      *
294600020225      * IMPOSTO TIPO RECORD PER LETTURE WORK FILE
294700970113     C  N20              MOVE      'P'           WTRC
294800120821     C   20              MOVE      'D'           WTRC
294900020225      ** LETTURA/AGGIORNAMENTO FITTIZIO PER GIUSTIFICARE L'APERTURA
295000020225      ** IN UPDATE (DEVO FARLO PER UTILIZZARE IL FILE SOTTO COMIT VISTO CHE
295100020225      ** E' UTILIZZATO IN UPDATE NEI PGM DI SCRITTURA RCD PER TRASMISSIONE)
295200970124     C     *LOVAL        SETLL     FNARB01L
295300970124     C                   READ      FNARB01L                               31
295400970124     C  N31              EXCEPT    FITTIZ
295500020225      *
295600960529     C                   ENDSR
295700030317      *****************************************************************
295800030317      *  Reperisce Peso/Volume o da CML o da Bollettato
295900030317      *****************************************************************
296000030317     C     VOL_PES       BEGSR
296100030317      *
296200030317     c     *like         define    arbvlb        wvol
296300030317     c     *like         define    arbpkb        wpes
296400030317      *
296500030317      * Prima cerca il volume fra bollettato e CML
296600050112     C     arbncR        ifeq      arbncl
296700030317      * se è considerata un'unica spedizione
296800030317     C                   z-add     ARBVLC        WVOL
296900030317     C                   else
297000030317      *
297100030317     C     ARBVLC        ifge      ARBVLb
297200030317     C                   z-add     ARBVLC        WVOL
297300030317     C                   else
297400030317     C                   z-add     ARBVLb        WVOL
297500030317     C                   end
297600030317      *
297700030317     C                   endif
297800030317      *
297900030317      * Poi cerca il peso fra bollettato e CML
298000050112     C     arbncP        ifeq      arbncl
298100030317      * se è considerata un'unica spedizione
298200030317     C                   Z-ADD     arbPKC        wpes
298300030317     C                   ELSE
298400030317      *
298500030317     C     arbPKC        ifge      arbPKb
298600030317     C                   Z-ADD     arbPKC        wpes
298700030317     C                   else
298800030317     C                   Z-ADD     arbPKb        wpes
298900030317     C                   end
299000030317      *
299100030317     C                   ENDIF
299200030317      *
299300030317     C                   ENDSR
299400020225      *---------------------------------------------------------------*
299500961023     OFNARB000  E            LIBER
299600961023     OFNARB000  E            AGGTRA
299700961023     O                       ARBFT3
299800961023     O                       ARBDT3
299900961112     OFNARB000  E            AGGFT3
300000961112     O                       ARBFT3
300100970124     OFNARB001  E            FITTIZ
300200961212     O*****************************************************************
300300961212     O* MANIFEST PER IL PARTNER
300400961212     O*****************************************************************
300500961213     OMANPTN    E            TESTA            02
300600041109     O                       D46(1)             112
300700961217     O                       STARAG              75
300800961217     O                       STPFAX              97
300900041109     O              N20                         112 '* PROVVISORIO'
301000041109     O               20                         112 '* DEFINITIVO '
301100961212     O          E            TESTA            03
301200931229     O                       ST(1)               66
301300041109     O                       D46(2)             112
301400961217     O                       UDATE8              54 '  /  /    '
301500961217     O                       UTIME               89 '  :  :  '
301600041109     O                       PAGE1         Z    111
301700961212     O          E            TESTA            04
301800961217     O                       ST(4)               66
301900041109     O                       D46(3)             112
302000961217     O                       RSUTM               63
302100961217     O                       STADLA             106
302200961212     O          E            TESTA            05
302300961217     O                       ST(3)               66
302400041109     O                       D46(4)             112
302500961217     O                       UDATE8              54 '  /  /    '
302600041109     O                       D82CMR             112
302700961212     O          E            TESTA            06
302800961217     O                       ST(19)              66
302900041109     O                       D46(5)             112
303000041109     O                       DATCMR              87 '  /  /    '
303100961212     O          E            TESTA          1 08
303200931229     O                       ST(6)               66
303300041109     O                       D46(6)             112
303400021204      *--------
303500931229     O          E            DET         1
303600100330     O              N21      §LB14               16
303700961129     O               21      §LB8                16
303800941222     O                       STARMN        Z   +  1
303900941222     O                       STARSM            +  1
304000941222     O                       STARSD            +  2
304100940111     O                       STATBO            +  1
304200940111     O                       ARBNCL        Z   +  2
304300961125     O                       PESPRT        4   +  2
304400041109     O                    N34PRTCAS        4   +  2
304500100223     O          E    24      DET         1
304600100223     O                       RagADD45            99
304700931229     O          E            DET         1
304800941221     O                       STADSP              16 '  /  /    '
304900130808     O***************        ARBCAM              36
305000130808     O***************        STALOM              53
305100130808     O                       STAINM              52
305200941222     O                       STAIND              79
305300961011     O                       ARBNAS             100
305400041109     O                    N34AR9VCA             112
305500130808     O          E    25N27   DET         1
305600100223     O                       IndADD10            64
305700130808     O          E    27N25   DET         1
305800130808     O                       IndMAdd10           37
305900130808     O          E    27 25   DET         1
306000130808     O                       IndMAdd10           37
306100130808     O                       IndADD10            64
306200941222     O          E            DET         1
306300940111     O                     02                    13 '* PARTIAL *'
306400130808     O***************        ARBRMA              30
306500130808     O***************                            37 'P.IVA:'
306600130808     O***************        MITCPI              53
306700130808     O                       ARBCAM              36
306800130808     O                       STALOM              53
306900950614     O                       ARBCAD              63
307000950614     O                       STALOD              84
307100950614     O                                           91 'P.IVA:'
307200950614     O                       DESCPI             107
307300100223     O          E    26      DET         1
307400100224     O                       CitADD15            69
307500130808     O          E            DET         1
307600130808     O                       ARBRMA              30
307700130808     O                                           37 'P.IVA:'
307800130808     O                       MITCPI              53
307900961220     O          E   N34      DET         1
308000041109     O                       STATIC             112
308100961121     O          E   N36      DET         1
308200950304     O         OR   N35
308300041109     O                    N35STANT1              75
308400041109     O                    N36STANT2             111
308500961121     O          E   N36      DET         0  1
308600961121     O         OR   N35
308700041109     O                    N35STANT1              75
308800041109     O                    N36STANT2             111
308900100223      * Contatto Nom./tel.
309000100223     O          E   N33      DET         1
309100100223     O                                           37 'Contact Name:'
309200100223     O                       STACNA              72
309300100223     O                                           80 'Tel.:'
309400100223     O                       STACTE             106
309500100223      *
309600941222     O* EXCPT FITTIZIA
309700941222     O          E          36DET         1  0
309800041109     O                                          112 ' '
309900021204     O* valore merce dichiarato
310000021204     O          E            VMDDET         1
310100021204     O                                           37 'Valore Merce:'
310200021204     O                       ARBVMD        2  +   1
310300021204     O                       ARBVAD           +   1
310400021204     O*
310500940111     O          E            TOTAL       1
310600940111     O                       ST(15)              66
310700041109     O                       D46(7)             112
310800940111     O          E            TOTAL       1
310900940111     O                       ST(17)              66
311000041109     O                       D46(8)             112
311100940111     O                       TOTSPE        Z     24
311200940111     O                       TOTNCL        Z     50
311300961125     O                       TOTPES        4     79
311400961125     O                       TOTVLF        4    104
311500940111     O          E            TOTAL       1
311600940111     O                       ST(15)              66
311700041109     O                       D46(7)             112
311800931229     O          E            FINE        1
311900931229     O                                           69 '***  END  OF  PRINT  ***'
312000961121     O*
312100970110     O*****************************************************************
312200970110     O* MANIFEST PER IL PARTNER IN FORMA SINTETICA
312300970110     O*****************************************************************
312400970110     OMANPTS    E            TESSIN           02
312500970110     O                       MAS(1)              66
312600970110     O                       MAS(2)             132
312700970110     O                       RSUT                20
312800970110     O                       SIMFEL              24
312900970110     O                       UDATE8             121 '  /  /    '
313000970110     O                       PAGE1         Z    132
313100970110     O          E            TESSIN           03
313200970110     O                       MAS(3)              66
313300970110     O                       MAS(4)             132
313400970110     O                       KNSIF               10
313500970110     O                       KNMUS               21
313600000208     O                       D82NFV              40
313700000208     O               22                          44 'BIS'
313800970110     O                       STADLS              65
313900970110     O                       STARGS             105
314000970114     O                       UTIME              119 '  :  :  '
314100970110     O          E            TESSIN         1 05
314200970110     O                       MAS(5)              66
314300970110     O                       MAS(7)             132
314400970110     O          E            DETSIN         1
314500970110     O                       ARBAAS               4
314600970110     O                       ARBLNP               8
314700970110     O                       ARBNRS        Z     12
314800970110     O                       ARBNSP        Z     20
314900970110     O                       ARBNAS              37
315000970110     O                       ARBRSM              74
315100020917     O                       ARBRMA              91
315200970110     O                       ARBRMN        Z    108
315300970110     O                       ARBNCL        Z    115
315400970110     O                       PESPRT        4    125
315500970110     O          E            TOTSIN         1
315600970110     O                       MAS(9)              66
315700970110     O                       MAS(9)             132
315800970110     O          E            TOTSIN         1
315900970110     O                       MAS(10)             66
316000970110     O                       MAS(11)            132
316100970110     O                       TOTSPE        Z     24
316200970110     O                       TOTNCL        Z     49
316300970110     O                       TOTPES        4     77
316400970110     O                       TOTVLF        4    103
316500970110     O          E            TOTSIN      1
316600970110     O                       MAS(9)              66
316700970110     O                       MAS(9)             132
316800970110     O          E            FINSIN      1
316900970110     O                                           69 '***   FINE  STAMPA   ***'
317000970110     O*
317100970110     O*
317200961212     O*****************************************************************
317300961212     O* MANIFEST SPEDIZIONI C.O.D.
317400961212     O*****************************************************************
317500961213     OMANCOD    E            TESCOD           02
317600041109     O                       D46(1)             112
317700961217     O                       STARAG              75
317800961217     O                       STPFAX              97
317900041109     O              N20                         112 '* PROVVISORIO'
318000041109     O               20                         112 '* DEFINITIVO '
318100961212     O          E            TESCOD           03
318200961121     O                       ST(1)               66
318300041109     O                       D46(2)             112
318400961217     O                       UDATE8              54 '  /  /    '
318500961217     O                       UTIME               89 '  :  :  '
318600041109     O                       PAGE2         Z    111
318700961212     O          E            TESCOD           04
318800961217     O                       ST(4)               66
318900041109     O                       D46(3)             112
319000961217     O                       RSUTM               63
319100961217     O                       STADLA             106
319200961212     O          E            TESCOD           05
319300961217     O                       ST(3)               66
319400041109     O                       D46(4)             112
319500961217     O                       UDATE8              54 '  /  /    '
319600041109     O                       D82CMR             112
319700961212     O          E            TESCOD           06
319800961217     O                       ST(19)              66
319900041109     O                       D46(5)             112
320000041109     O                       DATCMR              87 '  /  /    '
320100961216     O          E            TESCOD           07
320200041109     O                                          112 '* MANIFEST C.O.D.'
320300961212     O          E            TESCOD         1 08
320400961121     O                       ST(6)               66
320500041109     O                       D46(6)             112
320600961121     O          E            DETCOD      1
320700100330     O              N21      §LB14               16
320800961129     O               21      §LB8                16
320900961121     O                       STARMN        Z   +  1
321000961121     O                       STARSM            +  1
321100961121     O                       STARSD            +  2
321200961121     O                       STATBO            +  1
321300961121     O                       ARBNCL        Z   +  2
321400961125     O                       PESPRT        4   +  2
321500041109     O                    N34PRTCAS        4   +  2
321600961121     O          E            DETCOD      1
321700961121     O                       STADSP              16 '  /  /    '
321800130808     O**************         ARBCAM              36
321900130808     O**************         STALOM              53
322000130808     O                       STAINM              52
322100961121     O                       STAIND              79
322200961121     O                       ARBNAS             100
322300041109     O                    N34AR9VCA             112
322400961121     O          E            DETCOD      1
322500961121     O                     02                    13 '* PARTIAL *'
322600130808     O**************         ARBRMA              30
322700130808     O**************                             37 'P.IVA:'
322800130808     O**************         MITCPI              53
322900130808     O                       ARBCAM              36
323000130808     O                       STALOM              53
323100961121     O                       ARBCAD              63
323200961121     O                       STALOD              84
323300961121     O                                           91 'P.IVA:'
323400961121     O                       DESCPI             107
323500130808     O          E            DETCOD      1
323600130808     O                       ARBRMA              30
323700130808     O                                           37 'P.IVA:'
323800130808     O                       MITCPI              53
323900061127      * Contatto Nome/tel.
324000061127     O          E   N33      DETCOD      1
324100061127     O                                           37 'Contact Name:'
324200061127     O                       STACNA              72
324300061127     O                                           80 'Tel.:'
324400061127     O                       STACTE             106
324500061127     O
324600061127      *
324700961220     O          E   N34      DETCOD      1
324800041109     O                       STATIC             112
324900961121     O          E   N36      DETCOD      1
325000961121     O         OR   N35
325100041109     O                    N35STANT1              75
325200041109     O                    N36STANT2             111
325300961121     O          E   N36      DETCOD      0  1
325400961121     O         OR   N35
325500041109     O                    N35STANT1              75
325600041109     O                    N36STANT2             111
325700961121     O* EXCPT FITTIZIA
325800961121     O          E          36DETCOD      1  0
325900041109     O                                          112 ' '
326000961121     O          E            TOTCOD      1
326100961121     O                       ST(15)              66
326200041109     O                       D46(7)             112
326300961121     O          E            TOTCOD      1
326400961121     O                       ST(17)              66
326500041109     O                       D46(8)             112
326600961204     O                       CODSPE        Z     24
326700961204     O                       CODNCL        Z     50
326800961204     O                       CODPES        4     79
326900961204     O                       CODVLF        4    104
327000961121     O          E            TOTCOD      1
327100961121     O                       ST(15)              66
327200041109     O                       D46(7)             112
327300961121     O          E            FINCOD      1
327400961121     O                                           69 '***  END  OF  PRINT  ***'
327500961212     O*****************************************************************
327600961212     O* MANIFEST PER LA DOGANA
327700961212     O*****************************************************************
327800961213     OMANDGN    E            TESDGN           02
327900041109     O                       D46(1)             112
328000970117     O                       STARSX              75
328100970117     O                       D82DGX              97
328200041109     O*               N20               112 '* PROVVISORIO'
328300041109     O*                20               112 '* DEFINITIVO '
328400961212     O          E            TESDGN           03
328500961212     O                       ST(1)               66
328600041109     O                       D46(2)             112
328700961217     O                       UDATE8              54 '  /  /    '
328800961217     O                       UTIME               89 '  :  :  '
328900041109     O                       PAGE3         Z    111
329000961212     O          E            TESDGN           04
329100961217     O                       ST(4)               66
329200041109     O                       D46(3)             112
329300961217     O                       RSUTM               63
329400961217     O                       STADLA             106
329500961212     O          E            TESDGN           05
329600961217     O                       ST(3)               66
329700041109     O                       D46(4)             112
329800961217     O                       UDATE8              54 '  /  /    '
329900041109     O                       D82CMR             112
330000961212     O          E            TESDGN           06
330100961217     O                       ST(19)              66
330200041109     O                       D46(5)             112
330300041109     O                       DATCMR              87 '  /  /    '
330400961212     O          E            TESDGN         1 08
330500961212     O                       ST(6)               66
330600041109     O                       D46(6)             112
330700961212     O          E            DETDGN      1
330800100330     O              N21      §LB14               16
330900961212     O               21      §LB8                16
331000961212     O                       STARMN        Z   +  1
331100961212     O                       STARSM            +  1
331200961212     O                       STARSD            +  2
331300961212     O                       STATBO            +  1
331400961212     O                       ARBNCL        Z   +  2
331500961212     O                       PESPRT        4   +  2
331600041109     O                    N34PRTCAS        4   +  2
331700961212     O          E            DETDGN      1
331800961212     O                       STADSP              16 '  /  /    '
331900130808     O                       STAINM              52
332000130808     O*************          ARBCAM              36
332100130808     O*************          STALOM              53
332200130808     O                       STAINM              52
332300961212     O                       STAIND              79
332400961212     O                       ARBNAS             100
332500041109     O                    N34AR9VCA             112
332600961212     O          E            DETDGN      1
332700961218     O                     02                    13 '* PARTIAL *'
332800130808     O**************         ARBRMA              30
332900130808     O**************                             37 'P.IVA:'
333000130808     O**************         MITCPI              53
333100130808     O                       ARBCAM              36
333200130808     O                       STALOM              53
333300961212     O                       ARBCAD              63
333400961212     O                       STALOD              84
333500961212     O                                           91 'P.IVA:'
333600961212     O                       DESCPI             107
333700130808     O          E            DETDGN      1
333800130808     O                       ARBRMA              30
333900130808     O                                           37 'P.IVA:'
334000130808     O                       MITCPI              53
334100961220     O          E   N34      DETDGN      1
334200041109     O                       STATIC             112
334300961212     O          E   N36      DETDGN      1
334400961212     O         OR   N35
334500041109     O                    N35STANT1              75
334600041109     O                    N36STANT2             111
334700961212     O          E   N36      DETDGN      0  1
334800961212     O         OR   N35
334900041109     O                    N35STANT1              75
335000041109     O                    N36STANT2             111
335100961212     O* EXCPT FITTIZIA
335200961212     O          E          36DETDGN      1  0
335300041109     O                                          112 ' '
335400961212     O          E            TOTDGN      1
335500961212     O                       ST(15)              66
335600041109     O                       D46(7)             112
335700961212     O          E            TOTDGN      1
335800961212     O                       ST(17)              66
335900041109     O                       D46(8)             112
336000961212     O                       TOTSPE        Z     24
336100961212     O                       TOTNCL        Z     50
336200961212     O                       TOTPES        4     79
336300961212     O                       TOTVLF        4    104
336400961212     O          E            TOTDGN      1
336500961212     O                       ST(15)              66
336600041109     O                       D46(7)             112
336700961212     O          E            FINDGN      1
336800961212     O                                           69 '***  END  OF  PRINT  ***'
336900941222     O*
337000961212     O*****************************************************************
337100970114     O* LISTA DISCORDANZE VIA FAX
337200961212     O*****************************************************************
337300961213     OLISDIS    E            TESDIS           02
337400961217     O                       DIS(21)            132
337500961217     O                       STARAG              75
337600961217     O                       STPFAX              97
337700041109     O              N20                         112 '* PROVVISORIA'
337800041109     O               20                         112 '* DEFINITIVA '
337900961127     O          E            TESDIS           04
338000961125     O              N20      DIS(1)              66
338100961125     O              N20      DIS(2)             132
338200961125     O               20      DIS(17)             66
338300961125     O               20      DIS(18)            132
338400961125     O              N20      UDATE8              85 '  /  /    '
338500961129     O               20      UDATE8              67 '  /  /    '
338600961217     O                       UTIME              100 '  :  :  '
338700041109     O                       WPAG          Z    116
338800961127     O          E            TESDIS           06
338900961217     O                       DIS(3)              66
339000961217     O                       DIS(4)             132
339100961217     O                       D82NFV        Z     50
339200961217     O                       PARDFV              70 '  /  /    '
339300961217     O                       STADLA             115
339400961217     O          E            TESDIS           07
339500961217     O                       DIS(22)             66
339600961217     O                       DATCMR              55 '  /  /    '
339700961217     O                       D82CMR              91
339800961217     O          E            TESDIS           09
339900961122     O                       DIS(5)              66
340000961122     O                       DIS(8)             132
340100961217     O          E            TESDIS         1 10
340200961122     O                       DIS(6)              66
340300961122     O                       DIS(9)             132
340400961122     O          E            DETDIS         1
340500961127     O               16      KFLS                 3
340600961127     O               16      KLNA                 7
340700961129     O               16 17N18KNRS                10
340800961129     O               16 17N18                    11 '-'
340900961129     O               16   N18KNSC          Z     18
341000961129     O                  14   WNPS                22
341100961127     O               15      WPAAS               26
341200961127     O               15      WLNP                30
341300961127     O               15 17   KNRS                34
341400961127     O               15 17                       35 '-'
341500961127     O               15      WNSP          Z     42
341600961122     O                       KCEV                47
341700961122     O                                           48 '-'
341800970114     O                       DCEVE              108
341900961128     O                 N14   WFIL               112
342000961129     O                 N14   WNPS               115
342100961125     O          E            DETDI2         1
342200961202     O*                        COSPRT    68
342300961127     O                       WDTPA1              79 '  /  /    '
342400961129     O*
342500961129     O          E            RIGCNS         1
342600961129     O               12                           8 'PARCELS:'
342700961129     O               17      KNRS                10
342800961129     O               17                          11 '-'
342900961129     O                       NSC(1)              19
343000961129     O                       NSC(2)              27
343100961129     O                       NSC(3)              35
343200961129     O                       NSC(4)              43
343300961129     O                       NSC(5)              51
343400961129     O                       NSC(6)              59
343500961129     O                       NSC(7)              67
343600961129     O                       NSC(8)              75
343700961129     O                       NSC(9)              83
343800961129     O                       NSC(10)             91
343900961129     O                       NSC(11)             99
344000961129     O                       NSC(12)            107
344100961129     O                       NSC(13)            115
344200961127     O          E            FIT         1
344300961122     O*                                     ' '
344400961122     O          E            FINDIS      1
344500961127     O               20                          75 '***  END OF PRINT  ***'
344600961127     O              N20                          75 '***  FINE STAMPA  ***'
344700961217     O          E            DETDI3           19
344800961125     O              N20      DIS(15)             66
344900961125     O              N20      DIS(16)            132
345000961125     O               20      DIS(19)             66
345100970114     O*****************************************************************
345200970114     O* LISTA DISCORDANZE SU CARTA
345300970114     O*****************************************************************
345400970114     OLISDIC    E            TESDIC           02
345500970114     O                       DIS(21)            132
345600970114     O                       STARAG              75
345700970114     O                       STPFAX              97
345800041109     O              N20                         112 '* PROVVISORIA'
345900041109     O               20                         112 '* DEFINITIVA '
346000970114     O          E            TESDIC           04
346100970114     O                       DIS(1)              66
346200970114     O                       DIS(2)             132
346300970114     O                       UDATE8              85 '  /  /    '
346400970114     O                       UTIME              100 '  :  :  '
346500970114     O                       WPAG1         Z    116
346600970114     O          E            TESDIC           06
346700970114     O                       DIS(3)              66
346800970114     O                       DIS(4)             132
346900970114     O                       D82NFV        Z     50
347000970117     O               22                          54 'BIS'
347100970114     O                       PARDFV              70 '  /  /    '
347200970114     O                       STADLA             115
347300970114     O          E            TESDIC           07
347400970114     O                       DIS(22)             66
347500970114     O                       DATCMR              55 '  /  /    '
347600970114     O                       D82CMR              91
347700970114     O          E            TESDIC           09
347800970114     O                       DIS(5)              66
347900970114     O                       DIS(8)             132
348000970114     O          E            TESDIC         1 10
348100970114     O                       DIS(6)              66
348200970114     O                       DIS(9)             132
348300970114     O          E            DETDIC         1
348400970114     O               16      KFLS                 3
348500970114     O               16      KLNA                 7
348600970114     O               16 17N18KNRS                10
348700970114     O               16 17N18                    11 '-'
348800970114     O               16   N18KNSC          Z     18
348900970114     O                  14   WNPS                22
349000970114     O               15      WPAAS               26
349100970114     O               15      WLNP                30
349200970114     O               15 17   KNRS                34
349300970114     O               15 17                       35 '-'
349400970114     O               15      WNSP          Z     42
349500970114     O                       KCEV                47
349600970114     O                                           48 '-'
349700970114     O                       DCEV               108
349800970114     O                 N14   WFIL               112
349900970114     O                 N14   WNPS               115
350000970114     O          E            DETDC2         1
350100970114     O*                        COSPRT    68
350200970114     O                       WDTPA1              79 '  /  /    '
350300970114     O*
350400970114     O          E            RIGCNC         1
350500970114     O               12                           8 'PARCELS:'
350600970114     O               17      KNRS                10
350700970114     O               17                          11 '-'
350800970114     O                       NSC(1)              19
350900970114     O                       NSC(2)              27
351000970114     O                       NSC(3)              35
351100970114     O                       NSC(4)              43
351200970114     O                       NSC(5)              51
351300970114     O                       NSC(6)              59
351400970114     O                       NSC(7)              67
351500970114     O                       NSC(8)              75
351600970114     O                       NSC(9)              83
351700970114     O                       NSC(10)             91
351800970114     O                       NSC(11)             99
351900970114     O                       NSC(12)            107
352000970114     O                       NSC(13)            115
352100970114     O          E            FITC        1
352200970114     O*                                     ' '
352300970114     O          E            FINDIC      1
352400970114     O                                           75 '***  FINE STAMPA  ***'
352500970114     O          E            DETDC3           19
352600970114     O                       DIS(15)             66
352700970114     O                       DIS(16)            132
352800970110**
352900961217                           Date printed    :XX/XX/XXXX          Ti  1
353000961217me printed    :XX:XX:XX               Page:  xxxx                   2
353100961216                           Shipment date   :xx/xx/xxxx          CM  3
353200961216                           Exporting branch:1234567890123456789 Im  4
353300961216porting branch:1234567890123456789012345                            5
353400960223    Shipmentno    Rif.Exp  Exporter                   Consignee     6
353500960223    XXXXXXXXXXXX XXXXXXXXX xxxxxxxxxxxxxxxxxxxxxxxxx  XXXXXXXXXXXX  7
353600961011        XX/XX/XX           xxxxxxxxx xxxxxxxxxxxxxxxx xxxxxxxxxxxx  8
353700961011  * PARTIAL *  xxxxxxxxxxxxxxx P.IVA:xxxxxxxxxxxxxxxx xxxxxxxxx xx  9
353800961125              Imp Parcels   Weight    C.O.D.                       10
353900961011XXXXXXXXXXXXX xxx  xxxxx  xxxxxx,x  xxxxxxxxxx,xxx                  1
354000961011XXxxxxxxxxxxx      123456789012345             xxx                  2
354100961011XXxxxxxxxxxxxxxxxx P.IVA: xxxxxxxxxx                                3
354200940131
354300940111------------------------------------------------------------------  5
354400940111--------------------------------------------------                  6
354500961125  Total Shipments: xxxxx    Total Parcels: xxxxxxx    Total Weight  7
354600961125: xxxxxxxxx,x   Total Volume: xxxx,xxx                              8
354700961216                                                                CM 19
354800961216R Number      :12345678901234567890123456789012345                 20
354900961216R Date        :xx/xx/xxxx                                          21
355000931229**
355100961217                          ** LISTA DISCORDANZE SPEDIZIONI PER L'ES  1
355200961223TERO DEL XX/XX/XXXX **    XX:XX:XX       PAG. XXXX
355300961217                           FOGLIO DEFLUENZA :XXXXX      DEL XX/XX/  3
355400961217XXXX    LINEA DI ARRIVO:1234567890123456789012345                   4
355500961128  P  A  R  C  E  L  S    S H I P M E N T                            5
355600961122LHD LHA  NUMBER    O.R. YR LHD   NUMBER                             6
355700961121XXX XXX XX XXXXXXX  XX  XX XXX  XX XXXXXXX  XXX-123456789012345678  7
355800961128                                           SPUNTA                   8
355900961128                                           P.O O.R                  9
356000961128901234567890123456789012345678901234567890 XXX XX                  10
356100961127                                                123456789012345678 11
35620096112790 XX/XX/XXXX                                                      12
356300961129PARCELS:XX-1234567,1234567,1234567,1234567,1234567,1234567,1234567 13
356400961129,1234567,1234567,1234567,1234567,1234567,1234567                   14
356500970121                **  N E S S U N A   D I S C O R D A N Z A   R I S  15
356600970121C O N T R A T A  **                                                16
356700961217                                *** DISCREPANCY LIST DTD XX/XX/XXX 17
356800961223X ***                     XX:XX:XX       PAG. XXXX                 18
356900961212                             **  N O   D I S C R E P A N C Y  **   19
357000961217                           123456789012345678901234567890123456789 20
357100961219012345678 *Fax:1234567890123456                                    21
357200961217                           CMR..............:XX/XX/XXXX 1234567890 22
3573009612171234567890123456789012345                                          23
357400041109** D46
357500041109_________ *Fax:________________                                     1
357600041109me printed    :XX:XX:XX           Page:  xxxx                       2
357700041021porting branch:_________________________                            3
357800041109R Number  :___________________________________                      4
357900041109R Date    :xx/xx/xxxx                                               5
358000041021              Imp Parcels   Weight    C.O.D.                        6
358100041109----------------------------------------------                      7
358200041021: xxxxxxxxx,x   Total Volume: xxxx,xxx                              8
358300041021                                                                    9
358400041021                                                                   10
358500961213** CMD
358600961213OVRPRTF MANPTN QSYSPRT SAVE(*YES)                                    1
358700971230OVRPRTF MANPTN QSYSPRT OUTQ(FAXIN     ) SAVE(*YES)                   2
358800961213OVRPRTF MANCOD QSYSPRT SAVE(*YES)                                    3
358900971230OVRPRTF MANCOD QSYSPRT OUTQ(FAXIN     ) SAVE(*YES)                   4
359000961213OVRPRTF MANDGN QSYSPRT SAVE(*YES)                                    5
359100971230OVRPRTF MANDGN QSYSPRT OUTQ(FAXIN     ) SAVE(*YES)                   6
359200990512OVRPRTF LISDIC QSYSPRT SAVE(*YES)       HOLD(*YES)                   7
359300971230OVRPRTF LISDIS QSYSPRT OUTQ(FAXIN     ) SAVE(*YES)                   8
359400970110OVRPRTF MANPTS QSYSPRT SAVE(*YES)                                    9
359500971230OVRPRTF MANPTN QSYSPRT OUTQ(FAXOUT    ) SAVE(*YES) FORMTYPE('SAVEDI')10
359600971230                                                                     11
359700971230OVRPRTF LISDIS QSYSPRT SAVE(*YES)                                    12
359800970110**
359900970110XXXXXXXXXXXXXXXXXXXX/XXX                  ** MANIFEST SPEDIZIONI E   1
360000970110XPORT SINTETICO **               TRTC83R1    XX/XX/XXXX  PAG. XXXX   2
360100970121XXXXXXXXXX XXXXXXXXXX  Foglio Defl.XXXXX      Lna:123456789012345    3
360200970115 Partner:123456789012345678901234567890      XX:XX:XX                4
360300970110Anno Lnp Ser  Numero  Natura merce     Ragione sociale mittente      5
360400970110XXXX XXX  XX XXXXXXX  123456789012345  123456789012345678901234567   6
360500970110          Rif.mitt.alfab.  Rif.mitt.numer.  Colli    Peso            7
36060097011089012345  123456789012345  123456789012345  12345  123456,7          8
360700970110------------------------------------------------------------------   9
360800970110Totale Spedizioni: xxxxx    Totale Colli: xxxxxxx    Totale Peso:   10
360900970110xxxxxxxxx,x   Totale Volume: xxxx,xxx                               11
