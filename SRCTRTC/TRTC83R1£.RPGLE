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
137800990412     C* COLLO NON INVIATO MA ARRIVATO O CON ARRIVO FORZATO
137900970116    3C     ARTDAM        IFGT      *ZEROS
138000970116     C     ARTDAM        ANDLE     D82DFV
138100990412     C     WARFOR        OREQ      'S'
138200990412      *
138300970117    4C     *IN22         IFEQ      *OFF
138400970116     C* NO BIS: VERIFICO SE COLLO A TERRA
138500970116     C                   Z-ADD     645           KCAA
138600970116     C                   EXSR      CTRANM
138700970116   X4C                   ELSE
138800970116     C* SI BIS: VERIFICO SE IN PARTENZA
138900970117     C                   Z-ADD     1             KNPG
139000070123     C     KBRV1         SETLL     FNBRV01L                               31
139100020225     c   31              eval      *in09 = *off
139200020225     c  N31              eval      *in09 = *on
139300970116    4C                   END
139400970116     C* 09 OFF --> LA SPEDIZIONE E' DA STAMPARE NEL MANIFEST
139500970116    4C     *IN09         IFEQ      *OFF
139600060913     c
139700970116     C                   MOVE      'T'           ARBFT3
139800060913     c
139900970116   X4C                   ELSE
140000060918     c
140100060918     C* SE IL COLLO NON PARTE in IMA, ACCENDO INDICATORE DI PARTIAL
140200060918     c* In questo caso non devo creare l'anomalia E8 ma un persiste
140300060918     c                   if        creae8='S'
140400060918     c                   eval      creae7x='S'
140500060918     c                   endif
140600060918     c*
140700060918     c                   eval      creae8='N'
140800060918     c
140900020225     C                   eval        *in02 = *on
141000060918     C                   eval        *in01 = *on
141100970116    4C                   END
141200970116   X3C                   ELSE
141300970116     C* SE IL COLLO NON E' ARRIVATO ACCENDO INDICATORE DI PARTIAL
141400020225     C                   eval        *in02 = *on
141500970116    3C                   END
141600970116    2C                   END
141700020225      *
141800020225      * SE NON DEVO NE' STAMPARE NE' CREARE LE DISCORDANZE E AI FINI
141900020225      * DEL MANIFEST HO GIA' RILEVATO QUELLO CHE MI SERVIVA (FT3='T'
142000020225      * E 02 ON) ESCO DAL CICLO
142100970110     C     *IN20         IFEQ      *OFF
142200970110     C     D82LDS        ANDEQ     'NO'
142300970110     C     *IN02         ANDEQ     *ON
142400970110     C     ARBFT3        ANDEQ     'T'
142500970110     C                   SETON                                        31
142600970110     C                   ELSE
142700120823     C     KARb          READE     FNART01L                               31
142800970110     C                   END
142900020225      *
143000961119    1C                   ENDDO
143100961129     C*
143200961127     C* Stampa elenco segnacolli a fine ciclo
143300060918    1C     NSC(1)        IFNE      *BLANKS
143400060918     c                   exsr      STAGIAINV
143500060918     c                   endif
143600060918     c
143700961218     C* stampo riga vuota per distanziarla
143800961127     C* dalla prossima discordanza che verrà eventualmente stampata
143900060918    1C     WDIS4         IFEQ      '1'
144000970114     C     D82LDS        ANDEQ     'SI'
144100120821    2c     d82ftt        ifne      FEDEX
144200040423
144300060918    3c                   if        FAX_Disc_Est = 'S'
144400970115     C   20              EXCEPT    FIT
144500060918    3c                   end
144600040423
144700060918    2c                   endif
144800970114     C                   EXCEPT    FITC
144900060918    1C                   END
145000961114     C*
145100961114     C                   ENDSR
145200060918     *** *****************************************************************
145300060918     *** * stampa riga segnacolli già inviati
145400060918     *** *****************************************************************
145500060918     c     STAGIAINV     BEGSR
145600060918     C* Stampo riga con elenco segnacolli
145700060918    8C   20*INOB         IFEQ      *ON
145800120821     c     d82ftt        andne     FEDEX
145900060918
146000060918     c                   if        FAX_Disc_Est = 'S'
146100060918     C                   ADD       1             WPAG
146200060918     C                   EXCEPT    TESDIS
146300060918     c                   end
146400060918
146500060918    8C                   END
146600060918     c
146700060918    8C     *INOE         IFEQ      *ON
146800060918     C                   ADD       1             WPAG1
146900060918     C                   EXCEPT    TESDIC
147000060918    8C                   END
147100060918     c
147200120821    8c     d82ftt        ifne      FEDEX
147300060918    9c                   if        FAX_Disc_Est = 'S'
147400060918     C   20              EXCEPT    RIGCNS
147500060918    9c                   end
147600060918    8c                   endif
147700060918     c
147800060918     C                   EXCEPT    RIGCNC
147900060918     c
148000060918     c                   ENDSR
148100961213     C*****************************************************************
148200961213     C* STAMPA MANIFEST ----------------------------------------------*
148300961213     C*****************************************************************
148400961213     C     PRTMAN        BEGSR
148500961213     C*
148600961213     C                   CLEAR                   MITCPI
148700961213     C                   CLEAR                   DESCPI
148800961220     C                   CLEAR                   STATIC
148900061127     C                   CLEAR                   STACNA           35
149000061127     C                   CLEAR                   STACTE           25
149100961213     C*
149200961213     C* ACCORCIO LA LUNGHEZZA DI ALCUNI CAMPI
149300961213     C                   Z-ADD     ARBRMN        STARMN            9 0
149400961213     C                   MOVEL     ARBRSM        STARSM           25
149500130808     C                   MOVEL     ARBINM        STAINM           25
149600961213     C                   MOVEL     ARBRSD        STARSD           25
149700961213     C                   MOVEL     ARBLOM        STALOM           20
149800961213     C                   MOVEL     ARBIND        STAIND           25
149900961213     C                   MOVEL     ARBLOD        STALOD           20
150000961213     C*
150100970110     C* SE DEVO STAMPARE SOLO IL MANIFEST SINTETICO NON OCCORRE FARE
150200970110     C* LE SEGUENTI OPERAZIONI
150300970110     C     *IN20         IFEQ      *ON
150400970110     C     D82DGN        OREQ      'SI'
150500120821     c     d82ftt        andne     FEDEX
150600970110     C     D82COD        OREQ      'SI'
150700970110     C     §3AFCA        ANDGT     0
150800120821     c     d82ftt        andne     FEDEX
150900061127      *
151000061127      * Prendo i dati del Contatto Nome e Telefono
151100061127     c     KAR5          chain     fiar501l                           33
151200061127     c                   if        %Found(fiar501l)
151300061127     c                   Movel     Ar5Uni        DAr5Gen
151400061127     c                   movel     §ar5ref       STACNA
151500061127     c                   movel     §ar5teld      STACTE
151600061127      * se i campi sono vuoti non deve stamparli..... Accende quindi il 33
151700061127      *   come se non avesse trovato il record.
151800061127     c                   if        STACNA=*blank and STACTE=*blank
151900061127     c                   seton                                        33
152000061127     c                   end
152100061127     c                   end
152200130215      ***
152300970110     C* Prendo i dati del c/assegno
152400120823     C     KARb          CHAIN     FIAR901L                           34
152500130215      ***
152600961220     C* Reperisco decodifica tipo incasso contrassegno
152700961220     C     *IN34         IFEQ      *OFF
152800041109     c                   z-add     ar9cas        prtcas            9 3
152900130215      *
153000961220     C                   MOVEL     '1A'          KECOD
153100961220     C                   MOVEL(P)  AR9TIC        KEKEY
153200130215      ***
153300130215     c                   z-add     1             tentativi         3 0
153400130215      * prima ci prova se c'è un suffisso particolare
153500130215      *   quindi preimposta anche la possibilità di cercare 2 volte
153600130215     c                   if        §pzSUFFtab <> *blank
153700130215     C                   MOVE      §pzSUFFtab    KEKEY
153800130215     c                   z-add     2             tentativi
153900130215     c                   end
154000130215      *
154100130215     C                   CLEAR                   edids1a
154200130215      *** può fare max 2 tentativi:
154300130215     c                   do        tentativi
154400130215     C     KETAB         CHAIN     EDTAB01L
154500130215     c                   if        %Found(EDTAB01L)
154600130215     C                   MOVEL     TABUNI        edids1a
154700961220     C                   MOVEL     §1ADES        STATIC
154800130215      * se ha trovato esce dopo aver impostato il campo per la stampa
154900130215     c                   LEAVE
155000130215     c                   else
155100130215      * serve x il secondo giro senza suffisso
155200130215     C                   MOVE      *blank        Due_Blanks        2
155300130215     C                   MOVE      Due_Blanks    KEKEY
155400130215     c                   end
155500130215      ***
155600130215     c                   EndDO
155700130215      ***
155800961220     C                   END
155900970110     C* Prendo le note
156000961213     C                   MOVEL     '8'           KTRC                           *NOTE1
156100060213     C     KAR4          CHAIN     fiar401L                           35
156200961213     C  N35              MOVEL     AR4NOT        STANT1
156300961213     C                   MOVEL     '9'           KTRC                           *NOTE2
156400060213     C     KAR4          CHAIN     fiar401L                           36
156500961213     C  N36              MOVEL     AR4NOT        STANT2
156600961213     C*
156700961213     C                   MOVEL     'P'           KTRC                           *P.IVA
156800060213     C     KAR4          CHAIN     fiar401L                           37
156900961213     C  N37              MOVEL     AR4NOT        MITCPI           16
157000961213     C  N37              MOVE      AR4NOT        DESCPI           16
157100130808
157200130808      * se non c'è partita IVA del mittente cerco il codice fiscale del mittente
157300130808     c                   IF        MITCPI = *blanks
157400130808     c                   eval      ktrc = 'Q'
157500130808     c     kar4          chain     fiar401l                           37
157600130808     c  N37              movel     AR4NOT        MITCPI
157700130808     c                   ENDIF
157800100223     C*
157900100223     C*  ragione sociale aggiuntiva di altri 35 + i primi 10 con il finale della
158000100223     C*  ragione sociale destinatario.
158100100223     C                   clear                   RagAdd45         45
158200100223     c                   eval      RagAdd45 = %subst(arbRSD:26:10)
158300100223      *  in più se c'è anche la parte addizionale
158400120821     C                   MOVEL     'D'           KTRC                           *Ragione Sociale
158500100223     C     KAR4          CHAIN     fiar401L                           23         aggiuntiva
158600100223     C  N23              MOVE      AR4NOT        RagAdd45
158700100223     C*
158800100223     C                   clear                   IndAdd10         10
158900100223     c                   eval      IndAdd10 = %subst(arbIND:26:10)
159000100223     C*
159100100224     C                   clear                   CitAdd15         15
159200100224     c                   eval      CitAdd15 = %subst(arbLOD:21:15)
159300130808      * Altri 10 caratteri per l'indirizzo del mittente
159400130808     C                   clear                   IndMAdd10        10
159500130808     c                   eval      IndMAdd10 = %subst(arbINM:26:10)
159600100223     C*
159700100223     C*  stampa righe aggiuntive
159800100223     c                   setoff                                       242526
159900130808     c                   eval      *in27 = *off
160000100223     c                   if        RagAdd45 <> *blank
160100100223     c                   setoN                                        24
160200100223     c                   end
160300961213     C*
160400100223     c                   if        IndAdd10 <> *blank
160500100223     c                   setoN                                        25
160600100223     c                   end
160700100223     C*
160800100224     c                   if        CitAdd15 <> *blank
160900100223     c                   setoN                                        26
161000100223     c                   end
161100130808      *
161200130808     c                   IF        IndMAdd10 <> *blank
161300130808     c                   eval      *in27 = *on
161400130808     c                   ENDIF
161500100223     C*
161600970110     C* data spedizione
161700961213     C                   Z-ADD     ARBDSP        DATA
161800961213     C                   Z-ADD     MM            GGMM
161900961213     C                   MOVEL     GG            GGMM
162000961213     C                   Z-ADD     AA            STADSP
162100961213     C                   MOVEL     GGMM          STADSP
162200970110     C* numero spedizione compattato e non
162300961213     C                   MOVE      ARBLNP        §LB2              2 0
162400961213     C                   MOVE      ARBNSP        §LB8              8 0
162500961213     C                   MOVEL     §LB2          §LB8
162600961213     C                   MOVEL     ARBNRS        §LB9
162700961213     C                   MOVE      ARBNSP        §LB9              9 0
162800961213     C                   MOVEL     ARBLNP        §LB12            12 0
162900961213     C                   MOVE      §LB9          §LB12            12 0
163000100330     C*
163100100330     C                   MOVE      ARBAAS        §LB2
163200100330     C                   MOVEL     §LB2          §LB5              5
163300100330     C                   MOVE      ARBLNP        §LB5
163400100330     C                   MOVE      §LB9          §LB14            14 0
163500100330     C                   MOVEL     §LB5          §LB14
163600970110     C*
163700970110     C                   END
163800961213     C*
163900961213     C* STAMPA MANIFEST PER PARTNER
164000970110     C   20
164100970110     CAN OF
164200970110     CORN20
164300970110     CAN OCD82STB        IFEQ      'SI'
164400961213     C* 04 on - stampa qualcosa nel manifest
164500961213     C                   SETON                                        04
164600041109     c                   if        Email_PTN = *blank or
164700041109     c                             PRT_PTN_tes = *blank
164800041109     c                   eval      PRT_PTN_tes = 'S'
164900970110     C   20              EXCEPT    TESTA
165000041109     c                   end
165100970110     C  N20              EXCEPT    TESSIN
165200970110     C                   SETOFF                                       OFOC
165300970110     C                   ENDIF
165400961213     C*
165500961213     C* STAMPA MANIFEST PER DOGANA
165600961213     C     D82DGN        IFEQ      'SI'
165700120821     c     d82ftt        andne     FEDEX
165800961213     C     *INOD         ANDEQ     *ON
165900961213     C* 06 on - stampa qualcosa nel manifest
166000961213     C                   SETON                                        06
166100041109     c                   if        Email_DGN = *blank or
166200041109     c                             PRT_DGN_tes = *blank
166300041109     c                   eval      PRT_DGN_tes = 'S'
166400961213     C                   EXCEPT    TESDGN
166500041109     c                   end
166600961213     C                   SETOFF                                       OD
166700961213     C                   ENDIF
166800961213     C*
166900961213     C     D82STB        IFEQ      'SI'
167000970110     C   20              EXCEPT    DET
167100970110     C  N20              EXCEPT    DETSIN
167200021125      * Stampa il Valore Merce Dichiarato solo su Manifest Sintetico Provvisorio
167300021204     c   20              if        arbvmd <> 0
167400021204     C                   except    VMDdet
167500021125     c                   end
167600961213     C                   END
167700961213     C*
167800961213     C     D82DGN        IFEQ      'SI'
167900120821     c     d82ftt        andne     FEDEX
168000961213     C                   EXCEPT    DETDGN
168100961213     C                   END
168200961213     C*
168300961213     C* STAMPA MANIFEST DEI C.O.D.
168400961213     C     D82COD        IFEQ      'SI'
168500961213     C     §3AFCA        ANDGT     0
168600120821     c     d82ftt        andne     FEDEX
168700961213     C*
168800961213     C     *INOG         IFEQ      *ON
168900961213     C* 05 On - Stampa qualcosa nel manifest
169000961213     C                   SETON                                        05
169100041109     c                   if        Email_COD = *blank or
169200041109     c                             PRT_COD_tes = *blank
169300041109     c                   eval      PRT_COD_tes = 'S'
169400961213     C                   EXCEPT    TESCOD
169500041109     c                   end
169600961213     C                   SETOFF                                       OG
169700961213     C                   ENDIF
169800961213     C*
169900961213     C                   ADD       1             CODSPE            5 0
170000961213     C                   ADD       ARBNCL        CODNCL            7 0
170100961213     C                   ADD       PESPRT        CODPES           10 1
170200030318     C*
170300030318     C*  questa subroutine non dovrebbe essere mai chiamata per Fedex
170400030318     C*   ma è stato mantenuto il test sul volume per rispettare le logiche
170500030318     C*     applicate alla Fedex.......non si sa mai ......
170600150825     c*********          if        d82ftt = FEDEX
170700150825     C                   ADD       wvol          CODVLF            7 3
170800150825     c*********          else
170900150825     C*********          ADD       ARBVLF        CODVLF            7 3
171000150825     c*********          end
171100961213     C                   EXCEPT    DETCOD
171200961213     C                   END
171300961213     C*
171400961213     C                   ENDSR
171500961119     C*****************************************************************
171600060913     C* CREAZIONE EVENTO E4 o E8 + STAMPA
171700961119     C*****************************************************************
171800060913     C     GEDIS48       BEGSR
171900961127     C*
172000060913     C******             MOVE      'E4 '         KCEV
172100970117     C****
172200970117     C* STAMPO LA DISCORDANZA
172300970117     C****
172400970117     C     D82LDS        IFEQ      'SI'
172500970117     C                   MOVE      ARBAAS        WAAS
172600970117     C                   MOVE      ARBLNP        WLNP
172700970117     C                   MOVE      ARBNSP        WNSP
172800970117     C                   EXSR      PRTDIS
172900970117     C                   END
173000961120     C*
173100961120     C****
173200961120     C* SE ELABORAZIONE DEFINITIVA CREO EVENTO
173300961120     C****
173400961205    1C     *IN20         IFEQ      *ON
173500961120     C                   CLEAR                   FNEVS1
173600961120     C*
173700961120     C                   MOVE      ARBAAS        EVSAAS
173800961120     C                   MOVE      ARBLNP        EVSLNP
173900961120     C                   MOVE      ARBNRS        EVSNRS
174000961120     C                   MOVE      ARBNSP        EVSNSP
174100961120     C                   MOVE      ARBLNA        EVSLNA
174200961120     C*
174300961120     C                   CLEAR                   EVSFLS
174400961120     C                   CLEAR                   EVSNSC
174500961120     C*
174600961212     C                   MOVE      D82DFV        EVSDEV
174700961120     C                   MOVE      KCEV          EVSCEV
174800961120     C*
174900961120     C                   WRITE     FNEVS1
175000961120    1C                   END
175100961119     C*
175200961119     C                   ENDSR
175300961113     C*****************************************************************
175400961113     C* VERIFICA DISCORDANZE
175500961113     C*****************************************************************
175600961113     C     CTRDIS        BEGSR
175700961115     C*
175800961115     C                   CLEAR                   WDTPAR
175900961115     C                   CLEAR                   WFIL
176000961121     C*----------------------------------------------------------------
176100961120     C* CONTROLLO DISCORDANZE 2/2A - COLLO INOLTRATO AL PARTNER SENZA
176200961120     C*                              RECORD BOLLA
176300961121     C*----------------------------------------------------------------
176400020301     c* Per Fedex il giro sui manca record non deve essere fatto
176500120821     c     D82FTT        ifne      FEDEX
176600020301     c*
176700961115     C                   SETOFF                                       43
176800961121     C* LEGGO ANOMALIE 10-MANCA RECORD BOLLA ARR., APERTE, PER LNA
176900961113     C                   Z-ADD     10            KCAA
177000990519     C     KANM10        SETLL     FNANM10L
177100990519     C     KANM10        READE(N)  FNANM10L                               31
177200961114    1C     *IN31         DOWEQ     *OFF
177300970121     C* CONSIDERO SOLO LE APERTE OPPURE LE CHIUSE CON CCH = 'PR'
177400970121    2C     ANMDAO        IFLE      D82DFV
177500970121     C     ANMDCH        ANDEQ     *ZEROS
177600970121     C     ANMDAO        ORLE      D82DFV
177700970121     C     ANMDCH        ANDGT     *ZEROS
177800970121     C     ANMCCH        ANDEQ     'PR'
177900961203     C* ESCLUDO LOCALI
178000970117    3C     ANMLNP        IFGT      *ZEROS
178100961203     C     ANMLNP        CABEQ     ANMLNA        REDA10
178200970117   X3C                   ELSE
178300961203     C     ANMFLS        CABEQ     ANMLNA        REDA10
178400970117    3C                   END
178500970117     C* VERIFICO SE COLLO E' IN PARTENZA
178600970117     C* Se fv no bis verifico se collo a terra
178700961120     C                   MOVE      ANMFLS        KFLS
178800961120     C                   MOVE      ANMLNA        KLNA
178900961120     C                   MOVE      ANMNRS        KNRS
179000961120     C                   MOVE      ANMSCN        KNSC
179100970117    3C     *IN22         IFEQ      *OFF
179200961122     C                   Z-ADD     645           KCAA
179300961122     C                   EXSR      CTRANM
179400970117   X3C                   ELSE
179500970117     C* Se fv bis verifico se sparato in defluenza su questo fv
179600970117     C                   EXSR      CHKBRV
179700970117    3C                   END
179800970117     C* 09 OFF --> COLLO IN PARTENZA
179900961120    3C     *IN09         IFEQ      *OFF
180000961120     C* VERIFICO ESISTENZA DISCORDANZA 2-COLLO INOLTRATO NEL GIORNO
180100961120     C*                                  SENZA BOLLA
180200961113     C                   MOVE      'E2 '         KCEV
180300961121     C     KEVS2         SETGT     FNEVS02L
180400961127     C     KEVS2         READPE    FNEVS02L                               31
180500970117    4C     *IN31         IFEQ      *ON
180600961121     C* CREO E STAMPO DISCORDANZA 2
180700060913     C                   EXSR      GEDIS27
180800970117   X4C                   ELSE
180900970117   4AC     EVSDEV        IFLT      D82DFV
181000970117     C     *IN22         ANDEQ     *OFF
181100961121     C* ESISTE EVENTO 2 CREATO IN UN GIORNO PRECEDENTE --> MEMORIZZO LA
181200961129     C* DATA E LA PISTOLA
181300961120     C                   MOVE      EVSDEV        WDEV
181400040415     C                   MOVE      EVSDEV        WDEV2a
181500961129     C                   MOVE      EVSNPS        WNPS
181600961121     C* CERCO ANCHE SE ESISTE EVENTO 2A
181700961120     C                   MOVE      'E2A'         KCEV
181800970120     C* SE TROVO E2A IN DATA DEL FOGLIO NON FACCIO NIENTE
181900120823     C     KEVS2A        SETLL     FNEVS02L                               31
182000970120     C     *IN31         CABEQ     *ON           REDA10
182100961120     C     KEVS2         SETGT     FNEVS02L
182200961127     C     KEVS2         READPE    FNEVS02L                               31
182300961121    5C     *IN31         IFEQ      *OFF
182400970117     C* TENGO LA DATA EVENTO MAGGIORE FRA EVENTO E2 ED EVENTO E2A
182500970117    6C     EVSDEV        IFGT      WDEV
182600961120     C                   MOVE      EVSDEV        WDEV
182700970117    6C                   END
182800970117    5C                   END
182900961121     C* CERCO SE ESISTE EVENTO 6-COLLO DATO PER INOLTRATO MA A TERRA
183000961121     C* IN DATA > WDEV
183100961121     C                   MOVE      'E6 '         KCEV
183200961121     C     KEVS2B        SETGT     FNEVS02L
183300961127     C     KEVS2         READE     FNEVS02L                               31
183400961121    5C     *IN31         IFEQ      *OFF
183500970120     C     EVSDEV        ANDLT     D82DFV
183600961121     C                   MOVE      'E2 '         KCEV
183700060913     C                   EXSR      GEDIS27
183800961121   X5C                   ELSE
183900961121     C* CREO EVENTO E2A SOLO SE IL COLLO NON E' IN PARTENZA OGGI
184000961121     C                   Z-ADD     620           KCAA
184100990519     C     KANM2         SETLL     FNANM02L                               31
184200970122     C  N31              MOVE      'E6B'         KCEV
184300120823     C  N31KEVS2A        SETLL     FNEVS02L                               31
184400961121    6C     *IN31         IFEQ      *OFF
184500040419     c* Memorizzo, se esiste, data discordanza E6B se > data E2 più recente
184600040419     c     kevs2         readpe    fnevs02l                               31
184700040419     c     *in31         ifeq      *off
184800040419     c     evsdev        andgt     wdev2a
184900040419     c                   eval      wdev2a=evsdev
185000040419     c                   endif
185100040415     c* creo la discordanza solo se non sono passati più di x giorni
185200040415     c* dalla data dell'E2/E6B più recente
185300040416     c     *iso          Movel     wdev2a        wdta
185400040416     c     *iso          Movel     d82dfv        wdta1
185500040416     c                   Adddur    §7agde:*D     wdta
185600040416     c     wdta          ifge      wdta1
185700961121     C                   MOVE      'E2A'         KCEV
185800060913     C                   EXSR      GEDIS27
185900040416     c                   endif
186000961121    6C                   END
186100961121    5C                   END
186200970117   4AC                   END
186300970117    4C                   END
186400961120    3C                   END
186500961120    2C                   END
186600961129     C     REDA10        TAG
186700961128     C                   Z-ADD     10            KCAA
186800990519     C     KANM10        READE(N)  FNANM10L                               31
186900961120    1C                   ENDDO
187000020301     c*
187100020301     c                   endif
187200961120     C*
187300961121     C*----------------------------------------------------------------
187400961114     C* LEGGO COLLI DELLE BOLLE CON DTR = 0 E:
187500961114     C* - AGGIORNO DTR SE COLLO ARRIVATO E NON E' A TERRA
187600961114     C* - RILEVO DISCORDANZE 1, 1A, 3, 5, 5A
187700961121     C*----------------------------------------------------------------
187800961115     C                   SETOFF                                       42
187900961114     C                   MOVE      *ZEROS        KDTR
188000961114     C     KART55        SETLL     FNART55L
188100961114     C   20KART55        READE     FNART55L                               31
188200961114     C  N20KART55        READE(N)  FNART55L                               31
188300961114    1C     *IN31         DOWEQ     *OFF
188400970627     C* ESCLUDO COLLI LOCALI
188500961203     C     ARTLNP        IFEQ      ARTLNA
188600961203     C   20              UNLOCK    FNART55L
188700961203     C                   GOTO      LEG55
188800961203     C                   END
188900961114     C*
189000961115     C                   CLEAR                   WFIL
189100961115     C                   CLEAR                   WCODIS
189200961120     C                   CLEAR                   WNPS
189300961120     C                   CLEAR                   WFNPS
189400961114     C*
189500961114     C* A CAMBIO SPEDIZIONE CHAINO FNARB
189600961114     C                   MOVEL     ARTAAS        AAS
189700961114     C                   MOVEL     ARTLNP        LNP
189800961114     C                   MOVEL     ARTNRS        NRS
189900961114     C                   MOVEL     ARTNSP        NSP
190000961114    2C     ARTSPE        IFNE      SAVSPE
190100970124     C     KARB1         CHAIN(N)  FNARB01L                           31
190200961114     C                   MOVE      ARTSPE        SAVSPE
190300961114    2C                   END
190400990412      * SE HO  COLLO ARRIVATO MA NON SPARATO FORZO L'ARRIVO   Solo se no bis
190500990412     C                   CLEAR                   WARFOR
190600961114     C*
190700961114    2C                   SELECT
190800990412     C* C O L L O   N O N   A R R I V A T O  e non forzato l'arrivo
190900970116     C* O ARRIVATO IN DATA > ALLA DATA DEL FOGLIO DI DEFLUENZA (SOLO SE
191000970116     C* NO BIS)
191100961114     C     ARTDAM        WHENEQ    *ZEROS
191200990412     C     WARFOR        ANDEQ     *BLANKS
191300970117     C     *IN22         ANDEQ     *OFF
191400961224     C     ARTDAM        ORGT      D82DFV
191500970117     C     *IN22         ANDEQ     *OFF
191600961121     C   20              UNLOCK    FNART55L
191700961114     C* - RELATIVO A SPEDIZIONE DEL MANIFEST DEL GIORNO --> DISC. 1A
191800961212    3C     ARBDT3        IFEQ      D82DFV
191900961114     C     ARBDT3        OREQ      *ZEROS
192000961114     C     ARBFT3        ANDEQ     'T'
192100961115     C                   MOVE      'E1A'         WCODIS
192200961114   X3C                   ELSE
192300961114     C* - RELATIVO A SPEDIZIONE DI MANIFEST PRECEDENTE ---> DISC. 5A
192400961114    4C     ARBDT3        IFGT      *ZEROS
192500961212     C     ARBDT3        ANDLT     D82DFV
192600040416     c* creo la discordanza solo se non sono passati più di x giorni
192700040416     c* dalla data del'invio del record bolla
192800040416     c     *iso          Movel     arbdt3        wdta
192900040416     c     *iso          Movel     d82dfv        wdta1
193000040416     c                   Adddur    §7agde:*D     wdta
193100040416     c     wdta          ifge      wdta1
193200961115     C                   MOVE      'E5A'         WCODIS
193300040416     c                   endif
193400961114    4C                   END
193500961114    3C                   END
193600961120     C*
193700961115    3C     WCODIS        IFNE      *BLANKS
193800961127     C                   MOVE      WCODIS        KCEV
193900961127     C                   MOVE      ARTFLS        KFLS
194000961127     C                   MOVE      ARTLNA        KLNA
194100961127     C                   MOVE      ARTNRS        KNRS
194200961127     C                   MOVE      ARTNSC        KNSC
194300961127     C                   MOVE      ARTAAS        WAAS
194400961127     C                   MOVE      ARTLNP        WLNP
194500961127     C                   MOVE      ARTNSP        WNSP
194600970117     C* STAMPO DISCORDANZA
194700970117     C     D82LDS        IFEQ      'SI'
194800970117     C                   EXSR      PRTDIS
194900970117     C                   END
195000961114     C* SE ELABORAZIONE DEFINITIVA CREO L'EVENTO
195100961127     C   20              EXSR      CRTEVS
195200961114    3C                   END
195300961114     C*
195400961114     C* C O L L O    A R R I V A T O
195500990412     C* o forzato l'arrivo (solo se no BIS)
195600961212     C     ARTDAM        WHENLE    D82DFV
195700990412     C     WARFOR        OREQ      'S'
195800961115     C                   MOVE      ARTFLS        KFLS
195900961115     C                   MOVE      ARTLNA        KLNA
196000961115     C                   MOVE      ARTNRS        KNRS
196100961115     C                   MOVE      ARTNSC        KNSC
196200970116     C*
196300970117   3AC     *IN22         IFEQ      *OFF
196400961122     C                   Z-ADD     645           KCAA
196500961115     C                   EXSR      CTRANM
196600961115     C* COLLO A TERRA
196700961115    3C     *IN09         IFEQ      *ON
196800961120     C* MEMORIZZO NUMERO PISTOLA
196900961120     C                   MOVE      ANMNPS        WNPS
197000961114     C* - DISCORDANZA 1
197100961212    4C     ARBDT3        IFEQ      D82DFV
197200961114     C     ARBDT3        OREQ      *ZEROS
197300961114     C     ARBFT3        ANDEQ     'T'
197400961114     C                   MOVE      'E1 '         WCODIS
197500961115     C                   MOVE      ANMFLE        WFIL
197600961114   X4C                   ELSE
197700961114     C* - DISCORDANZA 5
197800961114    5C     ARBDT3        IFGT      *ZEROS
197900961212     C     ARBDT3        ANDLT     D82DFV
198000961114     C                   MOVE      'E5 '         WCODIS
198100961115     C                   MOVE      ANMFLE        WFIL
198200961114    5C                   END
198300961114    4C                   END
198400970116    3C                   END
198500970116  X3AC                   ELSE
198600970116     C* SE BIS VERIFICO SE IN PARTENZA
198700970116     C                   EXSR      CHKBRV
198800970116   3AC                   END
198900970116     C*
199000970116    3C     *IN09         IFEQ      *OFF
199100961122    4C     ARBDT3        IFGT      *ZEROS
199200961212     C     ARBDT3        ANDLT     D82DFV
199300970627     C     ARTDCM        ANDEQ     *ZEROS
199400970116     C* E3 - RECORD GIA' INVIATO:INVIO COLLO
199500961114     C                   MOVE      'E3 '         WCODIS
199600970117    4C                   END
199700961224     C* DETERMINO NUMERO PISTOLA E SE ESISTE SPUNTA DI DEFLUENZA
199800970116     C* (SOLO SE NO BIS)
199900970117     C  N22              EXSR      CHKBRV
200000961122    3C                   END
200100060913     C*
200200060913     c*
200300060913     C* COLLO ARRIVATO DA INVIARE MA NON INVIATO record bolla perchè
200400060913     c*  invio parziale
200500060913     c                   if        not *in09 and arbft3=' ' and d82isc='S'
200600060915     c                             and   artdcm=0
200700060913     c* Se sto inviando collo --> disc. E7 altrimenti avviso
200800060913     c* che c'è ancora una parziale (E7A)
200900060913     c                   exsr      CTRE7x
201000060913     c                   endif
201100060913     c
201200060913     c
201300961114    3C     WCODIS        IFNE      *BLANKS
201400961127     C                   MOVE      WCODIS        KCEV
201500961127     C                   MOVE      ARTAAS        WAAS
201600961127     C                   MOVE      ARTLNP        WLNP
201700961127     C                   MOVE      ARTNSP        WNSP
201800970117     C* STAMPO DISCORDANZA
201900970117     C     D82LDS        IFEQ      'SI'
202000970117     C                   EXSR      PRTDIS
202100970117     C                   END
202200961114     C* SE ELABORAZIONE DEFINITIVA CREO EVENTO
202300961127     C   20              EXSR      CRTEVS
202400961114    3C                   END
202500970117     C* SE COLLO IN PARTENZA AGGIORNO ARTDTR
202600970117    3C     *IN09         IFEQ      *OFF
202700961205     C     *IN20         ANDEQ     *ON
202800970627     C     ARTDCM        ANDEQ     *ZEROS
202900120821     c     d82ftt        andne     FEDEX
203000020304     c* per fedex lo deve fare solo se sono arrivati tutti i colli
203100020304    3C     *IN09         oreq      *OFF
203200020304     C     *IN20         ANDEQ     *ON
203300020304     C     ARTDCM        ANDEQ     *ZEROS
203400020304     c     arbft3        andeq     'T'
203500961120     C* RICHIAMO ROUTINE PER AUTOGENERARE SPUNTA PARTENZA SE NON C'E'
203600961115     C* E CHIUDERE EVENTUALI ANOMALIE 145 E 146 DEI GIORNI PRECEDENTI
203700961115     C                   SETON                                        43
203800961115     C                   EXSR      PARCOL
203900961212     C                   MOVE      D82DFV        ARTDTR
204000961114     C                   UPDATE    FNART055
204100961114   X3C                   ELSE
204200961114     C   20              UNLOCK    FNART55L
204300961114    3C                   END
204400961114     C*
204500961114    2C                   ENDSL
204600961203     C*
204700961203     C     LEG55         TAG
204800961203     C*
204900961114     C   20KART55        READE     FNART55L                               31
205000961114     C  N20KART55        READE(N)  FNART55L                               31
205100961114    1C                   ENDDO
205200961121     C*----------------------------------------------------------------
205300961120     C* CONTROLLO DISCORDANZE 6A - COLLO DATO PER INOLTR.MA IN PARTENZA
205400961121     C*----------------------------------------------------------------
205500961115     C                   CLEAR                   WFIL
205600961115     C* LEGGO ANOMALIE 620 PER LNA
205700961115     C                   Z-ADD     620           KCAA
205800990519     C     KANM10        SETLL     FNANM10L
205900990519     C   20KANM10        READE     FNANM10L                               31
206000990519     C  N20KANM10        READE(N)  FNANM10L                               31
206100961121    1C     *IN31         DOWEQ     *OFF
206200961203     C* ESCLUDO LOCALI
206300961203     C     ANMLNP        IFGT      *ZEROS
206400961203     C     ANMLNP        ANDEQ     ANMLNA
206500961203     C     ANMLNP        OREQ      *ZEROS
206600961203     C     ANMFLS        ANDEQ     ANMLNA
206700990519     C   20              UNLOCK    FNANM10L
206800961203     C                   GOTO      LEG620
206900961203     C                   END
207000970121     C*
207100970121     C                   MOVE      ANMFLS        KFLS
207200970121     C                   MOVE      ANMLNA        KLNA
207300970121     C                   MOVE      ANMNRS        KNRS
207400970121     C                   MOVE      ANMSCN        KNSC
207500970121     C*
207600970121     C* SE BIS CERCO SPUNTA DEFLUENZA
207700970117     C   22              EXSR      CHKBRV
207800961203     C*
207900970117     C     *IN22         IFEQ      *ON
208000970117     C     *IN09         ANDEQ     *OFF
208100970117     C     *IN22         OREQ      *OFF
208200961118     C* DETERMINO QUANDO IL COLLO E' STATO DATO PER PARTITO
208300961118     C                   EXSR      RCRDTP
208400961205     C                   EXSR      GEDE6A
208500970117     C                   END
208600961121     C*
208700961203     C     LEG620        TAG
208800961129     C                   Z-ADD     620           KCAA
208900990519     C   20KANM10        READE     FNANM10L                               31
209000990519     C  N20KANM10        READE(N)  FNANM10L                               31
209100961121    1C                   ENDDO
209200961121     C*----------------------------------------------------------------
209300961120     C* CONTROLLO DISCORDANZE 6  - COLLO DATO PER INOLTRATO MA A TERRA
209400961121     C*----------------------------------------------------------------
209500970117     C* LEGGO ANOMALIE 600 PER LNA SOLO SE NO BIS
209600970117   1AC     *IN22         IFEQ      *OFF
209700961115     C                   Z-ADD     600           KCAA
209800990519     C     KANM10        SETLL     FNANM10L
209900990519     C   20KANM10        READE     FNANM10L                               31
210000990519     C  N20KANM10        READE(N)  FNANM10L                               31
210100961121    1C     *IN31         DOWEQ     *OFF
210200961203     C* ESCLUDO LOCALI
210300961203     C     ANMLNP        IFGT      *ZEROS
210400961203     C     ANMLNP        ANDEQ     ANMLNA
210500961203     C     ANMLNP        OREQ      *ZEROS
210600961203     C     ANMFLS        ANDEQ     ANMLNA
210700990519     C   20              UNLOCK    FNANM10L
210800961203     C                   GOTO      LEG600
210900961203     C                   END
211000961203     C*
211100961118     C* DETERMINO DATA IN CUI IL COLLO E' STATO DATO PER PARTITO
211200961121     C                   MOVE      ANMFLS        KFLS
211300961121     C                   MOVE      ANMLNA        KLNA
211400961121     C                   MOVE      ANMNRS        KNRS
211500961121     C                   MOVE      ANMSCN        KNSC
211600961118     C                   EXSR      RCRDTP
211700961205     C*
211800961205     C                   MOVE      ANMFLE        WFIL
211900961205     C                   MOVE      ANMNPS        WNPS
212000961205     C*
212100961205     C                   SETON                                        09
212200961205     C* IL COLLO PUO' NON ESSERE PIU' A TERRA SE L'ANOMALIA 600 CHE
212300961205     C* STO LEGGENDO HA DATA < ALLA DATA DEL FOGLIO DI DEFL. CHE STO
212400961205     C* CHIUDENDO
212500961212     C     ANMDAO        IFLT      D82DFV
212600961205     C                   Z-ADD     645           KCAA
212700961205     C                   EXSR      CTRANM
212800961205     C                   END
212900961205     C*
213000961205    2C     *IN09         IFEQ      *ON
213100961127     C                   MOVE      'E6 '         KCEV
213200970117     C* STAMPO LA DISCORDANZA
213300970117     C     D82LDS        IFEQ      'SI'
213400970117     C                   EXSR      PRTDIS
213500970117     C                   END
213600961115     C*
213700961113     C* ELABORAZIONE DEFINITIVA
213800961205     C   20              EXSR      CRTEVS
213900961121     C*
214000961121     C*
214100961121     C* COMPLETO AGGIORNAMENTI SE ELABORAZIONE DEFINITIVA
214200961121     C* SE C'E' RECORD BOLLA AGGIORNO ARTDTR=0
214300961205    3C     *IN20         IFEQ      *ON
214400961205    4C     WBOLLA        IFEQ      'S'
214500961121     C                   MOVE      *ZEROS        ARTDTR
214600961121     C                   UPDATE    FNART000
214700961205    4C                   END
214800961121     C* CANCELLO ANOMALIA
214900990519     C                   DELETE    FNANM000
215000961205    3C                   END
215100961205   X2C                   ELSE
215200961205     C* IL COLLO NON E' PIU' A TERRA MA E' IN PARTENZA
215300961205     C                   CLEAR                   WFIL
215400961205     C                   EXSR      GEDE6A
215500961205    2C                   END
215600961203     C*
215700961203     C     LEG600        TAG
215800961203     C*
215900961205     C                   Z-ADD     600           KCAA
216000990519     C   20KANM10        READE     FNANM10L                               31
216100990519     C  N20KANM10        READE(N)  FNANM10L                               31
216200961121    1C                   ENDDO
216300970117   1AC                   END
216400961113     C                   ENDSR
216500060913     c*****************************************************************
216600060913     C* VERIFICO ESISTENZA DISCORDANZA E7-spedizione parziale ma invio
216700060913     C*                                  il collo
216800060913     c*****************************************************************
216900060913     c     CTRE7x        BEGSR
217000060913     C                   MOVE      'E7 '         KCEV
217100060913     C     KEVS2         SETGT     FNEVS02L
217200060913     C     KEVS2         READPE    FNEVS02L                               31
217300060913    4C     *IN31         IFEQ      *ON
217400060913     C* CREO E STAMPO DISCORDANZA E7
217500060913     C                   EXSR      GEDIS27
217600060913   X4C                   ELSE
217700060913   4AC     EVSDEV        IFLT      D82DFV
217800060913     C     *IN22         ANDEQ     *OFF
217900060913     C* ESISTE EVENTO 2 CREATO IN UN GIORNO PRECEDENTE --> MEMORIZZO LA
218000060913     C* DATA E LA PISTOLA
218100060913     C                   MOVE      EVSDEV        WDEV
218200060913     C                   MOVE      EVSNPS        WNPS
218300060913     C* CERCO ANCHE SE ESISTE EVENTO 7A
218400060913     C                   MOVE      'E7A'         KCEV
218500060913     C* SE TROVO E7A IN DATA DEL FOGLIO NON FACCIO NIENTE
218600120823     C     KEVS2A        SETLL     FNEVS02L                               31
218700060913     C     *IN31         CABEQ     *ON           fineCTRE7
218800060913     C     KEVS2         SETGT     FNEVS02L
218900060913     C     KEVS2         READPE    FNEVS02L                               31
219000060913    5C     *IN31         IFEQ      *OFF
219100060913     C* TENGO LA DATA EVENTO MAGGIORE FRA EVENTO E2 ED EVENTO E2A
219200060913    6C     EVSDEV        IFGT      WDEV
219300060913     C                   MOVE      EVSDEV        WDEV
219400060913    6C                   END
219500060913    5C                   END
219600060913     C* CERCO SE ESISTE EVENTO 6-COLLO DATO PER INOLTRATO MA A TERRA
219700060913     C* IN DATA > WDEV
219800060913     C                   MOVE      'E6 '         KCEV
219900060913     C     KEVS2B        SETGT     FNEVS02L
220000060913     C     KEVS2         READE     FNEVS02L                               31
220100060913    5C     *IN31         IFEQ      *OFF
220200060913     C     EVSDEV        ANDLT     D82DFV
220300060913     C                   MOVE      'E7 '         KCEV
220400060913     C                   EXSR      GEDIS27
220500060913   X5C                   ELSE
220600060913     C* CREO EVENTO E7A SOLO SE IL COLLO NON E' IN PARTENZA OGGI
220700060913     C                   Z-ADD     620           KCAA
220800060913     C     KANM2         SETLL     FNANM02L                               31
220900060913     C  N31              MOVE      'E7B'         KCEV
221000120823     C  N31KEVS2A        SETLL     FNEVS02L                               31
221100060913    6C     *IN31         IFEQ      *OFF
221200060913     C                   MOVE      'E7A'         KCEV
221300060913     C                   EXSR      GEDIS27
221400060913    6C                   END
221500060913    5C                   END
221600060913   4AC                   END
221700060913    4C                   END
221800060913    cc     FINECTRE7     ENDSR
221900961205     C*****************************************************************
222000961205     C* STAMPA + CREAZIONE DISCORDANZA 6A
222100961205     C*****************************************************************
222200961205     C     GEDE6A        BEGSR
222300970124     C* VERIFICO SE ESISTE GIA' DISCORDANZA: POTREBBE ESSERE, INFATTI,
222400970124     C* GIA' STATA CREATA DALLA PRECEDENTE CHIUSURA DEL BIS
222500970124     C* SE ESISTE GIA' NON FACCIO NIENTE
222600970124     C                   MOVEL     'E6A'         KCEV
222700970124     C     KEVS2A        SETLL     FNEVS02L                               31
222800060913     c                   if        not *in31
222900060913     C                   MOVEL     'E6B'         KCEV
223000060913     C     KEVS2A        SETLL     FNEVS02L                               31
223100060913     c                   endif
223200060913     c                   if        not *in31
223300060913     C                   MOVEL     'E7B'         KCEV
223400060913     C     KEVS2A        SETLL     FNEVS02L                               31
223500060913     c                   endif
223600060913     c
223700970124    1C     *IN31         IFEQ      *OFF
223800961205     C*
223900961205     C* DETERMINO NUMERO PISTOLA E SE ESISTE SPUNTA DI DEFLUENZA
224000970117     C  N22              EXSR      CHKBRV
224100970117     C*
224200970117     C     WBOLLA        IFEQ      'S'
224300060913     c* Se spedizioni completa E6A, se non inviata bolla per
224400060913     c*  invio solo parziale, E7B
224500060913     c                   if        d82isc=' ' or arbft3='T'
224600970117     C                   MOVE      'E6A'         KCEV
224700060913     c                   else
224800060913     C                   MOVE      'E7B'         KCEV
224900060913     c                   endif
225000060913     c
225100970117     C                   ELSE
225200970117     C                   MOVE      'E6B'         KCEV
225300970117     C                   END
225400970117     C* STAMPO DISCORDANZA
225500970117     C     D82LDS        IFEQ      'SI'
225600970117     C                   EXSR      PRTDIS
225700970117     C                   END
225800970117     C*
225900961205     C* ELABORAZIONE DEFINITIVA
226000961205    2C     *IN20         IFEQ      *ON
226100970117     C* RICHIAMO ROUTINE PER AUTOGENERARE SPUNTA PARTENZA SE NON C'E'
226200970117     C* E CHIUDERE EVENTUALI ANOMALIE 145 E 146 DEI GIORNI PRECEDENTI
226300970117     C     WBOLLA        COMP      'S'                                424243
226400970117     C                   EXSR      PARCOL
226500970117     C*
226600961205     C                   EXSR      CRTEVS
226700961205     C*
226800961205     C* SE ELAB.DEFINITIVA E C'è REC.BOLLA AGGIORNO ARTDTR=DATA EVENTO
226900961205    3C     WBOLLA        IFEQ      'S'
227000961205     C                   MOVE      EVSDEV        ARTDTR
227100961205     C                   UPDATE    FNART000
227200961205    3C                   END
227300961205     C* CANCELLO ANOMALIA
227400990519     C                   DELETE    FNANM000
227500961205    2C                   END
227600970124    1C                   END
227700961205     C                   ENDSR
227800961120     C*****************************************************************
227900060913     C* STAMPA + CREAZIONE DISCORDANZA E2/E2A/E7/E7A
228000961120     C*****************************************************************
228100060913     C     GEDIS27       BEGSR
228200961120     C*
228300961121     C* SE DISCORDANZA 2 DETERMINO SE ESISTE LA SPUNTA DI DEFLUENZA
228400961121     C* E IL NUMERO PISTOLA
228500961121     C     KCEV          IFEQ      'E2 '
228600970117     C     *IN22         ANDEQ     *OFF
228700060913     c
228800060913     C     KCEV          orEQ      'E7 '
228900060913     C     *IN22         ANDEQ     *OFF
229000961121     C                   EXSR      CHKBRV
229100961120     C                   END
229200970117     C*
229300961127     C                   CLEAR                   WAAS
229400961127     C                   CLEAR                   WLNP
229500961127     C                   CLEAR                   WNSP
229600970117     C***
229700970117     C*  STAMPO DISCORDANZA
229800970117     C     D82LDS        IFEQ      'SI'
229900970117     C                   EXSR      PRTDIS
230000970117     C                   END
230100961120     C****
230200060913     C* CREO EVENTO
230300961205    4C     *IN20         IFEQ      *ON
230400970117     C* SOLO SE CREO DISCORDANZA 2 (ESCLUDO QUINDI SE DISC. 2A):
230500970117     C* RICHIAMO ROUTINE PER AUTOGENERARE SPUNTA PARTENZA SE NON C'E'
230600970117     C* E CHIUDERE EVENTUALI ANOMALIE 145 E 146 DEI GIORNI PRECEDENTI
230700970117     C     KCEV          IFEQ      'E2 '
230800060913     C     KCEV          oreq      'E7 '
230900970117     C                   SETON                                        42
231000970117     C                   EXSR      PARCOL
231100970117     C                   END
231200970117     C*
231300961120     C                   EXSR      CRTEVS
231400961120     C*
231500961120    4C                   ENDIF
231600961120     C*
231700961120     C                   ENDSR
231800961121     C*****************************************************************
231900961121     C*    DETERMINO ESISTENZA SPUNTA DI DEFLUENZA E NUMERO PISTOLA   *
232000961121     C*****************************************************************
232100961121     C     CHKBRV        BEGSR
232200961121     C*
232300961121     C                   CLEAR                   WFNPS
232400961121     C                   Z-ADD     1             KNPG
232500070123     C     KBRV1         CHAIN(n)  FNBRV01L                           31
232600961121     C     *IN31         IFEQ      *OFF
232700961121     C                   MOVE      BRVNPS        WNPS
232800961121     C                   ELSE
232900961121     C                   MOVE      88            WNPS
233000961122     C                   MOVE      '1'           WFNPS             1
233100961121     C                   END
233200970116     C*
233300970116     C* SE FV BIS IMPOSTO *IN09
233400970117     C   22WFNPS         IFEQ      '1'
233500970116     C                   SETON                                        09
233600970116     C                   ELSE
233700970116     C                   SETOFF                                       09
233800970116     C                   END
233900961121     C                   ENDSR
234000961118     C*****************************************************************
234100961118     C*    DETERMINO QUANDO COLLO E' STATO DATO PER PARTITO           *
234200961118     C*****************************************************************
234300961118     C     RCRDTP        BEGSR
234400961118     C*
234500961118     C                   CLEAR                   WDTPAR
234600961118     C                   MOVE      *BLANKS       WBOLLA            1
234700961118     C*
234800961118     C  N20KART27        CHAIN(N)  FNART27L                           31
234900961118     C   20KART27        CHAIN     FNART27L                           31
235000961118     C     *IN31         IFEQ      *OFF
235100961118     C                   MOVE      'S'           WBOLLA
235200961118     C                   MOVE      ARTDTR        WDTPAR
235300961118     C                   MOVE      ARTAAS        WAAS
235400961118     C                   MOVE      ARTLNP        WLNP
235500961118     C                   MOVE      ARTNSP        WNSP
235600060913     c
235700060913     c* Se devo inviare soltanto le spedizioni complete, chaino anche
235800060913     c*  la bolla per sapere se inviata
235900060913     c                   if        d82isc='S'
236000060913     c     karb1         chain(n)  fnarb01l
236100060913     c                   endif
236200060913     c
236300961118     C                   ELSE
236400970602     C                   CLEAR                   WAAS
236500961118     C                   MOVE      ANMAAS        WAAS
236600961118     C                   MOVE      ANMLNP        WLNP
236700961118     C                   MOVE      ANMNSP        WNSP
236800961118     C                   MOVE      'E2 '         KCEV
236900961121     C     KEVS2         SETGT     FNEVS02L
237000961127     C     KEVS2         READPE    FNEVS02L                               31
237100961118     C  N31              MOVE      EVSDEV        WDTPAR
237200060913     c*
237300961118     C                   END
237400961118     C                   ENDSR
237500961112     C*****************************************************************
237600961112     C*    SCRITTURA EVENTO                                           *
237700961112     C*****************************************************************
237800961113     C     CRTEVS        BEGSR
237900961113     C                   CLEAR                   FNEVS000
238000961115     C*
238100961115     C                   MOVE      WAAS          EVSAAS
238200961115     C                   MOVE      WLNP          EVSLNP
238300961115     C                   MOVE      WNSP          EVSNSP
238400961115     C*
238500961115     C                   MOVE      KFLS          EVSFLS
238600961115     C                   MOVE      KLNA          EVSLNA
238700961115     C                   MOVE      KNRS          EVSNRS
238800961115     C                   MOVE      KNSC          EVSNSC
238900961120     C                   MOVE      WNPS          EVSNPS
239000961115     C*
239100961212     C                   MOVE      D82DFV        EVSDEV
239200961115     C                   MOVE      KCEV          EVSCEV
239300961115     C* SE COLLO A TERRA MEMORIZZO FILIALE CHE HA SPARATO IL COLLO
239400961115     C                   MOVE      WFIL          EVSFIL
239500961115     C*
239600961115     C* EVSDAT --> PER ORA MI SERVE PER DISCORDANZA 6 PER MEMORIZZARE
239700961115     C*            LA DATA IN CUI IL COLLO ERA STATO DATO PER PARTITO
239800961115     C                   MOVE      WDTPAR        EVSDAT
239900961125     C     WDTPAR        IFGT      *ZEROS
240000961115     C                   MOVEL     COS1          EVSDES
240100961125     C                   END
240200961115     C*
240300961115     C                   WRITE     FNEVS000
240400120823     C*
240500961112     C                   ENDSR
240600961114     C*****************************************************************
240700961122     C*    RICERCA ANOMALIA 645  IN DATA = DATA FOGLIO DEFLUENZA      *
240800961114     C*****************************************************************
240900961114     C     CTRANM        BEGSR
241000020225     C                   SETOFF                                       09
241100020225     C     KANM2A        CHAIN     FNANM02L                           31
241200020225     C  N31              SETON                                        09
241300961114     C                   ENDSR
241400961115     C*****************************************************************
241500961115     C*    OPERAZIONI PER COLLO CHE PARTE                             *
241600961115     C*****************************************************************
241700961115     C     PARCOL        BEGSR
241800961122     C* CHIUSURA ANOMALIE 145 E 146
241900961122     C                   Z-ADD     145           KCAA
242000961118    1C                   DO        2
242100990519     C     KANM2         CHAIN     FNANM02L                           31
242200961122    2C     *IN31         IFEQ      *OFF
242300020225     C                   CLEAR                   FNLR33DS
242400961212     C                   MOVE      D82DFV        D33DCH
242500961115     C                   MOVE      ANMNR2        D33NRR
242600961115     C* PRENDO FASE ANOMALIA
242700961115     C                   CALL      'TRUL11R'
242800961115     C                   PARM                    PARAM9
242900961115     C                   MOVE      FASE          D33FSC
243000961115     C                   CALL      'FNLR33R'
243100020225     C                   PARM                    FNLR33DS
243200961118    3C                   END
243300961115     C* RIPETO GIRO PER 146
243400961115     C                   Z-ADD     146           KCAA
243500961118    1C                   ENDDO
243600961115     C*
243700961115     C* AUTOGENERAZIONE SPUNTA PARTENZA 88 SE NON ESISTE
243800961120     C     WFNPS         IFEQ      '1'
243900961120     C* SE DEVO AUTOGENERARE SPUNTA PARTENZA DI COLLO CON MANCA RECORD
244000961120     C* BOLLA ARRIVO CERCO LA ZONA LEGGENDO UNA QUALUNQUE SPUNTA DEL
244100961120     C* COLLO
244200961120     C     *IN42         IFEQ      *ON
244300961120     C                   CLEAR                   WRVZNC
244400070123     C     KBRV7         CHAIN     FNBRV07L                           31
244500961120     C     *IN31         IFEQ      *OFF
244600961120     C                   MOVE      BRVZNC        WRVZNC
244700961120     C                   END
244800961120     C                   END
244900980126     C*
245000980126     C                   EXSR      DOPPIE
245100980126     C*
245200140625     C   40              CLEAR                   FNBRV5
245300040303     c  n40*in41         ifeq      *off
245400040303    7C     NST(WA)       orne      '1'                                          NR.SPUNTE GG.
245500070123     C                   CLEAR                   FNBRV000
245600040303     c                   else
245700140625     C                   CLEAR                   FNBRV5
245800040303     c                   endif
245900961115     C*
246000961212     C                   Z-ADD     SIMFEL        BRVFGS                         FIL GESTIONE
246100000208     C                   Z-ADD     SIMFEL        BRVPES                         FIL GESTIONE
246200000208     C                   Z-ADD     D82NFV        BRVNFV                         NUM.FOGLIO
246300961115     C                   Z-ADD     1             BRVNPG                         CAT.FOGLIO
246400961115     C                   Z-ADD     SIMFEL        BRVFLE                         FIL.ELABORAT
246500070220     c                   time                    wtimez
246600070220     c* millisecondi
246700070220     c                   extrct    wtimez:*ms    millisec          6 0
246800070220     c                   movel     millisec      brvmis
246900070220     c* data immissione
247000070220     c                   movel     wtimez        dataiso
247100070220     c                   movel     dataiso       brvdfs
247200070220     c* ora immissione
247300070220     c                   move      wtimez        oraiso
247400070220     c                   movel     oraiso        brvhms
247500070220     c*
247600070220     c                   movel     'A'           brvtap
247700070220     c                   eval      brvpru='§'+ nompgm
247800961204     C                   MOVE      KFLS          BRVLNP
247900961204     C                   MOVE      KLNA          BRVLNA
248000961204     C                   MOVE      KNRS          BRVNRS
248100961204     C                   MOVE      KNSC          BRVNSC
248200961115     C*
248300961204     C*  DA ANOMALIE
248400961204     C   42              MOVE      WRVZNC        BRVZNC                          ZONA
248500961204     C*  DA FNART
248600961204     C   43              MOVE      ARBZNC        BRVZNC                          ZONA
248700961115     C*
248800961115     C                   Z-ADD     88            BRVNPS
248900961115     C*
249000140625     C   40              WRITE     FNBRV5
249100040303     c  n40*in41         ifeq      *off
249200040303    7C     NST(WA)       orne      '1'                                          NR.SPUNTE GG.
249300070123     C                   update    fnbrv000
249400040303     c                   else
249500140625     C                   update    fnbrv5
249600040303     c                   endif
249700961115     C                   END
249800961115     C*
249900961115     C                   ENDSR
250000980126     C**************************************************************************
250100040303     C* VERIFICO SE esiste già la spunta:
250200040303     C* 1) per p.o. se previsto dalla categoria, tabella 7n (4)
250300040303     C* 2) per num foglio (npg+nfv+fgs) se da tenere tutte
250400980126     C**************************************************************************
250500980126     C     DOPPIE        BEGSR
250600040303     C*
250700040303     C                   Z-ADD     1             KNPG                           CATEGORIA
250800040303     C                   Z-ADD     SIMFEL        KFLE
250900040303     C*
251000040303     C* VERIFICO IL FLAG NELLA DS7N
251100040303     C                   Z-ADD     1             WA                3 0
251200040303     C     1             LOOKUP    CAT(WA)                                41     ON=TROVATO
251300040303     c     *in41         ifeq      *off
251400040303    7C     NST(WA)       orne      '1'                                          NR.SPUNTE GG.
251500140625     C     KBRV1         CHAIN     FNBRV01L                           40
251600040303     c                   else
251700140625     C     KBRV3         CHAIN     FNBRV05L                           40
251800040303     c                   endif
251900040303     c
252000070123     C* SE C'E' L'ANOMALIA "E" SCriVO FILE FNBRVE0F
252100040303     C  n40BRVCAN        IFEQ      'E'
252200070123     C     KBRVE         SETLL     FNBRVE1L                               99
252300070123     C  N99              WRITE     FNBRV00E
252400040303     C                   ENDIF
252500980126     C*
252600980126     C                   ENDSR
252700961112     C*****************************************************************
252800961112     C*    CHAIN TABEL00F                                             *
252900961112     C*****************************************************************
253000961112     C     CTABEL        BEGSR
253100961112     C     KTAB          CHAIN     TABEL                              31
253200961114     C  N31TBLFLG        IFNE      ' '
253300961112     C                   SETON                                        31
253400961112     C                   END
253500961112     C                   ENDSR
253600961129     C**************************************************************************
253700961129     C*  ABBLANCA ZERO NON SIGNIFICATIVI
253800961129     C**************************************************************************
253900961129     C     ABBL          BEGSR
254000961129     C*
254100020225     C                   Z-ADD     1             YY
254200020225     C     K08(YY)       DOWEQ     '0'
254300020225     C     YY            ANDLT     8
254400020225     C                   MOVEL     ' '           K08(YY)
254500020225     C                   ADD       1             YY
254600961129     C                   ENDDO
254700961129     C*
254800961129     C                   ENDSR
254900961121     C*****************************************************************
255000961121     C* STAMPA DISCORDANZE -------------------------------------------*
255100961121     C*****************************************************************
255200961121     C     PRTDIS        BEGSR
255300961121     C*
255400961122     C                   SETON                                        08
255500961125     C*
255600961125     C     WFIL          COMP      *ZEROS                                 14
255700961127     C*
255800961127     C     WNSP          COMP      *ZEROS                             1515
255900961127     C     KNSC          COMP      *ZEROS                             1616
256000961127     C     KNRS          COMP      *ZEROS                             1717
256100961128     C*
256200961129     C     KCEV          COMP      'E4 '                                  18
256300060918     C  n18KCEV          COMP      'E8 '                                  18
256400961127     C*
256500961127     C* STAMPO ANNO DI 2
256600961127     C                   MOVE      WAAS          WPAAS             2 0
256700961122     C*
256800961122     C* DECODIFICO CAUSALE EVENTO
256900961122     C                   MOVE      '8E'          KCOD
257000961122     C                   MOVEL(P)  KCEV          KKEY
257100970114     C* prendo la descrizione in italiano
257200970114     C     KTAB          CHAIN     TABEL00F                           31
257300970114     C  N31              MOVEL     TBLUNI        DCEV             60
257400970114     C   31              CLEAR                   DCEV
257500970114     C* se devo inviarlo anche via fax prendo anche la descrizione in
257600970114     C* inglese
257700120821     c     d82ftt        ifne      FEDEX
257800970115     C   20              MOVE      '1'           KKEY
257900970115     C   20KTAB          CHAIN     TABEL00F                           31
258000970115     C  N31
258100970115     CAN 20              MOVEL     TBLUNI        DCEVE            60
258200970115     C   31
258300970115     CAN 20              CLEAR                   DCEVE
258400970115    1C   20*INOB         IFEQ      *ON
258500040423
258600040423     c                   if        FAX_Disc_Est = 'S'
258700961122     C                   ADD       1             WPAG
258800961121     C                   EXCEPT    TESDIS
258900970114     C                   SETOFF                                       OB
259000040423     c                   end
259100040423
259200961129    1C                   END
259300040423
259400040423     c                   if        FAX_Disc_Est = 'S'
259500970115     C   20              EXCEPT    DETDIS
259600040423     c                   end
259700040423
259800020301     c                   endif
259900970114    1C     *INOE         IFEQ      *ON
260000970114     C                   ADD       1             WPAG1
260100970114     C                   EXCEPT    TESDIC
260200970114     C                   SETOFF                                       OE
260300970114    1C                   END
260400970114     C                   EXCEPT    DETDIC
260500961122     C* STAMPA DATA PARTENZA COLLO SE DISCORDANZA 6 O 6A
260600961129    1C     WDTPAR        IFGT      *ZEROS
260700961122     C                   CLEAR                   WLBDA8
260800961122     C                   MOVE      WDTPAR        G02INV
260900961122     C                   MOVE      '3'           G02ERR
261000961122     C                   CALL      'XSRDA8'
261100961122     C                   PARM                    WLBDA8
261200961122     C                   MOVE      G02DAT        WDTPA1
261300120821     c     d82ftt        ifne      FEDEX
261400970115    2C   20*INOB         IFEQ      *ON
261500040423
261600040423     c                   if        FAX_Disc_Est = 'S'
261700961122     C                   ADD       1             WPAG
261800961122     C                   EXCEPT    TESDIS
261900970114     C                   SETOFF                                       OB
262000040423     c                   end
262100040423
262200961129    2C                   END
262300040423
262400040423     c                   if        FAX_Disc_Est = 'S'
262500970115     C   20              EXCEPT    DETDI2
262600970115     C   20              EXCEPT    FIT
262700040423     c                   end
262800040423
262900020301     c                   endif
263000970114    2C     *INOE         IFEQ      *ON
263100970114     C                   ADD       1             WPAG1
263200970114     C                   EXCEPT    TESDIC
263300970114     C                   SETOFF                                       OE
263400970114    2C                   END
263500970114     C                   EXCEPT    DETDC2
263600970114     C                   EXCEPT    FITC
263700961129   X1C                   ELSE
263800961129     C     KCEV          IFNE      'E4 '
263900120821     c     d82ftt        ifne      FEDEX
264000040423
264100040423     c                   if        FAX_Disc_Est = 'S'
264200970115     C   20              EXCEPT    FIT
264300040423     c                   end
264400040423
264500020301     c                   endif
264600970114     C                   EXCEPT    FITC
264700961129     C                   END
264800961129    1C                   END
264900961121     C*
265000961121     C                   ENDSR
265100960529     C*-----------------------------------------------------------------*
265200960529     C*  Operazioni iniziali
265300960529     C*-----------------------------------------------------------------*
265400960529     C     *INZSR        BEGSR
265500960529     C*
265600960529     C*  Reperisco dati da video
265700960529     C     *ENTRY        PLIST
265800960529     C                   PARM                    KPJBA
265900020225     C                   MOVEL     KPJBU         TRTC82DS
266000040423
266100040423     ** Maggio 2004:
266200120823     ** Non si vuole più mandare il FAX discordanze al Partner
266300040423     c                   move      'N'           FAX_Disc_Est      1
266400040423
266500070123     C* MUOVO N.FOGLIO IN CAMPO LUNGO 6,0 PER CHAIN SU FNBRV
266600000208     C                   Z-ADD     D82NFV        D82NFS
266700990421      * SALVO KPJBU "ORIGINALE"
266800990421     C                   MOVEL     KPJBU         WKPJBU
266900971230      *
267000971230     C* MI SALVO LINEA DI ARRIVO ESTERA (ALTRIMENTI SI SPORCA NEL
267100971230     C* RICHIAMARE IL TRTC83R2)
267200961211     C                   MOVE      D82LNP        LNAEXP
267300961129     C*  Controllo se devo scrivere il nr.spedizione compattato
267400961129     C     D82SPC        COMP      'S'                                    21
267500960529     C*  Richiamo XPARUT
267600960529     C                   Z-ADD     1             CODUT
267700960529     C                   CALL      'X§PARUT'
267800020225     C                   PARM                    UT§DSE0F
267900960529     C                   MOVEL     REC80         CNCR80
268000110511     C                   MOVEL     REC42         CNCR42
268100110511     C                   MOVEL     simRGS        RSUT             20
268200110511     C                   MOVEL     simRGS        RSUTM            19
268300020225     C* RICERCA CAPOCONTI
268400961122     C                   DO        50            X                 2 0
268500961122     C                   MOVE      TCU(X)        TCUDS
268600961122     C     F56           IFEQ      'CG'
268700961122     C     F4            ANDEQ     '1'
268800961122     C     F3            ANDEQ     '0'
268900961122     C                   Z-ADD     KCU(X)        KCI               4 0
269000020225     C                   ENDIF
269100020225     C                   ENDDO
269200961122     C* REPERISCO KG PER METRO CUBO PER CALCOLO PESO TASSABILE
269300961122     C                   MOVEL     '7A'          KCOD
269400961122     C                   MOVEL(P)  '2'           KKEY
269500961122     C     KTAB          CHAIN     TABEL00F                           31
269600961122     C  N31              MOVEL     TBLUNI        DS7A2
269700961122     C   31              CLEAR                   DS7A2
269800150825     C*
269900150825     C** CARICAMENTO DATI VARI TASSAZIONE
270000150825     C                   MOVEL     'QT'          kcod
270100150825     C                   MOVEL(P)  '1'           kKEY
270200150825     C     KTAB          CHAIN     TABEL00F                           31
270300150825     C  N31              MOVEL     TBLUNI        DSQT1
270400150825     C   31              CLEAR                   DSQT1
270500150825     C*
270600980126     C* SERVE SAPERE SE NELLE VARIE CATEGORIE FOGLI POSSO
270700980126     C* AVERE SPUNTE DOPPIE OPPURE NO
270800980126     C                   Z-ADD     *ZERO         WA                             CONTATORE
270900980126     C                   MOVEL     '7N'          KCOD
271000980126     C     KTAB2         SETLL     TABEL00F
271100980126     C     KTAB2         READE     TABEL00F                               31
271200980126     C     *IN31         DOWEQ     *OFF
271300980126     C     TBLFLG        IFEQ      *BLANKS
271400980126     C                   ADD       1             WA
271500980126     C                   MOVEL     TBLUNI        DS7N
271600980126     C                   MOVEL     TBLKEY        CAT(WA)
271700980126     C                   MOVEL     §7NNST        NST(WA)
271800980126     C                   ENDIF
271900980126     C     KTAB2         READE     TABEL00F                               31
272000980126     C                   ENDDO
272100990412      * CARICO TABELLA £1 SE NON SONO IN SEDE
272200990412     C     SIMFEL        IFNE      *ZEROS
272300020225     C                   CLEAR                   TRUL06DS
272400990412     C                   CLEAR                   KPJBU
272500990412     C                   MOVEL     '£1'          D06COD
272600990412     C                   MOVEL     SIMFEL        D06KEY
272700990412     C                   MOVEL     'L'           D06TLA
272800020225     C                   MOVEL     TRUL06DS      KPJBU
272900990412      *
273000990412     C                   CALL      'TRUL06R'
273100990412     C                   PARM                    KPJBA
273200020225     C                   MOVEL     KPJBU         TRUL06DS
273300990421      * REIMPOSTO KPJBU "ORIGINALE"
273400990421     C                   MOVEL     WKPJBU        KPJBU
273500990421     C                   END
273600090402     c
273700090402     c                   open(e)   tita430c
273800090402     c                   if        not %open(tita430c)
273900090402     c                   eval      %subst(WTA430:7:4)='GRPS'
274000090402     c                   eval      %subst(WTAS30:7:4)='GRPS'
274100090402     c                   open      tita430c
274200090402     c                   endif
274300090402     c                   open      titas30c
274400130215     c
274500960529     C*---------------------------------------------------------------*
274600960529     C* Definisco chiavi di accesso ai file
274700961114     C     KARB1         KLIST
274800961114     C                   KFLD                    ARTAAS
274900961114     C                   KFLD                    ARTLNP
275000961114     C                   KFLD                    ARTNRS
275100961114     C                   KFLD                    ARTNSP
275200961113     C     KARB55        KLIST
275300961211     C                   KFLD                    LNAEXP
275400961113     C                   KFLD                    KDT3
275500120823     C     KARB          KLIST
275600961121     C                   KFLD                    ARBAAS
275700961121     C                   KFLD                    ARBLNP
275800961121     C                   KFLD                    ARBNRS
275900961121     C                   KFLD                    ARBNSP
276000960529     C     KAR4          KLIST
276100961121     C                   KFLD                    ARBAAS
276200961121     C                   KFLD                    ARBLNP
276300961121     C                   KFLD                    ARBNRS
276400961121     C                   KFLD                    ARBNSP
276500960529     C                   KFLD                    KTRC
276600061127      * contatto nome e telefono (Tipo record [GEN])
276700061127     C     KAR5          KLIST
276800061127     C                   KFLD                    ARBAAS
276900061127     C                   KFLD                    ARBLNP
277000061127     C                   KFLD                    ARBNRS
277100061127     C                   KFLD                    ARBNSP
277200061127     C                   KFLD                    KGEN
277300061127     c                   movel     'GEN'         KGEN              3
277400960529     C     KTAB          KLIST
277500961122     C                   KFLD                    CODUT
277600960529     C                   KFLD                    KCOD
277700960529     C                   KFLD                    KKEY
277800980126     C     KTAB2         KLIST
277900980126     C                   KFLD                    CODUT
278000980126     C                   KFLD                    KCOD
278100961220     C     KETAB         KLIST
278200961220     C                   KFLD                    KECOD
278300961220     C                   KFLD                    KEKEY
278400130215     C     KEDI          KLIST
278500130215     C                   KFLD                    tabcod
278600130215     C                   KFLD                    tabkey
278700961113     C     KANM10        KLIST
278800961113     C                   KFLD                    KCAA
278900961211     C                   KFLD                    LNAEXP
279000961115     C     KANM2         KLIST
279100961115     C                   KFLD                    KFLS
279200961115     C                   KFLD                    KLNA
279300961115     C                   KFLD                    KNRS
279400961122     C                   KFLD                    KNSC
279500961115     C                   KFLD                    KCAA
279600961122     C     KANM2A        KLIST
279700961122     C                   KFLD                    KFLS
279800961122     C                   KFLD                    KLNA
279900961122     C                   KFLD                    KNRS
280000961122     C                   KFLD                    KNSC
280100961122     C                   KFLD                    KCAA
280200961212     C                   KFLD                    DFV06
280300961115     C     KEVS2         KLIST
280400961115     C                   KFLD                    KFLS
280500961115     C                   KFLD                    KLNA
280600961115     C                   KFLD                    KNRS
280700961122     C                   KFLD                    KNSC
280800961115     C                   KFLD                    KCEV
280900120823      **
281000961115     C     KEVS2A        KLIST
281100961115     C                   KFLD                    KFLS
281200961115     C                   KFLD                    KLNA
281300961115     C                   KFLD                    KNRS
281400961122     C                   KFLD                    KNSC
281500961115     C                   KFLD                    KCEV
281600961212     C                   KFLD                    D82DFV
281700120823      **
281800961119     C     KEVS2B        KLIST
281900961119     C                   KFLD                    KFLS
282000961119     C                   KFLD                    KLNA
282100961119     C                   KFLD                    KNRS
282200961122     C                   KFLD                    KNSC
282300961119     C                   KFLD                    KCEV
282400961120     C                   KFLD                    WDEV
282500120823      **
282600961121     C     KART27        KLIST
282700961121     C                   KFLD                    KFLS
282800961121     C                   KFLD                    KLNA
282900961121     C                   KFLD                    KNRS
283000961121     C                   KFLD                    KNSC
283100120823      **
283200120823     C     KBRV7         KLIST
283300120823     C                   KFLD                    KFLS
283400120823     C                   KFLD                    KLNA
283500120823     C                   KFLD                    KNRS
283600120823     C                   KFLD                    KNSC
283700120823      **
283800961121     C     KART55        KLIST
283900961211     C                   KFLD                    LNAEXP
284000961121     C                   KFLD                    KDTR
284100961120     C     KBRV1         KLIST
284200961118     C                   KFLD                    KNPG
284300000208     C                   KFLD                    D82NFS
284400961212     C                   KFLD                    SIMFEL
284500961118     C                   KFLD                    KFLS
284600961118     C                   KFLD                    KLNA
284700961118     C                   KFLD                    KNRS
284800961122     C                   KFLD                    KNSC
284900140625     C     KBRV3         KLIST
285000980126     C                   KFLD                    KNPG                           ..CATEGORIA
285100980126     C                   KFLD                    KFLS                           ..LNP
285200980126     C                   KFLD                    KLNA                           ..LNA
285300980126     C                   KFLD                    KNRS                           ..SERIE
285400980126     C                   KFLD                    KNSC                           ..SEGNACOLLO
285500980126     C                   KFLD                    KFLE                           ..FIL.GESTIONE
285600961122     C*
285700961122     C     KIND          KLIST
285800961122     C                   KFLD                    CODUT
285900961122     C                   KFLD                    KCI
286000961122     C                   KFLD                    D82CPT
286100010613     C     KBRVE         KLIST
286200010613     C                   KFLD                    BRVNPG
286300010613     C                   KFLD                    BRVNFV
286400010613     C                   KFLD                    BRVFGS
286500010613     C                   KFLD                    BRVLNP
286600010613     C                   KFLD                    BRVLNA
286700010613     C                   KFLD                    BRVNRS
286800010613     C                   KFLD                    BRVNSC
286900020808     c     K02FGV01      klist
287000020808     c                   kfld                    D82nfv
287100020808     C                   kfld                    SIMfel
287200110510     C     ktbe          klist
287300110510     C                   kfld                    tbecod
287400110510     C                   kfld                    tbeke1
287500110510     C                   kfld                    tbeke2
287600960529     C*---------------------------------------------------------------*
287700960529     C                   TIME                    UTIME             6 0
287800960529     C* PRENDO LA DATA DEL GIORNO DA TIME
287900960529     C                   TIME                    W0140            14 0
288000960716     C                   MOVE      W0140         UDATE8
288100960716     C                   MOVE      UDATE8        G02DAT
288200960716     C                   MOVEL     *BLANK        G02ERR
288300960716     C                   CALL      'XSRDA8'
288400960716     C                   PARM                    WLBDA8
288500960716     C                   Z-ADD     G02INV        WOGGI             8 0           UDATE
288600130215      *------------
288700130215     C* Caricamento Tabella Partner
288800130215     C                   MOVEL     'PT'          TABCOD
288900130215     C     TABCOD        setll     EDTAB01L
289000130215     C     TABCOD        reade     EDTAB01L
289100130215      *
289200130215     C                   DOW       not %EoF(edtab01l)
289300130215      *
289400130215     C     TABFLG        IFEQ      *BLANKS
289500130215     c                   eval      dsPT = tabuni
289600130215      *
289700130215      * deve uscire con un solo record  tramite il codice KSC del PARTNER
289800130215      *  se ci fossero più codici KSC uguali per motivi particolari il primo
289900130215      *  in ordine di KEY DEVE essere, x regola, quello del Partner mentre il
290000130215      *  secondo è definito come CLIENTE sempre.
290100130215     C     D82CPT        ifeq      §PTKSC
290200130215      *
290300130215      *** al suo interno carica anche l'altra tabella correlata
290400130215      **   in questo caso la PZ e poi esce
290500130215     C                   MOVEL     'PZ'          TABCOD
290600130215     C     Kedi          chain     EDTAB01L
290700130215     c                   if        %Found(edtab01l)
290800130215     c                   eval      dsPZ = tabuni
290900130215     c                   end
291000130215      *
291100130215     c                   leave
291200130215     c                   end
291300130215      *
291400130215     C                   END
291500130215      *
291600130215     C     TABCOD        READE     EDTAB01L
291700130215     C                   END
291800130215      *------------
291900110510      *
292000110510     c                   move      'VPO'         tbecod
292100110510     c                   movel     'BRT'         tbeke1
292200110510     c                   clear                   tbeke2
292300110510     c     ktbe          chain     tntbe01l
292400110510     c                   if        %found(tntbe01l)
292500110510     c                   movel     tbeuni        dvpobrt
292600110510     c                   if        Woggi  <= §VPOBRTDTA
292700110510     c                   seton                                        66
292800110510     c                   endif
292900110510     c                   endif
293000120305      *
293100120305      ***cambio di marchio in BRT finale x il 15/3
293200120305     c                   setoff                                       66
293300120305     c                   if        Woggi  <  20120315
293400120305     c                   seton                                        66
293500120305     c                   endif
293600961114     C*
293700961114     C     D82DEF        COMP      'SI'                                   20
293800970117     C     D82FVB        COMP      'B'                                    22
293900970114     C*
294000970114     C*  IMPOSTO DATA FOGLIO VIAGGIO PER STAMPA
294100970114     C                   Z-ADD     D82DFV        DATA
294200970114     C                   Z-ADD     MM            GGMM              4 0
294300970114     C                   MOVEL     GG            GGMM
294400970114     C                   Z-ADD     AA            PARDFV
294500970114     C                   MOVEL     GGMM          PARDFV
294600970114     C                   MOVE      D82DFV        DFV06
294700970114     C*  Data CMR
294800970114     C                   Z-ADD     D82DCM        DATA
294900970114     C                   Z-ADD     MM            GGMM              4 0
295000970114     C                   MOVEL     GG            GGMM
295100970114     C                   Z-ADD     AA            DATCMR            8 0
295200970114     C                   MOVEL     GGMM          DATCMR
295300020225      *
295400970114     C   20              OPEN      FNEVS01L
295500020225      *
295600020225      * IMPOSTO TIPO RECORD PER LETTURE WORK FILE
295700970113     C  N20              MOVE      'P'           WTRC
295800120821     C   20              MOVE      'D'           WTRC
295900020225      ** LETTURA/AGGIORNAMENTO FITTIZIO PER GIUSTIFICARE L'APERTURA
296000020225      ** IN UPDATE (DEVO FARLO PER UTILIZZARE IL FILE SOTTO COMIT VISTO CHE
296100020225      ** E' UTILIZZATO IN UPDATE NEI PGM DI SCRITTURA RCD PER TRASMISSIONE)
296200970124     C     *LOVAL        SETLL     FNARB01L
296300970124     C                   READ      FNARB01L                               31
296400970124     C  N31              EXCEPT    FITTIZ
296500020225      *
296600960529     C                   ENDSR
296700150825     C*****************************************************************
296800150825     C*  Reperisce Peso/Volume o da CML o da Bollettato
296900150825     C*****************************************************************
297000150825     C     VOL_PES       BEGSR
297100150825     C*
297200150825     c     *like         define    arbvlb        wvol
297300150825     c     *like         define    arbpkb        wpes
297400150825     C     *LIKE         DEFINE    §qttpc        WTARA
297500150825      *
297600150825      * Prima cerca il volume fra bollettato e CML
297700150825     C     arbncR        ifeq      arbncl
297800150825      * se è considerata un'unica spedizione
297900150825     C                   z-add     ARBVLC        WVOL
298000150825     C                   else
298100150825     C*
298200150825     C     ARBVLC        ifge      ARBVLb
298300150825     C                   z-add     ARBVLC        WVOL
298400150825     C                   else
298500150825     C                   z-add     ARBVLb        WVOL
298600150825     C                   end
298700150825     C*
298800150825     C                   endif
298900150825     C* PESO VDL - TARA
299000150825     C     §QTTPC        MULT      arbncp        WTARA
299100150825     C                   Z-ADD     arbPKC        wpes
299200150825     c                   sub(h)    wtara         wpes
299300150825      *
299400150825      * Poi cerca il peso fra bollettato e CML
299500150825     C     arbncP        ifeq      arbncl
299600150825      * se è considerata un'unica spedizione
299700150825     C                   ELSE
299800150825     C*  altrimenti se il bollettato è superiore passo il Bollettato.
299900150825     C     wpes          iflt      arbPKb
300000150825     C                   Z-ADD     arbPKb        wpes
300100150825     C                   end
300200150825     C*
300300150825     C                   ENDIF
300400150825     C*
300500150825     C                   ENDSR
300600150825     C*--------------------------------------------------------------*
300700961023     OFNARB000  E            LIBER
300800961023     OFNARB000  E            AGGTRA
300900961023     O                       ARBFT3
301000961023     O                       ARBDT3
301100961112     OFNARB000  E            AGGFT3
301200961112     O                       ARBFT3
301300970124     OFNARB001  E            FITTIZ
301400961212     O*****************************************************************
301500961212     O* MANIFEST PER IL PARTNER
301600961212     O*****************************************************************
301700961213     OMANPTN    E            TESTA            02
301800041109     O                       D46(1)             112
301900961217     O                       STARAG              75
302000961217     O                       STPFAX              97
302100041109     O              N20                         112 '* PROVVISORIO'
302200041109     O               20                         112 '* DEFINITIVO '
302300961212     O          E            TESTA            03
302400931229     O                       ST(1)               66
302500041109     O                       D46(2)             112
302600961217     O                       UDATE8              54 '  /  /    '
302700961217     O                       UTIME               89 '  :  :  '
302800041109     O                       PAGE1         Z    111
302900961212     O          E            TESTA            04
303000961217     O                       ST(4)               66
303100041109     O                       D46(3)             112
303200961217     O                       RSUTM               63
303300961217     O                       STADLA             106
303400961212     O          E            TESTA            05
303500961217     O                       ST(3)               66
303600041109     O                       D46(4)             112
303700961217     O                       UDATE8              54 '  /  /    '
303800041109     O                       D82CMR             112
303900961212     O          E            TESTA            06
304000961217     O                       ST(19)              66
304100041109     O                       D46(5)             112
304200041109     O                       DATCMR              87 '  /  /    '
304300961212     O          E            TESTA          1 08
304400931229     O                       ST(6)               66
304500041109     O                       D46(6)             112
304600021204      *--------
304700931229     O          E            DET         1
304800100330     O              N21      §LB14               16
304900961129     O               21      §LB8                16
305000941222     O                       STARMN        Z   +  1
305100941222     O                       STARSM            +  1
305200941222     O                       STARSD            +  2
305300940111     O                       STATBO            +  1
305400940111     O                       ARBNCL        Z   +  2
305500961125     O                       PESPRT        4   +  2
305600041109     O                    N34PRTCAS        4   +  2
305700100223     O          E    24      DET         1
305800100223     O                       RagADD45            99
305900931229     O          E            DET         1
306000941221     O                       STADSP              16 '  /  /    '
306100130808     O***************        ARBCAM              36
306200130808     O***************        STALOM              53
306300130808     O                       STAINM              52
306400941222     O                       STAIND              79
306500961011     O                       ARBNAS             100
306600041109     O                    N34AR9VCA             112
306700130808     O          E    25N27   DET         1
306800100223     O                       IndADD10            64
306900130808     O          E    27N25   DET         1
307000130808     O                       IndMAdd10           37
307100130808     O          E    27 25   DET         1
307200130808     O                       IndMAdd10           37
307300130808     O                       IndADD10            64
307400941222     O          E            DET         1
307500940111     O                     02                    13 '* PARTIAL *'
307600130808     O***************        ARBRMA              30
307700130808     O***************                            37 'P.IVA:'
307800130808     O***************        MITCPI              53
307900130808     O                       ARBCAM              36
308000130808     O                       STALOM              53
308100950614     O                       ARBCAD              63
308200950614     O                       STALOD              84
308300950614     O                                           91 'P.IVA:'
308400950614     O                       DESCPI             107
308500100223     O          E    26      DET         1
308600100224     O                       CitADD15            69
308700130808     O          E            DET         1
308800130808     O                       ARBRMA              30
308900130808     O                                           37 'P.IVA:'
309000130808     O                       MITCPI              53
309100961220     O          E   N34      DET         1
309200041109     O                       STATIC             112
309300961121     O          E   N36      DET         1
309400950304     O         OR   N35
309500041109     O                    N35STANT1              75
309600041109     O                    N36STANT2             111
309700961121     O          E   N36      DET         0  1
309800961121     O         OR   N35
309900041109     O                    N35STANT1              75
310000041109     O                    N36STANT2             111
310100100223      * Contatto Nom./tel.
310200100223     O          E   N33      DET         1
310300100223     O                                           37 'Contact Name:'
310400100223     O                       STACNA              72
310500100223     O                                           80 'Tel.:'
310600100223     O                       STACTE             106
310700100223      *
310800941222     O* EXCPT FITTIZIA
310900941222     O          E          36DET         1  0
311000041109     O                                          112 ' '
311100021204     O* valore merce dichiarato
311200021204     O          E            VMDDET         1
311300021204     O                                           37 'Valore Merce:'
311400021204     O                       ARBVMD        2  +   1
311500021204     O                       ARBVAD           +   1
311600021204     O*
311700940111     O          E            TOTAL       1
311800940111     O                       ST(15)              66
311900041109     O                       D46(7)             112
312000940111     O          E            TOTAL       1
312100940111     O                       ST(17)              66
312200041109     O                       D46(8)             112
312300940111     O                       TOTSPE        Z     24
312400940111     O                       TOTNCL        Z     50
312500961125     O                       TOTPES        4     79
312600961125     O                       TOTVLF        4    104
312700940111     O          E            TOTAL       1
312800940111     O                       ST(15)              66
312900041109     O                       D46(7)             112
313000931229     O          E            FINE        1
313100931229     O                                           69 '***  END  OF  PRINT  ***'
313200961121     O*
313300970110     O*****************************************************************
313400970110     O* MANIFEST PER IL PARTNER IN FORMA SINTETICA
313500970110     O*****************************************************************
313600970110     OMANPTS    E            TESSIN           02
313700970110     O                       MAS(1)              66
313800970110     O                       MAS(2)             132
313900970110     O                       RSUT                20
314000970110     O                       SIMFEL              24
314100970110     O                       UDATE8             121 '  /  /    '
314200970110     O                       PAGE1         Z    132
314300970110     O          E            TESSIN           03
314400970110     O                       MAS(3)              66
314500970110     O                       MAS(4)             132
314600970110     O                       KNSIF               10
314700970110     O                       KNMUS               21
314800000208     O                       D82NFV              40
314900000208     O               22                          44 'BIS'
315000970110     O                       STADLS              65
315100970110     O                       STARGS             105
315200970114     O                       UTIME              119 '  :  :  '
315300970110     O          E            TESSIN         1 05
315400970110     O                       MAS(5)              66
315500970110     O                       MAS(7)             132
315600970110     O          E            DETSIN         1
315700970110     O                       ARBAAS               4
315800970110     O                       ARBLNP               8
315900970110     O                       ARBNRS        Z     12
316000970110     O                       ARBNSP        Z     20
316100970110     O                       ARBNAS              37
316200970110     O                       ARBRSM              74
316300020917     O                       ARBRMA              91
316400970110     O                       ARBRMN        Z    108
316500970110     O                       ARBNCL        Z    115
316600970110     O                       PESPRT        4    125
316700970110     O          E            TOTSIN         1
316800970110     O                       MAS(9)              66
316900970110     O                       MAS(9)             132
317000970110     O          E            TOTSIN         1
317100970110     O                       MAS(10)             66
317200970110     O                       MAS(11)            132
317300970110     O                       TOTSPE        Z     24
317400970110     O                       TOTNCL        Z     49
317500970110     O                       TOTPES        4     77
317600970110     O                       TOTVLF        4    103
317700970110     O          E            TOTSIN      1
317800970110     O                       MAS(9)              66
317900970110     O                       MAS(9)             132
318000970110     O          E            FINSIN      1
318100970110     O                                           69 '***   FINE  STAMPA   ***'
318200970110     O*
318300970110     O*
318400961212     O*****************************************************************
318500961212     O* MANIFEST SPEDIZIONI C.O.D.
318600961212     O*****************************************************************
318700961213     OMANCOD    E            TESCOD           02
318800041109     O                       D46(1)             112
318900961217     O                       STARAG              75
319000961217     O                       STPFAX              97
319100041109     O              N20                         112 '* PROVVISORIO'
319200041109     O               20                         112 '* DEFINITIVO '
319300961212     O          E            TESCOD           03
319400961121     O                       ST(1)               66
319500041109     O                       D46(2)             112
319600961217     O                       UDATE8              54 '  /  /    '
319700961217     O                       UTIME               89 '  :  :  '
319800041109     O                       PAGE2         Z    111
319900961212     O          E            TESCOD           04
320000961217     O                       ST(4)               66
320100041109     O                       D46(3)             112
320200961217     O                       RSUTM               63
320300961217     O                       STADLA             106
320400961212     O          E            TESCOD           05
320500961217     O                       ST(3)               66
320600041109     O                       D46(4)             112
320700961217     O                       UDATE8              54 '  /  /    '
320800041109     O                       D82CMR             112
320900961212     O          E            TESCOD           06
321000961217     O                       ST(19)              66
321100041109     O                       D46(5)             112
321200041109     O                       DATCMR              87 '  /  /    '
321300961216     O          E            TESCOD           07
321400041109     O                                          112 '* MANIFEST C.O.D.'
321500961212     O          E            TESCOD         1 08
321600961121     O                       ST(6)               66
321700041109     O                       D46(6)             112
321800961121     O          E            DETCOD      1
321900100330     O              N21      §LB14               16
322000961129     O               21      §LB8                16
322100961121     O                       STARMN        Z   +  1
322200961121     O                       STARSM            +  1
322300961121     O                       STARSD            +  2
322400961121     O                       STATBO            +  1
322500961121     O                       ARBNCL        Z   +  2
322600961125     O                       PESPRT        4   +  2
322700041109     O                    N34PRTCAS        4   +  2
322800961121     O          E            DETCOD      1
322900961121     O                       STADSP              16 '  /  /    '
323000130808     O**************         ARBCAM              36
323100130808     O**************         STALOM              53
323200130808     O                       STAINM              52
323300961121     O                       STAIND              79
323400961121     O                       ARBNAS             100
323500041109     O                    N34AR9VCA             112
323600961121     O          E            DETCOD      1
323700961121     O                     02                    13 '* PARTIAL *'
323800130808     O**************         ARBRMA              30
323900130808     O**************                             37 'P.IVA:'
324000130808     O**************         MITCPI              53
324100130808     O                       ARBCAM              36
324200130808     O                       STALOM              53
324300961121     O                       ARBCAD              63
324400961121     O                       STALOD              84
324500961121     O                                           91 'P.IVA:'
324600961121     O                       DESCPI             107
324700130808     O          E            DETCOD      1
324800130808     O                       ARBRMA              30
324900130808     O                                           37 'P.IVA:'
325000130808     O                       MITCPI              53
325100061127      * Contatto Nome/tel.
325200061127     O          E   N33      DETCOD      1
325300061127     O                                           37 'Contact Name:'
325400061127     O                       STACNA              72
325500061127     O                                           80 'Tel.:'
325600061127     O                       STACTE             106
325700061127     O
325800061127      *
325900961220     O          E   N34      DETCOD      1
326000041109     O                       STATIC             112
326100961121     O          E   N36      DETCOD      1
326200961121     O         OR   N35
326300041109     O                    N35STANT1              75
326400041109     O                    N36STANT2             111
326500961121     O          E   N36      DETCOD      0  1
326600961121     O         OR   N35
326700041109     O                    N35STANT1              75
326800041109     O                    N36STANT2             111
326900961121     O* EXCPT FITTIZIA
327000961121     O          E          36DETCOD      1  0
327100041109     O                                          112 ' '
327200961121     O          E            TOTCOD      1
327300961121     O                       ST(15)              66
327400041109     O                       D46(7)             112
327500961121     O          E            TOTCOD      1
327600961121     O                       ST(17)              66
327700041109     O                       D46(8)             112
327800961204     O                       CODSPE        Z     24
327900961204     O                       CODNCL        Z     50
328000961204     O                       CODPES        4     79
328100961204     O                       CODVLF        4    104
328200961121     O          E            TOTCOD      1
328300961121     O                       ST(15)              66
328400041109     O                       D46(7)             112
328500961121     O          E            FINCOD      1
328600961121     O                                           69 '***  END  OF  PRINT  ***'
328700961212     O*****************************************************************
328800961212     O* MANIFEST PER LA DOGANA
328900961212     O*****************************************************************
329000961213     OMANDGN    E            TESDGN           02
329100041109     O                       D46(1)             112
329200970117     O                       STARSX              75
329300970117     O                       D82DGX              97
329400041109     O*               N20               112 '* PROVVISORIO'
329500041109     O*                20               112 '* DEFINITIVO '
329600961212     O          E            TESDGN           03
329700961212     O                       ST(1)               66
329800041109     O                       D46(2)             112
329900961217     O                       UDATE8              54 '  /  /    '
330000961217     O                       UTIME               89 '  :  :  '
330100041109     O                       PAGE3         Z    111
330200961212     O          E            TESDGN           04
330300961217     O                       ST(4)               66
330400041109     O                       D46(3)             112
330500961217     O                       RSUTM               63
330600961217     O                       STADLA             106
330700961212     O          E            TESDGN           05
330800961217     O                       ST(3)               66
330900041109     O                       D46(4)             112
331000961217     O                       UDATE8              54 '  /  /    '
331100041109     O                       D82CMR             112
331200961212     O          E            TESDGN           06
331300961217     O                       ST(19)              66
331400041109     O                       D46(5)             112
331500041109     O                       DATCMR              87 '  /  /    '
331600961212     O          E            TESDGN         1 08
331700961212     O                       ST(6)               66
331800041109     O                       D46(6)             112
331900961212     O          E            DETDGN      1
332000100330     O              N21      §LB14               16
332100961212     O               21      §LB8                16
332200961212     O                       STARMN        Z   +  1
332300961212     O                       STARSM            +  1
332400961212     O                       STARSD            +  2
332500961212     O                       STATBO            +  1
332600961212     O                       ARBNCL        Z   +  2
332700961212     O                       PESPRT        4   +  2
332800041109     O                    N34PRTCAS        4   +  2
332900961212     O          E            DETDGN      1
333000961212     O                       STADSP              16 '  /  /    '
333100130808     O                       STAINM              52
333200130808     O*************          ARBCAM              36
333300130808     O*************          STALOM              53
333400130808     O                       STAINM              52
333500961212     O                       STAIND              79
333600961212     O                       ARBNAS             100
333700041109     O                    N34AR9VCA             112
333800961212     O          E            DETDGN      1
333900961218     O                     02                    13 '* PARTIAL *'
334000130808     O**************         ARBRMA              30
334100130808     O**************                             37 'P.IVA:'
334200130808     O**************         MITCPI              53
334300130808     O                       ARBCAM              36
334400130808     O                       STALOM              53
334500961212     O                       ARBCAD              63
334600961212     O                       STALOD              84
334700961212     O                                           91 'P.IVA:'
334800961212     O                       DESCPI             107
334900130808     O          E            DETDGN      1
335000130808     O                       ARBRMA              30
335100130808     O                                           37 'P.IVA:'
335200130808     O                       MITCPI              53
335300961220     O          E   N34      DETDGN      1
335400041109     O                       STATIC             112
335500961212     O          E   N36      DETDGN      1
335600961212     O         OR   N35
335700041109     O                    N35STANT1              75
335800041109     O                    N36STANT2             111
335900961212     O          E   N36      DETDGN      0  1
336000961212     O         OR   N35
336100041109     O                    N35STANT1              75
336200041109     O                    N36STANT2             111
336300961212     O* EXCPT FITTIZIA
336400961212     O          E          36DETDGN      1  0
336500041109     O                                          112 ' '
336600961212     O          E            TOTDGN      1
336700961212     O                       ST(15)              66
336800041109     O                       D46(7)             112
336900961212     O          E            TOTDGN      1
337000961212     O                       ST(17)              66
337100041109     O                       D46(8)             112
337200961212     O                       TOTSPE        Z     24
337300961212     O                       TOTNCL        Z     50
337400961212     O                       TOTPES        4     79
337500961212     O                       TOTVLF        4    104
337600961212     O          E            TOTDGN      1
337700961212     O                       ST(15)              66
337800041109     O                       D46(7)             112
337900961212     O          E            FINDGN      1
338000961212     O                                           69 '***  END  OF  PRINT  ***'
338100941222     O*
338200961212     O*****************************************************************
338300970114     O* LISTA DISCORDANZE VIA FAX
338400961212     O*****************************************************************
338500961213     OLISDIS    E            TESDIS           02
338600961217     O                       DIS(21)            132
338700961217     O                       STARAG              75
338800961217     O                       STPFAX              97
338900041109     O              N20                         112 '* PROVVISORIA'
339000041109     O               20                         112 '* DEFINITIVA '
339100961127     O          E            TESDIS           04
339200961125     O              N20      DIS(1)              66
339300961125     O              N20      DIS(2)             132
339400961125     O               20      DIS(17)             66
339500961125     O               20      DIS(18)            132
339600961125     O              N20      UDATE8              85 '  /  /    '
339700961129     O               20      UDATE8              67 '  /  /    '
339800961217     O                       UTIME              100 '  :  :  '
339900041109     O                       WPAG          Z    116
340000961127     O          E            TESDIS           06
340100961217     O                       DIS(3)              66
340200961217     O                       DIS(4)             132
340300961217     O                       D82NFV        Z     50
340400961217     O                       PARDFV              70 '  /  /    '
340500961217     O                       STADLA             115
340600961217     O          E            TESDIS           07
340700961217     O                       DIS(22)             66
340800961217     O                       DATCMR              55 '  /  /    '
340900961217     O                       D82CMR              91
341000961217     O          E            TESDIS           09
341100961122     O                       DIS(5)              66
341200961122     O                       DIS(8)             132
341300961217     O          E            TESDIS         1 10
341400961122     O                       DIS(6)              66
341500961122     O                       DIS(9)             132
341600961122     O          E            DETDIS         1
341700961127     O               16      KFLS                 3
341800961127     O               16      KLNA                 7
341900961129     O               16 17N18KNRS                10
342000961129     O               16 17N18                    11 '-'
342100961129     O               16   N18KNSC          Z     18
342200961129     O                  14   WNPS                22
342300961127     O               15      WPAAS               26
342400961127     O               15      WLNP                30
342500961127     O               15 17   KNRS                34
342600961127     O               15 17                       35 '-'
342700961127     O               15      WNSP          Z     42
342800961122     O                       KCEV                47
342900961122     O                                           48 '-'
343000970114     O                       DCEVE              108
343100961128     O                 N14   WFIL               112
343200961129     O                 N14   WNPS               115
343300961125     O          E            DETDI2         1
343400961202     O*                        COSPRT    68
343500961127     O                       WDTPA1              79 '  /  /    '
343600961129     O*
343700961129     O          E            RIGCNS         1
343800961129     O               12                           8 'PARCELS:'
343900961129     O               17      KNRS                10
344000961129     O               17                          11 '-'
344100961129     O                       NSC(1)              19
344200961129     O                       NSC(2)              27
344300961129     O                       NSC(3)              35
344400961129     O                       NSC(4)              43
344500961129     O                       NSC(5)              51
344600961129     O                       NSC(6)              59
344700961129     O                       NSC(7)              67
344800961129     O                       NSC(8)              75
344900961129     O                       NSC(9)              83
345000961129     O                       NSC(10)             91
345100961129     O                       NSC(11)             99
345200961129     O                       NSC(12)            107
345300961129     O                       NSC(13)            115
345400961127     O          E            FIT         1
345500961122     O*                                     ' '
345600961122     O          E            FINDIS      1
345700961127     O               20                          75 '***  END OF PRINT  ***'
345800961127     O              N20                          75 '***  FINE STAMPA  ***'
345900961217     O          E            DETDI3           19
346000961125     O              N20      DIS(15)             66
346100961125     O              N20      DIS(16)            132
346200961125     O               20      DIS(19)             66
346300970114     O*****************************************************************
346400970114     O* LISTA DISCORDANZE SU CARTA
346500970114     O*****************************************************************
346600970114     OLISDIC    E            TESDIC           02
346700970114     O                       DIS(21)            132
346800970114     O                       STARAG              75
346900970114     O                       STPFAX              97
347000041109     O              N20                         112 '* PROVVISORIA'
347100041109     O               20                         112 '* DEFINITIVA '
347200970114     O          E            TESDIC           04
347300970114     O                       DIS(1)              66
347400970114     O                       DIS(2)             132
347500970114     O                       UDATE8              85 '  /  /    '
347600970114     O                       UTIME              100 '  :  :  '
347700970114     O                       WPAG1         Z    116
347800970114     O          E            TESDIC           06
347900970114     O                       DIS(3)              66
348000970114     O                       DIS(4)             132
348100970114     O                       D82NFV        Z     50
348200970117     O               22                          54 'BIS'
348300970114     O                       PARDFV              70 '  /  /    '
348400970114     O                       STADLA             115
348500970114     O          E            TESDIC           07
348600970114     O                       DIS(22)             66
348700970114     O                       DATCMR              55 '  /  /    '
348800970114     O                       D82CMR              91
348900970114     O          E            TESDIC           09
349000970114     O                       DIS(5)              66
349100970114     O                       DIS(8)             132
349200970114     O          E            TESDIC         1 10
349300970114     O                       DIS(6)              66
349400970114     O                       DIS(9)             132
349500970114     O          E            DETDIC         1
349600970114     O               16      KFLS                 3
349700970114     O               16      KLNA                 7
349800970114     O               16 17N18KNRS                10
349900970114     O               16 17N18                    11 '-'
350000970114     O               16   N18KNSC          Z     18
350100970114     O                  14   WNPS                22
350200970114     O               15      WPAAS               26
350300970114     O               15      WLNP                30
350400970114     O               15 17   KNRS                34
350500970114     O               15 17                       35 '-'
350600970114     O               15      WNSP          Z     42
350700970114     O                       KCEV                47
350800970114     O                                           48 '-'
350900970114     O                       DCEV               108
351000970114     O                 N14   WFIL               112
351100970114     O                 N14   WNPS               115
351200970114     O          E            DETDC2         1
351300970114     O*                        COSPRT    68
351400970114     O                       WDTPA1              79 '  /  /    '
351500970114     O*
351600970114     O          E            RIGCNC         1
351700970114     O               12                           8 'PARCELS:'
351800970114     O               17      KNRS                10
351900970114     O               17                          11 '-'
352000970114     O                       NSC(1)              19
352100970114     O                       NSC(2)              27
352200970114     O                       NSC(3)              35
352300970114     O                       NSC(4)              43
352400970114     O                       NSC(5)              51
352500970114     O                       NSC(6)              59
352600970114     O                       NSC(7)              67
352700970114     O                       NSC(8)              75
352800970114     O                       NSC(9)              83
352900970114     O                       NSC(10)             91
353000970114     O                       NSC(11)             99
353100970114     O                       NSC(12)            107
353200970114     O                       NSC(13)            115
353300970114     O          E            FITC        1
353400970114     O*                                     ' '
353500970114     O          E            FINDIC      1
353600970114     O                                           75 '***  FINE STAMPA  ***'
353700970114     O          E            DETDC3           19
353800970114     O                       DIS(15)             66
353900970114     O                       DIS(16)            132
354000970110**
354100961217                           Date printed    :XX/XX/XXXX          Ti  1
354200961217me printed    :XX:XX:XX               Page:  xxxx                   2
354300961216                           Shipment date   :xx/xx/xxxx          CM  3
354400961216                           Exporting branch:1234567890123456789 Im  4
354500961216porting branch:1234567890123456789012345                            5
354600960223    Shipmentno    Rif.Exp  Exporter                   Consignee     6
354700960223    XXXXXXXXXXXX XXXXXXXXX xxxxxxxxxxxxxxxxxxxxxxxxx  XXXXXXXXXXXX  7
354800961011        XX/XX/XX           xxxxxxxxx xxxxxxxxxxxxxxxx xxxxxxxxxxxx  8
354900961011  * PARTIAL *  xxxxxxxxxxxxxxx P.IVA:xxxxxxxxxxxxxxxx xxxxxxxxx xx  9
355000961125              Imp Parcels   Weight    C.O.D.                       10
355100961011XXXXXXXXXXXXX xxx  xxxxx  xxxxxx,x  xxxxxxxxxx,xxx                  1
355200961011XXxxxxxxxxxxx      123456789012345             xxx                  2
355300961011XXxxxxxxxxxxxxxxxx P.IVA: xxxxxxxxxx                                3
355400940131
355500940111------------------------------------------------------------------  5
355600940111--------------------------------------------------                  6
355700961125  Total Shipments: xxxxx    Total Parcels: xxxxxxx    Total Weight  7
355800961125: xxxxxxxxx,x   Total Volume: xxxx,xxx                              8
355900961216                                                                CM 19
356000961216R Number      :12345678901234567890123456789012345                 20
356100961216R Date        :xx/xx/xxxx                                          21
356200931229**
356300961217                          ** LISTA DISCORDANZE SPEDIZIONI PER L'ES  1
356400961223TERO DEL XX/XX/XXXX **    XX:XX:XX       PAG. XXXX
356500961217                           FOGLIO DEFLUENZA :XXXXX      DEL XX/XX/  3
356600961217XXXX    LINEA DI ARRIVO:1234567890123456789012345                   4
356700961128  P  A  R  C  E  L  S    S H I P M E N T                            5
356800961122LHD LHA  NUMBER    O.R. YR LHD   NUMBER                             6
356900961121XXX XXX XX XXXXXXX  XX  XX XXX  XX XXXXXXX  XXX-123456789012345678  7
357000961128                                           SPUNTA                   8
357100961128                                           P.O O.R                  9
357200961128901234567890123456789012345678901234567890 XXX XX                  10
357300961127                                                123456789012345678 11
35740096112790 XX/XX/XXXX                                                      12
357500961129PARCELS:XX-1234567,1234567,1234567,1234567,1234567,1234567,1234567 13
357600961129,1234567,1234567,1234567,1234567,1234567,1234567                   14
357700970121                **  N E S S U N A   D I S C O R D A N Z A   R I S  15
357800970121C O N T R A T A  **                                                16
357900961217                                *** DISCREPANCY LIST DTD XX/XX/XXX 17
358000961223X ***                     XX:XX:XX       PAG. XXXX                 18
358100961212                             **  N O   D I S C R E P A N C Y  **   19
358200961217                           123456789012345678901234567890123456789 20
358300961219012345678 *Fax:1234567890123456                                    21
358400961217                           CMR..............:XX/XX/XXXX 1234567890 22
3585009612171234567890123456789012345                                          23
358600041109** D46
358700041109_________ *Fax:________________                                     1
358800041109me printed    :XX:XX:XX           Page:  xxxx                       2
358900041021porting branch:_________________________                            3
359000041109R Number  :___________________________________                      4
359100041109R Date    :xx/xx/xxxx                                               5
359200041021              Imp Parcels   Weight    C.O.D.                        6
359300041109----------------------------------------------                      7
359400041021: xxxxxxxxx,x   Total Volume: xxxx,xxx                              8
359500041021                                                                    9
359600041021                                                                   10
359700961213** CMD
359800961213OVRPRTF MANPTN QSYSPRT SAVE(*YES)                                    1
359900971230OVRPRTF MANPTN QSYSPRT OUTQ(FAXIN     ) SAVE(*YES)                   2
360000961213OVRPRTF MANCOD QSYSPRT SAVE(*YES)                                    3
360100971230OVRPRTF MANCOD QSYSPRT OUTQ(FAXIN     ) SAVE(*YES)                   4
360200961213OVRPRTF MANDGN QSYSPRT SAVE(*YES)                                    5
360300971230OVRPRTF MANDGN QSYSPRT OUTQ(FAXIN     ) SAVE(*YES)                   6
360400990512OVRPRTF LISDIC QSYSPRT SAVE(*YES)       HOLD(*YES)                   7
360500971230OVRPRTF LISDIS QSYSPRT OUTQ(FAXIN     ) SAVE(*YES)                   8
360600970110OVRPRTF MANPTS QSYSPRT SAVE(*YES)                                    9
360700971230OVRPRTF MANPTN QSYSPRT OUTQ(FAXOUT    ) SAVE(*YES) FORMTYPE('SAVEDI')10
360800971230                                                                     11
360900971230OVRPRTF LISDIS QSYSPRT SAVE(*YES)                                    12
361000970110**
361100970110XXXXXXXXXXXXXXXXXXXX/XXX                  ** MANIFEST SPEDIZIONI E   1
361200970110XPORT SINTETICO **               TRTC83R1    XX/XX/XXXX  PAG. XXXX   2
361300970121XXXXXXXXXX XXXXXXXXXX  Foglio Defl.XXXXX      Lna:123456789012345    3
361400970115 Partner:123456789012345678901234567890      XX:XX:XX                4
361500970110Anno Lnp Ser  Numero  Natura merce     Ragione sociale mittente      5
361600970110XXXX XXX  XX XXXXXXX  123456789012345  123456789012345678901234567   6
361700970110          Rif.mitt.alfab.  Rif.mitt.numer.  Colli    Peso            7
36180097011089012345  123456789012345  123456789012345  12345  123456,7          8
361900970110------------------------------------------------------------------   9
362000970110Totale Spedizioni: xxxxx    Totale Colli: xxxxxxx    Totale Peso:   10
362100970110xxxxxxxxx,x   Totale Volume: xxxx,xxx                               11
