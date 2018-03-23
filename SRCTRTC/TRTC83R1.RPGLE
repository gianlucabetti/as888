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
031700180309     D FEDEX_UK        s              1    inz('S')
031800180309     D CODFEDEX_UK     s                   LIKE(D82CPT) inz(3500011)
031900140311     d flagHDE         s              1    inz('H')
032000140311      **
032100140311      ** ______
032200020225
032300090402     ITITAS000      81
032400090402     ITITAS010      82
032500090402     ITITASP00      83
032600090402     I
032700090402     ITITA4000      84
032800090402     ITITA4P00      85
032900020225      ****************************************************************
033000020225      *  RIEPILOGO INDICATORI
033100020225      ***************************************************************
033200060918      * 01    - un collo della spedizione in  IMA
033300020225      * 02    - STAMPO 'PARTIAL' NEL MANIFEST SE INOLTRO PARZIALE
033400020225      * 04    - STAMPATO QUALCOSA NEL MANIFEST PER IL PARTNER
033500020225      * 05    - STAMPATO QUALCOSA NEL MANIFEST SPEDIZIONI C.O.D.
033600020225      * 06    - STAMPATO QUALCOSA NEL MANIFEST PER LA DOGANA
033700020225      * 08    - STAMPATO QUALCOSA NELLA LISTA DISCORDANZE
033800020225      * 09    - ON: COLLO A TERRA
033900020225      * 12    - PRIMA RIGA DI STAMPA ELENCO SEGNACOLLI PER DISC.4
034000020225      * 14    - ON - IN LISTA DISCORD. NON STAMPO IL P.O. SPUNTA COLLO
034100020225      * 15    - ON - HO LA KEY PER SPEDIZIONE PER STAMPA
034200020225      * 16    - ON - HO LA KEY PER SEGNACOLLO PER STAMPA
034300020225      * 17    - ON - HO LA SERIE
034400060913      * 18    - ON - STAMPO DISCORDANZA E4  - E8
034500020225      * 20    - ON - ELABORAZIONE DEFINITIVA
034600020225      * 22    - ON - FOGLIO VIAGGIO BIS
034700020226      * 31/32 - DI COMODO
034800061127      * 33    - STAMPO I DATI RELATIVI AL CONTATTO NOME/TEL
034900020225      * 34    - STAMPO I DATI RELATIVI AL C/ASSEGNO
035000020225      * 35/36 - STAMPO I DATI RELATIVI ALLE NOTE
035100020225      * 42    - CREO SPUNTA DA CAMPI DI FNANM
035200020225      * 43    - CREO SPUNTA DA CAMPI DI FNART
035300020225
035400020225      *****************************************************************
035500020225      * CICLO PRINCIPALE
035600020225      *****************************************************************
035700020225      * SE FV GIA' CHIUSO VADO A FINE PGM
035800020808     ***C     D82NFV        CHAIN     FNFGV01L
035900020808     C     K02FGV01      CHAIN     FNFGV01L
036000040317     C                   if        %found(FNFGV01L) and FGVFCF = 'S'
036100970127     C                   GOTO      FINEL
036200020225     C                   endif
036300020225      *
036400020225      * Reperisco dati Partner in base al codice cliente
036500960529     C                   EXSR      DATPAT
036600041020      *
036700020225      * Ovrprtf su file Printer
036800940131     C                   EXSR      OVRPRT
036900020225      *
037000020225      * GIRO SU ARB PER SPEDIZIONI DEL MANIFEST E DISCORDANZA 4
037100120823      *                *------------------*
037200961111     C                   EXSR      REDSPE
037300120823      *                *------------------*
037400020225      * GIRO SU ART E ANOMALIE PER LE ALTRE DISCORDANZE E AGGIORNARE
037500020225      * ARTDTR PER I COLLI CHE PARTONO
037600020225      * Se elaborazione definitiva o se richiesta lista discordanze
037700020225     c                   IF        *in20 = *on  or  D82lds = 'SI'
037800961113     C                   EXSR      CTRDIS
037900020225     C                   ENDIF
038000020225      *
038100020225      * FINE STAMPE
038200020225    1C     *IN08         IFEQ      *ON                                          LISTA DISCORD.
038300120821     c     d82ftt        ifne      FEDEX
038400040423
038500040423     c                   if        FAX_Disc_Est = 'S'
038600020225     C   20              EXCEPT    FINDIS                                       via fax
038700040423     c                   end
038800040423
038900020301     c                   endif
039000020225     C                   EXCEPT    FINDIC                                       su carta
039100020225   x1C                   ELSE
039200020225    2C     D82LDS        IFEQ      'SI'
039300970313     C* Al partner trasmetto LISDIS vuoto solo se ho stampato qualcosa
039400020225     C* nel manifest (solo se definitivo
039500020301     c                   if        *IN04 = *on AND *IN20 = *ON
039600120821     c                             and d82ftt <> FEDEX
039700040423
039800040423     c                   if        FAX_Disc_Est = 'S'
039900020225     C                   ADD       1             WPAG
040000020225     C                   EXCEPT    TESDIS
040100020225     C                   EXCEPT    DETDI3
040200040423     c                   end
040300040423
040400020225    3C                   endif
040500970114     C                   ADD       1             WPAG1
040600970114     C                   EXCEPT    TESDIC
040700970114     C                   EXCEPT    DETDC3
040800020225    2C                   ENDIF
040900020225    1C                   ENDIF
041000020225      *
041100961213     C     *IN04         IFEQ      *ON                                          MANIFEST PARTN
041200020225      *
041300020225     C                   if        *IN20 = *ON                                  MANIFEST PARTN
041400020225     C   OF              EXCEPT    TESTA
041500020225     C                   EXCEPT    TOTAL
041600020225     C                   EXCEPT    FINE
041700020225     c                   else
041800020225     C   OC              EXCEPT    TESSIN
041900020225     C                   EXCEPT    TOTSIN
042000020225     C                   EXCEPT    FINSIN
042100020225     c                   endif
042200020225      *
042300970110     C                   ELSE
042400020225     C                   if        D82STB = 'SI' and *in20 = *off
042500020225     C                   EXCEPT    TESSIN
042600020225     C                   EXCEPT    FINSIN
042700020225     C                   endif
042800970115     C                   ENDIF
042900961212     C*
043000961213     C     *IN06         IFEQ      *ON                                          MANIFEST DOG
043100120821     c     d82ftt        andne     FEDEX
043200961212     C   OD              EXCEPT    TESDGN
043300961212     C                   EXCEPT    TOTDGN
043400961212     C                   EXCEPT    FINDGN
043500961212     C                   ENDIF
043600020225      *
043700961213     C     *IN05         IFEQ      *ON                                          MANIFEST COD
043800120821     c     d82ftt        andne     FEDEX
043900961121     C   OG              EXCEPT    TESCOD
044000961121     C                   EXCEPT    TOTCOD
044100961121     C                   EXCEPT    FINCOD
044200961121     C                   ENDIF
044300020225      *
044400020225      * Per elaborazione definitiva
044500020225      * Richiamo scrittura file x ultima volta con numero sped = 0
044600020225     C                   SELECT
044700020225     C                   WHEN      *IN20 = *off
044800020917      * EUROEXPRESS in sede
044900051221      *   o DPD x SEUR (EDI)
045000120821     c                   WHEN      D82FTT = EDINOPRINT
045100120821     c                              or
045200120821     c                             D82FTT = EDIESTAMPA
045300120821     c                              or
045400120821     c                             D82FTT = SCANDPDEDI
045500020225     C                   clear                   D82AAS
045600020225     C                   clear                   D82LNP
045700020225     C                   clear                   D82NRS
045800020225     C                   clear                   D82NSP
045900020225     C                   MOVEL     TRTC82DS      KPJBU
046000180309      *
046100960530     C                   CALL      'TRTC83R2'
046200960530     C                   PARM                    KPJBA
046300120823      **
046400120821      * GEL   (Scrittura del file NET EXPRESS EUROPE)
046500120821     c                   WHEN      D82FTT = NETEXP
046600120821     C                   clear                   F83AAS
046700120821     C                   clear                   F83LNP
046800120821     C                   clear                   F83NRS
046900120821     C                   clear                   F83NSP
047000120821     c                   clear                   f83dfv
047100120821     C                   clear                   KPJBU
047200120821     C                   MOVEL     TRTC83DS1     KPJBU
047300120905     C                   CALL      'TRTC83G2R'
047400120821     C                   PARM                    KPJBA
047500120821      * REIMPOSTO KPJBU "ORIGINALE"
047600120821     C                   MOVEL     WKPJBU        KPJBU
047700120821      *
047800020225      * FEDEX
047900120821     c                   WHEN      D82FTT = FEDEX
048000020228     C                   clear                   F83AAS
048100020228     C                   clear                   F83LNP
048200020228     C                   clear                   F83NRS
048300020228     C                   clear                   F83NSP
048400020228     c                   clear                   f83dfv
048500020228     C                   MOVEL     TRTC83DS1     KPJBU
048600020225     C                   CALL      'TRTC83R21'
048700971230     C                   PARM                    KPJBA
048800020304      * REIMPOSTO KPJBU "ORIGINALE"
048900020304     C                   MOVEL     WKPJBU        KPJBU
049000981106      *
049100020225     C                   ENDSL
049200020225      *
049300020225      * SCRIVO RECORD IN WORK FILE PER LA LNA APPENA ELABORATA
049400970527     C                   CLEAR                   TREXP000
049500970527     C  N20              MOVEL     'P'           TRETRC
049600120821     C   20              MOVEL     'D'           TRETRC
049700970527     C                   MOVE      D82NFV        TRENFV
049800970527     C                   MOVE      SIMFEL        TRELNP
049900970527     C                   MOVE      LNAEXP        TRELNA
050000020225      * MEMORIZZO TOTALI SOLO SE PROVVISORIO O SE ELABORAZ.PER DOGANA
050100020225      * (INFATTI LA STAMPA DEI TOTALI DEL FV SERVE PER LA COMPILAZIONE
050200020225      * DEL MODULO CMR E DA TRASMETTERE ALLA DOGANA INSIEME AL MANIFEST
050300020301     C                   IF        *IN20 = *OFF  or (D82DGN = 'SI' and
050400120821     C                             d82ftt <> FEDEX)
050500970527     C                   Z-ADD     TOTPES        TRETOP
050600050520     C                   Z-ADD     totvlf        TRETOv
050700970527     C                   Z-ADD     TOTNCL        TRETOC
050800020225     C                   ENDIF
050900020225      *
051000970527     C                   WRITE     TREXP000                             31
051100020225      *
051200020225      * Reimposto LNA salvata nella DS
051300970123     C                   MOVE      LNAEXP        D82LNP
051400020225      *
051500970127     C     FINEL         TAG
051600020225      *
051700020225      *
051800020225     C                   eval      *inLR = *ON
051900960529     C*****************************************************************
052000970114     C*  Reperisco i dati del partner
052100960529     C*****************************************************************
052200960529     C     DATPAT        BEGSR
052300960529     C*
052400961127     C                   MOVEL     *ALL'?'       STADLA           25
052500960529     C*  Decodifico lna
052600020226     C     LNAEXP        CHAIN     AZORG
052700020226     C                   movel     orgde3        OG143
052800020226     C                   MOVEL(P)  ORGDES        STADLA
052900970110     C*
053000970110     C                   MOVEL     STADLA        STADLS           15
053100970117     C*  Reperisco numero fax e ragione sociale partner
053200961127     C*
053300961127     C     KIND          CHAIN     CNACO00F                           31
053400961217     C  N31              MOVEL     ACORAG        STARAG           48
053500961127     C   31              CLEAR                   STARAG
053600970110     C                   MOVEL     STARAG        STARGS           30
053700961127     C*
053800961122     C                   CLEAR                   STPFAX
053900961122     C     KIND          CHAIN     CNIND00F                           31
054000961122     C     *IN31         IFEQ      *OFF
054100020225     C                   CLEAR                   TRUL42DS
054200961122     C                   MOVEL     INDTLF        D42FAX
054300961122     C                   CALL      'TRUL42R'
054400020225     C                   PARM                    TRUL42DS
054500961122     C     D42ERR        IFEQ      ' '
054600961122     C                   MOVEL     INDTLF        STPFAX
054700961122     C                   END
054800961216     C                   END
054900970117     C*
055000970117     C* PER COMODITA' MUOVO RAGIONE SOCIALE DOGANA IN CAMPO LUNGO 48
055100970117     C                   MOVEL     D82RSX        STARSX           48
055200041020     C*
055300041020     C* Nuovo Concetto:
055400041020     C*  L'indirizzo E-mail è prioritario rispetto al FAX ossia se presente
055500041020     C*  in anagrafica oltre al nr.FAX l'invio del MANIFEST non viene più
055600041020     C*  eseguito mediante FAX ma per posta Elettronica.
055700041020     C* Prende indirizzo e-mail del Partner in anagrafica Note.
055800041020     C*
055900041020     C*
056000960529     C* Pulisco  i campi di totale x partner
056100960529     C                   CLEAR                   TOTSPE
056200960529     C                   CLEAR                   TOTNCL
056300961122     C                   CLEAR                   TOTPES
056400960529     C                   CLEAR                   TOTVLF
056500961204     C                   CLEAR                   CODSPE
056600961204     C                   CLEAR                   CODNCL
056700961204     C                   CLEAR                   CODPES
056800961204     C                   CLEAR                   CODVLF
056900960530     C*
057000960530     C                   ENDSR
057100960529     C*****************************************************************
057200960529     C* OVRPRTF: Ovrprtf file di stampa
057300960529     C*****************************************************************
057400960529     C     OVRPRT        BEGSR
057500021009     C*
057600041021     C                   Exsr      Code_FAX
057700041020     C*
057800971230     C                   Z-ADD     69            LUNG
057900961213     C**
058000961213     C* OVRPRTF MANIFEST PER PARTNER
058100961213     C**
058200961213    1C     D82STB        IFEQ      'SI'
058300970205     C* E l a b o r a z i o n e   d e f i n i t i v a
058400961213    2C     *IN20         IFEQ      *ON
058500041020     C**
058600970205    3C                   SELECT
058700041020     C**
058800970224     C* Se ho trovato il n.fax, ed è previsto l'invio della stampa
058900020917     c* (ftt = ' ' )
059000970205     C* --> metto spool in faxin per invio fax in automatico
059100970205     C     STPFAX        WHENNE    *BLANKS
059200120821     C     D82FTT        ANDEQ     EDIESTAMPA
059300120821     C     STPFAX        orNE      *BLANKS
059400120821     C     D82FTT        ANDEQ     NETEXP
059500971230     C                   MOVEL     CMD(2)        CMDPRT
059600021009     C                   MOVEL     FaxInOutQ     OUTQPR
059700971230     C                   MOVEL     CMDPRT        COMMAN
059800041020     C**
059900020917     C* Se è prevista solo la trasmiEDI  (FTT='W')
060000970205     c* --> metto spool in faxout per avere una copia di sicurezza
060100970205     C*     nel caso la trasmissione non dovesse riuscire
060200120821     C     D82FTT        WHENEQ    EDINOPRINT
060300120821     C     D82FTT        OREQ      FEDEX
060400120821     C     D82FTT        OREQ      SCANDPD
060500120821     C     D82FTT        OREQ      SCANDPDEDI
060600971230     C                   MOVEL     CMD(10)       CMDPRT
060700021009     C                   MOVEL     FaxOutOutQ    OUTQPR
060800971230     C                   MOVEL     CMDPRT        COMMAN
060900041020     C**
061000970205     C* altrimenti lo lascio nella coda di stampa di default
061100970205     C                   OTHER
061200961213     C                   MOVEL     CMD(1)        COMMAN
061300970205    3C                   ENDSL
061400041021     C**
061500041021     C**  però adesso bisogna fare Attenzione:  --> E-MAIL !!!!
061600041021     C**    se c'è l'indirizzo E-mail e prima era previsto l'invio con il FAX,
061700041021     C**    non deve essere più inviato il FAX ma tutto viene trasmesso con e-mail.
061800041021     C**  Il TRUL44R esegue anche l'OVRPRTF x e-mail.
061900120821     C     D82FTT        ifeq      EDIESTAMPA
062000120821     C     D82FTT        oreq      PRINTNOEDI
062100120821     C     D82FTT        oreq      NETEXP
062200041021     c                   clear                   trul44ds
062300050913     c                   clear                   trtcm1ds
062400041021     c                   eval      d44pgm = 'TRTC82R'
062500041021     c                   move      d82cpt        d44ksc
062600041021     c                   eval      d44apl = 'C'
062700041118     c                   eval      d44nk1 = '0151' + %editc(D82CPT:'X')
062800041021     c                   eval      d44tnt = '86'
062900041021     c                   eval      d44scope='*JOB'
063000041021     c                   eval      d44ovr = 'S'
063100120305     c***66              eval      d44obj = '*OBJM*Bartolini Shipment '+
063200120305     c   66              eval      d44obj = '*OBJM*Bartolini-BRT Shipment '+
063300110609     c                             'Manifest del ' + %editw(PARDFV:'  /  /    ')
063400110609     c                              + ' - ' + %trim(Starag)
063500120305     c**N66              eval      d44obj = '*OBJM*Bartolini-BRT Shipment '+
063600120305     c  N66              eval      d44obj = '*OBJM*BRT Shipment '+
063700110609     c                             'Manifest del ' + %editw(PARDFV:'  -  -    ')
063800110609     c                              + ' - ' + %trim(Starag)
063900041021     c                   eval      d44prtf = 'MANPTN'
064000041021     c                   call      'TRUL44R'
064100041021     c                   parm                    trul44ds
064200050913     c                   parm                    trtcm1ds
064300041025      **  Se tutto OK salta la QCMDEXEC
064400041025      **   con la vecchia OVRPRTF
064500041109     c     d44err        ifeq      *blank
064600041109     c                   eval      Email_PTN='S'
064700041109     c                   goto      gia_fatta
064800041109     c                   end
064900041021     c                   end
065000041021     C**
065100041109     C**  Viene eseguita se non è stata mandata via E-mail
065200041109     C**   notare il TAG seguente
065300970115     C                   CALL      'QCMDEXC'                            90
065400970115     C                   PARM                    COMMAN           80
065500970115     C                   PARM                    LUNG             15 5
065600041021     c     gia_fatta     tag
065700041020     C**
065800041020     C**  quindi apre il Manifest x il Partner
065900970110     C                   OPEN      MANPTN
066000041020     C**
066100970110   X2C                   ELSE
066200970205     C* E l a b o r a z i o n e   p r o v v i s o r i a
066300970110     C* Se elaborazione provvisoria stampo il manifest in forma sinte-
066400970110     C* tica
066500970110     C                   MOVEL     CMD(9)        COMMAN
066600970110     C                   CALL      'QCMDEXC'                            90
066700970110     C                   PARM                    COMMAN           80
066800970110     C                   PARM                    LUNG             15 5
066900970110     C                   OPEN      MANPTS
067000970110    2C                   END
067100961213    1C                   END
067200041020     C** --------------
067300041020     C**
067400961213     C* OVRPRTF MANIFEST PER DOGANA
067500020301     c* Per Fedex non devo mai stampare il manifest per dogana
067600961213     C**
067700961213    1C     D82DGN        IFEQ      'SI'
067800120821     c     D82FTT        andne     FEDEX
067900041020     C**
068000970114     C* Se ho trovato il n.fax --> metto in faxin per invio fax autom.
068100970117    2C     D82DGX        IFNE      *BLANKS
068200041021     C**
068300971230     C                   MOVEL     CMD(6)        CMDPRT
068400021009     C                   MOVEL     FaxInOutQ     OUTQPR
068500971230     C                   MOVEL     CMDPRT        COMMAN
068600041020     C**
068700961213   X2C                   ELSE
068800961213     C* altrimenti lo lascio nella coda di stampa di default
068900961213     C                   MOVEL     CMD(5)        COMMAN
069000961213    2C                   END
069100041021     C**
069200041021     C**  però adesso bisogna fare Attenzione:  --> E-MAIL !!!!
069300041021     C**    se c'è l'indirizzo E-mail e prima era previsto l'invio con il FAX,
069400041021     C**    non deve essere più inviato il FAX ma tutto viene trasmesso con e-mail.
069500041021     C**  L'OVRPRTF è in base ai parametri restituiti dal TRUL44R.
069600041021     C**      è possibile eseguire tramite il TRUL44R l'OVRPRTF necessaria.
069700041021     c                   clear                   trul44ds
069800050913     c                   clear                   trtcm1ds
069900041021     c                   eval      d44pgm = 'TRTC82R'
070000041025     c                   eval      d44cl = '200'
070100041021     c                   move      D82CPT        d44ksc
070200041021     c                   eval      d44scope='*JOB'
070300041021     c                   eval      d44ovr = 'S'
070400120305     c***66              eval      d44obj = '*OBJM*Bartolini Shipment '+
070500120305     c   66              eval      d44obj = '*OBJM*Bartolini-BRT Shipment '+
070600110609     c                             'Manifest del ' + %editw(PARDFV:'  /  /    ')
070700110609     c                              + ' - ' + %trim(D82RSX)
070800120305     c**N66              eval      d44obj = '*OBJM*Bartolini-BRT Shipment '+
070900120305     c  N66              eval      d44obj = '*OBJM*BRT Shipment '+
071000110608     c                             'Manifest del ' + %editw(PARDFV:'  .  .    ')
071100110608     c                              + ' - ' + %trim(D82RSX)
071200041021     c                   eval      d44prtf = 'MANDGN'
071300041021     c                   call      'TRUL44R'
071400041021     c                   parm                    trul44ds
071500050913     c                   parm                    trtcm1ds
071600041025      **  Se tutto OK salta la QCMDEXEC
071700041025      **   con la vecchia OVRPRTF
071800041109     c     d44err        ifeq      *blank
071900041109     c                   eval      Email_DGN='S'
072000041109     c                   goto      salta_OVRPRTF
072100041109     c                   end
072200041020      **
072300041109     C**  Viene eseguita se non è stata mandata via E-mail
072400041109     C**   notare il TAG seguente
072500970115     C                   CALL      'QCMDEXC'                            90
072600970115     C                   PARM                    COMMAN           80
072700970115     C                   PARM                    LUNG             15 5
072800041021     c     salta_OVRPRTF tag
072900041020      **
073000961213     C                   OPEN      MANDGN
073100961213    1C                   END
073200961213     C**
073300961213     C* OVRPRTF MANIFEST PER SPEDIZIONI C.O.D.
073400020301     c* Per fedex non stampo mai il manifest delle sped. c.o.d.
073500961213     C**
073600961213    1C     D82COD        IFEQ      'SI'
073700120821     c     D82FTT        ANDNE     FEDEX
073800041020     C**
073900961213     C* Se elaboraz.defin. e ho trovato il n.fax --> metto spool in
074000961213     C* in faxin per invio fax in automatico
074100961213    2C     *IN20         IFEQ      *ON
074200961213     C     STPFAX        ANDNE     *BLANKS
074300041021     C**
074400971230     C                   MOVEL     CMD(4)        CMDPRT
074500021009     C                   MOVEL     FaxInOutQ     OUTQPR
074600971230     C                   MOVEL     CMDPRT        COMMAN
074700041020     C**
074800961213   X2C                   ELSE
074900961213     C* altrimenti lo lascio nella coda di stampa di default
075000961213     C                   MOVEL     CMD(3)        COMMAN
075100961213    2C                   END
075200041021     C**
075300041021     C**  però adesso bisogna fare Attenzione:  --> E-MAIL !!!!
075400041021     C**    se c'è l'indirizzo E-mail e prima era previsto l'invio con il FAX,
075500041021     C**    non deve essere più inviato il FAX ma tutto viene trasmesso con e-mail.
075600041021     C**  Per l'OVRPRTF tramite posta elettronica si utilizza il TRUL44R
075700041021     C**  che imposta i parametri oltre che reperisce anche l'indirizzo e-mail.
075800041021    2C     *IN20         IFEQ      *ON
075900041021     c                   clear                   trul44ds
076000050913     c                   clear                   trtcm1ds
076100041021     c                   eval      d44pgm = 'TRTC82R'
076200041021     c                   move      d82cpt        d44ksc
076300041021     c                   eval      d44apl = 'C'
076400041118     c                   eval      d44nk1 = '0151' + %editc(D82CPT:'X')
076500041021     c                   eval      d44tnt = '86'
076600041021     c                   eval      d44scope='*JOB'
076700041021     c                   eval      d44ovr = 'S'
076800120305     c***66              eval      d44obj = '*OBJM*Bartolini C.O.D. '+
076900120305     c   66              eval      d44obj = '*OBJM*Bartolini-BRT C.O.D. '+
077000110609     c                             'Manifest del ' + %editw(PARDFV:'  /  /    ')
077100110609     c                              + ' - ' + %trim(Starag)
077200120305     c**N66              eval      d44obj = '*OBJM*Bartolini-BRT C.O.D. '+
077300120305     c  N66              eval      d44obj = '*OBJM*BRT C.O.D. '+
077400110608     c                             'Manifest del ' + %editw(PARDFV:'  .  .    ')
077500110608     c                              + ' - ' + %trim(Starag)
077600041021     c                   eval      d44prtf = 'MANCOD'
077700041021     c                   call      'TRUL44R'
077800041021     c                   parm                    trul44ds
077900050913     c                   parm                    trtcm1ds
078000041025      **  Se tutto OK salta la QCMDEXEC
078100041025      **   con la vecchia OVRPRTF
078200041109     c     d44err        ifeq      *blank
078300041109     c                   eval      Email_COD='S'
078400041109     c                   goto      salta_OVR
078500041109     c                   end
078600041021    1C                   END
078700041020     C**
078800041109     C**  Viene eseguita se non è stata mandata via E-mail
078900041109     C**   notare il TAG seguente
079000970115     C                   CALL      'QCMDEXC'                            90
079100970115     C                   PARM                    COMMAN           80
079200970115     C                   PARM                    LUNG             15 5
079300041021     c     salta_OVR     tag
079400041021     C**
079500961213     C                   OPEN      MANCOD
079600961213    1C                   END
079700961213     C**
079800961213     C* OVRPRTF PER LISTA DISCORDANZE
079900961213     C**
080000970110   1AC     D82LDS        IFEQ      'SI'
080100970114     C* lista discordanze via fax se elaborazione definitiva e ho il n.
080200020301     C* fax
080300961213    1C     *IN20         IFEQ      *ON
080400120821     c     d82ftt        andne     FEDEX
080500040423
080600040423     c                   if        FAX_Disc_Est = 'S'
080700970115     C     STPFAX        IFNE      *BLANKS
080800971230     C                   MOVEL     CMD(8)        CMDPRT
080900021009     C                   MOVEL     FaxInOutQ     OUTQPR
081000971230     C                   MOVEL     CMDPRT        COMMAN
081100970115     C                   ELSE
081200970115     C                   MOVEL     CMD(12)       COMMAN
081300970115     C                   END
081400970115     C                   CALL      'QCMDEXC'                            90
081500970115     C                   PARM                    COMMAN           80
081600970115     C                   PARM                    LUNG             15 5
081700970114     C                   OPEN      LISDIS
081800040423     c                   end
081900040423
082000970114    1C                   END
082100970114     C*
082200970114     C* lista discordanze sempre su carta
082300961213     C                   MOVEL     CMD(7)        COMMAN
082400041021     C*
082500961213     C                   CALL      'QCMDEXC'                            90
082600961213     C                   PARM                    COMMAN           80
082700961213     C                   PARM                    LUNG             15 5
082800041021     C*
082900970114     C                   OPEN      LISDIC
083000970110   1AC                   END
083100971219     C*
083200971219     C*  Accendo indicatori di eccedenza
083300971219     C                   SETON                                        OFOGOD
083400971219     C                   SETON                                        OCOEOB
083500961121     C*
083600960529     C                   ENDSR
083700020225      *****************************************************************
083800041020      * Imposta le code x invio FAX
083900020225      *****************************************************************
084000041021     C     Code_FAX      BEGSR
084100020225      *
084200041020     ** Reperisco la coda delle stampe FAXIN .
084300041020     C                   CALL      'TRULOUTQ'
084400041020     ** Parametri di input.
084500041020     C                   PARM                    PrmFine
084600041020     C                   PARM      SimPOU        PrmFilS
084700041020     C                   PARM      FaxIn         PrmTpOutQ
084800041020     ** Parametri di output.
084900041020     C     MyOutQ        PARM      *BLANK        PrmOutQ
085000041020     C     MyOutQLib     PARM      *BLANK        PrmOutQLib
085100041020     C                   PARM      *BLANK        PrmEsito
085200041020     ** Se il reperimento della coda non è riuscito, creo lo spool in QDKT.
085300041020     C                   IF        PrmEsito = Errore
085400041020     C                   EVAL      MyOutQ = 'QDKT'
085500041020     C                   EVAL      MyOutQLib = '*LIBL'
085600041020     C                   ENDIF
085700041020      *  Se non ha trovato imposta la coda generica
085800041020     C                   IF        MyOutQ = *BLANK
085900041020     C                   EVAL      MyOutQ = 'QDKT'
086000041020     C                   EVAL      MyOutQLib = '*LIBL'
086100041020     C                   ENDIF
086200041020      *
086300041020     C                   EVAL      FaxInOutQ = MyOutQ
086400041020     C                   EVAL      FaxInLibQ = MyOutQLib
086500041020     C**
086600041020     C*---------------------------------------------------------
086700041020     ** Reperisco la coda delle stampe FAXOUT.
086800041020     C                   CALL      'TRULOUTQ'
086900041020     ** Parametri di input.
087000041020     C                   PARM                    PrmFine
087100041020     C                   PARM      SimPOU        PrmFilS
087200041020     C                   PARM      FaxOut        PrmTpOutQ
087300041020     ** Parametri di output.
087400041020     C     MyOutQ        PARM      *BLANK        PrmOutQ
087500041020     C     MyOutQLib     PARM      *BLANK        PrmOutQLib
087600041020     C                   PARM      *BLANK        PrmEsito
087700041020     ** Se il reperimento della coda non è riuscito, creo lo spool in QDKT.
087800041020     C                   IF        PrmEsito = Errore
087900041020     C                   EVAL      MyOutQ = 'QDKT'
088000041020     C                   EVAL      MyOutQLib = '*LIBL'
088100041020     C                   ENDIF
088200041020      *  Se non ha trovato imposta la coda generica
088300041020     C                   IF        MyOutQ = *BLANK
088400041020     C                   EVAL      MyOutQ = 'QDKT'
088500041020     C                   EVAL      MyOutQLib = '*LIBL'
088600041020     C                   ENDIF
088700041020      *
088800041020     C                   EVAL      FaxOutOutQ = MyOutQ
088900041020     C                   EVAL      FaxOutLibQ = MyOutQLib
089000041020      *
089100041020     C                   ENDSR
089200041020      *****************************************************************
089300041020      * LETTURA DELLE SPEDIZIONI
089400041020      *****************************************************************
089500041020     C     REDSPE        BEGSR
089600041020      *
089700020225      * LEGGO V.L. SU FNARB PER LNA E DATA TRASMISSIONE = zero
089800020225      * (I RECORD GIA' TRASMESSI NON DEVONO ESSERE RIELABORATI)
089900020225     C                   clear                   KDT3
090000961113     C     KARB55        SETLL     FNARB55L
090100020225     C     KARB55        READE     FNARB55L
090200020225      *
090300020225    1C                   DOW       not %EOF(FNARB55L)
090400020225      *
090500020225      * Escludo locali e spedizioni consegnate
090600020225    2c                   IF        ARBlnp <> ARBlna  and  ARBdcm = *zeros
090700020225      *
090800020225      * Leggo ds3a - tabella codici bolla se varia arbcbo
090900020225    3C     ARBCBO        IFNE      SAVCBO
091000961112     C                   MOVEL     '3A'          KCOD
091100961112     C                   MOVEL(P)  ARBCBO        KKEY
091200961112     C                   EXSR      CTABEL
091300961114     C  N31              MOVEL     TBLUNI        DS3A
091400961114     C   31              CLEAR                   DS3A
091500961112     C                   MOVE      ARBCBO        SAVCBO
091600020225    3C                   ENDIF
091700020225      * Escludo bolle di recupero
091800020225    3c                   IF        §3Arbl <> 'R'
091900020225      *
092000020225      * SFLAGGO BOLLA IN OGNI CASO (POTREBBE ESSERCI 'T' IN SEGUITO
092100020225      * A PRECEDENTE ELABORAZIONE PROVVISORIA MA TALE 'T' POTREBBE NON
092200020225      * ESSERE PIU' VERA)
092300020225     C                   clear                   ARBFT3
092400060918     C                   CLEAR                   CreaE8            1
092500060918     C                   CLEAR                   CreaE7x           1
092600020225      * LEGGO DETTAGLIO COLLI
092700060918     c* Primo giro: per no invio spedizioni parziali
092800060918     c*             non creo le discordanze
092900060918     c                   if        d82isc='S'
093000060918     c                   eval      wcreadis=' '
093100060918     c                   else
093200060918     c                   eval      wcreadis='S'
093300060918     c                   endif
093400060918     c
093500961112     C                   EXSR      REDART
093600060918     c
093700060918     c* Se devo creare discordanze e stampare faccio un secondo giro
093800060918     c*  solo per INVII no parziali
093900060918     c                   if        d82isc='S' and
094000060918     c                             (creae8='S' or creae7x='S')
094100060918     c                   eval      wcreadis='S'
094200060918     C                   EXSR      REDART
094300060918     c                   endif
094400060918     c*
094500020225      **
094600020225      * LA SPEDIZIONE FA PARTE DEL MANIFEST
094700060913      * PER FEDEX       INCLUDO SOLO QUELLE CON ABILITAZIONE MERCE ARR > 1
094800060913      * Se invio TOTALE includo solo quelle con abilitazione merce arr > 1
094900060915      *  e se non in IMA
095000020225      **------------------------------------------------------
095100120821    4C                   IF        (ARBFT3 ='T' and d82ftt <> FEDEX and
095200060913     c                              d82isc=' ') or
095300120821    4C                             (d82ftt = FEDEX and ARBAMA > 1)  or
095400060915    4C                             (d82isc ='S' and ARBAMA > 1 and
095500060918     c                              arbft3='T' and not *in01)
095600060913      *
095700020225      * CODICE BOLLA
095800020225      *  EuroExpress e FedEX
095900020225    6C                   select
096000020225     c                   when      §3ATB1 = 'FS'
096100970219     C                   MOVEL     'EXW'         STATBO
096200020225     c                   when      §3ATB1 = 'AS'
096300970219     C                   MOVEL     'FHC'         STATBO
096400020226     c                   when      %SUBST(§3ATB1:1:1) = 'A'
096500020225     c                             or ARBFBR = 'F'
096600970219     C                   MOVEL     'EXW'         STATBO
096700020225     C                   other
096800020225     C                   MOVEL     'FHC'         STATBO
096900020225    6C                   endsl
097000030317      * Per Fedex
097100150825      * adesso per tutti vale la precedente regola di FEDEX x il PESO/VOLUME
097200150825     c*********          if        d82ftt = FEDEX
097300030318      *
097400030317      * calcola il Volume/Peso da CML o da Bollettato
097500030317     c                   exsr      VOL_PES
097600030317      *
097700150825     C                   Z-ADD     wpes          PESPRT            7 1
097800030317      *
097900150825     c*********          else
098000150825      ********
098100020225      * Determino peso tassabile per le spedizioni in assegnato
098200020225      * (E'il maggiore fra arbpkf e il risultato della moltiplicazione)
098300150825    5C**** STATBO        IFEQ      'EXW'
098400150825     C**** §7AKPM        MULT(H)   ARBVLF        PESPRT            7 1
098500150825    6C****               if        ARBPKF > PESPRT
098600150825     C****               Z-ADD     ARBPKF        PESPRT
098700150825    6C****               endif
098800150825   x5C****               ELSE
098900020225      * Per le spedizioni in franco invece,stampo il peso da fatturare
099000150825     C****               Z-ADD     ARBPKF        PESPRT
099100150825    5C****               END
099200150825     c*********          endIF
099300030317      *
099400020225      * Totale spedizione/colli/peso e volume
099500020225      * N.B.:I totali mi servono anche se elaborazione provvisoria, no
099600020225      *      dogana in quento dovrò stampare i totali del foglio viaggi
099700020225    5C     D82STB        IFEQ      'SI'
099800970219     C     D82DGN        OREQ      'SI'
099900970219     C     *IN20         OREQ      *OFF
100000970219     C                   ADD       1             TOTSPE            5 0
100100970219     C                   ADD       ARBNCL        TOTNCL            7 0
100200970219     C                   ADD       PESPRT        TOTPES           10 1
100300150825     c**********         if        d82ftt = FEDEX
100400150825     C                   add       wvol          TOTVLF            7 3
100500150825     c**********         else
100600150825     C**********         ADD       ARBVLF        TOTVLF            7 3
100700150825     c**********         end
100800020225    5C                   ENDIF
100900020225      *
101000020225      * STAMPO MANIFEST SOLO SE RICHIESTO:
101100020225      * - Manifest per partner
101200020225    5C     D82STB        IFEQ      'SI'
101300020225      * - Manifest per dogana
101400970219     C     D82DGN        OREQ      'SI'
101500120821     c     d82ftt        andne     FEDEX
101600020225      * - Manifest spedizioni C.O.D. e questa è spedizione C.O.D.
101700970219     C     D82COD        OREQ      'SI'
101800961112     C     §3AFCA        ANDGT     0
101900120821     c     d82ftt        andne     FEDEX
102000961121     C                   EXSR      PRTMAN
102100020225    5C                   ENDIF
102200020225      *
102300020225      * FLEGGO FNARB E SCRIVO RCD PER TRASMISSIONE
102400020226    5C                   SELECT
102500041021      **
102600020225      * "Provvisorio": aggiorno ARBFT3 per l'elaborazione successiva
102700020225     c                   when      *in20 = *off
102800020225     C                   EXCEPT    AGGFT3
102900041021      **
103000020225      * "Definitivo" e "No trasmissione": fleggo la bolla come trasmessa
103100050104      *  o se invio solo via E-Mail
103200120821     c                   when      D82FTT = SOLOSTAMPA
103300120821     c                              or
103400120821     c                             D82FTT = PRINTNOEDI
103500020225     C                   MOVEL     'T'           ARBFT3
103600020225     C                   Z-ADD     D82DFV        ARBDT3
103700090402     c* data manifest in filiale
103800090402     C                   EXCEPT    AGGTRA
103900090402     c* Data manifest in sede
104000090402     c                   Exsr      AggDataM
104100120823      **
104200120823      * GEL   (Scrittura del file NET EXPRESS EUROPE)
104300120823      * "Definitivo" e "Trasmissione": scrivo record per trasmissione,
104400120823      *   il pgm che trasmette flegga la bolla
104500120823     c                   WHEN      D82FTT = NETEXP
104600120823     C                   EXCEPT    LIBER
104700120823     C                   Z-ADD     ARBAAS        F83AAS
104800120823     C                   Z-ADD     ARBLNP        F83LNP
104900120823     C                   Z-ADD     ARBNRS        F83NRS
105000120823     C                   Z-ADD     ARBNSP        F83NSP
105100120823     c                   z-add     d82dfv        f83dfv
105200120823     C                   MOVEL     TRTC83DS1     KPJBU
105300120905     C                   CALL      'TRTC83G2R'
105400120823     C                   PARM                    KPJBA
105500120823      * REIMPOSTO KPJBU "ORIGINALE"
105600120823     C                   MOVEL     WKPJBU        KPJBU
105700120823     c* Data manifest in sede
105800120823     c                   Exsr      AggDataM
105900041021      **
106000020225      * "Definitivo" e "Trasmissione FED": scrivo record per trasmissione,
106100020225      *   il pgm che trasmette flegga la bolla
106200120821     c                   when      D82FTT = FEDEX
106300020225     C                   EXCEPT    LIBER
106400020228     C                   Z-ADD     ARBAAS        F83AAS
106500020228     C                   Z-ADD     ARBLNP        F83LNP
106600020228     C                   Z-ADD     ARBNRS        F83NRS
106700020228     C                   Z-ADD     ARBNSP        F83NSP
106800020228     c                   z-add     d82dfv        f83dfv
106900020228     C                   MOVEL     TRTC83DS1     KPJBU
107000020225     C                   CALL      'TRTC83R21'
107100020225     C                   PARM                    KPJBA
107200020304      * REIMPOSTO KPJBU "ORIGINALE"
107300020304     C                   MOVEL     WKPJBU        KPJBU
107400090402     c* Data manifest in sede
107500090402     c                   Exsr      AggDataM
107600041021      **
107700020225      * "Definitivo" e "Trasmissione EEX": scrivo record per trasmissione,
107800020225      *   il pgm che trasmette flegga la bolla
107900020226     c                   other
108000060109      *
108100060109      * ?dal 9/01/2006 -  DPD esegue il Manifest Definitivo + invio SCAN
108200060109      * ?....invio SCAN DPD oltre EDI se tipo trasm.--> ("S")
108300060109      * ?    al posto del vecchio FIEU25R dal giro degli SCAN DPD schedulato
108400060109     c                   if        wOGGI >= 20060109
108500120821     c                   if        D82FTT = SCANDPDEDI or D82FTT = SCANDPD
108600060109
108700140311      * vecchio invio SCAN a DPD (no GEOPOST file ma vecchi SCAN)
108800140311
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
110100140311      **
110200140311      * Invio file GEOPOST Nuovo tracciato
110300140311
110400140311      **
110500140311      ** la prima volta deve fare la testata e in seguito tutti i dettagli
110600140311      **  il GOTO è fatto solo quindi per il primo record poichè il flagHDE è
110700140311      **  inizializzato a 'H' Head TESTATA
110800140311      **
110900140311     c     Ri_esegui     tag
111000140311     C                   Z-ADD     ARBAAS        F83AAS
111100140311     C                   Z-ADD     ARBLNP        F83LNP
111200140311     C                   Z-ADD     ARBNRS        F83NRS
111300140311     C                   Z-ADD     ARBNSP        F83NSP
111400140311     c                   z-add     d82dfv        f83dfv
111500140311      *
111600140311     C                   MOVEL     TRTC83DS1     KPJBU
111700140311     C                   call      'TRTC83R13'
111800140311     C                   PARM                    KPJBA
111900140311     C                   PARM                    flagHDE           1
112000140311      *
112100140311      * Solo dopo la prima volta x poter scrivere la testata
112200140311     C                   if            flagHDE ='H'
112300140311     C                   movel     'D'           flagHDE
112400140311     c                   goto      ri_esegui
112500140311     c                   end
112600140311      *
112700140311      * REIMPOSTO KPJBU "ORIGINALE"
112800140311     C                   MOVEL     WKPJBU        KPJBU
112900140311      **
113000140311      **
113100120821     c                   if        D82FTT  = SCANDPD
113200060109     C                   MOVEL     'T'           ARBFT3
113300060109     C                   Z-ADD     D82DFV        ARBDT3
113400090402     c* Data manifest in filiale
113500090402     C                   EXCEPT    AGGTRA
113600090402     c* Data manifest in sede
113700090402     C                   Exsr      AggDataM
113800060109     c                   end
113900060109      *
114000060109     c                   end
114100060109     c                   end
114200051221
114300051223      * ?Non deve eseguire l'EDI x le Trasmissioni DPD non a SEUR
114400120821     c                   if        D82FTT <> SCANDPD
114500051223
114600961112     C                   EXCEPT    LIBER
114700961112     C                   Z-ADD     ARBAAS        D82AAS
114800961112     C                   Z-ADD     ARBLNP        D82LNP
114900961112     C                   Z-ADD     ARBNRS        D82NRS
115000961112     C                   Z-ADD     ARBNSP        D82NSP
115100020225     C                   MOVEL     TRTC82DS      KPJBU
115200180312      *
115300180312      * TRACCIATO FEDEX tramite un CSV separato da virgole
115400180312     c                   if        FEDEX_UK ='S' and
115500180312     c                              D82CPT = CODFEDEX_UK
115600180312     C                   CALL      'TRTC83U2R'
115700180312     C                   PARM                    KPJBA
115800180312      *
115900180312     c                   else
116000180312      *
116100961112     C                   CALL      'TRTC83R2'
116200961112     C                   PARM                    KPJBA
116300180312      *
116400180312     c                   end
116500180312      *
116600090402     c* Data manifest in sede
116700090402     C                   Exsr      AggDataM
116800051223
116900051223     c                   end
117000020225      *
117100020225    5C                   ENDSL
117200020225      **
117300020225      * SE RECORD NON DA INVIARE AGGIORNO ARBFT3 PER "PULIRLO"
117400020225   X4C                   ELSE
117500041021      **
117600020304     c* se sto elaborando manifest per fedex arbft3 potrebbe essere = 'T'
117700020304     c* (spedizione parziale) ma se entro qua lo devo clearare
117800020304     C                   clear                   ARBFT3
117900970319     C                   EXCEPT    AGGFT3
118000020225    4C                   ENDIF
118100020225      *
118200020225    3c                   ENDIF
118300020225    2c                   ENDIF
118400020225     C     KARB55        READE     FNARB55L
118500961120    1C                   ENDDO
118600961112     C*
118700140311      *
118800140311      *  Solo per DPD nuovo tracciato GEOPOST, se è passato dal Dettaglio
118900140311      *  generando il file, il flag è impostato a "D" altrimenti sarebbe ancora
119000140311      *  impostato a "T" come da inizializzazione.
119100140311      *   A questo punto deve chiudere il file che ha aperto e scritto in Dettaglio
119200140311     C                   if            flagHDE ='D'
119300140311      **
119400140311      ** il flag HDE ha funzionato come sintesi del concetto che il programma
119500140311      **  è stato richiamato per fare un Manifest x DPD ed inoltre in modalità
119600140311      **  GEOPOST (altrimenti non varrebbe "D" in nessun altro caso)
119700140311     C                   movel     'E'           flagHDE
119800140311     C                   clear                   KPJBU
119900140311     C                   call      'TRTC83R13'
120000140311     C                   PARM                    KPJBA
120100140311     C                   PARM                    flagHDE
120200140311      **
120300140513      * REIMPOSTO KPJBU "ORIGINALE"
120400140513     C                   MOVEL     WKPJBU        KPJBU
120500140513      *
120600140311     c                   end
120700140311      *
120800961112     C                   ENDSR
120900090402     C*****************************************************************
121000090402     C*    Aggiorna data manifest in filiale e sede                   *
121100090402     C*****************************************************************
121200090402     c     AggDataM      BEGSR
121300090402     c                   setoff                                       818283
121400090402     c                   setoff                                       8485
121500090402     c                   clear                   dta4k
121600090402     c*
121700090402     c
121800090402     c* Anche in sede
121900090402     c                   movel     'K'           ktrc
122000090402     c     kar4          chain     tita430c
122100090402    1c                   if        %found(tita430c)
122200090402     c                   movel     ta4not        dta4k
122300090402   x1c                   else
122400090402     c
122500120823     c     karB          chain     titas30c
122600090402     c                   clear                   ta4not
122700090402    1c                   endif
122800090402     c
122900090403     c                   movel     d82dfv        §ta4kdmf
123000090402     c                   movel     dta4k         ta4not
123100090402    1c                   if        %found(tita430c)
123200090402     c   84              update    tita4000
123300090402     c   85              update    tita4p00
123400090402   x1c                   else
123500090403    2c                   if        %found(titas30c)
123600090402     c                   movel     arbaas        ta4aas
123700090402     c                   movel     arblnp        ta4lnp
123800090402     c                   movel     arbnrs        ta4nrs
123900090402     c                   movel     arbnsp        ta4nsp
124000090402     c                   movel     'K'           ta4trc
124100090402     c  n83              write     tita4000
124200090402     c   83              write     tita4p00
124300090402    2c                   endif
124400090402    1c                   endif
124500090402     c
124600090402     C                   ENDSR
124700961114     C*****************************************************************
124800961114     C*    LETTURA DETTAGLIO COLLI                                    *
124900961114     C*****************************************************************
125000961114     C     REDART        BEGSR
125100020225      *
125200961115     C                   CLEAR                   WDTPAR
125300961115     C                   CLEAR                   WFIL
125400961120     C                   CLEAR                   WNPS
125500020225     C                   clear                   WDIS4
125600020225     C                   clear                   XX
125700020225     C                   clear                   YY
125800961218     C                   CLEAR                   NSC
125900020225     c                   eval      *in02 = *off
126000060918     c                   eval      *in01 = *off
126100020225     c                   eval      *in12 = *on
126200020225      *
126300120823     C     KARb          SETLL     FNART01L
126400120823     C     KARb          READE     FNART01L                               31
126500961114    1C     *IN31         DOWEQ     *OFF
126600961121     C*
126700961121     C                   MOVE      ARTFLS        KFLS
126800961121     C                   MOVE      ARTLNA        KLNA
126900961121     C                   MOVE      ARTNRS        KNRS
127000961121     C                   MOVE      ARTNSC        KNSC
127100970116     C*
127200970116    2C     ARTDTR        IFGT      *ZEROS
127300970116     C                   Z-ADD     645           KCAA
127400970116     C                   EXSR      CTRANM
127500970116    3C     *IN09         IFEQ      *OFF
127600060913     c
127700970116     C* MEMORIZZO CHE LA SPEDIZIONE E' DA STAMPARE NEL MANIFEST
127800970116     C                   MOVE      'T'           ARBFT3
127900060913     c
128000970116     C* CONTROLLI PER DISCORDANZA E4-INVIO RECORD:COLLO GIA' INVIATO
128100970116    4C     *IN20         IFEQ      *ON
128200970116     C     D82LDS        OREQ      'SI'
128300970116     C* Controllo che collo non sia nuovamente in partenza (anom.620)
128400970116     C                   Z-ADD     620           KCAA
128500990519     C     KANM2         SETLL     FNANM02L                               31
128600970116    5C     *IN31         IFEQ      *OFF
128700970116    6C     WDIS4         IFEQ      *BLANKS
128800060913     c
128900060918     c* Per INVII anche parziali
129000060918    7c                   if        d82isc=' '
129100060913     c                   eval      kcev='E4'
129200060913     C                   EXSR      GEDIS48
129300060918     c
129400060918     C                   MOVE      '1'           WDIS4
129500060918    7c                   endif
129600060918     c*
129700060918     c* Per INVII solo totali o forzati
129800060918    7c                   if        d82isc='S'
129900060918     c
130000060918     c* 1° giro: Memorizzo che devo creare l'anomalia
130100060918    8c                   if        wcreadis=' '
130200060918     c
130300060918     c*  Se bolla abilitata la differenza la fa l'IMA
130400060918    9c                   if        arbama>1
130500060918     c
130600060918     c* Se non c'e' ancora nulla devo creare la E8
130700060918     c                   if        creae8=' '
130800060918     c                   eval      creae8='S'
130900060918     c                   endif
131000060918     c* Se non devo creare la S8 allora memorizzo il persiste E7A
131100060918     c                   if        creae8='N'
131200060918     c                   eval      creae7x='S'
131300060918     c                   endif
131400060918   x9c                   else
131500060918     c
131600060918     c* scrivo il persiste manca invio record
131700060918     c                   eval      creae7x='S'
131800060918    9c                   endif
131900060918     c*
132000060918   x8c                   else
132100060918     c* 2° giro: stampo un base alle momrizzazioni per giro precedente
132200060918    9c                   if        creaE8='S'
132300060918     c                   eval      kcev='E8'
132400060918     C                   EXSR      GEDIS48
132500060918     C                   MOVE      '1'           WDIS4
132600060918    9c                   endif
132700060918     c
132800060915     c* scrivo il persiste manca invio record
132900060918    9c                   if        creaE7x='S'
133000060915     c                   exsr      CTRE7X
133100060918    9c                   endif
133200060918    8c                   endif
133300060918    7c                   endif
133400060913     c
133500970116    6C                   END
133600970116     C*
133700970116     C* STAMPA RIGA SEGNACOLLI GIA' INVIATI
133800060918     c*  per la E4 o la E8 al 2° giro se creata
133900970116    6C     D82LDS        IFEQ      'SI'
134000060918     c
134100060918   6ac                   if        d82isc=' ' or
134200060918     c                             (D82isc='S' and wcreadis='S' and
134300060918     c                              creae8='S')
134400060915     c
134500020225    7C     XX            IFGE      13
134600060918     C                   MOVE      ','           NSC(XX)
134700060918     c                   exsr      Stagiainv
134800060918     c
134900970116     C                   SETOFF                                       12
135000020225     C                   MOVE      *ZEROS        XX
135100970116     C                   CLEAR                   NSC
135200060918    7C                   ENDif
135300970116     C* Memorizzo in schiera di stampa il numero segnacollo
135400020225     C                   ADD       1             XX
135500970116     C* Richiamo routine per abblencare zeri non significativi
135600970116     C                   MOVE      ARTNSC        W007A             7
135700970116     C                   MOVEA     W007A         K08
135800970116     C                   EXSR      ABBL
135900020225     C                   MOVEA     K08           NSC(XX)
136000970116     C* Metto "," nell'elemento di schiera precedente
136100020225    7C     XX            IFGT      1
136200020225     C     XX            SUB       1             YY
136300020225     C                   MOVE      ','           NSC(YY)
136400970116    7C                   END
136500060915   6aC                   END
136600060918     c
136700060915    6C                   END
136800970116    5C                   END
136900970116    4C                   END
137000970116    3C                   END
137100020225      ***
137200970116   X2C                   ELSE
137300990412      * SE HO  COLLO ARRIVATO MA NON SPARATO FORZO L'ARRIVO   Solo se no bis
137400990412     C                   CLEAR                   WARFOR
137500001127     C**********
137600001127     C***  ASTERISCATO PERCHÈ NON SI VUOLE PIÙ LA GENERAZIONE DEL MANIFEST
137700001127     C***  CON PARTENZE SPARATE SOLO DA FILIALI DI PARTENZA.
137800001127     C***  QUESTO ERA STATO ATTIVATO PRECEDETEMENTE IL 12/04/99
137900001127     C***  ORA SI È TORNATI INDIETRO.(VEDERE NOTE DI ND)
138000001127     C**********
138100001127    3C********** ARTDAM    IFEQ *ZEROS
138200001127     C********** *IN22     ANDEQ*OFF
138300001127     C**********           EXSR ARFORZ
138400001127    3C**********           ENDIF
138500001127     C***
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
191400961114     C*
191500961114    2C                   SELECT
191600990412     C* C O L L O   N O N   A R R I V A T O  e non forzato l'arrivo
191700970116     C* O ARRIVATO IN DATA > ALLA DATA DEL FOGLIO DI DEFLUENZA (SOLO SE
191800970116     C* NO BIS)
191900961114     C     ARTDAM        WHENEQ    *ZEROS
192000990412     C     WARFOR        ANDEQ     *BLANKS
192100970117     C     *IN22         ANDEQ     *OFF
192200961224     C     ARTDAM        ORGT      D82DFV
192300970117     C     *IN22         ANDEQ     *OFF
192400961121     C   20              UNLOCK    FNART55L
192500961114     C* - RELATIVO A SPEDIZIONE DEL MANIFEST DEL GIORNO --> DISC. 1A
192600961212    3C     ARBDT3        IFEQ      D82DFV
192700961114     C     ARBDT3        OREQ      *ZEROS
192800961114     C     ARBFT3        ANDEQ     'T'
192900961115     C                   MOVE      'E1A'         WCODIS
193000961114   X3C                   ELSE
193100961114     C* - RELATIVO A SPEDIZIONE DI MANIFEST PRECEDENTE ---> DISC. 5A
193200961114    4C     ARBDT3        IFGT      *ZEROS
193300961212     C     ARBDT3        ANDLT     D82DFV
193400040416     c* creo la discordanza solo se non sono passati più di x giorni
193500040416     c* dalla data del'invio del record bolla
193600040416     c     *iso          Movel     arbdt3        wdta
193700040416     c     *iso          Movel     d82dfv        wdta1
193800040416     c                   Adddur    §7agde:*D     wdta
193900040416     c     wdta          ifge      wdta1
194000961115     C                   MOVE      'E5A'         WCODIS
194100040416     c                   endif
194200961114    4C                   END
194300961114    3C                   END
194400961120     C*
194500961115    3C     WCODIS        IFNE      *BLANKS
194600961127     C                   MOVE      WCODIS        KCEV
194700961127     C                   MOVE      ARTFLS        KFLS
194800961127     C                   MOVE      ARTLNA        KLNA
194900961127     C                   MOVE      ARTNRS        KNRS
195000961127     C                   MOVE      ARTNSC        KNSC
195100961127     C                   MOVE      ARTAAS        WAAS
195200961127     C                   MOVE      ARTLNP        WLNP
195300961127     C                   MOVE      ARTNSP        WNSP
195400970117     C* STAMPO DISCORDANZA
195500970117     C     D82LDS        IFEQ      'SI'
195600970117     C                   EXSR      PRTDIS
195700970117     C                   END
195800961114     C* SE ELABORAZIONE DEFINITIVA CREO L'EVENTO
195900961127     C   20              EXSR      CRTEVS
196000961114    3C                   END
196100961114     C*
196200961114     C* C O L L O    A R R I V A T O
196300990412     C* o forzato l'arrivo (solo se no BIS)
196400961212     C     ARTDAM        WHENLE    D82DFV
196500990412     C     WARFOR        OREQ      'S'
196600961115     C                   MOVE      ARTFLS        KFLS
196700961115     C                   MOVE      ARTLNA        KLNA
196800961115     C                   MOVE      ARTNRS        KNRS
196900961115     C                   MOVE      ARTNSC        KNSC
197000970116     C*
197100970117   3AC     *IN22         IFEQ      *OFF
197200961122     C                   Z-ADD     645           KCAA
197300961115     C                   EXSR      CTRANM
197400961115     C* COLLO A TERRA
197500961115    3C     *IN09         IFEQ      *ON
197600961120     C* MEMORIZZO NUMERO PISTOLA
197700961120     C                   MOVE      ANMNPS        WNPS
197800961114     C* - DISCORDANZA 1
197900961212    4C     ARBDT3        IFEQ      D82DFV
198000961114     C     ARBDT3        OREQ      *ZEROS
198100961114     C     ARBFT3        ANDEQ     'T'
198200961114     C                   MOVE      'E1 '         WCODIS
198300961115     C                   MOVE      ANMFLE        WFIL
198400961114   X4C                   ELSE
198500961114     C* - DISCORDANZA 5
198600961114    5C     ARBDT3        IFGT      *ZEROS
198700961212     C     ARBDT3        ANDLT     D82DFV
198800961114     C                   MOVE      'E5 '         WCODIS
198900961115     C                   MOVE      ANMFLE        WFIL
199000961114    5C                   END
199100961114    4C                   END
199200970116    3C                   END
199300970116  X3AC                   ELSE
199400970116     C* SE BIS VERIFICO SE IN PARTENZA
199500970116     C                   EXSR      CHKBRV
199600970116   3AC                   END
199700970116     C*
199800970116    3C     *IN09         IFEQ      *OFF
199900961122    4C     ARBDT3        IFGT      *ZEROS
200000961212     C     ARBDT3        ANDLT     D82DFV
200100970627     C     ARTDCM        ANDEQ     *ZEROS
200200970116     C* E3 - RECORD GIA' INVIATO:INVIO COLLO
200300961114     C                   MOVE      'E3 '         WCODIS
200400970117    4C                   END
200500961224     C* DETERMINO NUMERO PISTOLA E SE ESISTE SPUNTA DI DEFLUENZA
200600970116     C* (SOLO SE NO BIS)
200700970117     C  N22              EXSR      CHKBRV
200800961122    3C                   END
200900060913     C*
201000060913     c*
201100060913     C* COLLO ARRIVATO DA INVIARE MA NON INVIATO record bolla perchè
201200060913     c*  invio parziale
201300060913     c                   if        not *in09 and arbft3=' ' and d82isc='S'
201400060915     c                             and   artdcm=0
201500060913     c* Se sto inviando collo --> disc. E7 altrimenti avviso
201600060913     c* che c'è ancora una parziale (E7A)
201700060913     c                   exsr      CTRE7x
201800060913     c                   endif
201900060913     c
202000060913     c
202100961114    3C     WCODIS        IFNE      *BLANKS
202200961127     C                   MOVE      WCODIS        KCEV
202300961127     C                   MOVE      ARTAAS        WAAS
202400961127     C                   MOVE      ARTLNP        WLNP
202500961127     C                   MOVE      ARTNSP        WNSP
202600970117     C* STAMPO DISCORDANZA
202700970117     C     D82LDS        IFEQ      'SI'
202800970117     C                   EXSR      PRTDIS
202900970117     C                   END
203000961114     C* SE ELABORAZIONE DEFINITIVA CREO EVENTO
203100961127     C   20              EXSR      CRTEVS
203200961114    3C                   END
203300970117     C* SE COLLO IN PARTENZA AGGIORNO ARTDTR
203400970117    3C     *IN09         IFEQ      *OFF
203500961205     C     *IN20         ANDEQ     *ON
203600970627     C     ARTDCM        ANDEQ     *ZEROS
203700120821     c     d82ftt        andne     FEDEX
203800020304     c* per fedex lo deve fare solo se sono arrivati tutti i colli
203900020304    3C     *IN09         oreq      *OFF
204000020304     C     *IN20         ANDEQ     *ON
204100020304     C     ARTDCM        ANDEQ     *ZEROS
204200020304     c     arbft3        andeq     'T'
204300961120     C* RICHIAMO ROUTINE PER AUTOGENERARE SPUNTA PARTENZA SE NON C'E'
204400961115     C* E CHIUDERE EVENTUALI ANOMALIE 145 E 146 DEI GIORNI PRECEDENTI
204500961115     C                   SETON                                        43
204600961115     C                   EXSR      PARCOL
204700961212     C                   MOVE      D82DFV        ARTDTR
204800961114     C                   UPDATE    FNART055
204900961114   X3C                   ELSE
205000961114     C   20              UNLOCK    FNART55L
205100961114    3C                   END
205200961114     C*
205300961114    2C                   ENDSL
205400961203     C*
205500961203     C     LEG55         TAG
205600961203     C*
205700961114     C   20KART55        READE     FNART55L                               31
205800961114     C  N20KART55        READE(N)  FNART55L                               31
205900961114    1C                   ENDDO
206000961121     C*----------------------------------------------------------------
206100961120     C* CONTROLLO DISCORDANZE 6A - COLLO DATO PER INOLTR.MA IN PARTENZA
206200961121     C*----------------------------------------------------------------
206300961115     C                   CLEAR                   WFIL
206400961115     C* LEGGO ANOMALIE 620 PER LNA
206500961115     C                   Z-ADD     620           KCAA
206600990519     C     KANM10        SETLL     FNANM10L
206700990519     C   20KANM10        READE     FNANM10L                               31
206800990519     C  N20KANM10        READE(N)  FNANM10L                               31
206900961121    1C     *IN31         DOWEQ     *OFF
207000961203     C* ESCLUDO LOCALI
207100961203     C     ANMLNP        IFGT      *ZEROS
207200961203     C     ANMLNP        ANDEQ     ANMLNA
207300961203     C     ANMLNP        OREQ      *ZEROS
207400961203     C     ANMFLS        ANDEQ     ANMLNA
207500990519     C   20              UNLOCK    FNANM10L
207600961203     C                   GOTO      LEG620
207700961203     C                   END
207800970121     C*
207900970121     C                   MOVE      ANMFLS        KFLS
208000970121     C                   MOVE      ANMLNA        KLNA
208100970121     C                   MOVE      ANMNRS        KNRS
208200970121     C                   MOVE      ANMSCN        KNSC
208300970121     C*
208400970121     C* SE BIS CERCO SPUNTA DEFLUENZA
208500970117     C   22              EXSR      CHKBRV
208600961203     C*
208700970117     C     *IN22         IFEQ      *ON
208800970117     C     *IN09         ANDEQ     *OFF
208900970117     C     *IN22         OREQ      *OFF
209000961118     C* DETERMINO QUANDO IL COLLO E' STATO DATO PER PARTITO
209100961118     C                   EXSR      RCRDTP
209200961205     C                   EXSR      GEDE6A
209300970117     C                   END
209400961121     C*
209500961203     C     LEG620        TAG
209600961129     C                   Z-ADD     620           KCAA
209700990519     C   20KANM10        READE     FNANM10L                               31
209800990519     C  N20KANM10        READE(N)  FNANM10L                               31
209900961121    1C                   ENDDO
210000961121     C*----------------------------------------------------------------
210100961120     C* CONTROLLO DISCORDANZE 6  - COLLO DATO PER INOLTRATO MA A TERRA
210200961121     C*----------------------------------------------------------------
210300970117     C* LEGGO ANOMALIE 600 PER LNA SOLO SE NO BIS
210400970117   1AC     *IN22         IFEQ      *OFF
210500961115     C                   Z-ADD     600           KCAA
210600990519     C     KANM10        SETLL     FNANM10L
210700990519     C   20KANM10        READE     FNANM10L                               31
210800990519     C  N20KANM10        READE(N)  FNANM10L                               31
210900961121    1C     *IN31         DOWEQ     *OFF
211000961203     C* ESCLUDO LOCALI
211100961203     C     ANMLNP        IFGT      *ZEROS
211200961203     C     ANMLNP        ANDEQ     ANMLNA
211300961203     C     ANMLNP        OREQ      *ZEROS
211400961203     C     ANMFLS        ANDEQ     ANMLNA
211500990519     C   20              UNLOCK    FNANM10L
211600961203     C                   GOTO      LEG600
211700961203     C                   END
211800961203     C*
211900961118     C* DETERMINO DATA IN CUI IL COLLO E' STATO DATO PER PARTITO
212000961121     C                   MOVE      ANMFLS        KFLS
212100961121     C                   MOVE      ANMLNA        KLNA
212200961121     C                   MOVE      ANMNRS        KNRS
212300961121     C                   MOVE      ANMSCN        KNSC
212400961118     C                   EXSR      RCRDTP
212500961205     C*
212600961205     C                   MOVE      ANMFLE        WFIL
212700961205     C                   MOVE      ANMNPS        WNPS
212800961205     C*
212900961205     C                   SETON                                        09
213000961205     C* IL COLLO PUO' NON ESSERE PIU' A TERRA SE L'ANOMALIA 600 CHE
213100961205     C* STO LEGGENDO HA DATA < ALLA DATA DEL FOGLIO DI DEFL. CHE STO
213200961205     C* CHIUDENDO
213300961212     C     ANMDAO        IFLT      D82DFV
213400961205     C                   Z-ADD     645           KCAA
213500961205     C                   EXSR      CTRANM
213600961205     C                   END
213700961205     C*
213800961205    2C     *IN09         IFEQ      *ON
213900961127     C                   MOVE      'E6 '         KCEV
214000970117     C* STAMPO LA DISCORDANZA
214100970117     C     D82LDS        IFEQ      'SI'
214200970117     C                   EXSR      PRTDIS
214300970117     C                   END
214400961115     C*
214500961113     C* ELABORAZIONE DEFINITIVA
214600961205     C   20              EXSR      CRTEVS
214700961121     C*
214800961121     C*
214900961121     C* COMPLETO AGGIORNAMENTI SE ELABORAZIONE DEFINITIVA
215000961121     C* SE C'E' RECORD BOLLA AGGIORNO ARTDTR=0
215100961205    3C     *IN20         IFEQ      *ON
215200961205    4C     WBOLLA        IFEQ      'S'
215300961121     C                   MOVE      *ZEROS        ARTDTR
215400961121     C                   UPDATE    FNART000
215500961205    4C                   END
215600961121     C* CANCELLO ANOMALIA
215700990519     C                   DELETE    FNANM000
215800961205    3C                   END
215900961205   X2C                   ELSE
216000961205     C* IL COLLO NON E' PIU' A TERRA MA E' IN PARTENZA
216100961205     C                   CLEAR                   WFIL
216200961205     C                   EXSR      GEDE6A
216300961205    2C                   END
216400961203     C*
216500961203     C     LEG600        TAG
216600961203     C*
216700961205     C                   Z-ADD     600           KCAA
216800990519     C   20KANM10        READE     FNANM10L                               31
216900990519     C  N20KANM10        READE(N)  FNANM10L                               31
217000961121    1C                   ENDDO
217100970117   1AC                   END
217200961113     C                   ENDSR
217300060913     c*****************************************************************
217400060913     C* VERIFICO ESISTENZA DISCORDANZA E7-spedizione parziale ma invio
217500060913     C*                                  il collo
217600060913     c*****************************************************************
217700060913     c     CTRE7x        BEGSR
217800060913     C                   MOVE      'E7 '         KCEV
217900060913     C     KEVS2         SETGT     FNEVS02L
218000060913     C     KEVS2         READPE    FNEVS02L                               31
218100060913    4C     *IN31         IFEQ      *ON
218200060913     C* CREO E STAMPO DISCORDANZA E7
218300060913     C                   EXSR      GEDIS27
218400060913   X4C                   ELSE
218500060913   4AC     EVSDEV        IFLT      D82DFV
218600060913     C     *IN22         ANDEQ     *OFF
218700060913     C* ESISTE EVENTO 2 CREATO IN UN GIORNO PRECEDENTE --> MEMORIZZO LA
218800060913     C* DATA E LA PISTOLA
218900060913     C                   MOVE      EVSDEV        WDEV
219000060913     C                   MOVE      EVSNPS        WNPS
219100060913     C* CERCO ANCHE SE ESISTE EVENTO 7A
219200060913     C                   MOVE      'E7A'         KCEV
219300060913     C* SE TROVO E7A IN DATA DEL FOGLIO NON FACCIO NIENTE
219400120823     C     KEVS2A        SETLL     FNEVS02L                               31
219500060913     C     *IN31         CABEQ     *ON           fineCTRE7
219600060913     C     KEVS2         SETGT     FNEVS02L
219700060913     C     KEVS2         READPE    FNEVS02L                               31
219800060913    5C     *IN31         IFEQ      *OFF
219900060913     C* TENGO LA DATA EVENTO MAGGIORE FRA EVENTO E2 ED EVENTO E2A
220000060913    6C     EVSDEV        IFGT      WDEV
220100060913     C                   MOVE      EVSDEV        WDEV
220200060913    6C                   END
220300060913    5C                   END
220400060913     C* CERCO SE ESISTE EVENTO 6-COLLO DATO PER INOLTRATO MA A TERRA
220500060913     C* IN DATA > WDEV
220600060913     C                   MOVE      'E6 '         KCEV
220700060913     C     KEVS2B        SETGT     FNEVS02L
220800060913     C     KEVS2         READE     FNEVS02L                               31
220900060913    5C     *IN31         IFEQ      *OFF
221000060913     C     EVSDEV        ANDLT     D82DFV
221100060913     C                   MOVE      'E7 '         KCEV
221200060913     C                   EXSR      GEDIS27
221300060913   X5C                   ELSE
221400060913     C* CREO EVENTO E7A SOLO SE IL COLLO NON E' IN PARTENZA OGGI
221500060913     C                   Z-ADD     620           KCAA
221600060913     C     KANM2         SETLL     FNANM02L                               31
221700060913     C  N31              MOVE      'E7B'         KCEV
221800120823     C  N31KEVS2A        SETLL     FNEVS02L                               31
221900060913    6C     *IN31         IFEQ      *OFF
222000060913     C                   MOVE      'E7A'         KCEV
222100060913     C                   EXSR      GEDIS27
222200060913    6C                   END
222300060913    5C                   END
222400060913   4AC                   END
222500060913    4C                   END
222600060913    cc     FINECTRE7     ENDSR
222700961205     C*****************************************************************
222800961205     C* STAMPA + CREAZIONE DISCORDANZA 6A
222900961205     C*****************************************************************
223000961205     C     GEDE6A        BEGSR
223100970124     C* VERIFICO SE ESISTE GIA' DISCORDANZA: POTREBBE ESSERE, INFATTI,
223200970124     C* GIA' STATA CREATA DALLA PRECEDENTE CHIUSURA DEL BIS
223300970124     C* SE ESISTE GIA' NON FACCIO NIENTE
223400970124     C                   MOVEL     'E6A'         KCEV
223500970124     C     KEVS2A        SETLL     FNEVS02L                               31
223600060913     c                   if        not *in31
223700060913     C                   MOVEL     'E6B'         KCEV
223800060913     C     KEVS2A        SETLL     FNEVS02L                               31
223900060913     c                   endif
224000060913     c                   if        not *in31
224100060913     C                   MOVEL     'E7B'         KCEV
224200060913     C     KEVS2A        SETLL     FNEVS02L                               31
224300060913     c                   endif
224400060913     c
224500970124    1C     *IN31         IFEQ      *OFF
224600961205     C*
224700961205     C* DETERMINO NUMERO PISTOLA E SE ESISTE SPUNTA DI DEFLUENZA
224800970117     C  N22              EXSR      CHKBRV
224900970117     C*
225000970117     C     WBOLLA        IFEQ      'S'
225100060913     c* Se spedizioni completa E6A, se non inviata bolla per
225200060913     c*  invio solo parziale, E7B
225300060913     c                   if        d82isc=' ' or arbft3='T'
225400970117     C                   MOVE      'E6A'         KCEV
225500060913     c                   else
225600060913     C                   MOVE      'E7B'         KCEV
225700060913     c                   endif
225800060913     c
225900970117     C                   ELSE
226000970117     C                   MOVE      'E6B'         KCEV
226100970117     C                   END
226200970117     C* STAMPO DISCORDANZA
226300970117     C     D82LDS        IFEQ      'SI'
226400970117     C                   EXSR      PRTDIS
226500970117     C                   END
226600970117     C*
226700961205     C* ELABORAZIONE DEFINITIVA
226800961205    2C     *IN20         IFEQ      *ON
226900970117     C* RICHIAMO ROUTINE PER AUTOGENERARE SPUNTA PARTENZA SE NON C'E'
227000970117     C* E CHIUDERE EVENTUALI ANOMALIE 145 E 146 DEI GIORNI PRECEDENTI
227100970117     C     WBOLLA        COMP      'S'                                424243
227200970117     C                   EXSR      PARCOL
227300970117     C*
227400961205     C                   EXSR      CRTEVS
227500961205     C*
227600961205     C* SE ELAB.DEFINITIVA E C'è REC.BOLLA AGGIORNO ARTDTR=DATA EVENTO
227700961205    3C     WBOLLA        IFEQ      'S'
227800961205     C                   MOVE      EVSDEV        ARTDTR
227900961205     C                   UPDATE    FNART000
228000961205    3C                   END
228100961205     C* CANCELLO ANOMALIA
228200990519     C                   DELETE    FNANM000
228300961205    2C                   END
228400970124    1C                   END
228500961205     C                   ENDSR
228600961120     C*****************************************************************
228700060913     C* STAMPA + CREAZIONE DISCORDANZA E2/E2A/E7/E7A
228800961120     C*****************************************************************
228900060913     C     GEDIS27       BEGSR
229000961120     C*
229100961121     C* SE DISCORDANZA 2 DETERMINO SE ESISTE LA SPUNTA DI DEFLUENZA
229200961121     C* E IL NUMERO PISTOLA
229300961121     C     KCEV          IFEQ      'E2 '
229400970117     C     *IN22         ANDEQ     *OFF
229500060913     c
229600060913     C     KCEV          orEQ      'E7 '
229700060913     C     *IN22         ANDEQ     *OFF
229800961121     C                   EXSR      CHKBRV
229900961120     C                   END
230000970117     C*
230100961127     C                   CLEAR                   WAAS
230200961127     C                   CLEAR                   WLNP
230300961127     C                   CLEAR                   WNSP
230400970117     C***
230500970117     C*  STAMPO DISCORDANZA
230600970117     C     D82LDS        IFEQ      'SI'
230700970117     C                   EXSR      PRTDIS
230800970117     C                   END
230900961120     C****
231000060913     C* CREO EVENTO
231100961205    4C     *IN20         IFEQ      *ON
231200970117     C* SOLO SE CREO DISCORDANZA 2 (ESCLUDO QUINDI SE DISC. 2A):
231300970117     C* RICHIAMO ROUTINE PER AUTOGENERARE SPUNTA PARTENZA SE NON C'E'
231400970117     C* E CHIUDERE EVENTUALI ANOMALIE 145 E 146 DEI GIORNI PRECEDENTI
231500970117     C     KCEV          IFEQ      'E2 '
231600060913     C     KCEV          oreq      'E7 '
231700970117     C                   SETON                                        42
231800970117     C                   EXSR      PARCOL
231900970117     C                   END
232000970117     C*
232100961120     C                   EXSR      CRTEVS
232200961120     C*
232300961120    4C                   ENDIF
232400961120     C*
232500961120     C                   ENDSR
232600961121     C*****************************************************************
232700961121     C*    DETERMINO ESISTENZA SPUNTA DI DEFLUENZA E NUMERO PISTOLA   *
232800961121     C*****************************************************************
232900961121     C     CHKBRV        BEGSR
233000961121     C*
233100961121     C                   CLEAR                   WFNPS
233200961121     C                   Z-ADD     1             KNPG
233300070123     C     KBRV1         CHAIN(n)  FNBRV01L                           31
233400961121     C     *IN31         IFEQ      *OFF
233500961121     C                   MOVE      BRVNPS        WNPS
233600961121     C                   ELSE
233700961121     C                   MOVE      88            WNPS
233800961122     C                   MOVE      '1'           WFNPS             1
233900961121     C                   END
234000970116     C*
234100970116     C* SE FV BIS IMPOSTO *IN09
234200970117     C   22WFNPS         IFEQ      '1'
234300970116     C                   SETON                                        09
234400970116     C                   ELSE
234500970116     C                   SETOFF                                       09
234600970116     C                   END
234700961121     C                   ENDSR
234800961118     C*****************************************************************
234900961118     C*    DETERMINO QUANDO COLLO E' STATO DATO PER PARTITO           *
235000961118     C*****************************************************************
235100961118     C     RCRDTP        BEGSR
235200961118     C*
235300961118     C                   CLEAR                   WDTPAR
235400961118     C                   MOVE      *BLANKS       WBOLLA            1
235500961118     C*
235600961118     C  N20KART27        CHAIN(N)  FNART27L                           31
235700961118     C   20KART27        CHAIN     FNART27L                           31
235800961118     C     *IN31         IFEQ      *OFF
235900961118     C                   MOVE      'S'           WBOLLA
236000961118     C                   MOVE      ARTDTR        WDTPAR
236100961118     C                   MOVE      ARTAAS        WAAS
236200961118     C                   MOVE      ARTLNP        WLNP
236300961118     C                   MOVE      ARTNSP        WNSP
236400060913     c
236500060913     c* Se devo inviare soltanto le spedizioni complete, chaino anche
236600060913     c*  la bolla per sapere se inviata
236700060913     c                   if        d82isc='S'
236800060913     c     karb1         chain(n)  fnarb01l
236900060913     c                   endif
237000060913     c
237100961118     C                   ELSE
237200970602     C                   CLEAR                   WAAS
237300961118     C                   MOVE      ANMAAS        WAAS
237400961118     C                   MOVE      ANMLNP        WLNP
237500961118     C                   MOVE      ANMNSP        WNSP
237600961118     C                   MOVE      'E2 '         KCEV
237700961121     C     KEVS2         SETGT     FNEVS02L
237800961127     C     KEVS2         READPE    FNEVS02L                               31
237900961118     C  N31              MOVE      EVSDEV        WDTPAR
238000060913     c*
238100961118     C                   END
238200961118     C                   ENDSR
238300961112     C*****************************************************************
238400961112     C*    SCRITTURA EVENTO                                           *
238500961112     C*****************************************************************
238600961113     C     CRTEVS        BEGSR
238700961113     C                   CLEAR                   FNEVS000
238800961115     C*
238900961115     C                   MOVE      WAAS          EVSAAS
239000961115     C                   MOVE      WLNP          EVSLNP
239100961115     C                   MOVE      WNSP          EVSNSP
239200961115     C*
239300961115     C                   MOVE      KFLS          EVSFLS
239400961115     C                   MOVE      KLNA          EVSLNA
239500961115     C                   MOVE      KNRS          EVSNRS
239600961115     C                   MOVE      KNSC          EVSNSC
239700961120     C                   MOVE      WNPS          EVSNPS
239800961115     C*
239900961212     C                   MOVE      D82DFV        EVSDEV
240000961115     C                   MOVE      KCEV          EVSCEV
240100961115     C* SE COLLO A TERRA MEMORIZZO FILIALE CHE HA SPARATO IL COLLO
240200961115     C                   MOVE      WFIL          EVSFIL
240300961115     C*
240400961115     C* EVSDAT --> PER ORA MI SERVE PER DISCORDANZA 6 PER MEMORIZZARE
240500961115     C*            LA DATA IN CUI IL COLLO ERA STATO DATO PER PARTITO
240600961115     C                   MOVE      WDTPAR        EVSDAT
240700961125     C     WDTPAR        IFGT      *ZEROS
240800961115     C                   MOVEL     COS1          EVSDES
240900961125     C                   END
241000961115     C*
241100961115     C                   WRITE     FNEVS000
241200120823     C*
241300961112     C                   ENDSR
241400961114     C*****************************************************************
241500961122     C*    RICERCA ANOMALIA 645  IN DATA = DATA FOGLIO DEFLUENZA      *
241600961114     C*****************************************************************
241700961114     C     CTRANM        BEGSR
241800020225     C                   SETOFF                                       09
241900020225     C     KANM2A        CHAIN     FNANM02L                           31
242000020225     C  N31              SETON                                        09
242100961114     C                   ENDSR
242200961115     C*****************************************************************
242300961115     C*    OPERAZIONI PER COLLO CHE PARTE                             *
242400961115     C*****************************************************************
242500961115     C     PARCOL        BEGSR
242600961122     C* CHIUSURA ANOMALIE 145 E 146
242700961122     C                   Z-ADD     145           KCAA
242800961118    1C                   DO        2
242900990519     C     KANM2         CHAIN     FNANM02L                           31
243000961122    2C     *IN31         IFEQ      *OFF
243100020225     C                   CLEAR                   FNLR33DS
243200961212     C                   MOVE      D82DFV        D33DCH
243300961115     C                   MOVE      ANMNR2        D33NRR
243400961115     C* PRENDO FASE ANOMALIA
243500961115     C                   CALL      'TRUL11R'
243600961115     C                   PARM                    PARAM9
243700961115     C                   MOVE      FASE          D33FSC
243800961115     C                   CALL      'FNLR33R'
243900020225     C                   PARM                    FNLR33DS
244000961118    3C                   END
244100961115     C* RIPETO GIRO PER 146
244200961115     C                   Z-ADD     146           KCAA
244300961118    1C                   ENDDO
244400961115     C*
244500961115     C* AUTOGENERAZIONE SPUNTA PARTENZA 88 SE NON ESISTE
244600961120     C     WFNPS         IFEQ      '1'
244700961120     C* SE DEVO AUTOGENERARE SPUNTA PARTENZA DI COLLO CON MANCA RECORD
244800961120     C* BOLLA ARRIVO CERCO LA ZONA LEGGENDO UNA QUALUNQUE SPUNTA DEL
244900961120     C* COLLO
245000961120     C     *IN42         IFEQ      *ON
245100961120     C                   CLEAR                   WRVZNC
245200070123     C     KBRV7         CHAIN     FNBRV07L                           31
245300961120     C     *IN31         IFEQ      *OFF
245400961120     C                   MOVE      BRVZNC        WRVZNC
245500961120     C                   END
245600961120     C                   END
245700980126     C*
245800980126     C                   EXSR      DOPPIE
245900980126     C*
246000140625     C   40              CLEAR                   FNBRV5
246100040303     c  n40*in41         ifeq      *off
246200040303    7C     NST(WA)       orne      '1'                                          NR.SPUNTE GG.
246300070123     C                   CLEAR                   FNBRV000
246400040303     c                   else
246500140625     C                   CLEAR                   FNBRV5
246600040303     c                   endif
246700961115     C*
246800961212     C                   Z-ADD     SIMFEL        BRVFGS                         FIL GESTIONE
246900000208     C                   Z-ADD     SIMFEL        BRVPES                         FIL GESTIONE
247000000208     C                   Z-ADD     D82NFV        BRVNFV                         NUM.FOGLIO
247100961115     C                   Z-ADD     1             BRVNPG                         CAT.FOGLIO
247200961115     C                   Z-ADD     SIMFEL        BRVFLE                         FIL.ELABORAT
247300070220     c                   time                    wtimez
247400070220     c* millisecondi
247500070220     c                   extrct    wtimez:*ms    millisec          6 0
247600070220     c                   movel     millisec      brvmis
247700070220     c* data immissione
247800070220     c                   movel     wtimez        dataiso
247900070220     c                   movel     dataiso       brvdfs
248000070220     c* ora immissione
248100070220     c                   move      wtimez        oraiso
248200070220     c                   movel     oraiso        brvhms
248300070220     c*
248400070220     c                   movel     'A'           brvtap
248500070220     c                   eval      brvpru='§'+ nompgm
248600961204     C                   MOVE      KFLS          BRVLNP
248700961204     C                   MOVE      KLNA          BRVLNA
248800961204     C                   MOVE      KNRS          BRVNRS
248900961204     C                   MOVE      KNSC          BRVNSC
249000961115     C*
249100961204     C*  DA ANOMALIE
249200961204     C   42              MOVE      WRVZNC        BRVZNC                          ZONA
249300961204     C*  DA FNART
249400961204     C   43              MOVE      ARBZNC        BRVZNC                          ZONA
249500961115     C*
249600961115     C                   Z-ADD     88            BRVNPS
249700961115     C*
249800140625     C   40              WRITE     FNBRV5
249900040303     c  n40*in41         ifeq      *off
250000040303    7C     NST(WA)       orne      '1'                                          NR.SPUNTE GG.
250100070123     C                   update    fnbrv000
250200040303     c                   else
250300140625     C                   update    fnbrv5
250400040303     c                   endif
250500961115     C                   END
250600961115     C*
250700961115     C                   ENDSR
250800980126     C**************************************************************************
250900040303     C* VERIFICO SE esiste già la spunta:
251000040303     C* 1) per p.o. se previsto dalla categoria, tabella 7n (4)
251100040303     C* 2) per num foglio (npg+nfv+fgs) se da tenere tutte
251200980126     C**************************************************************************
251300980126     C     DOPPIE        BEGSR
251400040303     C*
251500040303     C                   Z-ADD     1             KNPG                           CATEGORIA
251600040303     C                   Z-ADD     SIMFEL        KFLE
251700040303     C*
251800040303     C* VERIFICO IL FLAG NELLA DS7N
251900040303     C                   Z-ADD     1             WA                3 0
252000040303     C     1             LOOKUP    CAT(WA)                                41     ON=TROVATO
252100040303     c     *in41         ifeq      *off
252200040303    7C     NST(WA)       orne      '1'                                          NR.SPUNTE GG.
252300140625     C     KBRV1         CHAIN     FNBRV01L                           40
252400040303     c                   else
252500140625     C     KBRV3         CHAIN     FNBRV05L                           40
252600040303     c                   endif
252700040303     c
252800070123     C* SE C'E' L'ANOMALIA "E" SCriVO FILE FNBRVE0F
252900040303     C  n40BRVCAN        IFEQ      'E'
253000070123     C     KBRVE         SETLL     FNBRVE1L                               99
253100070123     C  N99              WRITE     FNBRV00E
253200040303     C                   ENDIF
253300980126     C*
253400980126     C                   ENDSR
253500961112     C*****************************************************************
253600961112     C*    CHAIN TABEL00F                                             *
253700961112     C*****************************************************************
253800961112     C     CTABEL        BEGSR
253900961112     C     KTAB          CHAIN     TABEL                              31
254000961114     C  N31TBLFLG        IFNE      ' '
254100961112     C                   SETON                                        31
254200961112     C                   END
254300961112     C                   ENDSR
254400961129     C**************************************************************************
254500961129     C*  ABBLANCA ZERO NON SIGNIFICATIVI
254600961129     C**************************************************************************
254700961129     C     ABBL          BEGSR
254800961129     C*
254900020225     C                   Z-ADD     1             YY
255000020225     C     K08(YY)       DOWEQ     '0'
255100020225     C     YY            ANDLT     8
255200020225     C                   MOVEL     ' '           K08(YY)
255300020225     C                   ADD       1             YY
255400961129     C                   ENDDO
255500961129     C*
255600961129     C                   ENDSR
255700961121     C*****************************************************************
255800961121     C* STAMPA DISCORDANZE -------------------------------------------*
255900961121     C*****************************************************************
256000961121     C     PRTDIS        BEGSR
256100961121     C*
256200961122     C                   SETON                                        08
256300961125     C*
256400961125     C     WFIL          COMP      *ZEROS                                 14
256500961127     C*
256600961127     C     WNSP          COMP      *ZEROS                             1515
256700961127     C     KNSC          COMP      *ZEROS                             1616
256800961127     C     KNRS          COMP      *ZEROS                             1717
256900961128     C*
257000961129     C     KCEV          COMP      'E4 '                                  18
257100060918     C  n18KCEV          COMP      'E8 '                                  18
257200961127     C*
257300961127     C* STAMPO ANNO DI 2
257400961127     C                   MOVE      WAAS          WPAAS             2 0
257500961122     C*
257600961122     C* DECODIFICO CAUSALE EVENTO
257700961122     C                   MOVE      '8E'          KCOD
257800961122     C                   MOVEL(P)  KCEV          KKEY
257900970114     C* prendo la descrizione in italiano
258000970114     C     KTAB          CHAIN     TABEL00F                           31
258100970114     C  N31              MOVEL     TBLUNI        DCEV             60
258200970114     C   31              CLEAR                   DCEV
258300970114     C* se devo inviarlo anche via fax prendo anche la descrizione in
258400970114     C* inglese
258500120821     c     d82ftt        ifne      FEDEX
258600970115     C   20              MOVE      '1'           KKEY
258700970115     C   20KTAB          CHAIN     TABEL00F                           31
258800970115     C  N31
258900970115     CAN 20              MOVEL     TBLUNI        DCEVE            60
259000970115     C   31
259100970115     CAN 20              CLEAR                   DCEVE
259200970115    1C   20*INOB         IFEQ      *ON
259300040423
259400040423     c                   if        FAX_Disc_Est = 'S'
259500961122     C                   ADD       1             WPAG
259600961121     C                   EXCEPT    TESDIS
259700970114     C                   SETOFF                                       OB
259800040423     c                   end
259900040423
260000961129    1C                   END
260100040423
260200040423     c                   if        FAX_Disc_Est = 'S'
260300970115     C   20              EXCEPT    DETDIS
260400040423     c                   end
260500040423
260600020301     c                   endif
260700970114    1C     *INOE         IFEQ      *ON
260800970114     C                   ADD       1             WPAG1
260900970114     C                   EXCEPT    TESDIC
261000970114     C                   SETOFF                                       OE
261100970114    1C                   END
261200970114     C                   EXCEPT    DETDIC
261300961122     C* STAMPA DATA PARTENZA COLLO SE DISCORDANZA 6 O 6A
261400961129    1C     WDTPAR        IFGT      *ZEROS
261500961122     C                   CLEAR                   WLBDA8
261600961122     C                   MOVE      WDTPAR        G02INV
261700961122     C                   MOVE      '3'           G02ERR
261800961122     C                   CALL      'XSRDA8'
261900961122     C                   PARM                    WLBDA8
262000961122     C                   MOVE      G02DAT        WDTPA1
262100120821     c     d82ftt        ifne      FEDEX
262200970115    2C   20*INOB         IFEQ      *ON
262300040423
262400040423     c                   if        FAX_Disc_Est = 'S'
262500961122     C                   ADD       1             WPAG
262600961122     C                   EXCEPT    TESDIS
262700970114     C                   SETOFF                                       OB
262800040423     c                   end
262900040423
263000961129    2C                   END
263100040423
263200040423     c                   if        FAX_Disc_Est = 'S'
263300970115     C   20              EXCEPT    DETDI2
263400970115     C   20              EXCEPT    FIT
263500040423     c                   end
263600040423
263700020301     c                   endif
263800970114    2C     *INOE         IFEQ      *ON
263900970114     C                   ADD       1             WPAG1
264000970114     C                   EXCEPT    TESDIC
264100970114     C                   SETOFF                                       OE
264200970114    2C                   END
264300970114     C                   EXCEPT    DETDC2
264400970114     C                   EXCEPT    FITC
264500961129   X1C                   ELSE
264600961129     C     KCEV          IFNE      'E4 '
264700120821     c     d82ftt        ifne      FEDEX
264800040423
264900040423     c                   if        FAX_Disc_Est = 'S'
265000970115     C   20              EXCEPT    FIT
265100040423     c                   end
265200040423
265300020301     c                   endif
265400970114     C                   EXCEPT    FITC
265500961129     C                   END
265600961129    1C                   END
265700961121     C*
265800961121     C                   ENDSR
265900960529     C*-----------------------------------------------------------------*
266000960529     C*  Operazioni iniziali
266100960529     C*-----------------------------------------------------------------*
266200960529     C     *INZSR        BEGSR
266300960529     C*
266400960529     C*  Reperisco dati da video
266500960529     C     *ENTRY        PLIST
266600960529     C                   PARM                    KPJBA
266700020225     C                   MOVEL     KPJBU         TRTC82DS
266800040423
266900040423     ** Maggio 2004:
267000120823     ** Non si vuole più mandare il FAX discordanze al Partner
267100040423     c                   move      'N'           FAX_Disc_Est      1
267200040423
267300070123     C* MUOVO N.FOGLIO IN CAMPO LUNGO 6,0 PER CHAIN SU FNBRV
267400000208     C                   Z-ADD     D82NFV        D82NFS
267500990421      * SALVO KPJBU "ORIGINALE"
267600990421     C                   MOVEL     KPJBU         WKPJBU
267700971230      *
267800971230     C* MI SALVO LINEA DI ARRIVO ESTERA (ALTRIMENTI SI SPORCA NEL
267900971230     C* RICHIAMARE IL TRTC83R2)
268000961211     C                   MOVE      D82LNP        LNAEXP
268100961129     C*  Controllo se devo scrivere il nr.spedizione compattato
268200961129     C     D82SPC        COMP      'S'                                    21
268300960529     C*  Richiamo XPARUT
268400960529     C                   Z-ADD     1             CODUT
268500960529     C                   CALL      'X§PARUT'
268600020225     C                   PARM                    UT§DSE0F
268700960529     C                   MOVEL     REC80         CNCR80
268800110511     C                   MOVEL     REC42         CNCR42
268900110511     C                   MOVEL     simRGS        RSUT             20
269000110511     C                   MOVEL     simRGS        RSUTM            19
269100020225     C* RICERCA CAPOCONTI
269200961122     C                   DO        50            X                 2 0
269300961122     C                   MOVE      TCU(X)        TCUDS
269400961122     C     F56           IFEQ      'CG'
269500961122     C     F4            ANDEQ     '1'
269600961122     C     F3            ANDEQ     '0'
269700961122     C                   Z-ADD     KCU(X)        KCI               4 0
269800020225     C                   ENDIF
269900020225     C                   ENDDO
270000961122     C* REPERISCO KG PER METRO CUBO PER CALCOLO PESO TASSABILE
270100961122     C                   MOVEL     '7A'          KCOD
270200961122     C                   MOVEL(P)  '2'           KKEY
270300961122     C     KTAB          CHAIN     TABEL00F                           31
270400961122     C  N31              MOVEL     TBLUNI        DS7A2
270500961122     C   31              CLEAR                   DS7A2
270600150825     C*
270700150825     C** CARICAMENTO DATI VARI TASSAZIONE
270800150825     C                   MOVEL     'QT'          kcod
270900150825     C                   MOVEL(P)  '1'           kKEY
271000150825     C     KTAB          CHAIN     TABEL00F                           31
271100150825     C  N31              MOVEL     TBLUNI        DSQT1
271200150825     C   31              CLEAR                   DSQT1
271300150825     C*
271400980126     C* SERVE SAPERE SE NELLE VARIE CATEGORIE FOGLI POSSO
271500980126     C* AVERE SPUNTE DOPPIE OPPURE NO
271600980126     C                   Z-ADD     *ZERO         WA                             CONTATORE
271700980126     C                   MOVEL     '7N'          KCOD
271800980126     C     KTAB2         SETLL     TABEL00F
271900980126     C     KTAB2         READE     TABEL00F                               31
272000980126     C     *IN31         DOWEQ     *OFF
272100980126     C     TBLFLG        IFEQ      *BLANKS
272200980126     C                   ADD       1             WA
272300980126     C                   MOVEL     TBLUNI        DS7N
272400980126     C                   MOVEL     TBLKEY        CAT(WA)
272500980126     C                   MOVEL     §7NNST        NST(WA)
272600980126     C                   ENDIF
272700980126     C     KTAB2         READE     TABEL00F                               31
272800980126     C                   ENDDO
272900990412      * CARICO TABELLA £1 SE NON SONO IN SEDE
273000990412     C     SIMFEL        IFNE      *ZEROS
273100020225     C                   CLEAR                   TRUL06DS
273200990412     C                   CLEAR                   KPJBU
273300990412     C                   MOVEL     '£1'          D06COD
273400990412     C                   MOVEL     SIMFEL        D06KEY
273500990412     C                   MOVEL     'L'           D06TLA
273600020225     C                   MOVEL     TRUL06DS      KPJBU
273700990412      *
273800990412     C                   CALL      'TRUL06R'
273900990412     C                   PARM                    KPJBA
274000020225     C                   MOVEL     KPJBU         TRUL06DS
274100990421      * REIMPOSTO KPJBU "ORIGINALE"
274200990421     C                   MOVEL     WKPJBU        KPJBU
274300990421     C                   END
274400090402     c
274500090402     c                   open(e)   tita430c
274600090402     c                   if        not %open(tita430c)
274700090402     c                   eval      %subst(WTA430:7:4)='GRPS'
274800090402     c                   eval      %subst(WTAS30:7:4)='GRPS'
274900090402     c                   open      tita430c
275000090402     c                   endif
275100090402     c                   open      titas30c
275200130215     c
275300960529     C*---------------------------------------------------------------*
275400960529     C* Definisco chiavi di accesso ai file
275500961114     C     KARB1         KLIST
275600961114     C                   KFLD                    ARTAAS
275700961114     C                   KFLD                    ARTLNP
275800961114     C                   KFLD                    ARTNRS
275900961114     C                   KFLD                    ARTNSP
276000961113     C     KARB55        KLIST
276100961211     C                   KFLD                    LNAEXP
276200961113     C                   KFLD                    KDT3
276300120823     C     KARB          KLIST
276400961121     C                   KFLD                    ARBAAS
276500961121     C                   KFLD                    ARBLNP
276600961121     C                   KFLD                    ARBNRS
276700961121     C                   KFLD                    ARBNSP
276800960529     C     KAR4          KLIST
276900961121     C                   KFLD                    ARBAAS
277000961121     C                   KFLD                    ARBLNP
277100961121     C                   KFLD                    ARBNRS
277200961121     C                   KFLD                    ARBNSP
277300960529     C                   KFLD                    KTRC
277400061127      * contatto nome e telefono (Tipo record [GEN])
277500061127     C     KAR5          KLIST
277600061127     C                   KFLD                    ARBAAS
277700061127     C                   KFLD                    ARBLNP
277800061127     C                   KFLD                    ARBNRS
277900061127     C                   KFLD                    ARBNSP
278000061127     C                   KFLD                    KGEN
278100061127     c                   movel     'GEN'         KGEN              3
278200960529     C     KTAB          KLIST
278300961122     C                   KFLD                    CODUT
278400960529     C                   KFLD                    KCOD
278500960529     C                   KFLD                    KKEY
278600980126     C     KTAB2         KLIST
278700980126     C                   KFLD                    CODUT
278800980126     C                   KFLD                    KCOD
278900961220     C     KETAB         KLIST
279000961220     C                   KFLD                    KECOD
279100961220     C                   KFLD                    KEKEY
279200130215     C     KEDI          KLIST
279300130215     C                   KFLD                    tabcod
279400130215     C                   KFLD                    tabkey
279500961113     C     KANM10        KLIST
279600961113     C                   KFLD                    KCAA
279700961211     C                   KFLD                    LNAEXP
279800961115     C     KANM2         KLIST
279900961115     C                   KFLD                    KFLS
280000961115     C                   KFLD                    KLNA
280100961115     C                   KFLD                    KNRS
280200961122     C                   KFLD                    KNSC
280300961115     C                   KFLD                    KCAA
280400961122     C     KANM2A        KLIST
280500961122     C                   KFLD                    KFLS
280600961122     C                   KFLD                    KLNA
280700961122     C                   KFLD                    KNRS
280800961122     C                   KFLD                    KNSC
280900961122     C                   KFLD                    KCAA
281000961212     C                   KFLD                    DFV06
281100961115     C     KEVS2         KLIST
281200961115     C                   KFLD                    KFLS
281300961115     C                   KFLD                    KLNA
281400961115     C                   KFLD                    KNRS
281500961122     C                   KFLD                    KNSC
281600961115     C                   KFLD                    KCEV
281700120823      **
281800961115     C     KEVS2A        KLIST
281900961115     C                   KFLD                    KFLS
282000961115     C                   KFLD                    KLNA
282100961115     C                   KFLD                    KNRS
282200961122     C                   KFLD                    KNSC
282300961115     C                   KFLD                    KCEV
282400961212     C                   KFLD                    D82DFV
282500120823      **
282600961119     C     KEVS2B        KLIST
282700961119     C                   KFLD                    KFLS
282800961119     C                   KFLD                    KLNA
282900961119     C                   KFLD                    KNRS
283000961122     C                   KFLD                    KNSC
283100961119     C                   KFLD                    KCEV
283200961120     C                   KFLD                    WDEV
283300120823      **
283400961121     C     KART27        KLIST
283500961121     C                   KFLD                    KFLS
283600961121     C                   KFLD                    KLNA
283700961121     C                   KFLD                    KNRS
283800961121     C                   KFLD                    KNSC
283900120823      **
284000120823     C     KBRV7         KLIST
284100120823     C                   KFLD                    KFLS
284200120823     C                   KFLD                    KLNA
284300120823     C                   KFLD                    KNRS
284400120823     C                   KFLD                    KNSC
284500120823      **
284600961121     C     KART55        KLIST
284700961211     C                   KFLD                    LNAEXP
284800961121     C                   KFLD                    KDTR
284900961120     C     KBRV1         KLIST
285000961118     C                   KFLD                    KNPG
285100000208     C                   KFLD                    D82NFS
285200961212     C                   KFLD                    SIMFEL
285300961118     C                   KFLD                    KFLS
285400961118     C                   KFLD                    KLNA
285500961118     C                   KFLD                    KNRS
285600961122     C                   KFLD                    KNSC
285700140625     C     KBRV3         KLIST
285800980126     C                   KFLD                    KNPG                           ..CATEGORIA
285900980126     C                   KFLD                    KFLS                           ..LNP
286000980126     C                   KFLD                    KLNA                           ..LNA
286100980126     C                   KFLD                    KNRS                           ..SERIE
286200980126     C                   KFLD                    KNSC                           ..SEGNACOLLO
286300980126     C                   KFLD                    KFLE                           ..FIL.GESTIONE
286400961122     C*
286500961122     C     KIND          KLIST
286600961122     C                   KFLD                    CODUT
286700961122     C                   KFLD                    KCI
286800961122     C                   KFLD                    D82CPT
286900010613     C     KBRVE         KLIST
287000010613     C                   KFLD                    BRVNPG
287100010613     C                   KFLD                    BRVNFV
287200010613     C                   KFLD                    BRVFGS
287300010613     C                   KFLD                    BRVLNP
287400010613     C                   KFLD                    BRVLNA
287500010613     C                   KFLD                    BRVNRS
287600010613     C                   KFLD                    BRVNSC
287700020808     c     K02FGV01      klist
287800020808     c                   kfld                    D82nfv
287900020808     C                   kfld                    SIMfel
288000110510     C     ktbe          klist
288100110510     C                   kfld                    tbecod
288200110510     C                   kfld                    tbeke1
288300110510     C                   kfld                    tbeke2
288400960529     C*---------------------------------------------------------------*
288500960529     C                   TIME                    UTIME             6 0
288600960529     C* PRENDO LA DATA DEL GIORNO DA TIME
288700960529     C                   TIME                    W0140            14 0
288800960716     C                   MOVE      W0140         UDATE8
288900960716     C                   MOVE      UDATE8        G02DAT
289000960716     C                   MOVEL     *BLANK        G02ERR
289100960716     C                   CALL      'XSRDA8'
289200960716     C                   PARM                    WLBDA8
289300960716     C                   Z-ADD     G02INV        WOGGI             8 0           UDATE
289400130215      *------------
289500130215     C* Caricamento Tabella Partner
289600130215     C                   MOVEL     'PT'          TABCOD
289700130215     C     TABCOD        setll     EDTAB01L
289800130215     C     TABCOD        reade     EDTAB01L
289900130215      *
290000130215     C                   DOW       not %EoF(edtab01l)
290100130215      *
290200130215     C     TABFLG        IFEQ      *BLANKS
290300130215     c                   eval      dsPT = tabuni
290400130215      *
290500130215      * deve uscire con un solo record  tramite il codice KSC del PARTNER
290600130215      *  se ci fossero più codici KSC uguali per motivi particolari il primo
290700130215      *  in ordine di KEY DEVE essere, x regola, quello del Partner mentre il
290800130215      *  secondo è definito come CLIENTE sempre.
290900130215     C     D82CPT        ifeq      §PTKSC
291000130215      *
291100130215      *** al suo interno carica anche l'altra tabella correlata
291200130215      **   in questo caso la PZ e poi esce
291300130215     C                   MOVEL     'PZ'          TABCOD
291400130215     C     Kedi          chain     EDTAB01L
291500130215     c                   if        %Found(edtab01l)
291600130215     c                   eval      dsPZ = tabuni
291700130215     c                   end
291800130215      *
291900130215     c                   leave
292000130215     c                   end
292100130215      *
292200130215     C                   END
292300130215      *
292400130215     C     TABCOD        READE     EDTAB01L
292500130215     C                   END
292600130215      *------------
292700110510      *
292800110510     c                   move      'VPO'         tbecod
292900110510     c                   movel     'BRT'         tbeke1
293000110510     c                   clear                   tbeke2
293100110510     c     ktbe          chain     tntbe01l
293200110510     c                   if        %found(tntbe01l)
293300110510     c                   movel     tbeuni        dvpobrt
293400110510     c                   if        Woggi  <= §VPOBRTDTA
293500110510     c                   seton                                        66
293600110510     c                   endif
293700110510     c                   endif
293800120305      *
293900120305      ***cambio di marchio in BRT finale x il 15/3
294000120305     c                   setoff                                       66
294100120305     c                   if        Woggi  <  20120315
294200120305     c                   seton                                        66
294300120305     c                   endif
294400961114     C*
294500961114     C     D82DEF        COMP      'SI'                                   20
294600970117     C     D82FVB        COMP      'B'                                    22
294700970114     C*
294800970114     C*  IMPOSTO DATA FOGLIO VIAGGIO PER STAMPA
294900970114     C                   Z-ADD     D82DFV        DATA
295000970114     C                   Z-ADD     MM            GGMM              4 0
295100970114     C                   MOVEL     GG            GGMM
295200970114     C                   Z-ADD     AA            PARDFV
295300970114     C                   MOVEL     GGMM          PARDFV
295400970114     C                   MOVE      D82DFV        DFV06
295500970114     C*  Data CMR
295600970114     C                   Z-ADD     D82DCM        DATA
295700970114     C                   Z-ADD     MM            GGMM              4 0
295800970114     C                   MOVEL     GG            GGMM
295900970114     C                   Z-ADD     AA            DATCMR            8 0
296000970114     C                   MOVEL     GGMM          DATCMR
296100020225      *
296200970114     C   20              OPEN      FNEVS01L
296300020225      *
296400020225      * IMPOSTO TIPO RECORD PER LETTURE WORK FILE
296500970113     C  N20              MOVE      'P'           WTRC
296600120821     C   20              MOVE      'D'           WTRC
296700020225      ** LETTURA/AGGIORNAMENTO FITTIZIO PER GIUSTIFICARE L'APERTURA
296800020225      ** IN UPDATE (DEVO FARLO PER UTILIZZARE IL FILE SOTTO COMIT VISTO CHE
296900020225      ** E' UTILIZZATO IN UPDATE NEI PGM DI SCRITTURA RCD PER TRASMISSIONE)
297000970124     C     *LOVAL        SETLL     FNARB01L
297100970124     C                   READ      FNARB01L                               31
297200970124     C  N31              EXCEPT    FITTIZ
297300020225      *
297400960529     C                   ENDSR
297500150825     C*****************************************************************
297600150825     C*  Reperisce Peso/Volume o da CML o da Bollettato
297700150825     C*****************************************************************
297800150825     C     VOL_PES       BEGSR
297900150825     C*
298000150825     c     *like         define    arbvlb        wvol
298100150825     c     *like         define    arbpkb        wpes
298200150825     C     *LIKE         DEFINE    §qttpc        WTARA
298300150825      *
298400150825      * Prima cerca il volume fra bollettato e CML
298500150825     C     arbncR        ifeq      arbncl
298600150825      * se è considerata un'unica spedizione
298700150825     C                   z-add     ARBVLC        WVOL
298800150825     C                   else
298900150825     C*
299000150825     C     ARBVLC        ifge      ARBVLb
299100150825     C                   z-add     ARBVLC        WVOL
299200150825     C                   else
299300150825     C                   z-add     ARBVLb        WVOL
299400150825     C                   end
299500150825     C*
299600150825     C                   endif
299700150825     C* PESO VDL - TARA
299800150825     C     §QTTPC        MULT      arbncp        WTARA
299900150825     C                   Z-ADD     arbPKC        wpes
300000150825     c                   sub(h)    wtara         wpes
300100150825      *
300200150825      * Poi cerca il peso fra bollettato e CML
300300150825     C     arbncP        ifeq      arbncl
300400150825      * se è considerata un'unica spedizione
300500150825     C                   ELSE
300600150825     C*  altrimenti se il bollettato è superiore passo il Bollettato.
300700150825     C     wpes          iflt      arbPKb
300800150825     C                   Z-ADD     arbPKb        wpes
300900150825     C                   end
301000150825     C*
301100150825     C                   ENDIF
301200150825     C*
301300150825     C                   ENDSR
301400150825     C*--------------------------------------------------------------*
301500961023     OFNARB000  E            LIBER
301600961023     OFNARB000  E            AGGTRA
301700961023     O                       ARBFT3
301800961023     O                       ARBDT3
301900961112     OFNARB000  E            AGGFT3
302000961112     O                       ARBFT3
302100970124     OFNARB001  E            FITTIZ
302200961212     O*****************************************************************
302300961212     O* MANIFEST PER IL PARTNER
302400961212     O*****************************************************************
302500961213     OMANPTN    E            TESTA            02
302600041109     O                       D46(1)             112
302700961217     O                       STARAG              75
302800961217     O                       STPFAX              97
302900041109     O              N20                         112 '* PROVVISORIO'
303000041109     O               20                         112 '* DEFINITIVO '
303100961212     O          E            TESTA            03
303200931229     O                       ST(1)               66
303300041109     O                       D46(2)             112
303400961217     O                       UDATE8              54 '  /  /    '
303500961217     O                       UTIME               89 '  :  :  '
303600041109     O                       PAGE1         Z    111
303700961212     O          E            TESTA            04
303800961217     O                       ST(4)               66
303900041109     O                       D46(3)             112
304000961217     O                       RSUTM               63
304100961217     O                       STADLA             106
304200961212     O          E            TESTA            05
304300961217     O                       ST(3)               66
304400041109     O                       D46(4)             112
304500961217     O                       UDATE8              54 '  /  /    '
304600041109     O                       D82CMR             112
304700961212     O          E            TESTA            06
304800961217     O                       ST(19)              66
304900041109     O                       D46(5)             112
305000041109     O                       DATCMR              87 '  /  /    '
305100961212     O          E            TESTA          1 08
305200931229     O                       ST(6)               66
305300041109     O                       D46(6)             112
305400021204      *--------
305500931229     O          E            DET         1
305600100330     O              N21      §LB14               16
305700961129     O               21      §LB8                16
305800941222     O                       STARMN        Z   +  1
305900941222     O                       STARSM            +  1
306000941222     O                       STARSD            +  2
306100940111     O                       STATBO            +  1
306200940111     O                       ARBNCL        Z   +  2
306300961125     O                       PESPRT        4   +  2
306400041109     O                    N34PRTCAS        4   +  2
306500100223     O          E    24      DET         1
306600100223     O                       RagADD45            99
306700931229     O          E            DET         1
306800941221     O                       STADSP              16 '  /  /    '
306900130808     O***************        ARBCAM              36
307000130808     O***************        STALOM              53
307100130808     O                       STAINM              52
307200941222     O                       STAIND              79
307300961011     O                       ARBNAS             100
307400041109     O                    N34AR9VCA             112
307500130808     O          E    25N27   DET         1
307600100223     O                       IndADD10            64
307700130808     O          E    27N25   DET         1
307800130808     O                       IndMAdd10           37
307900130808     O          E    27 25   DET         1
308000130808     O                       IndMAdd10           37
308100130808     O                       IndADD10            64
308200941222     O          E            DET         1
308300940111     O                     02                    13 '* PARTIAL *'
308400130808     O***************        ARBRMA              30
308500130808     O***************                            37 'P.IVA:'
308600130808     O***************        MITCPI              53
308700130808     O                       ARBCAM              36
308800130808     O                       STALOM              53
308900950614     O                       ARBCAD              63
309000950614     O                       STALOD              84
309100950614     O                                           91 'P.IVA:'
309200950614     O                       DESCPI             107
309300100223     O          E    26      DET         1
309400100224     O                       CitADD15            69
309500130808     O          E            DET         1
309600130808     O                       ARBRMA              30
309700130808     O                                           37 'P.IVA:'
309800130808     O                       MITCPI              53
309900961220     O          E   N34      DET         1
310000041109     O                       STATIC             112
310100961121     O          E   N36      DET         1
310200950304     O         OR   N35
310300041109     O                    N35STANT1              75
310400041109     O                    N36STANT2             111
310500961121     O          E   N36      DET         0  1
310600961121     O         OR   N35
310700041109     O                    N35STANT1              75
310800041109     O                    N36STANT2             111
310900100223      * Contatto Nom./tel.
311000100223     O          E   N33      DET         1
311100100223     O                                           37 'Contact Name:'
311200100223     O                       STACNA              72
311300100223     O                                           80 'Tel.:'
311400100223     O                       STACTE             106
311500100223      *
311600941222     O* EXCPT FITTIZIA
311700941222     O          E          36DET         1  0
311800041109     O                                          112 ' '
311900021204     O* valore merce dichiarato
312000021204     O          E            VMDDET         1
312100021204     O                                           37 'Valore Merce:'
312200021204     O                       ARBVMD        2  +   1
312300021204     O                       ARBVAD           +   1
312400021204     O*
312500940111     O          E            TOTAL       1
312600940111     O                       ST(15)              66
312700041109     O                       D46(7)             112
312800940111     O          E            TOTAL       1
312900940111     O                       ST(17)              66
313000041109     O                       D46(8)             112
313100940111     O                       TOTSPE        Z     24
313200940111     O                       TOTNCL        Z     50
313300961125     O                       TOTPES        4     79
313400961125     O                       TOTVLF        4    104
313500940111     O          E            TOTAL       1
313600940111     O                       ST(15)              66
313700041109     O                       D46(7)             112
313800931229     O          E            FINE        1
313900931229     O                                           69 '***  END  OF  PRINT  ***'
314000961121     O*
314100970110     O*****************************************************************
314200970110     O* MANIFEST PER IL PARTNER IN FORMA SINTETICA
314300970110     O*****************************************************************
314400970110     OMANPTS    E            TESSIN           02
314500970110     O                       MAS(1)              66
314600970110     O                       MAS(2)             132
314700970110     O                       RSUT                20
314800970110     O                       SIMFEL              24
314900970110     O                       UDATE8             121 '  /  /    '
315000970110     O                       PAGE1         Z    132
315100970110     O          E            TESSIN           03
315200970110     O                       MAS(3)              66
315300970110     O                       MAS(4)             132
315400970110     O                       KNSIF               10
315500970110     O                       KNMUS               21
315600000208     O                       D82NFV              40
315700000208     O               22                          44 'BIS'
315800970110     O                       STADLS              65
315900970110     O                       STARGS             105
316000970114     O                       UTIME              119 '  :  :  '
316100970110     O          E            TESSIN         1 05
316200970110     O                       MAS(5)              66
316300970110     O                       MAS(7)             132
316400970110     O          E            DETSIN         1
316500970110     O                       ARBAAS               4
316600970110     O                       ARBLNP               8
316700970110     O                       ARBNRS        Z     12
316800970110     O                       ARBNSP        Z     20
316900970110     O                       ARBNAS              37
317000970110     O                       ARBRSM              74
317100020917     O                       ARBRMA              91
317200970110     O                       ARBRMN        Z    108
317300970110     O                       ARBNCL        Z    115
317400970110     O                       PESPRT        4    125
317500970110     O          E            TOTSIN         1
317600970110     O                       MAS(9)              66
317700970110     O                       MAS(9)             132
317800970110     O          E            TOTSIN         1
317900970110     O                       MAS(10)             66
318000970110     O                       MAS(11)            132
318100970110     O                       TOTSPE        Z     24
318200970110     O                       TOTNCL        Z     49
318300970110     O                       TOTPES        4     77
318400970110     O                       TOTVLF        4    103
318500970110     O          E            TOTSIN      1
318600970110     O                       MAS(9)              66
318700970110     O                       MAS(9)             132
318800970110     O          E            FINSIN      1
318900970110     O                                           69 '***   FINE  STAMPA   ***'
319000970110     O*
319100970110     O*
319200961212     O*****************************************************************
319300961212     O* MANIFEST SPEDIZIONI C.O.D.
319400961212     O*****************************************************************
319500961213     OMANCOD    E            TESCOD           02
319600041109     O                       D46(1)             112
319700961217     O                       STARAG              75
319800961217     O                       STPFAX              97
319900041109     O              N20                         112 '* PROVVISORIO'
320000041109     O               20                         112 '* DEFINITIVO '
320100961212     O          E            TESCOD           03
320200961121     O                       ST(1)               66
320300041109     O                       D46(2)             112
320400961217     O                       UDATE8              54 '  /  /    '
320500961217     O                       UTIME               89 '  :  :  '
320600041109     O                       PAGE2         Z    111
320700961212     O          E            TESCOD           04
320800961217     O                       ST(4)               66
320900041109     O                       D46(3)             112
321000961217     O                       RSUTM               63
321100961217     O                       STADLA             106
321200961212     O          E            TESCOD           05
321300961217     O                       ST(3)               66
321400041109     O                       D46(4)             112
321500961217     O                       UDATE8              54 '  /  /    '
321600041109     O                       D82CMR             112
321700961212     O          E            TESCOD           06
321800961217     O                       ST(19)              66
321900041109     O                       D46(5)             112
322000041109     O                       DATCMR              87 '  /  /    '
322100961216     O          E            TESCOD           07
322200041109     O                                          112 '* MANIFEST C.O.D.'
322300961212     O          E            TESCOD         1 08
322400961121     O                       ST(6)               66
322500041109     O                       D46(6)             112
322600961121     O          E            DETCOD      1
322700100330     O              N21      §LB14               16
322800961129     O               21      §LB8                16
322900961121     O                       STARMN        Z   +  1
323000961121     O                       STARSM            +  1
323100961121     O                       STARSD            +  2
323200961121     O                       STATBO            +  1
323300961121     O                       ARBNCL        Z   +  2
323400961125     O                       PESPRT        4   +  2
323500041109     O                    N34PRTCAS        4   +  2
323600961121     O          E            DETCOD      1
323700961121     O                       STADSP              16 '  /  /    '
323800130808     O**************         ARBCAM              36
323900130808     O**************         STALOM              53
324000130808     O                       STAINM              52
324100961121     O                       STAIND              79
324200961121     O                       ARBNAS             100
324300041109     O                    N34AR9VCA             112
324400961121     O          E            DETCOD      1
324500961121     O                     02                    13 '* PARTIAL *'
324600130808     O**************         ARBRMA              30
324700130808     O**************                             37 'P.IVA:'
324800130808     O**************         MITCPI              53
324900130808     O                       ARBCAM              36
325000130808     O                       STALOM              53
325100961121     O                       ARBCAD              63
325200961121     O                       STALOD              84
325300961121     O                                           91 'P.IVA:'
325400961121     O                       DESCPI             107
325500130808     O          E            DETCOD      1
325600130808     O                       ARBRMA              30
325700130808     O                                           37 'P.IVA:'
325800130808     O                       MITCPI              53
325900061127      * Contatto Nome/tel.
326000061127     O          E   N33      DETCOD      1
326100061127     O                                           37 'Contact Name:'
326200061127     O                       STACNA              72
326300061127     O                                           80 'Tel.:'
326400061127     O                       STACTE             106
326500061127     O
326600061127      *
326700961220     O          E   N34      DETCOD      1
326800041109     O                       STATIC             112
326900961121     O          E   N36      DETCOD      1
327000961121     O         OR   N35
327100041109     O                    N35STANT1              75
327200041109     O                    N36STANT2             111
327300961121     O          E   N36      DETCOD      0  1
327400961121     O         OR   N35
327500041109     O                    N35STANT1              75
327600041109     O                    N36STANT2             111
327700961121     O* EXCPT FITTIZIA
327800961121     O          E          36DETCOD      1  0
327900041109     O                                          112 ' '
328000961121     O          E            TOTCOD      1
328100961121     O                       ST(15)              66
328200041109     O                       D46(7)             112
328300961121     O          E            TOTCOD      1
328400961121     O                       ST(17)              66
328500041109     O                       D46(8)             112
328600961204     O                       CODSPE        Z     24
328700961204     O                       CODNCL        Z     50
328800961204     O                       CODPES        4     79
328900961204     O                       CODVLF        4    104
329000961121     O          E            TOTCOD      1
329100961121     O                       ST(15)              66
329200041109     O                       D46(7)             112
329300961121     O          E            FINCOD      1
329400961121     O                                           69 '***  END  OF  PRINT  ***'
329500961212     O*****************************************************************
329600961212     O* MANIFEST PER LA DOGANA
329700961212     O*****************************************************************
329800961213     OMANDGN    E            TESDGN           02
329900041109     O                       D46(1)             112
330000970117     O                       STARSX              75
330100970117     O                       D82DGX              97
330200041109     O*               N20               112 '* PROVVISORIO'
330300041109     O*                20               112 '* DEFINITIVO '
330400961212     O          E            TESDGN           03
330500961212     O                       ST(1)               66
330600041109     O                       D46(2)             112
330700961217     O                       UDATE8              54 '  /  /    '
330800961217     O                       UTIME               89 '  :  :  '
330900041109     O                       PAGE3         Z    111
331000961212     O          E            TESDGN           04
331100961217     O                       ST(4)               66
331200041109     O                       D46(3)             112
331300961217     O                       RSUTM               63
331400961217     O                       STADLA             106
331500961212     O          E            TESDGN           05
331600961217     O                       ST(3)               66
331700041109     O                       D46(4)             112
331800961217     O                       UDATE8              54 '  /  /    '
331900041109     O                       D82CMR             112
332000961212     O          E            TESDGN           06
332100961217     O                       ST(19)              66
332200041109     O                       D46(5)             112
332300041109     O                       DATCMR              87 '  /  /    '
332400961212     O          E            TESDGN         1 08
332500961212     O                       ST(6)               66
332600041109     O                       D46(6)             112
332700961212     O          E            DETDGN      1
332800100330     O              N21      §LB14               16
332900961212     O               21      §LB8                16
333000961212     O                       STARMN        Z   +  1
333100961212     O                       STARSM            +  1
333200961212     O                       STARSD            +  2
333300961212     O                       STATBO            +  1
333400961212     O                       ARBNCL        Z   +  2
333500961212     O                       PESPRT        4   +  2
333600041109     O                    N34PRTCAS        4   +  2
333700961212     O          E            DETDGN      1
333800961212     O                       STADSP              16 '  /  /    '
333900130808     O                       STAINM              52
334000130808     O*************          ARBCAM              36
334100130808     O*************          STALOM              53
334200130808     O                       STAINM              52
334300961212     O                       STAIND              79
334400961212     O                       ARBNAS             100
334500041109     O                    N34AR9VCA             112
334600961212     O          E            DETDGN      1
334700961218     O                     02                    13 '* PARTIAL *'
334800130808     O**************         ARBRMA              30
334900130808     O**************                             37 'P.IVA:'
335000130808     O**************         MITCPI              53
335100130808     O                       ARBCAM              36
335200130808     O                       STALOM              53
335300961212     O                       ARBCAD              63
335400961212     O                       STALOD              84
335500961212     O                                           91 'P.IVA:'
335600961212     O                       DESCPI             107
335700130808     O          E            DETDGN      1
335800130808     O                       ARBRMA              30
335900130808     O                                           37 'P.IVA:'
336000130808     O                       MITCPI              53
336100961220     O          E   N34      DETDGN      1
336200041109     O                       STATIC             112
336300961212     O          E   N36      DETDGN      1
336400961212     O         OR   N35
336500041109     O                    N35STANT1              75
336600041109     O                    N36STANT2             111
336700961212     O          E   N36      DETDGN      0  1
336800961212     O         OR   N35
336900041109     O                    N35STANT1              75
337000041109     O                    N36STANT2             111
337100961212     O* EXCPT FITTIZIA
337200961212     O          E          36DETDGN      1  0
337300041109     O                                          112 ' '
337400961212     O          E            TOTDGN      1
337500961212     O                       ST(15)              66
337600041109     O                       D46(7)             112
337700961212     O          E            TOTDGN      1
337800961212     O                       ST(17)              66
337900041109     O                       D46(8)             112
338000961212     O                       TOTSPE        Z     24
338100961212     O                       TOTNCL        Z     50
338200961212     O                       TOTPES        4     79
338300961212     O                       TOTVLF        4    104
338400961212     O          E            TOTDGN      1
338500961212     O                       ST(15)              66
338600041109     O                       D46(7)             112
338700961212     O          E            FINDGN      1
338800961212     O                                           69 '***  END  OF  PRINT  ***'
338900941222     O*
339000961212     O*****************************************************************
339100970114     O* LISTA DISCORDANZE VIA FAX
339200961212     O*****************************************************************
339300961213     OLISDIS    E            TESDIS           02
339400961217     O                       DIS(21)            132
339500961217     O                       STARAG              75
339600961217     O                       STPFAX              97
339700041109     O              N20                         112 '* PROVVISORIA'
339800041109     O               20                         112 '* DEFINITIVA '
339900961127     O          E            TESDIS           04
340000961125     O              N20      DIS(1)              66
340100961125     O              N20      DIS(2)             132
340200961125     O               20      DIS(17)             66
340300961125     O               20      DIS(18)            132
340400961125     O              N20      UDATE8              85 '  /  /    '
340500961129     O               20      UDATE8              67 '  /  /    '
340600961217     O                       UTIME              100 '  :  :  '
340700041109     O                       WPAG          Z    116
340800961127     O          E            TESDIS           06
340900961217     O                       DIS(3)              66
341000961217     O                       DIS(4)             132
341100961217     O                       D82NFV        Z     50
341200961217     O                       PARDFV              70 '  /  /    '
341300961217     O                       STADLA             115
341400961217     O          E            TESDIS           07
341500961217     O                       DIS(22)             66
341600961217     O                       DATCMR              55 '  /  /    '
341700961217     O                       D82CMR              91
341800961217     O          E            TESDIS           09
341900961122     O                       DIS(5)              66
342000961122     O                       DIS(8)             132
342100961217     O          E            TESDIS         1 10
342200961122     O                       DIS(6)              66
342300961122     O                       DIS(9)             132
342400961122     O          E            DETDIS         1
342500961127     O               16      KFLS                 3
342600961127     O               16      KLNA                 7
342700961129     O               16 17N18KNRS                10
342800961129     O               16 17N18                    11 '-'
342900961129     O               16   N18KNSC          Z     18
343000961129     O                  14   WNPS                22
343100961127     O               15      WPAAS               26
343200961127     O               15      WLNP                30
343300961127     O               15 17   KNRS                34
343400961127     O               15 17                       35 '-'
343500961127     O               15      WNSP          Z     42
343600961122     O                       KCEV                47
343700961122     O                                           48 '-'
343800970114     O                       DCEVE              108
343900961128     O                 N14   WFIL               112
344000961129     O                 N14   WNPS               115
344100961125     O          E            DETDI2         1
344200961202     O*                        COSPRT    68
344300961127     O                       WDTPA1              79 '  /  /    '
344400961129     O*
344500961129     O          E            RIGCNS         1
344600961129     O               12                           8 'PARCELS:'
344700961129     O               17      KNRS                10
344800961129     O               17                          11 '-'
344900961129     O                       NSC(1)              19
345000961129     O                       NSC(2)              27
345100961129     O                       NSC(3)              35
345200961129     O                       NSC(4)              43
345300961129     O                       NSC(5)              51
345400961129     O                       NSC(6)              59
345500961129     O                       NSC(7)              67
345600961129     O                       NSC(8)              75
345700961129     O                       NSC(9)              83
345800961129     O                       NSC(10)             91
345900961129     O                       NSC(11)             99
346000961129     O                       NSC(12)            107
346100961129     O                       NSC(13)            115
346200961127     O          E            FIT         1
346300961122     O*                                     ' '
346400961122     O          E            FINDIS      1
346500961127     O               20                          75 '***  END OF PRINT  ***'
346600961127     O              N20                          75 '***  FINE STAMPA  ***'
346700961217     O          E            DETDI3           19
346800961125     O              N20      DIS(15)             66
346900961125     O              N20      DIS(16)            132
347000961125     O               20      DIS(19)             66
347100970114     O*****************************************************************
347200970114     O* LISTA DISCORDANZE SU CARTA
347300970114     O*****************************************************************
347400970114     OLISDIC    E            TESDIC           02
347500970114     O                       DIS(21)            132
347600970114     O                       STARAG              75
347700970114     O                       STPFAX              97
347800041109     O              N20                         112 '* PROVVISORIA'
347900041109     O               20                         112 '* DEFINITIVA '
348000970114     O          E            TESDIC           04
348100970114     O                       DIS(1)              66
348200970114     O                       DIS(2)             132
348300970114     O                       UDATE8              85 '  /  /    '
348400970114     O                       UTIME              100 '  :  :  '
348500970114     O                       WPAG1         Z    116
348600970114     O          E            TESDIC           06
348700970114     O                       DIS(3)              66
348800970114     O                       DIS(4)             132
348900970114     O                       D82NFV        Z     50
349000970117     O               22                          54 'BIS'
349100970114     O                       PARDFV              70 '  /  /    '
349200970114     O                       STADLA             115
349300970114     O          E            TESDIC           07
349400970114     O                       DIS(22)             66
349500970114     O                       DATCMR              55 '  /  /    '
349600970114     O                       D82CMR              91
349700970114     O          E            TESDIC           09
349800970114     O                       DIS(5)              66
349900970114     O                       DIS(8)             132
350000970114     O          E            TESDIC         1 10
350100970114     O                       DIS(6)              66
350200970114     O                       DIS(9)             132
350300970114     O          E            DETDIC         1
350400970114     O               16      KFLS                 3
350500970114     O               16      KLNA                 7
350600970114     O               16 17N18KNRS                10
350700970114     O               16 17N18                    11 '-'
350800970114     O               16   N18KNSC          Z     18
350900970114     O                  14   WNPS                22
351000970114     O               15      WPAAS               26
351100970114     O               15      WLNP                30
351200970114     O               15 17   KNRS                34
351300970114     O               15 17                       35 '-'
351400970114     O               15      WNSP          Z     42
351500970114     O                       KCEV                47
351600970114     O                                           48 '-'
351700970114     O                       DCEV               108
351800970114     O                 N14   WFIL               112
351900970114     O                 N14   WNPS               115
352000970114     O          E            DETDC2         1
352100970114     O*                        COSPRT    68
352200970114     O                       WDTPA1              79 '  /  /    '
352300970114     O*
352400970114     O          E            RIGCNC         1
352500970114     O               12                           8 'PARCELS:'
352600970114     O               17      KNRS                10
352700970114     O               17                          11 '-'
352800970114     O                       NSC(1)              19
352900970114     O                       NSC(2)              27
353000970114     O                       NSC(3)              35
353100970114     O                       NSC(4)              43
353200970114     O                       NSC(5)              51
353300970114     O                       NSC(6)              59
353400970114     O                       NSC(7)              67
353500970114     O                       NSC(8)              75
353600970114     O                       NSC(9)              83
353700970114     O                       NSC(10)             91
353800970114     O                       NSC(11)             99
353900970114     O                       NSC(12)            107
354000970114     O                       NSC(13)            115
354100970114     O          E            FITC        1
354200970114     O*                                     ' '
354300970114     O          E            FINDIC      1
354400970114     O                                           75 '***  FINE STAMPA  ***'
354500970114     O          E            DETDC3           19
354600970114     O                       DIS(15)             66
354700970114     O                       DIS(16)            132
354800970110**
354900961217                           Date printed    :XX/XX/XXXX          Ti  1
355000961217me printed    :XX:XX:XX               Page:  xxxx                   2
355100961216                           Shipment date   :xx/xx/xxxx          CM  3
355200961216                           Exporting branch:1234567890123456789 Im  4
355300961216porting branch:1234567890123456789012345                            5
355400960223    Shipmentno    Rif.Exp  Exporter                   Consignee     6
355500960223    XXXXXXXXXXXX XXXXXXXXX xxxxxxxxxxxxxxxxxxxxxxxxx  XXXXXXXXXXXX  7
355600961011        XX/XX/XX           xxxxxxxxx xxxxxxxxxxxxxxxx xxxxxxxxxxxx  8
355700961011  * PARTIAL *  xxxxxxxxxxxxxxx P.IVA:xxxxxxxxxxxxxxxx xxxxxxxxx xx  9
355800961125              Imp Parcels   Weight    C.O.D.                       10
355900961011XXXXXXXXXXXXX xxx  xxxxx  xxxxxx,x  xxxxxxxxxx,xxx                  1
356000961011XXxxxxxxxxxxx      123456789012345             xxx                  2
356100961011XXxxxxxxxxxxxxxxxx P.IVA: xxxxxxxxxx                                3
356200940131
356300940111------------------------------------------------------------------  5
356400940111--------------------------------------------------                  6
356500961125  Total Shipments: xxxxx    Total Parcels: xxxxxxx    Total Weight  7
356600961125: xxxxxxxxx,x   Total Volume: xxxx,xxx                              8
356700961216                                                                CM 19
356800961216R Number      :12345678901234567890123456789012345                 20
356900961216R Date        :xx/xx/xxxx                                          21
357000931229**
357100961217                          ** LISTA DISCORDANZE SPEDIZIONI PER L'ES  1
357200961223TERO DEL XX/XX/XXXX **    XX:XX:XX       PAG. XXXX
357300961217                           FOGLIO DEFLUENZA :XXXXX      DEL XX/XX/  3
357400961217XXXX    LINEA DI ARRIVO:1234567890123456789012345                   4
357500961128  P  A  R  C  E  L  S    S H I P M E N T                            5
357600961122LHD LHA  NUMBER    O.R. YR LHD   NUMBER                             6
357700961121XXX XXX XX XXXXXXX  XX  XX XXX  XX XXXXXXX  XXX-123456789012345678  7
357800961128                                           SPUNTA                   8
357900961128                                           P.O O.R                  9
358000961128901234567890123456789012345678901234567890 XXX XX                  10
358100961127                                                123456789012345678 11
35820096112790 XX/XX/XXXX                                                      12
358300961129PARCELS:XX-1234567,1234567,1234567,1234567,1234567,1234567,1234567 13
358400961129,1234567,1234567,1234567,1234567,1234567,1234567                   14
358500970121                **  N E S S U N A   D I S C O R D A N Z A   R I S  15
358600970121C O N T R A T A  **                                                16
358700961217                                *** DISCREPANCY LIST DTD XX/XX/XXX 17
358800961223X ***                     XX:XX:XX       PAG. XXXX                 18
358900961212                             **  N O   D I S C R E P A N C Y  **   19
359000961217                           123456789012345678901234567890123456789 20
359100961219012345678 *Fax:1234567890123456                                    21
359200961217                           CMR..............:XX/XX/XXXX 1234567890 22
3593009612171234567890123456789012345                                          23
359400041109** D46
359500041109_________ *Fax:________________                                     1
359600041109me printed    :XX:XX:XX           Page:  xxxx                       2
359700041021porting branch:_________________________                            3
359800041109R Number  :___________________________________                      4
359900041109R Date    :xx/xx/xxxx                                               5
360000041021              Imp Parcels   Weight    C.O.D.                        6
360100041109----------------------------------------------                      7
360200041021: xxxxxxxxx,x   Total Volume: xxxx,xxx                              8
360300041021                                                                    9
360400041021                                                                   10
360500961213** CMD
360600961213OVRPRTF MANPTN QSYSPRT SAVE(*YES)                                    1
360700971230OVRPRTF MANPTN QSYSPRT OUTQ(FAXIN     ) SAVE(*YES)                   2
360800961213OVRPRTF MANCOD QSYSPRT SAVE(*YES)                                    3
360900971230OVRPRTF MANCOD QSYSPRT OUTQ(FAXIN     ) SAVE(*YES)                   4
361000961213OVRPRTF MANDGN QSYSPRT SAVE(*YES)                                    5
361100971230OVRPRTF MANDGN QSYSPRT OUTQ(FAXIN     ) SAVE(*YES)                   6
361200990512OVRPRTF LISDIC QSYSPRT SAVE(*YES)       HOLD(*YES)                   7
361300971230OVRPRTF LISDIS QSYSPRT OUTQ(FAXIN     ) SAVE(*YES)                   8
361400970110OVRPRTF MANPTS QSYSPRT SAVE(*YES)                                    9
361500971230OVRPRTF MANPTN QSYSPRT OUTQ(FAXOUT    ) SAVE(*YES) FORMTYPE('SAVEDI')10
361600971230                                                                     11
361700971230OVRPRTF LISDIS QSYSPRT SAVE(*YES)                                    12
361800970110**
361900970110XXXXXXXXXXXXXXXXXXXX/XXX                  ** MANIFEST SPEDIZIONI E   1
362000970110XPORT SINTETICO **               TRTC83R1    XX/XX/XXXX  PAG. XXXX   2
362100970121XXXXXXXXXX XXXXXXXXXX  Foglio Defl.XXXXX      Lna:123456789012345    3
362200970115 Partner:123456789012345678901234567890      XX:XX:XX                4
362300970110Anno Lnp Ser  Numero  Natura merce     Ragione sociale mittente      5
362400970110XXXX XXX  XX XXXXXXX  123456789012345  123456789012345678901234567   6
362500970110          Rif.mitt.alfab.  Rif.mitt.numer.  Colli    Peso            7
36260097011089012345  123456789012345  123456789012345  12345  123456,7          8
362700970110------------------------------------------------------------------   9
362800970110Totale Spedizioni: xxxxx    Totale Colli: xxxxxxx    Totale Peso:   10
362900970110xxxxxxxxx,x   Totale Volume: xxxx,xxx                               11
