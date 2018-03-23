000100130315     /*PRM dbgview(*source)
000200130315     /*END
000300141003     H DECEDIT('0,') DATEDIT(*DMY.) option(*nodebugio)
000400940927     H* FNLG20R *----------------------------------------------------*
000500040506     H*               ESECUZIONE DISPOSI. ARRIVO                     *
000600030131     H*             - CREAZIONE BOLLE DI RECUPERO                    *
000700940224     H*--------------------------------------------------------------*
000800940224     H*  01 ON --> PRIMO LIVELLO                                     *
000900940224     H*  02 POSIZIONAMENTO SU CODICE FILIALE DI ARRIVO               *
001000940309     H*  03 CAMBIO LINEA ARRIVO IN GESTIONE                          *
001100940309     H*  18                                                          *
001200940224     H*  19 IND. ERRORE GENERICO VIDEO                               *
001300940224     H*  20 NON TROVATA TARIFFA X CALCOLO PROVVIGIONI C/A            *
001400900418     H*  21 IND. COMODO                                              *
001500940224     H*  22 ERRMSG: CODICE FINE GIACENZA ERRATO                      *
001600950120     H*  23 POSIZIONAM. SU CODICE FINE GIACENZA SE RICONSEGNA        *
001700940224     H*  24 CLIENTI ESTERO                                           *
001800940224     H*  25 CLIENTI ITALIA                                           *
001900941217     H*  26 FATT.GIA' CONTABILIZZ. NON VARIAZ 1° BOLLA               *
002000941217     H*  27 FATT.GIA' CONTABILIZZ. NON VARIAZ 2° BOLLA               *
002100940224     H*  30 CODICE CAMBIO BOLLA NON TROVATO NUOVO CODICE             *
002200940224     H*  31 CREAZIONE BOLLA PARTENZA                                 *
002300940225     H*  32 RICHIESTA NOTA DI ACCREDITO                              *
002400940225     H*  33 VISUALIZZA SPESE DI GIACENZA                             *
002500941217     H*  34 VISUALIZZA RICH. X INDICARE TIPO INCASSO C/ASS SE ERRATO *
002600940225     H*  35 VISUALIZZA RECUPERO PROVVIGIONI C/A                      *
002700940225     H*  36 VISUALIZZA RECUPERO SP. DI TRASP.: NUOVO PORTO ASSEGNATO *
002800940225     H*  37 NOTA DI ACCREDITO  2° BOLLA                              *
002900941217     H*  38                                                          *
003000940225     H*  39 DO' PER INCASSATO L'ASSEGNATO                            *
003100940225     H*  40 CHAIN FLGCP000                                           *
003200940225     H*  42 ULTIMA RIAPERTURA                                        *
003300940225     H*  43 CHAIN FLARB                                              *
003400940225     H*  44 REVERSE IMMAGE SU CAMPO FILIALE DI ARRIVO                *
003500940225     H*  45 VISUALIZZA CODICE CLIENTE SE NUOVO PORTO ASSEGNATO       *
003600940225     H*  46 CONTROLLO INDIRIZZO DEL DESTINATARIO (SE ON)             *
003700940225     H*  48..56 MESSAGGI DI ERRORE                                   *
003800940308     H*  54 DATA FINE  > DATA DEL GIORNO                             *
003900941219     H*  57 NON ESISTE TARIFFA  GIACENZA                             *
004000940225     H*  58..60 MESSAGGI DI ERRORE                                   *
004100940225     H*  61 CI SONO SPESE                                            *
004200940309     H*  62..   MESSAGGI DI ERRORE                                   *
004300940225     H*  69 SPEDIZIONE NON STORNATA                                  *
004400941216     H*  70 DSPCTL/CLEAR SUBFILE                                     *
004500941216     H*  79 EMETTE VIDEO LA DATA DI ESEGUIBILTA DISPOSIZ             *
004600941216     H*  80 RICH. PGM VARIAZ..  DSARBK                               *
004700941216     H*  81 RICH. PGM VARIAZ..  DSARBD                               *
004800941216     H*  82 RICH. PGM VARIAZ..  DSARBT                               *
004900941216     H*  83 RICH. PGM VARIAZ..  DSARB2                               *
005000941216     H*  84 ST.NOTA ACCR. SEGNALA DIROTTAM                           *
005100941216     H*  85 ST.NOTA ACCR. SEGNALA RICONSEGNA                         *
005200941216     H*  86 INTESTA RICH.N.ACCRED. ALLA SEDE                         *
005300941216     H*  87 EMISS. VIDEO RICHIESTA SE RICH. TASSAZIONE               *
005400940308     H*  88 CREAZIONE BOLLA ANCHE IN ARRIVO                          *
005500940310     H*  90 ON = RICH.DA GIACENZA -CONDIZ. CAMPI VIDEO+STAMPA        *
005600000421     H*  93 ON = FORZATURA C/S PER RESO GABBIE POSTE                 *
005700950104     H*  94 ON = RICH.- NO GIAC. DISPOSIZ. OBBLIGO RESO              *
005800941216     H*  95 ON = PROTEGGE ALCUNI CAMPI SE RICH.DA GIAC               *
005900940310     H/EJECT
006000940309      *--------------------------------------------------------------*
006100940309      *  MAIN PROGRAM
006200940309      *--------------------------------------------------------------*
006300940309     H*       ----> 1)  -SRGIAC =SE RICH. DA GIAC.
006400940309     H*                O-SRDIRO =SE RICH. DA BOLLE
006500940309     H*                         ---> 1) -CHKARB = LEGGO FLARB
006600940309      *
006700940309     H*  SRF01----> 1)  -ARBVID =VIDEO X IMMISS. DISPOS. SE  DA BOLLE
006800940309     H*                O-GCAVID =VIDEO X VISUAL.DISPOS.  SE  DA GIACE.
006900940309     H*                  GCANOT           "
007000940309     H*             2)  -AZZER  =AZZERAM. INDIC. + CAMPI...
007100940309     H*             3)  -RIENTR =SE DISPOS. RIENTRO  OPERAZ. DA FARE
007200940310     H*                O-DIROT1 =SE DISPOS. DIROTTAMENTO      "
007300940310     H*                O-RICONS =SE DISPOS. RICONSEGNARE  1° operaz.
007400941010     H*                         ---> O -RICNOC = RIC.NO CAMBIO PORTO
007500941010     H*                              O -RICSIC = RIC.SI CAMBIO PORTO
007600940309      *
007700940309     H*  SRF02----> 1)  -CTRV02 =VIDEO X CONTR. DESTIN./LNA/F. GIAC...
007800940310     H*                 -DIROT2 =SE DISPOS. DIROTTAMENTO  2° operaz.
007900941217     H* ELIMINATA               ---> O -DIRSLA = DIR. STESSA L.ARRIVO
008000941010     H*                              O -DIRALA = DIR. ALTRA L.ARRIVO
008100940309     H*             2)  -SRF03  =VIDEO X CONTR. SPESE GIAC....
008200940309     H*                         ---> 1) -TGCVID = CALCOLO SPESE GIAC.
008300940309     H*                              2) -CTRV03 = CONTROLLO SPESE
008400940309     H*             3)  -PGARBK =RICH. PGM VARIAZ. BOLLA
008500940309     H*                O-PGARBI =         "
008600940309     H*                O-PGARBT =         "
008700940309     H*                O-PGAR2T =         "
008800940309     H*             4)  -SRAGG  =SE TUTTO OK AGGIORNO ARCHIVI
008900940309     H*                         ---> 1) -PGNBLP = CREAZ. BOLLA PART.
009000030807     H*                                 -AGGAR5 = DUPLICA FIAR5
009100940309     H*                              2) -AZZIMC = AZZERO C/ASS DA FIGLIA
009200940309     H*                              4) -       = STORIC. O NO  LA MAMMA
009300940309     H*                                 -AGGART = AGG. DETT.SEGNACOLLI
009400941005     H*                                 -EVENTO = CREO EVENTO X DIR/C.POR
009500940309     H*                              5) -RCHNAC = RICH. NOTA ACCREDITO
009600940309     H*                              6) -RCHREC = RICH. RECUPERO TRASP.
009700940309     H*                              7) -LEGBOL = CREO LEGAME BOLLA
009800950103     H*                                 -AGGAR4 = SRIVO LE NOTE
009900030826     H*                                 -AGGORM = agg.ORM x part."RC"
010000940309     H*                              8) -ARRBOL = SCRIVO BOLLA IN ARRIVO
010100940309     H*                                         = RICH. PGM TASSAZIONE
010200940309     H*                                         = RICH. PGM ST.BOLLA
010300940309     H*                              9) -GCPWTR = AGGIORNO GIACENZA
010400950421     H*                                 -RCHNCF = RICH.N.ACCR.B.PRECED.
010500940309     H*  FINE-----> SOTTOMETTO RICEZIONE VARIAZIONI LOCALI (SE RICH.DA BOLLE)
010600900418     H*--------------------------------------------------------------*
010700941006     FFNLG20D   CF   E             WORKSTN
010800940915     F                                     SFILE(LG20S05:NRR)
010900141210     FFNARB01L  UF   E           K DISK    infds(arbinf)
011000050316     FFNBLP01L  UF   E           K DISK
011100940928      * TASSAZ. 1° BOLLA
011200990930     FFIAR601L  IF   E           K DISK
011300940928      * TASSAZ. 1° BOLLA ESTENSIONE X VARIE
011400990930     FFIAR701L  IF   E           K DISK
011500940928      * CONTRASSEGNO
011600051115     FFIAR901L  IF   E           K DISK
011700060213     FFiar401L  UF A E           K DISK
011800050222     Ftigcp21l  IF   E           K DISK
011900050222     F                                     RENAME(tigcp000:GCPINT)
012000050316     Ftigcp02l  UF   E           K DISK    USROPN commit infds(gcpinf)
012100050222     Ftignp01l  IF   E           K DISK    USROPN
012200000214     FFNARBX0F  O  A E             DISK
012300050316     FFNART01L  UF   E           K DISK
012400990518     FFNANM02L  IF   E           K DISK
012500960618     F                                     INFDS(FLAN02)
012600990518     FFNANM01L  IF   E           K DISK
012700990518     F                                     RENAME(FNANM000:FNANM001)
012800960618     F                                     INFDS(FLAN01)
012900990803     FFNEVB00F  O    E           K DISK
013000990803     FFNEVB01L  IF   E           K DISK
013100990803     F                                     RENAME(FNEVB000:FNEVB1)
013200940928     FFNLBL01L  IF   E           K DISK
013300941006     F                                     RENAME(FNLBL000:LBL01)
013400940928     FFNLBL02L  IF   E           K DISK
013500941006     F                                     RENAME(FNLBL000:LBL02)
013600941005     FFNLBL00F  O  A E           K DISK
013700991028     FTNTBE01L  IF   E           K DISK
013800991028     FTABEL00F  IF   E           K DISK
013900940304     FAZORG01L  IF   F 5000     2PIDISK    KEYLOC(4)
014000990730     FTITGC01L  IF   E           K DISK    USROPN
014100010522     FFNSPE01L  IF   E           K DISK
014200041221     Ffnsp201L  IF   E           K DISK
014300051013     FFiar501L  uF a E           K DISK
014400050411     Ffigic00f  o    E             DISK
014500060314     Ffiar500f  o    E             DISK    rename(fiar5000:fiar50)
014600030128     Fwfmgb00F  O  A E           K DISK
014700040312     FFnncr00f  o    E             DISK
014800040527     FFiar801L  IF   E           K DISK
014900070614     FFiarn11L  uf a E           K DISK
015000920206     FLET132    O    F  132        PRINTER OFLIND(*INOF)
015100920206     F                                     USROPN
015200941221     FFNLV22P   O    F  132        PRINTER USROPN
015300110909     FFipdc02t  IF   E           K DISK
015400110909     FFipct01L  IF   E           K DISK
015500140916     FFisgn05l  uF   E           K DISK
015600050316     D*--------------------------------------------------------------*
015700110909     d fipctcokds    e ds
015800121107     d trulflbds     e ds
015900050316     d trul82ds      e ds
016000160826     d fnlg28ds      e ds
016100050701     d fnlg09ds      e ds
016200050316     d gcpinf          ds
016300050316     d  gcpnrr               397    400i 0
016400141210     d arbinf          ds
016500141210     d  arbnrr               397    400i 0
016600050316     D*--------------------------------------------------------------*
016700900424     D TGS             S              3  0 DIM(3)
016800990730     D TTS             S             11  3 DIM(3)
016900900424     D SGG             S              3  0 DIM(3)
017000990628     D STU             S             11  3 DIM(3)
017100990628     D STT             S             11  3 DIM(3)
017200911213     D CFG§            S              1    DIM(3)
017300941012     D VCA§            S              1    DIM(3)
017400920211     D FGB             S              7    DIM(40)
017500940928     D CDB             S              2    DIM(40)
017600941221     D D3A             S             89    DIM(40)
017700920130     D §TV             S              2    DIM(50)
017800920130     D §DF             S              1    DIM(50)
017900020509     D L£6             S              3  0 DIM(30)
018000030807     D fig             S                   DIM(30) like(ar5trd)
018100030807     D rec             S                   DIM(30) like(ar5trd)
018200030807     D ar5             S                   DIM(30) like(ar5trd)
018300941013     D CMB             S              1    DIM(80) CTDATA PERRCD(80)
018400991206     D CMP             S              1    DIM(80) CTDATA PERRCD(80)
018500030304     D CMPf            S              1    DIM(80) CTDATA PERRCD(80)
018600941013     D CME             S              1    DIM(80) CTDATA PERRCD(80)
018700941013     D CMN             S              1    DIM(80) CTDATA PERRCD(80)
018800030715     D CM4             S              1    DIM(80) CTDATA PERRCD(80)
018900030715     D CM5             S              1    DIM(80) CTDATA PERRCD(80)
019000941013     D DOV             S              1    DIM(80) CTDATA PERRCD(80)
019100141105     D ERR             S             78    DIM(69) CTDATA PERRCD(1)
019200940624     D*
019300940624     D CEI             S              1    DIM(50)
019400020621     D ESE             S              3    DIM(50)
019500150120     D CCC             S              1    DIM(100)
019600150120     D IPB             S              1    DIM(100)
019700950121     D DIV             S              3    DIM(30)
019800950121     D DCV             S             89    DIM(30)
019900951206     D C2A             S              3    DIM(200)
020000951206     D D2A             S             89    DIM(200)
020100990930     D* VARIE DI FIAR600F+FNAR700F
020200941216     D ASV             S              1    DIM(20)
020300990930     D AVA             S             11  3 DIM(20)
020400000131     D* VARIE DI FIAR600F+FNAR700F FIGLIE
020500000131     D FSV             S              1    DIM(20)
020600000131     D FVA             S             11  3 DIM(20)
020700990930     D* VARIE DELLA DTASV
020800941216     D* VARIE DELLA DSARBT
020900941216     D* WORK SIGLE DELLE VARIE X RICERCA
021000941216     D WSV             S              1    DIM(20)
021100960618     D DSLR33        E DS                  EXTNAME(FNLR33DS)
021200960618     D FLAN02          DS
021300960618     D  ANMNR2               397    400B 0
021400960618     D FLAN01          DS
021500960618     D  ANMNR1               397    400B 0
021600991028     D DGED          E DS
021700940624     D DSEI          E DS
021800000202     D OG143         E DS
021900070416     D OG146         E DS
022000051221     D OG148         E DS
022100140729     D OG150         E DS
022200140609     D Dvpo          E DS
022300030715     D trul90ds      E DS                  inz
022400940624     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
022500900418     D  TCU                  398    697
022600900418     D                                     DIM(50)
022700900418     D  KCU                  698    847P 0
022800900418     D                                     DIM(50)
022900900418     D                                     PACKEVEN
023000901219     D  DTU                  848    895P 0
023100901219     D                                     DIM(12)
023200901219     D                                     PACKEVEN
023300900418     D TCUDS           DS
023400900418     D  F3                     3      3
023500900418     D  F4                     4      4
023600900418     D  F56                    5      6
023700940928      * GESTIONE DATE
023800940928     D WLBDA8          DS
023900940928     D  G08DAT                 1      8  0
024000940928     D  G08INV                 9     16  0
024100940928     D  G08ERR                17     17
024200940928     D  G08TGI                18     22  0
024300051013     D DGNPDMC915    E DS                  prefix(d915) inz
024400051013     D DGNPDMC917    E DS                  prefix(d917) inz
024500051013     D DGcpflr       E DS
024600940217     D* PARAMETRI PASSATI AL PGM DI STAMPA BOLLA
024700030624     D FNLSB5DS      E DS
024800130315     d FNLSB6ds1     e ds                  inz
024900130315     d  DB6pdf       e                     inz('X')
025000970709     D* PARAMETRI PASSATI AL PGM TISI95R X CONTROLLO CAP
025100970709     D TISI95        E DS                  EXTNAME(TISI95DS)
025200970711     D TISI00        E DS                  EXTNAME(TISI00DS)
025300970710     D* PARAMETRI PASSATI routine cap
025400970710     D FNLV13        E DS                  EXTNAME(FNLV13DS)
025500020617      * ds reperimento se bolla trasmessa da cliente
025600020617     D DSBL4a        E DS
025700070320      * ds reperimento blocchi
025800070320     D DSBL4l        E DS
025900030826      * DS per richiamo pgm fnlr99r x gestione ORM in particolarità RC
026000030826     D fnlr99ds      E DS
026100071126     D fnlv55ds      E DS
026200080123     D fnlv59ds      E DS
026300160303     d  KS_CT                 74    118    dim(15)
026400160303     d  KS_pr                119    163    dim(15)
026500160303     d  KS_ie                164    178    dim(15)
026600041125     d
026700041125     drecordlbl      E DS                  extname(fnlbl00f)
026800941006     D* PARAMETRI PASSATI AL PGM DI INTERROGAZ. BOLLE FNLR36R
026900070622     D PARAM1          DS                  INZ
027000070622     D  PA1AAS                14     17  0
027100070622     D  PA1LNP                18     20  0
027200070622     D  PA1NRS                21     22  0
027300070622     D  PA1NSP                23     29  0
027400070622     D  PA1F03                30     30
027500070622     D  PA1FLG                31     31
027600070622     D* SE =1 NON RICH. FNLG30R IN MODO RICORSIVO DA INTERR.BOLLE
027700070622     D  PA1GIA               144    144
027800911115     D NWFGB           DS
027900911115     D  NWTB1                  1      2
028000911115     D  NWTB2                  3      4
028100911115     D  NWFCA                  5      5
028200911115     D  NWFAN                  6      6
028300911115     D  NWFTD                  7      7
028400911122     D                 DS
028500911028     D  TESTO1                 1      5
028600911121     D  GCAAGC                 6      7  0
028700911121     D  GCAFGC                 9     11  0
028800911025     D  GCANGC                13     19  0
028900911028     D  TESTO2                21     25
029000911114     D  VARIO                 27     33
029100911122     D  RIFGIA                 1     33
029200920116     D                 DS
029300920116     D  TESTO3                 1      8
029400941217     D  RAAS                   9     10  0
029500941217     D  RLNP                  12     14  0
029600941217     D  RNRS                  16     17  0
029700941217     D  RNSP                  19     25  0
029800920116     D  RIFSPE                 1     25
029900911212     D                 DS
030000911212     D  TESNWB                 1      7
030100911212     D  NEWAAS                 9     10  0
030200911212     D  NEWLNP                12     14  0
030300911212     D  NEWNRS                16     17  0
030400911212     D  NEWNSP                19     25  0
030500920522     D  TE2NWB                27     30
030600920522     D  NE2LNA                31     33  0
030700920522     D  NEWBOL                 1     33
030800990930     D YEURDS        E DS                  EXTNAME(YEURCODS)
030900990930     D CNCR80        E DS
031000020509     D TR06DS        E DS                  EXTNAME(TRUL06DS)
031100020509     D  LIN                    1     90  0 dim(30)
031200991001     D DSCC          E DS
031300060410     D DS2c          E DS
031400060808     D DS2z          E DS
031500911113     D DS2D          E DS
031600911113     D DS2E          E DS
031700040719     D DS2G2         E DS
031800911113     D DS1A          E DS
031900951206     D DS2A          E DS
032000910314     D DS3A          E DS
032100000214     D DS7R          E DS
032200110802     D DS7k          E DS
032300941216     D DS$2          E DS
032400920130     D DS7L          E DS
032500940929     D DSCV          E DS
032600941006     D DS15          E DS
032700990930     D DBLP          E DS
032800990930     D DTAS          E DS
032900060927     D DTAS01        E DS
033000990930     D DTASV         E DS
033100941216     D  NSV                    1     20
033200941216     D                                     DIM(20)
033300990930     D  NVA                   21    140P 3
033400990930     D                                     DIM(20)
033500990930     D §DBLP           DS
033600940303     D  VUOTO1                 1    256
033700941214     D  VUOTO2               257    512
033800990930     D  VUOTO3               513    624
033900941215     D DSLR48        E DS                  EXTNAME(FNLR48DS)
034000961202     D FNLG21        E DS                  EXTNAME(FNLG21DS)
034100160303     D TR27DS        E DS                  EXTNAME(TRUL27DS1)
034200160303     D trulc7ds      E DS
034300050511     D fnlra2ds      E DS
034400941214     D DSARBK        E DS
034500920212     D DSARBD        E DS
034600030207     D Dar5ban       E DS
034700030805     D Dar5bnb       E DS
034800030805     D dar5          E DS                  inz
034900031119     D Dar5gen       E DS
035000990930     D DARBT         E DS
035100990930     D  VSV                   44     63
035200941216     D                                     DIM(20)
035300990930     D  VVA                   64    183P 3
035400990930     D                                     DIM(20)
035500930511     D DSARBG        E DS
035600900418     D KPJBA         E DS
035700141003     d fnlg20ds      e ds
035800141103     d fnlry09ds2    e ds
035900940927     D FNLG82        E DS                  EXTNAME(FNLG82DS)
036000941005     D DSUL41        E DS                  EXTNAME(TRUL41DS)
036100941216     D  U41AAS       E                     EXTFLD(D41AAS)
036200941216     D  U41LNP       E                     EXTFLD(D41LNP)
036300941216     D  U41NRS       E                     EXTFLD(D41NRS)
036400941216     D  U41NSP       E                     EXTFLD(D41NSP)
036500941227     D  U41TPB       E                     EXTFLD(D41TPB)
036600960124      *  DS per scomposizione record con dati Provincia/Cap/Nazione
036700960124     D                 DS
036800960205     D  WCAD                   1      9
036900960205     D  WPRD                  11     12
037000960124     D  WNZD                  14     16
037100960124     D  WNOT                   1     35
037200020813     D* DS PER TRUL33R - REPERIMENTO/AGGIORNAMENTO NUMERATORI SU AZNUM
037300020813     D TRUL33DS      E DS
037400021114     d TIBS36DS      E DS
037500020618     d TIBS69DS      E DS                  INZ
037600020618     d DS_cnaco      E DS                  extname(CNACO00F)
037700020618     d DS_cnind      E DS                  extname(CNIND00F)
037800020618     d DS_cnclp      E DS                  extname(CNCLP00F)
037900020618     d DS_fncls      E DS                  extname(FNCLS00F)
038000060103     D tisit0ds      E DS
038100071008     D tisit0x1ds    E DS
038200060103     d ddstflo       e ds
038300060629     d fnlg24ds      e ds
038400151020     d tisie10ds     e ds
038500151020     d fieuLPDds     e ds
038600151020     d Dlpd          e ds
038700040716     d fidn12ds      e ds
038701171003      *ds per richiamo monitor variazioni
038702171003     d tigcp         e ds                  extname(tigcp00f)
038703171003     d fnlg33ds      e ds                  inz
038704171003     d tigcpa        e ds                  extname(tigcp00f)
038705171003     d                                     prefix(a)
038706171003     d tigcpb        e ds                  extname(tigcp00f)
038707171003     d                                     prefix(b)
038800140217     D fnlrn6ds      E DS
038900020621     D CdIVA           S              4
039000020927      *parametri per reperimento coda
039100051013     d dataiso         s               d
039200051013     d finep           s              1
039300020927     d codpo           s              3s 0
039400020927     d tpcoda          s              1
039500020927     d nomcoda         s             10
039600020927     d libcoda         s             10
039700020927     d esito           s              1
039800030807     d compar          s                   like(arbgva)
039900040315     d WORIGINE        s                   like(nchorigine)
040000040316     d Wpocompl        s                   like(nctpocompl)
040100170228
040200170228     d wlnpb           s                   like(§lplnp)
040300170323     d*idx             s              3  0
040400940928     D*
040500061026      * campi salvataggio chiave bolla originale da dirottamento
040600061026     d LBLAAOsv        s                   like(LBLAAO)
040700061026     d LBLLPOsv        s                   like(LBLLPO)
040800061026     d LBLNROsv        s                   like(LBLNRO)
040900061026     d LBLNSOsv        s                   like(LBLNSO)
041000061026      *
041100041221      * eccezioni luogo 300 partenza arrivo
041200041221     D dpar            DS
041300041221     D  par                    1     60  0 dim(20)
041400041221     D darr            DS
041500041221     D  arr                    1     60  0 dim(20)
041600060807     D dzon            DS
041700060807     D  zon                    1     40  0 dim(20)
041800070322     D dgir            DS
041900070322     D  gir                    1     60    dim(6)
042000070613      *
042100070613     d skvdmc          s             50    dim(4)
042200041221      *
042300021009     d kpjbus          s                   like(kpjbu)
042400071130      *
042500071130     d Wdata8          DS
042600071130     d  dadata                 1      8  0
042700071130     d  adata                  9     16  0
042800071130     d  GioLav                17     21  0
042900071130      *
043000940928     D* DEFINIZIONE COSTANTI
043100940928     D*
043200940928     D CNOTA           C                   CONST('** NESSUNA NOTA **')
043300950330     D*-------------------------------------------------------------*
043400950330     D* Costanti per definzione vari importi della bolla di recupero
043500950330     D*-------------------------------------------------------------*
043600950330     D CVAR            C                   CONST('Importo Varia    ')
043700950330     D CTRA            C                   CONST('Trasporto        ')
043800950330     D CDIR            C                   CONST('Diritto fisso    ')
043900950330     D CINL            C                   CONST('Inoltro          ')
044000951214     D CNOT1           C                   CONST('C.PORTO DA ASS. A FR')
044100951214     D CNOT2           C                   CONST('ANCO CL.       ')
044200961129     D CNOT3           C                   CONST('EX.SPED. IN FRANCO F')
044300961129     D CNOT4           C                   CONST('RONTIERA       ')
044400000925     D IMPORT          C                   CONST('I              ')
044500020621
044600940304     IAZORG01L  AA
044700971113     I                                  1    3  ORGDIT
044800971113     I                                  6    6  ORGFVA
044900971113     I                                 11   11  ORGFL1
045000940304     I                                 14   33  ORGDES
045100960327     I                                 34   53  ORGIND
045200960327     I                             P   54   56 0ORGCPF
045300960327     I                                 57   76  ORGLOC
045400960327     I                                 77   78  ORGPRO
045500000202     I                               3776 3800  ORGDE3
045600070416     I                               3851 3875  ORGDE6
045700051221     I                               3901 3925  ORGDE8
045800140729     I                               3951 3975  ORGDF0
045900990730     ITITGC000
046000900424     I              TGCSG1                      TGS(1)
046100900424     I              TGCSG2                      TGS(2)
046200900424     I              TGCSG3                      TGS(3)
046300900424     I              TGCST1                      TTS(1)
046400900424     I              TGCST2                      TTS(2)
046500900424     I              TGCST3                      TTS(3)
046600950330     I*-------------------------------------------------------------*
046700140729     C*
046800970529     C     *ENTRY        PLIST
046900970529     C                   PARM                    KPJBA
047000141006     C                   PARM                    fnlg20ds
047100970529     C* PASS. PARAMETRI DA PGM CHIAMANTE
047200970529     C                   MOVEL     KPJBU         FNLG82
047300970529     C                   MOVEL     KPJBU         §KPJBU
047400050316     c                   move      d82comm       gescomm           1
047500141007     c                   if        %parms > 1
047600141007     C                   movel     'N'           d82nodsp
047700141007     c                   endif
047800030715     c* mi compongo la ds trul90ds per pgm FNLS01R, FNLR41/8R
047900030715     c                   exsr      srtrul90
048000970529     C* PRIMA VOLTA
048100970529     C     WPRIMA        IFEQ      ' '
048200970529     C                   EXSR      DEFCAM
048300970529     C                   MOVEL     'S'           WPRIMA            1
048400970529     C                   END
048500970529     C*
048600940304     C                   MOVEL     '0'           $FINE
048700940304     C     $FINE         DOWEQ     '0'
048800940301      *
048900940301     C     $GIAC         IFEQ      '1'
049000940301     C                   EXSR      SRGIAC
049100920113     C                   ELSE
049200940301     C                   EXSR      SRDIRO
049300940301     C                   END
049400940304      *
049500940304     C     $FINE         IFEQ      '0'
049600940304     C                   MOVEL     '0'           $OKF01
049700940304     C                   MOVEL     '0'           $OKF02
049800940304     C                   MOVEL     '0'           $OKF03
049900940310     C                   MOVEL     '0'           $FOANC
050000940310     C                   MOVEL     '0'           $FOLNA
050100940310     C                   MOVEL     '0'           $FOFRI
050200940301      *
050300940304      * =======================================================
050400940304     C*?$F01 = VIDEO X DISPOSIZIONI
050500940304      * =======================================================
050600940307     C                   MOVEL     '1'           $F01
050700940307     C     $F01          DOWEQ     '1'
050800141003     c     d82nodsp      ifeq      *blank
050900940304     C                   EXSR      SRF01
051000141006     c                   else
051100141006      * se richiamato senza video salto prima routine e passo a riempire campi
051200141006      * video per completare il dirottamento
051300141006     C                   EXSR      azzer
051400141006     c                   move      '1'           $okf01
051500141003     c                   endif
051600961213      *---------------------------------------------------------*
051700961213      *  Se sto eseguendo delle disposizioni per bolle export
051800961213      *  controllo se devo emettere mascherina x fax
051900971113     C                   MOVEL     *BLANKS       ORGFL1
052000971113     C                   MOVEL     *BLANKS       ORGDIT
052100971113     C     ARBLNA        CHAIN     AZORG01L                           21
052200971113     C                   MOVEL     ORGDIT        SIFDIT            3
052300000314     C                   MOVEL     ORGDE3        OG143
052400020729     C                   MOVEL     *blank        flgest            1
052500020729     C     §OgNTW        ifeq      'FED'
052600020729     C     §OgNTW        oreq      'EEX'
052700020729     C     §OgNTW        oreq      'EUP'
052800020729     C                   MOVEL     'E'           flgest
052900020729     c                   end
053000020225      *PER FEDEX NON MANDA MAI IL FAX
053100020225     C     §OGNTW        IFNE      'FED'
053200020729     C     flgest        IFEQ      'E'
053300000314     C     VFRI          ANDNE     'D'
053400020702     C     §OGntw        OREQ      'DPD'
053500961213     C     VFRI          ANDNE     'D'
053600961213     C     $GIAC         IFEQ      '1'
053700961213     C     ARBFT3        ORNE      *BLANKS
053800961213     C     VFRI          OREQ      'R'
053900961213     C                   EXSR      CALL21
054000961213     C                   END
054100961213     C                   END
054200020225     C                   END
054300940304      * =======================================================
054400940304     C*?$F02 = VIDEO X DESTINATARIO /LNA/ COD.FINE GIACENZA...
054500940304      * =======================================================
054600940304     C     $OKF01        IFEQ      '1'
054700940304     C                   EXSR      SRF02
054800940304     C                   ENDIF
054900940307     C                   ENDDO
055000900418     C*
055100940304     C                   ENDIF
055200950113     C* SE PASSATO NR.SPEDIZ.:PGM RICHIAMATO ..VADO A FINE
055300950113     C     D82NSP        IFNE      *BLANK
055400950113     C                   MOVEL     '1'           $FINE
055500950113     C                   ENDIF
055600940304     C                   ENDDO
055700940303      *-------------------------------------------------------
055800940308     C     FINE          TAG
055900940303      *-------------------------------------------------------
056000950119     C* SE NON E' RICHIAM. DA GIACENZE O DA FNLG82R
056100950119     C* ---------->    CHIAMO PGM X SOTTOMETTERE
056200930511     C* RICEZIONE VARIAZ. LOCALI
056300940303     C     $GIAC         IFEQ      '0'
056400950119     C     D82NSP        ANDEQ     *BLANK
056500930511     C                   END
056600940303      *
056700050905     C*    D82F12        IFNE      '1'
056800050905     C*                  MOVEL     §KPJBU        KPJBU
056900050905     C*                  END
057000020227      *RICHIAMO PROGRAMMA PER CHIUSURA FILE
057100020227     C                   CLEAR                   TR27DS
057200020227     C                   MOVE      'C'           I27TLA
057300160303     C                   CALL      'TRUL27R1'
057400020227     C                   PARM                    TR27DS
057500160303      *
057600160303     C                   CLEAR                   TRulc7ds
057700160303     C                   MOVE      'C'           Ic7TLA
057800160303     C                   CALL      'TRULC7R'
057900160303     C                   PARM                    trulc7ds
058000080124      *
058100080124     C                   CLEAR                   fnlv59ds
058200080124     C                   MOVE      'C'           Ilv59tla
058300080124     C                   CALL      'FNLV59R'
058400080124     C                   PARM                    fnlv59ds
058500080124     c
058600050701     C                   CLEAR                   fnlg09ds
058700050701     C                   MOVEL     'C'           d09TLA
058800050701     c                   movel     fnlg09ds      kpjbu
058900050701     C                   CALL      'FNLG09R'
059000050701     C                   PARM                    kpjba
059100041125     c
059200041125     C                   CALL      'FNLG25R'
059300041125     C                   PARM                    recordlbl
059400041125     C                   PARM      'C'           i25tla
059500050905     c*
059600050905     C                   MOVEL     fnlg82        KPJBU
059700900418     C                   SETON                                        LR
059800940301     C/SPACE 3
059900940309     C*?$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
060000940308     C*?    R O U T I N E    CONTROLLO
060100940309     C*?$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
060200920113     C/SPACE 3
060300940304     C*---------------------------------------------------------------*
060400940304     C*   S R F 0 1    - FORMATO DISPOSIZIONI                         *
060500940304     C*---------------------------------------------------------------*
060600940304     C     SRF01         BEGSR
060700940304     C                   MOVEL     '1'           $F01
060800940304     C                   MOVEL     '0'           $OKF01
060900940304     C                   MOVEL     *BLANK        VNOT1
061000940304     C                   MOVEL     *BLANK        VNOT2
061100940304     C     $F01          DOWEQ     '1'
061200940304     C                   SETOFF                                       95
061300940304      *
061400940304     C     $GIAC         IFEQ      '0'
061500940304      * CONTROLLO  DISPOSIZ. IMMESSE A VIDEO DA BOLLE
061600940309     C                   EXSR      ARBVID
061700940304     C   KF              MOVEL     '0'           $F01
061800940304     C   KF              MOVEL     '1'           $OKF01
061900940304     C   KL              MOVEL     '0'           $F01
062000940304     C   KL              MOVEL     '0'           $OKF01
062100970204     C   KLD82NSP        IFNE      *BLANKS
062200970204     C                   MOVEL     '1'           D82F12
062300970204     C                   MOVEL     FNLG82        KPJBU
062400970204     C                   END
062500940304     C                   ELSE
062600961203      * CARICO LE DISPOSIZIONI  DELLA GIAC. A VIDEO
062700940304     C                   EXSR      GCAVID
062800961203     C* SCRIVO PRIMA PARTE  VIDEO
062900961203     C                   WRITE     LG20D08
063000961203     C* GESTIONE NOTE GIACENZE -  SFL
063100961203     C                   EXSR      GCANOT
063200941012     C                   MOVEL     '1'           $OKF01
063300941012     C                   MOVEL     '0'           $F01
063400961203     C* NON SI POSSONO ESEGUIRE DISP. PRIMA
063500961203     C     GCPDED        IFGT      AMGUDT
063600941012     C                   EXFMT     LG20Z09
063700941012     C                   MOVEL     '0'           $OKF01
063800941012     C                   MOVEL     '0'           $F01
063900941012     C                   MOVEL     '1'           $FINE
064000940928     C                   ENDIF
064100940304     C                   END
064200940304      *
064300940304     C*   ESECUZIONE DISPOSIZIONI
064400940304      *   -----------------------
064500940304     C     $OKF01        IFEQ      '1'
064600940304     C                   EXSR      AZZER
064700940304     C*
064800940304     C* TEST SULLE DISPOSIZIONI PER EMETTERE LE VIDEATE NECESSARIE
064900940304     C                   SELECT
065000940304     C     VFRI          WHENEQ    'R'
065100940304     C                   EXSR      RIENTR
065200940304     C     VFRI          WHENEQ    'C'
065300940304     C                   EXSR      RICONS
065400940304     C     VFRI          WHENEQ    'D'
065500940310     C                   EXSR      DIROT1
065600940304     C                   ENDSL
065700940304     C                   ENDIF
065800940304     C                   ENDDO
065900940304     C     ENDF01        ENDSR
066000940308     C*---------------------------------------------------------------*
066100940308     C*   S R F 0 2    - FORMATO X FINE GIAC./DESTINAT/ LNA ....      *
066200940308     C*---------------------------------------------------------------*
066300940308     C     SRF02         BEGSR
066400141003     C                   IF        D82nodsp <> *blank
066500141013      *forza indicatore per sproteggere campi destinatario
066600141013     c                   seton                                        46
066700141013      *
066800151127     c                   if        vfri = 'R'
066900151127     c                   exsr      rientr
067000151127     c                   else
067100141003     C                   MOVEL     d20RSD        VRSD
067200141003     C                   MOVEL     d20Rs2        VRD2
067300141003     C                   MOVEL     d20IND        VIND
067400141003     C                   MOVEL     d20LOD        VLOD
067500141003     C                   MOVEL     d20PRD        VPRD
067600141003     C                   MOVE      d20CAD        VCAD
067700141003     C                   MOVE      d20LNA        VLNA
067800141003     C                   MOVEL     d20NZD        VNZD
067900141003     C                   MOVEL     d20tel        Vteld
068000141003     C                   MOVEL     d20ref        Vref
068100141003     c                   move      d20snp        vapc
068200141003     c                   move      d20mdp        vppc
068300151127     c                   endif
068400141003     C                   MOVEL     '0'           $FOFRI            1
068500141003     c                   else
068600940309     C* SE HO FORZATO DISPOSIZ. REIMPOSTO I DATI DIGITATI
068700940309     C     $FOFRI        IFEQ      '1'
068800940309     C                   MOVEL     SAVRSD        VRSD
068900941011     C                   MOVEL     SAVRD2        VRD2
069000940309     C                   MOVEL     SAVIND        VIND
069100940309     C                   MOVEL     SAVLOD        VLOD
069200940309     C                   MOVEL     SAVPRD        VPRD
069300940309     C                   MOVE      SAVCAD        VCAD
069400940309     C                   MOVE      SAVLNA        VLNA
069500961203     C                   MOVEL     SAVNZD        VNZD
069600031121     C                   MOVEL     SAVteld       Vteld
069700031121     C                   MOVEL     SAVref        Vref
069800940309     C                   MOVEL     '0'           $FOFRI            1
069900940309     C                   END
070000141003     c                   endif
070100940309     C* SE HO FORZATO LINEA D'ARRIVO
070200940309     C                   MOVEL     '0'           $FOLNA            1
070300940309     C                   MOVEL     '0'           $RIEMI            1
070400940309     C                   SETOFF                                         1922
070500940308     C                   SETOFF                                       566265
070600941219     C                   SETOFF                                       344959
070700940308     C                   MOVEL     *BLANK        $MSG
070800940310     C     INIF02        TAG
070900941221     C                   SETOFF                                       9934
071000940308     C* EMETTO VIDEATA CON DATI FINE GIACENZA
071100940308     C                   MOVEL     '1'           $F11
071200961203      *---------------------------------------------------------*
071300961213     C     $F11          DOWEQ     '1'
071400940308     C                   SETON                                        95
071500940915     C                   WRITE     LG20Z02
071600940308     C     $GIAC         IFEQ      '0'
071700940915     C                   WRITE     LG20D04
071800940308     C                   ELSE
071900940915     C                   WRITE     LG20D08
072000940308     C                   EXSR      GCANOT
072100961203     C                   END
072200070619      *se disposizioni in consegna attivo comando per note ldv
072300091126      * e protezione campo nazione
072400091126     c                   setoff                                       1715
072500070619     c                   if        vfri = 'C'
072600091126     c                   seton                                        1715
072700070619     c                   endif
072800070619
072900940915     C                   WRITE     LG20Z03
073000940308     C                   MOVEL     '1'           $F02
073100940308     C     $F02          DOWEQ     '1'
073200940308     C                   MOVEL     '0'           $OKF02
073300940308     C                   MOVEL     '0'           $OKF03
073400940915     C                   EXFMT     LG20D06
073500070622      * richiamo pgm interrogazione bolle
073600070622     C   Kg              exsr      richbl
073700070622
073800950103     C  NKL              EXSR      CTRV02
073900070613      *richiamo pgm note ldv
074000070614     c                   if        *inkr and §oggeot = 'S'
074100070613     c                   clear                   skvdmc
074200070613      *carico eventuali note da passare al pgm
074300070614      *solo se è una giacenza
074400070614     c                   if        $giac = '1'
074500070613     c                   do        4             sfl               1 0
074600070614     c     sfl           chain     lg20s05                            60
074700070614     c                   if        not *in60
074800070613     c                   movel     vdmc          skvdmc(sfl)
074900070613     c                   endif
075000070613     c                   enddo
075100070614     c                   endif
075200070613      *
075300140217     c                   clear                   fnlrn6ds
075400070613     C                   movel     kpjbu         WSVKPJ          256
075500140217     C                   move      'I'           in6mod
075600140217     C                   z-add     arbaas        in6AAS
075700140217     C                   z-add     arblnp        in6LNP
075800140217     C                   z-add     arbnrs        in6NRS
075900140217     C                   z-add     arbnsp        in6NSP
076000140217     C                   movel     skvdmc(1)     in6not1
076100140217     C                   movel     skvdmc(2)     in6not2
076200140217     C                   movel     skvdmc(3)     in6not3
076300140217     C                   movel     skvdmc(4)     in6not4
076400140217     C                   movel     'FNLG20R'     in6pgm
076500140217     C                   call      'FNLRN6R'                            98
076600070613     C                   parm                    kpjba
076700140217     C                   parm                    fnlrn6ds
076800070613     C                   movel     WSVKPJ        kpjbu
076900070613     c                   endif
077000070614     C     *INKL         IFEQ      '1'
077100940308     C                   SELECT
077200141007     C                   WHEN      d82nodsp <> *blank
077300141007     c                   move      'R'           d20erro
077400141007     C                   return
077500940308     C     $GIAC         WHENEQ    '0'
077600940308     C                   MOVEL     '0'           $OKF02
077700940308     C                   MOVEL     '0'           $F02
077800940308     C                   MOVEL     '0'           $F11
077900940308     C                   MOVEL     '1'           $F01
078000940308     C     $GIAC         WHENEQ    '1'
078100940308     C                   MOVEL     '0'           $OKF02
078200940308     C                   MOVEL     '0'           $F02
078300940308     C                   MOVEL     '0'           $F11
078400940308     C                   MOVEL     '0'           $F01
078500940308     C                   MOVEL     '1'           $FINE
078600940308     C                   END
078700940308     C                   ELSE
078800940308      *ERRORI
078900940308     C     *IN19         IFEQ      '1'
079000940308     C     *IN30         OREQ      '1'
079100940308     C                   MOVEL     '1'           $F02
079200940308     C                   MOVEL     '0'           $OKF02
079300940308     C                   ELSE
079400940308     C   KF              MOVEL     '1'           $OKF02
079500940308     C   KF              MOVEL     '0'           $F02
079600940308     C   KF              MOVEL     '0'           $F01
079700940308     C   KF$GIAC         IFEQ      '1'
079800940308     C                   MOVEL     '1'           $FINE
079900940308     C                   END
080000940308     C                   END
080100941215      * SE  HO FORZATO DIROTTAMENTO MA DISPOSIZ. ERANO RICONSEGNA
080200941215      * SE  HO FORZATO RICONSEGNA DISPOSIZ. ERANO DIROTTAMENTO
080300940309     C     $FOFRI        IFEQ      '1'
080400941215     C     VFRI          IFEQ      'C'
080500940308     C                   MOVEL     'D'           VFRI
080600940309     C     $GIAC         IFEQ      '1'
080700940309     C                   MOVEL     'M'           VPSG
080800940309     C                   MOVEL     'S'           VASG
080900940309     C                   END
081000941215     C                   ELSE
081100941215     C                   MOVEL     'C'           VFRI
081200941215     C                   ENDIF
081300940308     C                   MOVEL     '0'           $OKF02
081400940308     C                   MOVEL     '0'           $F02
081500940308     C                   MOVEL     '0'           $F11
081600940308     C                   MOVEL     '1'           $F01
081700940308     C                   END
081800940309     C     $RIEMI        IFEQ      '1'
081900940309     C                   MOVEL     '0'           $OKF02
082000940309     C                   MOVEL     '0'           $F02
082100940309     C                   MOVEL     '1'           $F11
082200940310     C                   MOVEL     '0'           $RIEMI
082300940309     C                   END
082400940308     C                   END
082500940308     C     $OKF02        IFEQ      '1'
082600940308     C     *IN33         IFEQ      '1'
082700940308     C     *IN35         OREQ      '1'
082800940308     C     *IN36         OREQ      '1'
082900940308      * $F03 = VIDEO X RECUPERO SPESE GIAC./PROVV. C.ASSEGNO....
083000940308     C                   MOVEL     '1'           $F03
083100940308     C                   EXSR      SRF03
083200940308     C                   ELSE
083300940308     C                   MOVEL     '1'           $OKF03
083400940308     C                   END
083500940308     C     $OKF03        IFEQ      '1'
083600940308     C                   MOVEL     '0'           $F02
083700940308     C                   MOVEL     '0'           $F11
083800941221     C                   CLEAR                   DSLR48
083900020617      *emissione video controllo responsabilità cliente o vettore
084000020617     c                   move      *blank        viddcli
084100030617     C     $GIAC         ifne      '1'
084200020617     C     VFRI          ifeQ      'D'
084300020617     C     WVARIO        andne     0
084400020617     C     WVARIO        ANDNE     8888
084500020617     C     WVARIO        ANDNE     9999
084600030620     c                   eval      viddtit = 'DIROTTAMENTO'
084700020617     C                   MOVEL     'A'           §TRC
084800060213     C     KAR4          CHAIN(N)  FiAR401L                           43
084900020617     C     *IN43         IFEQ      '0'
085000020617     C                   MOVEL     AR4NOT        DSBL4A
085100030917      * se è un supermercato la responsabilità è del vettore
085200030918     c                   if        arbtc1 = 'S' and §b4aso = '%' or
085300030918     c                             arbtc2 = 'S' and §b4aso = '%'
085400030917     c                   move      'V'           viddcli
085500030917     c                   else
085600030917      * disk C o disk A con errato instradamento responsabilità cliente
085700030918     c                   if        §b4aso = '§' or §b4aso = ':' or §b4aso = '%'
085800020617     c                   move      'C'           viddcli
085900020617     c                   else
086000020617     c                   move      'V'           viddcli
086100020617     c                   end
086200030917     c                   end
086300021113     c                   if        viddcli = 'C'
086400021113     C     VRSD          IFeq      WSVRSD
086500021113     C     VRD2          andeq     WSVRD2
086600021113     C     VIND          andeq     WSVIND
086700021113     C     VCAD          andeq     WSVCAD
086800021113     C     VPRD          andeq     WSVPRD
086900021113     C     VLOD          andeq     WSVLOD
087000021113     C     VNZD          andeq     WSVNZD
087100021113     c                   move      'V'           viddcli
087200021113     c                   end
087300021113     c                   end
087400030617     C                   END
087500020617     C                   END
087600030617     c* imposto come default l'addebito delle spese x il cambio di
087700030617     c* porto al cliente
087800030617     C                   if        (vfri = 'C' and vnpc = 'F')
087900030617     c                   move      'C'           viddcli
088000030620     c                   eval      viddtit = 'CAMBIO DI PORTO'
088100030617     c                   end
088200030617     c* emetto il video per chiedere a chi fare l'addebito delle spese
088300030620     C                   if        (vfri = 'C' and vnpc = 'F') OR
088400030620     C                             (vfri = 'D' and WVARIO <> 0 and
088500030620     C                             wvario <> 8888 and wvario <> 9999)
088600020617     c                   do        *hival
088700020617     C                   EXFMT     LG20w01
088800020617     c   kf              leave
088900020617     c                   end
089000030617     c                   end
089100030617     c                   end
089200940308     C* VARIAZIONE BOLLA  C/ASS. O CODICE BOLLA
089300940308     C     *IN80         IFEQ      '1'
089400940308     C                   EXSR      PGARBK
089500941214     C     D48ERR        IFNE      ' '
089600941214     C                   SETON                                        19
089700941214     C                   MOVEL     D48MSG        $MSG
089800941214     C                   MOVEL     '1'           $F11
089900941214     C                   ENDIF
090000940310     C                   END
090100940308     C     *IN81         IFEQ      '1'
090200941219     C     $F11          ANDEQ     '0'
090300940308     C* VARIAZIONE BOLLA  IND. DESTINATARIO
090400051017     C                   MOVEL     'S'           D48FFR
090500940308     C                   EXSR      PGARBI
090600941214     C     D48ERR        IFNE      ' '
090700941214     C                   SETON                                        19
090800941214     C                   MOVEL     D48MSG        $MSG
090900941214     C                   MOVEL     '1'           $F11
091000941214     C                   ENDIF
091100940308     C                   END
091200940308     C* VARIAZIONE BOLLA  TASSAZ. 1° BOLLA
091300940308     C     *IN82         IFEQ      '1'
091400941219     C     $F11          ANDEQ     '0'
091500940308     C                   EXSR      PGARBT
091600941215     C     D48ERR        IFNE      ' '
091700941214     C                   SETON                                        19
091800941214     C                   MOVEL     D48MSG        $MSG
091900941214     C                   MOVEL     '1'           $F11
092000941214     C                   ENDIF
092100940308     C                   END
092200940308     C* VARIAZIONE BOLLA  TASSAZ. 2° BOLLA
092300940308     C     *IN83         IFEQ      '1'
092400941219     C     $F11          ANDEQ     '0'
092500940308     C                   EXSR      PGAR2T
092600941214     C     D48ERR        IFNE      ' '
092700941214     C                   SETON                                        19
092800941214     C                   MOVEL     D48MSG        $MSG
092900941214     C                   MOVEL     '1'           $F11
093000941214     C                   ENDIF
093100940308     C                   END
093200940308      * SE NON HO ERRORI OKF03 =1
093300940308     C                   ENDIF
093400940308     C                   ENDIF
093500940308     C                   ENDDO
093600940308      *
093700940308     C                   ENDDO
093800940308      * =======================================================
093900940308     C*?SUPERATI TUTTI I CONTROLLI   AGGIORNO
094000940308      * =======================================================
094100940308     C     $OKF03        IFEQ      '1'
094200940613      * SE ACCESO IND. PER BOLLA IN ARRIVO, MA è CAMBIATA LNA SPENGO 88
094300941217     C   88VLNA          IFNE      EXLNA
094400940613     C                   SETOFF                                       88
094500940613     C                   ENDIF
094600940613      *
094700940308     C                   EXSR      SRAGG
094800940308     C                   ENDIF
094900940310     C   99              SETON                                        19
095000940310     C   99              MOVEL     ERR(35)       $MSG
095100940308     C   99              GOTO      INIF02
095200940308     C     ENDF02        ENDSR
095300940308     C/SPACE 3
095400961202     C*---------------------------------------------------------------*
095500961202     C*   CALL21: RICHIAMO VIDEATA FAX
095600961202     C*---------------------------------------------------------------*
095700961202     C     CALL21        BEGSR
095800021009     C*-                                                   *
095900021009     c                   move      'L'           finep
096000021009     c                   z-add     filarr        codpo
096100021009     c                   move      '2'           tpcoda
096200021009     c                   clear                   nomcoda
096300021009     c                   clear                   libcoda
096400021009     c                   clear                   esito
096500021009     c                   call      'TRULOUTQ'
096600021009     c                   parm                    finep
096700021009     c                   parm                    codpo
096800021009     c                   parm                    tpcoda
096900021009     c                   parm                    nomcoda
097000021009     c                   parm                    libcoda
097100021009     c                   parm                    esito
097200021009     c                   if        esito <> '0' or nomcoda = *blank
097300021009     c                   eval      nomcoda = 'QDKT'
097400021009     c                   endif
097500961202     C*
097600961202     C                   CLEAR                   FNLG21
097700961202     C     $GIAC         IFEQ      '1'
097800050222     C                   Z-ADD     GCPAASa       D21AAS
097900050222     C                   Z-ADD     GCPLNPa       D21LNP
098000050222     C                   Z-ADD     GCPNRSa       D21NRS
098100050222     C                   Z-ADD     GCPNSPa       D21NSP
098200961202     C                   Z-ADD     GCPAGC        D21AGC
098300961202     C                   Z-ADD     GCPFGC        D21FGC
098400961202     C                   Z-ADD     GCPNGC        D21NGC
098500970418     C                   Z-ADD     0             D21FRG
098600961202     C                   MOVEL     'G'           D21TPR
098700961202     C                   MOVEL     VFRI          D21DIS
098800961202     C                   ELSE
098900961202     C                   Z-ADD     ARBAAS        D21AAS
099000961202     C                   Z-ADD     ARBLNP        D21LNP
099100961202     C                   Z-ADD     ARBNRS        D21NRS
099200961202     C                   Z-ADD     ARBNSP        D21NSP
099300961202     C                   MOVEL     'C'           D21TPR
099400961202     C                   MOVEL     VFRI          D21DIS
099500961202     C                   END
099600020927     C                   MOVE      nomcoda       D21PRF
099700000314     C*  Se lna Dpd imposto apposito flag al pgm gestione
099800000314     C*  invio fax in quanto cambia il modo di reperire il
099900000314     C*  il codice dell'anagrafica
100000020617     C     §OGntw        IFEQ      'DPD'
100100000314     C                   MOVEL     'S'           D21DPD
100200000314     C                   END
100300961202     C*  Richiamo videata x fax
100400961202     C                   MOVEL     FNLG21        KPJBU
100500961213     C                   CALL      'FNLG21C'
100600961202     C                   PARM                    KPJBA
100700961202     C*
100800961202     C                   ENDSR
100900940308     C*---------------------------------------------------------------*
101000940308     C*   C T R V 0 2      FINE GIACENZA                              *
101100940308     C*---------------------------------------------------------------*
101200940308     C     CTRV02        BEGSR
101300940309     C                   SETOFF                                         1922
101400940308     C                   SETOFF                                       566265
101500941003     C                   SETOFF                                       204959
101600941219     C                   SETOFF                                       66  64
101700940308     C                   MOVEL     *BLANK        $MSG
101800940310     C     $GIAC         IFEQ      '1'
101900941012     C                   MOVEA     VCFG          CFG§
102000940310     C     '?'           LOOKUP    CFG§                                   21
102100940308     C   21              DO
102200940308     C                   MOVEL     '2C'          §COD
102300940308     C                   MOVEL     *BLANK        §KEY
102400940308     C                   MOVEL     *BLANK        §DES
102500940308     C                   MOVE      *BLANK        VCFG
102600940308     C                   CALL      'X§TB60R'
102700940308     C                   PARM                    CODUT
102800940308     C                   PARM                    §COD
102900940308     C                   PARM                    §KEY
103000940308     C                   PARM                    §DES
103100940308     C                   MOVEL     §KEY          VCFG
103200940308     C                   MOVEL     §DES          DCFG
103300940309     C                   MOVEL     '1'           $RIEMI            1
103400940309     C                   GOTO      ENDCT2
103500940308     C                   END
103600071203      * verifico se sto dando disposizioni di riconsegna ad una giacenza
103700071203      * che ha in bolla la dcr impostata con una data maggiore della udate
103800071130      * ATTENZIONE non è un errore ma solo una segnalazione UNA_TANTUM
103900071130     c                   if        contr_dcr = *blank and
104000071130     c                             arbdcr > amgudt and arbtcr <> 'P'
104100071203     c                             and vfri = 'C'
104200071130     c                   move      amgudt        dadata
104300071130     c                   move      arbdcr        adata
104400071130     c                   CALL      'XSRLAV8'
104500071130     c                   PARM                    Wdata8
104600071130      * se il risultato è maggiore di 1 vuol dire che la dcr deve essere
104700071130      * modificata se si vuole gestire la bolla in consegna
104800071130     c                   if        giolav > 1
104900071130     c                   move      'X'           contr_dcr         1
105000071130     C                   MOVEL     '1'           $RIEMI
105100071130     c                   seton                                        19
105200071130     C                   MOVEL     ERR(66)       $MSG
105300071130     C                   GOTO      ENDCT2
105400071130     c                   endif
105500071130     c                   endif
105600940310      * CONTROLLO COD FINE GIAC.
105700940308     C                   MOVEL     '2C'          TBLCOD
105800940308     C                   MOVEL     *BLANK        TBLKEY
105900940308     C                   MOVEL     VCFG          TBLKEY
106000940310     C     KTAB          CHAIN     TABEL                              21
106100940308     C   21              MOVEL     *ALL'?'       DCFG
106200060410     C  N21              MOVEL     TBLUNI        Ds2c
106300060410     C  N21              MOVEL     §2cdes        DCFG
106400060801      * x non trovato o codice obsoleto segnalo errore
106500060801     c                   if        *in21 or §2cobs <> *blank
106600060801     C                   SETON                                        1922
106700060801     c                   endif
106800940308     C   22              MOVEL     ERR(17)       $MSG
106900940308     C                   END
107000940308     C* DESTINATARIO
107100940308     C     *IN46         IFEQ      '1'
107200940308     C     VRSD          IFEQ      *BLANK
107300940308     C     VIND          OREQ      *BLANK
107400940308     C     VPRD          OREQ      *BLANK
107500961205     C     VNZD          ANDEQ     *BLANK
107600940308     C     VLOD          OREQ      *BLANK
107700940309     C                   SETON                                        561965
107800940308     C                   MOVEL     ERR(19)       $MSG
107900940308     C                   END
108000940308     C                   END
108100940308     C* CAP A ZERO: INTERROGAZIONE SU CAPPARIO
108200941006     C     VCAD          IFEQ      *BLANK
108300940309     C     *IN46         ANDEQ     '1'
108400970711     C                   CLEAR                   TISI00
108500970711     C                   MOVEL     'L01'         D00OP0
108600970711     C                   MOVEL     VLOD          D00LOC
108700980219     C                   MOVEL     AMGUDT        D00DRI
108800970711     C                   CALL      'TISI30R'
108900970711     C                   PARM                    KPJBA
109000970711     C                   PARM                    TISI00
109100940308     C*
109200970711     C     D00CAP        IFNE      *BLANKS
109300970711     C                   MOVEL     D00CAP        VCAD
109400970711     C                   MOVEL     D00LOC        VLOD
109500970711     C                   MOVEL     D00PRV        VPRD
109600970711     C                   MOVEL     D00NAR        VNZD
109700940308     C                   END
109800940308     C*
109900940309     C                   MOVEL     '1'           $RIEMI
110000940308     C                   GOTO      ENDCT2
110100940308     C                   END
110200940930     C* CONTROLLO CAP
110300940308     C  N19*IN46         IFEQ      '1'
110400970710     C* Imposto parametri per routine controllo Cap
110500970709     C                   CLEAR                   TISI95
110600970710     C                   CLEAR                   FNLV13
110700970709     C                   MOVEL     '7'           I95TCN
110800970709     C                   MOVEL     *BLANK        I95TLA
110900970709     C                   MOVEL     VNZD          I95NAR
111000970710     C                   MOVEL     VIND          I95IND
111100970709     C                   MOVEL     VCAD          I95CAP
111200970709     C                   MOVEL     VLOD          I95LOC
111300970709     C                   MOVEL     VPRD          I95PRV
111400970710     C                   MOVEL     ARBPKF        I95LKG
111500970710     C                   MOVEL     ARBVLF        I95LMC
111600970709     C                   MOVEL     ARBFFD        I95FFD
111700970710     C                   MOVEL     ARBTSP        I95TSP
111800971202     C                   Z-ADD     1             §§                3 0
111900971202     C* CONTROLLO PORTO DELLA BOLLA ORIGINALE
112000971202     C     ARBCBO        LOOKUP    CDB(§§)                                04
112100971202     C                   MOVEL     §3ATB1        WPOR1             1
112200971202     C                   MOVEL     §3ATB2        WPOR2             1
112300971202     C     WPOR1         IFEQ      'A'
112400971202     C     §3ATB1        ANDNE     'AS'
112500971202     C     WPOR2         OREQ      'A'
112600971202     C     §3ATB2        ANDNE     'AS'
112700971202     C     §3ATB1        OREQ      'FS'
112800971202     C                   MOVEL     'A'           I95TPO
112900971202     C                   ELSE
113000971202     C                   MOVEL     'F'           I95TPO
113100971202     C                   END
113200971202     C* CONTROLLO SE LA BOLLA ORIGINALE AVEVA IL C/ASSEGNO
113300971202     C     §3AFCA        IFNE      0
113400971202     C                   MOVEL     'S'           I95FCA
113500971202     C                   ELSE
113600971202     C                   MOVEL     ' '           I95FCA
113700971202     C                   END
113800041018     C* passiamo le consegne particolari al TISI95 se dispos. non sono reso
113900041018     c     vfri          ifne      'R'
114000040929     C                   MOVEL     ARBtc1        I95fi2
114100040929     C                   MOVE      ARBtc2        I95fi2
114200041018     c                   else
114300041018     c                   clear                   I95fi2
114400041018     c                   endif
114500121107     c* .... Forzatura da tabella "FLB/FLA"
114600121107     c* Forzatura linea di arrivo per  linea di p.
114700121107      * gestito in questo modo parrebbe funzionare solo per le linee
114800121107      * estere perchè per l'italia proseguendo il giro torna fare il giro del
114900121107      * cappario e da errore
115000121109     C*?SPOSTATO IN CTRIND per provare a forzare
115100121109     c*m                 if        vfri = 'C'
115200121109     c*m                 clear                   trulflbds
115300121109     c*m                 movel     Vlna          iflblna
115400121109     c*m                 move      arblnp        Iflblnp
115500121109     c*m                 call      'TRULFLBR'
115600121109     c*m                 parm                    TRULFLBDS
115700121109    2c*m                 if        Oflblfz>*zeros
115800121109     c*m                 move      OflbLFZ       vlna
115900121109    2c*m                 endif
116000121109    2c*m                 endif
116100120330     c*********
116200120330     c***** Era stato fatto x Ducros come Partner contemporaneo a Sernam sulla
116300120330     c***   Francia. Una volta tolta Sernam come Partner e diventata Ducros
116400120330      ***   Partner Francese unico su linea 330 (anzichè 333) questo chiodo
116500120330     c***** deve essere rimosso.
116600120330     c*********
116700120124     c* CHIODO TEMPORANEO FORZATURA LINEA 330
116800120330     c**** vfri          ifEQ      'C'
116900120330     c**** ARBLNa        ANDEQ     333
117000120330     C********           Z-ADD     ARBLNa        VLNA
117100120330     c*********          ENDIF
117200971202     C*
117300970710     C                   MOVEL     'S'           I13AF0
117400970710     C                   MOVEL     'S'           I13AF1
117500970710     C                   MOVEL     'S'           I13SZ2
117600970710     C                   MOVEL     'S'           I13LA3
117700970710     C                   MOVEL     'S'           I13SZ3
117800970710     C                   MOVEL     'S'           I13CNV
117900970710     C                   MOVEL     'S'           I13SZV
118000970710     C* Gestione errori forzabili: se cambiato CAP/Località
118100970710     C* riazzero i valori di errori forzabili
118200970710    2C     VCAD          IFNE      WSVCAP
118300970710     C                   CLEAR                   WFZV
118400970710     C                   CLEAR                   WFZ1
118500970710     C                   CLEAR                   WFZ2
118600970710     C                   CLEAR                   WFZ3
118700970710     C                   MOVEL     VCAD          WSVCAP
118800970710    2C                   ENDIF
118900970710     C* Modifica località
119000970710    2C     VLOD          IFNE      WSVLOC
119100970710     C                   MOVEL     VLOD          WSVLOC
119200970710     C                   CLEAR                   WFZ1
119300970710     C                   CLEAR                   WFZ2
119400970710     C                   CLEAR                   WFZ3
119500970710    2C                   ENDIF
119600970710     C* Modifica PROVINCIA
119700970710    2C     VPRD          IFNE      WSVPRO
119800970710     C                   MOVEL     VPRD          WSVPRO
119900970710     C                   CLEAR                   WFZ3
120000970710    2C                   ENDIF
120100970710     C* Se il livello di affidabilità non è inferiore a 2 e
120200970710     C** CALL
120300970710     C                   MOVEL     WFZV          E13FZV
120400970710     C                   MOVEL     WFZ1          E13FZ1
120500970710     C                   MOVEL     WFZ2          E13FZ2
120600970710     C                   MOVEL     WFZ3          E13FZ3
120700970710     C                   CALL      'FNLV13R'
120800970710     C                   PARM                    KPJBA
120900970710     C                   PARM                    FNLV13
121000970710     C                   PARM                    TISI95
121100970710     C* Ricontrollo errori
121200970710     C                   MOVEL     O13ERR        WERR
121300970710     C                   MOVEL     O13MSG        WMSG
121400970710     C                   MOVEL     E13FZV        WFZV
121500970710     C                   MOVEL     E13FZ1        WFZ1
121600970710     C                   MOVEL     E13FZ2        WFZ2
121700970710     C                   MOVEL     E13FZ3        WFZ3
121800970710     C* reimposto dati da TISI95
121900970710     C     O13ROC        IFEQ      'S'
122000970710     C     O13RON        OREQ      'S'
122100970710     C     O13ROP        OREQ      'S'
122200970710     C     O13ROL        OREQ      'S'
122300970710     C                   MOVEL     O95LOC        VLOD
122400970710     C                   MOVEL     O95PRV        VPRD
122500970710     C                   MOVEL     O95NAR        VNZD
122600970710     C                   MOVEL     O95CAP        VCAD
122700970710     C                   END
122800970710     C     O95ERR        IFEQ      *BLANKS
122900040423      * se solo cambio di porto non varia la zona per la nuova bolla
123000040423     c                   if        vfri = 'C' and export <> vnpc
123100040423     c                   move      *blank        §lpznc
123200070326     c                   move      *blank        §lpcgi
123300040423     c                   else
123400070904     C  n55              MOVEL     O95ZNC        §LPZNC
123500040423     c                   end
123600970710     C                   MOVEL     O95LNA        §LNA
123700000313     C* se ex lna DPD e sto eseguendo una riconsegna non reiposto la
123800000313     C* lna in base al Cappario ma rimane uguale alla vecchia
123900000313     C     VFRI          IFEQ      'C'
124000000313     C     ARBLNA        CHAIN     AZORG01L                           21
124100000313     C     *IN21         IFEQ      '0'
124200000313     C     ORGFVA        ANDNE     '*'
124300000313     C                   MOVEL     ORGDE3        OG143
124400020225     C     §OGNTW        IFEQ      'DPD'
124500020225     C     §OGNTW        OREQ      'FED'
124600000313     C                   MOVEL     ARBLNA        §LNA
124700000313     C                   END
124800000313     C                   END
124900000313     C                   END
125000000313     C                   END
125100060103      * richiamo pgm x verificare che non sia un destinatario particolare
125200060103      * il lancio viene effettuato per C e D clienti non disk b
125300060103      * e per i resi se non c'è stata già la forzatura della linea arrivo
125400091015     c                   if        vlna = 0
125500060103     c                   clear                   des_par           1
125600071025     c                   clear                   des_parg          1
125700091015     c                   endif
125800091015
125900170321     c**                 if        vfri = 'C' and arbnrs = 0 or
126000170321     c**                           vfri = 'D' and arbnrs = 0 or
126100170322     c**                           vfri = 'R' and vlna = 0
126200170321     c                   if        vfri = 'C' or
126300170321     c                             vfri = 'D' or
126400170322     c                             vfri = 'R' and §lplna = 0
126500060103     c                   clear                   tisit0ds
126600060104     c                   move      i95loc        IT0loc
126700060104     c                   move      i95prv        IT0prv
126800060104     c                   move      i95nar        IT0naz
126900060104     c                   move      i95cap        IT0cap
127000060104     c                   move      i95ind        IT0ind
127100060103     C                   movel     VRSD          IT0rag
127200071008     c                   clear                   tisit0x1ds
127300060103     c                   call      'TISIT5R'
127400060103     c                   parm                    tisit0ds
127500071008     c                   parm                    tisit0x1ds
127600060103      * se non c'è errore risulta trovato quindi imposta flag per bypassare
127700060103      * i controlli sulla linea che viene forzata dalla ds di ritorno
127800060103    2c     ot0err        ifeq      *blanks
127900071025     c                   move      'X'           des_parg
128000060103     c                   movel     ot0flo        ddstflo
128100060404     c                   if        §dstlna  <> *blank
128200170321     c*                  move      §DSTLNA       vlna
128300170321     c                   move      §DSTLNA       §lna
128400071025     c                   move      §DSTZNC       §lpznc
128500060404     c                   move      'X'           des_par
128600060404     c                   endif
128700060103     c                   endif
128800060103     c                   endif
128900970710     C*
129000970710     C                   EXSR      CTRIND
129100970710     C                   END
129200971113     C*
129300970710     C   19              GOTO      ENDCT2
129400971113     C*
129500040531      * verifica dirottamenti
129600040531      * non è ammesso il dirottamento se il cap o la località sono
129700040531      * comunque serviti dalla linea d'arrivo indipendentemente da
129800040531      * limiti di peso o altro
129900040719      * sono escluse le eccezioni della seconda condizione
130000040607     C  N19*IN46         IFEQ      '1'
130100040607     C     VFRI          ANDEQ     'D'
130200060103     C     des_par       ANDEQ     *blank
130300170323      *reperisce eventuali eccezioni organigramma per forzatura dei dirottamenti
130400170323     c                   exsr      SRog150
130500040719      *verifico se periodo con dirottamento permesso causa limitazioni traffico
130600040719     c                   if        amgudt < §2g2dpi or amgudt > §2g2dpf
130700040719
130800040531     c                   if        flgest <> 'E' and vlna <> 101 and
130900040601     c                             vlna <> 102 and arbffd <> 'S'
131000170323     c                             and arbfbr <> 'S' and
131100170323     c                             §OGESBLDI <> 'S'
131200040531     c                   if        exlna <> vlna
131300040531     c                   if        o95lla > 0
131400040531     c                   if        exlna = o95lla or exlna = o95llo or
131500040531     c                             exlna = o95lls
131600040531     c                   seton                                        1949
131700040531     C                   movel     err(65)       $msg
131800040531     C                   GOTO      ENDCT2
131900040531     c                   endif
132000040531     c                   else
132100040531     c                   if        exlna = o95cla or exlna = o95clo or
132200040531     c                             exlna = o95cls
132300040531     c                   seton                                        1949
132400040531     C                   movel     err(65)       $msg
132500040531     C                   GOTO      ENDCT2
132600040531     c                   endif
132700040531     c                   endif
132800040531     c                   endif
132900040531     c                   endif
133000040719     c                   endif
133100040607     c                   ENDIF
133200971113     C* Controllo se la lna esiste
133300071126      * verifico se la filiale è gestita in £6 da qualcuno nel caso
133400071126      * i riferimenti di azorg x le ds og146/148 sono quelli della capofila
133500071126      **
133600071126     c                   clear                   Og_da_£6          1
133700071126     c                   exsr      capofila
133800071126      *
133900971113     C                   MOVEL     *BLANK        VDLNA
134000971113     C     VLNA          CHAIN     AZORG01L                           21
134100971113     C     *IN21         IFEQ      '0'
134200971113     C     ORGFVA        COMP      '*'                                    21
134300971113     C                   MOVEL     ORGDES        VDLNA
134400071126      * se gestita da altri le ds vengono impostate con i campi valorizzati
134500071126      * nella routine capofila
134600071126     c                   if        Og_da_£6 = *blank
134700051221     C                   MOVEL     orgde8        og148
134800070416     C                   MOVEL     orgde6        og146
134900071126     c                   endif
135000070424     c                   if        §ogdda <> *blank
135100070424     c                   move      §ogdda        ogddan            8 0
135200070424     c                   endif
135300051221      *---
135400051221      *  Linea Arrivo non utilizzabile in Bollettazione
135500051221      *     es.: x DPD  le nuove linee di Backup sono settate così.
135600051221     C     §OGLBL        IFEQ      'N'
135700051221     C                   SETON                                        1949
135800051221     C                   MOVEL     ERR(21)       $MSG
135900051221    2C                   ENDIF
136000051221      *---
136100971113     C* Controllo se la lna è congruente con il sif.in cui sono
136200971113     C                   ELSE
136300971113     C                   MOVEL     *ALL'?'       VDLNA
136400971113     C                   SETON                                        1949
136500971113     C                   MOVEL     ERR(21)       $MSG
136600971113     C                   END
136700940308     C* CMD 09 X FORZARE LINEA ARRIVO
136800940309      *------------------------------
136900940309     C   KI              MOVEL     '1'           $FOLNA
137000940308     C*
137100170323      *Se filiale esente blocco dirottamento non effettua questo controllo
137200170323     c                   if        §ogesbldi <> 'S'
137300940309     C   46
137400940309     CANN19$FOLNA        IFEQ      '0'
137500940316     C     VLNA          IFNE      *ZEROS
137600940308     C     §LNA          COMP      VLNA                               2121
137700940309     C   21              SETON                                        194959
137800940308     C   21              MOVEL     ERR(22)       $MSG
137900940316     C                   END
138000940308     C                   END
138100170323     c                   endif
138200940309     C* LINEA ARRIVO
138300940308     C*  SE HO FORZATO LA LINEA LA ZONA = 0
138400940309     C   46$FOLNA        IFEQ      '1'
138500940308     C     §LNA          COMP      VLNA                               2121
138600070904     c  n55
138700070904     Can 21              MOVE      *ZEROS        §LPZNC               1949
138800940308     C                   END
138900070509     c* verifico se per la linea di arrivo che si sta impostando è presente
139000070509      * un giro predefinito per assegnarlo solo in caso di RESO e se esiste
139100070509      * un LUOGO 300 legato al codice cliente
139200070510     c                   if        vfri = 'R' and vlna <> §lplna
139300070510     c                   clear                   §lpcgi
139400070510     c                   exsr      ecceluogo2
139500070510     c                   endif
139600941003     C* CONTROLLO NAZIONE
139700941217     C  N19
139800941217     CAN 46              DO
139900941003     C                   MOVEL     '15'          TBLCOD
140000941003     C                   MOVEL     *BLANK        TBLKEY
140100941003     C                   MOVEL     VNZD          TBLKEY
140200941003     C     KTAB          CHAIN     TABEL                              21
140300941003     C   21              MOVEL     *ALL'?'       §15DES
140400941003     C   21              SETON                                        6619
140500941003     C   21              MOVEL     ERR(40)       $MSG
140600941003     C  N21              MOVEL     TBLUNI        DS15
140700941003     C                   END
140800941217     C* CONTROLLO TIPO INCASSO CONTASSEGNO SE GIA EMESSO IL CAMPO
140900941217     C     *IN34         IFEQ      *ON
141000941217     C* RICERCA
141100941217     C     '?'           SCAN      VTIC                                   21
141200941217     C*
141300941217     C     *IN21         IFEQ      *ON
141400941217     C                   MOVEL     '1A'          §COD
141500941217     C                   MOVE      *BLANKS       VTIC
141600941217     C                   CALL      'X§TABER'
141700941217     C                   PARM                    CODUT
141800941217     C                   PARM                    §COD
141900941217     C                   PARM                    §KEY
142000941217     C                   PARM                    §DES
142100941217     C                   MOVEL     §KEY          VTIC
142200941221     C                   MOVEL     '1'           $RIEMI
142300941217     C                   GOTO      ENDCT2
142400941217     C                   ENDIF
142500941217     C                   ENDIF
142600941217     C* SE HANNO FATTO VARIAZIONI SUL C/ASSEGNO  CONTROLLO
142700941217     C* IL TIPO INCASSO C/ASSEGNO E LA CONGRUENZA CON LA DIVISA
142800941217     C     VVCS          IFEQ      'S'
142900020417     C     VASS          ANDNE     0
143000941217     C                   MOVEL     '1A'          TBLCOD
143100941217     C                   MOVEL(P)  VTIC          TBLKEY
143200941217     C     KTAB          CHAIN     TABEL                              21
143300941217     C   21              SETON                                        6419
143400941217     C   21              MOVEL     ERR(43)       $MSG
143500941217     C  N21              MOVEL     TBLUNI        DS1A
143600941222     C     VVCS          IFEQ      'S'
143700941222     C     VASS          ANDEQ     0
143800941222     C     *IN34         ANDEQ     *OFF
143900941222     C                   SETON                                        34
144000941222     C                   MOVEL     '1'           $RIEMI
144100941222     C                   GOTO      ENDCT2
144200941222     C                   ENDIF
144300941217     C* TIPO INCASSO ERRATO
144400941217     C     *IN64         IFEQ      *ON
144500941222     C* SE HO AGGIUNTO IL C/ASSEGNO  CHIEDO IL TIPO INCASSO
144600941217     C* SE DIVISA ESTERA VEDO SE POSSO ACCETTARE IL TIPO INCASSO
144700020227     C     §CVDVA        ORNE      'S'
144800941217     C     §1AUDE        ANDNE     'S'
144900941217     C* SE IL TIPO INCASSO NON è CONGRUENTE EMETTO A VIDEO IL CAMPO
145000941217     C* PER POTERLO CORREGGERE
145100941217     C     *IN34         IFEQ      *OFF
145200941217     C                   SETON                                        34
145300941217     C                   MOVEL     '1'           $RIEMI
145400941217     C                   GOTO      ENDCT2
145500941217     C                   ELSE
145600941217     C* ALTRIMENTI SE L'HO GIA' EMESSO DO ERRORE
145700941217     C                   SETON                                        19
145800941217     C                   MOVEL     ERR(43)       $MSG
145900941217     C                   GOTO      ENDCT2
146000941217     C                   ENDIF
146100941217     C                   ENDIF
146200941217     C                   ENDIF
146300941217     C*
146400940308     C* SE RICHIAMO PGM TASSAZ. LA LINEA ARRIVO DEVE ESSERE =
146500940308     C  N19
146600940308     CAN 87VSIFAT        IFEQ      'S'
146700941216     C     VLNA          ANDNE     EXLNA
146800940308     C                   SETON                                        1962
146900940308     C                   MOVEL     ERR(18)       $MSG
147000940308     C                   END
147100971006      *-------------------------------------------------------
147200971006     C* Recupero codice cliente mittente
147300971006     C     EXPORT        IFEQ      'A'
147400971006     C     WEXTB1        ANDNE     'AS'
147500971006     C                   MOVE      ARBCCM        WVARIO            4 0
147600971006     C                   ELSE
147700971006     C                   MOVE      ARBKSC        WVARIO
147800971006     C                   END
147900941215     C* CMD 08 X FORZARE DIROTTAMENTO O RICONSEGNA IN BASE LNA
148000941215      *-------------------------------------------------------
148100940309     C   KH              DO
148200971006     C* Dirottamento non forzabile se provengo da giacenza
148300971006     C* e il mittente non è codificato
148400971006     C     WVARIO        IFNE      0
148500971006     C     WVARIO        ANDNE     8888
148600971006     C     WVARIO        ANDNE     9999
148700980505     C     VFRI          OREQ      'D'
148800971006     C     $GIAC         ORNE      '1'
148900060228     C     des_par       ORNE      *blank
149000940309     C                   MOVEL     '1'           $FOFRI
149100940309     C                   MOVEL     VRSD          SAVRSD
149200941011     C                   MOVEL     VRD2          SAVRD2
149300940309     C                   MOVEL     VIND          SAVIND
149400940309     C                   MOVEL     VLOD          SAVLOD
149500940309     C                   MOVEL     VPRD          SAVPRD
149600940309     C                   MOVE      VCAD          SAVCAD
149700940309     C                   MOVE      VLNA          SAVLNA
149800961203     C                   MOVEL     VNZD          SAVNZD
149900031119     C                   MOVEL     Vteld         SAVteld
150000031119     C                   MOVEL     Vref          SAVref
150100940309     C                   END
150200971006     C                   END
150300940308      * SE LINEA ARRIVO DIVERSA  E DISPOSIZIONI SONO DI RICONSEGNARE
150400940308      * SEGNALO CHE LE DISPOSIZ. SARANNO CAMBIATE IN DIROTTAM.
150500940309     C     $FOFRI        IFEQ      '0'
150600941229     C     VLNA          ANDNE     *ZEROS
150700941216     C     VLNA          ANDNE     EXLNA
150800940308     C     VFRI          ANDEQ     'C'
150900971006     C* Se devo forzare disposizioni di dirottamento nel caso
151000971006     C* di spedizioni con codice cliente mittente non codif.
151100971006     C* su una bolla giacente do' errore perchè le spedizioni
151200971006     C* non possono essere immesse in arrivo
151300971006     C     WVARIO        IFNE      0
151400971006     C     WVARIO        ANDNE     8888
151500971006     C     WVARIO        ANDNE     9999
151600971006     C     $GIAC         ORNE      '1'
151700060228     C     des_par       ORNE      *blank
151800940308     C                   SETON                                        19
151900940308     C                   MOVEL     ERR(29)       $MSG
152000971006     C                   ELSE
152100971006     C                   SETON                                        19
152200971006     C                   MOVEL     ERR(56)       $MSG
152300940308     C                   END
152400971006     C                   END
152500940308     C*
152600940308     C   19              GOTO      ENDCT2
152700941215      * SE LINEA ARRIVO UGUALE MA  DISPOSIZIONI SONO DI DIROTTARE
152800941215      * SEGNALO CHE LE DISPOSIZ. SARANNO CAMBIATE IN RICONSEGNA
152900961128     C     $FOFRI        IFEQ      '0'
153000961128     C     VLNA          ANDEQ     EXLNA
153100941215     C     VFRI          ANDEQ     'D'
153200941215     C                   SETON                                        19
153300941215     C                   MOVEL     ERR(42)       $MSG
153400941215     C                   END
153500961128     C   19              GOTO      ENDCT2
153600941215     C*
153700941215     C   19              GOTO      ENDCT2
153800940308     C* VARIAZIONE INDIRIZZO X RICONSEGNA NO NUOVA BOLLA
153900940308     C  N31VFRI          IFEQ      'C'
154000940308     C     VRSD          IFNE      ARBRSD
154100941011     C     VRD2          ORNE      AR4RD2
154200940308     C     VIND          ORNE      ARBIND
154300940308     C     VLOD          ORNE      ARBLOD
154400940308     C     VPRD          ORNE      ARBPRD
154500940308     C     VCAD          ORNE      ARBCAD
154600940930     C     VNZD          ORNE      ARBNZD
154700060314     C     Vref          ORNE      §AR5REF
154800060317     C     flgdis        andne     'F'
154900060314     C     Vteld         ORNE      §AR5TELD
155000940308     C                   SETON                                        81
155100940308     C                   END
155200940308     C                   END
155300940310     C* DIROTTAMENTO 2 - OPERAZ. DA FARE IN BASE AI DATI IMPOSTATI
155400940310     C     VFRI          IFEQ      'D'
155500940310     C                   EXSR      DIROT2
155600940310     C   19              GOTO      ENDCT2
155700940310     C                   END
155800940308     C     ENDCT2        ENDSR
155900970710     C*------------------------------------------------------*
156000970710     C*  Controllo dati indirizzo
156100970710     C*------------------------------------------------------*
156200970710     C     CTRIND        BEGSR
156300970710     C*
156400970710     C* Errore !!!!
156500970710     C     WERR          IFNE      ' '
156600970710     C     WMSG          ANDNE     *BLANKS
156700970710     C* Manca la Ragione sociale
156800970710     C                   SELECT
156900970710     C* Errore su località/città
157000970710     C     WERR          WHENEQ    '3'
157100970710     C                   SETON                                        5619
157200970710     C* Errore su Cap
157300970710     C     WERR          WHENEQ    '5'
157400970710     C                   SETON                                        5919
157500970710     C* Errore su provincia
157600970710     C     WERR          WHENEQ    '4'
157700970710     C                   SETON                                        6519
157800970710     C* Nazione  errata
157900970710     C     WERR          WHENEQ    '6'
158000970710     C                   SETON                                        6619
158100970710    2C                   ENDSL
158200970710     C                   MOVEL     WMSG          $MSG
158300970710     C                   END
158400970710     C* Se non ci sono errori proseguo
158500970710     C   19              GOTO      FININD
158600970710     C*
158700970710     C* Controllo se reimpostata località
158800970710     C     O13ROL        IFEQ      'S'
158900970710     C                   SETON                                        5619
159000970710     C                   MOVEL     ERR(52)       $MSG
159100970710     C                   GOTO      FININD
159200970710     C                   END
159300970710     C* Controllo se reimpostata Provincia
159400970710     C     O13ROP        IFEQ      'S'
159500970710     C                   SETON                                        6519
159600970710     C                   MOVEL     ERR(54)       $MSG
159700970710     C                   GOTO      FININD
159800970710     C                   END
159900970710     C* Controllo se reimpostato CAP
160000970710     C     O13ROC        IFEQ      'S'
160100970710     C                   SETON                                        5919
160200970710     C                   MOVEL     ERR(53)       $MSG
160300970710     C                   GOTO      FININD
160400970710     C                   END
160500970710     C* Controllo se reimpostata Nazione
160600970710     C     O13RON        IFEQ      'S'
160700970710     C                   SETON                                        6619
160800970710     C                   MOVEL     ERR(55)       $MSG
160900970710     C                   GOTO      FININD
161000970710     C                   END
161100970710     C*
161200970710     C  N19VLNA          IFEQ      0
161300970710     C                   MOVE      §LNA          VLNA
161400970710     C                   END
161500121112     c                   if        flgest  = 'E' and
161600121112     c                             (vfri = 'C' or vfri = 'D')
161700141117     c                   if        (vnzd = arbnzd or arbnrs <> 0)
161800141117     c                             and arblna <> 330
161900121112     c                   move      arblna        vlna
162000121112     c                   else
162100121109     c                   clear                   trulflbds
162200121109     c                   movel     Vlna          iflblna
162300121109     c                   move      arblnp        Iflblnp
162400121109     c                   call      'TRULFLBR'
162500121109     c                   parm                    TRULFLBDS
162600121109    2c                   if        Oflblfz>*zeros
162700121109     c                   move      OflbLFZ       vlna
162800121109    2c                   endif
162900121112    2c                   endif
163000121109    2c                   endif
163100970710     C*
163200970710     C     FININD        ENDSR
163300940310     C* ------------------------------------------------------
163400940310     C* D I R O T 2  - OPERAZIONI DA FARE X DIROTTAM
163500940310     C* ------------------------------------------------------
163600940310     C     DIROT2        BEGSR
163700941217     C*DIROTTO SEMPRE AD ALTRA LINEA ARRIVO
163800941217     C* -----------------------------------
163900941010     C                   EXSR      DIRALA
164000941217     C* -----------------> TRATTO LA VECCHIA BOLLA
164100941217     C*
164200941217     C* SE ERA ASSEGNATO NON CONTAB.--> TRASFORMO IN CONTO SERVIZIO
164300940310     C  N26
164400940310     CANN27EXPORT        IFEQ      'A'
164500950726     C     WEXTB1        ANDNE     'AS'
164600940310     C                   MOVEL     §CDVAR        §§KCVB
164700940310     C                   SETON                                        80
164800940310     C                   MOVEL     'AS'          NWTB1
164900940310     C                   SETON                                        39
165000940310     C                   END
165100941217     C* SE ERA FRANCO E NUOVO PORTO =A --> TRASFORMO IN CONTO SERVIZIO
165200940310     C  N26
165300940310     CANN27EXPORT        IFEQ      'F'
165400950727     C     WEXTB1        ANDNE     'FS'
165500940310     C     VNPC          ANDEQ     'A'
165600940310     C                   MOVEL     §CDVAR        §§KCVB
165700940310     C                   SETON                                        80
165800940310     C                   MOVEL     'FS'          NWTB1
165900940310     C                   SETON                                        39
166000940310     C                   END
166100940310     C*CERCO NUOVO COD. BOLLA
166200940310     C  N26
166300940310     CANN27*IN80         IFEQ      '1'
166400940310     C                   MOVEL     *BLANK        NWCBL
166500940310     C                   MOVEL     *BLANK        §BKCBN
166600940310     C                   EXSR      NEWCBL
166700940310     C  N30              MOVEL     NWCBL         §BKCBN
166800940310     C                   END
166900940310     C* RICH. NOTA ACCREDITO
167000940310     C     *IN26         IFEQ      '1'
167100991001     C     AR6TRC        ANDEQ     '1'
167200991001     C     AR6FIV        ANDEQ     ARBLNA
167300950726     C     ARBICA        ANDNE     'Y'
167400940310     C                   SETON                                        32
167500940930     C                   Z-ADD     AR6IMV        §INAC1
167600940310     C                   END
167700940310     C     *IN27         IFEQ      '1'
167800991001     C     AR6TRC        ANDEQ     '2'
167900950726     C     ARBICA        ANDNE     'Y'
168000940310     C                   SETON                                        37
168100991001     C                   Z-ADD     AR6IMV        §INAC2
168200940310     C                   END
168300941217     C* -----------------> TRATTO LA NUOVA BOLLA
168400940310     C                   SETON                                        31
168500940310     C                   MOVEL     *BLANK        NWTB2
168600940310     C* NUOVO PORTO = F O ERA GIA FRANCO
168700940310     C     VNPC          IFEQ      'F'
168800940310     C     VNPC          OREQ      ' '
168900940310     C     EXPORT        ANDEQ     'F'
169000941010      *
169100940310     C                   MOVEL     'F7'          NWTB1
169200940310     C* SE C'E' C/ASSEGNO
169300940310     C     VNCA          IFGT      0
169400940310     C     VVCS          OREQ      'N'
169500940310     C     VASS          ANDGT     0
169600940310     C*  E C'ERA LA 2°BOLLA E NON HO VARIATO C/ASS. E NON PAGA "M"
169700940310     C*   O DICO CHE IL DEST. PAGA PROVV.C/A
169800940310     C     §3ATB2        IFEQ      'A3'
169900940310     C     VVCS          ANDEQ     'N'
170000940310     C     VPPC          ANDNE     'M'
170100940310     C     VPPC          OREQ      'D'
170200940310     C                   MOVEL     'A3'          NWTB2
170300940310     C                   END
170400940310     C                   END
170500940310     C                   END
170600940310     C* NUOVO PORTO = A O ERA GIA ASSEGNATO
170700940310     C     VNPC          IFEQ      'A'
170800940310     C     VNPC          OREQ      ' '
170900940310     C     EXPORT        ANDEQ     'A'
171000940310     C                   MOVEL     'A2'          NWTB1
171100940310     C                   MOVEL     '  '          NWTB2
171200940310     C                   END
171300940310     C* CERCO NUOVO CODICE BOLLA
171400940310     C     VNCA          IFGT      0
171500940310     C     VVCS          OREQ      'N'
171600940310     C     VASS          ANDGT     0
171700940310     C     NWTB2         IFEQ      *BLANK
171800940310     C                   MOVEL     '1'           NWFCA
171900940310     C                   ELSE
172000940310     C                   MOVEL     '2'           NWFCA
172100940310     C                   END
172200940310     C                   ELSE
172300940310     C                   MOVEL     '0'           NWFCA
172400940310     C                   END
172500940310     C                   MOVEL     *BLANK        NWCBL
172600940310     C                   MOVEL     '0'           NWFTD
172700941003     C                   MOVEL     *BLANK        §LPCBO
172800940310     C                   EXSR      NEWCBL
172900030528     c* per bolle FW (servizio c/a) se dirottamento forzo sempre lo stesso
173000030528     c* codice bolla
173100030528     c                   if        arbcbo = 'FW'
173200030528     c                   movel     arbcbo        nwcbl
173300030528     c                   setoff                                       30
173400030528     c                   end
173500941003     C  N30              MOVEL     NWCBL         §LPCBO
173600011130     C     §LPCBO        IFEQ      'A '
173700011130     C     ARBCBO        ANDNE     'A '
173800011130     C                   MOVEL     '$'           §LPCBO
173900011130     C                   END
174000941217     C* SPOSTO C/A SU NUOVA BOLLA
174100941217     C     NWFCA         IFEQ      '1'
174200940310     C     NWFCA         OREQ      '2'
174300940930     C     VASS          IFGT      *ZEROS
174400940310     C     VVCS          ANDEQ     'N'
174500941003     C                   Z-ADD     AR9CAS        §LPCAS
174600941214     C                   MOVEL     AR9VCA        §LPVCA
174700941003     C                   MOVEL     AR9TIC        §LPTIC
174800940310     C                   END
174900940310     C     VNCA          IFGT      *ZEROS
175000941003     C                   Z-ADD     VNCA          §LPCAS
175100941003     C                   MOVEL     VVCA          §LPVCA
175200941217     C                   MOVE      VTIC          §LPTIC
175300940310     C                   END
175400960125     C                   ELSE
175500960125     C                   Z-ADD     0             §LPCAS
175600960125     C                   MOVEL     *BLANKS       §LPVCA
175700960125     C                   MOVEL     *BLANKS       §LPTIC
175800940310     C                   END
175900940310     C*  LP
176000941217     C                   MOVE      EXLNA         §LPLNP
176100940310     C* IMPOSTO §LPNOT
176200940310     C                   EXSR      XLPNOT
176300940310     C     ENDDI2        ENDSR
176400941010     C* ----------------------------------------------------
176500941010     C* DIROTTAMENTO ALTRA  LINEA ARRIVO
176600941010     C* ---------------------------------------------------
176700941010     C     DIRALA        BEGSR
176800960125     C*
176900960125     C*  SE LA NUOVA LINEA DI ARRIVO E' UGUALE A 101 AZZERO
177000960125     C*  C/ASSEGNO/ANTICIPATA/SE PORTO ASSEGNATO --> FRANCO
177100960125     C     *INKF         IFEQ      '1'
177200960125     C     VLNA          ANDEQ     101
177300960227     C     *INKF         OREQ      '1'
177400960227     C     VLNA          ANDEQ     102
177500960125     C     VASS          IFNE      0
177600960125     C                   MOVEL     'S'           VVCS
177700960125     C                   Z-ADD     0             VNCA
177800960125     C                   END
177900960125     C     EXPORT        IFEQ      'A'
178000960125     C                   MOVEL     'F'           VNPC
178100960125     C                   END
178200960125     C                   END
178300941010     C*  DEVO TOGLIERE C/A DALLA VECCHIA BOLLA E METTERLO SULLA NUOVA
178400941010     C     VASS          IFGT      0
178500941215     C                   MOVE      *ZEROS        §BKCAS
178600941219     C                   MOVE      *BLANK        §BKVCA
178700941010     C                   MOVEL     'KB'          §CDVAR
178800941010     C                   END
178900941010     C     §CDVAR        IFEQ      'KB'
179000941010     C                   EXSR      CNTVAR
179100941010     C  N26
179200941010     CANN27              MOVEL     §CDVAR        §§KCVB
179300941010     C  N26
179400941010     CANN27              SETON                                        80
179500941010     C                   END
179600941010     C* GUARDO SE DEVO VARIARE TIPO BOLLA  PRIMA IMPOSTO COME ERA
179700941010     C                   MOVEL     §3ATB1        NWTB1
179800941010     C                   MOVEL     §3ATB2        NWTB2
179900941010     C                   MOVEL     §3AFCA        NWFCA
180000941010     C                   MOVEL     §3AFAN        NWFAN
180100941010     C                   MOVEL     §3AFTD        NWFTD
180200941010     C                   MOVEL     'K8'          §CDVAR
180300941010     C                   EXSR      CNTVAR
180400941010     C  N26
180500941010     CANN27VASS          IFGT      *ZEROS
180600941010     C                   MOVEL     §CDVAR        §§KCVB
180700941010     C                   SETON                                        80
180800941010     C                   MOVEL     '0'           NWFCA
180900941010     C  N27NWTB2         IFEQ      'A3'
181000950727     C     NWTB2         OREQ      'AP'
181100941010     C                   MOVEL     '  '          NWTB2
181200941010     C                   SETON                                        39
181300941010     C                   END
181400941010     C                   END
181500140610      * codice trattamento merce per SEGNACOLLI NUOVI
181600140610     c                   exsr      srctm2k
181700941010     C                   ENDSR
181800940308     C*---------------------------------------------------------------*
181900140609     C* forzo cod.tratt.merce 2k in caso di dirottamento per fare stampare
182000140609     c* il segnacollo in arrivo
182100940308     C*---------------------------------------------------------------*
182200140609     C     SRctm2k       BEGSR
182300140610     C                   MOVEL     '6 '          §LPCTM
182400140729     c
182500170328     c                   clear                   FNLV55ds
182600170328     c                   eval      D55tpt =  '6'
182700170328     c                   eval      D55lin =  exlna
182800170328     c                   eval      D55drf =  amgudt
182900170328     c                   call      'FNLV55R'
183000170328     c                   parm                    FNLV55ds
183100170228     c                   clear                   og150
183200170328     C     d55tfa        CHAIN     AZORG01L                           21
183300140729     c                   if        not *in21
183400140729     C                   MOVEL     ORGDf0        OG150
183500170228     c                   if        §OGDKCDIR ='S'
183600140609     C                   MOVEL     '2K'          §LPCTM
183700140729     c                   endif
183800140729     c                   endif
183900140729     c
184000140609     C                   ENDSR
184100170323     C*---------------------------------------------------------------*
184200170323     C* Eseguo reperimento OG150 generico con EXLNA
184300170323     C*---------------------------------------------------------------*
184400170323     C     SRog150       BEGSR
184500170323     c                   clear                   og150
184600170323     C     exlna         CHAIN     AZORG01L                           21
184700170323     c                   if        not *in21
184800170323     C                   MOVEL     ORGDf0        OG150
184900170323     c                   endif
185000170323     c
185100170323     C                   ENDSR
185200140609     C*---------------------------------------------------------------*
185300140609     C* S R F 0 3     VIDEO X SPESE GIACENZA ......
185400140609     C*---------------------------------------------------------------*
185500140609     C     SRF03         BEGSR
185600940309     C                   SETOFF                                       6157
185700940308     C     $F03          DOWEQ     '1'
185800991028     C* Imposto flag trovata tariffa di giacenza
185900991028     C                   MOVEL     'N'           WTRTAR            1
186000160304     C                   MOVEL     'EUR'         WGCDIV            3
186100160304     C                   MOVEL     'EUR'         WNWDIV            3
186200060410     c* verifica se richiamare routine per calcolo spese sui dati del prg 0
186300060410     c* se no spese oppure se motivo con azzeramento spese non esegue
186400060410     c                   if        gcpasg <>'N' and §2cnosp = *blank
186500940310     C   33$GIAC         IFEQ      '1'
186600940310     C                   EXSR      TGCVID
186700991001     C* Reperisco tariffa di giacenza in vigore alla data del giorno
186800991028     C* per il recupero della divisa di tassazione solo a meno che
186900991028     C* non vada in variazione della vecchia bolla. In tal caso
187000991028     C* tasso in base alla divisa di quest'ultima
187100160304     C**   *IN82         IFEQ      '0'
187200160304     C**   *IN83         ANDEQ     '0'
187300160304     ***                 EXSR      TARDIV
187400160304     C**                 ELSE
187500160304     c**                 if        ar6div <> ' '
187600160304     C**                 MOVEL     AR6DIV        WNWDIV
187700160304     C**                 END
187800160304     C**                 END
187900991001     C* Se vecifico che la tariffa in vigore ha divisa diversa da
188000991001     C* quella delle spese di giacenza appena calcolate effettuo
188100991001     C* conversione
188200160304     C**   WGCDIV        IFNE      WNWDIV
188300160304     C**                 EXSR      CNVSPG
188400160304     C**                 END
188500160304     c
188600991001     C                   END
188700060410     c                   endif
188800060410      *
188900940915     C                   WRITE     LG20Z03
189000940915     C                   EXFMT     LG20D07
189100940308     C                   EXSR      CTRV03
189200940308     C                   MOVEL     '0'           $OKF03
189300940308     C   19              MOVEL     '1'           $F03
189400940308     C   KL              MOVEL     '0'           $F03
189500940308     C   KF
189600940308     CANN19              MOVEL     '0'           $F03
189700940308     C   KF
189800940308     CANN19              MOVEL     '1'           $OKF03
189900940308F03  C                   END
190000940308     C     ENDF03        ENDSR
190100940308     C*---------------------------------------------------------------*
190200940308     C*   T G C V I D    DATI TARIFFA GIACENZA                        *
190300940308     C*---------------------------------------------------------------*
190400940308     C     TGCVID        BEGSR
190500940930     C                   MOVEL     *BLANK        WTCLI             1
190600941010     C                   MOVE      *ZEROS        KKSC
190700941010     C     §KSC          IFGT      0
190800941006     C                   MOVE      §KSC          KKSC
190900941010     C                   ENDIF
191000941006     C                   Z-ADD     KKSC          APPO              4 0
191100020225      *RICHIAMO PROGRAMMA PER REPERIMENTO INFORMAZIONI TARIFFA
191200020225     C                   CLEAR                   TR27DS
191300021118     C                   MOVE      ARBctr        I27ctb
191400020318     C                   MOVE      ARBTSP        I27TSP
191500020318     C                   MOVE      ARBLNA        I27LNA
191600020318     C                   MOVE      ARBLNP        I27LNP
191700020225     C                   MOVE      KKSC          I27CLI
191800160303     C**                 MOVEL     ARBAAS        I27DTA
191900160303     C**                 MOVE      ARBMGS        I27DTA
192000160303     C                   CALL      'TRUL27R1'
192100020225     C                   PARM                    TR27DS
192200941219     C* SE CLIENTE CODIFICATO CERCO LA SUA TARIFFA
192300150512     C                   CLEAR                   fnlv59ds
192400150512     C                   MOVE      ARBCTR        KCTR
192500150512     C                   MOVEL     ' '           ilv59tbo
192600150512     C                   MOVEL     KCI           ilv59kci
192700150512     C                   MOVEL     ARBTSP        ilv59tsp
192800150512     C                   MOVEL     'S'           ilv59CON
192900150512     C                   MOVEL     AMGDSP        ilv59dsp
193000150512     c                   eval      ilv59ta2='B'
193100150512     c                   eval      ilv59blo='S'
193200150512     C                   MOVEL     KKSC          ilv59KSC
193300000202     C                   MOVEL     KKSC          W0030             3 0
193400160303    1C     APPO          IFNE      8888
193500940930     C     APPO          ANDNE     9999
193600941010     C     APPO          ANDNE     *ZEROS
193700940930     C*  Cerco progressivo tariffa in vigore su TNTAM
193800150512     C                   MOVE      O27FIE        ilv59FIE
193900150512     C                   MOVEL     KCTR          ilv59CTR
194000150512     C                   CALL      'FNLV59R'
194100150512     C                   PARM                    fnlv59ds
194200941219     C*
194300160303    2C     olv59ERR      IFEQ      *BLANKS
194400150512     C     olv59tks      ANDEQ     'S'
194500991028     C                   MOVEL     'S'           WTRTAR
194600941219     C*  Se ho trovato tariffa in vigore mi posiziono sul file
194700941219     C*  delle tariffe giacenze con il proggressivo giusto
194800150512     C                   MOVE      olv59prg      KPRG
194900990730     C     KTGC          CHAIN     TITGC01L                           21
195000941219     C  N21              MOVEL     'S'           WTCLI
195100160303    2C                   ENDIF
195200160303    1C                   ENDIF
195300940930     C*
195400940930     C** SE NON HO TROVATO TARIFFA CLIENTE CERCO SU TAR. CARTELLO
195500160303    1C     WTCLI         IFEQ      *BLANK
195600160303     c* Carico la cartello :
195700160303     c*  - per 8888 col fie della bolla
195800160303     c*  - per tariffe cliente esistente col fie della tariffa
195900160303     c                   clear                   trulc7ds
196000160303    2C     olv59ERR      ifeq      *blanks
196100160303     C     olv59tks      andeq     'S'
196200160303     c                   z-add     1             indx              3 0
196300160303     c                   eval      indx=%lookup(ilv59ctr:ks_ct)
196400160303    3c                   if        indx>0 and %editc(olv59prg:'X')=
196500160303     c                             ks_pr(indx)
196600160303     c                   eval      ic7tfie=ks_ie(indx)
196700160303   x3c                   else
196800160303     c                   eval      ic7tntw=o27ntw
196900160303    3c                   endif
197000160303     c
197100160303   x2c                   else
197200160303     c                   eval      ic7tntw=o27ntw
197300160303    2c                   endif
197400160303     c
197500160303     C                   MOVEL     AMGDSP        ic7dta
197600160303     C                   move      arbctr        Ic7ctb
197700160303     c                   call      'TRULC7R'
197800160303     c                   parm                    trulc7ds
197900160303     c
198000940930     C*  Mi posiziono sul file
198100940930     C*  delle tariffe giacenze con il proggressivo giusto
198200160303     C                   MOVE      Oc7CTRc       KCTR
198300160303     C                   MOVE      Oc7PRGc       KPRG
198400160303     C                   z-add     Oc7KSCc       KKSC
198500990730     C     KTGC          CHAIN     TITGC01L                           21
198600941219     C* SE NON HO TROVATO TARIFFA GIACENZA LO SEGNALO
198700941219     C     *IN21         IFEQ      '1'
198800941219     C                   SETON                                        57
198900941219     C                   GOTO      ENDTGC
199000941219     C                   ENDIF
199100940930     C                   ENDIF
199200940308     C*
199300940308     C     VDIG          IFEQ      *ZEROS
199400940308     C                   Z-ADD     VDTG          VDSD
199500940308     C                   ELSE
199600940308     C                   Z-ADD     VDIG          VDSD
199700940308     C                   END
199800941012     C     VDED          IFNE      *ZEROS
199900941012     C                   Z-ADD     VDED          VDSA
200000941012     C                   ELSE
200100941012     C     VDDM          IFNE      *ZEROS
200200940308     C                   Z-ADD     VDDM          VDSA
200300940325     C                   END
200400941012     C                   END
200500940308     C*
200600050701     C* CALC. le riaperture
200700050701     c                   exsr      srconta
200800050701     C* CALC. I GG. COMPLESSIVI DI SOSTA DELLE RIAPERTE
200900940308     C                   MOVE      *ZEROS        §GGRIA            3 0
201000050222     C     KGIP          SETLL     tigcp000
201100940930      *
201200940308     C     *IN21         DOUEQ     '1'
201300050316     C     KGIP          READE(e)  TIGCP000                               21
201400050316     c                   if        %error
201500070521     C     KGIP          READE(n)  TIGCP000
201600050316     c                   exsr      geslck
201700050318     C     KGIP          READE(e)  TIGCP000                               21
201800050316     c                   endif
201900940930     C*
202000940930     C  N21GCPFRG        IFGT      0
202100060410     C     GCPasg        andne     'N'
202200940930     C                   MOVE      *ZEROS        G08DAT
202300940930     C                   MOVE      *ZEROS        G08INV
202400940308     C* CALCOLO GG SOSTA PER OGNI RIAPERTURA
202500941006     C     GCPDUR        IFGT      *ZEROS
202600941006     C                   MOVE      GCPDUR        G08INV
202700940308     C                   ELSE
202800940930     C                   MOVEL     GCPAGC        G08INV
202900940930     C                   MOVE      GCPMGC        G08INV
203000940308     C                   END
203100941006     C                   MOVEL     '3'           G08ERR
203200940930     C                   CALL      'XSRDA8'
203300940930     C                   PARM                    WLBDA8
203400940930     C                   Z-ADD     G08TGI        §GGDA
203500940308     C*
203600941011     C* SE C'E' DATA ESEGUIB. CALCOLO CON QUELLA
203700941011     C     GCPDED        IFGT      *ZEROS
203800941011     C                   MOVE      GCPDED        G08INV
203900941011     C                   ELSE
204000940314     C     GCPDDM        IFNE      *ZEROS
204100940930     C                   MOVE      GCPDDM        G08INV
204200940314     C                   ELSE
204300940930     C                   MOVE      AMGUDT        G08INV
204400940314     C                   END
204500941011     C                   END
204600940930     C                   MOVEL     '3'           G08ERR
204700940930     C                   CALL      'XSRDA8'
204800940930     C                   PARM                    WLBDA8
204900940930     C                   Z-ADD     G08TGI        §GGA
205000940308     C*
205100940308     C     §GGA          SUB       §GGDA         §GGXX             3 0
205200940308     C                   ADD       1             §GGXX
205300940308     C                   ADD       §GGXX         §GGRIA
205400940930     C                   ENDIF
205500940930     C                   ENDDO
205600940930      *
205700940930      * IMPOSTO I CAMPI A VIDEO SOLO LA 1° VOLTA
205800940930      *
205900940930     C     SGD$          IFEQ      *BLANK
206000940308     C                   Z-ADD     TGCSGD        VSGD
206100940330     C* MOLTIPLICO SPESE DOSSIER    X NR. RIAPERTURE
206200940330     C     VSGD          MULT      §NRG          VSGD
206300940325     C                   MOVEL     '1'           SGD$
206400150512     C     olv59div      IFNE      *BLANK
206500150512     C                   MOVE      olv59div      V6CDIV
206600150512     C                   MOVE      olv59div      V6CRVA
206700150512     C                   MOVE      olv59div      V6CRTA
206800020318     C                   ENDIF
206900940930     C                   ENDIF
207000940930      * GG FRANCHIGIA
207100940930     C     SGF$          IFEQ      *BLANK
207200940930     C                   Z-ADD     TGCSGF        VSGF
207300940325     C                   MOVEL     '1'           SGF$
207400940930     C                   END
207500940930      * CALCOLO GG. SOSTA IMPOSTATI A VIDEO
207600060111     C                   EXSR      TGCCAL
207700940308     C*                    -----------
207800940308     C* SPESE RICONSEGNA
207900940930     C     SGR$          IFEQ      *BLANK
208000941228     C     ARBFIN        IFNE      'C'
208100110623     C     ARBFIN        andne     'T'
208200940308     C                   Z-ADD     TGCSGP        §SGR
208300940308     C                   ELSE
208400941006     C                   Z-ADD     TGCSGR        §SGR
208500940308     C                   END
208600940930     C* X NR. RIAPERTURE
208700050701     C     §SGR          MULT      d09spe        §SGR
208800940308     C* SPESE RICONS.AL QUINTALE + SPESE VARIE
208900940308     C     §SGR          MULT      §PESCA        §SGR
209000100715     C     §SGR          ADD       TGCSGV        VSGR
209100100715     c                   if        tgcfaf = '3' and
209200100715     c                             vggs <= vsgf
209300100715     c                   sub       tgcsgv        vsgr
209400100715     c                   endif
209500940325     C                   MOVEL     '1'           SGR$
209600060407     C                   ENDIF
209700940308     C     ENDTGC        ENDSR
209800050701     C*-----------------------------------------------------*
209900050701     c* conto quanti record di giacenza ho e quanti di questi sono motivi
210000050701     c* di aperture tecniche
210100050701     C*-----------------------------------------------------*
210200050701     C     srconta       BEGSR
210300050701     C*
210400050701     c                   clear                   fnlg09ds
210500050701     c                   eval      d09tla = 'R'
210600050701     c                   eval      d09AGC = §AGC
210700050701     c                   eval      d09FGC = §FGC
210800050701     c                   eval      d09NGC = §NGC
210900050701     c                   movel(p)  fnlg09ds      kpjbu
211000050701     C                   CALL      'FNLG09R'
211100050701     C                   PARM                    kpjba
211200050701     c                   movel(p)  kpjbu         fnlg09ds
211300050701     c     d09cnt        sub       d09ced        §nrg
211400060808      * verifico se motivvo di chiusura è da eliminare da motivi di riconsegna
211500060808      * solo se non l'ho precedentemente escluso come motivo di apertura PRG 0
211600060808     C                   movel     '2Z'          TBLCOD
211700060808     C                   movel     *blank        TBLKEY
211800060808     C                   movel     GCpcmc        TBLKEY
211900060808     C     KTAB          CHAIN     TABEL                              21
212000060808     C  N21              MOVEL     TBLUNI        Ds2z
212100060808     c                   if        §2znsr <> 'N' and §2nospr <> *blank
212200060808     c                   sub       1             d09spe
212300060808     c                   endif
212400050701     C*
212500050701     C                   ENDSR
212600940308     C/SPACE 3
212700991001     C*---------------------------------------------------------------*
212800991001     C*   T A R D I V    REPERISCO DIVISA TARIFFA IN VIGORE X SP.Giac *
212900991001     C*---------------------------------------------------------------*
213000160304     C     TARDIV        BEGSR
213100160304     c***
213200160304     C**Memorizzo divisa con cui ho calcolato le spese di giacenza
213300160304     C                   MOVEL     olv59div      WGCDIV            3
213400160304     C**Calcolo tariffa in vigore
213500160304     C                   MOVEL     *BLANK        WTCLI             1
213600160304     C                   MOVE      *ZEROS        KKSC
213700160304     C     §KSC          IFGT      0
213800160304     C                   MOVE      §KSC          KKSC
213900160304     C                   ENDIF
214000160304     C                   Z-ADD     KKSC          APPO              4 0
214100160304     C**SE CLIENTE CODIFICATO CERCO LA SUA TARIFFA
214200160304     C                   CLEAR                   fnlv59ds
214300160304     C                   MOVE      ARBCTR        KCTR
214400160304     C                   MOVEL     ' '           ilv59TBO
214500160304     C                   MOVEL     KCI           ilv59KCI
214600160304     C                   MOVEL     ARBTSP        ilv59TSP
214700150512     C                   MOVEL     'S'           ilv59CON
214800150512     c                   eval      ilv59ta2='B'
214900150512     c                   eval      ilv59blo='S'
215000150512     C                   MOVEL     AMGUDT        ilv59DSP
215100150512     C                   MOVEL     O27FIE        ilv59FIE
215200991001     C     APPO          IFNE      8888
215300991001     C     APPO          ANDNE     9999
215400991001     C     APPO          ANDNE     *ZEROS
215500991001     C*  Cerco progressivo tariffa in vigore su TNTAM
215600150512     C                   MOVEL     KCTR          ilv59CTR
215700150512     C                   MOVEL     KKSC          ilv59KSC
215800150512     C                   CALL      'FNLV59R'
215900150512     C                   PARM                    fnlv59ds
216000991001     C*
216100150512     C     olv59err      IFEQ      *BLANKS
216200150512     C     olv59tks      ANDEQ     'S'
216300991028     C                   MOVEL     'S'           WTRTAR
216400991001     C*  Se ho trovato tariffa in vigore mi posiziono sul file
216500991001     C*  delle tariffe giacenze con il proggressivo giusto
216600150512     C                   MOVE      olv59prg      KPRG
216700991001     C     KTGC          SETLL     TITGC01L                               21
216800991001     C   21              MOVEL     'S'           WTCLI
216900991001     C                   ENDIF
217000991001     C                   ENDIF
217100991001     C*
217200991001     C** SE NON HO TROVATO TARIFFA CLIENTE CERCO SU TAR. CARTELLO
217300991001     C     WTCLI         IFEQ      *BLANK
217400991001     C*  Mi posiziono sul file
217500991001     C*  delle tariffe giacenze con il proggressivo giusto
217600160304
217700160304     c* SE SERVE REPERIRE COL TRULC7R LA TARIFFA DI CARTELLO
217800160304     C****               MOVE      Oc7CTRc       KCTR
217900160304     C***+               MOVE      Oc7PRGc       KPRG
218000160304     C****               z-add     Oc7KSCc       KKSC
218100160304
218200991001     C     KTGC          SETLL     TITGC01L                               21
218300991001     C* SE trovata tariffa memorizzo divisa relativa altrimenti la
218400991001     C* imposto uguale a quella di giacenza
218500991001     C                   ENDIF
218600150512     C     olv59err      IFNE      '1'
218700150512     C                   MOVEL     olv59div      WNWDIV            3
218800991028     C                   ELSE
218900991028     C                   MOVEL     WGCDIV        WNWDIV
219000991028     C                   ENDIF
219100991001     C*
219200991001     C                   ENDSR
219300991001     C*---------------------------------------------------------------*
219400991001     C* CNVSPG: Procedo alla conversione delle spese di giacenza      *
219500991001     C*---------------------------------------------------------------*
219600991001     C     CNVSPG        BEGSR
219700991001     C*
219800991001     C* converto le spese di sosta
219900991001     C                   CLEAR                   YEURDS
220000991001     C                   MOVEL     WGCDIV        YECDVI
220100991001     C                   Z-ADD     VTSGS         YECIMI
220200991001     C                   MOVEL     WNWDIV        YECDVO
220300991001     C                   MOVEL     '3'           YECDCO
220400991001     C                   MOVEL     'H'           YECTAR
220500991001     C                   CALL      'YEURCO'
220600991001     C                   PARM                    YEURDS
220700991001     C                   Z-ADD     YECIMO        VTSGS
220800991001     C* converto le spese di riconsegna
220900991028     C     WNWDIV        IFNE      V6CDIV
221000991001     C                   CLEAR                   YEURDS
221100991001     C                   MOVEL     WGCDIV        YECDVI
221200991001     C                   Z-ADD     VSGR          YECIMI
221300991001     C                   MOVEL     WNWDIV        YECDVO
221400991001     C                   MOVEL     '3'           YECDCO
221500991001     C                   MOVEL     'H'           YECTAR
221600991001     C                   CALL      'YEURCO'
221700991001     C                   PARM                    YEURDS
221800991001     C                   Z-ADD     YECIMO        VSGR
221900991001     C* converto le spese di dossier
222000991001     C                   CLEAR                   YEURDS
222100991001     C                   MOVEL     WGCDIV        YECDVI
222200991001     C                   Z-ADD     VSGD          YECIMI
222300991028     C                   MOVEL     WNWDIV        YECDVO
222400991001     C                   MOVEL     '3'           YECDCO
222500991001     C                   MOVEL     'H'           YECTAR
222600991001     C                   CALL      'YEURCO'
222700991001     C                   PARM                    YEURDS
222800991001     C                   Z-ADD     YECIMO        VSGD
222900991001     C* converto le spese aggiuntive
223000991001     C                   CLEAR                   YEURDS
223100991001     C                   MOVEL     WGCDIV        YECDVI
223200991001     C                   Z-ADD     VSGA          YECIMI
223300991001     C                   MOVEL     WNWDIV        YECDVO
223400991001     C                   MOVEL     '3'           YECDCO
223500991001     C                   MOVEL     'H'           YECTAR
223600991001     C                   CALL      'YEURCO'
223700991001     C                   PARM                    YEURDS
223800991001     C                   Z-ADD     YECIMO        VSGA
223900991028     C                   END
224000991028     C* Se esistono provvigioni C/Assegno converto anche quelle
224100991028     C     VRCA          IFGT      0
224200991028     C     V6CRVA        ANDNE     WNWDIV
224300991028     C                   CLEAR                   YEURDS
224400991028     C                   MOVEL     V6CRVA        YECDVI
224500991028     C                   Z-ADD     VRCA          YECIMI
224600991028     C                   MOVEL     WNWDIV        YECDVO
224700991028     C                   MOVEL     '3'           YECDCO
224800991028     C                   MOVEL     'H'           YECTAR
224900991028     C                   CALL      'YEURCO'
225000991028     C                   PARM                    YEURDS
225100991028     C                   Z-ADD     YECIMO        VRCA
225200991028     C                   MOVEL     WNWDIV        V6CRVA
225300991028     C                   END
225400991001     C*
225500991028     C* Se esistono provvigioni C/Assegno converto anche quelle
225600991028     C     VRTR          IFGT      0
225700991028     C     V6CRTA        ANDNE     WNWDIV
225800991028     C                   CLEAR                   YEURDS
225900991028     C                   MOVEL     V6CRTA        YECDVI
226000991028     C                   Z-ADD     VRTR          YECIMI
226100991028     C                   MOVEL     WNWDIV        YECDVO
226200991028     C                   MOVEL     '3'           YECDCO
226300991028     C                   MOVEL     'H'           YECTAR
226400991028     C                   CALL      'YEURCO'
226500991028     C                   PARM                    YEURDS
226600991028     C                   Z-ADD     YECIMO        VRTR
226700991028     C                   MOVEL     WNWDIV        V6CRTA
226800991028     C                   END
226900991001     C                   MOVEL     WNWDIV        V6CDIV
227000991001     C*
227100991001     C                   ENDSR
227200940308     C*---------------------------------------------------------------*
227300940308     C*   C T R V 0 3    CALCOLO IMPORTI DA ADDEBITARE                *
227400940308     C*---------------------------------------------------------------*
227500940308     C     CTRV03        BEGSR
227600991028     C                   SETOFF                                       121314
227700991028     C                   SETOFF                                       195253
227800990716     C                   SETOFF                                       485154
227900990716     C                   SETOFF                                       5063
228000991028     C                   MOVEL     *BLANKS       $MSG
228100940308     C* CALCOLO SPESE GIACENZA
228200940308     C     *IN33         IFEQ      '1'
228300940309     C* NON HO TROVATO TARIFFA GIACENZA
228400940309     C   57              SETON                                        1950
228500940309     C   57              MOVEL     ERR(34)       $MSG
228600940309     C   57              GOTO      ENDCT3
228700990716     C* CONTROLLO SE IMMESSI DECIMALI PER DIVISA CHE NON LI GESTISCE
228800991129     C     VTSGS         IFGT      0
228900991129     C     VSGD          ORGT      0
229000991129     C     VSGA          ORGT      0
229100991129     C     VSGR          ORGT      0
229200991129     C                   Z-ADD     1             XF
229300990716     C     V6CDIV        LOOKUP    DIV(XF)                                21
229400991028     C* Se divisa inesistente do errore
229500991028     C     *IN21         IFEQ      '0'
229600991028     C                   SETON                                        1914
229700991028     C                   MOVEL     ERR(60)       $MSG
229800991028     C                   GOTO      ENDCT3
229900991028     C                   ENDIF
230000990716     C   21              MOVEL     DCV(XF)       DSCV
230100990716     C  N19§CVFDC        IFNE      'S'
230200990716     C                   MOVE      VTSGS         WDEC03            3 0
230300990716     C     WDEC03        IFNE      0
230400990716     C                   MOVEL     ERR(59)       $MSG
230500990716     C                   SETON                                        1950
230600990716     C                   GOTO      ENDCT3
230700990716     C                   ENDIF
230800990716     C                   MOVE      VSGR          WDEC03            3 0
230900990716     C     WDEC03        IFNE      0
231000990716     C                   MOVEL     ERR(59)       $MSG
231100990716     C                   SETON                                        1948
231200990716     C                   GOTO      ENDCT3
231300990716     C                   ENDIF
231400990716     C                   MOVE      VSGD          WDEC03            3 0
231500990716     C     WDEC03        IFNE      0
231600990716     C                   MOVEL     ERR(59)       $MSG
231700990716     C                   SETON                                        1951
231800990716     C                   GOTO      ENDCT3
231900990716     C                   ENDIF
232000990716     C                   MOVE      VSGA          WDEC03            3 0
232100990716     C     WDEC03        IFNE      0
232200990716     C                   MOVEL     ERR(59)       $MSG
232300990716     C                   SETON                                        1954
232400990716     C                   GOTO      ENDCT3
232500990716     C                   ENDIF
232600991129     C                   ENDIF
232700990716     C                   ENDIF
232800940308     C* CONTROLLO FORMALE   DATA   - DA
232900940930     C                   MOVE      VDSD          G08DAT
233000940930     C                   MOVE      *ZEROS        G08INV
233100940930     C                   MOVEL     *BLANK        G08ERR
233200940930     C                   CALL      'XSRDA8'
233300940930     C                   PARM                    WLBDA8
233400941006     C     G08ERR        COMP      '1'                                    21
233500940930     C  N21              MOVE      G08INV        AMGDSD
233600940308     C  N21AMGDSD        COMP      AMGDSP                               21
233700940308     C  N21AMGDSD        COMP      AMGUDT                             21
233800940308     C   21              SETON                                        5219
233900940309     C   21              MOVEL     ERR(30)       $MSG
234000940308     C* CONTROLLO FORMALE   DATA   -  A
234100940930     C                   MOVE      VDSA          G08DAT
234200940930     C                   MOVE      *ZEROS        G08INV
234300940930     C                   MOVEL     *BLANK        G08ERR
234400940930     C                   CALL      'XSRDA8'
234500940930     C                   PARM                    WLBDA8
234600941006     C     G08ERR        COMP      '1'                                    21
234700940930     C  N21              MOVE      G08INV        AMGDSA
234800940308     C  N21AMGDSA        COMP      AMGDSP                               21
234900940308     C  N21AMGDSA        COMP      AMGUDT                             21
235000940308     C   21              SETON                                        5319
235100940309     C   21              MOVEL     ERR(30)       $MSG
235200940308     C* CONTROLLO DA DATA A DATA CONGRUENZA
235300940325     C  N19              DO
235400940309     C     AMGDSD        COMP      AMGDSA                             21
235500940309     C   21              SETON                                        525319
235600940309     C   21              MOVEL     ERR(31)       $MSG
235700940309     C                   END
235800940308     C*
235900940308     C   19              GOTO      ENDCT3
236000990628     C                   Z-ADD     0             §TOTSP           11 3
236100940308     C                   MOVE      *ZEROS        VTSGS
236200940308     C                   EXSR      TGCCAL
236300991001     C* Riconverto le spese di sosta se necessario
236400991001     C     WGCDIV        IFNE      WNWDIV
236500991001     C                   CLEAR                   YEURDS
236600991001     C                   MOVEL     WGCDIV        YECDVI
236700991001     C                   Z-ADD     VTSGS         YECIMI
236800991001     C                   MOVEL     WNWDIV        YECDVO
236900991001     C                   MOVEL     '3'           YECDCO
237000991001     C                   MOVEL     'H'           YECTAR
237100991001     C                   CALL      'YEURCO'
237200991001     C                   PARM                    YEURDS
237300991001     C                   Z-ADD     YECIMO        VTSGS
237400991001     C                   END
237500940308     C     VTSGS         ADD       VSGR          §TOTSP
237600940308     C     VSGD          ADD       §TOTSP        §TOTSP
237700940308     C     VSGA          ADD       §TOTSP        §TOTSP
237800940308     C* METTO IN RI I GG SOSTA SE LI HANNO CAMBIATI
237900940308     C* SE CONFERMANO LA SECONDA VOLTA NON CONTROLLO
238000940308     C     *INKF         IFEQ      '1'
238100940308     C   61§TOTSP        IFEQ      *ZEROS
238200940308     C                   GOTO      ENDCT3
238300940308     C                   END
238400940308     C     *IN58         CABEQ     '1'           SPESE
238500940308     C                   END
238600940308     C  N19VGGS          IFNE      §GGSAV
238700940308     C                   SETON                                        5819
238800940308     C                   END
238900940308     C*
239000940308     C     SPESE         TAG
239100940308     C     §TOTSP        COMP      *ZEROS                               2121
239200940308     C   21              SETON                                        501961
239300940309     C   21              MOVEL     ERR(32)       $MSG
239400940308     C                   END
239500940308     C* CONTROLLO SE SI DEVE METTERE PROVV. C/ASS
239600991028     C     *IN35         IFEQ      '1'
239700991028     C     VRCA          IFEQ      *ZEROS
239800940308     C                   SETON                                        6319
239900940309     C                   MOVEL     ERR(33)       $MSG
240000940308     C                   END
240100991028     C                   Z-ADD     1             XF                3 0
240200991028     C     V6CRVA        LOOKUP    DIV(XF)                                21
240300991028     C* Se divisa inesistente do errore
240400991028     C     *IN21         IFEQ      '0'
240500991028     C                   SETON                                        1912
240600991028     C                   MOVEL     ERR(60)       $MSG
240700991028     C                   GOTO      ENDCT3
240800991028     C                   ENDIF
240900991028     C   21              MOVEL     DCV(XF)       DSCV
241000991028     C  N19§CVFDC        IFNE      'S'
241100991028     C                   MOVE      VRCA          WDEC03            3 0
241200991028     C     WDEC03        IFNE      0
241300991028     C                   MOVEL     ERR(59)       $MSG
241400991028     C                   SETON                                        121963
241500991028     C                   GOTO      ENDCT3
241600991028     C                   ENDIF
241700991028     C                   ENDIF
241800991028     C                   ENDIF
241900991028     C* CONTROLLO DIVISA RECUPERO TRASPORTO
242000991028     C     *IN36         IFEQ      '1'
242100991028     C                   Z-ADD     1             XF                3 0
242200991028     C     V6CRTA        LOOKUP    DIV(XF)                                21
242300991028     C* Se divisa inesistente do errore
242400991028     C     *IN21         IFEQ      '0'
242500991028     C                   SETON                                        1913
242600991028     C                   MOVEL     ERR(60)       $MSG
242700991028     C                   GOTO      ENDCT3
242800991028     C                   ENDIF
242900991028     C   21              MOVEL     DCV(XF)       DSCV
243000991028     C  N19§CVFDC        IFNE      'S'
243100991028     C                   MOVE      VRTR          WDEC03            3 0
243200991028     C     WDEC03        IFNE      0
243300991028     C                   MOVEL     ERR(59)       $MSG
243400991028     C                   SETON                                        1319
243500991028     C                   GOTO      ENDCT3
243600991028     C                   ENDIF
243700991028     C                   ENDIF
243800991028     C                   ENDIF
243900991028     C* Se controllo se le tre divise immesse sono congruenti
244000991028     C* fra loro
244100991028     C     *IN33         IFEQ      '1'
244200991028     C     *IN35         ANDEQ     '1'
244300991028     C     V6CDIV        ANDNE     V6CRVA
244400991028     C     *IN33         OREQ      '1'
244500991028     C     *IN36         ANDEQ     '1'
244600991028     C     V6CDIV        ANDNE     V6CRTA
244700991028     C     *IN35         OREQ      '1'
244800991028     C     *IN36         ANDEQ     '1'
244900991028     C     V6CRVA        ANDNE     V6CRTA
245000991028     C                   SETON                                        121314
245100991028     C                   SETON                                        19
245200991028     C                   MOVEL     ERR(61)       $MSG
245300991028     C                   END
245400991028     C*
245500940308     C     ENDCT3        ENDSR
245600940308     C/SPACE 3
245700940308     C*---------------------------------------------------------------*
245800940308     C*   T G C C A L    CALCOLO IMPORTI DA ADDEBITARE X SOSTA        *
245900940308     C*---------------------------------------------------------------*
246000940308     C     TGCCAL        BEGSR
246100050405     c                   eval      spearr = 1
246200940930      * GIORNI SOSTA DA CALCOLARE
246300940930     C     VGGS          IFEQ      *ZEROS
246400940930     C* CALCOLO GIORNI DI SOSTA  CON LE DATE IMPOSTATE A VIDEO
246500940325     C     VDSD          IFNE      *ZEROS
246600940325     C     VDSA          ANDNE     *ZEROS
246700940930     C                   MOVE      VDSD          G08DAT
246800940930     C                   MOVEL     *BLANK        G08ERR
246900940930     C                   CALL      'XSRDA8'
247000940930     C                   PARM                    WLBDA8
247100940930     C                   Z-ADD     G08TGI        §GGDA
247200940308     C*
247300940930     C                   MOVE      VDSA          G08DAT
247400940930     C                   MOVEL     *BLANK        G08ERR
247500940930     C                   CALL      'XSRDA8'
247600940930     C                   PARM                    WLBDA8
247700941006     C                   Z-ADD     G08TGI        §GGA
247800940308     C*
247900940308     C     §GGA          SUB       §GGDA         §GGSOS
248000940308     C                   ADD       1             §GGSOS
248100940308     C* SOMMO GG RIAPERTURE
248200940308     C                   ADD       §GGRIA        §GGSOS
248300940308     C     §GGSOS        IFGT      VSGF
248400940308     C                   Z-ADD     §GGSOS        §GGPAG
248500940308     C                   ELSE
248600940308     C                   Z-ADD     *ZEROS        §GGPAG
248700940308     C                   END
248800940308     C                   Z-ADD     §GGPAG        VGGS
248900940308     C* MI SALVO I GG DA PAGARE CALCOLATI X SEGNALARE SE A VIDEO LI HANNO
249000940308     C* MODIFICATI
249100940308     C                   Z-ADD     §GGPAG        §GGSAV
249200940325     C                   END
249300940930     C                   ELSE
249400940308     C*                    ----
249500940308     C     VGGS          IFGT      VSGF
249600940308     C                   Z-ADD     VGGS          §GGPAG
249700940308     C                   ELSE
249800940308     C                   Z-ADD     *ZEROS        §GGPAG
249900940930     C                   END
250000940930     C                   END
250100940930      *
250200940308     C                   Z-ADD     0             XP                1 0
250300990628     C                   MOVE      *ZEROS        SGG
250400940308     C                   MOVE      *ZEROS        STU
250500940308     C                   MOVE      *ZEROS        STT
250600940308     C*
250700940930     C     §GGPAG        IFGT      0
250800940308     C                   Z-ADD     §GGPAG        §GGPAR
250900940308     C                   DO        3             X
251000940308     C     §GGPAG        IFGT      TGS(X)
251100940308     C     §GGPAG        SUB       TGS(X)        §GGPAR
251200940308     C     X             IFEQ      1
251300940308     C                   Z-ADD     TGS(X)        SGG(X)
251400940308     C                   ELSE
251500940308     C     TGS(X)        SUB       TGS(XP)       SGG(X)
251600940308     C                   END
251700940308     C                   Z-ADD     TTS(X)        STU(X)
251800940308     C                   ELSE
251900940308     C                   Z-ADD     §GGPAR        SGG(X)
252000940308     C                   Z-ADD     TTS(X)        STU(X)
252100940308     C                   Z-ADD     0             §GGPAG
252200940308     C                   Z-ADD     0             §GGPAR
252300940308     C                   END
252400940308     C                   ADD       1             XP
252500940308     C                   END
252600940930     C                   ENDIF
252700060113      *  TARIFFA SENZA FRANCHIGIA
252800060113     C*  CALCOLO PESO TASSABILE
252900060113     C     TGCRPV        MULT      ARBVLF        §PESVO           13 3
253000060113     C     §PESVO        MULT      100           §PESVO
253100060113     C     §PESVO        IFGT      ARBPKF
253200060113     C     §PESVO        DIV       100           §PES§            13 3
253300060113     C                   ELSE
253400060113     C     ARBPKF        DIV       100           §PES§
253500060113     C                   END
253600060113     C     §PES§         ADD       0,99          §PESCA            6 0
253700060113     C* CALCOLO TOTALE SPESE SOSTA
253800060113     C     SGG           MULT      STU           STT
253900060113     C     STT           MULT      §PESCA        STT
254000060113     C                   XFOOT     STT           VTSGS
254100060113     C* MINIMO TASSABILE SPESE SOSTA  SE SUPERO  GG DI FRANCHIGIA
254200060113     C     VTSGS         IFLT      TGCSTM
254300060113     C     VGGS          ANDGT     VSGF
254400060113     C                   Z-ADD     TGCSTM        VTSGS
254500060113     C                   END
254600940930      *  TARIFFA CON FRANCHIGIA DA APPLICARE A TUTTE LE SPESE
254700940930     C     TGCFAF        IFEQ      '2'
254800940930     C     VGGS          ANDLE     VSGF
254900060113     C     sgfaf$        ANDLE     *blank
255000940930     C                   MOVE      *ZEROS        VTSGS
255100940930     C                   MOVE      *ZEROS        VSGR
255200060113     C                   MOVE      '1'           SGR$
255300940930     C                   MOVE      *ZEROS        VSGD
255400940930     C                   MOVE      *ZEROS        VSGA
255500940930     C                   ELSE
255600060109      *TARIFFA CON FRANCHIGIA DA APPLICARE A TUTTE LE SPESE tranne riconsegna
255700060109     C     TGCFAF        IFEQ      '3'
255800060109     C     VGGS          ANDLE     VSGF
255900060113     C     sgfaf$        ANDLE     *blank
256000060109     C                   MOVE      *ZEROS        VTSGS
256100060113     C*    ARBFIN        IFNE      'C'
256200110623     C*    ARBFIN        andne     'T'
256300060113     C*                  Z-ADD     TGCSGP        vsgr
256400060113     C*                  ELSE
256500060113     C*                  Z-ADD     TGCSGR        vsgr
256600060113     C*                  END
256700060109     C                   MOVE      *ZEROS        VSGD
256800060109     C                   MOVE      *ZEROS        VSGA
256900060109     C                   ELSE
257000950120      *  TARIFFA CON FRANCHIGIA DA APPLICARE SOLO ALLE SPESE SOSTA
257100950120     C     TGCFAF        IFEQ      '1'
257200060113     C     sgfaf$        ANDLE     *blank
257300950120     C     VGGS          ANDLE     VSGF
257400950120     C                   MOVE      *ZEROS        VTSGS
257500060113     C*    ARBFIN        IFNE      'C'
257600110623     C*    ARBFIN        andne     'T'
257700060113     C*                  Z-ADD     TGCSGP        vsgr
257800060113     C*                  ELSE
257900060113     C*                  Z-ADD     TGCSGR        vsgr
258000060113     C*                  END
258100060113     C*mm                ELSE
258200950120     C                   ENDIF
258300060109     C                   ENDIF
258400940930     C                   ENDIF
258500060113     C                   move      '1'           sgfaf$
258600940308     C     ENDCAL        ENDSR
258700940308     C*---------------------------------------------------------------*
258800940308     C*   P G A R B K  RICH. PGM VARIAZ. BOLLA   C/ASS O COD.BOLLA    *
258900940308     C*---------------------------------------------------------------*
259000940308     C     PGARBK        BEGSR
259100940308     C     §§KCVB        IFNE      *BLANK
259200941215     C                   MOVEL     ' '           D48TLA
259300941215     C                   MOVEL     FILARR        D48FGS
259400941214     C                   MOVE      ARBAAS        D48AAS
259500941214     C                   MOVE      ARBLNP        D48LNP
259600941214     C                   MOVE      ARBNRS        D48NRS
259700941214     C                   MOVE      ARBNSP        D48NSP
259800941214     C                   MOVEL     ARBCBO        D48CBO
259900941214     C                   MOVEL     §§KCVB        D48CVB
260000941214     C                   MOVEL     'K'           D48TRC
260100941214     C                   MOVEL     'S'           D48FFR
260200941214     C                   MOVEL     ' '           D48ERR
260300941214     C                   MOVEL     *BLANK        D48MSG
260400941222     C                   MOVEL     'A'           D48TBO
260500941215     C     §BKCAS        IFGT      *ZEROS
260600941219     C                   MOVE      VTIC          §BKTIC
260700940308     C                   ELSE
260800940308     C                   MOVE      *BLANK        §BKTIC
260900940308     C                   END
261000040301     c                   z-add     arbQFT        §BTQFT
261100040301     c                   z-add     arbVMD        §BTVMD
261200040301     c                   MOVE      arbVAD        §BTVAD
261300040301     c                   z-add     arbIAS        §BTIAS
261400040301     c                   MOVE      arbVAS        §BTVAS
261500941219     C                   MOVEL     AR9GCA        §BKGCA
261600941219     C                   MOVEL     ARBGGA        §BKGGA
261700941219     C                   MOVEL     ARBGMA        §BKGMA
261800941219     C                   MOVEL     ARBGVA        §BKGVA
261900960205     C*§ ABBLENCO 2°BYTE PARTIC. VARIE
262000960205     C                   MOVE      ' '           §BKGVA
262100941214     C                   MOVEL     DSLR48        KPJBU
262200941214     C                   CALL      'FNLR48R'
262300940308     C                   PARM                    KPJBA
262400940308     C                   PARM                    DSARBD
262500940308     C                   PARM                    DSARBK
262600991001     C                   PARM                    DARBT
262700940308     C                   PARM                    DSARBG
262800030715     C                   PARM                    trul90ds
262900941214     C                   MOVEL     KPJBU         DSLR48
263000941214     C     D48ERR        IFEQ      *BLANK
263100941214     C                   CLEAR                   DSLR48
263200940308     C                   END
263300940308     C                   END
263400940308     C                   ENDSR
263500940308     C*---------------------------------------------------------------*
263600940308     C*   P G A R B I  RICH. PGM VARIAZ. BOLLA   INDIR. DESTINATARIO  *
263700940308     C*---------------------------------------------------------------*
263800940308     C     PGARBI        BEGSR
263900941215     C                   MOVEL     ' '           D48TLA
264000941215     C                   MOVEL     FILARR        D48FGS
264100941215     C                   MOVE      ARBAAS        D48AAS
264200941215     C                   MOVE      ARBLNP        D48LNP
264300941215     C                   MOVE      ARBNRS        D48NRS
264400941215     C                   MOVE      ARBNSP        D48NSP
264500941215     C                   MOVEL     ARBCBO        D48CBO
264600140729     C***                MOVEL     'I7'          D48CVB
264700140729     c* ES: 29/07/14 reimposto la causale I0 e solo in caso di disposiz
264800140729     c*     automatiche impostiamo I7
264900140729     C                   MOVEL     'I0'          D48CVB
265000941215     C                   MOVEL     'D'           D48TRC
265100941215     C                   MOVEL     ' '           D48ERR
265200941215     C                   MOVEL     *BLANK        D48MSG
265300941222     C                   MOVEL     'A'           D48TBO
265400941215     C*
265500060317     c                   if        flgdis = 'F'
265600060317     C                   MOVEL     'S'           §BDFFD
265700140729     C                   MOVEL     'I7'          D48CVB
265800060317     c                   else
265900060317     c                   move      arbffd        §bdffd
266000060317     c                   endif
266100940308     C                   MOVEL     ARBCPI        §BDCPI
266200941215     C                   MOVE      ARBPKF        §BDPKF
266300941215     C                   MOVE      ARBVLF        §BDVLF
266400940308     C                   MOVE      ARBFIN        §BDFIN
266500940308     C                   MOVEL     VRSD          §BDRSD
266600941223     C                   MOVEL     VRD2          §BDRD2
266700940308     C                   MOVEL     VIND          §BDIND
266800940308     C                   MOVEL     VCAD          §BDCAD
266900940308     C                   MOVEL     VLOD          §BDLOD
267000940308     C                   MOVEL     VPRD          §BDPRD
267100950206     C                   MOVEL     VNZD          §BDNZD
267200060314     C                   MOVEL     Vref          §BDref
267300060314     C                   MOVEL     Vteld         §BDtel
267400941215     C                   MOVE      ARBFIN        §BDFIN
267500040301     c                   z-add     arbQFT        §BTQFT
267600040301     c                   z-add     arbVMD        §BTVMD
267700040301     c                   MOVE      arbVAD        §BTVAD
267800040301     c                   z-add     arbIAS        §BTIAS
267900040301     c                   MOVE      arbVAS        §BTVAS
268000941215     C*
268100941215     C                   MOVEL     DSLR48        KPJBU
268200941215     C                   CALL      'FNLR48R'
268300940308     C                   PARM                    KPJBA
268400940308     C                   PARM                    DSARBD
268500940308     C                   PARM                    DSARBK
268600991001     C                   PARM                    DARBT
268700940308     C                   PARM                    DSARBG
268800030715     C                   PARM                    trul90ds
268900941215     C                   MOVEL     KPJBU         DSLR48
269000941215     C     D48ERR        IFEQ      *BLANK
269100941215     C                   CLEAR                   DSLR48
269200941215     C                   END
269300940308     C                   ENDSR
269400940308     C*---------------------------------------------------------------*
269500940308     C*   P G A R B T  RICH. PGM VARIAZ. BOLLA  TASSAZ. 1° BOLLA      *
269600940308     C*---------------------------------------------------------------*
269700940308     C     PGARBT        BEGSR
269800941215     C                   MOVEL     ' '           D48TLA
269900941215     C                   MOVEL     FILARR        D48FGS
270000941215     C                   MOVE      ARBAAS        D48AAS
270100941215     C                   MOVE      ARBLNP        D48LNP
270200941215     C                   MOVE      ARBNRS        D48NRS
270300941215     C                   MOVE      ARBNSP        D48NSP
270400941215     C                   MOVEL     ARBCBO        D48CBO
270500941215     C                   MOVEL     §§TCVB        D48CVB
270600941215     C                   MOVEL     'T'           D48TRC
270700941215     C                   MOVEL     'S'           D48FFR
270800941215     C                   MOVEL     ' '           D48ERR
270900941215     C                   MOVEL     *BLANK        D48MSG
271000941222     C                   MOVEL     'A'           D48TBO
271100941215     C*
271200940308     C     §BTKSC        IFEQ      0
271300940308     C                   MOVE      ARBKSC        §BTKSC
271400991203     C     AR6KSC        IFNE      0
271500991001     C     AR6TRC        ANDEQ     '1'
271600991001     C                   MOVE      AR6KSC        §BTKSC
271700940308     C                   END
271800991001     C                   END
271900991001     C                   MOVE      '1'           §BTTRC
272000991001     C                   MOVE      ARBCTR        §BTCTR
272100941006     C                   MOVE      AR6POR        §BTPOR
272200991001     C                   MOVE      AR6DIV        §BTDIV
272300040301     c                   z-add     arbQFT        §BTQFT
272400040301     c                   z-add     arbVMD        §BTVMD
272500040301     c                   MOVE      arbVAD        §BTVAD
272600040301     c                   z-add     arbIAS        §BTIAS
272700040301     c                   MOVE      arbVAS        §BTVAS
272800000105     C* SE IL CODICE VARIAZIONE E' T0 (NON ESISTE LA BOLLA)
272900000105     C* IMPOSTO LA DIVISA DALLA GIACENZA
273000000105     C     §CDVAR        IFEQ      'T0'
273100000105     C     AR6DIV        ANDEQ     *BLANKS
273200000105     C                   MOVE      V6CDIV        §BTDIV
273300000105     C                   END
273400940308     C                   MOVE      *ZEROS        §BTIMV
273500991001     C                   MOVE      AR6CEI        §BTCEI
273600941216     C* SE HO SPESE GIACENZA AGGIORNO LA VARIA I
273700941216     C                   MOVEL     §$2VRI        §SIGLA            1
273800941216     C                   MOVEL     VSV           WSV
273900941216      * CERCO L'INDICE SU CUI LAVORARE
274000941216     C                   EXSR      CHKVAR
274100950726     C     §TOTSP        IFGT      0
274200941216     C                   Z-ADD     §TOTSP        VVA(XS)
274300941216     C                   MOVEL     §$2VRI        VSV(XS)
274400950726     C                   ELSE
274500950726     C     VSV(XS)       IFEQ      §$2VRI
274600950726     C                   MOVEL     ' '           VSV(XS)
274700950726     C                   Z-ADD     0             VVA(XS)
274800950726     C                   END
274900950726     C                   END
275000991001     C* Tolgo varia Iva ...
275100991001     C                   Z-ADD     1             XS
275200991001     C     §$2BOL        LOOKUP    VSV(XS)                                20
275300991001     C     *IN20         IFEQ      '1'
275400991001     C                   MOVEL     ' '           VSV(XS)
275500991001     C                   Z-ADD     0             VVA(XS)
275600991001     C                   END
275700991001     C* ... e bollo sono state impostate
275800991001     C                   Z-ADD     1             XS
275900991001     C     §$2IVA        LOOKUP    VSV(XS)                                20
276000991001     C     *IN20         IFEQ      '1'
276100991001     C                   MOVEL     ' '           VSV(XS)
276200991001     C                   Z-ADD     0             VVA(XS)
276300991001     C                   END
276400941215     C*
276500941215     C                   MOVEL     DSLR48        KPJBU
276600941215     C                   CALL      'FNLR48R'
276700940308     C                   PARM                    KPJBA
276800940308     C                   PARM                    DSARBD
276900940308     C                   PARM                    DSARBK
277000991001     C                   PARM                    DARBT
277100940308     C                   PARM                    DSARBG
277200030715     C                   PARM                    trul90ds
277300941215     C*
277400941215     C                   MOVEL     KPJBU         DSLR48
277500941215     C     D48ERR        IFEQ      *BLANK
277600941215     C                   CLEAR                   DSLR48
277700941215     C                   END
277800940308     C                   ENDSR
277900940308     C*---------------------------------------------------------------*
278000940308     C*   P G A R 2 T  RICH. PGM VARIAZ. BOLLA  TASSAZ. 2° BOLLA      *
278100940308     C*---------------------------------------------------------------*
278200940308     C     PGAR2T        BEGSR
278300941215     C                   MOVEL     ' '           D48TLA
278400941215     C                   MOVEL     FILARR        D48FGS
278500941215     C                   MOVE      ARBAAS        D48AAS
278600941215     C                   MOVE      ARBLNP        D48LNP
278700941215     C                   MOVE      ARBNRS        D48NRS
278800941215     C                   MOVE      ARBNSP        D48NSP
278900941215     C                   MOVEL     ARBCBO        D48CBO
279000991001     C                   MOVEL     §§TCVB        D48CVB
279100991001     C                   MOVEL     '2'           D48TRC
279200941215     C                   MOVEL     'S'           D48FFR
279300941215     C                   MOVEL     ' '           D48ERR
279400941215     C                   MOVEL     *BLANK        D48MSG
279500941222     C                   MOVEL     'A'           D48TBO
279600991001     C                   MOVE      AR6CTR        §BTCTR
279700991001     C                   MOVE      AR6POR        §BTPOR
279800991001     C                   MOVE      AR6DIV        §BTDIV
279900040301     c                   z-add     arbQFT        §BTQFT
280000040301     c                   z-add     arbVMD        §BTVMD
280100040301     c                   MOVE      arbVAD        §BTVAD
280200040301     c                   z-add     arbIAS        §BTIAS
280300040301     c                   MOVE      arbVAS        §BTVAS
280400000105     C* SE IL CODICE VARIAZIONE E' T0 (NON ESISTE LA BOLLA)
280500000105     C* IMPOSTO LA DIVISA DALLA GIACENZA
280600000105     C     §CDVAR        IFEQ      'T0'
280700000105     C     AR6DIV        ANDEQ     *BLANKS
280800000105     C                   MOVE      V6CDIV        §BTDIV
280900000105     C                   END
281000991001     C                   MOVE      *ZEROS        §BTIMV
281100991001     C                   MOVE      AR6CEI        §BTCEI
281200941215     C*
281300991001     C     §BTKSC        IFEQ      0
281400991001     C                   MOVE      AR6KSC        §BTKSC
281500940308     C                   END
281600991001     C                   MOVE      '2'           §BTTRC
281700991001     C* Tolgo varia Iva ...
281800991001     C                   Z-ADD     1             XS
281900991001     C     §$2BOL        LOOKUP    VSV(XS)                                20
282000991001     C     *IN20         IFEQ      '1'
282100991001     C                   MOVEL     ' '           VSV(XS)
282200991001     C                   Z-ADD     0             VVA(XS)
282300991001     C                   END
282400991001     C* ... e bollo sono state impostate
282500991001     C                   Z-ADD     1             XS
282600991001     C     §$2IVA        LOOKUP    VSV(XS)                                20
282700991001     C     *IN20         IFEQ      '1'
282800991001     C                   MOVEL     ' '           VSV(XS)
282900991001     C                   Z-ADD     0             VVA(XS)
283000991001     C                   END
283100941215     C*
283200941215     C                   MOVEL     DSLR48        KPJBU
283300941215     C                   CALL      'FNLR48R'
283400940308     C                   PARM                    KPJBA
283500940308     C                   PARM                    DSARBD
283600940308     C                   PARM                    DSARBK
283700991001     C                   PARM                    DARBT
283800940308     C                   PARM                    DSARBG
283900030715     C                   PARM                    trul90ds
284000941215     C*
284100021009     C                   MOVEL     KPJBU         DSLR48
284200941215     C     D48ERR        IFEQ      *BLANK
284300941215     C                   CLEAR                   DSLR48
284400941215     C                   END
284500940308     C                   ENDSR
284600940309     C*?$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
284700940308     C*?    R O U T I N E     INIZIO
284800940309     C*?$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
284900940308     C*---------------------------------------------------------------*
285000940308     C*   S R G I A C  - RICHIAMATO DA GIACENZE                       *
285100940308     C*---------------------------------------------------------------*
285200940308     C     SRGIAC        BEGSR
285300050316     C     KGIA          CHAIN(e)  tigcp02l                           40
285400050316     c                   if        %error
285500070521     C     KGIA          CHAIN(n)  tigcp02l
285600050316     c                   exsr      geslck
285700050318     C     KGIA          CHAIN(e)  tigcp02l                           40
285800050316     c                   endif
285900940308     C   40              MOVEL     '1'           $FINE
286000940308     C   40              GOTO      ENDGIA
286100940308     C*
286200050222     C     GCPriap       COMP      'R'                                    42
286300941003     C* PRENDO DATI DELLA BOLLA  - FLARB01L
286400050222     C                   MOVE      GCPAASa       KAAS
286500050222     C                   MOVE      GCPLNPa       KLNP
286600050222     C                   MOVE      GCPNRSa       KNRS
286700050222     C                   MOVE      GCPNSPa       KNSP
286800170405      * verifico se la fase della giacenza è ancora congrua a ricevere
286900170405      * disposizioni
287000170418     C***  gcpfas        IFge      35
287100170418      *
287200170418     C***  36= immesse in partenza (quindi da considerare)
287300170418     c                   if        gcpFAS <> 36
287400170418      *
287500170418     C***  ma se >= di 35 deve NON FARE ESEGUIRE
287600170418     c                   if        gcpFAS >= 35
287700170405     C                   MOVEL     '1'           $FINE
287800170405     C                   GOTO      ENDGIA
287900170405     C                   END
288000170418     c                   end
288100170405      *
288200940308     C                   EXSR      CHKARB
288300940308     C     $ERR          IFEQ      '1'
288400940308     C                   MOVEL     '1'           $FINE
288500940308     C                   GOTO      ENDGIA
288600940308     C                   END
288700940308     C     ENDGIA        ENDSR
288800940308     C/SPACE 3
288900940308     C*---------------------------------------------------------------*
289000940308     C*   S R D I R O  - RICHIAMATO DA DIROTTAM/CAMBI PORTO           *
289100940308     C*---------------------------------------------------------------*
289200940308     C     SRDIRO        BEGSR
289300940928     C                   MOVE      ANNO          VAAS
289400940308     C                   MOVE      *ZEROS        VLNP
289500940308     C                   MOVE      *ZEROS        VNRS
289600940308     C                   MOVE      *ZEROS        VNSP
289700950104     C                   SETOFF                                       94
289800940928      * NR. SPEDIZIONE PASSATA : NON EMETTO VIDEATA RICHIESTA NR.SPED.
289900941216     C     D82NSP        IFNE      *BLANK
290000941216     C                   MOVE      D82AAS        VAAS
290100941216     C                   MOVE      D82LNP        VLNP
290200941216     C                   MOVE      D82NRS        VNRS
290300941216     C                   MOVE      D82NSP        VNSP
290400141113     c                   if        d82dis <> 'D'
290500950104     C                   SETON                                        94
290600160504     c                   exsr      chkevb
290700141113     c                   endif
290800940928     C                   GOTO      NOFD01
290900940928     C                   ENDIF
291000940308     C*
291100940308     C     FG2010        TAG
291200141006     c                   if        d82nodsp <> *blank
291300141007     c                   if        *in43
291400141006     c                   movel     $msg          d20errmo
291500141006     c                   movel     'E'           d20erro
291600141006     c                   return
291700141006     c                   endif
291800141006     c                   else
291900940915     C                   EXFMT     LG20D01
292000141006     c                   endif
292100940308     C                   MOVEL     *BLANK        $MSG
292200940308     C                   SETOFF                                       434419
292300950131     C*
292400950131     C                   MOVEL     VAAS          WAA2              2 0
292500950131     C     WAA2          IFEQ      0
292600950131     C     VAAS          IFGT      60
292700950131     C                   ADD       1900          VAAS
292800950131     C                   ELSE
292900950131     C                   ADD       2000          VAAS
293000950131     C                   END
293100950131     C                   END
293200940308     C*
293300940308     C*  OPERAZIONI DA EFFETTUARE SE FILIALE DI ARRIVO DI PRIMO LIVELLO
293400940308     C     *IN01         IFEQ      '1'
293500940308     C*  CONTROLLO SE HO RICHIESTO MODIFICA FILIALE
293600940308     C     *INKS         IFEQ      '1'
293700940308     C                   SETON                                        02
293800940308     C                   GOTO      FG2010
293900940308     C                   END
294000940308     C*
294100940308     C*  CONTROLLO SE HO MODIFICATO LA FILIALE
294200940308     C     *IN03         IFEQ      '1'
294300940308     C* AGGANCIO LA £6 E RECUPERO LE LINEE A LORO ASSOCIATE
294400940308     C*
294500021009     c                   eval      kpjbus = kpjbu
294600020509     C                   CLEAR                   TR06DS
294700020509     C                   MOVEL     '£6'          D06COD
294800020509     C                   MOVEL     filarr        D06KEY
294900020509     C                   MOVEL     TR06DS        KPJBU
295000020509     C                   CALL      'TRUL06R'
295100020509     C                   PARM                    KPJBA
295200020509     C                   MOVEL     KPJBU         TR06DS
295300020509     C                   MOVEA     LIN           l£6
295400021009     c                   eval      kpjbu = kpjbus
295500020226     C                   END
295600020226     C                   END
295700940308     C*  INTERROG. BOLLE
295800940308     C   KG              DO
295900940308     C                   MOVEL     KPJBU         §KPJBU
296000070622     C                   CLEAR                   param1
296100070622     C                   MOVEL     '2'           Pa1FLG
296200070622     C                   MOVEL     param1        KPJBU
296300941006     C                   CALL      'FNLR36R'
296400940308     C                   PARM                    KPJBA
296500070622     C                   MOVEL     KPJBU         param1
296600070622     C     Pa1AAS        IFNE      *ZEROS
296700070622     C                   MOVEL     Pa1AAS        VAAS
296800070622     C                   MOVEL     Pa1LNP        VLNP
296900070622     C                   MOVEL     Pa1NRS        VNRS
297000070622     C                   MOVEL     Pa1NSP        VNSP
297100940308     C                   END
297200940308     C                   MOVEL     §KPJBU        KPJBU
297300940308     C                   GOTO      FG2010
297400940308     C                   END
297500940308     C   KC              MOVEL     '1'           $FINE
297600940308     C   KC              GOTO      ENDDIR
297700940928     C     NOFD01        TAG
297800941216     C                   MOVE      VAAS          KAAS
297900941216     C                   MOVE      VLNP          KLNP
298000941216     C                   MOVE      VNRS          KNRS
298100941216     C                   MOVE      VNSP          KNSP
298200940308     C                   EXSR      CHKARB
298300940308     C     $ERR          IFEQ      '1'
298400940308     C                   SETON                                        19
298500940308     C                   MOVEL     ERR(1)        $MSG
298600940308     C                   GOTO      FG2010
298700940308     C                   END
298800000120     C     $ERR          IFEQ      '2'
298900000120     C                   SETON                                        19
299000000120     C                   MOVEL     ERR(62)       $MSG
299100000120     C                   GOTO      FG2010
299200000120     C                   END
299300000421      *CONTROLLO PER FILIALE POSTE  ERRORE BOLLE NON TRATTABILI
299400000417     C     $ERR          IFEQ      '3'
299500000417     C                   SETON                                        19
299600000417     C                   MOVEL     ERR(63)       $MSG
299700000417     C                   GOTO      FG2010
299800000417     C                   END
299900940928     C* E' UNA FIGLIA: NO DIROTT./CAMBIO PORTO
300000940928     C     KBOL          CHAIN     FNLBL01L                           43
300100940308     C     *IN43         IFEQ      '0'
300200940308     C     LBLLAN        COMP      LBLLAP                                 43
300300940308     C   43              MOVEL     ERR(2)        $MSG
300400940308     C   43              SETON                                        19
300500940308     C   19              GOTO      FG2010
300600940308     C                   ELSE
300700940308     C                   SETOFF                                       43
300800940308     C                   END
300900940308     C* E' UNA MAMMA : ERRORE
301000940928     C     KBOL          SETLL     FNLBL02L                               43
301100940308     C   43              SETON                                        19
301200940308     C   19              MOVEL     ERR(2)        $MSG
301300940308     C   19              GOTO      FG2010
301400940308     C* BOLLA SU ALTRA DISTINTA
301500940308     C                   Z-ADD     0             PROGR
301600940308     C     ARBNDC        COMP      *ZEROS                             4343
301700940308     C   43              SETON                                        19
301800940308     C   19              MOVEL     ERR(3)        $MSG
301900940308     C   19              GOTO      FG2010
302000940308     C* BOLLA  GIACENTE (O.K. SE GIAC. ANNULLATA)
302100050222     C     KBOLG         CHAIN     tigcp21L                           21
302200940308     C  N21GCPATB        COMP      '*'                                4343
302300990716     C   43
302400050418     CANN21GCPFAS        COMP      40                                   4343
302500990716     C   43              SETON                                        19
302600940308     C   19              MOVEL     ERR(4)        $MSG
302700940308     C   19              GOTO      FG2010
302800940308     C* LINEA ARRIVO NON IN GESTIONE
302900940308     C     ARBLNA        LOOKUP    L£6                                    21
303000940308     C  N21              SETON                                        1944
303100940308     C   19              MOVEL     ERR(5)        $MSG
303200940308     C   19              GOTO      FG2010
303300940928     C* BOLLA GIA CONSEGNATA
303400940928     C     ARBDCM        COMP      *ZEROS                             4343
303500940928     C   43              SETON                                        19
303600940928     C   19              MOVEL     ERR(6)        $MSG
303700940928     C   19              GOTO      FG2010
303800940308     C* BOLLA CONSEGNATA PARZIALMENTE
303900940308     C     ARBDCP        COMP      *ZEROS                             4343
304000940308     C   43              MOVEL     ERR(6)        $MSG
304100940308     C   43              SETON                                        19
304200940308     C   19              GOTO      FG2010
304300940308     C* BOLLA BLOCCATA
304400940308     C     ARBFBC        COMP      *BLANK                             4343
304500940308     C   43              MOVEL     ERR(7)        $MSG
304600940308     C   43              SETON                                        19
304700940308     C   19              GOTO      FG2010
304800110909     C* BOLLA con esiti PDA
304900110909     C                   EXSR      SRCTRPDA
305000110909     C   43              MOVEL     ERR(67)       $MSG
305100110909     C   43              SETON                                        19
305200110909     C   19              GOTO      FG2010
305300141103     C* BOLLA con disposizioni di consegna/dirottamento se dirottamento errore
305400141103      * esegue solo se richiamato da menu
305500141103     C                   IF        $giac = '0' and d82nsp = *blank
305600141103     C                             and %parms = 1
305700141103     c                   clear                   fnlry09ds2
305800141103     c                   eval      ilry09tdis= ' '
305900141103     c                   eval      ilry09tch = 'C'
306000141103     c                   eval      ilry09aas = arbaas
306100141103     c                   eval      ilry09lnp = arblnp
306200141103     c                   eval      ilry09nrs = arbnrs
306300141103     c                   eval      ilry09nsp = arbnsp
306400141103     c                   eval      ilry09fgs = arblna
306500141103     c                   movel     fnlry09ds2    kpjbu
306600141103     c                   call      'FNLRY09R'
306700141103     c                   parm                    kpjba
306800141103     c                   movel     kpjbu         fnlry09ds2
306900141103     c                   if        olry09err = *blank
307000141103     c                   select
307100141103     c                   when      olry09esdi = '1'
307200141103     C                   MOVEL     ERR(68)       $MSG
307300141103     C                   SETON                                        1943
307400141103     C   19              GOTO      FG2010
307500141103      *  altro
307600141103     c                   when      olry09esal = '1'
307700141103     c                   eval      ilry09tdis= 'E'
307800141103     c                   eval      ilry09tch = 'E'
307900141103     c                   movel     fnlry09ds2    kpjbu
308000141103     c                   call      'FNLRY09R'
308100141103     c                   parm                    kpjba
308200141103     c                   movel     kpjbu         fnlry09ds2
308300141103     c                   if        olry09esal = '1'
308400151204     C                   MOVEL     ERR(69)       $MSG
308500141105     C                   SETON                                        1943
308600141105     C   19              GOTO      FG2010
308700141103     c                   endif
308800141103     c                   endsl
308900141103     c                   endif
309000141103     C                   ENDIF
309100940308      * TUTTO OK
309200950104     C* SE DISPOSIZIONI PASSATE DA PGM CHIAMANTE
309300950104     C     D82NSP        IFNE      *BLANK
309400950104     C     D82DIS        ANDNE     *BLANK
309500950104     C                   MOVEL     D82DIS        VFRI
309600950104     C                   ELSE
309700940308     C                   MOVEL     *BLANK        VFRI
309800950104     C                   END
309900940308     C                   MOVEL     *BLANK        VNPC
310000940308     C                   MOVEL     'N'           VVCS
310100940308     C                   MOVE      *ZEROS        VNCA
310200940308     C                   MOVEL     'N'           VAPC
310300940308     C                   MOVEL     *BLANK        VPPC
310400940308     C                   MOVEL     *BLANK        DFRI
310500940308     C                   MOVEL     *BLANK        DNPC
310600940308     C                   MOVEL     *BLANK        DPPC
310700960327     C                   MOVEL     'N'           V4CRCS
310800960327     C                   SETOFF                                       07
310900940308     C     ENDDIR        ENDSR
311000940308     C/SPACE 3
311100940308     C*---------------------------------------------------------------*
311200110909     C*   srctrpda - CONTROLLO se esiti pda                           *
311300940308     C*---------------------------------------------------------------*
311400110909     C     srctrpda      BEGSR
311500110909     c*
311600110909     c     kbol          chain     fipdc02t
311700110909     c                   if        not %found(fipdc02t)
311800110909     c                   leavesr
311900110909     c                   end
312000110909     c* se trova la bolla sul file di trasmissione dati al pda verifico
312100110909     c* se confermato annullamento
312200110909     c                   if        pdcatb = ' '
312300110909     c                   seton                                        43
312400110909     c                   leavesr
312500110909     c                   end
312600110909     c* verifico di non avere ricevuto esiti
312700110909     c                   eval      pcttrd = 'CET'
312800110909     c     kpct          setgt     fipct01l
312900110909     c     kpct          readpe    fipct01l
313000110909     c                   if        not %eof(fipct01l) and pdcndc = pctndc
313100110909     c                   seton                                        43
313200110909     c                   leavesr
313300110909     c                   end
313400110909     c* verifico se ricevuta la conferma all'annullamento altrimenti errore
313500110909     c                   eval      pcttrd = 'COK'
313600110909     c     kpct          setgt     fipct01l
313700111212     c                   do        *hival
313800110909     c     kpct          readpe    fipct01l
313900111212     c                   if        %eof(fipct01l)
314000111212     c                   leave
314100111212     c                   end
314200111212     c                   if        pdcndc = pctndc
314300110909     c                   eval      fipctcokds = pctdati
314400110909     c                   if        §PCTIDROW < PDCIDROW
314500110909     c                   seton                                        43
314600110909     c                   leavesr
314700110909     c                   end
314800110909     c                   end
314900111212     c                   enddo
315000110909     c*
315100110909     C     END           ENDSR
315200110909     C/SPACE 3
315300110909     C*---------------------------------------------------------------*
315400110909     C*   C H K A R B  - CONTROLLO /LETTURA FNARB01L                  *
315500110909     C*---------------------------------------------------------------*
315600110909     C     CHKARB        BEGSR
315700940308     C                   MOVE      *ZEROS        §KSC
315800940308     C                   MOVE      *ZEROS        §QUATR
315900940308     C                   MOVEL     '0'           $ERR
316000941219     C     KBOL          CHAIN(N)  FNARB01L                           43
316100940928     C  N43ARBDCM        COMP      *ZEROS                             43
316200940308     C   43              MOVEL     '1'           $ERR
316300940308      *
316400991001     C     $ERR          IFEQ      '0'
316500941010     C                   MOVE      ARBMGS        AMGDSP
316600941010     C                   MOVEL     ARBAAS        AMGDSP
316700941012     C                   MOVE      AMGDSP        G08INV
316800941012     C                   MOVE      *ZEROS        G08DAT
316900941012     C                   MOVEL     '3'           G08ERR
317000941012     C                   CALL      'XSRDA8'
317100941012     C                   PARM                    WLBDA8
317200941012     C                   Z-ADD     G08DAT        VDSP
317300941216     C                   MOVE      ARBMGS        EXMGS
317400941216     C                   MOVE      ARBLNA        EXLNA
317500950102     C                   MOVE      ARBAAS        EXAAS
317600950102     C                   MOVE      ARBLNP        EXLNP
317700950102     C                   MOVE      ARBNRS        EXNRS
317800950102     C                   MOVE      ARBNSP        EXNSP
317900940308     C* TIPO BOLLA
318000941006     C                   MOVEL     ARBCBO        VTBL
318100940308     C                   MOVEL     '3A'          TBLCOD
318200940308     C                   MOVEL     *BLANK        TBLKEY
318300941006     C                   MOVEL     ARBCBO        TBLKEY
318400940308     C     KTAB          CHAIN     TABEL                              21
318500940308     C   21              MOVEL     *ALL'?'       DTBL
318600940308     C  N21              MOVEL     TBLUNI        DS3A
318700940308     C  N21              MOVEL     §3ADES        DTBL
318800940308     C  N21              MOVEL     §3ATB1        EXPORT            1
318900950725     C  N21              MOVEL     §3ATB1        WEXTB1            2
319000950725     C  N21              MOVEL     §3ATB2        WEXTB2            2
319100940308     C  N21              MOVEL     §3AFTD        TASDES            1
319200940308     C  N21EXPORT        IFEQ      'A'
319300940308     C                   MOVE      ARBKSC        §KSC
319400941006     C                   MOVE      ARBCTR        KCTR
319500940308     C                   END
319600000120      * SE BOLLA DI RECUPERO NON LAQSCIO EFFETTUARE NON LASCIO
319700000120      * EFFETTUARE NE RESO NE DIROTTAMENTO NE CAMBIO DI PORTO
319800000120     C     §3ARBL        IFEQ      'R'
319900000120     C                   MOVEL     '2'           $ERR
320000000120     C                   END
320100941219      * GUARDO SE BOLLA ITALIA O ESTERO
320200941219     C                   CLEAR                   DS15
320300941219     C                   MOVEL     '15'          TBLCOD
320400941219     C                   MOVEL     *BLANK        TBLKEY
320500941219     C                   MOVEL     ARBNZD        TBLKEY
320600941219     C     KTAB          CHAIN     TABEL                              21
320700941219     C  N21              MOVEL     TBLUNI        DS15
320800971113     C* Scopro se filiale Estera in base a flag su AZORG
320900971113     C                   MOVEL     *BLANKS       ORGFL1
321000971113     C     ARBLNP        CHAIN     AZORG01L                           21
321100971230     C                   MOVEL     ORGDIT        DITLNP            3
321200000421      *CONTROLLO SE FILIALE POSTE-SE SI BOLLA NON TRATTABILE TRANNE SERIE 00
321300000417     C                   MOVEL     ORGDE3        OG143
321400000421     C                   MOVE      *BLANKS       WPOSTE            1
321500020729     C     §OgNTW        IFEQ      'PPT'
321600000421     C     ARBNRS        IFEQ      0
321700000421     C                   MOVE      '1'           WPOSTE
321800000421     C                   ELSE
321900000417     C                   MOVE      '3'           $ERR
322000000421     C                   END
322100000417     C                   END
322200000417      *
322300940928      * BOLLA CON CONTRASSEGNO
322400941216     C                   MOVE      *ZEROS        VASS
322500941216     C                   MOVEL     *BLANK        VVCAP
322600941216     C                   MOVEL     *BLANK        VVTIC
322700950221     C                   CLEAR                   AR9CAS
322800950221     C                   CLEAR                   AR9CMB
322900950221     C                   CLEAR                   AR9GCA
323000950221     C                   CLEAR                   AR9VCA
323100950221     C                   CLEAR                   AR9TIC
323200940928     C     §3AFCA        IFNE      0
323300051115     C     KBOL          CHAIN     FIAR901L                           43
323400941216     C  N43              Z-ADD     AR9CAS        VASS
323500940928     C  N43              MOVEL     AR9VCA        VVCAP
323600941216     C  N43              MOVEL     AR9TIC        VVTIC
323700940928     C                   ENDIF
323800941217     C* IMPOSTO LA DIVISA UAGUALE ALLA VECCHIA SE NO GIACENZE
323900941217     C     $GIAC         IFEQ      '0'
324000941010     C                   MOVEL     VVCAP         VVCA
324100941221     C* GUARDO CHE DIVISA è
324200941221     C                   CLEAR                   DSCV
324300941221     C                   Z-ADD     1             X
324400941221     C     VVCA          LOOKUP    DIV(X)                                 21
324500941221     C   21              MOVEL     DCV(X)        DSCV
324600941221     C*
324700941217     C                   ENDIF
324800941217     C* IMPOSTO  TIPO INCASSO SEMPRE UGUALE AL   VECCHIO
324900941217     C                   MOVEL     VVTIC         VTIC
325000940928     C* PRENDO DATI TASSAZIONE  1° BOLLA
325100941216     C                   CLEAR                   AVA
325200941216     C                   CLEAR                   ASV
325300950221     C                   CLEAR                   AR6FIV
325400950221     C                   CLEAR                   AR6NFT
325500950221     C                   CLEAR                   AR6DFT
325600950221     C                   CLEAR                   AR6CEI
325700950221     C                   CLEAR                   AR6IFT
325800950221     C                   CLEAR                   AR6IMV
325900950221     C                   CLEAR                   AR6POR
326000991001     C                   CLEAR                   AR6CTR
326100991001     C                   CLEAR                   AR6KSC
326200990930     C* CONTROLLO SE LA BOLLA E' IN PORTO ASSEGNATO
326300991001     C                   MOVEL     §3ATB1        WPORTO            1
326400991001     C     WPORTO        IFEQ      'A'
326500990930     C                   MOVEL     '1'           KTRC
326600020618     C     KAR6          CHAIN     FIAR601L                           43
326700991001     C     AR6DFT        IFGT      0
326800991001     C                   MOVE      AR6FIV        VFIV
326900991001     C                   MOVE      AR6NFT        VNFT
327000991001     C                   MOVE      AR6DFT        G08INV
327100991001     C                   MOVE      *ZEROS        G08DAT
327200991001     C                   MOVEL     '3'           G08ERR
327300991001     C                   CALL      'XSRDA8'
327400991001     C                   PARM                    WLBDA8
327500991001     C                   Z-ADD     G08DAT        VDFT
327600991001     C                   END
327700991001     C                   ELSE
327800991001     C                   MOVEL     §3ATB2        WPORTO
327900991001     C     WPORTO        IFEQ      'A'
328000991001     C                   MOVEL     '2'           KTRC
328100020618     C     KAR6          CHAIN     FIAR601L                           43
328200991001     C                   END
328300991001     C                   END
328400941010     C* CARICO LE VARIE PRESENTI + DATI FATTURA
328500991001     C     *IN43         IFEQ      '0'
328600991001     C* Imposto codice tariffa e codice cliente da AR6
328700991001     C* se c'è
328800991001     C                   MOVE      AR6KSC        §KSC
328900991001     C                   MOVE      AR6CTR        KCTR
329000941010      *
329100941216     C                   MOVEL     AR6SV1        ASV(1)
329200941216     C                   MOVE      AR6VA1        AVA(1)
329300941216     C                   MOVEL     AR6SV2        ASV(2)
329400941216     C                   MOVE      AR6VA2        AVA(2)
329500941216     C                   MOVEL     AR6SV3        ASV(3)
329600941216     C                   MOVE      AR6VA3        AVA(3)
329700940929     C* PRENDO DATI ESTENSIONE TASSAZIONE X VARIE AGGIUNTIVE
329800940929      * SE LA TERZA VARIA è IMPOSTATA
329900991001     C     AR6SV3        IFNE      *BLANK
330000940929     C                   Z-ADD     3             X
330100040123     C     Kar6          SETLL     FIAR7000
330200040123     C     Kar6          READE     FIAR7000                               43
330300991001     C     *IN43         DOWEQ     *OFF
330400940929     C                   ADD       1             X
330500941216     C                   MOVEL     AR7SVN        ASV(X)
330600941216     C                   MOVE      AR7VAN        AVA(X)
330700040123     C     Kar6          READE     FIAR7000                               43
330800991001     C                   ENDDO
330900991001     C                   ENDIF
331000020102     C*CONVERTO IMPORTI IN EURO SE LA VECCHIA BOLLA ERA IN LIRE
331100020102     C     AR6DIV        IFNE      §GEDCN
331200020102      *SPESE TRASPORTO
331300020102     C     AR6POR        IFGT      0
331400020102     C                   CLEAR                   YEURDS
331500020102     C                   MOVEL     AR6DIV        YECDVI
331600020102     C                   Z-ADD     AR6POR        YECIMI
331700020102     C                   MOVEL     §GEDCN        YECDVO
331800020102     C                   MOVEL     '3'           YECDCO
331900020102     C                   MOVEL     'H'           YECTAR
332000020102     C                   CALL      'YEURCO'
332100020102     C                   PARM                    YEURDS
332200020102     C                   Z-ADD     YECIMO        AR6POR
332300020102     C                   ENDIF
332400020102     C                   DO        20            XM                3 0
332500020102     C     AVA(XM)       IFGT      0
332600020102     C                   CLEAR                   YEURDS
332700020102     C                   MOVEL     AR6DIV        YECDVI
332800020102     C                   Z-ADD     AVA(XM)       YECIMI
332900020102     C                   MOVEL     §GEDCN        YECDVO
333000020102     C                   MOVEL     '3'           YECDCO
333100020102     C                   MOVEL     'H'           YECTAR
333200020102     C                   CALL      'YEURCO'
333300020102     C                   PARM                    YEURDS
333400020102     C                   Z-ADD     YECIMO        AVA(XM)
333500020102     C                   ENDIF
333600020102     C                   ENDDO
333700020102     C                   ENDIF
333800991001     C                   ENDIF
333900941011     C* PRENDO 2° RAGIOSNE SOCIALE DESTINATARIO
334000950221     C                   CLEAR                   AR4NOT
334100950307     C                   CLEAR                   AR4RD2
334200941011     C                   MOVE      'D'           §TRC
334300060213     C     KAR4          CHAIN(N)  FiAR401L                           43
334400941011     C  N43              MOVE      AR4NOT        VRD2
334500941227     C  N43              MOVE      AR4NOT        AR4RD2
334600031119      *eseguo la stessa operazione per i riferimenti mittenti tipo
334700031119      * rekord GEN riferimenti destinatario
334800031119     C                   clear                   dar5gen
334900031119     C                   move      'GEN'         trcar5
335000051013     C     Kfiar5        CHAIN(n)  FiAR501L                           43
335100031119     C     *IN43         IFEQ      '0'
335200031119     C                   MOVEL     ar5uni        dar5gen
335300031119     C                   endif
335400941011     C                   END
335500940308     C                   ENDSR
335600920113     C*---------------------------------------------------------------*
335700940309     C*   A R B V I D                                                 *
335800920113     C*---------------------------------------------------------------*
335900941219     C     ARBVID        BEGSR
336000961009     C* se c'è il reso obbligat. da particolarità reperisco il motivo
336100961009     C* dall' ultimo evento della spedizione
336200961009     C   94              EXSR      CHKEVB
336300940307     C                   SETOFF                                       197172
336400940307     C                   SETOFF                                       737475
336500050222     C                   SETOFF                                       7647
336600940309     C                   MOVEL     '0'           $FOANC            1
336700940915     C                   WRITE     LG20Z02
336800920114     C     VID11         TAG
336900000421     C     WPOSTE        IFEQ      '1'
337000000421     C                   MOVE      'R'           VFRI
337100000421     C                   MOVE      'S'           V4CRCS
337200000421     C                   SETON                                        93
337300000421     C                   ELSE
337400000421     C                   SETOFF                                       93
337500000421     C                   ENDIF
337600940915     C                   WRITE     LG20Z03
337700940915     C                   EXFMT     LG20D04
337800940307     C                   MOVEL     *BLANK        $MSG
337900940307     C                   SETOFF                                       197172
338000940307     C                   SETOFF                                       737475
338100050222     C                   SETOFF                                       7667
338200961009     C                   SETOFF                                       4796
338300940309     C   KL              GOTO      ENDARB
338400100614      * richiamo pgm interrogazione bolle
338500100614     C   Kg              exsr      richbl
338600920305     C* KH= CMD 08 PER FORZARE ANNULLO CONTRASSEGMO
338700940309      *--------------------------------------------
338800940309     C   KH              MOVEL     '1'           $FOANC
338900941217     C*  IMMESSO ?
339000941217     C     VFRI          IFEQ      '?'
339100941217     C                   MOVEL     '2D'          §COD
339200941217     C                   MOVEL     *BLANK        §KEY
339300941217     C                   MOVEL     *BLANK        §DES
339400941217     C                   MOVE      *BLANK        VVCA
339500941217     C                   CALL      'X§TB60R'
339600941217     C                   PARM                    CODUT
339700941217     C                   PARM                    §COD
339800941217     C                   PARM                    §KEY
339900941217     C                   PARM                    §DES
340000941217     C                   MOVEL     §KEY          VFRI
340100941217     C                   GOTO      VID11
340200941217     C                   ENDIF
340300960327     C*  Se ho immesso disposizioni di rientro riemetto videata
340400960327     C*  se non ho ancora visualizzato se reso per avaria.
340500960327     C     VFRI          IFEQ      'R'
340600960327     C     *IN07         IFEQ      '0'
340700960327     C                   SETON                                        07
340800960327     C                   GOTO      VID11
340900960327     C                   END
341000960327     C                   ELSE
341100960327     C*  Se non ho immesso disposizioni di rientro azzero
341200960327     C*  indicatore reso per avaria.
341300960327     C                   SETOFF                                       07
341400960327     C                   MOVEL     'N'           V4CRCS
341500960327     C                   END
341600941217     C*  SOLO RICONSEGNA O DIROTTAMENTO  O RIENTRO
341700920114     C                   MOVEL     '2D'          TBLCOD
341800920114     C                   MOVEL     *BLANK        TBLKEY
341900920114     C                   MOVEL     VFRI          TBLKEY
342000920114     C     KTAB          CHAIN     TABEL                              21
342100920114     C   21              SETON                                        7119
342200920114     C   21              MOVEL     *ALL'?'       DFRI
342300920114     C  N21              MOVEL     TBLUNI        DS2D
342400920114     C  N21              MOVEL     §2DDES        DFRI
342500940307     C   71              MOVEL     ERR(8)        $MSG
342600941217     C   19              GOTO      VID11
342700920131     C* POSSO FARE SOLO DIROTTAM.O INVERSIONI DI PORTO
342800940307     C  N19VFRI          IFEQ      'C'
342900920131     C     VNPC          ANDEQ     *BLANK
343000940307     C                   SETON                                        7219
343100940307     C                   MOVEL     ERR(9)        $MSG
343200920131     C                   END
343300920114     C* NUOVO PORTO DEVE ESSERE DIVERSO DALLA BOLLA VECCHIA
343400920114     C  N19VNPC          IFEQ      EXPORT
343500921026     C     VFRI          ANDNE     'R'
343600941228     C                   SETON                                        7219
343700941228     C                   MOVEL     ERR(10)       $MSG
343800920114     C                   END
343900960205     C* NUOVO PORTO DEVE ESSERE DIVERSO DALLA BOLLA VECCHIA
344000960205     C  N19VNPC          IFNE      *BLANKS
344100960205     C     ARBFBR        ANDEQ     'S'
344200960315     C     VFRI          ANDEQ     'C'
344300960205     C                   SETON                                        7219
344400960205     C                   MOVEL     ERR(46)       $MSG
344500960205     C                   END
344600960327     C* Impossibile eseguire reso in C/Servizio su bolla di rientro
344700960327     C     ARBFBR        IFEQ      'S'
344800960327     C     VFRI          ANDEQ     'R'
344900960327     C     V4CRCS        ANDEQ     'S'
345000960328     C                   SETON                                        477119
345100960327     C                   MOVEL     ERR(49)       $MSG
345200960327     C                   END
345300960315     C* NUOVO PORTO non ammesso per disposizioni di reso
345400960315     C  N19VNPC          IFNE      *BLANKS
345500960315     C     VFRI          ANDEQ     'R'
345600960315     C                   SETON                                        7219
345700960315     C                   MOVEL     ERR(47)       $MSG
345800960315     C                   END
345900990203     C* Controllo se esiste pretica CA aperta se richiesto
346000990203     C* cambio di porto/dirottamentoCA aperta se richiesto
346100990203     C  N19VFRI          IFEQ      'C'
346200990203     C     VFRI          OREQ      'D'
346300990204     C*
346400040716      * SOSTITUISCO LETTURA DEL FILE FNDCT02L CON LA RICERCA DELLE CA LEGATE ALL
346500040716      * SIA COME MAMMA CHE COME FIGLIA
346600040716     c                   clear                   fidn12ds
346700040716     c                   eval      i12aas = kaas
346800040716     c                   eval      i12lnp = klnp
346900040716     c                   eval      i12nrs = knrs
347000040716     c                   eval      i12nsp = knsp
347100040716     c                   eval      i12fel = 'E'
347200040716     c                   eval      i12fan = 'E'
347300040716     c                   eval      i12fch = 'E'
347400040716     c                   call      'FIDN12R'
347500040716     c                   parm                    fidn12ds
347600040716      **
347700040716      * se c'è almeno una C.A. aperta
347800040716      **
347900040716     c     o12nca        ifgt      0
348000990203     C                   SETON                                        7119
348100990203     C                   MOVE      ERR(58)       $MSG
348200990203     C                   END
348300990203     C                   END
348400950331     C* SE BOLLA DA NON TRASMETTERE IN SEDE SI PUO' FARE SOLO
348500950307     C* DIROTTAM. SENZA CAMBIO DI PORTO IN QUANTO FORZERO' NELLA
348600950307     C* BOLLA DIROTTATA COD. BOLLA =£, PERCHè QUESTA BOLLA NON
348700950307     C* VIENE INVIATA IN SEDE E ANCHE LA FIGLIA NON VA TRASM.IN SEDE
348800950331     C     *IN19         IFEQ      '0'
348900950331     C                   CLEAR                   DS3A
349000950331     C                   Z-ADD     1             §F
349100950331     C     ARBCBO        LOOKUP    CDB(§F)                                04
349200950331     C   04              MOVEL     D3A(§F)       DS3A
349300950331     C     §3ABSD        IFEQ      'N'
349400950410     C     VFRI          ANDNE     'D'
349500950410     C     §3ABSD        OREQ      'N'
349600950410     C     VNPC          ANDNE     *BLANK
349700950307     C                   SETON                                        7119
349800950307     C                   MOVEL     ERR(45)       $MSG
349900950307     C                   END
350000950307     C                   END
350100921026     C* SE RIENTRO IL C/ASSEGNO DEVE ESSERE ANNULLATO
350200921026     C  N19VFRI          IFEQ      'R'
350300921026     C     VASS          ANDGT     0
350400921026     C     VVCS          IFNE      'S'
350500921026     C     VNCA          ORGT      *ZEROS
350600940307     C                   SETON                                        7319
350700940307     C                   MOVEL     ERR(11)       $MSG
350800921026     C                   END
350900921026     C                   END
351000040206     C* SE RIENTRO IL C/ASSEGNO DEVE ESSERE ANNULLATO
351100040206     C     Vnca          IFgt      *zeros
351200040206     C     Vvca          andeq     *blank
351300040206     C                   SETON                                        7319
351400040206     C                   MOVEL     ERR(64)       $MSG
351500040206     C                   END
351600960315     C* SE RIENTRO IL C/ASSEGNO DEVE ESSERE ANNULLATO
351700960315     C  N19VFRI          IFEQ      'R'
351800960315     C     VASS          ANDEQ     0
351900960315     C     VNCA          IFGT      0
352000960315     C     VVCS          OREQ      'S'
352100960315     C                   SETON                                        7319
352200960315     C                   MOVEL     ERR(48)       $MSG
352300960315     C                   END
352400960315     C                   END
352500940930     C* SI VARIAZ. C/A MA NON VARIATO L'IMPORTO O LA DIVISA
352600940930     C* NO VARIAZ. C/A MA     VARIATO L'IMPORTO O LA DIVISA
352700920114     C  N19VVCS          IFEQ      'S'
352800920114     C     VNCA          ANDEQ     VASS
352900940930     C     VVCA          ANDEQ     VVCAP
353000920114     C     VVCS          OREQ      'N'
353100920114     C     VNCA          ANDNE     VASS
353200920129     C     VNCA          ANDNE     *ZEROS
353300940930     C     VVCS          OREQ      'N'
353400940930     C     VVCA          ANDNE     VVCAP
353500920114     C                   SETON                                        7319
353600940307     C                   MOVEL     ERR(12)       $MSG
353700920114     C                   END
353800920207     C* NO CALCOLO PROVV.C/A MA VARIATO C/A
353900920114     C* NO CALCOLO PROVV.C/A MA INDICATO CHI PAGA
354000920114     C  N19VAPC          IFEQ      'N'
354100920114     C     VVCS          ANDEQ     'S'
354200920207     C     VNCA          ANDNE     *ZEROS
354300920114     C     VAPC          OREQ      'N'
354400920114     C     VPPC          ANDNE     *BLANK
354500920114     C                   SETON                                        7419
354600940307     C                   MOVEL     ERR(13)       $MSG
354700920114     C                   END
354800920114     C  N19VPPC          IFNE      *BLANK
354900920114     C                   MOVEL     '2E'          TBLCOD
355000920114     C                   MOVEL     *BLANK        TBLKEY
355100920114     C                   MOVEL     VPPC          TBLKEY
355200920114     C     KTAB          CHAIN     TABEL                              21
355300920114     C   21              SETON                                        7519
355400920114     C   21              MOVEL     *ALL'?'       DPSG
355500920114     C  N21              MOVEL     TBLUNI        DS2E
355600920114     C  N21              MOVEL     §2EDES        DPPC
355700920114     C                   ELSE
355800920114     C* SI CALCOLO PROVV.C/A MA NON INDICATO CHI PAGA
355900920114     C     VAPC          IFEQ      'S'
356000920114     C                   SETON                                        7519
356100920114     C                   END
356200920114     C                   END
356300940307     C   75              MOVEL     ERR(14)       $MSG
356400941221     C* SE EX PORTO ERA ASSEGNATO E NON E' CAMBIATO  O
356500941221     C* SE NUOVO PORTO è ASSEGNATO ..NON SI PUO INDICARE PROVVIG.
356600941010     C* A CARICO MITTENTE
356700941010     C  N19VNPC          IFEQ      'A'
356800941010     C     VPPC          ANDEQ     'M'
356900941221     C     EXPORT        OREQ      'A'
357000941221     C     VNPC          ANDEQ     ' '
357100941221     C     VPPC          ANDEQ     'M'
357200941010     C                   SETON                                          7519
357300941010     C                   MOVEL     ERR(14)       $MSG
357400941010     C                   ENDIF
357500940310     C* SE C'E' C/ASS: sE DIROTTO O NUOVO PORTO = ASSEGNATO
357600940310     C* DEVO INDICARE CHI PAGA LE PROVV.
357700940930     C  N19VASS          IFGT      *ZEROS
357800941223     C     VVCS          ANDEQ     'N'
357900940310     C     VNCA          ORGT      *ZEROS
358000940310     C     VFRI          IFEQ      'D'
358100940310     C     VAPC          ANDEQ     'N'
358200960108     C     VFRI          OREQ      'C'
358300940310     C     VAPC          ANDEQ     'N'
358400940310     C                   SETON                                        747519
358500940310     C                   MOVEL     ERR(13)       $MSG
358600940310     C                   END
358700940310     C                   END
358800940930     C* CONTROLLO DIVISA
358900941012     C                   MOVEA     VVCA          VCA§
359000941012     C     '?'           LOOKUP    VCA§                                   21
359100941012     C   21              DO
359200941012     C                   MOVEL     'CV'          §COD
359300941012     C                   MOVEL     *BLANK        §KEY
359400941012     C                   MOVEL     *BLANK        §DES
359500941012     C                   MOVE      *BLANK        VVCA
359600941012     C                   CALL      'X§TB60R'
359700941012     C                   PARM                    CODUT
359800941012     C                   PARM                    §COD
359900941012     C                   PARM                    §KEY
360000941012     C                   PARM                    §DES
360100941012     C                   MOVEL     §KEY          VVCA
360200941012     C                   GOTO      VID11
360300941012     C                   END
360400940930     C  N19VVCA          IFNE      *BLANK
360500940930     C     VVCAP         ORNE      *BLANK
360600940930     C     VVCA          IFNE      *BLANK
360700940930     C                   MOVEL     VVCA          WVCA              3
360800940930     C                   ELSE
360900940930     C                   MOVEL     VVCAP         WVCA
361000940930     C                   END
361100940930     C     VNCA          IFNE      *ZEROS
361200940930     C                   Z-ADD     VNCA          W0133            13 3
361300940930     C                   ELSE
361400940930     C                   Z-ADD     VASS          W0133
361500940930     C                   END
361600940930     C                   EXSR      CTRDIV
361700940930     C                   ENDIF
361800920305     C* RICH. CMD 08 PER CONFERMA ANNULLO C/ASSEGNO
361900940309     C  N19$FOANC        IFEQ      '0'
362000940309     C     VNCA          ANDEQ     0
362100920305     C     VVCS          ANDEQ     'S'
362200941010     C     VVCA          ANDEQ     *BLANK
362300940307     C                   SETON                                        1976
362400940307     C                   MOVEL     ERR(16)       $MSG
362500920305     C                   END
362600961009     C*  Se  obbligo disposiz. di reso metto in VNOT1 il motivo dall'evento
362700961009     C*  e obbligo l'inserimento del motivo se l'evento è generico
362800961009     C     *IN94         IFEQ      '1'
362900961009     C     VNOT1         ANDEQ     *BLANK
363000961009     C                   SETON                                        1996
363100961009     C                   MOVEL     ERR(50)       $MSG
363200961009     C                   ENDIF
363300930503     C  NKF
363400920114     COR 19              GOTO      VID11
363500940309     C     ENDARB        ENDSR
363600961009     C/SPACE 3
363700961009     C*---------------------------------------------------------------*
363800961009     C*   C H K E V B  _                                              *
363900961009     C*---------------------------------------------------------------*
364000961009     C     CHKEVB        BEGSR
364100961009     C                   MOVE      VAAS          KAAS1
364200961009     C                   MOVE      VLNP          KLNP1
364300961009     C                   MOVE      VNRS          KNRS1
364400961009     C                   MOVE      VNSP          KNSP1
364500961009     C                   MOVE      VNSP          KNSP1
364600961009     C                   MOVE      *HIVAL        KDEV
364700961009     C                   MOVE      *HIVAL        KHEV
364800990803     C     KEVB1         SETGT     FNEVB01L
364900990803     C     KEVB2         READPE    FNEVB01L                               45
365000961009     C     *IN45         IFEQ      '0'
365100081219     C                   Z-ADD     1             XY                3 0
365200961009     C     EVBCEV        LOOKUP    C2A(XY)                                45
365300961009     C     *IN45         IFEQ      '1'
365400961009     C                   MOVEL     D2A(XY)       DS2A
365500041122      * se l'evento,la cui descrizione deve essere messa nelle note della
365600041122      * bolla di reso per effetto della particolarità,è un evento di giacenz
365700060213      * di attesa (G02, G03..) prendo la descrizione da FiAR4 TRK "G"
365800041122     c                   if        §2Aftc='G' and §2Agla>0 and §2Aflg<>'S'
365900041122     C                   MOVEL     'G'           §TRC
366000060213     C     KAR4          CHAIN(N)  FiAR401L                           43
366100041122     C  N43              MOVEL     AR4NOT        VNOT1
366200041122     c                   else
366300041122     c                   MOVEL     §2ADES        VNOT1
366400041122     c                   endif
366500961009     C                   ENDIF
366600961009     C                   ENDIF
366700050504     c* se particolarità consegne = no parziali imposto la dicitura nel
366800050504     c* campo delle note: Reso x particolarità "NO consegna parziale"
366900050504     c                   if        vnot1 = *blanks and arbgma <> ' '
367000050504     C                   MOVEL     '7R'          TBLCOD
367100050504     C                   MOVEL     *BLANK        TBLKEY
367200050504     C                   MOVEL     ARBGMA        TBLKEY
367300050504     C     KTAB          CHAIN     TABEL                              21
367400050504     C     *IN21         IFEQ      '0'
367500050504     C                   MOVEL     TBLUNI        DS7R
367600050504     C                   if        §7R8cp='N'
367700050504     c                   eval      vnot1=
367800050505     c                             'Reso:NO consegna -
367900050505     c                             parziale'
368000050504     c                   end
368100050504     c                   end
368200050504     c                   end
368300961009     C                   ENDSR
368400940308     C/SPACE 3
368500940308     C*---------------------------------------------------------------*
368600940308     C*   G C A V I D                                                 *
368700940308     C*---------------------------------------------------------------*
368800940308     C     GCAVID        BEGSR
368900940308     C* DATA APERTURA GIACENZA
369000940308     C                   MOVEL     GCPAGC        AMGDTG
369100941012     C                   MOVE      GCPMGC        AMGDTG
369200941012     C                   MOVE      AMGDTG        G08INV
369300941012     C                   MOVE      *ZEROS        G08DAT
369400941012     C                   MOVEL     '3'           G08ERR
369500941012     C                   CALL      'XSRDA8'
369600941012     C                   PARM                    WLBDA8
369700941012     C                   Z-ADD     G08DAT        VDTG
369800940315     C                   MOVE      GCPNGC        VNGC
369900940308     C* FIL. CHE HA APERTO LA GIACENZA
370000940308     C                   MOVE      GCPFGC        VFGC
370100940308     C     VFGC          IFNE      *ZEROS
370200940308     C     VFGC          CHAIN     AZORG01L                           21
370300940308     C  N21ORGFVA        COMP      '*'                                    21
370400940308     C   21              MOVEL     *ALL'?'       DFGC
370500940308     C  N21              MOVEL     ORGDES        DFGC
370600940308     C                   END
370700940308     C*
370800940308     C* DATA ULTIMA RIAPERTURA
370900050222     c                   if        gcpriap = 'R'
371000940928     C                   MOVE      GCPDUR        G08INV
371100940928     C                   MOVE      *ZEROS        G08DAT
371200940928     C                   MOVEL     '3'           G08ERR
371300940928     C                   CALL      'XSRDA8'
371400940928     C                   PARM                    WLBDA8
371500940928     C                   Z-ADD     G08DAT        VDIG
371600050222     c                   else
371700050222     C                   Z-ADD     0             VDIG
371800050222     c                   end
371900940308     C* DATI DELLA SPEDIZIONE
372000050222     C                   Z-ADD     GCPLNPa       VLNP
372100050222     C                   Z-ADD     GCPAASa       VAAS
372200050222     C                   Z-ADD     GCPNRSa       VNRS
372300050222     C                   Z-ADD     GCPNSPa       VNSP
372400940308     C* DATA DISPOSIZINI
372500940928     C                   MOVE      GCPDDM        G08INV
372600940928     C                   MOVE      *ZEROS        G08DAT
372700940928     C                   MOVEL     '3'           G08ERR
372800940928     C                   CALL      'XSRDA8'
372900940928     C                   PARM                    WLBDA8
373000940928     C                   Z-ADD     G08DAT        VDDM
373100940928     C* DATA ESEGUIBILITA DISPOSIZIONI
373200940928     C                   SETOFF                                       79
373300940928     C     GCPDED        IFGT      0
373400940928     C                   MOVE      GCPDED        G08INV
373500940928     C                   MOVE      *ZEROS        G08DAT
373600940928     C                   MOVEL     '3'           G08ERR
373700940928     C                   CALL      'XSRDA8'
373800940928     C                   PARM                    WLBDA8
373900940928     C                   Z-ADD     G08DAT        VDED
374000940928     C     GCPDED        IFGT      AMGUDT
374100940928     C                   MOVEL     ERR(36)       $MSG
374200940928     C                   SETON                                        19
374300940928     C                   ENDIF
374400940928     C                   SETON                                        79
374500940928     C                   ENDIF
374600940308     C* TIPO DISPOSIZIONI
374700940311     C* SE HO FORZATO DISPOSIZ. NON IMPOSTO DA GIAC
374800940311     C     $FOFRI        IFEQ      '0'
374900941006     C                   MOVEL     GCPDIS        VFRI
375000940311     C                   MOVEL     GCPASG        VASG
375100940311     C                   MOVEL     GCPPSG        VPSG
375200940311     C                   END
375300940308     C                   MOVEL     '2D'          TBLCOD
375400940308     C                   MOVEL     *BLANK        TBLKEY
375500940308     C                   MOVEL     VFRI          TBLKEY
375600940308     C     KTAB          CHAIN     TABEL                              21
375700940308     C   21              MOVEL     *ALL'?'       DFRI
375800940308     C  N21              MOVEL     TBLUNI        DS2D
375900940308     C  N21              MOVEL     §2DDES        DFRI
376000940308     C* SPESE GIACENZA
376100940308     C     VPSG          IFNE      *BLANK
376200940308     C                   MOVEL     '2E'          TBLCOD
376300940308     C                   MOVEL     *BLANK        TBLKEY
376400940308     C                   MOVEL     VPSG          TBLKEY
376500940308     C     KTAB          CHAIN     TABEL                              21
376600940308     C   21              MOVEL     *ALL'?'       DPSG
376700940308     C  N21              MOVEL     TBLUNI        DS2E
376800940308     C  N21              MOVEL     §2EDES        DPSG
376900940308     C                   END
377000940308     C* NUOVO PORTO DI CONSEGNA
377100940308     C                   MOVEL     GCPNPC        VNPC
377200940308     C     GCPNPC        IFNE      *BLANK
377300940308     C     GCPNPC        COMP      'A'                                    21
377400940308     C   21              MOVEL     'ASSEGNAT'    DNPC
377500940308     C   21              MOVE      'O '          DNPC
377600940308     C  N21              MOVEL     'FRANCO  '    DNPC
377700940308     C  N21              MOVE      '  '          DNPC
377800940308     C                   ELSE
377900940308     C                   MOVE      *BLANKS       DNPC
378000940308     C                   END
378100991221     C* GEST. C/ASSEGNO
378200040203     c                   if        gcpvca <> *blank
378300940928     C                   MOVEL     GCPVCA        VVCA
378400040203     c                   else
378500040203     C                   MOVEL     ar9VCA        VVCA
378600040203     c                   end
378700941217     C* GUARDO CHE DIVISA è
378800941217     C                   CLEAR                   DSCV
378900941217     C                   Z-ADD     1             X
379000941217     C     VVCA          LOOKUP    DIV(X)                                 21
379100941217     C   21              MOVEL     DCV(X)        DSCV
379200941217     C*
379300940308     C                   MOVEL     GCPVCS        VVCS
379400940928     C                   MOVE      GCPCAS        VNCA
379500940308     C                   MOVEL     GCPAPC        VAPC
379600940308     C                   MOVEL     GCPPPC        VPPC
379700940308     C     VPPC          IFNE      *BLANKS
379800940308     C                   MOVEL     '2E'          TBLCOD
379900940308     C                   MOVEL     *BLANK        TBLKEY
380000940308     C                   MOVEL     VPPC          TBLKEY
380100940308     C     KTAB          CHAIN     TABEL                              21
380200940308     C   21              MOVEL     *ALL'?'       DPSG
380300940308     C  N21              MOVEL     TBLUNI        DS2E
380400940308     C  N21              MOVEL     §2EDES        DPPC
380500940308     C                   END
380600960327     C                   MOVEL     'N'           V4CRCS
380700960327     C                   SETOFF                                       07
380800940308     C                   ENDSR
380900940308     C*---------------------------------------------------------------*
381000940308     C* G C A N O T   GESTIONE NOTE
381100940308     C*---------------------------------------------------------------*
381200940308     C     GCANOT        BEGSR
381300940308     C                   EXSR      PULIZ
381400940308     C                   MOVE      *ZEROS        §VARR
381500970625     C*  CARICO NOTE APERTURA GIACENZA
381600970625     C                   Z-ADD     12            §FAS
381700050222     C     KNOT          CHAIN     tignp01L                           21
381800970625     C     *IN21         IFEQ      '0'
381900970625     C                   MOVEL     GNPDMC        VDMC
382000970625     C                   ADD       1             NRR               3 0
382100970625     C                   WRITE     LG20S05
382200970625     C                   END
382300970625     C*  NOTE GIACENZA
382400970625     C                   Z-ADD     20            §FAS
382500050222     C     KNOT          SETLL     tignp01L                               21
382600970625     C     *IN21         DOUEQ     '1'
382700050222     C     KNOT          READE     tignp01L                               21
382800051013     c                   if        not *in21
382900051013     c                   movel     gcpflr        dgcpflr
383000051013     c* se disposizioni automatiche
383100051013     c                   if        d§gcpdisa = 'S' and gnptpn = 'R'
383200051013     c                   if        gnpdif=915
383300051013     c                             or gnpdif=9152
383400051013     c                   eval      DGNPDMC915 = gnpdmc
383500051013     c                   movel     'D'           flgdis            1
383600051013     c                   end
383700051013     c                   if        gnpdif=917
383800051013     c                   eval      DGNPDMC917 = gnpdmc
383900051013     c                   movel     'F'           flgdis            1
384000060317     c                   movel     d917persona   vref
384100051013     c                   end
384200051013     c                   end
384300051220      **
384400051220      **  Non deve scrivere la riga che riporta la dicitura:
384500051220      **    "DATA DISPOS.CLIENTE:"
384600051220     c                   if        %subst(GNPDMC:1:20) <> 'DATA DISPOS.CLIENTE:'
384700051013     C                   MOVEL     GNPDMC        VDMC
384800051013     C                   ADD       1             NRR               3 0
384900051013     C                   WRITE     LG20S05
385000051220     c                   end
385100051220      **
385200970625     C* SE NR.PROGR. =999 E DESCRIZ. INIZIA CON §§ PRENDO L'IMPORTO
385300970625     C* DELLA VARIA R CALCOLATA DALLA PARTENZA SE DISPOSIZ. RIENTRO
385400970625     C*
385500051013     C     VFRI          IFEQ      'R'
385600970625     C     GNPNPR        ANDEQ     999
385700970625     C                   MOVEL     GNPDMC        §VR               2
385800970625     C     §VR           IFEQ      '§§'
385900970625     C                   MOVE      GNPDMC        §VARR
386000970625     C                   END
386100051013     C                   END
386200970625     C                   END
386300970625     C                   END
386400970625     C     NRR           IFEQ      0
386500970625     C                   MOVEL     CNOTA         VDMC
386600970625     C                   ADD       1             NRR               3 0
386700970625     C                   WRITE     LG20S05
386800970625     C                   END
386900940308     C*
387000961203     C                   WRITE     LG20C05
387100940308     C     ENDNOT        ENDSR
387200940308     C* PULIZIA SUBFILE
387300940308     C     PULIZ         BEGSR
387400940308     C                   SETON                                        70
387500940915     C                   WRITE     LG20C05
387600940308     C                   Z-ADD     0             NRR
387700940308     C                   SETOFF                                       70
387800940308     C                   ENDSR
387900920201     C/SPACE 3
388000920201     C*---------------------------------------------------------------*
388100920201     C*   A Z Z E R                                                   *
388200920201     C*---------------------------------------------------------------*
388300920201     C     AZZER         BEGSR
388400920201     C                   SETOFF                                       262730
388500920201     C                   SETOFF                                       313233
388600920228     C                   SETOFF                                       398788
388700920201     C                   SETOFF                                       353637
388800920201     C                   SETOFF                                       808182
388900950120     C                   SETOFF                                       832346
389000141118     C                   SETOFF                                       922034
389100940309     C                   SETOFF                                       99
389200940304     C*
389300990930     C                   CLEAR                   DBLP
389400990930     C                   CLEAR                   DTASV
389500051013     C                   CLEAR                   flgdis
389600920201     C*
389700141118     C                   MOVE      *BLANK        wrecsp
389800920217     C                   MOVE      *BLANK        §§KCVB
389900050405     C                   MOVE      0             spearr            1 0
390000940304     C                   CLEAR                   DSARBK
390100920217     C                   MOVE      *BLANK        §§TCVB
390200990930     C                   CLEAR                   DARBT
390300940304     C                   CLEAR                   DSARBD
390400920201     C*
390500950120     C                   MOVEL     'S'           VSIFAT
390600920201     C                   MOVEL     *ZEROS        VRCA
390700920201     C                   MOVEL     *ZEROS        VRTR
390800920201     C                   MOVEL     *ZEROS        VDSD
390900920201     C                   MOVEL     *ZEROS        VDSA
391000920201     C                   MOVEL     *ZEROS        VGGS
391100920201     C                   MOVEL     *ZEROS        VSGF
391200940325     C                   MOVEL     *BLANK        SGD$              1
391300940325     C                   MOVEL     *BLANK        SGF$              1
391400940325     C                   MOVEL     *BLANK        SGR$              1
391500060113     C                   MOVEL     *BLANK        sgfaf$            1
391600920201     C                   MOVEL     *ZEROS        VTSGS
391700920201     C                   MOVEL     *ZEROS        VSGR
391800920201     C                   MOVEL     *ZEROS        VSGD
391900920201     C                   MOVEL     *ZEROS        VSGA
392000991028     C                   MOVEL     §GEDCN        V6CDIV
392100991028     C                   MOVEL     §GEDCN        V6CRVA
392200991028     C                   MOVEL     §GEDCN        V6CRTA
392300990628     C                   MOVEL     *BLANK        VRSD
392400941011     C                   MOVEL     *BLANK        VRD2
392500920201     C                   MOVEL     *BLANK        VIND
392600920201     C                   MOVEL     *BLANK        VLOD
392700941217     C                   MOVEL     *BLANK        VCAD
392800920201     C                   MOVEL     *BLANK        VPRD
392900920201     C                   MOVEL     *ZEROS        VLNA
393000920201     C                   MOVEL     *BLANK        VDLNA
393100941216     C                   MOVE      *ZEROS        §ANTEP
393200031119     C                   MOVE      *BLANK        Vteld
393300031119     C                   MOVE      *BLANK        Vref
393400030807     C* ----------------------------
393500030807     C* IMPOSTA LA TABELLA DEI tipi record di ar5 da duplicare
393600030807     C* ----------------------------
393700030807     C                   MOVEL     'AR5'         TBECOD
393800030807     C                   Z-ADD     0             bb                3 0
393900030807     C                   Z-ADD     0             cc                3 0
394000030807     c                   clear                   rec
394100030807     c                   clear                   fig
394200030807     C     TBECOD        setll     TNTBE01L
394300030807     C                   DO        *hival
394400030807     C     TBECOD        READE     TNTBE01L
394500030807     c                   if        %eof(tntbe01l)
394600030807     c                   leave
394700030807     c                   end
394800030807     C     TBEATB        IFEQ      *BLANK
394900030807     C                   MOVEL     TBEUNI        DAR5
395000030807     c* aggiungo nella schiera FIG i tipi record da duplicare per le nuove
395100030807     c* bolle figlie a seconda delle varie disposizioni
395200030807     c                   select
395300030807     c                   when      vfri = 'D' and §ar5dupd = 'S'
395400030807     C                   ADD       1             bb
395500030807     C                   MOVEL     tbeke1        fig(bb)
395600030807     c                   when      vfri = 'R' and §ar5dupr = 'S'
395700030807     C                   ADD       1             bb
395800030807     C                   MOVEL     tbeke1        fig(bb)
395900030807     c                   when      vfri = 'C' and §ar5dupc = 'S'
396000030807     C                   ADD       1             bb
396100030807     C                   MOVEL     tbeke1        fig(bb)
396200030807     C                   ENDsl
396300030807     c* aggiungo nella schiera REC i tipi record da duplicare per le nuove
396400030807     c* bolle di recupero
396500030807     c                   if        §ar5drec = 'S'
396600030807     C                   ADD       1             cc
396700030807     C                   MOVEL     tbeke1        rec(cc)
396800030807     C                   END
396900030807     C                   END
397000030807     C*
397100030807     C                   ENDDO
397200920201     C                   ENDSR
397300911025     C*----------------------------------------------------*
397400911025     C*
397500911025     C*     R I E N T R I
397600911025     C*
397700911025     C*----------------------------------------------------*
397800911025     C     RIENTR        BEGSR
397900911114     C*
398000951107     C* NON PREVISTO  ADDEBITO SPESE GIACENZA AL DESTINAT.
398100951107     C*
398200920128     C* IN OGNI CASO IL C/ASSEGNO  DEVE ESSERE ANNULLATO
398300920324     C* SE è POSSIBILE TRASFORMO IN CONTO SERVIZIO
398400920129     C     VASS          IFGT      *ZEROS
398500920128     C* ANNULLO C/A CAMBIO TIPO BOLLA
398600911115     C                   SETON                                          80
398700920220     C                   MOVEL     'KB'          §CDVAR
398800920128     C                   END
398900920220     C     §CDVAR        IFEQ      'KB'
399000920220     C                   EXSR      CNTVAR
399100920220     C  N26
399200920220     CANN27              MOVEL     §CDVAR        §§KCVB
399300920220     C                   END
399400920324     C*
399500920324     C                   MOVEL     §3ATB1        NWTB1
399600920324     C* TRASFORMO IN C/S  SE ERA ASSEGNATO
399700920324     C     EXPORT        IFEQ      'A'
399800950726     C     WEXTB1        ANDNE     'AS'
399900920324     C                   MOVEL     'AS'          NWTB1
400000920324     C                   SETON                                          8039
400100920324     C                   END
400200951107     C*
400300920324     C                   MOVEL     §3ATB2        NWTB2
400400920324     C* ANNULLO C/A  SE ESISTE LA 2° BOLLA LA ELIMINO
400500920324     C     §3ATB2        IFEQ      'A3'
400600920505     C     §3ATB2        OREQ      'AP'
400700920324     C                   MOVEL     '  '          NWTB2
400800920324     C                   SETON                                          8039
400900920324     C                   END
401000920128     C     *IN80         IFEQ      '1'
401100920325     C*
401200941215     C                   MOVE      *ZEROS        §BKCAS
401300941219     C                   MOVEL     *BLANK        §BKVCA
401400920325     C* CONTROLLO SE VARIAZIONE è POSSIBILE
401500920325     C                   MOVEL     'K8'          §CDVAR
401600920325     C                   EXSR      CNTVAR
401700920325     C* CERCO NUOVO CODICE BOLLA
401800920325     C  N26
401900920325     CANN27              DO
402000071009     C                   MOVEL     §CDVAR        §§KCVB
402100920128     C                   MOVEL     '0'           NWFCA
402200920325     C                   MOVEL     §3AFAN        NWFAN
402300911115     C                   MOVEL     §3AFTD        NWFTD
402400920201     C                   MOVEL     *BLANK        NWCBL
402500920201     C                   MOVEL     *BLANK        §BKCBN
402600920201     C                   EXSR      NEWCBL
402700920201     C  N30              MOVEL     NWCBL         §BKCBN
402800920325     C                   END
402900911025     C                   END
403000920128     C* SE ESISTE 2°BOLLA E NON POSSO TRASF. IN C/S : N.ACCREDITO
403100991001     C   27AR6NFT        IFNE      *ZEROS
403200991001     C     AR6TRC        ANDEQ     '2'
403300950726     C     ARBICA        ANDNE     'Y'
403400920128     C                   SETON                                        37
403500991001     C                   Z-ADD     AR6IMV        §INAC2
403600920128     C                   END
403700920128     C* NON HO TRASFORMATO IN C/S RICHIEDO N.ACCRED. SE FATT.IN ARRIVO
403800940928     C   26AR6NFT        IFNE      *ZEROS
403900991001     C     AR6TRC        ANDEQ     '1'
404000940928     C     AR6FIV        ANDEQ     ARBLNA
404100950726     C     ARBICA        ANDNE     'Y'
404200920128     C                   SETON                                        32
404300940928     C                   Z-ADD     AR6IMV        §INAC1
404400920128     C                   END
404500911114     C* CREAZ. BOLLA DI RIENTRO A2
404600911114     C* --------------------------
404700911025     C                   SETON                                        31
404800911025     C*
404900941003     C                   MOVEL     '2'           §LPCBO
405000960327     C*  Se reso in conto servizio imposto 'A' nel codice bolla
405100960327     C     V4CRCS        IFEQ      'S'
405200960327     C                   MOVEL     'A'           §LPCBO
405300960327     C                   END
405400971113     C* INVERTO LP CON LA
405500971113     C                   MOVE      ARBLNP        §LPLNA
405600911113     C                   MOVE      ARBLNA        §LPLNP
405700150518     C*
405800950427     C                   Z-ADD     ARBAAS        KAAS
405900950427     C                   Z-ADD     ARBLNP        KLNP
406000950427     C                   Z-ADD     ARBNRS        KNRS
406100950427     C                   Z-ADD     ARBNSP        KNSP
406200950427     C     KBOL          CHAIN     FNLBL01L                           04
406300061026      ** salva la chiave da riutilizzare x Rientri DPD di bolle dirottate
406400061026      **  per poter riagganciare il Parcel dalla Bolla Originale da cui
406500061026      **  prendere il Depot x agganciare anagrafica indirizzo a cui rispedire
406600061026     C                   clear                   LBLAAOsv
406700061026     C                   clear                   LBLLPOsv
406800061026     C                   clear                   LBLNROsv
406900061026     C                   clear                   LBLNSOsv
407000061026     c                   if        %Found(FNLBL01L)
407100061026     C                   z-add     LBLAAO        LBLAAOsv
407200061026     C                   z-add     LBLLPO        LBLLPOsv
407300061026     C                   z-add     LBLNRO        LBLNROsv
407400061026     C                   z-add     LBLNSO        LBLNSOsv
407500061026     c                   end
407600061026      **
407700950427     C     *IN04         IFEQ      '0'
407800950427     C     LBLLPO        ANDNE     0
407900960315     C     LBLLPO        ANDNE     ARBLNA
408000150521     C     LBLLPO        ANDNE     §LPLNA
408100150518     C                   MOVE      LBLLPO        §LPLNA
408200950427     C                   END
408300150518      *****
408400150518     c*  Se su AZORG c'è la forzatura per la LNA viene sostituita la
408500150518     c*    LNA con quella di OG143 §ogLNAres
408600150518     c                   exsr      CHG_LNA_xFORZA
408700150518     C*
408800150518     C                   MOVE      §LPLNA        VLNA
408900150518      *****
409000080528     C**   VLNA          IFEQ      105
409100971113     C* SE SDI = 105 FORZO 005
409200080528     C**                 MOVE      005           §LPLNA
409300080528     C**                 MOVE      005           VLNA
409400080528     C**                 END
409500911218     C* STAMPA SEGNACOLLI SI - C. TRATT. MERCE  DEFAULT FILIALE
409600140610     c                   exsr      srctm2k
409700940310     C* IMPOSTO §LPNOT
409800940310     C                   EXSR      XLPNOT
409900960327     C*----------------------------------------------------*
410000960327     C* se no reso in C/Servizio
410100960327     C     V4CRCS        IFNE      'S'
410200960327     C*----------------------------------------------------*
410300000405     C* SPESE GIACENZA
410400000405     C     $GIAC         IFEQ      '1'
410500000405     C     GCPSGS        ADD       GCPSGR        §TOTSP
410600000405     C     GCPSGD        ADD       §TOTSP        §TOTSP
410700000405     C     GCPSGA        ADD       §TOTSP        §TOTSP
410800000405     C                   END
410900010524     C*IMPOSTO CODICE PER RICERCA LUOGO
411000010524     C                   Z-ADD     ARBKSC        WCODIC
411100911114     C* SE ERA ASSEGNATO TRASPORTO VA COME ANTEPORTO
411200911114     C     EXPORT        IFEQ      'A'
411300010522     C                   Z-ADD     88888888      §ANTEP
411400010524     C                   Z-ADD     ARBCCM        WCODIC
411500000405     C                   END
411600911025     C* MITTENTE
411700911113     C                   MOVEL     ARBRSD        §LPRSM
411800911113     C                   MOVEL     ARBIND        §LPINM
411900911113     C                   MOVE      ARBCAD        §LPCAM
412000911113     C                   MOVEL     ARBLOD        §LPLOM
412100911113     C                   MOVEL     ARBPRD        §LPPRM
412200940928     C                   MOVEL     ARBNZD        §LPNZM
412300070919      * imposto destinatario
412400070919     c
412500070919     C                   MOVEL     ARBPRM        §LPPRD
412600070919     C                   MOVE      ARBCAM        §LPCAD
412700070919     C                   MOVE      ARBCAM        VCAD
412800070919     C                   MOVEL     ARBPRM        VPRD
412900010522     C* DESTINATARIO IMPOSTO I DATI STANDARD MA POI CONTROLLO FORZATURA
413000010522     C* SE NELLA FILIALE È GESTITO IL LUOGO 300 IMPOSTO I DATI DI
413100010522     C* DESTINAZIONE DAL FNSPE SE PRESENTE PER IL CLIENTE
413200070809      * ?Se non esiste il luogo e il cliente è un codificato ed è impostato*/
413300070807      * ?RMO prende i dati da CNACO                                         */
413400071002     c                   if        wcodic <> 0
413500070810     c                   clear                   cod4              4 0
413600071002     c                   move      wcodic        cod4
413700070808     c                   if        arbrmo <> *blank and
413800070808     c                             (cod4 <> 9999 and cod4 <> 8888)
413900070808     c                   exsr      repcnaco
414000070919     c                   endif
414100070919     c                   endif
414200070919      *
414300010522     C                   MOVEL     ARBRSM        §LPRSD
414400010522     C                   MOVEL     *BLANK        §LPRD2
414500010522     C                   MOVEL     ARBINM        §LPIND
414600010522     C                   MOVEL     ARBLOM        §LPLOD
414700010522     C                   MOVEL     ARBNZM        §LPNZD
414800010522     C*
414900010522     C                   MOVEL     ARBRSM        VRSD
415000010522     C                   MOVEL     *BLANK        VRD2
415100010522     C                   MOVEL     ARBINM        VIND
415200010522     C                   MOVEL     ARBLOM        VLOD
415300010522     C                   MOVEL     ARBNZM        VNZD
415400010522      * SE GESTITO LUOGO 300 IN FILIALE CERCO LUOGO PER IL DESTINATARIO
415500010522     C     KFNSPE        CHAIN     FNSPE01L                           21
415600041221      * prima di impostare i dati del luogo verifico che non ci siano
415700041221      * delle eccezioni per cliente linea di partenza
415800070322      * pulisco comunque il campo del giro forzato
415900070322     c                   clear                   §lpcgi
416000070903     c                   setoff                                       55
416100041221     c                   if        not *in21
416200041221     c                   exsr      ecceluogo
416300041221     c                   endif
416400010522      * SE TROVO IL LUOGO IMPOSTATO LO FORZO RISPETTO ALL'ORIGINALE
416500010522     C     *IN21         IFEQ      '0'
416600070903     C  n55              MOVEL     *BLANK        §LPLNA
416700010522     C                   MOVEL     SPERAG        §LPRSD
416800010522     C                   MOVEL     *BLANK        §LPRD2
416900010522     C                   MOVEL     SPEIND        §LPIND
417000010522     C                   MOVE      SPECAP        §LPCAD
417100010522     C                   MOVEL     SPELOC        §LPLOD
417200010522     C                   MOVEL     SPEPRO        §LPPRD
417300010522     C                   MOVEL     SPENAZ        §LPNZD
417400010522     C*
417500070903     C  n55              MOVEL     *BLANK        VLNA
417600010522     C                   MOVEL     SPERAG        VRSD
417700010522     C                   MOVEL     *BLANK        VRD2
417800010522     C                   MOVEL     SPEIND        VIND
417900010522     C                   MOVE      SPECAP        VCAD
418000010522     C                   MOVEL     SPELOC        VLOD
418100010522     C                   MOVEL     SPEPRO        VPRD
418200010522     C                   MOVEL     SPENAZ        VNZD
418300010524     C                   SETON                                        46
418400010524     C                   ENDIF
418500911113     C*
418600911113     C* IMPOSTO IL CODICE FINE GIACENZA
418700941217     C     $GIAC         IFEQ      '1'
418800911121     C                   MOVEL     '002'         VCFG
418900911114     C                   MOVEL     '2C'          TBLCOD
419000911114     C                   MOVEL     *BLANK        TBLKEY
419100911114     C                   MOVEL     VCFG          TBLKEY
419200911114     C     KTAB          CHAIN     TABEL                              21
419300911114     C   21              MOVEL     *ALL'?'       DCFG
419400060410     C  N21              MOVEL     TBLUNI        Ds2c
419500060410     C  N21              MOVEL     §2cdes        DCFG
419600941012     C                   MOVE      GMAUDT        VDCG
419700960327     C                   ENDIF
419800960327     C*----------------------------------------------------*
419900960327     C                   ELSE
420000960327     C*  Reso in C/Servizio
420100960327     C*  Mittente: Filiale Lna bolla
420200960328     C     ARBLNA        CHAIN     AZORG01L                           21
420300960327     C                   MOVEL     ORGDES        §LPRSM
420400960327     C                   MOVEL     ORGIND        §LPINM
420500960328     C                   MOVEL     ORGCPF        §LPCAM
420600960327     C                   MOVEL     ORGLOC        §LPLOM
420700960327     C                   MOVEL     ORGPRO        §LPPRM
420800960327     C                   MOVEL     *BLANKS       §LPNZM
420900960327     C*  Destinatario: Filiale Lnp bolla originale
421000960328     C     VLNA          CHAIN     AZORG01L                           21
421100960327     C                   MOVEL     ORGDES        §LPRSD
421200960327     C                   MOVEL     *BLANK        §LPRD2
421300960327     C                   MOVEL     ORGIND        §LPIND
421400960328     C                   MOVEL     ORGCPF        §LPCAD
421500960327     C                   MOVEL     ORGLOC        §LPLOD
421600960327     C                   MOVEL     ORGPRO        §LPPRD
421700960327     C                   MOVEL     *BLANKS       §LPNZD
421800960327     C*  Seconda note: Ragione sociale mittente originale
421900960327     C                   MOVEL     ARBRSM        §LPNT2
422000960327     C*
422100960327     C                   MOVEL     ORGDES        VRSD
422200960327     C                   MOVEL     *BLANK        VRD2
422300960327     C                   MOVEL     ORGIND        VIND
422400960328     C                   MOVEL     ORGCPF        VCAD
422500960327     C                   MOVEL     ORGLOC        VLOD
422600960327     C                   MOVEL     ORGPRO        VPRD
422700960327     C                   MOVEL     *BLANKS       VNZD
422800960327     C                   ENDIF
422900911025     C                   ENDSR
423000070807      *_______________________________________________________
423100070807     c     repcnaco      begsr
423200070807      *_______________________________________________________
423300070807     c                   move      wcodic        i69kac
423400070807     c                   move      wcodic        i69kin
423500070807     C                   CALL      'TIBS69R'
423600070807     C                   PARM                    tibs69DS
423700070807     C                   PARM                    DS_cnaco
423800070807     C                   PARM                    DS_cnind
423900070807     C                   PARM                    DS_cnclp
424000070807     C                   PARM                    DS_fncls
424100070807      *
424200070807     C                   if        O69ERR <> '1'
424300070807     c                   movel     indcae        §lpcad
424400070807     c                   movel     indprv        §lpprd
424500070807     c                   end
424600070807     C*
424700070807     C                   MOVE      indcae        VCAD
424800070807     C                   MOVEL     indprv        VPRD
424900070807     C                   Endsr
425000041221      *----------------------------------------------------
425100041221     c     ecceluogo     begsr
425200041221      *----------------------------------------------------
425300041221     c                   move      'PP'          wtip
425400041221     c     ksp2          chain     fnsp201l
425500041221      * rek eccezioni partenza
425600041221     c                   if        %found(fnsp201l)
425700041221     c                   movel     sp2est        dpar
425800041221     c                   z-add     1             xi                3 0
425900070322     c                   z-add     0             xxi               3 0
426000061027      * se ho trovato il legame 04=*off cerco con la linea bolla originale
426100070322      * altrimenti con la linea bolla
426200061027     c  n04lbllpo        lookup    par(xi)                                55
426300061027     c   04arblnp        lookup    par(xi)                                55
426400041221      * se esiste linea di partenza cerco eccezione in arrivo corrispondente
426500041221     c                   if        *in55
426600041221     c                   move      'PA'          wtip
426700041221     c     ksp2          chain     fnsp201l
426800041221     c                   if        %found(fnsp201l)
426900041221     c                   movel     sp2est        darr
427000041221     c                   move      arr(xi)       vlna
427100041221     c                   move      arr(xi)       §lplna
427200041221     c                   endif
427300060807      * cerco anche la zona
427400060807     c                   move      'PZ'          wtip
427500060807     c     ksp2          chain     fnsp201l
427600060807     c                   if        %found(fnsp201l)
427700060807     c                   movel     sp2est        dzon
427800060807     c                   move      zon(xi)       §lpznc
427900060807     c                   endif
428000070322      *reperisco il rekord x il giro
428100070322      * gli eventuali giri forzati sono per ragioni di spazio scritti su 4
428200070322      * rekord quindi x reperire quello giusto è necessario individuare su
428300070322      * quale rekord puntare basandosi sull'indice della linea di partenza
428400070322      * ogni rekord contiene 6 giri quindi per ricavare l'indice corretto
428500070322      * è necessario sottrarre all'indice gli interi precedenti
428600070322     c                   select
428700070322     c                   when      xi < 7
428800070322     c                   move      'G1'          wtip
428900070322     c                   z-add     0             xxi
429000070322     c                   when      xi < 13
429100070322     c                   move      'G2'          wtip
429200070322     c                   z-add     6             xxi
429300070322     c                   when      xi < 19
429400070322     c                   move      'G3'          wtip
429500070322     c                   z-add     12            xxi
429600070322     c                   when      xi > 18
429700070322     c                   move      'G4'          wtip
429800070322     c                   z-add     18            xxi
429900070322     c                   endsl
430000070322     c     ksp2          chain     fnsp201l
430100070322     c                   if        %found(fnsp201l)
430200070322     c                   movel     sp2est        dgir
430300070322     c                   eval      §lpcgi = gir(xi - xxi)
430400070322     c                   endif
430500070322
430600041221      *non trovata linea partenza come eccezione verifico se esiste la linea
430700041221      * 999 nel caso non devo impostare il luogo ma forzo la LNP originale
430800041221     c                   else
430900041221     c                   z-add     1             xi
431000041221     c     999           lookup    par(xi)                                55
431100041221     c                   endif
431200041221      * se esistono eccezioni 55*on forzo indicatore per non impostare
431300041221      * i riferimenti del luogo 300
431400070807      * ?dal 07/08/2007 non più valido se il luogo esiste va impostatocampo */
431500070807      * ?sempre                                                             */
431600070807     c*mm55              seton                                        21
431700041221
431800041221     c                   endif
431900041221     c                   endsr
432000070509      *____________________________________________________
432100070509     c     ecceluogo2    begsr
432200070509      *____________________________________________________
432300070509     c                   move      'PA'          wtip
432400070509     c     ksp2          chain     fnsp201l
432500070509      * rek eccezioni arrivo
432600070509     c                   if        %found(fnsp201l)
432700070509     c                   movel     sp2est        darr
432800070509     c                   z-add     1             xi                3 0
432900070509     c                   z-add     0             xxi               3 0
433000070509      * cerca con linea a video
433100070509     c     vlna          lookup    arr(xi)                                55
433200070509      * se esiste linea di arrivo cerco eventuale personalizzazione giro
433300070509     c                   if        *in55
433400070509      *reperisco il rekord x il giro
433500070509      * gli eventuali giri forzati sono per ragioni di spazio scritti su 4
433600070509      * rekord quindi x reperire quello giusto è necessario individuare su
433700070509      * quale rekord puntare basandosi sull'indice della linea di partenza
433800070509      * ogni rekord contiene 6 giri quindi per ricavare l'indice corretto
433900070509      * è necessario sottrarre all'indice gli interi precedenti
434000070509     c                   select
434100070509     c                   when      xi < 7
434200070509     c                   move      'G1'          wtip
434300070509     c                   z-add     0             xxi
434400070509     c                   when      xi < 13
434500070509     c                   move      'G2'          wtip
434600070509     c                   z-add     6             xxi
434700070509     c                   when      xi < 19
434800070509     c                   move      'G3'          wtip
434900070509     c                   z-add     12            xxi
435000070509     c                   when      xi > 18
435100070509     c                   move      'G4'          wtip
435200070509     c                   z-add     18            xxi
435300070509     c                   endsl
435400070509     c     ksp2          chain     fnsp201l
435500070509     c                   if        %found(fnsp201l)
435600070509     c                   movel     sp2est        dgir
435700070509     c                   eval      §lpcgi = gir(xi - xxi)
435800070509     c                   endif
435900070509
436000070509     c                   endif
436100070509
436200070509     c                   endif
436300070509     c                   endsr
436400911025     C*----------------------------------------------------*
436500911025     C*
436600911025     C*     R I C O N S E G N A
436700911025     C*
436800911025     C*----------------------------------------------------*
436900911025     C     RICONS        BEGSR
437000090729      * se c'è contrassegno e non è abilitato attivo contatto
437100090729     C     §3AFCA        IFNE      0
437200090729     C                   IF        arbacc <> 'S'
437300090729     C                   exsr      contatto
437400090729     C                   endif
437500090729     C                   endif
437600911112     C*-------------------------
437700911026     C* NON  C'E CAMBIO DI PORTO
437800911026     C*-------------------------
437900920116     C     VNPC          IFEQ      ' '
438000941010     C                   EXSR      RICNOC
438100911026     C                   ELSE
438200941010     C*-------------------------
438300941010     C* C'E CAMBIO DI PORTO
438400941010     C*-------------------------
438500941010     C                   EXSR      RICSIC
438600941010     C                   END
438700950112     C* SE GIACENZA VISULIZZO  DATA FINE GIAC.
438800950120     C*  E MI POSIZIONO SUL CODICE FINE GIACENZA
438900950118     C     $GIAC         IFEQ      '1'
439000950118     C                   MOVEL     '   '         VCFG
439100950118     C                   MOVE      GMAUDT        VDCG
439200950120     C                   SETON                                        23
439300950118     C                   END
439400920203     C* SE SPESE GIAC. O PROVV.C/A O NUOVO PORTO ERA O E'=A
439500940307     C* DEVO CHIEDERE SE RICH. PGM TASSAZIONE
439600920107     C* SE SI VADO IN CONSEGNA CON NUOVA BOLLA : LA LEGO
439700920203     C   31VPSG          IFEQ      'D'
439800920107     C     VPPC          OREQ      'D'
439900920107     C     VNPC          OREQ      'A'
440000920203     C     VNPC          OREQ      ' '
440100920203     C     EXPORT        ANDEQ     'A'
440200920107     C                   SETON                                        87
440300920107     C                   END
440400911025     C                   ENDSR
440500941010     C*----------------------------------------------------*
440600941010     C*     R I C O N S E G N A - NO CAMBIO PORTO
440700941010     C*----------------------------------------------------*
440800971006     C     RICNOC        BEGSR
440900941010     C*  ADDEBITO SPESE GIACENZA AL DESTINAT.
441000941010     C     VPSG          IFEQ      'D'
441100941010     C                   SETON                                        33
441200941010     C                   END
441300941010     C*
441400950727     C     WEXTB1        IFEQ      'AS'
441500950727     C                   SETON                                        31
441600950727     C                   END
441700941010     C* C/ASSEGNO DEVO VEDERE SE C'ERA E CHI PAGAVA
441800941010     C*
441900941010     C     §3AFCA        IFNE      0
442000941010     C     §3AFCA        IFEQ      1
442100960506     C                   MOVEL     §3ATB1        §FA               1
442200941010     C                   ELSE
442300941010     C                   MOVEL     §3ATB2        §FA               1
442400941010     C                   END
442500941010     C*
442600160304     C***  §FA           IFEQ      'F'
442700160304     C***  §3ATB2        ANDNE     'AP'
442800160304     C***  §3ATB2        ANDNE     '  '
442900160304     C***  §FA           orNE      'F'
443000160304     c
443100160304     C     §FA           IFNE      'F'
443200941010     C     §3ATB1        ANDNE     'AS'
443300941010     C                   SETON                                        97
443400941010     C                   END
443500941010     C*
443600941010     C                   ELSE
443700941010     C                   SETOFF                                       97
443800941010     C                   END
443900941010     C*  CALCOLO PROVV. C/ASSEGNO SE PAGA DESTINAT.
444000941010     C*                           -----------------
444100941010     C                   MOVE      *ZEROS        EXRCA
444200941010     C                   MOVE      *ZEROS        §RCA
444300941010     C                   MOVE      *ZEROS        VRCA
444400941010     C     VAPC          IFEQ      'S'
444500941010     C     VPPC          ANDEQ     'D'
444600941219     C* IMPOSTO KEY X POSIZ. SU TARIFFA
444700941010     C                   EXSR      TARPCA
444800941219     C*
444900941010     C* SE PAGAVA IL DESTINATARIO  IMPORTO VECCHIO E NUOVO
445000941010     C     *IN97         IFEQ      '1'
445100950523     C     VASS          IFNE      *ZEROS
445200950523     C                   MOVEL     'V'           WCMB              1
445300950523     C                   Z-ADD     VASS          §CAS
445400950523     C                   EXSR      CASLIR
445500950523     C*  REPERISCO IMPORTO VECCHIA VARIA G
445600950523     C                   Z-ADD     0             EXRCA
445700950523     C* SPOSTO PROVV. CONTR. SU NUOVA BOLLA
445800991001     C     §3AFCA        IFNE      0
445900991001     C     AR6DFT        IFNE      0
446000991001     C                   Z-ADD     1             YY                4 0
446100991001     C     §$2VRG        LOOKUP    ASV(YY)                                05
446200991001     C     *IN05         IFEQ      '1'
446300991001     C                   Z-ADD     AVA(YY)       EXRCA
446400991001     C                   END
446500950523     C                   ELSE
446600971203     C                   MOVEL     *ALL'9'       EXRCA
446700991001     C                   ENDIF
446800991001     C                   ENDIF
446900950523     C                   ENDIF
447000950523     C* CALCOLO DIFFERNZA ANCHE SE NON C'ERA TASSAZIONE
447100971203     C     EXRCA         IFNE      *ALL'9'
447200941010     C     VVCAP         IFEQ      VVCA
447300941010     C                   MOVEL     'V'           WCMB
447400941010     C                   ELSE
447500941010     C                   MOVEL     'N'           WCMB
447600941010     C                   END
447700950523     C     VNCA          IFNE      *ZEROS
447800941010     C                   Z-ADD     VNCA          §CAS
447900950523     C                   END
448000941010     C                   EXSR      CASLIR
448100941010     C                   EXSR      CPROVV
448200950522     C     §RCA          SUB       EXRCA         WRCA
448300950522     C                   Z-ADD     §RCA          VRCA
448400971203     C                   END
448500941010     C                   ELSE
448600941010     C* NON PAGAVA NESSUNO O PAGAVA IL MITTENTE
448700941010     C* ---------------------------------------
448800941010     C     VNCA          IFGT      0
448900941010     C                   Z-ADD     VNCA          §CAS
449000941010     C                   ELSE
449100941010     C                   Z-ADD     VASS          §CAS
449200941010     C                   END
449300941010     C     VVCAP         IFEQ      VVCA
449400941010     C                   MOVEL     'V'           WCMB
449500941010     C                   ELSE
449600941010     C                   MOVEL     'N'           WCMB
449700941010     C                   END
449800941010     C                   EXSR      CASLIR
449900941010     C                   EXSR      CPROVV
450000941010     C                   MOVE      §RCA          VRCA
450100941010     C                   END
450200950522     C     VRCA          IFNE      0
450300971006     C* Per bolla in franco + C/Ass con addebito provv. detinat.
450400971006     C* se non ho variato l'importo del C/Assegno non devo accend.
450500971006     C* indicatore per addebito provviggioni
450600971203     C                   MOVEL     §3ATB1        WFANEW            1
450700971006     C     §FA           IFEQ      'F'
450800971006     C     §3ATB2        ANDNE     'AP'
450900971006     C     §3ATB2        ANDNE     '  '
451000971006     C     VVCS          ANDNE     'S'
451100971006     C                   ELSE
451200941010     C                   SETON                                        35
451300941010     C                   END
451400971006     C                   END
451500941010     C                   END
451600941010     C*
451700941010     C*  G E S T I O N E   T I P O     B O L L A
451800941010     C*      VARIAZIONE C/ASSEGNO
451900941010     C     VVCS          IFEQ      'S'
452000941215     C                   MOVE      VNCA          §BKCAS
452100991221     C     VNCA          IFGT      0
452200941219     C                   MOVE      VVCA          §BKVCA
452300991221     C                   ELSE
452400040506     C                   MOVEL     *BLANKS       vvca
452500991221     C                   MOVEL     *BLANKS       §BKVCA
452600991221     C                   END
452700140317      * forzato codice K8 PER VARIAZIONE SOLE SE CONTRASSEGNO AGGIUNTO
452800140317      * CODICE BOLLA DA 1 A 4  O DA 2 A  6
452900140317     c                   if        vass = 0
453000140318     C                   MOVEL     'K8'          §CDVAR
453100140317     c                   else
453200140318     C                   MOVEL     'KB'          §CDVAR
453300140317     c                   endif
453400941010     C                   ELSE
453500941215     C                   MOVE      VASS          §BKCAS
453600941219     C                   MOVE      VVCAP         §BKVCA
453700941010     C                   END
453800941010     C     §CDVAR        IFEQ      'KB'
453900941010     C                   EXSR      CNTVAR
454000941010     C  N26
454100941010     CANN27              MOVEL     §CDVAR        §§KCVB
454200941010     C  N26
454300941010     CANN27              SETON                                        80
454400941010     C                   END
454500941010     C* GUARDO SE DEVO VARIARE TIPO BOLLA  PRIMA IMPOSTO COME ERA
454600941010     C                   MOVEL     §3ATB1        NWTB1
454700941010     C                   MOVEL     §3ATB2        NWTB2
454800941010     C                   MOVEL     §3AFCA        NWFCA
454900941010     C                   MOVEL     §3AFAN        NWFAN
455000941010     C                   MOVEL     §3AFTD        NWFTD
455100941010     C* CONTROLLO ANCHE SE LO POSSO FARE
455200941010     C                   MOVEL     'K8'          §CDVAR
455300941010     C                   EXSR      CNTVAR
455400941010     C* MI RITORNANO INDICATORI  26 O 27
455500941010     C*                  VARIAZ. TIPO BOLLA
455600941010     C     VVCS          IFEQ      'S'
455700941010     C*  ANNULLAMENTO DEL CONTRASSEGNO
455800941010     C     VNCA          IFEQ      *ZEROS
455900941010     C     VVCA          ANDEQ     *BLANK
456000950224     C  N26
456100950224     CANN27              MOVEL     '0'           NWFCA
456200941010     C  N26
456300941010     CANN27              MOVEL     §CDVAR        §§KCVB
456400941010     C  N26
456500941010     CANN27              SETON                                        80
456600941010     C     NWTB2         IFEQ      'A3'
456700950727     C     NWTB2         OREQ      'AP'
456800941010     C  N27              MOVEL     '  '          NWTB2
456900941010     C  N27              SETON                                        39
457000941010     C* LA 2à BOLLA NON è MODIFICABILE RICH. NOTA ACCREDITO
457100950727     C     ARBICA        IFNE      'Y'
457200991001     C     AR6TRC        ANDEQ     '2'
457300941010     C   27              SETON                                        37
457400991001     C   27              Z-ADD     AR6IMV        §INAC2
457500941010     C                   END
457600950727     C                   END
457700941010     C                   END
457800950102     C* AGGIUNTO IL CONTRASSEGNO CAMBIO CODICE BOLLA SEMPRE
457900941010     C     VNCA          IFGT      *ZEROS
458000941010     C     §3AFCA        ANDEQ     0
458100950102     C     EXPORT        IFEQ      'F'
458200941010     C     VPPC          ANDEQ     'D'
458300941010     C                   MOVEL     '2'           NWFCA
458400941010     C                   MOVEL     'AP'          NWTB2
458500941010     C                   MOVEL     §CDVAR        §§KCVB
458600941010     C                   SETON                                        8039
458700950102     C                   ELSE
458800941010     C                   MOVEL     '1'           NWFCA
458900951108     C     NWFTD         IFEQ      *BLANKS
459000941010     C                   MOVEL     *BLANK        NWTB2
459100951108     C                   END
459200941010     C                   MOVEL     §CDVAR        §§KCVB
459300941010     C                   SETON                                        80
459400941010     C                   END
459500941010     C                   END
459600941010     C                   END
459700950102     C* SE CAMBIA CHI PAGA LE SPESE E IL C/ASS
459800950102     C* ERA SU UN  FRANCO  VEDO SE CAMBIO COD.BOLLA
459900941010     C     EXPORT        IFEQ      'F'
460000941010     C     §3AFCA        IFEQ      1
460100941010     C     VPPC          ANDEQ     'D'
460200000104     C* LO TRASFORMO IN COD.BOLLA  Y PERCHE' IN CASO DI
460300000104     C* RICONSEGNA CON LA CREAZIONE DI UNA BOLLA LEGATA
460400000104     C* LA GESTIONE DEGLI INCASSI SI ASPETTA DI AVERE IL
460500000104     C* C/ASS. SULLA BOLLA MAMMA E L'UNICO TIPO BOLLA
460600000104     C* MAMMA CHE NON PREVEDE ADDEBITO DI PROVVIGIONI DI
460700000104     C* PER SE STESSA E UN F1+AP
460800000104.    C                   MOVEL     '2'           NWFCA
460900000104     C                   MOVEL     'AP'          NWTB2
461000941010     C  N26
461100941010     CANN27              MOVEL     §CDVAR        §§KCVB
461200941010     C                   SETON                                        8039
461300941010     C                   END
461400941010     C     §3AFCA        IFEQ      2
461500941010     C     §3ATB2        ANDNE     'AP'
461600941010     C     VPPC          ANDEQ     'M'
461700941010     C     §3AFCA        OREQ      2
461800941010     C     §3ATB2        ANDNE     'AP'
461900941010     C     *IN33         ANDEQ     '1'
462000971006     C     VVCS          ANDNE     'S'
462100971006     C     §3AFCA        OREQ      2
462200971006     C     §3ATB2        ANDNE     'AP'
462300971006     C     *IN33         ANDEQ     '1'
462400971006     C     VNCA          ANDNE     *ZEROS
462500941010     C     *IN27         IFEQ      '0'
462600000104     C* LO TRASFORMO IN COD.BOLLA  Y PERCHE' IN CASO DI
462700000104     C* RICONSEGNA CON LA CREAZIONE DI UNA BOLLA LEGATA
462800000104     C* LA GESTIONE DEGLI INCASSI SI ASPETTA DI AVERE IL
462900000104     C* C/ASS. SULLA BOLLA MAMMA E L'UNICO TIPO BOLLA
463000000104     C* MAMMA CHE NON PREVEDE ADDEBITO DI PROVVIGIONI DI
463100000104     C* PER SE STESSA E UN F1+AP
463200941010     C                   MOVEL     'AP'          NWTB2
463300941010     C                   MOVEL     §CDVAR        §§KCVB
463400941010     C                   SETON                                        8039
463500941010     C                   ELSE
463600941010     C* LA 2à BOLLA NON è MODIFICABILE RICH. NOTA ACCREDITO
463700971006     C     ARBICA        IFNE      'Y'
463800991001     C     AR6TRC        ANDEQ     '2'
463900971006     C                   SETON                                        37
464000991001     C                   Z-ADD     AR6IMV        §INAC2
464100941010     C                   END
464200950727     C                   END
464300971006     C* Se il Destinatario pagava le provvigioni di C/Assegno e le
464400971006     C* paga tutt'ora se non riesco a variare la tassazione chiudo
464500971006     C* la vecchia bolla con nota di accredito
464600971006     C                   ELSE
464700971006     C  N37
464800971006     CAN 35§3AFCA        IFEQ      2
464900971006     C     §3ATB2        ANDNE     'AP'
465000971006     C     VPPC          ANDEQ     'D'
465100971006     C     *IN27         ANDEQ     '1'
465200991001     C     AR6TRC        ANDEQ     '2'
465300971006     C                   SETON                                        37
465400991001     C                   Z-ADD     AR6IMV        §INAC2
465500941010     C                   END
465600971006     C                   END
465700950102     C                   END
465800941010     C* RICERCA NUOVAO TIPO BOLLA
465900941010     C     *IN80         IFEQ      '1'
466000941010     C                   MOVEL     *BLANK        NWCBL
466100941010     C                   MOVEL     *BLANK        §BKCBN
466200941010     C                   EXSR      NEWCBL
466300941010     C  N30              MOVEL     NWCBL         §BKCBN
466400941010     C                   END
466500941010     C*
466600950102     C* SE E' ASSEGNATO VEDO SE POSSO VARIARE TASSAZIONE BOLLA
466700950102     C* ===============
466800950726     C                   MOVEL     ' '           WNOSPG            1
466900950102     C* PER SPESE GIACENZA O PROVVIGIONI C/A
467000950102     C* O AZZERO VARIAG SE C/ASS. ANNULLATO
467100950102     C     EXPORT        IFEQ      'A'
467200950102     C*          -----------------
467300950726     C*  SE NON HO SPESE DI GIACENZA CONTROLLO SE LE AVEVO ADDEBITATE
467400950726     C*  NEI GIRI PRECEDNETNI X ANDARE A STORNARLE
467500950726     C     *IN33         IFEQ      '0'
467600950726     C     §$2VRI        LOOKUP    VSV                                    21
467700950726     C   21              MOVEL     'S'           WNOSPG
467800950726     C                   END
467900950102     C     *IN33         IFEQ      '1'
468000950102     C     *IN35         OREQ      '1'
468100950102     C     VPPC          OREQ      'M'
468200950726     C     WNOSPG        OREQ      'S'
468300950102     C     VVCS          OREQ      'S'
468400950102     C     VNCA          ANDEQ     0
468500950102     C     VVCA          ANDEQ     *BLANK
468600991029     C     AR6NFT        IFEQ      0
468700950102     C                   MOVEL     'T0'          §CDVAR
468800950102     C                   ELSE
468900950102     C                   MOVEL     'T2'          §CDVAR
469000950102     C                   END
469100950102     C                   EXSR      CNTVAR
469200950727     C*  SE BOLLA ORIGINALE E' UN ASSEGNATO STORNO NON POSSO VARIARE
469300950727     C*  TASSAZIONE MA DEVO FARE UNA NUOVA BOLLA
469400950727     C  N26WEXTB1        IFEQ      'AS'
469500950727     C                   SETON                                        26
469600950727     C                   END
469700950727     C  N27WEXTB2        IFEQ      'AP'
469800950727     C                   SETON                                        27
469900950727     C                   END
470000950727     C*  CONTROLLO SE ESISTE UNA FIGLIA
470100950727     C  N26
470200950727     CANN27KBOL          CHAIN     FNLBL02L                           29
470300950727     C     *IN29         IFEQ      '0'
470400991001     C     AR6NFT        IFNE      *ZEROS
470500991001     C     AR6TRC        ANDEQ     '2'
470600950727     C                   SETON                                        27
470700950727     C                   ELSE
470800950727     C                   SETON                                        26
470900950727     C                   END
471000950727     C                   END
471100950102     C  N26
471200950102     CANN27              MOVEL     §CDVAR        §§TCVB
471300950102     C  N26
471400950102     CANN27              SETON                                        82
471500950102     C                   END
471600950102     C* SISTEMO PROVV. C/A  SUBITO
471700950102     C   82              DO
471800950102     C* CARICO LE VARIE ESISTENTI X DSARBT
471900950102     C                   MOVE      AVA           VVA
472000950102     C                   MOVEL     ASV           VSV
472100950102      * CERCO SE C'è GIA VARIA G
472200950102     C                   Z-ADD     1             X
472300950102     C     §$2VRG        LOOKUP    VSV(X)                                 21
472400950102      * TROVATA VARIA G
472500950102     C     *IN21         IFEQ      *ON
472600950522     C     WRCA          ADD       VVA(X)        VVA(X)
472700950102     C     VPPC          IFEQ      'M'
472800950102     C     VVCS          OREQ      'S'
472900950102     C     VNCA          ANDEQ     0
473000950102     C     VVCA          ANDEQ     *BLANK
473100950102     C                   MOVE      *ZEROS        VVA(X)
473200950102     C                   END
473300950102     C                   ELSE
473400950726      * NON TROVATA VARIA G
473500950726     C     VRCA          IFNE      0
473600040505     C     Vnca          andgt     0
473700950102     C                   Z-ADD     1             X
473800950102     C     ' '           LOOKUP    VSV(X)                                 21
473900950102     C     *IN21         IFEQ      *ON
474000950102     C                   Z-ADD     VRCA          VVA(X)
474100950102     C                   MOVEL     §$2VRG        VSV(X)
474200950102     C                   END
474300950726     C                   END
474400950726     C*
474500950102     C                   END
474600950102     C                   ENDDO
474700950102     C                   ELSE
474800950102     C* SE NON E' ASSEGNATO VEDO SE ESISTE 2° BOLLA
474900950102     C* ===================
475000950102     C* VARIO TASSAZ, 2° BOLLA :  CON PROVV. C/A SE NON HO SPESE GIAC.
475100950102   4 C     §3AFCA        IFEQ      2
475200950102     C     §3ATB2        ANDNE     'AP'
475300950102   3 C     *IN35         IFEQ      '1'
475400950102     C     *IN33         ANDEQ     '0'
475500991001   2 C     AR6NFT        IFEQ      0
475600950102     C                   MOVEL     '20'          §CDVAR
475700950102     C                   ELSE
475800950102     C                   MOVEL     '22'          §CDVAR
475900950102   2 C                   END
476000950102     C                   EXSR      CNTVAR
476100950102   2 C     *IN27         IFEQ      '0'
476200991001     C                   MOVEL     §CDVAR        §§TCVB
476300950102     C                   SETON                                        83
476400991001     C                   MOVE      AVA           VVA
476500991001     C                   MOVEL     ASV           VSV
476600991001      * CERCO SE C'è GIA VARIA G
476700991001     C                   Z-ADD     1             X
476800991001     C     §$2VRG        LOOKUP    VSV(X)                                 21
476900991001      * TROVATA VARIA G
477000991001     C     *IN21         IFEQ      *ON
477100991001     C     WRCA          ADD       VVA(X)        VVA(X)
477200991001     C     VPPC          IFEQ      'M'
477300991001     C     VVCS          OREQ      'S'
477400991001     C     VNCA          ANDEQ     0
477500991001     C     VVCA          ANDEQ     *BLANK
477600991001     C                   MOVE      *ZEROS        VVA(X)
477700991001     C                   END
477800991001     C                   ELSE
477900991001      * NON TROVATA VARIA G
478000991001     C     VRCA          IFNE      0
478100991001     C                   Z-ADD     1             X
478200991001     C     ' '           LOOKUP    VSV(X)                                 21
478300991001     C     *IN21         IFEQ      *ON
478400991001     C                   Z-ADD     VRCA          VVA(X)
478500991001     C                   MOVEL     §$2VRG        VSV(X)
478600991001     C                   END
478700991001     C                   END
478800991001     C*
478900991001     C                   END
479000950102   2 C                   END
479100950102   3 C                   END
479200950102   4 C                   END
479300950102     C                   END
479400941010     C                   SETON                                        46
479500941010     C                   MOVEL     ARBRSD        VRSD
479600941011     C                   MOVEL     AR4RD2        VRD2
479700941010     C                   MOVEL     ARBIND        VIND
479800941010     C                   MOVEL     ARBCAD        VCAD
479900941010     C                   MOVEL     ARBPRD        VPRD
480000941010     C                   MOVEL     ARBLOD        VLOD
480100961203     C                   MOVEL     ARBNZD        VNZD
480200031119     c* imposto dati referente destinatario
480300031119     c                   movel     §ar5teld      vteld
480400031119     c                   movel     §ar5ref       vref
480500960124     C*  Salvo dati anagrafici destinatario x controllare se devo
480600060213     C*  scrivere FiAR4 x variazione
480700960124     C                   MOVEL     ARBRSD        WSVRSD
480800960124     C                   MOVEL     AR4RD2        WSVRD2
480900960124     C                   MOVEL     ARBIND        WSVIND
481000960124     C                   MOVEL     ARBCAD        WSVCAD
481100960124     C                   MOVEL     ARBPRD        WSVPRD
481200960124     C                   MOVEL     ARBLOD        WSVLOD
481300961203     C                   MOVEL     ARBNZD        WSVNZD
481400941010     C*
481500941010     C* SE FARE NUOVA BOLLA PER RECUPERO SPESE
481600941010     C*         ------------------------------
481700941010     C* SE HO PROVV. C/A O SPESE GIACENZA E NON HO VARIATO TASSAZ.
481800941010     C* DEVO FARE NUOVA BOLLA
481900941010     C     *IN33         IFEQ      '1'
482000941010     C     *IN35         OREQ      '1'
482100941010     C  N82
482200941010     CANN83              SETON                                        31
482300941010     C                   END
482400960506     C*
482500941010     C     *IN31         IFEQ      '1'
482600941010     C* CREAZ. BOLLA A3
482700941010     C                   MOVEL     'AR'          NWTB1
482800941010     C                   SETON                                        88
482900941010     C                   MOVEL     '  '          NWTB2
483000941010     C     VNCA          IFGT      0
483100941010     C     VVCS          OREQ      'N'
483200941010     C     VASS          ANDGT     0
483300941010     C                   MOVEL     '1'           NWFCA
483400941010     C                   ELSE
483500941010     C                   MOVEL     '0'           NWFCA
483600941010     C                   END
483700941010     C                   MOVEL     '0'           NWFAN
483800941010     C                   MOVEL     '0'           NWFTD
483900941010     C                   MOVEL     *BLANK        NWCBL
484000941010     C                   MOVEL     *BLANK        §LPCBO
484100941010     C                   EXSR      NEWCBL
484200941010     C  N30              MOVEL     NWCBL         §LPCBO
484300941010     C*  LP
484400941010     C                   MOVE      ARBLNA        §LPLNP
484500941010     C* STAMPA SEGNACOLLI  - C. TRATT. MERCE
484600941010     C                   MOVEL     '4 '          §LPCTM
484700941010     C                   MOVE      ARBNCD        §LPNCD
484800941010     C                   MOVE      ARBNCA        §LPNCA
484900941010     C* CONTRASSEGNO
485000941010     C     VNCA          IFGT      0
485100941010     C                   Z-ADD     VNCA          §LPCAS
485200941010     C                   MOVEL     VVCA          §LPVCA
485300991221     C     §LPCAS        IFEQ      0
485400991221     C                   MOVEL     *BLANKS       §LPVCA
485500991221     C                   END
485600941010     C                   MOVEL     AR9TIC        §LPTIC
485700941010     C                   ELSE
485800941010     C     VASS          IFGT      0
485900941010     C     VVCS          ANDEQ     'N'
486000941010     C                   Z-ADD     VASS          §LPCAS
486100000103     C                   MOVEL     VVCAP         §LPVCA
486200991221     C     §LPCAS        IFEQ      0
486300991221     C                   MOVEL     *BLANKS       §LPVCA
486400991221     C                   END
486500941010     C                   MOVEL     AR9TIC        §LPTIC
486600941010     C                   END
486700941010     C                   END
486800941010     C* IMPOSTO §LPNOT
486900941010     C                   EXSR      XLPNOT
487000941010     C*SE ASSEGNATO E RICH. NOTA ACCREDITO IMPOSTO I VALORI VECCH.BOLLA
487100950727     C     EXPORT        IFEQ      'A'
487200950727     C     *IN26         ANDEQ     '1'
487300950727     C     WEXTB1        OREQ      'AS'
487400941010     C                   MOVE      AR6POR        §LPPOR
487500990930      * RIPORTO LE VARIE DELLA VECCHIA BOLLA X DTASV
487600941216     C                   MOVE      AVA           NVA
487700941216     C                   MOVEL     ASV           NSV
487800950522     C                   Z-ADD     1             YY                4 0
487900991001     C* Tolgo Iva e bollo dalle nuove varie
488000991001     C                   Z-ADD     1             YY                4 0
488100991001     C     §$2BOL        LOOKUP    NSV(YY)                                05
488200991001     C     *IN05         IFEQ      '1'
488300991001     C                   MOVEL     *BLANKS       NSV(YY)
488400991001     C                   Z-ADD     0             NVA(YY)
488500991001     C                   END
488600991001     C                   Z-ADD     1             YY                4 0
488700991001     C     §$2IVA        LOOKUP    NSV(YY)                                05
488800991001     C     *IN05         IFEQ      '1'
488900991001     C                   MOVEL     *BLANKS       NSV(YY)
489000991001     C                   Z-ADD     0             NVA(YY)
489100991001     C                   END
489200991001     C     §$2VRG        LOOKUP    NSV(YY)                                05
489300950522     C     *IN05         IFEQ      '1'
489400950508     C     NWFCA         IFEQ      '0'
489500950508     C                   MOVEL     *BLANKS       NSV(YY)
489600950508     C                   Z-ADD     0             NVA(YY)
489700950522     C                   ELSE
489800950522      * SE NON HO CALCOLATO LE PROVVIGIONI C/ASS. MA LE DEVE PAGARE IL
489900950522      * DESTINATARIO MI CARICO L'IMPORTO DALLA VECCHIA BOLLA
490000950522     C     VRCA          IFEQ      0
490100950522     C     VPPC          ANDEQ     'D'
490200950522     C                   Z-ADD     NVA(YY)       VRCA
490300950522     C                   END
490400950522     C                   END
490500950508     C                   END
490600941010     C* RICH. NOTA ACCR. SOLO SE FIL.ARRIVO
490700941010     C     AR6FIV        IFEQ      ARBLNA
490800950727     C     ARBICA        ANDNE     'Y'
490900941010     C                   Z-ADD     AR6IMV        §INAC1
491000941010     C                   SETON                                        32
491100941010     C                   END
491200941010     C                   END
491300941010     C                   END
491400941010     C                   ENDSR
491500941010     C*----------------------------------------------------*
491600941010     C*     R I C O N S E G N A - SI CAMBIO PORTO
491700941010     C*----------------------------------------------------*
491800941010     C     RICSIC        BEGSR
491900941010     C*  ADDEBITO SPESE GIACENZA AL DESTINAT.
492000941010     C     VPSG          IFEQ      'D'
492100941010     C                   SETON                                        33
492200941010     C                   SETON                                        31
492300941010     C                   END
492400941010     C*  CALCOLO PROVV. C/ASSEGNO SE PAGA DESTINAT.
492500941010     C     VAPC          IFEQ      'S'
492600941010     C     VPPC          ANDEQ     'D'
492700941010     C                   EXSR      TARPCA
492800941010     C* VARIATO C/ASS.
492900941010     C     VVCS          IFEQ      'S'
493000941010     C                   Z-ADD     VNCA          §CAS
493100941010     C                   ELSE
493200941010     C                   Z-ADD     VASS          §CAS
493300941010     C                   END
493400941010     C     VVCAP         IFEQ      VVCA
493500941010     C                   MOVEL     'V'           WCMB
493600941010     C                   ELSE
493700941010     C                   MOVEL     'N'           WCMB
493800941010     C                   END
493900941010     C                   EXSR      CASLIR
494000941010     C     §ASS          IFNE      *ZEROS
494100941010     C                   EXSR      CPROVV
494200941010     C                   MOVE      §RCA          VRCA
494300941010     C     VRCA          IFGT      0
494400941010     C                   SETON                                        35
494500941010     C                   SETON                                        31
494600941010     C                   END
494700941010     C                   END
494800941010     C                   END
494900941010     C* RICHIEDERE IL TRASPORTO SE NUOVO PORTO = A
495000941010     C     VNPC          IFEQ      'A'
495100060118     C**mm               SETON                                        36
495200941010     C                   SETON                                        31
495300941010     C                   END
495400941010     C* VARIATO C/ASS.    SEMPRE VARIAZ. X GEST. C/A
495500941010     C     VVCS          IFEQ      'S'
495600941215     C                   MOVE      VNCA          §BKCAS
495700941219     C                   MOVE      VVCA          §BKVCA
495800991221     C     VNCA          IFEQ      0
495900991221     C                   MOVEL     *BLANKS       §BKVCA
496000040205     C                   MOVEL     *BLANKS       vvca
496100991221     C                   END
496200941010     C                   MOVEL     'KB'          §CDVAR
496300941010     C                   ELSE
496400941215     C                   MOVE      VASS          §BKCAS
496500941219     C                   MOVE      VVCAP         §BKVCA
496600941010     C                   END
496700941010     C     §CDVAR        IFEQ      'KB'
496800941010     C                   EXSR      CNTVAR
496900941010     C  N26
497000941010     CANN27              MOVEL     §CDVAR        §§KCVB
497100941010     C  N26
497200941010     CANN27              SETON                                        80
497300941010     C                   END
497400941010     C* GUARDO SE DEVO VARIARE TIPO BOLLA  PRIMA IMPOSTO COME ERA
497500941010     C                   MOVEL     §3ATB1        NWTB1
497600941010     C                   MOVEL     §3ATB2        NWTB2
497700941010     C                   MOVEL     §3AFCA        NWFCA
497800941010     C                   MOVEL     §3AFAN        NWFAN
497900941010     C                   MOVEL     §3AFTD        NWFTD
498000941010     C* CONTROLLO ANCHE SE LO POSSO FARE
498100941010     C                   MOVEL     'K8'          §CDVAR
498200941010     C                   EXSR      CNTVAR
498300941010     C* MI RITORNANO GLI INDICATORI 26 O 27
498400941010     C*                  VARIAZ. TIPO BOLLA
498500941010     C     VVCS          IFEQ      'S'
498600941010     C*  ANNULLAMENTO DEL CONTRASSEGNO
498700941010     C     VNCA          IFEQ      *ZEROS
498800941010     C     VVCA          ANDEQ     *BLANK
498900950727     C  N26
499000950727     CANN27              MOVEL     '0'           NWFCA
499100941010     C  N26
499200941010     CANN27              MOVEL     §CDVAR        §§KCVB
499300941010     C  N26
499400941010     CANN27              SETON                                        80
499500941010     C     NWTB2         IFEQ      'A3'
499600950727     C     NWTB2         OREQ      'AP'
499700950727     C  N27              MOVEL     '  '          NWTB2
499800941010     C  N27              SETON                                        8039
499900941010     C* LA 2à BOLLA NON è MODIFICABILE RICH. NOTA ACCREDITO
500000950727     C     ARBICA        IFNE      'Y'
500100991001     C     AR6TRC        ANDEQ     '2'
500200941010     C   27              SETON                                        37
500300991001     C   27              Z-ADD     AR6IMV        §INAC2
500400941010     C                   END
500500950727     C                   END
500600941010     C                   END
500700941010     C*  AGGIUNTO CONTRASSEGNO
500800941010     C     VNCA          IFGT      *ZEROS
500900941010     C     §3AFCA        ANDEQ     0
501000950102     C                   MOVEL     '1'           NWFCA
501100941010     C  N26
501200941010     CANN27              MOVEL     §CDVAR        §§KCVB
501300941010     C  N26
501400941010     CANN27              SETON                                        80
501500941010     C                   END
501600941010     C                   END
501700941010     C     *IN26         IFEQ      '1'
501800941010     C     AR6FIV        ANDEQ     ARBLNA
501900950727     C     ARBICA        ANDNE     'Y'
502000941010     C                   SETON                                        32
502100941010     C                   Z-ADD     AR6IMV        §INAC1
502200941010     C                   END
502300941010     C* SE NON DEVO FARE NOTE ACCREDITO DEVO TRASFORMARE
502400941010     C*  LA VECCHIA BOLLA IN C/S
502500941010     C  N32EXPORT        IFEQ      'A'
502600950727     C     WEXTB1        ANDNE     'AS'
502700941010     C                   MOVEL     'AS'          NWTB1
502800941010     C  N26
502900941010     CANN27              MOVEL     §CDVAR        §§KCVB
503000941010     C                   SETON                                        3980
503100941010     C                   END
503200941010     C     EXPORT        IFEQ      'F'
503300941010     C                   MOVEL     'FS'          NWTB1
503400941010     C* SE DEVO FARE NUOVA BOLLA E C'ERA A3 LA STORNO
503500971006     C   31§3ATB2        IFEQ      'A3'
503600950727     C  N27              MOVEL     'AP'          NWTB2
503700950727     C     ARBICA        IFNE      'Y'
503800991001     C     AR6TRC        ANDEQ     '2'
503900941010     C   27              SETON                                        37
504000991001     C   27              Z-ADD     AR6IMV        §INAC2
504100941010     C                   END
504200950727     C                   END
504300941010     C  N26
504400941010     CANN27              MOVEL     §CDVAR        §§KCVB
504500941010     C  N26
504600941010     CANN27              SETON                                        3980
504700941010     C                   END
504800941010     C     *IN80         IFEQ      '1'
504900941010     C                   MOVEL     *BLANK        NWCBL
505000941010     C                   MOVEL     *BLANK        §BKCBN
505100941010     C                   EXSR      NEWCBL
505200941010     C  N30              MOVEL     NWCBL         §BKCBN
505300941010     C                   END
505400950727     C*  SE NON HO SPESE DI GIACENZA CONTROLLO SE LE AVEVO ADDEBITATE
505500950727     C*  NEI GIRI PRECEDNETNI X ANDARE A STORNARLE
505600950727     C                   MOVEL     ' '           WNOSPG            1
505700950727     C     *IN33         IFEQ      '0'
505800950727     C     §$2VRI        LOOKUP    VSV                                    21
505900950727     C   21              MOVEL     'S'           WNOSPG
506000950727     C                   END
506100941010     C*  AGGIUNTO CONTRASSEGNO  FORZO VARIAG=0 SU VECCHIA BOLLA
506200941010     C     VNCA          IFGT      *ZEROS
506300941010     C     §3AFCA        ANDEQ     0
506400941010     C     EXPORT        ANDEQ     'A'
506500950727     C     EXPORT        OREQ      'A'
506600950727     C     WNOSPG        ANDEQ     'S'
506700941010     C     AR6NFT        IFEQ      0
506800941010     C                   MOVEL     'T0'          §CDVAR
506900941010     C                   ELSE
507000941010     C                   MOVEL     'T2'          §CDVAR
507100941010     C                   END
507200941010     C                   EXSR      CNTVAR
507300950727     C*  SE BOLLA ORIGINALE E' UN ASSEGNATO STORNO NON POSSO VARIARE
507400950727     C*  TASSAZIONE MA DEVO FARE UNA NUOVA BOLLA
507500950727     C  N26WEXTB1        IFEQ      'AS'
507600950727     C                   SETON                                        26
507700950727     C                   END
507800950727     C  N27WEXTB2        IFEQ      'AP'
507900950727     C                   SETON                                        27
508000950727     C                   END
508100950727     C*  CONTROLLO SE ESISTE UNA FIGLIA
508200950727     C  N26
508300950727     CANN27KBOL          CHAIN     FNLBL02L                           29
508400950727     C     *IN29         IFEQ      '0'
508500991001     C     AR6NFT        IFNE      *ZEROS
508600991001     C     AR6TRC        ANDEQ     '2'
508700950727     C                   SETON                                        27
508800950727     C                   ELSE
508900950727     C                   SETON                                        26
509000950727     C                   END
509100950727     C                   END
509200941010     C  N26
509300941010     CANN27              MOVEL     §CDVAR        §§TCVB
509400941010     C  N26
509500941010     CANN27              SETON                                        82
509600941010     C*
509700941010     C   82              DO
509800941216     C* CARICO LE VARIE ESISTENTI X DSARBT
509900941216     C                   MOVE      AVA           VVA
510000941216     C                   MOVEL     ASV           VSV
510100941010      * CERCO SE C'è GIA VARIA G
510200941216     C                   MOVEL     §$2VRI        §SIGLA            1
510300941216     C                   MOVEL     VSV           WSV
510400941216      * CERCO L'INDICE SU CUI LAVORARE
510500941216     C                   EXSR      CHKVAR
510600941216     C                   Z-ADD     0             VVA(XS)
510700941216     C                   MOVEL     §$2VRI        VSV(XS)
510800941216     C                   END
510900941010     C                   END
511000941010     C* IMPOSTO I CAMPI X NUOVA BOLLA
511100941010     C* -----------------------------
511200941010     C* CREAZ. BOLLA AR
511300941010     C     *IN31         IFEQ      '1'
511400941010     C                   MOVEL     'AR'          NWTB1
511500941010     C                   SETON                                        88
511600941010     C                   ELSE
511700941010     C* SE NUOVO  PORTO = F :
511800941010     C     VNPC          IFEQ      'F'
511900941010     C*              SE NO GIACENZE  CREO BOLLA LEGATA SEMPRE
512000941010     C     $GIAC         IFEQ      '0'
512100941010     C                   SETON                                        8831
512200941010     C                   MOVEL     'FC'          NWTB1
512300941010     C                   ELSE
512400941010     C*              SE SI GIACENZE  CREO BOLLA LEGATA X N.ACCR.
512500941010     C     *IN32         IFEQ      '1'
512600941010     C     *IN37         OREQ      '1'
512700941010     C                   SETON                                        8831
512800941010     C                   MOVEL     'FC'          NWTB1
512900941010     C                   END
513000941010     C                   END
513100941010     C                   END
513200941010     C                   END
513300941010     C                   MOVEL     '  '          NWTB2
513400941010     C     VNCA          IFGT      0
513500941010     C     VVCS          OREQ      'N'
513600941010     C     VASS          ANDGT     0
513700941010     C                   MOVEL     '1'           NWFCA
513800941010     C                   ELSE
513900941010     C                   MOVEL     '0'           NWFCA
514000941010     C                   END
514100941010     C                   MOVEL     '0'           NWFAN
514200941010     C                   MOVEL     '0'           NWFTD
514300941010     C                   MOVEL     *BLANK        NWCBL
514400941010     C                   MOVEL     *BLANK        §LPCBO
514500941010     C                   EXSR      NEWCBL
514600941010     C  N30              MOVEL     NWCBL         §LPCBO
514700941010     C*
514800941010     C                   SETON                                        46
514900941010     C                   MOVEL     ARBRSD        VRSD
515000941011     C                   MOVEL     AR4RD2        VRD2
515100941010     C                   MOVEL     ARBIND        VIND
515200941010     C                   MOVEL     ARBCAD        VCAD
515300941010     C                   MOVEL     ARBPRD        VPRD
515400941010     C                   MOVEL     ARBLOD        VLOD
515500961203     C                   MOVEL     ARBNZD        VNZD
515600031119     c* imposto dati referente destinatario
515700031119     c                   movel     §ar5teld      vteld
515800031119     c                   movel     §ar5ref       vref
515900960124     C*  Salvo dati anagrafici destinatario x controllare se devo
516000060213     C*  scrivere FiAR4 x variazione
516100960124     C                   MOVEL     ARBRSD        WSVRSD
516200960124     C                   MOVEL     AR4RD2        WSVRD2
516300960124     C                   MOVEL     ARBIND        WSVIND
516400960124     C                   MOVEL     ARBCAD        WSVCAD
516500960124     C                   MOVEL     ARBPRD        WSVPRD
516600960124     C                   MOVEL     ARBLOD        WSVLOD
516700961203     C                   MOVEL     ARBNZD        WSVNZD
516800941010     C*  LP
516900941010     C                   MOVE      ARBLNA        §LPLNP
517000941010     C* C/ASS
517100941010     C     VNCA          IFGT      0
517200941010     C                   Z-ADD     VNCA          §LPCAS
517300941010     C                   MOVEL     VVCA          §LPVCA
517400991221     C     §LPCAS        IFEQ      0
517500991221     C                   MOVEL     *BLANKS       §LPVCA
517600991221     C                   END
517700941010     C                   MOVEL     AR9TIC        §LPTIC
517800941010     C                   ELSE
517900941010     C     VVCS          IFEQ      'N'
518000941010     C     VASS          ANDGT     0
518100941010     C                   Z-ADD     VASS          §LPCAS
518200941010     C                   MOVEL     VVCAP         §LPVCA
518300941010C    C                   MOVEL     AR9TIC        §LPTIC
518400941010     C                   END
518500941010     C                   END
518600941010     C*
518700941010     C                   MOVEL     '4 '          §LPCTM
518800941010     C                   MOVE      ARBNCD        §LPNCD
518900941010     C                   MOVE      ARBNCA        §LPNCA
519000941010     C* IMPOSTO §LPNOT
519100941010     C                   EXSR      XLPNOT
519200941010     C                   ENDSR
519300911026     C*----------------------------------------------------*
519400911026     C*
519500940310     C* DIROT1 - DIROTTAMENTO 1 OPERAZ. DA FARE
519600911026     C*
519700911026     C*----------------------------------------------------*
519800940310     C     DIROT1        BEGSR
519900911028     C*  NON PREVISTO ADDEBITO SPESE GIACENZA AL DESTINAT.
520000911028     C*
520100941217     C* DAL NUOVO RELEASE FEBR 95:
520200941217     C* SI DIROTTA SOLO SE LA LINEA ARRIVO è DIVERSA
520300941217     C*
520400911028     C* DEVO RICH. LNA E DESTINATARIO NUOVO
520500911028     C                   SETON                                        46
520600920522     C* PROPONGO VECCHIO DESTINATARIO SENZA IL CAP PER I
520700920522     C* DIROTTAMENTI NON DA GIACENZE
520800940310     C     $GIAC         IFEQ      '0'
520900920522     C                   MOVEL     ARBRSD        VRSD
521000941011     C                   MOVEL     AR4RD2        VRD2
521100920522     C                   MOVEL     ARBIND        VIND
521200920522     C                   MOVEL     ARBPRD        VPRD
521300920522     C                   MOVEL     ARBLOD        VLOD
521400940929     C                   MOVEL     ARBNZD        VNZD
521500031119     c* imposto dati referente destinatario
521600031119     c                   movel     §ar5teld      vteld
521700031119     c                   movel     §ar5ref       vref
521800920522     C                   END
521900960124     C*  Salvo dati anagrafici destinatario x controllare se devo
522000060213     C*  scrivere FiAR4 x variazione
522100960124     C                   MOVEL     ARBRSD        WSVRSD
522200960124     C                   MOVEL     AR4RD2        WSVRD2
522300960124     C                   MOVEL     ARBIND        WSVIND
522400960124     C                   MOVEL     ARBCAD        WSVCAD
522500960124     C                   MOVEL     ARBPRD        WSVPRD
522600960124     C                   MOVEL     ARBLOD        WSVLOD
522700960124     C                   MOVE      ARBNZD        WSVNZD
522800911113     C* IMPOSTO IL CODICE FINE GIACENZA
522900941217     C     $GIAC         IFEQ      '1'
523000911113     C                   MOVEL     '006'         VCFG
523100911114     C                   MOVEL     '2C'          TBLCOD
523200911114     C                   MOVEL     *BLANK        TBLKEY
523300911114     C                   MOVEL     VCFG          TBLKEY
523400911114     C     KTAB          CHAIN     TABEL                              21
523500911114     C   21              MOVEL     *ALL'?'       DCFG
523600060410     C  N21              MOVEL     TBLUNI        Ds2c
523700060410     C  N21              MOVEL     §2cdes        DCFG
523800941229     C                   MOVE      GMAUDT        VDCG
523900941217     C                   ENDIF
524000911026     C                   ENDSR
524100940929      *--------------------------------------------------------
524200940929      * C A S L I R  TRASFORMO C/ASSEGNO IN LIRE
524300940929      *------------
524400940929      * VIENE PASSATO §CAS : IMPORTO CONTRASSEGNO 13,3
524500940929      *               WCMB : SE=V DEVO PRENDERE CAMBIO SULLA BOLLA
524600940929      *                      SE=N è CAMBIATA LA DIVISA CERCO NUOVO CAMBIO
524700940929      * VIENE RESTITUITO §ASS : IMPORTO CONTRASSEGNO IN LIRE 13,0
524800940929      *--------------------------------------------------------
524900940929     C     CASLIR        BEGSR
525000011106      * RICHIAMO YEURCO SE LA DIVISA E' CAMBIATA PER APPLICARE
525100011106      * IL CAMBIO CORRETTO
525200011121     C     VVCA          IFNE      *BLANK
525300011121     C     VVCA          ANDNE     VVCAP
525400040206      *chiodissimo per divise a blank
525500040206     c                   if        vvcap = *blank
525600040206     c                   move      'EUR'         vvcap
525700040206     c                   endif
525800011106     C                   CLEAR                   YEURDS
525900011106     C                   MOVEL     VVCAP         YECDVI
526000011106     C                   Z-ADD     §CAS          YECIMI
526100011106     C                   MOVEL     VVCA          YECDVO
526200011106     C                   MOVEL     '3'           YECDCO
526300011106     C                   MOVEL     'H'           YECTAR
526400011106     C                   CALL      'YEURCO'
526500011106     C                   PARM                    YEURDS
526600011106     C                   Z-ADD     YECIMO        §CAS
526700011106     C                   END
526800011106      * RICHIAMO YEURCO SEMPRE PER TRADURRE L'IMPORTO IN MONETA DI
526900011106      * CONTO GESTIONALE
527000011106     C                   CLEAR                   YEURDS
527100011106     C                   MOVEL     VVCA          YECDVI
527200011106     C                   Z-ADD     §CAS          YECIMI
527300011106     C                   MOVEL     §GEDCN        YECDVO
527400011106     C                   MOVEL     '3'           YECDCO
527500011106     C                   MOVEL     'H'           YECTAR
527600011106     C                   CALL      'YEURCO'
527700011106     C                   PARM                    YEURDS
527800011106     C                   Z-ADD     YECIMO        §ASS
527900940929     C                   ENDSR
528000940308     C/EJECT
528100940308     C*?
528200940308     C*?    A G G I O R N A M E N T I
528300940308     C*?
528400940303      *--------------------------------------------------------
528500940303     C* S R A G G  -AGGIORNAMENTI  ARCHIVI
528600940303      *--------------------------------------------------------
528700940303     C     SRAGG         BEGSR
528800940303      *--------------------------------------------------------
528900940303     C* ADDEBITO SPESE E PASSAGGIO DATI PER CREAZ. BOLLA
529000940303      *--------------------------------------------------------
529100951214     C     *IN31         IFEQ      '1'
529200951214     C                   EXSR      PGNBLP
529300951214     C                   Z-ADD     §LPAAS        WSVAAS
529400951214     C                   Z-ADD     §LPLNP        WSVLNP
529500951214     C                   Z-ADD     §LPNRS        WSVNRS
529600951214     C                   Z-ADD     §LPNSP        WSVNSP
529700951214     C                   Z-ADD     §LPLNA        WSVLNA
529800940303     C* SE NON HO CREATO LA BOLLA ERRORE
529900951214     C     §LPNSP        IFEQ      0
530000940307     C                   SETON                                            99
530100940307     C   99              GOTO      ENDAGG
530200940307     C                   END
530300951214     C                   END
530400940303      *--------------------------------------------------------
530500940303     C* AGGIORNAMENTO VECCHIA BOLLA
530600940303      *--------------------------------------------------------
530700141210     c     rileggi       tag
530800141210     C     KBOL          CHAIN(e)  FNARB01L                           43
530900141210     c                   if        %error
531000141210     C     Kbol          CHAIN(n)  fnarb01l
531100141210     c                   exsr      geslckARB
531200141210     c   43              goto      rileggi
531300150129     C  n43KBOL          CHAIN     FNARB01L                           43
531400141210     c                   endif
531500941003     C     *IN43         IFEQ      '0'
531600960124      *--------------------------------------------------------
531700110427      * verifiche per resi
531800110427     C     VFRI          IFeq      'R'
531900030128      *controllo se la spedizione gestisce i bancali
532000030128      * se sono gestiti reperisco da fiar5 i rekord da scrivere per
532100030128      * legarli alla bolla di reso
532200030128     C                   clear                   dar5ban
532300030805     C                   move      'BAN'         trcar5
532400051013     C     Kfiar5        CHAIN(n)  FiAR501L                           43
532500030128     C     *IN43         IFEQ      '0'
532600030128     c                   clear                   wfmgb000
532700030128     C                   MOVEL     ar5uni        dar5ban
532800030128     c                   z-add     amgudt        wfmdat
532900030128     c                   movel     whhudt        wfmora
533000030128     c                   movel     knmus         wfmute
533100030128     c                   movel     simpou        wfmpou
533200030128     c                   movel(p)  'FNLG20R'     wfmpgm
533300030213     c                   movel     'REME'        wfmcau
533400030218     c                   Clear                   wfmsmc
533500030128     c                   movel     arbnsp        wfmrdo
533600030128     c                   movel     arbaas        wfmddo
533700030128     c                   move      arbmgs        wfmddo
533800030128      *  TIPO PORTO
533900030128     C                   movel     '3A'          tblcod
534000030128     C                   movel(P)  ARBcbo        tblkey
534100060807     C     ktab          chain     TABEL
534200030128     C                   movel     TBLUNI        DS3A
534300030128     C                   movel     §3ATB1        §DCTport          1
534400030128      *verifico il porto per codice cliente
534500030128     c                   if        §dctport = 'F'
534600030218     c                   movel     '0'           wfmsog
534700030218     c                   move      arbksc        wfmsog
534800030128     c                   else
534900030128     c                   if        arbccm > 0
535000030218     c                   movel     '0'           wfmsog
535100030218     c                   move      arbccm        wfmsog
535200030128     c                   else
535300030128     c                   move      *all'0'       arblna4           4
535400030128     c                   move      arblna        arblna4
535500030218     c                   eval      wfmsog  = arblna4 + '8888'
535600030128     c                   endif
535700030128     c                   endif
535800030128      *primo rekord se deve essere scritto
535900030128     c                   if        §ar5tba > 0
536000030128     c                   movel     §ar5tba       wfmcod
536100030128     c                   z-add     §ar5nba       wfmqta
536200030128     c                   write     wfmgb000
536300030128     c                   endif
536400030128      *
536500030128      *secondo rekord se deve essere scritto
536600030128     c                   if        §ar5tb2 > 0
536700030128     c                   movel     §ar5tb2       wfmcod
536800030128     c                   z-add     §ar5nb2       wfmqta
536900030128     c                   write     wfmgb000
537000030128     c                   endif
537100030128      *
537200030128     c                   endif
537300000214     C                   END
537400071025      * DA FUORI GIACENZE
537500071017      * scrive rekord spia per resi e dirottamenti per poterli stampare
537600071017      * in seguito abbinando bolla mamma e figlia
537700071025     c                   if        not *in90
537800071017     c                   if        vfri = 'D' or vfri = 'R'
537900071017     c                   move      vfri          arbtpl
538000071017     C                   Z-ADD     0             ARBDST
538100071017     C                   Z-ADD     FILARR        ARBFGS
538200071017     C                   MOVE      AMGUDT        ARBDDC
538300071017     C                   MOVE      *ALL'9'       ARBNDC
538400071017     c                   write     fnarbx00
538500071017     c                   clear                   arbtpl
538600071017     c                   endif
538700071025     c                   endif
538800960124      *--------------------------------------------------------
538900950331     C                   MOVEL     ARBCBO        EXCBO             2
539000941003     C* SE HO GIà RICH. NOTA ACCRED. NON DEVO STAMPARE RICH.
539100941005     C     ARBFIP        IFEQ      'C'
539200941003     C                   MOVEL     ARBFIP        EXNAC             1
539300941003     C                   END
539400941003     C   32
539500941003     COR 37              MOVEL     'C'           ARBFIP
539600941003     C   32
539700941003     COR 37              MOVEL     'S'           ARBFNC
539800941003     C* SE HO RICH.NOTA ACCRED.  O TRASF. IN C/S
539900950726     C     *IN32         IFEQ      '1'
540000950726     C     *IN37         OREQ      '1'
540100950726     C     *IN39         OREQ      '1'
540200950726     C                   MOVEL     'Y'           ARBICA
540300950726     C                   MOVEL     'S'           ARBACA
540400950726     C                   END
540500941003     C     VFRI          IFEQ      'R'
540600960327     C*  Se reso controllo se per avaria
540700960327     C     V4CRCS        IFEQ      'N'
540800941003     C                   MOVEL     '2'           ARBCCA
540900960327     C                   ELSE
541000960327     C                   MOVEL     '6'           ARBCCA
541100941003     C                   END
541200960327     C                   END
541300941003     C     VFRI          IFEQ      'D'
541400941003     C                   MOVEL     '1'           ARBCCA
541500941003     C                   END
541600941003     C     VFRI          IFEQ      'C'
541700941003     C     VNPC          ANDNE     ' '
541800941003     C                   MOVEL     '9'           ARBCCA
541900941003     C                   END
542000940303     C*  --------------------------->  HO FATTO NUOVA BOLLA
542100940303      * =======================================================
542200941229      * DO PER CONSEGNATA LA MAMMA - X RESO E DIROTTAMENTO
542300940303      * =======================================================
542400940303     C     VFRI          IFEQ      'R'
542500940303     C     VFRI          OREQ      'D'
542600941216     C     VLNA          ANDNE     EXLNA
542700950214     C* Se non c'è data arrivo forzo = data consegna
542800950214     C     ARBDAM        IFEQ      *ZEROS
542900950214     C                   Z-ADD     AMGUDT        ARBDAM
543000950214     C     ARBDTI        IFEQ      *ZEROS
543100950214     C                   Z-ADD     AMGUDT        ARBDTI
543200950214     C                   Z-ADD     ORAMIN        ARBHTI
543300950214     C                   END
543400950214     C                   END
543500940303      *
543600940303     C                   MOVEL     'Y'           ARBICC
543700950725     C                   MOVEL     'S'           ARBACC
543800940303     C                   MOVEL     'Y'           ARBICA
543900950725     C                   MOVEL     'S'           ARBACA
544000940303     C                   MOVE      AMGUDT        ARBDCM
544100940930     C                   MOVEL     ORAMIN        ARBHMC
544200940303     C                   MOVE      *BLANK        ARBFT1
544300941006     C                   MOVE      *ZEROS        ARBDT1
544400091221     c                   clear                   arbpdc
544500021009     C                   MOVEL     FILarr        ARBPDC
544600091221     C                   MOVE      0998          ARBPDC
544700940303     C                   MOVE      *ALL'9'       ARBNDC
544800941003      * LA DATA DELLA DISTINTA è LA DATA DEL DIROTTAMENTO
544900940303     C                   MOVE      AMGUDT        ARBDDC
545000940303     C                   MOVEL     'S'           ARBFDC
545100940303     C                   MOVE      *ALL'9'       ARBNMI
545200940303     C                   MOVE      AMGUDT        ARBDMI
545300940303     C                   MOVE      *ALL'9'       ARBCUC
545400021009     c                   z-add     filarr        arbifp
545500020813     c                   z-add     simpou        arbipp
545600940303     C                   MOVEL     *BLANK        ARBFBC
545700950104     C                   MOVEL     *BLANK        ARBCMC
545800970204     C* SE DIROTTAMENTO AD ALTRA FILIALE IMPOSTO ARBDT3 A 99999999
545900970204     C* E ARBFT3 A T
546000970204     C     VFRI          IFEQ      'D'
546100970204     C                   MOVEL     'T'           ARBFT3
546200970204     C                   Z-ADD     99999999      ARBDT3
546300970204     C                   END
546400940303      *
546500941011     C* DETT.SEGNACOLLI E ANOMALIE NELLO STORICO
546600940303     C                   EXSR      AGGART
546700940303      *
546800941229     C                   UPDATE    FNARB000
546900070320      * se impostato flag abilitazione in anagrafica verifico se esistono
547000070321      * dei blocchi nel caso updato il rekord dopo aver chiuso giacenza
547100070320     c                   if        §oggeot = 'S'
547200070321     c                   exsr      elibloc
547300070320     c                   endif
547400941229     C                   ELSE
547500940303      * ========================================================
547600940303      * IN TUTTI GLI ALTRI CASI NON STORICIZZO LA MAMMA
547700940303      * ========================================================
547800941229     C* SE HO CREATO LA BOLLA IN ARRIVO VADO IN CONSEGNA CON LA NUOVA
547900941229     C     *IN88         IFEQ      '1'
548000941229     C* BLOCCO LA VECCHIA BOLLA :
548100941229     C* SE NON C'E' GIACENZA  PERCHE' NON ESCA
548200941229     C* NELLA PRELISTA BOLLE DA CONSEGNARE
548300941003     C     $GIAC         IFEQ      '0'
548400940310     C                   MOVEL     '*'           ARBFBC
548500941229     C* SE C'E' GIACENZA SE NON E' BLOCCATA LA BLOCCO
548600941229     C                   ELSE
548700941229     C                   MOVEL     'G'           ARBFBC
548800940310     C                   END
548900940308     C* ABILITO LA VECCHIA BOLLA  PERCHE' NON ESCA  DA TASSARE     ££££
549000940308     C                   MOVEL     'S'           ARBACC
549100940308     C                   MOVEL     'S'           ARBACA
549200941229     C                   UPDATE    FNARB000
549300941229     C                   ELSE
549400941229      *                    ----
549500940303     C* SE NON HO CREATO NUOVA BOLLA X ANDARE IN CONSEGNA
549600941229     C* DEVO ABILITARE LA VECCHIA SE NON HA FIGLIE
549700941229     C     KBOL          CHAIN     FNLBL02L                           21
549800941229     C     *IN21         IFEQ      *ON
549900941003     C                   MOVEL     *BLANK        ARBFBC
550000941229     C                   UPDATE    FNARB000
550100941229     C                   ELSE
550200941229     C* SE HA FIGLIE SBLOCCO L'ULTIMA
550300941229     C     *IN21         DOWEQ     *OFF
550400941229     C     LBLLAN        IFEQ      EXLNA
550500941229     C                   MOVE      LBLAAN        ULFAAS
550600941229     C                   MOVE      LBLLPN        ULFLNP
550700941229     C                   MOVE      LBLNRN        ULFNRS
550800941229     C                   MOVE      LBLNSN        ULFNSP
550900941229     C                   END
551000941229     C     KBOL          READE     FNLBL02L                               21
551100941229     C                   ENDDO
551200941229     C     ULFNSP        IFNE      *ZEROS
551300941229     C* BLOCCO LA VECCHIA BOLLA
551400941229     C     $GIAC         IFEQ      '0'
551500941229     C                   MOVEL     '*'           ARBFBC
551600941229     C                   ELSE
551700941229     C                   MOVEL     'G'           ARBFBC
551800941229     C                   END
551900941229     C                   UPDATE    FNARB000
552000941229     C* SBLOCCO L'ULTIMA FIGLIA
552100941229     C     KBOLF         CHAIN     FNARB01L                           21
552200941229     C  N21              MOVEL     *BLANK        ARBFBC
552300960507     C* SE HO AGGIUNTO IL C/ASSEGNO E' HO GIA' UNA FIGLIA FACCIO
552400960507     C* NUOVA BOLLA
552500960507     C     *IN31         IFEQ      '0'
552600960507     C     VNCA          ANDGT     0
552700960507     C     VASS          ANDEQ     0
552800960507     C     *IN21         ANDEQ     '0'
552900960507     C     ARBDCM        ANDEQ     *ZEROS
553000960507      * GUARDO SE è UN ASSEGNATO
553100960507     C                   MOVEL     '3A'          TBLCOD
553200960507     C                   MOVEL     *BLANK        TBLKEY
553300960507     C                   MOVEL     ARBCBO        TBLKEY
553400960507     C     KTAB          CHAIN     TABEL                              18
553500960507     C     *IN18         IFEQ      '0'
553600960507     C                   MOVEL     TBLUNI        DS3A
553700960507     C                   MOVEL     §3ATB1        NWTB1
553800960507     C                   MOVEL     §3ATB2        NWTB2
553900960507     C                   MOVEL     '1'           NWFCA
554000960507     C                   MOVEL     §3AFAN        NWFAN
554100960507     C                   MOVEL     §3AFTD        NWFTD
554200960507     C                   EXSR      NEWCBL
554300960507     C  N30              MOVEL     NWCBL         ARBCBO
554400960507     C                   SETOFF                                       21
554500960507     C                   END
554600960507     C                   END
554700941229     C  N21              UPDATE    FNARB000
554800941229     C                   ELSE
554900941229     C* SE NON HO TROVATO FIGLIE DA SBLOCCARE
555000941229     C* SBLOCCO LA VECCHIA
555100941229     C                   MOVEL     *BLANK        ARBFBC
555200941229     C                   UPDATE    FNARB000
555300941229     C                   ENDIF
555400941229     C                   ENDIF
555500941229     C                   ENDIF
555600950918     C*  Se rimetto in consegna la bolla chiudo anomalie di giacenza
555700950918     C                   EXSR      CLOANM
555800941229     C                   ENDIF
555900941003     C                   ENDIF
556000941005      *--------------------------------------------------------
556100941217      * DIROTTAMENTO/CAMBIO PORTO  CREO EVENTO  SE NO GIACENZE
556200941005      *--------------------------------------------------------
556300941217     C     $GIAC         IFEQ      '0'
556400941005     C     VFRI          IFEQ      'D'
556500941217     C     VFRI          OREQ      'C'
556600941217     C     VNPC          ANDNE     ' '
556700941005     C                   EXSR      EVENTO
556800941005     C                   ENDIF
556900941217     C                   ENDIF
557000940303      *--------------------------------------------------------
557100940303     C*  PRENDO NR. RICH. RECUPERO   SE NO GIACENZE
557200940303      *--------------------------------------------------------
557300940310     C     $GIAC         IFEQ      '0'
557400940303     C     VFRI          IFEQ      'D'
557500941216     C     §LPLNA        ANDNE     EXLNA
557600940303     C     VNPC          ANDEQ     ' '
557700940303     C     EXPORT        ANDEQ     'F'
557800940303     C* CAMBIO PORTO
557900940303     C     VNPC          OREQ      'F'
558000940303     C*
558100021009     c                   eval      kpjbus=kpjbu
558200020813     c                   clear                   trul33ds
558300020813     c                   move      'L'           i33tla
558400020813     c                   z-add     15            i33cnu
558500021009     c                   z-add     filarr        i33po1
558600020813     c                   z-add     1             i33num
558700020813     c                   movel     trul33ds      kpjbu
558800020813     c                   call      'TRUL33R'
558900020813     c                   PARM                    kpjba
559000020813     c                   movel     kpjbu         trul33ds
559100020813     c                   z-add     o33nri        §rirec            7 0
559200021009     c                   eval      kpjbu=kpjbus
559300940303     C                   END
559400940303     C                   END
559500951213      *--------------------------------------------------------
559600951213     C* CREAZ. LEGAME BOLLA CREATA
559700951213      *--------------------------------------------------------
559800951213      * SE HO CREATO NUOVA BOLLA LA LEGO SEMPRE !!!!!
559900951213     C     *IN31         IFEQ      '1'
560000951213     C                   EXSR      LEGBOL
560100951213     C* METTO NELLE NOTE CONSEGNA LA NUOVA SPEDIZIONE
560200951213     C                   EXSR      AGGAR4
560300030826     C* per bolle con particolarità RC (orm contestuale alla consegna)
560400030826     c* chiudo l'orm della bolla originale e aggiorno il po di ritirno con
560500030826     c* il po d'arrivo della bolla originale in modo da far creare in
560600030826     c* automatico il nuovo orm
560700030826     C                   EXSR      AGGORM
560800951213     C                   END
560900951213     C*-------------------------------------------------------
561000951213     C* BOLLA DA CREARE ANCHE IN ARRIVO PER ANDARE IN CONSEGNA
561100951213     C*-------------------------------------------------------
561200951213     C     *IN88         IFEQ      '1'
561300951213     C                   EXSR      ARRBOL
561400951213     C                   ELSE
561500951213     C*  Se non devo creare una nuova bolla controllo se devo
561600951213     C*  ristampare la vecchia (l.avviso) in caso di riconseg.
561700951213     C     VFRI          IFEQ      'C'
561800070412      * procedura di automazione distinte non si deve più stampare la LDV
561900070412     c                   if        §ogdda = ' ' or
562000070424     c                             §ogdda = '20391231' or amgudt < ogddan
562100951213     C                   EXSR      CTRSTB
562200070412     C                   endif
562300951213     C                   END
562400951213     C                   END
562500951213     C*-------------------------------------------------------
562600951213     C* BOLLA NON CREATA IN ARRIVO, MA LINEA ARRIVO è = DEVO STAMPARLA
562700951213     C*-------------------------------------------------------
562800951213     C   31§LPLNA        IFEQ      EXLNA
562900951213     C     *IN88         ANDEQ     '0'
563000951213     C                   EXSR      STABLP
563100951213     C                   END
563200940303      *--------------------------------------------------------
563300940303     C*STAMPO  RICHIESTA RECUPERO
563400940303      *--------------------------------------------------------
563500951212     C     $GIAC         IFEQ      '0'
563600940303     C     VFRI          IFEQ      'D'
563700941216     C     §LPLNA        ANDNE     EXLNA
563800940303     C     VNPC          ANDEQ     ' '
563900940303     C     EXPORT        ANDEQ     'F'
564000940303     C                   SETON                                        84
564100940303     C                   END
564200940303     C* CAMBIO PORTO
564300940303     C     VNPC          IFEQ      'F'
564400951212     C     WEXTB1        OREQ      'FS'
564500951212     C     VNPC          ANDEQ     ' '
564600960126     C     VFRI          IFEQ      'C'
564700960126     C                   SETON                                        91
564800960126     C                   ELSE
564900960126     C                   SETON                                        85
565000940303     C                   END
565100960126     C                   END
565200941221     C* RECUPERO PROVV. C/ASS SE AGGIUNTO C/ASSEGNO E PAGA MITTENTE
565300941221     C     VVCS          IFEQ      'S'
565400941221     C     VNCA          ANDGT     0
565500941221     C     VASS          ANDEQ     0
565600941221     C     VPPC          ANDEQ     'M'
565700941221     C                   SETON                                        89
565800941221     C                   ENDIF
565900960126     C   84
566000960126     COR 85              EXSR      RCHREC
566100951212     C*  Per riconsegne con cambi di porto da assegnato a franco
566200951212     C*  creo una nuova bolla x recupero spese trasporto x lnp
566300960126     C   91              EXSR      RECTRA
566400030617     c                   if        viddcli = 'C' and vfri = 'D'
566500020617     c                   seton                                        92
566600160826      *-
566700020617     c                   exsr      rectra
566800160826      *-
566900160826      * per DPD scrive lo SCAN 06 di DIROTTAMENTO per recuperare le
567000160826      * spese solo se il dirottamento è fra 2 codici DEPOT LNP e LNA
567100160826      * differenti fra loro.
567200160826     c                   exsr      WRI_SCAN06
567300160826      *-
567400020625     c                   setoff                                       92
567500020617     c                   endif
567600960126     C                   SETOFF                                       858489
567700940303     C                   END
567800951212      *--------------------------------------------------------
567900951212     C* RICHIESTA NOTA ACCREDITO
568000951212      *--------------------------------------------------------
568100951212     C   32
568200951212     COR 37EXNAC         IFNE      'C'
568300951212      * STAMPO SOLO SE NON L'HO GIA' RICHIESTA
568400951212     C                   EXSR      RCHNAC
568500951212     C                   END
568600951213     C                   MOVEL     ' '           WRECSP            1
568700960126     C                   SETOFF                                       91
568800940303     C*-------------------------------------------------------
568900940303     C*  AGGIORNA ARCHIVI GIACENZA
569000940303     C*-------------------------------------------------------
569100940303     C     $GIAC         CASEQ     '1'           GCPWTR
569200940304     C                   ENDCS
569300940307     C     ENDAGG        ENDSR
569400160826      *---------------------------------------------------------*
569500160826      *  x DIROTTAMENTO causa DPD scrive  SCAN06 nel TIDP300F
569600160826      *---------------------------------------------------------*
569700160826     c     WRI_SCAN06    begsr
569800160826      *
569900160826     c                   clear                   FNLG28DS
570000160829     c                   eval        i28AAS = WSVAAS
570100160829     c                   eval        i28LNP = WSVLNP
570200160829     c                   eval        i28NRS = WSVNRS
570300160829     c                   eval        i28NSP = WSVNSP
570400160829     c                   eval        i28LNA = WSVLNA
570500160826      *
570600160826     c                   call      'FNLG28R'
570700160826     c                   parm                    FNLG28DS
570800160826      *
570900160826     C                   ENDSR
571000070321      *---------------------------------------------------------*
571100070321      *  elimina blocchi alla consegna da bolla data x consegnata
571200070321      *---------------------------------------------------------*
571300070321     c     elibloc       begsr
571400070321     C                   clear                   dsbl4l
571500070803     C     Kar4l         chain(e)  fiar401l
571600070803      *provvisorio da eliminare con test allocazione
571700070803     c                   if        %error
571800070803     C     Kar4l         chain(N)  fiar401l
571900070803     c                   goto      endbloc
572000070803     c                   endif
572100070321     C                   if        %found(fiar401l)
572200070321     C                   movel     ar4not        dsbl4l
572300070321     c                   move      *blank        §b4lbas
572400070321     c                   move      *blank        §b4lbge
572500070321     c                   move      *blank        §b4ltca
572600070321     c                   move      *blank        §b4ltfd
572700070614     C                   movel     dsbl4l        ar4not
572800070614     c                   update    fiar4000
572900070321     C                   endif
573000070321     c
573100070803     C     endbloc       endsr
573200090729      *---------------------------------------------------------*
573300090729      *  scrive contatto in fase di riconsegna se CA non abilitato
573400090729      *---------------------------------------------------------*
573500090729     c     contatto      begsr
573600090729     C                   clear                   dsbl4l
573700090729     C     Kar4l         chain(e)  fiar401l
573800090729      *provvisorio da eliminare con test allocazione
573900090729     c                   if        %error
574000090729     C     Kar4l         chain(N)  fiar401l
574100090729     c                   goto      endcont
574200090729     c                   endif
574300090729     C                   if        %found(fiar401l)
574400090729     C                   movel     ar4not        dsbl4l
574500090729     c                   move      'S'           §b4ltca
574600090729     C                   movel     dsbl4l        ar4not
574700090729     c                   update    fiar4000
574800090729     c                   else
574900090729     c                   eval      ar4aas = arbaas
575000090729     c                   eval      ar4lnp = arblnp
575100090729     c                   eval      ar4nrs = arbnrs
575200090729     c                   eval      ar4nsp = arbnsp
575300090729     c                   eval      ar4trc = 'L'
575400090729     c                   move      'S'           §b4ltca
575500090729     c                   eval      ar4not = dsbl4l
575600090729     c                   eval      ar4ftr = 'R'
575700090729     c                   eval      ar4dtr = amgudt
575800090729     c                   eval      ar4duv = amgudt
575900090729     c                   write     fiar4000                             99
576000090729     C                   endif
576100090729     c
576200090729     C     endcont       endsr
576300070614      *---------------------------------------------------------*
576400070614      *  Duplica eventuali note LDV presenti in bolla
576500070614      *---------------------------------------------------------*
576600070614     c     dupnotldv     begsr
576700070614     C                   move      'LDV'         cdnk
576800070614     C                   move      'S'           gstk
576900070614     C     Karn          setll     fiarn11l
577000070614     c                   do        *hival
577100070614     C     Karn          reade     fiarn11l
577200070614     C                   if        %eof(fiarn11l)
577300070614     c                   leave
577400070614     c                   endif
577500070614     c                   move      §lpaas        arnaas
577600070614     c                   move      §lplnp        arnlnp
577700070614     c                   move      §lpnrs        arnnrs
577800070614     c                   move      §lpnsp        arnnsp
577900070614     c                   write     fiarn000
578000070614     C                   enddo
578100070614     c
578200070614     C                   endsr
578300070622     C**************************************************************************
578400070622     C*  Routine per gestione richiamo interrogazione bolle
578500070622     C**************************************************************************
578600070622     C     richbl        begsr
578700070622      *
578800070622     c                   movel     kpjbu         §kpjbu
578900070622      *
579000070622     c                   z-add     arbaas        pa1aas
579100070622     c                   z-add     arblnp        pa1lnp
579200070622     c                   z-add     arbnrs        pa1nrs
579300070622     c                   z-add     arbnsp        pa1nsp
579400070622     c                   movel     ' '           pa1f03
579500070622     c                   movel     '1'           pa1flg
579600070622     c                   movel     param1        kpjbu
579700070622     c                   call      'FNLR36R'
579800070622     c                   parm                    kpjba
579900070622     c                   movel     kpjbu         param1
580000070622      *
580100070622     c                   movel     §kpjbu        kpjbu
580200070622      *
580300070622     c                   endsr
580400950918     C*-----------------------------------------------------*
580500950918     C*  Chiudo anomalie 140 su dettaglio segnacolli
580600950918     C*-----------------------------------------------------*
580700950918     C     CLOANM        BEGSR
580800950918     C*
580900950918     C                   Z-ADD     140           KCAA
581000990518     C     KANM1         CHAIN     FNANM01L                           38
581100950918     C     *IN38         DOWEQ     '0'
581200960618     C                   CLEAR                   DSLR33
581300960618     C                   MOVE      '4'           D33FSC
581400960618     C                   Z-ADD     AMGUDT        D33DCH
581500960618     C                   MOVEL     'DI'          D33CCH
581600960618     C                   Z-ADD     ANMNR1        D33NRR
581700960618     C                   CALL      'FNLR33R'
581800960618     C                   PARM                    DSLR33
581900990518     C     KANM1         READE     FNANM01L                               38
582000950918     C                   END
582100950918     C*
582200950918     C                   ENDSR
582300940308     C*---------------------------------------------------------------*
582400940309     C*   P G N B L P  RICH. PGM NUOVA BOLLA                          *
582500940308     C*---------------------------------------------------------------*
582600940309     C     PGNBLP        BEGSR
582700971113      *
582800170323     c                   clear                   wlnpb
582900941003      * IMPOSTO COD. CLIENTE MITTENTE
583000950130     C                   MOVE      EXLNA         §LPLNP
583100000313     C                   MOVE      ARBRMN        §LPRMN
583200000925     C                   MOVE      'N'           DPDSN             1
583300000313     C* Se la nuova bolla ha lnp DPD (EX LNA) imposto il nr.parcel
583400000313     C* nel rifermiento mittente numerico
583500150724     C*   QUINDI è dall'estero verso l'Italia
583600000313     C     §LPLNP        CHAIN     AZORG01L                           21
583700000313     C     *IN21         IFEQ      '0'
583800000313     C     ORGFVA        ANDNE     '*'
583900000313     C                   MOVEL     ORGDE3        OG143
584000020617     C     §OGntw        IFEQ      'DPD'
584100000925     C                   MOVE      'S'           DPDSN
584200150630     c                   end
584300150630     c                   endIF
584400170228     c* Ragionamenti particolari se §lplnp è una DPD
584500170228     c                   if        dpdsn='S'
584600170228     c                   exsr      sr_lnpdpd
584700170323     c**                 if        wlnpb>0
584800170323     c**                 eval      §lplnp=wlnpb
584900170323     c**                 endif
585000170228     c                   endif
585100150630      *
585200150630     c                   clear                   fnlg24ds
585300150630     c                   z-add     Kaas          i24aas
585400150630     c                   z-add     Klnp          i24lnp
585500150630     c                   z-add     Knrs          i24nrs
585600150630     c                   z-add     Knsp          i24nsp
585700150630     c*
585800150630     c                   call      'FNLG24R'
585900150630     C                   parm                    fnlg24ds
586000150630     c*
586100150630     c                   eval      §lpnpr = o24npr
586200150630     c*
586300150724     c* C'è il PARCEL quindi è un DPD:
586400150724     C* ... per dirottamenti e resi DPD, imposta il COD.trattamento merce
586500150724     C*     specifico x DPD.
586600150630     c                   if         o24npr <> *blank
586700000313     C     VFRI          IFEQ      'D'
586800160908     C     *in92         andeq     *Off
586900000313     C     VFRI          OREQ      'R'
587000140711     c                   exsr      srctm2k
587100140711     c                   if        §lpctm<>'2K'
587200000313     C                   MOVEL     '6F'          §LPCTM
587300140711     c                   else
587400140711     c                   movel     '2H'          §LPCTM
587500000313     C                   END
587600140711     C                   END
587700150630     c*
587800151020     c                   endIF
587900151020     c*
588000030131     C* in caso di dirottamento o cambio di porto su riconsegna
588100030131     C*reperisco i bancali e i campi correlati da impostare sulla nuova bolla
588200030131     C                   clear                   dar5ban
588300030131     C                   if        vfri = 'D'  or
588400030131     c                             vfri = 'C' and vnpc <> *blank and
588500030131     c                             vnpc <> export
588600030805     C                   move      'BAN'         trcar5
588700051013     C     Kfiar5        CHAIN(n)  FiAR501L                           43
588800030131     C     *IN43         IFEQ      '0'
588900030131     C                   MOVEL     ar5uni        dar5ban
589000030131     C                   endif
589100030131     c                   z-add     §AR5TBA       §lpTBA
589200030131     c                   z-add     §AR5NBA       §lpNBA
589300030131     c                   z-add     §AR5TB2       §lpTB2
589400030131     c                   z-add     §AR5NB2       §lpNB2
589500030131     C                   endif
589600031124      * Bancali Bartolini
589700031124     c                   If        %Subst(Arbgva:1:1) = 'O' and wrecsp = ' '
589800031124     c                   Clear                   dAr5BNB
589900031124     c                   Clear                   tasban
590000031124     C                   move      'BNB'         trcar5
590100051013     c     kFiar5        Chain(n)  Fiar501l
590200031124     c                   If        %Found(Fiar501l)
590300031124     c                   Movel     Ar5Uni        dAr5BNB
590400031124     c                   EndIf
590500031124     c                   z-add     §AR5bTBA      §lpTBA
590600031124     c                   z-add     §AR5bNBA      §lpNBA
590700031124     c                   z-add     §AR5bTB2      §lpTB2
590800031124     c                   z-add     §AR5bNB2      §lpNB2
590900031124     c                   Eval      §lpcor = §Ar5Bcor
591000031124     c                   Eval      §lpcsf = §Ar5Bcsf
591100031124     c                   EndIf
591200031119      * imposto dati per referente  destinatario  e num telefono
591300031119     C                   if        vfri = 'D'  or
591400031119     c                             vfri = 'C' and vnpc <> *blank and
591500031119     c                             vnpc <> export
591600031119     c                   movel     vref          §lpref
591700031119     c                   movel     vteld         §lptel
591800031119     c                   endif
591900941220     C                   EXSR      AGGCCM
592000940308     C                   MOVEL     AMGUDT        §LPAAS
592100940308     C                   MOVE      AMGUDT        §LPMGS
592200940308     C                   MOVE      *ZEROS        §LPNRS
592300940308     C                   MOVE      *ZEROS        §LPNSP
592400150630      * *****************
592500150630      * RESO___________:
592600150630      * *****************
592700150630      *
592800940308     C     VFRI          IFEQ      'R'
592900941003     C                   MOVE      'S'           §LPFBR
593000950104     C                   MOVEL     *BLANKS       §LPGCA
593100950104     C                   MOVEL     *BLANKS       §LPGGA
593200031125      **---- < ATTENZIONE >  CHIODO PER varia O per i resi
593300031126     c                   If        %Subst(Arbgva:1:1) = 'O'
593400031126     c                   movel     arbgva        §lpgva
593500031126     c                   else
593600030821     C                   MOVEL     *BLANKS       §LPGVA
593700150311     C                   clear                   §lpTBA
593800150311     C                   clear                   §lpNBA
593900150311     C                   clear                   §lpTB2
594000150311     C                   clear                   §lpNB2
594100150311     C                   clear                   §lpcor
594200150311     C                   clear                   §lpcsf
594300031125     c                   endif
594400000925     C                   MOVEL     *BLANKS       §LPGMA
594500000922      *
594600940308     C                   ELSE
594700150630     ******************
594800150630     **  NO RESO______:
594900150630     ******************
595000150630      *
595100950104     C* IMPOSTAZIONE CAMPI GESTIONI VARIE
595200950104     C                   MOVEL     AR9GCA        §LPGCA
595300950104     C                   MOVEL     ARBGGA        §LPGGA
595400030821     C                   MOVEL     ARBGMA        §LPGMA
595500030821     C                   MOVEL     ARBGVA        §LPGVA
595600960205     C*§ ABBLENCO 2°BYTE PARTIC. VARIE
595700960306     C                   MOVE      ' '           §LPGVA
595800110715      * se dirottata bolla di rientro cerca le varie relative alla giacenza prec
595900110715      * le salva per riscriverle sulla bolla nuova solo da fuori giacenza
596000110715     c                   clear                   memovar1         11 3
596100110715     c                   clear                   memovar2         11 3
596200110715     c                   clear                   memovar3         11 3
596300110715     c                   clear                   memova1           1
596400110715     c                   clear                   memova2           1
596500110715     c                   clear                   memova3           1
596600150630     c                   clear                   §LPFBR
596700150511     C* reperisco variabili tipo bolla
596800150511     C                   CLEAR                   DS3A
596900150512     C                   MOVEL     '3A'          TBLCOD
597000150512     C                   MOVEL     *BLANK        TBLKEY
597100150512     C                   MOVEL     §lpCBO        TBLKEY
597200150512     C     KTAB          CHAIN     TABEL                              21
597300150512     C  N21              MOVEL     TBLUNI        DS3A
597400110715
597500940308     C     ARBFBR        IFNE      *BLANK
597600150511     C     §3asva        andeq     *blank
597700940308     C                   MOVE      ARBFBR        §LPFBR
597800150630     c                   if           vfri = 'D' and not *in90
597900110714     c                   if        ar6sv1 = 'I' or ar6sv1 = 'N'
598000110714     c                   z-add     ar6va1        memovar1
598100110715     c                   movel     ar6sv1        memova1
598200110714     c                   endif
598300110714     c                   if        ar6sv2 = 'I' or ar6sv2 = 'N'
598400110714     c                   z-add     ar6va2        memovar2
598500110715     c                   movel     ar6sv2        memova2
598600110714     c                   endif
598700110714     c                   if        ar6sv3 = 'I' or ar6sv3 = 'N'
598800110714     c                   z-add     ar6va3        memovar3
598900110715     c                   movel     ar6sv3        memova3
599000110714     c                   endif
599100110714     c                   endif
599200940308     C                   END
599300150630      * ??????????????
599400961129     C*  Se bolla originale in franco frontiera olla figlia no
599500961129     C     ARBFBR        IFEQ      'F'
599600961129     C                   MOVEL     *BLANKS       §LPFBR
599700961129     C                   END
599800940308     C                   END
599900150630      *****************
600000150630     C* CONSEGNA______:
600100150630      *****************
600200950725     C     VFRI          IFEQ      'C'
600300940308     C                   MOVE      ARBFFD        §LPFFD
600400950725     C                   ELSE
600500950725     C                   MOVE      *BLANKS       §LPFFD
600600950725     C                   END
600700150630      *****************
600800150630      * - X TUTTI_____:
600900150630      *****************
601000940308     C                   MOVE      *BLANK        §LPCPI
601100940308     C                   MOVE      *ZEROS        §LPIMV
601200940308     C                   MOVE      *ZEROS        §LPDFT
601300940308     C                   MOVE      *ZEROS        §LPNFT
601400940308     C                   MOVE      *ZEROS        §LPFIV
601500950120     C                   MOVEL     ARBRMA        §LPRMA
601600941003     C                   MOVEL     ARBRMO        §LPRMO
601700091221     c                   clear                   §LPPDR
601800950118     C                   MOVEL     §LPLNP        §LPPDR
601900091221     C                   MOVE      0998          §LPPDR
602000940308     C                   MOVE      AMGUDT        §LPDRT
602100941003     C                   MOVE      *ZEROS        §LPNRT
602200941011     C                   MOVEL     'P'           §LPFPP
602300050208     C                   MOVE      *blank        §LPXCO
602400150310     C                   IF        §lpcbo <> 'F*' and
602500150310     C                             (vfri = 'C' or vfri = 'D')
602600031110     C                   MOVEL     arbtcr        §LPTCR
602700031110     C                   z-add     arbdcr        §LPDCR
602800031110     C                   z-add     arbhcr        §LPHCR
602900031110     c                   else
603000031110     C                   MOVEL     ' '           §LPTCR
603100940308     C                   MOVE      *ZEROS        §LPDCR
603200940308     C                   MOVE      *ZEROS        §LPHCR
603300031110     c                   endif
603400941221     C* GUARDO SE BOLLA IN C/SERVIZIO AZZERO LE VARIE
603500941221     C                   CLEAR                   DS3A
603600941221     C                   Z-ADD     1             §F                2 0
603700941221     C     §LPCBO        LOOKUP    CDB(§F)                                21
603800941221     C   21              MOVEL     D3A(§F)       DS3A
603900960227     C   21              MOVEL     §3ATB1        WNEWPO            1
604000960227     C   21WNEWPO        IFEQ      'A'
604100960227     C     §3ATB1        ANDNE     'AS'
604200960227     C     §APPO         ANDEQ     8888
604300960227     C                   MOVE      '9999'        §LPCCM
604400960227     C                   MOVE      '9999'        §LPCCN
604500960227     C                   END
604600941221     C     §3ARBL        IFEQ      'C'
604700941221     C                   CLEAR                   NVA
604800941221     C                   CLEAR                   NSV
604900941221     C                   ELSE
605000941216     C* ANTEPORTO
605100941216     C     §ANTEP        IFGT      *ZEROS
605200941216     C                   MOVEL     §$2VRN        §SIGLA            1
605300941216     C                   MOVEL     NSV           WSV
605400941216      * CERCO L'INDICE SU CUI LAVORARE
605500941216     C                   EXSR      CHKVAR
605600941216     C                   Z-ADD     §ANTEP        NVA(XS)
605700941216     C                   MOVEL     §$2VRN        NSV(XS)
605800941216     C                   ENDIF
605900941216     C* SPESE GIACENZA
606000941003     C     §TOTSP        IFGT      *ZEROS
606100941216     C                   MOVEL     §$2VRI        §SIGLA
606200941216     C                   MOVEL     NSV           WSV
606300941216      * CERCO L'INDICE SU CUI LAVORARE
606400941216     C                   EXSR      CHKVAR
606500941216     C                   Z-ADD     §TOTSP        NVA(XS)
606600941216     C                   MOVEL     §$2VRI        NSV(XS)
606700940308     C                   END
606800941003     C* SE RIENTRO E C'E' VARIA R DEVE ESSERE RIPORTATA
606900940308     C     VFRI          IFEQ      'R'
607000941003     C     §VARR         ANDGT     *ZEROS
607100960327     C     V4CRCS        ANDEQ     'N'
607200941216     C                   MOVEL     §$2VRR        §SIGLA
607300941216     C                   MOVEL     NSV           WSV
607400941216      * CERCO L'INDICE SU CUI LAVORARE
607500941216     C                   EXSR      CHKVAR
607600941216     C                   Z-ADD     §VARR         NVA(XS)
607700941216     C                   MOVEL     §$2VRR        NSV(XS)
607800941003     C                   END
607900941003     C* PROVV. C/ASSEGNO NELLA  VARIA G
608000940308     C     VRCA          IFGT      *ZEROS
608100941216     C                   MOVEL     §$2VRG        §SIGLA
608200941216     C                   MOVEL     NSV           WSV
608300941216      * CERCO L'INDICE SU CUI LAVORARE
608400941216     C                   EXSR      CHKVAR
608500941216     C                   Z-ADD     VRCA          NVA(XS)
608600941216     C                   MOVEL     §$2VRG        NSV(XS)
608700940308     C                   ELSE
608800940308     C* SE C'E C/ASSEGNO MA NON CI SONO PROVV. FORZO =0
608900941229     C* SE CREATA BOLLA IN ARRIVO
609000941229     C     §LPCAS        IFGT      0
609100941229     C     *IN88         ANDEQ     *ON
609200941229     C                   MOVEL     §$2VRG        §SIGLA
609300941229     C                   MOVEL     NSV           WSV
609400941216      * CERCO L'INDICE SU CUI LAVORARE
609500941229     C                   EXSR      CHKVAR
609600941229     C                   MOVE      *ZEROS        NVA(XS)
609700941229     C                   MOVEL     §$2VRG        NSV(XS)
609800941229     C                   END
609900940308     C                   END
610000941221     C                   ENDIF
610100941216     C* TRASPORTO
610200940308     C     VRTR          IFGT      *ZEROS
610300990721     C                   Z-ADD     VRTR          §LPPOR
610400940308     C                   END
610500971003     C* Se non devo recuperare le spese di trasporto imposto il totale
610600971003     C* imponibile
610700971003     C     EXPORT        IFEQ      'F'
610800971003     C     WEXTB1        ANDNE     'FS'
610900971003     C     VNPC          ANDNE     'A'
611000971013     C     VFRI          ANDNE     'R'
611100991001     C* Scorro le varie e le sommo nel totale imponibile solo se il
611200991001     C* flag tab cc lo prevede
611300991001     C                   DO        20            WW                3 0
611400991001     C                   Z-ADD     1             XW                3 0
611500991001     C     NSV(WW)       LOOKUP    CCC(XW)                                20
611600991001     C     *IN20         IFEQ      '1'
611700040123     C     IPB(XW)       ANDEQ     'S'
611800991001     C                   ADD       NVA(WW)       §LPIMV
611900991001     C                   END
612000991001     C                   END
612100971003     C                   END
612200940308     C* MITTENTE
612300940308     C* NON GIA IMPOSTATO PRIMA
612400940308     C     §LPRSM        IFEQ      *BLANKS
612500940308     C                   MOVEL     ARBRSM        §LPRSM
612600940308     C                   MOVEL     ARBINM        §LPINM
612700940308     C                   MOVE      ARBCAM        §LPCAM
612800940308     C                   MOVEL     ARBLOM        §LPLOM
612900940308     C                   MOVEL     ARBPRM        §LPPRM
613000951107     C                   MOVEL     ARBNZM        §LPNZM
613100940308     C                   END
613200970408     C* SE CODICE MITTENTE = 8888/9999 O RESO PASSO 0 NEL CODICE
613300970408     C* TARIFFA ALTRIMENTI GLI PASSO IL CODICE DELLA BOLLA ORIG.
613400970408     C                   MOVE      §LPCCM        W8899             4 0
613500970408     C     W8899         IFEQ      8888
613600970408     C     W8899         OREQ      9999
613700970408     C     VFRI          OREQ      'R'
613800970408     C                   MOVE      *ZEROS        §LPCTR
613900970408     C                   ELSE
614000970408     C                   MOVE      ARBCTR        §LPCTR
614100970408     C                   END
614200970408     C* CAMPI DALLA VECCHIA BOLLA
614300940308     C                   MOVE      *BLANK        §LPCTS
614400940308     C                   MOVE      ARBTSP        §LPTSP
614500091008      * forzatura h 10,30 x dirottamenti resi viene trasformato in 'E'
614600091008     c                   if        §lptsp = 'H' and
614700091008     c                             (vfri = 'D' or vfri = 'R')
614800091008     c                   move      'E'           §lptsp
614900091008     c                   endif
615000941003     C                   Z-ADD     ARBVMD        §LPVMD
615100941216     C                   MOVEL     ARBVAD        §LPVAD
615200960306     C                   MOVEL     'P'           §LPFPR
615300970407     C                   MOVEL     'G'           §LPRIC
615400980519     C* Reperisco flag x stampare o meno bolla in partenza
615500081114      * a meno che non abbia già fatto un tentativo nel caso forzo sempre'N'
615600081114     c                   if        arbntc > 0
615700081114     c                   move      'N'           §lpbam
615800081114     c                   else
615900980519     C                   CLEAR                   AR4NOT
616000980519     C                   MOVE      'A'           §TRC
616100060213     C     KAR4          CHAIN(N)  FiAR401L                           43
616200980519     C  N43              MOVEL     AR4NOT        §LPBAM
616300980519     C   43              MOVEL     'S'           §LPBAM
616400081114     c                   endif
616500000313     C* Salvo flag bolla no DDT per verificare se devo effettuare
616600000313     C* le stampe
616700000313     C                   MOVEL     §LPBAM        WOKSTP            1
616800150630      *
616900150630      *****************************
617000150630      *- NO BOLLA DI RECUPERO_____:
617100150630      *****************************
617200150630      *
617300951213     C* Se no bolla recupero spese x cambio di porto da assegnato a
617400951213     C* franco imposto i dati dal video e da vecchia bolla
617500951213     C     WRECSP        IFNE      'S'
617600150630      *
617700960306     C                   MOVEL     'G'           §LPFPR
617800961011     C  N94V4CRCS        IFNE      'S'
617900051006     C     vfri          andeq     'R'
618000951227     C                   MOVEL     *BLANK        §LPNT2
618100960328     C                   END
618200951213     C* DESTINATARIO
618300951213     C     VRSD          IFNE      *BLANKS
618400951213     C                   MOVEL     VRSD          §LPRSD
618500951213     C                   MOVEL     VRD2          §LPRD2
618600951213     C                   MOVEL     VIND          §LPIND
618700951213     C                   MOVE      VCAD          §LPCAD
618800951213     C                   MOVEL     VLOD          §LPLOD
618900951213     C                   MOVEL     VPRD          §LPPRD
619000951213     C                   MOVEL     VNZD          §LPNZD
619100951213     C                   ELSE
619200951213     C* NON GIA IMPOSTATO PRIMA
619300951213     C     §LPRSD        IFEQ      *BLANKS
619400951213     C                   MOVEL     ARBRSD        §LPRSD
619500951213     C                   MOVEL     AR4RD2        §LPRD2
619600951213     C                   MOVEL     ARBIND        §LPIND
619700951213     C                   MOVE      ARBCAD        §LPCAD
619800951213     C                   MOVEL     ARBLOD        §LPLOD
619900951213     C                   MOVEL     ARBPRD        §LPPRD
620000951213     C                   MOVEL     ARBNZD        §LPNZD
620100951213     C                   END
620200951213     C                   END
620300060929      **
620400000412     C* PER RESI DPD (IMPORT) IMPOSTO CODICE TRATTAMENTO MERCE=6F
620500951213     C                   MOVE      VLNA          §LPLNA
620600000412     C     §LPLNA        CHAIN     AZORG01L                           21
620700000412     C     *IN21         IFEQ      '0'
620800000412     C     ORGFVA        ANDNE     '*'
620900000412     C                   MOVEL     ORGDE3        OG143
621000150630      *
621100150630      * ? solo nel caso si tratti di un Reso a DPD
621200150630      *    cerchiamo l'indirizzo del DEPOT a cui fare la BOLLA .
621300151020     c     §ogntw        ifeq      'DPD'
621400000412     C     VFRI          ANDEQ     'R'
621500151020     c     o24npr        andNE     *blank
621600151020      **
621700151020     c*
621800151020     c*  con il DEPOT di origine del Parcel,  il TISIE10R restituisce
621900151020      *    l'indirizzo del DEPOT preso dal Cappario Originale DPD
622000151020     c                   clear                   tisie10ds
622100151020     c                   movel     o24npr        I10DEP
622200151020     c                   call      'TISIE10R'
622300151020     c                   parm                    tisie10ds
622400151020      *
622500151020      * e l'indirizzo del DEPOT a cui rendere
622600151020     c                   if        O10CDPNAZ <> *blank  and
622700151020     c                             O10ERR    <> 'E'
622800151020     C                   eval      §lprsd  = O10CDPNAM1
622900151020     C                   eval      §lprd2  = O10CDPNAM2
623000151020     C                   eval      §lpind  = O10CDPADR1 + O10CDPADR2
623100151020     C                   eval      §lpcad  = O10CDPPTC
623200151020     C                   eval      §lplod  = O10CDPCTY
623300151020     C                   eval      §lpprd  = *blank
623400151020     C                   eval      §lpnzd  = O10CDPNAZ
623500151020      *
623600151020     c                   endif
623700151021      **
623800151021      * ? MA ATTENZIONE:
623900151021      * Se x DPD la linea di Arrivo non è utilizzabile in bollettazione,
624000151021      * Prendo allora la linea del Cliente
624100151021     c                   if        §OGLBL = 'N'
624200151021     c                   movel     §lpccm        Vlna
624300151021     C                   MOVE      VLNA          §LPLNA
624400151021     c                   end
624500151020     c*---------
624600000412     C                   END
624700000412     C                   END
624800150630     c
624900951213     C                   MOVE      ARBGC1        §LPGC1
625000951213     C                   MOVE      ARBGC2        §LPGC2
625100071024      * se precedentemente trovato destinatario particolare imposto
625200071024      * campi da anagrafica destinatari particolari
625300071024      * turni
625400071024      * forza giorni di chiusura solo se non impostati sulla mamma
625500091015     c                   if        des_parg <> *blank and ot0err = *blank
625600071024     c                   if        vfri <> 'C'
625700071024     c                   move      OT0X1GC1      §LPGC1
625800071024     c                   move      OT0X1GC2      §LPGC2
625900071024     c                   endif
626000071024      *giro
626100071024      * forza giro di consegna se lna corrisponde a linea gestione giro
626200071024     c                   if        §lplna = OT0X1POG
626300071024     c                   move      OT0X1CGI      §LPcgi
626400071024     c                   endif
626500071024     c                   endif
626600950214     C* RIPORTO SU NUOVA BOLLA IMPORTO DA ASSICURARE SOLO SE FACCIO
626700950214     C* CAMBIO PORTO IN ASSEGN. O RIMANE ASSEGNATO
626800950214     C     VNPC          IFEQ      'A'
626900950214     C     VNPC          OREQ      ' '
627000950214     C     EXPORT        ANDEQ     'A'
627100960205     C     VFRI          OREQ      'R'
627200960327     C     V4CRCS        ANDEQ     'N'
627300050509     C     VFRI          OREQ      'D'
627400950206     C                   Z-ADD     ARBIAS        §LPIAS
627500950214     C                   MOVEL     ARBVAS        §LPVAS
627600950214     C                   ELSE
627700950214     C                   CLEAR                   §LPIAS
627800950214     C                   CLEAR                   §LPVAS
627900950214     C                   ENDIF
628000150630      *
628100150630      *****************************
628200150630      *- SI BOLLA DI RECUPERO_____:
628300150630      *****************************
628400150630      **
628500951213     C                   ELSE
628600150630      **
628700951213      *  Bolla recupero spese trasporto dal mittente x cambi di
628800951213      *  porto da assegnato a franco
628900951213     C                   MOVEL     *BLANKS       §LPFFD
629000000925     C                   MOVEL     *BLANKS       §LPGGA
629100000922      *
629200000922      **---- < ATTENZIONE >  CHIODO PER DPD
629300000922      *    NON DEVE ESSERE RIPULITO IL CAMPO SE GIÀ IMPOSTATO
629400000922     C     VFRI          IFNE      'R'
629500000925     C     DPDSN         ORNE      'S'
629600951213     C                   MOVEL     *BLANKS       §LPGMA
629700000925     C                   ENDIF
629800000925      *
629900150311     C                   clear                   §lpTBA
630000150311     C                   clear                   §lpNBA
630100150311     C                   clear                   §lpTB2
630200150311     C                   clear                   §lpNB2
630300150311     C                   clear                   §lpcor
630400150311     C                   clear                   §lpcsf
630500951213     C                   MOVEL     *BLANKS       §LPGVA
630600951213     C                   MOVEL     *BLANKS       §LPGCA
630700951213     C                   Z-ADD     0             §LPPOR
630800951213     C                   Z-ADD     0             §LPDFT
630900951213     C                   Z-ADD     0             §LPFIV
631000990930     C                   CLEAR                   DTASV
631100030617     C* se annullamento porto assegnato sommo la varia & solo se la
631200030617     C* responsabilità è del cliente
631300030617     C                   IF        vfri='C' and vnpc='F' and viddcli='C'
631400030617     C                   MOVEL     §$2Vpa        §SIGLA
631500030617     C                   MOVEL     NSV           WSV
631600030617      * CERCO L'INDICE SU CUI LAVORARE
631700030617     C                   EXSR      CHKVAR
631800030623     C                   z-add     88888888      NVA(XS)
631900030617     C                   MOVEL     §$2Vpa        NSV(XS)
632000030617     C                   END
632100951213      *  Controllo se devo recuperare le provvigioni del C/Ass.
632200951213     C     VAPC          IFEQ      'S'
632300951213     C     VPPC          ANDEQ     'M'
632400951213     C                   Z-ADD     1             YY                4 0
632500951213     C     §$2VRG        LOOKUP    ASV(YY)                                06
632600951227      *  Se esiste varia G e non ho variato il C/Assegno tasso le provv.
632700951227      *  x il mittente in base alla tariffa del destinatario
632800951227     C     VVCS          IFNE      'S'
632900951227     C     *IN06         ANDEQ     '1'
633000951213     C                   MOVEL     ASV(YY)       NSV(1)
633100951213     C                   Z-ADD     AVA(YY)       NVA(1)
633200951227      *  ... altrimenti uso le tariffe di cartello
633300951227     C                   ELSE
633400951227     C                   EXSR      PPROVV
633500951213     C                   END
633600951213     C                   END
633700951213     C                   END
633800950206     C* SE SPED. ESTERO E DIVISA NON è IMPOSTATA FORZO DIV.=ITL
633900020226     C                   MOVEL     *BLANKS       ORGFL1
634000020226     C     §LPLNA        CHAIN     AZORG01L                           23
634100020729     C                   clear                   NLAFL1            1
634200020729     C     §OgNTW        ifeq      'FED'
634300020729     C     §OgNTW        oreq      'EEX'
634400020729     C     §OgNTW        oreq      'EUP'
634500020729     C                   movel     'E'           NLAFL1            1
634600020729     c                   end
634700020729      *
634800950206     C     §LPVMD        IFGT      *ZEROS
634900950206     C     ARBVAD        ANDEQ     *BLANK
635000011130     C                   MOVEL     §GEDCN        §LPVAD
635100950206     C                   ENDIF
635200950206     C*
635300950206     C     §LPIAS        IFGT      *ZEROS
635400950206     C     ARBVAS        ANDEQ     *BLANK
635500011130     C                   MOVEL     §GEDCN        §LPVAS
635600950206     C                   ENDIF
635700940308     C                   MOVE      ARBNAS        §LPNAS
635800940308     C                   MOVE      ARBNCL        §LPNCL
635900051005     C                   MOVE      ARBPKF        §LPPKB
636000941003     C                   MOVE      ARBPKC        §LPPKC
636100941003     C                   MOVE      ARBNCP        §LPNCP
636200951109     C                   MOVE      ARBVLF        §LPVLB
636300951109     C                   MOVE      ARBFVF        §LPFVB
636400960229     C*--------------------------------------------------------*
636500960229     C*  Se il flag del volume bollettato è  'C' o 'K' oppure
636600960229     C*  è  'P' ed il cliente è codificato imposto 'R' in modo
636700960229     C*  che non venga più ricalcolato
636800960229     C     ARBFVF        IFEQ      'C'
636900960229     C     ARBFVF        OREQ      'K'
637000960229     C     ARBFVF        OREQ      'P'
637100960229     C     §APPO         ANDEQ     8888
637200960229     C     ARBFVF        OREQ      'P'
637300960229     C     §APPO         ANDEQ     9999
637400960229     C                   MOVEL     'R'           §LPFVB
637500960229     C                   END
637600020716     C* se sto creando bolla di recupero x dirottamento azzero le varie
637700020716     c     viddcli       ifeq      'C'
637800020716     c     *in92         andeq     *on
637900020716     C                   CLEAR                   NVA
638000020716     C                   CLEAR                   NSV
638100020716     c                   endif
638200110715      * se dirottata bolla di rientro reimposta le varie relative alla giacenza
638300110715      * se erano valorizzate
638400110715     c                   if        vfri = 'D' and not *in90 or
638500110715     c                             vfri = 'R' and not *in90
638600110715     c                   if        memova1 <> *blank or
638700110715     c                             memova2 <> *blank or
638800110715     c                             memova3 <> *blank
638900110715     c                   move      memova1       nsv(1)
639000110715     c                   z-add     memovar1      nva(1)
639100110715     c                   move      memova2       nsv(2)
639200110715     c                   z-add     memovar2      nva(2)
639300110715     c                   move      memova3       nsv(3)
639400110715     c                   z-add     memovar3      nva(3)
639500110715     c                   endif
639600110715     c                   endif
639700960229     C*--------------------------------------------------------*
639800941003     C                   MOVE      ARBVLC        §LPVLC
639900941003     C                   MOVE      ARBNCR        §LPNCR
640000941006     C                   MOVE      ARBQFT        §LPQFT
640100940308     C                   MOVE      ARBNCR        §LPNCR
640200941011     C                   MOVEL     *BLANK        §LPFTM
640300941011     C                   MOVEL     *BLANK        §LPCEI
640400040916      * per bolla di recupero abblenca sempre tc1 e tc2
640500040916     c                   if        wrecsp <> 'S'
640600050826      * x consegna riporta sempre
640700050826      * x dirottamento riporta se non S=SUPERMERCATO o D=DISAGIATO
640800050826     c                   if        vfri = 'C' or
640900050826     c                             vfri = 'D' and
641000050826     c                             (arbtc1 <> 'D' and arbtc1 <> 'S' and
641100050826     c                              arbtc2 <> 'D' and arbtc2 <> 'S')
641200941011     C                   MOVEL     ARBTC1        §LPTC1
641300941003     C                   MOVEL     ARBTC2        §LPTC2
641400960228     C                   ELSE
641500960228     C                   MOVEL     *BLANKS       §LPTC1
641600960228     C                   MOVEL     *BLANKS       §LPTC2
641700960228     C                   END
641800040916     C                   else
641900040916     C                   move      *blank        §lptc1
642000160905     C                   move      *blank        §lptc2
642100040916     C                   endif
642200941003     C                   MOVEL     '*'           §LPSOP
642300940308     C* RICH. IMMISS. BOLLE
642400030715     C* OVRPRTF  BOLLE old
642500991206     C                   Z-ADD     80            LUNCMD           15 5
642600991206     C                   MOVEA     D82PRB        CMB(31)
642700991206     C                   MOVEA     D82MOB        CMB(64)
642800991206     C                   CALL      'QCMDEXC'
642900991206     C                   PARM                    CMB
643000991206     C                   PARM                    LUNCMD
643100030715     C* OVRPRTF  BOLLE a4
643200030715     C                   MOVEA     D82PRB4       CM4(31)
643300030715     C                   MOVEA     D82MdB4       CM4(64)
643400030715     C                   CALL      'QCMDEXC'
643500030715     C                   PARM                    CM4
643600030715     C                   PARM                    LUNCMD
643700030715     C* OVRPRTF  BOLLE a5
643800030715     C                   MOVEA     D82PRB5       CM5(31)
643900030715     C                   MOVEA     D82Mdb5       CM5(64)
644000030715     C                   CALL      'QCMDEXC'
644100030715     C                   PARM                    CM5
644200030715     C                   PARM                    LUNCMD
644300991206     C* OVRPRTF  BOLLE  DPD
644400991206     C                   MOVEA     D82PRB        CMP(31)
644500940308     C                   CALL      'QCMDEXC'
644600991206     C                   PARM                    CMP
644700940308     C                   PARM                    LUNCMD
644800030304     C* OVRPRTF  BOLLE  FedEx
644900030307     c                   Movel     *Blanks       comman           90
645000030307     C                   Z-ADD     90            LUNCMD           15 5
645100030304     C                   MOVEA     D82PRB        CMPf(31)
645200030307     c                   Movea     Cmpf          Comman
645300030307     c                   Eval      %Subst(Comman:77:9) = 'COPIES(2)'
645400030304     C                   CALL      'QCMDEXC'
645500030307     c                   PARM                    Comman
645600030304     C                   PARM                    LUNCMD
645700940308     C* OVRPRTF  ETICHETTE
645800941013     C                   Z-ADD     80            LUNCMD           15 5
645900941013     C                   MOVEA     D82PRE        CME(31)
646000950123     C                   MOVEA     D82MOE        CME(64)
646100940308     C                   CALL      'QCMDEXC'
646200940308     C                   PARM                    CME
646300940308     C                   PARM                    LUNCMD
646400991028     C* Imposto divisa per calcolo tassazione
646500991119     C                   XFOOT     NVA           TOTVAR           15 3
646600991119     C     TOTVAR        IFGT      0
646700991119     C     §LPPOR        ORGT      0
646800991028     C   33              MOVEL     V6CDIV        §LPDIV
646900991028     C   35              MOVEL     V6CRVA        §LPDIV
647000991028     C   36              MOVEL     V6CRTA        §LPDIV
647100991112     C  N33
647200991112     CANN35
647300991112     CANN36              MOVEL     GCPDIV        §LPDIV
647400991119     C* Se la divisa
647500991119     C     §LPDIV        IFEQ      *BLANKS
647600011130     C                   MOVEL     §GEDCN        §LPDIV
647700991119     C                   END
647800991119     C                   ELSE
647900991119     C                   MOVEL     *BLANKS       §LPDIV
648000991119     C                   END
648100030715     c* se dirottamento di una spedizione FW (servizio contrassegni)
648200030715     c* se le note 2 sono = *blanks metto
648300030715     c* la la dicitura "cliente tassazione xxxxxxx" preso dalla bolla orig.
648400030715     c                   if        §lpnt2 = *blanks and arbcbo = 'FW'
648500030715     c                             and vfri = 'D'
648600030715     C                   MOVE      '8'           §TRC
648700060213     C     KAR4          CHAIN(N)  FiAR401L
648800060213     c                   if        %found(fiar401l) and
648900030715     c                             %subst(ar4not: 1: 18) = 'cliente tassazione'
649000030715     C                   eval      §lpnt2= ar4not
649100030715     c                   end
649200030715     c                   end
649300100517     c* in caso di particolarità consegna  devo verificare
649400030821     c* se passare la particolarità x non fare scrivere il fiar500f nel
649500030821     c* pgm fnls01r inquanto lo scriverò in questo pgm.
649600100517     c                   if        arbgma <>*blank
649700100517     C                   MOVEL     '7R'          TBLCOD
649800100517     C                   MOVEL     *BLANK        TBLKEY
649900100517     C                   MOVEL     ARBGMA        TBLKEY
650000100517     C     KTAB          CHAIN     TABEL                              21
650100100517     C     *IN21         IFEQ      '0'
650200100517     C                   MOVEL     TBLUNI        DS7R
650300100517     C                   if        §7Rrc ='S'
650400030821     c                   eval      compar = arbgma
650500030821     c                   exsr      srpar
650600030821     c                   eval      §lpgma = compar
650700100517     c                   end
650800100517     c                   end
650900030821     c                   end
651000040123      * richiama tnsf20 x varie con calcolo istat solo se imponibile > 0
651100040123     c                   if        §lpimv > 0
651200040123     c                   exsr      ritassa
651300040123     c                   endif
651400030715     c*
651500040527      * imposta flag valore se esiste NO BOLLE RECUPERO
651600040527     c                   if        wrecsp <> 'S'
651700040527     c                   movel     §ar5bva       §lpbva
651800040527     c                   endif
651900040527     c*
652000100324     c                   clear                   §lpksca
652100100324     c                   clear                   §lpctra
652200091104      * imposta flag Triangolo e codice cliente e tariffa
652300091104      * se da passare a nuova bolla per dirottamento da bolla con triangolo
652400091028     c                   clear                   §lpass
652500091028     c                   if        vfri = 'D' and §ar5ass = 'T'
652600091215     c* se ho il contrassegno e il codice intestatario assegno lo imposto
652700091215     c                   if        §lpcas > 0 and §ar5cdi <> ' '
652800091215     c                   move      §ar5cdi       §lpcdi
652900091215     c                   end
653000091028     c                   movel     §ar5ass       §lpass
653100091104     c                   move      arbksc        §lpksca
653200091104     c                   move      arbctr        §lpctra
653300091028     c                   endif
653400091028     c*
653500091030      * chiodo fedex per resi in c/s
653600091030     c                   if        vfri = 'R' and arblnp = 400
653700091030     C                   movel     '3A'          tblcod
653800091030     C                   movel(P)  ARBcbo        tblkey
653900091030     C     ktab          chain     TABEL
654000091030     C                   movel     TBLUNI        DS3A
654100091030     c                   if        §3arbl = 'C'
654200091030     c                   move      'A '          §lpcbo
654300091030     c                   endif
654400091030     c                   endif
654500091030     c*
654600040527      * imposta flag dati clienti in stampa bolla     O
654700040527     c                   if        wrecsp <> 'S' and vfri <> 'R'
654800040527     c     kbol          chain     fiar801l
654900040527     c                   if        %found(fiar801l)
655000040527     c                   move      'S'           §lpar8
655100040527     c                   endif
655200040527     c                   endif
655300040527     c                   z-add     kaas          §LPAASM
655400040527     c                   z-add     klnp          §LPLNPM
655500040527     c                   z-add     knrs          §LPNRSM
655600040527     c                   z-add     knsp          §LPNSPM
655700050811      *verifica se inserire i codici clienti per la tassazione assegnati
655800050811      * i campi vanno impostati solo se la vecchia bolla era un franco e
655900050811      * la nuova è un assegnato e se il mittente della vecchia bolla è un
656000050826      * codificato nel caso si tratti di bolla di recupero o di reso
656100050826     c                   if        export = 'F' and
656200050811     c                             wvario <> 8888 and wvario <> 9999
656300050826     c                             and (vfri = 'R' or wrecsp = 'S')
656400050811     c                   move      arbksc        §lpksca
656500050811     c                   move      arbctr        §lpctra
656600080123     c* Richiamo routine di controllo codice tariffa per tassazine assegnati
656700080123     c                   EXSR      ContrCTRA
656800050811     c                   endif
656900151222      **
657000151222      **  Attenzione                     il codice Cliente e la tariffa
657100151222      * ? solo nel caso si tratti di un Reso a DPD
657200151222      **   sono quelli presi in tabella LPD
657300151222     c     §ogntw        ifeq      'DPD'
657400151222     C     VFRI          ANDEQ     'R'
657500151222     c     o24npr        andNE     *blank
657600151222     c                   clear                   FIEULPDDS
657700151222     c                   z-add     §LPLNP        ILPDLNP
657800151222     c                   z-add     §LPLNA        ILPDLNA
657900151222     c                   movel     o24npr        ILPDPARCEL
658000151222     c*   il FIEULPDR
658100151222     c                   call      'FIEULPDR'
658200151222     c                   parm                    FIEULPDDS
658300151222     c*
658400151222     c                   eval      dLPD = oLPDUNI
658500151222     c                   move      §LPDkscRE     §lpksca
658600151222     c                   move      §LPDtRE       §lpctra
658700151222      **
658800151222     c                   endif
658900130920     c*
659000130920     c*  Se su AZORG c'è la forzatura per la LNA viene sostituita la
659100130920     c*    LNA con quella di OG143 §ogLNAres
659200130920     c                   exsr      CHG_LNA_xFORZA
659300130920     c*
659400170323     c                   if        wlnpb>0
659500170323     C                   CALL      'FNLS01R'
659600170323     C                   PARM                    KPJBA
659700170323     C                   PARM                    DBLP
659800170323     C                   PARM                    DTASV
659900170323     C                   PARM                    trul90ds
660000170323     C                   PARM                    §lplnp
660100170323     c                   else
660200941013     C                   CALL      'FNLS01R'
660300940308     C                   PARM                    KPJBA
660400990930     C                   PARM                    DBLP
660500990930     C                   PARM                    DTASV
660600030715     C                   PARM                    trul90ds
660700170323     c                   endif
660800150630      ** Errore per non aver generato la BOLLA
660900141030     c                   if        d82gia= 'N' and
661000141030     c                             (§lpfpr = '3' or §lpfpr = '4')
661100141030     c                   if        %parms > 1
661200141202     c                   if        wrecsp = *blank
661300141030     c                   move      'R'           d20erro
661400141202     c                   endif
661500141202     c
661600150630      *** se richiamato da gestione dei DIROTTAMENTI
661700141030     c                   else
661800141118     c                   if        wrecsp = *blank
661900141030     c                   move      '1'           d82f12
662000141118     c                   endif
662100141030     C                   MOVEL     fnlg82        KPJBU
662200141030     c                   endif
662300141118     c                   eval      *inlr = *on
662400141030     c                   return
662500141030     c                   endif
662600110802      * verifico se ho dei rekord di ar4 da riportare sulla bolla
662700110802     c                   exsr      DupAr4
662800030807     C* controllo se devo duplicare le estensioni (fiar5) dalla bolla orig.
662900030807     c* per le bolle figlie (wrecsp = ' ') e per le bolle di recupero
663000030807     c* (wrecsp = 'S')
663100030807     c                   if        wrecsp = ' '
663200030807     c                   movea     fig           ar5
663300030807     c                   else
663400030807     c                   movea     rec           ar5
663500030807     c                   end
663600030807     c                   exsr      AGGAR5
663700140916     c* Se la bolla figlia è un disk "C"  avente come "CHI SONO" il segnacollo
663800140916     c* Bartolini della bolla mamma devo cancellare dal FISGN il "CHI SONO"
663900140916     c* della bolla mamma. Se dovessero sparare il "CHI SONO" della mamma NON
664000140916     c* DEVE uscire l'etichetta, altrimenti uscirebbe il segnacollo BRT della
664100140916     c* mamma e non va bene.
664200140916     c                   exsr      sr_fisgn
664300940308     C* DLTOVR
664400941013     C                   Z-ADD     80            LUNCMD           15 5
664500940308     C                   CALL      'QCMDEXC'
664600940308     C                   PARM                    DOV
664700940308     C                   PARM                    LUNCMD
664800940308     C                   ENDSR
664900030805     C*---------------------------------------------------------------*
665000030821     C*   srpar : Controllo se devo abblencare la particolarità       *
665100030805     C*---------------------------------------------------------------*
665200030807     C     srpar         BEGSR
665300030807     c                   eval      tbecod = 'AR5'
665400030807     c                   clear                   tbeke2
665500030807     c                   eval      tbeke1 = 'RCC'
665600030807     C     ktbe          chain     TNTBE01L
665700030807     c                   if        %found(tntbe01l) and tbeatb = ' '
665800030807     C                   MOVEL     TBEUNI        DAR5
665900030807     c* abblenco la particolarità a seconda se scriverò io il fiar500f
666000030807     c* oppure se lo lascio scrivere al pgm fnls01r
666100030807     c* wrecsp = S sono nel caso che sto scrivendo la bolla di recupero
666200030821     c                   if        (wrecsp=' ' and vfri='D' and §ar5dupd='N') or
666300030821     c                             (wrecsp=' ' and vfri='R' and §ar5dupr='N') or
666400030821     c                             (wrecsp=' ' and vfri='C' and §ar5dupc='N') or
666500030821     c                             (wrecsp='S' and §ar5drec='N')
666600030807     c                   clear                   compar
666700030807     C                   END
666800030807     C                   END
666900030807     C                   ENDSR
667000040123     C*---------------------------------------------------------------*
667100040123     C*   richiama nuovamente tassatore per varie aggiuntive          *
667200040123     C*---------------------------------------------------------------*
667300040123     C     ritassa       BEGSR
667400040129     C                   CLEAR                   DTAS
667500040129     C                   MOVEL     §LPCTR        TASCTR
667600040129     C                   MOVE      KKSC          TASKSC
667700040129     C*
667800040129     C                   MOVEL     §LPCTS        TASCTS
667900040129     C                   MOVEL     §LPCAD        TASCAD
668000040129     C                   MOVEL     §LPCAM        TASCAM
668100040129     C                   MOVEL     §LPTSP        TASTSP
668200040129     C                   MOVEL     ARBTC1        TASTC1
668300040129     C                   MOVEL     ARBTC2        TASTC2
668400040129     C                   MOVEL     ARBFDN        TASFDN
668500040129     C                   MOVEL     §LPVAS        TASVAS
668600040129     C                   Z-ADD     §LPIAS        TASIAS
668700040129     C                   Z-ADD     ARBQFT        TASQFT
668800040129     C                   Z-ADD     ARBNCL        TASNCL
668900160530     C                   Z-ADD     ARBNCL        TASNCLb
669000040129     C                   Z-ADD     AMGUDT        TASDSP
669100040129     C                   Z-ADD     TASDSP        TASDCT
669200040129     C                   Z-ADD     §LPLNP        TASLNP
669300040129     C                   Z-ADD     VLNA          TASLNA
669400040129     C                   Z-ADD     ARBVLF        TASVLF
669500040129     C                   Z-ADD     ARBPKF        TASPKF
669600160530     C                   Z-ADD     ARBvlc        TASvlc
669700160530     C                   Z-ADD     ARBncr        TASncr
669800160530     C                   move      ARBfvf        TASfvf
669900160530     C                   Z-ADD     ARBpkb        TASPKb
670000040129     C                   CLEAR                   DS3A
670100040129     C                   Z-ADD     1             §F                2 0
670200040129     C     §LPCBO        LOOKUP    CDB(§F)                                21
670300040129     C   21              MOVEL     D3A(§F)       DS3A
670400040129     C   21              MOVEL     §3ATB1        TASTBL
670500040129     C* Reperisco codice tassazione
670600040129     C                   CLEAR                   TISI95
670700040129     C     TASLNP        CHAIN     AZORG01L                           21
670800040129     C     *IN21         IFEQ      '0'
670900040129     C                   MOVE      ORGDE3        OG143
671000040129     C     §OGNTW        IFEQ      'FED'
671100040129     C                   MOVE      'S'           I95FI1
671200040129     C                   END
671300040129     C                   END
671400040129     C                   MOVEL     '3'           I95TCN
671500040129     C                   MOVEL     §LPCAM        I95CAP
671600040129     C                   MOVEL     §LPNZM        I95NAR
671700040129     C                   MOVEL     §LPPRM        I95PRV
671800040129     C                   MOVEL     §LPLOM        I95LOC
671900040129     C                   MOVEL     §LPINM        I95IND
672000040129     C                   MOVEL     ARBTFP        I95TFP
672100040129     C                   MOVEL     ARBTSP        I95TSP
672200040129     C                   MOVEL     'S'           I95FRE
672300040129     C                   MOVEL     §LPNZM        I95NAR
672400040129     C                   MOVEL     AMGUDT        I95DAT
672500040129     C                   MOVEL     ARBtc1        I95fi2
672600040129     C                   MOVE      ARBtc2        I95fi2
672700040129     C                   CALL      'TISI95R'
672800040129     C                   PARM                    TISI95
672900040129     C                   MOVEL     O95CTS        TASMCT
673000040129     C*
673100040506     C                   MOVEL     ar9vca        TASVCA
673200040129     C                   Z-ADD     §ASS          TASCAS
673300040129     C                   MOVEL     VTIC          TASTIC
673400040129     C*
673500040129     C                   MOVE      §LPBAM        TASDDT
673600040129     c* imposto peso vdl e colli rilevati
673700040129     c                   z-add     arbncp        Tasncp
673800040129     c                   z-add     arbpkc        Taspkc
673900060927     C                   movel     ARBfbr        §asfbr
674000060927     C                   movel     ARBcca        §AScca
674100060927     C                   movel     dtas01        TASflo
674200040123     C*
674300040123     C                   MOVEL     'Z'           TASSVA
674400040123     C*
674500040123     C                   CALL      'TNSF20R'
674600040123     C                   PARM                    KPJBA
674700040123     C                   PARM                    DTAS
674800040123     C                   PARM                    DTASV
674900040123     C*
675000040123     C     TASERR        IFEQ      *BLANKS
675100040202     c                   z-add     0             §lpimv
675200040123     C* Scorro le varie e le sommo nel totale imponibile solo se il
675300040123     C* flag tab cc lo prevede
675400040123     C                   DO        20            WW                3 0
675500040123     C                   Z-ADD     1             XW                3 0
675600040123     C     NSV(WW)       LOOKUP    CCC(XW)                                20
675700040123     C     *IN20         IFEQ      '1'
675800040123     C     IPB(XW)       ANDEQ     'S'
675900040123     C                   ADD       NVA(WW)       §LPIMV
676000040123     C                   END
676100040123     C                   END
676200040123     C                   END
676300040123     C                   ENDSR
676400030807     C*---------------------------------------------------------------*
676500030807     C*   CTRSTB: Controllo se devo ristampare la bolla               *
676600030807     C*---------------------------------------------------------------*
676700030807     C     CTRSTB        BEGSR
676800951206     C*  Aggancio giacenza
676900050316     C     KGIA          CHAIN(e)  tigcp02L                           40
677000050316     c                   if        %error
677100070521     C     KGIA          CHAIN(n)  tigcp02L
677200050316     c                   exsr      geslck
677300050318     C     KGIA          CHAIN(e)  tigcp02L                           40
677400050316     c                   endif
677500951206     C     *IN40         IFEQ      '0'
677600951206     C                   MOVE      VAAS          KAAS1
677700951206     C                   MOVE      VLNP          KLNP1
677800951206     C                   MOVE      VNRS          KNRS1
677900951206     C                   MOVE      VNSP          KNSP1
678000951206     C                   MOVE      VNSP          KNSP1
678100951206     C                   MOVE      GCPDXD        KDEV
678200951206     C                   MOVE      *HIVAL        KHEV
678300990803     C     KEVB1         SETGT     FNEVB01L
678400990803     C     KEVB2         READPE    FNEVB01L                               45
678500951206     C     *IN45         IFEQ      '0'
678600081219     C                   Z-ADD     1             XY
678700951206     C     EVBCEV        LOOKUP    C2A(XY)                                45
678800951206     C     *IN45         IFEQ      '1'
678900951206     C                   MOVEL     D2A(XY)       DS2A
679000951206     C     §2AFTC        IFEQ      'A'
679100000313     C* Per bolle export e DPD non ristampo il laciato avviso x
679200000313     C* la consegna
679300000313     C     ARBLNA        CHAIN     AZORG01L                           21
679400000313     C                   MOVEL     ORGDE3        OG143
679500020729     C                   clear                   flgest            1
679600020729     C     §OgNTW        ifeq      'FED'
679700020729     C     §OgNTW        oreq      'EEX'
679800020729     C     §OgNTW        oreq      'EUP'
679900020729     C                   MOVEL     'E'           flgest            1
680000020729     c                   end
680100020729     C     Flgest        IFNE      'E'
680200020617     C     §OGntw        ANDNE     'DPD'
680300951206     C* RICH. IMMISS. BOLLE
680400030715     C* OVRPRTF  BOLLE old
680500951206     C                   Z-ADD     80            LUNCMD           15 5
680600951206     C                   MOVEA     D82PRB        CMB(31)
680700951206     C                   MOVEA     D82MOB        CMB(64)
680800951206     C                   CALL      'QCMDEXC'
680900951206     C                   PARM                    CMB
681000951206     C                   PARM                    LUNCMD
681100030715     C* OVRPRTF  BOLLE a4
681200030715     C                   MOVEA     D82PRB4       CM4(31)
681300030715     C                   MOVEA     D82Mdb4       CM4(64)
681400030715     C                   CALL      'QCMDEXC'
681500030715     C                   PARM                    CM4
681600030715     C                   PARM                    LUNCMD
681700030715     C* OVRPRTF  BOLLE a5
681800030715     C                   MOVEA     D82PRB5       CM5(31)
681900030715     C                   MOVEA     D82Mdb5       CM5(64)
682000030715     C                   CALL      'QCMDEXC'
682100030715     C                   PARM                    CM5
682200030715     C                   PARM                    LUNCMD
682300951206     C*  Richiamo pgm stampa bolle
682400130315     c                   reset                   FNLSB6ds1
682500030624     C                   CLEAR                   FNLSB5DS
682600951206     C                   MOVEL     ' '           DB0TLA
682700951206     C                   MOVEL     ' '           DB0GES
682800951206     C                   MOVEL     'A'           DB0TBO
682900951206     C                   MOVE      VAAS          DB0AAS
683000951206     C                   MOVE      VLNP          DB0LNP
683100951206     C                   MOVE      VNRS          DB0NRS
683200951206     C                   MOVE      VNSP          DB0NSP
683300030715     C                   CALL      d90psl
683400030624     C                   PARM                    FNLSB5DS
683500130315     c                   parm                    FNLSB6ds1
683600000313     C                   END
683700951206     C* MI RITORNA IL NR. BOLLA CREATO
683800951206     C* DLTOVR
683900951206     C                   Z-ADD     80            LUNCMD           15 5
684000951206     C                   CALL      'QCMDEXC'
684100951206     C                   PARM                    DOV
684200951206     C                   PARM                    LUNCMD
684300951206     C                   END
684400951206     C                   END
684500951206     C                   END
684600951206     C                   END
684700951206     C*
684800951206     C                   ENDSR
684900951206     C*---------------------------------------------------------------*
685000941220     C*   A G G C C M  FORZO COD.CLIENTE MITTENTE                     *
685100940308     C*---------------------------------------------------------------*
685200941220     C     AGGCCM        BEGSR
685300941220     C* IN CASO DI RESO
685400971230     C     VFRI          IFEQ      'R'
685500971230     C     V4CRCS        IFEQ      'N'
685600941220     C                   MOVE      9999          §LPCCM
685700941220     C                   MOVEL     §LPLNP        §LPCCM
685800971230     C                   ELSE
685900971230     C                   MOVE      8888          §LPCCM
686000960328     C                   MOVEL     §LPLNP        §LPCCM
686100960328     C                   END
686200941220     C                   ELSE
686300971230     C     WRECSP        IFEQ      'S'
686400020625     c                   if        *in92
686500020625     c                   move      arbksc        §lpccm
686600020625     c                   else
686700971230     C                   MOVE      9999          §LPCCM
686800951227     C                   MOVEL     §LPLNP        §LPCCM
686900020625     c                   end
687000951214     C                   ELSE
687100971230     C* NEGLI ALTRI CASI  IMPOSTO IL CLIENTE MITTENTE
687200941003     C     ARBCCM        IFGT      0
687300941003     C                   MOVE      ARBCCM        §LPCCM
687400941003     C                   ELSE
687500941003     C     EXPORT        IFEQ      'F'
687600941003     C                   MOVE      ARBKSC        §LPCCM
687700940308     C                   END
687800940308     C                   END
687900941220     C* SE NON SONO RIUSCITA AD IMPOSTARLO FORZO CLI.VARIO
688000941220     C     §LPCCM        IFEQ      *ZEROS
688100941220     C     VNPC          IFEQ      'F'
688200941220     C                   MOVE      *ALL'8'       §LPCCM
688300941220     C                   MOVEL     §LPLNP        §LPCCM
688400941220     C                   END
688500941220     C     VNPC          IFEQ      'A'
688600941220     C     VNPC          OREQ      ' '
688700941220     C     EXPORT        ANDEQ     'A'
688800941220     C                   MOVE      *ALL'9'       §LPCCM
688900941220     C                   MOVEL     §LPLNP        §LPCCM
689000941220     C                   END
689100971230     C                   ENDIF
689200971230     C                   ENDIF
689300971230     C                   ENDIF
689400950130     C                   MOVEL     §LPCCM        §LIN              3 0
689500950130     C                   MOVE      §LPCCM        §APPO             4 0
689600950130     C     §LIN          IFNE      §LPLNP
689700950130     C     §APPO         ANDEQ     8888
689800950130     C                   MOVEL     §LPLNP        §LPCCM
689900950130     C                   END
690000020625      * la move era incondizionata
690100020625     c                   if         *in92
690200020625     c                   movel     arblna        §lpccn
690300020625     c                   move      9999          §lpccn
690400020625     c                   else
690500941220     C                   MOVE      §LPCCM        §LPCCN
690600020625     c                   end
690700971230     C*
690800940308     C                   ENDSR
690900940309     C*---------------------------------------------------------------*
691000941011     C*   A G G A R 4           FILE NOTE                             *
691100940309     C*---------------------------------------------------------------*
691200941003     C     AGGAR4        BEGSR
691300940309     C                   MOVE      '0'           §TRC
691400060213     C     KAR4          CHAIN     FiAR401L                           21
691500940309     C                   MOVE      §LPAAS        NEWAAS
691600940309     C                   MOVE      §LPLNP        NEWLNP
691700940309     C                   MOVE      §LPNRS        NEWNRS
691800940309     C                   MOVE      §LPNSP        NEWNSP
691900940309     C                   MOVE      §LPLNA        NE2LNA
692000061025     C                   MOVEL(p)  NEWBOL        AR4NOT
692100941003     C   21              MOVE      §TRC          AR4TRC
692200941216     C   21              MOVE      KAAS          AR4AAS
692300941216     C   21              MOVE      KLNP          AR4LNP
692400941216     C   21              MOVE      KNRS          AR4NRS
692500941216     C   21              MOVE      KNSP          AR4NSP
692600060213     C  N21              UPDATE    FiAR4000
692700060213     C   21              WRITE     FiAR4000
692800940309     C                   ENDSR
692900030806     C*---------------------------------------------------------------*
693000030806     C* AGGAR5: Controllo se devo duplicare fiar500f                  *
693100030806     C*---------------------------------------------------------------*
693200030806     C     aggar5        BEGSR
693300030806     c                   if        §lpnsp <> 0
693400030806     c                   do        30            bb
693500030806     c                   if        ar5(bb) = *blanks
693600030806     c                   leave
693700030806     c                   end
693800030806     C                   movel     ar5(bb)       trcar5
693900051013     c     kFiar5        Chain(n)  Fiar501l
694000030806     c                   If        %Found(Fiar501l)
694100030806     c                   clear                   ar5prg
694200030911     c                   eval      ar5ft1 = 'T'
694300030911     c                   eval      ar5dt1 = 99999999
694400030911     c                   eval      ar5ft2 = 'T'
694500030911     c                   eval      ar5dt2 = 99999999
694600030806     c                   clear                   ar5ft3
694700030806     c                   clear                   ar5dt3
694800030806     c                   move      amgudt        ar5dac
694900030806     C                   TIME                    ar5ORc
695000030806     c                   eval      ar5aas = §lpaas
695100030806     c                   eval      ar5lnp = §lplnp
695200030806     c                   eval      ar5nrs = §lpnrs
695300030806     c                   eval      ar5nsp = §lpnsp
695400030806     c                   write     fiar50
695500030806     c                   end
695600030806     c                   enddo
695700030806     c                   end
695800030806     C                   ENDSR
695900110802     C*---------------------------------------------------------------*
696000110802     C*   duplica rekord di ar4 da riportare su nuova bolla           *
696100110802     C*---------------------------------------------------------------*
696200110802     C     DupAr4        BEGSR
696300110802     C     Kar4dup       setll     FiAR401L
696400110802     C                   do        *hival
696500110802     C     Kar4dup       reade     FiAR401L
696600110802     c                   if        %eof(fiar401l)
696700110802     c                   leave
696800110802     c                   endif
696900110802     C                   MOVEL     '7K'          TBLCOD
697000110802     C                   MOVEL     *BLANK        TBLKEY
697100110802     C                   MOVEL     ar4trc        TBLKEY
697200110802     c     ktab          chain     tabel00f
697300110802     c                   if        %found(tabel00f)
697400110802     c                   movel     tbluni        ds7k
697500110802     c                   if        §7KDUPD = 'S' and vfri = 'D' or
697600110802     c                             §7KDUPR = 'S' and vfri = 'R' or
697700110802     c                             §7KDUPC = 'S' and vfri = 'C' or
697800110802     c                             §7KDREC = 'S' and Wrecsp = 'S'
697900110802     c
698000110802     C                   MOVE      §LPAAS        AR4AAS
698100110802     C                   MOVE      §LPLNP        AR4LNP
698200110802     C                   MOVE      §LPNRS        AR4NRS
698300110802     C                   MOVE      §LPNSP        AR4NSP
698400110802     c                   clear                   AR4DUV
698500110802     c                   clear                   AR4FTR
698600110802     c                   clear                   AR4DTR
698700110802     C                   WRITE     FiAR4000
698800110802     c                   endif
698900110802     c                   endif
699000110802     c                   enddo
699100110802     C                   ENDSR
699200030826     C*-----------------------------------------------------------------*
699300030826     C* AGGORM x particolarità RC aggiorno anche l'orm della bolla orig.*
699400030826     C*-----------------------------------------------------------------*
699500030826     C     aggorm        BEGSR
699600100517
699700100517     c* se particolarità consegna divesra da blan testo il tipo per
699800100517     c* verificare se trattasi di particolarità con ritiro contestuale
699900100517     C                   clear                   DS7R
700000100517     c                   if        arbgma <>*blank
700100100517     C                   MOVEL     '7R'          TBLCOD
700200100517     C                   MOVEL     *BLANK        TBLKEY
700300100517     C                   MOVEL     ARBGMA        TBLKEY
700400100517     C     KTAB          CHAIN     TABEL                              21
700500100517     C     *IN21         IFEQ      '0'
700600100517     C                   MOVEL     TBLUNI        DS7R
700700100517     c                   endif
700800100517     c                   endif
700900100517     c
701000100517     c                   if        §lpnsp <> 0 and §7rrc  = 'S' and
701100031010     c                             (vfri<>'C' or (vfri='C' and §lplna<>arblna))
701200030826     c                   clear                   fnlr99ds
701300030826     c                   eval      i99tla = 'L'
701400030826     c                   eval      i99aas = arbaas
701500030826     c                   eval      i99lnp = arblnp
701600030826     c                   eval      i99nrs = arbnrs
701700030826     c                   eval      i99nsp = arbnsp
701800030826     c                   eval      i99fao = 999
701900071002     c                   eval      i99fgs = filarr
702000030826     c                   if        vfri = 'R'
702100030826     c                   eval      i99car = '91'
702200030826     c                   else
702300030826     c                   eval      i99car = '90'
702400030826     c                   eval      i99lna = §lplna
702500030826     c                   end
702600030826     c                   call      'FNLR99R'
702700030826     c                   parm                    kpjba
702800030826     c                   parm                    fnlr99ds
702900030826     c                   end
703000030826     C                   ENDSR
703100030826     C*---------------------------------------------------------------*
703200030826     C*   N O T D E S           note x modifica destinatario          *
703300030826     C*---------------------------------------------------------------*
703400030826     C     NOTDES        BEGSR
703500960124     C*
703600960124     C*  Prima ragione sociale
703700960124     C                   MOVE      'R'           §TRC
703800060213     C     KAR4          CHAIN     FiAR401L                           21
703900960129     C                   CLEAR                   AR4NOT
704000971124     C                   CLEAR                   AR4DTR
704100971124     C                   CLEAR                   AR4FTR
704200960124     C                   MOVEL     VRSD          AR4NOT
704300960124     C     *IN21         IFEQ      '1'
704400960124     C                   MOVE      §TRC          AR4TRC
704500960124     C                   MOVE      KAAS          AR4AAS
704600960124     C                   MOVE      KLNP          AR4LNP
704700960124     C                   MOVE      KNRS          AR4NRS
704800960124     C                   MOVE      KNSP          AR4NSP
704900060213     C                   WRITE     FiAR4000
705000960124     C                   ELSE
705100060213     C                   UPDATE    FiAR4000
705200960124     C                   END
705300960124     C*  Seconda ragione sociale
705400960125     C     VRD2          IFNE      *BLANKS
705500960124     C                   MOVE      'S'           §TRC
705600060213     C     KAR4          CHAIN     FiAR401L                           21
705700960129     C                   CLEAR                   AR4NOT
705800960124     C                   MOVEL     VRD2          AR4NOT
705900960124     C     *IN21         IFEQ      '1'
706000960124     C                   MOVE      §TRC          AR4TRC
706100960124     C                   MOVE      KAAS          AR4AAS
706200960124     C                   MOVE      KLNP          AR4LNP
706300960124     C                   MOVE      KNRS          AR4NRS
706400960124     C                   MOVE      KNSP          AR4NSP
706500060213     C                   WRITE     FiAR4000
706600960124     C                   ELSE
706700060213     C                   UPDATE    FiAR4000
706800960124     C                   END
706900960125     C                   END
707000960124     C*  Indirizzo
707100960124     C                   MOVE      'T'           §TRC
707200060213     C     KAR4          CHAIN     FiAR401L                           21
707300960129     C                   CLEAR                   AR4NOT
707400960124     C                   MOVEL     VIND          AR4NOT
707500960124     C     *IN21         IFEQ      '1'
707600960124     C                   MOVE      §TRC          AR4TRC
707700960124     C                   MOVE      KAAS          AR4AAS
707800960124     C                   MOVE      KLNP          AR4LNP
707900960124     C                   MOVE      KNRS          AR4NRS
708000960124     C                   MOVE      KNSP          AR4NSP
708100060213     C                   WRITE     FiAR4000
708200960124     C                   ELSE
708300060213     C                   UPDATE    FiAR4000
708400960124     C                   END
708500960124     C*  Località
708600960124     C                   MOVE      'U'           §TRC
708700060213     C     KAR4          CHAIN     FiAR401L                           21
708800960129     C                   CLEAR                   AR4NOT
708900960124     C                   MOVEL     VLOD          AR4NOT
709000960124     C     *IN21         IFEQ      '1'
709100960124     C                   MOVE      §TRC          AR4TRC
709200960124     C                   MOVE      KAAS          AR4AAS
709300960124     C                   MOVE      KLNP          AR4LNP
709400960124     C                   MOVE      KNRS          AR4NRS
709500960124     C                   MOVE      KNSP          AR4NSP
709600060213     C                   WRITE     FiAR4000
709700960124     C                   ELSE
709800060213     C                   UPDATE    FiAR4000
709900960124     C                   END
710000960124     C*  Cap./Provincia/Nazione
710100960124     C                   MOVE      'V'           §TRC
710200060213     C     KAR4          CHAIN     FiAR401L                           21
710300960124     C                   MOVEL     VCAD          WCAD
710400960124     C                   MOVEL     VNZD          WNZD
710500960124     C                   MOVEL     VPRD          WPRD
710600960129     C                   CLEAR                   AR4NOT
710700960124     C                   MOVEL     WNOT          AR4NOT
710800960124     C     *IN21         IFEQ      '1'
710900960124     C                   MOVE      §TRC          AR4TRC
711000960124     C                   MOVE      KAAS          AR4AAS
711100960124     C                   MOVE      KLNP          AR4LNP
711200960124     C                   MOVE      KNRS          AR4NRS
711300960124     C                   MOVE      KNSP          AR4NSP
711400060213     C                   WRITE     FiAR4000
711500960124     C                   ELSE
711600060213     C                   UPDATE    FiAR4000
711700960124     C                   END
711800960124     C*
711900960124     C                   ENDSR
712000940309     C/SPACE 3
712100940309     C*---------------------------------------------------------------*
712200940309     C*   A G G A R T           FILE DETTAGLIO SEGNACOLLI E ANOMALIE  *
712300940309     C*---------------------------------------------------------------*
712400940309     C     AGGART        BEGSR
712500940309     C*
712600941003     C     KBOL          SETLL     FNART01L
712700940309     C                   DO        *HIVAL
712800941003     C                   SETOFF                                           21
712900941006     C     KBOL          READE     FNART01L                               21
713000960618     C  N21              DO
713100950214     C                   Z-ADD     ARBDCM        ARTDCM
713200950214     C                   MOVEL     *BLANKS       ARTDAB
713300950214     C* Se manca data arrivo imposto = Data consegna
713400950214     C     ARTDAM        IFEQ      0
713500950215     C                   Z-ADD     ARBDCM        ARTDAM
713600950214     C                   Z-ADD     85            ARTNAP
713700950214     C                   END
713800970204     C* Se DIROTTAMENTO IMPOSTO 99999999 IN DTR
713900970204     C     VFRI          IFEQ      'D'
714000970204     C                   Z-ADD     99999999      ARTDTR
714100970204     C                   END
714200950214     C                   UPDATE    FNART000
714300940309     C* CONTROLLO SE ESISTONO DELLE ANOMALIE
714400990518     C     KANM          SETLL     FNANM02L
714500960618     C                   DO        *HIVAL
714600940309     C                   SETOFF                                       73
714700990518     C     KANM          READE     FNANM02L                               73
714800940309     C* CHIUDO ANOMALIA
714900960618     C     *IN73         IFEQ      '0'
715000960618     C                   CLEAR                   DSLR33
715100960618     C                   MOVE      '4'           D33FSC
715200960618     C                   Z-ADD     AMGUDT        D33DCH
715300960618     C                   MOVEL     'CO'          D33CCH
715400960618     C                   MOVEL     ARBCCA        D33CCA
715500960618     C                   Z-ADD     ANMNR2        D33NRR
715600960618     C                   CALL      'FNLR33R'
715700960618     C                   PARM                    DSLR33
715800960618     C                   END
715900960618     C  N73              END
716000960618     C                   ENDDO
716100940309     C  N21              END
716200040315     C*
716300940309     C                   ENDSR
716400940309     C/SPACE 3
716500940309     C*---------------------------------------------------------------*
716600941005     C*   E V E N T O  - SCRITTURA EVENTO X DIROTTAM. O CAMBIO PORTO  *
716700940309     C*---------------------------------------------------------------*
716800941005     C     EVENTO        BEGSR
716900941005     C                   MOVEL     *BLANKS       EVBATB
717000941005     C                   Z-ADD     AMGUDT        EVBDTV
717100941005     C                   TIME                    EVBORV
717200021009     C                   MOVE      simpou        EVBFLE
717300021114     C                   Z-ADD     o36cou        EVBCDU
717400941216     C                   Z-ADD     KLNP          EVBLNP
717500941216     C                   Z-ADD     KAAS          EVBAAS
717600941216     C                   Z-ADD     KNRS          EVBNRS
717700941216     C                   Z-ADD     KNSP          EVBNSP
717800941005     C                   Z-ADD     AMGUDT        EVBDEV
717900941005     C                   MOVEL     EVBORV        EVBHEV
718000941005     C     VFRI          IFEQ      'D'
718100941005     C                   MOVEL     'DIR'         EVBCEV
718200941005     C                   ELSE
718300941005     C                   MOVEL     'CPO'         EVBCEV
718400941005     C                   ENDIF
718500941005     C                   MOVEL     *BLANKS       EVBFTR
718600941005     C                   Z-ADD     0             EVBDTR
718700990803     C                   MOVEL     *BLANKS       EVBNOT
718800990803     C                   Z-ADD     0             EVBDTC
718900941005     C*
719000990803     C                   WRITE     FNEVB000
719100941005     C*
719200941005     C                   ENDSR
719300941005     C*---------------------------------------------------------------*
719400941005     C*   P G M N A C                                                 *
719500941005     C*---------------------------------------------------------------*
719600941005     C     RCHNAC        BEGSR
719700940309     C*
719800940309     C                   MOVEL     *ALL'?'       DLNP
719900940309     C     ARBLNP        CHAIN     AZORG01L                           21
720000940309     C  N21              MOVEL     ORGDES        DLNP             15
720100940309     C   21              MOVEL     *ALL'?'       DLNP
720200940309     C     ARBLNA        CHAIN     AZORG01L                           21
720300940309     C  N21              MOVEL     ORGDES        DLNA             15
720400940309     C   21              MOVEL     *ALL'?'       DLNA
720500951214     C     WSVLNA        CHAIN     AZORG01L                           21
720600940309     C  N21              MOVEL     ORGDES        DNWLNA           15
720700940309     C   21              MOVEL     *ALL'?'       DNWLNA
720800941006     C                   MOVE      AR6DFT        G08INV
720900941005     C                   MOVE      *ZEROS        G08DAT
721000941005     C                   MOVEL     '3'           G08ERR
721100941005     C                   CALL      'XSRDA8'
721200941005     C                   PARM                    WLBDA8
721300941005     C                   Z-ADD     G08DAT        GMADFT            8 0
721400940630     C     VNPC          COMP      'F'                                    28
721500940309     C* DA MANDARE DIRETTAMENTE A SEDE : RECUPERO GIA FATTO
721600950301     C  N98VFRI          IFEQ      'R'
721700940309     C                   SETON                                        86
721800940309     C                   END
721900941011      * SE HO DIROTTATO IN ASSEGNATO O RICONS. IN ASSEGNATO
722000950301     C  N98VFRI          IFEQ      'D'
722100940309     C     VFRI          OREQ      'C'
722200940309     C     VNPC          IFEQ      'A'
722300940309     C     VNPC          OREQ      ' '
722400940309     C     EXPORT        ANDEQ     'A'
722500941221      * RICH. NOTA ACCREDITO 2° BOLLA E PROVV. AL DESTIN.
722600941221     C     *IN37         OREQ      '1'
722700941221     C     VPPC          ANDEQ     'D'
722800940309     C                   SETON                                        86
722900940309     C                   END
723000940309     C                   END
723100940623     C*   Stampa Nota Accredito
723200950301     C     *IN98         IFEQ      '0'
723300991001     C                   Z-ADD     ARBKSC        CLIENT            7 0
723400991001     C     AR6KSC        IFNE      0
723500991001     C                   Z-ADD     AR6KSC        CLIENT            7 0
723600991001     C                   END
723700000131     C                   Z-ADD     0             TOTIMP           12 3
723800000131     C                   Z-ADD     0             TOTESE           12 3
723900000131     C                   Z-ADD     0             TOTNAC           12 3
724000940623     C     *IN32         IFEQ      '1'
724100991001     C                   Z-ADD     1             XS
724200991130     C     §$2IVA        LOOKUP    ASV(XS)                                20
724300991001     C     *IN20         IFEQ      '1'
724400020108     C     AR6IFT        SUB       AR6IMV        IMPALI           10 3
724500991001     C                   END
724600991027     C                   Z-ADD     §INAC1        INAC             12 3
724700991027     C* VERIFICO SE IMPORTO HA DECIMALI
724800991027     C                   MOVE      INAC          WCOMO3            3 0
724900991027     C     WCOMO3        COMP      0                                      11
725000941005     C                   Z-ADD     AR6ALI        ALIQ              3 1
725100020621     C                   EVAL      CdIVA = %EDITC(Aliq:'2')
725200941005     C                   MOVEL     AR6CEI        TPALI             1
725300940623     C                   END
725400940623     C     *IN37         IFEQ      '1'
725500991001     C                   Z-ADD     1             XS
725600991130     C     §$2IVA        LOOKUP    ASV(XS)                                20
725700991001     C     *IN20         IFEQ      '1'
725800020108     C     AR6IFT        SUB       AR6IMV        IMPALI
725900991001     C                   END
726000991027     C                   Z-ADD     §INAC2        INAC
726100991001     C                   Z-ADD     AR6ALI        ALIQ              3 1
726200020621     C                   EVAL      CdIVA = %EDITC(Aliq:'2')
726300991001     C                   MOVEL     AR6CEI        TPALI             1
726400940623     C                   END
726500011130      * STAMPA DOPPIO IMPORTO SE DIVISA NON DI CONTABILITÀ
726600011130     C                   CLEAR                   WAR6IM
726700011130     C     AR6DIV        IFNE      §GEDMC
726800011130     C                   CLEAR                   YEURDS
726900011130     C                   MOVEL     AR6DIV        YECDVI
727000011130     C                   Z-ADD     AR6IFT        YECIMI
727100011212     C                   MOVEL     §GEDMC        YECDVO
727200011130     C                   MOVEL     'H'           YECTAR
727300011130     C                   CALL      'YEURCO'
727400011130     C                   PARM                    YEURDS
727500011130     C                   Z-ADD     YECIMO        WAR6IM           13 3
727600011130     C                   ENDIF
727700011130      *
727800940623     C     TPALI         IFEQ      *BLANKS
727900000131     C                   Z-ADD     INAC          TOTIMP
728000940623     C                   ELSE
728100000131     C                   Z-ADD     INAC          TOTESE
728200940624     C                   Z-ADD     1             XX
728300940624     C     TPALI         LOOKUP    CEI(XX)                                41
728400940624     C     *IN41         IFEQ      '1'
728500020621     C                   MOVEL     ESE(XX)       CdIVA
728600940623     C                   END
728700940624     C                   MOVEL     'E'           TPALI
728800940624     C                   END
728900040315     c                   clear                   Worigine
729000040316     c                   clear                   Wpocompl
729100940623     C     VFRI          IFEQ      'D'
729200040423     c                   eval      worigine = 'DIROTTATA A ' + dnwlna
729300040317      *se dirottamento e linea arrivo 101 forzo sede
729400040317     c                   if        arblna = 101
729500040316     c                   eval      wpocompl = 046
729600040316     c                   endif
729700940623     C                   END
729800040316
729900940623     C     VFRI          IFEQ      'C'
730000040423     c   28              eval      worigine = 'CONSEGNATA IN FRANCO A ' + DNWLNA
730100040423     c  N28              eval      worigine = 'CONSEGNATA A: ' + DNWLNA
730200940623     C                   END
730300940623     C     VFRI          IFEQ      'R'
730400960327     C     V4CRCS        IFEQ      'N'
730500040315     c                   eval      worigine = 'LA MERCE E'' STATA RESA AL -
730600040315     C                             MITTENTE '
730700960327     C                   ELSE
730800040315     c                   eval      worigine = 'LA MERCE E'' STATA RESA AL -
730900040315     C                             MITTENTE IN C/SERVIZIO'
731000960327     C                   END
731100960327     C                   END
731200011130      * STAMPA DOPPIO IMPORTO SE DIVISA NON DI CONTABILITÀ
731300011130     C                   CLEAR                   WAR6IM
731400011130     C     AR6DIV        IFNE      §GEDMC
731500011130     C                   CLEAR                   YEURDS
731600011130     C                   MOVEL     AR6DIV        YECDVI
731700011130     C                   Z-ADD     INAC          YECIMI
731800011212     C                   MOVEL     §GEDMC        YECDVO
731900011130     C                   MOVEL     'H'           YECTAR
732000011130     C                   CALL      'YEURCO'
732100011130     C                   PARM                    YEURDS
732200011130     C                   Z-ADD     YECIMO        WAR6IM
732300011130     C                   ENDIF
732400011130      *
732500940623     C     TOTIMP        ADD       IMPALI        TOTNAC
732600940623     C                   ADD       TOTESE        TOTNAC
732700000131     C* VERIFICO SE IMPORTI CON DECIMALI
732800000131     C                   MOVE      TOTIMP        WCOMO3            3 0
732900000131     C     WCOMO3        COMP      0                                      08
733000000131     C                   MOVE      IMPALI        WCOMO3            3 0
733100000131     C     WCOMO3        COMP      0                                      09
733200000131     C                   MOVE      TOTESE        WCOMO3            3 0
733300000131     C     WCOMO3        COMP      0                                      10
733400000131     C                   MOVE      TOTNAC        WCOMO3            3 0
733500000131     C     WCOMO3        COMP      0                                      11
733600950301     C                   ELSE
733700950301     C                   END
733800040312      * scrittura file NCR
733900040312     c                   clear                   fnncr000
734000040315     c                   move      '201'         nctsocieta
734100040315     c                   z-add     arblna        nctfil
734200040316     c                   if        wpocompl <> *zeros
734300040316     c                   z-add     wpocompl      nctpocompl
734400040316     c                   else
734500040315     c                   z-add     arblnp        nctpocompl
734600040316     c                   endif
734700040315     c                   move      amgudt        nctddc
734800040315     c                   if        ar6dft > 0
734900040315     c                   move      ar6dft        nctdrp
735000040315     c                   end
735100040315     c                   z-add     ar6nft        nctnrp
735200040315     c                   z-add     ar6fiv        nctfpn
735300040315     c                   z-add     ar6ift        nctimr
735400040315     c                   move      *zeros        nctksc
735500040315     c                   move      ar6ksc        nctksc
735600040315     c                   move      *zeros        nctscf
735700040315     c                   move      ar6ksc        nctscf
735800040312     c                   move      knmus         nctutente
735900040312     c                   move      'I '          ncttnr
736000040312     c                   move      'C'           ncrrna
736100040312     c                   move      '3'           ncrcmn
736200040312      *reso
736300040429     c                   if        vfri ='R'
736400040312     c                   move      '4'           ncrcmn
736500040429     c                   endif
736600040429      *reso ma in C/S
736700040429     c                   if        vfri ='R' and v4crcs = 'S'
736800040429     c                   move      '0'           ncrcmn
736900040429     c                   move      'D'           ncrrna
737000040312     c                   endif
737100040312      *dirottamento non da giacenza
737200040312     c                   if        vfri ='D' and not *in90 and viddcli = 'V'
737300040312     c                   move      'A'           ncrrna
737400040312     c                   endif
737500040916      *se dirottamento e linea arrivo 101 forzo causale
737600041221     c                   if        arblna = 101 and vfri = 'D' or
737700041221     c                             §lplna = 101 and vfri = 'D'
737800040916     c                   move      '5'           ncrcmn
737900040916     c                   move      'C'           ncrrna
738000040916     c                   endif
738100040312      * storno spese giacenza
738200040312     c                   z-add     ar6imv        ncrim2
738300040312     c                   z-add     ar6ali        ncrali
738400040312     c                   z-add     arblnp        ncrflr
738500040312     c                   z-add     exaas         ncraassto
738600040312     c                   z-add     exlnp         ncrlnpsto
738700040315     c                   z-add     exnrs         ncrnrssto
738800040312     c                   z-add     exnsp         ncrnspsto
738900040429      *con causale 'D0' la spedizione di recupero non viene impostata
739000040429      *
739100040429     c                   if        ncrrna = '0' and ncrcmn = 'D'
739200040429     c                   z-add     0             ncraasrec
739300040429     c                   z-add     0             ncrlnprec
739400040429     c                   z-add     0             ncrnrsrec
739500040429     c                   z-add     0             ncrnsprec
739600040429     c                   z-add     0             nchlnarec
739700040429     c                   else
739800040429     c                   select
739900040429     c                   when      vfri = 'D' and vnpc = 'F'
740000040429     c                   z-add     0             ncraasrec
740100040429     c                   z-add     0             ncrlnprec
740200040429     c                   z-add     0             ncrnrsrec
740300040429     c                   z-add     0             ncrnsprec
740400040429     c                   z-add     0             nchlnarec
740500100514     c*                  when      vfri = 'D' and vnpc = 'A'
740600100514     c*                  z-add     exaas         ncraasrec
740700100514     c*                  z-add     exlnp         ncrlnprec
740800100514     c*                  z-add     exnrs         ncrnrsrec
740900100514     c*                  z-add     exnsp         ncrnsprec
741000100514     c*                  z-add     exlna         nchlnarec
741100040429     c                   other
741200040429     c   86              z-add     §lpaas        ncraasrec
741300040316     c   86              z-add     §lplnp        ncrlnprec
741400040316     c   86              z-add     §lpnsr        ncrnrsrec
741500040316     c   86              z-add     §lpnsp        ncrnsprec
741600040316     c   86              z-add     §lplna        nchlnarec
741700040429     c                   endsl
741800040429     c                   endif
741900040315      * se impostata la spedizione di recupero la richiesta si considera
742000040315      * completa
742100040315     c                   if        ncrnsprec > 0
742200040315     c                   move      '1'           nchricompl
742300040315     c                   else
742400040315     c                   move      '0'           nchricompl
742500040315     c                   endif
742600040315
742700040315     c                   move      ar6cei        nchcei
742800040312     c                   z-add     wsvaas        nchaasfgl
742900040312     c                   z-add     wsvlnp        nchlnpfgl
743000040312     c                   z-add     wsvnrs        nchnrsfgl
743100040312     c                   z-add     wsvnsp        nchnspfgl
743200040312     c                   z-add     wsvlna        nchlnafgl
743300040312     c   90              z-add     gcpagc        nchagc
743400040312     c   90              z-add     gcpfgc        nchfgc
743500040312     c   90              z-add     gcpngc        nchngc
743600040315     c                   movel     worigine      nchorigine
743700040312     c                   write     fnncr000
743800940623     C*
743900941011     C                   SETOFF                                           86
744000940309     C*
744100940309     C                   ENDSR
744200940309     C/SPACE 3
744300940309     C*---------------------------------------------------------------*
744400940309     C*   P G M R E C                                                 *
744500940309     C*---------------------------------------------------------------*
744600940309     C     RCHREC        BEGSR
744700940309     C*
744800970715     C* Stampo richiesta di recupero solo se non dirotto alla 101
744900970715     C     VLNA          IFNE      101
745000970715     C     VFRI          ORNE      'D'
745100970715     C*
745200940309     C                   MOVEL     *ALL'?'       DLNP
745300940309     C     ARBLNP        CHAIN     AZORG01L                           21
745400940309     C  N21              MOVEL     ORGDES        DLNP             15
745500940309     C   21              MOVEL     *ALL'?'       DLNP
745600940309     C     ARBLNA        CHAIN     AZORG01L                           21
745700940309     C  N21              MOVEL     ORGDES        DLNA             15
745800940309     C   21              MOVEL     *ALL'?'       DLNA
745900940309     C     §LPLNA        CHAIN     AZORG01L                           21
746000940309     C  N21              MOVEL     ORGDES        DNWLNA           15
746100940309     C   21              MOVEL     *ALL'?'       DNWLNA
746200941006     C                   MOVE      AR6DFT        G08INV
746300941005     C                   MOVE      *ZEROS        G08DAT
746400941005     C                   MOVEL     '3'           G08ERR
746500941005     C                   CALL      'XSRDA8'
746600941005     C                   PARM                    WLBDA8
746700941005     C                   Z-ADD     G08DAT        GMADFT
746800040429      * stampo solo se non richiedo la nota di credito
746900040429     c                   if        not *in32 and not *in37
747000940309     C                   EXCEPT    RECTES
747100940309     C   84              EXCEPT    DIRTES
747200960126     C   85              EXCEPT    RICTES
747300940309     C                   EXCEPT    RECDET
747400940309     C   84              EXCEPT    DIRDET
747500960126     C   85              EXCEPT    RICDET
747600941221     C   89              EXCEPT    RECPRO
747700040429     c                   endif
747800940309     C* SE DIROTTO IN FRANCO E NO CAMBIO PORTO NON HO STORNATO BOLLA
747900940309     C  N32
748000940309     CANN37VFRI          IFEQ      'D'
748100940309     C     EXPORT        ANDEQ     'F'
748200940309     C     VNPC          ANDEQ     ' '
748300940309     C                   SETON                                        69
748400940309     C                   END
748500040429      * stampo solo se non richiedo la nota di credito
748600040429     c                   if        not *in32 and not *in37
748700940309     C                   EXCEPT    RECFIN
748800040429     c                   endif
748900040429
749000940309     C                   SETOFF                                       69
749100970715     C*
749200970715     C                   END
749300970715     C*
749400940309     C                   ENDSR
749500951213     C*---------------------------------------------------------------*
749600951213     C*   R E C T R A  CREO BOLLA RECUPERO SPESE X CAMBIO PORTO A -> F*
749700951213     C*---------------------------------------------------------------*
749800951213     C     RECTRA        BEGSR
749900951213     C*
750000951214     C                   SETON                                        86
750100990930     C                   CLEAR                   DBLP
750200031121     C                   CLEAR                   vteld
750300031121     C                   CLEAR                   vref
750400951213     C                   MOVEL     'E '          §LPCBO
750500160908      **
750600160908      *** Bolla di recupero colpa del cliente
750700020617     c                   if        viddcli = 'C' and *in92
750800020617     c                   move      'F*'          §lpcbo
750900020617     c                   end
751000951213     C                   MOVEL     '4'           §LPCTM
751100160908      **
751200951213      *  Inverto mittente e destinatario
751300951213     C                   MOVEL     ARBRSD        §LPRSM
751400951213     C                   MOVEL     ARBIND        §LPINM
751500951213     C                   MOVEL     ARBCAD        §LPCAM
751600951213     C                   MOVEL     ARBLOD        §LPLOM
751700951213     C                   MOVEL     ARBPRD        §LPPRM
751800951213     C                   MOVEL     ARBNZD        §LPNZM
751900951213     C                   MOVEL     ARBRSM        §LPRSD
752000951213     C                   MOVEL     ARBINM        §LPIND
752100951213     C                   MOVEL     ARBCAM        §LPCAD
752200951213     C                   MOVEL     ARBLOM        §LPLOD
752300951213     C                   MOVEL     ARBPRM        §LPPRD
752400951213     C                   MOVEL     ARBNZM        §LPNZD
752500960205     C     WSVLPO        IFNE      ARBLNA
752600951228     C                   MOVEL     WSVLPO        §LPLNA
752700960205     C                   ELSE
752800960205     C                   MOVEL     ARBLNP        §LPLNA
752900960205     C                   END
753000020225     C     §LPLNA        CHAIN     AZORG01L                           21
753100020225     C     *IN21         IFEQ      '0'
753200020225     C                   MOVE      ORGDE3        OG143
753300020225     C     §OGNTW        IFEQ      'FED'
753400020225     C                   MOVE      'S'           I95FI1
753500020225     C                   END
753600020225     C                   END
753700951213      *  Calcolo zona consegna
753800970710     C                   CLEAR                   TISI95
753900970710     C                   MOVEL     '3'           I95TCN
754000970710     C                   MOVEL     *BLANK        I95TLA
754100970710     C                   MOVEL     VNZD          I95NAR
754200970710     C                   MOVEL     VIND          I95IND
754300970710     C                   MOVEL     VCAD          I95CAP
754400970710     C                   MOVEL     VLOD          I95LOC
754500970710     C                   MOVEL     VPRD          I95PRV
754600970710     C                   MOVEL     ARBPKF        I95LKG
754700970710     C                   MOVEL     ARBVLF        I95LMC
754800970710     C                   MOVEL     ARBFFD        I95FFD
754900970710     C                   MOVEL     ARBTSP        I95TSP
755000031021     C                   MOVEL     ARBtc1        I95fi2
755100031021     C                   MOVE      ARBtc2        I95fi2
755200970710     C                   CALL      'TISI95R'
755300970710     C                   PARM                    TISI95
755400970710     C                   MOVEL     O95ZNC        §LPZNC
755500051005      * BOLLA RECUPERO
755600051005     C                   MOVEL     'S'           WRECSP
755700951214      *  Imposto note generiche
755800951214     C                   EXSR      XLPNOT
755900951214      *  Imposto note bolla recupero
756000960206     C                   MOVEL     *BLANKS       §LPNT2
756100960206     C     ARBCCM        IFNE      0
756200951214     C                   MOVEL     CNOT1         §LPNT2
756300951214     C                   MOVE      CNOT2         §LPNT2
756400951227     C                   MOVE      ARBCCM        §LPNT2
756500960206     C                   END
756600160908      **
756700951213      *  Richiamo routine impostazione parametri call pgm imm.bolle
756800951213     C                   EXSR      PGNBLP
756900951213     C*
757000951213     C                   ENDSR
757100940308     C*---------------------------------------------------------------*
757200940308     C*   L E G B O L  LEGO LA NUOVA BOLLA CREATA                     *
757300940308     C*---------------------------------------------------------------*
757400940308     C     LEGBOL        BEGSR
757500940308     C     §LPNSP        IFGT      0
757600941005      * GUARDO SE ESISTE LEGAME PER REPERIRE I RIFERIM. DELLA BOLLA
757700941005      * ORIGINALE ,ALTRIMENTI LA BOLLA ORIGINALE è LA SPEDIZ. TRATTATA
757800941005     C     KBOL          CHAIN     FNLBL01L                           21
757900941005     C     *IN21         IFEQ      '1'
758000941216     C                   MOVE      KLNP          LBLLPO
758100941216     C                   MOVE      KAAS          LBLAAO
758200941216     C                   MOVE      EXMGS         LBLMGO
758300941216     C                   MOVE      KNRS          LBLNRO
758400941216     C                   MOVE      KNSP          LBLNSO
758500941216     C                   MOVE      EXLNA         LBLLAO
758600941005     C                   ENDIF
758700951228     C                   MOVE      LBLLPO        WSVLPO
758800951228     C                   MOVE      §LPLNP        LBLLPN
758900940308     C                   MOVE      §LPAAS        LBLAAN
759000940308     C                   MOVE      §LPMGS        LBLMGN
759100940308     C                   MOVE      §LPNRS        LBLNRN
759200940308     C                   MOVE      §LPNSP        LBLNSN
759300940308     C                   MOVE      §LPLNA        LBLLAN
759400941216     C                   MOVE      KLNP          LBLLPP
759500941216     C                   MOVE      KAAS          LBLAAP
759600941216     C                   MOVE      EXMGS         LBLMGP
759700941216     C                   MOVE      KNRS          LBLNRP
759800941216     C                   MOVE      KNSP          LBLNSP
759900941216     C                   MOVE      EXLNA         LBLLAP
760000941006     C                   MOVE      *BLANK        LBLFT1
760100941006     C                   MOVE      *ZEROS        LBLDT1
760200941006     C                   MOVE      *BLANK        LBLFT2
760300941006     C                   MOVE      *ZEROS        LBLDT2
760400950331     C                   CLEAR                   DS3A
760500950331     C                   Z-ADD     1             §F
760600950331     C     EXCBO         LOOKUP    CDB(§F)                                04
760700950331     C   04              MOVEL     D3A(§F)       DS3A
760800950331     C     §3ABSD        IFEQ      'N'
760900950525     C                   Z-ADD     99999999      LBLDT2
761000950331     C                   END
761100941005     C                   WRITE     FNLBL000
761200041125     c* aggiornamento titas con flag legame bolla
761300041125     c                   call      'FNLG25R'
761400041125     C                   PARM                    recordlbl
761500041125     C                   PARM      ' '           I25tla            1
761600041125     c
761700940308     C                   END
761800940308     C                   ENDSR
761900940308     C*---------------------------------------------------------------*
762000941005     C*   A R R B O L  CREO SUBITO BOLLA IN ARRIVO                    *
762100940308     C*---------------------------------------------------------------*
762200940308     C     ARRBOL        BEGSR
762300941005      * RICH. PGM X SCRITTURA BOLLA IN ARRIVO
762400941005     C                   CLEAR                   DSUL41
762500941216     C                   MOVEL     §LPAAS        U41AAS
762600941216     C                   MOVEL     §LPLNP        U41LNP
762700941216     C                   MOVEL     §LPNRS        U41NRS
762800941216     C                   MOVEL     §LPNSP        U41NSP
762900941005     C                   CALL      'TRUL41R'
763000941005     C                   PARM                    DSUL41
763100070614      * duplica eventuali note ldv su bolla figlia
763200070614     c                   if        §oggeot = 'S'
763300070614     c                   exsr      dupnotldv
763400070614     c                   endif
763500940308     C*
763600941005     C* MI RITORNA IL TIPO BOLLA:RICHIAMO PGM TASSAZIONE
763700941227     C     U41TPB        IFEQ      'A'
763800941006     C     VSIFAT        ANDEQ     'S'
763900030715     C                   MOVEL     'FAT999'      MODFAT            9
764000030715     C                   MOVE      FILarr        MODFAT            9
764100030715     c                   movel(p)  modfat        com10            10
764200030715     C                   Z-ADD     80            LUNCMD           15 5
764300030715     C* OVRPRTF  FAT999 old
764400030715     C                   MOVEA     D82PRB        CMb(31)
764500030715     C                   MOVEA     com10         CMb(64)
764600991206     C                   CALL      'QCMDEXC'
764700030715     C                   PARM                    CMb
764800991206     C                   PARM                    LUNCMD
764900030715     C* OVRPRTF  FAT999 a4
765000030715     C                   MOVEA     D82PRB4       CM4(31)
765100030715     C                   MOVEA     com10         CM4(64)
765200030715     C                   CALL      'QCMDEXC'
765300030715     C                   PARM                    CM4
765400030715     C                   PARM                    LUNCMD
765500030715     C* OVRPRTF  FAT999 a5
765600030715     C                   MOVEA     D82PRB5       CM5(31)
765700030715     C                   MOVEA     com10         CM5(64)
765800030715     C                   CALL      'QCMDEXC'
765900030715     C                   PARM                    CM5
766000030715     C                   PARM                    LUNCMD
766100991206     C* OVRPRTF  FAT999 DPD
766200030715     C                   MOVEA     D82PRB        CMp(31)
766300940308     C                   CALL      'QCMDEXC'
766400030715     C                   PARM                    CMp
766500940308     C                   PARM                    LUNCMD
766600030304     C* OVRPRTF  FAT999 FedEx
766700030307     c                   Movel     *Blanks       comman           90
766800030307     C                   Z-ADD     90            LUNCMD           15 5
766900030715     C                   MOVEA     D82PRB        CMpf(31)
767000030715     c                   Movea     Cmpf          Comman
767100030307     c                   Eval      %Subst(Comman:77:9) = 'COPIES(2)'
767200030304     C                   CALL      'QCMDEXC'
767300030307     c                   PARM                    Comman
767400030304     C                   PARM                    LUNCMD
767500940308     C*
767600050511     C                   CLEAR                   fnlra2ds
767700050511     C                   MOVEL     FILARR        ILRa2FGS
767800050511     C                   MOVE      §LPAAS        ILRa2AAS
767900050511     C                   MOVE      §LPLNP        ILRa2LNP
768000050511     C                   MOVE      §LPNRS        ILRa2NRS
768100050511     C                   MOVE      §LPNSP        ILRa2NSP
768200050511     C                   MOVEL     'TB'          ILRa2CVB
768300050228     C******             MOVEL     'T'           D41GES
768400050511     C                   MOVEL     fnlra2ds      KPJBU
768500050228     C*
768600050511     C                   CALL      'FNLRA2R'
768700050228     C                   PARM                    KPJBA
768800050228     C                   PARM                    trul90ds
768900050511     C                   MOVEL     KPJBU         fnlrA2DS
769000940308     C* DLTOVR
769100941013     C                   Z-ADD     80            LUNCMD           15 5
769200940308     C                   CALL      'QCMDEXC'
769300940308     C                   PARM                    DOV
769400940308     C                   PARM                    LUNCMD
769500940311     C                   END
769600991028      * AGGIORNO LA BOLLA IN PARTENZA COME TRASMESSA
769700991028     C     KBLP          CHAIN     FNBLP01L                           38
769800991028     C     *IN38         IFEQ      '0'
769900991028     C                   MOVEL     'T'           BLPFT1
770000991028     C                   Z-ADD     AMGUDT        BLPDT1
770100991028     C                   EXCEPT    AGGTRA
770200991028     C                   END
770300940311      * STAMPA BOLLA  SE NON HO FATTO  FATTURA
770400940311     C     VSIFAT        IFEQ      'S'
770500941228     C     U41TPB        ANDEQ     'A'
770600050511     C     olrA2DFT      ANDLE     *ZEROS
770700940613     C     VSIFAT        OREQ      'N'
770800941227     C     U41TPB        ANDEQ     'A'
770900941227     C     U41TPB        OREQ      'F'
771000000313     C* Stampo la bolla in arrivo se non DPD e no Euroexpress
771100000313     C     NLAFL1        IFNE      'E'
771200020617     C     §OGntw        ANDNE     'DPD'
771300000313     C* oppure Euroexpress e ma si ddt
771400000313     C     NLAFL1        OREQ      'E'
771500000313     C     §LPBAM        ANDNE     'N'
771600000313     C     §LPBAM        ANDNE     'Y'
771700000313     C     §LPBAM        ANDNE     'K'
771800130315     c                   reset                   FNLSB6ds1
771900030624     C                   CLEAR                   FNLSB5DS
772000941215     C                   MOVEL     ' '           DB0TLA
772100941215     C                   MOVEL     ' '           DB0GES
772200941215     C                   MOVEL     'A'           DB0TBO
772300941227     C                   MOVE      §LPAAS        DB0AAS
772400941227     C                   MOVE      §LPLNP        DB0LNP
772500941227     C                   MOVE      §LPNRS        DB0NRS
772600941227     C                   MOVE      §LPNSP        DB0NSP
772700991208      *
772800030715     C                   CALL      d90psl
772900030624     C                   PARM                    FNLSB5DS
773000130315     c                   parm                    FNLSB6ds1
773100000313     C                   END
773200940308     C                   END
773300940308     C                   ENDSR
773400940308     C*---------------------------------------------------------------*
773500940308     C*   S T A B L P  STAMPA BOLLA LOCALE SE NON CREATA IN ARRIVO    *
773600940308     C*---------------------------------------------------------------*
773700940308     C     STABLP        BEGSR
773800000313     C* per lna Euroexpress no ddt o DPD non devo ristampare
773900000313     C* la bolla
774000000313     C     EXLNA         CHAIN     AZORG01L                           30
774100000313     C                   MOVEL     ORGDE3        OG143
774200000313     C* No Dpd / no euroexoress oppure
774300020617     C     §OGntw        IFNE      'DPD'
774400000313     C     NLAFL1        ANDNE     'E'
774500000313     C* Euroexpress si DDT
774600000313     C     NLAFL1        OREQ      'E'
774700000313     C     §LPBAM        ANDNE     'N'
774800000313     C     §LPBAM        ANDNE     'Y'
774900000313     C     §LPBAM        ANDNE     'K'
775000130315     c                   reset                   FNLSB6ds1
775100030624     C                   CLEAR                   FNLSB5DS
775200941215     C                   MOVEL     ' '           DB0TLA
775300941215     C                   MOVEL     ' '           DB0GES
775400941215     C                   MOVEL     'P'           DB0TBO
775500941227     C                   MOVE      §LPAAS        DB0AAS
775600941227     C                   MOVE      §LPLNP        DB0LNP
775700941227     C                   MOVE      §LPNRS        DB0NRS
775800941227     C                   MOVE      §LPNSP        DB0NSP
775900941215     C*
776000030715     C                   CALL      d90psl
776100030624     C                   PARM                    FNLSB5DS
776200130315     c                   parm                    FNLSB6ds1
776300000313     C                   END
776400000313     C*
776500940308     C                   ENDSR
776600900504     C/SPACE 3
776700900504     C*---------------------------------------------------------------*
776800900504     C*   G C P W T R                                                 *
776900900504     C*---------------------------------------------------------------*
777000900504     C     GCPWTR        BEGSR
777100050316     C     KGIA          CHAIN(e)  tigcp000                           40
777200050316     c                   if        %error
777300070521     C     KGIA          CHAIN(n)  tigcp000
777400050316     c                   exsr      geslck
777500050318     C     KGIA          CHAIN(e)  tigcp000                           40
777600050316     c                   endif
777601171003      * imposta dati ds before per monitor variazioni
777602171003     c                   eval      tigcpb = tigcp
777700941229     C* SE FORZATE  DISPOSIZIONI DA CONS. A DIROTT E VICEVERSA
777800941006     C     GCPDIS        IFNE      VFRI
777900941006     C                   MOVEL     VFRI          GCPDIS
778000941229     C                   MOVEL     VNPC          GCPNPC
778100941229     C                   MOVEL     VASG          GCPASG
778200941229     C                   MOVEL     VPSG          GCPPSG
778300940308     C                   END
778400941229     C* DATA CHIUSURA GIACENZA
778500050405     C                   MOVEL     VCFG          GCPCFG
778600050405     c                   if        spearr = 1
778700050405     C                   Z-ADD     VSGR          GCPSGR
778800050405     C                   Z-ADD     VGGS          GCPGGS
778900050405     C                   Z-ADD     VSGF          GCPSGF
779000050405     C                   Z-ADD     VTSGS         GCPSGS
779100050405     C                   Z-ADD     VSGR          GCPSGR
779200050405     C                   Z-ADD     VSGD          GCPSGD
779300911118     C                   Z-ADD     §TFT          GCPTFT
779400900423     C                   Z-ADD     AMGDSD        GCPDSD
779500900423     C                   Z-ADD     AMGDSA        GCPDSA
779600050405     c                   end
779700050405     C                   Z-ADD     AMGUDT        GCPDCG
779800050222     C                   Z-ADD     §LPLNP        GCPLPWa
779900050222     C                   Z-ADD     §LPAAS        GCPASWa
780000050222     C                   Z-ADD     §LPNRS        GCPNSWa
780100050222     C                   Z-ADD     §LPNSP        GCPNBWa
780200050222     C                   MOVE      AMGUDT        GCPDeda
780300050408     C* imposto prima fase =35
780400050408     C                   Z-ADD     035           GCPFAS
780500050414     C                   Z-ADD     0             GCPduma
780600050408     c* però se è un reso poichè in partenza non si devono rec. spese fase =50
780700050408     C     VCFG          IFEQ      '002'
780800050408     C                   Z-ADD     050           GCPFAS
780900050408     C                   END
781000050408     c*      se è dirottamento e la partenza ha già recuperato  fase =50
781100050408     C     VCFG          ifEQ      '006'
781200050408     C     VLNA          ANDNE     GCPLNA
781300050408     C     gcpnbw        ifne      0
781400050408     C                   Z-ADD     050           GCPFAS
781500050408     c                   else
781600050408     c*     se è dirottamento e la partenza non ha recuperato  fase =40
781700050408     C                   Z-ADD     040           GCPFAS
781800050408     C                   END
781900050408     C                   END
782000050222     C  N40              UPDATE    tigcp000
782001171003     c                   exsr      RegVariazioni
782100050411     c* scrivo il FIGIC00F chiusura giacenza
782200050411     c                   if        not *in40 and (gcpfas = 040 or
782300050411     c                             gcpfas = 050 )
782400050411     c                   exsr      gicwtr
782500050411     c                   end
782600050316     c                   if        gescomm = 'S'
782700050316     c                   commit
782800050316     c                   endif
782900051013     c* richiamo il pgm di variazione bolla se disposizioni automatiche, se
783000051019     c* data consegna richiesta o f.d. e disposizioni = consegna
783100051013     c                   movel     gcpflr        dgcpflr
783200051013     c                   if        d§gcpdisa = 'S' and flgdis <> ' '
783300051019     c                             and gcpdis = 'C'
783400051013     c                   exsr      srvarbol
783500051013     c                   end
783600941229     C* CONTROLLO SE C'ERANO ALTRE SPEDIZIONI FIGLIE
783700941229     C* GENERATE IN PRECEDENZA
783800950301WW   C                   SETOFF                                       98
783900941229     C     KBOL          CHAIN     FNLBL02L                           21
784000941229     C     *IN21         DOWEQ     *OFF
784100950102     C* ESCLUDO LA BOLLA APPENA CREATA
784200950102     C     LBLNSN        IFNE      §LPNSP
784300950414     C     LBLLAN        ANDEQ     EXLNA
784400950421      * RICH. NOTE ACCREDITO SU PRECEDENTI FIGLIE
784500950307      *************
784600950414     C                   EXSR      RCHNCF
784700950307      *************
784800950102     C                   ENDIF
784900941229     C     KBOL          READE     FNLBL02L                               21
785000941229     C                   ENDDO
785100950301WW   C                   SETOFF                                       98
785200900418     C                   ENDSR
785201171003     C*__________________________________________________________
785202171003     c     RegVariazioni begsr
785203171003     C*__________________________________________________________
785204171003     c                   eval      ILG33CTA = 'M'
785205171003     c                   eval      ILG33PRU = knmus
785206171003     c                   eval      ILG33NOJ = knmeb
785207171003     c                   eval      ILG33PGM = 'FNLG20R'
785208171003     c                   eval      tigcpa = tigcp
785209171003     c                   call      'FNLG33R'
785210171003     c                   parm                    fnlg33ds
785211171003     c                   parm                    tigcpb
785212171003     c                   parm                    tigcpa
785213171003     c                   endsr
785300051013     C*---------------------------------------------------------------*
785400051013     C     srvarbol      BEGSR
785500051013     C*---------------------------------------------------------------*
785600051013     C                   clear                   DSARBD
785700051013     C                   clear                   DSARBK
785800051013     C                   clear                   DARBT
785900051013     C                   clear                   DSARBG
786000051013     C                   clear                   DSlr48
786100051013     c* disposizioni automatiche per f.d.
786200051013     C                   IF        flgdis = 'F'
786300051017     C                   MOVEL     'E'           D48FFR
786400060314     c                   movel     *blanks       vteld
786500051017     C                   EXSR      PGARBI
786600051013     c                   end
786700051013     c* disposizioni automatiche per data consegna richiesta
786800051013     C                   IF        flgdis = 'D'
786900051013     C                   MOVEL     ' '           D48TLA
787000051013     C                   MOVEL     FILARR        D48FGS
787100051013     C                   MOVE      ARBAAS        D48AAS
787200051013     C                   MOVE      ARBLNP        D48LNP
787300051013     C                   MOVE      ARBNRS        D48NRS
787400051013     C                   MOVE      ARBNSP        D48NSP
787500051013     C                   MOVEL     ARBCBO        D48CBO
787600051109     c                   movel     arbGC1        §BGGC1
787700051109     c                   movel     arbGC2        §BGGC2
787800051109     c                   movel     arbTC1        §BGTC1
787900051109     c                   movel     arbTC2        §BGTC2
788000051013     C                   MOVEL     'CD'          D48CVB
788100051013     C                   MOVEL     'G'           D48TRC
788200051013     C                   MOVEL     'E'           D48FFR
788300051013     C                   MOVEL     ' '           D48ERR
788400051013     C                   MOVEL     *BLANK        D48MSG
788500051013     C                   MOVEL     'A'           D48TBO
788600051109      * se consegna particolare supermercato imposta causale CS
788700051109     c                   if        arbtc1 = 'S' or arbtc2 = 'S'
788800051109     c                   Clear                   Ds7l
788900051109     c                   Movel     '7L'          tblcod
789000051109     c                   Movel(p)  'CS'          tblKey
789100051109     c     kTab          Chain     Tabel00f                           96
789200051109     c                   If        Not *In96 and TblFlg <> ' '
789300051109     c                   Eval      *In96 = *On
789400051109     c                   EndIf
789500051109     c  n96              Movel     TblUni        Ds7l
789600051109     c                   If        Not *In96 and §7LSup <> 'N'
789700051109     c                   Movel     'CS'          D48Cvb
789800051109     c                   Movel     'V'           D48Ffr
789900051109     c                   Movel     'S'           D48F12
790000051109     c                   EndIf
790100051109     c                   endIf
790200051109      *fine supermercato
790300051013     c                   move      d915ndcr      dataiso
790400051013     c                   move      dataiso       §BGDCR
790500051014     c                   if        gnpdif = 915
790600051013     c                   clear                   §BGTCR
790700051013     c                   else
790800051013     c                   eval      §BGTCR='D'
790900051013     c                   end
791000051013     c*
791100051013     C                   MOVEL     DSLR48        KPJBU
791200051013     C                   CALL      'FNLR48R'
791300051013     C                   PARM                    KPJBA
791400051013     C                   PARM                    DSARBD
791500051013     C                   PARM                    DSARBK
791600051013     C                   PARM                    DARBT
791700051013     C                   PARM                    DSARBG
791800051013     C                   PARM                    trul90ds
791900051013     C                   MOVEL     KPJBU         DSLR48
792000051017     c                   end
792100051013     C                   ENDSR
792200050411     C*---------------------------------------------------------------*
792300050411     C* Scrivo figic00f per allineare saltuariamente la sede (TITAS+TNCSB)
792400050411     C*---------------------------------------------------------------*
792500050411     C     gicwtr        BEGSR
792600050411     C                   MOVEL     'CHGC'        GICCAV
792700050411     C                   Z-ADD     AMGUDT        GICDTV
792800050411     C                   TIME                    GICORV
792900050411     C                   Z-ADD     GCPLNPa       GICFEV
793000050411     C                   Z-ADD     GCPLNPa       GICLNP
793100050411     C                   Z-ADD     GCPLNAa       GICLNA
793200050411     C                   Z-ADD     GCPAASa       GICAAS
793300050411     C                   Z-ADD     GCPNRSa       GICNRS
793400050411     C                   Z-ADD     GCPNSPa       GICNSP
793500050411     C                   MOVEL     GCPAGC        GICDGC
793600050411     C                   MOVE      GCPMGC        GICDGC
793700050411     C                   Z-ADD     GCPFGC        GICFGC
793800050411     C                   Z-ADD     GCPNGC        GICNGC
793900050411     C                   Z-ADD     ARBDCM        GICDCH
794000050411     C                   WRITE     FIGIC000
794100050411     C                   ENDSR
794200950214     C*---------------------------------------------------------------*
794300950214     C*   R C H N C F - RICH. N.ACCR. SU FIGLIE PRECEDENTI            *
794400950214     C*---------------------------------------------------------------*
794500950214     C     RCHNCF        BEGSR
794600950214     C                   MOVE      LBLAAN        FAAS
794700950214     C                   MOVE      LBLLPN        FLNP
794800950214     C                   MOVE      LBLNRN        FNRS
794900950214     C                   MOVE      LBLNSN        FNSP
795000950414     C     KARBF         CHAIN     FNARB01L                           18
795100950330      * DEVE ESSERE BLOCCATA E NON DEVE ESSERE GIà STATA CHIESTA N.ACCR
795200950725B01  C     *IN18         IFEQ      *OFF
795300950330     C     ARBFBC        ANDNE     *BLANK
795400950330     C     ARBDCM        ANDEQ     *ZEROS
795500950330     C     ARBFIP        ANDNE     'C'
795600950330      * GUARDO SE è UN ASSEGNATO
795700950330     C                   MOVEL     '3A'          TBLCOD
795800950330     C                   MOVEL     *BLANK        TBLKEY
795900950330     C                   MOVEL     ARBCBO        TBLKEY
796000950330     C     KTAB          CHAIN     TABEL                              21
796100950330     C  N21              MOVEL     TBLUNI        DS3A
796200950330     C  N21              MOVEL     §3ADES        DTBL
796300950330     C  N21              MOVEL     §3ATB1        PORT              1
796400950725B02  C     PORT          IFEQ      'A'
796500950330     C*  Decodifico linea di arrivo
796600950330     C     ARBLNA        CHAIN     AZORG01L                           21
796700950330     C  N21              MOVEL     ORGDES        DLNA             15
796800950330     C   21              MOVEL     *ALL'?'       DLNA
796900950330      *----------------------------------------------------------*
797000000131     C                   CLEAR                   FVA
797100000131     C                   CLEAR                   FSV
797200950214     C                   CLEAR                   AR6NFT
797300991001     C                   MOVEL     '1'           KTRC
797400020618     C     KAR6F         CHAIN     FIAR601L                           18
797500950725B03  C     *IN18         IFEQ      *OFF
797600000131     C                   MOVEL     AR6SV1        FSV(1)
797700000131     C                   MOVE      AR6VA1        FVA(1)
797800000131     C                   MOVEL     AR6SV2        FSV(2)
797900000303     C                   MOVE      AR6VA2        FVA(2)
798000000131     C                   MOVEL     AR6SV3        FSV(3)
798100000131     C                   MOVE      AR6VA3        FVA(3)
798200000131     C     AR6SV3        IFNE      *BLANK
798300000131     C                   Z-ADD     3             X
798400040123     C     KAR6F         CHAIN     FIAR701L                           43
798500000131     C     *IN43         DOWEQ     *OFF
798600000131     C                   ADD       1             X
798700000131     C                   MOVEL     AR7SVN        FSV(X)
798800000131     C                   MOVE      AR7VAN        FVA(X)
798900040123     C     kar6f         READE     FIAR7000                               43
799000000131     C                   ENDDO
799100000131     C                   ENDIF
799200950330      *----------------------------------------------------------*
799300950330      * CONTROLLO SE BOLLA TASSATA
799400950330      *----------------------------------------------------------*
799500950725B04  C     AR6NFT        IFNE      *ZEROS
799600950214     C*
799700950214     C                   MOVEL     *ALL'?'       DLNP
799800950214     C     ARBLNP        CHAIN     AZORG01L                           21
799900950214     C  N21              MOVEL     ORGDES        DLNP             15
800000950214     C   21              MOVEL     *ALL'?'       DLNP
800100950214     C                   CLEAR                   DNWLNA
800200950214     C*
800300950214     C                   MOVE      AR6DFT        G08INV
800400950214     C                   MOVE      *ZEROS        G08DAT
800500950214     C                   MOVEL     '3'           G08ERR
800600950214     C                   CALL      'XSRDA8'
800700950214     C                   PARM                    WLBDA8
800800950214     C                   Z-ADD     G08DAT        GMADFT            8 0
800900950214     C* MANDARE IN SEDE SEMPRE
801000950214     C                   SETON                                        86
801100950214     C*   Stampa Nota Accredito
801200950214     C                   Z-ADD     ARBKSC        CLIENT            7 0
801300000131     C                   Z-ADD     0             TOTIMP
801400000131     C                   Z-ADD     0             TOTESE
801500000131     C                   Z-ADD     0             TOTNAC
801600991001     C                   Z-ADD     1             XS
801700000131     C     §$2IVA        LOOKUP    FSV(XS)                                20
801800991001     C     *IN20         IFEQ      '1'
801900020108     C     AR6IFT        SUB       AR6IMV        IMPALI
802000991001     C                   END
802100991027     C                   Z-ADD     AR6IMV        INAC
802200000131     C                   MOVE      INAC          WCOMO3            3 0
802300000131     C     WCOMO3        COMP      0                                      11
802400950214     C                   Z-ADD     AR6ALI        ALIQ              3 1
802500020621     C                   EVAL      CdIVA = %EDITC(Aliq:'2')
802600950214     C                   MOVEL     AR6CEI        TPALI             1
802700950214     C     TPALI         IFEQ      *BLANKS
802800000131     C                   Z-ADD     INAC          TOTIMP
802900950214     C                   ELSE
803000000131     C                   Z-ADD     INAC          TOTESE
803100950214     C                   Z-ADD     1             XX
803200950214     C     TPALI         LOOKUP    CEI(XX)                                41
803300950214     C     *IN41         IFEQ      '1'
803400020621     C                   MOVEL     ESE(XX)       CdIVA
803500950214     C                   END
803600950214     C                   MOVEL     'E'           TPALI
803700950214     C                   END
803800011130      * STAMPA DOPPIO IMPORTO SE DIVISA NON DI CONTABILITÀ
803900011130     C                   CLEAR                   WAR6IM
804000011130     C     AR6DIV        IFNE      §GEDMC
804100011130     C                   CLEAR                   YEURDS
804200011130     C                   MOVEL     AR6DIV        YECDVI
804300011130     C                   Z-ADD     INAC          YECIMI
804400011212     C                   MOVEL     §GEDMC        YECDVO
804500011130     C                   MOVEL     'H'           YECTAR
804600011130     C                   CALL      'YEURCO'
804700011130     C                   PARM                    YEURDS
804800011130     C                   Z-ADD     YECIMO        WAR6IM
804900011130     C                   ENDIF
805000011130      *
805100950214     C     TOTIMP        ADD       IMPALI        TOTNAC
805200950214     C                   ADD       TOTESE        TOTNAC
805300000131     C* VERIFICO SE IMPORTI CON DECIMALI
805400000131     C                   MOVE      TOTIMP        WCOMO3            3 0
805500000131     C     WCOMO3        COMP      0                                      08
805600000131     C                   MOVE      IMPALI        WCOMO3            3 0
805700000131     C     WCOMO3        COMP      0                                      09
805800000131     C                   MOVE      TOTESE        WCOMO3            3 0
805900000131     C     WCOMO3        COMP      0                                      10
806000000131     C                   MOVE      TOTNAC        WCOMO3            3 0
806100000131     C     WCOMO3        COMP      0                                      11
806200950214      * FORZO DATI DELLA FIGLIA
806300950214     C                   MOVEL     ARBAAS        G08INV
806400950214     C                   MOVE      ARBMGS        G08INV
806500950214     C                   MOVE      *ZEROS        G08DAT
806600950214     C                   MOVEL     '3'           G08ERR
806700950214     C                   CALL      'XSRDA8'
806800950214     C                   PARM                    WLBDA8
806900950214     C                   Z-ADD     G08DAT        VDSP
807000950214     C                   MOVE      ARBAAS        EXAAS
807100950214     C                   MOVE      ARBLNP        EXLNP
807200950214     C                   MOVE      ARBNRS        EXNRS
807300950214     C                   MOVE      ARBNSP        EXNSP
807400950214     C                   MOVE      ARBLNA        EXLNA
807500950330     C*  Aggiorno bolla x emissione Nota Accredito
807600950330     C                   MOVEL     'S'           ARBFNC
807700950330     C                   MOVEL     'C'           ARBFIP
807800040315      * scrittura file NCR
807900040315     c                   clear                   fnncr000
808000040315     c                   move      '201'         nctsocieta
808100040315     c                   z-add     arblna        nctfil
808200040315     c                   z-add     arblnp        nctpocompl
808300040315     c                   move      amgudt        nctddc
808400040315     c                   if        ar6dft > 0
808500040315     c                   move      ar6dft        nctdrp
808600040315     c                   end
808700040315     c                   z-add     ar6nft        nctnrp
808800040315     c                   z-add     ar6fiv        nctfpn
808900040315     c                   z-add     ar6ift        nctimr
809000040315     c                   move      *zeros        nctksc
809100040315     c                   move      ar6ksc        nctksc
809200040315     c                   move      *zeros        nctscf
809300040315     c                   move      ar6ksc        nctscf
809400040315     c                   move      knmus         nctutente
809500040315     c                   move      'I '          ncttnr
809600040315      * storno spese giacenza
809700040315     c                   move      'D'           ncrrna
809800040429     c                   move      '0'           ncrcmn
809900040315     c                   z-add     ar6imv        ncrim2
810000040315     c                   z-add     ar6ali        ncrali
810100040315     c                   z-add     arblnp        ncrflr
810200040315     c                   z-add     exaas         ncraassto
810300040315     c                   z-add     exlnp         ncrlnpsto
810400040315     c                   z-add     exnrs         ncrnrssto
810500040315     c                   z-add     exnsp         ncrnspsto
810600040429     c                   z-add     0             ncraasrec
810700040429     c                   z-add     0             ncrlnprec
810800040429     c                   z-add     0             ncrnrsrec
810900040429     c                   z-add     0             ncrnsprec
811000040429     c                   z-add     0             nchlnarec
811100040429      * completa viene forzata
811200040315     c                   move      '1'           nchricompl
811300040315
811400040315     c                   move      ar6cei        nchcei
811500040315     c                   z-add     wsvaas        nchaasfgl
811600040315     c                   z-add     wsvlnp        nchlnpfgl
811700040315     c                   z-add     wsvnrs        nchnrsfgl
811800040315     c                   z-add     wsvnsp        nchnspfgl
811900040315     c                   z-add     wsvlna        nchlnafgl
812000040315     c   90              z-add     gcpagc        nchagc
812100040315     c   90              z-add     gcpfgc        nchfgc
812200040315     c   90              z-add     gcpngc        nchngc
812300040315     c                   eval      nchorigine = 'STORNO SPESE DI GIACENZA '
812400040315     c                   write     fnncr000
812500950330     C*----------------------------------------------------------*
812600950330     C*  Segue Fattura: Stampo modulo x avvisare uff.contabilità
812700950330     C*                 della possibilità che venga richiesta
812800950330     C*                 Nota Accredito
812900950330     C*----------------------------------------------------------*
813000950725X04  C                   ELSE
813100950330     C*----------------------------------------------------------*
813200950330     C                   Z-ADD     GCPFGC        CFGC
813300950330     C                   Z-ADD     GCPNGC        CNGC
813400950330     C                   CLEAR                   WLBDA8
813500950330     C                   Z-ADD     GCPMGC        G08INV
813600950330     C                   MOVEL     GCPAGC        G08INV
813700950330     C                   MOVEL     '3'           G08ERR
813800950330     C                   CALL      'XSRDA8'
813900950330     C                   PARM                    WLBDA8
814000950330     C                   Z-ADD     G08DAT        CDGC
814100050222     C                   Z-ADD     GCPAASa       CAAS
814200050222     C                   Z-ADD     GCPLNPa       CLNP
814300050222     C                   Z-ADD     GCPNRSa       CNRS
814400050222     C                   Z-ADD     GCPNSPa       CNSP
814500950330     C                   Z-ADD     ARBAAS        CAAS1
814600950330     C                   Z-ADD     ARBLNP        CLNP1
814700950330     C                   Z-ADD     ARBNRS        CNRS1
814800950330     C                   Z-ADD     ARBNSP        CNSP1
814900950330     C                   Z-ADD     ARBMGS        G08INV
815000950330     C                   MOVEL     ARBAAS        G08INV
815100950330     C                   MOVEL     '3'           G08ERR
815200950330     C                   CALL      'XSRDA8'
815300950330     C                   PARM                    WLBDA8
815400950330     C                   Z-ADD     G08DAT        CDSP1             8 0
815500950330     C                   EXCEPT    AVVNAC
815600950330     C*  Calcolo Varie:
815700950330     C                   MOVEL     CVAR          DIMP             17
815800950330     C     AR6VA1        IFNE      0
815900950330     C                   MOVE      AR6SV1        DIMP
816000950330     C                   Z-ADD     AR6VA1        CIMP              9 0
816100950330     C                   EXCEPT    DETBOL
816200950330     C                   END
816300950330     C     AR6VA2        IFNE      0
816400950330     C                   MOVE      AR6SV2        DIMP
816500950330     C                   Z-ADD     AR6VA2        CIMP
816600950330     C                   EXCEPT    DETBOL
816700950330     C                   END
816800950330     C     AR6VA3        IFNE      0
816900950330     C                   MOVE      AR6SV3        DIMP
817000950330     C                   Z-ADD     AR6VA3        CIMP
817100950330     C                   EXCEPT    DETBOL
817200950330     C*  Se c'è la terza varia controllo se ce ne sono altre
817300991001     C     KARBF         CHAIN     FIAR701L                           18
817400950330     C     *IN18         DOWEQ     '0'
817500950330     C                   MOVE      AR7SVN        DIMP
817600950330     C                   Z-ADD     AR7VAN        CIMP
817700950330     C                   EXCEPT    DETBOL
817800991001     C     KARBF         READE     FIAR701L                               18
817900950330     C                   END
818000950330     C                   END
818100950330     C*  Trasporto
818200950330     C     AR6POR        IFNE      0
818300950330     C                   MOVEL     CTRA          DIMP
818400950330     C                   Z-ADD     AR6POR        CIMP
818500950330     C                   EXCEPT    DETBOL
818600950330     C                   END
818700950330     C*  Inoltro
818800991001     C                   Z-ADD     1             WW
818900991001     C     §$2FIN        LOOKUP    ASV(WW)                                20
819000991001     C     *IN20         IFEQ      '1'
819100950418     C                   MOVEL     CINL          DIMP
819200991001     C                   Z-ADD     AVA(WW)       CIMP
819300950330     C                   EXCEPT    DETBOL
819400950330     C                   END
819500950330     C*  Diritto fisso
819600991001     C                   Z-ADD     1             WW
819700991001     C     §$2VRH        LOOKUP    ASV(WW)                                20
819800991001     C     *IN20         IFEQ      '1'
819900950418     C                   MOVEL     CDIR          DIMP
820000991001     C                   Z-ADD     AVA(WW)       CIMP
820100950330     C                   EXCEPT    DETBOL
820200950330     C                   END
820300950330     C*  Tot.Imponibile
820400950330     C     AR6IMV        IFNE      0
820500950330     C                   Z-ADD     AR6IMV        CIMV
820600950330     C                   EXCEPT    TOTIMV
820700950330     C                   END
820800950330     C*  Imposto dati mittente
820900950330     C                   MOVEL     ARBRSD        CRAG
821000950330     C                   MOVEL     ARBIND        CIND
821100950330     C                   MOVEL     ARBCAD        CCAP
821200950330     C                   MOVEL     ARBLOD        CCIT
821300950330     C                   MOVEL     ARBPRD        CPRV
821400950330     C                   MOVEL     ARBNZD        CNAZ
821500950330     C                   EXCEPT    PIENAC
821600950725E04  C                   END
821700950330      *----------------------------------------------------------*
821800950725E03  C                   ENDIF
821900950725E02  C                   ENDIF
822000040312     C*
822100950330      *----------------------------------------------------------*
822200950214      *  DO PER CONSEGNATA LA BOLLA
822300950214     C                   MOVEL     'Y'           ARBICC
822400950725     C                   MOVEL     'S'           ARBACC
822500950214     C                   MOVEL     'Y'           ARBICA
822600950725     C                   MOVEL     'S'           ARBACA
822700950725     C                   MOVE      AMGUDT        ARBDCM
822800950214     C                   MOVEL     ORAMIN        ARBHMC
822900950214     C                   MOVE      *BLANK        ARBFT1
823000950214     C                   MOVE      *ZEROS        ARBDT1
823100091221     c                   clear                   ARBPDC
823200021009     C                   MOVEL     filarr        ARBPDC
823300091221     C                   MOVE      0998          ARBPDC
823400021009     C                   z-add     filarr        ARBifp
823500021009     C                   z-add     simpou        ARBipp
823600950214     C                   MOVE      *ALL'9'       ARBNDC
823700950214     C                   MOVE      AMGUDT        ARBDDC
823800950214     C                   MOVEL     'S'           ARBFDC
823900950214     C                   MOVE      *ALL'9'       ARBNMI
824000950214     C                   MOVE      AMGUDT        ARBDMI
824100950214     C                   MOVE      *ALL'9'       ARBCUC
824200950214     C                   MOVEL     *BLANK        ARBFBC
824300950214     C                   MOVEL     *BLANK        ARBCMC
824400950214      *
824500950214     C                   UPDATE    FNARB000
824600950214      *
824700950214     C                   ENDIF
824800950214     C*
824900950214     C                   SETOFF                                           86
825000950214     C*
825100950214     C                   ENDSR
825200900418      /EJECT
825300940309     C*----------------------------------------------------*
825400941219     C*    IMPOSTO CHIAVI PER TARIFFA                      *
825500940309     C*----------------------------------------------------*
825600941219     C     TARPCA        BEGSR
825700941219     C* §KSC VIENE IMPOSTATO (V.CHKARB) SE ASSEGNATO O SE C'E' 2°BOLLA
825800941219     C* KCTR    "      "         "            "              "
825900940309     C     §KSC          IFNE      *ZEROS
826000941006     C                   MOVE      §KSC          KKSC
826100940309     C                   ELSE
826200020225      *RICHIAMO PROGRAMMA PER REPERIMENTO INFORMAZIONI TARIFFA
826300020225     C                   CLEAR                   TR27DS
826400021118     C                   MOVE      ARBctr        I27ctb
826500020225     C                   MOVE      ARBTSP        I27TSP
826600020225     C                   MOVE      ARBLNA        I27LNA
826700020225     C                   MOVE      ARBLNP        I27LNP
826800020225     C                   MOVE      KKSC          I27CLI
826900160304     C**                 MOVEL     ARBAAS        I27DTA
827000160304     C**                 MOVE      ARBMGS        I27DTA
827100160304     C                   CALL      'TRUL27R1'
827200020225     C                   PARM                    TR27DS
827300160304     c
827400160304     c                   clear                   trulc7ds
827500160304     c                   eval      ic7tntw=o27ntw
827600160304     c
827700160304     C                   MOVEL     ARBAAS        Ic7DTA
827800160304     C                   MOVE      ARBMGS        Ic7DTA
827900160304     C                   move      arbctr        Ic7ctb
828000160304     c                   call      'TRULC7R'
828100160304     c                   parm                    trulc7ds
828200160304     C                   z-add     Oc7KSCc       kKSC
828300160304     C                   MOVEL     Oc7CTRc       KCTR
828400940309     C                   END
828500940309     C                   ENDSR
828600940309     C*----------------------------------------------------*
828700941219     C*    CALCOLO PROVVIGIONI C/ASSEGNO                   *
828800940309     C*----------------------------------------------------*
828900940309     C     CPROVV        BEGSR
829000980519     C*
829100990930     C                   CLEAR                   DTAS
829200990930     C                   CLEAR                   DTASV
829300980519     C                   MOVEL     KCTR          TASCTR
829400980519     C                   MOVE      KKSC          TASKSC
829500980519     C*
829600980519     C                   MOVEL     ARBCTS        TASCTS
829700980519     C                   MOVEL     ARBCAD        TASCAD
829800980519     C                   MOVEL     ARBFIN        TASFIN
829900980519     C                   MOVEL     ARBCAM        TASCAM
830000980519     C                   MOVEL     ARBFAP        TASFAP
830100980519     C                   MOVEL     ARBTSP        TASTSP
830200980519     C                   MOVEL     ARBTC1        TASTC1
830300980519     C                   MOVEL     ARBTC2        TASTC2
830400980519     C                   MOVEL     ARBFDN        TASFDN
830500980519     C                   MOVEL     ARBVAS        TASVAS
830600980519     C                   Z-ADD     ARBIAS        TASIAS
830700980519     C                   Z-ADD     ARBQFT        TASQFT
830800980519     C                   Z-ADD     ARBNCL        TASNCL
830900160530     C                   Z-ADD     ARBNCL        TASNCLb
831000980519     C                   Z-ADD     AMGDSP        TASDSP
831100980519     C                   Z-ADD     TASDSP        TASDCT
831200980519     C                   Z-ADD     ARBLNP        TASLNP
831300980519     C                   Z-ADD     ARBLNA        TASLNA
831400980519     C                   Z-ADD     ARBVLF        TASVLF
831500980519     C                   Z-ADD     ARBPKF        TASPKF
831600160530     C                   Z-ADD     ARBvlc        TASvlc
831700160530     C                   Z-ADD     ARBncr        TASncr
831800160530     C                   move      ARBfvf        TASfvf
831900160530     C                   Z-ADD     ARBpkb        TASPKb
832000980519     C* Reperisco codice tassazione
832100980519     C                   CLEAR                   TISI95
832200020225     C     TASLNP        CHAIN     AZORG01L                           21
832300020225     C     *IN21         IFEQ      '0'
832400020225     C                   MOVE      ORGDE3        OG143
832500020225     C     §OGNTW        IFEQ      'FED'
832600020225     C                   MOVE      'S'           I95FI1
832700020225     C                   END
832800020225     C                   END
832900980519     C                   MOVEL     '3'           I95TCN
833000980519     C                   MOVEL     ARBCAM        I95CAP
833100980519     C                   MOVEL     ARBNZM        I95NAR
833200980519     C                   MOVEL     ARBPRM        I95PRV
833300980519     C                   MOVEL     ARBLOM        I95LOC
833400980519     C                   MOVEL     ARBINM        I95IND
833500980519     C                   MOVEL     ARBTFP        I95TFP
833600980602     C                   MOVEL     ARBTSP        I95TSP
833700980602     C                   MOVEL     'S'           I95FRE
833800980519     C                   MOVEL     ARBNZM        I95NAR
833900980519     C                   MOVEL     AMGDSP        I95DAT
834000031021     C                   MOVEL     ARBtc1        I95fi2
834100031021     C                   MOVE      ARBtc2        I95fi2
834200980519     C                   CALL      'TISI95R'
834300980519     C                   PARM                    TISI95
834400980519     C                   MOVEL     O95CTS        TASMCT
834500980519     C*
834600040506     C                   MOVEL     ar9vca        TASVCA
834700980519     C                   Z-ADD     §ASS          TASCAS
834800980519     C                   MOVEL     AR9TIC        TASTIC
834900060927     C                   movel     ARBfbr        §asfbr
835000060927     C                   movel     ARBcca        §AScca
835100060927     C                   movel     dtas01        TASflo
835200980519     C*
835300980519     C                   MOVEL     'G'           TASSVA
835400021001     c* imposto peso vdl e colli rilevati
835500030206     c                   z-add     arbncp        Tasncp
835600030206     c                   z-add     arbpkc        Taspkc
835700030206      * Bancali
835800030206     c                   If        %Subst(Arbgva:1:1) = 'B'
835900030206     c                   Clear                   dAr5Ban
836000030805     C                   move      'BAN'         trcar5
836100051013     c     kFiar5        Chain(n)  Fiar501l
836200030206     c                   If        %Found(Fiar501l)
836300030206     c                   Movel     Ar5Uni        dAr5Ban
836400030206     c                   EndIf
836500030206     c                   Eval      TasBan = §Ar5Nba + §ar5Nb2
836600030206     c                   EndIf
836700030805      * Bancali Bartolini
836800030805     c                   If        %Subst(Arbgva:1:1) = 'O'
836900030805     c                   Clear                   dAr5BNB
837000030805     c                   Clear                   tasban
837100030805     C                   move      'BNB'         trcar5
837200051013     c     kFiar5        Chain(n)  Fiar501l
837300030805     c                   If        %Found(Fiar501l)
837400030805     c                   Movel     Ar5Uni        dAr5BNB
837500030805     c                   EndIf
837600030805     c                   Eval      TasNCL = §Ar5Bcor
837700030805     c                   EndIf
837800980519     C*
837900980519     C                   CALL      'TNSF20R'
838000980519     C                   PARM                    KPJBA
838100990930     C                   PARM                    DTAS
838200990930     C                   PARM                    DTASV
838300980519     C*
838400980519     C     TASERR        IFEQ      *BLANKS
838500991028     C                   MOVEL     TASDIV        V6CRVA
838600980519     C                   Z-ADD     1             §F
838700980519     C     TASSVA        LOOKUP    NSV(§F)                                21
838800980519     C   21              Z-ADD     NVA(§F)       §RCA
838900980519     C                   ELSE
839000980519     C                   SETON                                        20
839100980519     C                   END
839200980519     C     ENDCPR        ENDSR
839300951227     C*----------------------------------------------------*
839400951227     C*    CALCOLO PROVVIGIONI in partenza                 *
839500951227     C*----------------------------------------------------*
839600951227     C     PPROVV        BEGSR
839700951227     C                   MOVEL     *ALL'8'       KKSC
839800980519    1C     VNCA          IFNE      *ZEROS
839900951227     C                   Z-ADD     VNCA          §CAS
840000951227     C                   ELSE
840100951227     C                   Z-ADD     VASS          §CAS
840200980519    1C                   END
840300951227     C                   MOVEL     'V'           WCMB              1
840400951227     C                   EXSR      CASLIR
840500980519     C* CALCOLO PROVVIGIONI CON RICHIAMO DEL PGM TNSF20R
840600980519     C*
840700990930     C                   CLEAR                   DTAS
840800990930     C                   CLEAR                   DTASV
840900980519     C                   MOVEL     §LPCTR        TASCTR
841000040506     C                   MOVE      §KSC          TASKSC
841100980519     C*
841200980519     C                   MOVEL     §LPCTS        TASCTS
841300980519     C                   MOVEL     §LPCAD        TASCAD
841400980519     C                   MOVEL     §LPCAM        TASCAM
841500980519     C                   MOVEL     §LPTSP        TASTSP
841600980519     C                   MOVEL     ARBTC1        TASTC1
841700980519     C                   MOVEL     ARBTC2        TASTC2
841800980519     C                   MOVEL     ARBFDN        TASFDN
841900980519     C                   MOVEL     §LPVAS        TASVAS
842000980519     C                   Z-ADD     §LPIAS        TASIAS
842100980519     C                   Z-ADD     ARBQFT        TASQFT
842200980519     C                   Z-ADD     ARBNCL        TASNCL
842300160530     C                   Z-ADD     ARBNCL        TASNCLb
842400980519     C                   Z-ADD     AMGUDT        TASDSP
842500980519     C                   Z-ADD     TASDSP        TASDCT
842600980519     C                   Z-ADD     §LPLNP        TASLNP
842700980519     C                   Z-ADD     VLNA          TASLNA
842800980519     C                   Z-ADD     ARBVLF        TASVLF
842900980519     C                   Z-ADD     ARBPKF        TASPKF
843000160530     C                   Z-ADD     ARBvlc        TASvlc
843100160530     C                   Z-ADD     ARBncr        TASncr
843200160530     C                   move      ARBfvf        TASfvf
843300160530     C                   Z-ADD     ARBpkb        TASPKb
843400980519     C                   CLEAR                   DS3A
843500980519     C                   Z-ADD     1             §F                2 0
843600980519     C     §LPCBO        LOOKUP    CDB(§F)                                21
843700980519     C   21              MOVEL     D3A(§F)       DS3A
843800980519     C   21              MOVEL     §3ATB1        TASTBL
843900980519     C* Reperisco codice tassazione
844000980519     C                   CLEAR                   TISI95
844100020225     C     TASLNP        CHAIN     AZORG01L                           21
844200020225     C     *IN21         IFEQ      '0'
844300020225     C                   MOVE      ORGDE3        OG143
844400020225     C     §OGNTW        IFEQ      'FED'
844500020225     C                   MOVE      'S'           I95FI1
844600020225     C                   END
844700020225     C                   END
844800980519     C                   MOVEL     '3'           I95TCN
844900980519     C                   MOVEL     §LPCAM        I95CAP
845000980519     C                   MOVEL     §LPNZM        I95NAR
845100980519     C                   MOVEL     §LPPRM        I95PRV
845200980519     C                   MOVEL     §LPLOM        I95LOC
845300980519     C                   MOVEL     §LPINM        I95IND
845400980519     C                   MOVEL     ARBTFP        I95TFP
845500980602     C                   MOVEL     ARBTSP        I95TSP
845600980602     C                   MOVEL     'S'           I95FRE
845700980519     C                   MOVEL     §LPNZM        I95NAR
845800980519     C                   MOVEL     AMGUDT        I95DAT
845900031021     C                   MOVEL     ARBtc1        I95fi2
846000031021     C                   MOVE      ARBtc2        I95fi2
846100980519     C                   CALL      'TISI95R'
846200980519     C                   PARM                    TISI95
846300980519     C                   MOVEL     O95CTS        TASMCT
846400980519     C*
846500040506     C                   MOVEL     ar9vca        TASVCA
846600980519     C                   Z-ADD     §ASS          TASCAS
846700980519     C                   MOVEL     VTIC          TASTIC
846800980519     C*
846900980519     C                   MOVE      §LPBAM        TASDDT
847000980519     C                   MOVEL     'G'           TASSVA
847100021001     c* imposto peso vdl e colli rilevati
847200030206     c                   z-add     arbncp        Tasncp
847300030206     c                   z-add     arbpkc        Taspkc
847400060927     C                   movel     ARBfbr        §asfbr
847500060927     C                   movel     ARBcca        §AScca
847600060927     C                   movel     dtas01        TASflo
847700030206      * Bancali
847800030206     c                   If        %Subst(Arbgva:1:1) = 'B'
847900030206     c                   Clear                   dAr5Ban
848000030805     C                   move      'BAN'         trcar5
848100051013     c     kFiar5        Chain(n)  Fiar501l
848200030206     c                   If        %Found(Fiar501l)
848300030206     c                   Movel     Ar5Uni        dAr5Ban
848400030206     c                   EndIf
848500030206     c                   Eval      TasBan = §Ar5Nba + §ar5Nb2
848600030206     c                   EndIf
848700030805      * Bancali Bartolini
848800030805     c                   If        %Subst(Arbgva:1:1) = 'O'
848900030805     c                   Clear                   dAr5BNB
849000030805     c                   Clear                   tasban
849100030805     C                   move      'BNB'         trcar5
849200051013     c     kFiar5        Chain(n)  Fiar501l
849300030805     c                   If        %Found(Fiar501l)
849400030805     c                   Movel     Ar5Uni        dAr5BNB
849500030805     c                   EndIf
849600030805     c                   Eval      TasNCL = §Ar5Bcor
849700030805     c                   EndIf
849800980519     C*
849900980519     C                   CALL      'TNSF20R'
850000980519     C                   PARM                    KPJBA
850100990930     C                   PARM                    DTAS
850200990930     C                   PARM                    DTASV
850300991028     C*
850400991028     C     TASERR        IFEQ      *BLANKS
850500991028     C                   MOVEL     TASDIV        V6CRVA
850600991028     C                   END
850700991028     C*
850800991028     C                   ENDSR
850900940309     C/SPACE 5
851000940309     C*---------------------------------------------------------------*
851100941010     C* N E W C B L   CERCO NUOVO COICE BOLLA
851200940309     C*---------------------------------------------------------------*
851300940309     C     NEWCBL        BEGSR
851400940309     C                   Z-ADD     1             §F                2 0
851500940309     C     NWFGB         LOOKUP    FGB(§F)                                21
851600940309     C   21              MOVEL     CDB(§F)       NWCBL
851700940309     C* SE COME C/S PROPONE COD.=£ IMPONGO COD.BOLLA = A
851800940309     C* IN QUANTO £ è SOLO X LE COMUNIC. GIACENZE
851900941010     C   21NWCBL         IFEQ      '£ '
852000011130     C     NWCBL         OREQ      '$ '
852100941010     C                   MOVEL     'A '          NWCBL
852200940309     C                   END
852300950307     C* SE LA VECCHIA BOLLA ERA =£ ANZICHè C/S=A,  REIMPOSTO £ :
852400950307      * POICHè LE BOLLE £ NON VENGNO TRASMESSE IN SEDE QUINDI
852500950307     C* RICEVEREI LA FIGLIA SENZA AVERE LA MAMMA
852600950331     C                   CLEAR                   DS3A
852700950331     C                   Z-ADD     1             §F
852800950331     C     ARBCBO        LOOKUP    CDB(§F)                                04
852900950331     C   04              MOVEL     D3A(§F)       DS3A
853000950331     C     §3ABSD        IFEQ      'N'
853100950331     C     §3ACFD        ANDNE     *BLANKS
853200950331     C                   MOVEL     §3ACFD        NWCBL
853300950331     C                   ENDIF
853400950307     C*
853500940309     C  N21              SETON                                        30
853600940309     C                   ENDSR
853700940309     C/SPACE 5
853800940309     C*---------------------------------------------------------------*
853900940309     C* C N T V A R   CONTROLLO SE POSSO FARE VARIAZIONE BOLLA
854000940309     C*---------------------------------------------------------------*
854100940309     C     CNTVAR        BEGSR
854200940309     C                   SETOFF                                       2627
854300940309     C                   Z-ADD     1             §F
854400940309     C* CERCO TIPO VARIAZIONE
854500940309     C     §CDVAR        LOOKUP    §TV(§F)                                21
854600940309     C  N21              SETON                                        30
854700940309     C* GUARDO SE DEVO CONTROLLARE LA DATA CONTABILIZZAIONE
854800940309     C   21§DF(§F)       IFEQ      ' '
854900940928      * TASSAZ. 1°BOLLA
855000940928     C     AR6NFT        IFNE      *ZEROS
855100941013     C     AR6DFT        ANDLE     §DTCON
855200991001     C     AR6TRC        ANDEQ     '1'
855300940309     C                   SETON                                        26
855400940309     C                   END
855500940928      * TASSAZ. 2°BOLLA
855600991001     C     AR6NFT        IFNE      *ZEROS
855700991001     C     AR6DFT        ANDLE     §DTCON
855800991001     C     AR6TRC        ANDEQ     '2'
855900940309     C                   SETON                                        27
856000940309     C                   END
856100940309     C                   END
856200940309     C*  NON POSSO  VARIARE CODICE BOLLA :
856300940309     C     §CDVAR        IFEQ      'K8'
856400950725     C*  SE ESISTE FIGLIA NON POSSO VARIARE IL CODICE BOLLA
856500940309     C* SE TASSATO DESTINO SEMPRE NOTA ACCREDITO
856600940309     C     TASDES        IFEQ      '1'
856700940309     C                   SETON                                        27
856800940309     C                   END
856900940309     C                   END
857000940309     C                   ENDSR
857100940310     C*----------------------------------------------------*
857200940310     C*  X L P N O T  - IMPOSTO §LPNOT DATI EX SP.O GIAC.  *
857300940310     C*----------------------------------------------------*
857400940310     C     XLPNOT        BEGSR
857500051005     C                   clear                   §LPNot
857600051005     C                   clear                   §LPNT2
857700940310     C     $GIAC         IFEQ      '1'
857800940310     C                   MOVEL     'GIAC.'       TESTO1
857900940310     C                   MOVE      GCPAGC        GCAAGC
858000940310     C                   MOVE      GCPFGC        GCAFGC
858100940310     C                   MOVE      GCPNGC        GCANGC
858200940310     C                   MOVEL     'LNP. '       TESTO2
858300940310     C                   MOVE      ARBLNP        VARIO
858400940310     C                   MOVEL     RIFGIA        §LPNOT
858500051005      * reperisco note vecchia bolla da inserire dopo i riferimenti
858600051005      * solo in caso di dirottamento o cambio di porto
858700051005     c                   if        vfri = 'D' and wrecsp <> 'S'
858800051005     c                             or export <> vnpc and vnpc <> ' '
858900051006     c                             and wrecsp <> 'S'
859000051005     c                   movel(p)  rifgia        §comodo          70
859100051005     c                   exsr      oldnot
859200051005     c                   endif
859300940310     C                   ELSE
859400150108     C                   clear                   RIFSPE
859500150107      *imposto i riferimenti EX-SPED solo se non è dirottamento
859600150107     c                   if        vfri <> 'D'
859700940310     C                   MOVEL     'EX SPED.'    TESTO3
859800941217     C                   MOVE      KAAS          RAAS
859900941217     C                   MOVE      KLNP          RLNP
860000941217     C                   MOVE      KNRS          RNRS
860100941217     C                   MOVE      KNSP          RNSP
860200940310     C                   MOVEL     RIFSPE        §LPNOT
860300150107     c                   endif
860400051005      * reperisco note vecchia bolla da inserire dopo i riferimenti
860500051005      * solo in caso di dirottamento o cambio di porto
860600160908     c*********          if        vfri = 'D' and wrecsp <> 'S'
860700160908     c                   if        vfri = 'D'
860800051005     c                             or export <> vnpc and vnpc <> ' '
860900051006     c                             and wrecsp <> 'S'
861000051005     c                   movel(p)  rifspe        §comodo
861100051005     c                   exsr      oldnot
861200051005     c                   endif
861300961009     C* obbligo reso per particolarità giac.
861400961009     C   94              MOVEL     VNOT1         §LPNT2
861500961129     C*  Se devo effettuare un reso di una spedizione in Franco
861600961129     C*  Frontiera e su di essa non ho la particolarità che mi
861700961129     C*  obbliga ad effettuare il reso imposto nelle note cosegna
861800961129     C*  originale era un Franco frontiera
861900961129     C  N94VFRI          IFEQ      'R'
862000961129     C     V4CRCS        ANDNE     'S'
862100961129     C     ARBFBR        ANDEQ     'F'
862200961129     C                   MOVEL     CNOT3         §LPNT2
862300961129     C                   MOVE      CNOT4         §LPNT2
862400940310     C                   END
862500961129     C*
862600961129     C                   END
862700940310     C                   ENDSR
862800051005     C*-----------------------------------------------------
862900051005     C* O l d N o t - reperimento note da vecchia bolla
863000051005     C*-----------------------------------------------------
863100051005     C     oldnot        BEGSR
863200051005
863300051005     C                   MOVE      '8'           §TRC
863400060213     C     KAR4          CHAIN(N)  FiAR401L
863500060213     c                   if        %found(fiar401l)
863600150108     c                   if        §comodo <> *blank
863700051012     C                   eval      §comodo = %trim(§comodo)+ '*' +
863800051005     c                             %trim(ar4not)
863900150108     c                   else
864000150108     C                   eval      §comodo = %trim(ar4not)
864100150108     c                   endif
864200150108     c                   endif
864300051005     C                   MOVE      '9'           §TRC
864400060213     C     KAR4          CHAIN(N)  FiAR401L
864500060213     c                   if        %found(fiar401l)
864600150108     c                   if        §comodo <> *blank
864700051012     C                   eval      §comodo = %trim(§comodo) + '*' +
864800051005     c                             %trim(ar4not)
864900150108     c                   else
865000150108     C                   eval      §comodo = %trim(ar4not)
865100150108     c                   endif
865200051005     c                   end
865300051005     C                   movel     §comodo       §lpNot
865400051005     C                   move      §comodo       §lpNt2
865500051005
865600051005     C                   ENDSR
865700940930     C*-----------------------------------------------------
865800940930     C* C T R D I V - CONTROLLO DIVISA
865900940930     C*-----------------------------------------------------
866000940930     C     CTRDIV        BEGSR
866100941010     C                   CLEAR                   DSCV
866200940930     C                   Z-ADD     1             X
866300940930     C     WVCA          LOOKUP    DIV(X)                                 21
866400941006     C   21              MOVEL     DCV(X)        DSCV
866500940930     C  N21              SETON                                        1967
866600940930     C  N21              MOVEL     ERR(37)       $MSG
866700941217     C  N19§CVFDC        IFNE      'S'
866800940930     C                   MOVE      W0133         W0033             3 3
866900940930     C     W0033         IFNE      0
867000940930     C                   MOVEL     ERR(38)       $MSG
867100940930     C                   SETON                                        1976
867200940930     C                   ENDIF
867300940930     C                   ENDIF
867400940930     C*  C'ERA DIVISA E VARIO importo C/Assegno
867500940930     C*          --------> SE NON IMMESSA NUOVA DIVISA ERRORE
867600940930     C  N19VVCAP         IFNE      *BLANK
867700940930     C     VVCS          ANDEQ     'S'
867800940930     C     VNCA          ANDNE     *ZEROS
867900940930     C     VVCA          ANDEQ     *BLANK
868000940930     C                   MOVEL     ERR(39)       $MSG
868100940930     C                   SETON                                        1967
868200940930     C                   ENDIF
868300941010      * SE  CAMBIO DIVISA OBBLIGO DIGITARE ANCHE IMPORTO
868400941010     C     VNCA          IFEQ      *ZEROS
868500941010     C     VVCA          ANDNE     VVCAP
868600941010     C                   SETON                                        7619
868700941010     C                   MOVEL     ERR(41)       $MSG
868800941010     C                   END
868900940930     C                   ENDSR
869000941216     C*----------------------------------------------------*
869100941216     C*  C H K V A R
869200941216     C*  -----------
869300941216     C*  CERCO LA SIGLA DELLA VARIA LA ELABOARARE
869400941216     C*  MI RITORNA L'INDICE SU CUI LAVORARE
869500941216     C*----------------------------------------------------*
869600941216     C     CHKVAR        BEGSR
869700941216     C                   Z-ADD     1             XS                2 0
869800941216     C     §SIGLA        LOOKUP    WSV(XS)                                21
869900941216     C     *IN21         IFEQ      *OFF
870000941216     C                   Z-ADD     1             XS
870100941216     C     ' '           LOOKUP    WSV(XS)                                21
870200941216     C                   END
870300941216     C                   ENDSR
870400071126     C*-----------------------------------------------------
870500071126     C* CAPOFILA    - reperimento CAPOFILA £6
870600071126     C*-----------------------------------------------------
870700071126     C     Capofila      begsr
870800071127     c                   clear                   FNLV55ds
870900071126     c                   eval      D55tpt =  '6'
871000071126     c                   eval      D55lin =  vlna
871100071126     c                   eval      D55drf =  amgudt
871200071126     c                   call      'FNLV55R'
871300071126     c                   parm                    FNLV55ds
871400071126      * - Reperimento 146ª e 148ª descrizioni dell'organigramma
871500071126      *   (flag Tellus del capofila e flag gestione telefonate GEO)
871600071126if  2c                   if        D55tfa <> vlna and d55err = *blank
871700071127     c     D55tfa        chain     AZORG01l
871800071126if  3c                   if        %found(AZORG01L)
871900071128     c                   movel     ORGde6        Og146
872000071128     c                   movel     ORGde8        Og148
872100071126     c                   move      'X'           Og_da_£6
872200071126e   3c                   endif
872300071126e   2c                   endif
872400071126
872500071126     c                   endsr
872600990203     C*----------------------------------------------------------*
872700940301     C/EJECT
872800940301     C*---------------------------------------------------------------*
872900030715     C*   R O U T I N E   INIZIALE  x trul90ds                        *
873000940301     C*---------------------------------------------------------------*
873100030715     C     srtrul90      BEGSR
873200030715     c                   clear                   trul90ds
873300030715     c                   eval      d90mde = d82moe
873400030715     c                   eval      d90mdb = d82mob
873500030715     c                   eval      d90prb = d82prb
873600030715     c                   eval      d90pre = d82pre
873700030715     c                   eval      d90pss = d82pss
873800030715     c                   eval      d90mdb4 = d82mdb4
873900030715     c                   eval      d90mdb5 = d82mdb5
874000030715     c                   eval      d90psl = d82psl
874100030716     c                   eval      d90bsl = d82bsl
874200030715     c                   eval      d90prb4 = d82prb4
874300030715     c                   eval      d90prb5 = d82prb5
874400030715     c                   endsr
874500080123     C*---------------------------------------------------------------*
874600080123     C*   Controllo §LPCTRA ctr di tassazione porti assegnati in arrivo
874700080123     c*   per Resi: verifico se codice valido anche in arrivo
874800080123     C*---------------------------------------------------------------*
874900080123     C     contrCTRA     Begsr
875000080123     C                   CLEAR                   TR27DS
875100080123     C                   MOVE      §lppkb        I27pkg
875200080123     C                   MOVE      §lptsp        I27TSP
875300080123     C                   MOVE      §lpLNA        I27LNA
875400080123     C                   MOVE      §lplnp        I27LNP
875500080123     C                   MOVE      §lpksca       I27CLI
875600160303     C                   CALL      'TRUL27R1'
875700080123     C                   PARM                    TR27DS
875800080123     c
875900080123     c                   clear                   fnlv59DS
876000080123     C                   MOVEL     'A'           ilv59tbo
876100080123     C                   MOVEL     §lpksca       ilv59KSC
876200080123     C                   MOVEL     KCI           ilv59KCI
876300080123     C                   MOVEL     o27fie        ilv59FIE
876400080123     C                   MOVEL     amgudt        ilv59DSP
876500080123     C                   MOVEL     §lptsp        ilv59TSP
876600080123     C                   MOVEL     'S'           ilv59RIC
876700080123     C                   MOVE      §lpctra       ilv59CTR
876800080123     C*
876900080123     C                   CALL      'FNLV59R'
877000080123     C                   PARM                    fnlv59ds
877100080123     c*
877200080124     c* Se restituisce con errore o non preenti tariffe
877300080124     c*                           cerco una tariffa valida in arrivo
877400080124     c                   if        olv59err<>' '  or olv59tks=' '
877500080123     c                   clear                   ilv59CTR
877600080123     C                   CALL      'FNLV59R'
877700080123     C                   PARM                    fnlv59ds
877800080124     c                   select
877900080124     c* Se non trovata nessuna tariffa --> azzero il codice
878000080124     c                   when      olv59tks=' '
878100080124     c                   clear                   §lpctra
878200080124     c                   clear                   §lpksca
878300080124     c                   when      olv59ct3=' '
878400080123     c                   movel     olv59ctr      §lpctra
878500080124     c                   other
878600080124     c                   clear                   §lpctra
878700080124     c                   endsl
878800080124     c
878900080123     c                   endif
879000080123     c
879100080123     c                   endsr
879200030715     C*---------------------------------------------------------------*
879300030715     C*   R O U T I N E   INIZIALE                                    *
879400030715     C*---------------------------------------------------------------*
879500030715     C     DEFCAM        BEGSR
879600940927      *
879700940928     C     D82GIA        IFEQ      'S'
879800940928      * MI VIENE PASSATO IL NR. GIACENZA
879900940928     C                   MOVE      D82AGC        §AGC
880000940928     C                   MOVE      D82FGC        §FGC
880100940928     C                   MOVE      D82NGC        §NGC
880200940928      *
880300940301     C                   SETON                                        90
880400940301     C                   MOVEL     '1'           $GIAC             1
880500050222     C                   OPEN      tigcp02L
880600050222     C                   OPEN      tignp01L
880700990730     C                   OPEN      TITGC01L
880800940301     C                   ELSE
880900940301     C                   MOVEL     '0'           $GIAC
881000940301     C                   END
881100940301      *
881200940301     C* FINTO OPEN X RENDERE SHR YES  I FILE
881300941221     C                   OPEN      FNLV22P
881400940301     C   LR
881500940301     CANNLR              EXCEPT    FINTO
881600940301     C* MI SALVO KPJBU
881700940301     C                   MOVEL     KPJBU         §KPJBU          256
881800940301     C                   Z-ADD     1             CODUT
881900940301     C                   CALL      'X§PARUT'
882000940301     C                   PARM                    UTEDSE
882100940301     C     CODUT         CABEQ     -1            FINE
882200940301     C                   MOVEL     RAGUT         RSUT             20
882300941011     C                   MOVEL     '* FNLG20'    VPROGR
882400940301     C                   MOVE      'R *'         VPROGR
882500940301     C*
882600940301     C                   MOVEL     REC80         CNCR80
882700941013      * TRSFORMO DATA CONTABILIZZ. DA 6 A 8
882800941013     C                   MOVEL     DTU(10)       W20               2 0
882900941013     C     W20           IFGT      50
883000941013     C     1900          ADD       W20           W40               4 0
883100941013     C                   ELSE
883200941013     C     2000          ADD       W20           W40
883300941013     C                   END
883400941013     C                   MOVE      DTU(10)       §DTCON            8 0
883500941013     C                   MOVEL     W40           §DTCON
883600020730      *
883700020730      * se è il primo livello
883800021009     C                   SETOFF                                       2101
883900020730     c                   if        simtpp = '1'
884000020730     C                   SETON                                        2101
884100020730     c                   end
884200940928      *
884300021009     C                   MOVE      simpou        FILARR
884400941215     C* SE PASSATO IMPOSTO FILIALE IN GESTIONE
884500941216     C     D82FGS        IFGT      '000'
884600941215     C                   MOVEL     D82FGS        FILARR
884700941215     C                   ENDIF
884800940301     C                   SETOFF                                       02
884900940301     C* OVRPRTF  N. ACCREDITO
885000940301     C                   MOVEL     'ACCR.REC'    MODNAC            9
885100941013     C                   Z-ADD     80            LUNCMD           15 5
885200950123     C                   MOVEA     MODNAC        CMN(63)
885300940301     C                   CALL      'QCMDEXC'
885400940301     C                   PARM                    CMN
885500940301     C                   PARM                    LUNCMD
885600940301     C                   OPEN      LET132
885700940928      *
885800940624     C* AGGANCIO LA EI
885900940624     C                   MOVEL     'EI'          TBLCOD
886000020621     C                   CLEAR                   ESE
886100940624     C                   MOVEA     *BLANKS       CEI
886200940624     C     KTABP         CHAIN     TABEL00F                           21
886300040123     C                   Z-ADD     1             XX
886400940624     C     *IN21         DOWEQ     '0'
886500940627     C     XX            ANDLE     50
886600940624     C                   MOVEL     TBLUNI        DSEI
886700020621     C                   MOVE      §EIJEI        ESE(XX)
886800940624     C                   MOVEL     TBLKEY        CEI(XX)
886900940624     C                   ADD       1             XX
887000940624     C     KTABP         READE     TABEL00F                               21
887100940624     C                   END
887200940301     C*
887300991001     C* CARICO DATI DELLE VARIE CON FLG SOGGETTE O NO TOTALE IMP.
887400991001     C                   MOVEL     'CC'          TBLCOD
887500020806     C                   CLEAR                   IPB
887600020806     C                   MOVEA     *BLANKS       CCC
887700991001     C     KTABP         CHAIN     TABEL00F                           21
887800040123     C                   Z-ADD     1             XX                3 0
887900991001     C     *IN21         DOWEQ     '0'
888000991001     C     XX            ANDLE     100
888100991001     C* Controllo se sto esaminando un rcd di una varia
888200991001     C                   MOVEL     TBLKEY        WCOMO5            5
888300991001     C     WCOMO5        IFEQ      'VARIE'
888400991001     C                   MOVEL     TBLUNI        DSCC
888500991001     C                   MOVE      §CCFL6        IPB(XX)
888600991001     C                   MOVE      TBLKEY        CCC(XX)
888700991001     C                   ADD       1             XX
888800991001     C                   END
888900991001     C     KTABP         READE     TABEL00F                               21
889000991001     C                   END
889100940928     C* AGGANCIO LA £6 E RECUPERO LE LINEE A LORO ASSOCIATE
889200021009     c                   eval      kpjbus = kpjbu
889300020509     C                   CLEAR                   TR06DS
889400020509     C                   MOVEL     '£6'          D06COD
889500020509     C                   MOVEL     filarr        D06KEY
889600020509     C                   MOVEL     TR06DS        KPJBU
889700020509     C                   CALL      'TRUL06R'
889800020509     C                   PARM                    KPJBA
889900020509     C                   MOVEL     KPJBU         TR06DS
890000020509     C                   MOVEA     LIN           l£6
890100021009     c                   eval      kpjbu = kpjbus
890200940301     C*
890300940928     C* CERCO IL CODICE DELL'OPERATORE
890400021114     C                   CLEAR                   Tibs36Ds
890500021114     c                   EVAL      I36ute = knmus
890600021114     c                   EVAL      I36Tla = 'L'
890700021114     C                   CALL      'TIBS36R'
890800021114     C                   PARM                    Tibs36Ds
890900021009     C*                                                              i3
891000940301     C*--- RICERCA CAPOCONTI------------------------------------------*
891100940301     C                   DO        50            X                 2 0
891200940301     C                   MOVE      TCU(X)        TCUDS
891300940301     C     F56           CABNE     'CG'          END1
891400940301     C     F4            COMP      '1'                                    21
891500940301     C** 1 CLIENTI   21
891600940301     C     F3            COMP      '0'                                242425
891700940301     C** 0 ITALIA   25
891800940301     C** 1 ESTERO   24
891900940301     C   21
892000940301     CAN 24              Z-ADD     KCU(X)        KCE               4 0
892100940301     C   21
892200940301     CAN 25              Z-ADD     KCU(X)        KCI               4 0
892300940301     C     END1          TAG
892400940301     C                   END
892500940301     C*---------------------------------------------------------------*
892600940301     C*  KLIST
892700940301     C     KGIA          KLIST
892800940301     C                   KFLD                    §AGC
892900940301     C                   KFLD                    §FGC
893000940301     C                   KFLD                    §NGC
893100940301     C                   KFLD                    §FRG
893200940301     C                   MOVE      0             §FRG
893300940301     C*
893400940301     C     KNOT          KLIST
893500940301     C                   KFLD                    §AGC
893600940301     C                   KFLD                    §FGC
893700940301     C                   KFLD                    §NGC
893800940301     C                   KFLD                    §FRG
893900940301     C                   KFLD                    §FAS
894000051013     C*
894100051013     C     Kar5          KLIST
894200051013     C                   KFLD                    ARBAAS
894300051013     C                   KFLD                    ARBLNP
894400051013     C                   KFLD                    ARBNRS
894500051013     C                   KFLD                    ARBNSP
894600051013     C                   KFLD                    ar5trd
894700940301     C*
894800051013     C                   MOVE      0             §FRG
894900940301     C                   MOVE      020           §FAS              3 0
895000940301     C*
895100940301     C     KGIP          KLIST
895200940301     C                   KFLD                    §AGC
895300940301     C                   KFLD                    §FGC
895400940301     C                   KFLD                    §NGC
895500940301     C*
895600940301     C     KBOL          KLIST
895700941216     C                   KFLD                    KAAS
895800941216     C                   KFLD                    KLNP
895900941216     C                   KFLD                    KNRS
896000941216     C                   KFLD                    KNSP
896100991001     C     KAR6          KLIST
896200991001     C                   KFLD                    KAAS
896300991001     C                   KFLD                    KLNP
896400991001     C                   KFLD                    KNRS
896500991001     C                   KFLD                    KNSP
896600991028     C                   KFLD                    KTRC
896700991028     C     KBLP          KLIST
896800991028     C                   KFLD                    §LPAAS
896900991028     C                   KFLD                    §LPLNP
897000991028     C                   KFLD                    §LPNRS
897100991028     C                   KFLD                    §LPNSP
897200940301     C*
897300940301     C     KBOLG         KLIST
897400941216     C                   KFLD                    KAAS
897500941216     C                   KFLD                    KLNP
897600941216     C                   KFLD                    KNRS
897700941216     C                   KFLD                    KNSP
897800990628     C                   KFLD                    PROGR             2 0
897900941229     C*
898000941229     C     KBOLF         KLIST
898100941229     C                   KFLD                    ULFAAS
898200941229     C                   KFLD                    ULFLNP
898300941229     C                   KFLD                    ULFNRS
898400941229     C                   KFLD                    ULFNSP
898500991001     C     KAR6F         KLIST
898600000121     C                   KFLD                    FAAS
898700000121     C                   KFLD                    FLNP
898800000121     C                   KFLD                    FNRS
898900000121     C                   KFLD                    FNSP
899000991001     C                   KFLD                    KTRC
899100940301     C*
899200941003     C     KAR4          KLIST
899300941216     C                   KFLD                    KAAS
899400941216     C                   KFLD                    KLNP
899500941216     C                   KFLD                    KNRS
899600941216     C                   KFLD                    KNSP
899700940301     C                   KFLD                    §TRC
899800061026     C*
899900110802     C     Kar4dup       KLIST
900000110802     C                   KFLD                    arbAAS
900100110802     C                   KFLD                    arbLNP
900200110802     C                   KFLD                    arbNRS
900300110802     C                   KFLD                    arbNSP
900400110802     C*
900500070320     C     kar4l         klist
900600070320     C                   kfld                    arbAAS
900700070320     C                   kfld                    arbLNP
900800070320     C                   kfld                    arbNRS
900900070320     C                   kfld                    arbNSP
901000070320     C                   kfld                    §TRCl             1
901100070320     C                   move      'L'           §TRCl
901200070320     C*
901300061026     C     KAR4lbl       KLIST
901400061026     C                   KFLD                    LBLAAOsv
901500061026     C                   KFLD                    LBLLPOsv
901600061026     C                   KFLD                    LBLNROsv
901700061026     C                   KFLD                    LBLNSOsv
901800061026     C                   KFLD                    §TRC
901900030123     C*
902000030123     C     Kfiar5        KLIST
902100030123     C                   KFLD                    KAAS
902200030123     C                   KFLD                    KLNP
902300030123     C                   KFLD                    KNRS
902400030123     C                   KFLD                    KNSP
902500030123     C                   KFLD                    trcar5            3
902600950214     C*
902700950214     C     KARBF         KLIST
902800950214     C                   KFLD                    FAAS
902900950214     C                   KFLD                    FLNP
903000950214     C                   KFLD                    FNRS
903100950214     C                   KFLD                    FNSP
903200940301     C*
903300940301     C     KTAB          KLIST
903400940301     C                   KFLD                    CODUT
903500940301     C                   KFLD                    TBLCOD
903600940301     C                   KFLD                    TBLKEY
903700991028     C*
903800991028     C     KTBE          KLIST
903900991028     C                   KFLD                    TBECOD
904000991028     C                   KFLD                    TBEKE1
904100991028     C                   KFLD                    TBEKE2
904200940301     C*
904300940301     C     KTABP         KLIST
904400940301     C                   KFLD                    CODUT
904500940301     C                   KFLD                    TBLCOD
904600940301     C*
904700000925     C     KTAM          KLIST
904800150512     C                   KFLD                    ilv59ksc
904900000925     C                   KFLD                    CTRD51            3 0
905000150512     C                   KFLD                    olv59prg
905100940930     C*
905200940930     C     KTGC          KLIST
905300940930     C                   KFLD                    KKSC
905400940930     C                   KFLD                    KCTR
905500940930     C                   KFLD                    KPRG
905600940301     C*
905700940301     C     KANM          KLIST
905800940301     C                   KFLD                    ARTFLS
905900940301     C                   KFLD                    ARTLNA
906000940301     C                   KFLD                    ARTNRS
906100941006     C                   KFLD                    ARTNSC
906200951206     C*
906300951206     C     KEVB1         KLIST
906400951206     C                   KFLD                    KAAS1
906500951206     C                   KFLD                    KLNP1
906600951206     C                   KFLD                    KNRS1
906700951206     C                   KFLD                    KNSP1
906800951206     C                   KFLD                    KDEV
906900951206     C                   KFLD                    KHEV
907000951206     C*
907100951206     C     KEVB2         KLIST
907200951206     C                   KFLD                    KAAS1
907300951206     C                   KFLD                    KLNP1
907400951206     C                   KFLD                    KNRS1
907500951206     C                   KFLD                    KNSP1
907600950918     C*
907700950918     C     KANM1         KLIST
907800990518     C                   KFLD                    KAAS
907900950918     C                   KFLD                    KLNP
908000950918     C                   KFLD                    KNRS
908100950918     C                   KFLD                    KNSP
908200950918     C                   KFLD                    KCAA              3 0
908300010522      *
908400010522     C     KFNSPE        KLIST
908500010522     C                   KFLD                    WFLS              1
908600010524     C                   KFLD                    WCODIC            7 0
908700010522     C                   KFLD                    WLUO              3
908800010522     C                   MOVE      'L'           WFLS
908900010522     C                   MOVE      '300'         WLUO
909000041221     C     Ksp2          KLIST
909100041221     C                   KFLD                    WCODIC
909200041221     C                   KFLD                    WLUO
909300041221     C                   KFLD                    Wtip              2
909400070614     c     karn          klist
909500070614     c                   kfld                    arbaas
909600070614     c                   kfld                    arblnp
909700070614     c                   kfld                    arbnrs
909800070614     c                   kfld                    arbnsp
909900070614     c                   kfld                    cdnk              3
910000070614     c                   kfld                    gstk              1
910100110909     C*
910200110909     C     Kpct          KLIST
910300110909     C                   KFLD                    KAAS
910400110909     C                   KFLD                    KLNP
910500110909     C                   KFLD                    KNRS
910600110909     C                   KFLD                    KNSP
910700110909     C                   KFLD                    pcttrd
910800140916
910900140916     C     Ksgn5         KLIST
911000140916     C                   KFLD                    §LPAASm
911100140916     C                   KFLD                    §LPLNPm
911200140916     C                   KFLD                    §LPNRSm
911300140916     C                   KFLD                    §LPNSPm
911400000925      *
911500940301     C                   MOVEL     '0'           $FINE             1
911600940301     C                   MOVEL     '0'           $ERR              1
911700940303     C                   MOVEL     '0'           $F01              1
911800940303     C                   MOVEL     '0'           $F02              1
911900940303     C                   MOVEL     '0'           $F03              1
912000940304     C                   MOVEL     '0'           $F11              1
912100940304     C                   MOVEL     '0'           $OKF01            1
912200940304     C                   MOVEL     '0'           $OKF02            1
912300940304     C                   MOVEL     '0'           $OKF03            1
912400940301     C                   MOVEL     'V.BOLLA'     TESNWB
912500940301     C                   MOVEL     'L.A.'        TE2NWB
912600940308     C                   MOVEL     *ZEROS        §QUATR            4
912700960125     C     *LIKE         DEFINE    ARBRSD        WSVRSD
912800960125     C     *LIKE         DEFINE    AR4RD2        WSVRD2
912900960125     C     *LIKE         DEFINE    ARBIND        WSVIND
913000960125     C     *LIKE         DEFINE    ARBCAD        WSVCAD
913100960125     C     *LIKE         DEFINE    ARBPRD        WSVPRD
913200960125     C     *LIKE         DEFINE    ARBLOD        WSVLOD
913300960125     C     *LIKE         DEFINE    ARBNZD        WSVNZD
913400941216     C     *LIKE         DEFINE    GCPAAS        KAAS
913500941216     C     *LIKE         DEFINE    ARBMGS        EXMGS
913600941216     C     *LIKE         DEFINE    ARBLNA        EXLNA
913700950102     C     *LIKE         DEFINE    ARBAAS        EXAAS
913800950102     C     *LIKE         DEFINE    ARBLNP        EXLNP
913900950102     C     *LIKE         DEFINE    ARBNRS        EXNRS
914000950102     C     *LIKE         DEFINE    ARBNSP        EXNSP
914100951214     C     *LIKE         DEFINE    ARBAAS        WSVAAS
914200951214     C     *LIKE         DEFINE    ARBLNP        WSVLNP
914300951214     C     *LIKE         DEFINE    ARBNRS        WSVNRS
914400951214     C     *LIKE         DEFINE    ARBNSP        WSVNSP
914500951214     C     *LIKE         DEFINE    ARBLNA        WSVLNA
914600951228     C     *LIKE         DEFINE    LBLLPO        WSVLPO
914700941216     C     *LIKE         DEFINE    GCPLNP        KLNP
914800941216     C     *LIKE         DEFINE    GCPNRS        KNRS
914900941216     C     *LIKE         DEFINE    GCPNSP        KNSP
915000951206     C     *LIKE         DEFINE    EVBAAS        KAAS1
915100951206     C     *LIKE         DEFINE    EVBLNP        KLNP1
915200951206     C     *LIKE         DEFINE    EVBNRS        KNRS1
915300951206     C     *LIKE         DEFINE    EVBNSP        KNSP1
915400951206     C     *LIKE         DEFINE    EVBDEV        KDEV
915500951206     C     *LIKE         DEFINE    EVBHEV        KHEV
915600941011     C     *LIKE         DEFINE    AR4NOT        AR4RD2
915700940301     C     *LIKE         DEFINE    GCPAGC        §AGC
915800940301     C     *LIKE         DEFINE    GCPFGC        §FGC
915900940301     C     *LIKE         DEFINE    GCPNGC        §NGC
916000940301     C     *LIKE         DEFINE    GCPFRG        §FRG
916100940301     C     *LIKE         DEFINE    GCPTFT        §TFT
916200940308     C     *LIKE         DEFINE    ARBKSC        §KSC
916300941006     C     *LIKE         DEFINE    ARBKSC        KKSC
916400941006     C     *LIKE         DEFINE    ARBCTR        KCTR
916500980519     C     *LIKE         DEFINE    TGCPRG        KPRG
916600941010     C     *LIKE         DEFINE    GCPDDM        AMGDSP
916700940301     C     *LIKE         DEFINE    GCPDDM        AMGDDM
916800940301     C     *LIKE         DEFINE    GCPDDM        AMGDSD
916900940301     C     *LIKE         DEFINE    GCPDDM        AMGDSA
917000940301     C     *LIKE         DEFINE    GCPDDM        AMGDTG
917100941006     C     *LIKE         DEFINE    G08TGI        §GGDA
917200941006     C     *LIKE         DEFINE    G08TGI        §GGA
917300941006     C     *LIKE         DEFINE    G08TGI        §GG
917400990628     C     *LIKE         DEFINE    GCPSGR        §SGR
917500940301     C     *LIKE         DEFINE    GCPGGS        §GGPAG
917600940301     C     *LIKE         DEFINE    GCPGGS        §GGPAR
917700940301     C     *LIKE         DEFINE    GCPGGS        §GGSOS
917800940301     C     *LIKE         DEFINE    GCPGGS        §GGSAV
917900990628     C     *LIKE         DEFINE    GCPFRG        §NRG
918000940929     C     *LIKE         DEFINE    AR9CAS        §CAS
918100941006     C     *LIKE         DEFINE    AR9CMB        §CMB
918200941006     C                   MOVE      *ZEROS        §ASS             13 0
918300941214     C     *LIKE         DEFINE    TASVA1        §VARR
918400940301     C     *LIKE         DEFINE    VRCA          EXRCA
918500940301     C     *LIKE         DEFINE    VRCA          §RCA
918600950522     C     *LIKE         DEFINE    VRCA          WRCA
918700941006     C     *LIKE         DEFINE    AR6IFT        §INAC1
918800941006     C     *LIKE         DEFINE    AR6IFT        §INAC2
918900940301     C     *LIKE         DEFINE    TBLCOD        §COD
919000940301     C     *LIKE         DEFINE    TBLKEY        §KEY
919100940301     C                   MOVEL     *BLANKS       §DES             60
919200941006     C     *LIKE         DEFINE    ARBCBO        NWCBL
919300941215     C     *LIKE         DEFINE    D48CVB        §CDVAR
919400941215     C     *LIKE         DEFINE    D48CVB        §§KCVB
919500941215     C     *LIKE         DEFINE    D48CVB        §§TCVB
919600941006     C     *LIKE         DEFINE    AR4TRC        §TRC
919700991001     C     *LIKE         DEFINE    AR6TRC        KTRC
919800940301     C     *LIKE         DEFINE    ARBLNA        §LNA
919900941006     C     *LIKE         DEFINE    ARBCAD        COMCAP
920000940310     C     *LIKE         DEFINE    ARBCAD        SAVCAD
920100940930     C     *LIKE         DEFINE    ARBNZD        SAVNZD
920200940310     C     *LIKE         DEFINE    ARBIND        SAVIND
920300940310     C     *LIKE         DEFINE    ARBLNA        SAVLNA
920400940310     C     *LIKE         DEFINE    ARBLOD        SAVLOD
920500940310     C     *LIKE         DEFINE    ARBPRD        SAVPRD
920600940310     C     *LIKE         DEFINE    ARBRSD        SAVRSD
920700941011     C     *LIKE         DEFINE    ARBRSD        SAVRD2
920800941216     C     *LIKE         DEFINE    AR6VA1        §ANTEP
920900941229     C     *LIKE         DEFINE    ARBLNP        ULFLNP
921000941229     C     *LIKE         DEFINE    ARBAAS        ULFAAS
921100941229     C     *LIKE         DEFINE    ARBNRS        ULFNRS
921200941229     C     *LIKE         DEFINE    ARBNSP        ULFNSP
921300950214     C     *LIKE         DEFINE    ARBLNP        FLNP
921400950214     C     *LIKE         DEFINE    ARBAAS        FAAS
921500950214     C     *LIKE         DEFINE    ARBNRS        FNRS
921600950330     C     *LIKE         DEFINE    ARBNSP        FNSP
921700950330     C     *LIKE         DEFINE    GCPFGC        CFGC
921800950330     C     *LIKE         DEFINE    GCPNGC        CNGC
921900950330     C     *LIKE         DEFINE    GCPDUR        CDGC
922000950330     C     *LIKE         DEFINE    GCPAAS        CAAS
922100950330     C     *LIKE         DEFINE    GCPLNP        CLNP
922200950330     C     *LIKE         DEFINE    GCPNRS        CNRS
922300950330     C     *LIKE         DEFINE    GCPNSP        CNSP
922400950330     C     *LIKE         DEFINE    ARBAAS        CAAS1
922500950330     C     *LIKE         DEFINE    ARBLNP        CLNP1
922600950330     C     *LIKE         DEFINE    ARBNRS        CNRS1
922700950330     C     *LIKE         DEFINE    ARBNSP        CNSP1
922800950330     C     *LIKE         DEFINE    AR6IMV        CIMV
922900950330     C     *LIKE         DEFINE    AR6IFT        CIFT
923000950330     C     *LIKE         DEFINE    ARBRSD        CRAG
923100950330     C     *LIKE         DEFINE    ARBIND        CIND
923200950330     C     *LIKE         DEFINE    ARBCAD        CCAP
923300950330     C     *LIKE         DEFINE    ARBLOD        CCIT
923400950330     C     *LIKE         DEFINE    ARBPRD        CPRV
923500950330     C     *LIKE         DEFINE    ARBNZD        CNAZ
923600970710     C     *LIKE         DEFINE    O13ERR        WERR
923700970710     C     *LIKE         DEFINE    O13MSG        WMSG
923800970710     C     *LIKE         DEFINE    I95CAP        WSVCAP
923900970710     C     *LIKE         DEFINE    I95LOC        WSVLOC
924000970710     C     *LIKE         DEFINE    VPRD          WSVPRO
924100970710     C     *LIKE         DEFINE    E13FZV        WFZV
924200970710     C     *LIKE         DEFINE    E13FZ1        WFZ1
924300970710     C     *LIKE         DEFINE    E13FZ2        WFZ2
924400970710     C     *LIKE         DEFINE    E13FZ3        WFZ3
924500031119     C     *LIKE         DEFINE    vteld         SAVteld
924600031119     C     *LIKE         DEFINE    vref          SAVref
924700940928      * UDATE
924800940928     C                   TIME                    WHHUDT           14 0
924900940928     C                   MOVE      WHHUDT        G08DAT
925000940928     C                   MOVE      *ZEROS        G08INV
925100940928     C                   MOVEL     *BLANK        G08ERR
925200940928     C                   CALL      'XSRDA8'
925300940928     C                   PARM                    WLBDA8
925400940928     C                   Z-ADD     G08INV        AMGUDT            8 0
925500940928     C                   Z-ADD     G08DAT        GMAUDT            8 0
925600940928     C                   MOVEL     AMGUDT        ANNO              4 0
925700940930     C                   MOVEL     WHHUDT        ORAMIN            4 0
925800940928     C*                                                    *
925900000925     C* >>>>>>>>>>>>>>>
926000991028     C* REPERISCO LA DIVISA NELLA MONETA CORRENTE
926100991028     C                   MOVEL     'GED'         TBECOD
926200991028     C                   MOVEL     *BLANKS       TBEKE1
926300991028     C                   MOVEL     *BLANKS       TBEKE2
926400991028     C                   MOVEL     '1'           TBEKE1
926500991028     C     KTBE          CHAIN     TNTBE01L                           21
926600991028     C  N21              MOVEL     TBEUNI        DGED
926700940301     C* CARICO I TIPI BOLLA
926800940301     C                   MOVEL     '3A'          TBLCOD
926900940301     C                   MOVEL     *BLANK        TBLKEY
927000940301     C     KTAB          SETLL     TABEL
927100940301     C     *IN21         DOUEQ     '1'
927200940301     C     KTABP         READE     TABEL                                  21
927300940301     C  N21TBLFLG        IFEQ      *BLANK
927400940301     C                   MOVEL     TBLUNI        DS3A
927500020617     C     §3afgc        IFeq      *blank
927600940301     C                   MOVEL     §3ATB1        NWTB1
927700940301     C                   MOVEL     §3ATB2        NWTB2
927800940301     C                   MOVEL     §3AFCA        NWFCA
927900940301     C                   MOVEL     §3AFAN        NWFAN
928000940301     C                   MOVEL     §3AFTD        NWFTD
928100940301     C                   ADD       1             §F
928200940301     C                   MOVEL     NWFGB         FGB(§F)
928300940301     C                   MOVEL     TBLKEY        CDB(§F)
928400941221     C                   MOVEL     TBLUNI        D3A(§F)
928500020417     C                   END
928600940301     C                   END
928700940301     C                   END
928800951206     C* CARICO I TIPI BOLLA
928900081219     C                   Z-ADD     0             XY
929000951206     C                   MOVEL     '2A'          TBLCOD
929100951206     C                   MOVEL     *BLANK        TBLKEY
929200951206     C     KTAB          SETLL     TABEL
929300951206     C     *IN21         DOUEQ     '1'
929400951206     C     KTABP         READE     TABEL                                  21
929500951206     C  N21TBLFLG        IFEQ      *BLANK
929600951206     C                   ADD       1             XY
929700951206     C                   MOVEL     TBLKEY        C2A(XY)
929800951206     C                   MOVEL     TBLUNI        D2A(XY)
929900951206     C                   END
930000951206     C                   END
930100940301     C*
930200940301     C* CARICO I TIPI VARIAZIONE BOLLA
930300940301     C                   MOVE      *ZERO         §F
930400940301     C                   MOVEL     '7L'          TBLCOD
930500940301     C                   MOVEL     *BLANK        TBLKEY
930600940301     C     KTAB          SETLL     TABEL
930700940301     C     *IN21         DOUEQ     '1'
930800940301     C     KTABP         READE     TABEL                                  21
930900940301     C  N21TBLFLG        IFEQ      *BLANK
931000940301     C                   MOVEL     TBLUNI        DS7L
931100940301     C                   ADD       1             §F
931200940301     C                   MOVEL     TBLKEY        §TV(§F)
931300991029     C                   MOVEL     §7LVTA        §DF(§F)
931400940301     C                   END
931500940301     C                   END
931600940929     C* CARICO I CODICI DIVISA E RELATIVI CAMBI
931700940929     C                   MOVE      *ZERO         §F
931800940929     C                   MOVEL     'CV'          TBLCOD
931900940929     C                   MOVEL     *BLANK        TBLKEY
932000940929     C     KTAB          SETLL     TABEL
932100940929     C     *IN21         DOUEQ     '1'
932200940929     C     KTABP         READE     TABEL                                  21
932300940929     C  N21TBLFLG        IFEQ      *BLANK
932400940929     C                   ADD       1             §F
932500940929     C                   MOVEL     TBLKEY        DIV(§F)
932600941006     C                   MOVEL     TBLUNI        DCV(§F)
932700940929     C                   END
932800940929     C                   END
932900941216     C* CARICO SIGLE DELLE VARIE
933000941216     C                   MOVEL     '$2'          TBLCOD
933100941216     C                   MOVEL     *BLANK        TBLKEY
933200941216     C                   MOVEL     '1'           TBLKEY
933300941216     C     KTAB          CHAIN     TABEL                              21
933400941216     C  N21TBLFLG        COMP      *BLANK                             2121
933500941216     C  N21              MOVEL     TBLUNI        DS$2
933600940301     C*
933700040719     C* CARICO seconda chiave per limitazioni giacenze
933800040719     C                   MOVEL     '2G'          TBLCOD
933900040719     C                   MOVEL     *BLANK        TBLKEY
934000040719     C                   MOVEL     '2'           TBLKEY
934100040719     C     KTAB          CHAIN     TABEL                              21
934200040719     C  N21TBLFLG        COMP      *BLANK                             2121
934300040719     C  N21              MOVEL     TBLUNI        DS2g2
934400940301     C                   ENDSR
934500050316     C*----------------------------------------------------*
934600050316     C     geslck        begsr
934700050316     C*----------------------------------------------------*
934800050316     c                   Eval      ul82§rrn = gcpnrr
934900050316     c                   Eval      UL82§FIL = 'TIGCP00F'
935000050316     c                   Eval      UL82§WIN = 'S'
935100050316     c                   Eval      UL82§F7  = 'S'
935200050316     c                   Eval      UL82§num = 3
935300050316     c                   Eval      UL82§att = 3
935400050404     c                   Eval      UL82§mss = 'Si sta bloccando la giacenza Nr.'
935500050404     c                             + %editc(§agc:'Z') +  '  '+ %editc(§fgc:'Z')
935600050404     c                             + '  ' +  %editc(§ngc:'Z') + ' Terminare il -
935700050404     c                             lavoro'
935800050404     c                   Eval      UL82§msw = 'La giacenza Nr. '
935900050404     c                             + %editc(§agc:'Z') +  '  '+ %editc(§fgc:'Z')
936000050404     c                             + '  ' +  %editc(§ngc:'Z') +
936100050404     c                             ' non è manutenzionabile'
936200050404
936300050316      * Effettuo la chiamata al *PGM d utilità
936400050316     c                   Call(e)   'TRUL82R'
936500050316     c                   Parm                    Trul82ds
936600050316     c                   if        ul82§sts = 'A'
936700050316     c                   goto      fine
936800050316     c                   endif
936900050316
937000050316     C                   ENDSR
937100141210     C*----------------------------------------------------*
937200141210     C     geslckArb     begsr
937300141210     C*----------------------------------------------------*
937400141210     c                   Eval      ul82§rrn = arbnrr
937500141210     c                   Eval      UL82§FIL = 'FNARB00F'
937600141210     c                   Eval      UL82§WIN = 'S'
937700141210     c                   Eval      UL82§F7  = 'S'
937800141210     c                   Eval      UL82§num = 3
937900141210     c                   Eval      UL82§att = 3
938000141210     c                   Eval      UL82§mss = 'Si sta bloccando la Bolla Nr.'
938100141210     c                             + %editc(kaas:'Z') +  '  '+ %editc(klnp:'Z')
938200141210     c                             +   '  '+ %editc(knrs:'Z')
938300141210     c                             + '  ' +  %editc(knsp:'Z') + ' Terminare il -
938400141210     c                             lavoro'
938500141210     c                   Eval      UL82§msw = 'La Bolla Nr. '
938600141210     c                             + %editc(kaas:'Z') +  '  '+ %editc(klnp:'Z')
938700141210     c                             +   '  '+ %editc(knrs:'Z')
938800141210     c                             + '  ' +  %editc(knsp:'Z') +
938900141210     c                             ' non è manutenzionabile'
939000141210
939100141210      * Effettuo la chiamata al *PGM d utilità
939200141210     c                   Call(e)   'TRUL82R'
939300141210     c                   Parm                    Trul82ds
939400141210     c                   if        ul82§sts = 'A'
939500141210     c                   seton                                        43
939600141210     c                   endif
939700141210
939800141210     C                   ENDSR
939900130920     c*-------------------------------------------------------------
940000130920      *  Se è prevista una LNA da forzare da ORGANIGRAMMA
940100130920     c*-------------------------------------------------------------
940200130920     C     CHG_LNA_xFORZAbegSR
940300130920      *
940400130920      *  Se la LNA prevede su organigramma di essere sostituita
940500130920      *   x fare un RESO.
940600130920      *
940700130920     c     §LPLNA        chain     azorg01L
940800130920      *
940900130920     c                   IF        %Found(azorg01L)
941000130920     c                   eval      og143 = orgde3
941100130920     C*
941200130920     C*** Se si deve RENDERE utilizzando un'altra LINEA prevista in ORGANIGRAMMA
941300130920     C***    FORZATAMENTE su un'altra LNA (tipico CASO della DPD)
941400130920     C***  es.:   ciò che arriva da 193 o 192 deve tornare x 190
941500130920     C***       oppure ciò che arriva da 292  deve tornare x 197
941600130920      *
941700130920     c                   if        §ogLNARES <> *blank
941800130920     c                               and §ogLNARES <> '000'
941900130920      *
942000130920     c                   move      §ogLNARES     §LPLNA
942100130920     c                   end
942200130920      *
942300130920     C                   END
942400130920     C*
942500130920     C                   ENDSR
942600140916     c*-------------------------------------------------------------
942700140916      *  Annullamento chi sono bolla mamma su FISGN
942800140916     c*-------------------------------------------------------------
942900140916     C     sr_fisgn      begSR
943000140916
943100140916    1c                   if        §lpctm='2H' or §lpctm='2K'
943200140917     c     ksgn5         setll     fisgn05l
943300140917     c     ksgn5         reade     fisgn05l
943400140917     c                   dow       not %eof(fisgn05l)
943500140916    3c                   if        sgntnp=simfel
943600140916     c                   eval      sgnatb='A'
943700140916     c                   eval      sgndatora = %char(%timestamp:*iso0)
943800140916     C                   CLEAR                   SGNT6A
943900140916     C                   CLEAR                   SGNT6B
944000140916     C                   CLEAR                   SGNT6C
944100140916     C                   CLEAR                   SGNT6D
944200140916     C                   CLEAR                   SGNT6E
944300140916     C                   CLEAR                   SGNT6F
944400140916     c                   update    fisgn000
944500140916    3c                   endif
944600140917     c     ksgn5         reade     fisgn05l
944700140917    2c                   enddo
944800140917     c                   unlock    fisgn05l
944900140916    1c                   endif
945000140916
945100140916     c                   endsr
945200170228     c*-------------------------------------------------------------
945300170228      *  Determina lnp dpd se tfp<>tfa
945400170228     c*-------------------------------------------------------------
945500170228     c     sr_lnpdpd     begsr
945600170228     c                   clear                   wlnpb
945700170228     c                   clear                   fnlv55ds
945800170228     c                   eval      D55lin =  §lplnp
945900170228     c                   eval      D55drf =  amgudt
946000170228     c                   call      'FNLV55R'
946100170228     c                   parm                    FNLV55ds
946200170228    1c                   if        d55err=*blanks and
946300170228     c                             d55tfp<>d55tfa
946400170323     c                   eval      wlnpb = §lplnp
946500170323     c*    d55tfa        chain     azorg01l
946600170323    2c*                  if        %found(azorg01l)
946700170323     c*                  movel     orgde3        og143
946800170323    3c*                  if        §oglnpb>*zeros
946900170323     c*                  move      §OGLNPB       wlnpb
947000170323   x3c*                  else
947100170228     c* se su azorg la lnp valida come lnp bolla è vuota prendo da azcae la
947200170228     c* prima filiale dpd del terminal
947300170323     c*                  eval      kpjbus = kpjbu
947400170323     C*                  CLEAR                   TR06ds
947500170323     C*                  MOVE      'PP'          D06COD
947600170323     C*                  MOVEL     d55tfa        D06KEY
947700170323     c*                  move      'S'           d06esc
947800170323     C*                  MOVEL     tr06ds        KPJBU
947900170323     c*                  call      'TRUL06R'
948000170323     C*                  PARM                    KPJBA
948100170323     c*                  movel     kpjbu         tr06ds
948200170323     c*                  eval      kpjbu = kpjbus
948300170228     c* loop su schiera delle filiali
948400170323     c*                  z-add     1             idx
948500170323     c*                  clear                   wlnpb
948600170323    4c*                  dow       idx<=30  and lin(idx)>0 and wlnpb=0
948700170323     c*    lin(idx)      chain     azorg01l
948800170323    5c*                  if        %found(azorg01l)
948900170323     c*                  movel     orgde3        og143
949000170323    6c*                  if        §ogntw='DPD'
949100170323     c*                  move      lin(idx)      wlnpb
949200170323    6c*                  endif
949300170323    5c*                  endif
949400170323     c*                  add       1             idx
949500170323    4c*                  enddo
949600170323    3c*                  endif
949700170323    2c*                  endif
949800170228     c
949900170228    1c                   endif
950000170228     c                   endsr
950100991028     O*-------------------------------------------------------------
950200991028     OFNBLP000  E            AGGTRA
950300991028     O                       BLPDT1
950400991028     O                       BLPFT1
950500040428     O*RICH. RECUPERO
950600040429     Olet132    E            RECTES         1 01
950700040428     O                                         +  1 '* FNLG20R *'
950800040428     O                       GMAUDT            +  1 '  /  /    '
950900040428     O          E            RECTES         1 10
951000040428     O                                         + 30 'SPETT. P.OPERATIVO DI'
951100040428     O                       DLNP              +  1
951200040428     O          E            RECTES         2
951300040428     O                                         + 30 'ATT.NE UFF.COMMER'
951400040428     O                                         +  0 'CIALE'
951500040428     O          E            RECTES         3
951600040428     O                       DLNA              +  1
951700040428     O                       UDATE         Y   +  1
951800040428     O          E            DIRTES         3 18
951900040428     O                                         +  1 'OGGETTO:'
952000040428     O                                         +  1 '- RECUPERO SPESE DIROTTA'
952100040428     O                                         +  0 'MENTO - NR.'
952200040428     O                       filarr            +  1
952300040428     O                                         +  0 '/'
952400040428     O                       §RIREC        Z   +  0
952500040428     O          E            RICTES         3 18
952600040428     O                                         +  1 'OGGETTO:'
952700040428     O                                         +  1 '- RECUPERO SPESE TRASPOR'
952800040428     O                                         +  0 'TO - NR.'
952900040428     O                       filarr            +  1
953000040428     O                                         +  0 '/'
953100040428     O                       §RIREC        Z   +  0
953200040428     O                                         +  0 '-'
953300040428     O          E            RECDET         2
953400040428     O                                         +  1 'SPEDIZIONE'
953500040428     O                       EXAAS             +  1
953600040428     O                       EXLNP             +  1
953700040428     O                       EXNRS             +  1
953800040428     O                       EXNSP         Z   +  1
953900040428     O                       VDSP              +  1 '  /  /    '
954000040428     O                                         +  1 ' / '
954100040428     O                       ARBLNA            +  1
954200040428     O                       DTBL              +  2
954300040428     O          E            RECDET         2
954400040428     O                                         +  1 'FATTURA'
954500040428     O                       AR6FIV        Z   +  1
954600040428     O                       AR6NFT        Z   +  1
954700040428     O                       GMADFT            +  1 '  /  /    '
954800040428     O                                         +  6 'CLIENTE'
954900040428     O                       ARBKSC        Z   +  1
955000040428     O          E            RECDET         1
955100040428     O                                         +  1 'MITTENTE'
955200040428     O                                           49 'DESTINATARIO'
955300040428     O          E            RECDET         1
955400040428     O                       ARBRSM            +  1
955500040428     O                       ARBRSD            +  1
955600040428     O          E            RECDET         1
955700040428     O                       ARBINM            +  1
955800040428     O                       ARBIND            +  1
955900040428     O          E            RECDET         1
956000040428     O                       ARBLOM            +  1
956100040428     O                       ARBLOD            +  1
956200040428     O          E            RECDET         2
956300040428     O                       ARBCAM            +  1
956400040428     O                       ARBPRM            +  1
956500040428     O                       ARBNZM            +  1
956600040428     O                       ARBCAD              46
956700040428     O                       ARBPRD            +  1
956800040428     O                       ARBNZD            +  1
956900040428     O          E            RECDET         2
957000040428     O                                         +  1 'RIF. MITT.'
957100040428     O                       ARBRMN        Z   +  1
957200040428     O                                         +  1 'COLLI'
957300040428     O                       ARBNCL        2   +  1
957400040428     O                                         +  1 'PESO'
957500040428     O                       ARBPKF        2   +  1
957600040428     O                                         +  1 'VOLUME'
957700040428     O                       ARBVLF        2   +  1
957800040428     O          E            DIRDET         3
957900040428     O                                         +  1 'LA MERCE E'''
958000040428     O                                         +  1 'STATA DIROTTATA A '
958100040428     O                       DNWLNA            +  1
958200040428     O          E            RICDET         3
958300040428     O                                         +  1 'LA MERCE E'''
958400040428     O                                         +  1 'STATA CONSEGNATA IN FRAN'
958500040428     O                                         +  0 'CO A:'
958600040428     O                       DNWLNA            +  1
958700040428     O          E            RECPRO         3
958800040428     O                                         +  1 'INOLTRE E'''
958900040428     O                                         +  1 'STATO AGGIUNTO IL C/ASS'
959000040428     O                                         +  0 'EGNO CON PROVV.AL MITTE'
959100040428     O                                         +  0 'NTE'
959200040428     O          E            RECFIN         3
959300040428     O                     31                  +  1 'CON SPEDIZIONE:'
959400040428     O                     31§LPLNP            +  7
959500040428     O                     31§LPAAS            +  1
959600040428     O                     31§LPNRS            +  1
959700040428     O                     31§LPNSP        Z   +  1
959800040428     O          E            RECFIN         1
959900040428     O                                         +  1 'NOTE:'
960000040428     O                       VNOT1             +  1
960100040428     O          E            RECFIN         2
960200040428     O                       VNOT2             +  7
960300040428     O          E            RECFIN         3
960400040428     O                                         +  1 'SI PREGA IL P.OPERATIVO'
960500040428     O                                         +  1 'DI'
960600040428     O                       DLNP              +  1
960700040428     O                                         +  1 'DI FARE BOLLA DI RECUP'
960800040428     O                                         +  0 'ERO.'
960900040428     O          E            RECFIN      2  1
961000040428     O                                         + 30 'DISTINTI SALUTI'
961100040428     O          E            RECFIN         3
961200040428     O                       DLNA              + 30
961300040428     O          E   N32N37N69RECFIN      2  1
961400040428     O                                         +  1 '- LA SPEDIZIONE IN'
961500040428     O                                         +  1 'OGGETTO E'''
961600040428     O                                         +  1 'STATA STORNATA.'
961700040428     O          E    32      RECFIN      2  1
961800040428     O         OR    37
961900040428     O                                         +  1 '- PER LA SPEDIZIONE IN'
962000040428     O                                         +  1 'OGGETTO E'''
962100040428     O                                         +  1 'STATA RICHIESTA NOTA'
962200040428     O                                         +  1 'ACCREDITO.'
962300040428     O          E    32      RECFIN      2  1
962400040428     O         OR    37
962500040428     O                                         +  1 '- V.NR. RICHIESTA'
962600040428     O                       ar6fiv            +  1
962700040428     O                                         +  0 '/'
962800950329     O*------------------------------------------------*
962900970410     O*  Avviso possibile richiesta nota accredito
963000950329     O*------------------------------------------------*
963100950329     O          E            AVVNAC         1 01
963200950329     O                                         +  1 '* FNLG20R *'
963300950329     O                       GMAUDT            +  1 '  :  :    '
963400950329     O          E            AVVNAC      2  1
963500950329     O                                           43 'ATT.NE UFFICIO'
963600950418     O                                           56 'CONTABILITA'''
963700950329     O          E            AVVNAC         3
963800961002     O                                           50 'P.OPERATIVO DI'
963900950329     O                       DLNA              +  1
964000950329     O          E            AVVNAC      1  2
964100950329     O                                           24 'OGGETTO: Bolle di recupe'
964200950329     O                                           48 'ro Spese Giacenza segue '
964300950329     O                                           55 'fattura'
964400950329     O          E            AVVNAC         1
964500950329     O                                           31 'Nell''esecuzione disposi'
964600950329     O                                           51 'zioni della giacenza'
964700950329     O                       CFGC          Z     55
964800950329     O                                           56 '/'
964900950329     O                       CNGC          Z     63
965000950329     O                                           67 'del'
965100950329     O          E            AVVNAC         1
965200950329     O                       CDGC          8     10
965300950329     O                                           31 'aperta su spedizione'
965400950329     O                       CAAS          Z     36
965500950329     O                                           37 '/'
965600950329     O                       CLNP          Z     40
965700950329     O                                           41 '/'
965800950329     O                       CNRS          Z     43
965900950329     O                                           44 '-'
966000950329     O                       CNSP          Z     51
966100950329     O                                           67 'è stata creata'
966200950329     O          E            AVVNAC         2
966300950329     O                                           24 'la seguente bolla di rec'
966400950329     O                                           30 'upero:'
966500950329     O          E            AVVNAC         0
966600950329     O                       CLNP1         Z      6
966700950329     O                                            7 '/'
966800950329     O                       CNRS1         Z      9
966900950329     O                                           10 '-'
967000950329     O                       CNSP1         Z     17
967100950329     O                                           21 'del'
967200950330     O                       CDSP1         8     32
967300950330     O          E            DETBOL         1
967400950330     O                       DIMP                51
967500950329     O                                           56 '= £.'
967600950329     O                       CIMP          2     67
967700950330     O          E            TOTIMV         1
967800950330     O                                           56 '= £.'
967900950330     O                                           67 '-------------'
968000950330     O          E            TOTIMV         1
968100950330     O                                           51 'Totale Imponibile'
968200950330     O                                           56 '= £.'
968300950330     O                       CIMV          2     67
968400950330     O          E            TOTIMV         1
968500950330     O                                           67 '============='
968600950329     O          E            PIENAC      1  1
968700950329     O                                           24 'tassata come segue fattu'
968800950329     O                                           37 'ra al cliente'
968900950329     O          E            PIENAC         1
969000950329     O                       CRAG                40
969100950329     O          E            PIENAC         1
969200950329     O                       CIND                40
969300950329     O          E            PIENAC         2
969400950418     O                       CCAP                14
969500950418     O                       CCIT                50
969600950418     O                       CPRV                53
969700950418     O                       CNAZ                57
969800950329     O          E            PIENAC         1
969900950329     O                                           33 'Poichè la giacenza è sta'
970000950418     O                                           57 'ta riaperta e quindi le '
970100950329     O                                           67 'spese sono'
970200950329     O          E            PIENAC         1
970300950329     O                                           24 'state recuperate con alt'
970400950329     O                                           48 're spedizioni, è probabi'
970500950329     O                                           67 'le che il  suddetto'
970600950329     O          E            PIENAC         3
970700950329     O                                           24 'cliente richieda Nota di'
970800950329     O                                           35 ' Accredito.'
970900950329     O          E            PIENAC         1
971000950418     O                                           33 'In questo caso allegare '
971100950418     O                                           57 'la presente al modulo di'
971200950418     O                                           67 ' richiesta'
971300950329     O          E            PIENAC         3
971400950329     O                                           24 'Nota Accredito che inolt'
971500950418     O                                           39 'rerete in sede.'
971600950329     O          E            PIENAC      1
971700950329     O                                           54 'Ufficio Giacenze'
971800950329     OFNLV22P   E            FINTO
971900000417** CMB
972000991206OVRPRTF FILE(FNLSB0P   ) OUTQ(          ) SAVE(*YES) FORMTYPE('          ')
972100991206** CMP
972200991206OVRPRTF FILE(FNLSB0P1  ) OUTQ(          ) SAVE(*YES) FORMTYPE('BOLLE-dpd ')
972300030304** CMPf
972400030304OVRPRTF FILE(FNLSB0P2  ) OUTQ(          ) SAVE(*YES) FORMTYPE('BOLLE-Fed ')
972500920206** CME
972600141210OVRPRTF FILE(FNLV22P   ) OUTQ(          )            FORMTYPE('          ')
972700920206** CMN
972800941013OVRPRTF FILE(LET132    ) OUTQ(*JOB      ) SAVE(*YES) FORMTYPE(          )
972900030715** CM4
973000141210OVRPRTF FILE(FNLSB5PA4 ) OUTQ(          )            FORMTYPE('          ')
973100030715** CM5
973200141210OVRPRTF FILE(FNLSB5PA5 ) OUTQ(          )            FORMTYPE('          ')
973300920206** DOV
973400920206DLTOVR  FILE(*ALL)
973500930427** ERR
973600970710Bolla Inesistente o già Consegnata                                             1
973700970710Bolla Legata: non eseguibile Cambio porto/Dirottam......                       2
973800970710Bolla inserita in Distinta di Consegna                                         3
973900970710Bolla GIACENTE: Agire da "GESTIONE GIACENZE"                                   4
974000970710Linea Arrivo Bolla Non in Gestione !!                                          5
974100970710Bolla Consegnata o Cons.Parzialm.: non possibile Cambio porto/Dirott.          6
974200970710Bolla Bloccata                                                                07
974300970710Tipo Consegna Errato !!                                                       08
974400970710Indicare Nuovo Porto !!                                                       09
974500970710Indicare Nuovo Porto SOLO SE DIVERSO !!                                       10
974600970710Se si fa RESO il C/Assegno deve essere ANNULLATO !!                           11
974700970710Variaz. C/A (S/N) non congruente con Importo o Divisa                         12
974800970710Indicare "S" per il calcolo Provvigioni C/Assegno                             13
974900970710A CHI addebitare provv. C/Assegno Errato !!                                   14
975000970710Variaz. Anticipata non congruente con Importo                                 15
975100970710Annullo C/Assegno : premere F8  per FORZARE ANNULLAMENTO !!                   16
975200970710Codice Fine Giacenza ERRATO !!                                                17
975300970710Si può richiamare Tassazione solo se linea arrivo é uguale !!                 18
975400970710Immettere dati anagrafici !!                                                  19
975500970710Provincia Incongruente con CAP  !! LIBERO LO MANDA PGM CTR CAP                20
975600970710Linea Arrivo errata !!                                                        21
975700970710Linea Arrivo incogruente con CAP : premere F9 x FORZARE !!!                   22
975800970710CAP ERRATO  !!                                                                23
975900970710Non Trovata Tariffa x Calcolo Provv.C/Assegno                                 24
976000970710Non Eseguibile variazione tipo bolla     LIBERO                               25
976100970710Non Eseguibile variazione Destinatario   LIBERO                               26
976200970710Non Eseguibile variazione Tassazione 1° bolla  LIBERO                         27
976300970710Non Eseguibile variazione Tassazione 2° bolla  LIBERO                         28
976400970710L. Arrivo diversa premere F8  x forzare dispos. in DIROTTAMENTO !!            29
976500970710Data errata, o < Apert. giacenza, o > Data del giorno                         30
976600970710Date Incongruenti                                                             31
976700970710Nessuna spesa da addebitare                                                   32
976800970710Immettere Importo Provv. C/Assegno                                            33
976900970710Non Trovata Tariffa giacenza !!                                               34
977000970710Non creata Bolla: premuto CMD12 da immiss.bolle! RIPETERE OPERAZ.             35
977100970710Non si possono eseguire le disposizioni prima della data indicata !           36
977200970710Divisa errata !                                                               37
977300970710La Divisa non prevede l'uso di decimali nell'importo C/assegno                38
977400970710Se si varia C/assegno immettere anche la Divisa !                             39
977500970710Nazione errata !                                                              40
977600970710Se si varia Divisa digitare anche l'importo del C/Assegno                     41
977700970710L.Arrivo uguale premere F8 per forzare disposizioni in "CONSEGNA"             42
977800970710Tipo incasso C/assegno errato o non congruente con valuta                     43
977900970710Non si puo' gestire Anticipata su una bolla in Franco                         44
978000970710Bolla C/S per comunicaz. giac. e possibile fare solo DIROTTAMENTO             45
978100970710Non è possibile eseguire cambio di porto su bolla di RIENTRO                  46
978200970710Cambio di porto non ammesso se immesse disposizioni di reso                   47
978300970710Impossibile aggiungere il C/Assegno se immesse disposizioni di reso           48
978400970710Impossibile eseguire reso in conto servizio su bolla di rientro               49
978500970710Inserire il motivo per cui si rende la merce !!!                              50
978600970710Impossibile eseguire dirottamento per bolle export                            51
978700970710Attenzione !! Modificata Località                                             52
978800970710Attenzione !! Modificato CAP                                                  53
978900970710Attenzione !! Modificata Provincia                                            54
979000970710Attenzione !! Modificata Nazione                                              55
979100971006L.Arrivo diversa impossibile effettuare DIROTTAMENTO: mittente non codificato 56
979200971113L.Arrivo non gestita dall'Azienda !!!                                         57
979300990203Impossibile eseguire cambio porto/dirottamento: bolla con C.A. Aperta         58
979400991028La Divisa non prevede l'uso di decimali nEll'importo                          59
979500991028Codice divisa inesistente                                                     60
979600991028Divisa non congruente con le altre della tassazione                           61
979700000120Bolla di recupero: impossibile effettuare cambi di porto/resi/dirottamenti    62
979800040206Impossibile eseguire cambio porto/dirottamento:PER BOLLE POSTE                63
979900040206La divisa deve essere inserita se immesso un importo nel C/A                  64
980000040531Cambio linea arrivo non ammesso la LOC.o il CAP sono serviti dalla attuale    65
980100071130ATTENZIONE verificare la data consegna richiesta della bolla !!!!!            66
980200110909Spedizione passata al PDA. Ricevere l'annullamento o chiudere la distinta !!! 67
980300151204ATTENZIONE presenti disposizioni Dirottamento/Reso spedizione non gestibile   68
980400141105ATTENZIONE esistono delle disposizioni di consegna che devono essere risolte  69
