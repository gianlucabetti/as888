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
038500171122     d tisie10ds1    e ds
038600151020     d fieuLPDds     e ds
038700151020     d Dlpd          e ds
038800040716     d fidn12ds      e ds
038900171003      *ds per richiamo monitor variazioni
039000171003     d tigcp         e ds                  extname(tigcp00f)
039100171003     d fnlg33ds      e ds                  inz
039200171003     d tigcpa        e ds                  extname(tigcp00f)
039300171003     d                                     prefix(a)
039400171003     d tigcpb        e ds                  extname(tigcp00f)
039500171003     d                                     prefix(b)
039600140217     D fnlrn6ds      E DS
039700020621     D CdIVA           S              4
039800020927      *parametri per reperimento coda
039900051013     d dataiso         s               d
040000051013     d finep           s              1
040100020927     d codpo           s              3s 0
040200020927     d tpcoda          s              1
040300020927     d nomcoda         s             10
040400020927     d libcoda         s             10
040500020927     d esito           s              1
040600030807     d compar          s                   like(arbgva)
040700040315     d WORIGINE        s                   like(nchorigine)
040800040316     d Wpocompl        s                   like(nctpocompl)
040900170228
041000170228     d wlnpb           s                   like(§lplnp)
041100170323     d*idx             s              3  0
041200940928     D*
041300061026      * campi salvataggio chiave bolla originale da dirottamento
041400061026     d LBLAAOsv        s                   like(LBLAAO)
041500061026     d LBLLPOsv        s                   like(LBLLPO)
041600061026     d LBLNROsv        s                   like(LBLNRO)
041700061026     d LBLNSOsv        s                   like(LBLNSO)
041800061026      *
041900041221      * eccezioni luogo 300 partenza arrivo
042000041221     D dpar            DS
042100041221     D  par                    1     60  0 dim(20)
042200041221     D darr            DS
042300041221     D  arr                    1     60  0 dim(20)
042400060807     D dzon            DS
042500060807     D  zon                    1     40  0 dim(20)
042600070322     D dgir            DS
042700070322     D  gir                    1     60    dim(6)
042800070613      *
042900070613     d skvdmc          s             50    dim(4)
043000041221      *
043100021009     d kpjbus          s                   like(kpjbu)
043200071130      *
043300071130     d Wdata8          DS
043400071130     d  dadata                 1      8  0
043500071130     d  adata                  9     16  0
043600071130     d  GioLav                17     21  0
043700071130      *
043800940928     D* DEFINIZIONE COSTANTI
043900940928     D*
044000940928     D CNOTA           C                   CONST('** NESSUNA NOTA **')
044100950330     D*-------------------------------------------------------------*
044200950330     D* Costanti per definzione vari importi della bolla di recupero
044300950330     D*-------------------------------------------------------------*
044400950330     D CVAR            C                   CONST('Importo Varia    ')
044500950330     D CTRA            C                   CONST('Trasporto        ')
044600950330     D CDIR            C                   CONST('Diritto fisso    ')
044700950330     D CINL            C                   CONST('Inoltro          ')
044800951214     D CNOT1           C                   CONST('C.PORTO DA ASS. A FR')
044900951214     D CNOT2           C                   CONST('ANCO CL.       ')
045000961129     D CNOT3           C                   CONST('EX.SPED. IN FRANCO F')
045100961129     D CNOT4           C                   CONST('RONTIERA       ')
045200000925     D IMPORT          C                   CONST('I              ')
045300020621
045400940304     IAZORG01L  AA
045500971113     I                                  1    3  ORGDIT
045600971113     I                                  6    6  ORGFVA
045700971113     I                                 11   11  ORGFL1
045800940304     I                                 14   33  ORGDES
045900960327     I                                 34   53  ORGIND
046000960327     I                             P   54   56 0ORGCPF
046100960327     I                                 57   76  ORGLOC
046200960327     I                                 77   78  ORGPRO
046300000202     I                               3776 3800  ORGDE3
046400070416     I                               3851 3875  ORGDE6
046500051221     I                               3901 3925  ORGDE8
046600140729     I                               3951 3975  ORGDF0
046700990730     ITITGC000
046800900424     I              TGCSG1                      TGS(1)
046900900424     I              TGCSG2                      TGS(2)
047000900424     I              TGCSG3                      TGS(3)
047100900424     I              TGCST1                      TTS(1)
047200900424     I              TGCST2                      TTS(2)
047300900424     I              TGCST3                      TTS(3)
047400950330     I*-------------------------------------------------------------*
047500140729     C*
047600970529     C     *ENTRY        PLIST
047700970529     C                   PARM                    KPJBA
047800141006     C                   PARM                    fnlg20ds
047900970529     C* PASS. PARAMETRI DA PGM CHIAMANTE
048000970529     C                   MOVEL     KPJBU         FNLG82
048100970529     C                   MOVEL     KPJBU         §KPJBU
048200050316     c                   move      d82comm       gescomm           1
048300141007     c                   if        %parms > 1
048400141007     C                   movel     'N'           d82nodsp
048500141007     c                   endif
048600030715     c* mi compongo la ds trul90ds per pgm FNLS01R, FNLR41/8R
048700030715     c                   exsr      srtrul90
048800970529     C* PRIMA VOLTA
048900970529     C     WPRIMA        IFEQ      ' '
049000970529     C                   EXSR      DEFCAM
049100970529     C                   MOVEL     'S'           WPRIMA            1
049200970529     C                   END
049300970529     C*
049400940304     C                   MOVEL     '0'           $FINE
049500940304     C     $FINE         DOWEQ     '0'
049600940301      *
049700940301     C     $GIAC         IFEQ      '1'
049800940301     C                   EXSR      SRGIAC
049900920113     C                   ELSE
050000940301     C                   EXSR      SRDIRO
050100940301     C                   END
050200940304      *
050300940304     C     $FINE         IFEQ      '0'
050400940304     C                   MOVEL     '0'           $OKF01
050500940304     C                   MOVEL     '0'           $OKF02
050600940304     C                   MOVEL     '0'           $OKF03
050700940310     C                   MOVEL     '0'           $FOANC
050800940310     C                   MOVEL     '0'           $FOLNA
050900940310     C                   MOVEL     '0'           $FOFRI
051000940301      *
051100940304      * =======================================================
051200940304     C*?$F01 = VIDEO X DISPOSIZIONI
051300940304      * =======================================================
051400940307     C                   MOVEL     '1'           $F01
051500940307     C     $F01          DOWEQ     '1'
051600141003     c     d82nodsp      ifeq      *blank
051700940304     C                   EXSR      SRF01
051800141006     c                   else
051900141006      * se richiamato senza video salto prima routine e passo a riempire campi
052000141006      * video per completare il dirottamento
052100141006     C                   EXSR      azzer
052200141006     c                   move      '1'           $okf01
052300141003     c                   endif
052400961213      *---------------------------------------------------------*
052500961213      *  Se sto eseguendo delle disposizioni per bolle export
052600961213      *  controllo se devo emettere mascherina x fax
052700971113     C                   MOVEL     *BLANKS       ORGFL1
052800971113     C                   MOVEL     *BLANKS       ORGDIT
052900971113     C     ARBLNA        CHAIN     AZORG01L                           21
053000971113     C                   MOVEL     ORGDIT        SIFDIT            3
053100000314     C                   MOVEL     ORGDE3        OG143
053200020729     C                   MOVEL     *blank        flgest            1
053300020729     C     §OgNTW        ifeq      'FED'
053400020729     C     §OgNTW        oreq      'EEX'
053500020729     C     §OgNTW        oreq      'EUP'
053600020729     C                   MOVEL     'E'           flgest
053700020729     c                   end
053800020225      *PER FEDEX NON MANDA MAI IL FAX
053900020225     C     §OGNTW        IFNE      'FED'
054000020729     C     flgest        IFEQ      'E'
054100000314     C     VFRI          ANDNE     'D'
054200020702     C     §OGntw        OREQ      'DPD'
054300961213     C     VFRI          ANDNE     'D'
054400961213     C     $GIAC         IFEQ      '1'
054500961213     C     ARBFT3        ORNE      *BLANKS
054600961213     C     VFRI          OREQ      'R'
054700961213     C                   EXSR      CALL21
054800961213     C                   END
054900961213     C                   END
055000020225     C                   END
055100940304      * =======================================================
055200940304     C*?$F02 = VIDEO X DESTINATARIO /LNA/ COD.FINE GIACENZA...
055300940304      * =======================================================
055400940304     C     $OKF01        IFEQ      '1'
055500940304     C                   EXSR      SRF02
055600940304     C                   ENDIF
055700940307     C                   ENDDO
055800900418     C*
055900940304     C                   ENDIF
056000950113     C* SE PASSATO NR.SPEDIZ.:PGM RICHIAMATO ..VADO A FINE
056100950113     C     D82NSP        IFNE      *BLANK
056200950113     C                   MOVEL     '1'           $FINE
056300950113     C                   ENDIF
056400940304     C                   ENDDO
056500940303      *-------------------------------------------------------
056600940308     C     FINE          TAG
056700940303      *-------------------------------------------------------
056800950119     C* SE NON E' RICHIAM. DA GIACENZE O DA FNLG82R
056900950119     C* ---------->    CHIAMO PGM X SOTTOMETTERE
057000930511     C* RICEZIONE VARIAZ. LOCALI
057100940303     C     $GIAC         IFEQ      '0'
057200950119     C     D82NSP        ANDEQ     *BLANK
057300930511     C                   END
057400940303      *
057500050905     C*    D82F12        IFNE      '1'
057600050905     C*                  MOVEL     §KPJBU        KPJBU
057700050905     C*                  END
057800020227      *RICHIAMO PROGRAMMA PER CHIUSURA FILE
057900020227     C                   CLEAR                   TR27DS
058000020227     C                   MOVE      'C'           I27TLA
058100160303     C                   CALL      'TRUL27R1'
058200020227     C                   PARM                    TR27DS
058300160303      *
058400160303     C                   CLEAR                   TRulc7ds
058500160303     C                   MOVE      'C'           Ic7TLA
058600160303     C                   CALL      'TRULC7R'
058700160303     C                   PARM                    trulc7ds
058800080124      *
058900080124     C                   CLEAR                   fnlv59ds
059000080124     C                   MOVE      'C'           Ilv59tla
059100080124     C                   CALL      'FNLV59R'
059200080124     C                   PARM                    fnlv59ds
059300080124     c
059400050701     C                   CLEAR                   fnlg09ds
059500050701     C                   MOVEL     'C'           d09TLA
059600050701     c                   movel     fnlg09ds      kpjbu
059700050701     C                   CALL      'FNLG09R'
059800050701     C                   PARM                    kpjba
059900041125     c
060000041125     C                   CALL      'FNLG25R'
060100041125     C                   PARM                    recordlbl
060200041125     C                   PARM      'C'           i25tla
060300050905     c*
060400050905     C                   MOVEL     fnlg82        KPJBU
060500900418     C                   SETON                                        LR
060600940301     C/SPACE 3
060700940309     C*?$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
060800940308     C*?    R O U T I N E    CONTROLLO
060900940309     C*?$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
061000920113     C/SPACE 3
061100940304     C*---------------------------------------------------------------*
061200940304     C*   S R F 0 1    - FORMATO DISPOSIZIONI                         *
061300940304     C*---------------------------------------------------------------*
061400940304     C     SRF01         BEGSR
061500940304     C                   MOVEL     '1'           $F01
061600940304     C                   MOVEL     '0'           $OKF01
061700940304     C                   MOVEL     *BLANK        VNOT1
061800940304     C                   MOVEL     *BLANK        VNOT2
061900940304     C     $F01          DOWEQ     '1'
062000940304     C                   SETOFF                                       95
062100940304      *
062200940304     C     $GIAC         IFEQ      '0'
062300940304      * CONTROLLO  DISPOSIZ. IMMESSE A VIDEO DA BOLLE
062400940309     C                   EXSR      ARBVID
062500940304     C   KF              MOVEL     '0'           $F01
062600940304     C   KF              MOVEL     '1'           $OKF01
062700940304     C   KL              MOVEL     '0'           $F01
062800940304     C   KL              MOVEL     '0'           $OKF01
062900970204     C   KLD82NSP        IFNE      *BLANKS
063000970204     C                   MOVEL     '1'           D82F12
063100970204     C                   MOVEL     FNLG82        KPJBU
063200970204     C                   END
063300940304     C                   ELSE
063400961203      * CARICO LE DISPOSIZIONI  DELLA GIAC. A VIDEO
063500940304     C                   EXSR      GCAVID
063600961203     C* SCRIVO PRIMA PARTE  VIDEO
063700961203     C                   WRITE     LG20D08
063800961203     C* GESTIONE NOTE GIACENZE -  SFL
063900961203     C                   EXSR      GCANOT
064000941012     C                   MOVEL     '1'           $OKF01
064100941012     C                   MOVEL     '0'           $F01
064200961203     C* NON SI POSSONO ESEGUIRE DISP. PRIMA
064300961203     C     GCPDED        IFGT      AMGUDT
064400941012     C                   EXFMT     LG20Z09
064500941012     C                   MOVEL     '0'           $OKF01
064600941012     C                   MOVEL     '0'           $F01
064700941012     C                   MOVEL     '1'           $FINE
064800940928     C                   ENDIF
064900940304     C                   END
065000940304      *
065100940304     C*   ESECUZIONE DISPOSIZIONI
065200940304      *   -----------------------
065300940304     C     $OKF01        IFEQ      '1'
065400940304     C                   EXSR      AZZER
065500940304     C*
065600940304     C* TEST SULLE DISPOSIZIONI PER EMETTERE LE VIDEATE NECESSARIE
065700940304     C                   SELECT
065800940304     C     VFRI          WHENEQ    'R'
065900940304     C                   EXSR      RIENTR
066000940304     C     VFRI          WHENEQ    'C'
066100940304     C                   EXSR      RICONS
066200940304     C     VFRI          WHENEQ    'D'
066300940310     C                   EXSR      DIROT1
066400940304     C                   ENDSL
066500940304     C                   ENDIF
066600940304     C                   ENDDO
066700940304     C     ENDF01        ENDSR
066800940308     C*---------------------------------------------------------------*
066900940308     C*   S R F 0 2    - FORMATO X FINE GIAC./DESTINAT/ LNA ....      *
067000940308     C*---------------------------------------------------------------*
067100940308     C     SRF02         BEGSR
067200141003     C                   IF        D82nodsp <> *blank
067300141013      *forza indicatore per sproteggere campi destinatario
067400141013     c                   seton                                        46
067500141013      *
067600151127     c                   if        vfri = 'R'
067700151127     c                   exsr      rientr
067800151127     c                   else
067900141003     C                   MOVEL     d20RSD        VRSD
068000141003     C                   MOVEL     d20Rs2        VRD2
068100141003     C                   MOVEL     d20IND        VIND
068200141003     C                   MOVEL     d20LOD        VLOD
068300141003     C                   MOVEL     d20PRD        VPRD
068400141003     C                   MOVE      d20CAD        VCAD
068500141003     C                   MOVE      d20LNA        VLNA
068600141003     C                   MOVEL     d20NZD        VNZD
068700141003     C                   MOVEL     d20tel        Vteld
068800141003     C                   MOVEL     d20ref        Vref
068900141003     c                   move      d20snp        vapc
069000141003     c                   move      d20mdp        vppc
069100151127     c                   endif
069200141003     C                   MOVEL     '0'           $FOFRI            1
069300141003     c                   else
069400940309     C* SE HO FORZATO DISPOSIZ. REIMPOSTO I DATI DIGITATI
069500940309     C     $FOFRI        IFEQ      '1'
069600940309     C                   MOVEL     SAVRSD        VRSD
069700941011     C                   MOVEL     SAVRD2        VRD2
069800940309     C                   MOVEL     SAVIND        VIND
069900940309     C                   MOVEL     SAVLOD        VLOD
070000940309     C                   MOVEL     SAVPRD        VPRD
070100940309     C                   MOVE      SAVCAD        VCAD
070200940309     C                   MOVE      SAVLNA        VLNA
070300961203     C                   MOVEL     SAVNZD        VNZD
070400031121     C                   MOVEL     SAVteld       Vteld
070500031121     C                   MOVEL     SAVref        Vref
070600940309     C                   MOVEL     '0'           $FOFRI            1
070700940309     C                   END
070800141003     c                   endif
070900940309     C* SE HO FORZATO LINEA D'ARRIVO
071000940309     C                   MOVEL     '0'           $FOLNA            1
071100940309     C                   MOVEL     '0'           $RIEMI            1
071200940309     C                   SETOFF                                         1922
071300940308     C                   SETOFF                                       566265
071400941219     C                   SETOFF                                       344959
071500940308     C                   MOVEL     *BLANK        $MSG
071600940310     C     INIF02        TAG
071700941221     C                   SETOFF                                       9934
071800940308     C* EMETTO VIDEATA CON DATI FINE GIACENZA
071900940308     C                   MOVEL     '1'           $F11
072000961203      *---------------------------------------------------------*
072100961213     C     $F11          DOWEQ     '1'
072200940308     C                   SETON                                        95
072300940915     C                   WRITE     LG20Z02
072400940308     C     $GIAC         IFEQ      '0'
072500940915     C                   WRITE     LG20D04
072600940308     C                   ELSE
072700940915     C                   WRITE     LG20D08
072800940308     C                   EXSR      GCANOT
072900961203     C                   END
073000070619      *se disposizioni in consegna attivo comando per note ldv
073100091126      * e protezione campo nazione
073200091126     c                   setoff                                       1715
073300070619     c                   if        vfri = 'C'
073400091126     c                   seton                                        1715
073500070619     c                   endif
073600070619
073700940915     C                   WRITE     LG20Z03
073800940308     C                   MOVEL     '1'           $F02
073900940308     C     $F02          DOWEQ     '1'
074000940308     C                   MOVEL     '0'           $OKF02
074100940308     C                   MOVEL     '0'           $OKF03
074200940915     C                   EXFMT     LG20D06
074300070622      * richiamo pgm interrogazione bolle
074400070622     C   Kg              exsr      richbl
074500070622
074600950103     C  NKL              EXSR      CTRV02
074700070613      *richiamo pgm note ldv
074800070614     c                   if        *inkr and §oggeot = 'S'
074900070613     c                   clear                   skvdmc
075000070613      *carico eventuali note da passare al pgm
075100070614      *solo se è una giacenza
075200070614     c                   if        $giac = '1'
075300070613     c                   do        4             sfl               1 0
075400070614     c     sfl           chain     lg20s05                            60
075500070614     c                   if        not *in60
075600070613     c                   movel     vdmc          skvdmc(sfl)
075700070613     c                   endif
075800070613     c                   enddo
075900070614     c                   endif
076000070613      *
076100140217     c                   clear                   fnlrn6ds
076200070613     C                   movel     kpjbu         WSVKPJ          256
076300140217     C                   move      'I'           in6mod
076400140217     C                   z-add     arbaas        in6AAS
076500140217     C                   z-add     arblnp        in6LNP
076600140217     C                   z-add     arbnrs        in6NRS
076700140217     C                   z-add     arbnsp        in6NSP
076800140217     C                   movel     skvdmc(1)     in6not1
076900140217     C                   movel     skvdmc(2)     in6not2
077000140217     C                   movel     skvdmc(3)     in6not3
077100140217     C                   movel     skvdmc(4)     in6not4
077200140217     C                   movel     'FNLG20R'     in6pgm
077300140217     C                   call      'FNLRN6R'                            98
077400070613     C                   parm                    kpjba
077500140217     C                   parm                    fnlrn6ds
077600070613     C                   movel     WSVKPJ        kpjbu
077700070613     c                   endif
077800070614     C     *INKL         IFEQ      '1'
077900940308     C                   SELECT
078000141007     C                   WHEN      d82nodsp <> *blank
078100141007     c                   move      'R'           d20erro
078200141007     C                   return
078300940308     C     $GIAC         WHENEQ    '0'
078400940308     C                   MOVEL     '0'           $OKF02
078500940308     C                   MOVEL     '0'           $F02
078600940308     C                   MOVEL     '0'           $F11
078700940308     C                   MOVEL     '1'           $F01
078800940308     C     $GIAC         WHENEQ    '1'
078900940308     C                   MOVEL     '0'           $OKF02
079000940308     C                   MOVEL     '0'           $F02
079100940308     C                   MOVEL     '0'           $F11
079200940308     C                   MOVEL     '0'           $F01
079300940308     C                   MOVEL     '1'           $FINE
079400940308     C                   END
079500940308     C                   ELSE
079600940308      *ERRORI
079700940308     C     *IN19         IFEQ      '1'
079800940308     C     *IN30         OREQ      '1'
079900940308     C                   MOVEL     '1'           $F02
080000940308     C                   MOVEL     '0'           $OKF02
080100940308     C                   ELSE
080200940308     C   KF              MOVEL     '1'           $OKF02
080300940308     C   KF              MOVEL     '0'           $F02
080400940308     C   KF              MOVEL     '0'           $F01
080500940308     C   KF$GIAC         IFEQ      '1'
080600940308     C                   MOVEL     '1'           $FINE
080700940308     C                   END
080800940308     C                   END
080900941215      * SE  HO FORZATO DIROTTAMENTO MA DISPOSIZ. ERANO RICONSEGNA
081000941215      * SE  HO FORZATO RICONSEGNA DISPOSIZ. ERANO DIROTTAMENTO
081100940309     C     $FOFRI        IFEQ      '1'
081200941215     C     VFRI          IFEQ      'C'
081300940308     C                   MOVEL     'D'           VFRI
081400940309     C     $GIAC         IFEQ      '1'
081500940309     C                   MOVEL     'M'           VPSG
081600940309     C                   MOVEL     'S'           VASG
081700940309     C                   END
081800941215     C                   ELSE
081900941215     C                   MOVEL     'C'           VFRI
082000941215     C                   ENDIF
082100940308     C                   MOVEL     '0'           $OKF02
082200940308     C                   MOVEL     '0'           $F02
082300940308     C                   MOVEL     '0'           $F11
082400940308     C                   MOVEL     '1'           $F01
082500940308     C                   END
082600940309     C     $RIEMI        IFEQ      '1'
082700940309     C                   MOVEL     '0'           $OKF02
082800940309     C                   MOVEL     '0'           $F02
082900940309     C                   MOVEL     '1'           $F11
083000940310     C                   MOVEL     '0'           $RIEMI
083100940309     C                   END
083200940308     C                   END
083300940308     C     $OKF02        IFEQ      '1'
083400940308     C     *IN33         IFEQ      '1'
083500940308     C     *IN35         OREQ      '1'
083600940308     C     *IN36         OREQ      '1'
083700940308      * $F03 = VIDEO X RECUPERO SPESE GIAC./PROVV. C.ASSEGNO....
083800940308     C                   MOVEL     '1'           $F03
083900940308     C                   EXSR      SRF03
084000940308     C                   ELSE
084100940308     C                   MOVEL     '1'           $OKF03
084200940308     C                   END
084300940308     C     $OKF03        IFEQ      '1'
084400940308     C                   MOVEL     '0'           $F02
084500940308     C                   MOVEL     '0'           $F11
084600941221     C                   CLEAR                   DSLR48
084700020617      *emissione video controllo responsabilità cliente o vettore
084800020617     c                   move      *blank        viddcli
084900030617     C     $GIAC         ifne      '1'
085000020617     C     VFRI          ifeQ      'D'
085100020617     C     WVARIO        andne     0
085200020617     C     WVARIO        ANDNE     8888
085300020617     C     WVARIO        ANDNE     9999
085400030620     c                   eval      viddtit = 'DIROTTAMENTO'
085500020617     C                   MOVEL     'A'           §TRC
085600060213     C     KAR4          CHAIN(N)  FiAR401L                           43
085700020617     C     *IN43         IFEQ      '0'
085800020617     C                   MOVEL     AR4NOT        DSBL4A
085900030917      * se è un supermercato la responsabilità è del vettore
086000030918     c                   if        arbtc1 = 'S' and §b4aso = '%' or
086100030918     c                             arbtc2 = 'S' and §b4aso = '%'
086200030917     c                   move      'V'           viddcli
086300030917     c                   else
086400030917      * disk C o disk A con errato instradamento responsabilità cliente
086500030918     c                   if        §b4aso = '§' or §b4aso = ':' or §b4aso = '%'
086600020617     c                   move      'C'           viddcli
086700020617     c                   else
086800020617     c                   move      'V'           viddcli
086900020617     c                   end
087000030917     c                   end
087100021113     c                   if        viddcli = 'C'
087200021113     C     VRSD          IFeq      WSVRSD
087300021113     C     VRD2          andeq     WSVRD2
087400021113     C     VIND          andeq     WSVIND
087500021113     C     VCAD          andeq     WSVCAD
087600021113     C     VPRD          andeq     WSVPRD
087700021113     C     VLOD          andeq     WSVLOD
087800021113     C     VNZD          andeq     WSVNZD
087900021113     c                   move      'V'           viddcli
088000021113     c                   end
088100021113     c                   end
088200030617     C                   END
088300020617     C                   END
088400030617     c* imposto come default l'addebito delle spese x il cambio di
088500030617     c* porto al cliente
088600030617     C                   if        (vfri = 'C' and vnpc = 'F')
088700030617     c                   move      'C'           viddcli
088800030620     c                   eval      viddtit = 'CAMBIO DI PORTO'
088900030617     c                   end
089000030617     c* emetto il video per chiedere a chi fare l'addebito delle spese
089100030620     C                   if        (vfri = 'C' and vnpc = 'F') OR
089200030620     C                             (vfri = 'D' and WVARIO <> 0 and
089300030620     C                             wvario <> 8888 and wvario <> 9999)
089400020617     c                   do        *hival
089500020617     C                   EXFMT     LG20w01
089600020617     c   kf              leave
089700020617     c                   end
089800030617     c                   end
089900030617     c                   end
090000940308     C* VARIAZIONE BOLLA  C/ASS. O CODICE BOLLA
090100940308     C     *IN80         IFEQ      '1'
090200940308     C                   EXSR      PGARBK
090300941214     C     D48ERR        IFNE      ' '
090400941214     C                   SETON                                        19
090500941214     C                   MOVEL     D48MSG        $MSG
090600941214     C                   MOVEL     '1'           $F11
090700941214     C                   ENDIF
090800940310     C                   END
090900940308     C     *IN81         IFEQ      '1'
091000941219     C     $F11          ANDEQ     '0'
091100940308     C* VARIAZIONE BOLLA  IND. DESTINATARIO
091200051017     C                   MOVEL     'S'           D48FFR
091300940308     C                   EXSR      PGARBI
091400941214     C     D48ERR        IFNE      ' '
091500941214     C                   SETON                                        19
091600941214     C                   MOVEL     D48MSG        $MSG
091700941214     C                   MOVEL     '1'           $F11
091800941214     C                   ENDIF
091900940308     C                   END
092000940308     C* VARIAZIONE BOLLA  TASSAZ. 1° BOLLA
092100940308     C     *IN82         IFEQ      '1'
092200941219     C     $F11          ANDEQ     '0'
092300940308     C                   EXSR      PGARBT
092400941215     C     D48ERR        IFNE      ' '
092500941214     C                   SETON                                        19
092600941214     C                   MOVEL     D48MSG        $MSG
092700941214     C                   MOVEL     '1'           $F11
092800941214     C                   ENDIF
092900940308     C                   END
093000940308     C* VARIAZIONE BOLLA  TASSAZ. 2° BOLLA
093100940308     C     *IN83         IFEQ      '1'
093200941219     C     $F11          ANDEQ     '0'
093300940308     C                   EXSR      PGAR2T
093400941214     C     D48ERR        IFNE      ' '
093500941214     C                   SETON                                        19
093600941214     C                   MOVEL     D48MSG        $MSG
093700941214     C                   MOVEL     '1'           $F11
093800941214     C                   ENDIF
093900940308     C                   END
094000940308      * SE NON HO ERRORI OKF03 =1
094100940308     C                   ENDIF
094200940308     C                   ENDIF
094300940308     C                   ENDDO
094400940308      *
094500940308     C                   ENDDO
094600940308      * =======================================================
094700940308     C*?SUPERATI TUTTI I CONTROLLI   AGGIORNO
094800940308      * =======================================================
094900940308     C     $OKF03        IFEQ      '1'
095000940613      * SE ACCESO IND. PER BOLLA IN ARRIVO, MA è CAMBIATA LNA SPENGO 88
095100941217     C   88VLNA          IFNE      EXLNA
095200940613     C                   SETOFF                                       88
095300940613     C                   ENDIF
095400940613      *
095500940308     C                   EXSR      SRAGG
095600940308     C                   ENDIF
095700940310     C   99              SETON                                        19
095800940310     C   99              MOVEL     ERR(35)       $MSG
095900940308     C   99              GOTO      INIF02
096000940308     C     ENDF02        ENDSR
096100940308     C/SPACE 3
096200961202     C*---------------------------------------------------------------*
096300961202     C*   CALL21: RICHIAMO VIDEATA FAX
096400961202     C*---------------------------------------------------------------*
096500961202     C     CALL21        BEGSR
096600021009     C*-                                                   *
096700021009     c                   move      'L'           finep
096800021009     c                   z-add     filarr        codpo
096900021009     c                   move      '2'           tpcoda
097000021009     c                   clear                   nomcoda
097100021009     c                   clear                   libcoda
097200021009     c                   clear                   esito
097300021009     c                   call      'TRULOUTQ'
097400021009     c                   parm                    finep
097500021009     c                   parm                    codpo
097600021009     c                   parm                    tpcoda
097700021009     c                   parm                    nomcoda
097800021009     c                   parm                    libcoda
097900021009     c                   parm                    esito
098000021009     c                   if        esito <> '0' or nomcoda = *blank
098100021009     c                   eval      nomcoda = 'QDKT'
098200021009     c                   endif
098300961202     C*
098400961202     C                   CLEAR                   FNLG21
098500961202     C     $GIAC         IFEQ      '1'
098600050222     C                   Z-ADD     GCPAASa       D21AAS
098700050222     C                   Z-ADD     GCPLNPa       D21LNP
098800050222     C                   Z-ADD     GCPNRSa       D21NRS
098900050222     C                   Z-ADD     GCPNSPa       D21NSP
099000961202     C                   Z-ADD     GCPAGC        D21AGC
099100961202     C                   Z-ADD     GCPFGC        D21FGC
099200961202     C                   Z-ADD     GCPNGC        D21NGC
099300970418     C                   Z-ADD     0             D21FRG
099400961202     C                   MOVEL     'G'           D21TPR
099500961202     C                   MOVEL     VFRI          D21DIS
099600961202     C                   ELSE
099700961202     C                   Z-ADD     ARBAAS        D21AAS
099800961202     C                   Z-ADD     ARBLNP        D21LNP
099900961202     C                   Z-ADD     ARBNRS        D21NRS
100000961202     C                   Z-ADD     ARBNSP        D21NSP
100100961202     C                   MOVEL     'C'           D21TPR
100200961202     C                   MOVEL     VFRI          D21DIS
100300961202     C                   END
100400020927     C                   MOVE      nomcoda       D21PRF
100500000314     C*  Se lna Dpd imposto apposito flag al pgm gestione
100600000314     C*  invio fax in quanto cambia il modo di reperire il
100700000314     C*  il codice dell'anagrafica
100800020617     C     §OGntw        IFEQ      'DPD'
100900000314     C                   MOVEL     'S'           D21DPD
101000000314     C                   END
101100961202     C*  Richiamo videata x fax
101200961202     C                   MOVEL     FNLG21        KPJBU
101300961213     C                   CALL      'FNLG21C'
101400961202     C                   PARM                    KPJBA
101500961202     C*
101600961202     C                   ENDSR
101700940308     C*---------------------------------------------------------------*
101800940308     C*   C T R V 0 2      FINE GIACENZA                              *
101900940308     C*---------------------------------------------------------------*
102000940308     C     CTRV02        BEGSR
102100940309     C                   SETOFF                                         1922
102200940308     C                   SETOFF                                       566265
102300941003     C                   SETOFF                                       204959
102400941219     C                   SETOFF                                       66  64
102500940308     C                   MOVEL     *BLANK        $MSG
102600940310     C     $GIAC         IFEQ      '1'
102700941012     C                   MOVEA     VCFG          CFG§
102800940310     C     '?'           LOOKUP    CFG§                                   21
102900940308     C   21              DO
103000940308     C                   MOVEL     '2C'          §COD
103100940308     C                   MOVEL     *BLANK        §KEY
103200940308     C                   MOVEL     *BLANK        §DES
103300940308     C                   MOVE      *BLANK        VCFG
103400940308     C                   CALL      'X§TB60R'
103500940308     C                   PARM                    CODUT
103600940308     C                   PARM                    §COD
103700940308     C                   PARM                    §KEY
103800940308     C                   PARM                    §DES
103900940308     C                   MOVEL     §KEY          VCFG
104000940308     C                   MOVEL     §DES          DCFG
104100940309     C                   MOVEL     '1'           $RIEMI            1
104200940309     C                   GOTO      ENDCT2
104300940308     C                   END
104400071203      * verifico se sto dando disposizioni di riconsegna ad una giacenza
104500071203      * che ha in bolla la dcr impostata con una data maggiore della udate
104600071130      * ATTENZIONE non è un errore ma solo una segnalazione UNA_TANTUM
104700071130     c                   if        contr_dcr = *blank and
104800071130     c                             arbdcr > amgudt and arbtcr <> 'P'
104900071203     c                             and vfri = 'C'
105000071130     c                   move      amgudt        dadata
105100071130     c                   move      arbdcr        adata
105200071130     c                   CALL      'XSRLAV8'
105300071130     c                   PARM                    Wdata8
105400071130      * se il risultato è maggiore di 1 vuol dire che la dcr deve essere
105500071130      * modificata se si vuole gestire la bolla in consegna
105600071130     c                   if        giolav > 1
105700071130     c                   move      'X'           contr_dcr         1
105800071130     C                   MOVEL     '1'           $RIEMI
105900071130     c                   seton                                        19
106000071130     C                   MOVEL     ERR(66)       $MSG
106100071130     C                   GOTO      ENDCT2
106200071130     c                   endif
106300071130     c                   endif
106400940310      * CONTROLLO COD FINE GIAC.
106500940308     C                   MOVEL     '2C'          TBLCOD
106600940308     C                   MOVEL     *BLANK        TBLKEY
106700940308     C                   MOVEL     VCFG          TBLKEY
106800940310     C     KTAB          CHAIN     TABEL                              21
106900940308     C   21              MOVEL     *ALL'?'       DCFG
107000060410     C  N21              MOVEL     TBLUNI        Ds2c
107100060410     C  N21              MOVEL     §2cdes        DCFG
107200060801      * x non trovato o codice obsoleto segnalo errore
107300060801     c                   if        *in21 or §2cobs <> *blank
107400060801     C                   SETON                                        1922
107500060801     c                   endif
107600940308     C   22              MOVEL     ERR(17)       $MSG
107700940308     C                   END
107800940308     C* DESTINATARIO
107900940308     C     *IN46         IFEQ      '1'
108000940308     C     VRSD          IFEQ      *BLANK
108100940308     C     VIND          OREQ      *BLANK
108200940308     C     VPRD          OREQ      *BLANK
108300961205     C     VNZD          ANDEQ     *BLANK
108400940308     C     VLOD          OREQ      *BLANK
108500940309     C                   SETON                                        561965
108600940308     C                   MOVEL     ERR(19)       $MSG
108700940308     C                   END
108800940308     C                   END
108900940308     C* CAP A ZERO: INTERROGAZIONE SU CAPPARIO
109000941006     C     VCAD          IFEQ      *BLANK
109100940309     C     *IN46         ANDEQ     '1'
109200970711     C                   CLEAR                   TISI00
109300970711     C                   MOVEL     'L01'         D00OP0
109400970711     C                   MOVEL     VLOD          D00LOC
109500980219     C                   MOVEL     AMGUDT        D00DRI
109600970711     C                   CALL      'TISI30R'
109700970711     C                   PARM                    KPJBA
109800970711     C                   PARM                    TISI00
109900940308     C*
110000970711     C     D00CAP        IFNE      *BLANKS
110100970711     C                   MOVEL     D00CAP        VCAD
110200970711     C                   MOVEL     D00LOC        VLOD
110300970711     C                   MOVEL     D00PRV        VPRD
110400970711     C                   MOVEL     D00NAR        VNZD
110500940308     C                   END
110600940308     C*
110700940309     C                   MOVEL     '1'           $RIEMI
110800940308     C                   GOTO      ENDCT2
110900940308     C                   END
111000940930     C* CONTROLLO CAP
111100940308     C  N19*IN46         IFEQ      '1'
111200970710     C* Imposto parametri per routine controllo Cap
111300970709     C                   CLEAR                   TISI95
111400970710     C                   CLEAR                   FNLV13
111500970709     C                   MOVEL     '7'           I95TCN
111600970709     C                   MOVEL     *BLANK        I95TLA
111700970709     C                   MOVEL     VNZD          I95NAR
111800970710     C                   MOVEL     VIND          I95IND
111900970709     C                   MOVEL     VCAD          I95CAP
112000970709     C                   MOVEL     VLOD          I95LOC
112100970709     C                   MOVEL     VPRD          I95PRV
112200970710     C                   MOVEL     ARBPKF        I95LKG
112300970710     C                   MOVEL     ARBVLF        I95LMC
112400970709     C                   MOVEL     ARBFFD        I95FFD
112500970710     C                   MOVEL     ARBTSP        I95TSP
112600971202     C                   Z-ADD     1             §§                3 0
112700971202     C* CONTROLLO PORTO DELLA BOLLA ORIGINALE
112800971202     C     ARBCBO        LOOKUP    CDB(§§)                                04
112900971202     C                   MOVEL     §3ATB1        WPOR1             1
113000971202     C                   MOVEL     §3ATB2        WPOR2             1
113100971202     C     WPOR1         IFEQ      'A'
113200971202     C     §3ATB1        ANDNE     'AS'
113300971202     C     WPOR2         OREQ      'A'
113400971202     C     §3ATB2        ANDNE     'AS'
113500971202     C     §3ATB1        OREQ      'FS'
113600971202     C                   MOVEL     'A'           I95TPO
113700971202     C                   ELSE
113800971202     C                   MOVEL     'F'           I95TPO
113900971202     C                   END
114000971202     C* CONTROLLO SE LA BOLLA ORIGINALE AVEVA IL C/ASSEGNO
114100971202     C     §3AFCA        IFNE      0
114200971202     C                   MOVEL     'S'           I95FCA
114300971202     C                   ELSE
114400971202     C                   MOVEL     ' '           I95FCA
114500971202     C                   END
114600041018     C* passiamo le consegne particolari al TISI95 se dispos. non sono reso
114700041018     c     vfri          ifne      'R'
114800040929     C                   MOVEL     ARBtc1        I95fi2
114900040929     C                   MOVE      ARBtc2        I95fi2
115000041018     c                   else
115100041018     c                   clear                   I95fi2
115200041018     c                   endif
115300121107     c* .... Forzatura da tabella "FLB/FLA"
115400121107     c* Forzatura linea di arrivo per  linea di p.
115500121107      * gestito in questo modo parrebbe funzionare solo per le linee
115600121107      * estere perchè per l'italia proseguendo il giro torna fare il giro del
115700121107      * cappario e da errore
115800121109     C*?SPOSTATO IN CTRIND per provare a forzare
115900121109     c*m                 if        vfri = 'C'
116000121109     c*m                 clear                   trulflbds
116100121109     c*m                 movel     Vlna          iflblna
116200121109     c*m                 move      arblnp        Iflblnp
116300121109     c*m                 call      'TRULFLBR'
116400121109     c*m                 parm                    TRULFLBDS
116500121109    2c*m                 if        Oflblfz>*zeros
116600121109     c*m                 move      OflbLFZ       vlna
116700121109    2c*m                 endif
116800121109    2c*m                 endif
116900120330     c*********
117000120330     c***** Era stato fatto x Ducros come Partner contemporaneo a Sernam sulla
117100120330     c***   Francia. Una volta tolta Sernam come Partner e diventata Ducros
117200120330      ***   Partner Francese unico su linea 330 (anzichè 333) questo chiodo
117300120330     c***** deve essere rimosso.
117400120330     c*********
117500120124     c* CHIODO TEMPORANEO FORZATURA LINEA 330
117600120330     c**** vfri          ifEQ      'C'
117700120330     c**** ARBLNa        ANDEQ     333
117800120330     C********           Z-ADD     ARBLNa        VLNA
117900120330     c*********          ENDIF
118000971202     C*
118100970710     C                   MOVEL     'S'           I13AF0
118200970710     C                   MOVEL     'S'           I13AF1
118300970710     C                   MOVEL     'S'           I13SZ2
118400970710     C                   MOVEL     'S'           I13LA3
118500970710     C                   MOVEL     'S'           I13SZ3
118600970710     C                   MOVEL     'S'           I13CNV
118700970710     C                   MOVEL     'S'           I13SZV
118800970710     C* Gestione errori forzabili: se cambiato CAP/Località
118900970710     C* riazzero i valori di errori forzabili
119000970710    2C     VCAD          IFNE      WSVCAP
119100970710     C                   CLEAR                   WFZV
119200970710     C                   CLEAR                   WFZ1
119300970710     C                   CLEAR                   WFZ2
119400970710     C                   CLEAR                   WFZ3
119500970710     C                   MOVEL     VCAD          WSVCAP
119600970710    2C                   ENDIF
119700970710     C* Modifica località
119800970710    2C     VLOD          IFNE      WSVLOC
119900970710     C                   MOVEL     VLOD          WSVLOC
120000970710     C                   CLEAR                   WFZ1
120100970710     C                   CLEAR                   WFZ2
120200970710     C                   CLEAR                   WFZ3
120300970710    2C                   ENDIF
120400970710     C* Modifica PROVINCIA
120500970710    2C     VPRD          IFNE      WSVPRO
120600970710     C                   MOVEL     VPRD          WSVPRO
120700970710     C                   CLEAR                   WFZ3
120800970710    2C                   ENDIF
120900970710     C* Se il livello di affidabilità non è inferiore a 2 e
121000970710     C** CALL
121100970710     C                   MOVEL     WFZV          E13FZV
121200970710     C                   MOVEL     WFZ1          E13FZ1
121300970710     C                   MOVEL     WFZ2          E13FZ2
121400970710     C                   MOVEL     WFZ3          E13FZ3
121500970710     C                   CALL      'FNLV13R'
121600970710     C                   PARM                    KPJBA
121700970710     C                   PARM                    FNLV13
121800970710     C                   PARM                    TISI95
121900970710     C* Ricontrollo errori
122000970710     C                   MOVEL     O13ERR        WERR
122100970710     C                   MOVEL     O13MSG        WMSG
122200970710     C                   MOVEL     E13FZV        WFZV
122300970710     C                   MOVEL     E13FZ1        WFZ1
122400970710     C                   MOVEL     E13FZ2        WFZ2
122500970710     C                   MOVEL     E13FZ3        WFZ3
122600970710     C* reimposto dati da TISI95
122700970710     C     O13ROC        IFEQ      'S'
122800970710     C     O13RON        OREQ      'S'
122900970710     C     O13ROP        OREQ      'S'
123000970710     C     O13ROL        OREQ      'S'
123100970710     C                   MOVEL     O95LOC        VLOD
123200970710     C                   MOVEL     O95PRV        VPRD
123300970710     C                   MOVEL     O95NAR        VNZD
123400970710     C                   MOVEL     O95CAP        VCAD
123500970710     C                   END
123600970710     C     O95ERR        IFEQ      *BLANKS
123700040423      * se solo cambio di porto non varia la zona per la nuova bolla
123800040423     c                   if        vfri = 'C' and export <> vnpc
123900040423     c                   move      *blank        §lpznc
124000070326     c                   move      *blank        §lpcgi
124100040423     c                   else
124200070904     C  n55              MOVEL     O95ZNC        §LPZNC
124300040423     c                   end
124400970710     C                   MOVEL     O95LNA        §LNA
124500000313     C* se ex lna DPD e sto eseguendo una riconsegna non reiposto la
124600000313     C* lna in base al Cappario ma rimane uguale alla vecchia
124700000313     C     VFRI          IFEQ      'C'
124800000313     C     ARBLNA        CHAIN     AZORG01L                           21
124900000313     C     *IN21         IFEQ      '0'
125000000313     C     ORGFVA        ANDNE     '*'
125100000313     C                   MOVEL     ORGDE3        OG143
125200020225     C     §OGNTW        IFEQ      'DPD'
125300020225     C     §OGNTW        OREQ      'FED'
125400000313     C                   MOVEL     ARBLNA        §LNA
125500000313     C                   END
125600000313     C                   END
125700000313     C                   END
125800000313     C                   END
125900060103      * richiamo pgm x verificare che non sia un destinatario particolare
126000060103      * il lancio viene effettuato per C e D clienti non disk b
126100060103      * e per i resi se non c'è stata già la forzatura della linea arrivo
126200091015     c                   if        vlna = 0
126300060103     c                   clear                   des_par           1
126400071025     c                   clear                   des_parg          1
126500091015     c                   endif
126600091015
126700170321     c**                 if        vfri = 'C' and arbnrs = 0 or
126800170321     c**                           vfri = 'D' and arbnrs = 0 or
126900170322     c**                           vfri = 'R' and vlna = 0
127000170321     c                   if        vfri = 'C' or
127100170321     c                             vfri = 'D' or
127200170322     c                             vfri = 'R' and §lplna = 0
127300060103     c                   clear                   tisit0ds
127400060104     c                   move      i95loc        IT0loc
127500060104     c                   move      i95prv        IT0prv
127600060104     c                   move      i95nar        IT0naz
127700060104     c                   move      i95cap        IT0cap
127800060104     c                   move      i95ind        IT0ind
127900060103     C                   movel     VRSD          IT0rag
128000071008     c                   clear                   tisit0x1ds
128100060103     c                   call      'TISIT5R'
128200060103     c                   parm                    tisit0ds
128300071008     c                   parm                    tisit0x1ds
128400060103      * se non c'è errore risulta trovato quindi imposta flag per bypassare
128500060103      * i controlli sulla linea che viene forzata dalla ds di ritorno
128600060103    2c     ot0err        ifeq      *blanks
128700071025     c                   move      'X'           des_parg
128800060103     c                   movel     ot0flo        ddstflo
128900060404     c                   if        §dstlna  <> *blank
129000170321     c*                  move      §DSTLNA       vlna
129100170321     c                   move      §DSTLNA       §lna
129200071025     c                   move      §DSTZNC       §lpznc
129300060404     c                   move      'X'           des_par
129400060404     c                   endif
129500060103     c                   endif
129600060103     c                   endif
129700970710     C*
129800970710     C                   EXSR      CTRIND
129900970710     C                   END
130000971113     C*
130100970710     C   19              GOTO      ENDCT2
130200971113     C*
130300040531      * verifica dirottamenti
130400040531      * non è ammesso il dirottamento se il cap o la località sono
130500040531      * comunque serviti dalla linea d'arrivo indipendentemente da
130600040531      * limiti di peso o altro
130700040719      * sono escluse le eccezioni della seconda condizione
130800040607     C  N19*IN46         IFEQ      '1'
130900040607     C     VFRI          ANDEQ     'D'
131000060103     C     des_par       ANDEQ     *blank
131100170323      *reperisce eventuali eccezioni organigramma per forzatura dei dirottamenti
131200170323     c                   exsr      SRog150
131300040719      *verifico se periodo con dirottamento permesso causa limitazioni traffico
131400040719     c                   if        amgudt < §2g2dpi or amgudt > §2g2dpf
131500040719
131600040531     c                   if        flgest <> 'E' and vlna <> 101 and
131700040601     c                             vlna <> 102 and arbffd <> 'S'
131800170323     c                             and arbfbr <> 'S' and
131900170323     c                             §OGESBLDI <> 'S'
132000040531     c                   if        exlna <> vlna
132100040531     c                   if        o95lla > 0
132200040531     c                   if        exlna = o95lla or exlna = o95llo or
132300040531     c                             exlna = o95lls
132400040531     c                   seton                                        1949
132500040531     C                   movel     err(65)       $msg
132600040531     C                   GOTO      ENDCT2
132700040531     c                   endif
132800040531     c                   else
132900040531     c                   if        exlna = o95cla or exlna = o95clo or
133000040531     c                             exlna = o95cls
133100040531     c                   seton                                        1949
133200040531     C                   movel     err(65)       $msg
133300040531     C                   GOTO      ENDCT2
133400040531     c                   endif
133500040531     c                   endif
133600040531     c                   endif
133700040531     c                   endif
133800040719     c                   endif
133900040607     c                   ENDIF
134000971113     C* Controllo se la lna esiste
134100071126      * verifico se la filiale è gestita in £6 da qualcuno nel caso
134200071126      * i riferimenti di azorg x le ds og146/148 sono quelli della capofila
134300071126      **
134400071126     c                   clear                   Og_da_£6          1
134500071126     c                   exsr      capofila
134600071126      *
134700971113     C                   MOVEL     *BLANK        VDLNA
134800971113     C     VLNA          CHAIN     AZORG01L                           21
134900971113     C     *IN21         IFEQ      '0'
135000971113     C     ORGFVA        COMP      '*'                                    21
135100971113     C                   MOVEL     ORGDES        VDLNA
135200071126      * se gestita da altri le ds vengono impostate con i campi valorizzati
135300071126      * nella routine capofila
135400071126     c                   if        Og_da_£6 = *blank
135500051221     C                   MOVEL     orgde8        og148
135600070416     C                   MOVEL     orgde6        og146
135700071126     c                   endif
135800070424     c                   if        §ogdda <> *blank
135900070424     c                   move      §ogdda        ogddan            8 0
136000070424     c                   endif
136100051221      *---
136200051221      *  Linea Arrivo non utilizzabile in Bollettazione
136300051221      *     es.: x DPD  le nuove linee di Backup sono settate così.
136400051221     C     §OGLBL        IFEQ      'N'
136500051221     C                   SETON                                        1949
136600051221     C                   MOVEL     ERR(21)       $MSG
136700051221    2C                   ENDIF
136800051221      *---
136900971113     C* Controllo se la lna è congruente con il sif.in cui sono
137000971113     C                   ELSE
137100971113     C                   MOVEL     *ALL'?'       VDLNA
137200971113     C                   SETON                                        1949
137300971113     C                   MOVEL     ERR(21)       $MSG
137400971113     C                   END
137500940308     C* CMD 09 X FORZARE LINEA ARRIVO
137600940309      *------------------------------
137700940309     C   KI              MOVEL     '1'           $FOLNA
137800940308     C*
137900170323      *Se filiale esente blocco dirottamento non effettua questo controllo
138000170323     c                   if        §ogesbldi <> 'S'
138100940309     C   46
138200940309     CANN19$FOLNA        IFEQ      '0'
138300940316     C     VLNA          IFNE      *ZEROS
138400940308     C     §LNA          COMP      VLNA                               2121
138500940309     C   21              SETON                                        194959
138600940308     C   21              MOVEL     ERR(22)       $MSG
138700940316     C                   END
138800940308     C                   END
138900170323     c                   endif
139000940309     C* LINEA ARRIVO
139100940308     C*  SE HO FORZATO LA LINEA LA ZONA = 0
139200940309     C   46$FOLNA        IFEQ      '1'
139300940308     C     §LNA          COMP      VLNA                               2121
139400070904     c  n55
139500070904     Can 21              MOVE      *ZEROS        §LPZNC               1949
139600940308     C                   END
139700070509     c* verifico se per la linea di arrivo che si sta impostando è presente
139800070509      * un giro predefinito per assegnarlo solo in caso di RESO e se esiste
139900070509      * un LUOGO 300 legato al codice cliente
140000070510     c                   if        vfri = 'R' and vlna <> §lplna
140100070510     c                   clear                   §lpcgi
140200070510     c                   exsr      ecceluogo2
140300070510     c                   endif
140400941003     C* CONTROLLO NAZIONE
140500941217     C  N19
140600941217     CAN 46              DO
140700941003     C                   MOVEL     '15'          TBLCOD
140800941003     C                   MOVEL     *BLANK        TBLKEY
140900941003     C                   MOVEL     VNZD          TBLKEY
141000941003     C     KTAB          CHAIN     TABEL                              21
141100941003     C   21              MOVEL     *ALL'?'       §15DES
141200941003     C   21              SETON                                        6619
141300941003     C   21              MOVEL     ERR(40)       $MSG
141400941003     C  N21              MOVEL     TBLUNI        DS15
141500941003     C                   END
141600941217     C* CONTROLLO TIPO INCASSO CONTASSEGNO SE GIA EMESSO IL CAMPO
141700941217     C     *IN34         IFEQ      *ON
141800941217     C* RICERCA
141900941217     C     '?'           SCAN      VTIC                                   21
142000941217     C*
142100941217     C     *IN21         IFEQ      *ON
142200941217     C                   MOVEL     '1A'          §COD
142300941217     C                   MOVE      *BLANKS       VTIC
142400941217     C                   CALL      'X§TABER'
142500941217     C                   PARM                    CODUT
142600941217     C                   PARM                    §COD
142700941217     C                   PARM                    §KEY
142800941217     C                   PARM                    §DES
142900941217     C                   MOVEL     §KEY          VTIC
143000941221     C                   MOVEL     '1'           $RIEMI
143100941217     C                   GOTO      ENDCT2
143200941217     C                   ENDIF
143300941217     C                   ENDIF
143400941217     C* SE HANNO FATTO VARIAZIONI SUL C/ASSEGNO  CONTROLLO
143500941217     C* IL TIPO INCASSO C/ASSEGNO E LA CONGRUENZA CON LA DIVISA
143600941217     C     VVCS          IFEQ      'S'
143700020417     C     VASS          ANDNE     0
143800941217     C                   MOVEL     '1A'          TBLCOD
143900941217     C                   MOVEL(P)  VTIC          TBLKEY
144000941217     C     KTAB          CHAIN     TABEL                              21
144100941217     C   21              SETON                                        6419
144200941217     C   21              MOVEL     ERR(43)       $MSG
144300941217     C  N21              MOVEL     TBLUNI        DS1A
144400941222     C     VVCS          IFEQ      'S'
144500941222     C     VASS          ANDEQ     0
144600941222     C     *IN34         ANDEQ     *OFF
144700941222     C                   SETON                                        34
144800941222     C                   MOVEL     '1'           $RIEMI
144900941222     C                   GOTO      ENDCT2
145000941222     C                   ENDIF
145100941217     C* TIPO INCASSO ERRATO
145200941217     C     *IN64         IFEQ      *ON
145300941222     C* SE HO AGGIUNTO IL C/ASSEGNO  CHIEDO IL TIPO INCASSO
145400941217     C* SE DIVISA ESTERA VEDO SE POSSO ACCETTARE IL TIPO INCASSO
145500020227     C     §CVDVA        ORNE      'S'
145600941217     C     §1AUDE        ANDNE     'S'
145700941217     C* SE IL TIPO INCASSO NON è CONGRUENTE EMETTO A VIDEO IL CAMPO
145800941217     C* PER POTERLO CORREGGERE
145900941217     C     *IN34         IFEQ      *OFF
146000941217     C                   SETON                                        34
146100941217     C                   MOVEL     '1'           $RIEMI
146200941217     C                   GOTO      ENDCT2
146300941217     C                   ELSE
146400941217     C* ALTRIMENTI SE L'HO GIA' EMESSO DO ERRORE
146500941217     C                   SETON                                        19
146600941217     C                   MOVEL     ERR(43)       $MSG
146700941217     C                   GOTO      ENDCT2
146800941217     C                   ENDIF
146900941217     C                   ENDIF
147000941217     C                   ENDIF
147100941217     C*
147200940308     C* SE RICHIAMO PGM TASSAZ. LA LINEA ARRIVO DEVE ESSERE =
147300940308     C  N19
147400940308     CAN 87VSIFAT        IFEQ      'S'
147500941216     C     VLNA          ANDNE     EXLNA
147600940308     C                   SETON                                        1962
147700940308     C                   MOVEL     ERR(18)       $MSG
147800940308     C                   END
147900971006      *-------------------------------------------------------
148000971006     C* Recupero codice cliente mittente
148100971006     C     EXPORT        IFEQ      'A'
148200971006     C     WEXTB1        ANDNE     'AS'
148300971006     C                   MOVE      ARBCCM        WVARIO            4 0
148400971006     C                   ELSE
148500971006     C                   MOVE      ARBKSC        WVARIO
148600971006     C                   END
148700941215     C* CMD 08 X FORZARE DIROTTAMENTO O RICONSEGNA IN BASE LNA
148800941215      *-------------------------------------------------------
148900940309     C   KH              DO
149000971006     C* Dirottamento non forzabile se provengo da giacenza
149100971006     C* e il mittente non è codificato
149200971006     C     WVARIO        IFNE      0
149300971006     C     WVARIO        ANDNE     8888
149400971006     C     WVARIO        ANDNE     9999
149500980505     C     VFRI          OREQ      'D'
149600971006     C     $GIAC         ORNE      '1'
149700060228     C     des_par       ORNE      *blank
149800940309     C                   MOVEL     '1'           $FOFRI
149900940309     C                   MOVEL     VRSD          SAVRSD
150000941011     C                   MOVEL     VRD2          SAVRD2
150100940309     C                   MOVEL     VIND          SAVIND
150200940309     C                   MOVEL     VLOD          SAVLOD
150300940309     C                   MOVEL     VPRD          SAVPRD
150400940309     C                   MOVE      VCAD          SAVCAD
150500940309     C                   MOVE      VLNA          SAVLNA
150600961203     C                   MOVEL     VNZD          SAVNZD
150700031119     C                   MOVEL     Vteld         SAVteld
150800031119     C                   MOVEL     Vref          SAVref
150900940309     C                   END
151000971006     C                   END
151100940308      * SE LINEA ARRIVO DIVERSA  E DISPOSIZIONI SONO DI RICONSEGNARE
151200940308      * SEGNALO CHE LE DISPOSIZ. SARANNO CAMBIATE IN DIROTTAM.
151300940309     C     $FOFRI        IFEQ      '0'
151400941229     C     VLNA          ANDNE     *ZEROS
151500941216     C     VLNA          ANDNE     EXLNA
151600940308     C     VFRI          ANDEQ     'C'
151700971006     C* Se devo forzare disposizioni di dirottamento nel caso
151800971006     C* di spedizioni con codice cliente mittente non codif.
151900971006     C* su una bolla giacente do' errore perchè le spedizioni
152000971006     C* non possono essere immesse in arrivo
152100971006     C     WVARIO        IFNE      0
152200971006     C     WVARIO        ANDNE     8888
152300971006     C     WVARIO        ANDNE     9999
152400971006     C     $GIAC         ORNE      '1'
152500060228     C     des_par       ORNE      *blank
152600940308     C                   SETON                                        19
152700940308     C                   MOVEL     ERR(29)       $MSG
152800971006     C                   ELSE
152900971006     C                   SETON                                        19
153000971006     C                   MOVEL     ERR(56)       $MSG
153100940308     C                   END
153200971006     C                   END
153300940308     C*
153400940308     C   19              GOTO      ENDCT2
153500941215      * SE LINEA ARRIVO UGUALE MA  DISPOSIZIONI SONO DI DIROTTARE
153600941215      * SEGNALO CHE LE DISPOSIZ. SARANNO CAMBIATE IN RICONSEGNA
153700961128     C     $FOFRI        IFEQ      '0'
153800961128     C     VLNA          ANDEQ     EXLNA
153900941215     C     VFRI          ANDEQ     'D'
154000941215     C                   SETON                                        19
154100941215     C                   MOVEL     ERR(42)       $MSG
154200941215     C                   END
154300961128     C   19              GOTO      ENDCT2
154400941215     C*
154500941215     C   19              GOTO      ENDCT2
154600940308     C* VARIAZIONE INDIRIZZO X RICONSEGNA NO NUOVA BOLLA
154700940308     C  N31VFRI          IFEQ      'C'
154800940308     C     VRSD          IFNE      ARBRSD
154900941011     C     VRD2          ORNE      AR4RD2
155000940308     C     VIND          ORNE      ARBIND
155100940308     C     VLOD          ORNE      ARBLOD
155200940308     C     VPRD          ORNE      ARBPRD
155300940308     C     VCAD          ORNE      ARBCAD
155400940930     C     VNZD          ORNE      ARBNZD
155500060314     C     Vref          ORNE      §AR5REF
155600060317     C     flgdis        andne     'F'
155700060314     C     Vteld         ORNE      §AR5TELD
155800940308     C                   SETON                                        81
155900940308     C                   END
156000940308     C                   END
156100940310     C* DIROTTAMENTO 2 - OPERAZ. DA FARE IN BASE AI DATI IMPOSTATI
156200940310     C     VFRI          IFEQ      'D'
156300940310     C                   EXSR      DIROT2
156400940310     C   19              GOTO      ENDCT2
156500940310     C                   END
156600940308     C     ENDCT2        ENDSR
156700970710     C*------------------------------------------------------*
156800970710     C*  Controllo dati indirizzo
156900970710     C*------------------------------------------------------*
157000970710     C     CTRIND        BEGSR
157100970710     C*
157200970710     C* Errore !!!!
157300970710     C     WERR          IFNE      ' '
157400970710     C     WMSG          ANDNE     *BLANKS
157500970710     C* Manca la Ragione sociale
157600970710     C                   SELECT
157700970710     C* Errore su località/città
157800970710     C     WERR          WHENEQ    '3'
157900970710     C                   SETON                                        5619
158000970710     C* Errore su Cap
158100970710     C     WERR          WHENEQ    '5'
158200970710     C                   SETON                                        5919
158300970710     C* Errore su provincia
158400970710     C     WERR          WHENEQ    '4'
158500970710     C                   SETON                                        6519
158600970710     C* Nazione  errata
158700970710     C     WERR          WHENEQ    '6'
158800970710     C                   SETON                                        6619
158900970710    2C                   ENDSL
159000970710     C                   MOVEL     WMSG          $MSG
159100970710     C                   END
159200970710     C* Se non ci sono errori proseguo
159300970710     C   19              GOTO      FININD
159400970710     C*
159500970710     C* Controllo se reimpostata località
159600970710     C     O13ROL        IFEQ      'S'
159700970710     C                   SETON                                        5619
159800970710     C                   MOVEL     ERR(52)       $MSG
159900970710     C                   GOTO      FININD
160000970710     C                   END
160100970710     C* Controllo se reimpostata Provincia
160200970710     C     O13ROP        IFEQ      'S'
160300970710     C                   SETON                                        6519
160400970710     C                   MOVEL     ERR(54)       $MSG
160500970710     C                   GOTO      FININD
160600970710     C                   END
160700970710     C* Controllo se reimpostato CAP
160800970710     C     O13ROC        IFEQ      'S'
160900970710     C                   SETON                                        5919
161000970710     C                   MOVEL     ERR(53)       $MSG
161100970710     C                   GOTO      FININD
161200970710     C                   END
161300970710     C* Controllo se reimpostata Nazione
161400970710     C     O13RON        IFEQ      'S'
161500970710     C                   SETON                                        6619
161600970710     C                   MOVEL     ERR(55)       $MSG
161700970710     C                   GOTO      FININD
161800970710     C                   END
161900970710     C*
162000970710     C  N19VLNA          IFEQ      0
162100970710     C                   MOVE      §LNA          VLNA
162200970710     C                   END
162300121112     c                   if        flgest  = 'E' and
162400121112     c                             (vfri = 'C' or vfri = 'D')
162500141117     c                   if        (vnzd = arbnzd or arbnrs <> 0)
162600141117     c                             and arblna <> 330
162700121112     c                   move      arblna        vlna
162800121112     c                   else
162900121109     c                   clear                   trulflbds
163000121109     c                   movel     Vlna          iflblna
163100121109     c                   move      arblnp        Iflblnp
163200121109     c                   call      'TRULFLBR'
163300121109     c                   parm                    TRULFLBDS
163400121109    2c                   if        Oflblfz>*zeros
163500121109     c                   move      OflbLFZ       vlna
163600121109    2c                   endif
163700121112    2c                   endif
163800121109    2c                   endif
163900970710     C*
164000970710     C     FININD        ENDSR
164100940310     C* ------------------------------------------------------
164200940310     C* D I R O T 2  - OPERAZIONI DA FARE X DIROTTAM
164300940310     C* ------------------------------------------------------
164400940310     C     DIROT2        BEGSR
164500941217     C*DIROTTO SEMPRE AD ALTRA LINEA ARRIVO
164600941217     C* -----------------------------------
164700941010     C                   EXSR      DIRALA
164800941217     C* -----------------> TRATTO LA VECCHIA BOLLA
164900941217     C*
165000941217     C* SE ERA ASSEGNATO NON CONTAB.--> TRASFORMO IN CONTO SERVIZIO
165100940310     C  N26
165200940310     CANN27EXPORT        IFEQ      'A'
165300950726     C     WEXTB1        ANDNE     'AS'
165400940310     C                   MOVEL     §CDVAR        §§KCVB
165500940310     C                   SETON                                        80
165600940310     C                   MOVEL     'AS'          NWTB1
165700940310     C                   SETON                                        39
165800940310     C                   END
165900941217     C* SE ERA FRANCO E NUOVO PORTO =A --> TRASFORMO IN CONTO SERVIZIO
166000940310     C  N26
166100940310     CANN27EXPORT        IFEQ      'F'
166200950727     C     WEXTB1        ANDNE     'FS'
166300940310     C     VNPC          ANDEQ     'A'
166400940310     C                   MOVEL     §CDVAR        §§KCVB
166500940310     C                   SETON                                        80
166600940310     C                   MOVEL     'FS'          NWTB1
166700940310     C                   SETON                                        39
166800940310     C                   END
166900940310     C*CERCO NUOVO COD. BOLLA
167000940310     C  N26
167100940310     CANN27*IN80         IFEQ      '1'
167200940310     C                   MOVEL     *BLANK        NWCBL
167300940310     C                   MOVEL     *BLANK        §BKCBN
167400940310     C                   EXSR      NEWCBL
167500940310     C  N30              MOVEL     NWCBL         §BKCBN
167600940310     C                   END
167700940310     C* RICH. NOTA ACCREDITO
167800940310     C     *IN26         IFEQ      '1'
167900991001     C     AR6TRC        ANDEQ     '1'
168000991001     C     AR6FIV        ANDEQ     ARBLNA
168100950726     C     ARBICA        ANDNE     'Y'
168200940310     C                   SETON                                        32
168300940930     C                   Z-ADD     AR6IMV        §INAC1
168400940310     C                   END
168500940310     C     *IN27         IFEQ      '1'
168600991001     C     AR6TRC        ANDEQ     '2'
168700950726     C     ARBICA        ANDNE     'Y'
168800940310     C                   SETON                                        37
168900991001     C                   Z-ADD     AR6IMV        §INAC2
169000940310     C                   END
169100941217     C* -----------------> TRATTO LA NUOVA BOLLA
169200940310     C                   SETON                                        31
169300940310     C                   MOVEL     *BLANK        NWTB2
169400940310     C* NUOVO PORTO = F O ERA GIA FRANCO
169500940310     C     VNPC          IFEQ      'F'
169600940310     C     VNPC          OREQ      ' '
169700940310     C     EXPORT        ANDEQ     'F'
169800941010      *
169900940310     C                   MOVEL     'F7'          NWTB1
170000940310     C* SE C'E' C/ASSEGNO
170100940310     C     VNCA          IFGT      0
170200940310     C     VVCS          OREQ      'N'
170300940310     C     VASS          ANDGT     0
170400940310     C*  E C'ERA LA 2°BOLLA E NON HO VARIATO C/ASS. E NON PAGA "M"
170500940310     C*   O DICO CHE IL DEST. PAGA PROVV.C/A
170600940310     C     §3ATB2        IFEQ      'A3'
170700940310     C     VVCS          ANDEQ     'N'
170800940310     C     VPPC          ANDNE     'M'
170900940310     C     VPPC          OREQ      'D'
171000940310     C                   MOVEL     'A3'          NWTB2
171100940310     C                   END
171200940310     C                   END
171300940310     C                   END
171400940310     C* NUOVO PORTO = A O ERA GIA ASSEGNATO
171500940310     C     VNPC          IFEQ      'A'
171600940310     C     VNPC          OREQ      ' '
171700940310     C     EXPORT        ANDEQ     'A'
171800940310     C                   MOVEL     'A2'          NWTB1
171900940310     C                   MOVEL     '  '          NWTB2
172000940310     C                   END
172100940310     C* CERCO NUOVO CODICE BOLLA
172200940310     C     VNCA          IFGT      0
172300940310     C     VVCS          OREQ      'N'
172400940310     C     VASS          ANDGT     0
172500940310     C     NWTB2         IFEQ      *BLANK
172600940310     C                   MOVEL     '1'           NWFCA
172700940310     C                   ELSE
172800940310     C                   MOVEL     '2'           NWFCA
172900940310     C                   END
173000940310     C                   ELSE
173100940310     C                   MOVEL     '0'           NWFCA
173200940310     C                   END
173300940310     C                   MOVEL     *BLANK        NWCBL
173400940310     C                   MOVEL     '0'           NWFTD
173500941003     C                   MOVEL     *BLANK        §LPCBO
173600940310     C                   EXSR      NEWCBL
173700030528     c* per bolle FW (servizio c/a) se dirottamento forzo sempre lo stesso
173800030528     c* codice bolla
173900030528     c                   if        arbcbo = 'FW'
174000030528     c                   movel     arbcbo        nwcbl
174100030528     c                   setoff                                       30
174200030528     c                   end
174300941003     C  N30              MOVEL     NWCBL         §LPCBO
174400011130     C     §LPCBO        IFEQ      'A '
174500011130     C     ARBCBO        ANDNE     'A '
174600011130     C                   MOVEL     '$'           §LPCBO
174700011130     C                   END
174800941217     C* SPOSTO C/A SU NUOVA BOLLA
174900941217     C     NWFCA         IFEQ      '1'
175000940310     C     NWFCA         OREQ      '2'
175100940930     C     VASS          IFGT      *ZEROS
175200940310     C     VVCS          ANDEQ     'N'
175300941003     C                   Z-ADD     AR9CAS        §LPCAS
175400941214     C                   MOVEL     AR9VCA        §LPVCA
175500941003     C                   MOVEL     AR9TIC        §LPTIC
175600940310     C                   END
175700940310     C     VNCA          IFGT      *ZEROS
175800941003     C                   Z-ADD     VNCA          §LPCAS
175900941003     C                   MOVEL     VVCA          §LPVCA
176000941217     C                   MOVE      VTIC          §LPTIC
176100940310     C                   END
176200960125     C                   ELSE
176300960125     C                   Z-ADD     0             §LPCAS
176400960125     C                   MOVEL     *BLANKS       §LPVCA
176500960125     C                   MOVEL     *BLANKS       §LPTIC
176600940310     C                   END
176700940310     C*  LP
176800941217     C                   MOVE      EXLNA         §LPLNP
176900940310     C* IMPOSTO §LPNOT
177000940310     C                   EXSR      XLPNOT
177100940310     C     ENDDI2        ENDSR
177200941010     C* ----------------------------------------------------
177300941010     C* DIROTTAMENTO ALTRA  LINEA ARRIVO
177400941010     C* ---------------------------------------------------
177500941010     C     DIRALA        BEGSR
177600960125     C*
177700960125     C*  SE LA NUOVA LINEA DI ARRIVO E' UGUALE A 101 AZZERO
177800960125     C*  C/ASSEGNO/ANTICIPATA/SE PORTO ASSEGNATO --> FRANCO
177900960125     C     *INKF         IFEQ      '1'
178000960125     C     VLNA          ANDEQ     101
178100960227     C     *INKF         OREQ      '1'
178200960227     C     VLNA          ANDEQ     102
178300960125     C     VASS          IFNE      0
178400960125     C                   MOVEL     'S'           VVCS
178500960125     C                   Z-ADD     0             VNCA
178600960125     C                   END
178700960125     C     EXPORT        IFEQ      'A'
178800960125     C                   MOVEL     'F'           VNPC
178900960125     C                   END
179000960125     C                   END
179100941010     C*  DEVO TOGLIERE C/A DALLA VECCHIA BOLLA E METTERLO SULLA NUOVA
179200941010     C     VASS          IFGT      0
179300941215     C                   MOVE      *ZEROS        §BKCAS
179400941219     C                   MOVE      *BLANK        §BKVCA
179500941010     C                   MOVEL     'KB'          §CDVAR
179600941010     C                   END
179700941010     C     §CDVAR        IFEQ      'KB'
179800941010     C                   EXSR      CNTVAR
179900941010     C  N26
180000941010     CANN27              MOVEL     §CDVAR        §§KCVB
180100941010     C  N26
180200941010     CANN27              SETON                                        80
180300941010     C                   END
180400941010     C* GUARDO SE DEVO VARIARE TIPO BOLLA  PRIMA IMPOSTO COME ERA
180500941010     C                   MOVEL     §3ATB1        NWTB1
180600941010     C                   MOVEL     §3ATB2        NWTB2
180700941010     C                   MOVEL     §3AFCA        NWFCA
180800941010     C                   MOVEL     §3AFAN        NWFAN
180900941010     C                   MOVEL     §3AFTD        NWFTD
181000941010     C                   MOVEL     'K8'          §CDVAR
181100941010     C                   EXSR      CNTVAR
181200941010     C  N26
181300941010     CANN27VASS          IFGT      *ZEROS
181400941010     C                   MOVEL     §CDVAR        §§KCVB
181500941010     C                   SETON                                        80
181600941010     C                   MOVEL     '0'           NWFCA
181700941010     C  N27NWTB2         IFEQ      'A3'
181800950727     C     NWTB2         OREQ      'AP'
181900941010     C                   MOVEL     '  '          NWTB2
182000941010     C                   SETON                                        39
182100941010     C                   END
182200941010     C                   END
182300140610      * codice trattamento merce per SEGNACOLLI NUOVI
182400140610     c                   exsr      srctm2k
182500941010     C                   ENDSR
182600940308     C*---------------------------------------------------------------*
182700140609     C* forzo cod.tratt.merce 2k in caso di dirottamento per fare stampare
182800140609     c* il segnacollo in arrivo
182900940308     C*---------------------------------------------------------------*
183000140609     C     SRctm2k       BEGSR
183100140610     C                   MOVEL     '6 '          §LPCTM
183200140729     c
183300170328     c                   clear                   FNLV55ds
183400170328     c                   eval      D55tpt =  '6'
183500170328     c                   eval      D55lin =  exlna
183600170328     c                   eval      D55drf =  amgudt
183700170328     c                   call      'FNLV55R'
183800170328     c                   parm                    FNLV55ds
183900170228     c                   clear                   og150
184000170328     C     d55tfa        CHAIN     AZORG01L                           21
184100140729     c                   if        not *in21
184200140729     C                   MOVEL     ORGDf0        OG150
184300170228     c                   if        §OGDKCDIR ='S'
184400140609     C                   MOVEL     '2K'          §LPCTM
184500140729     c                   endif
184600140729     c                   endif
184700140729     c
184800140609     C                   ENDSR
184900170323     C*---------------------------------------------------------------*
185000170323     C* Eseguo reperimento OG150 generico con EXLNA
185100170323     C*---------------------------------------------------------------*
185200170323     C     SRog150       BEGSR
185300170323     c                   clear                   og150
185400170323     C     exlna         CHAIN     AZORG01L                           21
185500170323     c                   if        not *in21
185600170323     C                   MOVEL     ORGDf0        OG150
185700170323     c                   endif
185800170323     c
185900170323     C                   ENDSR
186000140609     C*---------------------------------------------------------------*
186100140609     C* S R F 0 3     VIDEO X SPESE GIACENZA ......
186200140609     C*---------------------------------------------------------------*
186300140609     C     SRF03         BEGSR
186400940309     C                   SETOFF                                       6157
186500940308     C     $F03          DOWEQ     '1'
186600991028     C* Imposto flag trovata tariffa di giacenza
186700991028     C                   MOVEL     'N'           WTRTAR            1
186800160304     C                   MOVEL     'EUR'         WGCDIV            3
186900160304     C                   MOVEL     'EUR'         WNWDIV            3
187000060410     c* verifica se richiamare routine per calcolo spese sui dati del prg 0
187100060410     c* se no spese oppure se motivo con azzeramento spese non esegue
187200060410     c                   if        gcpasg <>'N' and §2cnosp = *blank
187300940310     C   33$GIAC         IFEQ      '1'
187400940310     C                   EXSR      TGCVID
187500991001     C* Reperisco tariffa di giacenza in vigore alla data del giorno
187600991028     C* per il recupero della divisa di tassazione solo a meno che
187700991028     C* non vada in variazione della vecchia bolla. In tal caso
187800991028     C* tasso in base alla divisa di quest'ultima
187900160304     C**   *IN82         IFEQ      '0'
188000160304     C**   *IN83         ANDEQ     '0'
188100160304     ***                 EXSR      TARDIV
188200160304     C**                 ELSE
188300160304     c**                 if        ar6div <> ' '
188400160304     C**                 MOVEL     AR6DIV        WNWDIV
188500160304     C**                 END
188600160304     C**                 END
188700991001     C* Se vecifico che la tariffa in vigore ha divisa diversa da
188800991001     C* quella delle spese di giacenza appena calcolate effettuo
188900991001     C* conversione
189000160304     C**   WGCDIV        IFNE      WNWDIV
189100160304     C**                 EXSR      CNVSPG
189200160304     C**                 END
189300160304     c
189400991001     C                   END
189500060410     c                   endif
189600060410      *
189700940915     C                   WRITE     LG20Z03
189800940915     C                   EXFMT     LG20D07
189900940308     C                   EXSR      CTRV03
190000940308     C                   MOVEL     '0'           $OKF03
190100940308     C   19              MOVEL     '1'           $F03
190200940308     C   KL              MOVEL     '0'           $F03
190300940308     C   KF
190400940308     CANN19              MOVEL     '0'           $F03
190500940308     C   KF
190600940308     CANN19              MOVEL     '1'           $OKF03
190700940308F03  C                   END
190800940308     C     ENDF03        ENDSR
190900940308     C*---------------------------------------------------------------*
191000940308     C*   T G C V I D    DATI TARIFFA GIACENZA                        *
191100940308     C*---------------------------------------------------------------*
191200940308     C     TGCVID        BEGSR
191300940930     C                   MOVEL     *BLANK        WTCLI             1
191400941010     C                   MOVE      *ZEROS        KKSC
191500941010     C     §KSC          IFGT      0
191600941006     C                   MOVE      §KSC          KKSC
191700941010     C                   ENDIF
191800941006     C                   Z-ADD     KKSC          APPO              4 0
191900020225      *RICHIAMO PROGRAMMA PER REPERIMENTO INFORMAZIONI TARIFFA
192000020225     C                   CLEAR                   TR27DS
192100021118     C                   MOVE      ARBctr        I27ctb
192200020318     C                   MOVE      ARBTSP        I27TSP
192300020318     C                   MOVE      ARBLNA        I27LNA
192400020318     C                   MOVE      ARBLNP        I27LNP
192500020225     C                   MOVE      KKSC          I27CLI
192600160303     C**                 MOVEL     ARBAAS        I27DTA
192700160303     C**                 MOVE      ARBMGS        I27DTA
192800160303     C                   CALL      'TRUL27R1'
192900020225     C                   PARM                    TR27DS
193000941219     C* SE CLIENTE CODIFICATO CERCO LA SUA TARIFFA
193100150512     C                   CLEAR                   fnlv59ds
193200150512     C                   MOVE      ARBCTR        KCTR
193300150512     C                   MOVEL     ' '           ilv59tbo
193400150512     C                   MOVEL     KCI           ilv59kci
193500150512     C                   MOVEL     ARBTSP        ilv59tsp
193600150512     C                   MOVEL     'S'           ilv59CON
193700150512     C                   MOVEL     AMGDSP        ilv59dsp
193800150512     c                   eval      ilv59ta2='B'
193900150512     c                   eval      ilv59blo='S'
194000150512     C                   MOVEL     KKSC          ilv59KSC
194100000202     C                   MOVEL     KKSC          W0030             3 0
194200160303    1C     APPO          IFNE      8888
194300940930     C     APPO          ANDNE     9999
194400941010     C     APPO          ANDNE     *ZEROS
194500940930     C*  Cerco progressivo tariffa in vigore su TNTAM
194600150512     C                   MOVE      O27FIE        ilv59FIE
194700150512     C                   MOVEL     KCTR          ilv59CTR
194800150512     C                   CALL      'FNLV59R'
194900150512     C                   PARM                    fnlv59ds
195000941219     C*
195100160303    2C     olv59ERR      IFEQ      *BLANKS
195200150512     C     olv59tks      ANDEQ     'S'
195300991028     C                   MOVEL     'S'           WTRTAR
195400941219     C*  Se ho trovato tariffa in vigore mi posiziono sul file
195500941219     C*  delle tariffe giacenze con il proggressivo giusto
195600150512     C                   MOVE      olv59prg      KPRG
195700990730     C     KTGC          CHAIN     TITGC01L                           21
195800941219     C  N21              MOVEL     'S'           WTCLI
195900160303    2C                   ENDIF
196000160303    1C                   ENDIF
196100940930     C*
196200940930     C** SE NON HO TROVATO TARIFFA CLIENTE CERCO SU TAR. CARTELLO
196300160303    1C     WTCLI         IFEQ      *BLANK
196400160303     c* Carico la cartello :
196500160303     c*  - per 8888 col fie della bolla
196600160303     c*  - per tariffe cliente esistente col fie della tariffa
196700160303     c                   clear                   trulc7ds
196800160303    2C     olv59ERR      ifeq      *blanks
196900160303     C     olv59tks      andeq     'S'
197000160303     c                   z-add     1             indx              3 0
197100160303     c                   eval      indx=%lookup(ilv59ctr:ks_ct)
197200160303    3c                   if        indx>0 and %editc(olv59prg:'X')=
197300160303     c                             ks_pr(indx)
197400160303     c                   eval      ic7tfie=ks_ie(indx)
197500160303   x3c                   else
197600160303     c                   eval      ic7tntw=o27ntw
197700160303    3c                   endif
197800160303     c
197900160303   x2c                   else
198000160303     c                   eval      ic7tntw=o27ntw
198100160303    2c                   endif
198200160303     c
198300160303     C                   MOVEL     AMGDSP        ic7dta
198400160303     C                   move      arbctr        Ic7ctb
198500160303     c                   call      'TRULC7R'
198600160303     c                   parm                    trulc7ds
198700160303     c
198800940930     C*  Mi posiziono sul file
198900940930     C*  delle tariffe giacenze con il proggressivo giusto
199000160303     C                   MOVE      Oc7CTRc       KCTR
199100160303     C                   MOVE      Oc7PRGc       KPRG
199200160303     C                   z-add     Oc7KSCc       KKSC
199300990730     C     KTGC          CHAIN     TITGC01L                           21
199400941219     C* SE NON HO TROVATO TARIFFA GIACENZA LO SEGNALO
199500941219     C     *IN21         IFEQ      '1'
199600941219     C                   SETON                                        57
199700941219     C                   GOTO      ENDTGC
199800941219     C                   ENDIF
199900940930     C                   ENDIF
200000940308     C*
200100940308     C     VDIG          IFEQ      *ZEROS
200200940308     C                   Z-ADD     VDTG          VDSD
200300940308     C                   ELSE
200400940308     C                   Z-ADD     VDIG          VDSD
200500940308     C                   END
200600941012     C     VDED          IFNE      *ZEROS
200700941012     C                   Z-ADD     VDED          VDSA
200800941012     C                   ELSE
200900941012     C     VDDM          IFNE      *ZEROS
201000940308     C                   Z-ADD     VDDM          VDSA
201100940325     C                   END
201200941012     C                   END
201300940308     C*
201400050701     C* CALC. le riaperture
201500050701     c                   exsr      srconta
201600050701     C* CALC. I GG. COMPLESSIVI DI SOSTA DELLE RIAPERTE
201700940308     C                   MOVE      *ZEROS        §GGRIA            3 0
201800050222     C     KGIP          SETLL     tigcp000
201900940930      *
202000940308     C     *IN21         DOUEQ     '1'
202100050316     C     KGIP          READE(e)  TIGCP000                               21
202200050316     c                   if        %error
202300070521     C     KGIP          READE(n)  TIGCP000
202400050316     c                   exsr      geslck
202500050318     C     KGIP          READE(e)  TIGCP000                               21
202600050316     c                   endif
202700940930     C*
202800940930     C  N21GCPFRG        IFGT      0
202900060410     C     GCPasg        andne     'N'
203000940930     C                   MOVE      *ZEROS        G08DAT
203100940930     C                   MOVE      *ZEROS        G08INV
203200940308     C* CALCOLO GG SOSTA PER OGNI RIAPERTURA
203300941006     C     GCPDUR        IFGT      *ZEROS
203400941006     C                   MOVE      GCPDUR        G08INV
203500940308     C                   ELSE
203600940930     C                   MOVEL     GCPAGC        G08INV
203700940930     C                   MOVE      GCPMGC        G08INV
203800940308     C                   END
203900941006     C                   MOVEL     '3'           G08ERR
204000940930     C                   CALL      'XSRDA8'
204100940930     C                   PARM                    WLBDA8
204200940930     C                   Z-ADD     G08TGI        §GGDA
204300940308     C*
204400941011     C* SE C'E' DATA ESEGUIB. CALCOLO CON QUELLA
204500941011     C     GCPDED        IFGT      *ZEROS
204600941011     C                   MOVE      GCPDED        G08INV
204700941011     C                   ELSE
204800940314     C     GCPDDM        IFNE      *ZEROS
204900940930     C                   MOVE      GCPDDM        G08INV
205000940314     C                   ELSE
205100940930     C                   MOVE      AMGUDT        G08INV
205200940314     C                   END
205300941011     C                   END
205400940930     C                   MOVEL     '3'           G08ERR
205500940930     C                   CALL      'XSRDA8'
205600940930     C                   PARM                    WLBDA8
205700940930     C                   Z-ADD     G08TGI        §GGA
205800940308     C*
205900940308     C     §GGA          SUB       §GGDA         §GGXX             3 0
206000940308     C                   ADD       1             §GGXX
206100940308     C                   ADD       §GGXX         §GGRIA
206200940930     C                   ENDIF
206300940930     C                   ENDDO
206400940930      *
206500940930      * IMPOSTO I CAMPI A VIDEO SOLO LA 1° VOLTA
206600940930      *
206700940930     C     SGD$          IFEQ      *BLANK
206800940308     C                   Z-ADD     TGCSGD        VSGD
206900940330     C* MOLTIPLICO SPESE DOSSIER    X NR. RIAPERTURE
207000940330     C     VSGD          MULT      §NRG          VSGD
207100940325     C                   MOVEL     '1'           SGD$
207200150512     C     olv59div      IFNE      *BLANK
207300150512     C                   MOVE      olv59div      V6CDIV
207400150512     C                   MOVE      olv59div      V6CRVA
207500150512     C                   MOVE      olv59div      V6CRTA
207600020318     C                   ENDIF
207700940930     C                   ENDIF
207800940930      * GG FRANCHIGIA
207900940930     C     SGF$          IFEQ      *BLANK
208000940930     C                   Z-ADD     TGCSGF        VSGF
208100940325     C                   MOVEL     '1'           SGF$
208200940930     C                   END
208300940930      * CALCOLO GG. SOSTA IMPOSTATI A VIDEO
208400060111     C                   EXSR      TGCCAL
208500940308     C*                    -----------
208600940308     C* SPESE RICONSEGNA
208700940930     C     SGR$          IFEQ      *BLANK
208800941228     C     ARBFIN        IFNE      'C'
208900110623     C     ARBFIN        andne     'T'
209000940308     C                   Z-ADD     TGCSGP        §SGR
209100940308     C                   ELSE
209200941006     C                   Z-ADD     TGCSGR        §SGR
209300940308     C                   END
209400940930     C* X NR. RIAPERTURE
209500050701     C     §SGR          MULT      d09spe        §SGR
209600940308     C* SPESE RICONS.AL QUINTALE + SPESE VARIE
209700940308     C     §SGR          MULT      §PESCA        §SGR
209800100715     C     §SGR          ADD       TGCSGV        VSGR
209900100715     c                   if        tgcfaf = '3' and
210000100715     c                             vggs <= vsgf
210100100715     c                   sub       tgcsgv        vsgr
210200100715     c                   endif
210300940325     C                   MOVEL     '1'           SGR$
210400060407     C                   ENDIF
210500940308     C     ENDTGC        ENDSR
210600050701     C*-----------------------------------------------------*
210700050701     c* conto quanti record di giacenza ho e quanti di questi sono motivi
210800050701     c* di aperture tecniche
210900050701     C*-----------------------------------------------------*
211000050701     C     srconta       BEGSR
211100050701     C*
211200050701     c                   clear                   fnlg09ds
211300050701     c                   eval      d09tla = 'R'
211400050701     c                   eval      d09AGC = §AGC
211500050701     c                   eval      d09FGC = §FGC
211600050701     c                   eval      d09NGC = §NGC
211700050701     c                   movel(p)  fnlg09ds      kpjbu
211800050701     C                   CALL      'FNLG09R'
211900050701     C                   PARM                    kpjba
212000050701     c                   movel(p)  kpjbu         fnlg09ds
212100050701     c     d09cnt        sub       d09ced        §nrg
212200060808      * verifico se motivvo di chiusura è da eliminare da motivi di riconsegna
212300060808      * solo se non l'ho precedentemente escluso come motivo di apertura PRG 0
212400060808     C                   movel     '2Z'          TBLCOD
212500060808     C                   movel     *blank        TBLKEY
212600060808     C                   movel     GCpcmc        TBLKEY
212700060808     C     KTAB          CHAIN     TABEL                              21
212800060808     C  N21              MOVEL     TBLUNI        Ds2z
212900060808     c                   if        §2znsr <> 'N' and §2nospr <> *blank
213000060808     c                   sub       1             d09spe
213100060808     c                   endif
213200050701     C*
213300050701     C                   ENDSR
213400940308     C/SPACE 3
213500991001     C*---------------------------------------------------------------*
213600991001     C*   T A R D I V    REPERISCO DIVISA TARIFFA IN VIGORE X SP.Giac *
213700991001     C*---------------------------------------------------------------*
213800160304     C     TARDIV        BEGSR
213900160304     c***
214000160304     C**Memorizzo divisa con cui ho calcolato le spese di giacenza
214100160304     C                   MOVEL     olv59div      WGCDIV            3
214200160304     C**Calcolo tariffa in vigore
214300160304     C                   MOVEL     *BLANK        WTCLI             1
214400160304     C                   MOVE      *ZEROS        KKSC
214500160304     C     §KSC          IFGT      0
214600160304     C                   MOVE      §KSC          KKSC
214700160304     C                   ENDIF
214800160304     C                   Z-ADD     KKSC          APPO              4 0
214900160304     C**SE CLIENTE CODIFICATO CERCO LA SUA TARIFFA
215000160304     C                   CLEAR                   fnlv59ds
215100160304     C                   MOVE      ARBCTR        KCTR
215200160304     C                   MOVEL     ' '           ilv59TBO
215300160304     C                   MOVEL     KCI           ilv59KCI
215400160304     C                   MOVEL     ARBTSP        ilv59TSP
215500150512     C                   MOVEL     'S'           ilv59CON
215600150512     c                   eval      ilv59ta2='B'
215700150512     c                   eval      ilv59blo='S'
215800150512     C                   MOVEL     AMGUDT        ilv59DSP
215900150512     C                   MOVEL     O27FIE        ilv59FIE
216000991001     C     APPO          IFNE      8888
216100991001     C     APPO          ANDNE     9999
216200991001     C     APPO          ANDNE     *ZEROS
216300991001     C*  Cerco progressivo tariffa in vigore su TNTAM
216400150512     C                   MOVEL     KCTR          ilv59CTR
216500150512     C                   MOVEL     KKSC          ilv59KSC
216600150512     C                   CALL      'FNLV59R'
216700150512     C                   PARM                    fnlv59ds
216800991001     C*
216900150512     C     olv59err      IFEQ      *BLANKS
217000150512     C     olv59tks      ANDEQ     'S'
217100991028     C                   MOVEL     'S'           WTRTAR
217200991001     C*  Se ho trovato tariffa in vigore mi posiziono sul file
217300991001     C*  delle tariffe giacenze con il proggressivo giusto
217400150512     C                   MOVE      olv59prg      KPRG
217500991001     C     KTGC          SETLL     TITGC01L                               21
217600991001     C   21              MOVEL     'S'           WTCLI
217700991001     C                   ENDIF
217800991001     C                   ENDIF
217900991001     C*
218000991001     C** SE NON HO TROVATO TARIFFA CLIENTE CERCO SU TAR. CARTELLO
218100991001     C     WTCLI         IFEQ      *BLANK
218200991001     C*  Mi posiziono sul file
218300991001     C*  delle tariffe giacenze con il proggressivo giusto
218400160304
218500160304     c* SE SERVE REPERIRE COL TRULC7R LA TARIFFA DI CARTELLO
218600160304     C****               MOVE      Oc7CTRc       KCTR
218700160304     C***+               MOVE      Oc7PRGc       KPRG
218800160304     C****               z-add     Oc7KSCc       KKSC
218900160304
219000991001     C     KTGC          SETLL     TITGC01L                               21
219100991001     C* SE trovata tariffa memorizzo divisa relativa altrimenti la
219200991001     C* imposto uguale a quella di giacenza
219300991001     C                   ENDIF
219400150512     C     olv59err      IFNE      '1'
219500150512     C                   MOVEL     olv59div      WNWDIV            3
219600991028     C                   ELSE
219700991028     C                   MOVEL     WGCDIV        WNWDIV
219800991028     C                   ENDIF
219900991001     C*
220000991001     C                   ENDSR
220100991001     C*---------------------------------------------------------------*
220200991001     C* CNVSPG: Procedo alla conversione delle spese di giacenza      *
220300991001     C*---------------------------------------------------------------*
220400991001     C     CNVSPG        BEGSR
220500991001     C*
220600991001     C* converto le spese di sosta
220700991001     C                   CLEAR                   YEURDS
220800991001     C                   MOVEL     WGCDIV        YECDVI
220900991001     C                   Z-ADD     VTSGS         YECIMI
221000991001     C                   MOVEL     WNWDIV        YECDVO
221100991001     C                   MOVEL     '3'           YECDCO
221200991001     C                   MOVEL     'H'           YECTAR
221300991001     C                   CALL      'YEURCO'
221400991001     C                   PARM                    YEURDS
221500991001     C                   Z-ADD     YECIMO        VTSGS
221600991001     C* converto le spese di riconsegna
221700991028     C     WNWDIV        IFNE      V6CDIV
221800991001     C                   CLEAR                   YEURDS
221900991001     C                   MOVEL     WGCDIV        YECDVI
222000991001     C                   Z-ADD     VSGR          YECIMI
222100991001     C                   MOVEL     WNWDIV        YECDVO
222200991001     C                   MOVEL     '3'           YECDCO
222300991001     C                   MOVEL     'H'           YECTAR
222400991001     C                   CALL      'YEURCO'
222500991001     C                   PARM                    YEURDS
222600991001     C                   Z-ADD     YECIMO        VSGR
222700991001     C* converto le spese di dossier
222800991001     C                   CLEAR                   YEURDS
222900991001     C                   MOVEL     WGCDIV        YECDVI
223000991001     C                   Z-ADD     VSGD          YECIMI
223100991028     C                   MOVEL     WNWDIV        YECDVO
223200991001     C                   MOVEL     '3'           YECDCO
223300991001     C                   MOVEL     'H'           YECTAR
223400991001     C                   CALL      'YEURCO'
223500991001     C                   PARM                    YEURDS
223600991001     C                   Z-ADD     YECIMO        VSGD
223700991001     C* converto le spese aggiuntive
223800991001     C                   CLEAR                   YEURDS
223900991001     C                   MOVEL     WGCDIV        YECDVI
224000991001     C                   Z-ADD     VSGA          YECIMI
224100991001     C                   MOVEL     WNWDIV        YECDVO
224200991001     C                   MOVEL     '3'           YECDCO
224300991001     C                   MOVEL     'H'           YECTAR
224400991001     C                   CALL      'YEURCO'
224500991001     C                   PARM                    YEURDS
224600991001     C                   Z-ADD     YECIMO        VSGA
224700991028     C                   END
224800991028     C* Se esistono provvigioni C/Assegno converto anche quelle
224900991028     C     VRCA          IFGT      0
225000991028     C     V6CRVA        ANDNE     WNWDIV
225100991028     C                   CLEAR                   YEURDS
225200991028     C                   MOVEL     V6CRVA        YECDVI
225300991028     C                   Z-ADD     VRCA          YECIMI
225400991028     C                   MOVEL     WNWDIV        YECDVO
225500991028     C                   MOVEL     '3'           YECDCO
225600991028     C                   MOVEL     'H'           YECTAR
225700991028     C                   CALL      'YEURCO'
225800991028     C                   PARM                    YEURDS
225900991028     C                   Z-ADD     YECIMO        VRCA
226000991028     C                   MOVEL     WNWDIV        V6CRVA
226100991028     C                   END
226200991001     C*
226300991028     C* Se esistono provvigioni C/Assegno converto anche quelle
226400991028     C     VRTR          IFGT      0
226500991028     C     V6CRTA        ANDNE     WNWDIV
226600991028     C                   CLEAR                   YEURDS
226700991028     C                   MOVEL     V6CRTA        YECDVI
226800991028     C                   Z-ADD     VRTR          YECIMI
226900991028     C                   MOVEL     WNWDIV        YECDVO
227000991028     C                   MOVEL     '3'           YECDCO
227100991028     C                   MOVEL     'H'           YECTAR
227200991028     C                   CALL      'YEURCO'
227300991028     C                   PARM                    YEURDS
227400991028     C                   Z-ADD     YECIMO        VRTR
227500991028     C                   MOVEL     WNWDIV        V6CRTA
227600991028     C                   END
227700991001     C                   MOVEL     WNWDIV        V6CDIV
227800991001     C*
227900991001     C                   ENDSR
228000940308     C*---------------------------------------------------------------*
228100940308     C*   C T R V 0 3    CALCOLO IMPORTI DA ADDEBITARE                *
228200940308     C*---------------------------------------------------------------*
228300940308     C     CTRV03        BEGSR
228400991028     C                   SETOFF                                       121314
228500991028     C                   SETOFF                                       195253
228600990716     C                   SETOFF                                       485154
228700990716     C                   SETOFF                                       5063
228800991028     C                   MOVEL     *BLANKS       $MSG
228900940308     C* CALCOLO SPESE GIACENZA
229000940308     C     *IN33         IFEQ      '1'
229100940309     C* NON HO TROVATO TARIFFA GIACENZA
229200940309     C   57              SETON                                        1950
229300940309     C   57              MOVEL     ERR(34)       $MSG
229400940309     C   57              GOTO      ENDCT3
229500990716     C* CONTROLLO SE IMMESSI DECIMALI PER DIVISA CHE NON LI GESTISCE
229600991129     C     VTSGS         IFGT      0
229700991129     C     VSGD          ORGT      0
229800991129     C     VSGA          ORGT      0
229900991129     C     VSGR          ORGT      0
230000991129     C                   Z-ADD     1             XF
230100990716     C     V6CDIV        LOOKUP    DIV(XF)                                21
230200991028     C* Se divisa inesistente do errore
230300991028     C     *IN21         IFEQ      '0'
230400991028     C                   SETON                                        1914
230500991028     C                   MOVEL     ERR(60)       $MSG
230600991028     C                   GOTO      ENDCT3
230700991028     C                   ENDIF
230800990716     C   21              MOVEL     DCV(XF)       DSCV
230900990716     C  N19§CVFDC        IFNE      'S'
231000990716     C                   MOVE      VTSGS         WDEC03            3 0
231100990716     C     WDEC03        IFNE      0
231200990716     C                   MOVEL     ERR(59)       $MSG
231300990716     C                   SETON                                        1950
231400990716     C                   GOTO      ENDCT3
231500990716     C                   ENDIF
231600990716     C                   MOVE      VSGR          WDEC03            3 0
231700990716     C     WDEC03        IFNE      0
231800990716     C                   MOVEL     ERR(59)       $MSG
231900990716     C                   SETON                                        1948
232000990716     C                   GOTO      ENDCT3
232100990716     C                   ENDIF
232200990716     C                   MOVE      VSGD          WDEC03            3 0
232300990716     C     WDEC03        IFNE      0
232400990716     C                   MOVEL     ERR(59)       $MSG
232500990716     C                   SETON                                        1951
232600990716     C                   GOTO      ENDCT3
232700990716     C                   ENDIF
232800990716     C                   MOVE      VSGA          WDEC03            3 0
232900990716     C     WDEC03        IFNE      0
233000990716     C                   MOVEL     ERR(59)       $MSG
233100990716     C                   SETON                                        1954
233200990716     C                   GOTO      ENDCT3
233300990716     C                   ENDIF
233400991129     C                   ENDIF
233500990716     C                   ENDIF
233600940308     C* CONTROLLO FORMALE   DATA   - DA
233700940930     C                   MOVE      VDSD          G08DAT
233800940930     C                   MOVE      *ZEROS        G08INV
233900940930     C                   MOVEL     *BLANK        G08ERR
234000940930     C                   CALL      'XSRDA8'
234100940930     C                   PARM                    WLBDA8
234200941006     C     G08ERR        COMP      '1'                                    21
234300940930     C  N21              MOVE      G08INV        AMGDSD
234400940308     C  N21AMGDSD        COMP      AMGDSP                               21
234500940308     C  N21AMGDSD        COMP      AMGUDT                             21
234600940308     C   21              SETON                                        5219
234700940309     C   21              MOVEL     ERR(30)       $MSG
234800940308     C* CONTROLLO FORMALE   DATA   -  A
234900940930     C                   MOVE      VDSA          G08DAT
235000940930     C                   MOVE      *ZEROS        G08INV
235100940930     C                   MOVEL     *BLANK        G08ERR
235200940930     C                   CALL      'XSRDA8'
235300940930     C                   PARM                    WLBDA8
235400941006     C     G08ERR        COMP      '1'                                    21
235500940930     C  N21              MOVE      G08INV        AMGDSA
235600940308     C  N21AMGDSA        COMP      AMGDSP                               21
235700940308     C  N21AMGDSA        COMP      AMGUDT                             21
235800940308     C   21              SETON                                        5319
235900940309     C   21              MOVEL     ERR(30)       $MSG
236000940308     C* CONTROLLO DA DATA A DATA CONGRUENZA
236100940325     C  N19              DO
236200940309     C     AMGDSD        COMP      AMGDSA                             21
236300940309     C   21              SETON                                        525319
236400940309     C   21              MOVEL     ERR(31)       $MSG
236500940309     C                   END
236600940308     C*
236700940308     C   19              GOTO      ENDCT3
236800990628     C                   Z-ADD     0             §TOTSP           11 3
236900940308     C                   MOVE      *ZEROS        VTSGS
237000940308     C                   EXSR      TGCCAL
237100991001     C* Riconverto le spese di sosta se necessario
237200991001     C     WGCDIV        IFNE      WNWDIV
237300991001     C                   CLEAR                   YEURDS
237400991001     C                   MOVEL     WGCDIV        YECDVI
237500991001     C                   Z-ADD     VTSGS         YECIMI
237600991001     C                   MOVEL     WNWDIV        YECDVO
237700991001     C                   MOVEL     '3'           YECDCO
237800991001     C                   MOVEL     'H'           YECTAR
237900991001     C                   CALL      'YEURCO'
238000991001     C                   PARM                    YEURDS
238100991001     C                   Z-ADD     YECIMO        VTSGS
238200991001     C                   END
238300940308     C     VTSGS         ADD       VSGR          §TOTSP
238400940308     C     VSGD          ADD       §TOTSP        §TOTSP
238500940308     C     VSGA          ADD       §TOTSP        §TOTSP
238600940308     C* METTO IN RI I GG SOSTA SE LI HANNO CAMBIATI
238700940308     C* SE CONFERMANO LA SECONDA VOLTA NON CONTROLLO
238800940308     C     *INKF         IFEQ      '1'
238900940308     C   61§TOTSP        IFEQ      *ZEROS
239000940308     C                   GOTO      ENDCT3
239100940308     C                   END
239200940308     C     *IN58         CABEQ     '1'           SPESE
239300940308     C                   END
239400940308     C  N19VGGS          IFNE      §GGSAV
239500940308     C                   SETON                                        5819
239600940308     C                   END
239700940308     C*
239800940308     C     SPESE         TAG
239900940308     C     §TOTSP        COMP      *ZEROS                               2121
240000940308     C   21              SETON                                        501961
240100940309     C   21              MOVEL     ERR(32)       $MSG
240200940308     C                   END
240300940308     C* CONTROLLO SE SI DEVE METTERE PROVV. C/ASS
240400991028     C     *IN35         IFEQ      '1'
240500991028     C     VRCA          IFEQ      *ZEROS
240600940308     C                   SETON                                        6319
240700940309     C                   MOVEL     ERR(33)       $MSG
240800940308     C                   END
240900991028     C                   Z-ADD     1             XF                3 0
241000991028     C     V6CRVA        LOOKUP    DIV(XF)                                21
241100991028     C* Se divisa inesistente do errore
241200991028     C     *IN21         IFEQ      '0'
241300991028     C                   SETON                                        1912
241400991028     C                   MOVEL     ERR(60)       $MSG
241500991028     C                   GOTO      ENDCT3
241600991028     C                   ENDIF
241700991028     C   21              MOVEL     DCV(XF)       DSCV
241800991028     C  N19§CVFDC        IFNE      'S'
241900991028     C                   MOVE      VRCA          WDEC03            3 0
242000991028     C     WDEC03        IFNE      0
242100991028     C                   MOVEL     ERR(59)       $MSG
242200991028     C                   SETON                                        121963
242300991028     C                   GOTO      ENDCT3
242400991028     C                   ENDIF
242500991028     C                   ENDIF
242600991028     C                   ENDIF
242700991028     C* CONTROLLO DIVISA RECUPERO TRASPORTO
242800991028     C     *IN36         IFEQ      '1'
242900991028     C                   Z-ADD     1             XF                3 0
243000991028     C     V6CRTA        LOOKUP    DIV(XF)                                21
243100991028     C* Se divisa inesistente do errore
243200991028     C     *IN21         IFEQ      '0'
243300991028     C                   SETON                                        1913
243400991028     C                   MOVEL     ERR(60)       $MSG
243500991028     C                   GOTO      ENDCT3
243600991028     C                   ENDIF
243700991028     C   21              MOVEL     DCV(XF)       DSCV
243800991028     C  N19§CVFDC        IFNE      'S'
243900991028     C                   MOVE      VRTR          WDEC03            3 0
244000991028     C     WDEC03        IFNE      0
244100991028     C                   MOVEL     ERR(59)       $MSG
244200991028     C                   SETON                                        1319
244300991028     C                   GOTO      ENDCT3
244400991028     C                   ENDIF
244500991028     C                   ENDIF
244600991028     C                   ENDIF
244700991028     C* Se controllo se le tre divise immesse sono congruenti
244800991028     C* fra loro
244900991028     C     *IN33         IFEQ      '1'
245000991028     C     *IN35         ANDEQ     '1'
245100991028     C     V6CDIV        ANDNE     V6CRVA
245200991028     C     *IN33         OREQ      '1'
245300991028     C     *IN36         ANDEQ     '1'
245400991028     C     V6CDIV        ANDNE     V6CRTA
245500991028     C     *IN35         OREQ      '1'
245600991028     C     *IN36         ANDEQ     '1'
245700991028     C     V6CRVA        ANDNE     V6CRTA
245800991028     C                   SETON                                        121314
245900991028     C                   SETON                                        19
246000991028     C                   MOVEL     ERR(61)       $MSG
246100991028     C                   END
246200991028     C*
246300940308     C     ENDCT3        ENDSR
246400940308     C/SPACE 3
246500940308     C*---------------------------------------------------------------*
246600940308     C*   T G C C A L    CALCOLO IMPORTI DA ADDEBITARE X SOSTA        *
246700940308     C*---------------------------------------------------------------*
246800940308     C     TGCCAL        BEGSR
246900050405     c                   eval      spearr = 1
247000940930      * GIORNI SOSTA DA CALCOLARE
247100940930     C     VGGS          IFEQ      *ZEROS
247200940930     C* CALCOLO GIORNI DI SOSTA  CON LE DATE IMPOSTATE A VIDEO
247300940325     C     VDSD          IFNE      *ZEROS
247400940325     C     VDSA          ANDNE     *ZEROS
247500940930     C                   MOVE      VDSD          G08DAT
247600940930     C                   MOVEL     *BLANK        G08ERR
247700940930     C                   CALL      'XSRDA8'
247800940930     C                   PARM                    WLBDA8
247900940930     C                   Z-ADD     G08TGI        §GGDA
248000940308     C*
248100940930     C                   MOVE      VDSA          G08DAT
248200940930     C                   MOVEL     *BLANK        G08ERR
248300940930     C                   CALL      'XSRDA8'
248400940930     C                   PARM                    WLBDA8
248500941006     C                   Z-ADD     G08TGI        §GGA
248600940308     C*
248700940308     C     §GGA          SUB       §GGDA         §GGSOS
248800940308     C                   ADD       1             §GGSOS
248900940308     C* SOMMO GG RIAPERTURE
249000940308     C                   ADD       §GGRIA        §GGSOS
249100940308     C     §GGSOS        IFGT      VSGF
249200940308     C                   Z-ADD     §GGSOS        §GGPAG
249300940308     C                   ELSE
249400940308     C                   Z-ADD     *ZEROS        §GGPAG
249500940308     C                   END
249600940308     C                   Z-ADD     §GGPAG        VGGS
249700940308     C* MI SALVO I GG DA PAGARE CALCOLATI X SEGNALARE SE A VIDEO LI HANNO
249800940308     C* MODIFICATI
249900940308     C                   Z-ADD     §GGPAG        §GGSAV
250000940325     C                   END
250100940930     C                   ELSE
250200940308     C*                    ----
250300940308     C     VGGS          IFGT      VSGF
250400940308     C                   Z-ADD     VGGS          §GGPAG
250500940308     C                   ELSE
250600940308     C                   Z-ADD     *ZEROS        §GGPAG
250700940930     C                   END
250800940930     C                   END
250900940930      *
251000940308     C                   Z-ADD     0             XP                1 0
251100990628     C                   MOVE      *ZEROS        SGG
251200940308     C                   MOVE      *ZEROS        STU
251300940308     C                   MOVE      *ZEROS        STT
251400940308     C*
251500940930     C     §GGPAG        IFGT      0
251600940308     C                   Z-ADD     §GGPAG        §GGPAR
251700940308     C                   DO        3             X
251800940308     C     §GGPAG        IFGT      TGS(X)
251900940308     C     §GGPAG        SUB       TGS(X)        §GGPAR
252000940308     C     X             IFEQ      1
252100940308     C                   Z-ADD     TGS(X)        SGG(X)
252200940308     C                   ELSE
252300940308     C     TGS(X)        SUB       TGS(XP)       SGG(X)
252400940308     C                   END
252500940308     C                   Z-ADD     TTS(X)        STU(X)
252600940308     C                   ELSE
252700940308     C                   Z-ADD     §GGPAR        SGG(X)
252800940308     C                   Z-ADD     TTS(X)        STU(X)
252900940308     C                   Z-ADD     0             §GGPAG
253000940308     C                   Z-ADD     0             §GGPAR
253100940308     C                   END
253200940308     C                   ADD       1             XP
253300940308     C                   END
253400940930     C                   ENDIF
253500060113      *  TARIFFA SENZA FRANCHIGIA
253600060113     C*  CALCOLO PESO TASSABILE
253700060113     C     TGCRPV        MULT      ARBVLF        §PESVO           13 3
253800060113     C     §PESVO        MULT      100           §PESVO
253900060113     C     §PESVO        IFGT      ARBPKF
254000060113     C     §PESVO        DIV       100           §PES§            13 3
254100060113     C                   ELSE
254200060113     C     ARBPKF        DIV       100           §PES§
254300060113     C                   END
254400060113     C     §PES§         ADD       0,99          §PESCA            6 0
254500060113     C* CALCOLO TOTALE SPESE SOSTA
254600060113     C     SGG           MULT      STU           STT
254700060113     C     STT           MULT      §PESCA        STT
254800060113     C                   XFOOT     STT           VTSGS
254900060113     C* MINIMO TASSABILE SPESE SOSTA  SE SUPERO  GG DI FRANCHIGIA
255000060113     C     VTSGS         IFLT      TGCSTM
255100060113     C     VGGS          ANDGT     VSGF
255200060113     C                   Z-ADD     TGCSTM        VTSGS
255300060113     C                   END
255400940930      *  TARIFFA CON FRANCHIGIA DA APPLICARE A TUTTE LE SPESE
255500940930     C     TGCFAF        IFEQ      '2'
255600940930     C     VGGS          ANDLE     VSGF
255700060113     C     sgfaf$        ANDLE     *blank
255800940930     C                   MOVE      *ZEROS        VTSGS
255900940930     C                   MOVE      *ZEROS        VSGR
256000060113     C                   MOVE      '1'           SGR$
256100940930     C                   MOVE      *ZEROS        VSGD
256200940930     C                   MOVE      *ZEROS        VSGA
256300940930     C                   ELSE
256400060109      *TARIFFA CON FRANCHIGIA DA APPLICARE A TUTTE LE SPESE tranne riconsegna
256500060109     C     TGCFAF        IFEQ      '3'
256600060109     C     VGGS          ANDLE     VSGF
256700060113     C     sgfaf$        ANDLE     *blank
256800060109     C                   MOVE      *ZEROS        VTSGS
256900060113     C*    ARBFIN        IFNE      'C'
257000110623     C*    ARBFIN        andne     'T'
257100060113     C*                  Z-ADD     TGCSGP        vsgr
257200060113     C*                  ELSE
257300060113     C*                  Z-ADD     TGCSGR        vsgr
257400060113     C*                  END
257500060109     C                   MOVE      *ZEROS        VSGD
257600060109     C                   MOVE      *ZEROS        VSGA
257700060109     C                   ELSE
257800950120      *  TARIFFA CON FRANCHIGIA DA APPLICARE SOLO ALLE SPESE SOSTA
257900950120     C     TGCFAF        IFEQ      '1'
258000060113     C     sgfaf$        ANDLE     *blank
258100950120     C     VGGS          ANDLE     VSGF
258200950120     C                   MOVE      *ZEROS        VTSGS
258300060113     C*    ARBFIN        IFNE      'C'
258400110623     C*    ARBFIN        andne     'T'
258500060113     C*                  Z-ADD     TGCSGP        vsgr
258600060113     C*                  ELSE
258700060113     C*                  Z-ADD     TGCSGR        vsgr
258800060113     C*                  END
258900060113     C*mm                ELSE
259000950120     C                   ENDIF
259100060109     C                   ENDIF
259200940930     C                   ENDIF
259300060113     C                   move      '1'           sgfaf$
259400940308     C     ENDCAL        ENDSR
259500940308     C*---------------------------------------------------------------*
259600940308     C*   P G A R B K  RICH. PGM VARIAZ. BOLLA   C/ASS O COD.BOLLA    *
259700940308     C*---------------------------------------------------------------*
259800940308     C     PGARBK        BEGSR
259900940308     C     §§KCVB        IFNE      *BLANK
260000941215     C                   MOVEL     ' '           D48TLA
260100941215     C                   MOVEL     FILARR        D48FGS
260200941214     C                   MOVE      ARBAAS        D48AAS
260300941214     C                   MOVE      ARBLNP        D48LNP
260400941214     C                   MOVE      ARBNRS        D48NRS
260500941214     C                   MOVE      ARBNSP        D48NSP
260600941214     C                   MOVEL     ARBCBO        D48CBO
260700941214     C                   MOVEL     §§KCVB        D48CVB
260800941214     C                   MOVEL     'K'           D48TRC
260900941214     C                   MOVEL     'S'           D48FFR
261000941214     C                   MOVEL     ' '           D48ERR
261100941214     C                   MOVEL     *BLANK        D48MSG
261200941222     C                   MOVEL     'A'           D48TBO
261300941215     C     §BKCAS        IFGT      *ZEROS
261400941219     C                   MOVE      VTIC          §BKTIC
261500940308     C                   ELSE
261600940308     C                   MOVE      *BLANK        §BKTIC
261700940308     C                   END
261800040301     c                   z-add     arbQFT        §BTQFT
261900040301     c                   z-add     arbVMD        §BTVMD
262000040301     c                   MOVE      arbVAD        §BTVAD
262100040301     c                   z-add     arbIAS        §BTIAS
262200040301     c                   MOVE      arbVAS        §BTVAS
262300941219     C                   MOVEL     AR9GCA        §BKGCA
262400941219     C                   MOVEL     ARBGGA        §BKGGA
262500941219     C                   MOVEL     ARBGMA        §BKGMA
262600941219     C                   MOVEL     ARBGVA        §BKGVA
262700960205     C*§ ABBLENCO 2°BYTE PARTIC. VARIE
262800960205     C                   MOVE      ' '           §BKGVA
262900941214     C                   MOVEL     DSLR48        KPJBU
263000941214     C                   CALL      'FNLR48R'
263100940308     C                   PARM                    KPJBA
263200940308     C                   PARM                    DSARBD
263300940308     C                   PARM                    DSARBK
263400991001     C                   PARM                    DARBT
263500940308     C                   PARM                    DSARBG
263600030715     C                   PARM                    trul90ds
263700941214     C                   MOVEL     KPJBU         DSLR48
263800941214     C     D48ERR        IFEQ      *BLANK
263900941214     C                   CLEAR                   DSLR48
264000940308     C                   END
264100940308     C                   END
264200940308     C                   ENDSR
264300940308     C*---------------------------------------------------------------*
264400940308     C*   P G A R B I  RICH. PGM VARIAZ. BOLLA   INDIR. DESTINATARIO  *
264500940308     C*---------------------------------------------------------------*
264600940308     C     PGARBI        BEGSR
264700941215     C                   MOVEL     ' '           D48TLA
264800941215     C                   MOVEL     FILARR        D48FGS
264900941215     C                   MOVE      ARBAAS        D48AAS
265000941215     C                   MOVE      ARBLNP        D48LNP
265100941215     C                   MOVE      ARBNRS        D48NRS
265200941215     C                   MOVE      ARBNSP        D48NSP
265300941215     C                   MOVEL     ARBCBO        D48CBO
265400140729     C***                MOVEL     'I7'          D48CVB
265500140729     c* ES: 29/07/14 reimposto la causale I0 e solo in caso di disposiz
265600140729     c*     automatiche impostiamo I7
265700140729     C                   MOVEL     'I0'          D48CVB
265800941215     C                   MOVEL     'D'           D48TRC
265900941215     C                   MOVEL     ' '           D48ERR
266000941215     C                   MOVEL     *BLANK        D48MSG
266100941222     C                   MOVEL     'A'           D48TBO
266200941215     C*
266300060317     c                   if        flgdis = 'F'
266400060317     C                   MOVEL     'S'           §BDFFD
266500140729     C                   MOVEL     'I7'          D48CVB
266600060317     c                   else
266700060317     c                   move      arbffd        §bdffd
266800060317     c                   endif
266900940308     C                   MOVEL     ARBCPI        §BDCPI
267000941215     C                   MOVE      ARBPKF        §BDPKF
267100941215     C                   MOVE      ARBVLF        §BDVLF
267200940308     C                   MOVE      ARBFIN        §BDFIN
267300940308     C                   MOVEL     VRSD          §BDRSD
267400941223     C                   MOVEL     VRD2          §BDRD2
267500940308     C                   MOVEL     VIND          §BDIND
267600940308     C                   MOVEL     VCAD          §BDCAD
267700940308     C                   MOVEL     VLOD          §BDLOD
267800940308     C                   MOVEL     VPRD          §BDPRD
267900950206     C                   MOVEL     VNZD          §BDNZD
268000060314     C                   MOVEL     Vref          §BDref
268100060314     C                   MOVEL     Vteld         §BDtel
268200941215     C                   MOVE      ARBFIN        §BDFIN
268300040301     c                   z-add     arbQFT        §BTQFT
268400040301     c                   z-add     arbVMD        §BTVMD
268500040301     c                   MOVE      arbVAD        §BTVAD
268600040301     c                   z-add     arbIAS        §BTIAS
268700040301     c                   MOVE      arbVAS        §BTVAS
268800941215     C*
268900941215     C                   MOVEL     DSLR48        KPJBU
269000941215     C                   CALL      'FNLR48R'
269100940308     C                   PARM                    KPJBA
269200940308     C                   PARM                    DSARBD
269300940308     C                   PARM                    DSARBK
269400991001     C                   PARM                    DARBT
269500940308     C                   PARM                    DSARBG
269600030715     C                   PARM                    trul90ds
269700941215     C                   MOVEL     KPJBU         DSLR48
269800941215     C     D48ERR        IFEQ      *BLANK
269900941215     C                   CLEAR                   DSLR48
270000941215     C                   END
270100940308     C                   ENDSR
270200940308     C*---------------------------------------------------------------*
270300940308     C*   P G A R B T  RICH. PGM VARIAZ. BOLLA  TASSAZ. 1° BOLLA      *
270400940308     C*---------------------------------------------------------------*
270500940308     C     PGARBT        BEGSR
270600941215     C                   MOVEL     ' '           D48TLA
270700941215     C                   MOVEL     FILARR        D48FGS
270800941215     C                   MOVE      ARBAAS        D48AAS
270900941215     C                   MOVE      ARBLNP        D48LNP
271000941215     C                   MOVE      ARBNRS        D48NRS
271100941215     C                   MOVE      ARBNSP        D48NSP
271200941215     C                   MOVEL     ARBCBO        D48CBO
271300941215     C                   MOVEL     §§TCVB        D48CVB
271400941215     C                   MOVEL     'T'           D48TRC
271500941215     C                   MOVEL     'S'           D48FFR
271600941215     C                   MOVEL     ' '           D48ERR
271700941215     C                   MOVEL     *BLANK        D48MSG
271800941222     C                   MOVEL     'A'           D48TBO
271900941215     C*
272000940308     C     §BTKSC        IFEQ      0
272100940308     C                   MOVE      ARBKSC        §BTKSC
272200991203     C     AR6KSC        IFNE      0
272300991001     C     AR6TRC        ANDEQ     '1'
272400991001     C                   MOVE      AR6KSC        §BTKSC
272500940308     C                   END
272600991001     C                   END
272700991001     C                   MOVE      '1'           §BTTRC
272800991001     C                   MOVE      ARBCTR        §BTCTR
272900941006     C                   MOVE      AR6POR        §BTPOR
273000991001     C                   MOVE      AR6DIV        §BTDIV
273100040301     c                   z-add     arbQFT        §BTQFT
273200040301     c                   z-add     arbVMD        §BTVMD
273300040301     c                   MOVE      arbVAD        §BTVAD
273400040301     c                   z-add     arbIAS        §BTIAS
273500040301     c                   MOVE      arbVAS        §BTVAS
273600000105     C* SE IL CODICE VARIAZIONE E' T0 (NON ESISTE LA BOLLA)
273700000105     C* IMPOSTO LA DIVISA DALLA GIACENZA
273800000105     C     §CDVAR        IFEQ      'T0'
273900000105     C     AR6DIV        ANDEQ     *BLANKS
274000000105     C                   MOVE      V6CDIV        §BTDIV
274100000105     C                   END
274200940308     C                   MOVE      *ZEROS        §BTIMV
274300991001     C                   MOVE      AR6CEI        §BTCEI
274400941216     C* SE HO SPESE GIACENZA AGGIORNO LA VARIA I
274500941216     C                   MOVEL     §$2VRI        §SIGLA            1
274600941216     C                   MOVEL     VSV           WSV
274700941216      * CERCO L'INDICE SU CUI LAVORARE
274800941216     C                   EXSR      CHKVAR
274900950726     C     §TOTSP        IFGT      0
275000941216     C                   Z-ADD     §TOTSP        VVA(XS)
275100941216     C                   MOVEL     §$2VRI        VSV(XS)
275200950726     C                   ELSE
275300950726     C     VSV(XS)       IFEQ      §$2VRI
275400950726     C                   MOVEL     ' '           VSV(XS)
275500950726     C                   Z-ADD     0             VVA(XS)
275600950726     C                   END
275700950726     C                   END
275800991001     C* Tolgo varia Iva ...
275900991001     C                   Z-ADD     1             XS
276000991001     C     §$2BOL        LOOKUP    VSV(XS)                                20
276100991001     C     *IN20         IFEQ      '1'
276200991001     C                   MOVEL     ' '           VSV(XS)
276300991001     C                   Z-ADD     0             VVA(XS)
276400991001     C                   END
276500991001     C* ... e bollo sono state impostate
276600991001     C                   Z-ADD     1             XS
276700991001     C     §$2IVA        LOOKUP    VSV(XS)                                20
276800991001     C     *IN20         IFEQ      '1'
276900991001     C                   MOVEL     ' '           VSV(XS)
277000991001     C                   Z-ADD     0             VVA(XS)
277100991001     C                   END
277200941215     C*
277300941215     C                   MOVEL     DSLR48        KPJBU
277400941215     C                   CALL      'FNLR48R'
277500940308     C                   PARM                    KPJBA
277600940308     C                   PARM                    DSARBD
277700940308     C                   PARM                    DSARBK
277800991001     C                   PARM                    DARBT
277900940308     C                   PARM                    DSARBG
278000030715     C                   PARM                    trul90ds
278100941215     C*
278200941215     C                   MOVEL     KPJBU         DSLR48
278300941215     C     D48ERR        IFEQ      *BLANK
278400941215     C                   CLEAR                   DSLR48
278500941215     C                   END
278600940308     C                   ENDSR
278700940308     C*---------------------------------------------------------------*
278800940308     C*   P G A R 2 T  RICH. PGM VARIAZ. BOLLA  TASSAZ. 2° BOLLA      *
278900940308     C*---------------------------------------------------------------*
279000940308     C     PGAR2T        BEGSR
279100941215     C                   MOVEL     ' '           D48TLA
279200941215     C                   MOVEL     FILARR        D48FGS
279300941215     C                   MOVE      ARBAAS        D48AAS
279400941215     C                   MOVE      ARBLNP        D48LNP
279500941215     C                   MOVE      ARBNRS        D48NRS
279600941215     C                   MOVE      ARBNSP        D48NSP
279700941215     C                   MOVEL     ARBCBO        D48CBO
279800991001     C                   MOVEL     §§TCVB        D48CVB
279900991001     C                   MOVEL     '2'           D48TRC
280000941215     C                   MOVEL     'S'           D48FFR
280100941215     C                   MOVEL     ' '           D48ERR
280200941215     C                   MOVEL     *BLANK        D48MSG
280300941222     C                   MOVEL     'A'           D48TBO
280400991001     C                   MOVE      AR6CTR        §BTCTR
280500991001     C                   MOVE      AR6POR        §BTPOR
280600991001     C                   MOVE      AR6DIV        §BTDIV
280700040301     c                   z-add     arbQFT        §BTQFT
280800040301     c                   z-add     arbVMD        §BTVMD
280900040301     c                   MOVE      arbVAD        §BTVAD
281000040301     c                   z-add     arbIAS        §BTIAS
281100040301     c                   MOVE      arbVAS        §BTVAS
281200000105     C* SE IL CODICE VARIAZIONE E' T0 (NON ESISTE LA BOLLA)
281300000105     C* IMPOSTO LA DIVISA DALLA GIACENZA
281400000105     C     §CDVAR        IFEQ      'T0'
281500000105     C     AR6DIV        ANDEQ     *BLANKS
281600000105     C                   MOVE      V6CDIV        §BTDIV
281700000105     C                   END
281800991001     C                   MOVE      *ZEROS        §BTIMV
281900991001     C                   MOVE      AR6CEI        §BTCEI
282000941215     C*
282100991001     C     §BTKSC        IFEQ      0
282200991001     C                   MOVE      AR6KSC        §BTKSC
282300940308     C                   END
282400991001     C                   MOVE      '2'           §BTTRC
282500991001     C* Tolgo varia Iva ...
282600991001     C                   Z-ADD     1             XS
282700991001     C     §$2BOL        LOOKUP    VSV(XS)                                20
282800991001     C     *IN20         IFEQ      '1'
282900991001     C                   MOVEL     ' '           VSV(XS)
283000991001     C                   Z-ADD     0             VVA(XS)
283100991001     C                   END
283200991001     C* ... e bollo sono state impostate
283300991001     C                   Z-ADD     1             XS
283400991001     C     §$2IVA        LOOKUP    VSV(XS)                                20
283500991001     C     *IN20         IFEQ      '1'
283600991001     C                   MOVEL     ' '           VSV(XS)
283700991001     C                   Z-ADD     0             VVA(XS)
283800991001     C                   END
283900941215     C*
284000941215     C                   MOVEL     DSLR48        KPJBU
284100941215     C                   CALL      'FNLR48R'
284200940308     C                   PARM                    KPJBA
284300940308     C                   PARM                    DSARBD
284400940308     C                   PARM                    DSARBK
284500991001     C                   PARM                    DARBT
284600940308     C                   PARM                    DSARBG
284700030715     C                   PARM                    trul90ds
284800941215     C*
284900021009     C                   MOVEL     KPJBU         DSLR48
285000941215     C     D48ERR        IFEQ      *BLANK
285100941215     C                   CLEAR                   DSLR48
285200941215     C                   END
285300940308     C                   ENDSR
285400940309     C*?$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
285500940308     C*?    R O U T I N E     INIZIO
285600940309     C*?$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
285700940308     C*---------------------------------------------------------------*
285800940308     C*   S R G I A C  - RICHIAMATO DA GIACENZE                       *
285900940308     C*---------------------------------------------------------------*
286000940308     C     SRGIAC        BEGSR
286100050316     C     KGIA          CHAIN(e)  tigcp02l                           40
286200050316     c                   if        %error
286300070521     C     KGIA          CHAIN(n)  tigcp02l
286400050316     c                   exsr      geslck
286500050318     C     KGIA          CHAIN(e)  tigcp02l                           40
286600050316     c                   endif
286700940308     C   40              MOVEL     '1'           $FINE
286800940308     C   40              GOTO      ENDGIA
286900940308     C*
287000050222     C     GCPriap       COMP      'R'                                    42
287100941003     C* PRENDO DATI DELLA BOLLA  - FLARB01L
287200050222     C                   MOVE      GCPAASa       KAAS
287300050222     C                   MOVE      GCPLNPa       KLNP
287400050222     C                   MOVE      GCPNRSa       KNRS
287500050222     C                   MOVE      GCPNSPa       KNSP
287600170405      * verifico se la fase della giacenza è ancora congrua a ricevere
287700170405      * disposizioni
287800170418     C***  gcpfas        IFge      35
287900170418      *
288000170418     C***  36= immesse in partenza (quindi da considerare)
288100170418     c                   if        gcpFAS <> 36
288200170418      *
288300170418     C***  ma se >= di 35 deve NON FARE ESEGUIRE
288400170418     c                   if        gcpFAS >= 35
288500170405     C                   MOVEL     '1'           $FINE
288600170405     C                   GOTO      ENDGIA
288700170405     C                   END
288800170418     c                   end
288900170405      *
289000940308     C                   EXSR      CHKARB
289100940308     C     $ERR          IFEQ      '1'
289200940308     C                   MOVEL     '1'           $FINE
289300940308     C                   GOTO      ENDGIA
289400940308     C                   END
289500940308     C     ENDGIA        ENDSR
289600940308     C/SPACE 3
289700940308     C*---------------------------------------------------------------*
289800940308     C*   S R D I R O  - RICHIAMATO DA DIROTTAM/CAMBI PORTO           *
289900940308     C*---------------------------------------------------------------*
290000940308     C     SRDIRO        BEGSR
290100940928     C                   MOVE      ANNO          VAAS
290200940308     C                   MOVE      *ZEROS        VLNP
290300940308     C                   MOVE      *ZEROS        VNRS
290400940308     C                   MOVE      *ZEROS        VNSP
290500950104     C                   SETOFF                                       94
290600940928      * NR. SPEDIZIONE PASSATA : NON EMETTO VIDEATA RICHIESTA NR.SPED.
290700941216     C     D82NSP        IFNE      *BLANK
290800941216     C                   MOVE      D82AAS        VAAS
290900941216     C                   MOVE      D82LNP        VLNP
291000941216     C                   MOVE      D82NRS        VNRS
291100941216     C                   MOVE      D82NSP        VNSP
291200141113     c                   if        d82dis <> 'D'
291300950104     C                   SETON                                        94
291400160504     c                   exsr      chkevb
291500141113     c                   endif
291600940928     C                   GOTO      NOFD01
291700940928     C                   ENDIF
291800940308     C*
291900940308     C     FG2010        TAG
292000141006     c                   if        d82nodsp <> *blank
292100141007     c                   if        *in43
292200141006     c                   movel     $msg          d20errmo
292300141006     c                   movel     'E'           d20erro
292400141006     c                   return
292500141006     c                   endif
292600141006     c                   else
292700940915     C                   EXFMT     LG20D01
292800141006     c                   endif
292900940308     C                   MOVEL     *BLANK        $MSG
293000940308     C                   SETOFF                                       434419
293100950131     C*
293200950131     C                   MOVEL     VAAS          WAA2              2 0
293300950131     C     WAA2          IFEQ      0
293400950131     C     VAAS          IFGT      60
293500950131     C                   ADD       1900          VAAS
293600950131     C                   ELSE
293700950131     C                   ADD       2000          VAAS
293800950131     C                   END
293900950131     C                   END
294000940308     C*
294100940308     C*  OPERAZIONI DA EFFETTUARE SE FILIALE DI ARRIVO DI PRIMO LIVELLO
294200940308     C     *IN01         IFEQ      '1'
294300940308     C*  CONTROLLO SE HO RICHIESTO MODIFICA FILIALE
294400940308     C     *INKS         IFEQ      '1'
294500940308     C                   SETON                                        02
294600940308     C                   GOTO      FG2010
294700940308     C                   END
294800940308     C*
294900940308     C*  CONTROLLO SE HO MODIFICATO LA FILIALE
295000940308     C     *IN03         IFEQ      '1'
295100940308     C* AGGANCIO LA £6 E RECUPERO LE LINEE A LORO ASSOCIATE
295200940308     C*
295300021009     c                   eval      kpjbus = kpjbu
295400020509     C                   CLEAR                   TR06DS
295500020509     C                   MOVEL     '£6'          D06COD
295600020509     C                   MOVEL     filarr        D06KEY
295700020509     C                   MOVEL     TR06DS        KPJBU
295800020509     C                   CALL      'TRUL06R'
295900020509     C                   PARM                    KPJBA
296000020509     C                   MOVEL     KPJBU         TR06DS
296100020509     C                   MOVEA     LIN           l£6
296200021009     c                   eval      kpjbu = kpjbus
296300020226     C                   END
296400020226     C                   END
296500940308     C*  INTERROG. BOLLE
296600940308     C   KG              DO
296700940308     C                   MOVEL     KPJBU         §KPJBU
296800070622     C                   CLEAR                   param1
296900070622     C                   MOVEL     '2'           Pa1FLG
297000070622     C                   MOVEL     param1        KPJBU
297100941006     C                   CALL      'FNLR36R'
297200940308     C                   PARM                    KPJBA
297300070622     C                   MOVEL     KPJBU         param1
297400070622     C     Pa1AAS        IFNE      *ZEROS
297500070622     C                   MOVEL     Pa1AAS        VAAS
297600070622     C                   MOVEL     Pa1LNP        VLNP
297700070622     C                   MOVEL     Pa1NRS        VNRS
297800070622     C                   MOVEL     Pa1NSP        VNSP
297900940308     C                   END
298000940308     C                   MOVEL     §KPJBU        KPJBU
298100940308     C                   GOTO      FG2010
298200940308     C                   END
298300940308     C   KC              MOVEL     '1'           $FINE
298400940308     C   KC              GOTO      ENDDIR
298500940928     C     NOFD01        TAG
298600941216     C                   MOVE      VAAS          KAAS
298700941216     C                   MOVE      VLNP          KLNP
298800941216     C                   MOVE      VNRS          KNRS
298900941216     C                   MOVE      VNSP          KNSP
299000940308     C                   EXSR      CHKARB
299100940308     C     $ERR          IFEQ      '1'
299200940308     C                   SETON                                        19
299300940308     C                   MOVEL     ERR(1)        $MSG
299400940308     C                   GOTO      FG2010
299500940308     C                   END
299600000120     C     $ERR          IFEQ      '2'
299700000120     C                   SETON                                        19
299800000120     C                   MOVEL     ERR(62)       $MSG
299900000120     C                   GOTO      FG2010
300000000120     C                   END
300100000421      *CONTROLLO PER FILIALE POSTE  ERRORE BOLLE NON TRATTABILI
300200000417     C     $ERR          IFEQ      '3'
300300000417     C                   SETON                                        19
300400000417     C                   MOVEL     ERR(63)       $MSG
300500000417     C                   GOTO      FG2010
300600000417     C                   END
300700940928     C* E' UNA FIGLIA: NO DIROTT./CAMBIO PORTO
300800940928     C     KBOL          CHAIN     FNLBL01L                           43
300900940308     C     *IN43         IFEQ      '0'
301000940308     C     LBLLAN        COMP      LBLLAP                                 43
301100940308     C   43              MOVEL     ERR(2)        $MSG
301200940308     C   43              SETON                                        19
301300940308     C   19              GOTO      FG2010
301400940308     C                   ELSE
301500940308     C                   SETOFF                                       43
301600940308     C                   END
301700940308     C* E' UNA MAMMA : ERRORE
301800940928     C     KBOL          SETLL     FNLBL02L                               43
301900940308     C   43              SETON                                        19
302000940308     C   19              MOVEL     ERR(2)        $MSG
302100940308     C   19              GOTO      FG2010
302200940308     C* BOLLA SU ALTRA DISTINTA
302300940308     C                   Z-ADD     0             PROGR
302400940308     C     ARBNDC        COMP      *ZEROS                             4343
302500940308     C   43              SETON                                        19
302600940308     C   19              MOVEL     ERR(3)        $MSG
302700940308     C   19              GOTO      FG2010
302800940308     C* BOLLA  GIACENTE (O.K. SE GIAC. ANNULLATA)
302900050222     C     KBOLG         CHAIN     tigcp21L                           21
303000940308     C  N21GCPATB        COMP      '*'                                4343
303100990716     C   43
303200050418     CANN21GCPFAS        COMP      40                                   4343
303300990716     C   43              SETON                                        19
303400940308     C   19              MOVEL     ERR(4)        $MSG
303500940308     C   19              GOTO      FG2010
303600940308     C* LINEA ARRIVO NON IN GESTIONE
303700940308     C     ARBLNA        LOOKUP    L£6                                    21
303800940308     C  N21              SETON                                        1944
303900940308     C   19              MOVEL     ERR(5)        $MSG
304000940308     C   19              GOTO      FG2010
304100940928     C* BOLLA GIA CONSEGNATA
304200940928     C     ARBDCM        COMP      *ZEROS                             4343
304300940928     C   43              SETON                                        19
304400940928     C   19              MOVEL     ERR(6)        $MSG
304500940928     C   19              GOTO      FG2010
304600940308     C* BOLLA CONSEGNATA PARZIALMENTE
304700940308     C     ARBDCP        COMP      *ZEROS                             4343
304800940308     C   43              MOVEL     ERR(6)        $MSG
304900940308     C   43              SETON                                        19
305000940308     C   19              GOTO      FG2010
305100940308     C* BOLLA BLOCCATA
305200940308     C     ARBFBC        COMP      *BLANK                             4343
305300940308     C   43              MOVEL     ERR(7)        $MSG
305400940308     C   43              SETON                                        19
305500940308     C   19              GOTO      FG2010
305600110909     C* BOLLA con esiti PDA
305700110909     C                   EXSR      SRCTRPDA
305800110909     C   43              MOVEL     ERR(67)       $MSG
305900110909     C   43              SETON                                        19
306000110909     C   19              GOTO      FG2010
306100141103     C* BOLLA con disposizioni di consegna/dirottamento se dirottamento errore
306200141103      * esegue solo se richiamato da menu
306300141103     C                   IF        $giac = '0' and d82nsp = *blank
306400141103     C                             and %parms = 1
306500141103     c                   clear                   fnlry09ds2
306600141103     c                   eval      ilry09tdis= ' '
306700141103     c                   eval      ilry09tch = 'C'
306800141103     c                   eval      ilry09aas = arbaas
306900141103     c                   eval      ilry09lnp = arblnp
307000141103     c                   eval      ilry09nrs = arbnrs
307100141103     c                   eval      ilry09nsp = arbnsp
307200141103     c                   eval      ilry09fgs = arblna
307300141103     c                   movel     fnlry09ds2    kpjbu
307400141103     c                   call      'FNLRY09R'
307500141103     c                   parm                    kpjba
307600141103     c                   movel     kpjbu         fnlry09ds2
307700141103     c                   if        olry09err = *blank
307800141103     c                   select
307900141103     c                   when      olry09esdi = '1'
308000141103     C                   MOVEL     ERR(68)       $MSG
308100141103     C                   SETON                                        1943
308200141103     C   19              GOTO      FG2010
308300141103      *  altro
308400141103     c                   when      olry09esal = '1'
308500141103     c                   eval      ilry09tdis= 'E'
308600141103     c                   eval      ilry09tch = 'E'
308700141103     c                   movel     fnlry09ds2    kpjbu
308800141103     c                   call      'FNLRY09R'
308900141103     c                   parm                    kpjba
309000141103     c                   movel     kpjbu         fnlry09ds2
309100141103     c                   if        olry09esal = '1'
309200151204     C                   MOVEL     ERR(69)       $MSG
309300141105     C                   SETON                                        1943
309400141105     C   19              GOTO      FG2010
309500141103     c                   endif
309600141103     c                   endsl
309700141103     c                   endif
309800141103     C                   ENDIF
309900940308      * TUTTO OK
310000950104     C* SE DISPOSIZIONI PASSATE DA PGM CHIAMANTE
310100950104     C     D82NSP        IFNE      *BLANK
310200950104     C     D82DIS        ANDNE     *BLANK
310300950104     C                   MOVEL     D82DIS        VFRI
310400950104     C                   ELSE
310500940308     C                   MOVEL     *BLANK        VFRI
310600950104     C                   END
310700940308     C                   MOVEL     *BLANK        VNPC
310800940308     C                   MOVEL     'N'           VVCS
310900940308     C                   MOVE      *ZEROS        VNCA
311000940308     C                   MOVEL     'N'           VAPC
311100940308     C                   MOVEL     *BLANK        VPPC
311200940308     C                   MOVEL     *BLANK        DFRI
311300940308     C                   MOVEL     *BLANK        DNPC
311400940308     C                   MOVEL     *BLANK        DPPC
311500960327     C                   MOVEL     'N'           V4CRCS
311600960327     C                   SETOFF                                       07
311700940308     C     ENDDIR        ENDSR
311800940308     C/SPACE 3
311900940308     C*---------------------------------------------------------------*
312000110909     C*   srctrpda - CONTROLLO se esiti pda                           *
312100940308     C*---------------------------------------------------------------*
312200110909     C     srctrpda      BEGSR
312300110909     c*
312400110909     c     kbol          chain     fipdc02t
312500110909     c                   if        not %found(fipdc02t)
312600110909     c                   leavesr
312700110909     c                   end
312800110909     c* se trova la bolla sul file di trasmissione dati al pda verifico
312900110909     c* se confermato annullamento
313000110909     c                   if        pdcatb = ' '
313100110909     c                   seton                                        43
313200110909     c                   leavesr
313300110909     c                   end
313400110909     c* verifico di non avere ricevuto esiti
313500110909     c                   eval      pcttrd = 'CET'
313600110909     c     kpct          setgt     fipct01l
313700110909     c     kpct          readpe    fipct01l
313800110909     c                   if        not %eof(fipct01l) and pdcndc = pctndc
313900110909     c                   seton                                        43
314000110909     c                   leavesr
314100110909     c                   end
314200110909     c* verifico se ricevuta la conferma all'annullamento altrimenti errore
314300110909     c                   eval      pcttrd = 'COK'
314400110909     c     kpct          setgt     fipct01l
314500111212     c                   do        *hival
314600110909     c     kpct          readpe    fipct01l
314700111212     c                   if        %eof(fipct01l)
314800111212     c                   leave
314900111212     c                   end
315000111212     c                   if        pdcndc = pctndc
315100110909     c                   eval      fipctcokds = pctdati
315200110909     c                   if        §PCTIDROW < PDCIDROW
315300110909     c                   seton                                        43
315400110909     c                   leavesr
315500110909     c                   end
315600110909     c                   end
315700111212     c                   enddo
315800110909     c*
315900110909     C     END           ENDSR
316000110909     C/SPACE 3
316100110909     C*---------------------------------------------------------------*
316200110909     C*   C H K A R B  - CONTROLLO /LETTURA FNARB01L                  *
316300110909     C*---------------------------------------------------------------*
316400110909     C     CHKARB        BEGSR
316500940308     C                   MOVE      *ZEROS        §KSC
316600940308     C                   MOVE      *ZEROS        §QUATR
316700940308     C                   MOVEL     '0'           $ERR
316800941219     C     KBOL          CHAIN(N)  FNARB01L                           43
316900940928     C  N43ARBDCM        COMP      *ZEROS                             43
317000940308     C   43              MOVEL     '1'           $ERR
317100940308      *
317200991001     C     $ERR          IFEQ      '0'
317300941010     C                   MOVE      ARBMGS        AMGDSP
317400941010     C                   MOVEL     ARBAAS        AMGDSP
317500941012     C                   MOVE      AMGDSP        G08INV
317600941012     C                   MOVE      *ZEROS        G08DAT
317700941012     C                   MOVEL     '3'           G08ERR
317800941012     C                   CALL      'XSRDA8'
317900941012     C                   PARM                    WLBDA8
318000941012     C                   Z-ADD     G08DAT        VDSP
318100941216     C                   MOVE      ARBMGS        EXMGS
318200941216     C                   MOVE      ARBLNA        EXLNA
318300950102     C                   MOVE      ARBAAS        EXAAS
318400950102     C                   MOVE      ARBLNP        EXLNP
318500950102     C                   MOVE      ARBNRS        EXNRS
318600950102     C                   MOVE      ARBNSP        EXNSP
318700940308     C* TIPO BOLLA
318800941006     C                   MOVEL     ARBCBO        VTBL
318900940308     C                   MOVEL     '3A'          TBLCOD
319000940308     C                   MOVEL     *BLANK        TBLKEY
319100941006     C                   MOVEL     ARBCBO        TBLKEY
319200940308     C     KTAB          CHAIN     TABEL                              21
319300940308     C   21              MOVEL     *ALL'?'       DTBL
319400940308     C  N21              MOVEL     TBLUNI        DS3A
319500940308     C  N21              MOVEL     §3ADES        DTBL
319600940308     C  N21              MOVEL     §3ATB1        EXPORT            1
319700950725     C  N21              MOVEL     §3ATB1        WEXTB1            2
319800950725     C  N21              MOVEL     §3ATB2        WEXTB2            2
319900940308     C  N21              MOVEL     §3AFTD        TASDES            1
320000940308     C  N21EXPORT        IFEQ      'A'
320100940308     C                   MOVE      ARBKSC        §KSC
320200941006     C                   MOVE      ARBCTR        KCTR
320300940308     C                   END
320400000120      * SE BOLLA DI RECUPERO NON LAQSCIO EFFETTUARE NON LASCIO
320500000120      * EFFETTUARE NE RESO NE DIROTTAMENTO NE CAMBIO DI PORTO
320600000120     C     §3ARBL        IFEQ      'R'
320700000120     C                   MOVEL     '2'           $ERR
320800000120     C                   END
320900941219      * GUARDO SE BOLLA ITALIA O ESTERO
321000941219     C                   CLEAR                   DS15
321100941219     C                   MOVEL     '15'          TBLCOD
321200941219     C                   MOVEL     *BLANK        TBLKEY
321300941219     C                   MOVEL     ARBNZD        TBLKEY
321400941219     C     KTAB          CHAIN     TABEL                              21
321500941219     C  N21              MOVEL     TBLUNI        DS15
321600971113     C* Scopro se filiale Estera in base a flag su AZORG
321700971113     C                   MOVEL     *BLANKS       ORGFL1
321800971113     C     ARBLNP        CHAIN     AZORG01L                           21
321900971230     C                   MOVEL     ORGDIT        DITLNP            3
322000000421      *CONTROLLO SE FILIALE POSTE-SE SI BOLLA NON TRATTABILE TRANNE SERIE 00
322100000417     C                   MOVEL     ORGDE3        OG143
322200000421     C                   MOVE      *BLANKS       WPOSTE            1
322300020729     C     §OgNTW        IFEQ      'PPT'
322400000421     C     ARBNRS        IFEQ      0
322500000421     C                   MOVE      '1'           WPOSTE
322600000421     C                   ELSE
322700000417     C                   MOVE      '3'           $ERR
322800000421     C                   END
322900000417     C                   END
323000000417      *
323100940928      * BOLLA CON CONTRASSEGNO
323200941216     C                   MOVE      *ZEROS        VASS
323300941216     C                   MOVEL     *BLANK        VVCAP
323400941216     C                   MOVEL     *BLANK        VVTIC
323500950221     C                   CLEAR                   AR9CAS
323600950221     C                   CLEAR                   AR9CMB
323700950221     C                   CLEAR                   AR9GCA
323800950221     C                   CLEAR                   AR9VCA
323900950221     C                   CLEAR                   AR9TIC
324000940928     C     §3AFCA        IFNE      0
324100051115     C     KBOL          CHAIN     FIAR901L                           43
324200941216     C  N43              Z-ADD     AR9CAS        VASS
324300940928     C  N43              MOVEL     AR9VCA        VVCAP
324400941216     C  N43              MOVEL     AR9TIC        VVTIC
324500940928     C                   ENDIF
324600941217     C* IMPOSTO LA DIVISA UAGUALE ALLA VECCHIA SE NO GIACENZE
324700941217     C     $GIAC         IFEQ      '0'
324800941010     C                   MOVEL     VVCAP         VVCA
324900941221     C* GUARDO CHE DIVISA è
325000941221     C                   CLEAR                   DSCV
325100941221     C                   Z-ADD     1             X
325200941221     C     VVCA          LOOKUP    DIV(X)                                 21
325300941221     C   21              MOVEL     DCV(X)        DSCV
325400941221     C*
325500941217     C                   ENDIF
325600941217     C* IMPOSTO  TIPO INCASSO SEMPRE UGUALE AL   VECCHIO
325700941217     C                   MOVEL     VVTIC         VTIC
325800940928     C* PRENDO DATI TASSAZIONE  1° BOLLA
325900941216     C                   CLEAR                   AVA
326000941216     C                   CLEAR                   ASV
326100950221     C                   CLEAR                   AR6FIV
326200950221     C                   CLEAR                   AR6NFT
326300950221     C                   CLEAR                   AR6DFT
326400950221     C                   CLEAR                   AR6CEI
326500950221     C                   CLEAR                   AR6IFT
326600950221     C                   CLEAR                   AR6IMV
326700950221     C                   CLEAR                   AR6POR
326800991001     C                   CLEAR                   AR6CTR
326900991001     C                   CLEAR                   AR6KSC
327000990930     C* CONTROLLO SE LA BOLLA E' IN PORTO ASSEGNATO
327100991001     C                   MOVEL     §3ATB1        WPORTO            1
327200991001     C     WPORTO        IFEQ      'A'
327300990930     C                   MOVEL     '1'           KTRC
327400020618     C     KAR6          CHAIN     FIAR601L                           43
327500991001     C     AR6DFT        IFGT      0
327600991001     C                   MOVE      AR6FIV        VFIV
327700991001     C                   MOVE      AR6NFT        VNFT
327800991001     C                   MOVE      AR6DFT        G08INV
327900991001     C                   MOVE      *ZEROS        G08DAT
328000991001     C                   MOVEL     '3'           G08ERR
328100991001     C                   CALL      'XSRDA8'
328200991001     C                   PARM                    WLBDA8
328300991001     C                   Z-ADD     G08DAT        VDFT
328400991001     C                   END
328500991001     C                   ELSE
328600991001     C                   MOVEL     §3ATB2        WPORTO
328700991001     C     WPORTO        IFEQ      'A'
328800991001     C                   MOVEL     '2'           KTRC
328900020618     C     KAR6          CHAIN     FIAR601L                           43
329000991001     C                   END
329100991001     C                   END
329200941010     C* CARICO LE VARIE PRESENTI + DATI FATTURA
329300991001     C     *IN43         IFEQ      '0'
329400991001     C* Imposto codice tariffa e codice cliente da AR6
329500991001     C* se c'è
329600991001     C                   MOVE      AR6KSC        §KSC
329700991001     C                   MOVE      AR6CTR        KCTR
329800941010      *
329900941216     C                   MOVEL     AR6SV1        ASV(1)
330000941216     C                   MOVE      AR6VA1        AVA(1)
330100941216     C                   MOVEL     AR6SV2        ASV(2)
330200941216     C                   MOVE      AR6VA2        AVA(2)
330300941216     C                   MOVEL     AR6SV3        ASV(3)
330400941216     C                   MOVE      AR6VA3        AVA(3)
330500940929     C* PRENDO DATI ESTENSIONE TASSAZIONE X VARIE AGGIUNTIVE
330600940929      * SE LA TERZA VARIA è IMPOSTATA
330700991001     C     AR6SV3        IFNE      *BLANK
330800940929     C                   Z-ADD     3             X
330900040123     C     Kar6          SETLL     FIAR7000
331000040123     C     Kar6          READE     FIAR7000                               43
331100991001     C     *IN43         DOWEQ     *OFF
331200940929     C                   ADD       1             X
331300941216     C                   MOVEL     AR7SVN        ASV(X)
331400941216     C                   MOVE      AR7VAN        AVA(X)
331500040123     C     Kar6          READE     FIAR7000                               43
331600991001     C                   ENDDO
331700991001     C                   ENDIF
331800020102     C*CONVERTO IMPORTI IN EURO SE LA VECCHIA BOLLA ERA IN LIRE
331900020102     C     AR6DIV        IFNE      §GEDCN
332000020102      *SPESE TRASPORTO
332100020102     C     AR6POR        IFGT      0
332200020102     C                   CLEAR                   YEURDS
332300020102     C                   MOVEL     AR6DIV        YECDVI
332400020102     C                   Z-ADD     AR6POR        YECIMI
332500020102     C                   MOVEL     §GEDCN        YECDVO
332600020102     C                   MOVEL     '3'           YECDCO
332700020102     C                   MOVEL     'H'           YECTAR
332800020102     C                   CALL      'YEURCO'
332900020102     C                   PARM                    YEURDS
333000020102     C                   Z-ADD     YECIMO        AR6POR
333100020102     C                   ENDIF
333200020102     C                   DO        20            XM                3 0
333300020102     C     AVA(XM)       IFGT      0
333400020102     C                   CLEAR                   YEURDS
333500020102     C                   MOVEL     AR6DIV        YECDVI
333600020102     C                   Z-ADD     AVA(XM)       YECIMI
333700020102     C                   MOVEL     §GEDCN        YECDVO
333800020102     C                   MOVEL     '3'           YECDCO
333900020102     C                   MOVEL     'H'           YECTAR
334000020102     C                   CALL      'YEURCO'
334100020102     C                   PARM                    YEURDS
334200020102     C                   Z-ADD     YECIMO        AVA(XM)
334300020102     C                   ENDIF
334400020102     C                   ENDDO
334500020102     C                   ENDIF
334600991001     C                   ENDIF
334700941011     C* PRENDO 2° RAGIOSNE SOCIALE DESTINATARIO
334800950221     C                   CLEAR                   AR4NOT
334900950307     C                   CLEAR                   AR4RD2
335000941011     C                   MOVE      'D'           §TRC
335100060213     C     KAR4          CHAIN(N)  FiAR401L                           43
335200941011     C  N43              MOVE      AR4NOT        VRD2
335300941227     C  N43              MOVE      AR4NOT        AR4RD2
335400031119      *eseguo la stessa operazione per i riferimenti mittenti tipo
335500031119      * rekord GEN riferimenti destinatario
335600031119     C                   clear                   dar5gen
335700031119     C                   move      'GEN'         trcar5
335800051013     C     Kfiar5        CHAIN(n)  FiAR501L                           43
335900031119     C     *IN43         IFEQ      '0'
336000031119     C                   MOVEL     ar5uni        dar5gen
336100031119     C                   endif
336200941011     C                   END
336300940308     C                   ENDSR
336400920113     C*---------------------------------------------------------------*
336500940309     C*   A R B V I D                                                 *
336600920113     C*---------------------------------------------------------------*
336700941219     C     ARBVID        BEGSR
336800961009     C* se c'è il reso obbligat. da particolarità reperisco il motivo
336900961009     C* dall' ultimo evento della spedizione
337000961009     C   94              EXSR      CHKEVB
337100940307     C                   SETOFF                                       197172
337200940307     C                   SETOFF                                       737475
337300050222     C                   SETOFF                                       7647
337400940309     C                   MOVEL     '0'           $FOANC            1
337500940915     C                   WRITE     LG20Z02
337600920114     C     VID11         TAG
337700000421     C     WPOSTE        IFEQ      '1'
337800000421     C                   MOVE      'R'           VFRI
337900000421     C                   MOVE      'S'           V4CRCS
338000000421     C                   SETON                                        93
338100000421     C                   ELSE
338200000421     C                   SETOFF                                       93
338300000421     C                   ENDIF
338400940915     C                   WRITE     LG20Z03
338500940915     C                   EXFMT     LG20D04
338600940307     C                   MOVEL     *BLANK        $MSG
338700940307     C                   SETOFF                                       197172
338800940307     C                   SETOFF                                       737475
338900050222     C                   SETOFF                                       7667
339000961009     C                   SETOFF                                       4796
339100940309     C   KL              GOTO      ENDARB
339200100614      * richiamo pgm interrogazione bolle
339300100614     C   Kg              exsr      richbl
339400920305     C* KH= CMD 08 PER FORZARE ANNULLO CONTRASSEGMO
339500940309      *--------------------------------------------
339600940309     C   KH              MOVEL     '1'           $FOANC
339700941217     C*  IMMESSO ?
339800941217     C     VFRI          IFEQ      '?'
339900941217     C                   MOVEL     '2D'          §COD
340000941217     C                   MOVEL     *BLANK        §KEY
340100941217     C                   MOVEL     *BLANK        §DES
340200941217     C                   MOVE      *BLANK        VVCA
340300941217     C                   CALL      'X§TB60R'
340400941217     C                   PARM                    CODUT
340500941217     C                   PARM                    §COD
340600941217     C                   PARM                    §KEY
340700941217     C                   PARM                    §DES
340800941217     C                   MOVEL     §KEY          VFRI
340900941217     C                   GOTO      VID11
341000941217     C                   ENDIF
341100960327     C*  Se ho immesso disposizioni di rientro riemetto videata
341200960327     C*  se non ho ancora visualizzato se reso per avaria.
341300960327     C     VFRI          IFEQ      'R'
341400960327     C     *IN07         IFEQ      '0'
341500960327     C                   SETON                                        07
341600960327     C                   GOTO      VID11
341700960327     C                   END
341800960327     C                   ELSE
341900960327     C*  Se non ho immesso disposizioni di rientro azzero
342000960327     C*  indicatore reso per avaria.
342100960327     C                   SETOFF                                       07
342200960327     C                   MOVEL     'N'           V4CRCS
342300960327     C                   END
342400941217     C*  SOLO RICONSEGNA O DIROTTAMENTO  O RIENTRO
342500920114     C                   MOVEL     '2D'          TBLCOD
342600920114     C                   MOVEL     *BLANK        TBLKEY
342700920114     C                   MOVEL     VFRI          TBLKEY
342800920114     C     KTAB          CHAIN     TABEL                              21
342900920114     C   21              SETON                                        7119
343000920114     C   21              MOVEL     *ALL'?'       DFRI
343100920114     C  N21              MOVEL     TBLUNI        DS2D
343200920114     C  N21              MOVEL     §2DDES        DFRI
343300940307     C   71              MOVEL     ERR(8)        $MSG
343400941217     C   19              GOTO      VID11
343500920131     C* POSSO FARE SOLO DIROTTAM.O INVERSIONI DI PORTO
343600940307     C  N19VFRI          IFEQ      'C'
343700920131     C     VNPC          ANDEQ     *BLANK
343800940307     C                   SETON                                        7219
343900940307     C                   MOVEL     ERR(9)        $MSG
344000920131     C                   END
344100920114     C* NUOVO PORTO DEVE ESSERE DIVERSO DALLA BOLLA VECCHIA
344200920114     C  N19VNPC          IFEQ      EXPORT
344300921026     C     VFRI          ANDNE     'R'
344400941228     C                   SETON                                        7219
344500941228     C                   MOVEL     ERR(10)       $MSG
344600920114     C                   END
344700960205     C* NUOVO PORTO DEVE ESSERE DIVERSO DALLA BOLLA VECCHIA
344800960205     C  N19VNPC          IFNE      *BLANKS
344900960205     C     ARBFBR        ANDEQ     'S'
345000960315     C     VFRI          ANDEQ     'C'
345100960205     C                   SETON                                        7219
345200960205     C                   MOVEL     ERR(46)       $MSG
345300960205     C                   END
345400960327     C* Impossibile eseguire reso in C/Servizio su bolla di rientro
345500960327     C     ARBFBR        IFEQ      'S'
345600960327     C     VFRI          ANDEQ     'R'
345700960327     C     V4CRCS        ANDEQ     'S'
345800960328     C                   SETON                                        477119
345900960327     C                   MOVEL     ERR(49)       $MSG
346000960327     C                   END
346100960315     C* NUOVO PORTO non ammesso per disposizioni di reso
346200960315     C  N19VNPC          IFNE      *BLANKS
346300960315     C     VFRI          ANDEQ     'R'
346400960315     C                   SETON                                        7219
346500960315     C                   MOVEL     ERR(47)       $MSG
346600960315     C                   END
346700990203     C* Controllo se esiste pretica CA aperta se richiesto
346800990203     C* cambio di porto/dirottamentoCA aperta se richiesto
346900990203     C  N19VFRI          IFEQ      'C'
347000990203     C     VFRI          OREQ      'D'
347100990204     C*
347200040716      * SOSTITUISCO LETTURA DEL FILE FNDCT02L CON LA RICERCA DELLE CA LEGATE ALL
347300040716      * SIA COME MAMMA CHE COME FIGLIA
347400040716     c                   clear                   fidn12ds
347500040716     c                   eval      i12aas = kaas
347600040716     c                   eval      i12lnp = klnp
347700040716     c                   eval      i12nrs = knrs
347800040716     c                   eval      i12nsp = knsp
347900040716     c                   eval      i12fel = 'E'
348000040716     c                   eval      i12fan = 'E'
348100040716     c                   eval      i12fch = 'E'
348200040716     c                   call      'FIDN12R'
348300040716     c                   parm                    fidn12ds
348400040716      **
348500040716      * se c'è almeno una C.A. aperta
348600040716      **
348700040716     c     o12nca        ifgt      0
348800990203     C                   SETON                                        7119
348900990203     C                   MOVE      ERR(58)       $MSG
349000990203     C                   END
349100990203     C                   END
349200950331     C* SE BOLLA DA NON TRASMETTERE IN SEDE SI PUO' FARE SOLO
349300950307     C* DIROTTAM. SENZA CAMBIO DI PORTO IN QUANTO FORZERO' NELLA
349400950307     C* BOLLA DIROTTATA COD. BOLLA =£, PERCHè QUESTA BOLLA NON
349500950307     C* VIENE INVIATA IN SEDE E ANCHE LA FIGLIA NON VA TRASM.IN SEDE
349600950331     C     *IN19         IFEQ      '0'
349700950331     C                   CLEAR                   DS3A
349800950331     C                   Z-ADD     1             §F
349900950331     C     ARBCBO        LOOKUP    CDB(§F)                                04
350000950331     C   04              MOVEL     D3A(§F)       DS3A
350100950331     C     §3ABSD        IFEQ      'N'
350200950410     C     VFRI          ANDNE     'D'
350300950410     C     §3ABSD        OREQ      'N'
350400950410     C     VNPC          ANDNE     *BLANK
350500950307     C                   SETON                                        7119
350600950307     C                   MOVEL     ERR(45)       $MSG
350700950307     C                   END
350800950307     C                   END
350900921026     C* SE RIENTRO IL C/ASSEGNO DEVE ESSERE ANNULLATO
351000921026     C  N19VFRI          IFEQ      'R'
351100921026     C     VASS          ANDGT     0
351200921026     C     VVCS          IFNE      'S'
351300921026     C     VNCA          ORGT      *ZEROS
351400940307     C                   SETON                                        7319
351500940307     C                   MOVEL     ERR(11)       $MSG
351600921026     C                   END
351700921026     C                   END
351800040206     C* SE RIENTRO IL C/ASSEGNO DEVE ESSERE ANNULLATO
351900040206     C     Vnca          IFgt      *zeros
352000040206     C     Vvca          andeq     *blank
352100040206     C                   SETON                                        7319
352200040206     C                   MOVEL     ERR(64)       $MSG
352300040206     C                   END
352400960315     C* SE RIENTRO IL C/ASSEGNO DEVE ESSERE ANNULLATO
352500960315     C  N19VFRI          IFEQ      'R'
352600960315     C     VASS          ANDEQ     0
352700960315     C     VNCA          IFGT      0
352800960315     C     VVCS          OREQ      'S'
352900960315     C                   SETON                                        7319
353000960315     C                   MOVEL     ERR(48)       $MSG
353100960315     C                   END
353200960315     C                   END
353300940930     C* SI VARIAZ. C/A MA NON VARIATO L'IMPORTO O LA DIVISA
353400940930     C* NO VARIAZ. C/A MA     VARIATO L'IMPORTO O LA DIVISA
353500920114     C  N19VVCS          IFEQ      'S'
353600920114     C     VNCA          ANDEQ     VASS
353700940930     C     VVCA          ANDEQ     VVCAP
353800920114     C     VVCS          OREQ      'N'
353900920114     C     VNCA          ANDNE     VASS
354000920129     C     VNCA          ANDNE     *ZEROS
354100940930     C     VVCS          OREQ      'N'
354200940930     C     VVCA          ANDNE     VVCAP
354300920114     C                   SETON                                        7319
354400940307     C                   MOVEL     ERR(12)       $MSG
354500920114     C                   END
354600920207     C* NO CALCOLO PROVV.C/A MA VARIATO C/A
354700920114     C* NO CALCOLO PROVV.C/A MA INDICATO CHI PAGA
354800920114     C  N19VAPC          IFEQ      'N'
354900920114     C     VVCS          ANDEQ     'S'
355000920207     C     VNCA          ANDNE     *ZEROS
355100920114     C     VAPC          OREQ      'N'
355200920114     C     VPPC          ANDNE     *BLANK
355300920114     C                   SETON                                        7419
355400940307     C                   MOVEL     ERR(13)       $MSG
355500920114     C                   END
355600920114     C  N19VPPC          IFNE      *BLANK
355700920114     C                   MOVEL     '2E'          TBLCOD
355800920114     C                   MOVEL     *BLANK        TBLKEY
355900920114     C                   MOVEL     VPPC          TBLKEY
356000920114     C     KTAB          CHAIN     TABEL                              21
356100920114     C   21              SETON                                        7519
356200920114     C   21              MOVEL     *ALL'?'       DPSG
356300920114     C  N21              MOVEL     TBLUNI        DS2E
356400920114     C  N21              MOVEL     §2EDES        DPPC
356500920114     C                   ELSE
356600920114     C* SI CALCOLO PROVV.C/A MA NON INDICATO CHI PAGA
356700920114     C     VAPC          IFEQ      'S'
356800920114     C                   SETON                                        7519
356900920114     C                   END
357000920114     C                   END
357100940307     C   75              MOVEL     ERR(14)       $MSG
357200941221     C* SE EX PORTO ERA ASSEGNATO E NON E' CAMBIATO  O
357300941221     C* SE NUOVO PORTO è ASSEGNATO ..NON SI PUO INDICARE PROVVIG.
357400941010     C* A CARICO MITTENTE
357500941010     C  N19VNPC          IFEQ      'A'
357600941010     C     VPPC          ANDEQ     'M'
357700941221     C     EXPORT        OREQ      'A'
357800941221     C     VNPC          ANDEQ     ' '
357900941221     C     VPPC          ANDEQ     'M'
358000941010     C                   SETON                                          7519
358100941010     C                   MOVEL     ERR(14)       $MSG
358200941010     C                   ENDIF
358300940310     C* SE C'E' C/ASS: sE DIROTTO O NUOVO PORTO = ASSEGNATO
358400940310     C* DEVO INDICARE CHI PAGA LE PROVV.
358500940930     C  N19VASS          IFGT      *ZEROS
358600941223     C     VVCS          ANDEQ     'N'
358700940310     C     VNCA          ORGT      *ZEROS
358800940310     C     VFRI          IFEQ      'D'
358900940310     C     VAPC          ANDEQ     'N'
359000960108     C     VFRI          OREQ      'C'
359100940310     C     VAPC          ANDEQ     'N'
359200940310     C                   SETON                                        747519
359300940310     C                   MOVEL     ERR(13)       $MSG
359400940310     C                   END
359500940310     C                   END
359600940930     C* CONTROLLO DIVISA
359700941012     C                   MOVEA     VVCA          VCA§
359800941012     C     '?'           LOOKUP    VCA§                                   21
359900941012     C   21              DO
360000941012     C                   MOVEL     'CV'          §COD
360100941012     C                   MOVEL     *BLANK        §KEY
360200941012     C                   MOVEL     *BLANK        §DES
360300941012     C                   MOVE      *BLANK        VVCA
360400941012     C                   CALL      'X§TB60R'
360500941012     C                   PARM                    CODUT
360600941012     C                   PARM                    §COD
360700941012     C                   PARM                    §KEY
360800941012     C                   PARM                    §DES
360900941012     C                   MOVEL     §KEY          VVCA
361000941012     C                   GOTO      VID11
361100941012     C                   END
361200940930     C  N19VVCA          IFNE      *BLANK
361300940930     C     VVCAP         ORNE      *BLANK
361400940930     C     VVCA          IFNE      *BLANK
361500940930     C                   MOVEL     VVCA          WVCA              3
361600940930     C                   ELSE
361700940930     C                   MOVEL     VVCAP         WVCA
361800940930     C                   END
361900940930     C     VNCA          IFNE      *ZEROS
362000940930     C                   Z-ADD     VNCA          W0133            13 3
362100940930     C                   ELSE
362200940930     C                   Z-ADD     VASS          W0133
362300940930     C                   END
362400940930     C                   EXSR      CTRDIV
362500940930     C                   ENDIF
362600920305     C* RICH. CMD 08 PER CONFERMA ANNULLO C/ASSEGNO
362700940309     C  N19$FOANC        IFEQ      '0'
362800940309     C     VNCA          ANDEQ     0
362900920305     C     VVCS          ANDEQ     'S'
363000941010     C     VVCA          ANDEQ     *BLANK
363100940307     C                   SETON                                        1976
363200940307     C                   MOVEL     ERR(16)       $MSG
363300920305     C                   END
363400961009     C*  Se  obbligo disposiz. di reso metto in VNOT1 il motivo dall'evento
363500961009     C*  e obbligo l'inserimento del motivo se l'evento è generico
363600961009     C     *IN94         IFEQ      '1'
363700961009     C     VNOT1         ANDEQ     *BLANK
363800961009     C                   SETON                                        1996
363900961009     C                   MOVEL     ERR(50)       $MSG
364000961009     C                   ENDIF
364100930503     C  NKF
364200920114     COR 19              GOTO      VID11
364300940309     C     ENDARB        ENDSR
364400961009     C/SPACE 3
364500961009     C*---------------------------------------------------------------*
364600961009     C*   C H K E V B  _                                              *
364700961009     C*---------------------------------------------------------------*
364800961009     C     CHKEVB        BEGSR
364900961009     C                   MOVE      VAAS          KAAS1
365000961009     C                   MOVE      VLNP          KLNP1
365100961009     C                   MOVE      VNRS          KNRS1
365200961009     C                   MOVE      VNSP          KNSP1
365300961009     C                   MOVE      VNSP          KNSP1
365400961009     C                   MOVE      *HIVAL        KDEV
365500961009     C                   MOVE      *HIVAL        KHEV
365600990803     C     KEVB1         SETGT     FNEVB01L
365700990803     C     KEVB2         READPE    FNEVB01L                               45
365800961009     C     *IN45         IFEQ      '0'
365900081219     C                   Z-ADD     1             XY                3 0
366000961009     C     EVBCEV        LOOKUP    C2A(XY)                                45
366100961009     C     *IN45         IFEQ      '1'
366200961009     C                   MOVEL     D2A(XY)       DS2A
366300041122      * se l'evento,la cui descrizione deve essere messa nelle note della
366400041122      * bolla di reso per effetto della particolarità,è un evento di giacenz
366500060213      * di attesa (G02, G03..) prendo la descrizione da FiAR4 TRK "G"
366600041122     c                   if        §2Aftc='G' and §2Agla>0 and §2Aflg<>'S'
366700041122     C                   MOVEL     'G'           §TRC
366800060213     C     KAR4          CHAIN(N)  FiAR401L                           43
366900041122     C  N43              MOVEL     AR4NOT        VNOT1
367000041122     c                   else
367100041122     c                   MOVEL     §2ADES        VNOT1
367200041122     c                   endif
367300961009     C                   ENDIF
367400961009     C                   ENDIF
367500050504     c* se particolarità consegne = no parziali imposto la dicitura nel
367600050504     c* campo delle note: Reso x particolarità "NO consegna parziale"
367700050504     c                   if        vnot1 = *blanks and arbgma <> ' '
367800050504     C                   MOVEL     '7R'          TBLCOD
367900050504     C                   MOVEL     *BLANK        TBLKEY
368000050504     C                   MOVEL     ARBGMA        TBLKEY
368100050504     C     KTAB          CHAIN     TABEL                              21
368200050504     C     *IN21         IFEQ      '0'
368300050504     C                   MOVEL     TBLUNI        DS7R
368400050504     C                   if        §7R8cp='N'
368500050504     c                   eval      vnot1=
368600050505     c                             'Reso:NO consegna -
368700050505     c                             parziale'
368800050504     c                   end
368900050504     c                   end
369000050504     c                   end
369100961009     C                   ENDSR
369200940308     C/SPACE 3
369300940308     C*---------------------------------------------------------------*
369400940308     C*   G C A V I D                                                 *
369500940308     C*---------------------------------------------------------------*
369600940308     C     GCAVID        BEGSR
369700940308     C* DATA APERTURA GIACENZA
369800940308     C                   MOVEL     GCPAGC        AMGDTG
369900941012     C                   MOVE      GCPMGC        AMGDTG
370000941012     C                   MOVE      AMGDTG        G08INV
370100941012     C                   MOVE      *ZEROS        G08DAT
370200941012     C                   MOVEL     '3'           G08ERR
370300941012     C                   CALL      'XSRDA8'
370400941012     C                   PARM                    WLBDA8
370500941012     C                   Z-ADD     G08DAT        VDTG
370600940315     C                   MOVE      GCPNGC        VNGC
370700940308     C* FIL. CHE HA APERTO LA GIACENZA
370800940308     C                   MOVE      GCPFGC        VFGC
370900940308     C     VFGC          IFNE      *ZEROS
371000940308     C     VFGC          CHAIN     AZORG01L                           21
371100940308     C  N21ORGFVA        COMP      '*'                                    21
371200940308     C   21              MOVEL     *ALL'?'       DFGC
371300940308     C  N21              MOVEL     ORGDES        DFGC
371400940308     C                   END
371500940308     C*
371600940308     C* DATA ULTIMA RIAPERTURA
371700050222     c                   if        gcpriap = 'R'
371800940928     C                   MOVE      GCPDUR        G08INV
371900940928     C                   MOVE      *ZEROS        G08DAT
372000940928     C                   MOVEL     '3'           G08ERR
372100940928     C                   CALL      'XSRDA8'
372200940928     C                   PARM                    WLBDA8
372300940928     C                   Z-ADD     G08DAT        VDIG
372400050222     c                   else
372500050222     C                   Z-ADD     0             VDIG
372600050222     c                   end
372700940308     C* DATI DELLA SPEDIZIONE
372800050222     C                   Z-ADD     GCPLNPa       VLNP
372900050222     C                   Z-ADD     GCPAASa       VAAS
373000050222     C                   Z-ADD     GCPNRSa       VNRS
373100050222     C                   Z-ADD     GCPNSPa       VNSP
373200940308     C* DATA DISPOSIZINI
373300940928     C                   MOVE      GCPDDM        G08INV
373400940928     C                   MOVE      *ZEROS        G08DAT
373500940928     C                   MOVEL     '3'           G08ERR
373600940928     C                   CALL      'XSRDA8'
373700940928     C                   PARM                    WLBDA8
373800940928     C                   Z-ADD     G08DAT        VDDM
373900940928     C* DATA ESEGUIBILITA DISPOSIZIONI
374000940928     C                   SETOFF                                       79
374100940928     C     GCPDED        IFGT      0
374200940928     C                   MOVE      GCPDED        G08INV
374300940928     C                   MOVE      *ZEROS        G08DAT
374400940928     C                   MOVEL     '3'           G08ERR
374500940928     C                   CALL      'XSRDA8'
374600940928     C                   PARM                    WLBDA8
374700940928     C                   Z-ADD     G08DAT        VDED
374800940928     C     GCPDED        IFGT      AMGUDT
374900940928     C                   MOVEL     ERR(36)       $MSG
375000940928     C                   SETON                                        19
375100940928     C                   ENDIF
375200940928     C                   SETON                                        79
375300940928     C                   ENDIF
375400940308     C* TIPO DISPOSIZIONI
375500940311     C* SE HO FORZATO DISPOSIZ. NON IMPOSTO DA GIAC
375600940311     C     $FOFRI        IFEQ      '0'
375700941006     C                   MOVEL     GCPDIS        VFRI
375800940311     C                   MOVEL     GCPASG        VASG
375900940311     C                   MOVEL     GCPPSG        VPSG
376000940311     C                   END
376100940308     C                   MOVEL     '2D'          TBLCOD
376200940308     C                   MOVEL     *BLANK        TBLKEY
376300940308     C                   MOVEL     VFRI          TBLKEY
376400940308     C     KTAB          CHAIN     TABEL                              21
376500940308     C   21              MOVEL     *ALL'?'       DFRI
376600940308     C  N21              MOVEL     TBLUNI        DS2D
376700940308     C  N21              MOVEL     §2DDES        DFRI
376800940308     C* SPESE GIACENZA
376900940308     C     VPSG          IFNE      *BLANK
377000940308     C                   MOVEL     '2E'          TBLCOD
377100940308     C                   MOVEL     *BLANK        TBLKEY
377200940308     C                   MOVEL     VPSG          TBLKEY
377300940308     C     KTAB          CHAIN     TABEL                              21
377400940308     C   21              MOVEL     *ALL'?'       DPSG
377500940308     C  N21              MOVEL     TBLUNI        DS2E
377600940308     C  N21              MOVEL     §2EDES        DPSG
377700940308     C                   END
377800940308     C* NUOVO PORTO DI CONSEGNA
377900940308     C                   MOVEL     GCPNPC        VNPC
378000940308     C     GCPNPC        IFNE      *BLANK
378100940308     C     GCPNPC        COMP      'A'                                    21
378200940308     C   21              MOVEL     'ASSEGNAT'    DNPC
378300940308     C   21              MOVE      'O '          DNPC
378400940308     C  N21              MOVEL     'FRANCO  '    DNPC
378500940308     C  N21              MOVE      '  '          DNPC
378600940308     C                   ELSE
378700940308     C                   MOVE      *BLANKS       DNPC
378800940308     C                   END
378900991221     C* GEST. C/ASSEGNO
379000040203     c                   if        gcpvca <> *blank
379100940928     C                   MOVEL     GCPVCA        VVCA
379200040203     c                   else
379300040203     C                   MOVEL     ar9VCA        VVCA
379400040203     c                   end
379500941217     C* GUARDO CHE DIVISA è
379600941217     C                   CLEAR                   DSCV
379700941217     C                   Z-ADD     1             X
379800941217     C     VVCA          LOOKUP    DIV(X)                                 21
379900941217     C   21              MOVEL     DCV(X)        DSCV
380000941217     C*
380100940308     C                   MOVEL     GCPVCS        VVCS
380200940928     C                   MOVE      GCPCAS        VNCA
380300940308     C                   MOVEL     GCPAPC        VAPC
380400940308     C                   MOVEL     GCPPPC        VPPC
380500940308     C     VPPC          IFNE      *BLANKS
380600940308     C                   MOVEL     '2E'          TBLCOD
380700940308     C                   MOVEL     *BLANK        TBLKEY
380800940308     C                   MOVEL     VPPC          TBLKEY
380900940308     C     KTAB          CHAIN     TABEL                              21
381000940308     C   21              MOVEL     *ALL'?'       DPSG
381100940308     C  N21              MOVEL     TBLUNI        DS2E
381200940308     C  N21              MOVEL     §2EDES        DPPC
381300940308     C                   END
381400960327     C                   MOVEL     'N'           V4CRCS
381500960327     C                   SETOFF                                       07
381600940308     C                   ENDSR
381700940308     C*---------------------------------------------------------------*
381800940308     C* G C A N O T   GESTIONE NOTE
381900940308     C*---------------------------------------------------------------*
382000940308     C     GCANOT        BEGSR
382100940308     C                   EXSR      PULIZ
382200940308     C                   MOVE      *ZEROS        §VARR
382300970625     C*  CARICO NOTE APERTURA GIACENZA
382400970625     C                   Z-ADD     12            §FAS
382500050222     C     KNOT          CHAIN     tignp01L                           21
382600970625     C     *IN21         IFEQ      '0'
382700970625     C                   MOVEL     GNPDMC        VDMC
382800970625     C                   ADD       1             NRR               3 0
382900970625     C                   WRITE     LG20S05
383000970625     C                   END
383100970625     C*  NOTE GIACENZA
383200970625     C                   Z-ADD     20            §FAS
383300050222     C     KNOT          SETLL     tignp01L                               21
383400970625     C     *IN21         DOUEQ     '1'
383500050222     C     KNOT          READE     tignp01L                               21
383600051013     c                   if        not *in21
383700051013     c                   movel     gcpflr        dgcpflr
383800051013     c* se disposizioni automatiche
383900051013     c                   if        d§gcpdisa = 'S' and gnptpn = 'R'
384000051013     c                   if        gnpdif=915
384100051013     c                             or gnpdif=9152
384200051013     c                   eval      DGNPDMC915 = gnpdmc
384300051013     c                   movel     'D'           flgdis            1
384400051013     c                   end
384500051013     c                   if        gnpdif=917
384600051013     c                   eval      DGNPDMC917 = gnpdmc
384700051013     c                   movel     'F'           flgdis            1
384800060317     c                   movel     d917persona   vref
384900051013     c                   end
385000051013     c                   end
385100051220      **
385200051220      **  Non deve scrivere la riga che riporta la dicitura:
385300051220      **    "DATA DISPOS.CLIENTE:"
385400051220     c                   if        %subst(GNPDMC:1:20) <> 'DATA DISPOS.CLIENTE:'
385500051013     C                   MOVEL     GNPDMC        VDMC
385600051013     C                   ADD       1             NRR               3 0
385700051013     C                   WRITE     LG20S05
385800051220     c                   end
385900051220      **
386000970625     C* SE NR.PROGR. =999 E DESCRIZ. INIZIA CON §§ PRENDO L'IMPORTO
386100970625     C* DELLA VARIA R CALCOLATA DALLA PARTENZA SE DISPOSIZ. RIENTRO
386200970625     C*
386300051013     C     VFRI          IFEQ      'R'
386400970625     C     GNPNPR        ANDEQ     999
386500970625     C                   MOVEL     GNPDMC        §VR               2
386600970625     C     §VR           IFEQ      '§§'
386700970625     C                   MOVE      GNPDMC        §VARR
386800970625     C                   END
386900051013     C                   END
387000970625     C                   END
387100970625     C                   END
387200970625     C     NRR           IFEQ      0
387300970625     C                   MOVEL     CNOTA         VDMC
387400970625     C                   ADD       1             NRR               3 0
387500970625     C                   WRITE     LG20S05
387600970625     C                   END
387700940308     C*
387800961203     C                   WRITE     LG20C05
387900940308     C     ENDNOT        ENDSR
388000940308     C* PULIZIA SUBFILE
388100940308     C     PULIZ         BEGSR
388200940308     C                   SETON                                        70
388300940915     C                   WRITE     LG20C05
388400940308     C                   Z-ADD     0             NRR
388500940308     C                   SETOFF                                       70
388600940308     C                   ENDSR
388700920201     C/SPACE 3
388800920201     C*---------------------------------------------------------------*
388900920201     C*   A Z Z E R                                                   *
389000920201     C*---------------------------------------------------------------*
389100920201     C     AZZER         BEGSR
389200920201     C                   SETOFF                                       262730
389300920201     C                   SETOFF                                       313233
389400920228     C                   SETOFF                                       398788
389500920201     C                   SETOFF                                       353637
389600920201     C                   SETOFF                                       808182
389700950120     C                   SETOFF                                       832346
389800141118     C                   SETOFF                                       922034
389900940309     C                   SETOFF                                       99
390000940304     C*
390100990930     C                   CLEAR                   DBLP
390200990930     C                   CLEAR                   DTASV
390300051013     C                   CLEAR                   flgdis
390400920201     C*
390500141118     C                   MOVE      *BLANK        wrecsp
390600920217     C                   MOVE      *BLANK        §§KCVB
390700050405     C                   MOVE      0             spearr            1 0
390800940304     C                   CLEAR                   DSARBK
390900920217     C                   MOVE      *BLANK        §§TCVB
391000990930     C                   CLEAR                   DARBT
391100940304     C                   CLEAR                   DSARBD
391200920201     C*
391300950120     C                   MOVEL     'S'           VSIFAT
391400920201     C                   MOVEL     *ZEROS        VRCA
391500920201     C                   MOVEL     *ZEROS        VRTR
391600920201     C                   MOVEL     *ZEROS        VDSD
391700920201     C                   MOVEL     *ZEROS        VDSA
391800920201     C                   MOVEL     *ZEROS        VGGS
391900920201     C                   MOVEL     *ZEROS        VSGF
392000940325     C                   MOVEL     *BLANK        SGD$              1
392100940325     C                   MOVEL     *BLANK        SGF$              1
392200940325     C                   MOVEL     *BLANK        SGR$              1
392300060113     C                   MOVEL     *BLANK        sgfaf$            1
392400920201     C                   MOVEL     *ZEROS        VTSGS
392500920201     C                   MOVEL     *ZEROS        VSGR
392600920201     C                   MOVEL     *ZEROS        VSGD
392700920201     C                   MOVEL     *ZEROS        VSGA
392800991028     C                   MOVEL     §GEDCN        V6CDIV
392900991028     C                   MOVEL     §GEDCN        V6CRVA
393000991028     C                   MOVEL     §GEDCN        V6CRTA
393100990628     C                   MOVEL     *BLANK        VRSD
393200941011     C                   MOVEL     *BLANK        VRD2
393300920201     C                   MOVEL     *BLANK        VIND
393400920201     C                   MOVEL     *BLANK        VLOD
393500941217     C                   MOVEL     *BLANK        VCAD
393600920201     C                   MOVEL     *BLANK        VPRD
393700920201     C                   MOVEL     *ZEROS        VLNA
393800920201     C                   MOVEL     *BLANK        VDLNA
393900941216     C                   MOVE      *ZEROS        §ANTEP
394000031119     C                   MOVE      *BLANK        Vteld
394100031119     C                   MOVE      *BLANK        Vref
394200030807     C* ----------------------------
394300030807     C* IMPOSTA LA TABELLA DEI tipi record di ar5 da duplicare
394400030807     C* ----------------------------
394500030807     C                   MOVEL     'AR5'         TBECOD
394600030807     C                   Z-ADD     0             bb                3 0
394700030807     C                   Z-ADD     0             cc                3 0
394800030807     c                   clear                   rec
394900030807     c                   clear                   fig
395000030807     C     TBECOD        setll     TNTBE01L
395100030807     C                   DO        *hival
395200030807     C     TBECOD        READE     TNTBE01L
395300030807     c                   if        %eof(tntbe01l)
395400030807     c                   leave
395500030807     c                   end
395600030807     C     TBEATB        IFEQ      *BLANK
395700030807     C                   MOVEL     TBEUNI        DAR5
395800030807     c* aggiungo nella schiera FIG i tipi record da duplicare per le nuove
395900030807     c* bolle figlie a seconda delle varie disposizioni
396000030807     c                   select
396100030807     c                   when      vfri = 'D' and §ar5dupd = 'S'
396200030807     C                   ADD       1             bb
396300030807     C                   MOVEL     tbeke1        fig(bb)
396400030807     c                   when      vfri = 'R' and §ar5dupr = 'S'
396500030807     C                   ADD       1             bb
396600030807     C                   MOVEL     tbeke1        fig(bb)
396700030807     c                   when      vfri = 'C' and §ar5dupc = 'S'
396800030807     C                   ADD       1             bb
396900030807     C                   MOVEL     tbeke1        fig(bb)
397000030807     C                   ENDsl
397100030807     c* aggiungo nella schiera REC i tipi record da duplicare per le nuove
397200030807     c* bolle di recupero
397300030807     c                   if        §ar5drec = 'S'
397400030807     C                   ADD       1             cc
397500030807     C                   MOVEL     tbeke1        rec(cc)
397600030807     C                   END
397700030807     C                   END
397800030807     C*
397900030807     C                   ENDDO
398000920201     C                   ENDSR
398100911025     C*----------------------------------------------------*
398200911025     C*
398300911025     C*     R I E N T R I
398400911025     C*
398500911025     C*----------------------------------------------------*
398600911025     C     RIENTR        BEGSR
398700911114     C*
398800951107     C* NON PREVISTO  ADDEBITO SPESE GIACENZA AL DESTINAT.
398900951107     C*
399000920128     C* IN OGNI CASO IL C/ASSEGNO  DEVE ESSERE ANNULLATO
399100920324     C* SE è POSSIBILE TRASFORMO IN CONTO SERVIZIO
399200920129     C     VASS          IFGT      *ZEROS
399300920128     C* ANNULLO C/A CAMBIO TIPO BOLLA
399400911115     C                   SETON                                          80
399500920220     C                   MOVEL     'KB'          §CDVAR
399600920128     C                   END
399700920220     C     §CDVAR        IFEQ      'KB'
399800920220     C                   EXSR      CNTVAR
399900920220     C  N26
400000920220     CANN27              MOVEL     §CDVAR        §§KCVB
400100920220     C                   END
400200920324     C*
400300920324     C                   MOVEL     §3ATB1        NWTB1
400400920324     C* TRASFORMO IN C/S  SE ERA ASSEGNATO
400500920324     C     EXPORT        IFEQ      'A'
400600950726     C     WEXTB1        ANDNE     'AS'
400700920324     C                   MOVEL     'AS'          NWTB1
400800920324     C                   SETON                                          8039
400900920324     C                   END
401000951107     C*
401100920324     C                   MOVEL     §3ATB2        NWTB2
401200920324     C* ANNULLO C/A  SE ESISTE LA 2° BOLLA LA ELIMINO
401300920324     C     §3ATB2        IFEQ      'A3'
401400920505     C     §3ATB2        OREQ      'AP'
401500920324     C                   MOVEL     '  '          NWTB2
401600920324     C                   SETON                                          8039
401700920324     C                   END
401800920128     C     *IN80         IFEQ      '1'
401900920325     C*
402000941215     C                   MOVE      *ZEROS        §BKCAS
402100941219     C                   MOVEL     *BLANK        §BKVCA
402200920325     C* CONTROLLO SE VARIAZIONE è POSSIBILE
402300920325     C                   MOVEL     'K8'          §CDVAR
402400920325     C                   EXSR      CNTVAR
402500920325     C* CERCO NUOVO CODICE BOLLA
402600920325     C  N26
402700920325     CANN27              DO
402800071009     C                   MOVEL     §CDVAR        §§KCVB
402900920128     C                   MOVEL     '0'           NWFCA
403000920325     C                   MOVEL     §3AFAN        NWFAN
403100911115     C                   MOVEL     §3AFTD        NWFTD
403200920201     C                   MOVEL     *BLANK        NWCBL
403300920201     C                   MOVEL     *BLANK        §BKCBN
403400920201     C                   EXSR      NEWCBL
403500920201     C  N30              MOVEL     NWCBL         §BKCBN
403600920325     C                   END
403700911025     C                   END
403800920128     C* SE ESISTE 2°BOLLA E NON POSSO TRASF. IN C/S : N.ACCREDITO
403900991001     C   27AR6NFT        IFNE      *ZEROS
404000991001     C     AR6TRC        ANDEQ     '2'
404100950726     C     ARBICA        ANDNE     'Y'
404200920128     C                   SETON                                        37
404300991001     C                   Z-ADD     AR6IMV        §INAC2
404400920128     C                   END
404500920128     C* NON HO TRASFORMATO IN C/S RICHIEDO N.ACCRED. SE FATT.IN ARRIVO
404600940928     C   26AR6NFT        IFNE      *ZEROS
404700991001     C     AR6TRC        ANDEQ     '1'
404800940928     C     AR6FIV        ANDEQ     ARBLNA
404900950726     C     ARBICA        ANDNE     'Y'
405000920128     C                   SETON                                        32
405100940928     C                   Z-ADD     AR6IMV        §INAC1
405200920128     C                   END
405300911114     C* CREAZ. BOLLA DI RIENTRO A2
405400911114     C* --------------------------
405500911025     C                   SETON                                        31
405600911025     C*
405700941003     C                   MOVEL     '2'           §LPCBO
405800960327     C*  Se reso in conto servizio imposto 'A' nel codice bolla
405900960327     C     V4CRCS        IFEQ      'S'
406000960327     C                   MOVEL     'A'           §LPCBO
406100960327     C                   END
406200971113     C* INVERTO LP CON LA
406300971113     C                   MOVE      ARBLNP        §LPLNA
406400911113     C                   MOVE      ARBLNA        §LPLNP
406500150518     C*
406600950427     C                   Z-ADD     ARBAAS        KAAS
406700950427     C                   Z-ADD     ARBLNP        KLNP
406800950427     C                   Z-ADD     ARBNRS        KNRS
406900950427     C                   Z-ADD     ARBNSP        KNSP
407000950427     C     KBOL          CHAIN     FNLBL01L                           04
407100061026      ** salva la chiave da riutilizzare x Rientri DPD di bolle dirottate
407200061026      **  per poter riagganciare il Parcel dalla Bolla Originale da cui
407300061026      **  prendere il Depot x agganciare anagrafica indirizzo a cui rispedire
407400061026     C                   clear                   LBLAAOsv
407500061026     C                   clear                   LBLLPOsv
407600061026     C                   clear                   LBLNROsv
407700061026     C                   clear                   LBLNSOsv
407800061026     c                   if        %Found(FNLBL01L)
407900061026     C                   z-add     LBLAAO        LBLAAOsv
408000061026     C                   z-add     LBLLPO        LBLLPOsv
408100061026     C                   z-add     LBLNRO        LBLNROsv
408200061026     C                   z-add     LBLNSO        LBLNSOsv
408300061026     c                   end
408400061026      **
408500950427     C     *IN04         IFEQ      '0'
408600950427     C     LBLLPO        ANDNE     0
408700960315     C     LBLLPO        ANDNE     ARBLNA
408800150521     C     LBLLPO        ANDNE     §LPLNA
408900150518     C                   MOVE      LBLLPO        §LPLNA
409000950427     C                   END
409100150518      *****
409200150518     c*  Se su AZORG c'è la forzatura per la LNA viene sostituita la
409300150518     c*    LNA con quella di OG143 §ogLNAres
409400150518     c                   exsr      CHG_LNA_xFORZA
409500150518     C*
409600150518     C                   MOVE      §LPLNA        VLNA
409700150518      *****
409800080528     C**   VLNA          IFEQ      105
409900971113     C* SE SDI = 105 FORZO 005
410000080528     C**                 MOVE      005           §LPLNA
410100080528     C**                 MOVE      005           VLNA
410200080528     C**                 END
410300911218     C* STAMPA SEGNACOLLI SI - C. TRATT. MERCE  DEFAULT FILIALE
410400140610     c                   exsr      srctm2k
410500940310     C* IMPOSTO §LPNOT
410600940310     C                   EXSR      XLPNOT
410700960327     C*----------------------------------------------------*
410800960327     C* se no reso in C/Servizio
410900960327     C     V4CRCS        IFNE      'S'
411000960327     C*----------------------------------------------------*
411100000405     C* SPESE GIACENZA
411200000405     C     $GIAC         IFEQ      '1'
411300000405     C     GCPSGS        ADD       GCPSGR        §TOTSP
411400000405     C     GCPSGD        ADD       §TOTSP        §TOTSP
411500000405     C     GCPSGA        ADD       §TOTSP        §TOTSP
411600000405     C                   END
411700010524     C*IMPOSTO CODICE PER RICERCA LUOGO
411800010524     C                   Z-ADD     ARBKSC        WCODIC
411900911114     C* SE ERA ASSEGNATO TRASPORTO VA COME ANTEPORTO
412000911114     C     EXPORT        IFEQ      'A'
412100010522     C                   Z-ADD     88888888      §ANTEP
412200010524     C                   Z-ADD     ARBCCM        WCODIC
412300000405     C                   END
412400911025     C* MITTENTE
412500911113     C                   MOVEL     ARBRSD        §LPRSM
412600911113     C                   MOVEL     ARBIND        §LPINM
412700911113     C                   MOVE      ARBCAD        §LPCAM
412800911113     C                   MOVEL     ARBLOD        §LPLOM
412900911113     C                   MOVEL     ARBPRD        §LPPRM
413000940928     C                   MOVEL     ARBNZD        §LPNZM
413100070919      * imposto destinatario
413200070919     c
413300070919     C                   MOVEL     ARBPRM        §LPPRD
413400070919     C                   MOVE      ARBCAM        §LPCAD
413500070919     C                   MOVE      ARBCAM        VCAD
413600070919     C                   MOVEL     ARBPRM        VPRD
413700010522     C* DESTINATARIO IMPOSTO I DATI STANDARD MA POI CONTROLLO FORZATURA
413800010522     C* SE NELLA FILIALE È GESTITO IL LUOGO 300 IMPOSTO I DATI DI
413900010522     C* DESTINAZIONE DAL FNSPE SE PRESENTE PER IL CLIENTE
414000070809      * ?Se non esiste il luogo e il cliente è un codificato ed è impostato*/
414100070807      * ?RMO prende i dati da CNACO                                         */
414200071002     c                   if        wcodic <> 0
414300070810     c                   clear                   cod4              4 0
414400071002     c                   move      wcodic        cod4
414500070808     c                   if        arbrmo <> *blank and
414600070808     c                             (cod4 <> 9999 and cod4 <> 8888)
414700070808     c                   exsr      repcnaco
414800070919     c                   endif
414900070919     c                   endif
415000070919      *
415100010522     C                   MOVEL     ARBRSM        §LPRSD
415200010522     C                   MOVEL     *BLANK        §LPRD2
415300010522     C                   MOVEL     ARBINM        §LPIND
415400010522     C                   MOVEL     ARBLOM        §LPLOD
415500010522     C                   MOVEL     ARBNZM        §LPNZD
415600010522     C*
415700010522     C                   MOVEL     ARBRSM        VRSD
415800010522     C                   MOVEL     *BLANK        VRD2
415900010522     C                   MOVEL     ARBINM        VIND
416000010522     C                   MOVEL     ARBLOM        VLOD
416100010522     C                   MOVEL     ARBNZM        VNZD
416200010522      * SE GESTITO LUOGO 300 IN FILIALE CERCO LUOGO PER IL DESTINATARIO
416300010522     C     KFNSPE        CHAIN     FNSPE01L                           21
416400041221      * prima di impostare i dati del luogo verifico che non ci siano
416500041221      * delle eccezioni per cliente linea di partenza
416600070322      * pulisco comunque il campo del giro forzato
416700070322     c                   clear                   §lpcgi
416800070903     c                   setoff                                       55
416900041221     c                   if        not *in21
417000041221     c                   exsr      ecceluogo
417100041221     c                   endif
417200010522      * SE TROVO IL LUOGO IMPOSTATO LO FORZO RISPETTO ALL'ORIGINALE
417300010522     C     *IN21         IFEQ      '0'
417400070903     C  n55              MOVEL     *BLANK        §LPLNA
417500010522     C                   MOVEL     SPERAG        §LPRSD
417600010522     C                   MOVEL     *BLANK        §LPRD2
417700010522     C                   MOVEL     SPEIND        §LPIND
417800010522     C                   MOVE      SPECAP        §LPCAD
417900010522     C                   MOVEL     SPELOC        §LPLOD
418000010522     C                   MOVEL     SPEPRO        §LPPRD
418100010522     C                   MOVEL     SPENAZ        §LPNZD
418200010522     C*
418300070903     C  n55              MOVEL     *BLANK        VLNA
418400010522     C                   MOVEL     SPERAG        VRSD
418500010522     C                   MOVEL     *BLANK        VRD2
418600010522     C                   MOVEL     SPEIND        VIND
418700010522     C                   MOVE      SPECAP        VCAD
418800010522     C                   MOVEL     SPELOC        VLOD
418900010522     C                   MOVEL     SPEPRO        VPRD
419000010522     C                   MOVEL     SPENAZ        VNZD
419100010524     C                   SETON                                        46
419200010524     C                   ENDIF
419300911113     C*
419400911113     C* IMPOSTO IL CODICE FINE GIACENZA
419500941217     C     $GIAC         IFEQ      '1'
419600911121     C                   MOVEL     '002'         VCFG
419700911114     C                   MOVEL     '2C'          TBLCOD
419800911114     C                   MOVEL     *BLANK        TBLKEY
419900911114     C                   MOVEL     VCFG          TBLKEY
420000911114     C     KTAB          CHAIN     TABEL                              21
420100911114     C   21              MOVEL     *ALL'?'       DCFG
420200060410     C  N21              MOVEL     TBLUNI        Ds2c
420300060410     C  N21              MOVEL     §2cdes        DCFG
420400941012     C                   MOVE      GMAUDT        VDCG
420500960327     C                   ENDIF
420600960327     C*----------------------------------------------------*
420700960327     C                   ELSE
420800960327     C*  Reso in C/Servizio
420900960327     C*  Mittente: Filiale Lna bolla
421000960328     C     ARBLNA        CHAIN     AZORG01L                           21
421100960327     C                   MOVEL     ORGDES        §LPRSM
421200960327     C                   MOVEL     ORGIND        §LPINM
421300960328     C                   MOVEL     ORGCPF        §LPCAM
421400960327     C                   MOVEL     ORGLOC        §LPLOM
421500960327     C                   MOVEL     ORGPRO        §LPPRM
421600960327     C                   MOVEL     *BLANKS       §LPNZM
421700960327     C*  Destinatario: Filiale Lnp bolla originale
421800960328     C     VLNA          CHAIN     AZORG01L                           21
421900960327     C                   MOVEL     ORGDES        §LPRSD
422000960327     C                   MOVEL     *BLANK        §LPRD2
422100960327     C                   MOVEL     ORGIND        §LPIND
422200960328     C                   MOVEL     ORGCPF        §LPCAD
422300960327     C                   MOVEL     ORGLOC        §LPLOD
422400960327     C                   MOVEL     ORGPRO        §LPPRD
422500960327     C                   MOVEL     *BLANKS       §LPNZD
422600960327     C*  Seconda note: Ragione sociale mittente originale
422700960327     C                   MOVEL     ARBRSM        §LPNT2
422800960327     C*
422900960327     C                   MOVEL     ORGDES        VRSD
423000960327     C                   MOVEL     *BLANK        VRD2
423100960327     C                   MOVEL     ORGIND        VIND
423200960328     C                   MOVEL     ORGCPF        VCAD
423300960327     C                   MOVEL     ORGLOC        VLOD
423400960327     C                   MOVEL     ORGPRO        VPRD
423500960327     C                   MOVEL     *BLANKS       VNZD
423600960327     C                   ENDIF
423700911025     C                   ENDSR
423800070807      *_______________________________________________________
423900070807     c     repcnaco      begsr
424000070807      *_______________________________________________________
424100070807     c                   move      wcodic        i69kac
424200070807     c                   move      wcodic        i69kin
424300070807     C                   CALL      'TIBS69R'
424400070807     C                   PARM                    tibs69DS
424500070807     C                   PARM                    DS_cnaco
424600070807     C                   PARM                    DS_cnind
424700070807     C                   PARM                    DS_cnclp
424800070807     C                   PARM                    DS_fncls
424900070807      *
425000070807     C                   if        O69ERR <> '1'
425100070807     c                   movel     indcae        §lpcad
425200070807     c                   movel     indprv        §lpprd
425300070807     c                   end
425400070807     C*
425500070807     C                   MOVE      indcae        VCAD
425600070807     C                   MOVEL     indprv        VPRD
425700070807     C                   Endsr
425800041221      *----------------------------------------------------
425900041221     c     ecceluogo     begsr
426000041221      *----------------------------------------------------
426100041221     c                   move      'PP'          wtip
426200041221     c     ksp2          chain     fnsp201l
426300041221      * rek eccezioni partenza
426400041221     c                   if        %found(fnsp201l)
426500041221     c                   movel     sp2est        dpar
426600041221     c                   z-add     1             xi                3 0
426700070322     c                   z-add     0             xxi               3 0
426800061027      * se ho trovato il legame 04=*off cerco con la linea bolla originale
426900070322      * altrimenti con la linea bolla
427000061027     c  n04lbllpo        lookup    par(xi)                                55
427100061027     c   04arblnp        lookup    par(xi)                                55
427200041221      * se esiste linea di partenza cerco eccezione in arrivo corrispondente
427300041221     c                   if        *in55
427400041221     c                   move      'PA'          wtip
427500041221     c     ksp2          chain     fnsp201l
427600041221     c                   if        %found(fnsp201l)
427700041221     c                   movel     sp2est        darr
427800041221     c                   move      arr(xi)       vlna
427900041221     c                   move      arr(xi)       §lplna
428000041221     c                   endif
428100060807      * cerco anche la zona
428200060807     c                   move      'PZ'          wtip
428300060807     c     ksp2          chain     fnsp201l
428400060807     c                   if        %found(fnsp201l)
428500060807     c                   movel     sp2est        dzon
428600060807     c                   move      zon(xi)       §lpznc
428700060807     c                   endif
428800070322      *reperisco il rekord x il giro
428900070322      * gli eventuali giri forzati sono per ragioni di spazio scritti su 4
429000070322      * rekord quindi x reperire quello giusto è necessario individuare su
429100070322      * quale rekord puntare basandosi sull'indice della linea di partenza
429200070322      * ogni rekord contiene 6 giri quindi per ricavare l'indice corretto
429300070322      * è necessario sottrarre all'indice gli interi precedenti
429400070322     c                   select
429500070322     c                   when      xi < 7
429600070322     c                   move      'G1'          wtip
429700070322     c                   z-add     0             xxi
429800070322     c                   when      xi < 13
429900070322     c                   move      'G2'          wtip
430000070322     c                   z-add     6             xxi
430100070322     c                   when      xi < 19
430200070322     c                   move      'G3'          wtip
430300070322     c                   z-add     12            xxi
430400070322     c                   when      xi > 18
430500070322     c                   move      'G4'          wtip
430600070322     c                   z-add     18            xxi
430700070322     c                   endsl
430800070322     c     ksp2          chain     fnsp201l
430900070322     c                   if        %found(fnsp201l)
431000070322     c                   movel     sp2est        dgir
431100070322     c                   eval      §lpcgi = gir(xi - xxi)
431200070322     c                   endif
431300070322
431400041221      *non trovata linea partenza come eccezione verifico se esiste la linea
431500041221      * 999 nel caso non devo impostare il luogo ma forzo la LNP originale
431600041221     c                   else
431700041221     c                   z-add     1             xi
431800041221     c     999           lookup    par(xi)                                55
431900041221     c                   endif
432000041221      * se esistono eccezioni 55*on forzo indicatore per non impostare
432100041221      * i riferimenti del luogo 300
432200070807      * ?dal 07/08/2007 non più valido se il luogo esiste va impostatocampo */
432300070807      * ?sempre                                                             */
432400070807     c*mm55              seton                                        21
432500041221
432600041221     c                   endif
432700041221     c                   endsr
432800070509      *____________________________________________________
432900070509     c     ecceluogo2    begsr
433000070509      *____________________________________________________
433100070509     c                   move      'PA'          wtip
433200070509     c     ksp2          chain     fnsp201l
433300070509      * rek eccezioni arrivo
433400070509     c                   if        %found(fnsp201l)
433500070509     c                   movel     sp2est        darr
433600070509     c                   z-add     1             xi                3 0
433700070509     c                   z-add     0             xxi               3 0
433800070509      * cerca con linea a video
433900070509     c     vlna          lookup    arr(xi)                                55
434000070509      * se esiste linea di arrivo cerco eventuale personalizzazione giro
434100070509     c                   if        *in55
434200070509      *reperisco il rekord x il giro
434300070509      * gli eventuali giri forzati sono per ragioni di spazio scritti su 4
434400070509      * rekord quindi x reperire quello giusto è necessario individuare su
434500070509      * quale rekord puntare basandosi sull'indice della linea di partenza
434600070509      * ogni rekord contiene 6 giri quindi per ricavare l'indice corretto
434700070509      * è necessario sottrarre all'indice gli interi precedenti
434800070509     c                   select
434900070509     c                   when      xi < 7
435000070509     c                   move      'G1'          wtip
435100070509     c                   z-add     0             xxi
435200070509     c                   when      xi < 13
435300070509     c                   move      'G2'          wtip
435400070509     c                   z-add     6             xxi
435500070509     c                   when      xi < 19
435600070509     c                   move      'G3'          wtip
435700070509     c                   z-add     12            xxi
435800070509     c                   when      xi > 18
435900070509     c                   move      'G4'          wtip
436000070509     c                   z-add     18            xxi
436100070509     c                   endsl
436200070509     c     ksp2          chain     fnsp201l
436300070509     c                   if        %found(fnsp201l)
436400070509     c                   movel     sp2est        dgir
436500070509     c                   eval      §lpcgi = gir(xi - xxi)
436600070509     c                   endif
436700070509
436800070509     c                   endif
436900070509
437000070509     c                   endif
437100070509     c                   endsr
437200911025     C*----------------------------------------------------*
437300911025     C*
437400911025     C*     R I C O N S E G N A
437500911025     C*
437600911025     C*----------------------------------------------------*
437700911025     C     RICONS        BEGSR
437800090729      * se c'è contrassegno e non è abilitato attivo contatto
437900090729     C     §3AFCA        IFNE      0
438000090729     C                   IF        arbacc <> 'S'
438100090729     C                   exsr      contatto
438200090729     C                   endif
438300090729     C                   endif
438400911112     C*-------------------------
438500911026     C* NON  C'E CAMBIO DI PORTO
438600911026     C*-------------------------
438700920116     C     VNPC          IFEQ      ' '
438800941010     C                   EXSR      RICNOC
438900911026     C                   ELSE
439000941010     C*-------------------------
439100941010     C* C'E CAMBIO DI PORTO
439200941010     C*-------------------------
439300941010     C                   EXSR      RICSIC
439400941010     C                   END
439500950112     C* SE GIACENZA VISULIZZO  DATA FINE GIAC.
439600950120     C*  E MI POSIZIONO SUL CODICE FINE GIACENZA
439700950118     C     $GIAC         IFEQ      '1'
439800950118     C                   MOVEL     '   '         VCFG
439900950118     C                   MOVE      GMAUDT        VDCG
440000950120     C                   SETON                                        23
440100950118     C                   END
440200920203     C* SE SPESE GIAC. O PROVV.C/A O NUOVO PORTO ERA O E'=A
440300940307     C* DEVO CHIEDERE SE RICH. PGM TASSAZIONE
440400920107     C* SE SI VADO IN CONSEGNA CON NUOVA BOLLA : LA LEGO
440500920203     C   31VPSG          IFEQ      'D'
440600920107     C     VPPC          OREQ      'D'
440700920107     C     VNPC          OREQ      'A'
440800920203     C     VNPC          OREQ      ' '
440900920203     C     EXPORT        ANDEQ     'A'
441000920107     C                   SETON                                        87
441100920107     C                   END
441200911025     C                   ENDSR
441300941010     C*----------------------------------------------------*
441400941010     C*     R I C O N S E G N A - NO CAMBIO PORTO
441500941010     C*----------------------------------------------------*
441600971006     C     RICNOC        BEGSR
441700941010     C*  ADDEBITO SPESE GIACENZA AL DESTINAT.
441800941010     C     VPSG          IFEQ      'D'
441900941010     C                   SETON                                        33
442000941010     C                   END
442100941010     C*
442200950727     C     WEXTB1        IFEQ      'AS'
442300950727     C                   SETON                                        31
442400950727     C                   END
442500941010     C* C/ASSEGNO DEVO VEDERE SE C'ERA E CHI PAGAVA
442600941010     C*
442700941010     C     §3AFCA        IFNE      0
442800941010     C     §3AFCA        IFEQ      1
442900960506     C                   MOVEL     §3ATB1        §FA               1
443000941010     C                   ELSE
443100941010     C                   MOVEL     §3ATB2        §FA               1
443200941010     C                   END
443300941010     C*
443400160304     C***  §FA           IFEQ      'F'
443500160304     C***  §3ATB2        ANDNE     'AP'
443600160304     C***  §3ATB2        ANDNE     '  '
443700160304     C***  §FA           orNE      'F'
443800160304     c
443900160304     C     §FA           IFNE      'F'
444000941010     C     §3ATB1        ANDNE     'AS'
444100941010     C                   SETON                                        97
444200941010     C                   END
444300941010     C*
444400941010     C                   ELSE
444500941010     C                   SETOFF                                       97
444600941010     C                   END
444700941010     C*  CALCOLO PROVV. C/ASSEGNO SE PAGA DESTINAT.
444800941010     C*                           -----------------
444900941010     C                   MOVE      *ZEROS        EXRCA
445000941010     C                   MOVE      *ZEROS        §RCA
445100941010     C                   MOVE      *ZEROS        VRCA
445200941010     C     VAPC          IFEQ      'S'
445300941010     C     VPPC          ANDEQ     'D'
445400941219     C* IMPOSTO KEY X POSIZ. SU TARIFFA
445500941010     C                   EXSR      TARPCA
445600941219     C*
445700941010     C* SE PAGAVA IL DESTINATARIO  IMPORTO VECCHIO E NUOVO
445800941010     C     *IN97         IFEQ      '1'
445900950523     C     VASS          IFNE      *ZEROS
446000950523     C                   MOVEL     'V'           WCMB              1
446100950523     C                   Z-ADD     VASS          §CAS
446200950523     C                   EXSR      CASLIR
446300950523     C*  REPERISCO IMPORTO VECCHIA VARIA G
446400950523     C                   Z-ADD     0             EXRCA
446500950523     C* SPOSTO PROVV. CONTR. SU NUOVA BOLLA
446600991001     C     §3AFCA        IFNE      0
446700991001     C     AR6DFT        IFNE      0
446800991001     C                   Z-ADD     1             YY                4 0
446900991001     C     §$2VRG        LOOKUP    ASV(YY)                                05
447000991001     C     *IN05         IFEQ      '1'
447100991001     C                   Z-ADD     AVA(YY)       EXRCA
447200991001     C                   END
447300950523     C                   ELSE
447400971203     C                   MOVEL     *ALL'9'       EXRCA
447500991001     C                   ENDIF
447600991001     C                   ENDIF
447700950523     C                   ENDIF
447800950523     C* CALCOLO DIFFERNZA ANCHE SE NON C'ERA TASSAZIONE
447900971203     C     EXRCA         IFNE      *ALL'9'
448000941010     C     VVCAP         IFEQ      VVCA
448100941010     C                   MOVEL     'V'           WCMB
448200941010     C                   ELSE
448300941010     C                   MOVEL     'N'           WCMB
448400941010     C                   END
448500950523     C     VNCA          IFNE      *ZEROS
448600941010     C                   Z-ADD     VNCA          §CAS
448700950523     C                   END
448800941010     C                   EXSR      CASLIR
448900941010     C                   EXSR      CPROVV
449000950522     C     §RCA          SUB       EXRCA         WRCA
449100950522     C                   Z-ADD     §RCA          VRCA
449200971203     C                   END
449300941010     C                   ELSE
449400941010     C* NON PAGAVA NESSUNO O PAGAVA IL MITTENTE
449500941010     C* ---------------------------------------
449600941010     C     VNCA          IFGT      0
449700941010     C                   Z-ADD     VNCA          §CAS
449800941010     C                   ELSE
449900941010     C                   Z-ADD     VASS          §CAS
450000941010     C                   END
450100941010     C     VVCAP         IFEQ      VVCA
450200941010     C                   MOVEL     'V'           WCMB
450300941010     C                   ELSE
450400941010     C                   MOVEL     'N'           WCMB
450500941010     C                   END
450600941010     C                   EXSR      CASLIR
450700941010     C                   EXSR      CPROVV
450800941010     C                   MOVE      §RCA          VRCA
450900941010     C                   END
451000950522     C     VRCA          IFNE      0
451100971006     C* Per bolla in franco + C/Ass con addebito provv. detinat.
451200971006     C* se non ho variato l'importo del C/Assegno non devo accend.
451300971006     C* indicatore per addebito provviggioni
451400971203     C                   MOVEL     §3ATB1        WFANEW            1
451500971006     C     §FA           IFEQ      'F'
451600971006     C     §3ATB2        ANDNE     'AP'
451700971006     C     §3ATB2        ANDNE     '  '
451800971006     C     VVCS          ANDNE     'S'
451900971006     C                   ELSE
452000941010     C                   SETON                                        35
452100941010     C                   END
452200971006     C                   END
452300941010     C                   END
452400941010     C*
452500941010     C*  G E S T I O N E   T I P O     B O L L A
452600941010     C*      VARIAZIONE C/ASSEGNO
452700941010     C     VVCS          IFEQ      'S'
452800941215     C                   MOVE      VNCA          §BKCAS
452900991221     C     VNCA          IFGT      0
453000941219     C                   MOVE      VVCA          §BKVCA
453100991221     C                   ELSE
453200040506     C                   MOVEL     *BLANKS       vvca
453300991221     C                   MOVEL     *BLANKS       §BKVCA
453400991221     C                   END
453500140317      * forzato codice K8 PER VARIAZIONE SOLE SE CONTRASSEGNO AGGIUNTO
453600140317      * CODICE BOLLA DA 1 A 4  O DA 2 A  6
453700140317     c                   if        vass = 0
453800140318     C                   MOVEL     'K8'          §CDVAR
453900140317     c                   else
454000140318     C                   MOVEL     'KB'          §CDVAR
454100140317     c                   endif
454200941010     C                   ELSE
454300941215     C                   MOVE      VASS          §BKCAS
454400941219     C                   MOVE      VVCAP         §BKVCA
454500941010     C                   END
454600941010     C     §CDVAR        IFEQ      'KB'
454700941010     C                   EXSR      CNTVAR
454800941010     C  N26
454900941010     CANN27              MOVEL     §CDVAR        §§KCVB
455000941010     C  N26
455100941010     CANN27              SETON                                        80
455200941010     C                   END
455300941010     C* GUARDO SE DEVO VARIARE TIPO BOLLA  PRIMA IMPOSTO COME ERA
455400941010     C                   MOVEL     §3ATB1        NWTB1
455500941010     C                   MOVEL     §3ATB2        NWTB2
455600941010     C                   MOVEL     §3AFCA        NWFCA
455700941010     C                   MOVEL     §3AFAN        NWFAN
455800941010     C                   MOVEL     §3AFTD        NWFTD
455900941010     C* CONTROLLO ANCHE SE LO POSSO FARE
456000941010     C                   MOVEL     'K8'          §CDVAR
456100941010     C                   EXSR      CNTVAR
456200941010     C* MI RITORNANO INDICATORI  26 O 27
456300941010     C*                  VARIAZ. TIPO BOLLA
456400941010     C     VVCS          IFEQ      'S'
456500941010     C*  ANNULLAMENTO DEL CONTRASSEGNO
456600941010     C     VNCA          IFEQ      *ZEROS
456700941010     C     VVCA          ANDEQ     *BLANK
456800950224     C  N26
456900950224     CANN27              MOVEL     '0'           NWFCA
457000941010     C  N26
457100941010     CANN27              MOVEL     §CDVAR        §§KCVB
457200941010     C  N26
457300941010     CANN27              SETON                                        80
457400941010     C     NWTB2         IFEQ      'A3'
457500950727     C     NWTB2         OREQ      'AP'
457600941010     C  N27              MOVEL     '  '          NWTB2
457700941010     C  N27              SETON                                        39
457800941010     C* LA 2à BOLLA NON è MODIFICABILE RICH. NOTA ACCREDITO
457900950727     C     ARBICA        IFNE      'Y'
458000991001     C     AR6TRC        ANDEQ     '2'
458100941010     C   27              SETON                                        37
458200991001     C   27              Z-ADD     AR6IMV        §INAC2
458300941010     C                   END
458400950727     C                   END
458500941010     C                   END
458600950102     C* AGGIUNTO IL CONTRASSEGNO CAMBIO CODICE BOLLA SEMPRE
458700941010     C     VNCA          IFGT      *ZEROS
458800941010     C     §3AFCA        ANDEQ     0
458900950102     C     EXPORT        IFEQ      'F'
459000941010     C     VPPC          ANDEQ     'D'
459100941010     C                   MOVEL     '2'           NWFCA
459200941010     C                   MOVEL     'AP'          NWTB2
459300941010     C                   MOVEL     §CDVAR        §§KCVB
459400941010     C                   SETON                                        8039
459500950102     C                   ELSE
459600941010     C                   MOVEL     '1'           NWFCA
459700951108     C     NWFTD         IFEQ      *BLANKS
459800941010     C                   MOVEL     *BLANK        NWTB2
459900951108     C                   END
460000941010     C                   MOVEL     §CDVAR        §§KCVB
460100941010     C                   SETON                                        80
460200941010     C                   END
460300941010     C                   END
460400941010     C                   END
460500950102     C* SE CAMBIA CHI PAGA LE SPESE E IL C/ASS
460600950102     C* ERA SU UN  FRANCO  VEDO SE CAMBIO COD.BOLLA
460700941010     C     EXPORT        IFEQ      'F'
460800941010     C     §3AFCA        IFEQ      1
460900941010     C     VPPC          ANDEQ     'D'
461000000104     C* LO TRASFORMO IN COD.BOLLA  Y PERCHE' IN CASO DI
461100000104     C* RICONSEGNA CON LA CREAZIONE DI UNA BOLLA LEGATA
461200000104     C* LA GESTIONE DEGLI INCASSI SI ASPETTA DI AVERE IL
461300000104     C* C/ASS. SULLA BOLLA MAMMA E L'UNICO TIPO BOLLA
461400000104     C* MAMMA CHE NON PREVEDE ADDEBITO DI PROVVIGIONI DI
461500000104     C* PER SE STESSA E UN F1+AP
461600000104.    C                   MOVEL     '2'           NWFCA
461700000104     C                   MOVEL     'AP'          NWTB2
461800941010     C  N26
461900941010     CANN27              MOVEL     §CDVAR        §§KCVB
462000941010     C                   SETON                                        8039
462100941010     C                   END
462200941010     C     §3AFCA        IFEQ      2
462300941010     C     §3ATB2        ANDNE     'AP'
462400941010     C     VPPC          ANDEQ     'M'
462500941010     C     §3AFCA        OREQ      2
462600941010     C     §3ATB2        ANDNE     'AP'
462700941010     C     *IN33         ANDEQ     '1'
462800971006     C     VVCS          ANDNE     'S'
462900971006     C     §3AFCA        OREQ      2
463000971006     C     §3ATB2        ANDNE     'AP'
463100971006     C     *IN33         ANDEQ     '1'
463200971006     C     VNCA          ANDNE     *ZEROS
463300941010     C     *IN27         IFEQ      '0'
463400000104     C* LO TRASFORMO IN COD.BOLLA  Y PERCHE' IN CASO DI
463500000104     C* RICONSEGNA CON LA CREAZIONE DI UNA BOLLA LEGATA
463600000104     C* LA GESTIONE DEGLI INCASSI SI ASPETTA DI AVERE IL
463700000104     C* C/ASS. SULLA BOLLA MAMMA E L'UNICO TIPO BOLLA
463800000104     C* MAMMA CHE NON PREVEDE ADDEBITO DI PROVVIGIONI DI
463900000104     C* PER SE STESSA E UN F1+AP
464000941010     C                   MOVEL     'AP'          NWTB2
464100941010     C                   MOVEL     §CDVAR        §§KCVB
464200941010     C                   SETON                                        8039
464300941010     C                   ELSE
464400941010     C* LA 2à BOLLA NON è MODIFICABILE RICH. NOTA ACCREDITO
464500971006     C     ARBICA        IFNE      'Y'
464600991001     C     AR6TRC        ANDEQ     '2'
464700971006     C                   SETON                                        37
464800991001     C                   Z-ADD     AR6IMV        §INAC2
464900941010     C                   END
465000950727     C                   END
465100971006     C* Se il Destinatario pagava le provvigioni di C/Assegno e le
465200971006     C* paga tutt'ora se non riesco a variare la tassazione chiudo
465300971006     C* la vecchia bolla con nota di accredito
465400971006     C                   ELSE
465500971006     C  N37
465600971006     CAN 35§3AFCA        IFEQ      2
465700971006     C     §3ATB2        ANDNE     'AP'
465800971006     C     VPPC          ANDEQ     'D'
465900971006     C     *IN27         ANDEQ     '1'
466000991001     C     AR6TRC        ANDEQ     '2'
466100971006     C                   SETON                                        37
466200991001     C                   Z-ADD     AR6IMV        §INAC2
466300941010     C                   END
466400971006     C                   END
466500950102     C                   END
466600941010     C* RICERCA NUOVAO TIPO BOLLA
466700941010     C     *IN80         IFEQ      '1'
466800941010     C                   MOVEL     *BLANK        NWCBL
466900941010     C                   MOVEL     *BLANK        §BKCBN
467000941010     C                   EXSR      NEWCBL
467100941010     C  N30              MOVEL     NWCBL         §BKCBN
467200941010     C                   END
467300941010     C*
467400950102     C* SE E' ASSEGNATO VEDO SE POSSO VARIARE TASSAZIONE BOLLA
467500950102     C* ===============
467600950726     C                   MOVEL     ' '           WNOSPG            1
467700950102     C* PER SPESE GIACENZA O PROVVIGIONI C/A
467800950102     C* O AZZERO VARIAG SE C/ASS. ANNULLATO
467900950102     C     EXPORT        IFEQ      'A'
468000950102     C*          -----------------
468100950726     C*  SE NON HO SPESE DI GIACENZA CONTROLLO SE LE AVEVO ADDEBITATE
468200950726     C*  NEI GIRI PRECEDNETNI X ANDARE A STORNARLE
468300950726     C     *IN33         IFEQ      '0'
468400950726     C     §$2VRI        LOOKUP    VSV                                    21
468500950726     C   21              MOVEL     'S'           WNOSPG
468600950726     C                   END
468700950102     C     *IN33         IFEQ      '1'
468800950102     C     *IN35         OREQ      '1'
468900950102     C     VPPC          OREQ      'M'
469000950726     C     WNOSPG        OREQ      'S'
469100950102     C     VVCS          OREQ      'S'
469200950102     C     VNCA          ANDEQ     0
469300950102     C     VVCA          ANDEQ     *BLANK
469400991029     C     AR6NFT        IFEQ      0
469500950102     C                   MOVEL     'T0'          §CDVAR
469600950102     C                   ELSE
469700950102     C                   MOVEL     'T2'          §CDVAR
469800950102     C                   END
469900950102     C                   EXSR      CNTVAR
470000950727     C*  SE BOLLA ORIGINALE E' UN ASSEGNATO STORNO NON POSSO VARIARE
470100950727     C*  TASSAZIONE MA DEVO FARE UNA NUOVA BOLLA
470200950727     C  N26WEXTB1        IFEQ      'AS'
470300950727     C                   SETON                                        26
470400950727     C                   END
470500950727     C  N27WEXTB2        IFEQ      'AP'
470600950727     C                   SETON                                        27
470700950727     C                   END
470800950727     C*  CONTROLLO SE ESISTE UNA FIGLIA
470900950727     C  N26
471000950727     CANN27KBOL          CHAIN     FNLBL02L                           29
471100950727     C     *IN29         IFEQ      '0'
471200991001     C     AR6NFT        IFNE      *ZEROS
471300991001     C     AR6TRC        ANDEQ     '2'
471400950727     C                   SETON                                        27
471500950727     C                   ELSE
471600950727     C                   SETON                                        26
471700950727     C                   END
471800950727     C                   END
471900950102     C  N26
472000950102     CANN27              MOVEL     §CDVAR        §§TCVB
472100950102     C  N26
472200950102     CANN27              SETON                                        82
472300950102     C                   END
472400950102     C* SISTEMO PROVV. C/A  SUBITO
472500950102     C   82              DO
472600950102     C* CARICO LE VARIE ESISTENTI X DSARBT
472700950102     C                   MOVE      AVA           VVA
472800950102     C                   MOVEL     ASV           VSV
472900950102      * CERCO SE C'è GIA VARIA G
473000950102     C                   Z-ADD     1             X
473100950102     C     §$2VRG        LOOKUP    VSV(X)                                 21
473200950102      * TROVATA VARIA G
473300950102     C     *IN21         IFEQ      *ON
473400950522     C     WRCA          ADD       VVA(X)        VVA(X)
473500950102     C     VPPC          IFEQ      'M'
473600950102     C     VVCS          OREQ      'S'
473700950102     C     VNCA          ANDEQ     0
473800950102     C     VVCA          ANDEQ     *BLANK
473900950102     C                   MOVE      *ZEROS        VVA(X)
474000950102     C                   END
474100950102     C                   ELSE
474200950726      * NON TROVATA VARIA G
474300950726     C     VRCA          IFNE      0
474400040505     C     Vnca          andgt     0
474500950102     C                   Z-ADD     1             X
474600950102     C     ' '           LOOKUP    VSV(X)                                 21
474700950102     C     *IN21         IFEQ      *ON
474800950102     C                   Z-ADD     VRCA          VVA(X)
474900950102     C                   MOVEL     §$2VRG        VSV(X)
475000950102     C                   END
475100950726     C                   END
475200950726     C*
475300950102     C                   END
475400950102     C                   ENDDO
475500950102     C                   ELSE
475600950102     C* SE NON E' ASSEGNATO VEDO SE ESISTE 2° BOLLA
475700950102     C* ===================
475800950102     C* VARIO TASSAZ, 2° BOLLA :  CON PROVV. C/A SE NON HO SPESE GIAC.
475900950102   4 C     §3AFCA        IFEQ      2
476000950102     C     §3ATB2        ANDNE     'AP'
476100950102   3 C     *IN35         IFEQ      '1'
476200950102     C     *IN33         ANDEQ     '0'
476300991001   2 C     AR6NFT        IFEQ      0
476400950102     C                   MOVEL     '20'          §CDVAR
476500950102     C                   ELSE
476600950102     C                   MOVEL     '22'          §CDVAR
476700950102   2 C                   END
476800950102     C                   EXSR      CNTVAR
476900950102   2 C     *IN27         IFEQ      '0'
477000991001     C                   MOVEL     §CDVAR        §§TCVB
477100950102     C                   SETON                                        83
477200991001     C                   MOVE      AVA           VVA
477300991001     C                   MOVEL     ASV           VSV
477400991001      * CERCO SE C'è GIA VARIA G
477500991001     C                   Z-ADD     1             X
477600991001     C     §$2VRG        LOOKUP    VSV(X)                                 21
477700991001      * TROVATA VARIA G
477800991001     C     *IN21         IFEQ      *ON
477900991001     C     WRCA          ADD       VVA(X)        VVA(X)
478000991001     C     VPPC          IFEQ      'M'
478100991001     C     VVCS          OREQ      'S'
478200991001     C     VNCA          ANDEQ     0
478300991001     C     VVCA          ANDEQ     *BLANK
478400991001     C                   MOVE      *ZEROS        VVA(X)
478500991001     C                   END
478600991001     C                   ELSE
478700991001      * NON TROVATA VARIA G
478800991001     C     VRCA          IFNE      0
478900991001     C                   Z-ADD     1             X
479000991001     C     ' '           LOOKUP    VSV(X)                                 21
479100991001     C     *IN21         IFEQ      *ON
479200991001     C                   Z-ADD     VRCA          VVA(X)
479300991001     C                   MOVEL     §$2VRG        VSV(X)
479400991001     C                   END
479500991001     C                   END
479600991001     C*
479700991001     C                   END
479800950102   2 C                   END
479900950102   3 C                   END
480000950102   4 C                   END
480100950102     C                   END
480200941010     C                   SETON                                        46
480300941010     C                   MOVEL     ARBRSD        VRSD
480400941011     C                   MOVEL     AR4RD2        VRD2
480500941010     C                   MOVEL     ARBIND        VIND
480600941010     C                   MOVEL     ARBCAD        VCAD
480700941010     C                   MOVEL     ARBPRD        VPRD
480800941010     C                   MOVEL     ARBLOD        VLOD
480900961203     C                   MOVEL     ARBNZD        VNZD
481000031119     c* imposto dati referente destinatario
481100031119     c                   movel     §ar5teld      vteld
481200031119     c                   movel     §ar5ref       vref
481300960124     C*  Salvo dati anagrafici destinatario x controllare se devo
481400060213     C*  scrivere FiAR4 x variazione
481500960124     C                   MOVEL     ARBRSD        WSVRSD
481600960124     C                   MOVEL     AR4RD2        WSVRD2
481700960124     C                   MOVEL     ARBIND        WSVIND
481800960124     C                   MOVEL     ARBCAD        WSVCAD
481900960124     C                   MOVEL     ARBPRD        WSVPRD
482000960124     C                   MOVEL     ARBLOD        WSVLOD
482100961203     C                   MOVEL     ARBNZD        WSVNZD
482200941010     C*
482300941010     C* SE FARE NUOVA BOLLA PER RECUPERO SPESE
482400941010     C*         ------------------------------
482500941010     C* SE HO PROVV. C/A O SPESE GIACENZA E NON HO VARIATO TASSAZ.
482600941010     C* DEVO FARE NUOVA BOLLA
482700941010     C     *IN33         IFEQ      '1'
482800941010     C     *IN35         OREQ      '1'
482900941010     C  N82
483000941010     CANN83              SETON                                        31
483100941010     C                   END
483200960506     C*
483300941010     C     *IN31         IFEQ      '1'
483400941010     C* CREAZ. BOLLA A3
483500941010     C                   MOVEL     'AR'          NWTB1
483600941010     C                   SETON                                        88
483700941010     C                   MOVEL     '  '          NWTB2
483800941010     C     VNCA          IFGT      0
483900941010     C     VVCS          OREQ      'N'
484000941010     C     VASS          ANDGT     0
484100941010     C                   MOVEL     '1'           NWFCA
484200941010     C                   ELSE
484300941010     C                   MOVEL     '0'           NWFCA
484400941010     C                   END
484500941010     C                   MOVEL     '0'           NWFAN
484600941010     C                   MOVEL     '0'           NWFTD
484700941010     C                   MOVEL     *BLANK        NWCBL
484800941010     C                   MOVEL     *BLANK        §LPCBO
484900941010     C                   EXSR      NEWCBL
485000941010     C  N30              MOVEL     NWCBL         §LPCBO
485100941010     C*  LP
485200941010     C                   MOVE      ARBLNA        §LPLNP
485300941010     C* STAMPA SEGNACOLLI  - C. TRATT. MERCE
485400941010     C                   MOVEL     '4 '          §LPCTM
485500941010     C                   MOVE      ARBNCD        §LPNCD
485600941010     C                   MOVE      ARBNCA        §LPNCA
485700941010     C* CONTRASSEGNO
485800941010     C     VNCA          IFGT      0
485900941010     C                   Z-ADD     VNCA          §LPCAS
486000941010     C                   MOVEL     VVCA          §LPVCA
486100991221     C     §LPCAS        IFEQ      0
486200991221     C                   MOVEL     *BLANKS       §LPVCA
486300991221     C                   END
486400941010     C                   MOVEL     AR9TIC        §LPTIC
486500941010     C                   ELSE
486600941010     C     VASS          IFGT      0
486700941010     C     VVCS          ANDEQ     'N'
486800941010     C                   Z-ADD     VASS          §LPCAS
486900000103     C                   MOVEL     VVCAP         §LPVCA
487000991221     C     §LPCAS        IFEQ      0
487100991221     C                   MOVEL     *BLANKS       §LPVCA
487200991221     C                   END
487300941010     C                   MOVEL     AR9TIC        §LPTIC
487400941010     C                   END
487500941010     C                   END
487600941010     C* IMPOSTO §LPNOT
487700941010     C                   EXSR      XLPNOT
487800941010     C*SE ASSEGNATO E RICH. NOTA ACCREDITO IMPOSTO I VALORI VECCH.BOLLA
487900950727     C     EXPORT        IFEQ      'A'
488000950727     C     *IN26         ANDEQ     '1'
488100950727     C     WEXTB1        OREQ      'AS'
488200941010     C                   MOVE      AR6POR        §LPPOR
488300990930      * RIPORTO LE VARIE DELLA VECCHIA BOLLA X DTASV
488400941216     C                   MOVE      AVA           NVA
488500941216     C                   MOVEL     ASV           NSV
488600950522     C                   Z-ADD     1             YY                4 0
488700991001     C* Tolgo Iva e bollo dalle nuove varie
488800991001     C                   Z-ADD     1             YY                4 0
488900991001     C     §$2BOL        LOOKUP    NSV(YY)                                05
489000991001     C     *IN05         IFEQ      '1'
489100991001     C                   MOVEL     *BLANKS       NSV(YY)
489200991001     C                   Z-ADD     0             NVA(YY)
489300991001     C                   END
489400991001     C                   Z-ADD     1             YY                4 0
489500991001     C     §$2IVA        LOOKUP    NSV(YY)                                05
489600991001     C     *IN05         IFEQ      '1'
489700991001     C                   MOVEL     *BLANKS       NSV(YY)
489800991001     C                   Z-ADD     0             NVA(YY)
489900991001     C                   END
490000991001     C     §$2VRG        LOOKUP    NSV(YY)                                05
490100950522     C     *IN05         IFEQ      '1'
490200950508     C     NWFCA         IFEQ      '0'
490300950508     C                   MOVEL     *BLANKS       NSV(YY)
490400950508     C                   Z-ADD     0             NVA(YY)
490500950522     C                   ELSE
490600950522      * SE NON HO CALCOLATO LE PROVVIGIONI C/ASS. MA LE DEVE PAGARE IL
490700950522      * DESTINATARIO MI CARICO L'IMPORTO DALLA VECCHIA BOLLA
490800950522     C     VRCA          IFEQ      0
490900950522     C     VPPC          ANDEQ     'D'
491000950522     C                   Z-ADD     NVA(YY)       VRCA
491100950522     C                   END
491200950522     C                   END
491300950508     C                   END
491400941010     C* RICH. NOTA ACCR. SOLO SE FIL.ARRIVO
491500941010     C     AR6FIV        IFEQ      ARBLNA
491600950727     C     ARBICA        ANDNE     'Y'
491700941010     C                   Z-ADD     AR6IMV        §INAC1
491800941010     C                   SETON                                        32
491900941010     C                   END
492000941010     C                   END
492100941010     C                   END
492200941010     C                   ENDSR
492300941010     C*----------------------------------------------------*
492400941010     C*     R I C O N S E G N A - SI CAMBIO PORTO
492500941010     C*----------------------------------------------------*
492600941010     C     RICSIC        BEGSR
492700941010     C*  ADDEBITO SPESE GIACENZA AL DESTINAT.
492800941010     C     VPSG          IFEQ      'D'
492900941010     C                   SETON                                        33
493000941010     C                   SETON                                        31
493100941010     C                   END
493200941010     C*  CALCOLO PROVV. C/ASSEGNO SE PAGA DESTINAT.
493300941010     C     VAPC          IFEQ      'S'
493400941010     C     VPPC          ANDEQ     'D'
493500941010     C                   EXSR      TARPCA
493600941010     C* VARIATO C/ASS.
493700941010     C     VVCS          IFEQ      'S'
493800941010     C                   Z-ADD     VNCA          §CAS
493900941010     C                   ELSE
494000941010     C                   Z-ADD     VASS          §CAS
494100941010     C                   END
494200941010     C     VVCAP         IFEQ      VVCA
494300941010     C                   MOVEL     'V'           WCMB
494400941010     C                   ELSE
494500941010     C                   MOVEL     'N'           WCMB
494600941010     C                   END
494700941010     C                   EXSR      CASLIR
494800941010     C     §ASS          IFNE      *ZEROS
494900941010     C                   EXSR      CPROVV
495000941010     C                   MOVE      §RCA          VRCA
495100941010     C     VRCA          IFGT      0
495200941010     C                   SETON                                        35
495300941010     C                   SETON                                        31
495400941010     C                   END
495500941010     C                   END
495600941010     C                   END
495700941010     C* RICHIEDERE IL TRASPORTO SE NUOVO PORTO = A
495800941010     C     VNPC          IFEQ      'A'
495900060118     C**mm               SETON                                        36
496000941010     C                   SETON                                        31
496100941010     C                   END
496200941010     C* VARIATO C/ASS.    SEMPRE VARIAZ. X GEST. C/A
496300941010     C     VVCS          IFEQ      'S'
496400941215     C                   MOVE      VNCA          §BKCAS
496500941219     C                   MOVE      VVCA          §BKVCA
496600991221     C     VNCA          IFEQ      0
496700991221     C                   MOVEL     *BLANKS       §BKVCA
496800040205     C                   MOVEL     *BLANKS       vvca
496900991221     C                   END
497000941010     C                   MOVEL     'KB'          §CDVAR
497100941010     C                   ELSE
497200941215     C                   MOVE      VASS          §BKCAS
497300941219     C                   MOVE      VVCAP         §BKVCA
497400941010     C                   END
497500941010     C     §CDVAR        IFEQ      'KB'
497600941010     C                   EXSR      CNTVAR
497700941010     C  N26
497800941010     CANN27              MOVEL     §CDVAR        §§KCVB
497900941010     C  N26
498000941010     CANN27              SETON                                        80
498100941010     C                   END
498200941010     C* GUARDO SE DEVO VARIARE TIPO BOLLA  PRIMA IMPOSTO COME ERA
498300941010     C                   MOVEL     §3ATB1        NWTB1
498400941010     C                   MOVEL     §3ATB2        NWTB2
498500941010     C                   MOVEL     §3AFCA        NWFCA
498600941010     C                   MOVEL     §3AFAN        NWFAN
498700941010     C                   MOVEL     §3AFTD        NWFTD
498800941010     C* CONTROLLO ANCHE SE LO POSSO FARE
498900941010     C                   MOVEL     'K8'          §CDVAR
499000941010     C                   EXSR      CNTVAR
499100941010     C* MI RITORNANO GLI INDICATORI 26 O 27
499200941010     C*                  VARIAZ. TIPO BOLLA
499300941010     C     VVCS          IFEQ      'S'
499400941010     C*  ANNULLAMENTO DEL CONTRASSEGNO
499500941010     C     VNCA          IFEQ      *ZEROS
499600941010     C     VVCA          ANDEQ     *BLANK
499700950727     C  N26
499800950727     CANN27              MOVEL     '0'           NWFCA
499900941010     C  N26
500000941010     CANN27              MOVEL     §CDVAR        §§KCVB
500100941010     C  N26
500200941010     CANN27              SETON                                        80
500300941010     C     NWTB2         IFEQ      'A3'
500400950727     C     NWTB2         OREQ      'AP'
500500950727     C  N27              MOVEL     '  '          NWTB2
500600941010     C  N27              SETON                                        8039
500700941010     C* LA 2à BOLLA NON è MODIFICABILE RICH. NOTA ACCREDITO
500800950727     C     ARBICA        IFNE      'Y'
500900991001     C     AR6TRC        ANDEQ     '2'
501000941010     C   27              SETON                                        37
501100991001     C   27              Z-ADD     AR6IMV        §INAC2
501200941010     C                   END
501300950727     C                   END
501400941010     C                   END
501500941010     C*  AGGIUNTO CONTRASSEGNO
501600941010     C     VNCA          IFGT      *ZEROS
501700941010     C     §3AFCA        ANDEQ     0
501800950102     C                   MOVEL     '1'           NWFCA
501900941010     C  N26
502000941010     CANN27              MOVEL     §CDVAR        §§KCVB
502100941010     C  N26
502200941010     CANN27              SETON                                        80
502300941010     C                   END
502400941010     C                   END
502500941010     C     *IN26         IFEQ      '1'
502600941010     C     AR6FIV        ANDEQ     ARBLNA
502700950727     C     ARBICA        ANDNE     'Y'
502800941010     C                   SETON                                        32
502900941010     C                   Z-ADD     AR6IMV        §INAC1
503000941010     C                   END
503100941010     C* SE NON DEVO FARE NOTE ACCREDITO DEVO TRASFORMARE
503200941010     C*  LA VECCHIA BOLLA IN C/S
503300941010     C  N32EXPORT        IFEQ      'A'
503400950727     C     WEXTB1        ANDNE     'AS'
503500941010     C                   MOVEL     'AS'          NWTB1
503600941010     C  N26
503700941010     CANN27              MOVEL     §CDVAR        §§KCVB
503800941010     C                   SETON                                        3980
503900941010     C                   END
504000941010     C     EXPORT        IFEQ      'F'
504100941010     C                   MOVEL     'FS'          NWTB1
504200941010     C* SE DEVO FARE NUOVA BOLLA E C'ERA A3 LA STORNO
504300971006     C   31§3ATB2        IFEQ      'A3'
504400950727     C  N27              MOVEL     'AP'          NWTB2
504500950727     C     ARBICA        IFNE      'Y'
504600991001     C     AR6TRC        ANDEQ     '2'
504700941010     C   27              SETON                                        37
504800991001     C   27              Z-ADD     AR6IMV        §INAC2
504900941010     C                   END
505000950727     C                   END
505100941010     C  N26
505200941010     CANN27              MOVEL     §CDVAR        §§KCVB
505300941010     C  N26
505400941010     CANN27              SETON                                        3980
505500941010     C                   END
505600941010     C     *IN80         IFEQ      '1'
505700941010     C                   MOVEL     *BLANK        NWCBL
505800941010     C                   MOVEL     *BLANK        §BKCBN
505900941010     C                   EXSR      NEWCBL
506000941010     C  N30              MOVEL     NWCBL         §BKCBN
506100941010     C                   END
506200950727     C*  SE NON HO SPESE DI GIACENZA CONTROLLO SE LE AVEVO ADDEBITATE
506300950727     C*  NEI GIRI PRECEDNETNI X ANDARE A STORNARLE
506400950727     C                   MOVEL     ' '           WNOSPG            1
506500950727     C     *IN33         IFEQ      '0'
506600950727     C     §$2VRI        LOOKUP    VSV                                    21
506700950727     C   21              MOVEL     'S'           WNOSPG
506800950727     C                   END
506900941010     C*  AGGIUNTO CONTRASSEGNO  FORZO VARIAG=0 SU VECCHIA BOLLA
507000941010     C     VNCA          IFGT      *ZEROS
507100941010     C     §3AFCA        ANDEQ     0
507200941010     C     EXPORT        ANDEQ     'A'
507300950727     C     EXPORT        OREQ      'A'
507400950727     C     WNOSPG        ANDEQ     'S'
507500941010     C     AR6NFT        IFEQ      0
507600941010     C                   MOVEL     'T0'          §CDVAR
507700941010     C                   ELSE
507800941010     C                   MOVEL     'T2'          §CDVAR
507900941010     C                   END
508000941010     C                   EXSR      CNTVAR
508100950727     C*  SE BOLLA ORIGINALE E' UN ASSEGNATO STORNO NON POSSO VARIARE
508200950727     C*  TASSAZIONE MA DEVO FARE UNA NUOVA BOLLA
508300950727     C  N26WEXTB1        IFEQ      'AS'
508400950727     C                   SETON                                        26
508500950727     C                   END
508600950727     C  N27WEXTB2        IFEQ      'AP'
508700950727     C                   SETON                                        27
508800950727     C                   END
508900950727     C*  CONTROLLO SE ESISTE UNA FIGLIA
509000950727     C  N26
509100950727     CANN27KBOL          CHAIN     FNLBL02L                           29
509200950727     C     *IN29         IFEQ      '0'
509300991001     C     AR6NFT        IFNE      *ZEROS
509400991001     C     AR6TRC        ANDEQ     '2'
509500950727     C                   SETON                                        27
509600950727     C                   ELSE
509700950727     C                   SETON                                        26
509800950727     C                   END
509900950727     C                   END
510000941010     C  N26
510100941010     CANN27              MOVEL     §CDVAR        §§TCVB
510200941010     C  N26
510300941010     CANN27              SETON                                        82
510400941010     C*
510500941010     C   82              DO
510600941216     C* CARICO LE VARIE ESISTENTI X DSARBT
510700941216     C                   MOVE      AVA           VVA
510800941216     C                   MOVEL     ASV           VSV
510900941010      * CERCO SE C'è GIA VARIA G
511000941216     C                   MOVEL     §$2VRI        §SIGLA            1
511100941216     C                   MOVEL     VSV           WSV
511200941216      * CERCO L'INDICE SU CUI LAVORARE
511300941216     C                   EXSR      CHKVAR
511400941216     C                   Z-ADD     0             VVA(XS)
511500941216     C                   MOVEL     §$2VRI        VSV(XS)
511600941216     C                   END
511700941010     C                   END
511800941010     C* IMPOSTO I CAMPI X NUOVA BOLLA
511900941010     C* -----------------------------
512000941010     C* CREAZ. BOLLA AR
512100941010     C     *IN31         IFEQ      '1'
512200941010     C                   MOVEL     'AR'          NWTB1
512300941010     C                   SETON                                        88
512400941010     C                   ELSE
512500941010     C* SE NUOVO  PORTO = F :
512600941010     C     VNPC          IFEQ      'F'
512700941010     C*              SE NO GIACENZE  CREO BOLLA LEGATA SEMPRE
512800941010     C     $GIAC         IFEQ      '0'
512900941010     C                   SETON                                        8831
513000941010     C                   MOVEL     'FC'          NWTB1
513100941010     C                   ELSE
513200941010     C*              SE SI GIACENZE  CREO BOLLA LEGATA X N.ACCR.
513300941010     C     *IN32         IFEQ      '1'
513400941010     C     *IN37         OREQ      '1'
513500941010     C                   SETON                                        8831
513600941010     C                   MOVEL     'FC'          NWTB1
513700941010     C                   END
513800941010     C                   END
513900941010     C                   END
514000941010     C                   END
514100941010     C                   MOVEL     '  '          NWTB2
514200941010     C     VNCA          IFGT      0
514300941010     C     VVCS          OREQ      'N'
514400941010     C     VASS          ANDGT     0
514500941010     C                   MOVEL     '1'           NWFCA
514600941010     C                   ELSE
514700941010     C                   MOVEL     '0'           NWFCA
514800941010     C                   END
514900941010     C                   MOVEL     '0'           NWFAN
515000941010     C                   MOVEL     '0'           NWFTD
515100941010     C                   MOVEL     *BLANK        NWCBL
515200941010     C                   MOVEL     *BLANK        §LPCBO
515300941010     C                   EXSR      NEWCBL
515400941010     C  N30              MOVEL     NWCBL         §LPCBO
515500941010     C*
515600941010     C                   SETON                                        46
515700941010     C                   MOVEL     ARBRSD        VRSD
515800941011     C                   MOVEL     AR4RD2        VRD2
515900941010     C                   MOVEL     ARBIND        VIND
516000941010     C                   MOVEL     ARBCAD        VCAD
516100941010     C                   MOVEL     ARBPRD        VPRD
516200941010     C                   MOVEL     ARBLOD        VLOD
516300961203     C                   MOVEL     ARBNZD        VNZD
516400031119     c* imposto dati referente destinatario
516500031119     c                   movel     §ar5teld      vteld
516600031119     c                   movel     §ar5ref       vref
516700960124     C*  Salvo dati anagrafici destinatario x controllare se devo
516800060213     C*  scrivere FiAR4 x variazione
516900960124     C                   MOVEL     ARBRSD        WSVRSD
517000960124     C                   MOVEL     AR4RD2        WSVRD2
517100960124     C                   MOVEL     ARBIND        WSVIND
517200960124     C                   MOVEL     ARBCAD        WSVCAD
517300960124     C                   MOVEL     ARBPRD        WSVPRD
517400960124     C                   MOVEL     ARBLOD        WSVLOD
517500961203     C                   MOVEL     ARBNZD        WSVNZD
517600941010     C*  LP
517700941010     C                   MOVE      ARBLNA        §LPLNP
517800941010     C* C/ASS
517900941010     C     VNCA          IFGT      0
518000941010     C                   Z-ADD     VNCA          §LPCAS
518100941010     C                   MOVEL     VVCA          §LPVCA
518200991221     C     §LPCAS        IFEQ      0
518300991221     C                   MOVEL     *BLANKS       §LPVCA
518400991221     C                   END
518500941010     C                   MOVEL     AR9TIC        §LPTIC
518600941010     C                   ELSE
518700941010     C     VVCS          IFEQ      'N'
518800941010     C     VASS          ANDGT     0
518900941010     C                   Z-ADD     VASS          §LPCAS
519000941010     C                   MOVEL     VVCAP         §LPVCA
519100941010C    C                   MOVEL     AR9TIC        §LPTIC
519200941010     C                   END
519300941010     C                   END
519400941010     C*
519500941010     C                   MOVEL     '4 '          §LPCTM
519600941010     C                   MOVE      ARBNCD        §LPNCD
519700941010     C                   MOVE      ARBNCA        §LPNCA
519800941010     C* IMPOSTO §LPNOT
519900941010     C                   EXSR      XLPNOT
520000941010     C                   ENDSR
520100911026     C*----------------------------------------------------*
520200911026     C*
520300940310     C* DIROT1 - DIROTTAMENTO 1 OPERAZ. DA FARE
520400911026     C*
520500911026     C*----------------------------------------------------*
520600940310     C     DIROT1        BEGSR
520700911028     C*  NON PREVISTO ADDEBITO SPESE GIACENZA AL DESTINAT.
520800911028     C*
520900941217     C* DAL NUOVO RELEASE FEBR 95:
521000941217     C* SI DIROTTA SOLO SE LA LINEA ARRIVO è DIVERSA
521100941217     C*
521200911028     C* DEVO RICH. LNA E DESTINATARIO NUOVO
521300911028     C                   SETON                                        46
521400920522     C* PROPONGO VECCHIO DESTINATARIO SENZA IL CAP PER I
521500920522     C* DIROTTAMENTI NON DA GIACENZE
521600940310     C     $GIAC         IFEQ      '0'
521700920522     C                   MOVEL     ARBRSD        VRSD
521800941011     C                   MOVEL     AR4RD2        VRD2
521900920522     C                   MOVEL     ARBIND        VIND
522000920522     C                   MOVEL     ARBPRD        VPRD
522100920522     C                   MOVEL     ARBLOD        VLOD
522200940929     C                   MOVEL     ARBNZD        VNZD
522300031119     c* imposto dati referente destinatario
522400031119     c                   movel     §ar5teld      vteld
522500031119     c                   movel     §ar5ref       vref
522600920522     C                   END
522700960124     C*  Salvo dati anagrafici destinatario x controllare se devo
522800060213     C*  scrivere FiAR4 x variazione
522900960124     C                   MOVEL     ARBRSD        WSVRSD
523000960124     C                   MOVEL     AR4RD2        WSVRD2
523100960124     C                   MOVEL     ARBIND        WSVIND
523200960124     C                   MOVEL     ARBCAD        WSVCAD
523300960124     C                   MOVEL     ARBPRD        WSVPRD
523400960124     C                   MOVEL     ARBLOD        WSVLOD
523500960124     C                   MOVE      ARBNZD        WSVNZD
523600911113     C* IMPOSTO IL CODICE FINE GIACENZA
523700941217     C     $GIAC         IFEQ      '1'
523800911113     C                   MOVEL     '006'         VCFG
523900911114     C                   MOVEL     '2C'          TBLCOD
524000911114     C                   MOVEL     *BLANK        TBLKEY
524100911114     C                   MOVEL     VCFG          TBLKEY
524200911114     C     KTAB          CHAIN     TABEL                              21
524300911114     C   21              MOVEL     *ALL'?'       DCFG
524400060410     C  N21              MOVEL     TBLUNI        Ds2c
524500060410     C  N21              MOVEL     §2cdes        DCFG
524600941229     C                   MOVE      GMAUDT        VDCG
524700941217     C                   ENDIF
524800911026     C                   ENDSR
524900940929      *--------------------------------------------------------
525000940929      * C A S L I R  TRASFORMO C/ASSEGNO IN LIRE
525100940929      *------------
525200940929      * VIENE PASSATO §CAS : IMPORTO CONTRASSEGNO 13,3
525300940929      *               WCMB : SE=V DEVO PRENDERE CAMBIO SULLA BOLLA
525400940929      *                      SE=N è CAMBIATA LA DIVISA CERCO NUOVO CAMBIO
525500940929      * VIENE RESTITUITO §ASS : IMPORTO CONTRASSEGNO IN LIRE 13,0
525600940929      *--------------------------------------------------------
525700940929     C     CASLIR        BEGSR
525800011106      * RICHIAMO YEURCO SE LA DIVISA E' CAMBIATA PER APPLICARE
525900011106      * IL CAMBIO CORRETTO
526000011121     C     VVCA          IFNE      *BLANK
526100011121     C     VVCA          ANDNE     VVCAP
526200040206      *chiodissimo per divise a blank
526300040206     c                   if        vvcap = *blank
526400040206     c                   move      'EUR'         vvcap
526500040206     c                   endif
526600011106     C                   CLEAR                   YEURDS
526700011106     C                   MOVEL     VVCAP         YECDVI
526800011106     C                   Z-ADD     §CAS          YECIMI
526900011106     C                   MOVEL     VVCA          YECDVO
527000011106     C                   MOVEL     '3'           YECDCO
527100011106     C                   MOVEL     'H'           YECTAR
527200011106     C                   CALL      'YEURCO'
527300011106     C                   PARM                    YEURDS
527400011106     C                   Z-ADD     YECIMO        §CAS
527500011106     C                   END
527600011106      * RICHIAMO YEURCO SEMPRE PER TRADURRE L'IMPORTO IN MONETA DI
527700011106      * CONTO GESTIONALE
527800011106     C                   CLEAR                   YEURDS
527900011106     C                   MOVEL     VVCA          YECDVI
528000011106     C                   Z-ADD     §CAS          YECIMI
528100011106     C                   MOVEL     §GEDCN        YECDVO
528200011106     C                   MOVEL     '3'           YECDCO
528300011106     C                   MOVEL     'H'           YECTAR
528400011106     C                   CALL      'YEURCO'
528500011106     C                   PARM                    YEURDS
528600011106     C                   Z-ADD     YECIMO        §ASS
528700940929     C                   ENDSR
528800940308     C/EJECT
528900940308     C*?
529000940308     C*?    A G G I O R N A M E N T I
529100940308     C*?
529200940303      *--------------------------------------------------------
529300940303     C* S R A G G  -AGGIORNAMENTI  ARCHIVI
529400940303      *--------------------------------------------------------
529500940303     C     SRAGG         BEGSR
529600940303      *--------------------------------------------------------
529700940303     C* ADDEBITO SPESE E PASSAGGIO DATI PER CREAZ. BOLLA
529800940303      *--------------------------------------------------------
529900951214     C     *IN31         IFEQ      '1'
530000951214     C                   EXSR      PGNBLP
530100951214     C                   Z-ADD     §LPAAS        WSVAAS
530200951214     C                   Z-ADD     §LPLNP        WSVLNP
530300951214     C                   Z-ADD     §LPNRS        WSVNRS
530400951214     C                   Z-ADD     §LPNSP        WSVNSP
530500951214     C                   Z-ADD     §LPLNA        WSVLNA
530600940303     C* SE NON HO CREATO LA BOLLA ERRORE
530700951214     C     §LPNSP        IFEQ      0
530800940307     C                   SETON                                            99
530900940307     C   99              GOTO      ENDAGG
531000940307     C                   END
531100951214     C                   END
531200940303      *--------------------------------------------------------
531300940303     C* AGGIORNAMENTO VECCHIA BOLLA
531400940303      *--------------------------------------------------------
531500141210     c     rileggi       tag
531600141210     C     KBOL          CHAIN(e)  FNARB01L                           43
531700141210     c                   if        %error
531800141210     C     Kbol          CHAIN(n)  fnarb01l
531900141210     c                   exsr      geslckARB
532000141210     c   43              goto      rileggi
532100150129     C  n43KBOL          CHAIN     FNARB01L                           43
532200141210     c                   endif
532300941003     C     *IN43         IFEQ      '0'
532400960124      *--------------------------------------------------------
532500110427      * verifiche per resi
532600110427     C     VFRI          IFeq      'R'
532700030128      *controllo se la spedizione gestisce i bancali
532800030128      * se sono gestiti reperisco da fiar5 i rekord da scrivere per
532900030128      * legarli alla bolla di reso
533000030128     C                   clear                   dar5ban
533100030805     C                   move      'BAN'         trcar5
533200051013     C     Kfiar5        CHAIN(n)  FiAR501L                           43
533300030128     C     *IN43         IFEQ      '0'
533400030128     c                   clear                   wfmgb000
533500030128     C                   MOVEL     ar5uni        dar5ban
533600030128     c                   z-add     amgudt        wfmdat
533700030128     c                   movel     whhudt        wfmora
533800030128     c                   movel     knmus         wfmute
533900030128     c                   movel     simpou        wfmpou
534000030128     c                   movel(p)  'FNLG20R'     wfmpgm
534100030213     c                   movel     'REME'        wfmcau
534200030218     c                   Clear                   wfmsmc
534300030128     c                   movel     arbnsp        wfmrdo
534400030128     c                   movel     arbaas        wfmddo
534500030128     c                   move      arbmgs        wfmddo
534600030128      *  TIPO PORTO
534700030128     C                   movel     '3A'          tblcod
534800030128     C                   movel(P)  ARBcbo        tblkey
534900060807     C     ktab          chain     TABEL
535000030128     C                   movel     TBLUNI        DS3A
535100030128     C                   movel     §3ATB1        §DCTport          1
535200030128      *verifico il porto per codice cliente
535300030128     c                   if        §dctport = 'F'
535400030218     c                   movel     '0'           wfmsog
535500030218     c                   move      arbksc        wfmsog
535600030128     c                   else
535700030128     c                   if        arbccm > 0
535800030218     c                   movel     '0'           wfmsog
535900030218     c                   move      arbccm        wfmsog
536000030128     c                   else
536100030128     c                   move      *all'0'       arblna4           4
536200030128     c                   move      arblna        arblna4
536300030218     c                   eval      wfmsog  = arblna4 + '8888'
536400030128     c                   endif
536500030128     c                   endif
536600030128      *primo rekord se deve essere scritto
536700030128     c                   if        §ar5tba > 0
536800030128     c                   movel     §ar5tba       wfmcod
536900030128     c                   z-add     §ar5nba       wfmqta
537000030128     c                   write     wfmgb000
537100030128     c                   endif
537200030128      *
537300030128      *secondo rekord se deve essere scritto
537400030128     c                   if        §ar5tb2 > 0
537500030128     c                   movel     §ar5tb2       wfmcod
537600030128     c                   z-add     §ar5nb2       wfmqta
537700030128     c                   write     wfmgb000
537800030128     c                   endif
537900030128      *
538000030128     c                   endif
538100000214     C                   END
538200071025      * DA FUORI GIACENZE
538300071017      * scrive rekord spia per resi e dirottamenti per poterli stampare
538400071017      * in seguito abbinando bolla mamma e figlia
538500071025     c                   if        not *in90
538600071017     c                   if        vfri = 'D' or vfri = 'R'
538700071017     c                   move      vfri          arbtpl
538800071017     C                   Z-ADD     0             ARBDST
538900071017     C                   Z-ADD     FILARR        ARBFGS
539000071017     C                   MOVE      AMGUDT        ARBDDC
539100071017     C                   MOVE      *ALL'9'       ARBNDC
539200071017     c                   write     fnarbx00
539300071017     c                   clear                   arbtpl
539400071017     c                   endif
539500071025     c                   endif
539600960124      *--------------------------------------------------------
539700950331     C                   MOVEL     ARBCBO        EXCBO             2
539800941003     C* SE HO GIà RICH. NOTA ACCRED. NON DEVO STAMPARE RICH.
539900941005     C     ARBFIP        IFEQ      'C'
540000941003     C                   MOVEL     ARBFIP        EXNAC             1
540100941003     C                   END
540200941003     C   32
540300941003     COR 37              MOVEL     'C'           ARBFIP
540400941003     C   32
540500941003     COR 37              MOVEL     'S'           ARBFNC
540600941003     C* SE HO RICH.NOTA ACCRED.  O TRASF. IN C/S
540700950726     C     *IN32         IFEQ      '1'
540800950726     C     *IN37         OREQ      '1'
540900950726     C     *IN39         OREQ      '1'
541000950726     C                   MOVEL     'Y'           ARBICA
541100950726     C                   MOVEL     'S'           ARBACA
541200950726     C                   END
541300941003     C     VFRI          IFEQ      'R'
541400960327     C*  Se reso controllo se per avaria
541500960327     C     V4CRCS        IFEQ      'N'
541600941003     C                   MOVEL     '2'           ARBCCA
541700960327     C                   ELSE
541800960327     C                   MOVEL     '6'           ARBCCA
541900941003     C                   END
542000960327     C                   END
542100941003     C     VFRI          IFEQ      'D'
542200941003     C                   MOVEL     '1'           ARBCCA
542300941003     C                   END
542400941003     C     VFRI          IFEQ      'C'
542500941003     C     VNPC          ANDNE     ' '
542600941003     C                   MOVEL     '9'           ARBCCA
542700941003     C                   END
542800940303     C*  --------------------------->  HO FATTO NUOVA BOLLA
542900940303      * =======================================================
543000941229      * DO PER CONSEGNATA LA MAMMA - X RESO E DIROTTAMENTO
543100940303      * =======================================================
543200940303     C     VFRI          IFEQ      'R'
543300940303     C     VFRI          OREQ      'D'
543400941216     C     VLNA          ANDNE     EXLNA
543500950214     C* Se non c'è data arrivo forzo = data consegna
543600950214     C     ARBDAM        IFEQ      *ZEROS
543700950214     C                   Z-ADD     AMGUDT        ARBDAM
543800950214     C     ARBDTI        IFEQ      *ZEROS
543900950214     C                   Z-ADD     AMGUDT        ARBDTI
544000950214     C                   Z-ADD     ORAMIN        ARBHTI
544100950214     C                   END
544200950214     C                   END
544300940303      *
544400940303     C                   MOVEL     'Y'           ARBICC
544500950725     C                   MOVEL     'S'           ARBACC
544600940303     C                   MOVEL     'Y'           ARBICA
544700950725     C                   MOVEL     'S'           ARBACA
544800940303     C                   MOVE      AMGUDT        ARBDCM
544900940930     C                   MOVEL     ORAMIN        ARBHMC
545000940303     C                   MOVE      *BLANK        ARBFT1
545100941006     C                   MOVE      *ZEROS        ARBDT1
545200091221     c                   clear                   arbpdc
545300021009     C                   MOVEL     FILarr        ARBPDC
545400091221     C                   MOVE      0998          ARBPDC
545500940303     C                   MOVE      *ALL'9'       ARBNDC
545600941003      * LA DATA DELLA DISTINTA è LA DATA DEL DIROTTAMENTO
545700940303     C                   MOVE      AMGUDT        ARBDDC
545800940303     C                   MOVEL     'S'           ARBFDC
545900940303     C                   MOVE      *ALL'9'       ARBNMI
546000940303     C                   MOVE      AMGUDT        ARBDMI
546100940303     C                   MOVE      *ALL'9'       ARBCUC
546200021009     c                   z-add     filarr        arbifp
546300020813     c                   z-add     simpou        arbipp
546400940303     C                   MOVEL     *BLANK        ARBFBC
546500950104     C                   MOVEL     *BLANK        ARBCMC
546600970204     C* SE DIROTTAMENTO AD ALTRA FILIALE IMPOSTO ARBDT3 A 99999999
546700970204     C* E ARBFT3 A T
546800970204     C     VFRI          IFEQ      'D'
546900970204     C                   MOVEL     'T'           ARBFT3
547000970204     C                   Z-ADD     99999999      ARBDT3
547100970204     C                   END
547200940303      *
547300941011     C* DETT.SEGNACOLLI E ANOMALIE NELLO STORICO
547400940303     C                   EXSR      AGGART
547500940303      *
547600941229     C                   UPDATE    FNARB000
547700070320      * se impostato flag abilitazione in anagrafica verifico se esistono
547800070321      * dei blocchi nel caso updato il rekord dopo aver chiuso giacenza
547900070320     c                   if        §oggeot = 'S'
548000070321     c                   exsr      elibloc
548100070320     c                   endif
548200941229     C                   ELSE
548300940303      * ========================================================
548400940303      * IN TUTTI GLI ALTRI CASI NON STORICIZZO LA MAMMA
548500940303      * ========================================================
548600941229     C* SE HO CREATO LA BOLLA IN ARRIVO VADO IN CONSEGNA CON LA NUOVA
548700941229     C     *IN88         IFEQ      '1'
548800941229     C* BLOCCO LA VECCHIA BOLLA :
548900941229     C* SE NON C'E' GIACENZA  PERCHE' NON ESCA
549000941229     C* NELLA PRELISTA BOLLE DA CONSEGNARE
549100941003     C     $GIAC         IFEQ      '0'
549200940310     C                   MOVEL     '*'           ARBFBC
549300941229     C* SE C'E' GIACENZA SE NON E' BLOCCATA LA BLOCCO
549400941229     C                   ELSE
549500941229     C                   MOVEL     'G'           ARBFBC
549600940310     C                   END
549700940308     C* ABILITO LA VECCHIA BOLLA  PERCHE' NON ESCA  DA TASSARE     ££££
549800940308     C                   MOVEL     'S'           ARBACC
549900940308     C                   MOVEL     'S'           ARBACA
550000941229     C                   UPDATE    FNARB000
550100941229     C                   ELSE
550200941229      *                    ----
550300940303     C* SE NON HO CREATO NUOVA BOLLA X ANDARE IN CONSEGNA
550400941229     C* DEVO ABILITARE LA VECCHIA SE NON HA FIGLIE
550500941229     C     KBOL          CHAIN     FNLBL02L                           21
550600941229     C     *IN21         IFEQ      *ON
550700941003     C                   MOVEL     *BLANK        ARBFBC
550800941229     C                   UPDATE    FNARB000
550900941229     C                   ELSE
551000941229     C* SE HA FIGLIE SBLOCCO L'ULTIMA
551100941229     C     *IN21         DOWEQ     *OFF
551200941229     C     LBLLAN        IFEQ      EXLNA
551300941229     C                   MOVE      LBLAAN        ULFAAS
551400941229     C                   MOVE      LBLLPN        ULFLNP
551500941229     C                   MOVE      LBLNRN        ULFNRS
551600941229     C                   MOVE      LBLNSN        ULFNSP
551700941229     C                   END
551800941229     C     KBOL          READE     FNLBL02L                               21
551900941229     C                   ENDDO
552000941229     C     ULFNSP        IFNE      *ZEROS
552100941229     C* BLOCCO LA VECCHIA BOLLA
552200941229     C     $GIAC         IFEQ      '0'
552300941229     C                   MOVEL     '*'           ARBFBC
552400941229     C                   ELSE
552500941229     C                   MOVEL     'G'           ARBFBC
552600941229     C                   END
552700941229     C                   UPDATE    FNARB000
552800941229     C* SBLOCCO L'ULTIMA FIGLIA
552900941229     C     KBOLF         CHAIN     FNARB01L                           21
553000941229     C  N21              MOVEL     *BLANK        ARBFBC
553100960507     C* SE HO AGGIUNTO IL C/ASSEGNO E' HO GIA' UNA FIGLIA FACCIO
553200960507     C* NUOVA BOLLA
553300960507     C     *IN31         IFEQ      '0'
553400960507     C     VNCA          ANDGT     0
553500960507     C     VASS          ANDEQ     0
553600960507     C     *IN21         ANDEQ     '0'
553700960507     C     ARBDCM        ANDEQ     *ZEROS
553800960507      * GUARDO SE è UN ASSEGNATO
553900960507     C                   MOVEL     '3A'          TBLCOD
554000960507     C                   MOVEL     *BLANK        TBLKEY
554100960507     C                   MOVEL     ARBCBO        TBLKEY
554200960507     C     KTAB          CHAIN     TABEL                              18
554300960507     C     *IN18         IFEQ      '0'
554400960507     C                   MOVEL     TBLUNI        DS3A
554500960507     C                   MOVEL     §3ATB1        NWTB1
554600960507     C                   MOVEL     §3ATB2        NWTB2
554700960507     C                   MOVEL     '1'           NWFCA
554800960507     C                   MOVEL     §3AFAN        NWFAN
554900960507     C                   MOVEL     §3AFTD        NWFTD
555000960507     C                   EXSR      NEWCBL
555100960507     C  N30              MOVEL     NWCBL         ARBCBO
555200960507     C                   SETOFF                                       21
555300960507     C                   END
555400960507     C                   END
555500941229     C  N21              UPDATE    FNARB000
555600941229     C                   ELSE
555700941229     C* SE NON HO TROVATO FIGLIE DA SBLOCCARE
555800941229     C* SBLOCCO LA VECCHIA
555900941229     C                   MOVEL     *BLANK        ARBFBC
556000941229     C                   UPDATE    FNARB000
556100941229     C                   ENDIF
556200941229     C                   ENDIF
556300941229     C                   ENDIF
556400950918     C*  Se rimetto in consegna la bolla chiudo anomalie di giacenza
556500950918     C                   EXSR      CLOANM
556600941229     C                   ENDIF
556700941003     C                   ENDIF
556800941005      *--------------------------------------------------------
556900941217      * DIROTTAMENTO/CAMBIO PORTO  CREO EVENTO  SE NO GIACENZE
557000941005      *--------------------------------------------------------
557100941217     C     $GIAC         IFEQ      '0'
557200941005     C     VFRI          IFEQ      'D'
557300941217     C     VFRI          OREQ      'C'
557400941217     C     VNPC          ANDNE     ' '
557500941005     C                   EXSR      EVENTO
557600941005     C                   ENDIF
557700941217     C                   ENDIF
557800940303      *--------------------------------------------------------
557900940303     C*  PRENDO NR. RICH. RECUPERO   SE NO GIACENZE
558000940303      *--------------------------------------------------------
558100940310     C     $GIAC         IFEQ      '0'
558200940303     C     VFRI          IFEQ      'D'
558300941216     C     §LPLNA        ANDNE     EXLNA
558400940303     C     VNPC          ANDEQ     ' '
558500940303     C     EXPORT        ANDEQ     'F'
558600940303     C* CAMBIO PORTO
558700940303     C     VNPC          OREQ      'F'
558800940303     C*
558900021009     c                   eval      kpjbus=kpjbu
559000020813     c                   clear                   trul33ds
559100020813     c                   move      'L'           i33tla
559200020813     c                   z-add     15            i33cnu
559300021009     c                   z-add     filarr        i33po1
559400020813     c                   z-add     1             i33num
559500020813     c                   movel     trul33ds      kpjbu
559600020813     c                   call      'TRUL33R'
559700020813     c                   PARM                    kpjba
559800020813     c                   movel     kpjbu         trul33ds
559900020813     c                   z-add     o33nri        §rirec            7 0
560000021009     c                   eval      kpjbu=kpjbus
560100940303     C                   END
560200940303     C                   END
560300951213      *--------------------------------------------------------
560400951213     C* CREAZ. LEGAME BOLLA CREATA
560500951213      *--------------------------------------------------------
560600951213      * SE HO CREATO NUOVA BOLLA LA LEGO SEMPRE !!!!!
560700951213     C     *IN31         IFEQ      '1'
560800951213     C                   EXSR      LEGBOL
560900951213     C* METTO NELLE NOTE CONSEGNA LA NUOVA SPEDIZIONE
561000951213     C                   EXSR      AGGAR4
561100030826     C* per bolle con particolarità RC (orm contestuale alla consegna)
561200030826     c* chiudo l'orm della bolla originale e aggiorno il po di ritirno con
561300030826     c* il po d'arrivo della bolla originale in modo da far creare in
561400030826     c* automatico il nuovo orm
561500030826     C                   EXSR      AGGORM
561600951213     C                   END
561700951213     C*-------------------------------------------------------
561800951213     C* BOLLA DA CREARE ANCHE IN ARRIVO PER ANDARE IN CONSEGNA
561900951213     C*-------------------------------------------------------
562000951213     C     *IN88         IFEQ      '1'
562100951213     C                   EXSR      ARRBOL
562200951213     C                   ELSE
562300951213     C*  Se non devo creare una nuova bolla controllo se devo
562400951213     C*  ristampare la vecchia (l.avviso) in caso di riconseg.
562500951213     C     VFRI          IFEQ      'C'
562600070412      * procedura di automazione distinte non si deve più stampare la LDV
562700070412     c                   if        §ogdda = ' ' or
562800070424     c                             §ogdda = '20391231' or amgudt < ogddan
562900951213     C                   EXSR      CTRSTB
563000070412     C                   endif
563100951213     C                   END
563200951213     C                   END
563300951213     C*-------------------------------------------------------
563400951213     C* BOLLA NON CREATA IN ARRIVO, MA LINEA ARRIVO è = DEVO STAMPARLA
563500951213     C*-------------------------------------------------------
563600951213     C   31§LPLNA        IFEQ      EXLNA
563700951213     C     *IN88         ANDEQ     '0'
563800951213     C                   EXSR      STABLP
563900951213     C                   END
564000940303      *--------------------------------------------------------
564100940303     C*STAMPO  RICHIESTA RECUPERO
564200940303      *--------------------------------------------------------
564300951212     C     $GIAC         IFEQ      '0'
564400940303     C     VFRI          IFEQ      'D'
564500941216     C     §LPLNA        ANDNE     EXLNA
564600940303     C     VNPC          ANDEQ     ' '
564700940303     C     EXPORT        ANDEQ     'F'
564800940303     C                   SETON                                        84
564900940303     C                   END
565000940303     C* CAMBIO PORTO
565100940303     C     VNPC          IFEQ      'F'
565200951212     C     WEXTB1        OREQ      'FS'
565300951212     C     VNPC          ANDEQ     ' '
565400960126     C     VFRI          IFEQ      'C'
565500960126     C                   SETON                                        91
565600960126     C                   ELSE
565700960126     C                   SETON                                        85
565800940303     C                   END
565900960126     C                   END
566000941221     C* RECUPERO PROVV. C/ASS SE AGGIUNTO C/ASSEGNO E PAGA MITTENTE
566100941221     C     VVCS          IFEQ      'S'
566200941221     C     VNCA          ANDGT     0
566300941221     C     VASS          ANDEQ     0
566400941221     C     VPPC          ANDEQ     'M'
566500941221     C                   SETON                                        89
566600941221     C                   ENDIF
566700960126     C   84
566800960126     COR 85              EXSR      RCHREC
566900951212     C*  Per riconsegne con cambi di porto da assegnato a franco
567000951212     C*  creo una nuova bolla x recupero spese trasporto x lnp
567100960126     C   91              EXSR      RECTRA
567200030617     c                   if        viddcli = 'C' and vfri = 'D'
567300020617     c                   seton                                        92
567400160826      *-
567500020617     c                   exsr      rectra
567600160826      *-
567700160826      * per DPD scrive lo SCAN 06 di DIROTTAMENTO per recuperare le
567800160826      * spese solo se il dirottamento è fra 2 codici DEPOT LNP e LNA
567900160826      * differenti fra loro.
568000160826     c                   exsr      WRI_SCAN06
568100160826      *-
568200020625     c                   setoff                                       92
568300020617     c                   endif
568400960126     C                   SETOFF                                       858489
568500940303     C                   END
568600951212      *--------------------------------------------------------
568700951212     C* RICHIESTA NOTA ACCREDITO
568800951212      *--------------------------------------------------------
568900951212     C   32
569000951212     COR 37EXNAC         IFNE      'C'
569100951212      * STAMPO SOLO SE NON L'HO GIA' RICHIESTA
569200951212     C                   EXSR      RCHNAC
569300951212     C                   END
569400951213     C                   MOVEL     ' '           WRECSP            1
569500960126     C                   SETOFF                                       91
569600940303     C*-------------------------------------------------------
569700940303     C*  AGGIORNA ARCHIVI GIACENZA
569800940303     C*-------------------------------------------------------
569900940303     C     $GIAC         CASEQ     '1'           GCPWTR
570000940304     C                   ENDCS
570100940307     C     ENDAGG        ENDSR
570200160826      *---------------------------------------------------------*
570300160826      *  x DIROTTAMENTO causa DPD scrive  SCAN06 nel TIDP300F
570400160826      *---------------------------------------------------------*
570500160826     c     WRI_SCAN06    begsr
570600160826      *
570700160826     c                   clear                   FNLG28DS
570800160829     c                   eval        i28AAS = WSVAAS
570900160829     c                   eval        i28LNP = WSVLNP
571000160829     c                   eval        i28NRS = WSVNRS
571100160829     c                   eval        i28NSP = WSVNSP
571200160829     c                   eval        i28LNA = WSVLNA
571300160826      *
571400160826     c                   call      'FNLG28R'
571500160826     c                   parm                    FNLG28DS
571600160826      *
571700160826     C                   ENDSR
571800070321      *---------------------------------------------------------*
571900070321      *  elimina blocchi alla consegna da bolla data x consegnata
572000070321      *---------------------------------------------------------*
572100070321     c     elibloc       begsr
572200070321     C                   clear                   dsbl4l
572300070803     C     Kar4l         chain(e)  fiar401l
572400070803      *provvisorio da eliminare con test allocazione
572500070803     c                   if        %error
572600070803     C     Kar4l         chain(N)  fiar401l
572700070803     c                   goto      endbloc
572800070803     c                   endif
572900070321     C                   if        %found(fiar401l)
573000070321     C                   movel     ar4not        dsbl4l
573100070321     c                   move      *blank        §b4lbas
573200070321     c                   move      *blank        §b4lbge
573300070321     c                   move      *blank        §b4ltca
573400070321     c                   move      *blank        §b4ltfd
573500070614     C                   movel     dsbl4l        ar4not
573600070614     c                   update    fiar4000
573700070321     C                   endif
573800070321     c
573900070803     C     endbloc       endsr
574000090729      *---------------------------------------------------------*
574100090729      *  scrive contatto in fase di riconsegna se CA non abilitato
574200090729      *---------------------------------------------------------*
574300090729     c     contatto      begsr
574400090729     C                   clear                   dsbl4l
574500090729     C     Kar4l         chain(e)  fiar401l
574600090729      *provvisorio da eliminare con test allocazione
574700090729     c                   if        %error
574800090729     C     Kar4l         chain(N)  fiar401l
574900090729     c                   goto      endcont
575000090729     c                   endif
575100090729     C                   if        %found(fiar401l)
575200090729     C                   movel     ar4not        dsbl4l
575300090729     c                   move      'S'           §b4ltca
575400090729     C                   movel     dsbl4l        ar4not
575500090729     c                   update    fiar4000
575600090729     c                   else
575700090729     c                   eval      ar4aas = arbaas
575800090729     c                   eval      ar4lnp = arblnp
575900090729     c                   eval      ar4nrs = arbnrs
576000090729     c                   eval      ar4nsp = arbnsp
576100090729     c                   eval      ar4trc = 'L'
576200090729     c                   move      'S'           §b4ltca
576300090729     c                   eval      ar4not = dsbl4l
576400090729     c                   eval      ar4ftr = 'R'
576500090729     c                   eval      ar4dtr = amgudt
576600090729     c                   eval      ar4duv = amgudt
576700090729     c                   write     fiar4000                             99
576800090729     C                   endif
576900090729     c
577000090729     C     endcont       endsr
577100070614      *---------------------------------------------------------*
577200070614      *  Duplica eventuali note LDV presenti in bolla
577300070614      *---------------------------------------------------------*
577400070614     c     dupnotldv     begsr
577500070614     C                   move      'LDV'         cdnk
577600070614     C                   move      'S'           gstk
577700070614     C     Karn          setll     fiarn11l
577800070614     c                   do        *hival
577900070614     C     Karn          reade     fiarn11l
578000070614     C                   if        %eof(fiarn11l)
578100070614     c                   leave
578200070614     c                   endif
578300070614     c                   move      §lpaas        arnaas
578400070614     c                   move      §lplnp        arnlnp
578500070614     c                   move      §lpnrs        arnnrs
578600070614     c                   move      §lpnsp        arnnsp
578700070614     c                   write     fiarn000
578800070614     C                   enddo
578900070614     c
579000070614     C                   endsr
579100070622     C**************************************************************************
579200070622     C*  Routine per gestione richiamo interrogazione bolle
579300070622     C**************************************************************************
579400070622     C     richbl        begsr
579500070622      *
579600070622     c                   movel     kpjbu         §kpjbu
579700070622      *
579800070622     c                   z-add     arbaas        pa1aas
579900070622     c                   z-add     arblnp        pa1lnp
580000070622     c                   z-add     arbnrs        pa1nrs
580100070622     c                   z-add     arbnsp        pa1nsp
580200070622     c                   movel     ' '           pa1f03
580300070622     c                   movel     '1'           pa1flg
580400070622     c                   movel     param1        kpjbu
580500070622     c                   call      'FNLR36R'
580600070622     c                   parm                    kpjba
580700070622     c                   movel     kpjbu         param1
580800070622      *
580900070622     c                   movel     §kpjbu        kpjbu
581000070622      *
581100070622     c                   endsr
581200950918     C*-----------------------------------------------------*
581300950918     C*  Chiudo anomalie 140 su dettaglio segnacolli
581400950918     C*-----------------------------------------------------*
581500950918     C     CLOANM        BEGSR
581600950918     C*
581700950918     C                   Z-ADD     140           KCAA
581800990518     C     KANM1         CHAIN     FNANM01L                           38
581900950918     C     *IN38         DOWEQ     '0'
582000960618     C                   CLEAR                   DSLR33
582100960618     C                   MOVE      '4'           D33FSC
582200960618     C                   Z-ADD     AMGUDT        D33DCH
582300960618     C                   MOVEL     'DI'          D33CCH
582400960618     C                   Z-ADD     ANMNR1        D33NRR
582500960618     C                   CALL      'FNLR33R'
582600960618     C                   PARM                    DSLR33
582700990518     C     KANM1         READE     FNANM01L                               38
582800950918     C                   END
582900950918     C*
583000950918     C                   ENDSR
583100940308     C*---------------------------------------------------------------*
583200940309     C*   P G N B L P  RICH. PGM NUOVA BOLLA                          *
583300940308     C*---------------------------------------------------------------*
583400940309     C     PGNBLP        BEGSR
583500971113      *
583600170323     c                   clear                   wlnpb
583700941003      * IMPOSTO COD. CLIENTE MITTENTE
583800950130     C                   MOVE      EXLNA         §LPLNP
583900000313     C                   MOVE      ARBRMN        §LPRMN
584000000925     C                   MOVE      'N'           DPDSN             1
584100000313     C* Se la nuova bolla ha lnp DPD (EX LNA) imposto il nr.parcel
584200000313     C* nel rifermiento mittente numerico
584300150724     C*   QUINDI è dall'estero verso l'Italia
584400000313     C     §LPLNP        CHAIN     AZORG01L                           21
584500000313     C     *IN21         IFEQ      '0'
584600000313     C     ORGFVA        ANDNE     '*'
584700000313     C                   MOVEL     ORGDE3        OG143
584800020617     C     §OGntw        IFEQ      'DPD'
584900000925     C                   MOVE      'S'           DPDSN
585000150630     c                   end
585100150630     c                   endIF
585200170228     c* Ragionamenti particolari se §lplnp è una DPD
585300170228     c                   if        dpdsn='S'
585400170228     c                   exsr      sr_lnpdpd
585500170323     c**                 if        wlnpb>0
585600170323     c**                 eval      §lplnp=wlnpb
585700170323     c**                 endif
585800170228     c                   endif
585900150630      *
586000150630     c                   clear                   fnlg24ds
586100150630     c                   z-add     Kaas          i24aas
586200150630     c                   z-add     Klnp          i24lnp
586300150630     c                   z-add     Knrs          i24nrs
586400150630     c                   z-add     Knsp          i24nsp
586500150630     c*
586600150630     c                   call      'FNLG24R'
586700150630     C                   parm                    fnlg24ds
586800150630     c*
586900150630     c                   eval      §lpnpr = o24npr
587000150630     c*
587100150724     c* C'è il PARCEL quindi è un DPD:
587200150724     C* ... per dirottamenti e resi DPD, imposta il COD.trattamento merce
587300150724     C*     specifico x DPD.
587400150630     c                   if         o24npr <> *blank
587500000313     C     VFRI          IFEQ      'D'
587600160908     C     *in92         andeq     *Off
587700000313     C     VFRI          OREQ      'R'
587800140711     c                   exsr      srctm2k
587900140711     c                   if        §lpctm<>'2K'
588000000313     C                   MOVEL     '6F'          §LPCTM
588100140711     c                   else
588200140711     c                   movel     '2H'          §LPCTM
588300000313     C                   END
588400140711     C                   END
588500150630     c*
588600151020     c                   endIF
588700151020     c*
588800030131     C* in caso di dirottamento o cambio di porto su riconsegna
588900030131     C*reperisco i bancali e i campi correlati da impostare sulla nuova bolla
589000030131     C                   clear                   dar5ban
589100030131     C                   if        vfri = 'D'  or
589200030131     c                             vfri = 'C' and vnpc <> *blank and
589300030131     c                             vnpc <> export
589400030805     C                   move      'BAN'         trcar5
589500051013     C     Kfiar5        CHAIN(n)  FiAR501L                           43
589600030131     C     *IN43         IFEQ      '0'
589700030131     C                   MOVEL     ar5uni        dar5ban
589800030131     C                   endif
589900030131     c                   z-add     §AR5TBA       §lpTBA
590000030131     c                   z-add     §AR5NBA       §lpNBA
590100030131     c                   z-add     §AR5TB2       §lpTB2
590200030131     c                   z-add     §AR5NB2       §lpNB2
590300030131     C                   endif
590400031124      * Bancali Bartolini
590500031124     c                   If        %Subst(Arbgva:1:1) = 'O' and wrecsp = ' '
590600031124     c                   Clear                   dAr5BNB
590700031124     c                   Clear                   tasban
590800031124     C                   move      'BNB'         trcar5
590900051013     c     kFiar5        Chain(n)  Fiar501l
591000031124     c                   If        %Found(Fiar501l)
591100031124     c                   Movel     Ar5Uni        dAr5BNB
591200031124     c                   EndIf
591300031124     c                   z-add     §AR5bTBA      §lpTBA
591400031124     c                   z-add     §AR5bNBA      §lpNBA
591500031124     c                   z-add     §AR5bTB2      §lpTB2
591600031124     c                   z-add     §AR5bNB2      §lpNB2
591700031124     c                   Eval      §lpcor = §Ar5Bcor
591800031124     c                   Eval      §lpcsf = §Ar5Bcsf
591900031124     c                   EndIf
592000031119      * imposto dati per referente  destinatario  e num telefono
592100031119     C                   if        vfri = 'D'  or
592200031119     c                             vfri = 'C' and vnpc <> *blank and
592300031119     c                             vnpc <> export
592400031119     c                   movel     vref          §lpref
592500031119     c                   movel     vteld         §lptel
592600031119     c                   endif
592700941220     C                   EXSR      AGGCCM
592800940308     C                   MOVEL     AMGUDT        §LPAAS
592900940308     C                   MOVE      AMGUDT        §LPMGS
593000940308     C                   MOVE      *ZEROS        §LPNRS
593100940308     C                   MOVE      *ZEROS        §LPNSP
593200150630      * *****************
593300150630      * RESO___________:
593400150630      * *****************
593500150630      *
593600940308     C     VFRI          IFEQ      'R'
593700941003     C                   MOVE      'S'           §LPFBR
593800950104     C                   MOVEL     *BLANKS       §LPGCA
593900950104     C                   MOVEL     *BLANKS       §LPGGA
594000031125      **---- < ATTENZIONE >  CHIODO PER varia O per i resi
594100031126     c                   If        %Subst(Arbgva:1:1) = 'O'
594200031126     c                   movel     arbgva        §lpgva
594300031126     c                   else
594400030821     C                   MOVEL     *BLANKS       §LPGVA
594500150311     C                   clear                   §lpTBA
594600150311     C                   clear                   §lpNBA
594700150311     C                   clear                   §lpTB2
594800150311     C                   clear                   §lpNB2
594900150311     C                   clear                   §lpcor
595000150311     C                   clear                   §lpcsf
595100031125     c                   endif
595200000925     C                   MOVEL     *BLANKS       §LPGMA
595300000922      *
595400940308     C                   ELSE
595500150630     ******************
595600150630     **  NO RESO______:
595700150630     ******************
595800150630      *
595900950104     C* IMPOSTAZIONE CAMPI GESTIONI VARIE
596000950104     C                   MOVEL     AR9GCA        §LPGCA
596100950104     C                   MOVEL     ARBGGA        §LPGGA
596200030821     C                   MOVEL     ARBGMA        §LPGMA
596300030821     C                   MOVEL     ARBGVA        §LPGVA
596400960205     C*§ ABBLENCO 2°BYTE PARTIC. VARIE
596500960306     C                   MOVE      ' '           §LPGVA
596600110715      * se dirottata bolla di rientro cerca le varie relative alla giacenza prec
596700110715      * le salva per riscriverle sulla bolla nuova solo da fuori giacenza
596800110715     c                   clear                   memovar1         11 3
596900110715     c                   clear                   memovar2         11 3
597000110715     c                   clear                   memovar3         11 3
597100110715     c                   clear                   memova1           1
597200110715     c                   clear                   memova2           1
597300110715     c                   clear                   memova3           1
597400150630     c                   clear                   §LPFBR
597500150511     C* reperisco variabili tipo bolla
597600150511     C                   CLEAR                   DS3A
597700150512     C                   MOVEL     '3A'          TBLCOD
597800150512     C                   MOVEL     *BLANK        TBLKEY
597900150512     C                   MOVEL     §lpCBO        TBLKEY
598000150512     C     KTAB          CHAIN     TABEL                              21
598100150512     C  N21              MOVEL     TBLUNI        DS3A
598200110715
598300940308     C     ARBFBR        IFNE      *BLANK
598400150511     C     §3asva        andeq     *blank
598500940308     C                   MOVE      ARBFBR        §LPFBR
598600150630     c                   if           vfri = 'D' and not *in90
598700110714     c                   if        ar6sv1 = 'I' or ar6sv1 = 'N'
598800110714     c                   z-add     ar6va1        memovar1
598900110715     c                   movel     ar6sv1        memova1
599000110714     c                   endif
599100110714     c                   if        ar6sv2 = 'I' or ar6sv2 = 'N'
599200110714     c                   z-add     ar6va2        memovar2
599300110715     c                   movel     ar6sv2        memova2
599400110714     c                   endif
599500110714     c                   if        ar6sv3 = 'I' or ar6sv3 = 'N'
599600110714     c                   z-add     ar6va3        memovar3
599700110715     c                   movel     ar6sv3        memova3
599800110714     c                   endif
599900110714     c                   endif
600000940308     C                   END
600100150630      * ??????????????
600200961129     C*  Se bolla originale in franco frontiera olla figlia no
600300961129     C     ARBFBR        IFEQ      'F'
600400961129     C                   MOVEL     *BLANKS       §LPFBR
600500961129     C                   END
600600940308     C                   END
600700150630      *****************
600800150630     C* CONSEGNA______:
600900150630      *****************
601000950725     C     VFRI          IFEQ      'C'
601100940308     C                   MOVE      ARBFFD        §LPFFD
601200950725     C                   ELSE
601300950725     C                   MOVE      *BLANKS       §LPFFD
601400950725     C                   END
601500150630      *****************
601600150630      * - X TUTTI_____:
601700150630      *****************
601800940308     C                   MOVE      *BLANK        §LPCPI
601900940308     C                   MOVE      *ZEROS        §LPIMV
602000940308     C                   MOVE      *ZEROS        §LPDFT
602100940308     C                   MOVE      *ZEROS        §LPNFT
602200940308     C                   MOVE      *ZEROS        §LPFIV
602300950120     C                   MOVEL     ARBRMA        §LPRMA
602400941003     C                   MOVEL     ARBRMO        §LPRMO
602500091221     c                   clear                   §LPPDR
602600950118     C                   MOVEL     §LPLNP        §LPPDR
602700091221     C                   MOVE      0998          §LPPDR
602800940308     C                   MOVE      AMGUDT        §LPDRT
602900941003     C                   MOVE      *ZEROS        §LPNRT
603000941011     C                   MOVEL     'P'           §LPFPP
603100050208     C                   MOVE      *blank        §LPXCO
603200150310     C                   IF        §lpcbo <> 'F*' and
603300150310     C                             (vfri = 'C' or vfri = 'D')
603400031110     C                   MOVEL     arbtcr        §LPTCR
603500031110     C                   z-add     arbdcr        §LPDCR
603600031110     C                   z-add     arbhcr        §LPHCR
603700031110     c                   else
603800031110     C                   MOVEL     ' '           §LPTCR
603900940308     C                   MOVE      *ZEROS        §LPDCR
604000940308     C                   MOVE      *ZEROS        §LPHCR
604100031110     c                   endif
604200941221     C* GUARDO SE BOLLA IN C/SERVIZIO AZZERO LE VARIE
604300941221     C                   CLEAR                   DS3A
604400941221     C                   Z-ADD     1             §F                2 0
604500941221     C     §LPCBO        LOOKUP    CDB(§F)                                21
604600941221     C   21              MOVEL     D3A(§F)       DS3A
604700960227     C   21              MOVEL     §3ATB1        WNEWPO            1
604800960227     C   21WNEWPO        IFEQ      'A'
604900960227     C     §3ATB1        ANDNE     'AS'
605000960227     C     §APPO         ANDEQ     8888
605100960227     C                   MOVE      '9999'        §LPCCM
605200960227     C                   MOVE      '9999'        §LPCCN
605300960227     C                   END
605400941221     C     §3ARBL        IFEQ      'C'
605500941221     C                   CLEAR                   NVA
605600941221     C                   CLEAR                   NSV
605700941221     C                   ELSE
605800941216     C* ANTEPORTO
605900941216     C     §ANTEP        IFGT      *ZEROS
606000941216     C                   MOVEL     §$2VRN        §SIGLA            1
606100941216     C                   MOVEL     NSV           WSV
606200941216      * CERCO L'INDICE SU CUI LAVORARE
606300941216     C                   EXSR      CHKVAR
606400941216     C                   Z-ADD     §ANTEP        NVA(XS)
606500941216     C                   MOVEL     §$2VRN        NSV(XS)
606600941216     C                   ENDIF
606700941216     C* SPESE GIACENZA
606800941003     C     §TOTSP        IFGT      *ZEROS
606900941216     C                   MOVEL     §$2VRI        §SIGLA
607000941216     C                   MOVEL     NSV           WSV
607100941216      * CERCO L'INDICE SU CUI LAVORARE
607200941216     C                   EXSR      CHKVAR
607300941216     C                   Z-ADD     §TOTSP        NVA(XS)
607400941216     C                   MOVEL     §$2VRI        NSV(XS)
607500940308     C                   END
607600941003     C* SE RIENTRO E C'E' VARIA R DEVE ESSERE RIPORTATA
607700940308     C     VFRI          IFEQ      'R'
607800941003     C     §VARR         ANDGT     *ZEROS
607900960327     C     V4CRCS        ANDEQ     'N'
608000941216     C                   MOVEL     §$2VRR        §SIGLA
608100941216     C                   MOVEL     NSV           WSV
608200941216      * CERCO L'INDICE SU CUI LAVORARE
608300941216     C                   EXSR      CHKVAR
608400941216     C                   Z-ADD     §VARR         NVA(XS)
608500941216     C                   MOVEL     §$2VRR        NSV(XS)
608600941003     C                   END
608700941003     C* PROVV. C/ASSEGNO NELLA  VARIA G
608800940308     C     VRCA          IFGT      *ZEROS
608900941216     C                   MOVEL     §$2VRG        §SIGLA
609000941216     C                   MOVEL     NSV           WSV
609100941216      * CERCO L'INDICE SU CUI LAVORARE
609200941216     C                   EXSR      CHKVAR
609300941216     C                   Z-ADD     VRCA          NVA(XS)
609400941216     C                   MOVEL     §$2VRG        NSV(XS)
609500940308     C                   ELSE
609600940308     C* SE C'E C/ASSEGNO MA NON CI SONO PROVV. FORZO =0
609700941229     C* SE CREATA BOLLA IN ARRIVO
609800941229     C     §LPCAS        IFGT      0
609900941229     C     *IN88         ANDEQ     *ON
610000941229     C                   MOVEL     §$2VRG        §SIGLA
610100941229     C                   MOVEL     NSV           WSV
610200941216      * CERCO L'INDICE SU CUI LAVORARE
610300941229     C                   EXSR      CHKVAR
610400941229     C                   MOVE      *ZEROS        NVA(XS)
610500941229     C                   MOVEL     §$2VRG        NSV(XS)
610600941229     C                   END
610700940308     C                   END
610800941221     C                   ENDIF
610900941216     C* TRASPORTO
611000940308     C     VRTR          IFGT      *ZEROS
611100990721     C                   Z-ADD     VRTR          §LPPOR
611200940308     C                   END
611300971003     C* Se non devo recuperare le spese di trasporto imposto il totale
611400971003     C* imponibile
611500971003     C     EXPORT        IFEQ      'F'
611600971003     C     WEXTB1        ANDNE     'FS'
611700971003     C     VNPC          ANDNE     'A'
611800971013     C     VFRI          ANDNE     'R'
611900991001     C* Scorro le varie e le sommo nel totale imponibile solo se il
612000991001     C* flag tab cc lo prevede
612100991001     C                   DO        20            WW                3 0
612200991001     C                   Z-ADD     1             XW                3 0
612300991001     C     NSV(WW)       LOOKUP    CCC(XW)                                20
612400991001     C     *IN20         IFEQ      '1'
612500040123     C     IPB(XW)       ANDEQ     'S'
612600991001     C                   ADD       NVA(WW)       §LPIMV
612700991001     C                   END
612800991001     C                   END
612900971003     C                   END
613000940308     C* MITTENTE
613100940308     C* NON GIA IMPOSTATO PRIMA
613200940308     C     §LPRSM        IFEQ      *BLANKS
613300940308     C                   MOVEL     ARBRSM        §LPRSM
613400940308     C                   MOVEL     ARBINM        §LPINM
613500940308     C                   MOVE      ARBCAM        §LPCAM
613600940308     C                   MOVEL     ARBLOM        §LPLOM
613700940308     C                   MOVEL     ARBPRM        §LPPRM
613800951107     C                   MOVEL     ARBNZM        §LPNZM
613900940308     C                   END
614000970408     C* SE CODICE MITTENTE = 8888/9999 O RESO PASSO 0 NEL CODICE
614100970408     C* TARIFFA ALTRIMENTI GLI PASSO IL CODICE DELLA BOLLA ORIG.
614200970408     C                   MOVE      §LPCCM        W8899             4 0
614300970408     C     W8899         IFEQ      8888
614400970408     C     W8899         OREQ      9999
614500970408     C     VFRI          OREQ      'R'
614600970408     C                   MOVE      *ZEROS        §LPCTR
614700970408     C                   ELSE
614800970408     C                   MOVE      ARBCTR        §LPCTR
614900970408     C                   END
615000970408     C* CAMPI DALLA VECCHIA BOLLA
615100940308     C                   MOVE      *BLANK        §LPCTS
615200940308     C                   MOVE      ARBTSP        §LPTSP
615300091008      * forzatura h 10,30 x dirottamenti resi viene trasformato in 'E'
615400091008     c                   if        §lptsp = 'H' and
615500091008     c                             (vfri = 'D' or vfri = 'R')
615600091008     c                   move      'E'           §lptsp
615700091008     c                   endif
615800941003     C                   Z-ADD     ARBVMD        §LPVMD
615900941216     C                   MOVEL     ARBVAD        §LPVAD
616000960306     C                   MOVEL     'P'           §LPFPR
616100970407     C                   MOVEL     'G'           §LPRIC
616200980519     C* Reperisco flag x stampare o meno bolla in partenza
616300081114      * a meno che non abbia già fatto un tentativo nel caso forzo sempre'N'
616400081114     c                   if        arbntc > 0
616500081114     c                   move      'N'           §lpbam
616600081114     c                   else
616700980519     C                   CLEAR                   AR4NOT
616800980519     C                   MOVE      'A'           §TRC
616900060213     C     KAR4          CHAIN(N)  FiAR401L                           43
617000980519     C  N43              MOVEL     AR4NOT        §LPBAM
617100980519     C   43              MOVEL     'S'           §LPBAM
617200081114     c                   endif
617300000313     C* Salvo flag bolla no DDT per verificare se devo effettuare
617400000313     C* le stampe
617500000313     C                   MOVEL     §LPBAM        WOKSTP            1
617600150630      *
617700150630      *****************************
617800150630      *- NO BOLLA DI RECUPERO_____:
617900150630      *****************************
618000150630      *
618100951213     C* Se no bolla recupero spese x cambio di porto da assegnato a
618200951213     C* franco imposto i dati dal video e da vecchia bolla
618300951213     C     WRECSP        IFNE      'S'
618400150630      *
618500960306     C                   MOVEL     'G'           §LPFPR
618600961011     C  N94V4CRCS        IFNE      'S'
618700051006     C     vfri          andeq     'R'
618800951227     C                   MOVEL     *BLANK        §LPNT2
618900960328     C                   END
619000951213     C* DESTINATARIO
619100951213     C     VRSD          IFNE      *BLANKS
619200951213     C                   MOVEL     VRSD          §LPRSD
619300951213     C                   MOVEL     VRD2          §LPRD2
619400951213     C                   MOVEL     VIND          §LPIND
619500951213     C                   MOVE      VCAD          §LPCAD
619600951213     C                   MOVEL     VLOD          §LPLOD
619700951213     C                   MOVEL     VPRD          §LPPRD
619800951213     C                   MOVEL     VNZD          §LPNZD
619900951213     C                   ELSE
620000951213     C* NON GIA IMPOSTATO PRIMA
620100951213     C     §LPRSD        IFEQ      *BLANKS
620200951213     C                   MOVEL     ARBRSD        §LPRSD
620300951213     C                   MOVEL     AR4RD2        §LPRD2
620400951213     C                   MOVEL     ARBIND        §LPIND
620500951213     C                   MOVE      ARBCAD        §LPCAD
620600951213     C                   MOVEL     ARBLOD        §LPLOD
620700951213     C                   MOVEL     ARBPRD        §LPPRD
620800951213     C                   MOVEL     ARBNZD        §LPNZD
620900951213     C                   END
621000951213     C                   END
621100060929      **
621200000412     C* PER RESI DPD (IMPORT) IMPOSTO CODICE TRATTAMENTO MERCE=6F
621300951213     C                   MOVE      VLNA          §LPLNA
621400000412     C     §LPLNA        CHAIN     AZORG01L                           21
621500000412     C     *IN21         IFEQ      '0'
621600000412     C     ORGFVA        ANDNE     '*'
621700000412     C                   MOVEL     ORGDE3        OG143
621800150630      *
621900150630      * ? solo nel caso si tratti di un Reso a DPD
622000150630      *    cerchiamo l'indirizzo del DEPOT a cui fare la BOLLA .
622100151020     c     §ogntw        ifeq      'DPD'
622200000412     C     VFRI          ANDEQ     'R'
622300151020     c     o24npr        andNE     *blank
622400151020      **
622500151020     c*
622600151020     c*  con il DEPOT di origine del Parcel,  il TISIE10R restituisce
622700151020      *    l'indirizzo del DEPOT preso dal Cappario Originale DPD
622800151020     c                   clear                   tisie10ds
622900171122     c                   clear                   tisie10ds1
623000171122     c                   movel     o24dep4       I10DEP
623100171122     c                   if           I10DEP = *blank
623200151020     c                   movel     o24npr        I10DEP
623300171122     c                   end
623400171122     c                   movel     o24dep7       I10DEP7
623500151020     c                   call      'TISIE10R'
623600151020     c                   parm                    tisie10ds
623700171122     c                   parm                    tisie10ds1
623800151020      *
623900151020      * e l'indirizzo del DEPOT a cui rendere
624000151020     c                   if        O10CDPNAZ <> *blank  and
624100151020     c                             O10ERR    <> 'E'
624200151020     C                   eval      §lprsd  = O10CDPNAM1
624300151020     C                   eval      §lprd2  = O10CDPNAM2
624400151020     C                   eval      §lpind  = O10CDPADR1 + O10CDPADR2
624500151020     C                   eval      §lpcad  = O10CDPPTC
624600151020     C                   eval      §lplod  = O10CDPCTY
624700151020     C                   eval      §lpprd  = *blank
624800151020     C                   eval      §lpnzd  = O10CDPNAZ
624900151020      *
625000151020     c                   endif
625100151021      **
625200151021      * ? MA ATTENZIONE:
625300151021      * Se x DPD la linea di Arrivo non è utilizzabile in bollettazione,
625400151021      * Prendo allora la linea del Cliente
625500151021     c                   if        §OGLBL = 'N'
625600151021     c                   movel     §lpccm        Vlna
625700151021     C                   MOVE      VLNA          §LPLNA
625800151021     c                   end
625900151020     c*---------
626000000412     C                   END
626100000412     C                   END
626200150630     c
626300951213     C                   MOVE      ARBGC1        §LPGC1
626400951213     C                   MOVE      ARBGC2        §LPGC2
626500071024      * se precedentemente trovato destinatario particolare imposto
626600071024      * campi da anagrafica destinatari particolari
626700071024      * turni
626800071024      * forza giorni di chiusura solo se non impostati sulla mamma
626900091015     c                   if        des_parg <> *blank and ot0err = *blank
627000071024     c                   if        vfri <> 'C'
627100071024     c                   move      OT0X1GC1      §LPGC1
627200071024     c                   move      OT0X1GC2      §LPGC2
627300071024     c                   endif
627400071024      *giro
627500071024      * forza giro di consegna se lna corrisponde a linea gestione giro
627600071024     c                   if        §lplna = OT0X1POG
627700071024     c                   move      OT0X1CGI      §LPcgi
627800071024     c                   endif
627900071024     c                   endif
628000950214     C* RIPORTO SU NUOVA BOLLA IMPORTO DA ASSICURARE SOLO SE FACCIO
628100950214     C* CAMBIO PORTO IN ASSEGN. O RIMANE ASSEGNATO
628200950214     C     VNPC          IFEQ      'A'
628300950214     C     VNPC          OREQ      ' '
628400950214     C     EXPORT        ANDEQ     'A'
628500960205     C     VFRI          OREQ      'R'
628600960327     C     V4CRCS        ANDEQ     'N'
628700050509     C     VFRI          OREQ      'D'
628800950206     C                   Z-ADD     ARBIAS        §LPIAS
628900950214     C                   MOVEL     ARBVAS        §LPVAS
629000950214     C                   ELSE
629100950214     C                   CLEAR                   §LPIAS
629200950214     C                   CLEAR                   §LPVAS
629300950214     C                   ENDIF
629400150630      *
629500150630      *****************************
629600150630      *- SI BOLLA DI RECUPERO_____:
629700150630      *****************************
629800150630      **
629900951213     C                   ELSE
630000150630      **
630100951213      *  Bolla recupero spese trasporto dal mittente x cambi di
630200951213      *  porto da assegnato a franco
630300951213     C                   MOVEL     *BLANKS       §LPFFD
630400000925     C                   MOVEL     *BLANKS       §LPGGA
630500000922      *
630600000922      **---- < ATTENZIONE >  CHIODO PER DPD
630700000922      *    NON DEVE ESSERE RIPULITO IL CAMPO SE GIÀ IMPOSTATO
630800000922     C     VFRI          IFNE      'R'
630900000925     C     DPDSN         ORNE      'S'
631000951213     C                   MOVEL     *BLANKS       §LPGMA
631100000925     C                   ENDIF
631200000925      *
631300150311     C                   clear                   §lpTBA
631400150311     C                   clear                   §lpNBA
631500150311     C                   clear                   §lpTB2
631600150311     C                   clear                   §lpNB2
631700150311     C                   clear                   §lpcor
631800150311     C                   clear                   §lpcsf
631900951213     C                   MOVEL     *BLANKS       §LPGVA
632000951213     C                   MOVEL     *BLANKS       §LPGCA
632100951213     C                   Z-ADD     0             §LPPOR
632200951213     C                   Z-ADD     0             §LPDFT
632300951213     C                   Z-ADD     0             §LPFIV
632400990930     C                   CLEAR                   DTASV
632500030617     C* se annullamento porto assegnato sommo la varia & solo se la
632600030617     C* responsabilità è del cliente
632700030617     C                   IF        vfri='C' and vnpc='F' and viddcli='C'
632800030617     C                   MOVEL     §$2Vpa        §SIGLA
632900030617     C                   MOVEL     NSV           WSV
633000030617      * CERCO L'INDICE SU CUI LAVORARE
633100030617     C                   EXSR      CHKVAR
633200030623     C                   z-add     88888888      NVA(XS)
633300030617     C                   MOVEL     §$2Vpa        NSV(XS)
633400030617     C                   END
633500951213      *  Controllo se devo recuperare le provvigioni del C/Ass.
633600951213     C     VAPC          IFEQ      'S'
633700951213     C     VPPC          ANDEQ     'M'
633800951213     C                   Z-ADD     1             YY                4 0
633900951213     C     §$2VRG        LOOKUP    ASV(YY)                                06
634000951227      *  Se esiste varia G e non ho variato il C/Assegno tasso le provv.
634100951227      *  x il mittente in base alla tariffa del destinatario
634200951227     C     VVCS          IFNE      'S'
634300951227     C     *IN06         ANDEQ     '1'
634400951213     C                   MOVEL     ASV(YY)       NSV(1)
634500951213     C                   Z-ADD     AVA(YY)       NVA(1)
634600951227      *  ... altrimenti uso le tariffe di cartello
634700951227     C                   ELSE
634800951227     C                   EXSR      PPROVV
634900951213     C                   END
635000951213     C                   END
635100951213     C                   END
635200950206     C* SE SPED. ESTERO E DIVISA NON è IMPOSTATA FORZO DIV.=ITL
635300020226     C                   MOVEL     *BLANKS       ORGFL1
635400020226     C     §LPLNA        CHAIN     AZORG01L                           23
635500020729     C                   clear                   NLAFL1            1
635600020729     C     §OgNTW        ifeq      'FED'
635700020729     C     §OgNTW        oreq      'EEX'
635800020729     C     §OgNTW        oreq      'EUP'
635900020729     C                   movel     'E'           NLAFL1            1
636000020729     c                   end
636100020729      *
636200950206     C     §LPVMD        IFGT      *ZEROS
636300950206     C     ARBVAD        ANDEQ     *BLANK
636400011130     C                   MOVEL     §GEDCN        §LPVAD
636500950206     C                   ENDIF
636600950206     C*
636700950206     C     §LPIAS        IFGT      *ZEROS
636800950206     C     ARBVAS        ANDEQ     *BLANK
636900011130     C                   MOVEL     §GEDCN        §LPVAS
637000950206     C                   ENDIF
637100940308     C                   MOVE      ARBNAS        §LPNAS
637200940308     C                   MOVE      ARBNCL        §LPNCL
637300051005     C                   MOVE      ARBPKF        §LPPKB
637400941003     C                   MOVE      ARBPKC        §LPPKC
637500941003     C                   MOVE      ARBNCP        §LPNCP
637600951109     C                   MOVE      ARBVLF        §LPVLB
637700951109     C                   MOVE      ARBFVF        §LPFVB
637800960229     C*--------------------------------------------------------*
637900960229     C*  Se il flag del volume bollettato è  'C' o 'K' oppure
638000960229     C*  è  'P' ed il cliente è codificato imposto 'R' in modo
638100960229     C*  che non venga più ricalcolato
638200960229     C     ARBFVF        IFEQ      'C'
638300960229     C     ARBFVF        OREQ      'K'
638400960229     C     ARBFVF        OREQ      'P'
638500960229     C     §APPO         ANDEQ     8888
638600960229     C     ARBFVF        OREQ      'P'
638700960229     C     §APPO         ANDEQ     9999
638800960229     C                   MOVEL     'R'           §LPFVB
638900960229     C                   END
639000020716     C* se sto creando bolla di recupero x dirottamento azzero le varie
639100020716     c     viddcli       ifeq      'C'
639200020716     c     *in92         andeq     *on
639300020716     C                   CLEAR                   NVA
639400020716     C                   CLEAR                   NSV
639500020716     c                   endif
639600110715      * se dirottata bolla di rientro reimposta le varie relative alla giacenza
639700110715      * se erano valorizzate
639800110715     c                   if        vfri = 'D' and not *in90 or
639900110715     c                             vfri = 'R' and not *in90
640000110715     c                   if        memova1 <> *blank or
640100110715     c                             memova2 <> *blank or
640200110715     c                             memova3 <> *blank
640300110715     c                   move      memova1       nsv(1)
640400110715     c                   z-add     memovar1      nva(1)
640500110715     c                   move      memova2       nsv(2)
640600110715     c                   z-add     memovar2      nva(2)
640700110715     c                   move      memova3       nsv(3)
640800110715     c                   z-add     memovar3      nva(3)
640900110715     c                   endif
641000110715     c                   endif
641100960229     C*--------------------------------------------------------*
641200941003     C                   MOVE      ARBVLC        §LPVLC
641300941003     C                   MOVE      ARBNCR        §LPNCR
641400941006     C                   MOVE      ARBQFT        §LPQFT
641500940308     C                   MOVE      ARBNCR        §LPNCR
641600941011     C                   MOVEL     *BLANK        §LPFTM
641700941011     C                   MOVEL     *BLANK        §LPCEI
641800040916      * per bolla di recupero abblenca sempre tc1 e tc2
641900040916     c                   if        wrecsp <> 'S'
642000050826      * x consegna riporta sempre
642100050826      * x dirottamento riporta se non S=SUPERMERCATO o D=DISAGIATO
642200050826     c                   if        vfri = 'C' or
642300050826     c                             vfri = 'D' and
642400050826     c                             (arbtc1 <> 'D' and arbtc1 <> 'S' and
642500050826     c                              arbtc2 <> 'D' and arbtc2 <> 'S')
642600941011     C                   MOVEL     ARBTC1        §LPTC1
642700941003     C                   MOVEL     ARBTC2        §LPTC2
642800960228     C                   ELSE
642900960228     C                   MOVEL     *BLANKS       §LPTC1
643000960228     C                   MOVEL     *BLANKS       §LPTC2
643100960228     C                   END
643200040916     C                   else
643300040916     C                   move      *blank        §lptc1
643400160905     C                   move      *blank        §lptc2
643500040916     C                   endif
643600941003     C                   MOVEL     '*'           §LPSOP
643700940308     C* RICH. IMMISS. BOLLE
643800030715     C* OVRPRTF  BOLLE old
643900991206     C                   Z-ADD     80            LUNCMD           15 5
644000991206     C                   MOVEA     D82PRB        CMB(31)
644100991206     C                   MOVEA     D82MOB        CMB(64)
644200991206     C                   CALL      'QCMDEXC'
644300991206     C                   PARM                    CMB
644400991206     C                   PARM                    LUNCMD
644500030715     C* OVRPRTF  BOLLE a4
644600030715     C                   MOVEA     D82PRB4       CM4(31)
644700030715     C                   MOVEA     D82MdB4       CM4(64)
644800030715     C                   CALL      'QCMDEXC'
644900030715     C                   PARM                    CM4
645000030715     C                   PARM                    LUNCMD
645100030715     C* OVRPRTF  BOLLE a5
645200030715     C                   MOVEA     D82PRB5       CM5(31)
645300030715     C                   MOVEA     D82Mdb5       CM5(64)
645400030715     C                   CALL      'QCMDEXC'
645500030715     C                   PARM                    CM5
645600030715     C                   PARM                    LUNCMD
645700991206     C* OVRPRTF  BOLLE  DPD
645800991206     C                   MOVEA     D82PRB        CMP(31)
645900940308     C                   CALL      'QCMDEXC'
646000991206     C                   PARM                    CMP
646100940308     C                   PARM                    LUNCMD
646200030304     C* OVRPRTF  BOLLE  FedEx
646300030307     c                   Movel     *Blanks       comman           90
646400030307     C                   Z-ADD     90            LUNCMD           15 5
646500030304     C                   MOVEA     D82PRB        CMPf(31)
646600030307     c                   Movea     Cmpf          Comman
646700030307     c                   Eval      %Subst(Comman:77:9) = 'COPIES(2)'
646800030304     C                   CALL      'QCMDEXC'
646900030307     c                   PARM                    Comman
647000030304     C                   PARM                    LUNCMD
647100940308     C* OVRPRTF  ETICHETTE
647200941013     C                   Z-ADD     80            LUNCMD           15 5
647300941013     C                   MOVEA     D82PRE        CME(31)
647400950123     C                   MOVEA     D82MOE        CME(64)
647500940308     C                   CALL      'QCMDEXC'
647600940308     C                   PARM                    CME
647700940308     C                   PARM                    LUNCMD
647800991028     C* Imposto divisa per calcolo tassazione
647900991119     C                   XFOOT     NVA           TOTVAR           15 3
648000991119     C     TOTVAR        IFGT      0
648100991119     C     §LPPOR        ORGT      0
648200991028     C   33              MOVEL     V6CDIV        §LPDIV
648300991028     C   35              MOVEL     V6CRVA        §LPDIV
648400991028     C   36              MOVEL     V6CRTA        §LPDIV
648500991112     C  N33
648600991112     CANN35
648700991112     CANN36              MOVEL     GCPDIV        §LPDIV
648800991119     C* Se la divisa
648900991119     C     §LPDIV        IFEQ      *BLANKS
649000011130     C                   MOVEL     §GEDCN        §LPDIV
649100991119     C                   END
649200991119     C                   ELSE
649300991119     C                   MOVEL     *BLANKS       §LPDIV
649400991119     C                   END
649500030715     c* se dirottamento di una spedizione FW (servizio contrassegni)
649600030715     c* se le note 2 sono = *blanks metto
649700030715     c* la la dicitura "cliente tassazione xxxxxxx" preso dalla bolla orig.
649800030715     c                   if        §lpnt2 = *blanks and arbcbo = 'FW'
649900030715     c                             and vfri = 'D'
650000030715     C                   MOVE      '8'           §TRC
650100060213     C     KAR4          CHAIN(N)  FiAR401L
650200060213     c                   if        %found(fiar401l) and
650300030715     c                             %subst(ar4not: 1: 18) = 'cliente tassazione'
650400030715     C                   eval      §lpnt2= ar4not
650500030715     c                   end
650600030715     c                   end
650700100517     c* in caso di particolarità consegna  devo verificare
650800030821     c* se passare la particolarità x non fare scrivere il fiar500f nel
650900030821     c* pgm fnls01r inquanto lo scriverò in questo pgm.
651000100517     c                   if        arbgma <>*blank
651100100517     C                   MOVEL     '7R'          TBLCOD
651200100517     C                   MOVEL     *BLANK        TBLKEY
651300100517     C                   MOVEL     ARBGMA        TBLKEY
651400100517     C     KTAB          CHAIN     TABEL                              21
651500100517     C     *IN21         IFEQ      '0'
651600100517     C                   MOVEL     TBLUNI        DS7R
651700100517     C                   if        §7Rrc ='S'
651800030821     c                   eval      compar = arbgma
651900030821     c                   exsr      srpar
652000030821     c                   eval      §lpgma = compar
652100100517     c                   end
652200100517     c                   end
652300030821     c                   end
652400040123      * richiama tnsf20 x varie con calcolo istat solo se imponibile > 0
652500040123     c                   if        §lpimv > 0
652600040123     c                   exsr      ritassa
652700040123     c                   endif
652800030715     c*
652900040527      * imposta flag valore se esiste NO BOLLE RECUPERO
653000040527     c                   if        wrecsp <> 'S'
653100040527     c                   movel     §ar5bva       §lpbva
653200040527     c                   endif
653300040527     c*
653400100324     c                   clear                   §lpksca
653500100324     c                   clear                   §lpctra
653600091104      * imposta flag Triangolo e codice cliente e tariffa
653700091104      * se da passare a nuova bolla per dirottamento da bolla con triangolo
653800091028     c                   clear                   §lpass
653900091028     c                   if        vfri = 'D' and §ar5ass = 'T'
654000091215     c* se ho il contrassegno e il codice intestatario assegno lo imposto
654100091215     c                   if        §lpcas > 0 and §ar5cdi <> ' '
654200091215     c                   move      §ar5cdi       §lpcdi
654300091215     c                   end
654400091028     c                   movel     §ar5ass       §lpass
654500091104     c                   move      arbksc        §lpksca
654600091104     c                   move      arbctr        §lpctra
654700091028     c                   endif
654800091028     c*
654900091030      * chiodo fedex per resi in c/s
655000091030     c                   if        vfri = 'R' and arblnp = 400
655100091030     C                   movel     '3A'          tblcod
655200091030     C                   movel(P)  ARBcbo        tblkey
655300091030     C     ktab          chain     TABEL
655400091030     C                   movel     TBLUNI        DS3A
655500091030     c                   if        §3arbl = 'C'
655600091030     c                   move      'A '          §lpcbo
655700091030     c                   endif
655800091030     c                   endif
655900091030     c*
656000040527      * imposta flag dati clienti in stampa bolla     O
656100040527     c                   if        wrecsp <> 'S' and vfri <> 'R'
656200040527     c     kbol          chain     fiar801l
656300040527     c                   if        %found(fiar801l)
656400040527     c                   move      'S'           §lpar8
656500040527     c                   endif
656600040527     c                   endif
656700040527     c                   z-add     kaas          §LPAASM
656800040527     c                   z-add     klnp          §LPLNPM
656900040527     c                   z-add     knrs          §LPNRSM
657000040527     c                   z-add     knsp          §LPNSPM
657100050811      *verifica se inserire i codici clienti per la tassazione assegnati
657200050811      * i campi vanno impostati solo se la vecchia bolla era un franco e
657300050811      * la nuova è un assegnato e se il mittente della vecchia bolla è un
657400050826      * codificato nel caso si tratti di bolla di recupero o di reso
657500050826     c                   if        export = 'F' and
657600050811     c                             wvario <> 8888 and wvario <> 9999
657700050826     c                             and (vfri = 'R' or wrecsp = 'S')
657800050811     c                   move      arbksc        §lpksca
657900050811     c                   move      arbctr        §lpctra
658000080123     c* Richiamo routine di controllo codice tariffa per tassazine assegnati
658100080123     c                   EXSR      ContrCTRA
658200050811     c                   endif
658300151222      **
658400151222      **  Attenzione                     il codice Cliente e la tariffa
658500151222      * ? solo nel caso si tratti di un Reso a DPD
658600151222      **   sono quelli presi in tabella LPD
658700151222     c     §ogntw        ifeq      'DPD'
658800151222     C     VFRI          ANDEQ     'R'
658900151222     c     o24npr        andNE     *blank
659000151222     c                   clear                   FIEULPDDS
659100151222     c                   z-add     §LPLNP        ILPDLNP
659200151222     c                   z-add     §LPLNA        ILPDLNA
659300151222     c                   movel     o24npr        ILPDPARCEL
659400151222     c*   il FIEULPDR
659500151222     c                   call      'FIEULPDR'
659600151222     c                   parm                    FIEULPDDS
659700151222     c*
659800151222     c                   eval      dLPD = oLPDUNI
659900151222     c                   move      §LPDkscRE     §lpksca
660000151222     c                   move      §LPDtRE       §lpctra
660100151222      **
660200151222     c                   endif
660300130920     c*
660400130920     c*  Se su AZORG c'è la forzatura per la LNA viene sostituita la
660500130920     c*    LNA con quella di OG143 §ogLNAres
660600130920     c                   exsr      CHG_LNA_xFORZA
660700130920     c*
660800170323     c                   if        wlnpb>0
660900170323     C                   CALL      'FNLS01R'
661000170323     C                   PARM                    KPJBA
661100170323     C                   PARM                    DBLP
661200170323     C                   PARM                    DTASV
661300170323     C                   PARM                    trul90ds
661400170323     C                   PARM                    §lplnp
661500170323     c                   else
661600941013     C                   CALL      'FNLS01R'
661700940308     C                   PARM                    KPJBA
661800990930     C                   PARM                    DBLP
661900990930     C                   PARM                    DTASV
662000030715     C                   PARM                    trul90ds
662100170323     c                   endif
662200150630      ** Errore per non aver generato la BOLLA
662300141030     c                   if        d82gia= 'N' and
662400141030     c                             (§lpfpr = '3' or §lpfpr = '4')
662500141030     c                   if        %parms > 1
662600141202     c                   if        wrecsp = *blank
662700141030     c                   move      'R'           d20erro
662800141202     c                   endif
662900141202     c
663000150630      *** se richiamato da gestione dei DIROTTAMENTI
663100141030     c                   else
663200141118     c                   if        wrecsp = *blank
663300141030     c                   move      '1'           d82f12
663400141118     c                   endif
663500141030     C                   MOVEL     fnlg82        KPJBU
663600141030     c                   endif
663700141118     c                   eval      *inlr = *on
663800141030     c                   return
663900141030     c                   endif
664000110802      * verifico se ho dei rekord di ar4 da riportare sulla bolla
664100110802     c                   exsr      DupAr4
664200030807     C* controllo se devo duplicare le estensioni (fiar5) dalla bolla orig.
664300030807     c* per le bolle figlie (wrecsp = ' ') e per le bolle di recupero
664400030807     c* (wrecsp = 'S')
664500030807     c                   if        wrecsp = ' '
664600030807     c                   movea     fig           ar5
664700030807     c                   else
664800030807     c                   movea     rec           ar5
664900030807     c                   end
665000030807     c                   exsr      AGGAR5
665100140916     c* Se la bolla figlia è un disk "C"  avente come "CHI SONO" il segnacollo
665200140916     c* Bartolini della bolla mamma devo cancellare dal FISGN il "CHI SONO"
665300140916     c* della bolla mamma. Se dovessero sparare il "CHI SONO" della mamma NON
665400140916     c* DEVE uscire l'etichetta, altrimenti uscirebbe il segnacollo BRT della
665500140916     c* mamma e non va bene.
665600140916     c                   exsr      sr_fisgn
665700940308     C* DLTOVR
665800941013     C                   Z-ADD     80            LUNCMD           15 5
665900940308     C                   CALL      'QCMDEXC'
666000940308     C                   PARM                    DOV
666100940308     C                   PARM                    LUNCMD
666200940308     C                   ENDSR
666300030805     C*---------------------------------------------------------------*
666400030821     C*   srpar : Controllo se devo abblencare la particolarità       *
666500030805     C*---------------------------------------------------------------*
666600030807     C     srpar         BEGSR
666700030807     c                   eval      tbecod = 'AR5'
666800030807     c                   clear                   tbeke2
666900030807     c                   eval      tbeke1 = 'RCC'
667000030807     C     ktbe          chain     TNTBE01L
667100030807     c                   if        %found(tntbe01l) and tbeatb = ' '
667200030807     C                   MOVEL     TBEUNI        DAR5
667300030807     c* abblenco la particolarità a seconda se scriverò io il fiar500f
667400030807     c* oppure se lo lascio scrivere al pgm fnls01r
667500030807     c* wrecsp = S sono nel caso che sto scrivendo la bolla di recupero
667600030821     c                   if        (wrecsp=' ' and vfri='D' and §ar5dupd='N') or
667700030821     c                             (wrecsp=' ' and vfri='R' and §ar5dupr='N') or
667800030821     c                             (wrecsp=' ' and vfri='C' and §ar5dupc='N') or
667900030821     c                             (wrecsp='S' and §ar5drec='N')
668000030807     c                   clear                   compar
668100030807     C                   END
668200030807     C                   END
668300030807     C                   ENDSR
668400040123     C*---------------------------------------------------------------*
668500040123     C*   richiama nuovamente tassatore per varie aggiuntive          *
668600040123     C*---------------------------------------------------------------*
668700040123     C     ritassa       BEGSR
668800040129     C                   CLEAR                   DTAS
668900040129     C                   MOVEL     §LPCTR        TASCTR
669000040129     C                   MOVE      KKSC          TASKSC
669100040129     C*
669200040129     C                   MOVEL     §LPCTS        TASCTS
669300040129     C                   MOVEL     §LPCAD        TASCAD
669400040129     C                   MOVEL     §LPCAM        TASCAM
669500040129     C                   MOVEL     §LPTSP        TASTSP
669600040129     C                   MOVEL     ARBTC1        TASTC1
669700040129     C                   MOVEL     ARBTC2        TASTC2
669800040129     C                   MOVEL     ARBFDN        TASFDN
669900040129     C                   MOVEL     §LPVAS        TASVAS
670000040129     C                   Z-ADD     §LPIAS        TASIAS
670100040129     C                   Z-ADD     ARBQFT        TASQFT
670200040129     C                   Z-ADD     ARBNCL        TASNCL
670300160530     C                   Z-ADD     ARBNCL        TASNCLb
670400040129     C                   Z-ADD     AMGUDT        TASDSP
670500040129     C                   Z-ADD     TASDSP        TASDCT
670600040129     C                   Z-ADD     §LPLNP        TASLNP
670700040129     C                   Z-ADD     VLNA          TASLNA
670800040129     C                   Z-ADD     ARBVLF        TASVLF
670900040129     C                   Z-ADD     ARBPKF        TASPKF
671000160530     C                   Z-ADD     ARBvlc        TASvlc
671100160530     C                   Z-ADD     ARBncr        TASncr
671200160530     C                   move      ARBfvf        TASfvf
671300160530     C                   Z-ADD     ARBpkb        TASPKb
671400040129     C                   CLEAR                   DS3A
671500040129     C                   Z-ADD     1             §F                2 0
671600040129     C     §LPCBO        LOOKUP    CDB(§F)                                21
671700040129     C   21              MOVEL     D3A(§F)       DS3A
671800040129     C   21              MOVEL     §3ATB1        TASTBL
671900040129     C* Reperisco codice tassazione
672000040129     C                   CLEAR                   TISI95
672100040129     C     TASLNP        CHAIN     AZORG01L                           21
672200040129     C     *IN21         IFEQ      '0'
672300040129     C                   MOVE      ORGDE3        OG143
672400040129     C     §OGNTW        IFEQ      'FED'
672500040129     C                   MOVE      'S'           I95FI1
672600040129     C                   END
672700040129     C                   END
672800040129     C                   MOVEL     '3'           I95TCN
672900040129     C                   MOVEL     §LPCAM        I95CAP
673000040129     C                   MOVEL     §LPNZM        I95NAR
673100040129     C                   MOVEL     §LPPRM        I95PRV
673200040129     C                   MOVEL     §LPLOM        I95LOC
673300040129     C                   MOVEL     §LPINM        I95IND
673400040129     C                   MOVEL     ARBTFP        I95TFP
673500040129     C                   MOVEL     ARBTSP        I95TSP
673600040129     C                   MOVEL     'S'           I95FRE
673700040129     C                   MOVEL     §LPNZM        I95NAR
673800040129     C                   MOVEL     AMGUDT        I95DAT
673900040129     C                   MOVEL     ARBtc1        I95fi2
674000040129     C                   MOVE      ARBtc2        I95fi2
674100040129     C                   CALL      'TISI95R'
674200040129     C                   PARM                    TISI95
674300040129     C                   MOVEL     O95CTS        TASMCT
674400040129     C*
674500040506     C                   MOVEL     ar9vca        TASVCA
674600040129     C                   Z-ADD     §ASS          TASCAS
674700040129     C                   MOVEL     VTIC          TASTIC
674800040129     C*
674900040129     C                   MOVE      §LPBAM        TASDDT
675000040129     c* imposto peso vdl e colli rilevati
675100040129     c                   z-add     arbncp        Tasncp
675200040129     c                   z-add     arbpkc        Taspkc
675300060927     C                   movel     ARBfbr        §asfbr
675400060927     C                   movel     ARBcca        §AScca
675500060927     C                   movel     dtas01        TASflo
675600040123     C*
675700040123     C                   MOVEL     'Z'           TASSVA
675800040123     C*
675900040123     C                   CALL      'TNSF20R'
676000040123     C                   PARM                    KPJBA
676100040123     C                   PARM                    DTAS
676200040123     C                   PARM                    DTASV
676300040123     C*
676400040123     C     TASERR        IFEQ      *BLANKS
676500040202     c                   z-add     0             §lpimv
676600040123     C* Scorro le varie e le sommo nel totale imponibile solo se il
676700040123     C* flag tab cc lo prevede
676800040123     C                   DO        20            WW                3 0
676900040123     C                   Z-ADD     1             XW                3 0
677000040123     C     NSV(WW)       LOOKUP    CCC(XW)                                20
677100040123     C     *IN20         IFEQ      '1'
677200040123     C     IPB(XW)       ANDEQ     'S'
677300040123     C                   ADD       NVA(WW)       §LPIMV
677400040123     C                   END
677500040123     C                   END
677600040123     C                   END
677700040123     C                   ENDSR
677800030807     C*---------------------------------------------------------------*
677900030807     C*   CTRSTB: Controllo se devo ristampare la bolla               *
678000030807     C*---------------------------------------------------------------*
678100030807     C     CTRSTB        BEGSR
678200951206     C*  Aggancio giacenza
678300050316     C     KGIA          CHAIN(e)  tigcp02L                           40
678400050316     c                   if        %error
678500070521     C     KGIA          CHAIN(n)  tigcp02L
678600050316     c                   exsr      geslck
678700050318     C     KGIA          CHAIN(e)  tigcp02L                           40
678800050316     c                   endif
678900951206     C     *IN40         IFEQ      '0'
679000951206     C                   MOVE      VAAS          KAAS1
679100951206     C                   MOVE      VLNP          KLNP1
679200951206     C                   MOVE      VNRS          KNRS1
679300951206     C                   MOVE      VNSP          KNSP1
679400951206     C                   MOVE      VNSP          KNSP1
679500951206     C                   MOVE      GCPDXD        KDEV
679600951206     C                   MOVE      *HIVAL        KHEV
679700990803     C     KEVB1         SETGT     FNEVB01L
679800990803     C     KEVB2         READPE    FNEVB01L                               45
679900951206     C     *IN45         IFEQ      '0'
680000081219     C                   Z-ADD     1             XY
680100951206     C     EVBCEV        LOOKUP    C2A(XY)                                45
680200951206     C     *IN45         IFEQ      '1'
680300951206     C                   MOVEL     D2A(XY)       DS2A
680400951206     C     §2AFTC        IFEQ      'A'
680500000313     C* Per bolle export e DPD non ristampo il laciato avviso x
680600000313     C* la consegna
680700000313     C     ARBLNA        CHAIN     AZORG01L                           21
680800000313     C                   MOVEL     ORGDE3        OG143
680900020729     C                   clear                   flgest            1
681000020729     C     §OgNTW        ifeq      'FED'
681100020729     C     §OgNTW        oreq      'EEX'
681200020729     C     §OgNTW        oreq      'EUP'
681300020729     C                   MOVEL     'E'           flgest            1
681400020729     c                   end
681500020729     C     Flgest        IFNE      'E'
681600020617     C     §OGntw        ANDNE     'DPD'
681700951206     C* RICH. IMMISS. BOLLE
681800030715     C* OVRPRTF  BOLLE old
681900951206     C                   Z-ADD     80            LUNCMD           15 5
682000951206     C                   MOVEA     D82PRB        CMB(31)
682100951206     C                   MOVEA     D82MOB        CMB(64)
682200951206     C                   CALL      'QCMDEXC'
682300951206     C                   PARM                    CMB
682400951206     C                   PARM                    LUNCMD
682500030715     C* OVRPRTF  BOLLE a4
682600030715     C                   MOVEA     D82PRB4       CM4(31)
682700030715     C                   MOVEA     D82Mdb4       CM4(64)
682800030715     C                   CALL      'QCMDEXC'
682900030715     C                   PARM                    CM4
683000030715     C                   PARM                    LUNCMD
683100030715     C* OVRPRTF  BOLLE a5
683200030715     C                   MOVEA     D82PRB5       CM5(31)
683300030715     C                   MOVEA     D82Mdb5       CM5(64)
683400030715     C                   CALL      'QCMDEXC'
683500030715     C                   PARM                    CM5
683600030715     C                   PARM                    LUNCMD
683700951206     C*  Richiamo pgm stampa bolle
683800130315     c                   reset                   FNLSB6ds1
683900030624     C                   CLEAR                   FNLSB5DS
684000951206     C                   MOVEL     ' '           DB0TLA
684100951206     C                   MOVEL     ' '           DB0GES
684200951206     C                   MOVEL     'A'           DB0TBO
684300951206     C                   MOVE      VAAS          DB0AAS
684400951206     C                   MOVE      VLNP          DB0LNP
684500951206     C                   MOVE      VNRS          DB0NRS
684600951206     C                   MOVE      VNSP          DB0NSP
684700030715     C                   CALL      d90psl
684800030624     C                   PARM                    FNLSB5DS
684900130315     c                   parm                    FNLSB6ds1
685000000313     C                   END
685100951206     C* MI RITORNA IL NR. BOLLA CREATO
685200951206     C* DLTOVR
685300951206     C                   Z-ADD     80            LUNCMD           15 5
685400951206     C                   CALL      'QCMDEXC'
685500951206     C                   PARM                    DOV
685600951206     C                   PARM                    LUNCMD
685700951206     C                   END
685800951206     C                   END
685900951206     C                   END
686000951206     C                   END
686100951206     C*
686200951206     C                   ENDSR
686300951206     C*---------------------------------------------------------------*
686400941220     C*   A G G C C M  FORZO COD.CLIENTE MITTENTE                     *
686500940308     C*---------------------------------------------------------------*
686600941220     C     AGGCCM        BEGSR
686700941220     C* IN CASO DI RESO
686800971230     C     VFRI          IFEQ      'R'
686900971230     C     V4CRCS        IFEQ      'N'
687000941220     C                   MOVE      9999          §LPCCM
687100941220     C                   MOVEL     §LPLNP        §LPCCM
687200971230     C                   ELSE
687300971230     C                   MOVE      8888          §LPCCM
687400960328     C                   MOVEL     §LPLNP        §LPCCM
687500960328     C                   END
687600941220     C                   ELSE
687700971230     C     WRECSP        IFEQ      'S'
687800020625     c                   if        *in92
687900020625     c                   move      arbksc        §lpccm
688000020625     c                   else
688100971230     C                   MOVE      9999          §LPCCM
688200951227     C                   MOVEL     §LPLNP        §LPCCM
688300020625     c                   end
688400951214     C                   ELSE
688500971230     C* NEGLI ALTRI CASI  IMPOSTO IL CLIENTE MITTENTE
688600941003     C     ARBCCM        IFGT      0
688700941003     C                   MOVE      ARBCCM        §LPCCM
688800941003     C                   ELSE
688900941003     C     EXPORT        IFEQ      'F'
689000941003     C                   MOVE      ARBKSC        §LPCCM
689100940308     C                   END
689200940308     C                   END
689300941220     C* SE NON SONO RIUSCITA AD IMPOSTARLO FORZO CLI.VARIO
689400941220     C     §LPCCM        IFEQ      *ZEROS
689500941220     C     VNPC          IFEQ      'F'
689600941220     C                   MOVE      *ALL'8'       §LPCCM
689700941220     C                   MOVEL     §LPLNP        §LPCCM
689800941220     C                   END
689900941220     C     VNPC          IFEQ      'A'
690000941220     C     VNPC          OREQ      ' '
690100941220     C     EXPORT        ANDEQ     'A'
690200941220     C                   MOVE      *ALL'9'       §LPCCM
690300941220     C                   MOVEL     §LPLNP        §LPCCM
690400941220     C                   END
690500971230     C                   ENDIF
690600971230     C                   ENDIF
690700971230     C                   ENDIF
690800950130     C                   MOVEL     §LPCCM        §LIN              3 0
690900950130     C                   MOVE      §LPCCM        §APPO             4 0
691000950130     C     §LIN          IFNE      §LPLNP
691100950130     C     §APPO         ANDEQ     8888
691200950130     C                   MOVEL     §LPLNP        §LPCCM
691300950130     C                   END
691400020625      * la move era incondizionata
691500020625     c                   if         *in92
691600020625     c                   movel     arblna        §lpccn
691700020625     c                   move      9999          §lpccn
691800020625     c                   else
691900941220     C                   MOVE      §LPCCM        §LPCCN
692000020625     c                   end
692100971230     C*
692200940308     C                   ENDSR
692300940309     C*---------------------------------------------------------------*
692400941011     C*   A G G A R 4           FILE NOTE                             *
692500940309     C*---------------------------------------------------------------*
692600941003     C     AGGAR4        BEGSR
692700940309     C                   MOVE      '0'           §TRC
692800060213     C     KAR4          CHAIN     FiAR401L                           21
692900940309     C                   MOVE      §LPAAS        NEWAAS
693000940309     C                   MOVE      §LPLNP        NEWLNP
693100940309     C                   MOVE      §LPNRS        NEWNRS
693200940309     C                   MOVE      §LPNSP        NEWNSP
693300940309     C                   MOVE      §LPLNA        NE2LNA
693400061025     C                   MOVEL(p)  NEWBOL        AR4NOT
693500941003     C   21              MOVE      §TRC          AR4TRC
693600941216     C   21              MOVE      KAAS          AR4AAS
693700941216     C   21              MOVE      KLNP          AR4LNP
693800941216     C   21              MOVE      KNRS          AR4NRS
693900941216     C   21              MOVE      KNSP          AR4NSP
694000060213     C  N21              UPDATE    FiAR4000
694100060213     C   21              WRITE     FiAR4000
694200940309     C                   ENDSR
694300030806     C*---------------------------------------------------------------*
694400030806     C* AGGAR5: Controllo se devo duplicare fiar500f                  *
694500030806     C*---------------------------------------------------------------*
694600030806     C     aggar5        BEGSR
694700030806     c                   if        §lpnsp <> 0
694800030806     c                   do        30            bb
694900030806     c                   if        ar5(bb) = *blanks
695000030806     c                   leave
695100030806     c                   end
695200030806     C                   movel     ar5(bb)       trcar5
695300051013     c     kFiar5        Chain(n)  Fiar501l
695400030806     c                   If        %Found(Fiar501l)
695500030806     c                   clear                   ar5prg
695600030911     c                   eval      ar5ft1 = 'T'
695700030911     c                   eval      ar5dt1 = 99999999
695800030911     c                   eval      ar5ft2 = 'T'
695900030911     c                   eval      ar5dt2 = 99999999
696000030806     c                   clear                   ar5ft3
696100030806     c                   clear                   ar5dt3
696200030806     c                   move      amgudt        ar5dac
696300030806     C                   TIME                    ar5ORc
696400030806     c                   eval      ar5aas = §lpaas
696500030806     c                   eval      ar5lnp = §lplnp
696600030806     c                   eval      ar5nrs = §lpnrs
696700030806     c                   eval      ar5nsp = §lpnsp
696800030806     c                   write     fiar50
696900030806     c                   end
697000030806     c                   enddo
697100030806     c                   end
697200030806     C                   ENDSR
697300110802     C*---------------------------------------------------------------*
697400110802     C*   duplica rekord di ar4 da riportare su nuova bolla           *
697500110802     C*---------------------------------------------------------------*
697600110802     C     DupAr4        BEGSR
697700110802     C     Kar4dup       setll     FiAR401L
697800110802     C                   do        *hival
697900110802     C     Kar4dup       reade     FiAR401L
698000110802     c                   if        %eof(fiar401l)
698100110802     c                   leave
698200110802     c                   endif
698300110802     C                   MOVEL     '7K'          TBLCOD
698400110802     C                   MOVEL     *BLANK        TBLKEY
698500110802     C                   MOVEL     ar4trc        TBLKEY
698600110802     c     ktab          chain     tabel00f
698700110802     c                   if        %found(tabel00f)
698800110802     c                   movel     tbluni        ds7k
698900110802     c                   if        §7KDUPD = 'S' and vfri = 'D' or
699000110802     c                             §7KDUPR = 'S' and vfri = 'R' or
699100110802     c                             §7KDUPC = 'S' and vfri = 'C' or
699200110802     c                             §7KDREC = 'S' and Wrecsp = 'S'
699300110802     c
699400110802     C                   MOVE      §LPAAS        AR4AAS
699500110802     C                   MOVE      §LPLNP        AR4LNP
699600110802     C                   MOVE      §LPNRS        AR4NRS
699700110802     C                   MOVE      §LPNSP        AR4NSP
699800110802     c                   clear                   AR4DUV
699900110802     c                   clear                   AR4FTR
700000110802     c                   clear                   AR4DTR
700100110802     C                   WRITE     FiAR4000
700200110802     c                   endif
700300110802     c                   endif
700400110802     c                   enddo
700500110802     C                   ENDSR
700600030826     C*-----------------------------------------------------------------*
700700030826     C* AGGORM x particolarità RC aggiorno anche l'orm della bolla orig.*
700800030826     C*-----------------------------------------------------------------*
700900030826     C     aggorm        BEGSR
701000100517
701100100517     c* se particolarità consegna divesra da blan testo il tipo per
701200100517     c* verificare se trattasi di particolarità con ritiro contestuale
701300100517     C                   clear                   DS7R
701400100517     c                   if        arbgma <>*blank
701500100517     C                   MOVEL     '7R'          TBLCOD
701600100517     C                   MOVEL     *BLANK        TBLKEY
701700100517     C                   MOVEL     ARBGMA        TBLKEY
701800100517     C     KTAB          CHAIN     TABEL                              21
701900100517     C     *IN21         IFEQ      '0'
702000100517     C                   MOVEL     TBLUNI        DS7R
702100100517     c                   endif
702200100517     c                   endif
702300100517     c
702400100517     c                   if        §lpnsp <> 0 and §7rrc  = 'S' and
702500031010     c                             (vfri<>'C' or (vfri='C' and §lplna<>arblna))
702600030826     c                   clear                   fnlr99ds
702700030826     c                   eval      i99tla = 'L'
702800030826     c                   eval      i99aas = arbaas
702900030826     c                   eval      i99lnp = arblnp
703000030826     c                   eval      i99nrs = arbnrs
703100030826     c                   eval      i99nsp = arbnsp
703200030826     c                   eval      i99fao = 999
703300071002     c                   eval      i99fgs = filarr
703400030826     c                   if        vfri = 'R'
703500030826     c                   eval      i99car = '91'
703600030826     c                   else
703700030826     c                   eval      i99car = '90'
703800030826     c                   eval      i99lna = §lplna
703900030826     c                   end
704000030826     c                   call      'FNLR99R'
704100030826     c                   parm                    kpjba
704200030826     c                   parm                    fnlr99ds
704300030826     c                   end
704400030826     C                   ENDSR
704500030826     C*---------------------------------------------------------------*
704600030826     C*   N O T D E S           note x modifica destinatario          *
704700030826     C*---------------------------------------------------------------*
704800030826     C     NOTDES        BEGSR
704900960124     C*
705000960124     C*  Prima ragione sociale
705100960124     C                   MOVE      'R'           §TRC
705200060213     C     KAR4          CHAIN     FiAR401L                           21
705300960129     C                   CLEAR                   AR4NOT
705400971124     C                   CLEAR                   AR4DTR
705500971124     C                   CLEAR                   AR4FTR
705600960124     C                   MOVEL     VRSD          AR4NOT
705700960124     C     *IN21         IFEQ      '1'
705800960124     C                   MOVE      §TRC          AR4TRC
705900960124     C                   MOVE      KAAS          AR4AAS
706000960124     C                   MOVE      KLNP          AR4LNP
706100960124     C                   MOVE      KNRS          AR4NRS
706200960124     C                   MOVE      KNSP          AR4NSP
706300060213     C                   WRITE     FiAR4000
706400960124     C                   ELSE
706500060213     C                   UPDATE    FiAR4000
706600960124     C                   END
706700960124     C*  Seconda ragione sociale
706800960125     C     VRD2          IFNE      *BLANKS
706900960124     C                   MOVE      'S'           §TRC
707000060213     C     KAR4          CHAIN     FiAR401L                           21
707100960129     C                   CLEAR                   AR4NOT
707200960124     C                   MOVEL     VRD2          AR4NOT
707300960124     C     *IN21         IFEQ      '1'
707400960124     C                   MOVE      §TRC          AR4TRC
707500960124     C                   MOVE      KAAS          AR4AAS
707600960124     C                   MOVE      KLNP          AR4LNP
707700960124     C                   MOVE      KNRS          AR4NRS
707800960124     C                   MOVE      KNSP          AR4NSP
707900060213     C                   WRITE     FiAR4000
708000960124     C                   ELSE
708100060213     C                   UPDATE    FiAR4000
708200960124     C                   END
708300960125     C                   END
708400960124     C*  Indirizzo
708500960124     C                   MOVE      'T'           §TRC
708600060213     C     KAR4          CHAIN     FiAR401L                           21
708700960129     C                   CLEAR                   AR4NOT
708800960124     C                   MOVEL     VIND          AR4NOT
708900960124     C     *IN21         IFEQ      '1'
709000960124     C                   MOVE      §TRC          AR4TRC
709100960124     C                   MOVE      KAAS          AR4AAS
709200960124     C                   MOVE      KLNP          AR4LNP
709300960124     C                   MOVE      KNRS          AR4NRS
709400960124     C                   MOVE      KNSP          AR4NSP
709500060213     C                   WRITE     FiAR4000
709600960124     C                   ELSE
709700060213     C                   UPDATE    FiAR4000
709800960124     C                   END
709900960124     C*  Località
710000960124     C                   MOVE      'U'           §TRC
710100060213     C     KAR4          CHAIN     FiAR401L                           21
710200960129     C                   CLEAR                   AR4NOT
710300960124     C                   MOVEL     VLOD          AR4NOT
710400960124     C     *IN21         IFEQ      '1'
710500960124     C                   MOVE      §TRC          AR4TRC
710600960124     C                   MOVE      KAAS          AR4AAS
710700960124     C                   MOVE      KLNP          AR4LNP
710800960124     C                   MOVE      KNRS          AR4NRS
710900960124     C                   MOVE      KNSP          AR4NSP
711000060213     C                   WRITE     FiAR4000
711100960124     C                   ELSE
711200060213     C                   UPDATE    FiAR4000
711300960124     C                   END
711400960124     C*  Cap./Provincia/Nazione
711500960124     C                   MOVE      'V'           §TRC
711600060213     C     KAR4          CHAIN     FiAR401L                           21
711700960124     C                   MOVEL     VCAD          WCAD
711800960124     C                   MOVEL     VNZD          WNZD
711900960124     C                   MOVEL     VPRD          WPRD
712000960129     C                   CLEAR                   AR4NOT
712100960124     C                   MOVEL     WNOT          AR4NOT
712200960124     C     *IN21         IFEQ      '1'
712300960124     C                   MOVE      §TRC          AR4TRC
712400960124     C                   MOVE      KAAS          AR4AAS
712500960124     C                   MOVE      KLNP          AR4LNP
712600960124     C                   MOVE      KNRS          AR4NRS
712700960124     C                   MOVE      KNSP          AR4NSP
712800060213     C                   WRITE     FiAR4000
712900960124     C                   ELSE
713000060213     C                   UPDATE    FiAR4000
713100960124     C                   END
713200960124     C*
713300960124     C                   ENDSR
713400940309     C/SPACE 3
713500940309     C*---------------------------------------------------------------*
713600940309     C*   A G G A R T           FILE DETTAGLIO SEGNACOLLI E ANOMALIE  *
713700940309     C*---------------------------------------------------------------*
713800940309     C     AGGART        BEGSR
713900940309     C*
714000941003     C     KBOL          SETLL     FNART01L
714100940309     C                   DO        *HIVAL
714200941003     C                   SETOFF                                           21
714300941006     C     KBOL          READE     FNART01L                               21
714400960618     C  N21              DO
714500950214     C                   Z-ADD     ARBDCM        ARTDCM
714600950214     C                   MOVEL     *BLANKS       ARTDAB
714700950214     C* Se manca data arrivo imposto = Data consegna
714800950214     C     ARTDAM        IFEQ      0
714900950215     C                   Z-ADD     ARBDCM        ARTDAM
715000950214     C                   Z-ADD     85            ARTNAP
715100950214     C                   END
715200970204     C* Se DIROTTAMENTO IMPOSTO 99999999 IN DTR
715300970204     C     VFRI          IFEQ      'D'
715400970204     C                   Z-ADD     99999999      ARTDTR
715500970204     C                   END
715600950214     C                   UPDATE    FNART000
715700940309     C* CONTROLLO SE ESISTONO DELLE ANOMALIE
715800990518     C     KANM          SETLL     FNANM02L
715900960618     C                   DO        *HIVAL
716000940309     C                   SETOFF                                       73
716100990518     C     KANM          READE     FNANM02L                               73
716200940309     C* CHIUDO ANOMALIA
716300960618     C     *IN73         IFEQ      '0'
716400960618     C                   CLEAR                   DSLR33
716500960618     C                   MOVE      '4'           D33FSC
716600960618     C                   Z-ADD     AMGUDT        D33DCH
716700960618     C                   MOVEL     'CO'          D33CCH
716800960618     C                   MOVEL     ARBCCA        D33CCA
716900960618     C                   Z-ADD     ANMNR2        D33NRR
717000960618     C                   CALL      'FNLR33R'
717100960618     C                   PARM                    DSLR33
717200960618     C                   END
717300960618     C  N73              END
717400960618     C                   ENDDO
717500940309     C  N21              END
717600040315     C*
717700940309     C                   ENDSR
717800940309     C/SPACE 3
717900940309     C*---------------------------------------------------------------*
718000941005     C*   E V E N T O  - SCRITTURA EVENTO X DIROTTAM. O CAMBIO PORTO  *
718100940309     C*---------------------------------------------------------------*
718200941005     C     EVENTO        BEGSR
718300941005     C                   MOVEL     *BLANKS       EVBATB
718400941005     C                   Z-ADD     AMGUDT        EVBDTV
718500941005     C                   TIME                    EVBORV
718600021009     C                   MOVE      simpou        EVBFLE
718700021114     C                   Z-ADD     o36cou        EVBCDU
718800941216     C                   Z-ADD     KLNP          EVBLNP
718900941216     C                   Z-ADD     KAAS          EVBAAS
719000941216     C                   Z-ADD     KNRS          EVBNRS
719100941216     C                   Z-ADD     KNSP          EVBNSP
719200941005     C                   Z-ADD     AMGUDT        EVBDEV
719300941005     C                   MOVEL     EVBORV        EVBHEV
719400941005     C     VFRI          IFEQ      'D'
719500941005     C                   MOVEL     'DIR'         EVBCEV
719600941005     C                   ELSE
719700941005     C                   MOVEL     'CPO'         EVBCEV
719800941005     C                   ENDIF
719900941005     C                   MOVEL     *BLANKS       EVBFTR
720000941005     C                   Z-ADD     0             EVBDTR
720100990803     C                   MOVEL     *BLANKS       EVBNOT
720200990803     C                   Z-ADD     0             EVBDTC
720300941005     C*
720400990803     C                   WRITE     FNEVB000
720500941005     C*
720600941005     C                   ENDSR
720700941005     C*---------------------------------------------------------------*
720800941005     C*   P G M N A C                                                 *
720900941005     C*---------------------------------------------------------------*
721000941005     C     RCHNAC        BEGSR
721100940309     C*
721200940309     C                   MOVEL     *ALL'?'       DLNP
721300940309     C     ARBLNP        CHAIN     AZORG01L                           21
721400940309     C  N21              MOVEL     ORGDES        DLNP             15
721500940309     C   21              MOVEL     *ALL'?'       DLNP
721600940309     C     ARBLNA        CHAIN     AZORG01L                           21
721700940309     C  N21              MOVEL     ORGDES        DLNA             15
721800940309     C   21              MOVEL     *ALL'?'       DLNA
721900951214     C     WSVLNA        CHAIN     AZORG01L                           21
722000940309     C  N21              MOVEL     ORGDES        DNWLNA           15
722100940309     C   21              MOVEL     *ALL'?'       DNWLNA
722200941006     C                   MOVE      AR6DFT        G08INV
722300941005     C                   MOVE      *ZEROS        G08DAT
722400941005     C                   MOVEL     '3'           G08ERR
722500941005     C                   CALL      'XSRDA8'
722600941005     C                   PARM                    WLBDA8
722700941005     C                   Z-ADD     G08DAT        GMADFT            8 0
722800940630     C     VNPC          COMP      'F'                                    28
722900940309     C* DA MANDARE DIRETTAMENTE A SEDE : RECUPERO GIA FATTO
723000950301     C  N98VFRI          IFEQ      'R'
723100940309     C                   SETON                                        86
723200940309     C                   END
723300941011      * SE HO DIROTTATO IN ASSEGNATO O RICONS. IN ASSEGNATO
723400950301     C  N98VFRI          IFEQ      'D'
723500940309     C     VFRI          OREQ      'C'
723600940309     C     VNPC          IFEQ      'A'
723700940309     C     VNPC          OREQ      ' '
723800940309     C     EXPORT        ANDEQ     'A'
723900941221      * RICH. NOTA ACCREDITO 2° BOLLA E PROVV. AL DESTIN.
724000941221     C     *IN37         OREQ      '1'
724100941221     C     VPPC          ANDEQ     'D'
724200940309     C                   SETON                                        86
724300940309     C                   END
724400940309     C                   END
724500940623     C*   Stampa Nota Accredito
724600950301     C     *IN98         IFEQ      '0'
724700991001     C                   Z-ADD     ARBKSC        CLIENT            7 0
724800991001     C     AR6KSC        IFNE      0
724900991001     C                   Z-ADD     AR6KSC        CLIENT            7 0
725000991001     C                   END
725100000131     C                   Z-ADD     0             TOTIMP           12 3
725200000131     C                   Z-ADD     0             TOTESE           12 3
725300000131     C                   Z-ADD     0             TOTNAC           12 3
725400940623     C     *IN32         IFEQ      '1'
725500991001     C                   Z-ADD     1             XS
725600991130     C     §$2IVA        LOOKUP    ASV(XS)                                20
725700991001     C     *IN20         IFEQ      '1'
725800020108     C     AR6IFT        SUB       AR6IMV        IMPALI           10 3
725900991001     C                   END
726000991027     C                   Z-ADD     §INAC1        INAC             12 3
726100991027     C* VERIFICO SE IMPORTO HA DECIMALI
726200991027     C                   MOVE      INAC          WCOMO3            3 0
726300991027     C     WCOMO3        COMP      0                                      11
726400941005     C                   Z-ADD     AR6ALI        ALIQ              3 1
726500020621     C                   EVAL      CdIVA = %EDITC(Aliq:'2')
726600941005     C                   MOVEL     AR6CEI        TPALI             1
726700940623     C                   END
726800940623     C     *IN37         IFEQ      '1'
726900991001     C                   Z-ADD     1             XS
727000991130     C     §$2IVA        LOOKUP    ASV(XS)                                20
727100991001     C     *IN20         IFEQ      '1'
727200020108     C     AR6IFT        SUB       AR6IMV        IMPALI
727300991001     C                   END
727400991027     C                   Z-ADD     §INAC2        INAC
727500991001     C                   Z-ADD     AR6ALI        ALIQ              3 1
727600020621     C                   EVAL      CdIVA = %EDITC(Aliq:'2')
727700991001     C                   MOVEL     AR6CEI        TPALI             1
727800940623     C                   END
727900011130      * STAMPA DOPPIO IMPORTO SE DIVISA NON DI CONTABILITÀ
728000011130     C                   CLEAR                   WAR6IM
728100011130     C     AR6DIV        IFNE      §GEDMC
728200011130     C                   CLEAR                   YEURDS
728300011130     C                   MOVEL     AR6DIV        YECDVI
728400011130     C                   Z-ADD     AR6IFT        YECIMI
728500011212     C                   MOVEL     §GEDMC        YECDVO
728600011130     C                   MOVEL     'H'           YECTAR
728700011130     C                   CALL      'YEURCO'
728800011130     C                   PARM                    YEURDS
728900011130     C                   Z-ADD     YECIMO        WAR6IM           13 3
729000011130     C                   ENDIF
729100011130      *
729200940623     C     TPALI         IFEQ      *BLANKS
729300000131     C                   Z-ADD     INAC          TOTIMP
729400940623     C                   ELSE
729500000131     C                   Z-ADD     INAC          TOTESE
729600940624     C                   Z-ADD     1             XX
729700940624     C     TPALI         LOOKUP    CEI(XX)                                41
729800940624     C     *IN41         IFEQ      '1'
729900020621     C                   MOVEL     ESE(XX)       CdIVA
730000940623     C                   END
730100940624     C                   MOVEL     'E'           TPALI
730200940624     C                   END
730300040315     c                   clear                   Worigine
730400040316     c                   clear                   Wpocompl
730500940623     C     VFRI          IFEQ      'D'
730600040423     c                   eval      worigine = 'DIROTTATA A ' + dnwlna
730700040317      *se dirottamento e linea arrivo 101 forzo sede
730800040317     c                   if        arblna = 101
730900040316     c                   eval      wpocompl = 046
731000040316     c                   endif
731100940623     C                   END
731200040316
731300940623     C     VFRI          IFEQ      'C'
731400040423     c   28              eval      worigine = 'CONSEGNATA IN FRANCO A ' + DNWLNA
731500040423     c  N28              eval      worigine = 'CONSEGNATA A: ' + DNWLNA
731600940623     C                   END
731700940623     C     VFRI          IFEQ      'R'
731800960327     C     V4CRCS        IFEQ      'N'
731900040315     c                   eval      worigine = 'LA MERCE E'' STATA RESA AL -
732000040315     C                             MITTENTE '
732100960327     C                   ELSE
732200040315     c                   eval      worigine = 'LA MERCE E'' STATA RESA AL -
732300040315     C                             MITTENTE IN C/SERVIZIO'
732400960327     C                   END
732500960327     C                   END
732600011130      * STAMPA DOPPIO IMPORTO SE DIVISA NON DI CONTABILITÀ
732700011130     C                   CLEAR                   WAR6IM
732800011130     C     AR6DIV        IFNE      §GEDMC
732900011130     C                   CLEAR                   YEURDS
733000011130     C                   MOVEL     AR6DIV        YECDVI
733100011130     C                   Z-ADD     INAC          YECIMI
733200011212     C                   MOVEL     §GEDMC        YECDVO
733300011130     C                   MOVEL     'H'           YECTAR
733400011130     C                   CALL      'YEURCO'
733500011130     C                   PARM                    YEURDS
733600011130     C                   Z-ADD     YECIMO        WAR6IM
733700011130     C                   ENDIF
733800011130      *
733900940623     C     TOTIMP        ADD       IMPALI        TOTNAC
734000940623     C                   ADD       TOTESE        TOTNAC
734100000131     C* VERIFICO SE IMPORTI CON DECIMALI
734200000131     C                   MOVE      TOTIMP        WCOMO3            3 0
734300000131     C     WCOMO3        COMP      0                                      08
734400000131     C                   MOVE      IMPALI        WCOMO3            3 0
734500000131     C     WCOMO3        COMP      0                                      09
734600000131     C                   MOVE      TOTESE        WCOMO3            3 0
734700000131     C     WCOMO3        COMP      0                                      10
734800000131     C                   MOVE      TOTNAC        WCOMO3            3 0
734900000131     C     WCOMO3        COMP      0                                      11
735000950301     C                   ELSE
735100950301     C                   END
735200040312      * scrittura file NCR
735300040312     c                   clear                   fnncr000
735400040315     c                   move      '201'         nctsocieta
735500040315     c                   z-add     arblna        nctfil
735600040316     c                   if        wpocompl <> *zeros
735700040316     c                   z-add     wpocompl      nctpocompl
735800040316     c                   else
735900040315     c                   z-add     arblnp        nctpocompl
736000040316     c                   endif
736100040315     c                   move      amgudt        nctddc
736200040315     c                   if        ar6dft > 0
736300040315     c                   move      ar6dft        nctdrp
736400040315     c                   end
736500040315     c                   z-add     ar6nft        nctnrp
736600040315     c                   z-add     ar6fiv        nctfpn
736700040315     c                   z-add     ar6ift        nctimr
736800040315     c                   move      *zeros        nctksc
736900040315     c                   move      ar6ksc        nctksc
737000040315     c                   move      *zeros        nctscf
737100040315     c                   move      ar6ksc        nctscf
737200040312     c                   move      knmus         nctutente
737300040312     c                   move      'I '          ncttnr
737400040312     c                   move      'C'           ncrrna
737500040312     c                   move      '3'           ncrcmn
737600040312      *reso
737700040429     c                   if        vfri ='R'
737800040312     c                   move      '4'           ncrcmn
737900040429     c                   endif
738000040429      *reso ma in C/S
738100040429     c                   if        vfri ='R' and v4crcs = 'S'
738200040429     c                   move      '0'           ncrcmn
738300040429     c                   move      'D'           ncrrna
738400040312     c                   endif
738500040312      *dirottamento non da giacenza
738600040312     c                   if        vfri ='D' and not *in90 and viddcli = 'V'
738700040312     c                   move      'A'           ncrrna
738800040312     c                   endif
738900040916      *se dirottamento e linea arrivo 101 forzo causale
739000041221     c                   if        arblna = 101 and vfri = 'D' or
739100041221     c                             §lplna = 101 and vfri = 'D'
739200040916     c                   move      '5'           ncrcmn
739300040916     c                   move      'C'           ncrrna
739400040916     c                   endif
739500040312      * storno spese giacenza
739600040312     c                   z-add     ar6imv        ncrim2
739700040312     c                   z-add     ar6ali        ncrali
739800040312     c                   z-add     arblnp        ncrflr
739900040312     c                   z-add     exaas         ncraassto
740000040312     c                   z-add     exlnp         ncrlnpsto
740100040315     c                   z-add     exnrs         ncrnrssto
740200040312     c                   z-add     exnsp         ncrnspsto
740300040429      *con causale 'D0' la spedizione di recupero non viene impostata
740400040429      *
740500040429     c                   if        ncrrna = '0' and ncrcmn = 'D'
740600040429     c                   z-add     0             ncraasrec
740700040429     c                   z-add     0             ncrlnprec
740800040429     c                   z-add     0             ncrnrsrec
740900040429     c                   z-add     0             ncrnsprec
741000040429     c                   z-add     0             nchlnarec
741100040429     c                   else
741200040429     c                   select
741300040429     c                   when      vfri = 'D' and vnpc = 'F'
741400040429     c                   z-add     0             ncraasrec
741500040429     c                   z-add     0             ncrlnprec
741600040429     c                   z-add     0             ncrnrsrec
741700040429     c                   z-add     0             ncrnsprec
741800040429     c                   z-add     0             nchlnarec
741900100514     c*                  when      vfri = 'D' and vnpc = 'A'
742000100514     c*                  z-add     exaas         ncraasrec
742100100514     c*                  z-add     exlnp         ncrlnprec
742200100514     c*                  z-add     exnrs         ncrnrsrec
742300100514     c*                  z-add     exnsp         ncrnsprec
742400100514     c*                  z-add     exlna         nchlnarec
742500040429     c                   other
742600040429     c   86              z-add     §lpaas        ncraasrec
742700040316     c   86              z-add     §lplnp        ncrlnprec
742800040316     c   86              z-add     §lpnsr        ncrnrsrec
742900040316     c   86              z-add     §lpnsp        ncrnsprec
743000040316     c   86              z-add     §lplna        nchlnarec
743100040429     c                   endsl
743200040429     c                   endif
743300040315      * se impostata la spedizione di recupero la richiesta si considera
743400040315      * completa
743500040315     c                   if        ncrnsprec > 0
743600040315     c                   move      '1'           nchricompl
743700040315     c                   else
743800040315     c                   move      '0'           nchricompl
743900040315     c                   endif
744000040315
744100040315     c                   move      ar6cei        nchcei
744200040312     c                   z-add     wsvaas        nchaasfgl
744300040312     c                   z-add     wsvlnp        nchlnpfgl
744400040312     c                   z-add     wsvnrs        nchnrsfgl
744500040312     c                   z-add     wsvnsp        nchnspfgl
744600040312     c                   z-add     wsvlna        nchlnafgl
744700040312     c   90              z-add     gcpagc        nchagc
744800040312     c   90              z-add     gcpfgc        nchfgc
744900040312     c   90              z-add     gcpngc        nchngc
745000040315     c                   movel     worigine      nchorigine
745100040312     c                   write     fnncr000
745200940623     C*
745300941011     C                   SETOFF                                           86
745400940309     C*
745500940309     C                   ENDSR
745600940309     C/SPACE 3
745700940309     C*---------------------------------------------------------------*
745800940309     C*   P G M R E C                                                 *
745900940309     C*---------------------------------------------------------------*
746000940309     C     RCHREC        BEGSR
746100940309     C*
746200970715     C* Stampo richiesta di recupero solo se non dirotto alla 101
746300970715     C     VLNA          IFNE      101
746400970715     C     VFRI          ORNE      'D'
746500970715     C*
746600940309     C                   MOVEL     *ALL'?'       DLNP
746700940309     C     ARBLNP        CHAIN     AZORG01L                           21
746800940309     C  N21              MOVEL     ORGDES        DLNP             15
746900940309     C   21              MOVEL     *ALL'?'       DLNP
747000940309     C     ARBLNA        CHAIN     AZORG01L                           21
747100940309     C  N21              MOVEL     ORGDES        DLNA             15
747200940309     C   21              MOVEL     *ALL'?'       DLNA
747300940309     C     §LPLNA        CHAIN     AZORG01L                           21
747400940309     C  N21              MOVEL     ORGDES        DNWLNA           15
747500940309     C   21              MOVEL     *ALL'?'       DNWLNA
747600941006     C                   MOVE      AR6DFT        G08INV
747700941005     C                   MOVE      *ZEROS        G08DAT
747800941005     C                   MOVEL     '3'           G08ERR
747900941005     C                   CALL      'XSRDA8'
748000941005     C                   PARM                    WLBDA8
748100941005     C                   Z-ADD     G08DAT        GMADFT
748200040429      * stampo solo se non richiedo la nota di credito
748300040429     c                   if        not *in32 and not *in37
748400940309     C                   EXCEPT    RECTES
748500940309     C   84              EXCEPT    DIRTES
748600960126     C   85              EXCEPT    RICTES
748700940309     C                   EXCEPT    RECDET
748800940309     C   84              EXCEPT    DIRDET
748900960126     C   85              EXCEPT    RICDET
749000941221     C   89              EXCEPT    RECPRO
749100040429     c                   endif
749200940309     C* SE DIROTTO IN FRANCO E NO CAMBIO PORTO NON HO STORNATO BOLLA
749300940309     C  N32
749400940309     CANN37VFRI          IFEQ      'D'
749500940309     C     EXPORT        ANDEQ     'F'
749600940309     C     VNPC          ANDEQ     ' '
749700940309     C                   SETON                                        69
749800940309     C                   END
749900040429      * stampo solo se non richiedo la nota di credito
750000040429     c                   if        not *in32 and not *in37
750100940309     C                   EXCEPT    RECFIN
750200040429     c                   endif
750300040429
750400940309     C                   SETOFF                                       69
750500970715     C*
750600970715     C                   END
750700970715     C*
750800940309     C                   ENDSR
750900951213     C*---------------------------------------------------------------*
751000951213     C*   R E C T R A  CREO BOLLA RECUPERO SPESE X CAMBIO PORTO A -> F*
751100951213     C*---------------------------------------------------------------*
751200951213     C     RECTRA        BEGSR
751300951213     C*
751400951214     C                   SETON                                        86
751500990930     C                   CLEAR                   DBLP
751600031121     C                   CLEAR                   vteld
751700031121     C                   CLEAR                   vref
751800951213     C                   MOVEL     'E '          §LPCBO
751900160908      **
752000160908      *** Bolla di recupero colpa del cliente
752100020617     c                   if        viddcli = 'C' and *in92
752200020617     c                   move      'F*'          §lpcbo
752300020617     c                   end
752400951213     C                   MOVEL     '4'           §LPCTM
752500160908      **
752600951213      *  Inverto mittente e destinatario
752700951213     C                   MOVEL     ARBRSD        §LPRSM
752800951213     C                   MOVEL     ARBIND        §LPINM
752900951213     C                   MOVEL     ARBCAD        §LPCAM
753000951213     C                   MOVEL     ARBLOD        §LPLOM
753100951213     C                   MOVEL     ARBPRD        §LPPRM
753200951213     C                   MOVEL     ARBNZD        §LPNZM
753300951213     C                   MOVEL     ARBRSM        §LPRSD
753400951213     C                   MOVEL     ARBINM        §LPIND
753500951213     C                   MOVEL     ARBCAM        §LPCAD
753600951213     C                   MOVEL     ARBLOM        §LPLOD
753700951213     C                   MOVEL     ARBPRM        §LPPRD
753800951213     C                   MOVEL     ARBNZM        §LPNZD
753900960205     C     WSVLPO        IFNE      ARBLNA
754000951228     C                   MOVEL     WSVLPO        §LPLNA
754100960205     C                   ELSE
754200960205     C                   MOVEL     ARBLNP        §LPLNA
754300960205     C                   END
754400020225     C     §LPLNA        CHAIN     AZORG01L                           21
754500020225     C     *IN21         IFEQ      '0'
754600020225     C                   MOVE      ORGDE3        OG143
754700020225     C     §OGNTW        IFEQ      'FED'
754800020225     C                   MOVE      'S'           I95FI1
754900020225     C                   END
755000020225     C                   END
755100951213      *  Calcolo zona consegna
755200970710     C                   CLEAR                   TISI95
755300970710     C                   MOVEL     '3'           I95TCN
755400970710     C                   MOVEL     *BLANK        I95TLA
755500970710     C                   MOVEL     VNZD          I95NAR
755600970710     C                   MOVEL     VIND          I95IND
755700970710     C                   MOVEL     VCAD          I95CAP
755800970710     C                   MOVEL     VLOD          I95LOC
755900970710     C                   MOVEL     VPRD          I95PRV
756000970710     C                   MOVEL     ARBPKF        I95LKG
756100970710     C                   MOVEL     ARBVLF        I95LMC
756200970710     C                   MOVEL     ARBFFD        I95FFD
756300970710     C                   MOVEL     ARBTSP        I95TSP
756400031021     C                   MOVEL     ARBtc1        I95fi2
756500031021     C                   MOVE      ARBtc2        I95fi2
756600970710     C                   CALL      'TISI95R'
756700970710     C                   PARM                    TISI95
756800970710     C                   MOVEL     O95ZNC        §LPZNC
756900051005      * BOLLA RECUPERO
757000051005     C                   MOVEL     'S'           WRECSP
757100951214      *  Imposto note generiche
757200951214     C                   EXSR      XLPNOT
757300951214      *  Imposto note bolla recupero
757400960206     C                   MOVEL     *BLANKS       §LPNT2
757500960206     C     ARBCCM        IFNE      0
757600951214     C                   MOVEL     CNOT1         §LPNT2
757700951214     C                   MOVE      CNOT2         §LPNT2
757800951227     C                   MOVE      ARBCCM        §LPNT2
757900960206     C                   END
758000160908      **
758100951213      *  Richiamo routine impostazione parametri call pgm imm.bolle
758200951213     C                   EXSR      PGNBLP
758300951213     C*
758400951213     C                   ENDSR
758500940308     C*---------------------------------------------------------------*
758600940308     C*   L E G B O L  LEGO LA NUOVA BOLLA CREATA                     *
758700940308     C*---------------------------------------------------------------*
758800940308     C     LEGBOL        BEGSR
758900940308     C     §LPNSP        IFGT      0
759000941005      * GUARDO SE ESISTE LEGAME PER REPERIRE I RIFERIM. DELLA BOLLA
759100941005      * ORIGINALE ,ALTRIMENTI LA BOLLA ORIGINALE è LA SPEDIZ. TRATTATA
759200941005     C     KBOL          CHAIN     FNLBL01L                           21
759300941005     C     *IN21         IFEQ      '1'
759400941216     C                   MOVE      KLNP          LBLLPO
759500941216     C                   MOVE      KAAS          LBLAAO
759600941216     C                   MOVE      EXMGS         LBLMGO
759700941216     C                   MOVE      KNRS          LBLNRO
759800941216     C                   MOVE      KNSP          LBLNSO
759900941216     C                   MOVE      EXLNA         LBLLAO
760000941005     C                   ENDIF
760100951228     C                   MOVE      LBLLPO        WSVLPO
760200951228     C                   MOVE      §LPLNP        LBLLPN
760300940308     C                   MOVE      §LPAAS        LBLAAN
760400940308     C                   MOVE      §LPMGS        LBLMGN
760500940308     C                   MOVE      §LPNRS        LBLNRN
760600940308     C                   MOVE      §LPNSP        LBLNSN
760700940308     C                   MOVE      §LPLNA        LBLLAN
760800941216     C                   MOVE      KLNP          LBLLPP
760900941216     C                   MOVE      KAAS          LBLAAP
761000941216     C                   MOVE      EXMGS         LBLMGP
761100941216     C                   MOVE      KNRS          LBLNRP
761200941216     C                   MOVE      KNSP          LBLNSP
761300941216     C                   MOVE      EXLNA         LBLLAP
761400941006     C                   MOVE      *BLANK        LBLFT1
761500941006     C                   MOVE      *ZEROS        LBLDT1
761600941006     C                   MOVE      *BLANK        LBLFT2
761700941006     C                   MOVE      *ZEROS        LBLDT2
761800950331     C                   CLEAR                   DS3A
761900950331     C                   Z-ADD     1             §F
762000950331     C     EXCBO         LOOKUP    CDB(§F)                                04
762100950331     C   04              MOVEL     D3A(§F)       DS3A
762200950331     C     §3ABSD        IFEQ      'N'
762300950525     C                   Z-ADD     99999999      LBLDT2
762400950331     C                   END
762500941005     C                   WRITE     FNLBL000
762600041125     c* aggiornamento titas con flag legame bolla
762700041125     c                   call      'FNLG25R'
762800041125     C                   PARM                    recordlbl
762900041125     C                   PARM      ' '           I25tla            1
763000041125     c
763100940308     C                   END
763200940308     C                   ENDSR
763300940308     C*---------------------------------------------------------------*
763400941005     C*   A R R B O L  CREO SUBITO BOLLA IN ARRIVO                    *
763500940308     C*---------------------------------------------------------------*
763600940308     C     ARRBOL        BEGSR
763700941005      * RICH. PGM X SCRITTURA BOLLA IN ARRIVO
763800941005     C                   CLEAR                   DSUL41
763900941216     C                   MOVEL     §LPAAS        U41AAS
764000941216     C                   MOVEL     §LPLNP        U41LNP
764100941216     C                   MOVEL     §LPNRS        U41NRS
764200941216     C                   MOVEL     §LPNSP        U41NSP
764300941005     C                   CALL      'TRUL41R'
764400941005     C                   PARM                    DSUL41
764500070614      * duplica eventuali note ldv su bolla figlia
764600070614     c                   if        §oggeot = 'S'
764700070614     c                   exsr      dupnotldv
764800070614     c                   endif
764900940308     C*
765000941005     C* MI RITORNA IL TIPO BOLLA:RICHIAMO PGM TASSAZIONE
765100941227     C     U41TPB        IFEQ      'A'
765200941006     C     VSIFAT        ANDEQ     'S'
765300030715     C                   MOVEL     'FAT999'      MODFAT            9
765400030715     C                   MOVE      FILarr        MODFAT            9
765500030715     c                   movel(p)  modfat        com10            10
765600030715     C                   Z-ADD     80            LUNCMD           15 5
765700030715     C* OVRPRTF  FAT999 old
765800030715     C                   MOVEA     D82PRB        CMb(31)
765900030715     C                   MOVEA     com10         CMb(64)
766000991206     C                   CALL      'QCMDEXC'
766100030715     C                   PARM                    CMb
766200991206     C                   PARM                    LUNCMD
766300030715     C* OVRPRTF  FAT999 a4
766400030715     C                   MOVEA     D82PRB4       CM4(31)
766500030715     C                   MOVEA     com10         CM4(64)
766600030715     C                   CALL      'QCMDEXC'
766700030715     C                   PARM                    CM4
766800030715     C                   PARM                    LUNCMD
766900030715     C* OVRPRTF  FAT999 a5
767000030715     C                   MOVEA     D82PRB5       CM5(31)
767100030715     C                   MOVEA     com10         CM5(64)
767200030715     C                   CALL      'QCMDEXC'
767300030715     C                   PARM                    CM5
767400030715     C                   PARM                    LUNCMD
767500991206     C* OVRPRTF  FAT999 DPD
767600030715     C                   MOVEA     D82PRB        CMp(31)
767700940308     C                   CALL      'QCMDEXC'
767800030715     C                   PARM                    CMp
767900940308     C                   PARM                    LUNCMD
768000030304     C* OVRPRTF  FAT999 FedEx
768100030307     c                   Movel     *Blanks       comman           90
768200030307     C                   Z-ADD     90            LUNCMD           15 5
768300030715     C                   MOVEA     D82PRB        CMpf(31)
768400030715     c                   Movea     Cmpf          Comman
768500030307     c                   Eval      %Subst(Comman:77:9) = 'COPIES(2)'
768600030304     C                   CALL      'QCMDEXC'
768700030307     c                   PARM                    Comman
768800030304     C                   PARM                    LUNCMD
768900940308     C*
769000050511     C                   CLEAR                   fnlra2ds
769100050511     C                   MOVEL     FILARR        ILRa2FGS
769200050511     C                   MOVE      §LPAAS        ILRa2AAS
769300050511     C                   MOVE      §LPLNP        ILRa2LNP
769400050511     C                   MOVE      §LPNRS        ILRa2NRS
769500050511     C                   MOVE      §LPNSP        ILRa2NSP
769600050511     C                   MOVEL     'TB'          ILRa2CVB
769700050228     C******             MOVEL     'T'           D41GES
769800050511     C                   MOVEL     fnlra2ds      KPJBU
769900050228     C*
770000050511     C                   CALL      'FNLRA2R'
770100050228     C                   PARM                    KPJBA
770200050228     C                   PARM                    trul90ds
770300050511     C                   MOVEL     KPJBU         fnlrA2DS
770400940308     C* DLTOVR
770500941013     C                   Z-ADD     80            LUNCMD           15 5
770600940308     C                   CALL      'QCMDEXC'
770700940308     C                   PARM                    DOV
770800940308     C                   PARM                    LUNCMD
770900940311     C                   END
771000991028      * AGGIORNO LA BOLLA IN PARTENZA COME TRASMESSA
771100991028     C     KBLP          CHAIN     FNBLP01L                           38
771200991028     C     *IN38         IFEQ      '0'
771300991028     C                   MOVEL     'T'           BLPFT1
771400991028     C                   Z-ADD     AMGUDT        BLPDT1
771500991028     C                   EXCEPT    AGGTRA
771600991028     C                   END
771700940311      * STAMPA BOLLA  SE NON HO FATTO  FATTURA
771800940311     C     VSIFAT        IFEQ      'S'
771900941228     C     U41TPB        ANDEQ     'A'
772000050511     C     olrA2DFT      ANDLE     *ZEROS
772100940613     C     VSIFAT        OREQ      'N'
772200941227     C     U41TPB        ANDEQ     'A'
772300941227     C     U41TPB        OREQ      'F'
772400000313     C* Stampo la bolla in arrivo se non DPD e no Euroexpress
772500000313     C     NLAFL1        IFNE      'E'
772600020617     C     §OGntw        ANDNE     'DPD'
772700000313     C* oppure Euroexpress e ma si ddt
772800000313     C     NLAFL1        OREQ      'E'
772900000313     C     §LPBAM        ANDNE     'N'
773000000313     C     §LPBAM        ANDNE     'Y'
773100000313     C     §LPBAM        ANDNE     'K'
773200130315     c                   reset                   FNLSB6ds1
773300030624     C                   CLEAR                   FNLSB5DS
773400941215     C                   MOVEL     ' '           DB0TLA
773500941215     C                   MOVEL     ' '           DB0GES
773600941215     C                   MOVEL     'A'           DB0TBO
773700941227     C                   MOVE      §LPAAS        DB0AAS
773800941227     C                   MOVE      §LPLNP        DB0LNP
773900941227     C                   MOVE      §LPNRS        DB0NRS
774000941227     C                   MOVE      §LPNSP        DB0NSP
774100991208      *
774200030715     C                   CALL      d90psl
774300030624     C                   PARM                    FNLSB5DS
774400130315     c                   parm                    FNLSB6ds1
774500000313     C                   END
774600940308     C                   END
774700940308     C                   ENDSR
774800940308     C*---------------------------------------------------------------*
774900940308     C*   S T A B L P  STAMPA BOLLA LOCALE SE NON CREATA IN ARRIVO    *
775000940308     C*---------------------------------------------------------------*
775100940308     C     STABLP        BEGSR
775200000313     C* per lna Euroexpress no ddt o DPD non devo ristampare
775300000313     C* la bolla
775400000313     C     EXLNA         CHAIN     AZORG01L                           30
775500000313     C                   MOVEL     ORGDE3        OG143
775600000313     C* No Dpd / no euroexoress oppure
775700020617     C     §OGntw        IFNE      'DPD'
775800000313     C     NLAFL1        ANDNE     'E'
775900000313     C* Euroexpress si DDT
776000000313     C     NLAFL1        OREQ      'E'
776100000313     C     §LPBAM        ANDNE     'N'
776200000313     C     §LPBAM        ANDNE     'Y'
776300000313     C     §LPBAM        ANDNE     'K'
776400130315     c                   reset                   FNLSB6ds1
776500030624     C                   CLEAR                   FNLSB5DS
776600941215     C                   MOVEL     ' '           DB0TLA
776700941215     C                   MOVEL     ' '           DB0GES
776800941215     C                   MOVEL     'P'           DB0TBO
776900941227     C                   MOVE      §LPAAS        DB0AAS
777000941227     C                   MOVE      §LPLNP        DB0LNP
777100941227     C                   MOVE      §LPNRS        DB0NRS
777200941227     C                   MOVE      §LPNSP        DB0NSP
777300941215     C*
777400030715     C                   CALL      d90psl
777500030624     C                   PARM                    FNLSB5DS
777600130315     c                   parm                    FNLSB6ds1
777700000313     C                   END
777800000313     C*
777900940308     C                   ENDSR
778000900504     C/SPACE 3
778100900504     C*---------------------------------------------------------------*
778200900504     C*   G C P W T R                                                 *
778300900504     C*---------------------------------------------------------------*
778400900504     C     GCPWTR        BEGSR
778500050316     C     KGIA          CHAIN(e)  tigcp000                           40
778600050316     c                   if        %error
778700070521     C     KGIA          CHAIN(n)  tigcp000
778800050316     c                   exsr      geslck
778900050318     C     KGIA          CHAIN(e)  tigcp000                           40
779000050316     c                   endif
779100171003      * imposta dati ds before per monitor variazioni
779200171003     c                   eval      tigcpb = tigcp
779300941229     C* SE FORZATE  DISPOSIZIONI DA CONS. A DIROTT E VICEVERSA
779400941006     C     GCPDIS        IFNE      VFRI
779500941006     C                   MOVEL     VFRI          GCPDIS
779600941229     C                   MOVEL     VNPC          GCPNPC
779700941229     C                   MOVEL     VASG          GCPASG
779800941229     C                   MOVEL     VPSG          GCPPSG
779900940308     C                   END
780000941229     C* DATA CHIUSURA GIACENZA
780100050405     C                   MOVEL     VCFG          GCPCFG
780200050405     c                   if        spearr = 1
780300050405     C                   Z-ADD     VSGR          GCPSGR
780400050405     C                   Z-ADD     VGGS          GCPGGS
780500050405     C                   Z-ADD     VSGF          GCPSGF
780600050405     C                   Z-ADD     VTSGS         GCPSGS
780700050405     C                   Z-ADD     VSGR          GCPSGR
780800050405     C                   Z-ADD     VSGD          GCPSGD
780900911118     C                   Z-ADD     §TFT          GCPTFT
781000900423     C                   Z-ADD     AMGDSD        GCPDSD
781100900423     C                   Z-ADD     AMGDSA        GCPDSA
781200050405     c                   end
781300050405     C                   Z-ADD     AMGUDT        GCPDCG
781400050222     C                   Z-ADD     §LPLNP        GCPLPWa
781500050222     C                   Z-ADD     §LPAAS        GCPASWa
781600050222     C                   Z-ADD     §LPNRS        GCPNSWa
781700050222     C                   Z-ADD     §LPNSP        GCPNBWa
781800050222     C                   MOVE      AMGUDT        GCPDeda
781900050408     C* imposto prima fase =35
782000050408     C                   Z-ADD     035           GCPFAS
782100050414     C                   Z-ADD     0             GCPduma
782200050408     c* però se è un reso poichè in partenza non si devono rec. spese fase =50
782300050408     C     VCFG          IFEQ      '002'
782400050408     C                   Z-ADD     050           GCPFAS
782500050408     C                   END
782600050408     c*      se è dirottamento e la partenza ha già recuperato  fase =50
782700050408     C     VCFG          ifEQ      '006'
782800050408     C     VLNA          ANDNE     GCPLNA
782900050408     C     gcpnbw        ifne      0
783000050408     C                   Z-ADD     050           GCPFAS
783100050408     c                   else
783200050408     c*     se è dirottamento e la partenza non ha recuperato  fase =40
783300050408     C                   Z-ADD     040           GCPFAS
783400050408     C                   END
783500050408     C                   END
783600050222     C  N40              UPDATE    tigcp000
783700171003     c                   exsr      RegVariazioni
783800050411     c* scrivo il FIGIC00F chiusura giacenza
783900050411     c                   if        not *in40 and (gcpfas = 040 or
784000050411     c                             gcpfas = 050 )
784100050411     c                   exsr      gicwtr
784200050411     c                   end
784300050316     c                   if        gescomm = 'S'
784400050316     c                   commit
784500050316     c                   endif
784600051013     c* richiamo il pgm di variazione bolla se disposizioni automatiche, se
784700051019     c* data consegna richiesta o f.d. e disposizioni = consegna
784800051013     c                   movel     gcpflr        dgcpflr
784900051013     c                   if        d§gcpdisa = 'S' and flgdis <> ' '
785000051019     c                             and gcpdis = 'C'
785100051013     c                   exsr      srvarbol
785200051013     c                   end
785300941229     C* CONTROLLO SE C'ERANO ALTRE SPEDIZIONI FIGLIE
785400941229     C* GENERATE IN PRECEDENZA
785500950301WW   C                   SETOFF                                       98
785600941229     C     KBOL          CHAIN     FNLBL02L                           21
785700941229     C     *IN21         DOWEQ     *OFF
785800950102     C* ESCLUDO LA BOLLA APPENA CREATA
785900950102     C     LBLNSN        IFNE      §LPNSP
786000950414     C     LBLLAN        ANDEQ     EXLNA
786100950421      * RICH. NOTE ACCREDITO SU PRECEDENTI FIGLIE
786200950307      *************
786300950414     C                   EXSR      RCHNCF
786400950307      *************
786500950102     C                   ENDIF
786600941229     C     KBOL          READE     FNLBL02L                               21
786700941229     C                   ENDDO
786800950301WW   C                   SETOFF                                       98
786900900418     C                   ENDSR
787000171003     C*__________________________________________________________
787100171003     c     RegVariazioni begsr
787200171003     C*__________________________________________________________
787300171003     c                   eval      ILG33CTA = 'M'
787400171003     c                   eval      ILG33PRU = knmus
787500171003     c                   eval      ILG33NOJ = knmeb
787600171003     c                   eval      ILG33PGM = 'FNLG20R'
787700171003     c                   eval      tigcpa = tigcp
787800171003     c                   call      'FNLG33R'
787900171003     c                   parm                    fnlg33ds
788000171003     c                   parm                    tigcpb
788100171003     c                   parm                    tigcpa
788200171003     c                   endsr
788300051013     C*---------------------------------------------------------------*
788400051013     C     srvarbol      BEGSR
788500051013     C*---------------------------------------------------------------*
788600051013     C                   clear                   DSARBD
788700051013     C                   clear                   DSARBK
788800051013     C                   clear                   DARBT
788900051013     C                   clear                   DSARBG
789000051013     C                   clear                   DSlr48
789100051013     c* disposizioni automatiche per f.d.
789200051013     C                   IF        flgdis = 'F'
789300051017     C                   MOVEL     'E'           D48FFR
789400060314     c                   movel     *blanks       vteld
789500051017     C                   EXSR      PGARBI
789600051013     c                   end
789700051013     c* disposizioni automatiche per data consegna richiesta
789800051013     C                   IF        flgdis = 'D'
789900051013     C                   MOVEL     ' '           D48TLA
790000051013     C                   MOVEL     FILARR        D48FGS
790100051013     C                   MOVE      ARBAAS        D48AAS
790200051013     C                   MOVE      ARBLNP        D48LNP
790300051013     C                   MOVE      ARBNRS        D48NRS
790400051013     C                   MOVE      ARBNSP        D48NSP
790500051013     C                   MOVEL     ARBCBO        D48CBO
790600051109     c                   movel     arbGC1        §BGGC1
790700051109     c                   movel     arbGC2        §BGGC2
790800051109     c                   movel     arbTC1        §BGTC1
790900051109     c                   movel     arbTC2        §BGTC2
791000051013     C                   MOVEL     'CD'          D48CVB
791100051013     C                   MOVEL     'G'           D48TRC
791200051013     C                   MOVEL     'E'           D48FFR
791300051013     C                   MOVEL     ' '           D48ERR
791400051013     C                   MOVEL     *BLANK        D48MSG
791500051013     C                   MOVEL     'A'           D48TBO
791600051109      * se consegna particolare supermercato imposta causale CS
791700051109     c                   if        arbtc1 = 'S' or arbtc2 = 'S'
791800051109     c                   Clear                   Ds7l
791900051109     c                   Movel     '7L'          tblcod
792000051109     c                   Movel(p)  'CS'          tblKey
792100051109     c     kTab          Chain     Tabel00f                           96
792200051109     c                   If        Not *In96 and TblFlg <> ' '
792300051109     c                   Eval      *In96 = *On
792400051109     c                   EndIf
792500051109     c  n96              Movel     TblUni        Ds7l
792600051109     c                   If        Not *In96 and §7LSup <> 'N'
792700051109     c                   Movel     'CS'          D48Cvb
792800051109     c                   Movel     'V'           D48Ffr
792900051109     c                   Movel     'S'           D48F12
793000051109     c                   EndIf
793100051109     c                   endIf
793200051109      *fine supermercato
793300051013     c                   move      d915ndcr      dataiso
793400051013     c                   move      dataiso       §BGDCR
793500051014     c                   if        gnpdif = 915
793600051013     c                   clear                   §BGTCR
793700051013     c                   else
793800051013     c                   eval      §BGTCR='D'
793900051013     c                   end
794000051013     c*
794100051013     C                   MOVEL     DSLR48        KPJBU
794200051013     C                   CALL      'FNLR48R'
794300051013     C                   PARM                    KPJBA
794400051013     C                   PARM                    DSARBD
794500051013     C                   PARM                    DSARBK
794600051013     C                   PARM                    DARBT
794700051013     C                   PARM                    DSARBG
794800051013     C                   PARM                    trul90ds
794900051013     C                   MOVEL     KPJBU         DSLR48
795000051017     c                   end
795100051013     C                   ENDSR
795200050411     C*---------------------------------------------------------------*
795300050411     C* Scrivo figic00f per allineare saltuariamente la sede (TITAS+TNCSB)
795400050411     C*---------------------------------------------------------------*
795500050411     C     gicwtr        BEGSR
795600050411     C                   MOVEL     'CHGC'        GICCAV
795700050411     C                   Z-ADD     AMGUDT        GICDTV
795800050411     C                   TIME                    GICORV
795900050411     C                   Z-ADD     GCPLNPa       GICFEV
796000050411     C                   Z-ADD     GCPLNPa       GICLNP
796100050411     C                   Z-ADD     GCPLNAa       GICLNA
796200050411     C                   Z-ADD     GCPAASa       GICAAS
796300050411     C                   Z-ADD     GCPNRSa       GICNRS
796400050411     C                   Z-ADD     GCPNSPa       GICNSP
796500050411     C                   MOVEL     GCPAGC        GICDGC
796600050411     C                   MOVE      GCPMGC        GICDGC
796700050411     C                   Z-ADD     GCPFGC        GICFGC
796800050411     C                   Z-ADD     GCPNGC        GICNGC
796900050411     C                   Z-ADD     ARBDCM        GICDCH
797000050411     C                   WRITE     FIGIC000
797100050411     C                   ENDSR
797200950214     C*---------------------------------------------------------------*
797300950214     C*   R C H N C F - RICH. N.ACCR. SU FIGLIE PRECEDENTI            *
797400950214     C*---------------------------------------------------------------*
797500950214     C     RCHNCF        BEGSR
797600950214     C                   MOVE      LBLAAN        FAAS
797700950214     C                   MOVE      LBLLPN        FLNP
797800950214     C                   MOVE      LBLNRN        FNRS
797900950214     C                   MOVE      LBLNSN        FNSP
798000950414     C     KARBF         CHAIN     FNARB01L                           18
798100950330      * DEVE ESSERE BLOCCATA E NON DEVE ESSERE GIà STATA CHIESTA N.ACCR
798200950725B01  C     *IN18         IFEQ      *OFF
798300950330     C     ARBFBC        ANDNE     *BLANK
798400950330     C     ARBDCM        ANDEQ     *ZEROS
798500950330     C     ARBFIP        ANDNE     'C'
798600950330      * GUARDO SE è UN ASSEGNATO
798700950330     C                   MOVEL     '3A'          TBLCOD
798800950330     C                   MOVEL     *BLANK        TBLKEY
798900950330     C                   MOVEL     ARBCBO        TBLKEY
799000950330     C     KTAB          CHAIN     TABEL                              21
799100950330     C  N21              MOVEL     TBLUNI        DS3A
799200950330     C  N21              MOVEL     §3ADES        DTBL
799300950330     C  N21              MOVEL     §3ATB1        PORT              1
799400950725B02  C     PORT          IFEQ      'A'
799500950330     C*  Decodifico linea di arrivo
799600950330     C     ARBLNA        CHAIN     AZORG01L                           21
799700950330     C  N21              MOVEL     ORGDES        DLNA             15
799800950330     C   21              MOVEL     *ALL'?'       DLNA
799900950330      *----------------------------------------------------------*
800000000131     C                   CLEAR                   FVA
800100000131     C                   CLEAR                   FSV
800200950214     C                   CLEAR                   AR6NFT
800300991001     C                   MOVEL     '1'           KTRC
800400020618     C     KAR6F         CHAIN     FIAR601L                           18
800500950725B03  C     *IN18         IFEQ      *OFF
800600000131     C                   MOVEL     AR6SV1        FSV(1)
800700000131     C                   MOVE      AR6VA1        FVA(1)
800800000131     C                   MOVEL     AR6SV2        FSV(2)
800900000303     C                   MOVE      AR6VA2        FVA(2)
801000000131     C                   MOVEL     AR6SV3        FSV(3)
801100000131     C                   MOVE      AR6VA3        FVA(3)
801200000131     C     AR6SV3        IFNE      *BLANK
801300000131     C                   Z-ADD     3             X
801400040123     C     KAR6F         CHAIN     FIAR701L                           43
801500000131     C     *IN43         DOWEQ     *OFF
801600000131     C                   ADD       1             X
801700000131     C                   MOVEL     AR7SVN        FSV(X)
801800000131     C                   MOVE      AR7VAN        FVA(X)
801900040123     C     kar6f         READE     FIAR7000                               43
802000000131     C                   ENDDO
802100000131     C                   ENDIF
802200950330      *----------------------------------------------------------*
802300950330      * CONTROLLO SE BOLLA TASSATA
802400950330      *----------------------------------------------------------*
802500950725B04  C     AR6NFT        IFNE      *ZEROS
802600950214     C*
802700950214     C                   MOVEL     *ALL'?'       DLNP
802800950214     C     ARBLNP        CHAIN     AZORG01L                           21
802900950214     C  N21              MOVEL     ORGDES        DLNP             15
803000950214     C   21              MOVEL     *ALL'?'       DLNP
803100950214     C                   CLEAR                   DNWLNA
803200950214     C*
803300950214     C                   MOVE      AR6DFT        G08INV
803400950214     C                   MOVE      *ZEROS        G08DAT
803500950214     C                   MOVEL     '3'           G08ERR
803600950214     C                   CALL      'XSRDA8'
803700950214     C                   PARM                    WLBDA8
803800950214     C                   Z-ADD     G08DAT        GMADFT            8 0
803900950214     C* MANDARE IN SEDE SEMPRE
804000950214     C                   SETON                                        86
804100950214     C*   Stampa Nota Accredito
804200950214     C                   Z-ADD     ARBKSC        CLIENT            7 0
804300000131     C                   Z-ADD     0             TOTIMP
804400000131     C                   Z-ADD     0             TOTESE
804500000131     C                   Z-ADD     0             TOTNAC
804600991001     C                   Z-ADD     1             XS
804700000131     C     §$2IVA        LOOKUP    FSV(XS)                                20
804800991001     C     *IN20         IFEQ      '1'
804900020108     C     AR6IFT        SUB       AR6IMV        IMPALI
805000991001     C                   END
805100991027     C                   Z-ADD     AR6IMV        INAC
805200000131     C                   MOVE      INAC          WCOMO3            3 0
805300000131     C     WCOMO3        COMP      0                                      11
805400950214     C                   Z-ADD     AR6ALI        ALIQ              3 1
805500020621     C                   EVAL      CdIVA = %EDITC(Aliq:'2')
805600950214     C                   MOVEL     AR6CEI        TPALI             1
805700950214     C     TPALI         IFEQ      *BLANKS
805800000131     C                   Z-ADD     INAC          TOTIMP
805900950214     C                   ELSE
806000000131     C                   Z-ADD     INAC          TOTESE
806100950214     C                   Z-ADD     1             XX
806200950214     C     TPALI         LOOKUP    CEI(XX)                                41
806300950214     C     *IN41         IFEQ      '1'
806400020621     C                   MOVEL     ESE(XX)       CdIVA
806500950214     C                   END
806600950214     C                   MOVEL     'E'           TPALI
806700950214     C                   END
806800011130      * STAMPA DOPPIO IMPORTO SE DIVISA NON DI CONTABILITÀ
806900011130     C                   CLEAR                   WAR6IM
807000011130     C     AR6DIV        IFNE      §GEDMC
807100011130     C                   CLEAR                   YEURDS
807200011130     C                   MOVEL     AR6DIV        YECDVI
807300011130     C                   Z-ADD     INAC          YECIMI
807400011212     C                   MOVEL     §GEDMC        YECDVO
807500011130     C                   MOVEL     'H'           YECTAR
807600011130     C                   CALL      'YEURCO'
807700011130     C                   PARM                    YEURDS
807800011130     C                   Z-ADD     YECIMO        WAR6IM
807900011130     C                   ENDIF
808000011130      *
808100950214     C     TOTIMP        ADD       IMPALI        TOTNAC
808200950214     C                   ADD       TOTESE        TOTNAC
808300000131     C* VERIFICO SE IMPORTI CON DECIMALI
808400000131     C                   MOVE      TOTIMP        WCOMO3            3 0
808500000131     C     WCOMO3        COMP      0                                      08
808600000131     C                   MOVE      IMPALI        WCOMO3            3 0
808700000131     C     WCOMO3        COMP      0                                      09
808800000131     C                   MOVE      TOTESE        WCOMO3            3 0
808900000131     C     WCOMO3        COMP      0                                      10
809000000131     C                   MOVE      TOTNAC        WCOMO3            3 0
809100000131     C     WCOMO3        COMP      0                                      11
809200950214      * FORZO DATI DELLA FIGLIA
809300950214     C                   MOVEL     ARBAAS        G08INV
809400950214     C                   MOVE      ARBMGS        G08INV
809500950214     C                   MOVE      *ZEROS        G08DAT
809600950214     C                   MOVEL     '3'           G08ERR
809700950214     C                   CALL      'XSRDA8'
809800950214     C                   PARM                    WLBDA8
809900950214     C                   Z-ADD     G08DAT        VDSP
810000950214     C                   MOVE      ARBAAS        EXAAS
810100950214     C                   MOVE      ARBLNP        EXLNP
810200950214     C                   MOVE      ARBNRS        EXNRS
810300950214     C                   MOVE      ARBNSP        EXNSP
810400950214     C                   MOVE      ARBLNA        EXLNA
810500950330     C*  Aggiorno bolla x emissione Nota Accredito
810600950330     C                   MOVEL     'S'           ARBFNC
810700950330     C                   MOVEL     'C'           ARBFIP
810800040315      * scrittura file NCR
810900040315     c                   clear                   fnncr000
811000040315     c                   move      '201'         nctsocieta
811100040315     c                   z-add     arblna        nctfil
811200040315     c                   z-add     arblnp        nctpocompl
811300040315     c                   move      amgudt        nctddc
811400040315     c                   if        ar6dft > 0
811500040315     c                   move      ar6dft        nctdrp
811600040315     c                   end
811700040315     c                   z-add     ar6nft        nctnrp
811800040315     c                   z-add     ar6fiv        nctfpn
811900040315     c                   z-add     ar6ift        nctimr
812000040315     c                   move      *zeros        nctksc
812100040315     c                   move      ar6ksc        nctksc
812200040315     c                   move      *zeros        nctscf
812300040315     c                   move      ar6ksc        nctscf
812400040315     c                   move      knmus         nctutente
812500040315     c                   move      'I '          ncttnr
812600040315      * storno spese giacenza
812700040315     c                   move      'D'           ncrrna
812800040429     c                   move      '0'           ncrcmn
812900040315     c                   z-add     ar6imv        ncrim2
813000040315     c                   z-add     ar6ali        ncrali
813100040315     c                   z-add     arblnp        ncrflr
813200040315     c                   z-add     exaas         ncraassto
813300040315     c                   z-add     exlnp         ncrlnpsto
813400040315     c                   z-add     exnrs         ncrnrssto
813500040315     c                   z-add     exnsp         ncrnspsto
813600040429     c                   z-add     0             ncraasrec
813700040429     c                   z-add     0             ncrlnprec
813800040429     c                   z-add     0             ncrnrsrec
813900040429     c                   z-add     0             ncrnsprec
814000040429     c                   z-add     0             nchlnarec
814100040429      * completa viene forzata
814200040315     c                   move      '1'           nchricompl
814300040315
814400040315     c                   move      ar6cei        nchcei
814500040315     c                   z-add     wsvaas        nchaasfgl
814600040315     c                   z-add     wsvlnp        nchlnpfgl
814700040315     c                   z-add     wsvnrs        nchnrsfgl
814800040315     c                   z-add     wsvnsp        nchnspfgl
814900040315     c                   z-add     wsvlna        nchlnafgl
815000040315     c   90              z-add     gcpagc        nchagc
815100040315     c   90              z-add     gcpfgc        nchfgc
815200040315     c   90              z-add     gcpngc        nchngc
815300040315     c                   eval      nchorigine = 'STORNO SPESE DI GIACENZA '
815400040315     c                   write     fnncr000
815500950330     C*----------------------------------------------------------*
815600950330     C*  Segue Fattura: Stampo modulo x avvisare uff.contabilità
815700950330     C*                 della possibilità che venga richiesta
815800950330     C*                 Nota Accredito
815900950330     C*----------------------------------------------------------*
816000950725X04  C                   ELSE
816100950330     C*----------------------------------------------------------*
816200950330     C                   Z-ADD     GCPFGC        CFGC
816300950330     C                   Z-ADD     GCPNGC        CNGC
816400950330     C                   CLEAR                   WLBDA8
816500950330     C                   Z-ADD     GCPMGC        G08INV
816600950330     C                   MOVEL     GCPAGC        G08INV
816700950330     C                   MOVEL     '3'           G08ERR
816800950330     C                   CALL      'XSRDA8'
816900950330     C                   PARM                    WLBDA8
817000950330     C                   Z-ADD     G08DAT        CDGC
817100050222     C                   Z-ADD     GCPAASa       CAAS
817200050222     C                   Z-ADD     GCPLNPa       CLNP
817300050222     C                   Z-ADD     GCPNRSa       CNRS
817400050222     C                   Z-ADD     GCPNSPa       CNSP
817500950330     C                   Z-ADD     ARBAAS        CAAS1
817600950330     C                   Z-ADD     ARBLNP        CLNP1
817700950330     C                   Z-ADD     ARBNRS        CNRS1
817800950330     C                   Z-ADD     ARBNSP        CNSP1
817900950330     C                   Z-ADD     ARBMGS        G08INV
818000950330     C                   MOVEL     ARBAAS        G08INV
818100950330     C                   MOVEL     '3'           G08ERR
818200950330     C                   CALL      'XSRDA8'
818300950330     C                   PARM                    WLBDA8
818400950330     C                   Z-ADD     G08DAT        CDSP1             8 0
818500950330     C                   EXCEPT    AVVNAC
818600950330     C*  Calcolo Varie:
818700950330     C                   MOVEL     CVAR          DIMP             17
818800950330     C     AR6VA1        IFNE      0
818900950330     C                   MOVE      AR6SV1        DIMP
819000950330     C                   Z-ADD     AR6VA1        CIMP              9 0
819100950330     C                   EXCEPT    DETBOL
819200950330     C                   END
819300950330     C     AR6VA2        IFNE      0
819400950330     C                   MOVE      AR6SV2        DIMP
819500950330     C                   Z-ADD     AR6VA2        CIMP
819600950330     C                   EXCEPT    DETBOL
819700950330     C                   END
819800950330     C     AR6VA3        IFNE      0
819900950330     C                   MOVE      AR6SV3        DIMP
820000950330     C                   Z-ADD     AR6VA3        CIMP
820100950330     C                   EXCEPT    DETBOL
820200950330     C*  Se c'è la terza varia controllo se ce ne sono altre
820300991001     C     KARBF         CHAIN     FIAR701L                           18
820400950330     C     *IN18         DOWEQ     '0'
820500950330     C                   MOVE      AR7SVN        DIMP
820600950330     C                   Z-ADD     AR7VAN        CIMP
820700950330     C                   EXCEPT    DETBOL
820800991001     C     KARBF         READE     FIAR701L                               18
820900950330     C                   END
821000950330     C                   END
821100950330     C*  Trasporto
821200950330     C     AR6POR        IFNE      0
821300950330     C                   MOVEL     CTRA          DIMP
821400950330     C                   Z-ADD     AR6POR        CIMP
821500950330     C                   EXCEPT    DETBOL
821600950330     C                   END
821700950330     C*  Inoltro
821800991001     C                   Z-ADD     1             WW
821900991001     C     §$2FIN        LOOKUP    ASV(WW)                                20
822000991001     C     *IN20         IFEQ      '1'
822100950418     C                   MOVEL     CINL          DIMP
822200991001     C                   Z-ADD     AVA(WW)       CIMP
822300950330     C                   EXCEPT    DETBOL
822400950330     C                   END
822500950330     C*  Diritto fisso
822600991001     C                   Z-ADD     1             WW
822700991001     C     §$2VRH        LOOKUP    ASV(WW)                                20
822800991001     C     *IN20         IFEQ      '1'
822900950418     C                   MOVEL     CDIR          DIMP
823000991001     C                   Z-ADD     AVA(WW)       CIMP
823100950330     C                   EXCEPT    DETBOL
823200950330     C                   END
823300950330     C*  Tot.Imponibile
823400950330     C     AR6IMV        IFNE      0
823500950330     C                   Z-ADD     AR6IMV        CIMV
823600950330     C                   EXCEPT    TOTIMV
823700950330     C                   END
823800950330     C*  Imposto dati mittente
823900950330     C                   MOVEL     ARBRSD        CRAG
824000950330     C                   MOVEL     ARBIND        CIND
824100950330     C                   MOVEL     ARBCAD        CCAP
824200950330     C                   MOVEL     ARBLOD        CCIT
824300950330     C                   MOVEL     ARBPRD        CPRV
824400950330     C                   MOVEL     ARBNZD        CNAZ
824500950330     C                   EXCEPT    PIENAC
824600950725E04  C                   END
824700950330      *----------------------------------------------------------*
824800950725E03  C                   ENDIF
824900950725E02  C                   ENDIF
825000040312     C*
825100950330      *----------------------------------------------------------*
825200950214      *  DO PER CONSEGNATA LA BOLLA
825300950214     C                   MOVEL     'Y'           ARBICC
825400950725     C                   MOVEL     'S'           ARBACC
825500950214     C                   MOVEL     'Y'           ARBICA
825600950725     C                   MOVEL     'S'           ARBACA
825700950725     C                   MOVE      AMGUDT        ARBDCM
825800950214     C                   MOVEL     ORAMIN        ARBHMC
825900950214     C                   MOVE      *BLANK        ARBFT1
826000950214     C                   MOVE      *ZEROS        ARBDT1
826100091221     c                   clear                   ARBPDC
826200021009     C                   MOVEL     filarr        ARBPDC
826300091221     C                   MOVE      0998          ARBPDC
826400021009     C                   z-add     filarr        ARBifp
826500021009     C                   z-add     simpou        ARBipp
826600950214     C                   MOVE      *ALL'9'       ARBNDC
826700950214     C                   MOVE      AMGUDT        ARBDDC
826800950214     C                   MOVEL     'S'           ARBFDC
826900950214     C                   MOVE      *ALL'9'       ARBNMI
827000950214     C                   MOVE      AMGUDT        ARBDMI
827100950214     C                   MOVE      *ALL'9'       ARBCUC
827200950214     C                   MOVEL     *BLANK        ARBFBC
827300950214     C                   MOVEL     *BLANK        ARBCMC
827400950214      *
827500950214     C                   UPDATE    FNARB000
827600950214      *
827700950214     C                   ENDIF
827800950214     C*
827900950214     C                   SETOFF                                           86
828000950214     C*
828100950214     C                   ENDSR
828200900418      /EJECT
828300940309     C*----------------------------------------------------*
828400941219     C*    IMPOSTO CHIAVI PER TARIFFA                      *
828500940309     C*----------------------------------------------------*
828600941219     C     TARPCA        BEGSR
828700941219     C* §KSC VIENE IMPOSTATO (V.CHKARB) SE ASSEGNATO O SE C'E' 2°BOLLA
828800941219     C* KCTR    "      "         "            "              "
828900940309     C     §KSC          IFNE      *ZEROS
829000941006     C                   MOVE      §KSC          KKSC
829100940309     C                   ELSE
829200020225      *RICHIAMO PROGRAMMA PER REPERIMENTO INFORMAZIONI TARIFFA
829300020225     C                   CLEAR                   TR27DS
829400021118     C                   MOVE      ARBctr        I27ctb
829500020225     C                   MOVE      ARBTSP        I27TSP
829600020225     C                   MOVE      ARBLNA        I27LNA
829700020225     C                   MOVE      ARBLNP        I27LNP
829800020225     C                   MOVE      KKSC          I27CLI
829900160304     C**                 MOVEL     ARBAAS        I27DTA
830000160304     C**                 MOVE      ARBMGS        I27DTA
830100160304     C                   CALL      'TRUL27R1'
830200020225     C                   PARM                    TR27DS
830300160304     c
830400160304     c                   clear                   trulc7ds
830500160304     c                   eval      ic7tntw=o27ntw
830600160304     c
830700160304     C                   MOVEL     ARBAAS        Ic7DTA
830800160304     C                   MOVE      ARBMGS        Ic7DTA
830900160304     C                   move      arbctr        Ic7ctb
831000160304     c                   call      'TRULC7R'
831100160304     c                   parm                    trulc7ds
831200160304     C                   z-add     Oc7KSCc       kKSC
831300160304     C                   MOVEL     Oc7CTRc       KCTR
831400940309     C                   END
831500940309     C                   ENDSR
831600940309     C*----------------------------------------------------*
831700941219     C*    CALCOLO PROVVIGIONI C/ASSEGNO                   *
831800940309     C*----------------------------------------------------*
831900940309     C     CPROVV        BEGSR
832000980519     C*
832100990930     C                   CLEAR                   DTAS
832200990930     C                   CLEAR                   DTASV
832300980519     C                   MOVEL     KCTR          TASCTR
832400980519     C                   MOVE      KKSC          TASKSC
832500980519     C*
832600980519     C                   MOVEL     ARBCTS        TASCTS
832700980519     C                   MOVEL     ARBCAD        TASCAD
832800980519     C                   MOVEL     ARBFIN        TASFIN
832900980519     C                   MOVEL     ARBCAM        TASCAM
833000980519     C                   MOVEL     ARBFAP        TASFAP
833100980519     C                   MOVEL     ARBTSP        TASTSP
833200980519     C                   MOVEL     ARBTC1        TASTC1
833300980519     C                   MOVEL     ARBTC2        TASTC2
833400980519     C                   MOVEL     ARBFDN        TASFDN
833500980519     C                   MOVEL     ARBVAS        TASVAS
833600980519     C                   Z-ADD     ARBIAS        TASIAS
833700980519     C                   Z-ADD     ARBQFT        TASQFT
833800980519     C                   Z-ADD     ARBNCL        TASNCL
833900160530     C                   Z-ADD     ARBNCL        TASNCLb
834000980519     C                   Z-ADD     AMGDSP        TASDSP
834100980519     C                   Z-ADD     TASDSP        TASDCT
834200980519     C                   Z-ADD     ARBLNP        TASLNP
834300980519     C                   Z-ADD     ARBLNA        TASLNA
834400980519     C                   Z-ADD     ARBVLF        TASVLF
834500980519     C                   Z-ADD     ARBPKF        TASPKF
834600160530     C                   Z-ADD     ARBvlc        TASvlc
834700160530     C                   Z-ADD     ARBncr        TASncr
834800160530     C                   move      ARBfvf        TASfvf
834900160530     C                   Z-ADD     ARBpkb        TASPKb
835000980519     C* Reperisco codice tassazione
835100980519     C                   CLEAR                   TISI95
835200020225     C     TASLNP        CHAIN     AZORG01L                           21
835300020225     C     *IN21         IFEQ      '0'
835400020225     C                   MOVE      ORGDE3        OG143
835500020225     C     §OGNTW        IFEQ      'FED'
835600020225     C                   MOVE      'S'           I95FI1
835700020225     C                   END
835800020225     C                   END
835900980519     C                   MOVEL     '3'           I95TCN
836000980519     C                   MOVEL     ARBCAM        I95CAP
836100980519     C                   MOVEL     ARBNZM        I95NAR
836200980519     C                   MOVEL     ARBPRM        I95PRV
836300980519     C                   MOVEL     ARBLOM        I95LOC
836400980519     C                   MOVEL     ARBINM        I95IND
836500980519     C                   MOVEL     ARBTFP        I95TFP
836600980602     C                   MOVEL     ARBTSP        I95TSP
836700980602     C                   MOVEL     'S'           I95FRE
836800980519     C                   MOVEL     ARBNZM        I95NAR
836900980519     C                   MOVEL     AMGDSP        I95DAT
837000031021     C                   MOVEL     ARBtc1        I95fi2
837100031021     C                   MOVE      ARBtc2        I95fi2
837200980519     C                   CALL      'TISI95R'
837300980519     C                   PARM                    TISI95
837400980519     C                   MOVEL     O95CTS        TASMCT
837500980519     C*
837600040506     C                   MOVEL     ar9vca        TASVCA
837700980519     C                   Z-ADD     §ASS          TASCAS
837800980519     C                   MOVEL     AR9TIC        TASTIC
837900060927     C                   movel     ARBfbr        §asfbr
838000060927     C                   movel     ARBcca        §AScca
838100060927     C                   movel     dtas01        TASflo
838200980519     C*
838300980519     C                   MOVEL     'G'           TASSVA
838400021001     c* imposto peso vdl e colli rilevati
838500030206     c                   z-add     arbncp        Tasncp
838600030206     c                   z-add     arbpkc        Taspkc
838700030206      * Bancali
838800030206     c                   If        %Subst(Arbgva:1:1) = 'B'
838900030206     c                   Clear                   dAr5Ban
839000030805     C                   move      'BAN'         trcar5
839100051013     c     kFiar5        Chain(n)  Fiar501l
839200030206     c                   If        %Found(Fiar501l)
839300030206     c                   Movel     Ar5Uni        dAr5Ban
839400030206     c                   EndIf
839500030206     c                   Eval      TasBan = §Ar5Nba + §ar5Nb2
839600030206     c                   EndIf
839700030805      * Bancali Bartolini
839800030805     c                   If        %Subst(Arbgva:1:1) = 'O'
839900030805     c                   Clear                   dAr5BNB
840000030805     c                   Clear                   tasban
840100030805     C                   move      'BNB'         trcar5
840200051013     c     kFiar5        Chain(n)  Fiar501l
840300030805     c                   If        %Found(Fiar501l)
840400030805     c                   Movel     Ar5Uni        dAr5BNB
840500030805     c                   EndIf
840600030805     c                   Eval      TasNCL = §Ar5Bcor
840700030805     c                   EndIf
840800980519     C*
840900980519     C                   CALL      'TNSF20R'
841000980519     C                   PARM                    KPJBA
841100990930     C                   PARM                    DTAS
841200990930     C                   PARM                    DTASV
841300980519     C*
841400980519     C     TASERR        IFEQ      *BLANKS
841500991028     C                   MOVEL     TASDIV        V6CRVA
841600980519     C                   Z-ADD     1             §F
841700980519     C     TASSVA        LOOKUP    NSV(§F)                                21
841800980519     C   21              Z-ADD     NVA(§F)       §RCA
841900980519     C                   ELSE
842000980519     C                   SETON                                        20
842100980519     C                   END
842200980519     C     ENDCPR        ENDSR
842300951227     C*----------------------------------------------------*
842400951227     C*    CALCOLO PROVVIGIONI in partenza                 *
842500951227     C*----------------------------------------------------*
842600951227     C     PPROVV        BEGSR
842700951227     C                   MOVEL     *ALL'8'       KKSC
842800980519    1C     VNCA          IFNE      *ZEROS
842900951227     C                   Z-ADD     VNCA          §CAS
843000951227     C                   ELSE
843100951227     C                   Z-ADD     VASS          §CAS
843200980519    1C                   END
843300951227     C                   MOVEL     'V'           WCMB              1
843400951227     C                   EXSR      CASLIR
843500980519     C* CALCOLO PROVVIGIONI CON RICHIAMO DEL PGM TNSF20R
843600980519     C*
843700990930     C                   CLEAR                   DTAS
843800990930     C                   CLEAR                   DTASV
843900980519     C                   MOVEL     §LPCTR        TASCTR
844000040506     C                   MOVE      §KSC          TASKSC
844100980519     C*
844200980519     C                   MOVEL     §LPCTS        TASCTS
844300980519     C                   MOVEL     §LPCAD        TASCAD
844400980519     C                   MOVEL     §LPCAM        TASCAM
844500980519     C                   MOVEL     §LPTSP        TASTSP
844600980519     C                   MOVEL     ARBTC1        TASTC1
844700980519     C                   MOVEL     ARBTC2        TASTC2
844800980519     C                   MOVEL     ARBFDN        TASFDN
844900980519     C                   MOVEL     §LPVAS        TASVAS
845000980519     C                   Z-ADD     §LPIAS        TASIAS
845100980519     C                   Z-ADD     ARBQFT        TASQFT
845200980519     C                   Z-ADD     ARBNCL        TASNCL
845300160530     C                   Z-ADD     ARBNCL        TASNCLb
845400980519     C                   Z-ADD     AMGUDT        TASDSP
845500980519     C                   Z-ADD     TASDSP        TASDCT
845600980519     C                   Z-ADD     §LPLNP        TASLNP
845700980519     C                   Z-ADD     VLNA          TASLNA
845800980519     C                   Z-ADD     ARBVLF        TASVLF
845900980519     C                   Z-ADD     ARBPKF        TASPKF
846000160530     C                   Z-ADD     ARBvlc        TASvlc
846100160530     C                   Z-ADD     ARBncr        TASncr
846200160530     C                   move      ARBfvf        TASfvf
846300160530     C                   Z-ADD     ARBpkb        TASPKb
846400980519     C                   CLEAR                   DS3A
846500980519     C                   Z-ADD     1             §F                2 0
846600980519     C     §LPCBO        LOOKUP    CDB(§F)                                21
846700980519     C   21              MOVEL     D3A(§F)       DS3A
846800980519     C   21              MOVEL     §3ATB1        TASTBL
846900980519     C* Reperisco codice tassazione
847000980519     C                   CLEAR                   TISI95
847100020225     C     TASLNP        CHAIN     AZORG01L                           21
847200020225     C     *IN21         IFEQ      '0'
847300020225     C                   MOVE      ORGDE3        OG143
847400020225     C     §OGNTW        IFEQ      'FED'
847500020225     C                   MOVE      'S'           I95FI1
847600020225     C                   END
847700020225     C                   END
847800980519     C                   MOVEL     '3'           I95TCN
847900980519     C                   MOVEL     §LPCAM        I95CAP
848000980519     C                   MOVEL     §LPNZM        I95NAR
848100980519     C                   MOVEL     §LPPRM        I95PRV
848200980519     C                   MOVEL     §LPLOM        I95LOC
848300980519     C                   MOVEL     §LPINM        I95IND
848400980519     C                   MOVEL     ARBTFP        I95TFP
848500980602     C                   MOVEL     ARBTSP        I95TSP
848600980602     C                   MOVEL     'S'           I95FRE
848700980519     C                   MOVEL     §LPNZM        I95NAR
848800980519     C                   MOVEL     AMGUDT        I95DAT
848900031021     C                   MOVEL     ARBtc1        I95fi2
849000031021     C                   MOVE      ARBtc2        I95fi2
849100980519     C                   CALL      'TISI95R'
849200980519     C                   PARM                    TISI95
849300980519     C                   MOVEL     O95CTS        TASMCT
849400980519     C*
849500040506     C                   MOVEL     ar9vca        TASVCA
849600980519     C                   Z-ADD     §ASS          TASCAS
849700980519     C                   MOVEL     VTIC          TASTIC
849800980519     C*
849900980519     C                   MOVE      §LPBAM        TASDDT
850000980519     C                   MOVEL     'G'           TASSVA
850100021001     c* imposto peso vdl e colli rilevati
850200030206     c                   z-add     arbncp        Tasncp
850300030206     c                   z-add     arbpkc        Taspkc
850400060927     C                   movel     ARBfbr        §asfbr
850500060927     C                   movel     ARBcca        §AScca
850600060927     C                   movel     dtas01        TASflo
850700030206      * Bancali
850800030206     c                   If        %Subst(Arbgva:1:1) = 'B'
850900030206     c                   Clear                   dAr5Ban
851000030805     C                   move      'BAN'         trcar5
851100051013     c     kFiar5        Chain(n)  Fiar501l
851200030206     c                   If        %Found(Fiar501l)
851300030206     c                   Movel     Ar5Uni        dAr5Ban
851400030206     c                   EndIf
851500030206     c                   Eval      TasBan = §Ar5Nba + §ar5Nb2
851600030206     c                   EndIf
851700030805      * Bancali Bartolini
851800030805     c                   If        %Subst(Arbgva:1:1) = 'O'
851900030805     c                   Clear                   dAr5BNB
852000030805     c                   Clear                   tasban
852100030805     C                   move      'BNB'         trcar5
852200051013     c     kFiar5        Chain(n)  Fiar501l
852300030805     c                   If        %Found(Fiar501l)
852400030805     c                   Movel     Ar5Uni        dAr5BNB
852500030805     c                   EndIf
852600030805     c                   Eval      TasNCL = §Ar5Bcor
852700030805     c                   EndIf
852800980519     C*
852900980519     C                   CALL      'TNSF20R'
853000980519     C                   PARM                    KPJBA
853100990930     C                   PARM                    DTAS
853200990930     C                   PARM                    DTASV
853300991028     C*
853400991028     C     TASERR        IFEQ      *BLANKS
853500991028     C                   MOVEL     TASDIV        V6CRVA
853600991028     C                   END
853700991028     C*
853800991028     C                   ENDSR
853900940309     C/SPACE 5
854000940309     C*---------------------------------------------------------------*
854100941010     C* N E W C B L   CERCO NUOVO COICE BOLLA
854200940309     C*---------------------------------------------------------------*
854300940309     C     NEWCBL        BEGSR
854400940309     C                   Z-ADD     1             §F                2 0
854500940309     C     NWFGB         LOOKUP    FGB(§F)                                21
854600940309     C   21              MOVEL     CDB(§F)       NWCBL
854700940309     C* SE COME C/S PROPONE COD.=£ IMPONGO COD.BOLLA = A
854800940309     C* IN QUANTO £ è SOLO X LE COMUNIC. GIACENZE
854900941010     C   21NWCBL         IFEQ      '£ '
855000011130     C     NWCBL         OREQ      '$ '
855100941010     C                   MOVEL     'A '          NWCBL
855200940309     C                   END
855300950307     C* SE LA VECCHIA BOLLA ERA =£ ANZICHè C/S=A,  REIMPOSTO £ :
855400950307      * POICHè LE BOLLE £ NON VENGNO TRASMESSE IN SEDE QUINDI
855500950307     C* RICEVEREI LA FIGLIA SENZA AVERE LA MAMMA
855600950331     C                   CLEAR                   DS3A
855700950331     C                   Z-ADD     1             §F
855800950331     C     ARBCBO        LOOKUP    CDB(§F)                                04
855900950331     C   04              MOVEL     D3A(§F)       DS3A
856000950331     C     §3ABSD        IFEQ      'N'
856100950331     C     §3ACFD        ANDNE     *BLANKS
856200950331     C                   MOVEL     §3ACFD        NWCBL
856300950331     C                   ENDIF
856400950307     C*
856500940309     C  N21              SETON                                        30
856600940309     C                   ENDSR
856700940309     C/SPACE 5
856800940309     C*---------------------------------------------------------------*
856900940309     C* C N T V A R   CONTROLLO SE POSSO FARE VARIAZIONE BOLLA
857000940309     C*---------------------------------------------------------------*
857100940309     C     CNTVAR        BEGSR
857200940309     C                   SETOFF                                       2627
857300940309     C                   Z-ADD     1             §F
857400940309     C* CERCO TIPO VARIAZIONE
857500940309     C     §CDVAR        LOOKUP    §TV(§F)                                21
857600940309     C  N21              SETON                                        30
857700940309     C* GUARDO SE DEVO CONTROLLARE LA DATA CONTABILIZZAIONE
857800940309     C   21§DF(§F)       IFEQ      ' '
857900940928      * TASSAZ. 1°BOLLA
858000940928     C     AR6NFT        IFNE      *ZEROS
858100941013     C     AR6DFT        ANDLE     §DTCON
858200991001     C     AR6TRC        ANDEQ     '1'
858300940309     C                   SETON                                        26
858400940309     C                   END
858500940928      * TASSAZ. 2°BOLLA
858600991001     C     AR6NFT        IFNE      *ZEROS
858700991001     C     AR6DFT        ANDLE     §DTCON
858800991001     C     AR6TRC        ANDEQ     '2'
858900940309     C                   SETON                                        27
859000940309     C                   END
859100940309     C                   END
859200940309     C*  NON POSSO  VARIARE CODICE BOLLA :
859300940309     C     §CDVAR        IFEQ      'K8'
859400950725     C*  SE ESISTE FIGLIA NON POSSO VARIARE IL CODICE BOLLA
859500940309     C* SE TASSATO DESTINO SEMPRE NOTA ACCREDITO
859600940309     C     TASDES        IFEQ      '1'
859700940309     C                   SETON                                        27
859800940309     C                   END
859900940309     C                   END
860000940309     C                   ENDSR
860100940310     C*----------------------------------------------------*
860200940310     C*  X L P N O T  - IMPOSTO §LPNOT DATI EX SP.O GIAC.  *
860300940310     C*----------------------------------------------------*
860400940310     C     XLPNOT        BEGSR
860500051005     C                   clear                   §LPNot
860600051005     C                   clear                   §LPNT2
860700940310     C     $GIAC         IFEQ      '1'
860800940310     C                   MOVEL     'GIAC.'       TESTO1
860900940310     C                   MOVE      GCPAGC        GCAAGC
861000940310     C                   MOVE      GCPFGC        GCAFGC
861100940310     C                   MOVE      GCPNGC        GCANGC
861200940310     C                   MOVEL     'LNP. '       TESTO2
861300940310     C                   MOVE      ARBLNP        VARIO
861400940310     C                   MOVEL     RIFGIA        §LPNOT
861500051005      * reperisco note vecchia bolla da inserire dopo i riferimenti
861600051005      * solo in caso di dirottamento o cambio di porto
861700051005     c                   if        vfri = 'D' and wrecsp <> 'S'
861800051005     c                             or export <> vnpc and vnpc <> ' '
861900051006     c                             and wrecsp <> 'S'
862000051005     c                   movel(p)  rifgia        §comodo          70
862100051005     c                   exsr      oldnot
862200051005     c                   endif
862300940310     C                   ELSE
862400150108     C                   clear                   RIFSPE
862500150107      *imposto i riferimenti EX-SPED solo se non è dirottamento
862600150107     c                   if        vfri <> 'D'
862700940310     C                   MOVEL     'EX SPED.'    TESTO3
862800941217     C                   MOVE      KAAS          RAAS
862900941217     C                   MOVE      KLNP          RLNP
863000941217     C                   MOVE      KNRS          RNRS
863100941217     C                   MOVE      KNSP          RNSP
863200940310     C                   MOVEL     RIFSPE        §LPNOT
863300150107     c                   endif
863400051005      * reperisco note vecchia bolla da inserire dopo i riferimenti
863500051005      * solo in caso di dirottamento o cambio di porto
863600160908     c*********          if        vfri = 'D' and wrecsp <> 'S'
863700160908     c                   if        vfri = 'D'
863800051005     c                             or export <> vnpc and vnpc <> ' '
863900051006     c                             and wrecsp <> 'S'
864000051005     c                   movel(p)  rifspe        §comodo
864100051005     c                   exsr      oldnot
864200051005     c                   endif
864300961009     C* obbligo reso per particolarità giac.
864400961009     C   94              MOVEL     VNOT1         §LPNT2
864500961129     C*  Se devo effettuare un reso di una spedizione in Franco
864600961129     C*  Frontiera e su di essa non ho la particolarità che mi
864700961129     C*  obbliga ad effettuare il reso imposto nelle note cosegna
864800961129     C*  originale era un Franco frontiera
864900961129     C  N94VFRI          IFEQ      'R'
865000961129     C     V4CRCS        ANDNE     'S'
865100961129     C     ARBFBR        ANDEQ     'F'
865200961129     C                   MOVEL     CNOT3         §LPNT2
865300961129     C                   MOVE      CNOT4         §LPNT2
865400940310     C                   END
865500961129     C*
865600961129     C                   END
865700940310     C                   ENDSR
865800051005     C*-----------------------------------------------------
865900051005     C* O l d N o t - reperimento note da vecchia bolla
866000051005     C*-----------------------------------------------------
866100051005     C     oldnot        BEGSR
866200051005
866300051005     C                   MOVE      '8'           §TRC
866400060213     C     KAR4          CHAIN(N)  FiAR401L
866500060213     c                   if        %found(fiar401l)
866600150108     c                   if        §comodo <> *blank
866700051012     C                   eval      §comodo = %trim(§comodo)+ '*' +
866800051005     c                             %trim(ar4not)
866900150108     c                   else
867000150108     C                   eval      §comodo = %trim(ar4not)
867100150108     c                   endif
867200150108     c                   endif
867300051005     C                   MOVE      '9'           §TRC
867400060213     C     KAR4          CHAIN(N)  FiAR401L
867500060213     c                   if        %found(fiar401l)
867600150108     c                   if        §comodo <> *blank
867700051012     C                   eval      §comodo = %trim(§comodo) + '*' +
867800051005     c                             %trim(ar4not)
867900150108     c                   else
868000150108     C                   eval      §comodo = %trim(ar4not)
868100150108     c                   endif
868200051005     c                   end
868300051005     C                   movel     §comodo       §lpNot
868400051005     C                   move      §comodo       §lpNt2
868500051005
868600051005     C                   ENDSR
868700940930     C*-----------------------------------------------------
868800940930     C* C T R D I V - CONTROLLO DIVISA
868900940930     C*-----------------------------------------------------
869000940930     C     CTRDIV        BEGSR
869100941010     C                   CLEAR                   DSCV
869200940930     C                   Z-ADD     1             X
869300940930     C     WVCA          LOOKUP    DIV(X)                                 21
869400941006     C   21              MOVEL     DCV(X)        DSCV
869500940930     C  N21              SETON                                        1967
869600940930     C  N21              MOVEL     ERR(37)       $MSG
869700941217     C  N19§CVFDC        IFNE      'S'
869800940930     C                   MOVE      W0133         W0033             3 3
869900940930     C     W0033         IFNE      0
870000940930     C                   MOVEL     ERR(38)       $MSG
870100940930     C                   SETON                                        1976
870200940930     C                   ENDIF
870300940930     C                   ENDIF
870400940930     C*  C'ERA DIVISA E VARIO importo C/Assegno
870500940930     C*          --------> SE NON IMMESSA NUOVA DIVISA ERRORE
870600940930     C  N19VVCAP         IFNE      *BLANK
870700940930     C     VVCS          ANDEQ     'S'
870800940930     C     VNCA          ANDNE     *ZEROS
870900940930     C     VVCA          ANDEQ     *BLANK
871000940930     C                   MOVEL     ERR(39)       $MSG
871100940930     C                   SETON                                        1967
871200940930     C                   ENDIF
871300941010      * SE  CAMBIO DIVISA OBBLIGO DIGITARE ANCHE IMPORTO
871400941010     C     VNCA          IFEQ      *ZEROS
871500941010     C     VVCA          ANDNE     VVCAP
871600941010     C                   SETON                                        7619
871700941010     C                   MOVEL     ERR(41)       $MSG
871800941010     C                   END
871900940930     C                   ENDSR
872000941216     C*----------------------------------------------------*
872100941216     C*  C H K V A R
872200941216     C*  -----------
872300941216     C*  CERCO LA SIGLA DELLA VARIA LA ELABOARARE
872400941216     C*  MI RITORNA L'INDICE SU CUI LAVORARE
872500941216     C*----------------------------------------------------*
872600941216     C     CHKVAR        BEGSR
872700941216     C                   Z-ADD     1             XS                2 0
872800941216     C     §SIGLA        LOOKUP    WSV(XS)                                21
872900941216     C     *IN21         IFEQ      *OFF
873000941216     C                   Z-ADD     1             XS
873100941216     C     ' '           LOOKUP    WSV(XS)                                21
873200941216     C                   END
873300941216     C                   ENDSR
873400071126     C*-----------------------------------------------------
873500071126     C* CAPOFILA    - reperimento CAPOFILA £6
873600071126     C*-----------------------------------------------------
873700071126     C     Capofila      begsr
873800071127     c                   clear                   FNLV55ds
873900071126     c                   eval      D55tpt =  '6'
874000071126     c                   eval      D55lin =  vlna
874100071126     c                   eval      D55drf =  amgudt
874200071126     c                   call      'FNLV55R'
874300071126     c                   parm                    FNLV55ds
874400071126      * - Reperimento 146ª e 148ª descrizioni dell'organigramma
874500071126      *   (flag Tellus del capofila e flag gestione telefonate GEO)
874600071126if  2c                   if        D55tfa <> vlna and d55err = *blank
874700071127     c     D55tfa        chain     AZORG01l
874800071126if  3c                   if        %found(AZORG01L)
874900071128     c                   movel     ORGde6        Og146
875000071128     c                   movel     ORGde8        Og148
875100071126     c                   move      'X'           Og_da_£6
875200071126e   3c                   endif
875300071126e   2c                   endif
875400071126
875500071126     c                   endsr
875600990203     C*----------------------------------------------------------*
875700940301     C/EJECT
875800940301     C*---------------------------------------------------------------*
875900030715     C*   R O U T I N E   INIZIALE  x trul90ds                        *
876000940301     C*---------------------------------------------------------------*
876100030715     C     srtrul90      BEGSR
876200030715     c                   clear                   trul90ds
876300030715     c                   eval      d90mde = d82moe
876400030715     c                   eval      d90mdb = d82mob
876500030715     c                   eval      d90prb = d82prb
876600030715     c                   eval      d90pre = d82pre
876700030715     c                   eval      d90pss = d82pss
876800030715     c                   eval      d90mdb4 = d82mdb4
876900030715     c                   eval      d90mdb5 = d82mdb5
877000030715     c                   eval      d90psl = d82psl
877100030716     c                   eval      d90bsl = d82bsl
877200030715     c                   eval      d90prb4 = d82prb4
877300030715     c                   eval      d90prb5 = d82prb5
877400030715     c                   endsr
877500080123     C*---------------------------------------------------------------*
877600080123     C*   Controllo §LPCTRA ctr di tassazione porti assegnati in arrivo
877700080123     c*   per Resi: verifico se codice valido anche in arrivo
877800080123     C*---------------------------------------------------------------*
877900080123     C     contrCTRA     Begsr
878000080123     C                   CLEAR                   TR27DS
878100080123     C                   MOVE      §lppkb        I27pkg
878200080123     C                   MOVE      §lptsp        I27TSP
878300080123     C                   MOVE      §lpLNA        I27LNA
878400080123     C                   MOVE      §lplnp        I27LNP
878500080123     C                   MOVE      §lpksca       I27CLI
878600160303     C                   CALL      'TRUL27R1'
878700080123     C                   PARM                    TR27DS
878800080123     c
878900080123     c                   clear                   fnlv59DS
879000080123     C                   MOVEL     'A'           ilv59tbo
879100080123     C                   MOVEL     §lpksca       ilv59KSC
879200080123     C                   MOVEL     KCI           ilv59KCI
879300080123     C                   MOVEL     o27fie        ilv59FIE
879400080123     C                   MOVEL     amgudt        ilv59DSP
879500080123     C                   MOVEL     §lptsp        ilv59TSP
879600080123     C                   MOVEL     'S'           ilv59RIC
879700080123     C                   MOVE      §lpctra       ilv59CTR
879800080123     C*
879900080123     C                   CALL      'FNLV59R'
880000080123     C                   PARM                    fnlv59ds
880100080123     c*
880200080124     c* Se restituisce con errore o non preenti tariffe
880300080124     c*                           cerco una tariffa valida in arrivo
880400080124     c                   if        olv59err<>' '  or olv59tks=' '
880500080123     c                   clear                   ilv59CTR
880600080123     C                   CALL      'FNLV59R'
880700080123     C                   PARM                    fnlv59ds
880800080124     c                   select
880900080124     c* Se non trovata nessuna tariffa --> azzero il codice
881000080124     c                   when      olv59tks=' '
881100080124     c                   clear                   §lpctra
881200080124     c                   clear                   §lpksca
881300080124     c                   when      olv59ct3=' '
881400080123     c                   movel     olv59ctr      §lpctra
881500080124     c                   other
881600080124     c                   clear                   §lpctra
881700080124     c                   endsl
881800080124     c
881900080123     c                   endif
882000080123     c
882100080123     c                   endsr
882200030715     C*---------------------------------------------------------------*
882300030715     C*   R O U T I N E   INIZIALE                                    *
882400030715     C*---------------------------------------------------------------*
882500030715     C     DEFCAM        BEGSR
882600940927      *
882700940928     C     D82GIA        IFEQ      'S'
882800940928      * MI VIENE PASSATO IL NR. GIACENZA
882900940928     C                   MOVE      D82AGC        §AGC
883000940928     C                   MOVE      D82FGC        §FGC
883100940928     C                   MOVE      D82NGC        §NGC
883200940928      *
883300940301     C                   SETON                                        90
883400940301     C                   MOVEL     '1'           $GIAC             1
883500050222     C                   OPEN      tigcp02L
883600050222     C                   OPEN      tignp01L
883700990730     C                   OPEN      TITGC01L
883800940301     C                   ELSE
883900940301     C                   MOVEL     '0'           $GIAC
884000940301     C                   END
884100940301      *
884200940301     C* FINTO OPEN X RENDERE SHR YES  I FILE
884300941221     C                   OPEN      FNLV22P
884400940301     C   LR
884500940301     CANNLR              EXCEPT    FINTO
884600940301     C* MI SALVO KPJBU
884700940301     C                   MOVEL     KPJBU         §KPJBU          256
884800940301     C                   Z-ADD     1             CODUT
884900940301     C                   CALL      'X§PARUT'
885000940301     C                   PARM                    UTEDSE
885100940301     C     CODUT         CABEQ     -1            FINE
885200940301     C                   MOVEL     RAGUT         RSUT             20
885300941011     C                   MOVEL     '* FNLG20'    VPROGR
885400940301     C                   MOVE      'R *'         VPROGR
885500940301     C*
885600940301     C                   MOVEL     REC80         CNCR80
885700941013      * TRSFORMO DATA CONTABILIZZ. DA 6 A 8
885800941013     C                   MOVEL     DTU(10)       W20               2 0
885900941013     C     W20           IFGT      50
886000941013     C     1900          ADD       W20           W40               4 0
886100941013     C                   ELSE
886200941013     C     2000          ADD       W20           W40
886300941013     C                   END
886400941013     C                   MOVE      DTU(10)       §DTCON            8 0
886500941013     C                   MOVEL     W40           §DTCON
886600020730      *
886700020730      * se è il primo livello
886800021009     C                   SETOFF                                       2101
886900020730     c                   if        simtpp = '1'
887000020730     C                   SETON                                        2101
887100020730     c                   end
887200940928      *
887300021009     C                   MOVE      simpou        FILARR
887400941215     C* SE PASSATO IMPOSTO FILIALE IN GESTIONE
887500941216     C     D82FGS        IFGT      '000'
887600941215     C                   MOVEL     D82FGS        FILARR
887700941215     C                   ENDIF
887800940301     C                   SETOFF                                       02
887900940301     C* OVRPRTF  N. ACCREDITO
888000940301     C                   MOVEL     'ACCR.REC'    MODNAC            9
888100941013     C                   Z-ADD     80            LUNCMD           15 5
888200950123     C                   MOVEA     MODNAC        CMN(63)
888300940301     C                   CALL      'QCMDEXC'
888400940301     C                   PARM                    CMN
888500940301     C                   PARM                    LUNCMD
888600940301     C                   OPEN      LET132
888700940928      *
888800940624     C* AGGANCIO LA EI
888900940624     C                   MOVEL     'EI'          TBLCOD
889000020621     C                   CLEAR                   ESE
889100940624     C                   MOVEA     *BLANKS       CEI
889200940624     C     KTABP         CHAIN     TABEL00F                           21
889300040123     C                   Z-ADD     1             XX
889400940624     C     *IN21         DOWEQ     '0'
889500940627     C     XX            ANDLE     50
889600940624     C                   MOVEL     TBLUNI        DSEI
889700020621     C                   MOVE      §EIJEI        ESE(XX)
889800940624     C                   MOVEL     TBLKEY        CEI(XX)
889900940624     C                   ADD       1             XX
890000940624     C     KTABP         READE     TABEL00F                               21
890100940624     C                   END
890200940301     C*
890300991001     C* CARICO DATI DELLE VARIE CON FLG SOGGETTE O NO TOTALE IMP.
890400991001     C                   MOVEL     'CC'          TBLCOD
890500020806     C                   CLEAR                   IPB
890600020806     C                   MOVEA     *BLANKS       CCC
890700991001     C     KTABP         CHAIN     TABEL00F                           21
890800040123     C                   Z-ADD     1             XX                3 0
890900991001     C     *IN21         DOWEQ     '0'
891000991001     C     XX            ANDLE     100
891100991001     C* Controllo se sto esaminando un rcd di una varia
891200991001     C                   MOVEL     TBLKEY        WCOMO5            5
891300991001     C     WCOMO5        IFEQ      'VARIE'
891400991001     C                   MOVEL     TBLUNI        DSCC
891500991001     C                   MOVE      §CCFL6        IPB(XX)
891600991001     C                   MOVE      TBLKEY        CCC(XX)
891700991001     C                   ADD       1             XX
891800991001     C                   END
891900991001     C     KTABP         READE     TABEL00F                               21
892000991001     C                   END
892100940928     C* AGGANCIO LA £6 E RECUPERO LE LINEE A LORO ASSOCIATE
892200021009     c                   eval      kpjbus = kpjbu
892300020509     C                   CLEAR                   TR06DS
892400020509     C                   MOVEL     '£6'          D06COD
892500020509     C                   MOVEL     filarr        D06KEY
892600020509     C                   MOVEL     TR06DS        KPJBU
892700020509     C                   CALL      'TRUL06R'
892800020509     C                   PARM                    KPJBA
892900020509     C                   MOVEL     KPJBU         TR06DS
893000020509     C                   MOVEA     LIN           l£6
893100021009     c                   eval      kpjbu = kpjbus
893200940301     C*
893300940928     C* CERCO IL CODICE DELL'OPERATORE
893400021114     C                   CLEAR                   Tibs36Ds
893500021114     c                   EVAL      I36ute = knmus
893600021114     c                   EVAL      I36Tla = 'L'
893700021114     C                   CALL      'TIBS36R'
893800021114     C                   PARM                    Tibs36Ds
893900021009     C*                                                              i3
894000940301     C*--- RICERCA CAPOCONTI------------------------------------------*
894100940301     C                   DO        50            X                 2 0
894200940301     C                   MOVE      TCU(X)        TCUDS
894300940301     C     F56           CABNE     'CG'          END1
894400940301     C     F4            COMP      '1'                                    21
894500940301     C** 1 CLIENTI   21
894600940301     C     F3            COMP      '0'                                242425
894700940301     C** 0 ITALIA   25
894800940301     C** 1 ESTERO   24
894900940301     C   21
895000940301     CAN 24              Z-ADD     KCU(X)        KCE               4 0
895100940301     C   21
895200940301     CAN 25              Z-ADD     KCU(X)        KCI               4 0
895300940301     C     END1          TAG
895400940301     C                   END
895500940301     C*---------------------------------------------------------------*
895600940301     C*  KLIST
895700940301     C     KGIA          KLIST
895800940301     C                   KFLD                    §AGC
895900940301     C                   KFLD                    §FGC
896000940301     C                   KFLD                    §NGC
896100940301     C                   KFLD                    §FRG
896200940301     C                   MOVE      0             §FRG
896300940301     C*
896400940301     C     KNOT          KLIST
896500940301     C                   KFLD                    §AGC
896600940301     C                   KFLD                    §FGC
896700940301     C                   KFLD                    §NGC
896800940301     C                   KFLD                    §FRG
896900940301     C                   KFLD                    §FAS
897000051013     C*
897100051013     C     Kar5          KLIST
897200051013     C                   KFLD                    ARBAAS
897300051013     C                   KFLD                    ARBLNP
897400051013     C                   KFLD                    ARBNRS
897500051013     C                   KFLD                    ARBNSP
897600051013     C                   KFLD                    ar5trd
897700940301     C*
897800051013     C                   MOVE      0             §FRG
897900940301     C                   MOVE      020           §FAS              3 0
898000940301     C*
898100940301     C     KGIP          KLIST
898200940301     C                   KFLD                    §AGC
898300940301     C                   KFLD                    §FGC
898400940301     C                   KFLD                    §NGC
898500940301     C*
898600940301     C     KBOL          KLIST
898700941216     C                   KFLD                    KAAS
898800941216     C                   KFLD                    KLNP
898900941216     C                   KFLD                    KNRS
899000941216     C                   KFLD                    KNSP
899100991001     C     KAR6          KLIST
899200991001     C                   KFLD                    KAAS
899300991001     C                   KFLD                    KLNP
899400991001     C                   KFLD                    KNRS
899500991001     C                   KFLD                    KNSP
899600991028     C                   KFLD                    KTRC
899700991028     C     KBLP          KLIST
899800991028     C                   KFLD                    §LPAAS
899900991028     C                   KFLD                    §LPLNP
900000991028     C                   KFLD                    §LPNRS
900100991028     C                   KFLD                    §LPNSP
900200940301     C*
900300940301     C     KBOLG         KLIST
900400941216     C                   KFLD                    KAAS
900500941216     C                   KFLD                    KLNP
900600941216     C                   KFLD                    KNRS
900700941216     C                   KFLD                    KNSP
900800990628     C                   KFLD                    PROGR             2 0
900900941229     C*
901000941229     C     KBOLF         KLIST
901100941229     C                   KFLD                    ULFAAS
901200941229     C                   KFLD                    ULFLNP
901300941229     C                   KFLD                    ULFNRS
901400941229     C                   KFLD                    ULFNSP
901500991001     C     KAR6F         KLIST
901600000121     C                   KFLD                    FAAS
901700000121     C                   KFLD                    FLNP
901800000121     C                   KFLD                    FNRS
901900000121     C                   KFLD                    FNSP
902000991001     C                   KFLD                    KTRC
902100940301     C*
902200941003     C     KAR4          KLIST
902300941216     C                   KFLD                    KAAS
902400941216     C                   KFLD                    KLNP
902500941216     C                   KFLD                    KNRS
902600941216     C                   KFLD                    KNSP
902700940301     C                   KFLD                    §TRC
902800061026     C*
902900110802     C     Kar4dup       KLIST
903000110802     C                   KFLD                    arbAAS
903100110802     C                   KFLD                    arbLNP
903200110802     C                   KFLD                    arbNRS
903300110802     C                   KFLD                    arbNSP
903400110802     C*
903500070320     C     kar4l         klist
903600070320     C                   kfld                    arbAAS
903700070320     C                   kfld                    arbLNP
903800070320     C                   kfld                    arbNRS
903900070320     C                   kfld                    arbNSP
904000070320     C                   kfld                    §TRCl             1
904100070320     C                   move      'L'           §TRCl
904200070320     C*
904300061026     C     KAR4lbl       KLIST
904400061026     C                   KFLD                    LBLAAOsv
904500061026     C                   KFLD                    LBLLPOsv
904600061026     C                   KFLD                    LBLNROsv
904700061026     C                   KFLD                    LBLNSOsv
904800061026     C                   KFLD                    §TRC
904900030123     C*
905000030123     C     Kfiar5        KLIST
905100030123     C                   KFLD                    KAAS
905200030123     C                   KFLD                    KLNP
905300030123     C                   KFLD                    KNRS
905400030123     C                   KFLD                    KNSP
905500030123     C                   KFLD                    trcar5            3
905600950214     C*
905700950214     C     KARBF         KLIST
905800950214     C                   KFLD                    FAAS
905900950214     C                   KFLD                    FLNP
906000950214     C                   KFLD                    FNRS
906100950214     C                   KFLD                    FNSP
906200940301     C*
906300940301     C     KTAB          KLIST
906400940301     C                   KFLD                    CODUT
906500940301     C                   KFLD                    TBLCOD
906600940301     C                   KFLD                    TBLKEY
906700991028     C*
906800991028     C     KTBE          KLIST
906900991028     C                   KFLD                    TBECOD
907000991028     C                   KFLD                    TBEKE1
907100991028     C                   KFLD                    TBEKE2
907200940301     C*
907300940301     C     KTABP         KLIST
907400940301     C                   KFLD                    CODUT
907500940301     C                   KFLD                    TBLCOD
907600940301     C*
907700000925     C     KTAM          KLIST
907800150512     C                   KFLD                    ilv59ksc
907900000925     C                   KFLD                    CTRD51            3 0
908000150512     C                   KFLD                    olv59prg
908100940930     C*
908200940930     C     KTGC          KLIST
908300940930     C                   KFLD                    KKSC
908400940930     C                   KFLD                    KCTR
908500940930     C                   KFLD                    KPRG
908600940301     C*
908700940301     C     KANM          KLIST
908800940301     C                   KFLD                    ARTFLS
908900940301     C                   KFLD                    ARTLNA
909000940301     C                   KFLD                    ARTNRS
909100941006     C                   KFLD                    ARTNSC
909200951206     C*
909300951206     C     KEVB1         KLIST
909400951206     C                   KFLD                    KAAS1
909500951206     C                   KFLD                    KLNP1
909600951206     C                   KFLD                    KNRS1
909700951206     C                   KFLD                    KNSP1
909800951206     C                   KFLD                    KDEV
909900951206     C                   KFLD                    KHEV
910000951206     C*
910100951206     C     KEVB2         KLIST
910200951206     C                   KFLD                    KAAS1
910300951206     C                   KFLD                    KLNP1
910400951206     C                   KFLD                    KNRS1
910500951206     C                   KFLD                    KNSP1
910600950918     C*
910700950918     C     KANM1         KLIST
910800990518     C                   KFLD                    KAAS
910900950918     C                   KFLD                    KLNP
911000950918     C                   KFLD                    KNRS
911100950918     C                   KFLD                    KNSP
911200950918     C                   KFLD                    KCAA              3 0
911300010522      *
911400010522     C     KFNSPE        KLIST
911500010522     C                   KFLD                    WFLS              1
911600010524     C                   KFLD                    WCODIC            7 0
911700010522     C                   KFLD                    WLUO              3
911800010522     C                   MOVE      'L'           WFLS
911900010522     C                   MOVE      '300'         WLUO
912000041221     C     Ksp2          KLIST
912100041221     C                   KFLD                    WCODIC
912200041221     C                   KFLD                    WLUO
912300041221     C                   KFLD                    Wtip              2
912400070614     c     karn          klist
912500070614     c                   kfld                    arbaas
912600070614     c                   kfld                    arblnp
912700070614     c                   kfld                    arbnrs
912800070614     c                   kfld                    arbnsp
912900070614     c                   kfld                    cdnk              3
913000070614     c                   kfld                    gstk              1
913100110909     C*
913200110909     C     Kpct          KLIST
913300110909     C                   KFLD                    KAAS
913400110909     C                   KFLD                    KLNP
913500110909     C                   KFLD                    KNRS
913600110909     C                   KFLD                    KNSP
913700110909     C                   KFLD                    pcttrd
913800140916
913900140916     C     Ksgn5         KLIST
914000140916     C                   KFLD                    §LPAASm
914100140916     C                   KFLD                    §LPLNPm
914200140916     C                   KFLD                    §LPNRSm
914300140916     C                   KFLD                    §LPNSPm
914400000925      *
914500940301     C                   MOVEL     '0'           $FINE             1
914600940301     C                   MOVEL     '0'           $ERR              1
914700940303     C                   MOVEL     '0'           $F01              1
914800940303     C                   MOVEL     '0'           $F02              1
914900940303     C                   MOVEL     '0'           $F03              1
915000940304     C                   MOVEL     '0'           $F11              1
915100940304     C                   MOVEL     '0'           $OKF01            1
915200940304     C                   MOVEL     '0'           $OKF02            1
915300940304     C                   MOVEL     '0'           $OKF03            1
915400940301     C                   MOVEL     'V.BOLLA'     TESNWB
915500940301     C                   MOVEL     'L.A.'        TE2NWB
915600940308     C                   MOVEL     *ZEROS        §QUATR            4
915700960125     C     *LIKE         DEFINE    ARBRSD        WSVRSD
915800960125     C     *LIKE         DEFINE    AR4RD2        WSVRD2
915900960125     C     *LIKE         DEFINE    ARBIND        WSVIND
916000960125     C     *LIKE         DEFINE    ARBCAD        WSVCAD
916100960125     C     *LIKE         DEFINE    ARBPRD        WSVPRD
916200960125     C     *LIKE         DEFINE    ARBLOD        WSVLOD
916300960125     C     *LIKE         DEFINE    ARBNZD        WSVNZD
916400941216     C     *LIKE         DEFINE    GCPAAS        KAAS
916500941216     C     *LIKE         DEFINE    ARBMGS        EXMGS
916600941216     C     *LIKE         DEFINE    ARBLNA        EXLNA
916700950102     C     *LIKE         DEFINE    ARBAAS        EXAAS
916800950102     C     *LIKE         DEFINE    ARBLNP        EXLNP
916900950102     C     *LIKE         DEFINE    ARBNRS        EXNRS
917000950102     C     *LIKE         DEFINE    ARBNSP        EXNSP
917100951214     C     *LIKE         DEFINE    ARBAAS        WSVAAS
917200951214     C     *LIKE         DEFINE    ARBLNP        WSVLNP
917300951214     C     *LIKE         DEFINE    ARBNRS        WSVNRS
917400951214     C     *LIKE         DEFINE    ARBNSP        WSVNSP
917500951214     C     *LIKE         DEFINE    ARBLNA        WSVLNA
917600951228     C     *LIKE         DEFINE    LBLLPO        WSVLPO
917700941216     C     *LIKE         DEFINE    GCPLNP        KLNP
917800941216     C     *LIKE         DEFINE    GCPNRS        KNRS
917900941216     C     *LIKE         DEFINE    GCPNSP        KNSP
918000951206     C     *LIKE         DEFINE    EVBAAS        KAAS1
918100951206     C     *LIKE         DEFINE    EVBLNP        KLNP1
918200951206     C     *LIKE         DEFINE    EVBNRS        KNRS1
918300951206     C     *LIKE         DEFINE    EVBNSP        KNSP1
918400951206     C     *LIKE         DEFINE    EVBDEV        KDEV
918500951206     C     *LIKE         DEFINE    EVBHEV        KHEV
918600941011     C     *LIKE         DEFINE    AR4NOT        AR4RD2
918700940301     C     *LIKE         DEFINE    GCPAGC        §AGC
918800940301     C     *LIKE         DEFINE    GCPFGC        §FGC
918900940301     C     *LIKE         DEFINE    GCPNGC        §NGC
919000940301     C     *LIKE         DEFINE    GCPFRG        §FRG
919100940301     C     *LIKE         DEFINE    GCPTFT        §TFT
919200940308     C     *LIKE         DEFINE    ARBKSC        §KSC
919300941006     C     *LIKE         DEFINE    ARBKSC        KKSC
919400941006     C     *LIKE         DEFINE    ARBCTR        KCTR
919500980519     C     *LIKE         DEFINE    TGCPRG        KPRG
919600941010     C     *LIKE         DEFINE    GCPDDM        AMGDSP
919700940301     C     *LIKE         DEFINE    GCPDDM        AMGDDM
919800940301     C     *LIKE         DEFINE    GCPDDM        AMGDSD
919900940301     C     *LIKE         DEFINE    GCPDDM        AMGDSA
920000940301     C     *LIKE         DEFINE    GCPDDM        AMGDTG
920100941006     C     *LIKE         DEFINE    G08TGI        §GGDA
920200941006     C     *LIKE         DEFINE    G08TGI        §GGA
920300941006     C     *LIKE         DEFINE    G08TGI        §GG
920400990628     C     *LIKE         DEFINE    GCPSGR        §SGR
920500940301     C     *LIKE         DEFINE    GCPGGS        §GGPAG
920600940301     C     *LIKE         DEFINE    GCPGGS        §GGPAR
920700940301     C     *LIKE         DEFINE    GCPGGS        §GGSOS
920800940301     C     *LIKE         DEFINE    GCPGGS        §GGSAV
920900990628     C     *LIKE         DEFINE    GCPFRG        §NRG
921000940929     C     *LIKE         DEFINE    AR9CAS        §CAS
921100941006     C     *LIKE         DEFINE    AR9CMB        §CMB
921200941006     C                   MOVE      *ZEROS        §ASS             13 0
921300941214     C     *LIKE         DEFINE    TASVA1        §VARR
921400940301     C     *LIKE         DEFINE    VRCA          EXRCA
921500940301     C     *LIKE         DEFINE    VRCA          §RCA
921600950522     C     *LIKE         DEFINE    VRCA          WRCA
921700941006     C     *LIKE         DEFINE    AR6IFT        §INAC1
921800941006     C     *LIKE         DEFINE    AR6IFT        §INAC2
921900940301     C     *LIKE         DEFINE    TBLCOD        §COD
922000940301     C     *LIKE         DEFINE    TBLKEY        §KEY
922100940301     C                   MOVEL     *BLANKS       §DES             60
922200941006     C     *LIKE         DEFINE    ARBCBO        NWCBL
922300941215     C     *LIKE         DEFINE    D48CVB        §CDVAR
922400941215     C     *LIKE         DEFINE    D48CVB        §§KCVB
922500941215     C     *LIKE         DEFINE    D48CVB        §§TCVB
922600941006     C     *LIKE         DEFINE    AR4TRC        §TRC
922700991001     C     *LIKE         DEFINE    AR6TRC        KTRC
922800940301     C     *LIKE         DEFINE    ARBLNA        §LNA
922900941006     C     *LIKE         DEFINE    ARBCAD        COMCAP
923000940310     C     *LIKE         DEFINE    ARBCAD        SAVCAD
923100940930     C     *LIKE         DEFINE    ARBNZD        SAVNZD
923200940310     C     *LIKE         DEFINE    ARBIND        SAVIND
923300940310     C     *LIKE         DEFINE    ARBLNA        SAVLNA
923400940310     C     *LIKE         DEFINE    ARBLOD        SAVLOD
923500940310     C     *LIKE         DEFINE    ARBPRD        SAVPRD
923600940310     C     *LIKE         DEFINE    ARBRSD        SAVRSD
923700941011     C     *LIKE         DEFINE    ARBRSD        SAVRD2
923800941216     C     *LIKE         DEFINE    AR6VA1        §ANTEP
923900941229     C     *LIKE         DEFINE    ARBLNP        ULFLNP
924000941229     C     *LIKE         DEFINE    ARBAAS        ULFAAS
924100941229     C     *LIKE         DEFINE    ARBNRS        ULFNRS
924200941229     C     *LIKE         DEFINE    ARBNSP        ULFNSP
924300950214     C     *LIKE         DEFINE    ARBLNP        FLNP
924400950214     C     *LIKE         DEFINE    ARBAAS        FAAS
924500950214     C     *LIKE         DEFINE    ARBNRS        FNRS
924600950330     C     *LIKE         DEFINE    ARBNSP        FNSP
924700950330     C     *LIKE         DEFINE    GCPFGC        CFGC
924800950330     C     *LIKE         DEFINE    GCPNGC        CNGC
924900950330     C     *LIKE         DEFINE    GCPDUR        CDGC
925000950330     C     *LIKE         DEFINE    GCPAAS        CAAS
925100950330     C     *LIKE         DEFINE    GCPLNP        CLNP
925200950330     C     *LIKE         DEFINE    GCPNRS        CNRS
925300950330     C     *LIKE         DEFINE    GCPNSP        CNSP
925400950330     C     *LIKE         DEFINE    ARBAAS        CAAS1
925500950330     C     *LIKE         DEFINE    ARBLNP        CLNP1
925600950330     C     *LIKE         DEFINE    ARBNRS        CNRS1
925700950330     C     *LIKE         DEFINE    ARBNSP        CNSP1
925800950330     C     *LIKE         DEFINE    AR6IMV        CIMV
925900950330     C     *LIKE         DEFINE    AR6IFT        CIFT
926000950330     C     *LIKE         DEFINE    ARBRSD        CRAG
926100950330     C     *LIKE         DEFINE    ARBIND        CIND
926200950330     C     *LIKE         DEFINE    ARBCAD        CCAP
926300950330     C     *LIKE         DEFINE    ARBLOD        CCIT
926400950330     C     *LIKE         DEFINE    ARBPRD        CPRV
926500950330     C     *LIKE         DEFINE    ARBNZD        CNAZ
926600970710     C     *LIKE         DEFINE    O13ERR        WERR
926700970710     C     *LIKE         DEFINE    O13MSG        WMSG
926800970710     C     *LIKE         DEFINE    I95CAP        WSVCAP
926900970710     C     *LIKE         DEFINE    I95LOC        WSVLOC
927000970710     C     *LIKE         DEFINE    VPRD          WSVPRO
927100970710     C     *LIKE         DEFINE    E13FZV        WFZV
927200970710     C     *LIKE         DEFINE    E13FZ1        WFZ1
927300970710     C     *LIKE         DEFINE    E13FZ2        WFZ2
927400970710     C     *LIKE         DEFINE    E13FZ3        WFZ3
927500031119     C     *LIKE         DEFINE    vteld         SAVteld
927600031119     C     *LIKE         DEFINE    vref          SAVref
927700940928      * UDATE
927800940928     C                   TIME                    WHHUDT           14 0
927900940928     C                   MOVE      WHHUDT        G08DAT
928000940928     C                   MOVE      *ZEROS        G08INV
928100940928     C                   MOVEL     *BLANK        G08ERR
928200940928     C                   CALL      'XSRDA8'
928300940928     C                   PARM                    WLBDA8
928400940928     C                   Z-ADD     G08INV        AMGUDT            8 0
928500940928     C                   Z-ADD     G08DAT        GMAUDT            8 0
928600940928     C                   MOVEL     AMGUDT        ANNO              4 0
928700940930     C                   MOVEL     WHHUDT        ORAMIN            4 0
928800940928     C*                                                    *
928900000925     C* >>>>>>>>>>>>>>>
929000991028     C* REPERISCO LA DIVISA NELLA MONETA CORRENTE
929100991028     C                   MOVEL     'GED'         TBECOD
929200991028     C                   MOVEL     *BLANKS       TBEKE1
929300991028     C                   MOVEL     *BLANKS       TBEKE2
929400991028     C                   MOVEL     '1'           TBEKE1
929500991028     C     KTBE          CHAIN     TNTBE01L                           21
929600991028     C  N21              MOVEL     TBEUNI        DGED
929700940301     C* CARICO I TIPI BOLLA
929800940301     C                   MOVEL     '3A'          TBLCOD
929900940301     C                   MOVEL     *BLANK        TBLKEY
930000940301     C     KTAB          SETLL     TABEL
930100940301     C     *IN21         DOUEQ     '1'
930200940301     C     KTABP         READE     TABEL                                  21
930300940301     C  N21TBLFLG        IFEQ      *BLANK
930400940301     C                   MOVEL     TBLUNI        DS3A
930500020617     C     §3afgc        IFeq      *blank
930600940301     C                   MOVEL     §3ATB1        NWTB1
930700940301     C                   MOVEL     §3ATB2        NWTB2
930800940301     C                   MOVEL     §3AFCA        NWFCA
930900940301     C                   MOVEL     §3AFAN        NWFAN
931000940301     C                   MOVEL     §3AFTD        NWFTD
931100940301     C                   ADD       1             §F
931200940301     C                   MOVEL     NWFGB         FGB(§F)
931300940301     C                   MOVEL     TBLKEY        CDB(§F)
931400941221     C                   MOVEL     TBLUNI        D3A(§F)
931500020417     C                   END
931600940301     C                   END
931700940301     C                   END
931800951206     C* CARICO I TIPI BOLLA
931900081219     C                   Z-ADD     0             XY
932000951206     C                   MOVEL     '2A'          TBLCOD
932100951206     C                   MOVEL     *BLANK        TBLKEY
932200951206     C     KTAB          SETLL     TABEL
932300951206     C     *IN21         DOUEQ     '1'
932400951206     C     KTABP         READE     TABEL                                  21
932500951206     C  N21TBLFLG        IFEQ      *BLANK
932600951206     C                   ADD       1             XY
932700951206     C                   MOVEL     TBLKEY        C2A(XY)
932800951206     C                   MOVEL     TBLUNI        D2A(XY)
932900951206     C                   END
933000951206     C                   END
933100940301     C*
933200940301     C* CARICO I TIPI VARIAZIONE BOLLA
933300940301     C                   MOVE      *ZERO         §F
933400940301     C                   MOVEL     '7L'          TBLCOD
933500940301     C                   MOVEL     *BLANK        TBLKEY
933600940301     C     KTAB          SETLL     TABEL
933700940301     C     *IN21         DOUEQ     '1'
933800940301     C     KTABP         READE     TABEL                                  21
933900940301     C  N21TBLFLG        IFEQ      *BLANK
934000940301     C                   MOVEL     TBLUNI        DS7L
934100940301     C                   ADD       1             §F
934200940301     C                   MOVEL     TBLKEY        §TV(§F)
934300991029     C                   MOVEL     §7LVTA        §DF(§F)
934400940301     C                   END
934500940301     C                   END
934600940929     C* CARICO I CODICI DIVISA E RELATIVI CAMBI
934700940929     C                   MOVE      *ZERO         §F
934800940929     C                   MOVEL     'CV'          TBLCOD
934900940929     C                   MOVEL     *BLANK        TBLKEY
935000940929     C     KTAB          SETLL     TABEL
935100940929     C     *IN21         DOUEQ     '1'
935200940929     C     KTABP         READE     TABEL                                  21
935300940929     C  N21TBLFLG        IFEQ      *BLANK
935400940929     C                   ADD       1             §F
935500940929     C                   MOVEL     TBLKEY        DIV(§F)
935600941006     C                   MOVEL     TBLUNI        DCV(§F)
935700940929     C                   END
935800940929     C                   END
935900941216     C* CARICO SIGLE DELLE VARIE
936000941216     C                   MOVEL     '$2'          TBLCOD
936100941216     C                   MOVEL     *BLANK        TBLKEY
936200941216     C                   MOVEL     '1'           TBLKEY
936300941216     C     KTAB          CHAIN     TABEL                              21
936400941216     C  N21TBLFLG        COMP      *BLANK                             2121
936500941216     C  N21              MOVEL     TBLUNI        DS$2
936600940301     C*
936700040719     C* CARICO seconda chiave per limitazioni giacenze
936800040719     C                   MOVEL     '2G'          TBLCOD
936900040719     C                   MOVEL     *BLANK        TBLKEY
937000040719     C                   MOVEL     '2'           TBLKEY
937100040719     C     KTAB          CHAIN     TABEL                              21
937200040719     C  N21TBLFLG        COMP      *BLANK                             2121
937300040719     C  N21              MOVEL     TBLUNI        DS2g2
937400940301     C                   ENDSR
937500050316     C*----------------------------------------------------*
937600050316     C     geslck        begsr
937700050316     C*----------------------------------------------------*
937800050316     c                   Eval      ul82§rrn = gcpnrr
937900050316     c                   Eval      UL82§FIL = 'TIGCP00F'
938000050316     c                   Eval      UL82§WIN = 'S'
938100050316     c                   Eval      UL82§F7  = 'S'
938200050316     c                   Eval      UL82§num = 3
938300050316     c                   Eval      UL82§att = 3
938400050404     c                   Eval      UL82§mss = 'Si sta bloccando la giacenza Nr.'
938500050404     c                             + %editc(§agc:'Z') +  '  '+ %editc(§fgc:'Z')
938600050404     c                             + '  ' +  %editc(§ngc:'Z') + ' Terminare il -
938700050404     c                             lavoro'
938800050404     c                   Eval      UL82§msw = 'La giacenza Nr. '
938900050404     c                             + %editc(§agc:'Z') +  '  '+ %editc(§fgc:'Z')
939000050404     c                             + '  ' +  %editc(§ngc:'Z') +
939100050404     c                             ' non è manutenzionabile'
939200050404
939300050316      * Effettuo la chiamata al *PGM d utilità
939400050316     c                   Call(e)   'TRUL82R'
939500050316     c                   Parm                    Trul82ds
939600050316     c                   if        ul82§sts = 'A'
939700050316     c                   goto      fine
939800050316     c                   endif
939900050316
940000050316     C                   ENDSR
940100141210     C*----------------------------------------------------*
940200141210     C     geslckArb     begsr
940300141210     C*----------------------------------------------------*
940400141210     c                   Eval      ul82§rrn = arbnrr
940500141210     c                   Eval      UL82§FIL = 'FNARB00F'
940600141210     c                   Eval      UL82§WIN = 'S'
940700141210     c                   Eval      UL82§F7  = 'S'
940800141210     c                   Eval      UL82§num = 3
940900141210     c                   Eval      UL82§att = 3
941000141210     c                   Eval      UL82§mss = 'Si sta bloccando la Bolla Nr.'
941100141210     c                             + %editc(kaas:'Z') +  '  '+ %editc(klnp:'Z')
941200141210     c                             +   '  '+ %editc(knrs:'Z')
941300141210     c                             + '  ' +  %editc(knsp:'Z') + ' Terminare il -
941400141210     c                             lavoro'
941500141210     c                   Eval      UL82§msw = 'La Bolla Nr. '
941600141210     c                             + %editc(kaas:'Z') +  '  '+ %editc(klnp:'Z')
941700141210     c                             +   '  '+ %editc(knrs:'Z')
941800141210     c                             + '  ' +  %editc(knsp:'Z') +
941900141210     c                             ' non è manutenzionabile'
942000141210
942100141210      * Effettuo la chiamata al *PGM d utilità
942200141210     c                   Call(e)   'TRUL82R'
942300141210     c                   Parm                    Trul82ds
942400141210     c                   if        ul82§sts = 'A'
942500141210     c                   seton                                        43
942600141210     c                   endif
942700141210
942800141210     C                   ENDSR
942900130920     c*-------------------------------------------------------------
943000130920      *  Se è prevista una LNA da forzare da ORGANIGRAMMA
943100130920     c*-------------------------------------------------------------
943200130920     C     CHG_LNA_xFORZAbegSR
943300130920      *
943400130920      *  Se la LNA prevede su organigramma di essere sostituita
943500130920      *   x fare un RESO.
943600130920      *
943700130920     c     §LPLNA        chain     azorg01L
943800130920      *
943900130920     c                   IF        %Found(azorg01L)
944000130920     c                   eval      og143 = orgde3
944100130920     C*
944200130920     C*** Se si deve RENDERE utilizzando un'altra LINEA prevista in ORGANIGRAMMA
944300130920     C***    FORZATAMENTE su un'altra LNA (tipico CASO della DPD)
944400130920     C***  es.:   ciò che arriva da 193 o 192 deve tornare x 190
944500130920     C***       oppure ciò che arriva da 292  deve tornare x 197
944600130920      *
944700130920     c                   if        §ogLNARES <> *blank
944800130920     c                               and §ogLNARES <> '000'
944900130920      *
945000130920     c                   move      §ogLNARES     §LPLNA
945100130920     c                   end
945200130920      *
945300130920     C                   END
945400130920     C*
945500130920     C                   ENDSR
945600140916     c*-------------------------------------------------------------
945700140916      *  Annullamento chi sono bolla mamma su FISGN
945800140916     c*-------------------------------------------------------------
945900140916     C     sr_fisgn      begSR
946000140916
946100140916    1c                   if        §lpctm='2H' or §lpctm='2K'
946200140917     c     ksgn5         setll     fisgn05l
946300140917     c     ksgn5         reade     fisgn05l
946400140917     c                   dow       not %eof(fisgn05l)
946500140916    3c                   if        sgntnp=simfel
946600140916     c                   eval      sgnatb='A'
946700140916     c                   eval      sgndatora = %char(%timestamp:*iso0)
946800140916     C                   CLEAR                   SGNT6A
946900140916     C                   CLEAR                   SGNT6B
947000140916     C                   CLEAR                   SGNT6C
947100140916     C                   CLEAR                   SGNT6D
947200140916     C                   CLEAR                   SGNT6E
947300140916     C                   CLEAR                   SGNT6F
947400140916     c                   update    fisgn000
947500140916    3c                   endif
947600140917     c     ksgn5         reade     fisgn05l
947700140917    2c                   enddo
947800140917     c                   unlock    fisgn05l
947900140916    1c                   endif
948000140916
948100140916     c                   endsr
948200170228     c*-------------------------------------------------------------
948300170228      *  Determina lnp dpd se tfp<>tfa
948400170228     c*-------------------------------------------------------------
948500170228     c     sr_lnpdpd     begsr
948600170228     c                   clear                   wlnpb
948700170228     c                   clear                   fnlv55ds
948800170228     c                   eval      D55lin =  §lplnp
948900170228     c                   eval      D55drf =  amgudt
949000170228     c                   call      'FNLV55R'
949100170228     c                   parm                    FNLV55ds
949200170228    1c                   if        d55err=*blanks and
949300170228     c                             d55tfp<>d55tfa
949400170323     c                   eval      wlnpb = §lplnp
949500170323     c*    d55tfa        chain     azorg01l
949600170323    2c*                  if        %found(azorg01l)
949700170323     c*                  movel     orgde3        og143
949800170323    3c*                  if        §oglnpb>*zeros
949900170323     c*                  move      §OGLNPB       wlnpb
950000170323   x3c*                  else
950100170228     c* se su azorg la lnp valida come lnp bolla è vuota prendo da azcae la
950200170228     c* prima filiale dpd del terminal
950300170323     c*                  eval      kpjbus = kpjbu
950400170323     C*                  CLEAR                   TR06ds
950500170323     C*                  MOVE      'PP'          D06COD
950600170323     C*                  MOVEL     d55tfa        D06KEY
950700170323     c*                  move      'S'           d06esc
950800170323     C*                  MOVEL     tr06ds        KPJBU
950900170323     c*                  call      'TRUL06R'
951000170323     C*                  PARM                    KPJBA
951100170323     c*                  movel     kpjbu         tr06ds
951200170323     c*                  eval      kpjbu = kpjbus
951300170228     c* loop su schiera delle filiali
951400170323     c*                  z-add     1             idx
951500170323     c*                  clear                   wlnpb
951600170323    4c*                  dow       idx<=30  and lin(idx)>0 and wlnpb=0
951700170323     c*    lin(idx)      chain     azorg01l
951800170323    5c*                  if        %found(azorg01l)
951900170323     c*                  movel     orgde3        og143
952000170323    6c*                  if        §ogntw='DPD'
952100170323     c*                  move      lin(idx)      wlnpb
952200170323    6c*                  endif
952300170323    5c*                  endif
952400170323     c*                  add       1             idx
952500170323    4c*                  enddo
952600170323    3c*                  endif
952700170323    2c*                  endif
952800170228     c
952900170228    1c                   endif
953000170228     c                   endsr
953100991028     O*-------------------------------------------------------------
953200991028     OFNBLP000  E            AGGTRA
953300991028     O                       BLPDT1
953400991028     O                       BLPFT1
953500040428     O*RICH. RECUPERO
953600040429     Olet132    E            RECTES         1 01
953700040428     O                                         +  1 '* FNLG20R *'
953800040428     O                       GMAUDT            +  1 '  /  /    '
953900040428     O          E            RECTES         1 10
954000040428     O                                         + 30 'SPETT. P.OPERATIVO DI'
954100040428     O                       DLNP              +  1
954200040428     O          E            RECTES         2
954300040428     O                                         + 30 'ATT.NE UFF.COMMER'
954400040428     O                                         +  0 'CIALE'
954500040428     O          E            RECTES         3
954600040428     O                       DLNA              +  1
954700040428     O                       UDATE         Y   +  1
954800040428     O          E            DIRTES         3 18
954900040428     O                                         +  1 'OGGETTO:'
955000040428     O                                         +  1 '- RECUPERO SPESE DIROTTA'
955100040428     O                                         +  0 'MENTO - NR.'
955200040428     O                       filarr            +  1
955300040428     O                                         +  0 '/'
955400040428     O                       §RIREC        Z   +  0
955500040428     O          E            RICTES         3 18
955600040428     O                                         +  1 'OGGETTO:'
955700040428     O                                         +  1 '- RECUPERO SPESE TRASPOR'
955800040428     O                                         +  0 'TO - NR.'
955900040428     O                       filarr            +  1
956000040428     O                                         +  0 '/'
956100040428     O                       §RIREC        Z   +  0
956200040428     O                                         +  0 '-'
956300040428     O          E            RECDET         2
956400040428     O                                         +  1 'SPEDIZIONE'
956500040428     O                       EXAAS             +  1
956600040428     O                       EXLNP             +  1
956700040428     O                       EXNRS             +  1
956800040428     O                       EXNSP         Z   +  1
956900040428     O                       VDSP              +  1 '  /  /    '
957000040428     O                                         +  1 ' / '
957100040428     O                       ARBLNA            +  1
957200040428     O                       DTBL              +  2
957300040428     O          E            RECDET         2
957400040428     O                                         +  1 'FATTURA'
957500040428     O                       AR6FIV        Z   +  1
957600040428     O                       AR6NFT        Z   +  1
957700040428     O                       GMADFT            +  1 '  /  /    '
957800040428     O                                         +  6 'CLIENTE'
957900040428     O                       ARBKSC        Z   +  1
958000040428     O          E            RECDET         1
958100040428     O                                         +  1 'MITTENTE'
958200040428     O                                           49 'DESTINATARIO'
958300040428     O          E            RECDET         1
958400040428     O                       ARBRSM            +  1
958500040428     O                       ARBRSD            +  1
958600040428     O          E            RECDET         1
958700040428     O                       ARBINM            +  1
958800040428     O                       ARBIND            +  1
958900040428     O          E            RECDET         1
959000040428     O                       ARBLOM            +  1
959100040428     O                       ARBLOD            +  1
959200040428     O          E            RECDET         2
959300040428     O                       ARBCAM            +  1
959400040428     O                       ARBPRM            +  1
959500040428     O                       ARBNZM            +  1
959600040428     O                       ARBCAD              46
959700040428     O                       ARBPRD            +  1
959800040428     O                       ARBNZD            +  1
959900040428     O          E            RECDET         2
960000040428     O                                         +  1 'RIF. MITT.'
960100040428     O                       ARBRMN        Z   +  1
960200040428     O                                         +  1 'COLLI'
960300040428     O                       ARBNCL        2   +  1
960400040428     O                                         +  1 'PESO'
960500040428     O                       ARBPKF        2   +  1
960600040428     O                                         +  1 'VOLUME'
960700040428     O                       ARBVLF        2   +  1
960800040428     O          E            DIRDET         3
960900040428     O                                         +  1 'LA MERCE E'''
961000040428     O                                         +  1 'STATA DIROTTATA A '
961100040428     O                       DNWLNA            +  1
961200040428     O          E            RICDET         3
961300040428     O                                         +  1 'LA MERCE E'''
961400040428     O                                         +  1 'STATA CONSEGNATA IN FRAN'
961500040428     O                                         +  0 'CO A:'
961600040428     O                       DNWLNA            +  1
961700040428     O          E            RECPRO         3
961800040428     O                                         +  1 'INOLTRE E'''
961900040428     O                                         +  1 'STATO AGGIUNTO IL C/ASS'
962000040428     O                                         +  0 'EGNO CON PROVV.AL MITTE'
962100040428     O                                         +  0 'NTE'
962200040428     O          E            RECFIN         3
962300040428     O                     31                  +  1 'CON SPEDIZIONE:'
962400040428     O                     31§LPLNP            +  7
962500040428     O                     31§LPAAS            +  1
962600040428     O                     31§LPNRS            +  1
962700040428     O                     31§LPNSP        Z   +  1
962800040428     O          E            RECFIN         1
962900040428     O                                         +  1 'NOTE:'
963000040428     O                       VNOT1             +  1
963100040428     O          E            RECFIN         2
963200040428     O                       VNOT2             +  7
963300040428     O          E            RECFIN         3
963400040428     O                                         +  1 'SI PREGA IL P.OPERATIVO'
963500040428     O                                         +  1 'DI'
963600040428     O                       DLNP              +  1
963700040428     O                                         +  1 'DI FARE BOLLA DI RECUP'
963800040428     O                                         +  0 'ERO.'
963900040428     O          E            RECFIN      2  1
964000040428     O                                         + 30 'DISTINTI SALUTI'
964100040428     O          E            RECFIN         3
964200040428     O                       DLNA              + 30
964300040428     O          E   N32N37N69RECFIN      2  1
964400040428     O                                         +  1 '- LA SPEDIZIONE IN'
964500040428     O                                         +  1 'OGGETTO E'''
964600040428     O                                         +  1 'STATA STORNATA.'
964700040428     O          E    32      RECFIN      2  1
964800040428     O         OR    37
964900040428     O                                         +  1 '- PER LA SPEDIZIONE IN'
965000040428     O                                         +  1 'OGGETTO E'''
965100040428     O                                         +  1 'STATA RICHIESTA NOTA'
965200040428     O                                         +  1 'ACCREDITO.'
965300040428     O          E    32      RECFIN      2  1
965400040428     O         OR    37
965500040428     O                                         +  1 '- V.NR. RICHIESTA'
965600040428     O                       ar6fiv            +  1
965700040428     O                                         +  0 '/'
965800950329     O*------------------------------------------------*
965900970410     O*  Avviso possibile richiesta nota accredito
966000950329     O*------------------------------------------------*
966100950329     O          E            AVVNAC         1 01
966200950329     O                                         +  1 '* FNLG20R *'
966300950329     O                       GMAUDT            +  1 '  :  :    '
966400950329     O          E            AVVNAC      2  1
966500950329     O                                           43 'ATT.NE UFFICIO'
966600950418     O                                           56 'CONTABILITA'''
966700950329     O          E            AVVNAC         3
966800961002     O                                           50 'P.OPERATIVO DI'
966900950329     O                       DLNA              +  1
967000950329     O          E            AVVNAC      1  2
967100950329     O                                           24 'OGGETTO: Bolle di recupe'
967200950329     O                                           48 'ro Spese Giacenza segue '
967300950329     O                                           55 'fattura'
967400950329     O          E            AVVNAC         1
967500950329     O                                           31 'Nell''esecuzione disposi'
967600950329     O                                           51 'zioni della giacenza'
967700950329     O                       CFGC          Z     55
967800950329     O                                           56 '/'
967900950329     O                       CNGC          Z     63
968000950329     O                                           67 'del'
968100950329     O          E            AVVNAC         1
968200950329     O                       CDGC          8     10
968300950329     O                                           31 'aperta su spedizione'
968400950329     O                       CAAS          Z     36
968500950329     O                                           37 '/'
968600950329     O                       CLNP          Z     40
968700950329     O                                           41 '/'
968800950329     O                       CNRS          Z     43
968900950329     O                                           44 '-'
969000950329     O                       CNSP          Z     51
969100950329     O                                           67 'è stata creata'
969200950329     O          E            AVVNAC         2
969300950329     O                                           24 'la seguente bolla di rec'
969400950329     O                                           30 'upero:'
969500950329     O          E            AVVNAC         0
969600950329     O                       CLNP1         Z      6
969700950329     O                                            7 '/'
969800950329     O                       CNRS1         Z      9
969900950329     O                                           10 '-'
970000950329     O                       CNSP1         Z     17
970100950329     O                                           21 'del'
970200950330     O                       CDSP1         8     32
970300950330     O          E            DETBOL         1
970400950330     O                       DIMP                51
970500950329     O                                           56 '= £.'
970600950329     O                       CIMP          2     67
970700950330     O          E            TOTIMV         1
970800950330     O                                           56 '= £.'
970900950330     O                                           67 '-------------'
971000950330     O          E            TOTIMV         1
971100950330     O                                           51 'Totale Imponibile'
971200950330     O                                           56 '= £.'
971300950330     O                       CIMV          2     67
971400950330     O          E            TOTIMV         1
971500950330     O                                           67 '============='
971600950329     O          E            PIENAC      1  1
971700950329     O                                           24 'tassata come segue fattu'
971800950329     O                                           37 'ra al cliente'
971900950329     O          E            PIENAC         1
972000950329     O                       CRAG                40
972100950329     O          E            PIENAC         1
972200950329     O                       CIND                40
972300950329     O          E            PIENAC         2
972400950418     O                       CCAP                14
972500950418     O                       CCIT                50
972600950418     O                       CPRV                53
972700950418     O                       CNAZ                57
972800950329     O          E            PIENAC         1
972900950329     O                                           33 'Poichè la giacenza è sta'
973000950418     O                                           57 'ta riaperta e quindi le '
973100950329     O                                           67 'spese sono'
973200950329     O          E            PIENAC         1
973300950329     O                                           24 'state recuperate con alt'
973400950329     O                                           48 're spedizioni, è probabi'
973500950329     O                                           67 'le che il  suddetto'
973600950329     O          E            PIENAC         3
973700950329     O                                           24 'cliente richieda Nota di'
973800950329     O                                           35 ' Accredito.'
973900950329     O          E            PIENAC         1
974000950418     O                                           33 'In questo caso allegare '
974100950418     O                                           57 'la presente al modulo di'
974200950418     O                                           67 ' richiesta'
974300950329     O          E            PIENAC         3
974400950329     O                                           24 'Nota Accredito che inolt'
974500950418     O                                           39 'rerete in sede.'
974600950329     O          E            PIENAC      1
974700950329     O                                           54 'Ufficio Giacenze'
974800950329     OFNLV22P   E            FINTO
974900000417** CMB
975000991206OVRPRTF FILE(FNLSB0P   ) OUTQ(          ) SAVE(*YES) FORMTYPE('          ')
975100991206** CMP
975200991206OVRPRTF FILE(FNLSB0P1  ) OUTQ(          ) SAVE(*YES) FORMTYPE('BOLLE-dpd ')
975300030304** CMPf
975400030304OVRPRTF FILE(FNLSB0P2  ) OUTQ(          ) SAVE(*YES) FORMTYPE('BOLLE-Fed ')
975500920206** CME
975600141210OVRPRTF FILE(FNLV22P   ) OUTQ(          )            FORMTYPE('          ')
975700920206** CMN
975800941013OVRPRTF FILE(LET132    ) OUTQ(*JOB      ) SAVE(*YES) FORMTYPE(          )
975900030715** CM4
976000141210OVRPRTF FILE(FNLSB5PA4 ) OUTQ(          )            FORMTYPE('          ')
976100030715** CM5
976200141210OVRPRTF FILE(FNLSB5PA5 ) OUTQ(          )            FORMTYPE('          ')
976300920206** DOV
976400920206DLTOVR  FILE(*ALL)
976500930427** ERR
976600970710Bolla Inesistente o già Consegnata                                             1
976700970710Bolla Legata: non eseguibile Cambio porto/Dirottam......                       2
976800970710Bolla inserita in Distinta di Consegna                                         3
976900970710Bolla GIACENTE: Agire da "GESTIONE GIACENZE"                                   4
977000970710Linea Arrivo Bolla Non in Gestione !!                                          5
977100970710Bolla Consegnata o Cons.Parzialm.: non possibile Cambio porto/Dirott.          6
977200970710Bolla Bloccata                                                                07
977300970710Tipo Consegna Errato !!                                                       08
977400970710Indicare Nuovo Porto !!                                                       09
977500970710Indicare Nuovo Porto SOLO SE DIVERSO !!                                       10
977600970710Se si fa RESO il C/Assegno deve essere ANNULLATO !!                           11
977700970710Variaz. C/A (S/N) non congruente con Importo o Divisa                         12
977800970710Indicare "S" per il calcolo Provvigioni C/Assegno                             13
977900970710A CHI addebitare provv. C/Assegno Errato !!                                   14
978000970710Variaz. Anticipata non congruente con Importo                                 15
978100970710Annullo C/Assegno : premere F8  per FORZARE ANNULLAMENTO !!                   16
978200970710Codice Fine Giacenza ERRATO !!                                                17
978300970710Si può richiamare Tassazione solo se linea arrivo é uguale !!                 18
978400970710Immettere dati anagrafici !!                                                  19
978500970710Provincia Incongruente con CAP  !! LIBERO LO MANDA PGM CTR CAP                20
978600970710Linea Arrivo errata !!                                                        21
978700970710Linea Arrivo incogruente con CAP : premere F9 x FORZARE !!!                   22
978800970710CAP ERRATO  !!                                                                23
978900970710Non Trovata Tariffa x Calcolo Provv.C/Assegno                                 24
979000970710Non Eseguibile variazione tipo bolla     LIBERO                               25
979100970710Non Eseguibile variazione Destinatario   LIBERO                               26
979200970710Non Eseguibile variazione Tassazione 1° bolla  LIBERO                         27
979300970710Non Eseguibile variazione Tassazione 2° bolla  LIBERO                         28
979400970710L. Arrivo diversa premere F8  x forzare dispos. in DIROTTAMENTO !!            29
979500970710Data errata, o < Apert. giacenza, o > Data del giorno                         30
979600970710Date Incongruenti                                                             31
979700970710Nessuna spesa da addebitare                                                   32
979800970710Immettere Importo Provv. C/Assegno                                            33
979900970710Non Trovata Tariffa giacenza !!                                               34
980000970710Non creata Bolla: premuto CMD12 da immiss.bolle! RIPETERE OPERAZ.             35
980100970710Non si possono eseguire le disposizioni prima della data indicata !           36
980200970710Divisa errata !                                                               37
980300970710La Divisa non prevede l'uso di decimali nell'importo C/assegno                38
980400970710Se si varia C/assegno immettere anche la Divisa !                             39
980500970710Nazione errata !                                                              40
980600970710Se si varia Divisa digitare anche l'importo del C/Assegno                     41
980700970710L.Arrivo uguale premere F8 per forzare disposizioni in "CONSEGNA"             42
980800970710Tipo incasso C/assegno errato o non congruente con valuta                     43
980900970710Non si puo' gestire Anticipata su una bolla in Franco                         44
981000970710Bolla C/S per comunicaz. giac. e possibile fare solo DIROTTAMENTO             45
981100970710Non è possibile eseguire cambio di porto su bolla di RIENTRO                  46
981200970710Cambio di porto non ammesso se immesse disposizioni di reso                   47
981300970710Impossibile aggiungere il C/Assegno se immesse disposizioni di reso           48
981400970710Impossibile eseguire reso in conto servizio su bolla di rientro               49
981500970710Inserire il motivo per cui si rende la merce !!!                              50
981600970710Impossibile eseguire dirottamento per bolle export                            51
981700970710Attenzione !! Modificata Località                                             52
981800970710Attenzione !! Modificato CAP                                                  53
981900970710Attenzione !! Modificata Provincia                                            54
982000970710Attenzione !! Modificata Nazione                                              55
982100971006L.Arrivo diversa impossibile effettuare DIROTTAMENTO: mittente non codificato 56
982200971113L.Arrivo non gestita dall'Azienda !!!                                         57
982300990203Impossibile eseguire cambio porto/dirottamento: bolla con C.A. Aperta         58
982400991028La Divisa non prevede l'uso di decimali nEll'importo                          59
982500991028Codice divisa inesistente                                                     60
982600991028Divisa non congruente con le altre della tassazione                           61
982700000120Bolla di recupero: impossibile effettuare cambi di porto/resi/dirottamenti    62
982800040206Impossibile eseguire cambio porto/dirottamento:PER BOLLE POSTE                63
982900040206La divisa deve essere inserita se immesso un importo nel C/A                  64
983000040531Cambio linea arrivo non ammesso la LOC.o il CAP sono serviti dalla attuale    65
983100071130ATTENZIONE verificare la data consegna richiesta della bolla !!!!!            66
983200110909Spedizione passata al PDA. Ricevere l'annullamento o chiudere la distinta !!! 67
983300151204ATTENZIONE presenti disposizioni Dirottamento/Reso spedizione non gestibile   68
983400141105ATTENZIONE esistono delle disposizioni di consegna che devono essere risolte  69
