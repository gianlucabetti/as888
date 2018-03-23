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
003000160805     H*  39 DO' PER INCASSATO L'ASSEGNATO                            *
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
006000940309      *--------------------------------------------------------------*
006100940309      *  MAIN PROGRAM
006200940309      *--------------------------------------------------------------*
006300940309     H*       ----> 1)  -SRGIAC =SE RICH. DA GIAC.
006400940309     H*                O-SRDIRO =SE RICH. DA BOLLE
006500940309     H*                         ---> 1) -CHKARB = LEGGO FLARB
006700940309     H*  SRF01----> 1)  -ARBVID =VIDEO X IMMISS. DISPOS. SE  DA BOLLE
006800940309     H*                O-GCAVID =VIDEO X VISUAL.DISPOS.  SE  DA GIACE.
006900940309     H*                  GCANOT           "
007000940309     H*             2)  -AZZER  =AZZERAM. INDIC. + CAMPI...
007100940309     H*             3)  -RIENTR =SE DISPOS. RIENTRO  OPERAZ. DA FARE
007200940310     H*                O-DIROT1 =SE DISPOS. DIROTTAMENTO      "
007300940310     H*                O-RICONS =SE DISPOS. RICONSEGNARE  1° operaz.
007400941010     H*                         ---> O -RICNOC = RIC.NO CAMBIO PORTO
007500941010     H*                              O -RICSIC = RIC.SI CAMBIO PORTO
007700940309     H*  SRF02----> 1)  -CTRV02 =VIDEO X CONTR. DESTIN./LNA/F. GIAC...
007800940310     H*                 -DIROT2 =SE DISPOS. DIROTTAMENTO  2° operaz.
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
016000050701     d fnlg09ds      e ds
016100050316     d gcpinf          ds
016200050316     d  gcpnrr               397    400i 0
016300141210     d arbinf          ds
016400141210     d  arbnrr               397    400i 0
016500050316     D*--------------------------------------------------------------*
016600900424     D TGS             S              3  0 DIM(3)
016700990730     D TTS             S             11  3 DIM(3)
016800900424     D SGG             S              3  0 DIM(3)
016900990628     D STU             S             11  3 DIM(3)
017000990628     D STT             S             11  3 DIM(3)
017100911213     D CFG§            S              1    DIM(3)
017200941012     D VCA§            S              1    DIM(3)
017300920211     D FGB             S              7    DIM(40)
017400940928     D CDB             S              2    DIM(40)
017500941221     D D3A             S             89    DIM(40)
017600920130     D §TV             S              2    DIM(50)
017700920130     D §DF             S              1    DIM(50)
017800020509     D L£6             S              3  0 DIM(30)
017900030807     D fig             S                   DIM(30) like(ar5trd)
018000030807     D rec             S                   DIM(30) like(ar5trd)
018100030807     D ar5             S                   DIM(30) like(ar5trd)
018200941013     D CMB             S              1    DIM(80) CTDATA PERRCD(80)
018300991206     D CMP             S              1    DIM(80) CTDATA PERRCD(80)
018400030304     D CMPf            S              1    DIM(80) CTDATA PERRCD(80)
018500941013     D CME             S              1    DIM(80) CTDATA PERRCD(80)
018600941013     D CMN             S              1    DIM(80) CTDATA PERRCD(80)
018700030715     D CM4             S              1    DIM(80) CTDATA PERRCD(80)
018800030715     D CM5             S              1    DIM(80) CTDATA PERRCD(80)
018900941013     D DOV             S              1    DIM(80) CTDATA PERRCD(80)
019000141105     D ERR             S             78    DIM(69) CTDATA PERRCD(1)
019100940624     D*
019200940624     D CEI             S              1    DIM(50)
019300020621     D ESE             S              3    DIM(50)
019400150120     D CCC             S              1    DIM(100)
019500150120     D IPB             S              1    DIM(100)
019600950121     D DIV             S              3    DIM(30)
019700950121     D DCV             S             89    DIM(30)
019800951206     D C2A             S              3    DIM(200)
019900951206     D D2A             S             89    DIM(200)
020000990930     D* VARIE DI FIAR600F+FNAR700F
020100941216     D ASV             S              1    DIM(20)
020200990930     D AVA             S             11  3 DIM(20)
020300000131     D* VARIE DI FIAR600F+FNAR700F FIGLIE
020400000131     D FSV             S              1    DIM(20)
020500000131     D FVA             S             11  3 DIM(20)
020800941216     D* WORK SIGLE DELLE VARIE X RICERCA
020900941216     D WSV             S              1    DIM(20)
021000960618     D DSLR33        E DS                  EXTNAME(FNLR33DS)
021100960618     D FLAN02          DS
021200960618     D  ANMNR2               397    400B 0
021300960618     D FLAN01          DS
021400960618     D  ANMNR1               397    400B 0
021500991028     D DGED          E DS
021600940624     D DSEI          E DS
021700000202     D OG143         E DS
021800070416     D OG146         E DS
021900051221     D OG148         E DS
022000140729     D OG150         E DS
022200030715     D trul90ds      E DS                  inz
022300940624     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
022400900418     D  TCU                  398    697
022500900418     D                                     DIM(50)
022600900418     D  KCU                  698    847P 0
022700900418     D                                     DIM(50)
022800900418     D                                     PACKEVEN
022900901219     D  DTU                  848    895P 0
023000901219     D                                     DIM(12)
023100901219     D                                     PACKEVEN
023200900418     D TCUDS           DS
023300900418     D  F3                     3      3
023400900418     D  F4                     4      4
023500900418     D  F56                    5      6
023800940928      * GESTIONE DATE
023900940928     D WLBDA8          DS
024000940928     D  G08DAT                 1      8  0
024100940928     D  G08INV                 9     16  0
024200940928     D  G08ERR                17     17
024300940928     D  G08TGI                18     22  0
024400051013     D DGNPDMC915    E DS                  prefix(d915) inz
024500051013     D DGNPDMC917    E DS                  prefix(d917) inz
024600051013     D DGcpflr       E DS
024700940217     D* PARAMETRI PASSATI AL PGM DI STAMPA BOLLA
024800030624     D FNLSB5DS      E DS
024900130315     d FNLSB6ds1     e ds                  inz
025000130315     d  DB6pdf       e                     inz('X')
025100970709     D* PARAMETRI PASSATI AL PGM TISI95R X CONTROLLO CAP
025200970709     D TISI95        E DS                  EXTNAME(TISI95DS)
025300970711     D TISI00        E DS                  EXTNAME(TISI00DS)
025400970710     D* PARAMETRI PASSATI routine cap
025500970710     D FNLV13        E DS                  EXTNAME(FNLV13DS)
025600020617      * ds reperimento se bolla trasmessa da cliente
025700020617     D DSBL4a        E DS
025800070320      * ds reperimento blocchi
025900070320     D DSBL4l        E DS
026000030826      * DS per richiamo pgm fnlr99r x gestione ORM in particolarità RC
026100030826     D fnlr99ds      E DS
026200071126     D fnlv55ds      E DS
026300080123     D fnlv59ds      E DS
026400160303     d  KS_CT                 74    118    dim(15)
026500160303     d  KS_pr                119    163    dim(15)
026600160303     d  KS_ie                164    178    dim(15)
026700041125     d
026800041125     drecordlbl      E DS                  extname(fnlbl00f)
026900941006     D* PARAMETRI PASSATI AL PGM DI INTERROGAZ. BOLLE FNLR36R
027000070622     D PARAM1          DS                  INZ
027100070622     D  PA1AAS                14     17  0
027200070622     D  PA1LNP                18     20  0
027300070622     D  PA1NRS                21     22  0
027400070622     D  PA1NSP                23     29  0
027500070622     D  PA1F03                30     30
027600070622     D  PA1FLG                31     31
027700070622     D* SE =1 NON RICH. FNLG30R IN MODO RICORSIVO DA INTERR.BOLLE
027800070622     D  PA1GIA               144    144
027900911115     D NWFGB           DS
028000911115     D  NWTB1                  1      2
028100911115     D  NWTB2                  3      4
028200911115     D  NWFCA                  5      5
028300911115     D  NWFAN                  6      6
028400911115     D  NWFTD                  7      7
028500911122     D                 DS
028600911028     D  TESTO1                 1      5
028700911121     D  GCAAGC                 6      7  0
028800911121     D  GCAFGC                 9     11  0
028900911025     D  GCANGC                13     19  0
029000911028     D  TESTO2                21     25
029100911114     D  VARIO                 27     33
029200911122     D  RIFGIA                 1     33
029300920116     D                 DS
029400920116     D  TESTO3                 1      8
029500941217     D  RAAS                   9     10  0
029600941217     D  RLNP                  12     14  0
029700941217     D  RNRS                  16     17  0
029800941217     D  RNSP                  19     25  0
029900920116     D  RIFSPE                 1     25
030000911212     D                 DS
030100911212     D  TESNWB                 1      7
030200911212     D  NEWAAS                 9     10  0
030300911212     D  NEWLNP                12     14  0
030400911212     D  NEWNRS                16     17  0
030500911212     D  NEWNSP                19     25  0
030600920522     D  TE2NWB                27     30
030700920522     D  NE2LNA                31     33  0
030800920522     D  NEWBOL                 1     33
030900990930     D YEURDS        E DS                  EXTNAME(YEURCODS)
031000990930     D CNCR80        E DS
031100020509     D TR06DS        E DS                  EXTNAME(TRUL06DS)
031200020509     D  LIN                    1     90  0 dim(30)
031300991001     D DSCC          E DS
031400060410     D DS2c          E DS
031500060808     D DS2z          E DS
031600911113     D DS2D          E DS
031700911113     D DS2E          E DS
031800040719     D DS2G2         E DS
031900911113     D DS1A          E DS
032000951206     D DS2A          E DS
032100910314     D DS3A          E DS
032200000214     D DS7R          E DS
032300110802     D DS7k          E DS
032400941216     D DS$2          E DS
032500920130     D DS7L          E DS
032600940929     D DSCV          E DS
032700941006     D DS15          E DS
032800990930     D DBLP          E DS
032900990930     D DTAS          E DS
033000060927     D DTAS01        E DS
033100990930     D DTASV         E DS
033200941216     D  NSV                    1     20
033300941216     D                                     DIM(20)
033400990930     D  NVA                   21    140P 3
033500990930     D                                     DIM(20)
033600990930     D §DBLP           DS
033700940303     D  VUOTO1                 1    256
033800941214     D  VUOTO2               257    512
033900990930     D  VUOTO3               513    624
034000941215     D DSLR48        E DS                  EXTNAME(FNLR48DS)
034100961202     D FNLG21        E DS                  EXTNAME(FNLG21DS)
034200160303     D TR27DS        E DS                  EXTNAME(TRUL27DS1)
034300160303     D trulc7ds      E DS
034400050511     D fnlra2ds      E DS
034500941214     D DSARBK        E DS
034600920212     D DSARBD        E DS
034700030207     D Dar5ban       E DS
034800030805     D Dar5bnb       E DS
034900030805     D dar5          E DS                  inz
035000031119     D Dar5gen       E DS
035100990930     D DARBT         E DS
035200990930     D  VSV                   44     63
035300941216     D                                     DIM(20)
035400990930     D  VVA                   64    183P 3
035500990930     D                                     DIM(20)
035600930511     D DSARBG        E DS
035700900418     D KPJBA         E DS
035800141003     d fnlg20ds      e ds
035900141103     d fnlry09ds2    e ds
036000940927     D FNLG82        E DS                  EXTNAME(FNLG82DS)
036100941005     D DSUL41        E DS                  EXTNAME(TRUL41DS)
036200941216     D  U41AAS       E                     EXTFLD(D41AAS)
036300941216     D  U41LNP       E                     EXTFLD(D41LNP)
036400941216     D  U41NRS       E                     EXTFLD(D41NRS)
036500941216     D  U41NSP       E                     EXTFLD(D41NSP)
036600941227     D  U41TPB       E                     EXTFLD(D41TPB)
036700960124      *  DS per scomposizione record con dati Provincia/Cap/Nazione
036800960124     D                 DS
036900960205     D  WCAD                   1      9
037000960205     D  WPRD                  11     12
037100960124     D  WNZD                  14     16
037200960124     D  WNOT                   1     35
037300020813     D* DS PER TRUL33R - REPERIMENTO/AGGIORNAMENTO NUMERATORI SU AZNUM
037400020813     D TRUL33DS      E DS
037500021114     d TIBS36DS      E DS
037600020618     d TIBS69DS      E DS                  INZ
037700020618     d DS_cnaco      E DS                  extname(CNACO00F)
037800020618     d DS_cnind      E DS                  extname(CNIND00F)
037900020618     d DS_cnclp      E DS                  extname(CNCLP00F)
038000020618     d DS_fncls      E DS                  extname(FNCLS00F)
038100060103     D tisit0ds      E DS
038200071008     D tisit0x1ds    E DS
038300060103     d ddstflo       e ds
038400060629     d fnlg24ds      e ds
038500151020     d tisie10ds     e ds
038600151020     d fieuLPDds     e ds
038700151020     d Dlpd          e ds
038800040716     d fidn12ds      e ds
038900140217     D fnlrn6ds      E DS
039000020621     D CdIVA           S              4
039100020927      *parametri per reperimento coda
039200051013     d dataiso         s               d
039300051013     d finep           s              1
039400020927     d codpo           s              3s 0
039500020927     d tpcoda          s              1
039600020927     d nomcoda         s             10
039700020927     d libcoda         s             10
039800020927     d esito           s              1
039900030807     d compar          s                   like(arbgva)
040000040315     d WORIGINE        s                   like(nchorigine)
040100040316     d Wpocompl        s                   like(nctpocompl)
040200940928     D*
040300061026      * campi salvataggio chiave bolla originale da dirottamento
040400061026     d LBLAAOsv        s                   like(LBLAAO)
040500061026     d LBLLPOsv        s                   like(LBLLPO)
040600061026     d LBLNROsv        s                   like(LBLNRO)
040700061026     d LBLNSOsv        s                   like(LBLNSO)
040800061026      *
040900041221      * eccezioni luogo 300 partenza arrivo
041000041221     D dpar            DS
041100041221     D  par                    1     60  0 dim(20)
041200041221     D darr            DS
041300041221     D  arr                    1     60  0 dim(20)
041400160805     D  zon            DS
041500060807     D  zon                    1     40  0 dim(20)
041600070322     D dgir            DS
041700070322     D  gir                    1     60    dim(6)
041800070613      *
041900070613     d skvdmc          s             50    dim(4)
042300071130     d Wdata8          DS
042400071130     d  dadata                 1      8  0
042500071130     d  adata                  9     16  0
042600071130     d  GioLav                17     21  0
042700071130      *
042800940928     D* DEFINIZIONE COSTANTI
042900940928     D*
043000940928     D CNOTA           C                   CONST('** NESSUNA NOTA **')
043100950330     D*-------------------------------------------------------------*
043200950330     D* Costanti per definzione vari importi della bolla di recupero
043300950330     D*-------------------------------------------------------------*
043400950330     D CVAR            C                   CONST('Importo Varia    ')
043500950330     D CTRA            C                   CONST('Trasporto        ')
043600950330     D CDIR            C                   CONST('Diritto fisso    ')
043700950330     D CINL            C                   CONST('Inoltro          ')
043800951214     D CNOT1           C                   CONST('C.PORTO DA ASS. A FR')
043900951214     D CNOT2           C                   CONST('ANCO CL.       ')
044000961129     D CNOT3           C                   CONST('EX.SPED. IN FRANCO F')
044100961129     D CNOT4           C                   CONST('RONTIERA       ')
044200000925     D IMPORT          C                   CONST('I              ')
044300020621
044400940304     IAZORG01L  AA
044500971113     I                                  1    3  ORGDIT
044600971113     I                                  6    6  ORGFVA
044700971113     I                                 11   11  ORGFL1
044800940304     I                                 14   33  ORGDES
044900960327     I                                 34   53  ORGIND
045000960327     I                             P   54   56 0ORGCPF
045100960327     I                                 57   76  ORGLOC
045200960327     I                                 77   78  ORGPRO
045300000202     I                               3776 3800  ORGDE3
045400070416     I                               3851 3875  ORGDE6
045500051221     I                               3901 3925  ORGDE8
045600140729     I                               3951 3975  ORGDF0
045700990730     ITITGC000
045800900424     I              TGCSG1                      TGS(1)
045900900424     I              TGCSG2                      TGS(2)
046000900424     I              TGCSG3                      TGS(3)
046100900424     I              TGCST1                      TTS(1)
046200900424     I              TGCST2                      TTS(2)
046300900424     I              TGCST3                      TTS(3)
046400950330     I*-------------------------------------------------------------*
046500140729     C*
046600970529     C     *ENTRY        PLIST
046700970529     C                   PARM                    KPJBA
046800141006     C                   PARM                    fnlg20ds
046801160729     c*
047000970529     C                   MOVEL     KPJBU         FNLG82
047200050316     c                   move      d82comm       gescomm           1
047300141007     c                   if        %parms > 1
047400141007     C                   movel     'N'           d82nodsp
047500141007     c                   endif
047600030715     c* mi compongo la ds trul90ds per pgm FNLS01R, FNLR41/8R
047700030715     c                   exsr      srtrul90
047800970529     C* PRIMA VOLTA
047900970529     C     WPRIMA        IFEQ      ' '
048000970529     C                   EXSR      DEFCAM
048100970529     C                   MOVEL     'S'           WPRIMA            1
048200970529     C                   END
048300970529     C*
048400940304     C                   MOVEL     '0'           $FINE
048500940304     C     $FINE         DOWEQ     '0'
048600940301      *
048700940301     C     $GIAC         IFEQ      '1'
048800940301     C                   EXSR      SRGIAC
048900920113     C                   ELSE
049000940301     C                   EXSR      SRDIRO
049100940301     C                   END
049200940304      *
049300940304     C     $FINE         IFEQ      '0'
049400940304     C                   MOVEL     '0'           $OKF01
049500940304     C                   MOVEL     '0'           $OKF02
049600940304     C                   MOVEL     '0'           $OKF03
049700940310     C                   MOVEL     '0'           $FOANC
049800940310     C                   MOVEL     '0'           $FOLNA
049900940310     C                   MOVEL     '0'           $FOFRI
050000940301      *
050100940304      * =======================================================
050200940304     C*?$F01 = VIDEO X DISPOSIZIONI
050300940304      * =======================================================
050400940307     C                   MOVEL     '1'           $F01
050500940307     C     $F01          DOWEQ     '1'
050600141003     c     d82nodsp      ifeq      *blank
050700940304     C                   EXSR      SRF01
050800141006     c                   else
050900141006      * se richiamato senza video salto prima routine e passo a riempire campi
051000141006      * video per completare il dirottamento
051100141006     C                   EXSR      azzer
051200141006     c                   move      '1'           $okf01
051300141003     c                   endif
051400961213      *---------------------------------------------------------*
051500961213      *  Se sto eseguendo delle disposizioni per bolle export
051600961213      *  controllo se devo emettere mascherina x fax
051700971113     C                   MOVEL     *BLANKS       ORGFL1
051800971113     C                   MOVEL     *BLANKS       ORGDIT
051900971113     C     ARBLNA        CHAIN     AZORG01L                           21
052000971113     C                   MOVEL     ORGDIT        SIFDIT            3
052100000314     C                   MOVEL     ORGDE3        OG143
052200020729     C                   MOVEL     *blank        flgest            1
052300020729     C     §OgNTW        ifeq      'FED'
052400020729     C     §OgNTW        oreq      'EEX'
052500020729     C     §OgNTW        oreq      'EUP'
052600020729     C                   MOVEL     'E'           flgest
052700020729     c                   end
052800020225      *PER FEDEX NON MANDA MAI IL FAX
052900020225     C     §OGNTW        IFNE      'FED'
053000020729     C     flgest        IFEQ      'E'
053100000314     C     VFRI          ANDNE     'D'
053200020702     C     §OGntw        OREQ      'DPD'
053300961213     C     VFRI          ANDNE     'D'
053400961213     C     $GIAC         IFEQ      '1'
053500961213     C     ARBFT3        ORNE      *BLANKS
053600961213     C     VFRI          OREQ      'R'
053700961213     C                   EXSR      CALL21
053800961213     C                   END
053900961213     C                   END
054000020225     C                   END
054100940304      * =======================================================
054200940304     C*?$F02 = VIDEO X DESTINATARIO /LNA/ COD.FINE GIACENZA...
054300940304      * =======================================================
054400940304     C     $OKF01        IFEQ      '1'
054500940304     C                   EXSR      SRF02
054600940304     C                   ENDIF
054700940307     C                   ENDDO
054800900418     C*
054900940304     C                   ENDIF
055000950113     C* SE PASSATO NR.SPEDIZ.:PGM RICHIAMATO ..VADO A FINE
055100950113     C     D82NSP        IFNE      *BLANK
055200950113     C                   MOVEL     '1'           $FINE
055300950113     C                   ENDIF
055400940304     C                   ENDDO
060100940304     C*---------------------------------------------------------------*
060200940304     C*   S R F 0 1    - FORMATO DISPOSIZIONI                         *
060300940304     C*---------------------------------------------------------------*
060400940304     C     SRF01         BEGSR
060500940304     C                   MOVEL     '1'           $F01
060600940304     C                   MOVEL     '0'           $OKF01
060700940304     C                   MOVEL     *BLANK        VNOT1
060800940304     C                   MOVEL     *BLANK        VNOT2
060900940304     C     $F01          DOWEQ     '1'
061000940304     C                   SETOFF                                       95
061100160729     c* da menu
061200940304     C     $GIAC         IFEQ      '0'
061400940309     C                   EXSR      ARBVID
061401160729     c* conferma
061402160729     c                   if        *inkf
061403160729     C                   MOVEL     '0'           $F01
061600160729     C                   MOVEL     '1'           $OKF01
061602160729     c                   end
061603160729     c* ritorno
061604160729     c                   if        *inkl
061800160729     C                   MOVEL     '0'           $OKF01
061801160729     C                   MOVEL     '0'           $F01
061900160729     C     D82NSP        IFNE      *BLANKS
062000970204     C                   MOVEL     '1'           D82F12
062100970204     C                   MOVEL     FNLG82        KPJBU
062200970204     C                   END
062202160729     C                   END
062402160729     c* richiamato
062403160729     C     $GIAC         IFEQ      '1'
062500940304     C                   EXSR      GCAVID
062700961203     C                   WRITE     LG20D08
062800961203     C* GESTIONE NOTE GIACENZE -  SFL
062900961203     C                   EXSR      GCANOT
063000941012     C                   MOVEL     '1'           $OKF01
063001160729     C                   MOVEL     '0'           $F01
063200961203     C* NON SI POSSONO ESEGUIRE DISP. PRIMA
063300961203     C     GCPDED        IFGT      AMGUDT
063400941012     C                   EXFMT     LG20Z09
063500941012     C                   MOVEL     '0'           $OKF01
063501160729     C                   MOVEL     '0'           $F01
063700941012     C                   MOVEL     '1'           $FINE
063800940928     C                   ENDIF
063801160729     C                   ENDIF
064000940304      *
064100940304     C*   ESECUZIONE DISPOSIZIONI
064200160729      *
064300940304     C     $OKF01        IFEQ      '1'
064400940304     C                   EXSR      AZZER
064600940304     C* TEST SULLE DISPOSIZIONI PER EMETTERE LE VIDEATE NECESSARIE
064700940304     C                   SELECT
064800940304     C     VFRI          WHENEQ    'R'
064900940304     C                   EXSR      RIENTR
065000940304     C     VFRI          WHENEQ    'C'
065100940304     C                   EXSR      RICONS
065200940304     C     VFRI          WHENEQ    'D'
065300940310     C                   EXSR      DIROT1
065400940304     C                   ENDSL
065500940304     C                   ENDIF
065501160729     c*
065600940304     C                   ENDDO
065700160729     C                   ENDSR
065800940308     C*---------------------------------------------------------------*
065900940308     C*   S R F 0 2    - FORMATO X FINE GIAC./DESTINAT/ LNA ....      *
066000940308     C*---------------------------------------------------------------*
066100940308     C     SRF02         BEGSR
066101160729     c* no video
066200141003     C                   IF        D82nodsp <> *blank
066300141013      *forza indicatore per sproteggere campi destinatario
066400141013     c                   seton                                        46
066401160729     c* reso
066600151127     c                   if        vfri = 'R'
066700151127     c                   exsr      rientr
066800151127     c                   else
066900141003     C                   MOVEL     d20RSD        VRSD
067000141003     C                   MOVEL     d20Rs2        VRD2
067100141003     C                   MOVEL     d20IND        VIND
067200141003     C                   MOVEL     d20LOD        VLOD
067300141003     C                   MOVEL     d20PRD        VPRD
067400141003     C                   MOVE      d20CAD        VCAD
067500141003     C                   MOVE      d20LNA        VLNA
067600141003     C                   MOVEL     d20NZD        VNZD
067700141003     C                   MOVEL     d20tel        Vteld
067800141003     C                   MOVEL     d20ref        Vref
067900141003     c                   move      d20snp        vapc
068000141003     c                   move      d20mdp        vppc
068100151127     c                   endif
068101160729     C                   MOVEL     '0'           $FOFRI            1
068300141003     c                   else
068400940309     C* SE HO FORZATO DISPOSIZ. REIMPOSTO I DATI DIGITATI
068500940309     C     $FOFRI        IFEQ      '1'
068600940309     C                   MOVEL     SAVRSD        VRSD
068700941011     C                   MOVEL     SAVRD2        VRD2
068800940309     C                   MOVEL     SAVIND        VIND
068900940309     C                   MOVEL     SAVLOD        VLOD
069000940309     C                   MOVEL     SAVPRD        VPRD
069100940309     C                   MOVE      SAVCAD        VCAD
069200940309     C                   MOVE      SAVLNA        VLNA
069300961203     C                   MOVEL     SAVNZD        VNZD
069400031121     C                   MOVEL     SAVteld       Vteld
069500031121     C                   MOVEL     SAVref        Vref
069600940309     C                   MOVEL     '0'           $FOFRI            1
069700940309     C                   END
069800141003     c                   endif
069900940309     C* SE HO FORZATO LINEA D'ARRIVO
070000940309     C                   MOVEL     '0'           $FOLNA            1
070100940309     C                   MOVEL     '0'           $RIEMI            1
070200940309     C                   SETOFF                                         1922
070300940308     C                   SETOFF                                       566265
070400941219     C                   SETOFF                                       344959
070500940308     C                   MOVEL     *BLANK        $MSG
070600940310     C     INIF02        TAG
070700941221     C                   SETOFF                                       9934
070800940308     C* EMETTO VIDEATA CON DATI FINE GIACENZA
070900940308     C                   MOVEL     '1'           $F11
071000961203      *---------------------------------------------------------*
071100961213     C     $F11          DOWEQ     '1'
071200940308     C                   SETON                                        95
071300940915     C                   WRITE     LG20Z02
071400940308     C     $GIAC         IFEQ      '0'
071500940915     C                   WRITE     LG20D04
071600940308     C                   ELSE
071700940915     C                   WRITE     LG20D08
071800940308     C                   EXSR      GCANOT
071900961203     C                   END
072000070619      *se disposizioni in consegna attivo comando per note ldv
072100091126      * e protezione campo nazione
072200091126     c                   setoff                                       1715
072300070619     c                   if        vfri = 'C'
072400091126     c                   seton                                        1715
072500070619     c                   endif
072600070619
072700940915     C                   WRITE     LG20Z03
072800940308     C                   MOVEL     '1'           $F02
072900940308     C     $F02          DOWEQ     '1'
073000940308     C                   MOVEL     '0'           $OKF02
073100940308     C                   MOVEL     '0'           $OKF03
073200940915     C                   EXFMT     LG20D06
073300070622      * richiamo pgm interrogazione bolle
073301160729     c                   if        *inkg
073400160729     C                   exsr      richbl
073401160729     c                   end
073700070613      *richiamo pgm note ldv
073800070614     c                   if        *inkr and §oggeot = 'S'
073801160729     c                   exsr      srnote
076700070613     c                   endif
076701160729     c* ritorno
076800070614     C     *INKL         IFEQ      '1'
076900940308     C                   SELECT
076901160729     c* no video
077000141007     C                   WHEN      d82nodsp <> *blank
077100141007     c                   move      'R'           d20erro
077200141007     C                   return
077201160729     c* da menu
077300940308     C     $GIAC         WHENEQ    '0'
077400940308     C                   MOVEL     '0'           $OKF02
077500940308     C                   MOVEL     '0'           $F02
077600940308     C                   MOVEL     '0'           $F11
077700940308     C                   MOVEL     '1'           $F01
077701160729     c* richiamato
077800940308     C     $GIAC         WHENEQ    '1'
077900940308     C                   MOVEL     '0'           $OKF02
078000940308     C                   MOVEL     '0'           $F02
078100940308     C                   MOVEL     '0'           $F11
078200940308     C                   MOVEL     '0'           $F01
078300940308     C                   MOVEL     '1'           $FINE
078400160729     C                   ENDsr
078500940308     C                   ELSE
078501160729     c* controllo video
078502160729     C                   EXSR      CTRV02
078600940308      *ERRORI
078700940308     C     *IN19         IFEQ      '1'
078800940308     C     *IN30         OREQ      '1'
078900940308     C                   MOVEL     '1'           $F02
079000940308     C                   MOVEL     '0'           $OKF02
079100940308     C                   ELSE
079101160729     c* conferma no errori
079102160729     c                   if        *inkf
079200160729     C                   MOVEL     '1'           $OKF02
079300160729     C                   MOVEL     '0'           $F02
079400160729     C                   MOVEL     '0'           $F01
079500160729     C     $GIAC         IFEQ      '1'
079600940308     C                   MOVEL     '1'           $FINE
079700940308     C                   END
079701160729     C                   END
079800940308     C                   END
079900941215      * SE  HO FORZATO DIROTTAMENTO MA DISPOSIZ. ERANO RICONSEGNA
080000941215      * SE  HO FORZATO RICONSEGNA DISPOSIZ. ERANO DIROTTAMENTO
080100940309     C     $FOFRI        IFEQ      '1'
080200941215     C     VFRI          IFEQ      'C'
080300940308     C                   MOVEL     'D'           VFRI
080400940309     C     $GIAC         IFEQ      '1'
080500940309     C                   MOVEL     'M'           VPSG
080600940309     C                   MOVEL     'S'           VASG
080700940309     C                   END
080800941215     C                   ELSE
080900941215     C                   MOVEL     'C'           VFRI
081000941215     C                   ENDIF
081100940308     C                   MOVEL     '0'           $OKF02
081200940308     C                   MOVEL     '0'           $F02
081300940308     C                   MOVEL     '0'           $F11
081400940308     C                   MOVEL     '1'           $F01
081500940308     C                   END
081600940309     C     $RIEMI        IFEQ      '1'
081700940309     C                   MOVEL     '0'           $OKF02
081800940309     C                   MOVEL     '0'           $F02
081900940309     C                   MOVEL     '1'           $F11
082000940310     C                   MOVEL     '0'           $RIEMI
082100940309     C                   END
082200940308     C                   END
082300940308     C     $OKF02        IFEQ      '1'
082400940308     C     *IN33         IFEQ      '1'
082500940308     C     *IN35         OREQ      '1'
082600940308     C     *IN36         OREQ      '1'
082700940308      * $F03 = VIDEO X RECUPERO SPESE GIAC./PROVV. C.ASSEGNO....
082800940308     C                   MOVEL     '1'           $F03
082900940308     C                   EXSR      SRF03
083000940308     C                   ELSE
083100940308     C                   MOVEL     '1'           $OKF03
083200940308     C                   END
083300940308     C     $OKF03        IFEQ      '1'
083400940308     C                   MOVEL     '0'           $F02
083500940308     C                   MOVEL     '0'           $F11
083700160729      *video controllo responsabilità cliente o vettore solo da menù
083701160729     C     $GIAC         ifeq      '0'
083702160729     c                   exsr      srrespo
083703160729     c                   end
083704160729     C                   CLEAR                   DSLR48
089000940308     C* VARIAZIONE BOLLA  C/ASS. O CODICE BOLLA
089100940308     C     *IN80         IFEQ      '1'
089200940308     C                   EXSR      PGARBK
089300941214     C     D48ERR        IFNE      ' '
089400941214     C                   SETON                                        19
089500941214     C                   MOVEL     D48MSG        $MSG
089600941214     C                   MOVEL     '1'           $F11
089700941214     C                   ENDIF
089800940310     C                   END
089801160729     C* VARIAZIONE BOLLA  IND. DESTINATARIO
089900940308     C     *IN81         IFEQ      '1'
090000941219     C     $F11          ANDEQ     '0'
090200051017     C                   MOVEL     'S'           D48FFR
090300940308     C                   EXSR      PGARBI
090400941214     C     D48ERR        IFNE      ' '
090500941214     C                   SETON                                        19
090600941214     C                   MOVEL     D48MSG        $MSG
090700941214     C                   MOVEL     '1'           $F11
090800941214     C                   ENDIF
090900940308     C                   END
091000940308     C* VARIAZIONE BOLLA  TASSAZ. 1° BOLLA
091100940308     C     *IN82         IFEQ      '1'
091200941219     C     $F11          ANDEQ     '0'
091300940308     C                   EXSR      PGARBT
091400941215     C     D48ERR        IFNE      ' '
091500941214     C                   SETON                                        19
091600941214     C                   MOVEL     D48MSG        $MSG
091700941214     C                   MOVEL     '1'           $F11
091800941214     C                   ENDIF
091900940308     C                   END
092000940308     C* VARIAZIONE BOLLA  TASSAZ. 2° BOLLA
092100940308     C     *IN83         IFEQ      '1'
092200941219     C     $F11          ANDEQ     '0'
092300940308     C                   EXSR      PGAR2T
092400941214     C     D48ERR        IFNE      ' '
092500941214     C                   SETON                                        19
092600941214     C                   MOVEL     D48MSG        $MSG
092700941214     C                   MOVEL     '1'           $F11
092800941214     C                   ENDIF
092900940308     C                   END
093100940308     C                   ENDIF
093200940308     C                   ENDIF
093300940308     C                   ENDDO
093400940308      *
093500940308     C                   ENDDO
093600940308      * =======================================================
093700940308     C*?SUPERATI TUTTI I CONTROLLI   AGGIORNO
093800940308      * =======================================================
094000940613      * SE ACCESO IND. PER BOLLA IN ARRIVO, MA è CAMBIATA LNA SPENGO 88
094001160729     C     $OKF03        IFEQ      '1'
094100941217     C   88VLNA          IFNE      EXLNA
094200940613     C                   SETOFF                                       88
094300940613     C                   ENDIF
094500940308     C                   EXSR      SRAGG
094600940308     C                   ENDIF
094601160729      * non creata la bolla f12 da immissione bolle
094602160729     c                   if        *in99
094700160729     C                   SETON                                        19
094800160729     C                   MOVEL     ERR(35)       $MSG
094900160729     C                   GOTO      INIF02
094901160729     c                   end
095000160729     C                   ENDSR
100700940308     C*---------------------------------------------------------------*
100800940308     C*   C T R V 0 2      FINE GIACENZA                              *
100900940308     C*---------------------------------------------------------------*
101000940308     C     CTRV02        BEGSR
101100940309     C                   SETOFF                                         1922
101200940308     C                   SETOFF                                       566265
101300941003     C                   SETOFF                                       204959
101400941219     C                   SETOFF                                       66  64
101500940308     C                   MOVEL     *BLANK        $MSG
101501160729     c* se richiamato
101600940310     C     $GIAC         IFEQ      '1'
101601160729     c* ricerca codice fine giacenza
101700941012     C                   MOVEA     VCFG          CFG§
101800940310     C     '?'           LOOKUP    CFG§                                   21
101900940308     C   21              DO
102000940308     C                   MOVEL     '2C'          §COD
102100940308     C                   MOVEL     *BLANK        §KEY
102200940308     C                   MOVEL     *BLANK        §DES
102300940308     C                   MOVE      *BLANK        VCFG
102400940308     C                   CALL      'X§TB60R'
102500940308     C                   PARM                    CODUT
102600940308     C                   PARM                    §COD
102700940308     C                   PARM                    §KEY
102800940308     C                   PARM                    §DES
102900940308     C                   MOVEL     §KEY          VCFG
103000940308     C                   MOVEL     §DES          DCFG
103100940309     C                   MOVEL     '1'           $RIEMI            1
103200160729     C                   leavesr
103300940308     C                   END
105400940310      * CONTROLLO COD FINE GIAC.
105500940308     C                   MOVEL     '2C'          TBLCOD
105600940308     C                   MOVEL     *BLANK        TBLKEY
105700940308     C                   MOVEL     VCFG          TBLKEY
105800160729     C     KTAB          CHAIN     TABEL
105801160729     c                   if        not %found(tabel00f)
105900160729     C                   MOVEL     *ALL'?'       DCFG
105901160729     c                   else
106000160729     C                   MOVEL     TBLUNI        Ds2c
106100160729     C                   MOVEL     §2cdes        DCFG
106101160729     c                   end
106200060801      * x non trovato o codice obsoleto segnalo errore
106201160729     c                   if        not %found(tabel00f)
106300160729     c                             or §2cobs <> *blank
106400060801     C                   SETON                                        1922
106600160729     C                   MOVEL     ERR(17)       $MSG
106601160729     c                   endif
106602160729      * verifico se sto dando disposizioni di riconsegna ad una giacenza
106603160729      * che ha in bolla la dcr impostata con una data maggiore della udate
106604160729      * ATTENZIONE non è un errore ma solo una segnalazione UNA_TANTUM
106605160729     c                   if        contr_dcr = *blank and
106606160729     c                             arbdcr > amgudt and arbtcr <> 'P'
106607160729     c                             and vfri = 'C'
106608160729     c                   move      amgudt        dadata
106609160729     c                   move      arbdcr        adata
106610160729     c                   CALL      'XSRLAV8'
106611160729     c                   PARM                    Wdata8
106612160729      * se il risultato è maggiore di 1 vuol dire che la dcr deve essere
106613160729      * modificata se si vuole gestire la bolla in consegna
106614160729     c                   if        giolav > 1
106615160729     c                   move      'X'           contr_dcr         1
106616160729     C                   MOVEL     '1'           $RIEMI
106617160729     c                   seton                                        19
106618160729     C                   MOVEL     ERR(66)       $MSG
106619160729     C                   leavesr
106620160729     c                   endif
106621160729     c                   endif
106700940308     C                   END
106800940308     C* DESTINATARIO
106900940308     C     *IN46         IFEQ      '1'
106901160729     c*
107000940308     C     VRSD          IFEQ      *BLANK
107100940308     C     VIND          OREQ      *BLANK
107200940308     C     VPRD          OREQ      *BLANK
107300961205     C     VNZD          ANDEQ     *BLANK
107400940308     C     VLOD          OREQ      *BLANK
107500940309     C                   SETON                                        561965
107600940308     C                   MOVEL     ERR(19)       $MSG
107601160729     c                   leavesr
107700940308     C                   END
107900940308     C* CAP A ZERO: INTERROGAZIONE SU CAPPARIO
108000941006     C     VCAD          IFEQ      *BLANK
108200970711     C                   CLEAR                   TISI00
108300970711     C                   MOVEL     'L01'         D00OP0
108400970711     C                   MOVEL     VLOD          D00LOC
108500980219     C                   MOVEL     AMGUDT        D00DRI
108600970711     C                   CALL      'TISI30R'
108700970711     C                   PARM                    KPJBA
108800970711     C                   PARM                    TISI00
109000970711     C     D00CAP        IFNE      *BLANKS
109100970711     C                   MOVEL     D00CAP        VCAD
109200970711     C                   MOVEL     D00LOC        VLOD
109300970711     C                   MOVEL     D00PRV        VPRD
109400970711     C                   MOVEL     D00NAR        VNZD
109500940308     C                   END
109700940309     C                   MOVEL     '1'           $RIEMI
109801160729     C                   leavesr
109900940308     C                   END
110000160729     C* CONTROLLO e IMPOSTA vlna
110101160729     c                   exsr      srindlna
128700160729     C   19              leavesr
128900040531      * verifica dirottamenti
129000040531      * non è ammesso il dirottamento se il cap o la località sono
129100040531      * comunque serviti dalla linea d'arrivo indipendentemente da
129200040531      * limiti di peso o altro
129300040719      * sono escluse le eccezioni della seconda condizione
129500160729     C     VFRI          ifeq      'D'
129600060103     C     des_par       ANDEQ     *blank
129700040719      *verifico se periodo con dirottamento permesso causa limitazioni traffico
129800040719     c                   if        amgudt < §2g2dpi or amgudt > §2g2dpf
129900040719
130000040531     c                   if        flgest <> 'E' and vlna <> 101 and
130100040601     c                             vlna <> 102 and arbffd <> 'S'
130200040601     c                             and arbfbr <> 'S'
130300040531     c                   if        exlna <> vlna
130400040531     c                   if        o95lla > 0
130500040531     c                   if        exlna = o95lla or exlna = o95llo or
130600040531     c                             exlna = o95lls
130700040531     c                   seton                                        1949
130800040531     C                   movel     err(65)       $msg
130901160729     C                   leavesr
131000040531     c                   endif
131100040531     c                   else
131200040531     c                   if        exlna = o95cla or exlna = o95clo or
131300040531     c                             exlna = o95cls
131400040531     c                   seton                                        1949
131500040531     C                   movel     err(65)       $msg
131601160729     C                   leavesr
131700040531     c                   endif
131800040531     c                   endif
131900040531     c                   endif
132000040531     c                   endif
132100040719     c                   endif
132200040607     c                   ENDIF
132201160729     c                   ENDIF
132300160729     C* Controllo se la Vlna esiste
132400071126      * verifico se la filiale è gestita in £6 da qualcuno nel caso
132500071126      * i riferimenti di azorg x le ds og146/148 sono quelli della capofila
132600071126      **
132700071126     c                   clear                   Og_da_£6          1
132800071126     c                   exsr      capofila
132900071126      *
133000971113     C                   MOVEL     *BLANK        VDLNA
133100160729     C     VLNA          CHAIN     AZORG01L                           21
133200971113     C     *IN21         IFEQ      '0'
133300971113     C     ORGFVA        COMP      '*'                                    21
133400971113     C                   MOVEL     ORGDES        VDLNA
133500071126      * se gestita da altri le ds vengono impostate con i campi valorizzati
133600071126      * nella routine capofila
133700071126     c                   if        Og_da_£6 = *blank
133800051221     C                   MOVEL     orgde8        og148
133900070416     C                   MOVEL     orgde6        og146
134000071126     c                   endif
134100070424     c                   if        §ogdda <> *blank
134200070424     c                   move      §ogdda        ogddan            8 0
134300070424     c                   endif
134500051221      *  Linea Arrivo non utilizzabile in Bollettazione
134600051221      *     es.: x DPD  le nuove linee di Backup sono settate così.
134700051221     C     §OGLBL        IFEQ      'N'
134800051221     C                   SETON                                        1949
134900051221     C                   MOVEL     ERR(21)       $MSG
134901160729    2C                   ENDIF
135300971113     C                   ELSE
135400971113     C                   MOVEL     *ALL'?'       VDLNA
135500971113     C                   SETON                                        1949
135600971113     C                   MOVEL     ERR(21)       $MSG
135700971113     C                   END
135800940308     C* CMD 09 X FORZARE LINEA ARRIVO
136000940309     C   KI              MOVEL     '1'           $FOLNA
136100940308     C*
136200940309     C   46
136300940309     CANN19$FOLNA        IFEQ      '0'
136400940316     C     VLNA          IFNE      *ZEROS
136500940308     C     §LNA          COMP      VLNA                               2121
136600940309     C   21              SETON                                        194959
136700940308     C   21              MOVEL     ERR(22)       $MSG
136800940316     C                   END
136900940308     C                   END
137000940309     C* LINEA ARRIVO
137100940308     C*  SE HO FORZATO LA LINEA LA ZONA = 0
137200940309     C   46$FOLNA        IFEQ      '1'
137300940308     C     §LNA          COMP      VLNA                               2121
137400070904     c  n55
137500070904     Can 21              MOVE      *ZEROS        §LPZNC               1949
137600940308     C                   END
137700070509     c* verifico se per la linea di arrivo che si sta impostando è presente
137800070509      * un giro predefinito per assegnarlo solo in caso di RESO e se esiste
137900070509      * un LUOGO 300 legato al codice cliente
138000070510     c                   if        vfri = 'R' and vlna <> §lplna
138100070510     c                   clear                   §lpcgi
138200070510     c                   exsr      ecceluogo2
138300070510     c                   endif
138400941003     C* CONTROLLO NAZIONE
138500941217     C  N19
138600941217     CAN 46              DO
138700941003     C                   MOVEL     '15'          TBLCOD
138800941003     C                   MOVEL     *BLANK        TBLKEY
138900941003     C                   MOVEL     VNZD          TBLKEY
139000941003     C     KTAB          CHAIN     TABEL                              21
139100941003     C   21              MOVEL     *ALL'?'       §15DES
139200941003     C   21              SETON                                        6619
139300941003     C   21              MOVEL     ERR(40)       $MSG
139400941003     C  N21              MOVEL     TBLUNI        DS15
139500941003     C                   END
139600941217     C* CONTROLLO TIPO INCASSO CONTASSEGNO SE GIA EMESSO IL CAMPO
139700941217     C     *IN34         IFEQ      *ON
139800941217     C* RICERCA
139900941217     C     '?'           SCAN      VTIC                                   21
140000941217     C*
140100941217     C     *IN21         IFEQ      *ON
140200941217     C                   MOVEL     '1A'          §COD
140300941217     C                   MOVE      *BLANKS       VTIC
140400941217     C                   CALL      'X§TABER'
140500941217     C                   PARM                    CODUT
140600941217     C                   PARM                    §COD
140700941217     C                   PARM                    §KEY
140800941217     C                   PARM                    §DES
140900941217     C                   MOVEL     §KEY          VTIC
141000941221     C                   MOVEL     '1'           $RIEMI
141100160729     C                   leavesr
141200941217     C                   ENDIF
141300941217     C                   ENDIF
141400941217     C* SE HANNO FATTO VARIAZIONI SUL C/ASSEGNO  CONTROLLO
141500941217     C* IL TIPO INCASSO C/ASSEGNO E LA CONGRUENZA CON LA DIVISA
141600941217     C     VVCS          IFEQ      'S'
141700020417     C     VASS          ANDNE     0
141800941217     C                   MOVEL     '1A'          TBLCOD
141900941217     C                   MOVEL(P)  VTIC          TBLKEY
142000941217     C     KTAB          CHAIN     TABEL                              21
142100941217     C   21              SETON                                        6419
142200941217     C   21              MOVEL     ERR(43)       $MSG
142300941217     C  N21              MOVEL     TBLUNI        DS1A
142400941222     C     VVCS          IFEQ      'S'
142500941222     C     VASS          ANDEQ     0
142600941222     C     *IN34         ANDEQ     *OFF
142700941222     C                   SETON                                        34
142800941222     C                   MOVEL     '1'           $RIEMI
142901160729     C                   leavesr
143000941222     C                   ENDIF
143100941217     C* TIPO INCASSO ERRATO
143200941217     C     *IN64         IFEQ      *ON
143300941222     C* SE HO AGGIUNTO IL C/ASSEGNO  CHIEDO IL TIPO INCASSO
143400941217     C* SE DIVISA ESTERA VEDO SE POSSO ACCETTARE IL TIPO INCASSO
143500020227     C     §CVDVA        ORNE      'S'
143600941217     C     §1AUDE        ANDNE     'S'
143700941217     C* SE IL TIPO INCASSO NON è CONGRUENTE EMETTO A VIDEO IL CAMPO
143800941217     C* PER POTERLO CORREGGERE
143900941217     C     *IN34         IFEQ      *OFF
144000941217     C                   SETON                                        34
144100941217     C                   MOVEL     '1'           $RIEMI
144201160729     C                   leavesr
144300941217     C                   ELSE
144400941217     C* ALTRIMENTI SE L'HO GIA' EMESSO DO ERRORE
144500941217     C                   SETON                                        19
144600941217     C                   MOVEL     ERR(43)       $MSG
144701160729     C                   leavesr
144800941217     C                   ENDIF
144900941217     C                   ENDIF
145000941217     C                   ENDIF
145100941217     C*
145200940308     C* SE RICHIAMO PGM TASSAZ. LA LINEA ARRIVO DEVE ESSERE =
145300940308     C  N19
145400940308     CAN 87VSIFAT        IFEQ      'S'
145500941216     C     VLNA          ANDNE     EXLNA
145600940308     C                   SETON                                        1962
145700940308     C                   MOVEL     ERR(18)       $MSG
145800940308     C                   END
145900971006      *-------------------------------------------------------
146000971006     C* Recupero codice cliente mittente
146100971006     C     EXPORT        IFEQ      'A'
146200971006     C     WEXTB1        ANDNE     'AS'
146300971006     C                   MOVE      ARBCCM        WVARIO            4 0
146400971006     C                   ELSE
146500971006     C                   MOVE      ARBKSC        WVARIO
146600971006     C                   END
146700941215     C* CMD 08 X FORZARE DIROTTAMENTO O RICONSEGNA IN BASE LNA
146800941215      *-------------------------------------------------------
146900940309     C   KH              DO
147000971006     C* Dirottamento non forzabile se provengo da giacenza
147100971006     C* e il mittente non è codificato
147200971006     C     WVARIO        IFNE      0
147300971006     C     WVARIO        ANDNE     8888
147400971006     C     WVARIO        ANDNE     9999
147500980505     C     VFRI          OREQ      'D'
147600971006     C     $GIAC         ORNE      '1'
147700060228     C     des_par       ORNE      *blank
147800940309     C                   MOVEL     '1'           $FOFRI
147900940309     C                   MOVEL     VRSD          SAVRSD
148000941011     C                   MOVEL     VRD2          SAVRD2
148100940309     C                   MOVEL     VIND          SAVIND
148200940309     C                   MOVEL     VLOD          SAVLOD
148300940309     C                   MOVEL     VPRD          SAVPRD
148400940309     C                   MOVE      VCAD          SAVCAD
148500940309     C                   MOVE      VLNA          SAVLNA
148600961203     C                   MOVEL     VNZD          SAVNZD
148700031119     C                   MOVEL     Vteld         SAVteld
148800031119     C                   MOVEL     Vref          SAVref
148900940309     C                   END
149000971006     C                   END
149100940308      * SE LINEA ARRIVO DIVERSA  E DISPOSIZIONI SONO DI RICONSEGNARE
149200940308      * SEGNALO CHE LE DISPOSIZ. SARANNO CAMBIATE IN DIROTTAM.
149300940309     C     $FOFRI        IFEQ      '0'
149400941229     C     VLNA          ANDNE     *ZEROS
149500941216     C     VLNA          ANDNE     EXLNA
149600940308     C     VFRI          ANDEQ     'C'
149700971006     C* Se devo forzare disposizioni di dirottamento nel caso
149800971006     C* di spedizioni con codice cliente mittente non codif.
149900971006     C* su una bolla giacente do' errore perchè le spedizioni
150000971006     C* non possono essere immesse in arrivo
150100971006     C     WVARIO        IFNE      0
150200971006     C     WVARIO        ANDNE     8888
150300971006     C     WVARIO        ANDNE     9999
150400971006     C     $GIAC         ORNE      '1'
150500060228     C     des_par       ORNE      *blank
150600940308     C                   SETON                                        19
150700940308     C                   MOVEL     ERR(29)       $MSG
150800971006     C                   ELSE
150900971006     C                   SETON                                        19
151000971006     C                   MOVEL     ERR(56)       $MSG
151100940308     C                   END
151200971006     C                   END
151300940308     C*
151400160729     C   19              leavesr
151500941215      * SE LINEA ARRIVO UGUALE MA  DISPOSIZIONI SONO DI DIROTTARE
151600941215      * SEGNALO CHE LE DISPOSIZ. SARANNO CAMBIATE IN RICONSEGNA
151700961128     C     $FOFRI        IFEQ      '0'
151800961128     C     VLNA          ANDEQ     EXLNA
151900941215     C     VFRI          ANDEQ     'D'
152000941215     C                   SETON                                        19
152100941215     C                   MOVEL     ERR(42)       $MSG
152101160729     C                   leavesr
152200941215     C                   END
152600940308     C* VARIAZIONE INDIRIZZO X RICONSEGNA NO NUOVA BOLLA
152700940308     C  N31VFRI          IFEQ      'C'
152800940308     C     VRSD          IFNE      ARBRSD
152900941011     C     VRD2          ORNE      AR4RD2
153000940308     C     VIND          ORNE      ARBIND
153100940308     C     VLOD          ORNE      ARBLOD
153200940308     C     VPRD          ORNE      ARBPRD
153300940308     C     VCAD          ORNE      ARBCAD
153400940930     C     VNZD          ORNE      ARBNZD
153500060314     C     Vref          ORNE      §AR5REF
153600060317     C     flgdis        andne     'F'
153700060314     C     Vteld         ORNE      §AR5TELD
153800940308     C                   SETON                                        81
153900940308     C                   END
154000940308     C                   END
154100940310     C* DIROTTAMENTO 2 - OPERAZ. DA FARE IN BASE AI DATI IMPOSTATI
154200940310     C     VFRI          IFEQ      'D'
154300940310     C                   EXSR      DIROT2
154401160729     C   19              leavesr
154500940310     C                   END
154600160729     C                   ENDSR
162100940310     C* ------------------------------------------------------
162200940310     C* D I R O T 2  - OPERAZIONI DA FARE X DIROTTAM
162300940310     C* ------------------------------------------------------
162400940310     C     DIROT2        BEGSR
162701160804     C*  SE LA NUOVA LINEA DI ARRIVO E' UGUALE A 101 o 102 azzero
162702160804     C*  C/ASSEGNO e SE PORTO ASSEGNATO --> FRANCO
162703160804     C     *INKF         IFEQ      '1'
162704160804     C     VLNA          ANDEQ     101
162705160804     C     *INKF         OREQ      '1'
162706160804     C     VLNA          ANDEQ     102
162707160804     C     VASS          IFNE      0
162708160804     C                   MOVEL     'S'           VVCS
162709160804     C                   Z-ADD     0             VNCA
162710160804     C                   END
162711160804     C     EXPORT        IFEQ      'A'
162712160804     C                   MOVEL     'F'           VNPC
162713160804     C                   END
162714160804     C                   END
162800941217     C* -----------------> TRATTO LA VECCHIA BOLLA
162900160805     C                   exsr      srdsk
167100941217     C* -----------------> TRATTO LA NUOVA BOLLA
167101160804     c                   exsr      srnewbol
167102160805     c*
180500941010     C                   ENDSR
180600940308     C*---------------------------------------------------------------*
180700140609     C* forzo cod.tratt.merce 2k in caso di dirottamento per fare stampare
180800140609     c* il segnacollo in arrivo
180900940308     C*---------------------------------------------------------------*
181000140609     C     SRctm2k       BEGSR
181100140610     C                   MOVEL     '6 '          §LPCTM
181300140729     c* solo se lna della bolla (precedente) è un 1° livello (considero £6)
181400140729     c                   clear                   FNLV55ds
181500140729     c                   eval      D55tpt =  '6'
181600140729     c                   eval      D55lin =  exlna
181700140729     c                   eval      D55drf =  amgudt
181800140729     c                   call      'FNLV55R'
181900140729     c                   parm                    FNLV55ds
182200140729     c                   if        d55tfa= simfel
183700140729     C     d55tfa        CHAIN     AZORG01L                           21
183800140729     c                   if        not *in21
183900140729     C                   MOVEL     ORGDf0        OG150
184000140729     c                   if        §OGDKC='S'
184100140609     C                   MOVEL     '2K'          §LPCTM
184200140729     c                   endif
184300140729     c                   endif
184600140610     c                   end
184700140609     C                   ENDSR
184800140609     C*---------------------------------------------------------------*
184900140609     C* S R F 0 3     VIDEO X SPESE GIACENZA ......
185000140609     C*---------------------------------------------------------------*
185100140609     C     SRF03         BEGSR
185200940309     C                   SETOFF                                       6157
185300940308     C     $F03          DOWEQ     '1'
185400991028     C* Imposto flag trovata tariffa di giacenza
185500991028     C                   MOVEL     'N'           WTRTAR            1
185600160304     C                   MOVEL     'EUR'         WGCDIV            3
185700160304     C                   MOVEL     'EUR'         WNWDIV            3
185800060410     c* verifica se richiamare routine per calcolo spese sui dati del prg 0
185900060410     c* se no spese oppure se motivo con azzeramento spese non esegue
186000060410     c                   if        gcpasg <>'N' and §2cnosp = *blank
186100940310     C   33$GIAC         IFEQ      '1'
186200940310     C                   EXSR      TGCVID
188100160304     c
188200991001     C                   END
188300060410     c                   endif
188400060410      *
188500940915     C                   WRITE     LG20Z03
188600940915     C                   EXFMT     LG20D07
188700940308     C                   EXSR      CTRV03
188800940308     C                   MOVEL     '0'           $OKF03
188900940308     C   19              MOVEL     '1'           $F03
189000940308     C   KL              MOVEL     '0'           $F03
189100940308     C   KF
189200940308     CANN19              MOVEL     '0'           $F03
189300940308     C   KF
189400940308     CANN19              MOVEL     '1'           $OKF03
189500940308F03  C                   END
189600940308     C     ENDF03        ENDSR
189700940308     C*---------------------------------------------------------------*
189800940308     C*   T G C V I D    DATI TARIFFA GIACENZA                        *
189900940308     C*---------------------------------------------------------------*
190000940308     C     TGCVID        BEGSR
190100940930     C                   MOVEL     *BLANK        WTCLI             1
190200941010     C                   MOVE      *ZEROS        KKSC
190300941010     C     §KSC          IFGT      0
190400941006     C                   MOVE      §KSC          KKSC
190500941010     C                   ENDIF
190600941006     C                   Z-ADD     KKSC          APPO              4 0
190700020225      *RICHIAMO PROGRAMMA PER REPERIMENTO INFORMAZIONI TARIFFA
190800020225     C                   CLEAR                   TR27DS
190900021118     C                   MOVE      ARBctr        I27ctb
191000020318     C                   MOVE      ARBTSP        I27TSP
191100020318     C                   MOVE      ARBLNA        I27LNA
191200020318     C                   MOVE      ARBLNP        I27LNP
191300020225     C                   MOVE      KKSC          I27CLI
191600160303     C                   CALL      'TRUL27R1'
191700020225     C                   PARM                    TR27DS
191800941219     C* SE CLIENTE CODIFICATO CERCO LA SUA TARIFFA
191900150512     C                   CLEAR                   fnlv59ds
192000150512     C                   MOVE      ARBCTR        KCTR
192100150512     C                   MOVEL     ' '           ilv59tbo
192200150512     C                   MOVEL     KCI           ilv59kci
192300150512     C                   MOVEL     ARBTSP        ilv59tsp
192400150512     C                   MOVEL     'S'           ilv59CON
192500150512     C                   MOVEL     AMGDSP        ilv59dsp
192600150512     c                   eval      ilv59ta2='B'
192700150512     c                   eval      ilv59blo='S'
192800150512     C                   MOVEL     KKSC          ilv59KSC
192900000202     C                   MOVEL     KKSC          W0030             3 0
193000160303    1C     APPO          IFNE      8888
193100940930     C     APPO          ANDNE     9999
193200941010     C     APPO          ANDNE     *ZEROS
193300940930     C*  Cerco progressivo tariffa in vigore su TNTAM
193400150512     C                   MOVE      O27FIE        ilv59FIE
193500150512     C                   MOVEL     KCTR          ilv59CTR
193600150512     C                   CALL      'FNLV59R'
193700150512     C                   PARM                    fnlv59ds
193800941219     C*
193900160303    2C     olv59ERR      IFEQ      *BLANKS
194000150512     C     olv59tks      ANDEQ     'S'
194100991028     C                   MOVEL     'S'           WTRTAR
194200941219     C*  Se ho trovato tariffa in vigore mi posiziono sul file
194300941219     C*  delle tariffe giacenze con il proggressivo giusto
194400150512     C                   MOVE      olv59prg      KPRG
194500990730     C     KTGC          CHAIN     TITGC01L                           21
194600941219     C  N21              MOVEL     'S'           WTCLI
194700160303    2C                   ENDIF
194800160303    1C                   ENDIF
194900940930     C*
195000940930     C** SE NON HO TROVATO TARIFFA CLIENTE CERCO SU TAR. CARTELLO
195100160303    1C     WTCLI         IFEQ      *BLANK
195200160303     c* Carico la cartello :
195300160303     c*  - per 8888 col fie della bolla
195400160303     c*  - per tariffe cliente esistente col fie della tariffa
195500160303     c                   clear                   trulc7ds
195600160303    2C     olv59ERR      ifeq      *blanks
195700160303     C     olv59tks      andeq     'S'
195800160303     c                   z-add     1             indx              3 0
195900160303     c                   eval      indx=%lookup(ilv59ctr:ks_ct)
196000160303    3c                   if        indx>0 and %editc(olv59prg:'X')=
196100160303     c                             ks_pr(indx)
196200160303     c                   eval      ic7tfie=ks_ie(indx)
196300160303   x3c                   else
196400160303     c                   eval      ic7tntw=o27ntw
196500160303    3c                   endif
196600160303     c
196700160303   x2c                   else
196800160303     c                   eval      ic7tntw=o27ntw
196900160303    2c                   endif
197000160303     c
197100160303     C                   MOVEL     AMGDSP        ic7dta
197200160303     C                   move      arbctr        Ic7ctb
197300160303     c                   call      'TRULC7R'
197400160303     c                   parm                    trulc7ds
197500160303     c
197600940930     C*  Mi posiziono sul file
197700940930     C*  delle tariffe giacenze con il proggressivo giusto
197800160303     C                   MOVE      Oc7CTRc       KCTR
197900160303     C                   MOVE      Oc7PRGc       KPRG
198000160303     C                   z-add     Oc7KSCc       KKSC
198100990730     C     KTGC          CHAIN     TITGC01L                           21
198200941219     C* SE NON HO TROVATO TARIFFA GIACENZA LO SEGNALO
198300941219     C     *IN21         IFEQ      '1'
198400941219     C                   SETON                                        57
198500941219     C                   GOTO      ENDTGC
198600941219     C                   ENDIF
198700940930     C                   ENDIF
198800940308     C*
198900940308     C     VDIG          IFEQ      *ZEROS
199000940308     C                   Z-ADD     VDTG          VDSD
199100940308     C                   ELSE
199200940308     C                   Z-ADD     VDIG          VDSD
199300940308     C                   END
199400941012     C     VDED          IFNE      *ZEROS
199500941012     C                   Z-ADD     VDED          VDSA
199600941012     C                   ELSE
199700941012     C     VDDM          IFNE      *ZEROS
199800940308     C                   Z-ADD     VDDM          VDSA
199900940325     C                   END
200000941012     C                   END
200100940308     C*
200200050701     C* CALC. le riaperture
200300050701     c                   exsr      srconta
200400050701     C* CALC. I GG. COMPLESSIVI DI SOSTA DELLE RIAPERTE
200500940308     C                   MOVE      *ZEROS        §GGRIA            3 0
200600050222     C     KGIP          SETLL     tigcp000
200700940930      *
200800940308     C     *IN21         DOUEQ     '1'
200900050316     C     KGIP          READE(e)  TIGCP000                               21
201000050316     c                   if        %error
201100070521     C     KGIP          READE(n)  TIGCP000
201200050316     c                   exsr      geslck
201300050318     C     KGIP          READE(e)  TIGCP000                               21
201400050316     c                   endif
201500940930     C*
201600940930     C  N21GCPFRG        IFGT      0
201700060410     C     GCPasg        andne     'N'
201800940930     C                   MOVE      *ZEROS        G08DAT
201900940930     C                   MOVE      *ZEROS        G08INV
202000940308     C* CALCOLO GG SOSTA PER OGNI RIAPERTURA
202100941006     C     GCPDUR        IFGT      *ZEROS
202200941006     C                   MOVE      GCPDUR        G08INV
202300940308     C                   ELSE
202400940930     C                   MOVEL     GCPAGC        G08INV
202500940930     C                   MOVE      GCPMGC        G08INV
202600940308     C                   END
202700941006     C                   MOVEL     '3'           G08ERR
202800940930     C                   CALL      'XSRDA8'
202900940930     C                   PARM                    WLBDA8
203000940930     C                   Z-ADD     G08TGI        §GGDA
203100940308     C*
203200941011     C* SE C'E' DATA ESEGUIB. CALCOLO CON QUELLA
203300941011     C     GCPDED        IFGT      *ZEROS
203400941011     C                   MOVE      GCPDED        G08INV
203500941011     C                   ELSE
203600940314     C     GCPDDM        IFNE      *ZEROS
203700940930     C                   MOVE      GCPDDM        G08INV
203800940314     C                   ELSE
203900940930     C                   MOVE      AMGUDT        G08INV
204000940314     C                   END
204100941011     C                   END
204200940930     C                   MOVEL     '3'           G08ERR
204300940930     C                   CALL      'XSRDA8'
204400940930     C                   PARM                    WLBDA8
204500940930     C                   Z-ADD     G08TGI        §GGA
204600940308     C*
204700940308     C     §GGA          SUB       §GGDA         §GGXX             3 0
204800940308     C                   ADD       1             §GGXX
204900940308     C                   ADD       §GGXX         §GGRIA
205000940930     C                   ENDIF
205100940930     C                   ENDDO
205200940930      *
205300940930      * IMPOSTO I CAMPI A VIDEO SOLO LA 1° VOLTA
205400940930      *
205500940930     C     SGD$          IFEQ      *BLANK
205600940308     C                   Z-ADD     TGCSGD        VSGD
205700940330     C* MOLTIPLICO SPESE DOSSIER    X NR. RIAPERTURE
205800940330     C     VSGD          MULT      §NRG          VSGD
205900940325     C                   MOVEL     '1'           SGD$
206000150512     C     olv59div      IFNE      *BLANK
206100150512     C                   MOVE      olv59div      V6CDIV
206200150512     C                   MOVE      olv59div      V6CRVA
206300150512     C                   MOVE      olv59div      V6CRTA
206400020318     C                   ENDIF
206500940930     C                   ENDIF
206600940930      * GG FRANCHIGIA
206700940930     C     SGF$          IFEQ      *BLANK
206800940930     C                   Z-ADD     TGCSGF        VSGF
206900940325     C                   MOVEL     '1'           SGF$
207000940930     C                   END
207100940930      * CALCOLO GG. SOSTA IMPOSTATI A VIDEO
207200060111     C                   EXSR      TGCCAL
207300940308     C*                    -----------
207400940308     C* SPESE RICONSEGNA
207500940930     C     SGR$          IFEQ      *BLANK
207600941228     C     ARBFIN        IFNE      'C'
207700110623     C     ARBFIN        andne     'T'
207800940308     C                   Z-ADD     TGCSGP        §SGR
207900940308     C                   ELSE
208000941006     C                   Z-ADD     TGCSGR        §SGR
208100940308     C                   END
208200940930     C* X NR. RIAPERTURE
208300050701     C     §SGR          MULT      d09spe        §SGR
208400940308     C* SPESE RICONS.AL QUINTALE + SPESE VARIE
208500940308     C     §SGR          MULT      §PESCA        §SGR
208600100715     C     §SGR          ADD       TGCSGV        VSGR
208700100715     c                   if        tgcfaf = '3' and
208800100715     c                             vggs <= vsgf
208900100715     c                   sub       tgcsgv        vsgr
209000100715     c                   endif
209100940325     C                   MOVEL     '1'           SGR$
209200060407     C                   ENDIF
209300940308     C     ENDTGC        ENDSR
209400050701     C*-----------------------------------------------------*
209500050701     c* conto quanti record di giacenza ho e quanti di questi sono motivi
209600050701     c* di aperture tecniche
209700050701     C*-----------------------------------------------------*
209800050701     C     srconta       BEGSR
209900050701     C*
210000050701     c                   clear                   fnlg09ds
210100050701     c                   eval      d09tla = 'R'
210200050701     c                   eval      d09AGC = §AGC
210300050701     c                   eval      d09FGC = §FGC
210400050701     c                   eval      d09NGC = §NGC
210500050701     c                   movel(p)  fnlg09ds      kpjbu
210600050701     C                   CALL      'FNLG09R'
210700050701     C                   PARM                    kpjba
210800050701     c                   movel(p)  kpjbu         fnlg09ds
210900050701     c     d09cnt        sub       d09ced        §nrg
211000060808      * verifico se motivvo di chiusura è da eliminare da motivi di riconsegna
211100060808      * solo se non l'ho precedentemente escluso come motivo di apertura PRG 0
211200060808     C                   movel     '2Z'          TBLCOD
211300060808     C                   movel     *blank        TBLKEY
211400060808     C                   movel     GCpcmc        TBLKEY
211500060808     C     KTAB          CHAIN     TABEL                              21
211600060808     C  N21              MOVEL     TBLUNI        Ds2z
211700060808     c                   if        §2znsr <> 'N' and §2nospr <> *blank
211800060808     c                   sub       1             d09spe
211900060808     c                   endif
212000050701     C*
212100050701     C                   ENDSR
212200940308     C/SPACE 3
212300991001     C*---------------------------------------------------------------*
212400991001     C*   T A R D I V    REPERISCO DIVISA TARIFFA IN VIGORE X SP.Giac *
212500991001     C*---------------------------------------------------------------*
212600160304     C     TARDIV        BEGSR
212700160304     c***
212800160304     C**Memorizzo divisa con cui ho calcolato le spese di giacenza
212900160304     C                   MOVEL     olv59div      WGCDIV            3
213000160304     C**Calcolo tariffa in vigore
213100160304     C                   MOVEL     *BLANK        WTCLI             1
213200160304     C                   MOVE      *ZEROS        KKSC
213300160304     C     §KSC          IFGT      0
213400160304     C                   MOVE      §KSC          KKSC
213500160304     C                   ENDIF
213600160304     C                   Z-ADD     KKSC          APPO              4 0
213700160304     C**SE CLIENTE CODIFICATO CERCO LA SUA TARIFFA
213800160304     C                   CLEAR                   fnlv59ds
213900160304     C                   MOVE      ARBCTR        KCTR
214000160304     C                   MOVEL     ' '           ilv59TBO
214100160304     C                   MOVEL     KCI           ilv59KCI
214200160304     C                   MOVEL     ARBTSP        ilv59TSP
214300150512     C                   MOVEL     'S'           ilv59CON
214400150512     c                   eval      ilv59ta2='B'
214500150512     c                   eval      ilv59blo='S'
214600150512     C                   MOVEL     AMGUDT        ilv59DSP
214700150512     C                   MOVEL     O27FIE        ilv59FIE
214800991001     C     APPO          IFNE      8888
214900991001     C     APPO          ANDNE     9999
215000991001     C     APPO          ANDNE     *ZEROS
215100991001     C*  Cerco progressivo tariffa in vigore su TNTAM
215200150512     C                   MOVEL     KCTR          ilv59CTR
215300150512     C                   MOVEL     KKSC          ilv59KSC
215400150512     C                   CALL      'FNLV59R'
215500150512     C                   PARM                    fnlv59ds
215600991001     C*
215700150512     C     olv59err      IFEQ      *BLANKS
215800150512     C     olv59tks      ANDEQ     'S'
215900991028     C                   MOVEL     'S'           WTRTAR
216000991001     C*  Se ho trovato tariffa in vigore mi posiziono sul file
216100991001     C*  delle tariffe giacenze con il proggressivo giusto
216200150512     C                   MOVE      olv59prg      KPRG
216300991001     C     KTGC          SETLL     TITGC01L                               21
216400991001     C   21              MOVEL     'S'           WTCLI
216500991001     C                   ENDIF
216600991001     C                   ENDIF
216700991001     C*
216800991001     C** SE NON HO TROVATO TARIFFA CLIENTE CERCO SU TAR. CARTELLO
216900991001     C     WTCLI         IFEQ      *BLANK
217000991001     C*  Mi posiziono sul file
217100991001     C*  delle tariffe giacenze con il proggressivo giusto
217200160304
217700160304
217800991001     C     KTGC          SETLL     TITGC01L                               21
217900991001     C* SE trovata tariffa memorizzo divisa relativa altrimenti la
218000991001     C* imposto uguale a quella di giacenza
218100991001     C                   ENDIF
218200150512     C     olv59err      IFNE      '1'
218300150512     C                   MOVEL     olv59div      WNWDIV            3
218400991028     C                   ELSE
218500991028     C                   MOVEL     WGCDIV        WNWDIV
218600991028     C                   ENDIF
218700991001     C*
218800991001     C                   ENDSR
218900991001     C*---------------------------------------------------------------*
219000991001     C* CNVSPG: Procedo alla conversione delle spese di giacenza      *
219100991001     C*---------------------------------------------------------------*
219200991001     C     CNVSPG        BEGSR
219300991001     C*
219400991001     C* converto le spese di sosta
219500991001     C                   CLEAR                   YEURDS
219600991001     C                   MOVEL     WGCDIV        YECDVI
219700991001     C                   Z-ADD     VTSGS         YECIMI
219800991001     C                   MOVEL     WNWDIV        YECDVO
219900991001     C                   MOVEL     '3'           YECDCO
220000991001     C                   MOVEL     'H'           YECTAR
220100991001     C                   CALL      'YEURCO'
220200991001     C                   PARM                    YEURDS
220300991001     C                   Z-ADD     YECIMO        VTSGS
220400991001     C* converto le spese di riconsegna
220500991028     C     WNWDIV        IFNE      V6CDIV
220600991001     C                   CLEAR                   YEURDS
220700991001     C                   MOVEL     WGCDIV        YECDVI
220800991001     C                   Z-ADD     VSGR          YECIMI
220900991001     C                   MOVEL     WNWDIV        YECDVO
221000991001     C                   MOVEL     '3'           YECDCO
221100991001     C                   MOVEL     'H'           YECTAR
221200991001     C                   CALL      'YEURCO'
221300991001     C                   PARM                    YEURDS
221400991001     C                   Z-ADD     YECIMO        VSGR
221500991001     C* converto le spese di dossier
221600991001     C                   CLEAR                   YEURDS
221700991001     C                   MOVEL     WGCDIV        YECDVI
221800991001     C                   Z-ADD     VSGD          YECIMI
221900991028     C                   MOVEL     WNWDIV        YECDVO
222000991001     C                   MOVEL     '3'           YECDCO
222100991001     C                   MOVEL     'H'           YECTAR
222200991001     C                   CALL      'YEURCO'
222300991001     C                   PARM                    YEURDS
222400991001     C                   Z-ADD     YECIMO        VSGD
222500991001     C* converto le spese aggiuntive
222600991001     C                   CLEAR                   YEURDS
222700991001     C                   MOVEL     WGCDIV        YECDVI
222800991001     C                   Z-ADD     VSGA          YECIMI
222900991001     C                   MOVEL     WNWDIV        YECDVO
223000991001     C                   MOVEL     '3'           YECDCO
223100991001     C                   MOVEL     'H'           YECTAR
223200991001     C                   CALL      'YEURCO'
223300991001     C                   PARM                    YEURDS
223400991001     C                   Z-ADD     YECIMO        VSGA
223500991028     C                   END
223600991028     C* Se esistono provvigioni C/Assegno converto anche quelle
223700991028     C     VRCA          IFGT      0
223800991028     C     V6CRVA        ANDNE     WNWDIV
223900991028     C                   CLEAR                   YEURDS
224000991028     C                   MOVEL     V6CRVA        YECDVI
224100991028     C                   Z-ADD     VRCA          YECIMI
224200991028     C                   MOVEL     WNWDIV        YECDVO
224300991028     C                   MOVEL     '3'           YECDCO
224400991028     C                   MOVEL     'H'           YECTAR
224500991028     C                   CALL      'YEURCO'
224600991028     C                   PARM                    YEURDS
224700991028     C                   Z-ADD     YECIMO        VRCA
224800991028     C                   MOVEL     WNWDIV        V6CRVA
224900991028     C                   END
225000991001     C*
225100991028     C* Se esistono provvigioni C/Assegno converto anche quelle
225200991028     C     VRTR          IFGT      0
225300991028     C     V6CRTA        ANDNE     WNWDIV
225400991028     C                   CLEAR                   YEURDS
225500991028     C                   MOVEL     V6CRTA        YECDVI
225600991028     C                   Z-ADD     VRTR          YECIMI
225700991028     C                   MOVEL     WNWDIV        YECDVO
225800991028     C                   MOVEL     '3'           YECDCO
225900991028     C                   MOVEL     'H'           YECTAR
226000991028     C                   CALL      'YEURCO'
226100991028     C                   PARM                    YEURDS
226200991028     C                   Z-ADD     YECIMO        VRTR
226300991028     C                   MOVEL     WNWDIV        V6CRTA
226400991028     C                   END
226500991001     C                   MOVEL     WNWDIV        V6CDIV
226600991001     C*
226700991001     C                   ENDSR
226800940308     C*---------------------------------------------------------------*
226900940308     C*   C T R V 0 3    CALCOLO IMPORTI DA ADDEBITARE                *
227000940308     C*---------------------------------------------------------------*
227100940308     C     CTRV03        BEGSR
227200991028     C                   SETOFF                                       121314
227300991028     C                   SETOFF                                       195253
227400990716     C                   SETOFF                                       485154
227500990716     C                   SETOFF                                       5063
227600991028     C                   MOVEL     *BLANKS       $MSG
227700940308     C* CALCOLO SPESE GIACENZA
227800940308     C     *IN33         IFEQ      '1'
227900940309     C* NON HO TROVATO TARIFFA GIACENZA
228000940309     C   57              SETON                                        1950
228100940309     C   57              MOVEL     ERR(34)       $MSG
228200940309     C   57              GOTO      ENDCT3
228300990716     C* CONTROLLO SE IMMESSI DECIMALI PER DIVISA CHE NON LI GESTISCE
228400991129     C     VTSGS         IFGT      0
228500991129     C     VSGD          ORGT      0
228600991129     C     VSGA          ORGT      0
228700991129     C     VSGR          ORGT      0
228800991129     C                   Z-ADD     1             XF
228900990716     C     V6CDIV        LOOKUP    DIV(XF)                                21
229000991028     C* Se divisa inesistente do errore
229100991028     C     *IN21         IFEQ      '0'
229200991028     C                   SETON                                        1914
229300991028     C                   MOVEL     ERR(60)       $MSG
229400991028     C                   GOTO      ENDCT3
229500991028     C                   ENDIF
229600990716     C   21              MOVEL     DCV(XF)       DSCV
229700990716     C  N19§CVFDC        IFNE      'S'
229800990716     C                   MOVE      VTSGS         WDEC03            3 0
229900990716     C     WDEC03        IFNE      0
230000990716     C                   MOVEL     ERR(59)       $MSG
230100990716     C                   SETON                                        1950
230200990716     C                   GOTO      ENDCT3
230300990716     C                   ENDIF
230400990716     C                   MOVE      VSGR          WDEC03            3 0
230500990716     C     WDEC03        IFNE      0
230600990716     C                   MOVEL     ERR(59)       $MSG
230700990716     C                   SETON                                        1948
230800990716     C                   GOTO      ENDCT3
230900990716     C                   ENDIF
231000990716     C                   MOVE      VSGD          WDEC03            3 0
231100990716     C     WDEC03        IFNE      0
231200990716     C                   MOVEL     ERR(59)       $MSG
231300990716     C                   SETON                                        1951
231400990716     C                   GOTO      ENDCT3
231500990716     C                   ENDIF
231600990716     C                   MOVE      VSGA          WDEC03            3 0
231700990716     C     WDEC03        IFNE      0
231800990716     C                   MOVEL     ERR(59)       $MSG
231900990716     C                   SETON                                        1954
232000990716     C                   GOTO      ENDCT3
232100990716     C                   ENDIF
232200991129     C                   ENDIF
232300990716     C                   ENDIF
232400940308     C* CONTROLLO FORMALE   DATA   - DA
232500940930     C                   MOVE      VDSD          G08DAT
232600940930     C                   MOVE      *ZEROS        G08INV
232700940930     C                   MOVEL     *BLANK        G08ERR
232800940930     C                   CALL      'XSRDA8'
232900940930     C                   PARM                    WLBDA8
233000941006     C     G08ERR        COMP      '1'                                    21
233100940930     C  N21              MOVE      G08INV        AMGDSD
233200940308     C  N21AMGDSD        COMP      AMGDSP                               21
233300940308     C  N21AMGDSD        COMP      AMGUDT                             21
233400940308     C   21              SETON                                        5219
233500940309     C   21              MOVEL     ERR(30)       $MSG
233600940308     C* CONTROLLO FORMALE   DATA   -  A
233700940930     C                   MOVE      VDSA          G08DAT
233800940930     C                   MOVE      *ZEROS        G08INV
233900940930     C                   MOVEL     *BLANK        G08ERR
234000940930     C                   CALL      'XSRDA8'
234100940930     C                   PARM                    WLBDA8
234200941006     C     G08ERR        COMP      '1'                                    21
234300940930     C  N21              MOVE      G08INV        AMGDSA
234400940308     C  N21AMGDSA        COMP      AMGDSP                               21
234500940308     C  N21AMGDSA        COMP      AMGUDT                             21
234600940308     C   21              SETON                                        5319
234700940309     C   21              MOVEL     ERR(30)       $MSG
234800940308     C* CONTROLLO DA DATA A DATA CONGRUENZA
234900940325     C  N19              DO
235000940309     C     AMGDSD        COMP      AMGDSA                             21
235100940309     C   21              SETON                                        525319
235200940309     C   21              MOVEL     ERR(31)       $MSG
235300940309     C                   END
235400940308     C*
235500940308     C   19              GOTO      ENDCT3
235600990628     C                   Z-ADD     0             §TOTSP           11 3
235700940308     C                   MOVE      *ZEROS        VTSGS
235800940308     C                   EXSR      TGCCAL
235900991001     C* Riconverto le spese di sosta se necessario
236000991001     C     WGCDIV        IFNE      WNWDIV
236100991001     C                   CLEAR                   YEURDS
236200991001     C                   MOVEL     WGCDIV        YECDVI
236300991001     C                   Z-ADD     VTSGS         YECIMI
236400991001     C                   MOVEL     WNWDIV        YECDVO
236500991001     C                   MOVEL     '3'           YECDCO
236600991001     C                   MOVEL     'H'           YECTAR
236700991001     C                   CALL      'YEURCO'
236800991001     C                   PARM                    YEURDS
236900991001     C                   Z-ADD     YECIMO        VTSGS
237000991001     C                   END
237100940308     C     VTSGS         ADD       VSGR          §TOTSP
237200940308     C     VSGD          ADD       §TOTSP        §TOTSP
237300940308     C     VSGA          ADD       §TOTSP        §TOTSP
237400940308     C* METTO IN RI I GG SOSTA SE LI HANNO CAMBIATI
237500940308     C* SE CONFERMANO LA SECONDA VOLTA NON CONTROLLO
237600940308     C     *INKF         IFEQ      '1'
237700940308     C   61§TOTSP        IFEQ      *ZEROS
237800940308     C                   GOTO      ENDCT3
237900940308     C                   END
238000940308     C     *IN58         CABEQ     '1'           SPESE
238100940308     C                   END
238200940308     C  N19VGGS          IFNE      §GGSAV
238300940308     C                   SETON                                        5819
238400940308     C                   END
238500940308     C*
238600940308     C     SPESE         TAG
238700940308     C     §TOTSP        COMP      *ZEROS                               2121
238800940308     C   21              SETON                                        501961
238900940309     C   21              MOVEL     ERR(32)       $MSG
239000940308     C                   END
239100940308     C* CONTROLLO SE SI DEVE METTERE PROVV. C/ASS
239200991028     C     *IN35         IFEQ      '1'
239300991028     C     VRCA          IFEQ      *ZEROS
239400940308     C                   SETON                                        6319
239500940309     C                   MOVEL     ERR(33)       $MSG
239600940308     C                   END
239700991028     C                   Z-ADD     1             XF                3 0
239800991028     C     V6CRVA        LOOKUP    DIV(XF)                                21
239900991028     C* Se divisa inesistente do errore
240000991028     C     *IN21         IFEQ      '0'
240100991028     C                   SETON                                        1912
240200991028     C                   MOVEL     ERR(60)       $MSG
240300991028     C                   GOTO      ENDCT3
240400991028     C                   ENDIF
240500991028     C   21              MOVEL     DCV(XF)       DSCV
240600991028     C  N19§CVFDC        IFNE      'S'
240700991028     C                   MOVE      VRCA          WDEC03            3 0
240800991028     C     WDEC03        IFNE      0
240900991028     C                   MOVEL     ERR(59)       $MSG
241000991028     C                   SETON                                        121963
241100991028     C                   GOTO      ENDCT3
241200991028     C                   ENDIF
241300991028     C                   ENDIF
241400991028     C                   ENDIF
241500991028     C* CONTROLLO DIVISA RECUPERO TRASPORTO
241600991028     C     *IN36         IFEQ      '1'
241700991028     C                   Z-ADD     1             XF                3 0
241800991028     C     V6CRTA        LOOKUP    DIV(XF)                                21
241900991028     C* Se divisa inesistente do errore
242000991028     C     *IN21         IFEQ      '0'
242100991028     C                   SETON                                        1913
242200991028     C                   MOVEL     ERR(60)       $MSG
242300991028     C                   GOTO      ENDCT3
242400991028     C                   ENDIF
242500991028     C   21              MOVEL     DCV(XF)       DSCV
242600991028     C  N19§CVFDC        IFNE      'S'
242700991028     C                   MOVE      VRTR          WDEC03            3 0
242800991028     C     WDEC03        IFNE      0
242900991028     C                   MOVEL     ERR(59)       $MSG
243000991028     C                   SETON                                        1319
243100991028     C                   GOTO      ENDCT3
243200991028     C                   ENDIF
243300991028     C                   ENDIF
243400991028     C                   ENDIF
243500991028     C* Se controllo se le tre divise immesse sono congruenti
243600991028     C* fra loro
243700991028     C     *IN33         IFEQ      '1'
243800991028     C     *IN35         ANDEQ     '1'
243900991028     C     V6CDIV        ANDNE     V6CRVA
244000991028     C     *IN33         OREQ      '1'
244100991028     C     *IN36         ANDEQ     '1'
244200991028     C     V6CDIV        ANDNE     V6CRTA
244300991028     C     *IN35         OREQ      '1'
244400991028     C     *IN36         ANDEQ     '1'
244500991028     C     V6CRVA        ANDNE     V6CRTA
244600991028     C                   SETON                                        121314
244700991028     C                   SETON                                        19
244800991028     C                   MOVEL     ERR(61)       $MSG
244900991028     C                   END
245000991028     C*
245100940308     C     ENDCT3        ENDSR
245200940308     C/SPACE 3
245300940308     C*---------------------------------------------------------------*
245400940308     C*   T G C C A L    CALCOLO IMPORTI DA ADDEBITARE X SOSTA        *
245500940308     C*---------------------------------------------------------------*
245600940308     C     TGCCAL        BEGSR
245700050405     c                   eval      spearr = 1
245800940930      * GIORNI SOSTA DA CALCOLARE
245900940930     C     VGGS          IFEQ      *ZEROS
246000940930     C* CALCOLO GIORNI DI SOSTA  CON LE DATE IMPOSTATE A VIDEO
246100940325     C     VDSD          IFNE      *ZEROS
246200940325     C     VDSA          ANDNE     *ZEROS
246300940930     C                   MOVE      VDSD          G08DAT
246400940930     C                   MOVEL     *BLANK        G08ERR
246500940930     C                   CALL      'XSRDA8'
246600940930     C                   PARM                    WLBDA8
246700940930     C                   Z-ADD     G08TGI        §GGDA
246800940308     C*
246900940930     C                   MOVE      VDSA          G08DAT
247000940930     C                   MOVEL     *BLANK        G08ERR
247100940930     C                   CALL      'XSRDA8'
247200940930     C                   PARM                    WLBDA8
247300941006     C                   Z-ADD     G08TGI        §GGA
247400940308     C*
247500940308     C     §GGA          SUB       §GGDA         §GGSOS
247600940308     C                   ADD       1             §GGSOS
247700940308     C* SOMMO GG RIAPERTURE
247800940308     C                   ADD       §GGRIA        §GGSOS
247900940308     C     §GGSOS        IFGT      VSGF
248000940308     C                   Z-ADD     §GGSOS        §GGPAG
248100940308     C                   ELSE
248200940308     C                   Z-ADD     *ZEROS        §GGPAG
248300940308     C                   END
248400940308     C                   Z-ADD     §GGPAG        VGGS
248500940308     C* MI SALVO I GG DA PAGARE CALCOLATI X SEGNALARE SE A VIDEO LI HANNO
248600940308     C* MODIFICATI
248700940308     C                   Z-ADD     §GGPAG        §GGSAV
248800940325     C                   END
248900940930     C                   ELSE
249000940308     C*                    ----
249100940308     C     VGGS          IFGT      VSGF
249200940308     C                   Z-ADD     VGGS          §GGPAG
249300940308     C                   ELSE
249400940308     C                   Z-ADD     *ZEROS        §GGPAG
249500940930     C                   END
249600940930     C                   END
249700940930      *
249800940308     C                   Z-ADD     0             XP                1 0
249900990628     C                   MOVE      *ZEROS        SGG
250000940308     C                   MOVE      *ZEROS        STU
250100940308     C                   MOVE      *ZEROS        STT
250200940308     C*
250300940930     C     §GGPAG        IFGT      0
250400940308     C                   Z-ADD     §GGPAG        §GGPAR
250500940308     C                   DO        3             X
250600940308     C     §GGPAG        IFGT      TGS(X)
250700940308     C     §GGPAG        SUB       TGS(X)        §GGPAR
250800940308     C     X             IFEQ      1
250900940308     C                   Z-ADD     TGS(X)        SGG(X)
251000940308     C                   ELSE
251100940308     C     TGS(X)        SUB       TGS(XP)       SGG(X)
251200940308     C                   END
251300940308     C                   Z-ADD     TTS(X)        STU(X)
251400940308     C                   ELSE
251500940308     C                   Z-ADD     §GGPAR        SGG(X)
251600940308     C                   Z-ADD     TTS(X)        STU(X)
251700940308     C                   Z-ADD     0             §GGPAG
251800940308     C                   Z-ADD     0             §GGPAR
251900940308     C                   END
252000940308     C                   ADD       1             XP
252100940308     C                   END
252200940930     C                   ENDIF
252300060113      *  TARIFFA SENZA FRANCHIGIA
252400060113     C*  CALCOLO PESO TASSABILE
252500060113     C     TGCRPV        MULT      ARBVLF        §PESVO           13 3
252600060113     C     §PESVO        MULT      100           §PESVO
252700060113     C     §PESVO        IFGT      ARBPKF
252800060113     C     §PESVO        DIV       100           §PES§            13 3
252900060113     C                   ELSE
253000060113     C     ARBPKF        DIV       100           §PES§
253100060113     C                   END
253200060113     C     §PES§         ADD       0,99          §PESCA            6 0
253300060113     C* CALCOLO TOTALE SPESE SOSTA
253400060113     C     SGG           MULT      STU           STT
253500060113     C     STT           MULT      §PESCA        STT
253600060113     C                   XFOOT     STT           VTSGS
253700060113     C* MINIMO TASSABILE SPESE SOSTA  SE SUPERO  GG DI FRANCHIGIA
253800060113     C     VTSGS         IFLT      TGCSTM
253900060113     C     VGGS          ANDGT     VSGF
254000060113     C                   Z-ADD     TGCSTM        VTSGS
254100060113     C                   END
254200940930      *  TARIFFA CON FRANCHIGIA DA APPLICARE A TUTTE LE SPESE
254300940930     C     TGCFAF        IFEQ      '2'
254400940930     C     VGGS          ANDLE     VSGF
254500060113     C     sgfaf$        ANDLE     *blank
254600940930     C                   MOVE      *ZEROS        VTSGS
254700940930     C                   MOVE      *ZEROS        VSGR
254800060113     C                   MOVE      '1'           SGR$
254900940930     C                   MOVE      *ZEROS        VSGD
255000940930     C                   MOVE      *ZEROS        VSGA
255100940930     C                   ELSE
255200060109      *TARIFFA CON FRANCHIGIA DA APPLICARE A TUTTE LE SPESE tranne riconsegna
255300060109     C     TGCFAF        IFEQ      '3'
255400060109     C     VGGS          ANDLE     VSGF
255500060113     C     sgfaf$        ANDLE     *blank
255600060109     C                   MOVE      *ZEROS        VTSGS
256300060109     C                   MOVE      *ZEROS        VSGD
256400060109     C                   MOVE      *ZEROS        VSGA
256500060109     C                   ELSE
256600950120      *  TARIFFA CON FRANCHIGIA DA APPLICARE SOLO ALLE SPESE SOSTA
256700950120     C     TGCFAF        IFEQ      '1'
256800060113     C     sgfaf$        ANDLE     *blank
256900950120     C     VGGS          ANDLE     VSGF
257000950120     C                   MOVE      *ZEROS        VTSGS
257800950120     C                   ENDIF
257900060109     C                   ENDIF
258000940930     C                   ENDIF
258100060113     C                   move      '1'           sgfaf$
258200940308     C     ENDCAL        ENDSR
258300940308     C*---------------------------------------------------------------*
258400940308     C*   P G A R B K  RICH. PGM VARIAZ. BOLLA   C/ASS O COD.BOLLA    *
258500940308     C*---------------------------------------------------------------*
258600940308     C     PGARBK        BEGSR
258700940308     C     §§KCVB        IFNE      *BLANK
258800941215     C                   MOVEL     ' '           D48TLA
258900941215     C                   MOVEL     FILARR        D48FGS
259000941214     C                   MOVE      ARBAAS        D48AAS
259100941214     C                   MOVE      ARBLNP        D48LNP
259200941214     C                   MOVE      ARBNRS        D48NRS
259300941214     C                   MOVE      ARBNSP        D48NSP
259400941214     C                   MOVEL     ARBCBO        D48CBO
259500941214     C                   MOVEL     §§KCVB        D48CVB
259600941214     C                   MOVEL     'K'           D48TRC
259700941214     C                   MOVEL     'S'           D48FFR
259800941214     C                   MOVEL     ' '           D48ERR
259900941214     C                   MOVEL     *BLANK        D48MSG
260000941222     C                   MOVEL     'A'           D48TBO
260100941215     C     §BKCAS        IFGT      *ZEROS
260200941219     C                   MOVE      VTIC          §BKTIC
260300940308     C                   ELSE
260400940308     C                   MOVE      *BLANK        §BKTIC
260500940308     C                   END
260600040301     c                   z-add     arbQFT        §BTQFT
260700040301     c                   z-add     arbVMD        §BTVMD
260800040301     c                   MOVE      arbVAD        §BTVAD
260900040301     c                   z-add     arbIAS        §BTIAS
261000040301     c                   MOVE      arbVAS        §BTVAS
261100941219     C                   MOVEL     AR9GCA        §BKGCA
261200941219     C                   MOVEL     ARBGGA        §BKGGA
261300941219     C                   MOVEL     ARBGMA        §BKGMA
261400941219     C                   MOVEL     ARBGVA        §BKGVA
261500960205     C*§ ABBLENCO 2°BYTE PARTIC. VARIE
261600960205     C                   MOVE      ' '           §BKGVA
261700941214     C                   MOVEL     DSLR48        KPJBU
261800941214     C                   CALL      'FNLR48R'
261900940308     C                   PARM                    KPJBA
262000940308     C                   PARM                    DSARBD
262100940308     C                   PARM                    DSARBK
262200991001     C                   PARM                    DARBT
262300940308     C                   PARM                    DSARBG
262400030715     C                   PARM                    trul90ds
262500941214     C                   MOVEL     KPJBU         DSLR48
262600941214     C     D48ERR        IFEQ      *BLANK
262700941214     C                   CLEAR                   DSLR48
262800940308     C                   END
262900940308     C                   END
263000940308     C                   ENDSR
263100940308     C*---------------------------------------------------------------*
263200940308     C*   P G A R B I  RICH. PGM VARIAZ. BOLLA   INDIR. DESTINATARIO  *
263300940308     C*---------------------------------------------------------------*
263400940308     C     PGARBI        BEGSR
263500941215     C                   MOVEL     ' '           D48TLA
263600941215     C                   MOVEL     FILARR        D48FGS
263700941215     C                   MOVE      ARBAAS        D48AAS
263800941215     C                   MOVE      ARBLNP        D48LNP
263900941215     C                   MOVE      ARBNRS        D48NRS
264000941215     C                   MOVE      ARBNSP        D48NSP
264100941215     C                   MOVEL     ARBCBO        D48CBO
264500140729     C                   MOVEL     'I0'          D48CVB
264600941215     C                   MOVEL     'D'           D48TRC
264700941215     C                   MOVEL     ' '           D48ERR
264800941215     C                   MOVEL     *BLANK        D48MSG
264900941222     C                   MOVEL     'A'           D48TBO
265000941215     C*
265100060317     c                   if        flgdis = 'F'
265200060317     C                   MOVEL     'S'           §BDFFD
265300140729     C                   MOVEL     'I7'          D48CVB
265400060317     c                   else
265500060317     c                   move      arbffd        §bdffd
265600060317     c                   endif
265700940308     C                   MOVEL     ARBCPI        §BDCPI
265800941215     C                   MOVE      ARBPKF        §BDPKF
265900941215     C                   MOVE      ARBVLF        §BDVLF
266000940308     C                   MOVE      ARBFIN        §BDFIN
266100940308     C                   MOVEL     VRSD          §BDRSD
266200941223     C                   MOVEL     VRD2          §BDRD2
266300940308     C                   MOVEL     VIND          §BDIND
266400940308     C                   MOVEL     VCAD          §BDCAD
266500940308     C                   MOVEL     VLOD          §BDLOD
266600940308     C                   MOVEL     VPRD          §BDPRD
266700950206     C                   MOVEL     VNZD          §BDNZD
266800060314     C                   MOVEL     Vref          §BDref
266900060314     C                   MOVEL     Vteld         §BDtel
267000941215     C                   MOVE      ARBFIN        §BDFIN
267100040301     c                   z-add     arbQFT        §BTQFT
267200040301     c                   z-add     arbVMD        §BTVMD
267300040301     c                   MOVE      arbVAD        §BTVAD
267400040301     c                   z-add     arbIAS        §BTIAS
267500040301     c                   MOVE      arbVAS        §BTVAS
267600941215     C*
267700941215     C                   MOVEL     DSLR48        KPJBU
267800941215     C                   CALL      'FNLR48R'
267900940308     C                   PARM                    KPJBA
268000940308     C                   PARM                    DSARBD
268100940308     C                   PARM                    DSARBK
268200991001     C                   PARM                    DARBT
268300940308     C                   PARM                    DSARBG
268400030715     C                   PARM                    trul90ds
268500941215     C                   MOVEL     KPJBU         DSLR48
268600941215     C     D48ERR        IFEQ      *BLANK
268700941215     C                   CLEAR                   DSLR48
268800941215     C                   END
268900940308     C                   ENDSR
269000940308     C*---------------------------------------------------------------*
269100940308     C*   P G A R B T  RICH. PGM VARIAZ. BOLLA  TASSAZ. 1° BOLLA      *
269200940308     C*---------------------------------------------------------------*
269300940308     C     PGARBT        BEGSR
269400941215     C                   MOVEL     ' '           D48TLA
269500941215     C                   MOVEL     FILARR        D48FGS
269600941215     C                   MOVE      ARBAAS        D48AAS
269700941215     C                   MOVE      ARBLNP        D48LNP
269800941215     C                   MOVE      ARBNRS        D48NRS
269900941215     C                   MOVE      ARBNSP        D48NSP
270000941215     C                   MOVEL     ARBCBO        D48CBO
270100941215     C                   MOVEL     §§TCVB        D48CVB
270200941215     C                   MOVEL     'T'           D48TRC
270300941215     C                   MOVEL     'S'           D48FFR
270400941215     C                   MOVEL     ' '           D48ERR
270500941215     C                   MOVEL     *BLANK        D48MSG
270600941222     C                   MOVEL     'A'           D48TBO
270700941215     C*
270800940308     C     §BTKSC        IFEQ      0
270900940308     C                   MOVE      ARBKSC        §BTKSC
271000991203     C     AR6KSC        IFNE      0
271100991001     C     AR6TRC        ANDEQ     '1'
271200991001     C                   MOVE      AR6KSC        §BTKSC
271300940308     C                   END
271400991001     C                   END
271500991001     C                   MOVE      '1'           §BTTRC
271600991001     C                   MOVE      ARBCTR        §BTCTR
271700941006     C                   MOVE      AR6POR        §BTPOR
271800991001     C                   MOVE      AR6DIV        §BTDIV
271900040301     c                   z-add     arbQFT        §BTQFT
272000040301     c                   z-add     arbVMD        §BTVMD
272100040301     c                   MOVE      arbVAD        §BTVAD
272200040301     c                   z-add     arbIAS        §BTIAS
272300040301     c                   MOVE      arbVAS        §BTVAS
272400000105     C* SE IL CODICE VARIAZIONE E' T0 (NON ESISTE LA BOLLA)
272500000105     C* IMPOSTO LA DIVISA DALLA GIACENZA
272600000105     C     §CDVAR        IFEQ      'T0'
272700000105     C     AR6DIV        ANDEQ     *BLANKS
272800000105     C                   MOVE      V6CDIV        §BTDIV
272900000105     C                   END
273000940308     C                   MOVE      *ZEROS        §BTIMV
273100991001     C                   MOVE      AR6CEI        §BTCEI
273200941216     C* SE HO SPESE GIACENZA AGGIORNO LA VARIA I
273300941216     C                   MOVEL     §$2VRI        §SIGLA            1
273400941216     C                   MOVEL     VSV           WSV
273500941216      * CERCO L'INDICE SU CUI LAVORARE
273600941216     C                   EXSR      CHKVAR
273700950726     C     §TOTSP        IFGT      0
273800941216     C                   Z-ADD     §TOTSP        VVA(XS)
273900941216     C                   MOVEL     §$2VRI        VSV(XS)
274000950726     C                   ELSE
274100950726     C     VSV(XS)       IFEQ      §$2VRI
274200950726     C                   MOVEL     ' '           VSV(XS)
274300950726     C                   Z-ADD     0             VVA(XS)
274400950726     C                   END
274500950726     C                   END
274600991001     C* Tolgo varia Iva ...
274700991001     C                   Z-ADD     1             XS
274800991001     C     §$2BOL        LOOKUP    VSV(XS)                                20
274900991001     C     *IN20         IFEQ      '1'
275000991001     C                   MOVEL     ' '           VSV(XS)
275100991001     C                   Z-ADD     0             VVA(XS)
275200991001     C                   END
275300991001     C* ... e bollo sono state impostate
275400991001     C                   Z-ADD     1             XS
275500991001     C     §$2IVA        LOOKUP    VSV(XS)                                20
275600991001     C     *IN20         IFEQ      '1'
275700991001     C                   MOVEL     ' '           VSV(XS)
275800991001     C                   Z-ADD     0             VVA(XS)
275900991001     C                   END
276000941215     C*
276100941215     C                   MOVEL     DSLR48        KPJBU
276200941215     C                   CALL      'FNLR48R'
276300940308     C                   PARM                    KPJBA
276400940308     C                   PARM                    DSARBD
276500940308     C                   PARM                    DSARBK
276600991001     C                   PARM                    DARBT
276700940308     C                   PARM                    DSARBG
276800030715     C                   PARM                    trul90ds
276900941215     C*
277000941215     C                   MOVEL     KPJBU         DSLR48
277100941215     C     D48ERR        IFEQ      *BLANK
277200941215     C                   CLEAR                   DSLR48
277300941215     C                   END
277400940308     C                   ENDSR
277500940308     C*---------------------------------------------------------------*
277600940308     C*   P G A R 2 T  RICH. PGM VARIAZ. BOLLA  TASSAZ. 2° BOLLA      *
277700940308     C*---------------------------------------------------------------*
277800940308     C     PGAR2T        BEGSR
277900941215     C                   MOVEL     ' '           D48TLA
278000941215     C                   MOVEL     FILARR        D48FGS
278100941215     C                   MOVE      ARBAAS        D48AAS
278200941215     C                   MOVE      ARBLNP        D48LNP
278300941215     C                   MOVE      ARBNRS        D48NRS
278400941215     C                   MOVE      ARBNSP        D48NSP
278500941215     C                   MOVEL     ARBCBO        D48CBO
278600991001     C                   MOVEL     §§TCVB        D48CVB
278700991001     C                   MOVEL     '2'           D48TRC
278800941215     C                   MOVEL     'S'           D48FFR
278900941215     C                   MOVEL     ' '           D48ERR
279000941215     C                   MOVEL     *BLANK        D48MSG
279100941222     C                   MOVEL     'A'           D48TBO
279200991001     C                   MOVE      AR6CTR        §BTCTR
279300991001     C                   MOVE      AR6POR        §BTPOR
279400991001     C                   MOVE      AR6DIV        §BTDIV
279500040301     c                   z-add     arbQFT        §BTQFT
279600040301     c                   z-add     arbVMD        §BTVMD
279700040301     c                   MOVE      arbVAD        §BTVAD
279800040301     c                   z-add     arbIAS        §BTIAS
279900040301     c                   MOVE      arbVAS        §BTVAS
280000000105     C* SE IL CODICE VARIAZIONE E' T0 (NON ESISTE LA BOLLA)
280100000105     C* IMPOSTO LA DIVISA DALLA GIACENZA
280200000105     C     §CDVAR        IFEQ      'T0'
280300000105     C     AR6DIV        ANDEQ     *BLANKS
280400000105     C                   MOVE      V6CDIV        §BTDIV
280500000105     C                   END
280600991001     C                   MOVE      *ZEROS        §BTIMV
280700991001     C                   MOVE      AR6CEI        §BTCEI
280800941215     C*
280900991001     C     §BTKSC        IFEQ      0
281000991001     C                   MOVE      AR6KSC        §BTKSC
281100940308     C                   END
281200991001     C                   MOVE      '2'           §BTTRC
281300991001     C* Tolgo varia Iva ...
281400991001     C                   Z-ADD     1             XS
281500991001     C     §$2BOL        LOOKUP    VSV(XS)                                20
281600991001     C     *IN20         IFEQ      '1'
281700991001     C                   MOVEL     ' '           VSV(XS)
281800991001     C                   Z-ADD     0             VVA(XS)
281900991001     C                   END
282000991001     C* ... e bollo sono state impostate
282100991001     C                   Z-ADD     1             XS
282200991001     C     §$2IVA        LOOKUP    VSV(XS)                                20
282300991001     C     *IN20         IFEQ      '1'
282400991001     C                   MOVEL     ' '           VSV(XS)
282500991001     C                   Z-ADD     0             VVA(XS)
282600991001     C                   END
282700941215     C*
282800941215     C                   MOVEL     DSLR48        KPJBU
282900941215     C                   CALL      'FNLR48R'
283000940308     C                   PARM                    KPJBA
283100940308     C                   PARM                    DSARBD
283200940308     C                   PARM                    DSARBK
283300991001     C                   PARM                    DARBT
283400940308     C                   PARM                    DSARBG
283500030715     C                   PARM                    trul90ds
283600941215     C*
283700021009     C                   MOVEL     KPJBU         DSLR48
283800941215     C     D48ERR        IFEQ      *BLANK
283900941215     C                   CLEAR                   DSLR48
284000941215     C                   END
284100940308     C                   ENDSR
284200940309     C*?$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
284300940308     C*?    R O U T I N E     INIZIO
284400940309     C*?$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
284500940308     C*---------------------------------------------------------------*
284600940308     C*   S R G I A C  - RICHIAMATO DA GIACENZE                       *
284700940308     C*---------------------------------------------------------------*
284800940308     C     SRGIAC        BEGSR
284900050316     C     KGIA          CHAIN(e)  tigcp02l                           40
285000050316     c                   if        %error
285100070521     C     KGIA          CHAIN(n)  tigcp02l
285200050316     c                   exsr      geslck
285300050318     C     KGIA          CHAIN(e)  tigcp02l                           40
285400050316     c                   endif
285500940308     C   40              MOVEL     '1'           $FINE
285600940308     C   40              GOTO      ENDGIA
285700940308     C*
285800050222     C     GCPriap       COMP      'R'                                    42
285900941003     C* PRENDO DATI DELLA BOLLA  - FLARB01L
286000050222     C                   MOVE      GCPAASa       KAAS
286100050222     C                   MOVE      GCPLNPa       KLNP
286200050222     C                   MOVE      GCPNRSa       KNRS
286300050222     C                   MOVE      GCPNSPa       KNSP
286400940308     C                   EXSR      CHKARB
286500940308     C     $ERR          IFEQ      '1'
286600940308     C                   MOVEL     '1'           $FINE
286700940308     C                   GOTO      ENDGIA
286800940308     C                   END
286900940308     C     ENDGIA        ENDSR
287000940308     C/SPACE 3
287100940308     C*---------------------------------------------------------------*
287200940308     C*   S R D I R O  - RICHIAMATO DA DIROTTAM/CAMBI PORTO           *
287300940308     C*---------------------------------------------------------------*
287400940308     C     SRDIRO        BEGSR
287500940928     C                   MOVE      ANNO          VAAS
287600940308     C                   MOVE      *ZEROS        VLNP
287700940308     C                   MOVE      *ZEROS        VNRS
287800940308     C                   MOVE      *ZEROS        VNSP
287900950104     C                   SETOFF                                       94
288000940928      * NR. SPEDIZIONE PASSATA : NON EMETTO VIDEATA RICHIESTA NR.SPED.
288100941216     C     D82NSP        IFNE      *BLANK
288200941216     C                   MOVE      D82AAS        VAAS
288300941216     C                   MOVE      D82LNP        VLNP
288400941216     C                   MOVE      D82NRS        VNRS
288500941216     C                   MOVE      D82NSP        VNSP
288600141113     c                   if        d82dis <> 'D'
288700950104     C                   SETON                                        94
288800160504     c                   exsr      chkevb
288900141113     c                   endif
289000940928     C                   GOTO      NOFD01
289100940928     C                   ENDIF
289200940308     C*
289300940308     C     FG2010        TAG
289400141006     c                   if        d82nodsp <> *blank
289500141007     c                   if        *in43
289600141006     c                   movel     $msg          d20errmo
289700141006     c                   movel     'E'           d20erro
289800141006     c                   return
289900141006     c                   endif
290000141006     c                   else
290100940915     C                   EXFMT     LG20D01
290200141006     c                   endif
290300940308     C                   MOVEL     *BLANK        $MSG
290400940308     C                   SETOFF                                       434419
290500950131     C*
290600950131     C                   MOVEL     VAAS          WAA2              2 0
290700950131     C     WAA2          IFEQ      0
290800950131     C     VAAS          IFGT      60
290900950131     C                   ADD       1900          VAAS
291000950131     C                   ELSE
291100950131     C                   ADD       2000          VAAS
291200950131     C                   END
291300950131     C                   END
291400940308     C*
291500940308     C*  OPERAZIONI DA EFFETTUARE SE FILIALE DI ARRIVO DI PRIMO LIVELLO
291600940308     C     *IN01         IFEQ      '1'
291700940308     C*  CONTROLLO SE HO RICHIESTO MODIFICA FILIALE
291800940308     C     *INKS         IFEQ      '1'
291900940308     C                   SETON                                        02
292000940308     C                   GOTO      FG2010
292100940308     C                   END
292200940308     C*
292300940308     C*  CONTROLLO SE HO MODIFICATO LA FILIALE
292400940308     C     *IN03         IFEQ      '1'
292500940308     C* AGGANCIO LA £6 E RECUPERO LE LINEE A LORO ASSOCIATE
292600940308     C*
292800020509     C                   CLEAR                   TR06DS
292900020509     C                   MOVEL     '£6'          D06COD
293000020509     C                   MOVEL     filarr        D06KEY
293100020509     C                   MOVEL     TR06DS        KPJBU
293200020509     C                   CALL      'TRUL06R'
293300020509     C                   PARM                    KPJBA
293400020509     C                   MOVEL     KPJBU         TR06DS
293500020509     C                   MOVEA     LIN           l£6
293700020226     C                   END
293800020226     C                   END
293900940308     C*  INTERROG. BOLLE
294000940308     C   KG              DO
294200070622     C                   CLEAR                   param1
294300070622     C                   MOVEL     '2'           Pa1FLG
294400070622     C                   MOVEL     param1        KPJBU
294500941006     C                   CALL      'FNLR36R'
294600940308     C                   PARM                    KPJBA
294700070622     C                   MOVEL     KPJBU         param1
294800070622     C     Pa1AAS        IFNE      *ZEROS
294900070622     C                   MOVEL     Pa1AAS        VAAS
295000070622     C                   MOVEL     Pa1LNP        VLNP
295100070622     C                   MOVEL     Pa1NRS        VNRS
295200070622     C                   MOVEL     Pa1NSP        VNSP
295300940308     C                   END
295500940308     C                   GOTO      FG2010
295600940308     C                   END
295700940308     C   KC              MOVEL     '1'           $FINE
295800940308     C   KC              GOTO      ENDDIR
295900940928     C     NOFD01        TAG
296000941216     C                   MOVE      VAAS          KAAS
296100941216     C                   MOVE      VLNP          KLNP
296200941216     C                   MOVE      VNRS          KNRS
296300941216     C                   MOVE      VNSP          KNSP
296400940308     C                   EXSR      CHKARB
296500940308     C     $ERR          IFEQ      '1'
296600940308     C                   SETON                                        19
296700940308     C                   MOVEL     ERR(1)        $MSG
296800940308     C                   GOTO      FG2010
296900940308     C                   END
297000000120     C     $ERR          IFEQ      '2'
297100000120     C                   SETON                                        19
297200000120     C                   MOVEL     ERR(62)       $MSG
297300000120     C                   GOTO      FG2010
297400000120     C                   END
297500000421      *CONTROLLO PER FILIALE POSTE  ERRORE BOLLE NON TRATTABILI
297600000417     C     $ERR          IFEQ      '3'
297700000417     C                   SETON                                        19
297800000417     C                   MOVEL     ERR(63)       $MSG
297900000417     C                   GOTO      FG2010
298000000417     C                   END
298100940928     C* E' UNA FIGLIA: NO DIROTT./CAMBIO PORTO
298200940928     C     KBOL          CHAIN     FNLBL01L                           43
298300940308     C     *IN43         IFEQ      '0'
298400940308     C     LBLLAN        COMP      LBLLAP                                 43
298500940308     C   43              MOVEL     ERR(2)        $MSG
298600940308     C   43              SETON                                        19
298700940308     C   19              GOTO      FG2010
298800940308     C                   ELSE
298900940308     C                   SETOFF                                       43
299000940308     C                   END
299100940308     C* E' UNA MAMMA : ERRORE
299200940928     C     KBOL          SETLL     FNLBL02L                               43
299300940308     C   43              SETON                                        19
299400940308     C   19              MOVEL     ERR(2)        $MSG
299500940308     C   19              GOTO      FG2010
299600940308     C* BOLLA SU ALTRA DISTINTA
299700940308     C                   Z-ADD     0             PROGR
299800940308     C     ARBNDC        COMP      *ZEROS                             4343
299900940308     C   43              SETON                                        19
300000940308     C   19              MOVEL     ERR(3)        $MSG
300100940308     C   19              GOTO      FG2010
300200940308     C* BOLLA  GIACENTE (O.K. SE GIAC. ANNULLATA)
300300050222     C     KBOLG         CHAIN     tigcp21L                           21
300400940308     C  N21GCPATB        COMP      '*'                                4343
300500990716     C   43
300600050418     CANN21GCPFAS        COMP      40                                   4343
300700990716     C   43              SETON                                        19
300800940308     C   19              MOVEL     ERR(4)        $MSG
300900940308     C   19              GOTO      FG2010
301000940308     C* LINEA ARRIVO NON IN GESTIONE
301100940308     C     ARBLNA        LOOKUP    L£6                                    21
301200940308     C  N21              SETON                                        1944
301300940308     C   19              MOVEL     ERR(5)        $MSG
301400940308     C   19              GOTO      FG2010
301500940928     C* BOLLA GIA CONSEGNATA
301600940928     C     ARBDCM        COMP      *ZEROS                             4343
301700940928     C   43              SETON                                        19
301800940928     C   19              MOVEL     ERR(6)        $MSG
301900940928     C   19              GOTO      FG2010
302000940308     C* BOLLA CONSEGNATA PARZIALMENTE
302100940308     C     ARBDCP        COMP      *ZEROS                             4343
302200940308     C   43              MOVEL     ERR(6)        $MSG
302300940308     C   43              SETON                                        19
302400940308     C   19              GOTO      FG2010
302500940308     C* BOLLA BLOCCATA
302600940308     C     ARBFBC        COMP      *BLANK                             4343
302700940308     C   43              MOVEL     ERR(7)        $MSG
302800940308     C   43              SETON                                        19
302900940308     C   19              GOTO      FG2010
303000110909     C* BOLLA con esiti PDA
303100110909     C                   EXSR      SRCTRPDA
303200110909     C   43              MOVEL     ERR(67)       $MSG
303300110909     C   43              SETON                                        19
303400110909     C   19              GOTO      FG2010
303500141103     C* BOLLA con disposizioni di consegna/dirottamento se dirottamento errore
303600141103      * esegue solo se richiamato da menu
303700141103     C                   IF        $giac = '0' and d82nsp = *blank
303800141103     C                             and %parms = 1
303900141103     c                   clear                   fnlry09ds2
304000141103     c                   eval      ilry09tdis= ' '
304100141103     c                   eval      ilry09tch = 'C'
304200141103     c                   eval      ilry09aas = arbaas
304300141103     c                   eval      ilry09lnp = arblnp
304400141103     c                   eval      ilry09nrs = arbnrs
304500141103     c                   eval      ilry09nsp = arbnsp
304600141103     c                   eval      ilry09fgs = arblna
304700141103     c                   movel     fnlry09ds2    kpjbu
304800141103     c                   call      'FNLRY09R'
304900141103     c                   parm                    kpjba
305000141103     c                   movel     kpjbu         fnlry09ds2
305100141103     c                   if        olry09err = *blank
305200141103     c                   select
305300141103     c                   when      olry09esdi = '1'
305400141103     C                   MOVEL     ERR(68)       $MSG
305500141103     C                   SETON                                        1943
305600141103     C   19              GOTO      FG2010
305700141103      *  altro
305800141103     c                   when      olry09esal = '1'
305900141103     c                   eval      ilry09tdis= 'E'
306000141103     c                   eval      ilry09tch = 'E'
306100141103     c                   movel     fnlry09ds2    kpjbu
306200141103     c                   call      'FNLRY09R'
306300141103     c                   parm                    kpjba
306400141103     c                   movel     kpjbu         fnlry09ds2
306500141103     c                   if        olry09esal = '1'
306600151204     C                   MOVEL     ERR(69)       $MSG
306700141105     C                   SETON                                        1943
306800141105     C   19              GOTO      FG2010
306900141103     c                   endif
307000141103     c                   endsl
307100141103     c                   endif
307200141103     C                   ENDIF
307300940308      * TUTTO OK
307400950104     C* SE DISPOSIZIONI PASSATE DA PGM CHIAMANTE
307500950104     C     D82NSP        IFNE      *BLANK
307600950104     C     D82DIS        ANDNE     *BLANK
307700950104     C                   MOVEL     D82DIS        VFRI
307800950104     C                   ELSE
307900940308     C                   MOVEL     *BLANK        VFRI
308000950104     C                   END
308100940308     C                   MOVEL     *BLANK        VNPC
308200940308     C                   MOVEL     'N'           VVCS
308300940308     C                   MOVE      *ZEROS        VNCA
308400940308     C                   MOVEL     'N'           VAPC
308500940308     C                   MOVEL     *BLANK        VPPC
308600940308     C                   MOVEL     *BLANK        DFRI
308700940308     C                   MOVEL     *BLANK        DNPC
308800940308     C                   MOVEL     *BLANK        DPPC
308900960327     C                   MOVEL     'N'           V4CRCS
309000960327     C                   SETOFF                                       07
309100940308     C     ENDDIR        ENDSR
309200940308     C/SPACE 3
309300940308     C*---------------------------------------------------------------*
309400110909     C*   srctrpda - CONTROLLO se esiti pda                           *
309500940308     C*---------------------------------------------------------------*
309600110909     C     srctrpda      BEGSR
309700110909     c*
309800110909     c     kbol          chain     fipdc02t
309900110909     c                   if        not %found(fipdc02t)
310000110909     c                   leavesr
310100110909     c                   end
310200110909     c* se trova la bolla sul file di trasmissione dati al pda verifico
310300110909     c* se confermato annullamento
310400110909     c                   if        pdcatb = ' '
310500110909     c                   seton                                        43
310600110909     c                   leavesr
310700110909     c                   end
310800110909     c* verifico di non avere ricevuto esiti
310900110909     c                   eval      pcttrd = 'CET'
311000110909     c     kpct          setgt     fipct01l
311100110909     c     kpct          readpe    fipct01l
311200110909     c                   if        not %eof(fipct01l) and pdcndc = pctndc
311300110909     c                   seton                                        43
311400110909     c                   leavesr
311500110909     c                   end
311600110909     c* verifico se ricevuta la conferma all'annullamento altrimenti errore
311700110909     c                   eval      pcttrd = 'COK'
311800110909     c     kpct          setgt     fipct01l
311900111212     c                   do        *hival
312000110909     c     kpct          readpe    fipct01l
312100111212     c                   if        %eof(fipct01l)
312200111212     c                   leave
312300111212     c                   end
312400111212     c                   if        pdcndc = pctndc
312500110909     c                   eval      fipctcokds = pctdati
312600110909     c                   if        §PCTIDROW < PDCIDROW
312700110909     c                   seton                                        43
312800110909     c                   leavesr
312900110909     c                   end
313000110909     c                   end
313100111212     c                   enddo
313200110909     c*
313300110909     C     END           ENDSR
313400110909     C/SPACE 3
313500110909     C*---------------------------------------------------------------*
313600110909     C*   C H K A R B  - CONTROLLO /LETTURA FNARB01L                  *
313700110909     C*---------------------------------------------------------------*
313800110909     C     CHKARB        BEGSR
313900940308     C                   MOVE      *ZEROS        §KSC
314000940308     C                   MOVE      *ZEROS        §QUATR
314100940308     C                   MOVEL     '0'           $ERR
314200941219     C     KBOL          CHAIN(N)  FNARB01L                           43
314300940928     C  N43ARBDCM        COMP      *ZEROS                             43
314400940308     C   43              MOVEL     '1'           $ERR
314500940308      *
314600991001     C     $ERR          IFEQ      '0'
314700941010     C                   MOVE      ARBMGS        AMGDSP
314800941010     C                   MOVEL     ARBAAS        AMGDSP
314900941012     C                   MOVE      AMGDSP        G08INV
315000941012     C                   MOVE      *ZEROS        G08DAT
315100941012     C                   MOVEL     '3'           G08ERR
315200941012     C                   CALL      'XSRDA8'
315300941012     C                   PARM                    WLBDA8
315400941012     C                   Z-ADD     G08DAT        VDSP
315500941216     C                   MOVE      ARBMGS        EXMGS
315600941216     C                   MOVE      ARBLNA        EXLNA
315700950102     C                   MOVE      ARBAAS        EXAAS
315800950102     C                   MOVE      ARBLNP        EXLNP
315900950102     C                   MOVE      ARBNRS        EXNRS
316000950102     C                   MOVE      ARBNSP        EXNSP
316100940308     C* TIPO BOLLA
316200941006     C                   MOVEL     ARBCBO        VTBL
316300940308     C                   MOVEL     '3A'          TBLCOD
316400940308     C                   MOVEL     *BLANK        TBLKEY
316500941006     C                   MOVEL     ARBCBO        TBLKEY
316600940308     C     KTAB          CHAIN     TABEL                              21
316700940308     C   21              MOVEL     *ALL'?'       DTBL
316800940308     C  N21              MOVEL     TBLUNI        DS3A
316900940308     C  N21              MOVEL     §3ADES        DTBL
317000940308     C  N21              MOVEL     §3ATB1        EXPORT            1
317100950725     C  N21              MOVEL     §3ATB1        WEXTB1            2
317200950725     C  N21              MOVEL     §3ATB2        WEXTB2            2
317300940308     C  N21              MOVEL     §3AFTD        TASDES            1
317400940308     C  N21EXPORT        IFEQ      'A'
317500940308     C                   MOVE      ARBKSC        §KSC
317600941006     C                   MOVE      ARBCTR        KCTR
317700940308     C                   END
317800000120      * SE BOLLA DI RECUPERO NON LAQSCIO EFFETTUARE NON LASCIO
317900000120      * EFFETTUARE NE RESO NE DIROTTAMENTO NE CAMBIO DI PORTO
318000000120     C     §3ARBL        IFEQ      'R'
318100000120     C                   MOVEL     '2'           $ERR
318200000120     C                   END
318300941219      * GUARDO SE BOLLA ITALIA O ESTERO
318400941219     C                   CLEAR                   DS15
318500941219     C                   MOVEL     '15'          TBLCOD
318600941219     C                   MOVEL     *BLANK        TBLKEY
318700941219     C                   MOVEL     ARBNZD        TBLKEY
318800941219     C     KTAB          CHAIN     TABEL                              21
318900941219     C  N21              MOVEL     TBLUNI        DS15
319000971113     C* Scopro se filiale Estera in base a flag su AZORG
319100971113     C                   MOVEL     *BLANKS       ORGFL1
319200971113     C     ARBLNP        CHAIN     AZORG01L                           21
319300971230     C                   MOVEL     ORGDIT        DITLNP            3
319400000421      *CONTROLLO SE FILIALE POSTE-SE SI BOLLA NON TRATTABILE TRANNE SERIE 00
319500000417     C                   MOVEL     ORGDE3        OG143
319600000421     C                   MOVE      *BLANKS       WPOSTE            1
319700020729     C     §OgNTW        IFEQ      'PPT'
319800000421     C     ARBNRS        IFEQ      0
319900000421     C                   MOVE      '1'           WPOSTE
320000000421     C                   ELSE
320100000417     C                   MOVE      '3'           $ERR
320200000421     C                   END
320300000417     C                   END
320400000417      *
320500940928      * BOLLA CON CONTRASSEGNO
320600941216     C                   MOVE      *ZEROS        VASS
320700941216     C                   MOVEL     *BLANK        VVCAP
320800941216     C                   MOVEL     *BLANK        VVTIC
320900950221     C                   CLEAR                   AR9CAS
321000950221     C                   CLEAR                   AR9CMB
321100950221     C                   CLEAR                   AR9GCA
321200950221     C                   CLEAR                   AR9VCA
321300950221     C                   CLEAR                   AR9TIC
321400940928     C     §3AFCA        IFNE      0
321500051115     C     KBOL          CHAIN     FIAR901L                           43
321600941216     C  N43              Z-ADD     AR9CAS        VASS
321700940928     C  N43              MOVEL     AR9VCA        VVCAP
321800941216     C  N43              MOVEL     AR9TIC        VVTIC
321900940928     C                   ENDIF
322000941217     C* IMPOSTO LA DIVISA UAGUALE ALLA VECCHIA SE NO GIACENZE
322100941217     C     $GIAC         IFEQ      '0'
322200941010     C                   MOVEL     VVCAP         VVCA
322300941221     C* GUARDO CHE DIVISA è
322400941221     C                   CLEAR                   DSCV
322500941221     C                   Z-ADD     1             X
322600941221     C     VVCA          LOOKUP    DIV(X)                                 21
322700941221     C   21              MOVEL     DCV(X)        DSCV
322800941221     C*
322900941217     C                   ENDIF
323000941217     C* IMPOSTO  TIPO INCASSO SEMPRE UGUALE AL   VECCHIO
323100941217     C                   MOVEL     VVTIC         VTIC
323200940928     C* PRENDO DATI TASSAZIONE  1° BOLLA
323300941216     C                   CLEAR                   AVA
323400941216     C                   CLEAR                   ASV
323500950221     C                   CLEAR                   AR6FIV
323600950221     C                   CLEAR                   AR6NFT
323700950221     C                   CLEAR                   AR6DFT
323800950221     C                   CLEAR                   AR6CEI
323900950221     C                   CLEAR                   AR6IFT
324000950221     C                   CLEAR                   AR6IMV
324100950221     C                   CLEAR                   AR6POR
324200991001     C                   CLEAR                   AR6CTR
324300991001     C                   CLEAR                   AR6KSC
324400990930     C* CONTROLLO SE LA BOLLA E' IN PORTO ASSEGNATO
324500991001     C                   MOVEL     §3ATB1        WPORTO            1
324600991001     C     WPORTO        IFEQ      'A'
324700990930     C                   MOVEL     '1'           KTRC
324800020618     C     KAR6          CHAIN     FIAR601L                           43
324900991001     C     AR6DFT        IFGT      0
325000991001     C                   MOVE      AR6FIV        VFIV
325100991001     C                   MOVE      AR6NFT        VNFT
325200991001     C                   MOVE      AR6DFT        G08INV
325300991001     C                   MOVE      *ZEROS        G08DAT
325400991001     C                   MOVEL     '3'           G08ERR
325500991001     C                   CALL      'XSRDA8'
325600991001     C                   PARM                    WLBDA8
325700991001     C                   Z-ADD     G08DAT        VDFT
325800991001     C                   END
325900991001     C                   ELSE
326000991001     C                   MOVEL     §3ATB2        WPORTO
326100991001     C     WPORTO        IFEQ      'A'
326200991001     C                   MOVEL     '2'           KTRC
326300020618     C     KAR6          CHAIN     FIAR601L                           43
326400991001     C                   END
326500991001     C                   END
326600941010     C* CARICO LE VARIE PRESENTI + DATI FATTURA
326700991001     C     *IN43         IFEQ      '0'
326800991001     C* Imposto codice tariffa e codice cliente da AR6
326900991001     C* se c'è
327000991001     C                   MOVE      AR6KSC        §KSC
327100991001     C                   MOVE      AR6CTR        KCTR
327200941010      *
327300941216     C                   MOVEL     AR6SV1        ASV(1)
327400941216     C                   MOVE      AR6VA1        AVA(1)
327500941216     C                   MOVEL     AR6SV2        ASV(2)
327600941216     C                   MOVE      AR6VA2        AVA(2)
327700941216     C                   MOVEL     AR6SV3        ASV(3)
327800941216     C                   MOVE      AR6VA3        AVA(3)
327900940929     C* PRENDO DATI ESTENSIONE TASSAZIONE X VARIE AGGIUNTIVE
328000940929      * SE LA TERZA VARIA è IMPOSTATA
328100991001     C     AR6SV3        IFNE      *BLANK
328200940929     C                   Z-ADD     3             X
328300040123     C     Kar6          SETLL     FIAR7000
328400040123     C     Kar6          READE     FIAR7000                               43
328500991001     C     *IN43         DOWEQ     *OFF
328600940929     C                   ADD       1             X
328700941216     C                   MOVEL     AR7SVN        ASV(X)
328800941216     C                   MOVE      AR7VAN        AVA(X)
328900040123     C     Kar6          READE     FIAR7000                               43
329000991001     C                   ENDDO
329100991001     C                   ENDIF
329200020102     C*CONVERTO IMPORTI IN EURO SE LA VECCHIA BOLLA ERA IN LIRE
329300020102     C     AR6DIV        IFNE      §GEDCN
329400020102      *SPESE TRASPORTO
329500020102     C     AR6POR        IFGT      0
329600020102     C                   CLEAR                   YEURDS
329700020102     C                   MOVEL     AR6DIV        YECDVI
329800020102     C                   Z-ADD     AR6POR        YECIMI
329900020102     C                   MOVEL     §GEDCN        YECDVO
330000020102     C                   MOVEL     '3'           YECDCO
330100020102     C                   MOVEL     'H'           YECTAR
330200020102     C                   CALL      'YEURCO'
330300020102     C                   PARM                    YEURDS
330400020102     C                   Z-ADD     YECIMO        AR6POR
330500020102     C                   ENDIF
330600020102     C                   DO        20            XM                3 0
330700020102     C     AVA(XM)       IFGT      0
330800020102     C                   CLEAR                   YEURDS
330900020102     C                   MOVEL     AR6DIV        YECDVI
331000020102     C                   Z-ADD     AVA(XM)       YECIMI
331100020102     C                   MOVEL     §GEDCN        YECDVO
331200020102     C                   MOVEL     '3'           YECDCO
331300020102     C                   MOVEL     'H'           YECTAR
331400020102     C                   CALL      'YEURCO'
331500020102     C                   PARM                    YEURDS
331600020102     C                   Z-ADD     YECIMO        AVA(XM)
331700020102     C                   ENDIF
331800020102     C                   ENDDO
331900020102     C                   ENDIF
332000991001     C                   ENDIF
332100941011     C* PRENDO 2° RAGIOSNE SOCIALE DESTINATARIO
332200950221     C                   CLEAR                   AR4NOT
332300950307     C                   CLEAR                   AR4RD2
332400941011     C                   MOVE      'D'           §TRC
332500060213     C     KAR4          CHAIN(N)  FiAR401L                           43
332600941011     C  N43              MOVE      AR4NOT        VRD2
332700941227     C  N43              MOVE      AR4NOT        AR4RD2
332800031119      *eseguo la stessa operazione per i riferimenti mittenti tipo
332900031119      * rekord GEN riferimenti destinatario
333000031119     C                   clear                   dar5gen
333100031119     C                   move      'GEN'         trcar5
333200051013     C     Kfiar5        CHAIN(n)  FiAR501L                           43
333300031119     C     *IN43         IFEQ      '0'
333400031119     C                   MOVEL     ar5uni        dar5gen
333500031119     C                   endif
333600941011     C                   END
333700940308     C                   ENDSR
333800920113     C*---------------------------------------------------------------*
333900940309     C*   A R B V I D                                                 *
334000920113     C*---------------------------------------------------------------*
334100941219     C     ARBVID        BEGSR
334200961009     C* se c'è il reso obbligat. da particolarità reperisco il motivo
334300961009     C* dall' ultimo evento della spedizione
334400961009     C   94              EXSR      CHKEVB
334500940307     C                   SETOFF                                       197172
334600940307     C                   SETOFF                                       737475
334700050222     C                   SETOFF                                       7647
334800940309     C                   MOVEL     '0'           $FOANC            1
334900940915     C                   WRITE     LG20Z02
335000920114     C     VID11         TAG
335100000421     C     WPOSTE        IFEQ      '1'
335200000421     C                   MOVE      'R'           VFRI
335300000421     C                   MOVE      'S'           V4CRCS
335400000421     C                   SETON                                        93
335500000421     C                   ELSE
335600000421     C                   SETOFF                                       93
335700000421     C                   ENDIF
335800940915     C                   WRITE     LG20Z03
335900940915     C                   EXFMT     LG20D04
336000940307     C                   MOVEL     *BLANK        $MSG
336100940307     C                   SETOFF                                       197172
336200940307     C                   SETOFF                                       737475
336300050222     C                   SETOFF                                       7667
336400961009     C                   SETOFF                                       4796
336500940309     C   KL              GOTO      ENDARB
336600100614      * richiamo pgm interrogazione bolle
336700100614     C   Kg              exsr      richbl
336800920305     C* KH= CMD 08 PER FORZARE ANNULLO CONTRASSEGMO
336900940309      *--------------------------------------------
337000940309     C   KH              MOVEL     '1'           $FOANC
337100941217     C*  IMMESSO ?
337200941217     C     VFRI          IFEQ      '?'
337300941217     C                   MOVEL     '2D'          §COD
337400941217     C                   MOVEL     *BLANK        §KEY
337500941217     C                   MOVEL     *BLANK        §DES
337600941217     C                   MOVE      *BLANK        VVCA
337700941217     C                   CALL      'X§TB60R'
337800941217     C                   PARM                    CODUT
337900941217     C                   PARM                    §COD
338000941217     C                   PARM                    §KEY
338100941217     C                   PARM                    §DES
338200941217     C                   MOVEL     §KEY          VFRI
338300941217     C                   GOTO      VID11
338400941217     C                   ENDIF
338500960327     C*  Se ho immesso disposizioni di rientro riemetto videata
338600960327     C*  se non ho ancora visualizzato se reso per avaria.
338700960327     C     VFRI          IFEQ      'R'
338800960327     C     *IN07         IFEQ      '0'
338900960327     C                   SETON                                        07
339000960327     C                   GOTO      VID11
339100960327     C                   END
339200960327     C                   ELSE
339300960327     C*  Se non ho immesso disposizioni di rientro azzero
339400960327     C*  indicatore reso per avaria.
339500960327     C                   SETOFF                                       07
339600960327     C                   MOVEL     'N'           V4CRCS
339700960327     C                   END
339800941217     C*  SOLO RICONSEGNA O DIROTTAMENTO  O RIENTRO
339900920114     C                   MOVEL     '2D'          TBLCOD
340000920114     C                   MOVEL     *BLANK        TBLKEY
340100920114     C                   MOVEL     VFRI          TBLKEY
340200920114     C     KTAB          CHAIN     TABEL                              21
340300920114     C   21              SETON                                        7119
340400920114     C   21              MOVEL     *ALL'?'       DFRI
340500920114     C  N21              MOVEL     TBLUNI        DS2D
340600920114     C  N21              MOVEL     §2DDES        DFRI
340700940307     C   71              MOVEL     ERR(8)        $MSG
340800941217     C   19              GOTO      VID11
340900920131     C* POSSO FARE SOLO DIROTTAM.O INVERSIONI DI PORTO
341000940307     C  N19VFRI          IFEQ      'C'
341100920131     C     VNPC          ANDEQ     *BLANK
341200940307     C                   SETON                                        7219
341300940307     C                   MOVEL     ERR(9)        $MSG
341400920131     C                   END
341500920114     C* NUOVO PORTO DEVE ESSERE DIVERSO DALLA BOLLA VECCHIA
341600920114     C  N19VNPC          IFEQ      EXPORT
341700921026     C     VFRI          ANDNE     'R'
341800941228     C                   SETON                                        7219
341900941228     C                   MOVEL     ERR(10)       $MSG
342000920114     C                   END
342100960205     C* NUOVO PORTO DEVE ESSERE DIVERSO DALLA BOLLA VECCHIA
342200960205     C  N19VNPC          IFNE      *BLANKS
342300960205     C     ARBFBR        ANDEQ     'S'
342400960315     C     VFRI          ANDEQ     'C'
342500960205     C                   SETON                                        7219
342600960205     C                   MOVEL     ERR(46)       $MSG
342700960205     C                   END
342800960327     C* Impossibile eseguire reso in C/Servizio su bolla di rientro
342900960327     C     ARBFBR        IFEQ      'S'
343000960327     C     VFRI          ANDEQ     'R'
343100960327     C     V4CRCS        ANDEQ     'S'
343200960328     C                   SETON                                        477119
343300960327     C                   MOVEL     ERR(49)       $MSG
343400960327     C                   END
343500960315     C* NUOVO PORTO non ammesso per disposizioni di reso
343600960315     C  N19VNPC          IFNE      *BLANKS
343700960315     C     VFRI          ANDEQ     'R'
343800960315     C                   SETON                                        7219
343900960315     C                   MOVEL     ERR(47)       $MSG
344000960315     C                   END
344100990203     C* Controllo se esiste pretica CA aperta se richiesto
344200990203     C* cambio di porto/dirottamentoCA aperta se richiesto
344300990203     C  N19VFRI          IFEQ      'C'
344400990203     C     VFRI          OREQ      'D'
344500990204     C*
344600040716      * SOSTITUISCO LETTURA DEL FILE FNDCT02L CON LA RICERCA DELLE CA LEGATE ALL
344700040716      * SIA COME MAMMA CHE COME FIGLIA
344800040716     c                   clear                   fidn12ds
344900040716     c                   eval      i12aas = kaas
345000040716     c                   eval      i12lnp = klnp
345100040716     c                   eval      i12nrs = knrs
345200040716     c                   eval      i12nsp = knsp
345300040716     c                   eval      i12fel = 'E'
345400040716     c                   eval      i12fan = 'E'
345500040716     c                   eval      i12fch = 'E'
345600040716     c                   call      'FIDN12R'
345700040716     c                   parm                    fidn12ds
345800040716      **
345900040716      * se c'è almeno una C.A. aperta
346000040716      **
346100040716     c     o12nca        ifgt      0
346200990203     C                   SETON                                        7119
346300990203     C                   MOVE      ERR(58)       $MSG
346400990203     C                   END
346500990203     C                   END
346600950331     C* SE BOLLA DA NON TRASMETTERE IN SEDE SI PUO' FARE SOLO
346700950307     C* DIROTTAM. SENZA CAMBIO DI PORTO IN QUANTO FORZERO' NELLA
346800950307     C* BOLLA DIROTTATA COD. BOLLA =£, PERCHè QUESTA BOLLA NON
346900950307     C* VIENE INVIATA IN SEDE E ANCHE LA FIGLIA NON VA TRASM.IN SEDE
347000950331     C     *IN19         IFEQ      '0'
347100950331     C                   CLEAR                   DS3A
347200950331     C                   Z-ADD     1             §F
347300950331     C     ARBCBO        LOOKUP    CDB(§F)                                04
347400950331     C   04              MOVEL     D3A(§F)       DS3A
347500950331     C     §3ABSD        IFEQ      'N'
347600950410     C     VFRI          ANDNE     'D'
347700950410     C     §3ABSD        OREQ      'N'
347800950410     C     VNPC          ANDNE     *BLANK
347900950307     C                   SETON                                        7119
348000950307     C                   MOVEL     ERR(45)       $MSG
348100950307     C                   END
348200950307     C                   END
348300921026     C* SE RIENTRO IL C/ASSEGNO DEVE ESSERE ANNULLATO
348400921026     C  N19VFRI          IFEQ      'R'
348500921026     C     VASS          ANDGT     0
348600921026     C     VVCS          IFNE      'S'
348700921026     C     VNCA          ORGT      *ZEROS
348800940307     C                   SETON                                        7319
348900940307     C                   MOVEL     ERR(11)       $MSG
349000921026     C                   END
349100921026     C                   END
349200040206     C* SE RIENTRO IL C/ASSEGNO DEVE ESSERE ANNULLATO
349300040206     C     Vnca          IFgt      *zeros
349400040206     C     Vvca          andeq     *blank
349500040206     C                   SETON                                        7319
349600040206     C                   MOVEL     ERR(64)       $MSG
349700040206     C                   END
349800960315     C* SE RIENTRO IL C/ASSEGNO DEVE ESSERE ANNULLATO
349900960315     C  N19VFRI          IFEQ      'R'
350000960315     C     VASS          ANDEQ     0
350100960315     C     VNCA          IFGT      0
350200960315     C     VVCS          OREQ      'S'
350300960315     C                   SETON                                        7319
350400960315     C                   MOVEL     ERR(48)       $MSG
350500960315     C                   END
350600960315     C                   END
350700940930     C* SI VARIAZ. C/A MA NON VARIATO L'IMPORTO O LA DIVISA
350800940930     C* NO VARIAZ. C/A MA     VARIATO L'IMPORTO O LA DIVISA
350900920114     C  N19VVCS          IFEQ      'S'
351000920114     C     VNCA          ANDEQ     VASS
351100940930     C     VVCA          ANDEQ     VVCAP
351200920114     C     VVCS          OREQ      'N'
351300920114     C     VNCA          ANDNE     VASS
351400920129     C     VNCA          ANDNE     *ZEROS
351500940930     C     VVCS          OREQ      'N'
351600940930     C     VVCA          ANDNE     VVCAP
351700920114     C                   SETON                                        7319
351800940307     C                   MOVEL     ERR(12)       $MSG
351900920114     C                   END
352000920207     C* NO CALCOLO PROVV.C/A MA VARIATO C/A
352100920114     C* NO CALCOLO PROVV.C/A MA INDICATO CHI PAGA
352200920114     C  N19VAPC          IFEQ      'N'
352300920114     C     VVCS          ANDEQ     'S'
352400920207     C     VNCA          ANDNE     *ZEROS
352500920114     C     VAPC          OREQ      'N'
352600920114     C     VPPC          ANDNE     *BLANK
352700920114     C                   SETON                                        7419
352800940307     C                   MOVEL     ERR(13)       $MSG
352900920114     C                   END
353000920114     C  N19VPPC          IFNE      *BLANK
353100920114     C                   MOVEL     '2E'          TBLCOD
353200920114     C                   MOVEL     *BLANK        TBLKEY
353300920114     C                   MOVEL     VPPC          TBLKEY
353400920114     C     KTAB          CHAIN     TABEL                              21
353500920114     C   21              SETON                                        7519
353600920114     C   21              MOVEL     *ALL'?'       DPSG
353700920114     C  N21              MOVEL     TBLUNI        DS2E
353800920114     C  N21              MOVEL     §2EDES        DPPC
353900920114     C                   ELSE
354000920114     C* SI CALCOLO PROVV.C/A MA NON INDICATO CHI PAGA
354100920114     C     VAPC          IFEQ      'S'
354200920114     C                   SETON                                        7519
354300920114     C                   END
354400920114     C                   END
354500940307     C   75              MOVEL     ERR(14)       $MSG
354600941221     C* SE EX PORTO ERA ASSEGNATO E NON E' CAMBIATO  O
354700941221     C* SE NUOVO PORTO è ASSEGNATO ..NON SI PUO INDICARE PROVVIG.
354800941010     C* A CARICO MITTENTE
354900941010     C  N19VNPC          IFEQ      'A'
355000941010     C     VPPC          ANDEQ     'M'
355100941221     C     EXPORT        OREQ      'A'
355200941221     C     VNPC          ANDEQ     ' '
355300941221     C     VPPC          ANDEQ     'M'
355400941010     C                   SETON                                          7519
355500941010     C                   MOVEL     ERR(14)       $MSG
355600941010     C                   ENDIF
355700940310     C* SE C'E' C/ASS: sE DIROTTO O NUOVO PORTO = ASSEGNATO
355800940310     C* DEVO INDICARE CHI PAGA LE PROVV.
355900940930     C  N19VASS          IFGT      *ZEROS
356000941223     C     VVCS          ANDEQ     'N'
356100940310     C     VNCA          ORGT      *ZEROS
356200940310     C     VFRI          IFEQ      'D'
356300940310     C     VAPC          ANDEQ     'N'
356400960108     C     VFRI          OREQ      'C'
356500940310     C     VAPC          ANDEQ     'N'
356600940310     C                   SETON                                        747519
356700940310     C                   MOVEL     ERR(13)       $MSG
356800940310     C                   END
356900940310     C                   END
357000940930     C* CONTROLLO DIVISA
357100941012     C                   MOVEA     VVCA          VCA§
357200941012     C     '?'           LOOKUP    VCA§                                   21
357300941012     C   21              DO
357400941012     C                   MOVEL     'CV'          §COD
357500941012     C                   MOVEL     *BLANK        §KEY
357600941012     C                   MOVEL     *BLANK        §DES
357700941012     C                   MOVE      *BLANK        VVCA
357800941012     C                   CALL      'X§TB60R'
357900941012     C                   PARM                    CODUT
358000941012     C                   PARM                    §COD
358100941012     C                   PARM                    §KEY
358200941012     C                   PARM                    §DES
358300941012     C                   MOVEL     §KEY          VVCA
358400941012     C                   GOTO      VID11
358500941012     C                   END
358600940930     C  N19VVCA          IFNE      *BLANK
358700940930     C     VVCAP         ORNE      *BLANK
358800940930     C     VVCA          IFNE      *BLANK
358900940930     C                   MOVEL     VVCA          WVCA              3
359000940930     C                   ELSE
359100940930     C                   MOVEL     VVCAP         WVCA
359200940930     C                   END
359300940930     C     VNCA          IFNE      *ZEROS
359400940930     C                   Z-ADD     VNCA          W0133            13 3
359500940930     C                   ELSE
359600940930     C                   Z-ADD     VASS          W0133
359700940930     C                   END
359800940930     C                   EXSR      CTRDIV
359900940930     C                   ENDIF
360000920305     C* RICH. CMD 08 PER CONFERMA ANNULLO C/ASSEGNO
360100940309     C  N19$FOANC        IFEQ      '0'
360200940309     C     VNCA          ANDEQ     0
360300920305     C     VVCS          ANDEQ     'S'
360400941010     C     VVCA          ANDEQ     *BLANK
360500940307     C                   SETON                                        1976
360600940307     C                   MOVEL     ERR(16)       $MSG
360700920305     C                   END
360800961009     C*  Se  obbligo disposiz. di reso metto in VNOT1 il motivo dall'evento
360900961009     C*  e obbligo l'inserimento del motivo se l'evento è generico
361000961009     C     *IN94         IFEQ      '1'
361100961009     C     VNOT1         ANDEQ     *BLANK
361200961009     C                   SETON                                        1996
361300961009     C                   MOVEL     ERR(50)       $MSG
361400961009     C                   ENDIF
361500930503     C  NKF
361600920114     COR 19              GOTO      VID11
361700940309     C     ENDARB        ENDSR
361800961009     C/SPACE 3
361900961009     C*---------------------------------------------------------------*
362000961009     C*   C H K E V B  _                                              *
362100961009     C*---------------------------------------------------------------*
362200961009     C     CHKEVB        BEGSR
362300961009     C                   MOVE      VAAS          KAAS1
362400961009     C                   MOVE      VLNP          KLNP1
362500961009     C                   MOVE      VNRS          KNRS1
362600961009     C                   MOVE      VNSP          KNSP1
362700961009     C                   MOVE      VNSP          KNSP1
362800961009     C                   MOVE      *HIVAL        KDEV
362900961009     C                   MOVE      *HIVAL        KHEV
363000990803     C     KEVB1         SETGT     FNEVB01L
363100990803     C     KEVB2         READPE    FNEVB01L                               45
363200961009     C     *IN45         IFEQ      '0'
363300081219     C                   Z-ADD     1             XY                3 0
363400961009     C     EVBCEV        LOOKUP    C2A(XY)                                45
363500961009     C     *IN45         IFEQ      '1'
363600961009     C                   MOVEL     D2A(XY)       DS2A
363700041122      * se l'evento,la cui descrizione deve essere messa nelle note della
363800041122      * bolla di reso per effetto della particolarità,è un evento di giacenz
363900060213      * di attesa (G02, G03..) prendo la descrizione da FiAR4 TRK "G"
364000041122     c                   if        §2Aftc='G' and §2Agla>0 and §2Aflg<>'S'
364100041122     C                   MOVEL     'G'           §TRC
364200060213     C     KAR4          CHAIN(N)  FiAR401L                           43
364300041122     C  N43              MOVEL     AR4NOT        VNOT1
364400041122     c                   else
364500041122     c                   MOVEL     §2ADES        VNOT1
364600041122     c                   endif
364700961009     C                   ENDIF
364800961009     C                   ENDIF
364900050504     c* se particolarità consegne = no parziali imposto la dicitura nel
365000050504     c* campo delle note: Reso x particolarità "NO consegna parziale"
365100050504     c                   if        vnot1 = *blanks and arbgma <> ' '
365200050504     C                   MOVEL     '7R'          TBLCOD
365300050504     C                   MOVEL     *BLANK        TBLKEY
365400050504     C                   MOVEL     ARBGMA        TBLKEY
365500050504     C     KTAB          CHAIN     TABEL                              21
365600050504     C     *IN21         IFEQ      '0'
365700050504     C                   MOVEL     TBLUNI        DS7R
365800050504     C                   if        §7R8cp='N'
365900050504     c                   eval      vnot1=
366000050505     c                             'Reso:NO consegna -
366100050505     c                             parziale'
366200050504     c                   end
366300050504     c                   end
366400050504     c                   end
366500961009     C                   ENDSR
366600940308     C/SPACE 3
366700940308     C*---------------------------------------------------------------*
366800940308     C*   G C A V I D                                                 *
366900940308     C*---------------------------------------------------------------*
367000940308     C     GCAVID        BEGSR
367100940308     C* DATA APERTURA GIACENZA
367200940308     C                   MOVEL     GCPAGC        AMGDTG
367300941012     C                   MOVE      GCPMGC        AMGDTG
367400941012     C                   MOVE      AMGDTG        G08INV
367500941012     C                   MOVE      *ZEROS        G08DAT
367600941012     C                   MOVEL     '3'           G08ERR
367700941012     C                   CALL      'XSRDA8'
367800941012     C                   PARM                    WLBDA8
367900941012     C                   Z-ADD     G08DAT        VDTG
368000940315     C                   MOVE      GCPNGC        VNGC
368100940308     C* FIL. CHE HA APERTO LA GIACENZA
368200940308     C                   MOVE      GCPFGC        VFGC
368300940308     C     VFGC          IFNE      *ZEROS
368400940308     C     VFGC          CHAIN     AZORG01L                           21
368500940308     C  N21ORGFVA        COMP      '*'                                    21
368600940308     C   21              MOVEL     *ALL'?'       DFGC
368700940308     C  N21              MOVEL     ORGDES        DFGC
368800940308     C                   END
368900940308     C*
369000940308     C* DATA ULTIMA RIAPERTURA
369100050222     c                   if        gcpriap = 'R'
369200940928     C                   MOVE      GCPDUR        G08INV
369300940928     C                   MOVE      *ZEROS        G08DAT
369400940928     C                   MOVEL     '3'           G08ERR
369500940928     C                   CALL      'XSRDA8'
369600940928     C                   PARM                    WLBDA8
369700940928     C                   Z-ADD     G08DAT        VDIG
369800050222     c                   else
369900050222     C                   Z-ADD     0             VDIG
370000050222     c                   end
370100940308     C* DATI DELLA SPEDIZIONE
370200050222     C                   Z-ADD     GCPLNPa       VLNP
370300050222     C                   Z-ADD     GCPAASa       VAAS
370400050222     C                   Z-ADD     GCPNRSa       VNRS
370500050222     C                   Z-ADD     GCPNSPa       VNSP
370600940308     C* DATA DISPOSIZINI
370700940928     C                   MOVE      GCPDDM        G08INV
370800940928     C                   MOVE      *ZEROS        G08DAT
370900940928     C                   MOVEL     '3'           G08ERR
371000940928     C                   CALL      'XSRDA8'
371100940928     C                   PARM                    WLBDA8
371200940928     C                   Z-ADD     G08DAT        VDDM
371300940928     C* DATA ESEGUIBILITA DISPOSIZIONI
371400940928     C                   SETOFF                                       79
371500940928     C     GCPDED        IFGT      0
371600940928     C                   MOVE      GCPDED        G08INV
371700940928     C                   MOVE      *ZEROS        G08DAT
371800940928     C                   MOVEL     '3'           G08ERR
371900940928     C                   CALL      'XSRDA8'
372000940928     C                   PARM                    WLBDA8
372100940928     C                   Z-ADD     G08DAT        VDED
372200940928     C     GCPDED        IFGT      AMGUDT
372300940928     C                   MOVEL     ERR(36)       $MSG
372400940928     C                   SETON                                        19
372500940928     C                   ENDIF
372600940928     C                   SETON                                        79
372700940928     C                   ENDIF
372800940308     C* TIPO DISPOSIZIONI
372900940311     C* SE HO FORZATO DISPOSIZ. NON IMPOSTO DA GIAC
373000940311     C     $FOFRI        IFEQ      '0'
373100941006     C                   MOVEL     GCPDIS        VFRI
373200940311     C                   MOVEL     GCPASG        VASG
373300940311     C                   MOVEL     GCPPSG        VPSG
373400940311     C                   END
373500940308     C                   MOVEL     '2D'          TBLCOD
373600940308     C                   MOVEL     *BLANK        TBLKEY
373700940308     C                   MOVEL     VFRI          TBLKEY
373800940308     C     KTAB          CHAIN     TABEL                              21
373900940308     C   21              MOVEL     *ALL'?'       DFRI
374000940308     C  N21              MOVEL     TBLUNI        DS2D
374100940308     C  N21              MOVEL     §2DDES        DFRI
374200940308     C* SPESE GIACENZA
374300940308     C     VPSG          IFNE      *BLANK
374400940308     C                   MOVEL     '2E'          TBLCOD
374500940308     C                   MOVEL     *BLANK        TBLKEY
374600940308     C                   MOVEL     VPSG          TBLKEY
374700940308     C     KTAB          CHAIN     TABEL                              21
374800940308     C   21              MOVEL     *ALL'?'       DPSG
374900940308     C  N21              MOVEL     TBLUNI        DS2E
375000940308     C  N21              MOVEL     §2EDES        DPSG
375100940308     C                   END
375200940308     C* NUOVO PORTO DI CONSEGNA
375300940308     C                   MOVEL     GCPNPC        VNPC
375400940308     C     GCPNPC        IFNE      *BLANK
375500940308     C     GCPNPC        COMP      'A'                                    21
375600940308     C   21              MOVEL     'ASSEGNAT'    DNPC
375700940308     C   21              MOVE      'O '          DNPC
375800940308     C  N21              MOVEL     'FRANCO  '    DNPC
375900940308     C  N21              MOVE      '  '          DNPC
376000940308     C                   ELSE
376100940308     C                   MOVE      *BLANKS       DNPC
376200940308     C                   END
376300991221     C* GEST. C/ASSEGNO
376400040203     c                   if        gcpvca <> *blank
376500940928     C                   MOVEL     GCPVCA        VVCA
376600040203     c                   else
376700040203     C                   MOVEL     ar9VCA        VVCA
376800040203     c                   end
376900941217     C* GUARDO CHE DIVISA è
377000941217     C                   CLEAR                   DSCV
377100941217     C                   Z-ADD     1             X
377200941217     C     VVCA          LOOKUP    DIV(X)                                 21
377300941217     C   21              MOVEL     DCV(X)        DSCV
377400941217     C*
377500940308     C                   MOVEL     GCPVCS        VVCS
377600940928     C                   MOVE      GCPCAS        VNCA
377700940308     C                   MOVEL     GCPAPC        VAPC
377800940308     C                   MOVEL     GCPPPC        VPPC
377900940308     C     VPPC          IFNE      *BLANKS
378000940308     C                   MOVEL     '2E'          TBLCOD
378100940308     C                   MOVEL     *BLANK        TBLKEY
378200940308     C                   MOVEL     VPPC          TBLKEY
378300940308     C     KTAB          CHAIN     TABEL                              21
378400940308     C   21              MOVEL     *ALL'?'       DPSG
378500940308     C  N21              MOVEL     TBLUNI        DS2E
378600940308     C  N21              MOVEL     §2EDES        DPPC
378700940308     C                   END
378800960327     C                   MOVEL     'N'           V4CRCS
378900960327     C                   SETOFF                                       07
379000940308     C                   ENDSR
379100940308     C*---------------------------------------------------------------*
379200940308     C* G C A N O T   GESTIONE NOTE
379300940308     C*---------------------------------------------------------------*
379400940308     C     GCANOT        BEGSR
379500940308     C                   EXSR      PULIZ
379600940308     C                   MOVE      *ZEROS        §VARR
379700970625     C*  CARICO NOTE APERTURA GIACENZA
379800970625     C                   Z-ADD     12            §FAS
379900050222     C     KNOT          CHAIN     tignp01L                           21
380000970625     C     *IN21         IFEQ      '0'
380100970625     C                   MOVEL     GNPDMC        VDMC
380200970625     C                   ADD       1             NRR               3 0
380300970625     C                   WRITE     LG20S05
380400970625     C                   END
380500970625     C*  NOTE GIACENZA
380600970625     C                   Z-ADD     20            §FAS
380700050222     C     KNOT          SETLL     tignp01L                               21
380800970625     C     *IN21         DOUEQ     '1'
380900050222     C     KNOT          READE     tignp01L                               21
381000051013     c                   if        not *in21
381100051013     c                   movel     gcpflr        dgcpflr
381200051013     c* se disposizioni automatiche
381300051013     c                   if        d§gcpdisa = 'S' and gnptpn = 'R'
381400051013     c                   if        gnpdif=915
381500051013     c                             or gnpdif=9152
381600051013     c                   eval      DGNPDMC915 = gnpdmc
381700051013     c                   movel     'D'           flgdis            1
381800051013     c                   end
381900051013     c                   if        gnpdif=917
382000051013     c                   eval      DGNPDMC917 = gnpdmc
382100051013     c                   movel     'F'           flgdis            1
382200060317     c                   movel     d917persona   vref
382300051013     c                   end
382400051013     c                   end
382600160805     c* Non deve scrivere la riga che riporta la dicitura:
382700160805     c* "DATA DISPOS.CLIENTE:"
382800051220     c                   if        %subst(GNPDMC:1:20) <> 'DATA DISPOS.CLIENTE:'
382900051013     C                   MOVEL     GNPDMC        VDMC
383000051013     C                   ADD       1             NRR               3 0
383100051013     C                   WRITE     LG20S05
383200051220     c                   end
383300051220      **
383400970625     C* SE NR.PROGR. =999 E DESCRIZ. INIZIA CON §§ PRENDO L'IMPORTO
383500970625     C* DELLA VARIA R CALCOLATA DALLA PARTENZA SE DISPOSIZ. RIENTRO
383600970625     C*
383700051013     C     VFRI          IFEQ      'R'
383800970625     C     GNPNPR        ANDEQ     999
383900970625     C                   MOVEL     GNPDMC        §VR               2
384000970625     C     §VR           IFEQ      '§§'
384100970625     C                   MOVE      GNPDMC        §VARR
384200970625     C                   END
384300051013     C                   END
384400970625     C                   END
384500970625     C                   END
384600970625     C     NRR           IFEQ      0
384700970625     C                   MOVEL     CNOTA         VDMC
384800970625     C                   ADD       1             NRR               3 0
384900970625     C                   WRITE     LG20S05
385000970625     C                   END
385100940308     C*
385200961203     C                   WRITE     LG20C05
385300940308     C     ENDNOT        ENDSR
385400940308     C* PULIZIA SUBFILE
385500940308     C     PULIZ         BEGSR
385600940308     C                   SETON                                        70
385700940915     C                   WRITE     LG20C05
385800940308     C                   Z-ADD     0             NRR
385900940308     C                   SETOFF                                       70
386000940308     C                   ENDSR
386100920201     C/SPACE 3
386200920201     C*---------------------------------------------------------------*
386300920201     C*   A Z Z E R                                                   *
386400920201     C*---------------------------------------------------------------*
386500920201     C     AZZER         BEGSR
386600920201     C                   SETOFF                                       262730
386700920201     C                   SETOFF                                       313233
386800920228     C                   SETOFF                                       398788
386900920201     C                   SETOFF                                       353637
387000920201     C                   SETOFF                                       808182
387100950120     C                   SETOFF                                       832346
387200141118     C                   SETOFF                                       922034
387300940309     C                   SETOFF                                       99
387400940304     C*
387500990930     C                   CLEAR                   DBLP
387600990930     C                   CLEAR                   DTASV
387700051013     C                   CLEAR                   flgdis
387800920201     C*
387900141118     C                   MOVE      *BLANK        wrecsp
388000920217     C                   MOVE      *BLANK        §§KCVB
388100050405     C                   MOVE      0             spearr            1 0
388200940304     C                   CLEAR                   DSARBK
388300920217     C                   MOVE      *BLANK        §§TCVB
388400990930     C                   CLEAR                   DARBT
388500940304     C                   CLEAR                   DSARBD
388600920201     C*
388700950120     C                   MOVEL     'S'           VSIFAT
388800920201     C                   MOVEL     *ZEROS        VRCA
388900920201     C                   MOVEL     *ZEROS        VRTR
389000920201     C                   MOVEL     *ZEROS        VDSD
389100920201     C                   MOVEL     *ZEROS        VDSA
389200920201     C                   MOVEL     *ZEROS        VGGS
389300920201     C                   MOVEL     *ZEROS        VSGF
389400940325     C                   MOVEL     *BLANK        SGD$              1
389500940325     C                   MOVEL     *BLANK        SGF$              1
389600940325     C                   MOVEL     *BLANK        SGR$              1
389700060113     C                   MOVEL     *BLANK        sgfaf$            1
389800920201     C                   MOVEL     *ZEROS        VTSGS
389900920201     C                   MOVEL     *ZEROS        VSGR
390000920201     C                   MOVEL     *ZEROS        VSGD
390100920201     C                   MOVEL     *ZEROS        VSGA
390200991028     C                   MOVEL     §GEDCN        V6CDIV
390300991028     C                   MOVEL     §GEDCN        V6CRVA
390400991028     C                   MOVEL     §GEDCN        V6CRTA
390500990628     C                   MOVEL     *BLANK        VRSD
390600941011     C                   MOVEL     *BLANK        VRD2
390700920201     C                   MOVEL     *BLANK        VIND
390800920201     C                   MOVEL     *BLANK        VLOD
390900941217     C                   MOVEL     *BLANK        VCAD
391000920201     C                   MOVEL     *BLANK        VPRD
391100920201     C                   MOVEL     *ZEROS        VLNA
391200920201     C                   MOVEL     *BLANK        VDLNA
391300941216     C                   MOVE      *ZEROS        §ANTEP
391400031119     C                   MOVE      *BLANK        Vteld
391500031119     C                   MOVE      *BLANK        Vref
391600030807     C* ----------------------------
391700030807     C* IMPOSTA LA TABELLA DEI tipi record di ar5 da duplicare
391800030807     C* ----------------------------
391900030807     C                   MOVEL     'AR5'         TBECOD
392000030807     C                   Z-ADD     0             bb                3 0
392100030807     C                   Z-ADD     0             cc                3 0
392200030807     c                   clear                   rec
392300030807     c                   clear                   fig
392400030807     C     TBECOD        setll     TNTBE01L
392500030807     C                   DO        *hival
392600030807     C     TBECOD        READE     TNTBE01L
392700030807     c                   if        %eof(tntbe01l)
392800030807     c                   leave
392900030807     c                   end
393000030807     C     TBEATB        IFEQ      *BLANK
393100030807     C                   MOVEL     TBEUNI        DAR5
393200030807     c* aggiungo nella schiera FIG i tipi record da duplicare per le nuove
393300030807     c* bolle figlie a seconda delle varie disposizioni
393400030807     c                   select
393500030807     c                   when      vfri = 'D' and §ar5dupd = 'S'
393600030807     C                   ADD       1             bb
393700030807     C                   MOVEL     tbeke1        fig(bb)
393800030807     c                   when      vfri = 'R' and §ar5dupr = 'S'
393900030807     C                   ADD       1             bb
394000030807     C                   MOVEL     tbeke1        fig(bb)
394100030807     c                   when      vfri = 'C' and §ar5dupc = 'S'
394200030807     C                   ADD       1             bb
394300030807     C                   MOVEL     tbeke1        fig(bb)
394400030807     C                   ENDsl
394500030807     c* aggiungo nella schiera REC i tipi record da duplicare per le nuove
394600030807     c* bolle di recupero
394700030807     c                   if        §ar5drec = 'S'
394800030807     C                   ADD       1             cc
394900030807     C                   MOVEL     tbeke1        rec(cc)
395000030807     C                   END
395100030807     C                   END
395200030807     C*
395300030807     C                   ENDDO
395400920201     C                   ENDSR
395500911025     C*----------------------------------------------------*
395600911025     C*
395700911025     C*     R I E N T R I
395800911025     C*
395900911025     C*----------------------------------------------------*
396000911025     C     RIENTR        BEGSR
396001160805     c* forzo l'azzeramento dell'eventuale c/ass
396601160804     c                   eval      vvcs= 'S'
396602160804     c                   eval      vvca= ' '
396603160804     c                   eval      vnca= 0
396604160805     c*
396605160804     c                   exsr      srdsk
402700911114     C* CREAZ. BOLLA DI RIENTRO A2
402800911114     C* --------------------------
402900911025     C                   SETON                                        31
403100941003     C                   MOVEL     '2'           §LPCBO
403200960327     C*  Se reso in conto servizio imposto 'A' nel codice bolla
403300960327     C     V4CRCS        IFEQ      'S'
403400960327     C                   MOVEL     'A'           §LPCBO
403500960327     C                   END
403600971113     C* INVERTO LP CON LA
403700971113     C                   MOVE      ARBLNP        §LPLNA
403800911113     C                   MOVE      ARBLNA        §LPLNP
403801160804     c*
404000950427     C                   Z-ADD     ARBAAS        KAAS
404100950427     C                   Z-ADD     ARBLNP        KLNP
404200950427     C                   Z-ADD     ARBNRS        KNRS
404300950427     C                   Z-ADD     ARBNSP        KNSP
404400950427     C     KBOL          CHAIN     FNLBL01L                           04
404500061026      ** salva la chiave da riutilizzare x Rientri DPD di bolle dirottate
404600061026      **  per poter riagganciare il Parcel dalla Bolla Originale da cui
404700061026      **  prendere il Depot x agganciare anagrafica indirizzo a cui rispedire
404800061026     C                   clear                   LBLAAOsv
404900061026     C                   clear                   LBLLPOsv
405000061026     C                   clear                   LBLNROsv
405100061026     C                   clear                   LBLNSOsv
405200061026     c                   if        %Found(FNLBL01L)
405300061026     C                   z-add     LBLAAO        LBLAAOsv
405400061026     C                   z-add     LBLLPO        LBLLPOsv
405500061026     C                   z-add     LBLNRO        LBLNROsv
405600061026     C                   z-add     LBLNSO        LBLNSOsv
405700061026     c                   end
405800061026      **
405900950427     C     *IN04         IFEQ      '0'
406000950427     C     LBLLPO        ANDNE     0
406100960315     C     LBLLPO        ANDNE     ARBLNA
406200150521     C     LBLLPO        ANDNE     §LPLNA
406300150518     C                   MOVE      LBLLPO        §LPLNA
406400950427     C                   END
406600150518     c*  Se su AZORG c'è la forzatura per la LNA viene sostituita la
406700150518     c*    LNA con quella di OG143 §ogLNAres
406800150518     c                   exsr      CHG_LNA_xFORZA
406900150518     C*
407000150518     C                   MOVE      §LPLNA        VLNA
408200960327     C* se no reso in C/Servizio
408300960327     C     V4CRCS        IFNE      'S'
408500000405     C* SPESE GIACENZA
408600000405     C     $GIAC         IFEQ      '1'
408700000405     C     GCPSGS        ADD       GCPSGR        §TOTSP
408800000405     C     GCPSGD        ADD       §TOTSP        §TOTSP
408900000405     C     GCPSGA        ADD       §TOTSP        §TOTSP
409000000405     C                   END
409100010524     C*IMPOSTO CODICE PER RICERCA LUOGO
409200010524     C                   Z-ADD     ARBKSC        WCODIC
409300911114     C* SE ERA ASSEGNATO TRASPORTO VA COME ANTEPORTO
409400911114     C     EXPORT        IFEQ      'A'
409500010522     C                   Z-ADD     88888888      §ANTEP
409600010524     C                   Z-ADD     ARBCCM        WCODIC
409700000405     C                   END
409800911025     C* MITTENTE
409900911113     C                   MOVEL     ARBRSD        §LPRSM
410000911113     C                   MOVEL     ARBIND        §LPINM
410100911113     C                   MOVE      ARBCAD        §LPCAM
410200911113     C                   MOVEL     ARBLOD        §LPLOM
410300911113     C                   MOVEL     ARBPRD        §LPPRM
410400940928     C                   MOVEL     ARBNZD        §LPNZM
410500070919      * imposto destinatario
410700070919     C                   MOVEL     ARBPRM        §LPPRD
410800070919     C                   MOVE      ARBCAM        §LPCAD
410900070919     C                   MOVE      ARBCAM        VCAD
411000070919     C                   MOVEL     ARBPRM        VPRD
411100010522     C* DESTINATARIO IMPOSTO I DATI STANDARD MA POI CONTROLLO FORZATURA
411200010522     C* SE NELLA FILIALE È GESTITO IL LUOGO 300 IMPOSTO I DATI DI
411300010522     C* DESTINAZIONE DAL FNSPE SE PRESENTE PER IL CLIENTE
411400160805     c*?Se non esiste il luogo e il cliente è un codificato ed è impos tato
411500160805     c*?RMO prende i dati da CNACO
411600071002     c                   if        wcodic <> 0
411700070810     c                   clear                   cod4              4 0
411800071002     c                   move      wcodic        cod4
411900070808     c                   if        arbrmo <> *blank and
412000070808     c                             (cod4 <> 9999 and cod4 <> 8888)
412100070808     c                   exsr      repcnaco
412200070919     c                   endif
412300070919     c                   endif
412400070919      *
412500010522     C                   MOVEL     ARBRSM        §LPRSD
412600010522     C                   MOVEL     *BLANK        §LPRD2
412700010522     C                   MOVEL     ARBINM        §LPIND
412800010522     C                   MOVEL     ARBLOM        §LPLOD
412900010522     C                   MOVEL     ARBNZM        §LPNZD
413000010522     C*
413100010522     C                   MOVEL     ARBRSM        VRSD
413200010522     C                   MOVEL     *BLANK        VRD2
413300010522     C                   MOVEL     ARBINM        VIND
413400010522     C                   MOVEL     ARBLOM        VLOD
413500010522     C                   MOVEL     ARBNZM        VNZD
413600010522      * SE GESTITO LUOGO 300 IN FILIALE CERCO LUOGO PER IL DESTINATARIO
413700010522     C     KFNSPE        CHAIN     FNSPE01L                           21
413800041221      * prima di impostare i dati del luogo verifico che non ci siano
413900041221      * delle eccezioni per cliente linea di partenza
414000070322      * pulisco comunque il campo del giro forzato
414100070322     c                   clear                   §lpcgi
414200070903     c                   setoff                                       55
414300041221     c                   if        not *in21
414400041221     c                   exsr      ecceluogo
414500041221     c                   endif
414600010522      * SE TROVO IL LUOGO IMPOSTATO LO FORZO RISPETTO ALL'ORIGINALE
414700010522     C     *IN21         IFEQ      '0'
414800070903     C  n55              MOVEL     *BLANK        §LPLNA
414900010522     C                   MOVEL     SPERAG        §LPRSD
415000010522     C                   MOVEL     *BLANK        §LPRD2
415100010522     C                   MOVEL     SPEIND        §LPIND
415200010522     C                   MOVE      SPECAP        §LPCAD
415300010522     C                   MOVEL     SPELOC        §LPLOD
415400010522     C                   MOVEL     SPEPRO        §LPPRD
415500010522     C                   MOVEL     SPENAZ        §LPNZD
415600010522     C*
415700070903     C  n55              MOVEL     *BLANK        VLNA
415800010522     C                   MOVEL     SPERAG        VRSD
415900010522     C                   MOVEL     *BLANK        VRD2
416000010522     C                   MOVEL     SPEIND        VIND
416100010522     C                   MOVE      SPECAP        VCAD
416200010522     C                   MOVEL     SPELOC        VLOD
416300010522     C                   MOVEL     SPEPRO        VPRD
416400010522     C                   MOVEL     SPENAZ        VNZD
416500010524     C                   SETON                                        46
416600010524     C                   ENDIF
416700911113     C*
416800911113     C* IMPOSTO IL CODICE FINE GIACENZA
416900941217     C     $GIAC         IFEQ      '1'
417000911121     C                   MOVEL     '002'         VCFG
417100911114     C                   MOVEL     '2C'          TBLCOD
417200911114     C                   MOVEL     *BLANK        TBLKEY
417300911114     C                   MOVEL     VCFG          TBLKEY
417400911114     C     KTAB          CHAIN     TABEL                              21
417500911114     C   21              MOVEL     *ALL'?'       DCFG
417600060410     C  N21              MOVEL     TBLUNI        Ds2c
417700060410     C  N21              MOVEL     §2cdes        DCFG
417800941012     C                   MOVE      GMAUDT        VDCG
417900960327     C                   ENDIF
418000960327     C*----------------------------------------------------*
418100960327     C                   ELSE
418200960327     C*  Reso in C/Servizio
418300960327     C*  Mittente: Filiale Lna bolla
418400960328     C     ARBLNA        CHAIN     AZORG01L                           21
418500960327     C                   MOVEL     ORGDES        §LPRSM
418600960327     C                   MOVEL     ORGIND        §LPINM
418700960328     C                   MOVEL     ORGCPF        §LPCAM
418800960327     C                   MOVEL     ORGLOC        §LPLOM
418900960327     C                   MOVEL     ORGPRO        §LPPRM
419000960327     C                   MOVEL     *BLANKS       §LPNZM
419100960327     C*  Destinatario: Filiale Lnp bolla originale
419200960328     C     VLNA          CHAIN     AZORG01L                           21
419300960327     C                   MOVEL     ORGDES        §LPRSD
419400960327     C                   MOVEL     *BLANK        §LPRD2
419500960327     C                   MOVEL     ORGIND        §LPIND
419600960328     C                   MOVEL     ORGCPF        §LPCAD
419700960327     C                   MOVEL     ORGLOC        §LPLOD
419800960327     C                   MOVEL     ORGPRO        §LPPRD
419900960327     C                   MOVEL     *BLANKS       §LPNZD
420000960327     C*  Seconda note: Ragione sociale mittente originale
420100960327     C                   MOVEL     ARBRSM        §LPNT2
420200960327     C*
420300960327     C                   MOVEL     ORGDES        VRSD
420400960327     C                   MOVEL     *BLANK        VRD2
420500960327     C                   MOVEL     ORGIND        VIND
420600960328     C                   MOVEL     ORGCPF        VCAD
420700960327     C                   MOVEL     ORGLOC        VLOD
420800960327     C                   MOVEL     ORGPRO        VPRD
420900960327     C                   MOVEL     *BLANKS       VNZD
421000960327     C                   ENDIF
421001160804     C* STAMPA SEGNACOLLI SI - C. TRATT. MERCE  DEFAULT FILIALE
421002160804     c                   exsr      srctm2k
421003160804     C* IMPOSTO §LPNOT
421004160804     C                   EXSR      XLPNOT
421100911025     C                   ENDSR
421200070807      *_______________________________________________________
421300070807     c     repcnaco      begsr
421400070807      *_______________________________________________________
421500070807     c                   move      wcodic        i69kac
421600070807     c                   move      wcodic        i69kin
421700070807     C                   CALL      'TIBS69R'
421800070807     C                   PARM                    tibs69DS
421900070807     C                   PARM                    DS_cnaco
422000070807     C                   PARM                    DS_cnind
422100070807     C                   PARM                    DS_cnclp
422200070807     C                   PARM                    DS_fncls
422300070807      *
422400070807     C                   if        O69ERR <> '1'
422500070807     c                   movel     indcae        §lpcad
422600070807     c                   movel     indprv        §lpprd
422700070807     c                   end
422800070807     C*
422900070807     C                   MOVE      indcae        VCAD
423000070807     C                   MOVEL     indprv        VPRD
423100070807     C                   Endsr
423200041221      *----------------------------------------------------
423300041221     c     ecceluogo     begsr
423400041221      *----------------------------------------------------
423500041221     c                   move      'PP'          wtip
423600041221     c     ksp2          chain     fnsp201l
423700041221      * rek eccezioni partenza
423800041221     c                   if        %found(fnsp201l)
423900041221     c                   movel     sp2est        dpar
424000041221     c                   z-add     1             xi                3 0
424100070322     c                   z-add     0             xxi               3 0
424200061027      * se ho trovato il legame 04=*off cerco con la linea bolla originale
424300070322      * altrimenti con la linea bolla
424400061027     c  n04lbllpo        lookup    par(xi)                                55
424500061027     c   04arblnp        lookup    par(xi)                                55
424600041221      * se esiste linea di partenza cerco eccezione in arrivo corrispondente
424700041221     c                   if        *in55
424800041221     c                   move      'PA'          wtip
424900041221     c     ksp2          chain     fnsp201l
425000041221     c                   if        %found(fnsp201l)
425100041221     c                   movel     sp2est        darr
425200041221     c                   move      arr(xi)       vlna
425300041221     c                   move      arr(xi)       §lplna
425400041221     c                   endif
425500060807      * cerco anche la zona
425600060807     c                   move      'PZ'          wtip
425700060807     c     ksp2          chain     fnsp201l
425800060807     c                   if        %found(fnsp201l)
425900060807     c                   movel     sp2est        dzon
426000060807     c                   move      zon(xi)       §lpznc
426100060807     c                   endif
426200070322      *reperisco il rekord x il giro
426300070322      * gli eventuali giri forzati sono per ragioni di spazio scritti su 4
426400070322      * rekord quindi x reperire quello giusto è necessario individuare su
426500070322      * quale rekord puntare basandosi sull'indice della linea di partenza
426600070322      * ogni rekord contiene 6 giri quindi per ricavare l'indice corretto
426700070322      * è necessario sottrarre all'indice gli interi precedenti
426800070322     c                   select
426900070322     c                   when      xi < 7
427000070322     c                   move      'G1'          wtip
427100070322     c                   z-add     0             xxi
427200070322     c                   when      xi < 13
427300070322     c                   move      'G2'          wtip
427400070322     c                   z-add     6             xxi
427500070322     c                   when      xi < 19
427600070322     c                   move      'G3'          wtip
427700070322     c                   z-add     12            xxi
427800070322     c                   when      xi > 18
427900070322     c                   move      'G4'          wtip
428000070322     c                   z-add     18            xxi
428100070322     c                   endsl
428200070322     c     ksp2          chain     fnsp201l
428300070322     c                   if        %found(fnsp201l)
428400070322     c                   movel     sp2est        dgir
428500070322     c                   eval      §lpcgi = gir(xi - xxi)
428600070322     c                   endif
428700070322
428800041221      *non trovata linea partenza come eccezione verifico se esiste la linea
428900041221      * 999 nel caso non devo impostare il luogo ma forzo la LNP originale
429000041221     c                   else
429100041221     c                   z-add     1             xi
429200041221     c     999           lookup    par(xi)                                55
429300041221     c                   endif
429400041221      * se esistono eccezioni 55*on forzo indicatore per non impostare
429500041221      * i riferimenti del luogo 300
429900041221
430000041221     c                   endif
430100041221     c                   endsr
430200070509      *____________________________________________________
430300070509     c     ecceluogo2    begsr
430400070509      *____________________________________________________
430500070509     c                   move      'PA'          wtip
430600070509     c     ksp2          chain     fnsp201l
430700070509      * rek eccezioni arrivo
430800070509     c                   if        %found(fnsp201l)
430900070509     c                   movel     sp2est        darr
431000070509     c                   z-add     1             xi                3 0
431100070509     c                   z-add     0             xxi               3 0
431200070509      * cerca con linea a video
431300070509     c     vlna          lookup    arr(xi)                                55
431400070509      * se esiste linea di arrivo cerco eventuale personalizzazione giro
431500070509     c                   if        *in55
431600070509      *reperisco il rekord x il giro
431700070509      * gli eventuali giri forzati sono per ragioni di spazio scritti su 4
431800070509      * rekord quindi x reperire quello giusto è necessario individuare su
431900070509      * quale rekord puntare basandosi sull'indice della linea di partenza
432000070509      * ogni rekord contiene 6 giri quindi per ricavare l'indice corretto
432100070509      * è necessario sottrarre all'indice gli interi precedenti
432200070509     c                   select
432300070509     c                   when      xi < 7
432400070509     c                   move      'G1'          wtip
432500070509     c                   z-add     0             xxi
432600070509     c                   when      xi < 13
432700070509     c                   move      'G2'          wtip
432800070509     c                   z-add     6             xxi
432900070509     c                   when      xi < 19
433000070509     c                   move      'G3'          wtip
433100070509     c                   z-add     12            xxi
433200070509     c                   when      xi > 18
433300070509     c                   move      'G4'          wtip
433400070509     c                   z-add     18            xxi
433500070509     c                   endsl
433600070509     c     ksp2          chain     fnsp201l
433700070509     c                   if        %found(fnsp201l)
433800070509     c                   movel     sp2est        dgir
433900070509     c                   eval      §lpcgi = gir(xi - xxi)
434000070509     c                   endif
434100070509
434200070509     c                   endif
434300070509
434400070509     c                   endif
434500070509     c                   endsr
434600911025     C*----------------------------------------------------*
434700911025     C*
434800911025     C*     R I C O N S E G N A
434900911025     C*
435000911025     C*----------------------------------------------------*
435100911025     C     RICONS        BEGSR
435200090729      * se c'è contrassegno e non è abilitato attivo contatto
435300090729     C     §3AFCA        IFNE      0
435400090729     C                   IF        arbacc <> 'S'
435500090729     C                   exsr      contatto
435600090729     C                   endif
435700090729     C                   endif
435800911112     C*-------------------------
435900911026     C* NON  C'E CAMBIO DI PORTO
436000911026     C*-------------------------
436100920116     C     VNPC          IFEQ      ' '
436200941010     C                   EXSR      RICNOC
436300911026     C                   ELSE
436400941010     C*-------------------------
436500941010     C* C'E CAMBIO DI PORTO
436600941010     C*-------------------------
436700941010     C                   EXSR      RICSIC
436800941010     C                   END
436900950112     C* SE GIACENZA VISULIZZO  DATA FINE GIAC.
437000950120     C*  E MI POSIZIONO SUL CODICE FINE GIACENZA
437100950118     C     $GIAC         IFEQ      '1'
437200950118     C                   MOVEL     '   '         VCFG
437300950118     C                   MOVE      GMAUDT        VDCG
437400950120     C                   SETON                                        23
437500950118     C                   END
437600920203     C* SE SPESE GIAC. O PROVV.C/A O NUOVO PORTO ERA O E'=A
437700940307     C* DEVO CHIEDERE SE RICH. PGM TASSAZIONE
437800920107     C* SE SI VADO IN CONSEGNA CON NUOVA BOLLA : LA LEGO
437900920203     C   31VPSG          IFEQ      'D'
438000920107     C     VPPC          OREQ      'D'
438100920107     C     VNPC          OREQ      'A'
438200920203     C     VNPC          OREQ      ' '
438300920203     C     EXPORT        ANDEQ     'A'
438400920107     C                   SETON                                        87
438500920107     C                   END
438600911025     C                   ENDSR
438700941010     C*----------------------------------------------------*
438800941010     C*     R I C O N S E G N A - NO CAMBIO PORTO
438900941010     C*----------------------------------------------------*
439000971006     C     RICNOC        BEGSR
439100941010     C*  ADDEBITO SPESE GIACENZA AL DESTINAT.
439200941010     C     VPSG          IFEQ      'D'
439300941010     C                   SETON                                        33
439400941010     C                   END
439500941010     C*
439600950727     C     WEXTB1        IFEQ      'AS'
439700950727     C                   SETON                                        31
439800950727     C                   END
439900941010     C* C/ASSEGNO DEVO VEDERE SE C'ERA E CHI PAGAVA
440100941010     C     §3AFCA        IFNE      0
440200941010     C     §3AFCA        IFEQ      1
440300960506     C                   MOVEL     §3ATB1        §FA               1
440400941010     C                   ELSE
440500941010     C                   MOVEL     §3ATB2        §FA               1
440600941010     C                   END
441300160304     C     §FA           IFNE      'F'
441400941010     C     §3ATB1        ANDNE     'AS'
441500941010     C                   SETON                                        97
441600941010     C                   END
441800941010     C                   ELSE
441900941010     C                   SETOFF                                       97
442000941010     C                   END
442100941010     C*  CALCOLO PROVV. C/ASSEGNO SE PAGA DESTINAT.
442300941010     C                   MOVE      *ZEROS        EXRCA
442400941010     C                   MOVE      *ZEROS        §RCA
442500941010     C                   MOVE      *ZEROS        VRCA
442600941010     C     VAPC          IFEQ      'S'
442700941010     C     VPPC          ANDEQ     'D'
442800941219     C* IMPOSTO KEY X POSIZ. SU TARIFFA
442900941010     C                   EXSR      TARPCA
443100941010     C* SE PAGAVA IL DESTINATARIO  IMPORTO VECCHIO E NUOVO
443200941010     C     *IN97         IFEQ      '1'
443300950523     C     VASS          IFNE      *ZEROS
443400950523     C                   MOVEL     'V'           WCMB              1
443500950523     C                   Z-ADD     VASS          §CAS
443600950523     C                   EXSR      CASLIR
443700950523     C*  REPERISCO IMPORTO VECCHIA VARIA G
443800950523     C                   Z-ADD     0             EXRCA
443900950523     C* SPOSTO PROVV. CONTR. SU NUOVA BOLLA
444000991001     C     §3AFCA        IFNE      0
444100991001     C     AR6DFT        IFNE      0
444200991001     C                   Z-ADD     1             YY                4 0
444300991001     C     §$2VRG        LOOKUP    ASV(YY)                                05
444400991001     C     *IN05         IFEQ      '1'
444500991001     C                   Z-ADD     AVA(YY)       EXRCA
444600991001     C                   END
444700950523     C                   ELSE
444800971203     C                   MOVEL     *ALL'9'       EXRCA
444900991001     C                   ENDIF
445000991001     C                   ENDIF
445100950523     C                   ENDIF
445200950523     C* CALCOLO DIFFERNZA ANCHE SE NON C'ERA TASSAZIONE
445300971203     C     EXRCA         IFNE      *ALL'9'
445400941010     C     VVCAP         IFEQ      VVCA
445500941010     C                   MOVEL     'V'           WCMB
445600941010     C                   ELSE
445700941010     C                   MOVEL     'N'           WCMB
445800941010     C                   END
445900950523     C     VNCA          IFNE      *ZEROS
446000941010     C                   Z-ADD     VNCA          §CAS
446100950523     C                   END
446200941010     C                   EXSR      CASLIR
446300941010     C                   EXSR      CPROVV
446400950522     C     §RCA          SUB       EXRCA         WRCA
446500950522     C                   Z-ADD     §RCA          VRCA
446600971203     C                   END
446700941010     C                   ELSE
446800941010     C* NON PAGAVA NESSUNO O PAGAVA IL MITTENTE
446900941010     C* ---------------------------------------
447000941010     C     VNCA          IFGT      0
447100941010     C                   Z-ADD     VNCA          §CAS
447200941010     C                   ELSE
447300941010     C                   Z-ADD     VASS          §CAS
447400941010     C                   END
447500941010     C     VVCAP         IFEQ      VVCA
447600941010     C                   MOVEL     'V'           WCMB
447700941010     C                   ELSE
447800941010     C                   MOVEL     'N'           WCMB
447900941010     C                   END
448000941010     C                   EXSR      CASLIR
448100941010     C                   EXSR      CPROVV
448200941010     C                   MOVE      §RCA          VRCA
448300941010     C                   END
448400950522     C     VRCA          IFNE      0
448500971006     C* Per bolla in franco + C/Ass con addebito provv. detinat.
448600971006     C* se non ho variato l'importo del C/Assegno non devo accend.
448700971006     C* indicatore per addebito provviggioni
448900971006     C     §FA           IFEQ      'F'
449000971006     C     §3ATB2        ANDNE     'AP'
449100971006     C     §3ATB2        ANDNE     '  '
449200971006     C     VVCS          ANDNE     'S'
449300971006     C                   ELSE
449400941010     C                   SETON                                        35
449500941010     C                   END
449600971006     C                   END
449700941010     C                   END
449900941010     C*  G E S T I O N E   T I P O     B O L L A
450000941010     C*      VARIAZIONE C/ASSEGNO
450001160804     c                   exsr      srdsk
464800950102     C* SE E' ASSEGNATO VEDO SE POSSO VARIARE TASSAZIONE BOLLA
465000950726     C                   MOVEL     ' '           WNOSPG            1
465100950102     C* PER SPESE GIACENZA O PROVVIGIONI C/A
465200160805     C* O AZZERO VARIA "G" SE C/ASS. ANNULLATO
465300950102     C     EXPORT        IFEQ      'A'
465500160805     C* SE NON HO SPESE DI GIACENZA CONTROLLO SE LE AVEVO ADDEBITATE
465600160805     C* NEI GIRI PRECEDNETNI X ANDARE A STORNARLE
465700950726     C     *IN33         IFEQ      '0'
465800950726     C     §$2VRI        LOOKUP    VSV                                    21
465900950726     C   21              MOVEL     'S'           WNOSPG
466000950726     C                   END
466100950102     C     *IN33         IFEQ      '1'
466200950102     C     *IN35         OREQ      '1'
466300950102     C     VPPC          OREQ      'M'
466400950726     C     WNOSPG        OREQ      'S'
466500950102     C     VVCS          OREQ      'S'
466600950102     C     VNCA          ANDEQ     0
466700950102     C     VVCA          ANDEQ     *BLANK
466800991029     C     AR6NFT        IFEQ      0
466900950102     C                   MOVEL     'T0'          §CDVAR
467000950102     C                   ELSE
467100950102     C                   MOVEL     'T2'          §CDVAR
467200950102     C                   END
467300950102     C                   EXSR      CNTVAR
467400160805     C* SE BOLLA ORIGINALE E' UN ASSEGNATO STORNO NON POSSO VARIARE
467500160805     C* TASSAZIONE MA DEVO FARE UNA NUOVA BOLLA
467600950727     C  N26WEXTB1        IFEQ      'AS'
467700950727     C                   SETON                                        26
467800950727     C                   END
467900950727     C  N27WEXTB2        IFEQ      'AP'
468000950727     C                   SETON                                        27
468100950727     C                   END
468200950727     C*  CONTROLLO SE ESISTE UNA FIGLIA
468300950727     C  N26
468400950727     CANN27KBOL          CHAIN     FNLBL02L                           29
468500950727     C     *IN29         IFEQ      '0'
468600991001     C     AR6NFT        IFNE      *ZEROS
468700991001     C     AR6TRC        ANDEQ     '2'
468800950727     C                   SETON                                        27
468900950727     C                   ELSE
469000950727     C                   SETON                                        26
469100950727     C                   END
469200950727     C                   END
469300950102     C  N26
469400950102     CANN27              MOVEL     §CDVAR        §§TCVB
469500950102     C  N26
469600950102     CANN27              SETON                                        82
469700950102     C                   END
469800950102     C* SISTEMO PROVV. C/A  SUBITO
469900950102     C   82              DO
470000950102     C* CARICO LE VARIE ESISTENTI X DSARBT
470100950102     C                   MOVE      AVA           VVA
470200950102     C                   MOVEL     ASV           VSV
470300950102      * CERCO SE C'è GIA VARIA G
470400950102     C                   Z-ADD     1             X
470500950102     C     §$2VRG        LOOKUP    VSV(X)                                 21
470600950102      * TROVATA VARIA G
470700950102     C     *IN21         IFEQ      *ON
470800950522     C     WRCA          ADD       VVA(X)        VVA(X)
470900950102     C     VPPC          IFEQ      'M'
471000950102     C     VVCS          OREQ      'S'
471100950102     C     VNCA          ANDEQ     0
471200950102     C     VVCA          ANDEQ     *BLANK
471300950102     C                   MOVE      *ZEROS        VVA(X)
471400950102     C                   END
471500950102     C                   ELSE
471600950726      * NON TROVATA VARIA G
471700950726     C     VRCA          IFNE      0
471800040505     C     Vnca          andgt     0
471900950102     C                   Z-ADD     1             X
472000950102     C     ' '           LOOKUP    VSV(X)                                 21
472100950102     C     *IN21         IFEQ      *ON
472200950102     C                   Z-ADD     VRCA          VVA(X)
472300950102     C                   MOVEL     §$2VRG        VSV(X)
472400950102     C                   END
472500950726     C                   END
472600950726     C*
472700950102     C                   END
472800950102     C                   ENDDO
472900950102     C                   ELSE
473000950102     C* SE NON E' ASSEGNATO VEDO SE ESISTE 2° BOLLA
473100950102     C* ===================
473200950102     C* VARIO TASSAZ, 2° BOLLA :  CON PROVV. C/A SE NON HO SPESE GIAC.
473300950102   4 C     §3AFCA        IFEQ      2
473400950102     C     §3ATB2        ANDNE     'AP'
473500950102   3 C     *IN35         IFEQ      '1'
473600950102     C     *IN33         ANDEQ     '0'
473700991001   2 C     AR6NFT        IFEQ      0
473800950102     C                   MOVEL     '20'          §CDVAR
473900950102     C                   ELSE
474000950102     C                   MOVEL     '22'          §CDVAR
474100950102   2 C                   END
474200950102     C                   EXSR      CNTVAR
474300950102   2 C     *IN27         IFEQ      '0'
474400991001     C                   MOVEL     §CDVAR        §§TCVB
474500950102     C                   SETON                                        83
474600991001     C                   MOVE      AVA           VVA
474700991001     C                   MOVEL     ASV           VSV
474800991001      * CERCO SE C'è GIA VARIA G
474900991001     C                   Z-ADD     1             X
475000991001     C     §$2VRG        LOOKUP    VSV(X)                                 21
475100991001      * TROVATA VARIA G
475200991001     C     *IN21         IFEQ      *ON
475300991001     C     WRCA          ADD       VVA(X)        VVA(X)
475400991001     C     VPPC          IFEQ      'M'
475500991001     C     VVCS          OREQ      'S'
475600991001     C     VNCA          ANDEQ     0
475700991001     C     VVCA          ANDEQ     *BLANK
475800991001     C                   MOVE      *ZEROS        VVA(X)
475900991001     C                   END
476000991001     C                   ELSE
476100991001      * NON TROVATA VARIA G
476200991001     C     VRCA          IFNE      0
476300991001     C                   Z-ADD     1             X
476400991001     C     ' '           LOOKUP    VSV(X)                                 21
476500991001     C     *IN21         IFEQ      *ON
476600991001     C                   Z-ADD     VRCA          VVA(X)
476700991001     C                   MOVEL     §$2VRG        VSV(X)
476800991001     C                   END
476900991001     C                   END
477000991001     C*
477100991001     C                   END
477200950102   2 C                   END
477300950102   3 C                   END
477400950102   4 C                   END
477500950102     C                   END
477600941010     C                   SETON                                        46
477700941010     C                   MOVEL     ARBRSD        VRSD
477800941011     C                   MOVEL     AR4RD2        VRD2
477900941010     C                   MOVEL     ARBIND        VIND
478000941010     C                   MOVEL     ARBCAD        VCAD
478100941010     C                   MOVEL     ARBPRD        VPRD
478200941010     C                   MOVEL     ARBLOD        VLOD
478300961203     C                   MOVEL     ARBNZD        VNZD
478400031119     c* imposto dati referente destinatario
478500031119     c                   movel     §ar5teld      vteld
478600031119     c                   movel     §ar5ref       vref
478700960124     C*  Salvo dati anagrafici destinatario x controllare se devo
478800060213     C*  scrivere FiAR4 x variazione
478900960124     C                   MOVEL     ARBRSD        WSVRSD
479000960124     C                   MOVEL     AR4RD2        WSVRD2
479100960124     C                   MOVEL     ARBIND        WSVIND
479200960124     C                   MOVEL     ARBCAD        WSVCAD
479300960124     C                   MOVEL     ARBPRD        WSVPRD
479400960124     C                   MOVEL     ARBLOD        WSVLOD
479500961203     C                   MOVEL     ARBNZD        WSVNZD
479600941010     C*
479700941010     C* SE FARE NUOVA BOLLA PER RECUPERO SPESE
479800941010     C*         ------------------------------
479900941010     C* SE HO PROVV. C/A O SPESE GIACENZA E NON HO VARIATO TASSAZ.
480000941010     C* DEVO FARE NUOVA BOLLA
480100941010     C     *IN33         IFEQ      '1'
480200941010     C     *IN35         OREQ      '1'
480300941010     C  N82
480400941010     CANN83              SETON                                        31
480500941010     C                   END
480600960506     C*
480700941010     C     *IN31         IFEQ      '1'
480800941010     C* CREAZ. BOLLA A3
480900941010     C                   MOVEL     'AR'          NWTB1
481000941010     C                   SETON                                        88
481100941010     C                   MOVEL     '  '          NWTB2
481200941010     C     VNCA          IFGT      0
481300941010     C     VVCS          OREQ      'N'
481400941010     C     VASS          ANDGT     0
481500941010     C                   MOVEL     '1'           NWFCA
481600941010     C                   ELSE
481700941010     C                   MOVEL     '0'           NWFCA
481800941010     C                   END
481900941010     C                   MOVEL     '0'           NWFAN
482000941010     C                   MOVEL     '0'           NWFTD
482100941010     C                   MOVEL     *BLANK        NWCBL
482200941010     C                   MOVEL     *BLANK        §LPCBO
482300941010     C                   EXSR      NEWCBL
482400941010     C  N30              MOVEL     NWCBL         §LPCBO
482500941010     C*  LP
482600941010     C                   MOVE      ARBLNA        §LPLNP
482700941010     C* STAMPA SEGNACOLLI  - C. TRATT. MERCE
482800941010     C                   MOVEL     '4 '          §LPCTM
482900941010     C                   MOVE      ARBNCD        §LPNCD
483000941010     C                   MOVE      ARBNCA        §LPNCA
483100941010     C* CONTRASSEGNO
483200941010     C     VNCA          IFGT      0
483300941010     C                   Z-ADD     VNCA          §LPCAS
483400941010     C                   MOVEL     VVCA          §LPVCA
483500991221     C     §LPCAS        IFEQ      0
483600991221     C                   MOVEL     *BLANKS       §LPVCA
483700991221     C                   END
483800941010     C                   MOVEL     AR9TIC        §LPTIC
483900941010     C                   ELSE
484000941010     C     VASS          IFGT      0
484100941010     C     VVCS          ANDEQ     'N'
484200941010     C                   Z-ADD     VASS          §LPCAS
484300000103     C                   MOVEL     VVCAP         §LPVCA
484400991221     C     §LPCAS        IFEQ      0
484500991221     C                   MOVEL     *BLANKS       §LPVCA
484600991221     C                   END
484700941010     C                   MOVEL     AR9TIC        §LPTIC
484800941010     C                   END
484900941010     C                   END
485000941010     C* IMPOSTO §LPNOT
485100941010     C                   EXSR      XLPNOT
485200941010     C*SE ASSEGNATO E RICH. NOTA ACCREDITO IMPOSTO I VALORI VECCH.BOLLA
485300950727     C     EXPORT        IFEQ      'A'
485400950727     C     *IN26         ANDEQ     '1'
485500950727     C     WEXTB1        OREQ      'AS'
485600941010     C                   MOVE      AR6POR        §LPPOR
485700990930      * RIPORTO LE VARIE DELLA VECCHIA BOLLA X DTASV
485800941216     C                   MOVE      AVA           NVA
485900941216     C                   MOVEL     ASV           NSV
486000950522     C                   Z-ADD     1             YY                4 0
486100991001     C* Tolgo Iva e bollo dalle nuove varie
486200991001     C                   Z-ADD     1             YY                4 0
486300991001     C     §$2BOL        LOOKUP    NSV(YY)                                05
486400991001     C     *IN05         IFEQ      '1'
486500991001     C                   MOVEL     *BLANKS       NSV(YY)
486600991001     C                   Z-ADD     0             NVA(YY)
486700991001     C                   END
486800991001     C                   Z-ADD     1             YY                4 0
486900991001     C     §$2IVA        LOOKUP    NSV(YY)                                05
487000991001     C     *IN05         IFEQ      '1'
487100991001     C                   MOVEL     *BLANKS       NSV(YY)
487200991001     C                   Z-ADD     0             NVA(YY)
487300991001     C                   END
487400991001     C     §$2VRG        LOOKUP    NSV(YY)                                05
487500950522     C     *IN05         IFEQ      '1'
487600950508     C     NWFCA         IFEQ      '0'
487700950508     C                   MOVEL     *BLANKS       NSV(YY)
487800950508     C                   Z-ADD     0             NVA(YY)
487900950522     C                   ELSE
488000950522      * SE NON HO CALCOLATO LE PROVVIGIONI C/ASS. MA LE DEVE PAGARE IL
488100950522      * DESTINATARIO MI CARICO L'IMPORTO DALLA VECCHIA BOLLA
488200950522     C     VRCA          IFEQ      0
488300950522     C     VPPC          ANDEQ     'D'
488400950522     C                   Z-ADD     NVA(YY)       VRCA
488500950522     C                   END
488600950522     C                   END
488700950508     C                   END
488800941010     C* RICH. NOTA ACCR. SOLO SE FIL.ARRIVO
488900941010     C     AR6FIV        IFEQ      ARBLNA
489000950727     C     ARBICA        ANDNE     'Y'
489100941010     C                   Z-ADD     AR6IMV        §INAC1
489200941010     C                   SETON                                        32
489300941010     C                   END
489400941010     C                   END
489500941010     C                   END
489600941010     C                   ENDSR
489700941010     C*----------------------------------------------------*
489800941010     C*     R I C O N S E G N A - SI CAMBIO PORTO
489900941010     C*----------------------------------------------------*
490000941010     C     RICSIC        BEGSR
490100941010     C*  ADDEBITO SPESE GIACENZA AL DESTINAT.
490200941010     C     VPSG          IFEQ      'D'
490300941010     C                   SETON                                        33
490400941010     C                   SETON                                        31
490500941010     C                   END
490600941010     C*  CALCOLO PROVV. C/ASSEGNO SE PAGA DESTINAT.
490700941010     C     VAPC          IFEQ      'S'
490800941010     C     VPPC          ANDEQ     'D'
490900941010     C                   EXSR      TARPCA
491000941010     C* VARIATO C/ASS.
491100941010     C     VVCS          IFEQ      'S'
491200941010     C                   Z-ADD     VNCA          §CAS
491300941010     C                   ELSE
491400941010     C                   Z-ADD     VASS          §CAS
491500941010     C                   END
491600941010     C     VVCAP         IFEQ      VVCA
491700941010     C                   MOVEL     'V'           WCMB
491800941010     C                   ELSE
491900941010     C                   MOVEL     'N'           WCMB
492000941010     C                   END
492100941010     C                   EXSR      CASLIR
492200941010     C     §ASS          IFNE      *ZEROS
492300941010     C                   EXSR      CPROVV
492400941010     C                   MOVE      §RCA          VRCA
492500941010     C     VRCA          IFGT      0
492600941010     C                   SETON                                        35
492700941010     C                   SETON                                        31
492800941010     C                   END
492900941010     C                   END
493000941010     C                   END
493100941010     C* RICHIEDERE IL TRASPORTO SE NUOVO PORTO = A
493200941010     C     VNPC          IFEQ      'A'
493400941010     C                   SETON                                        31
493500941010     C                   END
493600941010     C* VARIATO C/ASS.    SEMPRE VARIAZ. X GEST. C/A
493601160804     c                   exsr      srdsk
503600950727     C*  SE NON HO SPESE DI GIACENZA CONTROLLO SE LE AVEVO ADDEBITATE
503700950727     C*  NEI GIRI PRECEDNETNI X ANDARE A STORNARLE
503800950727     C                   MOVEL     ' '           WNOSPG            1
503900950727     C     *IN33         IFEQ      '0'
504000950727     C     §$2VRI        LOOKUP    VSV                                    21
504100950727     C   21              MOVEL     'S'           WNOSPG
504200950727     C                   END
504300941010     C*  AGGIUNTO CONTRASSEGNO  FORZO VARIAG=0 SU VECCHIA BOLLA
504400941010     C     VNCA          IFGT      *ZEROS
504500941010     C     §3AFCA        ANDEQ     0
504600941010     C     EXPORT        ANDEQ     'A'
504700950727     C     EXPORT        OREQ      'A'
504800950727     C     WNOSPG        ANDEQ     'S'
504900941010     C     AR6NFT        IFEQ      0
505000941010     C                   MOVEL     'T0'          §CDVAR
505100941010     C                   ELSE
505200941010     C                   MOVEL     'T2'          §CDVAR
505300941010     C                   END
505400941010     C                   EXSR      CNTVAR
505500950727     C*  SE BOLLA ORIGINALE E' UN ASSEGNATO STORNO NON POSSO VARIARE
505600950727     C*  TASSAZIONE MA DEVO FARE UNA NUOVA BOLLA
505700950727     C  N26WEXTB1        IFEQ      'AS'
505800950727     C                   SETON                                        26
505900950727     C                   END
506000950727     C  N27WEXTB2        IFEQ      'AP'
506100950727     C                   SETON                                        27
506200950727     C                   END
506300950727     C*  CONTROLLO SE ESISTE UNA FIGLIA
506400950727     C  N26
506500950727     CANN27KBOL          CHAIN     FNLBL02L                           29
506600950727     C     *IN29         IFEQ      '0'
506700991001     C     AR6NFT        IFNE      *ZEROS
506800991001     C     AR6TRC        ANDEQ     '2'
506900950727     C                   SETON                                        27
507000950727     C                   ELSE
507100950727     C                   SETON                                        26
507200950727     C                   END
507300950727     C                   END
507400941010     C  N26
507500941010     CANN27              MOVEL     §CDVAR        §§TCVB
507600941010     C  N26
507700941010     CANN27              SETON                                        82
507800941010     C*
507900941010     C   82              DO
508000941216     C* CARICO LE VARIE ESISTENTI X DSARBT
508100941216     C                   MOVE      AVA           VVA
508200941216     C                   MOVEL     ASV           VSV
508300941010      * CERCO SE C'è GIA VARIA G
508400941216     C                   MOVEL     §$2VRI        §SIGLA            1
508500941216     C                   MOVEL     VSV           WSV
508600941216      * CERCO L'INDICE SU CUI LAVORARE
508700941216     C                   EXSR      CHKVAR
508800941216     C                   Z-ADD     0             VVA(XS)
508900941216     C                   MOVEL     §$2VRI        VSV(XS)
509000941216     C                   END
509100941010     C                   END
509200941010     C* IMPOSTO I CAMPI X NUOVA BOLLA
509300941010     C* -----------------------------
509400941010     C* CREAZ. BOLLA AR
509500941010     C     *IN31         IFEQ      '1'
509600941010     C                   MOVEL     'AR'          NWTB1
509700941010     C                   SETON                                        88
509800941010     C                   ELSE
509900941010     C* SE NUOVO  PORTO = F :
510000941010     C     VNPC          IFEQ      'F'
510100941010     C*              SE NO GIACENZE  CREO BOLLA LEGATA SEMPRE
510200941010     C     $GIAC         IFEQ      '0'
510300941010     C                   SETON                                        8831
510400941010     C                   MOVEL     'FC'          NWTB1
510500941010     C                   ELSE
510600941010     C*              SE SI GIACENZE  CREO BOLLA LEGATA X N.ACCR.
510700941010     C     *IN32         IFEQ      '1'
510800941010     C     *IN37         OREQ      '1'
510900941010     C                   SETON                                        8831
511000941010     C                   MOVEL     'FC'          NWTB1
511100941010     C                   END
511200941010     C                   END
511300941010     C                   END
511400941010     C                   END
511500941010     C                   MOVEL     '  '          NWTB2
511600941010     C     VNCA          IFGT      0
511700941010     C     VVCS          OREQ      'N'
511800941010     C     VASS          ANDGT     0
511900941010     C                   MOVEL     '1'           NWFCA
512000941010     C                   ELSE
512100941010     C                   MOVEL     '0'           NWFCA
512200941010     C                   END
512300941010     C                   MOVEL     '0'           NWFAN
512400941010     C                   MOVEL     '0'           NWFTD
512500941010     C                   MOVEL     *BLANK        NWCBL
512600941010     C                   MOVEL     *BLANK        §LPCBO
512700941010     C                   EXSR      NEWCBL
512800941010     C  N30              MOVEL     NWCBL         §LPCBO
512900941010     C*
513000941010     C                   SETON                                        46
513100941010     C                   MOVEL     ARBRSD        VRSD
513200941011     C                   MOVEL     AR4RD2        VRD2
513300941010     C                   MOVEL     ARBIND        VIND
513400941010     C                   MOVEL     ARBCAD        VCAD
513500941010     C                   MOVEL     ARBPRD        VPRD
513600941010     C                   MOVEL     ARBLOD        VLOD
513700961203     C                   MOVEL     ARBNZD        VNZD
513800031119     c* imposto dati referente destinatario
513900031119     c                   movel     §ar5teld      vteld
514000031119     c                   movel     §ar5ref       vref
514100960124     C*  Salvo dati anagrafici destinatario x controllare se devo
514200060213     C*  scrivere FiAR4 x variazione
514300960124     C                   MOVEL     ARBRSD        WSVRSD
514400960124     C                   MOVEL     AR4RD2        WSVRD2
514500960124     C                   MOVEL     ARBIND        WSVIND
514600960124     C                   MOVEL     ARBCAD        WSVCAD
514700960124     C                   MOVEL     ARBPRD        WSVPRD
514800960124     C                   MOVEL     ARBLOD        WSVLOD
514900961203     C                   MOVEL     ARBNZD        WSVNZD
515000941010     C*  LP
515100941010     C                   MOVE      ARBLNA        §LPLNP
515200941010     C* C/ASS
515300941010     C     VNCA          IFGT      0
515400941010     C                   Z-ADD     VNCA          §LPCAS
515500941010     C                   MOVEL     VVCA          §LPVCA
515600991221     C     §LPCAS        IFEQ      0
515700991221     C                   MOVEL     *BLANKS       §LPVCA
515800991221     C                   END
515900941010     C                   MOVEL     AR9TIC        §LPTIC
516000941010     C                   ELSE
516100941010     C     VVCS          IFEQ      'N'
516200941010     C     VASS          ANDGT     0
516300941010     C                   Z-ADD     VASS          §LPCAS
516400941010     C                   MOVEL     VVCAP         §LPVCA
516500941010C    C                   MOVEL     AR9TIC        §LPTIC
516600941010     C                   END
516700941010     C                   END
516800941010     C*
516900941010     C                   MOVEL     '4 '          §LPCTM
517000941010     C                   MOVE      ARBNCD        §LPNCD
517100941010     C                   MOVE      ARBNCA        §LPNCA
517200941010     C* IMPOSTO §LPNOT
517300941010     C                   EXSR      XLPNOT
517400941010     C                   ENDSR
517500911026     C*----------------------------------------------------*
517600911026     C*
517700940310     C* DIROT1 - DIROTTAMENTO 1 OPERAZ. DA FARE
517800911026     C*
517900911026     C*----------------------------------------------------*
518000940310     C     DIROT1        BEGSR
518100911028     C*  NON PREVISTO ADDEBITO SPESE GIACENZA AL DESTINAT.
518200911028     C*
518300941217     C* DAL NUOVO RELEASE FEBR 95:
518400941217     C* SI DIROTTA SOLO SE LA LINEA ARRIVO è DIVERSA
518500941217     C*
518600911028     C* DEVO RICH. LNA E DESTINATARIO NUOVO
518700911028     C                   SETON                                        46
518800920522     C* PROPONGO VECCHIO DESTINATARIO SENZA IL CAP PER I
518900920522     C* DIROTTAMENTI NON DA GIACENZE
519000940310     C     $GIAC         IFEQ      '0'
519100920522     C                   MOVEL     ARBRSD        VRSD
519200941011     C                   MOVEL     AR4RD2        VRD2
519300920522     C                   MOVEL     ARBIND        VIND
519400920522     C                   MOVEL     ARBPRD        VPRD
519500920522     C                   MOVEL     ARBLOD        VLOD
519600940929     C                   MOVEL     ARBNZD        VNZD
519700031119     c* imposto dati referente destinatario
519800031119     c                   movel     §ar5teld      vteld
519900031119     c                   movel     §ar5ref       vref
520000920522     C                   END
520100960124     C*  Salvo dati anagrafici destinatario x controllare se devo
520200060213     C*  scrivere FiAR4 x variazione
520300960124     C                   MOVEL     ARBRSD        WSVRSD
520400960124     C                   MOVEL     AR4RD2        WSVRD2
520500960124     C                   MOVEL     ARBIND        WSVIND
520600960124     C                   MOVEL     ARBCAD        WSVCAD
520700960124     C                   MOVEL     ARBPRD        WSVPRD
520800960124     C                   MOVEL     ARBLOD        WSVLOD
520900960124     C                   MOVE      ARBNZD        WSVNZD
521000911113     C* IMPOSTO IL CODICE FINE GIACENZA
521100941217     C     $GIAC         IFEQ      '1'
521200911113     C                   MOVEL     '006'         VCFG
521300911114     C                   MOVEL     '2C'          TBLCOD
521400911114     C                   MOVEL     *BLANK        TBLKEY
521500911114     C                   MOVEL     VCFG          TBLKEY
521600911114     C     KTAB          CHAIN     TABEL                              21
521700911114     C   21              MOVEL     *ALL'?'       DCFG
521800060410     C  N21              MOVEL     TBLUNI        Ds2c
521900060410     C  N21              MOVEL     §2cdes        DCFG
522000941229     C                   MOVE      GMAUDT        VDCG
522100941217     C                   ENDIF
522200911026     C                   ENDSR
522300940929      *--------------------------------------------------------
522400940929      * C A S L I R  TRASFORMO C/ASSEGNO IN LIRE
522500940929      *------------
522600940929      * VIENE PASSATO §CAS : IMPORTO CONTRASSEGNO 13,3
522700940929      *               WCMB : SE=V DEVO PRENDERE CAMBIO SULLA BOLLA
522800940929      *                      SE=N è CAMBIATA LA DIVISA CERCO NUOVO CAMBIO
522900940929      * VIENE RESTITUITO §ASS : IMPORTO CONTRASSEGNO IN LIRE 13,0
523000940929      *--------------------------------------------------------
523100940929     C     CASLIR        BEGSR
523200011106      * RICHIAMO YEURCO SE LA DIVISA E' CAMBIATA PER APPLICARE
523300011106      * IL CAMBIO CORRETTO
523400011121     C     VVCA          IFNE      *BLANK
523500011121     C     VVCA          ANDNE     VVCAP
523600040206      *chiodissimo per divise a blank
523700040206     c                   if        vvcap = *blank
523800040206     c                   move      'EUR'         vvcap
523900040206     c                   endif
524000011106     C                   CLEAR                   YEURDS
524100011106     C                   MOVEL     VVCAP         YECDVI
524200011106     C                   Z-ADD     §CAS          YECIMI
524300011106     C                   MOVEL     VVCA          YECDVO
524400011106     C                   MOVEL     '3'           YECDCO
524500011106     C                   MOVEL     'H'           YECTAR
524600011106     C                   CALL      'YEURCO'
524700011106     C                   PARM                    YEURDS
524800011106     C                   Z-ADD     YECIMO        §CAS
524900011106     C                   END
525000011106      * RICHIAMO YEURCO SEMPRE PER TRADURRE L'IMPORTO IN MONETA DI
525100011106      * CONTO GESTIONALE
525200011106     C                   CLEAR                   YEURDS
525300011106     C                   MOVEL     VVCA          YECDVI
525400011106     C                   Z-ADD     §CAS          YECIMI
525500011106     C                   MOVEL     §GEDCN        YECDVO
525600011106     C                   MOVEL     '3'           YECDCO
525700011106     C                   MOVEL     'H'           YECTAR
525800011106     C                   CALL      'YEURCO'
525900011106     C                   PARM                    YEURDS
526000011106     C                   Z-ADD     YECIMO        §ASS
526100940929     C                   ENDSR
526200940308     C/EJECT
526300940308     C*?
526400940308     C*?    A G G I O R N A M E N T I
526500940308     C*?
526600940303      *--------------------------------------------------------
526700940303     C* S R A G G  -AGGIORNAMENTI  ARCHIVI
526800940303      *--------------------------------------------------------
526900940303     C     SRAGG         BEGSR
527000940303      *--------------------------------------------------------
527100940303     C* ADDEBITO SPESE E PASSAGGIO DATI PER CREAZ. BOLLA
527200940303      *--------------------------------------------------------
527300951214     C     *IN31         IFEQ      '1'
527400951214     C                   EXSR      PGNBLP
527500951214     C                   Z-ADD     §LPAAS        WSVAAS
527600951214     C                   Z-ADD     §LPLNP        WSVLNP
527700951214     C                   Z-ADD     §LPNRS        WSVNRS
527800951214     C                   Z-ADD     §LPNSP        WSVNSP
527900951214     C                   Z-ADD     §LPLNA        WSVLNA
528000940303     C* SE NON HO CREATO LA BOLLA ERRORE
528100951214     C     §LPNSP        IFEQ      0
528200940307     C                   SETON                                            99
528300940307     C   99              GOTO      ENDAGG
528400940307     C                   END
528500951214     C                   END
528600940303      *--------------------------------------------------------
528700940303     C* AGGIORNAMENTO VECCHIA BOLLA
528800940303      *--------------------------------------------------------
528900141210     c     rileggi       tag
529000141210     C     KBOL          CHAIN(e)  FNARB01L                           43
529100141210     c                   if        %error
529200141210     C     Kbol          CHAIN(n)  fnarb01l
529300141210     c                   exsr      geslckARB
529400141210     c   43              goto      rileggi
529500150129     C  n43KBOL          CHAIN     FNARB01L                           43
529600141210     c                   endif
529700941003     C     *IN43         IFEQ      '0'
529800960124      *--------------------------------------------------------
529900110427      * verifiche per resi
530000110427     C     VFRI          IFeq      'R'
530100030128      *controllo se la spedizione gestisce i bancali
530200030128      * se sono gestiti reperisco da fiar5 i rekord da scrivere per
530300030128      * legarli alla bolla di reso
530400030128     C                   clear                   dar5ban
530500030805     C                   move      'BAN'         trcar5
530600051013     C     Kfiar5        CHAIN(n)  FiAR501L                           43
530700030128     C     *IN43         IFEQ      '0'
530800030128     c                   clear                   wfmgb000
530900030128     C                   MOVEL     ar5uni        dar5ban
531000030128     c                   z-add     amgudt        wfmdat
531100030128     c                   movel     whhudt        wfmora
531200030128     c                   movel     knmus         wfmute
531300030128     c                   movel     simpou        wfmpou
531400030128     c                   movel(p)  'FNLG20R'     wfmpgm
531500030213     c                   movel     'REME'        wfmcau
531600030218     c                   Clear                   wfmsmc
531700030128     c                   movel     arbnsp        wfmrdo
531800030128     c                   movel     arbaas        wfmddo
531900030128     c                   move      arbmgs        wfmddo
532000030128      *  TIPO PORTO
532100030128     C                   movel     '3A'          tblcod
532200030128     C                   movel(P)  ARBcbo        tblkey
532300060807     C     ktab          chain     TABEL
532400030128     C                   movel     TBLUNI        DS3A
532500030128     C                   movel     §3ATB1        §DCTport          1
532600030128      *verifico il porto per codice cliente
532700030128     c                   if        §dctport = 'F'
532800030218     c                   movel     '0'           wfmsog
532900030218     c                   move      arbksc        wfmsog
533000030128     c                   else
533100030128     c                   if        arbccm > 0
533200030218     c                   movel     '0'           wfmsog
533300030218     c                   move      arbccm        wfmsog
533400030128     c                   else
533500030128     c                   move      *all'0'       arblna4           4
533600030128     c                   move      arblna        arblna4
533700030218     c                   eval      wfmsog  = arblna4 + '8888'
533800030128     c                   endif
533900030128     c                   endif
534000030128      *primo rekord se deve essere scritto
534100030128     c                   if        §ar5tba > 0
534200030128     c                   movel     §ar5tba       wfmcod
534300030128     c                   z-add     §ar5nba       wfmqta
534400030128     c                   write     wfmgb000
534500030128     c                   endif
534600030128      *
534700030128      *secondo rekord se deve essere scritto
534800030128     c                   if        §ar5tb2 > 0
534900030128     c                   movel     §ar5tb2       wfmcod
535000030128     c                   z-add     §ar5nb2       wfmqta
535100030128     c                   write     wfmgb000
535200030128     c                   endif
535300030128      *
535400030128     c                   endif
535500000214     C                   END
535600071025      * DA FUORI GIACENZE
535700071017      * scrive rekord spia per resi e dirottamenti per poterli stampare
535800071017      * in seguito abbinando bolla mamma e figlia
535900071025     c                   if        not *in90
536000071017     c                   if        vfri = 'D' or vfri = 'R'
536100071017     c                   move      vfri          arbtpl
536200071017     C                   Z-ADD     0             ARBDST
536300071017     C                   Z-ADD     FILARR        ARBFGS
536400071017     C                   MOVE      AMGUDT        ARBDDC
536500071017     C                   MOVE      *ALL'9'       ARBNDC
536600071017     c                   write     fnarbx00
536700071017     c                   clear                   arbtpl
536800071017     c                   endif
536900071025     c                   endif
537000960124      *--------------------------------------------------------
537100950331     C                   MOVEL     ARBCBO        EXCBO             2
537200941003     C* SE HO GIà RICH. NOTA ACCRED. NON DEVO STAMPARE RICH.
537300941005     C     ARBFIP        IFEQ      'C'
537400941003     C                   MOVEL     ARBFIP        EXNAC             1
537500941003     C                   END
537600941003     C   32
537700941003     COR 37              MOVEL     'C'           ARBFIP
537800941003     C   32
537900941003     COR 37              MOVEL     'S'           ARBFNC
538000941003     C* SE HO RICH.NOTA ACCRED.  O TRASF. IN C/S
538100950726     C     *IN32         IFEQ      '1'
538200950726     C     *IN37         OREQ      '1'
538300950726     C     *IN39         OREQ      '1'
538400950726     C                   MOVEL     'Y'           ARBICA
538500950726     C                   MOVEL     'S'           ARBACA
538600950726     C                   END
538700941003     C     VFRI          IFEQ      'R'
538800960327     C*  Se reso controllo se per avaria
538900960327     C     V4CRCS        IFEQ      'N'
539000941003     C                   MOVEL     '2'           ARBCCA
539100960327     C                   ELSE
539200960327     C                   MOVEL     '6'           ARBCCA
539300941003     C                   END
539400960327     C                   END
539500941003     C     VFRI          IFEQ      'D'
539600941003     C                   MOVEL     '1'           ARBCCA
539700941003     C                   END
539800941003     C     VFRI          IFEQ      'C'
539900941003     C     VNPC          ANDNE     ' '
540000941003     C                   MOVEL     '9'           ARBCCA
540100941003     C                   END
540200940303     C*  --------------------------->  HO FATTO NUOVA BOLLA
540300940303      * =======================================================
540400941229      * DO PER CONSEGNATA LA MAMMA - X RESO E DIROTTAMENTO
540500940303      * =======================================================
540600940303     C     VFRI          IFEQ      'R'
540700940303     C     VFRI          OREQ      'D'
540800941216     C     VLNA          ANDNE     EXLNA
540900950214     C* Se non c'è data arrivo forzo = data consegna
541000950214     C     ARBDAM        IFEQ      *ZEROS
541100950214     C                   Z-ADD     AMGUDT        ARBDAM
541200950214     C     ARBDTI        IFEQ      *ZEROS
541300950214     C                   Z-ADD     AMGUDT        ARBDTI
541400950214     C                   Z-ADD     ORAMIN        ARBHTI
541500950214     C                   END
541600950214     C                   END
541700940303      *
541800940303     C                   MOVEL     'Y'           ARBICC
541900950725     C                   MOVEL     'S'           ARBACC
542000940303     C                   MOVEL     'Y'           ARBICA
542100950725     C                   MOVEL     'S'           ARBACA
542200940303     C                   MOVE      AMGUDT        ARBDCM
542300940930     C                   MOVEL     ORAMIN        ARBHMC
542400940303     C                   MOVE      *BLANK        ARBFT1
542500941006     C                   MOVE      *ZEROS        ARBDT1
542600091221     c                   clear                   arbpdc
542700021009     C                   MOVEL     FILarr        ARBPDC
542800091221     C                   MOVE      0998          ARBPDC
542900940303     C                   MOVE      *ALL'9'       ARBNDC
543000941003      * LA DATA DELLA DISTINTA è LA DATA DEL DIROTTAMENTO
543100940303     C                   MOVE      AMGUDT        ARBDDC
543200940303     C                   MOVEL     'S'           ARBFDC
543300940303     C                   MOVE      *ALL'9'       ARBNMI
543400940303     C                   MOVE      AMGUDT        ARBDMI
543500940303     C                   MOVE      *ALL'9'       ARBCUC
543600021009     c                   z-add     filarr        arbifp
543700020813     c                   z-add     simpou        arbipp
543800940303     C                   MOVEL     *BLANK        ARBFBC
543900950104     C                   MOVEL     *BLANK        ARBCMC
544000970204     C* SE DIROTTAMENTO AD ALTRA FILIALE IMPOSTO ARBDT3 A 99999999
544100970204     C* E ARBFT3 A T
544200970204     C     VFRI          IFEQ      'D'
544300970204     C                   MOVEL     'T'           ARBFT3
544400970204     C                   Z-ADD     99999999      ARBDT3
544500970204     C                   END
544600940303      *
544700941011     C* DETT.SEGNACOLLI E ANOMALIE NELLO STORICO
544800940303     C                   EXSR      AGGART
544900940303      *
545000941229     C                   UPDATE    FNARB000
545100070320      * se impostato flag abilitazione in anagrafica verifico se esistono
545200070321      * dei blocchi nel caso updato il rekord dopo aver chiuso giacenza
545300070320     c                   if        §oggeot = 'S'
545400070321     c                   exsr      elibloc
545500070320     c                   endif
545600941229     C                   ELSE
545700940303      * ========================================================
545800940303      * IN TUTTI GLI ALTRI CASI NON STORICIZZO LA MAMMA
545900940303      * ========================================================
546000941229     C* SE HO CREATO LA BOLLA IN ARRIVO VADO IN CONSEGNA CON LA NUOVA
546100941229     C     *IN88         IFEQ      '1'
546200941229     C* BLOCCO LA VECCHIA BOLLA :
546300941229     C* SE NON C'E' GIACENZA  PERCHE' NON ESCA
546400941229     C* NELLA PRELISTA BOLLE DA CONSEGNARE
546500941003     C     $GIAC         IFEQ      '0'
546600940310     C                   MOVEL     '*'           ARBFBC
546700941229     C* SE C'E' GIACENZA SE NON E' BLOCCATA LA BLOCCO
546800941229     C                   ELSE
546900941229     C                   MOVEL     'G'           ARBFBC
547000940310     C                   END
547100940308     C* ABILITO LA VECCHIA BOLLA  PERCHE' NON ESCA  DA TASSARE     ££££
547200940308     C                   MOVEL     'S'           ARBACC
547300940308     C                   MOVEL     'S'           ARBACA
547400941229     C                   UPDATE    FNARB000
547500941229     C                   ELSE
547600941229      *                    ----
547700940303     C* SE NON HO CREATO NUOVA BOLLA X ANDARE IN CONSEGNA
547800941229     C* DEVO ABILITARE LA VECCHIA SE NON HA FIGLIE
547900941229     C     KBOL          CHAIN     FNLBL02L                           21
548000941229     C     *IN21         IFEQ      *ON
548100941003     C                   MOVEL     *BLANK        ARBFBC
548200941229     C                   UPDATE    FNARB000
548300941229     C                   ELSE
548400941229     C* SE HA FIGLIE SBLOCCO L'ULTIMA
548500941229     C     *IN21         DOWEQ     *OFF
548600941229     C     LBLLAN        IFEQ      EXLNA
548700941229     C                   MOVE      LBLAAN        ULFAAS
548800941229     C                   MOVE      LBLLPN        ULFLNP
548900941229     C                   MOVE      LBLNRN        ULFNRS
549000941229     C                   MOVE      LBLNSN        ULFNSP
549100941229     C                   END
549200941229     C     KBOL          READE     FNLBL02L                               21
549300941229     C                   ENDDO
549400941229     C     ULFNSP        IFNE      *ZEROS
549500941229     C* BLOCCO LA VECCHIA BOLLA
549600941229     C     $GIAC         IFEQ      '0'
549700941229     C                   MOVEL     '*'           ARBFBC
549800941229     C                   ELSE
549900941229     C                   MOVEL     'G'           ARBFBC
550000941229     C                   END
550100941229     C                   UPDATE    FNARB000
550200941229     C* SBLOCCO L'ULTIMA FIGLIA
550300941229     C     KBOLF         CHAIN     FNARB01L                           21
550400941229     C  N21              MOVEL     *BLANK        ARBFBC
550500960507     C* SE HO AGGIUNTO IL C/ASSEGNO E' HO GIA' UNA FIGLIA FACCIO
550600960507     C* NUOVA BOLLA
550700960507     C     *IN31         IFEQ      '0'
550800960507     C     VNCA          ANDGT     0
550900960507     C     VASS          ANDEQ     0
551000960507     C     *IN21         ANDEQ     '0'
551100960507     C     ARBDCM        ANDEQ     *ZEROS
551200960507      * GUARDO SE è UN ASSEGNATO
551300960507     C                   MOVEL     '3A'          TBLCOD
551400960507     C                   MOVEL     *BLANK        TBLKEY
551500960507     C                   MOVEL     ARBCBO        TBLKEY
551600160805     C     KTAB          CHAIN     TABEL
551700160805     C                   if        %found(tabel00f)
551800960507     C                   MOVEL     TBLUNI        DS3A
551900960507     C                   MOVEL     §3ATB1        NWTB1
552000960507     C                   MOVEL     §3ATB2        NWTB2
552100960507     C                   MOVEL     '1'           NWFCA
552200960507     C                   MOVEL     §3AFAN        NWFAN
552300960507     C                   MOVEL     §3AFTD        NWFTD
552400960507     C                   EXSR      NEWCBL
552500960507     C  N30              MOVEL     NWCBL         ARBCBO
552600960507     C                   SETOFF                                       21
552700960507     C                   END
552800960507     C                   END
552900941229     C  N21              UPDATE    FNARB000
553000941229     C                   ELSE
553100941229     C* SE NON HO TROVATO FIGLIE DA SBLOCCARE
553200941229     C* SBLOCCO LA VECCHIA
553300941229     C                   MOVEL     *BLANK        ARBFBC
553400941229     C                   UPDATE    FNARB000
553500941229     C                   ENDIF
553600941229     C                   ENDIF
553700941229     C                   ENDIF
553800950918     C*  Se rimetto in consegna la bolla chiudo anomalie di giacenza
553900950918     C                   EXSR      CLOANM
554000941229     C                   ENDIF
554100941003     C                   ENDIF
554200941005      *--------------------------------------------------------
554300941217      * DIROTTAMENTO/CAMBIO PORTO  CREO EVENTO  SE NO GIACENZE
554400941005      *--------------------------------------------------------
554500941217     C     $GIAC         IFEQ      '0'
554600941005     C     VFRI          IFEQ      'D'
554700941217     C     VFRI          OREQ      'C'
554800941217     C     VNPC          ANDNE     ' '
554900941005     C                   EXSR      EVENTO
555000941005     C                   ENDIF
555100941217     C                   ENDIF
555200940303      *--------------------------------------------------------
555300940303     C*  PRENDO NR. RICH. RECUPERO   SE NO GIACENZE
555400940303      *--------------------------------------------------------
555500940310     C     $GIAC         IFEQ      '0'
555600940303     C     VFRI          IFEQ      'D'
555700941216     C     §LPLNA        ANDNE     EXLNA
555800940303     C     VNPC          ANDEQ     ' '
555900940303     C     EXPORT        ANDEQ     'F'
556000940303     C* CAMBIO PORTO
556100940303     C     VNPC          OREQ      'F'
556200940303     C*
556400020813     c                   clear                   trul33ds
556500020813     c                   move      'L'           i33tla
556600020813     c                   z-add     15            i33cnu
556700021009     c                   z-add     filarr        i33po1
556800020813     c                   z-add     1             i33num
556900020813     c                   movel     trul33ds      kpjbu
557000020813     c                   call      'TRUL33R'
557100020813     c                   PARM                    kpjba
557200020813     c                   movel     kpjbu         trul33ds
557300020813     c                   z-add     o33nri        §rirec            7 0
557500940303     C                   END
557600940303     C                   END
557700951213      *--------------------------------------------------------
557800951213     C* CREAZ. LEGAME BOLLA CREATA
557900951213      *--------------------------------------------------------
558000951213      * SE HO CREATO NUOVA BOLLA LA LEGO SEMPRE !!!!!
558100951213     C     *IN31         IFEQ      '1'
558200951213     C                   EXSR      LEGBOL
558300951213     C* METTO NELLE NOTE CONSEGNA LA NUOVA SPEDIZIONE
558400951213     C                   EXSR      AGGAR4
558500030826     C* per bolle con particolarità RC (orm contestuale alla consegna)
558600030826     c* chiudo l'orm della bolla originale e aggiorno il po di ritirno con
558700030826     c* il po d'arrivo della bolla originale in modo da far creare in
558800030826     c* automatico il nuovo orm
558900030826     C                   EXSR      AGGORM
559000951213     C                   END
559100951213     C*-------------------------------------------------------
559200951213     C* BOLLA DA CREARE ANCHE IN ARRIVO PER ANDARE IN CONSEGNA
559300951213     C*-------------------------------------------------------
559400951213     C     *IN88         IFEQ      '1'
559500951213     C                   EXSR      ARRBOL
559600951213     C                   ELSE
559700951213     C*  Se non devo creare una nuova bolla controllo se devo
559800951213     C*  ristampare la vecchia (l.avviso) in caso di riconseg.
559900951213     C     VFRI          IFEQ      'C'
560000070412      * procedura di automazione distinte non si deve più stampare la LDV
560100070412     c                   if        §ogdda = ' ' or
560200070424     c                             §ogdda = '20391231' or amgudt < ogddan
560300951213     C                   EXSR      CTRSTB
560400070412     C                   endif
560500951213     C                   END
560600951213     C                   END
560700951213     C*-------------------------------------------------------
560800951213     C* BOLLA NON CREATA IN ARRIVO, MA LINEA ARRIVO è = DEVO STAMPARLA
560900951213     C*-------------------------------------------------------
561000951213     C   31§LPLNA        IFEQ      EXLNA
561100951213     C     *IN88         ANDEQ     '0'
561200951213     C                   EXSR      STABLP
561300951213     C                   END
561400940303      *--------------------------------------------------------
561500940303     C*STAMPO  RICHIESTA RECUPERO
561600940303      *--------------------------------------------------------
561700951212     C     $GIAC         IFEQ      '0'
561800940303     C     VFRI          IFEQ      'D'
561900941216     C     §LPLNA        ANDNE     EXLNA
562000940303     C     VNPC          ANDEQ     ' '
562100940303     C     EXPORT        ANDEQ     'F'
562200940303     C                   SETON                                        84
562300940303     C                   END
562400940303     C* CAMBIO PORTO
562500940303     C     VNPC          IFEQ      'F'
562600951212     C     WEXTB1        OREQ      'FS'
562700951212     C     VNPC          ANDEQ     ' '
562800960126     C     VFRI          IFEQ      'C'
562900960126     C                   SETON                                        91
563000960126     C                   ELSE
563100960126     C                   SETON                                        85
563200940303     C                   END
563300960126     C                   END
563400941221     C* RECUPERO PROVV. C/ASS SE AGGIUNTO C/ASSEGNO E PAGA MITTENTE
563500941221     C     VVCS          IFEQ      'S'
563600941221     C     VNCA          ANDGT     0
563700941221     C     VASS          ANDEQ     0
563800941221     C     VPPC          ANDEQ     'M'
563900941221     C                   SETON                                        89
564000941221     C                   ENDIF
564100960126     C   84
564200960126     COR 85              EXSR      RCHREC
564300951212     C*  Per riconsegne con cambi di porto da assegnato a franco
564400951212     C*  creo una nuova bolla x recupero spese trasporto x lnp
564500960126     C   91              EXSR      RECTRA
564600030617     c                   if        viddcli = 'C' and vfri = 'D'
564700020617     c                   seton                                        92
564800020617     c                   exsr      rectra
564900020625     c                   setoff                                       92
565000020617     c                   endif
565100960126     C                   SETOFF                                       858489
565200940303     C                   END
565300951212      *--------------------------------------------------------
565400951212     C* RICHIESTA NOTA ACCREDITO
565500951212      *--------------------------------------------------------
565600951212     C   32
565700951212     COR 37EXNAC         IFNE      'C'
565800951212      * STAMPO SOLO SE NON L'HO GIA' RICHIESTA
565900951212     C                   EXSR      RCHNAC
566000951212     C                   END
566100951213     C                   MOVEL     ' '           WRECSP            1
566200960126     C                   SETOFF                                       91
566300940303     C*-------------------------------------------------------
566400940303     C*  AGGIORNA ARCHIVI GIACENZA
566500940303     C*-------------------------------------------------------
566600940303     C     $GIAC         CASEQ     '1'           GCPWTR
566700940304     C                   ENDCS
566800940307     C     ENDAGG        ENDSR
566900070321      *---------------------------------------------------------*
567000070321      *  elimina blocchi alla consegna da bolla data x consegnata
567100070321      *---------------------------------------------------------*
567200070321     c     elibloc       begsr
567300070321     C                   clear                   dsbl4l
567400070803     C     Kar4l         chain(e)  fiar401l
567500070803      *provvisorio da eliminare con test allocazione
567600070803     c                   if        %error
567700070803     C     Kar4l         chain(N)  fiar401l
567800070803     c                   goto      endbloc
567900070803     c                   endif
568000070321     C                   if        %found(fiar401l)
568100070321     C                   movel     ar4not        dsbl4l
568200070321     c                   move      *blank        §b4lbas
568300070321     c                   move      *blank        §b4lbge
568400070321     c                   move      *blank        §b4ltca
568500070321     c                   move      *blank        §b4ltfd
568600070614     C                   movel     dsbl4l        ar4not
568700070614     c                   update    fiar4000
568800070321     C                   endif
568900070321     c
569000070803     C     endbloc       endsr
569100090729      *---------------------------------------------------------*
569200090729      *  scrive contatto in fase di riconsegna se CA non abilitato
569300090729      *---------------------------------------------------------*
569400090729     c     contatto      begsr
569500090729     C                   clear                   dsbl4l
569600090729     C     Kar4l         chain(e)  fiar401l
569700090729      *provvisorio da eliminare con test allocazione
569800090729     c                   if        %error
569900090729     C     Kar4l         chain(N)  fiar401l
570000090729     c                   goto      endcont
570100090729     c                   endif
570200090729     C                   if        %found(fiar401l)
570300090729     C                   movel     ar4not        dsbl4l
570400090729     c                   move      'S'           §b4ltca
570500090729     C                   movel     dsbl4l        ar4not
570600090729     c                   update    fiar4000
570700090729     c                   else
570800090729     c                   eval      ar4aas = arbaas
570900090729     c                   eval      ar4lnp = arblnp
571000090729     c                   eval      ar4nrs = arbnrs
571100090729     c                   eval      ar4nsp = arbnsp
571200090729     c                   eval      ar4trc = 'L'
571300090729     c                   move      'S'           §b4ltca
571400090729     c                   eval      ar4not = dsbl4l
571500090729     c                   eval      ar4ftr = 'R'
571600090729     c                   eval      ar4dtr = amgudt
571700090729     c                   eval      ar4duv = amgudt
571800090729     c                   write     fiar4000                             99
571900090729     C                   endif
572000090729     c
572100090729     C     endcont       endsr
572200070614      *---------------------------------------------------------*
572300070614      *  Duplica eventuali note LDV presenti in bolla
572400070614      *---------------------------------------------------------*
572500070614     c     dupnotldv     begsr
572600070614     C                   move      'LDV'         cdnk
572700070614     C                   move      'S'           gstk
572800070614     C     Karn          setll     fiarn11l
572900070614     c                   do        *hival
573000070614     C     Karn          reade     fiarn11l
573100070614     C                   if        %eof(fiarn11l)
573200070614     c                   leave
573300070614     c                   endif
573400070614     c                   move      §lpaas        arnaas
573500070614     c                   move      §lplnp        arnlnp
573600070614     c                   move      §lpnrs        arnnrs
573700070614     c                   move      §lpnsp        arnnsp
573800070614     c                   write     fiarn000
573900070614     C                   enddo
574000070614     c
574100070614     C                   endsr
574200070622     C**************************************************************************
574300070622     C*  Routine per gestione richiamo interrogazione bolle
574400070622     C**************************************************************************
574500070622     C     richbl        begsr
574800070622      *
574900070622     c                   z-add     arbaas        pa1aas
575000070622     c                   z-add     arblnp        pa1lnp
575100070622     c                   z-add     arbnrs        pa1nrs
575200070622     c                   z-add     arbnsp        pa1nsp
575300070622     c                   movel     ' '           pa1f03
575400070622     c                   movel     '1'           pa1flg
575500070622     c                   movel     param1        kpjbu
575600070622     c                   call      'FNLR36R'
575700070622     c                   parm                    kpjba
575800070622     c                   movel     kpjbu         param1
576100070622      *
576200070622     c                   endsr
576300950918     C*-----------------------------------------------------*
576400950918     C*  Chiudo anomalie 140 su dettaglio segnacolli
576500950918     C*-----------------------------------------------------*
576600950918     C     CLOANM        BEGSR
576700950918     C*
576800950918     C                   Z-ADD     140           KCAA
576900160805     C     KANM1         setll     FNANM01L
577000160805     C                   DO        *hival
577001160805     C     KANM1         READE     FNANM01L
577002160805     c                   if        %eof(fnanm01l)
577003160805     c                   leave
577004160805     c                   end
577100960618     C                   CLEAR                   DSLR33
577200960618     C                   MOVE      '4'           D33FSC
577300960618     C                   Z-ADD     AMGUDT        D33DCH
577400960618     C                   MOVEL     'DI'          D33CCH
577500960618     C                   Z-ADD     ANMNR1        D33NRR
577600960618     C                   CALL      'FNLR33R'
577700960618     C                   PARM                    DSLR33
577900950918     C                   END
578000950918     C*
578100950918     C                   ENDSR
578200940308     C*---------------------------------------------------------------*
578300940309     C*   P G N B L P  RICH. PGM NUOVA BOLLA                          *
578400940308     C*---------------------------------------------------------------*
578500940309     C     PGNBLP        BEGSR
578600971113      *
578700941003      * IMPOSTO COD. CLIENTE MITTENTE
578800950130     C                   MOVE      EXLNA         §LPLNP
578900000313     C                   MOVE      ARBRMN        §LPRMN
579000000925     C                   MOVE      'N'           DPDSN             1
579100000313     C* Se la nuova bolla ha lnp DPD (EX LNA) imposto il nr.parcel
579200000313     C* nel rifermiento mittente numerico
579300150724     C*   QUINDI è dall'estero verso l'Italia
579400000313     C     §LPLNP        CHAIN     AZORG01L                           21
579500000313     C     *IN21         IFEQ      '0'
579600000313     C     ORGFVA        ANDNE     '*'
579700000313     C                   MOVEL     ORGDE3        OG143
579800020617     C     §OGntw        IFEQ      'DPD'
579900000925     C                   MOVE      'S'           DPDSN
580000150630     c                   end
580100150630     c                   endIF
580200150630      *
580300150630     c                   clear                   fnlg24ds
580400150630     c                   z-add     Kaas          i24aas
580500150630     c                   z-add     Klnp          i24lnp
580600150630     c                   z-add     Knrs          i24nrs
580700150630     c                   z-add     Knsp          i24nsp
580800150630     c*
580900150630     c                   call      'FNLG24R'
581000150630     C                   parm                    fnlg24ds
581100150630     c*
581200150630     c                   eval      §lpnpr = o24npr
581300150630     c*
581400150724     c* C'è il PARCEL quindi è un DPD:
581500150724     C* ... per dirottamenti e resi DPD, imposta il COD.trattamento merce
581600150724     C*     specifico x DPD.
581700150630     c                   if         o24npr <> *blank
581800000313     C     VFRI          IFEQ      'D'
581900000313     C     VFRI          OREQ      'R'
582000140711     c                   exsr      srctm2k
582100140711     c                   if        §lpctm<>'2K'
582200000313     C                   MOVEL     '6F'          §LPCTM
582300140711     c                   else
582400140711     c                   movel     '2H'          §LPCTM
582500000313     C                   END
582600140711     C                   END
582700150630     c*
582800151020     c                   endIF
582900151020     c*
583000030131     C* in caso di dirottamento o cambio di porto su riconsegna
583100030131     C*reperisco i bancali e i campi correlati da impostare sulla nuova bolla
583200030131     C                   clear                   dar5ban
583300030131     C                   if        vfri = 'D'  or
583400030131     c                             vfri = 'C' and vnpc <> *blank and
583500030131     c                             vnpc <> export
583600030805     C                   move      'BAN'         trcar5
583700051013     C     Kfiar5        CHAIN(n)  FiAR501L                           43
583800030131     C     *IN43         IFEQ      '0'
583900030131     C                   MOVEL     ar5uni        dar5ban
584000030131     C                   endif
584100030131     c                   z-add     §AR5TBA       §lpTBA
584200030131     c                   z-add     §AR5NBA       §lpNBA
584300030131     c                   z-add     §AR5TB2       §lpTB2
584400030131     c                   z-add     §AR5NB2       §lpNB2
584500030131     C                   endif
584600031124      * Bancali Bartolini
584700031124     c                   If        %Subst(Arbgva:1:1) = 'O' and wrecsp = ' '
584800031124     c                   Clear                   dAr5BNB
584900031124     c                   Clear                   tasban
585000031124     C                   move      'BNB'         trcar5
585100051013     c     kFiar5        Chain(n)  Fiar501l
585200031124     c                   If        %Found(Fiar501l)
585300031124     c                   Movel     Ar5Uni        dAr5BNB
585400031124     c                   EndIf
585500031124     c                   z-add     §AR5bTBA      §lpTBA
585600031124     c                   z-add     §AR5bNBA      §lpNBA
585700031124     c                   z-add     §AR5bTB2      §lpTB2
585800031124     c                   z-add     §AR5bNB2      §lpNB2
585900031124     c                   Eval      §lpcor = §Ar5Bcor
586000031124     c                   Eval      §lpcsf = §Ar5Bcsf
586100031124     c                   EndIf
586200031119      * imposto dati per referente  destinatario  e num telefono
586300031119     C                   if        vfri = 'D'  or
586400031119     c                             vfri = 'C' and vnpc <> *blank and
586500031119     c                             vnpc <> export
586600031119     c                   movel     vref          §lpref
586700031119     c                   movel     vteld         §lptel
586800031119     c                   endif
586900941220     C                   EXSR      AGGCCM
587000940308     C                   MOVEL     AMGUDT        §LPAAS
587100940308     C                   MOVE      AMGUDT        §LPMGS
587200940308     C                   MOVE      *ZEROS        §LPNRS
587300940308     C                   MOVE      *ZEROS        §LPNSP
587400150630      * *****************
587500150630      * RESO___________:
587600150630      * *****************
587700150630      *
587800940308     C     VFRI          IFEQ      'R'
587900941003     C                   MOVE      'S'           §LPFBR
588000950104     C                   MOVEL     *BLANKS       §LPGCA
588100950104     C                   MOVEL     *BLANKS       §LPGGA
588200031125      **---- < ATTENZIONE >  CHIODO PER varia O per i resi
588300031126     c                   If        %Subst(Arbgva:1:1) = 'O'
588400031126     c                   movel     arbgva        §lpgva
588500031126     c                   else
588600030821     C                   MOVEL     *BLANKS       §LPGVA
588700150311     C                   clear                   §lpTBA
588800150311     C                   clear                   §lpNBA
588900150311     C                   clear                   §lpTB2
589000150311     C                   clear                   §lpNB2
589100150311     C                   clear                   §lpcor
589200150311     C                   clear                   §lpcsf
589300031125     c                   endif
589400000925     C                   MOVEL     *BLANKS       §LPGMA
589500000922      *
589600940308     C                   ELSE
589700150630     ******************
589800150630     **  NO RESO______:
589900150630     ******************
590000150630      *
590100950104     C* IMPOSTAZIONE CAMPI GESTIONI VARIE
590200950104     C                   MOVEL     AR9GCA        §LPGCA
590300950104     C                   MOVEL     ARBGGA        §LPGGA
590400030821     C                   MOVEL     ARBGMA        §LPGMA
590500030821     C                   MOVEL     ARBGVA        §LPGVA
590600960205     C*§ ABBLENCO 2°BYTE PARTIC. VARIE
590700960306     C                   MOVE      ' '           §LPGVA
590800110715      * se dirottata bolla di rientro cerca le varie relative alla giacenza prec
590900110715      * le salva per riscriverle sulla bolla nuova solo da fuori giacenza
591000110715     c                   clear                   memovar1         11 3
591100110715     c                   clear                   memovar2         11 3
591200110715     c                   clear                   memovar3         11 3
591300110715     c                   clear                   memova1           1
591400110715     c                   clear                   memova2           1
591500110715     c                   clear                   memova3           1
591600150630     c                   clear                   §LPFBR
591700150511     C* reperisco variabili tipo bolla
591800150511     C                   CLEAR                   DS3A
591900150512     C                   MOVEL     '3A'          TBLCOD
592000150512     C                   MOVEL     *BLANK        TBLKEY
592100150512     C                   MOVEL     §lpCBO        TBLKEY
592200150512     C     KTAB          CHAIN     TABEL                              21
592300150512     C  N21              MOVEL     TBLUNI        DS3A
592400110715
592500940308     C     ARBFBR        IFNE      *BLANK
592600150511     C     §3asva        andeq     *blank
592700940308     C                   MOVE      ARBFBR        §LPFBR
592800150630     c                   if           vfri = 'D' and not *in90
592900110714     c                   if        ar6sv1 = 'I' or ar6sv1 = 'N'
593000110714     c                   z-add     ar6va1        memovar1
593100110715     c                   movel     ar6sv1        memova1
593200110714     c                   endif
593300110714     c                   if        ar6sv2 = 'I' or ar6sv2 = 'N'
593400110714     c                   z-add     ar6va2        memovar2
593500110715     c                   movel     ar6sv2        memova2
593600110714     c                   endif
593700110714     c                   if        ar6sv3 = 'I' or ar6sv3 = 'N'
593800110714     c                   z-add     ar6va3        memovar3
593900110715     c                   movel     ar6sv3        memova3
594000110714     c                   endif
594100110714     c                   endif
594200940308     C                   END
594400961129     C*  Se bolla originale in franco frontiera olla figlia no
594500961129     C     ARBFBR        IFEQ      'F'
594600961129     C                   MOVEL     *BLANKS       §LPFBR
594700961129     C                   END
594800940308     C                   END
594900150630      *****************
595000150630     C* CONSEGNA______:
595100150630      *****************
595200950725     C     VFRI          IFEQ      'C'
595300940308     C                   MOVE      ARBFFD        §LPFFD
595400950725     C                   ELSE
595500950725     C                   MOVE      *BLANKS       §LPFFD
595600950725     C                   END
595700150630      *****************
595800150630      * - X TUTTI_____:
595900150630      *****************
596000940308     C                   MOVE      *BLANK        §LPCPI
596100940308     C                   MOVE      *ZEROS        §LPIMV
596200940308     C                   MOVE      *ZEROS        §LPDFT
596300940308     C                   MOVE      *ZEROS        §LPNFT
596400940308     C                   MOVE      *ZEROS        §LPFIV
596500950120     C                   MOVEL     ARBRMA        §LPRMA
596600941003     C                   MOVEL     ARBRMO        §LPRMO
596700091221     c                   clear                   §LPPDR
596800950118     C                   MOVEL     §LPLNP        §LPPDR
596900091221     C                   MOVE      0998          §LPPDR
597000940308     C                   MOVE      AMGUDT        §LPDRT
597100941003     C                   MOVE      *ZEROS        §LPNRT
597200941011     C                   MOVEL     'P'           §LPFPP
597300050208     C                   MOVE      *blank        §LPXCO
597400150310     C                   IF        §lpcbo <> 'F*' and
597500150310     C                             (vfri = 'C' or vfri = 'D')
597600031110     C                   MOVEL     arbtcr        §LPTCR
597700031110     C                   z-add     arbdcr        §LPDCR
597800031110     C                   z-add     arbhcr        §LPHCR
597900031110     c                   else
598000031110     C                   MOVEL     ' '           §LPTCR
598100940308     C                   MOVE      *ZEROS        §LPDCR
598200940308     C                   MOVE      *ZEROS        §LPHCR
598300031110     c                   endif
598400941221     C* GUARDO SE BOLLA IN C/SERVIZIO AZZERO LE VARIE
598500941221     C                   CLEAR                   DS3A
598600941221     C                   Z-ADD     1             §F                2 0
598700941221     C     §LPCBO        LOOKUP    CDB(§F)                                21
598800941221     C   21              MOVEL     D3A(§F)       DS3A
598900960227     C   21              MOVEL     §3ATB1        WNEWPO            1
599000960227     C   21WNEWPO        IFEQ      'A'
599100960227     C     §3ATB1        ANDNE     'AS'
599200960227     C     §APPO         ANDEQ     8888
599300960227     C                   MOVE      '9999'        §LPCCM
599400960227     C                   MOVE      '9999'        §LPCCN
599500960227     C                   END
599600941221     C     §3ARBL        IFEQ      'C'
599700941221     C                   CLEAR                   NVA
599800941221     C                   CLEAR                   NSV
599900941221     C                   ELSE
600000941216     C* ANTEPORTO
600100941216     C     §ANTEP        IFGT      *ZEROS
600200941216     C                   MOVEL     §$2VRN        §SIGLA            1
600300941216     C                   MOVEL     NSV           WSV
600400941216      * CERCO L'INDICE SU CUI LAVORARE
600500941216     C                   EXSR      CHKVAR
600600941216     C                   Z-ADD     §ANTEP        NVA(XS)
600700941216     C                   MOVEL     §$2VRN        NSV(XS)
600800941216     C                   ENDIF
600900941216     C* SPESE GIACENZA
601000941003     C     §TOTSP        IFGT      *ZEROS
601100941216     C                   MOVEL     §$2VRI        §SIGLA
601200941216     C                   MOVEL     NSV           WSV
601300941216      * CERCO L'INDICE SU CUI LAVORARE
601400941216     C                   EXSR      CHKVAR
601500941216     C                   Z-ADD     §TOTSP        NVA(XS)
601600941216     C                   MOVEL     §$2VRI        NSV(XS)
601700940308     C                   END
601800941003     C* SE RIENTRO E C'E' VARIA R DEVE ESSERE RIPORTATA
601900940308     C     VFRI          IFEQ      'R'
602000941003     C     §VARR         ANDGT     *ZEROS
602100960327     C     V4CRCS        ANDEQ     'N'
602200941216     C                   MOVEL     §$2VRR        §SIGLA
602300941216     C                   MOVEL     NSV           WSV
602400941216      * CERCO L'INDICE SU CUI LAVORARE
602500941216     C                   EXSR      CHKVAR
602600941216     C                   Z-ADD     §VARR         NVA(XS)
602700941216     C                   MOVEL     §$2VRR        NSV(XS)
602800941003     C                   END
602900941003     C* PROVV. C/ASSEGNO NELLA  VARIA G
603000940308     C     VRCA          IFGT      *ZEROS
603100941216     C                   MOVEL     §$2VRG        §SIGLA
603200941216     C                   MOVEL     NSV           WSV
603300941216      * CERCO L'INDICE SU CUI LAVORARE
603400941216     C                   EXSR      CHKVAR
603500941216     C                   Z-ADD     VRCA          NVA(XS)
603600941216     C                   MOVEL     §$2VRG        NSV(XS)
603700940308     C                   ELSE
603800940308     C* SE C'E C/ASSEGNO MA NON CI SONO PROVV. FORZO =0
603900941229     C* SE CREATA BOLLA IN ARRIVO
604000941229     C     §LPCAS        IFGT      0
604100941229     C     *IN88         ANDEQ     *ON
604200941229     C                   MOVEL     §$2VRG        §SIGLA
604300941229     C                   MOVEL     NSV           WSV
604400941216      * CERCO L'INDICE SU CUI LAVORARE
604500941229     C                   EXSR      CHKVAR
604600941229     C                   MOVE      *ZEROS        NVA(XS)
604700941229     C                   MOVEL     §$2VRG        NSV(XS)
604800941229     C                   END
604900940308     C                   END
605000941221     C                   ENDIF
605100941216     C* TRASPORTO
605200940308     C     VRTR          IFGT      *ZEROS
605300990721     C                   Z-ADD     VRTR          §LPPOR
605400940308     C                   END
605500971003     C* Se non devo recuperare le spese di trasporto imposto il totale
605600971003     C* imponibile
605700971003     C     EXPORT        IFEQ      'F'
605800971003     C     WEXTB1        ANDNE     'FS'
605900971003     C     VNPC          ANDNE     'A'
606000971013     C     VFRI          ANDNE     'R'
606100991001     C* Scorro le varie e le sommo nel totale imponibile solo se il
606200991001     C* flag tab cc lo prevede
606300991001     C                   DO        20            WW                3 0
606400991001     C                   Z-ADD     1             XW                3 0
606500991001     C     NSV(WW)       LOOKUP    CCC(XW)                                20
606600991001     C     *IN20         IFEQ      '1'
606700040123     C     IPB(XW)       ANDEQ     'S'
606800991001     C                   ADD       NVA(WW)       §LPIMV
606900991001     C                   END
607000991001     C                   END
607100971003     C                   END
607200940308     C* MITTENTE
607300940308     C* NON GIA IMPOSTATO PRIMA
607400940308     C     §LPRSM        IFEQ      *BLANKS
607500940308     C                   MOVEL     ARBRSM        §LPRSM
607600940308     C                   MOVEL     ARBINM        §LPINM
607700940308     C                   MOVE      ARBCAM        §LPCAM
607800940308     C                   MOVEL     ARBLOM        §LPLOM
607900940308     C                   MOVEL     ARBPRM        §LPPRM
608000951107     C                   MOVEL     ARBNZM        §LPNZM
608100940308     C                   END
608200970408     C* SE CODICE MITTENTE = 8888/9999 O RESO PASSO 0 NEL CODICE
608300970408     C* TARIFFA ALTRIMENTI GLI PASSO IL CODICE DELLA BOLLA ORIG.
608400970408     C                   MOVE      §LPCCM        W8899             4 0
608500970408     C     W8899         IFEQ      8888
608600970408     C     W8899         OREQ      9999
608700970408     C     VFRI          OREQ      'R'
608800970408     C                   MOVE      *ZEROS        §LPCTR
608900970408     C                   ELSE
609000970408     C                   MOVE      ARBCTR        §LPCTR
609100970408     C                   END
609200970408     C* CAMPI DALLA VECCHIA BOLLA
609300940308     C                   MOVE      *BLANK        §LPCTS
609400940308     C                   MOVE      ARBTSP        §LPTSP
609500091008      * forzatura h 10,30 x dirottamenti resi viene trasformato in 'E'
609600091008     c                   if        §lptsp = 'H' and
609700091008     c                             (vfri = 'D' or vfri = 'R')
609800091008     c                   move      'E'           §lptsp
609900091008     c                   endif
610000941003     C                   Z-ADD     ARBVMD        §LPVMD
610100941216     C                   MOVEL     ARBVAD        §LPVAD
610200960306     C                   MOVEL     'P'           §LPFPR
610300970407     C                   MOVEL     'G'           §LPRIC
610400980519     C* Reperisco flag x stampare o meno bolla in partenza
610500081114      * a meno che non abbia già fatto un tentativo nel caso forzo sempre'N'
610600081114     c                   if        arbntc > 0
610700081114     c                   move      'N'           §lpbam
610800081114     c                   else
610900980519     C                   CLEAR                   AR4NOT
611000980519     C                   MOVE      'A'           §TRC
611100060213     C     KAR4          CHAIN(N)  FiAR401L                           43
611200980519     C  N43              MOVEL     AR4NOT        §LPBAM
611300980519     C   43              MOVEL     'S'           §LPBAM
611400081114     c                   endif
611500000313     C* Salvo flag bolla no DDT per verificare se devo effettuare
611600000313     C* le stampe
611700000313     C                   MOVEL     §LPBAM        WOKSTP            1
611800150630      *
611900150630      *****************************
612000150630      *- NO BOLLA DI RECUPERO_____:
612100150630      *****************************
612200150630      *
612300951213     C* Se no bolla recupero spese x cambio di porto da assegnato a
612400951213     C* franco imposto i dati dal video e da vecchia bolla
612500951213     C     WRECSP        IFNE      'S'
612600150630      *
612700960306     C                   MOVEL     'G'           §LPFPR
612800961011     C  N94V4CRCS        IFNE      'S'
612900051006     C     vfri          andeq     'R'
613000951227     C                   MOVEL     *BLANK        §LPNT2
613100960328     C                   END
613200951213     C* DESTINATARIO
613300951213     C     VRSD          IFNE      *BLANKS
613400951213     C                   MOVEL     VRSD          §LPRSD
613500951213     C                   MOVEL     VRD2          §LPRD2
613600951213     C                   MOVEL     VIND          §LPIND
613700951213     C                   MOVE      VCAD          §LPCAD
613800951213     C                   MOVEL     VLOD          §LPLOD
613900951213     C                   MOVEL     VPRD          §LPPRD
614000951213     C                   MOVEL     VNZD          §LPNZD
614100951213     C                   ELSE
614200951213     C* NON GIA IMPOSTATO PRIMA
614300951213     C     §LPRSD        IFEQ      *BLANKS
614400951213     C                   MOVEL     ARBRSD        §LPRSD
614500951213     C                   MOVEL     AR4RD2        §LPRD2
614600951213     C                   MOVEL     ARBIND        §LPIND
614700951213     C                   MOVE      ARBCAD        §LPCAD
614800951213     C                   MOVEL     ARBLOD        §LPLOD
614900951213     C                   MOVEL     ARBPRD        §LPPRD
615000951213     C                   MOVEL     ARBNZD        §LPNZD
615100951213     C                   END
615200951213     C                   END
615300060929      **
615400000412     C* PER RESI DPD (IMPORT) IMPOSTO CODICE TRATTAMENTO MERCE=6F
615500951213     C                   MOVE      VLNA          §LPLNA
615600000412     C     §LPLNA        CHAIN     AZORG01L                           21
615700000412     C     *IN21         IFEQ      '0'
615800000412     C     ORGFVA        ANDNE     '*'
615900000412     C                   MOVEL     ORGDE3        OG143
616000150630      *
616100150630      * ? solo nel caso si tratti di un Reso a DPD
616200150630      *    cerchiamo l'indirizzo del DEPOT a cui fare la BOLLA .
616300151020     c     §ogntw        ifeq      'DPD'
616400000412     C     VFRI          ANDEQ     'R'
616500151020     c     o24npr        andNE     *blank
616600151020      **
616700151020     c*
616800151020     c*  con il DEPOT di origine del Parcel,  il TISIE10R restituisce
616900151020      *    l'indirizzo del DEPOT preso dal Cappario Originale DPD
617000151020     c                   clear                   tisie10ds
617100151020     c                   movel     o24npr        I10DEP
617200151020     c                   call      'TISIE10R'
617300151020     c                   parm                    tisie10ds
617400151020      *
617500151020      * e l'indirizzo del DEPOT a cui rendere
617600151020     c                   if        O10CDPNAZ <> *blank  and
617700151020     c                             O10ERR    <> 'E'
617800151020     C                   eval      §lprsd  = O10CDPNAM1
617900151020     C                   eval      §lprd2  = O10CDPNAM2
618000151020     C                   eval      §lpind  = O10CDPADR1 + O10CDPADR2
618100151020     C                   eval      §lpcad  = O10CDPPTC
618200151020     C                   eval      §lplod  = O10CDPCTY
618300151020     C                   eval      §lpprd  = *blank
618400151020     C                   eval      §lpnzd  = O10CDPNAZ
618500151020      *
618600151020     c                   endif
618700151021      **
618800151021      * ? MA ATTENZIONE:
618900151021      * Se x DPD la linea di Arrivo non è utilizzabile in bollettazione,
619000151021      * Prendo allora la linea del Cliente
619100151021     c                   if        §OGLBL = 'N'
619200151021     c                   movel     §lpccm        Vlna
619300151021     C                   MOVE      VLNA          §LPLNA
619400151021     c                   end
619500151020     c*---------
619600000412     C                   END
619700000412     C                   END
619800150630     c
619900951213     C                   MOVE      ARBGC1        §LPGC1
620000951213     C                   MOVE      ARBGC2        §LPGC2
620100071024      * se precedentemente trovato destinatario particolare imposto
620200071024      * campi da anagrafica destinatari particolari
620300071024      * turni
620400071024      * forza giorni di chiusura solo se non impostati sulla mamma
620500091015     c                   if        des_parg <> *blank and ot0err = *blank
620600071024     c                   if        vfri <> 'C'
620700071024     c                   move      OT0X1GC1      §LPGC1
620800071024     c                   move      OT0X1GC2      §LPGC2
620900071024     c                   endif
621000071024      *giro
621100071024      * forza giro di consegna se lna corrisponde a linea gestione giro
621200071024     c                   if        §lplna = OT0X1POG
621300071024     c                   move      OT0X1CGI      §LPcgi
621400071024     c                   endif
621500071024     c                   endif
621600950214     C* RIPORTO SU NUOVA BOLLA IMPORTO DA ASSICURARE SOLO SE FACCIO
621700950214     C* CAMBIO PORTO IN ASSEGN. O RIMANE ASSEGNATO
621800950214     C     VNPC          IFEQ      'A'
621900950214     C     VNPC          OREQ      ' '
622000950214     C     EXPORT        ANDEQ     'A'
622100960205     C     VFRI          OREQ      'R'
622200960327     C     V4CRCS        ANDEQ     'N'
622300050509     C     VFRI          OREQ      'D'
622400950206     C                   Z-ADD     ARBIAS        §LPIAS
622500950214     C                   MOVEL     ARBVAS        §LPVAS
622600950214     C                   ELSE
622700950214     C                   CLEAR                   §LPIAS
622800950214     C                   CLEAR                   §LPVAS
622900950214     C                   ENDIF
623000150630      *
623100150630      *****************************
623200150630      *- SI BOLLA DI RECUPERO_____:
623300150630      *****************************
623400150630      **
623500951213     C                   ELSE
623600150630      **
623700951213      *  Bolla recupero spese trasporto dal mittente x cambi di
623800951213      *  porto da assegnato a franco
623900951213     C                   MOVEL     *BLANKS       §LPFFD
624000000925     C                   MOVEL     *BLANKS       §LPGGA
624100000922      *
624200000922      **---- < ATTENZIONE >  CHIODO PER DPD
624300000922      *    NON DEVE ESSERE RIPULITO IL CAMPO SE GIÀ IMPOSTATO
624400000922     C     VFRI          IFNE      'R'
624500000925     C     DPDSN         ORNE      'S'
624600951213     C                   MOVEL     *BLANKS       §LPGMA
624700000925     C                   ENDIF
624800000925      *
624900150311     C                   clear                   §lpTBA
625000150311     C                   clear                   §lpNBA
625100150311     C                   clear                   §lpTB2
625200150311     C                   clear                   §lpNB2
625300150311     C                   clear                   §lpcor
625400150311     C                   clear                   §lpcsf
625500951213     C                   MOVEL     *BLANKS       §LPGVA
625600951213     C                   MOVEL     *BLANKS       §LPGCA
625700951213     C                   Z-ADD     0             §LPPOR
625800951213     C                   Z-ADD     0             §LPDFT
625900951213     C                   Z-ADD     0             §LPFIV
626000990930     C                   CLEAR                   DTASV
626100030617     C* se annullamento porto assegnato sommo la varia & solo se la
626200030617     C* responsabilità è del cliente
626300030617     C                   IF        vfri='C' and vnpc='F' and viddcli='C'
626400030617     C                   MOVEL     §$2Vpa        §SIGLA
626500030617     C                   MOVEL     NSV           WSV
626600030617      * CERCO L'INDICE SU CUI LAVORARE
626700030617     C                   EXSR      CHKVAR
626800030623     C                   z-add     88888888      NVA(XS)
626900030617     C                   MOVEL     §$2Vpa        NSV(XS)
627000030617     C                   END
627100951213      *  Controllo se devo recuperare le provvigioni del C/Ass.
627200951213     C     VAPC          IFEQ      'S'
627300951213     C     VPPC          ANDEQ     'M'
627400951213     C                   Z-ADD     1             YY                4 0
627500951213     C     §$2VRG        LOOKUP    ASV(YY)                                06
627600951227      *  Se esiste varia G e non ho variato il C/Assegno tasso le provv.
627700951227      *  x il mittente in base alla tariffa del destinatario
627800951227     C     VVCS          IFNE      'S'
627900951227     C     *IN06         ANDEQ     '1'
628000951213     C                   MOVEL     ASV(YY)       NSV(1)
628100951213     C                   Z-ADD     AVA(YY)       NVA(1)
628200951227      *  ... altrimenti uso le tariffe di cartello
628300951227     C                   ELSE
628400951227     C                   EXSR      PPROVV
628500951213     C                   END
628600951213     C                   END
628700951213     C                   END
628800950206     C* SE SPED. ESTERO E DIVISA NON è IMPOSTATA FORZO DIV.=ITL
628900020226     C                   MOVEL     *BLANKS       ORGFL1
629000020226     C     §LPLNA        CHAIN     AZORG01L                           23
629100020729     C                   clear                   NLAFL1            1
629200020729     C     §OgNTW        ifeq      'FED'
629300020729     C     §OgNTW        oreq      'EEX'
629400020729     C     §OgNTW        oreq      'EUP'
629500020729     C                   movel     'E'           NLAFL1            1
629600020729     c                   end
629700020729      *
629800950206     C     §LPVMD        IFGT      *ZEROS
629900950206     C     ARBVAD        ANDEQ     *BLANK
630000011130     C                   MOVEL     §GEDCN        §LPVAD
630100950206     C                   ENDIF
630200950206     C*
630300950206     C     §LPIAS        IFGT      *ZEROS
630400950206     C     ARBVAS        ANDEQ     *BLANK
630500011130     C                   MOVEL     §GEDCN        §LPVAS
630600950206     C                   ENDIF
630700940308     C                   MOVE      ARBNAS        §LPNAS
630800940308     C                   MOVE      ARBNCL        §LPNCL
630900051005     C                   MOVE      ARBPKF        §LPPKB
631000941003     C                   MOVE      ARBPKC        §LPPKC
631100941003     C                   MOVE      ARBNCP        §LPNCP
631200951109     C                   MOVE      ARBVLF        §LPVLB
631300951109     C                   MOVE      ARBFVF        §LPFVB
631400960229     C*--------------------------------------------------------*
631500960229     C*  Se il flag del volume bollettato è  'C' o 'K' oppure
631600960229     C*  è  'P' ed il cliente è codificato imposto 'R' in modo
631700960229     C*  che non venga più ricalcolato
631800960229     C     ARBFVF        IFEQ      'C'
631900960229     C     ARBFVF        OREQ      'K'
632000960229     C     ARBFVF        OREQ      'P'
632100960229     C     §APPO         ANDEQ     8888
632200960229     C     ARBFVF        OREQ      'P'
632300960229     C     §APPO         ANDEQ     9999
632400960229     C                   MOVEL     'R'           §LPFVB
632500960229     C                   END
632600020716     C* se sto creando bolla di recupero x dirottamento azzero le varie
632700020716     c     viddcli       ifeq      'C'
632800020716     c     *in92         andeq     *on
632900020716     C                   CLEAR                   NVA
633000020716     C                   CLEAR                   NSV
633100020716     c                   endif
633200110715      * se dirottata bolla di rientro reimposta le varie relative alla giacenza
633300110715      * se erano valorizzate
633400110715     c                   if        vfri = 'D' and not *in90 or
633500110715     c                             vfri = 'R' and not *in90
633600110715     c                   if        memova1 <> *blank or
633700110715     c                             memova2 <> *blank or
633800110715     c                             memova3 <> *blank
633900110715     c                   move      memova1       nsv(1)
634000110715     c                   z-add     memovar1      nva(1)
634100110715     c                   move      memova2       nsv(2)
634200110715     c                   z-add     memovar2      nva(2)
634300110715     c                   move      memova3       nsv(3)
634400110715     c                   z-add     memovar3      nva(3)
634500110715     c                   endif
634600110715     c                   endif
634700960229     C*--------------------------------------------------------*
634800941003     C                   MOVE      ARBVLC        §LPVLC
634900941003     C                   MOVE      ARBNCR        §LPNCR
635000941006     C                   MOVE      ARBQFT        §LPQFT
635100940308     C                   MOVE      ARBNCR        §LPNCR
635200941011     C                   MOVEL     *BLANK        §LPFTM
635300941011     C                   MOVEL     *BLANK        §LPCEI
635400040916      * per bolla di recupero abblenca sempre tc1 e tc2
635500040916     c                   if        wrecsp <> 'S'
635600050826      * x consegna riporta sempre
635700050826      * x dirottamento riporta se non S=SUPERMERCATO o D=DISAGIATO
635800050826     c                   if        vfri = 'C' or
635900050826     c                             vfri = 'D' and
636000050826     c                             (arbtc1 <> 'D' and arbtc1 <> 'S' and
636100050826     c                              arbtc2 <> 'D' and arbtc2 <> 'S')
636200941011     C                   MOVEL     ARBTC1        §LPTC1
636300941003     C                   MOVEL     ARBTC2        §LPTC2
636400960228     C                   ELSE
636500960228     C                   MOVEL     *BLANKS       §LPTC1
636600960228     C                   MOVEL     *BLANKS       §LPTC2
636700960228     C                   END
636800040916     C                   else
636900040916     C                   move      *blank        §lptc1
637000040916     C                   move      *blank        §lptc1
637100040916     C                   endif
637200941003     C                   MOVEL     '*'           §LPSOP
637300940308     C* RICH. IMMISS. BOLLE
637400030715     C* OVRPRTF  BOLLE old
637500991206     C                   Z-ADD     80            LUNCMD           15 5
637600991206     C                   MOVEA     D82PRB        CMB(31)
637700991206     C                   MOVEA     D82MOB        CMB(64)
637800991206     C                   CALL      'QCMDEXC'
637900991206     C                   PARM                    CMB
638000991206     C                   PARM                    LUNCMD
638100030715     C* OVRPRTF  BOLLE a4
638200030715     C                   MOVEA     D82PRB4       CM4(31)
638300030715     C                   MOVEA     D82MdB4       CM4(64)
638400030715     C                   CALL      'QCMDEXC'
638500030715     C                   PARM                    CM4
638600030715     C                   PARM                    LUNCMD
638700030715     C* OVRPRTF  BOLLE a5
638800030715     C                   MOVEA     D82PRB5       CM5(31)
638900030715     C                   MOVEA     D82Mdb5       CM5(64)
639000030715     C                   CALL      'QCMDEXC'
639100030715     C                   PARM                    CM5
639200030715     C                   PARM                    LUNCMD
639300991206     C* OVRPRTF  BOLLE  DPD
639400991206     C                   MOVEA     D82PRB        CMP(31)
639500940308     C                   CALL      'QCMDEXC'
639600991206     C                   PARM                    CMP
639700940308     C                   PARM                    LUNCMD
639800030304     C* OVRPRTF  BOLLE  FedEx
639900030307     c                   Movel     *Blanks       comman           90
640000030307     C                   Z-ADD     90            LUNCMD           15 5
640100030304     C                   MOVEA     D82PRB        CMPf(31)
640200030307     c                   Movea     Cmpf          Comman
640300030307     c                   Eval      %Subst(Comman:77:9) = 'COPIES(2)'
640400030304     C                   CALL      'QCMDEXC'
640500030307     c                   PARM                    Comman
640600030304     C                   PARM                    LUNCMD
640700940308     C* OVRPRTF  ETICHETTE
640800941013     C                   Z-ADD     80            LUNCMD           15 5
640900941013     C                   MOVEA     D82PRE        CME(31)
641000950123     C                   MOVEA     D82MOE        CME(64)
641100940308     C                   CALL      'QCMDEXC'
641200940308     C                   PARM                    CME
641300940308     C                   PARM                    LUNCMD
641400991028     C* Imposto divisa per calcolo tassazione
641500991119     C                   XFOOT     NVA           TOTVAR           15 3
641600991119     C     TOTVAR        IFGT      0
641700991119     C     §LPPOR        ORGT      0
641800991028     C   33              MOVEL     V6CDIV        §LPDIV
641900991028     C   35              MOVEL     V6CRVA        §LPDIV
642000991028     C   36              MOVEL     V6CRTA        §LPDIV
642100991112     C  N33
642200991112     CANN35
642300991112     CANN36              MOVEL     GCPDIV        §LPDIV
642400991119     C* Se la divisa
642500991119     C     §LPDIV        IFEQ      *BLANKS
642600011130     C                   MOVEL     §GEDCN        §LPDIV
642700991119     C                   END
642800991119     C                   ELSE
642900991119     C                   MOVEL     *BLANKS       §LPDIV
643000991119     C                   END
643100030715     c* se dirottamento di una spedizione FW (servizio contrassegni)
643200030715     c* se le note 2 sono = *blanks metto
643300030715     c* la la dicitura "cliente tassazione xxxxxxx" preso dalla bolla orig.
643400030715     c                   if        §lpnt2 = *blanks and arbcbo = 'FW'
643500030715     c                             and vfri = 'D'
643600030715     C                   MOVE      '8'           §TRC
643700060213     C     KAR4          CHAIN(N)  FiAR401L
643800060213     c                   if        %found(fiar401l) and
643900030715     c                             %subst(ar4not: 1: 18) = 'cliente tassazione'
644000030715     C                   eval      §lpnt2= ar4not
644100030715     c                   end
644200030715     c                   end
644300100517     c* in caso di particolarità consegna  devo verificare
644400030821     c* se passare la particolarità x non fare scrivere il fiar500f nel
644500030821     c* pgm fnls01r inquanto lo scriverò in questo pgm.
644600100517     c                   if        arbgma <>*blank
644700100517     C                   MOVEL     '7R'          TBLCOD
644800100517     C                   MOVEL     *BLANK        TBLKEY
644900100517     C                   MOVEL     ARBGMA        TBLKEY
645000100517     C     KTAB          CHAIN     TABEL                              21
645100100517     C     *IN21         IFEQ      '0'
645200100517     C                   MOVEL     TBLUNI        DS7R
645300100517     C                   if        §7Rrc ='S'
645400030821     c                   eval      compar = arbgma
645500030821     c                   exsr      srpar
645600030821     c                   eval      §lpgma = compar
645700100517     c                   end
645800100517     c                   end
645900030821     c                   end
646000040123      * richiama tnsf20 x varie con calcolo istat solo se imponibile > 0
646100040123     c                   if        §lpimv > 0
646200040123     c                   exsr      ritassa
646300040123     c                   endif
646400030715     c*
646500040527      * imposta flag valore se esiste NO BOLLE RECUPERO
646600040527     c                   if        wrecsp <> 'S'
646700040527     c                   movel     §ar5bva       §lpbva
646800040527     c                   endif
646900040527     c*
647000100324     c                   clear                   §lpksca
647100100324     c                   clear                   §lpctra
647200091104      * imposta flag Triangolo e codice cliente e tariffa
647300091104      * se da passare a nuova bolla per dirottamento da bolla con triangolo
647400091028     c                   clear                   §lpass
647500091028     c                   if        vfri = 'D' and §ar5ass = 'T'
647600091215     c* se ho il contrassegno e il codice intestatario assegno lo imposto
647700091215     c                   if        §lpcas > 0 and §ar5cdi <> ' '
647800091215     c                   move      §ar5cdi       §lpcdi
647900091215     c                   end
648000091028     c                   movel     §ar5ass       §lpass
648100091104     c                   move      arbksc        §lpksca
648200091104     c                   move      arbctr        §lpctra
648300091028     c                   endif
648400091028     c*
648500091030      * chiodo fedex per resi in c/s
648600091030     c                   if        vfri = 'R' and arblnp = 400
648700091030     C                   movel     '3A'          tblcod
648800091030     C                   movel(P)  ARBcbo        tblkey
648900091030     C     ktab          chain     TABEL
649000091030     C                   movel     TBLUNI        DS3A
649100091030     c                   if        §3arbl = 'C'
649200091030     c                   move      'A '          §lpcbo
649300091030     c                   endif
649400091030     c                   endif
649500091030     c*
649600040527      * imposta flag dati clienti in stampa bolla     O
649700040527     c                   if        wrecsp <> 'S' and vfri <> 'R'
649800040527     c     kbol          chain     fiar801l
649900040527     c                   if        %found(fiar801l)
650000040527     c                   move      'S'           §lpar8
650100040527     c                   endif
650200040527     c                   endif
650300040527     c                   z-add     kaas          §LPAASM
650400040527     c                   z-add     klnp          §LPLNPM
650500040527     c                   z-add     knrs          §LPNRSM
650600040527     c                   z-add     knsp          §LPNSPM
650700050811      *verifica se inserire i codici clienti per la tassazione assegnati
650800050811      * i campi vanno impostati solo se la vecchia bolla era un franco e
650900050811      * la nuova è un assegnato e se il mittente della vecchia bolla è un
651000050826      * codificato nel caso si tratti di bolla di recupero o di reso
651100050826     c                   if        export = 'F' and
651200050811     c                             wvario <> 8888 and wvario <> 9999
651300050826     c                             and (vfri = 'R' or wrecsp = 'S')
651400050811     c                   move      arbksc        §lpksca
651500050811     c                   move      arbctr        §lpctra
651600080123     c* Richiamo routine di controllo codice tariffa per tassazine assegnati
651700080123     c                   EXSR      ContrCTRA
651800050811     c                   endif
651900151222      **
652000151222      **  Attenzione                     il codice Cliente e la tariffa
652100151222      * ? solo nel caso si tratti di un Reso a DPD
652200151222      **   sono quelli presi in tabella LPD
652300151222     c     §ogntw        ifeq      'DPD'
652400151222     C     VFRI          ANDEQ     'R'
652500151222     c     o24npr        andNE     *blank
652600151222     c                   clear                   FIEULPDDS
652700151222     c                   z-add     §LPLNP        ILPDLNP
652800151222     c                   z-add     §LPLNA        ILPDLNA
652900151222     c                   movel     o24npr        ILPDPARCEL
653000151222     c*   il FIEULPDR
653100151222     c                   call      'FIEULPDR'
653200151222     c                   parm                    FIEULPDDS
653300151222     c*
653400151222     c                   eval      dLPD = oLPDUNI
653500151222     c                   move      §LPDkscRE     §lpksca
653600151222     c                   move      §LPDtRE       §lpctra
653700151222      **
653800151222     c                   endif
653900130920     c*
654000130920     c*  Se su AZORG c'è la forzatura per la LNA viene sostituita la
654100130920     c*    LNA con quella di OG143 §ogLNAres
654200130920     c                   exsr      CHG_LNA_xFORZA
654300130920     c*
654400941013     C                   CALL      'FNLS01R'
654500940308     C                   PARM                    KPJBA
654600990930     C                   PARM                    DBLP
654700990930     C                   PARM                    DTASV
654800030715     C                   PARM                    trul90ds
654900150630      ** Errore per non aver generato la BOLLA
655000141030     c                   if        d82gia= 'N' and
655100141030     c                             (§lpfpr = '3' or §lpfpr = '4')
655200141030     c                   if        %parms > 1
655300141202     c                   if        wrecsp = *blank
655400141030     c                   move      'R'           d20erro
655500141202     c                   endif
655600141202     c
655700150630      *** se richiamato da gestione dei DIROTTAMENTI
655800141030     c                   else
655900141118     c                   if        wrecsp = *blank
656000141030     c                   move      '1'           d82f12
656100141118     c                   endif
656200141030     C                   MOVEL     fnlg82        KPJBU
656300141030     c                   endif
656400141118     c                   eval      *inlr = *on
656500141030     c                   return
656600141030     c                   endif
656700110802      * verifico se ho dei rekord di ar4 da riportare sulla bolla
656800110802     c                   exsr      DupAr4
656900030807     C* controllo se devo duplicare le estensioni (fiar5) dalla bolla orig.
657000030807     c* per le bolle figlie (wrecsp = ' ') e per le bolle di recupero
657100030807     c* (wrecsp = 'S')
657200030807     c                   if        wrecsp = ' '
657300030807     c                   movea     fig           ar5
657400030807     c                   else
657500030807     c                   movea     rec           ar5
657600030807     c                   end
657700030807     c                   exsr      AGGAR5
657800140916     c* Se la bolla figlia è un disk "C"  avente come "CHI SONO" il segnacollo
657900140916     c* Bartolini della bolla mamma devo cancellare dal FISGN il "CHI SONO"
658000140916     c* della bolla mamma. Se dovessero sparare il "CHI SONO" della mamma NON
658100140916     c* DEVE uscire l'etichetta, altrimenti uscirebbe il segnacollo BRT della
658200140916     c* mamma e non va bene.
658300140916     c                   exsr      sr_fisgn
658400940308     C* DLTOVR
658500941013     C                   Z-ADD     80            LUNCMD           15 5
658600940308     C                   CALL      'QCMDEXC'
658700940308     C                   PARM                    DOV
658800940308     C                   PARM                    LUNCMD
658900940308     C                   ENDSR
659000030805     C*---------------------------------------------------------------*
659100030821     C*   srpar : Controllo se devo abblencare la particolarità       *
659200030805     C*---------------------------------------------------------------*
659300030807     C     srpar         BEGSR
659400030807     c                   eval      tbecod = 'AR5'
659500030807     c                   clear                   tbeke2
659600030807     c                   eval      tbeke1 = 'RCC'
659700030807     C     ktbe          chain     TNTBE01L
659800030807     c                   if        %found(tntbe01l) and tbeatb = ' '
659900030807     C                   MOVEL     TBEUNI        DAR5
660000030807     c* abblenco la particolarità a seconda se scriverò io il fiar500f
660100030807     c* oppure se lo lascio scrivere al pgm fnls01r
660200030807     c* wrecsp = S sono nel caso che sto scrivendo la bolla di recupero
660300030821     c                   if        (wrecsp=' ' and vfri='D' and §ar5dupd='N') or
660400030821     c                             (wrecsp=' ' and vfri='R' and §ar5dupr='N') or
660500030821     c                             (wrecsp=' ' and vfri='C' and §ar5dupc='N') or
660600030821     c                             (wrecsp='S' and §ar5drec='N')
660700030807     c                   clear                   compar
660800030807     C                   END
660900030807     C                   END
661000030807     C                   ENDSR
661100040123     C*---------------------------------------------------------------*
661200040123     C*   richiama nuovamente tassatore per varie aggiuntive          *
661300040123     C*---------------------------------------------------------------*
661400040123     C     ritassa       BEGSR
661500040129     C                   CLEAR                   DTAS
661600040129     C                   MOVEL     §LPCTR        TASCTR
661700040129     C                   MOVE      KKSC          TASKSC
661800040129     C*
661900040129     C                   MOVEL     §LPCTS        TASCTS
662000040129     C                   MOVEL     §LPCAD        TASCAD
662100040129     C                   MOVEL     §LPCAM        TASCAM
662200040129     C                   MOVEL     §LPTSP        TASTSP
662300040129     C                   MOVEL     ARBTC1        TASTC1
662400040129     C                   MOVEL     ARBTC2        TASTC2
662500040129     C                   MOVEL     ARBFDN        TASFDN
662600040129     C                   MOVEL     §LPVAS        TASVAS
662700040129     C                   Z-ADD     §LPIAS        TASIAS
662800040129     C                   Z-ADD     ARBQFT        TASQFT
662900040129     C                   Z-ADD     ARBNCL        TASNCL
663000160530     C                   Z-ADD     ARBNCL        TASNCLb
663100040129     C                   Z-ADD     AMGUDT        TASDSP
663200040129     C                   Z-ADD     TASDSP        TASDCT
663300040129     C                   Z-ADD     §LPLNP        TASLNP
663400040129     C                   Z-ADD     VLNA          TASLNA
663500040129     C                   Z-ADD     ARBVLF        TASVLF
663600040129     C                   Z-ADD     ARBPKF        TASPKF
663700160530     C                   Z-ADD     ARBvlc        TASvlc
663800160530     C                   Z-ADD     ARBncr        TASncr
663900160530     C                   move      ARBfvf        TASfvf
664000160530     C                   Z-ADD     ARBpkb        TASPKb
664100040129     C                   CLEAR                   DS3A
664200040129     C                   Z-ADD     1             §F                2 0
664300040129     C     §LPCBO        LOOKUP    CDB(§F)                                21
664400040129     C   21              MOVEL     D3A(§F)       DS3A
664500040129     C   21              MOVEL     §3ATB1        TASTBL
664600040129     C* Reperisco codice tassazione
664700040129     C                   CLEAR                   TISI95
664800040129     C     TASLNP        CHAIN     AZORG01L                           21
664900040129     C     *IN21         IFEQ      '0'
665000040129     C                   MOVE      ORGDE3        OG143
665100040129     C     §OGNTW        IFEQ      'FED'
665200040129     C                   MOVE      'S'           I95FI1
665300040129     C                   END
665400040129     C                   END
665500040129     C                   MOVEL     '3'           I95TCN
665600040129     C                   MOVEL     §LPCAM        I95CAP
665700040129     C                   MOVEL     §LPNZM        I95NAR
665800040129     C                   MOVEL     §LPPRM        I95PRV
665900040129     C                   MOVEL     §LPLOM        I95LOC
666000040129     C                   MOVEL     §LPINM        I95IND
666100040129     C                   MOVEL     ARBTFP        I95TFP
666200040129     C                   MOVEL     ARBTSP        I95TSP
666300040129     C                   MOVEL     'S'           I95FRE
666400040129     C                   MOVEL     §LPNZM        I95NAR
666500040129     C                   MOVEL     AMGUDT        I95DAT
666600040129     C                   MOVEL     ARBtc1        I95fi2
666700040129     C                   MOVE      ARBtc2        I95fi2
666800040129     C                   CALL      'TISI95R'
666900040129     C                   PARM                    TISI95
667000040129     C                   MOVEL     O95CTS        TASMCT
667100040129     C*
667200040506     C                   MOVEL     ar9vca        TASVCA
667300040129     C                   Z-ADD     §ASS          TASCAS
667400040129     C                   MOVEL     VTIC          TASTIC
667500040129     C*
667600040129     C                   MOVE      §LPBAM        TASDDT
667700040129     c* imposto peso vdl e colli rilevati
667800040129     c                   z-add     arbncp        Tasncp
667900040129     c                   z-add     arbpkc        Taspkc
668000060927     C                   movel     ARBfbr        §asfbr
668100060927     C                   movel     ARBcca        §AScca
668200060927     C                   movel     dtas01        TASflo
668300040123     C*
668400040123     C                   MOVEL     'Z'           TASSVA
668500040123     C*
668600040123     C                   CALL      'TNSF20R'
668700040123     C                   PARM                    KPJBA
668800040123     C                   PARM                    DTAS
668900040123     C                   PARM                    DTASV
669000040123     C*
669100040123     C     TASERR        IFEQ      *BLANKS
669200040202     c                   z-add     0             §lpimv
669300040123     C* Scorro le varie e le sommo nel totale imponibile solo se il
669400040123     C* flag tab cc lo prevede
669500040123     C                   DO        20            WW                3 0
669600040123     C                   Z-ADD     1             XW                3 0
669700040123     C     NSV(WW)       LOOKUP    CCC(XW)                                20
669800040123     C     *IN20         IFEQ      '1'
669900040123     C     IPB(XW)       ANDEQ     'S'
670000040123     C                   ADD       NVA(WW)       §LPIMV
670100040123     C                   END
670200040123     C                   END
670300040123     C                   END
670400040123     C                   ENDSR
670500030807     C*---------------------------------------------------------------*
670600030807     C*   CTRSTB: Controllo se devo ristampare la bolla               *
670700030807     C*---------------------------------------------------------------*
670800030807     C     CTRSTB        BEGSR
670900951206     C*  Aggancio giacenza
671000050316     C     KGIA          CHAIN(e)  tigcp02L                           40
671100050316     c                   if        %error
671200070521     C     KGIA          CHAIN(n)  tigcp02L
671300050316     c                   exsr      geslck
671400050318     C     KGIA          CHAIN(e)  tigcp02L                           40
671500050316     c                   endif
671600951206     C     *IN40         IFEQ      '0'
671700951206     C                   MOVE      VAAS          KAAS1
671800951206     C                   MOVE      VLNP          KLNP1
671900951206     C                   MOVE      VNRS          KNRS1
672000951206     C                   MOVE      VNSP          KNSP1
672100951206     C                   MOVE      VNSP          KNSP1
672200951206     C                   MOVE      GCPDXD        KDEV
672300951206     C                   MOVE      *HIVAL        KHEV
672400990803     C     KEVB1         SETGT     FNEVB01L
672500990803     C     KEVB2         READPE    FNEVB01L                               45
672600951206     C     *IN45         IFEQ      '0'
672700081219     C                   Z-ADD     1             XY
672800951206     C     EVBCEV        LOOKUP    C2A(XY)                                45
672900951206     C     *IN45         IFEQ      '1'
673000951206     C                   MOVEL     D2A(XY)       DS2A
673100951206     C     §2AFTC        IFEQ      'A'
673200000313     C* Per bolle export e DPD non ristampo il laciato avviso x
673300000313     C* la consegna
673400000313     C     ARBLNA        CHAIN     AZORG01L                           21
673500000313     C                   MOVEL     ORGDE3        OG143
673600020729     C                   clear                   flgest            1
673700020729     C     §OgNTW        ifeq      'FED'
673800020729     C     §OgNTW        oreq      'EEX'
673900020729     C     §OgNTW        oreq      'EUP'
674000020729     C                   MOVEL     'E'           flgest            1
674100020729     c                   end
674200020729     C     Flgest        IFNE      'E'
674300020617     C     §OGntw        ANDNE     'DPD'
674400951206     C* RICH. IMMISS. BOLLE
674500030715     C* OVRPRTF  BOLLE old
674600951206     C                   Z-ADD     80            LUNCMD           15 5
674700951206     C                   MOVEA     D82PRB        CMB(31)
674800951206     C                   MOVEA     D82MOB        CMB(64)
674900951206     C                   CALL      'QCMDEXC'
675000951206     C                   PARM                    CMB
675100951206     C                   PARM                    LUNCMD
675200030715     C* OVRPRTF  BOLLE a4
675300030715     C                   MOVEA     D82PRB4       CM4(31)
675400030715     C                   MOVEA     D82Mdb4       CM4(64)
675500030715     C                   CALL      'QCMDEXC'
675600030715     C                   PARM                    CM4
675700030715     C                   PARM                    LUNCMD
675800030715     C* OVRPRTF  BOLLE a5
675900030715     C                   MOVEA     D82PRB5       CM5(31)
676000030715     C                   MOVEA     D82Mdb5       CM5(64)
676100030715     C                   CALL      'QCMDEXC'
676200030715     C                   PARM                    CM5
676300030715     C                   PARM                    LUNCMD
676400951206     C*  Richiamo pgm stampa bolle
676500130315     c                   reset                   FNLSB6ds1
676600030624     C                   CLEAR                   FNLSB5DS
676700951206     C                   MOVEL     ' '           DB0TLA
676800951206     C                   MOVEL     ' '           DB0GES
676900951206     C                   MOVEL     'A'           DB0TBO
677000951206     C                   MOVE      VAAS          DB0AAS
677100951206     C                   MOVE      VLNP          DB0LNP
677200951206     C                   MOVE      VNRS          DB0NRS
677300951206     C                   MOVE      VNSP          DB0NSP
677400030715     C                   CALL      d90psl
677500030624     C                   PARM                    FNLSB5DS
677600130315     c                   parm                    FNLSB6ds1
677700000313     C                   END
677800951206     C* MI RITORNA IL NR. BOLLA CREATO
677900951206     C* DLTOVR
678000951206     C                   Z-ADD     80            LUNCMD           15 5
678100951206     C                   CALL      'QCMDEXC'
678200951206     C                   PARM                    DOV
678300951206     C                   PARM                    LUNCMD
678400951206     C                   END
678500951206     C                   END
678600951206     C                   END
678700951206     C                   END
678800951206     C*
678900951206     C                   ENDSR
679000951206     C*---------------------------------------------------------------*
679100941220     C*   A G G C C M  FORZO COD.CLIENTE MITTENTE                     *
679200940308     C*---------------------------------------------------------------*
679300941220     C     AGGCCM        BEGSR
679400941220     C* IN CASO DI RESO
679500971230     C     VFRI          IFEQ      'R'
679600971230     C     V4CRCS        IFEQ      'N'
679700941220     C                   MOVE      9999          §LPCCM
679800941220     C                   MOVEL     §LPLNP        §LPCCM
679900971230     C                   ELSE
680000971230     C                   MOVE      8888          §LPCCM
680100960328     C                   MOVEL     §LPLNP        §LPCCM
680200960328     C                   END
680300941220     C                   ELSE
680400971230     C     WRECSP        IFEQ      'S'
680500020625     c                   if        *in92
680600020625     c                   move      arbksc        §lpccm
680700020625     c                   else
680800971230     C                   MOVE      9999          §LPCCM
680900951227     C                   MOVEL     §LPLNP        §LPCCM
681000020625     c                   end
681100951214     C                   ELSE
681200971230     C* NEGLI ALTRI CASI  IMPOSTO IL CLIENTE MITTENTE
681300941003     C     ARBCCM        IFGT      0
681400941003     C                   MOVE      ARBCCM        §LPCCM
681500941003     C                   ELSE
681600941003     C     EXPORT        IFEQ      'F'
681700941003     C                   MOVE      ARBKSC        §LPCCM
681800940308     C                   END
681900940308     C                   END
682000941220     C* SE NON SONO RIUSCITA AD IMPOSTARLO FORZO CLI.VARIO
682100941220     C     §LPCCM        IFEQ      *ZEROS
682200941220     C     VNPC          IFEQ      'F'
682300941220     C                   MOVE      *ALL'8'       §LPCCM
682400941220     C                   MOVEL     §LPLNP        §LPCCM
682500941220     C                   END
682600941220     C     VNPC          IFEQ      'A'
682700941220     C     VNPC          OREQ      ' '
682800941220     C     EXPORT        ANDEQ     'A'
682900941220     C                   MOVE      *ALL'9'       §LPCCM
683000941220     C                   MOVEL     §LPLNP        §LPCCM
683100941220     C                   END
683200971230     C                   ENDIF
683300971230     C                   ENDIF
683400971230     C                   ENDIF
683500950130     C                   MOVEL     §LPCCM        §LIN              3 0
683600950130     C                   MOVE      §LPCCM        §APPO             4 0
683700950130     C     §LIN          IFNE      §LPLNP
683800950130     C     §APPO         ANDEQ     8888
683900950130     C                   MOVEL     §LPLNP        §LPCCM
684000950130     C                   END
684100020625      * la move era incondizionata
684200020625     c                   if         *in92
684300020625     c                   movel     arblna        §lpccn
684400020625     c                   move      9999          §lpccn
684500020625     c                   else
684600941220     C                   MOVE      §LPCCM        §LPCCN
684700020625     c                   end
684800971230     C*
684900940308     C                   ENDSR
685000940309     C*---------------------------------------------------------------*
685100941011     C*   A G G A R 4           FILE NOTE                             *
685200940309     C*---------------------------------------------------------------*
685300941003     C     AGGAR4        BEGSR
685400940309     C                   MOVE      '0'           §TRC
685500060213     C     KAR4          CHAIN     FiAR401L                           21
685600940309     C                   MOVE      §LPAAS        NEWAAS
685700940309     C                   MOVE      §LPLNP        NEWLNP
685800940309     C                   MOVE      §LPNRS        NEWNRS
685900940309     C                   MOVE      §LPNSP        NEWNSP
686000940309     C                   MOVE      §LPLNA        NE2LNA
686100061025     C                   MOVEL(p)  NEWBOL        AR4NOT
686200941003     C   21              MOVE      §TRC          AR4TRC
686300941216     C   21              MOVE      KAAS          AR4AAS
686400941216     C   21              MOVE      KLNP          AR4LNP
686500941216     C   21              MOVE      KNRS          AR4NRS
686600941216     C   21              MOVE      KNSP          AR4NSP
686700060213     C  N21              UPDATE    FiAR4000
686800060213     C   21              WRITE     FiAR4000
686900940309     C                   ENDSR
687000030806     C*---------------------------------------------------------------*
687100030806     C* AGGAR5: Controllo se devo duplicare fiar500f                  *
687200030806     C*---------------------------------------------------------------*
687300030806     C     aggar5        BEGSR
687400030806     c                   if        §lpnsp <> 0
687500030806     c                   do        30            bb
687600030806     c                   if        ar5(bb) = *blanks
687700030806     c                   leave
687800030806     c                   end
687900030806     C                   movel     ar5(bb)       trcar5
688000051013     c     kFiar5        Chain(n)  Fiar501l
688100030806     c                   If        %Found(Fiar501l)
688200030806     c                   clear                   ar5prg
688300030911     c                   eval      ar5ft1 = 'T'
688400030911     c                   eval      ar5dt1 = 99999999
688500030911     c                   eval      ar5ft2 = 'T'
688600030911     c                   eval      ar5dt2 = 99999999
688700030806     c                   clear                   ar5ft3
688800030806     c                   clear                   ar5dt3
688900030806     c                   move      amgudt        ar5dac
689000030806     C                   TIME                    ar5ORc
689100030806     c                   eval      ar5aas = §lpaas
689200030806     c                   eval      ar5lnp = §lplnp
689300030806     c                   eval      ar5nrs = §lpnrs
689400030806     c                   eval      ar5nsp = §lpnsp
689500030806     c                   write     fiar50
689600030806     c                   end
689700030806     c                   enddo
689800030806     c                   end
689900030806     C                   ENDSR
690000110802     C*---------------------------------------------------------------*
690100110802     C*   duplica rekord di ar4 da riportare su nuova bolla           *
690200110802     C*---------------------------------------------------------------*
690300110802     C     DupAr4        BEGSR
690400110802     C     Kar4dup       setll     FiAR401L
690500110802     C                   do        *hival
690600110802     C     Kar4dup       reade     FiAR401L
690700110802     c                   if        %eof(fiar401l)
690800110802     c                   leave
690900110802     c                   endif
691000110802     C                   MOVEL     '7K'          TBLCOD
691100110802     C                   MOVEL     *BLANK        TBLKEY
691200110802     C                   MOVEL     ar4trc        TBLKEY
691300110802     c     ktab          chain     tabel00f
691400110802     c                   if        %found(tabel00f)
691500110802     c                   movel     tbluni        ds7k
691600110802     c                   if        §7KDUPD = 'S' and vfri = 'D' or
691700110802     c                             §7KDUPR = 'S' and vfri = 'R' or
691800110802     c                             §7KDUPC = 'S' and vfri = 'C' or
691900110802     c                             §7KDREC = 'S' and Wrecsp = 'S'
692000110802     c
692100110802     C                   MOVE      §LPAAS        AR4AAS
692200110802     C                   MOVE      §LPLNP        AR4LNP
692300110802     C                   MOVE      §LPNRS        AR4NRS
692400110802     C                   MOVE      §LPNSP        AR4NSP
692500110802     c                   clear                   AR4DUV
692600110802     c                   clear                   AR4FTR
692700110802     c                   clear                   AR4DTR
692800110802     C                   WRITE     FiAR4000
692900110802     c                   endif
693000110802     c                   endif
693100110802     c                   enddo
693200110802     C                   ENDSR
693300030826     C*-----------------------------------------------------------------*
693400030826     C* AGGORM x particolarità RC aggiorno anche l'orm della bolla orig.*
693500030826     C*-----------------------------------------------------------------*
693600030826     C     aggorm        BEGSR
693700100517
693800100517     c* se particolarità consegna divesra da blan testo il tipo per
693900100517     c* verificare se trattasi di particolarità con ritiro contestuale
694000100517     C                   clear                   DS7R
694100100517     c                   if        arbgma <>*blank
694200100517     C                   MOVEL     '7R'          TBLCOD
694300100517     C                   MOVEL     *BLANK        TBLKEY
694400100517     C                   MOVEL     ARBGMA        TBLKEY
694500100517     C     KTAB          CHAIN     TABEL                              21
694600100517     C     *IN21         IFEQ      '0'
694700100517     C                   MOVEL     TBLUNI        DS7R
694800100517     c                   endif
694900100517     c                   endif
695000100517     c
695100100517     c                   if        §lpnsp <> 0 and §7rrc  = 'S' and
695200031010     c                             (vfri<>'C' or (vfri='C' and §lplna<>arblna))
695300030826     c                   clear                   fnlr99ds
695400030826     c                   eval      i99tla = 'L'
695500030826     c                   eval      i99aas = arbaas
695600030826     c                   eval      i99lnp = arblnp
695700030826     c                   eval      i99nrs = arbnrs
695800030826     c                   eval      i99nsp = arbnsp
695900030826     c                   eval      i99fao = 999
696000071002     c                   eval      i99fgs = filarr
696100030826     c                   if        vfri = 'R'
696200030826     c                   eval      i99car = '91'
696300030826     c                   else
696400030826     c                   eval      i99car = '90'
696500030826     c                   eval      i99lna = §lplna
696600030826     c                   end
696700030826     c                   call      'FNLR99R'
696800030826     c                   parm                    kpjba
696900030826     c                   parm                    fnlr99ds
697000030826     c                   end
697100030826     C                   ENDSR
697200030826     C*---------------------------------------------------------------*
697300030826     C*   N O T D E S           note x modifica destinatario          *
697400030826     C*---------------------------------------------------------------*
697500030826     C     NOTDES        BEGSR
697600960124     C*
697700960124     C*  Prima ragione sociale
697800960124     C                   MOVE      'R'           §TRC
697900060213     C     KAR4          CHAIN     FiAR401L                           21
698000960129     C                   CLEAR                   AR4NOT
698100971124     C                   CLEAR                   AR4DTR
698200971124     C                   CLEAR                   AR4FTR
698300960124     C                   MOVEL     VRSD          AR4NOT
698400960124     C     *IN21         IFEQ      '1'
698500960124     C                   MOVE      §TRC          AR4TRC
698600960124     C                   MOVE      KAAS          AR4AAS
698700960124     C                   MOVE      KLNP          AR4LNP
698800960124     C                   MOVE      KNRS          AR4NRS
698900960124     C                   MOVE      KNSP          AR4NSP
699000060213     C                   WRITE     FiAR4000
699100960124     C                   ELSE
699200060213     C                   UPDATE    FiAR4000
699300960124     C                   END
699400960124     C*  Seconda ragione sociale
699500960125     C     VRD2          IFNE      *BLANKS
699600960124     C                   MOVE      'S'           §TRC
699700060213     C     KAR4          CHAIN     FiAR401L                           21
699800960129     C                   CLEAR                   AR4NOT
699900960124     C                   MOVEL     VRD2          AR4NOT
700000960124     C     *IN21         IFEQ      '1'
700100960124     C                   MOVE      §TRC          AR4TRC
700200960124     C                   MOVE      KAAS          AR4AAS
700300960124     C                   MOVE      KLNP          AR4LNP
700400960124     C                   MOVE      KNRS          AR4NRS
700500960124     C                   MOVE      KNSP          AR4NSP
700600060213     C                   WRITE     FiAR4000
700700960124     C                   ELSE
700800060213     C                   UPDATE    FiAR4000
700900960124     C                   END
701000960125     C                   END
701100960124     C*  Indirizzo
701200960124     C                   MOVE      'T'           §TRC
701300060213     C     KAR4          CHAIN     FiAR401L                           21
701400960129     C                   CLEAR                   AR4NOT
701500960124     C                   MOVEL     VIND          AR4NOT
701600960124     C     *IN21         IFEQ      '1'
701700960124     C                   MOVE      §TRC          AR4TRC
701800960124     C                   MOVE      KAAS          AR4AAS
701900960124     C                   MOVE      KLNP          AR4LNP
702000960124     C                   MOVE      KNRS          AR4NRS
702100960124     C                   MOVE      KNSP          AR4NSP
702200060213     C                   WRITE     FiAR4000
702300960124     C                   ELSE
702400060213     C                   UPDATE    FiAR4000
702500960124     C                   END
702600960124     C*  Località
702700960124     C                   MOVE      'U'           §TRC
702800060213     C     KAR4          CHAIN     FiAR401L                           21
702900960129     C                   CLEAR                   AR4NOT
703000960124     C                   MOVEL     VLOD          AR4NOT
703100960124     C     *IN21         IFEQ      '1'
703200960124     C                   MOVE      §TRC          AR4TRC
703300960124     C                   MOVE      KAAS          AR4AAS
703400960124     C                   MOVE      KLNP          AR4LNP
703500960124     C                   MOVE      KNRS          AR4NRS
703600960124     C                   MOVE      KNSP          AR4NSP
703700060213     C                   WRITE     FiAR4000
703800960124     C                   ELSE
703900060213     C                   UPDATE    FiAR4000
704000960124     C                   END
704100960124     C*  Cap./Provincia/Nazione
704200960124     C                   MOVE      'V'           §TRC
704300060213     C     KAR4          CHAIN     FiAR401L                           21
704400960124     C                   MOVEL     VCAD          WCAD
704500960124     C                   MOVEL     VNZD          WNZD
704600960124     C                   MOVEL     VPRD          WPRD
704700960129     C                   CLEAR                   AR4NOT
704800960124     C                   MOVEL     WNOT          AR4NOT
704900960124     C     *IN21         IFEQ      '1'
705000960124     C                   MOVE      §TRC          AR4TRC
705100960124     C                   MOVE      KAAS          AR4AAS
705200960124     C                   MOVE      KLNP          AR4LNP
705300960124     C                   MOVE      KNRS          AR4NRS
705400960124     C                   MOVE      KNSP          AR4NSP
705500060213     C                   WRITE     FiAR4000
705600960124     C                   ELSE
705700060213     C                   UPDATE    FiAR4000
705800960124     C                   END
705900960124     C*
706000960124     C                   ENDSR
706100940309     C/SPACE 3
706200940309     C*---------------------------------------------------------------*
706300940309     C*   A G G A R T           FILE DETTAGLIO SEGNACOLLI E ANOMALIE  *
706400940309     C*---------------------------------------------------------------*
706500940309     C     AGGART        BEGSR
706600940309     C*
706700941003     C     KBOL          SETLL     FNART01L
706800940309     C                   DO        *HIVAL
706900941003     C                   SETOFF                                           21
707000941006     C     KBOL          READE     FNART01L                               21
707100960618     C  N21              DO
707200950214     C                   Z-ADD     ARBDCM        ARTDCM
707300950214     C                   MOVEL     *BLANKS       ARTDAB
707400950214     C* Se manca data arrivo imposto = Data consegna
707500950214     C     ARTDAM        IFEQ      0
707600950215     C                   Z-ADD     ARBDCM        ARTDAM
707700950214     C                   Z-ADD     85            ARTNAP
707800950214     C                   END
707900970204     C* Se DIROTTAMENTO IMPOSTO 99999999 IN DTR
708000970204     C     VFRI          IFEQ      'D'
708100970204     C                   Z-ADD     99999999      ARTDTR
708200970204     C                   END
708300950214     C                   UPDATE    FNART000
708400940309     C* CONTROLLO SE ESISTONO DELLE ANOMALIE
708500990518     C     KANM          SETLL     FNANM02L
708600960618     C                   DO        *HIVAL
708700940309     C                   SETOFF                                       73
708800990518     C     KANM          READE     FNANM02L                               73
708900940309     C* CHIUDO ANOMALIA
709000960618     C     *IN73         IFEQ      '0'
709100960618     C                   CLEAR                   DSLR33
709200960618     C                   MOVE      '4'           D33FSC
709300960618     C                   Z-ADD     AMGUDT        D33DCH
709400960618     C                   MOVEL     'CO'          D33CCH
709500960618     C                   MOVEL     ARBCCA        D33CCA
709600960618     C                   Z-ADD     ANMNR2        D33NRR
709700960618     C                   CALL      'FNLR33R'
709800960618     C                   PARM                    DSLR33
709900960618     C                   END
710000960618     C  N73              END
710100960618     C                   ENDDO
710200940309     C  N21              END
710300040315     C*
710400940309     C                   ENDSR
710500940309     C/SPACE 3
710600940309     C*---------------------------------------------------------------*
710700941005     C*   E V E N T O  - SCRITTURA EVENTO X DIROTTAM. O CAMBIO PORTO  *
710800940309     C*---------------------------------------------------------------*
710900941005     C     EVENTO        BEGSR
711000941005     C                   MOVEL     *BLANKS       EVBATB
711100941005     C                   Z-ADD     AMGUDT        EVBDTV
711200941005     C                   TIME                    EVBORV
711300021009     C                   MOVE      simpou        EVBFLE
711400021114     C                   Z-ADD     o36cou        EVBCDU
711500941216     C                   Z-ADD     KLNP          EVBLNP
711600941216     C                   Z-ADD     KAAS          EVBAAS
711700941216     C                   Z-ADD     KNRS          EVBNRS
711800941216     C                   Z-ADD     KNSP          EVBNSP
711900941005     C                   Z-ADD     AMGUDT        EVBDEV
712000941005     C                   MOVEL     EVBORV        EVBHEV
712100941005     C     VFRI          IFEQ      'D'
712200941005     C                   MOVEL     'DIR'         EVBCEV
712300941005     C                   ELSE
712400941005     C                   MOVEL     'CPO'         EVBCEV
712500941005     C                   ENDIF
712600941005     C                   MOVEL     *BLANKS       EVBFTR
712700941005     C                   Z-ADD     0             EVBDTR
712800990803     C                   MOVEL     *BLANKS       EVBNOT
712900990803     C                   Z-ADD     0             EVBDTC
713000941005     C*
713100990803     C                   WRITE     FNEVB000
713200941005     C*
713300941005     C                   ENDSR
713400941005     C*---------------------------------------------------------------*
713500941005     C*   P G M N A C                                                 *
713600941005     C*---------------------------------------------------------------*
713700941005     C     RCHNAC        BEGSR
713800940309     C*
713900940309     C                   MOVEL     *ALL'?'       DLNP
714000940309     C     ARBLNP        CHAIN     AZORG01L                           21
714100940309     C  N21              MOVEL     ORGDES        DLNP             15
714200940309     C   21              MOVEL     *ALL'?'       DLNP
714300940309     C     ARBLNA        CHAIN     AZORG01L                           21
714400940309     C  N21              MOVEL     ORGDES        DLNA             15
714500940309     C   21              MOVEL     *ALL'?'       DLNA
714600951214     C     WSVLNA        CHAIN     AZORG01L                           21
714700940309     C  N21              MOVEL     ORGDES        DNWLNA           15
714800940309     C   21              MOVEL     *ALL'?'       DNWLNA
714900941006     C                   MOVE      AR6DFT        G08INV
715000941005     C                   MOVE      *ZEROS        G08DAT
715100941005     C                   MOVEL     '3'           G08ERR
715200941005     C                   CALL      'XSRDA8'
715300941005     C                   PARM                    WLBDA8
715400941005     C                   Z-ADD     G08DAT        GMADFT            8 0
715500940630     C     VNPC          COMP      'F'                                    28
715600940309     C* DA MANDARE DIRETTAMENTE A SEDE : RECUPERO GIA FATTO
715700950301     C  N98VFRI          IFEQ      'R'
715800940309     C                   SETON                                        86
715900940309     C                   END
716000941011      * SE HO DIROTTATO IN ASSEGNATO O RICONS. IN ASSEGNATO
716100950301     C  N98VFRI          IFEQ      'D'
716200940309     C     VFRI          OREQ      'C'
716300940309     C     VNPC          IFEQ      'A'
716400940309     C     VNPC          OREQ      ' '
716500940309     C     EXPORT        ANDEQ     'A'
716600941221      * RICH. NOTA ACCREDITO 2° BOLLA E PROVV. AL DESTIN.
716700941221     C     *IN37         OREQ      '1'
716800941221     C     VPPC          ANDEQ     'D'
716900940309     C                   SETON                                        86
717000940309     C                   END
717100940309     C                   END
717200940623     C*   Stampa Nota Accredito
717300950301     C     *IN98         IFEQ      '0'
717400991001     C                   Z-ADD     ARBKSC        CLIENT            7 0
717500991001     C     AR6KSC        IFNE      0
717600991001     C                   Z-ADD     AR6KSC        CLIENT            7 0
717700991001     C                   END
717800000131     C                   Z-ADD     0             TOTIMP           12 3
717900000131     C                   Z-ADD     0             TOTESE           12 3
718000000131     C                   Z-ADD     0             TOTNAC           12 3
718100940623     C     *IN32         IFEQ      '1'
718200991001     C                   Z-ADD     1             XS
718300991130     C     §$2IVA        LOOKUP    ASV(XS)                                20
718400991001     C     *IN20         IFEQ      '1'
718500020108     C     AR6IFT        SUB       AR6IMV        IMPALI           10 3
718600991001     C                   END
718700991027     C                   Z-ADD     §INAC1        INAC             12 3
718800991027     C* VERIFICO SE IMPORTO HA DECIMALI
718900991027     C                   MOVE      INAC          WCOMO3            3 0
719000991027     C     WCOMO3        COMP      0                                      11
719100941005     C                   Z-ADD     AR6ALI        ALIQ              3 1
719200020621     C                   EVAL      CdIVA = %EDITC(Aliq:'2')
719300941005     C                   MOVEL     AR6CEI        TPALI             1
719400940623     C                   END
719500940623     C     *IN37         IFEQ      '1'
719600991001     C                   Z-ADD     1             XS
719700991130     C     §$2IVA        LOOKUP    ASV(XS)                                20
719800991001     C     *IN20         IFEQ      '1'
719900020108     C     AR6IFT        SUB       AR6IMV        IMPALI
720000991001     C                   END
720100991027     C                   Z-ADD     §INAC2        INAC
720200991001     C                   Z-ADD     AR6ALI        ALIQ              3 1
720300020621     C                   EVAL      CdIVA = %EDITC(Aliq:'2')
720400991001     C                   MOVEL     AR6CEI        TPALI             1
720500940623     C                   END
720600011130      * STAMPA DOPPIO IMPORTO SE DIVISA NON DI CONTABILITÀ
720700011130     C                   CLEAR                   WAR6IM
720800011130     C     AR6DIV        IFNE      §GEDMC
720900011130     C                   CLEAR                   YEURDS
721000011130     C                   MOVEL     AR6DIV        YECDVI
721100011130     C                   Z-ADD     AR6IFT        YECIMI
721200011212     C                   MOVEL     §GEDMC        YECDVO
721300011130     C                   MOVEL     'H'           YECTAR
721400011130     C                   CALL      'YEURCO'
721500011130     C                   PARM                    YEURDS
721600011130     C                   Z-ADD     YECIMO        WAR6IM           13 3
721700011130     C                   ENDIF
721800011130      *
721900940623     C     TPALI         IFEQ      *BLANKS
722000000131     C                   Z-ADD     INAC          TOTIMP
722100940623     C                   ELSE
722200000131     C                   Z-ADD     INAC          TOTESE
722300940624     C                   Z-ADD     1             XX
722400940624     C     TPALI         LOOKUP    CEI(XX)                                41
722500940624     C     *IN41         IFEQ      '1'
722600020621     C                   MOVEL     ESE(XX)       CdIVA
722700940623     C                   END
722800940624     C                   MOVEL     'E'           TPALI
722900940624     C                   END
723000040315     c                   clear                   Worigine
723100040316     c                   clear                   Wpocompl
723200940623     C     VFRI          IFEQ      'D'
723300040423     c                   eval      worigine = 'DIROTTATA A ' + dnwlna
723400040317      *se dirottamento e linea arrivo 101 forzo sede
723500040317     c                   if        arblna = 101
723600040316     c                   eval      wpocompl = 046
723700040316     c                   endif
723800940623     C                   END
723900040316
724000940623     C     VFRI          IFEQ      'C'
724100040423     c   28              eval      worigine = 'CONSEGNATA IN FRANCO A ' + DNWLNA
724200040423     c  N28              eval      worigine = 'CONSEGNATA A: ' + DNWLNA
724300940623     C                   END
724400940623     C     VFRI          IFEQ      'R'
724500960327     C     V4CRCS        IFEQ      'N'
724600040315     c                   eval      worigine = 'LA MERCE E'' STATA RESA AL -
724700040315     C                             MITTENTE '
724800960327     C                   ELSE
724900040315     c                   eval      worigine = 'LA MERCE E'' STATA RESA AL -
725000040315     C                             MITTENTE IN C/SERVIZIO'
725100960327     C                   END
725200960327     C                   END
725300011130      * STAMPA DOPPIO IMPORTO SE DIVISA NON DI CONTABILITÀ
725400011130     C                   CLEAR                   WAR6IM
725500011130     C     AR6DIV        IFNE      §GEDMC
725600011130     C                   CLEAR                   YEURDS
725700011130     C                   MOVEL     AR6DIV        YECDVI
725800011130     C                   Z-ADD     INAC          YECIMI
725900011212     C                   MOVEL     §GEDMC        YECDVO
726000011130     C                   MOVEL     'H'           YECTAR
726100011130     C                   CALL      'YEURCO'
726200011130     C                   PARM                    YEURDS
726300011130     C                   Z-ADD     YECIMO        WAR6IM
726400011130     C                   ENDIF
726500011130      *
726600940623     C     TOTIMP        ADD       IMPALI        TOTNAC
726700940623     C                   ADD       TOTESE        TOTNAC
726800000131     C* VERIFICO SE IMPORTI CON DECIMALI
726900000131     C                   MOVE      TOTIMP        WCOMO3            3 0
727000000131     C     WCOMO3        COMP      0                                      08
727100000131     C                   MOVE      IMPALI        WCOMO3            3 0
727200000131     C     WCOMO3        COMP      0                                      09
727300000131     C                   MOVE      TOTESE        WCOMO3            3 0
727400000131     C     WCOMO3        COMP      0                                      10
727500000131     C                   MOVE      TOTNAC        WCOMO3            3 0
727600000131     C     WCOMO3        COMP      0                                      11
727700950301     C                   ELSE
727800950301     C                   END
727900040312      * scrittura file NCR
728000040312     c                   clear                   fnncr000
728100040315     c                   move      '201'         nctsocieta
728200040315     c                   z-add     arblna        nctfil
728300040316     c                   if        wpocompl <> *zeros
728400040316     c                   z-add     wpocompl      nctpocompl
728500040316     c                   else
728600040315     c                   z-add     arblnp        nctpocompl
728700040316     c                   endif
728800040315     c                   move      amgudt        nctddc
728900040315     c                   if        ar6dft > 0
729000040315     c                   move      ar6dft        nctdrp
729100040315     c                   end
729200040315     c                   z-add     ar6nft        nctnrp
729300040315     c                   z-add     ar6fiv        nctfpn
729400040315     c                   z-add     ar6ift        nctimr
729500040315     c                   move      *zeros        nctksc
729600040315     c                   move      ar6ksc        nctksc
729700040315     c                   move      *zeros        nctscf
729800040315     c                   move      ar6ksc        nctscf
729900040312     c                   move      knmus         nctutente
730000040312     c                   move      'I '          ncttnr
730100040312     c                   move      'C'           ncrrna
730200040312     c                   move      '3'           ncrcmn
730300040312      *reso
730400040429     c                   if        vfri ='R'
730500040312     c                   move      '4'           ncrcmn
730600040429     c                   endif
730700040429      *reso ma in C/S
730800040429     c                   if        vfri ='R' and v4crcs = 'S'
730900040429     c                   move      '0'           ncrcmn
731000040429     c                   move      'D'           ncrrna
731100040312     c                   endif
731200040312      *dirottamento non da giacenza
731300040312     c                   if        vfri ='D' and not *in90 and viddcli = 'V'
731400040312     c                   move      'A'           ncrrna
731500040312     c                   endif
731600040916      *se dirottamento e linea arrivo 101 forzo causale
731700041221     c                   if        arblna = 101 and vfri = 'D' or
731800041221     c                             §lplna = 101 and vfri = 'D'
731900040916     c                   move      '5'           ncrcmn
732000040916     c                   move      'C'           ncrrna
732100040916     c                   endif
732200040312      * storno spese giacenza
732300040312     c                   z-add     ar6imv        ncrim2
732400040312     c                   z-add     ar6ali        ncrali
732500040312     c                   z-add     arblnp        ncrflr
732600040312     c                   z-add     exaas         ncraassto
732700040312     c                   z-add     exlnp         ncrlnpsto
732800040315     c                   z-add     exnrs         ncrnrssto
732900040312     c                   z-add     exnsp         ncrnspsto
733000040429      *con causale 'D0' la spedizione di recupero non viene impostata
733100040429      *
733200040429     c                   if        ncrrna = '0' and ncrcmn = 'D'
733300040429     c                   z-add     0             ncraasrec
733400040429     c                   z-add     0             ncrlnprec
733500040429     c                   z-add     0             ncrnrsrec
733600040429     c                   z-add     0             ncrnsprec
733700040429     c                   z-add     0             nchlnarec
733800040429     c                   else
733900040429     c                   select
734000040429     c                   when      vfri = 'D' and vnpc = 'F'
734100040429     c                   z-add     0             ncraasrec
734200040429     c                   z-add     0             ncrlnprec
734300040429     c                   z-add     0             ncrnrsrec
734400040429     c                   z-add     0             ncrnsprec
734500040429     c                   z-add     0             nchlnarec
735200040429     c                   other
735300040429     c   86              z-add     §lpaas        ncraasrec
735400040316     c   86              z-add     §lplnp        ncrlnprec
735500040316     c   86              z-add     §lpnsr        ncrnrsrec
735600040316     c   86              z-add     §lpnsp        ncrnsprec
735700040316     c   86              z-add     §lplna        nchlnarec
735800040429     c                   endsl
735900040429     c                   endif
736000040315      * se impostata la spedizione di recupero la richiesta si considera
736100040315      * completa
736200040315     c                   if        ncrnsprec > 0
736300040315     c                   move      '1'           nchricompl
736400040315     c                   else
736500040315     c                   move      '0'           nchricompl
736600040315     c                   endif
736700040315
736800040315     c                   move      ar6cei        nchcei
736900040312     c                   z-add     wsvaas        nchaasfgl
737000040312     c                   z-add     wsvlnp        nchlnpfgl
737100040312     c                   z-add     wsvnrs        nchnrsfgl
737200040312     c                   z-add     wsvnsp        nchnspfgl
737300040312     c                   z-add     wsvlna        nchlnafgl
737400040312     c   90              z-add     gcpagc        nchagc
737500040312     c   90              z-add     gcpfgc        nchfgc
737600040312     c   90              z-add     gcpngc        nchngc
737700040315     c                   movel     worigine      nchorigine
737800040312     c                   write     fnncr000
737900940623     C*
738000941011     C                   SETOFF                                           86
738100940309     C*
738200940309     C                   ENDSR
738300940309     C/SPACE 3
738400940309     C*---------------------------------------------------------------*
738500940309     C*   P G M R E C                                                 *
738600940309     C*---------------------------------------------------------------*
738700940309     C     RCHREC        BEGSR
738800940309     C*
738900970715     C* Stampo richiesta di recupero solo se non dirotto alla 101
739000970715     C     VLNA          IFNE      101
739100970715     C     VFRI          ORNE      'D'
739200970715     C*
739300940309     C                   MOVEL     *ALL'?'       DLNP
739400940309     C     ARBLNP        CHAIN     AZORG01L                           21
739500940309     C  N21              MOVEL     ORGDES        DLNP             15
739600940309     C   21              MOVEL     *ALL'?'       DLNP
739700940309     C     ARBLNA        CHAIN     AZORG01L                           21
739800940309     C  N21              MOVEL     ORGDES        DLNA             15
739900940309     C   21              MOVEL     *ALL'?'       DLNA
740000940309     C     §LPLNA        CHAIN     AZORG01L                           21
740100940309     C  N21              MOVEL     ORGDES        DNWLNA           15
740200940309     C   21              MOVEL     *ALL'?'       DNWLNA
740300941006     C                   MOVE      AR6DFT        G08INV
740400941005     C                   MOVE      *ZEROS        G08DAT
740500941005     C                   MOVEL     '3'           G08ERR
740600941005     C                   CALL      'XSRDA8'
740700941005     C                   PARM                    WLBDA8
740800941005     C                   Z-ADD     G08DAT        GMADFT
740900040429      * stampo solo se non richiedo la nota di credito
741000040429     c                   if        not *in32 and not *in37
741100940309     C                   EXCEPT    RECTES
741200940309     C   84              EXCEPT    DIRTES
741300960126     C   85              EXCEPT    RICTES
741400940309     C                   EXCEPT    RECDET
741500940309     C   84              EXCEPT    DIRDET
741600960126     C   85              EXCEPT    RICDET
741700941221     C   89              EXCEPT    RECPRO
741800040429     c                   endif
741900940309     C* SE DIROTTO IN FRANCO E NO CAMBIO PORTO NON HO STORNATO BOLLA
742000940309     C  N32
742100940309     CANN37VFRI          IFEQ      'D'
742200940309     C     EXPORT        ANDEQ     'F'
742300940309     C     VNPC          ANDEQ     ' '
742400940309     C                   SETON                                        69
742500940309     C                   END
742600040429      * stampo solo se non richiedo la nota di credito
742700040429     c                   if        not *in32 and not *in37
742800940309     C                   EXCEPT    RECFIN
742900040429     c                   endif
743000040429
743100940309     C                   SETOFF                                       69
743200970715     C*
743300970715     C                   END
743400970715     C*
743500940309     C                   ENDSR
743600951213     C*---------------------------------------------------------------*
743700951213     C*   R E C T R A  CREO BOLLA RECUPERO SPESE X CAMBIO PORTO A -> F*
743800951213     C*---------------------------------------------------------------*
743900951213     C     RECTRA        BEGSR
744000951213     C*
744100951214     C                   SETON                                        86
744200990930     C                   CLEAR                   DBLP
744300031121     C                   CLEAR                   vteld
744400031121     C                   CLEAR                   vref
744500951213     C                   MOVEL     'E '          §LPCBO
744600020617     c                   if        viddcli = 'C' and *in92
744700020617     c                   move      'F*'          §lpcbo
744800020617     c                   end
744900951213     C                   MOVEL     '4'           §LPCTM
745000951213      *  Inverto mittente e destinatario
745100951213     C                   MOVEL     ARBRSD        §LPRSM
745200951213     C                   MOVEL     ARBIND        §LPINM
745300951213     C                   MOVEL     ARBCAD        §LPCAM
745400951213     C                   MOVEL     ARBLOD        §LPLOM
745500951213     C                   MOVEL     ARBPRD        §LPPRM
745600951213     C                   MOVEL     ARBNZD        §LPNZM
745700951213     C                   MOVEL     ARBRSM        §LPRSD
745800951213     C                   MOVEL     ARBINM        §LPIND
745900951213     C                   MOVEL     ARBCAM        §LPCAD
746000951213     C                   MOVEL     ARBLOM        §LPLOD
746100951213     C                   MOVEL     ARBPRM        §LPPRD
746200951213     C                   MOVEL     ARBNZM        §LPNZD
746300960205     C     WSVLPO        IFNE      ARBLNA
746400951228     C                   MOVEL     WSVLPO        §LPLNA
746500960205     C                   ELSE
746600960205     C                   MOVEL     ARBLNP        §LPLNA
746700960205     C                   END
746800020225     C     §LPLNA        CHAIN     AZORG01L                           21
746900020225     C     *IN21         IFEQ      '0'
747000020225     C                   MOVE      ORGDE3        OG143
747100020225     C     §OGNTW        IFEQ      'FED'
747200020225     C                   MOVE      'S'           I95FI1
747300020225     C                   END
747400020225     C                   END
747500951213      *  Calcolo zona consegna
747600970710     C                   CLEAR                   TISI95
747700970710     C                   MOVEL     '3'           I95TCN
747800970710     C                   MOVEL     *BLANK        I95TLA
747900970710     C                   MOVEL     VNZD          I95NAR
748000970710     C                   MOVEL     VIND          I95IND
748100970710     C                   MOVEL     VCAD          I95CAP
748200970710     C                   MOVEL     VLOD          I95LOC
748300970710     C                   MOVEL     VPRD          I95PRV
748400970710     C                   MOVEL     ARBPKF        I95LKG
748500970710     C                   MOVEL     ARBVLF        I95LMC
748600970710     C                   MOVEL     ARBFFD        I95FFD
748700970710     C                   MOVEL     ARBTSP        I95TSP
748800031021     C                   MOVEL     ARBtc1        I95fi2
748900031021     C                   MOVE      ARBtc2        I95fi2
749000970710     C                   CALL      'TISI95R'
749100970710     C                   PARM                    TISI95
749200970710     C                   MOVEL     O95ZNC        §LPZNC
749300051005      * BOLLA RECUPERO
749400051005     C                   MOVEL     'S'           WRECSP
749500951214      *  Imposto note generiche
749600951214     C                   EXSR      XLPNOT
749700951214      *  Imposto note bolla recupero
749800960206     C                   MOVEL     *BLANKS       §LPNT2
749900960206     C     ARBCCM        IFNE      0
750000951214     C                   MOVEL     CNOT1         §LPNT2
750100951214     C                   MOVE      CNOT2         §LPNT2
750200951227     C                   MOVE      ARBCCM        §LPNT2
750300960206     C                   END
750400951213      *  Richiamo routine impostazione parametri call pgm imm.bolle
750500951213     C                   EXSR      PGNBLP
750600951213     C*
750700951213     C                   ENDSR
750800940308     C*---------------------------------------------------------------*
750900940308     C*   L E G B O L  LEGO LA NUOVA BOLLA CREATA                     *
751000940308     C*---------------------------------------------------------------*
751100940308     C     LEGBOL        BEGSR
751200940308     C     §LPNSP        IFGT      0
751300941005      * GUARDO SE ESISTE LEGAME PER REPERIRE I RIFERIM. DELLA BOLLA
751400941005      * ORIGINALE ,ALTRIMENTI LA BOLLA ORIGINALE è LA SPEDIZ. TRATTATA
751500941005     C     KBOL          CHAIN     FNLBL01L                           21
751600941005     C     *IN21         IFEQ      '1'
751700941216     C                   MOVE      KLNP          LBLLPO
751800941216     C                   MOVE      KAAS          LBLAAO
751900941216     C                   MOVE      EXMGS         LBLMGO
752000941216     C                   MOVE      KNRS          LBLNRO
752100941216     C                   MOVE      KNSP          LBLNSO
752200941216     C                   MOVE      EXLNA         LBLLAO
752300941005     C                   ENDIF
752400951228     C                   MOVE      LBLLPO        WSVLPO
752500951228     C                   MOVE      §LPLNP        LBLLPN
752600940308     C                   MOVE      §LPAAS        LBLAAN
752700940308     C                   MOVE      §LPMGS        LBLMGN
752800940308     C                   MOVE      §LPNRS        LBLNRN
752900940308     C                   MOVE      §LPNSP        LBLNSN
753000940308     C                   MOVE      §LPLNA        LBLLAN
753100941216     C                   MOVE      KLNP          LBLLPP
753200941216     C                   MOVE      KAAS          LBLAAP
753300941216     C                   MOVE      EXMGS         LBLMGP
753400941216     C                   MOVE      KNRS          LBLNRP
753500941216     C                   MOVE      KNSP          LBLNSP
753600941216     C                   MOVE      EXLNA         LBLLAP
753700941006     C                   MOVE      *BLANK        LBLFT1
753800941006     C                   MOVE      *ZEROS        LBLDT1
753900941006     C                   MOVE      *BLANK        LBLFT2
754000941006     C                   MOVE      *ZEROS        LBLDT2
754100950331     C                   CLEAR                   DS3A
754200950331     C                   Z-ADD     1             §F
754300950331     C     EXCBO         LOOKUP    CDB(§F)                                04
754400950331     C   04              MOVEL     D3A(§F)       DS3A
754500950331     C     §3ABSD        IFEQ      'N'
754600950525     C                   Z-ADD     99999999      LBLDT2
754700950331     C                   END
754800941005     C                   WRITE     FNLBL000
754900041125     c* aggiornamento titas con flag legame bolla
755000041125     c                   call      'FNLG25R'
755100041125     C                   PARM                    recordlbl
755200041125     C                   PARM      ' '           I25tla            1
755300041125     c
755400940308     C                   END
755500940308     C                   ENDSR
755600940308     C*---------------------------------------------------------------*
755700941005     C*   A R R B O L  CREO SUBITO BOLLA IN ARRIVO                    *
755800940308     C*---------------------------------------------------------------*
755900940308     C     ARRBOL        BEGSR
756000941005      * RICH. PGM X SCRITTURA BOLLA IN ARRIVO
756100941005     C                   CLEAR                   DSUL41
756200941216     C                   MOVEL     §LPAAS        U41AAS
756300941216     C                   MOVEL     §LPLNP        U41LNP
756400941216     C                   MOVEL     §LPNRS        U41NRS
756500941216     C                   MOVEL     §LPNSP        U41NSP
756600941005     C                   CALL      'TRUL41R'
756700941005     C                   PARM                    DSUL41
756800070614      * duplica eventuali note ldv su bolla figlia
756900070614     c                   if        §oggeot = 'S'
757000070614     c                   exsr      dupnotldv
757100070614     c                   endif
757200940308     C*
757300941005     C* MI RITORNA IL TIPO BOLLA:RICHIAMO PGM TASSAZIONE
757400941227     C     U41TPB        IFEQ      'A'
757500941006     C     VSIFAT        ANDEQ     'S'
757600030715     C                   MOVEL     'FAT999'      MODFAT            9
757700030715     C                   MOVE      FILarr        MODFAT            9
757800030715     c                   movel(p)  modfat        com10            10
757900030715     C                   Z-ADD     80            LUNCMD           15 5
758000030715     C* OVRPRTF  FAT999 old
758100030715     C                   MOVEA     D82PRB        CMb(31)
758200030715     C                   MOVEA     com10         CMb(64)
758300991206     C                   CALL      'QCMDEXC'
758400030715     C                   PARM                    CMb
758500991206     C                   PARM                    LUNCMD
758600030715     C* OVRPRTF  FAT999 a4
758700030715     C                   MOVEA     D82PRB4       CM4(31)
758800030715     C                   MOVEA     com10         CM4(64)
758900030715     C                   CALL      'QCMDEXC'
759000030715     C                   PARM                    CM4
759100030715     C                   PARM                    LUNCMD
759200030715     C* OVRPRTF  FAT999 a5
759300030715     C                   MOVEA     D82PRB5       CM5(31)
759400030715     C                   MOVEA     com10         CM5(64)
759500030715     C                   CALL      'QCMDEXC'
759600030715     C                   PARM                    CM5
759700030715     C                   PARM                    LUNCMD
759800991206     C* OVRPRTF  FAT999 DPD
759900030715     C                   MOVEA     D82PRB        CMp(31)
760000940308     C                   CALL      'QCMDEXC'
760100030715     C                   PARM                    CMp
760200940308     C                   PARM                    LUNCMD
760300030304     C* OVRPRTF  FAT999 FedEx
760400030307     c                   Movel     *Blanks       comman           90
760500030307     C                   Z-ADD     90            LUNCMD           15 5
760600030715     C                   MOVEA     D82PRB        CMpf(31)
760700030715     c                   Movea     Cmpf          Comman
760800030307     c                   Eval      %Subst(Comman:77:9) = 'COPIES(2)'
760900030304     C                   CALL      'QCMDEXC'
761000030307     c                   PARM                    Comman
761100030304     C                   PARM                    LUNCMD
761200940308     C*
761300050511     C                   CLEAR                   fnlra2ds
761400050511     C                   MOVEL     FILARR        ILRa2FGS
761500050511     C                   MOVE      §LPAAS        ILRa2AAS
761600050511     C                   MOVE      §LPLNP        ILRa2LNP
761700050511     C                   MOVE      §LPNRS        ILRa2NRS
761800050511     C                   MOVE      §LPNSP        ILRa2NSP
761900050511     C                   MOVEL     'TB'          ILRa2CVB
762100050511     C                   MOVEL     fnlra2ds      KPJBU
762200050228     C*
762300050511     C                   CALL      'FNLRA2R'
762400050228     C                   PARM                    KPJBA
762500050228     C                   PARM                    trul90ds
762600050511     C                   MOVEL     KPJBU         fnlrA2DS
762700940308     C* DLTOVR
762800941013     C                   Z-ADD     80            LUNCMD           15 5
762900940308     C                   CALL      'QCMDEXC'
763000940308     C                   PARM                    DOV
763100940308     C                   PARM                    LUNCMD
763200940311     C                   END
763300991028      * AGGIORNO LA BOLLA IN PARTENZA COME TRASMESSA
763400160805     C     KBLP          CHAIN     FNBLP01L
763500160805     C                   IF        %found(fnblp01l)
763600991028     C                   MOVEL     'T'           BLPFT1
763700991028     C                   Z-ADD     AMGUDT        BLPDT1
763800991028     C                   EXCEPT    AGGTRA
763900991028     C                   END
764000940311      * STAMPA BOLLA  SE NON HO FATTO  FATTURA
764100940311     C     VSIFAT        IFEQ      'S'
764200941228     C     U41TPB        ANDEQ     'A'
764300050511     C     olrA2DFT      ANDLE     *ZEROS
764400940613     C     VSIFAT        OREQ      'N'
764500941227     C     U41TPB        ANDEQ     'A'
764600941227     C     U41TPB        OREQ      'F'
764700000313     C* Stampo la bolla in arrivo se non DPD e no Euroexpress
764800000313     C     NLAFL1        IFNE      'E'
764900020617     C     §OGntw        ANDNE     'DPD'
765000000313     C* oppure Euroexpress e ma si ddt
765100000313     C     NLAFL1        OREQ      'E'
765200000313     C     §LPBAM        ANDNE     'N'
765300000313     C     §LPBAM        ANDNE     'Y'
765400000313     C     §LPBAM        ANDNE     'K'
765500130315     c                   reset                   FNLSB6ds1
765600030624     C                   CLEAR                   FNLSB5DS
765700941215     C                   MOVEL     ' '           DB0TLA
765800941215     C                   MOVEL     ' '           DB0GES
765900941215     C                   MOVEL     'A'           DB0TBO
766000941227     C                   MOVE      §LPAAS        DB0AAS
766100941227     C                   MOVE      §LPLNP        DB0LNP
766200941227     C                   MOVE      §LPNRS        DB0NRS
766300941227     C                   MOVE      §LPNSP        DB0NSP
766400991208      *
766500030715     C                   CALL      d90psl
766600030624     C                   PARM                    FNLSB5DS
766700130315     c                   parm                    FNLSB6ds1
766800000313     C                   END
766900940308     C                   END
767000940308     C                   ENDSR
767100940308     C*---------------------------------------------------------------*
767200940308     C*   S T A B L P  STAMPA BOLLA LOCALE SE NON CREATA IN ARRIVO    *
767300940308     C*---------------------------------------------------------------*
767400940308     C     STABLP        BEGSR
767500000313     C* per lna Euroexpress no ddt o DPD non devo ristampare
767600000313     C* la bolla
767700000313     C     EXLNA         CHAIN     AZORG01L                           30
767800000313     C                   MOVEL     ORGDE3        OG143
767900000313     C* No Dpd / no euroexoress oppure
768000020617     C     §OGntw        IFNE      'DPD'
768100000313     C     NLAFL1        ANDNE     'E'
768200000313     C* Euroexpress si DDT
768300000313     C     NLAFL1        OREQ      'E'
768400000313     C     §LPBAM        ANDNE     'N'
768500000313     C     §LPBAM        ANDNE     'Y'
768600000313     C     §LPBAM        ANDNE     'K'
768700130315     c                   reset                   FNLSB6ds1
768800030624     C                   CLEAR                   FNLSB5DS
768900941215     C                   MOVEL     ' '           DB0TLA
769000941215     C                   MOVEL     ' '           DB0GES
769100941215     C                   MOVEL     'P'           DB0TBO
769200941227     C                   MOVE      §LPAAS        DB0AAS
769300941227     C                   MOVE      §LPLNP        DB0LNP
769400941227     C                   MOVE      §LPNRS        DB0NRS
769500941227     C                   MOVE      §LPNSP        DB0NSP
769600941215     C*
769700030715     C                   CALL      d90psl
769800030624     C                   PARM                    FNLSB5DS
769900130315     c                   parm                    FNLSB6ds1
770000000313     C                   END
770100000313     C*
770200940308     C                   ENDSR
770300900504     C/SPACE 3
770400900504     C*---------------------------------------------------------------*
770500900504     C*   G C P W T R                                                 *
770600900504     C*---------------------------------------------------------------*
770700900504     C     GCPWTR        BEGSR
770800050316     C     KGIA          CHAIN(e)  tigcp000                           40
770900050316     c                   if        %error
771000070521     C     KGIA          CHAIN(n)  tigcp000
771100050316     c                   exsr      geslck
771200050318     C     KGIA          CHAIN(e)  tigcp000                           40
771300050316     c                   endif
771400941229     C* SE FORZATE  DISPOSIZIONI DA CONS. A DIROTT E VICEVERSA
771500941006     C     GCPDIS        IFNE      VFRI
771600941006     C                   MOVEL     VFRI          GCPDIS
771700941229     C                   MOVEL     VNPC          GCPNPC
771800941229     C                   MOVEL     VASG          GCPASG
771900941229     C                   MOVEL     VPSG          GCPPSG
772000940308     C                   END
772100941229     C* DATA CHIUSURA GIACENZA
772200050405     C                   MOVEL     VCFG          GCPCFG
772300050405     c                   if        spearr = 1
772400050405     C                   Z-ADD     VSGR          GCPSGR
772500050405     C                   Z-ADD     VGGS          GCPGGS
772600050405     C                   Z-ADD     VSGF          GCPSGF
772700050405     C                   Z-ADD     VTSGS         GCPSGS
772800050405     C                   Z-ADD     VSGR          GCPSGR
772900050405     C                   Z-ADD     VSGD          GCPSGD
773000911118     C                   Z-ADD     §TFT          GCPTFT
773100900423     C                   Z-ADD     AMGDSD        GCPDSD
773200900423     C                   Z-ADD     AMGDSA        GCPDSA
773300050405     c                   end
773400050405     C                   Z-ADD     AMGUDT        GCPDCG
773500050222     C                   Z-ADD     §LPLNP        GCPLPWa
773600050222     C                   Z-ADD     §LPAAS        GCPASWa
773700050222     C                   Z-ADD     §LPNRS        GCPNSWa
773800050222     C                   Z-ADD     §LPNSP        GCPNBWa
773900050222     C                   MOVE      AMGUDT        GCPDeda
774000050408     C* imposto prima fase =35
774100050408     C                   Z-ADD     035           GCPFAS
774200050414     C                   Z-ADD     0             GCPduma
774300050408     c* però se è un reso poichè in partenza non si devono rec. spese fase =50
774400050408     C     VCFG          IFEQ      '002'
774500050408     C                   Z-ADD     050           GCPFAS
774600050408     C                   END
774700050408     c*      se è dirottamento e la partenza ha già recuperato  fase =50
774800050408     C     VCFG          ifEQ      '006'
774900050408     C     VLNA          ANDNE     GCPLNA
775000050408     C     gcpnbw        ifne      0
775100050408     C                   Z-ADD     050           GCPFAS
775200050408     c                   else
775300050408     c*     se è dirottamento e la partenza non ha recuperato  fase =40
775400050408     C                   Z-ADD     040           GCPFAS
775500050408     C                   END
775600050408     C                   END
775700050222     C  N40              UPDATE    tigcp000
775800050411     c* scrivo il FIGIC00F chiusura giacenza
775900050411     c                   if        not *in40 and (gcpfas = 040 or
776000050411     c                             gcpfas = 050 )
776100050411     c                   exsr      gicwtr
776200050411     c                   end
776300050316     c                   if        gescomm = 'S'
776400050316     c                   commit
776500050316     c                   endif
776600051013     c* richiamo il pgm di variazione bolla se disposizioni automatiche, se
776700051019     c* data consegna richiesta o f.d. e disposizioni = consegna
776800051013     c                   movel     gcpflr        dgcpflr
776900051013     c                   if        d§gcpdisa = 'S' and flgdis <> ' '
777000051019     c                             and gcpdis = 'C'
777100051013     c                   exsr      srvarbol
777200051013     c                   end
777300941229     C* CONTROLLO SE C'ERANO ALTRE SPEDIZIONI FIGLIE
777400941229     C* GENERATE IN PRECEDENZA
777500950301WW   C                   SETOFF                                       98
777600941229     C     KBOL          CHAIN     FNLBL02L                           21
777700941229     C     *IN21         DOWEQ     *OFF
777800950102     C* ESCLUDO LA BOLLA APPENA CREATA
777900950102     C     LBLNSN        IFNE      §LPNSP
778000950414     C     LBLLAN        ANDEQ     EXLNA
778100950421      * RICH. NOTE ACCREDITO SU PRECEDENTI FIGLIE
778200950307      *************
778300950414     C                   EXSR      RCHNCF
778400950307      *************
778500950102     C                   ENDIF
778600941229     C     KBOL          READE     FNLBL02L                               21
778700941229     C                   ENDDO
778800950301WW   C                   SETOFF                                       98
778900900418     C                   ENDSR
779000051013     C*---------------------------------------------------------------*
779100051013     C     srvarbol      BEGSR
779200051013     C*---------------------------------------------------------------*
779300051013     C                   clear                   DSARBD
779400051013     C                   clear                   DSARBK
779500051013     C                   clear                   DARBT
779600051013     C                   clear                   DSARBG
779700051013     C                   clear                   DSlr48
779800051013     c* disposizioni automatiche per f.d.
779900051013     C                   IF        flgdis = 'F'
780000051017     C                   MOVEL     'E'           D48FFR
780100060314     c                   movel     *blanks       vteld
780200051017     C                   EXSR      PGARBI
780300051013     c                   end
780400051013     c* disposizioni automatiche per data consegna richiesta
780500051013     C                   IF        flgdis = 'D'
780600051013     C                   MOVEL     ' '           D48TLA
780700051013     C                   MOVEL     FILARR        D48FGS
780800051013     C                   MOVE      ARBAAS        D48AAS
780900051013     C                   MOVE      ARBLNP        D48LNP
781000051013     C                   MOVE      ARBNRS        D48NRS
781100051013     C                   MOVE      ARBNSP        D48NSP
781200051013     C                   MOVEL     ARBCBO        D48CBO
781300051109     c                   movel     arbGC1        §BGGC1
781400051109     c                   movel     arbGC2        §BGGC2
781500051109     c                   movel     arbTC1        §BGTC1
781600051109     c                   movel     arbTC2        §BGTC2
781700051013     C                   MOVEL     'CD'          D48CVB
781800051013     C                   MOVEL     'G'           D48TRC
781900051013     C                   MOVEL     'E'           D48FFR
782000051013     C                   MOVEL     ' '           D48ERR
782100051013     C                   MOVEL     *BLANK        D48MSG
782200051013     C                   MOVEL     'A'           D48TBO
782300051109      * se consegna particolare supermercato imposta causale CS
782400051109     c                   if        arbtc1 = 'S' or arbtc2 = 'S'
782500051109     c                   Clear                   Ds7l
782600051109     c                   Movel     '7L'          tblcod
782700051109     c                   Movel(p)  'CS'          tblKey
782800051109     c     kTab          Chain     Tabel00f                           96
782900051109     c                   If        Not *In96 and TblFlg <> ' '
783000051109     c                   Eval      *In96 = *On
783100051109     c                   EndIf
783200051109     c  n96              Movel     TblUni        Ds7l
783300051109     c                   If        Not *In96 and §7LSup <> 'N'
783400051109     c                   Movel     'CS'          D48Cvb
783500051109     c                   Movel     'V'           D48Ffr
783600051109     c                   Movel     'S'           D48F12
783700051109     c                   EndIf
783800051109     c                   endIf
783900051109      *fine supermercato
784000051013     c                   move      d915ndcr      dataiso
784100051013     c                   move      dataiso       §BGDCR
784200051014     c                   if        gnpdif = 915
784300051013     c                   clear                   §BGTCR
784400051013     c                   else
784500051013     c                   eval      §BGTCR='D'
784600051013     c                   end
784700051013     c*
784800051013     C                   MOVEL     DSLR48        KPJBU
784900051013     C                   CALL      'FNLR48R'
785000051013     C                   PARM                    KPJBA
785100051013     C                   PARM                    DSARBD
785200051013     C                   PARM                    DSARBK
785300051013     C                   PARM                    DARBT
785400051013     C                   PARM                    DSARBG
785500051013     C                   PARM                    trul90ds
785600051013     C                   MOVEL     KPJBU         DSLR48
785700051017     c                   end
785800051013     C                   ENDSR
785900050411     C*---------------------------------------------------------------*
786000050411     C* Scrivo figic00f per allineare saltuariamente la sede (TITAS+TNCSB)
786100050411     C*---------------------------------------------------------------*
786200050411     C     gicwtr        BEGSR
786300050411     C                   MOVEL     'CHGC'        GICCAV
786400050411     C                   Z-ADD     AMGUDT        GICDTV
786500050411     C                   TIME                    GICORV
786600050411     C                   Z-ADD     GCPLNPa       GICFEV
786700050411     C                   Z-ADD     GCPLNPa       GICLNP
786800050411     C                   Z-ADD     GCPLNAa       GICLNA
786900050411     C                   Z-ADD     GCPAASa       GICAAS
787000050411     C                   Z-ADD     GCPNRSa       GICNRS
787100050411     C                   Z-ADD     GCPNSPa       GICNSP
787200050411     C                   MOVEL     GCPAGC        GICDGC
787300050411     C                   MOVE      GCPMGC        GICDGC
787400050411     C                   Z-ADD     GCPFGC        GICFGC
787500050411     C                   Z-ADD     GCPNGC        GICNGC
787600050411     C                   Z-ADD     ARBDCM        GICDCH
787700050411     C                   WRITE     FIGIC000
787800050411     C                   ENDSR
787900950214     C*---------------------------------------------------------------*
788000950214     C*   R C H N C F - RICH. N.ACCR. SU FIGLIE PRECEDENTI            *
788100950214     C*---------------------------------------------------------------*
788200950214     C     RCHNCF        BEGSR
788300950214     C                   MOVE      LBLAAN        FAAS
788400950214     C                   MOVE      LBLLPN        FLNP
788500950214     C                   MOVE      LBLNRN        FNRS
788600950214     C                   MOVE      LBLNSN        FNSP
788700160805     C     KARBF         CHAIN     FNARB01L
788800950330      * DEVE ESSERE BLOCCATA E NON DEVE ESSERE GIà STATA CHIESTA N.ACCR
788900160805B01  C                   If        %found(fnarb01l) and arbfbc <> ' '
789000160805     C                             and arbdcm = 0 and arbfip <> 'C'
789300950330      * GUARDO SE è UN ASSEGNATO
789400950330     C                   MOVEL     '3A'          TBLCOD
789500950330     C                   MOVEL     *BLANK        TBLKEY
789600950330     C                   MOVEL     ARBCBO        TBLKEY
789700950330     C     KTAB          CHAIN     TABEL                              21
789800950330     C  N21              MOVEL     TBLUNI        DS3A
789900950330     C  N21              MOVEL     §3ADES        DTBL
790000950330     C  N21              MOVEL     §3ATB1        PORT              1
790100950725B02  C     PORT          IFEQ      'A'
790200950330     C*  Decodifico linea di arrivo
790300950330     C     ARBLNA        CHAIN     AZORG01L                           21
790400950330     C  N21              MOVEL     ORGDES        DLNA             15
790500950330     C   21              MOVEL     *ALL'?'       DLNA
790600950330      *----------------------------------------------------------*
790700000131     C                   CLEAR                   FVA
790800000131     C                   CLEAR                   FSV
790900950214     C                   CLEAR                   AR6NFT
791000991001     C                   MOVEL     '1'           KTRC
791100160805     C     KAR6F         CHAIN     FIAR601L
791200160805B03  C                   IF        %found(fiar601l)
791300000131     C                   MOVEL     AR6SV1        FSV(1)
791400000131     C                   MOVE      AR6VA1        FVA(1)
791500000131     C                   MOVEL     AR6SV2        FSV(2)
791600000303     C                   MOVE      AR6VA2        FVA(2)
791700000131     C                   MOVEL     AR6SV3        FSV(3)
791800000131     C                   MOVE      AR6VA3        FVA(3)
791900000131     C     AR6SV3        IFNE      *BLANK
792000000131     C                   Z-ADD     3             X
792100040123     C     KAR6F         CHAIN     FIAR701L                           43
792200000131     C     *IN43         DOWEQ     *OFF
792300000131     C                   ADD       1             X
792400000131     C                   MOVEL     AR7SVN        FSV(X)
792500000131     C                   MOVE      AR7VAN        FVA(X)
792600040123     C     kar6f         READE     FIAR7000                               43
792700000131     C                   ENDDO
792800000131     C                   ENDIF
792900950330      *----------------------------------------------------------*
793000950330      * CONTROLLO SE BOLLA TASSATA
793100950330      *----------------------------------------------------------*
793200950725B04  C     AR6NFT        IFNE      *ZEROS
793300950214     C*
793400950214     C                   MOVEL     *ALL'?'       DLNP
793500950214     C     ARBLNP        CHAIN     AZORG01L                           21
793600950214     C  N21              MOVEL     ORGDES        DLNP             15
793700950214     C   21              MOVEL     *ALL'?'       DLNP
793800950214     C                   CLEAR                   DNWLNA
793900950214     C*
794000950214     C                   MOVE      AR6DFT        G08INV
794100950214     C                   MOVE      *ZEROS        G08DAT
794200950214     C                   MOVEL     '3'           G08ERR
794300950214     C                   CALL      'XSRDA8'
794400950214     C                   PARM                    WLBDA8
794500950214     C                   Z-ADD     G08DAT        GMADFT            8 0
794600950214     C* MANDARE IN SEDE SEMPRE
794700950214     C                   SETON                                        86
794800950214     C*   Stampa Nota Accredito
794900950214     C                   Z-ADD     ARBKSC        CLIENT            7 0
795000000131     C                   Z-ADD     0             TOTIMP
795100000131     C                   Z-ADD     0             TOTESE
795200000131     C                   Z-ADD     0             TOTNAC
795300991001     C                   Z-ADD     1             XS
795400000131     C     §$2IVA        LOOKUP    FSV(XS)                                20
795500991001     C     *IN20         IFEQ      '1'
795600020108     C     AR6IFT        SUB       AR6IMV        IMPALI
795700991001     C                   END
795800991027     C                   Z-ADD     AR6IMV        INAC
795900000131     C                   MOVE      INAC          WCOMO3            3 0
796000000131     C     WCOMO3        COMP      0                                      11
796100950214     C                   Z-ADD     AR6ALI        ALIQ              3 1
796200020621     C                   EVAL      CdIVA = %EDITC(Aliq:'2')
796300950214     C                   MOVEL     AR6CEI        TPALI             1
796400950214     C     TPALI         IFEQ      *BLANKS
796500000131     C                   Z-ADD     INAC          TOTIMP
796600950214     C                   ELSE
796700000131     C                   Z-ADD     INAC          TOTESE
796800950214     C                   Z-ADD     1             XX
796900950214     C     TPALI         LOOKUP    CEI(XX)                                41
797000950214     C     *IN41         IFEQ      '1'
797100020621     C                   MOVEL     ESE(XX)       CdIVA
797200950214     C                   END
797300950214     C                   MOVEL     'E'           TPALI
797400950214     C                   END
797500011130      * STAMPA DOPPIO IMPORTO SE DIVISA NON DI CONTABILITÀ
797600011130     C                   CLEAR                   WAR6IM
797700011130     C     AR6DIV        IFNE      §GEDMC
797800011130     C                   CLEAR                   YEURDS
797900011130     C                   MOVEL     AR6DIV        YECDVI
798000011130     C                   Z-ADD     INAC          YECIMI
798100011212     C                   MOVEL     §GEDMC        YECDVO
798200011130     C                   MOVEL     'H'           YECTAR
798300011130     C                   CALL      'YEURCO'
798400011130     C                   PARM                    YEURDS
798500011130     C                   Z-ADD     YECIMO        WAR6IM
798600011130     C                   ENDIF
798700011130      *
798800950214     C     TOTIMP        ADD       IMPALI        TOTNAC
798900950214     C                   ADD       TOTESE        TOTNAC
799000000131     C* VERIFICO SE IMPORTI CON DECIMALI
799100000131     C                   MOVE      TOTIMP        WCOMO3            3 0
799200000131     C     WCOMO3        COMP      0                                      08
799300000131     C                   MOVE      IMPALI        WCOMO3            3 0
799400000131     C     WCOMO3        COMP      0                                      09
799500000131     C                   MOVE      TOTESE        WCOMO3            3 0
799600000131     C     WCOMO3        COMP      0                                      10
799700000131     C                   MOVE      TOTNAC        WCOMO3            3 0
799800000131     C     WCOMO3        COMP      0                                      11
799900950214      * FORZO DATI DELLA FIGLIA
800000950214     C                   MOVEL     ARBAAS        G08INV
800100950214     C                   MOVE      ARBMGS        G08INV
800200950214     C                   MOVE      *ZEROS        G08DAT
800300950214     C                   MOVEL     '3'           G08ERR
800400950214     C                   CALL      'XSRDA8'
800500950214     C                   PARM                    WLBDA8
800600950214     C                   Z-ADD     G08DAT        VDSP
800700950214     C                   MOVE      ARBAAS        EXAAS
800800950214     C                   MOVE      ARBLNP        EXLNP
800900950214     C                   MOVE      ARBNRS        EXNRS
801000950214     C                   MOVE      ARBNSP        EXNSP
801100950214     C                   MOVE      ARBLNA        EXLNA
801200950330     C*  Aggiorno bolla x emissione Nota Accredito
801300950330     C                   MOVEL     'S'           ARBFNC
801400950330     C                   MOVEL     'C'           ARBFIP
801500040315      * scrittura file NCR
801600040315     c                   clear                   fnncr000
801700040315     c                   move      '201'         nctsocieta
801800040315     c                   z-add     arblna        nctfil
801900040315     c                   z-add     arblnp        nctpocompl
802000040315     c                   move      amgudt        nctddc
802100040315     c                   if        ar6dft > 0
802200040315     c                   move      ar6dft        nctdrp
802300040315     c                   end
802400040315     c                   z-add     ar6nft        nctnrp
802500040315     c                   z-add     ar6fiv        nctfpn
802600040315     c                   z-add     ar6ift        nctimr
802700040315     c                   move      *zeros        nctksc
802800040315     c                   move      ar6ksc        nctksc
802900040315     c                   move      *zeros        nctscf
803000040315     c                   move      ar6ksc        nctscf
803100040315     c                   move      knmus         nctutente
803200040315     c                   move      'I '          ncttnr
803300040315      * storno spese giacenza
803400040315     c                   move      'D'           ncrrna
803500040429     c                   move      '0'           ncrcmn
803600040315     c                   z-add     ar6imv        ncrim2
803700040315     c                   z-add     ar6ali        ncrali
803800040315     c                   z-add     arblnp        ncrflr
803900040315     c                   z-add     exaas         ncraassto
804000040315     c                   z-add     exlnp         ncrlnpsto
804100040315     c                   z-add     exnrs         ncrnrssto
804200040315     c                   z-add     exnsp         ncrnspsto
804300040429     c                   z-add     0             ncraasrec
804400040429     c                   z-add     0             ncrlnprec
804500040429     c                   z-add     0             ncrnrsrec
804600040429     c                   z-add     0             ncrnsprec
804700040429     c                   z-add     0             nchlnarec
804800040429      * completa viene forzata
804900040315     c                   move      '1'           nchricompl
805000040315
805100040315     c                   move      ar6cei        nchcei
805200040315     c                   z-add     wsvaas        nchaasfgl
805300040315     c                   z-add     wsvlnp        nchlnpfgl
805400040315     c                   z-add     wsvnrs        nchnrsfgl
805500040315     c                   z-add     wsvnsp        nchnspfgl
805600040315     c                   z-add     wsvlna        nchlnafgl
805700040315     c   90              z-add     gcpagc        nchagc
805800040315     c   90              z-add     gcpfgc        nchfgc
805900040315     c   90              z-add     gcpngc        nchngc
806000040315     c                   eval      nchorigine = 'STORNO SPESE DI GIACENZA '
806100040315     c                   write     fnncr000
806200950330     C*----------------------------------------------------------*
806300950330     C*  Segue Fattura: Stampo modulo x avvisare uff.contabilità
806400950330     C*                 della possibilità che venga richiesta
806500950330     C*                 Nota Accredito
806600950330     C*----------------------------------------------------------*
806700950725X04  C                   ELSE
806800950330     C*----------------------------------------------------------*
806900950330     C                   Z-ADD     GCPFGC        CFGC
807000950330     C                   Z-ADD     GCPNGC        CNGC
807100950330     C                   CLEAR                   WLBDA8
807200950330     C                   Z-ADD     GCPMGC        G08INV
807300950330     C                   MOVEL     GCPAGC        G08INV
807400950330     C                   MOVEL     '3'           G08ERR
807500950330     C                   CALL      'XSRDA8'
807600950330     C                   PARM                    WLBDA8
807700950330     C                   Z-ADD     G08DAT        CDGC
807800050222     C                   Z-ADD     GCPAASa       CAAS
807900050222     C                   Z-ADD     GCPLNPa       CLNP
808000050222     C                   Z-ADD     GCPNRSa       CNRS
808100050222     C                   Z-ADD     GCPNSPa       CNSP
808200950330     C                   Z-ADD     ARBAAS        CAAS1
808300950330     C                   Z-ADD     ARBLNP        CLNP1
808400950330     C                   Z-ADD     ARBNRS        CNRS1
808500950330     C                   Z-ADD     ARBNSP        CNSP1
808600950330     C                   Z-ADD     ARBMGS        G08INV
808700950330     C                   MOVEL     ARBAAS        G08INV
808800950330     C                   MOVEL     '3'           G08ERR
808900950330     C                   CALL      'XSRDA8'
809000950330     C                   PARM                    WLBDA8
809100950330     C                   Z-ADD     G08DAT        CDSP1             8 0
809200950330     C                   EXCEPT    AVVNAC
809300950330     C*  Calcolo Varie:
809400950330     C                   MOVEL     CVAR          DIMP             17
809500950330     C     AR6VA1        IFNE      0
809600950330     C                   MOVE      AR6SV1        DIMP
809700950330     C                   Z-ADD     AR6VA1        CIMP              9 0
809800950330     C                   EXCEPT    DETBOL
809900950330     C                   END
810000950330     C     AR6VA2        IFNE      0
810100950330     C                   MOVE      AR6SV2        DIMP
810200950330     C                   Z-ADD     AR6VA2        CIMP
810300950330     C                   EXCEPT    DETBOL
810400950330     C                   END
810500950330     C     AR6VA3        IFNE      0
810600950330     C                   MOVE      AR6SV3        DIMP
810700950330     C                   Z-ADD     AR6VA3        CIMP
810800950330     C                   EXCEPT    DETBOL
810900950330     C*  Se c'è la terza varia controllo se ce ne sono altre
811000160805     C     KARBF         setll     FIAR701L
811001160805     c                   do        *hival
811002160805     C     KARBF         READE     FIAR701L                               18
811100160805     C                   if        %eof(fiar701l)
811101160805     c                   leave
811102160805     c                   end
811200950330     C                   MOVE      AR7SVN        DIMP
811300950330     C                   Z-ADD     AR7VAN        CIMP
811400950330     C                   EXCEPT    DETBOL
811700160805     C                   ENDdo
811800950330     C*  Trasporto
811900950330     C     AR6POR        IFNE      0
812000950330     C                   MOVEL     CTRA          DIMP
812100950330     C                   Z-ADD     AR6POR        CIMP
812200950330     C                   EXCEPT    DETBOL
812300950330     C                   END
812400950330     C*  Inoltro
812500991001     C                   Z-ADD     1             WW
812600991001     C     §$2FIN        LOOKUP    ASV(WW)                                20
812700991001     C     *IN20         IFEQ      '1'
812800950418     C                   MOVEL     CINL          DIMP
812900991001     C                   Z-ADD     AVA(WW)       CIMP
813000950330     C                   EXCEPT    DETBOL
813100950330     C                   END
813200950330     C*  Diritto fisso
813300991001     C                   Z-ADD     1             WW
813400991001     C     §$2VRH        LOOKUP    ASV(WW)                                20
813500991001     C     *IN20         IFEQ      '1'
813600950418     C                   MOVEL     CDIR          DIMP
813700991001     C                   Z-ADD     AVA(WW)       CIMP
813800950330     C                   EXCEPT    DETBOL
813900950330     C                   END
814000950330     C*  Tot.Imponibile
814100950330     C     AR6IMV        IFNE      0
814200950330     C                   Z-ADD     AR6IMV        CIMV
814300950330     C                   EXCEPT    TOTIMV
814400950330     C                   END
814500950330     C*  Imposto dati mittente
814600950330     C                   MOVEL     ARBRSD        CRAG
814700950330     C                   MOVEL     ARBIND        CIND
814800950330     C                   MOVEL     ARBCAD        CCAP
814900950330     C                   MOVEL     ARBLOD        CCIT
815000950330     C                   MOVEL     ARBPRD        CPRV
815100950330     C                   MOVEL     ARBNZD        CNAZ
815200950330     C                   EXCEPT    PIENAC
815300950725E04  C                   END
815400950330      *----------------------------------------------------------*
815500950725E03  C                   ENDIF
815600950725E02  C                   ENDIF
815700040312     C*
815800950330      *----------------------------------------------------------*
815900950214      *  DO PER CONSEGNATA LA BOLLA
816000950214     C                   MOVEL     'Y'           ARBICC
816100950725     C                   MOVEL     'S'           ARBACC
816200950214     C                   MOVEL     'Y'           ARBICA
816300950725     C                   MOVEL     'S'           ARBACA
816400950725     C                   MOVE      AMGUDT        ARBDCM
816500950214     C                   MOVEL     ORAMIN        ARBHMC
816600950214     C                   MOVE      *BLANK        ARBFT1
816700950214     C                   MOVE      *ZEROS        ARBDT1
816800091221     c                   clear                   ARBPDC
816900021009     C                   MOVEL     filarr        ARBPDC
817000091221     C                   MOVE      0998          ARBPDC
817100021009     C                   z-add     filarr        ARBifp
817200021009     C                   z-add     simpou        ARBipp
817300950214     C                   MOVE      *ALL'9'       ARBNDC
817400950214     C                   MOVE      AMGUDT        ARBDDC
817500950214     C                   MOVEL     'S'           ARBFDC
817600950214     C                   MOVE      *ALL'9'       ARBNMI
817700950214     C                   MOVE      AMGUDT        ARBDMI
817800950214     C                   MOVE      *ALL'9'       ARBCUC
817900950214     C                   MOVEL     *BLANK        ARBFBC
818000950214     C                   MOVEL     *BLANK        ARBCMC
818100950214      *
818200950214     C                   UPDATE    FNARB000
818300950214      *
818400950214     C                   ENDIF
818500950214     C*
818600950214     C                   SETOFF                                           86
818700950214     C*
818800950214     C                   ENDSR
818900900418      /EJECT
819000940309     C*----------------------------------------------------*
819100941219     C*    IMPOSTO CHIAVI PER TARIFFA                      *
819200940309     C*----------------------------------------------------*
819300941219     C     TARPCA        BEGSR
819400941219     C* §KSC VIENE IMPOSTATO (V.CHKARB) SE ASSEGNATO O SE C'E' 2°BOLLA
819500941219     C* KCTR    "      "         "            "              "
819600940309     C     §KSC          IFNE      *ZEROS
819700941006     C                   MOVE      §KSC          KKSC
819800940309     C                   ELSE
819900020225      *RICHIAMO PROGRAMMA PER REPERIMENTO INFORMAZIONI TARIFFA
820000020225     C                   CLEAR                   TR27DS
820100021118     C                   MOVE      ARBctr        I27ctb
820200020225     C                   MOVE      ARBTSP        I27TSP
820300020225     C                   MOVE      ARBLNA        I27LNA
820400020225     C                   MOVE      ARBLNP        I27LNP
820500020225     C                   MOVE      KKSC          I27CLI
820600160304     C**                 MOVEL     ARBAAS        I27DTA
820700160304     C**                 MOVE      ARBMGS        I27DTA
820800160304     C                   CALL      'TRUL27R1'
820900020225     C                   PARM                    TR27DS
821000160304     c
821100160304     c                   clear                   trulc7ds
821200160304     c                   eval      ic7tntw=o27ntw
821300160304     c
821400160304     C                   MOVEL     ARBAAS        Ic7DTA
821500160304     C                   MOVE      ARBMGS        Ic7DTA
821600160304     C                   move      arbctr        Ic7ctb
821700160304     c                   call      'TRULC7R'
821800160304     c                   parm                    trulc7ds
821900160304     C                   z-add     Oc7KSCc       kKSC
822000160304     C                   MOVEL     Oc7CTRc       KCTR
822100940309     C                   END
822200940309     C                   ENDSR
822300940309     C*----------------------------------------------------*
822400941219     C*    CALCOLO PROVVIGIONI C/ASSEGNO                   *
822500940309     C*----------------------------------------------------*
822600940309     C     CPROVV        BEGSR
822700980519     C*
822800990930     C                   CLEAR                   DTAS
822900990930     C                   CLEAR                   DTASV
823000980519     C                   MOVEL     KCTR          TASCTR
823100980519     C                   MOVE      KKSC          TASKSC
823200980519     C*
823300980519     C                   MOVEL     ARBCTS        TASCTS
823400980519     C                   MOVEL     ARBCAD        TASCAD
823500980519     C                   MOVEL     ARBFIN        TASFIN
823600980519     C                   MOVEL     ARBCAM        TASCAM
823700980519     C                   MOVEL     ARBFAP        TASFAP
823800980519     C                   MOVEL     ARBTSP        TASTSP
823900980519     C                   MOVEL     ARBTC1        TASTC1
824000980519     C                   MOVEL     ARBTC2        TASTC2
824100980519     C                   MOVEL     ARBFDN        TASFDN
824200980519     C                   MOVEL     ARBVAS        TASVAS
824300980519     C                   Z-ADD     ARBIAS        TASIAS
824400980519     C                   Z-ADD     ARBQFT        TASQFT
824500980519     C                   Z-ADD     ARBNCL        TASNCL
824600160530     C                   Z-ADD     ARBNCL        TASNCLb
824700980519     C                   Z-ADD     AMGDSP        TASDSP
824800980519     C                   Z-ADD     TASDSP        TASDCT
824900980519     C                   Z-ADD     ARBLNP        TASLNP
825000980519     C                   Z-ADD     ARBLNA        TASLNA
825100980519     C                   Z-ADD     ARBVLF        TASVLF
825200980519     C                   Z-ADD     ARBPKF        TASPKF
825300160530     C                   Z-ADD     ARBvlc        TASvlc
825400160530     C                   Z-ADD     ARBncr        TASncr
825500160530     C                   move      ARBfvf        TASfvf
825600160530     C                   Z-ADD     ARBpkb        TASPKb
825700980519     C* Reperisco codice tassazione
825800980519     C                   CLEAR                   TISI95
825900020225     C     TASLNP        CHAIN     AZORG01L                           21
826000020225     C     *IN21         IFEQ      '0'
826100020225     C                   MOVE      ORGDE3        OG143
826200020225     C     §OGNTW        IFEQ      'FED'
826300020225     C                   MOVE      'S'           I95FI1
826400020225     C                   END
826500020225     C                   END
826600980519     C                   MOVEL     '3'           I95TCN
826700980519     C                   MOVEL     ARBCAM        I95CAP
826800980519     C                   MOVEL     ARBNZM        I95NAR
826900980519     C                   MOVEL     ARBPRM        I95PRV
827000980519     C                   MOVEL     ARBLOM        I95LOC
827100980519     C                   MOVEL     ARBINM        I95IND
827200980519     C                   MOVEL     ARBTFP        I95TFP
827300980602     C                   MOVEL     ARBTSP        I95TSP
827400980602     C                   MOVEL     'S'           I95FRE
827500980519     C                   MOVEL     ARBNZM        I95NAR
827600980519     C                   MOVEL     AMGDSP        I95DAT
827700031021     C                   MOVEL     ARBtc1        I95fi2
827800031021     C                   MOVE      ARBtc2        I95fi2
827900980519     C                   CALL      'TISI95R'
828000980519     C                   PARM                    TISI95
828100980519     C                   MOVEL     O95CTS        TASMCT
828200980519     C*
828300040506     C                   MOVEL     ar9vca        TASVCA
828400980519     C                   Z-ADD     §ASS          TASCAS
828500980519     C                   MOVEL     AR9TIC        TASTIC
828600060927     C                   movel     ARBfbr        §asfbr
828700060927     C                   movel     ARBcca        §AScca
828800060927     C                   movel     dtas01        TASflo
828900980519     C*
829000980519     C                   MOVEL     'G'           TASSVA
829100021001     c* imposto peso vdl e colli rilevati
829200030206     c                   z-add     arbncp        Tasncp
829300030206     c                   z-add     arbpkc        Taspkc
829400030206      * Bancali
829500030206     c                   If        %Subst(Arbgva:1:1) = 'B'
829600030206     c                   Clear                   dAr5Ban
829700030805     C                   move      'BAN'         trcar5
829800051013     c     kFiar5        Chain(n)  Fiar501l
829900030206     c                   If        %Found(Fiar501l)
830000030206     c                   Movel     Ar5Uni        dAr5Ban
830100030206     c                   EndIf
830200030206     c                   Eval      TasBan = §Ar5Nba + §ar5Nb2
830300030206     c                   EndIf
830400030805      * Bancali Bartolini
830500030805     c                   If        %Subst(Arbgva:1:1) = 'O'
830600030805     c                   Clear                   dAr5BNB
830700030805     c                   Clear                   tasban
830800030805     C                   move      'BNB'         trcar5
830900051013     c     kFiar5        Chain(n)  Fiar501l
831000030805     c                   If        %Found(Fiar501l)
831100030805     c                   Movel     Ar5Uni        dAr5BNB
831200030805     c                   EndIf
831300030805     c                   Eval      TasNCL = §Ar5Bcor
831400030805     c                   EndIf
831500980519     C*
831600980519     C                   CALL      'TNSF20R'
831700980519     C                   PARM                    KPJBA
831800990930     C                   PARM                    DTAS
831900990930     C                   PARM                    DTASV
832000980519     C*
832100980519     C     TASERR        IFEQ      *BLANKS
832200991028     C                   MOVEL     TASDIV        V6CRVA
832300980519     C                   Z-ADD     1             §F
832400980519     C     TASSVA        LOOKUP    NSV(§F)                                21
832500980519     C   21              Z-ADD     NVA(§F)       §RCA
832600980519     C                   ELSE
832700980519     C                   SETON                                        20
832800980519     C                   END
832900980519     C     ENDCPR        ENDSR
833000951227     C*----------------------------------------------------*
833100951227     C*    CALCOLO PROVVIGIONI in partenza                 *
833200951227     C*----------------------------------------------------*
833300951227     C     PPROVV        BEGSR
833400951227     C                   MOVEL     *ALL'8'       KKSC
833500980519    1C     VNCA          IFNE      *ZEROS
833600951227     C                   Z-ADD     VNCA          §CAS
833700951227     C                   ELSE
833800951227     C                   Z-ADD     VASS          §CAS
833900980519    1C                   END
834000951227     C                   MOVEL     'V'           WCMB              1
834100951227     C                   EXSR      CASLIR
834200980519     C* CALCOLO PROVVIGIONI CON RICHIAMO DEL PGM TNSF20R
834300980519     C*
834400990930     C                   CLEAR                   DTAS
834500990930     C                   CLEAR                   DTASV
834600980519     C                   MOVEL     §LPCTR        TASCTR
834700040506     C                   MOVE      §KSC          TASKSC
834800980519     C*
834900980519     C                   MOVEL     §LPCTS        TASCTS
835000980519     C                   MOVEL     §LPCAD        TASCAD
835100980519     C                   MOVEL     §LPCAM        TASCAM
835200980519     C                   MOVEL     §LPTSP        TASTSP
835300980519     C                   MOVEL     ARBTC1        TASTC1
835400980519     C                   MOVEL     ARBTC2        TASTC2
835500980519     C                   MOVEL     ARBFDN        TASFDN
835600980519     C                   MOVEL     §LPVAS        TASVAS
835700980519     C                   Z-ADD     §LPIAS        TASIAS
835800980519     C                   Z-ADD     ARBQFT        TASQFT
835900980519     C                   Z-ADD     ARBNCL        TASNCL
836000160530     C                   Z-ADD     ARBNCL        TASNCLb
836100980519     C                   Z-ADD     AMGUDT        TASDSP
836200980519     C                   Z-ADD     TASDSP        TASDCT
836300980519     C                   Z-ADD     §LPLNP        TASLNP
836400980519     C                   Z-ADD     VLNA          TASLNA
836500980519     C                   Z-ADD     ARBVLF        TASVLF
836600980519     C                   Z-ADD     ARBPKF        TASPKF
836700160530     C                   Z-ADD     ARBvlc        TASvlc
836800160530     C                   Z-ADD     ARBncr        TASncr
836900160530     C                   move      ARBfvf        TASfvf
837000160530     C                   Z-ADD     ARBpkb        TASPKb
837100980519     C                   CLEAR                   DS3A
837200980519     C                   Z-ADD     1             §F                2 0
837300980519     C     §LPCBO        LOOKUP    CDB(§F)                                21
837400980519     C   21              MOVEL     D3A(§F)       DS3A
837500980519     C   21              MOVEL     §3ATB1        TASTBL
837600980519     C* Reperisco codice tassazione
837700980519     C                   CLEAR                   TISI95
837800020225     C     TASLNP        CHAIN     AZORG01L                           21
837900020225     C     *IN21         IFEQ      '0'
838000020225     C                   MOVE      ORGDE3        OG143
838100020225     C     §OGNTW        IFEQ      'FED'
838200020225     C                   MOVE      'S'           I95FI1
838300020225     C                   END
838400020225     C                   END
838500980519     C                   MOVEL     '3'           I95TCN
838600980519     C                   MOVEL     §LPCAM        I95CAP
838700980519     C                   MOVEL     §LPNZM        I95NAR
838800980519     C                   MOVEL     §LPPRM        I95PRV
838900980519     C                   MOVEL     §LPLOM        I95LOC
839000980519     C                   MOVEL     §LPINM        I95IND
839100980519     C                   MOVEL     ARBTFP        I95TFP
839200980602     C                   MOVEL     ARBTSP        I95TSP
839300980602     C                   MOVEL     'S'           I95FRE
839400980519     C                   MOVEL     §LPNZM        I95NAR
839500980519     C                   MOVEL     AMGUDT        I95DAT
839600031021     C                   MOVEL     ARBtc1        I95fi2
839700031021     C                   MOVE      ARBtc2        I95fi2
839800980519     C                   CALL      'TISI95R'
839900980519     C                   PARM                    TISI95
840000980519     C                   MOVEL     O95CTS        TASMCT
840100980519     C*
840200040506     C                   MOVEL     ar9vca        TASVCA
840300980519     C                   Z-ADD     §ASS          TASCAS
840400980519     C                   MOVEL     VTIC          TASTIC
840500980519     C*
840600980519     C                   MOVE      §LPBAM        TASDDT
840700980519     C                   MOVEL     'G'           TASSVA
840800021001     c* imposto peso vdl e colli rilevati
840900030206     c                   z-add     arbncp        Tasncp
841000030206     c                   z-add     arbpkc        Taspkc
841100060927     C                   movel     ARBfbr        §asfbr
841200060927     C                   movel     ARBcca        §AScca
841300060927     C                   movel     dtas01        TASflo
841400030206      * Bancali
841500030206     c                   If        %Subst(Arbgva:1:1) = 'B'
841600030206     c                   Clear                   dAr5Ban
841700030805     C                   move      'BAN'         trcar5
841800051013     c     kFiar5        Chain(n)  Fiar501l
841900030206     c                   If        %Found(Fiar501l)
842000030206     c                   Movel     Ar5Uni        dAr5Ban
842100030206     c                   EndIf
842200030206     c                   Eval      TasBan = §Ar5Nba + §ar5Nb2
842300030206     c                   EndIf
842400030805      * Bancali Bartolini
842500030805     c                   If        %Subst(Arbgva:1:1) = 'O'
842600030805     c                   Clear                   dAr5BNB
842700030805     c                   Clear                   tasban
842800030805     C                   move      'BNB'         trcar5
842900051013     c     kFiar5        Chain(n)  Fiar501l
843000030805     c                   If        %Found(Fiar501l)
843100030805     c                   Movel     Ar5Uni        dAr5BNB
843200030805     c                   EndIf
843300030805     c                   Eval      TasNCL = §Ar5Bcor
843400030805     c                   EndIf
843500980519     C*
843600980519     C                   CALL      'TNSF20R'
843700980519     C                   PARM                    KPJBA
843800990930     C                   PARM                    DTAS
843900990930     C                   PARM                    DTASV
844000991028     C*
844100991028     C     TASERR        IFEQ      *BLANKS
844200991028     C                   MOVEL     TASDIV        V6CRVA
844300991028     C                   END
844400991028     C*
844500991028     C                   ENDSR
844600940309     C/SPACE 5
844700940309     C*---------------------------------------------------------------*
844800941010     C* N E W C B L   CERCO NUOVO COICE BOLLA
844900940309     C*---------------------------------------------------------------*
845000940309     C     NEWCBL        BEGSR
845100940309     C                   Z-ADD     1             §F                2 0
845200940309     C     NWFGB         LOOKUP    FGB(§F)                                21
845300940309     C   21              MOVEL     CDB(§F)       NWCBL
845400940309     C* SE COME C/S PROPONE COD.=£ IMPONGO COD.BOLLA = A
845500940309     C* IN QUANTO £ è SOLO X LE COMUNIC. GIACENZE
845600941010     C   21NWCBL         IFEQ      '£ '
845700011130     C     NWCBL         OREQ      '$ '
845800941010     C                   MOVEL     'A '          NWCBL
845900940309     C                   END
846000950307     C* SE LA VECCHIA BOLLA ERA =£ ANZICHè C/S=A,  REIMPOSTO £ :
846100950307      * POICHè LE BOLLE £ NON VENGNO TRASMESSE IN SEDE QUINDI
846200950307     C* RICEVEREI LA FIGLIA SENZA AVERE LA MAMMA
846300950331     C                   CLEAR                   DS3A
846400950331     C                   Z-ADD     1             §F
846500950331     C     ARBCBO        LOOKUP    CDB(§F)                                04
846600950331     C   04              MOVEL     D3A(§F)       DS3A
846700950331     C     §3ABSD        IFEQ      'N'
846800950331     C     §3ACFD        ANDNE     *BLANKS
846900950331     C                   MOVEL     §3ACFD        NWCBL
847000950331     C                   ENDIF
847100950307     C*
847200940309     C  N21              SETON                                        30
847300940309     C                   ENDSR
847400940309     C/SPACE 5
847500940309     C*---------------------------------------------------------------*
847600940309     C* C N T V A R   CONTROLLO SE POSSO FARE VARIAZIONE BOLLA
847700940309     C*---------------------------------------------------------------*
847800940309     C     CNTVAR        BEGSR
847900940309     C                   SETOFF                                       2627
848000940309     C                   Z-ADD     1             §F
848100940309     C* CERCO TIPO VARIAZIONE
848200940309     C     §CDVAR        LOOKUP    §TV(§F)                                21
848300940309     C  N21              SETON                                        30
848400940309     C* GUARDO SE DEVO CONTROLLARE LA DATA CONTABILIZZAIONE
848500940309     C   21§DF(§F)       IFEQ      ' '
848600940928      * TASSAZ. 1°BOLLA
848700940928     C     AR6NFT        IFNE      *ZEROS
848800941013     C     AR6DFT        ANDLE     §DTCON
848900991001     C     AR6TRC        ANDEQ     '1'
849000940309     C                   SETON                                        26
849100940309     C                   END
849200940928      * TASSAZ. 2°BOLLA
849300991001     C     AR6NFT        IFNE      *ZEROS
849400991001     C     AR6DFT        ANDLE     §DTCON
849500991001     C     AR6TRC        ANDEQ     '2'
849600940309     C                   SETON                                        27
849700940309     C                   END
849800940309     C                   END
849900940309     C*  NON POSSO  VARIARE CODICE BOLLA :
850000940309     C     §CDVAR        IFEQ      'K8'
850100950725     C*  SE ESISTE FIGLIA NON POSSO VARIARE IL CODICE BOLLA
850200940309     C* SE TASSATO DESTINO SEMPRE NOTA ACCREDITO
850300940309     C     TASDES        IFEQ      '1'
850400940309     C                   SETON                                        27
850500940309     C                   END
850600940309     C                   END
850700940309     C                   ENDSR
850800940310     C*----------------------------------------------------*
850900940310     C*  X L P N O T  - IMPOSTO §LPNOT DATI EX SP.O GIAC.  *
851000940310     C*----------------------------------------------------*
851100940310     C     XLPNOT        BEGSR
851200051005     C                   clear                   §LPNot
851300051005     C                   clear                   §LPNT2
851400940310     C     $GIAC         IFEQ      '1'
851500940310     C                   MOVEL     'GIAC.'       TESTO1
851600940310     C                   MOVE      GCPAGC        GCAAGC
851700940310     C                   MOVE      GCPFGC        GCAFGC
851800940310     C                   MOVE      GCPNGC        GCANGC
851900940310     C                   MOVEL     'LNP. '       TESTO2
852000940310     C                   MOVE      ARBLNP        VARIO
852100940310     C                   MOVEL     RIFGIA        §LPNOT
852200051005      * reperisco note vecchia bolla da inserire dopo i riferimenti
852300051005      * solo in caso di dirottamento o cambio di porto
852400051005     c                   if        vfri = 'D' and wrecsp <> 'S'
852500051005     c                             or export <> vnpc and vnpc <> ' '
852600051006     c                             and wrecsp <> 'S'
852700051005     c                   movel(p)  rifgia        §comodo          70
852800051005     c                   exsr      oldnot
852900051005     c                   endif
853000940310     C                   ELSE
853100150108     C                   clear                   RIFSPE
853200150107      *imposto i riferimenti EX-SPED solo se non è dirottamento
853300150107     c                   if        vfri <> 'D'
853400940310     C                   MOVEL     'EX SPED.'    TESTO3
853500941217     C                   MOVE      KAAS          RAAS
853600941217     C                   MOVE      KLNP          RLNP
853700941217     C                   MOVE      KNRS          RNRS
853800941217     C                   MOVE      KNSP          RNSP
853900940310     C                   MOVEL     RIFSPE        §LPNOT
854000150107     c                   endif
854100051005      * reperisco note vecchia bolla da inserire dopo i riferimenti
854200051005      * solo in caso di dirottamento o cambio di porto
854300051005     c                   if        vfri = 'D' and wrecsp <> 'S'
854400051005     c                             or export <> vnpc and vnpc <> ' '
854500051006     c                             and wrecsp <> 'S'
854600051005     c                   movel(p)  rifspe        §comodo
854700051005     c                   exsr      oldnot
854800051005     c                   endif
854900961009     C* obbligo reso per particolarità giac.
855000961009     C   94              MOVEL     VNOT1         §LPNT2
855100961129     C*  Se devo effettuare un reso di una spedizione in Franco
855200961129     C*  Frontiera e su di essa non ho la particolarità che mi
855300961129     C*  obbliga ad effettuare il reso imposto nelle note cosegna
855400961129     C*  originale era un Franco frontiera
855500961129     C  N94VFRI          IFEQ      'R'
855600961129     C     V4CRCS        ANDNE     'S'
855700961129     C     ARBFBR        ANDEQ     'F'
855800961129     C                   MOVEL     CNOT3         §LPNT2
855900961129     C                   MOVE      CNOT4         §LPNT2
856000940310     C                   END
856100961129     C*
856200961129     C                   END
856300940310     C                   ENDSR
856400051005     C*-----------------------------------------------------
856500051005     C* O l d N o t - reperimento note da vecchia bolla
856600051005     C*-----------------------------------------------------
856700051005     C     oldnot        BEGSR
856800051005
856900051005     C                   MOVE      '8'           §TRC
857000060213     C     KAR4          CHAIN(N)  FiAR401L
857100060213     c                   if        %found(fiar401l)
857200150108     c                   if        §comodo <> *blank
857300051012     C                   eval      §comodo = %trim(§comodo)+ '*' +
857400051005     c                             %trim(ar4not)
857500150108     c                   else
857600150108     C                   eval      §comodo = %trim(ar4not)
857700150108     c                   endif
857800150108     c                   endif
857900051005     C                   MOVE      '9'           §TRC
858000060213     C     KAR4          CHAIN(N)  FiAR401L
858100060213     c                   if        %found(fiar401l)
858200150108     c                   if        §comodo <> *blank
858300051012     C                   eval      §comodo = %trim(§comodo) + '*' +
858400051005     c                             %trim(ar4not)
858500150108     c                   else
858600150108     C                   eval      §comodo = %trim(ar4not)
858700150108     c                   endif
858800051005     c                   end
858900051005     C                   movel     §comodo       §lpNot
859000051005     C                   move      §comodo       §lpNt2
859100051005
859200051005     C                   ENDSR
859300940930     C*-----------------------------------------------------
859400940930     C* C T R D I V - CONTROLLO DIVISA
859500940930     C*-----------------------------------------------------
859600940930     C     CTRDIV        BEGSR
859700941010     C                   CLEAR                   DSCV
859800940930     C                   Z-ADD     1             X
859900940930     C     WVCA          LOOKUP    DIV(X)                                 21
860000941006     C   21              MOVEL     DCV(X)        DSCV
860100940930     C  N21              SETON                                        1967
860200940930     C  N21              MOVEL     ERR(37)       $MSG
860300941217     C  N19§CVFDC        IFNE      'S'
860400940930     C                   MOVE      W0133         W0033             3 3
860500940930     C     W0033         IFNE      0
860600940930     C                   MOVEL     ERR(38)       $MSG
860700940930     C                   SETON                                        1976
860800940930     C                   ENDIF
860900940930     C                   ENDIF
861000940930     C*  C'ERA DIVISA E VARIO importo C/Assegno
861100940930     C*          --------> SE NON IMMESSA NUOVA DIVISA ERRORE
861200940930     C  N19VVCAP         IFNE      *BLANK
861300940930     C     VVCS          ANDEQ     'S'
861400940930     C     VNCA          ANDNE     *ZEROS
861500940930     C     VVCA          ANDEQ     *BLANK
861600940930     C                   MOVEL     ERR(39)       $MSG
861700940930     C                   SETON                                        1967
861800940930     C                   ENDIF
861900941010      * SE  CAMBIO DIVISA OBBLIGO DIGITARE ANCHE IMPORTO
862000941010     C     VNCA          IFEQ      *ZEROS
862100941010     C     VVCA          ANDNE     VVCAP
862200941010     C                   SETON                                        7619
862300941010     C                   MOVEL     ERR(41)       $MSG
862400941010     C                   END
862500940930     C                   ENDSR
862600941216     C*----------------------------------------------------*
862700941216     C*  C H K V A R
862800941216     C*  -----------
862900941216     C*  CERCO LA SIGLA DELLA VARIA LA ELABOARARE
863000941216     C*  MI RITORNA L'INDICE SU CUI LAVORARE
863100941216     C*----------------------------------------------------*
863200941216     C     CHKVAR        BEGSR
863300941216     C                   Z-ADD     1             XS                2 0
863400941216     C     §SIGLA        LOOKUP    WSV(XS)                                21
863500941216     C     *IN21         IFEQ      *OFF
863600941216     C                   Z-ADD     1             XS
863700941216     C     ' '           LOOKUP    WSV(XS)                                21
863800941216     C                   END
863900941216     C                   ENDSR
864000071126     C*-----------------------------------------------------
864100071126     C* CAPOFILA    - reperimento CAPOFILA £6
864200071126     C*-----------------------------------------------------
864300071126     C     Capofila      begsr
864400071127     c                   clear                   FNLV55ds
864500071126     c                   eval      D55tpt =  '6'
864600071126     c                   eval      D55lin =  vlna
864700071126     c                   eval      D55drf =  amgudt
864800071126     c                   call      'FNLV55R'
864900071126     c                   parm                    FNLV55ds
865000071126      * - Reperimento 146ª e 148ª descrizioni dell'organigramma
865100071126      *   (flag Tellus del capofila e flag gestione telefonate GEO)
865200071126if  2c                   if        D55tfa <> vlna and d55err = *blank
865300071127     c     D55tfa        chain     AZORG01l
865400071126if  3c                   if        %found(AZORG01L)
865500071128     c                   movel     ORGde6        Og146
865600071128     c                   movel     ORGde8        Og148
865700071126     c                   move      'X'           Og_da_£6
865800071126e   3c                   endif
865900071126e   2c                   endif
865901160805     c                   endsr
866400940301     C*---------------------------------------------------------------*
866500030715     C*   R O U T I N E   INIZIALE  x trul90ds                        *
866600940301     C*---------------------------------------------------------------*
866700030715     C     srtrul90      BEGSR
866800030715     c                   clear                   trul90ds
866900030715     c                   eval      d90mde = d82moe
867000030715     c                   eval      d90mdb = d82mob
867100030715     c                   eval      d90prb = d82prb
867200030715     c                   eval      d90pre = d82pre
867300030715     c                   eval      d90pss = d82pss
867400030715     c                   eval      d90mdb4 = d82mdb4
867500030715     c                   eval      d90mdb5 = d82mdb5
867600030715     c                   eval      d90psl = d82psl
867700030716     c                   eval      d90bsl = d82bsl
867800030715     c                   eval      d90prb4 = d82prb4
867900030715     c                   eval      d90prb5 = d82prb5
868000030715     c                   endsr
868100080123     C*---------------------------------------------------------------*
868200080123     C*   Controllo §LPCTRA ctr di tassazione porti assegnati in arrivo
868300080123     c*   per Resi: verifico se codice valido anche in arrivo
868400080123     C*---------------------------------------------------------------*
868500080123     C     contrCTRA     Begsr
868600080123     C                   CLEAR                   TR27DS
868700080123     C                   MOVE      §lppkb        I27pkg
868800080123     C                   MOVE      §lptsp        I27TSP
868900080123     C                   MOVE      §lpLNA        I27LNA
869000080123     C                   MOVE      §lplnp        I27LNP
869100080123     C                   MOVE      §lpksca       I27CLI
869200160303     C                   CALL      'TRUL27R1'
869300080123     C                   PARM                    TR27DS
869400080123     c
869500080123     c                   clear                   fnlv59DS
869600080123     C                   MOVEL     'A'           ilv59tbo
869700080123     C                   MOVEL     §lpksca       ilv59KSC
869800080123     C                   MOVEL     KCI           ilv59KCI
869900080123     C                   MOVEL     o27fie        ilv59FIE
870000080123     C                   MOVEL     amgudt        ilv59DSP
870100080123     C                   MOVEL     §lptsp        ilv59TSP
870200080123     C                   MOVEL     'S'           ilv59RIC
870300080123     C                   MOVE      §lpctra       ilv59CTR
870400080123     C*
870500080123     C                   CALL      'FNLV59R'
870600080123     C                   PARM                    fnlv59ds
870700080123     c*
870800080124     c* Se restituisce con errore o non preenti tariffe
870900080124     c*                           cerco una tariffa valida in arrivo
871000080124     c                   if        olv59err<>' '  or olv59tks=' '
871100080123     c                   clear                   ilv59CTR
871200080123     C                   CALL      'FNLV59R'
871300080123     C                   PARM                    fnlv59ds
871400080124     c                   select
871500080124     c* Se non trovata nessuna tariffa --> azzero il codice
871600080124     c                   when      olv59tks=' '
871700080124     c                   clear                   §lpctra
871800080124     c                   clear                   §lpksca
871900080124     c                   when      olv59ct3=' '
872000080123     c                   movel     olv59ctr      §lpctra
872100080124     c                   other
872200080124     c                   clear                   §lpctra
872300080124     c                   endsl
872400080124     c
872500080123     c                   endif
872600080123     c
872700080123     c                   endsr
928100050316     C*----------------------------------------------------*
928200050316     C     geslck        begsr
928300050316     C*----------------------------------------------------*
928400050316     c                   Eval      ul82§rrn = gcpnrr
928500050316     c                   Eval      UL82§FIL = 'TIGCP00F'
928600050316     c                   Eval      UL82§WIN = 'S'
928700050316     c                   Eval      UL82§F7  = 'S'
928800050316     c                   Eval      UL82§num = 3
928900050316     c                   Eval      UL82§att = 3
929000050404     c                   Eval      UL82§mss = 'Si sta bloccando la giacenza Nr.'
929100050404     c                             + %editc(§agc:'Z') +  '  '+ %editc(§fgc:'Z')
929200050404     c                             + '  ' +  %editc(§ngc:'Z') + ' Terminare il -
929300050404     c                             lavoro'
929400050404     c                   Eval      UL82§msw = 'La giacenza Nr. '
929500050404     c                             + %editc(§agc:'Z') +  '  '+ %editc(§fgc:'Z')
929600050404     c                             + '  ' +  %editc(§ngc:'Z') +
929700050404     c                             ' non è manutenzionabile'
929800050404
929900050316      * Effettuo la chiamata al *PGM d utilità
930000050316     c                   Call(e)   'TRUL82R'
930100050316     c                   Parm                    Trul82ds
930200050316     c                   if        ul82§sts = 'A'
930300160729     c                   exsr      srfine
930400050316     c                   endif
930500050316
930600050316     C                   ENDSR
930700141210     C*----------------------------------------------------*
930800141210     C     geslckArb     begsr
930900141210     C*----------------------------------------------------*
931000141210     c                   Eval      ul82§rrn = arbnrr
931100141210     c                   Eval      UL82§FIL = 'FNARB00F'
931200141210     c                   Eval      UL82§WIN = 'S'
931300141210     c                   Eval      UL82§F7  = 'S'
931400141210     c                   Eval      UL82§num = 3
931500141210     c                   Eval      UL82§att = 3
931600141210     c                   Eval      UL82§mss = 'Si sta bloccando la Bolla Nr.'
931700141210     c                             + %editc(kaas:'Z') +  '  '+ %editc(klnp:'Z')
931800141210     c                             +   '  '+ %editc(knrs:'Z')
931900141210     c                             + '  ' +  %editc(knsp:'Z') + ' Terminare il -
932000141210     c                             lavoro'
932100141210     c                   Eval      UL82§msw = 'La Bolla Nr. '
932200141210     c                             + %editc(kaas:'Z') +  '  '+ %editc(klnp:'Z')
932300141210     c                             +   '  '+ %editc(knrs:'Z')
932400141210     c                             + '  ' +  %editc(knsp:'Z') +
932500141210     c                             ' non è manutenzionabile'
932600141210
932700141210      * Effettuo la chiamata al *PGM d utilità
932800141210     c                   Call(e)   'TRUL82R'
932900141210     c                   Parm                    Trul82ds
933000141210     c                   if        ul82§sts = 'A'
933100141210     c                   seton                                        43
933200141210     c                   endif
933300141210
933400141210     C                   ENDSR
933500130920     c*-------------------------------------------------------------
933600130920      *  Se è prevista una LNA da forzare da ORGANIGRAMMA
933700130920     c*-------------------------------------------------------------
933800130920     C     CHG_LNA_xFORZAbegSR
934000160805     c*  Se la LNA prevede su organigramma di essere sostituita
934100160805     c*   x fare un RESO.
934300130920     c     §LPLNA        chain     azorg01L
934500130920     c                   IF        %Found(azorg01L)
934600130920     c                   eval      og143 = orgde3
935300130920     c                   if        §ogLNARES <> *blank
935400130920     c                               and §ogLNARES <> '000'
935600130920     c                   move      §ogLNARES     §LPLNA
935700130920     c                   end
935900130920     C                   END
936100130920     C                   ENDSR
936200140916     c*-------------------------------------------------------------
936300140916      *  Annullamento chi sono bolla mamma su FISGN
936400140916     c*-------------------------------------------------------------
936500140916     C     sr_fisgn      begSR
936600140916
936700140916    1c                   if        §lpctm='2H' or §lpctm='2K'
936800140917     c     ksgn5         setll     fisgn05l
936900140917     c     ksgn5         reade     fisgn05l
937000140917     c                   dow       not %eof(fisgn05l)
937100140916    3c                   if        sgntnp=simfel
937200140916     c                   eval      sgnatb='A'
937300140916     c                   eval      sgndatora = %char(%timestamp:*iso0)
937400140916     C                   CLEAR                   SGNT6A
937500140916     C                   CLEAR                   SGNT6B
937600140916     C                   CLEAR                   SGNT6C
937700140916     C                   CLEAR                   SGNT6D
937800140916     C                   CLEAR                   SGNT6E
937900140916     C                   CLEAR                   SGNT6F
938000140916     c                   update    fisgn000
938100140916    3c                   endif
938200140917     c     ksgn5         reade     fisgn05l
938300140917    2c                   enddo
938400140917     c                   unlock    fisgn05l
938500140916    1c                   endif
938600160729     c                   endsr
938601160729     C*---------------------------------------------------------------*
938602160729     C     srindlna      BEGSR
938603160729     C*---------------------------------------------------------------*
938604160729     C* Imposto parametri per routine controllo Cap
938605160729     C                   CLEAR                   TISI95
938606160729     C                   CLEAR                   FNLV13
938607160729     C                   MOVEL     '7'           I95TCN
938608160729     C                   MOVEL     *BLANK        I95TLA
938609160729     C                   MOVEL     VNZD          I95NAR
938610160729     C                   MOVEL     VIND          I95IND
938611160729     C                   MOVEL     VCAD          I95CAP
938612160729     C                   MOVEL     VLOD          I95LOC
938613160729     C                   MOVEL     VPRD          I95PRV
938614160729     C                   MOVEL     ARBPKF        I95LKG
938615160729     C                   MOVEL     ARBVLF        I95LMC
938616160729     C                   MOVEL     ARBFFD        I95FFD
938617160729     C                   MOVEL     ARBTSP        I95TSP
938618160729     C* CONTROLLO PORTO DELLA BOLLA ORIGINALE
938619160729     C                   Z-ADD     1             §§                3 0
938620160729     C     ARBCBO        LOOKUP    CDB(§§)                                04
938621160729     C                   MOVEL     §3ATB1        WPOR1             1
938622160729     C                   MOVEL     §3ATB2        WPOR2             1
938623160729     C     WPOR1         IFEQ      'A'
938624160729     C     §3ATB1        ANDNE     'AS'
938625160729     C     WPOR2         OREQ      'A'
938626160729     C     §3ATB2        ANDNE     'AS'
938627160729     C     §3ATB1        OREQ      'FS'
938628160729     C                   MOVEL     'A'           I95TPO
938629160729     C                   ELSE
938630160729     C                   MOVEL     'F'           I95TPO
938631160729     C                   END
938632160729     C* CONTROLLO SE LA BOLLA ORIGINALE AVEVA IL C/ASSEGNO
938633160729     C     §3AFCA        IFNE      0
938634160729     C                   MOVEL     'S'           I95FCA
938635160729     C                   ELSE
938636160729     C                   MOVEL     ' '           I95FCA
938637160729     C                   END
938638160729     C* passiamo le consegne particolari al TISI95 se dispos. non sono reso
938639160729     c     vfri          ifne      'R'
938640160729     C                   MOVEL     ARBtc1        I95fi2
938641160729     C                   MOVE      ARBtc2        I95fi2
938642160729     c                   else
938643160729     c                   clear                   I95fi2
938644160729     c                   endif
938645160729     C*
938646160729     C                   MOVEL     'S'           I13AF0
938647160729     C                   MOVEL     'S'           I13AF1
938648160729     C                   MOVEL     'S'           I13SZ2
938649160729     C                   MOVEL     'S'           I13LA3
938650160729     C                   MOVEL     'S'           I13SZ3
938651160729     C                   MOVEL     'S'           I13CNV
938652160729     C                   MOVEL     'S'           I13SZV
938653160729     C* Gestione errori forzabili: se cambiato CAP/Località
938654160729     C* riazzero i valori di errori forzabili
938655160729    2C     VCAD          IFNE      WSVCAP
938656160729     C                   CLEAR                   WFZV
938657160729     C                   CLEAR                   WFZ1
938658160729     C                   CLEAR                   WFZ2
938659160729     C                   CLEAR                   WFZ3
938660160729     C                   MOVEL     VCAD          WSVCAP
938661160729    2C                   ENDIF
938662160729     C* Modifica località
938663160729    2C     VLOD          IFNE      WSVLOC
938664160729     C                   MOVEL     VLOD          WSVLOC
938665160729     C                   CLEAR                   WFZ1
938666160729     C                   CLEAR                   WFZ2
938667160729     C                   CLEAR                   WFZ3
938668160729    2C                   ENDIF
938669160729     C* Modifica PROVINCIA
938670160729    2C     VPRD          IFNE      WSVPRO
938671160729     C                   MOVEL     VPRD          WSVPRO
938672160729     C                   CLEAR                   WFZ3
938673160729    2C                   ENDIF
938674160729     C* Se il livello di affidabilità non è inferiore a 2 e
938675160729     C                   MOVEL     WFZV          E13FZV
938676160729     C                   MOVEL     WFZ1          E13FZ1
938677160729     C                   MOVEL     WFZ2          E13FZ2
938678160729     C                   MOVEL     WFZ3          E13FZ3
938679160729     C                   CALL      'FNLV13R'
938680160729     C                   PARM                    KPJBA
938681160729     C                   PARM                    FNLV13
938682160729     C                   PARM                    TISI95
938683160729     C* Ricontrollo errori
938686160729     C                   MOVEL     E13FZV        WFZV
938687160729     C                   MOVEL     E13FZ1        WFZ1
938688160729     C                   MOVEL     E13FZ2        WFZ2
938689160729     C                   MOVEL     E13FZ3        WFZ3
938690160729     C* reimposto dati da TISI95
938691160729     C     O13ROC        IFEQ      'S'
938692160729     C     O13RON        OREQ      'S'
938693160729     C     O13ROP        OREQ      'S'
938694160729     C     O13ROL        OREQ      'S'
938695160729     C                   MOVEL     O95LOC        VLOD
938696160729     C                   MOVEL     O95PRV        VPRD
938697160729     C                   MOVEL     O95NAR        VNZD
938698160729     C                   MOVEL     O95CAP        VCAD
938699160729     C                   END
938700160729     c* no errori
938702160729     C     O95ERR        IFEQ      *BLANKS
938703160729     C                   MOVEL     O95LNA        §LNA
938704160729      * se solo cambio di porto non varia la zona per la nuova bolla
938705160729     c                   if        vfri = 'C' and export <> vnpc
938706160729     c                   move      *blank        §lpznc
938707160729     c                   move      *blank        §lpcgi
938708160729     c                   else
938709160729     C  n55              MOVEL     O95ZNC        §LPZNC
938710160729     c                   end
938711160729     C* se ex lna DPD e sto eseguendo una riconsegna non reiposto la
938712160729     C* lna in base al Cappario ma rimane uguale alla vecchia
938713160729     C     VFRI          IFEQ      'C'
938714160729     C     ARBLNA        CHAIN     AZORG01L
938715160729     C                   IF        %found(azorg01l) and orgfva <> '*'
938716160729     C                   MOVEL     ORGDE3        OG143
938717160729     C     §OGNTW        IFEQ      'DPD'
938718160729     C     §OGNTW        OREQ      'FED'
938719160729     C                   MOVEL     ARBLNA        §LNA
938720160729     C                   END
938721160729     C                   END
938722160729     C                   END
938725160729      * richiamo pgm x verificare che non sia un destinatario particolare
938726160729      * il lancio viene effettuato per C e D clienti non disk b
938727160729      * e per i resi se non c'è stata già la forzatura della linea arrivo
938728160729     c                   if        vlna = 0
938729160729     c                   clear                   des_par           1
938730160729     c                   clear                   des_parg          1
938731160729     c                   endif
938732160729
938733160729     c                   if        vfri = 'C' and arbnrs = 0 or
938734160729     c                             vfri = 'D' and arbnrs = 0 or
938735160729     c                             vfri = 'R' and vlna = 0
938736160729     c                   clear                   tisit0ds
938737160729     c                   move      i95loc        IT0loc
938738160729     c                   move      i95prv        IT0prv
938739160729     c                   move      i95nar        IT0naz
938740160729     c                   move      i95cap        IT0cap
938741160729     c                   move      i95ind        IT0ind
938742160729     C                   movel     VRSD          IT0rag
938743160729     c                   clear                   tisit0x1ds
938744160729     c                   call      'TISIT5R'
938745160729     c                   parm                    tisit0ds
938746160729     c                   parm                    tisit0x1ds
938747160729      * se non c'è errore risulta trovato quindi imposta flag per bypassare
938748160729      * i controlli sulla linea che viene forzata dalla ds di ritorno
938749160729    2c     ot0err        ifeq      *blanks
938750160729     c                   move      'X'           des_parg
938751160729     c                   movel     ot0flo        ddstflo
938752160729     c                   if        §dstlna  <> *blank
938753160729     c                   move      §DSTLNA       vlna
938754160729     c                   move      §DSTZNC       §lpznc
938755160729     c                   move      'X'           des_par
938756160729     c                   endif
938757160729     c                   endif
938758160729     c                   endif
938759160729     C                   END
938760160729     C* Errore !!!!
938761160729     C     O13ERR        IFNE      ' '
938762160729     C     O13MSG        ANDNE     *BLANKS
938763160729     C* Manca la Ragione sociale
938764160729     C                   SELECT
938765160729     C* Errore su località/città
938766160729     C     O13ERR        WHENEQ    '3'
938767160729     C                   SETON                                        5619
938768160729     C* Errore su Cap
938769160729     C     O13ERR        WHENEQ    '5'
938770160729     C                   SETON                                        5919
938771160729     C* Errore su provincia
938772160729     C     O13ERR        WHENEQ    '4'
938773160729     C                   SETON                                        6519
938774160729     C* Nazione  errata
938775160729     C     O13ERR        WHENEQ    '6'
938776160729     C                   SETON                                        6619
938777160729    2C                   ENDSL
938778160729     C                   MOVEL     O13MSG        $MSG
938779160729     C   19              leavesr
938780160729     C                   END
938782160729     C* Controllo se reimpostata località
938783160729     C     O13ROL        IFEQ      'S'
938784160729     C                   SETON                                        5619
938785160729     C                   MOVEL     ERR(52)       $MSG
938786160729     C                   leavesr
938787160729     C                   END
938788160729     C* Controllo se reimpostata Provincia
938789160729     C     O13ROP        IFEQ      'S'
938790160729     C                   SETON                                        6519
938791160729     C                   MOVEL     ERR(54)       $MSG
938792160729     C                   leavesr
938793160729     C                   END
938794160729     C* Controllo se reimpostato CAP
938795160729     C     O13ROC        IFEQ      'S'
938796160729     C                   SETON                                        5919
938797160729     C                   MOVEL     ERR(53)       $MSG
938798160729     C                   leavesr
938799160729     C                   END
938800160729     C* Controllo se reimpostata Nazione
938801160729     C     O13RON        IFEQ      'S'
938802160729     C                   SETON                                        6619
938803160729     C                   MOVEL     ERR(55)       $MSG
938804160729     C                   leavesr
938805160729     C                   END
938806160729     C* se non ci sono errori imposta linea d'arrivo
938807160729     C                   IF        vlan = 0 and not *in19
938808160729     C                   MOVE      §LNA          VLNA
938809160729     C                   END
938810160729     c* estero: linea d'arrivo forzata
938811160729     c                   if        flgest  = 'E' and
938812160729     c                             (vfri = 'C' or vfri = 'D')
938813160729     c                   if        (vnzd = arbnzd or arbnrs <> 0)
938814160729     c                             and arblna <> 330
938815160729     c                   move      arblna        vlna
938816160729     c                   else
938817160729     c                   clear                   trulflbds
938818160729     c                   movel     Vlna          iflblna
938819160729     c                   move      arblnp        Iflblnp
938820160729     c                   call      'TRULFLBR'
938821160729     c                   parm                    TRULFLBDS
938822160729    2c                   if        Oflblfz>*zeros
938823160729     c                   move      OflbLFZ       vlna
938824160729    2c                   endif
938825160729    2c                   endif
938826160729    2c                   endif
938827160729     c*
938828140916     c                   endsr
938829160729     C*---------------------------------------------------------------*
938830160729     C     srrespo       BEGSR
938831160729     C*---------------------------------------------------------------*
938832160729     c                   move      *blank        viddcli
938833160729     c* dirottamento codificato
938834160729     C     VFRI          ifeq      'D'
938835160729     C     WVARIO        andne     0
938836160729     C     WVARIO        ANDNE     8888
938837160729     C     WVARIO        ANDNE     9999
938838160729     c                   eval      viddtit = 'DIROTTAMENTO'
938839160729      * se è un supermercato la responsabilità è del vettore
938840160729     C                   MOVEL     'A'           §TRC
938841160729     C     KAR4          CHAIN(N)  FiAR401L                           43
938842160729     C     *IN43         IFEQ      '0'
938843160729     C                   MOVEL     AR4NOT        DSBL4A
938844160729     c                   if        arbtc1 = 'S' and §b4aso = '%' or
938845160729     c                             arbtc2 = 'S' and §b4aso = '%'
938846160729     c                   move      'V'           viddcli
938847160729     c                   else
938848160729      * se è disk C o disk A con errato instradamento responsabilità cliente
938849160729     c                   if        §b4aso = '§' or §b4aso = ':' or §b4aso = '%'
938850160729     c                   move      'C'           viddcli
938851160729     c                   else
938852160729     c                   move      'V'           viddcli
938853160729     c                   end
938854160729     c                   end
938855160729     c                   end
938856160729     c* se cliente ma non cambia nulla responsabilità vettore
938857160729     c     viddcli       ifeq      'C'
938858160729     C     VRSD          andeq     WSVRSD
938859160729     C     VRD2          andeq     WSVRD2
938860160729     C     VIND          andeq     WSVIND
938861160729     C     VCAD          andeq     WSVCAD
938862160729     C     VPRD          andeq     WSVPRD
938863160729     C     VLOD          andeq     WSVLOD
938864160729     C     VNZD          andeq     WSVNZD
938865160729     c                   move      'V'           viddcli
938866160729     c                   end
938867160729     C                   END
938868160729     c* x cambio di porto responsabilità cliente
938869160729     C                   if        (vfri = 'C' and vnpc = 'F')
938870160729     c                   move      'C'           viddcli
938871160729     c                   eval      viddtit = 'CAMBIO DI PORTO'
938872160729     c                   end
938873160729     c* emetto il video per chiedere a chi fare l'addebito delle spese
938874160729     C                   if        (vfri = 'C' and vnpc = 'F') OR
938875160729     C                             (vfri = 'D' and WVARIO <> 0 and
938876160729     C                             wvario <> 8888 and wvario <> 9999)
938877160729     c                   do        *hival
938878160729     C                   EXFMT     LG20w01
938879160729     c   kf              leave
938880160729     c                   enddo
938881160729     c                   end
938882160729     c*
938883160729     c                   endsr
938884160729     C*---------------------------------------------------------------*
938885160729     C     srnote        BEGSR
938886160729     C*---------------------------------------------------------------*
938887160729      *carico eventuali note da passare al pgm
938888160729      *solo se è una giacenza
938889160729     c                   clear                   skvdmc
938890160729     c                   if        $giac = '1'
938891160729     c                   do        4             sfl               1 0
938892160729     c     sfl           chain     lg20s05                            60
938893160729     c                   if        not *in60
938894160729     c                   movel     vdmc          skvdmc(sfl)
938895160729     c                   endif
938896160729     c                   enddo
938897160729     c                   endif
938898160729      *
938899160729     c                   clear                   fnlrn6ds
938900160729     C                   move      'I'           in6mod
938901160729     C                   z-add     arbaas        in6AAS
938902160729     C                   z-add     arblnp        in6LNP
938903160729     C                   z-add     arbnrs        in6NRS
938904160729     C                   z-add     arbnsp        in6NSP
938905160729     C                   movel     skvdmc(1)     in6not1
938906160729     C                   movel     skvdmc(2)     in6not2
938907160729     C                   movel     skvdmc(3)     in6not3
938908160729     C                   movel     skvdmc(4)     in6not4
938909160729     C                   movel     'FNLG20R'     in6pgm
938910160729     C                   call      'FNLRN6R'                            98
938911160729     C                   parm                    kpjba
938912160729     C                   parm                    fnlrn6ds
938913160729     c                   endsr
938914160729     C*---------------------------------------------------------------*
938915160729     C*   CALL21: RICHIAMO VIDEATA FAX
938916160729     C*---------------------------------------------------------------*
938917160729     C     CALL21        BEGSR
938918160729     C*-                                                   *
938919160729     c                   move      'L'           finep
938920160729     c                   z-add     filarr        codpo
938921160729     c                   move      '2'           tpcoda
938922160729     c                   clear                   nomcoda
938923160729     c                   clear                   libcoda
938924160729     c                   clear                   esito
938925160729     c                   call      'TRULOUTQ'
938926160729     c                   parm                    finep
938927160729     c                   parm                    codpo
938928160729     c                   parm                    tpcoda
938929160729     c                   parm                    nomcoda
938930160729     c                   parm                    libcoda
938931160729     c                   parm                    esito
938932160729     c                   if        esito <> '0' or nomcoda = *blank
938933160729     c                   eval      nomcoda = 'QDKT'
938934160729     c                   endif
938935160729     C*
938936160729     C                   CLEAR                   FNLG21
938937160729     C     $GIAC         IFEQ      '1'
938938160729     C                   Z-ADD     GCPAASa       D21AAS
938939160729     C                   Z-ADD     GCPLNPa       D21LNP
938940160729     C                   Z-ADD     GCPNRSa       D21NRS
938941160729     C                   Z-ADD     GCPNSPa       D21NSP
938942160729     C                   Z-ADD     GCPAGC        D21AGC
938943160729     C                   Z-ADD     GCPFGC        D21FGC
938944160729     C                   Z-ADD     GCPNGC        D21NGC
938945160729     C                   Z-ADD     0             D21FRG
938946160729     C                   MOVEL     'G'           D21TPR
938947160729     C                   MOVEL     VFRI          D21DIS
938948160729     C                   ELSE
938949160729     C                   Z-ADD     ARBAAS        D21AAS
938950160729     C                   Z-ADD     ARBLNP        D21LNP
938951160729     C                   Z-ADD     ARBNRS        D21NRS
938952160729     C                   Z-ADD     ARBNSP        D21NSP
938953160729     C                   MOVEL     'C'           D21TPR
938954160729     C                   MOVEL     VFRI          D21DIS
938955160729     C                   END
938956160729     C                   MOVE      nomcoda       D21PRF
938957160729     C*  Se lna Dpd imposto apposito flag al pgm gestione
938958160729     C*  invio fax in quanto cambia il modo di reperire il
938959160729     C*  il codice dell'anagrafica
938960160729     C     §OGntw        IFEQ      'DPD'
938961160729     C                   MOVEL     'S'           D21DPD
938962160729     C                   END
938963160729     C*  Richiamo videata x fax
938964160729     C                   MOVEL     FNLG21        KPJBU
938965160729     C                   CALL      'FNLG21C'
938966160729     C                   PARM                    KPJBA
938967160729     C*
938968160729     C                   ENDSR
938969160804     C*---------------------------------------------------------------*
938970160804     C     srnc          BEGSR
938971160804     C*---------------------------------------------------------------*
938972160804     C* RICH. NOTA ACCREDITO
938973160804     C     *IN26         IFEQ      '1'
938974160804     C     AR6TRC        ANDEQ     '1'
938975160804     C     AR6FIV        ANDEQ     ARBLNA
938976160804     C     ARBICA        ANDNE     'Y'
938977160804     C                   SETON                                        32
938978160804     C                   Z-ADD     AR6IMV        §INAC1
938979160804     C                   END
938980160804     C     *IN27         IFEQ      '1'
938981160804     C     AR6TRC        ANDEQ     '2'
938982160804     C     ARBICA        ANDNE     'Y'
938983160804     C                   SETON                                        37
938984160804     C                   Z-ADD     AR6IMV        §INAC2
938985160804     C                   END
938986160804     C                   ENDSR
938987160729     C*---------------------------------------------------------------*
938988160804     C     srnewbolla    BEGSR
938989160729     C*---------------------------------------------------------------*
938990160804     C                   SETON                                        31
938991160804     C                   MOVEL     *BLANK        NWTB2
938992160804     C* NUOVO PORTO = F O ERA GIA FRANCO
938993160804     C     VNPC          IFEQ      'F'
938994160804     C     VNPC          OREQ      ' '
938995160804     C     EXPORT        ANDEQ     'F'
938996160804      *
938997160804     C                   MOVEL     'F7'          NWTB1
938998160804     C* SE C'E' C/ASSEGNO
938999160804     C     VNCA          IFGT      0
939000160804     C     VVCS          OREQ      'N'
939001160804     C     VASS          ANDGT     0
939002160804     C*  E C'ERA LA 2°BOLLA E NON HO VARIATO C/ASS. E NON PAGA "M"
939003160804     C*   O DICO CHE IL DEST. PAGA PROVV.C/A
939004160804     C     §3ATB2        IFEQ      'A3'
939005160804     C     VVCS          ANDEQ     'N'
939006160804     C     VPPC          ANDNE     'M'
939007160804     C     VPPC          OREQ      'D'
939008160804     C                   MOVEL     'A3'          NWTB2
939009160804     C                   END
939010160804     C                   END
939011160804     C                   END
939012160804     C* NUOVO PORTO = A O ERA GIA ASSEGNATO
939013160804     C     VNPC          IFEQ      'A'
939014160804     C     VNPC          OREQ      ' '
939015160804     C     EXPORT        ANDEQ     'A'
939016160804     C                   MOVEL     'A2'          NWTB1
939017160804     C                   MOVEL     '  '          NWTB2
939018160804     C                   END
939019160804     C* CERCO NUOVO CODICE BOLLA
939020160804     C     VNCA          IFGT      0
939021160804     C     VVCS          OREQ      'N'
939022160804     C     VASS          ANDGT     0
939023160804     C     NWTB2         IFEQ      *BLANK
939024160804     C                   MOVEL     '1'           NWFCA
939025160804     C                   ELSE
939026160804     C                   MOVEL     '2'           NWFCA
939027160804     C                   END
939028160804     C                   ELSE
939029160804     C                   MOVEL     '0'           NWFCA
939030160804     C                   END
939031160804     C                   MOVEL     *BLANK        NWCBL
939033160804     C                   MOVEL     *BLANK        §LPCBO
939034160804     C                   EXSR      NEWCBL
939036160804     c* per bolle FW (servizio c/a) se dirottamento forzo sempre lo stesso
939037160804     c* codice bolla
939038160804     c                   if        arbcbo = 'FW'
939039160804     c                   movel     arbcbo        nwcbl
939040160804     c                   setoff                                       30
939041160804     c                   end
939042160804     C  N30              MOVEL     NWCBL         §LPCBO
939043160804     C     §LPCBO        IFEQ      'A '
939044160804     C     ARBCBO        ANDNE     'A '
939045160804     C                   MOVEL     '$'           §LPCBO
939046160804     C                   END
939047160804     C* SPOSTO C/A SU NUOVA BOLLA
939048160804     C     NWFCA         IFEQ      '1'
939049160804     C     NWFCA         OREQ      '2'
939050160804     C     VASS          IFGT      *ZEROS
939051160804     C     VVCS          ANDEQ     'N'
939052160804     C                   Z-ADD     AR9CAS        §LPCAS
939053160804     C                   MOVEL     AR9VCA        §LPVCA
939054160804     C                   MOVEL     AR9TIC        §LPTIC
939055160804     C                   END
939056160804     C     VNCA          IFGT      *ZEROS
939057160804     C                   Z-ADD     VNCA          §LPCAS
939058160804     C                   MOVEL     VVCA          §LPVCA
939059160804     C                   MOVE      VTIC          §LPTIC
939060160804     C                   END
939061160804     C                   ELSE
939062160804     C                   Z-ADD     0             §LPCAS
939063160804     C                   MOVEL     *BLANKS       §LPVCA
939064160804     C                   MOVEL     *BLANKS       §LPTIC
939065160804     C                   END
939066160804     C*  LP
939067160804     C                   MOVE      EXLNA         §LPLNP
939068160805      * codice trattamento merce per SEGNACOLLI NUOVI
939069160805     c                   exsr      srctm2k
939070160804     C* IMPOSTO §LPNOT
939071160804     C                   EXSR      XLPNOT
939072160804     C                   ENDSR
939073160804     C*---------------------------------------------------------------*
939074160804     C     srdsk         BEGSR
939075160804     C*---------------------------------------------------------------*
939076160804     C                   SETON                                        80
939077160804     C* GUARDO SE DEVO VARIARE LA BOLLA, PRIMA IMPOSTO COME ERA
939078160804     c                   clear                   dsarbk
939079160804     C                   MOVEL     §3ATB1        NWTB1
939080160804     C                   MOVEL     §3ATB2        NWTB2
939081160804     C                   MOVEL     §3AFCA        NWFCA
939082160804     c* controllo se esiste già il blocco di fatturaz., se no utilizzo K8
939083160804     C                   MOVEL     'K8'          §CDVAR
939084160804     C                   EXSR      CNTVAR
939085160804     c* in inserimento c/ass oppure se già fatturato sempre KB
939086160804     c                   if        okvar = 'N' or
939087160804     c                             (vvcs = 'S' and vass <> 0 and vnca = 0)
939088160804     C                   MOVEL     'KB'          §CDVAR
939089160804     c                   end
939090160804     c* in annullo c/ass se già presente il blocco di fatturaz. nota credito
939091160804     c                   if        vvcs = 'S' and vass <> 0 and vnca = 0 and
939092160804     c                             okvar = 'N'
939093160804     c                   exsr      srnc
939094160804     c                   leavesr
939095160804     c                   end
939096160804     c* aggiungo o vario c/ass
939097160804     c                   if        vvcs = 'S'
939098160804     c                   eval      §bkcas = vnca
939099160804     c                   eval      §bkvca = vvac
939100160804     c                   else
939101160804     c* nessuna variazione ma vecchia bolla con c/ass
939102160804     C     §3AFCA        IFNE      0
939103160804     c                   eval      §bkcas = vass
939104160804     c                   eval      §bkvca = vvacp
939105160804     c                   end
939106160804     c                   end
939107160804     c* tipo bolla 1
939108160804     C     EXPORT        IFEQ      'A'
939109160804     C     WEXTB1        ANDNE     'AS'
939110160804     C                   MOVEL     'AS'          NWTB1
939111160804     C                   SETON                                        39
939112160804     C                   END
939113160804     C* SE ERA FRANCO E NUOVO PORTO =A --> TRASFORMO IN CONTO SERVIZIO
939114160804     C     EXPORT        IFEQ      'F'
939115160804     C     WEXTB1        ANDNE     'FS'
939116160804     C     VNPC          ANDEQ     'A'
939117160804     C                   MOVEL     'FS'          NWTB1
939118160804     C                   SETON                                        39
939119160804     C                   END
939120160804     c* tipo bolla 2
939121160804     C*  ANNULLAMENTO DEL CONTRASSEGNO
939122160804     C                   IF        vvcs = 'S' and vnca = 0  and
939123160804     C                             (§3atb2 = 'A3' or §3atb2 = 'AP')
939124160804     C                   MOVEL     '  '          NWTB2
939125160804     C                   SETON                                        39
939126160804     C                   END
939127160804     C* AGGIUNTO IL CONTRASSEGNO e le provvigioni le paga il destinatario
939128160804     C                   IF        vnca > 0 and §3afca = 0  and
939129160804     C                             EXPORT = 'F'  and vppc = 'D'
939130160804     C                   MOVEL     'AP'          NWTB2
939131160804     C                   SETON                                        39
939132160804     C                   END
939133160804     C* TRASFORMO IN COD.BOLLA "AP" PERCHE' IN CASO DI
939134160804     C* RICONSEGNA CON LA CREAZIONE DI UNA BOLLA LEGATA
939135160804     C* LA GESTIONE DEGLI INCASSI SI ASPETTA DI AVERE IL
939136160804     C* C/ASS. SULLA BOLLA MAMMA E L'UNICO TIPO BOLLA
939137160804     C* MAMMA CHE NON PREVEDE ADDEBITO DI PROVVIGIONI E' "F1+AP"
939138160804     c*
939139160804     C* SE CAMBIA CHI PAGA LE provvigioni o le SPESE di giacenza
939140160804     c*
939141160804     c* c/assegno sulla prima bolla
939142160804     C     EXPORT        IFEQ      'F'
939143160804     C     §3AFCA        IFEQ      1
939144160804     C     VPPC          ANDEQ     'D'
939145160804     C                   MOVEL     'AP'          NWTB2
939146160804     C                   SETON                                        39
939147160804     C                   END
939148160804     c* c/ass sulla seconda bolla
939149160804     C     §3AFCA        IFEQ      2
939150160804     C     §3ATB2        ANDNE     'AP'
939151160804     C     VPPC          ifeq      'M'
939152160804     C     VVCS          oreq      'N'
939153160804     C     *IN33         ANDEQ     '1'
939154160804     C     VNCA          orne      *ZEROS
939155160804     C     *IN33         ANDEQ     '1'
939156160804     C                   MOVEL     'AP'          NWTB2
939157160804     C                   SETON                                        39
939158160804     C                   END
939159160804     C                   END
939160160804     C                   END
939161160804     c* imposto il flag del contrassegno
939162160804     C                   IF        vvcs = 'S' and vnca = 0
939163160804     C                   MOVEL     '0'           NWFCA
939164160804     c                   else
939165160804     C                   MOVEL     '1'           NWFCA
939166160804     c                   if        nwtb2 <> ' '
939167160804     C                   MOVEL     '2'           NWFCA
939168160804     c                   end
939169160804     c                   end
939170160804     C* RICERCA NUOVO TIPO BOLLA
939171160804     C                   MOVEL     *BLANK        NWCBL
939172160804     C                   MOVEL     *BLANK        §BKCBN
939173160804     C                   EXSR      NEWCBL
939174160804     C  N30              MOVEL     NWCBL         §BKCBN
939175160804     C                   ENDSR
939176160804     C*---------------------------------------------------------------*
939177160804     C*   R O U T I N E   INIZIALE                                    *
939178160804     C*---------------------------------------------------------------*
939179160804     C     DEFCAM        BEGSR
939180160729      * MI VIENE PASSATO IL NR. GIACENZA
939181160729     C     D82GIA        IFEQ      'S'
939182160729     C                   SETON                                        90
939183160729     C                   MOVE      D82AGC        §AGC
939184160729     C                   MOVE      D82FGC        §FGC
939185160729     C                   MOVE      D82NGC        §NGC
939186160729     C                   MOVEL     '1'           $GIAC             1
939187160729     C                   OPEN      tigcp02L
939188160729     C                   OPEN      tignp01L
939189160729     C                   OPEN      TITGC01L
939190160729     C                   ELSE
939191160729     C                   MOVEL     '0'           $GIAC
939192160729     C                   END
939193160729     C* FINTO OPEN X RENDERE SHR YES  I FILE
939194160729     C                   OPEN      FNLV22P
939195160729     C                   Z-ADD     1             CODUT
939196160729     C                   CALL      'X§PARUT'
939197160729     C                   PARM                    UTEDSE
939198160729     C                   MOVEL     RAGUT         RSUT             20
939199160805     C                   eval      vprogr = '* FNLG20R *'
939201160729     C*
939202160729     C                   MOVEL     REC80         CNCR80
939203160729      * TRSFORMO DATA CONTABILIZZ. DA 6 A 8
939204160729     C                   MOVEL     DTU(10)       W20               2 0
939205160729     C     W20           IFGT      50
939206160729     C     1900          ADD       W20           W40               4 0
939207160729     C                   ELSE
939208160729     C     2000          ADD       W20           W40
939209160729     C                   END
939210160729     C                   MOVE      DTU(10)       §DTCON            8 0
939211160729     C                   MOVEL     W40           §DTCON
939213160729      * se è il primo livello
939214160729     C                   SETOFF                                       2101
939215160729     c                   if        simtpp = '1'
939216160729     C                   SETON                                        2101
939217160729     c                   end
939219160729     C                   MOVE      simpou        FILARR
939220160729     C* SE PASSATO IMPOSTO FILIALE IN GESTIONE
939221160729     C     D82FGS        IFGT      '000'
939222160729     C                   MOVEL     D82FGS        FILARR
939223160729     C                   ENDIF
939224160729     C                   SETOFF                                       02
939225160729     C* OVRPRTF  N. ACCREDITO
939226160729     C                   MOVEL     'ACCR.REC'    MODNAC            9
939227160729     C                   Z-ADD     80            LUNCMD           15 5
939228160729     C                   MOVEA     MODNAC        CMN(63)
939229160729     C                   CALL      'QCMDEXC'
939230160729     C                   PARM                    CMN
939231160729     C                   PARM                    LUNCMD
939232160729     C                   OPEN      LET132
939234160729     C* AGGANCIO LA EI
939235160729     C                   MOVEL     'EI'          TBLCOD
939236160729     C                   CLEAR                   ESE
939237160729     C                   MOVEA     *BLANKS       CEI
939238160729     C     KTABP         CHAIN     TABEL00F                           21
939239160729     C                   Z-ADD     1             XX
939240160729     C     *IN21         DOWEQ     '0'
939241160729     C     XX            ANDLE     50
939242160729     C                   MOVEL     TBLUNI        DSEI
939243160729     C                   MOVE      §EIJEI        ESE(XX)
939244160729     C                   MOVEL     TBLKEY        CEI(XX)
939245160729     C                   ADD       1             XX
939246160729     C     KTABP         READE     TABEL00F                               21
939247160729     C                   END
939249160729     C* CARICO DATI DELLE VARIE CON FLG SOGGETTE O NO TOTALE IMP.
939250160729     C                   MOVEL     'CC'          TBLCOD
939251160729     C                   CLEAR                   IPB
939252160729     C                   MOVEA     *BLANKS       CCC
939253160729     C     KTABP         CHAIN     TABEL00F                           21
939254160729     C                   Z-ADD     1             XX                3 0
939255160729     C     *IN21         DOWEQ     '0'
939256160729     C     XX            ANDLE     100
939257160729     C* Controllo se sto esaminando un rcd di una varia
939258160729     C                   MOVEL     TBLKEY        WCOMO5            5
939259160729     C     WCOMO5        IFEQ      'VARIE'
939260160729     C                   MOVEL     TBLUNI        DSCC
939261160729     C                   MOVE      §CCFL6        IPB(XX)
939262160729     C                   MOVE      TBLKEY        CCC(XX)
939263160729     C                   ADD       1             XX
939264160729     C                   END
939265160729     C     KTABP         READE     TABEL00F                               21
939266160729     C                   END
939267160729     C* AGGANCIO LA £6 E RECUPERO LE LINEE A LORO ASSOCIATE
939268160729     C                   CLEAR                   TR06DS
939269160729     C                   MOVEL     '£6'          D06COD
939270160729     C                   MOVEL     filarr        D06KEY
939271160729     C                   MOVEL     TR06DS        KPJBU
939272160729     C                   CALL      'TRUL06R'
939273160729     C                   PARM                    KPJBA
939274160729     C                   MOVEL     KPJBU         TR06DS
939275160729     C                   MOVEA     LIN           l£6
939277160729     C* CERCO IL CODICE DELL'OPERATORE
939278160729     C                   CLEAR                   Tibs36Ds
939279160729     c                   EVAL      I36ute = knmus
939280160729     c                   EVAL      I36Tla = 'L'
939281160729     C                   CALL      'TIBS36R'
939282160729     C                   PARM                    Tibs36Ds
939284160729     C*--- RICERCA CAPOCONTI------------------------------------------*
939285160729     C                   DO        50            X                 2 0
939286160729     C                   MOVE      TCU(X)        TCUDS
939287160729     C     F56           CABNE     'CG'          END1
939288160729     C     F4            COMP      '1'                                    21
939289160729     C** 1 CLIENTI   21
939290160729     C     F3            COMP      '0'                                242425
939291160729     C** 0 ITALIA   25
939292160729     C** 1 ESTERO   24
939293160729     C   21
939294160729     CAN 24              Z-ADD     KCU(X)        KCE               4 0
939295160729     C   21
939296160729     CAN 25              Z-ADD     KCU(X)        KCI               4 0
939297160729     C     END1          TAG
939298160729     C                   END
939299160729     C*---------------------------------------------------------------*
939300160729     C*  KLIST
939301160729     C     KGIA          KLIST
939302160729     C                   KFLD                    §AGC
939303160729     C                   KFLD                    §FGC
939304160729     C                   KFLD                    §NGC
939305160729     C                   KFLD                    §FRG
939306160729     C                   MOVE      0             §FRG
939307160729     C*
939308160729     C     KNOT          KLIST
939309160729     C                   KFLD                    §AGC
939310160729     C                   KFLD                    §FGC
939311160729     C                   KFLD                    §NGC
939312160729     C                   KFLD                    §FRG
939313160729     C                   KFLD                    §FAS
939314160729     C*
939315160729     C     Kar5          KLIST
939316160729     C                   KFLD                    ARBAAS
939317160729     C                   KFLD                    ARBLNP
939318160729     C                   KFLD                    ARBNRS
939319160729     C                   KFLD                    ARBNSP
939320160729     C                   KFLD                    ar5trd
939321160729     C*
939322160729     C                   MOVE      0             §FRG
939323160729     C                   MOVE      020           §FAS              3 0
939324160729     C*
939325160729     C     KGIP          KLIST
939326160729     C                   KFLD                    §AGC
939327160729     C                   KFLD                    §FGC
939328160729     C                   KFLD                    §NGC
939329160729     C*
939330160729     C     KBOL          KLIST
939331160729     C                   KFLD                    KAAS
939332160729     C                   KFLD                    KLNP
939333160729     C                   KFLD                    KNRS
939334160729     C                   KFLD                    KNSP
939335160729     C     KAR6          KLIST
939336160729     C                   KFLD                    KAAS
939337160729     C                   KFLD                    KLNP
939338160729     C                   KFLD                    KNRS
939339160729     C                   KFLD                    KNSP
939340160729     C                   KFLD                    KTRC
939341160729     C     KBLP          KLIST
939342160729     C                   KFLD                    §LPAAS
939343160729     C                   KFLD                    §LPLNP
939344160729     C                   KFLD                    §LPNRS
939345160729     C                   KFLD                    §LPNSP
939346160729     C*
939347160729     C     KBOLG         KLIST
939348160729     C                   KFLD                    KAAS
939349160729     C                   KFLD                    KLNP
939350160729     C                   KFLD                    KNRS
939351160729     C                   KFLD                    KNSP
939352160729     C                   KFLD                    PROGR             2 0
939353160729     C*
939354160729     C     KBOLF         KLIST
939355160729     C                   KFLD                    ULFAAS
939356160729     C                   KFLD                    ULFLNP
939357160729     C                   KFLD                    ULFNRS
939358160729     C                   KFLD                    ULFNSP
939359160729     C     KAR6F         KLIST
939360160729     C                   KFLD                    FAAS
939361160729     C                   KFLD                    FLNP
939362160729     C                   KFLD                    FNRS
939363160729     C                   KFLD                    FNSP
939364160729     C                   KFLD                    KTRC
939365160729     C*
939366160729     C     KAR4          KLIST
939367160729     C                   KFLD                    KAAS
939368160729     C                   KFLD                    KLNP
939369160729     C                   KFLD                    KNRS
939370160729     C                   KFLD                    KNSP
939371160729     C                   KFLD                    §TRC
939372160729     C*
939373160729     C     Kar4dup       KLIST
939374160729     C                   KFLD                    arbAAS
939375160729     C                   KFLD                    arbLNP
939376160729     C                   KFLD                    arbNRS
939377160729     C                   KFLD                    arbNSP
939378160729     C*
939379160729     C     kar4l         klist
939380160729     C                   kfld                    arbAAS
939381160729     C                   kfld                    arbLNP
939382160729     C                   kfld                    arbNRS
939383160729     C                   kfld                    arbNSP
939384160729     C                   kfld                    §TRCl             1
939385160729     C                   move      'L'           §TRCl
939386160729     C*
939387160729     C     KAR4lbl       KLIST
939388160729     C                   KFLD                    LBLAAOsv
939389160729     C                   KFLD                    LBLLPOsv
939390160729     C                   KFLD                    LBLNROsv
939391160729     C                   KFLD                    LBLNSOsv
939392160729     C                   KFLD                    §TRC
939393160729     C*
939394160729     C     Kfiar5        KLIST
939395160729     C                   KFLD                    KAAS
939396160729     C                   KFLD                    KLNP
939397160729     C                   KFLD                    KNRS
939398160729     C                   KFLD                    KNSP
939399160729     C                   KFLD                    trcar5            3
939400160729     C*
939401160729     C     KARBF         KLIST
939402160729     C                   KFLD                    FAAS
939403160729     C                   KFLD                    FLNP
939404160729     C                   KFLD                    FNRS
939405160729     C                   KFLD                    FNSP
939406160729     C*
939407160729     C     KTAB          KLIST
939408160729     C                   KFLD                    CODUT
939409160729     C                   KFLD                    TBLCOD
939410160729     C                   KFLD                    TBLKEY
939411160729     C*
939412160729     C     KTBE          KLIST
939413160729     C                   KFLD                    TBECOD
939414160729     C                   KFLD                    TBEKE1
939415160729     C                   KFLD                    TBEKE2
939416160729     C*
939417160729     C     KTABP         KLIST
939418160729     C                   KFLD                    CODUT
939419160729     C                   KFLD                    TBLCOD
939420160729     C*
939421160729     C     KTAM          KLIST
939422160729     C                   KFLD                    ilv59ksc
939423160729     C                   KFLD                    CTRD51            3 0
939424160729     C                   KFLD                    olv59prg
939425160729     C*
939426160729     C     KTGC          KLIST
939427160729     C                   KFLD                    KKSC
939428160729     C                   KFLD                    KCTR
939429160729     C                   KFLD                    KPRG
939430160729     C*
939431160729     C     KANM          KLIST
939432160729     C                   KFLD                    ARTFLS
939433160729     C                   KFLD                    ARTLNA
939434160729     C                   KFLD                    ARTNRS
939435160729     C                   KFLD                    ARTNSC
939436160729     C*
939437160729     C     KEVB1         KLIST
939438160729     C                   KFLD                    KAAS1
939439160729     C                   KFLD                    KLNP1
939440160729     C                   KFLD                    KNRS1
939441160729     C                   KFLD                    KNSP1
939442160729     C                   KFLD                    KDEV
939443160729     C                   KFLD                    KHEV
939444160729     C*
939445160729     C     KEVB2         KLIST
939446160729     C                   KFLD                    KAAS1
939447160729     C                   KFLD                    KLNP1
939448160729     C                   KFLD                    KNRS1
939449160729     C                   KFLD                    KNSP1
939450160729     C*
939451160729     C     KANM1         KLIST
939452160729     C                   KFLD                    KAAS
939453160729     C                   KFLD                    KLNP
939454160729     C                   KFLD                    KNRS
939455160729     C                   KFLD                    KNSP
939456160729     C                   KFLD                    KCAA              3 0
939457160729      *
939458160729     C     KFNSPE        KLIST
939459160729     C                   KFLD                    WFLS              1
939460160729     C                   KFLD                    WCODIC            7 0
939461160729     C                   KFLD                    WLUO              3
939462160729     C                   MOVE      'L'           WFLS
939463160729     C                   MOVE      '300'         WLUO
939464160729     C     Ksp2          KLIST
939465160729     C                   KFLD                    WCODIC
939466160729     C                   KFLD                    WLUO
939467160729     C                   KFLD                    Wtip              2
939468160729     c     karn          klist
939469160729     c                   kfld                    arbaas
939470160729     c                   kfld                    arblnp
939471160729     c                   kfld                    arbnrs
939472160729     c                   kfld                    arbnsp
939473160729     c                   kfld                    cdnk              3
939474160729     c                   kfld                    gstk              1
939475160729     C*
939476160729     C     Kpct          KLIST
939477160729     C                   KFLD                    KAAS
939478160729     C                   KFLD                    KLNP
939479160729     C                   KFLD                    KNRS
939480160729     C                   KFLD                    KNSP
939481160729     C                   KFLD                    pcttrd
939482160729
939483160729     C     Ksgn5         KLIST
939484160729     C                   KFLD                    §LPAASm
939485160729     C                   KFLD                    §LPLNPm
939486160729     C                   KFLD                    §LPNRSm
939487160729     C                   KFLD                    §LPNSPm
939488160729      *
939489160729     C                   MOVEL     '0'           $FINE             1
939490160729     C                   MOVEL     '0'           $ERR              1
939491160729     C                   MOVEL     '0'           $F01              1
939492160729     C                   MOVEL     '0'           $F02              1
939493160729     C                   MOVEL     '0'           $F03              1
939494160729     C                   MOVEL     '0'           $F11              1
939495160729     C                   MOVEL     '0'           $OKF01            1
939496160729     C                   MOVEL     '0'           $OKF02            1
939497160729     C                   MOVEL     '0'           $OKF03            1
939498160729     C                   MOVEL     'V.BOLLA'     TESNWB
939499160729     C                   MOVEL     'L.A.'        TE2NWB
939500160729     C                   MOVEL     *ZEROS        §QUATR            4
939501160729     C     *LIKE         DEFINE    ARBRSD        WSVRSD
939502160729     C     *LIKE         DEFINE    AR4RD2        WSVRD2
939503160729     C     *LIKE         DEFINE    ARBIND        WSVIND
939504160729     C     *LIKE         DEFINE    ARBCAD        WSVCAD
939505160729     C     *LIKE         DEFINE    ARBPRD        WSVPRD
939506160729     C     *LIKE         DEFINE    ARBLOD        WSVLOD
939507160729     C     *LIKE         DEFINE    ARBNZD        WSVNZD
939508160729     C     *LIKE         DEFINE    GCPAAS        KAAS
939509160729     C     *LIKE         DEFINE    ARBMGS        EXMGS
939510160729     C     *LIKE         DEFINE    ARBLNA        EXLNA
939511160729     C     *LIKE         DEFINE    ARBAAS        EXAAS
939512160729     C     *LIKE         DEFINE    ARBLNP        EXLNP
939513160729     C     *LIKE         DEFINE    ARBNRS        EXNRS
939514160729     C     *LIKE         DEFINE    ARBNSP        EXNSP
939515160729     C     *LIKE         DEFINE    ARBAAS        WSVAAS
939516160729     C     *LIKE         DEFINE    ARBLNP        WSVLNP
939517160729     C     *LIKE         DEFINE    ARBNRS        WSVNRS
939518160729     C     *LIKE         DEFINE    ARBNSP        WSVNSP
939519160729     C     *LIKE         DEFINE    ARBLNA        WSVLNA
939520160729     C     *LIKE         DEFINE    LBLLPO        WSVLPO
939521160729     C     *LIKE         DEFINE    GCPLNP        KLNP
939522160729     C     *LIKE         DEFINE    GCPNRS        KNRS
939523160729     C     *LIKE         DEFINE    GCPNSP        KNSP
939524160729     C     *LIKE         DEFINE    EVBAAS        KAAS1
939525160729     C     *LIKE         DEFINE    EVBLNP        KLNP1
939526160729     C     *LIKE         DEFINE    EVBNRS        KNRS1
939527160729     C     *LIKE         DEFINE    EVBNSP        KNSP1
939528160729     C     *LIKE         DEFINE    EVBDEV        KDEV
939529160729     C     *LIKE         DEFINE    EVBHEV        KHEV
939530160729     C     *LIKE         DEFINE    AR4NOT        AR4RD2
939531160729     C     *LIKE         DEFINE    GCPAGC        §AGC
939532160729     C     *LIKE         DEFINE    GCPFGC        §FGC
939533160729     C     *LIKE         DEFINE    GCPNGC        §NGC
939534160729     C     *LIKE         DEFINE    GCPFRG        §FRG
939535160729     C     *LIKE         DEFINE    GCPTFT        §TFT
939536160729     C     *LIKE         DEFINE    ARBKSC        §KSC
939537160729     C     *LIKE         DEFINE    ARBKSC        KKSC
939538160729     C     *LIKE         DEFINE    ARBCTR        KCTR
939539160729     C     *LIKE         DEFINE    TGCPRG        KPRG
939540160729     C     *LIKE         DEFINE    GCPDDM        AMGDSP
939541160729     C     *LIKE         DEFINE    GCPDDM        AMGDDM
939542160729     C     *LIKE         DEFINE    GCPDDM        AMGDSD
939543160729     C     *LIKE         DEFINE    GCPDDM        AMGDSA
939544160729     C     *LIKE         DEFINE    GCPDDM        AMGDTG
939545160729     C     *LIKE         DEFINE    G08TGI        §GGDA
939546160729     C     *LIKE         DEFINE    G08TGI        §GGA
939547160729     C     *LIKE         DEFINE    G08TGI        §GG
939548160729     C     *LIKE         DEFINE    GCPSGR        §SGR
939549160729     C     *LIKE         DEFINE    GCPGGS        §GGPAG
939550160729     C     *LIKE         DEFINE    GCPGGS        §GGPAR
939551160729     C     *LIKE         DEFINE    GCPGGS        §GGSOS
939552160729     C     *LIKE         DEFINE    GCPGGS        §GGSAV
939553160729     C     *LIKE         DEFINE    GCPFRG        §NRG
939554160729     C     *LIKE         DEFINE    AR9CAS        §CAS
939555160729     C     *LIKE         DEFINE    AR9CMB        §CMB
939556160729     C                   MOVE      *ZEROS        §ASS             13 0
939557160729     C     *LIKE         DEFINE    TASVA1        §VARR
939558160729     C     *LIKE         DEFINE    VRCA          EXRCA
939559160729     C     *LIKE         DEFINE    VRCA          §RCA
939560160729     C     *LIKE         DEFINE    VRCA          WRCA
939561160729     C     *LIKE         DEFINE    AR6IFT        §INAC1
939562160729     C     *LIKE         DEFINE    AR6IFT        §INAC2
939563160729     C     *LIKE         DEFINE    TBLCOD        §COD
939564160729     C     *LIKE         DEFINE    TBLKEY        §KEY
939565160729     C                   MOVEL     *BLANKS       §DES             60
939566160729     C     *LIKE         DEFINE    ARBCBO        NWCBL
939567160729     C     *LIKE         DEFINE    D48CVB        §CDVAR
939568160729     C     *LIKE         DEFINE    D48CVB        §§KCVB
939569160729     C     *LIKE         DEFINE    D48CVB        §§TCVB
939570160729     C     *LIKE         DEFINE    AR4TRC        §TRC
939571160729     C     *LIKE         DEFINE    AR6TRC        KTRC
939572160729     C     *LIKE         DEFINE    ARBLNA        §LNA
939573160729     C     *LIKE         DEFINE    ARBCAD        COMCAP
939574160729     C     *LIKE         DEFINE    ARBCAD        SAVCAD
939575160729     C     *LIKE         DEFINE    ARBNZD        SAVNZD
939576160729     C     *LIKE         DEFINE    ARBIND        SAVIND
939577160729     C     *LIKE         DEFINE    ARBLNA        SAVLNA
939578160729     C     *LIKE         DEFINE    ARBLOD        SAVLOD
939579160729     C     *LIKE         DEFINE    ARBPRD        SAVPRD
939580160729     C     *LIKE         DEFINE    ARBRSD        SAVRSD
939581160729     C     *LIKE         DEFINE    ARBRSD        SAVRD2
939582160729     C     *LIKE         DEFINE    AR6VA1        §ANTEP
939583160729     C     *LIKE         DEFINE    ARBLNP        ULFLNP
939584160729     C     *LIKE         DEFINE    ARBAAS        ULFAAS
939585160729     C     *LIKE         DEFINE    ARBNRS        ULFNRS
939586160729     C     *LIKE         DEFINE    ARBNSP        ULFNSP
939587160729     C     *LIKE         DEFINE    ARBLNP        FLNP
939588160729     C     *LIKE         DEFINE    ARBAAS        FAAS
939589160729     C     *LIKE         DEFINE    ARBNRS        FNRS
939590160729     C     *LIKE         DEFINE    ARBNSP        FNSP
939591160729     C     *LIKE         DEFINE    GCPFGC        CFGC
939592160729     C     *LIKE         DEFINE    GCPNGC        CNGC
939593160729     C     *LIKE         DEFINE    GCPDUR        CDGC
939594160729     C     *LIKE         DEFINE    GCPAAS        CAAS
939595160729     C     *LIKE         DEFINE    GCPLNP        CLNP
939596160729     C     *LIKE         DEFINE    GCPNRS        CNRS
939597160729     C     *LIKE         DEFINE    GCPNSP        CNSP
939598160729     C     *LIKE         DEFINE    ARBAAS        CAAS1
939599160729     C     *LIKE         DEFINE    ARBLNP        CLNP1
939600160729     C     *LIKE         DEFINE    ARBNRS        CNRS1
939601160729     C     *LIKE         DEFINE    ARBNSP        CNSP1
939602160729     C     *LIKE         DEFINE    AR6IMV        CIMV
939603160729     C     *LIKE         DEFINE    AR6IFT        CIFT
939604160729     C     *LIKE         DEFINE    ARBRSD        CRAG
939605160729     C     *LIKE         DEFINE    ARBIND        CIND
939606160729     C     *LIKE         DEFINE    ARBCAD        CCAP
939607160729     C     *LIKE         DEFINE    ARBLOD        CCIT
939608160729     C     *LIKE         DEFINE    ARBPRD        CPRV
939609160729     C     *LIKE         DEFINE    ARBNZD        CNAZ
939610160729     C     *LIKE         DEFINE    I95CAP        WSVCAP
939611160729     C     *LIKE         DEFINE    I95LOC        WSVLOC
939612160729     C     *LIKE         DEFINE    VPRD          WSVPRO
939613160729     C     *LIKE         DEFINE    E13FZV        WFZV
939614160729     C     *LIKE         DEFINE    E13FZ1        WFZ1
939615160729     C     *LIKE         DEFINE    E13FZ2        WFZ2
939616160729     C     *LIKE         DEFINE    E13FZ3        WFZ3
939617160729     C     *LIKE         DEFINE    vteld         SAVteld
939618160729     C     *LIKE         DEFINE    vref          SAVref
939619160729      * UDATE
939620160729     C                   TIME                    WHHUDT           14 0
939621160729     C                   MOVE      WHHUDT        G08DAT
939622160729     C                   MOVE      *ZEROS        G08INV
939623160729     C                   MOVEL     *BLANK        G08ERR
939624160729     C                   CALL      'XSRDA8'
939625160729     C                   PARM                    WLBDA8
939626160729     C                   Z-ADD     G08INV        AMGUDT            8 0
939627160729     C                   Z-ADD     G08DAT        GMAUDT            8 0
939628160729     C                   MOVEL     AMGUDT        ANNO              4 0
939629160729     C                   MOVEL     WHHUDT        ORAMIN            4 0
939632160729     C* REPERISCO LA DIVISA NELLA MONETA CORRENTE
939633160729     C                   MOVEL     'GED'         TBECOD
939634160729     C                   MOVEL     *BLANKS       TBEKE1
939635160729     C                   MOVEL     *BLANKS       TBEKE2
939636160729     C                   MOVEL     '1'           TBEKE1
939637160805     C     KTBE          CHAIN     TNTBE01L
939638160805     c                   if        %found(tntbe01l)
939639160805     C                   MOVEL     TBEUNI        DGED
939640160805     c                   else
939641160805     c                   clear                   dged
939642160805     c                   end
939643160729     C* CARICO I TIPI BOLLA
939644160729     C                   MOVEL     '3A'          TBLCOD
939645160729     C                   MOVEL     *BLANK        TBLKEY
939646160729     C     KTAB          SETLL     TABEL
939647160729     C     *IN21         DOUEQ     '1'
939648160729     C     KTABP         READE     TABEL                                  21
939649160729     C  N21TBLFLG        IFEQ      *BLANK
939650160729     C                   MOVEL     TBLUNI        DS3A
939651160729     C     §3afgc        IFeq      *blank
939652160729     C                   MOVEL     §3ATB1        NWTB1
939653160729     C                   MOVEL     §3ATB2        NWTB2
939654160729     C                   MOVEL     §3AFCA        NWFCA
939655160729     C                   MOVEL     §3AFAN        NWFAN
939656160729     C                   MOVEL     §3AFTD        NWFTD
939657160729     C                   ADD       1             §F
939658160729     C                   MOVEL     NWFGB         FGB(§F)
939659160729     C                   MOVEL     TBLKEY        CDB(§F)
939660160729     C                   MOVEL     TBLUNI        D3A(§F)
939661160729     C                   END
939662160729     C                   END
939663160729     C                   END
939664160729     C* CARICO I TIPI BOLLA
939665160729     C                   Z-ADD     0             XY
939666160729     C                   MOVEL     '2A'          TBLCOD
939667160729     C                   MOVEL     *BLANK        TBLKEY
939668160729     C     KTAB          SETLL     TABEL
939669160729     C     *IN21         DOUEQ     '1'
939670160729     C     KTABP         READE     TABEL                                  21
939671160729     C  N21TBLFLG        IFEQ      *BLANK
939672160729     C                   ADD       1             XY
939673160729     C                   MOVEL     TBLKEY        C2A(XY)
939674160729     C                   MOVEL     TBLUNI        D2A(XY)
939675160729     C                   END
939676160729     C                   END
939677160729     C* CARICO I TIPI VARIAZIONE BOLLA
939678160729     C                   MOVE      *ZERO         §F
939679160729     C                   MOVEL     '7L'          TBLCOD
939680160729     C                   MOVEL     *BLANK        TBLKEY
939681160729     C     KTAB          SETLL     TABEL
939682160729     C     *IN21         DOUEQ     '1'
939683160729     C     KTABP         READE     TABEL                                  21
939684160729     C  N21TBLFLG        IFEQ      *BLANK
939685160729     C                   MOVEL     TBLUNI        DS7L
939686160729     C                   ADD       1             §F
939687160729     C                   MOVEL     TBLKEY        §TV(§F)
939688160729     C                   MOVEL     §7LVTA        §DF(§F)
939689160729     C                   END
939690160729     C                   END
939691160729     C* CARICO I CODICI DIVISA E RELATIVI CAMBI
939692160729     C                   MOVE      *ZERO         §F
939693160729     C                   MOVEL     'CV'          TBLCOD
939694160729     C                   MOVEL     *BLANK        TBLKEY
939695160729     C     KTAB          SETLL     TABEL
939696160729     C     *IN21         DOUEQ     '1'
939697160729     C     KTABP         READE     TABEL                                  21
939698160729     C  N21TBLFLG        IFEQ      *BLANK
939699160729     C                   ADD       1             §F
939700160729     C                   MOVEL     TBLKEY        DIV(§F)
939701160729     C                   MOVEL     TBLUNI        DCV(§F)
939702160729     C                   END
939703160729     C                   END
939704160729     C* CARICO SIGLE DELLE VARIE
939705160729     C                   MOVEL     '$2'          TBLCOD
939706160729     C                   MOVEL     *BLANK        TBLKEY
939707160729     C                   MOVEL     '1'           TBLKEY
939708160729     C     KTAB          CHAIN     TABEL                              21
939709160729     C  N21TBLFLG        COMP      *BLANK                             2121
939710160729     C  N21              MOVEL     TBLUNI        DS$2
939711160729     C* CARICO seconda chiave per limitazioni giacenze
939712160729     C                   MOVEL     '2G'          TBLCOD
939713160729     C                   MOVEL     *BLANK        TBLKEY
939714160729     C                   MOVEL     '2'           TBLKEY
939715160729     C     KTAB          CHAIN     TABEL                              21
939716160729     C  N21TBLFLG        COMP      *BLANK                             2121
939717160729     C  N21              MOVEL     TBLUNI        DS2g2
939718160729     C                   ENDSR
939719160729      *-------------------------------------------------------
939720160729     C     srfine        begsr
939721160729      *-------------------------------------------------------
939722160729      *RICHIAMO PROGRAMMA PER CHIUSURA FILE
939723160729     C                   CLEAR                   TR27DS
939724160729     C                   MOVE      'C'           I27TLA
939725160729     C                   CALL      'TRUL27R1'
939726160729     C                   PARM                    TR27DS
939727160729      *
939728160729     C                   CLEAR                   TRulc7ds
939729160729     C                   MOVE      'C'           Ic7TLA
939730160729     C                   CALL      'TRULC7R'
939731160729     C                   PARM                    trulc7ds
939732160729      *
939733160729     C                   CLEAR                   fnlv59ds
939734160729     C                   MOVE      'C'           Ilv59tla
939735160729     C                   CALL      'FNLV59R'
939736160729     C                   PARM                    fnlv59ds
939737160729     c
939738160729     C                   CLEAR                   fnlg09ds
939739160729     C                   MOVEL     'C'           d09TLA
939740160729     c                   movel     fnlg09ds      kpjbu
939741160729     C                   CALL      'FNLG09R'
939742160729     C                   PARM                    kpjba
939743160729     c
939744160729     C                   CALL      'FNLG25R'
939745160729     C                   PARM                    recordlbl
939746160729     C                   PARM      'C'           i25tla
939747160729     c*
939748160729     C                   MOVEL     fnlg82        KPJBU
939749160729     C                   SETON                                        LR
939750160729     c                   return
939751160729     c                   endsr
939752991028     O*-------------------------------------------------------------
939753991028     OFNBLP000  E            AGGTRA
939754991028     O                       BLPDT1
939755991028     O                       BLPFT1
939757040429     Olet132    E            RECTES         1 01
939758040428     O                                         +  1 '* FNLG20R *'
939759040428     O                       GMAUDT            +  1 '  /  /    '
939760040428     O          E            RECTES         1 10
939761040428     O                                         + 30 'SPETT. P.OPERATIVO DI'
939800040428     O                       DLNP              +  1
939900040428     O          E            RECTES         2
940000040428     O                                         + 30 'ATT.NE UFF.COMMER'
940100040428     O                                         +  0 'CIALE'
940200040428     O          E            RECTES         3
940300040428     O                       DLNA              +  1
940400040428     O                       UDATE         Y   +  1
940500040428     O          E            DIRTES         3 18
940600040428     O                                         +  1 'OGGETTO:'
940700040428     O                                         +  1 '- RECUPERO SPESE DIROTTA'
940800040428     O                                         +  0 'MENTO - NR.'
940900040428     O                       filarr            +  1
941000040428     O                                         +  0 '/'
941100040428     O                       §RIREC        Z   +  0
941200040428     O          E            RICTES         3 18
941300040428     O                                         +  1 'OGGETTO:'
941400040428     O                                         +  1 '- RECUPERO SPESE TRASPOR'
941500040428     O                                         +  0 'TO - NR.'
941600040428     O                       filarr            +  1
941700040428     O                                         +  0 '/'
941800040428     O                       §RIREC        Z   +  0
941900040428     O                                         +  0 '-'
942000040428     O          E            RECDET         2
942100040428     O                                         +  1 'SPEDIZIONE'
942200040428     O                       EXAAS             +  1
942300040428     O                       EXLNP             +  1
942400040428     O                       EXNRS             +  1
942500040428     O                       EXNSP         Z   +  1
942600040428     O                       VDSP              +  1 '  /  /    '
942700040428     O                                         +  1 ' / '
942800040428     O                       ARBLNA            +  1
942900040428     O                       DTBL              +  2
943000040428     O          E            RECDET         2
943100040428     O                                         +  1 'FATTURA'
943200040428     O                       AR6FIV        Z   +  1
943300040428     O                       AR6NFT        Z   +  1
943400040428     O                       GMADFT            +  1 '  /  /    '
943500040428     O                                         +  6 'CLIENTE'
943600040428     O                       ARBKSC        Z   +  1
943700040428     O          E            RECDET         1
943800040428     O                                         +  1 'MITTENTE'
943900040428     O                                           49 'DESTINATARIO'
944000040428     O          E            RECDET         1
944100040428     O                       ARBRSM            +  1
944200040428     O                       ARBRSD            +  1
944300040428     O          E            RECDET         1
944400040428     O                       ARBINM            +  1
944500040428     O                       ARBIND            +  1
944600040428     O          E            RECDET         1
944700040428     O                       ARBLOM            +  1
944800040428     O                       ARBLOD            +  1
944900040428     O          E            RECDET         2
945000040428     O                       ARBCAM            +  1
945100040428     O                       ARBPRM            +  1
945200040428     O                       ARBNZM            +  1
945300040428     O                       ARBCAD              46
945400040428     O                       ARBPRD            +  1
945500040428     O                       ARBNZD            +  1
945600040428     O          E            RECDET         2
945700040428     O                                         +  1 'RIF. MITT.'
945800040428     O                       ARBRMN        Z   +  1
945900040428     O                                         +  1 'COLLI'
946000040428     O                       ARBNCL        2   +  1
946100040428     O                                         +  1 'PESO'
946200040428     O                       ARBPKF        2   +  1
946300040428     O                                         +  1 'VOLUME'
946400040428     O                       ARBVLF        2   +  1
946500040428     O          E            DIRDET         3
946600040428     O                                         +  1 'LA MERCE E'''
946700040428     O                                         +  1 'STATA DIROTTATA A '
946800040428     O                       DNWLNA            +  1
946900040428     O          E            RICDET         3
947000040428     O                                         +  1 'LA MERCE E'''
947100040428     O                                         +  1 'STATA CONSEGNATA IN FRAN'
947200040428     O                                         +  0 'CO A:'
947300040428     O                       DNWLNA            +  1
947400040428     O          E            RECPRO         3
947500040428     O                                         +  1 'INOLTRE E'''
947600040428     O                                         +  1 'STATO AGGIUNTO IL C/ASS'
947700040428     O                                         +  0 'EGNO CON PROVV.AL MITTE'
947800040428     O                                         +  0 'NTE'
947900040428     O          E            RECFIN         3
948000040428     O                     31                  +  1 'CON SPEDIZIONE:'
948100040428     O                     31§LPLNP            +  7
948200040428     O                     31§LPAAS            +  1
948300040428     O                     31§LPNRS            +  1
948400040428     O                     31§LPNSP        Z   +  1
948500040428     O          E            RECFIN         1
948600040428     O                                         +  1 'NOTE:'
948700040428     O                       VNOT1             +  1
948800040428     O          E            RECFIN         2
948900040428     O                       VNOT2             +  7
949000040428     O          E            RECFIN         3
949100040428     O                                         +  1 'SI PREGA IL P.OPERATIVO'
949200040428     O                                         +  1 'DI'
949300040428     O                       DLNP              +  1
949400040428     O                                         +  1 'DI FARE BOLLA DI RECUP'
949500040428     O                                         +  0 'ERO.'
949600040428     O          E            RECFIN      2  1
949700040428     O                                         + 30 'DISTINTI SALUTI'
949800040428     O          E            RECFIN         3
949900040428     O                       DLNA              + 30
950000040428     O          E   N32N37N69RECFIN      2  1
950100040428     O                                         +  1 '- LA SPEDIZIONE IN'
950200040428     O                                         +  1 'OGGETTO E'''
950300040428     O                                         +  1 'STATA STORNATA.'
950400040428     O          E    32      RECFIN      2  1
950500040428     O         OR    37
950600040428     O                                         +  1 '- PER LA SPEDIZIONE IN'
950700040428     O                                         +  1 'OGGETTO E'''
950800040428     O                                         +  1 'STATA RICHIESTA NOTA'
950900040428     O                                         +  1 'ACCREDITO.'
951000040428     O          E    32      RECFIN      2  1
951100040428     O         OR    37
951200040428     O                                         +  1 '- V.NR. RICHIESTA'
951300040428     O                       ar6fiv            +  1
951400040428     O                                         +  0 '/'
951500950329     O*------------------------------------------------*
951600970410     O*  Avviso possibile richiesta nota accredito
951700950329     O*------------------------------------------------*
951800950329     O          E            AVVNAC         1 01
951900950329     O                                         +  1 '* FNLG20R *'
952000950329     O                       GMAUDT            +  1 '  :  :    '
952100950329     O          E            AVVNAC      2  1
952200950329     O                                           43 'ATT.NE UFFICIO'
952300950418     O                                           56 'CONTABILITA'''
952400950329     O          E            AVVNAC         3
952500961002     O                                           50 'P.OPERATIVO DI'
952600950329     O                       DLNA              +  1
952700950329     O          E            AVVNAC      1  2
952800950329     O                                           24 'OGGETTO: Bolle di recupe'
952900950329     O                                           48 'ro Spese Giacenza segue '
953000950329     O                                           55 'fattura'
953100950329     O          E            AVVNAC         1
953200950329     O                                           31 'Nell''esecuzione disposi'
953300950329     O                                           51 'zioni della giacenza'
953400950329     O                       CFGC          Z     55
953500950329     O                                           56 '/'
953600950329     O                       CNGC          Z     63
953700950329     O                                           67 'del'
953800950329     O          E            AVVNAC         1
953900950329     O                       CDGC          8     10
954000950329     O                                           31 'aperta su spedizione'
954100950329     O                       CAAS          Z     36
954200950329     O                                           37 '/'
954300950329     O                       CLNP          Z     40
954400950329     O                                           41 '/'
954500950329     O                       CNRS          Z     43
954600950329     O                                           44 '-'
954700950329     O                       CNSP          Z     51
954800950329     O                                           67 'è stata creata'
954900950329     O          E            AVVNAC         2
955000950329     O                                           24 'la seguente bolla di rec'
955100950329     O                                           30 'upero:'
955200950329     O          E            AVVNAC         0
955300950329     O                       CLNP1         Z      6
955400950329     O                                            7 '/'
955500950329     O                       CNRS1         Z      9
955600950329     O                                           10 '-'
955700950329     O                       CNSP1         Z     17
955800950329     O                                           21 'del'
955900950330     O                       CDSP1         8     32
956000950330     O          E            DETBOL         1
956100950330     O                       DIMP                51
956200950329     O                                           56 '= £.'
956300950329     O                       CIMP          2     67
956400950330     O          E            TOTIMV         1
956500950330     O                                           56 '= £.'
956600950330     O                                           67 '-------------'
956700950330     O          E            TOTIMV         1
956800950330     O                                           51 'Totale Imponibile'
956900950330     O                                           56 '= £.'
957000950330     O                       CIMV          2     67
957100950330     O          E            TOTIMV         1
957200950330     O                                           67 '============='
957300950329     O          E            PIENAC      1  1
957400950329     O                                           24 'tassata come segue fattu'
957500950329     O                                           37 'ra al cliente'
957600950329     O          E            PIENAC         1
957700950329     O                       CRAG                40
957800950329     O          E            PIENAC         1
957900950329     O                       CIND                40
958000950329     O          E            PIENAC         2
958100950418     O                       CCAP                14
958200950418     O                       CCIT                50
958300950418     O                       CPRV                53
958400950418     O                       CNAZ                57
958500950329     O          E            PIENAC         1
958600950329     O                                           33 'Poichè la giacenza è sta'
958700950418     O                                           57 'ta riaperta e quindi le '
958800950329     O                                           67 'spese sono'
958900950329     O          E            PIENAC         1
959000950329     O                                           24 'state recuperate con alt'
959100950329     O                                           48 're spedizioni, è probabi'
959200950329     O                                           67 'le che il  suddetto'
959300950329     O          E            PIENAC         3
959400950329     O                                           24 'cliente richieda Nota di'
959500950329     O                                           35 ' Accredito.'
959600950329     O          E            PIENAC         1
959700950418     O                                           33 'In questo caso allegare '
959800950418     O                                           57 'la presente al modulo di'
959900950418     O                                           67 ' richiesta'
960000950329     O          E            PIENAC         3
960100950329     O                                           24 'Nota Accredito che inolt'
960200950418     O                                           39 'rerete in sede.'
960300950329     O          E            PIENAC      1
960400950329     O                                           54 'Ufficio Giacenze'
960500950329     OFNLV22P   E            FINTO
960600000417** CMB
960700991206OVRPRTF FILE(FNLSB0P   ) OUTQ(          ) SAVE(*YES) FORMTYPE('          ')
960800991206** CMP
960900991206OVRPRTF FILE(FNLSB0P1  ) OUTQ(          ) SAVE(*YES) FORMTYPE('BOLLE-dpd ')
961000030304** CMPf
961100030304OVRPRTF FILE(FNLSB0P2  ) OUTQ(          ) SAVE(*YES) FORMTYPE('BOLLE-Fed ')
961200920206** CME
961300141210OVRPRTF FILE(FNLV22P   ) OUTQ(          )            FORMTYPE('          ')
961400920206** CMN
961500941013OVRPRTF FILE(LET132    ) OUTQ(*JOB      ) SAVE(*YES) FORMTYPE(          )
961600030715** CM4
961700141210OVRPRTF FILE(FNLSB5PA4 ) OUTQ(          )            FORMTYPE('          ')
961800030715** CM5
961900141210OVRPRTF FILE(FNLSB5PA5 ) OUTQ(          )            FORMTYPE('          ')
962000920206** DOV
962100920206DLTOVR  FILE(*ALL)
962200930427** ERR
962300970710Bolla Inesistente o già Consegnata                                             1
962400970710Bolla Legata: non eseguibile Cambio porto/Dirottam......                       2
962500970710Bolla inserita in Distinta di Consegna                                         3
962600970710Bolla GIACENTE: Agire da "GESTIONE GIACENZE"                                   4
962700970710Linea Arrivo Bolla Non in Gestione !!                                          5
962800970710Bolla Consegnata o Cons.Parzialm.: non possibile Cambio porto/Dirott.          6
962900970710Bolla Bloccata                                                                07
963000970710Tipo Consegna Errato !!                                                       08
963100970710Indicare Nuovo Porto !!                                                       09
963200970710Indicare Nuovo Porto SOLO SE DIVERSO !!                                       10
963300970710Se si fa RESO il C/Assegno deve essere ANNULLATO !!                           11
963400970710Variaz. C/A (S/N) non congruente con Importo o Divisa                         12
963500970710Indicare "S" per il calcolo Provvigioni C/Assegno                             13
963600970710A CHI addebitare provv. C/Assegno Errato !!                                   14
963700970710Variaz. Anticipata non congruente con Importo                                 15
963800970710Annullo C/Assegno : premere F8  per FORZARE ANNULLAMENTO !!                   16
963900970710Codice Fine Giacenza ERRATO !!                                                17
964000970710Si può richiamare Tassazione solo se linea arrivo é uguale !!                 18
964100970710Immettere dati anagrafici !!                                                  19
964200970710Provincia Incongruente con CAP  !! LIBERO LO MANDA PGM CTR CAP                20
964300970710Linea Arrivo errata !!                                                        21
964400970710Linea Arrivo incogruente con CAP : premere F9 x FORZARE !!!                   22
964500970710CAP ERRATO  !!                                                                23
964600970710Non Trovata Tariffa x Calcolo Provv.C/Assegno                                 24
964700970710Non Eseguibile variazione tipo bolla     LIBERO                               25
964800970710Non Eseguibile variazione Destinatario   LIBERO                               26
964900970710Non Eseguibile variazione Tassazione 1° bolla  LIBERO                         27
965000970710Non Eseguibile variazione Tassazione 2° bolla  LIBERO                         28
965100970710L. Arrivo diversa premere F8  x forzare dispos. in DIROTTAMENTO !!            29
965200970710Data errata, o < Apert. giacenza, o > Data del giorno                         30
965300970710Date Incongruenti                                                             31
965400970710Nessuna spesa da addebitare                                                   32
965500970710Immettere Importo Provv. C/Assegno                                            33
965600970710Non Trovata Tariffa giacenza !!                                               34
965700970710Non creata Bolla: premuto CMD12 da immiss.bolle! RIPETERE OPERAZ.             35
965800970710Non si possono eseguire le disposizioni prima della data indicata !           36
965900970710Divisa errata !                                                               37
966000970710La Divisa non prevede l'uso di decimali nell'importo C/assegno                38
966100970710Se si varia C/assegno immettere anche la Divisa !                             39
966200970710Nazione errata !                                                              40
966300970710Se si varia Divisa digitare anche l'importo del C/Assegno                     41
966400970710L.Arrivo uguale premere F8 per forzare disposizioni in "CONSEGNA"             42
966500970710Tipo incasso C/assegno errato o non congruente con valuta                     43
966600970710Non si puo' gestire Anticipata su una bolla in Franco                         44
966700970710Bolla C/S per comunicaz. giac. e possibile fare solo DIROTTAMENTO             45
966800970710Non è possibile eseguire cambio di porto su bolla di RIENTRO                  46
966900970710Cambio di porto non ammesso se immesse disposizioni di reso                   47
967000970710Impossibile aggiungere il C/Assegno se immesse disposizioni di reso           48
967100970710Impossibile eseguire reso in conto servizio su bolla di rientro               49
967200970710Inserire il motivo per cui si rende la merce !!!                              50
967300970710Impossibile eseguire dirottamento per bolle export                            51
967400970710Attenzione !! Modificata Località                                             52
967500970710Attenzione !! Modificato CAP                                                  53
967600970710Attenzione !! Modificata Provincia                                            54
967700970710Attenzione !! Modificata Nazione                                              55
967800971006L.Arrivo diversa impossibile effettuare DIROTTAMENTO: mittente non codificato 56
967900971113L.Arrivo non gestita dall'Azienda !!!                                         57
968000990203Impossibile eseguire cambio porto/dirottamento: bolla con C.A. Aperta         58
968100991028La Divisa non prevede l'uso di decimali nEll'importo                          59
968200991028Codice divisa inesistente                                                     60
968300991028Divisa non congruente con le altre della tassazione                           61
968400000120Bolla di recupero: impossibile effettuare cambi di porto/resi/dirottamenti    62
968500040206Impossibile eseguire cambio porto/dirottamento:PER BOLLE POSTE                63
968600040206La divisa deve essere inserita se immesso un importo nel C/A                  64
968700040531Cambio linea arrivo non ammesso la LOC.o il CAP sono serviti dalla attuale    65
968800071130ATTENZIONE verificare la data consegna richiesta della bolla !!!!!            66
968900110909Spedizione passata al PDA. Ricevere l'annullamento o chiudere la distinta !!! 67
969000151204ATTENZIONE presenti disposizioni Dirottamento/Reso spedizione non gestibile   68
969100141105ATTENZIONE esistono delle disposizioni di consegna che devono essere risolte  69
