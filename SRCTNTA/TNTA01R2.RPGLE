000100020312     H DECEDIT('0,') DATEDIT(*DMY.) option(*nodebugio)
000200940517     H* TNTA01R2 *---------------------------------------------------*
000300940502     H* 50 OFF      - MANUTENZIONE TARIFFE CLIENTI                   *
000400940502     H* 50 ON       - MANUTENZIONE OFFERTE CLIENTI                   *
000500000000     H*--------------------------------------------------------------*
000600940517     FTNTA01D2  CF   E             WORKSTN
000700940706     F                                     SFILE(TA01S03:NRR)
000800940510     F                                     INFDS(CA02DS)
000900940706     F                                     SFILE(TA01S05:NR1)
001000041222     F                                     SFILE(TA01S09:NR2)
001100061011     FTNTAM01L  UF A E           K DISK    usropn
001200061011     FTNOFM01L  uF a E           K DISK    USROPN
001300061011     F                                     RENAME(TNTAM000:TNOFM000)
001400061011     FTITAD04L  UF A E           K DISK    usropn
001500061011     FTIOFD01L  uF a E           K DISK    USROPN
001600061011     F                                     RENAME(TITAD000:TIOFD000)
001700080521     FTITPT01L  UF   E           K DISK    usropn
001800080521     FTIOPT01L  uF   E           K DISK    USROPN
001900061011     F                                     RENAME(TITPT000:TIOPT000)
002000080521     FTITPD01L  UF   E           K DISK    usropn
002100080521     FTIOPD01L  uF   E           K DISK    USROPN
002200061011     F                                     RENAME(TITPD000:TIOPD000)
002300061011     FTITGC01L  UF   E           K DISK    usropn
002400080521     FTIOGC01L  UF   E           K DISK    USROPN
002500990611     F                                     RENAME(TITGC000:TIOGC000)
002600000000     FTABEL00F  IF   E           K DISK
002700930223     FAZORG01L  IF   E           K DISK
002800100701     FTFNTC01L  UF   E           K DISK
002900050603     FTITAV00F  O  A E             DISK    usropn
003000091204     ftivof01l  UF a e           k disk    usropn
003100050603     FTITAV01L  UF   E           K DISK    RENAME(TITAV000:TITAV) usropn
003200101013     ftisis01l  if   e           k disk
003300130318     fTITAS41C  if   e           k disk    usropn extfile(wFLib)
003400030715
003500940510     D*
003600940510     D* DEFINIZIONE SCHIERE
003700041220     D CCT             S              2    DIM(600)
003800041220     D DCT             S             10    DIM(600)
003900041220     D ORP             S              6    DIM(600)
004000041220     d est             s              1    dim(600)
004100050303     d uta             s              1    dim(600)
004200101013      * DATE SCATTI ISTAT
004300101013     D DIA             S              8  0 DIM(999)
004400020215     D* FACCIO REG LUNGO UNO IN QUANTO E INUTILE
004500041220     D REG             S              1    DIM(600)
004600041220     D NAR             S              3    DIM(600)
004700041220     D UTF             S              1    DIM(600)
004800950522     D* SCHIERA CONTENENTE LE FILIALI GESTITE DALLA FILIALE COLLEGATA
004900941122     D*
005000000000     D CTR             S              1  0 DIM(50)
005100000000     D DTR             S             10    DIM(50)
005200940913     D MIN             S             13  3 DIM(50)
005300941108     D TAP             S              1    DIM(50)
005400950116     D SAP             S              1    DIM(50)
005500980505     D ARR             S              1    DIM(50)
005600980505     D RPV             S              1    DIM(50)
005700021118     D RUTF            S              1    DIM(50)
005800021118
005900950116     D NAZ             S              3    DIM(20)
006000960522     D K78             S              1    DIM(78)
006100041222     D LNPEL           S              3  0 DIM(120)
006200000000     D XDTF            S              2  0 DIM(12) CTDATA PERRCD(12)
006300940503     D*
006400940503     D TES             S             35    DIM(2) CTDATA PERRCD(1)
006500090608     D MSG             S             78    DIM(125) CTDATA PERRCD(1)
006600050530     d
006700050530     D TParEli         S              2    DIM(20)
006800050530     D TParDat         S              8  0 DIM(20)
006900050530     D ForzaTPar       S              2    DIM(20)
007000061012
007100061012     d wforza          s              1    dim(120)
007200020312
007300040927     d annultar        s              1    inz
007400030717     d instes          s              1    inz
007500030716     d mantes          s              1    inz
007600030714     d mandet          s              1    inz
007700030714     d mangia          s              1    inz
007800030714     d manftc          s              3    dim(150)
007900030714     d W_manftc        s              3
008000030714     d WW              s              3  0 inz
008100030715     d first           s              1    inz
008200030714
008300021118     d ForzaMsg        s              1    inz
008400131028     d ForzaMsgistat   s              1    inz
008500020312     d lnpntw          s                   like(§ogntw)
008600020312     d wsimula         s              1
008700050603     d soloTAV         s              1
008800020313     d wnoagg          s              1    inz('0')
008900020408     d woksfl          s              1    inz('0')
009000050530     d wCARTAB         s              1    inz(' ')
009100021029     d Sav_Orgfel      s                   like(OrgFel)
009200050628     d savlnp          s                   like(tadlnp)
009300050628     d savcts          s                   like(tadcts)
009400050628     d savsgl          s                   like(tadsgl)
009500050701     d savnaz          s                   like(tadnaz)
009600050701     d savcap          s                   like(tadcap)
009700131029     d savpri          s                   like(tampri)
009800050630     d werr            s              1    inz('0')
009900110128     d werrRpv         s              1n   inz(*off)
010000050701     d wlnp            s              3
010100050701     d wcts            s              2
010200050701     d wcap            s              9
010300110128     d wlnpRpv         s                   like(TADlnp)
010400110128     d wctsRpv         s                   like(TADcts)
010500110128     d wsglRpv         s                   like(tadsgl)
010600020313
010700020313     d w_taditr        s             12  3
010800020313     d w_tadino        s             12  3
010900020313     d w_tadmin        s             12  3
011000020313     d w_tadmax        s             12  3
011100020313     d w_tadrpv        s              4  1
011200020313
011300020313     d w_tpditr        s             12  3
011400020313     d w_tpdmin        s             12  3
011500020313     d w_tpdmax        s             12  3
011600020313     d w_tptrpv        s              4  1
011700020313     d w_itr           s                   like(tpditr)
011800020313     d w_min           s                   like(tpdmin)
011900020313     d w_max           s                   like(tpdmax)
012000050617     d w_tgcsgv        s             12  3
012100050617     d w_tgcsgr        s             12  3
012200050617     d w_tgcsgp        s             12  3
012300050617     d w_tgcsgd        s             12  3
012400050617     d w_tgcst1        s             12  3
012500050617     d w_tgcst2        s             12  3
012600050617     d w_tgcst3        s             12  3
012700050617     d w_tgcstm        s             12  3
012800050617     d w_tgcrpv        s              4  1
012900050617     d w_sgv           s                   like(tgcsgv)
013000050617     d w_sgr           s                   like(tgcsgr)
013100050617     d w_sgp           s                   like(tgcsgp)
013200050617     d w_sgd           s                   like(tgcsgd)
013300050617     d w_st1           s                   like(tgcst1)
013400050617     d w_st2           s                   like(tgcst2)
013500050617     d w_st3           s                   like(tgcst3)
013600050617     d w_stm           s                   like(tgcstm)
013700020312
013800101013     d w_v1crct        s                   like(tamrct)
013900041222
014000041222     d w_sbflnp        s              1
014100050530     d
014200050530     d wdatadmy        s               d   datfmt(*dmy)
014300061011
014400061011     d codut           s              1  0 Inz(1)
014500070309     d comftc          s              5    inz
014600070309     d wsftc           s              1    inz
014700070309     d wtptftc         s              1    inz
014800070309     d wokftc          s              1    inz
014900070312     d comando         s              1
015000070312     d kcod            s                   like(tblcod)
015100070312     d kkey            s                   like(tblkey)
015200070312     d ordina          s              1
015300070312     d wintftc         s              1    inz(*off)
015400120713     d wintcts         s              1n   inz(*off)
015500080610     d win01           s              1n   inz(*off)
015600101013     d $EoF            s              1n   inz(*off)
015700110128     d oldrpv          s                   like(tadrpv)
015800120529     d sav_TAMbap      s                   like(TAMbap)
015900130604     d sav_TAMtsp      s                   like(TAMtsp)
016000130124     d wMesejob        s              2  0
016100130124     d wMesetad        s              2  0
016200101013
016300101013     d wDate_EUR       s               d   datfmt(*eur)
016400101013     d wDate_ISO       s               d   datfmt(*iso)
016500120124     d wOggi           s               d   datfmt(*iso)
016600120314
016700120314       // -?Flag booleani
016800120314     d wFine           s               n   inz(*off)
016900120315     d wPrima          s               n   inz(*off)
017000120315     d wSeconda        s               n   inz(*off)
017100120315     d wTerza          s               n   inz(*off)
017200120315     d wQuarta         s               n   inz(*off)
017300130318     d wTParEli        s               n   inz(*off)
017400140224     d wNoOkFuel       s               n   inz(*off)
017500120315
017600120315       // -?Campi x gestione riga mobile tasti funzione
017700120315     d wTfv            s             61    dim(20) ctdata perrcd(1)
017800120315     d wf24            s              2  0
017900120315     d wpos            s              2  0
018000120315     d wposda          s              2  0
018100120315     d wpostf1         s              5  0
018200120315     d wpostf2         s              5  0
018300120315     d wpostf3         s              5  0
018400120315     d wrig            s              2  0
018500120315     d wtf1            s             61
018600120315     d wtf2            s             61
018700120315     d wtf3            s             61
018800120315     d tf              s              3  0
018900130318
019000130318       // -?Campi libreria e file per TITAS41C
019100130318     d wFLib           s             21
019200130318     d wLib            s             10
019300130318
019400130318     d wDataSP         s              8  0
019500130318     d sav_TAMddt      s                   like(TAMddt)
019600130318     d sav_TAMdst      s                   like(TAMdst)
019700031119
019800930629     D*
019900990601     D** CHIAVE X CHAIN COMPLETO SU FILE TITAD
020000000000     D KIAVE           DS
020100891005     D  TADLNP                 1      3  0
020200990608     D  TADORP                 4      8
020300990729     D  TADNAZ                 9     11
020400020624     D  tadCAP                12     20
020500990608     D  TADSGL                21     33  3
020600930629     D* CHIAVE INIZIALE SUBFILE AGGIORNAMENTO
020700000000     D KI              DS
020800891005     D  LNPI                   1      3  0
020900990608     D  ORPI                   4      8
021000990608     D  NAZI                   9     11
021100990608     D  PARI                  12     20
021200990608     D  SCGI                  21     33  3
021300930629     D*  CHIAVE FINALE SUBFILE AGGIORNAMENTO
021400000000     D KE              DS
021500891005     D  LNPE                   1      3  0
021600990608     D  ORPE                   4      8
021700990608     D  NAZE                   9     11
021800990608     D  PARE                  12     20
021900990608     D  SCGE                  21     33  3
022000930629     D** CHIAVE X CHAIN SCORRIMENTO ROLLUP E ROLLDOWN
022100000000     D KX              DS
022200891005     D  LNPX                   1      3  0
022300990608     D  ORPX                   4      8
022400990608     D  NAZX                   9     11
022500990608     D  PARX                  12     20
022600990608     D  SCGX                  21     33  3
022700930629     D*
022800940518     D** PARAMETRI PASSATI DAL TNTA01R
022900000000     D PARAMX          DS
023000950421     D* CODICE CLIENTE
023100940526     D  V1CKSC                 2      8  0
023200950421     D* PROGRESSIVO TARIFFA
023300000000     D  PRG                    9     11  0
023400950421     D* CAPOCONTO CLIENTI
023500000000     D  CCC                   12     15  0
023600950421     D* RAGIONE SOCIALE CLIENTE
023700940526     D  V1DKSC                16     63
023800950421     D* CODICE TARIFFA
023900900327     D  CTR2                  64     66
024000950421     D* CODICE CLIENTE NELLA GESTIONE VISITE/OFFERTE
024100900327     D  KSC2                  67     73
024200950421     D* VFLG =  '1' --> GESTIONE VISITE/OFFERTE
024300950421     D* VFLG >< '1' --> GESTIONE TARIFFE
024400950421     D* VFLG =  '5' --> PROVENGO DA AGGIUNTA TARIFFA: NON ABILITO CMD12
024500900327     D  VFLG                  74     74
024600950421     D* PARALL = 'A' ---> RECORD ALLOCATO: TARIFFA NON MANUTENZIONABILE
024700941007     D  PARALL                95     95
024800981111     D  CPOX                 156    161P 0
024900990913     D* FLAG DI RITORNO
025000990913     D* F12 = 'R' RITORNO ALLA VIDEATA
025100990913     D* F06/F03 = 'C' FINE PROGRAMMA
025200990913     D  FLGRTN               162    162
025300031118     D* PARIST = 'E' ---> SCATTO ISTAT INESISTENTE ERRORE
025400031118     D  PARIST               178    178
025500090916      * flag per richiamo da trattativa = 'T'
025600090916     d  partrat              179    179
025700010330     D*
025800940509     D*
025900941219     D* PASSAGIO AL FILTRO DELLA STAMPA TESTI       - FNLV43R -
026000940310     D PARAM2          DS
026100940310     D* NUMERO VISITA E PROGRESSIVO
026200940310     D  PA2NRV                 3      9  0
026300940310     D* CODICE CLIENTE
026400940310     D  PA2CLI                10     16  0
026500940311     D* CODICE CLIENTE POTENZIALE
026600940311     D  PA7CPO                17     27  0 INZ
026700941219     D* TIPO STAMPA TARIFFA -->PER FARE LE GIUSTE OVRDBF  AL TNTA43R
026800941219     D* 'R' E' RIFERIMENTO ALTRA TARIFFA
026900941219     D  PA2TST                98     98
027000940310     D* AMBIENTE STAMPA TESTI ("V"=VISITE/"C"=CLIENTI/"P"=POTENZIALI)
027100091021      * NEW 'T'=TRATTATIVE
027200941219     D  PA2AST                99     99
027300940729     D* COD TARIFFA E PROGRESSIVO PER STAMPA TARIFFA  -TNTA43R
027400940310     D  PA2CTR               100    102  0
027500940310     D  PA2PRG               103    105  0
027600940310     D* '2' -- DA OFFERTA / TARIFFA
027700940310     D* '4' -- DA ANNULLAMENTO
027800940310     D  PA2FLG               106    106
027900940310     D* RAGIONE SOCIALE
028000940310     D  PA2RGS               107    152
028100940310     D*
028200941221     D* RICHIAMO PGM GESTIONE TARIFFE GIACENZA         - TNTA14R -
028300900530     D PARAM3          DS
028400091210      * richiamato da tariffa/offerta
028500091210     d  pa3ast                 9      9
028600091210      * codice cliente/visita/trattativa
028700091210     d  pa3cli                10     16  0
028800941123     D* DECODIFICA CODICE TARIFFA
028900941221     D  PA3DCT                17     26
029000941123     D* DESCRIZIONE TARIFFA
029100941221     D  PA3DCV                27     41
029200961206     D* FLAG ITALIA/ESTERO
029300961206     D  PA3FIE                42     42
029400990824     D* CODICE DIVISA
029500990824     D  PA3DIV                43     45
029600990622     D* FLAG DECIMALI SI/NO IN BASE ALLA DIVISA
029700990622     D  PA3FDC                73     73
029800020204      * FLAG NETWORK DPD/FEDEX
029900020204     D  PA3NET                74     74
030000090916      * flag per richiamo da trattativa = 'T'
030100090916     d  pa3trat               75     75
030200091210      * codice tariffa e progressivo
030300091210     d  pa3ctr               125    127  0
030400091210     d  pa3prg               128    130  0
030500030714     D* PA3MAN --> SE RESTITUITO PIENO OCCORRE scrivere record
030600030714     D*            storico di variazione tariffa giacenza
030700030714     D  PA3MAN               256    256
030800940509     D*
030900940729     D** PARAMETRI PER GESTIONE TARIFFE PARTICOLARI - TNTA67R -
031000920708     D PARAM5          DS
031100950116     D* CODICE CLIENTE/NUMERO VISITA
031200920708     D  CLI5                   2      8  0
031300950116     D* RAGIONE SOCIALE
031400920708     D  RAG5                   9     56
031500950116     D* PROGRESSIVO TARIFFA
031600940526     D  V1CPRG                57     59  0
031700950116     D* CODICE TARIFFA
031800940526     D  V1CCTR                60     62  0
031900950116     D* DECODIFICA CODICE TARIFFA
032000940526     D  V1DCTR                63     72
032100950116     D* DESCRIZIONE TARIFFA
032200940526     D  V1CDCV                73     87
032300950116     D* TESTATA PROGRAMMA
032400940526     D  V1CTES                88    121
032500950116     D* FLG = " " ---> TARIFFE     FLG <> " " ---> OFFERTE
032600920708     D  FLG                  122    122
032700950116     D* FLG4   --> SE RESTITUITO PIENO INDICA CHE E' STATO PREMUTO CMD3
032800921118     D  FLG4                 123    123
032900950116     D* FLAG ITALIA/ESTERO
033000941111     D  V1CFIE               125    125
033100980309     D* FLAG IMPORT  CON 'Y' SI INDICA UNA TARIFFA IMPORT DA CONTROLLARE NELLA
033200980309     D* GESTIONE DELLE TARIFFE PARTICOLARI
033300980309     D  IMPORT               126    126
033400000420     D* FLAG CLIENTE POSTE
033500000420     D  WRKPT                127    127
033600990824     D* CODICE DIVISA
033700990824     D  V1CDIV               128    130
033800990622     D* FLAG DECIMALI SI/NO IN BASE ALLA DIVISA
033900990824     D  PA5FDC               131    131
034000000915     D* DATA DECORRENZA TARIFFA
034100000915     D  PA5DDT               132    139  0
034200000915     D* TIPO SERVIZIO BOLLE
034300000915     D  PA5TSP               140    140
034400020201     D* Flag Network
034500020201     D  FLGNET               141    141
034600050531     D* DATA DECORRENZA TARIFFA
034700050531     D  PA5DsT               142    149
034800060306      * flag rinuncia AC Base
034900060306     d  pa5fmp               150    150
035000090916      * flag per richiamo da trattativa = 'T'
035100090916     d  pa5trat              151    151
035200920528     D*
035300941010     D* DS PER TRTB69R - TRTB70R - TRTB72R - TRTB73R - INT/GEST PARTIC.
035400941010     D DS7PQR        E DS                  EXTNAME(DS7PQRS)
035500941012     D*
035600970718     D* DS PER TISI95R - CONTROLLO CAP
035700970718     D DSSI95        E DS                  EXTNAME(TISI95DS)
035800970718     D* DS PER FNLV13R - CONTROLLO CAP
035900970718     D DSLV13        E DS                  EXTNAME(FNLV13DS)
036000941010     D*
036100961212     D* DS PER TNTE01R - TARIFFE DA C/ECONOMICO
036200961212     D DSTE00        E DS                  EXTNAME(TNTE00DS)
036300970117     D*
036400970122     D* DS PER TNTA47R - ANNULLAMENTO DATI TIPO
036500970122     D DSTA47        E DS                  EXTNAME(TNTA47DS)
036600961212     D*
036700940506     D                 DS
036800940729     D  AA                     1      4  0
036900940729     D  MM                     5      6  0
037000940729     D  GG                     7      8  0
037100940729     D  DATA                   1      8  0
037200900620     D WLBDAT          DS
037300940701     D  G02DAT                 1      8  0
037400940701     D  G02INV                 9     16  0
037500940701     D  G02ERR                17     17
037600940701     D  G02TGI                18     22  0
037700900924     D CA02DS          DS
037800900924     D  PRIMA                378    379B 0
037900940526     D                 DS
038000940526     D  VIDMC1                 1      2
038100940526     D  VIDMC2                 3      4
038200940526     D  VIDMC3                 5      6
038300940526     D  VIDMC4                 7      8
038400940526     D  VIDMC5                 9     10
038500940526     D  VIDMC6                11     12
038600940526     D  VIDMC7                13     14
038700940526     D  VIDMC8                15     16
038800940526     D  VIDMC9                17     18
038900940526     D  VIDMC0                19     20
039000900410     D  VMC                    1     20
039100940531     D                                     DIM(10)
039200960522     D                 DS
039300960522     D  VIDMP1                 1      3  0
039400960522     D  VIDMP2                 4      6  0
039500960522     D  VIDMP3                 7      9  0
039600960522     D  VIDMP4                10     12  0
039700960522     D  VIDMP5                13     15  0
039800960522     D  VMP                    1     15  0
039900960522     D                                     DIM(5)
040000070309
040100070309     d                 ds
040200070309     d  vidftc1                1      1
040300070309     d  vidftc2                2      2
040400070309     d  vidftc3                3      3
040500070309     d  vidftc4                4      4
040600070309     d  vidftc5                5      5
040700070309     d  vftc                   1      5    dim(5)
040800070309
040900020206     D DSCT          E DS
041000910107     D DSTR          E DS
041100941011     D DS15          E DS
041200101013      *
041300101013     d TRTB12ds1     e ds                  inz
041400031117      *
041500940615     D TAMDS         E DS                  EXTNAME(TNTAM00F)
041600030704     D COMODO        E DS                  EXTNAME(TNTAM00F)prefix(§)
041700030716     D COMODO$       E DS                  EXTNAME(TNTAM00F)prefix($)
041800000000     D KPJBA         E DS
041900900418     D DS1G          E DS
042000990616     D DSCV          E DS
042100000918     D DS1P          E DS
042200000218     D OG143         E DS
042300000420     D DS5E          E DS
042400000223     D DS3IDP        E DS
042500030718      * ds per visualizzazione variazioni tariffe
042600030718     D TNTA04DS      E DS
042700030718      *
042800980504     D* AGGIUNTA DS EPR DEFINIRE I SINGOLI FLAG DEL CAMPO TAMFLO
042900980504     D DSTA01        E DS
043000021017
043100021017     d Tibs02Ds      e ds
043200021017      * Ds date statistiche
043300021017     d dSdf          e ds
043400021118      * Ds dati default tariffe FedEx
043500021118     d dDfe          e ds
043600031124      * Ds data massima elaborazione CAT da passare al TNTE01R
043700031124     d dcat          e ds
043800061011
043900061011     d Azuteds       e ds                  extname(Azute00f)
044000061011     d dDatiute      e ds
044100061011     d Tibs34ds      e ds
044200061011
044300061011     d Tnta25ds      e ds
044400061011     d Tnta25tad     e ds                  inz
044500061011     d Tnta25tgc     e ds                  inz
044600061011     d Tnta25tpd     e ds                  inz
044700061011     d Tnta25tpt     e ds                  inz
044800090709
044900090709     d tnta12ds      e ds
045000100407     d TNTA28DS      e ds
045100100407     d TRMK20DS      e ds
045200120314
045300120314       // -?ds dati passati da TNSB48R - Manca Tariffa
045400120314     d TNSB48ds      e ds
045500120315     d Parm48          s                   like(tnsb48ds)
045600120713
045700120713       // -?passaggio dati a TRTB09R - Ricerca codice tassazione
045800120713     d TRTB09DS      e ds
045900130124
046000130124       // -?abilitazioni profilo utente
046100130124     d dUTE01        e ds
046200160209
046300160209       // -?ds dati passati al TRSM90R - Cancella file Tariffe Inserite
046400160209     d TRSM90DS      e ds
046500160218
046600160218       // -?ds dati passati al TNTAT1R - Controllo tipologia tariffa
046700160218     d TNTAT1DS      e ds                  inz
046800021017
046900980226     D*
047000010323     D* CAMPO PER DSPATR "NO DISPLAY"+"PROTECT" DEI CAMPI A VIDEO
047100010323     D NDPR            C                   CONST(X'A7')
047200020207     D*
047300020207     D* CAMPO PER DSPATR "REVERSE IMAGE"        DEI CAMPI A VIDEO
047400020207     D RIMM            C                   CONST(X'21')
047500130124
047600130124      // campo per DSPATR "PROTECT"
047700130124     d nPRO            c                   CONST(X'A0')
047800030715
047900030715     D* Reperimento dati del PGM
048000030715     D PGMSTATUS      SDS
048100060116     d  SDSpgm           *proc
048200030715     D JOB_NAME              244    253
048300030715     D USER                  254    263
048400101013
048500101013      //---------------------------------------------------------------
048600101013      //?Definizione procedure esterne.
048700101013      //---------------------------------------------------------------
048800101013      // - Reperimento Scatti ISTAT
048900101013     d trtb12r1        pr                  extpgm('TRTB12R1')
049000101013     d  kpjba                              likeds(KPJBA)
049100120713
049200120713      // - Interrogazione codici tassazione
049300120713     d trtb09r         pr                  extpgm('TRTB09R')
049400120713     d  kpjba                              likeds(KPJBA)
049500160209
049600160209      // - Pgm Cancella file Tariffe Inserite
049700160209     d trsm90r         pr                  extpgm('TRSM90R')
049800160209     d  kpjba                              likeds(KPJBA)
049900160209     d  trsm90ds                           likeds(TRSM90DS)
050000090916
050100100407      //---------------------------------------------------------------
050200100407      //?prototipi
050300100407      //---------------------------------------------------------------
050400100407      /copy gaitrasrc/srcprotopr,tnta28r
050500160218      /copy gaitrasrc/srcProtoPR,TNTAT1R
050600100407      /copy gaitrasrc/srcprotopr,trmk20r
050700120315
050800120315      //---------------------------------------------------------------
050900120315      //?Costanti
051000120315      //---------------------------------------------------------------
051100120315     d cf2413          c                   const('F24=AlTasti(1/3)')
051200120315     d cf2423          c                   const('F24=AlTasti(2/3)')
051300120315     d cf2433          c                   const('F24=AlTasti(3/3)')
051400120315     d cf2412          c                   const('F24=AlTasti(1/2)')
051500120315     d cf2422          c                   const('F24=AlTasti(2/2)')
051600030715
051700031120     C*****************************************************************
051800031120     C* RIEPILOGO INDICATORI
051900031120     C*****************************************************************
052000031120     C* 01    - IMMISSIONE
052100031120     C* 02/09 - TASTI DI DUPLICA
052200031120     C* 10    - AGGIORNAMENTO
052300940530     C* 11    - TASTO DI DUPLICA
052400941109     C* 12    - PROVENGO DA AGGIUNTA TARIFFA QUINDI NON ABILITO IL CM12
052500941124     C* 13    - USATO NEL DETTAGLIO PER VISUALIZZARE LA LETTERA "M"
052600941124     C*         CHE INDICA LA PRESENZA DEL MINIMO O DEL MASSIMO
052700941109     C* 14    - CONDIZIONA LO SPEGNIMENTO DELL'INDICATORE DI SFLNXTCHG
052800940530     C* 15    - USATO IN INTERROGAZIONE PER PROTEGGERE I CAMPI
052900130318      *         (nel pgm TNTA02R2)
053000941109     C* 16    - INDICA IL TIPO TARIFFA "4 - A VALORE"
053100130124     C* 17    - Protezioni campi in modifica tariffe
053200950107     C* 18    - SONO IN FILIALE QUINDI VISUALIZZO NOTE
053300990601     C* 19    - ERRORE SU WRITE DI TITAD
053400941109     C* 20    - E' STATA DIGITATA UNA CHIAVE DI RICERCA
053500941109     C* 21    - ESISTONO RIGHE DI DETTAGLIO
053600940530     C* 22    - SFLNXTCHG
053700941109     C* 23    - SI TRATTA DI TARIFFA DI CARTELLO
053800020314     c* 24    - ERRORE
053900940530     C* 25    - ROLLUP
054000041222     C* 26    - USATO NEL SUBFILE PIÙ LINEE PARTENZA
054100950107     C* 27    - USATO PER EMETTERE UNA VIDEATA DOPO UN "?" SU UN CAMPO;
054200940530     C* 28    - INDICATORE DI EMISSIONE MESSAGGIO DI ERRORE
054300940530     C* 29    - READC SU SUBFILE
054400990601     C* 30    - INDICATORE DI MODIFICA DI TITAD
054500130318      * 31    - F16 abilitato
054600130318     C* 32/34 - DI COMODO
054700940530     C* 35    - USATO PER LOKUP E TESTN
054800941007     C* 36    - UTILIZZATO PER GESTIRE L'ALLOCAZIONE DEL RECORD: SE
054900941007     C*         IL RECORD E' ALLOCATO (CIOE' *IN36 ACCESO), EMETTO UN
055000130924     C*       - UTILIZZATO ANCHE PER GESTIRE MSG DI ERRORE MANCA SCAGLIONE
055100130924     C*         ISTAT AVVISARE IL CED DAL PGM CHIAMANTE
055200960523     C* 37    - DI COMODO
055300990617     C* 38    - NAZIONE ERRATA
055400021118      * 39    - Protezione natura merce
055500990617     C* 44    - DUPLICA
055600000118     C* 40/49 - ERRORI
055700940530     C* 50    - OFF ---> TARIFFE CLIENTI    ON ---> OFFERTE CLIENTI
055800990616     C* 51    - ERRORE MINIMO /MASSIMO SPEDIZIONI
055900990617     C* 52    - ERRORE DECIMALI IMPORTO MASSIMO
056000151016     C* 53    - Visualizzo applicazione calcolo peso desunto
056100940706     C* 51/59 - ERRORI
056200940706     C* 61/80 - ERRORI
056300941109     C* 81    - INDICATORE DI MODIFICA DI TNTAM(AGGIORNO DATA ULT.VAR.)
056400940811     C* 82    - ERRORE
056500941109     C* 83    - SFLDLT ---> SFL DI IMMISSIONE
056600941109     C* 84    - SFLCLR ---> SFL DI IMMISSIONE
056700090916      * 85    - Trattativa
056800090916     C* 86/87 - ERRORI
056900941124     C* 88    - IMPORTO TARIFFA IN PERCENTUALE
057000960522     C* 89/90 - ERRORI
057100941109     C* 91    - ELIMINAZIONE SUBFILE DI AGGIORNAMENTO SU DISCO
057200050530     C* 92    - usato
057300941109     C* 93    - SFLDLT ---> SFL DI AGGIORNAMENTO
057400941109     C* 94    - SFLCLR ---> SFL DI AGGIORNAMENTO
057500950116     C* 95    - ESISTONO TARIFFE PARTICOLARI: EVIDENZIO IL COMANDO
057600950116     C* 96    - ESISTONO TARIFFE GIACENZA   : EVIDENZIO IL COMANDO
057700120314      * 97    - Pgm richiamato x manca tariffa TNSB48
057800041222     C* 98    - USATO NEL SUBFILE PIÙ LINEE PARTENZA
057900941109     C* 99    - ELIMINAZIONE SUBFILE DI IMMISSIONE    SU DISCO
058000900329     C*****************************************************************
058100000000     C     *ENTRY        PLIST
058200000000     C                   PARM                    KPJBA
058300120315     c                   parm                    Parm48
058400120314
058500120314      /free
058600120314       //?controllo se passati tutti i parametri
058700120314         IF  %parms = 1;
058800120314           clear tnsb48ds;
058900120314           *in97 = *off;
059000120314         ELSE;
059100120315           clear tnsb48ds;
059200120315           tnsb48ds = parm48;
059300120314           *in97 = *on;
059400120314         ENDIF;
059500120314
059600120314         clear OSB48err;
059700120314         clear OSB48msg;
059800120314      /end-free
059900061011
060000061011     c     *dtaara       define    §azute        azuteds
060100061011     c     *dtaara       define    §datiute      ddatiute
060200061011     c                   in(E)     *dtaara
060300061011     c                   If        %error  or RSUT = *blanks
060400061011     c                   Clear                   Tibs34ds
060500061011     c                   Call      'TIBS34R'
060600061011     c                   Parm                    Tibs34ds
060700061011     c                   In        *dtaara
060800061011     c                   EndIf
060900061011
061000061011     C                   MOVEL     Rsut          V1CRSU
061100000000     C*---------------------------------------------------------------*
061200940525     C                   SETOFF                                       212726
061300921218     C                   SETOFF                                       9199
061400980226     C*---------------------------------------------------------------*
061500990601     C* ACCESSO TITAD04L
061600000000     C     KTAD          KLIST
061700940526     C                   KFLD                    V1CKSC
061800900320     C                   KFLD                    TAMCTR
061900000000     C                   KFLD                    PRG
062000000000     C                   KFLD                    TADLNP
062100000000     C                   KFLD                    TADORP
062200020624     C                   KFLD                    tadnaz
062300020624     C                   KFLD                    tadcap
062400940511     C                   KFLD                    TADSGL
062500960523     C     KTAD2         KLIST
062600960523     C                   KFLD                    V1CKSC
062700960523     C                   KFLD                    VCTR
062800960523     C                   KFLD                    PRG
062900960524     C                   KFLD                    COMLN2
063000960523     C     KTAD3         KLIST
063100960523     C                   KFLD                    V1CKSC
063200960523     C                   KFLD                    VCTR
063300960523     C                   KFLD                    PRG
063400960524     C                   KFLD                    COMLN2
063500960524     C                   KFLD                    COMOR2
063600940615     C* ACCESSO TNTAM01L
063700920526     C     KTAM          KLIST
063800940526     C                   KFLD                    V1CKSC
063900920526     C                   KFLD                    VCTR
064000990601     C* ACCESSO TNTAM01L/TITAD04L/TITPT01L/TITPD01L
064100900320     C     KTAM2         KLIST
064200940526     C                   KFLD                    V1CKSC
064300900320     C                   KFLD                    VCTR
064400900320     C                   KFLD                    PRG
064500061010     C* ACCESSO TNTAM01L
064600900419     C     KTAM5         KLIST
064700940503     C                   KFLD                    COMKSC
064800940503     C                   KFLD                    COMCTR
064900940503     C                   KFLD                    COMPRG
065000990601     C* ACCESSO TITPT01L/TIOPT01L  PER RICERCA TAR. PART. ASSIC. X CONTO
065100980309     C     KTAM7         KLIST
065200980309     C                   KFLD                    V1CKSC
065300980309     C                   KFLD                    VCTR
065400980309     C                   KFLD                    PRG
065500990609     C                   KFLD                    WTPAR             2
065600990601     C* ACCESSO TITAD04L
065700000000     C     KSCOR         KLIST
065800940526     C                   KFLD                    V1CKSC
065900900320     C                   KFLD                    TAMCTR
066000000000     C                   KFLD                    PRG
066100000000     C                   KFLD                    LNPX
066200000000     C                   KFLD                    ORPX
066300990608     C                   KFLD                    NAZX
066400930630     C                   KFLD                    COMX
066500000000     C                   KFLD                    SCGX
066600000000     C     KTTAD         KLIST
066700940526     C                   KFLD                    V1CKSC
066800900320     C                   KFLD                    TAMCTR
066900000000     C                   KFLD                    PRG
067000940526     C                   KFLD                    V1CRDA
067100990608     C                   KFLD                    VIDORP            5
067200020312      * accesso titpd01l
067300020312     c     ktpd          klist
067400020312     c                   kfld                    tptksc
067500020312     c                   kfld                    tptctr
067600020312     c                   kfld                    tptprg
067700020312     c                   kfld                    tptftc
067800050530     C     KTPT          KLIST
067900050530     C                   KFLD                    tamksc
068000050530     C                   KFLD                    TAMCTR
068100050530     C                   KFLD                    TAMPRG
068200050530     C                   KFLD                    tpareli(XX)
068300940510     C* ACCESSO TABEL00F
068400000000     C     KTAB          KLIST
068500000000     C                   KFLD                    CODUT
068600940503     C                   KFLD                    COD
068700940503     C                   KFLD                    KEY
068800940510     C     KTAB2         KLIST
068900920525     C                   KFLD                    CODUT
069000920525     C                   KFLD                    COD
069100940510     C* ACCESSO TFNTC01L
069200920526     C     KNTC          KLIST
069300940503     C                   KFLD                    APPL
069400940503     C                   KFLD                    NOTKEY
069500940503     C                   KFLD                    NOTKE2
069600940503     C*
069700940503     C* DEFINIZIONE DI ALCUNI CAMPI
069800970317     C     *LIKE         DEFINE    TAMDDT        DATEU8
069900940706     C     *LIKE         DEFINE    TAMDDT        UDATE8
070000940706     C*
070100940503     C     *LIKE         DEFINE    TBLCOD        COD
070200940503     C     *LIKE         DEFINE    TBLKEY        KEY
070300940503     C*
070400940503     C     *LIKE         DEFINE    TAMKSC        COMKSC
070500940503     C     *LIKE         DEFINE    TAMCTR        COMCTR
070600940503     C     *LIKE         DEFINE    TAMPRG        COMPRG
070700940503     C*
070800940503     C     *LIKE         DEFINE    NTCAPL        APPL
070900940503     C     *LIKE         DEFINE    NTCNK1        NOTKEY
071000940503     C     *LIKE         DEFINE    NTCNK2        NOTKE2
071100940518     C* CAMPI DI DUPLICA
071200940518     C     *LIKE         DEFINE    TADLNP        DUPLNP
071300940518     C     *LIKE         DEFINE    TADCTS        DUPCTS
071400940518     C     *LIKE         DEFINE    TADCAP        DUPCAP
071500990608     C     *LIKE         DEFINE    TADCAP        DUPNAZ
071600940518     C     *LIKE         DEFINE    TADSGL        DUPSGL
071700940518     C     *LIKE         DEFINE    TADITR        DUPITR
071800940518     C     *LIKE         DEFINE    TADINO        DUPINO
071900940518     C     *LIKE         DEFINE    TADRPV        DUPRPV
072000940518     C     *LIKE         DEFINE    TADMIN        DUPMIN
072100940518     C     *LIKE         DEFINE    TADMAX        DUPMAX
072200940518     C*
072300940518     C     *LIKE         DEFINE    TADITR        RISITR
072400940518     C     *LIKE         DEFINE    TADITR        COMITR
072500940518     C     *LIKE         DEFINE    TADITR        ITRCOM
072600940518     C     *LIKE         DEFINE    TADINO        RISINO
072700940518     C     *LIKE         DEFINE    TADINO        COMINO
072800940518     C     *LIKE         DEFINE    TADINO        INOCOM
072900940518     C     *LIKE         DEFINE    TADRPV        RISRPV
073000940518     C     *LIKE         DEFINE    TADRPV        RPVCOM
073100940518     C     *LIKE         DEFINE    TADMIN        RISMIN
073200940518     C     *LIKE         DEFINE    TADMIN        MINCOM
073300941124     C     *LIKE         DEFINE    TADMIN        WMIN
073400940518     C     *LIKE         DEFINE    TADMAX        RISMAX
073500940518     C     *LIKE         DEFINE    TADMAX        MAXCOM
073600941124     C     *LIKE         DEFINE    TADMAX        WMAX
073700940518     C     *LIKE         DEFINE    TADATB        ATBCOM
073800941108     C     *LIKE         DEFINE    TADCTS        WCTS
073900941108     C     *LIKE         DEFINE    TAMFIE        WFIE
074000020215     C     *LIKE         DEFINE    TAMTSP        WTSP
074100020215     C     *LIKE         DEFINE    V1CNET        WNET
074200960523     C     *LIKE         DEFINE    TADORP        WORP
074300960523     C     *LIKE         DEFINE    TADORP        WORPN
074400960524     C     *LIKE         DEFINE    TADORP        COMORP
074500960524     C     *LIKE         DEFINE    TADORP        COMOR2
074600960524     C     *LIKE         DEFINE    TADLNP        COMLNP
074700960524     C     *LIKE         DEFINE    TADLNP        COMLN2
074800960527     C*
074900960527     C     *LIKE         DEFINE    V8CLNN        SAVLNN
075000960527     C     *LIKE         DEFINE    V8CCTN        SAVCTN
075100940518     C*
075200931119     C*---------------------------------------------------------------*
075300000000     C                   MOVEL     *ZEROS        KI
075400000000     C                   MOVEL     *ZEROS        KE
075500000000     C                   MOVEL     *ZEROS        KX
075600000000     C                   MOVEL     *ZEROS        KIAVE
075700000000     C                   MOVEL     *BLANKS       TADORP
075800000000     C                   MOVEL     *BLANKS       ORPI
075900000000     C                   MOVEL     *BLANKS       PARI
076000990608     C                   MOVEL     *BLANKS       NAZI
076100000000     C                   MOVEL     *BLANKS       PARE
076200000000     C                   MOVEL     *BLANKS       ORPE
076300990608     C                   MOVEL     *BLANKS       NAZE
076400000000     C                   MOVEL     *BLANKS       ORPX
076500000000     C                   MOVEL     *BLANKS       PARX
076600990608     C                   MOVEL     *BLANKS       NAZX
076700050530     c
076800050530     c                   clear                   forzatpar
076900050530     C* INDICe DEDICATO PER LA SKIERA FORZATPAR: non usare per altro
077000050530     c                   clear                   yy
077100930628     C*
077200940728     C* GIRO DATA DEL GIORNO: LA PRENDO DA TIME
077300940728     C                   TIME                    W0120            12 0
077400940728     C                   MOVE      W0120         WDAT              6 0
077500940728     C*
077600940729     C                   Z-ADD     WDAT          G02DAT
077700940706     C                   MOVEL     *BLANK        G02ERR
077800940706     C                   CALL      'XSRDA8'
077900940706     C                   PARM                    WLBDAT
078000940706     C* UDATE A 8 IN AAAA/MM/GG
078100941221     C                   Z-ADD     G02INV        DATEU8
078200940706     C* UDATE A 8 IN GG/MM/AAAA
078300940706     C                   Z-ADD     G02DAT        UDATE8
078400120124
078500120124      /free
078600120124       //?Imposto oggi in ISO
078700120124        wOggi = %date(dateu8:*iso);
078800130124       //?Imposto mese in corso
078900130124        wMeseJob = %dec(%subst(%editc(dateu8:'X'):5:2):2:0);
079000120124      /end-free
079100120124
079200060116      * Nome pgm.
079300060116     c                   movel     SDSpgm        V1Cpgm
079400000223     C*
079500000223     C* AGGANCIO DS3IDP PER AVEREIL MASSIMO PESO
079600000223     C                   MOVEL     '3I'          COD
079700000223     C                   MOVEL(P)  'DPD'         KEY
079800000223     C*
079900000223     C     KTAB          CHAIN     TABEL                              30
080000000223     C  N30              MOVEL     TBLUNI        DS3IDP
080100000223     C   30              CLEAR                   DS3IDP
080200000223     C*
080300940706     C*
080400940526     C                   Z-ADD     0             V1CKSC
080500000000     C                   Z-ADD     0             PRG
080600000000     C                   Z-ADD     0             CCC
080700120906     C                   Z-ADD     0             V1CRDA
080800120906     C                   MOVEL     *BLANKS       V1CRA
080900940526     C                   MOVEL     *BLANKS       V1DKSC
081000000000     C                   MOVEL     KPJBU         PARAMX
081100990913     C* IMPOSTO IL CODICE DI RITORNO AL PROGRAMMA CHIAMANTE
081200990913     C*
081300990913     C                   MOVEL     'R'           FLGRTN
081400980309     C                   MOVE      ' '           UNO               1
081500090916
081600090916      * controllo se richiamato da trattativa
081700090916     c                   if        partrat = 'T'
081800090916     c                   eval      *in85 = *on
081900090916     c                   else
082000090916     c                   eval      *in85 = *off
082100090916     c                   endif
082200920707     C*
082300050530     C** CARICAMENTO TABELLE
082400050530     C                   IF        WCARTAB=' '
082500920707     C                   EXSR      CARTAB
082600050530     C                   EVAL      WCARTAB='S'
082700050530     C                   ENDIF
082800931119     C*
082900061011     C** SOLO SE SONO IN FILIALE APRO FILE NOTE
083000941012    1C  N18SIMFEL        IFNE      0
083100940530     C                   SETON                                        18
083200941012    1C                   ENDIF
083300941007     C*
083400941007     C* 23 ON  - SI TRATTA DI TARIFFA DI CARTELLO
083500941116     C                   SETOFF                                       23
083600941007     C                   MOVEL     V1CKSC        W0050             5 0
083700941012    1C     W0050         IFEQ      88888
083800941007     C                   SETON                                        23
083900941012    1C                   ENDIF
084000130124
084100991213     C* SE NON È TARIFFA DI CARTELLO RECUPERO LA FILIALE DI APPERTENENZA
084200991213     C* DEL CLIENTE PER RECUPERARE UN FLAG A PROGRAMMA PER LA GESTIONE DI
084300991213     C* TARIFFE ITALIA CON CODICI TASSAZIONE ESTERI
084400000420     C                   MOVEL     *BLANKS       WRKPT             1
084500000426     C* SE NON SI TRATTA DI OFFERTA
084600991213     C     *IN23         IFEQ      *OFF
084700030930     C     VFLG          ANDNE     '1'
084800991213     C                   MOVEL     V1CKSC        W0030
084900000118     C     W0030         CHAIN     AZORG01L                           37
085000000118     C     *IN37         IFEQ      *OFF
085100000420     C* CONTROLLO SE E' CLIENTE DPD
085200000218     C                   MOVEL     ORGDE3        OG143
085300000420     C* CONTROLLO SE E' CLIENTE POSTE
085400020312     c                   if        §ogntw = 'PPT'
085500020312     c                   eval      wrkpt = 'S'
085600020312     c                   endif
085700000420     C*
085800991213     C                   ENDIF
085900991213     C                   ENDIF
086000941007     C*
086100900327     C* TESTATA VIDEO
086200941012    1C     VFLG          IFNE      '1'
086300940503     C* VFLG <> '1'  ---> GESTIONE TARIFFE CLIENTI
086400940526     C                   MOVE      TES(1)        V1CTES
086500061011     c                   setoff                                       50
086600061011      * apro i file delle tariffe
086700061011     c                   open      tntam01l
086800061011     c                   open      titad04l
086900061011     c                   open      titpt01l
087000061011     c                   open      titpd01l
087100061011     c                   open      titgc01l
087200050603     c* Solo per tariffe apro titav
087300050603     c                   if        solotav=' '
087400050603     c                   open      titav00f
087500050603     c                   open      titav01l
087600050603     c                   eval      solotav='S'
087700050603     c                   endif
087800940503     C*
087900941012   X1C                   ELSE
088000940503     C* VFLG  = '1'  ---> GESTIONE VISITE/OFFERTE
088100940526     C                   MOVE      TES(2)        V1CTES
088200900329     C                   SETON                                        50
088300061011      * apro i file delle offerte
088400061011     c                   open      tnofm01l
088500061011     C                   OPEN      TIOFD01L
088600061011     c                   open      tiopt01l
088700061011     c                   open      tiopd01l
088800061011     c                   open      tiogc01l
088900091204     c   85              open      tivof01l
089000940526     C                   Z-ADD     V1CKSC        V1CNRV
089100090916     C                   Z-ADD     V1CKSC        V1CNRVt
089200021029      * Cerco ORGFEL di simfel
089300021029     c     Simfel        Chain     Azorg01l
089400021029     c                   If        %Found(Azorg01l)
089500021029     c                   Eval      Sav_Orgfel = Orgfel
089600021029     c                   Else
089700021029     c                   Clear                   Sav_Orgfel
089800021029     c                   EndIf
089900981111     C**
090000021017     C* LEGGO  LA DATA statistica
090100021017     c                   Clear                   Tibs02Ds
090200021017     c                   Clear                   dSdf
090300021017     c                   Eval      T02Mod = 'C'
090400021017     c                   Eval      T02Sif = Knsif
090500021017     c                   Eval      T02Cod = 'SDF'
090600021029     c                   Movel(p)  Sav_OrgFel    T02Ke1
090700021017     c                   Call      'TIBS02R'
090800021017     c                   Parm                    Kpjba
090900021017     c                   Parm                    Tibs02Ds
091000021017     c                   If        T02Err = *Blanks
091100021017     c                   Movel     T02Uni        dSdf
091200021017     c                   EndIf
091300941012    1C                   ENDIF
091400931119     C*
091500941012     C* VFLG  = '5'  ---> PROVENGO DA AGGIUNTA TARIFFA QUINDI NON
091600941012     C*                   ABILITO IL CMD12
091700940524     C                   SETOFF                                       12
091800941012    1C     VFLG          IFEQ      '5'
091900940524     C                   SETON                                        12
092000941012    1C                   ENDIF
092100940509     C*
092200061011     C                   SETOFF                                       011016
092300941012     C* CONTROLLO SE TROVO TARIFFA DI QUEL CLIENTE CON QUEL COD.TARIFFA
092400941012    1C     CTR2          IFNE      *BLANKS
092500900327     C                   MOVE      CTR2          VCTR              3 0
092600940811     C*
092700941109     C* 16 ON  - INDICA IL TIPO TARIFFA "4 - A VALORE"
092800940811     C                   MOVEL     CTR2          W001              1
092900941012    2C     W001          IFEQ      '4'
093000940811     C                   SETON                                        16
093100941012    2C                   ENDIF
093200941012    1C                   ENDIF
093300931119     C*
093400900329     C* POSIZIONAMENTO INIZIALE
093500900329     C                   EXSR      LEGGI
093600941012     C* 36 ON  - RECORD ALLOCATO: RITORNO AL PGM TNTA01R UN FLAG IN
093700941012     C*          MODO CHE MI EMETTA IL MESSAGGIO "TARIFFA GIA' IN USO"
093800941012     C   36              GOTO      FINE
093900130131
094000130131       //?Imposto indicatore per protezione campi in modifica tariffe
094100130131       //?ma solo TARIFFE e NO CARTELLO
094200130131     c                   eval      dUTE01 = UTEfaf
094300130131     c                   IF        not *in50 and not *in23 and *in10
094400130131     c                   eval      *in17 = (§UTEmodtar = *blanks)
094500130131     c                   ENDIF
094600130318
094700130318      /free
094800130318
094900130318       //?Imposto la libreria per il TITAS
095000130320       //?in Manutenzione tariffa no cartellp
095100130320        IF  not *in50 and *in10 and not *in23;
095200130320          IF  %subst(knsif : 7 : 1) = 'P';
095300130320            wLib = 'GAITRAGRPS';
095400130320          ELSE;
095500130320            wLib = 'GAITRAGRU';
095600130320          ENDIF;
095700130320         //?Apro il file
095800130320          wFLib = %trim(wLib) + '/TITAS41C';
095900130320          open  TITAS41C;
096000130320         //?Cerco la data ultima spedizione fatturata con la tariffa in
096100130320         //?modifica
096200130320          clear wDataSP;
096300130320          clear TASaas;
096400130320          clear TASmgs;
096500130320          setgt  (TAMKSC:TAMCTR) TITAS41C;
096600130320          readpe (TAMKSC:TAMCTR) TITAS41C;
096700130320          wDataSP = TASaas * 10000 + TASmgs;
096800130320         //?Abilito F16-Annulla
096900130320         //?se data decorrenza tariffa > della data ultima
097000130320         //?spedizione fatturata
097100130322          IF   TAMddt > wDataSP;
097200130318            *in31 = *on;
097300130318          ENDIF;
097400130320         //?mi salvo le date decorrenza e scadenza
097500130318          sav_TAMddt = TAMddt;
097600130318          sav_TAMdst = TAMdst;
097700130318        ENDIF;
097800130320         //?Abilito F16-Annulla
097900130320         //?se offerta
098000130320         //?se tariffa di cartellio
098100130320          IF  *in50 or *in23;
098200130320            *in31 = *on;
098300130320          ENDIF;
098400130318
098500130318      /end-free
098600130131
098700941012     C*
098800941012     C* PULIZIA CAMPI VIDEATA
098900941012     C                   EXSR      INZD01
099000031118     C* 36 ON  - MANCA TABELLA QI ISTAT INDICE RELATIVO ALLA DATA IMMISIIONE
099100031118     C*          TARIFFA EMISISONE DEL MESSAGGIO IN TNTA01R
099200031118     C   36              GOTO      FINE
099300031118     C*
099400931119     C*
099500941012     C* 10 ON  - AGGIORNAMENTO: CARICO DATI VIDEATA
099600941012     C   10              EXSR      RIED01
099700931119     C*
099800900530     C                   SETOFF                                       81
099900931119     C*
100000941012     C***
100100941012     C* EMISSIONE FORMATO 1
100200941012     C***
100300960109     C     FOR012        TAG
100400960423     C* 10 ON  - AGGIORNAMENTO: RIEMPIO CAMPI DELLA 2° VIDEATA
100500061010     C   10              EXSR      RIED02
100600120315
100700120315       //?Imposto i tasti funzione nella riga mobile
100800120315     c                   eval      wPrima = *on
100900120315     c                   eval      wSeconda = *off
101000120315     c                   eval      wTerza = *off
101100120315     c                   eval      wQuarta = *off
101200120315     c                   ExSr      Tastifun
101300960109     C*
101400941012     C     FOR01         TAG
101500120906     C                   Z-ADD     0             V1CRDA
101600120906     C                   MOVEL     *BLANKS       V1CRA
101700151016
101800151016      /free
101900151016       //?visualizzo calcolo peso desunto in fatturazione se:
102000151016       //?utente di sede sempre
102100151016       //?utente di filiale solo se valorizzato
102200151016        *in53 = *off;
102300151016        //ATRtpr = npro;
102400151016        IF  *in18 and V1Ctpr <> *blanks;
102500151016          *in53 = *on;
102600151016        ENDIF;
102700151016        IF  not *in18;
102800151016          *in53 = *on;
102900151016          //clear ATRtpr;
103000151016        ENDIF;
103100151016      /end-free
103200140224
103300140224       //?Se il Fuel non è ok emetto il messaggio
103400140226     c                   IF        wNoOkFuel and not *in17
103500140224     c                   eval      *in28 = *on
103600140224     c                   eval      V1Cmsg = msg(16)
103700140226     c                   eval      V1Cmsg = %trim(V1Cmsg) +
103800140226     c                             ' F21 x modificare.'
103900140224     c                   ENDIF
104000140226
104100140226       //?Se il Fuel non è ok emetto il messaggio
104200140226       //?forzabile se utente non abilitato alla variazione
104300140226       //?della tariffa
104400140226     c                   IF        wNoOkFuel and *in17
104500140226     c                   eval      *in28 = *on
104600140226     c                   eval      V1Cmsg = msg(16)
104700140226     c                   eval      wNoOkfuel = *off
104800140226     c                   ENDIF
104900931119     C*
105000120315     C                   write     TA01Z01
105100940624     C                   EXFMT     TA01D01
105200931119     C*
105300940526     C* PULIZIA CAMPO MESSAGGIO E RELATIVO INDICATORE (*IN28)
105400940526     C                   CLEAR                   V1CMSG
105500940526     C                   SETOFF                                       28
105600940526     C* AZZERO GLI INDICATORI RELATIVI AI MESSAGGI DI ERRORE
105700941012    1C     40            DO        49            I                 3 0
105800940526     C                   MOVE      '0'           *IN(I)
105900941012    1C                   ENDDO
106000150922     C                   SETOFF                                       51  45
106100941010     C                   SETOFF                                       548687
106200090706     c                   setoff                                       60
106300140224       //?Se il Fuel non è ok e non faccio F21 sempre errore
106400140224     c                   IF        wNoOkFuel and not *inkv
106500140224     c                             and not *inkc
106600140224     C                   goto      for01
106700140224     c                   ENDIF
106800140224
106900120314       //?F1 - dati manca tariffa
107000120314     c                   IF        *inka
107100120314     c                   exsr      gesw01
107200120314     c                   goto      For01
107300120314     c                   ENDIF
107400120314
107500011210     C*
107600011214     C* SE PREMUTO F10 E' COME SE FOSSE STATO PREMUTO F6
107700011214     C   KJ              MOVEL     *INKJ         *INKF
107800940519     C*
107900941012     C** CMD12 - RITORNO
108000120315      *  F3 - Fine
108100120315     c   kc
108200120315    1Cor KLVFLG          IFEQ      '5'
108300940503     C* SE PROVENGO DA AGGIUNTA TARIFFA NON ABILITO IL CMD12
108400120315     C   kl              MOVEL     MSG(21)       V1CMSG
108500120315     C   kc              MOVEL     MSG(20)       V1CMSG
108600940526     C                   SETON                                        4028
108700941012     C                   GOTO      FOR01
108800941012   X1C                   ELSE
108900991027     C* IN CASO DI IMMISISONE IMPOSTO COME FLAG DI RITORNO 'C'
109000991027     C   01              MOVEL     'C'           FLGRTN
109100900417     C                   GOTO      FINE
109200941012    1C                   ENDIF
109300931119     C*
109400931119     C** CMD16 - ANNULLAMENTO TOTALE TARIFFA
109500941012    1C     *INKQ         IFEQ      *ON
109600051108      * prima di annullare emetto una videata per chiedere conferma
109700051108     c                   ExSr      Sr_Confann
109800051108     c                   If        w11sino <> 'SI'
109900051108     c                   goto      for01
110000051108     c                   EndIf
110100941012     C                   EXSR      ANNTAR
110200020206     C                   MOVEL     'C'           FLGRTN
110300941012     C                   GOTO      FINE
110400941012    1C                   ENDIF
110500101013
110600101013      /free
110700101013       //?F4 = ISTAT
110800101013        IF  *inkd;
110900101013          clear TRTB12DS1;
111000101013          D12opz = '1';
111100130131          IF  *in17;
111200130104            D12opz = '5';
111300130104          ENDIF;
111400101013          kpjbu = TRTB12DS1;
111500101013          trtb12r1 (kpjba);
111600101013          TRTB12DS1 = kpjbu;
111700130104      /end-free
111800130104     c                   IF        *in10 and not *in50 and
111900150211     c                             not *in23 and *in17
112000130104     c                   eval      *in27 = *on
112100130104     c                   goto      For01
112200130104     c                   ENDIF
112300130104      /free
112400101013          IF  D12f12 = *blanks and D12err = *blanks;
112500101013            V1Crct    = D12sca;
112600101013            w_V1Crct  = D12sca;
112700101013            wDate_ISO = %date(D12data:*iso);
112800101013            wDate_Eur = wDate_ISO;
112900101013            V1Dist    = %dec(wDate_EUR);
113000101013          ENDIF;
113100101013          *in27 = *on;
113200101013          *in43 = *on;
113300101013        ENDIF;
113400101013      /end-free
113500101013     c                   IF        *inkd
113600101013     c                   goto      For01
113700101013     c                   ENDIF
113800101013
113900060116      *
114000030718      * F7 Visualizzazine storico variazioni
114100030718     c                   if        *inkg
114200030718     c                   feod      titav00f
114300030718     c                   clear                   tnta04ds
114400030718     c                   eval      i04ksc = v1cksc
114500030718     c                   eval      i04ctr = v1cctr
114600030718     c                   eval      i04prg = v1cprg
114700030718     c                   eval      i04rag = v1dksc
114800030718     c                   eval      i04dct = v1dctr
114900030718     c                   eval      i04dcv = v1cdcv
115000030718     c                   eval      i04div = v1cdiv
115100030718     c                   eval      i04ndf = v1cnet
115200030718      *
115300030718     c                   call      'TNTA04R'
115400030718     c                   parm                    tnta04ds
115500030718      *
115600030718     c                   goto      for01
115700030718      *
115800030718     c                   endif
115900120315       //?F24 Altri tasti
116000120315     c                   IF        *inky
116100120315     c                   exsr      gesf24
116200120315     c                   goto      For01
116300120315     c                   ENDIF
116400931119     C*
116500941012     C* CONTROLLI
116600941012     C                   EXSR      CTRD01
116700940729     C   27
116800941012     COR 28              GOTO      FOR01
116900991119     C* VALORIZZO LA DIVISA
117000991119     C                   MOVE      V1CDIV        VICDIV
117100940811     C*
117200950119     C** AGGIORNAMENTO
117300950119     C   10              EXSR      AGGTAM
117400050530     c
117500050530     c                   if        *in28
117600050530     c                   goto      for01
117700050530     c                   endif
117800950119     C*
117900941010     C* COMANDI
118000941010     C                   EXSR      COMAND
118100941010     C*
118200941012     C* WFLG = '1' ---> E' STATO PREMUTO UN TASTO DI FUNZIONE
118300941012    1C     WFLG          IFEQ      '1'
118400050530     C* SE PREMUTO F3 DALLE TARIF.PARTICOLARI
118500950116     C*   VADO A FINE PGM ALTRIMENTI RIEMETTO IL FORMATO
118600011214     C     *INKV         IFEQ      *ON
118700950117     C     FLG4          ANDNE     ' '
118800140224
118900140224     c                   IF        wNoOkFuel
119000140224     c                   goto      for01
119100140224     c                   ENDIF
119200050530     c
119300050530     c* Se premuto F3 - rieseguo controllo delle tariffe particolari in scad
119400050530     c                   eval      *inkv='0'
119500050530     c                   exsr      CTRTparEli
119600050530     c
119700050530     c                   if        *in28
119800050530     c                   goto      for01
119900050530     c                   endif
120000050530     c
120100991022     C                   MOVEL     'C'           FLGRTN
120200950116     C                   GOTO      FINE
120300950116     C                   ENDIF
120400950116     C*
120500941012     C                   GOTO      FOR01
120600941012    1C                   ENDIF
120700940811     C*
120800950119     C** CMD6 - AGGIORNAMENTO E FINE LAVORO
120900950119    1C     *INKF         IFEQ      *OFF
121000941012     C***
121100941012     C* EMISSIONE FORMATO 2
121200941012     C***
121300121228     C     FOR021        TAG
121400121227
121500121227       //?Imposto i tasti funzione nella riga mobile
121600121227     c                   eval      wPrima = *off
121700121227     c                   eval      wSeconda = *on
121800121227     c                   eval      wTerza = *off
121900121227     c                   eval      wQuarta = *off
122000121227     c                   ExSr      Tastifun
122100121227
122200121228     c     For02         tag
122300120906     C                   Z-ADD     0             V1CRDA
122400120906     C                   MOVEL     *BLANKS       V1CRA
122500941012     C                   CLEAR                   V2CGCR
122600941012     C                   CLEAR                   V2CGGR
122700941012     C                   CLEAR                   V2CGMR
122800941012     C                   CLEAR                   V2CGVR
122900021118     c                   Eval      *In39 = *Off
123000021118      * Se tariffa FedEx Documenti imposto la natura merce indicata in tabella
123100021118     c                   If        V1cNet = 'F' and DueCtr >= §DfeDalD
123200021118     c                             and DueCtr <= §DfeAlD
123300021118     c                             and §DfeNamD > *Blanks
123400021118     c                   Eval      V2cNas = §DfeNamD
123500021118     c                   Eval      *In39 = *On
123600021118     c                   EndIf
123700940729     C*
123800120319     C                   write     TA01Z02
123900940729     C                   EXFMT     TA01D02
124000940729     C*
124100940729     C* PULIZIA CAMPO MESSAGGIO E RELATIVO INDICATORE (*IN28)
124200940729     C                   CLEAR                   V1CMSG
124300940729     C* AZZERO GLI INDICATORI RELATIVI AI MESSAGGI DI ERRORE
124400940729     C                   SETOFF                                       777879
124500000118     C                   SETOFF                                       802849
124600120314       //?F1 - dati manca tariffa
124700120314     c                   IF        *inka
124800120314     c                   exsr      gesw01
124900120314     c                   goto      For02
125000120314     c                   ENDIF
125100011210     C*
125200011214     C* SE PREMUTO F10 E' COME SE FOSSE STATO PREMUTO F6
125300011214     C   KJ              MOVEL     *INKJ         *INKF
125400120315
125500120315       //?F3 - Fine
125600120315     c   kc              GOTO      FINE
125700940729     C*
125800940729     C** CMD12 - FINE LAVORO
125900960109     C   KL              GOTO      FOR012
126000940729     C*
126100940729     C** CMD16 - ANNULLAMENTO TOTALE TARIFFA
126200950119    2C     *INKQ         IFEQ      *ON
126300051108      * prima di annullare emetto una videata per chiedere conferma
126400051108     c                   ExSr      Sr_Confann
126500051108     c                   If        w11sino <> 'SI'
126600051108     c                   goto      for02
126700051108     c                   EndIf
126800941012     C                   EXSR      ANNTAR
126900020206     C                   MOVEL     'C'           FLGRTN
127000941012     C                   GOTO      FINE
127100950119    2C                   ENDIF
127200121227
127300121227      *
127400121227      * F7 Visualizzazine storico variazioni
127500121227     c                   if        *inkg
127600121227     c                   feod      titav00f
127700121227     c                   clear                   tnta04ds
127800121227     c                   eval      i04ksc = v1cksc
127900121227     c                   eval      i04ctr = v1cctr
128000121227     c                   eval      i04prg = v1cprg
128100121227     c                   eval      i04rag = v1dksc
128200121227     c                   eval      i04dct = v1dctr
128300121227     c                   eval      i04dcv = v1cdcv
128400121227     c                   eval      i04div = v1cdiv
128500121227     c                   eval      i04ndf = v1cnet
128600121227      *
128700121227     c                   call      'TNTA04R'
128800121227     c                   parm                    tnta04ds
128900121227      *
129000121227     c                   goto      for02
129100121227     c                   endif
129200121227
129300120315       //?F24 Altri tasti
129400120315     c                   IF        *inky
129500120315     c                   exsr      gesf24
129600121228     c                   goto      For02
129700120315     c                   ENDIF
129800940729     C*
129900941012     C* CONTROLLI
130000941012     C                   EXSR      CTRD02
130100940729     C   27
130200941012     COR 28              GOTO      FOR02
130300950119    1C                   ENDIF
130400941013     C*
130500931119     C** AGGIORNAMENTO
130600000000     C                   EXSR      AGGTAM
130700050530     c
130800050530     c                   if        *in28
130900050530     c                   goto      for02
131000050530     c                   endif
131100980309     C* PRIMA DI USCIRE DALL'AGGIORNAMENTO DEGLI ARCHIVI CONTROLLO SE
131200980309     C* IN CASO DI TARIFFA IMPORT LE TARIFFE SONO CORRETTE ALTRIMENTI
131300980309     C* EMETTO UN MESSAGGIO DI AVVERTIMENTO
131400980309     C     UNO           IFEQ      ' '
131500980309     C                   EXSR      CTRIMP
131600980309     C   28              MOVEL     '1'           UNO
131700980309     C   28              GOTO      FOR02
131800980309     C                   END
131900980309     C*
132000941011     C*
132100941011     C* COMANDI
132200941011     C                   EXSR      COMAND
132300941011     C*
132400950116     C* WFLG = '1' ---> E' STATO PREMUTO UN TASTO DI FUNZIONE
132500950119    1C     WFLG          IFEQ      '1'
132600011214     C* SE PREMUTO CMD3 DALLE TARIF.PARTICOLARI
132700950116     C*   VADO A FINE PGM ALTRIMENTI RIEMETTO IL FORMATO
132800011214     C     *INKV         IFEQ      *ON
132900950117     C     FLG4          ANDNE     ' '
133000950116     C                   GOTO      FINE
133100950116     C                   ENDIF
133200950116     C*
133300941012     C                   GOTO      FOR02
133400941013    1C                   ENDIF
133500920525     C*
133600920525     C** CMD6 - AGGIORNAMENTO E FINE LAVORO
133700990914     C* AGGIORNO IL FLAG DI RITORNO
133800990914     C   KF              MOVEL     'C'           FLGRTN
133900920525     C   KF              GOTO      FINE
134000000000     C** ENTER- AGGIORNAMENTO E AVANZAMENTO
134100921119     C*
134200000000     C     SUSF1         TAG
134300921118     C** 20 ON  - E' STATA BATTUTA UNA CHIAVE DI RICERCA
134400921118     C** 20 OFF - NON E' STATA BATTUTA NESSUNA CHAIVE DI RICERCA
134500940526     C     V1CRDA        COMP      0                                  2020
134600120723     C   20V1CRA         COMP      *BLANKS                            20
134700921118     C*
134800930224     C                   SETOFF                                       21
134900921118     C*
135000941011     C** 20 OFF - SIGNIFICA CHE NON E'STATA BATTUTA NESSUNA CHIAVE
135100990601     C**    DI RICERCA PER CUI ESEGUO CHAIN PER CLIENTE SU TITAD
135200941013    1C  N20              DO
135300061011     C  n50KTAM2         SETLL     TITAD000                               21
135400061011     c   50ktam2         setll     tiofd000                               21
135500900924     C   21              MOVEL     'A'           TADATB
135600941013    2C   21TADATB        DOWNE     *BLANKS
135700061011     C  n50KTAM2         READE     TITAD000                               32
135800061011     c   50ktam2         reade     tiofd000                               32
135900941013    2C  N32              ENDDO
136000921218     C   21
136100921218     CAN 32              SETOFF                                       21
136200061011     c   21
136300061011     Cann50KTAM2         SETLL     TITAD000
136400061011     c   21
136500061011     can 50ktam2         setll     tiofd000
136600941013    1C                   ENDDO
136700921118     C*
136800921118     C** 20 ON  - SIGNIFICA CHE E' STATA BATTUTA UNA CHIAVE DI RICERCA
136900921118     C**    PER CUI ESEGUO CHAIN PER CHIAVE DI RICERCA DA A
137000941013    1C   20              DO
137100120713     c                   IF        %scan('?':V1Cra) > *zeros
137200120713     c                   exsr      Ric_CT
137300120713     c                   eval      V1Cra = TB09cod
137400120713     c                   ENDIF
137500120906     c                   IF        v1cra = *blanks
137600120906     c                   goto      susf1
137700120906     c                   ENDIF
137800120906     c                   IF        v1cra <> *blanks
137900921118     C                   Z-ADD     1             A
138000940526     C     V1CRA         LOOKUP    CCT(A)                                 35
138100020215     C   35              MOVE      ORP(A)        VIDORP
138200940524     C  N35              MOVEL     *BLANKS       VIDORP
138300061011     C  n50KTTAD         CHAIN     TITAD000                           32
138400061011     c   50kttad         chain     tiofd000                           32
138500921118     C  N32              SETON                                        21
138600120723     c                   ENDIF
138700941013    1C                   ENDDO
138800921118     C*
138900941013     C** 21 ON  - SIGNIFICA CHE PER QUEL CLIENTE ESISTE ALMENO UNA RIGA
139000941013     C**    DI TARIFFA QUINDI VISUALIZZO LE TARIFFE CHE HO DALL'INIZIO
139100941013    1C     *IN21         IFEQ      *ON
139200941013     C** ESEGUO AGGIORNAMENTO
139300921118     C                   EXSR      AGGIOR
139400921118     C*
139500120315     c   kc              GOTO      FINE
139600121228     C   KL              GOTO      FOR021
139700921118     C** CMD6 - AGGIORNAMENTO E FINE LAVORO
139800980309     C* PRIMA DI USCIRE DALL'AGGIORNAMENTO DEGLI ARCHIVI CONTROLLO SE
139900980309     C* IN CASO DI TARIFFA IMPORT LE TARIFFE SONO CORRETTE ALTRIMENTI
140000980309     C* EMETTO UN MESSAGGIO DI AVVERTIMENTO
140100980310     C   KFUNO           IFEQ      ' '
140200980310     C                   EXSR      CTRIMP
140300980310     C   28              MOVEL     '1'           UNO
140400121228     C   28              GOTO      FOR021
140500980310     C                   END
140600980309     C*
140700011214     C   KF              GOTO      FINE
140800941013    1C                   ENDIF
140900921118     C*
141000941013     C** 21 OFF - SIGNIFICA CHE SI TRATTA DI IMMISSIONE COMPLETA IN
141100941013     C**    QUANTO NON ESISTE NEMMENO UNA RIGA DI TARIFFA PER IL CLIENTE
141200941013    1C     *IN21         IFEQ      *OFF
141300921118     C                   EXSR      IMMISS
141400980313     C* IN CASO DI TARIFFA IMPORT LA TARIFFA PARTICOLARE ASSIC. X CONTO
141500980313     C* DEVE ESSERE CORRETTA
141600980313     C     UNO           IFEQ      ' '
141700980313     C                   EXSR      CTRIMP
141800980313     C   28              MOVEL     '1'           UNO
141900121228     C   28              GOTO      FOR021
142000980313     C                   END
142100921118     C*
142200120315     c   kc              goto      fine
142300121228     C   KL              GOTO      FOR021
142400961231     C*
142500961231     C** CMD15-P O R T O  (ABILITATO SOLO NELLE OFFERTE)
142600980902     C   KP              DO
142700031124      * recupero dalla tabella CAT la data di elaborazione CAT consigliata
142800031124     c                   Clear                   Tibs02Ds
142900031124     c                   Clear                   dCat
143000031124     c                   Eval      T02Mod = 'C'
143100031124     c                   Eval      T02Sif = Knsif
143200031124     c                   Eval      T02Cod = 'CAT'
143300031124     c                   Movel(p)  '1'           T02Ke1
143400031124     c                   Call      'TIBS02R'
143500031124     c                   Parm                    Kpjba
143600031124     c                   Parm                    Tibs02Ds
143700031124     c                   If        T02Err = *Blanks
143800031124     c                   Movel     T02Uni        dCat
143900031124     c                   EndIf
144000031124    1 *
144100980514     C                   CLEAR                   DSTE00
144200980514     C                   MOVEL     'O02'         D00OP0
144300980514     C                   MOVEL     'F15'         D00OP1
144400980514     C                   MOVEL     'P'           D00TLA
144500091127      * Con l'arrivo delle trattative dobbiamo segnalare ai pgm del CAT che
144600091127      * stiamo parlando di trattativa e non di visita
144700091127      * Se trattativa (ind. 85 acceso) impostiamo "X" nel campo D00DSF
144800091125     C*                  MOVEL     'S'           D00DSF
144900091125     C   85              MOVEL     'X'           D00DSF
145000091127     C                   MOVEL     'O'           D00CTO
145100091125     C                   MOVEL     V1CPRG        D00PRG
145200980514     C                   MOVEL     V1CNRV        D00KSC
145300980514     C                   MOVEL     V1CCTR        D00CTR
145400980514     C                   MOVEL     V1CFIE        D00FIE
145500980514     C                   MOVEL     V1CTSP        D00TSP
145600011023     C                   MOVEL     V1CDIV        D00DIV
145700020205     C                   MOVE      V1CNET        D00DPD
145800011030     C                   MOVEL     *BLANKS       D00PRI
145900031124      * propongo data massima tra la data del giorno e quella recuperata
146000031124      * dalla tavella CAT
146100031124     c                   if        dateu8 > §CATdta
146200980514     C                   MOVEL     DATEU8        D00DTC
146300031124     C                   ELSE
146400031124     C                   MOVEL     §CATDTA       D00DTC
146500031124     C                   ENDIF
146600031124      *
146700980514     C                   CALL      'TNTE01C'
146800980514     C                   PARM                    KPJBA
146900980514     C                   PARM                    DSTE00
147000961231     C* D00ERR = "1" --> ERRORE
147100980514    3C     D00ERR        IFEQ      '1'
147200980514     C                   SETON                                          28
147300980514     C                   MOVEL     D00MSG        $MSG
147400980514    3C                   ENDIF
147500980902    2C                   ENDDO
147600961231     C*
147700921118     C** CMD6 - AGGIORNAMENTO E FINE LAVORO
147800980309     C* PRIMA DI USCIRE DALL'AGGIORNAMENTO DEGLI ARCHIVI CONTROLLO SE
147900980309     C* IN CASO DI TARIFFA IMPORT LE TARIFFE SONO CORRETTE ALTRIMENTI
148000980309     C* EMETTO UN MESSAGGIO DI AVVERTIMENTO
148100980310     C   KFUNO           IFEQ      ' '
148200980310     C                   EXSR      CTRIMP
148300980310     C   28              MOVEL     '1'           UNO
148400121228     C   28              GOTO      FOR021
148500980310     C                   END
148600980309     C*
148700921118     C   KF              GOTO      FINE
148800941013    1C                   ENDIF
148900921118     C*
149000921118     C                   GOTO      SUSF1
149100921118     C*
149200000000     C     FINE          TAG
149300921118     C** ELIMINAZIONE SU DISCO SUBFILE SF E S2
149400921118     C                   EXSR      ELISF
149500070523      * se tariffa/offerta allocata vado in fondo e non faccio + niente
149600070523     c                   if        parall = 'A'
149700070523     c                   goto      fondo
149800070523     c                   endif
149900921118     C*
150000940615     C** 81 ON  - INDICATORE DI MODIFICA TNTAM
150100990601     C** 30 ON  - INDICATORE DI MODIFICA TITAD
150200940111     C** ESEGUO L'AGGIORNAMENTO DELLA DATA DI VARIAZIONE SOLO SE *IN81
150300940114     C*   E' SPENTO E SE NON E' STATA ANNULLATA L'OFFERTA
150400941013     C   30
150500941013    1CANN81              DO
150600061011     C  n50KTAM2         CHAIN     TNTAM000                           01
150700061011     c   50ktam2         chain     tnofm000                           01
150800941221     C                   Z-ADD     DATEU8        TAMDUV
150900940511     C                   MOVEL     *BLANKS       TAMFTR
151000040913     C                   Z-ADD     dateu8        TAMDTR
151100061011     c  n01
151200061011     Cann50              UPDATE    TNTAM000
151300061011     c  n01
151400061011     Can 50              UPDATE    tnofm000
151500030714      * valorizzo il flag per la scrittura del file storico fasi
151600030714     c                   eval      mandet = 'S'
151700030714      *
151800941013    1C                   ENDDO
151900030716      * verifico:se è stato manutenzionato la testata e scrivo lo storico
152000030716     c                   if        mantes = 'S'
152100030716      * aggancio la testata per verificare se ho modificato qualcosa
152200061011     C  n50KTAM2         CHAIN(N)  TNTAM000
152300061011     c   50ktam2         chain(n)  TNofm000
152400061011     c                   if        %found
152500030716      * verifico i soli campi che mi interessano per la scrittura del record
152600030716      * con quelli salvati in COMODO$
152700030716     c                   if        tamtsp <> $tamtsp   or
152800030716     c                             tampag <> $tampag   or
152900041129     c                             tamppa <> $tamppa   or
153000030716     c                             tamflo <> $tamflo   or
153100030716     c                             tamkpm <> $tamkpm   or
153200030716     c                             tamata <> $tamata   or
153300030716     c                             tamarl <> $tamarl   or
153400030716     c                             tamarf <> $tamarf   or
153500030716     c                             tamaro <> $tamaro   or
153600030716     c                             tamfis <> $tamfis   or
153700030716     c                             tamfvd <> $tamfvd   or
153800030716     c                             tamftm <> $tamftm   or
153900031119     c                             tamfts <> $tamfts   OR
154000101013     c                             tamrct <> $tamrct   OR
154100060324     c                             tampri <> $tampri   or
154200151002     c                             tamfmp <> $tamfmp   or
154300151002     c                             tamtpr <> $tamtpr
154400030716      *
154500030716     c                   eval      tavtrd = 'TES'
154600030716     c                   eval      tavcta = 'M'
154700030716     c                   exsr      SUB_STORICO
154800030716     c                   endif
154900030716     c                   endif
155000030716      *
155100030716     c                   endif
155200030714      * verifico:se è stato manutenzionato il dettaglio scrivo lo storico
155300030714     c                   if        mandet = 'S'
155400030714     c* in questo caso scrivo lo storico per il dettaglio tariffario
155500030714     c                   eval      tavtrd = 'DET'
155600030714     c                   eval      tavcta = 'M'
155700030714     c                   exsr      SUB_storico
155800030716      *
155900030716     c                   clear                   mandet
156000030714      *
156100030714     c                   endif
156200030714      * verifico:se è stato manutenzionato le giacenze scrivo lo storico
156300030714     c                   if        mangia = 'S'
156400030714     c* in questo caso scrivo lo storico per il dettaglio tariffario
156500030714     c                   eval      tavtrd = 'GIA'
156600030714     c                   eval      tavcta = 'M'
156700030714     c                   exsr      SUB_storico
156800030716      *
156900030716     c                   clear                   mangia
157000030714      *
157100030714     c                   endif
157200030714     c*
157300000918     C*** CONTROLLO SE LE TARIFFE PARTICOLARI CHE SONO SEMPRE FISSE ESISTONO
157400000918     C*** GIA' IN TARIFFA  SE NON È ANNULLATA
157500000919     C*** NON ESEGUO PER LE TARIFFE DI CARTELLO
157600000919     C     *IN23         IFEQ      *OFF
157700070523     C  n50KTAM2         CHAIN(n)  TNTAM000
157800070523     c   50ktam2         chain(n)  tnofm000
157900061206     c                   if        %found and tamatb = *blanks
158000151217      * se tariffa e sono in variazione ma non posso variare la tariffa
158100151217      * non devo fare la inglo
158200170124     c*****              IF        not *in50 and *in10 and *in17
158300170124      * se sono in Tariffa e sono in Variazione ma non posso annullare la
158400170124      * tariffa, quindi vuol dire che è già stata utilizzata per la
158500170124      * tassazione, non devo fare la INGLO
158600170124     c                   IF        not *in50 and *in10 and not *in31
158700151217     c                   goto      saltainglo
158800151217     c                   ENDIF
158900000918     C                   EXSR      INGLO
159000151217     c     saltainglo    tag
159100080522
159200080521      * se immissione (tariffa/offerta) devo creare in automatico
159300080521      * la tariffa particolare 'f' = Fuel
159400080521      * se non già presente
159500080610     c                   if        win01
159600080521     c                   exsr      sr_fuel
159700080521     c                   endif
159800080522
159900061206     c                   endif
160000000919     C                   ENDIF
160100030714      * verifico se sono state manutenzionate delle tariffe particolari
160200030714      * scrivo lo storico
160300030714     c                   z-add     0             WW
160400030714      *
160500030714     c                   do        *hival
160600030714     c                   add       1             ww
160700030714
160800030714     c                   if        ww > 150
160900030714     c                   leave
161000030714     c                   endif
161100030714
161200030714     c                   if        manftc(ww) = *blank
161300030714     c                   leave
161400030714     c                   endif
161500030714
161600030714     c                   eval      TAVtrd = 'PAR'
161700030714     c                   movel     manftc(WW)    TAVftc
161800030714     c                   move      manftc(WW)    TAVcta
161900030714     c                   exsr      SUB_storico
162000030714
162100030714     c                   enddo
162200030716      *
162300030716     c                   clear                   manftc
162400030717     c                   clear                   first
162500030717     c                   clear                   instes
162600040927     c                   clear                   annultar
162700000918     C*
162800061011     C  n50KTAM2         SETLL     TNTAM000                               01
162900061011     c   50ktam2         setll     tnofm000                               01
163000061011     C  n50KTAM2         SETLL     TITAD000                               01
163100061011     c   50ktam2         setll     tiofd000                               01
163200011210     C*
163300011214     C* SE PREMUTO F10: OLTRE AGLI AGGIORNAMENTI (DA F6)
163400011210     C*  DEVO FARE LA STAMPA TARIFFA.
163500011214     C   KJ              MOVE      '2'           PA2FLG
163600011214     C   KJ              EXSR      STAMPA
163700070523
163800070523     c     fondo         tag
163900070523
164000941007     C*
164100970718     C* CALL A VUOTO AL PGM FNLV13R PER CHIUDERE I FILES
164200970718    1C     FNLV13        IFEQ      '1'
164300970718     C                   MOVEL     'C'           I13TLA
164400970718     C                   CALL      'FNLV13R'
164500970718     C                   PARM                    KPJBA
164600970718     C                   PARM                    DSLV13
164700970718     C                   PARM                    DSSI95
164800941013    1C                   ENDIF
164900941011     C*
165000970122     C* CALL A VUOTO AL PGM TNTA47R PER CHIUDERE I FILES
165100970122     C     WTA47         IFEQ      '1'
165200970122     C                   CLEAR                   DSTA47
165300970122     C                   MOVEL     'C'           D47TLA
165400970122     C                   MOVEL     DSTA47        KPJBU
165500970122     C                   CALL      'TNTA47R'
165600970117     C                   PARM                    KPJBA
165700970117     C                   ENDIF
165800020205      *
165900020205     C* RITORNO PARAMETRI AL TNTA01R
166000020205     C                   MOVEL     PARAMX        KPJBU
166100120315     c                   IF        *in97
166200120315     c                   eval      parm48 = tnsb48ds
166300120315     c                   ENDIF
166400970117     C*
166500061012     c                   eval      *inlr = *on
166600940503     C*
166700941010     C*--- ROUTINE COMANDI -------------------------------------------*
166800941010     C     COMAND        BEGSR
166900941012     C                   MOVEL     ' '           WFLG              1
167000941010     C*
167100941010     C** CMD14 - GESTIONE TARIFFE GIACENZE
167200941012    1C     *INKN         IFEQ      *ON
167300091210     C                   MOVEL     V1CKSC        pa3cli
167400091210     C  n50              MOVEL     'C'           pa3AST
167500091210     C   50              MOVEL     'V'           pa3AST
167600091210     C                   Z-ADD     V1CCTR        pa3CTR
167700091210     C                   Z-ADD     V1CPRG        pa3PRG
167800981113     C                   MOVEL     V1DCTR        PA3DCT
167900981113     C                   MOVEL     V1CDCV        PA3DCV
168000030714     C                   CLEAR                   PA3MAN
168100961206     C                   MOVEL     V1CFIE        PA3FIE
168200990824     C                   MOVEL     V1CDIV        PA3DIV
168300990622     C                   MOVEL     §CVFDC        PA3FDC
168400020204      * NETWORK
168500020204     C                   MOVE      V1CNET        PA3NET
168600090916      * trattativa
168700090916     c                   eval      pa3trat = partrat
168800010629      *
168900941010     C                   MOVEL     PARAM3        KPJBU
169000060207     C                   CALL      'TNTA14R'
169100941010     C                   PARM                    KPJBA
169200950116     C                   MOVEL     KPJBU         PARAM3
169300030714      *
169400030714     c                   if        pa3MAN <> ' '
169500030714     c                   eval      mangia = 'S'
169600030714     c                   endif
169700950116     C*
169800950116     C* 96 ON  - ESISTONO TARIFFE GIACENZA   : EVIDENZIO IL COMANDO
169900950116     C                   SETOFF                                       96
170000990611     C  N50KTAM2         CHAIN(N)  TITGC000                           32
170100990611     C   50KTAM2         CHAIN(N)  TIOGC000                           32
170200950116    2C  N32TGCATB        IFEQ      ' '
170300950116     C                   SETON                                        96
170400950116    2C                   ENDIF
170500941010     C*
170600941012     C                   MOVEL     '1'           WFLG
170700941010     C                   GOTO      ENDCOM
170800941012    1C                   ENDIF
170900941010     C*
171000941010     C** CMD21 - GESTIONE TARIFFE PARTICOLARI
171100941012    1C     *INKV         IFEQ      *ON
171200140224
171300140224
171400980309     C* CONTROLLO SE LA TARIFFA CHE STO MANUTENZIONANDO E' UNA TARIFFA IMPORT
171500980309     C*
171600980309     C                   EXSR      CTRIMP
171700980309     C*
171800941010     C                   Z-ADD     V1CKSC        CLI5
171900941010     C                   MOVEL     V1DKSC        RAG5
172000941010     C  N50              MOVEL     ' '           FLG
172100941010     C   50              MOVEL     'A'           FLG
172200941010     C                   MOVEL     ' '           FLG4
172300990622     C* MUOVO FLAG DECIMALI SI/NO IN BASE ALLA DIVISA
172400991005     C                   MOVEL     §CVFDC        PA5FDC
172500000915     C* PASSO LA DATA DECORRENZA DELLA TARIFFA
172600000915     C                   Z-ADD     COMDDT        PA5DDT
172700050531     C                   movel     COMDsT        PA5DsT
172800000915     C* PASSO IL TIPO SERVIZIO BOLLA
172900000915     C                   MOVE      V1CTSP        PA5TSP
173000020201     C* Flag Network
173100020201     C                   MOVEL     V1CNET        FLGNET
173200060306      * flag rinuncia AC BASE
173300060306     c                   movel     v2cfmp        pa5fmp
173400090916      * trattativa
173500090916     c                   eval      pa5trat = partrat
173600000915     C*
173700941010     C                   MOVEL     PARAM5        KPJBU
173800120320     c                   IF        not *in97
173900030714     C                   CALL      'TNTA67R'
174000941010     C                   PARM                    KPJBA
174100030714     c                   parm                    manftc
174200120320     c                   ELSE
174300120320     C                   CALL      'TNTA67R'
174400120320     C                   PARM                    KPJBA
174500120320     c                   parm                    manftc
174600120320     c                   parm                    tnsb48ds
174700120320     c                   ENDIF
174800941010     C*
174900941010     C                   MOVEL     KPJBU         PARAM5
175000140224
175100140224      * ricontrollo il Fuel
175200140224     c                   exsr      ctr_fuel
175300140224
175400950116     C*
175500950116     C* SOLO SE NON E' STATO PREMUTO CMD3(FLG4=' ') CONTROLLO SE SONO
175600950116     C*   PRESENTI TARIFFE PARTICOLARI PER EVIDENZIARE IL COMANDO
175700950116    2C     FLG4          IFEQ      ' '
175800950116     C* 95 ON  - ESISTONO TARIFFE PARTICOLARI: EVIDENZIO IL COMANDO
175900950116     C                   SETOFF                                       95
176000061011     C  n50KTAM2         SETLL     TITPT000
176100061011     c   50ktam2         setll     tiopt000
176200061011     C  n50KTAM2         READE(N)  TITPT000                               32
176300061011     C   50ktam2         reade(n)  tiopt000                               32
176400950116    3C     *IN32         DOWEQ     *OFF
176500950116     C     TPTATB        IFEQ      ' '
176600950116     C                   SETON                                        3295
176700950116     C                   ELSE
176800061011     C  n50KTAM2         READE(N)  TITPT000                               32
176900061011     c   50ktam2         reade(n)  tiopt000                               32
177000950116     C                   ENDIF
177100950116    3C                   ENDDO
177200950116     C*
177300950116    2C                   ENDIF
177400941010     C*
177500941011     C                   MOVEL     '1'           WFLG
177600941010     C                   GOTO      ENDCOM
177700941012    1C                   ENDIF
177800950116     C*
177900941010     C** CMD18 - GESTIONE NOTE
178000100407      /free
178100100806       //?Nuovo programma gestione note
178200100806        IF  *inks;
178300100407          clear trmk20ds;
178400110511          IMK20TLA = 'L' ;
178500100407
178600101126         //?passo trattativa  (abbiamo tolto richiamo delle note da trattativa)
178700100407          IF  *in85;
178800100407            IMK20nrv = v1cksc;
178900100407         //?passo cliente
179000100407          ELSE;
179100100407            IMK20ksc = v1cksc;
179200100407          ENDIF;
179300100407         //?ragione sociale
179400100407          IMK20rsc = v1dksc;
179500100407
179600100407          trmk20r (kpjba : TRMK20DS);
179700100407          wflg = '1';
179800100407          leavesr;
179900100407        ENDIF;
180000100806
180100100407       //?F8 - Note tariffa
180200100806        IF  *inkh;
180300100407          clear TNTA28ds;
180400100407
180500100407          ITA28ela = 'M';
180600100407
180700100407         //?richiamo per trattativa
180800100407          IF  *in85;
180900100407            ITA28tip = 'T';
181000100407          ELSE;
181100100407         //?richiamo per tariffe clienti
181200100407            ITA28tip = 'C';
181300100407          ENDIF;
181400100407
181500100407          ITA28cod = V1cksc;
181600100407          ITA28rsc = V1dksc;
181700100407          kpjbu    = TNTA28DS;
181800100407
181900100407          tnta28r (kpjba);
182000100407          wflg = '1';
182100100407          leavesr;
182200100407        ENDIF;
182300100407      /end-free
182400090709
182500090709     C** F2=RUBRICA
182600090709    1c                   if        *inkb
182700090709     c                   clear                   tnta12ds
182800090709     c                   eval      ta12ric = 'M'
182900090709     c  n50              Eval      ta12apl = 'C'
183000090709     c  n50              Eval      ta12ksc = v1cksc
183100090916     c  n85
183200090916     can 50              Eval      ta12apl = 'V'
183300090916     c   85              Eval      ta12apl = 'T'
183400090709     c   50              Eval      ta12nrv = v1cnrv
183500090709     c                   Eval      ta12rag = v1dksc
183600090709     c                   Call      'TNTA12R'
183700090709     c                   Parm                    kpjba
183800090709     c                   Parm                    tnta12ds
183900090709     C*
184000090709     C                   MOVEL     '1'           WFLG
184100090709     C                   GOTO      ENDCOM
184200090709    1C                   ENDIF
184300941010     C*
184400941010     C     ENDCOM        ENDSR
184500941010     C*
184600941013     C*--- PULIZIA CAMPI VIDEATA -------------------------------------*
184700941012     C     INZD01        BEGSR
184800031118     C                   SETOFF                                       8836
184900941124     C                   CLEAR                   WSAP
185000980505     C                   CLEAR                   WRPV
185100980505     C                   CLEAR                   WRKMIN
185200940503     C*
185300940526     C                   Z-ADD     PRG           V1CPRG
185400020201     C                   CLEAR                   V1CNET
185500000118     C                   CLEAR                   V1CDDT
185600940526     C                   CLEAR                   V1CDST
185700940526     C                   CLEAR                   V1CDUV
185800940526     C                   CLEAR                   V1DCTR
185900940526     C                   CLEAR                   V1CDCV
186000101013     C                   CLEAR                   V1Crct
186100940526     C                   CLEAR                   V1CPRI
186200031118     C                   CLEAR                   V1DIST
186300031118     c                   CLEAR                   PARIST
186400940526     C                   CLEAR                   V1CPAG
186500041129     C                   CLEAR                   V1CPpa
186600940526     C                   CLEAR                   V1CKPM
186700940526     C                   CLEAR                   V1CMPC
186800940526     C                   CLEAR                   V1CARL
186900940526     C                   CLEAR                   V1CARF
187000940526     C                   CLEAR                   V1CARO
187100940526     C                   CLEAR                   V1CATA
187200940526     C                   CLEAR                   V1CFTP
187300940526     C                   CLEAR                   V1CBAP
187400940526     C                   CLEAR                   V1CTSP
187500940811     C                   CLEAR                   V1DTSP
187600041012     c                   Eval      v1cdiv = 'EUR'
187700990616     C                   CLEAR                   V1CMIS
187800990616     C                   CLEAR                   V1CMAS
187900940526     C                   CLEAR                   V1CFIE
188000980505     C                   CLEAR                   V1CMAT
188100150922     C                   CLEAR                   V1Ctpr
188200941108     C                   CLEAR                   WFIE
188300020215     C                   CLEAR                   WTSP
188400020215     C                   CLEAR                   WNET
188500940705     C                   MOVEL     '0'           V2CFIS
188600960621     C  N50              MOVEL     'S'           V2CSIS
188700960621     C   50              CLEAR                   V2CSIS
188800940705     C                   MOVEL     '1'           V2CFTS
188900941108     C* 16 ON  - TIPO TARIFFA "4"-A VALORE
189000940811     C   16              MOVEL     '1'           V2CFVD
189100970128     C                   MOVEL     'N'           V2CBAM
189200940705     C                   CLEAR                   V2CNAS
189300040601     c                   Clear                   v2cbva
189400940705     C                   CLEAR                   V2CNOT
189500060303     c                   clear                   v2cfmp
189600940705     C                   CLEAR                   V2CFTM
189700940705     C                   CLEAR                   V2CGCA
189800940705     C                   CLEAR                   V2CGGA
189900940705     C                   CLEAR                   V2CGMA
190000940705     C                   CLEAR                   V2CGVA
190100941012     C                   CLEAR                   V2CGCR
190200941012     C                   CLEAR                   V2CGGR
190300941012     C                   CLEAR                   V2CGMR
190400941012     C                   CLEAR                   V2CGVR
190500940503     C*
190600131028     C                   CLEAR                   savpri
190700131028     C*
190800970317     C                   MOVEL     *BLANKS       COMODO
190900030716     C                   MOVEL     *BLANKS       COMODO$
191000940526     C                   MOVE      VCTR          V1CCTR
191100000000     C                   Z-ADD     0             COMLRC            7 0
191200000000     C                   Z-ADD     0             COMLAS            7 0
191300061011      * se sono in inserimento e non in copia 01 = *on
191400031118      * propongo il primo scaglione ISTAT con data scatto successiva
191500031118      * la data del giorno
191600061011     c                   If        *in01
191700120124     C                   Z-ADD     0             PI                4 0
191800031118     c                   z-add     0             svdia             8 0
191900101013     c                   z-add     0             svrct             3 0
192000101013    3C                   DO        999           PI
192100031118      * verifico la data scatto con la data del giorno
192200031121    3C                   IF        dateu8 < DIA(PI)
192300031118      * salvo la data e lo scaglione
192400031118     c                   eval      svdia = dia(pi)
192500101013     c                   eval      svrct = pi
192600031121     c                   leave
192700031118     c                   endif
192800031118      *
192900031118     c                   enddo
193000031118      * se non trovato scaglione ritorno al TNTA01R con  errore bloccante
193100101013     c                   if        svdia = 0  and svrct = 0
193200031118     c                   eval      parist = 'E'
193300031118     c                   seton                                        36
193400031118     c                   goto      endinz
193500031118     c                   endif
193600031118      *
193700101013     c                   eval      v1crct = svrct
193800031118     c                   eval      v1cpri = 100
193900031118      *
194000031118     c                   z-add     svdia         g02INV
194100031118     c                   movel     '3'           g02err
194200031118     c                   CALL      'XSRDA8'
194300031118     c                   PARM                    WLBDAT
194400031118      * GG/MM/AAAA
194500031118     c                   z-add     g02DAT        v1diST
194600031118      *
194700031118     c                   endif
194800031118      *
194900991119     C*
195000941012     C*
195100941012     C* DECODIFICA CODICE TARIFFA
195200941012     C                   Z-ADD     1             A
195300920525     C                   MOVEL     VCTR          UNOCTR
195400940524     C     UNOCTR        LOOKUP    CTR(A)                                 35
195500941108    1C     *IN35         IFEQ      *ON
195600941108     C                   MOVEL     DTR(A)        V1DCTR
195700941124     C* 88 ON  - IMPORTO TARIFFA IN PERCENTUALE
195800941108    2C     TAP(A)        IFEQ      'S'
195900941123     C                   SETON                                        88
196000941108    2C                   ENDIF
196100950116     C* WSAP = "S" ---> SCAGLIONE CON DECIMALI
196200941124     C                   MOVEL     SAP(A)        WSAP              1
196300980505     C* WRPV = "S" ---> ABILITATA LA GESTIONE DEL RAPPORTO PESO VOLUME
196400980505     C                   MOVEL     RPV(A)        WRPV              1
196500980505     C* WRKMIN = SALVO IL MINIMO TASSABILE PER QUESTA TARIFFA
196600980505     C                   Z-ADD     MIN(A)        WRKMIN           13 3
196700941108    1C                   ENDIF
196800941012     C*
196900061011     C* CONTROLLO SE DEVO EVIDENZIARE I COMANDI
197000950121     C                   SETOFF                                       9596
197100950121     C*
197200950116     C* 95 ON  - ESISTONO TARIFFE PARTICOLARI: EVIDENZIO IL COMANDO
197300061011     C  n50KTAM2         SETLL     TITPT000
197400061011     c   50ktam2         setll     tiopt000
197500061011     C  n50KTAM2         READE(N)  TITPT000                               32
197600061011     c   50ktam2         reade(n)  tiopt000                               32
197700950116    3C     *IN32         DOWEQ     *OFF
197800950116     C     TPTATB        IFEQ      ' '
197900950116     C                   SETON                                        3295
198000950116     C                   ELSE
198100061011     C  n50KTAM2         READE(N)  TITPT000                               32
198200061011     c   50ktam2         reade(n)  tiopt000                               32
198300950116     C                   ENDIF
198400950116    3C                   ENDDO
198500950116     C*
198600950116     C* 96 ON  - ESISTONO TARIFFE GIACENZA   : EVIDENZIO IL COMANDO
198700990611     C  N50KTAM2         CHAIN(N)  TITGC000                           32
198800990611     C   50KTAM2         CHAIN(N)  TIOGC000                           32
198900950116     C  N32TGCATB        IFEQ      ' '
199000950116     C                   SETON                                        96
199100950116     C                   ENDIF
199200080610      * salvo l'indicatore di nuova tariffa/offerta
199300080610     c                   eval      win01 = *in01
199400950116     C*
199500031118     C     endinz        ENDSR
199600940503     C*
199700941013     C*--- CONTROLLO DATI FORMATO 1 ----------------------------------*
199800941012     C     CTRD01        BEGSR
199900940729     C                   SETOFF                                       27
200000940503     C*
200100940503     C****  DATA DECORRENZA TARIFFA  ****
200200940729     C                   Z-ADD     V1CDDT        G02DAT
200300940701     C                   MOVEL     *BLANKS       G02ERR
200400940701     C                   CALL      'XSRDA8'
200500940503     C                   PARM                    WLBDAT
200600940701    1C     G02ERR        IFEQ      '1'
200700940526     C                   MOVEL     MSG(1)        V1CMSG
200800940526     C                   SETON                                        4028
200900940503     C                   GOTO      ENDCTR
201000940503   X1C                   ELSE
201100940701     C                   MOVE      G02INV        COMDDT            8 0
201200940729     C*
201300940729     C* IMPOSTO A VIDEO LA DATA A 8 SE IMMESSA A 6
201400940729     C                   Z-ADD     G02DAT        V1CDDT
201500940503    1C                   ENDIF
201600940503     C*
201700940503     C****  DATA SCADENZA TARIFFA  ****
201800940729     C                   Z-ADD     V1CDST        G02DAT
201900940701     C                   MOVEL     *BLANKS       G02ERR
202000940701     C                   CALL      'XSRDA8'
202100940503     C                   PARM                    WLBDAT
202200940701    1C     G02ERR        IFEQ      '1'
202300940526     C                   MOVEL     MSG(2)        V1CMSG
202400940526     C                   SETON                                        4128
202500940503     C                   GOTO      ENDCTR
202600940503   X1C                   ELSE
202700940701     C                   MOVE      G02INV        COMDST            8 0
202800940729     C*
202900940729     C* IMPOSTO A VIDEO LA DATA A 8 SE IMMESSA A 6
203000940729     C                   Z-ADD     G02DAT        V1CDST
203100940503    1C                   ENDIF
203200940503     C*
203300940503     C* LA DATA SCADENZA DEVE ESSERE MAGGIORE DELLA DATA DECORRENZA
203400940503    1C     COMDST        IFLT      COMDDT
203500940526     C                   MOVEL     MSG(3)        V1CMSG
203600940526     C                   SETON                                        4128
203700940503     C                   GOTO      ENDCTR
203800940503    1C                   ENDIF
203900120124
204000120124      /free
204100120124       //?La data scadenza non deve essere superiore ad oggi + 5 anni
204200120124       IF  comdst > 0;
204300120124         wDate_ISO = %date(comdst:*iso);
204400120124         IF  %diff(wDate_ISO : wOggi : *years) >= 5;
204500120124           V1Cmsg = msg(08);
204600120124           *in28 = *on;
204700120124           *in41 = *on;
204800120124           leavesr;
204900120124         ENDIF;
205000120124       ENDIF;
205100120124      /end-free
205200120124
205300900327     C*
205400940503     C* VFLG <> '1'  --->  S O L O    P E R    T A R I F F E
205500940503     C*                    CONTROLLO CONGRUENZA DECORRENZE/SCADENZE
205600940503    1C     VFLG          IFNE      '1'
205700940503     C*
205800940503     C* LA DATA SCADENZA DEL PROGRESSIVO PRECEDENTE DEVE ESSERE MINORE
205900940503     C*   DELLA DATA DECORRENZA DEL PROGRESSIVO SEGUENTE
206000940526     C                   Z-ADD     V1CKSC        COMKSC
206100900620     C                   Z-ADD     VCTR          COMCTR
206200940503     C     PRG           ADD       1             COMPRG
206300940503     C*
206400940615     C     KTAM5         SETLL     TNTAM000
206500940615     C     KTAM5         READE(N)  TNTAM000                               32
206600940503     C*
206700940510    2C     *IN32         DOWEQ     *OFF
206800940509     C     TAMATB        IFEQ      ' '
206900940509     C     COMDST        IFGE      TAMDDT
207000940526     C                   MOVEL     MSG(4)        V1CMSG
207100940526     C                   SETON                                        4128
207200940503     C                   GOTO      ENDCTR
207300940503     C                   ELSE
207400940510     C                   SETON                                        32
207500940503     C                   ENDIF
207600940503     C*
207700940503     C                   ELSE
207800940503     C* RECORD ANNULLATO: AGGIUNGO 1 E CERCO IL PROGRESSIVO SEGUENTE
207900940503     C                   ADD       1             COMPRG
208000940503     C                   ENDIF
208100940503     C*
208200940615     C  N32KTAM5         READE(N)  TNTAM000                               32
208300940503    2C                   ENDDO
208400940503     C*
208500940503     C* LA DATA DECORRENZA DEL PROGRESSIVO SEGUENTE DEVE ESSERE
208600940503     C*   MAGGIORE DELLA DATA SCADENZA DEL PROGRESSIVO PRECEDENTE
208700940503     C     PRG           SUB       1             COMPRG
208800940615     C     KTAM5         SETLL     TNTAM000
208900940615     C     KTAM5         READE(N)  TNTAM000                               32
209000940503     C*
209100940510    2C     *IN32         DOWEQ     *OFF
209200940503     C     TAMATB        IFEQ      ' '
209300940503     C     COMDDT        IFLE      TAMDST
209400940526     C                   MOVEL     MSG(5)        V1CMSG
209500940526     C                   SETON                                        4028
209600940503     C                   GOTO      ENDCTR
209700940503     C                   ELSE
209800940506     C*
209900940509     C* LA DATA DECORRENZA PROGRESSIVO SEGUENTE DEVE ESSERE MAGGIORE
210000940509     C*   DI 1 GIORNO DELLA DATA SCADENZA PROGRESSIVO PRECEDENTE
210100940510     C                   EXSR      CTRDAT
210200940506     C*
210300940510     C     COMDDT        IFNE      DATA
210400940526     C                   MOVEL     MSG(5)        V1CMSG
210500940526     C                   SETON                                        4028
210600940506     C                   GOTO      ENDCTR
210700940510     C                   ENDIF
210800940506     C*
210900940510     C                   SETON                                        32
211000940503     C                   ENDIF
211100940503     C*
211200940503     C                   ELSE
211300940503     C* RECORD ANNULLATO: SOTTRAGGO 1 E CERCO IL PROGRESSIVO PRECEDENTE
211400940503     C                   SUB       1             COMPRG
211500940506     C* RIESEGUO POSIZIONAMENTO CON PROGRESSIVO PRECEDENTE
211600940615     C     KTAM5         SETLL     TNTAM000
211700940503     C                   ENDIF
211800940503     C*
211900940615     C  N32KTAM5         READE(N)  TNTAM000                               32
212000940503    2C                   ENDDO
212100940503     C****
212200940509    1C                   ENDIF
212300130318
212400130318      /free
212500130320       //?Manutenzione Tariffa no cartello
212600130320       IF  not *in50 and *in10 and not *in23;
212700130318       //?Data decorrenza
212800130318       //?se <= data ultima spedizione fatturata
212900130318       //?non posso modificarla
213000130318       IF  sav_TAMddt <= wDataSP and sav_TAMddt <> comddt;
213100130318         *in40 = *on;
213200130318         *in28 = *on;
213300130318         V1Cmsg = 'Data Decorezza non modificabile, minore '+
213400130318                  'data ultima sped.fatt. ' +
213500130318                  %subst(%editc(wDataSP:'X'):7:2) + '/' +
213600130318                  %subst(%editc(wDataSP:'X'):5:2) + '/' +
213700130318                  %subst(%editc(wDataSP:'X'):1:4);
213800130318         leavesr;
213900130318       ENDIF;
214000130318       //?se > data ultima spedizione fatturata
214100130318       //?posso impostarla solo > alla data ultima sp.fatturata
214200130318       IF  sav_TAMddt > wDataSP and comddt <> sav_TAMddt and
214300130318           comddt <= wDataSP;
214400130318         *in40 = *on;
214500130318         *in28 = *on;
214600130318         V1Cmsg = 'Data Decor. deve essere più '+
214700130318                  'alta della data ultima sped.fatt. ' +
214800130318                  %subst(%editc(wDataSP:'X'):7:2) + '/' +
214900130318                  %subst(%editc(wDataSP:'X'):5:2) + '/' +
215000130318                  %subst(%editc(wDataSP:'X'):1:4);
215100130318         leavesr;
215200130318       ENDIF;
215300130318
215400130318       //?Data scadenza
215500130318       //?se <= data ultima spedizione fatturata
215600130318       //?non posso metterla più bassa di quella che c'è
215700130318       //?già sul file
215800130318       IF  sav_TAMdst <= wDataSP and comdst <> sav_TAMdst and
215900130318           comdst < sav_TAMdst;
216000130318         *in41 = *on;
216100130318         *in28 = *on;
216200130318         V1Cmsg = 'Data scadenza <= data ultima sped.fatt. '+
216300130318                  %subst(%editc(wDataSP:'X'):7:2) + '/' +
216400130318                  %subst(%editc(wDataSP:'X'):5:2) + '/' +
216500130318                  %subst(%editc(wDataSP:'X'):1:4) + ' ' +
216600130318                  'non si può anticipare.';
216700130318         leavesr;
216800130318       ENDIF;
216900130318       //?se > data ultima spedizione fatturata
217000130318       //?posso impostarla solo >= alla data ultima sp.fatturata
217100130318       IF  sav_TAMdst > wDataSP and comdst <> sav_TAMdst and
217200130318           comdst < wDataSP;
217300130318         *in41 = *on;
217400130318         *in28 = *on;
217500130319         V1Cmsg = 'Data Scadenza deve essere = o più '+
217600130318                  'alta della data ultima sped.fatt. ' +
217700130318                  %subst(%editc(wDataSP:'X'):7:2) + '/' +
217800130318                  %subst(%editc(wDataSP:'X'):5:2) + '/' +
217900130318                  %subst(%editc(wDataSP:'X'):1:4);
218000130318         leavesr;
218100130318       ENDIF;
218200130320       ENDIF;
218300130318      /end-free
218400151029
218500151029     C****  CODICE DIVISA    ****
218600151029     C                   MOVEL     'CV'          COD
218700151029     C                   MOVEL     *BLANKS       KEY
218800151029     C                   MOVEL     V1CDIV        KEY
218900151029     C     KTAB          CHAIN     TABEL                              32
219000151029    1C  N32TBLFLG        IFEQ      ' '
219100151029     C                   MOVEL     TBLUNI        DSCV
219200151029    1C                   ENDIF
219300900327     C*
219400941010     C****  FLAG ITALIA/ESTERO  ****
219500941028     C* PER LE TARIFFE CLIENTI     DEVE ESSERE OBBLIGATORIAMENTE PIENO
219600941028     C* PER LE TARIFFE DI CARTELLO DEVE ESSERE OBBLIGATORIAMENTE VUOTO
219700941028    1C     *IN23         IFEQ      *OFF
219800941028     C* 23 OFF - TARIFFE CLIENTI
219900941028    2C     V1CFIE        IFEQ      ' '
220000941010     C                   MOVEL     MSG(50)       V1CMSG
220100941010     C                   SETON                                        8728
220200941010     C                   GOTO      ENDCTR
220300941028    2C                   ENDIF
220400941028     C*
220500941107     C* CONTROLLO VALIDITA' DEL FLAG ITALIA/ESTERO
220600941107    2C     V1CFIE        IFNE      'I'
220700941109     C     V1CFIE        ANDNE     'E'
220800941107     C                   SETON                                        8728
220900941107     C                   MOVEL     MSG(51)       V1CMSG
221000941107     C                   GOTO      ENDCTR
221100941107    2C                   ENDIF
221200941107     C*
221300941028   X1C                   ELSE
221400941028     C* 23 ON  - TARIFFE DI CARTELLO
221500941028    1C                   ENDIF
221600941028     C*
221700020205     C* IN VARIAZIONE(10 ON) NON E' POSSIBILE VARIARE IL FLAG ITALIA/ESTERO
221800020213     C* SE DIVERSO DA BLANK
221900020213     C*
222000941108    1C   10V1CFIE        IFNE      WFIE
222100020213     C     WFIE          ANDNE     ' '
222200941107     C                   MOVEL     MSG(53)       V1CMSG
222300941012     C                   SETON                                        8728
222400941012     C                   GOTO      ENDCTR
222500941028    1C                   ENDIF
222600160218
222700941010     C*
222800940504     C****  FLAG ISTAT  ****
222900031118     C* SE E' STATO INSERITO IL FLAG ISTAT OCCORRE INSERIRE ANCHE
223000031118     C*   LA % ADDEBITO ISTAT E VICEVERSA
223100101013    1C     V1Crct        IFGT      0
223200031118     C     V1CPRI        IFEQ      0
223300031118     C                   MOVEL     MSG(6)        V1CMSG
223400031118     C                   SETON                                        4228
223500031118     C                   GOTO      ENDCTR
223600031118     C                   ENDIF
223700031119      * se non esiste lo scaglione (data scatto uguale a zero) do errore
223800031119      *
223900101013     c                   if        dia(v1crct) = 0
224000031119     c                   movel     msg(113)      v1cmsg
224100031119     c                   seton                                        4328
224200031119     c                   goto      endctr
224300031119     c                   endif
224400131029      *
224500131029     c                   if        savpri <> v1cpri
224600131113     c*                  clear                   forzamsgistat
224700131029     c                   endif
224800131113      * se sono in immisisone imposto tampri uguale a zero
224900131113     c                   If        *in01
225000131113     c                   clear                   tampri
225100131113     c                   endif
225200131031      *
225300131028      * la percentuale di addebito non deve essere > di 100 o < di 50
225400131028      * se
225500131028     c                   If        v1cpri > 100 OR V1CPRI < 50
225600131029      *
225700131028      * se sono in gestione OFFERTA su trattativa NUOVA quindi senza cliente
225800131028      * ERRORE NON FORZABILE
225900131029     c                   if        (*in50 and ksc2 = *zeros)
226000050302     c                   Eval      v1cmsg = msg(114)
226100050303     c                   Eval      *In42 = *On
226200050302     c                   Eval      *In28 = *On
226300050302     c                   Goto      endctr
226400050302     c                   EndIf
226500131028      * se sto gestendo un'offerta legata ad una trattativa su cliente
226600131028      * il messaggio  sulla % applicazione ISTAT diventa forzabile
226700131113      * se si mantiene la stessa % del file ma se si modifca dare errore
226800131113      * bloccante se non soddisfa >=50 e <= 100
226900131029      *                  oppure
227000131029      *
227100131113      * se sto aggiungendo un nuovo progressivo la 1° volta è un messaggio
227200131113      * informativo
227300131029     c                   if        ((*in50 and ksc2 > *zeros) or
227400131113     c                              not *in17 )
227500131113
227600131113     c                   if        savpri <> v1cpri and
227700131113     c                             tampri = v1cpri and
227800131113     c                             forzaMsgIstat = *blanks
227900131028     c                   Eval      v1cmsg = msg(122)
228000131028     c                   Eval      *In42 = *On
228100131028     c                   Eval      *In28 = *On
228200131028     c                   Eval      forzaMsgIstat = '1'
228300131028     c                   eval      savpri = v1cpri
228400131028     c                   Goto      endctr
228500131113     c                   endif
228600131113      *
228700131113     c                   If        v1cpri <> tampri
228800131113     c                   Eval      v1cmsg = msg(114)
228900131113     c                   Eval      *In42 = *On
229000131113     c                   Eval      *In28 = *On
229100131113     c                   Goto      endctr
229200131113     c                   EndIf
229300131113      *
229400131028     c                   EndIf
229500031119      *
229600131029     c                   EndIf
229700131029      *
229800031118      * visualizzo la decorrenza
229900031118      *
230000101013     c                   z-add     dia(v1crct)   g02INV
230100031118     c                   movel     '3'           g02err
230200031118     c                   CALL      'XSRDA8'
230300031118     c                   PARM                    WLBDAT
230400031118      * UDATE A 8 IN GG/MM/AAAA
230500031118     c                   z-add     g02dat        v1diST
230600031119      * se cambiato scaglione istat riemetto la videata per visualizzare
230700031119      * la data decorrenza
230800101013     C                   IF        V1Crct <> W_V1Crct
230900031119     C                   SETON                                        27
231000101013     c                   eval      W_v1crct = v1crct
231100031119     C                   ENDIF
231200031118     C*
231300031118   X1C                   ELSE
231400031118     C     V1CPRI        IFGT      0
231500031118     C                   MOVEL     MSG(7)        V1CMSG
231600031118     C                   SETON                                        4328
231700031118     C                   GOTO      ENDCTR
231800031118     C                   ENDIF
231900031118      *
232000031118     c                   clear                   v1dist
232100031118    1C                   ENDIF
232200031118
232300940503     C*
232400940504     C****  CODICE TARIFFA  ****
232500900621     C                   Z-ADD     1             A
232600940526     C                   MOVEL     V1CCTR        UNOCTR            1 0
232700021118     c                   Move      V1cCtr        DueCtr            2 0
232800940524     C     UNOCTR        LOOKUP    CTR(A)                                 35
232900940524    1C     *IN35         IFEQ      *OFF
233000940526     C                   MOVEL     MSG(9)        V1CMSG
233100000118     C                   SETON                                        28
233200940504     C                   GOTO      ENDCTR
233300940509    1C                   ENDIF
233400021118
233500940504     C* DESCRIZIONE CODICE TARIFFA
233600940526     C                   MOVEL     DTR(A)        V1DCTR
233700940504     C*
233800940504     C****  VOLUME AUTOMATICO  ****
233900940504     C* 50 ON  -  S O L O    P E R    O F F E R T E
234000940504     C* SE SI TRATTA DI CLIENTE NUOVO, VOLUME AUTOMATICO OBBLIGATORIO
234100940509    1C   50KSC2          IFLE      *ZEROS
234200940526     C     V1CKPM        ANDEQ     0
234300940526     C     V1CMPC        ANDEQ     0
234400940526     C                   MOVEL     MSG(10)       V1CMSG
234500940526     C                   SETON                                        4628
234600940504     C                   GOTO      ENDCTR
234700940509    1C                   ENDIF
234800940509     C*
234900940504     C* NON SI POSSONO INDICARE SIA I KG CHE I COLLI PER VOL.AUTOMATICO
235000940526    1C     V1CKPM        IFNE      0
235100940526     C     V1CMPC        ANDNE     0
235200940526     C                   MOVEL     MSG(11)       V1CMSG
235300940526     C                   SETON                                        4628
235400940504     C                   GOTO      ENDCTR
235500940509    1C                   ENDIF
235600090706      **** UTILIZZO PESO VDL ****
235700090706      * non posso immettere il campo utilizzo peso VDL se flag rapporto
235800090706      * peso/volume su tipo tariffa è blank
235900090706     c                   if        wrpv = *blanks and v1ctap <> *blanks
236000090706     c                   eval      v1cmsg = msg(14)
236100090706     c                   eval      *in28 = *on
236200090706     c                   eval      *in60 = *on
236300090706     c                   goto      endctr
236400090706     c                   endif
236500910423     C*
236600940504     C****  APPLICAZIONE TARIFFA FINITA  ****
236700941124    1C     V1CATA        IFNE      0
236800950112     C* WSAP = " " --> SI TRATTA DI SCAGLIONE SENZA DECIMALI PER CUI
236900950112     C*   NON SI POSSONO INSERIRE VALORI DOPO LA VIRGOLA
237000950112    2C     WSAP          IFEQ      ' '
237100941125     C                   MOVE      V1CATA        W0030
237200941124     C     W0030         IFGT      0
237300941124     C                   SETON                                        4728
237400950119     C                   MOVEL     MSG(57)       V1CMSG
237500941124     C                   GOTO      ENDCTR
237600941124     C                   ENDIF
237700950112    2C                   ENDIF
237800941124     C*
237900940504     C* CONTROLLO CHE L'APPLICAZIONE TARIFFA FINITA NON SIA INFERIORE
238000940504     C*   AL MINIMO TASSABILE INDICATO IN TABELLA
238100941124    2C     V1CATA        IFLT      MIN(A)
238200940526     C                   MOVEL     MSG(12)       V1CMSG
238300940526     C                   SETON                                        4728
238400940504     C                   GOTO      ENDCTR
238500941124    2C                   ENDIF
238600940509    1C                   ENDIF
238700940504     C*
238800940504     C****  ARROTONDAMENTI  ****
238900980505     C* CONTROLLO SE QUESTA TARIFFA GESTISCE GLI ARROTONDAMENTI
239000980505    1C     ARR(A)        IFEQ      ' '
239100980505    2C     V1CARL        IFGT      0
239200980505     C     V1CARF        ORGT      0
239300980505     C     V1CARO        ORGT      0
239400980505     C                   MOVEL     MSG(74)       V1CMSG
239500980505     C                   SETON                                        4828
239600980505     C                   GOTO      ENDCTR
239700980505    2C                   ENDIF
239800980505    1C                   ENDIF
239900940504     C* DEVONO ESSERE O TUTTI <> 0  OPPURE  TUTTI = 0
240000940712    1C     V1CARL        IFEQ      0
240100940504     C*
240200940712     C     V1CARF        IFGT      0
240300940526     C     V1CARO        ORGT      0
240400940526     C                   MOVEL     MSG(13)       V1CMSG
240500940526     C                   SETON                                        4828
240600940504     C                   GOTO      ENDCTR
240700940504     C                   ENDIF
240800940504     C*
240900940504   X1C                   ELSE
241000940504     C*
241100940712     C     V1CARF        IFEQ      0
241200940526     C     V1CARO        OREQ      0
241300940526     C                   MOVEL     MSG(13)       V1CMSG
241400940526     C                   SETON                                        4828
241500940504     C                   GOTO      ENDCTR
241600940504     C                   ENDIF
241700940504     C*
241800940504    1C                   ENDIF
241900940504     C*
242000941125     C* SE SONO STATI IMMESSI GLI ARROTONDAMENTI CONTROLLO SE SI TRATTA
242100950112     C*   DI SCAGLIONE SENZA DECIMALI PER CUI NON SI POSSONO INSERIRE
242200950112     C*   VALORI DOPO LA VIRGOLA
242300941125    1C     V1CARL        IFGT      0
242400941125     C     WSAP          ANDEQ     ' '
242500941125     C                   MOVE      V1CARL        W0030
242600941125    2C     W0030         IFGT      0
242700941125     C                   SETON                                        4828
242800950119     C                   MOVEL     MSG(56)       V1CMSG
242900941125     C                   GOTO      ENDCTR
243000941125    2C                   ENDIF
243100941125     C*
243200941125     C                   MOVE      V1CARF        W0030
243300941125    2C     W0030         IFGT      0
243400941125     C                   SETON                                        4828
243500950119     C                   MOVEL     MSG(56)       V1CMSG
243600941125     C                   GOTO      ENDCTR
243700941125    2C                   ENDIF
243800941125     C*
243900941125     C                   MOVE      V1CARO        W0030
244000941125    2C     W0030         IFGT      0
244100941125     C                   SETON                                        4828
244200950119     C                   MOVEL     MSG(56)       V1CMSG
244300941125     C                   GOTO      ENDCTR
244400941125    2C                   ENDIF
244500941125    1C                   ENDIF
244600990616     C*
244700990616     C* CONTROLLO SE INSERITO MINIMO MASSIMO SPEDIZIONI AFFIDATECI DAL CLIENTE
244800990616     C     V1CMIS        IFNE      *ZEROS
244900990616     C     V1CMAS        ANDGT     0
245000990616     C     V1CMIS        ANDGT     V1CMAS
245100990618     C                   SETON                                        5128
245200990617     C                   MOVEL     MSG(78)       V1CMSG
245300990616     C                   GOTO      ENDCTR
245400990616     C                   ENDIF
245500940503     C*
245600940707     C****  TIPO SPEDIZIONE  ****
245700950112    1C     V1CTSP        IFEQ      '?'
245800940707     C                   MOVEL     '5E'          COD
245900940707     C                   MOVEL     ' '           V1CTSP
246000020201     C                   MOVEL     *BLANKS       KEY
246100960527     C                   MOVEL     *BLANKS       DES              25
246200940707     C                   CALL      'X§TABER'
246300940707     C                   PARM                    CODUT
246400940707     C                   PARM                    COD
246500940707     C                   PARM                    KEY
246600940707     C                   PARM                    DES
246700940707     C                   MOVEL     KEY           V1CTSP
246800090608     C**!!!              MOVEL     DES           V1DTSP
246900940729     C                   SETON                                        27
247000090608     C**!!!              GOTO      ENDCTR
247100950112    1C                   ENDIF
247200020215     C*
247300940711     C* TIPO SPEDIZIONE OBBLIGATORIO
247400940707     C                   MOVEL     '5E'          COD
247500940707     C                   MOVEL     *BLANKS       KEY
247600940707     C                   MOVEL     V1CTSP        KEY
247700940707     C     KTAB          CHAIN     TABEL                              32
247800950112    1C  N32TBLFLG        IFNE      ' '
247900940707     C                   SETON                                        32
248000950112    1C                   ENDIF
248100940707     C* TIPO SPEDIZIONE ERRATO
248200950112    1C     *IN32         IFEQ      *ON
248300940707     C                   MOVEL     MSG(42)       V1CMSG
248400940707     C                   SETON                                        5428
248500940707     C                   GOTO      ENDCTR
248600950112    1C                   ENDIF
248700000420     C*
248800000420     C                   MOVEL     TBLUNI        DS5E
248900090915     C* CONTROLLO SE tipo servizio utilizzabile
249000090915     C***  *IN50         IFEQ      *OFF
249100090915     c*
249200090915     c                   if        §5euso='P'   or §5efum<>'S'
249300090915    1C***  §5EUSO        IFEQ      'P'
249400090915     C***  WRKPT         ANDNE     'S'
249500090915     C                   MOVEL     MSG(91)       V1CMSG
249600090915     C                   SETON                                        5428
249700090915     C                   GOTO      ENDCTR
249800090915    1C                   ENDIF
249900000420     C*
250000000420     C* VERIFICO SE SERVIZIO NON POSTA CON CLIENTE NON POSTA
250100090915    1C***  §5EUSO        IFEQ      'E'
250200090915     C***  WRKPT         ANDEQ     'S'
250300090915     C**+                MOVEL     MSG(92)       V1CMSG
250400090915     C***                SETON                                        5428
250500090915     C***                GOTO      ENDCTR
250600090915    1C***                ENDIF
250700090915     C***                ENDIF
250800940707     C*
250900940707     C* DECODIFICA TIPO SPEDIZIONE
251000090608     C                   MOVEL     §5ed08        V1DTSP
251100970401     C* NON POSSO INSERIRE TIPO SERVIZIO ESPRESSO SE TARIFFA ESTERA
251200970401     C     V1CTSP        IFEQ      'E'
251300970401     C     V1CFIE        ANDEQ     'E'
251400970401     C                   MOVEL     MSG(70)       V1CMSG
251500090608     c                   eval      %subst(v1cmsg:34:8) = §5ed08
251600970401     C                   SETON                                        5428
251700970401     C                   GOTO      ENDCTR
251800970401    1C                   ENDIF
251900001127     C* SE E' OFFERTA ED IL TIPO SERVIZIO E' UN TIPO SERVIZIO UTILIZZATO DA
252000001127     C* CLIENTI POSTA VALORIZZO IL WRKPT = A 'S'
252100000426     C     *IN50         IFEQ      *ON
252200000426     C     §5EUSO        ANDEQ     'P'
252300000426     C                   MOVEL     'S'           WRKPT
252400000426     C                   ENDIF
252500130514      /free
252600130514       //?Se manutenzione tariffa posso variare il tipo servizio solo da
252700130514       //?"C" a "D" o viceversa, se "E" non posso variare e non posso impostarla
252800130514         IF  not *in50 and *in10 and not *in23;
252900130604           IF  sav_TAMtsp = 'E' and V1Ctsp <> sav_TAMtsp;
253000130514             V1Cmsg = msg(90);
253100130514             *in28 = *on;
253200130514             *in54 = *on;
253300130514             leavesr;
253400130514           ENDIF;
253500130604           IF  (sav_TAMtsp = 'C' or sav_TAMtsp = 'D') and
253600130514                V1Ctsp <> 'C' and V1Ctsp <> 'D';
253700130514             V1Cmsg = msg(90);
253800130514             *in28 = *on;
253900130514             *in54 = *on;
254000130514             leavesr;
254100130514           ENDIF;
254200130514         ENDIF;
254300130514      /end-free
254400940706     C*
254500941010     C****  FLAG TARIFFA PREFERENZIALE  ****
254600940929     C* NON E' POSSIBILE SELEZIONARE UNA TARIFFA PREFERENZIALE SU
254700940929     C*   CODICI TARIFFA DIVERSI APPARTENENTI ALLO STESSO CLIENTE
254800941011     C*   A MENO CHE NON SI TRATTI DI TARIFFA PARTENZE E TARIFFA ARRIVI
254900941011     C*   O DI TARIFFA ITALIA O TARIFFA ESTERO
255000941109    1C     V1CFTP        IFEQ      'P'
255100061011     C  n50V1CKSC        SETLL     TNTAM000
255200061011     c   50v1cksc        setll     tnofm000
255300061011     C  n50V1CKSC        READE(N)  TNTAM000                               32
255400061011     c   50v1cksc        reade(n)  tnofm000                               32
255500940929     C*
255600941109    2C     *IN32         DOWEQ     *OFF
255700941109    3C     TAMATB        IFEQ      ' '
255800940929     C*
255900941109    4C     TAMCTR        IFNE      V1CCTR
256000940929     C     TAMFTP        ANDEQ     'P'
256100941011     C*
256200941109    5C     TAMFIE        IFEQ      V1CFIE
256300941011     C*
256400941109    6C     TAMBAP        IFEQ      'P'
256500941011     C     V1CBAP        ANDEQ     'P'
256600950105     C     TAMBAP        OREQ      'P'
256700950105     C     V1CBAP        ANDEQ     ' '
256800950105     C     TAMBAP        OREQ      ' '
256900950105     C     V1CBAP        ANDEQ     'P'
257000950105     C*
257100941011     C     TAMBAP        OREQ      'A'
257200941011     C     V1CBAP        ANDEQ     'A'
257300950105     C     TAMBAP        OREQ      'A'
257400950105     C     V1CBAP        ANDEQ     ' '
257500950105     C     TAMBAP        OREQ      ' '
257600950105     C     V1CBAP        ANDEQ     'A'
257700940929     C                   MOVEL     MSG(49)       V1CMSG
257800940929     C                   SETON                                        8628
257900940929     C                   GOTO      ENDCTR
258000941109    6C                   ENDIF
258100941109    5C                   ENDIF
258200941109    4C                   ENDIF
258300940929     C*
258400941109    3C                   ENDIF
258500061011     C  n50V1CKSC        READE(N)  TNTAM000                               32
258600061011     c   50v1cksc        reade(n)  tnofm000                               32
258700941109    2C                   ENDDO
258800941109    1C                   ENDIF
258900020205      *
259000020205      * Se non immesso Network non posso inserirlo
259100020205     C  N01V1CNET        IFNE      ' '
259200020205     C     §TADPD        ANDEQ     ' '
259300020205     C     §TAFED        ANDEQ     ' '
259400020205     C                   MOVEL     MSG(99)       V1CMSG
259500020205     C                   SETON                                        4528
259600020205     C                   GOTO      ENDCTR
259700020205    6C                   ENDIF
259800000118     C*
259900020130     C* SE IMMESSO SERVIZIO DPD NON POSSO PIU' TOGLIERLO
260000020201     C  N01V1CNET        IFEQ      ' '
260100000118     C     §TADPD        ANDEQ     'S'
260200000118     C                   MOVEL     MSG(86)       V1CMSG
260300000118     C                   SETON                                        4528
260400000118     C                   GOTO      ENDCTR
260500000118    6C                   ENDIF
260600020130      *
260700020205      * Se immesso Network FedEx non posso più toglierlo
260800020201     C  N01V1CNET        IFEQ      ' '
260900020130     C     §TAFED        ANDEQ     'S'
261000020130     C                   MOVEL     MSG(97)       V1CMSG
261100020130     C                   SETON                                        4528
261200020130     C                   GOTO      ENDCTR
261300020130     C                   ENDIF
261400000118     C* una tariffa dpd non puo' essere estera
261500020201     C     V1CNET        IFEQ      'D'
261600000118     C     V1CFIE        ANDEQ     'E'
261700000118     C                   MOVEL     MSG(87)       V1CMSG
261800000118     C                   SETON                                        4528
261900000118     C                   GOTO      ENDCTR
262000000118    6C                   ENDIF
262100020130      * una tariffa FedEx deve essere estera
262200020201     C     V1CNET        IFEQ      'F'
262300020130     C     V1CFIE        ANDNE     'E'
262400020130     C                   MOVEL     MSG(98)       V1CMSG
262500020130     C                   SETON                                        4528
262600020130     C                   GOTO      ENDCTR
262700020130    6C                   ENDIF
262800020130     C* una tariffa dpd o FedEx deve essere corriere
262900020201     C     V1CNET        IFNE      ' '
263000000121     C     V1CTSP        ANDNE     'C'
263100090608     c                   clear                   ds5e
263200090608     c                   eval      cod = '5E'
263300090608     c                   eval      key = 'C'
263400090608     c     ktab          chain     tabel00f
263500090608     c                   if        %found(tabel00f)
263600090608     c                   eval      ds5e = tbluni
263700090608     c                   endif
263800000121     C                   MOVEL     MSG(88)       V1CMSG
263900090608     c                   eval      %subst(v1cmsg:48:8) = §5ed08
264000000121     C                   SETON                                        4528
264100000121     C                   GOTO      ENDCTR
264200000121    6C                   ENDIF
264300000218     C* una tariffa DPD a kg. deve avere applicazione tariffa finita fino a 31,5
264400000223     C* CHE TROVO NELLA TABELLA 3I
264500020201     C     V1CNET        IFEQ      'D'
264600000218     C     UNOCTR        ANDEQ     3
264700000223     C     V1CATA        ANDNE     §3IPKD
264800000223     C                   Z-ADD     §3IPKD        V1CATA
264900160219     C*****              GOTO      ENDCTR
265000000218    6C                   ENDIF
265100021118      * Controllo per tariffa FedEx se ok codice tariffa inserito
265200021118     c                   If        V1cnet = 'F' and Rutf(a) <> 'S'
265300021118     c                   Eval      V1cMsg = Msg(111)
265400021118     c                   Eval      *In45 = *On
265500021118     c                   Eval      *In28 = *On
265600021118     c                   GoTo      EndCtr
265700021118     c                   EndIf
265800160218
265900160218      /free
266000160218       //?Se inserimento Offerta di trattativa su cliente
266100160218       //?devo controllare la tipologia dell'offerta con eventuale
266200160218       //?altra offerta già esistente.
266300160218         IF  not *in10 and *in50 and ksc2 > *zeros;
266400160218           clear TNTAT1DS;
266500160218           ITAT1ntc  = V1Cksc;
266600160218           ITAT1ksc  = %dec(ksc2:7:0);
266700160218           ITAT1ctr  = vctr;
266800160218           SELECT;
266900160218           WHEN  V1Cnet = 'D';
267000160218             ITAT1ntw = 'DPD';
267100160218           WHEN  V1Cnet = 'F';
267200160218             ITAT1ntw = 'FED';
267300160218           WHEN  V1Cfie = 'E';
267400160218             ITAT1ntw = 'EEX';
267500160218           OTHER;
267600160219             clear ITAT1ntw;
267700160218           ENDSL;
267800160218           kpjbu = TNTAT1DS;
267900160218           tntat1r (kpjba);
268000160218           TNTAT1DS = kpjbu;
268100160218           IF  OTAT1err = *on;
268200160218             *in28  = *on;
268300160218             *in87  = *on;
268400160218             V1Cmsg = OTAT1msg;
268500160218             leavesr;
268600160218           endif;
268700160218         ENDIF;
268800160218      /end-free
268900940929     C*
269000940503     C     ENDCTR        ENDSR
269100940503     C*
269200941013     C*--- CONTROLLO DATI FORMATO 2 ----------------------------------*
269300941012     C     CTRD02        BEGSR
269400020312     C                   SETOFF                                       2782
269500940729     C*
269600940811     C****  NATURA MERCE  ****
269700940811     C* NATURA MERCE OBBLIGATORIA
269800940811     C     V2CNAS        IFEQ      *BLANKS
269900940811     C                   MOVEL     MSG(43)       V1CMSG
270000940811     C                   SETON                                        8228
270100940811     C                   GOTO      ENDCT
270200940811     C                   ENDIF
270300060303
270400060303      * Rinuncia AC BASE
270500060303      * se imposto che rinuncio non ci deve essere la tariffa particolare "d"
270600060303     c                   if        v2cfmp <> *blanks
270700060303     c                   eval      wtpar = 'd'
270800070129     c  n50ktam7         chain(n)  titpt01l
270900070129     c   50ktam7         chain(n)  tiopt01l
271000061011     c                   If        %found and tptatb = *blanks
271100060303     c                   eval      v1cmsg = msg(119)
271200060303     c                   eval      *in28 = *on
271300060303     c                   move      rimm          at2fmp
271400060303     c                   Eval      h2riga = 10
271500060303     c                   Eval      h2colo = 19
271600060303     c                   leavesr
271700060303     c                   endif
271800060303     c                   endif
271900021118
272000940811     C*
272100940811     C****  FLAG VALORE MERCE DICHIARATO  ****
272200940811     C     *IN16         IFEQ      *ON
272300940811     C     V2CFVD        ANDEQ     ' '
272400940811     C                   MOVEL     MSG(44)       V1CMSG
272500090916     C                   SETON                                          28
272600090916     c                   Eval      h2riga = 13
272700090916     c                   Eval      h2colo = 36
272800940811     C                   GOTO      ENDCT
272900940811     C                   ENDIF
273000940811     C*
273100940729     C****  PARTICOLARITA' GESTIONE C/ASSEGNO  ****
273200941010    1C     V2CGCA        IFNE      *BLANKS
273300941010     C     V2CGCR        ORNE      *BLANKS
273400941010     C*
273500941010     C* RICHIAMO IL PROGRAMMA DI RICERCA
273600941010    2C     V2CGCR        IFEQ      '?'
273700941010     C                   CLEAR                   DS7PQR
273800941010     C                   MOVEL     'S'           DS7RIC
273900941010     C                   MOVEL     'V'           DS7GES
274000050428     C  n50              MOVEL     V1CKSC        DS7KSC
274100050428     C   50              MOVEL     ksc2          DS7KSC
274200941010     C                   MOVEL     V2CGCA        DS7COD
274300941010     C                   MOVEL     DS7PQR        KPJBU
274400941010     C                   CALL      'TRTB72R'
274500941010     C                   PARM                    KPJBA
274600941010     C                   MOVEL     KPJBU         DS7PQR
274700941010     C*
274800941010     C     V2CGCA        IFEQ      *BLANKS
274900941010     C                   MOVEL     DS7COD        V2CGCA
275000941010     C                   ENDIF
275100941010     C*
275200960109     C                   SETON                                        2777
275300941010     C                   GOTO      ENDCT
275400941010    2C                   ENDIF
275500941010     C*
275600941010     C* CONTROLLO
275700941010     C                   CLEAR                   DS7PQR
275800941010     C                   MOVEL     'S'           DS7RIC
275900941010     C                   MOVEL     'C'           DS7GES
276000050323     C  n50              MOVEL     V1CKSC        DS7KSC
276100050323     C   50              MOVEL     ksc2          DS7KSC
276200941010     C                   MOVEL     V2CGCA        DS7COD
276300941010     C                   MOVEL     DS7PQR        KPJBU
276400941010     C                   CALL      'TRTB72R'
276500941010     C                   PARM                    KPJBA
276600941010     C                   MOVEL     KPJBU         DS7PQR
276700941010     C* ERRORE
276800941010    2C     DS7ERR        IFEQ      '1'
276900941010     C                   MOVEL     DS7MSG        V1CMSG
277000941010     C                   SETON                                        7728
277100941010     C                   GOTO      ENDCT
277200941010    2C                   ENDIF
277300941010    1C                   ENDIF
277400941010     C*
277500940729     C****  PARTICOLARITA' GESTIONE GIACENZA  ****
277600941010    1C     V2CGGA        IFNE      *BLANKS
277700941010     C     V2CGGR        ORNE      *BLANKS
277800941010     C*
277900941010     C* RICHIAMO IL PROGRAMMA DI RICERCA
278000941010    2C     V2CGGR        IFEQ      '?'
278100941010     C                   CLEAR                   DS7PQR
278200941010     C                   MOVEL     'S'           DS7RIC
278300941010     C                   MOVEL     'V'           DS7GES
278400050428     ***C  n50              MOVEL     V1CKSC        DS7KSC
278500050428     ***C   50              MOVEL     ksc2          DS7KSC
278600941010     C                   MOVEL     V2CGGA        DS7COD
278700941010     C                   MOVEL     DS7PQR        KPJBU
278800941010     C                   CALL      'TRTB69R'
278900941010     C                   PARM                    KPJBA
279000941010     C                   MOVEL     KPJBU         DS7PQR
279100941010     C*
279200941010     C     V2CGGA        IFEQ      *BLANKS
279300941010     C                   MOVEL     DS7COD        V2CGGA
279400941010     C                   ENDIF
279500941010     C*
279600960109     C                   SETON                                        2778
279700941010     C                   GOTO      ENDCT
279800941010    2C                   ENDIF
279900941010     C*
280000941010     C* CONTROLLO
280100941010     C                   CLEAR                   DS7PQR
280200941010     C                   MOVEL     'S'           DS7RIC
280300941010     C                   MOVEL     'C'           DS7GES
280400050323     C  n50              MOVEL     V1CKSC        DS7KSC
280500050323     C   50              MOVEL     ksc2          DS7KSC
280600941010     C                   MOVEL     V2CGGA        DS7COD
280700960109     C                   MOVEL     DS7PQR        KPJBU
280800941010     C                   CALL      'TRTB69R'
280900941010     C                   PARM                    KPJBA
281000941010     C                   MOVEL     KPJBU         DS7PQR
281100941010     C* ERRORE
281200941010    2C     DS7ERR        IFEQ      '1'
281300941010     C                   MOVEL     DS7MSG        V1CMSG
281400941010     C                   SETON                                        7828
281500941010     C                   GOTO      ENDCT
281600941010    2C                   ENDIF
281700941010    1C                   ENDIF
281800941010     C*
281900941010     C****  PARTICOLARITA' GESTIONE CONSEGNA  ****
282000941010    1C     V2CGMA        IFNE      *BLANKS
282100941010     C     V2CGMR        ORNE      *BLANKS
282200941010     C*
282300941010     C* RICHIAMO IL PROGRAMMA DI RICERCA
282400941010    2C     V2CGMR        IFEQ      '?'
282500941010     C                   MOVEL     'S'           DS7RIC
282600941010     C                   MOVEL     'V'           DS7GES
282700050428     ***C  n50              MOVEL     V1CKSC        DS7KSC
282800050428     ***C   50              MOVEL     ksc2          DS7KSC
282900050606     c                   Clear                   ds7ksc
283000941012     C                   MOVEL     V2CGMA        DS7COD
283100960109     C                   MOVEL     DS7PQR        KPJBU
283200941010     C                   CALL      'TRTB70R'
283300941010     C                   PARM                    KPJBA
283400941010     C                   MOVEL     KPJBU         DS7PQR
283500941010     C*
283600941010     C     V2CGMA        IFEQ      *BLANKS
283700941010     C                   MOVEL     DS7COD        V2CGMA
283800941010     C                   ENDIF
283900941010     C*
284000960109     C                   SETON                                        2779
284100941010     C                   GOTO      ENDCT
284200941010    2C                   ENDIF
284300941010     C*
284400941010     C* CONTROLLO
284500941010     C                   CLEAR                   DS7PQR
284600941010     C                   MOVEL     'S'           DS7RIC
284700941010     C                   MOVEL     'C'           DS7GES
284800050323     C  n50              MOVEL     V1CKSC        DS7KSC
284900050323     C   50              MOVEL     ksc2          DS7KSC
285000941010     C                   MOVEL     V2CGMA        DS7COD
285100941010     C                   MOVEL     DS7PQR        KPJBU
285200941010     C                   CALL      'TRTB70R'
285300941010     C                   PARM                    KPJBA
285400941010     C                   MOVEL     KPJBU         DS7PQR
285500941010     C* ERRORE
285600941010    2C     DS7ERR        IFEQ      '1'
285700941010     C                   MOVEL     DS7MSG        V1CMSG
285800941010     C                   SETON                                        7928
285900941010     C                   GOTO      ENDCT
286000941010    2C                   ENDIF
286100941010    1C                   ENDIF
286200941010     C*
286300941010     C****  PARTICOLARITA' GESTIONI VARIE  ****
286400941010    1C     V2CGVA        IFNE      *BLANKS
286500941010     C     V2CGVR        ORNE      *BLANKS
286600941010     C*
286700941010     C* RICHIAMO IL PROGRAMMA DI RICERCA
286800941010    2C     V2CGVR        IFEQ      '?'
286900941010     C                   CLEAR                   DS7PQR
287000941010     C                   MOVEL     'S'           DS7RIC
287100941010     C                   MOVEL     'V'           DS7GES
287200050323     C  n50              MOVEL     V1CKSC        DS7KSC
287300050323     C   50              MOVEL     ksc2          DS7KSC
287400941010     C                   MOVEL     V2CGVA        DS7COD
287500941010     C                   MOVEL     DS7PQR        KPJBU
287600941010     C                   CALL      'TRTB73R'
287700941010     C                   PARM                    KPJBA
287800941010     C                   MOVEL     KPJBU         DS7PQR
287900941010     C*
288000941010     C     V2CGVA        IFEQ      *BLANKS
288100941010     C                   MOVEL     DS7COD        V2CGVA
288200941010     C                   ENDIF
288300941012     C*
288400960109     C                   SETON                                        2780
288500941012     C                   GOTO      ENDCT
288600941012    2C                   ENDIF
288700941010     C*
288800941010     C* CONTROLLO
288900941010     C                   CLEAR                   DS7PQR
289000941010     C                   MOVEL     'S'           DS7RIC
289100941010     C                   MOVEL     'C'           DS7GES
289200050323     C  n50              MOVEL     V1CKSC        DS7KSC
289300050323     C   50              MOVEL     ksc2          DS7KSC
289400941010     C                   MOVEL     V2CGVA        DS7COD
289500941010     C                   MOVEL     DS7PQR        KPJBU
289600960209     C                   CALL      'TRTB73R'
289700960209     C                   PARM                    KPJBA
289800941010     C                   MOVEL     KPJBU         DS7PQR
289900941010     C* ERRORE
290000941010     C     DS7ERR        IFEQ      '1'
290100941010     C                   MOVEL     DS7MSG        V1CMSG
290200941010     C                   SETON                                        8028
290300941010     C                   GOTO      ENDCT
290400941010     C                   ENDIF
290500941012    1C                   ENDIF
290600000118     C**
290700000118     C* FLAG DDT: PER TARIFFA DPD SEMPRE NO
290800020201     C     V1CNET        IFEQ      'D'
290900000118     C     V2CBAM        ANDEQ     'S'
291000000118     C                   MOVEL     MSG(85)       V1CMSG
291100000118     C                   SETON                                        4928
291200000118     C                   GOTO      ENDCT
291300000118     C                   ENDIF
291400940729     C*
291500940729     C     ENDCT         ENDSR
291600940729     C*
291700940509     C*--- PULIZIA SUBFILE AGGIORNAMENTO -----------------------------*
291800920525     C     PULSF         BEGSR
291900000000     C                   SETON                                        94
292000940706     C                   WRITE     TA01C03
292100000000     C                   SETOFF                                       94
292200900924     C                   Z-ADD     0             SAVREC
292300900924     C                   ENDSR
292400930629     C*
292500941013     C*--- PULIZIA E INIZIAL. SF DI IMMISSIONE -----------------------*
292600920525     C     PULSF2        BEGSR
292700000000     C                   SETON                                        84
292800940706     C                   WRITE     TA01C05
292900000000     C                   SETOFF                                       84
293000920525     C*
293100940517     C                   CLEAR                   TADORP
293200940517     C                   CLEAR                   TADATB
293300940517     C                   CLEAR                   TADLNP
293400940517     C                   CLEAR                   TADCTS
293500940517     C                   CLEAR                   DESCTS
293600020624     c                   clear                   tadcap
293700940517     C                   CLEAR                   TADSGL
293800940517     C                   CLEAR                   TADITR
293900940517     C                   CLEAR                   TADINO
294000940517     C                   CLEAR                   TADRPV
294100960521     C                   CLEAR                   SAVRPV
294200940517     C                   CLEAR                   TADMIN
294300940517     C                   CLEAR                   TADMAX
294400020624     c                   clear                   tadnaz
294500990623     C                   CLEAR                   DESNAZ
294600940518     C* CAMPI DI DUPLICA
294700940518     C                   CLEAR                   DUPLNP
294800940518     C                   CLEAR                   DUPCTS
294900940518     C                   CLEAR                   DUPCAP
295000940518     C                   CLEAR                   DUPSGL
295100940518     C                   CLEAR                   DUPITR
295200940518     C                   CLEAR                   DUPINO
295300940518     C                   CLEAR                   DUPRPV
295400940518     C                   CLEAR                   DUPMIN
295500940518     C                   CLEAR                   DUPMAX
295600990608     C                   CLEAR                   DUPNAZ
295700021118
295800021118     c                   Clear                   ForzaMsg
295900021118
296000000000     C                   DO        120           NR1
296100940706     C                   WRITE     TA01S05
296200000000     C                   END
296300930225     C                   Z-ADD     1             REC1
296400000000     C                   ENDSR
296500050530     c
296600050530     c*
296700050530     c* Controllo Tariffe particolari da eliminare ------------------------*
296800050530     c     CtrTParEli    BEGSR
296900050530     c                   z-add     1             xx
297000050530    2c                   dow       TParEli(XX)<>'  '
297100070129     c  n50ktpt          chain(n)  titpt01l
297200070129     c   50ktpt          chain(n)  tiopt01l
297300061011    3c                   if        %found and tptatb=' '
297400050530     c* verifico la data di scadenza della tariffa che deve essere
297500050530     c*  <= alla data di eliminazione della tariffa particolare
297600050530    4c                   if        comdst>TPardat(XX)
297700050530     c* Errore  forzabile per F21 se non già dato
297800050530    5c                   if        *inkv
297900130318     c**   tpareli(XX)   lookup    forzatpar                              31
298000130318     c                   IF        %lookup(tpareli(xx):forzatpar) > 0
298100130318     c                   eval      wTParEli = *on
298200130318     c                   ENDIF
298300050530     c                   else
298400050530     c* Errore non forzabile per gli altri casi
298500130318     c**                 setoff                                       31
298600130318     c                   eval      wTParEli = *off
298700050530    5c                   endif
298800050530     c
298900130318    5c**                 if        not *in31
299000130318     c                   IF        not wTParEli
299100050530     c* Errore
299200050530     c                   eval      v1cmsg=msg(115)
299300050530     c
299400050530     c                   eval      %subst(v1cmsg:19:2)=tpareli(XX)
299500050530     c     *iso          move      tpardat(XX)   wdatadmy
299600050530     c                   move      wdatadmy      wdata             6 0
299700050530     c                   eval      %subst(v1cmsg:41:8) =(%editw(wdata:'  /  / -
299800050530     c                              '))
299900050530     c
300000050530     c                   add       1             yy                4 0
300100050530     c                   movel     tpareli(xx)   forzatpar(YY)
300200050530     c                   seton                                        2841
300300050530     C                   GOTO      ENDCtrTPar
300400050530    5c                   endif
300500050530    4c                   endif
300600050530    3c                   endif
300700050530     c                   add       1             xx
300800050530    2c                   enddo
300900050530     c     EndCTRTPar    ENDSR
301000930629     C*
301100940615     C*-- AGGIORNAMENTO TNTAM ----------------------------------------*
301200920525     C     AGGTAM        BEGSR
301300940506     C*
301400061011     C** RIPOSIZIONAMENTO SUL FILE TNTAM
301500061011     C  n50KTAM2         CHAIN     TNTAM000                           01
301600061011     c   50ktam2         chain     tnofm000                           01
301700061010     C  N01TAMATB        COMP      'A'                                    01
301800050530     c* se non sono in immissione, controllo tariffe partic in scadenza
301900061010    1c                   if        not *in01
302000050530     c
302100050530     c* controllo se ci sono tariffe particolari da eliminare
302200050530     c                   EXSR      CtrTParEli
302300050530     c
302400050530     c* 28 on trovate --> errore
302500050530     c                   if        *in28
302600050530     c                   goto      ENDaggtam
302700050530    1c                   endif
302800050530    1c                   endif
302900050530     c
303000061010     C  N01              SETON                                        10
303100941012     C** MUOVO I CAMPI DEL VIDEO NEL FILE
303200941012     C                   EXSR      RIETAM
303300920525     C* I M M I S S I O N E
303400970226     C     *IN01         IFEQ      *ON
303500970226     C                   Z-ADD     V1CKSC        TAMKSC
303600970226     C                   Z-ADD     PRG           TAMPRG
303700970226     C                   Z-ADD     DATEU8        TAMDUV
303800970226     C                   MOVEL     *BLANKS       TAMFTR
303900040913     C                   Z-ADD     dateu8        TAMDTR
304000970226     C                   MOVEL     *BLANKS       TAMATB
304100970226     C                   CLEAR                   TAMDAT
304200150922     C***                CLEAR                   TAMTPR
304300101025     c                   clear                   tamfli
304400920525     C*
304500061011     C  n50              WRITE     TNTAM000
304600061011     c   50              write     tnofm000
304700040927      *
304800040927     c                   eval      tavtrd = 'TES'
304900040927     c                   eval      tavcta = 'I'
305000040927     c                   exsr      SUB_STORICO
305100090916      * se trattativa scrivo TIVOF
305200090916     c                   if        *in85
305300090916     c                   clear                   tivof000
305400090916     c                   eval      vofnrv = tamksc
305500090916     c                   move      tamctr        vofctr
305600090916     c                   eval      vofprg = %dec(tamprg:1:0)
305700090916     c                   select
305800090916     c                   when      §tadpd = 'S'
305900090916     c                   eval      voftpt = 'D'
306000090916     c                   when      §tafed = 'S'
306100090916     c                   eval      voftpt = 'F'
306200090916     c                   other
306300090916     c                   eval      voftpt = tamfie
306400090916     c                   endsl
306500091125      * valorizzo data presentazione offerta con la data del giorno
306600091125     c                   eval      vofdpo = dateu8
306700091125
306800090916     c                   write     tivof000
306900090916     c                   endif
307000920525     C*
307100940615     C** INDICATORE DI VARIAZIONE TNTAM
307200970226     C                   SETON                                        81
307300970226     C                   ENDIF
307400920525     C*
307500920525     C* V A R I A Z I O N E
307600000000     C   10COMODO        IFNE      TAMDS
307700941221     C                   Z-ADD     DATEU8        TAMDUV
307800040913     C                   Z-ADD     dateu8        TAMDTR
307900940511     C                   MOVEL     *BLANKS       TAMFTR
308000061011     C  n50              UPDATE    TNTAM000
308100061011     c   50              update    tnofm000
308200000000     C                   SETON                                        81
308300030704
308400030716     c                   eval      mantes = 'S'
308500960620     C                   MOVEL     TAMDS         COMODO
308600000000     C                   END
308700900709     C*
308800900320     C                   Z-ADD     TAMCTR        VCTR
308900941012     C*
309000050530     C     endaggtam     ENDSR
309100940509     C*
309200940509     C*--- SR DI IMMISSIONE TARIFFA ----------------------------------*
309300920525     C     IMMISS        BEGSR
309400921218     C                   SETON                                        99
309500000000     C     SUIMMX        TAG
309600921118     C** PULIZIA SF
309700000000     C                   EXSR      PULSF2
309800120315
309900120315       //?Imposto i tasti funzione nella riga mobile
310000120315     c                   eval      wPrima = *off
310100120315     c                   eval      wSeconda = *off
310200120315     c                   eval      wTerza = *off
310300120315     c                   eval      wQuarta = *on
310400120315     c                   ExSr      Tastifun
310500921118     C*
310600000000     C     SUIMM         TAG
310700120319     C** EMISSIONE COMANDI
310800120319     C                   WRITE     TA01Z06
310900941206     C***
311000941206     C   28
311100941206     CAN 62              DO
311200941206     C                   SETOFF                                       28
311300941206     C                   WRITE     TA01C05
311400941206     C                   SETON                                        28
311500941206     C                   ENDDO
311600941206     C***
311700921118     C** EMISSIONE SF IMMISSIONE
311800940706     C                   EXFMT     TA01C05
311900921118     C*
312000940526     C* PULIZIA CAMPO MESSAGGIO E RELATIVO INDICATORE (*IN28)
312100940526     C                   CLEAR                   $MSG
312200940526     C* AZZERO GLI INDICATORI RELATIVI AI MESSAGGI DI ERRORE
312300940526     C                   SETOFF                                       285556
312400940526     C                   SETOFF                                       575859
312500960522     C                   SETOFF                                       616289
312600990623     C                   SETOFF                                       5238
312700120315       //?F1 - dati manca tariffa
312800120315     c                   IF        *inka
312900120315     c                   exsr      gesw01
313000120315     c                   goto      suimm
313100120315     c                   ENDIF
313200940526     C*
313300120315     c  nkc
313400120315     CanNKL              DO
313500921118     C** ESEGUE CONTROLLI IMMISSIONE
313600000000     C                   EXSR      CTRIMM
313700940525     C   22              GOTO      SUIMM
313800921118     C*
313900000000     C** AGGIUNTA TARIFFE SU DISCO
314000920525     C                   EXSR      ADDTAD
314100000000     C** CMD7-NUOVA IMMISSIONE
314200920525     C   KG              GOTO      SUIMMX
314300921118     C*
314400921118     C**CMD6-IMMISSIONE E FINE LAVORO
314500921118     C  NKF              DO
314600940526     C                   Z-ADD     0             V1CRDA
314700940526     C                   MOVEL     *BLANKS       V1CRA
314800921118     C                   END
314900120315       //?F24 Altri tasti
315000120315     c                   IF        *inky
315100120315     c                   exsr      gesf24
315200120315     c                   goto      suimm
315300120315     c                   ENDIF
315400921118     C                   END
315500000000     C                   ENDSR
315600930629     C*
315700930629     C*--- CONTROLLO IMMISSIONE --------------------------------------*
315800920525     C     CTRIMM        BEGSR
315900990617     C                   SETOFF                                       221444
316000940526     C                   SETOFF                                       020304
316100940526     C                   SETOFF                                       050607
316200940526     C                   SETOFF                                       080911
316300940511     C                   Z-ADD     1             NR1               3 0
316400940510     C*
316500940706     C                   READC     TA01S05                                29
316600940524    1C     *IN29         DOWEQ     *OFF
316700020408
316800020408     c                   eval      woksfl = '0'
316900020624     c                   clear                   descts
317000020408
317100930629     C*
317200940510     C* TASTI DI DUPLICA
317300900420     C   02              Z-ADD     DUPLNP        TADLNP
317400900420     C   03              MOVE      DUPCTS        TADCTS
317500020624     c   04              movel     DUPCAP        tadcap
317600940524     C   05              MOVE      DUPSGL        TADSGL
317700940524     C   06              Z-ADD     DUPITR        TADITR
317800940524     C   07              Z-ADD     DUPINO        TADINO
317900940524     C   08              Z-ADD     DUPRPV        TADRPV
318000020624     c   44              movel     DUPNAZ        tadnaz
318100940524     C   09              Z-ADD     DUPMIN        TADMIN
318200940524     C   11              Z-ADD     DUPMAX        TADMAX
318300930629     C*
318400940510     C****  LINEA PARTENZA  ****
318500940510     C* LINEA PARTENZA OBBLIGATORIA
318600120906       //?Se non è stata inserita la LNP ma c'è il codice di tassazione
318700120906       //?errore di manca LNP
318800120906     c                   IF        TADlnp = *zeros and
318900120906     c                             TADcts <> *blanks
319000120906     c                   eval      *in55 = *on
319100120906     c                   eval      *in28 = *on
319200120906     c                   eval      $msg = msg(24)
319300120906     c                   goto      giuctr
319400120906     c                   ENDIF
319500120906
319600940510    2C     TADLNP        IFNE      0
319700930629     C*
319800940930     C*
319900940930     C* CONTROLLO VALIDITA' LINEA PARTENZA
320000940930     C     TADLNP        CHAIN     AZORG                              32
320100020312     c                   clear                   og143
320200940811     C*
320300940510    3C  N32ORGFVA        IFNE      ' '
320400940510     C                   SETON                                        32
320500940510    3C                   ENDIF
320600930219     C*
320700020527     c                   if        orgfag <> 'A' and orgfag <> 'F' and
320800020527     c                             orgfl2 <> 'S'
320900940510     C                   SETON                                        32
321000940510    3C                   ENDIF
321100930219     C*
321200940510     C* 32 ON  - LINEA PARTENZA INESISTENTE
321300940525     C     *IN32         IFEQ      *ON
321400940526     C                   SETON                                        5528
321500940525     C                   MOVEL     MSG(22)       $MSG
321600940525     C                   GOTO      GIUCTR
321700940525     C                   ENDIF
321800020312
321900020312     c                   eval      og143 = orgde3
322000020312     c                   eval      lnpntw = §ogntw
322100020312
322200971205     C* SE FLAG ITA/EST = 'I' ---> NON POSSO INSERIRE FILIALI ESTERE
322300971205    3C     V1CFIE        IFEQ      'I'
322400020201     C     V1CNET        ANDEQ     ' '
322500020312  4b c                   if        lnpntw = 'EEX' or lnpntw = 'EUP' or
322600020312     c                             lnpntw = 'FED'
322700971205     C                   SETON                                        5528
322800971205     C                   MOVEL     MSG(45)       $MSG
322900971205     C                   GOTO      GIUCTR
323000020312  4e c                   endif
323100971205    3C                   ENDIF
323200930629     C*
323300940510     C****  CODICE TASSAZIONE  ****
323400940510     C* CODICE TASSAZIONE OBBLIGATORIO
323500000000     C                   Z-ADD     1             A                 5 0
323600941011     C                   Z-ADD     1             K
323700120713     c                   IF        %scan('?':TADcts) > *zeros
323800120713     c                   exsr      Ric_CT
323900120713     c                   eval      TADcts = TB09cod
324000120713     c                   ENDIF
324100940524     C     TADCTS        LOOKUP    CCT(A)                                 35
324200940524    3C   35CCT(A)        IFEQ      '  '
324300940524     C                   SETOFF                                       35
324400940510    3C                   ENDIF
324500900111     C*
324600940525     C     *IN35         IFEQ      *OFF
324700940526     C                   SETON                                        5628
324800940525     C                   MOVEL     MSG(23)       $MSG
324900940525     C                   GOTO      GIUCTR
325000940525     C                   ENDIF
325100050303      * errore bloccante se codice tassazione non utilizzabile in gestione
325200050303      * tariffe clienti
325300050303     c                   If        uta(a) = 'N'
325400050303     c                   Eval      $msg = msg(36)
325500050303     c                   Eval      *In56 = *On
325600050303     c                   Eval      *In28 = *On
325700050303     c                   Goto      giuctr
325800050303     c                   EndIf
325900020206      *
326000020206      * Se tariffa FedEx devo utilizzare solo i codici tariffa
326100020206      * previsti per FedEx
326200020206     C     V1CNET        IFEQ      'F'
326300020206     C     UTF(A)        ANDNE     'S'
326400020206     C                   SETON                                        5628
326500020206     C                   MOVEL     MSG(100)      $MSG
326600020206     C                   GOTO      GIUCTR
326700020206     C                   ENDIF
326800020206      * Se tariffa non è FedEx non posso utilizzare i codici
326900020206      * tariffa previsti per FedEx
327000020206     C     V1CNET        IFNE      'F'
327100020206     C     UTF(A)        ANDEQ     'S'
327200020206     C                   SETON                                        5628
327300020206     C                   MOVEL     MSG(101)      $MSG
327400020206     C                   GOTO      GIUCTR
327500020206     C                   ENDIF
327600020206      *
327700940525     C*
327800941011     C* SE FLAG ITA/EST = 'I' ---> NON POSSO INSERIRE COD.TASS. ESTERI
327900940811     C     V1CFIE        IFEQ      'I'
328000020201     C     V1CNET        ANDEQ     ' '
328100941011     C     NAR(A)        LOOKUP    NAZ(K)                                 35
328200950117     C     *IN35         IFEQ      *OFF
328300940811     C                   SETON                                        5628
328400940811     C                   MOVEL     MSG(47)       $MSG
328500940811     C                   GOTO      GIUCTR
328600940811     C                   ENDIF
328700941011     C                   ENDIF
328800941011     C*
328900950112     C* SE FLAG ITA/EST = 'E' ---> NON POSSO INSERIRE NELLA STESSA RIGA
329000950112     C*   DI DETTAGLIO SIA LA LINEA PARTENZA CHE IL COD.TASS. ITALIANI.
329100941026     C*   SE LA LNP E' ESTERA NON IMPORTA CONTROLLARE SE IL CODICE
329200941026     C*   TASSAZIONE E' ESTERO O ITALIANO PERCHE' LO ACCETTO UGUALMENTE
329300020312     c                   if        v1cfie = 'E' and lnpntw <> 'EEX' and
329400020312     c                             lnpntw <> 'EUP' and lnpntw <> 'FED'
329500941011     C     NAR(A)        LOOKUP    NAZ(K)                                 35
329600950117     C     *IN35         IFEQ      *ON
329700940811     C                   SETON                                        5628
329800940811     C                   MOVEL     MSG(48)       $MSG
329900940811     C                   GOTO      GIUCTR
330000940811     C                   ENDIF
330100941011     C                   ENDIF
330200020328
330300020328      * se tariffa estera non posso accettare codice tassazione 'IT'
330400020328     c                   if        v1cfie = 'E' and tadcts = 'IT'
330500020328     c                   eval      *in56 = *on
330600020328     c                   eval      *in28 = *on
330700020328     c                   eval      $msg = msg(107)
330800020328     c                   goto      giuctr
330900020328     c                   endif
331000020404
331100020404      * se tariffa DPD non posso accettare codice tassazione 'EE'
331200020404     c                   if        v1cnet = 'D' and tadcts = 'EE'
331300020404     c                   eval      *in56 = *on
331400020404     c                   eval      *in28 = *on
331500020404     c                   eval      $msg = msg(109)
331600020404     c                   goto      giuctr
331700020404     c                   endif
331800020624
331900020624      * se linea di partenza 950 = tutte le linee non posso accettare i codici
332000020624      * di tassazione 'IT' e 'EE'
332100020624     c                   if        tadlnp = 950 and
332200020624     c                             (tadcts = 'IT' or tadcts = 'EE')
332300020624     c                   eval      *in56 = *on
332400020624     c                   eval      *in28 = *on
332500020624     c                   eval      $msg = msg(110)
332600020624     c                   goto      giuctr
332700020624     c                   endif
332800940811     C*
332900940510     C* DECODIFICA CODICE TASSAZIONE
333000940525     C                   MOVEL     DCT(A)        DESCTS
333100020215     C                   MOVE      ORP(A)        TADORP
333200930629     C*
333300941011     C****  C. A. P.  ****
333400020624    3c                   if        tadcap <> *blanks
333500970718     C                   CLEAR                   DSSI95
333600970718     C                   CLEAR                   DSLV13
333700941011     C*
333800970718     C                   MOVEL     '3'           I95TCN
333900020624     c                   movel     tadnaz        i95nar
334000020624     c                   movel     tadcap        i95cap
334100040316     c                   z-add     dateu8        I95DAT
334200970718     C                   CALL      'FNLV13R'
334300970718     C                   PARM                    KPJBA
334400970718     C                   PARM                    DSLV13
334500970718     C                   PARM                    DSSI95
334600970718     C                   MOVEL     '1'           FNLV13            1
334700941011     C*
334800970718     C     O13ERR        IFNE      *BLANKS
334900970718     C                   MOVEL     O13MSG        $MSG
335000941011     C                   SETON                                        5728
335100941011     C                   GOTO      GIUCTR
335200941011     C                   ENDIF
335300990624     C* DECODIFICO LA NAZIONE SE E' DIVERSA DA BLANKS
335400020624     c                   if        tadnaz = *blanks
335500990624     C                   CLEAR                   DESNAZ
335600990624     C                   ELSE
335700990623     C                   MOVEL     '15'          COD
335800990623     C                   MOVEL     *BLANKS       KEY
335900020624     c                   movel     tadnaz        key
336000990623     C     KTAB          CHAIN     TABEL                              32
336100990623     C*
336200990623     C     *IN32         IFEQ      *OFF
336300990623     C     TBLFLG        ANDEQ     ' '
336400990623     C                   MOVEL     TBLUNI        DESNAZ
336500990623     C                   ELSE
336600990623     C* ERRORE
336700990623     C                   SETON                                        3828
336800990623     C                   MOVEL     MSG(81)       $MSG
336900990623     C                   GOTO      GIUCTR
337000990623     C                   ENDIF
337100990624     C                   ENDIF
337200930630     C*
337300930629     C* SE TARIFFA REGIONE O ITALIA NON DEVE ESISTERE IL CAP
337400940525     C     REG(A)        IFEQ      '9'
337500940526     C                   SETON                                        5728
337600940525     C                   MOVEL     MSG(25)       $MSG
337700940525     C                   GOTO      GIUCTR
337800940525     C                   ENDIF
337900940525     C*
338000930702     C* CONTROLLO SE IL CAP E' CONGRUENTE CON IL CODICE TASSAZIONE
338100970718    4C     TADCTS        IFNE      O95CTS
338200940526     C                   SETON                                        5728
338300940525     C                   MOVEL     MSG(26)       $MSG
338400930702     C                   GOTO      GIUCTR
338500940510    4C                   ENDIF
338600931117     C*
338700931117     C* SE INSERITO IL CAP NON BISOGNA IMMETTERE LA TARIFFA PROVINCIA
338800940510    4C     TADINO        IFNE      0
338900940526     C                   SETON                                        6128
339000940525     C                   MOVEL     MSG(27)       $MSG
339100931117     C                   GOTO      GIUCTR
339200940510    4C                   ENDIF
339300931117     C*
339400940510    3C                   ENDIF
339500930629     C*
339600940510     C****  SCAGLIONE  TARIFFA  ****
339700940510     C* SCAGLIONE OBBLIGATORIO
339800940511    3C     TADSGL        IFEQ      0
339900940526     C                   SETON                                        5828
340000940525     C                   MOVEL     MSG(28)       $MSG
340100900420     C                   GOTO      GIUCTR
340200941124   X3C                   ELSE
340300950119     C* WSAP = " " --> SI TRATTA DI SCAGLIONE SENZA DECIMALI QUINDI NON
340400950116     C*                SI POSSONO INSERIRE VALORI DOPO LA VIRGOLA
340500941124    4C     WSAP          IFEQ      ' '
340600941124     C                   MOVE      TADSGL        W0030
340700941124     C     W0030         IFGT      0
340800941124     C                   SETON                                        5828
340900950118     C                   MOVEL     MSG(46)       $MSG
341000941124     C                   GOTO      GIUCTR
341100941124     C                   ENDIF
341200941124    4C                   ENDIF
341300021118      * Se tariffa FedEx Documenti devo controllare che non sia stato immesso
341400021118      * uno scaglione > del peso possibile per questa tariffa
341500021118      * messaggio forzabile
341600021118     c                   If        V1cNet= 'F' and DueCtr >= §DfeDalD
341700021118     c                             and DueCtr <= §DfeAlD
341800021118     c                             and TadSgl > §DfePkgD
341900021118     c                             and ForzaMsg = *Blanks
342000021118     c                   Movel     Msg(112)      $Msg
342100021118     c                   Eval      *In58 = *On
342200021118     c                   Eval      *In28 = *On
342300021118     c                   Eval      ForzaMsg = '1'
342400021118     c                   GoTo      GiuCtr
342500021118     c                   EndIf
342600940510    3C                   ENDIF
342700930629     C*
342800940510     C****  TARIFFA CITTA'  ****
342900940510     C* TARIFFA CITTA' OBBLIGATORIA
343000940525     C     TADITR        IFEQ      0
343100940526     C                   SETON                                        5928
343200940525     C                   MOVEL     MSG(29)       $MSG
343300940525     C                   GOTO      GIUCTR
343400941123     C                   ELSE
343500050616     C* 88 OFF -  TARIFFA NON IN % verifico se si
343600941124     C*   POSSONO INSERIRE DECIMALI
343700941124     C     *IN88         IFEQ      *OFF
343800050616     C* LA DIVISA NON PREVEDE DECIMALI
343900990617     C     §CVFDC        ANDNE     'S'
344000941123     C                   MOVE      TADITR        W0030             3 0
344100941123     C     W0030         IFGT      0
344200941123     C                   SETON                                        5928
344300990617     C                   MOVEL     MSG(79)       $MSG
344400941123     C                   GOTO      GIUCTR
344500941123     C                   ENDIF
344600941123     C                   ENDIF
344700940525     C                   ENDIF
344800930629     C*
344900941123     C****  TARIFFA PROVINCIA  ****
345000050616     C* 88 OFF -  TARIFFA NON IN % verifico se si
345100941124     C*   POSSONO INSERIRE DECIMALI
345200941124     C  N88TADINO        IFGT      0
345300941123     C                   MOVE      TADINO        W0030             3 0
345400941123     C     W0030         IFGT      0
345500050616     C*  LA DIVISA NON PREVEDE DECIMALI
345600990617     C     §CVFDC        ANDNE     'S'
345700941123     C                   SETON                                        6128
345800990617     C                   MOVEL     MSG(79)       $MSG
345900941123     C                   GOTO      GIUCTR
346000941123     C                   ENDIF
346100941123     C                   ENDIF
346200941123     C*
346300960520     C****  RAPPORTO PESO/VOLUME  ****
346400980505     C* CONTROLLO SE IL TIPO TARIFFA PREVEDE LA GESTIONE DEGLI ARROTONADMENTI
346500980505     C* IN BASE AL FLAG DELLA TABELLA TR
346600980505     C     WRPV          IFNE      'S'
346700980505     C     TADRPV        ANDNE     0
346800980505     C                   SETON                                        8928
346900980505     C                   MOVEL     MSG(75)       $MSG
347000980505     C                   GOTO      GIUCTR
347100980505     C                   ENDIF
347200960520     C* SE INSERITO UN RAPPORTO PESO/VOL. SUPERIORE AL LIMITE INDICATO
347300960520     C*   IN TABELLA EMETTO ERRORE FORZABILE
347400960521     C     TADRPV        IFNE      SAVRPV
347500960520     C     TADRPV        ANDGT     §1GRPV
347600960520     C                   SETON                                        8928
347700960520     C                   MOVEL     MSG(59)       $MSG
347800960521     C                   MOVEL     TADRPV        SAVRPV
347900960520     C                   GOTO      GIUCTR
348000960520     C                   ENDIF
348100020404
348200020404      * Nazione
348300020624     c                   if        tadnaz <> *blanks and tadcap = *blanks
348400020404     c                   eval      $msg = msg(108)
348500020404     c                   eval      *in28 = *on
348600020404     c                   eval      *in57 = *on
348700020404     c                   goto      giuctr
348800020404     c                   endif
348900990617     C*
349000990617     C****  IMPORTO MASSIMO TARIFFA  ****
349100050616     C* 88 OFF -  TARIFFA NON IN % verifico se si
349200990617     C*   POSSONO INSERIRE DECIMALI
349300990617     C  N88TADMAX        IFGT      0
349400990617     C                   MOVE      TADMAX        W0030             3 0
349500990617     C     W0030         IFGT      0
349600050616     C*  LA DIVISA NON PREVEDE DECIMALI
349700990617     C     §CVFDC        ANDNE     'S'
349800990617     C                   SETON                                        6228
349900990617     C                   MOVEL     MSG(79)       $MSG
350000990618     C                   MOVEL     *ON           *INKK
350100990617     C                   GOTO      GIUCTR
350200990617     C                   ENDIF
350300990617     C                   ENDIF
350400990617     C*
350500990617     C****  IMPORTO MINIMO  TARIFFA  ****
350600050616     C* 88 OFF -  TARIFFA NON IN % verifico se si
350700990617     C*   POSSONO INSERIRE DECIMALI
350800990617     C  N88TADMIN        IFGT      0
350900990617     C                   MOVE      TADMIN        W0030             3 0
351000990617     C     W0030         IFGT      0
351100050616     C*  LA DIVISA NON PREVEDE DECIMALI
351200990617     C     §CVFDC        ANDNE     'S'
351300990617     C                   SETON                                        5228
351400990617     C                   MOVEL     MSG(79)       $MSG
351500990618     C                   MOVEL     *ON           *INKK
351600990617     C                   GOTO      GIUCTR
351700990617     C                   ENDIF
351800990617     C                   ENDIF
351900960520     C*
352000940525     C****  IMPORTO MASSIMO TARIFFA  ****
352100940525     C* SE INSERITO DEVE ESSERE MAGGIORE O UGUALE AL MINIMO
352200940525     C     TADMAX        IFGT      0
352300940525     C     TADMAX        ANDLT     TADMIN
352400940526     C                   SETON                                        6228
352500940525     C                   MOVEL     MSG(31)       $MSG
352600941107     C                   MOVEL     *ON           *INKK
352700940525     C                   GOTO      GIUCTR
352800940525     C                   ENDIF
352900980310      * E' inutile inserire dei valori minimi e massimi per scaglioni inferiori
353000980310      * al valore minimo tassabile o al valore tariffa finita
353100980310      * quindi faccio il controllo di TADMAX E/O TADMAX
353200980310     C     TADMAX        IFGT      0
353300980310     C     TADMIN        ORGT      0
353400980310      * VALORE TARIFFA FINITA
353500980310     C     TAMATA        IFGT      TADSGL
353600980310     C     TAMATA        OREQ      0
353700980601     C     WRKMIN        ANDGT     TADSGL
353800980310     C                   SETON                                        6228
353900980310     C                   MOVEL     MSG(73)       $MSG
354000980310     C                   MOVEL     *ON           *INKK
354100980310     C                   GOTO      GIUCTR
354200980310     C                   ENDIF
354300980310      *
354400980310     C                   ENDIF
354500940525     C*
354600940518     C                   Z-ADD     TADITR        RISITR
354700000000     C                   Z-ADD     TADINO        RISINO
354800000000     C                   Z-ADD     TADRPV        RISRPV
354900940525     C                   Z-ADD     TADMIN        RISMIN
355000940525     C                   Z-ADD     TADMAX        RISMAX
355100940510     C*
355200061011     C  n50KTAD          CHAIN     TITAD000                           32
355300061011     c   50ktad          chain     tiofd000                           32
355400940525     C  N32TADATB        COMP      'A'                                    32
355500940525     C*
355600940525     C     *IN32         IFEQ      *OFF
355700940526     C                   SETON                                        5828
355800940525     C                   MOVEL     MSG(30)       $MSG
355900940525     C                   ENDIF
356000940525     C*
356100940518     C                   Z-ADD     RISITR        TADITR
356200000000     C                   Z-ADD     RISINO        TADINO
356300000000     C                   Z-ADD     RISRPV        TADRPV
356400940525     C                   Z-ADD     RISMIN        TADMIN
356500940525     C                   Z-ADD     RISMAX        TADMAX
356600940510     C*
356700940525     C* 58 ON - RECORD TARIFFA GIA' ESISTENTE
356800940525     C   58              GOTO      GIUCTR
356900930629     C*
357000940510     C* SE TUTTO BENE SALVO I CAMPI PER LA DUPLICA
357100900420     C                   MOVEL     TADINO        DUPINO
357200940518     C                   Z-ADD     TADITR        DUPITR
357300900420     C                   Z-ADD     TADLNP        DUPLNP
357400900420     C                   MOVEL     TADCTS        DUPCTS
357500940518     C                   MOVEL     TADSGL        DUPSGL
357600900420     C                   Z-ADD     TADRPV        DUPRPV
357700020624     c                   movel     tadcap        DUPCAP
357800020624     c                   movel     tadnaz        DUPNAZ
357900940518     C                   Z-ADD     TADMIN        DUPMIN
358000940518     C                   Z-ADD     TADMAX        DUPMAX
358100930629     C*
358200940525     C                   SETON                                        14
358300020408     c                   eval      woksfl = '1'
358400000000     C     GIUCTR        TAG
358500940525     C                   SETON                                        22
358600940706     C                   UPDATE    TA01S05
358700940525     C   14              SETOFF                                       2214
358800930629     C*
358900940525     C   22              Z-ADD     NR1           REC1
359000940525     C   22              GOTO      ENDCT2
359100940511    2C                   ENDIF
359200940511     C*
359300020408     c                   if        woksfl = '0'
359400020408     C                   UPDATE    TA01S05
359500020408     c                   endif
359600940706     C                   READC     TA01S05                                29
359700940511    1C                   ENDDO
359800940510     C*
359900940509     C     ENDCT2        ENDSR
360000930629     C*
360100940509     C*--- IMMISSIONE TARIFFE ----------------------------------------*
360200921118     C     ADDTAD        BEGSR
360300921119     C                   Z-ADD     1             NR1
360400940706     C                   READC     TA01S05                                29
360500940524     C     *IN29         DOWEQ     '0'
360600921118     C     TADLNP        IFGT      0
360700000000     C                   EXSR      WTRTAD
360800000000     C                   END
360900921119     C*
361000940706     C                   READC     TA01S05                                29
361100000000     C                   END
361200000000     C                   ENDSR
361300940509     C*
361400990601     C*--- SCRITTURA TITAD SU DISCO ----------------------------------*
361500921118     C     WTRTAD        BEGSR
361600980219     C* VALORE POSITIVO NEI CAMPI
361700980220     C                   MLLZO     1             TADSGL
361800980219     C                   MLLZO     1             TADITR
361900980219     C                   MLLZO     1             TADINO
362000980219     C                   MLLZO     1             TADRPV
362100980219     C                   MLLZO     1             TADMIN
362200980219     C                   MLLZO     1             TADMAX
362300980219     C*
362400940629     C                   Z-ADD     V1CKSC        TADKSC
362500000000     C                   Z-ADD     PRG           TADPRG
362600000000     C                   MOVEL     *BLANKS       TADATB
362700910605     C*
362800000000     C                   Z-ADD     1             A
362900940524     C     TADCTS        LOOKUP    CCT(A)                                 35
363000990608     C   35              MOVE      ORP(A)        TADORP
363100940524     C  N35              MOVEL     *BLANKS       TADORP
363200900320     C                   MOVEL     TAMCTR        TADCTR
363300940510     C*
363400061011     C  n50KTAD          SETLL     TITAD000                               32
363500061011     c   50ktad          setll     tiofd000                               32
363600940510     C*
363700940524     C     *IN32         IFEQ      *OFF
363800040913     C                   Z-ADD     DATEU8        TADDTR
363900940511     C                   MOVEL     *BLANKS       TADFTR
364000061011     C  n50              WRITE     TITAD000                             19
364100061011     c   50              write     tiofd000                             19
364200940510     C*
364300940510     C                   ELSE
364400940510     C*
364500940518     C                   Z-ADD     TADITR        RISITR
364600940518     C                   Z-ADD     TADINO        RISINO
364700940518     C                   Z-ADD     TADRPV        RISRPV
364800940518     C                   Z-ADD     TADMIN        RISMIN
364900940518     C                   Z-ADD     TADMAX        RISMAX
365000940518     C*
365100061011     C  n50KTAD          READE     TITAD000                               H2
365200061011     c   50ktad          reade     tiofd000                               H2
365300000000     C   H2              GOTO      FINE
365400000000     C                   MOVEL     *BLANKS       TADATB
365500130124     C********           Z-ADD     DATEU8        TADDTR
365600940511     C                   MOVEL     *BLANKS       TADFTR
365700940518     C                   Z-ADD     RISITR        TADITR
365800000000     C                   Z-ADD     RISINO        TADINO
365900000000     C                   Z-ADD     RISRPV        TADRPV
366000940518     C                   Z-ADD     RISMIN        TADMIN
366100940518     C                   Z-ADD     RISMAX        TADMAX
366200061011     C  n50              UPDATE    TITAD000
366300061011     c   50              update    tiofd000
366400940510     C                   ENDIF
366500940510     C*
366600921118     C  N19              SETON                                        30
366700921118     C                   ENDSR
366800940509     C*
366900940509     C*--- SR AGGIORNAMENTO ------------------------------------------*
367000921118     C     AGGIOR        BEGSR
367100050701
367200050701      * prima di caricare il subfile se è una tariffa a spedizione leggo
367300050701      * tutto il dettaglio per controllare che non esistano più scaglioni
367400050701      * per la stessa lnp e cts
367500050701      * questo perchè in fatturazione viene preso solo 1 scaglione quindi
367600050701      * non ha senso inserirne di più
367700050701     c                   If        tiptar = '2'
367800050701     c                   ExSr      sr_ctrtarspe
367900050701     c                   EndIf
368000110128
368100110128      /free
368200110128       //?Se tariffa FedEx devo controllare il rapporto perso volume
368300110128       //?deve essere uguale per tutti gli scaglioni
368400110128        IF  V1Cnet = 'F';
368500110128          exsr CtrRpv;
368600110128        ENDIF;
368700110128      /end-free
368800050701
368900921118     C     SUAGG2        TAG
369000061011     C  n50KTTAD         SETLL     TITAD000
369100061011     c   50kttad         setll     tiofd000
369200921118     C                   EXSR      PULSF
369300921118     C                   EXSR      CARSFA
369400930224     C     *IN21         IFEQ      '1'
369500921118     C*
369600921118     C     SUAGG1        TAG
369700940526     C                   Z-ADD     0             V1CRDA
369800940526     C                   MOVEL     *BLANKS       V1CRA
369900900924     C                   SETOFF                                       19
370000120315
370100120315       //?Imposto i tasti funzione nella riga mobile
370200120315     c                   eval      wPrima = *off
370300120315     c                   eval      wSeconda = *off
370400120315     c                   eval      wTerza = *on
370500120315     c                   eval      wQuarta = *off
370600120315     c                   ExSr      Tastifun
370700120315
370800000000     C     SUAGG         TAG
370900120319     C                   WRITE     TA01Z04
371000940706     C                   EXFMT     TA01C03
371100921118     C*
371200940526     C* PULIZIA CAMPO MESSAGGIO E RELATIVO INDICATORE (*IN28)
371300940526     C                   CLEAR                   VCCMSG
371400940526     C                   SETOFF                                       28
371500940624     C* AZZERO GLI INDICATORI RELATIVI AI MESSAGGI DI ERRORE
371600940526     C                   SETOFF                                       596162
371700960522     C                   SETOFF                                       89
371800940526     C*
371900120315     c  nkc
372000120315     CanNKL              DO
372100921118     C** CMD9 -IMMISSIONE
372200921118     C   KI              DO
372300050630werr c                   if        werr = *Off
372400110128     c                             and not werrRpv
372500921118     C                   EXSR      IMMISS
372600050701
372700050701      * per tariffa a spedizione devo controllare ora se tutto a posto
372800050701      * perchè non ragiono sul subfile ma sui dati caricati sul titad
372900050701     c                   If        tiptar = '2'
373000050701     c                   ExSr      sr_ctrtarspe
373100050701     c                   EndIf
373200050701
373300050701      * se errore per + scaglioni in tariffa a spedizione imposto
373400050701      * il msg e accendo l'indicatore
373500050701     c                   If        werr = *On
373600050701     c                   Eval      vccmsg = 'Par ' + wlnp + ' Arrivo ' +
373700050701     c                             wcts
373800050701     c                   If        wcap <> *Blanks
373900050701     c                   Eval      vccmsg = %trim(vccmsg) +
374000050701     c                             ' - c.a.p. ' + wcap
374100050701     c                   EndIf
374200050701     c                   Eval      vccmsg = %trim(vccmsg) +
374300050701     c                             ' presenti più scaglioni'
374400050701     c                   Eval      *In28 = *On
374500050701     c                   Goto      suagg2
374600050701     c                   EndIf
374700110128
374800110128      /free
374900110128       //?Se tariffa FedEx devo controllare il rapporto perso volume
375000110128       //?deve essere uguale per tutti gli scaglioni
375100110128        IF  V1Cnet = 'F';
375200110128          exsr CtrRpv;
375300110128        ENDIF;
375400110128      /end-free
375500110128       //?Se tariffa FedEx e scaglioni con rapporto peso volume
375600110128       //?diverso emetto messaggio di errore
375700110128     c                   IF        werrRpv
375800110128     c                   eval      VCCmsg = 'Par ' + %editc(wlnpRpv:'X') +
375900110128     c                             ' Arrivo ' + wctsRpv +
376000110128     c                             ' Scaglione ' + %editc(wsglRpv:'4') +
376100110128     c                             ' presente P/V diverso'
376200110128     c                   eval      *in28 = *on
376300110128     c                   goto      suagg2
376400110128     c                   ENDIF
376500110128
376600980313     C* CONTROLLO SE TARIFFA IMPORT CON LA TARIFFA PARTICOLARE ASSIC. X CONTO
376700980313     C     UNO           IFEQ      ' '
376800980313     C                   EXSR      CTRIMP
376900980313     C   28              MOVEL     MSG(72)       VCCMSG
377000980313     C   28              CLEAR                   V1CMSG
377100980313     C   28              MOVEL     '1'           UNO
377200980313     C   28              GOTO      SUAGG2
377300980313     C                   END
377400120314       //?F1 - dati manca tariffa
377500120314     c                   IF        *inka
377600120314     c                   exsr      gesw01
377700120315     c                   goto      suagg
377800120314     c                   ENDIF
377900120315       //?F3 fine
378000120315     c   kc              goto      fine
378100921118     C*
378200921118     C** CMD12-FINE LAVORO NO AGGIORNAMENTO
378300050630     C   kl              GOTO      SUAGG1
378400921118     C** CMD6 -AGGIORNAMENTO E FINE LAVORO
378500921118     C  NKF              GOTO      SUAGG2
378600921118     C   KF              GOTO      ENDAGG
378700120315       //?F24 Altri tasti
378800120315     c                   IF        *inky
378900120315     c                   exsr      gesf24
379000120315     c                   goto      suagg
379100120315     c                   ENDIF
379200050630werr c                   endif
379300921118     C                   END
379400900924     C*
379500921118     C** ROLLUP  AVANZAMENTO NO AGG.
379600940524     C     *IN25         IFEQ      *ON
379700940310     C                   MOVEL     KE            KX
379800940518     C                   MOVEL     PARX          COMX              9
379900061011     C  n50KSCOR         SETLL     TITAD000                               32
380000061011     c   50kscor         setll     tiofd000                               32
380100061011     C   32
380200061011     Cann50KTAM2         READE     TITAD000                               33
380300061011     c   32
380400061011     Can 50ktam2         reade     tiofd000                               33
380500940310     C                   EXSR      CARSFA
380600940310     C                   GOTO      SUAGG1
380700940310     C                   ENDIF
380800921118     C*
380900921118     C** CONTROLLI AGGIORNAMENTO
381000000000     C                   EXSR      CTRAGG
381100940526     C   28              GOTO      SUAGG
381200921118     C*
381300921118     C** ESEGUE AGGIORNAMENTI
381400000000     C                   EXSR      AGGTAD
381500050701
381600050701      * per tariffa a spedizione devo controllare ora se tutto a posto
381700050701      * perchè non ragiono sul subfile ma sui dati caricati sul titad
381800050701     c                   If        tiptar = '2'
381900050701     c                   ExSr      sr_ctrtarspe
382000050701     c                   EndIf
382100050701
382200050701      * se errore per + scaglioni in tariffa a spedizione imposto
382300050701      * il msg e accendo l'indicatore
382400050701     c                   If        werr = *On
382500050701     c                   Eval      vccmsg = 'Par ' + wlnp + ' Arrivo ' +
382600050701     c                             wcts
382700050701     c                   If        wcap <> *Blanks
382800050701     c                   Eval      vccmsg = %trim(vccmsg) +
382900050701     c                             ' - c.a.p. ' + wcap
383000050701     c                   EndIf
383100050701     c                   Eval      vccmsg = %trim(vccmsg) +
383200050701     c                             ' presenti più scaglioni'
383300050701     c                   Eval      *In28 = *On
383400050701     c                   Goto      suagg
383500050701     c                   EndIf
383600110128
383700110128      /free
383800110128       //?Se tariffa FedEx devo controllare il rapporto perso volume
383900110128       //?deve essere uguale per tutti gli scaglioni
384000110128        IF  V1Cnet = 'F';
384100110128          exsr CtrRpv;
384200110128        ENDIF;
384300110128      /end-free
384400110128       //?Se tariffa FedEx e scaglioni con rapporto peso volume
384500110128       //?diverso emetto messaggio di errore
384600110128     c                   IF        werrRpv
384700110128     c                   eval      VCCmsg = 'Par ' + %editc(wlnpRpv:'X') +
384800110128     c                             ' Arrivo ' + wctsRpv +
384900110128     c                             ' Scaglione ' + %editc(wsglRpv:'4') +
385000110128     c                             ' presente P/V diverso'
385100110128     c                   eval      *in28 = *on
385200110128     c                   goto      suagg
385300110128     c                   ENDIF
385400050701
385500980313     C* CONTROLLO SE TARIFFA IMPORT CON LA TARIFFA PARTICOLARE ASSIC. X CONTO
385600980313     C     UNO           IFEQ      ' '
385700980313     C                   EXSR      CTRIMP
385800980313     C   28              MOVEL     MSG(72)       VCCMSG
385900980313     C   28              CLEAR                   V1CMSG
386000980313     C   28              MOVEL     '1'           UNO
386100980313     C   28              GOTO      SUAGG
386200980313     C                   END
386300120315       //?F1 - dati manca tariffa
386400120315     c                   IF        *inka
386500120315     c                   exsr      gesw01
386600120315     c                   goto      suagg
386700120315     c                   ENDIF
386800011214     C*
386900011214     C** F10 - STAMPA TARIFFA (ALLA FINE)
387000011214     C   KJ
387100011214     CAN 15              GOTO      ENDAGG
387200011214     C   KJ
387300011214     CANN15              MOVEL     *INKJ         *INKF
387400921118     C*
387500961212     C** CMD6 -AGGIORNAMENTO E FINE
387600991115     C   KF              MOVEL     'C'           FLGRTN
387700921118     C   KF              GOTO      ENDAGG
387800120319       //?F24 Altri tasti
387900120319     c                   IF        *inky
388000120319     c                   exsr      gesf24
388100120319     c                   goto      suagg
388200120319     c                   ENDIF
388300900924     C*
388400961212     C** CMD15-P O R T O  (ABILITATO SOLO NELLE OFFERTE)
388500980902     C   KP              DO
388600031124      * recupero dalla tabella CAT la data di elaborazione CAT consigliata
388700031124     c                   Clear                   Tibs02Ds
388800031124     c                   Clear                   dCat
388900031124     c                   Eval      T02Mod = 'C'
389000031124     c                   Eval      T02Sif = Knsif
389100031124     c                   Eval      T02Cod = 'CAT'
389200031124     c                   Movel(p)  '1'           T02Ke1
389300031124     c                   Call      'TIBS02R'
389400031124     c                   Parm                    Kpjba
389500031124     c                   Parm                    Tibs02Ds
389600031124     c                   If        T02Err = *Blanks
389700031124     c                   Movel     T02Uni        dCat
389800031124     c                   EndIf
389900031124    1 *
390000961212     C                   CLEAR                   DSTE00
390100961212     C                   MOVEL     'O02'         D00OP0
390200961212     C                   MOVEL     'F15'         D00OP1
390300961212     C                   MOVEL     'P'           D00TLA
390400091127      * Con l'arrivo delle trattative dobbiamo segnalare ai pgm del CAT che
390500091127      * stiamo parlando di trattativa e non di visita
390600091127      * Se trattativa (ind. 85 acceso) impostiamo "X" nel campo D00DSF
390700091125     C**                 MOVEL     'S'           D00DSF
390800091125     C   85              MOVEL     'X'           D00DSF
390900091127     C                   MOVEL     'O'           D00CTO
391000961212     C                   MOVEL     V1CNRV        D00KSC
391100961212     C                   MOVEL     V1CCTR        D00CTR
391200091125     C                   MOVEL     V1CPRG        D00PRG
391300961212     C                   MOVEL     V1CFIE        D00FIE
391400961212     C                   MOVEL     V1CTSP        D00TSP
391500011023     C                   MOVEL     V1CDIV        D00DIV
391600020205     C                   MOVE      V1CNET        D00DPD
391700011030     C                   MOVEL     *BLANKS       D00PRI
391800031124      * propongo data massima tra la data del giorno e quella recuperata
391900031124      * dalla tavella CAT
392000031124     c                   if        dateu8 > §CATdta
392100031124     C                   MOVEL     DATEU8        D00DTC
392200031124     C                   ELSE
392300031124     C                   MOVEL     §CATDTA       D00DTC
392400031124     C                   ENDIF
392500031124      *
392600970131     C                   CALL      'TNTE01C'
392700961212     C                   PARM                    KPJBA
392800961212     C                   PARM                    DSTE00
392900961212     C* D00ERR = "1" --> ERRORE
393000961212     C     D00ERR        IFEQ      '1'
393100961212     C                   SETON                                          28
393200961212     C                   MOVEL     D00MSG        VCCMSG
393300961219     C                   GOTO      SUAGG1
393400961212     C                   ENDIF
393500961220     C* RICARICO IL SFILE PERCHE' POTREBBERO ESSERCI STATE VARIAZIONI
393600961219     C                   GOTO      SUAGG2
393700980902     C                   ENDDO
393800961212     C*
393900961212     C** CMD17-D E L T A
394000980902     C   KR              DO
394100980514     C                   CLEAR                   DSTE00
394200980514     C     *IN50         IFEQ      *OFF
394300980514     C                   MOVEL     'T02'         D00OP0
394400980514     C                   MOVEL     'T'           D00CTO
394500980514     C                   MOVEL     V1CKSC        D00KSC
394600980514     C                   MOVEL     V1CPRG        D00PRG
394700980514     C                   ELSE
394800980514     C                   MOVEL     'O02'         D00OP0
394900091127      * Con l'arrivo delle trattative dobbiamo segnalare ai pgm del CAT che
395000091127      * stiamo parlando di trattativa e non di visita
395100091127      * Se trattativa (ind. 85 acceso) impostiamo "X" nel campo D00DSF
395200091127     C                   MOVEL     'O'           D00CTO
395300091125     C   85              MOVEL     'X'           D00DSF
395400091125     C                   MOVEL     V1CPRG        D00PRG
395500980514     C                   MOVEL     V1CNRV        D00KSC
395600980514     C                   ENDIF
395700031124      * recupero dalla tabella CAT la data di elaborazione CAT consigliata
395800031124     c                   Clear                   Tibs02Ds
395900031124     c                   Clear                   dCat
396000031124     c                   Eval      T02Mod = 'C'
396100031124     c                   Eval      T02Sif = Knsif
396200031124     c                   Eval      T02Cod = 'CAT'
396300031124     c                   Movel(p)  '1'           T02Ke1
396400031124     c                   Call      'TIBS02R'
396500031124     c                   Parm                    Kpjba
396600031124     c                   Parm                    Tibs02Ds
396700031124     c                   If        T02Err = *Blanks
396800031124     c                   Movel     T02Uni        dCat
396900031124     c                   EndIf
397000031124    1 *
397100980514     C                   MOVEL     'F17'         D00OP1
397200091125      * Con l'arrivo delle trattative abbiamo modificato alcuni campi della DS
397300091125      * per una giusta gestione del CAT. non si mette + "S" nel campo
397400091125      * d00dsf in caso di tariffe / offerte e non trattativa
397500091125     C*****              MOVEL     'S'           D00DSF
397600980514     C                   MOVEL     'D'           D00TLA
397700980514     C                   MOVEL     V1CCTR        D00CTR
397800980514     C                   MOVEL     V1CFIE        D00FIE
397900980514     C                   MOVEL     V1CTSP        D00TSP
398000011023     C                   MOVEL     V1CDIV        D00DIV
398100020213     C                   MOVE      V1CNET        D00DPD
398200011030     C                   MOVEL     *BLANKS       D00PRI
398300031124      * propongo data massima tra la data del giorno e quella recuperata
398400031124      * dalla tavella CAT
398500031124     c                   if        dateu8 > §CATdta
398600031124     C                   MOVEL     DATEU8        D00DTC
398700031124     C                   ELSE
398800031124     C                   MOVEL     §CATDTA       D00DTC
398900031124     C                   ENDIF
399000031124      *
399100980514     C                   CALL      'TNTE01C'
399200980514     C                   PARM                    KPJBA
399300980514     C                   PARM                    DSTE00
399400980514     C* D00ERR = "1" --> ERRORE
399500980514     C     D00ERR        IFEQ      '1'
399600980514     C                   SETON                                          28
399700980514     C                   MOVEL     D00MSG        VCCMSG
399800980514     C                   GOTO      SUAGG1
399900980514     C                   ENDIF
400000980902     C                   ENDDO
400100900410     C**
400200920525     C**  CMD8 - M A N I P O L A Z I O N E
400300900410     C**
400400920525     C   KH              DO
400500900410     C                   EXSR      IMPMAN
400600941123     C*
400700900410     C     FOR07         TAG
400800070312     c                   eval      wintftc = *off
400900120713     c                   eval      wintcts = *off
401000940706     C                   EXFMT     TA01D07
401100941123     C* PULIZIA CAMPO MESSAGGIO E RELATIVO INDICATORE (*IN28)
401200941123     C                   CLEAR                   V6CMSG
401300960729     C                   SETOFF                                       289086
401400050616     c                   clear                   h7riga
401500050616     c                   clear                   h7colo
401600941123     C* AZZERO GLI INDICATORI RELATIVI AI MESSAGGI DI ERRORE
401700941123    1C     63            DO        76            I
401800941123     C                   MOVE      '0'           *IN(I)
401900941123    1C                   ENDDO
402000941123     C*
402100120315       //?F3 fine
402200120315     c   kc              GOTO      FINE
402300900410     C* CMD12 - RITORNO
402400940518     C   KL              GOTO      SUAGG1
402500900410     C* CONTROLLO
402600900410     C                   EXSR      CTRMAN
402700941109     C   28              GOTO      FOR07
402800070312      * se int.tariffe particolari riemetto la videata
402900070312     c                   if        wintftc = *on
403000070312     c                   goto      for07
403100070312     c                   endif
403200120713       //?se interrogazione codici tassazione riemetto la videata
403300120713     c                   IF        wintcts
403400120713     c                   goto      for07
403500120713     c                   ENDIF
403600921118     C*
403700920525     C* CMD6 - AGGIORNAMENTO TARIFFA
403800020312     c                   if        *inkf
403900020312     c                   clear                   wsimula
404000020312     C                   EXSR      MANIPO
404100020313     c                   if        wnoagg = *on
404200020313     c                   eval      vccmsg = msg(105)
404300020313     c                   eval      *in28 = *on
404400020312     c                   goto      suagg2
404500020312     c                   endif
404600020312     c                   eval      wsimula = '1'
404700020312     c                   exsr      manipo
404800020312     C                   GOTO      SUAGG2
404900020312     c                   endif
405000900410     C* ENTER - CONTROLLO E RIMANGO NELLA VIDEATA
405100900410     C                   GOTO      FOR07
405200900410     C                   END
405300960523     C**
405400960523     C**  CMD13 - D U P L I C A    T A R I F F A
405500960523     C**
405600960523     C     *INKM         IFEQ      *ON
405700960523     C* PULIZIA CAMPI VIDEATA
405800960523     C                   CLEAR                   V8CLNN
405900960523     C                   CLEAR                   V8CCTN
406000960523     C                   CLEAR                   V8CLNP
406100960523     C                   CLEAR                   V8CCTS
406200960523     C                   CLEAR                   WORP
406300960523     C                   CLEAR                   WORPN
406400960527     C                   CLEAR                   SAVCTN
406500960527     C                   CLEAR                   SAVLNN
406600041222      *
406700041222     c                   clear                   linpar
406800041223     c                   clear                   W_sbflnp
406900960523     C*
407000960523     C     FOR08         TAG
407100960523     C                   EXFMT     TA01D08
407200960523     C* PULIZIA CAMPO MESSAGGIO E RELATIVO INDICATORE (*IN28)
407300960523     C                   CLEAR                   V8CMSG
407400960523     C                   SETOFF                                       287172
407500960523     C                   SETOFF                                       7374
407600960523     C*
407700120315       //?F3 fine
407800120315     c   kc              GOTO      FINE
407900960523     C* CMD12 - RITORNO
408000960523     C   KL              GOTO      SUAGG1
408100041222
408200041222     C* CMD19 _ SOLO PER LA CARTELLO IMPOSTO PIU' LINEE PARTENZA DA CREARE
408300041222     C                   IF        *INKT
408400041222     C                   EXSR      SR_GESLNP
408500041222      * RIEMETTO LA VIDEATA
408600041222     C                   GOTO      FOR08
408700041222     C                   ENDIF
408800041222
408900960523     C* CONTROLLO
409000960523     C                   EXSR      CTRD08
409100960523     C   28              GOTO      FOR08
409200960523     C*
409300960523     C* CMD6 - AGGIORNAMENTO TARIFFA
409400990817     C     *INKF         IFEQ      '1'
409500041222      * duplica --- se ho solo un codice altrimenti elaboro la schiera
409600041222     c                   if        W_sbflnp <> ' '
409700041222     c                   z-add     1             po
409800041223     c                   dow       po<=120
409900041222     c                   if        lnpel(po) > *zeros
410000041222     c                   eval      v8clnn = lnpel(po)
410100041222     c                   exsr      duptar
410200041223     c                   add       1             po
410300041222     c                   iter
410400041222     c                   else
410500041222     c                   leave
410600041222     c                   endif
410700041222     c                   enddo
410800041222
410900041222     c                   else
411000041222      *
411100990817     C                   EXSR      DUPTAR
411200041222
411300041222     c                   endif
411400990817     C*
411500990817     C                   GOTO      SUAGG2
411600990817     C                   ENDIF
411700960523     C* ENTER - CONTROLLO E RIMANGO NELLA VIDEATA
411800960523     C                   GOTO      FOR08
411900960523     C                   ENDIF
412000921118     C*
412100921118     C** SE CANCELLATI RECORD RIEMETTO SUBFILE
412200921118     C   19              GOTO      SUAGG2
412300900410     C**
412400940526     C     V1CRDA        IFNE      0
412500940526     C     V1CRA         ORNE      *BLANKS
412600921118     C                   GOTO      ENDAGG
412700900321     C                   END
412800000000     C** E' STATA DIGITATA UNA CHIAVE DI RICERCA  E
412900000000     C** QUINDI DOPO AVER EFFETTUTATO L'AGGIORNAMENTO
413000000000     C** RICERCO LA CHIAVE E RIEMETTO IL SUBFILE
413100000000     C                   GOTO      SUAGG1
413200921118     C                   END
413300930224     C                   END
413400050701
413500050630      * se F12 e errore non posso tornare indietro devo restare nel
413600050630      * subfile ed eliminare gli scaglioni in più
413700120315      //?stessa cosa per F3
413800120315     c                   If        (*inkl or *inkc) and werr = *On
413900050701     c                             and tiptar = '2'
414000050701     c                   ExSr      sr_ctrtarspe
414100050701
414200050701      * se errore per + scaglioni in tariffa a spedizione imposto
414300050701      * il msg e accendo l'indicatore
414400050701     c                   If        werr = *On
414500050701     c                   Eval      vccmsg = 'Par ' + wlnp + ' Arrivo ' +
414600050701     c                             wcts
414700050701     c                   If        wcap <> *Blanks
414800050701     c                   Eval      vccmsg = %trim(vccmsg) +
414900050701     c                             ' - c.a.p. ' + wcap
415000050701     c                   EndIf
415100050701     c                   Eval      vccmsg = %trim(vccmsg) +
415200050701     c                             ' presenti più scaglioni'
415300050701     c                   Eval      *In28 = *On
415400050701     c                   EndIf
415500050701
415600050701     c   28              goto      suagg1
415700050630     c                   endif
415800110128
415900110128       //?Se F12 ed errore per il rapporto peso volume devo prima
416000110128       //?i dati a video poi posso uscire
416100120315       //?stessa cosa per F3
416200120315     c                   IF        (*inkl or *inkc) and werrRpv and
416300110128     c                             V1Cnet = 'F'
416400110128     c                   exsr      CtrRpv
416500110128       //?Se tariffa FedEx e scaglioni con rapporto peso volume
416600110128       //?diverso emetto messaggio di errore
416700110128     c                   IF        werrRpv
416800110128     c                   eval      VCCmsg = 'Par ' + %editc(wlnpRpv:'X') +
416900110128     c                             ' Arrivo ' + wctsRpv +
417000110128     c                             ' Scaglione ' + %editc(wsglRpv:'4') +
417100110128     c                             ' presente P/V diverso'
417200110128     c                   eval      *in28 = *on
417300110128     c                   goto      suagg1
417400110128     c                   ENDIF
417500110128     c                   ENDIF
417600050701
417700000000     C** ENTER  AGGIORNAMENTO E RIMANGO FERMO
417800050630     C*  RIPETO RIEMPIMENTO SF
417900921118     C     ENDAGG        ENDSR
418000900410     C*
418100930623     C*--- IMPOSTO CAMPI MANIPOLAZIONE -------------------------------*
418200900410     C     IMPMAN        BEGSR
418300900410     C                   MOVE      '+'           VIDAD
418400900410     C                   Z-ADD     0             VIDLIR
418500900530     C                   Z-ADD     0             VIDPEI
418600020312
418700020314     c                   eval      vidtad = 'SI'
418800020312     c                   eval      vidtpt = 'NO'
418900050616     c                   eval      vidtgc = 'NO'
419000020312
419100900410     C                   MOVE      'E'           VIDADE
419200991008     C                   Z-ADD     0             VIDALI
419300940526     C                   MOVE      '+'           VIDRAD
419400910606     C                   Z-ADD     0             VIDVRR
419500050627     c                   Clear                   vidarr
419600061207     c                   clear                   vidirr
419700960523     C                   MOVE      'O'           VIDIOP
419800960523     C                   CLEAR                   VMP
419900930625     C                   MOVE      'O'           VIDIO
420000930628     C                   CLEAR                   VMC
420100930625     C                   MOVE      'O'           VIDIO2
420200940518     C                   Z-ADD     0             VIDSGL
420300070309     c                   eval      vidioftc = 'O'
420400070309     c                   clear                   vftc
420500930625     C                   MOVE      'S'           VIDVTC
420600930625     C                   MOVE      'S'           VIDVTP
420700941124     C                   MOVE      'S'           VIDVMN
420800941124     C                   MOVE      'S'           VIDVMX
420900990624     C* DIVISA
421000990624     C                   MOVEL     V1CDIV        TAMDIV
421100010323     C* SE DIVISA SENZA DECIMALI NON EMETTO LA RIGA RELATIVA ALL'ARROTONDAMENTO
421200010323     C* AI DECIMALI
421300010323     C     §CVFDC        IFNE      'S'
421400010323     C* IMPOSTO I CAMPI A VIDEO PROTETTI E NON VISUALIZZZATI
421500010323     C* NDPR SOSTITUISCE GLI INDICATORI IN QUANTO FINITI
421600010323     C                   MOVEL     NDPR          PROSAD
421700010323     C                   ENDIF
421800010329     C* PULISCO I CAMPI REALTIVI LÌARROTONDAMENTO
421900010329     C                   CLEAR                   VIDSAD
422000010329     C                   CLEAR                   VIDDEC
422100050616
422200050616     c                   clear                   h7riga
422300050616     c                   clear                   h7colo
422400010323     C*
422500900410     C                   ENDSR
422600900410     C*
422700930623     C*--- CONTROLLO CAMPI VIDEO MANIPOLAZIONE -----------------------*
422800900410     C     CTRMAN        BEGSR
422900020312
423000070309      * spengo indicatori che uso in questa videata ma che sono stati
423100070309      * utilizzati in videate precedenti
423200020312     c                   eval      *in82 = *off
423300070309      * imposto subito il campo di comodo delle tariffe particolari xchè
423400070309      * mi serve da subito e non solo nel momento dei controlli delle
423500070309      * singole tariffe particolari
423600070309     c                   movea     vftc          comftc
423700020314
423800020327      * deve essere fatta almeno una scelta
423900020314     c                   if        vidtad = 'NO' and vidtpt = 'NO'
424000050616     c                             and vidtgc = 'NO'
424100020314     c                   eval      v6cmsg = msg(106)
424200020314     c                   eval      *in24 = *on
424300020314     c                   eval      *in28 = *on
424400020314     c                   goto      endman
424500020314     c                   endif
424600020312
424700900606     C                   MOVE      VIDPEI        VIDPER            5 4
424800940526     C*
424900940526     C****  VALORE/PERCENTUALE  ****
425000900410     C     VIDLIR        IFNE      0
425100940526     C     VIDPER        ANDNE     0
425200940526     C                   MOVEL     MSG(33)       V6CMSG
425300940526     C                   SETON                                        6328
425400900410     C                   GOTO      ENDMAN
425500940526     C                   ENDIF
425600050617
425700050617      * Se immesso un importo di aumento/diminuzione non posso manipolare
425800050617      * le tariffe particolari visto che sono miste cioè una parte espresse
425900050617      * ad importo e una parte espresse in %
426000050617      * la manipolazione è possibile solo inserendo una % +/-
426100050617     c                   If        vidlir <> *Zeros and vidtpt = 'SI'
426200050617     c                   Eval      v6cmsg = msg(116)
426300050617     c                   Eval      *In28 = *On
426400050617     c                   Eval      *In82 = *On
426500050617     c                   Goto      endman
426600050617     c                   EndIf
426700050616
426800050617      * rapporto peso/volume immettere o il valore o l'annullamento
426900061207      * o l'inserimento
427000061207     c                   If        (vidvrr <> *Zeros and vidarr <> *blanks) or
427100061207     c                             (vidvrr <> *Zeros and vidirr <> *zeros) or
427200061207     c                             (vidarr <> *blanks and vidirr <> *zeros)
427300050616     c                   Eval      v6cmsg = msg(33)
427400050616     c                   Eval      *In28 = *On
427500050616     c                   z-add     15            H7riga
427600061207     c                   z-add     29            H7colo
427700050616     c                   Goto      endman
427800050616     c                   EndIf
427900900410     C*
428000910606     C     VIDVRR        IFEQ      0
428100940526     C     VIDLIR        ANDEQ     0
428200940526     C     VIDPER        ANDEQ     0
428300050616     c     vidarr        andeq     *Blanks
428400061207     c     vidirr        andeq     *zeros
428500940526     C                   MOVEL     MSG(33)       V6CMSG
428600940526     C                   SETON                                        6328
428700900410     C                   GOTO      ENDMAN
428800940526     C                   ENDIF
428900020312
429000020312      * Manipolazione tariffe particolari
429100020327      * devono essere presenti le tariffe particolari
429200020312     c                   if        vidtpt = 'SI'
429300070129     c  n50ktam2         chain(n)  titpt01l
429400070129     c   50ktam2         chain(n)  tiopt01l
429500061011     c                   if        not %found
429600020312     c                   eval      v6cmsg = msg(104)
429700020312     c                   eval      *in28 = *on
429800020312     c                   eval      *in82 = *on
429900020312     c                   goto      endman
430000020312     c                   endif
430100020312     c                   endif
430200050617
430300050617      * Manipolazione tariffe di giacenza
430400050617      * devono essere presenti le tariffe di giacenza
430500050617     c                   if        vidtgc = 'SI'
430600050913     c  n50ktam2         chain     titgc01l
430700050913     c   50ktam2         chain     tiogc01l
430800050913     c                   if        not %found or tgcatb = 'A'
430900050617     c                   eval      v6cmsg = msg(117)
431000050617     c                   eval      *in28 = *on
431100050617     c                   z-add     10            H7riga
431200050617     c                   z-add     35            H7colo
431300050617     c                   goto      endman
431400050617     c                   endif
431500050617     c                   endif
431600020312
431700930623     C*
431800050616     C* 88 OFF -  TARIFFA NON IN % verifico se si
431900941124     C*   POSSONO INSERIRE DECIMALI
432000941124     C  N88VIDLIR        IFGT      0
432100941124     C                   MOVE      VIDLIR        W0030
432200941124     C     W0030         IFGT      0
432300050616     C*  la DIVISA NON AMMETTE DECIMALI
432400990617     C     §CVFDC        ANDNE     'S'
432500990617     C                   MOVEL     MSG(79)       V6CMSG
432600941124     C                   SETON                                        6328
432700941124     C                   GOTO      ENDMAN
432800941124     C                   ENDIF
432900941124     C                   ENDIF
433000941124     C*
433100050616     C* 88 OFF -  TARIFFA NON IN % verifico se si
433200050616     C*   POSSONO INSERIRE DECIMALI
433300960729     C  N88VIDPEI        IFGT      0
433400960729     C     VIDALI        ANDEQ     0
433500050616     C*  la DIVISA NON AMMETTE DECIMALI
433600010329     C     §CVFDC        ANDNE     'S'
433700960729     C                   MOVEL     MSG(69)       V6CMSG
433800960729     C                   SETON                                        8628
433900960729     C                   GOTO      ENDMAN
434000960729     C                   ENDIF
434100990617     C*
434200050616     C* 88 OFF -  TARIFFA NON IN % verifico se si
434300050616     C*   POSSONO INSERIRE DECIMALI
434400990617     C  N88VIDLIR        IFGT      0
434500990617     C     VIDPEI        ORGT      0
434600990617     C                   MOVE      VIDALI        W0030
434700991008     C                   Z-ADD     VIDALI        W0010
434800990617     C     W0030         IFGT      0
434900050616     C*  la DIVISA NON AMMETTE DECIMALI
435000991008     C     §CVFDC        ANDNE     'S'
435100990617     C                   MOVEL     MSG(79)       V6CMSG
435200990617     C                   SETON                                        8628
435300990617     C                   GOTO      ENDMAN
435400990617     C                   ENDIF
435500991008     C* ULTIMA CIFRA DELL'ARROTONDAMENTO DEVE ESSERE UGUALE A ZERO
435600991008     C* NEL CASO DI MONETA SENZA DECIMALE E' L'ULTIMO INTERO + I DECIMALI
435700991008     C* NEL CASO DI MONETA CON DECIMALI E' L'ULTIMO DECIMALE
435800991008     C     VIDALI        IFNE      *ZEROS
435900991008     C     §CVFDC        IFNE      'S'
436000991008     C* INTERO + DECIMALI
436100991008     C                   MOVE      VIDALI        W0040             4 0
436200991008     C*
436300991008     C     W0040         IFNE      0
436400991008     C                   MOVEL     MSG(83)       V6CMSG
436500991008     C                   SETON                                        8628
436600991008     C                   GOTO      ENDMAN
436700991008     C                   ENDIF
436800991008     C                   ELSE
436900991008     C* DECIMALI
437000991008     C                   MOVE      VIDALI        W0010
437100991008     C*
437200991008     C     W0010         IFNE      0
437300991008     C                   MOVEL     MSG(84)       V6CMSG
437400991008     C                   SETON                                        8628
437500991008     C                   GOTO      ENDMAN
437600991008     C                   ENDIF
437700010323     C*
437800010323     C* SE C'E' ARROTONDAMENTO ANCHE AL DECIMALE  DO ERRORE
437900010323     C*
438000010323     C     VIDSAD        IFEQ      'S'
438100010323     C                   MOVEL     MSG(94)       V6CMSG
438200010323     C                   SETON                                        8628
438300010323     C                   GOTO      ENDMAN
438400010323     C                   ENDIF
438500010323     C*
438600991008     C                   ENDIF
438700991008     C                   ENDIF
438800991008     C*
438900990617     C                   ENDIF
439000960729     C*
439100050616     C   88VIDLIR        IFGT      0
439200960729     C     VIDPEI        ORGT      0
439300990617     C                   MOVE      VIDALI        W0010             1 0
439400990617     C     W0010         IFGT      0
439500990617     C                   MOVEL     MSG(80)       V6CMSG
439600960729     C                   SETON                                        8628
439700960729     C                   GOTO      ENDMAN
439800960729     C                   ENDIF
439900960729     C                   ENDIF
440000960729     C*
440100960523     C* VIDIOP = I  E NON E' STATA INSERITA NESSUNA LINEA PARTENZA
440200960523     C                   XFOOT     VMP           COMVMP            5 0
440300960523     C     VIDIOP        IFEQ      'I'
440400960523     C     COMVMP        ANDEQ     0
440500960523     C                   MOVEL     MSG(60)       V6CMSG
440600960523     C                   SETON                                        9028
440700960523     C                   GOTO      ENDMAN
440800960523     C                   ENDIF
440900960523     C*
441000940526     C* VIDIO = I  E NON E' STATO INSERITO NESSUN CODICE TASSAZIONE
441100930625     C                   MOVEA     VMC           COMVMC           20
441200930623     C     VIDIO         IFEQ      'I'
441300930625     C     COMVMC        ANDEQ     *BLANKS
441400940526     C                   MOVEL     MSG(34)       V6CMSG
441500070309     C                   SETON                                        6628
441600930623     C                   GOTO      ENDMAN
441700930628     C                   ENDIF
441800960522     C*
441900940530     C* VIDIO2 = I  E NON E' STATO INSERITO LO SCAGLIONE
442000930623     C     VIDIO2        IFEQ      'I'
442100940518     C     VIDSGL        ANDEQ     0
442200941123     C                   MOVEL     MSG(35)       V6CMSG
442300940530     C                   SETON                                        6528
442400930623     C                   GOTO      ENDMAN
442500930628     C                   ENDIF
442600110131
442700110131       //?Se tariffa FedEx e Manipolazione rapporto peso volume
442800110131       //?non si può fare nessuna selezione su LNP - CTS - Scaglione
442900110131     c                   IF        V1Cnet = 'F' and (VIDvrr > 0 or
443000110131     c                              VIDarr <> *blanks or VIDirr > 0)
443100110131       //?Inclusione LNP
443200110131     c                   IF        VIDiop = 'I'
443300110131     c                   eval      v6cmsg = msg(118)
443400110131     c                   eval      *in28 = *on
443500110131     c                   z-add     17            H7riga
443600110131     c                   z-add     21            H7colo
443700110131     c                   goto      endman
443800110131     c                   ENDIF
443900110131       //?Omissione LNP
444000110131     c                   IF        comvmp > 0
444100110131     c                   eval      v6cmsg = msg(118)
444200110131     c                   eval      *in28 = *on
444300110131     c                   eval      *in90 = *on
444400110131     c                   goto      endman
444500110131     c                   ENDIF
444600110131       //?Inclusione CTS
444700110131     c                   IF        VIDio  = 'I'
444800110131     c                   eval      v6cmsg = msg(103)
444900110131     c                   eval      *in28 = *on
445000110131     c                   z-add     18            H7riga
445100110131     c                   z-add     21            H7colo
445200110131     c                   goto      endman
445300110131     c                   ENDIF
445400110131       //?Omissione CTS
445500110131     c                   IF        comvmc > *blanks
445600110131     c                   eval      v6cmsg = msg(103)
445700110131     c                   eval      *in28 = *on
445800110131     c                   eval      *in66 = *on
445900110131     c                   goto      endman
446000110131     c                   ENDIF
446100110131       //?Inclusione Scaglione
446200110131     c                   IF        VIDio2 = 'I'
446300110131     c                   eval      v6cmsg = msg(102)
446400110131     c                   eval      *in28 = *on
446500110131     c                   z-add     19            H7riga
446600110131     c                   z-add     21            H7colo
446700110131     c                   goto      endman
446800110131     c                   ENDIF
446900110131       //?Omissione Scaglione
447000110131     c                   IF        VIDsgl > 0
447100110131     c                   eval      v6cmsg = msg(102)
447200110131     c                   eval      *in28 = *on
447300110131     c                   eval      *in65 = *on
447400110131     c                   goto      endman
447500110131     c                   ENDIF
447600110131     c                   ENDIF
447700070309
447800070309      * vidioftc = I  e non è stata inserita nessuna tariffa particolare
447900070309     c                   if        vidtpt = 'SI' and
448000070309     c                             vidioftc = 'I' and comftc = *blanks
448100070309     c                   eval      v6cmsg = msg(38)
448200070309     c                   eval      *in28 = *on
448300070309     c                   eval      *in64 = *on
448400070309     c                   leavesr
448500070309     c                   endif
448600070309      * se immesse delle tariffe particolari e non è stata richiesta la
448700070309      * manipolazione tariffe particolari errore
448800070309     c                   if        vidtpt <> 'SI' and comftc <> *blanks
448900070309     c                   eval      v6cmsg = msg(39)
449000070309     c                   eval      *in28 = *on
449100070309     c                   eval      *in64 = *on
449200070309     c                   leavesr
449300070309     c                   endif
449400930623     C*
449500960523     C****  LINEE PARTENZA  ****
449600960523     C                   DO        5             X
449700960523     C     VMP(X)        IFGT      0
449800960523     C     VMP(X)        CHAIN     AZORG                              34
449900960523     C     *IN34         IFEQ      *ON
450000960523     C     ORGFVA        ORNE      ' '
450100960523     C     ORGFAG        ORNE      'A'
450200960523     C     ORGFAG        ANDNE     'F'
450300020527     c     orgfl2        andne     'S'
450400960523     C                   MOVEA     MSG(61)       K78
450500960523     C                   MOVEL     VMP(X)        ERRLNP            3
450600960523     C                   MOVEA     ERRLNP        K78(19)
450700960523     C                   MOVEA     K78           V6CMSG
450800960523     C                   SETON                                        9028
450900960523     C                   GOTO      ENDMAN
451000960523     C                   ENDIF
451100960523     C                   ENDIF
451200960523     C                   ENDDO
451300960523     C*
451400940526     C****  CODICI TASSAZIONE  ****
451500120713      /free
451600120713       //?Ricerca Codici Tassazione
451700120713         x = 1;
451800120713         FOR x by 1 to 10;
451900120713           IF  %scan ('?': vmc(x)) > *zeros;
452000120713             exsr Ric_CT;
452100120713             vmc(x) = TB09cod;
452200120713             h7riga = 18;
452300120713             h7colo = 28 + (x * 5);
452400120713             wintcts = *on;
452500120713             leavesr;
452600120713           ENDIF;
452700120713         ENDFOR;
452800120713      /end-free
452900960523     C                   DO        10            X                 3 0
453000020611     C                   Z-ADD     1             A
453100900410     C     VMC(X)        IFNE      *BLANKS
453200020207     C     VMC(X)        LOOKUP    CCT(A)                                 35
453300940530     C     *IN35         IFEQ      *OFF
453400941123     C                   MOVEL     MSG(36)       V6CMSG
453500940530     C                   SETON                                        28
453600940530     C                   EXSR      INDIC9
453700940530     C                   GOTO      ENDMAN
453800940530     C                   ENDIF
453900050303      * errore bloccante se codice tassazione non utilizzabile in gestione
454000050303      * tariffe clienti
454100050303     c                   If        uta(a) = 'N'
454200050303     c                   Eval      v6cmsg = msg(36)
454300050303     c                   Eval      *In28 = *On
454400050303     C                   ExSr      indic9
454500050303     c                   Goto      endman
454600050303     c                   EndIf
454700020207      *
454800020207      * Se tariffa FedEx devo utilizzare solo i codici tariffa
454900020207      * previsti per FedEx
455000020207     C     V1CNET        IFEQ      'F'
455100020207     C     UTF(A)        ANDNE     'S'
455200020207     C                   SETON                                        28
455300020207     C                   MOVEL     MSG(100)      V6CMSG
455400020207     C                   EXSR      INDIC9
455500020207     C                   GOTO      ENDMAN
455600020207     C                   ENDIF
455700020207      * Se tariffa non è FedEx non posso utilizzare i codici
455800020207      * tariffa previsti per FedEx
455900020207     C     V1CNET        IFNE      'F'
456000020207     C     UTF(A)        ANDEQ     'S'
456100020207     C                   SETON                                        28
456200020207     C                   MOVEL     MSG(101)      V6CMSG
456300020207     C                   EXSR      INDIC9
456400020207     C                   GOTO      ENDMAN
456500020207     C                   ENDIF
456600940530     C                   ENDIF
456700940530     C                   ENDDO
456800960522     C*
456900950118     C****  SCAGLIONE  ****
457000950118     C     VIDSGL        IFGT      0
457100950119     C* WSAP = " " --> SI TRATTA DI SCAGLIONE SENZA DECIMALI QUINDI NON
457200950118     C*                SI POSSONO INSERIRE VALORI DOPO LA VIRGOLA
457300950118     C     WSAP          ANDEQ     ' '
457400950118     C                   MOVE      VIDSGL        W0030
457500950118     C     W0030         IFGT      0
457600950118     C                   SETON                                        6528
457700950118     C                   MOVEL     MSG(46)       V6CMSG
457800950118     C                   GOTO      ENDMAN
457900950118     C                   ENDIF
458000950118     C                   ENDIF
458100070309
458200070309     c****  TARIFFE PARTICOLARI  ****
458300070312      * ricerca
458400070312     c                   eval      x = 1
458500070309     c                   for       x by 1 to 5
458600070312     c                   if        %scan('?':vftc(x)) > *zeros
458700070312     c                   eval      kcod = '1P'
458800070312     c                   call      'TRUL19R'
458900070312     c                   parm                    kcod
459000070312     c                   parm                    ordina
459100070312     c                   parm                    kkey
459200070312     c                   parm                    comando
459300070312     c                   eval      vftc(x) = kkey
459400070312     c                   eval      h7riga = 20
459500070312     c                   eval      h7colo = 30 + (x * 3)
459600070312     c                   eval      wintftc = *on
459700070312     c                   leavesr
459800070312     c                   endif
459900070312     c                   endfor
460000070312
460100070312      * controllo
460200070312     c                   eval      x = 1
460300070312     c                   for       x by 1 to 5
460400070309     c                   if        vftc(x) <> *blanks
460500070309     c                   eval      wtpar = vftc(x)
460600070412      * se tariffa particolare AC BASE o FUEL errore
460700070412      * tanto non le posso manipolare
460800070412     c                   if        wtpar = 'd' or wtpar = 'f'
460900070412     c                   eval      v6cmsg = msg(41)
461000070412     c                   eval      %subst(v6cmsg:25:1) = vftc(x)
461100070412     c                   eval      *in28 = *on
461200070412     c                   eval      h7riga = 20
461300070412     c                   eval      h7colo = 30 + (x * 3)
461400070412     c                   leavesr
461500070412     c                   endif
461600070309     c  n50ktam7         chain(n)  titpt01l
461700070309     c   50ktam7         chain(n)  tiopt01l
461800070309     c                   if        not %found or tptatb <> *blanks
461900070309     c                   eval      v6cmsg = msg(40)
462000070309     c                   eval      %subst(v6cmsg:22:1) = vftc(x)
462100070309     c                   eval      *in28 = *on
462200070312     c                   eval      h7riga = 20
462300070312     c                   eval      h7colo = 30 + (x * 3)
462400070309     c                   leavesr
462500070309     c                   endif
462600070309     c                   endif
462700070309     c                   endfor
462800950118     C*
462900941124     C****  VARIAZIONE TARIFFA CITTA'/ PROVINCIA / MINIMO / MASSIMO ***
463000900410     C     VIDVTP        IFNE      'S'
463100941124     C     VIDVTC        ANDNE     'S'
463200941124     C     VIDVMN        ANDNE     'S'
463300941124     C     VIDVMX        ANDNE     'S'
463400941123     C                   MOVEL     MSG(37)       V6CMSG
463500940530     C                   SETON                                        7628
463600900410     C                   GOTO      ENDMAN
463700900410     C                   END
463800900410     C*
463900900410     C     ENDMAN        ENDSR
464000900410     C*
464100930623     C*--- ACCENDO INDICATORI ERRORE CODICI TASSAZIONE ---------------*
464200900410     C     INDIC9        BEGSR
464300940530     C     X             COMP      1                                      66
464400940530     C     X             COMP      2                                      67
464500940530     C     X             COMP      3                                      68
464600940530     C     X             COMP      4                                      69
464700940530     C     X             COMP      5                                      70
464800940530     C     X             COMP      6                                      71
464900940530     C     X             COMP      7                                      72
465000940530     C     X             COMP      8                                      73
465100940530     C     X             COMP      9                                      74
465200940530     C     X             COMP      10                                     75
465300900410     C                   ENDSR
465400900410     C*
465500930623     C*--- ESEGUO MANIPOLAZIONE --------------------------------------*
465600900410     C     MANIPO        BEGSR
465700020327
465800020313     C                   eval      wnoagg = *off
465900020313     c                   eval      *in32 = *off
466000020312
466100960528     C                   CLEAR                   WSLNP
466200960528     C                   CLEAR                   WSCTS
466300960528     C                   CLEAR                   WSSGL
466400020313
466500960528     C* WSLNP = "S" --> RICHIESTA ESCLUSIONE DI ALMENO UNA LINEA PARTENZA
466600960528    1C     VIDIOP        IFEQ      'O'
466700960528     C     COMVMP        ANDGT     0
466800960528     C                   MOVEL     'S'           WSLNP             1
466900960528    1C                   ENDIF
467000960528     C* WSCTS = "S" --> RICHIESTA ESCLUSIONE DI ALMENO UN COD.TASSAZIONE
467100960528    1C     VIDIO         IFEQ      'O'
467200960528     C     COMVMC        ANDNE     *BLANKS
467300960528     C                   MOVEL     'S'           WSCTS             1
467400960528    1C                   ENDIF
467500960528     C* WSSGL = "S" --> RICHIESTA ESCLUSIONE DI UNO SCAGLIONE          E
467600960528    1C     VIDIO2        IFEQ      'O'
467700960528     C     VIDSGL        ANDGT     0
467800960528     C                   MOVEL     'S'           WSSGL             1
467900960528    1C                   ENDIF
468000070309      * wsftc = "S" --> richiesta sclusione di almeno una tariffa particolare
468100070309     c                   if        vidioftc = 'O' and comftc <> *blanks
468200070309     c                   eval      wsftc = 'S'
468300070309     c                   endif
468400960528     C*
468500020314      * ---- DETTAGLIO TARIFFE
468600020314 1ab c                   if        vidtad = 'SI'
468700960528     C*
468800960528     C* C I C L O    L E T T U R A
468900061011     C  n50KTAM2         SETLL     TITAD000
469000061011     c   50ktam2         setll     tiofd000
469100061011     C  n50KTAM2         READE     TITAD000                               32
469200061011     c   50ktam2         reade     tiofd000                               32
469300930623     C*
469400941124    1C     *IN32         DOWEQ     *OFF
469500930623     C* ESCLUDO RECORD ANNULLATI
469600941124    2C     TADATB        IFEQ      ' '
469700960528     C                   CLEAR                   WOKLNP
469800960528     C                   CLEAR                   WOKCTS
469900960528     C                   CLEAR                   WOKSGL
470000930624     C*
470100960528     C****  C O N T R O L L O     L E     I N C L U S I O N I  ****
470200960528     C* CONTROLLO SE LA LINEA PARTENZA E' INCLUDERE
470300960528    3C     COMVMP        IFGT      0
470400960527     C     TADLNP        LOOKUP    VMP                                    35
470500960528    4C     *IN35         IFEQ      *ON
470600960528     C                   MOVEL     'S'           WOKLNP            1
470700960528   X4C                   ELSE
470800960527     C* NON E' INCLUSA
470900960528     C  N35VIDIOP        IFEQ      'I'
471000960528     C                   GOTO      LEGTAD
471100960528     C                   ENDIF
471200960528    4C                   ENDIF
471300960528    3C                   ENDIF
471400960527     C*
471500960528     C* CONTROLLO SE IL CODICE TASSAZIONE E' DA INCLUDERE
471600960528    3C     COMVMC        IFNE      *BLANKS
471700940524     C     TADCTS        LOOKUP    VMC                                    35
471800960528    4C     *IN35         IFEQ      *ON
471900960528     C                   MOVEL     'S'           WOKCTS            1
472000960528   X4C                   ELSE
472100900410     C* NON E' INCLUSO
472200960528     C  N35VIDIO         IFEQ      'I'
472300960528     C                   GOTO      LEGTAD
472400960528     C                   ENDIF
472500960528    4C                   ENDIF
472600960528    3C                   ENDIF
472700960522     C*
472800960528     C* CONTROLLO SE LO SCAGLIONE E' DA INCLUDERE
472900960528    3C     VIDSGL        IFGT      0
473000960528    4C     TADSGL        IFEQ      VIDSGL
473100960528     C                   MOVEL     'S'           WOKSGL            1
473200960528   X4C                   ELSE
473300960528     C* NON E' INCLUSO
473400960528     C     TADSGL        IFNE      VIDSGL
473500960528     C     VIDIO2        ANDEQ     'I'
473600960528     C                   GOTO      LEGTAD
473700960528     C                   ENDIF
473800960528    4C                   ENDIF
473900960528    3C                   ENDIF
474000930624     C*
474100960528     C****  C O N T R O L L O     L E     E S C L U S I O N I  ****
474200960528     C* SE TUTTE LE CONDIZIONI SI VERIFICANO AGGIORNO IL RECORD
474300960528   3AC     WSLNP         IFEQ      'S'
474400960528     C     WOKLNP        ANDEQ     ' '
474500960528     C*
474600960528     C     WSCTS         OREQ      'S'
474700960528     C     WOKCTS        ANDEQ     ' '
474800960528     C*
474900960528     C     WSSGL         OREQ      'S'
475000960528     C     WOKSGL        ANDEQ     ' '
475100960528     C*
475200960528     C     WSLNP         OREQ      ' '
475300960528     C     WSCTS         ANDEQ     ' '
475400960528     C     WSSGL         ANDEQ     ' '
475500960528     C*
475600960528     C****  E L A B O R O    I L    R E C O R D  ****
475700020313
475800020313     c                   eval      w_taditr = taditr
475900020313     c                   eval      w_tadino = tadino
476000020313     c                   eval      w_tadmin = tadmin
476100020313     c                   eval      w_tadmax = tadmax
476200020313     c                   eval      w_tadrpv = tadrpv
476300020313
476400020313      * AUMENTO/DIMINUZUIONE CON IMPORTO IN LIRE
476500020313  4b C                   if        vidlir > *zeros
476600020313
476700020313      * VARIAZIONE TARIFFA CITTA'
476800020313  5b C                   IF        vidvtc = 'S'
476900020313  6b C                   IF        vidad = '+'
477000020313     C                   ADD       VIDLIR        w_TADITR
477100020313  6x C                   ELSE
477200020313     C                   SUB       VIDLIR        w_TADITR
477300020313      * SE MINORE NON AGGIORNO LA RIGA
477400020313  b7 c                   if        w_taditr <= *zeros
477500020313     c                   eval      wnoagg = *on
477600020313     C                   GOTO      LEGTAD
477700020313  e7 c                   endif
477800020313  6e C                   END
477900020313  5e C                   END
478000020313
478100020313      * VARIAZIONE TARIFFA PROVINCIA
478200020313  5b C                   IF        vidvtp = 'S' and
478300020313     c                             w_tadino > *zeros
478400020313  6b C                   IF        vidad = '+'
478500020313     C                   ADD       VIDLIR        w_TADINO
478600020313  6x C                   ELSE
478700020313     C                   SUB       VIDLIR        w_TADINO
478800020313      * se minore non aggiorno la riga
478900020313  7b c                   if        w_tadino <= *zeros
479000020313     c                   eval      wnoagg = *on
479100020313     C                   GOTO      LEGTAD
479200020313  7e c                   endif
479300020313  6e C                   END
479400020313  5e C                   END
479500020313
479600020313      * VARIAZIONE IMPORTO MINIMO  TARIFFA
479700020313  5b C                   IF        vidvmn = 'S' and
479800020313     c                             w_tadmin > *zeros
479900020313  6b C                   IF        vidad = '+'
480000020313     C                   ADD       VIDLIR        w_TADMIN
480100020313  6x C                   ELSE
480200020313     C                   SUB       VIDLIR        W_TADMIN
480300020313      * se minore non aggiorno la riga
480400020313  7b c                   if        w_tadmin <= *zeros
480500020313     c                   eval      wnoagg = *on
480600020313     C                   GOTO      LEGTAD
480700020313  7e c                   endif
480800020313  6e c                   endif
480900020313  5e C                   ENDIF
481000020313
481100020313      * VARIAZIONE IMPORTO MASSIMO TARIFFA
481200020313  5b C                   IF        vidvmx = 'S' and
481300020313     c                             w_tadmax > *zeros
481400020313  6b C                   IF        vidad = '+'
481500020313     C                   ADD       VIDLIR        w_TADMAX
481600020313  6x C                   ELSE
481700020313     C                   SUB       VIDLIR        w_TADMAX
481800020313      * se minore non aggiorno la riga
481900020313  7b c                   if        w_tadmax <= *zeros
482000020313     c                   eval      wnoagg = *on
482100020313     C                   GOTO      LEGTAD
482200020313  7e c                   endif
482300020313  6e C                   END
482400020313  5e C                   ENDIF
482500020313
482600020313  4e c                   endif
482700020313
482800020313      * AUMENTO/DIMINUZUIONE CON IMPORTO IN PERCENTUALE
482900020313  4b C                   if        vidper > *zeros
483000020313
483100020313      * VARIAZIONE TARIFFA CITTA'
483200020313  5b C                   IF        vidvtc ='S'
483300020313     C     w_TADITR      MULT(H)   VIDPER        COMITR
483400020313  6b C                   IF        vidad = '+'
483500020313     C                   ADD       COMITR        w_TADITR
483600020313  6x C                   ELSE
483700020313     C                   SUB       COMITR        w_TADITR
483800020313      * se minore non aggiorno la riga
483900020313  7b c                   if        w_taditr <= *zeros
484000020313     c                   eval      wnoagg = *on
484100020313     C                   GOTO      LEGTAD
484200020313  7e c                   endif
484300020313  6e C                   END
484400020313  5e C                   END
484500020313
484600020313      *  VARIAZIONE TARIFFA PROVINCIA
484700020313  5b C                   IF        vidvtp = 'S' and
484800020313     c                             w_tadino > *zeros
484900020313     C     w_TADINO      MULT(H)   VIDPER        COMINO
485000020313  6b C                   IF        vidad = '+'
485100020313     C                   ADD       COMINO        w_TADINO
485200020313  6x C                   ELSE
485300020313     C                   SUB       COMINO        w_TADINO
485400020313      * se minore non aggiorno la riga
485500020313  7b c                   if        w_tadino <= *zeros
485600020313     c                   eval      wnoagg = *on
485700020313     C                   GOTO      LEGTAD
485800020313  7e c                   endif
485900020313  6e c                   endif
486000020313  5e C                   END
486100020313
486200020313      * VARIAZIONE IMPORTO MINIMO  TARIFFA
486300020313  5b C                   IF        vidvmn = 'S' and
486400020313     c                             w_tadmin > *zeros
486500020313     C     w_TADMIN      MULT(H)   VIDPER        WMIN
486600020313  6b C                   IF        vidad = '+'
486700020313     C                   ADD       WMIN          w_TADMIN
486800020313  6x C                   ELSE
486900020313     C                   SUB       WMIN          w_TADMIN
487000020313      * se minore non aggiorno la riga
487100020313  7b c                   if        w_tadmin <= *zeros
487200020313     c                   eval      wnoagg = *on
487300020313     C                   GOTO      LEGTAD
487400020313  7e c                   endif
487500020313  6e c                   endif
487600020313  5e C                   ENDIF
487700020313
487800020313      * VARIAZIONE IMPORTO MASSIMO TARIFFA
487900020313  5b C                   IF        vidvmx = 'S' and
488000020313     c                             w_tadmax > *zeros
488100020313     C     w_TADMAX      MULT(H)   VIDPER        WMAX
488200020313  6b C                   IF        vidad = '+'
488300020313     C                   ADD       WMAX          w_TADMAX
488400020313  6x C                   ELSE
488500020313     C                   SUB       WMAX          w_TADMAX
488600020313      * se minore non aggiorno la riga
488700020313  7b c                   if        w_tadmax <= *zeros
488800020313     c                   eval      wnoagg = *on
488900020313     C                   GOTO      LEGTAD
489000020313  7e c                   endif
489100020313  6e c                   endif
489200020313  5e C                   ENDIF
489300020313
489400020313  4e c                   endif
489500020313
489600020313      * CONTROLLO CHE IL MASSIMO NON SIA INFERIORE DEL MINIMO
489700020313    3C                   IF        w_tadmax > 0 and
489800020313     C                             w_tadmax < w_tadmin
489900020313     C                   eval      wnoagg = *on
490000950120     C                   GOTO      LEGTAD
490100950120    3C                   ENDIF
490200020313
490300020313      * ARROTONDAMENTO
490400960607   3AC     VIDLIR        IFGT      0
490500960607     C     VIDPEI        ORGT      0
490600010323     C*
490700010323     C* ESEGUO GLI ARROTONDAMENTI SOLO SE VIENE RICHIESTO
490800010323     C*
490900010323   3BC     VIDALI        IFGT      0
491000010323     C*
491100900410     C                   Z-ADD     VIDALI        XARROT
491200900410     C                   MOVE      VIDADE        XDE
491300941124     C* TARIFFA CITTA'
491400941124    3C     VIDVTC        IFEQ      'S'
491500020313     C                   Z-ADD     w_TADITR      XVALUE
491600900410     C                   EXSR      XARROX
491700020313     C                   Z-ADD     XVALUE        w_TADITR
491800941124    3C                   END
491900941124     C* TARIFFA PROVINCIA
492000020313    3C                   IF        vidvtp = 'S' and w_tadino > *zeros
492100020313     C                   Z-ADD     w_TADINO      XVALUE
492200900410     C                   EXSR      XARROX
492300020313     C                   Z-ADD     XVALUE        w_TADINO
492400941124    3C                   END
492500941124     C* IMPORTO MINIMO  TARIFFA
492600020313    3C                   IF        vidvmn = 'S' and w_tadmin > *zeros
492700020313     C                   Z-ADD     w_TADMIN      XVALUE
492800941124     C                   EXSR      XARROX
492900020313     C                   Z-ADD     XVALUE        w_TADMIN
493000941124    3C                   END
493100941124     C* IMPORTO MASSIMO TARIFFA
493200020313    3C                   IF        vidvmx = 'S' and w_tadmax > *zeros
493300020313     C                   Z-ADD     w_TADMAX      XVALUE
493400941124     C                   EXSR      XARROX
493500020313     C                   Z-ADD     XVALUE        w_TADMAX
493600941124    3C                   END
493700010323     C*
493800010323  X3BC                   ELSE
493900010323     C* VERIFICO SE E' STATO RICHIESTO ARROTONDAMENTO AI DECIMALI
494000010323   3CC     VIDSAD        IFEQ      'S'
494100010323     C* TARIFFA CITTA'
494200010323    4C     VIDVTC        IFEQ      'S'
494300020313     C                   Z-ADD     w_TADITR      XVALDE           15 3
494400010323     C                   EXSR      XARROD
494500020313     C                   Z-ADD     XVALDE        w_TADITR
494600010323    4C                   END
494700010323     C* TARIFFA PROVINCIA
494800020313    4C                   IF        vidvtp = 'S' and w_tadino > *zeros
494900020313     C                   Z-ADD     w_TADINO      XVALDE
495000010323     C                   EXSR      XARROD
495100020313     C                   Z-ADD     XVALDE        w_TADINO
495200010323    4C                   END
495300010323     C* IMPORTO MINIMO  TARIFFA
495400020313    4C                   IF        vidvmn = 'S' and w_tadmin > *zeros
495500020313     C                   Z-ADD     w_TADMIN      XVALDE
495600010323     C                   EXSR      XARROD
495700020313     C                   Z-ADD     XVALDE        w_TADMIN
495800010323    4C                   END
495900010323     C* IMPORTO MASSIMO TARIFFA
496000020313    4C                   IF        vidvmx = 'S' and w_tadmax > *zeros
496100020313     C                   Z-ADD     w_TADMAX      XVALDE
496200010323     C                   EXSR      XARROD
496300020313     C                   Z-ADD     XVALDE        w_TADMAX
496400010323    4C                   END
496500010323     C*
496600010323   3CC                   END
496700010323     C*
496800010323   3DC                   ENDIF
496900010323     C*
497000960607   3AC                   ENDIF
497100020313
497200020313      * RAPPORTO PESO VOLUME
497300020315     c                   if        vidvrr > 0  and  w_tadrpv > 0
497400941124    3C     VIDRAD        IFEQ      '+'
497500020313     C                   ADD       VIDVRR        w_TADRPV
497600941124   X3C                   ELSE
497700020313     C                   SUB       VIDVRR        w_TADRPV
497800020313      * se minore non aggiorno la riga
497900020313     c                   if        w_tadrpv <= *zeros
498000020313     c                   eval      wnoagg = *on
498100020313     c                   goto      legtad
498200020315     c                   endif
498300941124    3C                   END
498400020313     c                   endif
498500050616      * annullo il rapporto peso volume
498600050617     c                   If        vidarr = 'SI' and w_tadrpv > 0
498700050617     c                   Clear                   w_tadrpv
498800050616     c                   EndIf
498900061207      * se inserimento imposto il rapporto peso volume
499000061207     c                   if        vidirr > 0
499100061207     c                   z-add     vidirr        w_tadrpv
499200061207     c                   endif
499300941124     C*
499400980219     C* VALORE POSITIVO NEI CAMPI
499500020313     C                   MLLZO     1             w_TADITR
499600020313     C                   MLLZO     1             w_TADINO
499700020313     C                   MLLZO     1             w_TADRPV
499800020313     C                   MLLZO     1             w_TADMIN
499900020313     C                   MLLZO     1             w_TADMAX
500000020312
500100020312      * controllo se i campi aggiornati superano il valore massimo
500200020313     c                   if        w_taditr > 99999999,999 or
500300020313     c                             w_tadino > 99999999,999 or
500400020313     c                             w_tadmin > 99999999,999 or
500500020313     c                             w_tadmax > 99999999,999 or
500600020313     c                             w_tadrpv > 99,9
500700020313     c                   eval      wnoagg = *on
500800020312     c                   goto      legtad
500900020312     c                   endif
501000980219     C*
501100900410     C* AGGIORNO DETTAGLIO
501200020312     c                   if        wsimula = '1'
501300020313
501400020313     c                   eval      taditr = w_taditr
501500020313     c                   eval      tadino = w_tadino
501600020313     c                   eval      tadmin = w_tadmin
501700020313     c                   eval      tadmax = w_tadmax
501800020313     c                   eval      tadrpv = w_tadrpv
501900020313
502000130124     C******             Z-ADD     dateu8        TADDTR
502100940511     C                   MOVEL     *BLANKS       TADFTR
502200061011     C  n50              UPDATE    TITAD000
502300061011     c   50              update    tiofd000
502400030714      * valorizzo il flag per la scrittura dello storico variazioni
502500030714     c                   eval      mandet = 'S'
502600030714      *
502700020312     c                   endif
502800900410     C     LEGTAD        TAG
502900960528   3AC                   ENDIF
503000941124    2C                   END
503100930623     C*
503200061011     C  n50KTAM2         READE     TITAD000                               32
503300061011     c   50ktam2         reade     tiofd000                               32
503400020312      * se errore esco dalla lettura
503500020313     c                   if        wnoagg = *on
503600020312     c                   eval      *in32 = *on
503700020312     c                   endif
503800020312
503900941124    1C                   END
504000020314
504100020314 1ae c                   endif
504200020312
504300020312      * Manipolazione tariffe particolari
504400020312     c                   if        vidtpt = 'SI'
504500020312     c                   exsr      manipo_tpt
504600020312     c                   endif
504700050616
504800050616      * Manipolazione tariffe di giacenza
504900050616     c                   if        vidtgc = 'SI'
505000050616     c                   exsr      manipo_tgc
505100050616     c                   endif
505200020312
505300020312     c                   if        wsimula = '1'
505400020312
505500930623     C*
505600900410     C* AGGIORNO DATA ULTIMA VARIAZIONE MASTER
505700061011     C  n50KTAM2         CHAIN     TNTAM000                           33
505800061011     c   50ktam2         chain     tnofm000                           33
505900990608     C*-------------------------------------------------------------------------
506000941221     C  N33              Z-ADD     DATEU8        TAMDUV
506100040913     C  n33              Z-ADD     dateu8        TAMDTR
506200990608     C*-------------------------------------------------------------------------
506300061011     C  N33
506400061011     Cann50              UPDATE    TNTAM000
506500061011     c  n33
506600061011     can 50              update    tnofm000
506700020312     c                   endif
506800930623     C                   ENDSR
506900020312
507000020312      *---------------------------------------------------------------*
507100020312      * Manipolazione tariffe particolari
507200020312      *---------------------------------------------------------------*
507300020312     c     manipo_tpt    begsr
507400020312
507500020313     c                   eval      wnoagg = *off
507600020312
507700020312      * Testata tariffe particolari
507800061011     c  n50ktam2         setll     titpt01l
507900061011     c   50ktam2         setll     tiopt01l
508000020312  b1 c                   do        *hival
508100061011     c  n50ktam2         reade     titpt01l
508200061011     c   50ktam2         reade     tiopt01l
508300020312      * fine lettura
508400061011     c                   if        %eof
508500020312     c                   leave
508600020312     c                   endif
508700020312      * escludo record annullati
508800020312     c                   if        tptatb <> *blanks
508900020312     c                   iter
509000020312     c                   endif
509100070410      * escludo AC BASE e FUEL (non si possono manipolare)
509200070410     c                   if        tptftc = 'd ' or tptftc = 'f '
509300070410     c                   iter
509400070410     c                   endif
509500070309
509600070309     c                   clear                   wokftc
509700070309
509800070309      ****  C O N T R O L L O     L E     I N C L U S I O N I  ****
509900070309      * controllo se la tariffa particolare è da includere
510000070309     c                   if        comftc <> *blanks
510100070309     c                   eval      wtptftc = tptftc
510200070309     c     wtptftc       lookup    vftc                                   35
510300070309     c                   if        *in35
510400070309     c                   eval      wokftc = 'S'
510500070309     c                   else
510600070309      * non è inlcusa
510700070309     c                   if        not *in35 and vidioftc = 'I'
510800070309     c                   iter
510900070309     c                   endif
511000070309     c                   endif
511100070309     c                   endif
511200070309      ****  C O N T R O L L O     L E     E S C L U S I O N I  ****
511300070309   1ac                   if        wsftc = 'S' and wokftc = *blanks
511400070309     c                             or wsftc = *blanks
511500070309
511600050628      * aggancio la tabella solo x controllare se la tariffa gestisce il
511700050628      * rapporto peso volume, non controllo + se gestita a valore
511800050628     c                   clear                   key
511900050628     c                   eval      cod = 'TR'
512000050628     c                   eval      key = tpttpg
512100050628     c     ktab          chain     tabel00f
512200050628     c                   if        %found (tabel00f)
512300050628     c                   movel     tbluni        dstr
512400050628     c                   endif
512500020312
512600020312      * Dettaglio tariffe particolari
512700061011     c  n50ktpd          setll     titpd01l
512800061011     c   50ktpd          setll     tiopd01l
512900020312  b2 c                   do        *hival
513000061011     c  n50ktpd          reade     titpd01l
513100061011     c   50ktpd          reade     tiopd01l
513200020312      * fine lettura
513300061011     c                   if        %eof
513400020312     c                   leave
513500020312     c                   endif
513600020312      * escludo record annullati
513700020312     c                   if        tpdatb <> *blanks
513800020312     c                   iter
513900020312     c                   endif
514000020314
514100020314      * ---- Controllo le inclusioni
514200020314     c                   clear                   wokcts
514300020314     c                   clear                   woksgl
514400020312
514500020312      * codice tassazione da includere
514600020312  b3 c                   if        comvmc <> *blanks
514700020312     c     tpdcts        lookup    vmc                                    35
514800020312  b4 c                   if        *in35
514900020312     c                   eval      wokcts = 'S'
515000020312  x4 c                   else
515100020312      * non è incluso
515200020312  b5 c                   if        vidio = 'I'
515300020312     c                   iter
515400020312  e5 c                   endif
515500020312  e4 c                   endif
515600020312  e3 c                   endif
515700020312      * scaglione da includere
515800020312  b3 c                   if        vidsgl > *zeros
515900020312  b4 c                   if        tpdsgl = vidsgl
516000020312     c                   eval      woksgl = 'S'
516100020312  x4 c                   else
516200020312      * non è incluso
516300020312  b5 c                   if        tpdsgl <> vidsgl and
516400020312     c                             vidio2 = 'I'
516500020312     c                   iter
516600020312  e5 c                   endif
516700020312  e4 c                   endif
516800020312  e3 c                   endif
516900020312      * ---- Controllo le esclusioni
517000020312  b3 c                   if        (wscts = 'S' and wokcts = *blanks) or
517100020312     c                             (wssgl = 'S' and woksgl = *blanks) or
517200020312     c                             (wscts = *blanks and wssgl = *blanks)
517300020312      * ---- Elabora tariffa particolare letta
517400020313
517500020313     c                   eval      w_tpditr = tpditr
517600020313     c                   eval      w_tpdmin = tpdmin
517700020313     c                   eval      w_tpdmax = tpdmax
517800020313
517900020312      * ---- Aumento/Diminuzione con importo
518000020312  b4 c                   if        vidlir > 0
518100020313
518200020312      * Variazione Tariffa
518300020315  b5 c                   if        vidvtc = 'S' and w_tpditr > *zeros
518400020312      * aumento
518500020312  b6 c                   if        vidad ='+'
518600020313     c                   add       vidlir        w_tpditr
518700020312  x6 c                   else
518800020312      * diminuisco
518900020327     c                   sub       vidlir        w_tpditr
519000020313      * se minore non aggiorno
519100020313  b7 c                   if        w_tpditr <= *zeros
519200020313     c                   eval      wnoagg = *on
519300020312     c                   leave
519400020313  e7 c                   endif
519500020312  e6 c                   endif
519600020312  e5 c                   endif
519700020312      * Variazione Tariffa minima
519800020313  b5 c                   if        vidvmn = 'S' and w_tpdmin > 0
519900020312      * aumento
520000020313  b6 c                   if        vidad ='+'
520100020313     c                   add       vidlir        w_tpdmin
520200020312  x6 c                   else
520300020312      * diminuisco
520400020313     c                   sub       vidlir        w_tpdmin
520500020313      * se minore non aggiorno
520600020313  b7 c                   if        w_tpdmin <= *zeros
520700020313     c                   eval      wnoagg = *on
520800020312     c                   leave
520900020313  e7 c                   endif
521000020312  e6 c                   endif
521100020312  e5 c                   endif
521200020312      * Variazione Tariffa massima
521300020313  b5 c                   if        vidvmx = 'S' and w_tpdmax > *zeros
521400020312      * aumento
521500020312  b6 c                   if        vidad ='+'
521600020313     c                   add       vidlir        w_tpdmax
521700020312  x6 c                   else
521800020312      * diminuisco
521900020313     c                   sub       vidlir        w_tpdmax
522000020313      * se minore non aggiorno
522100020313  b7 c                   if        w_tpdmax <= *zeros
522200020313     c                   eval      wnoagg = *on
522300020312     c                   leave
522400020312  e7 c                   endif
522500020312  e6 c                   endif
522600020312  e5 c                   endif
522700020312
522800020313  e4 c                   endif
522900020313
523000020312      * ---- Aumento/Diminuzione in percentuale
523100020313  4b c                   if        vidper > *zeros
523200020313
523300020312      * Variazione Tariffa
523400020315  b5 c                   if        vidvtc = 'S' and w_tpditr > *zeros
523500020313     c                   eval(h)   w_itr = w_tpditr * vidper
523600020312      * aumento
523700020312  b6 c                   if        vidad ='+'
523800020313     c                   add       w_itr         w_tpditr
523900020312  x6 c                   else
524000020312      * diminuisco
524100020313     c                   sub       w_itr         w_tpditr
524200020313      * se minore non aggiorno
524300020313  b7 c                   if        w_tpditr <= *zeros
524400020313     c                   eval      wnoagg = *on
524500020312     c                   leave
524600020312  e7 c                   endif
524700020312  e6 c                   endif
524800020312  e5 c                   endif
524900020312      * Variazione Tariffa minima
525000020313  b5 c                   if        vidvmn = 'S' and w_tpdmin > *zeros
525100020313     c                   eval(h)   w_min = w_tpdmin * vidper
525200020312      * aumento
525300020313  b6 c                   if        vidad ='+'
525400020313     c                   add       w_min         w_tpdmin
525500020313  x6 c                   else
525600020312      * diminuisco
525700020313     c                   sub       w_min         w_tpdmin
525800020313      * se minore non aggiorno
525900020313  b7 c                   if        w_tpdmin <= *zeros
526000020313     c                   eval      wnoagg = *on
526100020312     c                   leave
526200020312  e7 c                   endif
526300020312  e6 c                   endif
526400020312  e5 c                   endif
526500020312      * Variazione Tariffa massima
526600020313  b5 c                   if        vidvmx = 'S' and w_tpdmax > *zeros
526700020313     c                   eval(h)   w_max = w_tpdmax * vidper
526800020312      * aumento
526900020313  b6 c                   if        vidad ='+'
527000020313     c                   add       w_max         w_tpdmax
527100020313  x6 c                   else
527200020312      * diminuisco
527300020313     c                   sub       w_max         w_tpdmax
527400020313      * se minore non aggiorno
527500020313  b7 c                   if        w_tpdmax <= *zeros
527600020313     c                   eval      wnoagg = *on
527700020312     c                   leave
527800020312  e7 c                   endif
527900020312  e6 c                   endif
528000020312  e5 c                   endif
528100020312
528200020312  e4 c                   endif
528300020312
528400020312      * Controllo che il massimo non sia inferiore del minimo
528500020313  b4 c                   if        w_tpdmax > *zeros and w_tpdmax < tpdmin
528600020313     c                   eval      wnoagg = *on
528700020312     c                   leave
528800020312  e4 c                   endif
528900020312
529000020312      * ---- Arrotondamenti
529100020312  b4 c                   if        vidlir > *zeros or vidpei > *zeros
529200020312      * Arrotondamento
529300020312  b5 c                   if        vidali > *zeros
529400020312     c                   z-add     vidali        xarrot
529500020312     c                   move      vidade        xde
529600020312      * Tariffa
529700020327  b6 c                   if        vidvtc = 'S' and w_tpditr > *zeros
529800020313     c                   z-add     w_tpditr      xvalue
529900020312     c                   exsr      xarrox
530000020313     c                   z-add     xvalue        w_tpditr
530100020312  e6 c                   endif
530200020312      * Tariffa minima
530300020313  b6 c                   if        vidvmn = 'S' and w_tpdmin > *zeros
530400020313     c                   z-add     w_tpdmin      xvalue
530500020312     c                   exsr      xarrox
530600020313     c                   z-add     xvalue        w_tpdmin
530700020312  e6 c                   endif
530800020312      * Tariffa massima
530900020313  b6 c                   if        vidvmx = 'S' and w_tpdmax > *zeros
531000020313     c                   z-add     w_tpdmax      xvalue
531100020312     c                   exsr      xarrox
531200020313     c                   z-add     xvalue        w_tpdmax
531300020312  e6 c                   endif
531400020312  e5 c                   endif
531500020312      * Arrotondamento ai decimali
531600020312  b5 c                   if        vidsad = 'S'
531700020312      * Tariffa
531800020327  b6 c                   if        vidvtc = 'S' and w_tpditr > *zeros
531900020313     c                   z-add     w_tpditr      xvalde
532000020312     c                   exsr      xarrod
532100020313     c                   z-add     xvalde        w_tpditr
532200020312  e6 c                   endif
532300020312      * Tariffa minima
532400020313  b6 c                   if        vidvmn = 'S' and w_tpdmin > *zeros
532500020313     c                   z-add     w_tpdmin      xvalde
532600050628     c                   exsr      xarrod
532700020313     c                   z-add     xvalde        w_tpdmin
532800020312  e6 c                   endif
532900020312      * Tariffa massima
533000020313  b6 c                   if        vidvmx = 'S' and w_tpdmax > *zeros
533100020313     c                   z-add     w_tpdmax      xvalde
533200050628     c                   exsr      xarrod
533300020313     c                   z-add     xvalde        w_tpdmax
533400020312  e6 c                   endif
533500020312  e5 c                   endif
533600020312  e4 c                   endif
533700020312
533800020312      * valori positivi nei campi
533900020313     c                   mllzo     1             w_tpditr
534000020313     c                   mllzo     1             w_tpdmin
534100020313     c                   mllzo     1             w_tpdmax
534200020312
534300020312      * controllo se i campi aggiornati superano il valore massimo
534400020313     c                   if        w_tpditr > 99999999,999 or
534500020313     c                             w_tpdmin > 99999999,999 or
534600020313     c                             w_tpdmax > 99999999,999
534700020313     c                   eval      wnoagg = *on
534800020312     c                   leave
534900020312     c                   endif
535000020312
535100020312      * aggiorno dettaglio tariffe particolari
535200020312     c                   if        wsimula = '1'
535300020313
535400020313     c                   eval      tpditr = w_tpditr
535500020313     c                   eval      tpdmin = w_tpdmin
535600020313     c                   eval      tpdmax = w_tpdmax
535700020313
535800040914     c                   z-add     dateu8        tpddtr
535900020312     c                   clear                   tpdftr
536000061011     c  n50              update    titpd000
536100061011     c   50              update    tiopd000
536200020312     c                   endif
536300020312
536400020312  e3 c                   endif
536500020312
536600020312  e2 c                   enddo
536700020312
536800020313     c                   if        wnoagg = *on
536900020312     c                   leave
537000020312     c                   endif
537100020312
537200020313     c                   eval      w_tptrpv = tptrpv
537300020313
537400020312      * ---- Rapporto peso volume
537500020313  b2 c                   if        vidvrr > *zeros and §trrpv = 'S'
537600020315     c                             and w_tptrpv > 0
537700020312  b3 c                   if        vidrad = '+'
537800020313     c                   add       vidvrr        w_tptrpv
537900020312  x3 c                   else
538000020313     c                   sub       vidvrr        w_tptrpv
538100020313      * se minore non aggiorno
538200020315  b4 c                   if        w_tptrpv <= *zeros
538300020313     c                   eval      wnoagg = *on
538400020312     c                   leave
538500020315  e4 c                   endif
538600020312  e3 c                   endif
538700020312  e2 c                   endif
538800050616      * annullo i rapporto peso volume
538900050616     c                   If        vidarr = 'SI' and §trrpv = 'S'
539000050616     c                             and w_tptrpv > *Zeros
539100050616     c                   Clear                   w_tptrpv
539200050616     c                   EndIf
539300061207      * se inserimento imposto il rapporto peso volume
539400061207     c                   if        vidirr > 0 and §trrpv = 'S'
539500061207     c                   z-add     vidirr        w_tptrpv
539600061207     c                   endif
539700020312
539800020313     c                   mllzo     1             w_tptrpv
539900020312      * controllo se i campi aggiornati superano il valore massimo
540000020313     c                   if        w_tptrpv > 99,9
540100020313     c                   eval      wnoagg = *on
540200020312     c                   leave
540300020312     c                   endif
540400020312
540500020312      * aggiorno testata tariffe particolari
540600020312     c                   if        wsimula = '1'
540700020313
540800020313     c                   eval      tptrpv = w_tptrpv
540900020313
541000040914     c                   z-add     dateu8        tptdtr
541100020312     c                   clear                   tptftr
541200020312     c                   z-add     dateu8        tptduv
541300061011     c  n50              update    titpt000
541400061011     c   50              update    tiopt000
541500030714      * verifico se devo caricare la schiera per la scrittura dello storico
541600030714      * variazioni
541700030714     c                   movel(p)  tptftc        w_manftc
541800030714     c                   move      'M'           W_manftc
541900030714     c                   z-add     1             WW
542000130124     c     w_manftc      lookup    manftc                                 35
542100030714     c                   if        not %found
542200130124     c     '   '         lookup    manftc(ww)                             35
542300030714     c                   if        %found
542400030714     c                   movea     w_manftc      manftc(ww)
542500030714     c                   endif
542600030714     c                   endif
542700030714      *
542800020312     c                   endif
542900020312
543000070309   1ac                   endif
543100020312  e1 c                   enddo
543200020312
543300020312     c                   endsr
543400050616
543500050616      *---------------------------------------------------------------*
543600050616      * Manipolazione tariffe di giacenza
543700050616      *---------------------------------------------------------------*
543800050616     c     manipo_tgc    begsr
543900050616
544000050616     c                   eval      wnoagg = *off
544100050616
544200050616      * Tariffe di giacenza
544300050913     c  n50ktam2         Chain     Titgc01l
544400050913     c   50ktam2         Chain     Tiogc01l
544500050913if  1c                   If        %found and tgcatb = *Blanks
544600050616      * ---- Elabora tariffa di giacenza letta
544700050616     c                   eval      w_tgcsgv = tgcsgv
544800050616     c                   eval      w_tgcsgr = tgcsgr
544900050616     c                   eval      w_tgcsgp = tgcsgp
545000050616     c                   eval      w_tgcsgd = tgcsgd
545100050616     c                   eval      w_tgcst1 = tgcst1
545200050616     c                   eval      w_tgcst2 = tgcst2
545300050616     c                   eval      w_tgcst3 = tgcst3
545400050616     c                   eval      w_tgcstm = tgcstm
545500050616     c                   eval      w_tgcrpv = tgcrpv
545600050616
545700050616      * ---- Aumento/Diminuzione con importo
545800050617if  2c                   if        vidlir > 0
545900050616
546000050617      * Variazione spese varie giacenza
546100050617if  3c                   if        w_tgcsgv > *Zeros
546200050616      * aumento
546300050617     c                   if        vidad ='+'
546400050617     c                   add       vidlir        w_tgcsgv
546500050617     c                   else
546600050616      * diminuisco
546700050617     c                   sub       vidlir        w_tgcsgv
546800050616      * se minore non aggiorno
546900050617     c                   if        w_tgcsgv <= *zeros
547000050616     c                   eval      wnoagg = *on
547100050617     c                   leavesr
547200050617     c                   endif
547300050617     c                   endif
547400050617    3c                   endif
547500050617      * Variazione spese riconsegna città
547600050617if  3c                   if        w_tgcsgr > *Zeros
547700050616      * aumento
547800050617     c                   if        vidad ='+'
547900050617     c                   add       vidlir        w_tgcsgr
548000050617     c                   else
548100050616      * diminuisco
548200050617     c                   sub       vidlir        w_tgcsgr
548300050616      * se minore non aggiorno
548400050617     c                   if        w_tgcsgr <= *zeros
548500050616     c                   eval      wnoagg = *on
548600050617     c                   leavesr
548700050617     c                   endif
548800050617     c                   endif
548900050617    3c                   endif
549000050617      * Variazione spese riconsegna provincia
549100050617if  3c                   if        w_tgcsgp > *Zeros
549200050616      * aumento
549300050617     c                   if        vidad ='+'
549400050617     c                   add       vidlir        w_tgcsgp
549500050617     c                   else
549600050616      * diminuisco
549700050617     c                   sub       vidlir        w_tgcsgp
549800050616      * se minore non aggiorno
549900050617     c                   if        w_tgcsgp <= *zeros
550000050616     c                   eval      wnoagg = *on
550100050617     c                   leavesr
550200050617     c                   endif
550300050617     c                   endif
550400050617    3c                   endif
550500050617      * Variazione spese dossier giacenza
550600050617if  3c                   if        w_tgcsgd > *Zeros
550700050617      * aumento
550800050617     c                   if        vidad ='+'
550900050617     c                   add       vidlir        w_tgcsgd
551000050617     c                   else
551100050617      * diminuisco
551200050617     c                   sub       vidlir        w_tgcsgd
551300050617      * se minore non aggiorno
551400050617     c                   if        w_tgcsgd <= *zeros
551500050617     c                   eval      wnoagg = *on
551600050617     c                   leavesr
551700050617     c                   endif
551800050617     c                   endif
551900050617    3c                   endif
552000050617      * Variazione 1° scaglione tariffa sosta
552100050617if  3c                   if        w_tgcst1 > *Zeros
552200050617      * aumento
552300050617     c                   if        vidad ='+'
552400050617     c                   add       vidlir        w_tgcst1
552500050617     c                   else
552600050617      * diminuisco
552700050617     c                   sub       vidlir        w_tgcst1
552800050617      * se minore non aggiorno
552900050617     c                   if        w_tgcst1 <= *zeros
553000050617     c                   eval      wnoagg = *on
553100050617     c                   leavesr
553200050617     c                   endif
553300050617     c                   endif
553400050617    3c                   endif
553500050617      * Variazione 2° scaglione tariffa sosta
553600050617if  3c                   if        w_tgcst2 > *Zeros
553700050617      * aumento
553800050617     c                   if        vidad ='+'
553900050617     c                   add       vidlir        w_tgcst2
554000050617     c                   else
554100050617      * diminuisco
554200050617     c                   sub       vidlir        w_tgcst2
554300050617      * se minore non aggiorno
554400050617     c                   if        w_tgcst2 <= *zeros
554500050617     c                   eval      wnoagg = *on
554600050617     c                   leavesr
554700050617     c                   endif
554800050617     c                   endif
554900050617    3c                   endif
555000050617      * Variazione 3° scaglione tariffa sosta
555100050617if  3c                   if        w_tgcst3 > *Zeros
555200050617      * aumento
555300050617     c                   if        vidad ='+'
555400050617     c                   add       vidlir        w_tgcst3
555500050617     c                   else
555600050617      * diminuisco
555700050617     c                   sub       vidlir        w_tgcst3
555800050617      * se minore non aggiorno
555900050617     c                   if        w_tgcst3 <= *zeros
556000050617     c                   eval      wnoagg = *on
556100050617     c                   leavesr
556200050617     c                   endif
556300050617     c                   endif
556400050617    3c                   endif
556500050617      * Variazione tariffa minima sosta
556600050617if  3c                   if        w_tgcstm > *Zeros
556700050617      * aumento
556800050617     c                   if        vidad ='+'
556900050617     c                   add       vidlir        w_tgcstm
557000050617     c                   else
557100050617      * diminuisco
557200050617     c                   sub       vidlir        w_tgcstm
557300050617      * se minore non aggiorno
557400050617     c                   if        w_tgcstm <= *zeros
557500050617     c                   eval      wnoagg = *on
557600050617     c                   leavesr
557700050617     c                   endif
557800050617     c                   endif
557900050617    3c                   endif
558000050616
558100050617    2c                   endif
558200050616
558300050617      * ---- Aumento/Diminuzione in percentuale
558400050617if  2c                   if        vidper > *zeros
558500050617
558600050617      * Variazione spese varie giacenza
558700050617if  3c                   if        w_tgcsgv > *Zeros
558800050617     c                   eval(h)   w_sgv = w_tgcsgv * vidper
558900050617      * aumento
559000050617     c                   if        vidad ='+'
559100050617     c                   add       w_sgv         w_tgcsgv
559200050617     c                   else
559300050617      * diminuisco
559400050617     c                   sub       w_sgv         w_tgcsgv
559500050617      * se minore non aggiorno
559600050617     c                   if        w_tgcsgv <= *zeros
559700050617     c                   eval      wnoagg = *on
559800050617     c                   leavesr
559900050617     c                   endif
560000050617     c                   endif
560100050617    3c                   endif
560200050617      * Variazione spese riconsegna città
560300050617if  3c                   if        w_tgcsgr > *Zeros
560400050617     c                   eval(h)   w_sgr = w_tgcsgr * vidper
560500050617      * aumento
560600050617     c                   if        vidad ='+'
560700050617     c                   add       w_sgr         w_tgcsgr
560800050617     c                   else
560900050617      * diminuisco
561000050617     c                   sub       w_sgr         w_tgcsgr
561100050617      * se minore non aggiorno
561200050617     c                   if        w_tgcsgr <= *zeros
561300050617     c                   eval      wnoagg = *on
561400050617     c                   leavesr
561500050617     c                   endif
561600050617     c                   endif
561700050617    3c                   endif
561800050617      * Variazione spese riconsegna provincia
561900050617if  3c                   if        w_tgcsgp > *Zeros
562000050617     c                   eval(h)   w_sgp = w_tgcsgp * vidper
562100050617      * aumento
562200050617     c                   if        vidad ='+'
562300050617     c                   add       w_sgp         w_tgcsgp
562400050617     c                   else
562500050617      * diminuisco
562600050617     c                   sub       w_sgp         w_tgcsgp
562700050617      * se minore non aggiorno
562800050617     c                   if        w_tgcsgp <= *zeros
562900050617     c                   eval      wnoagg = *on
563000050617     c                   leavesr
563100050617     c                   endif
563200050617     c                   endif
563300050617    3c                   endif
563400050617      * Variazione spese dossier giacenza
563500050617if  3c                   if        w_tgcsgd > *Zeros
563600050617     c                   eval(h)   w_sgd = w_tgcsgd * vidper
563700050617      * aumento
563800050617     c                   if        vidad ='+'
563900050617     c                   add       w_sgd         w_tgcsgd
564000050617     c                   else
564100050617      * diminuisco
564200050617     c                   sub       w_sgd         w_tgcsgd
564300050617      * se minore non aggiorno
564400050617     c                   if        w_tgcsgd <= *zeros
564500050617     c                   eval      wnoagg = *on
564600050617     c                   leavesr
564700050617     c                   endif
564800050617     c                   endif
564900050617    3c                   endif
565000050617      * Variazione 1° scaglione tariffa sosta
565100050617if  3c                   if        w_tgcst1 > *Zeros
565200050617     c                   eval(h)   w_st1 = w_tgcst1 * vidper
565300050617      * aumento
565400050617     c                   if        vidad ='+'
565500050617     c                   add       w_st1         w_tgcst1
565600050617     c                   else
565700050617      * diminuisco
565800050617     c                   sub       w_st1         w_tgcst1
565900050617      * se minore non aggiorno
566000050617     c                   if        w_tgcst1 <= *zeros
566100050617     c                   eval      wnoagg = *on
566200050617     c                   leavesr
566300050617     c                   endif
566400050617     c                   endif
566500050617    3c                   endif
566600050617      * Variazione 2° scaglione tariffa sosta
566700050617if  3c                   if        w_tgcst2 > *Zeros
566800050617     c                   eval(h)   w_st2 = w_tgcst2 * vidper
566900050617      * aumento
567000050617     c                   if        vidad ='+'
567100050617     c                   add       w_st2         w_tgcst2
567200050617     c                   else
567300050617      * diminuisco
567400050617     c                   sub       w_st2         w_tgcst2
567500050617      * se minore non aggiorno
567600050617     c                   if        w_tgcst2 <= *zeros
567700050617     c                   eval      wnoagg = *on
567800050617     c                   leavesr
567900050617     c                   endif
568000050617     c                   endif
568100050617    3c                   endif
568200050617      * Variazione 3° scaglione tariffa sosta
568300050617if  3c                   if        w_tgcst3 > *Zeros
568400050617     c                   eval(h)   w_st3 = w_tgcst3 * vidper
568500050617      * aumento
568600050617     c                   if        vidad ='+'
568700050617     c                   add       w_st3         w_tgcst3
568800050617     c                   else
568900050617      * diminuisco
569000050617     c                   sub       w_st3         w_tgcst3
569100050617      * se minore non aggiorno
569200050617     c                   if        w_tgcst3 <= *zeros
569300050617     c                   eval      wnoagg = *on
569400050617     c                   leavesr
569500050617     c                   endif
569600050617     c                   endif
569700050617    3c                   endif
569800050617      * Variazione tariffa mimina sosta
569900050617if  3c                   if        w_tgcstm > *Zeros
570000050617     c                   eval(h)   w_stm = w_tgcstm * vidper
570100050617      * aumento
570200050617     c                   if        vidad ='+'
570300050617     c                   add       w_stm         w_tgcstm
570400050617     c                   else
570500050617      * diminuisco
570600050617     c                   sub       w_stm         w_tgcstm
570700050617      * se minore non aggiorno
570800050617     c                   if        w_tgcstm <= *zeros
570900050617     c                   eval      wnoagg = *on
571000050617     c                   leavesr
571100050617     c                   endif
571200050617     c                   endif
571300050617    3c                   endif
571400050617
571500050617    2c                   endif
571600050617
571700050617      * ---- Arrotondamenti
571800050617if  2c                   if        vidlir > *zeros or vidpei > *zeros
571900050617      * Arrotondamento
572000050617if  3c                   if        vidali > *zeros
572100050617     c                   z-add     vidali        xarrot
572200050617     c                   move      vidade        xde
572300050617      * Spese varie giacenza
572400050617if  4c                   if        w_tgcsgv > *zeros
572500050617     c                   z-add     w_tgcsgv      xvalue
572600050617     c                   exsr      xarrox
572700050617     c                   z-add     xvalue        w_tgcsgv
572800050617    4c                   endif
572900050617      * Spese riconsegna città
573000050617if  4c                   if        w_tgcsgr > *zeros
573100050617     c                   z-add     w_tgcsgr      xvalue
573200050617     c                   exsr      xarrox
573300050617     c                   z-add     xvalue        w_tgcsgr
573400050617    4c                   endif
573500050617      * Spese riconsegna provincia
573600050617if  4c                   if        w_tgcsgp > *zeros
573700050617     c                   z-add     w_tgcsgp      xvalue
573800050617     c                   exsr      xarrox
573900050617     c                   z-add     xvalue        w_tgcsgp
574000050617    4c                   endif
574100050617      * Spese dossier giacenza
574200050617if  4c                   if        w_tgcsgd > *zeros
574300050617     c                   z-add     w_tgcsgd      xvalue
574400050617     c                   exsr      xarrox
574500050617     c                   z-add     xvalue        w_tgcsgd
574600050617    4c                   endif
574700050617      * 1° scaglione tariffa sosta
574800050617if  4c                   if        w_tgcst1 > *zeros
574900050617     c                   z-add     w_tgcst1      xvalue
575000050617     c                   exsr      xarrox
575100050617     c                   z-add     xvalue        w_tgcst1
575200050617    4c                   endif
575300050617      * 2° scaglione tariffa sosta
575400050617if  4c                   if        w_tgcst2 > *zeros
575500050617     c                   z-add     w_tgcst2      xvalue
575600050617     c                   exsr      xarrox
575700050617     c                   z-add     xvalue        w_tgcst2
575800050617    4c                   endif
575900050617      * 3° scaglione tariffa sosta
576000050617if  4c                   if        w_tgcst3 > *zeros
576100050617     c                   z-add     w_tgcst3      xvalue
576200050617     c                   exsr      xarrox
576300050617     c                   z-add     xvalue        w_tgcst3
576400050617    4c                   endif
576500050617      * Tariffa minima sosta
576600050617if  4c                   if        w_tgcstm > *zeros
576700050617     c                   z-add     w_tgcstm      xvalue
576800050617     c                   exsr      xarrox
576900050617     c                   z-add     xvalue        w_tgcstm
577000050617    4c                   endif
577100050628    3c                   endif
577200050617      * Arrotondamento ai decimali
577300050628if  3c                   if        vidsad = 'S'
577400050617      * Spese varie giacenza
577500050628if  4c                   if        w_tgcsgv > *zeros
577600050617     c                   z-add     w_tgcsgv      xvalde
577700050617     c                   exsr      xarrod
577800050617     c                   z-add     xvalde        w_tgcsgv
577900050628    4c                   endif
578000050617      * Spese riconsegna città
578100050628if  4c                   if        w_tgcsgr > *zeros
578200050617     c                   z-add     w_tgcsgr      xvalde
578300050617     c                   exsr      xarrod
578400050617     c                   z-add     xvalde        w_tgcsgr
578500050628    4c                   endif
578600050617      * Spese riconsegna provincia
578700050628if  4c                   if        w_tgcsgp > *zeros
578800050617     c                   z-add     w_tgcsgp      xvalde
578900050617     c                   exsr      xarrod
579000050617     c                   z-add     xvalde        w_tgcsgp
579100050628    4c                   endif
579200050617      * Spese dossier giacenza
579300050628if  4c                   if        w_tgcsgd > *zeros
579400050617     c                   z-add     w_tgcsgd      xvalde
579500050617     c                   exsr      xarrod
579600050617     c                   z-add     xvalde        w_tgcsgd
579700050628    4c                   endif
579800050617      * 1° scaglione tariffa sosta
579900050628if  4c                   if        w_tgcst1 > *zeros
580000050617     c                   z-add     w_tgcst1      xvalde
580100050617     c                   exsr      xarrod
580200050617     c                   z-add     xvalde        w_tgcst1
580300050628    4c                   endif
580400050617      * 2° scaglione tariffa sosta
580500050628if  4c                   if        w_tgcst2 > *zeros
580600050617     c                   z-add     w_tgcst2      xvalde
580700050617     c                   exsr      xarrod
580800050617     c                   z-add     xvalde        w_tgcst2
580900050628    4c                   endif
581000050617      * 3° scaglione tariffa sosta
581100050628if  4c                   if        w_tgcst3 > *zeros
581200050617     c                   z-add     w_tgcst3      xvalde
581300050617     c                   exsr      xarrod
581400050617     c                   z-add     xvalde        w_tgcst3
581500050628    4c                   endif
581600050617      * Tariffa minima sosta
581700050628if  4c                   if        w_tgcstm > *zeros
581800050617     c                   z-add     w_tgcstm      xvalde
581900050617     c                   exsr      xarrod
582000050617     c                   z-add     xvalde        w_tgcstm
582100050628    4c                   endif
582200050628    3c                   endif
582300050617    2c                   endif
582400050617
582500050617      * valori positivi nei campi
582600050617     c                   mllzo     1             w_tgcsgv
582700050617     c                   mllzo     1             w_tgcsgr
582800050617     c                   mllzo     1             w_tgcsgp
582900050617     c                   mllzo     1             w_tgcsgd
583000050617     c                   mllzo     1             w_tgcst1
583100050617     c                   mllzo     1             w_tgcst2
583200050617     c                   mllzo     1             w_tgcst3
583300050617     c                   mllzo     1             w_tgcstm
583400050617
583500050617      * controllo se i campi aggiornati superano il valore massimo
583600050617if  2c                   if        w_tgcsgv > 99999999,999 or
583700050617     c                             w_tgcsgr > 99999999,999 or
583800050617     c                             w_tgcsgp > 99999999,999 or
583900050617     c                             w_tgcsgd > 99999999,999 or
584000050617     c                             w_tgcst1 > 99999999,999 or
584100050617     c                             w_tgcst2 > 99999999,999 or
584200050617     c                             w_tgcst3 > 99999999,999 or
584300050617     c                             w_tgcstm > 99999999,999
584400050617     c                   eval      wnoagg = *on
584500050617     c                   leavesr
584600050617    2c                   endif
584700050617
584800050617      * ---- Rapporto peso volume
584900050617     c                   eval      w_tgcrpv = tgcrpv
585000050617
585100050617if  2c                   if        vidvrr > *zeros and w_tgcrpv > *Zeros
585200050617     c                   if        vidrad = '+'
585300050617     c                   add       vidvrr        w_tgcrpv
585400050617     c                   else
585500050617     c                   sub       vidvrr        w_tgcrpv
585600050617      * se minore non aggiorno
585700050617     c                   if        w_tgcrpv <= *zeros
585800050617     c                   eval      wnoagg = *on
585900050617     c                   leavesr
586000050617     c                   endif
586100050617     c                   endif
586200050617    2c                   endif
586300050617      * annullo i rapporto peso volume
586400050617if  2c                   If        vidarr = 'SI' and w_tgcrpv > *Zeros
586500050617     c                   Clear                   w_tgcrpv
586600050617    2c                   EndIf
586700061207      * se inserimento imposto il rapporto peso volume
586800061207     c                   if        vidirr > 0
586900061207     c                   z-add     vidirr        w_tgcrpv
587000061207     c                   endif
587100050617
587200050617     c                   mllzo     1             w_tgcrpv
587300050617      * controllo se i campi aggiornati superano il valore massimo
587400050617if  2c                   if        w_tgcrpv > 99,9
587500050617     c                   eval      wnoagg = *on
587600050617     c                   leavesr
587700050617    2c                   endif
587800050617
587900050617      * aggiorno le tariffe di giacenza
588000050617if  2c                   if        wsimula = '1'
588100050617
588200050617     c                   eval      tgcsgv = w_tgcsgv
588300050617     c                   eval      tgcsgr = w_tgcsgr
588400050617     c                   eval      tgcsgp = w_tgcsgp
588500050617     c                   eval      tgcsgd = w_tgcsgd
588600050617     c                   eval      tgcst1 = w_tgcst1
588700050617     c                   eval      tgcst2 = w_tgcst2
588800050617     c                   eval      tgcst3 = w_tgcst3
588900050617     c                   eval      tgcstm = w_tgcstm
589000050627     c                   eval      tgcrpv = w_tgcrpv
589100050617
589200050617     c                   z-add     dateu8        tgcdtr
589300050617     c                   clear                   tgcftr
589400050913     c  n50              update    titgc000
589500050913     c   50              update    tiogc000
589600050617      * valorizzo il flag per la scrittura dello storico variazioni
589700050617     c                   eval      mangia = 'S'
589800050617    2c                   endif
589900050616
590000050617    1c                   endif
590100050616
590200050616     c                   endsr
590300900410     C*
590400930623     C*--- ARROTONDAMENTO VARIABILI ----------------------------------*
590500900410     C     XARROX        BEGSR
590600900410     C* XDE   : "D" DIFETTO   "E" ECCESSO
590700900410     C* XVALUE: VALORE DA ARROTONDARE
590800900410     C* XARROT: ARROTONDAMENTO
590900900410     C     *LIKE         DEFINE    XVALUE        XCOM02
591000900410     C     *LIKE         DEFINE    XVALUE        XARROT
591100900410     C                   MOVE      XDE           XDE               1
591200900410     C     XARROT        IFGT      0
591300900410     C     XVALUE        DIV       XARROT        XCOM01
591400900410     C                   MVR                     XCOM02
591500900410     C     XDE           IFEQ      'E'
591600900410     C     XCOM02        IFGT      0
591700950119     C                   ADD       1             XCOM01           15 0
591800900410     C                   END
591900900410     C                   END
592000950118     C     XCOM01        MULT      XARROT        XVALUE           15 3
592100900410     C                   END
592200900410     C                   ENDSR
592300930624     C*
592400010323     C*--- ARROTONDAMENTO VARIABILI AL DECIMALE-----------------------*
592500010323     C     XARROD        BEGSR
592600010323     C* VIDDEC: NUMERO DECIMALI
592700010323     C* XVALDE: VALORE DA ARROTONDARE AI DECIMALI
592800010323     C*
592900010323     C* 0 DECIMALI
593000010323     C     VIDDEC        IFEQ      0
593100010323     C                   Z-ADD(H)  XVALDE        CAMP0D           15 0
593200010323     C                   Z-ADD     CAMP0D        XVALDE
593300010323     C                   ENDIF
593400010323     C* 1 DECIMALE
593500010323     C     VIDDEC        IFEQ      1
593600010323     C                   Z-ADD(H)  XVALDE        CAMP1D           15 1
593700010323     C                   Z-ADD     CAMP1D        XVALDE
593800010323     C                   ENDIF
593900010323     C* 2 DECIMALI
594000010323     C     VIDDEC        IFEQ      2
594100010323     C                   Z-ADD(H)  XVALDE        CAMP2D           15 2
594200010323     C                   Z-ADD     CAMP2D        XVALDE
594300010323     C                   ENDIF
594400010323     C*
594500010323     C                   ENDSR
594600010323     C*
594700960523     C*--- CONTROLLO CAMPI VIDEO DUPLICA DETTAGLIO TARIFFA -----------*
594800960523     C     CTRD08        BEGSR
594900990817     C* CONTROLLI CHE VENGONO EFFETTUATI OLTRE ALLA CORRETTEZZA DEI DATI :
595000990817     C* A) SE SONO VALORIZZATE SOLO LE LINEE DI PARTENZE E NON  I CODICI
595100990817     C*    TASSAZIONE SI COPIA TUTTO SENZA FARE CONTROLLI
595200990817     C* B) SE INSERITI ENTRAMBI I CODICI TASSAZIONE :
595300990817     C*    1) DEVONO ESSERE ENTRAMBI ITALIA O ESTERO
595400990817     C*    2) NON SI COPIA IL DETTAGLIO CON INSERITO IL CAP ED EMETTERE UN
595500990817     C*       MESSAGGIO DI ERRORE FORZABIOLE LI
595600960523     C*
595700960523     C****  EFFETTUARE ALMENO UNA SELEZIONE  ****
595800960523    1C     V8CLNN        IFEQ      0
595900960523     C     V8CCTN        ANDEQ     *BLANKS
596000960523     C     V8CLNP        ANDEQ     0
596100960523     C     V8CCTS        ANDEQ     *BLANKS
596200960523     C                   MOVEL     MSG(37)       V8CMSG
596300960523     C                   SETON                                        7128
596400960523     C                   GOTO      ENDC08
596500960523    1C                   ENDIF
596600960523     C*
596700960527     C****  RICERCA CODICI TASSAZIONE  ****
596800960527    1C     V8CCTN        IFEQ      '?'
596900120713     c                   exsr      Ric_CT
597000120713     C                   MOVEL     TB09cod       V8CCTN
597100960527     C                   SETON                                        90
597200960527     C                   GOTO      ENDC08
597300960527    1C                   ENDIF
597400960527     C*
597500960527    1C     V8CCTS        IFEQ      '?'
597600120713     c                   exsr      Ric_CT
597700120713     c                   eval      TRTB09DS = kpjbu
597800120713     C                   MOVEL     TB09cod       V8CCTS
597900960527     C                   SETON                                        90
598000960527     C                   GOTO      ENDC08
598100960527    1C                   ENDIF
598200960527     C*
598300960523     C****  LINEA PARTENZA  ****
598400960523     C* SE IMMESSA LA LINEA PARTENZA DA CREARE OCCORRE IMMETTERE ANCHE
598500960523     C*   LA LINEA PARTENZA DA CUI COPIARE E VICEVERSA
598600960523    1C     V8CLNN        IFGT      0
598700960523     C     V8CLNP        ANDEQ     0
598800960524     C                   MOVEL     MSG(65)       V8CMSG
598900960523     C                   SETON                                        7328
599000960523     C                   GOTO      ENDC08
599100960523    1C                   ENDIF
599200960523     C*
599300960523    1C     V8CLNN        IFEQ      0
599400960523     C     V8CLNP        ANDGT     0
599500960524     C                   MOVEL     MSG(65)       V8CMSG
599600960523     C                   SETON                                        7128
599700960523     C                   GOTO      ENDC08
599800960523    1C                   ENDIF
599900960524     C*
600000960524     C* SE INSERITE SOLO LE LINEE PARTENZA DEVONO ESSERE DIVERSE
600100960524    1C     V8CLNN        IFGT      0
600200960524     C     V8CCTN        ANDEQ     *BLANKS
600300960524     C     V8CCTS        ANDEQ     *BLANKS
600400960524     C     V8CLNN        ANDEQ     V8CLNP
600500960524     C                   MOVEL     MSG(63)       V8CMSG
600600960524     C                   SETON                                        7128
600700960524     C                   GOTO      ENDC08
600800960524    1C                   ENDIF
600900960523     C*
601000960523     C****  CODICE TASSAZIONE  ****
601100960524     C* SE IMMESSO COD.TASSAZ. OCCORRE IMMETTERE ANCHE LINEA PARTENZA
601200960524    1C     V8CCTN        IFNE      *BLANKS
601300960524     C     V8CLNN        ANDEQ     0
601400960524     C                   MOVEL     MSG(65)       V8CMSG
601500960524     C                   SETON                                        7128
601600960524     C                   GOTO      ENDC08
601700960524    1C                   ENDIF
601800960524     C*
601900960524    1C     V8CCTS        IFNE      *BLANKS
602000960524     C     V8CLNP        ANDEQ     0
602100960524     C                   MOVEL     MSG(65)       V8CMSG
602200960524     C                   SETON                                        7328
602300960524     C                   GOTO      ENDC08
602400960524    1C                   ENDIF
602500960523     C*
602600960524     C* SE IMMESSO IL COD.TASSAZIONE DA CREARE OCCORRE IMMETTERE ANCHE
602700960524     C*   IL COD.TASSAZIONE DA CUI COPIARE E VICEVERSA
602800960524    1C     V8CCTS        IFNE      *BLANKS
602900960524     C     V8CCTN        ANDEQ     *BLANKS
603000960524     C                   MOVEL     MSG(66)       V8CMSG
603100960527     C                   SETON                                        7228
603200960524     C                   GOTO      ENDC08
603300960524    1C                   ENDIF
603400960524     C*
603500960524    1C     V8CCTN        IFNE      *BLANKS
603600960524     C     V8CCTS        ANDEQ     *BLANKS
603700960524     C                   MOVEL     MSG(66)       V8CMSG
603800960527     C                   SETON                                        7428
603900960524     C                   GOTO      ENDC08
604000960524    1C                   ENDIF
604100960524     C*
604200960524     C* SE INSERITI LINEE PARTENZA E COD.TASSAZ. DEVONO ESSERE DIVERSI
604300960524    1C     V8CLNN        IFGT      0
604400960524     C     V8CCTN        ANDNE     *BLANKS
604500960524     C     V8CLNN        ANDEQ     V8CLNP
604600960524     C     V8CCTN        ANDEQ     V8CCTS
604700960524     C                   MOVEL     MSG(64)       V8CMSG
604800960524     C                   SETON                                        7128
604900960524     C                   GOTO      ENDC08
605000960524    1C                   ENDIF
605100960524     C*
605200960523     C****  LINEA PARTENZA DA CREARE  ****
605300051019    1C     V8CLNN        IFne      0
605400020312     c                   clear                   og143
605500960523     C     V8CLNN        CHAIN     AZORG                              34
605600960523    2C     *IN34         IFEQ      *ON
605700960523     C     ORGFVA        ORNE      ' '
605800960523     C     ORGFAG        ORNE      'A'
605900960523     C     ORGFAG        ANDNE     'F'
606000020527     c     orgfl2        andne     'S'
606100960523     C                   MOVEL     MSG(22)       V8CMSG
606200960523     C                   SETON                                        7128
606300960523     C                   GOTO      ENDC08
606400960523    2C                   ENDIF
606500020312
606600020312     c                   eval      og143 = orgde3
606700020312
606800990817     C* VERIFICO SE LA LINEA DI PARTENZA E' CONGRUENTE CON  TIPO TARIFFA ITALIA
606900990817     C*
607000020312   2 c                   if        (§ogntw ='EEX' or §ogntw = 'EUP' or
607100020312     c                              §ogntw = 'FED') and v1cfie = 'I' and
607200020312     c                              v1cnet = *blanks
607300990817     C                   MOVEL     MSG(45)       V8CMSG
607400990817     C                   SETON                                        7128
607500990817     C                   GOTO      ENDC08
607600990817    2C                   ENDIF
607700990817     C*
607800960523    1C                   ENDIF
607900960523     C*
608000960523     C****  CODICE TASSAZIONE DA CREARE  ****
608100960523    1C     V8CCTN        IFNE      *BLANKS
608200960523     C                   Z-ADD     1             X
608300960523     C     V8CCTN        LOOKUP    CCT(X)                                 35
608400960523    2C     *IN35         IFEQ      *OFF
608500960523     C                   MOVEL     MSG(23)       V8CMSG
608600960523     C                   SETON                                        7228
608700960523     C                   GOTO      ENDC08
608800960523   X2C                   ELSE
608900050303      * errore bloccante se codice tassazione non utilizzabile in gestione
609000050303      * tariffe clienti
609100050303     c                   If        uta(x) = 'N'
609200050303     c                   Eval      v8cmsg = msg(36)
609300050303     c                   Eval      *In72 = *On
609400050303     c                   Eval      *In28 = *On
609500050303     c                   Goto      endc08
609600050303     c                   EndIf
609700020206      *
609800020206      * Se tariffa FedEx devo utilizzare solo i codici tariffa
609900020206      * previsti per FedEx
610000020206    3C     V1CNET        IFEQ      'F'
610100020214     C     UTF(X)        ANDNE     'S'
610200020206     C                   SETON                                        7228
610300020206     C                   MOVEL     MSG(100)      V8CMSG
610400020206     C                   GOTO      ENDC08
610500020206    3C                   ENDIF
610600020206      * Se tariffa non è FedEx non posso utilizzare i codici
610700020206      * tariffa previsti per FedEx
610800020206    3C     V1CNET        IFNE      'F'
610900020214     C     UTF(X)        ANDEQ     'S'
611000020206     C                   SETON                                        7228
611100020206     C                   MOVEL     MSG(101)      V8CMSG
611200020206     C                   GOTO      ENDC08
611300020206    3C                   ENDIF
611400020206      *
611500990817     C* VERIFICO SE CODICE TASSAZIONE  E' CONGRUENTE CON IL TIPO TARIFFA ITALIA
611600990817     C*
611700990817    3C     V1CFIE        IFEQ      'I'
611800020201     C     V1CNET        ANDEQ     ' '
611900990817     C     NAR(X)        LOOKUP    NAZ                                    35
612000990817    4C     *IN35         IFEQ      *OFF
612100990817     C                   MOVEL     MSG(47)       V8CMSG
612200990817     C                   SETON                                        7228
612300990817     C                   GOTO      ENDC08
612400990817    4C                   ENDIF
612500990817    3C                   ENDIF
612600990817     C*
612700990608     C                   MOVE      ORP(X)        WORPN
612800960523    2C                   ENDIF
612900990830     C*
613000990830     C* SE TARIFFA ESTERA LINEA DI PARTENZA E/O CODICE TASSAZIONE DEVE ESSERE
613100990830     C* ESTERO
613200020312   2 c                   if        v1cfie = 'E' and §ogntw <> 'EEX' and
613300020312     c                             §ogntw <> 'EUP' and §ogntw <> 'FED'
613400990830     C     NAR(X)        LOOKUP    NAZ                                    35
613500990830    3C     *IN35         IFEQ      *ON
613600990830     C                   MOVEL     MSG(48)       V8CMSG
613700990830     C                   SETON                                        7128
613800990830     C                   GOTO      ENDC08
613900990830    3C                   ENDIF
614000990830    2C                   ENDIF
614100960523    1C                   ENDIF
614200960523     C*
614300960523     C****  LINEA PARTENZA DA CUI COPIARE  ****
614400051019    1C     V8CLNP        IFne      0
614500960523     C     V8CLNP        CHAIN     AZORG                              34
614600960523    2C     *IN34         IFEQ      *ON
614700960523     C     ORGFVA        ORNE      ' '
614800960523     C     ORGFAG        ORNE      'A'
614900960523     C     ORGFAG        ANDNE     'F'
615000020527     c     orgfl2        andne     'S'
615100960523     C                   MOVEL     MSG(22)       V8CMSG
615200960523     C                   SETON                                        7328
615300960523     C                   GOTO      ENDC08
615400960523    2C                   ENDIF
615500960523    1C                   ENDIF
615600960523     C*
615700960523     C****  CODICE TASSAZIONE DA CUI COPIARE  ****
615800960523    1C     V8CCTS        IFNE      *BLANKS
615900960523     C                   Z-ADD     1             X
616000960523     C     V8CCTS        LOOKUP    CCT(X)                                 35
616100960523    2C     *IN35         IFEQ      *OFF
616200960523     C                   MOVEL     MSG(23)       V8CMSG
616300960523     C                   SETON                                        7428
616400960523     C                   GOTO      ENDC08
616500960523   X2C                   ELSE
616600050303      * errore bloccante se codice tassazione non utilizzabile in gestione
616700050303      * tariffe clienti
616800050303     c                   If        uta(x) = 'N'
616900050303     c                   Eval      v8cmsg = msg(36)
617000050303     c                   Eval      *In74 = *On
617100050303     c                   Eval      *In28 = *On
617200050303     c                   Goto      endc08
617300050303     c                   EndIf
617400020206      *
617500020206      * Se tariffa FedEx devo utilizzare solo i codici tariffa
617600020206      * previsti per FedEx
617700020206    3C     V1CNET        IFEQ      'F'
617800020214     C     UTF(X)        ANDNE     'S'
617900020206     C                   SETON                                        7428
618000020206     C                   MOVEL     MSG(100)      V8CMSG
618100020206     C                   GOTO      ENDC08
618200020206    3C                   ENDIF
618300020206      * Se tariffa non è FedEx non posso utilizzare i codici
618400020206      * tariffa previsti per FedEx
618500020206    3C     V1CNET        IFNE      'F'
618600020214     C     UTF(X)        ANDEQ     'S'
618700020206     C                   SETON                                        7428
618800020206     C                   MOVEL     MSG(101)      V8CMSG
618900020206     C                   GOTO      ENDC08
619000020206    3C                   ENDIF
619100020206      *
619200990608     C                   MOVE      ORP(X)        WORP
619300960523    2C                   ENDIF
619400960523    1C                   ENDIF
619500960523     C*
619600960523     C****  CONTROLLO CHE ESISTA ALMENO UNA RIGA DI DETTAGLIO  ****
619700960523     C****  DA CUI POTER COPIARE                               ****
619800960523     C                   MOVEL     '1'           WTROV             1
619900960523    1C     V8CLNP        IFGT      0
620000960524     C                   MOVEL     V8CLNP        COMLN2
620100960524     C*
620200960523     C* IMMESSA SOLO LA LINEA PARTENZA
620300960523    2C     V8CCTS        IFEQ      *BLANKS
620400061011     C  n50KTAD2         SETLL     TITAD000
620500061011     c   50ktad2         setll     tiofd000
620600061011     C  n50KTAD2         READE     TITAD000                               34
620700061011     c   50ktad2         reade     tiofd000                               34
620800960523   X2C                   ELSE
620900960523     C* IMMESSO ANCHE IL CODICE TASSAZIONE
621000990608     C                   MOVE      WORP          COMOR2
621100061011     C  n50KTAD3         SETLL     TITAD000
621200061011     c   50ktad3         setll     tiofd000
621300061011     C  n50KTAD3         READE     TITAD000                               34
621400061011     c   50ktad3         reade     tiofd000                               34
621500960523    2C                   ENDIF
621600960523     C*
621700960523    2C     *IN34         DOWEQ     *OFF
621800960523    3C     TADATB        IFEQ      ' '
621900960523     C                   SETON                                        34
622000960523     C                   CLEAR                   WTROV
622100960523   X3C                   ELSE
622200960523     C*
622300960523    4C     V8CCTS        IFEQ      *BLANKS
622400061011     C  n50KTAD2         READE     TITAD000                               34
622500061011     c   50ktad2         reade     tiofd000                               34
622600960523   X4C                   ELSE
622700061011     C  n50KTAD3         READE     TITAD000                               34
622800061011     c   50ktad3         reade     tiofd000                               34
622900960523    4C                   ENDIF
623000960523    3C                   ENDIF
623100960523    2C                   ENDDO
623200960523     C*
623300960523     C* WTROV <> " " --> NON ESISTONO RIGHE DI DETTAGLIO DA CUI COPIARE
623400960523    2C     WTROV         IFNE      ' '
623500960523     C                   MOVEL     MSG(62)       V8CMSG
623600960523     C                   SETON                                        7328
623700960523     C                   GOTO      ENDC08
623800960523    2C                   ENDIF
623900960523    1C                   ENDIF
624000960523     C*
624100960524     C****  SE ESISTONO GIA' RIGHE DI DETTAGLIO CON LINEA PART.   ****
624200960524     C****  E COD.TASS. DA CREARE AVVISO CON MESSAGGIO FORZABILE  ****
624300960527    0C     V8CLNN        IFNE      SAVLNN
624400960527     C     V8CCTN        ORNE      SAVCTN
624500960527     C                   MOVEL     V8CLNN        SAVLNN
624600960527     C                   MOVEL     V8CCTN        SAVCTN
624700960527     C                   CLEAR                   WTROV
624800960527     C*
624900960524    1C     V8CLNN        IFGT      0
625000960524     C                   MOVEL     V8CLNN        COMLN2
625100960524     C*
625200960524     C* IMMESSA SOLO LA LINEA PARTENZA
625300960524    2C     V8CCTN        IFEQ      *BLANKS
625400061011     C  n50KTAD2         SETLL     TITAD000
625500061011     c   50ktad2         setll     tiofd000
625600061011     C  n50KTAD2         READE     TITAD000                               34
625700061011     c   50ktad2         reade     tiofd000                               34
625800960524   X2C                   ELSE
625900960524     C* IMMESSO ANCHE IL CODICE TASSAZIONE
626000990608     C                   MOVE      WORPN         COMOR2
626100061011     C  n50KTAD3         SETLL     TITAD000
626200061011     c   50ktad3         setll     tiofd000
626300061011     C  n50KTAD3         READE     TITAD000                               34
626400061011     c   50ktad3         reade     tiofd000                               34
626500960524    2C                   ENDIF
626600960524     C*
626700960524    2C     *IN34         DOWEQ     *OFF
626800960524    3C     TADATB        IFEQ      ' '
626900960524     C                   SETON                                        34
627000960527     C                   MOVEL     '1'           WTROV
627100960524   X3C                   ELSE
627200960524     C*
627300960524    4C     V8CCTN        IFEQ      *BLANKS
627400061011     C  n50KTAD2         READE     TITAD000                               34
627500061011     c   50ktad2         reade     tiofd000                               34
627600960524   X4C                   ELSE
627700061011     C  n50KTAD3         READE     TITAD000                               34
627800061011     c   50ktad3         reade     tiofd000                               34
627900960524    4C                   ENDIF
628000960524    3C                   ENDIF
628100960524    2C                   ENDDO
628200960524     C*
628300960527     C* WTROV <> " " --> LA TARIFFA CHE SI VUOLE CREARE ESISTE GIA'
628400960527    2C     WTROV         IFNE      ' '
628500960527     C     V8CCTN        IFEQ      *BLANKS
628600960527     C                   MOVEL     MSG(68)       V8CMSG
628700960527     C                   ELSE
628800960524     C                   MOVEL     MSG(67)       V8CMSG
628900960527     C                   ENDIF
629000960527     C                   SETON                                        7128
629100960524     C                   GOTO      ENDC08
629200960524    2C                   ENDIF
629300960524    1C                   ENDIF
629400960527     C*
629500960527    0C                   ENDIF
629600090608
629700090625      * i prossimi controlli li avevamo messi convinti che la tassazione
629800090625      * non arrivasse a tassare con 950 e IT, invece no, ci sono clienti
629900090625      * che hanno solo questo tipo di tariffa e le spedizioni sono
630000090625      * regolarmente tassate quindi bypasso i controlli che avevamo fatto
630100090625      * quindi ridiamo la possibilità di duplicare verso 950 IT
630200090625     c                   leavesr
630300090608      * se la nuova linea di partenza è 950
630400090608    1c                   if        v8clnn = 950
630500090608      * non posso richiedere codice di tassazione IT o EE
630600090608     c                   if        v8cctn = 'IT' or v8cctn = 'EE'
630700090608     c                   eval      *in72 = *on
630800090608     c                   eval      *in28 = *on
630900090608     c                   eval      v8cmsg = msg(110)
631000090608     c                   leavesr
631100090608     c                   endif
631200090608      * non posso duplicare se esiste un codice di tassazione IT o EE
631300090608     c                   clear                   wtrov
631400090608     c                   movel     v8clnp        comln2
631500090608     c  n50ktad2         setll     titad04l
631600090608     c   50ktad2         setll     tiofd01l
631700090608    2c                   do        *hival
631800090608     c  n50ktad2         reade     titad04l
631900090608     c   50ktad2         reade     tiofd01l
632000090608     c                   if        %eof
632100090608     c                   leave
632200090608     c                   endif
632300090608     c                   if        tadatb <> *blanks
632400090608     c                   iter
632500090608     c                   endif
632600090608     c                   if        tadcts = 'IT' or tadcts = 'EE'
632700090608     c                   eval      wtrov = '1'
632800090608     c                   leave
632900090608     c                   endif
633000090608    2c                   enddo
633100090608     c                   if        wtrov <> *blanks
633200090608     c                   eval      *in71 = *on
633300090608     c                   eval      *in28 = *on
633400090608     c                   eval      v8cmsg = msg(121)
633500090608     c                   eval      %subst(v8cmsg:36:3) = %editc(v8clnp:'1')
633600090608     c                   leavesr
633700090608     c                   endif
633800090608    1c                   endif
633900960524     C*
634000960523     C     ENDC08        ENDSR
634100960523     C*
634200960523     C*--- ESEGUO DUPLICAZIONE DETTAGLIO TARIFFA ---------------------*
634300960523     C     DUPTAR        BEGSR
634400960523     C*
634500061012     c  n50              unlock    titad04l
634600061012     c   50              unlock    tiofd01l
634700061012
634800061012     c                   clear                   Tnta25ds
634900061012     c  n50              eval      ta25tipo = 'T'
635000061012     c   50              eval      ta25tipo = 'O'
635100061012     c                   eval      ta25ksco = v1cksc
635200061012     c                   move      vctr          ta25ctro
635300061012     c                   move      prg           ta25prgo
635400061012     c  n50              eval      ta25tipn = 'T'
635500061012     c   50              eval      ta25tipn = 'O'
635600061012     c                   eval      ta25kscn = v1cksc
635700061012     c                   move      vctr          ta25ctrn
635800061012     c                   move      prg           ta25prgn
635900061012     c                   eval      ta25tam = 'N'
636000061012     c                   eval      ta25fie = v1cfie
636100061012     c                   eval      ta25tam = 'N'
636200061012     c                   eval      ta25tad = 'N'
636300061012     c                   eval      ta25tpt = 'N'
636400061012     c                   eval      ta25tpd = 'N'
636500061012     c                   eval      ta25tgc = 'N'
636600061012     c                   eval      ta25cat = 'N'
636700061012     c                   eval      ta25inglo = 'N'
636800061012     c                   eval      ta25dupd = 'S'
636900061012     c                   move      v8clnp        ta25olnp
637000061012     c                   move      v8ccts        ta25octs
637100061012     c                   move      v8clnn        ta25nlnp
637200061012     c                   move      v8cctn        ta25ncts
637300090917      * imposto se da trattativa
637400090917     c                   eval      %subst(kpjbu:1:1) = partrat
637500061012
637600061012     c                   Call      'TNTA25R'
637700061012     c                   Parm                    kpjba
637800061012     c                   Parm                    Tnta25ds
637900061012     c                   Parm                    Tnta25tad
638000061012     c                   Parm                    Tnta25tgc
638100061012     c                   Parm                    Tnta25tpd
638200061012     c                   Parm                    Tnta25tpt
638300061012
638400061012     c                   If        ta25err <> *Blanks
638500061012     c                   Eval      *In28 = *On
638600061012     c                   Eval      vccmsg = ta25msg
638700061012     c                   endif
638800960523     C*
638900960523     C                   ENDSR
639000960523     C*
639100990601     C*--- CONTROLLO TITAD -------------------------------------------*
639200900410     C     CTRAGG        BEGSR
639300940525     C                   SETOFF                                       22
639400940525     C*
639500940706     C                   READC     TA01S03                                29
639600940525    1C     *IN29         DOWEQ     *OFF
639700940525     C*
639800940525     C* RECORD NON ANNULLATO
639900940525    2C     TADATB        IFNE      'A'
640000940525     C*
640100960522     C* SE ESISTONO IMPORTO MINIMO O IMPORTO MASSIMO VISUALIZZO
640200960522     C*   LA LETTERA "M" SULLA RIGA DI DETTAGLIO
640300960522    3C     TADMIN        IFGT      0
640400960522     C     TADMAX        ORGT      0
640500960522     C                   SETON                                        13
640600960522    3C                   ENDIF
640700960522     C*
640800940525     C****  TARIFFA CITTA'  ****
640900940525     C* TARIFFA CITTA' OBBLIGATORIA
641000940525    3C     TADITR        IFEQ      0
641100940526     C                   SETON                                        5928
641200940526     C                   MOVEL     MSG(29)       VCCMSG
641300940525     C                   GOTO      GIUCT2
641400941123     C                   ELSE
641500050616     C* 88 OFF -  TARIFFA NON IN % verifico se si
641600941124     C*   POSSONO INSERIRE DECIMALI
641700941124     C     *IN88         IFEQ      *OFF
641800941123     C                   MOVE      TADITR        W0030             3 0
641900941123     C     W0030         IFGT      0
642000050616     C*  LA MONETA NON ACCETTA DECIMALI
642100990617     C     §CVFDC        ANDNE     'S'
642200941123     C                   SETON                                        5928
642300990617     C                   MOVEL     MSG(79)       VCCMSG
642400941123     C                   GOTO      GIUCT2
642500941123     C                   ENDIF
642600941123     C                   ENDIF
642700940525    3C                   ENDIF
642800940525     C*
642900940525     C****  TARIFFA PROVINCIA  ****
643000940525     C* SE INSERITO IL CAP NON BISOGNA IMMETTERE LA TARIFFA PROVINCIA
643100020624    3C     tadCAP        IFNE      *BLANKS
643200940525     C     TADINO        ANDNE     0
643300940526     C                   SETON                                        6128
643400940526     C                   MOVEL     MSG(27)       VCCMSG
643500940525     C                   GOTO      GIUCT2
643600940525    3C                   ENDIF
643700941123     C*
643800050616     C* 88 OFF -  TARIFFA NON IN % verifico se si
643900941124     C*   POSSONO INSERIRE DECIMALI
644000941124     C  N88TADINO        IFGT      0
644100941123     C                   MOVE      TADINO        W0030             3 0
644200941123     C     W0030         IFGT      0
644300050616     C*  LA MONETA NON ACCETTA DECIMALI
644400990617     C     §CVFDC        ANDNE     'S'
644500941123     C                   SETON                                        6128
644600990617     C*                    MOVELMSG,55    VCCMSG
644700990617     C                   MOVEL     MSG(79)       VCCMSG
644800941123     C                   GOTO      GIUCT2
644900941123     C                   ENDIF
645000941123     C                   ENDIF
645100940525     C*
645200960522     C****  RAPPORTO PESO/VOLUME  ****
645300960522     C* SE INSERITO UN RAPPORTO PESO/VOL. SUPERIORE AL LIMITE INDICATO
645400960522     C*   IN TABELLA EMETTO ERRORE FORZABILE
645500960522     C     TADRPV        IFNE      SAVRPV
645600960522     C     TADRPV        ANDGT     §1GRPV
645700960522     C                   SETON                                        8928
645800960522     C                   MOVEL     MSG(59)       VCCMSG
645900960522     C                   MOVEL     TADRPV        SAVRPV
646000960522     C                   GOTO      GIUCT2
646100960522     C                   ENDIF
646200990617     C*
646300990617     C****  IMPORTO MASSIMO TARIFFA  ****
646400990617     C* VERIFICO SE INSERITO I DECIMALI SE SONO AMMESSI OPPURE NO
646500990617     C     TADMAX        IFGT      0
646600990617     C                   MOVE      TADMAX        W0030             3 0
646700990617     C     W0030         IFGT      0
646800990617     C* E LA MONETA NON ACCETTA DECIMALI
646900990617     C     §CVFDC        ANDNE     'S'
647000990617     C                   SETON                                        6228
647100990617     C                   MOVEL     MSG(79)       VCCMSG
647200990618     C                   MOVEL     *ON           *INKK
647300990617     C                   GOTO      GIUCT2
647400990617     C                   ENDIF
647500990617     C                   ENDIF
647600990617     C*
647700990617     C****  IMPORTO MINIMO  TARIFFA  ****
647800990617     C* VERIFICO SE INSERITO I DECIMALI SE SONO AMMESSI OPPURE NO
647900990617     C     TADMIN        IFGT      0
648000990617     C                   MOVE      TADMIN        W0030             3 0
648100990617     C     W0030         IFGT      0
648200990617     C* E LA MONETA NON ACCETTA DECIMALI
648300990617     C     §CVFDC        ANDNE     'S'
648400990617     C                   SETON                                        5228
648500990617     C                   MOVEL     MSG(79)       VCCMSG
648600990618     C                   MOVEL     *ON           *INKK
648700990617     C                   GOTO      GIUCT2
648800990617     C                   ENDIF
648900990617     C                   ENDIF
649000940525     C****  IMPORTO MASSIMO TARIFFA  ****
649100940525     C* SE INSERITO DEVE ESSERE MAGGIORE O UGUALE AL MINIMO
649200940525    3C     TADMAX        IFGT      0
649300940525     C     TADMAX        ANDLT     TADMIN
649400940526     C                   SETON                                        6228
649500940526     C                   MOVEL     MSG(31)       VCCMSG
649600990618     C                   MOVEL     *ON           *INKK
649700940525     C                   GOTO      GIUCT2
649800940525    3C                   ENDIF
649900980310      * E' inutile inserire dei valori minimi e massimi per scaglioni inferiori
650000980310      * al valore minimo tassabile o al valore tariffa finita
650100980310      * quindi faccio il controllo di TADMAX E/O TADMAX
650200980310     C     TADMAX        IFGT      0
650300980310     C     TADMIN        ORGT      0
650400980310      * VALORE TARIFFA FINITA
650500980310     C     TAMATA        IFGT      TADSGL
650600980310     C     TAMATA        OREQ      0
650700980505     C     WRKMIN        ANDGT     TADSGL
650800980310     C                   SETON                                        6228
650900980515     C                   MOVEL     MSG(73)       VCCMSG
651000990618     C                   MOVEL     *ON           *INKK
651100980310     C                   GOTO      GIUCT2
651200980310     C                   ENDIF
651300980310      *
651400980310     C                   ENDIF
651500940525     C*
651600940525    2C                   ENDIF
651700921119     C*
651800940525     C     GIUCT2        TAG
651900940526     C   28              Z-ADD     NRR           REC
652000940525     C*
652100940525     C* 22 - SFLNXTCHG
652200940525     C                   SETON                                        22
652300940706     C                   UPDATE    TA01S03
652400941126     C                   SETOFF                                       2213
652500940525     C*
652600940526     C   28              SETON                                        29
652700940706     C  N29              READC     TA01S03                                29
652800940525    1C                   ENDDO
652900921218     C*
653000940525     C                   ENDSR
653100940509     C*
653200990601     C*--- AGGIORNAMENTO TITAD ---------------------------------------*
653300921120     C     AGGTAD        BEGSR
653400900924     C                   SETOFF                                       19
653500940706     C                   READC     TA01S03                                29
653600940524     C     *IN29         DOWEQ     '0'
653700900925     C                   EXSR      UPDTAD
653800940706     C                   READC     TA01S03                                29
653900921218     C                   END
654000900925     C                   ENDSR
654100930630     C*
654200990601     C*--- AGGIORNAMENTO TITAD SU DISCO ------------------------------*
654300921118     C     UPDTAD        BEGSR
654400980219     C* VALORE POSITIVO NEI CAMPI
654500980220     C                   MLLZO     1             TADSGL
654600980219     C                   MLLZO     1             TADITR
654700980219     C                   MLLZO     1             TADINO
654800980219     C                   MLLZO     1             TADRPV
654900980219     C                   MLLZO     1             TADMIN
655000980219     C                   MLLZO     1             TADMAX
655100980219     C*
655200980219     C                   Z-ADD     TADITR        ITRCOM
655300980219     C                   Z-ADD     TADINO        INOCOM
655400980219     C                   Z-ADD     TADRPV        RPVCOM
655500940518     C                   Z-ADD     TADMIN        MINCOM
655600940518     C                   Z-ADD     TADMAX        MAXCOM
655700940518     C                   MOVEL     TADATB        ATBCOM
655800000000     C** SALVO CAMPI PRIMA CHAIN
655900061011     C  n50KTAD          CHAIN     TITAD000                           92
656000061011     c   50ktad          chain     tiofD000                           92
656100990729     C*
656200990729     C     *IN92         IFEQ      *OFF
656300921118     C*
656400930317     C     *IN50         IFEQ      '1'
656500930317     C     ATBCOM        ANDEQ     'A'
656600061011     C  n50              DELETE    TITAD000
656700061011     C   50              delete    tiofd000
656800930317     C                   ELSE
656900930317     C*
657000930317     C                   MOVEL     ATBCOM        TADATB
657100940518     C                   Z-ADD     ITRCOM        TADITR
657200921210     C                   Z-ADD     INOCOM        TADINO
657300940518     C                   Z-ADD     MINCOM        TADMIN
657400940518     C                   Z-ADD     MAXCOM        TADMAX
657500000000     C                   Z-ADD     RPVCOM        TADRPV
657600910605     C*
657700130124     C*******            Z-ADD     dateu8        TADDTR
657800940511     C                   MOVEL     *BLANKS       TADFTR
657900061011     C  n50              UPDATE    TITAD000
658000061011     C   50              update    tiofd000
658100930317     C                   END
658200990729     C*
658300990729     C                   END
658400930317     C*
658500930317     C     ATBCOM        IFEQ      'A'
658600900924     C                   SETON                                        19
658700900924     C                   END
658800000000     C                   SETON                                        30
658900000000     C                   ENDSR
659000050701      *---------------------------------------------------------------*
659100050701      * controllo se nella tariffa a spedizione esistono + scaglioni
659200050701      * per la stessa lnp e cts
659300050701      *---------------------------------------------------------------*
659400050701     c     Sr_CtrTarSpe  BegSr
659500050701
659600050701     c                   Clear                   savlnp
659700050701     c                   Clear                   savcts
659800050701     c                   Clear                   savsgl
659900050701     c                   Clear                   savnaz
660000050701     c                   Clear                   savcap
660100050701
660200050701     c                   Eval      werr = *Off
660300050701     c                   Clear                   wlnp
660400050701     c                   Clear                   wcts
660500050701     c                   Clear                   wcap
660600050701
660700050701      * Leggo tutto il titad
660800061011     c  n50ktam2         Setll     Titad04l
660900061011     c   50ktam2         Setll     Tiofd01l
661000050701     c                   Do        *Hival
661100061011     c  n50ktam2         Reade(n)  Titad04l
661200061011     c   50ktam2         Reade(n)  Tiofd01l
661300050701
661400061011     c                   If        %Eof
661500050701     c                   Leave
661600050701     c                   EndIf
661700050701
661800050701     c                   If        tadatb <> *Blanks
661900050701     c                   Iter
662000050701     c                   EndIf
662100050701
662200050701     c                   If        tadlnp = savlnp and tadcts = savcts
662300050701     c                             and tadnaz = savnaz and tadcap = savcap
662400050701     c                             and tadsgl <> savsgl and savsgl > *Zeros
662500050701     c                   Move      tadlnp        wlnp
662600050701     c                   Move      tadcts        wcts
662700050701     c                   If        Tadcap <> *Blanks
662800050701     c                   Move      tadcap        wcap
662900050701     c                   EndIf
663000050701     c                   Eval      werr = *On
663100050701     c                   Leave
663200050701     c                   EndIf
663300050701     c                   If         tadlnp <> savlnp or tadcts <> savcts
663400050701     c                              or tadnaz <> savnaz or tadcap <> savcap
663500050701     c                              or tadsgl <> savsgl
663600050701     c                   Eval       savlnp = tadlnp
663700050701     c                   Eval       savcts = tadcts
663800050701     c                   Eval       savnaz = tadnaz
663900050701     c                   Eval       savcap = tadcap
664000050701     c                   Eval       savsgl = tadsgl
664100050701     c                   EndIf
664200050701
664300050701     c                   EndDo
664400050701
664500050701     c                   EndSr
664600110128
664700110128      /free
664800110128       //---------------------------------------------------------------
664900110128       //?Controllo se rapporto peso volume uguale su tutti gli scaglioni
665000110128       //---------------------------------------------------------------
665100110128       BEGSR CtrRpv;
665200110128
665300110128         werrRpv = *off;
665400110128         clear oldrpv;
665500110128         clear wlnpRpv;
665600110128         clear wctsRpv;
665700110128         clear wsglRpv;
665800110128
665900110128       //?Leggo tutto il dettaglio tariffario per controllare il
666000110128       //?rapporto peso volume
666100110128         IF  not *in50;
666200110128           setll (v1cksc : vctr : prg) TITAD04L;
666300110128           reade(n) (v1cksc : vctr : prg) TITAD04L;
666400110128         ENDIF;
666500110128         IF  *in50;
666600110128           setll (v1cksc : vctr : prg) TIOFD01L;
666700110128           reade(n) (v1cksc : vctr : prg) TIOFD01L;
666800110128         ENDIF;
666900110128
667000110128         DOW not %eof;
667100110201         IF  TADatb = *blanks;
667200110128
667300110128           IF  oldrpv = 0;
667400110128             oldrpv = TADrpv;
667500110128           ENDIF;
667600110128
667700110128           IF  TADrpv <> OLDrpv;
667800110128             werrRpv = *on;
667900110128             wlnpRpv = TADlnp;
668000110128             wctsRpv = TADcts;
668100110128             wsglRpv = TADsgl;
668200110202             leave;
668300110128           ENDIF;
668400110128
668500110128           oldrpv = TADrpv;
668600110201         ENDIF;
668700110128
668800110128           IF  not *in50;
668900110128             reade(n) (v1cksc : vctr : prg) TITAD04L;
669000110128           ENDIF;
669100110128           IF  *in50;
669200110128             reade(n) (v1cksc : vctr : prg) TIOFD01L;
669300110128           ENDIF;
669400110128         ENDDO;
669500110128
669600110128       ENDSR;
669700110128      /end-free
669800110128
669900930630     C*
670000930630     C*--- CARICAMENTO SUBFILE IN AVANTI -----------------------------*
670100920526     C     CARSFA        BEGSR
670200921218     C                   SETON                                        91
670300900925     C                   Z-ADD     SAVREC        NRR               5 0
670400000000     C** PULIZIA SUBFILE AGGIORNAMENTO
670500900925     C     SOPRA         TAG
670600061011     C  n50KTAM2         READE     TITAD000                               33
670700061011     c   50ktam2         reade     tiofd000                               33
670800900924     C* FINE SCORRIMENTO
670900940524     C  N33TADATB        IFEQ      'A'
671000900925     C                   GOTO      SOPRA
671100900925     C                   END
671200900925     C*
671300940524     C   33              DO
671400930224     C     NRR           IFEQ      0
671500930224     C                   SETOFF                                       21
671600930224     C                   ELSE
671700900924     C                   MOVEL     KE            KI
671800900924     C                   MOVEL     CTSE          CTSI
671900940526     C*
672000940526     C                   MOVEL     MSG(32)       VCCMSG
672100940526     C                   SETON                                        28
672200930224     C                   END
672300900924     C                   GOTO      ENDCAR
672400900924     C                   END
672500900924     C*
672600900924     C                   Z-ADD     0             B                 3 0
672700900924     C                   DO        *HIVAL
672800020207      *
672900020207     C                   CLEAR                   AT0CTS
673000900924     C*
673100900924     C     TADATB        IFNE      'A'
673200000000     C                   ADD       1             NRR
673300900924     C                   ADD       1             B
673400900924     C     B             IFEQ      1
673500930630     C** KIAVE INIZIALE
673600000000     C                   MOVEL     KIAVE         KI
673700000000     C                   MOVEL     TADCTS        CTSI              2
673800000000     C                   END
673900930630     C*
674000930630     C** DECODIFICA CODICE TASSAZIONE
674100000000     C                   Z-ADD     1             A
674200940524     C     TADCTS        LOOKUP    CCT(A)                                 35
674300940524     C   35              MOVEL     DCT(A)        DESCTS
674400940524     C  N35              MOVEL     *BLANKS       DESCTS
674500020207      *
674600020207      * Se tariffa FedEx devo utilizzare solo i codici tariffa
674700020207      * previsti per FedEx
674800020207     C     V1CNET        IFEQ      'F'
674900020207     C     UTF(A)        ANDNE     'S'
675000020207     C                   MOVE      RIMM          AT0CTS
675100020207     C     VCCMSG        IFEQ      *BLANKS
675200020207     C                   SETON                                        28
675300020207     C                   MOVEL     MSG(100)      VCCMSG
675400020207     C                   ENDIF
675500020207     C                   ENDIF
675600020207      * Se tariffa non è FedEx non posso utilizzare i codici
675700020207      * tariffa previsti per FedEx
675800020207     C     V1CNET        IFNE      'F'
675900020207     C     UTF(A)        ANDEQ     'S'
676000020207     C                   MOVE      RIMM          AT0CTS
676100020207     C     VCCMSG        IFEQ      *BLANKS
676200020207     C                   SETON                                        28
676300020207     C                   MOVEL     MSG(101)      VCCMSG
676400020207     C                   ENDIF
676500020207     C                   ENDIF
676600990624     C* DECODIFICO LA NAZIONE SE E' DIVERSA DA BLANKS
676700990729     C     TADNAZ        IFEQ      *BLANKS
676800990624     C                   CLEAR                   DESNAZ
676900990624     C                   ELSE
677000990623     C                   MOVEL     '15'          COD
677100990623     C                   MOVEL     *BLANKS       KEY
677200990623     C                   MOVEL     TADNAZ        KEY
677300990623     C     KTAB          CHAIN     TABEL                              32
677400990623     C*
677500990623     C     *IN32         IFEQ      *OFF
677600990623     C     TBLFLG        ANDEQ     ' '
677700990623     C                   MOVEL     TBLUNI        DESNAZ
677800990623     C                   ENDIF
677900990624     C                   ENDIF
678000990623     C***
678100960521     C* RAPPORTO PESO/VOLUME
678200960521     C                   MOVEL     TADRPV        SAVRPV
678300941206     C*
678400941206     C* SE ESISTONO IMPORTO MINIMO O IMPORTO MASSIMO VISUALIZZO
678500941206     C*   LA LETTERA "M" SULLA RELATIVA RIGA DI DETTAGLIO
678600941206    3C     TADMIN        IFGT      0
678700941206     C     TADMAX        ORGT      0
678800941206     C                   SETON                                        13
678900941206    3C                   ENDIF
679000130124      /free
679100130124        clear atatb;
679200130124        clear atitr;
679300130124        clear atino;
679400130124        clear atrpv;
679500130124        clear atmin;
679600130124        clear atmax;
679700130124       //?imposto attributi visualizzazione sui campi che non sono da
679800130124       //?modificare
679900130124       //?lascio libera la modifica se rcd ha data = oggi
680000130124        IF  *in17 and TADdtr <> dateu8;
680100130124          atatb = nPRO;
680200130124          atitr = nPRO;
680300130124          atino = nPRO;
680400130124          atrpv = nPRO;
680500130124          atmin = nPRO;
680600130124          atmax = nPRO;
680700130124        ENDIF;
680800130124      /end-free
680900130124
681000930630     C*
681100940706     C                   WRITE     TA01S03
681200941206     C                   SETOFF                                       13
681300941206     C*
681400900924     C     B             CABEQ     16            ENDAGA
681500900924     C                   END
681600900924     C*
681700061011     C  n50KTAM2         READE     TITAD000                               33
681800061011     c   50ktam2         reade     tiofd000                               33
681900940524     C  N33              END
682000000000     C     ENDAGA        TAG
682100900924     C                   Z-ADD     NRR           REC
682200900924     C                   Z-ADD     NRR           SAVREC            9 0
682300900924     C*
682400000000     C                   MOVEL     KIAVE         KE
682500000000     C                   MOVEL     TADCTS        CTSE              2
682600050701
682700050701      * se errore per + scaglioni in tariffa a spedizione imposto
682800050701      * il msg e accendo l'indicatore
682900050701     c                   If        werr = *On
683000050701     c                   Eval      vccmsg = 'Par ' + wlnp + ' Arrivo ' +
683100050701     c                             wcts
683200050701     c                   If        wcap <> *Blanks
683300050701     c                   Eval      vccmsg = %trim(vccmsg) +
683400050701     c                             ' - c.a.p. ' + wcap
683500050701     c                   EndIf
683600050701     c                   Eval      vccmsg = %trim(vccmsg) +
683700050701     c                             ' presenti più scaglioni'
683800050701     c                   Eval      *In28 = *On
683900050701     c                   EndIf
684000110128
684100110128       //?Se tariffa FedEx e scaglioni con rapporto peso volume
684200110128       //?diverso emetto messaggio di errore
684300110128     c                   IF        werrRpv
684400110128     c                   eval      VCCmsg = 'Par ' + %editc(wlnpRpv:'X') +
684500110128     c                             ' Arrivo ' + wctsRpv +
684600110128     c                             ' Scaglione ' + %editc(wsglRpv:'4') +
684700110128     c                             ' presente P/V diverso'
684800110128     c                   eval      *in28 = *on
684900110128     c                   ENDIF
685000050701
685100000000     C** SE NON CI SONO PIU RECORD DA LEGGERE
685200000000     C** KE = KI
685300900924     C     ENDCAR        ENDSR
685400930630     C*
685500930630     C*--- ELIMINAZIONE SUBFILE SU DISCO -----------------------------*
685600921218     C     ELISF         BEGSR
685700921218     C   91              SETON                                        93
685800940706     C   91              WRITE     TA01C03
685900921218     C   91              SETOFF                                       93
686000921218     C   99              SETON                                        83
686100940706     C   99              WRITE     TA01C05
686200921218     C   99              SETOFF                                       83
686300000000     C                   ENDSR
686400930628     C*
686500930628     C*--- CARICAMENTO TABELLE ---------------------------------------*
686600920526     C     CARTAB        BEGSR
686700940503     C*
686800940503     C****  CODICI TASSAZIONE E DESCRIZIONI: CCT / DCT / ORP  ****
686900020911      * + EST
687000020206     C                   CLEAR                   DSCT
687100940531     C                   Z-ADD     1             K                 5 0
687200930628     C                   MOVEL     'CT'          COD
687300940510     C     KTAB2         SETLL     TABEL
687400940510     C     KTAB2         READE     TABEL                                  32
687500930628     C*
687600940510     C     *IN32         DOWEQ     *OFF
687700930628     C     TBLFLG        IFEQ      ' '
687800000000     C                   MOVEL     TBLKEY        CCT(K)
687900000000     C                   MOVEL     TBLUNI        DCT(K)
688000020206     C                   MOVEL     TBLUNI        DSCT
688100020206     C                   MOVE      §CTCOR        ORP(K)
688200020206     C                   MOVE      §CTCOR        REG(K)
688300020206     C                   MOVEL     §CTNAR        NAR(K)
688400020206     C                   MOVEL     §CTUTF        UTF(K)
688500020911      * Flag estero
688600020911     c                   Movel     §ctest        est(k)
688700050303      * flag utilizzo in gestione tariffe
688800050303     c                   Eval      uta(k) = §ctuta
688900930628     C                   ADD       1             K
689000930628     C                   ENDIF
689100930628     C*
689200940510     C     KTAB2         READE     TABEL                                  32
689300930628     C                   ENDDO
689400930628     C*
689500941124     C****  CODICI TARIFFA    E DESCRIZIONI: CTR/DTR/MIN/TAP/SAP  ****
689600940531     C                   Z-ADD     1             K
689700000000     C                   MOVEL     'TR'          COD
689800940510     C     KTAB2         SETLL     TABEL
689900940510     C     KTAB2         READE     TABEL                                  32
690000930628     C*
690100940510     C     *IN32         DOWEQ     *OFF
690200930628     C     TBLFLG        IFEQ      ' '
690300980505     C                   MOVEL     TBLUNI        DSTR
690400980505     C* CONTROLLO CHE IL TIPO TARIFFA E' ABILITATO ALLA GESTIONE TARIFFE CLIENTI
690500980505     C     §TRUTC        IFEQ      'S'
690600000000     C                   MOVEL     TBLKEY        CTR(K)
690700920525     C                   MOVEL     §TRDES        DTR(K)
690800930628     C* MINIMO TASSABILE PER TIPO TARIFFA
690900940913     C                   Z-ADD     §TRATA        MIN(K)
691000941124     C* IMPORTO TARIFFA  IN % O £ ("S"=TARIFFA IN % - " "=TARIFFA IN £)
691100941108     C                   MOVEL     §TRTAP        TAP(K)
691200950116     C* SCAGLIONE CON DECIMALI O SENZA ("S"=CON DECIM." "=SENZA DECIM.)
691300941124     C                   MOVEL     §TRSAP        SAP(K)
691400980505     C* GESTIONE DEGLI ARROTONDAMENTI IN TARIFFA
691500980505     C                   MOVEL     §TRARR        ARR(K)
691600980505     C* GESTIONE RAPPORTO PESO / VOLUME
691700980505     C                   MOVEL     §TRRPV        RPV(K)
691800021118     C* Flag abilitazione per tariffe FedEx
691900021118     c                   Movel     §TrUtf        Rutf(K)
692000930628     C                   ADD       1             K
692100930628     C                   ENDIF
692200980505     C                   ENDIF
692300930628     C*
692400940510     C     KTAB2         READE     TABEL                                  32
692500930628     C                   ENDDO
692600920525     C*
692700950116     C****  CODICI NAZIONE : NAZ  ****
692800941011     C                   Z-ADD     1             K
692900941011     C                   MOVEL     '15'          COD
693000941011     C     KTAB2         SETLL     TABEL
693100941011     C     KTAB2         READE     TABEL                                  32
693200941011     C*
693300941011     C     *IN32         DOWEQ     *OFF
693400941011     C     TBLFLG        IFEQ      ' '
693500941011     C                   MOVEL     TBLUNI        DS15
693600950116     C* CARICO SOLO NAZIONE ITALIA
693700950116     C     §15ITA        IFEQ      'I'
693800941011     C                   MOVEL     TBLKEY        NAZ(K)
693900941011     C                   ADD       1             K
694000950116     C                   ENDIF
694100941011     C                   ENDIF
694200941011     C*
694300941011     C     KTAB2         READE     TABEL                                  32
694400941011     C                   ENDDO
694500960520     C*
694600960520     C****  AGGANCIO LA TABELLA "1G"  ****
694700960520     C                   MOVE      '1G'          COD
694800960520     C                   MOVEL     '1       '    KEY
694900960520     C     KTAB          CHAIN     TABEL                              32
695000960520     C     *IN32         IFEQ      *OFF
695100960520     C                   MOVEL     TBLUNI        DS1G
695200960520     C                   ELSE
695300960520     C                   CLEAR                   DS1G
695400960520     C                   ENDIF
695500021118
695600021118      * Tabella dati di default per tariffe FedEx
695700021118
695800021118      * aggancio la tabella DFE
695900021118     c                   clear                   ddfe
696000021118     c                   clear                   tibs02ds
696100021118     c                   movel     'C'           t02mod
696200021118     c                   movel     knsif         t02sif
696300021118     c                   movel     'DFE'         t02cod
696400021118     c                   movel(p)  'FED'         t02ke1
696500021118     c                   call      'TIBS02R'
696600021118     c                   parm                    kpjba
696700021118     c                   parm                    tibs02ds
696800021118
696900021118     c                   if        t02err = *blanks
697000021118     c                   movel     t02uni        ddfe
697100021118     c                   endif
697200101013
697300101013      /free
697400101013       //?Carico scatti ISTAT
697500101013         SISsca = 0;
697600101013         clear DIA;
697700101013         setll (SISsca) TISIS01L;
697800101013
697900101013         DOW  not $EoF;
698000101013
698100101013           read TISIS01L;
698200101013
698300101013           IF  %Eof(TISIS01L);
698400101013             $EoF = *on;
698500101013             leave;
698600101013           ENDIF;
698700101013
698800101013           DIA(SISsca) = SISdata;
698900101013
699000101013         ENDDO;
699100101013
699200101013      /end-free
699300101013
699400050530     c* Carico tabella tariffe particolari in scadenza, da eliminare
699500050530     C                   MOVE      '1P'          COD
699600050530     c                   clear                   xx                4 0
699700050530     c     ktab2         setll     tabel00f
699800050530     c     ktab2         reade     tabel00f
699900050530     c                   dow       not %eof(tabel00f)
700000050530     c                   if        tblflg=' '
700100050530     c                   movel     tbluni        ds1p
700200050530     c                   if        §1peli>*zeros
700300050530     c                   add       1             xx
700400050530     c                   movel     tblkey        tpareli(xx)
700500050530     c                   movel     §1peli        tpardat(xx)
700600050530     c                   endif
700700050530     c                   endif
700800050530     c
700900050530     c     ktab2         reade     tabel00f
701000050530     c                   enddo
701100040915     c*
701200031118      *
701300000000     C                   ENDSR
701400051108
701500051108      *--------------------------------------------------------------------*
701600051108      * CONFERMA ANNULLAMENTO OFFERTA/TARIFFA
701700051108      *--------------------------------------------------------------------*
701800051108     c     Sr_Confann    BegSr
701900051108
702000051108     c   50              Eval      w11tip = 'OFFERTA'
702100051108     c  n50              Eval      w11tip = 'TARIFFA'
702200051108     c                   Clear                   w11sino
702300051108     c                   Do        *Hival
702400051108     c                   Exfmt     ta01w11
702500051108     c   kf              Leave
702600051108     c                   EndDo
702700051108
702800051108     c                   EndSr
702900051108
703000930628     C*
703100940705     C*--- ANNULLAMENTO TOTALE DELLA TARIFFA -------------------------*
703200000000     C     ANNTAR        BEGSR
703300931118     C*
703400941012     C* SOLO SE SONO IN FILIALE EMETTO VIDEATA PER STAMPA LETTERA DI
703500941012     C*   ANNULLAMENTO TARIFFA
703600941012    1C     SIMFEL        IFNE      0
703700940310     C                   MOVE      '4'           PA2FLG
703800920526     C                   EXSR      STAMPA
703900941012    1C                   ENDIF
704000901026     C*
704100940511     C****  A N N U L L O   N O T E  ****
704200940511     C* SE ESISTE ALTRO PROGRESSIVO LASCIO LE NOTE ALTRIMENTI LE DELETO
704300941012     C                   Z-ADD     0             ANN               5 0
704400940615     C  N50KTAM          SETLL     TNTAM000
704500100407     C   50KTAM          SETLL     TNofm000
704600940615     C  N50KTAM          READE     TNTAM000                               21
704700100407     C   50KTAM          READE     TNofm000                               21
704800920526     C*
704900100407    1C     *IN21         DOWEQ     *OFF
705000920526     C     TAMATB        IFEQ      ' '
705100941012     C                   ADD       1             ANN
705200940705     C                   ENDIF
705300061011     C  n50KTAM          READE     TNTAM000                               21
705400100407     C   50KTAM          READE     TNofm000                               21
705500941012    1C                   ENDDO
705600920526     C*
705700941012     C* 18 ON  - SONO IN FILIALE
705800941012     C* ANN < 2  --->  NON ESISTONO ALTRE TARIFFE PER QUEL CLIENTE
705900941012     C*                QUINDI CANCELLO LE NOTE
706000941012    1C   18ANN           IFLT      2
706100920526     C                   MOVEL     CCC           NOTKEY
706200940526     C                   MOVE      V1CKSC        NOTKEY
706300941012     C  N50              MOVEL     'C'           APPL
706400100407     C   50              MOVEL     'V'           APPL
706500100407     c   85              movel     'T'           appl
706600920526     C                   MOVEL     VCTR          NOTKE2
706700920526     C*
706800940615     C     KNTC          SETLL     TFNTC
706900920526     C     KNTC          READE     TFNTC                                  21
707000920526     C*
707100941012    2C     *IN21         DOWEQ     *OFF
707200040806     c                   delete    tfntc
707300920526     C     KNTC          READE     TFNTC                                  21
707400941012    2C                   ENDDO
707500941012    1C                   ENDIF
707600920526     C*
707700970117     C****  ANNULLO FILE DATI TIPO: TNETC/TNETT/TNETS  ****
707800970122     C                   CLEAR                   DSTA47
707900970122     C                   MOVEL     '1'           WTA47             1
708000970117     C     *IN50         IFEQ      *OFF
708100970122     C                   MOVEL     '1'           D47TUP
708200970122     C                   MOVEL     'T'           D47CTO
708300970117     C                   ELSE
708400970122     C                   MOVEL     '2'           D47TUP
708500970122     C                   MOVEL     'O'           D47CTO
708600970117     C                   ENDIF
708700050127     C                   MOVEL     'S'           D47DSF
708800970122     C                   Z-ADD     V1CKSC        D47KSC
708900970122     C                   Z-ADD     VCTR          D47CTR
709000970122     C                   Z-ADD     PRG           D47PRG
709100970122     C                   MOVEL     DSTA47        KPJBU
709200970122     C                   CALL      'TNTA47R'
709300970117     C                   PARM                    KPJBA
709400970117     C*
709500950105     C****  ANNULLO TARIFFE GIACENZA  ****
709600990611     C  N50KTAM2         SETLL     TITGC000
709700990611     C   50KTAM2         SETLL     TIOGC000
709800990611     C  N50KTAM2         READE     TITGC000                               21
709900990611     C   50KTAM2         READE     TIOGC000                               21
710000950105     C*
710100950105    1C     *IN21         DOWEQ     *OFF
710200950105     C* TARIFFA
710300950105    2C  N50TGCATB        IFEQ      ' '
710400040914     C                   Z-ADD     dateu8        TGCDTR
710500950105     C                   MOVEL     *BLANKS       TGCFTR
710600950105     C                   MOVEL     'A'           TGCATB
710700990611     C                   UPDATE    TITGC000
710800030714      *
710900950105    2C                   ENDIF
711000950105     C* OFFERTA
711100990611     C   50              DELETE    TIOGC000
711200950105     C*
711300990611     C  N50KTAM2         READE     TITGC000                               21
711400990611     C   50KTAM2         READE     TIOGC000                               21
711500950105    1C                   ENDDO
711600950105     C*
711700940511     C****  ANNULLO DETTAGLIO TARIFFE PARTICOLARI  ****
711800061011     C  n50KTAM2         SETLL     TITPD000
711900061011     c   50ktam2         setll     tiopd000
712000061011     C  n50KTAM2         READE     TITPD000                               21
712100061011     c   50ktam2         reade     tiopd000                               21
712200931118     C*
712300941012    1C     *IN21         DOWEQ     *OFF
712400931118     C* TARIFFA
712500941012    2C  N50TPDATB        IFEQ      ' '
712600040914     C                   Z-ADD     dateu8        TPDDTR
712700931118     C                   MOVEL     *BLANKS       TPDFTR
712800931118     C                   MOVEL     'A'           TPDATB
712900990601     C                   UPDATE    TITPD000
713000941012    2C                   ENDIF
713100931118     C* OFFERTA
713200061011     C   50              DELETE    TIoPD000
713300931118     C*
713400061011     C  n50KTAM2         READE     TITPD000                               21
713500061011     c   50ktam2         reade     tiopd000                               21
713600941012    1C                   ENDDO
713700931118     C*
713800940511     C****  ANNULLO TESTATA   TARIFFE PARTICOLARI  ****
713900061011     C  n50KTAM2         SETLL     TITPT000
714000061011     c   50ktam2         setll     tiopt000
714100061011     C  n50KTAM2         READE     TITPT000                               21
714200061011     c   50ktam2         reade     tiopt000                               21
714300931118     C*
714400941012    1C     *IN21         DOWEQ     *OFF
714500931118     C* TARIFFA
714600941012    2C  N50TPTATB        IFEQ      ' '
714700040914     C                   Z-ADD     dateu8        TPTDTR
714800931118     C                   MOVEL     *BLANKS       TPTFTR
714900931118     C                   MOVEL     'A'           TPTATB
715000990601     C                   UPDATE    TITPT000
715100030714      *
715200941012    2C                   ENDIF
715300931118     C* OFFERTA
715400061011     C   50              DELETE    TIoPT000
715500931118     C*
715600061011     C  n50KTAM2         READE     TITPT000                               21
715700061011     c   50ktam2         reade     tiopt000                               21
715800941012    1C                   ENDDO
715900931118     C*
716000940511     C****  A N N U L L O   D E T T A G L I O  ****
716100061011     C  n50KTAM2         SETLL     TITAD000
716200061011     c   50ktam2         setll     tiofd000
716300061011     C  n50KTAM2         READE     TITAD000                               21
716400061011     c   50ktam2         reade     tiofd000                               21
716500931118     C*
716600941012    1C     *IN21         DOWEQ     *OFF
716700931118     C* TARIFFA
716800941012    2C  N50TADATB        IFEQ      ' '
716900130124     C*********          Z-ADD     dateu8        TADDTR
717000940511     C                   MOVEL     *BLANKS       TADFTR
717100931118     C                   MOVEL     'A'           TADATB
717200990601     C                   UPDATE    TITAD000
717300941012    2C                   ENDIF
717400931118     C* OFFERTA
717500061011     C   50              DELETE    TIofD000
717600931118     C*
717700061011     C  n50KTAM2         READE     TITAD000                               21
717800061011     c   50ktam2         reade     tiofd000                               21
717900941012    1C                   ENDDO
718000931118     C*
718100940705     C****  A N N U L L O   T E S T A T A      ****
718200061011     C  n50KTAM2         CHAIN     TNTAM000                           21
718300061011     c   50ktam2         chain     tnofm000                           21
718400941012    1C     *IN21         IFEQ      *OFF
718500941012     C* TARIFFA
718600981111    2C     *IN50         IFEQ      *OFF
718700960509     C                   Z-ADD     DATEU8        TAMDUV
718800940511     C                   MOVEL     *BLANKS       TAMFTR
718900040913     C                   Z-ADD     DATEU8        TAMDTR
719000000000     C                   MOVEL     'A'           TAMATB
719100940615     C                   UPDATE    TNTAM000
719200160209      /free
719300160209       //?richiamo pgm per cancellare:
719400160209       //?file Tariffe Inserite
719500160209         clear TRSM90DS;
719600160209         ISM90ksc = TAMksc;
719700160209         ISM90ctr = TAMctr;
719800160209         ISM90prg = TAMprg;
719900160209         trsm90r (kpjba:TRSM90DS);
720000160209      /end-free
720100160209
720200030714      *
720300981111   X2C                   ELSE
720400981111     C* OFFERTA
720500981111     C**
720600061011     C                   DELETE    Tnofm000
720700981111     C**
720800981111     C* VERIFICO SE IL CLIENTE HA ALTRE OFFERTE
720900981111     C                   CLEAR                   WAGG
721000070522     C     V1CKSC        CHAIN(n)  TNofm000                           21
721100981111    3C     *IN21         DOWEQ     *OFF
721200981111    4C     TAMFIE        IFEQ      WFIE
721300981111     C* NON FACCIO NULLA PERCHE' C'E' UN'ALTRA OFFERTA PRESENTE
721400981111     C                   SETON                                        21
721500981111     C                   MOVEL     'N'           WAGG              1
721600090507      * appena trovo una tariffa dello stesso tipo dell'annullata
721700090507      * valorizzo il campo ed esco dal loop
721800090507      * per evitare di anadare sopra al campo WAGG
721900090507      * con una 'S'
722000090507     c                   leave
722100981111   X4C                   ELSE
722200981111     C* TROVATA UNA DI ALTRO TIPO MEMORIZZO
722300981111     C                   MOVEL     'S'           WAGG
722400070522     C     V1CKSC        READE(n)  TNofm000                               21
722500981111    4C                   ENDIF
722600981111    3C                   ENDDO
722700100421      * trattative
722800101104     c                   If        *in85
722900100421      ** TIVOF
723000100712      * Non cancello mai il TIVOF ma imposto esito = ad *
723100100712      * Così evitiamo di cancellare delle offerte non considerate nelle nuove
723200100712      * statistiche
723300100421     c     ktam2         Chain     tivof01l
723400100421     c                   If        %found(tivof01l)
723500100421      * verifico data presentazione
723600100712     c****               movel     vofdpo        w0060
723700100712     c****               if        w0060 <= §sdfstcu
723800100421     c                   eval      vofeso = '*'
723900100421     c                   update    tivof000
724000100712     c****               else
724100100712     c*****              delete    tivof000
724200100712     c***                endif
724300100421     c                   endif
724400100407
724500100421     c                   endif
724600981111    2C                   ENDIF
724700981111    1C                   ENDIF
724800100408
724900100408      /free
725000100408       //?Se note tariffe a livello cliente elimino se non ci sono più
725100100408       //?tariffe/offerte
725200100408       exsr sr_DelNote10;
725300100408      /end-free
725400030716      *
725500030716      * A N N U L L O   S T O R I C O   TITAV01L
725600030716      *
725700050603     c                   if        not *in50
725800030716     C     KTAM2         setll     TITAV01L
725900030716      *
726000030716     C                   do        *hival
726100030716      *
726200030716     C     KTAM2         reade     TITAV01L
726300030716      *
726400030716     C                   if        %eof(TITAV01L)
726500030716     c                   leave
726600030716     c                   endif
726700030716      *
726800030716     c                   eval      TAVatb = 'A'
726900030716     c                   clear                   TAVftr
727000040914     c                   z-add     dateu8        TAVdtr
727100030716      *
727200030716     c                   update    titav
727300030716      *
727400030716     c                   enddo
727500040927      * scrivo un nuovo record di TITAV (annullato) per l'annullamento
727600040927      * della tariffa
727700040927     c                   Eval      annultar = 'S'
727800040927     c                   eval      tavtrd = 'TES'
727900040927     c                   eval      tavcta = 'A'
728000040927     c                   exsr      SUB_STORICO
728100040927     c                   Clear                   annultar
728200050603     c                   endif
728300930223     C*
728400921218     C                   ENDSR
728500100407
728600100407      /free
728700100408       //---------------------------------------------------------------
728800100408       //?Deleto note 10.
728900100408       //---------------------------------------------------------------
729000100407       BEGSR sr_DelNote10;
729100100407
729200100407       //?Provo a cercare note a livello cliente senza niente in key 2
729300100407         clear NOTke2;
729400100408         NTCtnt = '10';
729500100408         setll (appl:notkey:notke2:ntctnt) TFNTC01L;
729600100407         IF  %equal(TFNTC01L);
729700100407         //?ci sono note tariffa a livello cliente, controllo se ci sono
729800100407         //?controllo se ci sono ancora tariffe
729900100407           IF  not *in50;
730000100407             setll v1cksc TNTAM01L;
730100100407             reade v1cksc TNTAM01L;
730200100407             DOW  not %eof(TNTAM01L);
730300100407               IF  tamatb = ' ';
730400100407                 leavesr;
730500100407               ENDIF;
730600100407               reade v1cksc TNTAM01L;
730700100407             ENDDO;
730800100407           ENDIF;
730900100407         //?controllo se ci sono ancora offerte
731000100407           IF  *in50;
731100100407             setll v1cksc TNOFM01L;
731200100407             reade v1cksc TNOFM01L;
731300100407             DOW  not %eof(TNOFM01L);
731400100407               IF  tamatb = ' ';
731500100407                 leavesr;
731600100407               ENDIF;
731700100407               reade v1cksc TNOFM01L;
731800100407             ENDDO;
731900100407           ENDIF;
732000100407         //?se arrivo qua vuol dire che non ho trovato ne tariffe ne offerte
732100100407         //?posso cancellare le note tariffa a livello cliente
732200100408           reade (appl:notkey:notke2:ntctnt) TFNTC01L;
732300100407           DOW  not %eof(TFNTC01L);
732400100407             delete TFNTC;
732500100408             reade (appl:notkey:notke2:ntctnt) TFNTC01L;
732600100407           ENDDO;
732700100407         ENDIF;
732800100407
732900100407       ENDSR;
733000100407      /end-free
733100931118     C*
733200940705     C*--- MUOVO CAMPI DA VIDEO A TNTAM ------------------------------*
733300941012     C     RIETAM        BEGSR
733400980219     C*  VALORE POSITIVO DEI CAMPI NUMERI A VIDEO
733500980219     C                   MLLZO     1             COMDDT
733600980219     C                   MLLZO     1             COMDST
733700101013     C                   MLLZO     1             V1Crct
733800980219     C                   MLLZO     1             V1CPRI
733900980219     C                   MLLZO     1             V1CPAG
734000041129     C                   MLLZO     1             V1CPpa
734100980219     C                   MLLZO     1             V1CKPM
734200980219     C                   MLLZO     1             V1CMPC
734300980219     C                   MLLZO     1             V1CATA
734400980219     C                   MLLZO     1             V1CARL
734500980219     C                   MLLZO     1             V1CARF
734600980219     C                   MLLZO     1             V1CARO
734700980219     C                   MLLZO     1             V2CFIS
734800940518     C*
734900900327     C                   Z-ADD     COMDDT        TAMDDT
735000900327     C                   Z-ADD     COMDST        TAMDST
735100940526     C                   MOVE      V1CCTR        TAMCTR
735200940526     C                   MOVEL     V1CDCV        TAMDCV
735300101013     C                   Z-ADD     V1Crct        TAMrct
735400940705     C                   Z-ADD     V1CPRI        TAMPRI
735500980504     C                   Z-ADD     *ZEROS        TAMDIF
735600940705     C                   Z-ADD     V1CPAG        TAMPAG
735700041129     C                   Z-ADD     v1cppa        TAMPPA
735800980504     C                   Z-ADD     *ZEROS        TAMPAM
735900980504     C                   Z-ADD     *ZEROS        TAMMPA
736000980504     C                   Z-ADD     *ZEROS        TAMMPM
736100940526     C                   MOVEL     V1CTSP        TAMTSP
736200940526     C                   MOVEL     V1CBAP        TAMBAP
736300940526     C                   MOVEL     V1CFTP        TAMFTP
736400150923     c                   IF        V1Ctpr = 'N'
736500150923     c                   eval      TAMtpr = 'NO'
736600150923     c                   ELSE
736700150923     c                   clear                   TAMtpr
736800150923     c                   ENDIF
736900980505     C* VALORIZZO TAMFLO   MODO APPLICAZIONE TARIFFA IN FATTURA
737000980505     C* IN CASO DI IMMISSIONE PULISCO TAMFLO
737100980505     C   01              CLEAR                   TAMFLO
737200980505     C                   MOVE      TAMFLO        DSTA01
737300980505     C                   MOVE      V1CMAT        §TAF02
737400020201     C     V1CNET        IFEQ      'D'
737500020130     C                   MOVE      'S'           §TADPD
737600020130     C                   ENDIF
737700020201     C     V1CNET        IFEQ      'F'
737800020130     C                   MOVE      'S'           §TAFED
737900020130     C                   ENDIF
738000990617     C* VALORIZZO IL CAMPO DELLA DIVISA
738100990617     C                   MOVE      V1CDIV        §TADIV
738200020109     C*
738300020109     C                   MOVE      V1CPCA        §TAPCA
738400020109     C                   MOVE      V1CTAP        §TATAP
738500040601     c                   Movel     V2cbva        §tabva
738600050914     c                   Movel     v1covb        §taovb
738700020109     C*
738800980505     C                   MOVE      DSTA01        TAMFLO
738900940518     C*
739000940705     C* VEDO SE KG. O COLLI PER CALCOLO VOLUME AUTOMATICO
739100940518     C                   Z-ADD     0             TAMKPM
739200940518     C                   MOVEL     *BLANKS       TAMTIN
739300940526     C     V1CKPM        IFNE      0
739400940518     C                   MOVEL     'K'           TAMTIN
739500940526     C                   MOVEL     V1CKPM        TAMKPM
739600940518     C                   END
739700940526     C     V1CMPC        IFNE      0
739800940518     C                   MOVEL     'C'           TAMTIN
739900940526     C                   MOVEL     V1CMPC        TAMKPM
740000940518     C                   END
740100940518     C*
740200940526     C                   Z-ADD     V1CATA        TAMATA
740300940526     C                   Z-ADD     V1CARL        TAMARL
740400940526     C                   Z-ADD     V1CARF        TAMARF
740500940526     C                   Z-ADD     V1CARO        TAMARO
740600940705     C                   MOVEL     V2CFTM        TAMFTM
740700940705     C                   MOVEL     V2CFTS        TAMFTS
740800940526     C                   MOVEL     V1CFIE        TAMFIE
740900941108     C                   MOVEL     V1CFIE        WFIE
741000940705     C                   MOVEL     V2CNAS        TAMNAS
741100940705     C                   MOVEL     V2CNOT        TAMNOT
741200940705     C                   MOVEL     V2CFIS        TAMFIS
741300940705     C                   MOVEL     V2CSIS        TAMSIS
741400940804     C                   MOVEL     V2CFVD        TAMFVD
741500940705     C                   MOVEL     V2CGCA        TAMGCA
741600940705     C                   MOVEL     V2CGMA        TAMGMA
741700940705     C                   MOVEL     V2CGGA        TAMGGA
741800940705     C                   MOVEL     V2CGVA        TAMGVA
741900970128     C                   MOVEL     V2CBAM        TAMBAM
742000120319     c                   IF        v2Internet = 'NO'
742100120319     c                   eval      TAMeds = 'N'
742200120319     c                   ELSE
742300120319     c                   clear                   TAMeds
742400120319     c                   ENDIF
742500120529
742600120529       //?Se tariffa bloccata devo impostare a 'N' il flag
742700120529       //?visualizza su internet
742800150923     c                   IF        TAMbap = 'B' and
742900120529     c                             TAMbap <> sav_TAMbap
743000120529     c                   eval      TAMeds = 'N'
743100120529     c                   ENDIF
743200120529
743300060307     c                   eval      tamfmp = v2cfmp
743400990621     C* PULISCO  I CAMPI CHE NON VENGONO PIU' UTILIZZATI
743500990621     C                   CLEAR                   TAMARS
743600990621     C                   CLEAR                   TAMLAS
743700990621     C                   CLEAR                   TAMLRC
743800990621     C                   CLEAR                   TAMRAT
743900990621     C                   CLEAR                   TAMSC2
744000990617     C* VALORIZZO I CAMPI DELLE SPEDIZIONI MINIME E MASSIME AFFIDATECI
744100990824     C                   Z-ADD     V1CMIS        TAMLRC
744200990824     C                   Z-ADD     V1CMAS        TAMLAS
744300940518     C*
744400900327     C                   ENDSR
744500940518     C*
744600960109     C*--- MUOVO CAMPI DA TNTAM A VIDEO - 1° VIDETATA ----------------*
744700941012     C     RIED01        BEGSR
744800991021     C* TIPO TARIFFA  (SOLO PER COPIA DA OFFERTA)
744900991021     C                   MOVEL     TAMCTR        TIPTAR            1
745000940518     C*
745100940531     C****  DATA SCADENZA TARIFFA  ****
745200940518     C     TAMDDT        IFGT      0
745300940518     C     TAMDST        IFGT      0
745400940729     C                   Z-ADD     TAMDST        DATA
745500940729     C                   Z-ADD     MM            GGMM              4 0
745600940729     C                   MOVEL     GG            GGMM
745700940729     C                   Z-ADD     AA            V1CDST
745800940729     C                   MOVEL     GGMM          V1CDST
745900940518     C                   ENDIF
746000940518     C*
746100940531     C****  DATA DECORRENZA TARIFFA  ****
746200940729     C                   Z-ADD     TAMDDT        DATA
746300940729     C                   Z-ADD     MM            GGMM              4 0
746400940729     C                   MOVEL     GG            GGMM
746500940729     C                   Z-ADD     AA            V1CDDT
746600940729     C                   MOVEL     GGMM          V1CDDT
746700940518     C                   ENDIF
746800940518     C*
746900940531     C****  DATA ULTIMA VARIAZIONE  ****
747000940518     C     TAMDUV        IFGT      0
747100940729     C                   Z-ADD     TAMDUV        DATA
747200940729     C                   Z-ADD     MM            GGMM              4 0
747300940729     C                   MOVEL     GG            GGMM
747400940729     C                   Z-ADD     AA            V1CDUV
747500940729     C                   MOVEL     GGMM          V1CDUV
747600940518     C                   ENDIF
747700940518     C*
747800940518     C                   MOVEL     TAMDS         COMODO
747900030716      * se la ds COMODO$ non è valorizzata
748000030716     C                   if        COMODO$ = *blanks
748100030716     c                   movel     tamds         COMODO$
748200030716     c                   endif
748300030716      *
748400940518     C                   Z-ADD     TAMLAS        COMLAS
748500940518     C*
748600940526     C                   Z-ADD     PRG           V1CPRG
748700940526     C                   MOVE      TAMCTR        V1CCTR
748800940526     C                   MOVE      TAMDCV        V1CDCV
748900101013     C                   Z-ADD     TAMrct        V1Crct
749000101013     C                   Z-ADD     TAMrct        W_V1Crct
749100940705     C                   Z-ADD     TAMPRI        V1CPRI
749200101013    1C     V1Crct        IFGT      0
749300031118      *
749400031118      * visualizzo la decorrenza
749500101013     c                   z-add     dia(v1crct)   g02INV
749600031118     c                   movel     '3'           g02err
749700031118     c                   CALL      'XSRDA8'
749800031118     c                   PARM                    WLBDAT
749900031118      * UDATE A 8 IN GG/MM/AAAA
750000031118     c                   z-add     g02dat        v1diST
750100031118      *
750200031118     c                   endif
750300031118      *
750400940705     C                   Z-ADD     TAMPAG        V1CPAG
750500041129     C                   Z-ADD     TAMPpa        V1CPpa
750600980505     C                   MOVEL     TAMFLO        DSTA01
750700980505     C                   MOVE      §TAF02        V1CMAT
750800020130     C     §TADPD        IFEQ      'S'
750900020201     C                   MOVE      'D'           V1CNET
751000020130     C                   ENDIF
751100020130     C     §TAFED        IFEQ      'S'
751200020201     C                   MOVE      'F'           V1CNET
751300020130     C                   ENDIF
751400020215     C*
751500020215     C                   MOVE      V1CNET        WNET
751600020109     C*
751700020109     C                   MOVE      §TAPCA        V1CPCA
751800020109     C                   MOVE      §TATAP        V1CTAP
751900050914     c                   Movel     §taovb        v1covb
752000150922
752100150923     c                   IF        TAMtpr = 'NO'
752200150923     c                   eval      V1Ctpr = 'N'
752300150923     c                   ELSE
752400150923     c                   clear                   V1Ctpr
752500150923     c                   ENDIF
752600020109     C*
752700991021     C* DIVISA
752800991022     C                   MOVE      §TADIV        V1CDIV
752900990617     C                   MOVEL     'CV'          COD
753000990617     C                   MOVEL     *BLANKS       KEY
753100990622     C                   MOVEL     V1CDIV        KEY
753200990617     C     KTAB          CHAIN     TABEL                              32
753300990617     C  N32              MOVEL     TBLUNI        DSCV
753400980310     C*
753500940526     C                   MOVE      TAMTSP        V1CTSP
753600020215     C                   MOVE      TAMTSP        WTSP
753700940811     C                   MOVEL     '5E'          COD
753800940811     C                   MOVEL     *BLANKS       KEY
753900940811     C                   MOVEL     V1CTSP        KEY
754000940811     C     KTAB          CHAIN     TABEL                              32
754100090608     C  N32              MOVEL     TBLUNI        ds5e
754200090608     C  N32              MOVEL     §5ed08        V1DTSP
754300940811     C   32              CLEAR                   V1DTSP
754400940811     C*
754500120529     c                   eval      sav_TAMbap = TAMbap
754600130604     c                   eval      sav_TAMtsp = TAMtsp
754700940526     C                   MOVE      TAMBAP        V1CBAP
754800940526     C                   MOVE      TAMFTP        V1CFTP
754900940518     C     TAMTIN        IFEQ      'K'
755000940526     C                   Z-ADD     TAMKPM        V1CKPM
755100940518     C                   END
755200940518     C     TAMTIN        IFEQ      'C'
755300940526     C                   MOVEL     TAMKPM        V1CMPC
755400940518     C                   END
755500991021     C*
755600940913     C                   Z-ADD     TAMATA        V1CATA
755700940705     C                   Z-ADD     TAMARL        V1CARL
755800940526     C                   Z-ADD     TAMARF        V1CARF
755900940705     C                   Z-ADD     TAMARO        V1CARO
756000010330     C*
756100940518     C*
756200940526     C                   MOVEL     TAMFIE        V1CFIE
756300941108     C                   MOVEL     TAMFIE        WFIE
756400990617     C* VALORIZZO I CAMPI DELLE SPEDIZIONI MINIME E MASSIME AFFIDATECI
756500990824     C                   Z-ADD     TAMLRC        V1CMIS
756600990824     C                   Z-ADD     TAMLAS        V1CMAS
756700060307      * imposto qua il flag rinuncia AC Base xchè se F21 da prima videata devo
756800060307      * passarlo al pgm delle tariffe particolari
756900060307     c                   movel     tamfmp        v2cfmp
757000080522
757100080522      * controllo se esiste la tariffa particolare fuel
757200080522      * in caso contrario messaggio a video
757300080522     c                   clear                   ds1p
757400080522     c                   eval      cod = '1P'
757500080522     c                   eval      key = 'f '
757600080522     c     ktab          chain     tabel00f
757700080522     c                   if        %found(tabel00f) and tblflg = *blanks
757800080522     c                   eval      ds1p = tbluni
757900080522     c                   endif
758000080522     c                   select
758100080522      * Tariffa con tipo servizio poste
758200080522     c                   when      tamtsp = 'P' and §1pfpt = 'N'
758300080522      * Tariffa con Network DPD
758400080522     c                   when      §tadpd = 'S' and §1pfdp = 'N'
758500080522      * Tariffa con Network FedEx
758600080522     c                   when      §tafed = 'S' and §1pffe = 'N'
758700080522      * Tariffa Italia
758800080522     c                   when      tamfie = 'I' and §1ptco = 'N'
758900080522      * Per tariffa Estero controllo flag §1pfie
759000080522     c                   when      tamfie = 'E' and §1pfie = 'N'
759100080522      * Utilizzabile in tariffa
759200080522     c                   when      §1puta <> 'S'
759300080522     c                   other
759400080522     c                   eval      wtpar = 'f '
759500080522     c  n50ktam7         chain(n)  titpt01l
759600140221     c   50ktam7         chain(n)  tiopt01l
759700080522     c                   if        not %found or tptatb <> *blanks
759800080522     c                   eval      *in28 = *on
759900140220     c                   eval      V1Cmsg = msg(15)
760000080522     c                   endif
760100140220      * se trovo la tariffa Fuel Surcharge devo verificare se la data
760200140220      * di riferimento del prezzo base è > di oggi
760300140224     c                   exsr      ctr_fuel
760400080522    1c                   endsl
760500960109     C*
760600900327     C                   ENDSR
760700900329     C*
760800960109     C*--- MUOVO CAMPI DA TNTAM A VIDEO - 2° VIDETATA ----------------*
760900960109     C     RIED02        BEGSR
761000060303
761100060303     c                   Clear                   at2fmp
761200060303     c                   Clear                   h2riga
761300060303     c                   Clear                   h2colo
761400060303
761500960109     C                   MOVEL     TAMNAS        V2CNAS
761600021118     c                   Eval      *In39 = *Off
761700021118      * Se tariffa FedEx Documenti proteggo il campo della natura merce
761800021118     c                   If        V1cNet = 'F' and DueCtr >= §DfeDalD
761900021118     c                             and DueCtr <= §DfeAlD
762000021118     c                   Eval      *In39 = *On
762100021118     c                   EndIf
762200040601     c                   Movel     §tabva        v2cbva
762300960109     C                   MOVEL     TAMNOT        V2CNOT
762400960109     C                   MOVEL     TAMFIS        V2CFIS
762500960621     C  N50              MOVEL     TAMSIS        V2CSIS
762600960109     C                   MOVEL     TAMFVD        V2CFVD
762700960109     C                   MOVEL     TAMGCA        V2CGCA
762800960109     C                   MOVEL     TAMGGA        V2CGGA
762900960109     C                   MOVEL     TAMGMA        V2CGMA
763000960109     C                   MOVEL     TAMGVA        V2CGVA
763100960109     C                   MOVEL     TAMFTM        V2CFTM
763200960109     C                   MOVEL     TAMFTS        V2CFTS
763300970128     C                   MOVEL     TAMBAM        V2CBAM
763400120319     c                   IF        TAMeds = 'N'
763500120319     c                   eval      v2Internet = 'NO'
763600120319     c                   ELSE
763700120319     c                   clear                   v2Internet
763800120319     c                   ENDIF
763900120319
764000960109     C                   ENDSR
764100960109     C*
764200930630     C*--- LETTURA FILE ----------------------------------------------*
764300900329     C     LEGGI         BEGSR
764400900329     C*
764500900329     C* SE NON COPIO POSIZIONAMENTO  NORMALE
764600101213     C**n50KTAM2         SETLL     TNTAM01l
764700101213     c** 50ktam2         setll     tnofm01l
764800101213     C**n50KTAM2         READE     TNTAM01l                             3601
764900101213     c** 50ktam2         reade     tnofm01l                             3601
765000101213     C  n50KTAM2         chain     TNTAM01l                           0136
765100101213     c   50ktam2         chain     tnofm01l                           0136
765200941007     C*
765300941007     C* 36 ON  - RECORD ALLOCATO
765400950121    1C     *IN36         IFEQ      *ON
765500941012     C                   MOVEL     'A'           PARALL
765600950121   X1C                   ELSE
765700941012     C                   CLEAR                   PARALL
765800941007     C*
765900061010     C  N01TAMATB        COMP      'A'                                    01
766000061010     C  N01              SETON                                        10
766100950121    1C                   ENDIF
766200900329     C** 01  INSERIMENTO
766300900329     C** 10  AGGIORNAMENTO
766400900329     C                   ENDSR
766500900329     C*
766600930630     C*--- STAMPA TARIFFA --------------------------------------------*
766700920525     C     STAMPA        BEGSR
766800920525     C*
766900940310     C                   Z-ADD     PRG           PA2PRG
767000940310     C                   Z-ADD     VCTR          PA2CTR
767100940526     C                   MOVEL     V1DKSC        PA2RGS
767200940310     C* AMBIENTE
767300941013    2C     VFLG          IFNE      '1'
767400940310     C                   MOVEL     'C'           PA2AST
767500940526     C                   Z-ADD     V1CKSC        PA2CLI
767600940311     C                   Z-ADD     0             PA2NRV
767700941013   X2C                   ELSE
767800940310     C                   MOVEL     'V'           PA2AST
767900940526     C                   Z-ADD     V1CKSC        PA2NRV
768000940311     C                   Z-ADD     0             PA2CLI
768100941013    2C                   ENDIF
768200091021      * se trattativa devo impostare new tipo richiamo in pa2ast = 'T'
768300100129      * passo il potenziale e se c'è il cliente
768400091021     c   85              eval      pa2ast = 'T'
768500100112     c   85              eval      pa7cpo = cpox
768600100129     c                   if        *in85 and ksc2 > *zeros
768700100129     c                   eval      pa2cli = %dec(ksc2:7:0)
768800100129     c                   endif
768900101105
769000940311     C                   MOVEL     ' '           PA2TST
769100941013     C*
769200920525     C                   MOVEL     PARAM2        KPJBU
769300920525     C* SE RIF ALTRA TAR DA OFFERTA PRIMA PASSO DA OVRDBF
769400941219     C                   CALL      'FNLV43R'
769500920525     C                   PARM                    KPJBA
769600941013     C*
769700920525     C                   ENDSR
769800920525     C*
769900940510     C*--- CONTROLLO DATE --------------------------------------------*
770000940510     C     CTRDAT        BEGSR
770100940510     C*
770200940510     C                   ADD       1             TAMDST
770300940510     C                   Z-ADD     TAMDST        DATA
770400940510     C* CONTROLLO SE ANNO BISESTILE
770500940510    1C     MM            IFEQ      2
770600940510     C     AA            DIV       4             G02QUA            2 0
770700940510     C                   MVR                     G02RES            1 0
770800940510    2C     G02RES        IFEQ      0
770900940510     C                   Z-ADD     29            XDTF(2)
771000940510    2C                   ENDIF
771100940510    1C                   ENDIF
771200940510     C*
771300940510    2C     GG            IFGT      XDTF(MM)
771400940510     C     GG            SUB       XDTF(MM)      GG
771500940510     C                   ADD       1             MM
771600940510    2C     MM            IFEQ      13
771700940510     C                   ADD       1             AA
771800940510     C                   Z-ADD     1             MM
771900940510    2C                   ENDIF
772000940510    1C                   ENDIF
772100940510     C                   Z-ADD     28            XDTF(2)
772200940510     C*
772300940510     C                   ENDSR
772400980309     C*
772500980309     C*--- CONTROLLO TARIFFA IMPORT ----------------------------------*
772600980309     C     CTRIMP        BEGSR
772700980309     C*
772800980309     C                   MOVE      ' '           IMPORT            1
772900980309     C* CONTROLLO SE LA TARIFFA E' PER L'ITALIA O PER L'ESTERO
773000980309    1C     V1CFIE        IFEQ      'E'
773100980309     C* CONTROLLO SE ESISTE ALMENO UNA LINEA DI PARTENZA ESTERA PER DEFINIRE
773200980309     C* UNA TARIFFA IMPORT
773300980309     C* LEGGO IL DETTAGLIO TARIFFA
773400061011     C  n50KTAM2         SETLL     TITAD000
773500061011     c   50ktam2         setll     tiofd000
773600980309    2C                   DO        *HIVAL
773700061011     C  n50KTAM2         READE     TITAD000                               32
773800061011     c   50ktam2         reade     tiofd000                               32
773900980309    3C     *IN32         IFEQ      *OFF
774000980313     C     TADATB        ANDEQ     ' '
774100980309     C     TADLNP        CHAIN     AZORG01L                           33
774200020312     c                   clear                   og143
774300020312     c                   if        not *in33
774400020312     c                   eval      og143 = orgde3
774500020312     c                   endif
774600020312     c                   if        §ogntw = 'EEX' or §ogntw = 'EUP' or
774700020312     c                             §ogntw = 'FED'
774800020312     c                   eval      *in32 = *on
774900020312     c                   else
775000130318     c***                eval      *in31 = *off
775100130318     c                   eval      wTParEli = *off
775200130318     c                   endif
775300020312
775400980309     C* TARIFFA IMPORT 32 = ON
775500020312     C   32              MOVEL     'Y'           IMPORT
775600980309    3C                   END
775700980309     C*
775800980309    2C  N32              ENDDO
775900980309     C*
776000980309     C* CONTROLLO SE LA TARIFFA PARTICOLARE ASSICURAZIONE PER CONTO E' CORRETTA
776100980309     C* SOLO NEL CASO IN CUI NON SIA STATO RICHIESTO IL CMD 21 MANUTENZIONE
776200980309     C* TARIFFE PARTICOLARI
776300980309     C*
776400980309    2C     *INKV         IFEQ      *OFF
776500980313     C     IMPORT        ANDEQ     'Y'
776600060303     C                   MOVE      'C '          WTPAR
776700070129     C  n50KTAM7         CHAIN(n)  TITPT01L                           33
776800070129     c   50ktam7         chain(n)  tiopt01L                           33
776900980309    3C     *IN33         IFEQ      *OFF
777000980313     C     TPTATB        ANDEQ     ' '
777100980309    4C     TPTAPL        IFNE      'A'
777200980309     C     TPTVLM        OREQ      *ZEROS
777300980309     C                   SETON                                        28
777400980309     C                   MOVEL     MSG(72)       V1CMSG
777500980309    4C                   END
777600980309    3C                   END
777700980309    2C                   END
777800980309     C*
777900980309    1C                   END
778000980309     C*
778100980309     C                   ENDSR
778200000918     C*-------------------------------------------------------------------*
778300000918     C* INGLOBO TARIFFE DI CARTELLO CHE SONO SEMPRE PRESENTI IN TARIFFA
778400000918     C*-------------------------------------------------------------------*
778500000918     C*
778600000918     C     INGLO         BEGSR
778700061011
778800061011     c                   Clear                   Tnta25ds
778900061011     c  n50              Eval      ta25tipo = 'T'
779000061011     c   50              Eval      ta25tipo = 'O'
779100061011     c                   Eval      ta25ksco = v1cksc
779200061011     c                   Move      vctr          ta25ctro
779300061011     c                   Move      prg           ta25prgo
779400061011     c  n50              Eval      ta25tipn = 'T'
779500061011     c   50              Eval      ta25tipn = 'O'
779600061011     c                   Eval      ta25kscn = v1cksc
779700061011     c                   Move      vctr          ta25ctrn
779800061011     c                   Move      prg           ta25prgn
779900061011     c                   Eval      ta25tam = 'N'
780000061011     c                   Eval      ta25fie = v1cfie
780100061011     c                   Eval      ta25tam = 'N'
780200061011     c                   Eval      ta25tad = 'N'
780300061011     c                   Eval      ta25tpt = 'N'
780400061011     c                   Eval      ta25tpd = 'N'
780500061011     c                   Eval      ta25tgc = 'N'
780600061011     c                   Eval      ta25cat = 'N'
780700061011     c                   Eval      ta25inglo = 'S'
780800090917      * imposto se da trattativa
780900110907     c                   eval      %subst(kpjbu:1:1) = partrat
781000110907      * se sono in immissione oppure in aggiunta di una nuova tariffa
781100110907      * dico al TNTA25 di imglobare la tariffa particolare "Q" = ZTL
781200110907      *
781300120403       // -?Da marzo 2012 si ingloba sempre (DV 2236)
781400120403     c******             If        win01 or *in12
781500120403     c******             eval      ta25ztlq = 'S'
781600120403     c******             endif
781700061011
781800061011     c                   Call      'TNTA25R'
781900061011     c                   Parm                    kpjba
782000061011     c                   Parm                    Tnta25ds
782100061011     c                   Parm                    Tnta25tad
782200061011     c                   Parm                    Tnta25tgc
782300061011     c                   Parm                    Tnta25tpd
782400061011     c                   Parm                    Tnta25tpt
782500061011
782600000918     C                   ENDSR
782700080521
782800080521      *-------------------------------------------------------------------*
782900080521      * Integro la tariffa con la tariffa 'f' Fuel
783000080521      *-------------------------------------------------------------------*
783100080521     c     sr_fuel       begsr
783200080521
783300080521     c                   clear                   Tnta25ds
783400080521     c  n50              eval      ta25tipo = 'T'
783500080521     c   50              eval      ta25tipo = 'O'
783600080521     c                   eval      ta25ksco = v1cksc
783700080521     c                   move      vctr          ta25ctro
783800080521     c                   move      prg           ta25prgo
783900080521     c  n50              eval      ta25tipn = 'T'
784000080521     c   50              eval      ta25tipn = 'O'
784100080521     c                   eval      ta25kscn = v1cksc
784200080521     c                   move      vctr          ta25ctrn
784300080521     c                   move      prg           ta25prgn
784400080521     c                   eval      ta25tam = 'N'
784500080521     c                   eval      ta25fie = v1cfie
784600080521     c                   eval      ta25tam = 'N'
784700080521     c                   eval      ta25tad = 'N'
784800080521     c                   eval      ta25tpt = 'N'
784900080521     c                   eval      ta25tpd = 'N'
785000080521     c                   eval      ta25tgc = 'N'
785100080521     c                   eval      ta25cat = 'N'
785200080521     c                   eval      ta25inglo = 'f'
785300090917      * imposto se da trattativa
785400090917     c                   eval      %subst(kpjbu:1:1) = partrat
785500080521
785600080521     c                   call      'TNTA25R'
785700080521     c                   parm                    kpjba
785800080521     c                   parm                    Tnta25ds
785900080521     c                   parm                    Tnta25tad
786000080521     c                   parm                    Tnta25tgc
786100080521     c                   parm                    Tnta25tpd
786200080521     c                   parm                    Tnta25tpt
786300080521
786400080521     c                   endsr
786500080521
786600030715     C*---------------------------------------------------------------*
786700030715     C* ROUTINE DI SCRITTURA FILE STORICO TITAV00F
786800030715     C*---------------------------------------------------------------*
786900030715     C     SUB_STORICO   BEGSR
787000030717      * se sono in offerta oppure sono in immissione non scrivo nulla
787100040927      * non più ora si deve scrivere anche quando viene inserita una nuova
787200040927      * tariffa
787300030717     c                   if        not *in50 and instes = ' '
787400030715      *la prima volta inizializzo i campi del file che vanno sempre bene
787500030715     c                   if        first = ' '
787600030715     C                   eval      TAVksc= v1cksc
787700030715     C                   move      ctr2          TAVctr
787800030715     C                   eval      TAVprg= prg
787900030715      * data variazione
788000030715     C                   eval      TAVdav= dateu8
788100030715     C                   eval      TAVnoj= job_name
788200030715     C                   eval      TAVpru= user
788300030715     C                   eval      first = 'N'
788400030717     c                   endif
788500030717      *
788600030717     c                   clear                   TAVatb
788700040927     c                   If        annultar <> *Blanks
788800040927     c                   Eval      TavAtb = 'A'
788900040927     c                   EndIf
789000030715      * recupero ora di variazione
789100030715     c                   time                    w0120
789200030715     c                   movel     w0120         TAVorv
789300030715      * se non scrivo variazioni alle tariffe particolari pulisco il
789400030715      * codice tariffa particolare
789500030715     c                   if        TAVtrd <> 'PAR'
789600030715     c                   clear                   TAVftc
789700030715     c                   endif
789800040914      *
789900040914     c                   z-add     dateu8        TAVdtr
790000030715      *
790100030715     c                   write     titav000
790200030715     C*
790300030717     c                   endif
790400030717      *
790500030715     C                   ENDSR
790600030715     C***
790700041222     C*---------------------------------------------------------------*
790800041222     C* ROUTINE DI GESTIONE SUBFILE RICHIESTA LINEE PARTENZA DA CREARE
790900041222     C*---------------------------------------------------------------*
791000041222     C     SR_GESLNP     BEGSR
791100041222      *
791200041222      * SE È LA PRIMA VOLTA CHE ENTRO PULISCO IL SUBFILE
791300041222      *
791400041222     c                   IF        W_sbflnp = ' '
791500041222     c                   seton                                        26
791600041222     c                   write     TA01C09
791700041222     c                   setoff                                       26
791800041222     c                   z-add     1             nr2               4 0
791900041222     c                   z-add     1             REC2
792000061012     c                   eval      wforza = '0'
792100041222     c                   endif
792200041222      * zoccolo
792300041222     c                   write     ta01z10
792400041222      *
792500041222     c                   do        *hival
792600041222      *
792700061012     c                   if        nr2 > rec2
792800061012     c                   eval      rec2 = 1
792900061012     c                   endif
793000041222     c                   exfmt     ta01c09
793100041222      *
793200041222     c                   clear                   $msg
793300041222     c                   setoff                                       9828
793400041222      *
793500041222     c                   if        *inkl
793600041222     c                   leave
793700041222     c                   endif
793800041222      *
793900041222      * lettura subfile e controllo
794000041222      *
794100061012     c                   clear                   nr2
794200041222     c                   do        *hival
794300061012     c                   eval      nr2 = nr2 + 1
794400041222      *
794500061012     c     nr2           chain     ta01s09                            30
794600041222      *
794700061012     c                   if        *in30
794800041222     c                   leave
794900041222     c                   endif
795000061012
795100061012     c                   setoff                                       9828
795200041222      *
795300041222      * controllo la linea di partenza
795400041222      *
795500041222     c                   if        vs9lnp > 0
795600041222     c     vs9lnp        chain     azorg01l
795700041222     c                   if        not %found(azorg01l)
795800041222     C                   movel     MSG(22)       $MSG
795900041222     C                   seton                                        9828
796000061012     c                   z-add     nr2           rec2
796100041222     c                   update    ta01s09
796200041222     c                   leave
796300041222     c                   endif
796400041222      * verifico
796500041222     c                   if        orgfva <> ' '  or (orgfag <> 'A' and
796600041222     c                             orgfag <> 'F' and orgfl2 <> 'S')
796700041222     C                   movel     MSG(22)       $MSG
796800041222     C                   seton                                        9828
796900061012     c                   z-add     nr2           rec2
797000041222     c                   update    ta01s09
797100041222     c                   leave
797200041222     c                   endif
797300041222      *
797400041222     c                   movel     orgdes        deslnp
797500061012      * controllo se c'è già un dettaglio tariffario con la linea indicata
797600061012      * messaggio forzabile
797700061012     c                   clear                   wtrov
797800061012     c                   move      vs9lnp        comln2
797900061012     c  n50ktad2         setll     titad000
798000061012     c   50ktad2         setll     tiofd000
798100061012     c  n50ktad2         reade     titad000                               34
798200061012     c   50ktad2         reade     tiofd000                               34
798300061012     c                   dow       not *in34
798400061012     c                   if        tadatb = *blanks
798500061012     c                   eval      *in34 = *on
798600061012     c                   eval      wtrov = '1'
798700061012     c                   else
798800061012     c  n50ktad2         reade     titad000                               34
798900061012     c   50ktad2         reade     tiofd000                               34
799000061012     c                   endif
799100061012     c                   enddo
799200061012     c                   if        wtrov <> *blanks and wforza(nr2) = *off
799300061012     c                   eval      $msg = msg(68)
799400061012     c                   z-add     nr2           rec2
799500061012     c                   seton                                        9828
799600061012     c                   eval      wforza(nr2) = *on
799700061012     c                   update    ta01s09
799800061012     c                   leave
799900061012     c                   endif
800000090608
800100090608      * se la nuova linea di partenza è 950
800200090608    1c                   if        vs9lnp = 950
800300090608      * non posso duplicare se esiste un codice di tassazione IT o EE
800400090608     c                   clear                   wtrov
800500090608     c  n50ktam2         setll     titad04l
800600090608     c   50ktam2         setll     tiofd01l
800700090608    2c                   do        *hival
800800090608     c  n50ktam2         reade     titad04l
800900090608     c   50ktam2         reade     tiofd01l
801000090608     c                   if        %eof
801100090608     c                   leave
801200090608     c                   endif
801300090608     c                   if        tadatb <> *blanks
801400090608     c                   iter
801500090608     c                   endif
801600090608     c                   if        tadcts = 'IT' or tadcts = 'EE'
801700090608     c                   eval      wtrov = '1'
801800090608     c                   leave
801900090608     c                   endif
802000090608    2c                   enddo
802100090608     c                   if        wtrov <> *blanks
802200090608     c                   eval      $msg = msg(121)
802300090608     c                   eval      %subst($msg:36:3) = %editc(tadlnp:'1')
802400090608     c                   z-add     nr2           rec2
802500090608     c                   seton                                        9828
802600090608     c                   update    ta01s09
802700090608     c                   leave
802800090608     c                   endif
802900090608    1c                   endif
803000041222      *
803100041222     c                   else
803200041222      *
803300041222     c                   clear                   deslnp
803400041222      *
803500041222     c                   endif
803600041222      *
803700061012     c                   z-add     nr2           rec2
803800041222     c                   update    ta01s09
803900041222      *
804000041222     c                   enddo
804100041222      *
804200041222     c                   if        *in28
804300041222     c                   iter
804400041222     c                   endif
804500041222      * f6 conferma carico la mia schiera
804600041222     c                   If        *inkf
804700041222     c                   eval      W_sbflnp = '1'
804800041222      * schiera
804900041222     c                   clear                   lnpel
805000041222     c                   z-add     0             nr2
805100041222     c                   z-add     0             po                3 0
805200041222      *
805300041222     c                   do        120           nr2
805400041222     c     nr2           chain     ta01s09
805500041222     c                   if        %found and vs9lnp > 0
805600041222     c                   add       1             po
805700041222     c                   movea     vs9lnp        lnpel(po)
805800041222     c                   If        v8clnn = 0
805900041222     c                   movel     vs9lnp        v8clnn
806000041222     c                   endif
806100041222     c                   endif
806200041222     c                   enddo
806300041222      * se non ho caricato neppure una lnp pulisco W_SBFLNP
806400041222     c                   if        po = 0
806500041222     c                   clear                   W_sbflnp
806600041222     c                   endif
806700041222      *
806800041222     c                   eval      linpar = 'Selezionate più linee'
806900041222     c                   leave
807000041222     c                   endif
807100041222      *
807200041222     c                   enddo
807300041222
807400041222     c                   endsr
807500120314
807600120314      /free
807700120315       //--------------------------------------------------------------
807800120315       //?Imposto i tasti funzionali.
807900120315       //--------------------------------------------------------------
808000120315       BEGSR TastiFun;
808100120315
808200120315       //?Imposto la seconda riga variabile
808300120315         clear wpos;
808400120315         clear wposda;
808500120315         clear wpostf1;
808600120315         clear wpostf2;
808700120315         clear wpostf3;
808800120315         clear wrig;
808900120315         clear wtf1;
809000120315         clear wtf2;
809100120315         clear wtf3;
809200120315
809300120315         tf = 1;
809400120315         FOR tf by 1 to %elem(wTfv);
809500120315
809600120315         //?Abilito F1
809700120315           IF  not *in97 and
809800120315               %subst(wTfv(tf):1:3) = 'F1=';
809900120315             iter;
810000120315           ENDIF;
810100120315
810200120315         //?Abilito Enter
810300120319           IF  (wPrima or wSeconda) and
810400120315               %subst(wTfv(tf):1:3) = 'Ent';
810500120315             iter;
810600120315           ENDIF;
810700120315
810800120315         //?Abilito F2
810900120319           IF  (wQuarta or wTerza or not *in18) and
811000120315               %subst(wTfv(tf):1:3) = 'F2=';
811100120315             iter;
811200120315           ENDIF;
811300120315
811400120315         //?Abilito F6
811500120319           IF  (wPrima or wSeconda or wQuarta or *in15) and
811600120315               %subst(wTfv(tf):1:3) = 'F6=';
811700120315             iter;
811800120315           ENDIF;
811900120315
812000120315         //?Abilito F8 - Note tariffa
812100130905           IF  (wTerza or wQuarta or not *in18) and
812200120315               %subst(wTfv(tf):1:3) = 'F8=';
812300120315             iter;
812400120315           ENDIF;
812500120315
812600120315         //?Abilito F10
812700120315           IF  (wQuarta or (wPrima and not *in10)) and
812800120315               %subst(wTfv(tf):1:3) = 'F10';
812900120315             iter;
813000120315           ENDIF;
813100120315
813200120315         //?Abilito F11
813300120315           IF  (wPrima or wSeconda) and
813400120315               %subst(wTfv(tf):1:3) = 'F11';
813500120315             iter;
813600120315           ENDIF;
813700120315
813800120315         //?Abilito F16
813900130318          // IF  (wTerza or wQuarta or not *in10 or *in17) and
814000130318           IF  (wTerza or wQuarta or not *in31) and
814100120315               %subst(wTfv(tf):1:3) = 'F16';
814200120315             iter;
814300120315           ENDIF;
814400120315
814500120315         //?Abilito F18
814600120319           IF  (wTerza or wQuarta or (wPrima and not *in10) or
814700120319               not *in18 or *in50) and
814800120315               %subst(wTfv(tf):1:3) = 'F18';
814900120315             iter;
815000120315           ENDIF;
815100120315
815200120315           IF  wTfv(tf) <> *blanks;
815300120315             wpos = %scan('#':wTfv(tf));
815400120315             wpostf1 += wpos;
815500120315
815600120315             IF  wpostf1 <= %len(VZFd01);
815700120315               wposda = wpostf1 - wpos + 1;
815800120315               %subst(wtf1:wposda:(wpos-1)) = %subst(wTfv(tf):1:(wpos-1));
815900120315             ELSE;
816000120315               wpostf2 += wpos;
816100120315               IF  wpostf2 <= %len(VZFd01);
816200120315                 wposda = wpostf2 - wpos + 1;
816300120315                 %subst(wtf2:wposda:(wpos-1)) = %subst(wTfv(tf):1:(wpos-1));
816400120315               ELSE;
816500120315                 wpostf3 += wpos;
816600120315                 IF  wpostf3 <= %len(VZFd01);
816700120315                   wposda = wpostf3 - wpos + 1;
816800120315                   %subst(wtf3:wposda:(wpos-1)) = %subst(wTfv(tf):1:(wpos-1));
816900120315                 ENDIF;
817000120315               ENDIF;
817100120315             ENDIF;
817200120315
817300120315           ENDIF;
817400120315         ENDFOR;
817500120315
817600120315       //?Imposto F24 giusto
817700120315         SELECT;
817800120315           WHEN  wtf3 <> *blanks;
817900120315             VZFd02 = cf2413;
818000120315             wf24 = 3;
818100120315           WHEN  wtf2 <> *blanks;
818200120315             VZFd02 = cf2412;
818300120315             wf24 = 2;
818400120315           OTHER;
818500120315             clear VZFd02;
818600120315             wf24 = 1;
818700120315         ENDSL;
818800120315
818900120315         VZFd01 = wtf1;
819000120315         wrig = 1;
819100120315
819200120315       ENDSR;
819300120315
819400120315       //--------------------------------------------------------------
819500120315       //?Gestione tasto funzionale F24
819600120315       //?F24=Altri Tasti
819700120315       //--------------------------------------------------------------
819800120315       BEGSR gesf24;
819900120315
820000120315         SELECT;
820100120315           WHEN  (wf24 = 3 and wrig = 3) or
820200120315                 (wf24 = 2 and wrig = 2);
820300120315             VZFd01 = wtf1;
820400120315             wrig = 1;
820500120315             IF  wf24 = 2;
820600120315               VZFd02 = cf2412;
820700120315             ELSE;
820800120315               VZFd02 = cf2413;
820900120315             ENDIF;
821000120315
821100120315           WHEN  (wf24 = 3 and wrig = 1) or
821200120315                 (wf24 = 2 and wrig = 1);
821300120315             VZFd01 = wtf2;
821400120315             wrig = 2;
821500120315             IF  wf24 = 2;
821600120315               VZFd02 = cf2422;
821700120315             ELSE;
821800120315               VZFd02 = cf2423;
821900120315             ENDIF;
822000120315
822100120315           WHEN  wf24 = 3 and wrig = 2;
822200120315             VZFd01 = wtf3;
822300120315             wrig = 3;
822400120315             VZFd02 = cf2433;
822500120315
822600120315         ENDSL;
822700120315
822800120315       ENDSR;
822900120315
823000120314       //--------------------------------------------------------------
823100120314       //?Visualizza Dati manca tariffa-
823200120314       //--------------------------------------------------------------
823300120314       BEGSR GesW01;
823400120314
823500120314         wFine = *off;
823600120314         W01dsp = ISB48dsp;
823700120314         W01tbl = ISB48tbl;
823800120314         W01lnp = ISB48lnp;
823900120314         W01lna = ISB48lna;
824000120314         W01pro = ISB48pro;
824100120314         W01cts = ISB48cts;
824200120314         W01ctr = ISB48ctr;
824300120314         W01ncl = ISB48ncl;
824400120314         W01pkf = ISB48pkf;
824500120314         W01vlf = ISB48vlf;
824600120314         W01err = ISB48err;
824700120314
824800120314         DOW wFine = *off;
824900120314           exfmt  TA01W01;
825000120314           //?- F12 = Ritorno
825100120314           IF  *inkl;
825200120314             wFine = *on;
825300120314           ENDIF;
825400120314         ENDDO;
825500120314
825600120314       ENDSR;
825700120713
825800120713       //--------------------------------------------------------------
825900120713       //?Ricerca Codice Tassazione
826000120713       //--------------------------------------------------------------
826100120713       BEGSR Ric_CT;
826200120713
826300120713         clear TRTB09DS;
826400120713         TB09fun = '1';
826500120713         TB09ord = '1';
826600120713         IF  V1Cnet = 'D';
826700120713           TB09est = 'E';
826800120713         ELSE;
826900120713           TB09est = V1Cfie;
827000120713         ENDIF;
827100120713         TB09uta = 'S';
827200120713         IF  V1Cnet = 'F';
827300120713           TB09utf = 'S';
827400120713         ELSE;
827500120713           TB09utf = 'N';
827600120713         ENDIF;
827700120713         kpjbu = TRTB09DS;
827800120713         trtb09r (kpjba);
827900120713         TRTB09DS = kpjbu;
828000120713
828100120713       ENDSR;
828200140224
828300140224       //--------------------------------------------------------------
828400140224       //?Controllo tariffa Fuel.
828500140224       //--------------------------------------------------------------
828600140224       BEGSR Ctr_Fuel;
828700140224
828800140224         wNoOkFuel = *off;
828900140224         wtpar = 'f ';
829000140224         IF  not *in50;
829100140224           chain(n) (v1cksc:vctr:prg:wtpar) titpt01l;
829200140224         ENDIF;
829300140224         IF  *in50;
829400140224           chain(n) (v1cksc:vctr:prg:wtpar) tiopt01l;
829500140224         ENDIF;
829600140224
829700140224       //?Se trovo tariffa FUEL verifico che la data di riferimento
829800140224       //?del prezzo base non sia > di oggi
829900140224         IF  %found and TPTatb = *blanks and
830000140224             TPTdpb > dateu8;
830100140224           wNoOkFuel = *on;
830200140224         ENDIF;
830300140224
830400140224       ENDSR;
830500120713
830600120314      /end-free
830700120314
830800000000     C*---------------------------------------------------------------*
830900041222**   XDTF
831000000000312831303130313130313031
831100900320**   TES
831200900321 ***  GESTIONE TARIFFE CLIENTI  ***
831300900327 ***  GESTIONE  VISITE/OFFERTE  ***
831400940607**   MSG
831500940607Data Decorrenza Errata                                                         1
831600940607Data Scadenza Errata                                                           2
831700940607Data Scadenza Incongruente con Data Decorrenza                                 3
831800940607Data Scadenza Incongruente con Data Decorrenza Progressivo Seguente            4
831900940607Data Decorrenza Incongruente con Data Scadenza Progressivo Precedente          5
832000940607Immettere % Addebito Istat                                                     6
832100940607Cliente Esente: non immettere % Istat                                          7
832200120124La Data Scadenza non può essere maggiore di 5 anni da oggi!!                   8
832300940607Codice Tariffa Inesistente                                                     9
832400940607Immettere il Volume Automatico per KG. o per Colli                            10
832500940607Immettere o i KG. o i MC. per Colli per Volume Automatico                     11
832600940912Valore inferiore al minimo previsto dal Tipo Tariffa                          12
832700940607Gli Arrotondamenti devono essere o tutti pieni o tutti lasciati in bianco     13
832800090706Tipo tariffa non prevede il rapporto peso/vol. non immettere utilizzo peso VDL14
832900140220ATTENZIONE!!! Non è stata inserita la tariffa Fuel Surcharge.                 15
833000140226Prezzo base Fuel mai rilevato settimanalmente dal Ministero.                  16
833100110128Tariffa FedEx. Il rapp.peso/volume deve essere uguale su tutti gli scaglioni  17
833200061013                                                                              18
833300061013                                                                              19
833400120315F3 non consentito in Aggiunta Tariffa!!!                                      20
833500940607CMD12 non consentito in Aggiunta Tariffa !!                                   21
833600940607Filiale Inesistente                                                           22
833700940607Codice Tassazione Inesistente                                                 23
833800120906Immettere la filiale                                                          24
833900940607Con Tariffa Regione o Italia non deve essere inserito il C.A.P. del Paese     25
834000940607C.A.P. Incongruente con il Codice Tassazione inserito !!                      26
834100940607Se inserito il C.A.P. non deve esistere la Tariffa Provincia                  27
834200940607Scaglione Obbligatorio                                                        28
834300940607Tariffa Citta' Obbligatoria                                                   29
834400940607Tariffa gia' esistente                                                        30
834500940607L'Importo Massimo deve essere minore o uguale al Minimo                       31
834600940607Fine Scorrimento                                                              32
834700050616Immettere uno dei quattro campi della manipolazione                           33
834800940607Immettere almeno un Codice di Tassazione                                      34
834900940607Immettere lo Scaglione                                                        35
835000940607Codice Tassazione Errato                                                      36
835100940607Effettuare almeno una scelta                                                  37
835200070309Immettere almeno una tariffa particolare                                      38
835300070309Immesse tariffe particolari ma non richiesta la manipolazione                 39
835400070309Tariffa particolare ' ' non presente                                          40
835500070412La tariffa particolare ' ' è esclusa dalla manipolazione tariffe              41
835600940729Tipo Servizio Inesistente o Annullato                                         42
835700940811Natura Merce Obbligatoria                                                     43
835800950105Gestione Valore da Fatturare Obbligatorio                                     44
835900950112Impossibile inserire linea di partenza estera                                 45
836000950118Impossibile inserire scaglione con decimali                                   46
836100950112Impossibile inserire codice tassazione estero                                 47
836200950112Impossibile inserire sia linea partenza che codice tassazione italiani        48
836300940929Impossibile inserire tariffa preferenz. perchè già presente in altro cod.tar. 49
836400941122Codice Italia/Estero Obbligatorio                                             50
836500941122Codice Italia/Estero Errato                                                   51
836600061013                                                                              52
836700020205Impossibile variare flag Italia/Estero                                        53
836800061013                                                                              54
836900061013                                                                              55
837000950119Impossibile inserire gli arrotondamenti con decimali                          56
837100950119Impossibile inserire l'applicazione tariffa finita con decimali               57
837200061013                                                                              58
837300960520Rapporto peso/volume superiore al limite indicato in tabella: Enter x forzare 59
837400960522Immettere almeno una Linea di Partenza                                        60
837500960522Linea di Partenza XXX Inesistente                                             61
837600960523Non esistono righe di dettaglio da copiare per la selezione fatta !!          62
837700960524Se inserite solo le linee partenza impossibile digitarle uguali               63
837800960524Se inseriti sia le linee part. che i cod.tassaz. impossibile digitarli uguali 64
837900960524Immettere la linea partenza                                                   65
838000960524Immettere il codice tassazione                                                66
838100960524Attenzione!! La tariffa che si vuole creare esiste già: Enter x forzare       67
838200960527Attenzione!! Esistono già delle tariffe per la linea da creare:Enter x forzare68
838300960729Inserire arrotondam. perchè gli importi della tariffa non sono a percentuale  69
838400090608Non si puo' inserire una tariffa xxxxxxxx  "ESTERA"                           70
838500061013                                                                              71
838600980309ATTENZIONE !! Tariffa particolare Assic. per conto errata per tariffa IMPORT  72
838700980310E' inutile inserire MIN e/o MAX per scaglioni inferiori al minimo tassabile   73
838800980505Per questa tariffa non viene gestito l'arrotondamento                         74
838900980505Per questa tariffa non e' prevista la gestione del rapporto Peso / Volume     75
839000150922                                                                              76
839100150922                                                                              77
839200990617Il numero massimo delle spedizioni deve essere maggiore del minimo            78
839300990617Non sono ammessi importi con decimali                                         79
839400990617In caso di tariffa in percentuale bastano 2 decimali                          80
839500990623Nazione errata                                                                81
839600061013                                                                              82
839700991008L'arrotondamento deve essere un multiplo di 10                                83
839800991008L'arrotondamento non deve avere l'ultimo decimale diverso da zero             84
839900000118Per tariffe DPD obbligatorio indicare  "NO DDT"                               85
840000000118Non si puo' eliminare il servizio DPD !!!                                     86
840100000118Una tariffa estera non puo' essere dpd                                        87
840200090608Una tariffa DPD o FedEx deve essere soltanto  "xxxxxxxx"                      88
840300061013                                                                              89
840400130514Tipo servizio non può essere variato in manutenzione tariffa.                 90
840500090915Tipo servizio non utilizzabile in tariffa                                     91
840600090915
840700061013                                                                              93
840800010323Si può eseguire un solo arrotondamento                                        94
840900061013                                                                              95
841000150922                                                                              96
841100020130Non si puo' eliminare il servizio FedEx !!!                                   97
841200020130Una tariffa italia non puo' essere FedEx !!!                                  98
841300020205Impossibile inserire Network                                                  99
841400020207Codice tassazione non utlizzabile per tariffa FedEx                           00
841500020207Codice tassazione utilizzabile solo per tariffe FedEx                         01
841600110131Manipolazione Rapporto peso/volume tariffa FedEx. NO I/O su Scaglione!!       02
841700110131Manipolazione Rapporto peso/volume tariffa FedEx. NO I/O su cod.Tassazione!!  03
841800020312Non sono state inserite tariffe particolari, non è possibile la manipolazione 04
841900020318ATTENZIONE!! Manipolazione non eseguita per importi errati                    05
842000050616Effettuare almeno una scelta: dettaglio tariffe o tar.particolari o giacenza! 06
842100020328Per tariffa import/export immettere solo codice tassazione generico 'EE'      07
842200020404Se inserita la Nazione inserire anche il cap!                                 08
842300020404Per tariffa DPD immettere solo codice tassazione generico 'IT'                09
842400020624Linea di partenza 950 non inserire codice tassazione generico 'IT' o 'EE'     10
842500021118Tariffa FedEx non consentita per il codice tariffa inserito                   11
842600021118Scaglione superiore al peso consentito per questa tariffa: ENTER x FORZARE    12
842700130926MANCA SCAGLIONE ISTAT  AVVISARE PRESIDIO VENDITE !!!!!!!!!                    13
842800131029La % applicazione ISTAT non può essere minore di 50 o maggiore di 100         14
842900050530Esiste TarPartic."XX"da eliminare entro xx/xx/xx con F21 o modificare scadenza
843000050617Manipolazione non possibile se immesso un importo di aumento/dimunuzione      16
843100050617Non sono state inserite tariffe di giacenza, non è possibile la manipolazione 17
843200110131Manipolazione Rapporto peso/volume tariffa FedEx. NO I/O su Linea Partenza!!  18
843300060307Impossibile inserire la rinuncia. Presente AC BASE                            19
843400061207Effettuare solo una scelta per il rapporto peso/volume                        20
843500090608Non inserire LNP 950 perchè la LNP     ha codici di tassazione 'IT' o 'EE'    21
843600131029La % applicazione ISTAT non può essere < di 50 o > di 100. ENTER PER FORZARE  22
843700131113
843800120315
843900131028
844000120315** - wTfv (in ordine di visualizzazione) ------------------------------------*
844100120321F1=Manca Tariffa  #
844200120315Enter=Aggiorna  #
844300120315F3=Fine  #
844400120315F2=Rubrica  #
844500120315F6=Aggiorna e fine  #
844600120315F8=Note Tar.  #
844700120315F10=Stampa  #
844800120315F11=Altri Dati  #
844900120315F16=Annulla  #
845000120315F18=Note  #
845100120315F12=Ritorno   #
845200131028
845300131028
845400131028
845500131028
845600131028
845700131028
845800131028
845900131028
846000131028
