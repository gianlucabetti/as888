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
158200151217     c                   IF        not *in50 and *in10 and *in17
158300151217     c                   goto      saltainglo
158400151217     c                   ENDIF
158500000918     C                   EXSR      INGLO
158600151217     c     saltainglo    tag
158700080522
158800080521      * se immissione (tariffa/offerta) devo creare in automatico
158900080521      * la tariffa particolare 'f' = Fuel
159000080521      * se non già presente
159100080610     c                   if        win01
159200080521     c                   exsr      sr_fuel
159300080521     c                   endif
159400080522
159500061206     c                   endif
159600000919     C                   ENDIF
159700030714      * verifico se sono state manutenzionate delle tariffe particolari
159800030714      * scrivo lo storico
159900030714     c                   z-add     0             WW
160000030714      *
160100030714     c                   do        *hival
160200030714     c                   add       1             ww
160300030714
160400030714     c                   if        ww > 150
160500030714     c                   leave
160600030714     c                   endif
160700030714
160800030714     c                   if        manftc(ww) = *blank
160900030714     c                   leave
161000030714     c                   endif
161100030714
161200030714     c                   eval      TAVtrd = 'PAR'
161300030714     c                   movel     manftc(WW)    TAVftc
161400030714     c                   move      manftc(WW)    TAVcta
161500030714     c                   exsr      SUB_storico
161600030714
161700030714     c                   enddo
161800030716      *
161900030716     c                   clear                   manftc
162000030717     c                   clear                   first
162100030717     c                   clear                   instes
162200040927     c                   clear                   annultar
162300000918     C*
162400061011     C  n50KTAM2         SETLL     TNTAM000                               01
162500061011     c   50ktam2         setll     tnofm000                               01
162600061011     C  n50KTAM2         SETLL     TITAD000                               01
162700061011     c   50ktam2         setll     tiofd000                               01
162800011210     C*
162900011214     C* SE PREMUTO F10: OLTRE AGLI AGGIORNAMENTI (DA F6)
163000011210     C*  DEVO FARE LA STAMPA TARIFFA.
163100011214     C   KJ              MOVE      '2'           PA2FLG
163200011214     C   KJ              EXSR      STAMPA
163300070523
163400070523     c     fondo         tag
163500070523
163600941007     C*
163700970718     C* CALL A VUOTO AL PGM FNLV13R PER CHIUDERE I FILES
163800970718    1C     FNLV13        IFEQ      '1'
163900970718     C                   MOVEL     'C'           I13TLA
164000970718     C                   CALL      'FNLV13R'
164100970718     C                   PARM                    KPJBA
164200970718     C                   PARM                    DSLV13
164300970718     C                   PARM                    DSSI95
164400941013    1C                   ENDIF
164500941011     C*
164600970122     C* CALL A VUOTO AL PGM TNTA47R PER CHIUDERE I FILES
164700970122     C     WTA47         IFEQ      '1'
164800970122     C                   CLEAR                   DSTA47
164900970122     C                   MOVEL     'C'           D47TLA
165000970122     C                   MOVEL     DSTA47        KPJBU
165100970122     C                   CALL      'TNTA47R'
165200970117     C                   PARM                    KPJBA
165300970117     C                   ENDIF
165400020205      *
165500020205     C* RITORNO PARAMETRI AL TNTA01R
165600020205     C                   MOVEL     PARAMX        KPJBU
165700120315     c                   IF        *in97
165800120315     c                   eval      parm48 = tnsb48ds
165900120315     c                   ENDIF
166000970117     C*
166100061012     c                   eval      *inlr = *on
166200940503     C*
166300941010     C*--- ROUTINE COMANDI -------------------------------------------*
166400941010     C     COMAND        BEGSR
166500941012     C                   MOVEL     ' '           WFLG              1
166600941010     C*
166700941010     C** CMD14 - GESTIONE TARIFFE GIACENZE
166800941012    1C     *INKN         IFEQ      *ON
166900091210     C                   MOVEL     V1CKSC        pa3cli
167000091210     C  n50              MOVEL     'C'           pa3AST
167100091210     C   50              MOVEL     'V'           pa3AST
167200091210     C                   Z-ADD     V1CCTR        pa3CTR
167300091210     C                   Z-ADD     V1CPRG        pa3PRG
167400981113     C                   MOVEL     V1DCTR        PA3DCT
167500981113     C                   MOVEL     V1CDCV        PA3DCV
167600030714     C                   CLEAR                   PA3MAN
167700961206     C                   MOVEL     V1CFIE        PA3FIE
167800990824     C                   MOVEL     V1CDIV        PA3DIV
167900990622     C                   MOVEL     §CVFDC        PA3FDC
168000020204      * NETWORK
168100020204     C                   MOVE      V1CNET        PA3NET
168200090916      * trattativa
168300090916     c                   eval      pa3trat = partrat
168400010629      *
168500941010     C                   MOVEL     PARAM3        KPJBU
168600060207     C                   CALL      'TNTA14R'
168700941010     C                   PARM                    KPJBA
168800950116     C                   MOVEL     KPJBU         PARAM3
168900030714      *
169000030714     c                   if        pa3MAN <> ' '
169100030714     c                   eval      mangia = 'S'
169200030714     c                   endif
169300950116     C*
169400950116     C* 96 ON  - ESISTONO TARIFFE GIACENZA   : EVIDENZIO IL COMANDO
169500950116     C                   SETOFF                                       96
169600990611     C  N50KTAM2         CHAIN(N)  TITGC000                           32
169700990611     C   50KTAM2         CHAIN(N)  TIOGC000                           32
169800950116    2C  N32TGCATB        IFEQ      ' '
169900950116     C                   SETON                                        96
170000950116    2C                   ENDIF
170100941010     C*
170200941012     C                   MOVEL     '1'           WFLG
170300941010     C                   GOTO      ENDCOM
170400941012    1C                   ENDIF
170500941010     C*
170600941010     C** CMD21 - GESTIONE TARIFFE PARTICOLARI
170700941012    1C     *INKV         IFEQ      *ON
170800140224
170900140224
171000980309     C* CONTROLLO SE LA TARIFFA CHE STO MANUTENZIONANDO E' UNA TARIFFA IMPORT
171100980309     C*
171200980309     C                   EXSR      CTRIMP
171300980309     C*
171400941010     C                   Z-ADD     V1CKSC        CLI5
171500941010     C                   MOVEL     V1DKSC        RAG5
171600941010     C  N50              MOVEL     ' '           FLG
171700941010     C   50              MOVEL     'A'           FLG
171800941010     C                   MOVEL     ' '           FLG4
171900990622     C* MUOVO FLAG DECIMALI SI/NO IN BASE ALLA DIVISA
172000991005     C                   MOVEL     §CVFDC        PA5FDC
172100000915     C* PASSO LA DATA DECORRENZA DELLA TARIFFA
172200000915     C                   Z-ADD     COMDDT        PA5DDT
172300050531     C                   movel     COMDsT        PA5DsT
172400000915     C* PASSO IL TIPO SERVIZIO BOLLA
172500000915     C                   MOVE      V1CTSP        PA5TSP
172600020201     C* Flag Network
172700020201     C                   MOVEL     V1CNET        FLGNET
172800060306      * flag rinuncia AC BASE
172900060306     c                   movel     v2cfmp        pa5fmp
173000090916      * trattativa
173100090916     c                   eval      pa5trat = partrat
173200000915     C*
173300941010     C                   MOVEL     PARAM5        KPJBU
173400120320     c                   IF        not *in97
173500030714     C                   CALL      'TNTA67R'
173600941010     C                   PARM                    KPJBA
173700030714     c                   parm                    manftc
173800120320     c                   ELSE
173900120320     C                   CALL      'TNTA67R'
174000120320     C                   PARM                    KPJBA
174100120320     c                   parm                    manftc
174200120320     c                   parm                    tnsb48ds
174300120320     c                   ENDIF
174400941010     C*
174500941010     C                   MOVEL     KPJBU         PARAM5
174600140224
174700140224      * ricontrollo il Fuel
174800140224     c                   exsr      ctr_fuel
174900140224
175000950116     C*
175100950116     C* SOLO SE NON E' STATO PREMUTO CMD3(FLG4=' ') CONTROLLO SE SONO
175200950116     C*   PRESENTI TARIFFE PARTICOLARI PER EVIDENZIARE IL COMANDO
175300950116    2C     FLG4          IFEQ      ' '
175400950116     C* 95 ON  - ESISTONO TARIFFE PARTICOLARI: EVIDENZIO IL COMANDO
175500950116     C                   SETOFF                                       95
175600061011     C  n50KTAM2         SETLL     TITPT000
175700061011     c   50ktam2         setll     tiopt000
175800061011     C  n50KTAM2         READE(N)  TITPT000                               32
175900061011     C   50ktam2         reade(n)  tiopt000                               32
176000950116    3C     *IN32         DOWEQ     *OFF
176100950116     C     TPTATB        IFEQ      ' '
176200950116     C                   SETON                                        3295
176300950116     C                   ELSE
176400061011     C  n50KTAM2         READE(N)  TITPT000                               32
176500061011     c   50ktam2         reade(n)  tiopt000                               32
176600950116     C                   ENDIF
176700950116    3C                   ENDDO
176800950116     C*
176900950116    2C                   ENDIF
177000941010     C*
177100941011     C                   MOVEL     '1'           WFLG
177200941010     C                   GOTO      ENDCOM
177300941012    1C                   ENDIF
177400950116     C*
177500941010     C** CMD18 - GESTIONE NOTE
177600100407      /free
177700100806       //?Nuovo programma gestione note
177800100806        IF  *inks;
177900100407          clear trmk20ds;
178000110511          IMK20TLA = 'L' ;
178100100407
178200101126         //?passo trattativa  (abbiamo tolto richiamo delle note da trattativa)
178300100407          IF  *in85;
178400100407            IMK20nrv = v1cksc;
178500100407         //?passo cliente
178600100407          ELSE;
178700100407            IMK20ksc = v1cksc;
178800100407          ENDIF;
178900100407         //?ragione sociale
179000100407          IMK20rsc = v1dksc;
179100100407
179200100407          trmk20r (kpjba : TRMK20DS);
179300100407          wflg = '1';
179400100407          leavesr;
179500100407        ENDIF;
179600100806
179700100407       //?F8 - Note tariffa
179800100806        IF  *inkh;
179900100407          clear TNTA28ds;
180000100407
180100100407          ITA28ela = 'M';
180200100407
180300100407         //?richiamo per trattativa
180400100407          IF  *in85;
180500100407            ITA28tip = 'T';
180600100407          ELSE;
180700100407         //?richiamo per tariffe clienti
180800100407            ITA28tip = 'C';
180900100407          ENDIF;
181000100407
181100100407          ITA28cod = V1cksc;
181200100407          ITA28rsc = V1dksc;
181300100407          kpjbu    = TNTA28DS;
181400100407
181500100407          tnta28r (kpjba);
181600100407          wflg = '1';
181700100407          leavesr;
181800100407        ENDIF;
181900100407      /end-free
182000090709
182100090709     C** F2=RUBRICA
182200090709    1c                   if        *inkb
182300090709     c                   clear                   tnta12ds
182400090709     c                   eval      ta12ric = 'M'
182500090709     c  n50              Eval      ta12apl = 'C'
182600090709     c  n50              Eval      ta12ksc = v1cksc
182700090916     c  n85
182800090916     can 50              Eval      ta12apl = 'V'
182900090916     c   85              Eval      ta12apl = 'T'
183000090709     c   50              Eval      ta12nrv = v1cnrv
183100090709     c                   Eval      ta12rag = v1dksc
183200090709     c                   Call      'TNTA12R'
183300090709     c                   Parm                    kpjba
183400090709     c                   Parm                    tnta12ds
183500090709     C*
183600090709     C                   MOVEL     '1'           WFLG
183700090709     C                   GOTO      ENDCOM
183800090709    1C                   ENDIF
183900941010     C*
184000941010     C     ENDCOM        ENDSR
184100941010     C*
184200941013     C*--- PULIZIA CAMPI VIDEATA -------------------------------------*
184300941012     C     INZD01        BEGSR
184400031118     C                   SETOFF                                       8836
184500941124     C                   CLEAR                   WSAP
184600980505     C                   CLEAR                   WRPV
184700980505     C                   CLEAR                   WRKMIN
184800940503     C*
184900940526     C                   Z-ADD     PRG           V1CPRG
185000020201     C                   CLEAR                   V1CNET
185100000118     C                   CLEAR                   V1CDDT
185200940526     C                   CLEAR                   V1CDST
185300940526     C                   CLEAR                   V1CDUV
185400940526     C                   CLEAR                   V1DCTR
185500940526     C                   CLEAR                   V1CDCV
185600101013     C                   CLEAR                   V1Crct
185700940526     C                   CLEAR                   V1CPRI
185800031118     C                   CLEAR                   V1DIST
185900031118     c                   CLEAR                   PARIST
186000940526     C                   CLEAR                   V1CPAG
186100041129     C                   CLEAR                   V1CPpa
186200940526     C                   CLEAR                   V1CKPM
186300940526     C                   CLEAR                   V1CMPC
186400940526     C                   CLEAR                   V1CARL
186500940526     C                   CLEAR                   V1CARF
186600940526     C                   CLEAR                   V1CARO
186700940526     C                   CLEAR                   V1CATA
186800940526     C                   CLEAR                   V1CFTP
186900940526     C                   CLEAR                   V1CBAP
187000940526     C                   CLEAR                   V1CTSP
187100940811     C                   CLEAR                   V1DTSP
187200041012     c                   Eval      v1cdiv = 'EUR'
187300990616     C                   CLEAR                   V1CMIS
187400990616     C                   CLEAR                   V1CMAS
187500940526     C                   CLEAR                   V1CFIE
187600980505     C                   CLEAR                   V1CMAT
187700150922     C                   CLEAR                   V1Ctpr
187800941108     C                   CLEAR                   WFIE
187900020215     C                   CLEAR                   WTSP
188000020215     C                   CLEAR                   WNET
188100940705     C                   MOVEL     '0'           V2CFIS
188200960621     C  N50              MOVEL     'S'           V2CSIS
188300960621     C   50              CLEAR                   V2CSIS
188400940705     C                   MOVEL     '1'           V2CFTS
188500941108     C* 16 ON  - TIPO TARIFFA "4"-A VALORE
188600940811     C   16              MOVEL     '1'           V2CFVD
188700970128     C                   MOVEL     'N'           V2CBAM
188800940705     C                   CLEAR                   V2CNAS
188900040601     c                   Clear                   v2cbva
189000940705     C                   CLEAR                   V2CNOT
189100060303     c                   clear                   v2cfmp
189200940705     C                   CLEAR                   V2CFTM
189300940705     C                   CLEAR                   V2CGCA
189400940705     C                   CLEAR                   V2CGGA
189500940705     C                   CLEAR                   V2CGMA
189600940705     C                   CLEAR                   V2CGVA
189700941012     C                   CLEAR                   V2CGCR
189800941012     C                   CLEAR                   V2CGGR
189900941012     C                   CLEAR                   V2CGMR
190000941012     C                   CLEAR                   V2CGVR
190100940503     C*
190200131028     C                   CLEAR                   savpri
190300131028     C*
190400970317     C                   MOVEL     *BLANKS       COMODO
190500030716     C                   MOVEL     *BLANKS       COMODO$
190600940526     C                   MOVE      VCTR          V1CCTR
190700000000     C                   Z-ADD     0             COMLRC            7 0
190800000000     C                   Z-ADD     0             COMLAS            7 0
190900061011      * se sono in inserimento e non in copia 01 = *on
191000031118      * propongo il primo scaglione ISTAT con data scatto successiva
191100031118      * la data del giorno
191200061011     c                   If        *in01
191300120124     C                   Z-ADD     0             PI                4 0
191400031118     c                   z-add     0             svdia             8 0
191500101013     c                   z-add     0             svrct             3 0
191600101013    3C                   DO        999           PI
191700031118      * verifico la data scatto con la data del giorno
191800031121    3C                   IF        dateu8 < DIA(PI)
191900031118      * salvo la data e lo scaglione
192000031118     c                   eval      svdia = dia(pi)
192100101013     c                   eval      svrct = pi
192200031121     c                   leave
192300031118     c                   endif
192400031118      *
192500031118     c                   enddo
192600031118      * se non trovato scaglione ritorno al TNTA01R con  errore bloccante
192700101013     c                   if        svdia = 0  and svrct = 0
192800031118     c                   eval      parist = 'E'
192900031118     c                   seton                                        36
193000031118     c                   goto      endinz
193100031118     c                   endif
193200031118      *
193300101013     c                   eval      v1crct = svrct
193400031118     c                   eval      v1cpri = 100
193500031118      *
193600031118     c                   z-add     svdia         g02INV
193700031118     c                   movel     '3'           g02err
193800031118     c                   CALL      'XSRDA8'
193900031118     c                   PARM                    WLBDAT
194000031118      * GG/MM/AAAA
194100031118     c                   z-add     g02DAT        v1diST
194200031118      *
194300031118     c                   endif
194400031118      *
194500991119     C*
194600941012     C*
194700941012     C* DECODIFICA CODICE TARIFFA
194800941012     C                   Z-ADD     1             A
194900920525     C                   MOVEL     VCTR          UNOCTR
195000940524     C     UNOCTR        LOOKUP    CTR(A)                                 35
195100941108    1C     *IN35         IFEQ      *ON
195200941108     C                   MOVEL     DTR(A)        V1DCTR
195300941124     C* 88 ON  - IMPORTO TARIFFA IN PERCENTUALE
195400941108    2C     TAP(A)        IFEQ      'S'
195500941123     C                   SETON                                        88
195600941108    2C                   ENDIF
195700950116     C* WSAP = "S" ---> SCAGLIONE CON DECIMALI
195800941124     C                   MOVEL     SAP(A)        WSAP              1
195900980505     C* WRPV = "S" ---> ABILITATA LA GESTIONE DEL RAPPORTO PESO VOLUME
196000980505     C                   MOVEL     RPV(A)        WRPV              1
196100980505     C* WRKMIN = SALVO IL MINIMO TASSABILE PER QUESTA TARIFFA
196200980505     C                   Z-ADD     MIN(A)        WRKMIN           13 3
196300941108    1C                   ENDIF
196400941012     C*
196500061011     C* CONTROLLO SE DEVO EVIDENZIARE I COMANDI
196600950121     C                   SETOFF                                       9596
196700950121     C*
196800950116     C* 95 ON  - ESISTONO TARIFFE PARTICOLARI: EVIDENZIO IL COMANDO
196900061011     C  n50KTAM2         SETLL     TITPT000
197000061011     c   50ktam2         setll     tiopt000
197100061011     C  n50KTAM2         READE(N)  TITPT000                               32
197200061011     c   50ktam2         reade(n)  tiopt000                               32
197300950116    3C     *IN32         DOWEQ     *OFF
197400950116     C     TPTATB        IFEQ      ' '
197500950116     C                   SETON                                        3295
197600950116     C                   ELSE
197700061011     C  n50KTAM2         READE(N)  TITPT000                               32
197800061011     c   50ktam2         reade(n)  tiopt000                               32
197900950116     C                   ENDIF
198000950116    3C                   ENDDO
198100950116     C*
198200950116     C* 96 ON  - ESISTONO TARIFFE GIACENZA   : EVIDENZIO IL COMANDO
198300990611     C  N50KTAM2         CHAIN(N)  TITGC000                           32
198400990611     C   50KTAM2         CHAIN(N)  TIOGC000                           32
198500950116     C  N32TGCATB        IFEQ      ' '
198600950116     C                   SETON                                        96
198700950116     C                   ENDIF
198800080610      * salvo l'indicatore di nuova tariffa/offerta
198900080610     c                   eval      win01 = *in01
199000950116     C*
199100031118     C     endinz        ENDSR
199200940503     C*
199300941013     C*--- CONTROLLO DATI FORMATO 1 ----------------------------------*
199400941012     C     CTRD01        BEGSR
199500940729     C                   SETOFF                                       27
199600940503     C*
199700940503     C****  DATA DECORRENZA TARIFFA  ****
199800940729     C                   Z-ADD     V1CDDT        G02DAT
199900940701     C                   MOVEL     *BLANKS       G02ERR
200000940701     C                   CALL      'XSRDA8'
200100940503     C                   PARM                    WLBDAT
200200940701    1C     G02ERR        IFEQ      '1'
200300940526     C                   MOVEL     MSG(1)        V1CMSG
200400940526     C                   SETON                                        4028
200500940503     C                   GOTO      ENDCTR
200600940503   X1C                   ELSE
200700940701     C                   MOVE      G02INV        COMDDT            8 0
200800940729     C*
200900940729     C* IMPOSTO A VIDEO LA DATA A 8 SE IMMESSA A 6
201000940729     C                   Z-ADD     G02DAT        V1CDDT
201100940503    1C                   ENDIF
201200940503     C*
201300940503     C****  DATA SCADENZA TARIFFA  ****
201400940729     C                   Z-ADD     V1CDST        G02DAT
201500940701     C                   MOVEL     *BLANKS       G02ERR
201600940701     C                   CALL      'XSRDA8'
201700940503     C                   PARM                    WLBDAT
201800940701    1C     G02ERR        IFEQ      '1'
201900940526     C                   MOVEL     MSG(2)        V1CMSG
202000940526     C                   SETON                                        4128
202100940503     C                   GOTO      ENDCTR
202200940503   X1C                   ELSE
202300940701     C                   MOVE      G02INV        COMDST            8 0
202400940729     C*
202500940729     C* IMPOSTO A VIDEO LA DATA A 8 SE IMMESSA A 6
202600940729     C                   Z-ADD     G02DAT        V1CDST
202700940503    1C                   ENDIF
202800940503     C*
202900940503     C* LA DATA SCADENZA DEVE ESSERE MAGGIORE DELLA DATA DECORRENZA
203000940503    1C     COMDST        IFLT      COMDDT
203100940526     C                   MOVEL     MSG(3)        V1CMSG
203200940526     C                   SETON                                        4128
203300940503     C                   GOTO      ENDCTR
203400940503    1C                   ENDIF
203500120124
203600120124      /free
203700120124       //?La data scadenza non deve essere superiore ad oggi + 5 anni
203800120124       IF  comdst > 0;
203900120124         wDate_ISO = %date(comdst:*iso);
204000120124         IF  %diff(wDate_ISO : wOggi : *years) >= 5;
204100120124           V1Cmsg = msg(08);
204200120124           *in28 = *on;
204300120124           *in41 = *on;
204400120124           leavesr;
204500120124         ENDIF;
204600120124       ENDIF;
204700120124      /end-free
204800120124
204900900327     C*
205000940503     C* VFLG <> '1'  --->  S O L O    P E R    T A R I F F E
205100940503     C*                    CONTROLLO CONGRUENZA DECORRENZE/SCADENZE
205200940503    1C     VFLG          IFNE      '1'
205300940503     C*
205400940503     C* LA DATA SCADENZA DEL PROGRESSIVO PRECEDENTE DEVE ESSERE MINORE
205500940503     C*   DELLA DATA DECORRENZA DEL PROGRESSIVO SEGUENTE
205600940526     C                   Z-ADD     V1CKSC        COMKSC
205700900620     C                   Z-ADD     VCTR          COMCTR
205800940503     C     PRG           ADD       1             COMPRG
205900940503     C*
206000940615     C     KTAM5         SETLL     TNTAM000
206100940615     C     KTAM5         READE(N)  TNTAM000                               32
206200940503     C*
206300940510    2C     *IN32         DOWEQ     *OFF
206400940509     C     TAMATB        IFEQ      ' '
206500940509     C     COMDST        IFGE      TAMDDT
206600940526     C                   MOVEL     MSG(4)        V1CMSG
206700940526     C                   SETON                                        4128
206800940503     C                   GOTO      ENDCTR
206900940503     C                   ELSE
207000940510     C                   SETON                                        32
207100940503     C                   ENDIF
207200940503     C*
207300940503     C                   ELSE
207400940503     C* RECORD ANNULLATO: AGGIUNGO 1 E CERCO IL PROGRESSIVO SEGUENTE
207500940503     C                   ADD       1             COMPRG
207600940503     C                   ENDIF
207700940503     C*
207800940615     C  N32KTAM5         READE(N)  TNTAM000                               32
207900940503    2C                   ENDDO
208000940503     C*
208100940503     C* LA DATA DECORRENZA DEL PROGRESSIVO SEGUENTE DEVE ESSERE
208200940503     C*   MAGGIORE DELLA DATA SCADENZA DEL PROGRESSIVO PRECEDENTE
208300940503     C     PRG           SUB       1             COMPRG
208400940615     C     KTAM5         SETLL     TNTAM000
208500940615     C     KTAM5         READE(N)  TNTAM000                               32
208600940503     C*
208700940510    2C     *IN32         DOWEQ     *OFF
208800940503     C     TAMATB        IFEQ      ' '
208900940503     C     COMDDT        IFLE      TAMDST
209000940526     C                   MOVEL     MSG(5)        V1CMSG
209100940526     C                   SETON                                        4028
209200940503     C                   GOTO      ENDCTR
209300940503     C                   ELSE
209400940506     C*
209500940509     C* LA DATA DECORRENZA PROGRESSIVO SEGUENTE DEVE ESSERE MAGGIORE
209600940509     C*   DI 1 GIORNO DELLA DATA SCADENZA PROGRESSIVO PRECEDENTE
209700940510     C                   EXSR      CTRDAT
209800940506     C*
209900940510     C     COMDDT        IFNE      DATA
210000940526     C                   MOVEL     MSG(5)        V1CMSG
210100940526     C                   SETON                                        4028
210200940506     C                   GOTO      ENDCTR
210300940510     C                   ENDIF
210400940506     C*
210500940510     C                   SETON                                        32
210600940503     C                   ENDIF
210700940503     C*
210800940503     C                   ELSE
210900940503     C* RECORD ANNULLATO: SOTTRAGGO 1 E CERCO IL PROGRESSIVO PRECEDENTE
211000940503     C                   SUB       1             COMPRG
211100940506     C* RIESEGUO POSIZIONAMENTO CON PROGRESSIVO PRECEDENTE
211200940615     C     KTAM5         SETLL     TNTAM000
211300940503     C                   ENDIF
211400940503     C*
211500940615     C  N32KTAM5         READE(N)  TNTAM000                               32
211600940503    2C                   ENDDO
211700940503     C****
211800940509    1C                   ENDIF
211900130318
212000130318      /free
212100130320       //?Manutenzione Tariffa no cartello
212200130320       IF  not *in50 and *in10 and not *in23;
212300130318       //?Data decorrenza
212400130318       //?se <= data ultima spedizione fatturata
212500130318       //?non posso modificarla
212600130318       IF  sav_TAMddt <= wDataSP and sav_TAMddt <> comddt;
212700130318         *in40 = *on;
212800130318         *in28 = *on;
212900130318         V1Cmsg = 'Data Decorezza non modificabile, minore '+
213000130318                  'data ultima sped.fatt. ' +
213100130318                  %subst(%editc(wDataSP:'X'):7:2) + '/' +
213200130318                  %subst(%editc(wDataSP:'X'):5:2) + '/' +
213300130318                  %subst(%editc(wDataSP:'X'):1:4);
213400130318         leavesr;
213500130318       ENDIF;
213600130318       //?se > data ultima spedizione fatturata
213700130318       //?posso impostarla solo > alla data ultima sp.fatturata
213800130318       IF  sav_TAMddt > wDataSP and comddt <> sav_TAMddt and
213900130318           comddt <= wDataSP;
214000130318         *in40 = *on;
214100130318         *in28 = *on;
214200130318         V1Cmsg = 'Data Decor. deve essere più '+
214300130318                  'alta della data ultima sped.fatt. ' +
214400130318                  %subst(%editc(wDataSP:'X'):7:2) + '/' +
214500130318                  %subst(%editc(wDataSP:'X'):5:2) + '/' +
214600130318                  %subst(%editc(wDataSP:'X'):1:4);
214700130318         leavesr;
214800130318       ENDIF;
214900130318
215000130318       //?Data scadenza
215100130318       //?se <= data ultima spedizione fatturata
215200130318       //?non posso metterla più bassa di quella che c'è
215300130318       //?già sul file
215400130318       IF  sav_TAMdst <= wDataSP and comdst <> sav_TAMdst and
215500130318           comdst < sav_TAMdst;
215600130318         *in41 = *on;
215700130318         *in28 = *on;
215800130318         V1Cmsg = 'Data scadenza <= data ultima sped.fatt. '+
215900130318                  %subst(%editc(wDataSP:'X'):7:2) + '/' +
216000130318                  %subst(%editc(wDataSP:'X'):5:2) + '/' +
216100130318                  %subst(%editc(wDataSP:'X'):1:4) + ' ' +
216200130318                  'non si può anticipare.';
216300130318         leavesr;
216400130318       ENDIF;
216500130318       //?se > data ultima spedizione fatturata
216600130318       //?posso impostarla solo >= alla data ultima sp.fatturata
216700130318       IF  sav_TAMdst > wDataSP and comdst <> sav_TAMdst and
216800130318           comdst < wDataSP;
216900130318         *in41 = *on;
217000130318         *in28 = *on;
217100130319         V1Cmsg = 'Data Scadenza deve essere = o più '+
217200130318                  'alta della data ultima sped.fatt. ' +
217300130318                  %subst(%editc(wDataSP:'X'):7:2) + '/' +
217400130318                  %subst(%editc(wDataSP:'X'):5:2) + '/' +
217500130318                  %subst(%editc(wDataSP:'X'):1:4);
217600130318         leavesr;
217700130318       ENDIF;
217800130320       ENDIF;
217900130318      /end-free
218000151029
218100151029     C****  CODICE DIVISA    ****
218200151029     C                   MOVEL     'CV'          COD
218300151029     C                   MOVEL     *BLANKS       KEY
218400151029     C                   MOVEL     V1CDIV        KEY
218500151029     C     KTAB          CHAIN     TABEL                              32
218600151029    1C  N32TBLFLG        IFEQ      ' '
218700151029     C                   MOVEL     TBLUNI        DSCV
218800151029    1C                   ENDIF
218900900327     C*
219000941010     C****  FLAG ITALIA/ESTERO  ****
219100941028     C* PER LE TARIFFE CLIENTI     DEVE ESSERE OBBLIGATORIAMENTE PIENO
219200941028     C* PER LE TARIFFE DI CARTELLO DEVE ESSERE OBBLIGATORIAMENTE VUOTO
219300941028    1C     *IN23         IFEQ      *OFF
219400941028     C* 23 OFF - TARIFFE CLIENTI
219500941028    2C     V1CFIE        IFEQ      ' '
219600941010     C                   MOVEL     MSG(50)       V1CMSG
219700941010     C                   SETON                                        8728
219800941010     C                   GOTO      ENDCTR
219900941028    2C                   ENDIF
220000941028     C*
220100941107     C* CONTROLLO VALIDITA' DEL FLAG ITALIA/ESTERO
220200941107    2C     V1CFIE        IFNE      'I'
220300941109     C     V1CFIE        ANDNE     'E'
220400941107     C                   SETON                                        8728
220500941107     C                   MOVEL     MSG(51)       V1CMSG
220600941107     C                   GOTO      ENDCTR
220700941107    2C                   ENDIF
220800941107     C*
220900941028   X1C                   ELSE
221000941028     C* 23 ON  - TARIFFE DI CARTELLO
221100941028    1C                   ENDIF
221200941028     C*
221300020205     C* IN VARIAZIONE(10 ON) NON E' POSSIBILE VARIARE IL FLAG ITALIA/ESTERO
221400020213     C* SE DIVERSO DA BLANK
221500020213     C*
221600941108    1C   10V1CFIE        IFNE      WFIE
221700020213     C     WFIE          ANDNE     ' '
221800941107     C                   MOVEL     MSG(53)       V1CMSG
221900941012     C                   SETON                                        8728
222000941012     C                   GOTO      ENDCTR
222100941028    1C                   ENDIF
222200160218
222300941010     C*
222400940504     C****  FLAG ISTAT  ****
222500031118     C* SE E' STATO INSERITO IL FLAG ISTAT OCCORRE INSERIRE ANCHE
222600031118     C*   LA % ADDEBITO ISTAT E VICEVERSA
222700101013    1C     V1Crct        IFGT      0
222800031118     C     V1CPRI        IFEQ      0
222900031118     C                   MOVEL     MSG(6)        V1CMSG
223000031118     C                   SETON                                        4228
223100031118     C                   GOTO      ENDCTR
223200031118     C                   ENDIF
223300031119      * se non esiste lo scaglione (data scatto uguale a zero) do errore
223400031119      *
223500101013     c                   if        dia(v1crct) = 0
223600031119     c                   movel     msg(113)      v1cmsg
223700031119     c                   seton                                        4328
223800031119     c                   goto      endctr
223900031119     c                   endif
224000131029      *
224100131029     c                   if        savpri <> v1cpri
224200131113     c*                  clear                   forzamsgistat
224300131029     c                   endif
224400131113      * se sono in immisisone imposto tampri uguale a zero
224500131113     c                   If        *in01
224600131113     c                   clear                   tampri
224700131113     c                   endif
224800131031      *
224900131028      * la percentuale di addebito non deve essere > di 100 o < di 50
225000131028      * se
225100131028     c                   If        v1cpri > 100 OR V1CPRI < 50
225200131029      *
225300131028      * se sono in gestione OFFERTA su trattativa NUOVA quindi senza cliente
225400131028      * ERRORE NON FORZABILE
225500131029     c                   if        (*in50 and ksc2 = *zeros)
225600050302     c                   Eval      v1cmsg = msg(114)
225700050303     c                   Eval      *In42 = *On
225800050302     c                   Eval      *In28 = *On
225900050302     c                   Goto      endctr
226000050302     c                   EndIf
226100131028      * se sto gestendo un'offerta legata ad una trattativa su cliente
226200131028      * il messaggio  sulla % applicazione ISTAT diventa forzabile
226300131113      * se si mantiene la stessa % del file ma se si modifca dare errore
226400131113      * bloccante se non soddisfa >=50 e <= 100
226500131029      *                  oppure
226600131029      *
226700131113      * se sto aggiungendo un nuovo progressivo la 1° volta è un messaggio
226800131113      * informativo
226900131029     c                   if        ((*in50 and ksc2 > *zeros) or
227000131113     c                              not *in17 )
227100131113
227200131113     c                   if        savpri <> v1cpri and
227300131113     c                             tampri = v1cpri and
227400131113     c                             forzaMsgIstat = *blanks
227500131028     c                   Eval      v1cmsg = msg(122)
227600131028     c                   Eval      *In42 = *On
227700131028     c                   Eval      *In28 = *On
227800131028     c                   Eval      forzaMsgIstat = '1'
227900131028     c                   eval      savpri = v1cpri
228000131028     c                   Goto      endctr
228100131113     c                   endif
228200131113      *
228300131113     c                   If        v1cpri <> tampri
228400131113     c                   Eval      v1cmsg = msg(114)
228500131113     c                   Eval      *In42 = *On
228600131113     c                   Eval      *In28 = *On
228700131113     c                   Goto      endctr
228800131113     c                   EndIf
228900131113      *
229000131028     c                   EndIf
229100031119      *
229200131029     c                   EndIf
229300131029      *
229400031118      * visualizzo la decorrenza
229500031118      *
229600101013     c                   z-add     dia(v1crct)   g02INV
229700031118     c                   movel     '3'           g02err
229800031118     c                   CALL      'XSRDA8'
229900031118     c                   PARM                    WLBDAT
230000031118      * UDATE A 8 IN GG/MM/AAAA
230100031118     c                   z-add     g02dat        v1diST
230200031119      * se cambiato scaglione istat riemetto la videata per visualizzare
230300031119      * la data decorrenza
230400101013     C                   IF        V1Crct <> W_V1Crct
230500031119     C                   SETON                                        27
230600101013     c                   eval      W_v1crct = v1crct
230700031119     C                   ENDIF
230800031118     C*
230900031118   X1C                   ELSE
231000031118     C     V1CPRI        IFGT      0
231100031118     C                   MOVEL     MSG(7)        V1CMSG
231200031118     C                   SETON                                        4328
231300031118     C                   GOTO      ENDCTR
231400031118     C                   ENDIF
231500031118      *
231600031118     c                   clear                   v1dist
231700031118    1C                   ENDIF
231800031118
231900940503     C*
232000940504     C****  CODICE TARIFFA  ****
232100900621     C                   Z-ADD     1             A
232200940526     C                   MOVEL     V1CCTR        UNOCTR            1 0
232300021118     c                   Move      V1cCtr        DueCtr            2 0
232400940524     C     UNOCTR        LOOKUP    CTR(A)                                 35
232500940524    1C     *IN35         IFEQ      *OFF
232600940526     C                   MOVEL     MSG(9)        V1CMSG
232700000118     C                   SETON                                        28
232800940504     C                   GOTO      ENDCTR
232900940509    1C                   ENDIF
233000021118
233100940504     C* DESCRIZIONE CODICE TARIFFA
233200940526     C                   MOVEL     DTR(A)        V1DCTR
233300940504     C*
233400940504     C****  VOLUME AUTOMATICO  ****
233500940504     C* 50 ON  -  S O L O    P E R    O F F E R T E
233600940504     C* SE SI TRATTA DI CLIENTE NUOVO, VOLUME AUTOMATICO OBBLIGATORIO
233700940509    1C   50KSC2          IFLE      *ZEROS
233800940526     C     V1CKPM        ANDEQ     0
233900940526     C     V1CMPC        ANDEQ     0
234000940526     C                   MOVEL     MSG(10)       V1CMSG
234100940526     C                   SETON                                        4628
234200940504     C                   GOTO      ENDCTR
234300940509    1C                   ENDIF
234400940509     C*
234500940504     C* NON SI POSSONO INDICARE SIA I KG CHE I COLLI PER VOL.AUTOMATICO
234600940526    1C     V1CKPM        IFNE      0
234700940526     C     V1CMPC        ANDNE     0
234800940526     C                   MOVEL     MSG(11)       V1CMSG
234900940526     C                   SETON                                        4628
235000940504     C                   GOTO      ENDCTR
235100940509    1C                   ENDIF
235200090706      **** UTILIZZO PESO VDL ****
235300090706      * non posso immettere il campo utilizzo peso VDL se flag rapporto
235400090706      * peso/volume su tipo tariffa è blank
235500090706     c                   if        wrpv = *blanks and v1ctap <> *blanks
235600090706     c                   eval      v1cmsg = msg(14)
235700090706     c                   eval      *in28 = *on
235800090706     c                   eval      *in60 = *on
235900090706     c                   goto      endctr
236000090706     c                   endif
236100910423     C*
236200940504     C****  APPLICAZIONE TARIFFA FINITA  ****
236300941124    1C     V1CATA        IFNE      0
236400950112     C* WSAP = " " --> SI TRATTA DI SCAGLIONE SENZA DECIMALI PER CUI
236500950112     C*   NON SI POSSONO INSERIRE VALORI DOPO LA VIRGOLA
236600950112    2C     WSAP          IFEQ      ' '
236700941125     C                   MOVE      V1CATA        W0030
236800941124     C     W0030         IFGT      0
236900941124     C                   SETON                                        4728
237000950119     C                   MOVEL     MSG(57)       V1CMSG
237100941124     C                   GOTO      ENDCTR
237200941124     C                   ENDIF
237300950112    2C                   ENDIF
237400941124     C*
237500940504     C* CONTROLLO CHE L'APPLICAZIONE TARIFFA FINITA NON SIA INFERIORE
237600940504     C*   AL MINIMO TASSABILE INDICATO IN TABELLA
237700941124    2C     V1CATA        IFLT      MIN(A)
237800940526     C                   MOVEL     MSG(12)       V1CMSG
237900940526     C                   SETON                                        4728
238000940504     C                   GOTO      ENDCTR
238100941124    2C                   ENDIF
238200940509    1C                   ENDIF
238300940504     C*
238400940504     C****  ARROTONDAMENTI  ****
238500980505     C* CONTROLLO SE QUESTA TARIFFA GESTISCE GLI ARROTONDAMENTI
238600980505    1C     ARR(A)        IFEQ      ' '
238700980505    2C     V1CARL        IFGT      0
238800980505     C     V1CARF        ORGT      0
238900980505     C     V1CARO        ORGT      0
239000980505     C                   MOVEL     MSG(74)       V1CMSG
239100980505     C                   SETON                                        4828
239200980505     C                   GOTO      ENDCTR
239300980505    2C                   ENDIF
239400980505    1C                   ENDIF
239500940504     C* DEVONO ESSERE O TUTTI <> 0  OPPURE  TUTTI = 0
239600940712    1C     V1CARL        IFEQ      0
239700940504     C*
239800940712     C     V1CARF        IFGT      0
239900940526     C     V1CARO        ORGT      0
240000940526     C                   MOVEL     MSG(13)       V1CMSG
240100940526     C                   SETON                                        4828
240200940504     C                   GOTO      ENDCTR
240300940504     C                   ENDIF
240400940504     C*
240500940504   X1C                   ELSE
240600940504     C*
240700940712     C     V1CARF        IFEQ      0
240800940526     C     V1CARO        OREQ      0
240900940526     C                   MOVEL     MSG(13)       V1CMSG
241000940526     C                   SETON                                        4828
241100940504     C                   GOTO      ENDCTR
241200940504     C                   ENDIF
241300940504     C*
241400940504    1C                   ENDIF
241500940504     C*
241600941125     C* SE SONO STATI IMMESSI GLI ARROTONDAMENTI CONTROLLO SE SI TRATTA
241700950112     C*   DI SCAGLIONE SENZA DECIMALI PER CUI NON SI POSSONO INSERIRE
241800950112     C*   VALORI DOPO LA VIRGOLA
241900941125    1C     V1CARL        IFGT      0
242000941125     C     WSAP          ANDEQ     ' '
242100941125     C                   MOVE      V1CARL        W0030
242200941125    2C     W0030         IFGT      0
242300941125     C                   SETON                                        4828
242400950119     C                   MOVEL     MSG(56)       V1CMSG
242500941125     C                   GOTO      ENDCTR
242600941125    2C                   ENDIF
242700941125     C*
242800941125     C                   MOVE      V1CARF        W0030
242900941125    2C     W0030         IFGT      0
243000941125     C                   SETON                                        4828
243100950119     C                   MOVEL     MSG(56)       V1CMSG
243200941125     C                   GOTO      ENDCTR
243300941125    2C                   ENDIF
243400941125     C*
243500941125     C                   MOVE      V1CARO        W0030
243600941125    2C     W0030         IFGT      0
243700941125     C                   SETON                                        4828
243800950119     C                   MOVEL     MSG(56)       V1CMSG
243900941125     C                   GOTO      ENDCTR
244000941125    2C                   ENDIF
244100941125    1C                   ENDIF
244200990616     C*
244300990616     C* CONTROLLO SE INSERITO MINIMO MASSIMO SPEDIZIONI AFFIDATECI DAL CLIENTE
244400990616     C     V1CMIS        IFNE      *ZEROS
244500990616     C     V1CMAS        ANDGT     0
244600990616     C     V1CMIS        ANDGT     V1CMAS
244700990618     C                   SETON                                        5128
244800990617     C                   MOVEL     MSG(78)       V1CMSG
244900990616     C                   GOTO      ENDCTR
245000990616     C                   ENDIF
245100940503     C*
245200940707     C****  TIPO SPEDIZIONE  ****
245300950112    1C     V1CTSP        IFEQ      '?'
245400940707     C                   MOVEL     '5E'          COD
245500940707     C                   MOVEL     ' '           V1CTSP
245600020201     C                   MOVEL     *BLANKS       KEY
245700960527     C                   MOVEL     *BLANKS       DES              25
245800940707     C                   CALL      'X§TABER'
245900940707     C                   PARM                    CODUT
246000940707     C                   PARM                    COD
246100940707     C                   PARM                    KEY
246200940707     C                   PARM                    DES
246300940707     C                   MOVEL     KEY           V1CTSP
246400090608     C**!!!              MOVEL     DES           V1DTSP
246500940729     C                   SETON                                        27
246600090608     C**!!!              GOTO      ENDCTR
246700950112    1C                   ENDIF
246800020215     C*
246900940711     C* TIPO SPEDIZIONE OBBLIGATORIO
247000940707     C                   MOVEL     '5E'          COD
247100940707     C                   MOVEL     *BLANKS       KEY
247200940707     C                   MOVEL     V1CTSP        KEY
247300940707     C     KTAB          CHAIN     TABEL                              32
247400950112    1C  N32TBLFLG        IFNE      ' '
247500940707     C                   SETON                                        32
247600950112    1C                   ENDIF
247700940707     C* TIPO SPEDIZIONE ERRATO
247800950112    1C     *IN32         IFEQ      *ON
247900940707     C                   MOVEL     MSG(42)       V1CMSG
248000940707     C                   SETON                                        5428
248100940707     C                   GOTO      ENDCTR
248200950112    1C                   ENDIF
248300000420     C*
248400000420     C                   MOVEL     TBLUNI        DS5E
248500090915     C* CONTROLLO SE tipo servizio utilizzabile
248600090915     C***  *IN50         IFEQ      *OFF
248700090915     c*
248800090915     c                   if        §5euso='P'   or §5efum<>'S'
248900090915    1C***  §5EUSO        IFEQ      'P'
249000090915     C***  WRKPT         ANDNE     'S'
249100090915     C                   MOVEL     MSG(91)       V1CMSG
249200090915     C                   SETON                                        5428
249300090915     C                   GOTO      ENDCTR
249400090915    1C                   ENDIF
249500000420     C*
249600000420     C* VERIFICO SE SERVIZIO NON POSTA CON CLIENTE NON POSTA
249700090915    1C***  §5EUSO        IFEQ      'E'
249800090915     C***  WRKPT         ANDEQ     'S'
249900090915     C**+                MOVEL     MSG(92)       V1CMSG
250000090915     C***                SETON                                        5428
250100090915     C***                GOTO      ENDCTR
250200090915    1C***                ENDIF
250300090915     C***                ENDIF
250400940707     C*
250500940707     C* DECODIFICA TIPO SPEDIZIONE
250600090608     C                   MOVEL     §5ed08        V1DTSP
250700970401     C* NON POSSO INSERIRE TIPO SERVIZIO ESPRESSO SE TARIFFA ESTERA
250800970401     C     V1CTSP        IFEQ      'E'
250900970401     C     V1CFIE        ANDEQ     'E'
251000970401     C                   MOVEL     MSG(70)       V1CMSG
251100090608     c                   eval      %subst(v1cmsg:34:8) = §5ed08
251200970401     C                   SETON                                        5428
251300970401     C                   GOTO      ENDCTR
251400970401    1C                   ENDIF
251500001127     C* SE E' OFFERTA ED IL TIPO SERVIZIO E' UN TIPO SERVIZIO UTILIZZATO DA
251600001127     C* CLIENTI POSTA VALORIZZO IL WRKPT = A 'S'
251700000426     C     *IN50         IFEQ      *ON
251800000426     C     §5EUSO        ANDEQ     'P'
251900000426     C                   MOVEL     'S'           WRKPT
252000000426     C                   ENDIF
252100130514      /free
252200130514       //?Se manutenzione tariffa posso variare il tipo servizio solo da
252300130514       //?"C" a "D" o viceversa, se "E" non posso variare e non posso impostarla
252400130514         IF  not *in50 and *in10 and not *in23;
252500130604           IF  sav_TAMtsp = 'E' and V1Ctsp <> sav_TAMtsp;
252600130514             V1Cmsg = msg(90);
252700130514             *in28 = *on;
252800130514             *in54 = *on;
252900130514             leavesr;
253000130514           ENDIF;
253100130604           IF  (sav_TAMtsp = 'C' or sav_TAMtsp = 'D') and
253200130514                V1Ctsp <> 'C' and V1Ctsp <> 'D';
253300130514             V1Cmsg = msg(90);
253400130514             *in28 = *on;
253500130514             *in54 = *on;
253600130514             leavesr;
253700130514           ENDIF;
253800130514         ENDIF;
253900130514      /end-free
254000940706     C*
254100941010     C****  FLAG TARIFFA PREFERENZIALE  ****
254200940929     C* NON E' POSSIBILE SELEZIONARE UNA TARIFFA PREFERENZIALE SU
254300940929     C*   CODICI TARIFFA DIVERSI APPARTENENTI ALLO STESSO CLIENTE
254400941011     C*   A MENO CHE NON SI TRATTI DI TARIFFA PARTENZE E TARIFFA ARRIVI
254500941011     C*   O DI TARIFFA ITALIA O TARIFFA ESTERO
254600941109    1C     V1CFTP        IFEQ      'P'
254700061011     C  n50V1CKSC        SETLL     TNTAM000
254800061011     c   50v1cksc        setll     tnofm000
254900061011     C  n50V1CKSC        READE(N)  TNTAM000                               32
255000061011     c   50v1cksc        reade(n)  tnofm000                               32
255100940929     C*
255200941109    2C     *IN32         DOWEQ     *OFF
255300941109    3C     TAMATB        IFEQ      ' '
255400940929     C*
255500941109    4C     TAMCTR        IFNE      V1CCTR
255600940929     C     TAMFTP        ANDEQ     'P'
255700941011     C*
255800941109    5C     TAMFIE        IFEQ      V1CFIE
255900941011     C*
256000941109    6C     TAMBAP        IFEQ      'P'
256100941011     C     V1CBAP        ANDEQ     'P'
256200950105     C     TAMBAP        OREQ      'P'
256300950105     C     V1CBAP        ANDEQ     ' '
256400950105     C     TAMBAP        OREQ      ' '
256500950105     C     V1CBAP        ANDEQ     'P'
256600950105     C*
256700941011     C     TAMBAP        OREQ      'A'
256800941011     C     V1CBAP        ANDEQ     'A'
256900950105     C     TAMBAP        OREQ      'A'
257000950105     C     V1CBAP        ANDEQ     ' '
257100950105     C     TAMBAP        OREQ      ' '
257200950105     C     V1CBAP        ANDEQ     'A'
257300940929     C                   MOVEL     MSG(49)       V1CMSG
257400940929     C                   SETON                                        8628
257500940929     C                   GOTO      ENDCTR
257600941109    6C                   ENDIF
257700941109    5C                   ENDIF
257800941109    4C                   ENDIF
257900940929     C*
258000941109    3C                   ENDIF
258100061011     C  n50V1CKSC        READE(N)  TNTAM000                               32
258200061011     c   50v1cksc        reade(n)  tnofm000                               32
258300941109    2C                   ENDDO
258400941109    1C                   ENDIF
258500020205      *
258600020205      * Se non immesso Network non posso inserirlo
258700020205     C  N01V1CNET        IFNE      ' '
258800020205     C     §TADPD        ANDEQ     ' '
258900020205     C     §TAFED        ANDEQ     ' '
259000020205     C                   MOVEL     MSG(99)       V1CMSG
259100020205     C                   SETON                                        4528
259200020205     C                   GOTO      ENDCTR
259300020205    6C                   ENDIF
259400000118     C*
259500020130     C* SE IMMESSO SERVIZIO DPD NON POSSO PIU' TOGLIERLO
259600020201     C  N01V1CNET        IFEQ      ' '
259700000118     C     §TADPD        ANDEQ     'S'
259800000118     C                   MOVEL     MSG(86)       V1CMSG
259900000118     C                   SETON                                        4528
260000000118     C                   GOTO      ENDCTR
260100000118    6C                   ENDIF
260200020130      *
260300020205      * Se immesso Network FedEx non posso più toglierlo
260400020201     C  N01V1CNET        IFEQ      ' '
260500020130     C     §TAFED        ANDEQ     'S'
260600020130     C                   MOVEL     MSG(97)       V1CMSG
260700020130     C                   SETON                                        4528
260800020130     C                   GOTO      ENDCTR
260900020130     C                   ENDIF
261000000118     C* una tariffa dpd non puo' essere estera
261100020201     C     V1CNET        IFEQ      'D'
261200000118     C     V1CFIE        ANDEQ     'E'
261300000118     C                   MOVEL     MSG(87)       V1CMSG
261400000118     C                   SETON                                        4528
261500000118     C                   GOTO      ENDCTR
261600000118    6C                   ENDIF
261700020130      * una tariffa FedEx deve essere estera
261800020201     C     V1CNET        IFEQ      'F'
261900020130     C     V1CFIE        ANDNE     'E'
262000020130     C                   MOVEL     MSG(98)       V1CMSG
262100020130     C                   SETON                                        4528
262200020130     C                   GOTO      ENDCTR
262300020130    6C                   ENDIF
262400020130     C* una tariffa dpd o FedEx deve essere corriere
262500020201     C     V1CNET        IFNE      ' '
262600000121     C     V1CTSP        ANDNE     'C'
262700090608     c                   clear                   ds5e
262800090608     c                   eval      cod = '5E'
262900090608     c                   eval      key = 'C'
263000090608     c     ktab          chain     tabel00f
263100090608     c                   if        %found(tabel00f)
263200090608     c                   eval      ds5e = tbluni
263300090608     c                   endif
263400000121     C                   MOVEL     MSG(88)       V1CMSG
263500090608     c                   eval      %subst(v1cmsg:48:8) = §5ed08
263600000121     C                   SETON                                        4528
263700000121     C                   GOTO      ENDCTR
263800000121    6C                   ENDIF
263900000218     C* una tariffa DPD a kg. deve avere applicazione tariffa finita fino a 31,5
264000000223     C* CHE TROVO NELLA TABELLA 3I
264100020201     C     V1CNET        IFEQ      'D'
264200000218     C     UNOCTR        ANDEQ     3
264300000223     C     V1CATA        ANDNE     §3IPKD
264400000223     C                   Z-ADD     §3IPKD        V1CATA
264500160219     C*****              GOTO      ENDCTR
264600000218    6C                   ENDIF
264700021118      * Controllo per tariffa FedEx se ok codice tariffa inserito
264800021118     c                   If        V1cnet = 'F' and Rutf(a) <> 'S'
264900021118     c                   Eval      V1cMsg = Msg(111)
265000021118     c                   Eval      *In45 = *On
265100021118     c                   Eval      *In28 = *On
265200021118     c                   GoTo      EndCtr
265300021118     c                   EndIf
265400160218
265500160218      /free
265600160218       //?Se inserimento Offerta di trattativa su cliente
265700160218       //?devo controllare la tipologia dell'offerta con eventuale
265800160218       //?altra offerta già esistente.
265900160218         IF  not *in10 and *in50 and ksc2 > *zeros;
266000160218           clear TNTAT1DS;
266100160218           ITAT1ntc  = V1Cksc;
266200160218           ITAT1ksc  = %dec(ksc2:7:0);
266300160218           ITAT1ctr  = vctr;
266400160218           SELECT;
266500160218           WHEN  V1Cnet = 'D';
266600160218             ITAT1ntw = 'DPD';
266700160218           WHEN  V1Cnet = 'F';
266800160218             ITAT1ntw = 'FED';
266900160218           WHEN  V1Cfie = 'E';
267000160218             ITAT1ntw = 'EEX';
267100160218           OTHER;
267200160219             clear ITAT1ntw;
267300160218           ENDSL;
267400160218           kpjbu = TNTAT1DS;
267500160218           tntat1r (kpjba);
267600160218           TNTAT1DS = kpjbu;
267700160218           IF  OTAT1err = *on;
267800160218             *in28  = *on;
267900160218             *in87  = *on;
268000160218             V1Cmsg = OTAT1msg;
268100160218             leavesr;
268200160218           endif;
268300160218         ENDIF;
268400160218      /end-free
268500940929     C*
268600940503     C     ENDCTR        ENDSR
268700940503     C*
268800941013     C*--- CONTROLLO DATI FORMATO 2 ----------------------------------*
268900941012     C     CTRD02        BEGSR
269000020312     C                   SETOFF                                       2782
269100940729     C*
269200940811     C****  NATURA MERCE  ****
269300940811     C* NATURA MERCE OBBLIGATORIA
269400940811     C     V2CNAS        IFEQ      *BLANKS
269500940811     C                   MOVEL     MSG(43)       V1CMSG
269600940811     C                   SETON                                        8228
269700940811     C                   GOTO      ENDCT
269800940811     C                   ENDIF
269900060303
270000060303      * Rinuncia AC BASE
270100060303      * se imposto che rinuncio non ci deve essere la tariffa particolare "d"
270200060303     c                   if        v2cfmp <> *blanks
270300060303     c                   eval      wtpar = 'd'
270400070129     c  n50ktam7         chain(n)  titpt01l
270500070129     c   50ktam7         chain(n)  tiopt01l
270600061011     c                   If        %found and tptatb = *blanks
270700060303     c                   eval      v1cmsg = msg(119)
270800060303     c                   eval      *in28 = *on
270900060303     c                   move      rimm          at2fmp
271000060303     c                   Eval      h2riga = 10
271100060303     c                   Eval      h2colo = 19
271200060303     c                   leavesr
271300060303     c                   endif
271400060303     c                   endif
271500021118
271600940811     C*
271700940811     C****  FLAG VALORE MERCE DICHIARATO  ****
271800940811     C     *IN16         IFEQ      *ON
271900940811     C     V2CFVD        ANDEQ     ' '
272000940811     C                   MOVEL     MSG(44)       V1CMSG
272100090916     C                   SETON                                          28
272200090916     c                   Eval      h2riga = 13
272300090916     c                   Eval      h2colo = 36
272400940811     C                   GOTO      ENDCT
272500940811     C                   ENDIF
272600940811     C*
272700940729     C****  PARTICOLARITA' GESTIONE C/ASSEGNO  ****
272800941010    1C     V2CGCA        IFNE      *BLANKS
272900941010     C     V2CGCR        ORNE      *BLANKS
273000941010     C*
273100941010     C* RICHIAMO IL PROGRAMMA DI RICERCA
273200941010    2C     V2CGCR        IFEQ      '?'
273300941010     C                   CLEAR                   DS7PQR
273400941010     C                   MOVEL     'S'           DS7RIC
273500941010     C                   MOVEL     'V'           DS7GES
273600050428     C  n50              MOVEL     V1CKSC        DS7KSC
273700050428     C   50              MOVEL     ksc2          DS7KSC
273800941010     C                   MOVEL     V2CGCA        DS7COD
273900941010     C                   MOVEL     DS7PQR        KPJBU
274000941010     C                   CALL      'TRTB72R'
274100941010     C                   PARM                    KPJBA
274200941010     C                   MOVEL     KPJBU         DS7PQR
274300941010     C*
274400941010     C     V2CGCA        IFEQ      *BLANKS
274500941010     C                   MOVEL     DS7COD        V2CGCA
274600941010     C                   ENDIF
274700941010     C*
274800960109     C                   SETON                                        2777
274900941010     C                   GOTO      ENDCT
275000941010    2C                   ENDIF
275100941010     C*
275200941010     C* CONTROLLO
275300941010     C                   CLEAR                   DS7PQR
275400941010     C                   MOVEL     'S'           DS7RIC
275500941010     C                   MOVEL     'C'           DS7GES
275600050323     C  n50              MOVEL     V1CKSC        DS7KSC
275700050323     C   50              MOVEL     ksc2          DS7KSC
275800941010     C                   MOVEL     V2CGCA        DS7COD
275900941010     C                   MOVEL     DS7PQR        KPJBU
276000941010     C                   CALL      'TRTB72R'
276100941010     C                   PARM                    KPJBA
276200941010     C                   MOVEL     KPJBU         DS7PQR
276300941010     C* ERRORE
276400941010    2C     DS7ERR        IFEQ      '1'
276500941010     C                   MOVEL     DS7MSG        V1CMSG
276600941010     C                   SETON                                        7728
276700941010     C                   GOTO      ENDCT
276800941010    2C                   ENDIF
276900941010    1C                   ENDIF
277000941010     C*
277100940729     C****  PARTICOLARITA' GESTIONE GIACENZA  ****
277200941010    1C     V2CGGA        IFNE      *BLANKS
277300941010     C     V2CGGR        ORNE      *BLANKS
277400941010     C*
277500941010     C* RICHIAMO IL PROGRAMMA DI RICERCA
277600941010    2C     V2CGGR        IFEQ      '?'
277700941010     C                   CLEAR                   DS7PQR
277800941010     C                   MOVEL     'S'           DS7RIC
277900941010     C                   MOVEL     'V'           DS7GES
278000050428     ***C  n50              MOVEL     V1CKSC        DS7KSC
278100050428     ***C   50              MOVEL     ksc2          DS7KSC
278200941010     C                   MOVEL     V2CGGA        DS7COD
278300941010     C                   MOVEL     DS7PQR        KPJBU
278400941010     C                   CALL      'TRTB69R'
278500941010     C                   PARM                    KPJBA
278600941010     C                   MOVEL     KPJBU         DS7PQR
278700941010     C*
278800941010     C     V2CGGA        IFEQ      *BLANKS
278900941010     C                   MOVEL     DS7COD        V2CGGA
279000941010     C                   ENDIF
279100941010     C*
279200960109     C                   SETON                                        2778
279300941010     C                   GOTO      ENDCT
279400941010    2C                   ENDIF
279500941010     C*
279600941010     C* CONTROLLO
279700941010     C                   CLEAR                   DS7PQR
279800941010     C                   MOVEL     'S'           DS7RIC
279900941010     C                   MOVEL     'C'           DS7GES
280000050323     C  n50              MOVEL     V1CKSC        DS7KSC
280100050323     C   50              MOVEL     ksc2          DS7KSC
280200941010     C                   MOVEL     V2CGGA        DS7COD
280300960109     C                   MOVEL     DS7PQR        KPJBU
280400941010     C                   CALL      'TRTB69R'
280500941010     C                   PARM                    KPJBA
280600941010     C                   MOVEL     KPJBU         DS7PQR
280700941010     C* ERRORE
280800941010    2C     DS7ERR        IFEQ      '1'
280900941010     C                   MOVEL     DS7MSG        V1CMSG
281000941010     C                   SETON                                        7828
281100941010     C                   GOTO      ENDCT
281200941010    2C                   ENDIF
281300941010    1C                   ENDIF
281400941010     C*
281500941010     C****  PARTICOLARITA' GESTIONE CONSEGNA  ****
281600941010    1C     V2CGMA        IFNE      *BLANKS
281700941010     C     V2CGMR        ORNE      *BLANKS
281800941010     C*
281900941010     C* RICHIAMO IL PROGRAMMA DI RICERCA
282000941010    2C     V2CGMR        IFEQ      '?'
282100941010     C                   MOVEL     'S'           DS7RIC
282200941010     C                   MOVEL     'V'           DS7GES
282300050428     ***C  n50              MOVEL     V1CKSC        DS7KSC
282400050428     ***C   50              MOVEL     ksc2          DS7KSC
282500050606     c                   Clear                   ds7ksc
282600941012     C                   MOVEL     V2CGMA        DS7COD
282700960109     C                   MOVEL     DS7PQR        KPJBU
282800941010     C                   CALL      'TRTB70R'
282900941010     C                   PARM                    KPJBA
283000941010     C                   MOVEL     KPJBU         DS7PQR
283100941010     C*
283200941010     C     V2CGMA        IFEQ      *BLANKS
283300941010     C                   MOVEL     DS7COD        V2CGMA
283400941010     C                   ENDIF
283500941010     C*
283600960109     C                   SETON                                        2779
283700941010     C                   GOTO      ENDCT
283800941010    2C                   ENDIF
283900941010     C*
284000941010     C* CONTROLLO
284100941010     C                   CLEAR                   DS7PQR
284200941010     C                   MOVEL     'S'           DS7RIC
284300941010     C                   MOVEL     'C'           DS7GES
284400050323     C  n50              MOVEL     V1CKSC        DS7KSC
284500050323     C   50              MOVEL     ksc2          DS7KSC
284600941010     C                   MOVEL     V2CGMA        DS7COD
284700941010     C                   MOVEL     DS7PQR        KPJBU
284800941010     C                   CALL      'TRTB70R'
284900941010     C                   PARM                    KPJBA
285000941010     C                   MOVEL     KPJBU         DS7PQR
285100941010     C* ERRORE
285200941010    2C     DS7ERR        IFEQ      '1'
285300941010     C                   MOVEL     DS7MSG        V1CMSG
285400941010     C                   SETON                                        7928
285500941010     C                   GOTO      ENDCT
285600941010    2C                   ENDIF
285700941010    1C                   ENDIF
285800941010     C*
285900941010     C****  PARTICOLARITA' GESTIONI VARIE  ****
286000941010    1C     V2CGVA        IFNE      *BLANKS
286100941010     C     V2CGVR        ORNE      *BLANKS
286200941010     C*
286300941010     C* RICHIAMO IL PROGRAMMA DI RICERCA
286400941010    2C     V2CGVR        IFEQ      '?'
286500941010     C                   CLEAR                   DS7PQR
286600941010     C                   MOVEL     'S'           DS7RIC
286700941010     C                   MOVEL     'V'           DS7GES
286800050323     C  n50              MOVEL     V1CKSC        DS7KSC
286900050323     C   50              MOVEL     ksc2          DS7KSC
287000941010     C                   MOVEL     V2CGVA        DS7COD
287100941010     C                   MOVEL     DS7PQR        KPJBU
287200941010     C                   CALL      'TRTB73R'
287300941010     C                   PARM                    KPJBA
287400941010     C                   MOVEL     KPJBU         DS7PQR
287500941010     C*
287600941010     C     V2CGVA        IFEQ      *BLANKS
287700941010     C                   MOVEL     DS7COD        V2CGVA
287800941010     C                   ENDIF
287900941012     C*
288000960109     C                   SETON                                        2780
288100941012     C                   GOTO      ENDCT
288200941012    2C                   ENDIF
288300941010     C*
288400941010     C* CONTROLLO
288500941010     C                   CLEAR                   DS7PQR
288600941010     C                   MOVEL     'S'           DS7RIC
288700941010     C                   MOVEL     'C'           DS7GES
288800050323     C  n50              MOVEL     V1CKSC        DS7KSC
288900050323     C   50              MOVEL     ksc2          DS7KSC
289000941010     C                   MOVEL     V2CGVA        DS7COD
289100941010     C                   MOVEL     DS7PQR        KPJBU
289200960209     C                   CALL      'TRTB73R'
289300960209     C                   PARM                    KPJBA
289400941010     C                   MOVEL     KPJBU         DS7PQR
289500941010     C* ERRORE
289600941010     C     DS7ERR        IFEQ      '1'
289700941010     C                   MOVEL     DS7MSG        V1CMSG
289800941010     C                   SETON                                        8028
289900941010     C                   GOTO      ENDCT
290000941010     C                   ENDIF
290100941012    1C                   ENDIF
290200000118     C**
290300000118     C* FLAG DDT: PER TARIFFA DPD SEMPRE NO
290400020201     C     V1CNET        IFEQ      'D'
290500000118     C     V2CBAM        ANDEQ     'S'
290600000118     C                   MOVEL     MSG(85)       V1CMSG
290700000118     C                   SETON                                        4928
290800000118     C                   GOTO      ENDCT
290900000118     C                   ENDIF
291000940729     C*
291100940729     C     ENDCT         ENDSR
291200940729     C*
291300940509     C*--- PULIZIA SUBFILE AGGIORNAMENTO -----------------------------*
291400920525     C     PULSF         BEGSR
291500000000     C                   SETON                                        94
291600940706     C                   WRITE     TA01C03
291700000000     C                   SETOFF                                       94
291800900924     C                   Z-ADD     0             SAVREC
291900900924     C                   ENDSR
292000930629     C*
292100941013     C*--- PULIZIA E INIZIAL. SF DI IMMISSIONE -----------------------*
292200920525     C     PULSF2        BEGSR
292300000000     C                   SETON                                        84
292400940706     C                   WRITE     TA01C05
292500000000     C                   SETOFF                                       84
292600920525     C*
292700940517     C                   CLEAR                   TADORP
292800940517     C                   CLEAR                   TADATB
292900940517     C                   CLEAR                   TADLNP
293000940517     C                   CLEAR                   TADCTS
293100940517     C                   CLEAR                   DESCTS
293200020624     c                   clear                   tadcap
293300940517     C                   CLEAR                   TADSGL
293400940517     C                   CLEAR                   TADITR
293500940517     C                   CLEAR                   TADINO
293600940517     C                   CLEAR                   TADRPV
293700960521     C                   CLEAR                   SAVRPV
293800940517     C                   CLEAR                   TADMIN
293900940517     C                   CLEAR                   TADMAX
294000020624     c                   clear                   tadnaz
294100990623     C                   CLEAR                   DESNAZ
294200940518     C* CAMPI DI DUPLICA
294300940518     C                   CLEAR                   DUPLNP
294400940518     C                   CLEAR                   DUPCTS
294500940518     C                   CLEAR                   DUPCAP
294600940518     C                   CLEAR                   DUPSGL
294700940518     C                   CLEAR                   DUPITR
294800940518     C                   CLEAR                   DUPINO
294900940518     C                   CLEAR                   DUPRPV
295000940518     C                   CLEAR                   DUPMIN
295100940518     C                   CLEAR                   DUPMAX
295200990608     C                   CLEAR                   DUPNAZ
295300021118
295400021118     c                   Clear                   ForzaMsg
295500021118
295600000000     C                   DO        120           NR1
295700940706     C                   WRITE     TA01S05
295800000000     C                   END
295900930225     C                   Z-ADD     1             REC1
296000000000     C                   ENDSR
296100050530     c
296200050530     c*
296300050530     c* Controllo Tariffe particolari da eliminare ------------------------*
296400050530     c     CtrTParEli    BEGSR
296500050530     c                   z-add     1             xx
296600050530    2c                   dow       TParEli(XX)<>'  '
296700070129     c  n50ktpt          chain(n)  titpt01l
296800070129     c   50ktpt          chain(n)  tiopt01l
296900061011    3c                   if        %found and tptatb=' '
297000050530     c* verifico la data di scadenza della tariffa che deve essere
297100050530     c*  <= alla data di eliminazione della tariffa particolare
297200050530    4c                   if        comdst>TPardat(XX)
297300050530     c* Errore  forzabile per F21 se non già dato
297400050530    5c                   if        *inkv
297500130318     c**   tpareli(XX)   lookup    forzatpar                              31
297600130318     c                   IF        %lookup(tpareli(xx):forzatpar) > 0
297700130318     c                   eval      wTParEli = *on
297800130318     c                   ENDIF
297900050530     c                   else
298000050530     c* Errore non forzabile per gli altri casi
298100130318     c**                 setoff                                       31
298200130318     c                   eval      wTParEli = *off
298300050530    5c                   endif
298400050530     c
298500130318    5c**                 if        not *in31
298600130318     c                   IF        not wTParEli
298700050530     c* Errore
298800050530     c                   eval      v1cmsg=msg(115)
298900050530     c
299000050530     c                   eval      %subst(v1cmsg:19:2)=tpareli(XX)
299100050530     c     *iso          move      tpardat(XX)   wdatadmy
299200050530     c                   move      wdatadmy      wdata             6 0
299300050530     c                   eval      %subst(v1cmsg:41:8) =(%editw(wdata:'  /  / -
299400050530     c                              '))
299500050530     c
299600050530     c                   add       1             yy                4 0
299700050530     c                   movel     tpareli(xx)   forzatpar(YY)
299800050530     c                   seton                                        2841
299900050530     C                   GOTO      ENDCtrTPar
300000050530    5c                   endif
300100050530    4c                   endif
300200050530    3c                   endif
300300050530     c                   add       1             xx
300400050530    2c                   enddo
300500050530     c     EndCTRTPar    ENDSR
300600930629     C*
300700940615     C*-- AGGIORNAMENTO TNTAM ----------------------------------------*
300800920525     C     AGGTAM        BEGSR
300900940506     C*
301000061011     C** RIPOSIZIONAMENTO SUL FILE TNTAM
301100061011     C  n50KTAM2         CHAIN     TNTAM000                           01
301200061011     c   50ktam2         chain     tnofm000                           01
301300061010     C  N01TAMATB        COMP      'A'                                    01
301400050530     c* se non sono in immissione, controllo tariffe partic in scadenza
301500061010    1c                   if        not *in01
301600050530     c
301700050530     c* controllo se ci sono tariffe particolari da eliminare
301800050530     c                   EXSR      CtrTParEli
301900050530     c
302000050530     c* 28 on trovate --> errore
302100050530     c                   if        *in28
302200050530     c                   goto      ENDaggtam
302300050530    1c                   endif
302400050530    1c                   endif
302500050530     c
302600061010     C  N01              SETON                                        10
302700941012     C** MUOVO I CAMPI DEL VIDEO NEL FILE
302800941012     C                   EXSR      RIETAM
302900920525     C* I M M I S S I O N E
303000970226     C     *IN01         IFEQ      *ON
303100970226     C                   Z-ADD     V1CKSC        TAMKSC
303200970226     C                   Z-ADD     PRG           TAMPRG
303300970226     C                   Z-ADD     DATEU8        TAMDUV
303400970226     C                   MOVEL     *BLANKS       TAMFTR
303500040913     C                   Z-ADD     dateu8        TAMDTR
303600970226     C                   MOVEL     *BLANKS       TAMATB
303700970226     C                   CLEAR                   TAMDAT
303800150922     C***                CLEAR                   TAMTPR
303900101025     c                   clear                   tamfli
304000920525     C*
304100061011     C  n50              WRITE     TNTAM000
304200061011     c   50              write     tnofm000
304300040927      *
304400040927     c                   eval      tavtrd = 'TES'
304500040927     c                   eval      tavcta = 'I'
304600040927     c                   exsr      SUB_STORICO
304700090916      * se trattativa scrivo TIVOF
304800090916     c                   if        *in85
304900090916     c                   clear                   tivof000
305000090916     c                   eval      vofnrv = tamksc
305100090916     c                   move      tamctr        vofctr
305200090916     c                   eval      vofprg = %dec(tamprg:1:0)
305300090916     c                   select
305400090916     c                   when      §tadpd = 'S'
305500090916     c                   eval      voftpt = 'D'
305600090916     c                   when      §tafed = 'S'
305700090916     c                   eval      voftpt = 'F'
305800090916     c                   other
305900090916     c                   eval      voftpt = tamfie
306000090916     c                   endsl
306100091125      * valorizzo data presentazione offerta con la data del giorno
306200091125     c                   eval      vofdpo = dateu8
306300091125
306400090916     c                   write     tivof000
306500090916     c                   endif
306600920525     C*
306700940615     C** INDICATORE DI VARIAZIONE TNTAM
306800970226     C                   SETON                                        81
306900970226     C                   ENDIF
307000920525     C*
307100920525     C* V A R I A Z I O N E
307200000000     C   10COMODO        IFNE      TAMDS
307300941221     C                   Z-ADD     DATEU8        TAMDUV
307400040913     C                   Z-ADD     dateu8        TAMDTR
307500940511     C                   MOVEL     *BLANKS       TAMFTR
307600061011     C  n50              UPDATE    TNTAM000
307700061011     c   50              update    tnofm000
307800000000     C                   SETON                                        81
307900030704
308000030716     c                   eval      mantes = 'S'
308100960620     C                   MOVEL     TAMDS         COMODO
308200000000     C                   END
308300900709     C*
308400900320     C                   Z-ADD     TAMCTR        VCTR
308500941012     C*
308600050530     C     endaggtam     ENDSR
308700940509     C*
308800940509     C*--- SR DI IMMISSIONE TARIFFA ----------------------------------*
308900920525     C     IMMISS        BEGSR
309000921218     C                   SETON                                        99
309100000000     C     SUIMMX        TAG
309200921118     C** PULIZIA SF
309300000000     C                   EXSR      PULSF2
309400120315
309500120315       //?Imposto i tasti funzione nella riga mobile
309600120315     c                   eval      wPrima = *off
309700120315     c                   eval      wSeconda = *off
309800120315     c                   eval      wTerza = *off
309900120315     c                   eval      wQuarta = *on
310000120315     c                   ExSr      Tastifun
310100921118     C*
310200000000     C     SUIMM         TAG
310300120319     C** EMISSIONE COMANDI
310400120319     C                   WRITE     TA01Z06
310500941206     C***
310600941206     C   28
310700941206     CAN 62              DO
310800941206     C                   SETOFF                                       28
310900941206     C                   WRITE     TA01C05
311000941206     C                   SETON                                        28
311100941206     C                   ENDDO
311200941206     C***
311300921118     C** EMISSIONE SF IMMISSIONE
311400940706     C                   EXFMT     TA01C05
311500921118     C*
311600940526     C* PULIZIA CAMPO MESSAGGIO E RELATIVO INDICATORE (*IN28)
311700940526     C                   CLEAR                   $MSG
311800940526     C* AZZERO GLI INDICATORI RELATIVI AI MESSAGGI DI ERRORE
311900940526     C                   SETOFF                                       285556
312000940526     C                   SETOFF                                       575859
312100960522     C                   SETOFF                                       616289
312200990623     C                   SETOFF                                       5238
312300120315       //?F1 - dati manca tariffa
312400120315     c                   IF        *inka
312500120315     c                   exsr      gesw01
312600120315     c                   goto      suimm
312700120315     c                   ENDIF
312800940526     C*
312900120315     c  nkc
313000120315     CanNKL              DO
313100921118     C** ESEGUE CONTROLLI IMMISSIONE
313200000000     C                   EXSR      CTRIMM
313300940525     C   22              GOTO      SUIMM
313400921118     C*
313500000000     C** AGGIUNTA TARIFFE SU DISCO
313600920525     C                   EXSR      ADDTAD
313700000000     C** CMD7-NUOVA IMMISSIONE
313800920525     C   KG              GOTO      SUIMMX
313900921118     C*
314000921118     C**CMD6-IMMISSIONE E FINE LAVORO
314100921118     C  NKF              DO
314200940526     C                   Z-ADD     0             V1CRDA
314300940526     C                   MOVEL     *BLANKS       V1CRA
314400921118     C                   END
314500120315       //?F24 Altri tasti
314600120315     c                   IF        *inky
314700120315     c                   exsr      gesf24
314800120315     c                   goto      suimm
314900120315     c                   ENDIF
315000921118     C                   END
315100000000     C                   ENDSR
315200930629     C*
315300930629     C*--- CONTROLLO IMMISSIONE --------------------------------------*
315400920525     C     CTRIMM        BEGSR
315500990617     C                   SETOFF                                       221444
315600940526     C                   SETOFF                                       020304
315700940526     C                   SETOFF                                       050607
315800940526     C                   SETOFF                                       080911
315900940511     C                   Z-ADD     1             NR1               3 0
316000940510     C*
316100940706     C                   READC     TA01S05                                29
316200940524    1C     *IN29         DOWEQ     *OFF
316300020408
316400020408     c                   eval      woksfl = '0'
316500020624     c                   clear                   descts
316600020408
316700930629     C*
316800940510     C* TASTI DI DUPLICA
316900900420     C   02              Z-ADD     DUPLNP        TADLNP
317000900420     C   03              MOVE      DUPCTS        TADCTS
317100020624     c   04              movel     DUPCAP        tadcap
317200940524     C   05              MOVE      DUPSGL        TADSGL
317300940524     C   06              Z-ADD     DUPITR        TADITR
317400940524     C   07              Z-ADD     DUPINO        TADINO
317500940524     C   08              Z-ADD     DUPRPV        TADRPV
317600020624     c   44              movel     DUPNAZ        tadnaz
317700940524     C   09              Z-ADD     DUPMIN        TADMIN
317800940524     C   11              Z-ADD     DUPMAX        TADMAX
317900930629     C*
318000940510     C****  LINEA PARTENZA  ****
318100940510     C* LINEA PARTENZA OBBLIGATORIA
318200120906       //?Se non è stata inserita la LNP ma c'è il codice di tassazione
318300120906       //?errore di manca LNP
318400120906     c                   IF        TADlnp = *zeros and
318500120906     c                             TADcts <> *blanks
318600120906     c                   eval      *in55 = *on
318700120906     c                   eval      *in28 = *on
318800120906     c                   eval      $msg = msg(24)
318900120906     c                   goto      giuctr
319000120906     c                   ENDIF
319100120906
319200940510    2C     TADLNP        IFNE      0
319300930629     C*
319400940930     C*
319500940930     C* CONTROLLO VALIDITA' LINEA PARTENZA
319600940930     C     TADLNP        CHAIN     AZORG                              32
319700020312     c                   clear                   og143
319800940811     C*
319900940510    3C  N32ORGFVA        IFNE      ' '
320000940510     C                   SETON                                        32
320100940510    3C                   ENDIF
320200930219     C*
320300020527     c                   if        orgfag <> 'A' and orgfag <> 'F' and
320400020527     c                             orgfl2 <> 'S'
320500940510     C                   SETON                                        32
320600940510    3C                   ENDIF
320700930219     C*
320800940510     C* 32 ON  - LINEA PARTENZA INESISTENTE
320900940525     C     *IN32         IFEQ      *ON
321000940526     C                   SETON                                        5528
321100940525     C                   MOVEL     MSG(22)       $MSG
321200940525     C                   GOTO      GIUCTR
321300940525     C                   ENDIF
321400020312
321500020312     c                   eval      og143 = orgde3
321600020312     c                   eval      lnpntw = §ogntw
321700020312
321800971205     C* SE FLAG ITA/EST = 'I' ---> NON POSSO INSERIRE FILIALI ESTERE
321900971205    3C     V1CFIE        IFEQ      'I'
322000020201     C     V1CNET        ANDEQ     ' '
322100020312  4b c                   if        lnpntw = 'EEX' or lnpntw = 'EUP' or
322200020312     c                             lnpntw = 'FED'
322300971205     C                   SETON                                        5528
322400971205     C                   MOVEL     MSG(45)       $MSG
322500971205     C                   GOTO      GIUCTR
322600020312  4e c                   endif
322700971205    3C                   ENDIF
322800930629     C*
322900940510     C****  CODICE TASSAZIONE  ****
323000940510     C* CODICE TASSAZIONE OBBLIGATORIO
323100000000     C                   Z-ADD     1             A                 5 0
323200941011     C                   Z-ADD     1             K
323300120713     c                   IF        %scan('?':TADcts) > *zeros
323400120713     c                   exsr      Ric_CT
323500120713     c                   eval      TADcts = TB09cod
323600120713     c                   ENDIF
323700940524     C     TADCTS        LOOKUP    CCT(A)                                 35
323800940524    3C   35CCT(A)        IFEQ      '  '
323900940524     C                   SETOFF                                       35
324000940510    3C                   ENDIF
324100900111     C*
324200940525     C     *IN35         IFEQ      *OFF
324300940526     C                   SETON                                        5628
324400940525     C                   MOVEL     MSG(23)       $MSG
324500940525     C                   GOTO      GIUCTR
324600940525     C                   ENDIF
324700050303      * errore bloccante se codice tassazione non utilizzabile in gestione
324800050303      * tariffe clienti
324900050303     c                   If        uta(a) = 'N'
325000050303     c                   Eval      $msg = msg(36)
325100050303     c                   Eval      *In56 = *On
325200050303     c                   Eval      *In28 = *On
325300050303     c                   Goto      giuctr
325400050303     c                   EndIf
325500020206      *
325600020206      * Se tariffa FedEx devo utilizzare solo i codici tariffa
325700020206      * previsti per FedEx
325800020206     C     V1CNET        IFEQ      'F'
325900020206     C     UTF(A)        ANDNE     'S'
326000020206     C                   SETON                                        5628
326100020206     C                   MOVEL     MSG(100)      $MSG
326200020206     C                   GOTO      GIUCTR
326300020206     C                   ENDIF
326400020206      * Se tariffa non è FedEx non posso utilizzare i codici
326500020206      * tariffa previsti per FedEx
326600020206     C     V1CNET        IFNE      'F'
326700020206     C     UTF(A)        ANDEQ     'S'
326800020206     C                   SETON                                        5628
326900020206     C                   MOVEL     MSG(101)      $MSG
327000020206     C                   GOTO      GIUCTR
327100020206     C                   ENDIF
327200020206      *
327300940525     C*
327400941011     C* SE FLAG ITA/EST = 'I' ---> NON POSSO INSERIRE COD.TASS. ESTERI
327500940811     C     V1CFIE        IFEQ      'I'
327600020201     C     V1CNET        ANDEQ     ' '
327700941011     C     NAR(A)        LOOKUP    NAZ(K)                                 35
327800950117     C     *IN35         IFEQ      *OFF
327900940811     C                   SETON                                        5628
328000940811     C                   MOVEL     MSG(47)       $MSG
328100940811     C                   GOTO      GIUCTR
328200940811     C                   ENDIF
328300941011     C                   ENDIF
328400941011     C*
328500950112     C* SE FLAG ITA/EST = 'E' ---> NON POSSO INSERIRE NELLA STESSA RIGA
328600950112     C*   DI DETTAGLIO SIA LA LINEA PARTENZA CHE IL COD.TASS. ITALIANI.
328700941026     C*   SE LA LNP E' ESTERA NON IMPORTA CONTROLLARE SE IL CODICE
328800941026     C*   TASSAZIONE E' ESTERO O ITALIANO PERCHE' LO ACCETTO UGUALMENTE
328900020312     c                   if        v1cfie = 'E' and lnpntw <> 'EEX' and
329000020312     c                             lnpntw <> 'EUP' and lnpntw <> 'FED'
329100941011     C     NAR(A)        LOOKUP    NAZ(K)                                 35
329200950117     C     *IN35         IFEQ      *ON
329300940811     C                   SETON                                        5628
329400940811     C                   MOVEL     MSG(48)       $MSG
329500940811     C                   GOTO      GIUCTR
329600940811     C                   ENDIF
329700941011     C                   ENDIF
329800020328
329900020328      * se tariffa estera non posso accettare codice tassazione 'IT'
330000020328     c                   if        v1cfie = 'E' and tadcts = 'IT'
330100020328     c                   eval      *in56 = *on
330200020328     c                   eval      *in28 = *on
330300020328     c                   eval      $msg = msg(107)
330400020328     c                   goto      giuctr
330500020328     c                   endif
330600020404
330700020404      * se tariffa DPD non posso accettare codice tassazione 'EE'
330800020404     c                   if        v1cnet = 'D' and tadcts = 'EE'
330900020404     c                   eval      *in56 = *on
331000020404     c                   eval      *in28 = *on
331100020404     c                   eval      $msg = msg(109)
331200020404     c                   goto      giuctr
331300020404     c                   endif
331400020624
331500020624      * se linea di partenza 950 = tutte le linee non posso accettare i codici
331600020624      * di tassazione 'IT' e 'EE'
331700020624     c                   if        tadlnp = 950 and
331800020624     c                             (tadcts = 'IT' or tadcts = 'EE')
331900020624     c                   eval      *in56 = *on
332000020624     c                   eval      *in28 = *on
332100020624     c                   eval      $msg = msg(110)
332200020624     c                   goto      giuctr
332300020624     c                   endif
332400940811     C*
332500940510     C* DECODIFICA CODICE TASSAZIONE
332600940525     C                   MOVEL     DCT(A)        DESCTS
332700020215     C                   MOVE      ORP(A)        TADORP
332800930629     C*
332900941011     C****  C. A. P.  ****
333000020624    3c                   if        tadcap <> *blanks
333100970718     C                   CLEAR                   DSSI95
333200970718     C                   CLEAR                   DSLV13
333300941011     C*
333400970718     C                   MOVEL     '3'           I95TCN
333500020624     c                   movel     tadnaz        i95nar
333600020624     c                   movel     tadcap        i95cap
333700040316     c                   z-add     dateu8        I95DAT
333800970718     C                   CALL      'FNLV13R'
333900970718     C                   PARM                    KPJBA
334000970718     C                   PARM                    DSLV13
334100970718     C                   PARM                    DSSI95
334200970718     C                   MOVEL     '1'           FNLV13            1
334300941011     C*
334400970718     C     O13ERR        IFNE      *BLANKS
334500970718     C                   MOVEL     O13MSG        $MSG
334600941011     C                   SETON                                        5728
334700941011     C                   GOTO      GIUCTR
334800941011     C                   ENDIF
334900990624     C* DECODIFICO LA NAZIONE SE E' DIVERSA DA BLANKS
335000020624     c                   if        tadnaz = *blanks
335100990624     C                   CLEAR                   DESNAZ
335200990624     C                   ELSE
335300990623     C                   MOVEL     '15'          COD
335400990623     C                   MOVEL     *BLANKS       KEY
335500020624     c                   movel     tadnaz        key
335600990623     C     KTAB          CHAIN     TABEL                              32
335700990623     C*
335800990623     C     *IN32         IFEQ      *OFF
335900990623     C     TBLFLG        ANDEQ     ' '
336000990623     C                   MOVEL     TBLUNI        DESNAZ
336100990623     C                   ELSE
336200990623     C* ERRORE
336300990623     C                   SETON                                        3828
336400990623     C                   MOVEL     MSG(81)       $MSG
336500990623     C                   GOTO      GIUCTR
336600990623     C                   ENDIF
336700990624     C                   ENDIF
336800930630     C*
336900930629     C* SE TARIFFA REGIONE O ITALIA NON DEVE ESISTERE IL CAP
337000940525     C     REG(A)        IFEQ      '9'
337100940526     C                   SETON                                        5728
337200940525     C                   MOVEL     MSG(25)       $MSG
337300940525     C                   GOTO      GIUCTR
337400940525     C                   ENDIF
337500940525     C*
337600930702     C* CONTROLLO SE IL CAP E' CONGRUENTE CON IL CODICE TASSAZIONE
337700970718    4C     TADCTS        IFNE      O95CTS
337800940526     C                   SETON                                        5728
337900940525     C                   MOVEL     MSG(26)       $MSG
338000930702     C                   GOTO      GIUCTR
338100940510    4C                   ENDIF
338200931117     C*
338300931117     C* SE INSERITO IL CAP NON BISOGNA IMMETTERE LA TARIFFA PROVINCIA
338400940510    4C     TADINO        IFNE      0
338500940526     C                   SETON                                        6128
338600940525     C                   MOVEL     MSG(27)       $MSG
338700931117     C                   GOTO      GIUCTR
338800940510    4C                   ENDIF
338900931117     C*
339000940510    3C                   ENDIF
339100930629     C*
339200940510     C****  SCAGLIONE  TARIFFA  ****
339300940510     C* SCAGLIONE OBBLIGATORIO
339400940511    3C     TADSGL        IFEQ      0
339500940526     C                   SETON                                        5828
339600940525     C                   MOVEL     MSG(28)       $MSG
339700900420     C                   GOTO      GIUCTR
339800941124   X3C                   ELSE
339900950119     C* WSAP = " " --> SI TRATTA DI SCAGLIONE SENZA DECIMALI QUINDI NON
340000950116     C*                SI POSSONO INSERIRE VALORI DOPO LA VIRGOLA
340100941124    4C     WSAP          IFEQ      ' '
340200941124     C                   MOVE      TADSGL        W0030
340300941124     C     W0030         IFGT      0
340400941124     C                   SETON                                        5828
340500950118     C                   MOVEL     MSG(46)       $MSG
340600941124     C                   GOTO      GIUCTR
340700941124     C                   ENDIF
340800941124    4C                   ENDIF
340900021118      * Se tariffa FedEx Documenti devo controllare che non sia stato immesso
341000021118      * uno scaglione > del peso possibile per questa tariffa
341100021118      * messaggio forzabile
341200021118     c                   If        V1cNet= 'F' and DueCtr >= §DfeDalD
341300021118     c                             and DueCtr <= §DfeAlD
341400021118     c                             and TadSgl > §DfePkgD
341500021118     c                             and ForzaMsg = *Blanks
341600021118     c                   Movel     Msg(112)      $Msg
341700021118     c                   Eval      *In58 = *On
341800021118     c                   Eval      *In28 = *On
341900021118     c                   Eval      ForzaMsg = '1'
342000021118     c                   GoTo      GiuCtr
342100021118     c                   EndIf
342200940510    3C                   ENDIF
342300930629     C*
342400940510     C****  TARIFFA CITTA'  ****
342500940510     C* TARIFFA CITTA' OBBLIGATORIA
342600940525     C     TADITR        IFEQ      0
342700940526     C                   SETON                                        5928
342800940525     C                   MOVEL     MSG(29)       $MSG
342900940525     C                   GOTO      GIUCTR
343000941123     C                   ELSE
343100050616     C* 88 OFF -  TARIFFA NON IN % verifico se si
343200941124     C*   POSSONO INSERIRE DECIMALI
343300941124     C     *IN88         IFEQ      *OFF
343400050616     C* LA DIVISA NON PREVEDE DECIMALI
343500990617     C     §CVFDC        ANDNE     'S'
343600941123     C                   MOVE      TADITR        W0030             3 0
343700941123     C     W0030         IFGT      0
343800941123     C                   SETON                                        5928
343900990617     C                   MOVEL     MSG(79)       $MSG
344000941123     C                   GOTO      GIUCTR
344100941123     C                   ENDIF
344200941123     C                   ENDIF
344300940525     C                   ENDIF
344400930629     C*
344500941123     C****  TARIFFA PROVINCIA  ****
344600050616     C* 88 OFF -  TARIFFA NON IN % verifico se si
344700941124     C*   POSSONO INSERIRE DECIMALI
344800941124     C  N88TADINO        IFGT      0
344900941123     C                   MOVE      TADINO        W0030             3 0
345000941123     C     W0030         IFGT      0
345100050616     C*  LA DIVISA NON PREVEDE DECIMALI
345200990617     C     §CVFDC        ANDNE     'S'
345300941123     C                   SETON                                        6128
345400990617     C                   MOVEL     MSG(79)       $MSG
345500941123     C                   GOTO      GIUCTR
345600941123     C                   ENDIF
345700941123     C                   ENDIF
345800941123     C*
345900960520     C****  RAPPORTO PESO/VOLUME  ****
346000980505     C* CONTROLLO SE IL TIPO TARIFFA PREVEDE LA GESTIONE DEGLI ARROTONADMENTI
346100980505     C* IN BASE AL FLAG DELLA TABELLA TR
346200980505     C     WRPV          IFNE      'S'
346300980505     C     TADRPV        ANDNE     0
346400980505     C                   SETON                                        8928
346500980505     C                   MOVEL     MSG(75)       $MSG
346600980505     C                   GOTO      GIUCTR
346700980505     C                   ENDIF
346800960520     C* SE INSERITO UN RAPPORTO PESO/VOL. SUPERIORE AL LIMITE INDICATO
346900960520     C*   IN TABELLA EMETTO ERRORE FORZABILE
347000960521     C     TADRPV        IFNE      SAVRPV
347100960520     C     TADRPV        ANDGT     §1GRPV
347200960520     C                   SETON                                        8928
347300960520     C                   MOVEL     MSG(59)       $MSG
347400960521     C                   MOVEL     TADRPV        SAVRPV
347500960520     C                   GOTO      GIUCTR
347600960520     C                   ENDIF
347700020404
347800020404      * Nazione
347900020624     c                   if        tadnaz <> *blanks and tadcap = *blanks
348000020404     c                   eval      $msg = msg(108)
348100020404     c                   eval      *in28 = *on
348200020404     c                   eval      *in57 = *on
348300020404     c                   goto      giuctr
348400020404     c                   endif
348500990617     C*
348600990617     C****  IMPORTO MASSIMO TARIFFA  ****
348700050616     C* 88 OFF -  TARIFFA NON IN % verifico se si
348800990617     C*   POSSONO INSERIRE DECIMALI
348900990617     C  N88TADMAX        IFGT      0
349000990617     C                   MOVE      TADMAX        W0030             3 0
349100990617     C     W0030         IFGT      0
349200050616     C*  LA DIVISA NON PREVEDE DECIMALI
349300990617     C     §CVFDC        ANDNE     'S'
349400990617     C                   SETON                                        6228
349500990617     C                   MOVEL     MSG(79)       $MSG
349600990618     C                   MOVEL     *ON           *INKK
349700990617     C                   GOTO      GIUCTR
349800990617     C                   ENDIF
349900990617     C                   ENDIF
350000990617     C*
350100990617     C****  IMPORTO MINIMO  TARIFFA  ****
350200050616     C* 88 OFF -  TARIFFA NON IN % verifico se si
350300990617     C*   POSSONO INSERIRE DECIMALI
350400990617     C  N88TADMIN        IFGT      0
350500990617     C                   MOVE      TADMIN        W0030             3 0
350600990617     C     W0030         IFGT      0
350700050616     C*  LA DIVISA NON PREVEDE DECIMALI
350800990617     C     §CVFDC        ANDNE     'S'
350900990617     C                   SETON                                        5228
351000990617     C                   MOVEL     MSG(79)       $MSG
351100990618     C                   MOVEL     *ON           *INKK
351200990617     C                   GOTO      GIUCTR
351300990617     C                   ENDIF
351400990617     C                   ENDIF
351500960520     C*
351600940525     C****  IMPORTO MASSIMO TARIFFA  ****
351700940525     C* SE INSERITO DEVE ESSERE MAGGIORE O UGUALE AL MINIMO
351800940525     C     TADMAX        IFGT      0
351900940525     C     TADMAX        ANDLT     TADMIN
352000940526     C                   SETON                                        6228
352100940525     C                   MOVEL     MSG(31)       $MSG
352200941107     C                   MOVEL     *ON           *INKK
352300940525     C                   GOTO      GIUCTR
352400940525     C                   ENDIF
352500980310      * E' inutile inserire dei valori minimi e massimi per scaglioni inferiori
352600980310      * al valore minimo tassabile o al valore tariffa finita
352700980310      * quindi faccio il controllo di TADMAX E/O TADMAX
352800980310     C     TADMAX        IFGT      0
352900980310     C     TADMIN        ORGT      0
353000980310      * VALORE TARIFFA FINITA
353100980310     C     TAMATA        IFGT      TADSGL
353200980310     C     TAMATA        OREQ      0
353300980601     C     WRKMIN        ANDGT     TADSGL
353400980310     C                   SETON                                        6228
353500980310     C                   MOVEL     MSG(73)       $MSG
353600980310     C                   MOVEL     *ON           *INKK
353700980310     C                   GOTO      GIUCTR
353800980310     C                   ENDIF
353900980310      *
354000980310     C                   ENDIF
354100940525     C*
354200940518     C                   Z-ADD     TADITR        RISITR
354300000000     C                   Z-ADD     TADINO        RISINO
354400000000     C                   Z-ADD     TADRPV        RISRPV
354500940525     C                   Z-ADD     TADMIN        RISMIN
354600940525     C                   Z-ADD     TADMAX        RISMAX
354700940510     C*
354800061011     C  n50KTAD          CHAIN     TITAD000                           32
354900061011     c   50ktad          chain     tiofd000                           32
355000940525     C  N32TADATB        COMP      'A'                                    32
355100940525     C*
355200940525     C     *IN32         IFEQ      *OFF
355300940526     C                   SETON                                        5828
355400940525     C                   MOVEL     MSG(30)       $MSG
355500940525     C                   ENDIF
355600940525     C*
355700940518     C                   Z-ADD     RISITR        TADITR
355800000000     C                   Z-ADD     RISINO        TADINO
355900000000     C                   Z-ADD     RISRPV        TADRPV
356000940525     C                   Z-ADD     RISMIN        TADMIN
356100940525     C                   Z-ADD     RISMAX        TADMAX
356200940510     C*
356300940525     C* 58 ON - RECORD TARIFFA GIA' ESISTENTE
356400940525     C   58              GOTO      GIUCTR
356500930629     C*
356600940510     C* SE TUTTO BENE SALVO I CAMPI PER LA DUPLICA
356700900420     C                   MOVEL     TADINO        DUPINO
356800940518     C                   Z-ADD     TADITR        DUPITR
356900900420     C                   Z-ADD     TADLNP        DUPLNP
357000900420     C                   MOVEL     TADCTS        DUPCTS
357100940518     C                   MOVEL     TADSGL        DUPSGL
357200900420     C                   Z-ADD     TADRPV        DUPRPV
357300020624     c                   movel     tadcap        DUPCAP
357400020624     c                   movel     tadnaz        DUPNAZ
357500940518     C                   Z-ADD     TADMIN        DUPMIN
357600940518     C                   Z-ADD     TADMAX        DUPMAX
357700930629     C*
357800940525     C                   SETON                                        14
357900020408     c                   eval      woksfl = '1'
358000000000     C     GIUCTR        TAG
358100940525     C                   SETON                                        22
358200940706     C                   UPDATE    TA01S05
358300940525     C   14              SETOFF                                       2214
358400930629     C*
358500940525     C   22              Z-ADD     NR1           REC1
358600940525     C   22              GOTO      ENDCT2
358700940511    2C                   ENDIF
358800940511     C*
358900020408     c                   if        woksfl = '0'
359000020408     C                   UPDATE    TA01S05
359100020408     c                   endif
359200940706     C                   READC     TA01S05                                29
359300940511    1C                   ENDDO
359400940510     C*
359500940509     C     ENDCT2        ENDSR
359600930629     C*
359700940509     C*--- IMMISSIONE TARIFFE ----------------------------------------*
359800921118     C     ADDTAD        BEGSR
359900921119     C                   Z-ADD     1             NR1
360000940706     C                   READC     TA01S05                                29
360100940524     C     *IN29         DOWEQ     '0'
360200921118     C     TADLNP        IFGT      0
360300000000     C                   EXSR      WTRTAD
360400000000     C                   END
360500921119     C*
360600940706     C                   READC     TA01S05                                29
360700000000     C                   END
360800000000     C                   ENDSR
360900940509     C*
361000990601     C*--- SCRITTURA TITAD SU DISCO ----------------------------------*
361100921118     C     WTRTAD        BEGSR
361200980219     C* VALORE POSITIVO NEI CAMPI
361300980220     C                   MLLZO     1             TADSGL
361400980219     C                   MLLZO     1             TADITR
361500980219     C                   MLLZO     1             TADINO
361600980219     C                   MLLZO     1             TADRPV
361700980219     C                   MLLZO     1             TADMIN
361800980219     C                   MLLZO     1             TADMAX
361900980219     C*
362000940629     C                   Z-ADD     V1CKSC        TADKSC
362100000000     C                   Z-ADD     PRG           TADPRG
362200000000     C                   MOVEL     *BLANKS       TADATB
362300910605     C*
362400000000     C                   Z-ADD     1             A
362500940524     C     TADCTS        LOOKUP    CCT(A)                                 35
362600990608     C   35              MOVE      ORP(A)        TADORP
362700940524     C  N35              MOVEL     *BLANKS       TADORP
362800900320     C                   MOVEL     TAMCTR        TADCTR
362900940510     C*
363000061011     C  n50KTAD          SETLL     TITAD000                               32
363100061011     c   50ktad          setll     tiofd000                               32
363200940510     C*
363300940524     C     *IN32         IFEQ      *OFF
363400040913     C                   Z-ADD     DATEU8        TADDTR
363500940511     C                   MOVEL     *BLANKS       TADFTR
363600061011     C  n50              WRITE     TITAD000                             19
363700061011     c   50              write     tiofd000                             19
363800940510     C*
363900940510     C                   ELSE
364000940510     C*
364100940518     C                   Z-ADD     TADITR        RISITR
364200940518     C                   Z-ADD     TADINO        RISINO
364300940518     C                   Z-ADD     TADRPV        RISRPV
364400940518     C                   Z-ADD     TADMIN        RISMIN
364500940518     C                   Z-ADD     TADMAX        RISMAX
364600940518     C*
364700061011     C  n50KTAD          READE     TITAD000                               H2
364800061011     c   50ktad          reade     tiofd000                               H2
364900000000     C   H2              GOTO      FINE
365000000000     C                   MOVEL     *BLANKS       TADATB
365100130124     C********           Z-ADD     DATEU8        TADDTR
365200940511     C                   MOVEL     *BLANKS       TADFTR
365300940518     C                   Z-ADD     RISITR        TADITR
365400000000     C                   Z-ADD     RISINO        TADINO
365500000000     C                   Z-ADD     RISRPV        TADRPV
365600940518     C                   Z-ADD     RISMIN        TADMIN
365700940518     C                   Z-ADD     RISMAX        TADMAX
365800061011     C  n50              UPDATE    TITAD000
365900061011     c   50              update    tiofd000
366000940510     C                   ENDIF
366100940510     C*
366200921118     C  N19              SETON                                        30
366300921118     C                   ENDSR
366400940509     C*
366500940509     C*--- SR AGGIORNAMENTO ------------------------------------------*
366600921118     C     AGGIOR        BEGSR
366700050701
366800050701      * prima di caricare il subfile se è una tariffa a spedizione leggo
366900050701      * tutto il dettaglio per controllare che non esistano più scaglioni
367000050701      * per la stessa lnp e cts
367100050701      * questo perchè in fatturazione viene preso solo 1 scaglione quindi
367200050701      * non ha senso inserirne di più
367300050701     c                   If        tiptar = '2'
367400050701     c                   ExSr      sr_ctrtarspe
367500050701     c                   EndIf
367600110128
367700110128      /free
367800110128       //?Se tariffa FedEx devo controllare il rapporto perso volume
367900110128       //?deve essere uguale per tutti gli scaglioni
368000110128        IF  V1Cnet = 'F';
368100110128          exsr CtrRpv;
368200110128        ENDIF;
368300110128      /end-free
368400050701
368500921118     C     SUAGG2        TAG
368600061011     C  n50KTTAD         SETLL     TITAD000
368700061011     c   50kttad         setll     tiofd000
368800921118     C                   EXSR      PULSF
368900921118     C                   EXSR      CARSFA
369000930224     C     *IN21         IFEQ      '1'
369100921118     C*
369200921118     C     SUAGG1        TAG
369300940526     C                   Z-ADD     0             V1CRDA
369400940526     C                   MOVEL     *BLANKS       V1CRA
369500900924     C                   SETOFF                                       19
369600120315
369700120315       //?Imposto i tasti funzione nella riga mobile
369800120315     c                   eval      wPrima = *off
369900120315     c                   eval      wSeconda = *off
370000120315     c                   eval      wTerza = *on
370100120315     c                   eval      wQuarta = *off
370200120315     c                   ExSr      Tastifun
370300120315
370400000000     C     SUAGG         TAG
370500120319     C                   WRITE     TA01Z04
370600940706     C                   EXFMT     TA01C03
370700921118     C*
370800940526     C* PULIZIA CAMPO MESSAGGIO E RELATIVO INDICATORE (*IN28)
370900940526     C                   CLEAR                   VCCMSG
371000940526     C                   SETOFF                                       28
371100940624     C* AZZERO GLI INDICATORI RELATIVI AI MESSAGGI DI ERRORE
371200940526     C                   SETOFF                                       596162
371300960522     C                   SETOFF                                       89
371400940526     C*
371500120315     c  nkc
371600120315     CanNKL              DO
371700921118     C** CMD9 -IMMISSIONE
371800921118     C   KI              DO
371900050630werr c                   if        werr = *Off
372000110128     c                             and not werrRpv
372100921118     C                   EXSR      IMMISS
372200050701
372300050701      * per tariffa a spedizione devo controllare ora se tutto a posto
372400050701      * perchè non ragiono sul subfile ma sui dati caricati sul titad
372500050701     c                   If        tiptar = '2'
372600050701     c                   ExSr      sr_ctrtarspe
372700050701     c                   EndIf
372800050701
372900050701      * se errore per + scaglioni in tariffa a spedizione imposto
373000050701      * il msg e accendo l'indicatore
373100050701     c                   If        werr = *On
373200050701     c                   Eval      vccmsg = 'Par ' + wlnp + ' Arrivo ' +
373300050701     c                             wcts
373400050701     c                   If        wcap <> *Blanks
373500050701     c                   Eval      vccmsg = %trim(vccmsg) +
373600050701     c                             ' - c.a.p. ' + wcap
373700050701     c                   EndIf
373800050701     c                   Eval      vccmsg = %trim(vccmsg) +
373900050701     c                             ' presenti più scaglioni'
374000050701     c                   Eval      *In28 = *On
374100050701     c                   Goto      suagg2
374200050701     c                   EndIf
374300110128
374400110128      /free
374500110128       //?Se tariffa FedEx devo controllare il rapporto perso volume
374600110128       //?deve essere uguale per tutti gli scaglioni
374700110128        IF  V1Cnet = 'F';
374800110128          exsr CtrRpv;
374900110128        ENDIF;
375000110128      /end-free
375100110128       //?Se tariffa FedEx e scaglioni con rapporto peso volume
375200110128       //?diverso emetto messaggio di errore
375300110128     c                   IF        werrRpv
375400110128     c                   eval      VCCmsg = 'Par ' + %editc(wlnpRpv:'X') +
375500110128     c                             ' Arrivo ' + wctsRpv +
375600110128     c                             ' Scaglione ' + %editc(wsglRpv:'4') +
375700110128     c                             ' presente P/V diverso'
375800110128     c                   eval      *in28 = *on
375900110128     c                   goto      suagg2
376000110128     c                   ENDIF
376100110128
376200980313     C* CONTROLLO SE TARIFFA IMPORT CON LA TARIFFA PARTICOLARE ASSIC. X CONTO
376300980313     C     UNO           IFEQ      ' '
376400980313     C                   EXSR      CTRIMP
376500980313     C   28              MOVEL     MSG(72)       VCCMSG
376600980313     C   28              CLEAR                   V1CMSG
376700980313     C   28              MOVEL     '1'           UNO
376800980313     C   28              GOTO      SUAGG2
376900980313     C                   END
377000120314       //?F1 - dati manca tariffa
377100120314     c                   IF        *inka
377200120314     c                   exsr      gesw01
377300120315     c                   goto      suagg
377400120314     c                   ENDIF
377500120315       //?F3 fine
377600120315     c   kc              goto      fine
377700921118     C*
377800921118     C** CMD12-FINE LAVORO NO AGGIORNAMENTO
377900050630     C   kl              GOTO      SUAGG1
378000921118     C** CMD6 -AGGIORNAMENTO E FINE LAVORO
378100921118     C  NKF              GOTO      SUAGG2
378200921118     C   KF              GOTO      ENDAGG
378300120315       //?F24 Altri tasti
378400120315     c                   IF        *inky
378500120315     c                   exsr      gesf24
378600120315     c                   goto      suagg
378700120315     c                   ENDIF
378800050630werr c                   endif
378900921118     C                   END
379000900924     C*
379100921118     C** ROLLUP  AVANZAMENTO NO AGG.
379200940524     C     *IN25         IFEQ      *ON
379300940310     C                   MOVEL     KE            KX
379400940518     C                   MOVEL     PARX          COMX              9
379500061011     C  n50KSCOR         SETLL     TITAD000                               32
379600061011     c   50kscor         setll     tiofd000                               32
379700061011     C   32
379800061011     Cann50KTAM2         READE     TITAD000                               33
379900061011     c   32
380000061011     Can 50ktam2         reade     tiofd000                               33
380100940310     C                   EXSR      CARSFA
380200940310     C                   GOTO      SUAGG1
380300940310     C                   ENDIF
380400921118     C*
380500921118     C** CONTROLLI AGGIORNAMENTO
380600000000     C                   EXSR      CTRAGG
380700940526     C   28              GOTO      SUAGG
380800921118     C*
380900921118     C** ESEGUE AGGIORNAMENTI
381000000000     C                   EXSR      AGGTAD
381100050701
381200050701      * per tariffa a spedizione devo controllare ora se tutto a posto
381300050701      * perchè non ragiono sul subfile ma sui dati caricati sul titad
381400050701     c                   If        tiptar = '2'
381500050701     c                   ExSr      sr_ctrtarspe
381600050701     c                   EndIf
381700050701
381800050701      * se errore per + scaglioni in tariffa a spedizione imposto
381900050701      * il msg e accendo l'indicatore
382000050701     c                   If        werr = *On
382100050701     c                   Eval      vccmsg = 'Par ' + wlnp + ' Arrivo ' +
382200050701     c                             wcts
382300050701     c                   If        wcap <> *Blanks
382400050701     c                   Eval      vccmsg = %trim(vccmsg) +
382500050701     c                             ' - c.a.p. ' + wcap
382600050701     c                   EndIf
382700050701     c                   Eval      vccmsg = %trim(vccmsg) +
382800050701     c                             ' presenti più scaglioni'
382900050701     c                   Eval      *In28 = *On
383000050701     c                   Goto      suagg
383100050701     c                   EndIf
383200110128
383300110128      /free
383400110128       //?Se tariffa FedEx devo controllare il rapporto perso volume
383500110128       //?deve essere uguale per tutti gli scaglioni
383600110128        IF  V1Cnet = 'F';
383700110128          exsr CtrRpv;
383800110128        ENDIF;
383900110128      /end-free
384000110128       //?Se tariffa FedEx e scaglioni con rapporto peso volume
384100110128       //?diverso emetto messaggio di errore
384200110128     c                   IF        werrRpv
384300110128     c                   eval      VCCmsg = 'Par ' + %editc(wlnpRpv:'X') +
384400110128     c                             ' Arrivo ' + wctsRpv +
384500110128     c                             ' Scaglione ' + %editc(wsglRpv:'4') +
384600110128     c                             ' presente P/V diverso'
384700110128     c                   eval      *in28 = *on
384800110128     c                   goto      suagg
384900110128     c                   ENDIF
385000050701
385100980313     C* CONTROLLO SE TARIFFA IMPORT CON LA TARIFFA PARTICOLARE ASSIC. X CONTO
385200980313     C     UNO           IFEQ      ' '
385300980313     C                   EXSR      CTRIMP
385400980313     C   28              MOVEL     MSG(72)       VCCMSG
385500980313     C   28              CLEAR                   V1CMSG
385600980313     C   28              MOVEL     '1'           UNO
385700980313     C   28              GOTO      SUAGG
385800980313     C                   END
385900120315       //?F1 - dati manca tariffa
386000120315     c                   IF        *inka
386100120315     c                   exsr      gesw01
386200120315     c                   goto      suagg
386300120315     c                   ENDIF
386400011214     C*
386500011214     C** F10 - STAMPA TARIFFA (ALLA FINE)
386600011214     C   KJ
386700011214     CAN 15              GOTO      ENDAGG
386800011214     C   KJ
386900011214     CANN15              MOVEL     *INKJ         *INKF
387000921118     C*
387100961212     C** CMD6 -AGGIORNAMENTO E FINE
387200991115     C   KF              MOVEL     'C'           FLGRTN
387300921118     C   KF              GOTO      ENDAGG
387400120319       //?F24 Altri tasti
387500120319     c                   IF        *inky
387600120319     c                   exsr      gesf24
387700120319     c                   goto      suagg
387800120319     c                   ENDIF
387900900924     C*
388000961212     C** CMD15-P O R T O  (ABILITATO SOLO NELLE OFFERTE)
388100980902     C   KP              DO
388200031124      * recupero dalla tabella CAT la data di elaborazione CAT consigliata
388300031124     c                   Clear                   Tibs02Ds
388400031124     c                   Clear                   dCat
388500031124     c                   Eval      T02Mod = 'C'
388600031124     c                   Eval      T02Sif = Knsif
388700031124     c                   Eval      T02Cod = 'CAT'
388800031124     c                   Movel(p)  '1'           T02Ke1
388900031124     c                   Call      'TIBS02R'
389000031124     c                   Parm                    Kpjba
389100031124     c                   Parm                    Tibs02Ds
389200031124     c                   If        T02Err = *Blanks
389300031124     c                   Movel     T02Uni        dCat
389400031124     c                   EndIf
389500031124    1 *
389600961212     C                   CLEAR                   DSTE00
389700961212     C                   MOVEL     'O02'         D00OP0
389800961212     C                   MOVEL     'F15'         D00OP1
389900961212     C                   MOVEL     'P'           D00TLA
390000091127      * Con l'arrivo delle trattative dobbiamo segnalare ai pgm del CAT che
390100091127      * stiamo parlando di trattativa e non di visita
390200091127      * Se trattativa (ind. 85 acceso) impostiamo "X" nel campo D00DSF
390300091125     C**                 MOVEL     'S'           D00DSF
390400091125     C   85              MOVEL     'X'           D00DSF
390500091127     C                   MOVEL     'O'           D00CTO
390600961212     C                   MOVEL     V1CNRV        D00KSC
390700961212     C                   MOVEL     V1CCTR        D00CTR
390800091125     C                   MOVEL     V1CPRG        D00PRG
390900961212     C                   MOVEL     V1CFIE        D00FIE
391000961212     C                   MOVEL     V1CTSP        D00TSP
391100011023     C                   MOVEL     V1CDIV        D00DIV
391200020205     C                   MOVE      V1CNET        D00DPD
391300011030     C                   MOVEL     *BLANKS       D00PRI
391400031124      * propongo data massima tra la data del giorno e quella recuperata
391500031124      * dalla tavella CAT
391600031124     c                   if        dateu8 > §CATdta
391700031124     C                   MOVEL     DATEU8        D00DTC
391800031124     C                   ELSE
391900031124     C                   MOVEL     §CATDTA       D00DTC
392000031124     C                   ENDIF
392100031124      *
392200970131     C                   CALL      'TNTE01C'
392300961212     C                   PARM                    KPJBA
392400961212     C                   PARM                    DSTE00
392500961212     C* D00ERR = "1" --> ERRORE
392600961212     C     D00ERR        IFEQ      '1'
392700961212     C                   SETON                                          28
392800961212     C                   MOVEL     D00MSG        VCCMSG
392900961219     C                   GOTO      SUAGG1
393000961212     C                   ENDIF
393100961220     C* RICARICO IL SFILE PERCHE' POTREBBERO ESSERCI STATE VARIAZIONI
393200961219     C                   GOTO      SUAGG2
393300980902     C                   ENDDO
393400961212     C*
393500961212     C** CMD17-D E L T A
393600980902     C   KR              DO
393700980514     C                   CLEAR                   DSTE00
393800980514     C     *IN50         IFEQ      *OFF
393900980514     C                   MOVEL     'T02'         D00OP0
394000980514     C                   MOVEL     'T'           D00CTO
394100980514     C                   MOVEL     V1CKSC        D00KSC
394200980514     C                   MOVEL     V1CPRG        D00PRG
394300980514     C                   ELSE
394400980514     C                   MOVEL     'O02'         D00OP0
394500091127      * Con l'arrivo delle trattative dobbiamo segnalare ai pgm del CAT che
394600091127      * stiamo parlando di trattativa e non di visita
394700091127      * Se trattativa (ind. 85 acceso) impostiamo "X" nel campo D00DSF
394800091127     C                   MOVEL     'O'           D00CTO
394900091125     C   85              MOVEL     'X'           D00DSF
395000091125     C                   MOVEL     V1CPRG        D00PRG
395100980514     C                   MOVEL     V1CNRV        D00KSC
395200980514     C                   ENDIF
395300031124      * recupero dalla tabella CAT la data di elaborazione CAT consigliata
395400031124     c                   Clear                   Tibs02Ds
395500031124     c                   Clear                   dCat
395600031124     c                   Eval      T02Mod = 'C'
395700031124     c                   Eval      T02Sif = Knsif
395800031124     c                   Eval      T02Cod = 'CAT'
395900031124     c                   Movel(p)  '1'           T02Ke1
396000031124     c                   Call      'TIBS02R'
396100031124     c                   Parm                    Kpjba
396200031124     c                   Parm                    Tibs02Ds
396300031124     c                   If        T02Err = *Blanks
396400031124     c                   Movel     T02Uni        dCat
396500031124     c                   EndIf
396600031124    1 *
396700980514     C                   MOVEL     'F17'         D00OP1
396800091125      * Con l'arrivo delle trattative abbiamo modificato alcuni campi della DS
396900091125      * per una giusta gestione del CAT. non si mette + "S" nel campo
397000091125      * d00dsf in caso di tariffe / offerte e non trattativa
397100091125     C*****              MOVEL     'S'           D00DSF
397200980514     C                   MOVEL     'D'           D00TLA
397300980514     C                   MOVEL     V1CCTR        D00CTR
397400980514     C                   MOVEL     V1CFIE        D00FIE
397500980514     C                   MOVEL     V1CTSP        D00TSP
397600011023     C                   MOVEL     V1CDIV        D00DIV
397700020213     C                   MOVE      V1CNET        D00DPD
397800011030     C                   MOVEL     *BLANKS       D00PRI
397900031124      * propongo data massima tra la data del giorno e quella recuperata
398000031124      * dalla tavella CAT
398100031124     c                   if        dateu8 > §CATdta
398200031124     C                   MOVEL     DATEU8        D00DTC
398300031124     C                   ELSE
398400031124     C                   MOVEL     §CATDTA       D00DTC
398500031124     C                   ENDIF
398600031124      *
398700980514     C                   CALL      'TNTE01C'
398800980514     C                   PARM                    KPJBA
398900980514     C                   PARM                    DSTE00
399000980514     C* D00ERR = "1" --> ERRORE
399100980514     C     D00ERR        IFEQ      '1'
399200980514     C                   SETON                                          28
399300980514     C                   MOVEL     D00MSG        VCCMSG
399400980514     C                   GOTO      SUAGG1
399500980514     C                   ENDIF
399600980902     C                   ENDDO
399700900410     C**
399800920525     C**  CMD8 - M A N I P O L A Z I O N E
399900900410     C**
400000920525     C   KH              DO
400100900410     C                   EXSR      IMPMAN
400200941123     C*
400300900410     C     FOR07         TAG
400400070312     c                   eval      wintftc = *off
400500120713     c                   eval      wintcts = *off
400600940706     C                   EXFMT     TA01D07
400700941123     C* PULIZIA CAMPO MESSAGGIO E RELATIVO INDICATORE (*IN28)
400800941123     C                   CLEAR                   V6CMSG
400900960729     C                   SETOFF                                       289086
401000050616     c                   clear                   h7riga
401100050616     c                   clear                   h7colo
401200941123     C* AZZERO GLI INDICATORI RELATIVI AI MESSAGGI DI ERRORE
401300941123    1C     63            DO        76            I
401400941123     C                   MOVE      '0'           *IN(I)
401500941123    1C                   ENDDO
401600941123     C*
401700120315       //?F3 fine
401800120315     c   kc              GOTO      FINE
401900900410     C* CMD12 - RITORNO
402000940518     C   KL              GOTO      SUAGG1
402100900410     C* CONTROLLO
402200900410     C                   EXSR      CTRMAN
402300941109     C   28              GOTO      FOR07
402400070312      * se int.tariffe particolari riemetto la videata
402500070312     c                   if        wintftc = *on
402600070312     c                   goto      for07
402700070312     c                   endif
402800120713       //?se interrogazione codici tassazione riemetto la videata
402900120713     c                   IF        wintcts
403000120713     c                   goto      for07
403100120713     c                   ENDIF
403200921118     C*
403300920525     C* CMD6 - AGGIORNAMENTO TARIFFA
403400020312     c                   if        *inkf
403500020312     c                   clear                   wsimula
403600020312     C                   EXSR      MANIPO
403700020313     c                   if        wnoagg = *on
403800020313     c                   eval      vccmsg = msg(105)
403900020313     c                   eval      *in28 = *on
404000020312     c                   goto      suagg2
404100020312     c                   endif
404200020312     c                   eval      wsimula = '1'
404300020312     c                   exsr      manipo
404400020312     C                   GOTO      SUAGG2
404500020312     c                   endif
404600900410     C* ENTER - CONTROLLO E RIMANGO NELLA VIDEATA
404700900410     C                   GOTO      FOR07
404800900410     C                   END
404900960523     C**
405000960523     C**  CMD13 - D U P L I C A    T A R I F F A
405100960523     C**
405200960523     C     *INKM         IFEQ      *ON
405300960523     C* PULIZIA CAMPI VIDEATA
405400960523     C                   CLEAR                   V8CLNN
405500960523     C                   CLEAR                   V8CCTN
405600960523     C                   CLEAR                   V8CLNP
405700960523     C                   CLEAR                   V8CCTS
405800960523     C                   CLEAR                   WORP
405900960523     C                   CLEAR                   WORPN
406000960527     C                   CLEAR                   SAVCTN
406100960527     C                   CLEAR                   SAVLNN
406200041222      *
406300041222     c                   clear                   linpar
406400041223     c                   clear                   W_sbflnp
406500960523     C*
406600960523     C     FOR08         TAG
406700960523     C                   EXFMT     TA01D08
406800960523     C* PULIZIA CAMPO MESSAGGIO E RELATIVO INDICATORE (*IN28)
406900960523     C                   CLEAR                   V8CMSG
407000960523     C                   SETOFF                                       287172
407100960523     C                   SETOFF                                       7374
407200960523     C*
407300120315       //?F3 fine
407400120315     c   kc              GOTO      FINE
407500960523     C* CMD12 - RITORNO
407600960523     C   KL              GOTO      SUAGG1
407700041222
407800041222     C* CMD19 _ SOLO PER LA CARTELLO IMPOSTO PIU' LINEE PARTENZA DA CREARE
407900041222     C                   IF        *INKT
408000041222     C                   EXSR      SR_GESLNP
408100041222      * RIEMETTO LA VIDEATA
408200041222     C                   GOTO      FOR08
408300041222     C                   ENDIF
408400041222
408500960523     C* CONTROLLO
408600960523     C                   EXSR      CTRD08
408700960523     C   28              GOTO      FOR08
408800960523     C*
408900960523     C* CMD6 - AGGIORNAMENTO TARIFFA
409000990817     C     *INKF         IFEQ      '1'
409100041222      * duplica --- se ho solo un codice altrimenti elaboro la schiera
409200041222     c                   if        W_sbflnp <> ' '
409300041222     c                   z-add     1             po
409400041223     c                   dow       po<=120
409500041222     c                   if        lnpel(po) > *zeros
409600041222     c                   eval      v8clnn = lnpel(po)
409700041222     c                   exsr      duptar
409800041223     c                   add       1             po
409900041222     c                   iter
410000041222     c                   else
410100041222     c                   leave
410200041222     c                   endif
410300041222     c                   enddo
410400041222
410500041222     c                   else
410600041222      *
410700990817     C                   EXSR      DUPTAR
410800041222
410900041222     c                   endif
411000990817     C*
411100990817     C                   GOTO      SUAGG2
411200990817     C                   ENDIF
411300960523     C* ENTER - CONTROLLO E RIMANGO NELLA VIDEATA
411400960523     C                   GOTO      FOR08
411500960523     C                   ENDIF
411600921118     C*
411700921118     C** SE CANCELLATI RECORD RIEMETTO SUBFILE
411800921118     C   19              GOTO      SUAGG2
411900900410     C**
412000940526     C     V1CRDA        IFNE      0
412100940526     C     V1CRA         ORNE      *BLANKS
412200921118     C                   GOTO      ENDAGG
412300900321     C                   END
412400000000     C** E' STATA DIGITATA UNA CHIAVE DI RICERCA  E
412500000000     C** QUINDI DOPO AVER EFFETTUTATO L'AGGIORNAMENTO
412600000000     C** RICERCO LA CHIAVE E RIEMETTO IL SUBFILE
412700000000     C                   GOTO      SUAGG1
412800921118     C                   END
412900930224     C                   END
413000050701
413100050630      * se F12 e errore non posso tornare indietro devo restare nel
413200050630      * subfile ed eliminare gli scaglioni in più
413300120315      //?stessa cosa per F3
413400120315     c                   If        (*inkl or *inkc) and werr = *On
413500050701     c                             and tiptar = '2'
413600050701     c                   ExSr      sr_ctrtarspe
413700050701
413800050701      * se errore per + scaglioni in tariffa a spedizione imposto
413900050701      * il msg e accendo l'indicatore
414000050701     c                   If        werr = *On
414100050701     c                   Eval      vccmsg = 'Par ' + wlnp + ' Arrivo ' +
414200050701     c                             wcts
414300050701     c                   If        wcap <> *Blanks
414400050701     c                   Eval      vccmsg = %trim(vccmsg) +
414500050701     c                             ' - c.a.p. ' + wcap
414600050701     c                   EndIf
414700050701     c                   Eval      vccmsg = %trim(vccmsg) +
414800050701     c                             ' presenti più scaglioni'
414900050701     c                   Eval      *In28 = *On
415000050701     c                   EndIf
415100050701
415200050701     c   28              goto      suagg1
415300050630     c                   endif
415400110128
415500110128       //?Se F12 ed errore per il rapporto peso volume devo prima
415600110128       //?i dati a video poi posso uscire
415700120315       //?stessa cosa per F3
415800120315     c                   IF        (*inkl or *inkc) and werrRpv and
415900110128     c                             V1Cnet = 'F'
416000110128     c                   exsr      CtrRpv
416100110128       //?Se tariffa FedEx e scaglioni con rapporto peso volume
416200110128       //?diverso emetto messaggio di errore
416300110128     c                   IF        werrRpv
416400110128     c                   eval      VCCmsg = 'Par ' + %editc(wlnpRpv:'X') +
416500110128     c                             ' Arrivo ' + wctsRpv +
416600110128     c                             ' Scaglione ' + %editc(wsglRpv:'4') +
416700110128     c                             ' presente P/V diverso'
416800110128     c                   eval      *in28 = *on
416900110128     c                   goto      suagg1
417000110128     c                   ENDIF
417100110128     c                   ENDIF
417200050701
417300000000     C** ENTER  AGGIORNAMENTO E RIMANGO FERMO
417400050630     C*  RIPETO RIEMPIMENTO SF
417500921118     C     ENDAGG        ENDSR
417600900410     C*
417700930623     C*--- IMPOSTO CAMPI MANIPOLAZIONE -------------------------------*
417800900410     C     IMPMAN        BEGSR
417900900410     C                   MOVE      '+'           VIDAD
418000900410     C                   Z-ADD     0             VIDLIR
418100900530     C                   Z-ADD     0             VIDPEI
418200020312
418300020314     c                   eval      vidtad = 'SI'
418400020312     c                   eval      vidtpt = 'NO'
418500050616     c                   eval      vidtgc = 'NO'
418600020312
418700900410     C                   MOVE      'E'           VIDADE
418800991008     C                   Z-ADD     0             VIDALI
418900940526     C                   MOVE      '+'           VIDRAD
419000910606     C                   Z-ADD     0             VIDVRR
419100050627     c                   Clear                   vidarr
419200061207     c                   clear                   vidirr
419300960523     C                   MOVE      'O'           VIDIOP
419400960523     C                   CLEAR                   VMP
419500930625     C                   MOVE      'O'           VIDIO
419600930628     C                   CLEAR                   VMC
419700930625     C                   MOVE      'O'           VIDIO2
419800940518     C                   Z-ADD     0             VIDSGL
419900070309     c                   eval      vidioftc = 'O'
420000070309     c                   clear                   vftc
420100930625     C                   MOVE      'S'           VIDVTC
420200930625     C                   MOVE      'S'           VIDVTP
420300941124     C                   MOVE      'S'           VIDVMN
420400941124     C                   MOVE      'S'           VIDVMX
420500990624     C* DIVISA
420600990624     C                   MOVEL     V1CDIV        TAMDIV
420700010323     C* SE DIVISA SENZA DECIMALI NON EMETTO LA RIGA RELATIVA ALL'ARROTONDAMENTO
420800010323     C* AI DECIMALI
420900010323     C     §CVFDC        IFNE      'S'
421000010323     C* IMPOSTO I CAMPI A VIDEO PROTETTI E NON VISUALIZZZATI
421100010323     C* NDPR SOSTITUISCE GLI INDICATORI IN QUANTO FINITI
421200010323     C                   MOVEL     NDPR          PROSAD
421300010323     C                   ENDIF
421400010329     C* PULISCO I CAMPI REALTIVI LÌARROTONDAMENTO
421500010329     C                   CLEAR                   VIDSAD
421600010329     C                   CLEAR                   VIDDEC
421700050616
421800050616     c                   clear                   h7riga
421900050616     c                   clear                   h7colo
422000010323     C*
422100900410     C                   ENDSR
422200900410     C*
422300930623     C*--- CONTROLLO CAMPI VIDEO MANIPOLAZIONE -----------------------*
422400900410     C     CTRMAN        BEGSR
422500020312
422600070309      * spengo indicatori che uso in questa videata ma che sono stati
422700070309      * utilizzati in videate precedenti
422800020312     c                   eval      *in82 = *off
422900070309      * imposto subito il campo di comodo delle tariffe particolari xchè
423000070309      * mi serve da subito e non solo nel momento dei controlli delle
423100070309      * singole tariffe particolari
423200070309     c                   movea     vftc          comftc
423300020314
423400020327      * deve essere fatta almeno una scelta
423500020314     c                   if        vidtad = 'NO' and vidtpt = 'NO'
423600050616     c                             and vidtgc = 'NO'
423700020314     c                   eval      v6cmsg = msg(106)
423800020314     c                   eval      *in24 = *on
423900020314     c                   eval      *in28 = *on
424000020314     c                   goto      endman
424100020314     c                   endif
424200020312
424300900606     C                   MOVE      VIDPEI        VIDPER            5 4
424400940526     C*
424500940526     C****  VALORE/PERCENTUALE  ****
424600900410     C     VIDLIR        IFNE      0
424700940526     C     VIDPER        ANDNE     0
424800940526     C                   MOVEL     MSG(33)       V6CMSG
424900940526     C                   SETON                                        6328
425000900410     C                   GOTO      ENDMAN
425100940526     C                   ENDIF
425200050617
425300050617      * Se immesso un importo di aumento/diminuzione non posso manipolare
425400050617      * le tariffe particolari visto che sono miste cioè una parte espresse
425500050617      * ad importo e una parte espresse in %
425600050617      * la manipolazione è possibile solo inserendo una % +/-
425700050617     c                   If        vidlir <> *Zeros and vidtpt = 'SI'
425800050617     c                   Eval      v6cmsg = msg(116)
425900050617     c                   Eval      *In28 = *On
426000050617     c                   Eval      *In82 = *On
426100050617     c                   Goto      endman
426200050617     c                   EndIf
426300050616
426400050617      * rapporto peso/volume immettere o il valore o l'annullamento
426500061207      * o l'inserimento
426600061207     c                   If        (vidvrr <> *Zeros and vidarr <> *blanks) or
426700061207     c                             (vidvrr <> *Zeros and vidirr <> *zeros) or
426800061207     c                             (vidarr <> *blanks and vidirr <> *zeros)
426900050616     c                   Eval      v6cmsg = msg(33)
427000050616     c                   Eval      *In28 = *On
427100050616     c                   z-add     15            H7riga
427200061207     c                   z-add     29            H7colo
427300050616     c                   Goto      endman
427400050616     c                   EndIf
427500900410     C*
427600910606     C     VIDVRR        IFEQ      0
427700940526     C     VIDLIR        ANDEQ     0
427800940526     C     VIDPER        ANDEQ     0
427900050616     c     vidarr        andeq     *Blanks
428000061207     c     vidirr        andeq     *zeros
428100940526     C                   MOVEL     MSG(33)       V6CMSG
428200940526     C                   SETON                                        6328
428300900410     C                   GOTO      ENDMAN
428400940526     C                   ENDIF
428500020312
428600020312      * Manipolazione tariffe particolari
428700020327      * devono essere presenti le tariffe particolari
428800020312     c                   if        vidtpt = 'SI'
428900070129     c  n50ktam2         chain(n)  titpt01l
429000070129     c   50ktam2         chain(n)  tiopt01l
429100061011     c                   if        not %found
429200020312     c                   eval      v6cmsg = msg(104)
429300020312     c                   eval      *in28 = *on
429400020312     c                   eval      *in82 = *on
429500020312     c                   goto      endman
429600020312     c                   endif
429700020312     c                   endif
429800050617
429900050617      * Manipolazione tariffe di giacenza
430000050617      * devono essere presenti le tariffe di giacenza
430100050617     c                   if        vidtgc = 'SI'
430200050913     c  n50ktam2         chain     titgc01l
430300050913     c   50ktam2         chain     tiogc01l
430400050913     c                   if        not %found or tgcatb = 'A'
430500050617     c                   eval      v6cmsg = msg(117)
430600050617     c                   eval      *in28 = *on
430700050617     c                   z-add     10            H7riga
430800050617     c                   z-add     35            H7colo
430900050617     c                   goto      endman
431000050617     c                   endif
431100050617     c                   endif
431200020312
431300930623     C*
431400050616     C* 88 OFF -  TARIFFA NON IN % verifico se si
431500941124     C*   POSSONO INSERIRE DECIMALI
431600941124     C  N88VIDLIR        IFGT      0
431700941124     C                   MOVE      VIDLIR        W0030
431800941124     C     W0030         IFGT      0
431900050616     C*  la DIVISA NON AMMETTE DECIMALI
432000990617     C     §CVFDC        ANDNE     'S'
432100990617     C                   MOVEL     MSG(79)       V6CMSG
432200941124     C                   SETON                                        6328
432300941124     C                   GOTO      ENDMAN
432400941124     C                   ENDIF
432500941124     C                   ENDIF
432600941124     C*
432700050616     C* 88 OFF -  TARIFFA NON IN % verifico se si
432800050616     C*   POSSONO INSERIRE DECIMALI
432900960729     C  N88VIDPEI        IFGT      0
433000960729     C     VIDALI        ANDEQ     0
433100050616     C*  la DIVISA NON AMMETTE DECIMALI
433200010329     C     §CVFDC        ANDNE     'S'
433300960729     C                   MOVEL     MSG(69)       V6CMSG
433400960729     C                   SETON                                        8628
433500960729     C                   GOTO      ENDMAN
433600960729     C                   ENDIF
433700990617     C*
433800050616     C* 88 OFF -  TARIFFA NON IN % verifico se si
433900050616     C*   POSSONO INSERIRE DECIMALI
434000990617     C  N88VIDLIR        IFGT      0
434100990617     C     VIDPEI        ORGT      0
434200990617     C                   MOVE      VIDALI        W0030
434300991008     C                   Z-ADD     VIDALI        W0010
434400990617     C     W0030         IFGT      0
434500050616     C*  la DIVISA NON AMMETTE DECIMALI
434600991008     C     §CVFDC        ANDNE     'S'
434700990617     C                   MOVEL     MSG(79)       V6CMSG
434800990617     C                   SETON                                        8628
434900990617     C                   GOTO      ENDMAN
435000990617     C                   ENDIF
435100991008     C* ULTIMA CIFRA DELL'ARROTONDAMENTO DEVE ESSERE UGUALE A ZERO
435200991008     C* NEL CASO DI MONETA SENZA DECIMALE E' L'ULTIMO INTERO + I DECIMALI
435300991008     C* NEL CASO DI MONETA CON DECIMALI E' L'ULTIMO DECIMALE
435400991008     C     VIDALI        IFNE      *ZEROS
435500991008     C     §CVFDC        IFNE      'S'
435600991008     C* INTERO + DECIMALI
435700991008     C                   MOVE      VIDALI        W0040             4 0
435800991008     C*
435900991008     C     W0040         IFNE      0
436000991008     C                   MOVEL     MSG(83)       V6CMSG
436100991008     C                   SETON                                        8628
436200991008     C                   GOTO      ENDMAN
436300991008     C                   ENDIF
436400991008     C                   ELSE
436500991008     C* DECIMALI
436600991008     C                   MOVE      VIDALI        W0010
436700991008     C*
436800991008     C     W0010         IFNE      0
436900991008     C                   MOVEL     MSG(84)       V6CMSG
437000991008     C                   SETON                                        8628
437100991008     C                   GOTO      ENDMAN
437200991008     C                   ENDIF
437300010323     C*
437400010323     C* SE C'E' ARROTONDAMENTO ANCHE AL DECIMALE  DO ERRORE
437500010323     C*
437600010323     C     VIDSAD        IFEQ      'S'
437700010323     C                   MOVEL     MSG(94)       V6CMSG
437800010323     C                   SETON                                        8628
437900010323     C                   GOTO      ENDMAN
438000010323     C                   ENDIF
438100010323     C*
438200991008     C                   ENDIF
438300991008     C                   ENDIF
438400991008     C*
438500990617     C                   ENDIF
438600960729     C*
438700050616     C   88VIDLIR        IFGT      0
438800960729     C     VIDPEI        ORGT      0
438900990617     C                   MOVE      VIDALI        W0010             1 0
439000990617     C     W0010         IFGT      0
439100990617     C                   MOVEL     MSG(80)       V6CMSG
439200960729     C                   SETON                                        8628
439300960729     C                   GOTO      ENDMAN
439400960729     C                   ENDIF
439500960729     C                   ENDIF
439600960729     C*
439700960523     C* VIDIOP = I  E NON E' STATA INSERITA NESSUNA LINEA PARTENZA
439800960523     C                   XFOOT     VMP           COMVMP            5 0
439900960523     C     VIDIOP        IFEQ      'I'
440000960523     C     COMVMP        ANDEQ     0
440100960523     C                   MOVEL     MSG(60)       V6CMSG
440200960523     C                   SETON                                        9028
440300960523     C                   GOTO      ENDMAN
440400960523     C                   ENDIF
440500960523     C*
440600940526     C* VIDIO = I  E NON E' STATO INSERITO NESSUN CODICE TASSAZIONE
440700930625     C                   MOVEA     VMC           COMVMC           20
440800930623     C     VIDIO         IFEQ      'I'
440900930625     C     COMVMC        ANDEQ     *BLANKS
441000940526     C                   MOVEL     MSG(34)       V6CMSG
441100070309     C                   SETON                                        6628
441200930623     C                   GOTO      ENDMAN
441300930628     C                   ENDIF
441400960522     C*
441500940530     C* VIDIO2 = I  E NON E' STATO INSERITO LO SCAGLIONE
441600930623     C     VIDIO2        IFEQ      'I'
441700940518     C     VIDSGL        ANDEQ     0
441800941123     C                   MOVEL     MSG(35)       V6CMSG
441900940530     C                   SETON                                        6528
442000930623     C                   GOTO      ENDMAN
442100930628     C                   ENDIF
442200110131
442300110131       //?Se tariffa FedEx e Manipolazione rapporto peso volume
442400110131       //?non si può fare nessuna selezione su LNP - CTS - Scaglione
442500110131     c                   IF        V1Cnet = 'F' and (VIDvrr > 0 or
442600110131     c                              VIDarr <> *blanks or VIDirr > 0)
442700110131       //?Inclusione LNP
442800110131     c                   IF        VIDiop = 'I'
442900110131     c                   eval      v6cmsg = msg(118)
443000110131     c                   eval      *in28 = *on
443100110131     c                   z-add     17            H7riga
443200110131     c                   z-add     21            H7colo
443300110131     c                   goto      endman
443400110131     c                   ENDIF
443500110131       //?Omissione LNP
443600110131     c                   IF        comvmp > 0
443700110131     c                   eval      v6cmsg = msg(118)
443800110131     c                   eval      *in28 = *on
443900110131     c                   eval      *in90 = *on
444000110131     c                   goto      endman
444100110131     c                   ENDIF
444200110131       //?Inclusione CTS
444300110131     c                   IF        VIDio  = 'I'
444400110131     c                   eval      v6cmsg = msg(103)
444500110131     c                   eval      *in28 = *on
444600110131     c                   z-add     18            H7riga
444700110131     c                   z-add     21            H7colo
444800110131     c                   goto      endman
444900110131     c                   ENDIF
445000110131       //?Omissione CTS
445100110131     c                   IF        comvmc > *blanks
445200110131     c                   eval      v6cmsg = msg(103)
445300110131     c                   eval      *in28 = *on
445400110131     c                   eval      *in66 = *on
445500110131     c                   goto      endman
445600110131     c                   ENDIF
445700110131       //?Inclusione Scaglione
445800110131     c                   IF        VIDio2 = 'I'
445900110131     c                   eval      v6cmsg = msg(102)
446000110131     c                   eval      *in28 = *on
446100110131     c                   z-add     19            H7riga
446200110131     c                   z-add     21            H7colo
446300110131     c                   goto      endman
446400110131     c                   ENDIF
446500110131       //?Omissione Scaglione
446600110131     c                   IF        VIDsgl > 0
446700110131     c                   eval      v6cmsg = msg(102)
446800110131     c                   eval      *in28 = *on
446900110131     c                   eval      *in65 = *on
447000110131     c                   goto      endman
447100110131     c                   ENDIF
447200110131     c                   ENDIF
447300070309
447400070309      * vidioftc = I  e non è stata inserita nessuna tariffa particolare
447500070309     c                   if        vidtpt = 'SI' and
447600070309     c                             vidioftc = 'I' and comftc = *blanks
447700070309     c                   eval      v6cmsg = msg(38)
447800070309     c                   eval      *in28 = *on
447900070309     c                   eval      *in64 = *on
448000070309     c                   leavesr
448100070309     c                   endif
448200070309      * se immesse delle tariffe particolari e non è stata richiesta la
448300070309      * manipolazione tariffe particolari errore
448400070309     c                   if        vidtpt <> 'SI' and comftc <> *blanks
448500070309     c                   eval      v6cmsg = msg(39)
448600070309     c                   eval      *in28 = *on
448700070309     c                   eval      *in64 = *on
448800070309     c                   leavesr
448900070309     c                   endif
449000930623     C*
449100960523     C****  LINEE PARTENZA  ****
449200960523     C                   DO        5             X
449300960523     C     VMP(X)        IFGT      0
449400960523     C     VMP(X)        CHAIN     AZORG                              34
449500960523     C     *IN34         IFEQ      *ON
449600960523     C     ORGFVA        ORNE      ' '
449700960523     C     ORGFAG        ORNE      'A'
449800960523     C     ORGFAG        ANDNE     'F'
449900020527     c     orgfl2        andne     'S'
450000960523     C                   MOVEA     MSG(61)       K78
450100960523     C                   MOVEL     VMP(X)        ERRLNP            3
450200960523     C                   MOVEA     ERRLNP        K78(19)
450300960523     C                   MOVEA     K78           V6CMSG
450400960523     C                   SETON                                        9028
450500960523     C                   GOTO      ENDMAN
450600960523     C                   ENDIF
450700960523     C                   ENDIF
450800960523     C                   ENDDO
450900960523     C*
451000940526     C****  CODICI TASSAZIONE  ****
451100120713      /free
451200120713       //?Ricerca Codici Tassazione
451300120713         x = 1;
451400120713         FOR x by 1 to 10;
451500120713           IF  %scan ('?': vmc(x)) > *zeros;
451600120713             exsr Ric_CT;
451700120713             vmc(x) = TB09cod;
451800120713             h7riga = 18;
451900120713             h7colo = 28 + (x * 5);
452000120713             wintcts = *on;
452100120713             leavesr;
452200120713           ENDIF;
452300120713         ENDFOR;
452400120713      /end-free
452500960523     C                   DO        10            X                 3 0
452600020611     C                   Z-ADD     1             A
452700900410     C     VMC(X)        IFNE      *BLANKS
452800020207     C     VMC(X)        LOOKUP    CCT(A)                                 35
452900940530     C     *IN35         IFEQ      *OFF
453000941123     C                   MOVEL     MSG(36)       V6CMSG
453100940530     C                   SETON                                        28
453200940530     C                   EXSR      INDIC9
453300940530     C                   GOTO      ENDMAN
453400940530     C                   ENDIF
453500050303      * errore bloccante se codice tassazione non utilizzabile in gestione
453600050303      * tariffe clienti
453700050303     c                   If        uta(a) = 'N'
453800050303     c                   Eval      v6cmsg = msg(36)
453900050303     c                   Eval      *In28 = *On
454000050303     C                   ExSr      indic9
454100050303     c                   Goto      endman
454200050303     c                   EndIf
454300020207      *
454400020207      * Se tariffa FedEx devo utilizzare solo i codici tariffa
454500020207      * previsti per FedEx
454600020207     C     V1CNET        IFEQ      'F'
454700020207     C     UTF(A)        ANDNE     'S'
454800020207     C                   SETON                                        28
454900020207     C                   MOVEL     MSG(100)      V6CMSG
455000020207     C                   EXSR      INDIC9
455100020207     C                   GOTO      ENDMAN
455200020207     C                   ENDIF
455300020207      * Se tariffa non è FedEx non posso utilizzare i codici
455400020207      * tariffa previsti per FedEx
455500020207     C     V1CNET        IFNE      'F'
455600020207     C     UTF(A)        ANDEQ     'S'
455700020207     C                   SETON                                        28
455800020207     C                   MOVEL     MSG(101)      V6CMSG
455900020207     C                   EXSR      INDIC9
456000020207     C                   GOTO      ENDMAN
456100020207     C                   ENDIF
456200940530     C                   ENDIF
456300940530     C                   ENDDO
456400960522     C*
456500950118     C****  SCAGLIONE  ****
456600950118     C     VIDSGL        IFGT      0
456700950119     C* WSAP = " " --> SI TRATTA DI SCAGLIONE SENZA DECIMALI QUINDI NON
456800950118     C*                SI POSSONO INSERIRE VALORI DOPO LA VIRGOLA
456900950118     C     WSAP          ANDEQ     ' '
457000950118     C                   MOVE      VIDSGL        W0030
457100950118     C     W0030         IFGT      0
457200950118     C                   SETON                                        6528
457300950118     C                   MOVEL     MSG(46)       V6CMSG
457400950118     C                   GOTO      ENDMAN
457500950118     C                   ENDIF
457600950118     C                   ENDIF
457700070309
457800070309     c****  TARIFFE PARTICOLARI  ****
457900070312      * ricerca
458000070312     c                   eval      x = 1
458100070309     c                   for       x by 1 to 5
458200070312     c                   if        %scan('?':vftc(x)) > *zeros
458300070312     c                   eval      kcod = '1P'
458400070312     c                   call      'TRUL19R'
458500070312     c                   parm                    kcod
458600070312     c                   parm                    ordina
458700070312     c                   parm                    kkey
458800070312     c                   parm                    comando
458900070312     c                   eval      vftc(x) = kkey
459000070312     c                   eval      h7riga = 20
459100070312     c                   eval      h7colo = 30 + (x * 3)
459200070312     c                   eval      wintftc = *on
459300070312     c                   leavesr
459400070312     c                   endif
459500070312     c                   endfor
459600070312
459700070312      * controllo
459800070312     c                   eval      x = 1
459900070312     c                   for       x by 1 to 5
460000070309     c                   if        vftc(x) <> *blanks
460100070309     c                   eval      wtpar = vftc(x)
460200070412      * se tariffa particolare AC BASE o FUEL errore
460300070412      * tanto non le posso manipolare
460400070412     c                   if        wtpar = 'd' or wtpar = 'f'
460500070412     c                   eval      v6cmsg = msg(41)
460600070412     c                   eval      %subst(v6cmsg:25:1) = vftc(x)
460700070412     c                   eval      *in28 = *on
460800070412     c                   eval      h7riga = 20
460900070412     c                   eval      h7colo = 30 + (x * 3)
461000070412     c                   leavesr
461100070412     c                   endif
461200070309     c  n50ktam7         chain(n)  titpt01l
461300070309     c   50ktam7         chain(n)  tiopt01l
461400070309     c                   if        not %found or tptatb <> *blanks
461500070309     c                   eval      v6cmsg = msg(40)
461600070309     c                   eval      %subst(v6cmsg:22:1) = vftc(x)
461700070309     c                   eval      *in28 = *on
461800070312     c                   eval      h7riga = 20
461900070312     c                   eval      h7colo = 30 + (x * 3)
462000070309     c                   leavesr
462100070309     c                   endif
462200070309     c                   endif
462300070309     c                   endfor
462400950118     C*
462500941124     C****  VARIAZIONE TARIFFA CITTA'/ PROVINCIA / MINIMO / MASSIMO ***
462600900410     C     VIDVTP        IFNE      'S'
462700941124     C     VIDVTC        ANDNE     'S'
462800941124     C     VIDVMN        ANDNE     'S'
462900941124     C     VIDVMX        ANDNE     'S'
463000941123     C                   MOVEL     MSG(37)       V6CMSG
463100940530     C                   SETON                                        7628
463200900410     C                   GOTO      ENDMAN
463300900410     C                   END
463400900410     C*
463500900410     C     ENDMAN        ENDSR
463600900410     C*
463700930623     C*--- ACCENDO INDICATORI ERRORE CODICI TASSAZIONE ---------------*
463800900410     C     INDIC9        BEGSR
463900940530     C     X             COMP      1                                      66
464000940530     C     X             COMP      2                                      67
464100940530     C     X             COMP      3                                      68
464200940530     C     X             COMP      4                                      69
464300940530     C     X             COMP      5                                      70
464400940530     C     X             COMP      6                                      71
464500940530     C     X             COMP      7                                      72
464600940530     C     X             COMP      8                                      73
464700940530     C     X             COMP      9                                      74
464800940530     C     X             COMP      10                                     75
464900900410     C                   ENDSR
465000900410     C*
465100930623     C*--- ESEGUO MANIPOLAZIONE --------------------------------------*
465200900410     C     MANIPO        BEGSR
465300020327
465400020313     C                   eval      wnoagg = *off
465500020313     c                   eval      *in32 = *off
465600020312
465700960528     C                   CLEAR                   WSLNP
465800960528     C                   CLEAR                   WSCTS
465900960528     C                   CLEAR                   WSSGL
466000020313
466100960528     C* WSLNP = "S" --> RICHIESTA ESCLUSIONE DI ALMENO UNA LINEA PARTENZA
466200960528    1C     VIDIOP        IFEQ      'O'
466300960528     C     COMVMP        ANDGT     0
466400960528     C                   MOVEL     'S'           WSLNP             1
466500960528    1C                   ENDIF
466600960528     C* WSCTS = "S" --> RICHIESTA ESCLUSIONE DI ALMENO UN COD.TASSAZIONE
466700960528    1C     VIDIO         IFEQ      'O'
466800960528     C     COMVMC        ANDNE     *BLANKS
466900960528     C                   MOVEL     'S'           WSCTS             1
467000960528    1C                   ENDIF
467100960528     C* WSSGL = "S" --> RICHIESTA ESCLUSIONE DI UNO SCAGLIONE          E
467200960528    1C     VIDIO2        IFEQ      'O'
467300960528     C     VIDSGL        ANDGT     0
467400960528     C                   MOVEL     'S'           WSSGL             1
467500960528    1C                   ENDIF
467600070309      * wsftc = "S" --> richiesta sclusione di almeno una tariffa particolare
467700070309     c                   if        vidioftc = 'O' and comftc <> *blanks
467800070309     c                   eval      wsftc = 'S'
467900070309     c                   endif
468000960528     C*
468100020314      * ---- DETTAGLIO TARIFFE
468200020314 1ab c                   if        vidtad = 'SI'
468300960528     C*
468400960528     C* C I C L O    L E T T U R A
468500061011     C  n50KTAM2         SETLL     TITAD000
468600061011     c   50ktam2         setll     tiofd000
468700061011     C  n50KTAM2         READE     TITAD000                               32
468800061011     c   50ktam2         reade     tiofd000                               32
468900930623     C*
469000941124    1C     *IN32         DOWEQ     *OFF
469100930623     C* ESCLUDO RECORD ANNULLATI
469200941124    2C     TADATB        IFEQ      ' '
469300960528     C                   CLEAR                   WOKLNP
469400960528     C                   CLEAR                   WOKCTS
469500960528     C                   CLEAR                   WOKSGL
469600930624     C*
469700960528     C****  C O N T R O L L O     L E     I N C L U S I O N I  ****
469800960528     C* CONTROLLO SE LA LINEA PARTENZA E' INCLUDERE
469900960528    3C     COMVMP        IFGT      0
470000960527     C     TADLNP        LOOKUP    VMP                                    35
470100960528    4C     *IN35         IFEQ      *ON
470200960528     C                   MOVEL     'S'           WOKLNP            1
470300960528   X4C                   ELSE
470400960527     C* NON E' INCLUSA
470500960528     C  N35VIDIOP        IFEQ      'I'
470600960528     C                   GOTO      LEGTAD
470700960528     C                   ENDIF
470800960528    4C                   ENDIF
470900960528    3C                   ENDIF
471000960527     C*
471100960528     C* CONTROLLO SE IL CODICE TASSAZIONE E' DA INCLUDERE
471200960528    3C     COMVMC        IFNE      *BLANKS
471300940524     C     TADCTS        LOOKUP    VMC                                    35
471400960528    4C     *IN35         IFEQ      *ON
471500960528     C                   MOVEL     'S'           WOKCTS            1
471600960528   X4C                   ELSE
471700900410     C* NON E' INCLUSO
471800960528     C  N35VIDIO         IFEQ      'I'
471900960528     C                   GOTO      LEGTAD
472000960528     C                   ENDIF
472100960528    4C                   ENDIF
472200960528    3C                   ENDIF
472300960522     C*
472400960528     C* CONTROLLO SE LO SCAGLIONE E' DA INCLUDERE
472500960528    3C     VIDSGL        IFGT      0
472600960528    4C     TADSGL        IFEQ      VIDSGL
472700960528     C                   MOVEL     'S'           WOKSGL            1
472800960528   X4C                   ELSE
472900960528     C* NON E' INCLUSO
473000960528     C     TADSGL        IFNE      VIDSGL
473100960528     C     VIDIO2        ANDEQ     'I'
473200960528     C                   GOTO      LEGTAD
473300960528     C                   ENDIF
473400960528    4C                   ENDIF
473500960528    3C                   ENDIF
473600930624     C*
473700960528     C****  C O N T R O L L O     L E     E S C L U S I O N I  ****
473800960528     C* SE TUTTE LE CONDIZIONI SI VERIFICANO AGGIORNO IL RECORD
473900960528   3AC     WSLNP         IFEQ      'S'
474000960528     C     WOKLNP        ANDEQ     ' '
474100960528     C*
474200960528     C     WSCTS         OREQ      'S'
474300960528     C     WOKCTS        ANDEQ     ' '
474400960528     C*
474500960528     C     WSSGL         OREQ      'S'
474600960528     C     WOKSGL        ANDEQ     ' '
474700960528     C*
474800960528     C     WSLNP         OREQ      ' '
474900960528     C     WSCTS         ANDEQ     ' '
475000960528     C     WSSGL         ANDEQ     ' '
475100960528     C*
475200960528     C****  E L A B O R O    I L    R E C O R D  ****
475300020313
475400020313     c                   eval      w_taditr = taditr
475500020313     c                   eval      w_tadino = tadino
475600020313     c                   eval      w_tadmin = tadmin
475700020313     c                   eval      w_tadmax = tadmax
475800020313     c                   eval      w_tadrpv = tadrpv
475900020313
476000020313      * AUMENTO/DIMINUZUIONE CON IMPORTO IN LIRE
476100020313  4b C                   if        vidlir > *zeros
476200020313
476300020313      * VARIAZIONE TARIFFA CITTA'
476400020313  5b C                   IF        vidvtc = 'S'
476500020313  6b C                   IF        vidad = '+'
476600020313     C                   ADD       VIDLIR        w_TADITR
476700020313  6x C                   ELSE
476800020313     C                   SUB       VIDLIR        w_TADITR
476900020313      * SE MINORE NON AGGIORNO LA RIGA
477000020313  b7 c                   if        w_taditr <= *zeros
477100020313     c                   eval      wnoagg = *on
477200020313     C                   GOTO      LEGTAD
477300020313  e7 c                   endif
477400020313  6e C                   END
477500020313  5e C                   END
477600020313
477700020313      * VARIAZIONE TARIFFA PROVINCIA
477800020313  5b C                   IF        vidvtp = 'S' and
477900020313     c                             w_tadino > *zeros
478000020313  6b C                   IF        vidad = '+'
478100020313     C                   ADD       VIDLIR        w_TADINO
478200020313  6x C                   ELSE
478300020313     C                   SUB       VIDLIR        w_TADINO
478400020313      * se minore non aggiorno la riga
478500020313  7b c                   if        w_tadino <= *zeros
478600020313     c                   eval      wnoagg = *on
478700020313     C                   GOTO      LEGTAD
478800020313  7e c                   endif
478900020313  6e C                   END
479000020313  5e C                   END
479100020313
479200020313      * VARIAZIONE IMPORTO MINIMO  TARIFFA
479300020313  5b C                   IF        vidvmn = 'S' and
479400020313     c                             w_tadmin > *zeros
479500020313  6b C                   IF        vidad = '+'
479600020313     C                   ADD       VIDLIR        w_TADMIN
479700020313  6x C                   ELSE
479800020313     C                   SUB       VIDLIR        W_TADMIN
479900020313      * se minore non aggiorno la riga
480000020313  7b c                   if        w_tadmin <= *zeros
480100020313     c                   eval      wnoagg = *on
480200020313     C                   GOTO      LEGTAD
480300020313  7e c                   endif
480400020313  6e c                   endif
480500020313  5e C                   ENDIF
480600020313
480700020313      * VARIAZIONE IMPORTO MASSIMO TARIFFA
480800020313  5b C                   IF        vidvmx = 'S' and
480900020313     c                             w_tadmax > *zeros
481000020313  6b C                   IF        vidad = '+'
481100020313     C                   ADD       VIDLIR        w_TADMAX
481200020313  6x C                   ELSE
481300020313     C                   SUB       VIDLIR        w_TADMAX
481400020313      * se minore non aggiorno la riga
481500020313  7b c                   if        w_tadmax <= *zeros
481600020313     c                   eval      wnoagg = *on
481700020313     C                   GOTO      LEGTAD
481800020313  7e c                   endif
481900020313  6e C                   END
482000020313  5e C                   ENDIF
482100020313
482200020313  4e c                   endif
482300020313
482400020313      * AUMENTO/DIMINUZUIONE CON IMPORTO IN PERCENTUALE
482500020313  4b C                   if        vidper > *zeros
482600020313
482700020313      * VARIAZIONE TARIFFA CITTA'
482800020313  5b C                   IF        vidvtc ='S'
482900020313     C     w_TADITR      MULT(H)   VIDPER        COMITR
483000020313  6b C                   IF        vidad = '+'
483100020313     C                   ADD       COMITR        w_TADITR
483200020313  6x C                   ELSE
483300020313     C                   SUB       COMITR        w_TADITR
483400020313      * se minore non aggiorno la riga
483500020313  7b c                   if        w_taditr <= *zeros
483600020313     c                   eval      wnoagg = *on
483700020313     C                   GOTO      LEGTAD
483800020313  7e c                   endif
483900020313  6e C                   END
484000020313  5e C                   END
484100020313
484200020313      *  VARIAZIONE TARIFFA PROVINCIA
484300020313  5b C                   IF        vidvtp = 'S' and
484400020313     c                             w_tadino > *zeros
484500020313     C     w_TADINO      MULT(H)   VIDPER        COMINO
484600020313  6b C                   IF        vidad = '+'
484700020313     C                   ADD       COMINO        w_TADINO
484800020313  6x C                   ELSE
484900020313     C                   SUB       COMINO        w_TADINO
485000020313      * se minore non aggiorno la riga
485100020313  7b c                   if        w_tadino <= *zeros
485200020313     c                   eval      wnoagg = *on
485300020313     C                   GOTO      LEGTAD
485400020313  7e c                   endif
485500020313  6e c                   endif
485600020313  5e C                   END
485700020313
485800020313      * VARIAZIONE IMPORTO MINIMO  TARIFFA
485900020313  5b C                   IF        vidvmn = 'S' and
486000020313     c                             w_tadmin > *zeros
486100020313     C     w_TADMIN      MULT(H)   VIDPER        WMIN
486200020313  6b C                   IF        vidad = '+'
486300020313     C                   ADD       WMIN          w_TADMIN
486400020313  6x C                   ELSE
486500020313     C                   SUB       WMIN          w_TADMIN
486600020313      * se minore non aggiorno la riga
486700020313  7b c                   if        w_tadmin <= *zeros
486800020313     c                   eval      wnoagg = *on
486900020313     C                   GOTO      LEGTAD
487000020313  7e c                   endif
487100020313  6e c                   endif
487200020313  5e C                   ENDIF
487300020313
487400020313      * VARIAZIONE IMPORTO MASSIMO TARIFFA
487500020313  5b C                   IF        vidvmx = 'S' and
487600020313     c                             w_tadmax > *zeros
487700020313     C     w_TADMAX      MULT(H)   VIDPER        WMAX
487800020313  6b C                   IF        vidad = '+'
487900020313     C                   ADD       WMAX          w_TADMAX
488000020313  6x C                   ELSE
488100020313     C                   SUB       WMAX          w_TADMAX
488200020313      * se minore non aggiorno la riga
488300020313  7b c                   if        w_tadmax <= *zeros
488400020313     c                   eval      wnoagg = *on
488500020313     C                   GOTO      LEGTAD
488600020313  7e c                   endif
488700020313  6e c                   endif
488800020313  5e C                   ENDIF
488900020313
489000020313  4e c                   endif
489100020313
489200020313      * CONTROLLO CHE IL MASSIMO NON SIA INFERIORE DEL MINIMO
489300020313    3C                   IF        w_tadmax > 0 and
489400020313     C                             w_tadmax < w_tadmin
489500020313     C                   eval      wnoagg = *on
489600950120     C                   GOTO      LEGTAD
489700950120    3C                   ENDIF
489800020313
489900020313      * ARROTONDAMENTO
490000960607   3AC     VIDLIR        IFGT      0
490100960607     C     VIDPEI        ORGT      0
490200010323     C*
490300010323     C* ESEGUO GLI ARROTONDAMENTI SOLO SE VIENE RICHIESTO
490400010323     C*
490500010323   3BC     VIDALI        IFGT      0
490600010323     C*
490700900410     C                   Z-ADD     VIDALI        XARROT
490800900410     C                   MOVE      VIDADE        XDE
490900941124     C* TARIFFA CITTA'
491000941124    3C     VIDVTC        IFEQ      'S'
491100020313     C                   Z-ADD     w_TADITR      XVALUE
491200900410     C                   EXSR      XARROX
491300020313     C                   Z-ADD     XVALUE        w_TADITR
491400941124    3C                   END
491500941124     C* TARIFFA PROVINCIA
491600020313    3C                   IF        vidvtp = 'S' and w_tadino > *zeros
491700020313     C                   Z-ADD     w_TADINO      XVALUE
491800900410     C                   EXSR      XARROX
491900020313     C                   Z-ADD     XVALUE        w_TADINO
492000941124    3C                   END
492100941124     C* IMPORTO MINIMO  TARIFFA
492200020313    3C                   IF        vidvmn = 'S' and w_tadmin > *zeros
492300020313     C                   Z-ADD     w_TADMIN      XVALUE
492400941124     C                   EXSR      XARROX
492500020313     C                   Z-ADD     XVALUE        w_TADMIN
492600941124    3C                   END
492700941124     C* IMPORTO MASSIMO TARIFFA
492800020313    3C                   IF        vidvmx = 'S' and w_tadmax > *zeros
492900020313     C                   Z-ADD     w_TADMAX      XVALUE
493000941124     C                   EXSR      XARROX
493100020313     C                   Z-ADD     XVALUE        w_TADMAX
493200941124    3C                   END
493300010323     C*
493400010323  X3BC                   ELSE
493500010323     C* VERIFICO SE E' STATO RICHIESTO ARROTONDAMENTO AI DECIMALI
493600010323   3CC     VIDSAD        IFEQ      'S'
493700010323     C* TARIFFA CITTA'
493800010323    4C     VIDVTC        IFEQ      'S'
493900020313     C                   Z-ADD     w_TADITR      XVALDE           15 3
494000010323     C                   EXSR      XARROD
494100020313     C                   Z-ADD     XVALDE        w_TADITR
494200010323    4C                   END
494300010323     C* TARIFFA PROVINCIA
494400020313    4C                   IF        vidvtp = 'S' and w_tadino > *zeros
494500020313     C                   Z-ADD     w_TADINO      XVALDE
494600010323     C                   EXSR      XARROD
494700020313     C                   Z-ADD     XVALDE        w_TADINO
494800010323    4C                   END
494900010323     C* IMPORTO MINIMO  TARIFFA
495000020313    4C                   IF        vidvmn = 'S' and w_tadmin > *zeros
495100020313     C                   Z-ADD     w_TADMIN      XVALDE
495200010323     C                   EXSR      XARROD
495300020313     C                   Z-ADD     XVALDE        w_TADMIN
495400010323    4C                   END
495500010323     C* IMPORTO MASSIMO TARIFFA
495600020313    4C                   IF        vidvmx = 'S' and w_tadmax > *zeros
495700020313     C                   Z-ADD     w_TADMAX      XVALDE
495800010323     C                   EXSR      XARROD
495900020313     C                   Z-ADD     XVALDE        w_TADMAX
496000010323    4C                   END
496100010323     C*
496200010323   3CC                   END
496300010323     C*
496400010323   3DC                   ENDIF
496500010323     C*
496600960607   3AC                   ENDIF
496700020313
496800020313      * RAPPORTO PESO VOLUME
496900020315     c                   if        vidvrr > 0  and  w_tadrpv > 0
497000941124    3C     VIDRAD        IFEQ      '+'
497100020313     C                   ADD       VIDVRR        w_TADRPV
497200941124   X3C                   ELSE
497300020313     C                   SUB       VIDVRR        w_TADRPV
497400020313      * se minore non aggiorno la riga
497500020313     c                   if        w_tadrpv <= *zeros
497600020313     c                   eval      wnoagg = *on
497700020313     c                   goto      legtad
497800020315     c                   endif
497900941124    3C                   END
498000020313     c                   endif
498100050616      * annullo il rapporto peso volume
498200050617     c                   If        vidarr = 'SI' and w_tadrpv > 0
498300050617     c                   Clear                   w_tadrpv
498400050616     c                   EndIf
498500061207      * se inserimento imposto il rapporto peso volume
498600061207     c                   if        vidirr > 0
498700061207     c                   z-add     vidirr        w_tadrpv
498800061207     c                   endif
498900941124     C*
499000980219     C* VALORE POSITIVO NEI CAMPI
499100020313     C                   MLLZO     1             w_TADITR
499200020313     C                   MLLZO     1             w_TADINO
499300020313     C                   MLLZO     1             w_TADRPV
499400020313     C                   MLLZO     1             w_TADMIN
499500020313     C                   MLLZO     1             w_TADMAX
499600020312
499700020312      * controllo se i campi aggiornati superano il valore massimo
499800020313     c                   if        w_taditr > 99999999,999 or
499900020313     c                             w_tadino > 99999999,999 or
500000020313     c                             w_tadmin > 99999999,999 or
500100020313     c                             w_tadmax > 99999999,999 or
500200020313     c                             w_tadrpv > 99,9
500300020313     c                   eval      wnoagg = *on
500400020312     c                   goto      legtad
500500020312     c                   endif
500600980219     C*
500700900410     C* AGGIORNO DETTAGLIO
500800020312     c                   if        wsimula = '1'
500900020313
501000020313     c                   eval      taditr = w_taditr
501100020313     c                   eval      tadino = w_tadino
501200020313     c                   eval      tadmin = w_tadmin
501300020313     c                   eval      tadmax = w_tadmax
501400020313     c                   eval      tadrpv = w_tadrpv
501500020313
501600130124     C******             Z-ADD     dateu8        TADDTR
501700940511     C                   MOVEL     *BLANKS       TADFTR
501800061011     C  n50              UPDATE    TITAD000
501900061011     c   50              update    tiofd000
502000030714      * valorizzo il flag per la scrittura dello storico variazioni
502100030714     c                   eval      mandet = 'S'
502200030714      *
502300020312     c                   endif
502400900410     C     LEGTAD        TAG
502500960528   3AC                   ENDIF
502600941124    2C                   END
502700930623     C*
502800061011     C  n50KTAM2         READE     TITAD000                               32
502900061011     c   50ktam2         reade     tiofd000                               32
503000020312      * se errore esco dalla lettura
503100020313     c                   if        wnoagg = *on
503200020312     c                   eval      *in32 = *on
503300020312     c                   endif
503400020312
503500941124    1C                   END
503600020314
503700020314 1ae c                   endif
503800020312
503900020312      * Manipolazione tariffe particolari
504000020312     c                   if        vidtpt = 'SI'
504100020312     c                   exsr      manipo_tpt
504200020312     c                   endif
504300050616
504400050616      * Manipolazione tariffe di giacenza
504500050616     c                   if        vidtgc = 'SI'
504600050616     c                   exsr      manipo_tgc
504700050616     c                   endif
504800020312
504900020312     c                   if        wsimula = '1'
505000020312
505100930623     C*
505200900410     C* AGGIORNO DATA ULTIMA VARIAZIONE MASTER
505300061011     C  n50KTAM2         CHAIN     TNTAM000                           33
505400061011     c   50ktam2         chain     tnofm000                           33
505500990608     C*-------------------------------------------------------------------------
505600941221     C  N33              Z-ADD     DATEU8        TAMDUV
505700040913     C  n33              Z-ADD     dateu8        TAMDTR
505800990608     C*-------------------------------------------------------------------------
505900061011     C  N33
506000061011     Cann50              UPDATE    TNTAM000
506100061011     c  n33
506200061011     can 50              update    tnofm000
506300020312     c                   endif
506400930623     C                   ENDSR
506500020312
506600020312      *---------------------------------------------------------------*
506700020312      * Manipolazione tariffe particolari
506800020312      *---------------------------------------------------------------*
506900020312     c     manipo_tpt    begsr
507000020312
507100020313     c                   eval      wnoagg = *off
507200020312
507300020312      * Testata tariffe particolari
507400061011     c  n50ktam2         setll     titpt01l
507500061011     c   50ktam2         setll     tiopt01l
507600020312  b1 c                   do        *hival
507700061011     c  n50ktam2         reade     titpt01l
507800061011     c   50ktam2         reade     tiopt01l
507900020312      * fine lettura
508000061011     c                   if        %eof
508100020312     c                   leave
508200020312     c                   endif
508300020312      * escludo record annullati
508400020312     c                   if        tptatb <> *blanks
508500020312     c                   iter
508600020312     c                   endif
508700070410      * escludo AC BASE e FUEL (non si possono manipolare)
508800070410     c                   if        tptftc = 'd ' or tptftc = 'f '
508900070410     c                   iter
509000070410     c                   endif
509100070309
509200070309     c                   clear                   wokftc
509300070309
509400070309      ****  C O N T R O L L O     L E     I N C L U S I O N I  ****
509500070309      * controllo se la tariffa particolare è da includere
509600070309     c                   if        comftc <> *blanks
509700070309     c                   eval      wtptftc = tptftc
509800070309     c     wtptftc       lookup    vftc                                   35
509900070309     c                   if        *in35
510000070309     c                   eval      wokftc = 'S'
510100070309     c                   else
510200070309      * non è inlcusa
510300070309     c                   if        not *in35 and vidioftc = 'I'
510400070309     c                   iter
510500070309     c                   endif
510600070309     c                   endif
510700070309     c                   endif
510800070309      ****  C O N T R O L L O     L E     E S C L U S I O N I  ****
510900070309   1ac                   if        wsftc = 'S' and wokftc = *blanks
511000070309     c                             or wsftc = *blanks
511100070309
511200050628      * aggancio la tabella solo x controllare se la tariffa gestisce il
511300050628      * rapporto peso volume, non controllo + se gestita a valore
511400050628     c                   clear                   key
511500050628     c                   eval      cod = 'TR'
511600050628     c                   eval      key = tpttpg
511700050628     c     ktab          chain     tabel00f
511800050628     c                   if        %found (tabel00f)
511900050628     c                   movel     tbluni        dstr
512000050628     c                   endif
512100020312
512200020312      * Dettaglio tariffe particolari
512300061011     c  n50ktpd          setll     titpd01l
512400061011     c   50ktpd          setll     tiopd01l
512500020312  b2 c                   do        *hival
512600061011     c  n50ktpd          reade     titpd01l
512700061011     c   50ktpd          reade     tiopd01l
512800020312      * fine lettura
512900061011     c                   if        %eof
513000020312     c                   leave
513100020312     c                   endif
513200020312      * escludo record annullati
513300020312     c                   if        tpdatb <> *blanks
513400020312     c                   iter
513500020312     c                   endif
513600020314
513700020314      * ---- Controllo le inclusioni
513800020314     c                   clear                   wokcts
513900020314     c                   clear                   woksgl
514000020312
514100020312      * codice tassazione da includere
514200020312  b3 c                   if        comvmc <> *blanks
514300020312     c     tpdcts        lookup    vmc                                    35
514400020312  b4 c                   if        *in35
514500020312     c                   eval      wokcts = 'S'
514600020312  x4 c                   else
514700020312      * non è incluso
514800020312  b5 c                   if        vidio = 'I'
514900020312     c                   iter
515000020312  e5 c                   endif
515100020312  e4 c                   endif
515200020312  e3 c                   endif
515300020312      * scaglione da includere
515400020312  b3 c                   if        vidsgl > *zeros
515500020312  b4 c                   if        tpdsgl = vidsgl
515600020312     c                   eval      woksgl = 'S'
515700020312  x4 c                   else
515800020312      * non è incluso
515900020312  b5 c                   if        tpdsgl <> vidsgl and
516000020312     c                             vidio2 = 'I'
516100020312     c                   iter
516200020312  e5 c                   endif
516300020312  e4 c                   endif
516400020312  e3 c                   endif
516500020312      * ---- Controllo le esclusioni
516600020312  b3 c                   if        (wscts = 'S' and wokcts = *blanks) or
516700020312     c                             (wssgl = 'S' and woksgl = *blanks) or
516800020312     c                             (wscts = *blanks and wssgl = *blanks)
516900020312      * ---- Elabora tariffa particolare letta
517000020313
517100020313     c                   eval      w_tpditr = tpditr
517200020313     c                   eval      w_tpdmin = tpdmin
517300020313     c                   eval      w_tpdmax = tpdmax
517400020313
517500020312      * ---- Aumento/Diminuzione con importo
517600020312  b4 c                   if        vidlir > 0
517700020313
517800020312      * Variazione Tariffa
517900020315  b5 c                   if        vidvtc = 'S' and w_tpditr > *zeros
518000020312      * aumento
518100020312  b6 c                   if        vidad ='+'
518200020313     c                   add       vidlir        w_tpditr
518300020312  x6 c                   else
518400020312      * diminuisco
518500020327     c                   sub       vidlir        w_tpditr
518600020313      * se minore non aggiorno
518700020313  b7 c                   if        w_tpditr <= *zeros
518800020313     c                   eval      wnoagg = *on
518900020312     c                   leave
519000020313  e7 c                   endif
519100020312  e6 c                   endif
519200020312  e5 c                   endif
519300020312      * Variazione Tariffa minima
519400020313  b5 c                   if        vidvmn = 'S' and w_tpdmin > 0
519500020312      * aumento
519600020313  b6 c                   if        vidad ='+'
519700020313     c                   add       vidlir        w_tpdmin
519800020312  x6 c                   else
519900020312      * diminuisco
520000020313     c                   sub       vidlir        w_tpdmin
520100020313      * se minore non aggiorno
520200020313  b7 c                   if        w_tpdmin <= *zeros
520300020313     c                   eval      wnoagg = *on
520400020312     c                   leave
520500020313  e7 c                   endif
520600020312  e6 c                   endif
520700020312  e5 c                   endif
520800020312      * Variazione Tariffa massima
520900020313  b5 c                   if        vidvmx = 'S' and w_tpdmax > *zeros
521000020312      * aumento
521100020312  b6 c                   if        vidad ='+'
521200020313     c                   add       vidlir        w_tpdmax
521300020312  x6 c                   else
521400020312      * diminuisco
521500020313     c                   sub       vidlir        w_tpdmax
521600020313      * se minore non aggiorno
521700020313  b7 c                   if        w_tpdmax <= *zeros
521800020313     c                   eval      wnoagg = *on
521900020312     c                   leave
522000020312  e7 c                   endif
522100020312  e6 c                   endif
522200020312  e5 c                   endif
522300020312
522400020313  e4 c                   endif
522500020313
522600020312      * ---- Aumento/Diminuzione in percentuale
522700020313  4b c                   if        vidper > *zeros
522800020313
522900020312      * Variazione Tariffa
523000020315  b5 c                   if        vidvtc = 'S' and w_tpditr > *zeros
523100020313     c                   eval(h)   w_itr = w_tpditr * vidper
523200020312      * aumento
523300020312  b6 c                   if        vidad ='+'
523400020313     c                   add       w_itr         w_tpditr
523500020312  x6 c                   else
523600020312      * diminuisco
523700020313     c                   sub       w_itr         w_tpditr
523800020313      * se minore non aggiorno
523900020313  b7 c                   if        w_tpditr <= *zeros
524000020313     c                   eval      wnoagg = *on
524100020312     c                   leave
524200020312  e7 c                   endif
524300020312  e6 c                   endif
524400020312  e5 c                   endif
524500020312      * Variazione Tariffa minima
524600020313  b5 c                   if        vidvmn = 'S' and w_tpdmin > *zeros
524700020313     c                   eval(h)   w_min = w_tpdmin * vidper
524800020312      * aumento
524900020313  b6 c                   if        vidad ='+'
525000020313     c                   add       w_min         w_tpdmin
525100020313  x6 c                   else
525200020312      * diminuisco
525300020313     c                   sub       w_min         w_tpdmin
525400020313      * se minore non aggiorno
525500020313  b7 c                   if        w_tpdmin <= *zeros
525600020313     c                   eval      wnoagg = *on
525700020312     c                   leave
525800020312  e7 c                   endif
525900020312  e6 c                   endif
526000020312  e5 c                   endif
526100020312      * Variazione Tariffa massima
526200020313  b5 c                   if        vidvmx = 'S' and w_tpdmax > *zeros
526300020313     c                   eval(h)   w_max = w_tpdmax * vidper
526400020312      * aumento
526500020313  b6 c                   if        vidad ='+'
526600020313     c                   add       w_max         w_tpdmax
526700020313  x6 c                   else
526800020312      * diminuisco
526900020313     c                   sub       w_max         w_tpdmax
527000020313      * se minore non aggiorno
527100020313  b7 c                   if        w_tpdmax <= *zeros
527200020313     c                   eval      wnoagg = *on
527300020312     c                   leave
527400020312  e7 c                   endif
527500020312  e6 c                   endif
527600020312  e5 c                   endif
527700020312
527800020312  e4 c                   endif
527900020312
528000020312      * Controllo che il massimo non sia inferiore del minimo
528100020313  b4 c                   if        w_tpdmax > *zeros and w_tpdmax < tpdmin
528200020313     c                   eval      wnoagg = *on
528300020312     c                   leave
528400020312  e4 c                   endif
528500020312
528600020312      * ---- Arrotondamenti
528700020312  b4 c                   if        vidlir > *zeros or vidpei > *zeros
528800020312      * Arrotondamento
528900020312  b5 c                   if        vidali > *zeros
529000020312     c                   z-add     vidali        xarrot
529100020312     c                   move      vidade        xde
529200020312      * Tariffa
529300020327  b6 c                   if        vidvtc = 'S' and w_tpditr > *zeros
529400020313     c                   z-add     w_tpditr      xvalue
529500020312     c                   exsr      xarrox
529600020313     c                   z-add     xvalue        w_tpditr
529700020312  e6 c                   endif
529800020312      * Tariffa minima
529900020313  b6 c                   if        vidvmn = 'S' and w_tpdmin > *zeros
530000020313     c                   z-add     w_tpdmin      xvalue
530100020312     c                   exsr      xarrox
530200020313     c                   z-add     xvalue        w_tpdmin
530300020312  e6 c                   endif
530400020312      * Tariffa massima
530500020313  b6 c                   if        vidvmx = 'S' and w_tpdmax > *zeros
530600020313     c                   z-add     w_tpdmax      xvalue
530700020312     c                   exsr      xarrox
530800020313     c                   z-add     xvalue        w_tpdmax
530900020312  e6 c                   endif
531000020312  e5 c                   endif
531100020312      * Arrotondamento ai decimali
531200020312  b5 c                   if        vidsad = 'S'
531300020312      * Tariffa
531400020327  b6 c                   if        vidvtc = 'S' and w_tpditr > *zeros
531500020313     c                   z-add     w_tpditr      xvalde
531600020312     c                   exsr      xarrod
531700020313     c                   z-add     xvalde        w_tpditr
531800020312  e6 c                   endif
531900020312      * Tariffa minima
532000020313  b6 c                   if        vidvmn = 'S' and w_tpdmin > *zeros
532100020313     c                   z-add     w_tpdmin      xvalde
532200050628     c                   exsr      xarrod
532300020313     c                   z-add     xvalde        w_tpdmin
532400020312  e6 c                   endif
532500020312      * Tariffa massima
532600020313  b6 c                   if        vidvmx = 'S' and w_tpdmax > *zeros
532700020313     c                   z-add     w_tpdmax      xvalde
532800050628     c                   exsr      xarrod
532900020313     c                   z-add     xvalde        w_tpdmax
533000020312  e6 c                   endif
533100020312  e5 c                   endif
533200020312  e4 c                   endif
533300020312
533400020312      * valori positivi nei campi
533500020313     c                   mllzo     1             w_tpditr
533600020313     c                   mllzo     1             w_tpdmin
533700020313     c                   mllzo     1             w_tpdmax
533800020312
533900020312      * controllo se i campi aggiornati superano il valore massimo
534000020313     c                   if        w_tpditr > 99999999,999 or
534100020313     c                             w_tpdmin > 99999999,999 or
534200020313     c                             w_tpdmax > 99999999,999
534300020313     c                   eval      wnoagg = *on
534400020312     c                   leave
534500020312     c                   endif
534600020312
534700020312      * aggiorno dettaglio tariffe particolari
534800020312     c                   if        wsimula = '1'
534900020313
535000020313     c                   eval      tpditr = w_tpditr
535100020313     c                   eval      tpdmin = w_tpdmin
535200020313     c                   eval      tpdmax = w_tpdmax
535300020313
535400040914     c                   z-add     dateu8        tpddtr
535500020312     c                   clear                   tpdftr
535600061011     c  n50              update    titpd000
535700061011     c   50              update    tiopd000
535800020312     c                   endif
535900020312
536000020312  e3 c                   endif
536100020312
536200020312  e2 c                   enddo
536300020312
536400020313     c                   if        wnoagg = *on
536500020312     c                   leave
536600020312     c                   endif
536700020312
536800020313     c                   eval      w_tptrpv = tptrpv
536900020313
537000020312      * ---- Rapporto peso volume
537100020313  b2 c                   if        vidvrr > *zeros and §trrpv = 'S'
537200020315     c                             and w_tptrpv > 0
537300020312  b3 c                   if        vidrad = '+'
537400020313     c                   add       vidvrr        w_tptrpv
537500020312  x3 c                   else
537600020313     c                   sub       vidvrr        w_tptrpv
537700020313      * se minore non aggiorno
537800020315  b4 c                   if        w_tptrpv <= *zeros
537900020313     c                   eval      wnoagg = *on
538000020312     c                   leave
538100020315  e4 c                   endif
538200020312  e3 c                   endif
538300020312  e2 c                   endif
538400050616      * annullo i rapporto peso volume
538500050616     c                   If        vidarr = 'SI' and §trrpv = 'S'
538600050616     c                             and w_tptrpv > *Zeros
538700050616     c                   Clear                   w_tptrpv
538800050616     c                   EndIf
538900061207      * se inserimento imposto il rapporto peso volume
539000061207     c                   if        vidirr > 0 and §trrpv = 'S'
539100061207     c                   z-add     vidirr        w_tptrpv
539200061207     c                   endif
539300020312
539400020313     c                   mllzo     1             w_tptrpv
539500020312      * controllo se i campi aggiornati superano il valore massimo
539600020313     c                   if        w_tptrpv > 99,9
539700020313     c                   eval      wnoagg = *on
539800020312     c                   leave
539900020312     c                   endif
540000020312
540100020312      * aggiorno testata tariffe particolari
540200020312     c                   if        wsimula = '1'
540300020313
540400020313     c                   eval      tptrpv = w_tptrpv
540500020313
540600040914     c                   z-add     dateu8        tptdtr
540700020312     c                   clear                   tptftr
540800020312     c                   z-add     dateu8        tptduv
540900061011     c  n50              update    titpt000
541000061011     c   50              update    tiopt000
541100030714      * verifico se devo caricare la schiera per la scrittura dello storico
541200030714      * variazioni
541300030714     c                   movel(p)  tptftc        w_manftc
541400030714     c                   move      'M'           W_manftc
541500030714     c                   z-add     1             WW
541600130124     c     w_manftc      lookup    manftc                                 35
541700030714     c                   if        not %found
541800130124     c     '   '         lookup    manftc(ww)                             35
541900030714     c                   if        %found
542000030714     c                   movea     w_manftc      manftc(ww)
542100030714     c                   endif
542200030714     c                   endif
542300030714      *
542400020312     c                   endif
542500020312
542600070309   1ac                   endif
542700020312  e1 c                   enddo
542800020312
542900020312     c                   endsr
543000050616
543100050616      *---------------------------------------------------------------*
543200050616      * Manipolazione tariffe di giacenza
543300050616      *---------------------------------------------------------------*
543400050616     c     manipo_tgc    begsr
543500050616
543600050616     c                   eval      wnoagg = *off
543700050616
543800050616      * Tariffe di giacenza
543900050913     c  n50ktam2         Chain     Titgc01l
544000050913     c   50ktam2         Chain     Tiogc01l
544100050913if  1c                   If        %found and tgcatb = *Blanks
544200050616      * ---- Elabora tariffa di giacenza letta
544300050616     c                   eval      w_tgcsgv = tgcsgv
544400050616     c                   eval      w_tgcsgr = tgcsgr
544500050616     c                   eval      w_tgcsgp = tgcsgp
544600050616     c                   eval      w_tgcsgd = tgcsgd
544700050616     c                   eval      w_tgcst1 = tgcst1
544800050616     c                   eval      w_tgcst2 = tgcst2
544900050616     c                   eval      w_tgcst3 = tgcst3
545000050616     c                   eval      w_tgcstm = tgcstm
545100050616     c                   eval      w_tgcrpv = tgcrpv
545200050616
545300050616      * ---- Aumento/Diminuzione con importo
545400050617if  2c                   if        vidlir > 0
545500050616
545600050617      * Variazione spese varie giacenza
545700050617if  3c                   if        w_tgcsgv > *Zeros
545800050616      * aumento
545900050617     c                   if        vidad ='+'
546000050617     c                   add       vidlir        w_tgcsgv
546100050617     c                   else
546200050616      * diminuisco
546300050617     c                   sub       vidlir        w_tgcsgv
546400050616      * se minore non aggiorno
546500050617     c                   if        w_tgcsgv <= *zeros
546600050616     c                   eval      wnoagg = *on
546700050617     c                   leavesr
546800050617     c                   endif
546900050617     c                   endif
547000050617    3c                   endif
547100050617      * Variazione spese riconsegna città
547200050617if  3c                   if        w_tgcsgr > *Zeros
547300050616      * aumento
547400050617     c                   if        vidad ='+'
547500050617     c                   add       vidlir        w_tgcsgr
547600050617     c                   else
547700050616      * diminuisco
547800050617     c                   sub       vidlir        w_tgcsgr
547900050616      * se minore non aggiorno
548000050617     c                   if        w_tgcsgr <= *zeros
548100050616     c                   eval      wnoagg = *on
548200050617     c                   leavesr
548300050617     c                   endif
548400050617     c                   endif
548500050617    3c                   endif
548600050617      * Variazione spese riconsegna provincia
548700050617if  3c                   if        w_tgcsgp > *Zeros
548800050616      * aumento
548900050617     c                   if        vidad ='+'
549000050617     c                   add       vidlir        w_tgcsgp
549100050617     c                   else
549200050616      * diminuisco
549300050617     c                   sub       vidlir        w_tgcsgp
549400050616      * se minore non aggiorno
549500050617     c                   if        w_tgcsgp <= *zeros
549600050616     c                   eval      wnoagg = *on
549700050617     c                   leavesr
549800050617     c                   endif
549900050617     c                   endif
550000050617    3c                   endif
550100050617      * Variazione spese dossier giacenza
550200050617if  3c                   if        w_tgcsgd > *Zeros
550300050617      * aumento
550400050617     c                   if        vidad ='+'
550500050617     c                   add       vidlir        w_tgcsgd
550600050617     c                   else
550700050617      * diminuisco
550800050617     c                   sub       vidlir        w_tgcsgd
550900050617      * se minore non aggiorno
551000050617     c                   if        w_tgcsgd <= *zeros
551100050617     c                   eval      wnoagg = *on
551200050617     c                   leavesr
551300050617     c                   endif
551400050617     c                   endif
551500050617    3c                   endif
551600050617      * Variazione 1° scaglione tariffa sosta
551700050617if  3c                   if        w_tgcst1 > *Zeros
551800050617      * aumento
551900050617     c                   if        vidad ='+'
552000050617     c                   add       vidlir        w_tgcst1
552100050617     c                   else
552200050617      * diminuisco
552300050617     c                   sub       vidlir        w_tgcst1
552400050617      * se minore non aggiorno
552500050617     c                   if        w_tgcst1 <= *zeros
552600050617     c                   eval      wnoagg = *on
552700050617     c                   leavesr
552800050617     c                   endif
552900050617     c                   endif
553000050617    3c                   endif
553100050617      * Variazione 2° scaglione tariffa sosta
553200050617if  3c                   if        w_tgcst2 > *Zeros
553300050617      * aumento
553400050617     c                   if        vidad ='+'
553500050617     c                   add       vidlir        w_tgcst2
553600050617     c                   else
553700050617      * diminuisco
553800050617     c                   sub       vidlir        w_tgcst2
553900050617      * se minore non aggiorno
554000050617     c                   if        w_tgcst2 <= *zeros
554100050617     c                   eval      wnoagg = *on
554200050617     c                   leavesr
554300050617     c                   endif
554400050617     c                   endif
554500050617    3c                   endif
554600050617      * Variazione 3° scaglione tariffa sosta
554700050617if  3c                   if        w_tgcst3 > *Zeros
554800050617      * aumento
554900050617     c                   if        vidad ='+'
555000050617     c                   add       vidlir        w_tgcst3
555100050617     c                   else
555200050617      * diminuisco
555300050617     c                   sub       vidlir        w_tgcst3
555400050617      * se minore non aggiorno
555500050617     c                   if        w_tgcst3 <= *zeros
555600050617     c                   eval      wnoagg = *on
555700050617     c                   leavesr
555800050617     c                   endif
555900050617     c                   endif
556000050617    3c                   endif
556100050617      * Variazione tariffa minima sosta
556200050617if  3c                   if        w_tgcstm > *Zeros
556300050617      * aumento
556400050617     c                   if        vidad ='+'
556500050617     c                   add       vidlir        w_tgcstm
556600050617     c                   else
556700050617      * diminuisco
556800050617     c                   sub       vidlir        w_tgcstm
556900050617      * se minore non aggiorno
557000050617     c                   if        w_tgcstm <= *zeros
557100050617     c                   eval      wnoagg = *on
557200050617     c                   leavesr
557300050617     c                   endif
557400050617     c                   endif
557500050617    3c                   endif
557600050616
557700050617    2c                   endif
557800050616
557900050617      * ---- Aumento/Diminuzione in percentuale
558000050617if  2c                   if        vidper > *zeros
558100050617
558200050617      * Variazione spese varie giacenza
558300050617if  3c                   if        w_tgcsgv > *Zeros
558400050617     c                   eval(h)   w_sgv = w_tgcsgv * vidper
558500050617      * aumento
558600050617     c                   if        vidad ='+'
558700050617     c                   add       w_sgv         w_tgcsgv
558800050617     c                   else
558900050617      * diminuisco
559000050617     c                   sub       w_sgv         w_tgcsgv
559100050617      * se minore non aggiorno
559200050617     c                   if        w_tgcsgv <= *zeros
559300050617     c                   eval      wnoagg = *on
559400050617     c                   leavesr
559500050617     c                   endif
559600050617     c                   endif
559700050617    3c                   endif
559800050617      * Variazione spese riconsegna città
559900050617if  3c                   if        w_tgcsgr > *Zeros
560000050617     c                   eval(h)   w_sgr = w_tgcsgr * vidper
560100050617      * aumento
560200050617     c                   if        vidad ='+'
560300050617     c                   add       w_sgr         w_tgcsgr
560400050617     c                   else
560500050617      * diminuisco
560600050617     c                   sub       w_sgr         w_tgcsgr
560700050617      * se minore non aggiorno
560800050617     c                   if        w_tgcsgr <= *zeros
560900050617     c                   eval      wnoagg = *on
561000050617     c                   leavesr
561100050617     c                   endif
561200050617     c                   endif
561300050617    3c                   endif
561400050617      * Variazione spese riconsegna provincia
561500050617if  3c                   if        w_tgcsgp > *Zeros
561600050617     c                   eval(h)   w_sgp = w_tgcsgp * vidper
561700050617      * aumento
561800050617     c                   if        vidad ='+'
561900050617     c                   add       w_sgp         w_tgcsgp
562000050617     c                   else
562100050617      * diminuisco
562200050617     c                   sub       w_sgp         w_tgcsgp
562300050617      * se minore non aggiorno
562400050617     c                   if        w_tgcsgp <= *zeros
562500050617     c                   eval      wnoagg = *on
562600050617     c                   leavesr
562700050617     c                   endif
562800050617     c                   endif
562900050617    3c                   endif
563000050617      * Variazione spese dossier giacenza
563100050617if  3c                   if        w_tgcsgd > *Zeros
563200050617     c                   eval(h)   w_sgd = w_tgcsgd * vidper
563300050617      * aumento
563400050617     c                   if        vidad ='+'
563500050617     c                   add       w_sgd         w_tgcsgd
563600050617     c                   else
563700050617      * diminuisco
563800050617     c                   sub       w_sgd         w_tgcsgd
563900050617      * se minore non aggiorno
564000050617     c                   if        w_tgcsgd <= *zeros
564100050617     c                   eval      wnoagg = *on
564200050617     c                   leavesr
564300050617     c                   endif
564400050617     c                   endif
564500050617    3c                   endif
564600050617      * Variazione 1° scaglione tariffa sosta
564700050617if  3c                   if        w_tgcst1 > *Zeros
564800050617     c                   eval(h)   w_st1 = w_tgcst1 * vidper
564900050617      * aumento
565000050617     c                   if        vidad ='+'
565100050617     c                   add       w_st1         w_tgcst1
565200050617     c                   else
565300050617      * diminuisco
565400050617     c                   sub       w_st1         w_tgcst1
565500050617      * se minore non aggiorno
565600050617     c                   if        w_tgcst1 <= *zeros
565700050617     c                   eval      wnoagg = *on
565800050617     c                   leavesr
565900050617     c                   endif
566000050617     c                   endif
566100050617    3c                   endif
566200050617      * Variazione 2° scaglione tariffa sosta
566300050617if  3c                   if        w_tgcst2 > *Zeros
566400050617     c                   eval(h)   w_st2 = w_tgcst2 * vidper
566500050617      * aumento
566600050617     c                   if        vidad ='+'
566700050617     c                   add       w_st2         w_tgcst2
566800050617     c                   else
566900050617      * diminuisco
567000050617     c                   sub       w_st2         w_tgcst2
567100050617      * se minore non aggiorno
567200050617     c                   if        w_tgcst2 <= *zeros
567300050617     c                   eval      wnoagg = *on
567400050617     c                   leavesr
567500050617     c                   endif
567600050617     c                   endif
567700050617    3c                   endif
567800050617      * Variazione 3° scaglione tariffa sosta
567900050617if  3c                   if        w_tgcst3 > *Zeros
568000050617     c                   eval(h)   w_st3 = w_tgcst3 * vidper
568100050617      * aumento
568200050617     c                   if        vidad ='+'
568300050617     c                   add       w_st3         w_tgcst3
568400050617     c                   else
568500050617      * diminuisco
568600050617     c                   sub       w_st3         w_tgcst3
568700050617      * se minore non aggiorno
568800050617     c                   if        w_tgcst3 <= *zeros
568900050617     c                   eval      wnoagg = *on
569000050617     c                   leavesr
569100050617     c                   endif
569200050617     c                   endif
569300050617    3c                   endif
569400050617      * Variazione tariffa mimina sosta
569500050617if  3c                   if        w_tgcstm > *Zeros
569600050617     c                   eval(h)   w_stm = w_tgcstm * vidper
569700050617      * aumento
569800050617     c                   if        vidad ='+'
569900050617     c                   add       w_stm         w_tgcstm
570000050617     c                   else
570100050617      * diminuisco
570200050617     c                   sub       w_stm         w_tgcstm
570300050617      * se minore non aggiorno
570400050617     c                   if        w_tgcstm <= *zeros
570500050617     c                   eval      wnoagg = *on
570600050617     c                   leavesr
570700050617     c                   endif
570800050617     c                   endif
570900050617    3c                   endif
571000050617
571100050617    2c                   endif
571200050617
571300050617      * ---- Arrotondamenti
571400050617if  2c                   if        vidlir > *zeros or vidpei > *zeros
571500050617      * Arrotondamento
571600050617if  3c                   if        vidali > *zeros
571700050617     c                   z-add     vidali        xarrot
571800050617     c                   move      vidade        xde
571900050617      * Spese varie giacenza
572000050617if  4c                   if        w_tgcsgv > *zeros
572100050617     c                   z-add     w_tgcsgv      xvalue
572200050617     c                   exsr      xarrox
572300050617     c                   z-add     xvalue        w_tgcsgv
572400050617    4c                   endif
572500050617      * Spese riconsegna città
572600050617if  4c                   if        w_tgcsgr > *zeros
572700050617     c                   z-add     w_tgcsgr      xvalue
572800050617     c                   exsr      xarrox
572900050617     c                   z-add     xvalue        w_tgcsgr
573000050617    4c                   endif
573100050617      * Spese riconsegna provincia
573200050617if  4c                   if        w_tgcsgp > *zeros
573300050617     c                   z-add     w_tgcsgp      xvalue
573400050617     c                   exsr      xarrox
573500050617     c                   z-add     xvalue        w_tgcsgp
573600050617    4c                   endif
573700050617      * Spese dossier giacenza
573800050617if  4c                   if        w_tgcsgd > *zeros
573900050617     c                   z-add     w_tgcsgd      xvalue
574000050617     c                   exsr      xarrox
574100050617     c                   z-add     xvalue        w_tgcsgd
574200050617    4c                   endif
574300050617      * 1° scaglione tariffa sosta
574400050617if  4c                   if        w_tgcst1 > *zeros
574500050617     c                   z-add     w_tgcst1      xvalue
574600050617     c                   exsr      xarrox
574700050617     c                   z-add     xvalue        w_tgcst1
574800050617    4c                   endif
574900050617      * 2° scaglione tariffa sosta
575000050617if  4c                   if        w_tgcst2 > *zeros
575100050617     c                   z-add     w_tgcst2      xvalue
575200050617     c                   exsr      xarrox
575300050617     c                   z-add     xvalue        w_tgcst2
575400050617    4c                   endif
575500050617      * 3° scaglione tariffa sosta
575600050617if  4c                   if        w_tgcst3 > *zeros
575700050617     c                   z-add     w_tgcst3      xvalue
575800050617     c                   exsr      xarrox
575900050617     c                   z-add     xvalue        w_tgcst3
576000050617    4c                   endif
576100050617      * Tariffa minima sosta
576200050617if  4c                   if        w_tgcstm > *zeros
576300050617     c                   z-add     w_tgcstm      xvalue
576400050617     c                   exsr      xarrox
576500050617     c                   z-add     xvalue        w_tgcstm
576600050617    4c                   endif
576700050628    3c                   endif
576800050617      * Arrotondamento ai decimali
576900050628if  3c                   if        vidsad = 'S'
577000050617      * Spese varie giacenza
577100050628if  4c                   if        w_tgcsgv > *zeros
577200050617     c                   z-add     w_tgcsgv      xvalde
577300050617     c                   exsr      xarrod
577400050617     c                   z-add     xvalde        w_tgcsgv
577500050628    4c                   endif
577600050617      * Spese riconsegna città
577700050628if  4c                   if        w_tgcsgr > *zeros
577800050617     c                   z-add     w_tgcsgr      xvalde
577900050617     c                   exsr      xarrod
578000050617     c                   z-add     xvalde        w_tgcsgr
578100050628    4c                   endif
578200050617      * Spese riconsegna provincia
578300050628if  4c                   if        w_tgcsgp > *zeros
578400050617     c                   z-add     w_tgcsgp      xvalde
578500050617     c                   exsr      xarrod
578600050617     c                   z-add     xvalde        w_tgcsgp
578700050628    4c                   endif
578800050617      * Spese dossier giacenza
578900050628if  4c                   if        w_tgcsgd > *zeros
579000050617     c                   z-add     w_tgcsgd      xvalde
579100050617     c                   exsr      xarrod
579200050617     c                   z-add     xvalde        w_tgcsgd
579300050628    4c                   endif
579400050617      * 1° scaglione tariffa sosta
579500050628if  4c                   if        w_tgcst1 > *zeros
579600050617     c                   z-add     w_tgcst1      xvalde
579700050617     c                   exsr      xarrod
579800050617     c                   z-add     xvalde        w_tgcst1
579900050628    4c                   endif
580000050617      * 2° scaglione tariffa sosta
580100050628if  4c                   if        w_tgcst2 > *zeros
580200050617     c                   z-add     w_tgcst2      xvalde
580300050617     c                   exsr      xarrod
580400050617     c                   z-add     xvalde        w_tgcst2
580500050628    4c                   endif
580600050617      * 3° scaglione tariffa sosta
580700050628if  4c                   if        w_tgcst3 > *zeros
580800050617     c                   z-add     w_tgcst3      xvalde
580900050617     c                   exsr      xarrod
581000050617     c                   z-add     xvalde        w_tgcst3
581100050628    4c                   endif
581200050617      * Tariffa minima sosta
581300050628if  4c                   if        w_tgcstm > *zeros
581400050617     c                   z-add     w_tgcstm      xvalde
581500050617     c                   exsr      xarrod
581600050617     c                   z-add     xvalde        w_tgcstm
581700050628    4c                   endif
581800050628    3c                   endif
581900050617    2c                   endif
582000050617
582100050617      * valori positivi nei campi
582200050617     c                   mllzo     1             w_tgcsgv
582300050617     c                   mllzo     1             w_tgcsgr
582400050617     c                   mllzo     1             w_tgcsgp
582500050617     c                   mllzo     1             w_tgcsgd
582600050617     c                   mllzo     1             w_tgcst1
582700050617     c                   mllzo     1             w_tgcst2
582800050617     c                   mllzo     1             w_tgcst3
582900050617     c                   mllzo     1             w_tgcstm
583000050617
583100050617      * controllo se i campi aggiornati superano il valore massimo
583200050617if  2c                   if        w_tgcsgv > 99999999,999 or
583300050617     c                             w_tgcsgr > 99999999,999 or
583400050617     c                             w_tgcsgp > 99999999,999 or
583500050617     c                             w_tgcsgd > 99999999,999 or
583600050617     c                             w_tgcst1 > 99999999,999 or
583700050617     c                             w_tgcst2 > 99999999,999 or
583800050617     c                             w_tgcst3 > 99999999,999 or
583900050617     c                             w_tgcstm > 99999999,999
584000050617     c                   eval      wnoagg = *on
584100050617     c                   leavesr
584200050617    2c                   endif
584300050617
584400050617      * ---- Rapporto peso volume
584500050617     c                   eval      w_tgcrpv = tgcrpv
584600050617
584700050617if  2c                   if        vidvrr > *zeros and w_tgcrpv > *Zeros
584800050617     c                   if        vidrad = '+'
584900050617     c                   add       vidvrr        w_tgcrpv
585000050617     c                   else
585100050617     c                   sub       vidvrr        w_tgcrpv
585200050617      * se minore non aggiorno
585300050617     c                   if        w_tgcrpv <= *zeros
585400050617     c                   eval      wnoagg = *on
585500050617     c                   leavesr
585600050617     c                   endif
585700050617     c                   endif
585800050617    2c                   endif
585900050617      * annullo i rapporto peso volume
586000050617if  2c                   If        vidarr = 'SI' and w_tgcrpv > *Zeros
586100050617     c                   Clear                   w_tgcrpv
586200050617    2c                   EndIf
586300061207      * se inserimento imposto il rapporto peso volume
586400061207     c                   if        vidirr > 0
586500061207     c                   z-add     vidirr        w_tgcrpv
586600061207     c                   endif
586700050617
586800050617     c                   mllzo     1             w_tgcrpv
586900050617      * controllo se i campi aggiornati superano il valore massimo
587000050617if  2c                   if        w_tgcrpv > 99,9
587100050617     c                   eval      wnoagg = *on
587200050617     c                   leavesr
587300050617    2c                   endif
587400050617
587500050617      * aggiorno le tariffe di giacenza
587600050617if  2c                   if        wsimula = '1'
587700050617
587800050617     c                   eval      tgcsgv = w_tgcsgv
587900050617     c                   eval      tgcsgr = w_tgcsgr
588000050617     c                   eval      tgcsgp = w_tgcsgp
588100050617     c                   eval      tgcsgd = w_tgcsgd
588200050617     c                   eval      tgcst1 = w_tgcst1
588300050617     c                   eval      tgcst2 = w_tgcst2
588400050617     c                   eval      tgcst3 = w_tgcst3
588500050617     c                   eval      tgcstm = w_tgcstm
588600050627     c                   eval      tgcrpv = w_tgcrpv
588700050617
588800050617     c                   z-add     dateu8        tgcdtr
588900050617     c                   clear                   tgcftr
589000050913     c  n50              update    titgc000
589100050913     c   50              update    tiogc000
589200050617      * valorizzo il flag per la scrittura dello storico variazioni
589300050617     c                   eval      mangia = 'S'
589400050617    2c                   endif
589500050616
589600050617    1c                   endif
589700050616
589800050616     c                   endsr
589900900410     C*
590000930623     C*--- ARROTONDAMENTO VARIABILI ----------------------------------*
590100900410     C     XARROX        BEGSR
590200900410     C* XDE   : "D" DIFETTO   "E" ECCESSO
590300900410     C* XVALUE: VALORE DA ARROTONDARE
590400900410     C* XARROT: ARROTONDAMENTO
590500900410     C     *LIKE         DEFINE    XVALUE        XCOM02
590600900410     C     *LIKE         DEFINE    XVALUE        XARROT
590700900410     C                   MOVE      XDE           XDE               1
590800900410     C     XARROT        IFGT      0
590900900410     C     XVALUE        DIV       XARROT        XCOM01
591000900410     C                   MVR                     XCOM02
591100900410     C     XDE           IFEQ      'E'
591200900410     C     XCOM02        IFGT      0
591300950119     C                   ADD       1             XCOM01           15 0
591400900410     C                   END
591500900410     C                   END
591600950118     C     XCOM01        MULT      XARROT        XVALUE           15 3
591700900410     C                   END
591800900410     C                   ENDSR
591900930624     C*
592000010323     C*--- ARROTONDAMENTO VARIABILI AL DECIMALE-----------------------*
592100010323     C     XARROD        BEGSR
592200010323     C* VIDDEC: NUMERO DECIMALI
592300010323     C* XVALDE: VALORE DA ARROTONDARE AI DECIMALI
592400010323     C*
592500010323     C* 0 DECIMALI
592600010323     C     VIDDEC        IFEQ      0
592700010323     C                   Z-ADD(H)  XVALDE        CAMP0D           15 0
592800010323     C                   Z-ADD     CAMP0D        XVALDE
592900010323     C                   ENDIF
593000010323     C* 1 DECIMALE
593100010323     C     VIDDEC        IFEQ      1
593200010323     C                   Z-ADD(H)  XVALDE        CAMP1D           15 1
593300010323     C                   Z-ADD     CAMP1D        XVALDE
593400010323     C                   ENDIF
593500010323     C* 2 DECIMALI
593600010323     C     VIDDEC        IFEQ      2
593700010323     C                   Z-ADD(H)  XVALDE        CAMP2D           15 2
593800010323     C                   Z-ADD     CAMP2D        XVALDE
593900010323     C                   ENDIF
594000010323     C*
594100010323     C                   ENDSR
594200010323     C*
594300960523     C*--- CONTROLLO CAMPI VIDEO DUPLICA DETTAGLIO TARIFFA -----------*
594400960523     C     CTRD08        BEGSR
594500990817     C* CONTROLLI CHE VENGONO EFFETTUATI OLTRE ALLA CORRETTEZZA DEI DATI :
594600990817     C* A) SE SONO VALORIZZATE SOLO LE LINEE DI PARTENZE E NON  I CODICI
594700990817     C*    TASSAZIONE SI COPIA TUTTO SENZA FARE CONTROLLI
594800990817     C* B) SE INSERITI ENTRAMBI I CODICI TASSAZIONE :
594900990817     C*    1) DEVONO ESSERE ENTRAMBI ITALIA O ESTERO
595000990817     C*    2) NON SI COPIA IL DETTAGLIO CON INSERITO IL CAP ED EMETTERE UN
595100990817     C*       MESSAGGIO DI ERRORE FORZABIOLE LI
595200960523     C*
595300960523     C****  EFFETTUARE ALMENO UNA SELEZIONE  ****
595400960523    1C     V8CLNN        IFEQ      0
595500960523     C     V8CCTN        ANDEQ     *BLANKS
595600960523     C     V8CLNP        ANDEQ     0
595700960523     C     V8CCTS        ANDEQ     *BLANKS
595800960523     C                   MOVEL     MSG(37)       V8CMSG
595900960523     C                   SETON                                        7128
596000960523     C                   GOTO      ENDC08
596100960523    1C                   ENDIF
596200960523     C*
596300960527     C****  RICERCA CODICI TASSAZIONE  ****
596400960527    1C     V8CCTN        IFEQ      '?'
596500120713     c                   exsr      Ric_CT
596600120713     C                   MOVEL     TB09cod       V8CCTN
596700960527     C                   SETON                                        90
596800960527     C                   GOTO      ENDC08
596900960527    1C                   ENDIF
597000960527     C*
597100960527    1C     V8CCTS        IFEQ      '?'
597200120713     c                   exsr      Ric_CT
597300120713     c                   eval      TRTB09DS = kpjbu
597400120713     C                   MOVEL     TB09cod       V8CCTS
597500960527     C                   SETON                                        90
597600960527     C                   GOTO      ENDC08
597700960527    1C                   ENDIF
597800960527     C*
597900960523     C****  LINEA PARTENZA  ****
598000960523     C* SE IMMESSA LA LINEA PARTENZA DA CREARE OCCORRE IMMETTERE ANCHE
598100960523     C*   LA LINEA PARTENZA DA CUI COPIARE E VICEVERSA
598200960523    1C     V8CLNN        IFGT      0
598300960523     C     V8CLNP        ANDEQ     0
598400960524     C                   MOVEL     MSG(65)       V8CMSG
598500960523     C                   SETON                                        7328
598600960523     C                   GOTO      ENDC08
598700960523    1C                   ENDIF
598800960523     C*
598900960523    1C     V8CLNN        IFEQ      0
599000960523     C     V8CLNP        ANDGT     0
599100960524     C                   MOVEL     MSG(65)       V8CMSG
599200960523     C                   SETON                                        7128
599300960523     C                   GOTO      ENDC08
599400960523    1C                   ENDIF
599500960524     C*
599600960524     C* SE INSERITE SOLO LE LINEE PARTENZA DEVONO ESSERE DIVERSE
599700960524    1C     V8CLNN        IFGT      0
599800960524     C     V8CCTN        ANDEQ     *BLANKS
599900960524     C     V8CCTS        ANDEQ     *BLANKS
600000960524     C     V8CLNN        ANDEQ     V8CLNP
600100960524     C                   MOVEL     MSG(63)       V8CMSG
600200960524     C                   SETON                                        7128
600300960524     C                   GOTO      ENDC08
600400960524    1C                   ENDIF
600500960523     C*
600600960523     C****  CODICE TASSAZIONE  ****
600700960524     C* SE IMMESSO COD.TASSAZ. OCCORRE IMMETTERE ANCHE LINEA PARTENZA
600800960524    1C     V8CCTN        IFNE      *BLANKS
600900960524     C     V8CLNN        ANDEQ     0
601000960524     C                   MOVEL     MSG(65)       V8CMSG
601100960524     C                   SETON                                        7128
601200960524     C                   GOTO      ENDC08
601300960524    1C                   ENDIF
601400960524     C*
601500960524    1C     V8CCTS        IFNE      *BLANKS
601600960524     C     V8CLNP        ANDEQ     0
601700960524     C                   MOVEL     MSG(65)       V8CMSG
601800960524     C                   SETON                                        7328
601900960524     C                   GOTO      ENDC08
602000960524    1C                   ENDIF
602100960523     C*
602200960524     C* SE IMMESSO IL COD.TASSAZIONE DA CREARE OCCORRE IMMETTERE ANCHE
602300960524     C*   IL COD.TASSAZIONE DA CUI COPIARE E VICEVERSA
602400960524    1C     V8CCTS        IFNE      *BLANKS
602500960524     C     V8CCTN        ANDEQ     *BLANKS
602600960524     C                   MOVEL     MSG(66)       V8CMSG
602700960527     C                   SETON                                        7228
602800960524     C                   GOTO      ENDC08
602900960524    1C                   ENDIF
603000960524     C*
603100960524    1C     V8CCTN        IFNE      *BLANKS
603200960524     C     V8CCTS        ANDEQ     *BLANKS
603300960524     C                   MOVEL     MSG(66)       V8CMSG
603400960527     C                   SETON                                        7428
603500960524     C                   GOTO      ENDC08
603600960524    1C                   ENDIF
603700960524     C*
603800960524     C* SE INSERITI LINEE PARTENZA E COD.TASSAZ. DEVONO ESSERE DIVERSI
603900960524    1C     V8CLNN        IFGT      0
604000960524     C     V8CCTN        ANDNE     *BLANKS
604100960524     C     V8CLNN        ANDEQ     V8CLNP
604200960524     C     V8CCTN        ANDEQ     V8CCTS
604300960524     C                   MOVEL     MSG(64)       V8CMSG
604400960524     C                   SETON                                        7128
604500960524     C                   GOTO      ENDC08
604600960524    1C                   ENDIF
604700960524     C*
604800960523     C****  LINEA PARTENZA DA CREARE  ****
604900051019    1C     V8CLNN        IFne      0
605000020312     c                   clear                   og143
605100960523     C     V8CLNN        CHAIN     AZORG                              34
605200960523    2C     *IN34         IFEQ      *ON
605300960523     C     ORGFVA        ORNE      ' '
605400960523     C     ORGFAG        ORNE      'A'
605500960523     C     ORGFAG        ANDNE     'F'
605600020527     c     orgfl2        andne     'S'
605700960523     C                   MOVEL     MSG(22)       V8CMSG
605800960523     C                   SETON                                        7128
605900960523     C                   GOTO      ENDC08
606000960523    2C                   ENDIF
606100020312
606200020312     c                   eval      og143 = orgde3
606300020312
606400990817     C* VERIFICO SE LA LINEA DI PARTENZA E' CONGRUENTE CON  TIPO TARIFFA ITALIA
606500990817     C*
606600020312   2 c                   if        (§ogntw ='EEX' or §ogntw = 'EUP' or
606700020312     c                              §ogntw = 'FED') and v1cfie = 'I' and
606800020312     c                              v1cnet = *blanks
606900990817     C                   MOVEL     MSG(45)       V8CMSG
607000990817     C                   SETON                                        7128
607100990817     C                   GOTO      ENDC08
607200990817    2C                   ENDIF
607300990817     C*
607400960523    1C                   ENDIF
607500960523     C*
607600960523     C****  CODICE TASSAZIONE DA CREARE  ****
607700960523    1C     V8CCTN        IFNE      *BLANKS
607800960523     C                   Z-ADD     1             X
607900960523     C     V8CCTN        LOOKUP    CCT(X)                                 35
608000960523    2C     *IN35         IFEQ      *OFF
608100960523     C                   MOVEL     MSG(23)       V8CMSG
608200960523     C                   SETON                                        7228
608300960523     C                   GOTO      ENDC08
608400960523   X2C                   ELSE
608500050303      * errore bloccante se codice tassazione non utilizzabile in gestione
608600050303      * tariffe clienti
608700050303     c                   If        uta(x) = 'N'
608800050303     c                   Eval      v8cmsg = msg(36)
608900050303     c                   Eval      *In72 = *On
609000050303     c                   Eval      *In28 = *On
609100050303     c                   Goto      endc08
609200050303     c                   EndIf
609300020206      *
609400020206      * Se tariffa FedEx devo utilizzare solo i codici tariffa
609500020206      * previsti per FedEx
609600020206    3C     V1CNET        IFEQ      'F'
609700020214     C     UTF(X)        ANDNE     'S'
609800020206     C                   SETON                                        7228
609900020206     C                   MOVEL     MSG(100)      V8CMSG
610000020206     C                   GOTO      ENDC08
610100020206    3C                   ENDIF
610200020206      * Se tariffa non è FedEx non posso utilizzare i codici
610300020206      * tariffa previsti per FedEx
610400020206    3C     V1CNET        IFNE      'F'
610500020214     C     UTF(X)        ANDEQ     'S'
610600020206     C                   SETON                                        7228
610700020206     C                   MOVEL     MSG(101)      V8CMSG
610800020206     C                   GOTO      ENDC08
610900020206    3C                   ENDIF
611000020206      *
611100990817     C* VERIFICO SE CODICE TASSAZIONE  E' CONGRUENTE CON IL TIPO TARIFFA ITALIA
611200990817     C*
611300990817    3C     V1CFIE        IFEQ      'I'
611400020201     C     V1CNET        ANDEQ     ' '
611500990817     C     NAR(X)        LOOKUP    NAZ                                    35
611600990817    4C     *IN35         IFEQ      *OFF
611700990817     C                   MOVEL     MSG(47)       V8CMSG
611800990817     C                   SETON                                        7228
611900990817     C                   GOTO      ENDC08
612000990817    4C                   ENDIF
612100990817    3C                   ENDIF
612200990817     C*
612300990608     C                   MOVE      ORP(X)        WORPN
612400960523    2C                   ENDIF
612500990830     C*
612600990830     C* SE TARIFFA ESTERA LINEA DI PARTENZA E/O CODICE TASSAZIONE DEVE ESSERE
612700990830     C* ESTERO
612800020312   2 c                   if        v1cfie = 'E' and §ogntw <> 'EEX' and
612900020312     c                             §ogntw <> 'EUP' and §ogntw <> 'FED'
613000990830     C     NAR(X)        LOOKUP    NAZ                                    35
613100990830    3C     *IN35         IFEQ      *ON
613200990830     C                   MOVEL     MSG(48)       V8CMSG
613300990830     C                   SETON                                        7128
613400990830     C                   GOTO      ENDC08
613500990830    3C                   ENDIF
613600990830    2C                   ENDIF
613700960523    1C                   ENDIF
613800960523     C*
613900960523     C****  LINEA PARTENZA DA CUI COPIARE  ****
614000051019    1C     V8CLNP        IFne      0
614100960523     C     V8CLNP        CHAIN     AZORG                              34
614200960523    2C     *IN34         IFEQ      *ON
614300960523     C     ORGFVA        ORNE      ' '
614400960523     C     ORGFAG        ORNE      'A'
614500960523     C     ORGFAG        ANDNE     'F'
614600020527     c     orgfl2        andne     'S'
614700960523     C                   MOVEL     MSG(22)       V8CMSG
614800960523     C                   SETON                                        7328
614900960523     C                   GOTO      ENDC08
615000960523    2C                   ENDIF
615100960523    1C                   ENDIF
615200960523     C*
615300960523     C****  CODICE TASSAZIONE DA CUI COPIARE  ****
615400960523    1C     V8CCTS        IFNE      *BLANKS
615500960523     C                   Z-ADD     1             X
615600960523     C     V8CCTS        LOOKUP    CCT(X)                                 35
615700960523    2C     *IN35         IFEQ      *OFF
615800960523     C                   MOVEL     MSG(23)       V8CMSG
615900960523     C                   SETON                                        7428
616000960523     C                   GOTO      ENDC08
616100960523   X2C                   ELSE
616200050303      * errore bloccante se codice tassazione non utilizzabile in gestione
616300050303      * tariffe clienti
616400050303     c                   If        uta(x) = 'N'
616500050303     c                   Eval      v8cmsg = msg(36)
616600050303     c                   Eval      *In74 = *On
616700050303     c                   Eval      *In28 = *On
616800050303     c                   Goto      endc08
616900050303     c                   EndIf
617000020206      *
617100020206      * Se tariffa FedEx devo utilizzare solo i codici tariffa
617200020206      * previsti per FedEx
617300020206    3C     V1CNET        IFEQ      'F'
617400020214     C     UTF(X)        ANDNE     'S'
617500020206     C                   SETON                                        7428
617600020206     C                   MOVEL     MSG(100)      V8CMSG
617700020206     C                   GOTO      ENDC08
617800020206    3C                   ENDIF
617900020206      * Se tariffa non è FedEx non posso utilizzare i codici
618000020206      * tariffa previsti per FedEx
618100020206    3C     V1CNET        IFNE      'F'
618200020214     C     UTF(X)        ANDEQ     'S'
618300020206     C                   SETON                                        7428
618400020206     C                   MOVEL     MSG(101)      V8CMSG
618500020206     C                   GOTO      ENDC08
618600020206    3C                   ENDIF
618700020206      *
618800990608     C                   MOVE      ORP(X)        WORP
618900960523    2C                   ENDIF
619000960523    1C                   ENDIF
619100960523     C*
619200960523     C****  CONTROLLO CHE ESISTA ALMENO UNA RIGA DI DETTAGLIO  ****
619300960523     C****  DA CUI POTER COPIARE                               ****
619400960523     C                   MOVEL     '1'           WTROV             1
619500960523    1C     V8CLNP        IFGT      0
619600960524     C                   MOVEL     V8CLNP        COMLN2
619700960524     C*
619800960523     C* IMMESSA SOLO LA LINEA PARTENZA
619900960523    2C     V8CCTS        IFEQ      *BLANKS
620000061011     C  n50KTAD2         SETLL     TITAD000
620100061011     c   50ktad2         setll     tiofd000
620200061011     C  n50KTAD2         READE     TITAD000                               34
620300061011     c   50ktad2         reade     tiofd000                               34
620400960523   X2C                   ELSE
620500960523     C* IMMESSO ANCHE IL CODICE TASSAZIONE
620600990608     C                   MOVE      WORP          COMOR2
620700061011     C  n50KTAD3         SETLL     TITAD000
620800061011     c   50ktad3         setll     tiofd000
620900061011     C  n50KTAD3         READE     TITAD000                               34
621000061011     c   50ktad3         reade     tiofd000                               34
621100960523    2C                   ENDIF
621200960523     C*
621300960523    2C     *IN34         DOWEQ     *OFF
621400960523    3C     TADATB        IFEQ      ' '
621500960523     C                   SETON                                        34
621600960523     C                   CLEAR                   WTROV
621700960523   X3C                   ELSE
621800960523     C*
621900960523    4C     V8CCTS        IFEQ      *BLANKS
622000061011     C  n50KTAD2         READE     TITAD000                               34
622100061011     c   50ktad2         reade     tiofd000                               34
622200960523   X4C                   ELSE
622300061011     C  n50KTAD3         READE     TITAD000                               34
622400061011     c   50ktad3         reade     tiofd000                               34
622500960523    4C                   ENDIF
622600960523    3C                   ENDIF
622700960523    2C                   ENDDO
622800960523     C*
622900960523     C* WTROV <> " " --> NON ESISTONO RIGHE DI DETTAGLIO DA CUI COPIARE
623000960523    2C     WTROV         IFNE      ' '
623100960523     C                   MOVEL     MSG(62)       V8CMSG
623200960523     C                   SETON                                        7328
623300960523     C                   GOTO      ENDC08
623400960523    2C                   ENDIF
623500960523    1C                   ENDIF
623600960523     C*
623700960524     C****  SE ESISTONO GIA' RIGHE DI DETTAGLIO CON LINEA PART.   ****
623800960524     C****  E COD.TASS. DA CREARE AVVISO CON MESSAGGIO FORZABILE  ****
623900960527    0C     V8CLNN        IFNE      SAVLNN
624000960527     C     V8CCTN        ORNE      SAVCTN
624100960527     C                   MOVEL     V8CLNN        SAVLNN
624200960527     C                   MOVEL     V8CCTN        SAVCTN
624300960527     C                   CLEAR                   WTROV
624400960527     C*
624500960524    1C     V8CLNN        IFGT      0
624600960524     C                   MOVEL     V8CLNN        COMLN2
624700960524     C*
624800960524     C* IMMESSA SOLO LA LINEA PARTENZA
624900960524    2C     V8CCTN        IFEQ      *BLANKS
625000061011     C  n50KTAD2         SETLL     TITAD000
625100061011     c   50ktad2         setll     tiofd000
625200061011     C  n50KTAD2         READE     TITAD000                               34
625300061011     c   50ktad2         reade     tiofd000                               34
625400960524   X2C                   ELSE
625500960524     C* IMMESSO ANCHE IL CODICE TASSAZIONE
625600990608     C                   MOVE      WORPN         COMOR2
625700061011     C  n50KTAD3         SETLL     TITAD000
625800061011     c   50ktad3         setll     tiofd000
625900061011     C  n50KTAD3         READE     TITAD000                               34
626000061011     c   50ktad3         reade     tiofd000                               34
626100960524    2C                   ENDIF
626200960524     C*
626300960524    2C     *IN34         DOWEQ     *OFF
626400960524    3C     TADATB        IFEQ      ' '
626500960524     C                   SETON                                        34
626600960527     C                   MOVEL     '1'           WTROV
626700960524   X3C                   ELSE
626800960524     C*
626900960524    4C     V8CCTN        IFEQ      *BLANKS
627000061011     C  n50KTAD2         READE     TITAD000                               34
627100061011     c   50ktad2         reade     tiofd000                               34
627200960524   X4C                   ELSE
627300061011     C  n50KTAD3         READE     TITAD000                               34
627400061011     c   50ktad3         reade     tiofd000                               34
627500960524    4C                   ENDIF
627600960524    3C                   ENDIF
627700960524    2C                   ENDDO
627800960524     C*
627900960527     C* WTROV <> " " --> LA TARIFFA CHE SI VUOLE CREARE ESISTE GIA'
628000960527    2C     WTROV         IFNE      ' '
628100960527     C     V8CCTN        IFEQ      *BLANKS
628200960527     C                   MOVEL     MSG(68)       V8CMSG
628300960527     C                   ELSE
628400960524     C                   MOVEL     MSG(67)       V8CMSG
628500960527     C                   ENDIF
628600960527     C                   SETON                                        7128
628700960524     C                   GOTO      ENDC08
628800960524    2C                   ENDIF
628900960524    1C                   ENDIF
629000960527     C*
629100960527    0C                   ENDIF
629200090608
629300090625      * i prossimi controlli li avevamo messi convinti che la tassazione
629400090625      * non arrivasse a tassare con 950 e IT, invece no, ci sono clienti
629500090625      * che hanno solo questo tipo di tariffa e le spedizioni sono
629600090625      * regolarmente tassate quindi bypasso i controlli che avevamo fatto
629700090625      * quindi ridiamo la possibilità di duplicare verso 950 IT
629800090625     c                   leavesr
629900090608      * se la nuova linea di partenza è 950
630000090608    1c                   if        v8clnn = 950
630100090608      * non posso richiedere codice di tassazione IT o EE
630200090608     c                   if        v8cctn = 'IT' or v8cctn = 'EE'
630300090608     c                   eval      *in72 = *on
630400090608     c                   eval      *in28 = *on
630500090608     c                   eval      v8cmsg = msg(110)
630600090608     c                   leavesr
630700090608     c                   endif
630800090608      * non posso duplicare se esiste un codice di tassazione IT o EE
630900090608     c                   clear                   wtrov
631000090608     c                   movel     v8clnp        comln2
631100090608     c  n50ktad2         setll     titad04l
631200090608     c   50ktad2         setll     tiofd01l
631300090608    2c                   do        *hival
631400090608     c  n50ktad2         reade     titad04l
631500090608     c   50ktad2         reade     tiofd01l
631600090608     c                   if        %eof
631700090608     c                   leave
631800090608     c                   endif
631900090608     c                   if        tadatb <> *blanks
632000090608     c                   iter
632100090608     c                   endif
632200090608     c                   if        tadcts = 'IT' or tadcts = 'EE'
632300090608     c                   eval      wtrov = '1'
632400090608     c                   leave
632500090608     c                   endif
632600090608    2c                   enddo
632700090608     c                   if        wtrov <> *blanks
632800090608     c                   eval      *in71 = *on
632900090608     c                   eval      *in28 = *on
633000090608     c                   eval      v8cmsg = msg(121)
633100090608     c                   eval      %subst(v8cmsg:36:3) = %editc(v8clnp:'1')
633200090608     c                   leavesr
633300090608     c                   endif
633400090608    1c                   endif
633500960524     C*
633600960523     C     ENDC08        ENDSR
633700960523     C*
633800960523     C*--- ESEGUO DUPLICAZIONE DETTAGLIO TARIFFA ---------------------*
633900960523     C     DUPTAR        BEGSR
634000960523     C*
634100061012     c  n50              unlock    titad04l
634200061012     c   50              unlock    tiofd01l
634300061012
634400061012     c                   clear                   Tnta25ds
634500061012     c  n50              eval      ta25tipo = 'T'
634600061012     c   50              eval      ta25tipo = 'O'
634700061012     c                   eval      ta25ksco = v1cksc
634800061012     c                   move      vctr          ta25ctro
634900061012     c                   move      prg           ta25prgo
635000061012     c  n50              eval      ta25tipn = 'T'
635100061012     c   50              eval      ta25tipn = 'O'
635200061012     c                   eval      ta25kscn = v1cksc
635300061012     c                   move      vctr          ta25ctrn
635400061012     c                   move      prg           ta25prgn
635500061012     c                   eval      ta25tam = 'N'
635600061012     c                   eval      ta25fie = v1cfie
635700061012     c                   eval      ta25tam = 'N'
635800061012     c                   eval      ta25tad = 'N'
635900061012     c                   eval      ta25tpt = 'N'
636000061012     c                   eval      ta25tpd = 'N'
636100061012     c                   eval      ta25tgc = 'N'
636200061012     c                   eval      ta25cat = 'N'
636300061012     c                   eval      ta25inglo = 'N'
636400061012     c                   eval      ta25dupd = 'S'
636500061012     c                   move      v8clnp        ta25olnp
636600061012     c                   move      v8ccts        ta25octs
636700061012     c                   move      v8clnn        ta25nlnp
636800061012     c                   move      v8cctn        ta25ncts
636900090917      * imposto se da trattativa
637000090917     c                   eval      %subst(kpjbu:1:1) = partrat
637100061012
637200061012     c                   Call      'TNTA25R'
637300061012     c                   Parm                    kpjba
637400061012     c                   Parm                    Tnta25ds
637500061012     c                   Parm                    Tnta25tad
637600061012     c                   Parm                    Tnta25tgc
637700061012     c                   Parm                    Tnta25tpd
637800061012     c                   Parm                    Tnta25tpt
637900061012
638000061012     c                   If        ta25err <> *Blanks
638100061012     c                   Eval      *In28 = *On
638200061012     c                   Eval      vccmsg = ta25msg
638300061012     c                   endif
638400960523     C*
638500960523     C                   ENDSR
638600960523     C*
638700990601     C*--- CONTROLLO TITAD -------------------------------------------*
638800900410     C     CTRAGG        BEGSR
638900940525     C                   SETOFF                                       22
639000940525     C*
639100940706     C                   READC     TA01S03                                29
639200940525    1C     *IN29         DOWEQ     *OFF
639300940525     C*
639400940525     C* RECORD NON ANNULLATO
639500940525    2C     TADATB        IFNE      'A'
639600940525     C*
639700960522     C* SE ESISTONO IMPORTO MINIMO O IMPORTO MASSIMO VISUALIZZO
639800960522     C*   LA LETTERA "M" SULLA RIGA DI DETTAGLIO
639900960522    3C     TADMIN        IFGT      0
640000960522     C     TADMAX        ORGT      0
640100960522     C                   SETON                                        13
640200960522    3C                   ENDIF
640300960522     C*
640400940525     C****  TARIFFA CITTA'  ****
640500940525     C* TARIFFA CITTA' OBBLIGATORIA
640600940525    3C     TADITR        IFEQ      0
640700940526     C                   SETON                                        5928
640800940526     C                   MOVEL     MSG(29)       VCCMSG
640900940525     C                   GOTO      GIUCT2
641000941123     C                   ELSE
641100050616     C* 88 OFF -  TARIFFA NON IN % verifico se si
641200941124     C*   POSSONO INSERIRE DECIMALI
641300941124     C     *IN88         IFEQ      *OFF
641400941123     C                   MOVE      TADITR        W0030             3 0
641500941123     C     W0030         IFGT      0
641600050616     C*  LA MONETA NON ACCETTA DECIMALI
641700990617     C     §CVFDC        ANDNE     'S'
641800941123     C                   SETON                                        5928
641900990617     C                   MOVEL     MSG(79)       VCCMSG
642000941123     C                   GOTO      GIUCT2
642100941123     C                   ENDIF
642200941123     C                   ENDIF
642300940525    3C                   ENDIF
642400940525     C*
642500940525     C****  TARIFFA PROVINCIA  ****
642600940525     C* SE INSERITO IL CAP NON BISOGNA IMMETTERE LA TARIFFA PROVINCIA
642700020624    3C     tadCAP        IFNE      *BLANKS
642800940525     C     TADINO        ANDNE     0
642900940526     C                   SETON                                        6128
643000940526     C                   MOVEL     MSG(27)       VCCMSG
643100940525     C                   GOTO      GIUCT2
643200940525    3C                   ENDIF
643300941123     C*
643400050616     C* 88 OFF -  TARIFFA NON IN % verifico se si
643500941124     C*   POSSONO INSERIRE DECIMALI
643600941124     C  N88TADINO        IFGT      0
643700941123     C                   MOVE      TADINO        W0030             3 0
643800941123     C     W0030         IFGT      0
643900050616     C*  LA MONETA NON ACCETTA DECIMALI
644000990617     C     §CVFDC        ANDNE     'S'
644100941123     C                   SETON                                        6128
644200990617     C*                    MOVELMSG,55    VCCMSG
644300990617     C                   MOVEL     MSG(79)       VCCMSG
644400941123     C                   GOTO      GIUCT2
644500941123     C                   ENDIF
644600941123     C                   ENDIF
644700940525     C*
644800960522     C****  RAPPORTO PESO/VOLUME  ****
644900960522     C* SE INSERITO UN RAPPORTO PESO/VOL. SUPERIORE AL LIMITE INDICATO
645000960522     C*   IN TABELLA EMETTO ERRORE FORZABILE
645100960522     C     TADRPV        IFNE      SAVRPV
645200960522     C     TADRPV        ANDGT     §1GRPV
645300960522     C                   SETON                                        8928
645400960522     C                   MOVEL     MSG(59)       VCCMSG
645500960522     C                   MOVEL     TADRPV        SAVRPV
645600960522     C                   GOTO      GIUCT2
645700960522     C                   ENDIF
645800990617     C*
645900990617     C****  IMPORTO MASSIMO TARIFFA  ****
646000990617     C* VERIFICO SE INSERITO I DECIMALI SE SONO AMMESSI OPPURE NO
646100990617     C     TADMAX        IFGT      0
646200990617     C                   MOVE      TADMAX        W0030             3 0
646300990617     C     W0030         IFGT      0
646400990617     C* E LA MONETA NON ACCETTA DECIMALI
646500990617     C     §CVFDC        ANDNE     'S'
646600990617     C                   SETON                                        6228
646700990617     C                   MOVEL     MSG(79)       VCCMSG
646800990618     C                   MOVEL     *ON           *INKK
646900990617     C                   GOTO      GIUCT2
647000990617     C                   ENDIF
647100990617     C                   ENDIF
647200990617     C*
647300990617     C****  IMPORTO MINIMO  TARIFFA  ****
647400990617     C* VERIFICO SE INSERITO I DECIMALI SE SONO AMMESSI OPPURE NO
647500990617     C     TADMIN        IFGT      0
647600990617     C                   MOVE      TADMIN        W0030             3 0
647700990617     C     W0030         IFGT      0
647800990617     C* E LA MONETA NON ACCETTA DECIMALI
647900990617     C     §CVFDC        ANDNE     'S'
648000990617     C                   SETON                                        5228
648100990617     C                   MOVEL     MSG(79)       VCCMSG
648200990618     C                   MOVEL     *ON           *INKK
648300990617     C                   GOTO      GIUCT2
648400990617     C                   ENDIF
648500990617     C                   ENDIF
648600940525     C****  IMPORTO MASSIMO TARIFFA  ****
648700940525     C* SE INSERITO DEVE ESSERE MAGGIORE O UGUALE AL MINIMO
648800940525    3C     TADMAX        IFGT      0
648900940525     C     TADMAX        ANDLT     TADMIN
649000940526     C                   SETON                                        6228
649100940526     C                   MOVEL     MSG(31)       VCCMSG
649200990618     C                   MOVEL     *ON           *INKK
649300940525     C                   GOTO      GIUCT2
649400940525    3C                   ENDIF
649500980310      * E' inutile inserire dei valori minimi e massimi per scaglioni inferiori
649600980310      * al valore minimo tassabile o al valore tariffa finita
649700980310      * quindi faccio il controllo di TADMAX E/O TADMAX
649800980310     C     TADMAX        IFGT      0
649900980310     C     TADMIN        ORGT      0
650000980310      * VALORE TARIFFA FINITA
650100980310     C     TAMATA        IFGT      TADSGL
650200980310     C     TAMATA        OREQ      0
650300980505     C     WRKMIN        ANDGT     TADSGL
650400980310     C                   SETON                                        6228
650500980515     C                   MOVEL     MSG(73)       VCCMSG
650600990618     C                   MOVEL     *ON           *INKK
650700980310     C                   GOTO      GIUCT2
650800980310     C                   ENDIF
650900980310      *
651000980310     C                   ENDIF
651100940525     C*
651200940525    2C                   ENDIF
651300921119     C*
651400940525     C     GIUCT2        TAG
651500940526     C   28              Z-ADD     NRR           REC
651600940525     C*
651700940525     C* 22 - SFLNXTCHG
651800940525     C                   SETON                                        22
651900940706     C                   UPDATE    TA01S03
652000941126     C                   SETOFF                                       2213
652100940525     C*
652200940526     C   28              SETON                                        29
652300940706     C  N29              READC     TA01S03                                29
652400940525    1C                   ENDDO
652500921218     C*
652600940525     C                   ENDSR
652700940509     C*
652800990601     C*--- AGGIORNAMENTO TITAD ---------------------------------------*
652900921120     C     AGGTAD        BEGSR
653000900924     C                   SETOFF                                       19
653100940706     C                   READC     TA01S03                                29
653200940524     C     *IN29         DOWEQ     '0'
653300900925     C                   EXSR      UPDTAD
653400940706     C                   READC     TA01S03                                29
653500921218     C                   END
653600900925     C                   ENDSR
653700930630     C*
653800990601     C*--- AGGIORNAMENTO TITAD SU DISCO ------------------------------*
653900921118     C     UPDTAD        BEGSR
654000980219     C* VALORE POSITIVO NEI CAMPI
654100980220     C                   MLLZO     1             TADSGL
654200980219     C                   MLLZO     1             TADITR
654300980219     C                   MLLZO     1             TADINO
654400980219     C                   MLLZO     1             TADRPV
654500980219     C                   MLLZO     1             TADMIN
654600980219     C                   MLLZO     1             TADMAX
654700980219     C*
654800980219     C                   Z-ADD     TADITR        ITRCOM
654900980219     C                   Z-ADD     TADINO        INOCOM
655000980219     C                   Z-ADD     TADRPV        RPVCOM
655100940518     C                   Z-ADD     TADMIN        MINCOM
655200940518     C                   Z-ADD     TADMAX        MAXCOM
655300940518     C                   MOVEL     TADATB        ATBCOM
655400000000     C** SALVO CAMPI PRIMA CHAIN
655500061011     C  n50KTAD          CHAIN     TITAD000                           92
655600061011     c   50ktad          chain     tiofD000                           92
655700990729     C*
655800990729     C     *IN92         IFEQ      *OFF
655900921118     C*
656000930317     C     *IN50         IFEQ      '1'
656100930317     C     ATBCOM        ANDEQ     'A'
656200061011     C  n50              DELETE    TITAD000
656300061011     C   50              delete    tiofd000
656400930317     C                   ELSE
656500930317     C*
656600930317     C                   MOVEL     ATBCOM        TADATB
656700940518     C                   Z-ADD     ITRCOM        TADITR
656800921210     C                   Z-ADD     INOCOM        TADINO
656900940518     C                   Z-ADD     MINCOM        TADMIN
657000940518     C                   Z-ADD     MAXCOM        TADMAX
657100000000     C                   Z-ADD     RPVCOM        TADRPV
657200910605     C*
657300130124     C*******            Z-ADD     dateu8        TADDTR
657400940511     C                   MOVEL     *BLANKS       TADFTR
657500061011     C  n50              UPDATE    TITAD000
657600061011     C   50              update    tiofd000
657700930317     C                   END
657800990729     C*
657900990729     C                   END
658000930317     C*
658100930317     C     ATBCOM        IFEQ      'A'
658200900924     C                   SETON                                        19
658300900924     C                   END
658400000000     C                   SETON                                        30
658500000000     C                   ENDSR
658600050701      *---------------------------------------------------------------*
658700050701      * controllo se nella tariffa a spedizione esistono + scaglioni
658800050701      * per la stessa lnp e cts
658900050701      *---------------------------------------------------------------*
659000050701     c     Sr_CtrTarSpe  BegSr
659100050701
659200050701     c                   Clear                   savlnp
659300050701     c                   Clear                   savcts
659400050701     c                   Clear                   savsgl
659500050701     c                   Clear                   savnaz
659600050701     c                   Clear                   savcap
659700050701
659800050701     c                   Eval      werr = *Off
659900050701     c                   Clear                   wlnp
660000050701     c                   Clear                   wcts
660100050701     c                   Clear                   wcap
660200050701
660300050701      * Leggo tutto il titad
660400061011     c  n50ktam2         Setll     Titad04l
660500061011     c   50ktam2         Setll     Tiofd01l
660600050701     c                   Do        *Hival
660700061011     c  n50ktam2         Reade(n)  Titad04l
660800061011     c   50ktam2         Reade(n)  Tiofd01l
660900050701
661000061011     c                   If        %Eof
661100050701     c                   Leave
661200050701     c                   EndIf
661300050701
661400050701     c                   If        tadatb <> *Blanks
661500050701     c                   Iter
661600050701     c                   EndIf
661700050701
661800050701     c                   If        tadlnp = savlnp and tadcts = savcts
661900050701     c                             and tadnaz = savnaz and tadcap = savcap
662000050701     c                             and tadsgl <> savsgl and savsgl > *Zeros
662100050701     c                   Move      tadlnp        wlnp
662200050701     c                   Move      tadcts        wcts
662300050701     c                   If        Tadcap <> *Blanks
662400050701     c                   Move      tadcap        wcap
662500050701     c                   EndIf
662600050701     c                   Eval      werr = *On
662700050701     c                   Leave
662800050701     c                   EndIf
662900050701     c                   If         tadlnp <> savlnp or tadcts <> savcts
663000050701     c                              or tadnaz <> savnaz or tadcap <> savcap
663100050701     c                              or tadsgl <> savsgl
663200050701     c                   Eval       savlnp = tadlnp
663300050701     c                   Eval       savcts = tadcts
663400050701     c                   Eval       savnaz = tadnaz
663500050701     c                   Eval       savcap = tadcap
663600050701     c                   Eval       savsgl = tadsgl
663700050701     c                   EndIf
663800050701
663900050701     c                   EndDo
664000050701
664100050701     c                   EndSr
664200110128
664300110128      /free
664400110128       //---------------------------------------------------------------
664500110128       //?Controllo se rapporto peso volume uguale su tutti gli scaglioni
664600110128       //---------------------------------------------------------------
664700110128       BEGSR CtrRpv;
664800110128
664900110128         werrRpv = *off;
665000110128         clear oldrpv;
665100110128         clear wlnpRpv;
665200110128         clear wctsRpv;
665300110128         clear wsglRpv;
665400110128
665500110128       //?Leggo tutto il dettaglio tariffario per controllare il
665600110128       //?rapporto peso volume
665700110128         IF  not *in50;
665800110128           setll (v1cksc : vctr : prg) TITAD04L;
665900110128           reade(n) (v1cksc : vctr : prg) TITAD04L;
666000110128         ENDIF;
666100110128         IF  *in50;
666200110128           setll (v1cksc : vctr : prg) TIOFD01L;
666300110128           reade(n) (v1cksc : vctr : prg) TIOFD01L;
666400110128         ENDIF;
666500110128
666600110128         DOW not %eof;
666700110201         IF  TADatb = *blanks;
666800110128
666900110128           IF  oldrpv = 0;
667000110128             oldrpv = TADrpv;
667100110128           ENDIF;
667200110128
667300110128           IF  TADrpv <> OLDrpv;
667400110128             werrRpv = *on;
667500110128             wlnpRpv = TADlnp;
667600110128             wctsRpv = TADcts;
667700110128             wsglRpv = TADsgl;
667800110202             leave;
667900110128           ENDIF;
668000110128
668100110128           oldrpv = TADrpv;
668200110201         ENDIF;
668300110128
668400110128           IF  not *in50;
668500110128             reade(n) (v1cksc : vctr : prg) TITAD04L;
668600110128           ENDIF;
668700110128           IF  *in50;
668800110128             reade(n) (v1cksc : vctr : prg) TIOFD01L;
668900110128           ENDIF;
669000110128         ENDDO;
669100110128
669200110128       ENDSR;
669300110128      /end-free
669400110128
669500930630     C*
669600930630     C*--- CARICAMENTO SUBFILE IN AVANTI -----------------------------*
669700920526     C     CARSFA        BEGSR
669800921218     C                   SETON                                        91
669900900925     C                   Z-ADD     SAVREC        NRR               5 0
670000000000     C** PULIZIA SUBFILE AGGIORNAMENTO
670100900925     C     SOPRA         TAG
670200061011     C  n50KTAM2         READE     TITAD000                               33
670300061011     c   50ktam2         reade     tiofd000                               33
670400900924     C* FINE SCORRIMENTO
670500940524     C  N33TADATB        IFEQ      'A'
670600900925     C                   GOTO      SOPRA
670700900925     C                   END
670800900925     C*
670900940524     C   33              DO
671000930224     C     NRR           IFEQ      0
671100930224     C                   SETOFF                                       21
671200930224     C                   ELSE
671300900924     C                   MOVEL     KE            KI
671400900924     C                   MOVEL     CTSE          CTSI
671500940526     C*
671600940526     C                   MOVEL     MSG(32)       VCCMSG
671700940526     C                   SETON                                        28
671800930224     C                   END
671900900924     C                   GOTO      ENDCAR
672000900924     C                   END
672100900924     C*
672200900924     C                   Z-ADD     0             B                 3 0
672300900924     C                   DO        *HIVAL
672400020207      *
672500020207     C                   CLEAR                   AT0CTS
672600900924     C*
672700900924     C     TADATB        IFNE      'A'
672800000000     C                   ADD       1             NRR
672900900924     C                   ADD       1             B
673000900924     C     B             IFEQ      1
673100930630     C** KIAVE INIZIALE
673200000000     C                   MOVEL     KIAVE         KI
673300000000     C                   MOVEL     TADCTS        CTSI              2
673400000000     C                   END
673500930630     C*
673600930630     C** DECODIFICA CODICE TASSAZIONE
673700000000     C                   Z-ADD     1             A
673800940524     C     TADCTS        LOOKUP    CCT(A)                                 35
673900940524     C   35              MOVEL     DCT(A)        DESCTS
674000940524     C  N35              MOVEL     *BLANKS       DESCTS
674100020207      *
674200020207      * Se tariffa FedEx devo utilizzare solo i codici tariffa
674300020207      * previsti per FedEx
674400020207     C     V1CNET        IFEQ      'F'
674500020207     C     UTF(A)        ANDNE     'S'
674600020207     C                   MOVE      RIMM          AT0CTS
674700020207     C     VCCMSG        IFEQ      *BLANKS
674800020207     C                   SETON                                        28
674900020207     C                   MOVEL     MSG(100)      VCCMSG
675000020207     C                   ENDIF
675100020207     C                   ENDIF
675200020207      * Se tariffa non è FedEx non posso utilizzare i codici
675300020207      * tariffa previsti per FedEx
675400020207     C     V1CNET        IFNE      'F'
675500020207     C     UTF(A)        ANDEQ     'S'
675600020207     C                   MOVE      RIMM          AT0CTS
675700020207     C     VCCMSG        IFEQ      *BLANKS
675800020207     C                   SETON                                        28
675900020207     C                   MOVEL     MSG(101)      VCCMSG
676000020207     C                   ENDIF
676100020207     C                   ENDIF
676200990624     C* DECODIFICO LA NAZIONE SE E' DIVERSA DA BLANKS
676300990729     C     TADNAZ        IFEQ      *BLANKS
676400990624     C                   CLEAR                   DESNAZ
676500990624     C                   ELSE
676600990623     C                   MOVEL     '15'          COD
676700990623     C                   MOVEL     *BLANKS       KEY
676800990623     C                   MOVEL     TADNAZ        KEY
676900990623     C     KTAB          CHAIN     TABEL                              32
677000990623     C*
677100990623     C     *IN32         IFEQ      *OFF
677200990623     C     TBLFLG        ANDEQ     ' '
677300990623     C                   MOVEL     TBLUNI        DESNAZ
677400990623     C                   ENDIF
677500990624     C                   ENDIF
677600990623     C***
677700960521     C* RAPPORTO PESO/VOLUME
677800960521     C                   MOVEL     TADRPV        SAVRPV
677900941206     C*
678000941206     C* SE ESISTONO IMPORTO MINIMO O IMPORTO MASSIMO VISUALIZZO
678100941206     C*   LA LETTERA "M" SULLA RELATIVA RIGA DI DETTAGLIO
678200941206    3C     TADMIN        IFGT      0
678300941206     C     TADMAX        ORGT      0
678400941206     C                   SETON                                        13
678500941206    3C                   ENDIF
678600130124      /free
678700130124        clear atatb;
678800130124        clear atitr;
678900130124        clear atino;
679000130124        clear atrpv;
679100130124        clear atmin;
679200130124        clear atmax;
679300130124       //?imposto attributi visualizzazione sui campi che non sono da
679400130124       //?modificare
679500130124       //?lascio libera la modifica se rcd ha data = oggi
679600130124        IF  *in17 and TADdtr <> dateu8;
679700130124          atatb = nPRO;
679800130124          atitr = nPRO;
679900130124          atino = nPRO;
680000130124          atrpv = nPRO;
680100130124          atmin = nPRO;
680200130124          atmax = nPRO;
680300130124        ENDIF;
680400130124      /end-free
680500130124
680600930630     C*
680700940706     C                   WRITE     TA01S03
680800941206     C                   SETOFF                                       13
680900941206     C*
681000900924     C     B             CABEQ     16            ENDAGA
681100900924     C                   END
681200900924     C*
681300061011     C  n50KTAM2         READE     TITAD000                               33
681400061011     c   50ktam2         reade     tiofd000                               33
681500940524     C  N33              END
681600000000     C     ENDAGA        TAG
681700900924     C                   Z-ADD     NRR           REC
681800900924     C                   Z-ADD     NRR           SAVREC            9 0
681900900924     C*
682000000000     C                   MOVEL     KIAVE         KE
682100000000     C                   MOVEL     TADCTS        CTSE              2
682200050701
682300050701      * se errore per + scaglioni in tariffa a spedizione imposto
682400050701      * il msg e accendo l'indicatore
682500050701     c                   If        werr = *On
682600050701     c                   Eval      vccmsg = 'Par ' + wlnp + ' Arrivo ' +
682700050701     c                             wcts
682800050701     c                   If        wcap <> *Blanks
682900050701     c                   Eval      vccmsg = %trim(vccmsg) +
683000050701     c                             ' - c.a.p. ' + wcap
683100050701     c                   EndIf
683200050701     c                   Eval      vccmsg = %trim(vccmsg) +
683300050701     c                             ' presenti più scaglioni'
683400050701     c                   Eval      *In28 = *On
683500050701     c                   EndIf
683600110128
683700110128       //?Se tariffa FedEx e scaglioni con rapporto peso volume
683800110128       //?diverso emetto messaggio di errore
683900110128     c                   IF        werrRpv
684000110128     c                   eval      VCCmsg = 'Par ' + %editc(wlnpRpv:'X') +
684100110128     c                             ' Arrivo ' + wctsRpv +
684200110128     c                             ' Scaglione ' + %editc(wsglRpv:'4') +
684300110128     c                             ' presente P/V diverso'
684400110128     c                   eval      *in28 = *on
684500110128     c                   ENDIF
684600050701
684700000000     C** SE NON CI SONO PIU RECORD DA LEGGERE
684800000000     C** KE = KI
684900900924     C     ENDCAR        ENDSR
685000930630     C*
685100930630     C*--- ELIMINAZIONE SUBFILE SU DISCO -----------------------------*
685200921218     C     ELISF         BEGSR
685300921218     C   91              SETON                                        93
685400940706     C   91              WRITE     TA01C03
685500921218     C   91              SETOFF                                       93
685600921218     C   99              SETON                                        83
685700940706     C   99              WRITE     TA01C05
685800921218     C   99              SETOFF                                       83
685900000000     C                   ENDSR
686000930628     C*
686100930628     C*--- CARICAMENTO TABELLE ---------------------------------------*
686200920526     C     CARTAB        BEGSR
686300940503     C*
686400940503     C****  CODICI TASSAZIONE E DESCRIZIONI: CCT / DCT / ORP  ****
686500020911      * + EST
686600020206     C                   CLEAR                   DSCT
686700940531     C                   Z-ADD     1             K                 5 0
686800930628     C                   MOVEL     'CT'          COD
686900940510     C     KTAB2         SETLL     TABEL
687000940510     C     KTAB2         READE     TABEL                                  32
687100930628     C*
687200940510     C     *IN32         DOWEQ     *OFF
687300930628     C     TBLFLG        IFEQ      ' '
687400000000     C                   MOVEL     TBLKEY        CCT(K)
687500000000     C                   MOVEL     TBLUNI        DCT(K)
687600020206     C                   MOVEL     TBLUNI        DSCT
687700020206     C                   MOVE      §CTCOR        ORP(K)
687800020206     C                   MOVE      §CTCOR        REG(K)
687900020206     C                   MOVEL     §CTNAR        NAR(K)
688000020206     C                   MOVEL     §CTUTF        UTF(K)
688100020911      * Flag estero
688200020911     c                   Movel     §ctest        est(k)
688300050303      * flag utilizzo in gestione tariffe
688400050303     c                   Eval      uta(k) = §ctuta
688500930628     C                   ADD       1             K
688600930628     C                   ENDIF
688700930628     C*
688800940510     C     KTAB2         READE     TABEL                                  32
688900930628     C                   ENDDO
689000930628     C*
689100941124     C****  CODICI TARIFFA    E DESCRIZIONI: CTR/DTR/MIN/TAP/SAP  ****
689200940531     C                   Z-ADD     1             K
689300000000     C                   MOVEL     'TR'          COD
689400940510     C     KTAB2         SETLL     TABEL
689500940510     C     KTAB2         READE     TABEL                                  32
689600930628     C*
689700940510     C     *IN32         DOWEQ     *OFF
689800930628     C     TBLFLG        IFEQ      ' '
689900980505     C                   MOVEL     TBLUNI        DSTR
690000980505     C* CONTROLLO CHE IL TIPO TARIFFA E' ABILITATO ALLA GESTIONE TARIFFE CLIENTI
690100980505     C     §TRUTC        IFEQ      'S'
690200000000     C                   MOVEL     TBLKEY        CTR(K)
690300920525     C                   MOVEL     §TRDES        DTR(K)
690400930628     C* MINIMO TASSABILE PER TIPO TARIFFA
690500940913     C                   Z-ADD     §TRATA        MIN(K)
690600941124     C* IMPORTO TARIFFA  IN % O £ ("S"=TARIFFA IN % - " "=TARIFFA IN £)
690700941108     C                   MOVEL     §TRTAP        TAP(K)
690800950116     C* SCAGLIONE CON DECIMALI O SENZA ("S"=CON DECIM." "=SENZA DECIM.)
690900941124     C                   MOVEL     §TRSAP        SAP(K)
691000980505     C* GESTIONE DEGLI ARROTONDAMENTI IN TARIFFA
691100980505     C                   MOVEL     §TRARR        ARR(K)
691200980505     C* GESTIONE RAPPORTO PESO / VOLUME
691300980505     C                   MOVEL     §TRRPV        RPV(K)
691400021118     C* Flag abilitazione per tariffe FedEx
691500021118     c                   Movel     §TrUtf        Rutf(K)
691600930628     C                   ADD       1             K
691700930628     C                   ENDIF
691800980505     C                   ENDIF
691900930628     C*
692000940510     C     KTAB2         READE     TABEL                                  32
692100930628     C                   ENDDO
692200920525     C*
692300950116     C****  CODICI NAZIONE : NAZ  ****
692400941011     C                   Z-ADD     1             K
692500941011     C                   MOVEL     '15'          COD
692600941011     C     KTAB2         SETLL     TABEL
692700941011     C     KTAB2         READE     TABEL                                  32
692800941011     C*
692900941011     C     *IN32         DOWEQ     *OFF
693000941011     C     TBLFLG        IFEQ      ' '
693100941011     C                   MOVEL     TBLUNI        DS15
693200950116     C* CARICO SOLO NAZIONE ITALIA
693300950116     C     §15ITA        IFEQ      'I'
693400941011     C                   MOVEL     TBLKEY        NAZ(K)
693500941011     C                   ADD       1             K
693600950116     C                   ENDIF
693700941011     C                   ENDIF
693800941011     C*
693900941011     C     KTAB2         READE     TABEL                                  32
694000941011     C                   ENDDO
694100960520     C*
694200960520     C****  AGGANCIO LA TABELLA "1G"  ****
694300960520     C                   MOVE      '1G'          COD
694400960520     C                   MOVEL     '1       '    KEY
694500960520     C     KTAB          CHAIN     TABEL                              32
694600960520     C     *IN32         IFEQ      *OFF
694700960520     C                   MOVEL     TBLUNI        DS1G
694800960520     C                   ELSE
694900960520     C                   CLEAR                   DS1G
695000960520     C                   ENDIF
695100021118
695200021118      * Tabella dati di default per tariffe FedEx
695300021118
695400021118      * aggancio la tabella DFE
695500021118     c                   clear                   ddfe
695600021118     c                   clear                   tibs02ds
695700021118     c                   movel     'C'           t02mod
695800021118     c                   movel     knsif         t02sif
695900021118     c                   movel     'DFE'         t02cod
696000021118     c                   movel(p)  'FED'         t02ke1
696100021118     c                   call      'TIBS02R'
696200021118     c                   parm                    kpjba
696300021118     c                   parm                    tibs02ds
696400021118
696500021118     c                   if        t02err = *blanks
696600021118     c                   movel     t02uni        ddfe
696700021118     c                   endif
696800101013
696900101013      /free
697000101013       //?Carico scatti ISTAT
697100101013         SISsca = 0;
697200101013         clear DIA;
697300101013         setll (SISsca) TISIS01L;
697400101013
697500101013         DOW  not $EoF;
697600101013
697700101013           read TISIS01L;
697800101013
697900101013           IF  %Eof(TISIS01L);
698000101013             $EoF = *on;
698100101013             leave;
698200101013           ENDIF;
698300101013
698400101013           DIA(SISsca) = SISdata;
698500101013
698600101013         ENDDO;
698700101013
698800101013      /end-free
698900101013
699000050530     c* Carico tabella tariffe particolari in scadenza, da eliminare
699100050530     C                   MOVE      '1P'          COD
699200050530     c                   clear                   xx                4 0
699300050530     c     ktab2         setll     tabel00f
699400050530     c     ktab2         reade     tabel00f
699500050530     c                   dow       not %eof(tabel00f)
699600050530     c                   if        tblflg=' '
699700050530     c                   movel     tbluni        ds1p
699800050530     c                   if        §1peli>*zeros
699900050530     c                   add       1             xx
700000050530     c                   movel     tblkey        tpareli(xx)
700100050530     c                   movel     §1peli        tpardat(xx)
700200050530     c                   endif
700300050530     c                   endif
700400050530     c
700500050530     c     ktab2         reade     tabel00f
700600050530     c                   enddo
700700040915     c*
700800031118      *
700900000000     C                   ENDSR
701000051108
701100051108      *--------------------------------------------------------------------*
701200051108      * CONFERMA ANNULLAMENTO OFFERTA/TARIFFA
701300051108      *--------------------------------------------------------------------*
701400051108     c     Sr_Confann    BegSr
701500051108
701600051108     c   50              Eval      w11tip = 'OFFERTA'
701700051108     c  n50              Eval      w11tip = 'TARIFFA'
701800051108     c                   Clear                   w11sino
701900051108     c                   Do        *Hival
702000051108     c                   Exfmt     ta01w11
702100051108     c   kf              Leave
702200051108     c                   EndDo
702300051108
702400051108     c                   EndSr
702500051108
702600930628     C*
702700940705     C*--- ANNULLAMENTO TOTALE DELLA TARIFFA -------------------------*
702800000000     C     ANNTAR        BEGSR
702900931118     C*
703000941012     C* SOLO SE SONO IN FILIALE EMETTO VIDEATA PER STAMPA LETTERA DI
703100941012     C*   ANNULLAMENTO TARIFFA
703200941012    1C     SIMFEL        IFNE      0
703300940310     C                   MOVE      '4'           PA2FLG
703400920526     C                   EXSR      STAMPA
703500941012    1C                   ENDIF
703600901026     C*
703700940511     C****  A N N U L L O   N O T E  ****
703800940511     C* SE ESISTE ALTRO PROGRESSIVO LASCIO LE NOTE ALTRIMENTI LE DELETO
703900941012     C                   Z-ADD     0             ANN               5 0
704000940615     C  N50KTAM          SETLL     TNTAM000
704100100407     C   50KTAM          SETLL     TNofm000
704200940615     C  N50KTAM          READE     TNTAM000                               21
704300100407     C   50KTAM          READE     TNofm000                               21
704400920526     C*
704500100407    1C     *IN21         DOWEQ     *OFF
704600920526     C     TAMATB        IFEQ      ' '
704700941012     C                   ADD       1             ANN
704800940705     C                   ENDIF
704900061011     C  n50KTAM          READE     TNTAM000                               21
705000100407     C   50KTAM          READE     TNofm000                               21
705100941012    1C                   ENDDO
705200920526     C*
705300941012     C* 18 ON  - SONO IN FILIALE
705400941012     C* ANN < 2  --->  NON ESISTONO ALTRE TARIFFE PER QUEL CLIENTE
705500941012     C*                QUINDI CANCELLO LE NOTE
705600941012    1C   18ANN           IFLT      2
705700920526     C                   MOVEL     CCC           NOTKEY
705800940526     C                   MOVE      V1CKSC        NOTKEY
705900941012     C  N50              MOVEL     'C'           APPL
706000100407     C   50              MOVEL     'V'           APPL
706100100407     c   85              movel     'T'           appl
706200920526     C                   MOVEL     VCTR          NOTKE2
706300920526     C*
706400940615     C     KNTC          SETLL     TFNTC
706500920526     C     KNTC          READE     TFNTC                                  21
706600920526     C*
706700941012    2C     *IN21         DOWEQ     *OFF
706800040806     c                   delete    tfntc
706900920526     C     KNTC          READE     TFNTC                                  21
707000941012    2C                   ENDDO
707100941012    1C                   ENDIF
707200920526     C*
707300970117     C****  ANNULLO FILE DATI TIPO: TNETC/TNETT/TNETS  ****
707400970122     C                   CLEAR                   DSTA47
707500970122     C                   MOVEL     '1'           WTA47             1
707600970117     C     *IN50         IFEQ      *OFF
707700970122     C                   MOVEL     '1'           D47TUP
707800970122     C                   MOVEL     'T'           D47CTO
707900970117     C                   ELSE
708000970122     C                   MOVEL     '2'           D47TUP
708100970122     C                   MOVEL     'O'           D47CTO
708200970117     C                   ENDIF
708300050127     C                   MOVEL     'S'           D47DSF
708400970122     C                   Z-ADD     V1CKSC        D47KSC
708500970122     C                   Z-ADD     VCTR          D47CTR
708600970122     C                   Z-ADD     PRG           D47PRG
708700970122     C                   MOVEL     DSTA47        KPJBU
708800970122     C                   CALL      'TNTA47R'
708900970117     C                   PARM                    KPJBA
709000970117     C*
709100950105     C****  ANNULLO TARIFFE GIACENZA  ****
709200990611     C  N50KTAM2         SETLL     TITGC000
709300990611     C   50KTAM2         SETLL     TIOGC000
709400990611     C  N50KTAM2         READE     TITGC000                               21
709500990611     C   50KTAM2         READE     TIOGC000                               21
709600950105     C*
709700950105    1C     *IN21         DOWEQ     *OFF
709800950105     C* TARIFFA
709900950105    2C  N50TGCATB        IFEQ      ' '
710000040914     C                   Z-ADD     dateu8        TGCDTR
710100950105     C                   MOVEL     *BLANKS       TGCFTR
710200950105     C                   MOVEL     'A'           TGCATB
710300990611     C                   UPDATE    TITGC000
710400030714      *
710500950105    2C                   ENDIF
710600950105     C* OFFERTA
710700990611     C   50              DELETE    TIOGC000
710800950105     C*
710900990611     C  N50KTAM2         READE     TITGC000                               21
711000990611     C   50KTAM2         READE     TIOGC000                               21
711100950105    1C                   ENDDO
711200950105     C*
711300940511     C****  ANNULLO DETTAGLIO TARIFFE PARTICOLARI  ****
711400061011     C  n50KTAM2         SETLL     TITPD000
711500061011     c   50ktam2         setll     tiopd000
711600061011     C  n50KTAM2         READE     TITPD000                               21
711700061011     c   50ktam2         reade     tiopd000                               21
711800931118     C*
711900941012    1C     *IN21         DOWEQ     *OFF
712000931118     C* TARIFFA
712100941012    2C  N50TPDATB        IFEQ      ' '
712200040914     C                   Z-ADD     dateu8        TPDDTR
712300931118     C                   MOVEL     *BLANKS       TPDFTR
712400931118     C                   MOVEL     'A'           TPDATB
712500990601     C                   UPDATE    TITPD000
712600941012    2C                   ENDIF
712700931118     C* OFFERTA
712800061011     C   50              DELETE    TIoPD000
712900931118     C*
713000061011     C  n50KTAM2         READE     TITPD000                               21
713100061011     c   50ktam2         reade     tiopd000                               21
713200941012    1C                   ENDDO
713300931118     C*
713400940511     C****  ANNULLO TESTATA   TARIFFE PARTICOLARI  ****
713500061011     C  n50KTAM2         SETLL     TITPT000
713600061011     c   50ktam2         setll     tiopt000
713700061011     C  n50KTAM2         READE     TITPT000                               21
713800061011     c   50ktam2         reade     tiopt000                               21
713900931118     C*
714000941012    1C     *IN21         DOWEQ     *OFF
714100931118     C* TARIFFA
714200941012    2C  N50TPTATB        IFEQ      ' '
714300040914     C                   Z-ADD     dateu8        TPTDTR
714400931118     C                   MOVEL     *BLANKS       TPTFTR
714500931118     C                   MOVEL     'A'           TPTATB
714600990601     C                   UPDATE    TITPT000
714700030714      *
714800941012    2C                   ENDIF
714900931118     C* OFFERTA
715000061011     C   50              DELETE    TIoPT000
715100931118     C*
715200061011     C  n50KTAM2         READE     TITPT000                               21
715300061011     c   50ktam2         reade     tiopt000                               21
715400941012    1C                   ENDDO
715500931118     C*
715600940511     C****  A N N U L L O   D E T T A G L I O  ****
715700061011     C  n50KTAM2         SETLL     TITAD000
715800061011     c   50ktam2         setll     tiofd000
715900061011     C  n50KTAM2         READE     TITAD000                               21
716000061011     c   50ktam2         reade     tiofd000                               21
716100931118     C*
716200941012    1C     *IN21         DOWEQ     *OFF
716300931118     C* TARIFFA
716400941012    2C  N50TADATB        IFEQ      ' '
716500130124     C*********          Z-ADD     dateu8        TADDTR
716600940511     C                   MOVEL     *BLANKS       TADFTR
716700931118     C                   MOVEL     'A'           TADATB
716800990601     C                   UPDATE    TITAD000
716900941012    2C                   ENDIF
717000931118     C* OFFERTA
717100061011     C   50              DELETE    TIofD000
717200931118     C*
717300061011     C  n50KTAM2         READE     TITAD000                               21
717400061011     c   50ktam2         reade     tiofd000                               21
717500941012    1C                   ENDDO
717600931118     C*
717700940705     C****  A N N U L L O   T E S T A T A      ****
717800061011     C  n50KTAM2         CHAIN     TNTAM000                           21
717900061011     c   50ktam2         chain     tnofm000                           21
718000941012    1C     *IN21         IFEQ      *OFF
718100941012     C* TARIFFA
718200981111    2C     *IN50         IFEQ      *OFF
718300960509     C                   Z-ADD     DATEU8        TAMDUV
718400940511     C                   MOVEL     *BLANKS       TAMFTR
718500040913     C                   Z-ADD     DATEU8        TAMDTR
718600000000     C                   MOVEL     'A'           TAMATB
718700940615     C                   UPDATE    TNTAM000
718800160209      /free
718900160209       //?richiamo pgm per cancellare:
719000160209       //?file Tariffe Inserite
719100160209         clear TRSM90DS;
719200160209         ISM90ksc = TAMksc;
719300160209         ISM90ctr = TAMctr;
719400160209         ISM90prg = TAMprg;
719500160209         trsm90r (kpjba:TRSM90DS);
719600160209      /end-free
719700160209
719800030714      *
719900981111   X2C                   ELSE
720000981111     C* OFFERTA
720100981111     C**
720200061011     C                   DELETE    Tnofm000
720300981111     C**
720400981111     C* VERIFICO SE IL CLIENTE HA ALTRE OFFERTE
720500981111     C                   CLEAR                   WAGG
720600070522     C     V1CKSC        CHAIN(n)  TNofm000                           21
720700981111    3C     *IN21         DOWEQ     *OFF
720800981111    4C     TAMFIE        IFEQ      WFIE
720900981111     C* NON FACCIO NULLA PERCHE' C'E' UN'ALTRA OFFERTA PRESENTE
721000981111     C                   SETON                                        21
721100981111     C                   MOVEL     'N'           WAGG              1
721200090507      * appena trovo una tariffa dello stesso tipo dell'annullata
721300090507      * valorizzo il campo ed esco dal loop
721400090507      * per evitare di anadare sopra al campo WAGG
721500090507      * con una 'S'
721600090507     c                   leave
721700981111   X4C                   ELSE
721800981111     C* TROVATA UNA DI ALTRO TIPO MEMORIZZO
721900981111     C                   MOVEL     'S'           WAGG
722000070522     C     V1CKSC        READE(n)  TNofm000                               21
722100981111    4C                   ENDIF
722200981111    3C                   ENDDO
722300100421      * trattative
722400101104     c                   If        *in85
722500100421      ** TIVOF
722600100712      * Non cancello mai il TIVOF ma imposto esito = ad *
722700100712      * Così evitiamo di cancellare delle offerte non considerate nelle nuove
722800100712      * statistiche
722900100421     c     ktam2         Chain     tivof01l
723000100421     c                   If        %found(tivof01l)
723100100421      * verifico data presentazione
723200100712     c****               movel     vofdpo        w0060
723300100712     c****               if        w0060 <= §sdfstcu
723400100421     c                   eval      vofeso = '*'
723500100421     c                   update    tivof000
723600100712     c****               else
723700100712     c*****              delete    tivof000
723800100712     c***                endif
723900100421     c                   endif
724000100407
724100100421     c                   endif
724200981111    2C                   ENDIF
724300981111    1C                   ENDIF
724400100408
724500100408      /free
724600100408       //?Se note tariffe a livello cliente elimino se non ci sono più
724700100408       //?tariffe/offerte
724800100408       exsr sr_DelNote10;
724900100408      /end-free
725000030716      *
725100030716      * A N N U L L O   S T O R I C O   TITAV01L
725200030716      *
725300050603     c                   if        not *in50
725400030716     C     KTAM2         setll     TITAV01L
725500030716      *
725600030716     C                   do        *hival
725700030716      *
725800030716     C     KTAM2         reade     TITAV01L
725900030716      *
726000030716     C                   if        %eof(TITAV01L)
726100030716     c                   leave
726200030716     c                   endif
726300030716      *
726400030716     c                   eval      TAVatb = 'A'
726500030716     c                   clear                   TAVftr
726600040914     c                   z-add     dateu8        TAVdtr
726700030716      *
726800030716     c                   update    titav
726900030716      *
727000030716     c                   enddo
727100040927      * scrivo un nuovo record di TITAV (annullato) per l'annullamento
727200040927      * della tariffa
727300040927     c                   Eval      annultar = 'S'
727400040927     c                   eval      tavtrd = 'TES'
727500040927     c                   eval      tavcta = 'A'
727600040927     c                   exsr      SUB_STORICO
727700040927     c                   Clear                   annultar
727800050603     c                   endif
727900930223     C*
728000921218     C                   ENDSR
728100100407
728200100407      /free
728300100408       //---------------------------------------------------------------
728400100408       //?Deleto note 10.
728500100408       //---------------------------------------------------------------
728600100407       BEGSR sr_DelNote10;
728700100407
728800100407       //?Provo a cercare note a livello cliente senza niente in key 2
728900100407         clear NOTke2;
729000100408         NTCtnt = '10';
729100100408         setll (appl:notkey:notke2:ntctnt) TFNTC01L;
729200100407         IF  %equal(TFNTC01L);
729300100407         //?ci sono note tariffa a livello cliente, controllo se ci sono
729400100407         //?controllo se ci sono ancora tariffe
729500100407           IF  not *in50;
729600100407             setll v1cksc TNTAM01L;
729700100407             reade v1cksc TNTAM01L;
729800100407             DOW  not %eof(TNTAM01L);
729900100407               IF  tamatb = ' ';
730000100407                 leavesr;
730100100407               ENDIF;
730200100407               reade v1cksc TNTAM01L;
730300100407             ENDDO;
730400100407           ENDIF;
730500100407         //?controllo se ci sono ancora offerte
730600100407           IF  *in50;
730700100407             setll v1cksc TNOFM01L;
730800100407             reade v1cksc TNOFM01L;
730900100407             DOW  not %eof(TNOFM01L);
731000100407               IF  tamatb = ' ';
731100100407                 leavesr;
731200100407               ENDIF;
731300100407               reade v1cksc TNOFM01L;
731400100407             ENDDO;
731500100407           ENDIF;
731600100407         //?se arrivo qua vuol dire che non ho trovato ne tariffe ne offerte
731700100407         //?posso cancellare le note tariffa a livello cliente
731800100408           reade (appl:notkey:notke2:ntctnt) TFNTC01L;
731900100407           DOW  not %eof(TFNTC01L);
732000100407             delete TFNTC;
732100100408             reade (appl:notkey:notke2:ntctnt) TFNTC01L;
732200100407           ENDDO;
732300100407         ENDIF;
732400100407
732500100407       ENDSR;
732600100407      /end-free
732700931118     C*
732800940705     C*--- MUOVO CAMPI DA VIDEO A TNTAM ------------------------------*
732900941012     C     RIETAM        BEGSR
733000980219     C*  VALORE POSITIVO DEI CAMPI NUMERI A VIDEO
733100980219     C                   MLLZO     1             COMDDT
733200980219     C                   MLLZO     1             COMDST
733300101013     C                   MLLZO     1             V1Crct
733400980219     C                   MLLZO     1             V1CPRI
733500980219     C                   MLLZO     1             V1CPAG
733600041129     C                   MLLZO     1             V1CPpa
733700980219     C                   MLLZO     1             V1CKPM
733800980219     C                   MLLZO     1             V1CMPC
733900980219     C                   MLLZO     1             V1CATA
734000980219     C                   MLLZO     1             V1CARL
734100980219     C                   MLLZO     1             V1CARF
734200980219     C                   MLLZO     1             V1CARO
734300980219     C                   MLLZO     1             V2CFIS
734400940518     C*
734500900327     C                   Z-ADD     COMDDT        TAMDDT
734600900327     C                   Z-ADD     COMDST        TAMDST
734700940526     C                   MOVE      V1CCTR        TAMCTR
734800940526     C                   MOVEL     V1CDCV        TAMDCV
734900101013     C                   Z-ADD     V1Crct        TAMrct
735000940705     C                   Z-ADD     V1CPRI        TAMPRI
735100980504     C                   Z-ADD     *ZEROS        TAMDIF
735200940705     C                   Z-ADD     V1CPAG        TAMPAG
735300041129     C                   Z-ADD     v1cppa        TAMPPA
735400980504     C                   Z-ADD     *ZEROS        TAMPAM
735500980504     C                   Z-ADD     *ZEROS        TAMMPA
735600980504     C                   Z-ADD     *ZEROS        TAMMPM
735700940526     C                   MOVEL     V1CTSP        TAMTSP
735800940526     C                   MOVEL     V1CBAP        TAMBAP
735900940526     C                   MOVEL     V1CFTP        TAMFTP
736000150923     c                   IF        V1Ctpr = 'N'
736100150923     c                   eval      TAMtpr = 'NO'
736200150923     c                   ELSE
736300150923     c                   clear                   TAMtpr
736400150923     c                   ENDIF
736500980505     C* VALORIZZO TAMFLO   MODO APPLICAZIONE TARIFFA IN FATTURA
736600980505     C* IN CASO DI IMMISSIONE PULISCO TAMFLO
736700980505     C   01              CLEAR                   TAMFLO
736800980505     C                   MOVE      TAMFLO        DSTA01
736900980505     C                   MOVE      V1CMAT        §TAF02
737000020201     C     V1CNET        IFEQ      'D'
737100020130     C                   MOVE      'S'           §TADPD
737200020130     C                   ENDIF
737300020201     C     V1CNET        IFEQ      'F'
737400020130     C                   MOVE      'S'           §TAFED
737500020130     C                   ENDIF
737600990617     C* VALORIZZO IL CAMPO DELLA DIVISA
737700990617     C                   MOVE      V1CDIV        §TADIV
737800020109     C*
737900020109     C                   MOVE      V1CPCA        §TAPCA
738000020109     C                   MOVE      V1CTAP        §TATAP
738100040601     c                   Movel     V2cbva        §tabva
738200050914     c                   Movel     v1covb        §taovb
738300020109     C*
738400980505     C                   MOVE      DSTA01        TAMFLO
738500940518     C*
738600940705     C* VEDO SE KG. O COLLI PER CALCOLO VOLUME AUTOMATICO
738700940518     C                   Z-ADD     0             TAMKPM
738800940518     C                   MOVEL     *BLANKS       TAMTIN
738900940526     C     V1CKPM        IFNE      0
739000940518     C                   MOVEL     'K'           TAMTIN
739100940526     C                   MOVEL     V1CKPM        TAMKPM
739200940518     C                   END
739300940526     C     V1CMPC        IFNE      0
739400940518     C                   MOVEL     'C'           TAMTIN
739500940526     C                   MOVEL     V1CMPC        TAMKPM
739600940518     C                   END
739700940518     C*
739800940526     C                   Z-ADD     V1CATA        TAMATA
739900940526     C                   Z-ADD     V1CARL        TAMARL
740000940526     C                   Z-ADD     V1CARF        TAMARF
740100940526     C                   Z-ADD     V1CARO        TAMARO
740200940705     C                   MOVEL     V2CFTM        TAMFTM
740300940705     C                   MOVEL     V2CFTS        TAMFTS
740400940526     C                   MOVEL     V1CFIE        TAMFIE
740500941108     C                   MOVEL     V1CFIE        WFIE
740600940705     C                   MOVEL     V2CNAS        TAMNAS
740700940705     C                   MOVEL     V2CNOT        TAMNOT
740800940705     C                   MOVEL     V2CFIS        TAMFIS
740900940705     C                   MOVEL     V2CSIS        TAMSIS
741000940804     C                   MOVEL     V2CFVD        TAMFVD
741100940705     C                   MOVEL     V2CGCA        TAMGCA
741200940705     C                   MOVEL     V2CGMA        TAMGMA
741300940705     C                   MOVEL     V2CGGA        TAMGGA
741400940705     C                   MOVEL     V2CGVA        TAMGVA
741500970128     C                   MOVEL     V2CBAM        TAMBAM
741600120319     c                   IF        v2Internet = 'NO'
741700120319     c                   eval      TAMeds = 'N'
741800120319     c                   ELSE
741900120319     c                   clear                   TAMeds
742000120319     c                   ENDIF
742100120529
742200120529       //?Se tariffa bloccata devo impostare a 'N' il flag
742300120529       //?visualizza su internet
742400150923     c                   IF        TAMbap = 'B' and
742500120529     c                             TAMbap <> sav_TAMbap
742600120529     c                   eval      TAMeds = 'N'
742700120529     c                   ENDIF
742800120529
742900060307     c                   eval      tamfmp = v2cfmp
743000990621     C* PULISCO  I CAMPI CHE NON VENGONO PIU' UTILIZZATI
743100990621     C                   CLEAR                   TAMARS
743200990621     C                   CLEAR                   TAMLAS
743300990621     C                   CLEAR                   TAMLRC
743400990621     C                   CLEAR                   TAMRAT
743500990621     C                   CLEAR                   TAMSC2
743600990617     C* VALORIZZO I CAMPI DELLE SPEDIZIONI MINIME E MASSIME AFFIDATECI
743700990824     C                   Z-ADD     V1CMIS        TAMLRC
743800990824     C                   Z-ADD     V1CMAS        TAMLAS
743900940518     C*
744000900327     C                   ENDSR
744100940518     C*
744200960109     C*--- MUOVO CAMPI DA TNTAM A VIDEO - 1° VIDETATA ----------------*
744300941012     C     RIED01        BEGSR
744400991021     C* TIPO TARIFFA  (SOLO PER COPIA DA OFFERTA)
744500991021     C                   MOVEL     TAMCTR        TIPTAR            1
744600940518     C*
744700940531     C****  DATA SCADENZA TARIFFA  ****
744800940518     C     TAMDDT        IFGT      0
744900940518     C     TAMDST        IFGT      0
745000940729     C                   Z-ADD     TAMDST        DATA
745100940729     C                   Z-ADD     MM            GGMM              4 0
745200940729     C                   MOVEL     GG            GGMM
745300940729     C                   Z-ADD     AA            V1CDST
745400940729     C                   MOVEL     GGMM          V1CDST
745500940518     C                   ENDIF
745600940518     C*
745700940531     C****  DATA DECORRENZA TARIFFA  ****
745800940729     C                   Z-ADD     TAMDDT        DATA
745900940729     C                   Z-ADD     MM            GGMM              4 0
746000940729     C                   MOVEL     GG            GGMM
746100940729     C                   Z-ADD     AA            V1CDDT
746200940729     C                   MOVEL     GGMM          V1CDDT
746300940518     C                   ENDIF
746400940518     C*
746500940531     C****  DATA ULTIMA VARIAZIONE  ****
746600940518     C     TAMDUV        IFGT      0
746700940729     C                   Z-ADD     TAMDUV        DATA
746800940729     C                   Z-ADD     MM            GGMM              4 0
746900940729     C                   MOVEL     GG            GGMM
747000940729     C                   Z-ADD     AA            V1CDUV
747100940729     C                   MOVEL     GGMM          V1CDUV
747200940518     C                   ENDIF
747300940518     C*
747400940518     C                   MOVEL     TAMDS         COMODO
747500030716      * se la ds COMODO$ non è valorizzata
747600030716     C                   if        COMODO$ = *blanks
747700030716     c                   movel     tamds         COMODO$
747800030716     c                   endif
747900030716      *
748000940518     C                   Z-ADD     TAMLAS        COMLAS
748100940518     C*
748200940526     C                   Z-ADD     PRG           V1CPRG
748300940526     C                   MOVE      TAMCTR        V1CCTR
748400940526     C                   MOVE      TAMDCV        V1CDCV
748500101013     C                   Z-ADD     TAMrct        V1Crct
748600101013     C                   Z-ADD     TAMrct        W_V1Crct
748700940705     C                   Z-ADD     TAMPRI        V1CPRI
748800101013    1C     V1Crct        IFGT      0
748900031118      *
749000031118      * visualizzo la decorrenza
749100101013     c                   z-add     dia(v1crct)   g02INV
749200031118     c                   movel     '3'           g02err
749300031118     c                   CALL      'XSRDA8'
749400031118     c                   PARM                    WLBDAT
749500031118      * UDATE A 8 IN GG/MM/AAAA
749600031118     c                   z-add     g02dat        v1diST
749700031118      *
749800031118     c                   endif
749900031118      *
750000940705     C                   Z-ADD     TAMPAG        V1CPAG
750100041129     C                   Z-ADD     TAMPpa        V1CPpa
750200980505     C                   MOVEL     TAMFLO        DSTA01
750300980505     C                   MOVE      §TAF02        V1CMAT
750400020130     C     §TADPD        IFEQ      'S'
750500020201     C                   MOVE      'D'           V1CNET
750600020130     C                   ENDIF
750700020130     C     §TAFED        IFEQ      'S'
750800020201     C                   MOVE      'F'           V1CNET
750900020130     C                   ENDIF
751000020215     C*
751100020215     C                   MOVE      V1CNET        WNET
751200020109     C*
751300020109     C                   MOVE      §TAPCA        V1CPCA
751400020109     C                   MOVE      §TATAP        V1CTAP
751500050914     c                   Movel     §taovb        v1covb
751600150922
751700150923     c                   IF        TAMtpr = 'NO'
751800150923     c                   eval      V1Ctpr = 'N'
751900150923     c                   ELSE
752000150923     c                   clear                   V1Ctpr
752100150923     c                   ENDIF
752200020109     C*
752300991021     C* DIVISA
752400991022     C                   MOVE      §TADIV        V1CDIV
752500990617     C                   MOVEL     'CV'          COD
752600990617     C                   MOVEL     *BLANKS       KEY
752700990622     C                   MOVEL     V1CDIV        KEY
752800990617     C     KTAB          CHAIN     TABEL                              32
752900990617     C  N32              MOVEL     TBLUNI        DSCV
753000980310     C*
753100940526     C                   MOVE      TAMTSP        V1CTSP
753200020215     C                   MOVE      TAMTSP        WTSP
753300940811     C                   MOVEL     '5E'          COD
753400940811     C                   MOVEL     *BLANKS       KEY
753500940811     C                   MOVEL     V1CTSP        KEY
753600940811     C     KTAB          CHAIN     TABEL                              32
753700090608     C  N32              MOVEL     TBLUNI        ds5e
753800090608     C  N32              MOVEL     §5ed08        V1DTSP
753900940811     C   32              CLEAR                   V1DTSP
754000940811     C*
754100120529     c                   eval      sav_TAMbap = TAMbap
754200130604     c                   eval      sav_TAMtsp = TAMtsp
754300940526     C                   MOVE      TAMBAP        V1CBAP
754400940526     C                   MOVE      TAMFTP        V1CFTP
754500940518     C     TAMTIN        IFEQ      'K'
754600940526     C                   Z-ADD     TAMKPM        V1CKPM
754700940518     C                   END
754800940518     C     TAMTIN        IFEQ      'C'
754900940526     C                   MOVEL     TAMKPM        V1CMPC
755000940518     C                   END
755100991021     C*
755200940913     C                   Z-ADD     TAMATA        V1CATA
755300940705     C                   Z-ADD     TAMARL        V1CARL
755400940526     C                   Z-ADD     TAMARF        V1CARF
755500940705     C                   Z-ADD     TAMARO        V1CARO
755600010330     C*
755700940518     C*
755800940526     C                   MOVEL     TAMFIE        V1CFIE
755900941108     C                   MOVEL     TAMFIE        WFIE
756000990617     C* VALORIZZO I CAMPI DELLE SPEDIZIONI MINIME E MASSIME AFFIDATECI
756100990824     C                   Z-ADD     TAMLRC        V1CMIS
756200990824     C                   Z-ADD     TAMLAS        V1CMAS
756300060307      * imposto qua il flag rinuncia AC Base xchè se F21 da prima videata devo
756400060307      * passarlo al pgm delle tariffe particolari
756500060307     c                   movel     tamfmp        v2cfmp
756600080522
756700080522      * controllo se esiste la tariffa particolare fuel
756800080522      * in caso contrario messaggio a video
756900080522     c                   clear                   ds1p
757000080522     c                   eval      cod = '1P'
757100080522     c                   eval      key = 'f '
757200080522     c     ktab          chain     tabel00f
757300080522     c                   if        %found(tabel00f) and tblflg = *blanks
757400080522     c                   eval      ds1p = tbluni
757500080522     c                   endif
757600080522     c                   select
757700080522      * Tariffa con tipo servizio poste
757800080522     c                   when      tamtsp = 'P' and §1pfpt = 'N'
757900080522      * Tariffa con Network DPD
758000080522     c                   when      §tadpd = 'S' and §1pfdp = 'N'
758100080522      * Tariffa con Network FedEx
758200080522     c                   when      §tafed = 'S' and §1pffe = 'N'
758300080522      * Tariffa Italia
758400080522     c                   when      tamfie = 'I' and §1ptco = 'N'
758500080522      * Per tariffa Estero controllo flag §1pfie
758600080522     c                   when      tamfie = 'E' and §1pfie = 'N'
758700080522      * Utilizzabile in tariffa
758800080522     c                   when      §1puta <> 'S'
758900080522     c                   other
759000080522     c                   eval      wtpar = 'f '
759100080522     c  n50ktam7         chain(n)  titpt01l
759200140221     c   50ktam7         chain(n)  tiopt01l
759300080522     c                   if        not %found or tptatb <> *blanks
759400080522     c                   eval      *in28 = *on
759500140220     c                   eval      V1Cmsg = msg(15)
759600080522     c                   endif
759700140220      * se trovo la tariffa Fuel Surcharge devo verificare se la data
759800140220      * di riferimento del prezzo base è > di oggi
759900140224     c                   exsr      ctr_fuel
760000080522    1c                   endsl
760100960109     C*
760200900327     C                   ENDSR
760300900329     C*
760400960109     C*--- MUOVO CAMPI DA TNTAM A VIDEO - 2° VIDETATA ----------------*
760500960109     C     RIED02        BEGSR
760600060303
760700060303     c                   Clear                   at2fmp
760800060303     c                   Clear                   h2riga
760900060303     c                   Clear                   h2colo
761000060303
761100960109     C                   MOVEL     TAMNAS        V2CNAS
761200021118     c                   Eval      *In39 = *Off
761300021118      * Se tariffa FedEx Documenti proteggo il campo della natura merce
761400021118     c                   If        V1cNet = 'F' and DueCtr >= §DfeDalD
761500021118     c                             and DueCtr <= §DfeAlD
761600021118     c                   Eval      *In39 = *On
761700021118     c                   EndIf
761800040601     c                   Movel     §tabva        v2cbva
761900960109     C                   MOVEL     TAMNOT        V2CNOT
762000960109     C                   MOVEL     TAMFIS        V2CFIS
762100960621     C  N50              MOVEL     TAMSIS        V2CSIS
762200960109     C                   MOVEL     TAMFVD        V2CFVD
762300960109     C                   MOVEL     TAMGCA        V2CGCA
762400960109     C                   MOVEL     TAMGGA        V2CGGA
762500960109     C                   MOVEL     TAMGMA        V2CGMA
762600960109     C                   MOVEL     TAMGVA        V2CGVA
762700960109     C                   MOVEL     TAMFTM        V2CFTM
762800960109     C                   MOVEL     TAMFTS        V2CFTS
762900970128     C                   MOVEL     TAMBAM        V2CBAM
763000120319     c                   IF        TAMeds = 'N'
763100120319     c                   eval      v2Internet = 'NO'
763200120319     c                   ELSE
763300120319     c                   clear                   v2Internet
763400120319     c                   ENDIF
763500120319
763600960109     C                   ENDSR
763700960109     C*
763800930630     C*--- LETTURA FILE ----------------------------------------------*
763900900329     C     LEGGI         BEGSR
764000900329     C*
764100900329     C* SE NON COPIO POSIZIONAMENTO  NORMALE
764200101213     C**n50KTAM2         SETLL     TNTAM01l
764300101213     c** 50ktam2         setll     tnofm01l
764400101213     C**n50KTAM2         READE     TNTAM01l                             3601
764500101213     c** 50ktam2         reade     tnofm01l                             3601
764600101213     C  n50KTAM2         chain     TNTAM01l                           0136
764700101213     c   50ktam2         chain     tnofm01l                           0136
764800941007     C*
764900941007     C* 36 ON  - RECORD ALLOCATO
765000950121    1C     *IN36         IFEQ      *ON
765100941012     C                   MOVEL     'A'           PARALL
765200950121   X1C                   ELSE
765300941012     C                   CLEAR                   PARALL
765400941007     C*
765500061010     C  N01TAMATB        COMP      'A'                                    01
765600061010     C  N01              SETON                                        10
765700950121    1C                   ENDIF
765800900329     C** 01  INSERIMENTO
765900900329     C** 10  AGGIORNAMENTO
766000900329     C                   ENDSR
766100900329     C*
766200930630     C*--- STAMPA TARIFFA --------------------------------------------*
766300920525     C     STAMPA        BEGSR
766400920525     C*
766500940310     C                   Z-ADD     PRG           PA2PRG
766600940310     C                   Z-ADD     VCTR          PA2CTR
766700940526     C                   MOVEL     V1DKSC        PA2RGS
766800940310     C* AMBIENTE
766900941013    2C     VFLG          IFNE      '1'
767000940310     C                   MOVEL     'C'           PA2AST
767100940526     C                   Z-ADD     V1CKSC        PA2CLI
767200940311     C                   Z-ADD     0             PA2NRV
767300941013   X2C                   ELSE
767400940310     C                   MOVEL     'V'           PA2AST
767500940526     C                   Z-ADD     V1CKSC        PA2NRV
767600940311     C                   Z-ADD     0             PA2CLI
767700941013    2C                   ENDIF
767800091021      * se trattativa devo impostare new tipo richiamo in pa2ast = 'T'
767900100129      * passo il potenziale e se c'è il cliente
768000091021     c   85              eval      pa2ast = 'T'
768100100112     c   85              eval      pa7cpo = cpox
768200100129     c                   if        *in85 and ksc2 > *zeros
768300100129     c                   eval      pa2cli = %dec(ksc2:7:0)
768400100129     c                   endif
768500101105
768600940311     C                   MOVEL     ' '           PA2TST
768700941013     C*
768800920525     C                   MOVEL     PARAM2        KPJBU
768900920525     C* SE RIF ALTRA TAR DA OFFERTA PRIMA PASSO DA OVRDBF
769000941219     C                   CALL      'FNLV43R'
769100920525     C                   PARM                    KPJBA
769200941013     C*
769300920525     C                   ENDSR
769400920525     C*
769500940510     C*--- CONTROLLO DATE --------------------------------------------*
769600940510     C     CTRDAT        BEGSR
769700940510     C*
769800940510     C                   ADD       1             TAMDST
769900940510     C                   Z-ADD     TAMDST        DATA
770000940510     C* CONTROLLO SE ANNO BISESTILE
770100940510    1C     MM            IFEQ      2
770200940510     C     AA            DIV       4             G02QUA            2 0
770300940510     C                   MVR                     G02RES            1 0
770400940510    2C     G02RES        IFEQ      0
770500940510     C                   Z-ADD     29            XDTF(2)
770600940510    2C                   ENDIF
770700940510    1C                   ENDIF
770800940510     C*
770900940510    2C     GG            IFGT      XDTF(MM)
771000940510     C     GG            SUB       XDTF(MM)      GG
771100940510     C                   ADD       1             MM
771200940510    2C     MM            IFEQ      13
771300940510     C                   ADD       1             AA
771400940510     C                   Z-ADD     1             MM
771500940510    2C                   ENDIF
771600940510    1C                   ENDIF
771700940510     C                   Z-ADD     28            XDTF(2)
771800940510     C*
771900940510     C                   ENDSR
772000980309     C*
772100980309     C*--- CONTROLLO TARIFFA IMPORT ----------------------------------*
772200980309     C     CTRIMP        BEGSR
772300980309     C*
772400980309     C                   MOVE      ' '           IMPORT            1
772500980309     C* CONTROLLO SE LA TARIFFA E' PER L'ITALIA O PER L'ESTERO
772600980309    1C     V1CFIE        IFEQ      'E'
772700980309     C* CONTROLLO SE ESISTE ALMENO UNA LINEA DI PARTENZA ESTERA PER DEFINIRE
772800980309     C* UNA TARIFFA IMPORT
772900980309     C* LEGGO IL DETTAGLIO TARIFFA
773000061011     C  n50KTAM2         SETLL     TITAD000
773100061011     c   50ktam2         setll     tiofd000
773200980309    2C                   DO        *HIVAL
773300061011     C  n50KTAM2         READE     TITAD000                               32
773400061011     c   50ktam2         reade     tiofd000                               32
773500980309    3C     *IN32         IFEQ      *OFF
773600980313     C     TADATB        ANDEQ     ' '
773700980309     C     TADLNP        CHAIN     AZORG01L                           33
773800020312     c                   clear                   og143
773900020312     c                   if        not *in33
774000020312     c                   eval      og143 = orgde3
774100020312     c                   endif
774200020312     c                   if        §ogntw = 'EEX' or §ogntw = 'EUP' or
774300020312     c                             §ogntw = 'FED'
774400020312     c                   eval      *in32 = *on
774500020312     c                   else
774600130318     c***                eval      *in31 = *off
774700130318     c                   eval      wTParEli = *off
774800130318     c                   endif
774900020312
775000980309     C* TARIFFA IMPORT 32 = ON
775100020312     C   32              MOVEL     'Y'           IMPORT
775200980309    3C                   END
775300980309     C*
775400980309    2C  N32              ENDDO
775500980309     C*
775600980309     C* CONTROLLO SE LA TARIFFA PARTICOLARE ASSICURAZIONE PER CONTO E' CORRETTA
775700980309     C* SOLO NEL CASO IN CUI NON SIA STATO RICHIESTO IL CMD 21 MANUTENZIONE
775800980309     C* TARIFFE PARTICOLARI
775900980309     C*
776000980309    2C     *INKV         IFEQ      *OFF
776100980313     C     IMPORT        ANDEQ     'Y'
776200060303     C                   MOVE      'C '          WTPAR
776300070129     C  n50KTAM7         CHAIN(n)  TITPT01L                           33
776400070129     c   50ktam7         chain(n)  tiopt01L                           33
776500980309    3C     *IN33         IFEQ      *OFF
776600980313     C     TPTATB        ANDEQ     ' '
776700980309    4C     TPTAPL        IFNE      'A'
776800980309     C     TPTVLM        OREQ      *ZEROS
776900980309     C                   SETON                                        28
777000980309     C                   MOVEL     MSG(72)       V1CMSG
777100980309    4C                   END
777200980309    3C                   END
777300980309    2C                   END
777400980309     C*
777500980309    1C                   END
777600980309     C*
777700980309     C                   ENDSR
777800000918     C*-------------------------------------------------------------------*
777900000918     C* INGLOBO TARIFFE DI CARTELLO CHE SONO SEMPRE PRESENTI IN TARIFFA
778000000918     C*-------------------------------------------------------------------*
778100000918     C*
778200000918     C     INGLO         BEGSR
778300061011
778400061011     c                   Clear                   Tnta25ds
778500061011     c  n50              Eval      ta25tipo = 'T'
778600061011     c   50              Eval      ta25tipo = 'O'
778700061011     c                   Eval      ta25ksco = v1cksc
778800061011     c                   Move      vctr          ta25ctro
778900061011     c                   Move      prg           ta25prgo
779000061011     c  n50              Eval      ta25tipn = 'T'
779100061011     c   50              Eval      ta25tipn = 'O'
779200061011     c                   Eval      ta25kscn = v1cksc
779300061011     c                   Move      vctr          ta25ctrn
779400061011     c                   Move      prg           ta25prgn
779500061011     c                   Eval      ta25tam = 'N'
779600061011     c                   Eval      ta25fie = v1cfie
779700061011     c                   Eval      ta25tam = 'N'
779800061011     c                   Eval      ta25tad = 'N'
779900061011     c                   Eval      ta25tpt = 'N'
780000061011     c                   Eval      ta25tpd = 'N'
780100061011     c                   Eval      ta25tgc = 'N'
780200061011     c                   Eval      ta25cat = 'N'
780300061011     c                   Eval      ta25inglo = 'S'
780400090917      * imposto se da trattativa
780500110907     c                   eval      %subst(kpjbu:1:1) = partrat
780600110907      * se sono in immissione oppure in aggiunta di una nuova tariffa
780700110907      * dico al TNTA25 di imglobare la tariffa particolare "Q" = ZTL
780800110907      *
780900120403       // -?Da marzo 2012 si ingloba sempre (DV 2236)
781000120403     c******             If        win01 or *in12
781100120403     c******             eval      ta25ztlq = 'S'
781200120403     c******             endif
781300061011
781400061011     c                   Call      'TNTA25R'
781500061011     c                   Parm                    kpjba
781600061011     c                   Parm                    Tnta25ds
781700061011     c                   Parm                    Tnta25tad
781800061011     c                   Parm                    Tnta25tgc
781900061011     c                   Parm                    Tnta25tpd
782000061011     c                   Parm                    Tnta25tpt
782100061011
782200000918     C                   ENDSR
782300080521
782400080521      *-------------------------------------------------------------------*
782500080521      * Integro la tariffa con la tariffa 'f' Fuel
782600080521      *-------------------------------------------------------------------*
782700080521     c     sr_fuel       begsr
782800080521
782900080521     c                   clear                   Tnta25ds
783000080521     c  n50              eval      ta25tipo = 'T'
783100080521     c   50              eval      ta25tipo = 'O'
783200080521     c                   eval      ta25ksco = v1cksc
783300080521     c                   move      vctr          ta25ctro
783400080521     c                   move      prg           ta25prgo
783500080521     c  n50              eval      ta25tipn = 'T'
783600080521     c   50              eval      ta25tipn = 'O'
783700080521     c                   eval      ta25kscn = v1cksc
783800080521     c                   move      vctr          ta25ctrn
783900080521     c                   move      prg           ta25prgn
784000080521     c                   eval      ta25tam = 'N'
784100080521     c                   eval      ta25fie = v1cfie
784200080521     c                   eval      ta25tam = 'N'
784300080521     c                   eval      ta25tad = 'N'
784400080521     c                   eval      ta25tpt = 'N'
784500080521     c                   eval      ta25tpd = 'N'
784600080521     c                   eval      ta25tgc = 'N'
784700080521     c                   eval      ta25cat = 'N'
784800080521     c                   eval      ta25inglo = 'f'
784900090917      * imposto se da trattativa
785000090917     c                   eval      %subst(kpjbu:1:1) = partrat
785100080521
785200080521     c                   call      'TNTA25R'
785300080521     c                   parm                    kpjba
785400080521     c                   parm                    Tnta25ds
785500080521     c                   parm                    Tnta25tad
785600080521     c                   parm                    Tnta25tgc
785700080521     c                   parm                    Tnta25tpd
785800080521     c                   parm                    Tnta25tpt
785900080521
786000080521     c                   endsr
786100080521
786200030715     C*---------------------------------------------------------------*
786300030715     C* ROUTINE DI SCRITTURA FILE STORICO TITAV00F
786400030715     C*---------------------------------------------------------------*
786500030715     C     SUB_STORICO   BEGSR
786600030717      * se sono in offerta oppure sono in immissione non scrivo nulla
786700040927      * non più ora si deve scrivere anche quando viene inserita una nuova
786800040927      * tariffa
786900030717     c                   if        not *in50 and instes = ' '
787000030715      *la prima volta inizializzo i campi del file che vanno sempre bene
787100030715     c                   if        first = ' '
787200030715     C                   eval      TAVksc= v1cksc
787300030715     C                   move      ctr2          TAVctr
787400030715     C                   eval      TAVprg= prg
787500030715      * data variazione
787600030715     C                   eval      TAVdav= dateu8
787700030715     C                   eval      TAVnoj= job_name
787800030715     C                   eval      TAVpru= user
787900030715     C                   eval      first = 'N'
788000030717     c                   endif
788100030717      *
788200030717     c                   clear                   TAVatb
788300040927     c                   If        annultar <> *Blanks
788400040927     c                   Eval      TavAtb = 'A'
788500040927     c                   EndIf
788600030715      * recupero ora di variazione
788700030715     c                   time                    w0120
788800030715     c                   movel     w0120         TAVorv
788900030715      * se non scrivo variazioni alle tariffe particolari pulisco il
789000030715      * codice tariffa particolare
789100030715     c                   if        TAVtrd <> 'PAR'
789200030715     c                   clear                   TAVftc
789300030715     c                   endif
789400040914      *
789500040914     c                   z-add     dateu8        TAVdtr
789600030715      *
789700030715     c                   write     titav000
789800030715     C*
789900030717     c                   endif
790000030717      *
790100030715     C                   ENDSR
790200030715     C***
790300041222     C*---------------------------------------------------------------*
790400041222     C* ROUTINE DI GESTIONE SUBFILE RICHIESTA LINEE PARTENZA DA CREARE
790500041222     C*---------------------------------------------------------------*
790600041222     C     SR_GESLNP     BEGSR
790700041222      *
790800041222      * SE È LA PRIMA VOLTA CHE ENTRO PULISCO IL SUBFILE
790900041222      *
791000041222     c                   IF        W_sbflnp = ' '
791100041222     c                   seton                                        26
791200041222     c                   write     TA01C09
791300041222     c                   setoff                                       26
791400041222     c                   z-add     1             nr2               4 0
791500041222     c                   z-add     1             REC2
791600061012     c                   eval      wforza = '0'
791700041222     c                   endif
791800041222      * zoccolo
791900041222     c                   write     ta01z10
792000041222      *
792100041222     c                   do        *hival
792200041222      *
792300061012     c                   if        nr2 > rec2
792400061012     c                   eval      rec2 = 1
792500061012     c                   endif
792600041222     c                   exfmt     ta01c09
792700041222      *
792800041222     c                   clear                   $msg
792900041222     c                   setoff                                       9828
793000041222      *
793100041222     c                   if        *inkl
793200041222     c                   leave
793300041222     c                   endif
793400041222      *
793500041222      * lettura subfile e controllo
793600041222      *
793700061012     c                   clear                   nr2
793800041222     c                   do        *hival
793900061012     c                   eval      nr2 = nr2 + 1
794000041222      *
794100061012     c     nr2           chain     ta01s09                            30
794200041222      *
794300061012     c                   if        *in30
794400041222     c                   leave
794500041222     c                   endif
794600061012
794700061012     c                   setoff                                       9828
794800041222      *
794900041222      * controllo la linea di partenza
795000041222      *
795100041222     c                   if        vs9lnp > 0
795200041222     c     vs9lnp        chain     azorg01l
795300041222     c                   if        not %found(azorg01l)
795400041222     C                   movel     MSG(22)       $MSG
795500041222     C                   seton                                        9828
795600061012     c                   z-add     nr2           rec2
795700041222     c                   update    ta01s09
795800041222     c                   leave
795900041222     c                   endif
796000041222      * verifico
796100041222     c                   if        orgfva <> ' '  or (orgfag <> 'A' and
796200041222     c                             orgfag <> 'F' and orgfl2 <> 'S')
796300041222     C                   movel     MSG(22)       $MSG
796400041222     C                   seton                                        9828
796500061012     c                   z-add     nr2           rec2
796600041222     c                   update    ta01s09
796700041222     c                   leave
796800041222     c                   endif
796900041222      *
797000041222     c                   movel     orgdes        deslnp
797100061012      * controllo se c'è già un dettaglio tariffario con la linea indicata
797200061012      * messaggio forzabile
797300061012     c                   clear                   wtrov
797400061012     c                   move      vs9lnp        comln2
797500061012     c  n50ktad2         setll     titad000
797600061012     c   50ktad2         setll     tiofd000
797700061012     c  n50ktad2         reade     titad000                               34
797800061012     c   50ktad2         reade     tiofd000                               34
797900061012     c                   dow       not *in34
798000061012     c                   if        tadatb = *blanks
798100061012     c                   eval      *in34 = *on
798200061012     c                   eval      wtrov = '1'
798300061012     c                   else
798400061012     c  n50ktad2         reade     titad000                               34
798500061012     c   50ktad2         reade     tiofd000                               34
798600061012     c                   endif
798700061012     c                   enddo
798800061012     c                   if        wtrov <> *blanks and wforza(nr2) = *off
798900061012     c                   eval      $msg = msg(68)
799000061012     c                   z-add     nr2           rec2
799100061012     c                   seton                                        9828
799200061012     c                   eval      wforza(nr2) = *on
799300061012     c                   update    ta01s09
799400061012     c                   leave
799500061012     c                   endif
799600090608
799700090608      * se la nuova linea di partenza è 950
799800090608    1c                   if        vs9lnp = 950
799900090608      * non posso duplicare se esiste un codice di tassazione IT o EE
800000090608     c                   clear                   wtrov
800100090608     c  n50ktam2         setll     titad04l
800200090608     c   50ktam2         setll     tiofd01l
800300090608    2c                   do        *hival
800400090608     c  n50ktam2         reade     titad04l
800500090608     c   50ktam2         reade     tiofd01l
800600090608     c                   if        %eof
800700090608     c                   leave
800800090608     c                   endif
800900090608     c                   if        tadatb <> *blanks
801000090608     c                   iter
801100090608     c                   endif
801200090608     c                   if        tadcts = 'IT' or tadcts = 'EE'
801300090608     c                   eval      wtrov = '1'
801400090608     c                   leave
801500090608     c                   endif
801600090608    2c                   enddo
801700090608     c                   if        wtrov <> *blanks
801800090608     c                   eval      $msg = msg(121)
801900090608     c                   eval      %subst($msg:36:3) = %editc(tadlnp:'1')
802000090608     c                   z-add     nr2           rec2
802100090608     c                   seton                                        9828
802200090608     c                   update    ta01s09
802300090608     c                   leave
802400090608     c                   endif
802500090608    1c                   endif
802600041222      *
802700041222     c                   else
802800041222      *
802900041222     c                   clear                   deslnp
803000041222      *
803100041222     c                   endif
803200041222      *
803300061012     c                   z-add     nr2           rec2
803400041222     c                   update    ta01s09
803500041222      *
803600041222     c                   enddo
803700041222      *
803800041222     c                   if        *in28
803900041222     c                   iter
804000041222     c                   endif
804100041222      * f6 conferma carico la mia schiera
804200041222     c                   If        *inkf
804300041222     c                   eval      W_sbflnp = '1'
804400041222      * schiera
804500041222     c                   clear                   lnpel
804600041222     c                   z-add     0             nr2
804700041222     c                   z-add     0             po                3 0
804800041222      *
804900041222     c                   do        120           nr2
805000041222     c     nr2           chain     ta01s09
805100041222     c                   if        %found and vs9lnp > 0
805200041222     c                   add       1             po
805300041222     c                   movea     vs9lnp        lnpel(po)
805400041222     c                   If        v8clnn = 0
805500041222     c                   movel     vs9lnp        v8clnn
805600041222     c                   endif
805700041222     c                   endif
805800041222     c                   enddo
805900041222      * se non ho caricato neppure una lnp pulisco W_SBFLNP
806000041222     c                   if        po = 0
806100041222     c                   clear                   W_sbflnp
806200041222     c                   endif
806300041222      *
806400041222     c                   eval      linpar = 'Selezionate più linee'
806500041222     c                   leave
806600041222     c                   endif
806700041222      *
806800041222     c                   enddo
806900041222
807000041222     c                   endsr
807100120314
807200120314      /free
807300120315       //--------------------------------------------------------------
807400120315       //?Imposto i tasti funzionali.
807500120315       //--------------------------------------------------------------
807600120315       BEGSR TastiFun;
807700120315
807800120315       //?Imposto la seconda riga variabile
807900120315         clear wpos;
808000120315         clear wposda;
808100120315         clear wpostf1;
808200120315         clear wpostf2;
808300120315         clear wpostf3;
808400120315         clear wrig;
808500120315         clear wtf1;
808600120315         clear wtf2;
808700120315         clear wtf3;
808800120315
808900120315         tf = 1;
809000120315         FOR tf by 1 to %elem(wTfv);
809100120315
809200120315         //?Abilito F1
809300120315           IF  not *in97 and
809400120315               %subst(wTfv(tf):1:3) = 'F1=';
809500120315             iter;
809600120315           ENDIF;
809700120315
809800120315         //?Abilito Enter
809900120319           IF  (wPrima or wSeconda) and
810000120315               %subst(wTfv(tf):1:3) = 'Ent';
810100120315             iter;
810200120315           ENDIF;
810300120315
810400120315         //?Abilito F2
810500120319           IF  (wQuarta or wTerza or not *in18) and
810600120315               %subst(wTfv(tf):1:3) = 'F2=';
810700120315             iter;
810800120315           ENDIF;
810900120315
811000120315         //?Abilito F6
811100120319           IF  (wPrima or wSeconda or wQuarta or *in15) and
811200120315               %subst(wTfv(tf):1:3) = 'F6=';
811300120315             iter;
811400120315           ENDIF;
811500120315
811600120315         //?Abilito F8 - Note tariffa
811700130905           IF  (wTerza or wQuarta or not *in18) and
811800120315               %subst(wTfv(tf):1:3) = 'F8=';
811900120315             iter;
812000120315           ENDIF;
812100120315
812200120315         //?Abilito F10
812300120315           IF  (wQuarta or (wPrima and not *in10)) and
812400120315               %subst(wTfv(tf):1:3) = 'F10';
812500120315             iter;
812600120315           ENDIF;
812700120315
812800120315         //?Abilito F11
812900120315           IF  (wPrima or wSeconda) and
813000120315               %subst(wTfv(tf):1:3) = 'F11';
813100120315             iter;
813200120315           ENDIF;
813300120315
813400120315         //?Abilito F16
813500130318          // IF  (wTerza or wQuarta or not *in10 or *in17) and
813600130318           IF  (wTerza or wQuarta or not *in31) and
813700120315               %subst(wTfv(tf):1:3) = 'F16';
813800120315             iter;
813900120315           ENDIF;
814000120315
814100120315         //?Abilito F18
814200120319           IF  (wTerza or wQuarta or (wPrima and not *in10) or
814300120319               not *in18 or *in50) and
814400120315               %subst(wTfv(tf):1:3) = 'F18';
814500120315             iter;
814600120315           ENDIF;
814700120315
814800120315           IF  wTfv(tf) <> *blanks;
814900120315             wpos = %scan('#':wTfv(tf));
815000120315             wpostf1 += wpos;
815100120315
815200120315             IF  wpostf1 <= %len(VZFd01);
815300120315               wposda = wpostf1 - wpos + 1;
815400120315               %subst(wtf1:wposda:(wpos-1)) = %subst(wTfv(tf):1:(wpos-1));
815500120315             ELSE;
815600120315               wpostf2 += wpos;
815700120315               IF  wpostf2 <= %len(VZFd01);
815800120315                 wposda = wpostf2 - wpos + 1;
815900120315                 %subst(wtf2:wposda:(wpos-1)) = %subst(wTfv(tf):1:(wpos-1));
816000120315               ELSE;
816100120315                 wpostf3 += wpos;
816200120315                 IF  wpostf3 <= %len(VZFd01);
816300120315                   wposda = wpostf3 - wpos + 1;
816400120315                   %subst(wtf3:wposda:(wpos-1)) = %subst(wTfv(tf):1:(wpos-1));
816500120315                 ENDIF;
816600120315               ENDIF;
816700120315             ENDIF;
816800120315
816900120315           ENDIF;
817000120315         ENDFOR;
817100120315
817200120315       //?Imposto F24 giusto
817300120315         SELECT;
817400120315           WHEN  wtf3 <> *blanks;
817500120315             VZFd02 = cf2413;
817600120315             wf24 = 3;
817700120315           WHEN  wtf2 <> *blanks;
817800120315             VZFd02 = cf2412;
817900120315             wf24 = 2;
818000120315           OTHER;
818100120315             clear VZFd02;
818200120315             wf24 = 1;
818300120315         ENDSL;
818400120315
818500120315         VZFd01 = wtf1;
818600120315         wrig = 1;
818700120315
818800120315       ENDSR;
818900120315
819000120315       //--------------------------------------------------------------
819100120315       //?Gestione tasto funzionale F24
819200120315       //?F24=Altri Tasti
819300120315       //--------------------------------------------------------------
819400120315       BEGSR gesf24;
819500120315
819600120315         SELECT;
819700120315           WHEN  (wf24 = 3 and wrig = 3) or
819800120315                 (wf24 = 2 and wrig = 2);
819900120315             VZFd01 = wtf1;
820000120315             wrig = 1;
820100120315             IF  wf24 = 2;
820200120315               VZFd02 = cf2412;
820300120315             ELSE;
820400120315               VZFd02 = cf2413;
820500120315             ENDIF;
820600120315
820700120315           WHEN  (wf24 = 3 and wrig = 1) or
820800120315                 (wf24 = 2 and wrig = 1);
820900120315             VZFd01 = wtf2;
821000120315             wrig = 2;
821100120315             IF  wf24 = 2;
821200120315               VZFd02 = cf2422;
821300120315             ELSE;
821400120315               VZFd02 = cf2423;
821500120315             ENDIF;
821600120315
821700120315           WHEN  wf24 = 3 and wrig = 2;
821800120315             VZFd01 = wtf3;
821900120315             wrig = 3;
822000120315             VZFd02 = cf2433;
822100120315
822200120315         ENDSL;
822300120315
822400120315       ENDSR;
822500120315
822600120314       //--------------------------------------------------------------
822700120314       //?Visualizza Dati manca tariffa-
822800120314       //--------------------------------------------------------------
822900120314       BEGSR GesW01;
823000120314
823100120314         wFine = *off;
823200120314         W01dsp = ISB48dsp;
823300120314         W01tbl = ISB48tbl;
823400120314         W01lnp = ISB48lnp;
823500120314         W01lna = ISB48lna;
823600120314         W01pro = ISB48pro;
823700120314         W01cts = ISB48cts;
823800120314         W01ctr = ISB48ctr;
823900120314         W01ncl = ISB48ncl;
824000120314         W01pkf = ISB48pkf;
824100120314         W01vlf = ISB48vlf;
824200120314         W01err = ISB48err;
824300120314
824400120314         DOW wFine = *off;
824500120314           exfmt  TA01W01;
824600120314           //?- F12 = Ritorno
824700120314           IF  *inkl;
824800120314             wFine = *on;
824900120314           ENDIF;
825000120314         ENDDO;
825100120314
825200120314       ENDSR;
825300120713
825400120713       //--------------------------------------------------------------
825500120713       //?Ricerca Codice Tassazione
825600120713       //--------------------------------------------------------------
825700120713       BEGSR Ric_CT;
825800120713
825900120713         clear TRTB09DS;
826000120713         TB09fun = '1';
826100120713         TB09ord = '1';
826200120713         IF  V1Cnet = 'D';
826300120713           TB09est = 'E';
826400120713         ELSE;
826500120713           TB09est = V1Cfie;
826600120713         ENDIF;
826700120713         TB09uta = 'S';
826800120713         IF  V1Cnet = 'F';
826900120713           TB09utf = 'S';
827000120713         ELSE;
827100120713           TB09utf = 'N';
827200120713         ENDIF;
827300120713         kpjbu = TRTB09DS;
827400120713         trtb09r (kpjba);
827500120713         TRTB09DS = kpjbu;
827600120713
827700120713       ENDSR;
827800140224
827900140224       //--------------------------------------------------------------
828000140224       //?Controllo tariffa Fuel.
828100140224       //--------------------------------------------------------------
828200140224       BEGSR Ctr_Fuel;
828300140224
828400140224         wNoOkFuel = *off;
828500140224         wtpar = 'f ';
828600140224         IF  not *in50;
828700140224           chain(n) (v1cksc:vctr:prg:wtpar) titpt01l;
828800140224         ENDIF;
828900140224         IF  *in50;
829000140224           chain(n) (v1cksc:vctr:prg:wtpar) tiopt01l;
829100140224         ENDIF;
829200140224
829300140224       //?Se trovo tariffa FUEL verifico che la data di riferimento
829400140224       //?del prezzo base non sia > di oggi
829500140224         IF  %found and TPTatb = *blanks and
829600140224             TPTdpb > dateu8;
829700140224           wNoOkFuel = *on;
829800140224         ENDIF;
829900140224
830000140224       ENDSR;
830100120713
830200120314      /end-free
830300120314
830400000000     C*---------------------------------------------------------------*
830500041222**   XDTF
830600000000312831303130313130313031
830700900320**   TES
830800900321 ***  GESTIONE TARIFFE CLIENTI  ***
830900900327 ***  GESTIONE  VISITE/OFFERTE  ***
831000940607**   MSG
831100940607Data Decorrenza Errata                                                         1
831200940607Data Scadenza Errata                                                           2
831300940607Data Scadenza Incongruente con Data Decorrenza                                 3
831400940607Data Scadenza Incongruente con Data Decorrenza Progressivo Seguente            4
831500940607Data Decorrenza Incongruente con Data Scadenza Progressivo Precedente          5
831600940607Immettere % Addebito Istat                                                     6
831700940607Cliente Esente: non immettere % Istat                                          7
831800120124La Data Scadenza non può essere maggiore di 5 anni da oggi!!                   8
831900940607Codice Tariffa Inesistente                                                     9
832000940607Immettere il Volume Automatico per KG. o per Colli                            10
832100940607Immettere o i KG. o i MC. per Colli per Volume Automatico                     11
832200940912Valore inferiore al minimo previsto dal Tipo Tariffa                          12
832300940607Gli Arrotondamenti devono essere o tutti pieni o tutti lasciati in bianco     13
832400090706Tipo tariffa non prevede il rapporto peso/vol. non immettere utilizzo peso VDL14
832500140220ATTENZIONE!!! Non è stata inserita la tariffa Fuel Surcharge.                 15
832600140226Prezzo base Fuel mai rilevato settimanalmente dal Ministero.                  16
832700110128Tariffa FedEx. Il rapp.peso/volume deve essere uguale su tutti gli scaglioni  17
832800061013                                                                              18
832900061013                                                                              19
833000120315F3 non consentito in Aggiunta Tariffa!!!                                      20
833100940607CMD12 non consentito in Aggiunta Tariffa !!                                   21
833200940607Filiale Inesistente                                                           22
833300940607Codice Tassazione Inesistente                                                 23
833400120906Immettere la filiale                                                          24
833500940607Con Tariffa Regione o Italia non deve essere inserito il C.A.P. del Paese     25
833600940607C.A.P. Incongruente con il Codice Tassazione inserito !!                      26
833700940607Se inserito il C.A.P. non deve esistere la Tariffa Provincia                  27
833800940607Scaglione Obbligatorio                                                        28
833900940607Tariffa Citta' Obbligatoria                                                   29
834000940607Tariffa gia' esistente                                                        30
834100940607L'Importo Massimo deve essere minore o uguale al Minimo                       31
834200940607Fine Scorrimento                                                              32
834300050616Immettere uno dei quattro campi della manipolazione                           33
834400940607Immettere almeno un Codice di Tassazione                                      34
834500940607Immettere lo Scaglione                                                        35
834600940607Codice Tassazione Errato                                                      36
834700940607Effettuare almeno una scelta                                                  37
834800070309Immettere almeno una tariffa particolare                                      38
834900070309Immesse tariffe particolari ma non richiesta la manipolazione                 39
835000070309Tariffa particolare ' ' non presente                                          40
835100070412La tariffa particolare ' ' è esclusa dalla manipolazione tariffe              41
835200940729Tipo Servizio Inesistente o Annullato                                         42
835300940811Natura Merce Obbligatoria                                                     43
835400950105Gestione Valore da Fatturare Obbligatorio                                     44
835500950112Impossibile inserire linea di partenza estera                                 45
835600950118Impossibile inserire scaglione con decimali                                   46
835700950112Impossibile inserire codice tassazione estero                                 47
835800950112Impossibile inserire sia linea partenza che codice tassazione italiani        48
835900940929Impossibile inserire tariffa preferenz. perchè già presente in altro cod.tar. 49
836000941122Codice Italia/Estero Obbligatorio                                             50
836100941122Codice Italia/Estero Errato                                                   51
836200061013                                                                              52
836300020205Impossibile variare flag Italia/Estero                                        53
836400061013                                                                              54
836500061013                                                                              55
836600950119Impossibile inserire gli arrotondamenti con decimali                          56
836700950119Impossibile inserire l'applicazione tariffa finita con decimali               57
836800061013                                                                              58
836900960520Rapporto peso/volume superiore al limite indicato in tabella: Enter x forzare 59
837000960522Immettere almeno una Linea di Partenza                                        60
837100960522Linea di Partenza XXX Inesistente                                             61
837200960523Non esistono righe di dettaglio da copiare per la selezione fatta !!          62
837300960524Se inserite solo le linee partenza impossibile digitarle uguali               63
837400960524Se inseriti sia le linee part. che i cod.tassaz. impossibile digitarli uguali 64
837500960524Immettere la linea partenza                                                   65
837600960524Immettere il codice tassazione                                                66
837700960524Attenzione!! La tariffa che si vuole creare esiste già: Enter x forzare       67
837800960527Attenzione!! Esistono già delle tariffe per la linea da creare:Enter x forzare68
837900960729Inserire arrotondam. perchè gli importi della tariffa non sono a percentuale  69
838000090608Non si puo' inserire una tariffa xxxxxxxx  "ESTERA"                           70
838100061013                                                                              71
838200980309ATTENZIONE !! Tariffa particolare Assic. per conto errata per tariffa IMPORT  72
838300980310E' inutile inserire MIN e/o MAX per scaglioni inferiori al minimo tassabile   73
838400980505Per questa tariffa non viene gestito l'arrotondamento                         74
838500980505Per questa tariffa non e' prevista la gestione del rapporto Peso / Volume     75
838600150922                                                                              76
838700150922                                                                              77
838800990617Il numero massimo delle spedizioni deve essere maggiore del minimo            78
838900990617Non sono ammessi importi con decimali                                         79
839000990617In caso di tariffa in percentuale bastano 2 decimali                          80
839100990623Nazione errata                                                                81
839200061013                                                                              82
839300991008L'arrotondamento deve essere un multiplo di 10                                83
839400991008L'arrotondamento non deve avere l'ultimo decimale diverso da zero             84
839500000118Per tariffe DPD obbligatorio indicare  "NO DDT"                               85
839600000118Non si puo' eliminare il servizio DPD !!!                                     86
839700000118Una tariffa estera non puo' essere dpd                                        87
839800090608Una tariffa DPD o FedEx deve essere soltanto  "xxxxxxxx"                      88
839900061013                                                                              89
840000130514Tipo servizio non può essere variato in manutenzione tariffa.                 90
840100090915Tipo servizio non utilizzabile in tariffa                                     91
840200090915
840300061013                                                                              93
840400010323Si può eseguire un solo arrotondamento                                        94
840500061013                                                                              95
840600150922                                                                              96
840700020130Non si puo' eliminare il servizio FedEx !!!                                   97
840800020130Una tariffa italia non puo' essere FedEx !!!                                  98
840900020205Impossibile inserire Network                                                  99
841000020207Codice tassazione non utlizzabile per tariffa FedEx                           00
841100020207Codice tassazione utilizzabile solo per tariffe FedEx                         01
841200110131Manipolazione Rapporto peso/volume tariffa FedEx. NO I/O su Scaglione!!       02
841300110131Manipolazione Rapporto peso/volume tariffa FedEx. NO I/O su cod.Tassazione!!  03
841400020312Non sono state inserite tariffe particolari, non è possibile la manipolazione 04
841500020318ATTENZIONE!! Manipolazione non eseguita per importi errati                    05
841600050616Effettuare almeno una scelta: dettaglio tariffe o tar.particolari o giacenza! 06
841700020328Per tariffa import/export immettere solo codice tassazione generico 'EE'      07
841800020404Se inserita la Nazione inserire anche il cap!                                 08
841900020404Per tariffa DPD immettere solo codice tassazione generico 'IT'                09
842000020624Linea di partenza 950 non inserire codice tassazione generico 'IT' o 'EE'     10
842100021118Tariffa FedEx non consentita per il codice tariffa inserito                   11
842200021118Scaglione superiore al peso consentito per questa tariffa: ENTER x FORZARE    12
842300130926MANCA SCAGLIONE ISTAT  AVVISARE PRESIDIO VENDITE !!!!!!!!!                    13
842400131029La % applicazione ISTAT non può essere minore di 50 o maggiore di 100         14
842500050530Esiste TarPartic."XX"da eliminare entro xx/xx/xx con F21 o modificare scadenza
842600050617Manipolazione non possibile se immesso un importo di aumento/dimunuzione      16
842700050617Non sono state inserite tariffe di giacenza, non è possibile la manipolazione 17
842800110131Manipolazione Rapporto peso/volume tariffa FedEx. NO I/O su Linea Partenza!!  18
842900060307Impossibile inserire la rinuncia. Presente AC BASE                            19
843000061207Effettuare solo una scelta per il rapporto peso/volume                        20
843100090608Non inserire LNP 950 perchè la LNP     ha codici di tassazione 'IT' o 'EE'    21
843200131029La % applicazione ISTAT non può essere < di 50 o > di 100. ENTER PER FORZARE  22
843300131113
843400120315
843500131028
843600120315** - wTfv (in ordine di visualizzazione) ------------------------------------*
843700120321F1=Manca Tariffa  #
843800120315Enter=Aggiorna  #
843900120315F3=Fine  #
844000120315F2=Rubrica  #
844100120315F6=Aggiorna e fine  #
844200120315F8=Note Tar.  #
844300120315F10=Stampa  #
844400120315F11=Altri Dati  #
844500120315F16=Annulla  #
844600120315F18=Note  #
844700120315F12=Ritorno   #
844800131028
844900131028
845000131028
845100131028
845200131028
845300131028
845400131028
845500131028
845600131028
