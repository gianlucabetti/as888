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
046500021017
046600980226     D*
046700010323     D* CAMPO PER DSPATR "NO DISPLAY"+"PROTECT" DEI CAMPI A VIDEO
046800010323     D NDPR            C                   CONST(X'A7')
046900020207     D*
047000020207     D* CAMPO PER DSPATR "REVERSE IMAGE"        DEI CAMPI A VIDEO
047100020207     D RIMM            C                   CONST(X'21')
047200130124
047300130124      // campo per DSPATR "PROTECT"
047400130124     d nPRO            c                   CONST(X'A0')
047500030715
047600030715     D* Reperimento dati del PGM
047700030715     D PGMSTATUS      SDS
047800060116     d  SDSpgm           *proc
047900030715     D JOB_NAME              244    253
048000030715     D USER                  254    263
048100101013
048200101013      //---------------------------------------------------------------
048300101013      //?Definizione procedure esterne.
048400101013      //---------------------------------------------------------------
048500101013      // - Reperimento Scatti ISTAT
048600101013     d trtb12r1        pr                  extpgm('TRTB12R1')
048700101013     d  kpjba                              likeds(KPJBA)
048800120713
048900120713      // - Interrogazione codici tassazione
049000120713     d trtb09r         pr                  extpgm('TRTB09R')
049100120713     d  kpjba                              likeds(KPJBA)
049200160209
049300160209      // - Pgm Cancella file Tariffe Inserite
049400160209     d trsm90r         pr                  extpgm('TRSM90R')
049500160209     d  kpjba                              likeds(KPJBA)
049600160209     d  trsm90ds                           likeds(TRSM90DS)
049700090916
049800100407      //---------------------------------------------------------------
049900100407      //?prototipi
050000100407      //---------------------------------------------------------------
050100100407      /copy gaitrasrc/srcprotopr,tnta28r
050200100407      /copy gaitrasrc/srcprotopr,trmk20r
050300120315
050400120315      //---------------------------------------------------------------
050500120315      //?Costanti
050600120315      //---------------------------------------------------------------
050700120315     d cf2413          c                   const('F24=AlTasti(1/3)')
050800120315     d cf2423          c                   const('F24=AlTasti(2/3)')
050900120315     d cf2433          c                   const('F24=AlTasti(3/3)')
051000120315     d cf2412          c                   const('F24=AlTasti(1/2)')
051100120315     d cf2422          c                   const('F24=AlTasti(2/2)')
051200030715
051300031120     C*****************************************************************
051400031120     C* RIEPILOGO INDICATORI
051500031120     C*****************************************************************
051600031120     C* 01    - IMMISSIONE
051700031120     C* 02/09 - TASTI DI DUPLICA
051800031120     C* 10    - AGGIORNAMENTO
051900940530     C* 11    - TASTO DI DUPLICA
052000941109     C* 12    - PROVENGO DA AGGIUNTA TARIFFA QUINDI NON ABILITO IL CM12
052100941124     C* 13    - USATO NEL DETTAGLIO PER VISUALIZZARE LA LETTERA "M"
052200941124     C*         CHE INDICA LA PRESENZA DEL MINIMO O DEL MASSIMO
052300941109     C* 14    - CONDIZIONA LO SPEGNIMENTO DELL'INDICATORE DI SFLNXTCHG
052400940530     C* 15    - USATO IN INTERROGAZIONE PER PROTEGGERE I CAMPI
052500130318      *         (nel pgm TNTA02R2)
052600941109     C* 16    - INDICA IL TIPO TARIFFA "4 - A VALORE"
052700130124     C* 17    - Protezioni campi in modifica tariffe
052800950107     C* 18    - SONO IN FILIALE QUINDI VISUALIZZO NOTE
052900990601     C* 19    - ERRORE SU WRITE DI TITAD
053000941109     C* 20    - E' STATA DIGITATA UNA CHIAVE DI RICERCA
053100941109     C* 21    - ESISTONO RIGHE DI DETTAGLIO
053200940530     C* 22    - SFLNXTCHG
053300941109     C* 23    - SI TRATTA DI TARIFFA DI CARTELLO
053400020314     c* 24    - ERRORE
053500940530     C* 25    - ROLLUP
053600041222     C* 26    - USATO NEL SUBFILE PIÙ LINEE PARTENZA
053700950107     C* 27    - USATO PER EMETTERE UNA VIDEATA DOPO UN "?" SU UN CAMPO;
053800940530     C* 28    - INDICATORE DI EMISSIONE MESSAGGIO DI ERRORE
053900940530     C* 29    - READC SU SUBFILE
054000990601     C* 30    - INDICATORE DI MODIFICA DI TITAD
054100130318      * 31    - F16 abilitato
054200130318     C* 32/34 - DI COMODO
054300940530     C* 35    - USATO PER LOKUP E TESTN
054400941007     C* 36    - UTILIZZATO PER GESTIRE L'ALLOCAZIONE DEL RECORD: SE
054500941007     C*         IL RECORD E' ALLOCATO (CIOE' *IN36 ACCESO), EMETTO UN
054600130924     C*       - UTILIZZATO ANCHE PER GESTIRE MSG DI ERRORE MANCA SCAGLIONE
054700130924     C*         ISTAT AVVISARE IL CED DAL PGM CHIAMANTE
054800960523     C* 37    - DI COMODO
054900990617     C* 38    - NAZIONE ERRATA
055000021118      * 39    - Protezione natura merce
055100990617     C* 44    - DUPLICA
055200000118     C* 40/49 - ERRORI
055300940530     C* 50    - OFF ---> TARIFFE CLIENTI    ON ---> OFFERTE CLIENTI
055400990616     C* 51    - ERRORE MINIMO /MASSIMO SPEDIZIONI
055500990617     C* 52    - ERRORE DECIMALI IMPORTO MASSIMO
055600151016     C* 53    - Visualizzo applicazione calcolo peso desunto
055700940706     C* 51/59 - ERRORI
055800940706     C* 61/80 - ERRORI
055900941109     C* 81    - INDICATORE DI MODIFICA DI TNTAM(AGGIORNO DATA ULT.VAR.)
056000940811     C* 82    - ERRORE
056100941109     C* 83    - SFLDLT ---> SFL DI IMMISSIONE
056200941109     C* 84    - SFLCLR ---> SFL DI IMMISSIONE
056300090916      * 85    - Trattativa
056400090916     C* 86/87 - ERRORI
056500941124     C* 88    - IMPORTO TARIFFA IN PERCENTUALE
056600960522     C* 89/90 - ERRORI
056700941109     C* 91    - ELIMINAZIONE SUBFILE DI AGGIORNAMENTO SU DISCO
056800050530     C* 92    - usato
056900941109     C* 93    - SFLDLT ---> SFL DI AGGIORNAMENTO
057000941109     C* 94    - SFLCLR ---> SFL DI AGGIORNAMENTO
057100950116     C* 95    - ESISTONO TARIFFE PARTICOLARI: EVIDENZIO IL COMANDO
057200950116     C* 96    - ESISTONO TARIFFE GIACENZA   : EVIDENZIO IL COMANDO
057300120314      * 97    - Pgm richiamato x manca tariffa TNSB48
057400041222     C* 98    - USATO NEL SUBFILE PIÙ LINEE PARTENZA
057500941109     C* 99    - ELIMINAZIONE SUBFILE DI IMMISSIONE    SU DISCO
057600900329     C*****************************************************************
057700000000     C     *ENTRY        PLIST
057800000000     C                   PARM                    KPJBA
057900120315     c                   parm                    Parm48
058000120314
058100120314      /free
058200120314       //?controllo se passati tutti i parametri
058300120314         IF  %parms = 1;
058400120314           clear tnsb48ds;
058500120314           *in97 = *off;
058600120314         ELSE;
058700120315           clear tnsb48ds;
058800120315           tnsb48ds = parm48;
058900120314           *in97 = *on;
059000120314         ENDIF;
059100120314
059200120314         clear OSB48err;
059300120314         clear OSB48msg;
059400120314      /end-free
059500061011
059600061011     c     *dtaara       define    §azute        azuteds
059700061011     c     *dtaara       define    §datiute      ddatiute
059800061011     c                   in(E)     *dtaara
059900061011     c                   If        %error  or RSUT = *blanks
060000061011     c                   Clear                   Tibs34ds
060100061011     c                   Call      'TIBS34R'
060200061011     c                   Parm                    Tibs34ds
060300061011     c                   In        *dtaara
060400061011     c                   EndIf
060500061011
060600061011     C                   MOVEL     Rsut          V1CRSU
060700000000     C*---------------------------------------------------------------*
060800940525     C                   SETOFF                                       212726
060900921218     C                   SETOFF                                       9199
061000980226     C*---------------------------------------------------------------*
061100990601     C* ACCESSO TITAD04L
061200000000     C     KTAD          KLIST
061300940526     C                   KFLD                    V1CKSC
061400900320     C                   KFLD                    TAMCTR
061500000000     C                   KFLD                    PRG
061600000000     C                   KFLD                    TADLNP
061700000000     C                   KFLD                    TADORP
061800020624     C                   KFLD                    tadnaz
061900020624     C                   KFLD                    tadcap
062000940511     C                   KFLD                    TADSGL
062100960523     C     KTAD2         KLIST
062200960523     C                   KFLD                    V1CKSC
062300960523     C                   KFLD                    VCTR
062400960523     C                   KFLD                    PRG
062500960524     C                   KFLD                    COMLN2
062600960523     C     KTAD3         KLIST
062700960523     C                   KFLD                    V1CKSC
062800960523     C                   KFLD                    VCTR
062900960523     C                   KFLD                    PRG
063000960524     C                   KFLD                    COMLN2
063100960524     C                   KFLD                    COMOR2
063200940615     C* ACCESSO TNTAM01L
063300920526     C     KTAM          KLIST
063400940526     C                   KFLD                    V1CKSC
063500920526     C                   KFLD                    VCTR
063600990601     C* ACCESSO TNTAM01L/TITAD04L/TITPT01L/TITPD01L
063700900320     C     KTAM2         KLIST
063800940526     C                   KFLD                    V1CKSC
063900900320     C                   KFLD                    VCTR
064000900320     C                   KFLD                    PRG
064100061010     C* ACCESSO TNTAM01L
064200900419     C     KTAM5         KLIST
064300940503     C                   KFLD                    COMKSC
064400940503     C                   KFLD                    COMCTR
064500940503     C                   KFLD                    COMPRG
064600990601     C* ACCESSO TITPT01L/TIOPT01L  PER RICERCA TAR. PART. ASSIC. X CONTO
064700980309     C     KTAM7         KLIST
064800980309     C                   KFLD                    V1CKSC
064900980309     C                   KFLD                    VCTR
065000980309     C                   KFLD                    PRG
065100990609     C                   KFLD                    WTPAR             2
065200990601     C* ACCESSO TITAD04L
065300000000     C     KSCOR         KLIST
065400940526     C                   KFLD                    V1CKSC
065500900320     C                   KFLD                    TAMCTR
065600000000     C                   KFLD                    PRG
065700000000     C                   KFLD                    LNPX
065800000000     C                   KFLD                    ORPX
065900990608     C                   KFLD                    NAZX
066000930630     C                   KFLD                    COMX
066100000000     C                   KFLD                    SCGX
066200000000     C     KTTAD         KLIST
066300940526     C                   KFLD                    V1CKSC
066400900320     C                   KFLD                    TAMCTR
066500000000     C                   KFLD                    PRG
066600940526     C                   KFLD                    V1CRDA
066700990608     C                   KFLD                    VIDORP            5
066800020312      * accesso titpd01l
066900020312     c     ktpd          klist
067000020312     c                   kfld                    tptksc
067100020312     c                   kfld                    tptctr
067200020312     c                   kfld                    tptprg
067300020312     c                   kfld                    tptftc
067400050530     C     KTPT          KLIST
067500050530     C                   KFLD                    tamksc
067600050530     C                   KFLD                    TAMCTR
067700050530     C                   KFLD                    TAMPRG
067800050530     C                   KFLD                    tpareli(XX)
067900940510     C* ACCESSO TABEL00F
068000000000     C     KTAB          KLIST
068100000000     C                   KFLD                    CODUT
068200940503     C                   KFLD                    COD
068300940503     C                   KFLD                    KEY
068400940510     C     KTAB2         KLIST
068500920525     C                   KFLD                    CODUT
068600920525     C                   KFLD                    COD
068700940510     C* ACCESSO TFNTC01L
068800920526     C     KNTC          KLIST
068900940503     C                   KFLD                    APPL
069000940503     C                   KFLD                    NOTKEY
069100940503     C                   KFLD                    NOTKE2
069200940503     C*
069300940503     C* DEFINIZIONE DI ALCUNI CAMPI
069400970317     C     *LIKE         DEFINE    TAMDDT        DATEU8
069500940706     C     *LIKE         DEFINE    TAMDDT        UDATE8
069600940706     C*
069700940503     C     *LIKE         DEFINE    TBLCOD        COD
069800940503     C     *LIKE         DEFINE    TBLKEY        KEY
069900940503     C*
070000940503     C     *LIKE         DEFINE    TAMKSC        COMKSC
070100940503     C     *LIKE         DEFINE    TAMCTR        COMCTR
070200940503     C     *LIKE         DEFINE    TAMPRG        COMPRG
070300940503     C*
070400940503     C     *LIKE         DEFINE    NTCAPL        APPL
070500940503     C     *LIKE         DEFINE    NTCNK1        NOTKEY
070600940503     C     *LIKE         DEFINE    NTCNK2        NOTKE2
070700940518     C* CAMPI DI DUPLICA
070800940518     C     *LIKE         DEFINE    TADLNP        DUPLNP
070900940518     C     *LIKE         DEFINE    TADCTS        DUPCTS
071000940518     C     *LIKE         DEFINE    TADCAP        DUPCAP
071100990608     C     *LIKE         DEFINE    TADCAP        DUPNAZ
071200940518     C     *LIKE         DEFINE    TADSGL        DUPSGL
071300940518     C     *LIKE         DEFINE    TADITR        DUPITR
071400940518     C     *LIKE         DEFINE    TADINO        DUPINO
071500940518     C     *LIKE         DEFINE    TADRPV        DUPRPV
071600940518     C     *LIKE         DEFINE    TADMIN        DUPMIN
071700940518     C     *LIKE         DEFINE    TADMAX        DUPMAX
071800940518     C*
071900940518     C     *LIKE         DEFINE    TADITR        RISITR
072000940518     C     *LIKE         DEFINE    TADITR        COMITR
072100940518     C     *LIKE         DEFINE    TADITR        ITRCOM
072200940518     C     *LIKE         DEFINE    TADINO        RISINO
072300940518     C     *LIKE         DEFINE    TADINO        COMINO
072400940518     C     *LIKE         DEFINE    TADINO        INOCOM
072500940518     C     *LIKE         DEFINE    TADRPV        RISRPV
072600940518     C     *LIKE         DEFINE    TADRPV        RPVCOM
072700940518     C     *LIKE         DEFINE    TADMIN        RISMIN
072800940518     C     *LIKE         DEFINE    TADMIN        MINCOM
072900941124     C     *LIKE         DEFINE    TADMIN        WMIN
073000940518     C     *LIKE         DEFINE    TADMAX        RISMAX
073100940518     C     *LIKE         DEFINE    TADMAX        MAXCOM
073200941124     C     *LIKE         DEFINE    TADMAX        WMAX
073300940518     C     *LIKE         DEFINE    TADATB        ATBCOM
073400941108     C     *LIKE         DEFINE    TADCTS        WCTS
073500941108     C     *LIKE         DEFINE    TAMFIE        WFIE
073600020215     C     *LIKE         DEFINE    TAMTSP        WTSP
073700020215     C     *LIKE         DEFINE    V1CNET        WNET
073800960523     C     *LIKE         DEFINE    TADORP        WORP
073900960523     C     *LIKE         DEFINE    TADORP        WORPN
074000960524     C     *LIKE         DEFINE    TADORP        COMORP
074100960524     C     *LIKE         DEFINE    TADORP        COMOR2
074200960524     C     *LIKE         DEFINE    TADLNP        COMLNP
074300960524     C     *LIKE         DEFINE    TADLNP        COMLN2
074400960527     C*
074500960527     C     *LIKE         DEFINE    V8CLNN        SAVLNN
074600960527     C     *LIKE         DEFINE    V8CCTN        SAVCTN
074700940518     C*
074800931119     C*---------------------------------------------------------------*
074900000000     C                   MOVEL     *ZEROS        KI
075000000000     C                   MOVEL     *ZEROS        KE
075100000000     C                   MOVEL     *ZEROS        KX
075200000000     C                   MOVEL     *ZEROS        KIAVE
075300000000     C                   MOVEL     *BLANKS       TADORP
075400000000     C                   MOVEL     *BLANKS       ORPI
075500000000     C                   MOVEL     *BLANKS       PARI
075600990608     C                   MOVEL     *BLANKS       NAZI
075700000000     C                   MOVEL     *BLANKS       PARE
075800000000     C                   MOVEL     *BLANKS       ORPE
075900990608     C                   MOVEL     *BLANKS       NAZE
076000000000     C                   MOVEL     *BLANKS       ORPX
076100000000     C                   MOVEL     *BLANKS       PARX
076200990608     C                   MOVEL     *BLANKS       NAZX
076300050530     c
076400050530     c                   clear                   forzatpar
076500050530     C* INDICe DEDICATO PER LA SKIERA FORZATPAR: non usare per altro
076600050530     c                   clear                   yy
076700930628     C*
076800940728     C* GIRO DATA DEL GIORNO: LA PRENDO DA TIME
076900940728     C                   TIME                    W0120            12 0
077000940728     C                   MOVE      W0120         WDAT              6 0
077100940728     C*
077200940729     C                   Z-ADD     WDAT          G02DAT
077300940706     C                   MOVEL     *BLANK        G02ERR
077400940706     C                   CALL      'XSRDA8'
077500940706     C                   PARM                    WLBDAT
077600940706     C* UDATE A 8 IN AAAA/MM/GG
077700941221     C                   Z-ADD     G02INV        DATEU8
077800940706     C* UDATE A 8 IN GG/MM/AAAA
077900940706     C                   Z-ADD     G02DAT        UDATE8
078000120124
078100120124      /free
078200120124       //?Imposto oggi in ISO
078300120124        wOggi = %date(dateu8:*iso);
078400130124       //?Imposto mese in corso
078500130124        wMeseJob = %dec(%subst(%editc(dateu8:'X'):5:2):2:0);
078600120124      /end-free
078700120124
078800060116      * Nome pgm.
078900060116     c                   movel     SDSpgm        V1Cpgm
079000000223     C*
079100000223     C* AGGANCIO DS3IDP PER AVEREIL MASSIMO PESO
079200000223     C                   MOVEL     '3I'          COD
079300000223     C                   MOVEL(P)  'DPD'         KEY
079400000223     C*
079500000223     C     KTAB          CHAIN     TABEL                              30
079600000223     C  N30              MOVEL     TBLUNI        DS3IDP
079700000223     C   30              CLEAR                   DS3IDP
079800000223     C*
079900940706     C*
080000940526     C                   Z-ADD     0             V1CKSC
080100000000     C                   Z-ADD     0             PRG
080200000000     C                   Z-ADD     0             CCC
080300120906     C                   Z-ADD     0             V1CRDA
080400120906     C                   MOVEL     *BLANKS       V1CRA
080500940526     C                   MOVEL     *BLANKS       V1DKSC
080600000000     C                   MOVEL     KPJBU         PARAMX
080700990913     C* IMPOSTO IL CODICE DI RITORNO AL PROGRAMMA CHIAMANTE
080800990913     C*
080900990913     C                   MOVEL     'R'           FLGRTN
081000980309     C                   MOVE      ' '           UNO               1
081100090916
081200090916      * controllo se richiamato da trattativa
081300090916     c                   if        partrat = 'T'
081400090916     c                   eval      *in85 = *on
081500090916     c                   else
081600090916     c                   eval      *in85 = *off
081700090916     c                   endif
081800920707     C*
081900050530     C** CARICAMENTO TABELLE
082000050530     C                   IF        WCARTAB=' '
082100920707     C                   EXSR      CARTAB
082200050530     C                   EVAL      WCARTAB='S'
082300050530     C                   ENDIF
082400931119     C*
082500061011     C** SOLO SE SONO IN FILIALE APRO FILE NOTE
082600941012    1C  N18SIMFEL        IFNE      0
082700940530     C                   SETON                                        18
082800941012    1C                   ENDIF
082900941007     C*
083000941007     C* 23 ON  - SI TRATTA DI TARIFFA DI CARTELLO
083100941116     C                   SETOFF                                       23
083200941007     C                   MOVEL     V1CKSC        W0050             5 0
083300941012    1C     W0050         IFEQ      88888
083400941007     C                   SETON                                        23
083500941012    1C                   ENDIF
083600130124
083700991213     C* SE NON È TARIFFA DI CARTELLO RECUPERO LA FILIALE DI APPERTENENZA
083800991213     C* DEL CLIENTE PER RECUPERARE UN FLAG A PROGRAMMA PER LA GESTIONE DI
083900991213     C* TARIFFE ITALIA CON CODICI TASSAZIONE ESTERI
084000000420     C                   MOVEL     *BLANKS       WRKPT             1
084100000426     C* SE NON SI TRATTA DI OFFERTA
084200991213     C     *IN23         IFEQ      *OFF
084300030930     C     VFLG          ANDNE     '1'
084400991213     C                   MOVEL     V1CKSC        W0030
084500000118     C     W0030         CHAIN     AZORG01L                           37
084600000118     C     *IN37         IFEQ      *OFF
084700000420     C* CONTROLLO SE E' CLIENTE DPD
084800000218     C                   MOVEL     ORGDE3        OG143
084900000420     C* CONTROLLO SE E' CLIENTE POSTE
085000020312     c                   if        §ogntw = 'PPT'
085100020312     c                   eval      wrkpt = 'S'
085200020312     c                   endif
085300000420     C*
085400991213     C                   ENDIF
085500991213     C                   ENDIF
085600941007     C*
085700900327     C* TESTATA VIDEO
085800941012    1C     VFLG          IFNE      '1'
085900940503     C* VFLG <> '1'  ---> GESTIONE TARIFFE CLIENTI
086000940526     C                   MOVE      TES(1)        V1CTES
086100061011     c                   setoff                                       50
086200061011      * apro i file delle tariffe
086300061011     c                   open      tntam01l
086400061011     c                   open      titad04l
086500061011     c                   open      titpt01l
086600061011     c                   open      titpd01l
086700061011     c                   open      titgc01l
086800050603     c* Solo per tariffe apro titav
086900050603     c                   if        solotav=' '
087000050603     c                   open      titav00f
087100050603     c                   open      titav01l
087200050603     c                   eval      solotav='S'
087300050603     c                   endif
087400940503     C*
087500941012   X1C                   ELSE
087600940503     C* VFLG  = '1'  ---> GESTIONE VISITE/OFFERTE
087700940526     C                   MOVE      TES(2)        V1CTES
087800900329     C                   SETON                                        50
087900061011      * apro i file delle offerte
088000061011     c                   open      tnofm01l
088100061011     C                   OPEN      TIOFD01L
088200061011     c                   open      tiopt01l
088300061011     c                   open      tiopd01l
088400061011     c                   open      tiogc01l
088500091204     c   85              open      tivof01l
088600940526     C                   Z-ADD     V1CKSC        V1CNRV
088700090916     C                   Z-ADD     V1CKSC        V1CNRVt
088800021029      * Cerco ORGFEL di simfel
088900021029     c     Simfel        Chain     Azorg01l
089000021029     c                   If        %Found(Azorg01l)
089100021029     c                   Eval      Sav_Orgfel = Orgfel
089200021029     c                   Else
089300021029     c                   Clear                   Sav_Orgfel
089400021029     c                   EndIf
089500981111     C**
089600021017     C* LEGGO  LA DATA statistica
089700021017     c                   Clear                   Tibs02Ds
089800021017     c                   Clear                   dSdf
089900021017     c                   Eval      T02Mod = 'C'
090000021017     c                   Eval      T02Sif = Knsif
090100021017     c                   Eval      T02Cod = 'SDF'
090200021029     c                   Movel(p)  Sav_OrgFel    T02Ke1
090300021017     c                   Call      'TIBS02R'
090400021017     c                   Parm                    Kpjba
090500021017     c                   Parm                    Tibs02Ds
090600021017     c                   If        T02Err = *Blanks
090700021017     c                   Movel     T02Uni        dSdf
090800021017     c                   EndIf
090900941012    1C                   ENDIF
091000931119     C*
091100941012     C* VFLG  = '5'  ---> PROVENGO DA AGGIUNTA TARIFFA QUINDI NON
091200941012     C*                   ABILITO IL CMD12
091300940524     C                   SETOFF                                       12
091400941012    1C     VFLG          IFEQ      '5'
091500940524     C                   SETON                                        12
091600941012    1C                   ENDIF
091700940509     C*
091800061011     C                   SETOFF                                       011016
091900941012     C* CONTROLLO SE TROVO TARIFFA DI QUEL CLIENTE CON QUEL COD.TARIFFA
092000941012    1C     CTR2          IFNE      *BLANKS
092100900327     C                   MOVE      CTR2          VCTR              3 0
092200940811     C*
092300941109     C* 16 ON  - INDICA IL TIPO TARIFFA "4 - A VALORE"
092400940811     C                   MOVEL     CTR2          W001              1
092500941012    2C     W001          IFEQ      '4'
092600940811     C                   SETON                                        16
092700941012    2C                   ENDIF
092800941012    1C                   ENDIF
092900931119     C*
093000900329     C* POSIZIONAMENTO INIZIALE
093100900329     C                   EXSR      LEGGI
093200941012     C* 36 ON  - RECORD ALLOCATO: RITORNO AL PGM TNTA01R UN FLAG IN
093300941012     C*          MODO CHE MI EMETTA IL MESSAGGIO "TARIFFA GIA' IN USO"
093400941012     C   36              GOTO      FINE
093500130131
093600130131       //?Imposto indicatore per protezione campi in modifica tariffe
093700130131       //?ma solo TARIFFE e NO CARTELLO
093800130131     c                   eval      dUTE01 = UTEfaf
093900130131     c                   IF        not *in50 and not *in23 and *in10
094000130131     c                   eval      *in17 = (§UTEmodtar = *blanks)
094100130131     c                   ENDIF
094200130318
094300130318      /free
094400130318
094500130318       //?Imposto la libreria per il TITAS
094600130320       //?in Manutenzione tariffa no cartellp
094700130320        IF  not *in50 and *in10 and not *in23;
094800130320          IF  %subst(knsif : 7 : 1) = 'P';
094900130320            wLib = 'GAITRAGRPS';
095000130320          ELSE;
095100130320            wLib = 'GAITRAGRU';
095200130320          ENDIF;
095300130320         //?Apro il file
095400130320          wFLib = %trim(wLib) + '/TITAS41C';
095500130320          open  TITAS41C;
095600130320         //?Cerco la data ultima spedizione fatturata con la tariffa in
095700130320         //?modifica
095800130320          clear wDataSP;
095900130320          clear TASaas;
096000130320          clear TASmgs;
096100130320          setgt  (TAMKSC:TAMCTR) TITAS41C;
096200130320          readpe (TAMKSC:TAMCTR) TITAS41C;
096300130320          wDataSP = TASaas * 10000 + TASmgs;
096400130320         //?Abilito F16-Annulla
096500130320         //?se data decorrenza tariffa > della data ultima
096600130320         //?spedizione fatturata
096700130322          IF   TAMddt > wDataSP;
096800130318            *in31 = *on;
096900130318          ENDIF;
097000130320         //?mi salvo le date decorrenza e scadenza
097100130318          sav_TAMddt = TAMddt;
097200130318          sav_TAMdst = TAMdst;
097300130318        ENDIF;
097400130320         //?Abilito F16-Annulla
097500130320         //?se offerta
097600130320         //?se tariffa di cartellio
097700130320          IF  *in50 or *in23;
097800130320            *in31 = *on;
097900130320          ENDIF;
098000130318
098100130318      /end-free
098200130131
098300941012     C*
098400941012     C* PULIZIA CAMPI VIDEATA
098500941012     C                   EXSR      INZD01
098600031118     C* 36 ON  - MANCA TABELLA QI ISTAT INDICE RELATIVO ALLA DATA IMMISIIONE
098700031118     C*          TARIFFA EMISISONE DEL MESSAGGIO IN TNTA01R
098800031118     C   36              GOTO      FINE
098900031118     C*
099000931119     C*
099100941012     C* 10 ON  - AGGIORNAMENTO: CARICO DATI VIDEATA
099200941012     C   10              EXSR      RIED01
099300931119     C*
099400900530     C                   SETOFF                                       81
099500931119     C*
099600941012     C***
099700941012     C* EMISSIONE FORMATO 1
099800941012     C***
099900960109     C     FOR012        TAG
100000960423     C* 10 ON  - AGGIORNAMENTO: RIEMPIO CAMPI DELLA 2° VIDEATA
100100061010     C   10              EXSR      RIED02
100200120315
100300120315       //?Imposto i tasti funzione nella riga mobile
100400120315     c                   eval      wPrima = *on
100500120315     c                   eval      wSeconda = *off
100600120315     c                   eval      wTerza = *off
100700120315     c                   eval      wQuarta = *off
100800120315     c                   ExSr      Tastifun
100900960109     C*
101000941012     C     FOR01         TAG
101100120906     C                   Z-ADD     0             V1CRDA
101200120906     C                   MOVEL     *BLANKS       V1CRA
101300151016
101400151016      /free
101500151016       //?visualizzo calcolo peso desunto in fatturazione se:
101600151016       //?utente di sede sempre
101700151016       //?utente di filiale solo se valorizzato
101800151016        *in53 = *off;
101900151016        //ATRtpr = npro;
102000151016        IF  *in18 and V1Ctpr <> *blanks;
102100151016          *in53 = *on;
102200151016        ENDIF;
102300151016        IF  not *in18;
102400151016          *in53 = *on;
102500151016          //clear ATRtpr;
102600151016        ENDIF;
102700151016      /end-free
102800140224
102900140224       //?Se il Fuel non è ok emetto il messaggio
103000140226     c                   IF        wNoOkFuel and not *in17
103100140224     c                   eval      *in28 = *on
103200140224     c                   eval      V1Cmsg = msg(16)
103300140226     c                   eval      V1Cmsg = %trim(V1Cmsg) +
103400140226     c                             ' F21 x modificare.'
103500140224     c                   ENDIF
103600140226
103700140226       //?Se il Fuel non è ok emetto il messaggio
103800140226       //?forzabile se utente non abilitato alla variazione
103900140226       //?della tariffa
104000140226     c                   IF        wNoOkFuel and *in17
104100140226     c                   eval      *in28 = *on
104200140226     c                   eval      V1Cmsg = msg(16)
104300140226     c                   eval      wNoOkfuel = *off
104400140226     c                   ENDIF
104500931119     C*
104600120315     C                   write     TA01Z01
104700940624     C                   EXFMT     TA01D01
104800931119     C*
104900940526     C* PULIZIA CAMPO MESSAGGIO E RELATIVO INDICATORE (*IN28)
105000940526     C                   CLEAR                   V1CMSG
105100940526     C                   SETOFF                                       28
105200940526     C* AZZERO GLI INDICATORI RELATIVI AI MESSAGGI DI ERRORE
105300941012    1C     40            DO        49            I                 3 0
105400940526     C                   MOVE      '0'           *IN(I)
105500941012    1C                   ENDDO
105600150922     C                   SETOFF                                       51  45
105700941010     C                   SETOFF                                       548687
105800090706     c                   setoff                                       60
105900140224       //?Se il Fuel non è ok e non faccio F21 sempre errore
106000140224     c                   IF        wNoOkFuel and not *inkv
106100140224     c                             and not *inkc
106200140224     C                   goto      for01
106300140224     c                   ENDIF
106400140224
106500120314       //?F1 - dati manca tariffa
106600120314     c                   IF        *inka
106700120314     c                   exsr      gesw01
106800120314     c                   goto      For01
106900120314     c                   ENDIF
107000120314
107100011210     C*
107200011214     C* SE PREMUTO F10 E' COME SE FOSSE STATO PREMUTO F6
107300011214     C   KJ              MOVEL     *INKJ         *INKF
107400940519     C*
107500941012     C** CMD12 - RITORNO
107600120315      *  F3 - Fine
107700120315     c   kc
107800120315    1Cor KLVFLG          IFEQ      '5'
107900940503     C* SE PROVENGO DA AGGIUNTA TARIFFA NON ABILITO IL CMD12
108000120315     C   kl              MOVEL     MSG(21)       V1CMSG
108100120315     C   kc              MOVEL     MSG(20)       V1CMSG
108200940526     C                   SETON                                        4028
108300941012     C                   GOTO      FOR01
108400941012   X1C                   ELSE
108500991027     C* IN CASO DI IMMISISONE IMPOSTO COME FLAG DI RITORNO 'C'
108600991027     C   01              MOVEL     'C'           FLGRTN
108700900417     C                   GOTO      FINE
108800941012    1C                   ENDIF
108900931119     C*
109000931119     C** CMD16 - ANNULLAMENTO TOTALE TARIFFA
109100941012    1C     *INKQ         IFEQ      *ON
109200051108      * prima di annullare emetto una videata per chiedere conferma
109300051108     c                   ExSr      Sr_Confann
109400051108     c                   If        w11sino <> 'SI'
109500051108     c                   goto      for01
109600051108     c                   EndIf
109700941012     C                   EXSR      ANNTAR
109800020206     C                   MOVEL     'C'           FLGRTN
109900941012     C                   GOTO      FINE
110000941012    1C                   ENDIF
110100101013
110200101013      /free
110300101013       //?F4 = ISTAT
110400101013        IF  *inkd;
110500101013          clear TRTB12DS1;
110600101013          D12opz = '1';
110700130131          IF  *in17;
110800130104            D12opz = '5';
110900130104          ENDIF;
111000101013          kpjbu = TRTB12DS1;
111100101013          trtb12r1 (kpjba);
111200101013          TRTB12DS1 = kpjbu;
111300130104      /end-free
111400130104     c                   IF        *in10 and not *in50 and
111500150211     c                             not *in23 and *in17
111600130104     c                   eval      *in27 = *on
111700130104     c                   goto      For01
111800130104     c                   ENDIF
111900130104      /free
112000101013          IF  D12f12 = *blanks and D12err = *blanks;
112100101013            V1Crct    = D12sca;
112200101013            w_V1Crct  = D12sca;
112300101013            wDate_ISO = %date(D12data:*iso);
112400101013            wDate_Eur = wDate_ISO;
112500101013            V1Dist    = %dec(wDate_EUR);
112600101013          ENDIF;
112700101013          *in27 = *on;
112800101013          *in43 = *on;
112900101013        ENDIF;
113000101013      /end-free
113100101013     c                   IF        *inkd
113200101013     c                   goto      For01
113300101013     c                   ENDIF
113400101013
113500060116      *
113600030718      * F7 Visualizzazine storico variazioni
113700030718     c                   if        *inkg
113800030718     c                   feod      titav00f
113900030718     c                   clear                   tnta04ds
114000030718     c                   eval      i04ksc = v1cksc
114100030718     c                   eval      i04ctr = v1cctr
114200030718     c                   eval      i04prg = v1cprg
114300030718     c                   eval      i04rag = v1dksc
114400030718     c                   eval      i04dct = v1dctr
114500030718     c                   eval      i04dcv = v1cdcv
114600030718     c                   eval      i04div = v1cdiv
114700030718     c                   eval      i04ndf = v1cnet
114800030718      *
114900030718     c                   call      'TNTA04R'
115000030718     c                   parm                    tnta04ds
115100030718      *
115200030718     c                   goto      for01
115300030718      *
115400030718     c                   endif
115500120315       //?F24 Altri tasti
115600120315     c                   IF        *inky
115700120315     c                   exsr      gesf24
115800120315     c                   goto      For01
115900120315     c                   ENDIF
116000931119     C*
116100941012     C* CONTROLLI
116200941012     C                   EXSR      CTRD01
116300940729     C   27
116400941012     COR 28              GOTO      FOR01
116500991119     C* VALORIZZO LA DIVISA
116600991119     C                   MOVE      V1CDIV        VICDIV
116700940811     C*
116800950119     C** AGGIORNAMENTO
116900950119     C   10              EXSR      AGGTAM
117000050530     c
117100050530     c                   if        *in28
117200050530     c                   goto      for01
117300050530     c                   endif
117400950119     C*
117500941010     C* COMANDI
117600941010     C                   EXSR      COMAND
117700941010     C*
117800941012     C* WFLG = '1' ---> E' STATO PREMUTO UN TASTO DI FUNZIONE
117900941012    1C     WFLG          IFEQ      '1'
118000050530     C* SE PREMUTO F3 DALLE TARIF.PARTICOLARI
118100950116     C*   VADO A FINE PGM ALTRIMENTI RIEMETTO IL FORMATO
118200011214     C     *INKV         IFEQ      *ON
118300950117     C     FLG4          ANDNE     ' '
118400140224
118500140224     c                   IF        wNoOkFuel
118600140224     c                   goto      for01
118700140224     c                   ENDIF
118800050530     c
118900050530     c* Se premuto F3 - rieseguo controllo delle tariffe particolari in scad
119000050530     c                   eval      *inkv='0'
119100050530     c                   exsr      CTRTparEli
119200050530     c
119300050530     c                   if        *in28
119400050530     c                   goto      for01
119500050530     c                   endif
119600050530     c
119700991022     C                   MOVEL     'C'           FLGRTN
119800950116     C                   GOTO      FINE
119900950116     C                   ENDIF
120000950116     C*
120100941012     C                   GOTO      FOR01
120200941012    1C                   ENDIF
120300940811     C*
120400950119     C** CMD6 - AGGIORNAMENTO E FINE LAVORO
120500950119    1C     *INKF         IFEQ      *OFF
120600941012     C***
120700941012     C* EMISSIONE FORMATO 2
120800941012     C***
120900121228     C     FOR021        TAG
121000121227
121100121227       //?Imposto i tasti funzione nella riga mobile
121200121227     c                   eval      wPrima = *off
121300121227     c                   eval      wSeconda = *on
121400121227     c                   eval      wTerza = *off
121500121227     c                   eval      wQuarta = *off
121600121227     c                   ExSr      Tastifun
121700121227
121800121228     c     For02         tag
121900120906     C                   Z-ADD     0             V1CRDA
122000120906     C                   MOVEL     *BLANKS       V1CRA
122100941012     C                   CLEAR                   V2CGCR
122200941012     C                   CLEAR                   V2CGGR
122300941012     C                   CLEAR                   V2CGMR
122400941012     C                   CLEAR                   V2CGVR
122500021118     c                   Eval      *In39 = *Off
122600021118      * Se tariffa FedEx Documenti imposto la natura merce indicata in tabella
122700021118     c                   If        V1cNet = 'F' and DueCtr >= §DfeDalD
122800021118     c                             and DueCtr <= §DfeAlD
122900021118     c                             and §DfeNamD > *Blanks
123000021118     c                   Eval      V2cNas = §DfeNamD
123100021118     c                   Eval      *In39 = *On
123200021118     c                   EndIf
123300940729     C*
123400120319     C                   write     TA01Z02
123500940729     C                   EXFMT     TA01D02
123600940729     C*
123700940729     C* PULIZIA CAMPO MESSAGGIO E RELATIVO INDICATORE (*IN28)
123800940729     C                   CLEAR                   V1CMSG
123900940729     C* AZZERO GLI INDICATORI RELATIVI AI MESSAGGI DI ERRORE
124000940729     C                   SETOFF                                       777879
124100000118     C                   SETOFF                                       802849
124200120314       //?F1 - dati manca tariffa
124300120314     c                   IF        *inka
124400120314     c                   exsr      gesw01
124500120314     c                   goto      For02
124600120314     c                   ENDIF
124700011210     C*
124800011214     C* SE PREMUTO F10 E' COME SE FOSSE STATO PREMUTO F6
124900011214     C   KJ              MOVEL     *INKJ         *INKF
125000120315
125100120315       //?F3 - Fine
125200120315     c   kc              GOTO      FINE
125300940729     C*
125400940729     C** CMD12 - FINE LAVORO
125500960109     C   KL              GOTO      FOR012
125600940729     C*
125700940729     C** CMD16 - ANNULLAMENTO TOTALE TARIFFA
125800950119    2C     *INKQ         IFEQ      *ON
125900051108      * prima di annullare emetto una videata per chiedere conferma
126000051108     c                   ExSr      Sr_Confann
126100051108     c                   If        w11sino <> 'SI'
126200051108     c                   goto      for02
126300051108     c                   EndIf
126400941012     C                   EXSR      ANNTAR
126500020206     C                   MOVEL     'C'           FLGRTN
126600941012     C                   GOTO      FINE
126700950119    2C                   ENDIF
126800121227
126900121227      *
127000121227      * F7 Visualizzazine storico variazioni
127100121227     c                   if        *inkg
127200121227     c                   feod      titav00f
127300121227     c                   clear                   tnta04ds
127400121227     c                   eval      i04ksc = v1cksc
127500121227     c                   eval      i04ctr = v1cctr
127600121227     c                   eval      i04prg = v1cprg
127700121227     c                   eval      i04rag = v1dksc
127800121227     c                   eval      i04dct = v1dctr
127900121227     c                   eval      i04dcv = v1cdcv
128000121227     c                   eval      i04div = v1cdiv
128100121227     c                   eval      i04ndf = v1cnet
128200121227      *
128300121227     c                   call      'TNTA04R'
128400121227     c                   parm                    tnta04ds
128500121227      *
128600121227     c                   goto      for02
128700121227     c                   endif
128800121227
128900120315       //?F24 Altri tasti
129000120315     c                   IF        *inky
129100120315     c                   exsr      gesf24
129200121228     c                   goto      For02
129300120315     c                   ENDIF
129400940729     C*
129500941012     C* CONTROLLI
129600941012     C                   EXSR      CTRD02
129700940729     C   27
129800941012     COR 28              GOTO      FOR02
129900950119    1C                   ENDIF
130000941013     C*
130100931119     C** AGGIORNAMENTO
130200000000     C                   EXSR      AGGTAM
130300050530     c
130400050530     c                   if        *in28
130500050530     c                   goto      for02
130600050530     c                   endif
130700980309     C* PRIMA DI USCIRE DALL'AGGIORNAMENTO DEGLI ARCHIVI CONTROLLO SE
130800980309     C* IN CASO DI TARIFFA IMPORT LE TARIFFE SONO CORRETTE ALTRIMENTI
130900980309     C* EMETTO UN MESSAGGIO DI AVVERTIMENTO
131000980309     C     UNO           IFEQ      ' '
131100980309     C                   EXSR      CTRIMP
131200980309     C   28              MOVEL     '1'           UNO
131300980309     C   28              GOTO      FOR02
131400980309     C                   END
131500980309     C*
131600941011     C*
131700941011     C* COMANDI
131800941011     C                   EXSR      COMAND
131900941011     C*
132000950116     C* WFLG = '1' ---> E' STATO PREMUTO UN TASTO DI FUNZIONE
132100950119    1C     WFLG          IFEQ      '1'
132200011214     C* SE PREMUTO CMD3 DALLE TARIF.PARTICOLARI
132300950116     C*   VADO A FINE PGM ALTRIMENTI RIEMETTO IL FORMATO
132400011214     C     *INKV         IFEQ      *ON
132500950117     C     FLG4          ANDNE     ' '
132600950116     C                   GOTO      FINE
132700950116     C                   ENDIF
132800950116     C*
132900941012     C                   GOTO      FOR02
133000941013    1C                   ENDIF
133100920525     C*
133200920525     C** CMD6 - AGGIORNAMENTO E FINE LAVORO
133300990914     C* AGGIORNO IL FLAG DI RITORNO
133400990914     C   KF              MOVEL     'C'           FLGRTN
133500920525     C   KF              GOTO      FINE
133600000000     C** ENTER- AGGIORNAMENTO E AVANZAMENTO
133700921119     C*
133800000000     C     SUSF1         TAG
133900921118     C** 20 ON  - E' STATA BATTUTA UNA CHIAVE DI RICERCA
134000921118     C** 20 OFF - NON E' STATA BATTUTA NESSUNA CHAIVE DI RICERCA
134100940526     C     V1CRDA        COMP      0                                  2020
134200120723     C   20V1CRA         COMP      *BLANKS                            20
134300921118     C*
134400930224     C                   SETOFF                                       21
134500921118     C*
134600941011     C** 20 OFF - SIGNIFICA CHE NON E'STATA BATTUTA NESSUNA CHIAVE
134700990601     C**    DI RICERCA PER CUI ESEGUO CHAIN PER CLIENTE SU TITAD
134800941013    1C  N20              DO
134900061011     C  n50KTAM2         SETLL     TITAD000                               21
135000061011     c   50ktam2         setll     tiofd000                               21
135100900924     C   21              MOVEL     'A'           TADATB
135200941013    2C   21TADATB        DOWNE     *BLANKS
135300061011     C  n50KTAM2         READE     TITAD000                               32
135400061011     c   50ktam2         reade     tiofd000                               32
135500941013    2C  N32              ENDDO
135600921218     C   21
135700921218     CAN 32              SETOFF                                       21
135800061011     c   21
135900061011     Cann50KTAM2         SETLL     TITAD000
136000061011     c   21
136100061011     can 50ktam2         setll     tiofd000
136200941013    1C                   ENDDO
136300921118     C*
136400921118     C** 20 ON  - SIGNIFICA CHE E' STATA BATTUTA UNA CHIAVE DI RICERCA
136500921118     C**    PER CUI ESEGUO CHAIN PER CHIAVE DI RICERCA DA A
136600941013    1C   20              DO
136700120713     c                   IF        %scan('?':V1Cra) > *zeros
136800120713     c                   exsr      Ric_CT
136900120713     c                   eval      V1Cra = TB09cod
137000120713     c                   ENDIF
137100120906     c                   IF        v1cra = *blanks
137200120906     c                   goto      susf1
137300120906     c                   ENDIF
137400120906     c                   IF        v1cra <> *blanks
137500921118     C                   Z-ADD     1             A
137600940526     C     V1CRA         LOOKUP    CCT(A)                                 35
137700020215     C   35              MOVE      ORP(A)        VIDORP
137800940524     C  N35              MOVEL     *BLANKS       VIDORP
137900061011     C  n50KTTAD         CHAIN     TITAD000                           32
138000061011     c   50kttad         chain     tiofd000                           32
138100921118     C  N32              SETON                                        21
138200120723     c                   ENDIF
138300941013    1C                   ENDDO
138400921118     C*
138500941013     C** 21 ON  - SIGNIFICA CHE PER QUEL CLIENTE ESISTE ALMENO UNA RIGA
138600941013     C**    DI TARIFFA QUINDI VISUALIZZO LE TARIFFE CHE HO DALL'INIZIO
138700941013    1C     *IN21         IFEQ      *ON
138800941013     C** ESEGUO AGGIORNAMENTO
138900921118     C                   EXSR      AGGIOR
139000921118     C*
139100120315     c   kc              GOTO      FINE
139200121228     C   KL              GOTO      FOR021
139300921118     C** CMD6 - AGGIORNAMENTO E FINE LAVORO
139400980309     C* PRIMA DI USCIRE DALL'AGGIORNAMENTO DEGLI ARCHIVI CONTROLLO SE
139500980309     C* IN CASO DI TARIFFA IMPORT LE TARIFFE SONO CORRETTE ALTRIMENTI
139600980309     C* EMETTO UN MESSAGGIO DI AVVERTIMENTO
139700980310     C   KFUNO           IFEQ      ' '
139800980310     C                   EXSR      CTRIMP
139900980310     C   28              MOVEL     '1'           UNO
140000121228     C   28              GOTO      FOR021
140100980310     C                   END
140200980309     C*
140300011214     C   KF              GOTO      FINE
140400941013    1C                   ENDIF
140500921118     C*
140600941013     C** 21 OFF - SIGNIFICA CHE SI TRATTA DI IMMISSIONE COMPLETA IN
140700941013     C**    QUANTO NON ESISTE NEMMENO UNA RIGA DI TARIFFA PER IL CLIENTE
140800941013    1C     *IN21         IFEQ      *OFF
140900921118     C                   EXSR      IMMISS
141000980313     C* IN CASO DI TARIFFA IMPORT LA TARIFFA PARTICOLARE ASSIC. X CONTO
141100980313     C* DEVE ESSERE CORRETTA
141200980313     C     UNO           IFEQ      ' '
141300980313     C                   EXSR      CTRIMP
141400980313     C   28              MOVEL     '1'           UNO
141500121228     C   28              GOTO      FOR021
141600980313     C                   END
141700921118     C*
141800120315     c   kc              goto      fine
141900121228     C   KL              GOTO      FOR021
142000961231     C*
142100961231     C** CMD15-P O R T O  (ABILITATO SOLO NELLE OFFERTE)
142200980902     C   KP              DO
142300031124      * recupero dalla tabella CAT la data di elaborazione CAT consigliata
142400031124     c                   Clear                   Tibs02Ds
142500031124     c                   Clear                   dCat
142600031124     c                   Eval      T02Mod = 'C'
142700031124     c                   Eval      T02Sif = Knsif
142800031124     c                   Eval      T02Cod = 'CAT'
142900031124     c                   Movel(p)  '1'           T02Ke1
143000031124     c                   Call      'TIBS02R'
143100031124     c                   Parm                    Kpjba
143200031124     c                   Parm                    Tibs02Ds
143300031124     c                   If        T02Err = *Blanks
143400031124     c                   Movel     T02Uni        dCat
143500031124     c                   EndIf
143600031124    1 *
143700980514     C                   CLEAR                   DSTE00
143800980514     C                   MOVEL     'O02'         D00OP0
143900980514     C                   MOVEL     'F15'         D00OP1
144000980514     C                   MOVEL     'P'           D00TLA
144100091127      * Con l'arrivo delle trattative dobbiamo segnalare ai pgm del CAT che
144200091127      * stiamo parlando di trattativa e non di visita
144300091127      * Se trattativa (ind. 85 acceso) impostiamo "X" nel campo D00DSF
144400091125     C*                  MOVEL     'S'           D00DSF
144500091125     C   85              MOVEL     'X'           D00DSF
144600091127     C                   MOVEL     'O'           D00CTO
144700091125     C                   MOVEL     V1CPRG        D00PRG
144800980514     C                   MOVEL     V1CNRV        D00KSC
144900980514     C                   MOVEL     V1CCTR        D00CTR
145000980514     C                   MOVEL     V1CFIE        D00FIE
145100980514     C                   MOVEL     V1CTSP        D00TSP
145200011023     C                   MOVEL     V1CDIV        D00DIV
145300020205     C                   MOVE      V1CNET        D00DPD
145400011030     C                   MOVEL     *BLANKS       D00PRI
145500031124      * propongo data massima tra la data del giorno e quella recuperata
145600031124      * dalla tavella CAT
145700031124     c                   if        dateu8 > §CATdta
145800980514     C                   MOVEL     DATEU8        D00DTC
145900031124     C                   ELSE
146000031124     C                   MOVEL     §CATDTA       D00DTC
146100031124     C                   ENDIF
146200031124      *
146300980514     C                   CALL      'TNTE01C'
146400980514     C                   PARM                    KPJBA
146500980514     C                   PARM                    DSTE00
146600961231     C* D00ERR = "1" --> ERRORE
146700980514    3C     D00ERR        IFEQ      '1'
146800980514     C                   SETON                                          28
146900980514     C                   MOVEL     D00MSG        $MSG
147000980514    3C                   ENDIF
147100980902    2C                   ENDDO
147200961231     C*
147300921118     C** CMD6 - AGGIORNAMENTO E FINE LAVORO
147400980309     C* PRIMA DI USCIRE DALL'AGGIORNAMENTO DEGLI ARCHIVI CONTROLLO SE
147500980309     C* IN CASO DI TARIFFA IMPORT LE TARIFFE SONO CORRETTE ALTRIMENTI
147600980309     C* EMETTO UN MESSAGGIO DI AVVERTIMENTO
147700980310     C   KFUNO           IFEQ      ' '
147800980310     C                   EXSR      CTRIMP
147900980310     C   28              MOVEL     '1'           UNO
148000121228     C   28              GOTO      FOR021
148100980310     C                   END
148200980309     C*
148300921118     C   KF              GOTO      FINE
148400941013    1C                   ENDIF
148500921118     C*
148600921118     C                   GOTO      SUSF1
148700921118     C*
148800000000     C     FINE          TAG
148900921118     C** ELIMINAZIONE SU DISCO SUBFILE SF E S2
149000921118     C                   EXSR      ELISF
149100070523      * se tariffa/offerta allocata vado in fondo e non faccio + niente
149200070523     c                   if        parall = 'A'
149300070523     c                   goto      fondo
149400070523     c                   endif
149500921118     C*
149600940615     C** 81 ON  - INDICATORE DI MODIFICA TNTAM
149700990601     C** 30 ON  - INDICATORE DI MODIFICA TITAD
149800940111     C** ESEGUO L'AGGIORNAMENTO DELLA DATA DI VARIAZIONE SOLO SE *IN81
149900940114     C*   E' SPENTO E SE NON E' STATA ANNULLATA L'OFFERTA
150000941013     C   30
150100941013    1CANN81              DO
150200061011     C  n50KTAM2         CHAIN     TNTAM000                           01
150300061011     c   50ktam2         chain     tnofm000                           01
150400941221     C                   Z-ADD     DATEU8        TAMDUV
150500940511     C                   MOVEL     *BLANKS       TAMFTR
150600040913     C                   Z-ADD     dateu8        TAMDTR
150700061011     c  n01
150800061011     Cann50              UPDATE    TNTAM000
150900061011     c  n01
151000061011     Can 50              UPDATE    tnofm000
151100030714      * valorizzo il flag per la scrittura del file storico fasi
151200030714     c                   eval      mandet = 'S'
151300030714      *
151400941013    1C                   ENDDO
151500030716      * verifico:se è stato manutenzionato la testata e scrivo lo storico
151600030716     c                   if        mantes = 'S'
151700030716      * aggancio la testata per verificare se ho modificato qualcosa
151800061011     C  n50KTAM2         CHAIN(N)  TNTAM000
151900061011     c   50ktam2         chain(n)  TNofm000
152000061011     c                   if        %found
152100030716      * verifico i soli campi che mi interessano per la scrittura del record
152200030716      * con quelli salvati in COMODO$
152300030716     c                   if        tamtsp <> $tamtsp   or
152400030716     c                             tampag <> $tampag   or
152500041129     c                             tamppa <> $tamppa   or
152600030716     c                             tamflo <> $tamflo   or
152700030716     c                             tamkpm <> $tamkpm   or
152800030716     c                             tamata <> $tamata   or
152900030716     c                             tamarl <> $tamarl   or
153000030716     c                             tamarf <> $tamarf   or
153100030716     c                             tamaro <> $tamaro   or
153200030716     c                             tamfis <> $tamfis   or
153300030716     c                             tamfvd <> $tamfvd   or
153400030716     c                             tamftm <> $tamftm   or
153500031119     c                             tamfts <> $tamfts   OR
153600101013     c                             tamrct <> $tamrct   OR
153700060324     c                             tampri <> $tampri   or
153800151002     c                             tamfmp <> $tamfmp   or
153900151002     c                             tamtpr <> $tamtpr
154000030716      *
154100030716     c                   eval      tavtrd = 'TES'
154200030716     c                   eval      tavcta = 'M'
154300030716     c                   exsr      SUB_STORICO
154400030716     c                   endif
154500030716     c                   endif
154600030716      *
154700030716     c                   endif
154800030714      * verifico:se è stato manutenzionato il dettaglio scrivo lo storico
154900030714     c                   if        mandet = 'S'
155000030714     c* in questo caso scrivo lo storico per il dettaglio tariffario
155100030714     c                   eval      tavtrd = 'DET'
155200030714     c                   eval      tavcta = 'M'
155300030714     c                   exsr      SUB_storico
155400030716      *
155500030716     c                   clear                   mandet
155600030714      *
155700030714     c                   endif
155800030714      * verifico:se è stato manutenzionato le giacenze scrivo lo storico
155900030714     c                   if        mangia = 'S'
156000030714     c* in questo caso scrivo lo storico per il dettaglio tariffario
156100030714     c                   eval      tavtrd = 'GIA'
156200030714     c                   eval      tavcta = 'M'
156300030714     c                   exsr      SUB_storico
156400030716      *
156500030716     c                   clear                   mangia
156600030714      *
156700030714     c                   endif
156800030714     c*
156900000918     C*** CONTROLLO SE LE TARIFFE PARTICOLARI CHE SONO SEMPRE FISSE ESISTONO
157000000918     C*** GIA' IN TARIFFA  SE NON È ANNULLATA
157100000919     C*** NON ESEGUO PER LE TARIFFE DI CARTELLO
157200000919     C     *IN23         IFEQ      *OFF
157300070523     C  n50KTAM2         CHAIN(n)  TNTAM000
157400070523     c   50ktam2         chain(n)  tnofm000
157500061206     c                   if        %found and tamatb = *blanks
157600151217      * se tariffa e sono in variazione ma non posso variare la tariffa
157700151217      * non devo fare la inglo
157800151217     c                   IF        not *in50 and *in10 and *in17
157900151217     c                   goto      saltainglo
158000151217     c                   ENDIF
158100000918     C                   EXSR      INGLO
158200151217     c     saltainglo    tag
158300080522
158400080521      * se immissione (tariffa/offerta) devo creare in automatico
158500080521      * la tariffa particolare 'f' = Fuel
158600080521      * se non già presente
158700080610     c                   if        win01
158800080521     c                   exsr      sr_fuel
158900080521     c                   endif
159000080522
159100061206     c                   endif
159200000919     C                   ENDIF
159300030714      * verifico se sono state manutenzionate delle tariffe particolari
159400030714      * scrivo lo storico
159500030714     c                   z-add     0             WW
159600030714      *
159700030714     c                   do        *hival
159800030714     c                   add       1             ww
159900030714
160000030714     c                   if        ww > 150
160100030714     c                   leave
160200030714     c                   endif
160300030714
160400030714     c                   if        manftc(ww) = *blank
160500030714     c                   leave
160600030714     c                   endif
160700030714
160800030714     c                   eval      TAVtrd = 'PAR'
160900030714     c                   movel     manftc(WW)    TAVftc
161000030714     c                   move      manftc(WW)    TAVcta
161100030714     c                   exsr      SUB_storico
161200030714
161300030714     c                   enddo
161400030716      *
161500030716     c                   clear                   manftc
161600030717     c                   clear                   first
161700030717     c                   clear                   instes
161800040927     c                   clear                   annultar
161900000918     C*
162000061011     C  n50KTAM2         SETLL     TNTAM000                               01
162100061011     c   50ktam2         setll     tnofm000                               01
162200061011     C  n50KTAM2         SETLL     TITAD000                               01
162300061011     c   50ktam2         setll     tiofd000                               01
162400011210     C*
162500011214     C* SE PREMUTO F10: OLTRE AGLI AGGIORNAMENTI (DA F6)
162600011210     C*  DEVO FARE LA STAMPA TARIFFA.
162700011214     C   KJ              MOVE      '2'           PA2FLG
162800011214     C   KJ              EXSR      STAMPA
162900070523
163000070523     c     fondo         tag
163100070523
163200941007     C*
163300970718     C* CALL A VUOTO AL PGM FNLV13R PER CHIUDERE I FILES
163400970718    1C     FNLV13        IFEQ      '1'
163500970718     C                   MOVEL     'C'           I13TLA
163600970718     C                   CALL      'FNLV13R'
163700970718     C                   PARM                    KPJBA
163800970718     C                   PARM                    DSLV13
163900970718     C                   PARM                    DSSI95
164000941013    1C                   ENDIF
164100941011     C*
164200970122     C* CALL A VUOTO AL PGM TNTA47R PER CHIUDERE I FILES
164300970122     C     WTA47         IFEQ      '1'
164400970122     C                   CLEAR                   DSTA47
164500970122     C                   MOVEL     'C'           D47TLA
164600970122     C                   MOVEL     DSTA47        KPJBU
164700970122     C                   CALL      'TNTA47R'
164800970117     C                   PARM                    KPJBA
164900970117     C                   ENDIF
165000020205      *
165100020205     C* RITORNO PARAMETRI AL TNTA01R
165200020205     C                   MOVEL     PARAMX        KPJBU
165300120315     c                   IF        *in97
165400120315     c                   eval      parm48 = tnsb48ds
165500120315     c                   ENDIF
165600970117     C*
165700061012     c                   eval      *inlr = *on
165800940503     C*
165900941010     C*--- ROUTINE COMANDI -------------------------------------------*
166000941010     C     COMAND        BEGSR
166100941012     C                   MOVEL     ' '           WFLG              1
166200941010     C*
166300941010     C** CMD14 - GESTIONE TARIFFE GIACENZE
166400941012    1C     *INKN         IFEQ      *ON
166500091210     C                   MOVEL     V1CKSC        pa3cli
166600091210     C  n50              MOVEL     'C'           pa3AST
166700091210     C   50              MOVEL     'V'           pa3AST
166800091210     C                   Z-ADD     V1CCTR        pa3CTR
166900091210     C                   Z-ADD     V1CPRG        pa3PRG
167000981113     C                   MOVEL     V1DCTR        PA3DCT
167100981113     C                   MOVEL     V1CDCV        PA3DCV
167200030714     C                   CLEAR                   PA3MAN
167300961206     C                   MOVEL     V1CFIE        PA3FIE
167400990824     C                   MOVEL     V1CDIV        PA3DIV
167500990622     C                   MOVEL     §CVFDC        PA3FDC
167600020204      * NETWORK
167700020204     C                   MOVE      V1CNET        PA3NET
167800090916      * trattativa
167900090916     c                   eval      pa3trat = partrat
168000010629      *
168100941010     C                   MOVEL     PARAM3        KPJBU
168200060207     C                   CALL      'TNTA14R'
168300941010     C                   PARM                    KPJBA
168400950116     C                   MOVEL     KPJBU         PARAM3
168500030714      *
168600030714     c                   if        pa3MAN <> ' '
168700030714     c                   eval      mangia = 'S'
168800030714     c                   endif
168900950116     C*
169000950116     C* 96 ON  - ESISTONO TARIFFE GIACENZA   : EVIDENZIO IL COMANDO
169100950116     C                   SETOFF                                       96
169200990611     C  N50KTAM2         CHAIN(N)  TITGC000                           32
169300990611     C   50KTAM2         CHAIN(N)  TIOGC000                           32
169400950116    2C  N32TGCATB        IFEQ      ' '
169500950116     C                   SETON                                        96
169600950116    2C                   ENDIF
169700941010     C*
169800941012     C                   MOVEL     '1'           WFLG
169900941010     C                   GOTO      ENDCOM
170000941012    1C                   ENDIF
170100941010     C*
170200941010     C** CMD21 - GESTIONE TARIFFE PARTICOLARI
170300941012    1C     *INKV         IFEQ      *ON
170400140224
170500140224
170600980309     C* CONTROLLO SE LA TARIFFA CHE STO MANUTENZIONANDO E' UNA TARIFFA IMPORT
170700980309     C*
170800980309     C                   EXSR      CTRIMP
170900980309     C*
171000941010     C                   Z-ADD     V1CKSC        CLI5
171100941010     C                   MOVEL     V1DKSC        RAG5
171200941010     C  N50              MOVEL     ' '           FLG
171300941010     C   50              MOVEL     'A'           FLG
171400941010     C                   MOVEL     ' '           FLG4
171500990622     C* MUOVO FLAG DECIMALI SI/NO IN BASE ALLA DIVISA
171600991005     C                   MOVEL     §CVFDC        PA5FDC
171700000915     C* PASSO LA DATA DECORRENZA DELLA TARIFFA
171800000915     C                   Z-ADD     COMDDT        PA5DDT
171900050531     C                   movel     COMDsT        PA5DsT
172000000915     C* PASSO IL TIPO SERVIZIO BOLLA
172100000915     C                   MOVE      V1CTSP        PA5TSP
172200020201     C* Flag Network
172300020201     C                   MOVEL     V1CNET        FLGNET
172400060306      * flag rinuncia AC BASE
172500060306     c                   movel     v2cfmp        pa5fmp
172600090916      * trattativa
172700090916     c                   eval      pa5trat = partrat
172800000915     C*
172900941010     C                   MOVEL     PARAM5        KPJBU
173000120320     c                   IF        not *in97
173100030714     C                   CALL      'TNTA67R'
173200941010     C                   PARM                    KPJBA
173300030714     c                   parm                    manftc
173400120320     c                   ELSE
173500120320     C                   CALL      'TNTA67R'
173600120320     C                   PARM                    KPJBA
173700120320     c                   parm                    manftc
173800120320     c                   parm                    tnsb48ds
173900120320     c                   ENDIF
174000941010     C*
174100941010     C                   MOVEL     KPJBU         PARAM5
174200140224
174300140224      * ricontrollo il Fuel
174400140224     c                   exsr      ctr_fuel
174500140224
174600950116     C*
174700950116     C* SOLO SE NON E' STATO PREMUTO CMD3(FLG4=' ') CONTROLLO SE SONO
174800950116     C*   PRESENTI TARIFFE PARTICOLARI PER EVIDENZIARE IL COMANDO
174900950116    2C     FLG4          IFEQ      ' '
175000950116     C* 95 ON  - ESISTONO TARIFFE PARTICOLARI: EVIDENZIO IL COMANDO
175100950116     C                   SETOFF                                       95
175200061011     C  n50KTAM2         SETLL     TITPT000
175300061011     c   50ktam2         setll     tiopt000
175400061011     C  n50KTAM2         READE(N)  TITPT000                               32
175500061011     C   50ktam2         reade(n)  tiopt000                               32
175600950116    3C     *IN32         DOWEQ     *OFF
175700950116     C     TPTATB        IFEQ      ' '
175800950116     C                   SETON                                        3295
175900950116     C                   ELSE
176000061011     C  n50KTAM2         READE(N)  TITPT000                               32
176100061011     c   50ktam2         reade(n)  tiopt000                               32
176200950116     C                   ENDIF
176300950116    3C                   ENDDO
176400950116     C*
176500950116    2C                   ENDIF
176600941010     C*
176700941011     C                   MOVEL     '1'           WFLG
176800941010     C                   GOTO      ENDCOM
176900941012    1C                   ENDIF
177000950116     C*
177100941010     C** CMD18 - GESTIONE NOTE
177200100407      /free
177300100806       //?Nuovo programma gestione note
177400100806        IF  *inks;
177500100407          clear trmk20ds;
177600110511          IMK20TLA = 'L' ;
177700100407
177800101126         //?passo trattativa  (abbiamo tolto richiamo delle note da trattativa)
177900100407          IF  *in85;
178000100407            IMK20nrv = v1cksc;
178100100407         //?passo cliente
178200100407          ELSE;
178300100407            IMK20ksc = v1cksc;
178400100407          ENDIF;
178500100407         //?ragione sociale
178600100407          IMK20rsc = v1dksc;
178700100407
178800100407          trmk20r (kpjba : TRMK20DS);
178900100407          wflg = '1';
179000100407          leavesr;
179100100407        ENDIF;
179200100806
179300100407       //?F8 - Note tariffa
179400100806        IF  *inkh;
179500100407          clear TNTA28ds;
179600100407
179700100407          ITA28ela = 'M';
179800100407
179900100407         //?richiamo per trattativa
180000100407          IF  *in85;
180100100407            ITA28tip = 'T';
180200100407          ELSE;
180300100407         //?richiamo per tariffe clienti
180400100407            ITA28tip = 'C';
180500100407          ENDIF;
180600100407
180700100407          ITA28cod = V1cksc;
180800100407          ITA28rsc = V1dksc;
180900100407          kpjbu    = TNTA28DS;
181000100407
181100100407          tnta28r (kpjba);
181200100407          wflg = '1';
181300100407          leavesr;
181400100407        ENDIF;
181500100407      /end-free
181600090709
181700090709     C** F2=RUBRICA
181800090709    1c                   if        *inkb
181900090709     c                   clear                   tnta12ds
182000090709     c                   eval      ta12ric = 'M'
182100090709     c  n50              Eval      ta12apl = 'C'
182200090709     c  n50              Eval      ta12ksc = v1cksc
182300090916     c  n85
182400090916     can 50              Eval      ta12apl = 'V'
182500090916     c   85              Eval      ta12apl = 'T'
182600090709     c   50              Eval      ta12nrv = v1cnrv
182700090709     c                   Eval      ta12rag = v1dksc
182800090709     c                   Call      'TNTA12R'
182900090709     c                   Parm                    kpjba
183000090709     c                   Parm                    tnta12ds
183100090709     C*
183200090709     C                   MOVEL     '1'           WFLG
183300090709     C                   GOTO      ENDCOM
183400090709    1C                   ENDIF
183500941010     C*
183600941010     C     ENDCOM        ENDSR
183700941010     C*
183800941013     C*--- PULIZIA CAMPI VIDEATA -------------------------------------*
183900941012     C     INZD01        BEGSR
184000031118     C                   SETOFF                                       8836
184100941124     C                   CLEAR                   WSAP
184200980505     C                   CLEAR                   WRPV
184300980505     C                   CLEAR                   WRKMIN
184400940503     C*
184500940526     C                   Z-ADD     PRG           V1CPRG
184600020201     C                   CLEAR                   V1CNET
184700000118     C                   CLEAR                   V1CDDT
184800940526     C                   CLEAR                   V1CDST
184900940526     C                   CLEAR                   V1CDUV
185000940526     C                   CLEAR                   V1DCTR
185100940526     C                   CLEAR                   V1CDCV
185200101013     C                   CLEAR                   V1Crct
185300940526     C                   CLEAR                   V1CPRI
185400031118     C                   CLEAR                   V1DIST
185500031118     c                   CLEAR                   PARIST
185600940526     C                   CLEAR                   V1CPAG
185700041129     C                   CLEAR                   V1CPpa
185800940526     C                   CLEAR                   V1CKPM
185900940526     C                   CLEAR                   V1CMPC
186000940526     C                   CLEAR                   V1CARL
186100940526     C                   CLEAR                   V1CARF
186200940526     C                   CLEAR                   V1CARO
186300940526     C                   CLEAR                   V1CATA
186400940526     C                   CLEAR                   V1CFTP
186500940526     C                   CLEAR                   V1CBAP
186600940526     C                   CLEAR                   V1CTSP
186700940811     C                   CLEAR                   V1DTSP
186800041012     c                   Eval      v1cdiv = 'EUR'
186900990616     C                   CLEAR                   V1CMIS
187000990616     C                   CLEAR                   V1CMAS
187100940526     C                   CLEAR                   V1CFIE
187200980505     C                   CLEAR                   V1CMAT
187300150922     C                   CLEAR                   V1Ctpr
187400941108     C                   CLEAR                   WFIE
187500020215     C                   CLEAR                   WTSP
187600020215     C                   CLEAR                   WNET
187700940705     C                   MOVEL     '0'           V2CFIS
187800960621     C  N50              MOVEL     'S'           V2CSIS
187900960621     C   50              CLEAR                   V2CSIS
188000940705     C                   MOVEL     '1'           V2CFTS
188100941108     C* 16 ON  - TIPO TARIFFA "4"-A VALORE
188200940811     C   16              MOVEL     '1'           V2CFVD
188300970128     C                   MOVEL     'N'           V2CBAM
188400940705     C                   CLEAR                   V2CNAS
188500040601     c                   Clear                   v2cbva
188600940705     C                   CLEAR                   V2CNOT
188700060303     c                   clear                   v2cfmp
188800940705     C                   CLEAR                   V2CFTM
188900940705     C                   CLEAR                   V2CGCA
189000940705     C                   CLEAR                   V2CGGA
189100940705     C                   CLEAR                   V2CGMA
189200940705     C                   CLEAR                   V2CGVA
189300941012     C                   CLEAR                   V2CGCR
189400941012     C                   CLEAR                   V2CGGR
189500941012     C                   CLEAR                   V2CGMR
189600941012     C                   CLEAR                   V2CGVR
189700940503     C*
189800131028     C                   CLEAR                   savpri
189900131028     C*
190000970317     C                   MOVEL     *BLANKS       COMODO
190100030716     C                   MOVEL     *BLANKS       COMODO$
190200940526     C                   MOVE      VCTR          V1CCTR
190300000000     C                   Z-ADD     0             COMLRC            7 0
190400000000     C                   Z-ADD     0             COMLAS            7 0
190500061011      * se sono in inserimento e non in copia 01 = *on
190600031118      * propongo il primo scaglione ISTAT con data scatto successiva
190700031118      * la data del giorno
190800061011     c                   If        *in01
190900120124     C                   Z-ADD     0             PI                4 0
191000031118     c                   z-add     0             svdia             8 0
191100101013     c                   z-add     0             svrct             3 0
191200101013    3C                   DO        999           PI
191300031118      * verifico la data scatto con la data del giorno
191400031121    3C                   IF        dateu8 < DIA(PI)
191500031118      * salvo la data e lo scaglione
191600031118     c                   eval      svdia = dia(pi)
191700101013     c                   eval      svrct = pi
191800031121     c                   leave
191900031118     c                   endif
192000031118      *
192100031118     c                   enddo
192200031118      * se non trovato scaglione ritorno al TNTA01R con  errore bloccante
192300101013     c                   if        svdia = 0  and svrct = 0
192400031118     c                   eval      parist = 'E'
192500031118     c                   seton                                        36
192600031118     c                   goto      endinz
192700031118     c                   endif
192800031118      *
192900101013     c                   eval      v1crct = svrct
193000031118     c                   eval      v1cpri = 100
193100031118      *
193200031118     c                   z-add     svdia         g02INV
193300031118     c                   movel     '3'           g02err
193400031118     c                   CALL      'XSRDA8'
193500031118     c                   PARM                    WLBDAT
193600031118      * GG/MM/AAAA
193700031118     c                   z-add     g02DAT        v1diST
193800031118      *
193900031118     c                   endif
194000031118      *
194100991119     C*
194200941012     C*
194300941012     C* DECODIFICA CODICE TARIFFA
194400941012     C                   Z-ADD     1             A
194500920525     C                   MOVEL     VCTR          UNOCTR
194600940524     C     UNOCTR        LOOKUP    CTR(A)                                 35
194700941108    1C     *IN35         IFEQ      *ON
194800941108     C                   MOVEL     DTR(A)        V1DCTR
194900941124     C* 88 ON  - IMPORTO TARIFFA IN PERCENTUALE
195000941108    2C     TAP(A)        IFEQ      'S'
195100941123     C                   SETON                                        88
195200941108    2C                   ENDIF
195300950116     C* WSAP = "S" ---> SCAGLIONE CON DECIMALI
195400941124     C                   MOVEL     SAP(A)        WSAP              1
195500980505     C* WRPV = "S" ---> ABILITATA LA GESTIONE DEL RAPPORTO PESO VOLUME
195600980505     C                   MOVEL     RPV(A)        WRPV              1
195700980505     C* WRKMIN = SALVO IL MINIMO TASSABILE PER QUESTA TARIFFA
195800980505     C                   Z-ADD     MIN(A)        WRKMIN           13 3
195900941108    1C                   ENDIF
196000941012     C*
196100061011     C* CONTROLLO SE DEVO EVIDENZIARE I COMANDI
196200950121     C                   SETOFF                                       9596
196300950121     C*
196400950116     C* 95 ON  - ESISTONO TARIFFE PARTICOLARI: EVIDENZIO IL COMANDO
196500061011     C  n50KTAM2         SETLL     TITPT000
196600061011     c   50ktam2         setll     tiopt000
196700061011     C  n50KTAM2         READE(N)  TITPT000                               32
196800061011     c   50ktam2         reade(n)  tiopt000                               32
196900950116    3C     *IN32         DOWEQ     *OFF
197000950116     C     TPTATB        IFEQ      ' '
197100950116     C                   SETON                                        3295
197200950116     C                   ELSE
197300061011     C  n50KTAM2         READE(N)  TITPT000                               32
197400061011     c   50ktam2         reade(n)  tiopt000                               32
197500950116     C                   ENDIF
197600950116    3C                   ENDDO
197700950116     C*
197800950116     C* 96 ON  - ESISTONO TARIFFE GIACENZA   : EVIDENZIO IL COMANDO
197900990611     C  N50KTAM2         CHAIN(N)  TITGC000                           32
198000990611     C   50KTAM2         CHAIN(N)  TIOGC000                           32
198100950116     C  N32TGCATB        IFEQ      ' '
198200950116     C                   SETON                                        96
198300950116     C                   ENDIF
198400080610      * salvo l'indicatore di nuova tariffa/offerta
198500080610     c                   eval      win01 = *in01
198600950116     C*
198700031118     C     endinz        ENDSR
198800940503     C*
198900941013     C*--- CONTROLLO DATI FORMATO 1 ----------------------------------*
199000941012     C     CTRD01        BEGSR
199100940729     C                   SETOFF                                       27
199200940503     C*
199300940503     C****  DATA DECORRENZA TARIFFA  ****
199400940729     C                   Z-ADD     V1CDDT        G02DAT
199500940701     C                   MOVEL     *BLANKS       G02ERR
199600940701     C                   CALL      'XSRDA8'
199700940503     C                   PARM                    WLBDAT
199800940701    1C     G02ERR        IFEQ      '1'
199900940526     C                   MOVEL     MSG(1)        V1CMSG
200000940526     C                   SETON                                        4028
200100940503     C                   GOTO      ENDCTR
200200940503   X1C                   ELSE
200300940701     C                   MOVE      G02INV        COMDDT            8 0
200400940729     C*
200500940729     C* IMPOSTO A VIDEO LA DATA A 8 SE IMMESSA A 6
200600940729     C                   Z-ADD     G02DAT        V1CDDT
200700940503    1C                   ENDIF
200800940503     C*
200900940503     C****  DATA SCADENZA TARIFFA  ****
201000940729     C                   Z-ADD     V1CDST        G02DAT
201100940701     C                   MOVEL     *BLANKS       G02ERR
201200940701     C                   CALL      'XSRDA8'
201300940503     C                   PARM                    WLBDAT
201400940701    1C     G02ERR        IFEQ      '1'
201500940526     C                   MOVEL     MSG(2)        V1CMSG
201600940526     C                   SETON                                        4128
201700940503     C                   GOTO      ENDCTR
201800940503   X1C                   ELSE
201900940701     C                   MOVE      G02INV        COMDST            8 0
202000940729     C*
202100940729     C* IMPOSTO A VIDEO LA DATA A 8 SE IMMESSA A 6
202200940729     C                   Z-ADD     G02DAT        V1CDST
202300940503    1C                   ENDIF
202400940503     C*
202500940503     C* LA DATA SCADENZA DEVE ESSERE MAGGIORE DELLA DATA DECORRENZA
202600940503    1C     COMDST        IFLT      COMDDT
202700940526     C                   MOVEL     MSG(3)        V1CMSG
202800940526     C                   SETON                                        4128
202900940503     C                   GOTO      ENDCTR
203000940503    1C                   ENDIF
203100120124
203200120124      /free
203300120124       //?La data scadenza non deve essere superiore ad oggi + 5 anni
203400120124       IF  comdst > 0;
203500120124         wDate_ISO = %date(comdst:*iso);
203600120124         IF  %diff(wDate_ISO : wOggi : *years) >= 5;
203700120124           V1Cmsg = msg(08);
203800120124           *in28 = *on;
203900120124           *in41 = *on;
204000120124           leavesr;
204100120124         ENDIF;
204200120124       ENDIF;
204300120124      /end-free
204400120124
204500900327     C*
204600940503     C* VFLG <> '1'  --->  S O L O    P E R    T A R I F F E
204700940503     C*                    CONTROLLO CONGRUENZA DECORRENZE/SCADENZE
204800940503    1C     VFLG          IFNE      '1'
204900940503     C*
205000940503     C* LA DATA SCADENZA DEL PROGRESSIVO PRECEDENTE DEVE ESSERE MINORE
205100940503     C*   DELLA DATA DECORRENZA DEL PROGRESSIVO SEGUENTE
205200940526     C                   Z-ADD     V1CKSC        COMKSC
205300900620     C                   Z-ADD     VCTR          COMCTR
205400940503     C     PRG           ADD       1             COMPRG
205500940503     C*
205600940615     C     KTAM5         SETLL     TNTAM000
205700940615     C     KTAM5         READE(N)  TNTAM000                               32
205800940503     C*
205900940510    2C     *IN32         DOWEQ     *OFF
206000940509     C     TAMATB        IFEQ      ' '
206100940509     C     COMDST        IFGE      TAMDDT
206200940526     C                   MOVEL     MSG(4)        V1CMSG
206300940526     C                   SETON                                        4128
206400940503     C                   GOTO      ENDCTR
206500940503     C                   ELSE
206600940510     C                   SETON                                        32
206700940503     C                   ENDIF
206800940503     C*
206900940503     C                   ELSE
207000940503     C* RECORD ANNULLATO: AGGIUNGO 1 E CERCO IL PROGRESSIVO SEGUENTE
207100940503     C                   ADD       1             COMPRG
207200940503     C                   ENDIF
207300940503     C*
207400940615     C  N32KTAM5         READE(N)  TNTAM000                               32
207500940503    2C                   ENDDO
207600940503     C*
207700940503     C* LA DATA DECORRENZA DEL PROGRESSIVO SEGUENTE DEVE ESSERE
207800940503     C*   MAGGIORE DELLA DATA SCADENZA DEL PROGRESSIVO PRECEDENTE
207900940503     C     PRG           SUB       1             COMPRG
208000940615     C     KTAM5         SETLL     TNTAM000
208100940615     C     KTAM5         READE(N)  TNTAM000                               32
208200940503     C*
208300940510    2C     *IN32         DOWEQ     *OFF
208400940503     C     TAMATB        IFEQ      ' '
208500940503     C     COMDDT        IFLE      TAMDST
208600940526     C                   MOVEL     MSG(5)        V1CMSG
208700940526     C                   SETON                                        4028
208800940503     C                   GOTO      ENDCTR
208900940503     C                   ELSE
209000940506     C*
209100940509     C* LA DATA DECORRENZA PROGRESSIVO SEGUENTE DEVE ESSERE MAGGIORE
209200940509     C*   DI 1 GIORNO DELLA DATA SCADENZA PROGRESSIVO PRECEDENTE
209300940510     C                   EXSR      CTRDAT
209400940506     C*
209500940510     C     COMDDT        IFNE      DATA
209600940526     C                   MOVEL     MSG(5)        V1CMSG
209700940526     C                   SETON                                        4028
209800940506     C                   GOTO      ENDCTR
209900940510     C                   ENDIF
210000940506     C*
210100940510     C                   SETON                                        32
210200940503     C                   ENDIF
210300940503     C*
210400940503     C                   ELSE
210500940503     C* RECORD ANNULLATO: SOTTRAGGO 1 E CERCO IL PROGRESSIVO PRECEDENTE
210600940503     C                   SUB       1             COMPRG
210700940506     C* RIESEGUO POSIZIONAMENTO CON PROGRESSIVO PRECEDENTE
210800940615     C     KTAM5         SETLL     TNTAM000
210900940503     C                   ENDIF
211000940503     C*
211100940615     C  N32KTAM5         READE(N)  TNTAM000                               32
211200940503    2C                   ENDDO
211300940503     C****
211400940509    1C                   ENDIF
211500130318
211600130318      /free
211700130320       //?Manutenzione Tariffa no cartello
211800130320       IF  not *in50 and *in10 and not *in23;
211900130318       //?Data decorrenza
212000130318       //?se <= data ultima spedizione fatturata
212100130318       //?non posso modificarla
212200130318       IF  sav_TAMddt <= wDataSP and sav_TAMddt <> comddt;
212300130318         *in40 = *on;
212400130318         *in28 = *on;
212500130318         V1Cmsg = 'Data Decorezza non modificabile, minore '+
212600130318                  'data ultima sped.fatt. ' +
212700130318                  %subst(%editc(wDataSP:'X'):7:2) + '/' +
212800130318                  %subst(%editc(wDataSP:'X'):5:2) + '/' +
212900130318                  %subst(%editc(wDataSP:'X'):1:4);
213000130318         leavesr;
213100130318       ENDIF;
213200130318       //?se > data ultima spedizione fatturata
213300130318       //?posso impostarla solo > alla data ultima sp.fatturata
213400130318       IF  sav_TAMddt > wDataSP and comddt <> sav_TAMddt and
213500130318           comddt <= wDataSP;
213600130318         *in40 = *on;
213700130318         *in28 = *on;
213800130318         V1Cmsg = 'Data Decor. deve essere più '+
213900130318                  'alta della data ultima sped.fatt. ' +
214000130318                  %subst(%editc(wDataSP:'X'):7:2) + '/' +
214100130318                  %subst(%editc(wDataSP:'X'):5:2) + '/' +
214200130318                  %subst(%editc(wDataSP:'X'):1:4);
214300130318         leavesr;
214400130318       ENDIF;
214500130318
214600130318       //?Data scadenza
214700130318       //?se <= data ultima spedizione fatturata
214800130318       //?non posso metterla più bassa di quella che c'è
214900130318       //?già sul file
215000130318       IF  sav_TAMdst <= wDataSP and comdst <> sav_TAMdst and
215100130318           comdst < sav_TAMdst;
215200130318         *in41 = *on;
215300130318         *in28 = *on;
215400130318         V1Cmsg = 'Data scadenza <= data ultima sped.fatt. '+
215500130318                  %subst(%editc(wDataSP:'X'):7:2) + '/' +
215600130318                  %subst(%editc(wDataSP:'X'):5:2) + '/' +
215700130318                  %subst(%editc(wDataSP:'X'):1:4) + ' ' +
215800130318                  'non si può anticipare.';
215900130318         leavesr;
216000130318       ENDIF;
216100130318       //?se > data ultima spedizione fatturata
216200130318       //?posso impostarla solo >= alla data ultima sp.fatturata
216300130318       IF  sav_TAMdst > wDataSP and comdst <> sav_TAMdst and
216400130318           comdst < wDataSP;
216500130318         *in41 = *on;
216600130318         *in28 = *on;
216700130319         V1Cmsg = 'Data Scadenza deve essere = o più '+
216800130318                  'alta della data ultima sped.fatt. ' +
216900130318                  %subst(%editc(wDataSP:'X'):7:2) + '/' +
217000130318                  %subst(%editc(wDataSP:'X'):5:2) + '/' +
217100130318                  %subst(%editc(wDataSP:'X'):1:4);
217200130318         leavesr;
217300130318       ENDIF;
217400130320       ENDIF;
217500130318      /end-free
217600151029
217700151029     C****  CODICE DIVISA    ****
217800151029     C                   MOVEL     'CV'          COD
217900151029     C                   MOVEL     *BLANKS       KEY
218000151029     C                   MOVEL     V1CDIV        KEY
218100151029     C     KTAB          CHAIN     TABEL                              32
218200151029    1C  N32TBLFLG        IFEQ      ' '
218300151029     C                   MOVEL     TBLUNI        DSCV
218400151029    1C                   ENDIF
218500900327     C*
218600941010     C****  FLAG ITALIA/ESTERO  ****
218700941028     C* PER LE TARIFFE CLIENTI     DEVE ESSERE OBBLIGATORIAMENTE PIENO
218800941028     C* PER LE TARIFFE DI CARTELLO DEVE ESSERE OBBLIGATORIAMENTE VUOTO
218900941028    1C     *IN23         IFEQ      *OFF
219000941028     C* 23 OFF - TARIFFE CLIENTI
219100941028    2C     V1CFIE        IFEQ      ' '
219200941010     C                   MOVEL     MSG(50)       V1CMSG
219300941010     C                   SETON                                        8728
219400941010     C                   GOTO      ENDCTR
219500941028    2C                   ENDIF
219600941028     C*
219700941107     C* CONTROLLO VALIDITA' DEL FLAG ITALIA/ESTERO
219800941107    2C     V1CFIE        IFNE      'I'
219900941109     C     V1CFIE        ANDNE     'E'
220000941107     C                   SETON                                        8728
220100941107     C                   MOVEL     MSG(51)       V1CMSG
220200941107     C                   GOTO      ENDCTR
220300941107    2C                   ENDIF
220400941107     C*
220500941028   X1C                   ELSE
220600941028     C* 23 ON  - TARIFFE DI CARTELLO
220700941028    1C                   ENDIF
220800941028     C*
220900020205     C* IN VARIAZIONE(10 ON) NON E' POSSIBILE VARIARE IL FLAG ITALIA/ESTERO
221000020213     C* SE DIVERSO DA BLANK
221100020213     C*
221200941108    1C   10V1CFIE        IFNE      WFIE
221300020213     C     WFIE          ANDNE     ' '
221400941107     C                   MOVEL     MSG(53)       V1CMSG
221500941012     C                   SETON                                        8728
221600941012     C                   GOTO      ENDCTR
221700941028    1C                   ENDIF
221800941010     C*
221900940504     C****  FLAG ISTAT  ****
222000031118     C* SE E' STATO INSERITO IL FLAG ISTAT OCCORRE INSERIRE ANCHE
222100031118     C*   LA % ADDEBITO ISTAT E VICEVERSA
222200101013    1C     V1Crct        IFGT      0
222300031118     C     V1CPRI        IFEQ      0
222400031118     C                   MOVEL     MSG(6)        V1CMSG
222500031118     C                   SETON                                        4228
222600031118     C                   GOTO      ENDCTR
222700031118     C                   ENDIF
222800031119      * se non esiste lo scaglione (data scatto uguale a zero) do errore
222900031119      *
223000101013     c                   if        dia(v1crct) = 0
223100031119     c                   movel     msg(113)      v1cmsg
223200031119     c                   seton                                        4328
223300031119     c                   goto      endctr
223400031119     c                   endif
223500131029      *
223600131029     c                   if        savpri <> v1cpri
223700131113     c*                  clear                   forzamsgistat
223800131029     c                   endif
223900131113      * se sono in immisisone imposto tampri uguale a zero
224000131113     c                   If        *in01
224100131113     c                   clear                   tampri
224200131113     c                   endif
224300131031      *
224400131028      * la percentuale di addebito non deve essere > di 100 o < di 50
224500131028      * se
224600131028     c                   If        v1cpri > 100 OR V1CPRI < 50
224700131029      *
224800131028      * se sono in gestione OFFERTA su trattativa NUOVA quindi senza cliente
224900131028      * ERRORE NON FORZABILE
225000131029     c                   if        (*in50 and ksc2 = *zeros)
225100050302     c                   Eval      v1cmsg = msg(114)
225200050303     c                   Eval      *In42 = *On
225300050302     c                   Eval      *In28 = *On
225400050302     c                   Goto      endctr
225500050302     c                   EndIf
225600131028      * se sto gestendo un'offerta legata ad una trattativa su cliente
225700131028      * il messaggio  sulla % applicazione ISTAT diventa forzabile
225800131113      * se si mantiene la stessa % del file ma se si modifca dare errore
225900131113      * bloccante se non soddisfa >=50 e <= 100
226000131029      *                  oppure
226100131029      *
226200131113      * se sto aggiungendo un nuovo progressivo la 1° volta è un messaggio
226300131113      * informativo
226400131029     c                   if        ((*in50 and ksc2 > *zeros) or
226500131113     c                              not *in17 )
226600131113
226700131113     c                   if        savpri <> v1cpri and
226800131113     c                             tampri = v1cpri and
226900131113     c                             forzaMsgIstat = *blanks
227000131028     c                   Eval      v1cmsg = msg(122)
227100131028     c                   Eval      *In42 = *On
227200131028     c                   Eval      *In28 = *On
227300131028     c                   Eval      forzaMsgIstat = '1'
227400131028     c                   eval      savpri = v1cpri
227500131028     c                   Goto      endctr
227600131113     c                   endif
227700131113      *
227800131113     c                   If        v1cpri <> tampri
227900131113     c                   Eval      v1cmsg = msg(114)
228000131113     c                   Eval      *In42 = *On
228100131113     c                   Eval      *In28 = *On
228200131113     c                   Goto      endctr
228300131113     c                   EndIf
228400131113      *
228500131028     c                   EndIf
228600031119      *
228700131029     c                   EndIf
228800131029      *
228900031118      * visualizzo la decorrenza
229000031118      *
229100101013     c                   z-add     dia(v1crct)   g02INV
229200031118     c                   movel     '3'           g02err
229300031118     c                   CALL      'XSRDA8'
229400031118     c                   PARM                    WLBDAT
229500031118      * UDATE A 8 IN GG/MM/AAAA
229600031118     c                   z-add     g02dat        v1diST
229700031119      * se cambiato scaglione istat riemetto la videata per visualizzare
229800031119      * la data decorrenza
229900101013     C                   IF        V1Crct <> W_V1Crct
230000031119     C                   SETON                                        27
230100101013     c                   eval      W_v1crct = v1crct
230200031119     C                   ENDIF
230300031118     C*
230400031118   X1C                   ELSE
230500031118     C     V1CPRI        IFGT      0
230600031118     C                   MOVEL     MSG(7)        V1CMSG
230700031118     C                   SETON                                        4328
230800031118     C                   GOTO      ENDCTR
230900031118     C                   ENDIF
231000031118      *
231100031118     c                   clear                   v1dist
231200031118    1C                   ENDIF
231300031118
231400940503     C*
231500940504     C****  CODICE TARIFFA  ****
231600900621     C                   Z-ADD     1             A
231700940526     C                   MOVEL     V1CCTR        UNOCTR            1 0
231800021118     c                   Move      V1cCtr        DueCtr            2 0
231900940524     C     UNOCTR        LOOKUP    CTR(A)                                 35
232000940524    1C     *IN35         IFEQ      *OFF
232100940526     C                   MOVEL     MSG(9)        V1CMSG
232200000118     C                   SETON                                        28
232300940504     C                   GOTO      ENDCTR
232400940509    1C                   ENDIF
232500021118
232600940504     C* DESCRIZIONE CODICE TARIFFA
232700940526     C                   MOVEL     DTR(A)        V1DCTR
232800940504     C*
232900940504     C****  VOLUME AUTOMATICO  ****
233000940504     C* 50 ON  -  S O L O    P E R    O F F E R T E
233100940504     C* SE SI TRATTA DI CLIENTE NUOVO, VOLUME AUTOMATICO OBBLIGATORIO
233200940509    1C   50KSC2          IFLE      *ZEROS
233300940526     C     V1CKPM        ANDEQ     0
233400940526     C     V1CMPC        ANDEQ     0
233500940526     C                   MOVEL     MSG(10)       V1CMSG
233600940526     C                   SETON                                        4628
233700940504     C                   GOTO      ENDCTR
233800940509    1C                   ENDIF
233900940509     C*
234000940504     C* NON SI POSSONO INDICARE SIA I KG CHE I COLLI PER VOL.AUTOMATICO
234100940526    1C     V1CKPM        IFNE      0
234200940526     C     V1CMPC        ANDNE     0
234300940526     C                   MOVEL     MSG(11)       V1CMSG
234400940526     C                   SETON                                        4628
234500940504     C                   GOTO      ENDCTR
234600940509    1C                   ENDIF
234700090706      **** UTILIZZO PESO VDL ****
234800090706      * non posso immettere il campo utilizzo peso VDL se flag rapporto
234900090706      * peso/volume su tipo tariffa è blank
235000090706     c                   if        wrpv = *blanks and v1ctap <> *blanks
235100090706     c                   eval      v1cmsg = msg(14)
235200090706     c                   eval      *in28 = *on
235300090706     c                   eval      *in60 = *on
235400090706     c                   goto      endctr
235500090706     c                   endif
235600910423     C*
235700940504     C****  APPLICAZIONE TARIFFA FINITA  ****
235800941124    1C     V1CATA        IFNE      0
235900950112     C* WSAP = " " --> SI TRATTA DI SCAGLIONE SENZA DECIMALI PER CUI
236000950112     C*   NON SI POSSONO INSERIRE VALORI DOPO LA VIRGOLA
236100950112    2C     WSAP          IFEQ      ' '
236200941125     C                   MOVE      V1CATA        W0030
236300941124     C     W0030         IFGT      0
236400941124     C                   SETON                                        4728
236500950119     C                   MOVEL     MSG(57)       V1CMSG
236600941124     C                   GOTO      ENDCTR
236700941124     C                   ENDIF
236800950112    2C                   ENDIF
236900941124     C*
237000940504     C* CONTROLLO CHE L'APPLICAZIONE TARIFFA FINITA NON SIA INFERIORE
237100940504     C*   AL MINIMO TASSABILE INDICATO IN TABELLA
237200941124    2C     V1CATA        IFLT      MIN(A)
237300940526     C                   MOVEL     MSG(12)       V1CMSG
237400940526     C                   SETON                                        4728
237500940504     C                   GOTO      ENDCTR
237600941124    2C                   ENDIF
237700940509    1C                   ENDIF
237800940504     C*
237900940504     C****  ARROTONDAMENTI  ****
238000980505     C* CONTROLLO SE QUESTA TARIFFA GESTISCE GLI ARROTONDAMENTI
238100980505    1C     ARR(A)        IFEQ      ' '
238200980505    2C     V1CARL        IFGT      0
238300980505     C     V1CARF        ORGT      0
238400980505     C     V1CARO        ORGT      0
238500980505     C                   MOVEL     MSG(74)       V1CMSG
238600980505     C                   SETON                                        4828
238700980505     C                   GOTO      ENDCTR
238800980505    2C                   ENDIF
238900980505    1C                   ENDIF
239000940504     C* DEVONO ESSERE O TUTTI <> 0  OPPURE  TUTTI = 0
239100940712    1C     V1CARL        IFEQ      0
239200940504     C*
239300940712     C     V1CARF        IFGT      0
239400940526     C     V1CARO        ORGT      0
239500940526     C                   MOVEL     MSG(13)       V1CMSG
239600940526     C                   SETON                                        4828
239700940504     C                   GOTO      ENDCTR
239800940504     C                   ENDIF
239900940504     C*
240000940504   X1C                   ELSE
240100940504     C*
240200940712     C     V1CARF        IFEQ      0
240300940526     C     V1CARO        OREQ      0
240400940526     C                   MOVEL     MSG(13)       V1CMSG
240500940526     C                   SETON                                        4828
240600940504     C                   GOTO      ENDCTR
240700940504     C                   ENDIF
240800940504     C*
240900940504    1C                   ENDIF
241000940504     C*
241100941125     C* SE SONO STATI IMMESSI GLI ARROTONDAMENTI CONTROLLO SE SI TRATTA
241200950112     C*   DI SCAGLIONE SENZA DECIMALI PER CUI NON SI POSSONO INSERIRE
241300950112     C*   VALORI DOPO LA VIRGOLA
241400941125    1C     V1CARL        IFGT      0
241500941125     C     WSAP          ANDEQ     ' '
241600941125     C                   MOVE      V1CARL        W0030
241700941125    2C     W0030         IFGT      0
241800941125     C                   SETON                                        4828
241900950119     C                   MOVEL     MSG(56)       V1CMSG
242000941125     C                   GOTO      ENDCTR
242100941125    2C                   ENDIF
242200941125     C*
242300941125     C                   MOVE      V1CARF        W0030
242400941125    2C     W0030         IFGT      0
242500941125     C                   SETON                                        4828
242600950119     C                   MOVEL     MSG(56)       V1CMSG
242700941125     C                   GOTO      ENDCTR
242800941125    2C                   ENDIF
242900941125     C*
243000941125     C                   MOVE      V1CARO        W0030
243100941125    2C     W0030         IFGT      0
243200941125     C                   SETON                                        4828
243300950119     C                   MOVEL     MSG(56)       V1CMSG
243400941125     C                   GOTO      ENDCTR
243500941125    2C                   ENDIF
243600941125    1C                   ENDIF
243700990616     C*
243800990616     C* CONTROLLO SE INSERITO MINIMO MASSIMO SPEDIZIONI AFFIDATECI DAL CLIENTE
243900990616     C     V1CMIS        IFNE      *ZEROS
244000990616     C     V1CMAS        ANDGT     0
244100990616     C     V1CMIS        ANDGT     V1CMAS
244200990618     C                   SETON                                        5128
244300990617     C                   MOVEL     MSG(78)       V1CMSG
244400990616     C                   GOTO      ENDCTR
244500990616     C                   ENDIF
244600940503     C*
244700940707     C****  TIPO SPEDIZIONE  ****
244800950112    1C     V1CTSP        IFEQ      '?'
244900940707     C                   MOVEL     '5E'          COD
245000940707     C                   MOVEL     ' '           V1CTSP
245100020201     C                   MOVEL     *BLANKS       KEY
245200960527     C                   MOVEL     *BLANKS       DES              25
245300940707     C                   CALL      'X§TABER'
245400940707     C                   PARM                    CODUT
245500940707     C                   PARM                    COD
245600940707     C                   PARM                    KEY
245700940707     C                   PARM                    DES
245800940707     C                   MOVEL     KEY           V1CTSP
245900090608     C**!!!              MOVEL     DES           V1DTSP
246000940729     C                   SETON                                        27
246100090608     C**!!!              GOTO      ENDCTR
246200950112    1C                   ENDIF
246300020215     C*
246400940711     C* TIPO SPEDIZIONE OBBLIGATORIO
246500940707     C                   MOVEL     '5E'          COD
246600940707     C                   MOVEL     *BLANKS       KEY
246700940707     C                   MOVEL     V1CTSP        KEY
246800940707     C     KTAB          CHAIN     TABEL                              32
246900950112    1C  N32TBLFLG        IFNE      ' '
247000940707     C                   SETON                                        32
247100950112    1C                   ENDIF
247200940707     C* TIPO SPEDIZIONE ERRATO
247300950112    1C     *IN32         IFEQ      *ON
247400940707     C                   MOVEL     MSG(42)       V1CMSG
247500940707     C                   SETON                                        5428
247600940707     C                   GOTO      ENDCTR
247700950112    1C                   ENDIF
247800000420     C*
247900000420     C                   MOVEL     TBLUNI        DS5E
248000090915     C* CONTROLLO SE tipo servizio utilizzabile
248100090915     C***  *IN50         IFEQ      *OFF
248200090915     c*
248300090915     c                   if        §5euso='P'   or §5efum<>'S'
248400090915    1C***  §5EUSO        IFEQ      'P'
248500090915     C***  WRKPT         ANDNE     'S'
248600090915     C                   MOVEL     MSG(91)       V1CMSG
248700090915     C                   SETON                                        5428
248800090915     C                   GOTO      ENDCTR
248900090915    1C                   ENDIF
249000000420     C*
249100000420     C* VERIFICO SE SERVIZIO NON POSTA CON CLIENTE NON POSTA
249200090915    1C***  §5EUSO        IFEQ      'E'
249300090915     C***  WRKPT         ANDEQ     'S'
249400090915     C**+                MOVEL     MSG(92)       V1CMSG
249500090915     C***                SETON                                        5428
249600090915     C***                GOTO      ENDCTR
249700090915    1C***                ENDIF
249800090915     C***                ENDIF
249900940707     C*
250000940707     C* DECODIFICA TIPO SPEDIZIONE
250100090608     C                   MOVEL     §5ed08        V1DTSP
250200970401     C* NON POSSO INSERIRE TIPO SERVIZIO ESPRESSO SE TARIFFA ESTERA
250300970401     C     V1CTSP        IFEQ      'E'
250400970401     C     V1CFIE        ANDEQ     'E'
250500970401     C                   MOVEL     MSG(70)       V1CMSG
250600090608     c                   eval      %subst(v1cmsg:34:8) = §5ed08
250700970401     C                   SETON                                        5428
250800970401     C                   GOTO      ENDCTR
250900970401    1C                   ENDIF
251000001127     C* SE E' OFFERTA ED IL TIPO SERVIZIO E' UN TIPO SERVIZIO UTILIZZATO DA
251100001127     C* CLIENTI POSTA VALORIZZO IL WRKPT = A 'S'
251200000426     C     *IN50         IFEQ      *ON
251300000426     C     §5EUSO        ANDEQ     'P'
251400000426     C                   MOVEL     'S'           WRKPT
251500000426     C                   ENDIF
251600130514      /free
251700130514       //?Se manutenzione tariffa posso variare il tipo servizio solo da
251800130514       //?"C" a "D" o viceversa, se "E" non posso variare e non posso impostarla
251900130514         IF  not *in50 and *in10 and not *in23;
252000130604           IF  sav_TAMtsp = 'E' and V1Ctsp <> sav_TAMtsp;
252100130514             V1Cmsg = msg(90);
252200130514             *in28 = *on;
252300130514             *in54 = *on;
252400130514             leavesr;
252500130514           ENDIF;
252600130604           IF  (sav_TAMtsp = 'C' or sav_TAMtsp = 'D') and
252700130514                V1Ctsp <> 'C' and V1Ctsp <> 'D';
252800130514             V1Cmsg = msg(90);
252900130514             *in28 = *on;
253000130514             *in54 = *on;
253100130514             leavesr;
253200130514           ENDIF;
253300130514         ENDIF;
253400130514      /end-free
253500940706     C*
253600941010     C****  FLAG TARIFFA PREFERENZIALE  ****
253700940929     C* NON E' POSSIBILE SELEZIONARE UNA TARIFFA PREFERENZIALE SU
253800940929     C*   CODICI TARIFFA DIVERSI APPARTENENTI ALLO STESSO CLIENTE
253900941011     C*   A MENO CHE NON SI TRATTI DI TARIFFA PARTENZE E TARIFFA ARRIVI
254000941011     C*   O DI TARIFFA ITALIA O TARIFFA ESTERO
254100941109    1C     V1CFTP        IFEQ      'P'
254200061011     C  n50V1CKSC        SETLL     TNTAM000
254300061011     c   50v1cksc        setll     tnofm000
254400061011     C  n50V1CKSC        READE(N)  TNTAM000                               32
254500061011     c   50v1cksc        reade(n)  tnofm000                               32
254600940929     C*
254700941109    2C     *IN32         DOWEQ     *OFF
254800941109    3C     TAMATB        IFEQ      ' '
254900940929     C*
255000941109    4C     TAMCTR        IFNE      V1CCTR
255100940929     C     TAMFTP        ANDEQ     'P'
255200941011     C*
255300941109    5C     TAMFIE        IFEQ      V1CFIE
255400941011     C*
255500941109    6C     TAMBAP        IFEQ      'P'
255600941011     C     V1CBAP        ANDEQ     'P'
255700950105     C     TAMBAP        OREQ      'P'
255800950105     C     V1CBAP        ANDEQ     ' '
255900950105     C     TAMBAP        OREQ      ' '
256000950105     C     V1CBAP        ANDEQ     'P'
256100950105     C*
256200941011     C     TAMBAP        OREQ      'A'
256300941011     C     V1CBAP        ANDEQ     'A'
256400950105     C     TAMBAP        OREQ      'A'
256500950105     C     V1CBAP        ANDEQ     ' '
256600950105     C     TAMBAP        OREQ      ' '
256700950105     C     V1CBAP        ANDEQ     'A'
256800940929     C                   MOVEL     MSG(49)       V1CMSG
256900940929     C                   SETON                                        8628
257000940929     C                   GOTO      ENDCTR
257100941109    6C                   ENDIF
257200941109    5C                   ENDIF
257300941109    4C                   ENDIF
257400940929     C*
257500941109    3C                   ENDIF
257600061011     C  n50V1CKSC        READE(N)  TNTAM000                               32
257700061011     c   50v1cksc        reade(n)  tnofm000                               32
257800941109    2C                   ENDDO
257900941109    1C                   ENDIF
258000020205      *
258100020205      * Se non immesso Network non posso inserirlo
258200020205     C  N01V1CNET        IFNE      ' '
258300020205     C     §TADPD        ANDEQ     ' '
258400020205     C     §TAFED        ANDEQ     ' '
258500020205     C                   MOVEL     MSG(99)       V1CMSG
258600020205     C                   SETON                                        4528
258700020205     C                   GOTO      ENDCTR
258800020205    6C                   ENDIF
258900000118     C*
259000020130     C* SE IMMESSO SERVIZIO DPD NON POSSO PIU' TOGLIERLO
259100020201     C  N01V1CNET        IFEQ      ' '
259200000118     C     §TADPD        ANDEQ     'S'
259300000118     C                   MOVEL     MSG(86)       V1CMSG
259400000118     C                   SETON                                        4528
259500000118     C                   GOTO      ENDCTR
259600000118    6C                   ENDIF
259700020130      *
259800020205      * Se immesso Network FedEx non posso più toglierlo
259900020201     C  N01V1CNET        IFEQ      ' '
260000020130     C     §TAFED        ANDEQ     'S'
260100020130     C                   MOVEL     MSG(97)       V1CMSG
260200020130     C                   SETON                                        4528
260300020130     C                   GOTO      ENDCTR
260400020130     C                   ENDIF
260500000118     C* una tariffa dpd non puo' essere estera
260600020201     C     V1CNET        IFEQ      'D'
260700000118     C     V1CFIE        ANDEQ     'E'
260800000118     C                   MOVEL     MSG(87)       V1CMSG
260900000118     C                   SETON                                        4528
261000000118     C                   GOTO      ENDCTR
261100000118    6C                   ENDIF
261200020130      * una tariffa FedEx deve essere estera
261300020201     C     V1CNET        IFEQ      'F'
261400020130     C     V1CFIE        ANDNE     'E'
261500020130     C                   MOVEL     MSG(98)       V1CMSG
261600020130     C                   SETON                                        4528
261700020130     C                   GOTO      ENDCTR
261800020130    6C                   ENDIF
261900020130     C* una tariffa dpd o FedEx deve essere corriere
262000020201     C     V1CNET        IFNE      ' '
262100000121     C     V1CTSP        ANDNE     'C'
262200090608     c                   clear                   ds5e
262300090608     c                   eval      cod = '5E'
262400090608     c                   eval      key = 'C'
262500090608     c     ktab          chain     tabel00f
262600090608     c                   if        %found(tabel00f)
262700090608     c                   eval      ds5e = tbluni
262800090608     c                   endif
262900000121     C                   MOVEL     MSG(88)       V1CMSG
263000090608     c                   eval      %subst(v1cmsg:48:8) = §5ed08
263100000121     C                   SETON                                        4528
263200000121     C                   GOTO      ENDCTR
263300000121    6C                   ENDIF
263400000218     C* una tariffa DPD a kg. deve avere applicazione tariffa finita fino a 31,5
263500000223     C* CHE TROVO NELLA TABELLA 3I
263600020201     C     V1CNET        IFEQ      'D'
263700000218     C     UNOCTR        ANDEQ     3
263800000223     C     V1CATA        ANDNE     §3IPKD
263900000223     C                   Z-ADD     §3IPKD        V1CATA
264000000218     C                   GOTO      ENDCTR
264100000218    6C                   ENDIF
264200021118      * Controllo per tariffa FedEx se ok codice tariffa inserito
264300021118     c                   If        V1cnet = 'F' and Rutf(a) <> 'S'
264400021118     c                   Eval      V1cMsg = Msg(111)
264500021118     c                   Eval      *In45 = *On
264600021118     c                   Eval      *In28 = *On
264700021118     c                   GoTo      EndCtr
264800021118     c                   EndIf
264900940929     C*
265000940503     C     ENDCTR        ENDSR
265100940503     C*
265200941013     C*--- CONTROLLO DATI FORMATO 2 ----------------------------------*
265300941012     C     CTRD02        BEGSR
265400020312     C                   SETOFF                                       2782
265500940729     C*
265600940811     C****  NATURA MERCE  ****
265700940811     C* NATURA MERCE OBBLIGATORIA
265800940811     C     V2CNAS        IFEQ      *BLANKS
265900940811     C                   MOVEL     MSG(43)       V1CMSG
266000940811     C                   SETON                                        8228
266100940811     C                   GOTO      ENDCT
266200940811     C                   ENDIF
266300060303
266400060303      * Rinuncia AC BASE
266500060303      * se imposto che rinuncio non ci deve essere la tariffa particolare "d"
266600060303     c                   if        v2cfmp <> *blanks
266700060303     c                   eval      wtpar = 'd'
266800070129     c  n50ktam7         chain(n)  titpt01l
266900070129     c   50ktam7         chain(n)  tiopt01l
267000061011     c                   If        %found and tptatb = *blanks
267100060303     c                   eval      v1cmsg = msg(119)
267200060303     c                   eval      *in28 = *on
267300060303     c                   move      rimm          at2fmp
267400060303     c                   Eval      h2riga = 10
267500060303     c                   Eval      h2colo = 19
267600060303     c                   leavesr
267700060303     c                   endif
267800060303     c                   endif
267900021118
268000940811     C*
268100940811     C****  FLAG VALORE MERCE DICHIARATO  ****
268200940811     C     *IN16         IFEQ      *ON
268300940811     C     V2CFVD        ANDEQ     ' '
268400940811     C                   MOVEL     MSG(44)       V1CMSG
268500090916     C                   SETON                                          28
268600090916     c                   Eval      h2riga = 13
268700090916     c                   Eval      h2colo = 36
268800940811     C                   GOTO      ENDCT
268900940811     C                   ENDIF
269000940811     C*
269100940729     C****  PARTICOLARITA' GESTIONE C/ASSEGNO  ****
269200941010    1C     V2CGCA        IFNE      *BLANKS
269300941010     C     V2CGCR        ORNE      *BLANKS
269400941010     C*
269500941010     C* RICHIAMO IL PROGRAMMA DI RICERCA
269600941010    2C     V2CGCR        IFEQ      '?'
269700941010     C                   CLEAR                   DS7PQR
269800941010     C                   MOVEL     'S'           DS7RIC
269900941010     C                   MOVEL     'V'           DS7GES
270000050428     C  n50              MOVEL     V1CKSC        DS7KSC
270100050428     C   50              MOVEL     ksc2          DS7KSC
270200941010     C                   MOVEL     V2CGCA        DS7COD
270300941010     C                   MOVEL     DS7PQR        KPJBU
270400941010     C                   CALL      'TRTB72R'
270500941010     C                   PARM                    KPJBA
270600941010     C                   MOVEL     KPJBU         DS7PQR
270700941010     C*
270800941010     C     V2CGCA        IFEQ      *BLANKS
270900941010     C                   MOVEL     DS7COD        V2CGCA
271000941010     C                   ENDIF
271100941010     C*
271200960109     C                   SETON                                        2777
271300941010     C                   GOTO      ENDCT
271400941010    2C                   ENDIF
271500941010     C*
271600941010     C* CONTROLLO
271700941010     C                   CLEAR                   DS7PQR
271800941010     C                   MOVEL     'S'           DS7RIC
271900941010     C                   MOVEL     'C'           DS7GES
272000050323     C  n50              MOVEL     V1CKSC        DS7KSC
272100050323     C   50              MOVEL     ksc2          DS7KSC
272200941010     C                   MOVEL     V2CGCA        DS7COD
272300941010     C                   MOVEL     DS7PQR        KPJBU
272400941010     C                   CALL      'TRTB72R'
272500941010     C                   PARM                    KPJBA
272600941010     C                   MOVEL     KPJBU         DS7PQR
272700941010     C* ERRORE
272800941010    2C     DS7ERR        IFEQ      '1'
272900941010     C                   MOVEL     DS7MSG        V1CMSG
273000941010     C                   SETON                                        7728
273100941010     C                   GOTO      ENDCT
273200941010    2C                   ENDIF
273300941010    1C                   ENDIF
273400941010     C*
273500940729     C****  PARTICOLARITA' GESTIONE GIACENZA  ****
273600941010    1C     V2CGGA        IFNE      *BLANKS
273700941010     C     V2CGGR        ORNE      *BLANKS
273800941010     C*
273900941010     C* RICHIAMO IL PROGRAMMA DI RICERCA
274000941010    2C     V2CGGR        IFEQ      '?'
274100941010     C                   CLEAR                   DS7PQR
274200941010     C                   MOVEL     'S'           DS7RIC
274300941010     C                   MOVEL     'V'           DS7GES
274400050428     ***C  n50              MOVEL     V1CKSC        DS7KSC
274500050428     ***C   50              MOVEL     ksc2          DS7KSC
274600941010     C                   MOVEL     V2CGGA        DS7COD
274700941010     C                   MOVEL     DS7PQR        KPJBU
274800941010     C                   CALL      'TRTB69R'
274900941010     C                   PARM                    KPJBA
275000941010     C                   MOVEL     KPJBU         DS7PQR
275100941010     C*
275200941010     C     V2CGGA        IFEQ      *BLANKS
275300941010     C                   MOVEL     DS7COD        V2CGGA
275400941010     C                   ENDIF
275500941010     C*
275600960109     C                   SETON                                        2778
275700941010     C                   GOTO      ENDCT
275800941010    2C                   ENDIF
275900941010     C*
276000941010     C* CONTROLLO
276100941010     C                   CLEAR                   DS7PQR
276200941010     C                   MOVEL     'S'           DS7RIC
276300941010     C                   MOVEL     'C'           DS7GES
276400050323     C  n50              MOVEL     V1CKSC        DS7KSC
276500050323     C   50              MOVEL     ksc2          DS7KSC
276600941010     C                   MOVEL     V2CGGA        DS7COD
276700960109     C                   MOVEL     DS7PQR        KPJBU
276800941010     C                   CALL      'TRTB69R'
276900941010     C                   PARM                    KPJBA
277000941010     C                   MOVEL     KPJBU         DS7PQR
277100941010     C* ERRORE
277200941010    2C     DS7ERR        IFEQ      '1'
277300941010     C                   MOVEL     DS7MSG        V1CMSG
277400941010     C                   SETON                                        7828
277500941010     C                   GOTO      ENDCT
277600941010    2C                   ENDIF
277700941010    1C                   ENDIF
277800941010     C*
277900941010     C****  PARTICOLARITA' GESTIONE CONSEGNA  ****
278000941010    1C     V2CGMA        IFNE      *BLANKS
278100941010     C     V2CGMR        ORNE      *BLANKS
278200941010     C*
278300941010     C* RICHIAMO IL PROGRAMMA DI RICERCA
278400941010    2C     V2CGMR        IFEQ      '?'
278500941010     C                   MOVEL     'S'           DS7RIC
278600941010     C                   MOVEL     'V'           DS7GES
278700050428     ***C  n50              MOVEL     V1CKSC        DS7KSC
278800050428     ***C   50              MOVEL     ksc2          DS7KSC
278900050606     c                   Clear                   ds7ksc
279000941012     C                   MOVEL     V2CGMA        DS7COD
279100960109     C                   MOVEL     DS7PQR        KPJBU
279200941010     C                   CALL      'TRTB70R'
279300941010     C                   PARM                    KPJBA
279400941010     C                   MOVEL     KPJBU         DS7PQR
279500941010     C*
279600941010     C     V2CGMA        IFEQ      *BLANKS
279700941010     C                   MOVEL     DS7COD        V2CGMA
279800941010     C                   ENDIF
279900941010     C*
280000960109     C                   SETON                                        2779
280100941010     C                   GOTO      ENDCT
280200941010    2C                   ENDIF
280300941010     C*
280400941010     C* CONTROLLO
280500941010     C                   CLEAR                   DS7PQR
280600941010     C                   MOVEL     'S'           DS7RIC
280700941010     C                   MOVEL     'C'           DS7GES
280800050323     C  n50              MOVEL     V1CKSC        DS7KSC
280900050323     C   50              MOVEL     ksc2          DS7KSC
281000941010     C                   MOVEL     V2CGMA        DS7COD
281100941010     C                   MOVEL     DS7PQR        KPJBU
281200941010     C                   CALL      'TRTB70R'
281300941010     C                   PARM                    KPJBA
281400941010     C                   MOVEL     KPJBU         DS7PQR
281500941010     C* ERRORE
281600941010    2C     DS7ERR        IFEQ      '1'
281700941010     C                   MOVEL     DS7MSG        V1CMSG
281800941010     C                   SETON                                        7928
281900941010     C                   GOTO      ENDCT
282000941010    2C                   ENDIF
282100941010    1C                   ENDIF
282200941010     C*
282300941010     C****  PARTICOLARITA' GESTIONI VARIE  ****
282400941010    1C     V2CGVA        IFNE      *BLANKS
282500941010     C     V2CGVR        ORNE      *BLANKS
282600941010     C*
282700941010     C* RICHIAMO IL PROGRAMMA DI RICERCA
282800941010    2C     V2CGVR        IFEQ      '?'
282900941010     C                   CLEAR                   DS7PQR
283000941010     C                   MOVEL     'S'           DS7RIC
283100941010     C                   MOVEL     'V'           DS7GES
283200050323     C  n50              MOVEL     V1CKSC        DS7KSC
283300050323     C   50              MOVEL     ksc2          DS7KSC
283400941010     C                   MOVEL     V2CGVA        DS7COD
283500941010     C                   MOVEL     DS7PQR        KPJBU
283600941010     C                   CALL      'TRTB73R'
283700941010     C                   PARM                    KPJBA
283800941010     C                   MOVEL     KPJBU         DS7PQR
283900941010     C*
284000941010     C     V2CGVA        IFEQ      *BLANKS
284100941010     C                   MOVEL     DS7COD        V2CGVA
284200941010     C                   ENDIF
284300941012     C*
284400960109     C                   SETON                                        2780
284500941012     C                   GOTO      ENDCT
284600941012    2C                   ENDIF
284700941010     C*
284800941010     C* CONTROLLO
284900941010     C                   CLEAR                   DS7PQR
285000941010     C                   MOVEL     'S'           DS7RIC
285100941010     C                   MOVEL     'C'           DS7GES
285200050323     C  n50              MOVEL     V1CKSC        DS7KSC
285300050323     C   50              MOVEL     ksc2          DS7KSC
285400941010     C                   MOVEL     V2CGVA        DS7COD
285500941010     C                   MOVEL     DS7PQR        KPJBU
285600960209     C                   CALL      'TRTB73R'
285700960209     C                   PARM                    KPJBA
285800941010     C                   MOVEL     KPJBU         DS7PQR
285900941010     C* ERRORE
286000941010     C     DS7ERR        IFEQ      '1'
286100941010     C                   MOVEL     DS7MSG        V1CMSG
286200941010     C                   SETON                                        8028
286300941010     C                   GOTO      ENDCT
286400941010     C                   ENDIF
286500941012    1C                   ENDIF
286600000118     C**
286700000118     C* FLAG DDT: PER TARIFFA DPD SEMPRE NO
286800020201     C     V1CNET        IFEQ      'D'
286900000118     C     V2CBAM        ANDEQ     'S'
287000000118     C                   MOVEL     MSG(85)       V1CMSG
287100000118     C                   SETON                                        4928
287200000118     C                   GOTO      ENDCT
287300000118     C                   ENDIF
287400940729     C*
287500940729     C     ENDCT         ENDSR
287600940729     C*
287700940509     C*--- PULIZIA SUBFILE AGGIORNAMENTO -----------------------------*
287800920525     C     PULSF         BEGSR
287900000000     C                   SETON                                        94
288000940706     C                   WRITE     TA01C03
288100000000     C                   SETOFF                                       94
288200900924     C                   Z-ADD     0             SAVREC
288300900924     C                   ENDSR
288400930629     C*
288500941013     C*--- PULIZIA E INIZIAL. SF DI IMMISSIONE -----------------------*
288600920525     C     PULSF2        BEGSR
288700000000     C                   SETON                                        84
288800940706     C                   WRITE     TA01C05
288900000000     C                   SETOFF                                       84
289000920525     C*
289100940517     C                   CLEAR                   TADORP
289200940517     C                   CLEAR                   TADATB
289300940517     C                   CLEAR                   TADLNP
289400940517     C                   CLEAR                   TADCTS
289500940517     C                   CLEAR                   DESCTS
289600020624     c                   clear                   tadcap
289700940517     C                   CLEAR                   TADSGL
289800940517     C                   CLEAR                   TADITR
289900940517     C                   CLEAR                   TADINO
290000940517     C                   CLEAR                   TADRPV
290100960521     C                   CLEAR                   SAVRPV
290200940517     C                   CLEAR                   TADMIN
290300940517     C                   CLEAR                   TADMAX
290400020624     c                   clear                   tadnaz
290500990623     C                   CLEAR                   DESNAZ
290600940518     C* CAMPI DI DUPLICA
290700940518     C                   CLEAR                   DUPLNP
290800940518     C                   CLEAR                   DUPCTS
290900940518     C                   CLEAR                   DUPCAP
291000940518     C                   CLEAR                   DUPSGL
291100940518     C                   CLEAR                   DUPITR
291200940518     C                   CLEAR                   DUPINO
291300940518     C                   CLEAR                   DUPRPV
291400940518     C                   CLEAR                   DUPMIN
291500940518     C                   CLEAR                   DUPMAX
291600990608     C                   CLEAR                   DUPNAZ
291700021118
291800021118     c                   Clear                   ForzaMsg
291900021118
292000000000     C                   DO        120           NR1
292100940706     C                   WRITE     TA01S05
292200000000     C                   END
292300930225     C                   Z-ADD     1             REC1
292400000000     C                   ENDSR
292500050530     c
292600050530     c*
292700050530     c* Controllo Tariffe particolari da eliminare ------------------------*
292800050530     c     CtrTParEli    BEGSR
292900050530     c                   z-add     1             xx
293000050530    2c                   dow       TParEli(XX)<>'  '
293100070129     c  n50ktpt          chain(n)  titpt01l
293200070129     c   50ktpt          chain(n)  tiopt01l
293300061011    3c                   if        %found and tptatb=' '
293400050530     c* verifico la data di scadenza della tariffa che deve essere
293500050530     c*  <= alla data di eliminazione della tariffa particolare
293600050530    4c                   if        comdst>TPardat(XX)
293700050530     c* Errore  forzabile per F21 se non già dato
293800050530    5c                   if        *inkv
293900130318     c**   tpareli(XX)   lookup    forzatpar                              31
294000130318     c                   IF        %lookup(tpareli(xx):forzatpar) > 0
294100130318     c                   eval      wTParEli = *on
294200130318     c                   ENDIF
294300050530     c                   else
294400050530     c* Errore non forzabile per gli altri casi
294500130318     c**                 setoff                                       31
294600130318     c                   eval      wTParEli = *off
294700050530    5c                   endif
294800050530     c
294900130318    5c**                 if        not *in31
295000130318     c                   IF        not wTParEli
295100050530     c* Errore
295200050530     c                   eval      v1cmsg=msg(115)
295300050530     c
295400050530     c                   eval      %subst(v1cmsg:19:2)=tpareli(XX)
295500050530     c     *iso          move      tpardat(XX)   wdatadmy
295600050530     c                   move      wdatadmy      wdata             6 0
295700050530     c                   eval      %subst(v1cmsg:41:8) =(%editw(wdata:'  /  / -
295800050530     c                              '))
295900050530     c
296000050530     c                   add       1             yy                4 0
296100050530     c                   movel     tpareli(xx)   forzatpar(YY)
296200050530     c                   seton                                        2841
296300050530     C                   GOTO      ENDCtrTPar
296400050530    5c                   endif
296500050530    4c                   endif
296600050530    3c                   endif
296700050530     c                   add       1             xx
296800050530    2c                   enddo
296900050530     c     EndCTRTPar    ENDSR
297000930629     C*
297100940615     C*-- AGGIORNAMENTO TNTAM ----------------------------------------*
297200920525     C     AGGTAM        BEGSR
297300940506     C*
297400061011     C** RIPOSIZIONAMENTO SUL FILE TNTAM
297500061011     C  n50KTAM2         CHAIN     TNTAM000                           01
297600061011     c   50ktam2         chain     tnofm000                           01
297700061010     C  N01TAMATB        COMP      'A'                                    01
297800050530     c* se non sono in immissione, controllo tariffe partic in scadenza
297900061010    1c                   if        not *in01
298000050530     c
298100050530     c* controllo se ci sono tariffe particolari da eliminare
298200050530     c                   EXSR      CtrTParEli
298300050530     c
298400050530     c* 28 on trovate --> errore
298500050530     c                   if        *in28
298600050530     c                   goto      ENDaggtam
298700050530    1c                   endif
298800050530    1c                   endif
298900050530     c
299000061010     C  N01              SETON                                        10
299100941012     C** MUOVO I CAMPI DEL VIDEO NEL FILE
299200941012     C                   EXSR      RIETAM
299300920525     C* I M M I S S I O N E
299400970226     C     *IN01         IFEQ      *ON
299500970226     C                   Z-ADD     V1CKSC        TAMKSC
299600970226     C                   Z-ADD     PRG           TAMPRG
299700970226     C                   Z-ADD     DATEU8        TAMDUV
299800970226     C                   MOVEL     *BLANKS       TAMFTR
299900040913     C                   Z-ADD     dateu8        TAMDTR
300000970226     C                   MOVEL     *BLANKS       TAMATB
300100970226     C                   CLEAR                   TAMDAT
300200150922     C***                CLEAR                   TAMTPR
300300101025     c                   clear                   tamfli
300400920525     C*
300500061011     C  n50              WRITE     TNTAM000
300600061011     c   50              write     tnofm000
300700040927      *
300800040927     c                   eval      tavtrd = 'TES'
300900040927     c                   eval      tavcta = 'I'
301000040927     c                   exsr      SUB_STORICO
301100090916      * se trattativa scrivo TIVOF
301200090916     c                   if        *in85
301300090916     c                   clear                   tivof000
301400090916     c                   eval      vofnrv = tamksc
301500090916     c                   move      tamctr        vofctr
301600090916     c                   eval      vofprg = %dec(tamprg:1:0)
301700090916     c                   select
301800090916     c                   when      §tadpd = 'S'
301900090916     c                   eval      voftpt = 'D'
302000090916     c                   when      §tafed = 'S'
302100090916     c                   eval      voftpt = 'F'
302200090916     c                   other
302300090916     c                   eval      voftpt = tamfie
302400090916     c                   endsl
302500091125      * valorizzo data presentazione offerta con la data del giorno
302600091125     c                   eval      vofdpo = dateu8
302700091125
302800090916     c                   write     tivof000
302900090916     c                   endif
303000920525     C*
303100940615     C** INDICATORE DI VARIAZIONE TNTAM
303200970226     C                   SETON                                        81
303300970226     C                   ENDIF
303400920525     C*
303500920525     C* V A R I A Z I O N E
303600000000     C   10COMODO        IFNE      TAMDS
303700941221     C                   Z-ADD     DATEU8        TAMDUV
303800040913     C                   Z-ADD     dateu8        TAMDTR
303900940511     C                   MOVEL     *BLANKS       TAMFTR
304000061011     C  n50              UPDATE    TNTAM000
304100061011     c   50              update    tnofm000
304200000000     C                   SETON                                        81
304300030704
304400030716     c                   eval      mantes = 'S'
304500960620     C                   MOVEL     TAMDS         COMODO
304600000000     C                   END
304700900709     C*
304800900320     C                   Z-ADD     TAMCTR        VCTR
304900941012     C*
305000050530     C     endaggtam     ENDSR
305100940509     C*
305200940509     C*--- SR DI IMMISSIONE TARIFFA ----------------------------------*
305300920525     C     IMMISS        BEGSR
305400921218     C                   SETON                                        99
305500000000     C     SUIMMX        TAG
305600921118     C** PULIZIA SF
305700000000     C                   EXSR      PULSF2
305800120315
305900120315       //?Imposto i tasti funzione nella riga mobile
306000120315     c                   eval      wPrima = *off
306100120315     c                   eval      wSeconda = *off
306200120315     c                   eval      wTerza = *off
306300120315     c                   eval      wQuarta = *on
306400120315     c                   ExSr      Tastifun
306500921118     C*
306600000000     C     SUIMM         TAG
306700120319     C** EMISSIONE COMANDI
306800120319     C                   WRITE     TA01Z06
306900941206     C***
307000941206     C   28
307100941206     CAN 62              DO
307200941206     C                   SETOFF                                       28
307300941206     C                   WRITE     TA01C05
307400941206     C                   SETON                                        28
307500941206     C                   ENDDO
307600941206     C***
307700921118     C** EMISSIONE SF IMMISSIONE
307800940706     C                   EXFMT     TA01C05
307900921118     C*
308000940526     C* PULIZIA CAMPO MESSAGGIO E RELATIVO INDICATORE (*IN28)
308100940526     C                   CLEAR                   $MSG
308200940526     C* AZZERO GLI INDICATORI RELATIVI AI MESSAGGI DI ERRORE
308300940526     C                   SETOFF                                       285556
308400940526     C                   SETOFF                                       575859
308500960522     C                   SETOFF                                       616289
308600990623     C                   SETOFF                                       5238
308700120315       //?F1 - dati manca tariffa
308800120315     c                   IF        *inka
308900120315     c                   exsr      gesw01
309000120315     c                   goto      suimm
309100120315     c                   ENDIF
309200940526     C*
309300120315     c  nkc
309400120315     CanNKL              DO
309500921118     C** ESEGUE CONTROLLI IMMISSIONE
309600000000     C                   EXSR      CTRIMM
309700940525     C   22              GOTO      SUIMM
309800921118     C*
309900000000     C** AGGIUNTA TARIFFE SU DISCO
310000920525     C                   EXSR      ADDTAD
310100000000     C** CMD7-NUOVA IMMISSIONE
310200920525     C   KG              GOTO      SUIMMX
310300921118     C*
310400921118     C**CMD6-IMMISSIONE E FINE LAVORO
310500921118     C  NKF              DO
310600940526     C                   Z-ADD     0             V1CRDA
310700940526     C                   MOVEL     *BLANKS       V1CRA
310800921118     C                   END
310900120315       //?F24 Altri tasti
311000120315     c                   IF        *inky
311100120315     c                   exsr      gesf24
311200120315     c                   goto      suimm
311300120315     c                   ENDIF
311400921118     C                   END
311500000000     C                   ENDSR
311600930629     C*
311700930629     C*--- CONTROLLO IMMISSIONE --------------------------------------*
311800920525     C     CTRIMM        BEGSR
311900990617     C                   SETOFF                                       221444
312000940526     C                   SETOFF                                       020304
312100940526     C                   SETOFF                                       050607
312200940526     C                   SETOFF                                       080911
312300940511     C                   Z-ADD     1             NR1               3 0
312400940510     C*
312500940706     C                   READC     TA01S05                                29
312600940524    1C     *IN29         DOWEQ     *OFF
312700020408
312800020408     c                   eval      woksfl = '0'
312900020624     c                   clear                   descts
313000020408
313100930629     C*
313200940510     C* TASTI DI DUPLICA
313300900420     C   02              Z-ADD     DUPLNP        TADLNP
313400900420     C   03              MOVE      DUPCTS        TADCTS
313500020624     c   04              movel     DUPCAP        tadcap
313600940524     C   05              MOVE      DUPSGL        TADSGL
313700940524     C   06              Z-ADD     DUPITR        TADITR
313800940524     C   07              Z-ADD     DUPINO        TADINO
313900940524     C   08              Z-ADD     DUPRPV        TADRPV
314000020624     c   44              movel     DUPNAZ        tadnaz
314100940524     C   09              Z-ADD     DUPMIN        TADMIN
314200940524     C   11              Z-ADD     DUPMAX        TADMAX
314300930629     C*
314400940510     C****  LINEA PARTENZA  ****
314500940510     C* LINEA PARTENZA OBBLIGATORIA
314600120906       //?Se non è stata inserita la LNP ma c'è il codice di tassazione
314700120906       //?errore di manca LNP
314800120906     c                   IF        TADlnp = *zeros and
314900120906     c                             TADcts <> *blanks
315000120906     c                   eval      *in55 = *on
315100120906     c                   eval      *in28 = *on
315200120906     c                   eval      $msg = msg(24)
315300120906     c                   goto      giuctr
315400120906     c                   ENDIF
315500120906
315600940510    2C     TADLNP        IFNE      0
315700930629     C*
315800940930     C*
315900940930     C* CONTROLLO VALIDITA' LINEA PARTENZA
316000940930     C     TADLNP        CHAIN     AZORG                              32
316100020312     c                   clear                   og143
316200940811     C*
316300940510    3C  N32ORGFVA        IFNE      ' '
316400940510     C                   SETON                                        32
316500940510    3C                   ENDIF
316600930219     C*
316700020527     c                   if        orgfag <> 'A' and orgfag <> 'F' and
316800020527     c                             orgfl2 <> 'S'
316900940510     C                   SETON                                        32
317000940510    3C                   ENDIF
317100930219     C*
317200940510     C* 32 ON  - LINEA PARTENZA INESISTENTE
317300940525     C     *IN32         IFEQ      *ON
317400940526     C                   SETON                                        5528
317500940525     C                   MOVEL     MSG(22)       $MSG
317600940525     C                   GOTO      GIUCTR
317700940525     C                   ENDIF
317800020312
317900020312     c                   eval      og143 = orgde3
318000020312     c                   eval      lnpntw = §ogntw
318100020312
318200971205     C* SE FLAG ITA/EST = 'I' ---> NON POSSO INSERIRE FILIALI ESTERE
318300971205    3C     V1CFIE        IFEQ      'I'
318400020201     C     V1CNET        ANDEQ     ' '
318500020312  4b c                   if        lnpntw = 'EEX' or lnpntw = 'EUP' or
318600020312     c                             lnpntw = 'FED'
318700971205     C                   SETON                                        5528
318800971205     C                   MOVEL     MSG(45)       $MSG
318900971205     C                   GOTO      GIUCTR
319000020312  4e c                   endif
319100971205    3C                   ENDIF
319200930629     C*
319300940510     C****  CODICE TASSAZIONE  ****
319400940510     C* CODICE TASSAZIONE OBBLIGATORIO
319500000000     C                   Z-ADD     1             A                 5 0
319600941011     C                   Z-ADD     1             K
319700120713     c                   IF        %scan('?':TADcts) > *zeros
319800120713     c                   exsr      Ric_CT
319900120713     c                   eval      TADcts = TB09cod
320000120713     c                   ENDIF
320100940524     C     TADCTS        LOOKUP    CCT(A)                                 35
320200940524    3C   35CCT(A)        IFEQ      '  '
320300940524     C                   SETOFF                                       35
320400940510    3C                   ENDIF
320500900111     C*
320600940525     C     *IN35         IFEQ      *OFF
320700940526     C                   SETON                                        5628
320800940525     C                   MOVEL     MSG(23)       $MSG
320900940525     C                   GOTO      GIUCTR
321000940525     C                   ENDIF
321100050303      * errore bloccante se codice tassazione non utilizzabile in gestione
321200050303      * tariffe clienti
321300050303     c                   If        uta(a) = 'N'
321400050303     c                   Eval      $msg = msg(36)
321500050303     c                   Eval      *In56 = *On
321600050303     c                   Eval      *In28 = *On
321700050303     c                   Goto      giuctr
321800050303     c                   EndIf
321900020206      *
322000020206      * Se tariffa FedEx devo utilizzare solo i codici tariffa
322100020206      * previsti per FedEx
322200020206     C     V1CNET        IFEQ      'F'
322300020206     C     UTF(A)        ANDNE     'S'
322400020206     C                   SETON                                        5628
322500020206     C                   MOVEL     MSG(100)      $MSG
322600020206     C                   GOTO      GIUCTR
322700020206     C                   ENDIF
322800020206      * Se tariffa non è FedEx non posso utilizzare i codici
322900020206      * tariffa previsti per FedEx
323000020206     C     V1CNET        IFNE      'F'
323100020206     C     UTF(A)        ANDEQ     'S'
323200020206     C                   SETON                                        5628
323300020206     C                   MOVEL     MSG(101)      $MSG
323400020206     C                   GOTO      GIUCTR
323500020206     C                   ENDIF
323600020206      *
323700940525     C*
323800941011     C* SE FLAG ITA/EST = 'I' ---> NON POSSO INSERIRE COD.TASS. ESTERI
323900940811     C     V1CFIE        IFEQ      'I'
324000020201     C     V1CNET        ANDEQ     ' '
324100941011     C     NAR(A)        LOOKUP    NAZ(K)                                 35
324200950117     C     *IN35         IFEQ      *OFF
324300940811     C                   SETON                                        5628
324400940811     C                   MOVEL     MSG(47)       $MSG
324500940811     C                   GOTO      GIUCTR
324600940811     C                   ENDIF
324700941011     C                   ENDIF
324800941011     C*
324900950112     C* SE FLAG ITA/EST = 'E' ---> NON POSSO INSERIRE NELLA STESSA RIGA
325000950112     C*   DI DETTAGLIO SIA LA LINEA PARTENZA CHE IL COD.TASS. ITALIANI.
325100941026     C*   SE LA LNP E' ESTERA NON IMPORTA CONTROLLARE SE IL CODICE
325200941026     C*   TASSAZIONE E' ESTERO O ITALIANO PERCHE' LO ACCETTO UGUALMENTE
325300020312     c                   if        v1cfie = 'E' and lnpntw <> 'EEX' and
325400020312     c                             lnpntw <> 'EUP' and lnpntw <> 'FED'
325500941011     C     NAR(A)        LOOKUP    NAZ(K)                                 35
325600950117     C     *IN35         IFEQ      *ON
325700940811     C                   SETON                                        5628
325800940811     C                   MOVEL     MSG(48)       $MSG
325900940811     C                   GOTO      GIUCTR
326000940811     C                   ENDIF
326100941011     C                   ENDIF
326200020328
326300020328      * se tariffa estera non posso accettare codice tassazione 'IT'
326400020328     c                   if        v1cfie = 'E' and tadcts = 'IT'
326500020328     c                   eval      *in56 = *on
326600020328     c                   eval      *in28 = *on
326700020328     c                   eval      $msg = msg(107)
326800020328     c                   goto      giuctr
326900020328     c                   endif
327000020404
327100020404      * se tariffa DPD non posso accettare codice tassazione 'EE'
327200020404     c                   if        v1cnet = 'D' and tadcts = 'EE'
327300020404     c                   eval      *in56 = *on
327400020404     c                   eval      *in28 = *on
327500020404     c                   eval      $msg = msg(109)
327600020404     c                   goto      giuctr
327700020404     c                   endif
327800020624
327900020624      * se linea di partenza 950 = tutte le linee non posso accettare i codici
328000020624      * di tassazione 'IT' e 'EE'
328100020624     c                   if        tadlnp = 950 and
328200020624     c                             (tadcts = 'IT' or tadcts = 'EE')
328300020624     c                   eval      *in56 = *on
328400020624     c                   eval      *in28 = *on
328500020624     c                   eval      $msg = msg(110)
328600020624     c                   goto      giuctr
328700020624     c                   endif
328800940811     C*
328900940510     C* DECODIFICA CODICE TASSAZIONE
329000940525     C                   MOVEL     DCT(A)        DESCTS
329100020215     C                   MOVE      ORP(A)        TADORP
329200930629     C*
329300941011     C****  C. A. P.  ****
329400020624    3c                   if        tadcap <> *blanks
329500970718     C                   CLEAR                   DSSI95
329600970718     C                   CLEAR                   DSLV13
329700941011     C*
329800970718     C                   MOVEL     '3'           I95TCN
329900020624     c                   movel     tadnaz        i95nar
330000020624     c                   movel     tadcap        i95cap
330100040316     c                   z-add     dateu8        I95DAT
330200970718     C                   CALL      'FNLV13R'
330300970718     C                   PARM                    KPJBA
330400970718     C                   PARM                    DSLV13
330500970718     C                   PARM                    DSSI95
330600970718     C                   MOVEL     '1'           FNLV13            1
330700941011     C*
330800970718     C     O13ERR        IFNE      *BLANKS
330900970718     C                   MOVEL     O13MSG        $MSG
331000941011     C                   SETON                                        5728
331100941011     C                   GOTO      GIUCTR
331200941011     C                   ENDIF
331300990624     C* DECODIFICO LA NAZIONE SE E' DIVERSA DA BLANKS
331400020624     c                   if        tadnaz = *blanks
331500990624     C                   CLEAR                   DESNAZ
331600990624     C                   ELSE
331700990623     C                   MOVEL     '15'          COD
331800990623     C                   MOVEL     *BLANKS       KEY
331900020624     c                   movel     tadnaz        key
332000990623     C     KTAB          CHAIN     TABEL                              32
332100990623     C*
332200990623     C     *IN32         IFEQ      *OFF
332300990623     C     TBLFLG        ANDEQ     ' '
332400990623     C                   MOVEL     TBLUNI        DESNAZ
332500990623     C                   ELSE
332600990623     C* ERRORE
332700990623     C                   SETON                                        3828
332800990623     C                   MOVEL     MSG(81)       $MSG
332900990623     C                   GOTO      GIUCTR
333000990623     C                   ENDIF
333100990624     C                   ENDIF
333200930630     C*
333300930629     C* SE TARIFFA REGIONE O ITALIA NON DEVE ESISTERE IL CAP
333400940525     C     REG(A)        IFEQ      '9'
333500940526     C                   SETON                                        5728
333600940525     C                   MOVEL     MSG(25)       $MSG
333700940525     C                   GOTO      GIUCTR
333800940525     C                   ENDIF
333900940525     C*
334000930702     C* CONTROLLO SE IL CAP E' CONGRUENTE CON IL CODICE TASSAZIONE
334100970718    4C     TADCTS        IFNE      O95CTS
334200940526     C                   SETON                                        5728
334300940525     C                   MOVEL     MSG(26)       $MSG
334400930702     C                   GOTO      GIUCTR
334500940510    4C                   ENDIF
334600931117     C*
334700931117     C* SE INSERITO IL CAP NON BISOGNA IMMETTERE LA TARIFFA PROVINCIA
334800940510    4C     TADINO        IFNE      0
334900940526     C                   SETON                                        6128
335000940525     C                   MOVEL     MSG(27)       $MSG
335100931117     C                   GOTO      GIUCTR
335200940510    4C                   ENDIF
335300931117     C*
335400940510    3C                   ENDIF
335500930629     C*
335600940510     C****  SCAGLIONE  TARIFFA  ****
335700940510     C* SCAGLIONE OBBLIGATORIO
335800940511    3C     TADSGL        IFEQ      0
335900940526     C                   SETON                                        5828
336000940525     C                   MOVEL     MSG(28)       $MSG
336100900420     C                   GOTO      GIUCTR
336200941124   X3C                   ELSE
336300950119     C* WSAP = " " --> SI TRATTA DI SCAGLIONE SENZA DECIMALI QUINDI NON
336400950116     C*                SI POSSONO INSERIRE VALORI DOPO LA VIRGOLA
336500941124    4C     WSAP          IFEQ      ' '
336600941124     C                   MOVE      TADSGL        W0030
336700941124     C     W0030         IFGT      0
336800941124     C                   SETON                                        5828
336900950118     C                   MOVEL     MSG(46)       $MSG
337000941124     C                   GOTO      GIUCTR
337100941124     C                   ENDIF
337200941124    4C                   ENDIF
337300021118      * Se tariffa FedEx Documenti devo controllare che non sia stato immesso
337400021118      * uno scaglione > del peso possibile per questa tariffa
337500021118      * messaggio forzabile
337600021118     c                   If        V1cNet= 'F' and DueCtr >= §DfeDalD
337700021118     c                             and DueCtr <= §DfeAlD
337800021118     c                             and TadSgl > §DfePkgD
337900021118     c                             and ForzaMsg = *Blanks
338000021118     c                   Movel     Msg(112)      $Msg
338100021118     c                   Eval      *In58 = *On
338200021118     c                   Eval      *In28 = *On
338300021118     c                   Eval      ForzaMsg = '1'
338400021118     c                   GoTo      GiuCtr
338500021118     c                   EndIf
338600940510    3C                   ENDIF
338700930629     C*
338800940510     C****  TARIFFA CITTA'  ****
338900940510     C* TARIFFA CITTA' OBBLIGATORIA
339000940525     C     TADITR        IFEQ      0
339100940526     C                   SETON                                        5928
339200940525     C                   MOVEL     MSG(29)       $MSG
339300940525     C                   GOTO      GIUCTR
339400941123     C                   ELSE
339500050616     C* 88 OFF -  TARIFFA NON IN % verifico se si
339600941124     C*   POSSONO INSERIRE DECIMALI
339700941124     C     *IN88         IFEQ      *OFF
339800050616     C* LA DIVISA NON PREVEDE DECIMALI
339900990617     C     §CVFDC        ANDNE     'S'
340000941123     C                   MOVE      TADITR        W0030             3 0
340100941123     C     W0030         IFGT      0
340200941123     C                   SETON                                        5928
340300990617     C                   MOVEL     MSG(79)       $MSG
340400941123     C                   GOTO      GIUCTR
340500941123     C                   ENDIF
340600941123     C                   ENDIF
340700940525     C                   ENDIF
340800930629     C*
340900941123     C****  TARIFFA PROVINCIA  ****
341000050616     C* 88 OFF -  TARIFFA NON IN % verifico se si
341100941124     C*   POSSONO INSERIRE DECIMALI
341200941124     C  N88TADINO        IFGT      0
341300941123     C                   MOVE      TADINO        W0030             3 0
341400941123     C     W0030         IFGT      0
341500050616     C*  LA DIVISA NON PREVEDE DECIMALI
341600990617     C     §CVFDC        ANDNE     'S'
341700941123     C                   SETON                                        6128
341800990617     C                   MOVEL     MSG(79)       $MSG
341900941123     C                   GOTO      GIUCTR
342000941123     C                   ENDIF
342100941123     C                   ENDIF
342200941123     C*
342300960520     C****  RAPPORTO PESO/VOLUME  ****
342400980505     C* CONTROLLO SE IL TIPO TARIFFA PREVEDE LA GESTIONE DEGLI ARROTONADMENTI
342500980505     C* IN BASE AL FLAG DELLA TABELLA TR
342600980505     C     WRPV          IFNE      'S'
342700980505     C     TADRPV        ANDNE     0
342800980505     C                   SETON                                        8928
342900980505     C                   MOVEL     MSG(75)       $MSG
343000980505     C                   GOTO      GIUCTR
343100980505     C                   ENDIF
343200960520     C* SE INSERITO UN RAPPORTO PESO/VOL. SUPERIORE AL LIMITE INDICATO
343300960520     C*   IN TABELLA EMETTO ERRORE FORZABILE
343400960521     C     TADRPV        IFNE      SAVRPV
343500960520     C     TADRPV        ANDGT     §1GRPV
343600960520     C                   SETON                                        8928
343700960520     C                   MOVEL     MSG(59)       $MSG
343800960521     C                   MOVEL     TADRPV        SAVRPV
343900960520     C                   GOTO      GIUCTR
344000960520     C                   ENDIF
344100020404
344200020404      * Nazione
344300020624     c                   if        tadnaz <> *blanks and tadcap = *blanks
344400020404     c                   eval      $msg = msg(108)
344500020404     c                   eval      *in28 = *on
344600020404     c                   eval      *in57 = *on
344700020404     c                   goto      giuctr
344800020404     c                   endif
344900990617     C*
345000990617     C****  IMPORTO MASSIMO TARIFFA  ****
345100050616     C* 88 OFF -  TARIFFA NON IN % verifico se si
345200990617     C*   POSSONO INSERIRE DECIMALI
345300990617     C  N88TADMAX        IFGT      0
345400990617     C                   MOVE      TADMAX        W0030             3 0
345500990617     C     W0030         IFGT      0
345600050616     C*  LA DIVISA NON PREVEDE DECIMALI
345700990617     C     §CVFDC        ANDNE     'S'
345800990617     C                   SETON                                        6228
345900990617     C                   MOVEL     MSG(79)       $MSG
346000990618     C                   MOVEL     *ON           *INKK
346100990617     C                   GOTO      GIUCTR
346200990617     C                   ENDIF
346300990617     C                   ENDIF
346400990617     C*
346500990617     C****  IMPORTO MINIMO  TARIFFA  ****
346600050616     C* 88 OFF -  TARIFFA NON IN % verifico se si
346700990617     C*   POSSONO INSERIRE DECIMALI
346800990617     C  N88TADMIN        IFGT      0
346900990617     C                   MOVE      TADMIN        W0030             3 0
347000990617     C     W0030         IFGT      0
347100050616     C*  LA DIVISA NON PREVEDE DECIMALI
347200990617     C     §CVFDC        ANDNE     'S'
347300990617     C                   SETON                                        5228
347400990617     C                   MOVEL     MSG(79)       $MSG
347500990618     C                   MOVEL     *ON           *INKK
347600990617     C                   GOTO      GIUCTR
347700990617     C                   ENDIF
347800990617     C                   ENDIF
347900960520     C*
348000940525     C****  IMPORTO MASSIMO TARIFFA  ****
348100940525     C* SE INSERITO DEVE ESSERE MAGGIORE O UGUALE AL MINIMO
348200940525     C     TADMAX        IFGT      0
348300940525     C     TADMAX        ANDLT     TADMIN
348400940526     C                   SETON                                        6228
348500940525     C                   MOVEL     MSG(31)       $MSG
348600941107     C                   MOVEL     *ON           *INKK
348700940525     C                   GOTO      GIUCTR
348800940525     C                   ENDIF
348900980310      * E' inutile inserire dei valori minimi e massimi per scaglioni inferiori
349000980310      * al valore minimo tassabile o al valore tariffa finita
349100980310      * quindi faccio il controllo di TADMAX E/O TADMAX
349200980310     C     TADMAX        IFGT      0
349300980310     C     TADMIN        ORGT      0
349400980310      * VALORE TARIFFA FINITA
349500980310     C     TAMATA        IFGT      TADSGL
349600980310     C     TAMATA        OREQ      0
349700980601     C     WRKMIN        ANDGT     TADSGL
349800980310     C                   SETON                                        6228
349900980310     C                   MOVEL     MSG(73)       $MSG
350000980310     C                   MOVEL     *ON           *INKK
350100980310     C                   GOTO      GIUCTR
350200980310     C                   ENDIF
350300980310      *
350400980310     C                   ENDIF
350500940525     C*
350600940518     C                   Z-ADD     TADITR        RISITR
350700000000     C                   Z-ADD     TADINO        RISINO
350800000000     C                   Z-ADD     TADRPV        RISRPV
350900940525     C                   Z-ADD     TADMIN        RISMIN
351000940525     C                   Z-ADD     TADMAX        RISMAX
351100940510     C*
351200061011     C  n50KTAD          CHAIN     TITAD000                           32
351300061011     c   50ktad          chain     tiofd000                           32
351400940525     C  N32TADATB        COMP      'A'                                    32
351500940525     C*
351600940525     C     *IN32         IFEQ      *OFF
351700940526     C                   SETON                                        5828
351800940525     C                   MOVEL     MSG(30)       $MSG
351900940525     C                   ENDIF
352000940525     C*
352100940518     C                   Z-ADD     RISITR        TADITR
352200000000     C                   Z-ADD     RISINO        TADINO
352300000000     C                   Z-ADD     RISRPV        TADRPV
352400940525     C                   Z-ADD     RISMIN        TADMIN
352500940525     C                   Z-ADD     RISMAX        TADMAX
352600940510     C*
352700940525     C* 58 ON - RECORD TARIFFA GIA' ESISTENTE
352800940525     C   58              GOTO      GIUCTR
352900930629     C*
353000940510     C* SE TUTTO BENE SALVO I CAMPI PER LA DUPLICA
353100900420     C                   MOVEL     TADINO        DUPINO
353200940518     C                   Z-ADD     TADITR        DUPITR
353300900420     C                   Z-ADD     TADLNP        DUPLNP
353400900420     C                   MOVEL     TADCTS        DUPCTS
353500940518     C                   MOVEL     TADSGL        DUPSGL
353600900420     C                   Z-ADD     TADRPV        DUPRPV
353700020624     c                   movel     tadcap        DUPCAP
353800020624     c                   movel     tadnaz        DUPNAZ
353900940518     C                   Z-ADD     TADMIN        DUPMIN
354000940518     C                   Z-ADD     TADMAX        DUPMAX
354100930629     C*
354200940525     C                   SETON                                        14
354300020408     c                   eval      woksfl = '1'
354400000000     C     GIUCTR        TAG
354500940525     C                   SETON                                        22
354600940706     C                   UPDATE    TA01S05
354700940525     C   14              SETOFF                                       2214
354800930629     C*
354900940525     C   22              Z-ADD     NR1           REC1
355000940525     C   22              GOTO      ENDCT2
355100940511    2C                   ENDIF
355200940511     C*
355300020408     c                   if        woksfl = '0'
355400020408     C                   UPDATE    TA01S05
355500020408     c                   endif
355600940706     C                   READC     TA01S05                                29
355700940511    1C                   ENDDO
355800940510     C*
355900940509     C     ENDCT2        ENDSR
356000930629     C*
356100940509     C*--- IMMISSIONE TARIFFE ----------------------------------------*
356200921118     C     ADDTAD        BEGSR
356300921119     C                   Z-ADD     1             NR1
356400940706     C                   READC     TA01S05                                29
356500940524     C     *IN29         DOWEQ     '0'
356600921118     C     TADLNP        IFGT      0
356700000000     C                   EXSR      WTRTAD
356800000000     C                   END
356900921119     C*
357000940706     C                   READC     TA01S05                                29
357100000000     C                   END
357200000000     C                   ENDSR
357300940509     C*
357400990601     C*--- SCRITTURA TITAD SU DISCO ----------------------------------*
357500921118     C     WTRTAD        BEGSR
357600980219     C* VALORE POSITIVO NEI CAMPI
357700980220     C                   MLLZO     1             TADSGL
357800980219     C                   MLLZO     1             TADITR
357900980219     C                   MLLZO     1             TADINO
358000980219     C                   MLLZO     1             TADRPV
358100980219     C                   MLLZO     1             TADMIN
358200980219     C                   MLLZO     1             TADMAX
358300980219     C*
358400940629     C                   Z-ADD     V1CKSC        TADKSC
358500000000     C                   Z-ADD     PRG           TADPRG
358600000000     C                   MOVEL     *BLANKS       TADATB
358700910605     C*
358800000000     C                   Z-ADD     1             A
358900940524     C     TADCTS        LOOKUP    CCT(A)                                 35
359000990608     C   35              MOVE      ORP(A)        TADORP
359100940524     C  N35              MOVEL     *BLANKS       TADORP
359200900320     C                   MOVEL     TAMCTR        TADCTR
359300940510     C*
359400061011     C  n50KTAD          SETLL     TITAD000                               32
359500061011     c   50ktad          setll     tiofd000                               32
359600940510     C*
359700940524     C     *IN32         IFEQ      *OFF
359800040913     C                   Z-ADD     DATEU8        TADDTR
359900940511     C                   MOVEL     *BLANKS       TADFTR
360000061011     C  n50              WRITE     TITAD000                             19
360100061011     c   50              write     tiofd000                             19
360200940510     C*
360300940510     C                   ELSE
360400940510     C*
360500940518     C                   Z-ADD     TADITR        RISITR
360600940518     C                   Z-ADD     TADINO        RISINO
360700940518     C                   Z-ADD     TADRPV        RISRPV
360800940518     C                   Z-ADD     TADMIN        RISMIN
360900940518     C                   Z-ADD     TADMAX        RISMAX
361000940518     C*
361100061011     C  n50KTAD          READE     TITAD000                               H2
361200061011     c   50ktad          reade     tiofd000                               H2
361300000000     C   H2              GOTO      FINE
361400000000     C                   MOVEL     *BLANKS       TADATB
361500130124     C********           Z-ADD     DATEU8        TADDTR
361600940511     C                   MOVEL     *BLANKS       TADFTR
361700940518     C                   Z-ADD     RISITR        TADITR
361800000000     C                   Z-ADD     RISINO        TADINO
361900000000     C                   Z-ADD     RISRPV        TADRPV
362000940518     C                   Z-ADD     RISMIN        TADMIN
362100940518     C                   Z-ADD     RISMAX        TADMAX
362200061011     C  n50              UPDATE    TITAD000
362300061011     c   50              update    tiofd000
362400940510     C                   ENDIF
362500940510     C*
362600921118     C  N19              SETON                                        30
362700921118     C                   ENDSR
362800940509     C*
362900940509     C*--- SR AGGIORNAMENTO ------------------------------------------*
363000921118     C     AGGIOR        BEGSR
363100050701
363200050701      * prima di caricare il subfile se è una tariffa a spedizione leggo
363300050701      * tutto il dettaglio per controllare che non esistano più scaglioni
363400050701      * per la stessa lnp e cts
363500050701      * questo perchè in fatturazione viene preso solo 1 scaglione quindi
363600050701      * non ha senso inserirne di più
363700050701     c                   If        tiptar = '2'
363800050701     c                   ExSr      sr_ctrtarspe
363900050701     c                   EndIf
364000110128
364100110128      /free
364200110128       //?Se tariffa FedEx devo controllare il rapporto perso volume
364300110128       //?deve essere uguale per tutti gli scaglioni
364400110128        IF  V1Cnet = 'F';
364500110128          exsr CtrRpv;
364600110128        ENDIF;
364700110128      /end-free
364800050701
364900921118     C     SUAGG2        TAG
365000061011     C  n50KTTAD         SETLL     TITAD000
365100061011     c   50kttad         setll     tiofd000
365200921118     C                   EXSR      PULSF
365300921118     C                   EXSR      CARSFA
365400930224     C     *IN21         IFEQ      '1'
365500921118     C*
365600921118     C     SUAGG1        TAG
365700940526     C                   Z-ADD     0             V1CRDA
365800940526     C                   MOVEL     *BLANKS       V1CRA
365900900924     C                   SETOFF                                       19
366000120315
366100120315       //?Imposto i tasti funzione nella riga mobile
366200120315     c                   eval      wPrima = *off
366300120315     c                   eval      wSeconda = *off
366400120315     c                   eval      wTerza = *on
366500120315     c                   eval      wQuarta = *off
366600120315     c                   ExSr      Tastifun
366700120315
366800000000     C     SUAGG         TAG
366900120319     C                   WRITE     TA01Z04
367000940706     C                   EXFMT     TA01C03
367100921118     C*
367200940526     C* PULIZIA CAMPO MESSAGGIO E RELATIVO INDICATORE (*IN28)
367300940526     C                   CLEAR                   VCCMSG
367400940526     C                   SETOFF                                       28
367500940624     C* AZZERO GLI INDICATORI RELATIVI AI MESSAGGI DI ERRORE
367600940526     C                   SETOFF                                       596162
367700960522     C                   SETOFF                                       89
367800940526     C*
367900120315     c  nkc
368000120315     CanNKL              DO
368100921118     C** CMD9 -IMMISSIONE
368200921118     C   KI              DO
368300050630werr c                   if        werr = *Off
368400110128     c                             and not werrRpv
368500921118     C                   EXSR      IMMISS
368600050701
368700050701      * per tariffa a spedizione devo controllare ora se tutto a posto
368800050701      * perchè non ragiono sul subfile ma sui dati caricati sul titad
368900050701     c                   If        tiptar = '2'
369000050701     c                   ExSr      sr_ctrtarspe
369100050701     c                   EndIf
369200050701
369300050701      * se errore per + scaglioni in tariffa a spedizione imposto
369400050701      * il msg e accendo l'indicatore
369500050701     c                   If        werr = *On
369600050701     c                   Eval      vccmsg = 'Par ' + wlnp + ' Arrivo ' +
369700050701     c                             wcts
369800050701     c                   If        wcap <> *Blanks
369900050701     c                   Eval      vccmsg = %trim(vccmsg) +
370000050701     c                             ' - c.a.p. ' + wcap
370100050701     c                   EndIf
370200050701     c                   Eval      vccmsg = %trim(vccmsg) +
370300050701     c                             ' presenti più scaglioni'
370400050701     c                   Eval      *In28 = *On
370500050701     c                   Goto      suagg2
370600050701     c                   EndIf
370700110128
370800110128      /free
370900110128       //?Se tariffa FedEx devo controllare il rapporto perso volume
371000110128       //?deve essere uguale per tutti gli scaglioni
371100110128        IF  V1Cnet = 'F';
371200110128          exsr CtrRpv;
371300110128        ENDIF;
371400110128      /end-free
371500110128       //?Se tariffa FedEx e scaglioni con rapporto peso volume
371600110128       //?diverso emetto messaggio di errore
371700110128     c                   IF        werrRpv
371800110128     c                   eval      VCCmsg = 'Par ' + %editc(wlnpRpv:'X') +
371900110128     c                             ' Arrivo ' + wctsRpv +
372000110128     c                             ' Scaglione ' + %editc(wsglRpv:'4') +
372100110128     c                             ' presente P/V diverso'
372200110128     c                   eval      *in28 = *on
372300110128     c                   goto      suagg2
372400110128     c                   ENDIF
372500110128
372600980313     C* CONTROLLO SE TARIFFA IMPORT CON LA TARIFFA PARTICOLARE ASSIC. X CONTO
372700980313     C     UNO           IFEQ      ' '
372800980313     C                   EXSR      CTRIMP
372900980313     C   28              MOVEL     MSG(72)       VCCMSG
373000980313     C   28              CLEAR                   V1CMSG
373100980313     C   28              MOVEL     '1'           UNO
373200980313     C   28              GOTO      SUAGG2
373300980313     C                   END
373400120314       //?F1 - dati manca tariffa
373500120314     c                   IF        *inka
373600120314     c                   exsr      gesw01
373700120315     c                   goto      suagg
373800120314     c                   ENDIF
373900120315       //?F3 fine
374000120315     c   kc              goto      fine
374100921118     C*
374200921118     C** CMD12-FINE LAVORO NO AGGIORNAMENTO
374300050630     C   kl              GOTO      SUAGG1
374400921118     C** CMD6 -AGGIORNAMENTO E FINE LAVORO
374500921118     C  NKF              GOTO      SUAGG2
374600921118     C   KF              GOTO      ENDAGG
374700120315       //?F24 Altri tasti
374800120315     c                   IF        *inky
374900120315     c                   exsr      gesf24
375000120315     c                   goto      suagg
375100120315     c                   ENDIF
375200050630werr c                   endif
375300921118     C                   END
375400900924     C*
375500921118     C** ROLLUP  AVANZAMENTO NO AGG.
375600940524     C     *IN25         IFEQ      *ON
375700940310     C                   MOVEL     KE            KX
375800940518     C                   MOVEL     PARX          COMX              9
375900061011     C  n50KSCOR         SETLL     TITAD000                               32
376000061011     c   50kscor         setll     tiofd000                               32
376100061011     C   32
376200061011     Cann50KTAM2         READE     TITAD000                               33
376300061011     c   32
376400061011     Can 50ktam2         reade     tiofd000                               33
376500940310     C                   EXSR      CARSFA
376600940310     C                   GOTO      SUAGG1
376700940310     C                   ENDIF
376800921118     C*
376900921118     C** CONTROLLI AGGIORNAMENTO
377000000000     C                   EXSR      CTRAGG
377100940526     C   28              GOTO      SUAGG
377200921118     C*
377300921118     C** ESEGUE AGGIORNAMENTI
377400000000     C                   EXSR      AGGTAD
377500050701
377600050701      * per tariffa a spedizione devo controllare ora se tutto a posto
377700050701      * perchè non ragiono sul subfile ma sui dati caricati sul titad
377800050701     c                   If        tiptar = '2'
377900050701     c                   ExSr      sr_ctrtarspe
378000050701     c                   EndIf
378100050701
378200050701      * se errore per + scaglioni in tariffa a spedizione imposto
378300050701      * il msg e accendo l'indicatore
378400050701     c                   If        werr = *On
378500050701     c                   Eval      vccmsg = 'Par ' + wlnp + ' Arrivo ' +
378600050701     c                             wcts
378700050701     c                   If        wcap <> *Blanks
378800050701     c                   Eval      vccmsg = %trim(vccmsg) +
378900050701     c                             ' - c.a.p. ' + wcap
379000050701     c                   EndIf
379100050701     c                   Eval      vccmsg = %trim(vccmsg) +
379200050701     c                             ' presenti più scaglioni'
379300050701     c                   Eval      *In28 = *On
379400050701     c                   Goto      suagg
379500050701     c                   EndIf
379600110128
379700110128      /free
379800110128       //?Se tariffa FedEx devo controllare il rapporto perso volume
379900110128       //?deve essere uguale per tutti gli scaglioni
380000110128        IF  V1Cnet = 'F';
380100110128          exsr CtrRpv;
380200110128        ENDIF;
380300110128      /end-free
380400110128       //?Se tariffa FedEx e scaglioni con rapporto peso volume
380500110128       //?diverso emetto messaggio di errore
380600110128     c                   IF        werrRpv
380700110128     c                   eval      VCCmsg = 'Par ' + %editc(wlnpRpv:'X') +
380800110128     c                             ' Arrivo ' + wctsRpv +
380900110128     c                             ' Scaglione ' + %editc(wsglRpv:'4') +
381000110128     c                             ' presente P/V diverso'
381100110128     c                   eval      *in28 = *on
381200110128     c                   goto      suagg
381300110128     c                   ENDIF
381400050701
381500980313     C* CONTROLLO SE TARIFFA IMPORT CON LA TARIFFA PARTICOLARE ASSIC. X CONTO
381600980313     C     UNO           IFEQ      ' '
381700980313     C                   EXSR      CTRIMP
381800980313     C   28              MOVEL     MSG(72)       VCCMSG
381900980313     C   28              CLEAR                   V1CMSG
382000980313     C   28              MOVEL     '1'           UNO
382100980313     C   28              GOTO      SUAGG
382200980313     C                   END
382300120315       //?F1 - dati manca tariffa
382400120315     c                   IF        *inka
382500120315     c                   exsr      gesw01
382600120315     c                   goto      suagg
382700120315     c                   ENDIF
382800011214     C*
382900011214     C** F10 - STAMPA TARIFFA (ALLA FINE)
383000011214     C   KJ
383100011214     CAN 15              GOTO      ENDAGG
383200011214     C   KJ
383300011214     CANN15              MOVEL     *INKJ         *INKF
383400921118     C*
383500961212     C** CMD6 -AGGIORNAMENTO E FINE
383600991115     C   KF              MOVEL     'C'           FLGRTN
383700921118     C   KF              GOTO      ENDAGG
383800120319       //?F24 Altri tasti
383900120319     c                   IF        *inky
384000120319     c                   exsr      gesf24
384100120319     c                   goto      suagg
384200120319     c                   ENDIF
384300900924     C*
384400961212     C** CMD15-P O R T O  (ABILITATO SOLO NELLE OFFERTE)
384500980902     C   KP              DO
384600031124      * recupero dalla tabella CAT la data di elaborazione CAT consigliata
384700031124     c                   Clear                   Tibs02Ds
384800031124     c                   Clear                   dCat
384900031124     c                   Eval      T02Mod = 'C'
385000031124     c                   Eval      T02Sif = Knsif
385100031124     c                   Eval      T02Cod = 'CAT'
385200031124     c                   Movel(p)  '1'           T02Ke1
385300031124     c                   Call      'TIBS02R'
385400031124     c                   Parm                    Kpjba
385500031124     c                   Parm                    Tibs02Ds
385600031124     c                   If        T02Err = *Blanks
385700031124     c                   Movel     T02Uni        dCat
385800031124     c                   EndIf
385900031124    1 *
386000961212     C                   CLEAR                   DSTE00
386100961212     C                   MOVEL     'O02'         D00OP0
386200961212     C                   MOVEL     'F15'         D00OP1
386300961212     C                   MOVEL     'P'           D00TLA
386400091127      * Con l'arrivo delle trattative dobbiamo segnalare ai pgm del CAT che
386500091127      * stiamo parlando di trattativa e non di visita
386600091127      * Se trattativa (ind. 85 acceso) impostiamo "X" nel campo D00DSF
386700091125     C**                 MOVEL     'S'           D00DSF
386800091125     C   85              MOVEL     'X'           D00DSF
386900091127     C                   MOVEL     'O'           D00CTO
387000961212     C                   MOVEL     V1CNRV        D00KSC
387100961212     C                   MOVEL     V1CCTR        D00CTR
387200091125     C                   MOVEL     V1CPRG        D00PRG
387300961212     C                   MOVEL     V1CFIE        D00FIE
387400961212     C                   MOVEL     V1CTSP        D00TSP
387500011023     C                   MOVEL     V1CDIV        D00DIV
387600020205     C                   MOVE      V1CNET        D00DPD
387700011030     C                   MOVEL     *BLANKS       D00PRI
387800031124      * propongo data massima tra la data del giorno e quella recuperata
387900031124      * dalla tavella CAT
388000031124     c                   if        dateu8 > §CATdta
388100031124     C                   MOVEL     DATEU8        D00DTC
388200031124     C                   ELSE
388300031124     C                   MOVEL     §CATDTA       D00DTC
388400031124     C                   ENDIF
388500031124      *
388600970131     C                   CALL      'TNTE01C'
388700961212     C                   PARM                    KPJBA
388800961212     C                   PARM                    DSTE00
388900961212     C* D00ERR = "1" --> ERRORE
389000961212     C     D00ERR        IFEQ      '1'
389100961212     C                   SETON                                          28
389200961212     C                   MOVEL     D00MSG        VCCMSG
389300961219     C                   GOTO      SUAGG1
389400961212     C                   ENDIF
389500961220     C* RICARICO IL SFILE PERCHE' POTREBBERO ESSERCI STATE VARIAZIONI
389600961219     C                   GOTO      SUAGG2
389700980902     C                   ENDDO
389800961212     C*
389900961212     C** CMD17-D E L T A
390000980902     C   KR              DO
390100980514     C                   CLEAR                   DSTE00
390200980514     C     *IN50         IFEQ      *OFF
390300980514     C                   MOVEL     'T02'         D00OP0
390400980514     C                   MOVEL     'T'           D00CTO
390500980514     C                   MOVEL     V1CKSC        D00KSC
390600980514     C                   MOVEL     V1CPRG        D00PRG
390700980514     C                   ELSE
390800980514     C                   MOVEL     'O02'         D00OP0
390900091127      * Con l'arrivo delle trattative dobbiamo segnalare ai pgm del CAT che
391000091127      * stiamo parlando di trattativa e non di visita
391100091127      * Se trattativa (ind. 85 acceso) impostiamo "X" nel campo D00DSF
391200091127     C                   MOVEL     'O'           D00CTO
391300091125     C   85              MOVEL     'X'           D00DSF
391400091125     C                   MOVEL     V1CPRG        D00PRG
391500980514     C                   MOVEL     V1CNRV        D00KSC
391600980514     C                   ENDIF
391700031124      * recupero dalla tabella CAT la data di elaborazione CAT consigliata
391800031124     c                   Clear                   Tibs02Ds
391900031124     c                   Clear                   dCat
392000031124     c                   Eval      T02Mod = 'C'
392100031124     c                   Eval      T02Sif = Knsif
392200031124     c                   Eval      T02Cod = 'CAT'
392300031124     c                   Movel(p)  '1'           T02Ke1
392400031124     c                   Call      'TIBS02R'
392500031124     c                   Parm                    Kpjba
392600031124     c                   Parm                    Tibs02Ds
392700031124     c                   If        T02Err = *Blanks
392800031124     c                   Movel     T02Uni        dCat
392900031124     c                   EndIf
393000031124    1 *
393100980514     C                   MOVEL     'F17'         D00OP1
393200091125      * Con l'arrivo delle trattative abbiamo modificato alcuni campi della DS
393300091125      * per una giusta gestione del CAT. non si mette + "S" nel campo
393400091125      * d00dsf in caso di tariffe / offerte e non trattativa
393500091125     C*****              MOVEL     'S'           D00DSF
393600980514     C                   MOVEL     'D'           D00TLA
393700980514     C                   MOVEL     V1CCTR        D00CTR
393800980514     C                   MOVEL     V1CFIE        D00FIE
393900980514     C                   MOVEL     V1CTSP        D00TSP
394000011023     C                   MOVEL     V1CDIV        D00DIV
394100020213     C                   MOVE      V1CNET        D00DPD
394200011030     C                   MOVEL     *BLANKS       D00PRI
394300031124      * propongo data massima tra la data del giorno e quella recuperata
394400031124      * dalla tavella CAT
394500031124     c                   if        dateu8 > §CATdta
394600031124     C                   MOVEL     DATEU8        D00DTC
394700031124     C                   ELSE
394800031124     C                   MOVEL     §CATDTA       D00DTC
394900031124     C                   ENDIF
395000031124      *
395100980514     C                   CALL      'TNTE01C'
395200980514     C                   PARM                    KPJBA
395300980514     C                   PARM                    DSTE00
395400980514     C* D00ERR = "1" --> ERRORE
395500980514     C     D00ERR        IFEQ      '1'
395600980514     C                   SETON                                          28
395700980514     C                   MOVEL     D00MSG        VCCMSG
395800980514     C                   GOTO      SUAGG1
395900980514     C                   ENDIF
396000980902     C                   ENDDO
396100900410     C**
396200920525     C**  CMD8 - M A N I P O L A Z I O N E
396300900410     C**
396400920525     C   KH              DO
396500900410     C                   EXSR      IMPMAN
396600941123     C*
396700900410     C     FOR07         TAG
396800070312     c                   eval      wintftc = *off
396900120713     c                   eval      wintcts = *off
397000940706     C                   EXFMT     TA01D07
397100941123     C* PULIZIA CAMPO MESSAGGIO E RELATIVO INDICATORE (*IN28)
397200941123     C                   CLEAR                   V6CMSG
397300960729     C                   SETOFF                                       289086
397400050616     c                   clear                   h7riga
397500050616     c                   clear                   h7colo
397600941123     C* AZZERO GLI INDICATORI RELATIVI AI MESSAGGI DI ERRORE
397700941123    1C     63            DO        76            I
397800941123     C                   MOVE      '0'           *IN(I)
397900941123    1C                   ENDDO
398000941123     C*
398100120315       //?F3 fine
398200120315     c   kc              GOTO      FINE
398300900410     C* CMD12 - RITORNO
398400940518     C   KL              GOTO      SUAGG1
398500900410     C* CONTROLLO
398600900410     C                   EXSR      CTRMAN
398700941109     C   28              GOTO      FOR07
398800070312      * se int.tariffe particolari riemetto la videata
398900070312     c                   if        wintftc = *on
399000070312     c                   goto      for07
399100070312     c                   endif
399200120713       //?se interrogazione codici tassazione riemetto la videata
399300120713     c                   IF        wintcts
399400120713     c                   goto      for07
399500120713     c                   ENDIF
399600921118     C*
399700920525     C* CMD6 - AGGIORNAMENTO TARIFFA
399800020312     c                   if        *inkf
399900020312     c                   clear                   wsimula
400000020312     C                   EXSR      MANIPO
400100020313     c                   if        wnoagg = *on
400200020313     c                   eval      vccmsg = msg(105)
400300020313     c                   eval      *in28 = *on
400400020312     c                   goto      suagg2
400500020312     c                   endif
400600020312     c                   eval      wsimula = '1'
400700020312     c                   exsr      manipo
400800020312     C                   GOTO      SUAGG2
400900020312     c                   endif
401000900410     C* ENTER - CONTROLLO E RIMANGO NELLA VIDEATA
401100900410     C                   GOTO      FOR07
401200900410     C                   END
401300960523     C**
401400960523     C**  CMD13 - D U P L I C A    T A R I F F A
401500960523     C**
401600960523     C     *INKM         IFEQ      *ON
401700960523     C* PULIZIA CAMPI VIDEATA
401800960523     C                   CLEAR                   V8CLNN
401900960523     C                   CLEAR                   V8CCTN
402000960523     C                   CLEAR                   V8CLNP
402100960523     C                   CLEAR                   V8CCTS
402200960523     C                   CLEAR                   WORP
402300960523     C                   CLEAR                   WORPN
402400960527     C                   CLEAR                   SAVCTN
402500960527     C                   CLEAR                   SAVLNN
402600041222      *
402700041222     c                   clear                   linpar
402800041223     c                   clear                   W_sbflnp
402900960523     C*
403000960523     C     FOR08         TAG
403100960523     C                   EXFMT     TA01D08
403200960523     C* PULIZIA CAMPO MESSAGGIO E RELATIVO INDICATORE (*IN28)
403300960523     C                   CLEAR                   V8CMSG
403400960523     C                   SETOFF                                       287172
403500960523     C                   SETOFF                                       7374
403600960523     C*
403700120315       //?F3 fine
403800120315     c   kc              GOTO      FINE
403900960523     C* CMD12 - RITORNO
404000960523     C   KL              GOTO      SUAGG1
404100041222
404200041222     C* CMD19 _ SOLO PER LA CARTELLO IMPOSTO PIU' LINEE PARTENZA DA CREARE
404300041222     C                   IF        *INKT
404400041222     C                   EXSR      SR_GESLNP
404500041222      * RIEMETTO LA VIDEATA
404600041222     C                   GOTO      FOR08
404700041222     C                   ENDIF
404800041222
404900960523     C* CONTROLLO
405000960523     C                   EXSR      CTRD08
405100960523     C   28              GOTO      FOR08
405200960523     C*
405300960523     C* CMD6 - AGGIORNAMENTO TARIFFA
405400990817     C     *INKF         IFEQ      '1'
405500041222      * duplica --- se ho solo un codice altrimenti elaboro la schiera
405600041222     c                   if        W_sbflnp <> ' '
405700041222     c                   z-add     1             po
405800041223     c                   dow       po<=120
405900041222     c                   if        lnpel(po) > *zeros
406000041222     c                   eval      v8clnn = lnpel(po)
406100041222     c                   exsr      duptar
406200041223     c                   add       1             po
406300041222     c                   iter
406400041222     c                   else
406500041222     c                   leave
406600041222     c                   endif
406700041222     c                   enddo
406800041222
406900041222     c                   else
407000041222      *
407100990817     C                   EXSR      DUPTAR
407200041222
407300041222     c                   endif
407400990817     C*
407500990817     C                   GOTO      SUAGG2
407600990817     C                   ENDIF
407700960523     C* ENTER - CONTROLLO E RIMANGO NELLA VIDEATA
407800960523     C                   GOTO      FOR08
407900960523     C                   ENDIF
408000921118     C*
408100921118     C** SE CANCELLATI RECORD RIEMETTO SUBFILE
408200921118     C   19              GOTO      SUAGG2
408300900410     C**
408400940526     C     V1CRDA        IFNE      0
408500940526     C     V1CRA         ORNE      *BLANKS
408600921118     C                   GOTO      ENDAGG
408700900321     C                   END
408800000000     C** E' STATA DIGITATA UNA CHIAVE DI RICERCA  E
408900000000     C** QUINDI DOPO AVER EFFETTUTATO L'AGGIORNAMENTO
409000000000     C** RICERCO LA CHIAVE E RIEMETTO IL SUBFILE
409100000000     C                   GOTO      SUAGG1
409200921118     C                   END
409300930224     C                   END
409400050701
409500050630      * se F12 e errore non posso tornare indietro devo restare nel
409600050630      * subfile ed eliminare gli scaglioni in più
409700120315      //?stessa cosa per F3
409800120315     c                   If        (*inkl or *inkc) and werr = *On
409900050701     c                             and tiptar = '2'
410000050701     c                   ExSr      sr_ctrtarspe
410100050701
410200050701      * se errore per + scaglioni in tariffa a spedizione imposto
410300050701      * il msg e accendo l'indicatore
410400050701     c                   If        werr = *On
410500050701     c                   Eval      vccmsg = 'Par ' + wlnp + ' Arrivo ' +
410600050701     c                             wcts
410700050701     c                   If        wcap <> *Blanks
410800050701     c                   Eval      vccmsg = %trim(vccmsg) +
410900050701     c                             ' - c.a.p. ' + wcap
411000050701     c                   EndIf
411100050701     c                   Eval      vccmsg = %trim(vccmsg) +
411200050701     c                             ' presenti più scaglioni'
411300050701     c                   Eval      *In28 = *On
411400050701     c                   EndIf
411500050701
411600050701     c   28              goto      suagg1
411700050630     c                   endif
411800110128
411900110128       //?Se F12 ed errore per il rapporto peso volume devo prima
412000110128       //?i dati a video poi posso uscire
412100120315       //?stessa cosa per F3
412200120315     c                   IF        (*inkl or *inkc) and werrRpv and
412300110128     c                             V1Cnet = 'F'
412400110128     c                   exsr      CtrRpv
412500110128       //?Se tariffa FedEx e scaglioni con rapporto peso volume
412600110128       //?diverso emetto messaggio di errore
412700110128     c                   IF        werrRpv
412800110128     c                   eval      VCCmsg = 'Par ' + %editc(wlnpRpv:'X') +
412900110128     c                             ' Arrivo ' + wctsRpv +
413000110128     c                             ' Scaglione ' + %editc(wsglRpv:'4') +
413100110128     c                             ' presente P/V diverso'
413200110128     c                   eval      *in28 = *on
413300110128     c                   goto      suagg1
413400110128     c                   ENDIF
413500110128     c                   ENDIF
413600050701
413700000000     C** ENTER  AGGIORNAMENTO E RIMANGO FERMO
413800050630     C*  RIPETO RIEMPIMENTO SF
413900921118     C     ENDAGG        ENDSR
414000900410     C*
414100930623     C*--- IMPOSTO CAMPI MANIPOLAZIONE -------------------------------*
414200900410     C     IMPMAN        BEGSR
414300900410     C                   MOVE      '+'           VIDAD
414400900410     C                   Z-ADD     0             VIDLIR
414500900530     C                   Z-ADD     0             VIDPEI
414600020312
414700020314     c                   eval      vidtad = 'SI'
414800020312     c                   eval      vidtpt = 'NO'
414900050616     c                   eval      vidtgc = 'NO'
415000020312
415100900410     C                   MOVE      'E'           VIDADE
415200991008     C                   Z-ADD     0             VIDALI
415300940526     C                   MOVE      '+'           VIDRAD
415400910606     C                   Z-ADD     0             VIDVRR
415500050627     c                   Clear                   vidarr
415600061207     c                   clear                   vidirr
415700960523     C                   MOVE      'O'           VIDIOP
415800960523     C                   CLEAR                   VMP
415900930625     C                   MOVE      'O'           VIDIO
416000930628     C                   CLEAR                   VMC
416100930625     C                   MOVE      'O'           VIDIO2
416200940518     C                   Z-ADD     0             VIDSGL
416300070309     c                   eval      vidioftc = 'O'
416400070309     c                   clear                   vftc
416500930625     C                   MOVE      'S'           VIDVTC
416600930625     C                   MOVE      'S'           VIDVTP
416700941124     C                   MOVE      'S'           VIDVMN
416800941124     C                   MOVE      'S'           VIDVMX
416900990624     C* DIVISA
417000990624     C                   MOVEL     V1CDIV        TAMDIV
417100010323     C* SE DIVISA SENZA DECIMALI NON EMETTO LA RIGA RELATIVA ALL'ARROTONDAMENTO
417200010323     C* AI DECIMALI
417300010323     C     §CVFDC        IFNE      'S'
417400010323     C* IMPOSTO I CAMPI A VIDEO PROTETTI E NON VISUALIZZZATI
417500010323     C* NDPR SOSTITUISCE GLI INDICATORI IN QUANTO FINITI
417600010323     C                   MOVEL     NDPR          PROSAD
417700010323     C                   ENDIF
417800010329     C* PULISCO I CAMPI REALTIVI LÌARROTONDAMENTO
417900010329     C                   CLEAR                   VIDSAD
418000010329     C                   CLEAR                   VIDDEC
418100050616
418200050616     c                   clear                   h7riga
418300050616     c                   clear                   h7colo
418400010323     C*
418500900410     C                   ENDSR
418600900410     C*
418700930623     C*--- CONTROLLO CAMPI VIDEO MANIPOLAZIONE -----------------------*
418800900410     C     CTRMAN        BEGSR
418900020312
419000070309      * spengo indicatori che uso in questa videata ma che sono stati
419100070309      * utilizzati in videate precedenti
419200020312     c                   eval      *in82 = *off
419300070309      * imposto subito il campo di comodo delle tariffe particolari xchè
419400070309      * mi serve da subito e non solo nel momento dei controlli delle
419500070309      * singole tariffe particolari
419600070309     c                   movea     vftc          comftc
419700020314
419800020327      * deve essere fatta almeno una scelta
419900020314     c                   if        vidtad = 'NO' and vidtpt = 'NO'
420000050616     c                             and vidtgc = 'NO'
420100020314     c                   eval      v6cmsg = msg(106)
420200020314     c                   eval      *in24 = *on
420300020314     c                   eval      *in28 = *on
420400020314     c                   goto      endman
420500020314     c                   endif
420600020312
420700900606     C                   MOVE      VIDPEI        VIDPER            5 4
420800940526     C*
420900940526     C****  VALORE/PERCENTUALE  ****
421000900410     C     VIDLIR        IFNE      0
421100940526     C     VIDPER        ANDNE     0
421200940526     C                   MOVEL     MSG(33)       V6CMSG
421300940526     C                   SETON                                        6328
421400900410     C                   GOTO      ENDMAN
421500940526     C                   ENDIF
421600050617
421700050617      * Se immesso un importo di aumento/diminuzione non posso manipolare
421800050617      * le tariffe particolari visto che sono miste cioè una parte espresse
421900050617      * ad importo e una parte espresse in %
422000050617      * la manipolazione è possibile solo inserendo una % +/-
422100050617     c                   If        vidlir <> *Zeros and vidtpt = 'SI'
422200050617     c                   Eval      v6cmsg = msg(116)
422300050617     c                   Eval      *In28 = *On
422400050617     c                   Eval      *In82 = *On
422500050617     c                   Goto      endman
422600050617     c                   EndIf
422700050616
422800050617      * rapporto peso/volume immettere o il valore o l'annullamento
422900061207      * o l'inserimento
423000061207     c                   If        (vidvrr <> *Zeros and vidarr <> *blanks) or
423100061207     c                             (vidvrr <> *Zeros and vidirr <> *zeros) or
423200061207     c                             (vidarr <> *blanks and vidirr <> *zeros)
423300050616     c                   Eval      v6cmsg = msg(33)
423400050616     c                   Eval      *In28 = *On
423500050616     c                   z-add     15            H7riga
423600061207     c                   z-add     29            H7colo
423700050616     c                   Goto      endman
423800050616     c                   EndIf
423900900410     C*
424000910606     C     VIDVRR        IFEQ      0
424100940526     C     VIDLIR        ANDEQ     0
424200940526     C     VIDPER        ANDEQ     0
424300050616     c     vidarr        andeq     *Blanks
424400061207     c     vidirr        andeq     *zeros
424500940526     C                   MOVEL     MSG(33)       V6CMSG
424600940526     C                   SETON                                        6328
424700900410     C                   GOTO      ENDMAN
424800940526     C                   ENDIF
424900020312
425000020312      * Manipolazione tariffe particolari
425100020327      * devono essere presenti le tariffe particolari
425200020312     c                   if        vidtpt = 'SI'
425300070129     c  n50ktam2         chain(n)  titpt01l
425400070129     c   50ktam2         chain(n)  tiopt01l
425500061011     c                   if        not %found
425600020312     c                   eval      v6cmsg = msg(104)
425700020312     c                   eval      *in28 = *on
425800020312     c                   eval      *in82 = *on
425900020312     c                   goto      endman
426000020312     c                   endif
426100020312     c                   endif
426200050617
426300050617      * Manipolazione tariffe di giacenza
426400050617      * devono essere presenti le tariffe di giacenza
426500050617     c                   if        vidtgc = 'SI'
426600050913     c  n50ktam2         chain     titgc01l
426700050913     c   50ktam2         chain     tiogc01l
426800050913     c                   if        not %found or tgcatb = 'A'
426900050617     c                   eval      v6cmsg = msg(117)
427000050617     c                   eval      *in28 = *on
427100050617     c                   z-add     10            H7riga
427200050617     c                   z-add     35            H7colo
427300050617     c                   goto      endman
427400050617     c                   endif
427500050617     c                   endif
427600020312
427700930623     C*
427800050616     C* 88 OFF -  TARIFFA NON IN % verifico se si
427900941124     C*   POSSONO INSERIRE DECIMALI
428000941124     C  N88VIDLIR        IFGT      0
428100941124     C                   MOVE      VIDLIR        W0030
428200941124     C     W0030         IFGT      0
428300050616     C*  la DIVISA NON AMMETTE DECIMALI
428400990617     C     §CVFDC        ANDNE     'S'
428500990617     C                   MOVEL     MSG(79)       V6CMSG
428600941124     C                   SETON                                        6328
428700941124     C                   GOTO      ENDMAN
428800941124     C                   ENDIF
428900941124     C                   ENDIF
429000941124     C*
429100050616     C* 88 OFF -  TARIFFA NON IN % verifico se si
429200050616     C*   POSSONO INSERIRE DECIMALI
429300960729     C  N88VIDPEI        IFGT      0
429400960729     C     VIDALI        ANDEQ     0
429500050616     C*  la DIVISA NON AMMETTE DECIMALI
429600010329     C     §CVFDC        ANDNE     'S'
429700960729     C                   MOVEL     MSG(69)       V6CMSG
429800960729     C                   SETON                                        8628
429900960729     C                   GOTO      ENDMAN
430000960729     C                   ENDIF
430100990617     C*
430200050616     C* 88 OFF -  TARIFFA NON IN % verifico se si
430300050616     C*   POSSONO INSERIRE DECIMALI
430400990617     C  N88VIDLIR        IFGT      0
430500990617     C     VIDPEI        ORGT      0
430600990617     C                   MOVE      VIDALI        W0030
430700991008     C                   Z-ADD     VIDALI        W0010
430800990617     C     W0030         IFGT      0
430900050616     C*  la DIVISA NON AMMETTE DECIMALI
431000991008     C     §CVFDC        ANDNE     'S'
431100990617     C                   MOVEL     MSG(79)       V6CMSG
431200990617     C                   SETON                                        8628
431300990617     C                   GOTO      ENDMAN
431400990617     C                   ENDIF
431500991008     C* ULTIMA CIFRA DELL'ARROTONDAMENTO DEVE ESSERE UGUALE A ZERO
431600991008     C* NEL CASO DI MONETA SENZA DECIMALE E' L'ULTIMO INTERO + I DECIMALI
431700991008     C* NEL CASO DI MONETA CON DECIMALI E' L'ULTIMO DECIMALE
431800991008     C     VIDALI        IFNE      *ZEROS
431900991008     C     §CVFDC        IFNE      'S'
432000991008     C* INTERO + DECIMALI
432100991008     C                   MOVE      VIDALI        W0040             4 0
432200991008     C*
432300991008     C     W0040         IFNE      0
432400991008     C                   MOVEL     MSG(83)       V6CMSG
432500991008     C                   SETON                                        8628
432600991008     C                   GOTO      ENDMAN
432700991008     C                   ENDIF
432800991008     C                   ELSE
432900991008     C* DECIMALI
433000991008     C                   MOVE      VIDALI        W0010
433100991008     C*
433200991008     C     W0010         IFNE      0
433300991008     C                   MOVEL     MSG(84)       V6CMSG
433400991008     C                   SETON                                        8628
433500991008     C                   GOTO      ENDMAN
433600991008     C                   ENDIF
433700010323     C*
433800010323     C* SE C'E' ARROTONDAMENTO ANCHE AL DECIMALE  DO ERRORE
433900010323     C*
434000010323     C     VIDSAD        IFEQ      'S'
434100010323     C                   MOVEL     MSG(94)       V6CMSG
434200010323     C                   SETON                                        8628
434300010323     C                   GOTO      ENDMAN
434400010323     C                   ENDIF
434500010323     C*
434600991008     C                   ENDIF
434700991008     C                   ENDIF
434800991008     C*
434900990617     C                   ENDIF
435000960729     C*
435100050616     C   88VIDLIR        IFGT      0
435200960729     C     VIDPEI        ORGT      0
435300990617     C                   MOVE      VIDALI        W0010             1 0
435400990617     C     W0010         IFGT      0
435500990617     C                   MOVEL     MSG(80)       V6CMSG
435600960729     C                   SETON                                        8628
435700960729     C                   GOTO      ENDMAN
435800960729     C                   ENDIF
435900960729     C                   ENDIF
436000960729     C*
436100960523     C* VIDIOP = I  E NON E' STATA INSERITA NESSUNA LINEA PARTENZA
436200960523     C                   XFOOT     VMP           COMVMP            5 0
436300960523     C     VIDIOP        IFEQ      'I'
436400960523     C     COMVMP        ANDEQ     0
436500960523     C                   MOVEL     MSG(60)       V6CMSG
436600960523     C                   SETON                                        9028
436700960523     C                   GOTO      ENDMAN
436800960523     C                   ENDIF
436900960523     C*
437000940526     C* VIDIO = I  E NON E' STATO INSERITO NESSUN CODICE TASSAZIONE
437100930625     C                   MOVEA     VMC           COMVMC           20
437200930623     C     VIDIO         IFEQ      'I'
437300930625     C     COMVMC        ANDEQ     *BLANKS
437400940526     C                   MOVEL     MSG(34)       V6CMSG
437500070309     C                   SETON                                        6628
437600930623     C                   GOTO      ENDMAN
437700930628     C                   ENDIF
437800960522     C*
437900940530     C* VIDIO2 = I  E NON E' STATO INSERITO LO SCAGLIONE
438000930623     C     VIDIO2        IFEQ      'I'
438100940518     C     VIDSGL        ANDEQ     0
438200941123     C                   MOVEL     MSG(35)       V6CMSG
438300940530     C                   SETON                                        6528
438400930623     C                   GOTO      ENDMAN
438500930628     C                   ENDIF
438600110131
438700110131       //?Se tariffa FedEx e Manipolazione rapporto peso volume
438800110131       //?non si può fare nessuna selezione su LNP - CTS - Scaglione
438900110131     c                   IF        V1Cnet = 'F' and (VIDvrr > 0 or
439000110131     c                              VIDarr <> *blanks or VIDirr > 0)
439100110131       //?Inclusione LNP
439200110131     c                   IF        VIDiop = 'I'
439300110131     c                   eval      v6cmsg = msg(118)
439400110131     c                   eval      *in28 = *on
439500110131     c                   z-add     17            H7riga
439600110131     c                   z-add     21            H7colo
439700110131     c                   goto      endman
439800110131     c                   ENDIF
439900110131       //?Omissione LNP
440000110131     c                   IF        comvmp > 0
440100110131     c                   eval      v6cmsg = msg(118)
440200110131     c                   eval      *in28 = *on
440300110131     c                   eval      *in90 = *on
440400110131     c                   goto      endman
440500110131     c                   ENDIF
440600110131       //?Inclusione CTS
440700110131     c                   IF        VIDio  = 'I'
440800110131     c                   eval      v6cmsg = msg(103)
440900110131     c                   eval      *in28 = *on
441000110131     c                   z-add     18            H7riga
441100110131     c                   z-add     21            H7colo
441200110131     c                   goto      endman
441300110131     c                   ENDIF
441400110131       //?Omissione CTS
441500110131     c                   IF        comvmc > *blanks
441600110131     c                   eval      v6cmsg = msg(103)
441700110131     c                   eval      *in28 = *on
441800110131     c                   eval      *in66 = *on
441900110131     c                   goto      endman
442000110131     c                   ENDIF
442100110131       //?Inclusione Scaglione
442200110131     c                   IF        VIDio2 = 'I'
442300110131     c                   eval      v6cmsg = msg(102)
442400110131     c                   eval      *in28 = *on
442500110131     c                   z-add     19            H7riga
442600110131     c                   z-add     21            H7colo
442700110131     c                   goto      endman
442800110131     c                   ENDIF
442900110131       //?Omissione Scaglione
443000110131     c                   IF        VIDsgl > 0
443100110131     c                   eval      v6cmsg = msg(102)
443200110131     c                   eval      *in28 = *on
443300110131     c                   eval      *in65 = *on
443400110131     c                   goto      endman
443500110131     c                   ENDIF
443600110131     c                   ENDIF
443700070309
443800070309      * vidioftc = I  e non è stata inserita nessuna tariffa particolare
443900070309     c                   if        vidtpt = 'SI' and
444000070309     c                             vidioftc = 'I' and comftc = *blanks
444100070309     c                   eval      v6cmsg = msg(38)
444200070309     c                   eval      *in28 = *on
444300070309     c                   eval      *in64 = *on
444400070309     c                   leavesr
444500070309     c                   endif
444600070309      * se immesse delle tariffe particolari e non è stata richiesta la
444700070309      * manipolazione tariffe particolari errore
444800070309     c                   if        vidtpt <> 'SI' and comftc <> *blanks
444900070309     c                   eval      v6cmsg = msg(39)
445000070309     c                   eval      *in28 = *on
445100070309     c                   eval      *in64 = *on
445200070309     c                   leavesr
445300070309     c                   endif
445400930623     C*
445500960523     C****  LINEE PARTENZA  ****
445600960523     C                   DO        5             X
445700960523     C     VMP(X)        IFGT      0
445800960523     C     VMP(X)        CHAIN     AZORG                              34
445900960523     C     *IN34         IFEQ      *ON
446000960523     C     ORGFVA        ORNE      ' '
446100960523     C     ORGFAG        ORNE      'A'
446200960523     C     ORGFAG        ANDNE     'F'
446300020527     c     orgfl2        andne     'S'
446400960523     C                   MOVEA     MSG(61)       K78
446500960523     C                   MOVEL     VMP(X)        ERRLNP            3
446600960523     C                   MOVEA     ERRLNP        K78(19)
446700960523     C                   MOVEA     K78           V6CMSG
446800960523     C                   SETON                                        9028
446900960523     C                   GOTO      ENDMAN
447000960523     C                   ENDIF
447100960523     C                   ENDIF
447200960523     C                   ENDDO
447300960523     C*
447400940526     C****  CODICI TASSAZIONE  ****
447500120713      /free
447600120713       //?Ricerca Codici Tassazione
447700120713         x = 1;
447800120713         FOR x by 1 to 10;
447900120713           IF  %scan ('?': vmc(x)) > *zeros;
448000120713             exsr Ric_CT;
448100120713             vmc(x) = TB09cod;
448200120713             h7riga = 18;
448300120713             h7colo = 28 + (x * 5);
448400120713             wintcts = *on;
448500120713             leavesr;
448600120713           ENDIF;
448700120713         ENDFOR;
448800120713      /end-free
448900960523     C                   DO        10            X                 3 0
449000020611     C                   Z-ADD     1             A
449100900410     C     VMC(X)        IFNE      *BLANKS
449200020207     C     VMC(X)        LOOKUP    CCT(A)                                 35
449300940530     C     *IN35         IFEQ      *OFF
449400941123     C                   MOVEL     MSG(36)       V6CMSG
449500940530     C                   SETON                                        28
449600940530     C                   EXSR      INDIC9
449700940530     C                   GOTO      ENDMAN
449800940530     C                   ENDIF
449900050303      * errore bloccante se codice tassazione non utilizzabile in gestione
450000050303      * tariffe clienti
450100050303     c                   If        uta(a) = 'N'
450200050303     c                   Eval      v6cmsg = msg(36)
450300050303     c                   Eval      *In28 = *On
450400050303     C                   ExSr      indic9
450500050303     c                   Goto      endman
450600050303     c                   EndIf
450700020207      *
450800020207      * Se tariffa FedEx devo utilizzare solo i codici tariffa
450900020207      * previsti per FedEx
451000020207     C     V1CNET        IFEQ      'F'
451100020207     C     UTF(A)        ANDNE     'S'
451200020207     C                   SETON                                        28
451300020207     C                   MOVEL     MSG(100)      V6CMSG
451400020207     C                   EXSR      INDIC9
451500020207     C                   GOTO      ENDMAN
451600020207     C                   ENDIF
451700020207      * Se tariffa non è FedEx non posso utilizzare i codici
451800020207      * tariffa previsti per FedEx
451900020207     C     V1CNET        IFNE      'F'
452000020207     C     UTF(A)        ANDEQ     'S'
452100020207     C                   SETON                                        28
452200020207     C                   MOVEL     MSG(101)      V6CMSG
452300020207     C                   EXSR      INDIC9
452400020207     C                   GOTO      ENDMAN
452500020207     C                   ENDIF
452600940530     C                   ENDIF
452700940530     C                   ENDDO
452800960522     C*
452900950118     C****  SCAGLIONE  ****
453000950118     C     VIDSGL        IFGT      0
453100950119     C* WSAP = " " --> SI TRATTA DI SCAGLIONE SENZA DECIMALI QUINDI NON
453200950118     C*                SI POSSONO INSERIRE VALORI DOPO LA VIRGOLA
453300950118     C     WSAP          ANDEQ     ' '
453400950118     C                   MOVE      VIDSGL        W0030
453500950118     C     W0030         IFGT      0
453600950118     C                   SETON                                        6528
453700950118     C                   MOVEL     MSG(46)       V6CMSG
453800950118     C                   GOTO      ENDMAN
453900950118     C                   ENDIF
454000950118     C                   ENDIF
454100070309
454200070309     c****  TARIFFE PARTICOLARI  ****
454300070312      * ricerca
454400070312     c                   eval      x = 1
454500070309     c                   for       x by 1 to 5
454600070312     c                   if        %scan('?':vftc(x)) > *zeros
454700070312     c                   eval      kcod = '1P'
454800070312     c                   call      'TRUL19R'
454900070312     c                   parm                    kcod
455000070312     c                   parm                    ordina
455100070312     c                   parm                    kkey
455200070312     c                   parm                    comando
455300070312     c                   eval      vftc(x) = kkey
455400070312     c                   eval      h7riga = 20
455500070312     c                   eval      h7colo = 30 + (x * 3)
455600070312     c                   eval      wintftc = *on
455700070312     c                   leavesr
455800070312     c                   endif
455900070312     c                   endfor
456000070312
456100070312      * controllo
456200070312     c                   eval      x = 1
456300070312     c                   for       x by 1 to 5
456400070309     c                   if        vftc(x) <> *blanks
456500070309     c                   eval      wtpar = vftc(x)
456600070412      * se tariffa particolare AC BASE o FUEL errore
456700070412      * tanto non le posso manipolare
456800070412     c                   if        wtpar = 'd' or wtpar = 'f'
456900070412     c                   eval      v6cmsg = msg(41)
457000070412     c                   eval      %subst(v6cmsg:25:1) = vftc(x)
457100070412     c                   eval      *in28 = *on
457200070412     c                   eval      h7riga = 20
457300070412     c                   eval      h7colo = 30 + (x * 3)
457400070412     c                   leavesr
457500070412     c                   endif
457600070309     c  n50ktam7         chain(n)  titpt01l
457700070309     c   50ktam7         chain(n)  tiopt01l
457800070309     c                   if        not %found or tptatb <> *blanks
457900070309     c                   eval      v6cmsg = msg(40)
458000070309     c                   eval      %subst(v6cmsg:22:1) = vftc(x)
458100070309     c                   eval      *in28 = *on
458200070312     c                   eval      h7riga = 20
458300070312     c                   eval      h7colo = 30 + (x * 3)
458400070309     c                   leavesr
458500070309     c                   endif
458600070309     c                   endif
458700070309     c                   endfor
458800950118     C*
458900941124     C****  VARIAZIONE TARIFFA CITTA'/ PROVINCIA / MINIMO / MASSIMO ***
459000900410     C     VIDVTP        IFNE      'S'
459100941124     C     VIDVTC        ANDNE     'S'
459200941124     C     VIDVMN        ANDNE     'S'
459300941124     C     VIDVMX        ANDNE     'S'
459400941123     C                   MOVEL     MSG(37)       V6CMSG
459500940530     C                   SETON                                        7628
459600900410     C                   GOTO      ENDMAN
459700900410     C                   END
459800900410     C*
459900900410     C     ENDMAN        ENDSR
460000900410     C*
460100930623     C*--- ACCENDO INDICATORI ERRORE CODICI TASSAZIONE ---------------*
460200900410     C     INDIC9        BEGSR
460300940530     C     X             COMP      1                                      66
460400940530     C     X             COMP      2                                      67
460500940530     C     X             COMP      3                                      68
460600940530     C     X             COMP      4                                      69
460700940530     C     X             COMP      5                                      70
460800940530     C     X             COMP      6                                      71
460900940530     C     X             COMP      7                                      72
461000940530     C     X             COMP      8                                      73
461100940530     C     X             COMP      9                                      74
461200940530     C     X             COMP      10                                     75
461300900410     C                   ENDSR
461400900410     C*
461500930623     C*--- ESEGUO MANIPOLAZIONE --------------------------------------*
461600900410     C     MANIPO        BEGSR
461700020327
461800020313     C                   eval      wnoagg = *off
461900020313     c                   eval      *in32 = *off
462000020312
462100960528     C                   CLEAR                   WSLNP
462200960528     C                   CLEAR                   WSCTS
462300960528     C                   CLEAR                   WSSGL
462400020313
462500960528     C* WSLNP = "S" --> RICHIESTA ESCLUSIONE DI ALMENO UNA LINEA PARTENZA
462600960528    1C     VIDIOP        IFEQ      'O'
462700960528     C     COMVMP        ANDGT     0
462800960528     C                   MOVEL     'S'           WSLNP             1
462900960528    1C                   ENDIF
463000960528     C* WSCTS = "S" --> RICHIESTA ESCLUSIONE DI ALMENO UN COD.TASSAZIONE
463100960528    1C     VIDIO         IFEQ      'O'
463200960528     C     COMVMC        ANDNE     *BLANKS
463300960528     C                   MOVEL     'S'           WSCTS             1
463400960528    1C                   ENDIF
463500960528     C* WSSGL = "S" --> RICHIESTA ESCLUSIONE DI UNO SCAGLIONE          E
463600960528    1C     VIDIO2        IFEQ      'O'
463700960528     C     VIDSGL        ANDGT     0
463800960528     C                   MOVEL     'S'           WSSGL             1
463900960528    1C                   ENDIF
464000070309      * wsftc = "S" --> richiesta sclusione di almeno una tariffa particolare
464100070309     c                   if        vidioftc = 'O' and comftc <> *blanks
464200070309     c                   eval      wsftc = 'S'
464300070309     c                   endif
464400960528     C*
464500020314      * ---- DETTAGLIO TARIFFE
464600020314 1ab c                   if        vidtad = 'SI'
464700960528     C*
464800960528     C* C I C L O    L E T T U R A
464900061011     C  n50KTAM2         SETLL     TITAD000
465000061011     c   50ktam2         setll     tiofd000
465100061011     C  n50KTAM2         READE     TITAD000                               32
465200061011     c   50ktam2         reade     tiofd000                               32
465300930623     C*
465400941124    1C     *IN32         DOWEQ     *OFF
465500930623     C* ESCLUDO RECORD ANNULLATI
465600941124    2C     TADATB        IFEQ      ' '
465700960528     C                   CLEAR                   WOKLNP
465800960528     C                   CLEAR                   WOKCTS
465900960528     C                   CLEAR                   WOKSGL
466000930624     C*
466100960528     C****  C O N T R O L L O     L E     I N C L U S I O N I  ****
466200960528     C* CONTROLLO SE LA LINEA PARTENZA E' INCLUDERE
466300960528    3C     COMVMP        IFGT      0
466400960527     C     TADLNP        LOOKUP    VMP                                    35
466500960528    4C     *IN35         IFEQ      *ON
466600960528     C                   MOVEL     'S'           WOKLNP            1
466700960528   X4C                   ELSE
466800960527     C* NON E' INCLUSA
466900960528     C  N35VIDIOP        IFEQ      'I'
467000960528     C                   GOTO      LEGTAD
467100960528     C                   ENDIF
467200960528    4C                   ENDIF
467300960528    3C                   ENDIF
467400960527     C*
467500960528     C* CONTROLLO SE IL CODICE TASSAZIONE E' DA INCLUDERE
467600960528    3C     COMVMC        IFNE      *BLANKS
467700940524     C     TADCTS        LOOKUP    VMC                                    35
467800960528    4C     *IN35         IFEQ      *ON
467900960528     C                   MOVEL     'S'           WOKCTS            1
468000960528   X4C                   ELSE
468100900410     C* NON E' INCLUSO
468200960528     C  N35VIDIO         IFEQ      'I'
468300960528     C                   GOTO      LEGTAD
468400960528     C                   ENDIF
468500960528    4C                   ENDIF
468600960528    3C                   ENDIF
468700960522     C*
468800960528     C* CONTROLLO SE LO SCAGLIONE E' DA INCLUDERE
468900960528    3C     VIDSGL        IFGT      0
469000960528    4C     TADSGL        IFEQ      VIDSGL
469100960528     C                   MOVEL     'S'           WOKSGL            1
469200960528   X4C                   ELSE
469300960528     C* NON E' INCLUSO
469400960528     C     TADSGL        IFNE      VIDSGL
469500960528     C     VIDIO2        ANDEQ     'I'
469600960528     C                   GOTO      LEGTAD
469700960528     C                   ENDIF
469800960528    4C                   ENDIF
469900960528    3C                   ENDIF
470000930624     C*
470100960528     C****  C O N T R O L L O     L E     E S C L U S I O N I  ****
470200960528     C* SE TUTTE LE CONDIZIONI SI VERIFICANO AGGIORNO IL RECORD
470300960528   3AC     WSLNP         IFEQ      'S'
470400960528     C     WOKLNP        ANDEQ     ' '
470500960528     C*
470600960528     C     WSCTS         OREQ      'S'
470700960528     C     WOKCTS        ANDEQ     ' '
470800960528     C*
470900960528     C     WSSGL         OREQ      'S'
471000960528     C     WOKSGL        ANDEQ     ' '
471100960528     C*
471200960528     C     WSLNP         OREQ      ' '
471300960528     C     WSCTS         ANDEQ     ' '
471400960528     C     WSSGL         ANDEQ     ' '
471500960528     C*
471600960528     C****  E L A B O R O    I L    R E C O R D  ****
471700020313
471800020313     c                   eval      w_taditr = taditr
471900020313     c                   eval      w_tadino = tadino
472000020313     c                   eval      w_tadmin = tadmin
472100020313     c                   eval      w_tadmax = tadmax
472200020313     c                   eval      w_tadrpv = tadrpv
472300020313
472400020313      * AUMENTO/DIMINUZUIONE CON IMPORTO IN LIRE
472500020313  4b C                   if        vidlir > *zeros
472600020313
472700020313      * VARIAZIONE TARIFFA CITTA'
472800020313  5b C                   IF        vidvtc = 'S'
472900020313  6b C                   IF        vidad = '+'
473000020313     C                   ADD       VIDLIR        w_TADITR
473100020313  6x C                   ELSE
473200020313     C                   SUB       VIDLIR        w_TADITR
473300020313      * SE MINORE NON AGGIORNO LA RIGA
473400020313  b7 c                   if        w_taditr <= *zeros
473500020313     c                   eval      wnoagg = *on
473600020313     C                   GOTO      LEGTAD
473700020313  e7 c                   endif
473800020313  6e C                   END
473900020313  5e C                   END
474000020313
474100020313      * VARIAZIONE TARIFFA PROVINCIA
474200020313  5b C                   IF        vidvtp = 'S' and
474300020313     c                             w_tadino > *zeros
474400020313  6b C                   IF        vidad = '+'
474500020313     C                   ADD       VIDLIR        w_TADINO
474600020313  6x C                   ELSE
474700020313     C                   SUB       VIDLIR        w_TADINO
474800020313      * se minore non aggiorno la riga
474900020313  7b c                   if        w_tadino <= *zeros
475000020313     c                   eval      wnoagg = *on
475100020313     C                   GOTO      LEGTAD
475200020313  7e c                   endif
475300020313  6e C                   END
475400020313  5e C                   END
475500020313
475600020313      * VARIAZIONE IMPORTO MINIMO  TARIFFA
475700020313  5b C                   IF        vidvmn = 'S' and
475800020313     c                             w_tadmin > *zeros
475900020313  6b C                   IF        vidad = '+'
476000020313     C                   ADD       VIDLIR        w_TADMIN
476100020313  6x C                   ELSE
476200020313     C                   SUB       VIDLIR        W_TADMIN
476300020313      * se minore non aggiorno la riga
476400020313  7b c                   if        w_tadmin <= *zeros
476500020313     c                   eval      wnoagg = *on
476600020313     C                   GOTO      LEGTAD
476700020313  7e c                   endif
476800020313  6e c                   endif
476900020313  5e C                   ENDIF
477000020313
477100020313      * VARIAZIONE IMPORTO MASSIMO TARIFFA
477200020313  5b C                   IF        vidvmx = 'S' and
477300020313     c                             w_tadmax > *zeros
477400020313  6b C                   IF        vidad = '+'
477500020313     C                   ADD       VIDLIR        w_TADMAX
477600020313  6x C                   ELSE
477700020313     C                   SUB       VIDLIR        w_TADMAX
477800020313      * se minore non aggiorno la riga
477900020313  7b c                   if        w_tadmax <= *zeros
478000020313     c                   eval      wnoagg = *on
478100020313     C                   GOTO      LEGTAD
478200020313  7e c                   endif
478300020313  6e C                   END
478400020313  5e C                   ENDIF
478500020313
478600020313  4e c                   endif
478700020313
478800020313      * AUMENTO/DIMINUZUIONE CON IMPORTO IN PERCENTUALE
478900020313  4b C                   if        vidper > *zeros
479000020313
479100020313      * VARIAZIONE TARIFFA CITTA'
479200020313  5b C                   IF        vidvtc ='S'
479300020313     C     w_TADITR      MULT(H)   VIDPER        COMITR
479400020313  6b C                   IF        vidad = '+'
479500020313     C                   ADD       COMITR        w_TADITR
479600020313  6x C                   ELSE
479700020313     C                   SUB       COMITR        w_TADITR
479800020313      * se minore non aggiorno la riga
479900020313  7b c                   if        w_taditr <= *zeros
480000020313     c                   eval      wnoagg = *on
480100020313     C                   GOTO      LEGTAD
480200020313  7e c                   endif
480300020313  6e C                   END
480400020313  5e C                   END
480500020313
480600020313      *  VARIAZIONE TARIFFA PROVINCIA
480700020313  5b C                   IF        vidvtp = 'S' and
480800020313     c                             w_tadino > *zeros
480900020313     C     w_TADINO      MULT(H)   VIDPER        COMINO
481000020313  6b C                   IF        vidad = '+'
481100020313     C                   ADD       COMINO        w_TADINO
481200020313  6x C                   ELSE
481300020313     C                   SUB       COMINO        w_TADINO
481400020313      * se minore non aggiorno la riga
481500020313  7b c                   if        w_tadino <= *zeros
481600020313     c                   eval      wnoagg = *on
481700020313     C                   GOTO      LEGTAD
481800020313  7e c                   endif
481900020313  6e c                   endif
482000020313  5e C                   END
482100020313
482200020313      * VARIAZIONE IMPORTO MINIMO  TARIFFA
482300020313  5b C                   IF        vidvmn = 'S' and
482400020313     c                             w_tadmin > *zeros
482500020313     C     w_TADMIN      MULT(H)   VIDPER        WMIN
482600020313  6b C                   IF        vidad = '+'
482700020313     C                   ADD       WMIN          w_TADMIN
482800020313  6x C                   ELSE
482900020313     C                   SUB       WMIN          w_TADMIN
483000020313      * se minore non aggiorno la riga
483100020313  7b c                   if        w_tadmin <= *zeros
483200020313     c                   eval      wnoagg = *on
483300020313     C                   GOTO      LEGTAD
483400020313  7e c                   endif
483500020313  6e c                   endif
483600020313  5e C                   ENDIF
483700020313
483800020313      * VARIAZIONE IMPORTO MASSIMO TARIFFA
483900020313  5b C                   IF        vidvmx = 'S' and
484000020313     c                             w_tadmax > *zeros
484100020313     C     w_TADMAX      MULT(H)   VIDPER        WMAX
484200020313  6b C                   IF        vidad = '+'
484300020313     C                   ADD       WMAX          w_TADMAX
484400020313  6x C                   ELSE
484500020313     C                   SUB       WMAX          w_TADMAX
484600020313      * se minore non aggiorno la riga
484700020313  7b c                   if        w_tadmax <= *zeros
484800020313     c                   eval      wnoagg = *on
484900020313     C                   GOTO      LEGTAD
485000020313  7e c                   endif
485100020313  6e c                   endif
485200020313  5e C                   ENDIF
485300020313
485400020313  4e c                   endif
485500020313
485600020313      * CONTROLLO CHE IL MASSIMO NON SIA INFERIORE DEL MINIMO
485700020313    3C                   IF        w_tadmax > 0 and
485800020313     C                             w_tadmax < w_tadmin
485900020313     C                   eval      wnoagg = *on
486000950120     C                   GOTO      LEGTAD
486100950120    3C                   ENDIF
486200020313
486300020313      * ARROTONDAMENTO
486400960607   3AC     VIDLIR        IFGT      0
486500960607     C     VIDPEI        ORGT      0
486600010323     C*
486700010323     C* ESEGUO GLI ARROTONDAMENTI SOLO SE VIENE RICHIESTO
486800010323     C*
486900010323   3BC     VIDALI        IFGT      0
487000010323     C*
487100900410     C                   Z-ADD     VIDALI        XARROT
487200900410     C                   MOVE      VIDADE        XDE
487300941124     C* TARIFFA CITTA'
487400941124    3C     VIDVTC        IFEQ      'S'
487500020313     C                   Z-ADD     w_TADITR      XVALUE
487600900410     C                   EXSR      XARROX
487700020313     C                   Z-ADD     XVALUE        w_TADITR
487800941124    3C                   END
487900941124     C* TARIFFA PROVINCIA
488000020313    3C                   IF        vidvtp = 'S' and w_tadino > *zeros
488100020313     C                   Z-ADD     w_TADINO      XVALUE
488200900410     C                   EXSR      XARROX
488300020313     C                   Z-ADD     XVALUE        w_TADINO
488400941124    3C                   END
488500941124     C* IMPORTO MINIMO  TARIFFA
488600020313    3C                   IF        vidvmn = 'S' and w_tadmin > *zeros
488700020313     C                   Z-ADD     w_TADMIN      XVALUE
488800941124     C                   EXSR      XARROX
488900020313     C                   Z-ADD     XVALUE        w_TADMIN
489000941124    3C                   END
489100941124     C* IMPORTO MASSIMO TARIFFA
489200020313    3C                   IF        vidvmx = 'S' and w_tadmax > *zeros
489300020313     C                   Z-ADD     w_TADMAX      XVALUE
489400941124     C                   EXSR      XARROX
489500020313     C                   Z-ADD     XVALUE        w_TADMAX
489600941124    3C                   END
489700010323     C*
489800010323  X3BC                   ELSE
489900010323     C* VERIFICO SE E' STATO RICHIESTO ARROTONDAMENTO AI DECIMALI
490000010323   3CC     VIDSAD        IFEQ      'S'
490100010323     C* TARIFFA CITTA'
490200010323    4C     VIDVTC        IFEQ      'S'
490300020313     C                   Z-ADD     w_TADITR      XVALDE           15 3
490400010323     C                   EXSR      XARROD
490500020313     C                   Z-ADD     XVALDE        w_TADITR
490600010323    4C                   END
490700010323     C* TARIFFA PROVINCIA
490800020313    4C                   IF        vidvtp = 'S' and w_tadino > *zeros
490900020313     C                   Z-ADD     w_TADINO      XVALDE
491000010323     C                   EXSR      XARROD
491100020313     C                   Z-ADD     XVALDE        w_TADINO
491200010323    4C                   END
491300010323     C* IMPORTO MINIMO  TARIFFA
491400020313    4C                   IF        vidvmn = 'S' and w_tadmin > *zeros
491500020313     C                   Z-ADD     w_TADMIN      XVALDE
491600010323     C                   EXSR      XARROD
491700020313     C                   Z-ADD     XVALDE        w_TADMIN
491800010323    4C                   END
491900010323     C* IMPORTO MASSIMO TARIFFA
492000020313    4C                   IF        vidvmx = 'S' and w_tadmax > *zeros
492100020313     C                   Z-ADD     w_TADMAX      XVALDE
492200010323     C                   EXSR      XARROD
492300020313     C                   Z-ADD     XVALDE        w_TADMAX
492400010323    4C                   END
492500010323     C*
492600010323   3CC                   END
492700010323     C*
492800010323   3DC                   ENDIF
492900010323     C*
493000960607   3AC                   ENDIF
493100020313
493200020313      * RAPPORTO PESO VOLUME
493300020315     c                   if        vidvrr > 0  and  w_tadrpv > 0
493400941124    3C     VIDRAD        IFEQ      '+'
493500020313     C                   ADD       VIDVRR        w_TADRPV
493600941124   X3C                   ELSE
493700020313     C                   SUB       VIDVRR        w_TADRPV
493800020313      * se minore non aggiorno la riga
493900020313     c                   if        w_tadrpv <= *zeros
494000020313     c                   eval      wnoagg = *on
494100020313     c                   goto      legtad
494200020315     c                   endif
494300941124    3C                   END
494400020313     c                   endif
494500050616      * annullo il rapporto peso volume
494600050617     c                   If        vidarr = 'SI' and w_tadrpv > 0
494700050617     c                   Clear                   w_tadrpv
494800050616     c                   EndIf
494900061207      * se inserimento imposto il rapporto peso volume
495000061207     c                   if        vidirr > 0
495100061207     c                   z-add     vidirr        w_tadrpv
495200061207     c                   endif
495300941124     C*
495400980219     C* VALORE POSITIVO NEI CAMPI
495500020313     C                   MLLZO     1             w_TADITR
495600020313     C                   MLLZO     1             w_TADINO
495700020313     C                   MLLZO     1             w_TADRPV
495800020313     C                   MLLZO     1             w_TADMIN
495900020313     C                   MLLZO     1             w_TADMAX
496000020312
496100020312      * controllo se i campi aggiornati superano il valore massimo
496200020313     c                   if        w_taditr > 99999999,999 or
496300020313     c                             w_tadino > 99999999,999 or
496400020313     c                             w_tadmin > 99999999,999 or
496500020313     c                             w_tadmax > 99999999,999 or
496600020313     c                             w_tadrpv > 99,9
496700020313     c                   eval      wnoagg = *on
496800020312     c                   goto      legtad
496900020312     c                   endif
497000980219     C*
497100900410     C* AGGIORNO DETTAGLIO
497200020312     c                   if        wsimula = '1'
497300020313
497400020313     c                   eval      taditr = w_taditr
497500020313     c                   eval      tadino = w_tadino
497600020313     c                   eval      tadmin = w_tadmin
497700020313     c                   eval      tadmax = w_tadmax
497800020313     c                   eval      tadrpv = w_tadrpv
497900020313
498000130124     C******             Z-ADD     dateu8        TADDTR
498100940511     C                   MOVEL     *BLANKS       TADFTR
498200061011     C  n50              UPDATE    TITAD000
498300061011     c   50              update    tiofd000
498400030714      * valorizzo il flag per la scrittura dello storico variazioni
498500030714     c                   eval      mandet = 'S'
498600030714      *
498700020312     c                   endif
498800900410     C     LEGTAD        TAG
498900960528   3AC                   ENDIF
499000941124    2C                   END
499100930623     C*
499200061011     C  n50KTAM2         READE     TITAD000                               32
499300061011     c   50ktam2         reade     tiofd000                               32
499400020312      * se errore esco dalla lettura
499500020313     c                   if        wnoagg = *on
499600020312     c                   eval      *in32 = *on
499700020312     c                   endif
499800020312
499900941124    1C                   END
500000020314
500100020314 1ae c                   endif
500200020312
500300020312      * Manipolazione tariffe particolari
500400020312     c                   if        vidtpt = 'SI'
500500020312     c                   exsr      manipo_tpt
500600020312     c                   endif
500700050616
500800050616      * Manipolazione tariffe di giacenza
500900050616     c                   if        vidtgc = 'SI'
501000050616     c                   exsr      manipo_tgc
501100050616     c                   endif
501200020312
501300020312     c                   if        wsimula = '1'
501400020312
501500930623     C*
501600900410     C* AGGIORNO DATA ULTIMA VARIAZIONE MASTER
501700061011     C  n50KTAM2         CHAIN     TNTAM000                           33
501800061011     c   50ktam2         chain     tnofm000                           33
501900990608     C*-------------------------------------------------------------------------
502000941221     C  N33              Z-ADD     DATEU8        TAMDUV
502100040913     C  n33              Z-ADD     dateu8        TAMDTR
502200990608     C*-------------------------------------------------------------------------
502300061011     C  N33
502400061011     Cann50              UPDATE    TNTAM000
502500061011     c  n33
502600061011     can 50              update    tnofm000
502700020312     c                   endif
502800930623     C                   ENDSR
502900020312
503000020312      *---------------------------------------------------------------*
503100020312      * Manipolazione tariffe particolari
503200020312      *---------------------------------------------------------------*
503300020312     c     manipo_tpt    begsr
503400020312
503500020313     c                   eval      wnoagg = *off
503600020312
503700020312      * Testata tariffe particolari
503800061011     c  n50ktam2         setll     titpt01l
503900061011     c   50ktam2         setll     tiopt01l
504000020312  b1 c                   do        *hival
504100061011     c  n50ktam2         reade     titpt01l
504200061011     c   50ktam2         reade     tiopt01l
504300020312      * fine lettura
504400061011     c                   if        %eof
504500020312     c                   leave
504600020312     c                   endif
504700020312      * escludo record annullati
504800020312     c                   if        tptatb <> *blanks
504900020312     c                   iter
505000020312     c                   endif
505100070410      * escludo AC BASE e FUEL (non si possono manipolare)
505200070410     c                   if        tptftc = 'd ' or tptftc = 'f '
505300070410     c                   iter
505400070410     c                   endif
505500070309
505600070309     c                   clear                   wokftc
505700070309
505800070309      ****  C O N T R O L L O     L E     I N C L U S I O N I  ****
505900070309      * controllo se la tariffa particolare è da includere
506000070309     c                   if        comftc <> *blanks
506100070309     c                   eval      wtptftc = tptftc
506200070309     c     wtptftc       lookup    vftc                                   35
506300070309     c                   if        *in35
506400070309     c                   eval      wokftc = 'S'
506500070309     c                   else
506600070309      * non è inlcusa
506700070309     c                   if        not *in35 and vidioftc = 'I'
506800070309     c                   iter
506900070309     c                   endif
507000070309     c                   endif
507100070309     c                   endif
507200070309      ****  C O N T R O L L O     L E     E S C L U S I O N I  ****
507300070309   1ac                   if        wsftc = 'S' and wokftc = *blanks
507400070309     c                             or wsftc = *blanks
507500070309
507600050628      * aggancio la tabella solo x controllare se la tariffa gestisce il
507700050628      * rapporto peso volume, non controllo + se gestita a valore
507800050628     c                   clear                   key
507900050628     c                   eval      cod = 'TR'
508000050628     c                   eval      key = tpttpg
508100050628     c     ktab          chain     tabel00f
508200050628     c                   if        %found (tabel00f)
508300050628     c                   movel     tbluni        dstr
508400050628     c                   endif
508500020312
508600020312      * Dettaglio tariffe particolari
508700061011     c  n50ktpd          setll     titpd01l
508800061011     c   50ktpd          setll     tiopd01l
508900020312  b2 c                   do        *hival
509000061011     c  n50ktpd          reade     titpd01l
509100061011     c   50ktpd          reade     tiopd01l
509200020312      * fine lettura
509300061011     c                   if        %eof
509400020312     c                   leave
509500020312     c                   endif
509600020312      * escludo record annullati
509700020312     c                   if        tpdatb <> *blanks
509800020312     c                   iter
509900020312     c                   endif
510000020314
510100020314      * ---- Controllo le inclusioni
510200020314     c                   clear                   wokcts
510300020314     c                   clear                   woksgl
510400020312
510500020312      * codice tassazione da includere
510600020312  b3 c                   if        comvmc <> *blanks
510700020312     c     tpdcts        lookup    vmc                                    35
510800020312  b4 c                   if        *in35
510900020312     c                   eval      wokcts = 'S'
511000020312  x4 c                   else
511100020312      * non è incluso
511200020312  b5 c                   if        vidio = 'I'
511300020312     c                   iter
511400020312  e5 c                   endif
511500020312  e4 c                   endif
511600020312  e3 c                   endif
511700020312      * scaglione da includere
511800020312  b3 c                   if        vidsgl > *zeros
511900020312  b4 c                   if        tpdsgl = vidsgl
512000020312     c                   eval      woksgl = 'S'
512100020312  x4 c                   else
512200020312      * non è incluso
512300020312  b5 c                   if        tpdsgl <> vidsgl and
512400020312     c                             vidio2 = 'I'
512500020312     c                   iter
512600020312  e5 c                   endif
512700020312  e4 c                   endif
512800020312  e3 c                   endif
512900020312      * ---- Controllo le esclusioni
513000020312  b3 c                   if        (wscts = 'S' and wokcts = *blanks) or
513100020312     c                             (wssgl = 'S' and woksgl = *blanks) or
513200020312     c                             (wscts = *blanks and wssgl = *blanks)
513300020312      * ---- Elabora tariffa particolare letta
513400020313
513500020313     c                   eval      w_tpditr = tpditr
513600020313     c                   eval      w_tpdmin = tpdmin
513700020313     c                   eval      w_tpdmax = tpdmax
513800020313
513900020312      * ---- Aumento/Diminuzione con importo
514000020312  b4 c                   if        vidlir > 0
514100020313
514200020312      * Variazione Tariffa
514300020315  b5 c                   if        vidvtc = 'S' and w_tpditr > *zeros
514400020312      * aumento
514500020312  b6 c                   if        vidad ='+'
514600020313     c                   add       vidlir        w_tpditr
514700020312  x6 c                   else
514800020312      * diminuisco
514900020327     c                   sub       vidlir        w_tpditr
515000020313      * se minore non aggiorno
515100020313  b7 c                   if        w_tpditr <= *zeros
515200020313     c                   eval      wnoagg = *on
515300020312     c                   leave
515400020313  e7 c                   endif
515500020312  e6 c                   endif
515600020312  e5 c                   endif
515700020312      * Variazione Tariffa minima
515800020313  b5 c                   if        vidvmn = 'S' and w_tpdmin > 0
515900020312      * aumento
516000020313  b6 c                   if        vidad ='+'
516100020313     c                   add       vidlir        w_tpdmin
516200020312  x6 c                   else
516300020312      * diminuisco
516400020313     c                   sub       vidlir        w_tpdmin
516500020313      * se minore non aggiorno
516600020313  b7 c                   if        w_tpdmin <= *zeros
516700020313     c                   eval      wnoagg = *on
516800020312     c                   leave
516900020313  e7 c                   endif
517000020312  e6 c                   endif
517100020312  e5 c                   endif
517200020312      * Variazione Tariffa massima
517300020313  b5 c                   if        vidvmx = 'S' and w_tpdmax > *zeros
517400020312      * aumento
517500020312  b6 c                   if        vidad ='+'
517600020313     c                   add       vidlir        w_tpdmax
517700020312  x6 c                   else
517800020312      * diminuisco
517900020313     c                   sub       vidlir        w_tpdmax
518000020313      * se minore non aggiorno
518100020313  b7 c                   if        w_tpdmax <= *zeros
518200020313     c                   eval      wnoagg = *on
518300020312     c                   leave
518400020312  e7 c                   endif
518500020312  e6 c                   endif
518600020312  e5 c                   endif
518700020312
518800020313  e4 c                   endif
518900020313
519000020312      * ---- Aumento/Diminuzione in percentuale
519100020313  4b c                   if        vidper > *zeros
519200020313
519300020312      * Variazione Tariffa
519400020315  b5 c                   if        vidvtc = 'S' and w_tpditr > *zeros
519500020313     c                   eval(h)   w_itr = w_tpditr * vidper
519600020312      * aumento
519700020312  b6 c                   if        vidad ='+'
519800020313     c                   add       w_itr         w_tpditr
519900020312  x6 c                   else
520000020312      * diminuisco
520100020313     c                   sub       w_itr         w_tpditr
520200020313      * se minore non aggiorno
520300020313  b7 c                   if        w_tpditr <= *zeros
520400020313     c                   eval      wnoagg = *on
520500020312     c                   leave
520600020312  e7 c                   endif
520700020312  e6 c                   endif
520800020312  e5 c                   endif
520900020312      * Variazione Tariffa minima
521000020313  b5 c                   if        vidvmn = 'S' and w_tpdmin > *zeros
521100020313     c                   eval(h)   w_min = w_tpdmin * vidper
521200020312      * aumento
521300020313  b6 c                   if        vidad ='+'
521400020313     c                   add       w_min         w_tpdmin
521500020313  x6 c                   else
521600020312      * diminuisco
521700020313     c                   sub       w_min         w_tpdmin
521800020313      * se minore non aggiorno
521900020313  b7 c                   if        w_tpdmin <= *zeros
522000020313     c                   eval      wnoagg = *on
522100020312     c                   leave
522200020312  e7 c                   endif
522300020312  e6 c                   endif
522400020312  e5 c                   endif
522500020312      * Variazione Tariffa massima
522600020313  b5 c                   if        vidvmx = 'S' and w_tpdmax > *zeros
522700020313     c                   eval(h)   w_max = w_tpdmax * vidper
522800020312      * aumento
522900020313  b6 c                   if        vidad ='+'
523000020313     c                   add       w_max         w_tpdmax
523100020313  x6 c                   else
523200020312      * diminuisco
523300020313     c                   sub       w_max         w_tpdmax
523400020313      * se minore non aggiorno
523500020313  b7 c                   if        w_tpdmax <= *zeros
523600020313     c                   eval      wnoagg = *on
523700020312     c                   leave
523800020312  e7 c                   endif
523900020312  e6 c                   endif
524000020312  e5 c                   endif
524100020312
524200020312  e4 c                   endif
524300020312
524400020312      * Controllo che il massimo non sia inferiore del minimo
524500020313  b4 c                   if        w_tpdmax > *zeros and w_tpdmax < tpdmin
524600020313     c                   eval      wnoagg = *on
524700020312     c                   leave
524800020312  e4 c                   endif
524900020312
525000020312      * ---- Arrotondamenti
525100020312  b4 c                   if        vidlir > *zeros or vidpei > *zeros
525200020312      * Arrotondamento
525300020312  b5 c                   if        vidali > *zeros
525400020312     c                   z-add     vidali        xarrot
525500020312     c                   move      vidade        xde
525600020312      * Tariffa
525700020327  b6 c                   if        vidvtc = 'S' and w_tpditr > *zeros
525800020313     c                   z-add     w_tpditr      xvalue
525900020312     c                   exsr      xarrox
526000020313     c                   z-add     xvalue        w_tpditr
526100020312  e6 c                   endif
526200020312      * Tariffa minima
526300020313  b6 c                   if        vidvmn = 'S' and w_tpdmin > *zeros
526400020313     c                   z-add     w_tpdmin      xvalue
526500020312     c                   exsr      xarrox
526600020313     c                   z-add     xvalue        w_tpdmin
526700020312  e6 c                   endif
526800020312      * Tariffa massima
526900020313  b6 c                   if        vidvmx = 'S' and w_tpdmax > *zeros
527000020313     c                   z-add     w_tpdmax      xvalue
527100020312     c                   exsr      xarrox
527200020313     c                   z-add     xvalue        w_tpdmax
527300020312  e6 c                   endif
527400020312  e5 c                   endif
527500020312      * Arrotondamento ai decimali
527600020312  b5 c                   if        vidsad = 'S'
527700020312      * Tariffa
527800020327  b6 c                   if        vidvtc = 'S' and w_tpditr > *zeros
527900020313     c                   z-add     w_tpditr      xvalde
528000020312     c                   exsr      xarrod
528100020313     c                   z-add     xvalde        w_tpditr
528200020312  e6 c                   endif
528300020312      * Tariffa minima
528400020313  b6 c                   if        vidvmn = 'S' and w_tpdmin > *zeros
528500020313     c                   z-add     w_tpdmin      xvalde
528600050628     c                   exsr      xarrod
528700020313     c                   z-add     xvalde        w_tpdmin
528800020312  e6 c                   endif
528900020312      * Tariffa massima
529000020313  b6 c                   if        vidvmx = 'S' and w_tpdmax > *zeros
529100020313     c                   z-add     w_tpdmax      xvalde
529200050628     c                   exsr      xarrod
529300020313     c                   z-add     xvalde        w_tpdmax
529400020312  e6 c                   endif
529500020312  e5 c                   endif
529600020312  e4 c                   endif
529700020312
529800020312      * valori positivi nei campi
529900020313     c                   mllzo     1             w_tpditr
530000020313     c                   mllzo     1             w_tpdmin
530100020313     c                   mllzo     1             w_tpdmax
530200020312
530300020312      * controllo se i campi aggiornati superano il valore massimo
530400020313     c                   if        w_tpditr > 99999999,999 or
530500020313     c                             w_tpdmin > 99999999,999 or
530600020313     c                             w_tpdmax > 99999999,999
530700020313     c                   eval      wnoagg = *on
530800020312     c                   leave
530900020312     c                   endif
531000020312
531100020312      * aggiorno dettaglio tariffe particolari
531200020312     c                   if        wsimula = '1'
531300020313
531400020313     c                   eval      tpditr = w_tpditr
531500020313     c                   eval      tpdmin = w_tpdmin
531600020313     c                   eval      tpdmax = w_tpdmax
531700020313
531800040914     c                   z-add     dateu8        tpddtr
531900020312     c                   clear                   tpdftr
532000061011     c  n50              update    titpd000
532100061011     c   50              update    tiopd000
532200020312     c                   endif
532300020312
532400020312  e3 c                   endif
532500020312
532600020312  e2 c                   enddo
532700020312
532800020313     c                   if        wnoagg = *on
532900020312     c                   leave
533000020312     c                   endif
533100020312
533200020313     c                   eval      w_tptrpv = tptrpv
533300020313
533400020312      * ---- Rapporto peso volume
533500020313  b2 c                   if        vidvrr > *zeros and §trrpv = 'S'
533600020315     c                             and w_tptrpv > 0
533700020312  b3 c                   if        vidrad = '+'
533800020313     c                   add       vidvrr        w_tptrpv
533900020312  x3 c                   else
534000020313     c                   sub       vidvrr        w_tptrpv
534100020313      * se minore non aggiorno
534200020315  b4 c                   if        w_tptrpv <= *zeros
534300020313     c                   eval      wnoagg = *on
534400020312     c                   leave
534500020315  e4 c                   endif
534600020312  e3 c                   endif
534700020312  e2 c                   endif
534800050616      * annullo i rapporto peso volume
534900050616     c                   If        vidarr = 'SI' and §trrpv = 'S'
535000050616     c                             and w_tptrpv > *Zeros
535100050616     c                   Clear                   w_tptrpv
535200050616     c                   EndIf
535300061207      * se inserimento imposto il rapporto peso volume
535400061207     c                   if        vidirr > 0 and §trrpv = 'S'
535500061207     c                   z-add     vidirr        w_tptrpv
535600061207     c                   endif
535700020312
535800020313     c                   mllzo     1             w_tptrpv
535900020312      * controllo se i campi aggiornati superano il valore massimo
536000020313     c                   if        w_tptrpv > 99,9
536100020313     c                   eval      wnoagg = *on
536200020312     c                   leave
536300020312     c                   endif
536400020312
536500020312      * aggiorno testata tariffe particolari
536600020312     c                   if        wsimula = '1'
536700020313
536800020313     c                   eval      tptrpv = w_tptrpv
536900020313
537000040914     c                   z-add     dateu8        tptdtr
537100020312     c                   clear                   tptftr
537200020312     c                   z-add     dateu8        tptduv
537300061011     c  n50              update    titpt000
537400061011     c   50              update    tiopt000
537500030714      * verifico se devo caricare la schiera per la scrittura dello storico
537600030714      * variazioni
537700030714     c                   movel(p)  tptftc        w_manftc
537800030714     c                   move      'M'           W_manftc
537900030714     c                   z-add     1             WW
538000130124     c     w_manftc      lookup    manftc                                 35
538100030714     c                   if        not %found
538200130124     c     '   '         lookup    manftc(ww)                             35
538300030714     c                   if        %found
538400030714     c                   movea     w_manftc      manftc(ww)
538500030714     c                   endif
538600030714     c                   endif
538700030714      *
538800020312     c                   endif
538900020312
539000070309   1ac                   endif
539100020312  e1 c                   enddo
539200020312
539300020312     c                   endsr
539400050616
539500050616      *---------------------------------------------------------------*
539600050616      * Manipolazione tariffe di giacenza
539700050616      *---------------------------------------------------------------*
539800050616     c     manipo_tgc    begsr
539900050616
540000050616     c                   eval      wnoagg = *off
540100050616
540200050616      * Tariffe di giacenza
540300050913     c  n50ktam2         Chain     Titgc01l
540400050913     c   50ktam2         Chain     Tiogc01l
540500050913if  1c                   If        %found and tgcatb = *Blanks
540600050616      * ---- Elabora tariffa di giacenza letta
540700050616     c                   eval      w_tgcsgv = tgcsgv
540800050616     c                   eval      w_tgcsgr = tgcsgr
540900050616     c                   eval      w_tgcsgp = tgcsgp
541000050616     c                   eval      w_tgcsgd = tgcsgd
541100050616     c                   eval      w_tgcst1 = tgcst1
541200050616     c                   eval      w_tgcst2 = tgcst2
541300050616     c                   eval      w_tgcst3 = tgcst3
541400050616     c                   eval      w_tgcstm = tgcstm
541500050616     c                   eval      w_tgcrpv = tgcrpv
541600050616
541700050616      * ---- Aumento/Diminuzione con importo
541800050617if  2c                   if        vidlir > 0
541900050616
542000050617      * Variazione spese varie giacenza
542100050617if  3c                   if        w_tgcsgv > *Zeros
542200050616      * aumento
542300050617     c                   if        vidad ='+'
542400050617     c                   add       vidlir        w_tgcsgv
542500050617     c                   else
542600050616      * diminuisco
542700050617     c                   sub       vidlir        w_tgcsgv
542800050616      * se minore non aggiorno
542900050617     c                   if        w_tgcsgv <= *zeros
543000050616     c                   eval      wnoagg = *on
543100050617     c                   leavesr
543200050617     c                   endif
543300050617     c                   endif
543400050617    3c                   endif
543500050617      * Variazione spese riconsegna città
543600050617if  3c                   if        w_tgcsgr > *Zeros
543700050616      * aumento
543800050617     c                   if        vidad ='+'
543900050617     c                   add       vidlir        w_tgcsgr
544000050617     c                   else
544100050616      * diminuisco
544200050617     c                   sub       vidlir        w_tgcsgr
544300050616      * se minore non aggiorno
544400050617     c                   if        w_tgcsgr <= *zeros
544500050616     c                   eval      wnoagg = *on
544600050617     c                   leavesr
544700050617     c                   endif
544800050617     c                   endif
544900050617    3c                   endif
545000050617      * Variazione spese riconsegna provincia
545100050617if  3c                   if        w_tgcsgp > *Zeros
545200050616      * aumento
545300050617     c                   if        vidad ='+'
545400050617     c                   add       vidlir        w_tgcsgp
545500050617     c                   else
545600050616      * diminuisco
545700050617     c                   sub       vidlir        w_tgcsgp
545800050616      * se minore non aggiorno
545900050617     c                   if        w_tgcsgp <= *zeros
546000050616     c                   eval      wnoagg = *on
546100050617     c                   leavesr
546200050617     c                   endif
546300050617     c                   endif
546400050617    3c                   endif
546500050617      * Variazione spese dossier giacenza
546600050617if  3c                   if        w_tgcsgd > *Zeros
546700050617      * aumento
546800050617     c                   if        vidad ='+'
546900050617     c                   add       vidlir        w_tgcsgd
547000050617     c                   else
547100050617      * diminuisco
547200050617     c                   sub       vidlir        w_tgcsgd
547300050617      * se minore non aggiorno
547400050617     c                   if        w_tgcsgd <= *zeros
547500050617     c                   eval      wnoagg = *on
547600050617     c                   leavesr
547700050617     c                   endif
547800050617     c                   endif
547900050617    3c                   endif
548000050617      * Variazione 1° scaglione tariffa sosta
548100050617if  3c                   if        w_tgcst1 > *Zeros
548200050617      * aumento
548300050617     c                   if        vidad ='+'
548400050617     c                   add       vidlir        w_tgcst1
548500050617     c                   else
548600050617      * diminuisco
548700050617     c                   sub       vidlir        w_tgcst1
548800050617      * se minore non aggiorno
548900050617     c                   if        w_tgcst1 <= *zeros
549000050617     c                   eval      wnoagg = *on
549100050617     c                   leavesr
549200050617     c                   endif
549300050617     c                   endif
549400050617    3c                   endif
549500050617      * Variazione 2° scaglione tariffa sosta
549600050617if  3c                   if        w_tgcst2 > *Zeros
549700050617      * aumento
549800050617     c                   if        vidad ='+'
549900050617     c                   add       vidlir        w_tgcst2
550000050617     c                   else
550100050617      * diminuisco
550200050617     c                   sub       vidlir        w_tgcst2
550300050617      * se minore non aggiorno
550400050617     c                   if        w_tgcst2 <= *zeros
550500050617     c                   eval      wnoagg = *on
550600050617     c                   leavesr
550700050617     c                   endif
550800050617     c                   endif
550900050617    3c                   endif
551000050617      * Variazione 3° scaglione tariffa sosta
551100050617if  3c                   if        w_tgcst3 > *Zeros
551200050617      * aumento
551300050617     c                   if        vidad ='+'
551400050617     c                   add       vidlir        w_tgcst3
551500050617     c                   else
551600050617      * diminuisco
551700050617     c                   sub       vidlir        w_tgcst3
551800050617      * se minore non aggiorno
551900050617     c                   if        w_tgcst3 <= *zeros
552000050617     c                   eval      wnoagg = *on
552100050617     c                   leavesr
552200050617     c                   endif
552300050617     c                   endif
552400050617    3c                   endif
552500050617      * Variazione tariffa minima sosta
552600050617if  3c                   if        w_tgcstm > *Zeros
552700050617      * aumento
552800050617     c                   if        vidad ='+'
552900050617     c                   add       vidlir        w_tgcstm
553000050617     c                   else
553100050617      * diminuisco
553200050617     c                   sub       vidlir        w_tgcstm
553300050617      * se minore non aggiorno
553400050617     c                   if        w_tgcstm <= *zeros
553500050617     c                   eval      wnoagg = *on
553600050617     c                   leavesr
553700050617     c                   endif
553800050617     c                   endif
553900050617    3c                   endif
554000050616
554100050617    2c                   endif
554200050616
554300050617      * ---- Aumento/Diminuzione in percentuale
554400050617if  2c                   if        vidper > *zeros
554500050617
554600050617      * Variazione spese varie giacenza
554700050617if  3c                   if        w_tgcsgv > *Zeros
554800050617     c                   eval(h)   w_sgv = w_tgcsgv * vidper
554900050617      * aumento
555000050617     c                   if        vidad ='+'
555100050617     c                   add       w_sgv         w_tgcsgv
555200050617     c                   else
555300050617      * diminuisco
555400050617     c                   sub       w_sgv         w_tgcsgv
555500050617      * se minore non aggiorno
555600050617     c                   if        w_tgcsgv <= *zeros
555700050617     c                   eval      wnoagg = *on
555800050617     c                   leavesr
555900050617     c                   endif
556000050617     c                   endif
556100050617    3c                   endif
556200050617      * Variazione spese riconsegna città
556300050617if  3c                   if        w_tgcsgr > *Zeros
556400050617     c                   eval(h)   w_sgr = w_tgcsgr * vidper
556500050617      * aumento
556600050617     c                   if        vidad ='+'
556700050617     c                   add       w_sgr         w_tgcsgr
556800050617     c                   else
556900050617      * diminuisco
557000050617     c                   sub       w_sgr         w_tgcsgr
557100050617      * se minore non aggiorno
557200050617     c                   if        w_tgcsgr <= *zeros
557300050617     c                   eval      wnoagg = *on
557400050617     c                   leavesr
557500050617     c                   endif
557600050617     c                   endif
557700050617    3c                   endif
557800050617      * Variazione spese riconsegna provincia
557900050617if  3c                   if        w_tgcsgp > *Zeros
558000050617     c                   eval(h)   w_sgp = w_tgcsgp * vidper
558100050617      * aumento
558200050617     c                   if        vidad ='+'
558300050617     c                   add       w_sgp         w_tgcsgp
558400050617     c                   else
558500050617      * diminuisco
558600050617     c                   sub       w_sgp         w_tgcsgp
558700050617      * se minore non aggiorno
558800050617     c                   if        w_tgcsgp <= *zeros
558900050617     c                   eval      wnoagg = *on
559000050617     c                   leavesr
559100050617     c                   endif
559200050617     c                   endif
559300050617    3c                   endif
559400050617      * Variazione spese dossier giacenza
559500050617if  3c                   if        w_tgcsgd > *Zeros
559600050617     c                   eval(h)   w_sgd = w_tgcsgd * vidper
559700050617      * aumento
559800050617     c                   if        vidad ='+'
559900050617     c                   add       w_sgd         w_tgcsgd
560000050617     c                   else
560100050617      * diminuisco
560200050617     c                   sub       w_sgd         w_tgcsgd
560300050617      * se minore non aggiorno
560400050617     c                   if        w_tgcsgd <= *zeros
560500050617     c                   eval      wnoagg = *on
560600050617     c                   leavesr
560700050617     c                   endif
560800050617     c                   endif
560900050617    3c                   endif
561000050617      * Variazione 1° scaglione tariffa sosta
561100050617if  3c                   if        w_tgcst1 > *Zeros
561200050617     c                   eval(h)   w_st1 = w_tgcst1 * vidper
561300050617      * aumento
561400050617     c                   if        vidad ='+'
561500050617     c                   add       w_st1         w_tgcst1
561600050617     c                   else
561700050617      * diminuisco
561800050617     c                   sub       w_st1         w_tgcst1
561900050617      * se minore non aggiorno
562000050617     c                   if        w_tgcst1 <= *zeros
562100050617     c                   eval      wnoagg = *on
562200050617     c                   leavesr
562300050617     c                   endif
562400050617     c                   endif
562500050617    3c                   endif
562600050617      * Variazione 2° scaglione tariffa sosta
562700050617if  3c                   if        w_tgcst2 > *Zeros
562800050617     c                   eval(h)   w_st2 = w_tgcst2 * vidper
562900050617      * aumento
563000050617     c                   if        vidad ='+'
563100050617     c                   add       w_st2         w_tgcst2
563200050617     c                   else
563300050617      * diminuisco
563400050617     c                   sub       w_st2         w_tgcst2
563500050617      * se minore non aggiorno
563600050617     c                   if        w_tgcst2 <= *zeros
563700050617     c                   eval      wnoagg = *on
563800050617     c                   leavesr
563900050617     c                   endif
564000050617     c                   endif
564100050617    3c                   endif
564200050617      * Variazione 3° scaglione tariffa sosta
564300050617if  3c                   if        w_tgcst3 > *Zeros
564400050617     c                   eval(h)   w_st3 = w_tgcst3 * vidper
564500050617      * aumento
564600050617     c                   if        vidad ='+'
564700050617     c                   add       w_st3         w_tgcst3
564800050617     c                   else
564900050617      * diminuisco
565000050617     c                   sub       w_st3         w_tgcst3
565100050617      * se minore non aggiorno
565200050617     c                   if        w_tgcst3 <= *zeros
565300050617     c                   eval      wnoagg = *on
565400050617     c                   leavesr
565500050617     c                   endif
565600050617     c                   endif
565700050617    3c                   endif
565800050617      * Variazione tariffa mimina sosta
565900050617if  3c                   if        w_tgcstm > *Zeros
566000050617     c                   eval(h)   w_stm = w_tgcstm * vidper
566100050617      * aumento
566200050617     c                   if        vidad ='+'
566300050617     c                   add       w_stm         w_tgcstm
566400050617     c                   else
566500050617      * diminuisco
566600050617     c                   sub       w_stm         w_tgcstm
566700050617      * se minore non aggiorno
566800050617     c                   if        w_tgcstm <= *zeros
566900050617     c                   eval      wnoagg = *on
567000050617     c                   leavesr
567100050617     c                   endif
567200050617     c                   endif
567300050617    3c                   endif
567400050617
567500050617    2c                   endif
567600050617
567700050617      * ---- Arrotondamenti
567800050617if  2c                   if        vidlir > *zeros or vidpei > *zeros
567900050617      * Arrotondamento
568000050617if  3c                   if        vidali > *zeros
568100050617     c                   z-add     vidali        xarrot
568200050617     c                   move      vidade        xde
568300050617      * Spese varie giacenza
568400050617if  4c                   if        w_tgcsgv > *zeros
568500050617     c                   z-add     w_tgcsgv      xvalue
568600050617     c                   exsr      xarrox
568700050617     c                   z-add     xvalue        w_tgcsgv
568800050617    4c                   endif
568900050617      * Spese riconsegna città
569000050617if  4c                   if        w_tgcsgr > *zeros
569100050617     c                   z-add     w_tgcsgr      xvalue
569200050617     c                   exsr      xarrox
569300050617     c                   z-add     xvalue        w_tgcsgr
569400050617    4c                   endif
569500050617      * Spese riconsegna provincia
569600050617if  4c                   if        w_tgcsgp > *zeros
569700050617     c                   z-add     w_tgcsgp      xvalue
569800050617     c                   exsr      xarrox
569900050617     c                   z-add     xvalue        w_tgcsgp
570000050617    4c                   endif
570100050617      * Spese dossier giacenza
570200050617if  4c                   if        w_tgcsgd > *zeros
570300050617     c                   z-add     w_tgcsgd      xvalue
570400050617     c                   exsr      xarrox
570500050617     c                   z-add     xvalue        w_tgcsgd
570600050617    4c                   endif
570700050617      * 1° scaglione tariffa sosta
570800050617if  4c                   if        w_tgcst1 > *zeros
570900050617     c                   z-add     w_tgcst1      xvalue
571000050617     c                   exsr      xarrox
571100050617     c                   z-add     xvalue        w_tgcst1
571200050617    4c                   endif
571300050617      * 2° scaglione tariffa sosta
571400050617if  4c                   if        w_tgcst2 > *zeros
571500050617     c                   z-add     w_tgcst2      xvalue
571600050617     c                   exsr      xarrox
571700050617     c                   z-add     xvalue        w_tgcst2
571800050617    4c                   endif
571900050617      * 3° scaglione tariffa sosta
572000050617if  4c                   if        w_tgcst3 > *zeros
572100050617     c                   z-add     w_tgcst3      xvalue
572200050617     c                   exsr      xarrox
572300050617     c                   z-add     xvalue        w_tgcst3
572400050617    4c                   endif
572500050617      * Tariffa minima sosta
572600050617if  4c                   if        w_tgcstm > *zeros
572700050617     c                   z-add     w_tgcstm      xvalue
572800050617     c                   exsr      xarrox
572900050617     c                   z-add     xvalue        w_tgcstm
573000050617    4c                   endif
573100050628    3c                   endif
573200050617      * Arrotondamento ai decimali
573300050628if  3c                   if        vidsad = 'S'
573400050617      * Spese varie giacenza
573500050628if  4c                   if        w_tgcsgv > *zeros
573600050617     c                   z-add     w_tgcsgv      xvalde
573700050617     c                   exsr      xarrod
573800050617     c                   z-add     xvalde        w_tgcsgv
573900050628    4c                   endif
574000050617      * Spese riconsegna città
574100050628if  4c                   if        w_tgcsgr > *zeros
574200050617     c                   z-add     w_tgcsgr      xvalde
574300050617     c                   exsr      xarrod
574400050617     c                   z-add     xvalde        w_tgcsgr
574500050628    4c                   endif
574600050617      * Spese riconsegna provincia
574700050628if  4c                   if        w_tgcsgp > *zeros
574800050617     c                   z-add     w_tgcsgp      xvalde
574900050617     c                   exsr      xarrod
575000050617     c                   z-add     xvalde        w_tgcsgp
575100050628    4c                   endif
575200050617      * Spese dossier giacenza
575300050628if  4c                   if        w_tgcsgd > *zeros
575400050617     c                   z-add     w_tgcsgd      xvalde
575500050617     c                   exsr      xarrod
575600050617     c                   z-add     xvalde        w_tgcsgd
575700050628    4c                   endif
575800050617      * 1° scaglione tariffa sosta
575900050628if  4c                   if        w_tgcst1 > *zeros
576000050617     c                   z-add     w_tgcst1      xvalde
576100050617     c                   exsr      xarrod
576200050617     c                   z-add     xvalde        w_tgcst1
576300050628    4c                   endif
576400050617      * 2° scaglione tariffa sosta
576500050628if  4c                   if        w_tgcst2 > *zeros
576600050617     c                   z-add     w_tgcst2      xvalde
576700050617     c                   exsr      xarrod
576800050617     c                   z-add     xvalde        w_tgcst2
576900050628    4c                   endif
577000050617      * 3° scaglione tariffa sosta
577100050628if  4c                   if        w_tgcst3 > *zeros
577200050617     c                   z-add     w_tgcst3      xvalde
577300050617     c                   exsr      xarrod
577400050617     c                   z-add     xvalde        w_tgcst3
577500050628    4c                   endif
577600050617      * Tariffa minima sosta
577700050628if  4c                   if        w_tgcstm > *zeros
577800050617     c                   z-add     w_tgcstm      xvalde
577900050617     c                   exsr      xarrod
578000050617     c                   z-add     xvalde        w_tgcstm
578100050628    4c                   endif
578200050628    3c                   endif
578300050617    2c                   endif
578400050617
578500050617      * valori positivi nei campi
578600050617     c                   mllzo     1             w_tgcsgv
578700050617     c                   mllzo     1             w_tgcsgr
578800050617     c                   mllzo     1             w_tgcsgp
578900050617     c                   mllzo     1             w_tgcsgd
579000050617     c                   mllzo     1             w_tgcst1
579100050617     c                   mllzo     1             w_tgcst2
579200050617     c                   mllzo     1             w_tgcst3
579300050617     c                   mllzo     1             w_tgcstm
579400050617
579500050617      * controllo se i campi aggiornati superano il valore massimo
579600050617if  2c                   if        w_tgcsgv > 99999999,999 or
579700050617     c                             w_tgcsgr > 99999999,999 or
579800050617     c                             w_tgcsgp > 99999999,999 or
579900050617     c                             w_tgcsgd > 99999999,999 or
580000050617     c                             w_tgcst1 > 99999999,999 or
580100050617     c                             w_tgcst2 > 99999999,999 or
580200050617     c                             w_tgcst3 > 99999999,999 or
580300050617     c                             w_tgcstm > 99999999,999
580400050617     c                   eval      wnoagg = *on
580500050617     c                   leavesr
580600050617    2c                   endif
580700050617
580800050617      * ---- Rapporto peso volume
580900050617     c                   eval      w_tgcrpv = tgcrpv
581000050617
581100050617if  2c                   if        vidvrr > *zeros and w_tgcrpv > *Zeros
581200050617     c                   if        vidrad = '+'
581300050617     c                   add       vidvrr        w_tgcrpv
581400050617     c                   else
581500050617     c                   sub       vidvrr        w_tgcrpv
581600050617      * se minore non aggiorno
581700050617     c                   if        w_tgcrpv <= *zeros
581800050617     c                   eval      wnoagg = *on
581900050617     c                   leavesr
582000050617     c                   endif
582100050617     c                   endif
582200050617    2c                   endif
582300050617      * annullo i rapporto peso volume
582400050617if  2c                   If        vidarr = 'SI' and w_tgcrpv > *Zeros
582500050617     c                   Clear                   w_tgcrpv
582600050617    2c                   EndIf
582700061207      * se inserimento imposto il rapporto peso volume
582800061207     c                   if        vidirr > 0
582900061207     c                   z-add     vidirr        w_tgcrpv
583000061207     c                   endif
583100050617
583200050617     c                   mllzo     1             w_tgcrpv
583300050617      * controllo se i campi aggiornati superano il valore massimo
583400050617if  2c                   if        w_tgcrpv > 99,9
583500050617     c                   eval      wnoagg = *on
583600050617     c                   leavesr
583700050617    2c                   endif
583800050617
583900050617      * aggiorno le tariffe di giacenza
584000050617if  2c                   if        wsimula = '1'
584100050617
584200050617     c                   eval      tgcsgv = w_tgcsgv
584300050617     c                   eval      tgcsgr = w_tgcsgr
584400050617     c                   eval      tgcsgp = w_tgcsgp
584500050617     c                   eval      tgcsgd = w_tgcsgd
584600050617     c                   eval      tgcst1 = w_tgcst1
584700050617     c                   eval      tgcst2 = w_tgcst2
584800050617     c                   eval      tgcst3 = w_tgcst3
584900050617     c                   eval      tgcstm = w_tgcstm
585000050627     c                   eval      tgcrpv = w_tgcrpv
585100050617
585200050617     c                   z-add     dateu8        tgcdtr
585300050617     c                   clear                   tgcftr
585400050913     c  n50              update    titgc000
585500050913     c   50              update    tiogc000
585600050617      * valorizzo il flag per la scrittura dello storico variazioni
585700050617     c                   eval      mangia = 'S'
585800050617    2c                   endif
585900050616
586000050617    1c                   endif
586100050616
586200050616     c                   endsr
586300900410     C*
586400930623     C*--- ARROTONDAMENTO VARIABILI ----------------------------------*
586500900410     C     XARROX        BEGSR
586600900410     C* XDE   : "D" DIFETTO   "E" ECCESSO
586700900410     C* XVALUE: VALORE DA ARROTONDARE
586800900410     C* XARROT: ARROTONDAMENTO
586900900410     C     *LIKE         DEFINE    XVALUE        XCOM02
587000900410     C     *LIKE         DEFINE    XVALUE        XARROT
587100900410     C                   MOVE      XDE           XDE               1
587200900410     C     XARROT        IFGT      0
587300900410     C     XVALUE        DIV       XARROT        XCOM01
587400900410     C                   MVR                     XCOM02
587500900410     C     XDE           IFEQ      'E'
587600900410     C     XCOM02        IFGT      0
587700950119     C                   ADD       1             XCOM01           15 0
587800900410     C                   END
587900900410     C                   END
588000950118     C     XCOM01        MULT      XARROT        XVALUE           15 3
588100900410     C                   END
588200900410     C                   ENDSR
588300930624     C*
588400010323     C*--- ARROTONDAMENTO VARIABILI AL DECIMALE-----------------------*
588500010323     C     XARROD        BEGSR
588600010323     C* VIDDEC: NUMERO DECIMALI
588700010323     C* XVALDE: VALORE DA ARROTONDARE AI DECIMALI
588800010323     C*
588900010323     C* 0 DECIMALI
589000010323     C     VIDDEC        IFEQ      0
589100010323     C                   Z-ADD(H)  XVALDE        CAMP0D           15 0
589200010323     C                   Z-ADD     CAMP0D        XVALDE
589300010323     C                   ENDIF
589400010323     C* 1 DECIMALE
589500010323     C     VIDDEC        IFEQ      1
589600010323     C                   Z-ADD(H)  XVALDE        CAMP1D           15 1
589700010323     C                   Z-ADD     CAMP1D        XVALDE
589800010323     C                   ENDIF
589900010323     C* 2 DECIMALI
590000010323     C     VIDDEC        IFEQ      2
590100010323     C                   Z-ADD(H)  XVALDE        CAMP2D           15 2
590200010323     C                   Z-ADD     CAMP2D        XVALDE
590300010323     C                   ENDIF
590400010323     C*
590500010323     C                   ENDSR
590600010323     C*
590700960523     C*--- CONTROLLO CAMPI VIDEO DUPLICA DETTAGLIO TARIFFA -----------*
590800960523     C     CTRD08        BEGSR
590900990817     C* CONTROLLI CHE VENGONO EFFETTUATI OLTRE ALLA CORRETTEZZA DEI DATI :
591000990817     C* A) SE SONO VALORIZZATE SOLO LE LINEE DI PARTENZE E NON  I CODICI
591100990817     C*    TASSAZIONE SI COPIA TUTTO SENZA FARE CONTROLLI
591200990817     C* B) SE INSERITI ENTRAMBI I CODICI TASSAZIONE :
591300990817     C*    1) DEVONO ESSERE ENTRAMBI ITALIA O ESTERO
591400990817     C*    2) NON SI COPIA IL DETTAGLIO CON INSERITO IL CAP ED EMETTERE UN
591500990817     C*       MESSAGGIO DI ERRORE FORZABIOLE LI
591600960523     C*
591700960523     C****  EFFETTUARE ALMENO UNA SELEZIONE  ****
591800960523    1C     V8CLNN        IFEQ      0
591900960523     C     V8CCTN        ANDEQ     *BLANKS
592000960523     C     V8CLNP        ANDEQ     0
592100960523     C     V8CCTS        ANDEQ     *BLANKS
592200960523     C                   MOVEL     MSG(37)       V8CMSG
592300960523     C                   SETON                                        7128
592400960523     C                   GOTO      ENDC08
592500960523    1C                   ENDIF
592600960523     C*
592700960527     C****  RICERCA CODICI TASSAZIONE  ****
592800960527    1C     V8CCTN        IFEQ      '?'
592900120713     c                   exsr      Ric_CT
593000120713     C                   MOVEL     TB09cod       V8CCTN
593100960527     C                   SETON                                        90
593200960527     C                   GOTO      ENDC08
593300960527    1C                   ENDIF
593400960527     C*
593500960527    1C     V8CCTS        IFEQ      '?'
593600120713     c                   exsr      Ric_CT
593700120713     c                   eval      TRTB09DS = kpjbu
593800120713     C                   MOVEL     TB09cod       V8CCTS
593900960527     C                   SETON                                        90
594000960527     C                   GOTO      ENDC08
594100960527    1C                   ENDIF
594200960527     C*
594300960523     C****  LINEA PARTENZA  ****
594400960523     C* SE IMMESSA LA LINEA PARTENZA DA CREARE OCCORRE IMMETTERE ANCHE
594500960523     C*   LA LINEA PARTENZA DA CUI COPIARE E VICEVERSA
594600960523    1C     V8CLNN        IFGT      0
594700960523     C     V8CLNP        ANDEQ     0
594800960524     C                   MOVEL     MSG(65)       V8CMSG
594900960523     C                   SETON                                        7328
595000960523     C                   GOTO      ENDC08
595100960523    1C                   ENDIF
595200960523     C*
595300960523    1C     V8CLNN        IFEQ      0
595400960523     C     V8CLNP        ANDGT     0
595500960524     C                   MOVEL     MSG(65)       V8CMSG
595600960523     C                   SETON                                        7128
595700960523     C                   GOTO      ENDC08
595800960523    1C                   ENDIF
595900960524     C*
596000960524     C* SE INSERITE SOLO LE LINEE PARTENZA DEVONO ESSERE DIVERSE
596100960524    1C     V8CLNN        IFGT      0
596200960524     C     V8CCTN        ANDEQ     *BLANKS
596300960524     C     V8CCTS        ANDEQ     *BLANKS
596400960524     C     V8CLNN        ANDEQ     V8CLNP
596500960524     C                   MOVEL     MSG(63)       V8CMSG
596600960524     C                   SETON                                        7128
596700960524     C                   GOTO      ENDC08
596800960524    1C                   ENDIF
596900960523     C*
597000960523     C****  CODICE TASSAZIONE  ****
597100960524     C* SE IMMESSO COD.TASSAZ. OCCORRE IMMETTERE ANCHE LINEA PARTENZA
597200960524    1C     V8CCTN        IFNE      *BLANKS
597300960524     C     V8CLNN        ANDEQ     0
597400960524     C                   MOVEL     MSG(65)       V8CMSG
597500960524     C                   SETON                                        7128
597600960524     C                   GOTO      ENDC08
597700960524    1C                   ENDIF
597800960524     C*
597900960524    1C     V8CCTS        IFNE      *BLANKS
598000960524     C     V8CLNP        ANDEQ     0
598100960524     C                   MOVEL     MSG(65)       V8CMSG
598200960524     C                   SETON                                        7328
598300960524     C                   GOTO      ENDC08
598400960524    1C                   ENDIF
598500960523     C*
598600960524     C* SE IMMESSO IL COD.TASSAZIONE DA CREARE OCCORRE IMMETTERE ANCHE
598700960524     C*   IL COD.TASSAZIONE DA CUI COPIARE E VICEVERSA
598800960524    1C     V8CCTS        IFNE      *BLANKS
598900960524     C     V8CCTN        ANDEQ     *BLANKS
599000960524     C                   MOVEL     MSG(66)       V8CMSG
599100960527     C                   SETON                                        7228
599200960524     C                   GOTO      ENDC08
599300960524    1C                   ENDIF
599400960524     C*
599500960524    1C     V8CCTN        IFNE      *BLANKS
599600960524     C     V8CCTS        ANDEQ     *BLANKS
599700960524     C                   MOVEL     MSG(66)       V8CMSG
599800960527     C                   SETON                                        7428
599900960524     C                   GOTO      ENDC08
600000960524    1C                   ENDIF
600100960524     C*
600200960524     C* SE INSERITI LINEE PARTENZA E COD.TASSAZ. DEVONO ESSERE DIVERSI
600300960524    1C     V8CLNN        IFGT      0
600400960524     C     V8CCTN        ANDNE     *BLANKS
600500960524     C     V8CLNN        ANDEQ     V8CLNP
600600960524     C     V8CCTN        ANDEQ     V8CCTS
600700960524     C                   MOVEL     MSG(64)       V8CMSG
600800960524     C                   SETON                                        7128
600900960524     C                   GOTO      ENDC08
601000960524    1C                   ENDIF
601100960524     C*
601200960523     C****  LINEA PARTENZA DA CREARE  ****
601300051019    1C     V8CLNN        IFne      0
601400020312     c                   clear                   og143
601500960523     C     V8CLNN        CHAIN     AZORG                              34
601600960523    2C     *IN34         IFEQ      *ON
601700960523     C     ORGFVA        ORNE      ' '
601800960523     C     ORGFAG        ORNE      'A'
601900960523     C     ORGFAG        ANDNE     'F'
602000020527     c     orgfl2        andne     'S'
602100960523     C                   MOVEL     MSG(22)       V8CMSG
602200960523     C                   SETON                                        7128
602300960523     C                   GOTO      ENDC08
602400960523    2C                   ENDIF
602500020312
602600020312     c                   eval      og143 = orgde3
602700020312
602800990817     C* VERIFICO SE LA LINEA DI PARTENZA E' CONGRUENTE CON  TIPO TARIFFA ITALIA
602900990817     C*
603000020312   2 c                   if        (§ogntw ='EEX' or §ogntw = 'EUP' or
603100020312     c                              §ogntw = 'FED') and v1cfie = 'I' and
603200020312     c                              v1cnet = *blanks
603300990817     C                   MOVEL     MSG(45)       V8CMSG
603400990817     C                   SETON                                        7128
603500990817     C                   GOTO      ENDC08
603600990817    2C                   ENDIF
603700990817     C*
603800960523    1C                   ENDIF
603900960523     C*
604000960523     C****  CODICE TASSAZIONE DA CREARE  ****
604100960523    1C     V8CCTN        IFNE      *BLANKS
604200960523     C                   Z-ADD     1             X
604300960523     C     V8CCTN        LOOKUP    CCT(X)                                 35
604400960523    2C     *IN35         IFEQ      *OFF
604500960523     C                   MOVEL     MSG(23)       V8CMSG
604600960523     C                   SETON                                        7228
604700960523     C                   GOTO      ENDC08
604800960523   X2C                   ELSE
604900050303      * errore bloccante se codice tassazione non utilizzabile in gestione
605000050303      * tariffe clienti
605100050303     c                   If        uta(x) = 'N'
605200050303     c                   Eval      v8cmsg = msg(36)
605300050303     c                   Eval      *In72 = *On
605400050303     c                   Eval      *In28 = *On
605500050303     c                   Goto      endc08
605600050303     c                   EndIf
605700020206      *
605800020206      * Se tariffa FedEx devo utilizzare solo i codici tariffa
605900020206      * previsti per FedEx
606000020206    3C     V1CNET        IFEQ      'F'
606100020214     C     UTF(X)        ANDNE     'S'
606200020206     C                   SETON                                        7228
606300020206     C                   MOVEL     MSG(100)      V8CMSG
606400020206     C                   GOTO      ENDC08
606500020206    3C                   ENDIF
606600020206      * Se tariffa non è FedEx non posso utilizzare i codici
606700020206      * tariffa previsti per FedEx
606800020206    3C     V1CNET        IFNE      'F'
606900020214     C     UTF(X)        ANDEQ     'S'
607000020206     C                   SETON                                        7228
607100020206     C                   MOVEL     MSG(101)      V8CMSG
607200020206     C                   GOTO      ENDC08
607300020206    3C                   ENDIF
607400020206      *
607500990817     C* VERIFICO SE CODICE TASSAZIONE  E' CONGRUENTE CON IL TIPO TARIFFA ITALIA
607600990817     C*
607700990817    3C     V1CFIE        IFEQ      'I'
607800020201     C     V1CNET        ANDEQ     ' '
607900990817     C     NAR(X)        LOOKUP    NAZ                                    35
608000990817    4C     *IN35         IFEQ      *OFF
608100990817     C                   MOVEL     MSG(47)       V8CMSG
608200990817     C                   SETON                                        7228
608300990817     C                   GOTO      ENDC08
608400990817    4C                   ENDIF
608500990817    3C                   ENDIF
608600990817     C*
608700990608     C                   MOVE      ORP(X)        WORPN
608800960523    2C                   ENDIF
608900990830     C*
609000990830     C* SE TARIFFA ESTERA LINEA DI PARTENZA E/O CODICE TASSAZIONE DEVE ESSERE
609100990830     C* ESTERO
609200020312   2 c                   if        v1cfie = 'E' and §ogntw <> 'EEX' and
609300020312     c                             §ogntw <> 'EUP' and §ogntw <> 'FED'
609400990830     C     NAR(X)        LOOKUP    NAZ                                    35
609500990830    3C     *IN35         IFEQ      *ON
609600990830     C                   MOVEL     MSG(48)       V8CMSG
609700990830     C                   SETON                                        7128
609800990830     C                   GOTO      ENDC08
609900990830    3C                   ENDIF
610000990830    2C                   ENDIF
610100960523    1C                   ENDIF
610200960523     C*
610300960523     C****  LINEA PARTENZA DA CUI COPIARE  ****
610400051019    1C     V8CLNP        IFne      0
610500960523     C     V8CLNP        CHAIN     AZORG                              34
610600960523    2C     *IN34         IFEQ      *ON
610700960523     C     ORGFVA        ORNE      ' '
610800960523     C     ORGFAG        ORNE      'A'
610900960523     C     ORGFAG        ANDNE     'F'
611000020527     c     orgfl2        andne     'S'
611100960523     C                   MOVEL     MSG(22)       V8CMSG
611200960523     C                   SETON                                        7328
611300960523     C                   GOTO      ENDC08
611400960523    2C                   ENDIF
611500960523    1C                   ENDIF
611600960523     C*
611700960523     C****  CODICE TASSAZIONE DA CUI COPIARE  ****
611800960523    1C     V8CCTS        IFNE      *BLANKS
611900960523     C                   Z-ADD     1             X
612000960523     C     V8CCTS        LOOKUP    CCT(X)                                 35
612100960523    2C     *IN35         IFEQ      *OFF
612200960523     C                   MOVEL     MSG(23)       V8CMSG
612300960523     C                   SETON                                        7428
612400960523     C                   GOTO      ENDC08
612500960523   X2C                   ELSE
612600050303      * errore bloccante se codice tassazione non utilizzabile in gestione
612700050303      * tariffe clienti
612800050303     c                   If        uta(x) = 'N'
612900050303     c                   Eval      v8cmsg = msg(36)
613000050303     c                   Eval      *In74 = *On
613100050303     c                   Eval      *In28 = *On
613200050303     c                   Goto      endc08
613300050303     c                   EndIf
613400020206      *
613500020206      * Se tariffa FedEx devo utilizzare solo i codici tariffa
613600020206      * previsti per FedEx
613700020206    3C     V1CNET        IFEQ      'F'
613800020214     C     UTF(X)        ANDNE     'S'
613900020206     C                   SETON                                        7428
614000020206     C                   MOVEL     MSG(100)      V8CMSG
614100020206     C                   GOTO      ENDC08
614200020206    3C                   ENDIF
614300020206      * Se tariffa non è FedEx non posso utilizzare i codici
614400020206      * tariffa previsti per FedEx
614500020206    3C     V1CNET        IFNE      'F'
614600020214     C     UTF(X)        ANDEQ     'S'
614700020206     C                   SETON                                        7428
614800020206     C                   MOVEL     MSG(101)      V8CMSG
614900020206     C                   GOTO      ENDC08
615000020206    3C                   ENDIF
615100020206      *
615200990608     C                   MOVE      ORP(X)        WORP
615300960523    2C                   ENDIF
615400960523    1C                   ENDIF
615500960523     C*
615600960523     C****  CONTROLLO CHE ESISTA ALMENO UNA RIGA DI DETTAGLIO  ****
615700960523     C****  DA CUI POTER COPIARE                               ****
615800960523     C                   MOVEL     '1'           WTROV             1
615900960523    1C     V8CLNP        IFGT      0
616000960524     C                   MOVEL     V8CLNP        COMLN2
616100960524     C*
616200960523     C* IMMESSA SOLO LA LINEA PARTENZA
616300960523    2C     V8CCTS        IFEQ      *BLANKS
616400061011     C  n50KTAD2         SETLL     TITAD000
616500061011     c   50ktad2         setll     tiofd000
616600061011     C  n50KTAD2         READE     TITAD000                               34
616700061011     c   50ktad2         reade     tiofd000                               34
616800960523   X2C                   ELSE
616900960523     C* IMMESSO ANCHE IL CODICE TASSAZIONE
617000990608     C                   MOVE      WORP          COMOR2
617100061011     C  n50KTAD3         SETLL     TITAD000
617200061011     c   50ktad3         setll     tiofd000
617300061011     C  n50KTAD3         READE     TITAD000                               34
617400061011     c   50ktad3         reade     tiofd000                               34
617500960523    2C                   ENDIF
617600960523     C*
617700960523    2C     *IN34         DOWEQ     *OFF
617800960523    3C     TADATB        IFEQ      ' '
617900960523     C                   SETON                                        34
618000960523     C                   CLEAR                   WTROV
618100960523   X3C                   ELSE
618200960523     C*
618300960523    4C     V8CCTS        IFEQ      *BLANKS
618400061011     C  n50KTAD2         READE     TITAD000                               34
618500061011     c   50ktad2         reade     tiofd000                               34
618600960523   X4C                   ELSE
618700061011     C  n50KTAD3         READE     TITAD000                               34
618800061011     c   50ktad3         reade     tiofd000                               34
618900960523    4C                   ENDIF
619000960523    3C                   ENDIF
619100960523    2C                   ENDDO
619200960523     C*
619300960523     C* WTROV <> " " --> NON ESISTONO RIGHE DI DETTAGLIO DA CUI COPIARE
619400960523    2C     WTROV         IFNE      ' '
619500960523     C                   MOVEL     MSG(62)       V8CMSG
619600960523     C                   SETON                                        7328
619700960523     C                   GOTO      ENDC08
619800960523    2C                   ENDIF
619900960523    1C                   ENDIF
620000960523     C*
620100960524     C****  SE ESISTONO GIA' RIGHE DI DETTAGLIO CON LINEA PART.   ****
620200960524     C****  E COD.TASS. DA CREARE AVVISO CON MESSAGGIO FORZABILE  ****
620300960527    0C     V8CLNN        IFNE      SAVLNN
620400960527     C     V8CCTN        ORNE      SAVCTN
620500960527     C                   MOVEL     V8CLNN        SAVLNN
620600960527     C                   MOVEL     V8CCTN        SAVCTN
620700960527     C                   CLEAR                   WTROV
620800960527     C*
620900960524    1C     V8CLNN        IFGT      0
621000960524     C                   MOVEL     V8CLNN        COMLN2
621100960524     C*
621200960524     C* IMMESSA SOLO LA LINEA PARTENZA
621300960524    2C     V8CCTN        IFEQ      *BLANKS
621400061011     C  n50KTAD2         SETLL     TITAD000
621500061011     c   50ktad2         setll     tiofd000
621600061011     C  n50KTAD2         READE     TITAD000                               34
621700061011     c   50ktad2         reade     tiofd000                               34
621800960524   X2C                   ELSE
621900960524     C* IMMESSO ANCHE IL CODICE TASSAZIONE
622000990608     C                   MOVE      WORPN         COMOR2
622100061011     C  n50KTAD3         SETLL     TITAD000
622200061011     c   50ktad3         setll     tiofd000
622300061011     C  n50KTAD3         READE     TITAD000                               34
622400061011     c   50ktad3         reade     tiofd000                               34
622500960524    2C                   ENDIF
622600960524     C*
622700960524    2C     *IN34         DOWEQ     *OFF
622800960524    3C     TADATB        IFEQ      ' '
622900960524     C                   SETON                                        34
623000960527     C                   MOVEL     '1'           WTROV
623100960524   X3C                   ELSE
623200960524     C*
623300960524    4C     V8CCTN        IFEQ      *BLANKS
623400061011     C  n50KTAD2         READE     TITAD000                               34
623500061011     c   50ktad2         reade     tiofd000                               34
623600960524   X4C                   ELSE
623700061011     C  n50KTAD3         READE     TITAD000                               34
623800061011     c   50ktad3         reade     tiofd000                               34
623900960524    4C                   ENDIF
624000960524    3C                   ENDIF
624100960524    2C                   ENDDO
624200960524     C*
624300960527     C* WTROV <> " " --> LA TARIFFA CHE SI VUOLE CREARE ESISTE GIA'
624400960527    2C     WTROV         IFNE      ' '
624500960527     C     V8CCTN        IFEQ      *BLANKS
624600960527     C                   MOVEL     MSG(68)       V8CMSG
624700960527     C                   ELSE
624800960524     C                   MOVEL     MSG(67)       V8CMSG
624900960527     C                   ENDIF
625000960527     C                   SETON                                        7128
625100960524     C                   GOTO      ENDC08
625200960524    2C                   ENDIF
625300960524    1C                   ENDIF
625400960527     C*
625500960527    0C                   ENDIF
625600090608
625700090625      * i prossimi controlli li avevamo messi convinti che la tassazione
625800090625      * non arrivasse a tassare con 950 e IT, invece no, ci sono clienti
625900090625      * che hanno solo questo tipo di tariffa e le spedizioni sono
626000090625      * regolarmente tassate quindi bypasso i controlli che avevamo fatto
626100090625      * quindi ridiamo la possibilità di duplicare verso 950 IT
626200090625     c                   leavesr
626300090608      * se la nuova linea di partenza è 950
626400090608    1c                   if        v8clnn = 950
626500090608      * non posso richiedere codice di tassazione IT o EE
626600090608     c                   if        v8cctn = 'IT' or v8cctn = 'EE'
626700090608     c                   eval      *in72 = *on
626800090608     c                   eval      *in28 = *on
626900090608     c                   eval      v8cmsg = msg(110)
627000090608     c                   leavesr
627100090608     c                   endif
627200090608      * non posso duplicare se esiste un codice di tassazione IT o EE
627300090608     c                   clear                   wtrov
627400090608     c                   movel     v8clnp        comln2
627500090608     c  n50ktad2         setll     titad04l
627600090608     c   50ktad2         setll     tiofd01l
627700090608    2c                   do        *hival
627800090608     c  n50ktad2         reade     titad04l
627900090608     c   50ktad2         reade     tiofd01l
628000090608     c                   if        %eof
628100090608     c                   leave
628200090608     c                   endif
628300090608     c                   if        tadatb <> *blanks
628400090608     c                   iter
628500090608     c                   endif
628600090608     c                   if        tadcts = 'IT' or tadcts = 'EE'
628700090608     c                   eval      wtrov = '1'
628800090608     c                   leave
628900090608     c                   endif
629000090608    2c                   enddo
629100090608     c                   if        wtrov <> *blanks
629200090608     c                   eval      *in71 = *on
629300090608     c                   eval      *in28 = *on
629400090608     c                   eval      v8cmsg = msg(121)
629500090608     c                   eval      %subst(v8cmsg:36:3) = %editc(v8clnp:'1')
629600090608     c                   leavesr
629700090608     c                   endif
629800090608    1c                   endif
629900960524     C*
630000960523     C     ENDC08        ENDSR
630100960523     C*
630200960523     C*--- ESEGUO DUPLICAZIONE DETTAGLIO TARIFFA ---------------------*
630300960523     C     DUPTAR        BEGSR
630400960523     C*
630500061012     c  n50              unlock    titad04l
630600061012     c   50              unlock    tiofd01l
630700061012
630800061012     c                   clear                   Tnta25ds
630900061012     c  n50              eval      ta25tipo = 'T'
631000061012     c   50              eval      ta25tipo = 'O'
631100061012     c                   eval      ta25ksco = v1cksc
631200061012     c                   move      vctr          ta25ctro
631300061012     c                   move      prg           ta25prgo
631400061012     c  n50              eval      ta25tipn = 'T'
631500061012     c   50              eval      ta25tipn = 'O'
631600061012     c                   eval      ta25kscn = v1cksc
631700061012     c                   move      vctr          ta25ctrn
631800061012     c                   move      prg           ta25prgn
631900061012     c                   eval      ta25tam = 'N'
632000061012     c                   eval      ta25fie = v1cfie
632100061012     c                   eval      ta25tam = 'N'
632200061012     c                   eval      ta25tad = 'N'
632300061012     c                   eval      ta25tpt = 'N'
632400061012     c                   eval      ta25tpd = 'N'
632500061012     c                   eval      ta25tgc = 'N'
632600061012     c                   eval      ta25cat = 'N'
632700061012     c                   eval      ta25inglo = 'N'
632800061012     c                   eval      ta25dupd = 'S'
632900061012     c                   move      v8clnp        ta25olnp
633000061012     c                   move      v8ccts        ta25octs
633100061012     c                   move      v8clnn        ta25nlnp
633200061012     c                   move      v8cctn        ta25ncts
633300090917      * imposto se da trattativa
633400090917     c                   eval      %subst(kpjbu:1:1) = partrat
633500061012
633600061012     c                   Call      'TNTA25R'
633700061012     c                   Parm                    kpjba
633800061012     c                   Parm                    Tnta25ds
633900061012     c                   Parm                    Tnta25tad
634000061012     c                   Parm                    Tnta25tgc
634100061012     c                   Parm                    Tnta25tpd
634200061012     c                   Parm                    Tnta25tpt
634300061012
634400061012     c                   If        ta25err <> *Blanks
634500061012     c                   Eval      *In28 = *On
634600061012     c                   Eval      vccmsg = ta25msg
634700061012     c                   endif
634800960523     C*
634900960523     C                   ENDSR
635000960523     C*
635100990601     C*--- CONTROLLO TITAD -------------------------------------------*
635200900410     C     CTRAGG        BEGSR
635300940525     C                   SETOFF                                       22
635400940525     C*
635500940706     C                   READC     TA01S03                                29
635600940525    1C     *IN29         DOWEQ     *OFF
635700940525     C*
635800940525     C* RECORD NON ANNULLATO
635900940525    2C     TADATB        IFNE      'A'
636000940525     C*
636100960522     C* SE ESISTONO IMPORTO MINIMO O IMPORTO MASSIMO VISUALIZZO
636200960522     C*   LA LETTERA "M" SULLA RIGA DI DETTAGLIO
636300960522    3C     TADMIN        IFGT      0
636400960522     C     TADMAX        ORGT      0
636500960522     C                   SETON                                        13
636600960522    3C                   ENDIF
636700960522     C*
636800940525     C****  TARIFFA CITTA'  ****
636900940525     C* TARIFFA CITTA' OBBLIGATORIA
637000940525    3C     TADITR        IFEQ      0
637100940526     C                   SETON                                        5928
637200940526     C                   MOVEL     MSG(29)       VCCMSG
637300940525     C                   GOTO      GIUCT2
637400941123     C                   ELSE
637500050616     C* 88 OFF -  TARIFFA NON IN % verifico se si
637600941124     C*   POSSONO INSERIRE DECIMALI
637700941124     C     *IN88         IFEQ      *OFF
637800941123     C                   MOVE      TADITR        W0030             3 0
637900941123     C     W0030         IFGT      0
638000050616     C*  LA MONETA NON ACCETTA DECIMALI
638100990617     C     §CVFDC        ANDNE     'S'
638200941123     C                   SETON                                        5928
638300990617     C                   MOVEL     MSG(79)       VCCMSG
638400941123     C                   GOTO      GIUCT2
638500941123     C                   ENDIF
638600941123     C                   ENDIF
638700940525    3C                   ENDIF
638800940525     C*
638900940525     C****  TARIFFA PROVINCIA  ****
639000940525     C* SE INSERITO IL CAP NON BISOGNA IMMETTERE LA TARIFFA PROVINCIA
639100020624    3C     tadCAP        IFNE      *BLANKS
639200940525     C     TADINO        ANDNE     0
639300940526     C                   SETON                                        6128
639400940526     C                   MOVEL     MSG(27)       VCCMSG
639500940525     C                   GOTO      GIUCT2
639600940525    3C                   ENDIF
639700941123     C*
639800050616     C* 88 OFF -  TARIFFA NON IN % verifico se si
639900941124     C*   POSSONO INSERIRE DECIMALI
640000941124     C  N88TADINO        IFGT      0
640100941123     C                   MOVE      TADINO        W0030             3 0
640200941123     C     W0030         IFGT      0
640300050616     C*  LA MONETA NON ACCETTA DECIMALI
640400990617     C     §CVFDC        ANDNE     'S'
640500941123     C                   SETON                                        6128
640600990617     C*                    MOVELMSG,55    VCCMSG
640700990617     C                   MOVEL     MSG(79)       VCCMSG
640800941123     C                   GOTO      GIUCT2
640900941123     C                   ENDIF
641000941123     C                   ENDIF
641100940525     C*
641200960522     C****  RAPPORTO PESO/VOLUME  ****
641300960522     C* SE INSERITO UN RAPPORTO PESO/VOL. SUPERIORE AL LIMITE INDICATO
641400960522     C*   IN TABELLA EMETTO ERRORE FORZABILE
641500960522     C     TADRPV        IFNE      SAVRPV
641600960522     C     TADRPV        ANDGT     §1GRPV
641700960522     C                   SETON                                        8928
641800960522     C                   MOVEL     MSG(59)       VCCMSG
641900960522     C                   MOVEL     TADRPV        SAVRPV
642000960522     C                   GOTO      GIUCT2
642100960522     C                   ENDIF
642200990617     C*
642300990617     C****  IMPORTO MASSIMO TARIFFA  ****
642400990617     C* VERIFICO SE INSERITO I DECIMALI SE SONO AMMESSI OPPURE NO
642500990617     C     TADMAX        IFGT      0
642600990617     C                   MOVE      TADMAX        W0030             3 0
642700990617     C     W0030         IFGT      0
642800990617     C* E LA MONETA NON ACCETTA DECIMALI
642900990617     C     §CVFDC        ANDNE     'S'
643000990617     C                   SETON                                        6228
643100990617     C                   MOVEL     MSG(79)       VCCMSG
643200990618     C                   MOVEL     *ON           *INKK
643300990617     C                   GOTO      GIUCT2
643400990617     C                   ENDIF
643500990617     C                   ENDIF
643600990617     C*
643700990617     C****  IMPORTO MINIMO  TARIFFA  ****
643800990617     C* VERIFICO SE INSERITO I DECIMALI SE SONO AMMESSI OPPURE NO
643900990617     C     TADMIN        IFGT      0
644000990617     C                   MOVE      TADMIN        W0030             3 0
644100990617     C     W0030         IFGT      0
644200990617     C* E LA MONETA NON ACCETTA DECIMALI
644300990617     C     §CVFDC        ANDNE     'S'
644400990617     C                   SETON                                        5228
644500990617     C                   MOVEL     MSG(79)       VCCMSG
644600990618     C                   MOVEL     *ON           *INKK
644700990617     C                   GOTO      GIUCT2
644800990617     C                   ENDIF
644900990617     C                   ENDIF
645000940525     C****  IMPORTO MASSIMO TARIFFA  ****
645100940525     C* SE INSERITO DEVE ESSERE MAGGIORE O UGUALE AL MINIMO
645200940525    3C     TADMAX        IFGT      0
645300940525     C     TADMAX        ANDLT     TADMIN
645400940526     C                   SETON                                        6228
645500940526     C                   MOVEL     MSG(31)       VCCMSG
645600990618     C                   MOVEL     *ON           *INKK
645700940525     C                   GOTO      GIUCT2
645800940525    3C                   ENDIF
645900980310      * E' inutile inserire dei valori minimi e massimi per scaglioni inferiori
646000980310      * al valore minimo tassabile o al valore tariffa finita
646100980310      * quindi faccio il controllo di TADMAX E/O TADMAX
646200980310     C     TADMAX        IFGT      0
646300980310     C     TADMIN        ORGT      0
646400980310      * VALORE TARIFFA FINITA
646500980310     C     TAMATA        IFGT      TADSGL
646600980310     C     TAMATA        OREQ      0
646700980505     C     WRKMIN        ANDGT     TADSGL
646800980310     C                   SETON                                        6228
646900980515     C                   MOVEL     MSG(73)       VCCMSG
647000990618     C                   MOVEL     *ON           *INKK
647100980310     C                   GOTO      GIUCT2
647200980310     C                   ENDIF
647300980310      *
647400980310     C                   ENDIF
647500940525     C*
647600940525    2C                   ENDIF
647700921119     C*
647800940525     C     GIUCT2        TAG
647900940526     C   28              Z-ADD     NRR           REC
648000940525     C*
648100940525     C* 22 - SFLNXTCHG
648200940525     C                   SETON                                        22
648300940706     C                   UPDATE    TA01S03
648400941126     C                   SETOFF                                       2213
648500940525     C*
648600940526     C   28              SETON                                        29
648700940706     C  N29              READC     TA01S03                                29
648800940525    1C                   ENDDO
648900921218     C*
649000940525     C                   ENDSR
649100940509     C*
649200990601     C*--- AGGIORNAMENTO TITAD ---------------------------------------*
649300921120     C     AGGTAD        BEGSR
649400900924     C                   SETOFF                                       19
649500940706     C                   READC     TA01S03                                29
649600940524     C     *IN29         DOWEQ     '0'
649700900925     C                   EXSR      UPDTAD
649800940706     C                   READC     TA01S03                                29
649900921218     C                   END
650000900925     C                   ENDSR
650100930630     C*
650200990601     C*--- AGGIORNAMENTO TITAD SU DISCO ------------------------------*
650300921118     C     UPDTAD        BEGSR
650400980219     C* VALORE POSITIVO NEI CAMPI
650500980220     C                   MLLZO     1             TADSGL
650600980219     C                   MLLZO     1             TADITR
650700980219     C                   MLLZO     1             TADINO
650800980219     C                   MLLZO     1             TADRPV
650900980219     C                   MLLZO     1             TADMIN
651000980219     C                   MLLZO     1             TADMAX
651100980219     C*
651200980219     C                   Z-ADD     TADITR        ITRCOM
651300980219     C                   Z-ADD     TADINO        INOCOM
651400980219     C                   Z-ADD     TADRPV        RPVCOM
651500940518     C                   Z-ADD     TADMIN        MINCOM
651600940518     C                   Z-ADD     TADMAX        MAXCOM
651700940518     C                   MOVEL     TADATB        ATBCOM
651800000000     C** SALVO CAMPI PRIMA CHAIN
651900061011     C  n50KTAD          CHAIN     TITAD000                           92
652000061011     c   50ktad          chain     tiofD000                           92
652100990729     C*
652200990729     C     *IN92         IFEQ      *OFF
652300921118     C*
652400930317     C     *IN50         IFEQ      '1'
652500930317     C     ATBCOM        ANDEQ     'A'
652600061011     C  n50              DELETE    TITAD000
652700061011     C   50              delete    tiofd000
652800930317     C                   ELSE
652900930317     C*
653000930317     C                   MOVEL     ATBCOM        TADATB
653100940518     C                   Z-ADD     ITRCOM        TADITR
653200921210     C                   Z-ADD     INOCOM        TADINO
653300940518     C                   Z-ADD     MINCOM        TADMIN
653400940518     C                   Z-ADD     MAXCOM        TADMAX
653500000000     C                   Z-ADD     RPVCOM        TADRPV
653600910605     C*
653700130124     C*******            Z-ADD     dateu8        TADDTR
653800940511     C                   MOVEL     *BLANKS       TADFTR
653900061011     C  n50              UPDATE    TITAD000
654000061011     C   50              update    tiofd000
654100930317     C                   END
654200990729     C*
654300990729     C                   END
654400930317     C*
654500930317     C     ATBCOM        IFEQ      'A'
654600900924     C                   SETON                                        19
654700900924     C                   END
654800000000     C                   SETON                                        30
654900000000     C                   ENDSR
655000050701      *---------------------------------------------------------------*
655100050701      * controllo se nella tariffa a spedizione esistono + scaglioni
655200050701      * per la stessa lnp e cts
655300050701      *---------------------------------------------------------------*
655400050701     c     Sr_CtrTarSpe  BegSr
655500050701
655600050701     c                   Clear                   savlnp
655700050701     c                   Clear                   savcts
655800050701     c                   Clear                   savsgl
655900050701     c                   Clear                   savnaz
656000050701     c                   Clear                   savcap
656100050701
656200050701     c                   Eval      werr = *Off
656300050701     c                   Clear                   wlnp
656400050701     c                   Clear                   wcts
656500050701     c                   Clear                   wcap
656600050701
656700050701      * Leggo tutto il titad
656800061011     c  n50ktam2         Setll     Titad04l
656900061011     c   50ktam2         Setll     Tiofd01l
657000050701     c                   Do        *Hival
657100061011     c  n50ktam2         Reade(n)  Titad04l
657200061011     c   50ktam2         Reade(n)  Tiofd01l
657300050701
657400061011     c                   If        %Eof
657500050701     c                   Leave
657600050701     c                   EndIf
657700050701
657800050701     c                   If        tadatb <> *Blanks
657900050701     c                   Iter
658000050701     c                   EndIf
658100050701
658200050701     c                   If        tadlnp = savlnp and tadcts = savcts
658300050701     c                             and tadnaz = savnaz and tadcap = savcap
658400050701     c                             and tadsgl <> savsgl and savsgl > *Zeros
658500050701     c                   Move      tadlnp        wlnp
658600050701     c                   Move      tadcts        wcts
658700050701     c                   If        Tadcap <> *Blanks
658800050701     c                   Move      tadcap        wcap
658900050701     c                   EndIf
659000050701     c                   Eval      werr = *On
659100050701     c                   Leave
659200050701     c                   EndIf
659300050701     c                   If         tadlnp <> savlnp or tadcts <> savcts
659400050701     c                              or tadnaz <> savnaz or tadcap <> savcap
659500050701     c                              or tadsgl <> savsgl
659600050701     c                   Eval       savlnp = tadlnp
659700050701     c                   Eval       savcts = tadcts
659800050701     c                   Eval       savnaz = tadnaz
659900050701     c                   Eval       savcap = tadcap
660000050701     c                   Eval       savsgl = tadsgl
660100050701     c                   EndIf
660200050701
660300050701     c                   EndDo
660400050701
660500050701     c                   EndSr
660600110128
660700110128      /free
660800110128       //---------------------------------------------------------------
660900110128       //?Controllo se rapporto peso volume uguale su tutti gli scaglioni
661000110128       //---------------------------------------------------------------
661100110128       BEGSR CtrRpv;
661200110128
661300110128         werrRpv = *off;
661400110128         clear oldrpv;
661500110128         clear wlnpRpv;
661600110128         clear wctsRpv;
661700110128         clear wsglRpv;
661800110128
661900110128       //?Leggo tutto il dettaglio tariffario per controllare il
662000110128       //?rapporto peso volume
662100110128         IF  not *in50;
662200110128           setll (v1cksc : vctr : prg) TITAD04L;
662300110128           reade(n) (v1cksc : vctr : prg) TITAD04L;
662400110128         ENDIF;
662500110128         IF  *in50;
662600110128           setll (v1cksc : vctr : prg) TIOFD01L;
662700110128           reade(n) (v1cksc : vctr : prg) TIOFD01L;
662800110128         ENDIF;
662900110128
663000110128         DOW not %eof;
663100110201         IF  TADatb = *blanks;
663200110128
663300110128           IF  oldrpv = 0;
663400110128             oldrpv = TADrpv;
663500110128           ENDIF;
663600110128
663700110128           IF  TADrpv <> OLDrpv;
663800110128             werrRpv = *on;
663900110128             wlnpRpv = TADlnp;
664000110128             wctsRpv = TADcts;
664100110128             wsglRpv = TADsgl;
664200110202             leave;
664300110128           ENDIF;
664400110128
664500110128           oldrpv = TADrpv;
664600110201         ENDIF;
664700110128
664800110128           IF  not *in50;
664900110128             reade(n) (v1cksc : vctr : prg) TITAD04L;
665000110128           ENDIF;
665100110128           IF  *in50;
665200110128             reade(n) (v1cksc : vctr : prg) TIOFD01L;
665300110128           ENDIF;
665400110128         ENDDO;
665500110128
665600110128       ENDSR;
665700110128      /end-free
665800110128
665900930630     C*
666000930630     C*--- CARICAMENTO SUBFILE IN AVANTI -----------------------------*
666100920526     C     CARSFA        BEGSR
666200921218     C                   SETON                                        91
666300900925     C                   Z-ADD     SAVREC        NRR               5 0
666400000000     C** PULIZIA SUBFILE AGGIORNAMENTO
666500900925     C     SOPRA         TAG
666600061011     C  n50KTAM2         READE     TITAD000                               33
666700061011     c   50ktam2         reade     tiofd000                               33
666800900924     C* FINE SCORRIMENTO
666900940524     C  N33TADATB        IFEQ      'A'
667000900925     C                   GOTO      SOPRA
667100900925     C                   END
667200900925     C*
667300940524     C   33              DO
667400930224     C     NRR           IFEQ      0
667500930224     C                   SETOFF                                       21
667600930224     C                   ELSE
667700900924     C                   MOVEL     KE            KI
667800900924     C                   MOVEL     CTSE          CTSI
667900940526     C*
668000940526     C                   MOVEL     MSG(32)       VCCMSG
668100940526     C                   SETON                                        28
668200930224     C                   END
668300900924     C                   GOTO      ENDCAR
668400900924     C                   END
668500900924     C*
668600900924     C                   Z-ADD     0             B                 3 0
668700900924     C                   DO        *HIVAL
668800020207      *
668900020207     C                   CLEAR                   AT0CTS
669000900924     C*
669100900924     C     TADATB        IFNE      'A'
669200000000     C                   ADD       1             NRR
669300900924     C                   ADD       1             B
669400900924     C     B             IFEQ      1
669500930630     C** KIAVE INIZIALE
669600000000     C                   MOVEL     KIAVE         KI
669700000000     C                   MOVEL     TADCTS        CTSI              2
669800000000     C                   END
669900930630     C*
670000930630     C** DECODIFICA CODICE TASSAZIONE
670100000000     C                   Z-ADD     1             A
670200940524     C     TADCTS        LOOKUP    CCT(A)                                 35
670300940524     C   35              MOVEL     DCT(A)        DESCTS
670400940524     C  N35              MOVEL     *BLANKS       DESCTS
670500020207      *
670600020207      * Se tariffa FedEx devo utilizzare solo i codici tariffa
670700020207      * previsti per FedEx
670800020207     C     V1CNET        IFEQ      'F'
670900020207     C     UTF(A)        ANDNE     'S'
671000020207     C                   MOVE      RIMM          AT0CTS
671100020207     C     VCCMSG        IFEQ      *BLANKS
671200020207     C                   SETON                                        28
671300020207     C                   MOVEL     MSG(100)      VCCMSG
671400020207     C                   ENDIF
671500020207     C                   ENDIF
671600020207      * Se tariffa non è FedEx non posso utilizzare i codici
671700020207      * tariffa previsti per FedEx
671800020207     C     V1CNET        IFNE      'F'
671900020207     C     UTF(A)        ANDEQ     'S'
672000020207     C                   MOVE      RIMM          AT0CTS
672100020207     C     VCCMSG        IFEQ      *BLANKS
672200020207     C                   SETON                                        28
672300020207     C                   MOVEL     MSG(101)      VCCMSG
672400020207     C                   ENDIF
672500020207     C                   ENDIF
672600990624     C* DECODIFICO LA NAZIONE SE E' DIVERSA DA BLANKS
672700990729     C     TADNAZ        IFEQ      *BLANKS
672800990624     C                   CLEAR                   DESNAZ
672900990624     C                   ELSE
673000990623     C                   MOVEL     '15'          COD
673100990623     C                   MOVEL     *BLANKS       KEY
673200990623     C                   MOVEL     TADNAZ        KEY
673300990623     C     KTAB          CHAIN     TABEL                              32
673400990623     C*
673500990623     C     *IN32         IFEQ      *OFF
673600990623     C     TBLFLG        ANDEQ     ' '
673700990623     C                   MOVEL     TBLUNI        DESNAZ
673800990623     C                   ENDIF
673900990624     C                   ENDIF
674000990623     C***
674100960521     C* RAPPORTO PESO/VOLUME
674200960521     C                   MOVEL     TADRPV        SAVRPV
674300941206     C*
674400941206     C* SE ESISTONO IMPORTO MINIMO O IMPORTO MASSIMO VISUALIZZO
674500941206     C*   LA LETTERA "M" SULLA RELATIVA RIGA DI DETTAGLIO
674600941206    3C     TADMIN        IFGT      0
674700941206     C     TADMAX        ORGT      0
674800941206     C                   SETON                                        13
674900941206    3C                   ENDIF
675000130124      /free
675100130124        clear atatb;
675200130124        clear atitr;
675300130124        clear atino;
675400130124        clear atrpv;
675500130124        clear atmin;
675600130124        clear atmax;
675700130124       //?imposto attributi visualizzazione sui campi che non sono da
675800130124       //?modificare
675900130124       //?lascio libera la modifica se rcd ha data = oggi
676000130124        IF  *in17 and TADdtr <> dateu8;
676100130124          atatb = nPRO;
676200130124          atitr = nPRO;
676300130124          atino = nPRO;
676400130124          atrpv = nPRO;
676500130124          atmin = nPRO;
676600130124          atmax = nPRO;
676700130124        ENDIF;
676800130124      /end-free
676900130124
677000930630     C*
677100940706     C                   WRITE     TA01S03
677200941206     C                   SETOFF                                       13
677300941206     C*
677400900924     C     B             CABEQ     16            ENDAGA
677500900924     C                   END
677600900924     C*
677700061011     C  n50KTAM2         READE     TITAD000                               33
677800061011     c   50ktam2         reade     tiofd000                               33
677900940524     C  N33              END
678000000000     C     ENDAGA        TAG
678100900924     C                   Z-ADD     NRR           REC
678200900924     C                   Z-ADD     NRR           SAVREC            9 0
678300900924     C*
678400000000     C                   MOVEL     KIAVE         KE
678500000000     C                   MOVEL     TADCTS        CTSE              2
678600050701
678700050701      * se errore per + scaglioni in tariffa a spedizione imposto
678800050701      * il msg e accendo l'indicatore
678900050701     c                   If        werr = *On
679000050701     c                   Eval      vccmsg = 'Par ' + wlnp + ' Arrivo ' +
679100050701     c                             wcts
679200050701     c                   If        wcap <> *Blanks
679300050701     c                   Eval      vccmsg = %trim(vccmsg) +
679400050701     c                             ' - c.a.p. ' + wcap
679500050701     c                   EndIf
679600050701     c                   Eval      vccmsg = %trim(vccmsg) +
679700050701     c                             ' presenti più scaglioni'
679800050701     c                   Eval      *In28 = *On
679900050701     c                   EndIf
680000110128
680100110128       //?Se tariffa FedEx e scaglioni con rapporto peso volume
680200110128       //?diverso emetto messaggio di errore
680300110128     c                   IF        werrRpv
680400110128     c                   eval      VCCmsg = 'Par ' + %editc(wlnpRpv:'X') +
680500110128     c                             ' Arrivo ' + wctsRpv +
680600110128     c                             ' Scaglione ' + %editc(wsglRpv:'4') +
680700110128     c                             ' presente P/V diverso'
680800110128     c                   eval      *in28 = *on
680900110128     c                   ENDIF
681000050701
681100000000     C** SE NON CI SONO PIU RECORD DA LEGGERE
681200000000     C** KE = KI
681300900924     C     ENDCAR        ENDSR
681400930630     C*
681500930630     C*--- ELIMINAZIONE SUBFILE SU DISCO -----------------------------*
681600921218     C     ELISF         BEGSR
681700921218     C   91              SETON                                        93
681800940706     C   91              WRITE     TA01C03
681900921218     C   91              SETOFF                                       93
682000921218     C   99              SETON                                        83
682100940706     C   99              WRITE     TA01C05
682200921218     C   99              SETOFF                                       83
682300000000     C                   ENDSR
682400930628     C*
682500930628     C*--- CARICAMENTO TABELLE ---------------------------------------*
682600920526     C     CARTAB        BEGSR
682700940503     C*
682800940503     C****  CODICI TASSAZIONE E DESCRIZIONI: CCT / DCT / ORP  ****
682900020911      * + EST
683000020206     C                   CLEAR                   DSCT
683100940531     C                   Z-ADD     1             K                 5 0
683200930628     C                   MOVEL     'CT'          COD
683300940510     C     KTAB2         SETLL     TABEL
683400940510     C     KTAB2         READE     TABEL                                  32
683500930628     C*
683600940510     C     *IN32         DOWEQ     *OFF
683700930628     C     TBLFLG        IFEQ      ' '
683800000000     C                   MOVEL     TBLKEY        CCT(K)
683900000000     C                   MOVEL     TBLUNI        DCT(K)
684000020206     C                   MOVEL     TBLUNI        DSCT
684100020206     C                   MOVE      §CTCOR        ORP(K)
684200020206     C                   MOVE      §CTCOR        REG(K)
684300020206     C                   MOVEL     §CTNAR        NAR(K)
684400020206     C                   MOVEL     §CTUTF        UTF(K)
684500020911      * Flag estero
684600020911     c                   Movel     §ctest        est(k)
684700050303      * flag utilizzo in gestione tariffe
684800050303     c                   Eval      uta(k) = §ctuta
684900930628     C                   ADD       1             K
685000930628     C                   ENDIF
685100930628     C*
685200940510     C     KTAB2         READE     TABEL                                  32
685300930628     C                   ENDDO
685400930628     C*
685500941124     C****  CODICI TARIFFA    E DESCRIZIONI: CTR/DTR/MIN/TAP/SAP  ****
685600940531     C                   Z-ADD     1             K
685700000000     C                   MOVEL     'TR'          COD
685800940510     C     KTAB2         SETLL     TABEL
685900940510     C     KTAB2         READE     TABEL                                  32
686000930628     C*
686100940510     C     *IN32         DOWEQ     *OFF
686200930628     C     TBLFLG        IFEQ      ' '
686300980505     C                   MOVEL     TBLUNI        DSTR
686400980505     C* CONTROLLO CHE IL TIPO TARIFFA E' ABILITATO ALLA GESTIONE TARIFFE CLIENTI
686500980505     C     §TRUTC        IFEQ      'S'
686600000000     C                   MOVEL     TBLKEY        CTR(K)
686700920525     C                   MOVEL     §TRDES        DTR(K)
686800930628     C* MINIMO TASSABILE PER TIPO TARIFFA
686900940913     C                   Z-ADD     §TRATA        MIN(K)
687000941124     C* IMPORTO TARIFFA  IN % O £ ("S"=TARIFFA IN % - " "=TARIFFA IN £)
687100941108     C                   MOVEL     §TRTAP        TAP(K)
687200950116     C* SCAGLIONE CON DECIMALI O SENZA ("S"=CON DECIM." "=SENZA DECIM.)
687300941124     C                   MOVEL     §TRSAP        SAP(K)
687400980505     C* GESTIONE DEGLI ARROTONDAMENTI IN TARIFFA
687500980505     C                   MOVEL     §TRARR        ARR(K)
687600980505     C* GESTIONE RAPPORTO PESO / VOLUME
687700980505     C                   MOVEL     §TRRPV        RPV(K)
687800021118     C* Flag abilitazione per tariffe FedEx
687900021118     c                   Movel     §TrUtf        Rutf(K)
688000930628     C                   ADD       1             K
688100930628     C                   ENDIF
688200980505     C                   ENDIF
688300930628     C*
688400940510     C     KTAB2         READE     TABEL                                  32
688500930628     C                   ENDDO
688600920525     C*
688700950116     C****  CODICI NAZIONE : NAZ  ****
688800941011     C                   Z-ADD     1             K
688900941011     C                   MOVEL     '15'          COD
689000941011     C     KTAB2         SETLL     TABEL
689100941011     C     KTAB2         READE     TABEL                                  32
689200941011     C*
689300941011     C     *IN32         DOWEQ     *OFF
689400941011     C     TBLFLG        IFEQ      ' '
689500941011     C                   MOVEL     TBLUNI        DS15
689600950116     C* CARICO SOLO NAZIONE ITALIA
689700950116     C     §15ITA        IFEQ      'I'
689800941011     C                   MOVEL     TBLKEY        NAZ(K)
689900941011     C                   ADD       1             K
690000950116     C                   ENDIF
690100941011     C                   ENDIF
690200941011     C*
690300941011     C     KTAB2         READE     TABEL                                  32
690400941011     C                   ENDDO
690500960520     C*
690600960520     C****  AGGANCIO LA TABELLA "1G"  ****
690700960520     C                   MOVE      '1G'          COD
690800960520     C                   MOVEL     '1       '    KEY
690900960520     C     KTAB          CHAIN     TABEL                              32
691000960520     C     *IN32         IFEQ      *OFF
691100960520     C                   MOVEL     TBLUNI        DS1G
691200960520     C                   ELSE
691300960520     C                   CLEAR                   DS1G
691400960520     C                   ENDIF
691500021118
691600021118      * Tabella dati di default per tariffe FedEx
691700021118
691800021118      * aggancio la tabella DFE
691900021118     c                   clear                   ddfe
692000021118     c                   clear                   tibs02ds
692100021118     c                   movel     'C'           t02mod
692200021118     c                   movel     knsif         t02sif
692300021118     c                   movel     'DFE'         t02cod
692400021118     c                   movel(p)  'FED'         t02ke1
692500021118     c                   call      'TIBS02R'
692600021118     c                   parm                    kpjba
692700021118     c                   parm                    tibs02ds
692800021118
692900021118     c                   if        t02err = *blanks
693000021118     c                   movel     t02uni        ddfe
693100021118     c                   endif
693200101013
693300101013      /free
693400101013       //?Carico scatti ISTAT
693500101013         SISsca = 0;
693600101013         clear DIA;
693700101013         setll (SISsca) TISIS01L;
693800101013
693900101013         DOW  not $EoF;
694000101013
694100101013           read TISIS01L;
694200101013
694300101013           IF  %Eof(TISIS01L);
694400101013             $EoF = *on;
694500101013             leave;
694600101013           ENDIF;
694700101013
694800101013           DIA(SISsca) = SISdata;
694900101013
695000101013         ENDDO;
695100101013
695200101013      /end-free
695300101013
695400050530     c* Carico tabella tariffe particolari in scadenza, da eliminare
695500050530     C                   MOVE      '1P'          COD
695600050530     c                   clear                   xx                4 0
695700050530     c     ktab2         setll     tabel00f
695800050530     c     ktab2         reade     tabel00f
695900050530     c                   dow       not %eof(tabel00f)
696000050530     c                   if        tblflg=' '
696100050530     c                   movel     tbluni        ds1p
696200050530     c                   if        §1peli>*zeros
696300050530     c                   add       1             xx
696400050530     c                   movel     tblkey        tpareli(xx)
696500050530     c                   movel     §1peli        tpardat(xx)
696600050530     c                   endif
696700050530     c                   endif
696800050530     c
696900050530     c     ktab2         reade     tabel00f
697000050530     c                   enddo
697100040915     c*
697200031118      *
697300000000     C                   ENDSR
697400051108
697500051108      *--------------------------------------------------------------------*
697600051108      * CONFERMA ANNULLAMENTO OFFERTA/TARIFFA
697700051108      *--------------------------------------------------------------------*
697800051108     c     Sr_Confann    BegSr
697900051108
698000051108     c   50              Eval      w11tip = 'OFFERTA'
698100051108     c  n50              Eval      w11tip = 'TARIFFA'
698200051108     c                   Clear                   w11sino
698300051108     c                   Do        *Hival
698400051108     c                   Exfmt     ta01w11
698500051108     c   kf              Leave
698600051108     c                   EndDo
698700051108
698800051108     c                   EndSr
698900051108
699000930628     C*
699100940705     C*--- ANNULLAMENTO TOTALE DELLA TARIFFA -------------------------*
699200000000     C     ANNTAR        BEGSR
699300931118     C*
699400941012     C* SOLO SE SONO IN FILIALE EMETTO VIDEATA PER STAMPA LETTERA DI
699500941012     C*   ANNULLAMENTO TARIFFA
699600941012    1C     SIMFEL        IFNE      0
699700940310     C                   MOVE      '4'           PA2FLG
699800920526     C                   EXSR      STAMPA
699900941012    1C                   ENDIF
700000901026     C*
700100940511     C****  A N N U L L O   N O T E  ****
700200940511     C* SE ESISTE ALTRO PROGRESSIVO LASCIO LE NOTE ALTRIMENTI LE DELETO
700300941012     C                   Z-ADD     0             ANN               5 0
700400940615     C  N50KTAM          SETLL     TNTAM000
700500100407     C   50KTAM          SETLL     TNofm000
700600940615     C  N50KTAM          READE     TNTAM000                               21
700700100407     C   50KTAM          READE     TNofm000                               21
700800920526     C*
700900100407    1C     *IN21         DOWEQ     *OFF
701000920526     C     TAMATB        IFEQ      ' '
701100941012     C                   ADD       1             ANN
701200940705     C                   ENDIF
701300061011     C  n50KTAM          READE     TNTAM000                               21
701400100407     C   50KTAM          READE     TNofm000                               21
701500941012    1C                   ENDDO
701600920526     C*
701700941012     C* 18 ON  - SONO IN FILIALE
701800941012     C* ANN < 2  --->  NON ESISTONO ALTRE TARIFFE PER QUEL CLIENTE
701900941012     C*                QUINDI CANCELLO LE NOTE
702000941012    1C   18ANN           IFLT      2
702100920526     C                   MOVEL     CCC           NOTKEY
702200940526     C                   MOVE      V1CKSC        NOTKEY
702300941012     C  N50              MOVEL     'C'           APPL
702400100407     C   50              MOVEL     'V'           APPL
702500100407     c   85              movel     'T'           appl
702600920526     C                   MOVEL     VCTR          NOTKE2
702700920526     C*
702800940615     C     KNTC          SETLL     TFNTC
702900920526     C     KNTC          READE     TFNTC                                  21
703000920526     C*
703100941012    2C     *IN21         DOWEQ     *OFF
703200040806     c                   delete    tfntc
703300920526     C     KNTC          READE     TFNTC                                  21
703400941012    2C                   ENDDO
703500941012    1C                   ENDIF
703600920526     C*
703700970117     C****  ANNULLO FILE DATI TIPO: TNETC/TNETT/TNETS  ****
703800970122     C                   CLEAR                   DSTA47
703900970122     C                   MOVEL     '1'           WTA47             1
704000970117     C     *IN50         IFEQ      *OFF
704100970122     C                   MOVEL     '1'           D47TUP
704200970122     C                   MOVEL     'T'           D47CTO
704300970117     C                   ELSE
704400970122     C                   MOVEL     '2'           D47TUP
704500970122     C                   MOVEL     'O'           D47CTO
704600970117     C                   ENDIF
704700050127     C                   MOVEL     'S'           D47DSF
704800970122     C                   Z-ADD     V1CKSC        D47KSC
704900970122     C                   Z-ADD     VCTR          D47CTR
705000970122     C                   Z-ADD     PRG           D47PRG
705100970122     C                   MOVEL     DSTA47        KPJBU
705200970122     C                   CALL      'TNTA47R'
705300970117     C                   PARM                    KPJBA
705400970117     C*
705500950105     C****  ANNULLO TARIFFE GIACENZA  ****
705600990611     C  N50KTAM2         SETLL     TITGC000
705700990611     C   50KTAM2         SETLL     TIOGC000
705800990611     C  N50KTAM2         READE     TITGC000                               21
705900990611     C   50KTAM2         READE     TIOGC000                               21
706000950105     C*
706100950105    1C     *IN21         DOWEQ     *OFF
706200950105     C* TARIFFA
706300950105    2C  N50TGCATB        IFEQ      ' '
706400040914     C                   Z-ADD     dateu8        TGCDTR
706500950105     C                   MOVEL     *BLANKS       TGCFTR
706600950105     C                   MOVEL     'A'           TGCATB
706700990611     C                   UPDATE    TITGC000
706800030714      *
706900950105    2C                   ENDIF
707000950105     C* OFFERTA
707100990611     C   50              DELETE    TIOGC000
707200950105     C*
707300990611     C  N50KTAM2         READE     TITGC000                               21
707400990611     C   50KTAM2         READE     TIOGC000                               21
707500950105    1C                   ENDDO
707600950105     C*
707700940511     C****  ANNULLO DETTAGLIO TARIFFE PARTICOLARI  ****
707800061011     C  n50KTAM2         SETLL     TITPD000
707900061011     c   50ktam2         setll     tiopd000
708000061011     C  n50KTAM2         READE     TITPD000                               21
708100061011     c   50ktam2         reade     tiopd000                               21
708200931118     C*
708300941012    1C     *IN21         DOWEQ     *OFF
708400931118     C* TARIFFA
708500941012    2C  N50TPDATB        IFEQ      ' '
708600040914     C                   Z-ADD     dateu8        TPDDTR
708700931118     C                   MOVEL     *BLANKS       TPDFTR
708800931118     C                   MOVEL     'A'           TPDATB
708900990601     C                   UPDATE    TITPD000
709000941012    2C                   ENDIF
709100931118     C* OFFERTA
709200061011     C   50              DELETE    TIoPD000
709300931118     C*
709400061011     C  n50KTAM2         READE     TITPD000                               21
709500061011     c   50ktam2         reade     tiopd000                               21
709600941012    1C                   ENDDO
709700931118     C*
709800940511     C****  ANNULLO TESTATA   TARIFFE PARTICOLARI  ****
709900061011     C  n50KTAM2         SETLL     TITPT000
710000061011     c   50ktam2         setll     tiopt000
710100061011     C  n50KTAM2         READE     TITPT000                               21
710200061011     c   50ktam2         reade     tiopt000                               21
710300931118     C*
710400941012    1C     *IN21         DOWEQ     *OFF
710500931118     C* TARIFFA
710600941012    2C  N50TPTATB        IFEQ      ' '
710700040914     C                   Z-ADD     dateu8        TPTDTR
710800931118     C                   MOVEL     *BLANKS       TPTFTR
710900931118     C                   MOVEL     'A'           TPTATB
711000990601     C                   UPDATE    TITPT000
711100030714      *
711200941012    2C                   ENDIF
711300931118     C* OFFERTA
711400061011     C   50              DELETE    TIoPT000
711500931118     C*
711600061011     C  n50KTAM2         READE     TITPT000                               21
711700061011     c   50ktam2         reade     tiopt000                               21
711800941012    1C                   ENDDO
711900931118     C*
712000940511     C****  A N N U L L O   D E T T A G L I O  ****
712100061011     C  n50KTAM2         SETLL     TITAD000
712200061011     c   50ktam2         setll     tiofd000
712300061011     C  n50KTAM2         READE     TITAD000                               21
712400061011     c   50ktam2         reade     tiofd000                               21
712500931118     C*
712600941012    1C     *IN21         DOWEQ     *OFF
712700931118     C* TARIFFA
712800941012    2C  N50TADATB        IFEQ      ' '
712900130124     C*********          Z-ADD     dateu8        TADDTR
713000940511     C                   MOVEL     *BLANKS       TADFTR
713100931118     C                   MOVEL     'A'           TADATB
713200990601     C                   UPDATE    TITAD000
713300941012    2C                   ENDIF
713400931118     C* OFFERTA
713500061011     C   50              DELETE    TIofD000
713600931118     C*
713700061011     C  n50KTAM2         READE     TITAD000                               21
713800061011     c   50ktam2         reade     tiofd000                               21
713900941012    1C                   ENDDO
714000931118     C*
714100940705     C****  A N N U L L O   T E S T A T A      ****
714200061011     C  n50KTAM2         CHAIN     TNTAM000                           21
714300061011     c   50ktam2         chain     tnofm000                           21
714400941012    1C     *IN21         IFEQ      *OFF
714500941012     C* TARIFFA
714600981111    2C     *IN50         IFEQ      *OFF
714700960509     C                   Z-ADD     DATEU8        TAMDUV
714800940511     C                   MOVEL     *BLANKS       TAMFTR
714900040913     C                   Z-ADD     DATEU8        TAMDTR
715000000000     C                   MOVEL     'A'           TAMATB
715100940615     C                   UPDATE    TNTAM000
715200160209      /free
715300160209       //?richiamo pgm per cancellare:
715400160209       //?file Tariffe Inserite
715500160209         clear TRSM90DS;
715600160209         ISM90ksc = TAMksc;
715700160209         ISM90ctr = TAMctr;
715800160209         ISM90prg = TAMprg;
715900160209         trsm90r (kpjba:TRSM90DS);
716000160209      /end-free
716100160209
716200030714      *
716300981111   X2C                   ELSE
716400981111     C* OFFERTA
716500981111     C**
716600061011     C                   DELETE    Tnofm000
716700981111     C**
716800981111     C* VERIFICO SE IL CLIENTE HA ALTRE OFFERTE
716900981111     C                   CLEAR                   WAGG
717000070522     C     V1CKSC        CHAIN(n)  TNofm000                           21
717100981111    3C     *IN21         DOWEQ     *OFF
717200981111    4C     TAMFIE        IFEQ      WFIE
717300981111     C* NON FACCIO NULLA PERCHE' C'E' UN'ALTRA OFFERTA PRESENTE
717400981111     C                   SETON                                        21
717500981111     C                   MOVEL     'N'           WAGG              1
717600090507      * appena trovo una tariffa dello stesso tipo dell'annullata
717700090507      * valorizzo il campo ed esco dal loop
717800090507      * per evitare di anadare sopra al campo WAGG
717900090507      * con una 'S'
718000090507     c                   leave
718100981111   X4C                   ELSE
718200981111     C* TROVATA UNA DI ALTRO TIPO MEMORIZZO
718300981111     C                   MOVEL     'S'           WAGG
718400070522     C     V1CKSC        READE(n)  TNofm000                               21
718500981111    4C                   ENDIF
718600981111    3C                   ENDDO
718700100421      * trattative
718800101104     c                   If        *in85
718900100421      ** TIVOF
719000100712      * Non cancello mai il TIVOF ma imposto esito = ad *
719100100712      * Così evitiamo di cancellare delle offerte non considerate nelle nuove
719200100712      * statistiche
719300100421     c     ktam2         Chain     tivof01l
719400100421     c                   If        %found(tivof01l)
719500100421      * verifico data presentazione
719600100712     c****               movel     vofdpo        w0060
719700100712     c****               if        w0060 <= §sdfstcu
719800100421     c                   eval      vofeso = '*'
719900100421     c                   update    tivof000
720000100712     c****               else
720100100712     c*****              delete    tivof000
720200100712     c***                endif
720300100421     c                   endif
720400100407
720500100421     c                   endif
720600981111    2C                   ENDIF
720700981111    1C                   ENDIF
720800100408
720900100408      /free
721000100408       //?Se note tariffe a livello cliente elimino se non ci sono più
721100100408       //?tariffe/offerte
721200100408       exsr sr_DelNote10;
721300100408      /end-free
721400030716      *
721500030716      * A N N U L L O   S T O R I C O   TITAV01L
721600030716      *
721700050603     c                   if        not *in50
721800030716     C     KTAM2         setll     TITAV01L
721900030716      *
722000030716     C                   do        *hival
722100030716      *
722200030716     C     KTAM2         reade     TITAV01L
722300030716      *
722400030716     C                   if        %eof(TITAV01L)
722500030716     c                   leave
722600030716     c                   endif
722700030716      *
722800030716     c                   eval      TAVatb = 'A'
722900030716     c                   clear                   TAVftr
723000040914     c                   z-add     dateu8        TAVdtr
723100030716      *
723200030716     c                   update    titav
723300030716      *
723400030716     c                   enddo
723500040927      * scrivo un nuovo record di TITAV (annullato) per l'annullamento
723600040927      * della tariffa
723700040927     c                   Eval      annultar = 'S'
723800040927     c                   eval      tavtrd = 'TES'
723900040927     c                   eval      tavcta = 'A'
724000040927     c                   exsr      SUB_STORICO
724100040927     c                   Clear                   annultar
724200050603     c                   endif
724300930223     C*
724400921218     C                   ENDSR
724500100407
724600100407      /free
724700100408       //---------------------------------------------------------------
724800100408       //?Deleto note 10.
724900100408       //---------------------------------------------------------------
725000100407       BEGSR sr_DelNote10;
725100100407
725200100407       //?Provo a cercare note a livello cliente senza niente in key 2
725300100407         clear NOTke2;
725400100408         NTCtnt = '10';
725500100408         setll (appl:notkey:notke2:ntctnt) TFNTC01L;
725600100407         IF  %equal(TFNTC01L);
725700100407         //?ci sono note tariffa a livello cliente, controllo se ci sono
725800100407         //?controllo se ci sono ancora tariffe
725900100407           IF  not *in50;
726000100407             setll v1cksc TNTAM01L;
726100100407             reade v1cksc TNTAM01L;
726200100407             DOW  not %eof(TNTAM01L);
726300100407               IF  tamatb = ' ';
726400100407                 leavesr;
726500100407               ENDIF;
726600100407               reade v1cksc TNTAM01L;
726700100407             ENDDO;
726800100407           ENDIF;
726900100407         //?controllo se ci sono ancora offerte
727000100407           IF  *in50;
727100100407             setll v1cksc TNOFM01L;
727200100407             reade v1cksc TNOFM01L;
727300100407             DOW  not %eof(TNOFM01L);
727400100407               IF  tamatb = ' ';
727500100407                 leavesr;
727600100407               ENDIF;
727700100407               reade v1cksc TNOFM01L;
727800100407             ENDDO;
727900100407           ENDIF;
728000100407         //?se arrivo qua vuol dire che non ho trovato ne tariffe ne offerte
728100100407         //?posso cancellare le note tariffa a livello cliente
728200100408           reade (appl:notkey:notke2:ntctnt) TFNTC01L;
728300100407           DOW  not %eof(TFNTC01L);
728400100407             delete TFNTC;
728500100408             reade (appl:notkey:notke2:ntctnt) TFNTC01L;
728600100407           ENDDO;
728700100407         ENDIF;
728800100407
728900100407       ENDSR;
729000100407      /end-free
729100931118     C*
729200940705     C*--- MUOVO CAMPI DA VIDEO A TNTAM ------------------------------*
729300941012     C     RIETAM        BEGSR
729400980219     C*  VALORE POSITIVO DEI CAMPI NUMERI A VIDEO
729500980219     C                   MLLZO     1             COMDDT
729600980219     C                   MLLZO     1             COMDST
729700101013     C                   MLLZO     1             V1Crct
729800980219     C                   MLLZO     1             V1CPRI
729900980219     C                   MLLZO     1             V1CPAG
730000041129     C                   MLLZO     1             V1CPpa
730100980219     C                   MLLZO     1             V1CKPM
730200980219     C                   MLLZO     1             V1CMPC
730300980219     C                   MLLZO     1             V1CATA
730400980219     C                   MLLZO     1             V1CARL
730500980219     C                   MLLZO     1             V1CARF
730600980219     C                   MLLZO     1             V1CARO
730700980219     C                   MLLZO     1             V2CFIS
730800940518     C*
730900900327     C                   Z-ADD     COMDDT        TAMDDT
731000900327     C                   Z-ADD     COMDST        TAMDST
731100940526     C                   MOVE      V1CCTR        TAMCTR
731200940526     C                   MOVEL     V1CDCV        TAMDCV
731300101013     C                   Z-ADD     V1Crct        TAMrct
731400940705     C                   Z-ADD     V1CPRI        TAMPRI
731500980504     C                   Z-ADD     *ZEROS        TAMDIF
731600940705     C                   Z-ADD     V1CPAG        TAMPAG
731700041129     C                   Z-ADD     v1cppa        TAMPPA
731800980504     C                   Z-ADD     *ZEROS        TAMPAM
731900980504     C                   Z-ADD     *ZEROS        TAMMPA
732000980504     C                   Z-ADD     *ZEROS        TAMMPM
732100940526     C                   MOVEL     V1CTSP        TAMTSP
732200940526     C                   MOVEL     V1CBAP        TAMBAP
732300940526     C                   MOVEL     V1CFTP        TAMFTP
732400150923     c                   IF        V1Ctpr = 'N'
732500150923     c                   eval      TAMtpr = 'NO'
732600150923     c                   ELSE
732700150923     c                   clear                   TAMtpr
732800150923     c                   ENDIF
732900980505     C* VALORIZZO TAMFLO   MODO APPLICAZIONE TARIFFA IN FATTURA
733000980505     C* IN CASO DI IMMISSIONE PULISCO TAMFLO
733100980505     C   01              CLEAR                   TAMFLO
733200980505     C                   MOVE      TAMFLO        DSTA01
733300980505     C                   MOVE      V1CMAT        §TAF02
733400020201     C     V1CNET        IFEQ      'D'
733500020130     C                   MOVE      'S'           §TADPD
733600020130     C                   ENDIF
733700020201     C     V1CNET        IFEQ      'F'
733800020130     C                   MOVE      'S'           §TAFED
733900020130     C                   ENDIF
734000990617     C* VALORIZZO IL CAMPO DELLA DIVISA
734100990617     C                   MOVE      V1CDIV        §TADIV
734200020109     C*
734300020109     C                   MOVE      V1CPCA        §TAPCA
734400020109     C                   MOVE      V1CTAP        §TATAP
734500040601     c                   Movel     V2cbva        §tabva
734600050914     c                   Movel     v1covb        §taovb
734700020109     C*
734800980505     C                   MOVE      DSTA01        TAMFLO
734900940518     C*
735000940705     C* VEDO SE KG. O COLLI PER CALCOLO VOLUME AUTOMATICO
735100940518     C                   Z-ADD     0             TAMKPM
735200940518     C                   MOVEL     *BLANKS       TAMTIN
735300940526     C     V1CKPM        IFNE      0
735400940518     C                   MOVEL     'K'           TAMTIN
735500940526     C                   MOVEL     V1CKPM        TAMKPM
735600940518     C                   END
735700940526     C     V1CMPC        IFNE      0
735800940518     C                   MOVEL     'C'           TAMTIN
735900940526     C                   MOVEL     V1CMPC        TAMKPM
736000940518     C                   END
736100940518     C*
736200940526     C                   Z-ADD     V1CATA        TAMATA
736300940526     C                   Z-ADD     V1CARL        TAMARL
736400940526     C                   Z-ADD     V1CARF        TAMARF
736500940526     C                   Z-ADD     V1CARO        TAMARO
736600940705     C                   MOVEL     V2CFTM        TAMFTM
736700940705     C                   MOVEL     V2CFTS        TAMFTS
736800940526     C                   MOVEL     V1CFIE        TAMFIE
736900941108     C                   MOVEL     V1CFIE        WFIE
737000940705     C                   MOVEL     V2CNAS        TAMNAS
737100940705     C                   MOVEL     V2CNOT        TAMNOT
737200940705     C                   MOVEL     V2CFIS        TAMFIS
737300940705     C                   MOVEL     V2CSIS        TAMSIS
737400940804     C                   MOVEL     V2CFVD        TAMFVD
737500940705     C                   MOVEL     V2CGCA        TAMGCA
737600940705     C                   MOVEL     V2CGMA        TAMGMA
737700940705     C                   MOVEL     V2CGGA        TAMGGA
737800940705     C                   MOVEL     V2CGVA        TAMGVA
737900970128     C                   MOVEL     V2CBAM        TAMBAM
738000120319     c                   IF        v2Internet = 'NO'
738100120319     c                   eval      TAMeds = 'N'
738200120319     c                   ELSE
738300120319     c                   clear                   TAMeds
738400120319     c                   ENDIF
738500120529
738600120529       //?Se tariffa bloccata devo impostare a 'N' il flag
738700120529       //?visualizza su internet
738800150923     c                   IF        TAMbap = 'B' and
738900120529     c                             TAMbap <> sav_TAMbap
739000120529     c                   eval      TAMeds = 'N'
739100120529     c                   ENDIF
739200120529
739300060307     c                   eval      tamfmp = v2cfmp
739400990621     C* PULISCO  I CAMPI CHE NON VENGONO PIU' UTILIZZATI
739500990621     C                   CLEAR                   TAMARS
739600990621     C                   CLEAR                   TAMLAS
739700990621     C                   CLEAR                   TAMLRC
739800990621     C                   CLEAR                   TAMRAT
739900990621     C                   CLEAR                   TAMSC2
740000990617     C* VALORIZZO I CAMPI DELLE SPEDIZIONI MINIME E MASSIME AFFIDATECI
740100990824     C                   Z-ADD     V1CMIS        TAMLRC
740200990824     C                   Z-ADD     V1CMAS        TAMLAS
740300940518     C*
740400900327     C                   ENDSR
740500940518     C*
740600960109     C*--- MUOVO CAMPI DA TNTAM A VIDEO - 1° VIDETATA ----------------*
740700941012     C     RIED01        BEGSR
740800991021     C* TIPO TARIFFA  (SOLO PER COPIA DA OFFERTA)
740900991021     C                   MOVEL     TAMCTR        TIPTAR            1
741000940518     C*
741100940531     C****  DATA SCADENZA TARIFFA  ****
741200940518     C     TAMDDT        IFGT      0
741300940518     C     TAMDST        IFGT      0
741400940729     C                   Z-ADD     TAMDST        DATA
741500940729     C                   Z-ADD     MM            GGMM              4 0
741600940729     C                   MOVEL     GG            GGMM
741700940729     C                   Z-ADD     AA            V1CDST
741800940729     C                   MOVEL     GGMM          V1CDST
741900940518     C                   ENDIF
742000940518     C*
742100940531     C****  DATA DECORRENZA TARIFFA  ****
742200940729     C                   Z-ADD     TAMDDT        DATA
742300940729     C                   Z-ADD     MM            GGMM              4 0
742400940729     C                   MOVEL     GG            GGMM
742500940729     C                   Z-ADD     AA            V1CDDT
742600940729     C                   MOVEL     GGMM          V1CDDT
742700940518     C                   ENDIF
742800940518     C*
742900940531     C****  DATA ULTIMA VARIAZIONE  ****
743000940518     C     TAMDUV        IFGT      0
743100940729     C                   Z-ADD     TAMDUV        DATA
743200940729     C                   Z-ADD     MM            GGMM              4 0
743300940729     C                   MOVEL     GG            GGMM
743400940729     C                   Z-ADD     AA            V1CDUV
743500940729     C                   MOVEL     GGMM          V1CDUV
743600940518     C                   ENDIF
743700940518     C*
743800940518     C                   MOVEL     TAMDS         COMODO
743900030716      * se la ds COMODO$ non è valorizzata
744000030716     C                   if        COMODO$ = *blanks
744100030716     c                   movel     tamds         COMODO$
744200030716     c                   endif
744300030716      *
744400940518     C                   Z-ADD     TAMLAS        COMLAS
744500940518     C*
744600940526     C                   Z-ADD     PRG           V1CPRG
744700940526     C                   MOVE      TAMCTR        V1CCTR
744800940526     C                   MOVE      TAMDCV        V1CDCV
744900101013     C                   Z-ADD     TAMrct        V1Crct
745000101013     C                   Z-ADD     TAMrct        W_V1Crct
745100940705     C                   Z-ADD     TAMPRI        V1CPRI
745200101013    1C     V1Crct        IFGT      0
745300031118      *
745400031118      * visualizzo la decorrenza
745500101013     c                   z-add     dia(v1crct)   g02INV
745600031118     c                   movel     '3'           g02err
745700031118     c                   CALL      'XSRDA8'
745800031118     c                   PARM                    WLBDAT
745900031118      * UDATE A 8 IN GG/MM/AAAA
746000031118     c                   z-add     g02dat        v1diST
746100031118      *
746200031118     c                   endif
746300031118      *
746400940705     C                   Z-ADD     TAMPAG        V1CPAG
746500041129     C                   Z-ADD     TAMPpa        V1CPpa
746600980505     C                   MOVEL     TAMFLO        DSTA01
746700980505     C                   MOVE      §TAF02        V1CMAT
746800020130     C     §TADPD        IFEQ      'S'
746900020201     C                   MOVE      'D'           V1CNET
747000020130     C                   ENDIF
747100020130     C     §TAFED        IFEQ      'S'
747200020201     C                   MOVE      'F'           V1CNET
747300020130     C                   ENDIF
747400020215     C*
747500020215     C                   MOVE      V1CNET        WNET
747600020109     C*
747700020109     C                   MOVE      §TAPCA        V1CPCA
747800020109     C                   MOVE      §TATAP        V1CTAP
747900050914     c                   Movel     §taovb        v1covb
748000150922
748100150923     c                   IF        TAMtpr = 'NO'
748200150923     c                   eval      V1Ctpr = 'N'
748300150923     c                   ELSE
748400150923     c                   clear                   V1Ctpr
748500150923     c                   ENDIF
748600020109     C*
748700991021     C* DIVISA
748800991022     C                   MOVE      §TADIV        V1CDIV
748900990617     C                   MOVEL     'CV'          COD
749000990617     C                   MOVEL     *BLANKS       KEY
749100990622     C                   MOVEL     V1CDIV        KEY
749200990617     C     KTAB          CHAIN     TABEL                              32
749300990617     C  N32              MOVEL     TBLUNI        DSCV
749400980310     C*
749500940526     C                   MOVE      TAMTSP        V1CTSP
749600020215     C                   MOVE      TAMTSP        WTSP
749700940811     C                   MOVEL     '5E'          COD
749800940811     C                   MOVEL     *BLANKS       KEY
749900940811     C                   MOVEL     V1CTSP        KEY
750000940811     C     KTAB          CHAIN     TABEL                              32
750100090608     C  N32              MOVEL     TBLUNI        ds5e
750200090608     C  N32              MOVEL     §5ed08        V1DTSP
750300940811     C   32              CLEAR                   V1DTSP
750400940811     C*
750500120529     c                   eval      sav_TAMbap = TAMbap
750600130604     c                   eval      sav_TAMtsp = TAMtsp
750700940526     C                   MOVE      TAMBAP        V1CBAP
750800940526     C                   MOVE      TAMFTP        V1CFTP
750900940518     C     TAMTIN        IFEQ      'K'
751000940526     C                   Z-ADD     TAMKPM        V1CKPM
751100940518     C                   END
751200940518     C     TAMTIN        IFEQ      'C'
751300940526     C                   MOVEL     TAMKPM        V1CMPC
751400940518     C                   END
751500991021     C*
751600940913     C                   Z-ADD     TAMATA        V1CATA
751700940705     C                   Z-ADD     TAMARL        V1CARL
751800940526     C                   Z-ADD     TAMARF        V1CARF
751900940705     C                   Z-ADD     TAMARO        V1CARO
752000010330     C*
752100940518     C*
752200940526     C                   MOVEL     TAMFIE        V1CFIE
752300941108     C                   MOVEL     TAMFIE        WFIE
752400990617     C* VALORIZZO I CAMPI DELLE SPEDIZIONI MINIME E MASSIME AFFIDATECI
752500990824     C                   Z-ADD     TAMLRC        V1CMIS
752600990824     C                   Z-ADD     TAMLAS        V1CMAS
752700060307      * imposto qua il flag rinuncia AC Base xchè se F21 da prima videata devo
752800060307      * passarlo al pgm delle tariffe particolari
752900060307     c                   movel     tamfmp        v2cfmp
753000080522
753100080522      * controllo se esiste la tariffa particolare fuel
753200080522      * in caso contrario messaggio a video
753300080522     c                   clear                   ds1p
753400080522     c                   eval      cod = '1P'
753500080522     c                   eval      key = 'f '
753600080522     c     ktab          chain     tabel00f
753700080522     c                   if        %found(tabel00f) and tblflg = *blanks
753800080522     c                   eval      ds1p = tbluni
753900080522     c                   endif
754000080522     c                   select
754100080522      * Tariffa con tipo servizio poste
754200080522     c                   when      tamtsp = 'P' and §1pfpt = 'N'
754300080522      * Tariffa con Network DPD
754400080522     c                   when      §tadpd = 'S' and §1pfdp = 'N'
754500080522      * Tariffa con Network FedEx
754600080522     c                   when      §tafed = 'S' and §1pffe = 'N'
754700080522      * Tariffa Italia
754800080522     c                   when      tamfie = 'I' and §1ptco = 'N'
754900080522      * Per tariffa Estero controllo flag §1pfie
755000080522     c                   when      tamfie = 'E' and §1pfie = 'N'
755100080522      * Utilizzabile in tariffa
755200080522     c                   when      §1puta <> 'S'
755300080522     c                   other
755400080522     c                   eval      wtpar = 'f '
755500080522     c  n50ktam7         chain(n)  titpt01l
755600140221     c   50ktam7         chain(n)  tiopt01l
755700080522     c                   if        not %found or tptatb <> *blanks
755800080522     c                   eval      *in28 = *on
755900140220     c                   eval      V1Cmsg = msg(15)
756000080522     c                   endif
756100140220      * se trovo la tariffa Fuel Surcharge devo verificare se la data
756200140220      * di riferimento del prezzo base è > di oggi
756300140224     c                   exsr      ctr_fuel
756400080522    1c                   endsl
756500960109     C*
756600900327     C                   ENDSR
756700900329     C*
756800960109     C*--- MUOVO CAMPI DA TNTAM A VIDEO - 2° VIDETATA ----------------*
756900960109     C     RIED02        BEGSR
757000060303
757100060303     c                   Clear                   at2fmp
757200060303     c                   Clear                   h2riga
757300060303     c                   Clear                   h2colo
757400060303
757500960109     C                   MOVEL     TAMNAS        V2CNAS
757600021118     c                   Eval      *In39 = *Off
757700021118      * Se tariffa FedEx Documenti proteggo il campo della natura merce
757800021118     c                   If        V1cNet = 'F' and DueCtr >= §DfeDalD
757900021118     c                             and DueCtr <= §DfeAlD
758000021118     c                   Eval      *In39 = *On
758100021118     c                   EndIf
758200040601     c                   Movel     §tabva        v2cbva
758300960109     C                   MOVEL     TAMNOT        V2CNOT
758400960109     C                   MOVEL     TAMFIS        V2CFIS
758500960621     C  N50              MOVEL     TAMSIS        V2CSIS
758600960109     C                   MOVEL     TAMFVD        V2CFVD
758700960109     C                   MOVEL     TAMGCA        V2CGCA
758800960109     C                   MOVEL     TAMGGA        V2CGGA
758900960109     C                   MOVEL     TAMGMA        V2CGMA
759000960109     C                   MOVEL     TAMGVA        V2CGVA
759100960109     C                   MOVEL     TAMFTM        V2CFTM
759200960109     C                   MOVEL     TAMFTS        V2CFTS
759300970128     C                   MOVEL     TAMBAM        V2CBAM
759400120319     c                   IF        TAMeds = 'N'
759500120319     c                   eval      v2Internet = 'NO'
759600120319     c                   ELSE
759700120319     c                   clear                   v2Internet
759800120319     c                   ENDIF
759900120319
760000960109     C                   ENDSR
760100960109     C*
760200930630     C*--- LETTURA FILE ----------------------------------------------*
760300900329     C     LEGGI         BEGSR
760400900329     C*
760500900329     C* SE NON COPIO POSIZIONAMENTO  NORMALE
760600101213     C**n50KTAM2         SETLL     TNTAM01l
760700101213     c** 50ktam2         setll     tnofm01l
760800101213     C**n50KTAM2         READE     TNTAM01l                             3601
760900101213     c** 50ktam2         reade     tnofm01l                             3601
761000101213     C  n50KTAM2         chain     TNTAM01l                           0136
761100101213     c   50ktam2         chain     tnofm01l                           0136
761200941007     C*
761300941007     C* 36 ON  - RECORD ALLOCATO
761400950121    1C     *IN36         IFEQ      *ON
761500941012     C                   MOVEL     'A'           PARALL
761600950121   X1C                   ELSE
761700941012     C                   CLEAR                   PARALL
761800941007     C*
761900061010     C  N01TAMATB        COMP      'A'                                    01
762000061010     C  N01              SETON                                        10
762100950121    1C                   ENDIF
762200900329     C** 01  INSERIMENTO
762300900329     C** 10  AGGIORNAMENTO
762400900329     C                   ENDSR
762500900329     C*
762600930630     C*--- STAMPA TARIFFA --------------------------------------------*
762700920525     C     STAMPA        BEGSR
762800920525     C*
762900940310     C                   Z-ADD     PRG           PA2PRG
763000940310     C                   Z-ADD     VCTR          PA2CTR
763100940526     C                   MOVEL     V1DKSC        PA2RGS
763200940310     C* AMBIENTE
763300941013    2C     VFLG          IFNE      '1'
763400940310     C                   MOVEL     'C'           PA2AST
763500940526     C                   Z-ADD     V1CKSC        PA2CLI
763600940311     C                   Z-ADD     0             PA2NRV
763700941013   X2C                   ELSE
763800940310     C                   MOVEL     'V'           PA2AST
763900940526     C                   Z-ADD     V1CKSC        PA2NRV
764000940311     C                   Z-ADD     0             PA2CLI
764100941013    2C                   ENDIF
764200091021      * se trattativa devo impostare new tipo richiamo in pa2ast = 'T'
764300100129      * passo il potenziale e se c'è il cliente
764400091021     c   85              eval      pa2ast = 'T'
764500100112     c   85              eval      pa7cpo = cpox
764600100129     c                   if        *in85 and ksc2 > *zeros
764700100129     c                   eval      pa2cli = %dec(ksc2:7:0)
764800100129     c                   endif
764900101105
765000940311     C                   MOVEL     ' '           PA2TST
765100941013     C*
765200920525     C                   MOVEL     PARAM2        KPJBU
765300920525     C* SE RIF ALTRA TAR DA OFFERTA PRIMA PASSO DA OVRDBF
765400941219     C                   CALL      'FNLV43R'
765500920525     C                   PARM                    KPJBA
765600941013     C*
765700920525     C                   ENDSR
765800920525     C*
765900940510     C*--- CONTROLLO DATE --------------------------------------------*
766000940510     C     CTRDAT        BEGSR
766100940510     C*
766200940510     C                   ADD       1             TAMDST
766300940510     C                   Z-ADD     TAMDST        DATA
766400940510     C* CONTROLLO SE ANNO BISESTILE
766500940510    1C     MM            IFEQ      2
766600940510     C     AA            DIV       4             G02QUA            2 0
766700940510     C                   MVR                     G02RES            1 0
766800940510    2C     G02RES        IFEQ      0
766900940510     C                   Z-ADD     29            XDTF(2)
767000940510    2C                   ENDIF
767100940510    1C                   ENDIF
767200940510     C*
767300940510    2C     GG            IFGT      XDTF(MM)
767400940510     C     GG            SUB       XDTF(MM)      GG
767500940510     C                   ADD       1             MM
767600940510    2C     MM            IFEQ      13
767700940510     C                   ADD       1             AA
767800940510     C                   Z-ADD     1             MM
767900940510    2C                   ENDIF
768000940510    1C                   ENDIF
768100940510     C                   Z-ADD     28            XDTF(2)
768200940510     C*
768300940510     C                   ENDSR
768400980309     C*
768500980309     C*--- CONTROLLO TARIFFA IMPORT ----------------------------------*
768600980309     C     CTRIMP        BEGSR
768700980309     C*
768800980309     C                   MOVE      ' '           IMPORT            1
768900980309     C* CONTROLLO SE LA TARIFFA E' PER L'ITALIA O PER L'ESTERO
769000980309    1C     V1CFIE        IFEQ      'E'
769100980309     C* CONTROLLO SE ESISTE ALMENO UNA LINEA DI PARTENZA ESTERA PER DEFINIRE
769200980309     C* UNA TARIFFA IMPORT
769300980309     C* LEGGO IL DETTAGLIO TARIFFA
769400061011     C  n50KTAM2         SETLL     TITAD000
769500061011     c   50ktam2         setll     tiofd000
769600980309    2C                   DO        *HIVAL
769700061011     C  n50KTAM2         READE     TITAD000                               32
769800061011     c   50ktam2         reade     tiofd000                               32
769900980309    3C     *IN32         IFEQ      *OFF
770000980313     C     TADATB        ANDEQ     ' '
770100980309     C     TADLNP        CHAIN     AZORG01L                           33
770200020312     c                   clear                   og143
770300020312     c                   if        not *in33
770400020312     c                   eval      og143 = orgde3
770500020312     c                   endif
770600020312     c                   if        §ogntw = 'EEX' or §ogntw = 'EUP' or
770700020312     c                             §ogntw = 'FED'
770800020312     c                   eval      *in32 = *on
770900020312     c                   else
771000130318     c***                eval      *in31 = *off
771100130318     c                   eval      wTParEli = *off
771200130318     c                   endif
771300020312
771400980309     C* TARIFFA IMPORT 32 = ON
771500020312     C   32              MOVEL     'Y'           IMPORT
771600980309    3C                   END
771700980309     C*
771800980309    2C  N32              ENDDO
771900980309     C*
772000980309     C* CONTROLLO SE LA TARIFFA PARTICOLARE ASSICURAZIONE PER CONTO E' CORRETTA
772100980309     C* SOLO NEL CASO IN CUI NON SIA STATO RICHIESTO IL CMD 21 MANUTENZIONE
772200980309     C* TARIFFE PARTICOLARI
772300980309     C*
772400980309    2C     *INKV         IFEQ      *OFF
772500980313     C     IMPORT        ANDEQ     'Y'
772600060303     C                   MOVE      'C '          WTPAR
772700070129     C  n50KTAM7         CHAIN(n)  TITPT01L                           33
772800070129     c   50ktam7         chain(n)  tiopt01L                           33
772900980309    3C     *IN33         IFEQ      *OFF
773000980313     C     TPTATB        ANDEQ     ' '
773100980309    4C     TPTAPL        IFNE      'A'
773200980309     C     TPTVLM        OREQ      *ZEROS
773300980309     C                   SETON                                        28
773400980309     C                   MOVEL     MSG(72)       V1CMSG
773500980309    4C                   END
773600980309    3C                   END
773700980309    2C                   END
773800980309     C*
773900980309    1C                   END
774000980309     C*
774100980309     C                   ENDSR
774200000918     C*-------------------------------------------------------------------*
774300000918     C* INGLOBO TARIFFE DI CARTELLO CHE SONO SEMPRE PRESENTI IN TARIFFA
774400000918     C*-------------------------------------------------------------------*
774500000918     C*
774600000918     C     INGLO         BEGSR
774700061011
774800061011     c                   Clear                   Tnta25ds
774900061011     c  n50              Eval      ta25tipo = 'T'
775000061011     c   50              Eval      ta25tipo = 'O'
775100061011     c                   Eval      ta25ksco = v1cksc
775200061011     c                   Move      vctr          ta25ctro
775300061011     c                   Move      prg           ta25prgo
775400061011     c  n50              Eval      ta25tipn = 'T'
775500061011     c   50              Eval      ta25tipn = 'O'
775600061011     c                   Eval      ta25kscn = v1cksc
775700061011     c                   Move      vctr          ta25ctrn
775800061011     c                   Move      prg           ta25prgn
775900061011     c                   Eval      ta25tam = 'N'
776000061011     c                   Eval      ta25fie = v1cfie
776100061011     c                   Eval      ta25tam = 'N'
776200061011     c                   Eval      ta25tad = 'N'
776300061011     c                   Eval      ta25tpt = 'N'
776400061011     c                   Eval      ta25tpd = 'N'
776500061011     c                   Eval      ta25tgc = 'N'
776600061011     c                   Eval      ta25cat = 'N'
776700061011     c                   Eval      ta25inglo = 'S'
776800090917      * imposto se da trattativa
776900110907     c                   eval      %subst(kpjbu:1:1) = partrat
777000110907      * se sono in immissione oppure in aggiunta di una nuova tariffa
777100110907      * dico al TNTA25 di imglobare la tariffa particolare "Q" = ZTL
777200110907      *
777300120403       // -?Da marzo 2012 si ingloba sempre (DV 2236)
777400120403     c******             If        win01 or *in12
777500120403     c******             eval      ta25ztlq = 'S'
777600120403     c******             endif
777700061011
777800061011     c                   Call      'TNTA25R'
777900061011     c                   Parm                    kpjba
778000061011     c                   Parm                    Tnta25ds
778100061011     c                   Parm                    Tnta25tad
778200061011     c                   Parm                    Tnta25tgc
778300061011     c                   Parm                    Tnta25tpd
778400061011     c                   Parm                    Tnta25tpt
778500061011
778600000918     C                   ENDSR
778700080521
778800080521      *-------------------------------------------------------------------*
778900080521      * Integro la tariffa con la tariffa 'f' Fuel
779000080521      *-------------------------------------------------------------------*
779100080521     c     sr_fuel       begsr
779200080521
779300080521     c                   clear                   Tnta25ds
779400080521     c  n50              eval      ta25tipo = 'T'
779500080521     c   50              eval      ta25tipo = 'O'
779600080521     c                   eval      ta25ksco = v1cksc
779700080521     c                   move      vctr          ta25ctro
779800080521     c                   move      prg           ta25prgo
779900080521     c  n50              eval      ta25tipn = 'T'
780000080521     c   50              eval      ta25tipn = 'O'
780100080521     c                   eval      ta25kscn = v1cksc
780200080521     c                   move      vctr          ta25ctrn
780300080521     c                   move      prg           ta25prgn
780400080521     c                   eval      ta25tam = 'N'
780500080521     c                   eval      ta25fie = v1cfie
780600080521     c                   eval      ta25tam = 'N'
780700080521     c                   eval      ta25tad = 'N'
780800080521     c                   eval      ta25tpt = 'N'
780900080521     c                   eval      ta25tpd = 'N'
781000080521     c                   eval      ta25tgc = 'N'
781100080521     c                   eval      ta25cat = 'N'
781200080521     c                   eval      ta25inglo = 'f'
781300090917      * imposto se da trattativa
781400090917     c                   eval      %subst(kpjbu:1:1) = partrat
781500080521
781600080521     c                   call      'TNTA25R'
781700080521     c                   parm                    kpjba
781800080521     c                   parm                    Tnta25ds
781900080521     c                   parm                    Tnta25tad
782000080521     c                   parm                    Tnta25tgc
782100080521     c                   parm                    Tnta25tpd
782200080521     c                   parm                    Tnta25tpt
782300080521
782400080521     c                   endsr
782500080521
782600030715     C*---------------------------------------------------------------*
782700030715     C* ROUTINE DI SCRITTURA FILE STORICO TITAV00F
782800030715     C*---------------------------------------------------------------*
782900030715     C     SUB_STORICO   BEGSR
783000030717      * se sono in offerta oppure sono in immissione non scrivo nulla
783100040927      * non più ora si deve scrivere anche quando viene inserita una nuova
783200040927      * tariffa
783300030717     c                   if        not *in50 and instes = ' '
783400030715      *la prima volta inizializzo i campi del file che vanno sempre bene
783500030715     c                   if        first = ' '
783600030715     C                   eval      TAVksc= v1cksc
783700030715     C                   move      ctr2          TAVctr
783800030715     C                   eval      TAVprg= prg
783900030715      * data variazione
784000030715     C                   eval      TAVdav= dateu8
784100030715     C                   eval      TAVnoj= job_name
784200030715     C                   eval      TAVpru= user
784300030715     C                   eval      first = 'N'
784400030717     c                   endif
784500030717      *
784600030717     c                   clear                   TAVatb
784700040927     c                   If        annultar <> *Blanks
784800040927     c                   Eval      TavAtb = 'A'
784900040927     c                   EndIf
785000030715      * recupero ora di variazione
785100030715     c                   time                    w0120
785200030715     c                   movel     w0120         TAVorv
785300030715      * se non scrivo variazioni alle tariffe particolari pulisco il
785400030715      * codice tariffa particolare
785500030715     c                   if        TAVtrd <> 'PAR'
785600030715     c                   clear                   TAVftc
785700030715     c                   endif
785800040914      *
785900040914     c                   z-add     dateu8        TAVdtr
786000030715      *
786100030715     c                   write     titav000
786200030715     C*
786300030717     c                   endif
786400030717      *
786500030715     C                   ENDSR
786600030715     C***
786700041222     C*---------------------------------------------------------------*
786800041222     C* ROUTINE DI GESTIONE SUBFILE RICHIESTA LINEE PARTENZA DA CREARE
786900041222     C*---------------------------------------------------------------*
787000041222     C     SR_GESLNP     BEGSR
787100041222      *
787200041222      * SE È LA PRIMA VOLTA CHE ENTRO PULISCO IL SUBFILE
787300041222      *
787400041222     c                   IF        W_sbflnp = ' '
787500041222     c                   seton                                        26
787600041222     c                   write     TA01C09
787700041222     c                   setoff                                       26
787800041222     c                   z-add     1             nr2               4 0
787900041222     c                   z-add     1             REC2
788000061012     c                   eval      wforza = '0'
788100041222     c                   endif
788200041222      * zoccolo
788300041222     c                   write     ta01z10
788400041222      *
788500041222     c                   do        *hival
788600041222      *
788700061012     c                   if        nr2 > rec2
788800061012     c                   eval      rec2 = 1
788900061012     c                   endif
789000041222     c                   exfmt     ta01c09
789100041222      *
789200041222     c                   clear                   $msg
789300041222     c                   setoff                                       9828
789400041222      *
789500041222     c                   if        *inkl
789600041222     c                   leave
789700041222     c                   endif
789800041222      *
789900041222      * lettura subfile e controllo
790000041222      *
790100061012     c                   clear                   nr2
790200041222     c                   do        *hival
790300061012     c                   eval      nr2 = nr2 + 1
790400041222      *
790500061012     c     nr2           chain     ta01s09                            30
790600041222      *
790700061012     c                   if        *in30
790800041222     c                   leave
790900041222     c                   endif
791000061012
791100061012     c                   setoff                                       9828
791200041222      *
791300041222      * controllo la linea di partenza
791400041222      *
791500041222     c                   if        vs9lnp > 0
791600041222     c     vs9lnp        chain     azorg01l
791700041222     c                   if        not %found(azorg01l)
791800041222     C                   movel     MSG(22)       $MSG
791900041222     C                   seton                                        9828
792000061012     c                   z-add     nr2           rec2
792100041222     c                   update    ta01s09
792200041222     c                   leave
792300041222     c                   endif
792400041222      * verifico
792500041222     c                   if        orgfva <> ' '  or (orgfag <> 'A' and
792600041222     c                             orgfag <> 'F' and orgfl2 <> 'S')
792700041222     C                   movel     MSG(22)       $MSG
792800041222     C                   seton                                        9828
792900061012     c                   z-add     nr2           rec2
793000041222     c                   update    ta01s09
793100041222     c                   leave
793200041222     c                   endif
793300041222      *
793400041222     c                   movel     orgdes        deslnp
793500061012      * controllo se c'è già un dettaglio tariffario con la linea indicata
793600061012      * messaggio forzabile
793700061012     c                   clear                   wtrov
793800061012     c                   move      vs9lnp        comln2
793900061012     c  n50ktad2         setll     titad000
794000061012     c   50ktad2         setll     tiofd000
794100061012     c  n50ktad2         reade     titad000                               34
794200061012     c   50ktad2         reade     tiofd000                               34
794300061012     c                   dow       not *in34
794400061012     c                   if        tadatb = *blanks
794500061012     c                   eval      *in34 = *on
794600061012     c                   eval      wtrov = '1'
794700061012     c                   else
794800061012     c  n50ktad2         reade     titad000                               34
794900061012     c   50ktad2         reade     tiofd000                               34
795000061012     c                   endif
795100061012     c                   enddo
795200061012     c                   if        wtrov <> *blanks and wforza(nr2) = *off
795300061012     c                   eval      $msg = msg(68)
795400061012     c                   z-add     nr2           rec2
795500061012     c                   seton                                        9828
795600061012     c                   eval      wforza(nr2) = *on
795700061012     c                   update    ta01s09
795800061012     c                   leave
795900061012     c                   endif
796000090608
796100090608      * se la nuova linea di partenza è 950
796200090608    1c                   if        vs9lnp = 950
796300090608      * non posso duplicare se esiste un codice di tassazione IT o EE
796400090608     c                   clear                   wtrov
796500090608     c  n50ktam2         setll     titad04l
796600090608     c   50ktam2         setll     tiofd01l
796700090608    2c                   do        *hival
796800090608     c  n50ktam2         reade     titad04l
796900090608     c   50ktam2         reade     tiofd01l
797000090608     c                   if        %eof
797100090608     c                   leave
797200090608     c                   endif
797300090608     c                   if        tadatb <> *blanks
797400090608     c                   iter
797500090608     c                   endif
797600090608     c                   if        tadcts = 'IT' or tadcts = 'EE'
797700090608     c                   eval      wtrov = '1'
797800090608     c                   leave
797900090608     c                   endif
798000090608    2c                   enddo
798100090608     c                   if        wtrov <> *blanks
798200090608     c                   eval      $msg = msg(121)
798300090608     c                   eval      %subst($msg:36:3) = %editc(tadlnp:'1')
798400090608     c                   z-add     nr2           rec2
798500090608     c                   seton                                        9828
798600090608     c                   update    ta01s09
798700090608     c                   leave
798800090608     c                   endif
798900090608    1c                   endif
799000041222      *
799100041222     c                   else
799200041222      *
799300041222     c                   clear                   deslnp
799400041222      *
799500041222     c                   endif
799600041222      *
799700061012     c                   z-add     nr2           rec2
799800041222     c                   update    ta01s09
799900041222      *
800000041222     c                   enddo
800100041222      *
800200041222     c                   if        *in28
800300041222     c                   iter
800400041222     c                   endif
800500041222      * f6 conferma carico la mia schiera
800600041222     c                   If        *inkf
800700041222     c                   eval      W_sbflnp = '1'
800800041222      * schiera
800900041222     c                   clear                   lnpel
801000041222     c                   z-add     0             nr2
801100041222     c                   z-add     0             po                3 0
801200041222      *
801300041222     c                   do        120           nr2
801400041222     c     nr2           chain     ta01s09
801500041222     c                   if        %found and vs9lnp > 0
801600041222     c                   add       1             po
801700041222     c                   movea     vs9lnp        lnpel(po)
801800041222     c                   If        v8clnn = 0
801900041222     c                   movel     vs9lnp        v8clnn
802000041222     c                   endif
802100041222     c                   endif
802200041222     c                   enddo
802300041222      * se non ho caricato neppure una lnp pulisco W_SBFLNP
802400041222     c                   if        po = 0
802500041222     c                   clear                   W_sbflnp
802600041222     c                   endif
802700041222      *
802800041222     c                   eval      linpar = 'Selezionate più linee'
802900041222     c                   leave
803000041222     c                   endif
803100041222      *
803200041222     c                   enddo
803300041222
803400041222     c                   endsr
803500120314
803600120314      /free
803700120315       //--------------------------------------------------------------
803800120315       //?Imposto i tasti funzionali.
803900120315       //--------------------------------------------------------------
804000120315       BEGSR TastiFun;
804100120315
804200120315       //?Imposto la seconda riga variabile
804300120315         clear wpos;
804400120315         clear wposda;
804500120315         clear wpostf1;
804600120315         clear wpostf2;
804700120315         clear wpostf3;
804800120315         clear wrig;
804900120315         clear wtf1;
805000120315         clear wtf2;
805100120315         clear wtf3;
805200120315
805300120315         tf = 1;
805400120315         FOR tf by 1 to %elem(wTfv);
805500120315
805600120315         //?Abilito F1
805700120315           IF  not *in97 and
805800120315               %subst(wTfv(tf):1:3) = 'F1=';
805900120315             iter;
806000120315           ENDIF;
806100120315
806200120315         //?Abilito Enter
806300120319           IF  (wPrima or wSeconda) and
806400120315               %subst(wTfv(tf):1:3) = 'Ent';
806500120315             iter;
806600120315           ENDIF;
806700120315
806800120315         //?Abilito F2
806900120319           IF  (wQuarta or wTerza or not *in18) and
807000120315               %subst(wTfv(tf):1:3) = 'F2=';
807100120315             iter;
807200120315           ENDIF;
807300120315
807400120315         //?Abilito F6
807500120319           IF  (wPrima or wSeconda or wQuarta or *in15) and
807600120315               %subst(wTfv(tf):1:3) = 'F6=';
807700120315             iter;
807800120315           ENDIF;
807900120315
808000120315         //?Abilito F8 - Note tariffa
808100130905           IF  (wTerza or wQuarta or not *in18) and
808200120315               %subst(wTfv(tf):1:3) = 'F8=';
808300120315             iter;
808400120315           ENDIF;
808500120315
808600120315         //?Abilito F10
808700120315           IF  (wQuarta or (wPrima and not *in10)) and
808800120315               %subst(wTfv(tf):1:3) = 'F10';
808900120315             iter;
809000120315           ENDIF;
809100120315
809200120315         //?Abilito F11
809300120315           IF  (wPrima or wSeconda) and
809400120315               %subst(wTfv(tf):1:3) = 'F11';
809500120315             iter;
809600120315           ENDIF;
809700120315
809800120315         //?Abilito F16
809900130318          // IF  (wTerza or wQuarta or not *in10 or *in17) and
810000130318           IF  (wTerza or wQuarta or not *in31) and
810100120315               %subst(wTfv(tf):1:3) = 'F16';
810200120315             iter;
810300120315           ENDIF;
810400120315
810500120315         //?Abilito F18
810600120319           IF  (wTerza or wQuarta or (wPrima and not *in10) or
810700120319               not *in18 or *in50) and
810800120315               %subst(wTfv(tf):1:3) = 'F18';
810900120315             iter;
811000120315           ENDIF;
811100120315
811200120315           IF  wTfv(tf) <> *blanks;
811300120315             wpos = %scan('#':wTfv(tf));
811400120315             wpostf1 += wpos;
811500120315
811600120315             IF  wpostf1 <= %len(VZFd01);
811700120315               wposda = wpostf1 - wpos + 1;
811800120315               %subst(wtf1:wposda:(wpos-1)) = %subst(wTfv(tf):1:(wpos-1));
811900120315             ELSE;
812000120315               wpostf2 += wpos;
812100120315               IF  wpostf2 <= %len(VZFd01);
812200120315                 wposda = wpostf2 - wpos + 1;
812300120315                 %subst(wtf2:wposda:(wpos-1)) = %subst(wTfv(tf):1:(wpos-1));
812400120315               ELSE;
812500120315                 wpostf3 += wpos;
812600120315                 IF  wpostf3 <= %len(VZFd01);
812700120315                   wposda = wpostf3 - wpos + 1;
812800120315                   %subst(wtf3:wposda:(wpos-1)) = %subst(wTfv(tf):1:(wpos-1));
812900120315                 ENDIF;
813000120315               ENDIF;
813100120315             ENDIF;
813200120315
813300120315           ENDIF;
813400120315         ENDFOR;
813500120315
813600120315       //?Imposto F24 giusto
813700120315         SELECT;
813800120315           WHEN  wtf3 <> *blanks;
813900120315             VZFd02 = cf2413;
814000120315             wf24 = 3;
814100120315           WHEN  wtf2 <> *blanks;
814200120315             VZFd02 = cf2412;
814300120315             wf24 = 2;
814400120315           OTHER;
814500120315             clear VZFd02;
814600120315             wf24 = 1;
814700120315         ENDSL;
814800120315
814900120315         VZFd01 = wtf1;
815000120315         wrig = 1;
815100120315
815200120315       ENDSR;
815300120315
815400120315       //--------------------------------------------------------------
815500120315       //?Gestione tasto funzionale F24
815600120315       //?F24=Altri Tasti
815700120315       //--------------------------------------------------------------
815800120315       BEGSR gesf24;
815900120315
816000120315         SELECT;
816100120315           WHEN  (wf24 = 3 and wrig = 3) or
816200120315                 (wf24 = 2 and wrig = 2);
816300120315             VZFd01 = wtf1;
816400120315             wrig = 1;
816500120315             IF  wf24 = 2;
816600120315               VZFd02 = cf2412;
816700120315             ELSE;
816800120315               VZFd02 = cf2413;
816900120315             ENDIF;
817000120315
817100120315           WHEN  (wf24 = 3 and wrig = 1) or
817200120315                 (wf24 = 2 and wrig = 1);
817300120315             VZFd01 = wtf2;
817400120315             wrig = 2;
817500120315             IF  wf24 = 2;
817600120315               VZFd02 = cf2422;
817700120315             ELSE;
817800120315               VZFd02 = cf2423;
817900120315             ENDIF;
818000120315
818100120315           WHEN  wf24 = 3 and wrig = 2;
818200120315             VZFd01 = wtf3;
818300120315             wrig = 3;
818400120315             VZFd02 = cf2433;
818500120315
818600120315         ENDSL;
818700120315
818800120315       ENDSR;
818900120315
819000120314       //--------------------------------------------------------------
819100120314       //?Visualizza Dati manca tariffa-
819200120314       //--------------------------------------------------------------
819300120314       BEGSR GesW01;
819400120314
819500120314         wFine = *off;
819600120314         W01dsp = ISB48dsp;
819700120314         W01tbl = ISB48tbl;
819800120314         W01lnp = ISB48lnp;
819900120314         W01lna = ISB48lna;
820000120314         W01pro = ISB48pro;
820100120314         W01cts = ISB48cts;
820200120314         W01ctr = ISB48ctr;
820300120314         W01ncl = ISB48ncl;
820400120314         W01pkf = ISB48pkf;
820500120314         W01vlf = ISB48vlf;
820600120314         W01err = ISB48err;
820700120314
820800120314         DOW wFine = *off;
820900120314           exfmt  TA01W01;
821000120314           //?- F12 = Ritorno
821100120314           IF  *inkl;
821200120314             wFine = *on;
821300120314           ENDIF;
821400120314         ENDDO;
821500120314
821600120314       ENDSR;
821700120713
821800120713       //--------------------------------------------------------------
821900120713       //?Ricerca Codice Tassazione
822000120713       //--------------------------------------------------------------
822100120713       BEGSR Ric_CT;
822200120713
822300120713         clear TRTB09DS;
822400120713         TB09fun = '1';
822500120713         TB09ord = '1';
822600120713         IF  V1Cnet = 'D';
822700120713           TB09est = 'E';
822800120713         ELSE;
822900120713           TB09est = V1Cfie;
823000120713         ENDIF;
823100120713         TB09uta = 'S';
823200120713         IF  V1Cnet = 'F';
823300120713           TB09utf = 'S';
823400120713         ELSE;
823500120713           TB09utf = 'N';
823600120713         ENDIF;
823700120713         kpjbu = TRTB09DS;
823800120713         trtb09r (kpjba);
823900120713         TRTB09DS = kpjbu;
824000120713
824100120713       ENDSR;
824200140224
824300140224       //--------------------------------------------------------------
824400140224       //?Controllo tariffa Fuel.
824500140224       //--------------------------------------------------------------
824600140224       BEGSR Ctr_Fuel;
824700140224
824800140224         wNoOkFuel = *off;
824900140224         wtpar = 'f ';
825000140224         IF  not *in50;
825100140224           chain(n) (v1cksc:vctr:prg:wtpar) titpt01l;
825200140224         ENDIF;
825300140224         IF  *in50;
825400140224           chain(n) (v1cksc:vctr:prg:wtpar) tiopt01l;
825500140224         ENDIF;
825600140224
825700140224       //?Se trovo tariffa FUEL verifico che la data di riferimento
825800140224       //?del prezzo base non sia > di oggi
825900140224         IF  %found and TPTatb = *blanks and
826000140224             TPTdpb > dateu8;
826100140224           wNoOkFuel = *on;
826200140224         ENDIF;
826300140224
826400140224       ENDSR;
826500120713
826600120314      /end-free
826700120314
826800000000     C*---------------------------------------------------------------*
826900041222**   XDTF
827000000000312831303130313130313031
827100900320**   TES
827200900321 ***  GESTIONE TARIFFE CLIENTI  ***
827300900327 ***  GESTIONE  VISITE/OFFERTE  ***
827400940607**   MSG
827500940607Data Decorrenza Errata                                                         1
827600940607Data Scadenza Errata                                                           2
827700940607Data Scadenza Incongruente con Data Decorrenza                                 3
827800940607Data Scadenza Incongruente con Data Decorrenza Progressivo Seguente            4
827900940607Data Decorrenza Incongruente con Data Scadenza Progressivo Precedente          5
828000940607Immettere % Addebito Istat                                                     6
828100940607Cliente Esente: non immettere % Istat                                          7
828200120124La Data Scadenza non può essere maggiore di 5 anni da oggi!!                   8
828300940607Codice Tariffa Inesistente                                                     9
828400940607Immettere il Volume Automatico per KG. o per Colli                            10
828500940607Immettere o i KG. o i MC. per Colli per Volume Automatico                     11
828600940912Valore inferiore al minimo previsto dal Tipo Tariffa                          12
828700940607Gli Arrotondamenti devono essere o tutti pieni o tutti lasciati in bianco     13
828800090706Tipo tariffa non prevede il rapporto peso/vol. non immettere utilizzo peso VDL14
828900140220ATTENZIONE!!! Non è stata inserita la tariffa Fuel Surcharge.                 15
829000140226Prezzo base Fuel mai rilevato settimanalmente dal Ministero.                  16
829100110128Tariffa FedEx. Il rapp.peso/volume deve essere uguale su tutti gli scaglioni  17
829200061013                                                                              18
829300061013                                                                              19
829400120315F3 non consentito in Aggiunta Tariffa!!!                                      20
829500940607CMD12 non consentito in Aggiunta Tariffa !!                                   21
829600940607Filiale Inesistente                                                           22
829700940607Codice Tassazione Inesistente                                                 23
829800120906Immettere la filiale                                                          24
829900940607Con Tariffa Regione o Italia non deve essere inserito il C.A.P. del Paese     25
830000940607C.A.P. Incongruente con il Codice Tassazione inserito !!                      26
830100940607Se inserito il C.A.P. non deve esistere la Tariffa Provincia                  27
830200940607Scaglione Obbligatorio                                                        28
830300940607Tariffa Citta' Obbligatoria                                                   29
830400940607Tariffa gia' esistente                                                        30
830500940607L'Importo Massimo deve essere minore o uguale al Minimo                       31
830600940607Fine Scorrimento                                                              32
830700050616Immettere uno dei quattro campi della manipolazione                           33
830800940607Immettere almeno un Codice di Tassazione                                      34
830900940607Immettere lo Scaglione                                                        35
831000940607Codice Tassazione Errato                                                      36
831100940607Effettuare almeno una scelta                                                  37
831200070309Immettere almeno una tariffa particolare                                      38
831300070309Immesse tariffe particolari ma non richiesta la manipolazione                 39
831400070309Tariffa particolare ' ' non presente                                          40
831500070412La tariffa particolare ' ' è esclusa dalla manipolazione tariffe              41
831600940729Tipo Servizio Inesistente o Annullato                                         42
831700940811Natura Merce Obbligatoria                                                     43
831800950105Gestione Valore da Fatturare Obbligatorio                                     44
831900950112Impossibile inserire linea di partenza estera                                 45
832000950118Impossibile inserire scaglione con decimali                                   46
832100950112Impossibile inserire codice tassazione estero                                 47
832200950112Impossibile inserire sia linea partenza che codice tassazione italiani        48
832300940929Impossibile inserire tariffa preferenz. perchè già presente in altro cod.tar. 49
832400941122Codice Italia/Estero Obbligatorio                                             50
832500941122Codice Italia/Estero Errato                                                   51
832600061013                                                                              52
832700020205Impossibile variare flag Italia/Estero                                        53
832800061013                                                                              54
832900061013                                                                              55
833000950119Impossibile inserire gli arrotondamenti con decimali                          56
833100950119Impossibile inserire l'applicazione tariffa finita con decimali               57
833200061013                                                                              58
833300960520Rapporto peso/volume superiore al limite indicato in tabella: Enter x forzare 59
833400960522Immettere almeno una Linea di Partenza                                        60
833500960522Linea di Partenza XXX Inesistente                                             61
833600960523Non esistono righe di dettaglio da copiare per la selezione fatta !!          62
833700960524Se inserite solo le linee partenza impossibile digitarle uguali               63
833800960524Se inseriti sia le linee part. che i cod.tassaz. impossibile digitarli uguali 64
833900960524Immettere la linea partenza                                                   65
834000960524Immettere il codice tassazione                                                66
834100960524Attenzione!! La tariffa che si vuole creare esiste già: Enter x forzare       67
834200960527Attenzione!! Esistono già delle tariffe per la linea da creare:Enter x forzare68
834300960729Inserire arrotondam. perchè gli importi della tariffa non sono a percentuale  69
834400090608Non si puo' inserire una tariffa xxxxxxxx  "ESTERA"                           70
834500061013                                                                              71
834600980309ATTENZIONE !! Tariffa particolare Assic. per conto errata per tariffa IMPORT  72
834700980310E' inutile inserire MIN e/o MAX per scaglioni inferiori al minimo tassabile   73
834800980505Per questa tariffa non viene gestito l'arrotondamento                         74
834900980505Per questa tariffa non e' prevista la gestione del rapporto Peso / Volume     75
835000150922                                                                              76
835100150922                                                                              77
835200990617Il numero massimo delle spedizioni deve essere maggiore del minimo            78
835300990617Non sono ammessi importi con decimali                                         79
835400990617In caso di tariffa in percentuale bastano 2 decimali                          80
835500990623Nazione errata                                                                81
835600061013                                                                              82
835700991008L'arrotondamento deve essere un multiplo di 10                                83
835800991008L'arrotondamento non deve avere l'ultimo decimale diverso da zero             84
835900000118Per tariffe DPD obbligatorio indicare  "NO DDT"                               85
836000000118Non si puo' eliminare il servizio DPD !!!                                     86
836100000118Una tariffa estera non puo' essere dpd                                        87
836200090608Una tariffa DPD o FedEx deve essere soltanto  "xxxxxxxx"                      88
836300061013                                                                              89
836400130514Tipo servizio non può essere variato in manutenzione tariffa.                 90
836500090915Tipo servizio non utilizzabile in tariffa                                     91
836600090915
836700061013                                                                              93
836800010323Si può eseguire un solo arrotondamento                                        94
836900061013                                                                              95
837000150922                                                                              96
837100020130Non si puo' eliminare il servizio FedEx !!!                                   97
837200020130Una tariffa italia non puo' essere FedEx !!!                                  98
837300020205Impossibile inserire Network                                                  99
837400020207Codice tassazione non utlizzabile per tariffa FedEx                           00
837500020207Codice tassazione utilizzabile solo per tariffe FedEx                         01
837600110131Manipolazione Rapporto peso/volume tariffa FedEx. NO I/O su Scaglione!!       02
837700110131Manipolazione Rapporto peso/volume tariffa FedEx. NO I/O su cod.Tassazione!!  03
837800020312Non sono state inserite tariffe particolari, non è possibile la manipolazione 04
837900020318ATTENZIONE!! Manipolazione non eseguita per importi errati                    05
838000050616Effettuare almeno una scelta: dettaglio tariffe o tar.particolari o giacenza! 06
838100020328Per tariffa import/export immettere solo codice tassazione generico 'EE'      07
838200020404Se inserita la Nazione inserire anche il cap!                                 08
838300020404Per tariffa DPD immettere solo codice tassazione generico 'IT'                09
838400020624Linea di partenza 950 non inserire codice tassazione generico 'IT' o 'EE'     10
838500021118Tariffa FedEx non consentita per il codice tariffa inserito                   11
838600021118Scaglione superiore al peso consentito per questa tariffa: ENTER x FORZARE    12
838700130926MANCA SCAGLIONE ISTAT  AVVISARE PRESIDIO VENDITE !!!!!!!!!                    13
838800131029La % applicazione ISTAT non può essere minore di 50 o maggiore di 100         14
838900050530Esiste TarPartic."XX"da eliminare entro xx/xx/xx con F21 o modificare scadenza
839000050617Manipolazione non possibile se immesso un importo di aumento/dimunuzione      16
839100050617Non sono state inserite tariffe di giacenza, non è possibile la manipolazione 17
839200110131Manipolazione Rapporto peso/volume tariffa FedEx. NO I/O su Linea Partenza!!  18
839300060307Impossibile inserire la rinuncia. Presente AC BASE                            19
839400061207Effettuare solo una scelta per il rapporto peso/volume                        20
839500090608Non inserire LNP 950 perchè la LNP     ha codici di tassazione 'IT' o 'EE'    21
839600131029La % applicazione ISTAT non può essere < di 50 o > di 100. ENTER PER FORZARE  22
839700131113
839800120315
839900131028
840000120315** - wTfv (in ordine di visualizzazione) ------------------------------------*
840100120321F1=Manca Tariffa  #
840200120315Enter=Aggiorna  #
840300120315F3=Fine  #
840400120315F2=Rubrica  #
840500120315F6=Aggiorna e fine  #
840600120315F8=Note Tar.  #
840700120315F10=Stampa  #
840800120315F11=Altri Dati  #
840900120315F16=Annulla  #
841000120315F18=Note  #
841100120315F12=Ritorno   #
841200131028
841300131028
841400131028
841500131028
841600131028
841700131028
841800131028
841900131028
842000131028
