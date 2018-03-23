000100950113     H*---------------------------------------------------------------*
000200950301     H*      LISTA SPEDIZIONI CON AFFIDABILITA'  -  ESEGUE            *
000300930610     H*---------------------------------------------------------------*
000400930610     H* INDICATORI USATI                                              *
000500930610     H*---------------------------------------------------------------*
000600930719     H* 01 - ON  RICHIESTA STAMPA PER AFFIDABILITA' CLIENTI
000700931013     H* 02 - ON  RICHIESTA BOLLE CONSEGNATE, OFF NON CONSEGNATE
000800930719     H* 03 - ON  RICHIESTA BOLLE PER FILIALE
000900930719     H* 04 - ON  RICHIESTA BOLLE PER CLIENTE
001000930719     H* 05 - ON  RICHIESTA BOLLE ARRIVO
001100961122     H* 06 - RICHIESTA BOLLE ARRIVO E LINEA ARRIVO DI BOLLE EXPORT
001200961122     H*      QUINDI STAMPO LE DICITURE IN INGLESE
001300930616     H* 07 - RICHIESTO TIPO SPEDIZIONE
001400930719     H* 08 - OFF NESSUNA SPEDIZIONE IN RITARDO
001500930618     H* 09 - SE SERIE DIVERSA DA ZERO STAMPO IL TRATTINO
001600930615     H* 10 - BOLLA DA NON ELABORARE
001700960614     H* 11 - STAMPO RIFERIMENTO MITTENTE ALFABETICO
001800930621     H* 12 - STAMPO CONSEGNA RICHIESTA
001900960614     H* 13 - STAMPO C/ASSEGNO
002000930621     H* 14 - STAMPO MANCATA CONSEGNA
002100930623     H* 15 - CHIAMATO IL PROGRAMMA TRSE98R
002200931013     H* 16 - ON STAMPA ORE RITARDO E DATA CONSEGNA TEORICA
002300931013     H* 17 - ON RICHIESTA SPED.CONSEGNATE E NON CONSEGNATE
002400931014     H* 18 - ON SELEZIONE SPEDIZIONI IN ANTICIPO |
002500931014     H* 19 - ON SELEZIONE SPEDIZIONI IN RITARDO  | 18 19 OFF: TUTTE
002600050621     H* 20 - richeista data spedizione
002700960614     H* 21 - ON NON VISUALIZZO LE ORE DI ANTICIPO PERCHE' RICHIESTA LA
002800960614     H*         STAMPA DELLE SOLE ORE DI RITARDO
002900961212     H* 22 - STAMPO LA DATA TRASMISSIONE (SOLO PER BOLLE EXPORT)
003000980519     H* 23 - ON SI TRATTA DI RICHIESTA ARRIVO E LINEA ARRIVO DI BOLLE EXPORT
003100980519     H*      CIRCUITO SDI-EUROPOLITAN
003200020212      * 24 - ON SI TRATTA DI RICHIESTA ARRIVO E LINEA ARRIVO DI BOLLE EXPORT
003300020212      *      Network FedEx
003400980209     H* 25 - ON RICHIESTA BOLLE PER DATA TRASMISSIONE
003500050622     H* 26 - ON RICHIESTA differenze pesi e volumi vdl
003600050622     H* 27 - sped in fermo deposito
003700050623     H* 28/29 - peso e volumi vdl non presenti in bolla
003800930615     H* 30 - LETTURA TABELLE
003900930615     H* 31 - RICERCA CODICE BOLLA
004000930615     H* 32 - LETTURA BOLLE ARRIVO E BOLLE PARTENZA
004100930615     H* 33 - LETTURA ESTENSIONE BOLLE ARRIVO
004200930616     H* 34 - LETTURA ORGANIGRAMMA AZIENDALE
004300930616     H* 35 - LETTURA ANAGRAFICO CLIENTI
004400050623     H* 80 - richiesta diff di peso vdl
004500050623     H* 81 - richiesta diff di volume vdl
004600050623     H* 82 - stampo differenza di peso o volume vdl
004700051107     H* 83 - on f.deposito sabato  off-f.deposito normale
004800070515     H* 84 - stampo affidabilità partenza solo per bolle partenza
004900070515     H* 85 - richiesta affidabilità partenza
005000130114     H* 86 - richiesta file excel
005100130115     H* 87 - richiestO RAGGRUPPAMENTO CLIENTI
005200930610     F*---------------------------------------------------------------*
005300930610     F* DATA BASE                                                     *
005400930610     F*---------------------------------------------------------------*
005500950113     H DECEDIT('0,') DATEDIT(*DMY.)
005600950113     F*------------
005700950113     F* P A R T E N Z A
005800950113     F*------------
005900950303     FFNBLP34L  IF   E           K DISK    USROPN
006000950303     F                                     RENAME(FNBLP000:FNBLP034)
006100950303     FFNBLP37L  IF   E           K DISK    USROPN
006200010717     FFNBLP31L  IF   E           K DISK    USROPN
006300010717     F                                     RENAME(FNBLP000:FNBLP031)
006400950113     F*------------
006500950113     F* A R R I V O
006600950113     F*------------
006700950303     FFNARB54L  IF   E           K DISK    USROPN
006800950303     FFNARB26L  IF   E           K DISK    USROPN
006900950113     F                                     RENAME(FNARB000:FNARB026)
007000980209     FFNARB55L  IF   E           K DISK    USROPN
007100980209     F                                     RENAME(FNARB000:FNARB055)
007200991104     FFIAR601L  IF   E           K DISK    USROPN
007300051115     FFiAR901L  IF   E           K DISK    USROPN
007400021203     FFIAPD01L  IF   E           K DISK    USROPN
007500060215     FFIAR401L  IF   E           K DISK    USROPN
007600930616     FAZORG01L  IF   E           K DISK
007700930616     FCNACO00F  IF   E           K DISK
007800930611     FTABEL00F  IF   E           K DISK
007900121220     Ftntbe01l  IF   E           K DISK
008000990809     FFNEVB04L  IF   E           K DISK
008100130114     Fwfpkc01l  uF a E           k DISK
008200950303     FPRTF198   O    F  198        PRINTER OFLIND(*INOF)
008300950303     F                                     USROPN
008400000621     F                                     FORMLEN(066)
008500000621     F                                     FORMOFL(062)
008600930610     D*---------------------------------------------------------------*
008700930610     D* SCHIERE                                                       *
008800930610     D*---------------------------------------------------------------*
008900950116     D*------------------
009000950116     D* TESTATA STAMPA
009100950116     D*------------------
009200050704     D ST              S             66    DIM(39) CTDATA PERRCD(1)
009300950116     D*------------------
009400950116     D* DESCRIZIONE TIPO SPEDIZIONE
009500950116     D*------------------
009600090604     D TIP             S              4    DIM(11) CTDATA PERRCD(1)
009700950116     D*------------------
009800950116     D* PARAMETRI IN STAMPA
009900950116     D*------------------
010000130115     D SPA             S             42    DIM(31) CTDATA PERRCD(1)
010100930610     D*------------------
010200931013     D* FILIALI DA ELABORARE
010300930610     D*------------------
010400940331     D LIN             S              3  0 DIM(30)
010500930614     D*------------------
010600930614     D* TIPI BOLLA DOPPIA
010700930614     D*------------------
010800060830     D DOP             S              2    DIM(50)
010900930614     D*------------------
011000930614     D* TIPO BOLLA ASSEGNATO
011100930614     D*------------------
011200060830     D ASS             S              2    DIM(50)
011300930623     D*------------------
011400930623     D* TIPO BOLLA RECUPERO
011500930623     D*------------------
011600060830     D REC             S              2    DIM(50)
011700931117      *------------------
011800931117      * RIEPILOGO CONSEGNE PARTICOLARI
011900931117      *------------------
012000931117     D P§N             S              1    DIM(50)                              CD.CONSEGNA PART.
012100931117     D P§S             S              9  0 DIM(50)                              NR.SPEDIZIONI
012200931117     D P§C             S              9  0 DIM(50)                              TOTALE COLLI
012300931117     D P§P             S             10  1 DIM(50)                              TOTALE PESO (KG)
012400950116     D P§V             S             10  3 DIM(50)                              TOTALE VOLUME (MC
012500931117      *------------------
012600931117      * RIEPILOGO SCAGLIONI DI RITARDO
012700931117      *------------------
012800931117     D P§O             S              3  0 DIM(169)                             ORE RITARDO
012900931117     D P§R             S              9  0 DIM(169)                             NR.TOT.RITARDI
013000931118     D P§L             S              9  0 DIM(169)                             TOTALE COLLI
013100931118     D P§K             S             10  1 DIM(169)                             TOTALE PESO (KG)
013200950116     D P§M             S             10  3 DIM(169)                             TOTALE VOLUME (MC
013300950303     D*
013400130114     D skicli          S              7    DIM(900)                             OVRPRTF
013500121221
013600121221     D CMD             S             80    DIM(1) CTDATA PERRCD(1)              OVRPRTF
013700930719     L*---------------------------------------------------------------*
013800950113     L* ESTENSIONI                                                    *
013900950113     L*---------------------------------------------------------------*
014000930610     D*---------------------------------------------------------------*
014100950113     D* INPUT                                                         *
014200930610     D*---------------------------------------------------------------*
014300020215
014400020215     d wawbfed         s             12
014500020607     d sav_parpd1      s                   like(parpd1)
014600020607     d sav_parpd2      s                   like(parpd2)
014700021203
014800021203     d kpdpdr          S                   LIKE(APDpdr)
014900021203     d kpdtip          S                   LIKE(APDtip)
015000021203
015100020607     d wpopdc1         s              3  0
015200020607     d wpopdc2         s              3  0
015300050621     d
015400050621     d vidvra          s                   like(parvra)
015500050623     d statipodif      s             15
015600050704     d sceltadif       s             26
015700050704     d stadpv          s             29
015800050704     d stadvv          s             29
015900050621     d viddpv          s              1
016000050623     d viddvv          s              1
016100050621     d stafvc          s              1
016200130114     d Indx            s              4  0
016300130114     d xx              s              4  0
016400130114     d contarec        s              7  0
016500050621     d
016600050621     d wncr            s                   like(arbncr)
016700050621     d wncl            s                   like(arbncl)
016800050622     d $pesovdl        s                   like($peso)
016900050622     d $volumvdl       s                   like($volum)
017000050623     d $todiffpeso     s             14  1
017100050623     d $todiffvolu     s             12  3
017200130114     d dateu_u         s              8  0
017300130114     d ora_u           s              6  0
017400130114     d unmese          s              8  0
017500130114     d Datasys         s               d   datfmt(*iso)  inz(*sys)
017600130114     d Ora_sys         s               t   timfmt(*iso)  inz(*sys)
017700130114     d Ora_iso         s               t   timfmt(*iso)
017800130114     d Dataiso         s               d   datfmt(*iso)
017900130114     d
018000121220     D kcod            S                   LIKE(tbecod) inz('KPV')
018100121220     D kke1            S                   LIKE(tbeke1)
018200130115     D kke2            S                   LIKE(tbeke2)
018300130115     d
018400130115     D  pInFILE        s             10
018500130115     D  pInIDJOB       s             26
018600130115     D  pInPWDXLS      s             15
018700130115     D  pInNOMXLS      s             78
018800130115     D  pInEMLDEST     s            121
018900130115     D  pInEMLVAR      s             80
019000130115     D  pInMRAKE2      s             15
019100130115     D  pOutESITO      s              1
019200020215
019300950113     D*------------------
019400950113     D* ARCHITETTURA
019500950113     D*------------------
019600950113     D KPJBA         E DS
019700130114     D cncr80        E DS
019800050623     d
019900050623     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
020000930610     D*------------------
020100950113     D* PARAMETRI LANCIO
020200930610     D*------------------
020300950113     D PARAM           DS
020400950113     D  VIDBPA                 1      1
020500950113     D  VIDSPC                 2      2
020600950301     D  P§RDAL                 3      7P 0
020700950301     D  P§RAL                  8     12P 0
020800950301     D  R§VDAL                13     17P 0
020900950301     D  R§VAL                 18     22P 0
021000950301     D  PARLNP                23     24P 0
021100950301     D  PARLNA                25     26P 0
021200950301     D  P§RKCI                27     29P 0
021300950301     D  PARCLI                30     33P 0
021400950301     D  PARGIO                34     35P 0
021500950301     D  PARFEL                36     37P 0
021600050623     D  PARmag                38     39
021700130111     d*  indico se devo leggere il codice cliente o il raggruppaoemnto
021800121220     D  PARragr               40     40
021900130111     d* richiesta file excel
022000130111     d  videxcel              41     41
022100130111     d*         --> l'indirizzo e-mail nel file di appoggio
022200130115     d  parprg_kpjbu          42     45P 0
022300130111     d
022400950301     D  LNP                   58    117P 0
022500940331     D                                     DIM(30)
022600950301     D  LNA                  118    177P 0
022700940331     D                                     DIM(30)
022800950301     D  VIDTIP               178    178
022900950301     D  VIDACL               179    179
023000950301     D  VIDACO               180    180
023100050621     D  parVRA               181    181
023200950301     D  VIDSRA               182    182
023300950301     D  V§DDSD               183    187P 0
023400950301     D  V§DDSA               188    192P 0
023500950301     D  R§VDSD               193    197P 0
023600950301     D  R§VDSA               198    202P 0
023700950301     D  VIDBDS               203    203
023800950302     D  PARELA               204    204
023900950302     D  PARTOT               205    205
024000950302     D  PARPD1               206    209P 0
024100950302     D  PARPD2               210    213P 0
024200950302     D  PARFPP               214    214
024300950302     D  PARNRT               215    215
024400950302     D  V§DDR1               216    220P 0
024500950302     D  V§DDR2               221    225P 0
024600070302     D  parnzd               226    228
024700070515     D  VIDACP               229    229
024800960614     D  PARRIT               236    236
024900980209     D  V§DDTD               237    241P 0
025000980209     D  V§DDTA               242    246P 0
025100070302     D  parcad               247    255
025200940331     D*-------------------
025300950113     D* CONTROLLO DATA (8)
025400940331     D*-------------------
025500950113     D WLBDAT          DS
025600950113     D  G02DAT                 1      8  0
025700950113     D  G02INV                 9     16  0
025800950113     D  G02ERR                17     17
025900950113     D  G02TGI                18     22  0
026000950113     D*------------------
026100950113     D* CONTROLLO DATA (6)
026200950113     D*------------------
026300950113     D WLBDA6          DS
026400950113     D  G06DAT                 1      6  0
026500950113     D  G06INV                 7     12  0
026600950113     D  G06ERR                13     13
026700950113     D  G06TGI                14     18  0
026800950113     D WGIDAT          DS
026900950113     D  GIODAT                 1      6  0
027000950113     D  GIOINV                 7     12  0
027100950113     D  GIOTGI                13     17  0
027200950113     D*------------------
027300950113     D* COMPOSIZIONE DATA SPEDIZIONE
027400950113     D*------------------
027500940331     D                 DS
027600950113     D  BLPAAS                 1      4  0
027700950113     D  BLPMGS                 5      8  0
027800950113     D  BLPDSP                 1      8  0
027900940331     D                 DS
028000950113     D  ARBAAS                 1      4  0
028100950113     D  ARBMGS                 5      8  0
028200950113     D  ARBDSP                 1      8  0
028300930614     D*------------------
028400930614     D* DS CODICI BOLLA
028500930614     D*------------------
028600930614     D DS3A          E DS
028700930615     D*------------------
028800930615     D* DS CODICI EVENTI
028900930615     D*------------------
029000930615     D DS2A          E DS
029100930615     D*------------------
029200930615     D* DS CONSEGNE PARTICOLARI
029300930615     D*------------------
029400930615     D DS1P          E DS
029500930615     D*------------------
029600930615     D* DS CALCOLO TEMPI DI DELIVERY
029700930615     D*------------------
029800970925     D DS99          E DS                  EXTNAME(TNSD99DS) INZ
029900000620     D*------------------
030000000620     D* DS CONTROLLO SE LNP = POSTEY
030100000620     D*------------------
030200000620     D OG143         E DS
030300051124     D*------------------
030400051124     D fnlv80ds      E DS
030500090604     d ds5e          e ds
030600130111     d trul01ds      e ds
030700130118     d
030800130118     D DSQT1         E DS
030900130118     d wTara           s                   Like(§QtTpc)
031000130118     d wNtara          s                   Like(blpPkf)
031100130118
031200931013     D*------------------
031300931013     D* DEFINIZIONE COSTANTI
031400931013     D*------------------
031500931014     D CTUTTE          C                   CONST('CONSEGNATE E NON CON-
031600931013     D                                     SEGNATE')
031700931013     D CCON            C                   CONST('SOLO CONSEGNATE     ')
031800931013     D CNOCON          C                   CONST('SOLO NON CONSEGNATE ')
031900931014     D CRITAR          C                   CONST('IN RITARDO ')
032000931014     D CANTIC          C                   CONST('IN ANTICIPO')
032100931014     D CRTANT          C                   CONST('TUTTI      ')
032200940527     D CDIS            C                   CONST('SOLO IN DISTINTA    ')
032300940527     D CNODIS          C                   CONST('SOLO NON IN DISTINTA')
032400940527     D CTUT            C                   CONST('IN DISTINTA E NON IN-
032500940527     D                                      DISTINTA')
032600950301     D CAFFI1          C                   CONST('CON AFFIDABILITA''')
032700950301     D CAFFI2          C                   CONST('SENZA AFFIDABILITA''')
032800950307     D CMAPO1          C                   CONST('MATTINO    ')
032900950301     D CMAPO2          C                   CONST('POMERIGGIO ')
033000950301     D CELAB1          C                   CONST('DATA CONS/SPED')
033100961122     D CELAB2          C                   CONST('AUTOTRASPORT. ')
033200950303     D CTOTA1          C                   CONST('IN DETTAGLIO   ')
033300950303     D CTOTA2          C                   CONST('SOLO TOTALI    ')
033400961122     D CTOTA3          C                   CONST('PARZ.AUTOTRASP.')
033500130115     d xlsE            c                   const('Indirizzo e-mail per +
033600130115     d                                     invio file excel:')
033700130115     d xlsF            c                   const('Excel troppo grande:+
033800130115     d                                     posto nella cartella')
033900130115     C*---------------------------------------------------------------*
034000130115      /copy gaitrasrc/srcprotopr,trtcm8r1
034100930610     C*---------------------------------------------------------------*
034200950113     C* MAIN LINE                                                     *
034300930610     C*---------------------------------------------------------------*
034400930614     C*
034500931013     C* 17 ON         - RICHIESTE SPEDIZIONI CONSEGNATE E NON
034600931013     C* 17 OFF 02 ON  - RICHIESTE SOLO BOLLE CONSEGNATE
034700931013     C* 17 OFF 02 OFF - RICHIESTE SOLO BOLLE NON CONSEGNATE
034800950116     C* 03 ON         - RICHIESTE BOLLE PER FILIALE
034900950116     C* 04 ON         - RICHIESTE BOLLE PER CLIENTE
035000950116     C* 05 ON         - RICHIESTE BOLLE ARRIVO
035100950303     C*
035200950303     C* DA QUI IL PROGRAMMA SI DIVIDE IN 2:
035300950303     C* ..  1)   ELABORAZIONE PER PADRONCINO (SOLO PARTENZE)
035400950303     C* ..  2)   ELABORAZIONE PER DATA CONS./SPED.
035500950303     C*-------------------------------------------------------------------------
035600950303     C*
035700950303     C* ?  1)    P E R    P A D R O N C I N O    ( SOLO PARTENZE )  ?
035800950303     C*
035900130115IF  0C     PARELA        IFEQ      'P'                                          'P'=PADRONCINO
036000950303     C     *IN05         ANDEQ     *OFF                                         N05=PARTENZA
036100950303     C                   OPEN      FNBLP34L
036200051115     C                   OPEN      Fiar901L
036300021203     C*                  OPEN      FNAPD01L
036400021203     C                   OPEN      FIAPD01L
036500020607
036600020607      * Devo elaborare solo gli autotrasportatori in gestione al p.o. di lancio
036700020607 b1  c                   do        30            i
036800020607     c                   if        lin(i) = *zeros
036900020607     c                   leave
037000020607     c                   endif
037100020607     c                   movel     sav_parpd1    wpopdc1
037200020607     c                   movel     sav_parpd2    wpopdc2
037300020607      * range di autotrasportatori
037400020607 b2  c                   if        sav_parpd1 <> *zeros and sav_parpd2 <> *zeros
037500020607      * imposto codice "dal"
037600020607     c                   if        wpopdc1 < lin(i)
037700020607     c                   clear                   parpd1
037800020607     c                   movel     lin(i)        parpd1
037900020607     c                   endif
038000020607     c                   if        wpopdc1 = lin(i)
038100020607     c                   eval      parpd1 = sav_parpd1
038200020607     c                   endif
038300020607     c                   if        wpopdc1 > lin(i)
038400020607     c                   iter
038500020607     c                   endif
038600020607      * imposto codice "al"
038700020607     c                   if        wpopdc2 < lin(i)
038800020607     c                   iter
038900020607     c                   endif
039000020607     c                   if        wpopdc1 = lin(i)
039100020607     c                   eval      parpd2 = sav_parpd2
039200020607     c                   endif
039300020607     c                   if        wpopdc2 > lin(i)
039400020607     c                   eval      parpd2 = *all'9'
039500020607     c                   movel     lin(i)        parpd2
039600020607     c                   endif
039700020607 e2  c                   endif
039800021021      * Solo 1 autotrasportatore
039900021021 b2  c                   If        sav_parpd1 > *zeros and sav_parpd2 = *zeros
040000021021     c                   if        wpopdc1 <> lin(i)
040100021021     c                   iter
040200021021     c                   endif
040300021021 e2  c                   EndIf
040400950303     C*
040500950303     C     KFID4         SETLL     FNBLP34L
040600950303     C* SOLO UN PADRONCINO ?
040700950303IF  1C     PARPD1        IFNE      *ZERO                                        COD.PADR.INIZIALE
040800950303     C     PARPD2        ANDEQ     *ZERO                                        COD.PADR.FINALE
040900950303     C* CI SONO LE DATE RITIRO ?
041000950303IF  2C     PARDR1        IFNE      *ZERO                                        DATA RITIRO INIZIALE
041100950303     C     PARDR2        ANDEQ     *ZERO                                        DATA RITIRO FINALE
041200950303     C     KFID4         READE     FNBLP34L                               32     SOLO 1 PAD.E 1 DATA
041300950303   X2C                   ELSE
041400950303     C     PARPD1        READE     FNBLP34L                               32     SOLO 1 PADR.
041500950303E   2C                   ENDIF
041600950303   X1C                   ELSE
041700950303     C                   READ      FNBLP34L                               32     PIU' PADR.
041800950303E   1C                   ENDIF
041900950303     C*
042000950303     C* ?  L E T T U R A    B O L L E  ?
042100950303     C*
042200950303DO  1C     *IN32         DOWEQ     *OFF
042300950303     C*
042400950303     C* STAMPA INTESTAZIONE
042500950303IF  2C     *INOF         IFEQ      '1'
042600950303     C                   EXCEPT    TES1                                         TESTATA STAMPA
042700950303IF  3C     PARTOT        IFEQ      'D'                                          'D'=DETTAGLIO
042800950303     C                   EXCEPT    TES
042900950303E   3C                   ENDIF
043000950303     C                   SETOFF                                       OF
043100950303E   2C                   ENDIF
043200950303     C*
043300950303     C* CONTROLLO SPEDIZIONI DA STAMPARE
043400950303     C                   EXSR      ESCLUS
043500950303     C*
043600950303     C* SE NON CI SONO ESCLUSIONI STAMPO ELENCO AFFIDABILITA SPEDIZIONI
043700950303     C  N10              EXSR      STAMPA
043800950303     C*
043900950303     C* ?  N U O V A    L E T T U R A  ?
044000950303     C*
044100950303IF  2C     *IN32         IFEQ      *OFF
044200950303     C* SOLO UN PADRONCINO ?
044300950303IF  3C     PARPD1        IFNE      *ZERO                                        COD.PADR.INIZIALE
044400950303     C     PARPD2        ANDEQ     *ZERO                                        COD.PADR.FINALE
044500950303     C* CI SONO LE DATE RITIRO ?
044600950303IF  4C     PARDR1        IFNE      *ZERO                                        DATA RITIRO INIZIALE
044700950303     C     PARDR2        ANDEQ     *ZERO                                        DATA RITIRO FINALE
044800950303     C     KFID4         READE     FNBLP34L                               32     SOLO 1 PAD.E 1 DATA
044900950303   X4C                   ELSE
045000950303     C     PARPD1        READE     FNBLP34L                               32     SOLO 1 PADR.
045100950303E   4C                   ENDIF
045200950303   X3C                   ELSE
045300950303     C                   READ      FNBLP34L                               32     PIU' PADR.
045400950303E   3C                   ENDIF
045500950303     C*
045600950303E   2C                   ENDIF
045700950303E   1C                   ENDDO
045800020607
045900020607 e1  c                   enddo                                                   do lin
046000950303     C*
046100950303     C*-------------------------------------------------------------------------
046200950303   X0C                   ELSE
046300950303     C*-------------------------------------------------------------------------
046400950303     C* ?  2)    P E R    D A T A    C O N S. / S P E D.  ?
046500950303     C*
046600950303     C     VIDBPA        IFEQ      'P'                                          'P'=PARTENZE
046700950303     C                   OPEN      FNBLP37L
046800010717     C                   OPEN      FNBLP31L
046900051115     C                   OPEN      Fiar901L
047000950303     C                   ELSE                                                   'A'=ARRIVI
047100950303     C                   OPEN      FNARB54L
047200950303     C                   OPEN      FNARB26L
047300991104     C                   OPEN      FIAR601L
047400060215     C                   OPEN      FiAR401L
047500051115     C                   OPEN      FiAR901L
047600980209     C                   OPEN      FNARB55L
047700950303     C                   ENDIF
047800930615     C*
047900931013     C* FINCHE' CI SONO FILIALI IN LIN
048000930618     C                   Z-ADD     1             I
048100931013DO  1C     LIN(I)        DOWGT     0
048200940331     C                   Z-ADD     LIN(I)        PARFIL            3 0
048300931013     C                   ADD       1             I
048400931013     C*
048500931013     C* SE DEVO ELABORARE SIA LE NON CONSEGNATE CHE LE CONSEGNATE
048600931013     C* FACCIO 2 GIRI DI ELABORAZIONE
048700950116    2C     *IN17         IFEQ      *ON                                          *TUTTE E DUE
048800950116     C                   Z-ADD     1             WELA              1 0          *DUE GIRI
048900980209     C* NEL CASO IN CUI LEGGO PER DATA TRASMISSIONE FACCIO UN GIRO SOLO
049000980209     C   25              Z-ADD     2             WELA                           *UN GIRO SOLO
049100980513     C  N25              SETOFF                                       02        *PRIMA LE NON CONSEG
049200931013   X2C                   ELSE
049300950116     C                   Z-ADD     2             WELA                           *UN GIRO SOLO
049400931013    2C                   ENDIF
049500931013     C***
049600931013    2C     WELA          DO        2
049700950116     C*
049800950116     C* SE DEVE LEGGERE LE CONSEGNATE E LA DATA SPEDIZIONE RICHIESTA E' UGUALE A
049900950116     C* ZERO, SI POSIZIONA PER DATA CONSEGNA MERCE UGUALE A UNO PERCHE' NEL FILE
050000950116     C* CI SONO SIA LE CONSEGNATE CHE LE NON CONSEGNATE (E QUESTE VENGONO PRIMA)
050100950116     C     *LIKE         DEFINE    ROVDAL        WSETLL
050200950116IF  3C     *IN02         IFEQ      *ON                                          *CONSEGNATE
050300950116IF  4C     ROVDAL        IFEQ      *ZEROS                                       *DATA RICHIESTA = 0
050400950116     C                   Z-ADD     1             WSETLL
050500950116X   4C                   ELSE                                                   *DATA RICHIESTA > 0
050600950116     C                   Z-ADD     ROVDAL        WSETLL
050700950116E   4C                   ENDIF
050800950116E   3C                   ENDIF
050900980209     C*
051000980209     C* ?  S P E D I Z I O N I    T R A S M E S S E    ?
051100980209     C*
051200980209     C*  LEGGO IN ORDINE DI LINEA DI ARRIVO E DATA TRASMISSIONE
051300980209IF  3C     WTRAS         IFEQ      'T'
051400980209     C     KARB55        SETLL     FNARB55L
051500980209     C     PARLNA        READE     FNARB55L                               32
051600980209X   3C                   ELSE
051700950303     C*
051800950303     C* ?  S P E D I Z I O N I    C O N S E G N A T E  ?
051900930623     C*
052000950303     C*?LEGGO PER --> DATA CONSEGNA?
052100940331    4C   02WLCON         IFEQ      'C'
052200930623     C* BOLLE PARTENZA
052300950113     C  N05KFID          SETLL     FNBLP37L
052400950113     C  N05PARFIL        READE     FNBLP37L                               32
052500930615     C*
052600930623     C* BOLLE ARRIVO
052700950113     C   05KFID          SETLL     FNARB54L
052800950113     C   05PARFIL        READE     FNARB54L                               32
052900940331     C*
053000940331   X4C                   ELSE
053100940331     C*
053200950303     C*?LEGGO PER --> DATA SPEDIZIONE?
053300940331     C* BOLLE PARTENZA
053400010717     C  N05KFID3         SETLL     FNBLP31L
053500010717     C  N05PARFIL        READE     FNBLP31L                               32
053600940331     C*
053700940331     C* BOLLE ARRIVO
053800950113     C   05KFID3         SETLL     FNARB26L
053900950113     C   05PARFIL        READE     FNARB26L                               32
054000940331    4C                   ENDIF
054100930623     C*
054200950303     C* ?  S P E D I Z I O N I    N O N    C O N S E G N A T E  ?
054300930623     C*
054400950303     C*?LEGGO PER --> DATA CONSEGNA?
054500940331    4C  N02WLNOC         IFEQ      'C'
054600930623     C* BOLLE ARRIVO
054700950113     C   05KFID2         SETLL     FNARB54L
054800950113     C   05KFID2         READE     FNARB54L                               32
054900930623     C*
055000930623     C* BOLLE PARTENZA
055100950113     C  N05KFID2         SETLL     FNBLP37L
055200950113     C  N05KFID2         READE     FNBLP37L                               32
055300940331     C*
055400940331   X4C                   ELSE
055500940331     C*
055600950303     C*?LEGGO PER --> DATA SPEDIZIONE?
055700940331     C* BOLLE PARTENZA
055800010717     C**N05      KFID3     SETLLFNBLP31C
055900010717     C**N05      PARFIL    READEFNBLP31C                 32
056000010717     C  N05KFID3         SETLL     FNBLP31L
056100010717     C  N05PARFIL        READE     FNBLP31L                               32
056200940331     C*
056300940331     C* BOLLE ARRIVO
056400950113     C   05KFID3         SETLL     FNARB26L
056500950113     C   05PARFIL        READE     FNARB26L                               32
056600940331E   4C                   ENDIF
056700980209     C*
056800980209E   3C                   ENDIF
056900940331     C*
057000950303     C* ?  L E T T U R A    B O L L E  ?
057100940331     C*
057200931013DO  3C     *IN32         DOWEQ     *OFF
057300930621     C*
057400930621     C* STAMPA INTESTAZIONE
057500931013IF  4C     *INOF         IFEQ      '1'
057600931117     C                   EXCEPT    TES1                                         TESTATA STAMPA
057700950302     C     PARTOT        IFEQ      'D'                                          'D'=DETTAGLIO
057800930621     C                   EXCEPT    TES
057900950302     C                   ENDIF
058000930621     C                   SETOFF                                       OF
058100931013E   4C                   ENDIF
058200930617     C*
058300930617     C* CONTROLLO SPEDIZIONI DA STAMPARE
058400930617     C                   EXSR      ESCLUS
058500930615     C*
058600930720     C* SE NON CI SONO ESCLUSIONI STAMPO ELENCO AFFIDABILITA SPEDIZIONI
058700930615     C  N10              EXSR      STAMPA
058800930615     C*
058900950303     C* ?  N U O V A    L E T T U R A  ?
059000940331     C*
059100940331IF  4C     *IN32         IFEQ      *OFF
059200980209     C*
059300980209     C* ?  S P E D I Z I O N I    T R A S M E S S E    ?
059400980209     C*
059500980209     C*  LEGGO IN ORDINE DI LINEA DI ARRIVO E DATA TRASMISSIONE
059600980209IF  5C     WTRAS         IFEQ      'T'
059700980209     C     PARLNA        READE     FNARB55L                               32
059800980209X   5C                   ELSE
059900950303     C*
060000950303     C* ?  S P E D I Z I O N I    C O N S E G N A T E  ?
060100940331     C*
060200950303     C*?LEGGO PER --> DATA CONSEGNA?
060300940331    6C   02WLCON         IFEQ      'C'
060400940331     C* BOLLE PARTENZA
060500950113     C  N05PARFIL        READE     FNBLP37L                               32
060600940331     C* BOLLE ARRIVO
060700950113     C   05PARFIL        READE     FNARB54L                               32
060800940331     C*
060900940331   X6C                   ELSE
061000940331     C*
061100950303     C*?LEGGO PER --> DATA SPEDIZIONE?
061200940331     C* BOLLE PARTENZA
061300010717     C**N05      PARFIL    READEFNBLP31C                 32
061400010717     C  N05PARFIL        READE     FNBLP31L                               32
061500940331     C* BOLLE ARRIVO
061600950113     C   05PARFIL        READE     FNARB26L                               32
061700940331    6C                   ENDIF
061800940331     C*
061900950303     C* ?  S P E D I Z I O N I    N O N    C O N S E G N A T E  ?
062000940331     C*
062100950303     C*?LEGGO PER --> DATA CONSEGNA?
062200940331    6C  N02WLNOC         IFEQ      'C'
062300940331     C* BOLLE ARRIVO
062400950113     C   05KFID2         READE     FNARB54L                               32
062500940331     C* BOLLE PARTENZA
062600950113     C  N05KFID2         READE     FNBLP37L                               32
062700940331     C*
062800940331   X6C                   ELSE
062900940331     C*
063000950303     C*?LEGGO PER --> DATA SPEDIZIONE?
063100940331     C* BOLLE PARTENZA
063200010717     C**N05      PARFIL    READEFNBLP31C                 32
063300010717     C  N05PARFIL        READE     FNBLP31L                               32
063400940331     C* BOLLE ARRIVO
063500950113     C   05PARFIL        READE     FNARB26L                               32
063600940331E   6C                   ENDIF
063700980209     C*
063800980209E   5C                   ENDIF
063900950303     C*
064000931013E   4C                   ENDIF
064100930623     C*
064200931013E   3C                   ENDDO
064300931013     C   17              SETON                                        02
064400931013E   2C                   ENDDO
064500931013E   1C                   ENDDO
064600950303     C*
064700950303     C*-------------------------------------------------------------------------
064800950303     C*
064900950303E   0C                   ENDIF
065000950303     C*
065100950303     C* ?  F I N E    E L A B O R A Z I O N I  ?
065200930615     C*
065300930621     C* PER NESSUNA SPEDIZIONE IN RITARDO STAMPO MESSAGGIO
065400930621     C  N08              EXCEPT    MES
065500950303     C*
065600950303     C* DEVO STAMPARE L'ULTIMO PARZIALE PER PADRONCINO
065700950303     C     PARELA        IFEQ      'P'                                          'P'=PADRONCINO
065800950303     C     *IN05         ANDEQ     *OFF                                         N05=PARTENZA
065900950303     C     PARTOT        IFEQ      'D'                                          'D'=DETTAGLIO
066000950303     C     PARTOT        OREQ      'P'                                          'P'=PARZIALI
066100950303     C                   EXCEPT    TOTPAD
066200950303     C                   ENDIF
066300950303     C                   ENDIF
066400931117      *
066500931117      * STAMPO I TOTALI GENERALI
066600050623     c* Eseguo differenze di peso
066700931117     C                   EXCEPT    FIN1                                         TOTALI COLONNE
066800931117      *
066900931117      * STAMPO IL RIEPILOGO CONSEGNE PARTICOLARI
067000931117     C                   EXSR      CONPAR                                       CONS.PARTICOLAR
067100931117      *
067200931117      * STAMPO IL RIEPILOGO SCAGLIONI DI RITARDI, SOLO SE RICHIESTI
067300950302     C     VIDVRA        IFEQ      'S'                                          SE RICHESTO
067400931117     C                   EXSR      SCARIT                                       SCAG.RITARDI
067500931117     C                   ENDIF
067600130115     c
067700130115     c* Prima di fine stampa creo il file excel o il file in disco N
067800130115     c                   clear                   staEM            50
067900130115     c                   clear                   staINVIO         40
068000130115     c   86              exsr      Fexcel
068100930621     C*
068200930621     C* F I N E  STAMPA
068300930621     C                   EXCEPT    FIN
068400930621     C*
068500930623     C* CALL AL PGM TRSE98R PER CHIUDERE I FILE (SE RICHIAMATO)
068600930623IF  1C     *IN15         IFEQ      '1'
068700930623     C                   MOVEL     'C'           D98TLA
068800970925     C                   CALL      'TNSD99R'
068900970925     C                   PARM                    DS99
069000930623E   1C                   ENDIF
069100930623     C*
069200930610     C                   SETON                                        LR
069300930615     C*---------------------------------------------------------------*
069400950113     C* ESCLUS - ESCLUSIONE BOLLE DALLA STAMPA                        *
069500930615     C*---------------------------------------------------------------*
069600930615     C     ESCLUS        BEGSR
069700930615     C*
069800930623     C                   SETOFF                                       103132
069900930615     C                   Z-ADD     *ZEROS        COMCLI            7 0
070000930719     C                   Z-ADD     *ZEROS        COMHRI            4 0
070100930622     C*
070200940331     C* ESCLUDO I RECUPERI PERCHE' MOMENTANEAMENTE ERRATI
070300060830     C  N05              MOVEL     BLPCBO        DUECBL            2
070400950113     C   05              MOVEL     ARBCBO        DUECBL
070500930622     C*
070600930623     C     DUECBL        LOOKUP    REC                                    31
070700930623IF  1C     *IN31         IFEQ      '1'
070800930623     C                   SETON                                        10
070900931014     C                   GOTO      ENDESC
071000930623E   1C                   ENDIF
071100950116     C*
071200950116     C* SE CHIESTE LE SPED.NI CONSEGNATE ESCLUDO LE BOLLE CON DATA CONSEGNA = 0
071300950116IF  1C     *IN02         IFEQ      *ON                                          CONSEGNATE
071400950116IF  2C     *IN05         IFEQ      *OFF                                         PARTENZA
071500950116IF  3C     BLPDCM        IFEQ      *ZEROS                                       DATA CONSEGNA MERCE=
071600950116     C                   SETON                                        10
071700950116     C                   GOTO      ENDESC
071800950116E   3C                   ENDIF
071900950116E   2C                   ENDIF
072000950116E   1C                   ENDIF
072100930615     C*
072200950303     C* SONO SEMPRE DI FRONTE A 2 STRADE:
072300950303     C* .. 1)  STO LEGGENDO PER PADRONCINO (SOLO PARTENZE)
072400950303     C* .. 2)   "     "      "  DATA CONS.SPED.
072500950303IF  0C     PARELA        IFEQ      'P'                                          'P'=PADRONCINO
072600950303     C     *IN05         ANDEQ     *OFF                                         N05=PARTENZA
072700950303     C*----------------------------------------------------------------
072800950303     C*?1)  STO LEGGENDO PER PADRONCINO (SOLO PARTENZE)?
072900950306     C*
073000950306     C* SOLO LE CONSEGNATE
073100950306     C     VIDSPC        IFEQ      'S'                                          'S'=SOLO CONSEGNATE
073200950306     C     BLPDCM        ANDEQ     *ZERO                                        DATA CONSEGNA
073300950306     C                   SETON                                        10
073400950306     C                   GOTO      ENDESC
073500950306     C                   ENDIF
073600950306     C*
073700950306     C* SOLO LE NON CONSEGNATE
073800950306     C     VIDSPC        IFEQ      'N'                                          'N'=SOLO NON CONSEGN
073900950306     C     BLPDCM        ANDNE     *ZERO                                        DATA CONSEGNA
074000950306     C                   SETON                                        10
074100950306     C                   GOTO      ENDESC
074200950306     C                   ENDIF
074300950303     C*
074400950303     C* IN QUESTO CASO, LE DATE DI CONSEGNA / SPEDIZIONE DEVO
074500950303     C* CONSIDERARLE COME SEMPLICI ESCLUSIONI
074600950303     C* BOLLE ARRIVO
074700950303     C* SE C'E' LA DATA CONSEGNA DAL/AL
074800950306IF  1C     R§VDAL        IFGT      *ZERO
074900950303     C* BOLLE PARTENZA
075000950306IF  2C     BLPDCM        IFLT      ROVDAL
075100950303     C     BLPDCM        ORGT      ROVAL
075200950303     C                   SETON                                        10
075300950303     C                   GOTO      ENDESC
075400950303E   2C                   ENDIF
075500950303E   1C                   ENDIF
075600950303     C* SE C'E' LA DATA SPEDIZIONE DAL/AL
075700950306IF  1C     R§VDSD        IFGT      *ZERO
075800950303     C* BOLLE PARTENZA
075900950306IF  2C     BLPDSP        IFLT      ROVDSD
076000950303     C     BLPDSP        ORGT      ROVDSA
076100950303     C                   SETON                                        10
076200950303     C                   GOTO      ENDESC
076300950303E   2C                   ENDIF
076400950303E   1C                   ENDIF
076500950303     C*
076600950303     C*----------------------------------------------------------------
076700950303     C                   ELSE
076800950303     C*----------------------------------------------------------------
076900980209     C* STO LEGGENDO PER DATA TRASMISSIONE
077000980210IF A1C     *IN25         IFEQ      *ON
077100980210     C* CONTROLLO LIMITE MASSIMO DATA TRASMISSIONE
077200980210IF  2C     ARBDT3        IFGT      ROVDTA
077300980209     C                   SETON                                        1032
077400980209     C                   GOTO      ENDESC
077500980210E   2C                   ENDIF
077600980210     C* ESCLUDO IL RECORD SE DATA CONSEGNA NON E' NEI LIMITI
077700980211     C* SE RICHIESTE SOLO LE CONSEGNATE
077800980210IF  2C     ROVDAL        IFGT      0
077900980211     C     VIDSPC        ANDEQ     'S'
078000980210     C     ARBDCM        COMP      ROVAL                              10
078100980211     C  N10ARBDCM        COMP      ROVDAL                               10
078200980210     C   10              GOTO      ENDESC
078300980210E   2C                   ENDIF
078400980211     C* ESCLUDO IL RECORD SE DATA CONSEGNA MAGGIORE DI 0
078500980211     C* SE RICHIESTE SOLO LE NON CONSEGNATE
078600980211IF  2C     ARBDCM        IFGT      0
078700980211     C     VIDSPC        ANDEQ     'N'
078800980211     C                   SETON                                        10
078900980211     C                   GOTO      ENDESC
079000980211E   2C                   ENDIF
079100980211     C* ESCLUDO SOLO SE DATA CONSEGNA NON = A 0 NON E' TRA I LIMITI
079200980211     C* SE RICHIESTE TUTTE CONSEGNATE E NON
079300980211IF  2C     ROVDAL        IFGT      0
079400980211     C     VIDSPC        ANDEQ     ' '
079500980211     C     ARBDCM        ANDNE     0
079600980211     C     ARBDCM        COMP      ROVAL                              10
079700980211     C  N10ARBDCM        COMP      ROVDAL                               10
079800980211     C   10              GOTO      ENDESC
079900980211E   2C                   ENDIF
080000980210     C* ESCLUDO IL RECORD SE DATA SPEDIZIONE NON E' NEI LIMITI
080100980210IF  2C     ROVDSA        IFGT      0
080200980210     C     ARBDSP        COMP      ROVDSD                               10
080300980211     C  N10ARBDSP        COMP      ROVDSA                             10
080400980210     C   10              GOTO      ENDESC
080500980210E   2C                   ENDIF
080600980210     C*
080700980210X  A1C                   ELSE
080800980209     C*
080900950303     C*?2)  STO LEGGENDO PER DATA CONS./SPED.?
081000950303     C*
081100940331     C* STO LEGGENDO --> LE SPEDIZIONI CONSEGNATE
081200940331     C*                  PER DATA CONSEGNA
081300940331     C*  SE C'E' DATA FACCIO ESCLUSIONI
081400931014    1C     *IN02         IFEQ      *ON
081500940331     C     WLCON         ANDEQ     'C'
081600940331     C     ROVDAL        ANDGT     0
081700930615     C*
081800930615     C* BOLLE ARRIVO
081900940331IF  2C   05ARBDCM        IFGT      ROVAL
082000980210     C                   SETON                                        1032
082100931014     C                   GOTO      ENDESC
082200940331E   2C                   ENDIF
082300930615     C* BOLLE PARTENZA
082400940331IF  2C  N05BLPDCM        IFGT      ROVAL
082500930615     C                   SETON                                        1032
082600931014     C                   GOTO      ENDESC
082700940331E   2C                   ENDIF
082800940331E   1C                   ENDIF
082900940331     C*
083000940331     C* STO LEGGENDO --> LE SPEDIZIONI CONSEGNATE O NON CONSEGNATE
083100940331     C*                  PER DATA SPEDIZIONE
083200940331    1C     *IN02         IFEQ      *ON
083300940331     C     WLCON         ANDEQ     'S'
083400940331     C*
083500940331     C     *IN02         OREQ      *OFF
083600940331     C     WLNOC         ANDEQ     'S'
083700940331     C*
083800940331     C* BOLLE ARRIVO
083900940331IF  2C   05ARBDSP        IFGT      ROVDSA
084000980210     C                   SETON                                        1032
084100940331     C                   GOTO      ENDESC
084200940331E   2C                   ENDIF
084300940331     C* BOLLE PARTENZA
084400940331IF  2C  N05BLPDSP        IFGT      ROVDSA
084500940331     C                   SETON                                        1032
084600940331     C                   GOTO      ENDESC
084700940331E   2C                   ENDIF
084800940331     C*
084900940405     C* DATA CONSEGNA COME ESCLUSIONE   --> PER LE CONSEGNATE
085000940405    2C   02ROVDAL        IFGT      0
085100940331     C*
085200940331     C* BOLLE ARRIVO
085300940331IF  3C   05ARBDCM        IFLT      ROVDAL
085400940331IF   C     ARBDCM        ORGT      ROVAL
085500940331     C                   SETON                                        10
085600940331     C                   GOTO      ENDESC
085700940331E   3C                   ENDIF
085800940331     C* BOLLE PARTENZA
085900940331IF  3C  N05BLPDCM        IFLT      ROVDAL
086000940331IF   C     BLPDCM        ORGT      ROVAL
086100940331     C                   SETON                                        10
086200940331     C                   GOTO      ENDESC
086300940331E   3C                   ENDIF
086400940331E   2C                   ENDIF
086500940405     C*
086600940405     C* DATA CONSEGNA = 0           --> PER LE NON CONSEGNATE
086700940405     C   05
086800940405    2CANN02ARBDCM        IFNE      0
086900940405     C                   SETON                                        10
087000940405     C                   GOTO      ENDESC
087100940405E   2C                   ENDIF
087200940405     C*
087300940405     C  N05
087400940405    2CANN02BLPDCM        IFNE      0
087500940405     C                   SETON                                        10
087600940405     C                   GOTO      ENDESC
087700940405E   2C                   ENDIF
087800940331E   1C                   ENDIF
087900940331     C*
088000940331     C* SE STO LEGGENDO PER DATA CONSEGNA -->
088100940331     C* DATA SPEDIZIONE COME ESCLUSIONE
088200940331    1C     *IN02         IFEQ      *ON
088300940331     C     WLCON         ANDEQ     'C'
088400940331     C*
088500940331     C     *IN02         OREQ      *OFF
088600940331     C     WLNOC         ANDEQ     'C'
088700940331     C*
088800940331    2C     *IN20         IFEQ      *ON
088900940331     C* BOLLE ARRIVO
089000940331IF  4C   05ARBDSP        IFLT      ROVDSD
089100940331IF  4C     ARBDSP        ORGT      ROVDSA
089200940331     C                   SETON                                        10
089300940331     C                   GOTO      ENDESC
089400940331E   4C                   ENDIF
089500940331     C* BOLLE PARTENZA
089600940331IF  4C  N05BLPDSP        IFLT      ROVDSD
089700940331IF  4C     BLPDSP        ORGT      ROVDSA
089800940331     C                   SETON                                        10
089900940331     C                   GOTO      ENDESC
090000940331E   4C                   ENDIF
090100940331E   2C                   ENDIF
090200940331E   1C                   ENDIF
090300950303     C*
090400980210E  A1C                   ENDIF
090500950303     C*----------------------------------------------------------------
090600950303E   0C                   ENDIF
090700940331     C*
090800940601     C* BOLLE IN DISTINTA O NON
090900940527     C*
091000940527IF  1C     *IN05         IFEQ      *ON
091100940601     C     *IN02         ANDEQ     *OFF
091200940527IF  2C     VIDBDS        IFEQ      'S'
091300940527     C     ARBNDC        ANDEQ     0
091400940527     C                   SETON                                        10
091500940527     C                   GOTO      ENDESC
091600940527E   2C                   ENDIF
091700940527     C*
091800940527IF  2C     VIDBDS        IFEQ      'N'
091900940527     C     ARBNDC        ANDGT     0
092000940527     C                   SETON                                        10
092100940527     C                   GOTO      ENDESC
092200940527E   2C                   ENDIF
092300940527E   1C                   ENDIF
092400940527     C*
092500940331     C* PARZ PER LINEA DI PARTENZA
092600930615     C*
092700940331    1C     VIDBPA        IFEQ      'A'
092800940331     C     PARLNP        ANDGT     0
092900940331     C*
093000940331     C   05ARBLNP        IFNE      PARLNP
093100940331     C                   SETON                                        10
093200940331     C                   GOTO      ENDESC
093300940331E   4C                   ENDIF
093400940331     C  N05BLPLNP        IFNE      PARLNP
093500940331     C                   SETON                                        10
093600940331     C                   GOTO      ENDESC
093700940331E   4C                   ENDIF
093800940331E   1C                   ENDIF
093900940331     C*
094000940331     C* PARZ PER LINEA DI ARRIVO
094100940331     C*
094200940331    1C     VIDBPA        IFEQ      'P'
094300940331     C     PARLNA        ANDGT     0
094400940331     C*
094500940331     C   05ARBLNA        IFNE      PARLNA
094600940331     C                   SETON                                        10
094700940331     C                   GOTO      ENDESC
094800940331E   4C                   ENDIF
094900940331     C  N05BLPLNA        IFNE      PARLNA
095000940331     C                   SETON                                        10
095100940331     C                   GOTO      ENDESC
095200940331E   4C                   ENDIF
095300940331E   1C                   ENDIF
095400940331     C*
095500000620     C* SE SCELTE TUTTE LE LINEE PARTENZA ESCLUDO POSTE
095600000620     C     PARLNP        IFEQ      0
095700000620     C   05ARBLNP        CHAIN     AZORG01L                           31
095800000620     C  N05BLPLNP        CHAIN     AZORG01L                           31
095900000620     C     *IN31         IFEQ      *OFF
096000000620     C                   MOVEL     ORGDE3        OG143
096100020212     C**!!!§OGPT         IFEQ      'S'
096200020212     c                   if        §ogntw = 'PPT'
096300000620     C                   SETON                                        10
096400000620     C                   GOTO      ENDESC
096500000620     C                   ENDIF
096600000620     C                   ENDIF
096700000620     C                   ENDIF
096800000620     C*
096900000620     C* SE IMMESSO IL CLIENTE ESCLUDO LE BOLLE NON SUE
097000931014IF  1C     *IN04         IFEQ      '1'
097100930615     C*
097200930615     C* BOLLE PARTENZA
097300930615IF  2C     *IN05         IFEQ      '0'
097400950116     C     DUECBL        LOOKUP    ASS                                    31
097500930615     C* ASSEGNATO
097600930615IF  3C     *IN31         IFEQ      '1'
097700960402     C* SE IL CODICE RICHIESTO E' 9999 PRENDO SEMPRE BLPKSC
097800960402     C                   MOVE      PARCLI        W0040             4 0
097900960402     C     W0040         IFNE      9999
098000930622     C                   Z-ADD     BLPCCM        COMCLI
098100960402     C                   ELSE
098200960402     C                   Z-ADD     BLPKSC        COMCLI
098300960402     C                   ENDIF
098400930615     C* FRANCO
098500930615X   3C                   ELSE
098600930622     C                   Z-ADD     BLPKSC        COMCLI
098700930615E   3C                   ENDIF
098800930615     C* BOLLE ARRIVO
098900930615X   2C                   ELSE
099000950116     C     DUECBL        LOOKUP    ASS                                    31
099100930615     C* ASSEGNATO
099200930615IF  3C     *IN31         IFEQ      '1'
099300930615     C                   Z-ADD     ARBKSC        COMCLI
099400930615X   3C                   ELSE
099500930615     C* TIPO BOLLA DOPPIA
099600950116     C     DUECBL        LOOKUP    DOP                                    31
099700930615IF  4C     *IN31         IFEQ      '1'
099800991104     C     KARB2         CHAIN     FIAR601L                           33
099900991104     C  N33              Z-ADD     AR6KSC        COMCLI
100000930615E   4C                   ENDIF
100100930615E   3C                   ENDIF
100200930615E   2C                   ENDIF
100300930615     C*
100400930615     C* BOLLE NON DEL CLIENTE SELEZIONATO
100500130114    2c                   if        parragr=' '
100600121220     c
100700130114IF  3C     PARCLI        IFNE      COMCLI
100800930615     C                   SETON                                        10
100900931014     C                   GOTO      ENDESC
101000130114E   3C                   ENDIF
101100121220     c
101200130114   x2c                   else
101300121220     c*
101400121220     c* controllo nella skiera caricata
101500130114     c                   movel     comcli        alfa7             7
101600130114     c                   eval      Indx=%lookup(alfa7:skicli)
101700130114    3c                   if        Indx=0
101800121220     C                   SETON                                        10
101900121220     C                   GOTO      ENDESC
102000130114E   3C                   ENDIF
102100130114E   2C                   ENDIF
102200130114E   1C                   ENDIF
102300930615     C*
102400930615     C* TIPO SPEDIZIONE DIVERSA DA QUELLA SELEZIONATA
102500931014IF  1C     VIDTIP        IFNE      *BLANKS
102600930616     C* PARTENZA
102700950113IF  2C  N05VIDTIP        IFNE      BLPTSP
102800930615     C                   SETON                                        10
102900931014     C                   GOTO      ENDESC
103000930616E   2C                   ENDIF
103100930616     C* ARRIVO
103200950113IF  2C   05VIDTIP        IFNE      ARBTSP
103300930622     C                   SETON                                        10
103400931014     C                   GOTO      ENDESC
103500930622E   2C                   ENDIF
103600930616E   1C                   ENDIF
103700070302     C*
103800070302     C* nazione/CAP destinatario
103900070302IF  1C     parcad        IFNE      *BLANKS
104000070302     C* PARTENZA
104100070302IF  2C  N05parcad        IFNE      BLPcad
104200070302IF  2C     parnzd        orne      BLPnzd
104300070302     C                   SETON                                        10
104400070302     C                   GOTO      ENDESC
104500070302E   2C                   ENDIF
104600070302     C* ARRIVO
104700070302IF  2C   05parcad        IFNE      ARBcad
104800070302IF  2C     parnzd        orne      ARBnzd
104900070302     C                   SETON                                        10
105000070302     C                   GOTO      ENDESC
105100070302E   2C                   ENDIF
105200070302E   1C                   ENDIF
105300950302     C*- - - - - - - - - - - - - - - - - - - - - - - - - -
105400950302     C* COD.PADRONCINO
105500950302     C*- - - - - - - - - - - - - - - - - - - - - - - - - -
105600950303IF  1C     PARPD1        IFGT      *ZERO
105700950302     C* PARTENZA
105800950303DO  2C  N05              DO
105900950302     C*
106000950303IF  3C     PARPD1        IFGT      BLPPDR                                       <= PADR.INIZIALE
106100950302     C                   SETON                                        10
106200950302     C                   GOTO      ENDESC
106300950303E   3C                   ENDIF
106400950303IF  3C     PARPD2        IFEQ      *ZERO                                        SOLO UN PADR.
106500950303IF  4C     PARPD1        IFLT      BLPPDR
106600950302     C                   SETON                                        10
106700950302     C                   GOTO      ENDESC
106800950303E   4C                   ENDIF
106900950303   X3C                   ELSE
107000950303IF  4C     PARPD2        IFLT      BLPPDR                                       >= PADR.FINALE
107100950302     C                   SETON                                        10
107200950302     C                   GOTO      ENDESC
107300950303E   4C                   ENDIF
107400950303E   3C                   ENDIF
107500950302     C*
107600950303E   2C                   ENDDO
107700950302     C* ARRIVO
107800950303DO  2C   05              DO
107900950302     C*
108000950303IF  3C     PARPD1        IFGT      ARBPDC                                       <= PADR.INIZIALE
108100950302     C                   SETON                                        10
108200950302     C                   GOTO      ENDESC
108300950303E   3C                   ENDIF
108400950303IF  3C     PARPD2        IFEQ      *ZERO                                        SOLO UN PADR.
108500950303IF  4C     PARPD1        IFLT      ARBPDC
108600950302     C                   SETON                                        10
108700950302     C                   GOTO      ENDESC
108800950303E   4C                   ENDIF
108900950303   X3C                   ELSE
109000950303IF  4C     PARPD2        IFLT      ARBPDC                                       >= PADR.FINALE
109100950302     C                   SETON                                        10
109200950302     C                   GOTO      ENDESC
109300950303E   4C                   ENDIF
109400950303E   3C                   ENDIF
109500950302     C*
109600950303E   2C                   ENDDO
109700950303E   1C                   ENDIF
109800950302     C*
109900950302     C*- - - - - - - - - - - - - - - - - - - - - - - - - -
110000950302     C* DATA RITIRO MERCE (PER PADRONCINO)
110100950302     C*- - - - - - - - - - - - - - - - - - - - - - - - - -
110200950303IF  1C     PARDR1        IFGT      *ZERO
110300950302     C* PARTENZA
110400950303DO  2C  N05              DO
110500950302     C*
110600950303IF  3C     PARDR1        IFGT      BLPDRT                                       <= DATA INIZIALE
110700950302     C                   SETON                                        10
110800950302     C                   GOTO      ENDESC
110900950303E   3C                   ENDIF
111000950303IF  3C     PARDR2        IFEQ      *ZERO                                        SOLO UNA DATA
111100950303IF  4C     PARDR1        IFLT      BLPDRT
111200950302     C                   SETON                                        10
111300950302     C                   GOTO      ENDESC
111400950303E   4C                   ENDIF
111500950303   X3C                   ELSE
111600950303IF  4C     PARDR2        IFLT      BLPDRT                                       >= DATA FINALE
111700950302     C                   SETON                                        10
111800950302     C                   GOTO      ENDESC
111900950303E   4C                   ENDIF
112000950303E   3C                   ENDIF
112100950302     C*
112200950303E   2C                   ENDDO
112300950303E   1C                   ENDIF
112400950302     C*
112500950302     C*- - - - - - - - - - - - - - - - - - - - - - - - - -
112600950302     C* MATTINO / POMERIGGIO (PER PADRONCINO)
112700950302     C*- - - - - - - - - - - - - - - - - - - - - - - - - -
112800950303IF  1C     PARFPP        IFNE      *BLANKS
112900950302     C* PARTENZA
113000950302IF  2C  N05PARFPP        IFNE      BLPFPP
113100950302     C                   SETON                                        10
113200950302     C                   GOTO      ENDESC
113300950302E   2C                   ENDIF
113400950303E   1C                   ENDIF
113500950302     C*
113600950302     C*- - - - - - - - - - - - - - - - - - - - - - - - - -
113700950302     C* NUMERO RITIRO (PER PADRONCINO)
113800950302     C*- - - - - - - - - - - - - - - - - - - - - - - - - -
113900950303IF  1C     PARNRT        IFNE      *BLANKS
114000950302     C* PARTENZA
114100950303DO  2C  N05              DO
114200950302     C                   MOVEL     PARNRT        WNRT
114300950302     C     *LIKE         DEFINE    BLPNRT        WNRT
114400950303IF  3C     WNRT          IFNE      BLPNRT
114500950302     C                   SETON                                        10
114600950302     C                   GOTO      ENDESC
114700950303E   3C                   ENDIF
114800950303E   2C                   ENDDO
114900950303E   1C                   ENDIF
115000950302     C*- - - - - - - - - - - - - - - - - - - - - - - - - -
115100930615     C*
115200930719     C* CONTROLLO ORE DI RITARDO
115300931014     C* SOLO SE LE DEVO STAMPARE E SE DEVO ESCLUDERNE
115400931014    1C     VIDVRA        IFEQ      'S'
115500931014     C     *IN18         OREQ      'S'
115600931014     C     *IN19         OREQ      'S'
115700930615     C*
115800930719     C* BOLLA ARRIVO: ORE DI RITARDO GIA' PRESENTE IN BOLLA
115900930719IF  2C     *IN05         IFEQ      *ON
116000930719     C*
116100950116     C* AFFIDABILITA' CLIENTI  --> RITARDO = ORE CONSEGNA + ORE TRAZIONE
116200950116     C* AFFIDABILITA' CONSEGNA --> RITARDO = ORE CONSEGNA
116300930719     C*
116400931012IF  3C     ARBDCE        IFGT      *ZEROS
116500950113     C   01              Z-ADD     ARBNRC        COMHRI                          TOTALE   E
116600950113     C  N01              Z-ADD     ARBNCI        COMHRI                          CONSEGNA I
116700930719     C* PER LA STAMPA
116800950116     C                   Z-ADD     ARBDCE        COMDCE            6 0           *PERDO IL SECOLO
116900930719X   3C                   ELSE
117000930615     C*
117100930615     C* CALL PROGRAMMA CALCOLO ORE DI RITARDO
117200930623     C                   SETON                                        15
117300970925     C                   CLEAR                   DS99
117400930615     C                   MOVEL     ' '           D98TLA
117500930615     C                   MOVEL     'A'           D98TBO
117600950116     C                   Z-ADD     ARBAAS        D98AAS
117700950116     C                   Z-ADD     ARBLNP        D98LNP
117800950116     C                   Z-ADD     ARBNRS        D98NRS
117900950116     C                   Z-ADD     ARBNSP        D98NSP
118000930615     C*
118100970925     C                   CALL      'TNSD99R'
118200970925     C                   PARM                    DS99
118300930615     C*
118400931012     C   01              Z-ADD     D98NRC        COMHRI                          TOTALE   E
118500931012     C  N01              Z-ADD     D98NCI        COMHRI                          CONSEGNA I
118600930719     C* PER LA STAMPA
118700950116     C                   Z-ADD     D98DCE        COMDCE                          *PERDO IL SECOLO
118800930719E   3C                   ENDIF
118900930719     C*
119000930719     C* BOLLA PARTENZA: ORE DI RITARDO GIA' PRESENTE IN BOLLA
119100930719X   2C                   ELSE
119200930719     C*
119300950116     C* AFFIDABILITA' CLIENTI  --> RITARDO = ORE CONS.  E + ORE TRAZIONE
119400950116     C* AFFIDABILITA' CONSEGNA --> RITARDO = ORE CONS.  I
119500930719     C*
119600931012IF  3C     BLPDCE        IFGT      *ZEROS
119700950116     C   01              Z-ADD     BLPNRC        COMHRI                          TOTALE   E
119800950116     C  N01              Z-ADD     BLPNCI        COMHRI                          CONSEGNA I
119900070515     C   85              Z-ADD     BLPNpi        COMHRI                          CONSEGNA I
120000930719     C* PER LA STAMPA
120100950116     C                   Z-ADD     BLPDCE        COMDCE                          *PERDO IL SECOLO
120200950116X   3C                   ELSE
120300930615     C*
120400930615     C* CALL PROGRAMMA CALCOLO ORE DI RITARDO
120500930623     C                   SETON                                        15
120600970925     C                   CLEAR                   DS99
120700930615     C                   MOVEL     ' '           D98TLA
120800930615     C                   MOVEL     'P'           D98TBO
120900950116     C                   Z-ADD     BLPAAS        D98AAS
121000950116     C                   Z-ADD     BLPLNP        D98LNP
121100950116     C                   Z-ADD     BLPNRS        D98NRS
121200950116     C                   Z-ADD     BLPNSP        D98NSP
121300070515     c* Per l'affidabilità partenza, se manca, la calcolo a udate
121400070515     c*  passando la data di riferimento
121500070515     c                   if        vidacp='S'
121600070515     C                   Z-ADD     dateu         D98DRF
121700070515     C                   Z-ADD     2359          D98ORF
121800070515     c                   endif
121900930615     C*
122000970925     C                   CALL      'TNSD99R'
122100970925     C                   PARM                    DS99
122200930615     C*
122300931012     C   01              Z-ADD     D98NRC        COMHRI                          TOTALE   E
122400931012     C  N01              Z-ADD     D98NCI        COMHRI                          CONSEGNA I
122500070515     C   85              Z-ADD     D98NpI        COMHRI                          CONSEGNA I
122600930719     C* PER LA STAMPA
122700950116     C                   Z-ADD     D98DCE        COMDCE                          *PERDO IL SECOLO
122800930719E   3C                   ENDIF
122900930719E   2C                   ENDIF
123000930615     C*
123100931014     C* RITARDO : SE ORE TROVATE < ORE RICHIESTE ESCLUDO
123200931014     C   19COMHRI        IFLT      PARGIO
123300930615     C                   SETON                                        10
123400050623     C                   GOTO      ENDESC
123500930615E   2C                   ENDIF
123600931014     C* ANTICIPO: SE NON E' < 0 ESCLUDO
123700931014     C   18COMHRI        IFGE      0
123800931014     C                   SETON                                        10
123900050623     C                   GOTO      ENDESC
124000931014E   2C                   ENDIF
124100930615E   1C                   ENDIF
124200050704     c* Per differenze pesi e volumi
124300050704     c                   if        *in26
124400130118     c* tolgo dal peso VDL la tara
124500130118     c     §QtTpc        Mult      blpNcp        wTara
124600130118     c     blpPkc        Sub(h)    wTara         wNtara
124700130118     c                   Z-Add     wNtara        blppkc
124800130118     c
124900050704     c
125000050704     c* azzero i minori rilevati parzialmente: come non rilevati
125100050704     c  n05              if        blppkc>0 and blpncp<blpncl and
125200050704     c                             blppkc<blppkf
125300050704     c                   clear                   blppkc
125400050704     c                   clear                   blpncp
125500050704     c                   endif
125600050704     c  n05              if        blpvlc>0 and blpncr<blpncl and
125700050704     c                             blpvlc<blpvlf
125800050704     c                   clear                   blpvlc
125900050704     c                   clear                   blpncr
126000050704     c                   endif
126100050704     c
126200050704     c   05              if        arbpkc>0 and arbncp<arbncl and
126300050704     c                             arbpkc<arbpkf
126400050704     c                   clear                   arbpkc
126500050704     c                   clear                   arbncp
126600050704     c                   endif
126700050704     c   05              if        arbvlc>0 and arbncr<arbncl and
126800050704     c                             arbvlc<arbvlf
126900050704     c                   clear                   arbvlc
127000050704     c                   clear                   arbncr
127100050704     c                   endif
127200050704     c
127300050704     c* Per differenze pesi e volumi, selezionato soltanto MAGGIORE
127400050704    1c                   if        parmag='MA'
127500050623     c                   select
127600050623     c                   when      viddpv='S' and viddvv='S'
127700050623    2c  n05              if        blppkc<=blppkf  and blpvlc<=blpvlf
127800050623     C                   SETON                                        10
127900050623     C                   GOTO      ENDESC
128000050623E   2C                   ENDIF
128100050623    2c   05              if        arbpkc<=arbpkf  and arbvlc<=arbvlf
128200050623     C                   SETON                                        10
128300050623     C                   GOTO      ENDESC
128400050623E   2C                   ENDIF
128500050623     c
128600050623     c                   when      viddpv='S'
128700050623    2c  n05              if        blppkc<=blppkf
128800050623     C                   SETON                                        10
128900050623     C                   GOTO      ENDESC
129000050623E   2C                   ENDIF
129100050623     c   05              if        arbpkc<=arbpkf
129200050623     C                   SETON                                        10
129300050623     C                   GOTO      ENDESC
129400050623E   2C                   ENDIF
129500050623     c                   when      viddvv='S'
129600050623    2c  n05              if        blpvlc<=blpvlf
129700050623     C                   SETON                                        10
129800050623     C                   GOTO      ENDESC
129900050623E   2C                   ENDIF
130000050623    2c   05              if        arbvlc<=arbvlf
130100050623     C                   SETON                                        10
130200050623     C                   GOTO      ENDESC
130300050623E   2C                   ENDIF
130400050623     C                   ENDSL
130500050623     c
130600050623    1c                   endif
130700050623     C
130800050704     c* Per differenze pesi e volumi, selezionato soltanto MINORE
130900050704    1c                   if        parmag='MI'
131000050623     c                   select
131100050623     c                   when      viddpv='S' and viddvv='S'
131200050623    2c  n05              if        blppkc>=blppkf  and blpvlc>=blpvlf
131300050623     C                   SETON                                        10
131400050623     C                   GOTO      ENDESC
131500050623E   2C                   ENDIF
131600050623     c* Escludo anche quando sono entrambi non rilevati
131700050623     c  n05              if        blppkc=0 and blpvlc=0
131800050623     C                   SETON                                        10
131900050623     C                   GOTO      ENDESC
132000050623E   2C                   ENDIF
132100050623    2c   05              if        arbpkc>=arbpkf  and arbvlc>=arbvlf
132200050623     C                   SETON                                        10
132300050623     C                   GOTO      ENDESC
132400050623E   2C                   ENDIF
132500050623     c* Escludo anche quando sono entrambi non rilevati
132600050623     c   05              if        arbpkc=0 and arbvlc=0
132700050623     C                   SETON                                        10
132800050623     C                   GOTO      ENDESC
132900050623E   2C                   ENDIF
133000050623     c
133100050623     c                   when      viddpv='S'
133200050623    2c  n05              if        blppkc>=blppkf or blppkc=0
133300050623     C                   SETON                                        10
133400050623     C                   GOTO      ENDESC
133500050623E   2C                   ENDIF
133600050623     c   05              if        arbpkc>=arbpkf or arbpkc=0
133700050623     C                   SETON                                        10
133800050623     C                   GOTO      ENDESC
133900050623E   2C                   ENDIF
134000050623     c                   when      viddvv='S'
134100050623    2c  n05              if        blpvlc>=blpvlf or blpvlc=0
134200050623     C                   SETON                                        10
134300050623     C                   GOTO      ENDESC
134400050623E   2C                   ENDIF
134500050623    2c   05              if        arbvlc>=arbvlf or arbvlc=0
134600050623     C                   SETON                                        10
134700050623     C                   GOTO      ENDESC
134800050623E   2C                   ENDIF
134900050623     C                   ENDSL
135000050624    1c                   endif
135100050623     c
135200050704     c* Per differenze pesi e volumi, selezionati ENTRAMBI
135300050704    1c                   if        parmag='EE'
135400050623     c* Escludo solo quando sono entrambi non rilevati
135500050623     c                   select
135600050623     c                   when      viddpv='S' and viddvv='S'
135700050623     c  n05              if        blppkc=0 and blpvlc=0
135800050623     C                   SETON                                        10
135900050623     C                   GOTO      ENDESC
136000050623E   2C                   ENDIF
136100050623     c   05              if        arbpkc=0 and arbvlc=0
136200050623     C                   SETON                                        10
136300050623     C                   GOTO      ENDESC
136400050623E   2C                   ENDIF
136500050623     c
136600050623     c                   when      viddpv='S'
136700050623     c  n05              if        blppkc=0
136800050623     C                   SETON                                        10
136900050623     C                   GOTO      ENDESC
137000050623E   2C                   ENDIF
137100050623     c   05              if        arbpkc=0
137200050623     C                   SETON                                        10
137300050623     C                   GOTO      ENDESC
137400050623E   2C                   ENDIF
137500050623     c
137600050623     c                   when      viddvv='S'
137700050623     c  n05              if        blpvlc=0
137800050623     C                   SETON                                        10
137900050623     C                   GOTO      ENDESC
138000050623E   2C                   ENDIF
138100050623     c   05              if        arbvlc=0
138200050623     C                   SETON                                        10
138300050623     C                   GOTO      ENDESC
138400050623E   2C                   ENDIF
138500050623     c                   endsl
138600050624E   1C                   ENDIF
138700050704E   1C                   ENDIF
138800050623     c
138900931014     C     ENDESC        ENDSR
139000930621     C*---------------------------------------------------------------*
139100950113     C* DECTES - DECODIFICA CAMPI INTESTAZIONE                        *
139200930621     C*---------------------------------------------------------------*
139300930621     C     DECTES        BEGSR
139400930621     C*
139500930621     C* ORA CORRENTE
139600950301     C                   TIME                    WTIME            14 0          ORA & DATA
139700950301     C                   MOVE      WTIME         WDATE             8 0          GG/MM/AAAA
139800950301     C                   MOVEL     WTIME         UTIME             6 0          HH:MM:SS
139900070515     c                   clear                   wlbdat
140000070515     c                   z-add     wdate         g02dat
140100070515     c                   call      'XSRDA8'
140200070515     c                   parm                    wlbdat
140300070515     c                   movel     g02inv        dateu             8 0
140400950301     C*
140500950301     C* CON O SENZA AFFIDABILITA'
140600950301     C                   MOVEL     *BLANKS       PARDE7
140700950302     C     VIDVRA        IFEQ      'S'
140800950301     C                   MOVEL     CAFFI1        PARDE7           20
140900950301     C                   ELSE
141000950301     C                   MOVEL     CAFFI2        PARDE7
141100950301     C                   ENDIF
141200950301     C*
141300950301     C* ELABORAZIONE PER DATA CONS/SPED O PADRONCINO
141400950301     C                   MOVEL     *BLANKS       PARDE8
141500950301     C     PARELA        IFEQ      'D'
141600950301     C                   MOVEL     CELAB1        PARDE8           20
141700950301     C                   ELSE
141800950301     C                   MOVEL     CELAB2        PARDE8
141900950301     C                   ENDIF
142000950302     C*
142100950302     C* STAMPA SOLO TOTALI O ANCHE IN DETTAGLIO
142200950303     C     PARTOT        IFEQ      'D'                                          'D'=DETTAGLIO
142300950303     C                   MOVEL     CTOTA1        PARD10           15
142400950302     C                   ELSE
142500950303     C     PARTOT        IFEQ      'P'                                          'P'=PARZIALE
142600950303     C                   MOVEL     CTOTA3        PARD10
142700950303     C                   ELSE
142800950303     C                   MOVEL     CTOTA2        PARD10
142900950302     C                   ENDIF
143000950303     C                   ENDIF
143100930621     C*
143200930621     C* DECODIFICA FILIALE CHE STAMPA
143300930621     C     PARFEL        CHAIN     AZORG01L                           34
143400930621IF  1C     *IN34         IFEQ      '0'
143500930621     C                   MOVEL     ORGDES        PARDES           15
143600930621X   1C                   ELSE
143700930621     C                   MOVEL     *BLANKS       PARDES
143800930621E   1C                   ENDIF
143900930621     C*
144000940527     C* DECODIFICA BOLLE IN DISTINTA O NON
144100940527IF  1C   05VIDBDS        IFEQ      'S'
144200940527     C                   MOVEL     CDIS          DECBDS           27
144300940527     C                   ELSE
144400940527     C     VIDBDS        IFEQ      'N'
144500940527     C                   MOVEL     CNODIS        DECBDS
144600940527     C                   ELSE
144700940527     C                   MOVEL     CTUT          DECBDS
144800940527     C                   ENDIF
144900940527E   1C                   ENDIF
145000940527     C*
145100940401     C* DECODIFICA LINEA DI ARRIVO SELEZIONATA
145200940401     C     PARLNA        IFGT      0
145300940401     C     PARLNA        CHAIN     AZORG01L                           34
145400930621IF  2C     *IN34         IFEQ      '0'
145500940405     C                   MOVEL     '-'           PARDE6
145600940405     C                   MOVE      ORGDES        PARDE6           22
145700930621X   2C                   ELSE
145800940405     C                   MOVEL     *BLANKS       PARDE6
145900930621E   2C                   ENDIF
146000930621E   1C                   ENDIF
146100940401     C* LINEA DI PARTENZA
146200940401     C     PARLNP        IFGT      0
146300940401     C     PARLNP        CHAIN     AZORG01L                           34
146400940401IF  2C     *IN34         IFEQ      '0'
146500940401     C                   MOVEL     '-'           PARDE5
146600940405     C                   MOVE      ORGDES        PARDE5           22
146700940401X   2C                   ELSE
146800940401     C                   MOVEL     *BLANKS       PARDE5
146900940401E   2C                   ENDIF
147000940401E   1C                   ENDIF
147100930621     C*
147200930621     C* DECODIFICA CLIENTE SELEZIONATO
147300930621IF  1C     *IN04         IFEQ      '1'
147400130115     c
147500130115   1aC                   if        parragr=' '
147600130115     c
147700130115     C                   Z-ADD     PARCLI        KSC
147800930621     C     KACO          CHAIN     CNACO00F                           35
147900930621IF  2C     *IN35         IFEQ      '0'
148000940405     C                   MOVEL     ACORAG        PARDE2           30
148100930621X   2C                   ELSE
148200930621     C                   MOVEL     *BLANKS       PARDE2
148300930621E   2C                   ENDIF
148400130115     c
148500130115   1ac                   else
148600130115     c
148700130115     c* decodifico da tabella
148800130115     c                   eval      kke1=%editc(simpou:'X')
148900130115     c                   eval      kke2=%editc(parcli:'X')
149000130115     c     ktbe2         chain     tntbe01l
149100130115     c                   if        %Found(tntbe01l)
149200130115     c                   movel     tbeuni        parde2
149300130115     c                   else
149400130115     c                   clear                   parde2
149500130115   1ac                   endif
149600130115     c
149700130115   1ac                   endif
149800130115     c
149900930621E   1C                   ENDIF
150000930621     C*
150100930621     C* DECODIFICA TIPO SPEDIZIONE
150200090604     c                   clear                   parde3           25
150300930621IF  1C     *IN07         IFEQ      '1'
150400090604     c                   clear                   ds5e
150500090604     c                   eval      cod = '5E'
150600090604     c                   clear                   KEY
150700090604     c                   eval      key = vidtip
150800090604     c     ktab          chain     tabel00f
150900090604     c                   if        %found(tabel00f)
151000090604     c                   eval      ds5e = tbluni
151100090604     c                   endif
151200090604     c                   eval      parde3 = §5edes
151300930719X   1C                   ELSE
151400090604     C                   eval      parde3 = 'TUTTI'
151500930621E   1C                   ENDIF
151600950301     C*
151700950301     C* DECODIFICA MATTINA / POMERIGGIO
151800950302IF  1C     PARFPP        IFEQ      ' '                                          ' '=TUTTI
151900950301     C                   MOVEL     CRTANT        PARDE9           11
152000950301     C                   ELSE
152100950301     C     PARFPP        IFEQ      'M'                                          'M'=MATTINA
152200950301     C                   MOVEL     CMAPO1        PARDE9
152300950301     C                   ELSE
152400950301     C                   MOVEL     CMAPO2        PARDE9
152500950301     C                   ENDIF
152600950301E   1C                   ENDIF
152700950302     C*
152800950302     C* NUMERO RITIRO
152900950302     C                   MOVEL     *BLANKS       PARD11
153000950302IF  1C     PARNRT        IFEQ      ' '                                          ' '=TUTTI
153100950302     C                   MOVEL     CRTANT        PARD11           11
153200950302     C                   ELSE
153300950302     C                   MOVEL     PARNRT        PARD11
153400950302E   1C                   ENDIF
153500930621     C*
153600931013     C* P A R A M E T R I
153700931013     C                   EXCEPT    PAR
153800070515     c                   seton                                        of
153900931013     C*
154000930621     C                   ENDSR
154100930615     C*---------------------------------------------------------------*
154200950113     C* STAMPA - STAMPA AFFIDABILITA' SPEDIZIONI                      *
154300930615     C*---------------------------------------------------------------*
154400930615     C     STAMPA        BEGSR
154500950303     C*
154600950303     C* SE 'P'=PARTENZA ED ELABORAZIONE 'P'=PADRONCINO, VERIFICO
154700950303     C* SE E' E' LO STESSO PADRONCINO DEL GIRO PRECEDENTE;
154800950303     C* SE NON LO E', STAMPO TOTALI PARZIALI PER PADRONCINO
154900950303     C     PARELA        IFEQ      'P'                                          'P'=PADRONCINO
155000950303     C     *IN05         ANDEQ     *OFF                                         N05=PARTENZA
155100950303     C     PARTOT        IFEQ      'D'                                          'D'=DETTAGLIO
155200950303     C     PARTOT        OREQ      'P'                                          'P'=PARZIALI
155300950303     C*
155400950303     C     BLPPDR        IFNE      WPDPDR                                       PRECEDENTE
155500950303     C* NO LA PRIMA VOLTA
155600950303     C     WPRIMA        IFNE      *BLANKS
155700950303     C                   EXCEPT    TOTPAD                                       STAMPO TOTALI
155800950303     C                   ENDIF
155900950303     C                   MOVEL     '1'           WPRIMA            1            PRIMA VOLTA
156000950303     C* AZZERO I PARZIALI PER PADRONCINO
156100950303     C                   Z-ADD     *ZERO         WPDSPE
156200950303     C                   Z-ADD     *ZERO         WPDCOL
156300950303     C                   Z-ADD     *ZERO         WPDPES
156400950303     C                   Z-ADD     *ZERO         WPDVOL
156500950303     C                   Z-ADD     BLPPDR        WPDPDR
156600950303     C* DECODIFICO IL PADRONCINO
156700021203     C*    WPDPDR        CHAIN     FNAPD01L                           33
156800021205     C                   move      wpdpdr        kpdpdr
156900021203     C                   move      'A'           kpdtip
157000021203     C     kfapd         chain     fiapd01L                           33
157100950303     C  N33              MOVEL     APDRSC        WPDRSC                         RAG.SOCIALE
157200950303     C   33              MOVEL     *BLANKS       WPDRSC
157300950303     C                   ENDIF
157400950303     C*
157500950303     C                   ENDIF
157600950303     C                   ENDIF
157700930617     C*
157800930618     C                   SETON                                        08
157900930618     C                   MOVEL     *BLANKS       DECTMC
158000980206     C                   MOVEL     *BLANKS       DECEMC
158100960614     C                   SETOFF                                       091221
158200960614     C                   SETOFF                                       141311
158300961118     C                   SETOFF                                       22
158400930616     C*
158500930616     C* D E C O D I F I C A  DETTAGLIO
158600930618     C*
158700930618     C* SERIE DIVERSA DA ZERO: STAMPO IL TRATTINO
158800930618     C  N05              MOVEL     BLPNRS        DUENRS            2 0
158900930618     C   05              MOVEL     ARBNRS        DUENRS
159000930618IF  1C     DUENRS        IFNE      *ZEROS
159100930618     C                   SETON                                        09
159200930618E   1C                   ENDIF
159300930617     C*
159400930617     C* DATA SPEDIZIONE
159500930617     C  N05              MOVEL     BLPMGS        DUEMGS            4 0
159600930617     C   05              MOVEL     ARBMGS        DUEMGS
159700930617     C*
159800930617     C                   MOVEL     DUEMGS        MM                2 0
159900051124     C                   MOVE      DUEMGS        GG                2 0
160000930617     C                   MOVEL     GG            DECMGS            4 0
160100930617     C                   MOVE      MM            DECMGS
160200930622     C*
160300930719     C* MITTENTE
160400950116     C  N05              MOVEL     BLPRSM        DECRAG           12
160500930622     C   05              MOVEL     ARBRSM        DECRAG
160600960614     C*
160700960614     C* RIFERIMENTO MITTENTE ALFABETICO
160800960614     C     *LIKE         DEFINE    BLPRMA        DECRMA
160900960614     C  N05              MOVEL     BLPRMA        DECRMA
161000960614     C   05              MOVEL     ARBRMA        DECRMA
161100960614     C     DECRMA        IFNE      *BLANKS
161200960614     C                   SETON                                        11
161300960614     C                   ENDIF
161400980519      *
161500980519      * SE ARRIVI ESTERO EUROPOLITAN (*IN23 *ON) RICAVO AWB
161600980519     C                   MOVEL     *BLANKS       WAWB             10
161700980519     C     *IN23         IFEQ      '1'
161800980519     C                   MOVEL     'E'           KTRC
161900060215     C     KAR4          CHAIN     FiAR4000                           30
162000980519     C  N30              MOVEL     AR4NOT        WAWB
162100980519     C                   ENDIF
162200020212      *
162300020212      * Se arrivi estero FedEx (*IN24 *ON) ricavo AWB
162400020215     C                   clear                   wawbfed
162500020212     C     *IN24         IFEQ      '1'
162600020212     C                   MOVEL     'E'           KTRC
162700060215     C     KAR4          CHAIN     FiAR4000                           30
162800020215     C  N30              MOVEL     AR4NOT        wawbfed
162900020212     C                   ENDIF
163000930719     C*
163100930719     C* DESTINATARIO
163200950116     C  N05              MOVEL     BLPRSD        DECRA2           12
163300930719     C   05              MOVEL     ARBRSD        DECRA2
163400930616     C*
163500960614     C* LOCALITA' DESTINATARIO
163600960614     C  N05              MOVEL     BLPLOD        DECLOD           20
163700960614     C   05              MOVEL     ARBLOD        DECLOD
163800050622     c* spedizione in Fermo Deposito
163900051107     C  N05blpffd        comp      ' '                                2727
164000051107     C   05arbffd        comp      ' '                                2727
164100051107     C  N05blpffd        comp      'M'                                    83
164200051107     C   05arbffd        comp      'M'                                    83
164300050622     c
164400050621     c* Provincia o nazione
164500050621     c                   if        *in05
164600050621     c                   if        arbnzd<>*blanks
164700050621     c                   movel     arbnzd        decprd            3
164800050621     c                   move      ' '           declod
164900050621     c                   else
165000050621     c                   movel(P)  arbprd        decprd
165100050621     c                   endif
165200050621     c                   else
165300050621     c                   if        blpnzd<>*blanks
165400050621     c                   movel     blpnzd        decprd
165500050621     c                   move      ' '           declod
165600050621     c                   else
165700050621     c                   movel(P)  blpprd        decprd
165800050621     c                   endif
165900050621     c                   endif
166000960614     C*
166100051124     c* Decodifico consegna richiesta
166200051124     c                   clear                   dectcr           11
166300051124     c                   clear                   decdcr            6 0
166400051124     c                   if        *in05 and (arbdcr>0 or arbhcr>0) or
166500051124     c                             not *in05 and (blpdcr>0 or blphcr>0)
166600051124     c                   clear                   fnlv80ds
166700051124     c                   if        *in05
166800051124     c                   eval      ilv80aas=arbaas
166900051124     c                   eval      ilv80lnp=arblnp
167000051124     c                   eval      ilv80nrs=arbnrs
167100051124     c                   eval      ilv80nsp=arbnsp
167200051124     c                   eval      ilv80mgs=arbmgs
167300051124     c                   eval      ilv80tc1=arbtc1
167400051124     c                   eval      ilv80tc2=arbtc2
167500051124     c                   eval      ilv80dcr=arbdcr
167600051124     c                   eval      ilv80hcr=arbhcr
167700051124     c                   eval      ilv80tcr=arbtcr
167800051124     c                   else
167900051124     c                   eval      ilv80aas=blpaas
168000051124     c                   eval      ilv80lnp=blplnp
168100051124     c                   eval      ilv80nrs=blpnrs
168200051124     c                   eval      ilv80nsp=blpnsp
168300051124     c                   eval      ilv80mgs=blpmgs
168400051124     c                   eval      ilv80tc1=blptc1
168500051124     c                   eval      ilv80tc2=blptc2
168600051124     c                   eval      ilv80dcr=blpdcr
168700051124     c                   eval      ilv80hcr=blphcr
168800051124     c                   eval      ilv80tcr=blptcr
168900051124     c                   endif
169000051124     c                   call      'FNLV80R'
169100051124     c                   parm                    kpjba
169200051124     c                   parm                    fnlv80ds
169300051124     c
169400051124     c                   eval      decdcr=olv80dcr6
169500051124     c                   eval      dectcr=olv80tcrd
169600051124     c                   seton                                        12
169700051124     c
169800051124     c* decodifica in inglese
169900051124     C* TIPO CONSEGNA RICHIESTA
170000051124     C  N05              MOVEL     BLPTCR        DUETCR            1
170100051124     C   05              MOVEL     ARBTCR        DUETCR
170200051124IF  1C   06DUETCR        IFNE      ' '
170300051124IF  2C     DUETCR        IFEQ      'P'
170400090604     C                   MOVEL     TIP(01)       DECTCR
170500051124E   2C                   ELSE
170600090604     C                   MOVEL     TIP(02)       DECTCR
170700051124E   2C                   ENDIF
170800051124E   1C                   ENDIF
170900051124     c
171000051124     c                   endif
171100930617     C*
171200930617     C* MANCATA CONSEGNA
171300930617     C  N05              MOVEL     BLPTMC        DUETMC            1
171400930617     C   05              MOVEL     ARBTMC        DUETMC
171500930617     C*
171600930618IF  1C     DUETMC        IFNE      *BLANKS
171700930617     C                   MOVEL     '2A'          COD
171800930617     C                   MOVEL     *BLANKS       KEY
171900930617     C                   MOVEL     DUETMC        KEY
172000930617     C     KTAB          CHAIN     TABEL00F                           30
172100930617IF  2C     *IN30         IFEQ      '0'
172200930621     C                   SETON                                        14
172300930617     C                   MOVEL     TBLUNI        DS2A
172400050622     C                   MOVEL     §2ADES        DECTMC           10
172500961220     C*
172600961220     C*
172700950410   X2C                   ELSE
172800950410     C*
172900950410    3C     *IN05         IFEQ      *ON
173000950410     C                   MOVEL(P)  ARBCMC        DUETMC
173100950410     C     KTAB          CHAIN     TABEL00F                           30
173200950410IF  4C     *IN30         IFEQ      '0'
173300950410     C                   SETON                                        14
173400950410     C                   MOVEL     TBLUNI        DS2A
173500050622     C                   MOVEL     §2ADES        DECTMC
173600950410E   4C                   ENDIF
173700950410E   3C                   ENDIF
173800950410     C*
173900950410E   2C                   ENDIF
174000961220     C**
174100970613    2C   05DUETMC        IFEQ      'N'
174200970613     C* SE C'E' L'EVENTO MAN SCRIVO MANCANZA INVECE CHE NON FATTA
174300970613     C                   Z-ADD     ARBAAS        WAAS
174400990809     C     KEVB          SETLL     FNEVB04L                               30
174500970613    3C     *IN30         IFEQ      *ON
174600961220     C                   MOVEL     'M'           DUETMC
174700961220     C                   CLEAR                   DECTMC
174800961220     C                   MOVEL     'MANCANZA'    DECTMC
174900970613    3C                   ENDIF
175000970613    2C                   ENDIF
175100980206AG*  C* DECODIFICO IN INGLESE I MANCATI CONSEGNA SOLO NEL CASO IN CUI
175200980206AG*  C* SI TRATTI DI LINEA ARRIVI ESTERA
175300980206AG* 2C     *IN05         IFEQ      '1'
175400980206AG*  C     *IN06         ANDEQ     '1'
175500980206AG*  C                   MOVEL     '2X'          COD
175600980206AG*  C                   MOVEL     *BLANKS       KEY
175700980206AG*  C                   MOVEL     DUETMC        KEY
175800980206AG*  C     KTAB          CHAIN     TABEL00F                           30
175900050622AG*  C  N30              MOVEL     TBLUNI        DECEMC           12
176000980206AG*  C   30              MOVEL     *BLANKS       DECEMC
176100980206AG* 2C                   ENDIF
176200930617E   1C                   ENDIF
176300050622     c
176400050622     c* Se richieste differenze di pesi e volumi indicato flag
176500050622     c* del volume vdl totale o parziale
176600050622     c                   if        *in26
176700050622     c   05              movel     arbncr        wncr
176800050622     c  n05              movel     blpncr        wncr
176900050622     c   05              movel     arbncl        wncl
177000050622     c  n05              movel     blpncl        wncl
177100050622     c                   if        wncr=wncl
177200050622     c                   eval      stafvc='T'
177300050622     c                   else
177400050622     c                   if        wncr=0
177500050622     c                   clear                   stafvc
177600050622     c                   else
177700050622     c                   eval      stafvc='Z'
177800050622     c                   endif
177900050622     c                   endif
178000050622     c* Differenze
178100050623     C                   clear                   diffpeso
178200050623     c                   setoff                                       2829
178300050623     c  n05              if        blppkc>0
178400050623     c     blppkf        sub       blppkc        diffpeso          6 1
178500050623     c                   else
178600050623     c                   seton                                        28
178700050623     c                   endif
178800050623     c   05              if        arbpkc>0
178900050623     c     arbpkf        sub       arbpkc        diffpeso          6 1
179000050623     c                   else
179100050623     c                   seton                                        28
179200050623     c                   endif
179300050623     c
179400050623     C                   clear                   diffvolume
179500050623     c  n05              if        blpvlc>0
179600050623     c     blpvlf        sub       blpvlc        diffvolume        5 3
179700050623     c                   else
179800050623     c                   seton                                        29
179900050622     c                   endif
180000050623     c   05              if        arbvlc>0
180100050623     c     arbvlf        sub       arbvlc        diffvolume        5 3
180200050623     c                   else
180300050623     c                   seton                                        29
180400050623     c                   endif
180500050623     c                   endif
180600930617     C*
180700930617     C* DATA CONSEGNA PARZIALE
180800950116     C  N05              Z-ADD     BLPDCP        DUEDCP            6 0          *PERDO IL SECOLO
180900950116     C   05              Z-ADD     ARBDCP        DUEDCP
181000930617     C*
181100930617     C                   Z-ADD     DUEDCP        DECDCP            6 0
181200051124     C                   MOVEL     DUEDCP        AA                2 0
181300930617     C                   MOVE      DUEDCP        GG
181400930617     C                   MOVEL     GG            DECDCP
181500930617     C                   MOVE      AA            DECDCP
181600930617     C*
181700930617     C* DATA LASCIATO AVVISO
181800950116     C*--  N05             Z-ADDBLPDLA    DUEDLA  60       *PERDO IL SECOLO
181900950116     C*--   05             Z-ADDARBDLA    DUEDLA
182000950116     C*--
182100950116     C*--                  Z-ADDDUEDLA    DECDLA  60
182200950116     C*--                  MOVELDUEDLA    AA
182300950116     C*--                  MOVE DUEDLA    GG
182400950116     C*--                  MOVELGG        DECDLA
182500950116     C*--                  MOVE AA        DECDLA
182600930617     C*
182700930617     C* DATA CONSEGNA PREVISTA
182800930617     C                   Z-ADD     COMDCE        DECDCE            6 0
182900930617     C                   MOVEL     COMDCE        AA
183000930617     C                   MOVE      COMDCE        GG
183100930617     C                   MOVEL     GG            DECDCE
183200930617     C                   MOVE      AA            DECDCE
183300930617     C*
183400930617     C* DATA CONSEGNA MERCE
183500950116     C  N05              Z-ADD     BLPDCM        DUEDCM            6 0          *PERDO IL SECOLO
183600930617     C   05              Z-ADD     ARBDCM        DUEDCM
183700930617     C*
183800930617IF  1C     DUEDCM        IFNE      *ZEROS
183900930617     C                   Z-ADD     DUEDCM        DECDCM            6 0
184000930617     C                   MOVEL     DUEDCM        AA
184100930617     C                   MOVE      DUEDCM        GG
184200930617     C                   MOVEL     GG            DECDCM
184300930617     C                   MOVE      AA            DECDCM
184400940405X   1C                   ELSE
184500940405     C                   CLEAR                   DECDCM
184600930617E   1C                   ENDIF
184700950116     C*
184800950116     C* RECUPERA L'IMPORTO DEL CONTRASSEGNO
184900950116IF  1C     *IN05         IFEQ      *OFF                                         P A R T E N Z A
185000051115     C     KBLP          CHAIN     Fiar901L                           33
185100950116X   1C                   ELSE                                                   A R R I V O
185200051115     C     KARB          CHAIN     FiAR901L                           33
185300950116E   1C                   ENDIF
185400051115     c
185500051115     c* C/A non trovato azzero i campi
185600051115IF  2C     *IN33         IFEQ      *ON                                          NON TROVATO
185700051115     C                   MOVEL     *BLANKS       AR9TIC
185800051115     C                   MOVEL     *BLANKS       AR9VCA
185900051115     C                   Z-ADD     *ZEROS        AR9CAS
186000051115E   2C                   ENDIF
186100950116     C*
186200950116     C* ANNO SPEDIZIONE LUNGO DUE
186300950116     C  N05              Z-ADD     BLPAAS        BLPAA2            2 0
186400950116     C   05              Z-ADD     ARBAAS        ARBAA2            2 0
186500960614     C*
186600960614     C* RICHIESTA LA STAMPA DELLE SOLE ORE DI RITARDO: SE SI TRATTA
186700960614     C*   DI RITARDO VISUALIZZO LE ORE DI RITARDO; SE SI TRATTA DI
186800960614     C*   ANTICIPO, LE ORE DI ANTICIPO LE VISUALIZZO A ZERO
186900960614    1C     PARRIT        IFEQ      'S'
187000070515     C     comhri        ANDLT     0
187100960614     C                   SETON                                        21
187200960614    1C                   ENDIF
187300930616     C*
187400930616     C* D E T T A G L I O
187500930616     C*
187600950302     C     PARTOT        IFEQ      'D'                                          'D'=DETTAGLIO
187700930616     C                   EXCEPT    DET
187800931117      *
187900960614     C* SE C'E' STAMPO IL RIFERIMENTO MITTENTE ALFABETICO
188000980519      * STAMPO ANCHE SE ESISTE AWB, IN QUESTO CASO IMPOSTO *IN11 *ON
188100980519     C     *IN11         IFEQ      '1'
188200980519     C                   EXCEPT    DET4                                         RIF.MITT.ALFABETICO
188300980519     C                   ELSE
188400980519     C     WAWB          IFNE      *BLANKS
188500020215     c     wawbfed       orne      *blanks
188600980519     C                   EXCEPT    DET4
188700980519     C                   SETON                                        11
188800980519     C                   ENDIF
188900980519     C                   ENDIF
189000960614     C*
189100961212     C* SE C'E' LA DATA TRASMISSIONE LA STAMPO (SOLO PER BOLLE EXPORT)
189200961118     C     *IN05         IFEQ      *ON
189300050704     c     arblna        chain     azorg01l
189400050704     c                   if        %found(azorg01l)
189500050704     c                   movel     orgde3        og143
189600050704     c                   else
189700050704     c                   clear                   og143
189800050704     c                   endif
189900050704     C**   ARBLNA        ANDGE     300
190000050704     C**   ARBDT3        ANDGT     0
190100060322     c                   if        (§ogntw='EEX' or §ogntw='FED' or §ogntw=
190200060322     c                             'DPD') and arbdt3>0
190300961118     C                   SETON                                        22
190400961118     C                   Z-ADD     ARBDT3        DUEDT3            6 0
190500961118     C                   Z-ADD     DUEDT3        STADT3            6 0
190600961118     C                   MOVEL     DUEDT3        AA
190700961118     C                   MOVE      DUEDT3        GG
190800961118     C                   MOVEL     GG            STADT3
190900961118     C                   MOVE      AA            STADT3
191000961118     C  N11              EXCEPT    DET3                                         SALTA RIGA
191100961118     C                   EXCEPT    DET6
191200961118     C                   ENDIF
191300050704     C                   ENDIF
191400961118      *
191500931117      * SE C'E' LA CONSEGNA RICHIESTA, LA STAMPO
191600050704     c                   if        (*in12 and not *in11 and not *in22) or
191700050704     c                             (*in27 and not *in11 and not *in22)
191800050704     C                   EXCEPT    DET3                                         SALTA RIGA
191900050704     c                   endif
192000050704     C**   12
192100050704     C**ANN11
192200050704     C**ANN22              EXCEPT    DET3                                       SALTA RIGA
192300050704     c   27
192400050704     Cor 12              EXCEPT    DET1
192500931124      *
192600960614     C* SE C'E' IL C/ASSEGNO LO STAMPO
192700051115     C     *LIKE         DEFINE    ar9CAS        $CASS
192800051115     C                   MOVE      AR9CAS        $CASS
192900960614     C     $CASS         IFGT      0
193000960614     C                   SETON                                        13
193100961118     C  N12
193200050704     CanN27
193300961118     CANN11
193400961118     CANN22              EXCEPT    DET3                                         SALTA RIGA
193500960614     C                   EXCEPT    DET5                                         STAMPA C/ASSEGNO
193600960614     C                   ENDIF
193700960712     C                   ENDIF
193800960614      *
193900931124      * SE E' CONSEGNA ANOMALA: DECODIFICO E STAMPO
194000931124     C     *LIKE         DEFINE    BLPCCA        $ANOMA
194100931124     C  N05              MOVE      BLPCCA        $ANOMA
194200931124     C   05              MOVE      ARBCCA        $ANOMA
194300931124     C     $ANOMA        IFNE      *BLANKS
194400950302     C     PARTOT        ANDEQ     'D'                                          'D'=DETTAGLIO
194500931124     C                   MOVE      *BLANKS       §7ODES                         DESCRIZIONE
194600931124     C                   MOVE      '7O'          COD                            COD.TABELLA
194700931124     C                   MOVE      *BLANKS       KEY
194800931124     C                   MOVEL     $ANOMA        KEY                            COD.ELEMENTO
194900931124     C     KTAB          CHAIN     TABEL00F                           30
195000931124     C  N30TBLFLG        COMP      *BLANK                             3030
195100961118     C  N30              MOVEL     TBLUNI        §7ODES           16            DESCRIZIONE
195200961118     C  N22
195300050704     CanN27
195400961118     CANN12
195500961118     CANN13
195600961118     CANN11              EXCEPT    DET3                                         SALTA RIGA
195700931124     C                   EXCEPT    DET2                                         STAMPA ANOMALA
195800931124     C                   ENDIF
195900130111     c
196000130111     c* Se richeista la creazione del file la faccio
196100130111     c                   if        videxcel='S'
196200130111     c                   exsr      Creafile
196300130111     c                   endif
196400931117      *
196500931117      * MANTENGO AGGIORNATI I TOTALI
196600931116     C                   ADD       1             $NRSPE                         NR.SPEDIZIONI
196700931116     C  N05              ADD       BLPNCL        $COLLI                         TOT.COLLI
196800931116     C   05              ADD       ARBNCL        $COLLI                          "    "
196900950116     C  N05              ADD       BLPPKF        $PESO                          TOT.PESO
197000950116     C   05              ADD       ARBPKF        $PESO                           "    "
197100950116     C  N05              ADD       BLPVLF        $VOLUM                         TOT.VOLUME
197200950116     C   05              ADD       ARBVLF        $VOLUM                          "    "
197300050622     c* Per differenze peso e volumi, stampo anche totali VDL
197400050622     c                   if        *in26
197500050622     C  N05              ADD       BLPPKc        $PESOvdl                       TOT.PESO
197600050622     C   05              ADD       ARBPKc        $PESOvdl                        "    "
197700050622     C  N05              ADD       BLPVLc        $VOLUMvdl                      TOT.VOLUME
197800050622     C   05              ADD       ARBVLc        $VOLUMvdl                       "    "
197900050623     c* Stampo anche il totale differenze
198000050623     c                   add       diffpeso      $todiffpeso
198100050623     c                   add       diffvolume    $todiffvolu
198200050622     c                   endif
198300950303     C*
198400950303     C* ANCHE I TOTALI PARZIALI PER PADRONCINO (SE NECESSARIO)
198500950303     C     PARELA        IFEQ      'P'                                          'P'=PADRONCINO
198600950303     C     *IN05         ANDEQ     *OFF                                         N05=PARTENZA
198700950303     C                   ADD       1             WPDSPE                         NR.SPEDIZIONI
198800950303     C                   ADD       BLPNCL        WPDCOL                         TOT.COLLI
198900950303     C                   ADD       BLPPKF        WPDPES                         TOT.PESO
199000950303     C                   ADD       BLPVLF        WPDVOL                         TOT.VOLUME
199100950303     C                   ENDIF
199200931117      *
199300950116      * PER CONSEGNA PARTICOLARE AGGIORNO IL RIEPILOGO
199400950116      *
199500950116      * FACCIO DUE GIRI PER TC1 E TC2 (PRIMA E SECONDA CONSEGNA PARTICOLARE)
199600950116     C     *LIKE         DEFINE    BLPTC1        BLPFTC                         CONS.PARTIC DI COMOD
199700950116     C     *LIKE         DEFINE    ARBTC1        ARBFTC                         CONS.PARTIC DI COMOD
199800950116DO  1C     1             DO        2             W                 4 0
199900950116     C*
200000950116IF  2C     W             IFEQ      1
200100950116     C  N05              MOVEL     BLPTC1        BLPFTC
200200950116     C   05              MOVEL     ARBTC1        ARBFTC
200300950116X   2C                   ELSE
200400950116     C  N05              MOVEL     BLPTC2        BLPFTC
200500950116     C   05              MOVEL     ARBTC2        ARBFTC
200600950116E   2C                   ENDIF
200700950116      *
200800950116     C     *LIKE         DEFINE    BLPFTC        $PARTI                         CONS.PARTICOLAR
200900950116     C  N05              MOVE      BLPFTC        $PARTI
201000950116     C   05              MOVE      ARBFTC        $PARTI
201100931117     C     $PARTI        IFNE      *BLANK
201200931117      *
201300931117      * VERIFICO SE HO GIA' AVUTO LA STESSA CONSEGNA PARTICOLARE
201400931117     C                   Z-ADD     1             $A                3 0          CONTATORE
201500931117     C     $PARTI        LOOKUP    P§N($A)                                40     ON=TROVATO
201600931117      *
201700931117      * SE L'HO GIA' AVUTA AGGIORNO I TOTALI
201800931117     C     *IN40         IFEQ      *ON                                          TROVATO
201900931117      *
202000931117     C                   ADD       1             P§S($A)                        NR.SPEDIZIONI
202100931117     C  N05              ADD       BLPNCL        P§C($A)                        NR.COLLI
202200931117     C   05              ADD       ARBNCL        P§C($A)                         "   "
202300950116     C  N05              ADD       BLPPKF        P§P($A)                        PESO
202400950116     C   05              ADD       ARBPKF        P§P($A)                         "
202500950116     C  N05              ADD       BLPVLF        P§V($A)                        VOLUME
202600950116     C   05              ADD       ARBVLF        P§V($A)                          "
202700931117      *
202800931117     C                   ELSE                                                   NON TROVATO
202900931117      *
203000931117      * SE L'HO GIA' AVUTA LA INSERISCO COME NUOVA
203100931117     C                   Z-ADD     1             $A                3 0          CONTATORE
203200931117     C     *BLANK        LOOKUP    P§N($A)                                40     ON=TROVATO
203300931117     C   40              MOVE      $PARTI        P§N($A)                        CD.CONSEGNA
203400931117     C   40              Z-ADD     1             P§S($A)                        NR.SPEDIZIONI
203500931117     C  N05
203600931117     CAN 40              Z-ADD     BLPNCL        P§C($A)                        NR.COLLI
203700931117     C   05
203800931117     CAN 40              Z-ADD     ARBNCL        P§C($A)                         "   "
203900950116     C  N05
204000950116     CAN 40              Z-ADD     BLPPKF        P§P($A)                        PESO
204100950116     C   05
204200950116     CAN 40              Z-ADD     ARBPKF        P§P($A)                         "
204300950116     C  N05
204400950116     CAN 40              Z-ADD     BLPVLF        P§V($A)                        VOLUME
204500950116     C   05
204600950116     CAN 40              Z-ADD     ARBVLF        P§V($A)                          "
204700931117      *
204800931117     C                   ENDIF
204900931117      *
205000931117     C                   ENDIF
2051009501162   1C                   ENDDO
205200931117      *
205300931117      * SE C'E' RITARDO, E RICHIESTO NEL PARAMETRI DEL FILTRO, AGGIORNO ANCHE
205400931117      * I TOTALI PER RITARDI
205500931117     C     *IN16         IFEQ      *ON                                          RICHIESTI RIT.
205600931117     C                   Z-ADD     *ZERO         P§ORE
205700960617     C     *IN21         IFEQ      *OFF
205800070515     C**N01              Z-ADD     DECDCI        P§ORE
205900070515     C** 01              Z-ADD     DECHCE        P§ORE
206000070515     C                   Z-ADD     COMHRI        P§ORE
206100960617     C                   ENDIF
206200931117      *
206300931117      * VERIFICO SE HO GIA' AVUTO LO STESSO SCAGLIONE DI ORE RITARDO
206400931117     C                   Z-ADD     1             $A                             CONTATORE
206500931117     C     P§ORE         LOOKUP    P§O($A)                                40     ON=TROVATO
206600931118     C   40              ADD       1             P§R($A)                        NR.SPEDIZIONI
206700931124     C  N05
206800931124     CAN 40              ADD       BLPNCL        P§L($A)                        NR.COLLI
206900931124     C   05
207000931124     CAN 40              ADD       ARBNCL        P§L($A)                         "   "
207100950116     C  N05
207200950116     CAN 40              ADD       BLPPKF        P§K($A)                        PESO
207300950116     C   05
207400950116     CAN 40              ADD       ARBPKF        P§K($A)                         "
207500950116     C  N05
207600950116     CAN 40              ADD       BLPVLF        P§M($A)                        VOLUME
207700950116     C   05
207800950116     CAN 40              ADD       ARBVLF        P§M($A)                          "
207900931117      *
208000931117     C                   ENDIF
208100931117      *
208200930615     C                   ENDSR
208300130111     C*---------------------------------------------------------------*
208400130111     C* Crea file fi work per inviare all'utnete file excel           *
208500130111     C*---------------------------------------------------------------*
208600130111     C     creafile      BEGSR
208700130111     c                   clear                   wfpkc000
208800130114     c                   eval      pkcidute=knmeb
208900130114     c                   eval      pkceut=knmus
209000130114     c                   eval      pkcepo=simpou
209100130114     c                   eval      pkcedt=dateu_u
209200130114     c                   eval      pkceor=ora_u
209300130114     c
209400130111     c* Se da blp
209500130111     c                   if        not *in05
209600130114     c                   if        blpccm>0
209700130114     c                   eval      pkcccm=blpccm
209800130114     c                   else
209900130114     c                   eval      pkcccm=blpksc
210000130114     c                   endif
210100130114     c                   eval      pkcrsm=blprsm
210200130111     c                   eval      pkcaas=blpaas
210300130111     c                   eval      pkclnp=blplnp
210400130111     c                   eval      pkcnrs=blpnrs
210500130111     c                   eval      pkcnsp=blpnsp
210600130111     c                   eval      pkcmgs=blpmgs
210700130111     c                   eval      pkclna=blplna
210800130114     c                   eval      pkcrmn=blprmn
210900130114     c                   eval      pkcrma=blprma
211000130111     c                   eval      pkcrsd=blprsd
211100130111     c                   eval      pkclod=blplod
211200130111     c                   eval      pkcprd=blpprd
211300130111     c                   eval      pkccad=blpcad
211400130111     c                   eval      pkcnzd=blpnzd
211500130114     c                   eval      pkctsp=blptsp
211600130114     c                   eval      pkcncl=blpncl
211700130114     c                   eval      pkcpkf=blppkf
211800130114     c                   eval      pkcvlf=blpvlf
211900130114     c  n28              eval      pkcpkc=blppkc
212000130111     c                   endif
212100130114     c                   if            *in05
212200130114     c                   if        arbccm>0
212300130114     c                   eval      pkcccm=arbccm
212400130114     c                   else
212500130114     c                   eval      pkcccm=arbksc
212600130114     c                   endif
212700130114     c                   eval      pkcrsm=arbrsm
212800130114     c                   eval      pkcaas=arbaas
212900130114     c                   eval      pkclnp=arblnp
213000130114     c                   eval      pkcnrs=arbnrs
213100130114     c                   eval      pkcnsp=arbnsp
213200130114     c                   eval      pkcmgs=arbmgs
213300130114     c                   eval      pkclna=arblna
213400130114     c                   eval      pkcrmn=arbrmn
213500130114     c                   eval      pkcrma=arbrma
213600130114     c                   eval      pkcrsd=arbrsd
213700130114     c                   eval      pkclod=arblod
213800130114     c                   eval      pkcprd=arbprd
213900130114     c                   eval      pkccad=arbcad
214000130114     c                   eval      pkcnzd=arbnzd
214100130114     c                   eval      pkctsp=arbtsp
214200130114     c                   eval      pkcncl=arbncl
214300130114     c                   eval      pkcpkf=arbpkf
214400130114     c                   eval      pkcvlf=arbvlf
214500130114     c  n28              eval      pkcpkc=arbpkc
214600130114     c                   endif
214700130114     c  n28              eval      pkcdifp=diffpeso
214800130111     c* Se da blp
214900130111     c                   if        *in05
215000130111     c                   endif
215100130111     c
215200130111     c                   write     wfpkc000
215300130114     c
215400130114     c                   eval      contarec=contarec+1
215500130114     c
215600130111     c                   ENDSR
215700930610     C*---------------------------------------------------------------*
215800950113     C* CARTAB - CARICAMENTO TABELLE                                  *
215900930610     C*---------------------------------------------------------------*
216000930610     C     CARTAB        BEGSR
216100930614     C*
216200930614     C                   Z-ADD     1             I                 2 0
216300930623     C                   Z-ADD     1             X                 2 0
216400930623     C                   Z-ADD     1             J                 2 0
216500930614     C*
216600930614     C                   MOVEL     '3A'          COD
216700930614     C     KTAB2         SETLL     TABEL00F
216800930614     C     KTAB2         READE     TABEL00F                               30
216900930615DO  1C     *IN30         DOWEQ     '0'
217000930615IF  2C     TBLFLG        IFEQ      *BLANKS
217100930614     C                   MOVEL     TBLUNI        DS3A
217200930614     C*
217300930623     C* TIPO BOLLA DOPPIA
217400060126IF  3C     I             IFLE      50
217500930614     C     §3ATB2        ANDNE     *BLANKS
217600930614     C                   MOVEL     TBLKEY        DOP(I)
217700930614     C                   ADD       1             I
217800930615E   3C                   ENDIF
217900930614     C*
218000930614     C* TIPO BOLLA ASSEGNATO
218100060126IF  3C     X             IFLE      50
218200930614     C                   MOVEL     §3ATB1        TIPBOL            1
218300930615IF  4C     TIPBOL        IFEQ      'A'
218400930614     C                   MOVEL     TBLKEY        ASS(X)
218500930614     C                   ADD       1             X
218600930615E   4C                   ENDIF
218700930615E   3C                   ENDIF
218800930623     C*
218900930623     C* TIPO BOLLA RECUPERO
219000931013IF  3C     §3ARBL        IFEQ      'R'
219100930623     C                   MOVEL     TBLKEY        REC(J)
219200930623     C                   ADD       1             J
219300930623E   3C                   ENDIF
219400930623E   2C                   ENDIF
219500930614     C*
219600930614     C     KTAB2         READE     TABEL00F                               30
219700930615E   1C                   ENDDO
219800130118     c
219900130118     c                   Clear                   dsQt1
220000130118     c                   Movel     'QT'          cod
220100130118     c                   Movel     1             key
220200130118     c     Ktab          Chain     Tabel00f
220300130118     c                   If        %Found(Tabel00f) and TblFlg = *Blanks
220400130118     c                   Movel     TblUni        dsQt1
220500130118     c                   EndIf
220600930614     C*
220700930610     C                   ENDSR
220800930610     C*---------------------------------------------------------------*
220900950113     C* *INZSR - OPERAZIONI INIZIALI                                  *
221000930610     C*---------------------------------------------------------------*
221100950113     C     *INZSR        BEGSR
221200930610     C*
221300930610     C     *ENTRY        PLIST
221400930610     C                   PARM                    KPJBA
221500930614     C                   MOVEL     KPJBU         PARAM
221600130111     c
221700130111     c* Reperisco se presente  indirizzo e-mail
221800130111     c                   clear                   paremail
221900130111     c                   if        parprg_kpjbu>0
222000130114     c                   clear                   TRUL01ds
222100130111     c                   movel     parprg_kpjbu  UL01prg
222200130111     c                   call      'TRUL01R'
222300130111     c                   parm                    TRUL01ds
222400130111     c                   if        UL01err =  *on
222500130111     c                   else
222600130111     c                   movel     UL01prm       paremail         30
222700130111     c                   endif
222800130111     c                   endif
222900050623     c
223000050623     C                   Z-ADD     1             CODUT
223100050623     C                   CALL      'X§PARUT'
223200050623     C                   PARM                    UT§DSE
223300050623     C                   MOVEL     RAGUT         RSUT             20
223400130114     C                   MOVEL     REC80         CNCR80
223500130114     c*
223600130114     c                   eval      dataiso=datasys
223700130114     c                   eval      dateu_u=%dec(dataiso)
223800130114     c                   eval      dataiso=dataiso-%days(30)
223900130114     c                   eval      unmese=%dec(dataiso)
224000130114     c                   eval      ora_iso=ora_sys
224100130114     c                   eval      ora_u=%dec(ora_iso)
224200130114     c
224300130114      /free
224400130114       // Se richiesto file pulisco il lancio precedente per
224500130114       //  fil e sempre per vecchiaia
224600130114       setll (simpou)  wfpkc01l   ;
224700130114       reade (simpou)  wfpkc01l   ;
224800130114
224900130114       if not %eof(wfpkc01l)  and
225000130114         (videxcel='S'    or pkcedt<unmese) ;
225100130114
225200130114       dow  not %eof(wfpkc01l)   ;
225300130114       delete wfpkc000  ;
225400130114
225500130114       reade (simpou)  wfpkc01l   ;
225600130114       enddo  ;
225700130114        else  ;
225800130114        unlock   wfpkc01l  ;
225900130114        endif  ;
226000130114
226100130114       if videxcel='S'   ;
226200130114         *in86='1'  ;
226300130114       endif  ;
226400130114
226500130114      /end-free
226600130114
226700950301     C*
226800950301     C* LE SPECIFICHE CHE SEGUONO MI SERVONO PER DICHIARARE QUEI
226900950301     C* CAMPI CHE, FATTI PACKED NELL DS DI PASSAGGIO PARAMETRI,
227000950301     C* DEVONO AVERE UNA LUNGHEZZA BEN DEFINITA (ES.: LE DATE DI
227100950301     C* 8 CIFRE E NON 9)
227200950301     C                   Z-ADD     P§RDAL        PARDAL            8 0
227300950301     C                   Z-ADD     P§RAL         PARAL             8 0
227400950301     C                   Z-ADD     R§VDAL        ROVDAL            8 0
227500950301     C                   Z-ADD     R§VAL         ROVAL             8 0
227600950301     C                   Z-ADD     P§RKCI        PARKCI            4 0
227700950301     C                   Z-ADD     V§DDSD        VIDDSD            8 0
227800950301     C                   Z-ADD     V§DDSA        VIDDSA            8 0
227900950301     C                   Z-ADD     R§VDSD        ROVDSD            8 0
228000950301     C                   Z-ADD     R§VDSA        ROVDSA            8 0
228100950301     C                   Z-ADD     V§DDR1        VIDDR1            8 0
228200950301     C                   Z-ADD     V§DDR2        VIDDR2            8 0
228300070302     C* DATa ritiro
228400070302     c                   clear                   wlbdat
228500070302     C                   MOVE      VIDDR1        G02DAT
228600070302     C                   CALL      'XSRDA8'
228700070302     C                   PARM                    WLBDAT
228800070509     C                   Z-ADD     G02inv        pardr1            8 0
228900070302     c
229000070302     c                   clear                   wlbdat
229100070302     C                   MOVE      VIDDR2        G02DAT
229200070302     C                   CALL      'XSRDA8'
229300070302     C                   PARM                    WLBDAT
229400070509     C                   Z-ADD     G02inv        pardr2            8 0
229500070302     c
229600980209     C                   Z-ADD     V§DDTD        VIDDTD            8 0
229700980209     C                   Z-ADD     V§DDTA        VIDDTA            8 0
229800070302     C* DATa trasmissione
229900070302     c                   clear                   wlbdat
230000070302     C                   MOVE      VIDDtd        G02DAT
230100070302     C                   CALL      'XSRDA8'
230200070302     C                   PARM                    WLBDAT
230300070509     C                   Z-ADD     G02inv        rovdtd            8 0
230400070302     c
230500070302     c                   clear                   wlbdat
230600070302     C                   MOVE      VIDDta        G02DAT
230700070302     C                   CALL      'XSRDA8'
230800070302     C                   PARM                    WLBDAT
230900070509     C                   Z-ADD     G02inv        rovdta            8 0
231000950301     C*
231100950301     C                   MOVEL     ROVDSD        ROVAAS            4 0          ANNO INIZIALE
231200950301     C                   Z-ADD     ROVDSD        ROVMGS            4 0          MESE/GIORNO INIZIALE
231300020607
231400020607      * salvo i codici autotrasportatori per la stampa
231500020607     c                   eval      sav_parpd1 = parpd1
231600020607     c                   eval      sav_parpd2 = parpd2
231700930614     C*
231800930614     C                   Z-ADD     1             CODUT
231900931014     C                   Z-ADD     0             WNUM6
232000930616     C                   SETON                                        OF
232100930610     C* TABEL00F
232200930610     C     KTAB          KLIST
232300930610     C                   KFLD                    CODUT             1 0
232400930610     C                   KFLD                    COD               2
232500930610     C                   KFLD                    KEY               8
232600930614     C     KTAB2         KLIST
232700930614     C                   KFLD                    CODUT
232800930614     C                   KFLD                    COD
232900130115     C     KTbe          KLIST
233000130115     C                   KFLD                    kcod
233100130115     C                   KFLD                    kke1
233200130115     C     KTbe2         KLIST
233300130115     C                   KFLD                    kcod
233400130115     C                   KFLD                    kke1
233500130115     C                   KFLD                    kke2
233600130115     C*
233700950116     C* FNBLP - FNARB
233800930615     C     KFID          KLIST
233900930615     C                   KFLD                    PARFIL
234000950116     C                   KFLD                    WSETLL
234100931014     C     KFID2         KLIST
234200931014     C                   KFLD                    PARFIL
234300950113     C                   KFLD                    WNUM6             8 0
234400940331     C     KFID3         KLIST
234500940331     C                   KFLD                    PARFIL
234600940405     C                   KFLD                    ROVAAS
234700940405     C                   KFLD                    ROVMGS
234800950303     C     KFID4         KLIST                                                  FNBLP34L
234900950303     C                   KFLD                    PARPD1                         ..COD.PADRONCINO
235000950303     C                   KFLD                    PARDR1                         ..DATA RITIRO
235100980209     C* FNARB55
235200980209     C     KARB55        KLIST
235300980209     C                   KFLD                    PARLNA
235400980209     C                   KFLD                    ROVDTD
235500051115     C* FNAR6 - FiAR9
235600930615     C     KARB          KLIST
235700930615     C                   KFLD                    ARBAAS
235800930615     C                   KFLD                    ARBLNP
235900930615     C                   KFLD                    ARBNRS
236000930615     C                   KFLD                    ARBNSP
236100991104     C     KARB2         KLIST
236200991104     C                   KFLD                    ARBAAS
236300991104     C                   KFLD                    ARBLNP
236400991104     C                   KFLD                    ARBNRS
236500991104     C                   KFLD                    ARBNSP
236600991104     C                   KFLD                    K2TRC
236700060215     C* FiAR4
236800980519     C     KAR4          KLIST
236900980519     C                   KFLD                    ARBAAS
237000980519     C                   KFLD                    ARBLNP
237100980519     C                   KFLD                    ARBNRS
237200980519     C                   KFLD                    ARBNSP
237300980519     C                   KFLD                    KTRC
237400950116     C* FNBLP
237500950116     C     KBLP          KLIST
237600950116     C                   KFLD                    BLPAAS
237700950116     C                   KFLD                    BLPLNP
237800950116     C                   KFLD                    BLPNRS
237900950116     C                   KFLD                    BLPNSP
238000950116     C* CNACO
238100930616     C     KACO          KLIST
238200930616     C                   KFLD                    CODUT
238300930616     C                   KFLD                    PARKCI
238400930616     C                   KFLD                    KSC               7 0
238500990809     C* FNEVB
238600970613     C     KEVB          KLIST
238700970613     C                   KFLD                    WAAS
238800970613     C                   KFLD                    ARBLNP
238900970613     C                   KFLD                    ARBNRS
239000970613     C                   KFLD                    ARBNSP
239100970613     C                   KFLD                    WCEV
239200021203
239300021203     C     Kfapd         klist
239400021203     C                   kfld                    kpdtip
239500021203     C                   kfld                    kpdpdr
239600991104     C*
239700991104     C                   MOVEL     2             K2TRC
239800970613     C*
239900991104     C     *LIKE         DEFINE    AR6TRC        K2TRC
240000991104     C     *LIKE         DEFINE    AR4TRC        KTRC
240100980519     C     *LIKE         DEFINE    EVBAAS        WAAS
240200970613     C     *LIKE         DEFINE    EVBCEV        WCEV
240300970613     C                   MOVEL     'MAN'         WCEV
240400930614     C*
240500950302     C* E' RICHIESTA L'AFFIDABILITA' O NO ?
240600050623     c                   select
240700050623     c                   when      parvra='D'
240800050621     c                   eval      vidvra='N'
240900050621     c                   eval      viddpv='S'
241000050623     c                   eval      viddvv='S'
241100050623     c
241200050623     c                   when      parvra='P'
241300050623     c                   eval      vidvra='N'
241400050623     c                   eval      viddpv='S'
241500050623     c                   eval      viddvv='N'
241600050623     c
241700050623     c                   when      parvra='V'
241800050623     c                   eval      vidvra='N'
241900050623     c                   eval      viddpv='N'
242000050623     c                   eval      viddvv='S'
242100050623     c                   other
242200050621     c                   eval      vidvra=parvra
242300050621     c                   eval      viddpv='N'
242400050623     c                   eval      viddvv='N'
242500050623     c                   endsl
242600050621     c
242700950302     C     VIDVRA        COMP      'S'                                    03     ON=CON AFFIDABILITA
242800050621     c
242900050623     c                   if        parmag='MA'
243000050704     c                   eval      sceltadif='-SOLO MAGGIORI            '
243100050623     C                   ENDIF
243200050623     c                   if        parmag='MI'
243300050704     c                   eval      sceltadif='-SOLO MINORI              '
243400050623     C                   ENDIF
243500050623     c                   if        parmag='EE'
243600050704     c                   eval      sceltadif='- TUTTE dei rilevati      '
243700050623     C                   ENDIF
243800050704     c                   if        parmag='NR'
243900050704     c                   eval      sceltadif='- TUTTE anche non rilevati'
244000050704     C                   ENDIF
244100050623     C
244200050621     c                   if        viddpv='S'
244300050621     c                   eval      stadpv='SI'
244400050704     c                   MOVE      SCELTADIF     STADPV
244500050621     c                   else
244600050621     c                   eval      stadpv='NO'
244700050621     c                   endif
244800050623     c                   if        viddvv='S'
244900050623     c                   eval      stadvv='SI'
245000050623     c                   MOVE      SCELTADIF     STADVV
245100050623     c                   else
245200050623     c                   eval      stadvv='NO'
245300050623     c                   endif
245400950302     C*
245500930614     C* I M P O S T A Z I O N I
245600930719     C*
245700940401IF  0C     VIDSPC        IFEQ      'S'
245800940401IF   C     VIDSPC        OREQ      ' '
245900940401     C*
246000930719     C* RICHIESTA STAMPA PER AFFIDABILITA' CLIENTI
246100930719IF  1C     VIDACL        IFEQ      'S'
246200930719     C                   SETON                                        01
246300931013     C                   MOVEL     'SI'          DECACL            2
246400931014     C                   MOVEL     'TOTALE  '    STACON            8
246500070515X   1C                   endif
246600070515     c                   if        vidaco='S'
246700931014     C                   MOVEL     'CONSEGNA'    STACON
246800930719E   1C                   ENDIF
246900070515     c                   if        vidacP='S'
247000070515     c                   seton                                        85
247100070515     C                   MOVEL     'PARTENZA'    STACON
247200070515E   1C                   ENDIF
247300940331     C*
247400940331     C                   SELECT
247500940331     C* SE IMMESSA SOLO UNA DATA CONSEGNA   --> ELABORO PER CONSEGNA
247600940331     C     PARDAL        WHENGT    0
247700940331     C     PARAL         ANDEQ     0
247800940331     C                   MOVEL     'C'           WLCON             1
247900940331     C*
248000940331     C* SE IMMESSA SOLO UNA DATA SPEDIZIONE --> ELABORO PER SPEDIZ
248100940331     C     VIDDSD        WHENGT    0
248200940331     C     VIDDSA        ANDEQ     0
248300940331     C                   MOVEL     'S'           WLCON             1
248400940331     C*
248500940331     C* SE IMMESSA SOLO SPEDIZIONE          --> ELABORO PER SPEDIZ
248600940331     C     VIDDSD        WHENGT    0
248700940331     C     PARDAL        ANDEQ     0
248800940331     C                   MOVEL     'S'           WLCON             1
248900940331     C*
249000940331     C* SE IMMESSA SOLO CONSEGNA            --> ELABORO PER CONSEGNA
249100940331     C     PARDAL        WHENGT    0
249200940331     C     VIDDSD        ANDEQ     0
249300940331     C                   MOVEL     'C'           WLCON             1
249400940331     C*
249500940331     C* SE CI SONO TUTTE PRENDO IL DAL AL PIU' PICCOLO
249600940331     C     PARDAL        WHENGT    0
249700940331     C     VIDDSD        ANDGT     0
249800940331     C* DATA CONSEGNA
249900940331     C                   MOVE      PARAL         G02DAT
250000940331     C                   MOVEL     *BLANK        G02ERR
250100950113     C                   CALL      'XSRDA8'
250200940331     C                   PARM                    WLBDAT
250300950113     C                   Z-ADD     G02TGI        GIOAL             8 0
250400940331     C*
250500940331     C                   MOVE      PARDAL        G02DAT
250600940331     C                   MOVEL     *BLANK        G02ERR
250700950113     C                   CALL      'XSRDA8'
250800940331     C                   PARM                    WLBDAT
250900940331     C*
251000950113     C     GIOAL         SUB       G02TGI        GIOCON            8 0
251100940331     C* DATE SPEDIZIONE
251200940331     C                   MOVE      VIDDSA        G02DAT
251300940331     C                   MOVEL     *BLANK        G02ERR
251400950113     C                   CALL      'XSRDA8'
251500940331     C                   PARM                    WLBDAT
251600950113     C                   Z-ADD     G02TGI        GIODSA            8 0
251700940331     C*
251800940331     C                   MOVE      VIDDSD        G02DAT
251900940331     C                   MOVEL     *BLANK        G02ERR
252000950113     C                   CALL      'XSRDA8'
252100940331     C                   PARM                    WLBDAT
252200950113     C     GIODSA        SUB       G02TGI        GIOSPE            8 0
252300940331     C*
252400940331     C     GIOCON        IFLE      GIOSPE
252500940331     C                   MOVEL     'C'           WLCON
252600940331     C                   ELSE
252700940331     C                   MOVEL     'S'           WLCON
252800940331     C                   ENDIF
252900940331     C*
253000940331     C                   ENDSL
253100940401     C                   ENDIF
253200940331     C*
253300940331     C* DATA SPEDIZIONE
253400950113IF  1C     VIDDSD        IFGT      0
253500940331     C                   SETON                                        20
253600940331     C                   MOVEL     'S'           WLNOC             1
253700950113IF  2C     VIDDSA        IFEQ      *ZEROS
253800940331     C                   Z-ADD     ROVDSD        ROVDSA
253900950113E   2C                   ENDIF
254000940401     C*
254100940331     C* SE NON IMMESSA LA DATA SPEDIZIONE LE NON CONSEGNATE LE LEGGO
254200940331     C*  PER DAATA CONSEGNA = 0
254300950113X   1C                   ELSE
254400950306     C     PARELA        IFNE      'P'                                          'P'=PADRONCINO
254500950113     C                   Z-ADD     00010101      ROVDSD
254600950113     C                   Z-ADD     99991231      ROVDSA
254700950306     C                   ENDIF
254800940331     C                   MOVEL     'C'           WLNOC             1
254900950113E   1C                   ENDIF
255000940401     C* DATA CONSEGNA
255100940401     C     PARDAL        IFEQ      0
255200940401     C     PARAL         ANDEQ     0
255300950306     C     PARELA        ANDNE     'P'                                          'P'=PADRONCINO
255400950113     C                   Z-ADD     00010101      ROVDAL
255500950113     C                   Z-ADD     99991231      ROVAL
255600940401     C                   ENDIF
255700940331     C*
255800940331     C* DATA CONSEGNA: SE NON IMMESSA DATA 'AL' = DAL
255900940331IF  1C     PARDAL        IFNE      *ZEROS
256000940331     C     PARAL         ANDEQ     *ZEROS
256100940331     C                   Z-ADD     ROVDAL        ROVAL
256200940331E   1C                   ENDIF
256300940124     C*
256400940331     C* SPEDIZIONI CONSEGNATE E NON CONSEGNATE
256500940124IF  1C     VIDSPC        IFEQ      ' '
256600931013     C                   SETON                                        17
256700931013     C                   MOVEL     CTUTTE        DECSPC           27
256800940124X   1C                   ELSE
256900931013     C*
257000931013     C* SELEZIONE BOLLE CONSEGNATE
257100931013IF  2C     VIDSPC        IFEQ      'S'
257200930614     C                   SETON                                        02
257300931013     C                   MOVEL     CCON          DECSPC           27
257400940124X   2C                   ELSE
257500931013     C                   MOVEL     CNOCON        DECSPC           27
257600931013E   2C                   ENDIF
257700980206E   1C                   ENDIF
257800980209AG*  C* SE E' STATA RICHIESTA LA SELEZIONE PER DATA TRASMISSIONE ESCLUDO
257900980209AG*  C* LE ALTRE SELEZIONI PER DTA CONSEGNA E DTA SPEDIZIONE
258000980209IF  1C     VIDDTD        IFNE      *ZEROS
258100980209     C                   MOVEL     'T'           WTRAS             1
258200980209     C                   SETON                                        25
258300980209IF  2C     VIDDTA        IFEQ      *ZEROS
258400980209     C                   Z-ADD     ROVDTD        ROVDTA
258500980209E   2C                   END
258600980209E   1C                   END
258700930614     C*
258800930614     C* SELEZIONE PER CLIENTE
258900930614IF  1C     PARCLI        IFNE      *ZEROS
259000930614     C                   SETON                                        04
259100930614E   1C                   ENDIF
259200121220     c* se richiesto il raggruppamento carico i codici
259300121220     c                   if        parragr='R'
259400130115     C                   SETON                                        87
259500130115     c                   eval      kke1=%editc(simpou:'X') +
259600130115     c                                  %editc(parcli:'X')
259700121220     c     ktbe          setll     tntbe01l
259800121220     c     ktbe          reade     tntbe01l
259900121220     c
260000121220     c                   dow       not %eof(tntbe01l)
260100130114     c                   if        tbeatb=' '  and
260200130114     c                             %subst(tbeke2:1:7) >*zeros
260300130114     c                   add       1             xx
260400130114     c                   eval      skicli(XX)=%subst(tbeke2:1:7)
260500121220     c                   endif
260600121220     c
260700121220     c     ktbe          reade     tntbe01l
260800121220     c                   enddo
260900121220     c
261000121220     c                   endif
261100930615     C*
261200930615     C* SELEZIONE BOLLE ARRIVO
261300930615IF  1C     VIDBPA        IFEQ      'A'
261400930615     C                   SETON                                        05
261500090604     C                   MOVEL     TIP(03)       PARDE4
261600940331     C                   MOVEA     LNA           LIN
261700961122     C* 06 ON  - RICHIESTA BOLLE ARRIVO E LINEA ARRIVO DI BOLLE EXPORT
261800961122     C*          QUINDI STAMPO LE DICITURA IN INGLESE
261900980206AG*  C* CONTROLLO LINEA DI ARRIVO ESTERO SE FLAG AZORG UGUALE AD "E"
262000980206     C     PARLNA        CHAIN     AZORG01L                           34
262100980206IF  2C     *IN34         IFEQ      '0'
262200020212     c                   movel     orgde3        og143
262300020212   2ac                   if        §ogntw = 'DPD' or §ogntw = 'FED' or
262400020212     c                             §ogntw = 'EEX' or §ogntw = 'EUP'
262500980206     C                   SETON                                        06
262600020212     c                   if        §ogntw = 'EUP'
262700980519     C                   SETON                                        23
262800980519E   3C                   ENDIF
262900020212     c                   if        §ogntw = 'FED'
263000020212     c                   eval      *in24 = *on
263100020212     c                   endif
263200020212   2ac                   endif
263300980519E   2C                   ENDIF
263400931013X   1C                   ELSE
263500090604     C                   MOVEL     TIP(04)       PARDE4            9
263600940331     C                   MOVEA     LNP           LIN
263700070515     c                   seton                                        84
263800930615E   1C                   ENDIF
263900930616     C*
264000930616     C* IMMESSO TIPO SPEDIZIONE
264100930616IF  1C     VIDTIP        IFNE      *BLANKS
264200930616     C                   SETON                                        07
264300930616E   1C                   ENDIF
264400931012     C* STAMPA ORE RITARDO
264500931014IF  1C     VIDVRA        IFEQ      'S'
264600931012     C                   SETON                                        16
264700931012E   1C                   ENDIF
264800050621     C* STAMPA differenze pesi volumi
264900050621IF  1C     VIDdpv        IFEQ      'S'
265000050623IF  1C     VIDdvv        oreq      'S'
265100050621     C                   SETON                                        26
265200050623     c                   if        viddpv='S'
265300050623     C                   SETON                                        80
265400050621E   1C                   ENDIF
265500050623     c                   if        viddvv='S'
265600050623     C                   SETON                                        81
265700050623E   1C                   ENDIF
265800050623     c
265900050623     c* Le differenze di peso e volume le stampo solo se :
266000050623     c*   richiesto o solo peso o solo volume
266100050623     c                   seton                                        82
266200050623     c
266300050623     c                   if        *in80 and *in81
266400050704     C  n06              EVAL      STATIPODIF='  PESO  VOLUME '
266500050704     C   06              EVAL      STATIPODIF='  WEIGHT VOLUME'
266600050623     c                   setoff                                       82
266700050623E   1C                   ENDIF
266800050623     c                   if        not *in80 and *in81
266900050623     C                   EVAL      STATIPODIF='   V O L U M E '
267000050623E   1C                   ENDIF
267100050623     c                   if        *in80 and not *in81
267200050704     C  n06              EVAL      STATIPODIF='   P E S O     '
267300050704     C   06              EVAL      STATIPODIF='   W E I G H T '
267400050623E   1C                   ENDIF
267500050623E   1C                   ENDIF
267600931014     C*
267700931014     C* SPEDIZIONI IN RITARDO /ANTICIPO /TUTTE
267800931014     C                   SELECT
267900931014IF  1C     VIDSRA        WHENEQ    'A'
268000931014     C                   SETON                                        18
268100931014     C                   MOVEL     CANTIC        DECRIT           11
268200931014IF  1C     VIDSRA        WHENEQ    'R'
268300931014     C                   SETON                                        19
268400931014     C                   MOVEL     CRITAR        DECRIT
268500931014     C                   OTHER
268600931014     C                   MOVEL     CRTANT        DECRIT
268700931014E   1C                   ENDSL
268800960614     C*
268900960614     C* STAMPA DELLE SOLE ORE DI RITARDO
269000960614     C     PARRIT        IFEQ      'S'
269100960614     C                   MOVEL     'SI'          STARIT            2
269200960614     C                   ENDIF
269300050621     C* STAMPA DIFFERENZE PESI VOLUMI
269400050621     C     PARRIT        IFEQ      'S'
269500050621     C                   MOVEL     'SI'          STARIT            2
269600050621     C                   ENDIF
269700960614     C*
269800950113     C* 17 OFF 02 ON  - RICHIESTE SOLO BOLLE CONSEGNATE E LA DATA DI CONSEGNA
269900931013     C*
270000931013     C* AFFIDABILITA CONSEGNE
270100931013IF  1C     VIDACO        IFEQ      'S'
270200931013     C                   MOVEL     'SI'          DECACO            2
270300931013E   1C                   ENDIF
270400070515IF  1C     VIDACP        IFEQ      'S'
270500070515     C                   MOVEL     'SI'          DECACP            2
270600070515E   1C                   ENDIF
270700930614     C*
270800050623     C                   Z-ADD     *ZERO         $NRSPE           13 0          NR.SPEDIZIONI
270900931116     C                   Z-ADD     *ZERO         $COLLI           15 0          TOT.COLLI
271000931116     C                   Z-ADD     *ZERO         $PESO            15 1          TOT.PESO
271100050623     C                   Z-ADD     *ZERO         $VOLUM           13 3          TOT.VOLUME
271200050713     C                   Z-ADD     *ZERO         WPDSPE           13 0          PADR.NR.SPEDIZIONI
271300950303     C                   Z-ADD     *ZERO         WPDCOL           15 0          PADR.TOT.COLLI
271400950303     C                   Z-ADD     *ZERO         WPDPES           15 1          PADR.TOT.PESO
271500050713     C                   Z-ADD     *ZERO         WPDVOL           13 3          PADR.TOT.VOLUME
271600950303     C                   Z-ADD     *ZERO         WPDPDR
271700950303     C                   MOVEL     *BLANKS       WPDRSC           35
271800950303     C     *LIKE         DEFINE    BLPPDR        WPDPDR                         COD.PADRONCINO
271900931117      *
272000931117      * RIEMPO, IN MANIERA ORDINATA, LA SCHIERA CON GLI ORARI DI RITARDO
272100931117     C                   Z-ADD     1             $B                3 0
272200931117     C     -83           DO        83            $A
272300931117     C                   ADD       1             $B
272400931117     C     $A            MULT      12            P§O($B)
272500931117     C                   ENDDO
272600931117     C                   Z-SUB     999           P§O(1)                         PRIMO VALORE
272700931117     C                   Z-ADD     169           $B
272800931117     C                   Z-ADD     999           P§O($B)                        ULTIMO VALORE
272900950303     C*
273000950303     C* ESEGUO L'OVERRIDE DEL FILE DI STAMPA
273100950303     C                   MOVEL     CMD(1)        COMMAN           80
273200950303     C                   Z-ADD     80            LUNG             15 5
273300950303     C                   CALL      'QCMDEXC'
273400950303     C                   PARM                    COMMAN
273500950303     C                   PARM                    LUNG
273600950303     C                   OPEN      PRTF198
273700950113     C*
273800950113     C* DECODIFICA CAMPI INTESTAZIONE (UNA VOLTA SOLA)
273900950113     C                   EXSR      DECTES
274000950113     C*
274100950113     C* CARICO SCHIERE ASSEGNATI E RECUPERI
274200950113     C                   EXSR      CARTAB
274300950113     C*
274400930610     C                   ENDSR
274500950113     C*---------------------------------------------------------------*
274600950113     C* CONPAR -  STAMPA TOTALI DI RIEPILOGO                          *
274700950113     C*---------------------------------------------------------------*
274800931117     C     CONPAR        BEGSR
274900931117      *
275000931117      * Emetto la testata
275100931117     C   OF              EXCEPT    TES1                                         TESTATA STAMPA
275200931117     C                   EXCEPT    TESPAR                                       TESTATA PARTIC.
275300070515     C                   SETOFF                                       4041of    NESSUNA CONS.
275400931117      *
275500931117      * Leggo le schiere memorizzate
275600931117     C     1             DO        50            $A
275700931117      *
275800931117     C     P§N($A)       IFNE      *BLANK
275900931117      *
276000931117      * Decodifico la consegna particolare
276100931117     C                   Z-ADD     1             CODUT                          CD.UTENTE
276200931117     C                   MOVE      '1P'          COD                            CD.TABELLA
276300931117     C                   MOVE      *BLANKS       KEY
276400931117     C                   MOVEL     P§N($A)       KEY                            CD.ELEMENTO
276500931117     C     KTAB          CHAIN     TABEL00F                           42
276600931117     C                   MOVE      *BLANKS       P§D1P            25            DESCRIZIONE
276700931117     C  N42              MOVEL     TBLUNI        P§D1P                          DESCRIZIONE
276800931117      *
276900931117     C                   MOVE      P§N($A)       P§FTC             1            CD.CONS.PARTIC.
277000931117     C                   Z-ADD     P§S($A)       P§SPE             9 0          NR.SPEDIZIONI
277100931117     C                   Z-ADD     P§C($A)       P§COL             9 0          TOT.COLLI
277200931117     C                   Z-ADD     P§P($A)       P§PES            10 1          TOT.PESI
277300950116     C                   Z-ADD     P§V($A)       P§VOL            10 3          TOT.VOLUMI
277400931124      *
277500931124      * Calcolo la percentuale sul totale spedizioni ($NRSPE)
277600931124     C     P§SPE         DIV       $NRSPE        P§PE1            12 5
277700931124     C     P§PE1         MULT      100           P§PE1                          PERCENTUALE
277800931124     C     P§PE1         ADD       0,005         P§PER             5 2          ARROTONDAMENTO
277900931117      *
278000931117      * Per overflow ristampo la testata
278100931117     C   OF              DO
278200931117     C                   EXCEPT    TES1                                         TESTATA STAMPA
278300931117     C                   EXCEPT    TESPAR                                       TESTATA PARTIC.
278400070515     c                   setoff                                       of
278500931117     C                   ENDDO
278600931117     C                   EXCEPT    DETPAR                                       DETTAGLIO PART.
278700931117     C  N40              SETON                                        40        ALMENO UNO
278800931117      *
278900931117     C                   ELSE                                                   FINITE
279000931117      *
279100931117     C                   SETON                                        41
279200931117     C                   ENDIF
279300931117      *
279400931117     C  N41              ENDDO
279500931117      *
279600931117      * A fine stampa testo se vi e' stata almeno una consegna particolare
279700931117     C  N40              EXCEPT    VUOPAR                                       VUOTO
279800000621     C* RIGA DI CHIUSURA
279900000621     C                   EXCEPT    TRATT
280000931117      *
280100931117     C                   ENDSR
280200950113     C*---------------------------------------------------------------*
280300950113     C* SCARIT - STAMPA DI TUTTI GLI SCAGLIONI DI ORE CON I RITARDI   *
280400950113     C*---------------------------------------------------------------*
280500931117     C     SCARIT        BEGSR
280600931117      *
280700931117      * Emetto la testata
280800931117     C   OF              EXCEPT    TES1                                         TESTATA STAMPA
280900931117     C                   EXCEPT    TESRIT                                       TESTATA RITARDI
281000070515     c                   setoff                                       of
281100931117      *
281200931117      * Leggo le schiere memorizzate
281300931117     C     1             DO        168           $A
281400931117      *
281500931117     C     P§R($A)       IFNE      *ZERO
281600931117      *
281700931117     C                   Z-ADD     P§O($A)       P§ORE             3 0          ORE RITARDO
281800931118     C                   Z-ADD     P§R($A)       P§SPE                          NR.TOT.RITARDI
281900931118     C                   Z-ADD     P§L($A)       P§COL                          TOT.COLLI
282000931118     C                   Z-ADD     P§K($A)       P§PES                          TOT.PESI
282100931118     C                   Z-ADD     P§M($A)       P§VOL                          TOT.VOLUMI
282200931124      *
282300931124      * Calcolo la percentuale sul totale spedizioni ($NRSPE)
282400931124     C     P§SPE         DIV       $NRSPE        P§PE1
282500931124     C     P§PE1         MULT      100           P§PE1                          PERCENTUALE
282600931124     C     P§PE1         ADD       0,005         P§PER                          ARROTONDAMENTO
282700931124      *
282800931117      *
282900931117      * Per overflow ristampo la testata
283000931117     C   OF              DO
283100931117     C                   EXCEPT    TES1                                         TESTATA STAMPA
283200931117     C                   EXCEPT    TESRIT                                       TESTATA RITARDI
283300070515     c                   setoff                                       of
283400931117     C                   ENDDO
283500931117     C                   EXCEPT    DETRIT                                       DETTAGLIO RIT.
283600931117      *
283700931117     C                   ENDIF
283800931117      *
283900931117     C                   ENDDO
284000931117      *
284100931117     C                   ENDSR
284200130115     C*---------------------------------------------------------------*
284300130115     C* Fexcel    creazione file excel                                *
284400130115     C*---------------------------------------------------------------*
284500130115     C     Fexcel        BEGSR
284600130115     c*
284700130115     c
284800130115    0c                   if        contarec>0
284900130115     c
285000130115    1c                   if        contarec <=2000
285100130115     c                   eval      staEM =%trim(paremail)+'@brt.it'
285200130115     c                   eval      stainvio=xlsE
285300130115    1c                   endif
285400130115     c
285500130115    1c                   if        contarec > 2000
285600130115     c                   eval      staEM ='N:\BARTMAIL\'+ %editc(simpou:'X') +
285700130115     c                             '\File Excel\WFPKC00F'
285800130115     c                   eval      stainvio=xlsF
285900130115    1c                   endif
286000130115      /free
286100130115
286200130115          pInIDJOB=knmeb       ;
286300130115
286400130116          pinEMLVAR='*OBJM* Lista spedizioni con differenze pesi rilevati:'  +
286500130115                       %subst(parde2:1:15)  ;
286600130115
286700130115        //      meno di 2000 record --> invio all'indirizzo e-mail
286800130115  1a      if contarec<= 2000 ;
286900130115            pInFILE='WFPKC00F' ;
287000130115            pInMRAKE2='XLS_E'  ;
287100130115            pInEMLDEST=%trim(paremail)+'@brt.it' ;
287200130115
287300130115  x1a     else  ;
287400130115          pInFILE='WFPKC00F' ;
287500130115            pInMRAKE2='XLS_F'  ;
287600130115  1       endif ;
287700130115
287800130115          callp trtcm8r1 (pInfile:pInIDJOB:pInPWDXLS:pInNOMXLS:pInEMLDEST:
287900130115                          pInEMLVAR:pInMRAKE2:pOutESITO) ;
288000130115
288100130115      /end-free
288200130115
288300130115     c                   else
288400130115     c
288500130115     c                   eval      staEM='NESSUNA bolla elaborata:file excel +
288600130115     c                             non creato'
288700130115     c                   endif
288800130115     c
288900130115
289000130115     C                   ENDSR
289100930615     O*---------------------------------------------------------------*
289200931117     OPRTF198   E            TES1             02
289300930616     O                       ST(1)               66
289400961122     O                 N06   ST(2)              132
289500961122     O                  06   ST(24)             132
289600930616     O                       ST(3)              198
289700050623     O                       RSUT                20
289800950301     O                       WDATE              179 '  /  /    '
289900930616     O                       PAGE          Z    198
290000930719     O*
290100931117     O          E            TES1             03
290200950301     O                       KNSIF               10
290300950301     O                       KNMUS               22
290400950301     O                       UTIME              178 '0 :  :  '
290500930719     O*
290600930719     O          E            TES              05
290700930719     O                       ST(17)              66
290800930719     O                       ST(18)             132
290900930719     O                       ST(19)             198
291000930719     O*
291100930616     O          E            TES              06
291200961122     O                 N06   ST(4)               66
291300961122     O                  06   ST(25)              66
291400961122     O                 N06   ST(7)              132
291500961122     O                  06   ST(28)             132
291600050621     O              n26N06   ST(10)             198
291700050621     O               26N06   ST(35)             198
291800050704     O               26 06   ST(38)             198
291900050704     O              n26 06   ST(31)             198
292000050704     O               26   n81                   182 '         '
292100050704     O               26   n80                   173 '        '
292200050622     O              N16N26                      173 '        '
292300050704     O              N16 06n26                   184 'DELIVERY'
292400050621     O              N16N26                      198 '           '
292500930719     O*
292600930623     O          E            TES              07
292700130114     O                 N06   ST(5)               66
292800961122     O                  06   ST(26)              66
292900961122     O                 N06   ST(8)              132
293000961122     O                  06   ST(29)             132
293100050621     O              n26N06   ST(11)             198
293200050621     O               26N06   ST(36)             198
293300050704     O              n26 06   ST(32)             198
293400050704     O               26 06   ST(39)             198
293500050704     O               26      statipodif         198
293600050704     O               26   n81                   182 '         '
293700050704     O               26   n80                   173 '        '
293800050621     O              N16N26                      173 '        '
293900050704     O              N16 06n26                   183 'DATE '
294000050621     O              N16N26                      198 '           '
294100931014     O               16      STACON             197
294200930719     O*
294300930623     O          E            TES              08
294400930719     O                       ST(17)              66
294500930719     O                       ST(18)             132
294600930719     O                       ST(19)             198
294700930719     O*---------------------------------------------------------------*
294800930616     O          E            DET         1
294900930719     O                       ST(15)             132
295000930719     O                       ST(16)             198
295100950116     O              N05      BLPAA2               2
295200950116     O               05      ARBAA2               2
295300930618     O              N05      BLPLNP               6
295400931014     O               05      ARBLNP               6
295500930621     O                       DUENRS        Z      9
295600930618     O               09                          10 '-'
295700980212     O              N05N06   BLPNSP        Z     17
295800980212     O               05N06   ARBNSP        Z     17
295900980212     O              N05 06   BLPNSP              17
296000980212     O               05 06   ARBNSP              17
296100930719     O                       DECMGS              23 '  /  '
296200930618     O              N05      BLPLNA              27
296300931014     O               05      ARBLNA              27
296400050621     O                       DECRAG              40
296500050621     O              N05      BLPRMN        Z     56
296600050621     O               05      ARBRMN        Z     56
296700050621     O                       DECRA2              69
296800050621     O              N05      BLPCAD              79
296900050621     O               05      ARBCAD              79
297000050621     O                       DECLOD             100
297100050621     O                       decPRD             103
297200050621     O              N05      BLPTSP             106
297300050621     O               05      ARBTSP             106
297400050621COLLIO              N05      BLPNCL        Z    113
297500050621  "  O               05      ARBNCL        Z    113
297600050621PESO O              N05      BLPPKF        4    122
297700050621 "   O               05      ARBPKF        4    122
297800050621VOLUMO              N05 26   BLPfvf             124
297900050621  "  O               05 26   ARBfvF             124
298000050621VOLUMO              N05      BLPVLF        4    131
298100050621  "  O               05      ARBVLF        4    131
298200050621DEPOSO**            N05      BLPFFD             133
298300050621  "  O**             05      ARBFFD             133
2984000506211' GGO              N05      BLPGC1             134
298500050621  "  O               05      ARBGC1             134
2986000506212' GGO              N05      BLPGC2             137
298700050621  "  O               05      ARBGC2             137
298800050621     O              N05      BLPTC1             140
298900050621     O               05      ARBTC1             140
299000050621     O              N05      BLPTC2             142
299100050621     O               05      ARBTC2             142
299200050622     O              N06      DUETMC             144
299300050622     O              N06 14                      145 '-'
299400050621     O              N06 14   DECTMC             155
299500050621     O               06 14   DECEMC             155
299600050621     O              n26      DECDCP             164 '  /  /  '
299700050621     O               16      DECDCE             173 '  /  /  '
299800050622     O              n26      DECDCM             184 '  /  /  '
299900050621     O               26      DECDCM             164 '  /  /  '
300000050621     o* Campi per differenze pesi e volumi
300100050623PESO O              N05 80n28BLPPKc        4    173
300200050623 "   O               05 80n28ARBPKc        4    173
300300050623 "   o                  80 28                   173 'NonRilev'
300400050623VOLUMO              N05 81n29stafvc             175
300500050623VOLUMO              N05 81n29BLPVLc        4    182
300600050623  "  O               05 81n29ARBVLc        4    182
300700050623 "   o                  81 29                   182 'NonRilev'
300800050623  "  O                  80   diffpeso      Q    191
300900050623  "  O                  81   diffvolume    Q    198
301000070515     O**             16N21N01DECDCI        M    194
301100070515     O**             16N21 01DECHCE        M    194
301200070515     O               16N21   COMHRI        M    194
301300961118     O*--- DATI DELLA 2° RIGA ----------------------------------------*
301400960614     O          E            DET4        1
301500980519AWB  O               23                           8 'A W B:'
301600980519AWB  O               23      WAWB                19
301700020212AWB  O               24                           8 'A W B:'
301800020215AWB  O               24      wawbfed             21
301900050704RIFERO                       DECRMA              56
302000961118     O*
302100960614     O          E            DET1        0  0
302200051107     O               27n83                       67 'F.DEPOSITO'
302300051107     O               27 83                       67 'F.D.SABATO'
302400050704     O                  12N06                    83 'CONS.RICHIESTA:'
302500050704     O                  12 06                    83 'DELIV.REQUIRED:'
302600960617CONSEO                  12   DECTCR            +  1
302700051124  "  O                  12   DECDCR             102 '  /  /  '
302800050622  "  O                  12N06                   106 'ORE'
302900050622  "  O                  12 06                   106 '   '
303000050622  "  O              N05 12   BLPHCR             112 '  :  '
303100050622  "  O               05 12   ARBHCR             112 '  :  '
303200050622     O          E            DET5        0  0
303300050622     O                    N06                   120 'C/ASS:'
303400050622     O                     06                   120 'C.O.D:'
303500051115  "  O                       AR9TIC             123
303600051115  "  O                       AR9CAS        4    137
303700051115  "  O                       AR9VCA             141
303800960614     O*
303900931124     O          E            DET2        0  0
304000050622ANOMAO              N05      BLPCCA             144
304100050622  "  O               05      ARBCCA             144
304200050622  "  O                                          145 '-'
304300050622  "  O                       §7ODES             161
304400050622     O*
304500050622     O          E    05      DET6        0  0
304600050622TRASMO                    N06                   175 'Invio Partner'
304700050622  "  O                     06                   175 'Transmis.Date'
304800050622  "  O                       STADT3             184 '  /  /  '
304900960614     O*
305000960614     O*
305100931124     O          E            DET3        1
305200931124     O                                            1 ' '
305300930719     O*---------------------------------------------------------------*
305400930621     O          E            MES              12
305500930719     O                       ST(13)             132
305600930617     O*---------------------------------------------------------------*
305700931117     O          E            FIN1        1
305800931117     O                       ST(17)              66
305900931117     O                       ST(18)             132
306000931117     O                       ST(19)             198
306100050622     O          E   n26      FIN1        2
306200961122     O                 N06                       16 'TOTALI GENERALI:'
306300961122     O                  06                       16 'T O T A L S    :'
306400961122     O                 N06                       40 'NUMERO SPEDIZIONI:'
306500961122     O                  06                       40 '       SHIPMENTS :'
306600931116     O                       $NRSPE        2   +  0
306700050623     O                 N06                       77 'TOTALE COLLI:'
306800050623     O                  06                       77 '      KOLLI :'
306900931116     O                       $COLLI        2   +  0
307000050623     O                 N06                      119 'TOTALE PESO (KG):'
307100050623     O                  06                      119 '         WEIGHT :'
307200931116     O                       $PESO         2   +  0
307300050623     O                 N06                      164 'TOTALE VOLUME (MC):'
307400050623     O                  06                      164 '       VOLUME (CM):'
307500931116     O                       $VOLUM        2   +  0
307600050622     O
307700050622     O          E    26      FIN1        2
307800050622     O                 N06                       16 'TOTALI GENERALI:'
307900050622     O                  06                       16 'T O T A L S    :'
308000050623     O                 N06                       28 'SPEDIZIONI:'
308100050623     O                  06                       28 'SHIPMENTS :'
308200050622     O                       $NRSPE        2   +  0
308300050623     O                 N06                       53 'COLLI:'
308400050623     O                  06                       53 'KOLLI:'
308500050622     O                       $COLLI        2   +  0
308600050623     O                 N06                       84 'PESO (KG):'
308700050623     O                  06                       84 '  WEIGHT :'
308800050622     O                       $PESO         2   +  0
308900050713     O               82N06 80                   117 'PESO  VDL :'
309000050713     O               82 06 80                   117 'WEIGHT VDL:'
309100050623     O                     80$PESOvdl      2   +  0
309200050623     O                 N06                      151 'VOLUME (MC):'
309300050623     O                  06                      151 '   VOLUME :'
309400050622     O                       $VOLUM        2   +  0
309500050713     O               82    81                   181 'VOLUME VDL:'
309600050713     O               82    81$VOLUMvdl     2   +  0
309700050623     O          E    26 82   FIN1        2
309800050623     O                     80                   117 'Differenza PESO:'
309900050623     O                     80$todiffpeso   O   +  0
310000050623     O                     81                   181 'Differenza VOLUME:'
310100050623     O                     81$todiffvolu   O   +  1
310200950303     O          E            FIN1        2
310300931118     O                       ST(20)              66
310400931118     O                       ST(21)             132
310500931118     O                       ST(22)             198
310600930621     O          E            FIN              64
310700961122     O                 N06   ST(14)             132
310800961122     O                  06   ST(34)             132
310900130115     O          E    87      FIN              65
311000130115     O                       stainvio            40
311100130115     O                       staEM            +   2
311200950303     O*---------------------------------------------------------------*
311300950303     O          E            TOTPAD      1
311400950303     O                       ST(17)              66
311500950303     O                       ST(18)             132
311600950303     O                       ST(19)             198
311700950307     O          E            TOTPAD      1
311800050713     O                                           18 'Autotrasportatore:'
311900050713     O                                           28 'SPEDIZ.:'
312000950303     O                       WPDSPE        2   +  0
312100050713     O                                           53 'COLLI:'
312200950303     O                       WPDCOL        2   +  0
312300050713     O                                           84 'PESO (KG):'
312400950303     O                       WPDPES        2   +  0
312500050713     O                                          151 'VOLUME (MC):'
312600950303     O                       WPDVOL        2   +  0
312700950307     O          E            TOTPAD      1  1
312800050713     O                       WPDPDR              17
312900050713     O                       WPDRSC            +  1
313000930719     O*---------------------------------------------------------------*
313100930719     O          E            PAR              02
313200930719     O                       ST(1)               66
313300961122     O                 N06   ST(2)              132
313400961122     O                  06   ST(24)             132
313500930719     O                       ST(3)              198
313600050623     O                       RSUT                20
313700950301     O                       WDATE              179 '  /  /    '
313800930719     O                       PAGE          Z    198
313900950301     O          E            PAR              03
314000950301     O                       KNSIF               10
314100950301     O                       KNMUS               22
314200950301     O                       UTIME              178 '0 :  :  '
314300930719     O*
314400070302     O          E            PAR              07
314500930720     O                       PARDE4             119
314600950301     O* STAMPA CON AFFIDABILITA' (S/N)
314700070302     O          E            PAR              09
314800950301     O                       SPA(16)            108
314900950302     O                       PARDE7             130
315000050621     O* STAMPA DIFFERENZE PESI E VOLUMI
315100070302     O          E            PAR              11
315200050621     O                       SPA(25)            108
315300050623     O                       stadpv             128
315400070302     O          E            PAR              12
315500050623     O                       SPA(26)            108
315600050623     O                       stadVv             128
315700950301     O* ELABORAZIONE PER DATA CONS/SPED O PADRONCINO
315800070302     O          E            PAR              14
315900950301     O                       SPA(17)            108
316000950302     O                       PARDE8             130
316100930719     O*
316200070302     O          E    03      PAR              17
316300940401     O                       SPA(4)             108
316400940401     O                       DECACL             112
316500940401     O*
316600070302     O          E    03      PAR              19
316700940405     O                                           88 'O P P U R E'
316800940405     O*
316900070302     O          E    03      PAR              21
317000940401     O                       SPA(5)             108
317100940401     O                       DECACO             112
317200070515     O          E    03      PAR         2
317300070515     O                                           88 'O P P U R E'
317400070515     O*
317500070515     O          E    03      PAR         2
317600070515     O                       SPA(28)            108
317700070515     O                       DECACP             112
317800940401     O*
317900070515     O          E            PAR         3
318000930719     O                       SPA(2)             108
318100931013     O                       DECSPC             137
318200930719     O*
318300070515     O          E            PAR         2
318400940405     O                       SPA(3)             108
318500950302     O               17      PARDAL             104 '  /  /    '
318600950302     O              N17 02   PARDAL             104 '  /  /    '
318700950302     O               17      PARAL              120 '  /  /    '
318800950302     O              N17 02   PARAL              120 '  /  /    '
318900930719     O*
319000070515     O          E            PAR         2
319100980209     O                       SPA(13)            108
319200980209     O                       VIDDSD             104 '  /  /    '
319300980209     O                       VIDDSA             120 '  /  /    '
319400130115     O                       SPA(10)            198
319500980209     O* DATA TRASMISSIONE
319600070515     O          E            PAR         2
319700980210     O               25      SPA(24)            108
319800980210     O               25      VIDDTD             104 '  /  /    '
319900980210     O               25      VIDDTA             120 '  /  /    '
320000940401     O*
320100070515     O          E          05PAR         2
320200940527     O                       SPA(15)            108
320300950116     O                       DECBDS             137
320400940527     O*
320500070515     O          E            PAR         2
320600940401     O                       SPA(14)            108
320700940401     O                       PARLNP        Z    113
320800940405     O                       PARDE5             136
320900940401     O*
321000070515     O          E            PAR         2
321100930719     O                       SPA(6)             108
321200940401     O                       PARLNA        Z    113
321300940405     O                       PARDE6             136
321400930719     O*
321500070515     O          E            PAR         2
321600930719     O                       SPA(7)             108
321700940401     O                    N05                    89 'MITTENTE'
321800940401     O                     05                    93 'DESTINATARIO'
321900130115     O               04n87   PARCLI        Z    117
322000130115     O               04n87   PARDE2             149
322100130115     O          E    87      PAR         1
322200130115     O                       SPA(31)            108
322300130115     O                       PARCLI        Z    117
322400130115     O                       PARDE2             149
322500960614     O* TIPO RITARDO SPEDIZIONI
322600070515     O          E    03      PAR         2
322700931014     O                       SPA(12)            108
322800931014     O                       DECRIT             121
322900960614     O* ORE MINIME RITARDO CONSEGNA
323000070515     O          E    03      PAR         2
323100930719     O                       SPA(8)             108
323200930719     O                       PARGIO        Z    113
323300960614     O* STAMPA DELLE SOLE ORE DI RITARDO
323400070515     O          E            PAR         2
323500960614     O                       SPA(23)            108
323600960614     O                       STARIT             112
323700960614     O* TIPO SERVIZIO
323800070515     O          E            PAR         2
323900930719     O                       SPA(9)             108
324000090604     O                       PARDE3             134
324100070302     O* NAZIONE E CAP DESTINATARIO
324200070515     O          E            PAR         2
324300070302     O                       SPA(27)            108
324400070302     O                       PARNZD             103
324500070302     O                                          115 '/'
324600070302     O                       PARCAD             125
324700950302     O* STAMPA DETTAGLIO O SOLO TOTALI
324800070515     O          E            PAR         2
324900950302     O                       SPA(22)            108
325000950303     O                       PARD10             125
325100950301     O* PADRONCINO DAL/AL
325200070515     O          E            PAR         4
325300950301     O                       SPA(18)            108
325400020607     O                       sav_PARPD1    Z    104
325500020607     O                       sav_PARPD2    Z    117
325600950301     O* DATA RITIRO MERCE
325700070515     O          E            PAR         2
325800950301     O                       SPA(19)            108
325900950302     O                       VIDDR1             104 '  /  /    '
326000950302     O                       VIDDR2             120 '  /  /    '
326100950301     O* MATTINA / POMERIGGIO
326200070515     O          E            PAR         2
326300950301     O                       SPA(20)            108
326400950302     O                       PARDE9             121
326500950301     O* NUMERO RITIRO
326600070515     O          E            PAR         2
326700950301     O                       SPA(21)            108
326800070515     O                       PARD11             121
326900130114     O* NUMERO RITIRO
327000130114     O          E    86      PAR         2
327100130114     O                       SPA(29)            108
327200931117      *
327300931117      * ---------------  CONSEGNE PARTICOLARI  ---------------
327400950303     O          E            TESPAR      2
327500931118     O                                           24 'RIEPILOGO PER CONSEGNE P'
327600931118     O                                         +  0 'ARTICOLARI:'
327700931124     O                                           72 'CONSEGNA PARTICOLARE'
327800931124     O                                           97 'NR.SPEDIZIONI'
327900931124     O                                          105 'PERC.'
328000931124     O                                          118 'NR.COLLI'
328100931124     O                                          133 'PESO (KG)'
328200931124     O                                          148 'VOLUME (MC)'
328300931117     O          E            TESPAR      1
328400931124     O                                           76 '------------------------'
328500931117     O                                         +  0 '------------------------'
328600931117     O                                         +  0 '------------------------'
328700931124     O                                         +  0 '------------------------'
328800931117     O          E            DETPAR      1
328900931124     O                       P§FTC               53
329000931124     O                       P§D1P               80
329100931124     O                       P§SPE         2     97
329200931124     O                       P§PER         2    104
329300931124     O                                          105 '%'
329400931124     O                       P§COL         2    118
329500931124     O                       P§PES         2    133
329600931124     O                       P§VOL         2    148
329700931117     O          E            VUOPAR      2
329800931118     O                                          110 '( NESSUNA CONSEGNA PARTI'
329900931117     O                                         +  0 'COLARE )'
330000000621     O          E            TRATT       2
330100000621     O                       ST(20)              66
330200000621     O                       ST(21)             132
330300000621     O                       ST(22)             198
330400931117      *
330500931117      * ---------------  RITARDI DI CONSEGNA  ---------------
330600931118     O          E            TESRIT      2
330700931118     O                                           24 'RIEPILOGO PER RITARDI DI'
330800931118     O                                         +  1 'CONSEGNA:'
330900931124     O                                           71 'ORE DI RITARDO'
331000931117     O          E            TESRIT      1
331100931124     O                                           76 'O ANTICIPO (SE NEGATIVE)'
331200931124     O                                           97 'NR.SPEDIZIONI'
331300931124     O                                          105 'PERC.'
331400931124     O                                          118 'NR.COLLI'
331500931124     O                                          133 'PESO (KG)'
331600931124     O                                          148 'VOLUME (MC)'
331700931117     O          E            TESRIT      1
331800931124     O                                           76 '------------------------'
331900931118     O                                         +  0 '------------------------'
332000931118     O                                         +  0 '------------------------'
332100931124     O                                         +  0 '------------------------'
332200931117     O          E            DETRIT      1
332300931124     O                       P§ORE         N     65
332400931124     O                       P§SPE         2     97
332500931124     O                       P§PER         2    104
332600931124     O                                          105 '%'
332700931124     O                       P§COL         2    118
332800931124     O                       P§PES         2    133
332900931124     O                       P§VOL         2    148
333000930621     O*---------------------------------------------------------------*
333100950303**
333200050623XXXXXXXXXXXXXXXXXXXX
333300950301          ***     L I S T A    S P E D I Z I O N I     ***           2
333400950301                  FNLRB7R                                PAG. XXXX   3
333500050621S  P  E  D  I  Z  I  O  N  E  MITTENTE       RIFERIM.     D   E      4
333600050621AN LNP SER NUMERO DATA  LNA                  MITTENTE    RAG.SOCIA   5
333700050621XX XXX XX-XXXXXXX XX/XX XXX XXXXXXXXXXXX XXXXXXXXXXXXXXX XXXXXXXXX   6
333800050621S   T   I   N   A   T   A   R   I   O SER       PESO kg VOLUME mc   7
333900050621LE   C A P     L O C A L I T À        VIZ COLLI IMMESSO/VARIATO     8
334000050621XXX XXXXXXXXX XXXXXXXXXXXXXXXXXXXX XX  X  XXXXX xxxxxx,x X XX,XXX    9
334100050622 GG   CONS Mancata Cons CONSEGNA CONSEGNA   CONSEGNA   ORE RITARDO  10
334200050622CHIUS PART Cons.Anomala PARZIALE  TEORICA     REALE                 11
334300050622XX XX  X X X-XXXXXXXXXx XX/XX/XX XX/XX/XX   XX/XX/XX      xxxx      12
334400930719 *** N E S S U N A   S P E D I Z I O N E   I N   R I T A R D O ***  13
334500930719                 ***  F I N E   S T A M P A  ***                    14
334600950116                                                                    15
334700950116                                                                    16
334800050622---------------------------+------------+---------------+---------  17
334900050621---+---------+-----------------------+---+-----+-----------------+  18
335000050622-----+----+------------+--------+-------------------+-------------  19
335100931118==================================================================  20
335200931118==================================================================  21
335300931118==================================================================  22
335400940405------------------------------------------------------------------  23
335500961122 ** L I S T   O F   S H I P M E N T   W I T H O U T   P. O. D. **   24
335600961122   S  H  I  P  M  E  N  T                     CLIENT                25
335700050621YR LHD  NUMBER    DATE  LHA  CONS/OR          REF.NR     CONSIGNE   26
335800050621XX XXX XX-XXXXXXX XX/XX XXX XXXXXXXXXXXX XXXXXXXXXXXXXXX XXXXXXXX   27
335900050622      Z I P    DELIVERY               TIP        WEIGHT    VOLUME   28
336000050622     C O D E   T O W N                SER KOLLI             (CM)    29
336100050621XXX XXXXXXXXX XXXXXXXXXXXXXXXXXXXX XX  X  XXXXX XXXXXX,X X XX,XXX   30
336200050622            REASON      PARTIAL  CONSEGNA   CONSEGNA   ORE RITARDO  31
336300050622           OF NON DEL.  DELIVERY  TEORICA     REALE                 32
336400050622XX XX  X X X-XXXXXXXXX  XX/XX/XX XX/XX/XX   XX/XX/XX      XXXX      33
336500961122              ***   E N D    O F    P R I N T   ***                 34
336600050623 GG   CONS Mancata Cons CONSEGNA PESO kg VOLUME mc    DIFFERENZA    35
336700130118CHIUS PART Cons.Anomala   Reale  VDL-Tara  VDL     XXXXXXXXXXXXXXX  36
336800050623XX XX  X X X-XXXXXXXXxX XX/XX/XX xxxxxx,x x xx,xxx Sxxxxx,xSxx,xxx  37
336900130118            REASON      DELIVERY WEIGHT  VOLUME cm    DIFFERENZA    38
337000050704           OF NON DEL.    DATE    VDL      VDL     XXXXXXXXXXXXXXX  39
337100930616**
337200090604  BEFORE                                                            01
337300090604  AFTER                                                             02
337400090604ARRIVO                                                              03
337500090604PARTENZA                                                            04
337600930719**
337700930719BOLLE IN PARTENZA/ARRIVO ......:                                     1
337800950302TIPO CONSEGNA SPEDIZIONI ......:                                     2
337900950302DATA CONSEGNA .......: DAL              AL                           3
338000930719AFFIDABILITA' CLIENTI .........:                                     4
338100930719AFFIDABILITA' CONSEGNE ........:                                     5
338200940331LINEA DI ARRIVO ...............:                                     6
338300930719CODICE CLIENTE ................:                                     7
338400950302ORE MINIME DI RITARDO CONSEGNA :                                     8
338500950116TIPO SERVIZIO .................:                                     9
338600931124SONO ESCLUSE LE SPEDIZIONI DI RECUPERO                              10
338700931124                                                                    11
338800931014TIPO RITARDO SPEDIZIONI........:                                    12
338900950302DATA SPEDIZIONE .....: DAL              AL                          13
339000940401LINEA DI PARTENZA .............:                                    14
339100940527BOLLE IN DISTINTA .............:                                    15
339200950301STAMPA DATI AFFIDABILITA' .....:                                    16
339300961122ELABORAZIONE X DATA O AUTOTR. .:                                    17
339400961122COD.AUTOTRASPORTATORE: DAL              AL                          18
339500950302DATA RITIRO MERCE ...: DAL              AL                          19
339600950301MATTINO / POMERIGGIO ..........:                                    20
339700950301NUMERO RITIRO .................:                                    21
339800950301DETTAGLIO O SOLO TOTALI .......:                                    22
339900960614STAMPA DELLE SOLE ORE RITARDO  :                                    23
340000980209DATA TRASMISSIONE....: DAL              AL                          24
340100050623VDL-STAMPA DIFFERENZE DI PESO .:                                    25
340200050623VDL-STAMPA DIFFERENZE DI VOLUME:                                    26
340300070302NAZIONE/C.A.P. DESTINATARIO ...:            XXX / XXXXXXXXX         27
340400070515AFFIDABILITA' PARTENZA ........:                                    28
340500130114CREAZIONE file EXCEL ..........: SI                                 29
340600130114Indirizzo e-mail ..............:                                    30
340700130115RAGGRUPPAMENTO Cod.Mittenti ...:                                    31
340800950303**
340900950303OVRPRTF FILE(PRTF198) USRDTA('Spedizioni')
