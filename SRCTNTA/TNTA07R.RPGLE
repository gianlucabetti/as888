000100020708     H DECEDIT('0,') DATEDIT(*ymd.) option(*nodebugio)
000200940822     F* TNTA07R *-----------------------------------------------------*H
000300940822     F*         STAMPA ANAGRAFICO CLIENTI IN FILIALE  - FILTRO        *
000400000000     F*---------------------------------------------------------------*H
000500940822     FTNTA07D   CF   E             WORKSTN
000600900831     FAZORG01L  IF   E           K DISK
000700920613     FTABEL00F  IF   E           K DISK
000800130801     fAZCMM01L  if   e           k disk
000900130805
001000130805       // -?Costante per controllo "caratteri solo numerici"?
001100130805     d c_Digits        c                   const('0123456789')
001200040119
001300051128     D MSG             S             79    DIM(20) CTDATA PERRCD(1)
001400040929
001500040929     d codut           s              1  0 Inz(1)
001600940822     D*
001700091103/*?******************************************************************/
001800091103     d*?  ATTENZIONE!!! SE VIENE CAMBIATA QUESTA DS BISONGA SISTEMARE
001900091103     d*?                IL PGM TISEN1R CHE UTILIZZA LA DS
002000091103/*?******************************************************************/
002100000000     D PARAM           DS
002200940822     D  V1CFIL                 1      3  0
002300940822     D  V1CSC1                 4      4
002400940822     D  V1CSC2                 5      5
002500940822     D  V1CSC3                 6      6
002600940822     D  V1CCIC                 7      7
002700060227      * tipo utilizzo peso VDL
002800060310     D  V1Ctap                 8      8
002900020306     D  V1CNTR                 9      9
003000940822     D  V1CSAC                10     10
003100040119     D  V1CIST                11     11
003200060310      * flag rinuncia ac base
003300060310     d  v1cracb               12     12
003400060310      * tariffa part. AC BASE
003500060310     d  v1cbase               13     13
003600051213     D  V1Cblk                14     14
003700110921      * tariffa particolare
003800110921     d  v1cctp                15     15
003900110922     d  v1cstp                16     16
004000070129
004100940822     D  V1CSNC                22     22
004200940822     D  V1CSCT                23     27
004300091103     d
004400020307     D  V1CTSP                33     33
004500020306     D  V1CTTR                94     94
004600020306     D  V1CCPA                95     96
004700020306     D  V1CBAM                97     97
004800001201     D* Stampa Note Clienti
004900020306     D  V1STPS                98     98
005000020306     D  V1STPN                99    100
005100020306     D  V1NOTE               101    101
005200020306     D  V1TNOT               102    103
005300020306     D  V1CNTW               104    106
005400011211     D* Agente
005500020306     D  V1AGE                107    113
005600020404     D  V1CCNO               114    114
005700020426     D* Intervallo mese/anno per richiesta clienti non operativi
005800020426     D  V1CDMM               115    116  0 inz
005900020426     D  V1CDAA               117    118  0 inz
006000020426     D  V1CAMM               119    120  0 inz
006100020426     D  V1CAAA               121    122  0 inz
006200040601      * Tipo merce
006300040601     d  v1cbva               123    123
006400040929      * dati necessari x il caricamento della sk fig
006500040929     d  prmabi               124    125
006600040929     d  prmcdi               126    126
006700040929     d  prmcar               127    129  0
006800040929     d  prmcpo               130    132  0
006900050617     d* Tipo pagamento C/A
007000050617     d  v1ctpc               133    133
007100051128     d* Luogo
007200051128     D  V1luog               134    134
007300051128     D  V1Tluo               135    137
007400121227      * Tassazione POST fatturazione
007500121227     d  V1Ctpf               138    138
007600950707     D*
007700960424     D* DS PER TRTB69R - TRTB70R - TRTB72R - TRTB73R - INT/GEST PARTIC.
007800960424     D DS7PQR        E DS                  EXTNAME(DS7PQRS)
007900020306     D*
008000020306     D* DS PER TRTB69R - TRTB70R - TRTB72R - TRTB73R - INT/GEST PARTIC.
008100020306     D S02DS         E DS                  EXTNAME(TIBS02DS)
008200940822     D*
008300000000     D KPJBA         E DS
008400040929
008500040929     d Azuteds       e ds                  extname(Azute00f)
008600040929     d dDatiute      e ds
008700040929     d Tibs34ds      e ds
008800090707     d tntaa1ds      e ds
008900110921     d TRTB24DS      e ds
009000130801     d TRMK43ds      e ds                  inz
009100940822     C*****************************************************************
009200940822     C* RIEPILOGO INDICATORI
009300940822     C*****************************************************************
009400950711     C* 08    - QUALSIASI TASTO VENGA PREMUTO --> ESCO DAL PROGRAMMA
009500040929      * 10    - Sono in SEDE o autorizzazione 'DI' = distretto
009600940822     C* 28    - INDICATORE DI EMISSIONE MESSAGGIO DI ERRORE
009700940822     C* 30    - DI COMODO
009800001201     C* 40/49 - ERRORI
009900940822     C*****************************************************************
010000940822     C*
010100001201     C* EMISSIONE FORMATO 1
010200940822     C     FOR01         TAG
010300940822     C                   EXFMT     TA07D01
010400940822     C*
010500940822     C* PULIZIA CAMPO MESSAGGIO E RELATIVO INDICATORE (*IN28)
010600940822     C                   CLEAR                   V1CMSG
010700940822     C* AZZERO GLI INDICATORI REALTIVI AI MESSAGGI DI ERRORE
010800020429     C                   SETOFF                                       284043
010900020429     C                   SETOFF                                       454651
011000940822     C*
011100950711     C** CMD3 - FINE LAVORO
011200950711     C   KC
011300950711     C** 08 ON  - QUALSIASI TASTO VENGA PREMUTO, ESCO DAL PROGRAMMA
011400950711     COR 08              GOTO      FINE
011500950711     C*
011600001201     C* CONTROLLI FORMATO 1
011700940822     C                   EXSR      CTRD01
011800020306     C   28
011900100607     COR 27              GOTO      FOR01
012000001201      *
012100001201     C     FOR02         TAG
012200011211     C*
012300011211     C* DECODIFICA CODICE AGENTE PER EMISSIONE FORMATO 2
012400011211     C                   CLEAR                   V1AGED
012500011211     C     V1AGE         IFNE      *BLANKS
012600130801     C                   MOVE      V1AGE         CMMcod
012700130801     C     CMMcod        CHAIN     AZCMM000                           30
012800130801     C                   IF        %found(AZCMM01L)
012900130801     C                   MOVEL     CMMdes        V1AGED
013000011211     C                   ENDIF
013100011211     C                   ENDIF
013200011211     C* EMISSIONE FORMATO 2
013300001201     C                   EXFMT     TA07D02
013400001201     C*
013500001201     C* PULIZIA CAMPO MESSAGGIO E RELATIVO INDICATORE (*IN28)
013600001201     C                   CLEAR                   V2CMSG
013700001201     C* AZZERO GLI INDICATORI REALTIVI AI MESSAGGI DI ERRORE
013800001201     C                   SETOFF                                       414249
013900001201     C                   SETOFF                                       442848
014000020429     C                   SETOFF                                       505247
014100051128     C                   SETOFF                                       515354
014200100607     C                   SETOFF                                       56
014300001201     C*
014400050617     C** F3 - FINE LAVORO
014500001201     C   KC
014600001201     C** 08 ON  - QUALSIASI TASTO VENGA PREMUTO, ESCO DAL PROGRAMMA
014700001201     COR 08              GOTO      FINE
014800001201     C** CMD12 - RITORNO
014900001201     C   KL              GOTO      FOR01
015000001201     C*
015100001201     C* CONTROLLI FORMATO 2
015200001201     C                   EXSR      CTRD02
015300100607     c   27
015400100607     Cor 28
015500001201     CORNKF              GOTO      FOR02
015600950711     C*
015700050617     C* F6 - CONFERMA ELABORAZIONE
015800111006     c                   IF        V1Csct = *blanks
015900111006     c                   eval      V1Csct = *zeros
016000111006     c                   ENDIF
016100000000     C                   MOVEL     PARAM         KPJBU
016200020430     C                   MOVE      'TA08'        KCOAZ
016300110923     C                   CALL      'BCH10'
016400110923     C****               CALL      'TNTA08R'
016500020408     C                   PARM                    KPJBA
016600920613     C*
016700000000     C     FINE          TAG
016800090707     C                   clear                   tntaa1ds
016900090707     C                   movel     'C'           Itaa1Tla
017000090707     C                   movel(p)  tntaa1ds      kpjbu
017100090707     C                   CALL      'TNTAA1C'
017200090707     C                   PARM                    KPJBA
017300090707     c
017400090707     C                   SETON                                        LR
017500940822     C*
017600950707     C*--- INZSR INIZIALE --------------------------------------------*
017700950707     C     *INZSR        BEGSR
017800950707     C*
017900950707     C     *ENTRY        PLIST
018000950707     C                   PARM                    KPJBA
018100040929
018200040929     c     *dtaara       define    §azute        azuteds
018300040929     c     *dtaara       define    §datiute      ddatiute
018400040929     c                   in(E)     *dtaara
018500040929     c                   If        %error  or RSUT = *blanks
018600040929     c                   Clear                   Tibs34ds
018700040929     c                   Call      'TIBS34R'
018800040929     c                   Parm                    Tibs34ds
018900040929     c                   In        *dtaara
019000040929     c                   EndIf
019100040929
019200040930     c                   Movel     rsut          V1CRSU
019300020429     C***
019400020429     C* GIRO DATA DEL GIORNO: LA PRENDO DA TIME
019500020429     C***
019600020429     C                   TIME                    W0140            14 0
019700020429     C                   MOVE      W0140         UDATE8            8 0
019800020429     C                   MOVE      UDate8        UDate4            4 0
019900020429     C                   MOVE      UDate8        UDate6            6 0
020000020429     C                   MOVEL     UDate6        UDate2            2 0
020100950707     C*
020200950707     C***
020300950707     C* CAMPI CHE INIZIALIZZO SOLO ALL'ENTRATA DEL PROGRAMMA E BASTA
020400950707     C***
020500040929     C* SE SONO UN secondo livello IMPOSTO LA FILIALE A VIDEO
020600040929    1C                   IF        DutLpo = '2' or DutLpo = *Blanks
020700040929     C                   MOVEL     DutPou        V1CFIL
020800950707   X1C                   ELSE
020900980217     C                   MOVEL     *ALL'0'       V1CFIL
021000950707    1C                   ENDIF
021100950707     C*
021200950707     C                   MOVE      'A'           V1CSC1
021300950707     C                   MOVE      'N'           V1CSC2
021400950707     C                   MOVE      'N'           V1CSC3
021500950707     C                   MOVE      ' '           V1CCIC
021600950707     C                   MOVE      'N'           V1CSNC
021700060310     c                   Clear                   v1ctap
021800040119     C                   MOVE      'N'           V1Cist
021900950707     C                   MOVE      'N'           V1CSAC
022000060310     c                   move      'N'           v1cracb
022100060310     c                   move      'N'           v1cbase
022200110921     c                   clear                   v1cctp
022300110922     c                   clear                   v1cstp
022400051213     C                   MOVE      'N'           V1Cblk
022500950707     C                   MOVE      ' '           V1CTSP
022600960424     C                   MOVE      ' '           V1CTTR
022700040601     c                   Clear                   v1cbva
022800960424     C                   MOVE      '  '          V1CCPA
022900970320     C                   MOVE      ' '           V1CBAM
023000001201     C                   MOVE      'NO'          V1STPN
023100001201     C                   MOVE      'N'           V1NOTE
023200051128     C                   MOVE      'N'           V1luog
023300020306     C                   MOVE      'S'           V1STPS
023400020306     C                   MOVE      'N'           V1CNTR
023500020404     C                   MOVE      'S'           V1CCNO
023600050617     C                   MOVE      'N'           V1Ctpc
023700050617     c                   eval      v1dtpc='No selezione'
023800121227     c                   eval      V1Ctpf = 'N'
023900020429      * imposto periodo per richiesta clienti non operativi
024000020429      * uguale agli ultimi 12 mesi, escludendo quello in corso
024100020429      *
024200020429      *  Calcolo anno iniziale e finale
024300020429   1aC     Udate2        ifne      1
024400020429     C                   move      UDate8        WAnnoF            4 0
024500020429     C     WAnnoF        sub       1             WAnnoI            4 0
024600020429     C                   else
024700020429     C     Udate8        sub       1             WAnnoF
024800020429     C     UDate8        sub       1             WAnnoI
024900020429   1-C                   endif
025000020429     C                   move      WAnnoI        V1CDAA
025100020429     C                   move      WAnnoF        V1CAAA
025200020429      *
025300020429      * Partiamo sempre da 12 mesi prima
025400020429     C                   move      UDate2        V1CDMM            2 0
025500020429      *
025600020429   1aC     V1CDMM        ifne      1
025700020429     C     V1CDMM        sub       1             V1CAMM            2 0
025800020429     C                   else
025900020429     C                   move      12            V1CAMM            2 0
026000020429   1-C                   endif
026100020429      *
026200020429      * Mi salvo l'intervallo di default
026300020429     C                   z-add     *zeros        DataInizioDFT     6 0
026400020429     C                   eval      DataInizioDFT = (WAnnoI * 100) + V1CDMM
026500020429     C                   z-add     *zeros        DataFineDFT       6 0
026600020429     C                   eval      DataFineDFT   = (WAnnoF * 100) + V1CAMM
026700041008
026800090707     c* Richiamo solo per avere codice di abilitazione
026900090707     c                   clear                   tntaa1ds
027000090707     c                   eval      itaa1caut='§utegtc'
027100090707     c                   clear                   kpjbu
027200090707     c                   movel     tntaa1ds      kpjbu
027300090707     c                   call      'TNTAA1C'
027400090707     c                   parm                    kpjba
027500090707     c                   movel     kpjbu         tntaa1ds
027600090707     C* UTENTE NON abilitato
027700090707     c                   if        otaa1fabi='N'
027800041008     c                   Seton                                        402808
027900041008     C                   movel     msg(07)       v1cmsg
028000041008e   1c                   EndIf
028100090707     c
028200041008      * Reperimento dei P.O. gestibili dall'utente
028300090707     c                   Eval      prmabi = otaa1cabi
028400090707     c                   Eval      prmcdi = dutdis
028500090707     c                   Eval      prmcar = dutare
028600090707     c                   Eval      prmcpo = dutpou
028700020429      *
028800041008      * controllo se sono in sede o se autorizzazione 'DI'
028900090707     c                   If        Simfel = *Zeros or otaa1cabi = 'DI'
029000041008     c                   eval      *in10 = *On
029100041008     c                   EndIf
029200950707     C***
029300950707     C* KLIST
029400950707     C***
029500950707     C* ACCESSO TABEL00F
029600950707     C     KTAB          KLIST
029700020306     C                   KFLD                    CODUT
029800950707     C                   KFLD                    COD
029900950707     C                   KFLD                    KEY
030000950707     C***
030100950707     C* DEFINIZIONE CAMPI
030200950707     C***
030300950707     C     *LIKE         DEFINE    TBLCOD        COD
030400950707     C     *LIKE         DEFINE    TBLKEY        KEY
030500950707     C                   ENDSR
030600950707     C*
030700001201     C*--- CONTROLLI FORMATO  1---------------------------------------*
030800940822     C     CTRD01        BEGSR
030900100607     C                   SETOFF                                       27
031000940822     C*
031100940822     C****  CODICE FILIALE  ****
031200020415
031300020415      * non accetto il codice 999 se sono in sede
031400040929      * o se autorizzazione 'DI'
031500020415     c                   if        *in10 and v1cfil = 999
031600020415     c                   eval      *in40 = *on
031700020415     c                   eval      *in28 = *on
031800020415     c                   eval      v1cmsg = msg(1)
031900020415     c                   goto      endctr
032000020415     c                   endif
032100020415
032200940822     C     V1CFIL        IFGT      *ZERO
032300940822     C     V1CFIL        ANDNE     999
032400090707     c                   clear                   tntaa1ds
032500090707     c                   eval      itaa1caut='§utegtc'
032600090707     c                   eval      itaa1fil =v1cfil
032700090707     c                   clear                   kpjbu
032800090707     c                   movel     tntaa1ds      kpjbu
032900090707     c                   call      'TNTAA1C'
033000090707     c                   parm                    kpjba
033100090707     c                   movel     kpjbu         tntaa1ds
033200090707     c
033300090707     c* Utente non abilitato alla gestione della filiale
033400090707     c                   if        otaa1fabi='N'
033500950711     C                   MOVEL     MSG(8)        V1CMSG
033600950711     C                   SETON                                        4028
033700950711     C                   GOTO      ENDCTR
033800950717     C                   ENDIF
033900950711     C*
034000940822     C     V1CFIL        CHAIN     AZORG                              30
034100940822     C     *IN30         IFEQ      *ON
034200940822     C                   MOVEL     MSG(1)        V1CMSG
034300940822     C                   SETON                                        4028
034400940822     C                   GOTO      ENDCTR
034500940822     C                   ENDIF
034600940822     C                   ENDIF
034700110921
034800110921       //?Ricerca tariffa particolare
034900110921     c                   IF        %scan('?' : V1Cctp) > *zero
035000110921     c                   clear                   TRTB24DS
035100110921     c                   eval      TB24fun = '1'
035200110921     c                   eval      TB24ass = 'S'
035300110921     c                   eval      kpjbu = TRTB24DS
035400110921     c                   call      'TRTB24R'
035500110921     c                   parm                    kpjba
035600110921     c                   eval      TRTB24DS = kpjbu
035700110921     c                   IF        TB24cod <> *blanks
035800110921     c                   eval      V1Cctp = TB24cod
035900110921     c                   ENDIF
036000110921     c                   ENDIF
036100110921
036200110921       //?Controllo tariffa particolare
036300110921     c                   IF        V1Cctp <> *blanks
036400110921     c                   eval      cod = '1P'
036500110921     c                   eval      key = V1Cctp
036600110921     c     ktab          chain     TABEL00F
036700110921     c                   IF        not %found(TABEL00F) or
036800110921     c                             TBLflg <> *blanks
036900110921     c                   eval      V1Cmsg = msg(04)
037000110921     c                   eval      *in28 = *on
037100110921     c                   eval      *in42 = *on
037200110921     c                   leavesr
037300110921     c                   ENDIF
037400110921       //?controllo tra tariffa particolare e il flag di come deve
037500110921       //?essere la tariffa particolare
037600110921     c                   IF        V1Cstp = *blanks
037700110921     c                   eval      V1Cmsg = msg(06)
037800110921     c                   eval      *in28 = *on
037900110921     c                   eval      *in44 = *on
038000110921     c                   leavesr
038100110921     c                   ENDIF
038200110921     c                   ELSE
038300110921     c                   IF        V1Cstp <> *blanks
038400110921     c                   eval      V1Cmsg = msg(04)
038500110921     c                   eval      *in28 = *on
038600110921     c                   eval      *in42 = *on
038700110921     c                   leavesr
038800110921     c                   ENDIF
038900110921     c                   ENDIF
039000110921
039100940822     C*
039200940822     C****  TIPO SERVIZIO  ****
039300940822     C     V1CTSP        IFEQ      '?'
039400940822     C                   MOVEL     '5E'          COD
039500940822     C                   MOVEL     ' '           V1CTSP
039600940822     C                   MOVEL     *BLANKS       DES              25
039700020314     C                   MOVEL     *BLANKS       KEY
039800940822     C                   CALL      'X§TABER'
039900940822     C                   PARM                    CODUT
040000940822     C                   PARM                    COD
040100940822     C                   PARM                    KEY
040200940822     C                   PARM                    DES
040300940822     C                   MOVEL     KEY           V1CTSP
040400940822     C                   SETON                                        27
040500940822     C                   GOTO      ENDCTR
040600940822     C                   ENDIF
040700940822     C*
040800940822     C* CONTROLLO VALIDITA' TIPO SERVIZIO
040900940822     C     V1CTSP        IFNE      ' '
041000940822     C                   MOVEL     '5E'          COD
041100940822     C                   MOVEL     *BLANKS       KEY
041200940822     C                   MOVEL     V1CTSP        KEY
041300940822     C     KTAB          CHAIN     TABEL                              30
041400940822     C  N30TBLFLG        IFNE      ' '
041500940822     C                   SETON                                        30
041600940822     C                   ENDIF
041700940822     C* TIPO SERVIZIO ERRATO
041800940822     C     *IN30         IFEQ      *ON
041900940822     C                   MOVEL     MSG(5)        V1CMSG
042000940822     C                   SETON                                        4328
042100940822     C                   GOTO      ENDCTR
042200940822     C                   ENDIF
042300940822     C                   ENDIF
042400940822     C*
042500960424     C****  TIPO TARIFFA  ****
042600960424     C     V1CTTR        IFEQ      '?'
042700960424     C                   MOVEL     'TR'          COD
042800960424     C                   MOVEL     ' '           V1CTTR
042900960424     C                   MOVEL     *BLANKS       DES              25
043000020314     C                   MOVEL     *BLANKS       KEY
043100960424     C                   CALL      'X§TABER'
043200960424     C                   PARM                    CODUT
043300960424     C                   PARM                    COD
043400960424     C                   PARM                    KEY
043500960424     C                   PARM                    DES
043600960424     C                   MOVEL     KEY           V1CTTR
043700960424     C                   SETON                                        27
043800960424     C                   GOTO      ENDCTR
043900960424     C                   ENDIF
044000960424     C*
044100960424     C* CONTROLLO VALIDITA' TIPO TARIFFA
044200960424     C     V1CTTR        IFNE      ' '
044300960424     C                   MOVEL     'TR'          COD
044400960424     C                   MOVEL(P)  V1CTTR        KEY
044500960424     C     KTAB          CHAIN     TABEL                              30
044600960424     C  N30TBLFLG        IFNE      ' '
044700960424     C                   SETON                                        30
044800960424     C                   ENDIF
044900960424     C* TIPO TARIFFA ERRATO
045000960424     C     *IN30         IFEQ      *ON
045100960424     C                   MOVEL     MSG(9)        V1CMSG
045200960424     C                   SETON                                        4528
045300960424     C                   GOTO      ENDCTR
045400960424     C                   ENDIF
045500960424     C                   ENDIF
045600960424     C*
045700960424     C****  PARTICOLARITA'  ****
045800960424     C     V1CCPA        IFNE      '  '
045900960424     C                   MOVEL     'E'           WERR              1
046000960424     C*
046100960424     C* PARTICOLARITA' CONTRASSEGNO
046200960424     C                   CLEAR                   DS7PQR
046300960424     C                   MOVEL     'S'           DS7RIC
046400960424     C                   MOVEL     'C'           DS7GES
046500960424     C                   MOVEL     V1CCPA        DS7COD
046600960424     C*
046700960424     C* PARTICOLARITA' GIACENZA
046800960424     C                   CLEAR                   DS7PQR
046900960424     C                   MOVEL     'S'           DS7RIC
047000960424     C                   MOVEL     'C'           DS7GES
047100960424     C                   MOVEL     V1CCPA        DS7COD
047200960424     C                   MOVEL     DS7PQR        KPJBU
047300960424     C                   CALL      'TRTB69R'
047400960424     C                   PARM                    KPJBA
047500960424     C                   MOVEL     KPJBU         DS7PQR
047600960424     C     DS7ERR        IFEQ      ' '
047700960424     C                   CLEAR                   WERR
047800960424     C                   ENDIF
047900960424     C*
048000960424     C* PARTICOLARITA' CONSEGNA
048100960424     C                   CLEAR                   DS7PQR
048200960424     C                   MOVEL     'S'           DS7RIC
048300960424     C                   MOVEL     'C'           DS7GES
048400960424     C                   MOVEL     V1CCPA        DS7COD
048500960424     C                   MOVEL     DS7PQR        KPJBU
048600960424     C                   CALL      'TRTB70R'
048700960424     C                   PARM                    KPJBA
048800960424     C                   MOVEL     KPJBU         DS7PQR
048900960424     C     DS7ERR        IFEQ      ' '
049000960424     C                   CLEAR                   WERR
049100960424     C                   ENDIF
049200960424     C*
049300960424     C* PARTICOLARITA' VARIE
049400960424     C                   CLEAR                   DS7PQR
049500960424     C                   MOVEL     'S'           DS7RIC
049600960424     C                   MOVEL     'C'           DS7GES
049700960424     C                   MOVEL     V1CCPA        DS7COD
049800960424     C                   MOVEL     DS7PQR        KPJBU
049900960424     C                   CALL      'TRTB73R'
050000960424     C                   PARM                    KPJBA
050100960424     C                   MOVEL     KPJBU         DS7PQR
050200960424     C     DS7ERR        IFEQ      ' '
050300960424     C                   CLEAR                   WERR
050400960424     C                   ENDIF
050500960424     C*
050600960424     C* WERR <> " " ---> CODICE PARTICOLARITA' INESISTENTE
050700960424     C     WERR          IFNE      ' '
050800960424     C                   MOVEL     MSG(10)       V1CMSG
050900960424     C                   SETON                                        4628
051000960424     C                   GOTO      ENDCTR
051100960424     C                   ENDIF
051200960424     C*
051300960424     C                   ENDIF
051400940822     C*
051500020306     C*
051600020306     C* CONTROLLO VALIDITA' TIPO TARIFFA
051700020306     C     V1CTTR        IFNE      ' '
051800020306     C                   MOVEL     'TR'          COD
051900020306     C                   MOVEL(P)  V1CTTR        KEY
052000020306     C     KTAB          CHAIN     TABEL                              30
052100020306     C  N30TBLFLG        IFNE      ' '
052200020306     C                   SETON                                        30
052300020306     C                   ENDIF
052400020306     C* TIPO TARIFFA ERRATO
052500020306     C     *IN30         IFEQ      *ON
052600020306     C                   MOVEL     MSG(9)        V1CMSG
052700020306     C                   SETON                                        4528
052800020306     C                   GOTO      ENDCTR
052900020306     C                   ENDIF
053000020306     C                   ENDIF
053100940822     C     ENDCTR        ENDSR
053200001201     C*
053300001201     C*--- CONTROLLI FORMATO  2---------------------------------------*
053400001201     C     CTRD02        BEGSR
053500001201     C                   SETOFF                                       27
053600060914     C****  NETWORK   ****
053700060914     C     '?'           SCAN      V1CNTW                                 27
053800060914     C     *IN27         IFEQ      *ON
053900060914      * PER RIEMETTERE IL VIDEO ALMENO UNA VOLTA DOPO LE RICERCHE
054000060914     C                   CLEAR                   S02DS
054100060914     C                   MOVEL     'R'           T02MOD
054200060914     C                   MOVEL     KNSIF         T02SIF
054300060914     C                   MOVEL     'NTW'         T02COD
054400060914     C                   CALL      'TIBS02R'
054500060914     C                   PARM                    KPJBA
054600060914     C                   PARM                    S02DS
054700060914     C     T02ERR        IFEQ      *BLANKS
054800060914     C                   MOVEL     T02KE1        V1CNTW
054900100607     c                   else
055000100607     c                   clear                   v1cntw
055100060914     C                   ENDIF
055200060914     C                   GOTO      ENDCT2
055300060914     C                   ENDIF
055400060914     C*
055500060914     C* CONTROLLO VALIDITA' NETWORK
055600060914     C     V1CNTW        IFNE      *BLANKS
055700060914     C     V1CNTW        IFNE      'ITA'
055800060914     C     V1CNTW        ANDNE     'EST'
055900060914     C                   CLEAR                   S02DS
056000060914     C                   MOVEL     'C'           T02MOD
056100060914     C                   MOVEL     KNSIF         T02SIF
056200060914     C                   MOVEL     'NTW'         T02COD
056300060914     C                   MOVEL     *BLANKS       T02KE1
056400060914     C                   MOVEL     V1CNTW        T02KE1
056500060914     C                   CALL      'TIBS02R'
056600060914     C                   PARM                    KPJBA
056700060914     C                   PARM                    S02DS
056800060914      *
056900060914     C     T02ERR        IFNE      *BLANKS
057000060914     C     V1CNTW        OREQ      'LOG'
057100060914     C                   MOVEL     MSG(15)       V2CMSG
057200100607     C                   SETON                                        2856
057300060914     C                   GOTO      ENDCT2
057400060914     C                   ENDIF
057500060914     C                   ENDIF
057600060914      *
057700060914      * SE NETWORK TARIFFE 'S' O 'E' SEGNALO ERRORE
057800060914     C                   ELSE
057900060914     C     V1CNTR        IFEQ      'S'
058000060914     C     V1CNTR        OREQ      'E'
058100060914     C                   MOVEL     MSG(16)       V2CMSG
058200100607     C                   SETON                                        2856
058300060914     C                   GOTO      ENDCT2
058400060914     C                   ENDIF
058500060914     C                   ENDIF
058600060914     C* CONTROLLO SE SCELTA INCONGRUENTE
058700060914     C     V1CNTW        IFNE      *BLANKS
058800060914     C     V1CNTR        ANDNE     'S'
058900060914     C     V1CNTW        IFNE      *BLANKS
059000060914     C     V1CNTR        ANDNE     'E'
059100060914     C                   MOVEL     MSG(3)        V2CMSG
059200100607     C                   SETON                                        2856
059300060914     C                   GOTO      ENDCT2
059400060914     C                   ENDIF
059500060914     C                   ENDIF
059600111006
059700111006      * Se selezione per categoria merceologica devo impostare anche il
059800111006      * codice della categoria merceologica
059900111006     c                   IF        V1Csnc = 'S' and V1Csct = *blanks
060000111006     c                   eval      v2cmsg = msg(02)
060100111006     c                   eval      *in28 = *on
060200111006     c                   eval      *in41 = *on
060300111006     c                   leavesr
060400111006     c                   ENDIF
060500001201     C*
060600001201     C****  CODICE CATEGORIA SEAT  ****
060700001201     C     '?'           SCAN      V1CSCT                                 27
060800001201     C*
060900001201     C     *IN27         IFEQ      *ON
061000001201     C                   MOVEL     '1L'          COD
061100001201     C                   CLEAR                   V1CSCT
061200001201     C                   CLEAR                   DES
061300020314     C                   CLEAR                   KEY
061400001201     C                   CALL      'X§TABER'
061500001201     C                   PARM                    CODUT
061600001201     C                   PARM                    COD
061700001201     C                   PARM                    KEY
061800001201     C                   PARM                    DES
061900001201     C                   MOVEL     KEY           V1CSCT
062000001204     C                   GOTO      ENDCT2
062100001201     C                   ENDIF
062200001201     C*
062300001201     C* CONTROLLO VALIDITA' CODICE CATEGORIA SEAT
062400001201     C     V1CSCT        IFGT      *ZEROS
062500111006     C**!!!V1CSCT        ANDNE     '99999'
062600001201     C                   MOVE      '1L'          COD
062700001201     C                   MOVEL     *BLANKS       KEY
062800001201     C                   MOVEL     V1CSCT        KEY
062900001201     C     KTAB          CHAIN     TABEL                              30
063000001201     C  N30TBLFLG        IFNE      ' '
063100001201     C                   SETON                                        30
063200001201     C                   ENDIF
063300001201     C* CODICE CATEGORIA ERRATO
063400001201     C     *IN30         IFEQ      *ON
063500020312     C                   MOVEL     MSG(2)        V2CMSG
063600001201     C                   SETON                                        4128
063700001204     C                   GOTO      ENDCT2
063800001201     C                   ENDIF
063900001201     C                   ENDIF
064000001201     C* CONTROLLO SE SCELTA INCONGRUENTE
064100001201     C     V1CSCT        IFGT      *ZEROS
064200001201     C     V1CSNC        ANDNE     'S'
064300020312     C                   MOVEL     MSG(3)        V2CMSG
064400001201     C                   SETON                                        4128
064500001204     C                   GOTO      ENDCT2
064600001201     C                   ENDIF
064700001201     C*
064800001201     C* METTO *ZEROS AL POSTO DEI *BLANKS
064900111006     C**!!!V1CSCT        IFLT      *ZEROS
065000111006     C**!!!              MOVE      *ZEROS        V1CSCT
065100111006     C**!!!              ENDIF
065200131211     c* verifico se il campo diverso da blank è numerico
065300131211     c                   If        v1csct <> *blanks and
065400131211     c                             %check(c_digits:v1csct) > 0
065500131211     C                   MOVEL     MSG(2)        V2CMSG
065600131211     C                   SETON                                        4128
065700131211     C                   GOTO      ENDCT2
065800131211     C                   ENDIF
065900001201     C*
066000001201     C****  TIPO NOTA CLIENTE  ****
066100001201     C     '?'           SCAN      V1TNOT                                 27
066200001201     C*
066300001201     C     *IN27         IFEQ      *ON
066400001201     C                   MOVEL     '1T'          COD
066500001201     C                   CLEAR                   V1TNOT
066600001201     C                   CLEAR                   DES
066700020314     C                   CLEAR                   KEY
066800001201     C                   CALL      'X§TABER'
066900001201     C                   PARM                    CODUT
067000001201     C                   PARM                    COD
067100001201     C                   PARM                    KEY
067200001201     C                   PARM                    DES
067300001201     C                   MOVEL     KEY           V1TNOT
067400001201     C                   GOTO      ENDCT2
067500001201     C                   ENDIF
067600001201     C*
067700001201     C* CONTROLLO VALIDITA' TIPO NOTA CLIENTE
067800001201     C     V1TNOT        IFGT      *BLANKS
067900001201     C                   MOVE      '1T'          COD
068000001201     C                   MOVEL     *BLANKS       KEY
068100001201     C                   MOVEL     V1TNOT        KEY
068200001201     C     KTAB          CHAIN     TABEL                              30
068300001201     C  N30TBLFLG        IFNE      ' '
068400001201     C                   SETON                                        30
068500001201     C                   ENDIF
068600001201     C* TIPO NOTA CLIENTE  ERRATO
068700001201     C     *IN30         IFEQ      *ON
068800001201     C                   MOVEL     MSG(12)       V2CMSG
068900001201     C                   SETON                                        4828
069000001201     C                   GOTO      ENDCT2
069100001201     C                   ENDIF
069200001201     C                   ENDIF
069300001201     C*
069400001201     C* CONTROLLO SCELTA INCONGRUENTE
069500001201     C     V1TNOT        IFGT      *BLANKS
069600001201     C     V1NOTE        ANDEQ     'N'
069700020312     C                   MOVEL     MSG(13)       V2CMSG
069800001201     C                   SETON                                        4928
069900001204     C                   GOTO      ENDCT2
070000001201     C                   ENDIF
070100051128     C****  COD. LUOGO         ****
070200051128     C     '?'           SCAN      V1Tluo                                 27
070300051128     C*
070400051128     C     *IN27         IFEQ      *ON
070500051128     C                   MOVEL     '4L'          COD
070600051128     C                   CLEAR                   V1Tluo
070700051128     C                   CLEAR                   DES
070800051128     C                   CLEAR                   KEY
070900051128     C                   CALL      'X§TABER'
071000051128     C                   PARM                    CODUT
071100051128     C                   PARM                    COD
071200051128     C                   PARM                    KEY
071300051128     C                   PARM                    DES
071400051128     C                   MOVEL     KEY           V1Tluo
071500051128     C                   GOTO      ENDCT2
071600051128     C                   ENDIF
071700051128     C*
071800051128     C* CONTROLLO VALIDITA' codice luogo
071900051128     C     V1Tluo        IFGT      *BLANKS
072000051128     C                   MOVE      '4L'          COD
072100051128     C                   MOVEL     *BLANKS       KEY
072200051128     C                   MOVEL     V1Tluo        KEY
072300051128     C     KTAB          CHAIN     TABEL                              30
072400051128     C  N30TBLFLG        IFNE      ' '
072500051128     C                   SETON                                        30
072600051128     C                   ENDIF
072700051128     C* cod. luogo errato
072800051128     C     *IN30         IFEQ      *ON
072900051128     C                   MOVEL     MSG(20)       V2CMSG
073000051128     C                   SETON                                        5428
073100051128     C                   GOTO      ENDCT2
073200051128     C                   ENDIF
073300051128     C                   ENDIF
073400051128     C*
073500051128     C* CONTROLLO SCELTA INCONGRUENTE
073600051128     C     V1Tluo        IFGT      *BLANKS
073700051128     C     V1luog        ANDEQ     'N'
073800051128     C                   MOVEL     MSG(13)       V2CMSG
073900051128     C                   SETON                                        5328
074000051128     C                   GOTO      ENDCT2
074100051128     C                   ENDIF
074200011211     C*
074300011211     C****  CODICE AGENTE ****
074400011211     C     '?'           SCAN      V1AGE                                  27
074500011211     C*
074600011211     C     *IN27         IFEQ      *ON
074700130801     c                   clear                   TRMK43ds
074800011211     C* SE VALORIZZATO PASSO IL PUNTO OPERATIVO
074900011211     C     V1CFIL        IFGT      *ZERO
075000011211     C     V1CFIL        ANDNE     999
075100130801     C                   z-add     V1CFIL        iMK43fil
075200011211     C                   ENDIF
075300011211     C*
075400130801     C                   CALL      'TRMK43R'
075500011211     C                   PARM                    KPJBA
075600130801     C                   PARM                    TRMK43ds
075700011211     C*
075800130801     c                   if        oMK43err = *off  and
075900130801     c                             oMK43fxx = *blank
076000130801     C                   MOVE      oMK43cmm      V1AGE
076100130801     C                   MOVEL     oMK43des      V1AGED
076200130801     c                   endif
076300011211     C                   ENDIF
076400011211     C*
076500011211     C* CONTROLLO E DECODIFICA CODICE AGENTE *****
076600011211     C                   CLEAR                   V1AGED
076700011211     C                   CLEAR                   V2CMSG
076800011211     C     V1AGE         IFNE      *BLANKS
076900130805      * Codice Agente NON numerico
077000130805     c                   if        %check(c_Digits : V1age) > *zero
077100130805     c                   eval      V2Cmsg = Msg(14)
077200130805     c                   eval      *in50 = *on
077300130805     c                   eval      *in28 = *on
077400130805     c                   leavesr
077500130805     c                   endif
077600130801     C                   MOVE      V1AGE         CMMcod
077700130801     C     CMMcod        CHAIN     AZCMM000
077800011211     C* CODICE AGENTE ERRATO
077900130801     C                   IF        Not %found(AZCMM01L)
078000011211     C                   MOVEL     MSG(14)       V2CMSG
078100011211     C                   SETON                                        5028
078200011211     C                   GOTO      ENDCT2
078300011211     C                   ELSE
078400130801     C                   MOVEL     CMMdes        V1AGED
078500011211     C                   ENDIF
078600011211     C                   ENDIF
078700020306     C**
078800020306     C****  CODICE IMPORTANZA CLIENTE  ***
078900020306     C     V1CCIC        IFEQ      '?'
079000020306     C                   MOVEL     'IC'          COD
079100020306     C                   MOVEL     ' '           V1CCIC
079200020306     C                   MOVEL     *BLANKS       DES              25
079300020314     C                   MOVEL     *BLANKS       KEY
079400020306     C                   CALL      'X§TABER'
079500020306     C                   PARM                    CODUT
079600020306     C                   PARM                    COD
079700020306     C                   PARM                    KEY
079800020306     C                   PARM                    DES
079900020306     C                   MOVEL     KEY           V1CCIC
080000020306     C                   SETON                                        2747
080100020306     C                   GOTO      ENDCT2
080200020306     C                   ENDIF
080300020306     C*
080400020306     C* CONTROLLO VALIDITA'
080500020306     C     V1CCIC        IFNE      ' '
080600020306     C                   MOVEL     'IC'          COD
080700020306     C                   MOVEL(P)  V1CCIC        KEY
080800020306     C     KTAB          CHAIN     TABEL                              30
080900020306     C  N30TBLFLG        IFNE      ' '
081000020306     C                   SETON                                        30
081100020306     C                   ENDIF
081200020306     C* CODICE    ERRATO
081300020306     C     *IN30         IFEQ      *ON
081400020312     C                   MOVEL     MSG(11)       V2CMSG
081500020306     C                   SETON                                        4728
081600020306     C                   GOTO      ENDCT2
081700020306     C                   ENDIF
081800020306     C                   ENDIF
081900011211     C*
082000020429     C****  PERIODO CLIENTI NON OPERATIVO
082100020429     C*
082200020429      * Se richieste selezioni su clienti non operativi il periodo
082300020429      * è obbligatorio, e non deve essere superiore agli ultimi 12 mesi
082400020429      * escludendo quello in corso.                                    o
082500020429   1aC                   IF        V1CCNO='N' or V1CCNO='E'
082600020429      *
082700020429   2aC                   IF        V1CDMM = *zeros or V1CAMM = *zeros
082800020429     C                   MOVEL     MSG(17)       V2CMSG
082900020429     C                   SETON                                        5228
083000020429     C                   GOTO      ENDCT2
083100020429   2-C                   ENDIF
083200020429      *
083300020429      * I mesi devono essere da 0 a 12
083400020429   2aC                   IF        V1CDMM >12 or V1CAMM >12
083500020429     C                   MOVEL     MSG(18)       V2CMSG
083600020429     C                   SETON                                        5228
083700020429     C                   GOTO      ENDCT2
083800020429   2-C                   ENDIF
083900020429      *
084000020429     C                   if        V1CDAA > 60
084100020429     C     1900          add       V1CDAA        WAnnoI
084200020429     C                   else
084300020429     C     2000          add       V1CDAA        WAnnoI
084400020429     C                   endif
084500020429     C                   if        V1CAAA > 60
084600020429     C     1900          add       V1CAAA        WAnnoF
084700020429     C                   else
084800020429     C     2000          add       V1CAAA        WAnnoF
084900020429     C                   endif
085000020429      *
085100020429     C                   z-add     *zeros        DataInizio        6 0
085200020429     C                   eval      DataInizio = (WannoI * 100) + V1CDMM
085300020429     C                   z-add     *zeros        DataFine          6 0
085400020429     C                   eval      DataFine   = (WAnnoF * 100) + V1CAMM
085500020429      *
085600020429      * La data fine non deve essere minore della data inizio
085700020429   2aC                   IF        DataFine < DataInizio
085800020429     C                   MOVEL     MSG(18)       V2CMSG
085900020429     C                   SETON                                        5228
086000020429     C                   GOTO      ENDCT2
086100020429   2-C                   ENDIF
086200020429      *
086300020429      * Intervallo data inizio/fine compreso in quello di default
086400020429   2aC     DataInizio    iflt      DataInizioDFT
086500020429     C     DataFine      orgt      DataFineDFT
086600020429     C                   MOVEL     MSG(18)       V2CMSG
086700020429     C                   SETON                                        5228
086800020429     C                   GOTO      ENDCT2
086900020429   2-C                   ENDIF
087000020429   1eC                   ELSE
087100020429      *
087200020429      * altrimenti pulisco il periodo
087300020429     C                   move      *zeros        V1CDMM
087400020429     C                   move      *zeros        V1CDAA
087500020429     C                   move      *zeros        V1CAMM
087600020429     C                   move      *zeros        V1CAAA
087700020429   1-C                   ENDIF
087800050617     C**
087900050617     C****  tipo pagamento C/Assegni  ***
088000050617     c                   eval      v1dtpc='No selezione'
088100050617    1C     V1CtpC        IFEQ      '?'
088200050617     C                   MOVEL     '4U'          COD
088300160728     C                   MOVEL     ' '           V1Ctpc
088400050617     C                   MOVEL     *BLANKS       DES              25
088500050617     C                   MOVEL     *BLANKS       KEY
088600050617     C                   CALL      'X§TABER'
088700050617     C                   PARM                    CODUT
088800050617     C                   PARM                    COD
088900050617     C                   PARM                    KEY
089000050617     C                   PARM                    DES
089100050617     C                   MOVEL     KEY           V1Ctpc
089200050617     C                   MOVEL     des           V1dtpc
089300050617     C                   SETON                                        2751
089400050617     C                   GOTO      ENDCT2
089500050617    1C                   ENDIF
089600050617     C*
089700050617     C* CONTROLLO VALIDITA'
089800050617    1C     V1Ctpc        IFNE      'N'
089900050617     C                   MOVEL     '4U'          COD
090000050617     C                   MOVEL(P)  V1Ctpc        KEY
090100050617     C     KTAB          CHAIN     TABEL                              30
090200050617     C  N30TBLFLG        IFNE      ' '
090300050617     C                   SETON                                        30
090400050617     C                   ENDIF
090500050617     C* CODICE    ERRATO
090600050617    2C     *IN30         IFEQ      *ON
090700050617     C                   MOVEL     MSG(19)       V2CMSG
090800050617     C                   SETON                                        5128
090900050617     C                   GOTO      ENDCT2
091000050617    2C                   ENDIF
091100050617     c                   movel     tbluni        v1dtpc
091200050617    1C                   ENDIF
091300050617     c
091400001201     C     ENDCT2        ENDSR
091500940822     C*---------------------------------------------------------------*
091600020306**  MSG
091700020711Punto operativo Inesistente                                                    1
091800940822Codice Categoria SEAT Errato                                                   2
091900940822Scelta Incongruente                                                            3
092000110921Tariffa Particolare Errata                                                     4
092100940822Tipo Servizio Errato                                                           5
092200110921Immessa Tar.Particolare. Immettere anche come deve essere espressa in tariffa  6
092300110921Utente non abilitato alla funzione                                             7
092400090707Filiale non in gestione all'utente                                             8
092500960424Tipo Tariffa Errato                                                            9
092600960424Codice Particolarità Errato                                                   10
092700001201Codice importanza cliente errato                                              11
092800001201Tipo nota cliente Errato                                                      12
092900001201Immettere S o E per parzializzare                                             13
093000050617Codice agente non valido                                                      14
093100050617Campo Network errato                                                          15
093200050617Campo Network obbligatorio se Network tariffe impostato a 'S' o 'E'           16
093300050617Periodo per richiesta clienti non operativi obbligatorio                      17
093400050617Periodo errato:deve essere compreso nei 12 mesi precedenti quello in corso    18
093500050617Tipo pagamento C/Assegni errato                                               19
093600051128Codice Luogo Errato                                                           20
