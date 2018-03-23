000100020415     H DECEDIT('0,') DATEDIT(*DMY.) option(*nodebugio)
000200970320     H* TNTA08R *-----------------------------------------------------*
000300950717     F*                STAMPA ANAGRAFICA CLIENTI       - BATCH -      *
000400000000     F*---------------------------------------------------------------*H
000500900831     FCNACO04L  IF   E           K DISK    USROPN
000600900831     F                                     RENAME(CNACO000:CNACOA)
000700900831     FCNACO00F  IF   E           K DISK    USROPN
000800900831     F                                     RENAME(CNACO000:CNACOC)
000900000000     FCNIND00F  IF   E           K DISK
001000020405     FSISDC01L  IF   E           K DISK
001100900713     FCNCLP00F  IF   E           K DISK
001200941122     FFNCLS01L  IF   E           K DISK
001300940822     FTNTAM01L  IF   E           K DISK
001400130802     fAZCMM01L  if   e           k disk
001500900831     FAZORG01L  IF   E           K DISK
001600900831     FTABEL00F  IF   E           K DISK
001700900901     FPRTF198   O    F  198        PRINTER OFLIND(*INOF)
001800990714     FTITPT01L  IF   E           K DISK
001900070129     FTITPD01L  IF   E           K DISK
002000990714     FTITGC01L  IF   E           K DISK
002100001204     FTFNTC01L  IF   E           K DISK
002200051129     FFNSPE03L  IF   E           K DISK
002300080211     fCnabi00f  if   e           k disk
002400040930
002500950717     D* DEFINIZIONE SCHIERE
002600040930     D FIL             S              3  0 DIM(250)
002700110926     D WAR             S              1    DIM(13)
002800110926     D DAR             S             13    DIM(5)
002900901009     D KPM             S              5  1 DIM(5)
003000910904     D TIN             S              1    DIM(5)
003100101015     D IS1             S              3  0 DIM(5)
003200040119     D IS2             S              5  2 DIM(5)
003300020912     D AC1             S              8  3 DIM(5)
003400930519     D FV1             S              1    DIM(5)
003500911014     D CAR             S              3    DIM(5)
003600950208     D TCM             S              1    DIM(5)
003700970320     D BAM             S              1    DIM(5)
003800040601     d bva             s              1    dim(5)
003900060310     d tap             s              1    dim(5)
004000060313     d acbase          s              1    dim(5)
004100110921     d fuel            s              1    dim(5)
004200110921     d tarpar          s              1    dim(5)
004300950802     D*
004400020307     D SK              S             66    DIM(8) CTDATA PERRCD(1)
004500920624     D SK1             S             66    DIM(3) CTDATA PERRCD(1)
004600920624     D SK2             S             66    DIM(3) CTDATA PERRCD(1)
004700920624     D SK3             S             66    DIM(3) CTDATA PERRCD(1)
004800920624     D SK4             S             66    DIM(3) CTDATA PERRCD(1)
004900920624     D SK5             S             66    DIM(3) CTDATA PERRCD(1)
005000020307     D SK6             S             66    DIM(3) CTDATA PERRCD(1)
005100040930
005200040930      * P.O. gestibili dall'utente
005300040930     d fig             s              3    dim(250) inz(*Zeros)
005400110922
005500110922      * SK tariffe particolari da tabella 1P
005600110922     d $1P             s              2    dim(999)
005700110922     d $tco            s              1    dim(999)
005800110922     d $fie            s              1    dim(999)
005900110922     d $fdp            s              1    dim(999)
006000110922     d $ffe            s              1    dim(999)
006100110922
006200950717     D*
006300900831     D TCUDS           DS
006400900831     D  F1                     1      1
006500900831     D  F3                     3      3
006600900831     D  F2                     2      2
006700900831     D  F4                     4      4
006800900831     D  F56                    5      6
006900900901     D KEY             DS
007000900901     D  §CDPAG                 1      3
007100900901     D  §CDLIN                 4      4
007200900901     D  §BLANK                 5      8
007300920617     D                 DS
007400920617     D  VIDFL1                 1      1
007500920617     D  VIDFL2                 2      2
007600950717     D  VIDFL3                 3      3
007700920617     D  VIDFL4                 4      4
007800920617     D  VIDFL5                 5      5
007900920617     D  VIDFL6                 6      6
008000920617     D  VIDFL7                 7      7
008100920617     D  VIDFL8                 8      8
008200920617     D  VIDFL9                 9      9
008300920617     D  VIDFL0                10     10
008400920617     D  FLG                    1     10
008500920617     D                                     DIM(10)
008600940903     D*
008700940903     D* PARAMETRI PASSATI DAL FILTRO                  - TNTA07R -
008800000000     D PARAM           DS
008900940903     D* CODICE FILIALE
009000900831     D  VIDFIL                 1      3  0
009100940903     D* SCELTA ORDINAMENTO STAMPA: 'C'=CODICE  'A'=RAGIONE SOCIALE
009200900831     D  VIDSC1                 4      4
009300940903     D* RICHIESTA CLIENTI NON ABILITATI ALLA SPEDIZIONE
009400900831     D  VIDSC2                 5      5
009500940903     D* RICHIESTA VOLUME AUTOMATICO
009600910405     D  VIDSC3                 6      6
009700940903     D* RICHIESTA CODICE IMPORTANZA CLIENTE
009800911011     D  VIDCIC                 7      7
009900060227      * tipo utilizzo peso VDL
010000060227     d  vidtap                 8      8
010100020307     D* NETWORK TARIFFE
010200020307     D  VIDNTR                 9      9
010300940903     D* RICHIESTA TARIFFE CON ASSICURAZIONE PER CONTO
010400920624     D  VIDSAC                10     10
010500040119     D* RICHIESTA ISTAT
010600040119     D  VIDIST                11     11
010700060310      * flag rinuncia ac base
010800060310     d  vidracb               12     12
010900060310      * tariffa part. AC BASE
011000060310     d  vidbase               13     13
011100060310     D* RICHIESTA TARIFFE bloccate
011200051213     D  VIDblk                14     14
011300110921      * tariffa particolare
011400110921     d  vidctp                15     15
011500110921     d  vidstp                16     16
011600940903     D* PARZIALIZZAZIONE PER CATEGORIA SEAT
011700920624     D  VIDSNC                22     22
011800940903     D* CODICE CATEGORIA SEAT
011900920624     D  VIDSCT                23     27  0
012000940903     D* RICHIESTA TIPO SERVIZIO
012100020307     D  VIDTSP                33     33
012200960424     D* RICHIESTA TIPO TARIFFA
012300020307     D  VIDTTR                94     94
012400960424     D* RICHIESTA CODICE PARTICOLARITA'
012500020307     D  VIDCPA                95     96
012600970320     D* RICHIESTA TARIFFE CON CONSEGNA DDT
012700020307     D  VIDBAM                97     97
012800020307     D* RICHIESTA TIPO STAMPA
012900020307     D  VIDTPS                98     98
013000001204     D* Stampa Note Clienti
013100020307     D  VISTPN                99    100
013200020307     D  VINOTE               101    101
013300020307     D  VITNOT               102    103
013400020307     D  VIDNTW               104    106
013500011211     D* Codice agente
013600020307     D  VIDAGE               107    113
013700020404     D  VIDCNO               114    114
013800020426     D* Intervallo mese/anno per richiesta clienti non operativi
013900020426     D  VIDDMM               115    116  0
014000020426     D  VIDDAA               117    118  0
014100020426     D  VIDAMM               119    120  0
014200020426     D  VIDAAA               121    122  0
014300040601     d  vidbva               123    123
014400040930      * dati necessari x il caricamento della sk fig
014500040930     d  prmabi               124    125
014600040930     d  prmcdi               126    126
014700040930     d  prmcar               127    129  0
014800040930     d  prmcpo               130    132  0
014900050617     D* parzializza tipo pagamento C/assegni
015000050617     D  VIdtpc               133    133
015100051128     d* Luogo
015200051128     D  Viluog               134    134
015300051128     D  ViTluo               135    137
015400121227      * Tassazione POST fatturazione
015500121227     d  Vidtpf               138    138
015600940903     D*
015700000725     D DSTA01        E DS
015800931228     D DS1L          E DS
015900020307     D OG143         E DS
016000900901     D KPJBA         E DS
016100040930     d Trul31ds      e ds
016200110922     d DS1P          e ds
016300110922
016400110922     d Azuteds       e ds                  extname(Azute00f)
016500110922     d dDatiute      e ds
016600110922     d Tibs34ds      e ds
016700080117
016800080117     d clpbab          ds
016900080117     d  iban1                  1      4
017000080117     d  iban2                  5      8
017100080117     d  iban3                  9     12
017200080117     d  iban4                 13     16
017300080117     d  iban5                 17     20
017400080117     d  iban6                 21     24
017500080117     d  iban7                 25     28
017600080211     d  iban8                 29     32
017700080117
017800070129      *-------------------- variabili ---------------------------------------
017900050617     d stafpc          s                   like(clpfpc)
018000070129     d contatore       s              2  0
018100070129     d comfue          s              1
018200080211     d wbanca          s             26
018300110927     d desctp          s             25    inz
018400110921     d desstp          s             40    inz
018500110922     d codut           s              1  0 Inz(1)
018600110922     d xx              s              4  0 inz
018700110922     d woktp           s              1
018800110922     d x               s              3  0
018900110926     d vidfuel         s              1    inz('f')
019000110922
019100950717     C*****************************************************************
019200921124     C*  RIEPILOGO INDICATORI
019300950717     C*****************************************************************
019400921124     C* 10    - SCELTA ORDINAMENTO STAMPA PER RAGIONE SOCIALE
019500921124     C* 11    - SCELTA ORDINAMENTO STAMPA PER CODICE CLIENTE
019600921124     C* 30/34 - DI COMODO
019700921124     C* 33    - ESCLUSIONI
019800070129     C* 35/43 - ESCLUSIONI
019900990714     C* 46/48 - CONDIZIONANO L'EMISSIONE DEI TRATTINI SEPARATORI
020000960424     C* 50/51 - DI COMODO
020100960424     C* 53/56 - DI COMODO
020200000725     C* 57    - SERVIZI DPD
020300000725     C* 58    - DI COMODO
020400960424     C* 60    - CLIENTE BLOCCATO
020500960424     C* 61/63 - PARZIALIZZAZIONE CLIENTI NON ABILITATI
020600040601     C* 64/67 - ESCLUSIONI
020700921124     C* 70/72 - DI COMODO
020800950717     C* 84    - DI COMODO
020900010130     C* 85    - DI COMODO
021000921124     C*****************************************************************
021100110922     C*
021200110922      * Carico le tariffe particolari in sk
021300110922     c                   exsr      car1p
021400110922
021500000000     C*
021600940903     C* POSIZIONAMENTO PER LETTURA
021700900831     C     VIDSC1        IFEQ      'C'
021800900831     C                   OPEN      CNACO00F
021900900831     C                   SETON                                        11
022000900831     C                   ELSE
022100900831     C                   OPEN      CNACO04L
022200900831     C                   SETON                                        10
022300900831     C                   END
022400940903     C*
022500940903     C* STAMPA RICHIESTE EFFETTUATE A VIDEO
022600050617     c                   if        vidtpc<>'N'
022700050617     c                   seton                                        15
022800050617     c                   endif
022900110921
023000110927      * decodifico tar.particolare
023100110927     c                   clear                   ds1P
023200110927     c                   eval      cod = '1P'
023300110927     c                   eval      key = vidctp
023400110927     c     ktab1         chain     TABEL00F
023500110927     c                   IF        %found(TABEL00F)
023600110927     c                   eval      ds1P = TBLuni
023700110927     c                   ENDIF
023800110927     c                   eval      desctp = §1Pdes
023900110921      * decodifico il flag di esposizione tar.particolare in tariffa
024000110921     c                   SELECT
024100110921     c                   WHEN      vidstp = 'S'
024200110927     c                   eval      desstp = 'Con tariffa presente'
024300110921     c                   WHEN      vidstp = 'V'
024400110927     c                   eval      desstp = 'Con tariffa presente, -
024500110921     c                             valorizzata'
024600110921     c                   WHEN      vidstp = 'A'
024700110927     c                   eval      desstp = 'Con tariffa presente, -
024800110921     c                             azzerata'
024900110921     c                   WHEN      vidstp = 'E'
025000110927     c                   eval      desstp = 'Senza tariffa'
025100110921     c                   OTHER
025200110921     c                   clear                   desstp
025300110921     c                   ENDSL
025400110921
025500940903     C                   EXCEPT    RICHI
025600900831     C*
025700950802     C                   CLEAR                   FIL
025800040930     c                   move      simfel        wsimfel           3
025900950802     C* S O N O   I N   F I L I A L E
026000950802     C     SIMFEL        IFGT      0
026100950802     C     VIDFIL        IFNE      *ZERO
026200950802     C     VIDFIL        ANDNE     999
026300950802     C                   MOVE      VIDFIL        FIL(1)
026400950802     C                   ELSE
026500040930      * Reperimento dei P.O. gestibili dall'utente
026600040930     c                   clear                   TRUL31DS
026700040930     c                   eval      I31abi = prmabi
026800040930     c                   eval      I31cdi = prmcdi
026900040930     c                   eval      I31car = prmcar
027000040930     c                   eval      I31cpo = prmcpo
027100040930     c                   call      'TRUL31R'
027200040930     c                   parm                    KPJBA
027300040930     c                   parm                    TRUL31DS
027400040930if  2c                   if        O31pog > *zeros
027500040930     c                   movea     O31pog        fig
027600040930e   2c                   endif
027700040930     c                   Eval      x = 0
027800040930     c                   Eval      w = 1
027900040930do  2c                   Dow       fig(w) > *Zeros
028000040930if  3c                   If        vidfil = 999
028100040930if  4c                   If        fig(w) <> wsimfel
028200040930     c                   add       1             x
028300040930     c                   Move      fig(w)        fil(x)
028400040930e   4c                   endif
028500040930   x3c                   Else
028600040930     c                   Move      fig(w)        fil(w)
028700040930e   3c                   Endif
028800040930     c                   add       1             w
028900040930     c                   EndDo
029000950802     C                   ENDIF
029100950802     C*
029200950802     C                   ELSE
029300950802     C* S O N O   I N   S E D E
029400950802     C     VIDFIL        IFNE      *ZERO
029500900831     C                   MOVE      VIDFIL        FIL(1)
029600020415     C                   ELSE
029700020415     c                   move      046           fil(1)
029800950802     C                   ENDIF
029900950802     C                   ENDIF
030000900831     C*
030100910927     C                   MOVE      *ZERO         X
030200910927     C                   MOVE      *ZERO         W                 3 0
030300900901     C                   MOVE      *ZERO         §KSCI
030400900831     C     VIDSC2        COMP      'N'                                636162
030500900831     C     LOOP1         TAG
030600900901     C                   ADD       1             W
030700900901     C     FIL(W)        CABEQ     *ZERO         FINE
030800900901     C                   MOVEL     FIL(W)        §KSCI
030900020415     c                   if        simfel > 0  or
031000020415     c                             (simfel = 0 and vidfil > 0)
031100900831     C   10CHIAC1        SETLL     CNACO04L
031200900831     C   11CHIACO        SETLL     CNACO00F
031300020415     c                   else
031400020415     c   10chiac1        setll     cnaco04l
031500020415     c   11chiac1        setll     cnaco00f
031600020415     c                   endif
031700900831     C*
031800020415     c                   if        simfel > 0  or
031900020415     c                             (simfel = 0 and vidfil > 0)
032000910927     C     §CTRC         IFGT      *ZERO
032100910927     C                   EXCEPT    TOTFIL
032200910927     C                   MOVE      *ZERO         §CTRC             5 0
032300910927     C                   END
032400020415     c                   endif
032500910927     C*
032600900901     C     FIL(W)        CHAIN     AZORG                              54
032700980122     C                   SETON                                        OF
032800000000     C*
032900900831     C     LOOP2         TAG
033000900901     C                   SETOFF                                       58
033100020415     c                   if        simfel > 0  or
033200020415     c                             (simfel = 0 and vidfil > 0)
033300900831     C   10              READ      CNACO04L                               58
033400900831     C   11              READ      CNACO00F                               58
033500020415     c                   else
033600020415     c   10              read      cnaco04l                               58
033700020415     c   11              read      cnaco00f                               58
033800020415     c                   endif
033900110922     C  N58ACOKCC        COMP      DUTkci                             5858
034000900831     C   58              GOTO      LOOP1
034100901023     C     ACOFLG        CABEQ     '*'           LOOP2
034200900831     C                   MOVEL     ACOKSC        §FIL              3 0
034300020415     c                   if        simfel > 0 or
034400020415     c                             (simfel = 0 and vidfil > 0)
034500900901     C   11§FIL          CABNE     FIL(W)        LOOP1
034600900901     C   10§FIL          CABNE     FIL(W)        LOOP2
034700020415     c                   endif
034800020417     c                   if        simfel = 0 and vidfil = 0
034900020417     c                             and acoksc = 0
035000020417     c                   goto      loop2
035100020417     c                   endif
035200900831     C*
035300900831     C* SCARTO A SECONDA DELLA SCELTA SU ABILITAZIONE CLIENTE
035400130315     C     ACOABL        COMP      ' '                                6060
035500900831     C  N60
035600900831     CAN 61
035700900901     COR 60
035800900901     CAN 62              GOTO      LOOP2
035900900831     C*
036000900831     C     CHIIND        CHAIN     CNIND000                           70
036100900831     C     CHIIND        CHAIN     CNCLP000                           71
036200941122     C     ACOKSC        CHAIN     FNCLS000                           72
036300920617     C                   MOVE      *BLANK        FLG
036400920617     C  N72              MOVEA     CLSFLO        FLG
036500911011     C*
036600911011     C* PARZIALIZZAZIONE X COD.IMPORTANZA CLIENTE
036700911011     C     VIDCIC        IFNE      *BLANK
036800911011     C     VIDCIC        CABNE     CLPCLV        LOOP2
036900911011     C                   END
037000050617     C* PARZIALIZZAZIONE X tipo pagamento C/assegni
037100050617     C     VIDtpc        IFNE      'N'
037200050617     C     VIDtpc        CABNE     CLPfpc        LOOP2
037300050617     C                   END
037400011211     C*
037500011211     C* PARZIALIZZAZIONE X COD.AGENTE
037600011211     C     VIDAGE        IFNE      *BLANK
037700011211     C                   MOVE      CLPAGE        AGENTE            7
037800011211     C     VIDAGE        CABNE     AGENTE        LOOP2
037900011211     C                   END
038000920613     C*
038100920613     C* PARZIALIZZAZIONE X COD.CATEGORIA SEAT
038200920613     C     VIDSNC        IFEQ      'S'
038300111006     C**!!!VIDSCT        IFNE      99999
038400920613     C     VIDSCT        CABNE     ACOITC        LOOP2
038500111006     C**!!!              END
038600111006     C**!!!VIDSCT        IFEQ      99999
038700111006     C**!!!ACOITC        CABEQ     0             LOOP2
038800111006     C**!!!              END
038900920613     C                   END
039000121227      * Parzializzazione per Tassazione POST Fatturazione
039100121227     c                   IF        VIDtpf = 'S' and
039200121227     c                             %subst(CLSflo:3:1) <> *blanks
039300121227     c                   goto      loop2
039400121227     c                   ENDIF
039500121227     c                   IF        VIDtpf = 'E' and
039600121227     c                             %subst(CLSflo:3:1) <> 'N'
039700121227     c                   goto      loop2
039800121227     c                   ENDIF
039900001204      *
040000001204      * PARZIALIZZAZIONE X NOTE CLIENTI
040100001204     C     VINOTE        IFNE      'N'
040200001204     C                   MOVEL     ACOKCC        KNK1
040300001204     C                   MOVE      ACOKSC        KNK1
040400001204      * PARZIALIZZA PER TIPO NOTA
040500001204     C     VITNOT        IFNE      *BLANKS
040600001204     C                   MOVE      VITNOT        KTNT
040700010130     C                   SETON                                        84
040800010130     C     KTFN          SETLL     TFNTC01L
040900010130     C                   DO        *HIVAL
041000010130     C     KTFN          READE     TFNTC01L                               85
041100010130     C   85              LEAVE
041200010130     C     NTCFLT        IFNE      'A'
041300010130     C                   SETOFF                                       84
041400010131     C                   LEAVE
041500010130     C                   ENDIF
041600010130     C                   ENDDO
041700001204     C     *IN84         IFEQ      *OFF
041800001204     C     VINOTE        ANDNE     'S'
041900001204     C                   GOTO      LOOP2
042000001204     C                   ENDIF
042100001204     C     *IN84         IFEQ      *ON
042200001204     C     VINOTE        ANDNE     'E'
042300001204     C                   GOTO      LOOP2
042400001204     C                   ENDIF
042500001204     C                   ENDIF
042600001204      * PARZIALIZZA PER TUTTI I TIPI NOTA
042700001204     C     VITNOT        IFEQ      *BLANKS
042800010130     C                   SETON                                        84
042900010130     C     KTFN1         SETLL     TFNTC01L
043000010130     C                   DO        *HIVAL
043100010130     C     KTFN1         READE     TFNTC01L                               85
043200010130     C   85              LEAVE
043300010130     C     NTCFLT        IFNE      'A'
043400010130     C                   SETOFF                                       84
043500010131     C                   LEAVE
043600010130     C                   ENDIF
043700010130     C                   ENDDO
043800001204     C     *IN84         IFEQ      *OFF
043900001204     C     VINOTE        ANDNE     'S'
044000001204     C                   GOTO      LOOP2
044100001204     C                   ENDIF
044200001204     C     *IN84         IFEQ      *ON
044300001204     C     VINOTE        ANDNE     'E'
044400001204     C                   GOTO      LOOP2
044500001204     C                   ENDIF
044600001204     C                   ENDIF
044700001204     C                   ENDIF
044800051128      *
044900051128      * PARZIALIZZAZIONE X LUOGO
045000051128     C     VIluog        IFNE      'N'
045100051128     C                   MOVEL     'L'           Kfls
045200051128     C                   z-add     ACOKSC        Kcli
045300051128      * PARZIALIZZA PER cod luogo
045400051128     C     VITluo        IFNE      *BLANKS
045500051128     C                   MOVE      VITluo        Kcod
045600051128     C                   SETON                                        84
045700051129     C     Kspe          SETLL     fnspe03L
045800051128     C                   DO        *HIVAL
045900051129     C     Kspe          READE     fnspe03L                               85
046000051128     C   85              LEAVE
046100051128     C                   SETOFF                                       84
046200051128     C                   LEAVE
046300051128     C                   ENDDO
046400051128     C     *IN84         IFEQ      *OFF
046500051128     C     VIluog        ANDNE     'S'
046600051128     C                   GOTO      LOOP2
046700051128     C                   ENDIF
046800051128     C     *IN84         IFEQ      *ON
046900051128     C     VIluog        ANDNE     'E'
047000051128     C                   GOTO      LOOP2
047100051128     C                   ENDIF
047200051128     C                   ENDIF
047300051128      * PARZIALIZZA PER TUTTI I codici luogo
047400051128     C     VITluo        IFEQ      *BLANKS
047500051128     C                   SETON                                        84
047600051129     C     kspe1         SETLL     fnspe03L
047700051128     C                   DO        *HIVAL
047800051129     C     kspe1         READE     fnspe03l                               85
047900051128     C   85              LEAVE
048000051128     C                   SETOFF                                       84
048100051128     C                   LEAVE
048200051128     C                   ENDDO
048300051128     C     *IN84         IFEQ      *OFF
048400051128     C     VIluog        ANDNE     'S'
048500051128     C                   GOTO      LOOP2
048600051128     C                   ENDIF
048700051128     C     *IN84         IFEQ      *ON
048800051128     C     VIluog        ANDNE     'E'
048900051128     C                   GOTO      LOOP2
049000051128     C                   ENDIF
049100051128     C                   ENDIF
049200051128     C                   ENDIF
049300020307      *
049400020411      *  PARZIALIZZAZIONE PER NETWORK TARIFFE
049500020410  1a C                   IF        VidNtr='S' or VidNtr ='E'
049600020411      *  recupero il network
049700020410     C                   MOVEL     AcoKsc        §Fil
049800020410     C     §Fil          CHAIN     AzOrg01L                           30
049900020411     C                   MOVEL     OrgDe3        OG143
050000020411      *  se su file tariffe non trovo nulla non stampo
050100020410     C                   MOVE      'S'           WNOSEL            1
050200020410     C                   MOVE      ' '           WSISEL            1
050300020315      *
050400020410     C                   MOVE      *blanks       §CTR              3
050500020410     C     AcoKsc        SETGT     TnTam01l
050600020410   2aC                   DO        *hival
050700020410     C     AcoKsc        READPE    Tntam01l                               57
050800020411     C                   IF        *in57 =*on
050900020411     C                   leave
051000020411     C                   ENDIF
051100020410      *
051200020410     C                   MOVE      TAMCTR        ALF3              3
051300020411   3aC                   IF        TamAtb <>'A' and alf3<> §ctr
051400020411     C                   MOVE      TAMCTR        §CTR
051500020410      *  Ho letto almeno una tariffa ..
051600020410     C                   MOVEL     TAMFLO        DSTA01
051700020307      *  SE IL FILTRO SU NETWORK TARIFFE E IMPOSTATO AD 'E'
051800020307      *  VOGLIO SOLO CLIENTI SENZA SERVIZIO IMPOSTATO UGUALE AL CAMPO
051900020307      *  VIDNTW  (DPD, FED, PPT, COR, MES, EEX, EUP, ITA, EST)
052000020307      *
052100020411      *  parto dall'idea di stampare tutto
052200020410     C                   MOVE      ' '           WNOSEL
052300020410   4aC     VIDNTR        IFEQ      'E'
052400000725      *
052500020307      * DPD
052600020307     C     VIDNTW        IFEQ      'DPD'
052700000725     C     §TADPD        IFEQ      'S'
052800020314     C                   MOVE      'S'           WNOSEL
052900000725     C                   LEAVE
053000000725     C                   ENDIF
053100020307     C                   ENDIF
053200000725      *
053300020307      * FED
053400020307     C     VIDNTW        IFEQ      'FED'
053500020307     C     §TAFED        IFEQ      'S'
053600020314     C                   MOVE      'S'           WNOSEL
053700020307     C                   LEAVE
053800020307     C                   ENDIF
053900020307     C                   ENDIF
054000020307      *
054100020307      * PPT
054200020307     C     VIDNTW        IFEQ      'PPT'
054300020307     C     TAMTSP        IFEQ      'P'
054400020314     C                   MOVE      'S'           WNOSEL
054500020307     C                   LEAVE
054600020307     C                   ENDIF
054700020307     C                   ENDIF
054800020307      *
054900020307      * COR
055000020307     C     VIDNTW        IFEQ      'COR'
055100020307     C     TAMFIE        IFEQ      'I'
055200020307     C     §OGNTW        ANDEQ     'COR'
055300020410     C     §TADPD        ANDNE     'S'
055400020314     C                   MOVE      'S'           WNOSEL
055500020307     C                   LEAVE
055600020307     C                   ENDIF
055700020307     C                   ENDIF
055800020307      *
055900020307      * MES
056000020307     C     VIDNTW        IFEQ      'MES'
056100020307     C     TAMFIE        IFEQ      'I'
056200020307     C     §OGNTW        ANDEQ     'MES'
056300020410     C     §TADPD        ANDNE     'S'
056400020314     C                   MOVE      'S'           WNOSEL
056500020307     C                   LEAVE
056600020307     C                   ENDIF
056700020307     C                   ENDIF
056800020307      *
056900020307      * EEX
057000020307     C     VIDNTW        IFEQ      'EEX'
057100020307     C     TAMFIE        IFEQ      'E'
057200020410     C     §TAFED        ANDNE     'S'
057300020411      * Includiamo i clienti Italia che spediscono
057400020411      * all'estero con EuroExpress
057500020411     C     §OGNTW        IFEQ      'EEX'
057600020411     C     §OGNTW        OREQ      'COR'
057700020411     C     §OGNTW        OREQ      'MES'
057800020314     C                   MOVE      'S'           WNOSEL
057900020307     C                   LEAVE
058000020307     C                   ENDIF
058100020307     C                   ENDIF
058200020411     C                   ENDIF
058300020307      *
058400020307      * EUP
058500020307     C     VIDNTW        IFEQ      'EUP'
058600020307     C     TAMFIE        IFEQ      'E'
058700020307     C     §OGNTW        ANDEQ     'EUP'
058800020410     C     §TAFED        ANDNE     'S'
058900020314     C                   MOVE      'S'           WNOSEL
059000020307     C                   LEAVE
059100020307     C                   ENDIF
059200020307     C                   ENDIF
059300020307      * ITA
059400020307     C     VIDNTW        IFEQ      'ITA'
059500020307     C     TAMFIE        IFEQ      'I'
059600020307     C     §TADPD        ANDNE     'S'
059700020314     C                   MOVE      'S'           WNOSEL
059800020307     C                   LEAVE
059900020307     C                   ENDIF
060000020307     C                   ENDIF
060100020307      * EST
060200020307     C     VIDNTW        IFEQ      'EST'
060300020411     C     TAMFIE        IFNE      'I'
060400020307     C     §TADPD        OREQ      'S'
060500020314     C                   MOVE      'S'           WNOSEL
060600020307     C                   LEAVE
060700020307     C                   ENDIF
060800020307     C                   ENDIF
060900020410  4- C                   ENDIF
061000020307      *
061100020307      *  SE IL FILTRO SU NETWORK TARIFFE E IMPOSTATO AD 'S'
061200020307      *  VOGLIO SOLO CLIENTI CON SERVIZIO IMPOSTATO UGUALE AL CAMPO
061300020307      *  VIDNTW  (DPD, FED, PPT, COR, MES, EEX, EUP, ITA, EST)
061400020307      *
061500020411      *  parto dall'idea di non stampere niente
061600020410     C                   MOVE      ' '           WSISEL
061700020410   4aC     VIDNTR        IFEQ      'S'
061800020307      *
061900020307      * DPD
062000020307     C     VIDNTW        IFEQ      'DPD'
062100020307     C     §TADPD        IFEQ      'S'
062200020314     C                   MOVE      'S'           WSISEL
062300020307     C                   LEAVE
062400020307     C                   ENDIF
062500020307     C                   ENDIF
062600020307      *
062700020307      * FED
062800020307     C     VIDNTW        IFEQ      'FED'
062900020307     C     §TAFED        IFEQ      'S'
063000020314     C                   MOVE      'S'           WSISEL
063100020307     C                   LEAVE
063200020307     C                   ENDIF
063300020307     C                   ENDIF
063400020307      *
063500020307      * PPT
063600020307     C     VIDNTW        IFEQ      'PPT'
063700020307     C     TAMTSP        IFEQ      'P'
063800020314     C                   MOVE      'S'           WSISEL
063900020307     C                   LEAVE
064000020307     C                   ENDIF
064100020307     C                   ENDIF
064200020307      *
064300020307      * COR
064400020307     C     VIDNTW        IFEQ      'COR'
064500020307     C     TAMFIE        IFEQ      'I'
064600020307     C     §OGNTW        ANDEQ     'COR'
064700020410     C     §TADPD        ANDNE     'S'
064800020314     C                   MOVE      'S'           WSISEL
064900020307     C                   LEAVE
065000020307     C                   ENDIF
065100020307     C                   ENDIF
065200020307      *
065300020307      * MES
065400020307     C     VIDNTW        IFEQ      'MES'
065500020307     C     TAMFIE        IFEQ      'I'
065600020307     C     §OGNTW        ANDEQ     'MES'
065700020410     C     §TADPD        ANDNE     'S'
065800020314     C                   MOVE      'S'           WSISEL
065900020307     C                   LEAVE
066000020307     C                   ENDIF
066100020307     C                   ENDIF
066200020307      *
066300020307      * EEX
066400020307     C     VIDNTW        IFEQ      'EEX'
066500020307     C     TAMFIE        IFEQ      'E'
066600020411     C     §TAFED        ANDNE     'S'
066700020411      * Includiamo i clienti Italia che spediscono
066800020411      * all'estero con EuroExpress
066900020411     C     §OGNTW        IFEQ      'EEX'
067000020411     C     §OGNTW        OREQ      'COR'
067100020411     C     §OGNTW        OREQ      'MES'
067200020314     C                   MOVE      'S'           WSISEL
067300020307     C                   LEAVE
067400020307     C                   ENDIF
067500020307     C                   ENDIF
067600020411     C                   ENDIF
067700020307      *
067800020307      * EUP
067900020307     C     VIDNTW        IFEQ      'EUP'
068000020307     C     TAMFIE        IFEQ      'E'
068100020307     C     §OGNTW        ANDEQ     'EUP'
068200020410     C     §TAFED        ANDNE     'S'
068300020314     C                   MOVE      'S'           WSISEL
068400020307     C                   LEAVE
068500020307     C                   ENDIF
068600020307     C                   ENDIF
068700020307      * ITA
068800020307     C     VIDNTW        IFEQ      'ITA'
068900020307     C     TAMFIE        IFEQ      'I'
069000020307     C     §TADPD        ANDNE     'S'
069100020314     C                   MOVE      'S'           WSISEL
069200020307     C                   LEAVE
069300020307     C                   ENDIF
069400020307     C                   ENDIF
069500020307      * EST
069600020307     C     VIDNTW        IFEQ      'EST'
069700020411     C     TAMFIE        IFNE      'I'
069800020307     C     §TADPD        OREQ      'S'
069900020314     C                   MOVE      'S'           WSISEL
070000020307     C                   LEAVE
070100020307     C                   ENDIF
070200020307     C                   ENDIF
070300020410  4- C                   ENDIF
070400020410      *
070500020410  3- C                   ENDIF
070600020410  2- C                   ENDDO
070700020307      *
070800020315     C     VIDNTR        IFEQ      'S'
070900020315     C     WSISEL        ANDNE     'S'
071000020307     C                   GOTO      LOOP2
071100020307     C                   ENDIF
071200020315      *
071300020315     C     VIDNTR        IFEQ      'E'
071400020315     C     WNOSEL        ANDNE     ' '
071500020315     C                   GOTO      LOOP2
071600020315     C                   ENDIF
071700020410   1-C                   ENDIF
071800020410      *
071900020410      *  SELEZIONI SU CLIENTE OPERATIVO 'N'; 'E'
072000020410   1aC                   IF        VidCno = 'N' or Vidcno ='E'
072100020410      *
072200020410      *  Parto dal presupposto che il cliente sia operativo ...
072300020405     C                   move      'S'           $CliOpe           1
072400020430      *  Imposto periodo di richiesta clienti non operativi
072500020430     C                   if        VIDDAA > 60
072600020430     C     1900          add       VIDDAA        WAnnoI            4 0
072700020430     C                   else
072800020430     C     2000          add       VIDDAA        WAnnoI
072900020430     C                   endif
073000020430     C                   if        VIDAAA > 60
073100020430     C     1900          add       VIDAAA        WAnnoF            4 0
073200020430     C                   else
073300020430     C     2000          add       VIDAAA        WAnnoF
073400020430     C                   endif
073500020430     C                   z-add     VIDDMM        WMeseI            2 0
073600020430      *
073700020430     C                   z-add     *zeros        DataFine          6 0
073800020430     C                   eval      DataFine   = (WAnnoF * 100) + VIDAMM
073900020405      *
074000020410      *  il cliente non è operativo se non ha il blocco e se non
074100020429      *  ha fatto spedizioni nel periodo richiesto.
074200130315   2aC     AcoAbl        ifeq      *blanks
074300020405      *
074400020405      *  Ciclo sulle spedizioni
074500020408     C                   move      'N'           $SiSpe            1
074600020405      *
074700020405     C     Ksdc          setll     SisdC01l
074800020405     C     AcoKsc        reade     SisdC01l                               25
074900020405      * leggo fino a fine file, o fino a cambio cliente , o fino
075000020405      * a 12 mesi dopo
075100020410   3aC     *in25         DOWEQ     *off
075200020429      *
075300020405      * Controllo la data
075400020405     C                   z-add     *zeros        DataLetta         6 0
075500020405     C                   eval      DataLetta  = (SdcAnn * 100) + SdcMes
075600020429   4aC     DataLetta     ifle      DataFine
075700020405      *
075800020410   5aC     SdcRbl        ifeq      'N'
075900020405      *
076000020430      *  Il cliente ha avuto almeno una spedizione valida
076100020430      *  nel periodo richiesto
076200020405     C                   move      'S'           $SiSpe
076300020405     C                   LEAVE
076400020410   5-C                   endif
076500020405      *
076600020405     C     Acoksc        reade     SisdC01l                               25
076700020410   4eC                   ELSE
076800020405     C                   leave
076900020410   4-C                   ENDIF
077000020410   3-C                   ENDDO
077100020405      *
077200020410   3 C     $SiSpe        ifeq      'N'
077300020405     C                   move      'N'           $CliOpe
077400020410   3-C                   endif
077500020410   2-C                   endif
077600020405      *
077700020405      *
077800020410   2 C     VidCno        ifeq      'E'
077900020405     C     $CliOpe       andeq     'S'
078000020405     C                   GOTO      LOOP2
078100020410   2-C                   endif
078200020405      *
078300020410   2 C     VidCno        ifeq      'N'
078400020405     C     $CliOpe       andeq     'N'
078500020405     C                   GOTO      LOOP2
078600020410   2-C                   endif
078700020410   1-C                   ENDIF
078800020315      *
078900900831     C*  CARICA LE PRIME 5 TARIFFE NON ANNULLATE
079000900831     C                   EXSR      CARTAM
079100900831     C*
079200920613     C* SE HO CHIESTO PARZIALIZZAZIONI SULLE TARIFFE
079300920613     C* E NON HO CARICATO TARIFFE NON STAMPO
079400971125     C     VIDSC3        IFNE      'N'
079500040119     C     VIDIST        ORNE      'N'
079600971125     C     VIDSAC        ORNE      'N'
079700060310     c     vidracb       orne      'N'
079800060310     c     vidbase       orne      'N'
079900110922     c     VIDctp        orne      *blanks
080000051213     C     VIDblk        ORNE      'N'
080100110921     C     VIDctp        ORNE      ' '
080200950103     C     VIDTSP        ORNE      ' '
080300960424     C     VIDTTR        ORNE      ' '
080400960424     C     VIDCPA        ORNE      '  '
080500970320     C     VIDBAM        ORNE      ' '
080600040601     c     vidbva        orne      *Blanks
080700060228     c     vidtap        orne      *blanks
080800911001     C     X             CABEQ     *ZERO         LOOP2
080900040119     C                   ENDIF
081000110926      * se richiesta la tariffa particolare fuel pulisco la sk tarpar
081100110926      * tanto la stampa del fuel è a parte.
081200110926     c                   If        VIDctp = VIDfuel
081300110926     c                   clear                   tarpar
081400110926     c                   endif
081500110926
081600911001     C*
081700920613     C* DECODIFICA COD PAGAM.
081800920613     C                   MOVEL     *BLANKS       §DEPAG
081900920613     C  N70              DO
082000920613     C                   MOVE      'FA'          COD
082100920613     C                   MOVEL     *BLANKS       KEY
082200920613     C                   MOVE      INDCDP        §CDPAG
082300000322     C                   MOVE      '1'           §CDLIN
082400920613     C     KTAB1         CHAIN     TABEL                              30
082500921125     C  N30              MOVEL     TBLUNI        §DEPAG           26
082600920613     C                   END
082700920613     C* DECODIFICA CATEG.SEAT
082800921125     C                   SETOFF                                       464748
082900921125     C                   MOVEL     *BLANKS       DESSCT           26
083000920613     C     ACOITC        IFGT      *ZERO
083100921125     C                   SETON                                        47
083200920613     C                   MOVE      '1L'          COD
083300920613     C                   MOVEL     *BLANKS       KEY2
083400920613     C                   Z-ADD     ACOITC        §COITC            5 0
083500920613     C                   MOVEL     §COITC        KEY2
083600920613     C     KTAB          CHAIN     TABEL                              84
083700920613     C  N84              MOVEL     TBLUNI        DESSCT
083800920613     C                   END
083900920613     C*
084000980122     C     *INOF         IFEQ      *ON
084100020307     C     VIDTPS        ANDEQ     'A'
084200980122     C                   EXCEPT    TESTA
084300980122     C                   SETOFF                                       OF
084400980122     C                   ENDIF
084500020307     C     *INOF         IFEQ      *ON
084600020307     C     VIDTPS        ANDEQ     'S'
084700020307     C                   EXCEPT    TESTA1
084800020307     C                   SETOFF                                       OF
084900020307     C                   ENDIF
085000980122     C**
085100900901     C*
085200080211     c                   clear                   wbanca
085300080211     c                   if        not *in70
085400080211     c                   if        indabi <> 99999 or indcab <> 99999
085500080211     c                   exsr      sr_desbanca
085600080211     c                   endif
085700080211     c                   endif
085800921125     C*
085900921125     C* SE CI SONO ENTRAMBI I CODICI ABI E CAB LI SEPARO CON
086000921125     C*    UN TRATTINO
086100921125     C  N70INDABI        IFGT      0
086200921125     C     INDCAB        ANDGT     0
086300921125     C                   SETON                                        46
086400921125     C                   END
086500921125     C*
086600921125     C  N71CLPABI        IFGT      0
086700921125     C     CLPCAB        ANDGT     0
086800921125     C                   SETON                                        48
086900921125     C                   END
087000050617     c
087100050617     c* Decodifica tipo pagamento C/A
087200050617     C                   MOVEL     *BLANKS       desfpc           16
087300050617     C                   if        not *in71
087400050617     C                   MOVE      '4U'          COD
087500050617     C                   MOVEL     *BLANKS       KEY
087600050617     C                   MOVEL     clpfpc        key
087700050617     C     KTAB1         CHAIN     TABEL                              30
087800050617     c  n30              eval      desfpc='-' + %subst(tbluni:1:15)
087900050617     c
088000050617     c                   if        clpfpc=' '
088100050617     c                   eval      stafpc='_'
088200050617     c                   else
088300050617     c                   eval      stafpc=clpfpc
088400050617     c                   endif
088500050617     c
088600050617     C                   END
088700130321
088800130321      * se blocco cliente "7" devo stampare "8"
088900130321     c                   IF        ACOabl = '7'
089000130321     c                   eval      ACOABL = '8'
089100130321     c                   ENDIF
089200921125     C*
089300910927     C                   ADD       1             §CTRC             5 0
089400910927     C                   ADD       1             §CTRT             5 0
089500020307     C     VIDTPS        IFEQ      'A'
089600931228     C                   EXCEPT    DETT
089700020307     C                   ENDIF
089800020307     C     VIDTPS        IFEQ      'S'
089900020312      * Recupero descrizione cd. commerciale
090000020312     C                   clear                   ClpDesP
090100020312     C     CLPAGE        IFGT      *ZERO
090200020312     C     *in71         andeq     *off
090300130802     C     CLPage        CHAIN     AZCMM000
090400130802     C                   if        %found(AZCMM01L)
090500130802     C                   movel     CMMdes        ClpDesP          15
090600130802     c                   endif
090700020312     C                   END
090800020307     C                   EXCEPT    DETT1
090900020307     C                   ENDIF
091000001204      * STAMPA NOTE
091100001204     C     VISTPN        IFEQ      'SI'
091200001204     C                   EXSR      SRNOTE
091300001204     C                   ENDIF
091400900831     C*
091500900831     C                   GOTO      LOOP2
091600000000     C     FINE          TAG
091700950717     C*
091800950717     C* STAMPO TOTALE CLIENTI DELL'ULTIMA FILIALE LETTA
091900020415     c                   if        simfel > 0  or
092000020415     c                             (simfel = 0 and vidfil > 0)
092100950717     C     §CTRC         IFGT      *ZERO
092200950717     C                   EXCEPT    TOTFIL
092300950717     C                   END
092400020415     c                   endif
092500950717     C*
092600950717     C* STAMPO TOTALE CLIENTI FINALE
092700910927     C     §CTRT         IFGT      *ZERO
092800910927     C                   EXCEPT    TOTFIN
092900910927     C                   END
093000950717     C*
093100000000     C                   SETON                                        LR
093200080211      *-------------------------------------------------------------------------
093300080211      *  Creo la descrizione della banca di appoggio in base ai codici ABI/CAB
093400080211      *-------------------------------------------------------------------------
093500080211     c     sr_desbanca   begsr
093600080211
093700080211     c     kcnabi        chain     cnabi00f
093800080211     c                   if        not %found(cnabi00f) or abiann = '*'
093900080211     c                   leavesr
094000080211     c                   endif
094100080211
094200080211     c                   eval      wbanca = abiist
094300080211
094400080211     c                   endsr
094500900831     C/SPACE
094600950717     C*--- INZSR INIZIALE --------------------------------------------*
094700950717     C     *INZSR        BEGSR
094800110922
094900110922     C     *ENTRY        PLIST
095000110922     C                   PARM                    KPJBA
095100110922
095200110922     C                   MOVEL     KPJBU         PARAM
095300110922
095400110922     c     *dtaara       define    §azute        azuteds
095500110922     c     *dtaara       define    §datiute      ddatiute
095600110922     c                   in(E)     *dtaara
095700110922     c                   If        %error  or RSUT = *blanks
095800110922     c                   Clear                   Tibs34ds
095900110922     c                   Call      'TIBS34R'
096000110922     c                   Parm                    Tibs34ds
096100110922     c                   In        *dtaara
096200110922     c                   EndIf
096300950717     C***
096400950717     C* GIRO DATA DEL GIORNO: LA PRENDO DA TIME
096500950717     C***
096600950717     C                   TIME                    W0140            14 0
096700950717     C                   MOVE      W0140         UDATE8            8 0
096800020429     C                   MOVE      UDate8        UDate4            4 0
096900020429     C                   MOVE      UDate8        UDate6            6 0
097000020405     C                   MOVEL     UDate6        UDate2            2 0
097100020405      *
097200950717     C                   TIME                    HHMMSS            6 0
097300950717     C***
097400950717     C* DEFINIZIONE CAMPI
097500950717     C***
097600950717     C     *LIKE         DEFINE    ACOKSC        §KSCI
097700950717     C     *LIKE         DEFINE    TBLCOD        COD
097800950717     C     *LIKE         DEFINE    TBLKEY        KEY2
097900001204      *****
098000001204     C     *LIKE         DEFINE    NTCAPL        KAPL
098100001204     C     *LIKE         DEFINE    NTCNK1        KNK1
098200001204     C     *LIKE         DEFINE    NTCNK2        KNK2
098300001204     C     *LIKE         DEFINE    NTCTNT        KTNT
098400051128     C     *LIKE         DEFINE    spefls        kfls
098500051128     C     *LIKE         DEFINE    specli        kcli
098600051128     C     *LIKE         DEFINE    specod        kcod
098700950717     C***
098800950717     C* KLIST
098900950717     C***
099000950717     C* ACCESSO TABEL00F
099100950717     C     KTAB          KLIST
099200950717     C                   KFLD                    CODUT
099300950717     C                   KFLD                    COD
099400950717     C                   KFLD                    KEY2
099500950717     C     KTAB1         KLIST
099600950717     C                   KFLD                    CODUT
099700950717     C                   KFLD                    COD
099800950717     C                   KFLD                    KEY
099900110922     c     ktab2         KLIST
100000110922     c                   KFLD                    CODUT
100100110922     c                   KFLD                    COD
100200950717     C* ACCESSO CNACO04L
100300950717     C     CHIAC1        KLIST
100400950717     C                   KFLD                    CODUT
100500110922     C                   KFLD                    DUTKCI
100600950717     C* ACCESSO CNACO00F
100700950717     C     CHIACO        KLIST
100800950717     C                   KFLD                    CODUT
100900110922     C                   KFLD                    DUTKCI
101000950717     C                   KFLD                    §KSCI
101100950717     C* ACCESSO CNIND00F/CNCLP00F
101200950717     C     CHIIND        KLIST
101300950717     C                   KFLD                    CODUT
101400950717     C                   KFLD                    ACOKCC
101500950717     C                   KFLD                    ACOKSC
101600990714     C* ACCESSO TITPT01L
101700950717     C     KTPT          KLIST
101800950717     C                   KFLD                    TAMKSC
101900950717     C                   KFLD                    TAMCTR
102000950717     C                   KFLD                    TAMPRG
102100990714     C                   KFLD                    FTC               2
102200990714     C* ACCESSO TITGC01L
102300950717     C     KTGC          KLIST
102400950717     C                   KFLD                    TAMKSC
102500950717     C                   KFLD                    TAMCTR
102600950717     C                   KFLD                    TAMPRG
102700001204     C* ACCESSO TFNTC01L
102800001204     C     KTFN          KLIST
102900001204     C                   KFLD                    KAPL
103000001204     C                   KFLD                    KNK1
103100001204     C                   KFLD                    KNK2
103200001204     C                   KFLD                    KTNT
103300001204     C     KTFN1         KLIST
103400001204     C                   KFLD                    KAPL
103500001204     C                   KFLD                    KNK1
103600001204     C                   KFLD                    KNK2
103700001204     C                   MOVEL     'C'           KAPL
103800001204     C                   MOVEL     *BLANKS       KNK2
103900051129     C* ACCESSO FNSPE03L
104000051128     C     Kspe          KLIST
104100051128     C                   KFLD                    Kfls
104200051128     C                   KFLD                    Kcli
104300051128     C                   KFLD                    Kcod
104400051128     C     Kspe1         KLIST
104500051128     C                   KFLD                    Kfls
104600051128     C                   KFLD                    Kcli
104700020405      *
104800020405     C     KSDC          KLIST
104900020405     C                   KFLD                    AcoKsc
105000020405     C                   KFLD                    WAnnoI
105100020405     C                   KFLD                    WMeseI
105200080211
105300080211     c     kCnabi        Klist
105400080211     c                   Kfld                    indabi
105500080211     c                   Kfld                    indcab
105600001204      *
105700950717     C                   ENDSR
105800110922      *---------------------------------------------------------------*
105900110922      * Carico in sk la tabella delle tariffe particolari             *
106000110922      *---------------------------------------------------------------*
106100110922     c     Car1P         BEGSR
106200110922
106300110922     c                   eval      cod = '1P'
106400110922     c                   clear                   xx
106500110922     c     ktab2         SETLL     TABEL00F
106600110922     c     ktab2         READE     TABEL00F
106700110922     c                   DOW       not %eof(TABEL00F)
106800110922
106900110922     c                   IF        TBLflg = *blanks
107000110922     c                   eval      ds1P = TBLuni
107100110922      * non carico le tariffe assicurazione x conto
107200110922     c                   IF        §1Pasc <> 'S'
107300110922     c                   add       1             xx
107400110922     c                   eval      $1P(xx) = tblkey
107500110922     c                   eval      $tco(xx) = §1Ptco
107600110922     c                   eval      $fie(xx) = §1Pfie
107700110922     c                   eval      $fdp(xx) = §1Pfdp
107800110922     c                   eval      $ffe(xx) = §1Pffe
107900110922     c                   ENDIF
108000110922     c                   ENDIF
108100110922     c
108200110922     c     ktab2         READE     TABEL00F
108300110922     c                   ENDDO
108400110922
108500110922     c                   ENDSR
108600110922
108700950802     C*
108800950717     C*--- CARICAMENTO SCHIERA TARIFFE -------------------------------*
108900900831     C     CARTAM        BEGSR
109000910506     C                   SETOFF                                       303433
109100910904     C                   SETOFF                                       505153
109200000725     C                   SETOFF                                       555657
109300900831     C                   MOVE      *ZERO         X
109400911014     C                   MOVE      *ZERO         Y                 3 0
109500901008     C                   MOVE      *ZERO         KPM
109600910904     C                   MOVE      *BLANK        TIN
109700910904     C                   MOVE      *BLANK        DAR
109800900901     C                   MOVE      *BLANK        CAR
109900101015     C                   MOVE      *zeros        is1
110000040119     C                   MOVE      *zeros        is2
110100920624     C                   MOVE      *ZERO         AC1
110200930519     C                   MOVE      *BLANK        FV1
110300950208     C                   MOVE      *BLANK        TCM
110400970320     C                   MOVE      *BLANK        BAM
110500040601     c                   Move      *Blanks       bva
110600930415     C                   MOVE      *BLANK        §CTR              3
110700060310     c                   move      *blanks       tap
110800060313     c                   move      *blanks       acbase
110900070129     c                   move      *blanks       fuel
111000110921     c                   move      *blanks       tarpar
111100920624     C*
111200950717     C     ACOKSC        SETGT     TNTAM000
111300040527    1C                   DO        *HIVAL
111400040527     C     acoksc        READPe    TNTAM000                               30
111500040527    2C                   IF        not *in30
111600900901     C*
111700930415     C                   MOVEL     TAMCTR        ALF3              3
111800040527    3C     TAMATB        IFNE      'A'
111900930415     C     ALF3          ANDNE     §CTR
112000900831     C                   MOVE      TAMCTR        §CTR
112100060310     C** PER 33+   36+37+38+   40+41+42+64+65+66+   CARICO TARIFFA
112200000725     C* PARAMETRO SERVIZIO DPD
112300040527     C                   setoff                                       57
112400020315     C                   MOVEL     tamflo        DSTA01
112500040527     C* SE C'E' UNA PARZIAlizzazione sulle tariffe, riparzializzo anche
112600040527     c*  il ntw, se inclusione
112700040527    4c                   if        vidntr='S'
112800040527    5C     VIDSC3        IFNE      'N'
112900040527     C     VIDIST        ORNE      'N'
113000040527     C     VIDSAC        ORNE      'N'
113100060310     c     vidracb       orne      'N'
113200060310     c     vidbase       orne      'N'
113300110922     c     Vidctp        orne      *blanks
113400051213     C     VIDblk        ORNE      'N'
113500040527     C     VIDTSP        ORNE      ' '
113600040527     C     VIDTTR        ORNE      ' '
113700040527     C     VIDCPA        ORNE      '  '
113800040527     C     VIDBAM        ORNE      ' '
113900040601     c     vidbva        orne      *blanks
114000060228     c     vidtap        orne      *blanks
114100040527      *
114200040527      *  SE IL FILTRO SU NETWORK TARIFFE E IMPOSTATO AD 'S'
114300040527      *  VOGLIO SOLO CLIENTI CON SERVIZIO IMPOSTATO UGUALE AL CAMPO
114400040527      *  VIDNTW  (DPD, FED, PPT, COR, MES, EEX, EUP, ITA, EST)
114500040527      *
114600040527      *  parto dall'idea di non stampere niente
114700040527     C                   MOVE      ' '           WSISEL
114800040527    6C     VIDNTR        IFEQ      'S'
114900040527      *
115000040527      * DPD
115100040527     C     VIDNTW        IFEQ      'DPD'
115200040527     C     §TADPD        IFEQ      'S'
115300040527     C                   MOVE      'S'           WSISEL
115400040527     C                   ENDIF
115500040527     C                   ENDIF
115600040527      *
115700040527      * FED
115800040527     C     VIDNTW        IFEQ      'FED'
115900040527     C     §TAFED        IFEQ      'S'
116000040527     C                   MOVE      'S'           WSISEL
116100040527     C                   ENDIF
116200040527     C                   ENDIF
116300040527      *
116400040527      * PPT
116500040527     C     VIDNTW        IFEQ      'PPT'
116600040527     C     TAMTSP        IFEQ      'P'
116700040527     C                   MOVE      'S'           WSISEL
116800040527     C                   ENDIF
116900040527     C                   ENDIF
117000040527      *
117100040527      * COR
117200040527     C     VIDNTW        IFEQ      'COR'
117300040527     C     TAMFIE        IFEQ      'I'
117400040527     C     §OGNTW        ANDEQ     'COR'
117500040527     C     §TADPD        ANDNE     'S'
117600040527     C                   MOVE      'S'           WSISEL
117700040527     C                   ENDIF
117800040527     C                   ENDIF
117900040527      *
118000040527      * MES
118100040527     C     VIDNTW        IFEQ      'MES'
118200040527     C     TAMFIE        IFEQ      'I'
118300040527     C     §OGNTW        ANDEQ     'MES'
118400040527     C     §TADPD        ANDNE     'S'
118500040527     C                   MOVE      'S'           WSISEL
118600040527     C                   ENDIF
118700040527     C                   ENDIF
118800040527      *
118900040527      * EEX
119000040527     C     VIDNTW        IFEQ      'EEX'
119100040527     C     TAMFIE        IFEQ      'E'
119200040527     C     §TAFED        ANDNE     'S'
119300040527      * Includiamo i clienti Italia che spediscono
119400040527      * all'estero con EuroExpress
119500040527     C     §OGNTW        IFEQ      'EEX'
119600040527     C     §OGNTW        OREQ      'COR'
119700040527     C     §OGNTW        OREQ      'MES'
119800040527     C                   MOVE      'S'           WSISEL
119900040527     C                   ENDIF
120000040527     C                   ENDIF
120100040527     C                   ENDIF
120200040527      *
120300040527      * EUP
120400040527     C     VIDNTW        IFEQ      'EUP'
120500040527     C     TAMFIE        IFEQ      'E'
120600040527     C     §OGNTW        ANDEQ     'EUP'
120700040527     C     §TAFED        ANDNE     'S'
120800040527     C                   MOVE      'S'           WSISEL
120900040527     C                   ENDIF
121000040527     C                   ENDIF
121100040527      * ITA
121200040527     C     VIDNTW        IFEQ      'ITA'
121300040527     C     TAMFIE        IFEQ      'I'
121400040527     C     §TADPD        ANDNE     'S'
121500040527     C                   MOVE      'S'           WSISEL
121600040527     C                   ENDIF
121700040527     C                   ENDIF
121800040527      * EST
121900040527     C     VIDNTW        IFEQ      'EST'
122000040527     C     TAMFIE        IFNE      'I'
122100040527     C     §TADPD        OREQ      'S'
122200040527     C                   MOVE      'S'           WSISEL
122300040527     C                   ENDIF
122400040527     C                   ENDIF
122500040527      *
122600040527     C     WSISEL        ifne      'S'
122700040527     C                   seton                                        57
122800040527     C                   ENDIF
122900040527      *
123000040527  3 6C                   ENDIF
123100040527  3 5C                   ENDIF
123200040527  3 4C                   ENDIF
123300040527    c
123400920625     C                   SETOFF                                       33
123500911001     C* ESCLUSIONE IN BASE ALLA RICHIESTA O MENO DEL VOLUME
123600911001     C     TAMKPM        COMP      *ZERO                                  34
123700971205     C     VIDSC3        IFEQ      'N'
123800971205     C                   SETON                                        33
123900971205     C                   END
124000930415     C  N34VIDSC3        IFEQ      'S'
124100930415     C                   SETON                                        33
124200911001     C                   END
124300930415     C*
124400940822     C  N34VIDSC3        IFEQ      'C'
124500940822     C     TAMTIN        ANDEQ     'C'
124600940822     C                   SETON                                        33
124700940822     C                   ENDIF
124800940822     C*
124900940822     C  N34VIDSC3        IFEQ      'K'
125000940822     C     TAMTIN        ANDEQ     'K'
125100940822     C                   SETON                                        33
125200940822     C                   ENDIF
125300940822     C*
125400930415     C   34VIDSC3        IFEQ      'E'
125500940822     C     TAMBAP        ANDNE     'A'
125600940822     C     TAMBAP        ANDNE     'B'
125700930415     C                   SETON                                        33
125800911001     C                   END
125900051213     C* ESCLUSIONE IN BASE ALLA RICHIESTA tariffe bloccate
126000051213     c                   SELECT
126100051213      * includo bloccate
126200051213     C                   WHEN      TAMbap = 'B'     and  VIDblk = 'S'
126300051213     C                   eval      *in38 = *on
126400051213      * includo solo no bloccate
126500051213     C                   WHEN      TAMbap <> 'B'    and  VIDblk = 'E'
126600051213     C                   eval      *in38 = *on
126700051213      * no Test
126800051213     C                   WHEN      VIDblk = 'N'
126900051213     C                   eval      *in38 = *on
127000051213      *
127100051213     C                   OTHER
127200051213     C                   eval      *in38 = *off
127300051213     C                   ENDSL
127400970320     C*
127500970320     C* ESCLUSIONE IN BASE ALLA RICHIESTA DELLA CONSEGNA DDT SI/NO/TUTT
127600970320     C                   SETOFF                                       66
127700970320     C     VIDBAM        IFEQ      ' '
127800970320     C                   SETON                                        66
127900970320     C                   ELSE
128000970320     C     VIDBAM        IFEQ      'S'
128100970320     C     TAMBAM        ANDEQ     'S'
128200970320     C                   SETON                                        66
128300970320     C                   ENDIF
128400970320     C     VIDBAM        IFEQ      'N'
128500970320     C     TAMBAM        ANDEQ     'N'
128600970320     C                   SETON                                        66
128700970320     C                   ENDIF
128800970320     C                   ENDIF
128900940822     C*
129000940822     C* ESCLUSIONE IN BASE ALLA RICHIESTA DEL TIPO SERVIZIO
129100940822     C                   SETOFF                                       40
129200940822     C     VIDTSP        IFEQ      ' '
129300940822     C                   SETON                                        40
129400940822     C                   ELSE
129500940822     C     VIDTSP        IFEQ      TAMTSP
129600940822     C                   SETON                                        40
129700940822     C                   ENDIF
129800940822     C                   ENDIF
129900960424     C*
130000960424     C* ESCLUSIONE IN BASE ALLA RICHIESTA DEL TIPO TARIFFA
130100960424     C                   SETOFF                                       64
130200960424     C     VIDTTR        IFEQ      ' '
130300960424     C                   SETON                                        64
130400960424     C                   ELSE
130500960424     C                   MOVEL     TAMCTR        W001              1
130600960424     C     VIDTTR        IFEQ      W001
130700960424     C                   SETON                                        64
130800960424     C                   ENDIF
130900960424     C                   ENDIF
131000040601
131100040601      * ESCLUSIONE IN BASE ALLA RICHIESTA DEL TIPO MERCE
131200040601     c                   Eval      *In67 = *Off
131300040601     c                   If        VidBva = *Blanks
131400040601     c                   Eval      *In67 = *On
131500040601     c                   Else
131600040601     c                   Move      §tabva        w001
131700040601     c                   If        VidBva = w001
131800040601     c                   Eval      *In67 = *On
131900040601     c                   EndIf
132000040601     c                   EndIf
132100060227
132200060227      * ESCLUSIONE IN BASE ALLA RICHIESTA DEL TIPO UTILIZZO PESO VDL
132300060227     c                   eval      *in35 = *off
132400060228     c                   if        vidtap = *blanks
132500060228     c                   eval      *in35 = *on
132600060228     c                   else
132700060228     c                   if        vidtap = 'V' and §tatap = *blanks
132800060227     c                   eval      *in35 = *on
132900060228     c                   else
133000060228     c                   if        vidtap = §tatap
133100060227     c                   eval      *in35 = *on
133200060228     c                   endif
133300060228     c                   endif
133400060228     c                   endif
133500960424     C*
133600960424     C* ESCLUSIONE IN BASE ALLA RICHIESTA DELLE PARTICOLARITA'
133700960424     C                   SETOFF                                       65
133800960424     C     VIDCPA        IFEQ      '  '
133900960424     C                   SETON                                        65
134000960424     C                   ELSE
134100960424     C     VIDCPA        IFEQ      TAMGCA
134200960424     C     VIDCPA        OREQ      TAMGGA
134300960424     C     VIDCPA        OREQ      TAMGMA
134400960424     C     VIDCPA        OREQ      TAMGVA
134500960424     C                   SETON                                        65
134600960424     C                   ENDIF
134700960424     C                   ENDIF
134800920625     C*
134900920625     C* ESCLUSIONE IN BASE ALLA RICHIESTA O MENO ISTAT
135000040119     c                   SELECT
135100040119      * includo solo con ISTAT
135200101015     C                   WHEN      TAMrct > *zeros  and  VIDIST = 'S'
135300040119     C                   eval      *in36 = *on
135400040119      * includo solo senza ISTAT
135500101015     C                   WHEN      TAMrct = *zeros  and  VIDIST = 'E'
135600040119     C                   eval      *in36 = *on
135700040119      * no Test
135800040119     C                   WHEN      VIDIST = 'N'
135900040119     C                   eval      *in36 = *on
136000040119      *
136100040119     C                   OTHER
136200040119     C                   eval      *in36 = *off
136300040119     C                   ENDSL
136400060310
136500060310      * esclusione in base a rinuncia AC BASE
136600060310     c                   select
136700060310      * includo solo se rinuncia inserita
136800060310     c                   when      tamfmp = 'S' and vidracb = 'S'
136900060310     c                   eval      *in41 = *on
137000060310      * includo solo senza rinuncia
137100060310     c                   when      tamfmp = *blanks and vidracb = 'E'
137200060310     c                   eval      *in41 = *on
137300060310      * no Test
137400060310     c                   when      vidracb = 'N'
137500060310     c                   eval      *in41 = *on
137600060310     c                   other
137700060310     c                   eval      *in41 = *off
137800060310     c                   endsl
137900920625     C*
138000060310     C* ESCLUSIONE IN BASE ALLA RICHIESTA O MENO DELL'AC PLUS
138100930519     C                   SETOFF                                       37
138200990714     C                   MOVEL     'C '          FTC
138300020912     C                   Z-ADD     0             COMPAC            8 3
138400930519     C                   MOVEL     ' '           COMFV1            1
138500930519     C*
138600990714     C     KTPT          CHAIN     TITPT000                           32
138700930519     C     VIDSAC        IFEQ      'N'
138800921123     C                   SETON                                        37
138900930519     C  N32TPTATB        IFEQ      ' '
139000930519     C                   Z-ADD     TPTVLM        COMPAC
139100930519     C                   END
139200930519     C*
139300921123     C                   ELSE
139400930519     C*
139500930519     C  N32TPTATB        IFEQ      ' '
139600930519     C     VIDSAC        ANDEQ     'S'
139700930519     C                   SETON                                        37
139800930519     C                   Z-ADD     TPTVLM        COMPAC
139900920625     C                   END
140000060310      * Solo reale
140100060310     c                   if        vidsac = 'R' and not *in32 and
140200060310     c                             tptatb = *blanks and tpttma = *blanks
140300060310     c                   Eval      *in37 = *on
140400060310     c                   z-add     tptvlm        compac
140500060310     c                   endif
140600060310      * Solo fittizio
140700060310     c                   if        vidsac = 'F' and not *in32 and
140800060310     c                             tptatb = *blanks and tpttma = 'F'
140900060310     c                   Eval      *in37 = *on
141000060310     c                   z-add     tptvlm        compac
141100060310     c                   endif
141200930519     C*
141300020502     c     *in32         ifeq      *on
141400020502     c     tptatb        orne      *blanks
141500020502     C     VIDSAC        IFEQ      'E'
141600930519     C                   SETON                                        37
141700920625     C                   END
141800020502     C                   END
141900920625     C                   END
142000921123     C*
142100930519     C* TIPO VALORE MERCE
142200930519     C     COMPAC        IFGT      0
142300930519     C     TPTFVM        IFEQ      '1'
142400930519     C                   MOVEL     'C'           COMFV1
142500930519     C                   ELSE
142600930519     C     TPTFVM        IFEQ      '2'
142700930519     C                   MOVEL     'S'           COMFV1
142800930519     C                   ELSE
142900930519     C                   MOVEL     'K'           COMFV1
143000930519     C                   END
143100930519     C                   END
143200930519     C                   END
143300060310
143400060310      * esclusione in base alla richiesta AC BASE
143500060310     c                   eval      *in42 = *off
143600060313     c                   clear                   comacbase         1
143700060310     c                   eval      ftc = 'd '
143800060313     c     ktpt          chain     titpt01l
143900060313    1c                   if        vidbase = 'N'
144000060310     c                   eval      *in42 = *on
144100060313     c                   if        %found(titpt01l) and tptatb = *blanks
144200060313     c                   eval      comacbase = 'S'
144300060313     c                   endif
144400060313   x1c                   else
144500060310     c                   if        %found(titpt01l) and tptatb = *blanks
144600060310     c                             and vidbase = 'S'
144700060310     c                   eval      *in42 = *on
144800060313     c                   eval      comacbase = 'S'
144900060310     c                   endif
145000060310
145100060314     c                   if        (not %found(titpt01l) or tptatb <> *blanks)
145200060310     c                             and vidbase = 'E'
145300060310     c                   eval      *in42 = *on
145400060310     c                   endif
145500060313    1c                   endif
145600110922
145700110922      * Tariffa particolare richiesta
145800110922     c                   eval      *in43 = *off
145900110922     c                   clear                   woktp
146000110922     c                   IF        VIDctp <> *blanks
146100110922     c                   eval      ftc = VIDctp
146200110922     c     Ktpt          chain     TITPT01L
146300110922     c                   IF        %found(TITPT01L) and TPTatb = *blanks
146400110922
146500110922      * Cerco dettaglio
146600110922     c     Ktpt          setll     TITPD01L
146700110922     c                   DO        *hival
146800110922     c     Ktpt          reade     TITPD01L
146900110922      * fine file
147000110922     c                   IF        %eof(TITPD01L)
147100110922     c                   leave
147200110922     c                   ENDIF
147300110922      * torno a leggere se trovo righe annullate
147400110922     c                   IF        TPDatb <> *blanks
147500110922     c                   iter
147600110922     c                   ENDIF
147700110922      * valorizzo il flag per la tariffa particolare
147800110922      * S = trovata
147900110922      * V = valorizzata
148000110922      * A = azzerata
148100110922     c                   SELECT
148200110922      * importo maggiore di 0 è valorizzata esco dal ciclo
148300110922     c                   WHEN      TPDitr > 0
148400110922     c                   eval      woktp = 'V'
148500110922     c                   leave
148600110922      * importo a 0 è azzerata
148700110922     c                   WHEN      TPDitr = 0
148800110922     c                   eval      woktp = 'A'
148900110922      * tariffa particolare trovata nel dettaglio
149000110922     c                   OTHER
149100110922     c                   eval      woktp = 'S'
149200110922     c                   ENDSL
149300110922
149400110922     c                   ENDDO
149500110922
149600110922     c                   ENDIF
149700110922     c                   ENDIF
149800110922
149900110922      * in base al flag per la tariffa particolare decido se la tariffa è da
150000110922      * stampare o meno
150100110922     c                   clear                   xx
150200110922     c                   SELECT
150300110922      * se richiesto 'E' - senza tariffe e richesto un ntw controllo se
150400110922      * tariffa abilitata per il ntw richiesto
150500110922     c                   WHEN      VIDstp = 'E' and woktp = *blanks and
150600110922     c                             VIDntw <> *blanks
150700110922     c                   eval      xx = %lookup(VIDctp:$1P)
150800110922     c                   IF        xx > 0
150900110922     c                   IF        VIDntw = 'ITA' and $tco(xx) <> 'N'
151000110922     c                   eval      *in43 = *on
151100110922     c                   ENDIF
151200110922     c                   IF        VIDntw = 'EEX' and $fie(xx) <> 'N'
151300110922     c                   eval      *in43 = *on
151400110922     c                   ENDIF
151500110922     c                   IF        VIDntw = 'DPD' and $fdp(xx) <> 'N'
151600110922     c                   eval      *in43 = *on
151700110922     c                   ENDIF
151800110922     c                   IF        VIDntw = 'FED' and $ffe(xx) <> 'N'
151900110922     c                   eval      *in43 = *on
152000110922     c                   ENDIF
152100110922     c                   ENDIF
152200110922     c                   WHEN      VIDstp = 'E' and woktp = *blanks
152300110922     c                   eval      *in43 = *on
152400110922     c                   WHEN      VIDstp = *blanks
152500110922     c                   eval      *in43 = *on
152600110922     c                   WHEN      VIDstp = 'S' and woktp <> *blanks
152700110922     c                   eval      *in43 = *on
152800110922     c                   WHEN      VIDstp = 'V' and woktp = 'V'
152900110922     c                   eval      *in43 = *on
153000110922     c                   WHEN      VIDstp = 'A' and woktp = 'A'
153100110922     c                   eval      *in43 = *on
153200110922     c                   ENDSL
153300110926      * Mi memorizzo sempre anche la presenza o meno del fuel
153400070130      * Supplemento Carburante Fuel Surcharge
153500070130      * in caso di richiesta delle tariffe senza fuel escludo tariffe FEDEX
153600070129     c                   movel     'f '          ftc
153700070129     c                   eval      contatore = 0
153800070129     c                   eval      comfue  = ' '
153900070129
154000070129     c     Ktpt          chain     Titpt01l
154100070129    1c                   if        %found(Titpt01l) and
154200070129     c                             tptatb = ' '
154300070129      * cerco importo percentuale
154400070129     c     ktpt          setll     titpd01l
154500070129    2c                   do        *hival
154600070129     c     ktpt          reade     titpd01l
154700070129      * fine file
154800070129     c                   if        %eof(titpd01l)
154900070129     c                   leave
155000070129     c                   endif
155100070129      * non annullate
155200070129     c                   if        tpdatb <> *blanks
155300070129     c                   iter
155400070129     c                   endif
155500070129      * scaglioni
155600070129     c                   add       1             contatore
155700070129      * 1° scaglione
155800070129     c                   if        contatore = 1
155900070129      * importo maggiore di zero
156000070129     c                   if        tpditr > 0
156100070129     c                   eval      comfue = 'S'
156200070129     c                   leave
156300070129     c                   endif
156400070129     c                   else
156500070129      * importo maggiore di zero dopo il primo scaglione
156600070129     c                   if        tpditr > 0
156700070129     c                   eval      comfue = 'S'
156800070129     c                   leave
156900070129     c                   endif
157000070129
157100070129     c                   endif
157200070129
157300070129    2c                   enddo
157400070129    1c                   endif
157500930519     C*
157600010618     C   33
157700060227     CAN 35
157800040119     CAN 36
157900020307     CAN 37
158000020307     CAN 38
158100020307     CAN 40
158200060310     CAN 41
158300060310     CAN 42
158400070129     CAN 43
158500020307     CAN 64
158600040527     CANN57
158700020307     CAN 65
158800040601     CAN 66
158900040601     can 67              do
159000900831     C                   ADD       1             X
159100110926     C                   MOVE      *BLANK        WAR
159200110926     C                   MOVEA     §CTR          WAR(1)
159300110926     C                   MOVEA     '-'           WAR(4)
159400110926     C                   MOVEA     TAMFTP        WAR(5)
159500110926     C                   MOVEA     TAMBAP        WAR(7)
159600110926     C     §TADPD        IFNE      'S'
159700110926     C     §TAfed        andne     'S'
159800110926     C                   MOVEA     TAMFIE        WAR(10)
159900110926     C                   ELSE
160000110926     C     §TADPD        IFeq      'S'
160100110926     C                   MOVEA     'D'           WAR(10)
160200110926     C                   ENDIF
160300110926     C     §TAfed        IFeq      'S'
160400110926     C                   MOVEA     'F'           WAR(10)
160500110926     C                   ENDIF
160600110926     C                   ENDIF
160700110926     C                   MOVEA     TAMTSP        WAR(12)
160800110926     C**!!!              MOVEL     TAMDCV        COMDCV           10
160900110926     C**!!!              MOVEA     COMDCV        WAR(14)
161000110926     C                   MOVEA     WAR           WAR12            12
161100110926     C                   MOVEL     WAR12         DAR(X)
161200900831     C                   MOVE      TAMCTR        CAR(X)
161300901008     C                   Z-ADD     TAMKPM        KPM(X)
161400910904     C                   MOVEL     TAMTIN        TIN(X)
161500920624     C*
161600101015     C                   z-add     TAMrct        IS1(X)
161700040119     C                   Z-ADD     TAMPRI        IS2(X)
161800920624     C*
161900921124     C                   Z-ADD     COMPAC        AC1(X)
162000930519     C                   MOVEL     COMFV1        FV1(X)
162100920624     C*
162200070129     C                   MOVEL     COMFUE        Fuel(X)
162300110926     c                   IF        woktp <> *blanks
162400110926     c                   eval      tarpar(x) = 'S'
162500110926     c                   ENDIF
162600070129     C*
162700950208     C* TIPO COMUNICAZIONE AL MITTENTE
162800990714     C     KTGC          CHAIN     TITGC000                           29
162900950208     C     *IN29         IFEQ      *OFF
163000950208     C     TGCATB        ANDEQ     ' '
163100950208     C     TGCTCM        IFEQ      ' '
163200950208     C                   MOVEL     VIDFL0        TCM(X)
163300950208     C                   ELSE
163400950208     C                   MOVEL     TGCTCM        TCM(X)
163500950208     C                   ENDIF
163600950208     C*
163700950208     C                   ELSE
163800950208     C* SE NON ESISTE TARIFFA GIACENZA PRENDO DA ANAGRAFICA
163900950208     C                   MOVEL     VIDFL0        TCM(X)
164000950208     C                   ENDIF
164100970320     C                   MOVEL     TAMBAM        BAM(X)
164200040601     c                   movel     §tabva        bva(x)
164300060310     c                   movel     §tatap        tap(x)
164400060314     c                   if        tamfmp = 'S'
164500060314     c                   move      'R'           acbase(x)
164600060314     c                   else
164700060313     c                   movel     comacbase     acbase(x)
164800060314     c                   endif
164900950208     C*
165000930211     C     X             COMP      5                                      30
165100900831     C                   END
165200900901     C                   END
165300911001     C                   END
165400900901     C  N30TAMKSC        COMP      ACOKSC                             3030
165500900831     C  N30              END
165600911001     C*
165700910904     C     KPM(1)        IFNE      0
165800910904     C     TIN(1)        COMP      'C'                                    50
165900910904     C                   END
166000910904     C     KPM(2)        IFNE      0
166100910904     C     TIN(2)        COMP      'C'                                    51
166200910904     C                   END
166300910904     C     KPM(3)        IFNE      0
166400910904     C     TIN(3)        COMP      'C'                                    53
166500910904     C                   END
166600910904     C     KPM(4)        IFNE      0
166700910904     C     TIN(4)        COMP      'C'                                    55
166800910904     C                   END
166900910904     C     KPM(5)        IFNE      0
167000910904     C     TIN(5)        COMP      'C'                                    56
167100910904     C                   END
167200921124     C*
167300900831     C                   ENDSR
167400001204     C*
167500001204     C*--- STAMPA NOTE -----------------------------------------------*
167600001204     C     SRNOTE        BEGSR
167700001204      *
167800001204     C                   SETOFF                                       90
167900001204      *
168000001204     C                   MOVEL     ACOKCC        KNK1
168100001204     C                   MOVE      ACOKSC        KNK1
168200001204     C     KTFN1         SETLL     TFNTC01L
168300001204     C                   DO        *HIVAL
168400001204     C     KTFN1         READE     TFNTC01L                               84
168500001204     C     *IN84         IFEQ      *ON
168600001204     C                   LEAVE
168700001204     C                   ENDIF
168800010130     C     NTCSNS        IFEQ      'N'
168900001204     C                   ITER
169000001205     C                   ENDIF
169100010130     C     NTCFLT        IFEQ      'A'
169200010130     C                   ITER
169300010130     C                   ENDIF
169400010130     C     NTCRNT        IFEQ      *BLANKS
169500001205     C                   ITER
169600001204     C                   ENDIF
169700001204     C                   EXCEPT    NOTE
169800001204     C                   SETON                                        90
169900001204     C                   ENDDO
170000001204      *
170100001204     C                   ENDSR
170200900831     C/SPACE
170300940903     C*---------------------------------------------------------------*
170400950208     OPRTF198   E            RICHI          1 02
170500940903     O                     10SK(1)              120
170600940903     O                     11SK(2)              120
170700940903     O                       RSUT                20
170800940903     O                                           21 '/'
170900940903     O                       SIMFEL              24
171000940903     O                                          153 'TNTA08R'
171100940903     O                       UDATE8             170 '  /  /    '
171200940903     O                                          193 'pag.'
171300940903     O                       PAGE          Z    198
171400950208     O          E            RICHI          2
171500940903     O                       KNSIF               10
171600940903     O                       KNMUS               22
171700940903     O                       HHMMSS             168 '  :  :  '
171800940903     O          E            RICHI          2
171900940903     O                                           20 'P A R A M E T R I'
172000940903     O                                           40 'R I C H I E S T I'
172100920613     O          E            RICHI          2
172200960930     O                                           24 'Punti Operativi ....:'
172300920613     O                       VIDFIL            +  1
172400940903     O          E            RICHI          2
172500940903     O                                           24 'Ordinamento stampa .:'
172600940903     O                       VIDSC1            +  1
172700001204     O          E            RICHI          2
172800001204     O                                           24 'Stampa note cliente :'
172900001204     O                       VISTPN            +  1
173000020312     O          E            RICHI          2
173100020312     O                                           24 'Tipo Stampa ........:'
173200020312     O                       VIDTPS            +  1
173300020312     O          E            RICHI          2
173400060227     O                                           22 'S E L E Z I O N I  S U'
173500060227     O                                           38 '  T A R I F F E'
173600940903     O          E            RICHI          2
173700040119     O                                           24 'Parzializz. ISTAT   :'
173800040119     O                       VIDIST            +  1
173900040119     O          E            RICHI          2
174000940903     O                                           24 'Parzializz. Volume .:'
174100920613     O                       VIDSC3            +  1
174200060310     O          E            RICHI          2
174300060310     O                                           24 'Rinuncia AC BASE....:'
174400060310     O                       vidracb           +  1
174500060310     O          E            RICHI          2
174600060310     O                                           24 'Parz. AC PLUS.......:'
174700060310     O                       VIDSAC            +  1
174800060310     O          E            RICHI          2
174900060310     O                                           24 'Parz. AC BASE.......:'
175000060310     O                       vidbase           +  1
175100070129     O          E            RICHI          2
175200110921     O                                           24 'Parz. Tar.Partic....:'
175300110921     O                       vidctp            +  1
175400110927     o                       desctp            +  1
175500110921     o                       desstp            +  2
175600051213     O          E            RICHI          2
175700051213     O                                           24 'Parz. tariffe blocc.:'
175800051213     O                       VIDblk            +  1
175900940903     O          E            RICHI          2
176000970320     O                                           24 'Parz. Consegna D D T:'
176100970320     O                       VIDBAM            +  1
176200940903     O          E            RICHI          2
176300940903     O                                           24 'Parz. Tipo Servizio :'
176400940903     O                       VIDTSP            +  1
176500960424     O          E            RICHI          2
176600960424     O                                           24 'Parz. Tipo Tariffa  :'
176700960424     O                       VIDTTR            +  1
176800040601     O          E            RICHI          2
176900040601     O                                           24 'Parz. Tipo Merce    :'
177000040601     O                       vidbva            +  1
177100000725     O          E            RICHI          2
177200020312     O                                           24 'Parz. Particolarità :'
177300020312     O                       VIDCPA            +  1
177400020312     O          E            RICHI          2
177500020307     O                                           24 'Parzializz.Tarif NTW:'
177600020312     O                       VIDNTR            +  1
177700020312     O                       VIDNTW            +  2
177800060227     O          E            RICHI          2
177900060227     O                                           24 'Parzializz.Peso VDL :'
178000060227     O                       VIDtap            +  1
178100020312     O          E            RICHI          2
178200020312     O                                           32 'S E L E Z I O N I  S U  -
178300020312     O                                              P. D. C.'
178400920613     O          E            RICHI          2
178500920613     O                                           24 'Parzializz.Categoria:'
178600920613     O                       VIDSNC            +  1
178700920613     O                       VIDSCT            +  2
178800001204     O          E            RICHI          2
178900001204     O                                           24 'Parz.NOTE Cliente ..:'
179000001204     O                       VINOTE            +  1
179100001204     O                       VITNOT            +  2
179200051128     O          E            RICHI          2
179300051128     O                                           24 'Parz.LUOGO .........:'
179400051128     O                       VIluog            +  1
179500051128     O                       VITluo            +  2
179600011211     O          E            RICHI          2
179700020312     O                                           24 'Parzializz.Cod.comm.:'
179800011211     O                       VIDAGE            +  1
179900020312     O          E            RICHI          2
180000020312     O                                           24 'Parzializz.Cod.imp. :'
180100020312     O                       VIDCIC            +  1
180200020312     O          E            RICHI          2
180300020312     O                                           24 'Parzializz.Clienti .:'
180400020312     O                       VIDSC2            +  1
180500020404     O          E            RICHI          2
180600020404     O                                           24 'Clienti no operativi:'
180700020404     O                       VIDCNO            +  1
180800020429     O                                         +  2 'nel periodo da .:'
180900020429     O                       VIDDMM            +  1
181000020429     O                                         +  0 '/'
181100020429     O                       VIDDAA            +  0
181200020429     O                                         +  2 'a .:'
181300020429     O                       VIDAMM            +  1
181400020429     O                                         +  0 '/'
181500020429     O                       VIDAAA            +  0
181600020429     O                                         +  3 '(mm/aa)'
181700050617     O          E    15      RICHI          2
181800050617     O                                           24 'Parz.Tipo Pagam. C/A:'
181900050617     o                                         +  1 '"'
182000050617     O                       VIDtpc            +  0
182100050617     o                                         +  0 '"'
182200050617     O          E   n15      RICHI          2
182300050617     O                                           24 'Parz.Tipo Pagam. C/A:'
182400050617     O                       VIDtpc            +  1
182500121227     O          E            RICHI          2
182600121227     O                                           24 'Parz.Tass.POST Fatt.:'
182700121227     O                       VIDtpf            +  1
182800950208     O          E            TESTA          1 02
182900940903     O                     10SK(1)              120
183000940903     O                     11SK(2)              120
183100900901     O                       RSUT                20
183200910904     O                                           21 '/'
183300910904     O                       SIMFEL              24
183400940903     O                                          153 'TNTA08R'
183500940903     O                       UDATE8             170 '  /  /    '
183600940903     O                                          193 'pag.'
183700940903     O                       PAGE          Z    198
183800950208     O          E            TESTA          2
183900940903     O                       KNSIF               10
184000940903     O                       KNMUS               22
184100940903     O                       HHMMSS             168 '  :  :  '
184200900831     O          E            TESTA          2
184300960930     O                                         +  4 'Punto Operativo:'
184400900901     O                       FIL(W)        Z   +  1
184500900831     O                       ORGDES            +  2
184600900901     O          E            TESTA          1
184700920624     O                       SK1                198
184800900901     O          E            TESTA          1
184900920624     O                       SK2                198
185000920613     O          E            TESTA          1
185100920624     O                       SK3                198
185200110927     o                       vidctp             179
185300920624     O          E            TESTA          1
185400920624     O                       SK4                198
185500920624     O          E            TESTA          2
185600920624     O                       SK5                198
185700920624     O* sk1
185800900901     O          E            DETT        1  1
185900900831     O                       ACOKSC
186000970320     O                       ACOABL               9
186100970320     O                       ACORAG              59
186200970320     O                    N70INDABI        Z     67
186300970320     O                     46                    68 '/'
186400970320     O                    N70INDCAB        Z     73
186500970320     O                     46                    74 '-'
186600080211     O                    N70wbanca             100
186700110926     O                       DAR(1)             129
186800110926     O                       IS1(1)        z    132
186900110926     O                       IS2(1)        4    139
187000110926     o                       tap(1)             143
187100110926     o                       acbase(1)          149
187200110926     O                       AC1(1)        4    165
187300110926     O                       FV1(1)             166
187400110926     O                       FUEL(1)            171
187500110926     O                       tarpar(1)          180
187600040601     O                 N50   KPM(1)        4    192
187700040601     O                  50   KPM(1)             192 '0 ,   '
187800110926     O**!!!                  TCM(1)             194
187900040601     O                       BAM(1)             196
188000040601     o                       bva(1)             198
188100920624     O* sk2
188200900831     O          E            DETT           1
188300970320     O                    N70INDVIA              41
188400970320     O                    N70INDCIT              72
188500970320     O                    N70INDCAE              82
188600970320     O                    N70INDPRV              85
188700970320     O                    N70INDSTA              91
188800970320     O                    N71CLPAGE        Z     99
188900970320     O                    N71CLPCLV             103
189000110926     O                       DAR(2)             129
189100110926     O                       IS1(2)        z    132
189200110926     O                       IS2(2)        4    139
189300110926     o                       tap(2)             143
189400110926     o                       acbase(2)          149
189500110926     O                       AC1(2)        4    165
189600110926     O                       FV1(2)             166
189700110926     O                       FUEL(2)            171
189800110926     O                       tarpar(2)          180
189900040601     O                 N51   KPM(2)        4    192
190000040601     O                  51   KPM(2)             192 '0 ,   '
190100110926     O**!!!                  TCM(2)             194
190200040601     O                       BAM(2)             196
190300040601     o                       bva(2)             198
190400920624     O* sk3
190500900901     O          E            DETT           1
190600970320     O                    N70INDTEL              27
190700970320     O                    N70INDTLF              44
190800970320     O                    N70INDTLX              61
190900970320     O                       ACOITC        Z     73
191000970320     O                     47                    74 '-'
191100970320     O                       DESSCT             100
191200970320     O                       ACOLIB        Z    113
191300110926     O                       DAR(3)             129
191400110926     O                       IS1(3)        z    132
191500110926     O                       IS2(3)        4    139
191600110926     o                       tap(3)             143
191700110926     o                       acbase(3)          149
191800110926     O                       AC1(3)        4    165
191900110926     O                       FV1(3)             166
192000110926     O                       FUEL(3)            171
192100110926     O                       tarpar(3)          180
192200040601     O                 N53   KPM(3)        4    192
192300040601     O                  53   KPM(3)             192 '0 ,   '
192400110926     O**!!!                  TCM(3)             194
192500040601     O                       BAM(3)             196
192600040601     o                       bva(3)             198
192700920624     O* sk4
192800920624     O          E            DETT           1
192900970320     O                    N70INDCDF              27
193000970320     O                    N70INDIVA              48
193100970320     O                    N70§CDPAG              73
193200970320     O                                           74 '-'
193300970320     O                    N70§DEPAG             100
193400110926     O                       DAR(4)             129
193500110926     O                       IS1(4)        z    132
193600110926     O                       IS2(4)        4    139
193700110926     o                       tap(4)             143
193800110926     o                       acbase(4)          149
193900110926     O                       AC1(4)        4    165
194000110926     O                       FV1(4)             166
194100110926     O                       FUEL(4)            171
194200110926     O                       tarpar(4)          180
194300040601     O                 N55   KPM(4)        4    192
194400040601     O                  55   KPM(4)             192 '0 ,   '
194500110926     O**!!!                  TCM(4)             194
194600040601     O                       BAM(4)             196
194700040601     o                       bva(4)             198
194800920624     O* sk5
194900920613     O          E            DETT           1
195000050617     O                    N71stafpc              46
195100050617     O                    N71desfpc              62
195200080117     o                    n71iban1              + 1
195300080117     o                    n71iban2              + 1
195400080117     o                    n71iban3              + 1
195500080117     o                    n71iban4              + 1
195600080117     o                    n71iban5              + 1
195700080117     o                    n71iban6              + 1
195800080117     o                    n71iban7              + 1
195900080211     o                    n71iban8              + 1
196000110926     O                       DAR(5)             129
196100110926     O                       IS1(5)        z    132
196200110926     O                       IS2(5)        4    139
196300110926     o                       tap(5)             143
196400110926     o                       acbase(5)          149
196500110926     O                       AC1(5)        4    165
196600110926     O                       FV1(5)             166
196700110926     O                       FUEL(5)            171
196800110926     O                       tarpar(5)          180
196900040601     O                 N56   KPM(5)        4    192
197000040601     O                  56   KPM(5)             192 '0 ,   '
197100110926     O**!!!                  TCM(5)             194
197200040601     O                       BAM(5)             196
197300040601     o                       bva(5)             198
197400001204     O* Note
197500001204     O          E            NOTE           1
197600001204     O              N90                          16 'Note:'
197700001204     O                       NTCTNT              19
197800001204     O                       NTCRNT              80
197900910927     O*
198000931228     O          E            TOTFIL      3
198100960930     O                                              'Totale Clienti'
198200960930     O                                         +  1 'Punto Operativo :'
198300910927     O                       §CTRC         2   +  1
198400910927     O*
198500950208     O          E            TOTFIN         1 02
198600940903     O                     10SK(1)              120
198700940903     O                     11SK(2)              120
198800910927     O                       RSUT                20
198900910927     O                                           21 '/'
199000910927     O                       SIMFEL              24
199100940903     O                                          153 'TNTA08R'
199200940903     O                       UDATE8             170 '  /  /    '
199300940903     O                                          193 'pag.'
199400940903     O                       PAGE          Z    198
199500950208     O          E            TOTFIN         2
199600940903     O                       KNSIF               10
199700940903     O                       KNMUS               22
199800940903     O                       HHMMSS             168 '  :  :  '
199900910927     O          E            TOTFIN      2
200000960930     O                                           32 'Totale Clienti Finale :'
200100910927     O                       §CTRT         2   +  1
200200020307      *
200300020308      * Stampa sintetica
200400020308     O          E            TESTA1         1 02
200500020308     O                     10SK(1)              120
200600020308     O                     11SK(2)              120
200700020308     O                       RSUT                20
200800020308     O                                           21 '/'
200900020308     O                       SIMFEL              24
201000020308     O                                          153 'TNTA08R'
201100020308     O                       UDATE8             170 '  /  /    '
201200020308     O                                          193 'pag.'
201300020308     O                       PAGE          Z    198
201400020308     O          E            TESTA1         2
201500020308     O                       KNSIF               10
201600020308     O                       KNMUS               22
201700020308     O                       HHMMSS             168 '  :  :  '
201800020308     O          E            TESTA1         2
201900020308     O                                         +  4 'Punto Operativo:'
202000020308     O                       FIL(W)        Z   +  1
202100020308     O                       ORGDES            +  2
202200020307     O          E            TESTA1         1
202300020307     O                       SK6                198
202400020308     O* sk6
202500020312     O          E            DETT1          1
202600020307     O                       ACOKSC
202700130315     O                       ACOABL            +  2
202800130315     O                       ACORAG            +  3
202900020312     O                    N70INDCIT            +  1
203000020314     O                    N70INDPRV            +  1
203100020312     O                    N70INDCAE            +  1
203200020312     O                       ACOITC        Z   +  1
203300020312     O                     47                  +  1 '-'
203400020312     O                       DESSCT            +  1
203500020312     O                    N71CLPAGE        Z   +  3
203600020312     O                    N71CLPDESP           +  1
203700020314     O                    N71CLPCLV            +  9
203800020314     O                       ACOLIB        Z   +  9
203900020308**   SK
204000020307                      ANAGRAFICO CLIENTI  -IN ORDINE ALFABETICO-   1 a
204100020307                      ANAGRAFICO CLIENTI  -IN ORDINE DI CODICE-    2 b
204200920624**   SK1
204300970320Codice Blo Ragione Sociale Cliente                              AB 3 a
204400970320I/CAB   Descrizione Banca Appoggio               C  O  N  D  I  Z  4 b
204500950208 I  O  N  I      T  A  R  I  F  F  A  R  I  E                      5 c
204600920624**   SK2
204700970320 Cli.  cc. Indirizzo                      Località                 6 a
204800110926       C.A.P     Pro Naz Commerc. CD.Imp            Pr Pa I Se |-  7 b
204900110926ISTAT -| Pes| AC BASE|  AC PLUS   | FUEL | Tar.Par|  Kg.xMc    D V 8 c
205000070129**   SK3
205100110926           Telefono         Fax                                   12 a
205200080307   Categ.Merceol.                     Potenzial     ef Ar E rv    13 b
205300110927       | VDL|            ValMer              " "     Mc.xCl    D A14 c
205400920624**   SK4
205500970320           Codice Fiscale       Partita IVA                       15 a
205600000725   Condizione Pagamento                             er Bl D iz    16 b
205700110926                                                               T L17 c
205800920624**   SK5
205900080117                                             Pagamento C/A     IBA 9 a
206000080117N per Bonif. Contrassegno                                         10 b
206100921124                                                                  11 c
206200020307**   SK6
206300020314Codice Cli.  Ragione Sociale Cliente                          Loca
206400080307lità                          C.A.P.        Categ.Merceol.
206500020314              Cd.Commerciale          Cd.Importanza  Cd.Potenziale
