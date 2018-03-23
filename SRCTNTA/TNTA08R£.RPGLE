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
015900020312     D DS01          E DS
016000020307     D OG143         E DS
016100900901     D KPJBA         E DS
016200040930     d Trul31ds      e ds
016300110922     d DS1P          e ds
016400110922
016500110922     d Azuteds       e ds                  extname(Azute00f)
016600110922     d dDatiute      e ds
016700110922     d Tibs34ds      e ds
016800080117
016900080117     d clpbab          ds
017000080117     d  iban1                  1      4
017100080117     d  iban2                  5      8
017200080117     d  iban3                  9     12
017300080117     d  iban4                 13     16
017400080117     d  iban5                 17     20
017500080117     d  iban6                 21     24
017600080117     d  iban7                 25     28
017700080211     d  iban8                 29     32
017800080117
017900070129      *-------------------- variabili ---------------------------------------
018000050617     d stafpc          s                   like(clpfpc)
018100070129     d contatore       s              2  0
018200070129     d comfue          s              1
018300080211     d wbanca          s             26
018400110927     d desctp          s             25    inz
018500110921     d desstp          s             40    inz
018600110922     d codut           s              1  0 Inz(1)
018700110922     d xx              s              4  0 inz
018800110922     d woktp           s              1
018900110922     d x               s              3  0
019000110926     d vidfuel         s              1    inz('f')
019100110922
019200950717     C*****************************************************************
019300921124     C*  RIEPILOGO INDICATORI
019400950717     C*****************************************************************
019500921124     C* 10    - SCELTA ORDINAMENTO STAMPA PER RAGIONE SOCIALE
019600921124     C* 11    - SCELTA ORDINAMENTO STAMPA PER CODICE CLIENTE
019700921124     C* 30/34 - DI COMODO
019800921124     C* 33    - ESCLUSIONI
019900070129     C* 35/43 - ESCLUSIONI
020000990714     C* 46/48 - CONDIZIONANO L'EMISSIONE DEI TRATTINI SEPARATORI
020100960424     C* 50/51 - DI COMODO
020200960424     C* 53/56 - DI COMODO
020300000725     C* 57    - SERVIZI DPD
020400000725     C* 58    - DI COMODO
020500960424     C* 60    - CLIENTE BLOCCATO
020600960424     C* 61/63 - PARZIALIZZAZIONE CLIENTI NON ABILITATI
020700040601     C* 64/67 - ESCLUSIONI
020800921124     C* 70/72 - DI COMODO
020900950717     C* 84    - DI COMODO
021000010130     C* 85    - DI COMODO
021100921124     C*****************************************************************
021200110922     C*
021300110922      * Carico le tariffe particolari in sk
021400110922     c                   exsr      car1p
021500110922
021600000000     C*
021700940903     C* POSIZIONAMENTO PER LETTURA
021800900831     C     VIDSC1        IFEQ      'C'
021900900831     C                   OPEN      CNACO00F
022000900831     C                   SETON                                        11
022100900831     C                   ELSE
022200900831     C                   OPEN      CNACO04L
022300900831     C                   SETON                                        10
022400900831     C                   END
022500940903     C*
022600940903     C* STAMPA RICHIESTE EFFETTUATE A VIDEO
022700050617     c                   if        vidtpc<>'N'
022800050617     c                   seton                                        15
022900050617     c                   endif
023000110921
023100110927      * decodifico tar.particolare
023200110927     c                   clear                   ds1P
023300110927     c                   eval      cod = '1P'
023400110927     c                   eval      key = vidctp
023500110927     c     ktab1         chain     TABEL00F
023600110927     c                   IF        %found(TABEL00F)
023700110927     c                   eval      ds1P = TBLuni
023800110927     c                   ENDIF
023900110927     c                   eval      desctp = §1Pdes
024000110921      * decodifico il flag di esposizione tar.particolare in tariffa
024100110921     c                   SELECT
024200110921     c                   WHEN      vidstp = 'S'
024300110927     c                   eval      desstp = 'Con tariffa presente'
024400110921     c                   WHEN      vidstp = 'V'
024500110927     c                   eval      desstp = 'Con tariffa presente, -
024600110921     c                             valorizzata'
024700110921     c                   WHEN      vidstp = 'A'
024800110927     c                   eval      desstp = 'Con tariffa presente, -
024900110921     c                             azzerata'
025000110921     c                   WHEN      vidstp = 'E'
025100110927     c                   eval      desstp = 'Senza tariffa'
025200110921     c                   OTHER
025300110921     c                   clear                   desstp
025400110921     c                   ENDSL
025500110921
025600940903     C                   EXCEPT    RICHI
025700900831     C*
025800950802     C                   CLEAR                   FIL
025900040930     c                   move      simfel        wsimfel           3
026000950802     C* S O N O   I N   F I L I A L E
026100950802     C     SIMFEL        IFGT      0
026200950802     C     VIDFIL        IFNE      *ZERO
026300950802     C     VIDFIL        ANDNE     999
026400950802     C                   MOVE      VIDFIL        FIL(1)
026500950802     C                   ELSE
026600040930      * Reperimento dei P.O. gestibili dall'utente
026700040930     c                   clear                   TRUL31DS
026800040930     c                   eval      I31abi = prmabi
026900040930     c                   eval      I31cdi = prmcdi
027000040930     c                   eval      I31car = prmcar
027100040930     c                   eval      I31cpo = prmcpo
027200040930     c                   call      'TRUL31R'
027300040930     c                   parm                    KPJBA
027400040930     c                   parm                    TRUL31DS
027500040930if  2c                   if        O31pog > *zeros
027600040930     c                   movea     O31pog        fig
027700040930e   2c                   endif
027800040930     c                   Eval      x = 0
027900040930     c                   Eval      w = 1
028000040930do  2c                   Dow       fig(w) > *Zeros
028100040930if  3c                   If        vidfil = 999
028200040930if  4c                   If        fig(w) <> wsimfel
028300040930     c                   add       1             x
028400040930     c                   Move      fig(w)        fil(x)
028500040930e   4c                   endif
028600040930   x3c                   Else
028700040930     c                   Move      fig(w)        fil(w)
028800040930e   3c                   Endif
028900040930     c                   add       1             w
029000040930     c                   EndDo
029100950802     C                   ENDIF
029200950802     C*
029300950802     C                   ELSE
029400950802     C* S O N O   I N   S E D E
029500950802     C     VIDFIL        IFNE      *ZERO
029600900831     C                   MOVE      VIDFIL        FIL(1)
029700020415     C                   ELSE
029800020415     c                   move      046           fil(1)
029900950802     C                   ENDIF
030000950802     C                   ENDIF
030100900831     C*
030200910927     C                   MOVE      *ZERO         X
030300910927     C                   MOVE      *ZERO         W                 3 0
030400900901     C                   MOVE      *ZERO         §KSCI
030500900831     C     VIDSC2        COMP      'N'                                636162
030600900831     C     LOOP1         TAG
030700900901     C                   ADD       1             W
030800900901     C     FIL(W)        CABEQ     *ZERO         FINE
030900900901     C                   MOVEL     FIL(W)        §KSCI
031000020415     c                   if        simfel > 0  or
031100020415     c                             (simfel = 0 and vidfil > 0)
031200900831     C   10CHIAC1        SETLL     CNACO04L
031300900831     C   11CHIACO        SETLL     CNACO00F
031400020415     c                   else
031500020415     c   10chiac1        setll     cnaco04l
031600020415     c   11chiac1        setll     cnaco00f
031700020415     c                   endif
031800900831     C*
031900020415     c                   if        simfel > 0  or
032000020415     c                             (simfel = 0 and vidfil > 0)
032100910927     C     §CTRC         IFGT      *ZERO
032200910927     C                   EXCEPT    TOTFIL
032300910927     C                   MOVE      *ZERO         §CTRC             5 0
032400910927     C                   END
032500020415     c                   endif
032600910927     C*
032700900901     C     FIL(W)        CHAIN     AZORG                              54
032800980122     C                   SETON                                        OF
032900000000     C*
033000900831     C     LOOP2         TAG
033100900901     C                   SETOFF                                       58
033200020415     c                   if        simfel > 0  or
033300020415     c                             (simfel = 0 and vidfil > 0)
033400900831     C   10              READ      CNACO04L                               58
033500900831     C   11              READ      CNACO00F                               58
033600020415     c                   else
033700020415     c   10              read      cnaco04l                               58
033800020415     c   11              read      cnaco00f                               58
033900020415     c                   endif
034000110922     C  N58ACOKCC        COMP      DUTkci                             5858
034100900831     C   58              GOTO      LOOP1
034200901023     C     ACOFLG        CABEQ     '*'           LOOP2
034300900831     C                   MOVEL     ACOKSC        §FIL              3 0
034400020415     c                   if        simfel > 0 or
034500020415     c                             (simfel = 0 and vidfil > 0)
034600900901     C   11§FIL          CABNE     FIL(W)        LOOP1
034700900901     C   10§FIL          CABNE     FIL(W)        LOOP2
034800020415     c                   endif
034900020417     c                   if        simfel = 0 and vidfil = 0
035000020417     c                             and acoksc = 0
035100020417     c                   goto      loop2
035200020417     c                   endif
035300900831     C*
035400900831     C* SCARTO A SECONDA DELLA SCELTA SU ABILITAZIONE CLIENTE
035500130315     C     ACOABL        COMP      ' '                                6060
035600900831     C  N60
035700900831     CAN 61
035800900901     COR 60
035900900901     CAN 62              GOTO      LOOP2
036000900831     C*
036100900831     C     CHIIND        CHAIN     CNIND000                           70
036200900831     C     CHIIND        CHAIN     CNCLP000                           71
036300941122     C     ACOKSC        CHAIN     FNCLS000                           72
036400920617     C                   MOVE      *BLANK        FLG
036500920617     C  N72              MOVEA     CLSFLO        FLG
036600911011     C*
036700911011     C* PARZIALIZZAZIONE X COD.IMPORTANZA CLIENTE
036800911011     C     VIDCIC        IFNE      *BLANK
036900911011     C     VIDCIC        CABNE     CLPCLV        LOOP2
037000911011     C                   END
037100050617     C* PARZIALIZZAZIONE X tipo pagamento C/assegni
037200050617     C     VIDtpc        IFNE      'N'
037300050617     C     VIDtpc        CABNE     CLPfpc        LOOP2
037400050617     C                   END
037500011211     C*
037600011211     C* PARZIALIZZAZIONE X COD.AGENTE
037700011211     C     VIDAGE        IFNE      *BLANK
037800011211     C                   MOVE      CLPAGE        AGENTE            7
037900011211     C     VIDAGE        CABNE     AGENTE        LOOP2
038000011211     C                   END
038100920613     C*
038200920613     C* PARZIALIZZAZIONE X COD.CATEGORIA SEAT
038300920613     C     VIDSNC        IFEQ      'S'
038400111006     C**!!!VIDSCT        IFNE      99999
038500920613     C     VIDSCT        CABNE     ACOITC        LOOP2
038600111006     C**!!!              END
038700111006     C**!!!VIDSCT        IFEQ      99999
038800111006     C**!!!ACOITC        CABEQ     0             LOOP2
038900111006     C**!!!              END
039000920613     C                   END
039100121227      * Parzializzazione per Tassazione POST Fatturazione
039200121227     c                   IF        VIDtpf = 'S' and
039300121227     c                             %subst(CLSflo:3:1) <> *blanks
039400121227     c                   goto      loop2
039500121227     c                   ENDIF
039600121227     c                   IF        VIDtpf = 'E' and
039700121227     c                             %subst(CLSflo:3:1) <> 'N'
039800121227     c                   goto      loop2
039900121227     c                   ENDIF
040000001204      *
040100001204      * PARZIALIZZAZIONE X NOTE CLIENTI
040200001204     C     VINOTE        IFNE      'N'
040300001204     C                   MOVEL     ACOKCC        KNK1
040400001204     C                   MOVE      ACOKSC        KNK1
040500001204      * PARZIALIZZA PER TIPO NOTA
040600001204     C     VITNOT        IFNE      *BLANKS
040700001204     C                   MOVE      VITNOT        KTNT
040800010130     C                   SETON                                        84
040900010130     C     KTFN          SETLL     TFNTC01L
041000010130     C                   DO        *HIVAL
041100010130     C     KTFN          READE     TFNTC01L                               85
041200010130     C   85              LEAVE
041300010130     C     NTCFLT        IFNE      'A'
041400010130     C                   SETOFF                                       84
041500010131     C                   LEAVE
041600010130     C                   ENDIF
041700010130     C                   ENDDO
041800001204     C     *IN84         IFEQ      *OFF
041900001204     C     VINOTE        ANDNE     'S'
042000001204     C                   GOTO      LOOP2
042100001204     C                   ENDIF
042200001204     C     *IN84         IFEQ      *ON
042300001204     C     VINOTE        ANDNE     'E'
042400001204     C                   GOTO      LOOP2
042500001204     C                   ENDIF
042600001204     C                   ENDIF
042700001204      * PARZIALIZZA PER TUTTI I TIPI NOTA
042800001204     C     VITNOT        IFEQ      *BLANKS
042900010130     C                   SETON                                        84
043000010130     C     KTFN1         SETLL     TFNTC01L
043100010130     C                   DO        *HIVAL
043200010130     C     KTFN1         READE     TFNTC01L                               85
043300010130     C   85              LEAVE
043400010130     C     NTCFLT        IFNE      'A'
043500010130     C                   SETOFF                                       84
043600010131     C                   LEAVE
043700010130     C                   ENDIF
043800010130     C                   ENDDO
043900001204     C     *IN84         IFEQ      *OFF
044000001204     C     VINOTE        ANDNE     'S'
044100001204     C                   GOTO      LOOP2
044200001204     C                   ENDIF
044300001204     C     *IN84         IFEQ      *ON
044400001204     C     VINOTE        ANDNE     'E'
044500001204     C                   GOTO      LOOP2
044600001204     C                   ENDIF
044700001204     C                   ENDIF
044800001204     C                   ENDIF
044900051128      *
045000051128      * PARZIALIZZAZIONE X LUOGO
045100051128     C     VIluog        IFNE      'N'
045200051128     C                   MOVEL     'L'           Kfls
045300051128     C                   z-add     ACOKSC        Kcli
045400051128      * PARZIALIZZA PER cod luogo
045500051128     C     VITluo        IFNE      *BLANKS
045600051128     C                   MOVE      VITluo        Kcod
045700051128     C                   SETON                                        84
045800051129     C     Kspe          SETLL     fnspe03L
045900051128     C                   DO        *HIVAL
046000051129     C     Kspe          READE     fnspe03L                               85
046100051128     C   85              LEAVE
046200051128     C                   SETOFF                                       84
046300051128     C                   LEAVE
046400051128     C                   ENDDO
046500051128     C     *IN84         IFEQ      *OFF
046600051128     C     VIluog        ANDNE     'S'
046700051128     C                   GOTO      LOOP2
046800051128     C                   ENDIF
046900051128     C     *IN84         IFEQ      *ON
047000051128     C     VIluog        ANDNE     'E'
047100051128     C                   GOTO      LOOP2
047200051128     C                   ENDIF
047300051128     C                   ENDIF
047400051128      * PARZIALIZZA PER TUTTI I codici luogo
047500051128     C     VITluo        IFEQ      *BLANKS
047600051128     C                   SETON                                        84
047700051129     C     kspe1         SETLL     fnspe03L
047800051128     C                   DO        *HIVAL
047900051129     C     kspe1         READE     fnspe03l                               85
048000051128     C   85              LEAVE
048100051128     C                   SETOFF                                       84
048200051128     C                   LEAVE
048300051128     C                   ENDDO
048400051128     C     *IN84         IFEQ      *OFF
048500051128     C     VIluog        ANDNE     'S'
048600051128     C                   GOTO      LOOP2
048700051128     C                   ENDIF
048800051128     C     *IN84         IFEQ      *ON
048900051128     C     VIluog        ANDNE     'E'
049000051128     C                   GOTO      LOOP2
049100051128     C                   ENDIF
049200051128     C                   ENDIF
049300051128     C                   ENDIF
049400020307      *
049500020411      *  PARZIALIZZAZIONE PER NETWORK TARIFFE
049600020410  1a C                   IF        VidNtr='S' or VidNtr ='E'
049700020411      *  recupero il network
049800020410     C                   MOVEL     AcoKsc        §Fil
049900020410     C     §Fil          CHAIN     AzOrg01L                           30
050000020411     C                   MOVEL     OrgDe3        OG143
050100020411      *  se su file tariffe non trovo nulla non stampo
050200020410     C                   MOVE      'S'           WNOSEL            1
050300020410     C                   MOVE      ' '           WSISEL            1
050400020315      *
050500020410     C                   MOVE      *blanks       §CTR              3
050600020410     C     AcoKsc        SETGT     TnTam01l
050700020410   2aC                   DO        *hival
050800020410     C     AcoKsc        READPE    Tntam01l                               57
050900020411     C                   IF        *in57 =*on
051000020411     C                   leave
051100020411     C                   ENDIF
051200020410      *
051300020410     C                   MOVE      TAMCTR        ALF3              3
051400020411   3aC                   IF        TamAtb <>'A' and alf3<> §ctr
051500020411     C                   MOVE      TAMCTR        §CTR
051600020410      *  Ho letto almeno una tariffa ..
051700020410     C                   MOVEL     TAMFLO        DSTA01
051800020307      *  SE IL FILTRO SU NETWORK TARIFFE E IMPOSTATO AD 'E'
051900020307      *  VOGLIO SOLO CLIENTI SENZA SERVIZIO IMPOSTATO UGUALE AL CAMPO
052000020307      *  VIDNTW  (DPD, FED, PPT, COR, MES, EEX, EUP, ITA, EST)
052100020307      *
052200020411      *  parto dall'idea di stampare tutto
052300020410     C                   MOVE      ' '           WNOSEL
052400020410   4aC     VIDNTR        IFEQ      'E'
052500000725      *
052600020307      * DPD
052700020307     C     VIDNTW        IFEQ      'DPD'
052800000725     C     §TADPD        IFEQ      'S'
052900020314     C                   MOVE      'S'           WNOSEL
053000000725     C                   LEAVE
053100000725     C                   ENDIF
053200020307     C                   ENDIF
053300000725      *
053400020307      * FED
053500020307     C     VIDNTW        IFEQ      'FED'
053600020307     C     §TAFED        IFEQ      'S'
053700020314     C                   MOVE      'S'           WNOSEL
053800020307     C                   LEAVE
053900020307     C                   ENDIF
054000020307     C                   ENDIF
054100020307      *
054200020307      * PPT
054300020307     C     VIDNTW        IFEQ      'PPT'
054400020307     C     TAMTSP        IFEQ      'P'
054500020314     C                   MOVE      'S'           WNOSEL
054600020307     C                   LEAVE
054700020307     C                   ENDIF
054800020307     C                   ENDIF
054900020307      *
055000020307      * COR
055100020307     C     VIDNTW        IFEQ      'COR'
055200020307     C     TAMFIE        IFEQ      'I'
055300020307     C     §OGNTW        ANDEQ     'COR'
055400020410     C     §TADPD        ANDNE     'S'
055500020314     C                   MOVE      'S'           WNOSEL
055600020307     C                   LEAVE
055700020307     C                   ENDIF
055800020307     C                   ENDIF
055900020307      *
056000020307      * MES
056100020307     C     VIDNTW        IFEQ      'MES'
056200020307     C     TAMFIE        IFEQ      'I'
056300020307     C     §OGNTW        ANDEQ     'MES'
056400020410     C     §TADPD        ANDNE     'S'
056500020314     C                   MOVE      'S'           WNOSEL
056600020307     C                   LEAVE
056700020307     C                   ENDIF
056800020307     C                   ENDIF
056900020307      *
057000020307      * EEX
057100020307     C     VIDNTW        IFEQ      'EEX'
057200020307     C     TAMFIE        IFEQ      'E'
057300020410     C     §TAFED        ANDNE     'S'
057400020411      * Includiamo i clienti Italia che spediscono
057500020411      * all'estero con EuroExpress
057600020411     C     §OGNTW        IFEQ      'EEX'
057700020411     C     §OGNTW        OREQ      'COR'
057800020411     C     §OGNTW        OREQ      'MES'
057900020314     C                   MOVE      'S'           WNOSEL
058000020307     C                   LEAVE
058100020307     C                   ENDIF
058200020307     C                   ENDIF
058300020411     C                   ENDIF
058400020307      *
058500020307      * EUP
058600020307     C     VIDNTW        IFEQ      'EUP'
058700020307     C     TAMFIE        IFEQ      'E'
058800020307     C     §OGNTW        ANDEQ     'EUP'
058900020410     C     §TAFED        ANDNE     'S'
059000020314     C                   MOVE      'S'           WNOSEL
059100020307     C                   LEAVE
059200020307     C                   ENDIF
059300020307     C                   ENDIF
059400020307      * ITA
059500020307     C     VIDNTW        IFEQ      'ITA'
059600020307     C     TAMFIE        IFEQ      'I'
059700020307     C     §TADPD        ANDNE     'S'
059800020314     C                   MOVE      'S'           WNOSEL
059900020307     C                   LEAVE
060000020307     C                   ENDIF
060100020307     C                   ENDIF
060200020307      * EST
060300020307     C     VIDNTW        IFEQ      'EST'
060400020411     C     TAMFIE        IFNE      'I'
060500020307     C     §TADPD        OREQ      'S'
060600020314     C                   MOVE      'S'           WNOSEL
060700020307     C                   LEAVE
060800020307     C                   ENDIF
060900020307     C                   ENDIF
061000020410  4- C                   ENDIF
061100020307      *
061200020307      *  SE IL FILTRO SU NETWORK TARIFFE E IMPOSTATO AD 'S'
061300020307      *  VOGLIO SOLO CLIENTI CON SERVIZIO IMPOSTATO UGUALE AL CAMPO
061400020307      *  VIDNTW  (DPD, FED, PPT, COR, MES, EEX, EUP, ITA, EST)
061500020307      *
061600020411      *  parto dall'idea di non stampere niente
061700020410     C                   MOVE      ' '           WSISEL
061800020410   4aC     VIDNTR        IFEQ      'S'
061900020307      *
062000020307      * DPD
062100020307     C     VIDNTW        IFEQ      'DPD'
062200020307     C     §TADPD        IFEQ      'S'
062300020314     C                   MOVE      'S'           WSISEL
062400020307     C                   LEAVE
062500020307     C                   ENDIF
062600020307     C                   ENDIF
062700020307      *
062800020307      * FED
062900020307     C     VIDNTW        IFEQ      'FED'
063000020307     C     §TAFED        IFEQ      'S'
063100020314     C                   MOVE      'S'           WSISEL
063200020307     C                   LEAVE
063300020307     C                   ENDIF
063400020307     C                   ENDIF
063500020307      *
063600020307      * PPT
063700020307     C     VIDNTW        IFEQ      'PPT'
063800020307     C     TAMTSP        IFEQ      'P'
063900020314     C                   MOVE      'S'           WSISEL
064000020307     C                   LEAVE
064100020307     C                   ENDIF
064200020307     C                   ENDIF
064300020307      *
064400020307      * COR
064500020307     C     VIDNTW        IFEQ      'COR'
064600020307     C     TAMFIE        IFEQ      'I'
064700020307     C     §OGNTW        ANDEQ     'COR'
064800020410     C     §TADPD        ANDNE     'S'
064900020314     C                   MOVE      'S'           WSISEL
065000020307     C                   LEAVE
065100020307     C                   ENDIF
065200020307     C                   ENDIF
065300020307      *
065400020307      * MES
065500020307     C     VIDNTW        IFEQ      'MES'
065600020307     C     TAMFIE        IFEQ      'I'
065700020307     C     §OGNTW        ANDEQ     'MES'
065800020410     C     §TADPD        ANDNE     'S'
065900020314     C                   MOVE      'S'           WSISEL
066000020307     C                   LEAVE
066100020307     C                   ENDIF
066200020307     C                   ENDIF
066300020307      *
066400020307      * EEX
066500020307     C     VIDNTW        IFEQ      'EEX'
066600020307     C     TAMFIE        IFEQ      'E'
066700020411     C     §TAFED        ANDNE     'S'
066800020411      * Includiamo i clienti Italia che spediscono
066900020411      * all'estero con EuroExpress
067000020411     C     §OGNTW        IFEQ      'EEX'
067100020411     C     §OGNTW        OREQ      'COR'
067200020411     C     §OGNTW        OREQ      'MES'
067300020314     C                   MOVE      'S'           WSISEL
067400020307     C                   LEAVE
067500020307     C                   ENDIF
067600020307     C                   ENDIF
067700020411     C                   ENDIF
067800020307      *
067900020307      * EUP
068000020307     C     VIDNTW        IFEQ      'EUP'
068100020307     C     TAMFIE        IFEQ      'E'
068200020307     C     §OGNTW        ANDEQ     'EUP'
068300020410     C     §TAFED        ANDNE     'S'
068400020314     C                   MOVE      'S'           WSISEL
068500020307     C                   LEAVE
068600020307     C                   ENDIF
068700020307     C                   ENDIF
068800020307      * ITA
068900020307     C     VIDNTW        IFEQ      'ITA'
069000020307     C     TAMFIE        IFEQ      'I'
069100020307     C     §TADPD        ANDNE     'S'
069200020314     C                   MOVE      'S'           WSISEL
069300020307     C                   LEAVE
069400020307     C                   ENDIF
069500020307     C                   ENDIF
069600020307      * EST
069700020307     C     VIDNTW        IFEQ      'EST'
069800020411     C     TAMFIE        IFNE      'I'
069900020307     C     §TADPD        OREQ      'S'
070000020314     C                   MOVE      'S'           WSISEL
070100020307     C                   LEAVE
070200020307     C                   ENDIF
070300020307     C                   ENDIF
070400020410  4- C                   ENDIF
070500020410      *
070600020410  3- C                   ENDIF
070700020410  2- C                   ENDDO
070800020307      *
070900020315     C     VIDNTR        IFEQ      'S'
071000020315     C     WSISEL        ANDNE     'S'
071100020307     C                   GOTO      LOOP2
071200020307     C                   ENDIF
071300020315      *
071400020315     C     VIDNTR        IFEQ      'E'
071500020315     C     WNOSEL        ANDNE     ' '
071600020315     C                   GOTO      LOOP2
071700020315     C                   ENDIF
071800020410   1-C                   ENDIF
071900020410      *
072000020410      *  SELEZIONI SU CLIENTE OPERATIVO 'N'; 'E'
072100020410   1aC                   IF        VidCno = 'N' or Vidcno ='E'
072200020410      *
072300020410      *  Parto dal presupposto che il cliente sia operativo ...
072400020405     C                   move      'S'           $CliOpe           1
072500020430      *  Imposto periodo di richiesta clienti non operativi
072600020430     C                   if        VIDDAA > 60
072700020430     C     1900          add       VIDDAA        WAnnoI            4 0
072800020430     C                   else
072900020430     C     2000          add       VIDDAA        WAnnoI
073000020430     C                   endif
073100020430     C                   if        VIDAAA > 60
073200020430     C     1900          add       VIDAAA        WAnnoF            4 0
073300020430     C                   else
073400020430     C     2000          add       VIDAAA        WAnnoF
073500020430     C                   endif
073600020430     C                   z-add     VIDDMM        WMeseI            2 0
073700020430      *
073800020430     C                   z-add     *zeros        DataFine          6 0
073900020430     C                   eval      DataFine   = (WAnnoF * 100) + VIDAMM
074000020405      *
074100020410      *  il cliente non è operativo se non ha il blocco e se non
074200020429      *  ha fatto spedizioni nel periodo richiesto.
074300130315   2aC     AcoAbl        ifeq      *blanks
074400020405      *
074500020405      *  Ciclo sulle spedizioni
074600020408     C                   move      'N'           $SiSpe            1
074700020405      *
074800020405     C     Ksdc          setll     SisdC01l
074900020405     C     AcoKsc        reade     SisdC01l                               25
075000020405      * leggo fino a fine file, o fino a cambio cliente , o fino
075100020405      * a 12 mesi dopo
075200020410   3aC     *in25         DOWEQ     *off
075300020429      *
075400020405      * Controllo la data
075500020405     C                   z-add     *zeros        DataLetta         6 0
075600020405     C                   eval      DataLetta  = (SdcAnn * 100) + SdcMes
075700020429   4aC     DataLetta     ifle      DataFine
075800020405      *
075900020410   5aC     SdcRbl        ifeq      'N'
076000020405      *
076100020430      *  Il cliente ha avuto almeno una spedizione valida
076200020430      *  nel periodo richiesto
076300020405     C                   move      'S'           $SiSpe
076400020405     C                   LEAVE
076500020410   5-C                   endif
076600020405      *
076700020405     C     Acoksc        reade     SisdC01l                               25
076800020410   4eC                   ELSE
076900020405     C                   leave
077000020410   4-C                   ENDIF
077100020410   3-C                   ENDDO
077200020405      *
077300020410   3 C     $SiSpe        ifeq      'N'
077400020405     C                   move      'N'           $CliOpe
077500020410   3-C                   endif
077600020410   2-C                   endif
077700020405      *
077800020405      *
077900020410   2 C     VidCno        ifeq      'E'
078000020405     C     $CliOpe       andeq     'S'
078100020405     C                   GOTO      LOOP2
078200020410   2-C                   endif
078300020405      *
078400020410   2 C     VidCno        ifeq      'N'
078500020405     C     $CliOpe       andeq     'N'
078600020405     C                   GOTO      LOOP2
078700020410   2-C                   endif
078800020410   1-C                   ENDIF
078900020315      *
079000900831     C*  CARICA LE PRIME 5 TARIFFE NON ANNULLATE
079100900831     C                   EXSR      CARTAM
079200900831     C*
079300920613     C* SE HO CHIESTO PARZIALIZZAZIONI SULLE TARIFFE
079400920613     C* E NON HO CARICATO TARIFFE NON STAMPO
079500971125     C     VIDSC3        IFNE      'N'
079600040119     C     VIDIST        ORNE      'N'
079700971125     C     VIDSAC        ORNE      'N'
079800060310     c     vidracb       orne      'N'
079900060310     c     vidbase       orne      'N'
080000110922     c     VIDctp        orne      *blanks
080100051213     C     VIDblk        ORNE      'N'
080200110921     C     VIDctp        ORNE      ' '
080300950103     C     VIDTSP        ORNE      ' '
080400960424     C     VIDTTR        ORNE      ' '
080500960424     C     VIDCPA        ORNE      '  '
080600970320     C     VIDBAM        ORNE      ' '
080700040601     c     vidbva        orne      *Blanks
080800060228     c     vidtap        orne      *blanks
080900911001     C     X             CABEQ     *ZERO         LOOP2
081000040119     C                   ENDIF
081100110926      * se richiesta la tariffa particolare fuel pulisco la sk tarpar
081200110926      * tanto la stampa del fuel è a parte.
081300110926     c                   If        VIDctp = VIDfuel
081400110926     c                   clear                   tarpar
081500110926     c                   endif
081600110926
081700911001     C*
081800920613     C* DECODIFICA COD PAGAM.
081900920613     C                   MOVEL     *BLANKS       §DEPAG
082000920613     C  N70              DO
082100920613     C                   MOVE      'FA'          COD
082200920613     C                   MOVEL     *BLANKS       KEY
082300920613     C                   MOVE      INDCDP        §CDPAG
082400000322     C                   MOVE      '1'           §CDLIN
082500920613     C     KTAB1         CHAIN     TABEL                              30
082600921125     C  N30              MOVEL     TBLUNI        §DEPAG           26
082700920613     C                   END
082800920613     C* DECODIFICA CATEG.SEAT
082900921125     C                   SETOFF                                       464748
083000921125     C                   MOVEL     *BLANKS       DESSCT           26
083100920613     C     ACOITC        IFGT      *ZERO
083200921125     C                   SETON                                        47
083300920613     C                   MOVE      '1L'          COD
083400920613     C                   MOVEL     *BLANKS       KEY2
083500920613     C                   Z-ADD     ACOITC        §COITC            5 0
083600920613     C                   MOVEL     §COITC        KEY2
083700920613     C     KTAB          CHAIN     TABEL                              84
083800920613     C  N84              MOVEL     TBLUNI        DESSCT
083900920613     C                   END
084000920613     C*
084100980122     C     *INOF         IFEQ      *ON
084200020307     C     VIDTPS        ANDEQ     'A'
084300980122     C                   EXCEPT    TESTA
084400980122     C                   SETOFF                                       OF
084500980122     C                   ENDIF
084600020307     C     *INOF         IFEQ      *ON
084700020307     C     VIDTPS        ANDEQ     'S'
084800020307     C                   EXCEPT    TESTA1
084900020307     C                   SETOFF                                       OF
085000020307     C                   ENDIF
085100980122     C**
085200900901     C*
085300080211     c                   clear                   wbanca
085400080211     c                   if        not *in70
085500080211     c                   if        indabi <> 99999 or indcab <> 99999
085600080211     c                   exsr      sr_desbanca
085700080211     c                   endif
085800080211     c                   endif
085900921125     C*
086000921125     C* SE CI SONO ENTRAMBI I CODICI ABI E CAB LI SEPARO CON
086100921125     C*    UN TRATTINO
086200921125     C  N70INDABI        IFGT      0
086300921125     C     INDCAB        ANDGT     0
086400921125     C                   SETON                                        46
086500921125     C                   END
086600921125     C*
086700921125     C  N71CLPABI        IFGT      0
086800921125     C     CLPCAB        ANDGT     0
086900921125     C                   SETON                                        48
087000921125     C                   END
087100050617     c
087200050617     c* Decodifica tipo pagamento C/A
087300050617     C                   MOVEL     *BLANKS       desfpc           16
087400050617     C                   if        not *in71
087500050617     C                   MOVE      '4U'          COD
087600050617     C                   MOVEL     *BLANKS       KEY
087700050617     C                   MOVEL     clpfpc        key
087800050617     C     KTAB1         CHAIN     TABEL                              30
087900050617     c  n30              eval      desfpc='-' + %subst(tbluni:1:15)
088000050617     c
088100050617     c                   if        clpfpc=' '
088200050617     c                   eval      stafpc='_'
088300050617     c                   else
088400050617     c                   eval      stafpc=clpfpc
088500050617     c                   endif
088600050617     c
088700050617     C                   END
088800130321
088900130321      * se blocco cliente "7" devo stampare "8"
089000130321     c                   IF        ACOabl = '7'
089100130321     c                   eval      ACOABL = '8'
089200130321     c                   ENDIF
089300921125     C*
089400910927     C                   ADD       1             §CTRC             5 0
089500910927     C                   ADD       1             §CTRT             5 0
089600020307     C     VIDTPS        IFEQ      'A'
089700931228     C                   EXCEPT    DETT
089800020307     C                   ENDIF
089900020307     C     VIDTPS        IFEQ      'S'
090000020312      * Recupero descrizione cd. commerciale
090100020312     C                   clear                   ClpDesP
090200020312     C     CLPAGE        IFGT      *ZERO
090300020312     C     *in71         andeq     *off
090400130802     C     CLPage        CHAIN     AZCMM000
090500130802     C                   if        %found(AZCMM01L)
090600130802     C                   movel     CMMdes        ClpDesP          15
090700130802     c                   endif
090800020312     C                   END
090900020307     C                   EXCEPT    DETT1
091000020307     C                   ENDIF
091100001204      * STAMPA NOTE
091200001204     C     VISTPN        IFEQ      'SI'
091300001204     C                   EXSR      SRNOTE
091400001204     C                   ENDIF
091500900831     C*
091600900831     C                   GOTO      LOOP2
091700000000     C     FINE          TAG
091800950717     C*
091900950717     C* STAMPO TOTALE CLIENTI DELL'ULTIMA FILIALE LETTA
092000020415     c                   if        simfel > 0  or
092100020415     c                             (simfel = 0 and vidfil > 0)
092200950717     C     §CTRC         IFGT      *ZERO
092300950717     C                   EXCEPT    TOTFIL
092400950717     C                   END
092500020415     c                   endif
092600950717     C*
092700950717     C* STAMPO TOTALE CLIENTI FINALE
092800910927     C     §CTRT         IFGT      *ZERO
092900910927     C                   EXCEPT    TOTFIN
093000910927     C                   END
093100950717     C*
093200000000     C                   SETON                                        LR
093300080211      *-------------------------------------------------------------------------
093400080211      *  Creo la descrizione della banca di appoggio in base ai codici ABI/CAB
093500080211      *-------------------------------------------------------------------------
093600080211     c     sr_desbanca   begsr
093700080211
093800080211     c     kcnabi        chain     cnabi00f
093900080211     c                   if        not %found(cnabi00f) or abiann = '*'
094000080211     c                   leavesr
094100080211     c                   endif
094200080211
094300080211     c                   eval      wbanca = abiist
094400080211
094500080211     c                   endsr
094600900831     C/SPACE
094700950717     C*--- INZSR INIZIALE --------------------------------------------*
094800950717     C     *INZSR        BEGSR
094900110922
095000110922     C     *ENTRY        PLIST
095100110922     C                   PARM                    KPJBA
095200110922
095300110922     C                   MOVEL     KPJBU         PARAM
095400110922
095500110922     c     *dtaara       define    §azute        azuteds
095600110922     c     *dtaara       define    §datiute      ddatiute
095700110922     c                   in(E)     *dtaara
095800110922     c                   If        %error  or RSUT = *blanks
095900110922     c                   Clear                   Tibs34ds
096000110922     c                   Call      'TIBS34R'
096100110922     c                   Parm                    Tibs34ds
096200110922     c                   In        *dtaara
096300110922     c                   EndIf
096400950717     C***
096500950717     C* GIRO DATA DEL GIORNO: LA PRENDO DA TIME
096600950717     C***
096700950717     C                   TIME                    W0140            14 0
096800950717     C                   MOVE      W0140         UDATE8            8 0
096900020429     C                   MOVE      UDate8        UDate4            4 0
097000020429     C                   MOVE      UDate8        UDate6            6 0
097100020405     C                   MOVEL     UDate6        UDate2            2 0
097200020405      *
097300950717     C                   TIME                    HHMMSS            6 0
097400950717     C***
097500950717     C* DEFINIZIONE CAMPI
097600950717     C***
097700950717     C     *LIKE         DEFINE    ACOKSC        §KSCI
097800950717     C     *LIKE         DEFINE    TBLCOD        COD
097900950717     C     *LIKE         DEFINE    TBLKEY        KEY2
098000001204      *****
098100001204     C     *LIKE         DEFINE    NTCAPL        KAPL
098200001204     C     *LIKE         DEFINE    NTCNK1        KNK1
098300001204     C     *LIKE         DEFINE    NTCNK2        KNK2
098400001204     C     *LIKE         DEFINE    NTCTNT        KTNT
098500051128     C     *LIKE         DEFINE    spefls        kfls
098600051128     C     *LIKE         DEFINE    specli        kcli
098700051128     C     *LIKE         DEFINE    specod        kcod
098800950717     C***
098900950717     C* KLIST
099000950717     C***
099100950717     C* ACCESSO TABEL00F
099200950717     C     KTAB          KLIST
099300950717     C                   KFLD                    CODUT
099400950717     C                   KFLD                    COD
099500950717     C                   KFLD                    KEY2
099600950717     C     KTAB1         KLIST
099700950717     C                   KFLD                    CODUT
099800950717     C                   KFLD                    COD
099900950717     C                   KFLD                    KEY
100000110922     c     ktab2         KLIST
100100110922     c                   KFLD                    CODUT
100200110922     c                   KFLD                    COD
100300950717     C* ACCESSO CNACO04L
100400950717     C     CHIAC1        KLIST
100500950717     C                   KFLD                    CODUT
100600110922     C                   KFLD                    DUTKCI
100700950717     C* ACCESSO CNACO00F
100800950717     C     CHIACO        KLIST
100900950717     C                   KFLD                    CODUT
101000110922     C                   KFLD                    DUTKCI
101100950717     C                   KFLD                    §KSCI
101200950717     C* ACCESSO CNIND00F/CNCLP00F
101300950717     C     CHIIND        KLIST
101400950717     C                   KFLD                    CODUT
101500950717     C                   KFLD                    ACOKCC
101600950717     C                   KFLD                    ACOKSC
101700990714     C* ACCESSO TITPT01L
101800950717     C     KTPT          KLIST
101900950717     C                   KFLD                    TAMKSC
102000950717     C                   KFLD                    TAMCTR
102100950717     C                   KFLD                    TAMPRG
102200990714     C                   KFLD                    FTC               2
102300990714     C* ACCESSO TITGC01L
102400950717     C     KTGC          KLIST
102500950717     C                   KFLD                    TAMKSC
102600950717     C                   KFLD                    TAMCTR
102700950717     C                   KFLD                    TAMPRG
102800001204     C* ACCESSO TFNTC01L
102900001204     C     KTFN          KLIST
103000001204     C                   KFLD                    KAPL
103100001204     C                   KFLD                    KNK1
103200001204     C                   KFLD                    KNK2
103300001204     C                   KFLD                    KTNT
103400001204     C     KTFN1         KLIST
103500001204     C                   KFLD                    KAPL
103600001204     C                   KFLD                    KNK1
103700001204     C                   KFLD                    KNK2
103800001204     C                   MOVEL     'C'           KAPL
103900001204     C                   MOVEL     *BLANKS       KNK2
104000051129     C* ACCESSO FNSPE03L
104100051128     C     Kspe          KLIST
104200051128     C                   KFLD                    Kfls
104300051128     C                   KFLD                    Kcli
104400051128     C                   KFLD                    Kcod
104500051128     C     Kspe1         KLIST
104600051128     C                   KFLD                    Kfls
104700051128     C                   KFLD                    Kcli
104800020405      *
104900020405     C     KSDC          KLIST
105000020405     C                   KFLD                    AcoKsc
105100020405     C                   KFLD                    WAnnoI
105200020405     C                   KFLD                    WMeseI
105300080211
105400080211     c     kCnabi        Klist
105500080211     c                   Kfld                    indabi
105600080211     c                   Kfld                    indcab
105700001204      *
105800950717     C                   ENDSR
105900110922      *---------------------------------------------------------------*
106000110922      * Carico in sk la tabella delle tariffe particolari             *
106100110922      *---------------------------------------------------------------*
106200110922     c     Car1P         BEGSR
106300110922
106400110922     c                   eval      cod = '1P'
106500110922     c                   clear                   xx
106600110922     c     ktab2         SETLL     TABEL00F
106700110922     c     ktab2         READE     TABEL00F
106800110922     c                   DOW       not %eof(TABEL00F)
106900110922
107000110922     c                   IF        TBLflg = *blanks
107100110922     c                   eval      ds1P = TBLuni
107200110922      * non carico le tariffe assicurazione x conto
107300110922     c                   IF        §1Pasc <> 'S'
107400110922     c                   add       1             xx
107500110922     c                   eval      $1P(xx) = tblkey
107600110922     c                   eval      $tco(xx) = §1Ptco
107700110922     c                   eval      $fie(xx) = §1Pfie
107800110922     c                   eval      $fdp(xx) = §1Pfdp
107900110922     c                   eval      $ffe(xx) = §1Pffe
108000110922     c                   ENDIF
108100110922     c                   ENDIF
108200110922     c
108300110922     c     ktab2         READE     TABEL00F
108400110922     c                   ENDDO
108500110922
108600110922     c                   ENDSR
108700110922
108800950802     C*
108900950717     C*--- CARICAMENTO SCHIERA TARIFFE -------------------------------*
109000900831     C     CARTAM        BEGSR
109100910506     C                   SETOFF                                       303433
109200910904     C                   SETOFF                                       505153
109300000725     C                   SETOFF                                       555657
109400900831     C                   MOVE      *ZERO         X
109500911014     C                   MOVE      *ZERO         Y                 3 0
109600901008     C                   MOVE      *ZERO         KPM
109700910904     C                   MOVE      *BLANK        TIN
109800910904     C                   MOVE      *BLANK        DAR
109900900901     C                   MOVE      *BLANK        CAR
110000101015     C                   MOVE      *zeros        is1
110100040119     C                   MOVE      *zeros        is2
110200920624     C                   MOVE      *ZERO         AC1
110300930519     C                   MOVE      *BLANK        FV1
110400950208     C                   MOVE      *BLANK        TCM
110500970320     C                   MOVE      *BLANK        BAM
110600040601     c                   Move      *Blanks       bva
110700930415     C                   MOVE      *BLANK        §CTR              3
110800060310     c                   move      *blanks       tap
110900060313     c                   move      *blanks       acbase
111000070129     c                   move      *blanks       fuel
111100110921     c                   move      *blanks       tarpar
111200920624     C*
111300950717     C     ACOKSC        SETGT     TNTAM000
111400040527    1C                   DO        *HIVAL
111500040527     C     acoksc        READPe    TNTAM000                               30
111600040527    2C                   IF        not *in30
111700900901     C*
111800930415     C                   MOVEL     TAMCTR        ALF3              3
111900040527    3C     TAMATB        IFNE      'A'
112000930415     C     ALF3          ANDNE     §CTR
112100900831     C                   MOVE      TAMCTR        §CTR
112200060310     C** PER 33+   36+37+38+   40+41+42+64+65+66+   CARICO TARIFFA
112300000725     C* PARAMETRO SERVIZIO DPD
112400040527     C                   setoff                                       57
112500020315     C                   MOVEL     tamflo        DSTA01
112600040527     C* SE C'E' UNA PARZIAlizzazione sulle tariffe, riparzializzo anche
112700040527     c*  il ntw, se inclusione
112800040527    4c                   if        vidntr='S'
112900040527    5C     VIDSC3        IFNE      'N'
113000040527     C     VIDIST        ORNE      'N'
113100040527     C     VIDSAC        ORNE      'N'
113200060310     c     vidracb       orne      'N'
113300060310     c     vidbase       orne      'N'
113400110922     c     Vidctp        orne      *blanks
113500051213     C     VIDblk        ORNE      'N'
113600040527     C     VIDTSP        ORNE      ' '
113700040527     C     VIDTTR        ORNE      ' '
113800040527     C     VIDCPA        ORNE      '  '
113900040527     C     VIDBAM        ORNE      ' '
114000040601     c     vidbva        orne      *blanks
114100060228     c     vidtap        orne      *blanks
114200040527      *
114300040527      *  SE IL FILTRO SU NETWORK TARIFFE E IMPOSTATO AD 'S'
114400040527      *  VOGLIO SOLO CLIENTI CON SERVIZIO IMPOSTATO UGUALE AL CAMPO
114500040527      *  VIDNTW  (DPD, FED, PPT, COR, MES, EEX, EUP, ITA, EST)
114600040527      *
114700040527      *  parto dall'idea di non stampere niente
114800040527     C                   MOVE      ' '           WSISEL
114900040527    6C     VIDNTR        IFEQ      'S'
115000040527      *
115100040527      * DPD
115200040527     C     VIDNTW        IFEQ      'DPD'
115300040527     C     §TADPD        IFEQ      'S'
115400040527     C                   MOVE      'S'           WSISEL
115500040527     C                   ENDIF
115600040527     C                   ENDIF
115700040527      *
115800040527      * FED
115900040527     C     VIDNTW        IFEQ      'FED'
116000040527     C     §TAFED        IFEQ      'S'
116100040527     C                   MOVE      'S'           WSISEL
116200040527     C                   ENDIF
116300040527     C                   ENDIF
116400040527      *
116500040527      * PPT
116600040527     C     VIDNTW        IFEQ      'PPT'
116700040527     C     TAMTSP        IFEQ      'P'
116800040527     C                   MOVE      'S'           WSISEL
116900040527     C                   ENDIF
117000040527     C                   ENDIF
117100040527      *
117200040527      * COR
117300040527     C     VIDNTW        IFEQ      'COR'
117400040527     C     TAMFIE        IFEQ      'I'
117500040527     C     §OGNTW        ANDEQ     'COR'
117600040527     C     §TADPD        ANDNE     'S'
117700040527     C                   MOVE      'S'           WSISEL
117800040527     C                   ENDIF
117900040527     C                   ENDIF
118000040527      *
118100040527      * MES
118200040527     C     VIDNTW        IFEQ      'MES'
118300040527     C     TAMFIE        IFEQ      'I'
118400040527     C     §OGNTW        ANDEQ     'MES'
118500040527     C     §TADPD        ANDNE     'S'
118600040527     C                   MOVE      'S'           WSISEL
118700040527     C                   ENDIF
118800040527     C                   ENDIF
118900040527      *
119000040527      * EEX
119100040527     C     VIDNTW        IFEQ      'EEX'
119200040527     C     TAMFIE        IFEQ      'E'
119300040527     C     §TAFED        ANDNE     'S'
119400040527      * Includiamo i clienti Italia che spediscono
119500040527      * all'estero con EuroExpress
119600040527     C     §OGNTW        IFEQ      'EEX'
119700040527     C     §OGNTW        OREQ      'COR'
119800040527     C     §OGNTW        OREQ      'MES'
119900040527     C                   MOVE      'S'           WSISEL
120000040527     C                   ENDIF
120100040527     C                   ENDIF
120200040527     C                   ENDIF
120300040527      *
120400040527      * EUP
120500040527     C     VIDNTW        IFEQ      'EUP'
120600040527     C     TAMFIE        IFEQ      'E'
120700040527     C     §OGNTW        ANDEQ     'EUP'
120800040527     C     §TAFED        ANDNE     'S'
120900040527     C                   MOVE      'S'           WSISEL
121000040527     C                   ENDIF
121100040527     C                   ENDIF
121200040527      * ITA
121300040527     C     VIDNTW        IFEQ      'ITA'
121400040527     C     TAMFIE        IFEQ      'I'
121500040527     C     §TADPD        ANDNE     'S'
121600040527     C                   MOVE      'S'           WSISEL
121700040527     C                   ENDIF
121800040527     C                   ENDIF
121900040527      * EST
122000040527     C     VIDNTW        IFEQ      'EST'
122100040527     C     TAMFIE        IFNE      'I'
122200040527     C     §TADPD        OREQ      'S'
122300040527     C                   MOVE      'S'           WSISEL
122400040527     C                   ENDIF
122500040527     C                   ENDIF
122600040527      *
122700040527     C     WSISEL        ifne      'S'
122800040527     C                   seton                                        57
122900040527     C                   ENDIF
123000040527      *
123100040527  3 6C                   ENDIF
123200040527  3 5C                   ENDIF
123300040527  3 4C                   ENDIF
123400040527    c
123500920625     C                   SETOFF                                       33
123600911001     C* ESCLUSIONE IN BASE ALLA RICHIESTA O MENO DEL VOLUME
123700911001     C     TAMKPM        COMP      *ZERO                                  34
123800971205     C     VIDSC3        IFEQ      'N'
123900971205     C                   SETON                                        33
124000971205     C                   END
124100930415     C  N34VIDSC3        IFEQ      'S'
124200930415     C                   SETON                                        33
124300911001     C                   END
124400930415     C*
124500940822     C  N34VIDSC3        IFEQ      'C'
124600940822     C     TAMTIN        ANDEQ     'C'
124700940822     C                   SETON                                        33
124800940822     C                   ENDIF
124900940822     C*
125000940822     C  N34VIDSC3        IFEQ      'K'
125100940822     C     TAMTIN        ANDEQ     'K'
125200940822     C                   SETON                                        33
125300940822     C                   ENDIF
125400940822     C*
125500930415     C   34VIDSC3        IFEQ      'E'
125600940822     C     TAMBAP        ANDNE     'A'
125700940822     C     TAMBAP        ANDNE     'B'
125800930415     C                   SETON                                        33
125900911001     C                   END
126000051213     C* ESCLUSIONE IN BASE ALLA RICHIESTA tariffe bloccate
126100051213     c                   SELECT
126200051213      * includo bloccate
126300051213     C                   WHEN      TAMbap = 'B'     and  VIDblk = 'S'
126400051213     C                   eval      *in38 = *on
126500051213      * includo solo no bloccate
126600051213     C                   WHEN      TAMbap <> 'B'    and  VIDblk = 'E'
126700051213     C                   eval      *in38 = *on
126800051213      * no Test
126900051213     C                   WHEN      VIDblk = 'N'
127000051213     C                   eval      *in38 = *on
127100051213      *
127200051213     C                   OTHER
127300051213     C                   eval      *in38 = *off
127400051213     C                   ENDSL
127500970320     C*
127600970320     C* ESCLUSIONE IN BASE ALLA RICHIESTA DELLA CONSEGNA DDT SI/NO/TUTT
127700970320     C                   SETOFF                                       66
127800970320     C     VIDBAM        IFEQ      ' '
127900970320     C                   SETON                                        66
128000970320     C                   ELSE
128100970320     C     VIDBAM        IFEQ      'S'
128200970320     C     TAMBAM        ANDEQ     'S'
128300970320     C                   SETON                                        66
128400970320     C                   ENDIF
128500970320     C     VIDBAM        IFEQ      'N'
128600970320     C     TAMBAM        ANDEQ     'N'
128700970320     C                   SETON                                        66
128800970320     C                   ENDIF
128900970320     C                   ENDIF
129000940822     C*
129100940822     C* ESCLUSIONE IN BASE ALLA RICHIESTA DEL TIPO SERVIZIO
129200940822     C                   SETOFF                                       40
129300940822     C     VIDTSP        IFEQ      ' '
129400940822     C                   SETON                                        40
129500940822     C                   ELSE
129600940822     C     VIDTSP        IFEQ      TAMTSP
129700940822     C                   SETON                                        40
129800940822     C                   ENDIF
129900940822     C                   ENDIF
130000960424     C*
130100960424     C* ESCLUSIONE IN BASE ALLA RICHIESTA DEL TIPO TARIFFA
130200960424     C                   SETOFF                                       64
130300960424     C     VIDTTR        IFEQ      ' '
130400960424     C                   SETON                                        64
130500960424     C                   ELSE
130600960424     C                   MOVEL     TAMCTR        W001              1
130700960424     C     VIDTTR        IFEQ      W001
130800960424     C                   SETON                                        64
130900960424     C                   ENDIF
131000960424     C                   ENDIF
131100040601
131200040601      * ESCLUSIONE IN BASE ALLA RICHIESTA DEL TIPO MERCE
131300040601     c                   Eval      *In67 = *Off
131400040601     c                   If        VidBva = *Blanks
131500040601     c                   Eval      *In67 = *On
131600040601     c                   Else
131700040601     c                   Move      §tabva        w001
131800040601     c                   If        VidBva = w001
131900040601     c                   Eval      *In67 = *On
132000040601     c                   EndIf
132100040601     c                   EndIf
132200060227
132300060227      * ESCLUSIONE IN BASE ALLA RICHIESTA DEL TIPO UTILIZZO PESO VDL
132400060227     c                   eval      *in35 = *off
132500060228     c                   if        vidtap = *blanks
132600060228     c                   eval      *in35 = *on
132700060228     c                   else
132800060228     c                   if        vidtap = 'V' and §tatap = *blanks
132900060227     c                   eval      *in35 = *on
133000060228     c                   else
133100060228     c                   if        vidtap = §tatap
133200060227     c                   eval      *in35 = *on
133300060228     c                   endif
133400060228     c                   endif
133500060228     c                   endif
133600960424     C*
133700960424     C* ESCLUSIONE IN BASE ALLA RICHIESTA DELLE PARTICOLARITA'
133800960424     C                   SETOFF                                       65
133900960424     C     VIDCPA        IFEQ      '  '
134000960424     C                   SETON                                        65
134100960424     C                   ELSE
134200960424     C     VIDCPA        IFEQ      TAMGCA
134300960424     C     VIDCPA        OREQ      TAMGGA
134400960424     C     VIDCPA        OREQ      TAMGMA
134500960424     C     VIDCPA        OREQ      TAMGVA
134600960424     C                   SETON                                        65
134700960424     C                   ENDIF
134800960424     C                   ENDIF
134900920625     C*
135000920625     C* ESCLUSIONE IN BASE ALLA RICHIESTA O MENO ISTAT
135100040119     c                   SELECT
135200040119      * includo solo con ISTAT
135300101015     C                   WHEN      TAMrct > *zeros  and  VIDIST = 'S'
135400040119     C                   eval      *in36 = *on
135500040119      * includo solo senza ISTAT
135600101015     C                   WHEN      TAMrct = *zeros  and  VIDIST = 'E'
135700040119     C                   eval      *in36 = *on
135800040119      * no Test
135900040119     C                   WHEN      VIDIST = 'N'
136000040119     C                   eval      *in36 = *on
136100040119      *
136200040119     C                   OTHER
136300040119     C                   eval      *in36 = *off
136400040119     C                   ENDSL
136500060310
136600060310      * esclusione in base a rinuncia AC BASE
136700060310     c                   select
136800060310      * includo solo se rinuncia inserita
136900060310     c                   when      tamfmp = 'S' and vidracb = 'S'
137000060310     c                   eval      *in41 = *on
137100060310      * includo solo senza rinuncia
137200060310     c                   when      tamfmp = *blanks and vidracb = 'E'
137300060310     c                   eval      *in41 = *on
137400060310      * no Test
137500060310     c                   when      vidracb = 'N'
137600060310     c                   eval      *in41 = *on
137700060310     c                   other
137800060310     c                   eval      *in41 = *off
137900060310     c                   endsl
138000920625     C*
138100060310     C* ESCLUSIONE IN BASE ALLA RICHIESTA O MENO DELL'AC PLUS
138200930519     C                   SETOFF                                       37
138300990714     C                   MOVEL     'C '          FTC
138400020912     C                   Z-ADD     0             COMPAC            8 3
138500930519     C                   MOVEL     ' '           COMFV1            1
138600930519     C*
138700990714     C     KTPT          CHAIN     TITPT000                           32
138800930519     C     VIDSAC        IFEQ      'N'
138900921123     C                   SETON                                        37
139000930519     C  N32TPTATB        IFEQ      ' '
139100930519     C                   Z-ADD     TPTVLM        COMPAC
139200930519     C                   END
139300930519     C*
139400921123     C                   ELSE
139500930519     C*
139600930519     C  N32TPTATB        IFEQ      ' '
139700930519     C     VIDSAC        ANDEQ     'S'
139800930519     C                   SETON                                        37
139900930519     C                   Z-ADD     TPTVLM        COMPAC
140000920625     C                   END
140100060310      * Solo reale
140200060310     c                   if        vidsac = 'R' and not *in32 and
140300060310     c                             tptatb = *blanks and tpttma = *blanks
140400060310     c                   Eval      *in37 = *on
140500060310     c                   z-add     tptvlm        compac
140600060310     c                   endif
140700060310      * Solo fittizio
140800060310     c                   if        vidsac = 'F' and not *in32 and
140900060310     c                             tptatb = *blanks and tpttma = 'F'
141000060310     c                   Eval      *in37 = *on
141100060310     c                   z-add     tptvlm        compac
141200060310     c                   endif
141300930519     C*
141400020502     c     *in32         ifeq      *on
141500020502     c     tptatb        orne      *blanks
141600020502     C     VIDSAC        IFEQ      'E'
141700930519     C                   SETON                                        37
141800920625     C                   END
141900020502     C                   END
142000920625     C                   END
142100921123     C*
142200930519     C* TIPO VALORE MERCE
142300930519     C     COMPAC        IFGT      0
142400930519     C     TPTFVM        IFEQ      '1'
142500930519     C                   MOVEL     'C'           COMFV1
142600930519     C                   ELSE
142700930519     C     TPTFVM        IFEQ      '2'
142800930519     C                   MOVEL     'S'           COMFV1
142900930519     C                   ELSE
143000930519     C                   MOVEL     'K'           COMFV1
143100930519     C                   END
143200930519     C                   END
143300930519     C                   END
143400060310
143500060310      * esclusione in base alla richiesta AC BASE
143600060310     c                   eval      *in42 = *off
143700060313     c                   clear                   comacbase         1
143800060310     c                   eval      ftc = 'd '
143900060313     c     ktpt          chain     titpt01l
144000060313    1c                   if        vidbase = 'N'
144100060310     c                   eval      *in42 = *on
144200060313     c                   if        %found(titpt01l) and tptatb = *blanks
144300060313     c                   eval      comacbase = 'S'
144400060313     c                   endif
144500060313   x1c                   else
144600060310     c                   if        %found(titpt01l) and tptatb = *blanks
144700060310     c                             and vidbase = 'S'
144800060310     c                   eval      *in42 = *on
144900060313     c                   eval      comacbase = 'S'
145000060310     c                   endif
145100060310
145200060314     c                   if        (not %found(titpt01l) or tptatb <> *blanks)
145300060310     c                             and vidbase = 'E'
145400060310     c                   eval      *in42 = *on
145500060310     c                   endif
145600060313    1c                   endif
145700110922
145800110922      * Tariffa particolare richiesta
145900110922     c                   eval      *in43 = *off
146000110922     c                   clear                   woktp
146100110922     c                   IF        VIDctp <> *blanks
146200110922     c                   eval      ftc = VIDctp
146300110922     c     Ktpt          chain     TITPT01L
146400110922     c                   IF        %found(TITPT01L) and TPTatb = *blanks
146500110922
146600110922      * Cerco dettaglio
146700110922     c     Ktpt          setll     TITPD01L
146800110922     c                   DO        *hival
146900110922     c     Ktpt          reade     TITPD01L
147000110922      * fine file
147100110922     c                   IF        %eof(TITPD01L)
147200110922     c                   leave
147300110922     c                   ENDIF
147400110922      * torno a leggere se trovo righe annullate
147500110922     c                   IF        TPDatb <> *blanks
147600110922     c                   iter
147700110922     c                   ENDIF
147800110922      * valorizzo il flag per la tariffa particolare
147900110922      * S = trovata
148000110922      * V = valorizzata
148100110922      * A = azzerata
148200110922     c                   SELECT
148300110922      * importo maggiore di 0 è valorizzata esco dal ciclo
148400110922     c                   WHEN      TPDitr > 0
148500110922     c                   eval      woktp = 'V'
148600110922     c                   leave
148700110922      * importo a 0 è azzerata
148800110922     c                   WHEN      TPDitr = 0
148900110922     c                   eval      woktp = 'A'
149000110922      * tariffa particolare trovata nel dettaglio
149100110922     c                   OTHER
149200110922     c                   eval      woktp = 'S'
149300110922     c                   ENDSL
149400110922
149500110922     c                   ENDDO
149600110922
149700110922     c                   ENDIF
149800110922     c                   ENDIF
149900110922
150000110922      * in base al flag per la tariffa particolare decido se la tariffa è da
150100110922      * stampare o meno
150200110922     c                   clear                   xx
150300110922     c                   SELECT
150400110922      * se richiesto 'E' - senza tariffe e richesto un ntw controllo se
150500110922      * tariffa abilitata per il ntw richiesto
150600110922     c                   WHEN      VIDstp = 'E' and woktp = *blanks and
150700110922     c                             VIDntw <> *blanks
150800110922     c                   eval      xx = %lookup(VIDctp:$1P)
150900110922     c                   IF        xx > 0
151000110922     c                   IF        VIDntw = 'ITA' and $tco(xx) <> 'N'
151100110922     c                   eval      *in43 = *on
151200110922     c                   ENDIF
151300110922     c                   IF        VIDntw = 'EEX' and $fie(xx) <> 'N'
151400110922     c                   eval      *in43 = *on
151500110922     c                   ENDIF
151600110922     c                   IF        VIDntw = 'DPD' and $fdp(xx) <> 'N'
151700110922     c                   eval      *in43 = *on
151800110922     c                   ENDIF
151900110922     c                   IF        VIDntw = 'FED' and $ffe(xx) <> 'N'
152000110922     c                   eval      *in43 = *on
152100110922     c                   ENDIF
152200110922     c                   ENDIF
152300110922     c                   WHEN      VIDstp = 'E' and woktp = *blanks
152400110922     c                   eval      *in43 = *on
152500110922     c                   WHEN      VIDstp = *blanks
152600110922     c                   eval      *in43 = *on
152700110922     c                   WHEN      VIDstp = 'S' and woktp <> *blanks
152800110922     c                   eval      *in43 = *on
152900110922     c                   WHEN      VIDstp = 'V' and woktp = 'V'
153000110922     c                   eval      *in43 = *on
153100110922     c                   WHEN      VIDstp = 'A' and woktp = 'A'
153200110922     c                   eval      *in43 = *on
153300110922     c                   ENDSL
153400110926      * Mi memorizzo sempre anche la presenza o meno del fuel
153500070130      * Supplemento Carburante Fuel Surcharge
153600070130      * in caso di richiesta delle tariffe senza fuel escludo tariffe FEDEX
153700070129     c                   movel     'f '          ftc
153800070129     c                   eval      contatore = 0
153900070129     c                   eval      comfue  = ' '
154000070129
154100070129     c     Ktpt          chain     Titpt01l
154200070129    1c                   if        %found(Titpt01l) and
154300070129     c                             tptatb = ' '
154400070129      * cerco importo percentuale
154500070129     c     ktpt          setll     titpd01l
154600070129    2c                   do        *hival
154700070129     c     ktpt          reade     titpd01l
154800070129      * fine file
154900070129     c                   if        %eof(titpd01l)
155000070129     c                   leave
155100070129     c                   endif
155200070129      * non annullate
155300070129     c                   if        tpdatb <> *blanks
155400070129     c                   iter
155500070129     c                   endif
155600070129      * scaglioni
155700070129     c                   add       1             contatore
155800070129      * 1° scaglione
155900070129     c                   if        contatore = 1
156000070129      * importo maggiore di zero
156100070129     c                   if        tpditr > 0
156200070129     c                   eval      comfue = 'S'
156300070129     c                   leave
156400070129     c                   endif
156500070129     c                   else
156600070129      * importo maggiore di zero dopo il primo scaglione
156700070129     c                   if        tpditr > 0
156800070129     c                   eval      comfue = 'S'
156900070129     c                   leave
157000070129     c                   endif
157100070129
157200070129     c                   endif
157300070129
157400070129    2c                   enddo
157500070129    1c                   endif
157600930519     C*
157700010618     C   33
157800060227     CAN 35
157900040119     CAN 36
158000020307     CAN 37
158100020307     CAN 38
158200020307     CAN 40
158300060310     CAN 41
158400060310     CAN 42
158500070129     CAN 43
158600020307     CAN 64
158700040527     CANN57
158800020307     CAN 65
158900040601     CAN 66
159000040601     can 67              do
159100900831     C                   ADD       1             X
159200110926     C                   MOVE      *BLANK        WAR
159300110926     C                   MOVEA     §CTR          WAR(1)
159400110926     C                   MOVEA     '-'           WAR(4)
159500110926     C                   MOVEA     TAMFTP        WAR(5)
159600110926     C                   MOVEA     TAMBAP        WAR(7)
159700110926     C     §TADPD        IFNE      'S'
159800110926     C     §TAfed        andne     'S'
159900110926     C                   MOVEA     TAMFIE        WAR(10)
160000110926     C                   ELSE
160100110926     C     §TADPD        IFeq      'S'
160200110926     C                   MOVEA     'D'           WAR(10)
160300110926     C                   ENDIF
160400110926     C     §TAfed        IFeq      'S'
160500110926     C                   MOVEA     'F'           WAR(10)
160600110926     C                   ENDIF
160700110926     C                   ENDIF
160800110926     C                   MOVEA     TAMTSP        WAR(12)
160900110926     C**!!!              MOVEL     TAMDCV        COMDCV           10
161000110926     C**!!!              MOVEA     COMDCV        WAR(14)
161100110926     C                   MOVEA     WAR           WAR12            12
161200110926     C                   MOVEL     WAR12         DAR(X)
161300900831     C                   MOVE      TAMCTR        CAR(X)
161400901008     C                   Z-ADD     TAMKPM        KPM(X)
161500910904     C                   MOVEL     TAMTIN        TIN(X)
161600920624     C*
161700101015     C                   z-add     TAMrct        IS1(X)
161800040119     C                   Z-ADD     TAMPRI        IS2(X)
161900920624     C*
162000921124     C                   Z-ADD     COMPAC        AC1(X)
162100930519     C                   MOVEL     COMFV1        FV1(X)
162200920624     C*
162300070129     C                   MOVEL     COMFUE        Fuel(X)
162400110926     c                   IF        woktp <> *blanks
162500110926     c                   eval      tarpar(x) = 'S'
162600110926     c                   ENDIF
162700070129     C*
162800950208     C* TIPO COMUNICAZIONE AL MITTENTE
162900990714     C     KTGC          CHAIN     TITGC000                           29
163000950208     C     *IN29         IFEQ      *OFF
163100950208     C     TGCATB        ANDEQ     ' '
163200950208     C     TGCTCM        IFEQ      ' '
163300950208     C                   MOVEL     VIDFL0        TCM(X)
163400950208     C                   ELSE
163500950208     C                   MOVEL     TGCTCM        TCM(X)
163600950208     C                   ENDIF
163700950208     C*
163800950208     C                   ELSE
163900950208     C* SE NON ESISTE TARIFFA GIACENZA PRENDO DA ANAGRAFICA
164000950208     C                   MOVEL     VIDFL0        TCM(X)
164100950208     C                   ENDIF
164200970320     C                   MOVEL     TAMBAM        BAM(X)
164300040601     c                   movel     §tabva        bva(x)
164400060310     c                   movel     §tatap        tap(x)
164500060314     c                   if        tamfmp = 'S'
164600060314     c                   move      'R'           acbase(x)
164700060314     c                   else
164800060313     c                   movel     comacbase     acbase(x)
164900060314     c                   endif
165000950208     C*
165100930211     C     X             COMP      5                                      30
165200900831     C                   END
165300900901     C                   END
165400911001     C                   END
165500900901     C  N30TAMKSC        COMP      ACOKSC                             3030
165600900831     C  N30              END
165700911001     C*
165800910904     C     KPM(1)        IFNE      0
165900910904     C     TIN(1)        COMP      'C'                                    50
166000910904     C                   END
166100910904     C     KPM(2)        IFNE      0
166200910904     C     TIN(2)        COMP      'C'                                    51
166300910904     C                   END
166400910904     C     KPM(3)        IFNE      0
166500910904     C     TIN(3)        COMP      'C'                                    53
166600910904     C                   END
166700910904     C     KPM(4)        IFNE      0
166800910904     C     TIN(4)        COMP      'C'                                    55
166900910904     C                   END
167000910904     C     KPM(5)        IFNE      0
167100910904     C     TIN(5)        COMP      'C'                                    56
167200910904     C                   END
167300921124     C*
167400900831     C                   ENDSR
167500001204     C*
167600001204     C*--- STAMPA NOTE -----------------------------------------------*
167700001204     C     SRNOTE        BEGSR
167800001204      *
167900001204     C                   SETOFF                                       90
168000001204      *
168100001204     C                   MOVEL     ACOKCC        KNK1
168200001204     C                   MOVE      ACOKSC        KNK1
168300001204     C     KTFN1         SETLL     TFNTC01L
168400001204     C                   DO        *HIVAL
168500001204     C     KTFN1         READE     TFNTC01L                               84
168600001204     C     *IN84         IFEQ      *ON
168700001204     C                   LEAVE
168800001204     C                   ENDIF
168900010130     C     NTCSNS        IFEQ      'N'
169000001204     C                   ITER
169100001205     C                   ENDIF
169200010130     C     NTCFLT        IFEQ      'A'
169300010130     C                   ITER
169400010130     C                   ENDIF
169500010130     C     NTCRNT        IFEQ      *BLANKS
169600001205     C                   ITER
169700001204     C                   ENDIF
169800001204     C                   EXCEPT    NOTE
169900001204     C                   SETON                                        90
170000001204     C                   ENDDO
170100001204      *
170200001204     C                   ENDSR
170300900831     C/SPACE
170400940903     C*---------------------------------------------------------------*
170500950208     OPRTF198   E            RICHI          1 02
170600940903     O                     10SK(1)              120
170700940903     O                     11SK(2)              120
170800940903     O                       RSUT                20
170900940903     O                                           21 '/'
171000940903     O                       SIMFEL              24
171100940903     O                                          153 'TNTA08R'
171200940903     O                       UDATE8             170 '  /  /    '
171300940903     O                                          193 'pag.'
171400940903     O                       PAGE          Z    198
171500950208     O          E            RICHI          2
171600940903     O                       KNSIF               10
171700940903     O                       KNMUS               22
171800940903     O                       HHMMSS             168 '  :  :  '
171900940903     O          E            RICHI          2
172000940903     O                                           20 'P A R A M E T R I'
172100940903     O                                           40 'R I C H I E S T I'
172200920613     O          E            RICHI          2
172300960930     O                                           24 'Punti Operativi ....:'
172400920613     O                       VIDFIL            +  1
172500940903     O          E            RICHI          2
172600940903     O                                           24 'Ordinamento stampa .:'
172700940903     O                       VIDSC1            +  1
172800001204     O          E            RICHI          2
172900001204     O                                           24 'Stampa note cliente :'
173000001204     O                       VISTPN            +  1
173100020312     O          E            RICHI          2
173200020312     O                                           24 'Tipo Stampa ........:'
173300020312     O                       VIDTPS            +  1
173400020312     O          E            RICHI          2
173500060227     O                                           22 'S E L E Z I O N I  S U'
173600060227     O                                           38 '  T A R I F F E'
173700940903     O          E            RICHI          2
173800040119     O                                           24 'Parzializz. ISTAT   :'
173900040119     O                       VIDIST            +  1
174000040119     O          E            RICHI          2
174100940903     O                                           24 'Parzializz. Volume .:'
174200920613     O                       VIDSC3            +  1
174300060310     O          E            RICHI          2
174400060310     O                                           24 'Rinuncia AC BASE....:'
174500060310     O                       vidracb           +  1
174600060310     O          E            RICHI          2
174700060310     O                                           24 'Parz. AC PLUS.......:'
174800060310     O                       VIDSAC            +  1
174900060310     O          E            RICHI          2
175000060310     O                                           24 'Parz. AC BASE.......:'
175100060310     O                       vidbase           +  1
175200070129     O          E            RICHI          2
175300110921     O                                           24 'Parz. Tar.Partic....:'
175400110921     O                       vidctp            +  1
175500110927     o                       desctp            +  1
175600110921     o                       desstp            +  2
175700051213     O          E            RICHI          2
175800051213     O                                           24 'Parz. tariffe blocc.:'
175900051213     O                       VIDblk            +  1
176000940903     O          E            RICHI          2
176100970320     O                                           24 'Parz. Consegna D D T:'
176200970320     O                       VIDBAM            +  1
176300940903     O          E            RICHI          2
176400940903     O                                           24 'Parz. Tipo Servizio :'
176500940903     O                       VIDTSP            +  1
176600960424     O          E            RICHI          2
176700960424     O                                           24 'Parz. Tipo Tariffa  :'
176800960424     O                       VIDTTR            +  1
176900040601     O          E            RICHI          2
177000040601     O                                           24 'Parz. Tipo Merce    :'
177100040601     O                       vidbva            +  1
177200000725     O          E            RICHI          2
177300020312     O                                           24 'Parz. Particolarità :'
177400020312     O                       VIDCPA            +  1
177500020312     O          E            RICHI          2
177600020307     O                                           24 'Parzializz.Tarif NTW:'
177700020312     O                       VIDNTR            +  1
177800020312     O                       VIDNTW            +  2
177900060227     O          E            RICHI          2
178000060227     O                                           24 'Parzializz.Peso VDL :'
178100060227     O                       VIDtap            +  1
178200020312     O          E            RICHI          2
178300020312     O                                           32 'S E L E Z I O N I  S U  -
178400020312     O                                              P. D. C.'
178500920613     O          E            RICHI          2
178600920613     O                                           24 'Parzializz.Categoria:'
178700920613     O                       VIDSNC            +  1
178800920613     O                       VIDSCT            +  2
178900001204     O          E            RICHI          2
179000001204     O                                           24 'Parz.NOTE Cliente ..:'
179100001204     O                       VINOTE            +  1
179200001204     O                       VITNOT            +  2
179300051128     O          E            RICHI          2
179400051128     O                                           24 'Parz.LUOGO .........:'
179500051128     O                       VIluog            +  1
179600051128     O                       VITluo            +  2
179700011211     O          E            RICHI          2
179800020312     O                                           24 'Parzializz.Cod.comm.:'
179900011211     O                       VIDAGE            +  1
180000020312     O          E            RICHI          2
180100020312     O                                           24 'Parzializz.Cod.imp. :'
180200020312     O                       VIDCIC            +  1
180300020312     O          E            RICHI          2
180400020312     O                                           24 'Parzializz.Clienti .:'
180500020312     O                       VIDSC2            +  1
180600020404     O          E            RICHI          2
180700020404     O                                           24 'Clienti no operativi:'
180800020404     O                       VIDCNO            +  1
180900020429     O                                         +  2 'nel periodo da .:'
181000020429     O                       VIDDMM            +  1
181100020429     O                                         +  0 '/'
181200020429     O                       VIDDAA            +  0
181300020429     O                                         +  2 'a .:'
181400020429     O                       VIDAMM            +  1
181500020429     O                                         +  0 '/'
181600020429     O                       VIDAAA            +  0
181700020429     O                                         +  3 '(mm/aa)'
181800050617     O          E    15      RICHI          2
181900050617     O                                           24 'Parz.Tipo Pagam. C/A:'
182000050617     o                                         +  1 '"'
182100050617     O                       VIDtpc            +  0
182200050617     o                                         +  0 '"'
182300050617     O          E   n15      RICHI          2
182400050617     O                                           24 'Parz.Tipo Pagam. C/A:'
182500050617     O                       VIDtpc            +  1
182600121227     O          E            RICHI          2
182700121227     O                                           24 'Parz.Tass.POST Fatt.:'
182800121227     O                       VIDtpf            +  1
182900950208     O          E            TESTA          1 02
183000940903     O                     10SK(1)              120
183100940903     O                     11SK(2)              120
183200900901     O                       RSUT                20
183300910904     O                                           21 '/'
183400910904     O                       SIMFEL              24
183500940903     O                                          153 'TNTA08R'
183600940903     O                       UDATE8             170 '  /  /    '
183700940903     O                                          193 'pag.'
183800940903     O                       PAGE          Z    198
183900950208     O          E            TESTA          2
184000940903     O                       KNSIF               10
184100940903     O                       KNMUS               22
184200940903     O                       HHMMSS             168 '  :  :  '
184300900831     O          E            TESTA          2
184400960930     O                                         +  4 'Punto Operativo:'
184500900901     O                       FIL(W)        Z   +  1
184600900831     O                       ORGDES            +  2
184700900901     O          E            TESTA          1
184800920624     O                       SK1                198
184900900901     O          E            TESTA          1
185000920624     O                       SK2                198
185100920613     O          E            TESTA          1
185200920624     O                       SK3                198
185300110927     o                       vidctp             179
185400920624     O          E            TESTA          1
185500920624     O                       SK4                198
185600920624     O          E            TESTA          2
185700920624     O                       SK5                198
185800920624     O* sk1
185900900901     O          E            DETT        1  1
186000900831     O                       ACOKSC
186100970320     O                       ACOABL               9
186200970320     O                       ACORAG              59
186300970320     O                    N70INDABI        Z     67
186400970320     O                     46                    68 '/'
186500970320     O                    N70INDCAB        Z     73
186600970320     O                     46                    74 '-'
186700080211     O                    N70wbanca             100
186800110926     O                       DAR(1)             129
186900110926     O                       IS1(1)        z    132
187000110926     O                       IS2(1)        4    139
187100110926     o                       tap(1)             143
187200110926     o                       acbase(1)          149
187300110926     O                       AC1(1)        4    165
187400110926     O                       FV1(1)             166
187500110926     O                       FUEL(1)            171
187600110926     O                       tarpar(1)          180
187700040601     O                 N50   KPM(1)        4    192
187800040601     O                  50   KPM(1)             192 '0 ,   '
187900110926     O**!!!                  TCM(1)             194
188000040601     O                       BAM(1)             196
188100040601     o                       bva(1)             198
188200920624     O* sk2
188300900831     O          E            DETT           1
188400970320     O                    N70INDVIA              41
188500970320     O                    N70INDCIT              72
188600970320     O                    N70INDCAE              82
188700970320     O                    N70INDPRV              85
188800970320     O                    N70INDSTA              91
188900970320     O                    N71CLPAGE        Z     99
189000970320     O                    N71CLPCLV             103
189100110926     O                       DAR(2)             129
189200110926     O                       IS1(2)        z    132
189300110926     O                       IS2(2)        4    139
189400110926     o                       tap(2)             143
189500110926     o                       acbase(2)          149
189600110926     O                       AC1(2)        4    165
189700110926     O                       FV1(2)             166
189800110926     O                       FUEL(2)            171
189900110926     O                       tarpar(2)          180
190000040601     O                 N51   KPM(2)        4    192
190100040601     O                  51   KPM(2)             192 '0 ,   '
190200110926     O**!!!                  TCM(2)             194
190300040601     O                       BAM(2)             196
190400040601     o                       bva(2)             198
190500920624     O* sk3
190600900901     O          E            DETT           1
190700970320     O                    N70INDTEL              27
190800970320     O                    N70INDTLF              44
190900970320     O                    N70INDTLX              61
191000970320     O                       ACOITC        Z     73
191100970320     O                     47                    74 '-'
191200970320     O                       DESSCT             100
191300970320     O                       ACOLIB        Z    113
191400110926     O                       DAR(3)             129
191500110926     O                       IS1(3)        z    132
191600110926     O                       IS2(3)        4    139
191700110926     o                       tap(3)             143
191800110926     o                       acbase(3)          149
191900110926     O                       AC1(3)        4    165
192000110926     O                       FV1(3)             166
192100110926     O                       FUEL(3)            171
192200110926     O                       tarpar(3)          180
192300040601     O                 N53   KPM(3)        4    192
192400040601     O                  53   KPM(3)             192 '0 ,   '
192500110926     O**!!!                  TCM(3)             194
192600040601     O                       BAM(3)             196
192700040601     o                       bva(3)             198
192800920624     O* sk4
192900920624     O          E            DETT           1
193000970320     O                    N70INDCDF              27
193100970320     O                    N70INDIVA              48
193200970320     O                    N70§CDPAG              73
193300970320     O                                           74 '-'
193400970320     O                    N70§DEPAG             100
193500110926     O                       DAR(4)             129
193600110926     O                       IS1(4)        z    132
193700110926     O                       IS2(4)        4    139
193800110926     o                       tap(4)             143
193900110926     o                       acbase(4)          149
194000110926     O                       AC1(4)        4    165
194100110926     O                       FV1(4)             166
194200110926     O                       FUEL(4)            171
194300110926     O                       tarpar(4)          180
194400040601     O                 N55   KPM(4)        4    192
194500040601     O                  55   KPM(4)             192 '0 ,   '
194600110926     O**!!!                  TCM(4)             194
194700040601     O                       BAM(4)             196
194800040601     o                       bva(4)             198
194900920624     O* sk5
195000920613     O          E            DETT           1
195100050617     O                    N71stafpc              46
195200050617     O                    N71desfpc              62
195300080117     o                    n71iban1              + 1
195400080117     o                    n71iban2              + 1
195500080117     o                    n71iban3              + 1
195600080117     o                    n71iban4              + 1
195700080117     o                    n71iban5              + 1
195800080117     o                    n71iban6              + 1
195900080117     o                    n71iban7              + 1
196000080211     o                    n71iban8              + 1
196100110926     O                       DAR(5)             129
196200110926     O                       IS1(5)        z    132
196300110926     O                       IS2(5)        4    139
196400110926     o                       tap(5)             143
196500110926     o                       acbase(5)          149
196600110926     O                       AC1(5)        4    165
196700110926     O                       FV1(5)             166
196800110926     O                       FUEL(5)            171
196900110926     O                       tarpar(5)          180
197000040601     O                 N56   KPM(5)        4    192
197100040601     O                  56   KPM(5)             192 '0 ,   '
197200110926     O**!!!                  TCM(5)             194
197300040601     O                       BAM(5)             196
197400040601     o                       bva(5)             198
197500001204     O* Note
197600001204     O          E            NOTE           1
197700001204     O              N90                          16 'Note:'
197800001204     O                       NTCTNT              19
197900001204     O                       NTCRNT              80
198000910927     O*
198100931228     O          E            TOTFIL      3
198200960930     O                                              'Totale Clienti'
198300960930     O                                         +  1 'Punto Operativo :'
198400910927     O                       §CTRC         2   +  1
198500910927     O*
198600950208     O          E            TOTFIN         1 02
198700940903     O                     10SK(1)              120
198800940903     O                     11SK(2)              120
198900910927     O                       RSUT                20
199000910927     O                                           21 '/'
199100910927     O                       SIMFEL              24
199200940903     O                                          153 'TNTA08R'
199300940903     O                       UDATE8             170 '  /  /    '
199400940903     O                                          193 'pag.'
199500940903     O                       PAGE          Z    198
199600950208     O          E            TOTFIN         2
199700940903     O                       KNSIF               10
199800940903     O                       KNMUS               22
199900940903     O                       HHMMSS             168 '  :  :  '
200000910927     O          E            TOTFIN      2
200100960930     O                                           32 'Totale Clienti Finale :'
200200910927     O                       §CTRT         2   +  1
200300020307      *
200400020308      * Stampa sintetica
200500020308     O          E            TESTA1         1 02
200600020308     O                     10SK(1)              120
200700020308     O                     11SK(2)              120
200800020308     O                       RSUT                20
200900020308     O                                           21 '/'
201000020308     O                       SIMFEL              24
201100020308     O                                          153 'TNTA08R'
201200020308     O                       UDATE8             170 '  /  /    '
201300020308     O                                          193 'pag.'
201400020308     O                       PAGE          Z    198
201500020308     O          E            TESTA1         2
201600020308     O                       KNSIF               10
201700020308     O                       KNMUS               22
201800020308     O                       HHMMSS             168 '  :  :  '
201900020308     O          E            TESTA1         2
202000020308     O                                         +  4 'Punto Operativo:'
202100020308     O                       FIL(W)        Z   +  1
202200020308     O                       ORGDES            +  2
202300020307     O          E            TESTA1         1
202400020307     O                       SK6                198
202500020308     O* sk6
202600020312     O          E            DETT1          1
202700020307     O                       ACOKSC
202800130315     O                       ACOABL            +  2
202900130315     O                       ACORAG            +  3
203000020312     O                    N70INDCIT            +  1
203100020314     O                    N70INDPRV            +  1
203200020312     O                    N70INDCAE            +  1
203300020312     O                       ACOITC        Z   +  1
203400020312     O                     47                  +  1 '-'
203500020312     O                       DESSCT            +  1
203600020312     O                    N71CLPAGE        Z   +  3
203700020312     O                    N71CLPDESP           +  1
203800020314     O                    N71CLPCLV            +  9
203900020314     O                       ACOLIB        Z   +  9
204000020308**   SK
204100020307                      ANAGRAFICO CLIENTI  -IN ORDINE ALFABETICO-   1 a
204200020307                      ANAGRAFICO CLIENTI  -IN ORDINE DI CODICE-    2 b
204300920624**   SK1
204400970320Codice Blo Ragione Sociale Cliente                              AB 3 a
204500970320I/CAB   Descrizione Banca Appoggio               C  O  N  D  I  Z  4 b
204600950208 I  O  N  I      T  A  R  I  F  F  A  R  I  E                      5 c
204700920624**   SK2
204800970320 Cli.  cc. Indirizzo                      Località                 6 a
204900110926       C.A.P     Pro Naz Commerc. CD.Imp            Pr Pa I Se |-  7 b
205000110926ISTAT -| Pes| AC BASE|  AC PLUS   | FUEL | Tar.Par|  Kg.xMc    D V 8 c
205100070129**   SK3
205200110926           Telefono         Fax                                   12 a
205300080307   Categ.Merceol.                     Potenzial     ef Ar E rv    13 b
205400110927       | VDL|            ValMer              " "     Mc.xCl    D A14 c
205500920624**   SK4
205600970320           Codice Fiscale       Partita IVA                       15 a
205700000725   Condizione Pagamento                             er Bl D iz    16 b
205800110926                                                               T L17 c
205900920624**   SK5
206000080117                                             Pagamento C/A     IBA 9 a
206100080117N per Bonif. Contrassegno                                         10 b
206200921124                                                                  11 c
206300020307**   SK6
206400020314Codice Cli.  Ragione Sociale Cliente                          Loca
206500080307lità                          C.A.P.        Categ.Merceol.
206600020314              Cd.Commerciale          Cd.Importanza  Cd.Potenziale
